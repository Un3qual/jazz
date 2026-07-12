{-# LANGUAGE OverloadedStrings #-}

-- | Evaluate a successful compiled program once in dependency order.
module JazzNext.Compiler.ModuleRuntime
  ( RuntimeExport (..),
    RuntimeModule (..),
    RuntimeProgram (..),
    evaluateCompiledProgram,
    lookupRuntimeModule
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import JazzNext.Compiler.CapabilityFacts (splitQualifiedMethodKey)
import JazzNext.Compiler.AST
  ( Expr (EBlock),
    Statement
  )
import JazzNext.Compiler.Diagnostics (Diagnostic)
import JazzNext.Compiler.ModuleGraph
  ( ResolvedImport (..),
    ResolvedModule (resolvedModuleExportInventory, resolvedModuleImports, resolvedModulePath)
  )
import JazzNext.Compiler.ModuleExports
  ( ModuleExport (..),
    ModuleExportInventory,
    ModuleImportMode (..),
    exportInventoryEntries,
    exportNamesInNamespace,
    inventoryHasExport,
    visibleImportInventory
  )
import JazzNext.Compiler.ModuleInterface
  ( CompiledModule (..),
    CompiledPrelude (..),
    CompiledProgram (..),
    ModuleInterface (..),
    moduleInterfaceExportInventory
  )
import JazzNext.Compiler.Name
  ( Name (ResolvedName),
    NameNamespace (..),
    ResolvedNameOrigin (..),
    identifierText,
    mkIdentifier,
    renderName,
    sourceName
  )
import JazzNext.Compiler.Runtime
  ( ModuleEvaluationMode (..),
    RuntimeCell,
    RuntimeEnv,
    RuntimeValue,
    ScopeResult (..),
    evaluateModuleScope
  )

-- | Runtime-facing exports keep capability methods structurally distinct from
-- ordinary values instead of encoding their owner in a value-name string.
data RuntimeExport
  = RuntimeBindingExport ModuleExport
  | RuntimeCapabilityMethodExport
      { runtimeExportCapabilityName :: Text,
        runtimeExportMethodName :: Text
      }
  deriving (Eq, Ord, Show)

data RuntimeModule = RuntimeModule
  { runtimeModulePath :: [Text],
    runtimeModuleExports :: Map RuntimeExport RuntimeCell
  }

data RuntimeProgram = RuntimeProgram
  { runtimeProgramModules :: [RuntimeModule],
    runtimeProgramOutput :: Maybe RuntimeValue
  }

lookupRuntimeModule :: [Text] -> RuntimeProgram -> Maybe RuntimeModule
lookupRuntimeModule modulePath =
  go . runtimeProgramModules
  where
    go modules =
      case modules of
        [] -> Nothing
        runtimeModule : rest
          | runtimeModulePath runtimeModule == modulePath -> Just runtimeModule
          | otherwise -> go rest

evaluateCompiledProgram :: CompiledProgram -> Either Diagnostic RuntimeProgram
evaluateCompiledProgram compiledProgram =
  case compiledProgramErrors compiledProgram of
    firstError : _ -> Left firstError
    [] -> do
      ambientEnv <- evaluatePrelude (compiledProgramPrelude compiledProgram)
      evaluateModules ambientEnv [] Nothing (compiledProgramModules compiledProgram)
  where
    entryPath = compiledProgramEntryPath compiledProgram

    evaluateModules ambientEnv runtimeModules output remainingModules =
      case remainingModules of
        [] ->
          Right
            RuntimeProgram
              { runtimeProgramModules = runtimeModules,
                runtimeProgramOutput = output
              }
        compiledModule : rest -> do
          let resolvedModule = compiledResolvedModule compiledModule
              modulePath = resolvedModulePath resolvedModule
              evaluationMode =
                if modulePath == entryPath
                  then EvaluateEntryModule
                  else EvaluateDependencyModule
              importedEnv =
                foldr
                  (importRuntimeModule compiledProgram runtimeModules)
                  ambientEnv
                  (resolvedModuleImports resolvedModule)
          scopeResult <-
            evaluateModuleScope
              (Just modulePath)
              evaluationMode
              (compiledPreludeBuiltinMode (compiledProgramPrelude compiledProgram))
              (interfaceRuntimeHints (compiledModuleInterface compiledModule))
              importedEnv
              (scopeStatements (compiledModuleExpr compiledModule))
          let runtimeModule =
                RuntimeModule
                  { runtimeModulePath = modulePath,
                    runtimeModuleExports =
                      publishExports
                        CurrentModule
                        (resolvedModuleExportInventory resolvedModule)
                        (compiledModuleInterface compiledModule)
                        (scopeResultEnvironment scopeResult)
                  }
              nextOutput =
                if modulePath == entryPath
                  then scopeResultValue scopeResult
                  else output
          evaluateModules ambientEnv (runtimeModules <> [runtimeModule]) nextOutput rest

evaluatePrelude :: CompiledPrelude -> Either Diagnostic RuntimeEnv
evaluatePrelude compiledPrelude =
  case compiledPreludeExpr compiledPrelude of
    Nothing -> Right Map.empty
    Just expression -> do
      scopeResult <-
        evaluateModuleScope
          (Just [])
          EvaluateDependencyModule
          (compiledPreludeBuiltinMode compiledPrelude)
          (compiledPreludeRuntimeHints compiledPrelude)
          Map.empty
          (scopeStatements expression)
      pure
        ( publishEnvironment
            AmbientPrelude
            (moduleInterfaceExportInventory (compiledPreludeInterface compiledPrelude))
            (compiledPreludeInterface compiledPrelude)
            (scopeResultEnvironment scopeResult)
        )

importRuntimeModule :: CompiledProgram -> [RuntimeModule] -> ResolvedImport -> RuntimeEnv -> RuntimeEnv
importRuntimeModule compiledProgram runtimeModules importDecl env =
  case (lookupCompiled dependencyPath, lookupRuntime dependencyPath) of
    (Just compiledDependency, Just runtimeDependency) ->
      let publicInventory =
            resolvedModuleExportInventory (compiledResolvedModule compiledDependency)
          selectedExports =
            [ (runtimeExport, cell)
              | (runtimeExport, cell) <- Map.toList (runtimeModuleExports runtimeDependency),
                runtimeExportSelected importDecl publicInventory runtimeExport
            ]
          insertExport (runtimeExport, cell) =
            Map.insert
              ( ResolvedName
                  (ImportedModule dependencyPath)
                  (runtimeExportNamespace runtimeExport)
                  (mkIdentifier (runtimeExportName runtimeExport))
              )
              cell
       in foldr insertExport env selectedExports
    _ -> env
  where
    dependencyPath = resolvedImportPath importDecl
    lookupCompiled path =
      findByPath (resolvedModulePath . compiledResolvedModule) path (compiledProgramModules compiledProgram)
    lookupRuntime path = findByPath runtimeModulePath path runtimeModules

publishEnvironment :: ResolvedNameOrigin -> ModuleExportInventory -> ModuleInterface -> RuntimeEnv -> RuntimeEnv
publishEnvironment origin publicInventory moduleInterface env =
  let renderedLookupIndex = buildRenderedLookupIndex env
   in Map.fromList
        [ (ResolvedName origin (runtimeExportNamespace runtimeExport) (mkIdentifier (runtimeExportName runtimeExport)), cell)
          | runtimeExport <- interfaceExports publicInventory moduleInterface,
            Just cell <- [lookupExportCell origin runtimeExport env renderedLookupIndex]
        ]

publishExports :: ResolvedNameOrigin -> ModuleExportInventory -> ModuleInterface -> RuntimeEnv -> Map RuntimeExport RuntimeCell
publishExports origin publicInventory moduleInterface env =
  let renderedLookupIndex = buildRenderedLookupIndex env
   in Map.fromList
        [ (runtimeExport, cell)
          | runtimeExport <- interfaceExports publicInventory moduleInterface,
            Just cell <- [lookupExportCell origin runtimeExport env renderedLookupIndex]
        ]

interfaceExports :: ModuleExportInventory -> ModuleInterface -> [RuntimeExport]
interfaceExports publicInventory moduleInterface =
  [ RuntimeBindingExport export
    | export <- Set.toList (exportInventoryEntries publicInventory),
      moduleExportNamespace export `elem` [ValueNamespace, ConstructorNamespace]
  ]
    <> [ RuntimeCapabilityMethodExport className methodName
         | methodKey <- Map.keys (interfaceClassMethods moduleInterface),
           Just (className, methodName) <- [splitQualifiedMethodKey methodKey],
           Set.member className publicClassNames
       ]
  where
    publicClassNames = exportNamesInNamespace CapabilityNamespace publicInventory

runtimeExportSelected :: ResolvedImport -> ModuleExportInventory -> RuntimeExport -> Bool
runtimeExportSelected importDecl publicInventory runtimeExport =
  case runtimeExport of
    RuntimeCapabilityMethodExport className _ ->
      resolvedImportAlias importDecl == Nothing
        && Set.member className selectedClassNames
    RuntimeBindingExport moduleExport ->
      inventoryHasExport moduleExport selectedInventory
  where
    importMode =
      case resolvedImportAlias importDecl of
        Nothing -> UnqualifiedImport
        Just _ -> QualifiedAliasImport
    selectedInventory =
      visibleImportInventory
        importMode
        (resolvedImportSymbols importDecl)
        publicInventory
    selectedClassNames =
      exportNamesInNamespace CapabilityNamespace selectedInventory

runtimeExportName :: RuntimeExport -> Text
runtimeExportName runtimeExport =
  case runtimeExport of
    RuntimeBindingExport moduleExport -> moduleExportName moduleExport
    RuntimeCapabilityMethodExport className methodName -> className <> "::" <> methodName

runtimeExportNamespace :: RuntimeExport -> NameNamespace
runtimeExportNamespace runtimeExport =
  case runtimeExport of
    RuntimeBindingExport moduleExport -> moduleExportNamespace moduleExport
    RuntimeCapabilityMethodExport {} -> ValueNamespace

type RenderedLookupIndex = Map (NameNamespace, Text) RuntimeCell

lookupExportCell :: ResolvedNameOrigin -> RuntimeExport -> RuntimeEnv -> RenderedLookupIndex -> Maybe RuntimeCell
lookupExportCell origin runtimeExport env renderedLookupIndex =
  case Map.lookup expectedName env of
    Just cell -> Just cell
    Nothing -> lookupRendered runtimeExport renderedLookupIndex
  where
    exportName = runtimeExportName runtimeExport
    expectedName =
      case origin of
        AmbientPrelude -> sourceName (mkIdentifier exportName)
        _ -> ResolvedName origin (runtimeExportNamespace runtimeExport) (mkIdentifier exportName)

buildRenderedLookupIndex :: RuntimeEnv -> RenderedLookupIndex
buildRenderedLookupIndex =
  foldr indexName Map.empty . Map.toList
  where
    indexName (name, cell) index =
      foldr
        (\key -> Map.insert key cell)
        index
        [ (namespace, renderedName)
          | namespace <- matchingNamespaces name,
            renderedName <- Set.toList (Set.fromList [renderName name, identifierText name])
        ]
    matchingNamespaces name =
      case name of
        ResolvedName _ namespace _ -> [namespace]
        _ -> [ValueNamespace, ConstructorNamespace, TypeNamespace, CapabilityNamespace]

lookupRendered :: RuntimeExport -> RenderedLookupIndex -> Maybe RuntimeCell
lookupRendered runtimeExport renderedLookupIndex =
  Map.lookup
    (runtimeExportNamespace runtimeExport, runtimeExportName runtimeExport)
    renderedLookupIndex

scopeStatements :: Expr -> [Statement]
scopeStatements expression =
  case expression of
    EBlock statements -> statements
    _ -> []

findByPath :: (a -> [Text]) -> [Text] -> [a] -> Maybe a
findByPath _ _ [] = Nothing
findByPath getPath target (item : rest)
  | getPath item == target = Just item
  | otherwise = findByPath getPath target rest
