{-# LANGUAGE OverloadedStrings #-}

-- | Evaluate a successful compiled program once in dependency order.
module JazzNext.Compiler.ModuleRuntime
  ( RuntimeModule (..),
    RuntimeProgram (..),
    evaluateCompiledProgram,
    lookupRuntimeModule
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import JazzNext.Compiler.AST
  ( Expr (EBlock),
    Statement
  )
import JazzNext.Compiler.Diagnostics (Diagnostic)
import JazzNext.Compiler.Identifier (identifierText, mkIdentifier)
import JazzNext.Compiler.ModuleGraph
  ( ResolvedImport (..),
    ResolvedModule (resolvedModuleImports, resolvedModulePath)
  )
import JazzNext.Compiler.ModuleInterface
  ( CompiledModule (..),
    CompiledPrelude (..),
    CompiledProgram (..),
    ModuleInterface (..)
  )
import JazzNext.Compiler.Name
  ( Name (ResolvedName),
    NameNamespace (..),
    ResolvedNameOrigin (..),
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
import JazzNext.Compiler.TypeInference.Types (TypeBinding (ConstructorTypeBinding))

data RuntimeModule = RuntimeModule
  { runtimeModulePath :: [Text],
    runtimeModuleExports :: Map Text RuntimeCell
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
          Nothing
          EvaluateDependencyModule
          (compiledPreludeBuiltinMode compiledPrelude)
          (compiledPreludeRuntimeHints compiledPrelude)
          Map.empty
          (scopeStatements expression)
      pure
        ( publishEnvironment
            AmbientPrelude
            (compiledPreludeInterface compiledPrelude)
            (scopeResultEnvironment scopeResult)
        )

importRuntimeModule :: CompiledProgram -> [RuntimeModule] -> ResolvedImport -> RuntimeEnv -> RuntimeEnv
importRuntimeModule compiledProgram runtimeModules importDecl env =
  case (lookupCompiled dependencyPath, lookupRuntime dependencyPath) of
    (Just compiledDependency, Just runtimeDependency) ->
      let interface = compiledModuleInterface compiledDependency
          selectedExports =
            [ (exportName, cell)
              | (exportName, cell) <- Map.toList (runtimeModuleExports runtimeDependency),
                maybe True (exportName `elem`) (resolvedImportSymbols importDecl)
            ]
          insertExport (exportName, cell) =
            Map.insert
              ( ResolvedName
                  (ImportedModule dependencyPath)
                  (exportNamespace exportName interface)
                  (mkIdentifier exportName)
              )
              cell
       in foldr insertExport env selectedExports
    _ -> env
  where
    dependencyPath = resolvedImportPath importDecl
    lookupCompiled path =
      findByPath (resolvedModulePath . compiledResolvedModule) path (compiledProgramModules compiledProgram)
    lookupRuntime path = findByPath runtimeModulePath path runtimeModules

publishEnvironment :: ResolvedNameOrigin -> ModuleInterface -> RuntimeEnv -> RuntimeEnv
publishEnvironment origin moduleInterface env =
  Map.fromList
    [ (ResolvedName origin (exportNamespace exportName moduleInterface) (mkIdentifier exportName), cell)
      | exportName <- Map.keys (interfaceValueTypes moduleInterface),
        Just cell <- [lookupExportCell origin exportName moduleInterface env]
    ]

publishExports :: ResolvedNameOrigin -> ModuleInterface -> RuntimeEnv -> Map Text RuntimeCell
publishExports origin moduleInterface env =
  Map.fromList
    [ (exportName, cell)
      | exportName <- Map.keys (interfaceValueTypes moduleInterface),
        Just cell <- [lookupExportCell origin exportName moduleInterface env]
    ]

lookupExportCell :: ResolvedNameOrigin -> Text -> ModuleInterface -> RuntimeEnv -> Maybe RuntimeCell
lookupExportCell origin exportName moduleInterface env =
  case Map.lookup expectedName env of
    Just cell -> Just cell
    Nothing -> lookupRendered exportName env
  where
    expectedName =
      case origin of
        AmbientPrelude -> sourceName (mkIdentifier exportName)
        _ -> ResolvedName origin (exportNamespace exportName moduleInterface) (mkIdentifier exportName)

lookupRendered :: Text -> RuntimeEnv -> Maybe RuntimeCell
lookupRendered targetName =
  go . Map.toList
  where
    go entries =
      case entries of
        [] -> Nothing
        (name, cell) : rest
          | renderName name == targetName || identifierText name == targetName -> Just cell
          | otherwise -> go rest

exportNamespace :: Text -> ModuleInterface -> NameNamespace
exportNamespace exportName moduleInterface =
  case Map.lookup exportName (interfaceValueTypes moduleInterface) of
    Just ConstructorTypeBinding {} -> ConstructorNamespace
    _ -> ValueNamespace

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
