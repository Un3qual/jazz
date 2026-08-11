{-# LANGUAGE OverloadedStrings #-}

-- | Evaluate a successful compiled program once in dependency order.
module Jazz.Compiler.ModuleRuntime
  ( RuntimeExport (..),
    RuntimeModule (..),
    RuntimeProgram (..),
    evaluateCompiledProgram,
    evaluateCompiledProgramObserved,
    evaluateCompiledProgramWithHost,
    evaluateCompiledProgramWithHostObserved,
    lookupRuntimeModule,
    prepareRuntimeImportSelection,
    runtimeExportSelectedBy
  ) where

import Control.Monad.Trans.Except
  ( ExceptT (..),
    runExceptT,
    throwE
  )
import Data.Functor.Identity (runIdentity)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Jazz.Compiler.CapabilityFacts (splitQualifiedMethodKey)
import Jazz.Compiler.AST
  ( Expr (EBlock),
    Statement
  )
import Jazz.Compiler.DiagnosticCatalog (ErrorCode (..))
import Jazz.Compiler.Diagnostics
  ( Diagnostic,
    DiagnosticOrigin (..),
    mkErrorDiagnostic
  )
import Jazz.Compiler.ModuleGraph (ResolvedImport (..))
import Jazz.Compiler.ModuleExports
  ( ModuleExport (..),
    ModuleExportInventory,
    ModuleImportMode (..),
    exportInventoryEntries,
    exportNamesInNamespace,
    inventoryHasExport,
    visibleImportInventory
  )
import Jazz.Compiler.ModuleInterface
  ( CompiledModule (..),
    CompiledPrelude (..),
    CompiledProgram (..),
    ModuleInterface (..),
    compiledProgramErrors,
    moduleInterfaceExportInventory
  )
import Jazz.Compiler.Name
  ( Name (ResolvedName),
    NameNamespace (..),
    ResolvedNameOrigin (..),
    identifierText,
    mkIdentifier,
    renderName,
    sourceName
  )
import Jazz.Compiler.Runtime
  ( ModuleEvaluationMode (..),
    RuntimeCell,
    RuntimeEnv,
    RuntimeHostEvaluationT,
    RuntimeControl (..),
    RuntimeValue,
    ScopeResult (..),
    evaluateModuleScope,
    evaluateModuleScopeWithRequiredEvaluationHostControl,
    runRuntimeHostEvaluation,
    runRuntimeHostEvaluationWithObservation,
    runtimeExprRequiresHost
  )
import Jazz.Compiler.Runtime.Observation
  ( RuntimeObservationRequest (..),
    RuntimeObservationResult (..),
    RuntimeOutcome (..),
    finishRuntimeObservationResult,
  )
import Jazz.Compiler.RuntimeHost
  ( RuntimeHost,
    disabledRuntimeHost
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

data RuntimeImportSelection = RuntimeImportSelection
  { runtimeImportSelectedInventory :: !ModuleExportInventory,
    runtimeImportSelectedCapabilityNames :: !(Set.Set Text),
    runtimeImportAllowsCapabilityMethods :: !Bool
  }

data RuntimeModule = RuntimeModule
  { runtimeModulePath :: [Text],
    runtimeModuleExports :: Map RuntimeExport RuntimeCell
  }

data RuntimeProgram = RuntimeProgram
  { runtimeProgramModules :: [RuntimeModule],
    runtimeProgramOutput :: Maybe RuntimeValue
  }

data RuntimeModuleAccumulator = RuntimeModuleAccumulator
  { accumulatedRuntimeModulesReversed :: ![RuntimeModule],
    accumulatedRuntimeModulesByPath :: !(Map [Text] RuntimeModule)
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
evaluateCompiledProgram =
  runtimeOutcomeAsDiagnosticResult . runtimeObservationOutcome
    . evaluateCompiledProgramObserved RuntimeObservationDisabled

evaluateCompiledProgramObserved :: RuntimeObservationRequest -> CompiledProgram -> RuntimeObservationResult RuntimeProgram
evaluateCompiledProgramObserved observationRequest compiledProgram =
  runIdentity
    (evaluateCompiledProgramWithHostObserved observationRequest disabledRuntimeHost compiledProgram)

evaluateCompiledProgramPure :: CompiledProgram -> Either Diagnostic RuntimeProgram
evaluateCompiledProgramPure compiledProgram =
  case compiledProgramErrors compiledProgram of
    firstError : _ -> Left firstError
    [] -> do
      ambientEnv <- evaluatePrelude (compiledProgramPrelude compiledProgram)
      evaluateModules compiledModulesByPath ambientEnv emptyRuntimeModuleAccumulator Nothing (compiledProgramModules compiledProgram)
  where
    entryPath = compiledProgramEntryPath compiledProgram
    compiledModulesByPath = buildCompiledModulePathIndex compiledProgram

    evaluateModules compiledModules ambientEnv runtimeModules output remainingModules =
      case remainingModules of
        [] ->
          Right
            (finishRuntimeProgram runtimeModules output)
        compiledModule : rest -> do
          let modulePath = compiledModulePath compiledModule
              evaluationMode =
                if modulePath == entryPath
                  then EvaluateEntryModule
                  else EvaluateDependencyModule
              importedEnv =
                foldr
                  (importRuntimeModule compiledModules (accumulatedRuntimeModulesByPath runtimeModules))
                  ambientEnv
                  (compiledModuleImports compiledModule)
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
                        (compiledModuleExportInventory compiledModule)
                        (compiledModuleInterface compiledModule)
                        (scopeResultEnvironment scopeResult)
                  }
              nextOutput =
                if modulePath == entryPath
                  then scopeResultValue scopeResult
                  else output
          evaluateModules compiledModules ambientEnv (accumulateRuntimeModule runtimeModule runtimeModules) nextOutput rest

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

evaluateCompiledProgramWithHost ::
  Monad m =>
  RuntimeHost m ->
  CompiledProgram ->
  m (Either Diagnostic RuntimeProgram)
evaluateCompiledProgramWithHost host compiledProgram =
  runtimeOutcomeAsDiagnosticResult . runtimeObservationOutcome
    <$> evaluateCompiledProgramWithHostObserved RuntimeObservationDisabled host compiledProgram

evaluateCompiledProgramWithHostObserved ::
  Monad m =>
  RuntimeObservationRequest ->
  RuntimeHost m ->
  CompiledProgram ->
  m (RuntimeObservationResult RuntimeProgram)
evaluateCompiledProgramWithHostObserved observationRequest host compiledProgram =
  {-# SCC "jazz-stage:evaluation" #-}
  case compiledProgramErrors compiledProgram of
    firstError : _ -> pure (RuntimeObservationResult (RuntimeOutcomeFailed firstError) Nothing)
    [] ->
      case observationRequest of
        RuntimeObservationDisabled -> do
          outcome <- evaluateCompiledProgramWithHostUnobserved host compiledProgram
          pure (RuntimeObservationResult outcome Nothing)
        _ -> do
          (outcome, observationState) <-
            runRuntimeHostEvaluationWithObservation observationRequest host $ \evaluationHost ->
              evaluateCompiledProgramWithEvaluationHost evaluationHost compiledProgram
          pure (finishRuntimeObservationResult (runtimeControlOutcome outcome) observationState)

evaluateCompiledProgramWithHostUnobserved ::
  Monad m =>
  RuntimeHost m ->
  CompiledProgram ->
  m (RuntimeOutcome RuntimeProgram)
evaluateCompiledProgramWithHostUnobserved host compiledProgram =
  if compiledProgramRequiresHost compiledProgram
    then
      runtimeControlOutcome
        <$> runRuntimeHostEvaluation host (\evaluationHost ->
          evaluateCompiledProgramWithEvaluationHost evaluationHost compiledProgram)
    else pure (diagnosticResultOutcome (evaluateCompiledProgramPure compiledProgram))

evaluateCompiledProgramWithEvaluationHost ::
  Monad m =>
  RuntimeHost (RuntimeHostEvaluationT m) ->
  CompiledProgram ->
  RuntimeHostEvaluationT m (Either RuntimeControl RuntimeProgram)
evaluateCompiledProgramWithEvaluationHost evaluationHost compiledProgram =
  runExceptT $
    case compiledProgramErrors compiledProgram of
      firstError : _ -> throwE (RuntimeDiagnostic firstError)
      [] -> do
        ambientEnv <- ExceptT (evaluatePreludeWithEvaluationHost evaluationHost (compiledProgramPrelude compiledProgram))
        evaluateModules compiledModulesByPath ambientEnv emptyRuntimeModuleAccumulator Nothing (compiledProgramModules compiledProgram)
  where
    entryPath = compiledProgramEntryPath compiledProgram
    compiledModulesByPath = buildCompiledModulePathIndex compiledProgram

    evaluateModules compiledModules ambientEnv runtimeModules output remainingModules =
      case remainingModules of
        [] ->
          pure
            (finishRuntimeProgram runtimeModules output)
        compiledModule : rest -> do
          let modulePath = compiledModulePath compiledModule
              evaluationMode =
                if modulePath == entryPath
                  then EvaluateEntryModule
                  else EvaluateDependencyModule
              importedEnv =
                foldr
                  (importRuntimeModule compiledModules (accumulatedRuntimeModulesByPath runtimeModules))
                  ambientEnv
                  (compiledModuleImports compiledModule)
          scopeResult <-
            ExceptT
              ( evaluateModuleScopeWithRequiredEvaluationHostControl
                  evaluationHost
                  (Just modulePath)
                  evaluationMode
                  (compiledPreludeBuiltinMode (compiledProgramPrelude compiledProgram))
                  (interfaceRuntimeHints (compiledModuleInterface compiledModule))
                  importedEnv
                  (scopeStatements (compiledModuleExpr compiledModule))
              )
          let runtimeModule =
                RuntimeModule
                  { runtimeModulePath = modulePath,
                    runtimeModuleExports =
                      publishExports
                        CurrentModule
                        (compiledModuleExportInventory compiledModule)
                        (compiledModuleInterface compiledModule)
                        (scopeResultEnvironment scopeResult)
                  }
              nextOutput =
                if modulePath == entryPath
                  then scopeResultValue scopeResult
                  else output
          evaluateModules compiledModules ambientEnv (accumulateRuntimeModule runtimeModule runtimeModules) nextOutput rest

compiledProgramRequiresHost :: CompiledProgram -> Bool
compiledProgramRequiresHost compiledProgram =
  maybe False runtimeExprRequiresHost (compiledPreludeExpr (compiledProgramPrelude compiledProgram))
    || any (runtimeExprRequiresHost . compiledModuleExpr) (compiledProgramModules compiledProgram)

evaluatePreludeWithEvaluationHost ::
  Monad m =>
  RuntimeHost (RuntimeHostEvaluationT m) ->
  CompiledPrelude ->
  RuntimeHostEvaluationT m (Either RuntimeControl RuntimeEnv)
evaluatePreludeWithEvaluationHost host compiledPrelude =
  case compiledPreludeExpr compiledPrelude of
    Nothing -> pure (Right Map.empty)
    Just expression -> do
      scopeResult <-
        evaluateModuleScopeWithRequiredEvaluationHostControl
          host
          (Just [])
          EvaluateDependencyModule
          (compiledPreludeBuiltinMode compiledPrelude)
          (compiledPreludeRuntimeHints compiledPrelude)
          Map.empty
          (scopeStatements expression)
      pure $
        fmap
          ( \result ->
              publishEnvironment
                AmbientPrelude
                (moduleInterfaceExportInventory (compiledPreludeInterface compiledPrelude))
                (compiledPreludeInterface compiledPrelude)
                (scopeResultEnvironment result)
          )
          scopeResult

runtimeControlOutcome :: Either RuntimeControl value -> RuntimeOutcome value
runtimeControlOutcome controlResult =
  case controlResult of
    Left (RuntimeDiagnostic diagnostic) -> RuntimeOutcomeFailed diagnostic
    Left (RuntimeExitRequested status) -> RuntimeOutcomeExited status
    Right value -> RuntimeOutcomeCompleted value

diagnosticResultOutcome :: Either Diagnostic value -> RuntimeOutcome value
diagnosticResultOutcome result =
  case result of
    Left diagnostic -> RuntimeOutcomeFailed diagnostic
    Right value -> RuntimeOutcomeCompleted value

runtimeOutcomeAsDiagnosticResult :: RuntimeOutcome value -> Either Diagnostic value
runtimeOutcomeAsDiagnosticResult outcome =
  case outcome of
    RuntimeOutcomeFailed diagnostic -> Left diagnostic
    RuntimeOutcomeExited status ->
      Left
        ( runtimeExitNotRepresentableDiagnostic status
        )
    RuntimeOutcomeCompleted value -> Right value

runtimeExitNotRepresentableDiagnostic :: Integer -> Diagnostic
runtimeExitNotRepresentableDiagnostic status =
  mkErrorDiagnostic
    E3020
    RuntimeOrigin
    ("runtime exit status " <> Text.pack (show status) <> " cannot be represented by this legacy evaluator result")

importRuntimeModule :: Map [Text] CompiledModule -> Map [Text] RuntimeModule -> ResolvedImport -> RuntimeEnv -> RuntimeEnv
importRuntimeModule compiledModules runtimeModules importDecl env =
  case (Map.lookup dependencyPath compiledModules, Map.lookup dependencyPath runtimeModules) of
    (Just compiledDependency, Just runtimeDependency) ->
      let publicInventory =
            compiledModuleExportInventory compiledDependency
          preparedSelection =
            prepareRuntimeImportSelection importDecl publicInventory
          selectedExports =
            [ (runtimeExport, cell)
              | (runtimeExport, cell) <- Map.toList (runtimeModuleExports runtimeDependency),
                runtimeExportSelectedBy preparedSelection runtimeExport
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

emptyRuntimeModuleAccumulator :: RuntimeModuleAccumulator
emptyRuntimeModuleAccumulator = RuntimeModuleAccumulator [] Map.empty

accumulateRuntimeModule :: RuntimeModule -> RuntimeModuleAccumulator -> RuntimeModuleAccumulator
accumulateRuntimeModule runtimeModule runtimeModules =
  RuntimeModuleAccumulator
    { accumulatedRuntimeModulesReversed = runtimeModule : accumulatedRuntimeModulesReversed runtimeModules,
      accumulatedRuntimeModulesByPath =
        Map.insertWith
          (\_ firstRuntimeModule -> firstRuntimeModule)
          (runtimeModulePath runtimeModule)
          runtimeModule
          (accumulatedRuntimeModulesByPath runtimeModules)
    }

finishRuntimeProgram :: RuntimeModuleAccumulator -> Maybe RuntimeValue -> RuntimeProgram
finishRuntimeProgram runtimeModules output =
  RuntimeProgram
    { runtimeProgramModules = reverse (accumulatedRuntimeModulesReversed runtimeModules),
      runtimeProgramOutput = output
    }

buildCompiledModulePathIndex :: CompiledProgram -> Map [Text] CompiledModule
buildCompiledModulePathIndex =
  Map.fromListWith (\_ firstCompiledModule -> firstCompiledModule)
    . map
      (\compiledModule -> (compiledModulePath compiledModule, compiledModule))
    . compiledProgramModules

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

prepareRuntimeImportSelection :: ResolvedImport -> ModuleExportInventory -> RuntimeImportSelection
prepareRuntimeImportSelection importDecl publicInventory =
  RuntimeImportSelection
    { runtimeImportSelectedInventory = selectedInventory,
      runtimeImportSelectedCapabilityNames =
        exportNamesInNamespace CapabilityNamespace selectedInventory,
      runtimeImportAllowsCapabilityMethods =
        resolvedImportAlias importDecl == Nothing
    }
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

runtimeExportSelectedBy :: RuntimeImportSelection -> RuntimeExport -> Bool
runtimeExportSelectedBy preparedSelection runtimeExport =
  case runtimeExport of
    RuntimeCapabilityMethodExport className _ ->
      runtimeImportAllowsCapabilityMethods preparedSelection
        && Set.member
          className
          (runtimeImportSelectedCapabilityNames preparedSelection)
    RuntimeBindingExport moduleExport ->
      inventoryHasExport
        moduleExport
        (runtimeImportSelectedInventory preparedSelection)

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
