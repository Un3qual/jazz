{-# LANGUAGE DataKinds #-}

-- | Evaluate a successfully analyzed program once in dependency order.
module Jazz.Compiler.ModuleRuntime
  ( RuntimeExport (..),
    RuntimeModule (..),
    RuntimeProgram (..),
    interpretAnalyzedProgram,
    evaluateAnalyzedProgram,
    evaluateAnalyzedProgramObserved,
    evaluateAnalyzedProgramWithHostObserved,
    lookupRuntimeModule,
  )
where

import Control.Monad.Trans.Except
  ( ExceptT (..),
    runExceptT,
  )
import Data.Foldable (toList)
import Data.Functor.Identity (runIdentity)
import Data.List (find)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import Data.Text (Text)
import Jazz.Compiler.AST
  ( CorePhase (..),
  )
import Jazz.Compiler.CoreIdentity (CapabilityId, ResolvedReference (..), capabilityExportName)
import Jazz.Compiler.Diagnostics
  ( Diagnostic,
  )
import Jazz.Compiler.ModuleCompiler (analyzedProgramErrors)
import Jazz.Compiler.ModuleExports
  ( ModuleExport (..),
    inventoryHasExport,
  )
import Jazz.Compiler.ModuleGraph
  ( AnalyzedModuleFacts (..),
    CoreModule (..),
    CoreProgram,
    ModuleImport,
    PreludeArtifact (..),
    coreModuleExpr,
    coreModuleFacts,
    coreModuleImports,
    coreModulePath,
    coreProgramEntry,
    coreProgramModules,
    coreProgramPrelude,
    lookupCoreModule,
  )
import qualified Jazz.Compiler.ModuleGraph as ModuleGraph
import Jazz.Compiler.ModuleIdentity (ModulePath, modulePathTextSegments)
import Jazz.Compiler.ModuleImportScope (ValidatedImportScope, dependencyImportViews)
import Jazz.Compiler.ModuleInterface
  ( ModuleInterface (..),
    ModuleValueBinding (..),
  )
import Jazz.Compiler.Name
  ( Identifier,
    NameNamespace (..),
  )
import Jazz.Compiler.Runtime
  ( ModuleEvaluationMode (..),
    RuntimeCell,
    RuntimeEnv,
    RuntimeHostEvaluationT,
    RuntimeValue,
    ScopeResult (..),
    evaluateModuleScopePure,
    evaluateModuleScopeWithRequiredEvaluationHostControl,
    runRuntimeHostEvaluation,
    runRuntimeHostEvaluationWithObservation,
    runtimeExprRequiresHost,
  )
import Jazz.Compiler.Runtime.Observation
  ( RuntimeObservationRequest (..),
    RuntimeObservationResult (..),
    finishRuntimeObservationResult,
  )
import Jazz.Compiler.Runtime.Outcome
  ( RuntimeControl (..),
    RuntimeOutcome (..),
    diagnosticResultOutcome,
    runtimeControlOutcome,
    runtimeOutcomeAsDiagnosticResult,
  )
import Jazz.Compiler.RuntimeHost
  ( RuntimeHost,
    disabledRuntimeHost,
  )
import Jazz.Compiler.SourceProgram (isStandaloneSourceModule)

-- | Runtime-facing exports keep capability methods structurally distinct from
-- ordinary values instead of encoding their owner in a value-name string.
data RuntimeExport
  = RuntimeBindingExport ModuleExport
  | RuntimeCapabilityMethodExport
      { runtimeExportCapability :: CapabilityId,
        runtimeExportMethod :: Identifier
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

data RuntimeModuleAccumulator = RuntimeModuleAccumulator
  { accumulatedRuntimeModules :: !(Seq.Seq RuntimeModule),
    accumulatedRuntimeModulesByPath :: !(Map ModulePath RuntimeModule)
  }

data PreparedModuleEvaluation = PreparedModuleEvaluation
  { preparedModulePath :: ModulePath,
    preparedModuleEvaluationMode :: ModuleEvaluationMode,
    preparedModuleImportedEnvironment :: RuntimeEnv
  }

lookupRuntimeModule :: [Text] -> RuntimeProgram -> Maybe RuntimeModule
lookupRuntimeModule modulePath =
  find ((== modulePath) . runtimeModulePath) . runtimeProgramModules

evaluateAnalyzedProgram :: CoreProgram 'Analyzed -> Either Diagnostic RuntimeProgram
evaluateAnalyzedProgram =
  runtimeOutcomeAsDiagnosticResult
    . runtimeObservationOutcome
    . evaluateAnalyzedProgramObserved RuntimeObservationDisabled

evaluateAnalyzedProgramObserved :: RuntimeObservationRequest -> CoreProgram 'Analyzed -> RuntimeObservationResult RuntimeProgram
evaluateAnalyzedProgramObserved observationRequest analyzedProgram =
  runIdentity
    (interpretAnalyzedProgram observationRequest disabledRuntimeHost analyzedProgram)

evaluateAnalyzedProgramPureUnchecked :: CoreProgram 'Analyzed -> Either Diagnostic RuntimeProgram
evaluateAnalyzedProgramPureUnchecked analyzedProgram = do
  (ambientEnv, preludeValue) <- evaluatePrelude (preludeEvaluationMode analyzedProgram) (coreProgramPrelude analyzedProgram)
  evaluateModules ambientEnv emptyRuntimeModuleAccumulator preludeValue (NonEmpty.toList (coreProgramModules analyzedProgram))
  where
    entryPath = coreProgramEntry analyzedProgram

    evaluateModules ambientEnv runtimeModules output remainingModules =
      case remainingModules of
        [] ->
          Right
            (finishRuntimeProgram runtimeModules output)
        analyzedModule : rest -> do
          let preparedModule =
                prepareModuleEvaluation entryPath analyzedProgram ambientEnv runtimeModules analyzedModule
          scopeResult <-
            evaluateModuleScopePure
              (preparedModuleEvaluationMode preparedModule)
              (preparedModuleImportedEnvironment preparedModule)
              (coreModuleExpr analyzedModule)
          let (nextRuntimeModules, nextOutput) =
                completeModuleEvaluation preparedModule analyzedModule scopeResult runtimeModules output
          evaluateModules ambientEnv nextRuntimeModules nextOutput rest

evaluatePrelude :: ModuleEvaluationMode -> PreludeArtifact 'Analyzed -> Either Diagnostic (RuntimeEnv, Maybe RuntimeValue)
evaluatePrelude mode analyzedPrelude =
  case preludeModule analyzedPrelude of
    Nothing -> Right (Map.empty, Nothing)
    Just analyzedModule -> do
      scopeResult <-
        evaluateModuleScopePure
          mode
          Map.empty
          (coreModuleExpr analyzedModule)
      pure
        ( publishEnvironment
            (coreModuleInterface analyzedModule)
            (scopeResultEnvironment scopeResult),
          scopeResultValue scopeResult
        )

evaluateAnalyzedProgramWithHostObserved ::
  (Monad m) =>
  RuntimeObservationRequest ->
  RuntimeHost m ->
  CoreProgram 'Analyzed ->
  m (RuntimeObservationResult RuntimeProgram)
evaluateAnalyzedProgramWithHostObserved = interpretAnalyzedProgram

interpretAnalyzedProgram ::
  (Monad m) =>
  RuntimeObservationRequest ->
  RuntimeHost m ->
  CoreProgram 'Analyzed ->
  m (RuntimeObservationResult RuntimeProgram)
interpretAnalyzedProgram observationRequest host analyzedProgram =
  {-# SCC "jazz-stage:evaluation" #-}
  case analyzedProgramErrors analyzedProgram of
    firstError : _ -> pure (RuntimeObservationResult (RuntimeOutcomeFailed firstError) Nothing)
    [] ->
      case observationRequest of
        RuntimeObservationDisabled -> do
          outcome <- evaluateAnalyzedProgramWithHostUnobserved host analyzedProgram
          pure (RuntimeObservationResult outcome Nothing)
        _ -> do
          (outcome, observationState) <-
            runRuntimeHostEvaluationWithObservation observationRequest host $ \evaluationHost ->
              evaluateAnalyzedProgramWithEvaluationHostUnchecked evaluationHost analyzedProgram
          pure (finishRuntimeObservationResult (runtimeControlOutcome outcome) observationState)

evaluateAnalyzedProgramWithHostUnobserved ::
  (Monad m) =>
  RuntimeHost m ->
  CoreProgram 'Analyzed ->
  m (RuntimeOutcome RuntimeProgram)
evaluateAnalyzedProgramWithHostUnobserved host analyzedProgram =
  if analyzedProgramRequiresHost analyzedProgram
    then
      runtimeControlOutcome
        <$> runRuntimeHostEvaluation
          host
          ( \evaluationHost ->
              evaluateAnalyzedProgramWithEvaluationHostUnchecked evaluationHost analyzedProgram
          )
    else pure (diagnosticResultOutcome (evaluateAnalyzedProgramPureUnchecked analyzedProgram))

evaluateAnalyzedProgramWithEvaluationHostUnchecked ::
  (Monad m) =>
  RuntimeHost (RuntimeHostEvaluationT m) ->
  CoreProgram 'Analyzed ->
  RuntimeHostEvaluationT m (Either RuntimeControl RuntimeProgram)
evaluateAnalyzedProgramWithEvaluationHostUnchecked evaluationHost analyzedProgram =
  runExceptT $ do
    (ambientEnv, preludeValue) <-
      ExceptT
        ( evaluatePreludeWithEvaluationHost
            evaluationHost
            (preludeEvaluationMode analyzedProgram)
            (coreProgramPrelude analyzedProgram)
        )
    evaluateModules ambientEnv emptyRuntimeModuleAccumulator preludeValue (NonEmpty.toList (coreProgramModules analyzedProgram))
  where
    entryPath = coreProgramEntry analyzedProgram

    evaluateModules ambientEnv runtimeModules output remainingModules =
      case remainingModules of
        [] ->
          pure
            (finishRuntimeProgram runtimeModules output)
        analyzedModule : rest -> do
          let preparedModule =
                prepareModuleEvaluation entryPath analyzedProgram ambientEnv runtimeModules analyzedModule
          scopeResult <-
            ExceptT
              ( evaluateModuleScopeWithRequiredEvaluationHostControl
                  evaluationHost
                  (preparedModuleEvaluationMode preparedModule)
                  (preparedModuleImportedEnvironment preparedModule)
                  (coreModuleExpr analyzedModule)
              )
          let (nextRuntimeModules, nextOutput) =
                completeModuleEvaluation preparedModule analyzedModule scopeResult runtimeModules output
          evaluateModules ambientEnv nextRuntimeModules nextOutput rest

prepareModuleEvaluation ::
  ModulePath ->
  CoreProgram 'Analyzed ->
  RuntimeEnv ->
  RuntimeModuleAccumulator ->
  CoreModule 'Analyzed ->
  PreparedModuleEvaluation
prepareModuleEvaluation entryPath analyzedProgram ambientEnv runtimeModules analyzedModule =
  PreparedModuleEvaluation
    { preparedModulePath = modulePath,
      preparedModuleEvaluationMode =
        if modulePath == entryPath
          then EvaluateEntryModule
          else EvaluateDependencyModule,
      preparedModuleImportedEnvironment =
        foldr
          (importRuntimeModule (analyzedModuleImportScope (coreModuleFacts analyzedModule)) analyzedProgram (accumulatedRuntimeModulesByPath runtimeModules))
          ambientEnv
          (coreModuleImports analyzedModule)
    }
  where
    modulePath = coreModulePath analyzedModule

completeModuleEvaluation ::
  PreparedModuleEvaluation ->
  CoreModule 'Analyzed ->
  ScopeResult ->
  RuntimeModuleAccumulator ->
  Maybe RuntimeValue ->
  (RuntimeModuleAccumulator, Maybe RuntimeValue)
completeModuleEvaluation preparedModule analyzedModule scopeResult runtimeModules output =
  ( accumulateRuntimeModule (preparedModulePath preparedModule) runtimeModule runtimeModules,
    case preparedModuleEvaluationMode preparedModule of
      EvaluateEntryModule | null (ModuleGraph.coreModuleStatements analyzedModule) -> output
      EvaluateEntryModule -> scopeResultValue scopeResult
      EvaluateDependencyModule -> output
  )
  where
    runtimeModule =
      RuntimeModule
        { runtimeModulePath = modulePathTexts (preparedModulePath preparedModule),
          runtimeModuleExports =
            publishExports
              (coreModuleInterface analyzedModule)
              (scopeResultEnvironment scopeResult)
        }

preludeEvaluationMode :: CoreProgram 'Analyzed -> ModuleEvaluationMode
preludeEvaluationMode program =
  if any isStandaloneSourceModule (coreProgramModules program)
    then EvaluateEntryModule
    else EvaluateDependencyModule

analyzedProgramRequiresHost :: CoreProgram 'Analyzed -> Bool
analyzedProgramRequiresHost analyzedProgram =
  maybe False (runtimeExprRequiresHost . coreModuleExpr) (preludeModule (coreProgramPrelude analyzedProgram))
    || any (runtimeExprRequiresHost . coreModuleExpr) (coreProgramModules analyzedProgram)

evaluatePreludeWithEvaluationHost ::
  (Monad m) =>
  RuntimeHost (RuntimeHostEvaluationT m) ->
  ModuleEvaluationMode ->
  PreludeArtifact 'Analyzed ->
  RuntimeHostEvaluationT m (Either RuntimeControl (RuntimeEnv, Maybe RuntimeValue))
evaluatePreludeWithEvaluationHost host mode analyzedPrelude =
  case preludeModule analyzedPrelude of
    Nothing -> pure (Right (Map.empty, Nothing))
    Just analyzedModule -> do
      scopeResult <-
        evaluateModuleScopeWithRequiredEvaluationHostControl
          host
          mode
          Map.empty
          (coreModuleExpr analyzedModule)
      pure $
        fmap
          ( \result ->
              ( publishEnvironment
                  (coreModuleInterface analyzedModule)
                  (scopeResultEnvironment result),
                scopeResultValue result
              )
          )
          scopeResult

importRuntimeModule :: ValidatedImportScope -> CoreProgram 'Analyzed -> Map ModulePath RuntimeModule -> ModuleImport 'Analyzed -> RuntimeEnv -> RuntimeEnv
importRuntimeModule scope analyzedProgram runtimeModules importDecl env =
  case (lookupCoreModule dependencyPath analyzedProgram, Map.lookup dependencyPath runtimeModules) of
    (Just analyzedDependency, Just runtimeDependency) ->
      let selectedExports =
            [ (runtimeExport, cell)
            | (runtimeExport, cell) <- Map.toList (runtimeModuleExports runtimeDependency),
              runtimeExportSelected scope dependencyPath runtimeExport
            ]
          insertExport (runtimeExport, cell) importedEnv =
            case exportReference (coreModuleInterface analyzedDependency) runtimeExport of
              Just reference -> Map.insert reference cell importedEnv
              Nothing -> importedEnv
       in foldr insertExport env selectedExports
    _ -> env
  where
    dependencyPath = ModuleGraph.importedModule importDecl

emptyRuntimeModuleAccumulator :: RuntimeModuleAccumulator
emptyRuntimeModuleAccumulator = RuntimeModuleAccumulator Seq.empty Map.empty

accumulateRuntimeModule :: ModulePath -> RuntimeModule -> RuntimeModuleAccumulator -> RuntimeModuleAccumulator
accumulateRuntimeModule modulePath runtimeModule runtimeModules =
  RuntimeModuleAccumulator
    { accumulatedRuntimeModules = accumulatedRuntimeModules runtimeModules Seq.|> runtimeModule,
      accumulatedRuntimeModulesByPath =
        Map.insertWith
          (\_ firstRuntimeModule -> firstRuntimeModule)
          modulePath
          runtimeModule
          (accumulatedRuntimeModulesByPath runtimeModules)
    }

finishRuntimeProgram :: RuntimeModuleAccumulator -> Maybe RuntimeValue -> RuntimeProgram
finishRuntimeProgram runtimeModules output =
  RuntimeProgram
    { runtimeProgramModules = toList (accumulatedRuntimeModules runtimeModules),
      runtimeProgramOutput = output
    }

coreModuleInterface :: CoreModule 'Analyzed -> ModuleInterface
coreModuleInterface = analyzedModuleInterface . coreModuleFacts

modulePathTexts :: ModulePath -> [Text]
modulePathTexts = NonEmpty.toList . modulePathTextSegments

publishEnvironment :: ModuleInterface -> RuntimeEnv -> RuntimeEnv
publishEnvironment interface env =
  Map.fromList
    [ (reference, cell)
    | runtimeExport <- interfaceExports interface,
      Just reference <- [exportReference interface runtimeExport],
      Just cell <- [Map.lookup reference env]
    ]

publishExports :: ModuleInterface -> RuntimeEnv -> Map RuntimeExport RuntimeCell
publishExports interface env =
  Map.fromList
    [ (runtimeExport, cell)
    | runtimeExport <- interfaceExports interface,
      Just reference <- [exportReference interface runtimeExport],
      Just cell <- [Map.lookup reference env]
    ]

exportReference :: ModuleInterface -> RuntimeExport -> Maybe ResolvedReference
exportReference interface runtimeExport = case runtimeExport of
  RuntimeBindingExport export ->
    LexicalReference . interfaceBindingId <$> Map.lookup export (interfaceValueBindings interface)
  RuntimeCapabilityMethodExport capability method ->
    Just (CapabilityMethodReference capability method)

interfaceExports :: ModuleInterface -> [RuntimeExport]
interfaceExports interface =
  map RuntimeBindingExport (Map.keys (interfaceValueBindings interface))
    <> map (uncurry RuntimeCapabilityMethodExport) (Map.keys (interfaceClassMethods interface))

runtimeExportSelected :: ValidatedImportScope -> ModulePath -> RuntimeExport -> Bool
runtimeExportSelected scope path runtimeExport =
  any (inventoryHasExport export . snd) (dependencyImportViews path scope)
  where
    export = case runtimeExport of
      RuntimeBindingExport binding -> binding
      RuntimeCapabilityMethodExport capability _ -> ModuleExport CapabilityNamespace (capabilityExportName capability)
