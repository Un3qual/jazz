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
import qualified Data.Set as Set
import Data.Text (Text)
import Jazz.Compiler.AST
  ( CoreNode (coreNodeFacts),
    CorePhase (..),
  )
import Jazz.Compiler.CapabilityFacts (splitQualifiedMethodKey)
import Jazz.Compiler.CoreIdentity (CapabilityId (..), ResolvedNodeFacts (resolvedNodeOwner), ResolvedReference (..))
import Jazz.Compiler.Diagnostics
  ( Diagnostic,
  )
import Jazz.Compiler.ModuleCompiler (analyzedProgramErrors)
import Jazz.Compiler.ModuleExports
  ( ModuleExport (..),
    ModuleExportInventory,
    exportInventoryEntries,
    exportNamesInNamespace,
    inventoryHasExport,
    selectExportNames,
  )
import Jazz.Compiler.ModuleGraph
  ( AnalyzedModuleFacts (..),
    CoreModule (..),
    CoreProgram,
    ImportExposure (..),
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
import Jazz.Compiler.ModuleInterface
  ( ModuleInterface (..),
    ModuleValueBinding (..),
  )
import Jazz.Compiler.Name
  ( Name (..),
    NameNamespace (..),
    ResolvedNameOrigin (..),
    ResolvedUserName (..),
    identifierText,
    mkIdentifier,
  )
import Jazz.Compiler.Runtime
  ( ModuleEvaluationMode (..),
    RuntimeCell,
    RuntimeEnv,
    RuntimeHostEvaluationT,
    RuntimeValue,
    ScopeResult (..),
    evaluateModuleScope,
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
import Jazz.Compiler.SemanticFacts (ExpressionFacts (expressionResolution))
import Jazz.Compiler.SourceUnitOwnership (SourceUnitOwner (..))

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
  ambientEnv <- evaluatePrelude (coreProgramPrelude analyzedProgram)
  evaluateModules ambientEnv emptyRuntimeModuleAccumulator Nothing (NonEmpty.toList (coreProgramModules analyzedProgram))
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
            evaluateModuleScope
              (Just (resolvedNodeOwner (expressionResolution (coreNodeFacts (ModuleGraph.coreModuleBodyNode analyzedModule)))))
              (preparedModuleEvaluationMode preparedModule)
              (preparedModuleImportedEnvironment preparedModule)
              (coreModuleExpr analyzedModule)
          let (nextRuntimeModules, nextOutput) =
                completeModuleEvaluation preparedModule analyzedModule scopeResult runtimeModules output
          evaluateModules ambientEnv nextRuntimeModules nextOutput rest

evaluatePrelude :: PreludeArtifact 'Analyzed -> Either Diagnostic RuntimeEnv
evaluatePrelude analyzedPrelude =
  case preludeModule analyzedPrelude of
    Nothing -> Right Map.empty
    Just analyzedModule -> do
      scopeResult <-
        evaluateModuleScope
          (Just (PreludeSourceUnit (coreModulePath analyzedModule)))
          EvaluateDependencyModule
          Map.empty
          (coreModuleExpr analyzedModule)
      pure
        ( publishEnvironment
            AmbientPrelude
            (moduleExportInventory analyzedModule)
            (coreModuleInterface analyzedModule)
            (scopeResultEnvironment scopeResult)
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
    ambientEnv <-
      ExceptT
        ( evaluatePreludeWithEvaluationHost
            evaluationHost
            (coreProgramPrelude analyzedProgram)
        )
    evaluateModules ambientEnv emptyRuntimeModuleAccumulator Nothing (NonEmpty.toList (coreProgramModules analyzedProgram))
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
                  (Just (resolvedNodeOwner (expressionResolution (coreNodeFacts (ModuleGraph.coreModuleBodyNode analyzedModule)))))
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
          (importRuntimeModule analyzedProgram (accumulatedRuntimeModulesByPath runtimeModules))
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
      EvaluateEntryModule -> scopeResultValue scopeResult
      EvaluateDependencyModule -> output
  )
  where
    runtimeModule =
      RuntimeModule
        { runtimeModulePath = modulePathTexts (preparedModulePath preparedModule),
          runtimeModuleExports =
            publishExports
              (ImportedModule (preparedModulePath preparedModule))
              (moduleExportInventory analyzedModule)
              (coreModuleInterface analyzedModule)
              (scopeResultEnvironment scopeResult)
        }

analyzedProgramRequiresHost :: CoreProgram 'Analyzed -> Bool
analyzedProgramRequiresHost analyzedProgram =
  maybe False (runtimeExprRequiresHost . coreModuleExpr) (preludeModule (coreProgramPrelude analyzedProgram))
    || any (runtimeExprRequiresHost . coreModuleExpr) (coreProgramModules analyzedProgram)

evaluatePreludeWithEvaluationHost ::
  (Monad m) =>
  RuntimeHost (RuntimeHostEvaluationT m) ->
  PreludeArtifact 'Analyzed ->
  RuntimeHostEvaluationT m (Either RuntimeControl RuntimeEnv)
evaluatePreludeWithEvaluationHost host analyzedPrelude =
  case preludeModule analyzedPrelude of
    Nothing -> pure (Right Map.empty)
    Just analyzedModule -> do
      scopeResult <-
        evaluateModuleScopeWithRequiredEvaluationHostControl
          host
          (Just (PreludeSourceUnit (coreModulePath analyzedModule)))
          EvaluateDependencyModule
          Map.empty
          (coreModuleExpr analyzedModule)
      pure $
        fmap
          ( \result ->
              publishEnvironment
                AmbientPrelude
                (moduleExportInventory analyzedModule)
                (coreModuleInterface analyzedModule)
                (scopeResultEnvironment result)
          )
          scopeResult

importRuntimeModule :: CoreProgram 'Analyzed -> Map ModulePath RuntimeModule -> ModuleImport 'Analyzed -> RuntimeEnv -> RuntimeEnv
importRuntimeModule analyzedProgram runtimeModules importDecl env =
  case (lookupCoreModule dependencyPath analyzedProgram, Map.lookup dependencyPath runtimeModules) of
    (Just analyzedDependency, Just runtimeDependency) ->
      let publicInventory =
            moduleExportInventory analyzedDependency
          selectedExports =
            [ (runtimeExport, cell)
            | (runtimeExport, cell) <- Map.toList (runtimeModuleExports runtimeDependency),
              runtimeExportSelected importDecl publicInventory runtimeExport
            ]
          insertExport (runtimeExport, cell) importedEnv =
            case exportReference dependencyOrigin (coreModuleInterface analyzedDependency) runtimeExport of
              Just reference -> Map.insert reference cell importedEnv
              Nothing -> importedEnv
       in foldr insertExport env selectedExports
    _ -> env
  where
    dependencyPath = ModuleGraph.importedModule importDecl
    dependencyOrigin = ImportedModule dependencyPath

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

moduleExportInventory :: CoreModule 'Analyzed -> ModuleExportInventory
moduleExportInventory = analyzedModuleExports . coreModuleFacts

modulePathTexts :: ModulePath -> [Text]
modulePathTexts = NonEmpty.toList . modulePathTextSegments

publishEnvironment :: ResolvedNameOrigin -> ModuleExportInventory -> ModuleInterface -> RuntimeEnv -> RuntimeEnv
publishEnvironment origin publicInventory moduleInterface env =
  Map.fromList
    [ (reference, cell)
    | runtimeExport <- interfaceExports publicInventory moduleInterface,
      Just reference <- [exportReference origin moduleInterface runtimeExport],
      Just cell <- [Map.lookup reference env]
    ]

publishExports :: ResolvedNameOrigin -> ModuleExportInventory -> ModuleInterface -> RuntimeEnv -> Map RuntimeExport RuntimeCell
publishExports origin publicInventory moduleInterface env =
  Map.fromList
    [ (runtimeExport, cell)
    | runtimeExport <- interfaceExports publicInventory moduleInterface,
      Just reference <- [exportReference origin moduleInterface runtimeExport],
      Just cell <- [Map.lookup reference env]
    ]

exportReference :: ResolvedNameOrigin -> ModuleInterface -> RuntimeExport -> Maybe ResolvedReference
exportReference origin moduleInterface runtimeExport = case runtimeExport of
  RuntimeBindingExport export ->
    LexicalReference . interfaceBindingId <$> Map.lookup export (interfaceValueBindings moduleInterface)
  RuntimeCapabilityMethodExport capability method ->
    Just (CapabilityMethodReference (CapabilityId (UserName (ResolvedUserName origin CapabilityNamespace (mkIdentifier capability)))) (mkIdentifier method))

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

runtimeExportSelected :: ModuleImport 'Analyzed -> ModuleExportInventory -> RuntimeExport -> Bool
runtimeExportSelected importDecl publicInventory runtimeExport =
  case ModuleGraph.importExposure importDecl of
    ImportAllUnqualified -> selectedBy Nothing
    ImportOnlyUnqualified symbolNames -> selectedBy (Just (map identifierText (NonEmpty.toList symbolNames)))
    ImportQualifiedOnly _ -> selectedBy Nothing
  where
    selectedBy symbolNames =
      case runtimeExport of
        RuntimeCapabilityMethodExport className _ ->
          Set.member className selectedClassNames
        RuntimeBindingExport moduleExport ->
          inventoryHasExport moduleExport selectedInventory
      where
        selectedInventory =
          selectExportNames
            symbolNames
            publicInventory
        selectedClassNames =
          exportNamesInNamespace CapabilityNamespace selectedInventory
