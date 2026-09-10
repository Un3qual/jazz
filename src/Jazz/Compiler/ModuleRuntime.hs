{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

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
  ( CorePhase (..),
    Expr (..),
    Statement,
  )
import Jazz.Compiler.CapabilityFacts (splitQualifiedMethodKey)
import Jazz.Compiler.DiagnosticCatalog (ErrorCode (E3021))
import Jazz.Compiler.Diagnostics
  ( Diagnostic,
  )
import Jazz.Compiler.ModuleCompiler (analyzedProgramErrors)
import Jazz.Compiler.ModuleExports
  ( ModuleExport (..),
    ModuleExportInventory,
    exportInventoryEntries,
    exportNamesInNamespace,
    exportOrigin,
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
  )
import Jazz.Compiler.Name
  ( Name (..),
    NameNamespace (..),
    ResolvedName,
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
    evaluateModuleScopeWithRequiredEvaluationHostControl,
    runRuntimeHostEvaluation,
    runRuntimeHostEvaluationWithObservation,
  )
import Jazz.Compiler.Runtime.Observation
  ( RuntimeObservationRequest (..),
    RuntimeObservationResult (..),
    finishRuntimeObservationResult,
  )
import Jazz.Compiler.Runtime.Outcome
  ( RuntimeControl (..),
    RuntimeOutcome (..),
    runtimeControlOutcome,
    runtimeOutcomeAsDiagnosticResult,
  )
import Jazz.Compiler.Runtime.Semantics (runtimeDiagnostic)
import Jazz.Compiler.Runtime.Types (RuntimeMethodCandidate (..), RuntimeValue (VQualifiedMethod))
import Jazz.Compiler.RuntimeHost
  ( RuntimeHost,
    disabledRuntimeHost,
  )
import Jazz.Compiler.SemanticFacts (EvidenceReference (..))
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
    preparedModuleImportedEnvironment :: RuntimeEnv,
    preparedModuleExports :: Set.Set RuntimeExport
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
  runtimeControlOutcome
    <$> runRuntimeHostEvaluation
      host
      (\evaluationHost -> evaluateAnalyzedProgramWithEvaluationHostUnchecked evaluationHost analyzedProgram)

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
                  (Just (NamedSourceUnit (preparedModulePath preparedModule)))
                  (preparedModuleEvaluationMode preparedModule)
                  (preparedModuleImportedEnvironment preparedModule)
                  (scopeStatements (coreModuleExpr analyzedModule))
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
        foldr importRuntimeExport ambientEnv importedExports,
      preparedModuleExports =
        Set.fromList $
          interfaceExports (moduleExportInventory analyzedModule) (coreModuleInterface analyzedModule)
            <> [ entry
               | (origin, entry, _) <- importedExports,
                 let declaration = runtimeExportDeclaration entry,
                 inventoryHasExport declaration (moduleExportInventory analyzedModule),
                 exportOrigin modulePath declaration (moduleExportInventory analyzedModule) == origin,
                 origin /= modulePath
               ]
    }
  where
    modulePath = coreModulePath analyzedModule
    importedExports =
      concatMap
        (selectedRuntimeExports analyzedProgram (accumulatedRuntimeModulesByPath runtimeModules))
        (coreModuleImports analyzedModule)

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
              (preparedModulePath preparedModule)
              (moduleExportInventory analyzedModule)
              (preparedModuleExports preparedModule)
              (scopeResultEnvironment scopeResult)
        }

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
          (scopeStatements (coreModuleExpr analyzedModule))
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

importRuntimeExport :: (ModulePath, RuntimeExport, RuntimeCell) -> RuntimeEnv -> RuntimeEnv
importRuntimeExport (origin, entry, cell) =
  case entry of
    RuntimeBindingExport {} -> Map.insert name cell
    RuntimeCapabilityMethodExport {} -> Map.insertWith mergeClassMethodCells name cell
  where
    name = runtimeExportResolvedName (ImportedModule origin) entry

-- Each route can add implementations of the same original class. Keep their
-- captured cells and deduplicate only repeated method identities.
mergeClassMethodCells :: RuntimeCell -> RuntimeCell -> RuntimeCell
mergeClassMethodCells left right = do
  leftValue <- left
  rightValue <- right
  case (leftValue, rightValue) of
    (VQualifiedMethod key parameter signature candidates args, VQualifiedMethod _ _ _ otherCandidates _) ->
      Right (VQualifiedMethod key parameter signature (candidates <> filter ((`Set.notMember` identities) . candidateIdentity) otherCandidates) args)
      where
        identities = Set.fromList (map candidateIdentity candidates)
    _ -> Left (runtimeDiagnostic E3021 "imported class method is missing its runtime signature")
  where
    candidateIdentity (RuntimeMethodCandidate evidence _) = (evidenceImplementation evidence, evidenceMethod evidence)

selectedRuntimeExports :: CoreProgram 'Analyzed -> Map ModulePath RuntimeModule -> ModuleImport 'Analyzed -> [(ModulePath, RuntimeExport, RuntimeCell)]
selectedRuntimeExports analyzedProgram runtimeModules importDecl =
  case (lookupCoreModule dependencyPath analyzedProgram, Map.lookup dependencyPath runtimeModules) of
    (Just analyzedDependency, Just runtimeDependency) ->
      let inventory = moduleExportInventory analyzedDependency
       in [ (exportOrigin dependencyPath (runtimeExportDeclaration entry) inventory, entry, cell)
          | (entry, cell) <- Map.toList (runtimeModuleExports runtimeDependency),
            runtimeExportSelected importDecl inventory entry
          ]
    _ -> []
  where
    dependencyPath = ModuleGraph.importedModule importDecl

runtimeExportDeclaration :: RuntimeExport -> ModuleExport
runtimeExportDeclaration runtimeExport = case runtimeExport of
  RuntimeBindingExport entry -> entry
  RuntimeCapabilityMethodExport className _ -> ModuleExport CapabilityNamespace className

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
    [ (runtimeExportResolvedName origin runtimeExport, cell)
    | runtimeExport <- interfaceExports publicInventory moduleInterface,
      Just cell <- [lookupExportCell CurrentModule runtimeExport env]
    ]

publishExports :: ModulePath -> ModuleExportInventory -> Set.Set RuntimeExport -> RuntimeEnv -> Map RuntimeExport RuntimeCell
publishExports modulePath publicInventory exports env =
  Map.fromList
    [ (runtimeExport, cell)
    | runtimeExport <- Set.toList exports,
      let owner = exportOrigin modulePath (runtimeExportDeclaration runtimeExport) publicInventory,
      let origin = if owner == modulePath then CurrentModule else ImportedModule owner,
      Just cell <- [lookupExportCell origin runtimeExport env]
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

runtimeExportResolvedName :: ResolvedNameOrigin -> RuntimeExport -> ResolvedName
runtimeExportResolvedName origin runtimeExport =
  UserName (ResolvedUserName origin (runtimeExportNamespace runtimeExport) (mkIdentifier (runtimeExportName runtimeExport)))

lookupExportCell :: ResolvedNameOrigin -> RuntimeExport -> RuntimeEnv -> Maybe RuntimeCell
lookupExportCell origin runtimeExport = Map.lookup (runtimeExportResolvedName origin runtimeExport)

scopeStatements :: Expr 'Analyzed -> [Statement 'Analyzed]
scopeStatements expression =
  case expression of
    EBlock _ statements -> statements
    _ -> []
