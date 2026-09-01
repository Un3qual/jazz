{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Evaluate a successfully analyzed program once in dependency order.
module Jazz.Compiler.ModuleRuntime
  ( RuntimeExport (..),
    RuntimeModule (..),
    RuntimeProgram (..),
    evaluateAnalyzedProgram,
    evaluateAnalyzedProgramObserved,
    evaluateAnalyzedProgramWithHost,
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
  ( CaseArm (..),
    ClassMethodSignature (..),
    CoreNode (..),
    CorePhase (..),
    DataConstructor (..),
    Expr (..),
    ImplMethod (..),
    Pattern (..),
    Statement (..),
  )
import Jazz.Compiler.CapabilityFacts (splitQualifiedMethodKey)
import Jazz.Compiler.DiagnosticCatalog (ErrorCode (E3020))
import Jazz.Compiler.Diagnostics
  ( Diagnostic,
    DiagnosticOrigin (RuntimeOrigin),
    mkErrorDiagnostic,
  )
import Jazz.Compiler.ModuleCompiler (analyzedProgramErrors)
import Jazz.Compiler.ModuleExports
  ( ModuleExport (..),
    ModuleExportInventory,
    ModuleImportMode (..),
    exportInventoryEntries,
    exportNamesInNamespace,
    inventoryHasExport,
    visibleImportInventory,
  )
import Jazz.Compiler.ModuleGraph
  ( AnalyzedModuleFacts (..),
    CoreModule (..),
    CoreProgram,
    ImportExposure (..),
    ModuleImport,
    PreludeArtifact (..),
    ResolvedModuleFacts (..),
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
    ResolvedNameOrigin (..),
    ResolvedUserName (..),
    identifierText,
    mkIdentifier,
    renderName,
    resolvedAmbientName,
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

evaluateAnalyzedProgram :: CoreProgram 'Resolved -> CoreProgram 'Analyzed -> Either Diagnostic RuntimeProgram
evaluateAnalyzedProgram resolvedProgram =
  runtimeOutcomeAsDiagnosticResult
    . runtimeObservationOutcome
    . evaluateAnalyzedProgramObserved RuntimeObservationDisabled resolvedProgram

evaluateAnalyzedProgramObserved :: RuntimeObservationRequest -> CoreProgram 'Resolved -> CoreProgram 'Analyzed -> RuntimeObservationResult RuntimeProgram
evaluateAnalyzedProgramObserved observationRequest resolvedProgram analyzedProgram =
  runIdentity
    (evaluateAnalyzedProgramWithHostObserved observationRequest disabledRuntimeHost resolvedProgram analyzedProgram)

evaluateAnalyzedProgramPureUnchecked :: CoreProgram 'Resolved -> CoreProgram 'Analyzed -> Either Diagnostic RuntimeProgram
evaluateAnalyzedProgramPureUnchecked resolvedProgram analyzedProgram = do
  ambientEnv <- evaluatePrelude (coreProgramPrelude resolvedProgram) (coreProgramPrelude analyzedProgram)
  evaluateModules analyzedProgram ambientEnv emptyRuntimeModuleAccumulator Nothing (NonEmpty.toList (coreProgramModules resolvedProgram))
  where
    entryPath = coreProgramEntry resolvedProgram
    builtinMode = preludeBuiltinMode (coreProgramPrelude resolvedProgram)

    evaluateModules analyzed ambientEnv runtimeModules output remainingModules =
      case remainingModules of
        [] ->
          Right
            (finishRuntimeProgram runtimeModules output)
        resolvedModule : rest -> do
          analyzedModule <- requireAnalyzedModule resolvedModule analyzed
          let preparedModule =
                prepareModuleEvaluation entryPath analyzed ambientEnv runtimeModules resolvedModule
          scopeResult <-
            evaluateModuleScope
              (Just (modulePathTexts (preparedModulePath preparedModule)))
              (preparedModuleEvaluationMode preparedModule)
              builtinMode
              (interfaceRuntimeHints (coreModuleInterface analyzedModule))
              (preparedModuleImportedEnvironment preparedModule)
              (scopeStatements (coreModuleExpr resolvedModule))
          let (nextRuntimeModules, nextOutput) =
                completeModuleEvaluation preparedModule analyzedModule scopeResult runtimeModules output
          evaluateModules analyzed ambientEnv nextRuntimeModules nextOutput rest

evaluatePrelude :: PreludeArtifact 'Resolved -> PreludeArtifact 'Analyzed -> Either Diagnostic RuntimeEnv
evaluatePrelude resolvedPrelude analyzedPrelude =
  case (preludeModule resolvedPrelude, preludeModule analyzedPrelude) of
    (Nothing, Nothing) -> Right Map.empty
    (Just resolvedModule, Just analyzedModule) -> do
      scopeResult <-
        evaluateModuleScope
          (Just (modulePathTexts (coreModulePath resolvedModule)))
          EvaluateDependencyModule
          (preludeBuiltinMode resolvedPrelude)
          (interfaceRuntimeHints (coreModuleInterface analyzedModule))
          Map.empty
          (scopeStatements (coreModuleExpr resolvedModule))
      pure
        ( publishEnvironment
            AmbientPrelude
            (moduleExportInventory analyzedModule)
            (coreModuleInterface analyzedModule)
            (scopeResultEnvironment scopeResult)
        )
    _ -> analyzedProgramMismatch

evaluateAnalyzedProgramWithHost ::
  (Monad m) =>
  RuntimeHost m ->
  CoreProgram 'Resolved ->
  CoreProgram 'Analyzed ->
  m (Either Diagnostic RuntimeProgram)
evaluateAnalyzedProgramWithHost host resolvedProgram analyzedProgram =
  runtimeOutcomeAsDiagnosticResult . runtimeObservationOutcome
    <$> evaluateAnalyzedProgramWithHostObserved RuntimeObservationDisabled host resolvedProgram analyzedProgram

evaluateAnalyzedProgramWithHostObserved ::
  (Monad m) =>
  RuntimeObservationRequest ->
  RuntimeHost m ->
  CoreProgram 'Resolved ->
  CoreProgram 'Analyzed ->
  m (RuntimeObservationResult RuntimeProgram)
evaluateAnalyzedProgramWithHostObserved observationRequest host resolvedProgram analyzedProgram =
  {-# SCC "jazz-stage:evaluation" #-}
  case validateProgramCorrespondence resolvedProgram analyzedProgram of
    Left diagnostic -> pure (RuntimeObservationResult (RuntimeOutcomeFailed diagnostic) Nothing)
    Right () ->
      case analyzedProgramErrors analyzedProgram of
        firstError : _ -> pure (RuntimeObservationResult (RuntimeOutcomeFailed firstError) Nothing)
        [] ->
          case observationRequest of
            RuntimeObservationDisabled -> do
              outcome <- evaluateAnalyzedProgramWithHostUnobserved host resolvedProgram analyzedProgram
              pure (RuntimeObservationResult outcome Nothing)
            _ -> do
              (outcome, observationState) <-
                runRuntimeHostEvaluationWithObservation observationRequest host $ \evaluationHost ->
                  evaluateAnalyzedProgramWithEvaluationHostUnchecked evaluationHost resolvedProgram analyzedProgram
              pure (finishRuntimeObservationResult (runtimeControlOutcome outcome) observationState)

evaluateAnalyzedProgramWithHostUnobserved ::
  (Monad m) =>
  RuntimeHost m ->
  CoreProgram 'Resolved ->
  CoreProgram 'Analyzed ->
  m (RuntimeOutcome RuntimeProgram)
evaluateAnalyzedProgramWithHostUnobserved host resolvedProgram analyzedProgram =
  if resolvedProgramRequiresHost resolvedProgram
    then
      runtimeControlOutcome
        <$> runRuntimeHostEvaluation
          host
          ( \evaluationHost ->
              evaluateAnalyzedProgramWithEvaluationHostUnchecked evaluationHost resolvedProgram analyzedProgram
          )
    else pure (diagnosticResultOutcome (evaluateAnalyzedProgramPureUnchecked resolvedProgram analyzedProgram))

evaluateAnalyzedProgramWithEvaluationHostUnchecked ::
  (Monad m) =>
  RuntimeHost (RuntimeHostEvaluationT m) ->
  CoreProgram 'Resolved ->
  CoreProgram 'Analyzed ->
  RuntimeHostEvaluationT m (Either RuntimeControl RuntimeProgram)
evaluateAnalyzedProgramWithEvaluationHostUnchecked evaluationHost resolvedProgram analyzedProgram =
  runExceptT $ do
    ambientEnv <-
      ExceptT
        ( evaluatePreludeWithEvaluationHost
            evaluationHost
            (coreProgramPrelude resolvedProgram)
            (coreProgramPrelude analyzedProgram)
        )
    evaluateModules ambientEnv emptyRuntimeModuleAccumulator Nothing (NonEmpty.toList (coreProgramModules resolvedProgram))
  where
    entryPath = coreProgramEntry resolvedProgram
    builtinMode = preludeBuiltinMode (coreProgramPrelude resolvedProgram)

    evaluateModules ambientEnv runtimeModules output remainingModules =
      case remainingModules of
        [] ->
          pure
            (finishRuntimeProgram runtimeModules output)
        resolvedModule : rest -> do
          analyzedModule <- ExceptT (pure (requireAnalyzedModuleControl resolvedModule analyzedProgram))
          let preparedModule =
                prepareModuleEvaluation entryPath analyzedProgram ambientEnv runtimeModules resolvedModule
          scopeResult <-
            ExceptT
              ( evaluateModuleScopeWithRequiredEvaluationHostControl
                  evaluationHost
                  (Just (modulePathTexts (preparedModulePath preparedModule)))
                  (preparedModuleEvaluationMode preparedModule)
                  builtinMode
                  (interfaceRuntimeHints (coreModuleInterface analyzedModule))
                  (preparedModuleImportedEnvironment preparedModule)
                  (scopeStatements (coreModuleExpr resolvedModule))
              )
          let (nextRuntimeModules, nextOutput) =
                completeModuleEvaluation preparedModule analyzedModule scopeResult runtimeModules output
          evaluateModules ambientEnv nextRuntimeModules nextOutput rest

prepareModuleEvaluation ::
  ModulePath ->
  CoreProgram 'Analyzed ->
  RuntimeEnv ->
  RuntimeModuleAccumulator ->
  CoreModule 'Resolved ->
  PreparedModuleEvaluation
prepareModuleEvaluation entryPath analyzedProgram ambientEnv runtimeModules resolvedModule =
  PreparedModuleEvaluation
    { preparedModulePath = modulePath,
      preparedModuleEvaluationMode =
        if modulePath == entryPath
          then EvaluateEntryModule
          else EvaluateDependencyModule,
      preparedModuleImportedEnvironment =
        foldr
          (importRuntimeModule analyzedModules (accumulatedRuntimeModulesByPath runtimeModules))
          ambientEnv
          (coreModuleImports resolvedModule)
    }
  where
    modulePath = coreModulePath resolvedModule
    analyzedModules = buildAnalyzedModulePathIndex analyzedProgram

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
              CurrentModule
              (moduleExportInventory analyzedModule)
              (coreModuleInterface analyzedModule)
              (scopeResultEnvironment scopeResult)
        }

resolvedProgramRequiresHost :: CoreProgram 'Resolved -> Bool
resolvedProgramRequiresHost resolvedProgram =
  maybe False (runtimeExprRequiresHost . coreModuleExpr) (preludeModule (coreProgramPrelude resolvedProgram))
    || any (runtimeExprRequiresHost . coreModuleExpr) (coreProgramModules resolvedProgram)

evaluatePreludeWithEvaluationHost ::
  (Monad m) =>
  RuntimeHost (RuntimeHostEvaluationT m) ->
  PreludeArtifact 'Resolved ->
  PreludeArtifact 'Analyzed ->
  RuntimeHostEvaluationT m (Either RuntimeControl RuntimeEnv)
evaluatePreludeWithEvaluationHost host resolvedPrelude analyzedPrelude =
  case (preludeModule resolvedPrelude, preludeModule analyzedPrelude) of
    (Nothing, Nothing) -> pure (Right Map.empty)
    (Just resolvedModule, Just analyzedModule) -> do
      scopeResult <-
        evaluateModuleScopeWithRequiredEvaluationHostControl
          host
          (Just (modulePathTexts (coreModulePath resolvedModule)))
          EvaluateDependencyModule
          (preludeBuiltinMode resolvedPrelude)
          (interfaceRuntimeHints (coreModuleInterface analyzedModule))
          Map.empty
          (scopeStatements (coreModuleExpr resolvedModule))
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
    _ -> pure (either (Left . RuntimeDiagnostic) Right analyzedProgramMismatch)

importRuntimeModule :: Map ModulePath (CoreModule 'Analyzed) -> Map ModulePath RuntimeModule -> ModuleImport 'Resolved -> RuntimeEnv -> RuntimeEnv
importRuntimeModule analyzedModules runtimeModules importDecl env =
  case (Map.lookup dependencyPath analyzedModules, Map.lookup dependencyPath runtimeModules) of
    (Just analyzedDependency, Just runtimeDependency) ->
      let publicInventory =
            moduleExportInventory analyzedDependency
          selectedExports =
            [ (runtimeExport, cell)
            | (runtimeExport, cell) <- Map.toList (runtimeModuleExports runtimeDependency),
              runtimeExportSelected importDecl publicInventory runtimeExport
            ]
          insertExport (runtimeExport, cell) =
            Map.insert
              ( UserName
                  ( ResolvedUserName
                      dependencyOrigin
                      (runtimeExportNamespace runtimeExport)
                      (mkIdentifier (runtimeExportName runtimeExport))
                  )
              )
              cell
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

buildAnalyzedModulePathIndex :: CoreProgram 'Analyzed -> Map ModulePath (CoreModule 'Analyzed)
buildAnalyzedModulePathIndex =
  Map.fromListWith (\_ firstAnalyzedModule -> firstAnalyzedModule)
    . map
      (\analyzedModule -> (coreModulePath analyzedModule, analyzedModule))
    . NonEmpty.toList
    . coreProgramModules

coreModuleInterface :: CoreModule 'Analyzed -> ModuleInterface
coreModuleInterface = analyzedModuleInterface . coreModuleFacts

moduleExportInventory :: CoreModule 'Analyzed -> ModuleExportInventory
moduleExportInventory = analyzedModuleExports . coreModuleFacts

requireAnalyzedModule :: CoreModule 'Resolved -> CoreProgram 'Analyzed -> Either Diagnostic (CoreModule 'Analyzed)
requireAnalyzedModule resolvedModule analyzedProgram =
  case lookupCoreModule (coreModulePath resolvedModule) analyzedProgram of
    Just analyzedModule -> Right analyzedModule
    Nothing -> analyzedProgramMismatch

requireAnalyzedModuleControl :: CoreModule 'Resolved -> CoreProgram 'Analyzed -> Either RuntimeControl (CoreModule 'Analyzed)
requireAnalyzedModuleControl resolvedModule =
  either (Left . RuntimeDiagnostic) Right . requireAnalyzedModule resolvedModule

validateProgramCorrespondence :: CoreProgram 'Resolved -> CoreProgram 'Analyzed -> Either Diagnostic ()
validateProgramCorrespondence resolvedProgram analyzedProgram =
  if programsCorrespond
    then Right ()
    else analyzedProgramMismatch
  where
    programsCorrespond =
      coreProgramEntry resolvedProgram == coreProgramEntry analyzedProgram
        && preludesCorrespond
          (coreProgramPrelude resolvedProgram)
          (coreProgramPrelude analyzedProgram)
        && listsCorrespond
          modulesCorrespond
          (NonEmpty.toList (coreProgramModules resolvedProgram))
          (NonEmpty.toList (coreProgramModules analyzedProgram))

preludesCorrespond :: PreludeArtifact 'Resolved -> PreludeArtifact 'Analyzed -> Bool
preludesCorrespond resolvedPrelude analyzedPrelude =
  preludeIdentity resolvedPrelude == preludeIdentity analyzedPrelude
    && preludeBuiltinMode resolvedPrelude == preludeBuiltinMode analyzedPrelude
    && case (preludeModule resolvedPrelude, preludeModule analyzedPrelude) of
      (Nothing, Nothing) -> True
      (Just resolvedModule, Just analyzedModule) -> modulesCorrespond resolvedModule analyzedModule
      _ -> False

modulesCorrespond :: CoreModule 'Resolved -> CoreModule 'Analyzed -> Bool
modulesCorrespond resolvedModule analyzedModule =
  coreModuleIdentity resolvedModule == coreModuleIdentity analyzedModule
    && nodesCorrespond
      (coreModuleBodyNode resolvedModule)
      (coreModuleBodyNode analyzedModule)
    && listsCorrespond importsCorrespond (coreModuleImports resolvedModule) (coreModuleImports analyzedModule)
    && listsCorrespond statementsCorrespond (coreModuleStatements resolvedModule) (coreModuleStatements analyzedModule)
    && resolvedModuleExports resolvedFacts == analyzedModuleExports analyzedFacts
    && resolvedModuleExportSelectors resolvedFacts == analyzedModuleExportSelectors analyzedFacts
  where
    resolvedFacts = coreModuleFacts resolvedModule
    analyzedFacts = coreModuleFacts analyzedModule

importsCorrespond :: ModuleImport 'Resolved -> ModuleImport 'Analyzed -> Bool
importsCorrespond resolvedImport analyzedImport =
  nodesCorrespond
    (ModuleGraph.moduleImportNode resolvedImport)
    (ModuleGraph.moduleImportNode analyzedImport)
    && ModuleGraph.importedModule resolvedImport == ModuleGraph.importedModule analyzedImport
    && ModuleGraph.importAlias resolvedImport == ModuleGraph.importAlias analyzedImport
    && ModuleGraph.importExposure resolvedImport == ModuleGraph.importExposure analyzedImport

nodesCorrespond :: CoreNode 'Resolved sort -> CoreNode 'Analyzed sort -> Bool
nodesCorrespond resolvedNode analyzedNode =
  coreNodeId resolvedNode == coreNodeId analyzedNode
    && coreNodeSpan resolvedNode == coreNodeSpan analyzedNode

expressionsCorrespond :: Expr 'Resolved -> Expr 'Analyzed -> Bool
expressionsCorrespond resolvedExpression analyzedExpression =
  case (resolvedExpression, analyzedExpression) of
    (ELit resolvedNode resolvedLiteral, ELit analyzedNode analyzedLiteral) ->
      nodesCorrespond resolvedNode analyzedNode && resolvedLiteral == analyzedLiteral
    (EVar resolvedNode resolvedName, EVar analyzedNode analyzedName) ->
      nodesCorrespond resolvedNode analyzedNode && resolvedName == analyzedName
    (ELambda resolvedNode resolvedName resolvedBody, ELambda analyzedNode analyzedName analyzedBody) ->
      nodesCorrespond resolvedNode analyzedNode
        && resolvedName == analyzedName
        && expressionsCorrespond resolvedBody analyzedBody
    (EOperatorValue resolvedNode resolvedOperator, EOperatorValue analyzedNode analyzedOperator) ->
      nodesCorrespond resolvedNode analyzedNode && resolvedOperator == analyzedOperator
    (EList resolvedNode resolvedElements, EList analyzedNode analyzedElements) ->
      nodesCorrespond resolvedNode analyzedNode
        && listsCorrespond expressionsCorrespond resolvedElements analyzedElements
    (ETuple resolvedNode resolvedElements, ETuple analyzedNode analyzedElements) ->
      nodesCorrespond resolvedNode analyzedNode
        && listsCorrespond expressionsCorrespond resolvedElements analyzedElements
    (EApply resolvedNode resolvedFunction resolvedArgument, EApply analyzedNode analyzedFunction analyzedArgument) ->
      nodesCorrespond resolvedNode analyzedNode
        && expressionsCorrespond resolvedFunction analyzedFunction
        && expressionsCorrespond resolvedArgument analyzedArgument
    ( ETypeApplication resolvedNode resolvedFunction resolvedSpan resolvedType,
      ETypeApplication analyzedNode analyzedFunction analyzedSpan analyzedType
      ) ->
        nodesCorrespond resolvedNode analyzedNode
          && expressionsCorrespond resolvedFunction analyzedFunction
          && resolvedSpan == analyzedSpan
          && resolvedType == analyzedType
    ( EIf resolvedNode resolvedCondition resolvedThen resolvedElse,
      EIf analyzedNode analyzedCondition analyzedThen analyzedElse
      ) ->
        nodesCorrespond resolvedNode analyzedNode
          && expressionsCorrespond resolvedCondition analyzedCondition
          && expressionsCorrespond resolvedThen analyzedThen
          && expressionsCorrespond resolvedElse analyzedElse
    (EPatternCase resolvedNode resolvedScrutinee resolvedArms, EPatternCase analyzedNode analyzedScrutinee analyzedArms) ->
      nodesCorrespond resolvedNode analyzedNode
        && expressionsCorrespond resolvedScrutinee analyzedScrutinee
        && listsCorrespond caseArmsCorrespond resolvedArms analyzedArms
    (EBinary resolvedNode resolvedOperator resolvedLeft resolvedRight, EBinary analyzedNode analyzedOperator analyzedLeft analyzedRight) ->
      nodesCorrespond resolvedNode analyzedNode
        && resolvedOperator == analyzedOperator
        && expressionsCorrespond resolvedLeft analyzedLeft
        && expressionsCorrespond resolvedRight analyzedRight
    (ESectionLeft resolvedNode resolvedLeft resolvedOperator, ESectionLeft analyzedNode analyzedLeft analyzedOperator) ->
      nodesCorrespond resolvedNode analyzedNode
        && expressionsCorrespond resolvedLeft analyzedLeft
        && resolvedOperator == analyzedOperator
    (ESectionRight resolvedNode resolvedOperator resolvedRight, ESectionRight analyzedNode analyzedOperator analyzedRight) ->
      nodesCorrespond resolvedNode analyzedNode
        && resolvedOperator == analyzedOperator
        && expressionsCorrespond resolvedRight analyzedRight
    (EBlock resolvedNode resolvedStatements, EBlock analyzedNode analyzedStatements) ->
      nodesCorrespond resolvedNode analyzedNode
        && listsCorrespond statementsCorrespond resolvedStatements analyzedStatements
    _ -> False

caseArmsCorrespond :: CaseArm 'Resolved -> CaseArm 'Analyzed -> Bool
caseArmsCorrespond
  (CaseArm resolvedNode resolvedPattern resolvedGuard resolvedBody)
  (CaseArm analyzedNode analyzedPattern analyzedGuard analyzedBody) =
    nodesCorrespond resolvedNode analyzedNode
      && patternsCorrespond resolvedPattern analyzedPattern
      && maybesCorrespond expressionsCorrespond resolvedGuard analyzedGuard
      && expressionsCorrespond resolvedBody analyzedBody

patternsCorrespond :: Pattern 'Resolved -> Pattern 'Analyzed -> Bool
patternsCorrespond resolvedPattern analyzedPattern =
  case (resolvedPattern, analyzedPattern) of
    (PWildcard resolvedNode, PWildcard analyzedNode) -> nodesCorrespond resolvedNode analyzedNode
    (PVariable resolvedNode resolvedName, PVariable analyzedNode analyzedName) ->
      nodesCorrespond resolvedNode analyzedNode && resolvedName == analyzedName
    (PLiteral resolvedNode resolvedLiteral, PLiteral analyzedNode analyzedLiteral) ->
      nodesCorrespond resolvedNode analyzedNode && resolvedLiteral == analyzedLiteral
    (PConstructor resolvedNode resolvedName resolvedPatterns, PConstructor analyzedNode analyzedName analyzedPatterns) ->
      nodesCorrespond resolvedNode analyzedNode
        && resolvedName == analyzedName
        && listsCorrespond patternsCorrespond resolvedPatterns analyzedPatterns
    (PList resolvedNode resolvedPatterns, PList analyzedNode analyzedPatterns) ->
      nodesCorrespond resolvedNode analyzedNode
        && listsCorrespond patternsCorrespond resolvedPatterns analyzedPatterns
    (PConsList resolvedNode resolvedHead resolvedTail, PConsList analyzedNode analyzedHead analyzedTail) ->
      nodesCorrespond resolvedNode analyzedNode
        && patternsCorrespond resolvedHead analyzedHead
        && patternsCorrespond resolvedTail analyzedTail
    (PTuple resolvedNode resolvedPatterns, PTuple analyzedNode analyzedPatterns) ->
      nodesCorrespond resolvedNode analyzedNode
        && listsCorrespond patternsCorrespond resolvedPatterns analyzedPatterns
    (PAs resolvedNode resolvedName resolvedNested, PAs analyzedNode analyzedName analyzedNested) ->
      nodesCorrespond resolvedNode analyzedNode
        && resolvedName == analyzedName
        && patternsCorrespond resolvedNested analyzedNested
    (POr resolvedNode resolvedAlternatives, POr analyzedNode analyzedAlternatives) ->
      nodesCorrespond resolvedNode analyzedNode
        && listsCorrespond patternsCorrespond resolvedAlternatives analyzedAlternatives
    _ -> False

statementsCorrespond :: Statement 'Resolved -> Statement 'Analyzed -> Bool
statementsCorrespond resolvedStatement analyzedStatement =
  case (resolvedStatement, analyzedStatement) of
    (SLet resolvedNode resolvedName resolvedValue, SLet analyzedNode analyzedName analyzedValue) ->
      nodesCorrespond resolvedNode analyzedNode
        && resolvedName == analyzedName
        && expressionsCorrespond resolvedValue analyzedValue
    (SSignature resolvedNode resolvedName resolvedSignature, SSignature analyzedNode analyzedName analyzedSignature) ->
      nodesCorrespond resolvedNode analyzedNode
        && resolvedName == analyzedName
        && resolvedSignature == analyzedSignature
    ( SData resolvedNode resolvedName resolvedParameters resolvedConstructors,
      SData analyzedNode analyzedName analyzedParameters analyzedConstructors
      ) ->
        nodesCorrespond resolvedNode analyzedNode
          && resolvedName == analyzedName
          && resolvedParameters == analyzedParameters
          && listsCorrespond constructorsCorrespond resolvedConstructors analyzedConstructors
    ( SClass resolvedNode resolvedName resolvedParameters resolvedMethods,
      SClass analyzedNode analyzedName analyzedParameters analyzedMethods
      ) ->
        nodesCorrespond resolvedNode analyzedNode
          && resolvedName == analyzedName
          && resolvedParameters == analyzedParameters
          && listsCorrespond classMethodsCorrespond resolvedMethods analyzedMethods
    (SImpl resolvedNode resolvedName resolvedArguments resolvedMethods, SImpl analyzedNode analyzedName analyzedArguments analyzedMethods) ->
      nodesCorrespond resolvedNode analyzedNode
        && resolvedName == analyzedName
        && resolvedArguments == analyzedArguments
        && listsCorrespond implMethodsCorrespond resolvedMethods analyzedMethods
    (SModule resolvedNode resolvedPath, SModule analyzedNode analyzedPath) ->
      nodesCorrespond resolvedNode analyzedNode && resolvedPath == analyzedPath
    ( SImport resolvedNode resolvedPath resolvedAlias resolvedNames,
      SImport analyzedNode analyzedPath analyzedAlias analyzedNames
      ) ->
        nodesCorrespond resolvedNode analyzedNode
          && resolvedPath == analyzedPath
          && resolvedAlias == analyzedAlias
          && resolvedNames == analyzedNames
    (SExpr resolvedNode resolvedValue, SExpr analyzedNode analyzedValue) ->
      nodesCorrespond resolvedNode analyzedNode
        && expressionsCorrespond resolvedValue analyzedValue
    _ -> False

constructorsCorrespond :: DataConstructor 'Resolved -> DataConstructor 'Analyzed -> Bool
constructorsCorrespond
  (DataConstructor resolvedNode resolvedName resolvedArguments)
  (DataConstructor analyzedNode analyzedName analyzedArguments) =
    nodesCorrespond resolvedNode analyzedNode
      && resolvedName == analyzedName
      && resolvedArguments == analyzedArguments

classMethodsCorrespond :: ClassMethodSignature 'Resolved -> ClassMethodSignature 'Analyzed -> Bool
classMethodsCorrespond
  (ClassMethodSignature resolvedNode resolvedName resolvedSignature)
  (ClassMethodSignature analyzedNode analyzedName analyzedSignature) =
    nodesCorrespond resolvedNode analyzedNode
      && resolvedName == analyzedName
      && resolvedSignature == analyzedSignature

implMethodsCorrespond :: ImplMethod 'Resolved -> ImplMethod 'Analyzed -> Bool
implMethodsCorrespond
  (ImplMethod resolvedNode resolvedName resolvedBody)
  (ImplMethod analyzedNode analyzedName analyzedBody) =
    nodesCorrespond resolvedNode analyzedNode
      && resolvedName == analyzedName
      && expressionsCorrespond resolvedBody analyzedBody

listsCorrespond :: (left -> right -> Bool) -> [left] -> [right] -> Bool
listsCorrespond correspond leftValues rightValues =
  length leftValues == length rightValues
    && and (zipWith correspond leftValues rightValues)

maybesCorrespond :: (left -> right -> Bool) -> Maybe left -> Maybe right -> Bool
maybesCorrespond correspond leftValue rightValue =
  case (leftValue, rightValue) of
    (Nothing, Nothing) -> True
    (Just left, Just right) -> correspond left right
    _ -> False

-- This can only be reached when callers mix independently produced resolved
-- and analyzed programs. The driver always passes the two views from one
-- successful analysis.
analyzedProgramMismatch :: Either Diagnostic value
analyzedProgramMismatch =
  Left
    ( mkErrorDiagnostic
        E3020
        RuntimeOrigin
        "runtime preparation received non-corresponding resolved and analyzed programs"
    )

modulePathTexts :: ModulePath -> [Text]
modulePathTexts = NonEmpty.toList . modulePathTextSegments

publishEnvironment :: ResolvedNameOrigin -> ModuleExportInventory -> ModuleInterface -> RuntimeEnv -> RuntimeEnv
publishEnvironment origin publicInventory moduleInterface env =
  let renderedLookupIndex = buildRenderedLookupIndex env
   in Map.fromList
        [ (UserName (ResolvedUserName origin (runtimeExportNamespace runtimeExport) (mkIdentifier (runtimeExportName runtimeExport))), cell)
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

runtimeExportSelected :: ModuleImport 'Resolved -> ModuleExportInventory -> RuntimeExport -> Bool
runtimeExportSelected importDecl publicInventory runtimeExport =
  case ModuleGraph.importExposure importDecl of
    ImportAllUnqualified -> selectedBy UnqualifiedImport Nothing True
    ImportOnlyUnqualified symbolNames -> selectedBy UnqualifiedImport (Just (map identifierText (NonEmpty.toList symbolNames))) True
    ImportQualifiedOnly -> selectedBy QualifiedAliasImport Nothing False
  where
    selectedBy importMode symbolNames includeCapabilityMethods =
      case runtimeExport of
        RuntimeCapabilityMethodExport className _ ->
          includeCapabilityMethods
            && Set.member className selectedClassNames
        RuntimeBindingExport moduleExport ->
          inventoryHasExport moduleExport selectedInventory
      where
        selectedInventory =
          visibleImportInventory
            importMode
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
        AmbientPrelude -> resolvedAmbientName (runtimeExportNamespace runtimeExport) (mkIdentifier exportName)
        _ -> UserName (ResolvedUserName origin (runtimeExportNamespace runtimeExport) (mkIdentifier exportName))

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
        UserName (ResolvedUserName _ namespace _) -> [namespace]
        _ -> [ValueNamespace, ConstructorNamespace, TypeNamespace, CapabilityNamespace]

lookupRendered :: RuntimeExport -> RenderedLookupIndex -> Maybe RuntimeCell
lookupRendered runtimeExport renderedLookupIndex =
  Map.lookup
    (runtimeExportNamespace runtimeExport, runtimeExportName runtimeExport)
    renderedLookupIndex

scopeStatements :: Expr 'Resolved -> [Statement 'Resolved]
scopeStatements expression =
  case expression of
    EBlock _ statements -> statements
    _ -> []
