{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}

-- | Compiler driver that coordinates parsing, prelude injection, module
-- resolution, analysis/type checking, warning promotion, and runtime execution.
module Jazz.Compiler.Driver
  ( CompileResult (..),
    compileErrors,
    compileWarnings,
    ResolvedPrelude (..),
    compileExpr,
    compileSource,
    compileSourceWithPrelude,
    compileSourceWithResolvedPrelude,
    compileModuleGraph,
    compileModuleGraphWithPrelude,
    compileModuleGraphWithResolvedPrelude,
    buildAnalyzedProgram,
    RunExecution (..),
    RunResult,
    runDiagnostics,
    runExecution,
    runRuntimeObservation,
    runOutput,
    runRuntimeValue,
    runExitStatus,
    runCompileErrors,
    runRuntimeErrors,
    runWarnings,
    withAnalyzedAttachment,
    runSource,
    runSourceObserved,
    runSourceWithPrelude,
    runSourceWithPreludeAndHost,
    runSourceWithResolvedPreludeAndHostObserved,
    runModuleGraph,
    runModuleGraphObserved,
    runModuleGraphWithPrelude,
    runModuleGraphWithPreludeAndHost,
    runModuleGraphWithResolvedPrelude,
    runModuleGraphWithResolvedPreludeAndHostObserved,
  )
where

import Control.Exception (evaluate)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Jazz.Compiler.AST
  ( CorePhase (..),
    Expr (..),
  )
import Jazz.Compiler.BundledPrelude
  ( loadBundledPreludeSource,
  )
import Jazz.Compiler.Diagnostics
  ( Diagnostic,
    isErrorDiagnostic,
    isRuntimeDiagnostic,
    isWarningDiagnostic,
  )
import Jazz.Compiler.Force
  ( forceAnalyzedProgramResult,
    forceDiagnostic,
    forceInferenceResult,
  )
import Jazz.Compiler.ModuleCompiler
  ( analyzeProgram,
  )
import Jazz.Compiler.ModuleExports (exportInventory)
import Jazz.Compiler.ModuleGraph (CoreProgram, preludeIdentity)
import Jazz.Compiler.ModuleIdentity (ModulePath, moduleIdentityPath, preludeModulePath)
import Jazz.Compiler.ModuleInterface (compileInputs)
import Jazz.Compiler.ModuleResolver
  ( ModuleResolutionConfig,
    resolvePreludeArtifact,
    resolveProgramWithAmbientExports,
    resolveSourceUnitExprNames,
  )
import Jazz.Compiler.ModuleRuntime
  ( RuntimeProgram (runtimeProgramOutput),
    evaluateAnalyzedProgramWithHostObserved,
  )
import Jazz.Compiler.Parser.Lower (reindexLoweredExpr)
import Jazz.Compiler.Prelude
  ( PreparedPrelude (..),
    ResolvedPrelude (..),
    preparePrelude,
    preparedPreludeExpr,
    resolvedExplicitPrelude,
  )
import Jazz.Compiler.Profiling
  ( CompilerStage (..),
    withCompilerStage,
    withCompilerStageResult,
  )
import Jazz.Compiler.Runtime
  ( RuntimeValue,
    evaluateRuntimeExprWithHostAndSourceUnitStatementsObserved,
    renderRuntimeValue,
  )
import Jazz.Compiler.Runtime.Observation
  ( RuntimeObservationReport,
    RuntimeObservationRequest (..),
    RuntimeObservationResult (..),
    RuntimeOutcome (..),
  )
import Jazz.Compiler.RuntimeHost
  ( RuntimeHost,
    disabledRuntimeHost,
  )
import Jazz.Compiler.SemanticFacts (SemanticFactInvariantFailure)
import Jazz.Compiler.SourceProgram
  ( parseAndLowerStandaloneSource,
    prependLoweredStatements,
    scopeStatements,
  )
import Jazz.Compiler.TypeInference
  ( analyzeSourceUnitExpression,
  )
import Jazz.Compiler.TypeInference.Result (InferenceResult (..))
import Jazz.Compiler.WarningConfig
  ( WarningSettings,
  )

-- | Result of a compile-only invocation. Severity views are derived from the
-- one ordered diagnostic stream below.
newtype CompileResult = CompileResult
  { compileDiagnostics :: [Diagnostic]
  }
  deriving stock (Eq, Show)

compileWarnings :: CompileResult -> [Diagnostic]
compileWarnings = filter isWarningDiagnostic . compileDiagnostics

compileErrors :: CompileResult -> [Diagnostic]
compileErrors = filter isErrorDiagnostic . compileDiagnostics

-- | Whether evaluation ran and how it terminated. Normal completion retains
-- the optional terminal value so valueless completion remains distinct from a
-- run that never started or failed at runtime.
data RunExecution
  = RunNotExecuted
  | RunRuntimeFailed
  | RunExited Integer
  | RunCompleted (Maybe RuntimeValue)
  deriving (Show)

-- | Result of a run invocation. Compile diagnostics precede runtime
-- diagnostics because evaluation only begins after compilation succeeds.
-- Construction stays private so the compatibility projections cannot
-- contradict the execution state.
data RunResult = RunResult
  { runDiagnostics :: [Diagnostic],
    runExecution :: RunExecution,
    runRuntimeObservation :: Maybe RuntimeObservationReport
  }
  deriving (Show)

runOutput :: RunResult -> Maybe Text
runOutput = fmap renderRuntimeValue . runRuntimeValue

runRuntimeValue :: RunResult -> Maybe RuntimeValue
runRuntimeValue result =
  case runExecution result of
    RunCompleted runtimeValue -> runtimeValue
    RunNotExecuted -> Nothing
    RunRuntimeFailed -> Nothing
    RunExited _ -> Nothing

runExitStatus :: RunResult -> Maybe Integer
runExitStatus result =
  case runExecution result of
    RunExited status -> Just status
    RunNotExecuted -> Nothing
    RunRuntimeFailed -> Nothing
    RunCompleted _ -> Nothing

runWarnings :: RunResult -> [Diagnostic]
runWarnings = filter isWarningDiagnostic . runDiagnostics

runCompileErrors :: RunResult -> [Diagnostic]
runCompileErrors =
  filter (\diagnostic -> isErrorDiagnostic diagnostic && not (isRuntimeDiagnostic diagnostic))
    . runDiagnostics

runRuntimeErrors :: RunResult -> [Diagnostic]
runRuntimeErrors =
  filter (\diagnostic -> isErrorDiagnostic diagnostic && isRuntimeDiagnostic diagnostic)
    . runDiagnostics

-- Compiler driver flow for the current implementation slice:
-- analyze -> collect warnings/errors -> apply warning-as-error policy.
compileExpr :: WarningSettings -> Expr 'Lowered -> IO CompileResult
compileExpr = compileExprWithHiddenStatements Set.empty

compileExprWithHiddenStatements ::
  Set Int ->
  WarningSettings ->
  Expr 'Lowered ->
  IO CompileResult
compileExprWithHiddenStatements hiddenStatementIndices settings expr =
  compileExprWithSourceUnitStatements hiddenStatementIndices hiddenStatementIndices preludeModulePath settings expr

compileExprWithSourceUnitStatements ::
  Set Int ->
  Set Int ->
  ModulePath ->
  WarningSettings ->
  Expr 'Lowered ->
  IO CompileResult
compileExprWithSourceUnitStatements hiddenStatementIndices preludeStatementIndices preludePath settings expr = do
  (diagnostics, analyzedAttachment) <- analyzeForDriver hiddenStatementIndices preludeStatementIndices preludePath settings expr
  withAnalyzedAttachment analyzedAttachment $ \_ ->
    pure
      CompileResult
        { compileDiagnostics = diagnostics
        }

compileSource :: WarningSettings -> Text -> IO CompileResult
compileSource settings source = do
  bundledPreludeSource <- loadBundledPreludeSource
  compileSourceWithResolvedPrelude settings (PreludeBundled bundledPreludeSource) source

compileSourceWithPrelude :: WarningSettings -> Maybe Text -> Text -> IO CompileResult
compileSourceWithPrelude settings preludeSource source =
  compileSourceWithResolvedPrelude settings (resolvedExplicitPrelude preludeSource) source

compileSourceWithResolvedPrelude :: WarningSettings -> ResolvedPrelude -> Text -> IO CompileResult
compileSourceWithResolvedPrelude settings resolvedPrelude source =
  case parseAndLowerSource resolvedPrelude source of
    Left parseErrorCode ->
      pure
        CompileResult
          { compileDiagnostics = [parseErrorCode]
          }
    Right loweredProgram ->
      compileExprWithSourceUnitStatements
        (parsedHiddenStatementIndices loweredProgram)
        (parsedPreludeStatementIndices loweredProgram)
        (parsedPreludeModulePath loweredProgram)
        settings
        (parsedExpr loweredProgram)

compileModuleGraph ::
  WarningSettings ->
  ModuleResolutionConfig ->
  [Text] ->
  (FilePath -> IO (Maybe Text)) ->
  IO CompileResult
compileModuleGraph settings resolutionConfig entryModulePath sourceLookup = do
  bundledPreludeSource <- loadBundledPreludeSource
  compileModuleGraphWithResolvedPrelude
    settings
    (PreludeBundled bundledPreludeSource)
    resolutionConfig
    entryModulePath
    sourceLookup

compileModuleGraphWithPrelude ::
  WarningSettings ->
  Maybe Text ->
  ModuleResolutionConfig ->
  [Text] ->
  (FilePath -> IO (Maybe Text)) ->
  IO CompileResult
compileModuleGraphWithPrelude settings preludeSource resolutionConfig entryModulePath sourceLookup =
  compileModuleGraphWithResolvedPrelude settings (resolvedExplicitPrelude preludeSource) resolutionConfig entryModulePath sourceLookup

compileModuleGraphWithResolvedPrelude ::
  WarningSettings ->
  ResolvedPrelude ->
  ModuleResolutionConfig ->
  [Text] ->
  (FilePath -> IO (Maybe Text)) ->
  IO CompileResult
compileModuleGraphWithResolvedPrelude settings resolvedPrelude resolutionConfig entryModulePath sourceLookup = do
  analyzedResult <-
    buildAnalyzedProgram
      settings
      resolvedPrelude
      resolutionConfig
      entryModulePath
      sourceLookup
  case analyzedResult of
    Left diagnostic ->
      pure
        CompileResult
          { compileDiagnostics = [diagnostic]
          }
    Right (_, diagnostics, _) ->
      pure
        CompileResult
          { compileDiagnostics = diagnostics
          }

runExprWithSourceUnitStatementsAndHostObserved ::
  RuntimeObservationRequest ->
  RuntimeHost IO ->
  Set Int ->
  Set Int ->
  ModulePath ->
  WarningSettings ->
  Expr 'Lowered ->
  IO RunResult
runExprWithSourceUnitStatementsAndHostObserved observationRequest host hiddenStatementIndices preludeStatementIndices preludePath settings expr = do
  (compilePhaseDiagnostics, analyzedAttachment) <-
    analyzeForDriver hiddenStatementIndices preludeStatementIndices preludePath settings expr
  if any isErrorDiagnostic compilePhaseDiagnostics
    then
      pure
        RunResult
          { runDiagnostics = compilePhaseDiagnostics,
            runExecution = RunNotExecuted,
            runRuntimeObservation = Nothing
          }
    else withAnalyzedAttachment analyzedAttachment $ \maybeAnalyzedExpr ->
      case maybeAnalyzedExpr of
        Just analyzedExpr -> do
          runtimeResult <-
            evaluateRuntimeExprWithHostAndSourceUnitStatementsObserved
              observationRequest
              host
              preludeStatementIndices
              preludePath
              analyzedExpr
          pure (runtimeObservationRunResult id compilePhaseDiagnostics runtimeResult)
        Nothing ->
          pure
            RunResult
              { runDiagnostics = compilePhaseDiagnostics,
                runExecution = RunNotExecuted,
                runRuntimeObservation = Nothing
              }

runSource :: WarningSettings -> Text -> IO RunResult
runSource = runSourceObserved RuntimeObservationDisabled

runSourceObserved :: RuntimeObservationRequest -> WarningSettings -> Text -> IO RunResult
runSourceObserved observationRequest =
  runSourceWithHostObserved observationRequest disabledRuntimeHost

runSourceWithHostObserved :: RuntimeObservationRequest -> RuntimeHost IO -> WarningSettings -> Text -> IO RunResult
runSourceWithHostObserved observationRequest host settings source = do
  bundledPreludeSource <- loadBundledPreludeSource
  runSourceWithResolvedPreludeAndHostObserved
    observationRequest
    host
    settings
    (PreludeBundled bundledPreludeSource)
    source

runSourceWithPrelude :: WarningSettings -> Maybe Text -> Text -> IO RunResult
runSourceWithPrelude = runSourceWithPreludeAndHost disabledRuntimeHost

runSourceWithPreludeAndHost :: RuntimeHost IO -> WarningSettings -> Maybe Text -> Text -> IO RunResult
runSourceWithPreludeAndHost host settings preludeSource source =
  runSourceWithResolvedPreludeAndHost host settings (resolvedExplicitPrelude preludeSource) source

runSourceWithResolvedPreludeAndHost :: RuntimeHost IO -> WarningSettings -> ResolvedPrelude -> Text -> IO RunResult
runSourceWithResolvedPreludeAndHost =
  runSourceWithResolvedPreludeAndHostObserved RuntimeObservationDisabled

runSourceWithResolvedPreludeAndHostObserved :: RuntimeObservationRequest -> RuntimeHost IO -> WarningSettings -> ResolvedPrelude -> Text -> IO RunResult
runSourceWithResolvedPreludeAndHostObserved observationRequest host settings resolvedPrelude source =
  case parseAndLowerSource resolvedPrelude source of
    Left parseErrorCode ->
      pure
        RunResult
          { runDiagnostics = [parseErrorCode],
            runExecution = RunNotExecuted,
            runRuntimeObservation = Nothing
          }
    Right loweredProgram ->
      runExprWithSourceUnitStatementsAndHostObserved
        observationRequest
        host
        (parsedHiddenStatementIndices loweredProgram)
        (parsedPreludeStatementIndices loweredProgram)
        (parsedPreludeModulePath loweredProgram)
        settings
        (parsedExpr loweredProgram)

runModuleGraph ::
  WarningSettings ->
  ModuleResolutionConfig ->
  [Text] ->
  (FilePath -> IO (Maybe Text)) ->
  IO RunResult
runModuleGraph = runModuleGraphObserved RuntimeObservationDisabled

runModuleGraphObserved ::
  RuntimeObservationRequest ->
  WarningSettings ->
  ModuleResolutionConfig ->
  [Text] ->
  (FilePath -> IO (Maybe Text)) ->
  IO RunResult
runModuleGraphObserved observationRequest =
  runModuleGraphWithHostObserved observationRequest disabledRuntimeHost

runModuleGraphWithHostObserved ::
  RuntimeObservationRequest ->
  RuntimeHost IO ->
  WarningSettings ->
  ModuleResolutionConfig ->
  [Text] ->
  (FilePath -> IO (Maybe Text)) ->
  IO RunResult
runModuleGraphWithHostObserved observationRequest host settings resolutionConfig entryModulePath sourceLookup = do
  bundledPreludeSource <- loadBundledPreludeSource
  runModuleGraphWithResolvedPreludeAndHostObserved
    observationRequest
    host
    settings
    (PreludeBundled bundledPreludeSource)
    resolutionConfig
    entryModulePath
    sourceLookup

runModuleGraphWithPrelude ::
  WarningSettings ->
  Maybe Text ->
  ModuleResolutionConfig ->
  [Text] ->
  (FilePath -> IO (Maybe Text)) ->
  IO RunResult
runModuleGraphWithPrelude settings preludeSource resolutionConfig entryModulePath sourceLookup =
  runModuleGraphWithPreludeAndHost
    disabledRuntimeHost
    settings
    preludeSource
    resolutionConfig
    entryModulePath
    sourceLookup

runModuleGraphWithPreludeAndHost ::
  RuntimeHost IO ->
  WarningSettings ->
  Maybe Text ->
  ModuleResolutionConfig ->
  [Text] ->
  (FilePath -> IO (Maybe Text)) ->
  IO RunResult
runModuleGraphWithPreludeAndHost host settings preludeSource resolutionConfig entryModulePath sourceLookup =
  runModuleGraphWithResolvedPreludeAndHost
    host
    settings
    (resolvedExplicitPrelude preludeSource)
    resolutionConfig
    entryModulePath
    sourceLookup

runModuleGraphWithResolvedPrelude ::
  WarningSettings ->
  ResolvedPrelude ->
  ModuleResolutionConfig ->
  [Text] ->
  (FilePath -> IO (Maybe Text)) ->
  IO RunResult
runModuleGraphWithResolvedPrelude = runModuleGraphWithResolvedPreludeAndHost disabledRuntimeHost

runModuleGraphWithResolvedPreludeAndHost ::
  RuntimeHost IO ->
  WarningSettings ->
  ResolvedPrelude ->
  ModuleResolutionConfig ->
  [Text] ->
  (FilePath -> IO (Maybe Text)) ->
  IO RunResult
runModuleGraphWithResolvedPreludeAndHost host settings resolvedPrelude resolutionConfig entryModulePath sourceLookup = do
  runModuleGraphWithResolvedPreludeAndHostObserved
    RuntimeObservationDisabled
    host
    settings
    resolvedPrelude
    resolutionConfig
    entryModulePath
    sourceLookup

runModuleGraphWithResolvedPreludeAndHostObserved ::
  RuntimeObservationRequest ->
  RuntimeHost IO ->
  WarningSettings ->
  ResolvedPrelude ->
  ModuleResolutionConfig ->
  [Text] ->
  (FilePath -> IO (Maybe Text)) ->
  IO RunResult
runModuleGraphWithResolvedPreludeAndHostObserved observationRequest host settings resolvedPrelude resolutionConfig entryModulePath sourceLookup = do
  analyzedResult <-
    buildAnalyzedProgram
      settings
      resolvedPrelude
      resolutionConfig
      entryModulePath
      sourceLookup
  case analyzedResult of
    Left diagnostic ->
      pure
        RunResult
          { runDiagnostics = [diagnostic],
            runExecution = RunNotExecuted,
            runRuntimeObservation = Nothing
          }
    Right (_, moduleDiagnostics, maybeAnalyzedProgram) ->
      case maybeAnalyzedProgram of
        Nothing ->
          pure
            RunResult
              { runDiagnostics = moduleDiagnostics,
                runExecution = RunNotExecuted,
                runRuntimeObservation = Nothing
              }
        Just analyzedProgram -> do
          runtimeResult <- evaluateAnalyzedProgramWithHostObserved observationRequest host analyzedProgram
          pure (runtimeObservationRunResult runtimeProgramOutput moduleDiagnostics runtimeResult)

runtimeObservationRunResult ::
  (value -> Maybe RuntimeValue) ->
  [Diagnostic] ->
  RuntimeObservationResult value ->
  RunResult
runtimeObservationRunResult runtimeValueProjection compilePhaseDiagnostics runtimeResult =
  case runtimeObservationOutcome runtimeResult of
    RuntimeOutcomeFailed runtimeError ->
      RunResult
        { runDiagnostics = compilePhaseDiagnostics <> [runtimeError],
          runExecution = RunRuntimeFailed,
          runRuntimeObservation = runtimeObservationReport runtimeResult
        }
    RuntimeOutcomeExited status ->
      RunResult
        { runDiagnostics = compilePhaseDiagnostics,
          runExecution = RunExited status,
          runRuntimeObservation = runtimeObservationReport runtimeResult
        }
    RuntimeOutcomeCompleted value ->
      RunResult
        { runDiagnostics = compilePhaseDiagnostics,
          runExecution = RunCompleted (runtimeValueProjection value),
          runRuntimeObservation = runtimeObservationReport runtimeResult
        }

buildAnalyzedProgram ::
  WarningSettings ->
  ResolvedPrelude ->
  ModuleResolutionConfig ->
  [Text] ->
  (FilePath -> IO (Maybe Text)) ->
  IO (Either Diagnostic (CoreProgram 'Resolved, [Diagnostic], Maybe (CoreProgram 'Analyzed)))
buildAnalyzedProgram settings resolvedPrelude resolutionConfig entryModulePath sourceLookup =
  case preparePrelude resolvedPrelude of
    Left preludeError -> pure (Left preludeError)
    Right preparedPrelude -> do
      case resolvePreludeArtifact
        (preparedPreludeVisibleExports preparedPrelude)
        (preparedPreludeArtifact preparedPrelude) of
        Left resolutionError -> pure (Left resolutionError)
        Right resolvedPreludeArtifact -> do
          resolvedResult <-
            withCompilerStage ModuleDiscoveryStage $
              resolveProgramWithAmbientExports
                resolutionConfig
                resolvedPreludeArtifact
                (preparedPreludeVisibleExports preparedPrelude)
                profiledSourceLookup
                entryModulePath
          case resolvedResult of
            Left resolutionError -> pure (Left resolutionError)
            Right resolvedProgram ->
              withCompilerStageResult RuntimePreparationStage forceAnalyzedBuildResult $ do
                (diagnostics, maybeAnalyzedProgram) <-
                  analyzeProgram
                    (compileInputs settings (preparedPreludeHiddenStatementIndices preparedPrelude))
                    resolvedProgram
                pure (Right (resolvedProgram, diagnostics, maybeAnalyzedProgram))
  where
    profiledSourceLookup sourcePath =
      withCompilerStageResult
        SourceLoadingStage
        (\maybeSource -> evaluate (maybe 0 Text.length maybeSource) >> pure ())
        (sourceLookup sourcePath)

    forceAnalyzedBuildResult result =
      evaluate $
        case result of
          Left diagnostic -> forceDiagnostic diagnostic
          Right (_, diagnostics, maybeProgram) ->
            forceAnalyzedProgramResult (diagnostics, maybeProgram)

-- | Run inference/canonicalization and retain the canonical diagnostic order
-- for downstream compile/run results.
analyzeForDriver :: Set Int -> Set Int -> ModulePath -> WarningSettings -> Expr 'Lowered -> IO ([Diagnostic], Either (NonEmpty.NonEmpty SemanticFactInvariantFailure) (Maybe (Expr 'Analyzed)))
analyzeForDriver hiddenStatementIndices preludeStatementIndices preludePath settings expr = do
  case resolveSourceUnitExprNames preludePath preludeStatementIndices (exportInventory []) (reindexLoweredExpr expr) of
    Left diagnostics ->
      pure (NonEmpty.toList diagnostics, Right Nothing)
    Right resolvedExpr -> do
      (inference, analyzedAttachment) <-
        withCompilerStageResult
          TypeInferenceStage
          (evaluate . forceInferenceResult . fst)
          ( analyzeSourceUnitExpression
              preludePath
              hiddenStatementIndices
              preludeStatementIndices
              settings
              resolvedExpr
          )
      let diagnostics = inferredDiagnostics inference
      case analyzedAttachment of
        Left failures ->
          pure (diagnostics, Left failures)
        Right maybeAnalyzed ->
          pure (diagnostics, Right maybeAnalyzed)

standaloneAttachmentFailure :: NonEmpty.NonEmpty SemanticFactInvariantFailure -> String
standaloneAttachmentFailure failures =
  "standalone analyzed facts violated inference invariants: " <> show failures

-- | An analyzed-plan invariant failure never enters runtime evaluation, even
-- when the resolved interpreter would independently produce a diagnostic.
withAnalyzedAttachment ::
  Either (NonEmpty.NonEmpty SemanticFactInvariantFailure) plan ->
  (plan -> IO result) ->
  IO result
withAnalyzedAttachment attachment continue =
  case attachment of
    Left failures -> fail (standaloneAttachmentFailure failures)
    Right plan -> continue plan

-- | Parse the incoming source and splice in prelude statements when required,
-- tracking which synthetic statements should stay hidden from user diagnostics.
parseAndLowerSource :: ResolvedPrelude -> Text -> Either Diagnostic ParsedProgram
parseAndLowerSource resolvedPrelude source = do
  loweredSource <- parseAndLowerStandaloneSource source
  preparedPrelude <- preparePrelude resolvedPrelude
  pure (mergePreparedPrelude preparedPrelude loweredSource)

mergePreparedPrelude :: PreparedPrelude -> Expr 'Lowered -> ParsedProgram
mergePreparedPrelude preparedPrelude loweredSource =
  case preparedPreludeExpr preparedPrelude of
    Nothing ->
      ParsedProgram
        { parsedExpr = loweredSource,
          parsedHiddenStatementIndices = Set.empty,
          parsedPreludeStatementIndices = Set.empty,
          parsedPreludeModulePath = preparedPreludePath preparedPrelude
        }
    Just loweredPrelude ->
      let preludeStatements = scopeStatements loweredPrelude
          combinedExpr = prependLoweredStatements preludeStatements loweredSource
          preludeStatementIndices = Set.fromList [0 .. length preludeStatements - 1]
       in ParsedProgram
            { parsedExpr = combinedExpr,
              parsedHiddenStatementIndices = preparedPreludeHiddenStatementIndices preparedPrelude,
              parsedPreludeStatementIndices = preludeStatementIndices,
              parsedPreludeModulePath = preparedPreludePath preparedPrelude
            }
  where
    preparedPreludePath =
      moduleIdentityPath . preludeIdentity . preparedPreludeArtifact

-- | Lowered program paired with statement indices that came from synthetic
-- bundled prelude source.
data ParsedProgram = ParsedProgram
  { parsedExpr :: Expr 'Lowered,
    parsedHiddenStatementIndices :: Set Int,
    parsedPreludeStatementIndices :: Set Int,
    parsedPreludeModulePath :: ModulePath
  }
