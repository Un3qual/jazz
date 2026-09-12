{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}

-- | Compiler driver that coordinates parsing, prelude artifacts, module
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
    buildAnalyzedSourceProgram,
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
  )
import Jazz.Compiler.ModuleCompiler
  ( analyzeProgram,
  )
import Jazz.Compiler.ModuleGraph (CoreProgram)
import Jazz.Compiler.ModuleInterface (emptyCompileInputs)
import Jazz.Compiler.ModuleResolver
  ( ModuleResolutionConfig,
    resolvePreludeArtifact,
    resolveProgramWithAmbientExports,
    resolveStandaloneProgram,
  )
import Jazz.Compiler.ModuleRuntime
  ( RuntimeProgram (runtimeProgramOutput),
    evaluateAnalyzedProgramWithHostObserved,
  )
import Jazz.Compiler.Prelude
  ( PreparedPrelude (..),
    ResolvedPrelude (..),
    preparePrelude,
    resolvedExplicitPrelude,
  )
import Jazz.Compiler.Profiling
  ( CompilerStage (..),
    withCompilerStage,
    withCompilerStageResult,
  )
import Jazz.Compiler.Runtime
  ( RuntimeValue,
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
import Jazz.Compiler.SourceProgram
  ( parseAndLowerStandaloneSource,
  )
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
compileExpr settings expression = do
  result <- buildAnalyzedSourceProgram settings PreludeAbsent expression
  pure (CompileResult (either (: []) (\(_, diagnostics, _) -> diagnostics) result))

compileSource :: WarningSettings -> Text -> IO CompileResult
compileSource settings source = do
  bundledPreludeSource <- loadBundledPreludeSource
  compileSourceWithResolvedPrelude settings (PreludeBundled bundledPreludeSource) source

compileSourceWithPrelude :: WarningSettings -> Maybe Text -> Text -> IO CompileResult
compileSourceWithPrelude settings preludeSource source =
  compileSourceWithResolvedPrelude settings (resolvedExplicitPrelude preludeSource) source

compileSourceWithResolvedPrelude :: WarningSettings -> ResolvedPrelude -> Text -> IO CompileResult
compileSourceWithResolvedPrelude settings resolvedPrelude source =
  case parseAndLowerStandaloneSource source of
    Left diagnostic -> pure (CompileResult [diagnostic])
    Right expression -> do
      result <- buildAnalyzedSourceProgram settings resolvedPrelude expression
      pure (CompileResult (either (: []) (\(_, diagnostics, _) -> diagnostics) result))

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
  case parseAndLowerStandaloneSource source of
    Left diagnostic -> executeAnalyzedBuild observationRequest host (Left diagnostic)
    Right expression ->
      buildAnalyzedSourceProgram settings resolvedPrelude expression
        >>= executeAnalyzedBuild observationRequest host

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
  executeAnalyzedBuild observationRequest host analyzedResult

executeAnalyzedBuild ::
  RuntimeObservationRequest ->
  RuntimeHost IO ->
  Either Diagnostic (CoreProgram 'Resolved, [Diagnostic], Maybe (CoreProgram 'Analyzed)) ->
  IO RunResult
executeAnalyzedBuild observationRequest host analyzedResult =
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

-- | Analyze an in-memory source unit through the program coordinator.
buildAnalyzedSourceProgram ::
  WarningSettings ->
  ResolvedPrelude ->
  Expr 'Lowered ->
  IO (Either Diagnostic (CoreProgram 'Resolved, [Diagnostic], Maybe (CoreProgram 'Analyzed)))
buildAnalyzedSourceProgram settings resolvedPrelude expression =
  case preparePrelude resolvedPrelude of
    Left diagnostic -> pure (Left diagnostic)
    Right preparedPrelude ->
      case do
        prelude <- resolvePreludeArtifact (preparedPreludeVisibleExports preparedPrelude) (preparedPreludeArtifact preparedPrelude)
        resolveStandaloneProgram prelude (preparedPreludeVisibleExports preparedPrelude) expression of
        Left diagnostic -> pure (Left diagnostic)
        Right program -> do
          (diagnostics, analyzed) <- analyzeProgram (emptyCompileInputs settings) program
          pure (Right (program, diagnostics, analyzed))

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
                    (emptyCompileInputs settings)
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
