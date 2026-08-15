{-# LANGUAGE OverloadedStrings #-}

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
    buildCompiledProgram,
    RunResult (..),
    runCompileErrors,
    runRuntimeErrors,
    runWarnings,
    runExpr,
    runExprObserved,
    runExprWithHost,
    runExprWithHostObserved,
    runSource,
    runSourceObserved,
    runSourceWithHost,
    runSourceWithHostObserved,
    runSourceWithPrelude,
    runSourceWithPreludeAndHost,
    runSourceWithResolvedPrelude,
    runSourceWithResolvedPreludeAndHost,
    runSourceWithResolvedPreludeAndHostObserved,
    runModuleGraph,
    runModuleGraphObserved,
    runModuleGraphWithHost,
    runModuleGraphWithHostObserved,
    runModuleGraphWithPrelude,
    runModuleGraphWithPreludeAndHost,
    runModuleGraphWithResolvedPrelude,
    runModuleGraphWithResolvedPreludeAndHost,
    runModuleGraphWithResolvedPreludeAndHostObserved
  ) where

import Control.Exception (evaluate)
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as Text
import Jazz.Compiler.AST
  ( SignatureType (..),
    Expr (..)
  )
import Jazz.Compiler.BundledPrelude
  ( loadBundledPreludeSource
  )
import Jazz.Compiler.BuiltinCatalog
  ( BuiltinResolutionMode (..)
  )
import Jazz.Compiler.Diagnostics
  ( Diagnostic,
    isErrorDiagnostic,
    isRuntimeDiagnostic,
    isWarningDiagnostic
  )
import Jazz.Compiler.Force
  ( forceCompiledProgramResult,
    forceInferenceResult
  )
import Jazz.Compiler.ModuleCompiler
  ( compilePreparedPrelude,
    compileResolvedProgram
  )
import Jazz.Compiler.ModuleInterface
  ( CompiledProgram (..),
    compileInputs,
    compiledProgramDiagnostics
  )
import Jazz.Compiler.ModuleResolver
  ( ModuleResolutionConfig,
    resolveProgramWithAmbientExports
  )
import Jazz.Compiler.ModuleRuntime
  ( RuntimeProgram (runtimeProgramOutput),
    evaluateCompiledProgramWithHostObserved
  )
import Jazz.Compiler.Prelude
  ( PreparedPrelude (..),
    ResolvedPrelude (..),
    preparePrelude,
    resolvedExplicitPrelude
  )
import Jazz.Compiler.Profiling
  ( CompilerStage (..),
    withCompilerStage,
    withCompilerStageResult
  )
import Jazz.Compiler.Runtime
  ( RuntimeValue,
    evaluateRuntimeExprWithHostAndBuiltinsAndBindingHintsAndSourceUnitStatementsObserved,
    renderRuntimeValue
  )
import Jazz.Compiler.Runtime.Observation
  ( RuntimeObservationReport,
    RuntimeObservationRequest (..),
    RuntimeObservationResult (..),
    RuntimeOutcome (..)
  )
import Jazz.Compiler.RuntimeHost
  ( RuntimeHost,
    disabledRuntimeHost
  )
import Jazz.Compiler.RuntimeHints
  ( BindingRuntimeHintKey
  )
import Jazz.Compiler.SourceProgram
  ( parseAndLowerStandaloneSource,
    scopeStatements
  )
import Jazz.Compiler.TypeInference
  ( inferExpressionWithBuiltinsAndSourceUnitStatements
  )
import Jazz.Compiler.TypeInference.Result (InferenceResult (..))
import Jazz.Compiler.WarningConfig
  ( WarningSettings
  )

-- | Result of a compile-only invocation. Severity views are derived from the
-- one ordered diagnostic stream below.
data CompileResult = CompileResult
  { compileDiagnostics :: [Diagnostic]
  }
  deriving (Eq, Show)

compileWarnings :: CompileResult -> [Diagnostic]
compileWarnings = filter isWarningDiagnostic . compileDiagnostics

compileErrors :: CompileResult -> [Diagnostic]
compileErrors = filter isErrorDiagnostic . compileDiagnostics

-- | Result of a run invocation. Compile diagnostics precede runtime
-- diagnostics because evaluation only begins after compilation succeeds.
data RunResult = RunResult
  { runDiagnostics :: [Diagnostic],
    runOutput :: Maybe Text,
    runRuntimeValue :: Maybe RuntimeValue,
    runExitStatus :: Maybe Integer,
    runRuntimeObservation :: Maybe RuntimeObservationReport
  }
  deriving (Eq, Show)

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
compileExpr :: WarningSettings -> Expr -> IO CompileResult
compileExpr = compileExprWithBuiltins ResolveKernelOnly

compileExprWithBuiltins :: BuiltinResolutionMode -> WarningSettings -> Expr -> IO CompileResult
compileExprWithBuiltins = compileExprWithBuiltinsAndHiddenStatements Set.empty

compileExprWithBuiltinsAndHiddenStatements ::
  Set Int ->
  BuiltinResolutionMode ->
  WarningSettings ->
  Expr ->
  IO CompileResult
compileExprWithBuiltinsAndHiddenStatements hiddenStatementIndices builtinMode settings expr =
  compileExprWithBuiltinsAndSourceUnitStatements hiddenStatementIndices hiddenStatementIndices builtinMode settings expr

compileExprWithBuiltinsAndSourceUnitStatements ::
  Set Int ->
  Set Int ->
  BuiltinResolutionMode ->
  WarningSettings ->
  Expr ->
  IO CompileResult
compileExprWithBuiltinsAndSourceUnitStatements hiddenStatementIndices preludeStatementIndices builtinMode settings expr = do
  (diagnostics, _, _) <- analyzeForDriver hiddenStatementIndices preludeStatementIndices builtinMode settings expr
  pure
    CompileResult
      { compileDiagnostics = diagnostics }

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
          { compileDiagnostics = [parseErrorCode] }
    Right loweredProgram ->
      compileExprWithBuiltinsAndSourceUnitStatements
        (parsedHiddenStatementIndices loweredProgram)
        (parsedPreludeStatementIndices loweredProgram)
        (parsedBuiltinMode loweredProgram)
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
  compiledResult <-
    buildCompiledProgram
      settings
      resolvedPrelude
      resolutionConfig
      entryModulePath
      sourceLookup
  case compiledResult of
    Left diagnostic ->
      pure
        CompileResult
          { compileDiagnostics = [diagnostic] }
    Right compiledProgram ->
      pure
        CompileResult
          { compileDiagnostics = compiledProgramDiagnostics compiledProgram }

runExpr :: WarningSettings -> Expr -> IO RunResult
runExpr = runExprObserved RuntimeObservationDisabled

runExprObserved :: RuntimeObservationRequest -> WarningSettings -> Expr -> IO RunResult
runExprObserved observationRequest =
  runExprWithHostObserved observationRequest disabledRuntimeHost

runExprWithHost :: RuntimeHost IO -> WarningSettings -> Expr -> IO RunResult
runExprWithHost = runExprWithHostObserved RuntimeObservationDisabled

runExprWithHostObserved :: RuntimeObservationRequest -> RuntimeHost IO -> WarningSettings -> Expr -> IO RunResult
runExprWithHostObserved observationRequest host =
  runExprWithBuiltinsAndHostObserved observationRequest host ResolveKernelOnly

runExprWithBuiltinsAndHostObserved :: RuntimeObservationRequest -> RuntimeHost IO -> BuiltinResolutionMode -> WarningSettings -> Expr -> IO RunResult
runExprWithBuiltinsAndHostObserved observationRequest host =
  runExprWithBuiltinsAndHiddenStatementsAndHostObserved observationRequest host Set.empty

runExprWithBuiltinsAndHiddenStatementsAndHostObserved ::
  RuntimeObservationRequest ->
  RuntimeHost IO ->
  Set Int ->
  BuiltinResolutionMode ->
  WarningSettings ->
  Expr ->
  IO RunResult
runExprWithBuiltinsAndHiddenStatementsAndHostObserved observationRequest host hiddenStatementIndices =
  runExprWithBuiltinsAndSourceUnitStatementsAndHostObserved
    observationRequest
    host
    hiddenStatementIndices
    hiddenStatementIndices

runExprWithBuiltinsAndSourceUnitStatementsAndHostObserved ::
  RuntimeObservationRequest ->
  RuntimeHost IO ->
  Set Int ->
  Set Int ->
  BuiltinResolutionMode ->
  WarningSettings ->
  Expr ->
  IO RunResult
runExprWithBuiltinsAndSourceUnitStatementsAndHostObserved observationRequest host hiddenStatementIndices preludeStatementIndices builtinMode settings expr = do
  (compilePhaseDiagnostics, canonicalExpr, runtimeTypeHints) <-
    analyzeForDriver hiddenStatementIndices preludeStatementIndices builtinMode settings expr
  if any isErrorDiagnostic compilePhaseDiagnostics
    then
      pure
        RunResult
          { runDiagnostics = compilePhaseDiagnostics,
            runOutput = Nothing,
            runRuntimeValue = Nothing,
            runExitStatus = Nothing,
            runRuntimeObservation = Nothing
          }
    else do
      runtimeResult <-
        evaluateRuntimeExprWithHostAndBuiltinsAndBindingHintsAndSourceUnitStatementsObserved
          observationRequest
          host
          preludeStatementIndices
          builtinMode
          runtimeTypeHints
          canonicalExpr
      case runtimeObservationOutcome runtimeResult of
        RuntimeOutcomeFailed runtimeError ->
          pure
            RunResult
              { runDiagnostics = compilePhaseDiagnostics <> [runtimeError],
                runOutput = Nothing,
                runRuntimeValue = Nothing,
                runExitStatus = Nothing,
                runRuntimeObservation = runtimeObservationReport runtimeResult
              }
        RuntimeOutcomeExited status ->
          pure
            RunResult
              { runDiagnostics = compilePhaseDiagnostics,
                runOutput = Nothing,
                runRuntimeValue = Nothing,
                runExitStatus = Just status,
                runRuntimeObservation = runtimeObservationReport runtimeResult
              }
        RuntimeOutcomeCompleted runtimeValue ->
          pure
            RunResult
              { runDiagnostics = compilePhaseDiagnostics,
                runOutput = fmap renderRuntimeValue runtimeValue,
                runRuntimeValue = runtimeValue,
                runExitStatus = Nothing,
                runRuntimeObservation = runtimeObservationReport runtimeResult
              }

runSource :: WarningSettings -> Text -> IO RunResult
runSource = runSourceObserved RuntimeObservationDisabled

runSourceObserved :: RuntimeObservationRequest -> WarningSettings -> Text -> IO RunResult
runSourceObserved observationRequest =
  runSourceWithHostObserved observationRequest disabledRuntimeHost

runSourceWithHost :: RuntimeHost IO -> WarningSettings -> Text -> IO RunResult
runSourceWithHost = runSourceWithHostObserved RuntimeObservationDisabled

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

runSourceWithResolvedPrelude :: WarningSettings -> ResolvedPrelude -> Text -> IO RunResult
runSourceWithResolvedPrelude = runSourceWithResolvedPreludeAndHost disabledRuntimeHost

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
            runOutput = Nothing,
            runRuntimeValue = Nothing,
            runExitStatus = Nothing,
            runRuntimeObservation = Nothing
          }
    Right loweredProgram ->
      runExprWithBuiltinsAndSourceUnitStatementsAndHostObserved
        observationRequest
        host
        (parsedHiddenStatementIndices loweredProgram)
        (parsedPreludeStatementIndices loweredProgram)
        (parsedBuiltinMode loweredProgram)
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

runModuleGraphWithHost ::
  RuntimeHost IO ->
  WarningSettings ->
  ModuleResolutionConfig ->
  [Text] ->
  (FilePath -> IO (Maybe Text)) ->
  IO RunResult
runModuleGraphWithHost = runModuleGraphWithHostObserved RuntimeObservationDisabled

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
  compiledResult <-
    buildCompiledProgram
      settings
      resolvedPrelude
      resolutionConfig
      entryModulePath
      sourceLookup
  case compiledResult of
    Left diagnostic ->
      pure
        RunResult
          { runDiagnostics = [diagnostic],
            runOutput = Nothing,
            runRuntimeValue = Nothing,
            runExitStatus = Nothing,
            runRuntimeObservation = Nothing
          }
    Right compiledProgram ->
      let moduleDiagnostics = compiledProgramDiagnostics compiledProgram
       in if any isErrorDiagnostic moduleDiagnostics
            then
              pure
                RunResult
                  { runDiagnostics = moduleDiagnostics,
                    runOutput = Nothing,
                    runRuntimeValue = Nothing,
                    runExitStatus = Nothing,
                    runRuntimeObservation = Nothing
                  }
            else do
              runtimeResult <- evaluateCompiledProgramWithHostObserved observationRequest host compiledProgram
              case runtimeObservationOutcome runtimeResult of
                RuntimeOutcomeFailed runtimeError ->
                  pure
                    RunResult
                      { runDiagnostics = moduleDiagnostics <> [runtimeError],
                        runOutput = Nothing,
                        runRuntimeValue = Nothing,
                        runExitStatus = Nothing,
                        runRuntimeObservation = runtimeObservationReport runtimeResult
                      }
                RuntimeOutcomeExited status ->
                  pure
                    RunResult
                      { runDiagnostics = moduleDiagnostics,
                        runOutput = Nothing,
                        runRuntimeValue = Nothing,
                        runExitStatus = Just status,
                        runRuntimeObservation = runtimeObservationReport runtimeResult
                      }
                RuntimeOutcomeCompleted runtimeProgram ->
                  pure
                    RunResult
                      { runDiagnostics = moduleDiagnostics,
                        runOutput = renderRuntimeValue <$> runtimeProgramOutput runtimeProgram,
                        runRuntimeValue = runtimeProgramOutput runtimeProgram,
                        runExitStatus = Nothing,
                        runRuntimeObservation = runtimeObservationReport runtimeResult
                      }

buildCompiledProgram ::
  WarningSettings ->
  ResolvedPrelude ->
  ModuleResolutionConfig ->
  [Text] ->
  (FilePath -> IO (Maybe Text)) ->
  IO (Either Diagnostic CompiledProgram)
buildCompiledProgram settings resolvedPrelude resolutionConfig entryModulePath sourceLookup =
  case preparePrelude resolvedPrelude of
    Left preludeError -> pure (Left preludeError)
    Right preparedPrelude -> do
      resolvedResult <-
        withCompilerStage ModuleDiscoveryStage $
          resolveProgramWithAmbientExports
            resolutionConfig
            (preparedPreludeBuiltinMode preparedPrelude)
            (preparedPreludeVisibleExports preparedPrelude)
            profiledSourceLookup
            entryModulePath
      case resolvedResult of
        Left resolutionError -> pure (Left resolutionError)
        Right resolvedProgram ->
          withCompilerStageResult RuntimePreparationStage (evaluate . forceCompiledProgramResult) $ do
            compiledPrelude <- compilePreparedPrelude settings preparedPrelude
            Right <$> compileResolvedProgram (compileInputs settings compiledPrelude) resolvedProgram
  where
    profiledSourceLookup sourcePath =
      withCompilerStageResult
        SourceLoadingStage
        (\maybeSource -> evaluate (maybe 0 Text.length maybeSource) >> pure ())
        (sourceLookup sourcePath)

-- | Run inference/canonicalization and retain the canonical diagnostic order
-- for downstream compile/run results.
analyzeForDriver :: Set Int -> Set Int -> BuiltinResolutionMode -> WarningSettings -> Expr -> IO ([Diagnostic], Expr, Map BindingRuntimeHintKey SignatureType)
analyzeForDriver hiddenStatementIndices preludeStatementIndices builtinMode settings expr = do
  inference <-
    withCompilerStageResult
      TypeInferenceStage
      (evaluate . forceInferenceResult)
      ( inferExpressionWithBuiltinsAndSourceUnitStatements
          builtinMode
          hiddenStatementIndices
          preludeStatementIndices
          settings
          expr
      )
  let diagnostics = inferredDiagnostics inference
  pure (diagnostics, inferredExpr inference, inferredRuntimeTypeHints inference)

-- | Parse the incoming source and splice in prelude statements when required,
-- tracking which synthetic statements should stay hidden from user diagnostics.
parseAndLowerSource :: ResolvedPrelude -> Text -> Either Diagnostic ParsedProgram
parseAndLowerSource resolvedPrelude source = do
  loweredSource <- parseAndLowerStandaloneSource source
  preparedPrelude <- preparePrelude resolvedPrelude
  pure (mergePreparedPrelude preparedPrelude loweredSource)

mergePreparedPrelude :: PreparedPrelude -> Expr -> ParsedProgram
mergePreparedPrelude preparedPrelude loweredSource =
  case preparedPreludeExpr preparedPrelude of
    Nothing ->
      ParsedProgram
        { parsedExpr = loweredSource,
          parsedHiddenStatementIndices = Set.empty,
          parsedPreludeStatementIndices = Set.empty,
          parsedBuiltinMode = preparedPreludeBuiltinMode preparedPrelude
        }
    Just loweredPrelude ->
      let preludeStatements = scopeStatements loweredPrelude
          combinedExpr =
            EBlock (preludeStatements ++ scopeStatements loweredSource)
          preludeStatementIndices = Set.fromList [0 .. length preludeStatements - 1]
       in ParsedProgram
            { parsedExpr = combinedExpr,
              parsedHiddenStatementIndices = preparedPreludeHiddenStatementIndices preparedPrelude,
              parsedPreludeStatementIndices = preludeStatementIndices,
              parsedBuiltinMode = preparedPreludeBuiltinMode preparedPrelude
            }

-- | Lowered program paired with statement indices that came from synthetic
-- bundled prelude source.
data ParsedProgram = ParsedProgram
  { parsedExpr :: Expr,
    parsedHiddenStatementIndices :: Set Int,
    parsedPreludeStatementIndices :: Set Int,
    parsedBuiltinMode :: BuiltinResolutionMode
  }
