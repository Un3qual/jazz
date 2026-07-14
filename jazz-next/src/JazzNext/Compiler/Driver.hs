{-# LANGUAGE OverloadedStrings #-}

-- | Compiler driver that coordinates parsing, prelude injection, module
-- resolution, analysis/type checking, warning promotion, and runtime execution.
module JazzNext.Compiler.Driver
  ( CompileResult (..),
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

import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Text (Text)
import JazzNext.Compiler.AST
  ( SignatureType (..),
    Expr (..)
  )
import JazzNext.Compiler.Diagnostics
  ( Diagnostic,
    RenderDiagnostic (..),
    WarningRecord (..)
  )
import JazzNext.Compiler.BundledPrelude
  ( loadBundledPreludeSource
  )
import JazzNext.Compiler.BuiltinCatalog
  ( BuiltinResolutionMode (..)
  )
import JazzNext.Compiler.ModuleCompiler
  ( compilePreparedPrelude,
    compileResolvedProgram
  )
import JazzNext.Compiler.ModuleInterface
  ( CompiledProgram (..),
    compileInputs
  )
import JazzNext.Compiler.ModuleResolver
  ( ModuleResolutionConfig,
    resolveProgram
  )
import JazzNext.Compiler.ModuleRuntime
  ( RuntimeProgram (runtimeProgramOutput),
    evaluateCompiledProgramWithHostObserved
  )
import JazzNext.Compiler.Prelude
  ( PreparedPrelude (..),
    ResolvedPrelude (..),
    preparePrelude,
    resolvedExplicitPrelude
  )
import JazzNext.Compiler.Runtime
  ( evaluateRuntimeExprWithHostAndBuiltinsAndBindingHintsAndSourceUnitStatementsObserved,
    renderRuntimeValue
  )
import JazzNext.Compiler.Runtime.Observation
  ( RuntimeObservationReport,
    RuntimeObservationRequest (..),
    RuntimeObservationResult (..)
  )
import JazzNext.Compiler.RuntimeHost
  ( RuntimeHost,
    disabledRuntimeHost
  )
import JazzNext.Compiler.RuntimeHints
  ( BindingRuntimeHintKey
  )
import JazzNext.Compiler.SourceProgram
  ( parseAndLowerStandaloneSource,
    scopeStatements
  )
import JazzNext.Compiler.TypeInference
  ( InferenceResult (..),
    inferExpressionWithBuiltinsAndSourceUnitStatements
  )
import JazzNext.Compiler.WarningConfig
  ( WarningSettings,
    isWarningError
  )

-- | Result of a compile-only invocation, including warnings and any promoted or
-- semantic errors.
data CompileResult = CompileResult
  { compileWarnings :: [WarningRecord],
    compileErrors :: [Diagnostic]
  }
  deriving (Eq, Show)

-- | Result of a run invocation, which may include compile-time and runtime
-- diagnostics separately.
data RunResult = RunResult
  { runWarnings :: [WarningRecord],
    runCompileErrors :: [Diagnostic],
    runRuntimeErrors :: [Diagnostic],
    runOutput :: Maybe Text,
    runRuntimeObservation :: Maybe RuntimeObservationReport
  }
  deriving (Eq, Show)

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
  (warnings, errors, _, _) <- analyzeWithWarnings hiddenStatementIndices preludeStatementIndices builtinMode settings expr
  pure
    CompileResult
      { compileWarnings = warnings,
        compileErrors = errors
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
          { compileWarnings = [],
            compileErrors = [parseErrorCode]
          }
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
          { compileWarnings = [],
            compileErrors = [diagnostic]
          }
    Right compiledProgram ->
      let warnings = compiledProgramWarnings compiledProgram
          promotedWarningErrors = map warningToError (filter (isPromoted settings) warnings)
       in pure
            CompileResult
              { compileWarnings = warnings,
                compileErrors = compiledProgramErrors compiledProgram <> promotedWarningErrors
              }

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
  (warnings, analysisErrors, canonicalExpr, runtimeTypeHints) <-
    analyzeWithWarnings hiddenStatementIndices preludeStatementIndices builtinMode settings expr
  if not (null analysisErrors)
    then
      pure
        RunResult
          { runWarnings = warnings,
            runCompileErrors = analysisErrors,
            runRuntimeErrors = [],
            runOutput = Nothing,
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
        Left runtimeError ->
          pure
            RunResult
              { runWarnings = warnings,
                runCompileErrors = [],
                runRuntimeErrors = [runtimeError],
                runOutput = Nothing,
                runRuntimeObservation = runtimeObservationReport runtimeResult
              }
        Right runtimeValue ->
          pure
            RunResult
              { runWarnings = warnings,
                runCompileErrors = [],
                runRuntimeErrors = [],
                runOutput = fmap renderRuntimeValue runtimeValue,
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
          { runWarnings = [],
            runCompileErrors = [parseErrorCode],
            runRuntimeErrors = [],
            runOutput = Nothing,
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
          { runWarnings = [],
            runCompileErrors = [diagnostic],
            runRuntimeErrors = [],
            runOutput = Nothing,
            runRuntimeObservation = Nothing
          }
    Right compiledProgram ->
      let warnings = compiledProgramWarnings compiledProgram
          promotedWarningErrors = map warningToError (filter (isPromoted settings) warnings)
          promotedCompileErrors = compiledProgramErrors compiledProgram <> promotedWarningErrors
       in if not (null promotedCompileErrors)
            then
              pure
                RunResult
                  { runWarnings = warnings,
                    runCompileErrors = promotedCompileErrors,
                    runRuntimeErrors = [],
                    runOutput = Nothing,
                    runRuntimeObservation = Nothing
                  }
            else do
              runtimeResult <- evaluateCompiledProgramWithHostObserved observationRequest host compiledProgram
              case runtimeObservationOutcome runtimeResult of
                Left runtimeError ->
                  pure
                    RunResult
                      { runWarnings = warnings,
                        runCompileErrors = [],
                        runRuntimeErrors = [runtimeError],
                        runOutput = Nothing,
                        runRuntimeObservation = runtimeObservationReport runtimeResult
                      }
                Right runtimeProgram ->
                  pure
                    RunResult
                      { runWarnings = warnings,
                        runCompileErrors = [],
                        runRuntimeErrors = [],
                        runOutput = renderRuntimeValue <$> runtimeProgramOutput runtimeProgram,
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
        resolveProgram
          resolutionConfig
          (preparedPreludeBuiltinMode preparedPrelude)
          (preparedPreludeVisibleValues preparedPrelude)
          (preparedPreludeVisibleClasses preparedPrelude)
          sourceLookup
          entryModulePath
      case resolvedResult of
        Left resolutionError -> pure (Left resolutionError)
        Right resolvedProgram -> do
          compiledPrelude <- compilePreparedPrelude settings preparedPrelude
          Right <$> compileResolvedProgram (compileInputs settings compiledPrelude) resolvedProgram

-- | Run inference/canonicalization, collect warnings from `inferredWarnings`,
-- promote configured warnings into errors, and return the canonicalized
-- `inferredExpr` for downstream compile/run steps.
analyzeWithWarnings :: Set Int -> Set Int -> BuiltinResolutionMode -> WarningSettings -> Expr -> IO ([WarningRecord], [Diagnostic], Expr, Map BindingRuntimeHintKey SignatureType)
analyzeWithWarnings hiddenStatementIndices preludeStatementIndices builtinMode settings expr = do
  inference <-
    inferExpressionWithBuiltinsAndSourceUnitStatements
      builtinMode
      hiddenStatementIndices
      preludeStatementIndices
      settings
      expr
  let warnings = inferredWarnings inference
      promotedWarnings = filter (isPromoted settings) warnings
      promotedWarningErrors = map warningToError promotedWarnings
      errors = inferredErrors inference ++ promotedWarningErrors
  pure (warnings, errors, inferredExpr inference, inferredRuntimeTypeHints inference)

isPromoted :: WarningSettings -> WarningRecord -> Bool
isPromoted settings warning = isWarningError settings (warningCategory warning)

warningToError :: WarningRecord -> Diagnostic
warningToError = toDiagnostic

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
