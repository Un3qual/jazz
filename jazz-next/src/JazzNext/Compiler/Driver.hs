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
    collectNeededLocalCapabilityExports,
    RunResult (..),
    runExpr,
    runSource,
    runSourceWithPrelude,
    runSourceWithResolvedPrelude,
    runModuleGraph,
    runModuleGraphWithPrelude,
    runModuleGraphWithResolvedPrelude
  ) where

import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Text (Text)
import JazzNext.Compiler.AST
  ( ConstraintSignatureType (..),
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
import JazzNext.Compiler.ModuleReplay
  ( ModuleGraphExpr (..),
    collectNeededLocalCapabilityExports,
    loadLoweredModuleGraph
  )
import JazzNext.Compiler.ModuleResolver
  ( ModuleResolutionConfig
  )
import JazzNext.Compiler.Prelude
  ( PreparedPrelude (..),
    ResolvedPrelude (..),
    preparePrelude,
    resolvedExplicitPrelude
  )
import JazzNext.Compiler.Name (renderName)
import JazzNext.Compiler.Runtime
  ( evaluateRuntimeExprWithBuiltinsAndBindingHints,
    renderRuntimeValue
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
    inferExpressionWithBuiltinsAndHiddenStatements
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
    runOutput :: Maybe Text
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
compileExprWithBuiltinsAndHiddenStatements hiddenStatementIndices builtinMode settings expr = do
  (warnings, errors, _, _) <- analyzeWithWarnings hiddenStatementIndices builtinMode settings expr
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
      compileExprWithBuiltinsAndHiddenStatements
        (parsedHiddenStatementIndices loweredProgram)
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
  case preparePrelude resolvedPrelude of
    Left preludeError ->
      pure
        CompileResult
          { compileWarnings = [],
            compileErrors = [preludeError]
          }
    Right preparedPrelude -> do
      let ambientVisibleSymbols = Set.map renderName (preparedPreludeVisibleValues preparedPrelude)
          ambientVisibleClassNames = Set.map renderName (preparedPreludeVisibleClasses preparedPrelude)
      moduleGraphExprResult <-
        loadLoweredModuleGraph
          (preparedPreludeBuiltinMode preparedPrelude)
          ambientVisibleSymbols
          ambientVisibleClassNames
          resolutionConfig
          entryModulePath
          sourceLookup
      case moduleGraphExprResult of
        Left resolutionError ->
          pure
            CompileResult
              { compileWarnings = [],
                compileErrors = [resolutionError]
              }
        Right moduleGraphExpr ->
          let loweredProgram = mergePreparedPrelude preparedPrelude (moduleGraphValidationExpr moduleGraphExpr)
           in compileExprWithBuiltinsAndHiddenStatements
                (parsedHiddenStatementIndices loweredProgram)
                (parsedBuiltinMode loweredProgram)
                settings
                (parsedExpr loweredProgram)

runExpr :: WarningSettings -> Expr -> IO RunResult
runExpr = runExprWithBuiltins ResolveKernelOnly

runExprWithBuiltins :: BuiltinResolutionMode -> WarningSettings -> Expr -> IO RunResult
runExprWithBuiltins = runExprWithBuiltinsAndHiddenStatements Set.empty

runExprWithBuiltinsAndHiddenStatements ::
  Set Int ->
  BuiltinResolutionMode ->
  WarningSettings ->
  Expr ->
  IO RunResult
runExprWithBuiltinsAndHiddenStatements hiddenStatementIndices builtinMode settings expr = do
  (warnings, compileErrors, canonicalExpr, runtimeTypeHints) <-
    analyzeWithWarnings hiddenStatementIndices builtinMode settings expr
  if not (null compileErrors)
    then
      pure
        RunResult
          { runWarnings = warnings,
            runCompileErrors = compileErrors,
            runRuntimeErrors = [],
            runOutput = Nothing
          }
    else
      case evaluateRuntimeExprWithBuiltinsAndBindingHints builtinMode runtimeTypeHints canonicalExpr of
        Left runtimeError ->
          pure
            RunResult
              { runWarnings = warnings,
                runCompileErrors = [],
                runRuntimeErrors = [runtimeError],
                runOutput = Nothing
              }
        Right runtimeValue ->
          pure
            RunResult
              { runWarnings = warnings,
                runCompileErrors = [],
                runRuntimeErrors = [],
                runOutput = fmap renderRuntimeValue runtimeValue
              }

runSource :: WarningSettings -> Text -> IO RunResult
runSource settings source = do
  bundledPreludeSource <- loadBundledPreludeSource
  runSourceWithResolvedPrelude settings (PreludeBundled bundledPreludeSource) source

runSourceWithPrelude :: WarningSettings -> Maybe Text -> Text -> IO RunResult
runSourceWithPrelude settings preludeSource source =
  runSourceWithResolvedPrelude settings (resolvedExplicitPrelude preludeSource) source

runSourceWithResolvedPrelude :: WarningSettings -> ResolvedPrelude -> Text -> IO RunResult
runSourceWithResolvedPrelude settings resolvedPrelude source =
  case parseAndLowerSource resolvedPrelude source of
    Left parseErrorCode ->
      pure
        RunResult
          { runWarnings = [],
            runCompileErrors = [parseErrorCode],
            runRuntimeErrors = [],
            runOutput = Nothing
          }
    Right loweredProgram ->
      runExprWithBuiltinsAndHiddenStatements
        (parsedHiddenStatementIndices loweredProgram)
        (parsedBuiltinMode loweredProgram)
        settings
        (parsedExpr loweredProgram)

runModuleGraph ::
  WarningSettings ->
  ModuleResolutionConfig ->
  [Text] ->
  (FilePath -> IO (Maybe Text)) ->
  IO RunResult
runModuleGraph settings resolutionConfig entryModulePath sourceLookup = do
  bundledPreludeSource <- loadBundledPreludeSource
  runModuleGraphWithResolvedPrelude
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
  runModuleGraphWithResolvedPrelude settings (resolvedExplicitPrelude preludeSource) resolutionConfig entryModulePath sourceLookup

runModuleGraphWithResolvedPrelude ::
  WarningSettings ->
  ResolvedPrelude ->
  ModuleResolutionConfig ->
  [Text] ->
  (FilePath -> IO (Maybe Text)) ->
  IO RunResult
runModuleGraphWithResolvedPrelude settings resolvedPrelude resolutionConfig entryModulePath sourceLookup = do
  case preparePrelude resolvedPrelude of
    Left preludeError ->
      pure
        RunResult
          { runWarnings = [],
            runCompileErrors = [preludeError],
            runRuntimeErrors = [],
            runOutput = Nothing
          }
    Right preparedPrelude -> do
      let ambientVisibleSymbols = Set.map renderName (preparedPreludeVisibleValues preparedPrelude)
          ambientVisibleClassNames = Set.map renderName (preparedPreludeVisibleClasses preparedPrelude)
      moduleGraphExprResult <-
        loadLoweredModuleGraph
          (preparedPreludeBuiltinMode preparedPrelude)
          ambientVisibleSymbols
          ambientVisibleClassNames
          resolutionConfig
          entryModulePath
          sourceLookup
      case moduleGraphExprResult of
        Left resolutionError ->
          pure
            RunResult
              { runWarnings = [],
                runCompileErrors = [resolutionError],
                runRuntimeErrors = [],
                runOutput = Nothing
              }
        Right moduleGraphExpr ->
          let validationProgram = mergePreparedPrelude preparedPrelude (moduleGraphValidationExpr moduleGraphExpr)
              runtimeProgram = mergePreparedPrelude preparedPrelude (moduleGraphRuntimeExpr moduleGraphExpr)
           in runExprWithValidationAndRuntimeExprs
                (parsedHiddenStatementIndices validationProgram)
                (parsedBuiltinMode validationProgram)
                settings
                (parsedExpr validationProgram)
                (parsedExpr runtimeProgram)

runExprWithValidationAndRuntimeExprs ::
  Set Int ->
  BuiltinResolutionMode ->
  WarningSettings ->
  Expr ->
  Expr ->
  IO RunResult
runExprWithValidationAndRuntimeExprs
  hiddenStatementIndices
  builtinMode
  settings
  validationExpr
  runtimeExpr = do
  (warnings, compileErrors, _, _) <-
    analyzeWithWarnings hiddenStatementIndices builtinMode settings validationExpr
  if not (null compileErrors)
    then
      pure
        RunResult
          { runWarnings = warnings,
            runCompileErrors = compileErrors,
            runRuntimeErrors = [],
            runOutput = Nothing
          }
    else do
      (_, runtimeCompileErrors, canonicalRuntimeExpr, runtimeTypeHints) <-
        analyzeWithWarnings hiddenStatementIndices builtinMode settings runtimeExpr
      if not (null runtimeCompileErrors)
        then
          pure
            RunResult
              { runWarnings = warnings,
                runCompileErrors = runtimeCompileErrors,
                runRuntimeErrors = [],
                runOutput = Nothing
              }
        else
          case evaluateRuntimeExprWithBuiltinsAndBindingHints builtinMode runtimeTypeHints canonicalRuntimeExpr of
            Left runtimeError ->
              pure
                RunResult
                  { runWarnings = warnings,
                    runCompileErrors = [],
                    runRuntimeErrors = [runtimeError],
                    runOutput = Nothing
                  }
            Right runtimeValue ->
              pure
                RunResult
                  { runWarnings = warnings,
                    runCompileErrors = [],
                    runRuntimeErrors = [],
                    runOutput = fmap renderRuntimeValue runtimeValue
                  }

-- | Run inference/canonicalization, collect warnings from `inferredWarnings`,
-- promote configured warnings into errors, and return the canonicalized
-- `inferredExpr` for downstream compile/run steps.
analyzeWithWarnings :: Set Int -> BuiltinResolutionMode -> WarningSettings -> Expr -> IO ([WarningRecord], [Diagnostic], Expr, Map BindingRuntimeHintKey ConstraintSignatureType)
analyzeWithWarnings hiddenStatementIndices builtinMode settings expr = do
  inference <-
    inferExpressionWithBuiltinsAndHiddenStatements
      builtinMode
      hiddenStatementIndices
      settings
      expr
  let warnings = filterWarningsForPromotion settings (inferredWarnings inference)
      promotedWarnings = filter (isPromoted settings) warnings
      promotedWarningErrors = map warningToError promotedWarnings
      errors = inferredErrors inference ++ promotedWarningErrors
  pure (warnings, errors, inferredExpr inference, inferredRuntimeTypeHints inference)

filterWarningsForPromotion :: WarningSettings -> [WarningRecord] -> [WarningRecord]
-- Placeholder hook for future category-level filtering.
filterWarningsForPromotion _ = id

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
          parsedBuiltinMode = preparedPreludeBuiltinMode preparedPrelude
        }
    Just loweredPrelude ->
      let preludeStatements = scopeStatements loweredPrelude
          combinedExpr =
            EBlock (preludeStatements ++ scopeStatements loweredSource)
       in ParsedProgram
            { parsedExpr = combinedExpr,
              parsedHiddenStatementIndices = preparedPreludeHiddenStatementIndices preparedPrelude,
              parsedBuiltinMode = preparedPreludeBuiltinMode preparedPrelude
            }

-- | Lowered program paired with statement indices that came from synthetic
-- bundled prelude source.
data ParsedProgram = ParsedProgram
  { parsedExpr :: Expr,
    parsedHiddenStatementIndices :: Set Int,
    parsedBuiltinMode :: BuiltinResolutionMode
  }
