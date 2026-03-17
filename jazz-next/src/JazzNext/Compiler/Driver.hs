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
    compileModuleGraphWithPrelude,
    compileModuleGraphWithResolvedPrelude,
    RunResult (..),
    runExpr,
    runSource,
    runSourceWithPrelude,
    runSourceWithResolvedPrelude,
    runModuleGraphWithPrelude,
    runModuleGraphWithResolvedPrelude
  ) where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.IORef
  ( newIORef,
    readIORef,
    writeIORef
  )
import JazzNext.Compiler.AST
  ( Expr (..),
    Statement (..)
  )
import JazzNext.Compiler.Diagnostics
  ( Diagnostic,
    RenderDiagnostic (..),
    SourceSpan (..),
    WarningRecord (..),
    mkDiagnostic,
    prependDiagnosticSummary,
    setDiagnosticCode
  )
import JazzNext.Compiler.BundledPrelude
  ( loadBundledPreludeSource
  )
import JazzNext.Compiler.BuiltinCatalog
  ( BuiltinResolutionMode (..)
  )
import JazzNext.Compiler.ModuleResolver
  ( ModuleResolutionConfig,
    ResolvedModule (..),
    resolveModuleGraphWithLookup
  )
import JazzNext.Compiler.Parser
  ( parseSurfaceProgram
  )
import JazzNext.Compiler.Parser.AST
  ( SurfaceExpr
  )
import JazzNext.Compiler.Parser.Lower
  ( lowerSurfaceExpr
  )
import JazzNext.Compiler.PreludeContract
  ( validatePreludeKernelBridges
  )
import JazzNext.Compiler.Runtime
  ( evaluateRuntimeExprWithBuiltins,
    renderRuntimeValue
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
    compileErrors :: [Diagnostic],
    generatedJs :: Maybe Text
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

-- | How the driver should source the prelude for the current invocation.
data ResolvedPrelude
  = PreludeAbsent
  | PreludeBundled Text
  | PreludeExplicit Text
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
  (warnings, errors, _) <- analyzeWithWarnings hiddenStatementIndices builtinMode settings expr
  let output =
        if null errors
          then
            -- This module is only responsible for warning/error control flow.
            -- JS generation remains a placeholder until codegen lands in jazz-next.
            Just "/* jazz-next codegen placeholder */"
          else Nothing
  pure
    CompileResult
      { compileWarnings = warnings,
        compileErrors = errors,
        generatedJs = output
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
            compileErrors = [parseErrorCode],
            generatedJs = Nothing
          }
    Right loweredProgram ->
      compileExprWithBuiltinsAndHiddenStatements
        (parsedHiddenStatementIndices loweredProgram)
        (builtinResolutionMode resolvedPrelude)
        settings
        (parsedExpr loweredProgram)

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
  moduleGraphSourceResult <- loadModuleGraphSource resolutionConfig entryModulePath sourceLookup
  case moduleGraphSourceResult of
    Left resolutionError ->
      pure
        CompileResult
          { compileWarnings = [],
            compileErrors = [resolutionError],
            generatedJs = Nothing
          }
    Right sourceText ->
      compileSourceWithResolvedPrelude settings resolvedPrelude sourceText

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
  (warnings, compileErrors, canonicalExpr) <-
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
      case evaluateRuntimeExprWithBuiltins builtinMode canonicalExpr of
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
        (builtinResolutionMode resolvedPrelude)
        settings
        (parsedExpr loweredProgram)

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
  moduleGraphSourceResult <- loadModuleGraphSource resolutionConfig entryModulePath sourceLookup
  case moduleGraphSourceResult of
    Left resolutionError ->
      pure
        RunResult
          { runWarnings = [],
            runCompileErrors = [resolutionError],
            runRuntimeErrors = [],
            runOutput = Nothing
          }
    Right sourceText ->
      runSourceWithResolvedPrelude settings resolvedPrelude sourceText

-- | Run inference/canonicalization, collect warnings from `inferredWarnings`,
-- promote configured warnings into errors, and return the canonicalized
-- `inferredExpr` for downstream compile/run steps.
analyzeWithWarnings :: Set Int -> BuiltinResolutionMode -> WarningSettings -> Expr -> IO ([WarningRecord], [Diagnostic], Expr)
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
  pure (warnings, errors, inferredExpr inference)

filterWarningsForPromotion :: WarningSettings -> [WarningRecord] -> [WarningRecord]
-- Placeholder hook for future category-level filtering.
filterWarningsForPromotion _ = id

isPromoted :: WarningSettings -> WarningRecord -> Bool
isPromoted settings warning = isWarningError settings (warningCategory warning)

warningToError :: WarningRecord -> Diagnostic
warningToError = toDiagnostic

-- | The builtin lookup policy currently stays kernel-only for every prelude
-- mode; explicit preludes change source, not builtin name resolution.
builtinResolutionMode :: ResolvedPrelude -> BuiltinResolutionMode
builtinResolutionMode resolvedPrelude =
  case resolvedPrelude of
    -- Explicit no-prelude paths are now kernel-only. Public names such as
    -- `map` and `print!` require an actual prelude source; low-level no-prelude
    -- entry points may reference only the `__kernel_*` bridge symbols.
    PreludeAbsent -> ResolveKernelOnly
    PreludeBundled _ -> ResolveKernelOnly
    PreludeExplicit _ -> ResolveKernelOnly

-- | Parse the incoming source and splice in prelude statements when required,
-- tracking which synthetic statements should stay hidden from user diagnostics.
parseAndLowerSource :: ResolvedPrelude -> Text -> Either Diagnostic ParsedProgram
parseAndLowerSource resolvedPrelude source = do
  loweredSource <- parseAndLowerStandaloneSource source
  case resolvedPrelude of
    PreludeAbsent ->
      pure
        ParsedProgram
          { parsedExpr = loweredSource,
            parsedHiddenStatementIndices = Set.empty
          }
    PreludeBundled preludeText -> do
      loweredPrelude <- validateAndLowerPrelude preludeText
      let preludeStatements = scopeStatements loweredPrelude
          combinedExpr =
            EBlock (preludeStatements ++ scopeStatements loweredSource)
          hiddenStatementIndices =
            Set.fromList [0 .. length preludeStatements - 1]
      pure
        ParsedProgram
          { parsedExpr = combinedExpr,
            parsedHiddenStatementIndices = hiddenStatementIndices
          }
    PreludeExplicit preludeText -> do
      loweredPrelude <- validateAndLowerPrelude preludeText
      let preludeStatements = scopeStatements loweredPrelude
          combinedExpr =
            EBlock (preludeStatements ++ scopeStatements loweredSource)
      pure
        ParsedProgram
          { parsedExpr = combinedExpr,
            parsedHiddenStatementIndices = Set.empty
          }

data ParsedProgram = ParsedProgram
  { parsedExpr :: Expr,
    parsedHiddenStatementIndices :: Set Int
  }

resolvedExplicitPrelude :: Maybe Text -> ResolvedPrelude
resolvedExplicitPrelude maybePrelude =
  case maybePrelude of
    Nothing -> PreludeAbsent
    Just preludeText -> PreludeExplicit preludeText

-- | Parse and validate an explicit/bundled prelude before it is merged into the
-- main program source.
validateAndLowerPrelude :: Text -> Either Diagnostic Expr
validateAndLowerPrelude preludeText =
  case parseSurfaceProgram preludeText of
    Left parseError ->
      Left (setDiagnosticCode "E0002" (prependDiagnosticSummary "prelude parse error: " parseError))
    Right preludeSurfaceExpr ->
      let loweredPrelude = lowerSurfaceExpr preludeSurfaceExpr
       in
        case validatePreludeKernelBridges loweredPrelude of
          [] -> Right loweredPrelude
          firstValidationError : _ -> Left firstValidationError

parseAndLowerStandaloneSource :: Text -> Either Diagnostic Expr
parseAndLowerStandaloneSource source = do
  surfaceProgram <- parseSurfaceWithErrorCode source
  pure (lowerSurfaceExpr surfaceProgram)

scopeStatements :: Expr -> [Statement]
scopeStatements expr =
  case expr of
    EBlock statements -> statements
    _ -> [SExpr (SourceSpan 1 1) expr]

parseSurfaceWithErrorCode :: Text -> Either Diagnostic SurfaceExpr
parseSurfaceWithErrorCode source =
  case parseSurfaceProgram source of
    Left parseError ->
      Left (setDiagnosticCode "E0001" (prependDiagnosticSummary "parse error: " parseError))
    Right surfaceProgram ->
      Right surfaceProgram

-- | Resolve an entry module graph and replay the source texts in dependency
-- order so the rest of the pipeline can still operate on a single source blob.
loadModuleGraphSource ::
  ModuleResolutionConfig ->
  [Text] ->
  (FilePath -> IO (Maybe Text)) ->
  IO (Either Diagnostic Text)
loadModuleGraphSource resolutionConfig entryModulePath sourceLookup = do
  memoizedSourceLookup <- memoizeSourceLookup sourceLookup
  resolutionResult <-
    resolveModuleGraphWithLookup resolutionConfig memoizedSourceLookup entryModulePath
  case resolutionResult of
    Left resolutionError ->
      pure (Left resolutionError)
    Right resolvedModules -> do
      sourceReplayResult <- replayResolvedSources resolvedModules memoizedSourceLookup
      pure (fmap (Text.intercalate "\n") sourceReplayResult)

-- | Replay resolved source files from the memoized lookup so driver errors stay
-- stable even after resolution has already succeeded.
replayResolvedSources ::
  [ResolvedModule] ->
  (FilePath -> IO (Maybe Text)) ->
  IO (Either Diagnostic [Text])
replayResolvedSources resolvedModules sourceLookup =
  go [] resolvedModules
  where
    go acc remainingModules =
      case remainingModules of
        [] -> pure (Right (reverse acc))
        resolvedModule : rest -> do
          maybeSource <- sourceLookup (resolvedSourcePath resolvedModule)
          case maybeSource of
            Nothing ->
              pure
                ( Left
                    ( mkDiagnostic
                        "E4001"
                        ( "unresolved import '"
                            <> renderModulePath (resolvedModulePath resolvedModule)
                            <> "'; expected source at '"
                            <> Text.pack (resolvedSourcePath resolvedModule)
                            <> "'"
                        )
                    )
                )
            Just sourceText ->
              go (sourceText : acc) rest

renderModulePath :: [Text] -> Text
renderModulePath segments = Text.intercalate "::" segments

-- | Memoize source lookups so module resolution and source replay do not read
-- the same file repeatedly.
memoizeSourceLookup ::
  (FilePath -> IO (Maybe Text)) ->
  IO (FilePath -> IO (Maybe Text))
memoizeSourceLookup sourceLookup = do
  cacheRef <- newIORef (Map.empty :: Map FilePath (Maybe Text))
  pure $
    \path -> do
      cache <- readIORef cacheRef
      case Map.lookup path cache of
        Just cachedSource ->
          pure cachedSource
        Nothing -> do
          loadedSource <- sourceLookup path
          writeIORef cacheRef (Map.insert path loadedSource cache)
          pure loadedSource
