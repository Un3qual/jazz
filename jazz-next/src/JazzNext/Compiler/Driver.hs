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

import Data.Text (Text)
import qualified Data.Text as Text
import Data.List
  ( foldl'
  )
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
  ( CaseArm (..),
    ClassMethodSignature (..),
    ConstraintSignatureType (..),
    DataConstructor (..),
    DataConstructorArgument (..),
    Expr (..),
    ImplMethod (..),
    Pattern (..),
    SignatureConstraint (..),
    SignaturePayload (..),
    SignatureToken (..),
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
import JazzNext.Compiler.Identifier
  ( Identifier,
    identifierText,
    isOperatorBindingIdentifierText,
    mkIdentifier,
    mkQualifiedIdentifier,
    operatorBindingIdentifierText,
    qualifiedIdentifierText,
    splitQualifiedIdentifierText
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
    resolveModuleGraphWithLookup,
    resolveModuleGraphWithLookupAndVisibleSymbols
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
import JazzNext.Compiler.Parser.Operator
  ( isBuiltinOperatorSymbol
  )
import JazzNext.Compiler.PreludeContract
  ( validatePreludeKernelBridges
  )
import JazzNext.Compiler.Runtime
  ( evaluateRuntimeExprWithBuiltinsAndBindingHints,
    renderRuntimeValue
  )
import JazzNext.Compiler.RuntimeHints
  ( BindingRuntimeHintKey
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

-- | How the driver should source the prelude for the current invocation.
data ResolvedPrelude
  = PreludeAbsent
  | PreludeBundled Text
  | PreludeExplicit Text
  deriving (Eq, Show)

-- | Parsed/lowered prelude form after source selection. The constructor records
-- whether statements should be hidden from user-facing diagnostics.
data LoweredResolvedPrelude
  = LoweredPreludeAbsent
  | LoweredPreludeBundled Expr
  | LoweredPreludeExplicit Expr

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
        (builtinResolutionMode resolvedPrelude)
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
  case lowerResolvedPrelude resolvedPrelude of
    Left preludeError ->
      pure
        CompileResult
          { compileWarnings = [],
            compileErrors = [preludeError]
          }
    Right loweredPrelude -> do
      let ambientVisibleSymbols = loweredPreludeVisibleSymbols loweredPrelude
          ambientVisibleClassNames = loweredPreludeVisibleClassNames loweredPrelude
      moduleGraphExprResult <- loadLoweredModuleGraph ambientVisibleSymbols ambientVisibleClassNames resolutionConfig entryModulePath sourceLookup
      case moduleGraphExprResult of
        Left resolutionError ->
          pure
            CompileResult
              { compileWarnings = [],
                compileErrors = [resolutionError]
              }
        Right moduleGraphExpr ->
          let loweredProgram = mergeLoweredResolvedPrelude loweredPrelude (moduleGraphValidationExpr moduleGraphExpr)
           in compileExprWithBuiltinsAndHiddenStatements
                (parsedHiddenStatementIndices loweredProgram)
                (builtinResolutionMode resolvedPrelude)
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
        (builtinResolutionMode resolvedPrelude)
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
  case lowerResolvedPrelude resolvedPrelude of
    Left preludeError ->
      pure
        RunResult
          { runWarnings = [],
            runCompileErrors = [preludeError],
            runRuntimeErrors = [],
            runOutput = Nothing
          }
    Right loweredPrelude -> do
      let ambientVisibleSymbols = loweredPreludeVisibleSymbols loweredPrelude
          ambientVisibleClassNames = loweredPreludeVisibleClassNames loweredPrelude
      moduleGraphExprResult <- loadLoweredModuleGraph ambientVisibleSymbols ambientVisibleClassNames resolutionConfig entryModulePath sourceLookup
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
          let validationProgram = mergeLoweredResolvedPrelude loweredPrelude (moduleGraphValidationExpr moduleGraphExpr)
              runtimeProgram = mergeLoweredResolvedPrelude loweredPrelude (moduleGraphRuntimeExpr moduleGraphExpr)
           in runExprWithValidationAndRuntimeExprs
                (parsedHiddenStatementIndices validationProgram)
                (builtinResolutionMode resolvedPrelude)
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
  mergeResolvedPrelude resolvedPrelude loweredSource

mergeResolvedPrelude :: ResolvedPrelude -> Expr -> Either Diagnostic ParsedProgram
mergeResolvedPrelude resolvedPrelude loweredSource =
  (`mergeLoweredResolvedPrelude` loweredSource) <$> lowerResolvedPrelude resolvedPrelude

mergeLoweredResolvedPrelude :: LoweredResolvedPrelude -> Expr -> ParsedProgram
mergeLoweredResolvedPrelude loweredResolvedPrelude loweredSource =
  case loweredResolvedPrelude of
    LoweredPreludeAbsent ->
      ParsedProgram
        { parsedExpr = loweredSource,
          parsedHiddenStatementIndices = Set.empty
        }
    LoweredPreludeBundled loweredPrelude ->
      let preludeStatements = scopeStatements loweredPrelude
          combinedExpr =
            EBlock (preludeStatements ++ scopeStatements loweredSource)
          hiddenStatementIndices =
            Set.fromList [0 .. length preludeStatements - 1]
       in ParsedProgram
            { parsedExpr = combinedExpr,
              parsedHiddenStatementIndices = hiddenStatementIndices
            }
    LoweredPreludeExplicit loweredPrelude ->
      let preludeStatements = scopeStatements loweredPrelude
          combinedExpr =
            EBlock (preludeStatements ++ scopeStatements loweredSource)
       in ParsedProgram
            { parsedExpr = combinedExpr,
              parsedHiddenStatementIndices = Set.empty
            }

-- | Lowered program paired with statement indices that came from synthetic
-- bundled prelude source.
data ParsedProgram = ParsedProgram
  { parsedExpr :: Expr,
    parsedHiddenStatementIndices :: Set Int
  }

-- | Module graph replay needs two programs: one that keeps dependency
-- expression statements for validation and one that strips them for runtime.
data ModuleGraphExpr = ModuleGraphExpr
  { moduleGraphValidationExpr :: Expr,
    moduleGraphRuntimeExpr :: Expr
  }

resolvedExplicitPrelude :: Maybe Text -> ResolvedPrelude
resolvedExplicitPrelude maybePrelude =
  case maybePrelude of
    Nothing -> PreludeAbsent
    Just preludeText -> PreludeExplicit preludeText

lowerResolvedPrelude :: ResolvedPrelude -> Either Diagnostic LoweredResolvedPrelude
lowerResolvedPrelude resolvedPrelude =
  case resolvedPrelude of
    PreludeAbsent -> Right LoweredPreludeAbsent
    PreludeBundled preludeText -> LoweredPreludeBundled <$> validateAndLowerPrelude preludeText
    PreludeExplicit preludeText -> LoweredPreludeExplicit <$> validateAndLowerPrelude preludeText

loweredPreludeVisibleSymbols :: LoweredResolvedPrelude -> Set Text
loweredPreludeVisibleSymbols loweredResolvedPrelude =
  case loweredResolvedPrelude of
    LoweredPreludeAbsent -> Set.empty
    LoweredPreludeBundled loweredPrelude -> collectVisiblePreludeBindings loweredPrelude
    LoweredPreludeExplicit loweredPrelude -> collectVisiblePreludeBindings loweredPrelude
  where
    collectVisiblePreludeBindings loweredPrelude =
      Set.fromList (collectTopLevelBindingNames loweredPrelude)

loweredPreludeVisibleClassNames :: LoweredResolvedPrelude -> Set Text
loweredPreludeVisibleClassNames loweredResolvedPrelude =
  case loweredResolvedPrelude of
    LoweredPreludeAbsent -> Set.empty
    LoweredPreludeBundled loweredPrelude -> collectVisiblePreludeClassNames loweredPrelude
    LoweredPreludeExplicit loweredPrelude -> collectVisiblePreludeClassNames loweredPrelude
  where
    collectVisiblePreludeClassNames loweredPrelude =
      Set.fromList (collectTopLevelClassNames loweredPrelude)

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

-- | Resolve and lower a module graph into validation and runtime replay
-- expressions. Dependency expressions stay present for semantic validation and
-- are stripped only from the runtime replay expression.
loadLoweredModuleGraph ::
  Set Text ->
  Set Text ->
  ModuleResolutionConfig ->
  [Text] ->
  (FilePath -> IO (Maybe Text)) ->
  IO (Either Diagnostic ModuleGraphExpr)
loadLoweredModuleGraph ambientVisibleSymbols ambientVisibleClassNames resolutionConfig entryModulePath sourceLookup = do
  memoizedSourceLookup <- memoizeSourceLookup sourceLookup
  resolutionResult <-
    resolveModuleGraphWithLookupAndVisibleSymbols resolutionConfig ambientVisibleSymbols ambientVisibleClassNames memoizedSourceLookup entryModulePath
  case resolutionResult of
    Left resolutionError ->
      pure (Left resolutionError)
    Right resolvedModules -> do
      sourceReplayResult <- replayResolvedSources resolvedModules memoizedSourceLookup
      pure $
        do
          replayedSources <- sourceReplayResult
          loweredModules <-
            sequence
              [ parseAndLowerResolvedModule resolvedModule sourceText
                | (resolvedModule, sourceText) <- zip resolvedModules replayedSources
              ]
          pure (buildModuleGraphExpr entryModulePath resolvedModules loweredModules)

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

parseAndLowerResolvedModule :: ResolvedModule -> Text -> Either Diagnostic Expr
parseAndLowerResolvedModule resolvedModule sourceText =
  case parseAndLowerStandaloneSource sourceText of
    Left parseError ->
      Left
        ( setDiagnosticCode
            "E4004"
            ( prependDiagnosticSummary
                ( "module parse error at '"
                    <> Text.pack (resolvedSourcePath resolvedModule)
                    <> "': "
                )
                parseError
            )
        )
    Right loweredSource ->
      Right loweredSource

-- | Build validation/runtime replay programs while preserving module import
-- visibility rules through qualified synthetic bindings.
buildModuleGraphExpr ::
  [Text] ->
  [ResolvedModule] ->
  [Expr] ->
  ModuleGraphExpr
buildModuleGraphExpr entryModulePath resolvedModules loweredModules =
  let exportsByModule = collectModuleExports resolvedModules loweredModules
      capabilityExportsByModule = collectModuleCapabilityExports resolvedModules loweredModules
      aliasReferencesByModule = map collectAliasQualifiedReferences loweredModules
      loweredModulesWithAliasReferences = zip loweredModules aliasReferencesByModule
      neededAliasExportsByModule = collectNeededAliasExports exportsByModule loweredModulesWithAliasReferences
      hiddenImportExportsByModule =
        collectHiddenImportExports
          exportsByModule
          loweredModules
      neededVisibleImportExportsByModule =
        collectNeededVisibleImportExports
          exportsByModule
          loweredModules
      neededVisibleImportCapabilityExportsByModule =
        collectNeededVisibleImportCapabilityExports
          capabilityExportsByModule
          loweredModules
      initialNeededModuleExportsByModule =
        Map.unionWith Set.union neededAliasExportsByModule neededVisibleImportExportsByModule
      neededModuleExportsByModule =
        expandNeededModuleExports
          resolvedModules
          loweredModules
          initialNeededModuleExportsByModule
      neededLocalCapabilityExportsByModule =
        collectNeededLocalCapabilityExports
          resolvedModules
          loweredModules
          neededModuleExportsByModule
          neededVisibleImportCapabilityExportsByModule
      neededCapabilityExportsByModule =
        Map.unionWith Set.union
          neededVisibleImportCapabilityExportsByModule
          neededLocalCapabilityExportsByModule
      (runtimeNeededModuleExportsByModule, runtimeNeededCapabilityExportsByModule) =
        closeRuntimeReplayNeeds
          resolvedModules
          loweredModules
          neededVisibleImportCapabilityExportsByModule
          neededModuleExportsByModule
          neededCapabilityExportsByModule
      replayBridgeModuleExportsByModule =
        Map.unionWith Set.union neededModuleExportsByModule runtimeNeededModuleExportsByModule
      loweredModulesWithVisibleImportReferences =
        map
          (rewriteVisibleImportReferences hiddenImportExportsByModule exportsByModule)
          loweredModules
      loweredModulesWithValidationAliasBindings =
        zipWith3
          (addAliasImportBindings exportsByModule neededModuleExportsByModule hiddenImportExportsByModule)
          resolvedModules
          loweredModulesWithVisibleImportReferences
          aliasReferencesByModule
      loweredModulesWithRuntimeAliasBindings =
        zipWith3
          (addAliasImportBindings exportsByModule replayBridgeModuleExportsByModule hiddenImportExportsByModule)
          resolvedModules
          loweredModulesWithVisibleImportReferences
          aliasReferencesByModule
      hiddenImportExportsFor resolvedModule =
        Map.findWithDefault Set.empty (resolvedModulePath resolvedModule) hiddenImportExportsByModule
      neededModuleExportsFor resolvedModule =
        Map.findWithDefault Set.empty (resolvedModulePath resolvedModule) neededModuleExportsByModule
      runtimeNeededModuleExportsFor resolvedModule =
        Map.findWithDefault Set.empty (resolvedModulePath resolvedModule) runtimeNeededModuleExportsByModule
      neededModuleCapabilityExportsFor resolvedModule =
        Map.findWithDefault Set.empty (resolvedModulePath resolvedModule) runtimeNeededCapabilityExportsByModule
      directlyNeededModuleCapabilityExportsFor resolvedModule =
        Map.findWithDefault Set.empty (resolvedModulePath resolvedModule) neededVisibleImportCapabilityExportsByModule
   in
  ModuleGraphExpr
    { moduleGraphValidationExpr =
        replayLoweredModules
          ( \resolvedModule loweredModule ->
              stripModuleDeclarations
                (resolvedModulePath resolvedModule)
                (resolvedModulePath resolvedModule == entryModulePath)
                (hiddenImportExportsFor resolvedModule)
                (neededModuleExportsFor resolvedModule)
                loweredModule
          )
          resolvedModules
          loweredModulesWithValidationAliasBindings,
      moduleGraphRuntimeExpr =
        replayLoweredModules
          ( \resolvedModule loweredModule ->
              stripModuleRuntimeReplayStatements
                (resolvedModulePath resolvedModule)
                (resolvedModulePath resolvedModule == entryModulePath)
                (hiddenImportExportsFor resolvedModule)
                (runtimeNeededModuleExportsFor resolvedModule)
                (neededModuleCapabilityExportsFor resolvedModule)
                (directlyNeededModuleCapabilityExportsFor resolvedModule)
                loweredModule
          )
          resolvedModules
          loweredModulesWithRuntimeAliasBindings
    }

collectModuleExports :: [ResolvedModule] -> [Expr] -> Map [Text] [Text]
collectModuleExports resolvedModules loweredModules =
  Map.fromList
    [ (resolvedModulePath resolvedModule, collectModuleExportNames loweredModule)
      | (resolvedModule, loweredModule) <- zip resolvedModules loweredModules
    ]

collectModuleExportNames :: Expr -> [Text]
collectModuleExportNames loweredModule =
  filter (not . isOperatorBindingIdentifierText) (collectTopLevelBindingNames loweredModule)

collectTopLevelBindingNames :: Expr -> [Text]
collectTopLevelBindingNames expr =
  case expr of
    EBlock statements ->
      concatMap collectStatementBindingNames statements
    _ -> []
  where
    collectStatementBindingNames statement =
      case statement of
        SLet bindingName _ _ ->
          [identifierText bindingName]
        SData _ _ _ constructors ->
          [ identifierText constructorName
            | DataConstructor constructorName _ <- constructors
          ]
        _ -> []

collectTopLevelClassNames :: Expr -> [Text]
collectTopLevelClassNames expr =
  case expr of
    EBlock statements ->
      [ identifierText className
        | SClass _ className _ _ <- statements
      ]
    _ -> []

collectModuleCapabilityExports :: [ResolvedModule] -> [Expr] -> Map [Text] (Set Text)
collectModuleCapabilityExports resolvedModules loweredModules =
  Map.fromList
    [ (resolvedModulePath resolvedModule, collectTopLevelCapabilityNames loweredModule)
      | (resolvedModule, loweredModule) <- zip resolvedModules loweredModules
    ]

collectHiddenImportExports ::
  Map [Text] [Text] ->
  [Expr] ->
  Map [Text] (Set Text)
collectHiddenImportExports exportsByModule loweredModules =
  Map.filter (not . Set.null) importExposures
  where
    importExposures =
      foldl'
        collectImportExposure
        Map.empty
        [ statement
          | EBlock statements <- loweredModules,
            statement <- statements
        ]

    collectImportExposure exposures statement =
      case statement of
        SImport _ modulePath maybeAlias maybeSymbolNames ->
          Map.insertWith
            Set.union
            modulePath
            (importExposure modulePath maybeAlias maybeSymbolNames)
            exposures
        _ -> exposures

    importExposure modulePath maybeAlias maybeSymbolNames =
      let exportedNames =
            Set.fromList (Map.findWithDefault [] modulePath exportsByModule)
       in case maybeAlias of
            Just _ ->
              exportedNames
            Nothing ->
              case maybeSymbolNames of
                Nothing ->
                  Set.empty
                Just symbolNames ->
                  let visibleExports = Set.intersection exportedNames (Set.fromList symbolNames)
                   in Set.difference exportedNames visibleExports

collectNeededVisibleImportExports ::
  Map [Text] [Text] ->
  [Expr] ->
  Map [Text] (Set Text)
collectNeededVisibleImportExports exportsByModule loweredModules =
  Map.unionsWith Set.union
    (map (visibleImportReferencesForModule exportsByModule) loweredModules)

visibleImportReferencesForModule ::
  Map [Text] [Text] ->
  Expr ->
  Map [Text] (Set Text)
visibleImportReferencesForModule exportsByModule expr =
  case expr of
    EBlock statements ->
      Map.fromListWith Set.union
        [ (modulePath, Set.singleton exportedName)
          | SImport _ modulePath Nothing maybeSymbolNames <- statements,
            exportedName <- Set.toList (visibleImportNames modulePath maybeSymbolNames),
            Set.member exportedName referencedNames
        ]
      where
        referencedNames = collectUnqualifiedReferences expr
        visibleImportNames modulePath maybeSymbolNames =
          let exportedNames = Set.fromList (Map.findWithDefault [] modulePath exportsByModule)
           in case maybeSymbolNames of
                Nothing -> exportedNames
                Just symbolNames -> Set.intersection exportedNames (Set.fromList symbolNames)
    _ -> Map.empty

collectNeededVisibleImportCapabilityExports ::
  Map [Text] (Set Text) ->
  [Expr] ->
  Map [Text] (Set Text)
collectNeededVisibleImportCapabilityExports capabilityExportsByModule loweredModules =
  Map.unionsWith Set.union
    (map (visibleImportCapabilityExportsForModule capabilityExportsByModule) loweredModules)

visibleImportCapabilityExportsForModule ::
  Map [Text] (Set Text) ->
  Expr ->
  Map [Text] (Set Text)
visibleImportCapabilityExportsForModule capabilityExportsByModule expr =
  case expr of
    EBlock statements ->
      Map.fromListWith Set.union
        [ (modulePath, visibleCapabilityNames modulePath maybeSymbolNames)
          | SImport _ modulePath Nothing maybeSymbolNames <- statements,
            not (Set.null (visibleCapabilityNames modulePath maybeSymbolNames))
        ]
      where
        visibleCapabilityNames modulePath maybeSymbolNames =
          let exportedCapabilityNames = Map.findWithDefault Set.empty modulePath capabilityExportsByModule
           in case maybeSymbolNames of
                Nothing -> exportedCapabilityNames
                Just symbolNames -> Set.intersection exportedCapabilityNames (Set.fromList symbolNames)
    _ -> Map.empty

collectTopLevelCapabilityNames :: Expr -> Set Text
collectTopLevelCapabilityNames expr =
  case expr of
    EBlock statements ->
      Set.fromList
        ( concatMap
            ( \statement ->
                case statement of
                  SClass _ className _ _ -> [identifierText className]
                  SImpl _ capabilityName _ _ -> [identifierText capabilityName]
                  _ -> []
            )
            statements
        )
    _ -> Set.empty

-- | Qualify references that came from imports whose other exports must stay
-- hidden, preventing dependency names from shadowing prelude/local bindings.
rewriteVisibleImportReferences ::
  Map [Text] (Set Text) ->
  Map [Text] [Text] ->
  Expr ->
  Expr
rewriteVisibleImportReferences hiddenImportExportsByModule exportsByModule expr =
  case expr of
    EBlock statements ->
      EBlock (rewriteBlockReferences importTargets Set.empty statements)
      where
        importTargets = visibleImportReferenceTargets statements
    _ -> expr
  where
    visibleImportReferenceTargets statements =
      Map.fromList
        [ (exportedName, modulePath)
          | SImport _ modulePath Nothing maybeSymbolNames <- statements,
            let hiddenExports = Map.findWithDefault Set.empty modulePath hiddenImportExportsByModule,
            exportedName <- Set.toList (visibleImportNames modulePath maybeSymbolNames),
            Set.member exportedName hiddenExports
        ]

    visibleImportNames modulePath maybeSymbolNames =
      let exportedNames = Set.fromList (Map.findWithDefault [] modulePath exportsByModule)
       in case maybeSymbolNames of
            Nothing -> exportedNames
            Just symbolNames -> Set.intersection exportedNames (Set.fromList symbolNames)

rewriteBlockReferences :: Map Text [Text] -> Set Text -> [Statement] -> [Statement]
rewriteBlockReferences importTargets outerBoundNames statements =
  map (rewriteStatementReferences importTargets blockBoundNames) statements
  where
    blockBoundNames =
      Set.union
        outerBoundNames
        (Set.fromList (concatMap statementBindingNames statements))

statementBindingNames :: Statement -> [Text]
statementBindingNames statement =
  case statement of
    SLet bindingName _ _ ->
      [identifierText bindingName]
    SData _ _ _ constructors ->
      [ identifierText constructorName
        | DataConstructor constructorName _ <- constructors
      ]
    _ -> []

rewriteStatementReferences :: Map Text [Text] -> Set Text -> Statement -> Statement
rewriteStatementReferences importTargets boundNames statement =
  case statement of
    SLet bindingName spanValue valueExpr ->
      SLet bindingName spanValue (rewriteExprReferences importTargets boundNames valueExpr)
    SExpr spanValue exprValue ->
      SExpr spanValue (rewriteExprReferences importTargets boundNames exprValue)
    SImpl spanValue capabilityName arguments methods ->
      SImpl
        spanValue
        capabilityName
        arguments
        [ ImplMethod methodName methodSpan (rewriteExprReferences importTargets boundNames methodExpr)
          | ImplMethod methodName methodSpan methodExpr <- methods
        ]
    _ -> statement

rewriteExprReferences :: Map Text [Text] -> Set Text -> Expr -> Expr
rewriteExprReferences importTargets boundNames expression =
  case expression of
    ELit _ -> expression
    EVar name ->
      EVar (rewriteReferenceIdentifier importTargets boundNames name)
    ELambda parameterName bodyExpr ->
      ELambda
        parameterName
        (rewriteExprReferences importTargets (Set.insert (identifierText parameterName) boundNames) bodyExpr)
    EOperatorValue _ -> expression
    EList elements ->
      EList (map (rewriteExprReferences importTargets boundNames) elements)
    ETuple elements ->
      ETuple (map (rewriteExprReferences importTargets boundNames) elements)
    EApply functionExpr argumentExpr ->
      EApply
        (rewriteExprReferences importTargets boundNames functionExpr)
        (rewriteExprReferences importTargets boundNames argumentExpr)
    EIf conditionExpr trueBranch falseBranch ->
      EIf
        (rewriteExprReferences importTargets boundNames conditionExpr)
        (rewriteExprReferences importTargets boundNames trueBranch)
        (rewriteExprReferences importTargets boundNames falseBranch)
    ECase conditionExpr trueBranch falseBranch ->
      ECase
        (rewriteExprReferences importTargets boundNames conditionExpr)
        (rewriteExprReferences importTargets boundNames trueBranch)
        (rewriteExprReferences importTargets boundNames falseBranch)
    EPatternCase scrutineeExpr caseArms ->
      EPatternCase
        (rewriteExprReferences importTargets boundNames scrutineeExpr)
        [ CaseArm
            (rewritePatternReferences importTargets boundNames patternValue)
            (fmap (rewriteExprReferences importTargets armBoundNames) guardExpr)
            (rewriteExprReferences importTargets armBoundNames bodyExpr)
          | CaseArm patternValue guardExpr bodyExpr <- caseArms,
            let armBoundNames = Set.union boundNames (patternBinders patternValue)
        ]
    EBinary operatorName leftExpr rightExpr ->
      EBinary
        operatorName
        (rewriteExprReferences importTargets boundNames leftExpr)
        (rewriteExprReferences importTargets boundNames rightExpr)
    ESectionLeft leftExpr operatorName ->
      ESectionLeft (rewriteExprReferences importTargets boundNames leftExpr) operatorName
    ESectionRight operatorName rightExpr ->
      ESectionRight operatorName (rewriteExprReferences importTargets boundNames rightExpr)
    EBlock nestedStatements ->
      EBlock (rewriteBlockReferences importTargets boundNames nestedStatements)

rewriteReferenceIdentifier :: Map Text [Text] -> Set Text -> Identifier -> Identifier
rewriteReferenceIdentifier importTargets boundNames name =
  let nameText = identifierText name
   in case Map.lookup nameText importTargets of
        Just modulePath
          | Set.notMember nameText boundNames ->
              mkIdentifier (moduleExportQualifiedName modulePath nameText)
        _ -> name

collectOperatorBindingNames :: Expr -> Set Text
collectOperatorBindingNames expr =
  case expr of
    EBlock statements ->
      Set.fromList
        [ bindingNameText
          | SLet bindingName _ _ <- statements,
            let bindingNameText = identifierText bindingName,
            isOperatorBindingIdentifierText bindingNameText
        ]
    _ -> Set.empty

rewriteOperatorBindingReferences :: [Text] -> Set Text -> Expr -> Expr
rewriteOperatorBindingReferences modulePath replayedOperatorBindings expression =
  case expression of
    ELit _ -> expression
    EVar _ -> expression
    ELambda parameterName bodyExpr ->
      ELambda parameterName (rewriteOperatorBindingReferences modulePath replayedOperatorBindings bodyExpr)
    EOperatorValue operatorName ->
      case operatorReplayReference operatorName of
        Just operatorReference -> EVar operatorReference
        Nothing -> expression
    EList elements ->
      EList (map rewriteOperatorReference elements)
    ETuple elements ->
      ETuple (map rewriteOperatorReference elements)
    EApply functionExpr argumentExpr ->
      EApply
        (rewriteOperatorReference functionExpr)
        (rewriteOperatorReference argumentExpr)
    EIf conditionExpr trueBranch falseBranch ->
      EIf
        (rewriteOperatorReference conditionExpr)
        (rewriteOperatorReference trueBranch)
        (rewriteOperatorReference falseBranch)
    ECase conditionExpr trueBranch falseBranch ->
      ECase
        (rewriteOperatorReference conditionExpr)
        (rewriteOperatorReference trueBranch)
        (rewriteOperatorReference falseBranch)
    EPatternCase scrutineeExpr caseArms ->
      EPatternCase
        (rewriteOperatorReference scrutineeExpr)
        [ CaseArm
            patternValue
            (fmap rewriteOperatorReference guardExpr)
            (rewriteOperatorReference bodyExpr)
          | CaseArm patternValue guardExpr bodyExpr <- caseArms
        ]
    EBinary operatorName leftExpr rightExpr ->
      let rewrittenLeft = rewriteOperatorReference leftExpr
          rewrittenRight = rewriteOperatorReference rightExpr
       in case operatorReplayReference operatorName of
            Just operatorReference ->
              EApply
                (EApply (EVar operatorReference) rewrittenLeft)
                rewrittenRight
            Nothing ->
              EBinary operatorName rewrittenLeft rewrittenRight
    ESectionLeft leftExpr operatorName ->
      let rewrittenLeft = rewriteOperatorReference leftExpr
       in case operatorReplayReference operatorName of
            Just operatorReference ->
              EApply (EVar operatorReference) rewrittenLeft
            Nothing ->
              ESectionLeft rewrittenLeft operatorName
    ESectionRight operatorName rightExpr ->
      let rewrittenRight = rewriteOperatorReference rightExpr
       in case operatorReplayReference operatorName of
            Just operatorReference ->
              ELambda
                operatorReplaySectionLeftParameter
                ( EApply
                    (EApply (EVar operatorReference) (EVar operatorReplaySectionLeftParameter))
                    rewrittenRight
                )
            Nothing ->
              ESectionRight operatorName rewrittenRight
    EBlock statements ->
      EBlock (map rewriteOperatorReferenceStatement statements)
  where
    rewriteOperatorReference =
      rewriteOperatorBindingReferences modulePath replayedOperatorBindings

    rewriteOperatorReferenceStatement statement =
      case statement of
        SLet bindingName spanValue valueExpr ->
          SLet bindingName spanValue (rewriteOperatorReference valueExpr)
        SExpr spanValue exprValue ->
          SExpr spanValue (rewriteOperatorReference exprValue)
        SImpl spanValue capabilityName arguments methods ->
          SImpl
            spanValue
            capabilityName
            arguments
            [ ImplMethod methodName methodSpan (rewriteOperatorReference methodExpr)
              | ImplMethod methodName methodSpan methodExpr <- methods
            ]
        _ -> statement

    operatorReplayReference operatorName
      | isBuiltinOperatorSymbol operatorName = Nothing
      | Set.member bindingName replayedOperatorBindings =
          Just (mkIdentifier (moduleExportQualifiedName modulePath bindingName))
      | otherwise = Nothing
      where
        bindingName = operatorBindingIdentifierText operatorName

    operatorReplaySectionLeftParameter =
      mkIdentifier "$operator_replay_section_left"

rewritePatternReferences :: Map Text [Text] -> Set Text -> Pattern -> Pattern
rewritePatternReferences importTargets boundNames patternValue =
  case patternValue of
    PWildcard -> PWildcard
    PVariable name -> PVariable name
    PLiteral literalValue -> PLiteral literalValue
    PConstructor constructorName nestedPatterns ->
      PConstructor
        (rewriteReferenceIdentifier importTargets boundNames constructorName)
        (map (rewritePatternReferences importTargets boundNames) nestedPatterns)
    PList nestedPatterns ->
      PList (map (rewritePatternReferences importTargets boundNames) nestedPatterns)
    PConsList headPattern tailPattern ->
      PConsList
        (rewritePatternReferences importTargets boundNames headPattern)
        (rewritePatternReferences importTargets boundNames tailPattern)
    PTuple nestedPatterns ->
      PTuple (map (rewritePatternReferences importTargets boundNames) nestedPatterns)
    PAs name nestedPattern ->
      PAs
        name
        ( rewritePatternReferences
            importTargets
            (Set.insert (identifierText name) boundNames)
            nestedPattern
        )
    POr alternatives ->
      POr (map (rewritePatternReferences importTargets boundNames) alternatives)

patternBinders :: Pattern -> Set Text
patternBinders patternValue =
  case patternValue of
    PWildcard -> Set.empty
    PVariable name -> Set.singleton (identifierText name)
    PLiteral _ -> Set.empty
    PConstructor _ nestedPatterns -> Set.unions (map patternBinders nestedPatterns)
    PList nestedPatterns -> Set.unions (map patternBinders nestedPatterns)
    PConsList headPattern tailPattern ->
      Set.union (patternBinders headPattern) (patternBinders tailPattern)
    PTuple nestedPatterns -> Set.unions (map patternBinders nestedPatterns)
    PAs name nestedPattern ->
      Set.insert (identifierText name) (patternBinders nestedPattern)
    POr alternatives ->
      commonPatternBinders alternatives

commonPatternBinders :: [Pattern] -> Set Text
commonPatternBinders alternatives =
  case alternatives of
    [] -> Set.empty
    firstAlternative : rest ->
      foldl'
        Set.intersection
        (patternBinders firstAlternative)
        (map patternBinders rest)

collectUnqualifiedReferences :: Expr -> Set Text
collectUnqualifiedReferences expr =
  case expr of
    ELit _ -> Set.empty
    EVar name ->
      let nameText = identifierText name
       in case splitQualifiedIdentifierText nameText of
            Just _ -> Set.empty
            Nothing -> Set.singleton nameText
    ELambda parameterName bodyExpr ->
      Set.delete (identifierText parameterName) (collectUnqualifiedReferences bodyExpr)
    EOperatorValue operatorName -> operatorBindingReferences operatorName
    EList elements -> Set.unions (map collectUnqualifiedReferences elements)
    ETuple elements -> Set.unions (map collectUnqualifiedReferences elements)
    EApply functionExpr argumentExpr ->
      Set.union (collectUnqualifiedReferences functionExpr) (collectUnqualifiedReferences argumentExpr)
    EIf conditionExpr trueBranch falseBranch ->
      Set.unions
        [ collectUnqualifiedReferences conditionExpr,
          collectUnqualifiedReferences trueBranch,
          collectUnqualifiedReferences falseBranch
        ]
    ECase conditionExpr trueBranch falseBranch ->
      Set.unions
        [ collectUnqualifiedReferences conditionExpr,
          collectUnqualifiedReferences trueBranch,
          collectUnqualifiedReferences falseBranch
        ]
    EPatternCase scrutineeExpr caseArms ->
      Set.unions
        [ collectUnqualifiedReferences scrutineeExpr,
          Set.unions
            [ Set.union
                (patternConstructorReferences patternValue)
                ( Set.difference
                    ( Set.union
                        (maybe Set.empty collectUnqualifiedReferences guardExpr)
                        (collectUnqualifiedReferences bodyExpr)
                    )
                    (patternBinders patternValue)
                )
              | CaseArm patternValue guardExpr bodyExpr <- caseArms
            ]
        ]
    EBinary operatorName leftExpr rightExpr ->
      Set.unions
        [ operatorBindingReferences operatorName,
          collectUnqualifiedReferences leftExpr,
          collectUnqualifiedReferences rightExpr
        ]
    ESectionLeft leftExpr operatorName ->
      Set.union
        (operatorBindingReferences operatorName)
        (collectUnqualifiedReferences leftExpr)
    ESectionRight operatorName rightExpr ->
      Set.union
        (operatorBindingReferences operatorName)
        (collectUnqualifiedReferences rightExpr)
    EBlock statements ->
      Set.difference
        ( Set.unions
            [ case statement of
                SLet _ _ valueExpr -> collectUnqualifiedReferences valueExpr
                SExpr _ exprValue -> collectUnqualifiedReferences exprValue
                SImpl _ _ _ methods ->
                  Set.unions
                    [ collectUnqualifiedReferences methodExpr
                      | ImplMethod _ _ methodExpr <- methods
                    ]
                _ -> Set.empty
              | statement <- statements
            ]
        )
        (Set.fromList (concatMap statementBindingNames statements))

operatorBindingReferences :: Text -> Set Text
operatorBindingReferences operatorName
  | isBuiltinOperatorSymbol operatorName = Set.empty
  | otherwise = Set.singleton (operatorBindingIdentifierText operatorName)

patternConstructorReferences :: Pattern -> Set Text
patternConstructorReferences patternValue =
  case patternValue of
    PWildcard -> Set.empty
    PVariable _ -> Set.empty
    PLiteral _ -> Set.empty
    PConstructor constructorName nestedPatterns ->
      Set.insert (identifierText constructorName) (Set.unions (map patternConstructorReferences nestedPatterns))
    PList nestedPatterns -> Set.unions (map patternConstructorReferences nestedPatterns)
    PConsList headPattern tailPattern ->
      Set.union (patternConstructorReferences headPattern) (patternConstructorReferences tailPattern)
    PTuple nestedPatterns -> Set.unions (map patternConstructorReferences nestedPatterns)
    PAs _ nestedPattern -> patternConstructorReferences nestedPattern
    POr alternatives -> Set.unions (map patternConstructorReferences alternatives)

expandNeededModuleExports ::
  [ResolvedModule] ->
  [Expr] ->
  Map [Text] (Set Text) ->
  Map [Text] (Set Text)
expandNeededModuleExports resolvedModules loweredModules neededByModule =
  foldl' expandModule neededByModule (zip resolvedModules loweredModules)
  where
    expandModule neededByModule (resolvedModule, loweredModule) =
      let modulePath = resolvedModulePath resolvedModule
          neededExports = Map.findWithDefault Set.empty modulePath neededByModule
          expandedExports = closeExportDependencies (collectExportDependencies loweredModule) neededExports
       in if Set.null expandedExports
            then neededByModule
            else Map.insert modulePath expandedExports neededByModule

collectExportDependencies :: Expr -> Map Text (Set Text)
collectExportDependencies expr =
  case expr of
    EBlock statements ->
      let exportedNames =
            Set.fromList (concatMap statementBindingNames statements)
       in Map.fromList
            [ (identifierText bindingName, Set.intersection exportedNames (collectUnqualifiedReferences valueExpr))
              | SLet bindingName _ valueExpr <- statements
            ]
    _ -> Map.empty

closeExportDependencies :: Map Text (Set Text) -> Set Text -> Set Text
closeExportDependencies exportDependencies neededExports =
  let expandedExports =
        Set.union
          neededExports
          ( Set.unions
              [ Map.findWithDefault Set.empty exportName exportDependencies
                | exportName <- Set.toList neededExports
              ]
          )
   in if expandedExports == neededExports
        then neededExports
        else closeExportDependencies exportDependencies expandedExports

closeRuntimeReplayNeeds ::
  [ResolvedModule] ->
  [Expr] ->
  Map [Text] (Set Text) ->
  Map [Text] (Set Text) ->
  Map [Text] (Set Text) ->
  (Map [Text] (Set Text), Map [Text] (Set Text))
closeRuntimeReplayNeeds resolvedModules loweredModules directlyNeededCapabilityExportsByModule neededModuleExportsByModule neededCapabilityExportsByModule =
  let neededImplBodyValueExportsByModule =
        collectNeededImplMethodValueExports
          resolvedModules
          loweredModules
          neededCapabilityExportsByModule
      expandedNeededModuleExportsByModule =
        expandNeededModuleExports
          resolvedModules
          loweredModules
          (Map.unionWith Set.union neededModuleExportsByModule neededImplBodyValueExportsByModule)
      expandedNeededLocalCapabilityExportsByModule =
        collectNeededLocalCapabilityExports
          resolvedModules
          loweredModules
          expandedNeededModuleExportsByModule
          directlyNeededCapabilityExportsByModule
      expandedNeededCapabilityExportsByModule =
        Map.unionWith Set.union
          directlyNeededCapabilityExportsByModule
          expandedNeededLocalCapabilityExportsByModule
   in if expandedNeededModuleExportsByModule == neededModuleExportsByModule
        && expandedNeededCapabilityExportsByModule == neededCapabilityExportsByModule
        then (expandedNeededModuleExportsByModule, expandedNeededCapabilityExportsByModule)
        else
          closeRuntimeReplayNeeds
            resolvedModules
            loweredModules
            directlyNeededCapabilityExportsByModule
            expandedNeededModuleExportsByModule
            expandedNeededCapabilityExportsByModule

collectNeededImplMethodValueExports ::
  [ResolvedModule] ->
  [Expr] ->
  Map [Text] (Set Text) ->
  Map [Text] (Set Text)
collectNeededImplMethodValueExports resolvedModules loweredModules neededCapabilityExportsByModule =
  Map.fromList
    [ (modulePath, neededValueExports)
      | (resolvedModule, loweredModule) <- zip resolvedModules loweredModules,
        let modulePath = resolvedModulePath resolvedModule,
        let neededCapabilities = Map.findWithDefault Set.empty modulePath neededCapabilityExportsByModule,
        let valueExports = Set.fromList (collectTopLevelBindingNames loweredModule),
        let neededValueExports = implMethodValueDependencies loweredModule valueExports neededCapabilities,
        not (Set.null neededValueExports)
    ]

implMethodValueDependencies :: Expr -> Set Text -> Set Text -> Set Text
implMethodValueDependencies expr valueExports neededCapabilities =
  case expr of
    EBlock statements ->
      Set.intersection
        valueExports
        ( Set.unions
            [ collectUnqualifiedReferences methodExpr
              | SImpl _ capabilityName _ methods <- statements,
                Set.member (identifierText capabilityName) neededCapabilities,
                ImplMethod _ _ methodExpr <- methods
            ]
        )
    _ ->
      Set.empty

collectNeededLocalCapabilityExports ::
  [ResolvedModule] ->
  [Expr] ->
  Map [Text] (Set Text) ->
  Map [Text] (Set Text) ->
  Map [Text] (Set Text)
collectNeededLocalCapabilityExports resolvedModules loweredModules neededModuleExportsByModule directlyNeededCapabilityExportsByModule =
  Map.fromList
    [ (modulePath, neededCapabilities)
      | (resolvedModule, loweredModule) <- zip resolvedModules loweredModules,
        let modulePath = resolvedModulePath resolvedModule,
        let neededExports = Map.findWithDefault Set.empty modulePath neededModuleExportsByModule,
        let directlyNeededCapabilities = Map.findWithDefault Set.empty modulePath directlyNeededCapabilityExportsByModule,
        let neededCapabilities = localCapabilityDependenciesForExports loweredModule neededExports directlyNeededCapabilities,
        not (Set.null neededCapabilities)
    ]

localCapabilityDependenciesForExports :: Expr -> Set Text -> Set Text -> Set Text
localCapabilityDependenciesForExports expr neededExports directlyNeededCapabilities =
  case expr of
    EBlock statements ->
      closeLocalCapabilityDependencies statements localCapabilityNames directDependencies
      where
        localCapabilityNames = collectTopLevelCapabilityNames expr
        directDependencies =
          Set.unions
            [ directlyNeededCapabilities,
              Set.unions
                [ collectLocalCapabilityReferences localCapabilityNames valueExpr
                  | SLet bindingName _ valueExpr <- statements,
                    Set.member (identifierText bindingName) neededExports
                ],
              Set.unions
                [ collectLocalCapabilityReferencesFromSignaturePayload localCapabilityNames signaturePayload
                  | SSignature signatureName _ signaturePayload <- statements,
                    Set.member (identifierText signatureName) neededExports
                ],
              Set.unions
                [ collectLocalCapabilityReferencesFromSignaturePayload localCapabilityNames methodSignature
                  | SClass _ className _ methods <- statements,
                    Set.member (identifierText className) directlyNeededCapabilities,
                    ClassMethodSignature _ _ methodSignature <- methods
                ]
            ]
    _ -> Set.empty

collectLocalCapabilityReferencesFromSignaturePayload :: Set Text -> SignaturePayload -> Set Text
collectLocalCapabilityReferencesFromSignaturePayload localCapabilityNames signaturePayload =
  case signaturePayload of
    ConstrainedSignature constraints signatureType ->
      Set.union
        (Set.unions (map (collectLocalCapabilityReferencesFromConstraint localCapabilityNames) constraints))
        (collectLocalCapabilityReferencesFromConstraintType localCapabilityNames signatureType)
    _ -> Set.empty

collectLocalCapabilityReferencesFromConstraint :: Set Text -> SignatureConstraint -> Set Text
collectLocalCapabilityReferencesFromConstraint localCapabilityNames (SignatureConstraint constraintName arguments) =
  Set.unions
    ( [ Set.singleton constraintNameText
        | Set.member constraintNameText localCapabilityNames
      ]
        ++ map (collectLocalCapabilityReferencesFromConstraintType localCapabilityNames) arguments
    )
  where
    constraintNameText = identifierText constraintName

collectLocalCapabilityReferencesFromConstraintType :: Set Text -> ConstraintSignatureType -> Set Text
collectLocalCapabilityReferencesFromConstraintType localCapabilityNames signatureType =
  case signatureType of
    ConstraintTypeName typeName ->
      let typeNameText = identifierText typeName
       in Set.fromList [typeNameText | Set.member typeNameText localCapabilityNames]
    ConstraintTypeApplication typeName arguments ->
      Set.filter
        (`Set.member` localCapabilityNames)
        ( Set.insert
            (identifierText typeName)
            (Set.unions (map (collectLocalCapabilityReferencesFromConstraintType localCapabilityNames) arguments))
        )
    ConstraintTypeList innerType ->
      collectLocalCapabilityReferencesFromConstraintType localCapabilityNames innerType
    ConstraintTypeTuple elementTypes ->
      Set.unions (map (collectLocalCapabilityReferencesFromConstraintType localCapabilityNames) elementTypes)
    ConstraintTypeFunction argumentType resultType ->
      Set.union
        (collectLocalCapabilityReferencesFromConstraintType localCapabilityNames argumentType)
        (collectLocalCapabilityReferencesFromConstraintType localCapabilityNames resultType)

closeLocalCapabilityDependencies :: [Statement] -> Set Text -> Set Text -> Set Text
closeLocalCapabilityDependencies statements localCapabilityNames neededCapabilities =
  let expandedCapabilities =
        Set.union
          neededCapabilities
          ( Set.unions
              [ Set.unions
                  [ collectLocalCapabilityReferencesFromSignaturePayload localCapabilityNames methodSignature
                    | SClass _ capabilityName _ methods <- statements,
                      Set.member (identifierText capabilityName) neededCapabilities,
                      ClassMethodSignature _ _ methodSignature <- methods
                  ],
                Set.unions
                  [ Set.unions
                      [ collectLocalCapabilityReferences localCapabilityNames methodExpr
                        | ImplMethod _ _ methodExpr <- methods
                      ]
                    | SImpl _ capabilityName _ methods <- statements,
                      Set.member (identifierText capabilityName) neededCapabilities
                  ]
              ]
          )
   in if expandedCapabilities == neededCapabilities
        then neededCapabilities
        else closeLocalCapabilityDependencies statements localCapabilityNames expandedCapabilities

collectLocalCapabilityReferences :: Set Text -> Expr -> Set Text
collectLocalCapabilityReferences localCapabilityNames expr =
  Set.fromList
    [ capabilityName
      | (capabilityName, _) <- Set.toList (collectAliasQualifiedReferencePairs expr),
        Set.member capabilityName localCapabilityNames
    ]

collectNeededAliasExports ::
  Map [Text] [Text] ->
  [(Expr, Map Text (Set Text))] ->
  Map [Text] (Set Text)
collectNeededAliasExports exportsByModule =
  foldl' collectModule Map.empty
  where
    collectModule neededExports (expr, aliasReferences) =
      Map.unionWith Set.union neededExports (collectNeededAliasExportsFromModule expr aliasReferences)

    collectNeededAliasExportsFromModule expr aliasReferences =
      case expr of
        EBlock statements ->
          foldl' (collectImportNeededExports aliasReferences) Map.empty statements
        _ -> Map.empty

    collectImportNeededExports aliasReferences neededExports statement =
      case statement of
        SImport _ modulePath (Just aliasName) Nothing ->
          let referencedNames = Map.findWithDefault Set.empty aliasName aliasReferences
              exportedNames = Set.fromList (Map.findWithDefault [] modulePath exportsByModule)
              neededNames = Set.intersection referencedNames exportedNames
           in if Set.null neededNames
                then neededExports
                else Map.insertWith Set.union modulePath neededNames neededExports
        _ -> neededExports

-- | Insert synthetic alias-qualified bridge bindings required by `Alias::name`
-- references without making alias-only exports visible unqualified.
addAliasImportBindings ::
  Map [Text] [Text] ->
  Map [Text] (Set Text) ->
  Map [Text] (Set Text) ->
  ResolvedModule ->
  Expr ->
  Map Text (Set Text) ->
  Expr
addAliasImportBindings exportsByModule neededModuleExportsByModule hiddenImportExportsByModule resolvedModule expr aliasReferences =
  case expr of
    EBlock statements ->
      EBlock (insertAliasBindings (concatMap aliasBindingsForStatement statements) statements)
    _ -> expr
  where
    sourceExportNames =
      Map.findWithDefault Set.empty (resolvedModulePath resolvedModule) neededModuleExportsByModule

    hiddenSourceExportNames =
      Map.findWithDefault Set.empty (resolvedModulePath resolvedModule) hiddenImportExportsByModule

    insertAliasBindings aliasBindings statements =
      case statements of
        moduleStatement@(SModule _ _) : rest ->
          moduleStatement : aliasBindings ++ concatMap expandStatement rest
        _ ->
          aliasBindings ++ concatMap expandStatement statements

    expandStatement statement =
      statement : sourceExportBindingsForStatement statement

    sourceExportBindingsForStatement statement =
      case statement of
        SLet exportedName spanValue valueExpr
          | Set.member (identifierText exportedName) sourceExportNames,
            not (isOperatorBindingIdentifierText (identifierText exportedName)) ->
              [ SLet
                  (mkIdentifier (moduleExportQualifiedName (resolvedModulePath resolvedModule) (identifierText exportedName)))
                  spanValue
                  ( if Set.member (identifierText exportedName) hiddenSourceExportNames
                      then
                        rewriteModuleExportReferences
                          (resolvedModulePath resolvedModule)
                          sourceExportNames
                          valueExpr
                      else EVar exportedName
                  )
              ]
        _ -> []

    aliasBindingsForStatement statement =
      case statement of
        SImport spanValue modulePath (Just aliasName) Nothing ->
          [ SLet
              (mkQualifiedIdentifier aliasName exportedName)
              spanValue
              (EVar (mkIdentifier (moduleExportQualifiedName modulePath exportedName)))
            | let referencedNames = Map.findWithDefault Set.empty aliasName aliasReferences,
              let exportedNames = Set.fromList (Map.findWithDefault [] modulePath exportsByModule),
              exportedName <- Set.toList (Set.intersection referencedNames exportedNames)
          ]
        _ -> []

moduleExportQualifiedName :: [Text] -> Text -> Text
moduleExportQualifiedName modulePath exportedName =
  qualifiedIdentifierText "__module" (renderModulePath modulePath <> "::" <> exportedName)

rewriteModuleExportReferences :: [Text] -> Set Text -> Expr -> Expr
rewriteModuleExportReferences modulePath exportNames =
  rewriteExprReferences importTargets Set.empty
  where
    importTargets =
      Map.fromList
        [ (exportName, modulePath)
          | exportName <- Set.toList exportNames
        ]

collectAliasQualifiedReferences :: Expr -> Map Text (Set Text)
collectAliasQualifiedReferences expr =
  Map.fromListWith Set.union
    [ (aliasName, Set.singleton memberName)
      | (aliasName, memberName) <- Set.toList (collectAliasQualifiedReferencePairs expr)
    ]

collectAliasQualifiedReferencePairs :: Expr -> Set (Text, Text)
collectAliasQualifiedReferencePairs expr =
  case expr of
    ELit _ -> Set.empty
    EVar name ->
      case splitQualifiedIdentifierText (identifierText name) of
        Just qualifiedName -> Set.singleton qualifiedName
        Nothing -> Set.empty
    ELambda _ bodyExpr ->
      collectAliasQualifiedReferencePairs bodyExpr
    EOperatorValue _ -> Set.empty
    EList elements ->
      Set.unions (map collectAliasQualifiedReferencePairs elements)
    ETuple elements ->
      Set.unions (map collectAliasQualifiedReferencePairs elements)
    EApply functionExpr argumentExpr ->
      Set.union
        (collectAliasQualifiedReferencePairs functionExpr)
        (collectAliasQualifiedReferencePairs argumentExpr)
    EIf conditionExpr trueBranch falseBranch ->
      Set.unions
        [ collectAliasQualifiedReferencePairs conditionExpr,
          collectAliasQualifiedReferencePairs trueBranch,
          collectAliasQualifiedReferencePairs falseBranch
        ]
    ECase conditionExpr trueBranch falseBranch ->
      Set.unions
        [ collectAliasQualifiedReferencePairs conditionExpr,
          collectAliasQualifiedReferencePairs trueBranch,
          collectAliasQualifiedReferencePairs falseBranch
        ]
    EPatternCase scrutineeExpr caseArms ->
      Set.unions
        ( collectAliasQualifiedReferencePairs scrutineeExpr :
          [ Set.union
              (maybe Set.empty collectAliasQualifiedReferencePairs guardExpr)
              (collectAliasQualifiedReferencePairs bodyExpr)
          | CaseArm _ guardExpr bodyExpr <- caseArms
          ]
        )
    EBinary _ leftExpr rightExpr ->
      Set.union
        (collectAliasQualifiedReferencePairs leftExpr)
        (collectAliasQualifiedReferencePairs rightExpr)
    ESectionLeft leftExpr _ ->
      collectAliasQualifiedReferencePairs leftExpr
    ESectionRight _ rightExpr ->
      collectAliasQualifiedReferencePairs rightExpr
    EBlock statements ->
      Set.unions (map collectAliasQualifiedReferencesFromStatement statements)

collectAliasQualifiedReferencesFromStatement :: Statement -> Set (Text, Text)
collectAliasQualifiedReferencesFromStatement statement =
  case statement of
    SLet _ _ valueExpr ->
      collectAliasQualifiedReferencePairs valueExpr
    SExpr _ expr ->
      collectAliasQualifiedReferencePairs expr
    SImpl _ _ _ methods ->
      Set.unions
        [ collectAliasQualifiedReferencePairs methodExpr
          | ImplMethod _ _ methodExpr <- methods
        ]
    SSignature {} -> Set.empty
    SData {} -> Set.empty
    SClass {} -> Set.empty
    SModule {} -> Set.empty
    SImport {} -> Set.empty

replayLoweredModules ::
  (ResolvedModule -> Expr -> Expr) ->
  [ResolvedModule] ->
  [Expr] ->
  Expr
replayLoweredModules transformModule resolvedModules loweredModules =
  EBlock
    ( concat
        [ scopeStatements (transformModule resolvedModule loweredModule)
          | (resolvedModule, loweredModule) <- zip resolvedModules loweredModules
        ]
    )

stripModuleDeclarations :: [Text] -> Bool -> Set Text -> Set Text -> Expr -> Expr
stripModuleDeclarations modulePath isEntryModule hiddenImportExports neededModuleExports expr =
  case expr of
    EBlock statements ->
      EBlock (ensureModuleValidationBoundary (concatMap keepModuleValidationStatement statements))
    _ -> expr
  where
    ensureModuleValidationBoundary statements =
      case statements of
        SModule {} : _ -> statements
        _ -> SModule (SourceSpan 1 1) modulePath : statements

    keepModuleValidationStatement statement =
      case statement of
        SModule {} -> [statement]
        SLet bindingName spanValue valueExpr
          | shouldQualifyOperatorBinding bindingName ->
              [ SLet
                  (operatorReplayIdentifier bindingName)
                  spanValue
                  (rewriteValidationReplayExpr valueExpr)
              ]
        SLet bindingName spanValue valueExpr
          | Set.member (identifierText bindingName) hiddenImportExports ->
              if Set.member (identifierText bindingName) neededModuleExports
                then []
                else
                  [ SLet
                      (hiddenValidationIdentifier bindingName)
                      spanValue
                      (rewriteValidationReplayExpr valueExpr)
                  ]
        SLet bindingName spanValue valueExpr ->
          [ SLet
              bindingName
              spanValue
              (rewriteValidationReplayExpr valueExpr)
          ]
        SExpr spanValue exprValue ->
          [ SExpr
              spanValue
              (rewriteValidationReplayExpr exprValue)
          ]
        SSignature signatureName spanValue signatureValue
          | Set.member (identifierText signatureName) hiddenImportExports ->
              [ SSignature
                  (hiddenValidationIdentifier signatureName)
                  spanValue
                  (rewriteModuleExportSignaturePayload modulePath dataTypeNames signatureValue)
              ]
        SData spanValue typeName typeParameters constructors ->
          rewriteDataStatementForReplay
            modulePath
            hiddenImportExports
            (Set.union hiddenImportExports neededModuleExports)
            spanValue
            typeName
            typeParameters
            constructors
        SClass spanValue capabilityName parameters methods ->
          [ SClass
              spanValue
              capabilityName
              parameters
              (rewriteModuleExportClassMethods modulePath dataTypeNames methods)
          ]
        SImpl spanValue capabilityName arguments methods ->
          [ SImpl
              spanValue
              capabilityName
              (rewriteModuleExportImplArguments modulePath dataTypeNames arguments)
              (rewriteValidationReplayImplMethods methods)
          ]
        _ -> [statement]
    dataTypeNames = collectDataTypeNames expr
    replayedOperatorBindings =
      if isEntryModule
        then Set.empty
        else collectOperatorBindingNames expr

    shouldQualifyOperatorBinding bindingName =
      Set.member (identifierText bindingName) replayedOperatorBindings

    operatorReplayIdentifier name =
      mkIdentifier (moduleExportQualifiedName modulePath (identifierText name))

    rewriteValidationReplayExpr =
      rewriteOperatorBindingReferences modulePath replayedOperatorBindings
        . rewriteModuleExportReferences modulePath hiddenImportExports

    rewriteValidationReplayImplMethods methods =
      [ ImplMethod methodName methodSpan (rewriteValidationReplayExpr methodExpr)
        | ImplMethod methodName methodSpan methodExpr <- methods
      ]

    hiddenValidationIdentifier name =
      mkIdentifier (moduleExportQualifiedName modulePath (identifierText name))

stripModuleRuntimeReplayStatements :: [Text] -> Bool -> Set Text -> Set Text -> Set Text -> Set Text -> Expr -> Expr
stripModuleRuntimeReplayStatements modulePath isEntryModule hiddenImportExports neededModuleExports neededCapabilityExports directlyNeededCapabilityExports expr =
  case expr of
    EBlock statements ->
      EBlock (ensureModuleRuntimeBoundary (concatMap keepModuleRuntimeReplayStatement statements))
    _ -> expr
  where
    ensureModuleRuntimeBoundary statements =
      case statements of
        SModule {} : _ -> statements
        _ -> SModule (SourceSpan 1 1) modulePath : statements

    keepModuleRuntimeReplayStatement statement =
      case statement of
        SModule {} -> [statement]
        SExpr spanValue exprValue ->
          [ SExpr
              spanValue
              (rewriteRuntimeReplayExpr exprValue)
            | isEntryModule
          ]
        SData spanValue typeName typeParameters constructors ->
          rewriteDataStatementForReplay modulePath hiddenImportExports neededModuleExports spanValue typeName typeParameters constructors
        SClass spanValue capabilityName parameters methods ->
          [ SClass
              spanValue
              (runtimeReplayCapabilityName capabilityName)
              parameters
              (rewriteRuntimeReplayClassMethods methods)
            | isEntryModule || Set.member (identifierText capabilityName) neededCapabilityExports
          ]
        SLet bindingName spanValue valueExpr
          | shouldQualifyOperatorBinding bindingName,
            shouldKeepRuntimeBinding bindingName ->
              [ SLet
                  (operatorReplayIdentifier bindingName)
                  spanValue
                  (rewriteRuntimeReplayExpr valueExpr)
              ]
        SLet bindingName _ _
          | shouldQualifyOperatorBinding bindingName -> []
        SLet bindingName spanValue valueExpr
          | shouldKeepRuntimeBinding bindingName,
            Set.notMember (identifierText bindingName) hiddenImportExports ->
              [ SLet
                  bindingName
                  spanValue
                  (rewriteRuntimeReplayExpr valueExpr)
              ]
        SLet {} -> []
        SSignature signatureName spanValue signatureValue
          | shouldKeepRuntimeBinding signatureName,
            Set.notMember (identifierText signatureName) hiddenImportExports ->
              [ SSignature
                  signatureName
                  spanValue
                  (rewriteRuntimeReplaySignaturePayload signatureValue)
              ]
        SSignature signatureName spanValue signatureValue
          | Set.member (identifierText signatureName) hiddenImportExports,
            Set.member (identifierText signatureName) neededModuleExports ->
              [ SSignature
                  (hiddenValidationIdentifier signatureName)
                  spanValue
                  (rewriteRuntimeReplaySignaturePayload signatureValue)
              ]
        SSignature {} -> []
        SImpl spanValue capabilityName arguments methods ->
          [ SImpl
              spanValue
              (runtimeReplayCapabilityName capabilityName)
              (rewriteRuntimeReplayImplArguments arguments)
              (rewriteRuntimeReplayImplMethods methods)
            | isEntryModule || Set.member (identifierText capabilityName) neededCapabilityExports
          ]
        _ | isHiddenImportExportStatement hiddenImportExports statement -> []
        _ -> [statement]
    dataTypeNames = collectDataTypeNames expr
    hiddenRuntimeCapabilities =
      if isEntryModule
        then Set.empty
        else Set.difference neededCapabilityExports directlyNeededCapabilityExports
    replayedOperatorBindings =
      if isEntryModule
        then Set.empty
        else collectOperatorBindingNames expr

    shouldKeepRuntimeBinding bindingName =
      isEntryModule || isNeededRuntimeBindingName (identifierText bindingName)

    isNeededRuntimeBindingName bindingNameText =
      Set.member bindingNameText neededModuleExports
        || case Text.stripPrefix (moduleExportQualifiedPrefix modulePath) bindingNameText of
          Just exportedName -> Set.member exportedName neededModuleExports
          Nothing -> False

    shouldQualifyOperatorBinding bindingName =
      Set.member (identifierText bindingName) replayedOperatorBindings

    operatorReplayIdentifier name =
      mkIdentifier (moduleExportQualifiedName modulePath (identifierText name))

    hiddenValidationIdentifier name =
      mkIdentifier (moduleExportQualifiedName modulePath (identifierText name))

    runtimeReplayCapabilityName capabilityName =
      let capabilityNameText = identifierText capabilityName
       in if Set.member capabilityNameText hiddenRuntimeCapabilities
            then mkIdentifier (moduleExportQualifiedName modulePath capabilityNameText)
            else capabilityName

    rewriteRuntimeReplayClassMethods methods =
      [ ClassMethodSignature methodName methodSpan (rewriteRuntimeReplaySignaturePayload methodSignature)
        | ClassMethodSignature methodName methodSpan methodSignature <- methods
      ]

    rewriteRuntimeReplayImplArguments =
      map rewriteRuntimeReplayConstraintType

    rewriteRuntimeReplayImplMethods methods =
      [ ImplMethod
          methodName
          methodSpan
          (rewriteRuntimeReplayExpr methodExpr)
        | ImplMethod methodName methodSpan methodExpr <- methods
      ]

    rewriteRuntimeReplayExpr =
      rewriteHiddenCapabilityReferences modulePath hiddenRuntimeCapabilities
        . rewriteOperatorBindingReferences modulePath replayedOperatorBindings
        . rewriteModuleExportReferences modulePath hiddenImportExports

    rewriteRuntimeReplaySignaturePayload signaturePayload =
      case signaturePayload of
        ConstrainedSignature constraints signatureType ->
          ConstrainedSignature
            (map rewriteRuntimeReplaySignatureConstraint constraints)
            (rewriteRuntimeReplayConstraintType signatureType)
        UnsupportedSignature signatureTokens ->
          UnsupportedSignature (map rewriteRuntimeReplaySignatureToken signatureTokens)
        _ -> signaturePayload

    rewriteRuntimeReplaySignatureConstraint (SignatureConstraint constraintName arguments) =
      SignatureConstraint
        (runtimeReplayCapabilityName constraintName)
        (map rewriteRuntimeReplayConstraintType arguments)

    rewriteRuntimeReplayConstraintType signatureType =
      case rewriteModuleExportImplArgument modulePath dataTypeNames signatureType of
        ConstraintTypeName name ->
          ConstraintTypeName (runtimeReplayCapabilityName name)
        ConstraintTypeApplication name arguments ->
          ConstraintTypeApplication
            (runtimeReplayCapabilityName name)
            (map rewriteRuntimeReplayConstraintType arguments)
        ConstraintTypeList innerType ->
          ConstraintTypeList (rewriteRuntimeReplayConstraintType innerType)
        ConstraintTypeTuple elementTypes ->
          ConstraintTypeTuple (map rewriteRuntimeReplayConstraintType elementTypes)
        ConstraintTypeFunction argumentType resultType ->
          ConstraintTypeFunction
            (rewriteRuntimeReplayConstraintType argumentType)
            (rewriteRuntimeReplayConstraintType resultType)

    rewriteRuntimeReplaySignatureToken signatureToken =
      rewriteModuleExportSignatureToken modulePath dataTypeNames signatureToken

moduleExportQualifiedPrefix :: [Text] -> Text
moduleExportQualifiedPrefix modulePath =
  qualifiedIdentifierText "__module" (renderModulePath modulePath <> "::")

rewriteHiddenCapabilityReferences :: [Text] -> Set Text -> Expr -> Expr
rewriteHiddenCapabilityReferences modulePath hiddenCapabilities =
  rewriteExprCapabilityReferences Set.empty
  where
    rewriteExprCapabilityReferences boundNames expression =
      case expression of
        ELit _ -> expression
        EVar name ->
          EVar (rewriteCapabilityReferenceIdentifier boundNames name)
        ELambda parameterName bodyExpr ->
          ELambda
            parameterName
            (rewriteExprCapabilityReferences (Set.insert (identifierText parameterName) boundNames) bodyExpr)
        EOperatorValue _ -> expression
        EList elements ->
          EList (map (rewriteExprCapabilityReferences boundNames) elements)
        ETuple elements ->
          ETuple (map (rewriteExprCapabilityReferences boundNames) elements)
        EApply functionExpr argumentExpr ->
          EApply
            (rewriteExprCapabilityReferences boundNames functionExpr)
            (rewriteExprCapabilityReferences boundNames argumentExpr)
        EIf conditionExpr trueBranch falseBranch ->
          EIf
            (rewriteExprCapabilityReferences boundNames conditionExpr)
            (rewriteExprCapabilityReferences boundNames trueBranch)
            (rewriteExprCapabilityReferences boundNames falseBranch)
        ECase conditionExpr trueBranch falseBranch ->
          ECase
            (rewriteExprCapabilityReferences boundNames conditionExpr)
            (rewriteExprCapabilityReferences boundNames trueBranch)
            (rewriteExprCapabilityReferences boundNames falseBranch)
        EPatternCase scrutineeExpr caseArms ->
          EPatternCase
            (rewriteExprCapabilityReferences boundNames scrutineeExpr)
            [ CaseArm
                patternValue
                (fmap (rewriteExprCapabilityReferences armBoundNames) guardExpr)
                (rewriteExprCapabilityReferences armBoundNames bodyExpr)
              | CaseArm patternValue guardExpr bodyExpr <- caseArms,
                let armBoundNames = Set.union boundNames (patternBinders patternValue)
            ]
        EBinary operatorName leftExpr rightExpr ->
          EBinary
            operatorName
            (rewriteExprCapabilityReferences boundNames leftExpr)
            (rewriteExprCapabilityReferences boundNames rightExpr)
        ESectionLeft leftExpr operatorName ->
          ESectionLeft (rewriteExprCapabilityReferences boundNames leftExpr) operatorName
        ESectionRight operatorName rightExpr ->
          ESectionRight operatorName (rewriteExprCapabilityReferences boundNames rightExpr)
        EBlock nestedStatements ->
          EBlock (rewriteBlockCapabilityReferences boundNames nestedStatements)

    rewriteBlockCapabilityReferences outerBoundNames statements =
      map (rewriteStatementCapabilityReferences blockBoundNames) statements
      where
        blockBoundNames =
          Set.union
            outerBoundNames
            (Set.fromList (concatMap statementBindingNames statements))

    rewriteStatementCapabilityReferences boundNames statement =
      case statement of
        SLet bindingName spanValue valueExpr ->
          SLet bindingName spanValue (rewriteExprCapabilityReferences boundNames valueExpr)
        SExpr spanValue exprValue ->
          SExpr spanValue (rewriteExprCapabilityReferences boundNames exprValue)
        SImpl spanValue capabilityName arguments methods ->
          SImpl
            spanValue
            capabilityName
            arguments
            [ ImplMethod methodName methodSpan (rewriteExprCapabilityReferences boundNames methodExpr)
              | ImplMethod methodName methodSpan methodExpr <- methods
            ]
        _ -> statement

    rewriteCapabilityReferenceIdentifier boundNames name =
      let nameText = identifierText name
       in case splitQualifiedIdentifierText nameText of
        Just (capabilityName, methodName)
          | Set.member capabilityName hiddenCapabilities,
            Set.notMember nameText boundNames ->
              mkIdentifier (qualifiedIdentifierText (moduleExportQualifiedName modulePath capabilityName) methodName)
        _ -> name

collectDataTypeNames :: Expr -> Set Text
collectDataTypeNames expr =
  case expr of
    EBlock statements ->
      Set.fromList
        [ identifierText typeName
          | SData _ typeName _ _ <- statements
        ]
    _ -> Set.empty

rewriteModuleExportImplArguments ::
  [Text] ->
  Set Text ->
  [ConstraintSignatureType] ->
  [ConstraintSignatureType]
rewriteModuleExportImplArguments modulePath dataTypeNames arguments =
  map (rewriteModuleExportImplArgument modulePath dataTypeNames) arguments

rewriteModuleExportClassMethods ::
  [Text] ->
  Set Text ->
  [ClassMethodSignature] ->
  [ClassMethodSignature]
rewriteModuleExportClassMethods modulePath dataTypeNames methods =
  [ ClassMethodSignature
      methodName
      methodSpan
      (rewriteModuleExportSignaturePayload modulePath dataTypeNames methodSignature)
    | ClassMethodSignature methodName methodSpan methodSignature <- methods
  ]

rewriteModuleExportSignaturePayload ::
  [Text] ->
  Set Text ->
  SignaturePayload ->
  SignaturePayload
rewriteModuleExportSignaturePayload modulePath dataTypeNames signaturePayload =
  case signaturePayload of
    ConstrainedSignature constraints signatureType ->
      ConstrainedSignature
        (map (rewriteModuleExportSignatureConstraint modulePath dataTypeNames) constraints)
        (rewriteModuleExportImplArgument modulePath dataTypeNames signatureType)
    UnsupportedSignature signatureTokens ->
      UnsupportedSignature (map (rewriteModuleExportSignatureToken modulePath dataTypeNames) signatureTokens)
    _ -> signaturePayload

rewriteModuleExportSignatureConstraint ::
  [Text] ->
  Set Text ->
  SignatureConstraint ->
  SignatureConstraint
rewriteModuleExportSignatureConstraint modulePath dataTypeNames (SignatureConstraint constraintName arguments) =
  SignatureConstraint
    constraintName
    (map (rewriteModuleExportImplArgument modulePath dataTypeNames) arguments)

rewriteModuleExportSignatureToken ::
  [Text] ->
  Set Text ->
  SignatureToken ->
  SignatureToken
rewriteModuleExportSignatureToken modulePath dataTypeNames signatureToken =
  case signatureToken of
    SignatureNameToken name
      | Set.member name dataTypeNames ->
          SignatureNameToken (moduleExportQualifiedName modulePath name)
    _ -> signatureToken

rewriteModuleExportImplArgument ::
  [Text] ->
  Set Text ->
  ConstraintSignatureType ->
  ConstraintSignatureType
rewriteModuleExportImplArgument modulePath dataTypeNames signatureType =
  case signatureType of
    ConstraintTypeName name ->
      ConstraintTypeName (rewriteModuleExportImplTypeName modulePath dataTypeNames name)
    ConstraintTypeApplication name arguments ->
      ConstraintTypeApplication
        (rewriteModuleExportImplTypeName modulePath dataTypeNames name)
        (map (rewriteModuleExportImplArgument modulePath dataTypeNames) arguments)
    ConstraintTypeList innerType ->
      ConstraintTypeList (rewriteModuleExportImplArgument modulePath dataTypeNames innerType)
    ConstraintTypeTuple elementTypes ->
      ConstraintTypeTuple (map (rewriteModuleExportImplArgument modulePath dataTypeNames) elementTypes)
    ConstraintTypeFunction argumentType resultType ->
      ConstraintTypeFunction
        (rewriteModuleExportImplArgument modulePath dataTypeNames argumentType)
        (rewriteModuleExportImplArgument modulePath dataTypeNames resultType)

rewriteModuleExportImplTypeName :: [Text] -> Set Text -> Identifier -> Identifier
rewriteModuleExportImplTypeName modulePath dataTypeNames typeName =
  let typeNameText = identifierText typeName
   in if Set.member typeNameText dataTypeNames
        then mkIdentifier (moduleExportQualifiedName modulePath typeNameText)
        else typeName

rewriteModuleExportImplMethods :: [Text] -> Set Text -> [ImplMethod] -> [ImplMethod]
rewriteModuleExportImplMethods modulePath hiddenImportExports methods =
  [ ImplMethod methodName methodSpan (rewriteModuleExportReferences modulePath hiddenImportExports methodExpr)
    | ImplMethod methodName methodSpan methodExpr <- methods
  ]

rewriteDataStatementForReplay ::
  [Text] ->
  Set Text ->
  Set Text ->
  SourceSpan ->
  Identifier ->
  [Identifier] ->
  [DataConstructor] ->
  [Statement]
rewriteDataStatementForReplay modulePath hiddenImportExports neededModuleExports spanValue typeName typeParameters constructors =
  [ SData spanValue replayTypeName typeParameters replayConstructors
    | not (null replayConstructors)
  ]
  where
    replayTypeName =
      mkIdentifier (moduleExportQualifiedName modulePath (identifierText typeName))

    replayConstructors =
      [ replayConstructor
        | DataConstructor constructorName constructorArguments <- constructors,
          let constructorText = identifierText constructorName,
          let hiddenConstructor = Set.member constructorText hiddenImportExports,
          not hiddenConstructor || Set.member constructorText neededModuleExports,
          let replayConstructorArguments = map replayConstructorArgument constructorArguments,
          let replayConstructor =
                if hiddenConstructor
                  then DataConstructor (mkIdentifier (moduleExportQualifiedName modulePath constructorText)) replayConstructorArguments
                  else DataConstructor constructorName replayConstructorArguments
      ]

    replayConstructorArgument constructorArgument =
      case constructorArgument of
        DataConstructorArgumentName argumentName
          | identifierText argumentName == identifierText typeName ->
              DataConstructorArgumentName replayTypeName
        _ -> constructorArgument

isHiddenImportExportStatement :: Set Text -> Statement -> Bool
isHiddenImportExportStatement hiddenImportExports statement =
  case statement of
    SLet bindingName _ _ -> Set.member (identifierText bindingName) hiddenImportExports
    SSignature signatureName _ _ -> Set.member (identifierText signatureName) hiddenImportExports
    _ -> False

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
