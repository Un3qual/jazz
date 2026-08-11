{-# LANGUAGE OverloadedStrings #-}

module Jazz.Benchmark.StageInputs
  ( PreparedBenchmark,
    PreparedCompilerScaleBenchmark,
    prepareBenchmark,
    prepareCompilerScaleBenchmark,
    runCompilerScaleCase,
    runPreparedBenchmark,
    runPreparedCompilerScaleBenchmark,
    selectProgramCases,
  )
where

import Control.DeepSeq (NFData (rnf))
import Control.Exception (evaluate)
import Data.Text (Text)
import qualified Data.Text as Text
import Jazz.Benchmark.Force
  ( forceCompiledModule,
    forceCompiledModules,
    forceCompiledProgram,
    forceCompiledProgramResult,
    forceExpr,
    forceProgramCaseResult,
    forceRuntimeProgramOutputResult,
    forceSurfaceExpr,
    forceTokens,
  )
import Jazz.Benchmark.ScaleCases
  ( CompilerScaleCase,
    compilerScaleCaseEntryModulePath,
    compilerScaleCaseExpectedOutput,
    compilerScaleCaseIdentifier,
    compilerScaleCaseResolutionConfig,
    compilerScaleCaseSource,
  )
import Jazz.Compiler.BundledPrelude (bundledPreludeSource)
import Jazz.Compiler.Diagnostics (Diagnostic)
import Jazz.Compiler.Diagnostics.Render (renderDiagnostic)
import Jazz.Compiler.Driver (ResolvedPrelude (PreludeBundled), buildCompiledProgram)
import Jazz.Compiler.ModuleCompiler (compileResolvedModule)
import Jazz.Compiler.ModuleGraph (ResolvedModule (..))
import Jazz.Compiler.ModuleInterface
  ( CompileInputs,
    CompiledModule (..),
    CompiledProgram (..),
    compileInputs,
    compiledModuleErrors,
    compiledProgramErrors,
    lookupCompiledModule,
  )
import Jazz.Compiler.ModuleRuntime
  ( RuntimeProgram (runtimeProgramOutput),
    evaluateCompiledProgram,
  )
import Jazz.Compiler.Parser (parseSurfaceProgramTokens)
import Jazz.Compiler.Parser.Lexer (tokenize)
import Jazz.Compiler.Parser.Lower (lowerSurfaceExpr)
import Jazz.Compiler.Profiling
  ( BenchmarkGroup (..),
    CompilerStage (..),
    withCompilerStage,
  )
import Jazz.Compiler.Runtime (renderRuntimeValue)
import Jazz.Compiler.WarningConfig (defaultWarningSettings)
import Jazz.ProgramCorpus.Runner
  ( ProgramCaseResult (..),
    loadProgramCaseEntrySource,
    prepareProgramCase,
    runProgramCase,
  )
import Jazz.ProgramCorpus.Types
  ( ProgramCase (..),
    ProgramTermination (..),
  )

data PreparedBenchmark
  = PreparedParseLower ProgramCase Text
  | PreparedAnalysis ProgramCase CompiledProgram CompileInputs [CompiledModule] ResolvedModule
  | PreparedModulePreparation ProgramCase
  | PreparedRuntime ProgramCase CompiledProgram
  | PreparedWholeProgram ProgramCase

data PreparedCompilerScaleBenchmark
  = PreparedCompilerScaleAnalysis CompilerScaleCase CompiledProgram CompileInputs [CompiledModule] ResolvedModule
  | PreparedCompilerScaleModulePreparation CompilerScaleCase
  | PreparedCompilerScaleWholeProgram CompilerScaleCase

instance NFData PreparedBenchmark where
  rnf preparedBenchmark =
    case preparedBenchmark of
      PreparedParseLower programCase source ->
        programCaseIdentifier programCase `seq` Text.length source `seq` ()
      PreparedAnalysis programCase compiledProgram inputs dependencies resolvedModule ->
        programCaseIdentifier programCase `seq`
          forceCompiledProgram compiledProgram `seq`
            inputs `seq`
              forceCompiledModules dependencies `seq`
                resolvedModule `seq`
                  ()
      PreparedModulePreparation programCase -> programCaseIdentifier programCase `seq` ()
      PreparedRuntime programCase compiledProgram ->
        programCaseIdentifier programCase `seq` forceCompiledProgram compiledProgram
      PreparedWholeProgram programCase -> programCaseIdentifier programCase `seq` ()

instance NFData PreparedCompilerScaleBenchmark where
  rnf preparedBenchmark =
    case preparedBenchmark of
      PreparedCompilerScaleAnalysis programCase compiledProgram inputs dependencies resolvedModule ->
        rnf programCase `seq`
          forceCompiledProgram compiledProgram `seq`
            inputs `seq`
              forceCompiledModules dependencies `seq`
                resolvedModule `seq`
                  ()
      PreparedCompilerScaleModulePreparation programCase -> rnf programCase
      PreparedCompilerScaleWholeProgram programCase -> rnf programCase

prepareBenchmark :: BenchmarkGroup -> ProgramCase -> IO PreparedBenchmark
prepareBenchmark benchmarkGroup programCase =
  case benchmarkGroup of
    ParseLowerBenchmark -> do
      source <-
        withCompilerStage SourceLoadingStage $ do
          sourceResult <- loadProgramCaseEntrySource programCase
          case sourceResult of
            Left diagnostic -> failBenchmarkDiagnostic diagnostic
            Right loadedSource -> do
              _ <- evaluate (Text.length loadedSource)
              pure loadedSource
      pure (PreparedParseLower programCase source)
    AnalysisBenchmark -> do
      compiledProgram <- prepareValidProgram programCase
      entryModule <-
        case lookupCompiledModule (compiledProgramEntryPath compiledProgram) compiledProgram of
          Nothing -> ioError (userError "compiled corpus program is missing its entry module")
          Just value -> pure value
      let entryPath = compiledProgramEntryPath compiledProgram
          dependencies =
            filter
              ((/= entryPath) . resolvedModulePath . compiledResolvedModule)
              (compiledProgramModules compiledProgram)
          inputs = compileInputs defaultWarningSettings (compiledProgramPrelude compiledProgram)
      pure
        ( PreparedAnalysis
            programCase
            compiledProgram
            inputs
            dependencies
            (compiledResolvedModule entryModule)
        )
    ModulePreparationBenchmark -> pure (PreparedModulePreparation programCase)
    RuntimeBenchmark -> PreparedRuntime programCase <$> prepareValidProgram programCase
    WholeProgramBenchmark -> pure (PreparedWholeProgram programCase)

prepareCompilerScaleBenchmark :: BenchmarkGroup -> CompilerScaleCase -> IO PreparedCompilerScaleBenchmark
prepareCompilerScaleBenchmark benchmarkGroup programCase =
  case benchmarkGroup of
    AnalysisBenchmark -> do
      compiledProgram <- prepareValidCompilerScaleProgram programCase
      entryModule <- requireCompilerScaleEntryModule programCase compiledProgram
      let entryPath = compiledProgramEntryPath compiledProgram
          dependencies =
            filter
              ((/= entryPath) . resolvedModulePath . compiledResolvedModule)
              (compiledProgramModules compiledProgram)
          inputs = compileInputs defaultWarningSettings (compiledProgramPrelude compiledProgram)
      pure
        ( PreparedCompilerScaleAnalysis
            programCase
            compiledProgram
            inputs
            dependencies
            (compiledResolvedModule entryModule)
        )
    ModulePreparationBenchmark -> pure (PreparedCompilerScaleModulePreparation programCase)
    WholeProgramBenchmark -> pure (PreparedCompilerScaleWholeProgram programCase)
    ParseLowerBenchmark -> unsupportedCompilerScaleGroup benchmarkGroup programCase
    RuntimeBenchmark -> unsupportedCompilerScaleGroup benchmarkGroup programCase

runPreparedBenchmark :: PreparedBenchmark -> IO ()
runPreparedBenchmark preparedBenchmark =
  case preparedBenchmark of
    PreparedParseLower _ source -> runParseLower source
    PreparedAnalysis _ _ inputs dependencies resolvedModule -> do
      compiledModule <-
        withCompilerStage TypeInferenceStage $ do
          value <- compileResolvedModule inputs dependencies resolvedModule
          evaluate (forceCompiledModule value)
          pure value
      case compiledModuleErrors compiledModule of
        [] -> pure ()
        diagnostic : _ -> failBenchmarkDiagnostic diagnostic
    PreparedModulePreparation programCase -> do
      compiledResult <-
        withCompilerStage RuntimePreparationStage (prepareProgramCase programCase)
      evaluate (forceCompiledProgramResult compiledResult)
      case compiledResult of
        Left diagnostic -> failBenchmarkDiagnostic diagnostic
        Right compiledProgram -> requireNoCompileErrors compiledProgram
    PreparedRuntime programCase compiledProgram ->
      withCompilerStage EvaluationStage $ do
        let runtimeResult = evaluateCompiledProgram compiledProgram
        evaluate (forceRuntimeProgramOutputResult runtimeResult)
        requireExpectedRuntimeResult programCase runtimeResult
    PreparedWholeProgram programCase -> do
      result <- runProgramCase programCase
      evaluate (forceProgramCaseResult result)
      requireExpectedProgramResult programCase result

runPreparedCompilerScaleBenchmark :: PreparedCompilerScaleBenchmark -> IO ()
runPreparedCompilerScaleBenchmark preparedBenchmark =
  case preparedBenchmark of
    PreparedCompilerScaleAnalysis _ _ inputs dependencies resolvedModule -> do
      compiledModule <-
        withCompilerStage TypeInferenceStage $ do
          value <- compileResolvedModule inputs dependencies resolvedModule
          evaluate (forceCompiledModule value)
          pure value
      case compiledModuleErrors compiledModule of
        [] -> pure ()
        diagnostic : _ -> failBenchmarkDiagnostic diagnostic
    PreparedCompilerScaleModulePreparation programCase -> do
      compiledResult <- buildCompilerScaleProgram programCase
      evaluate (forceCompiledProgramResult compiledResult)
      case compiledResult of
        Left diagnostic -> failBenchmarkDiagnostic diagnostic
        Right compiledProgram -> requireNoCompileErrors compiledProgram
    PreparedCompilerScaleWholeProgram programCase -> do
      actualOutput <- runCompilerScaleCase programCase
      if actualOutput == compilerScaleCaseExpectedOutput programCase
        then pure ()
        else
          ioError
            ( userError
                ( "compiler scale benchmark did not preserve expected output: "
                    <> Text.unpack (compilerScaleCaseIdentifier programCase)
                )
            )

runParseLower :: Text -> IO ()
runParseLower source = do
  tokens <-
    withCompilerStage LexingStage $ do
      tokenResult <- evaluate (tokenize source)
      case tokenResult of
        Left diagnostic -> failBenchmarkDiagnostic diagnostic
        Right values -> evaluate (forceTokens values) >> pure values
  surfaceProgram <-
    withCompilerStage ParsingStage $ do
      parseResult <- evaluate (parseSurfaceProgramTokens tokens)
      case parseResult of
        Left diagnostic -> failBenchmarkDiagnostic diagnostic
        Right value -> evaluate (forceSurfaceExpr value) >> pure value
  withCompilerStage LoweringStage $ do
    let expression = lowerSurfaceExpr surfaceProgram
    evaluate (forceExpr expression)

prepareValidProgram :: ProgramCase -> IO CompiledProgram
prepareValidProgram programCase = do
  compiledResult <- prepareProgramCase programCase
  evaluate (forceCompiledProgramResult compiledResult)
  case compiledResult of
    Left diagnostic -> failBenchmarkDiagnostic diagnostic
    Right compiledProgram -> requireNoCompileErrors compiledProgram >> pure compiledProgram

buildCompilerScaleProgram :: CompilerScaleCase -> IO (Either Diagnostic CompiledProgram)
buildCompilerScaleProgram programCase =
  buildCompiledProgram
    defaultWarningSettings
    (PreludeBundled bundledPreludeSource)
    (compilerScaleCaseResolutionConfig programCase)
    (compilerScaleCaseEntryModulePath programCase)
    (pure . compilerScaleCaseSource programCase)

prepareValidCompilerScaleProgram :: CompilerScaleCase -> IO CompiledProgram
prepareValidCompilerScaleProgram programCase = do
  compiledResult <- buildCompilerScaleProgram programCase
  evaluate (forceCompiledProgramResult compiledResult)
  case compiledResult of
    Left diagnostic -> failBenchmarkDiagnostic diagnostic
    Right compiledProgram -> requireNoCompileErrors compiledProgram >> pure compiledProgram

requireCompilerScaleEntryModule :: CompilerScaleCase -> CompiledProgram -> IO CompiledModule
requireCompilerScaleEntryModule programCase compiledProgram =
  case lookupCompiledModule (compiledProgramEntryPath compiledProgram) compiledProgram of
    Nothing ->
      ioError
        ( userError
            ( "compiled scale program is missing its entry module: "
                <> Text.unpack (compilerScaleCaseIdentifier programCase)
            )
        )
    Just value -> pure value

runCompilerScaleCase :: CompilerScaleCase -> IO Text
runCompilerScaleCase programCase = do
  compiledProgram <- prepareValidCompilerScaleProgram programCase
  withCompilerStage EvaluationStage $ do
    let runtimeResult = evaluateCompiledProgram compiledProgram
    evaluate (forceRuntimeProgramOutputResult runtimeResult)
    case runtimeResult of
      Left diagnostic -> failBenchmarkDiagnostic diagnostic
      Right runtimeProgram ->
        pure (maybe "" renderRuntimeValue (runtimeProgramOutput runtimeProgram))

unsupportedCompilerScaleGroup :: BenchmarkGroup -> CompilerScaleCase -> IO value
unsupportedCompilerScaleGroup benchmarkGroup programCase =
  ioError
    ( userError
        ( "unsupported compiler scale benchmark group for "
            <> Text.unpack (compilerScaleCaseIdentifier programCase)
            <> ": "
            <> show benchmarkGroup
        )
    )

requireNoCompileErrors :: CompiledProgram -> IO ()
requireNoCompileErrors compiledProgram =
  case compiledProgramErrors compiledProgram of
    [] -> pure ()
    diagnostic : _ -> failBenchmarkDiagnostic diagnostic

requireExpectedRuntimeResult :: ProgramCase -> Either Diagnostic RuntimeProgram -> IO ()
requireExpectedRuntimeResult programCase runtimeResult =
  let actualTermination =
        case runtimeResult of
          Left _ -> RuntimeFailedProgram
          Right _ -> SuccessfulProgram
      actualStdout =
        case runtimeResult of
          Left _ -> ""
          Right runtimeProgram -> maybe "" ((<> "\n") . renderRuntimeValue) (runtimeProgramOutput runtimeProgram)
   in requireExpectedBehavior programCase actualTermination actualStdout

requireExpectedProgramResult :: ProgramCase -> ProgramCaseResult -> IO ()
requireExpectedProgramResult programCase result =
  requireExpectedBehavior
    programCase
    (programCaseResultTermination result)
    (programCaseResultStdout result)

requireExpectedBehavior :: ProgramCase -> ProgramTermination -> Text -> IO ()
requireExpectedBehavior programCase actualTermination actualStdout
  | actualTermination == programCaseExpectedTermination programCase,
    actualStdout == programCaseExpectedStdout programCase =
      pure ()
  | otherwise =
      ioError
        ( userError
            ( "benchmark case did not preserve expected behavior: "
                <> Text.unpack (programCaseIdentifier programCase)
            )
        )

failBenchmarkDiagnostic :: Diagnostic -> IO value
failBenchmarkDiagnostic diagnostic =
  ioError (userError (Text.unpack (renderDiagnostic diagnostic)))

selectProgramCases :: [Text] -> [ProgramCase] -> Either Text [ProgramCase]
selectProgramCases requestedIdentifiers programCases
  | null requestedIdentifiers = Right programCases
  | not (null missingIdentifiers) =
      Left ("unknown corpus case(s): " <> Text.intercalate ", " missingIdentifiers)
  | otherwise =
      Right
        ( filter
            ((`elem` requestedIdentifiers) . programCaseIdentifier)
            programCases
        )
  where
    knownIdentifiers = map programCaseIdentifier programCases
    missingIdentifiers = filter (`notElem` knownIdentifiers) requestedIdentifiers
