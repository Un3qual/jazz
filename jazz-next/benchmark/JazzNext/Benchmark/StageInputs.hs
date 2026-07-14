{-# LANGUAGE OverloadedStrings #-}

module JazzNext.Benchmark.StageInputs
  ( PreparedBenchmark,
    prepareBenchmark,
    runPreparedBenchmark,
    selectProgramCases,
  )
where

import Control.DeepSeq (NFData (rnf))
import Control.Exception (evaluate)
import Data.Text (Text)
import qualified Data.Text as Text
import JazzNext.Benchmark.Force
  ( forceCompiledModule,
    forceCompiledModules,
    forceCompiledProgram,
    forceCompiledProgramResult,
    forceExpr,
    forceProgramCaseResult,
    forceRuntimeProgramResult,
    forceSurfaceExpr,
    forceTokens,
  )
import JazzNext.Compiler.Diagnostics (Diagnostic, renderDiagnostic)
import JazzNext.Compiler.ModuleCompiler (compileResolvedModule)
import JazzNext.Compiler.ModuleGraph (ResolvedModule (..))
import JazzNext.Compiler.ModuleInterface
  ( CompileInputs,
    CompiledModule (..),
    CompiledProgram (..),
    compileInputs,
    lookupCompiledModule,
  )
import JazzNext.Compiler.ModuleRuntime
  ( RuntimeProgram (runtimeProgramOutput),
    evaluateCompiledProgram,
  )
import JazzNext.Compiler.Parser (parseSurfaceProgramTokens)
import JazzNext.Compiler.Parser.Lexer (tokenize)
import JazzNext.Compiler.Parser.Lower (lowerSurfaceExpr)
import JazzNext.Compiler.Profiling
  ( BenchmarkGroup (..),
    CompilerStage (..),
    withCompilerStage,
  )
import JazzNext.Compiler.Runtime (renderRuntimeValue)
import JazzNext.Compiler.WarningConfig (defaultWarningSettings)
import JazzNext.ProgramCorpus.Runner
  ( ProgramCaseResult (..),
    loadProgramCaseEntrySource,
    prepareProgramCase,
    runProgramCase,
  )
import JazzNext.ProgramCorpus.Types
  ( ProgramCase (..),
    ProgramTermination (..),
  )

data PreparedBenchmark
  = PreparedParseLower ProgramCase Text
  | PreparedAnalysis ProgramCase CompiledProgram CompileInputs [CompiledModule] ResolvedModule
  | PreparedModulePreparation ProgramCase
  | PreparedRuntime ProgramCase CompiledProgram
  | PreparedWholeProgram ProgramCase

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

prepareBenchmark :: BenchmarkGroup -> ProgramCase -> IO PreparedBenchmark
prepareBenchmark benchmarkGroup programCase =
  case benchmarkGroup of
    ParseLowerBenchmark -> do
      source <-
        withCompilerStage SourceLoadingStage $ do
          loadedSource <- loadProgramCaseEntrySource programCase
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
        evaluate (forceRuntimeProgramResult runtimeResult)
        requireExpectedRuntimeResult programCase runtimeResult
    PreparedWholeProgram programCase -> do
      result <- runProgramCase programCase
      evaluate (forceProgramCaseResult result)
      requireExpectedProgramResult programCase result

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
