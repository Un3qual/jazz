{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}

module Jazz.Benchmark.StageInputs
  ( PreparedBenchmark,
    prepareBenchmark,
    prepareCompilerScaleBenchmark,
    runCompilerScaleCase,
    runPreparedBenchmark,
    selectProgramCases,
  )
where

import Control.DeepSeq (NFData (rnf))
import Control.Exception (evaluate)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import Jazz.Benchmark.Force
  ( forceAnalyzedProgramResult,
    forceDiagnostic,
    forceListWith,
    forceLoweredExpr,
    forceProgramCaseResult,
    forceRuntimeProgramOutputResult,
    forceSurfaceExpr,
    forceTokens,
  )
import Jazz.Benchmark.ScaleCases
  ( CompilerScaleCase,
    CompilerScaleScenario (..),
    compilerScaleCaseEntryModulePath,
    compilerScaleCaseEntrySource,
    compilerScaleCaseExpectedOutput,
    compilerScaleCaseIdentifier,
    compilerScaleCaseResolutionConfig,
    compilerScaleCaseScenario,
    compilerScaleCaseSize,
    compilerScaleCaseSource,
  )
import Jazz.Compiler.AST
  ( CoreNode (..),
    CoreNodeId (..),
    CorePhase (Analyzed, Resolved),
    Expr (..),
  )
import Jazz.Compiler.Analyzer
  ( AnalysisResult (..),
    analyzeProgram,
  )
import Jazz.Compiler.BundledPrelude (bundledPreludeSource)
import Jazz.Compiler.Diagnostics (Diagnostic, SourceSpan (..), isErrorDiagnostic)
import Jazz.Compiler.Diagnostics.Render (renderDiagnostic)
import Jazz.Compiler.Driver (ResolvedPrelude (PreludeBundled), buildAnalyzedProgram)
import qualified Jazz.Compiler.ModuleCompiler as ModuleCompiler
import Jazz.Compiler.ModuleGraph
  ( CoreProgram,
  )
import Jazz.Compiler.ModuleInterface (CompileInputs, compileInputs)
import Jazz.Compiler.ModuleRuntime
  ( RuntimeProgram (runtimeProgramOutput),
    evaluateAnalyzedProgram,
  )
import Jazz.Compiler.Name
  ( NameNamespace (ValueNamespace),
    mkIdentifier,
    resolvedLocalName,
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

-- | A prepared input travels with its matching runner and forcing dictionary.
-- Keeping the input separate prevents setup from executing the timed action.
data PreparedBenchmark = forall input. (NFData input) => PreparedBenchmark input (input -> IO ())

instance NFData PreparedBenchmark where
  rnf (PreparedBenchmark input run) = rnf input `seq` run `seq` ()

-- CompileInputs contains deliberately lazy environments. Force only its outer
-- constructor while fully preparing the resolved program.
data AnalysisInput = AnalysisInput CompileInputs (CoreProgram 'Resolved)

instance NFData AnalysisInput where
  rnf (AnalysisInput inputs program) = inputs `seq` rnf program

data ExpectedProgramBehavior = ExpectedProgramBehavior Text ProgramTermination Text
  deriving stock (Generic)
  deriving anyclass (NFData)

data ExpectedCompilerScaleOutput = ExpectedCompilerScaleOutput Text Text
  deriving stock (Generic)
  deriving anyclass (NFData)

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
      prepareFully (PreparedBenchmark source runParseLower)
    AnalysisBenchmark -> do
      (resolvedProgram, _) <- prepareValidProgram programCase
      let input = AnalysisInput (compileInputs defaultWarningSettings Set.empty) resolvedProgram
      prepareFully (PreparedBenchmark input runAnalysis)
    ModulePreparationBenchmark ->
      prepareFully (PreparedBenchmark programCase (runPreparedProgram . withCompilerStage RuntimePreparationStage . prepareProgramCase))
    DiagnosticAnalysisBenchmark -> unsupportedCorpusGroup benchmarkGroup programCase
    RuntimeBenchmark -> do
      (_, analyzedProgram) <- prepareValidProgram programCase
      prepareFully
        (PreparedBenchmark (expectedProgramBehavior programCase, analyzedProgram) (runRuntime requireExpectedRuntimeResult))
    WholeProgramBenchmark -> prepareFully (PreparedBenchmark programCase runWholeProgram)

prepareCompilerScaleBenchmark :: BenchmarkGroup -> CompilerScaleCase -> IO PreparedBenchmark
prepareCompilerScaleBenchmark benchmarkGroup programCase =
  case benchmarkGroup of
    ParseLowerBenchmark -> do
      source <-
        case compilerScaleCaseEntrySource programCase of
          Nothing ->
            ioError
              ( userError
                  ( "compiler scale case is missing its entry source: "
                      <> Text.unpack (compilerScaleCaseIdentifier programCase)
                  )
              )
          Just value -> evaluate (Text.length value) >> pure value
      prepareFully (PreparedBenchmark source runParseLower)
    AnalysisBenchmark -> do
      (resolvedProgram, _) <- prepareValidCompilerScaleProgram programCase
      let input = AnalysisInput (compileInputs defaultWarningSettings Set.empty) resolvedProgram
      prepareFully (PreparedBenchmark input runAnalysis)
    DiagnosticAnalysisBenchmark ->
      case diagnosticAnalysisInput (compilerScaleCaseScenario programCase) (compilerScaleCaseSize programCase) of
        Left message -> unsupportedCompilerScaleGroup benchmarkGroup programCase message
        Right input -> prepareFully (PreparedBenchmark input runDiagnosticAnalysis)
    ModulePreparationBenchmark ->
      prepareFully (PreparedBenchmark programCase (runPreparedProgram . buildCompilerScaleProgram))
    WholeProgramBenchmark -> prepareFully (PreparedBenchmark programCase runCompilerScaleWholeProgram)
    RuntimeBenchmark -> do
      (_, analyzedProgram) <- prepareValidCompilerScaleProgram programCase
      prepareFully
        (PreparedBenchmark (expectedCompilerScaleOutput programCase, analyzedProgram) (runRuntime requireExpectedCompilerScaleRuntimeResult))

runPreparedBenchmark :: PreparedBenchmark -> IO ()
runPreparedBenchmark (PreparedBenchmark input run) = run input

runAnalysis :: AnalysisInput -> IO ()
runAnalysis (AnalysisInput inputs resolvedProgram) = do
  analysisResult <-
    withCompilerStage TypeInferenceStage $ do
      value <- ModuleCompiler.analyzeProgram inputs resolvedProgram
      evaluate (forceAnalyzedProgramResult value)
      pure value
  requireSuccessfulAnalysis analysisResult

runPreparedProgram :: IO (Either Diagnostic (CoreProgram 'Resolved, [Diagnostic], Maybe (CoreProgram 'Analyzed))) -> IO ()
runPreparedProgram prepare = do
  analyzedResult <- prepare
  evaluate (forcePreparedProgramResult analyzedResult)
  case analyzedResult of
    Left diagnostic -> failBenchmarkDiagnostic diagnostic
    Right (_, diagnostics, maybeAnalyzedProgram) -> requireSuccessfulAnalysis (diagnostics, maybeAnalyzedProgram)

runRuntime :: (expected -> Either Diagnostic RuntimeProgram -> IO ()) -> (expected, CoreProgram 'Analyzed) -> IO ()
runRuntime requireExpected (expected, analyzedProgram) =
  withCompilerStage EvaluationStage $ do
    let runtimeResult = evaluateAnalyzedProgram analyzedProgram
    evaluate (forceRuntimeProgramOutputResult runtimeResult)
    requireExpected expected runtimeResult

runWholeProgram :: ProgramCase -> IO ()
runWholeProgram programCase = do
  result <- runProgramCase programCase
  evaluate (forceProgramCaseResult result)
  requireExpectedProgramResult programCase result

runDiagnosticAnalysis :: (Expr 'Resolved, Int) -> IO ()
runDiagnosticAnalysis (expression, expectedDiagnosticCount) =
  withCompilerStage StaticAnalysisStage $ do
    analysisResult <- analyzeProgram defaultWarningSettings expression
    evaluate (forceListWith forceDiagnostic (analysisDiagnostics analysisResult))
    let actualDiagnosticCount = length (filter isErrorDiagnostic (analysisDiagnostics analysisResult))
    if actualDiagnosticCount == expectedDiagnosticCount
      then pure ()
      else
        ioError
          ( userError
              ( "analyzer diagnostic benchmark produced "
                  <> show actualDiagnosticCount
                  <> " errors; expected "
                  <> show expectedDiagnosticCount
              )
          )

runCompilerScaleWholeProgram :: CompilerScaleCase -> IO ()
runCompilerScaleWholeProgram programCase = do
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
    evaluate (forceLoweredExpr expression)

analyzerDiagnosticChainExpression :: Int -> Expr 'Resolved
analyzerDiagnosticChainExpression expressionCount =
  case expressionCount of
    count
      | count <= 0 -> EBlock (diagnosticNode 0) []
      | otherwise ->
          foldl'
            (\left index -> EApply (diagnosticNode (count - 1 - index)) left (missingVariable (count - 1 + index) index))
            (missingVariable (count - 1) 0)
            [1 .. count - 1]
  where
    missingVariable :: Int -> Int -> Expr 'Resolved
    missingVariable nodeId index =
      EVar
        (diagnosticNode nodeId)
        (resolvedLocalName ValueNamespace (mkIdentifier ("missing" <> Text.pack (show index))))

    diagnosticNode :: Int -> CoreNode 'Resolved sort
    diagnosticNode nodeId = CoreNode (CoreNodeId nodeId) (SourceSpan 1 1) ()

diagnosticAnalysisInput :: CompilerScaleScenario -> Int -> Either Text (Expr 'Resolved, Int)
diagnosticAnalysisInput scenario size =
  case scenario of
    AnalyzerDiagnosticChain -> Right (analyzerDiagnosticChainExpression size, size)
    SequentialPolymorphicBindings -> unsupported
    WideModuleFanout -> unsupported
    SharedInterfaceFanout -> unsupported
    NestedRuntimeApplications -> unsupported
    RuntimeImportWidth -> unsupported
    ResolverFactRich -> unsupported
    WideConstructorApplication -> unsupported
    CapabilityCandidateWidth -> unsupported
    HostFreeOpaqueEnvironment -> unsupported
    InterleavedRecursiveGroups -> unsupported
    RecursivePreviewBursts -> unsupported
    RecursiveRebindings -> unsupported
    ConstrainedSignatures -> unsupported
    DeferredConstraintBursts -> unsupported
    DeepNestedLambdas -> unsupported
    LargeOperatorTables -> unsupported
    NestedBlocks -> unsupported
    AmbiguousCaseArmPipes -> unsupported
    LongTokenStream -> unsupported
    IdentifierTokenStream -> unsupported
    LiteralTokenStream -> unsupported
  where
    unsupported = Left "scenario has no direct analyzer-diagnostic artifact"

prepareValidProgram :: ProgramCase -> IO (CoreProgram 'Resolved, CoreProgram 'Analyzed)
prepareValidProgram programCase = do
  analyzedResult <- prepareProgramCase programCase
  evaluate (forcePreparedProgramResult analyzedResult)
  case analyzedResult of
    Left diagnostic -> failBenchmarkDiagnostic diagnostic
    Right (resolvedProgram, diagnostics, maybeAnalyzedProgram) -> do
      requireSuccessfulAnalysis (diagnostics, maybeAnalyzedProgram)
      case maybeAnalyzedProgram of
        Just analyzedProgram -> pure (resolvedProgram, analyzedProgram)
        Nothing -> ioError (userError "successful analysis did not produce analyzed core")

buildCompilerScaleProgram :: CompilerScaleCase -> IO (Either Diagnostic (CoreProgram 'Resolved, [Diagnostic], Maybe (CoreProgram 'Analyzed)))
buildCompilerScaleProgram programCase =
  buildAnalyzedProgram
    defaultWarningSettings
    (PreludeBundled bundledPreludeSource)
    (compilerScaleCaseResolutionConfig programCase)
    (compilerScaleCaseEntryModulePath programCase)
    (pure . compilerScaleCaseSource programCase)

prepareValidCompilerScaleProgram :: CompilerScaleCase -> IO (CoreProgram 'Resolved, CoreProgram 'Analyzed)
prepareValidCompilerScaleProgram programCase = do
  analyzedResult <- buildCompilerScaleProgram programCase
  evaluate (forcePreparedProgramResult analyzedResult)
  case analyzedResult of
    Left diagnostic -> failBenchmarkDiagnostic diagnostic
    Right (resolvedProgram, diagnostics, maybeAnalyzedProgram) -> do
      requireSuccessfulAnalysis (diagnostics, maybeAnalyzedProgram)
      case maybeAnalyzedProgram of
        Just analyzedProgram -> pure (resolvedProgram, analyzedProgram)
        Nothing -> ioError (userError "successful analysis did not produce analyzed core")

runCompilerScaleCase :: CompilerScaleCase -> IO Text
runCompilerScaleCase programCase = do
  (_, analyzedProgram) <- prepareValidCompilerScaleProgram programCase
  withCompilerStage EvaluationStage $ do
    let runtimeResult = evaluateAnalyzedProgram analyzedProgram
    evaluate (forceRuntimeProgramOutputResult runtimeResult)
    case runtimeResult of
      Left diagnostic -> failBenchmarkDiagnostic diagnostic
      Right runtimeProgram ->
        pure (maybe "" renderRuntimeValue (runtimeProgramOutput runtimeProgram))

unsupportedCorpusGroup :: BenchmarkGroup -> ProgramCase -> IO value
unsupportedCorpusGroup benchmarkGroup programCase =
  ioError
    ( userError
        ( "unsupported corpus benchmark group for "
            <> Text.unpack (programCaseIdentifier programCase)
            <> ": "
            <> show benchmarkGroup
        )
    )

unsupportedCompilerScaleGroup :: BenchmarkGroup -> CompilerScaleCase -> Text -> IO value
unsupportedCompilerScaleGroup benchmarkGroup programCase reason =
  ioError
    ( userError
        ( "unsupported compiler scale benchmark group for "
            <> Text.unpack (compilerScaleCaseIdentifier programCase)
            <> ": "
            <> show benchmarkGroup
            <> " ("
            <> Text.unpack reason
            <> ")"
        )
    )

prepareFully :: (NFData prepared) => prepared -> IO prepared
prepareFully prepared = evaluate (rnf prepared) >> pure prepared

expectedProgramBehavior :: ProgramCase -> ExpectedProgramBehavior
expectedProgramBehavior programCase =
  ExpectedProgramBehavior
    (programCaseIdentifier programCase)
    (programCaseExpectedTermination programCase)
    (programCaseExpectedStdout programCase)

expectedCompilerScaleOutput :: CompilerScaleCase -> ExpectedCompilerScaleOutput
expectedCompilerScaleOutput programCase =
  ExpectedCompilerScaleOutput
    (compilerScaleCaseIdentifier programCase)
    (compilerScaleCaseExpectedOutput programCase)

forcePreparedProgramResult :: Either Diagnostic (CoreProgram 'Resolved, [Diagnostic], Maybe (CoreProgram 'Analyzed)) -> ()
forcePreparedProgramResult result =
  case result of
    Left diagnostic -> forceDiagnostic diagnostic
    Right (resolvedProgram, diagnostics, maybeAnalyzedProgram) ->
      rnf resolvedProgram `seq` forceAnalyzedProgramResult (diagnostics, maybeAnalyzedProgram)

requireSuccessfulAnalysis :: ([Diagnostic], Maybe (CoreProgram 'Analyzed)) -> IO ()
requireSuccessfulAnalysis (diagnostics, maybeAnalyzedProgram) =
  case filter isErrorDiagnostic diagnostics of
    diagnostic : _ -> failBenchmarkDiagnostic diagnostic
    [] ->
      case maybeAnalyzedProgram of
        Just _ -> pure ()
        Nothing -> ioError (userError "analysis produced no errors and no analyzed core")

requireExpectedRuntimeResult :: ExpectedProgramBehavior -> Either Diagnostic RuntimeProgram -> IO ()
requireExpectedRuntimeResult (ExpectedProgramBehavior identifier expectedTermination expectedStdout) runtimeResult =
  let actualTermination =
        case runtimeResult of
          Left _ -> RuntimeFailedProgram
          Right _ -> SuccessfulProgram
      actualStdout =
        case runtimeResult of
          Left _ -> ""
          Right runtimeProgram -> maybe "" ((<> "\n") . renderRuntimeValue) (runtimeProgramOutput runtimeProgram)
   in requireExpectedBehavior identifier expectedTermination expectedStdout actualTermination actualStdout

requireExpectedCompilerScaleRuntimeResult :: ExpectedCompilerScaleOutput -> Either Diagnostic RuntimeProgram -> IO ()
requireExpectedCompilerScaleRuntimeResult (ExpectedCompilerScaleOutput identifier expectedOutput) runtimeResult =
  case runtimeResult of
    Left diagnostic -> failBenchmarkDiagnostic diagnostic
    Right runtimeProgram ->
      let actualOutput = maybe "" renderRuntimeValue (runtimeProgramOutput runtimeProgram)
       in if actualOutput == expectedOutput
            then pure ()
            else
              ioError
                ( userError
                    ( "compiler scale runtime benchmark did not preserve expected output: "
                        <> Text.unpack identifier
                    )
                )

requireExpectedProgramResult :: ProgramCase -> ProgramCaseResult -> IO ()
requireExpectedProgramResult programCase result =
  requireExpectedBehavior
    (programCaseIdentifier programCase)
    (programCaseExpectedTermination programCase)
    (programCaseExpectedStdout programCase)
    (programCaseResultTermination result)
    (programCaseResultStdout result)

requireExpectedBehavior :: Text -> ProgramTermination -> Text -> ProgramTermination -> Text -> IO ()
requireExpectedBehavior identifier expectedTermination expectedStdout actualTermination actualStdout
  | actualTermination == expectedTermination,
    actualStdout == expectedStdout =
      pure ()
  | otherwise =
      ioError
        ( userError
            ( "benchmark case did not preserve expected behavior: "
                <> Text.unpack identifier
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
