{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
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
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import Jazz.Benchmark.Force
  ( forceAnalyzedProgram,
    forceAnalyzedProgramResult,
    forceDiagnostic,
    forceListWith,
    forceLoweredExpr,
    forceProgramCaseResult,
    forceResolvedExpr,
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

data PreparedBenchmark
  = PreparedParseLower Text
  | PreparedAnalysis CompileInputs (CoreProgram 'Resolved)
  | PreparedModulePreparation ProgramCase
  | PreparedRuntime ExpectedProgramBehavior (CoreProgram 'Analyzed)
  | PreparedWholeProgram ProgramCase

data PreparedCompilerScaleBenchmark
  = PreparedCompilerScaleParseLower Text
  | PreparedCompilerScaleAnalysis CompileInputs (CoreProgram 'Resolved)
  | PreparedCompilerScaleModulePreparation CompilerScaleCase
  | PreparedCompilerScaleRuntime ExpectedCompilerScaleOutput (CoreProgram 'Analyzed)
  | PreparedCompilerScaleDiagnosticAnalysis (Expr 'Resolved) Int
  | PreparedCompilerScaleWholeProgram CompilerScaleCase

data ExpectedProgramBehavior = ExpectedProgramBehavior Text ProgramTermination Text
  deriving stock (Generic)
  deriving anyclass (NFData)

data ExpectedCompilerScaleOutput = ExpectedCompilerScaleOutput Text Text
  deriving stock (Generic)
  deriving anyclass (NFData)

instance NFData PreparedBenchmark where
  rnf preparedBenchmark =
    case preparedBenchmark of
      PreparedParseLower source -> Text.length source `seq` ()
      PreparedAnalysis inputs resolvedProgram ->
        inputs `seq`
          rnf resolvedProgram
      PreparedModulePreparation programCase -> rnf programCase
      PreparedRuntime expectedBehavior analyzedProgram ->
        rnf expectedBehavior `seq` forceAnalyzedProgram analyzedProgram
      PreparedWholeProgram programCase -> rnf programCase

instance NFData PreparedCompilerScaleBenchmark where
  rnf preparedBenchmark =
    case preparedBenchmark of
      PreparedCompilerScaleParseLower source -> Text.length source `seq` ()
      PreparedCompilerScaleAnalysis inputs resolvedProgram ->
        inputs `seq`
          rnf resolvedProgram
      PreparedCompilerScaleModulePreparation programCase -> rnf programCase
      PreparedCompilerScaleRuntime expectedOutput analyzedProgram ->
        rnf expectedOutput `seq` forceAnalyzedProgram analyzedProgram
      PreparedCompilerScaleDiagnosticAnalysis expression expectedDiagnosticCount ->
        forceResolvedExpr expression `seq` rnf expectedDiagnosticCount
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
      prepareFully (PreparedParseLower source)
    AnalysisBenchmark -> do
      (resolvedProgram, _) <- prepareValidProgram programCase
      let inputs = compileInputs defaultWarningSettings Set.empty
      prepareFully
        ( PreparedAnalysis
            inputs
            resolvedProgram
        )
    ModulePreparationBenchmark -> prepareFully (PreparedModulePreparation programCase)
    DiagnosticAnalysisBenchmark -> unsupportedCorpusGroup benchmarkGroup programCase
    RuntimeBenchmark -> do
      (_, analyzedProgram) <- prepareValidProgram programCase
      prepareFully
        ( PreparedRuntime
            (expectedProgramBehavior programCase)
            analyzedProgram
        )
    WholeProgramBenchmark -> prepareFully (PreparedWholeProgram programCase)

prepareCompilerScaleBenchmark :: BenchmarkGroup -> CompilerScaleCase -> IO PreparedCompilerScaleBenchmark
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
      prepareFully (PreparedCompilerScaleParseLower source)
    AnalysisBenchmark -> do
      (resolvedProgram, _) <- prepareValidCompilerScaleProgram programCase
      let inputs = compileInputs defaultWarningSettings Set.empty
      prepareFully
        ( PreparedCompilerScaleAnalysis
            inputs
            resolvedProgram
        )
    DiagnosticAnalysisBenchmark ->
      case diagnosticAnalysisInput (compilerScaleCaseScenario programCase) (compilerScaleCaseSize programCase) of
        Left message -> unsupportedCompilerScaleGroup benchmarkGroup programCase message
        Right (expression, expectedDiagnosticCount) ->
          prepareFully
            (PreparedCompilerScaleDiagnosticAnalysis expression expectedDiagnosticCount)
    ModulePreparationBenchmark -> prepareFully (PreparedCompilerScaleModulePreparation programCase)
    WholeProgramBenchmark -> prepareFully (PreparedCompilerScaleWholeProgram programCase)
    RuntimeBenchmark -> do
      (_, analyzedProgram) <- prepareValidCompilerScaleProgram programCase
      prepareFully
        ( PreparedCompilerScaleRuntime
            (expectedCompilerScaleOutput programCase)
            analyzedProgram
        )

runPreparedBenchmark :: PreparedBenchmark -> IO ()
runPreparedBenchmark preparedBenchmark =
  case preparedBenchmark of
    PreparedParseLower source -> runParseLower source
    PreparedAnalysis inputs resolvedProgram -> do
      analysisResult <-
        withCompilerStage TypeInferenceStage $ do
          value <- ModuleCompiler.analyzeProgram inputs resolvedProgram
          evaluate (forceAnalyzedProgramResult value)
          pure value
      requireSuccessfulAnalysis analysisResult
    PreparedModulePreparation programCase -> do
      analyzedResult <-
        withCompilerStage RuntimePreparationStage (prepareProgramCase programCase)
      evaluate (forcePreparedProgramResult analyzedResult)
      case analyzedResult of
        Left diagnostic -> failBenchmarkDiagnostic diagnostic
        Right (_, diagnostics, maybeAnalyzedProgram) -> requireSuccessfulAnalysis (diagnostics, maybeAnalyzedProgram)
    PreparedRuntime expectedBehavior analyzedProgram ->
      withCompilerStage EvaluationStage $ do
        let runtimeResult = evaluateAnalyzedProgram analyzedProgram
        evaluate (forceRuntimeProgramOutputResult runtimeResult)
        requireExpectedRuntimeResult expectedBehavior runtimeResult
    PreparedWholeProgram programCase -> do
      result <- runProgramCase programCase
      evaluate (forceProgramCaseResult result)
      requireExpectedProgramResult programCase result

runPreparedCompilerScaleBenchmark :: PreparedCompilerScaleBenchmark -> IO ()
runPreparedCompilerScaleBenchmark preparedBenchmark =
  case preparedBenchmark of
    PreparedCompilerScaleParseLower source -> runParseLower source
    PreparedCompilerScaleAnalysis inputs resolvedProgram -> do
      analysisResult <-
        withCompilerStage TypeInferenceStage $ do
          value <- ModuleCompiler.analyzeProgram inputs resolvedProgram
          evaluate (forceAnalyzedProgramResult value)
          pure value
      requireSuccessfulAnalysis analysisResult
    PreparedCompilerScaleModulePreparation programCase -> do
      analyzedResult <- buildCompilerScaleProgram programCase
      evaluate (forcePreparedProgramResult analyzedResult)
      case analyzedResult of
        Left diagnostic -> failBenchmarkDiagnostic diagnostic
        Right (_, diagnostics, maybeAnalyzedProgram) -> requireSuccessfulAnalysis (diagnostics, maybeAnalyzedProgram)
    PreparedCompilerScaleRuntime expectedOutput analyzedProgram ->
      withCompilerStage EvaluationStage $ do
        let runtimeResult = evaluateAnalyzedProgram analyzedProgram
        evaluate (forceRuntimeProgramOutputResult runtimeResult)
        requireExpectedCompilerScaleRuntimeResult expectedOutput runtimeResult
    PreparedCompilerScaleDiagnosticAnalysis expression expectedDiagnosticCount ->
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
