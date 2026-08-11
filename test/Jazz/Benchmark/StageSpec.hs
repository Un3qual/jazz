{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Exception (IOException, try)
import Control.Monad (void)
import Data.Text (Text)
import qualified Data.Text as Text
import Jazz.Benchmark.ScaleCases
  ( CompilerScaleCase,
    CompilerScaleScenario (..),
    compilerScaleCaseBenchmarks,
    compilerScaleCaseEntrySource,
    compilerScaleCaseIdentifier,
    compilerScaleCaseInterfaceWidth,
    compilerScaleCaseScenario,
    compilerScaleCaseSize,
    compilerScaleCaseSourceCount,
    compilerScaleCases,
    selectCompilerScaleCases,
  )
import Jazz.Benchmark.StageInputs
  ( prepareBenchmark,
    prepareCompilerScaleBenchmark,
    runCompilerScaleCase,
    runPreparedBenchmark,
    runPreparedCompilerScaleBenchmark,
    selectProgramCases,
  )
import Jazz.Benchmark.Stages
  ( BenchmarkCommand (benchmarkCommandSelectedCases, benchmarkCommandSelectedScaleCases),
    parseBenchmarkCommand,
  )
import Jazz.Compiler.Parser.Lexer (tokenize)
import Jazz.Compiler.Profiling (BenchmarkGroup (..))
import Jazz.ProgramCorpus.Manifest (loadProgramCorpus, programCaseById, renderProgramCorpusViolation)
import Jazz.ProgramCorpus.Types (ProgramCase (..), ProgramCorpus (..))
import Jazz.TestHarness (NamedTest, assertEqual, failTest, runTestSuite)

main :: IO ()
main = runTestSuite "BenchmarkStages" tests

tests :: [NamedTest]
tests =
  [ ("registers stable generated scale identities and sizes", testCompilerScaleRegistry),
    ("selects generated scale cases before stage preparation", testCompilerScaleSelection),
    ("parses opt-in generated scale case selectors", testCompilerScaleCommandSelection),
    ("rejects ambiguous generated scale case selectors", testCompilerScaleCommandRejection),
    ("sequential polymorphism preserves exact compiler semantics", testSequentialPolymorphismSemantics),
    ("wide module fanout preserves exact compiler semantics", testWideModuleFanoutSemantics),
    ("resolver fact-rich modules preserve exact compiler semantics", testResolverFactRichSemantics),
    ("typed validation handoff lowers exact valid programs", testTypedValidationHandoffSemantics),
    ("interleaved recursive groups preserve exact compiler semantics", testInterleavedRecursiveGroupSemantics),
    ("recursive preview bursts preserve exact compiler semantics", testRecursivePreviewBurstSemantics),
    ("same-name rebinding bursts preserve exact compiler semantics", testRecursiveRebindingBurstSemantics),
    ("constrained signatures preserve exact compiler semantics", testConstrainedSignatureSemantics),
    ("deferred constraint bursts preserve exact compiler semantics", testDeferredConstraintBurstSemantics),
    ("deep nested lambdas preserve exact compiler semantics", testDeepNestedLambdaSemantics),
    ("large operator tables exercise parse and lower", testLargeOperatorTableParseLower),
    ("nested blocks exercise parse and lower", testNestedBlocksParseLower),
    ("long token streams have exact token counts", testLongTokenStreamExactSize),
    ("selects requested cases before stage preparation", testCaseSelection),
    ("parse-lower setup does not require module compilation", testParseLowerSetupBoundary),
    ("parse-lower setup reports entry-source read failures", testParseLowerSourceReadFailure),
    ("analysis uses module-aware imported interfaces", testModuleAwareAnalysis),
    ("runtime benchmarks reject unexpected results", testRuntimeResultValidation)
  ]

testCompilerScaleRegistry :: IO ()
testCompilerScaleRegistry =
  assertEqual
    "generated scale registry"
    [ ("sequential-polymorphic-bindings-0064", SequentialPolymorphicBindings, 64, Nothing),
      ("sequential-polymorphic-bindings-0128", SequentialPolymorphicBindings, 128, Nothing),
      ("sequential-polymorphic-bindings-0256", SequentialPolymorphicBindings, 256, Nothing),
      ("sequential-polymorphic-bindings-0512", SequentialPolymorphicBindings, 512, Nothing),
      ("wide-module-fanout-0008x0016", WideModuleFanout, 8, Just 16),
      ("wide-module-fanout-0016x0016", WideModuleFanout, 16, Just 16),
      ("wide-module-fanout-0032x0016", WideModuleFanout, 32, Just 16),
      ("wide-module-fanout-0064x0016", WideModuleFanout, 64, Just 16),
      ("wide-module-fanout-0064x0001", WideModuleFanout, 64, Just 1),
      ("wide-module-fanout-0128x0001", WideModuleFanout, 128, Just 1),
      ("wide-module-fanout-0256x0001", WideModuleFanout, 256, Just 1),
      ("wide-module-fanout-0512x0001", WideModuleFanout, 512, Just 1),
      ("shared-interface-fanout-0016x0016", SharedInterfaceFanout, 16, Just 16),
      ("shared-interface-fanout-0032x0016", SharedInterfaceFanout, 32, Just 16),
      ("shared-interface-fanout-0064x0016", SharedInterfaceFanout, 64, Just 16),
      ("shared-interface-fanout-0128x0016", SharedInterfaceFanout, 128, Just 16),
      ("resolver-fact-rich-0016", ResolverFactRich, 16, Nothing),
      ("resolver-fact-rich-0032", ResolverFactRich, 32, Nothing),
      ("resolver-fact-rich-0064", ResolverFactRich, 64, Nothing),
      ("resolver-fact-rich-0128", ResolverFactRich, 128, Nothing),
      ("typed-validation-handoff-0064", TypedValidationHandoff, 64, Nothing),
      ("typed-validation-handoff-0128", TypedValidationHandoff, 128, Nothing),
      ("typed-validation-handoff-0256", TypedValidationHandoff, 256, Nothing),
      ("typed-validation-handoff-0512", TypedValidationHandoff, 512, Nothing),
      ("interleaved-recursive-groups-0016", InterleavedRecursiveGroups, 16, Nothing),
      ("interleaved-recursive-groups-0032", InterleavedRecursiveGroups, 32, Nothing),
      ("interleaved-recursive-groups-0064", InterleavedRecursiveGroups, 64, Nothing),
      ("interleaved-recursive-groups-0128", InterleavedRecursiveGroups, 128, Nothing),
      ("recursive-preview-burst-0016", InterleavedRecursiveGroups, 16, Nothing),
      ("recursive-preview-burst-0032", InterleavedRecursiveGroups, 32, Nothing),
      ("recursive-preview-burst-0064", InterleavedRecursiveGroups, 64, Nothing),
      ("recursive-preview-burst-0128", InterleavedRecursiveGroups, 128, Nothing),
      ("recursive-rebinding-burst-0128", RecursiveRebindings, 128, Nothing),
      ("recursive-rebinding-burst-0256", RecursiveRebindings, 256, Nothing),
      ("recursive-rebinding-burst-0512", RecursiveRebindings, 512, Nothing),
      ("recursive-rebinding-burst-1024", RecursiveRebindings, 1024, Nothing),
      ("constrained-signatures-0032", ConstrainedSignatures, 32, Nothing),
      ("constrained-signatures-0064", ConstrainedSignatures, 64, Nothing),
      ("constrained-signatures-0128", ConstrainedSignatures, 128, Nothing),
      ("constrained-signatures-0256", ConstrainedSignatures, 256, Nothing),
      ("deferred-constraint-burst-0128", ConstrainedSignatures, 128, Nothing),
      ("deferred-constraint-burst-0256", ConstrainedSignatures, 256, Nothing),
      ("deferred-constraint-burst-0512", ConstrainedSignatures, 512, Nothing),
      ("deferred-constraint-burst-1024", ConstrainedSignatures, 1024, Nothing),
      ("deep-nested-lambdas-0016", DeepNestedLambdas, 16, Nothing),
      ("deep-nested-lambdas-0032", DeepNestedLambdas, 32, Nothing),
      ("deep-nested-lambdas-0064", DeepNestedLambdas, 64, Nothing),
      ("deep-nested-lambdas-0128", DeepNestedLambdas, 128, Nothing),
      ("large-operator-tables-0016", LargeOperatorTables, 16, Nothing),
      ("large-operator-tables-0032", LargeOperatorTables, 32, Nothing),
      ("large-operator-tables-0064", LargeOperatorTables, 64, Nothing),
      ("large-operator-tables-0128", LargeOperatorTables, 128, Nothing),
      ("nested-blocks-0016", NestedBlocks, 16, Nothing),
      ("nested-blocks-0032", NestedBlocks, 32, Nothing),
      ("nested-blocks-0064", NestedBlocks, 64, Nothing),
      ("nested-blocks-0128", NestedBlocks, 128, Nothing),
      ("long-token-stream-01024", LongTokenStream, 1024, Nothing),
      ("long-token-stream-04096", LongTokenStream, 4096, Nothing),
      ("long-token-stream-16384", LongTokenStream, 16384, Nothing),
      ("long-token-stream-65536", LongTokenStream, 65536, Nothing)
    ]
    ( map
        ( \programCase ->
            ( compilerScaleCaseIdentifier programCase,
              compilerScaleCaseScenario programCase,
              compilerScaleCaseSize programCase,
              compilerScaleCaseInterfaceWidth programCase
            )
        )
        compilerScaleCases
    )

testCompilerScaleSelection :: IO ()
testCompilerScaleSelection = do
  selected <-
    case selectCompilerScaleCases ["wide-module-fanout-0016x0016"] compilerScaleCases of
      Left message -> failTest message
      Right value -> pure value
  assertEqual
    "selected generated scale identifiers"
    ["wide-module-fanout-0016x0016"]
    (map compilerScaleCaseIdentifier selected)
  case selectCompilerScaleCases ["missing-scale-case"] compilerScaleCases of
    Left message
      | "missing-scale-case" `Text.isInfixOf` message -> pure ()
    other -> failTest ("expected unknown generated scale case rejection, got " <> Text.pack (show other))

testCompilerScaleCommandSelection :: IO ()
testCompilerScaleCommandSelection =
  case parseBenchmarkCommand
    [ "--jazz-scale-case=sequential-polymorphic-bindings-0064",
      "--jazz-scale-case",
      "wide-module-fanout-0008x0016"
    ] of
    Left message -> failTest message
    Right benchmarkCommand -> do
      assertEqual
        "selected generated scale identifiers"
        ["sequential-polymorphic-bindings-0064", "wide-module-fanout-0008x0016"]
        (benchmarkCommandSelectedScaleCases benchmarkCommand)
      assertEqual
        "generated selector leaves corpus selection empty"
        []
        (benchmarkCommandSelectedCases benchmarkCommand)

testCompilerScaleCommandRejection :: IO ()
testCompilerScaleCommandRejection = do
  assertCommandError
    "missing generated selector value"
    "--jazz-scale-case requires a value"
    ["--jazz-scale-case"]
  assertCommandError
    "duplicate generated selector"
    "duplicate --jazz-scale-case: sequential-polymorphic-bindings-0064"
    [ "--jazz-scale-case=sequential-polymorphic-bindings-0064",
      "--jazz-scale-case=sequential-polymorphic-bindings-0064"
    ]
  assertCommandError
    "mixed corpus and generated selectors"
    "--jazz-case and --jazz-scale-case cannot be combined"
    ["--jazz-case=identifier-classifier", "--jazz-scale-case=sequential-polymorphic-bindings-0064"]
  assertCommandError
    "smoke mode with generated selectors"
    "--jazz-smoke cannot select compiler scale cases"
    ["--jazz-smoke", "--jazz-scale-case=sequential-polymorphic-bindings-0064"]

assertCommandError :: Text -> Text -> [String] -> IO ()
assertCommandError label expected arguments =
  case parseBenchmarkCommand arguments of
    Left message -> assertEqual label expected message
    Right _ -> failTest ("expected benchmark command rejection: " <> label)

testSequentialPolymorphismSemantics :: IO ()
testSequentialPolymorphismSemantics = do
  programCase <- loadCompilerScaleCase "sequential-polymorphic-bindings-0064"
  actualOutput <- runCompilerScaleCase programCase
  assertEqual "sequential polymorphism output" "(42, True)" actualOutput
  prepared <- prepareCompilerScaleBenchmark AnalysisBenchmark programCase
  runPreparedCompilerScaleBenchmark prepared

testWideModuleFanoutSemantics :: IO ()
testWideModuleFanoutSemantics = do
  programCase <- loadCompilerScaleCase "wide-module-fanout-0008x0016"
  assertEqual "wide fanout virtual source count" 9 (compilerScaleCaseSourceCount programCase)
  actualOutput <- runCompilerScaleCase programCase
  assertEqual "wide fanout output" "0" actualOutput
  prepared <- prepareCompilerScaleBenchmark ModulePreparationBenchmark programCase
  runPreparedCompilerScaleBenchmark prepared
  lookupCase <- loadCompilerScaleCase "wide-module-fanout-0064x0001"
  assertEqual "lookup fanout virtual source count" 65 (compilerScaleCaseSourceCount lookupCase)
  lookupOutput <- runCompilerScaleCase lookupCase
  assertEqual "lookup fanout output" "0" lookupOutput
  lookupPrepared <- prepareCompilerScaleBenchmark ModulePreparationBenchmark lookupCase
  runPreparedCompilerScaleBenchmark lookupPrepared
  sharedCase <- loadCompilerScaleCase "shared-interface-fanout-0016x0016"
  assertEqual "shared-interface fanout virtual source count" 18 (compilerScaleCaseSourceCount sharedCase)
  sharedOutput <- runCompilerScaleCase sharedCase
  assertEqual "shared-interface fanout output" "0" sharedOutput
  sharedPrepared <- prepareCompilerScaleBenchmark ModulePreparationBenchmark sharedCase
  runPreparedCompilerScaleBenchmark sharedPrepared

testResolverFactRichSemantics :: IO ()
testResolverFactRichSemantics = do
  programCase <- loadCompilerScaleCase "resolver-fact-rich-0016"
  assertEqual "resolver fact-rich virtual source count" 3 (compilerScaleCaseSourceCount programCase)
  actualOutput <- runCompilerScaleCase programCase
  assertEqual "resolver fact-rich output" "Token" actualOutput
  prepared <- prepareCompilerScaleBenchmark ModulePreparationBenchmark programCase
  runPreparedCompilerScaleBenchmark prepared

testTypedValidationHandoffSemantics :: IO ()
testTypedValidationHandoffSemantics = do
  programCase <- loadCompilerScaleCase "typed-validation-handoff-0064"
  prepared <- prepareCompilerScaleBenchmark TypedLoweringBenchmark programCase
  runPreparedCompilerScaleBenchmark prepared

testInterleavedRecursiveGroupSemantics :: IO ()
testInterleavedRecursiveGroupSemantics = do
  programCase <- loadCompilerScaleCase "interleaved-recursive-groups-0016"
  actualOutput <- runCompilerScaleCase programCase
  assertEqual "interleaved recursive group output" "(1, True)" actualOutput
  prepared <- prepareCompilerScaleBenchmark AnalysisBenchmark programCase
  runPreparedCompilerScaleBenchmark prepared

testRecursivePreviewBurstSemantics :: IO ()
testRecursivePreviewBurstSemantics = do
  programCase <- loadCompilerScaleCase "recursive-preview-burst-0016"
  actualOutput <- runCompilerScaleCase programCase
  assertEqual "recursive preview burst output" "(1, True)" actualOutput
  prepared <- prepareCompilerScaleBenchmark AnalysisBenchmark programCase
  runPreparedCompilerScaleBenchmark prepared

testRecursiveRebindingBurstSemantics :: IO ()
testRecursiveRebindingBurstSemantics = do
  programCase <- loadCompilerScaleCase "recursive-rebinding-burst-0128"
  actualOutput <- runCompilerScaleCase programCase
  assertEqual "same-name rebinding burst output" "127" actualOutput
  prepared <- prepareCompilerScaleBenchmark AnalysisBenchmark programCase
  runPreparedCompilerScaleBenchmark prepared

testConstrainedSignatureSemantics :: IO ()
testConstrainedSignatureSemantics = do
  programCase <- loadCompilerScaleCase "constrained-signatures-0032"
  assertEqual
    "constrained signature benchmark boundaries"
    [ParseLowerBenchmark, AnalysisBenchmark]
    (compilerScaleCaseBenchmarks programCase)
  actualOutput <- runCompilerScaleCase programCase
  assertEqual "constrained signature output" "(1, True)" actualOutput
  preparedParseLower <- prepareCompilerScaleBenchmark ParseLowerBenchmark programCase
  runPreparedCompilerScaleBenchmark preparedParseLower
  prepared <- prepareCompilerScaleBenchmark AnalysisBenchmark programCase
  runPreparedCompilerScaleBenchmark prepared

testDeferredConstraintBurstSemantics :: IO ()
testDeferredConstraintBurstSemantics = do
  programCase <- loadCompilerScaleCase "deferred-constraint-burst-0128"
  actualOutput <- runCompilerScaleCase programCase
  assertEqual
    "deferred constraint burst output"
    ("[" <> Text.intercalate ", " (replicate 128 "1") <> "]")
    actualOutput
  prepared <- prepareCompilerScaleBenchmark AnalysisBenchmark programCase
  runPreparedCompilerScaleBenchmark prepared

testDeepNestedLambdaSemantics :: IO ()
testDeepNestedLambdaSemantics = do
  programCase <- loadCompilerScaleCase "deep-nested-lambdas-0016"
  assertEqual
    "deep nested lambda benchmark boundaries"
    [AnalysisBenchmark, ModulePreparationBenchmark, WholeProgramBenchmark]
    (compilerScaleCaseBenchmarks programCase)
  actualOutput <- runCompilerScaleCase programCase
  assertEqual "deep nested lambda output" "(1, 16)" actualOutput
  prepared <- prepareCompilerScaleBenchmark ModulePreparationBenchmark programCase
  runPreparedCompilerScaleBenchmark prepared
  preparedWholeProgram <- prepareCompilerScaleBenchmark WholeProgramBenchmark programCase
  runPreparedCompilerScaleBenchmark preparedWholeProgram

testLargeOperatorTableParseLower :: IO ()
testLargeOperatorTableParseLower = do
  programCase <- loadCompilerScaleCase "large-operator-tables-0016"
  prepared <- prepareCompilerScaleBenchmark ParseLowerBenchmark programCase
  runPreparedCompilerScaleBenchmark prepared

testNestedBlocksParseLower :: IO ()
testNestedBlocksParseLower = do
  programCase <- loadCompilerScaleCase "nested-blocks-0016"
  prepared <- prepareCompilerScaleBenchmark ParseLowerBenchmark programCase
  runPreparedCompilerScaleBenchmark prepared

testLongTokenStreamExactSize :: IO ()
testLongTokenStreamExactSize = do
  programCase <- loadCompilerScaleCase "long-token-stream-01024"
  source <-
    case compilerScaleCaseEntrySource programCase of
      Nothing -> failTest "long token stream scale case is missing its entry source"
      Just value -> pure value
  tokens <-
    case tokenize source of
      Left diagnostic -> failTest ("long token stream did not tokenize: " <> Text.pack (show diagnostic))
      Right values -> pure values
  assertEqual "long token stream token count" 1024 (length tokens)
  prepared <- prepareCompilerScaleBenchmark ParseLowerBenchmark programCase
  runPreparedCompilerScaleBenchmark prepared

testCaseSelection :: IO ()
testCaseSelection = do
  corpus <- loadCheckedInCorpus
  selected <-
    case selectProgramCases ["identifier-classifier"] (programCorpusCases corpus) of
      Left message -> failTest message
      Right value -> pure value
  assertEqual "selected identifiers" ["identifier-classifier"] (map programCaseIdentifier selected)
  case selectProgramCases ["missing-case"] (programCorpusCases corpus) of
    Left message
      | "missing-case" `Text.isInfixOf` message -> pure ()
    other -> failTest ("expected unknown case rejection, got " <> Text.pack (show other))

testParseLowerSetupBoundary :: IO ()
testParseLowerSetupBoundary = do
  programCase <- loadCase "identifier-classifier"
  prepared <-
    prepareBenchmark
      ParseLowerBenchmark
      programCase {programCaseModuleRoot = "/path/that/cannot-resolve-modules"}
  runPreparedBenchmark prepared

testParseLowerSourceReadFailure :: IO ()
testParseLowerSourceReadFailure = do
  programCase <- loadCase "identifier-classifier"
  result <-
    try
      ( void
          ( prepareBenchmark
              ParseLowerBenchmark
              programCase {programCaseEntrySource = programCaseEntrySource programCase <> ".missing"}
          )
      ) ::
      IO (Either IOException ())
  case result of
    Left exception
      | "could not read corpus entry source for case 'identifier-classifier'"
          `Text.isInfixOf` Text.pack (show exception) ->
          pure ()
    Left exception ->
      failTest ("expected a structured entry-source read failure, got " <> Text.pack (show exception))
    Right () -> failTest "expected parse-lower setup to reject an unreadable entry source"

testModuleAwareAnalysis :: IO ()
testModuleAwareAnalysis = do
  programCase <- loadCase "mini-frontend"
  prepared <- prepareBenchmark AnalysisBenchmark programCase
  runPreparedBenchmark prepared

testRuntimeResultValidation :: IO ()
testRuntimeResultValidation = do
  programCase <- loadCase "identifier-classifier"
  prepared <-
    prepareBenchmark
      RuntimeBenchmark
      programCase {programCaseExpectedStdout = "not the program output\n"}
  result <- try (runPreparedBenchmark prepared) :: IO (Either IOException ())
  case result of
    Left _ -> pure ()
    Right () -> failTest "expected the runtime benchmark to reject mismatched output"

loadCase :: Text -> IO ProgramCase
loadCase identifier = do
  corpus <- loadCheckedInCorpus
  case programCaseById identifier corpus of
    Nothing -> failTest ("missing checked-in corpus case: " <> identifier)
    Just programCase -> pure programCase

loadCompilerScaleCase :: Text -> IO CompilerScaleCase
loadCompilerScaleCase identifier =
  case selectCompilerScaleCases [identifier] compilerScaleCases of
    Left message -> failTest message
    Right [programCase] -> pure programCase
    Right selected ->
      failTest
        ( "expected one generated scale case for "
            <> identifier
            <> ", got "
            <> Text.pack (show (map compilerScaleCaseIdentifier selected))
        )

loadCheckedInCorpus :: IO ProgramCorpus
loadCheckedInCorpus = do
  result <- loadProgramCorpus
  case result of
    Left violations ->
      failTest
        ("could not load checked-in corpus:\n" <> Text.unlines (map renderProgramCorpusViolation violations))
    Right corpus -> pure corpus
