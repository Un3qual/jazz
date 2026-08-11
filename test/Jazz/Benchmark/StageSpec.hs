{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Exception (IOException, try)
import Control.Monad (void)
import Data.Text (Text)
import qualified Data.Text as Text
import Jazz.Benchmark.ScaleCases
  ( CompilerScaleCase,
    CompilerScaleScenario (..),
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
      ("wide-module-fanout-0064x0016", WideModuleFanout, 64, Just 16)
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
