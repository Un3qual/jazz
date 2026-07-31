{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Exception (IOException, try)
import Control.Monad (void)
import Data.Text (Text)
import qualified Data.Text as Text
import Jazz.Benchmark.StageInputs
  ( prepareBenchmark,
    runPreparedBenchmark,
    selectProgramCases,
  )
import Jazz.Compiler.Profiling (BenchmarkGroup (..))
import Jazz.ProgramCorpus.Manifest (loadProgramCorpus, programCaseById, renderProgramCorpusViolation)
import Jazz.ProgramCorpus.Types (ProgramCase (..), ProgramCorpus (..))
import Jazz.TestHarness (NamedTest, assertEqual, failTest, runTestSuite)

main :: IO ()
main = runTestSuite "BenchmarkStages" tests

tests :: [NamedTest]
tests =
  [ ("selects requested cases before stage preparation", testCaseSelection),
    ("parse-lower setup does not require module compilation", testParseLowerSetupBoundary),
    ("parse-lower setup reports entry-source read failures", testParseLowerSourceReadFailure),
    ("analysis uses module-aware imported interfaces", testModuleAwareAnalysis),
    ("runtime benchmarks reject unexpected results", testRuntimeResultValidation)
  ]

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
      ) :: IO (Either IOException ())
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

loadCheckedInCorpus :: IO ProgramCorpus
loadCheckedInCorpus = do
  result <- loadProgramCorpus
  case result of
    Left violations ->
      failTest
        ("could not load checked-in corpus:\n" <> Text.unlines (map renderProgramCorpusViolation violations))
    Right corpus -> pure corpus
