{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Exception (bracket)
import Control.Monad (forM, forM_, unless)
import Data.List (sort, sortOn)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import JazzNext.Compiler.Diagnostics (renderDiagnostic)
import JazzNext.Compiler.Runtime.Observation
  ( RuntimeObservationReport (..),
    RuntimeObservationRequest (RuntimeObservationStatistics),
    RuntimeStatistics (..),
    RuntimeTermination (RuntimeSucceeded),
    emptyRuntimeStatistics,
  )
import JazzNext.ProgramCorpus.Manifest
  ( canonicalizeValidatedPath,
    loadProgramCorpus,
    loadProgramCorpusAt,
    loadProgramCorpusAtWithRootCanonicalizer,
    programCaseById,
    renderProgramCorpusViolation,
  )
import JazzNext.ProgramCorpus.Runner
  ( ProgramCaseResult (..),
    programCaseBudgetViolations,
    readProgramCaseSource,
    runProgramCase,
    runProgramCaseObserved,
  )
import JazzNext.ProgramCorpus.Types
  ( BenchmarkGroup,
    FeatureTag,
    ProgramBudgetMetric (..),
    ProgramBudgetViolation (..),
    ProgramBudgets (..),
    ProgramCase (..),
    ProgramCorpus (..),
    ProgramCorpusViolation (..),
    ProgramPathField (..),
    WorkloadClass,
  )
import JazzNext.TestHarness
  ( NamedTest,
    assertEqual,
    failTest,
    runTestSuite,
  )
import System.Directory
  ( createDirectory,
    createDirectoryIfMissing,
    createFileLink,
    doesDirectoryExist,
    emptyPermissions,
    getTemporaryDirectory,
    listDirectory,
    removeFile,
    removePathForcibly,
    setPermissions,
  )
import System.FilePath (takeExtension, (</>))
import System.IO (hClose, openTempFile)

main :: IO ()
main = runTestSuite "ProgramCorpus" tests

tests :: [NamedTest]
tests =
  [ ("reports all manifest violations in stable order", testAggregateManifestViolations),
    ("reports malformed manifest JSON", testMalformedManifest),
    ("reports unreadable manifests as corpus violations", testUnreadableManifest),
    ("treats unreadable corpus sources as unavailable", testUnreadableCorpusSource),
    ("rejects unknown budget fields", testUnknownBudgetField),
    ("reports corpus-root canonicalization failures as corpus violations", testRootCanonicalizationFailure),
    ("reports case-path canonicalization failures as corpus violations", testCasePathCanonicalizationFailure),
    ("reports every missing corpus path", testMissingCorpusPaths),
    ("rejects a source symlink that escapes the corpus root", testSymlinkEscape),
    ("loads and runs the checked-in identifier classifier", testIdentifierClassifier),
    ("covers the production-shaped corpus contract", testCheckedInCorpusCoverage),
    ("documents every checked-in corpus case", testCheckedInCorpusDocumentation),
    ("budget upper limits accept equal and lower work", testBudgetUpperLimits),
    ("optional budgets are enforced and violations are stably accumulated", testOptionalBudgetViolations),
    ("an in-memory checked-in budget override reports a useful violation", testCheckedInBudgetOverride),
    ("runtime statistics are deterministic for a checked-in case", testDeterministicRuntimeStatistics),
    ("runs every checked-in corpus case within its budgets", testEveryCheckedInCase)
  ]

testAggregateManifestViolations :: IO ()
testAggregateManifestViolations =
  withTemporaryDirectory $ \root -> do
    let caseRoot = root </> "same"
    createDirectory caseRoot
    TextIO.writeFile (caseRoot </> "Main.jz") validMainSource
    TextIO.writeFile (caseRoot </> "expected.stdout") "0\n"
    writeManifest root invalidAggregateManifest
    result <- loadProgramCorpusAt root
    case result of
      Right corpus -> failTest ("expected manifest violations, loaded " <> Text.pack (show corpus))
      Left violations -> do
        let expected =
              [ UnsupportedSchemaVersion 99,
                DuplicateCaseIdentifier "duplicate",
                DuplicateCaseDirectory "same",
                UnknownWorkloadClass "duplicate" "overnight",
                UnknownFeatureTag "duplicate" "telepathy",
                UnknownBenchmarkGroup "duplicate" "llvm",
                AbsoluteCorpusPath "duplicate" EntrySourcePath "/absolute/Main.jz",
                EscapingCorpusPath "duplicate" ModuleRootPath "../modules",
                EscapingCorpusPath "duplicate" ExpectedStdoutPath "../outside.stdout"
              ]
        assertEqual "aggregate violation set" (sort expected) (sort violations)
        assertEqual
          "stable rendered violation order"
          (sortOn renderProgramCorpusViolation violations)
          violations

testMalformedManifest :: IO ()
testMalformedManifest =
  withTemporaryDirectory $ \root -> do
    writeManifest root "{ definitely-not-json }"
    result <- loadProgramCorpusAt root
    case result of
      Left [ManifestDecodeFailure message]
        | not (Text.null message) -> pure ()
      Left violations ->
        failTest ("expected one manifest decode failure, got " <> Text.pack (show violations))
      Right corpus -> failTest ("expected malformed manifest failure, loaded " <> Text.pack (show corpus))

testUnreadableManifest :: IO ()
testUnreadableManifest =
  withTemporaryDirectory $ \root -> do
    let manifestPath = root </> "corpus.json"
    writeManifest root "{}"
    setPermissions manifestPath emptyPermissions
    result <- loadProgramCorpusAt root
    case result of
      Left violations
        | any
            (Text.isInfixOf "could not read program corpus manifest" . renderProgramCorpusViolation)
            violations ->
            pure ()
      Left violations ->
        failTest ("expected an unreadable-manifest violation, got " <> Text.pack (show violations))
      Right corpus -> failTest ("expected unreadable manifest failure, loaded " <> Text.pack (show corpus))

testUnreadableCorpusSource :: IO ()
testUnreadableCorpusSource =
  withTemporaryDirectory $ \root -> do
    let sourcePath = root </> "Main.jz"
    TextIO.writeFile sourcePath validMainSource
    setPermissions sourcePath emptyPermissions
    source <- readProgramCaseSource sourcePath
    assertEqual "unreadable corpus source" Nothing source

testUnknownBudgetField :: IO ()
testUnknownBudgetField =
  withTemporaryDirectory $ \root -> do
    let caseRoot = root </> "budget-typo"
    createDirectory caseRoot
    TextIO.writeFile (caseRoot </> "Main.jz") validMainSource
    TextIO.writeFile (caseRoot </> "expected.stdout") "0\n"
    writeManifest root unknownBudgetFieldManifest
    result <- loadProgramCorpusAt root
    case result of
      Left [ManifestDecodeFailure message]
        | "unknown program budget field: forcedValue" `Text.isInfixOf` message -> pure ()
      Left violations ->
        failTest ("expected an unknown-budget decode failure, got " <> Text.pack (show violations))
      Right corpus -> failTest ("expected unknown budget field failure, loaded " <> Text.pack (show corpus))

testRootCanonicalizationFailure :: IO ()
testRootCanonicalizationFailure =
  withTemporaryDirectory $ \root -> do
    writeManifest root "{\"schemaVersion\":1,\"cases\":[]}"
    result <-
      loadProgramCorpusAtWithRootCanonicalizer
        (\_ -> ioError (userError "simulated root canonicalization failure"))
        root
    case result of
      Left [UnreadableCorpusRoot path message]
        | path == root,
          "simulated root canonicalization failure" `Text.isInfixOf` message ->
            pure ()
      other -> failTest ("expected corpus-root violation, got " <> Text.pack (show other))

testCasePathCanonicalizationFailure :: IO ()
testCasePathCanonicalizationFailure = do
  result <-
    canonicalizeValidatedPath
      (\_ -> ioError (userError "simulated canonicalization failure"))
      "/corpus"
      "broken"
      EntrySourcePath
      "broken/Main.jz"
  case result of
    Left (UnreadableCorpusPath "broken" EntrySourcePath "broken/Main.jz" message)
      | "simulated canonicalization failure" `Text.isInfixOf` message -> pure ()
    other -> failTest ("expected canonicalization violation, got " <> Text.pack (show other))

testMissingCorpusPaths :: IO ()
testMissingCorpusPaths =
  withTemporaryDirectory $ \root -> do
    createDirectory (root </> "missing-case")
    writeManifest root missingPathsManifest
    result <- loadProgramCorpusAt root
    case result of
      Left violations ->
        assertEqual
          "missing path violations"
          [ MissingCorpusPath "missing" EntrySourcePath "missing-case/Main.jz",
            MissingCorpusPath "missing" ExpectedStdoutPath "missing-case/expected.stdout"
          ]
          violations
      Right corpus -> failTest ("expected missing path violations, loaded " <> Text.pack (show corpus))

testSymlinkEscape :: IO ()
testSymlinkEscape =
  withTemporaryDirectory $ \temporaryRoot -> do
    let corpusRoot = temporaryRoot </> "programs"
        caseRoot = corpusRoot </> "linked"
        outsideSource = temporaryRoot </> "Outside.jz"
        linkedSource = caseRoot </> "Main.jz"
    createDirectory corpusRoot
    createDirectory caseRoot
    TextIO.writeFile outsideSource validMainSource
    createFileLink outsideSource linkedSource
    TextIO.writeFile (caseRoot </> "expected.stdout") "0\n"
    writeManifest corpusRoot symlinkManifest
    result <- loadProgramCorpusAt corpusRoot
    case result of
      Left violations ->
        assertEqual
          "symlink escape violations"
          [EscapingCorpusPath "linked" EntrySourcePath "linked/Main.jz"]
          violations
      Right corpus -> failTest ("expected symlink escape violation, loaded " <> Text.pack (show corpus))

testIdentifierClassifier :: IO ()
testIdentifierClassifier = do
  corpus <- loadCheckedInCorpus
  programCase <-
    case programCaseById "identifier-classifier" corpus of
      Nothing -> failTest "checked-in corpus is missing identifier-classifier"
      Just value -> pure value
  result <- runProgramCase programCase
  assertEqual
    "identifier-classifier termination"
    (programCaseExpectedTermination programCase)
    (programCaseResultTermination result)
  assertEqual
    "identifier-classifier stdout"
    (programCaseExpectedStdout programCase)
    (programCaseResultStdout result)
  assertEqual
    "identifier-classifier diagnostics"
    []
    (map renderDiagnostic (programCaseResultDiagnostics result))
  assertEqual
    "identifier-classifier warnings"
    []
    (programCaseResultWarnings result)

testCheckedInCorpusCoverage :: IO ()
testCheckedInCorpusCoverage = do
  corpus <- loadCheckedInCorpus
  let cases = programCorpusCases corpus
  assertEqual
    "stable corpus case identifiers"
    ( Set.fromList
        [ "identifier-classifier",
          "expression-evaluator",
          "tree-transformations",
          "dependency-planner",
          "capability-workflow",
          "mini-frontend"
        ]
    )
    (Set.fromList (map programCaseIdentifier cases))
  assertEqual
    "feature tag coverage"
    (Set.fromList ([minBound .. maxBound] :: [FeatureTag]))
    (Set.fromList (concatMap programCaseFeatures cases))
  assertEqual
    "workload class coverage"
    (Set.fromList ([minBound .. maxBound] :: [WorkloadClass]))
    (Set.fromList (map programCaseWorkload cases))
  assertEqual
    "benchmark group coverage"
    (Set.fromList ([minBound .. maxBound] :: [BenchmarkGroup]))
    (Set.fromList (concatMap programCaseBenchmarks cases))
  sourceCounts <- mapM countJazzSources cases
  if any (> 1) sourceCounts
    then pure ()
    else failTest "expected at least one multi-module corpus case"

testCheckedInCorpusDocumentation :: IO ()
testCheckedInCorpusDocumentation = do
  corpus <- loadCheckedInCorpus
  readme <- TextIO.readFile (programCorpusRoot corpus </> "README.md")
  let heading = "## Current cases"
      (_, sectionAndFollowing) = Text.breakOn heading readme
  whenMissingSection sectionAndFollowing
  let currentCasesSection =
        Text.unlines
          ( takeWhile
              (not . Text.isPrefixOf "## ")
              (drop 1 (Text.lines sectionAndFollowing))
          )
  forM_ (programCorpusCases corpus) $ \programCase ->
    unless (programCaseIdentifier programCase `Text.isInfixOf` currentCasesSection) $
      failTest
        ( "programs/README.md does not document corpus case "
            <> programCaseIdentifier programCase
        )
  where
    whenMissingSection section
      | Text.null section = failTest "programs/README.md is missing a Current cases section"
      | otherwise = pure ()

testEveryCheckedInCase :: IO ()
testEveryCheckedInCase = do
  corpus <- loadCheckedInCorpus
  forM_ (programCorpusCases corpus) $ \programCase -> do
    result <- runProgramCaseObserved RuntimeObservationStatistics programCase
    assertEqual
      (programCaseIdentifier programCase <> " termination")
      (programCaseExpectedTermination programCase)
      (programCaseResultTermination result)
    assertEqual
      (programCaseIdentifier programCase <> " stdout")
      (programCaseExpectedStdout programCase)
      (programCaseResultStdout result)
    assertEqual
      (programCaseIdentifier programCase <> " diagnostics")
      []
      (map renderDiagnostic (programCaseResultDiagnostics result))
    assertEqual
      (programCaseIdentifier programCase <> " warnings")
      []
      (programCaseResultWarnings result)
    report <- requireProgramReport programCase result
    assertEqual
      (programCaseIdentifier programCase <> " budget violations")
      []
      (programCaseBudgetViolations programCase report)

testBudgetUpperLimits :: IO ()
testBudgetUpperLimits = do
  programCase <- firstCheckedInCase
  let budgetedCase =
        programCase
          { programCaseBudgets =
              ProgramBudgets
                { programBudgetSteps = 10,
                  programBudgetApplications = 5,
                  programBudgetMaxContinuationDepth = 3,
                  programBudgetOptionalLimits = Map.empty
                }
          }
      equalStatistics =
        emptyRuntimeStatistics
          { runtimeEvaluatorTransitions = 10,
            runtimeApplications = 5,
            runtimeMaximumContinuationDepth = 3
          }
      lowerStatistics =
        equalStatistics
          { runtimeEvaluatorTransitions = 9,
            runtimeApplications = 4,
            runtimeMaximumContinuationDepth = 2
          }
  assertEqual "equal work passes" [] (programCaseBudgetViolations budgetedCase (successfulReport equalStatistics))
  assertEqual "lower work passes" [] (programCaseBudgetViolations budgetedCase (successfulReport lowerStatistics))

testOptionalBudgetViolations :: IO ()
testOptionalBudgetViolations = do
  programCase <- firstCheckedInCase
  let budgetedCase =
        programCase
          { programCaseBudgets =
              ProgramBudgets
                { programBudgetSteps = 10,
                  programBudgetApplications = 5,
                  programBudgetMaxContinuationDepth = 3,
                  programBudgetOptionalLimits =
                    Map.fromList
                      [ (ClosuresCreatedBudget, 1),
                        (ListCellsConstructedBudget, 2)
                      ]
                }
          }
      statistics =
        emptyRuntimeStatistics
          { runtimeEvaluatorTransitions = 11,
            runtimeApplications = 5,
            runtimeMaximumContinuationDepth = 3,
            runtimeClosuresCreated = 2,
            runtimeListCellsConstructed = 4
          }
      expected =
        [ ProgramBudgetViolation
            { programBudgetViolationCase = programCaseIdentifier programCase,
              programBudgetViolationMetric = EvaluatorTransitionsBudget,
              programBudgetViolationLimit = 10,
              programBudgetViolationActual = 11,
              programBudgetViolationPercentageIncrease = Just 10
            },
          ProgramBudgetViolation
            { programBudgetViolationCase = programCaseIdentifier programCase,
              programBudgetViolationMetric = ClosuresCreatedBudget,
              programBudgetViolationLimit = 1,
              programBudgetViolationActual = 2,
              programBudgetViolationPercentageIncrease = Just 100
            },
          ProgramBudgetViolation
            { programBudgetViolationCase = programCaseIdentifier programCase,
              programBudgetViolationMetric = ListCellsConstructedBudget,
              programBudgetViolationLimit = 2,
              programBudgetViolationActual = 4,
              programBudgetViolationPercentageIncrease = Just 100
            }
        ]
  assertEqual
    "stable accumulated violations"
    expected
    (programCaseBudgetViolations budgetedCase (successfulReport statistics))

testCheckedInBudgetOverride :: IO ()
testCheckedInBudgetOverride = do
  programCase <- firstCheckedInCase
  result <- runProgramCaseObserved RuntimeObservationStatistics programCase
  report <- requireProgramReport programCase result
  let statistics = runtimeObservationStatistics report
      actualSteps = runtimeEvaluatorTransitions statistics
      overBudgetCase =
        programCase
          { programCaseBudgets =
              (programCaseBudgets programCase)
                { programBudgetSteps = actualSteps - 1
                }
          }
      violations = programCaseBudgetViolations overBudgetCase report
  case violations of
    violation : _ -> do
      assertEqual "override case" (programCaseIdentifier programCase) (programBudgetViolationCase violation)
      assertEqual "override metric" EvaluatorTransitionsBudget (programBudgetViolationMetric violation)
      assertEqual "override actual" actualSteps (programBudgetViolationActual violation)
    [] -> failTest "expected the in-memory budget override to fail"

testDeterministicRuntimeStatistics :: IO ()
testDeterministicRuntimeStatistics = do
  programCase <- firstCheckedInCase
  firstResult <- runProgramCaseObserved RuntimeObservationStatistics programCase
  secondResult <- runProgramCaseObserved RuntimeObservationStatistics programCase
  firstReport <- requireProgramReport programCase firstResult
  secondReport <- requireProgramReport programCase secondResult
  assertEqual "repeat runtime reports" firstReport secondReport
  assertEqual
    "repeat budget results"
    (programCaseBudgetViolations programCase firstReport)
    (programCaseBudgetViolations programCase secondReport)

firstCheckedInCase :: IO ProgramCase
firstCheckedInCase = do
  corpus <- loadCheckedInCorpus
  case programCorpusCases corpus of
    programCase : _ -> pure programCase
    [] -> failTest "checked-in corpus is empty"

successfulReport :: RuntimeStatistics -> RuntimeObservationReport
successfulReport statistics =
  RuntimeObservationReport
    { runtimeObservationTermination = RuntimeSucceeded,
      runtimeObservationStatistics = statistics,
      runtimeObservationProfile = Nothing
    }

requireProgramReport :: ProgramCase -> ProgramCaseResult -> IO RuntimeObservationReport
requireProgramReport programCase result =
  case programCaseResultObservation result of
    Just report -> pure report
    Nothing -> failTest (programCaseIdentifier programCase <> " did not produce runtime statistics")

loadCheckedInCorpus :: IO ProgramCorpus
loadCheckedInCorpus = do
  corpusResult <- loadProgramCorpus
  case corpusResult of
    Left violations ->
      failTest
        ( "could not load checked-in corpus:\n"
            <> Text.unlines (map renderProgramCorpusViolation violations)
        )
    Right value -> pure value

countJazzSources :: ProgramCase -> IO Int
countJazzSources programCase = do
  paths <- listFilesRecursively (programCaseDirectory programCase)
  pure (length (filter ((== ".jz") . takeExtension) paths))

listFilesRecursively :: FilePath -> IO [FilePath]
listFilesRecursively directory = do
  entries <- listDirectory directory
  fmap concat $ forM entries $ \entry -> do
    let path = directory </> entry
    isDirectory <- doesDirectoryExist path
    if isDirectory then listFilesRecursively path else pure [path]

writeManifest :: FilePath -> Text -> IO ()
writeManifest root = TextIO.writeFile (root </> "corpus.json")

withTemporaryDirectory :: (FilePath -> IO a) -> IO a
withTemporaryDirectory = bracket create removePathForcibly
  where
    create = do
      temporaryDirectory <- getTemporaryDirectory
      (path, handle) <- openTempFile temporaryDirectory "jazz-next-program-corpus"
      hClose handle
      removeFile path
      createDirectoryIfMissing True path
      pure path

validMainSource :: Text
validMainSource =
  """
  module Main {
    0.
  }
  """

invalidAggregateManifest :: Text
invalidAggregateManifest =
  """
  {
    "schemaVersion": 99,
    "cases": [
      {
        "id": "duplicate",
        "directory": "same",
        "entrySource": "Main.jz",
        "moduleRoot": ".",
        "expectedTermination": "success",
        "expectedStdout": "expected.stdout",
        "workload": "fast",
        "features": ["text"],
        "benchmarks": ["runtime"],
        "budgets": {
          "steps": 100,
          "applications": 10,
          "maxContinuationDepth": 10
        }
      },
      {
        "id": "duplicate",
        "directory": "same",
        "entrySource": "/absolute/Main.jz",
        "moduleRoot": "../modules",
        "expectedTermination": "success",
        "expectedStdout": "../outside.stdout",
        "workload": "overnight",
        "features": ["telepathy"],
        "benchmarks": ["llvm"],
        "budgets": {
          "steps": 100,
          "applications": 10,
          "maxContinuationDepth": 10
        }
      }
    ]
  }
  """

missingPathsManifest :: Text
missingPathsManifest =
  """
  {
    "schemaVersion": 1,
    "cases": [
      {
        "id": "missing",
        "directory": "missing-case",
        "entrySource": "Main.jz",
        "moduleRoot": ".",
        "expectedTermination": "success",
        "expectedStdout": "expected.stdout",
        "workload": "fast",
        "features": ["modules"],
        "benchmarks": ["whole-program"],
        "budgets": {
          "steps": 100,
          "applications": 10,
          "maxContinuationDepth": 10
        }
      }
    ]
  }
  """

unknownBudgetFieldManifest :: Text
unknownBudgetFieldManifest =
  """
  {
    "schemaVersion": 1,
    "cases": [
      {
        "id": "budget-typo",
        "directory": "budget-typo",
        "entrySource": "Main.jz",
        "moduleRoot": ".",
        "expectedTermination": "success",
        "expectedStdout": "expected.stdout",
        "workload": "fast",
        "features": ["deterministic-runtime"],
        "benchmarks": ["runtime"],
        "budgets": {
          "steps": 100,
          "applications": 10,
          "maxContinuationDepth": 10,
          "forcedValue": 20
        }
      }
    ]
  }
  """

symlinkManifest :: Text
symlinkManifest =
  """
  {
    "schemaVersion": 1,
    "cases": [
      {
        "id": "linked",
        "directory": "linked",
        "entrySource": "Main.jz",
        "moduleRoot": ".",
        "expectedTermination": "success",
        "expectedStdout": "expected.stdout",
        "workload": "fast",
        "features": ["modules"],
        "benchmarks": ["whole-program"],
        "budgets": {
          "steps": 100,
          "applications": 10,
          "maxContinuationDepth": 10
        }
      }
    ]
  }
  """
