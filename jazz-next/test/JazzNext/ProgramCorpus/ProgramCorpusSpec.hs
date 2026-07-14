{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Exception (bracket)
import Control.Monad (forM, forM_)
import Data.List (sort, sortOn)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import JazzNext.Compiler.Diagnostics (renderDiagnostic)
import JazzNext.ProgramCorpus.Manifest
  ( loadProgramCorpus,
    loadProgramCorpusAt,
    programCaseById,
    renderProgramCorpusViolation,
  )
import JazzNext.ProgramCorpus.Runner
  ( ProgramCaseResult (..),
    runProgramCase,
  )
import JazzNext.ProgramCorpus.Types
  ( ProgramCase (..),
    ProgramCorpus (..),
    ProgramCorpusViolation (..),
    ProgramPathField (..),
    BenchmarkGroup,
    FeatureTag,
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
    getTemporaryDirectory,
    doesDirectoryExist,
    listDirectory,
    removeFile,
    removePathForcibly,
  )
import System.FilePath (takeExtension, (</>))
import System.IO (hClose, openTempFile)

main :: IO ()
main = runTestSuite "ProgramCorpus" tests

tests :: [NamedTest]
tests =
  [ ("reports all manifest violations in stable order", testAggregateManifestViolations),
    ("reports malformed manifest JSON", testMalformedManifest),
    ("reports every missing corpus path", testMissingCorpusPaths),
    ("rejects a source symlink that escapes the corpus root", testSymlinkEscape),
    ("loads and runs the checked-in identifier classifier", testIdentifierClassifier),
    ("covers the production-shaped corpus contract", testCheckedInCorpusCoverage),
    ("runs every checked-in corpus case", testEveryCheckedInCase)
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

testEveryCheckedInCase :: IO ()
testEveryCheckedInCase = do
  corpus <- loadCheckedInCorpus
  forM_ (programCorpusCases corpus) $ \programCase -> do
    result <- runProgramCase programCase
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
