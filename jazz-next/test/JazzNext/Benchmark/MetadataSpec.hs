{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Exception (bracket)
import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy as LazyByteString
import Data.List (sort)
import qualified Data.Text as Text
import JazzNext.Benchmark.Metadata
  ( BenchmarkArtifactPaths (benchmarkArtifactDirectory),
    BenchmarkBuildMode (..),
    BenchmarkEnvironment (..),
    BenchmarkTimeMode (..),
    CompatibilityDecision (..),
    CompatibilityField (..),
    CompatibilityPolicy (..),
    PlatformFact (..),
    benchmarkArtifactPaths,
    benchmarkEnvironmentJson,
    checkBenchmarkCompatibility,
    compatibilityMismatchField,
    validateEnvironmentLabel,
  )
import JazzNext.Benchmark.Stages (runBenchmarkMainWithArguments)
import JazzNext.TestHarness
  ( NamedTest,
    assertEqual,
    failTest,
    runTestSuite,
  )
import System.Directory
  ( createDirectoryIfMissing,
    doesFileExist,
    getTemporaryDirectory,
    listDirectory,
    removeFile,
    removePathForcibly,
  )
import System.Environment (getArgs, getExecutablePath)
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath ((</>))
import System.IO (hClose, openTempFile)
import System.Process (readProcessWithExitCode)

main :: IO ()
main = do
  arguments <- getArgs
  case arguments of
    ["--artifact-smoke-child", resultRoot] -> runRecordedArtifactSmokeChild resultRoot
    _ -> runTestSuite "BenchmarkMetadata" tests

tests :: [NamedTest]
tests =
  [ ("encodes stable environment JSON", testStableEncoding),
    ("round trips every required identity field", testRequiredIdentityRoundTrip),
    ("records unavailable optional platform facts", testUnavailablePlatformFact),
    ("accepts only path-safe environment labels", testEnvironmentLabels),
    ("accepts exactly compatible environments", testExactCompatibility),
    ("classifies every compatibility mismatch", testIndividualMismatchCategories),
    ("accumulates compatibility mismatches", testAccumulatedMismatches),
    ("records an explicit compatibility override", testCompatibilityOverride),
    ("writes labelled artifacts for a fast benchmark", testRecordedArtifactSmoke)
  ]

testStableEncoding :: IO ()
testStableEncoding = do
  let first = benchmarkEnvironmentJson fixtureEnvironment
      second = benchmarkEnvironmentJson fixtureEnvironment
  assertEqual "deterministic bytes" first second
  assertEqual "stable JSON bytes" expectedFixtureJson first

testRequiredIdentityRoundTrip :: IO ()
testRequiredIdentityRoundTrip =
  case eitherDecode (benchmarkEnvironmentJson fixtureEnvironment) of
    Left message -> failTest ("could not decode environment JSON: " <> Text.pack message)
    Right decoded -> assertEqual "environment round trip" fixtureEnvironment decoded

testUnavailablePlatformFact :: IO ()
testUnavailablePlatformFact = do
  let unavailableEnvironment =
        fixtureEnvironment
          { environmentCpuIdentity = UnavailablePlatformFact "not reported"
          }
  case eitherDecode (benchmarkEnvironmentJson unavailableEnvironment) of
    Left message -> failTest ("could not decode unavailable CPU identity: " <> Text.pack message)
    Right decoded -> assertEqual "unavailable platform fact round trip" unavailableEnvironment decoded

testEnvironmentLabels :: IO ()
testEnvironmentLabels = do
  mapM_ assertAccepted ["m1-arm64", "linux_ci.2", "release-2026_07"]
  mapM_
    assertRejected
    [ "",
      ".hidden",
      "../escape",
      "nested/path",
      "with space",
      "mächine",
      Text.replicate 65 "a"
    ]
  where
    assertAccepted label =
      assertEqual ("accepted label " <> label) (Right label) (validateEnvironmentLabel label)
    assertRejected label =
      case validateEnvironmentLabel label of
        Left _ -> pure ()
        Right value -> failTest ("expected unsafe label to be rejected: " <> value)

testExactCompatibility :: IO ()
testExactCompatibility =
  assertEqual
    "exact compatibility"
    (Right CompatibleBenchmarks)
    (checkBenchmarkCompatibility RequireCompatible fixtureEnvironment fixtureEnvironment)

testIndividualMismatchCategories :: IO ()
testIndividualMismatchCategories =
  mapM_ assertCategory individualMismatches
  where
    assertCategory (expectedField, candidate) =
      case checkBenchmarkCompatibility RequireCompatible fixtureEnvironment candidate of
        Left [mismatch] ->
          assertEqual "mismatch field" expectedField (compatibilityMismatchField mismatch)
        Left mismatches ->
          failTest ("expected one mismatch, got " <> Text.pack (show mismatches))
        Right decision ->
          failTest ("expected incompatibility, got " <> Text.pack (show decision))

testAccumulatedMismatches :: IO ()
testAccumulatedMismatches =
  case checkBenchmarkCompatibility RequireCompatible fixtureEnvironment manyMismatches of
    Left mismatches ->
      assertEqual
        "stable accumulated fields"
        [EnvironmentLabelField, CorpusSchemaField, GhcVersionField, ArchitectureField, TimeModeField]
        (map compatibilityMismatchField mismatches)
    Right decision -> failTest ("expected mismatches, got " <> Text.pack (show decision))

testCompatibilityOverride :: IO ()
testCompatibilityOverride =
  case checkBenchmarkCompatibility AllowIncompatible fixtureEnvironment manyMismatches of
    Right (IncompatibleBenchmarksAllowed mismatches) ->
      assertEqual
        "recorded override fields"
        (sort [EnvironmentLabelField, CorpusSchemaField, GhcVersionField, ArchitectureField, TimeModeField])
        (sort (map compatibilityMismatchField mismatches))
    Right CompatibleBenchmarks -> failTest "expected the override to retain mismatches"
    Left mismatches -> failTest ("explicit override was rejected: " <> Text.pack (show mismatches))

testRecordedArtifactSmoke :: IO ()
testRecordedArtifactSmoke =
  withTemporaryDirectory $ \resultRoot -> do
    executable <- getExecutablePath
    (exitCode, _, standardError) <-
      readProcessWithExitCode executable ["--artifact-smoke-child", resultRoot] ""
    assertEqual ("artifact benchmark stderr: " <> Text.pack standardError) ExitSuccess exitCode
    runDirectories <- listDirectory (resultRoot </> "artifact-smoke")
    runDirectory <-
      case runDirectories of
        [value] -> pure (resultRoot </> "artifact-smoke" </> value)
        _ -> failTest ("expected one recorded run directory, got " <> Text.pack (show runDirectories))
    let resultsPath = runDirectory </> "results.csv"
        environmentPath = runDirectory </> "environment.json"
    resultsExists <- doesFileExist resultsPath
    environmentExists <- doesFileExist environmentPath
    assertEqual "results.csv exists" True resultsExists
    assertEqual "environment.json exists" True environmentExists
    encodedEnvironment <- LazyByteString.readFile environmentPath
    case eitherDecode encodedEnvironment of
      Left message -> failTest ("could not decode recorded metadata: " <> Text.pack message)
      Right environment -> do
        assertEqual "recorded environment label" "artifact-smoke" (environmentLabel environment)
        assertEqual "recorded selected cases" ["identifier-classifier"] (environmentSelectedCases environment)
        expectedPaths <-
          case benchmarkArtifactPaths resultRoot "artifact-smoke" (environmentRunIdentifier environment) of
            Left message -> failTest message
            Right value -> pure value
        assertEqual "recorded artifact directory" runDirectory (benchmarkArtifactDirectory expectedPaths)

runRecordedArtifactSmokeChild :: FilePath -> IO ()
runRecordedArtifactSmokeChild resultRoot =
  runBenchmarkMainWithArguments
    [ "--environment-label=artifact-smoke",
      "--result-root=" <> resultRoot,
      "--jazz-case=identifier-classifier",
      "--pattern=parse-lower",
      "--stdev=100",
      "--timeout=1s",
      "--color=never"
    ]

fixtureEnvironment :: BenchmarkEnvironment
fixtureEnvironment =
  BenchmarkEnvironment
    { environmentSchemaVersion = 1,
      environmentRunIdentifier = "20260714T120000000000Z",
      environmentLabel = "m1-arm64",
      environmentGitRevision = "0123456789abcdef",
      environmentGitDirty = False,
      environmentCorpusSchemaVersion = 1,
      environmentSelectedCases = ["identifier-classifier", "mini-frontend"],
      environmentGhcVersion = "9.14.1",
      environmentCabalVersion = "3.16.0.0",
      environmentPackageVersion = "0.1.0.0",
      environmentOperatingSystem = "darwin",
      environmentArchitecture = "aarch64",
      environmentCpuIdentity = AvailablePlatformFact "Example CPU",
      environmentBuildMode = OptimizedBenchmarkBuild,
      environmentRtsCapabilities = 1,
      environmentRtsArguments = ["-T"],
      environmentBenchmarkArguments = ["--stdev=5", "--time-mode=cpu"],
      environmentTimeMode = CpuBenchmarkTime,
      environmentRunTimestamp = "2026-07-14T12:00:00Z"
    }

expectedFixtureJson :: LazyByteString.ByteString
expectedFixtureJson =
  "{\"schema_version\":1,\"run_id\":\"20260714T120000000000Z\",\"environment_label\":\"m1-arm64\",\"git_revision\":\"0123456789abcdef\",\"git_dirty\":false,\"corpus_schema_version\":1,\"selected_cases\":[\"identifier-classifier\",\"mini-frontend\"],\"ghc_version\":\"9.14.1\",\"cabal_version\":\"3.16.0.0\",\"package_version\":\"0.1.0.0\",\"operating_system\":\"darwin\",\"architecture\":\"aarch64\",\"cpu_identity\":{\"status\":\"available\",\"value\":\"Example CPU\"},\"build_mode\":\"optimized\",\"rts_capabilities\":1,\"rts_arguments\":[\"-T\"],\"benchmark_arguments\":[\"--stdev=5\",\"--time-mode=cpu\"],\"time_mode\":\"cpu\",\"run_timestamp\":\"2026-07-14T12:00:00Z\"}"

individualMismatches :: [(CompatibilityField, BenchmarkEnvironment)]
individualMismatches =
  [ (MetadataSchemaField, fixtureEnvironment {environmentSchemaVersion = 2}),
    (EnvironmentLabelField, fixtureEnvironment {environmentLabel = "other"}),
    (CorpusSchemaField, fixtureEnvironment {environmentCorpusSchemaVersion = 2}),
    (SelectedCasesField, fixtureEnvironment {environmentSelectedCases = ["mini-frontend"]}),
    (GhcVersionField, fixtureEnvironment {environmentGhcVersion = "9.16.1"}),
    (CabalVersionField, fixtureEnvironment {environmentCabalVersion = "3.18.0.0"}),
    (PackageVersionField, fixtureEnvironment {environmentPackageVersion = "0.2.0.0"}),
    (OperatingSystemField, fixtureEnvironment {environmentOperatingSystem = "linux"}),
    (ArchitectureField, fixtureEnvironment {environmentArchitecture = "x86_64"}),
    (CpuIdentityField, fixtureEnvironment {environmentCpuIdentity = UnavailablePlatformFact "not reported"}),
    (BuildModeField, fixtureEnvironment {environmentBuildMode = ProfilingBenchmarkBuild}),
    (RtsCapabilitiesField, fixtureEnvironment {environmentRtsCapabilities = 4}),
    (RtsArgumentsField, fixtureEnvironment {environmentRtsArguments = ["-T", "-N4"]}),
    (TimeModeField, fixtureEnvironment {environmentTimeMode = WallBenchmarkTime})
  ]

manyMismatches :: BenchmarkEnvironment
manyMismatches =
  fixtureEnvironment
    { environmentLabel = "other",
      environmentCorpusSchemaVersion = 2,
      environmentGhcVersion = "9.16.1",
      environmentArchitecture = "x86_64",
      environmentTimeMode = MutatorWallBenchmarkTime
    }

withTemporaryDirectory :: (FilePath -> IO a) -> IO a
withTemporaryDirectory = bracket create removePathForcibly
  where
    create = do
      temporaryDirectory <- getTemporaryDirectory
      (path, handle) <- openTempFile temporaryDirectory "jazz-next-benchmark-metadata"
      hClose handle
      removeFile path
      createDirectoryIfMissing True path
      pure path
