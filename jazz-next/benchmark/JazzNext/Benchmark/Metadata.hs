{-# LANGUAGE OverloadedStrings #-}

module JazzNext.Benchmark.Metadata
  ( BenchmarkArtifactPaths (..),
    BenchmarkBuildMode (..),
    BenchmarkEnvironment (..),
    BenchmarkEnvironmentCapture (..),
    BenchmarkTimeMode (..),
    CompatibilityDecision (..),
    CompatibilityField (..),
    CompatibilityMismatch (..),
    CompatibilityPolicy (..),
    EnvironmentFact (..),
    benchmarkArtifactPaths,
    benchmarkBuildModeForProfiling,
    benchmarkEnvironmentJson,
    benchmarkRunIdentity,
    benchmarkTimeModeFromArguments,
    captureBenchmarkEnvironment,
    checkBenchmarkCompatibility,
    createBenchmarkArtifactDirectory,
    validateEnvironmentLabel,
    writeBenchmarkEnvironment,
  )
where

import Control.Exception (IOException, try)
import Data.Aeson
  ( FromJSON (parseJSON),
    ToJSON (toEncoding, toJSON),
    object,
    pairs,
    withObject,
    withText,
    (.:),
    (.=),
  )
import Data.Aeson.Encoding (encodingToLazyByteString)
import qualified Data.ByteString.Lazy as LazyByteString
import Data.Char (isAlphaNum, isAscii)
import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Version (showVersion)
import GHC.Conc (getNumCapabilities)
import GHC.Environment (getFullArgs)
import Paths_jazz_next (version)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath ((</>))
import qualified System.Info as SystemInfo
import System.Process (readProcessWithExitCode)

data BenchmarkBuildMode
  = OptimizedBenchmarkBuild
  | ProfilingBenchmarkBuild
  deriving (Bounded, Enum, Eq, Ord, Show)

data BenchmarkTimeMode
  = CpuBenchmarkTime
  | WallBenchmarkTime
  | MutatorCpuBenchmarkTime
  | MutatorWallBenchmarkTime
  deriving (Bounded, Enum, Eq, Ord, Show)

data EnvironmentFact value
  = AvailableEnvironmentFact value
  | UnavailableEnvironmentFact Text
  deriving (Eq, Ord, Show)

data BenchmarkEnvironment = BenchmarkEnvironment
  { environmentSchemaVersion :: Int,
    environmentRunIdentifier :: Text,
    environmentLabel :: Text,
    environmentGitRevision :: EnvironmentFact Text,
    environmentGitDirty :: EnvironmentFact Bool,
    environmentCorpusSchemaVersion :: Int,
    environmentSelectedCases :: [Text],
    environmentGhcVersion :: Text,
    environmentCabalVersion :: EnvironmentFact Text,
    environmentPackageVersion :: Text,
    environmentOperatingSystem :: Text,
    environmentArchitecture :: Text,
    environmentCpuIdentity :: EnvironmentFact Text,
    environmentBuildMode :: BenchmarkBuildMode,
    environmentRtsCapabilities :: Int,
    environmentRtsArguments :: [Text],
    environmentBenchmarkArguments :: [Text],
    environmentTimeMode :: BenchmarkTimeMode,
    environmentRunTimestamp :: Text
  }
  deriving (Eq, Show)

data BenchmarkEnvironmentCapture = BenchmarkEnvironmentCapture
  { capturePackageRoot :: FilePath,
    captureRunIdentifier :: Text,
    captureEnvironmentLabel :: Text,
    captureCorpusSchemaVersion :: Int,
    captureSelectedCases :: [Text],
    captureBuildMode :: BenchmarkBuildMode,
    captureBenchmarkArguments :: [String],
    captureTimeMode :: BenchmarkTimeMode,
    captureRunTimestamp :: Text
  }
  deriving (Eq, Show)

data BenchmarkArtifactPaths = BenchmarkArtifactPaths
  { benchmarkArtifactLabel :: Text,
    benchmarkArtifactRunIdentifier :: Text,
    benchmarkArtifactDirectory :: FilePath,
    benchmarkArtifactResultsCsv :: FilePath,
    benchmarkArtifactEnvironmentJson :: FilePath
  }
  deriving (Eq, Show)

data CompatibilityField
  = MetadataSchemaField
  | EnvironmentLabelField
  | CorpusSchemaField
  | SelectedCasesField
  | GhcVersionField
  | CabalVersionField
  | PackageVersionField
  | OperatingSystemField
  | ArchitectureField
  | CpuIdentityField
  | BuildModeField
  | RtsCapabilitiesField
  | RtsArgumentsField
  | BenchmarkArgumentsField
  | TimeModeField
  deriving (Bounded, Enum, Eq, Ord, Show)

data CompatibilityMismatch = CompatibilityMismatch
  { compatibilityMismatchField :: CompatibilityField,
    compatibilityBaselineValue :: Text,
    compatibilityCandidateValue :: Text
  }
  deriving (Eq, Show)

data CompatibilityPolicy
  = RequireCompatible
  | AllowIncompatible
  deriving (Eq, Show)

data CompatibilityDecision
  = CompatibleBenchmarks
  | IncompatibleBenchmarksAllowed [CompatibilityMismatch]
  deriving (Eq, Show)

instance ToJSON BenchmarkBuildMode where
  toJSON = toJSON . benchmarkBuildModeName
  toEncoding = toEncoding . benchmarkBuildModeName

instance FromJSON BenchmarkBuildMode where
  parseJSON = withText "BenchmarkBuildMode" $ \name ->
    case benchmarkBuildModeFromName name of
      Just buildMode -> pure buildMode
      Nothing -> fail ("unknown benchmark build mode: " <> Text.unpack name)

instance ToJSON BenchmarkTimeMode where
  toJSON = toJSON . benchmarkTimeModeName
  toEncoding = toEncoding . benchmarkTimeModeName

instance FromJSON BenchmarkTimeMode where
  parseJSON = withText "BenchmarkTimeMode" $ \name ->
    case benchmarkTimeModeFromName name of
      Just timeMode -> pure timeMode
      Nothing -> fail ("unknown benchmark time mode: " <> Text.unpack name)

instance (ToJSON value) => ToJSON (EnvironmentFact value) where
  toJSON environmentFact =
    case environmentFact of
      AvailableEnvironmentFact value ->
        object ["status" .= ("available" :: Text), "value" .= value]
      UnavailableEnvironmentFact reason ->
        object ["status" .= ("unavailable" :: Text), "reason" .= reason]
  toEncoding environmentFact =
    case environmentFact of
      AvailableEnvironmentFact value ->
        pairs ("status" .= ("available" :: Text) <> "value" .= value)
      UnavailableEnvironmentFact reason ->
        pairs ("status" .= ("unavailable" :: Text) <> "reason" .= reason)

instance (FromJSON value) => FromJSON (EnvironmentFact value) where
  parseJSON = withObject "EnvironmentFact" $ \environmentFact -> do
    status <- environmentFact .: "status"
    case (status :: Text) of
      "available" -> AvailableEnvironmentFact <$> environmentFact .: "value"
      "unavailable" -> UnavailableEnvironmentFact <$> environmentFact .: "reason"
      _ -> fail ("unknown environment fact status: " <> Text.unpack status)

instance ToJSON BenchmarkEnvironment where
  toJSON environment =
    object
      [ "schema_version" .= environmentSchemaVersion environment,
        "run_id" .= environmentRunIdentifier environment,
        "environment_label" .= environmentLabel environment,
        "git_revision" .= environmentGitRevision environment,
        "git_dirty" .= environmentGitDirty environment,
        "corpus_schema_version" .= environmentCorpusSchemaVersion environment,
        "selected_cases" .= environmentSelectedCases environment,
        "ghc_version" .= environmentGhcVersion environment,
        "cabal_version" .= environmentCabalVersion environment,
        "package_version" .= environmentPackageVersion environment,
        "operating_system" .= environmentOperatingSystem environment,
        "architecture" .= environmentArchitecture environment,
        "cpu_identity" .= environmentCpuIdentity environment,
        "build_mode" .= environmentBuildMode environment,
        "rts_capabilities" .= environmentRtsCapabilities environment,
        "rts_arguments" .= environmentRtsArguments environment,
        "benchmark_arguments" .= environmentBenchmarkArguments environment,
        "time_mode" .= environmentTimeMode environment,
        "run_timestamp" .= environmentRunTimestamp environment
      ]
  toEncoding environment =
    pairs
      ( "schema_version" .= environmentSchemaVersion environment
          <> "run_id" .= environmentRunIdentifier environment
          <> "environment_label" .= environmentLabel environment
          <> "git_revision" .= environmentGitRevision environment
          <> "git_dirty" .= environmentGitDirty environment
          <> "corpus_schema_version" .= environmentCorpusSchemaVersion environment
          <> "selected_cases" .= environmentSelectedCases environment
          <> "ghc_version" .= environmentGhcVersion environment
          <> "cabal_version" .= environmentCabalVersion environment
          <> "package_version" .= environmentPackageVersion environment
          <> "operating_system" .= environmentOperatingSystem environment
          <> "architecture" .= environmentArchitecture environment
          <> "cpu_identity" .= environmentCpuIdentity environment
          <> "build_mode" .= environmentBuildMode environment
          <> "rts_capabilities" .= environmentRtsCapabilities environment
          <> "rts_arguments" .= environmentRtsArguments environment
          <> "benchmark_arguments" .= environmentBenchmarkArguments environment
          <> "time_mode" .= environmentTimeMode environment
          <> "run_timestamp" .= environmentRunTimestamp environment
      )

instance FromJSON BenchmarkEnvironment where
  parseJSON = withObject "BenchmarkEnvironment" $ \environment ->
    BenchmarkEnvironment
      <$> environment .: "schema_version"
      <*> environment .: "run_id"
      <*> environment .: "environment_label"
      <*> environment .: "git_revision"
      <*> environment .: "git_dirty"
      <*> environment .: "corpus_schema_version"
      <*> environment .: "selected_cases"
      <*> environment .: "ghc_version"
      <*> environment .: "cabal_version"
      <*> environment .: "package_version"
      <*> environment .: "operating_system"
      <*> environment .: "architecture"
      <*> environment .: "cpu_identity"
      <*> environment .: "build_mode"
      <*> environment .: "rts_capabilities"
      <*> environment .: "rts_arguments"
      <*> environment .: "benchmark_arguments"
      <*> environment .: "time_mode"
      <*> environment .: "run_timestamp"

benchmarkEnvironmentJson :: BenchmarkEnvironment -> LazyByteString.ByteString
benchmarkEnvironmentJson = encodingToLazyByteString . toEncoding

validateEnvironmentLabel :: Text -> Either Text Text
validateEnvironmentLabel = validatePathSegment "environment label" 64

benchmarkArtifactPaths :: FilePath -> Text -> Text -> Either Text BenchmarkArtifactPaths
benchmarkArtifactPaths resultRoot label runIdentifier = do
  validLabel <- validateEnvironmentLabel label
  validRunIdentifier <- validatePathSegment "run identifier" 80 runIdentifier
  let artifactDirectory = resultRoot </> Text.unpack validLabel </> Text.unpack validRunIdentifier
  pure
    BenchmarkArtifactPaths
      { benchmarkArtifactLabel = validLabel,
        benchmarkArtifactRunIdentifier = validRunIdentifier,
        benchmarkArtifactDirectory = artifactDirectory,
        benchmarkArtifactResultsCsv = artifactDirectory </> "results.csv",
        benchmarkArtifactEnvironmentJson = artifactDirectory </> "environment.json"
      }

createBenchmarkArtifactDirectory :: BenchmarkArtifactPaths -> IO ()
createBenchmarkArtifactDirectory = createDirectoryIfMissing True . benchmarkArtifactDirectory

writeBenchmarkEnvironment :: BenchmarkArtifactPaths -> BenchmarkEnvironment -> IO ()
writeBenchmarkEnvironment artifactPaths environment
  | benchmarkArtifactLabel artifactPaths /= environmentLabel environment =
      ioError (userError "benchmark artifact label does not match environment metadata")
  | benchmarkArtifactRunIdentifier artifactPaths /= environmentRunIdentifier environment =
      ioError (userError "benchmark artifact run identifier does not match environment metadata")
  | otherwise = do
      createBenchmarkArtifactDirectory artifactPaths
      LazyByteString.writeFile
        (benchmarkArtifactEnvironmentJson artifactPaths)
        (benchmarkEnvironmentJson environment)

benchmarkRunIdentity :: IO (Text, Text)
benchmarkRunIdentity = do
  timestamp <- getCurrentTime
  pure
    ( Text.pack (formatTime defaultTimeLocale "%Y%m%dT%H%M%S%qZ" timestamp),
      renderRunTimestamp timestamp
    )

captureBenchmarkEnvironment :: BenchmarkEnvironmentCapture -> IO BenchmarkEnvironment
captureBenchmarkEnvironment capture = do
  gitRevision <- captureProcessFact "Git revision" False "git" ["-C", capturePackageRoot capture, "rev-parse", "HEAD"]
  gitStatus <- captureProcessFact "Git status" True "git" ["-C", capturePackageRoot capture, "status", "--porcelain"]
  cabalVersion <- captureProcessFact "Cabal version" False "cabal" ["--numeric-version"]
  cpuIdentity <- captureCpuIdentity
  capabilities <- getNumCapabilities
  fullArguments <- getFullArgs
  pure
    BenchmarkEnvironment
      { environmentSchemaVersion = 2,
        environmentRunIdentifier = captureRunIdentifier capture,
        environmentLabel = captureEnvironmentLabel capture,
        environmentGitRevision = gitRevision,
        environmentGitDirty = mapEnvironmentFact (not . Text.null) gitStatus,
        environmentCorpusSchemaVersion = captureCorpusSchemaVersion capture,
        environmentSelectedCases = sort (captureSelectedCases capture),
        environmentGhcVersion = Text.pack (showVersion SystemInfo.compilerVersion),
        environmentCabalVersion = cabalVersion,
        environmentPackageVersion = Text.pack (showVersion version),
        environmentOperatingSystem = Text.pack SystemInfo.os,
        environmentArchitecture = Text.pack SystemInfo.arch,
        environmentCpuIdentity = cpuIdentity,
        environmentBuildMode = captureBuildMode capture,
        environmentRtsCapabilities = capabilities,
        environmentRtsArguments = map Text.pack (extractRtsArguments fullArguments),
        environmentBenchmarkArguments = map Text.pack (captureBenchmarkArguments capture),
        environmentTimeMode = captureTimeMode capture,
        environmentRunTimestamp = captureRunTimestamp capture
      }

benchmarkTimeModeFromArguments :: [String] -> Either Text BenchmarkTimeMode
benchmarkTimeModeFromArguments = go CpuBenchmarkTime
  where
    go current arguments =
      case arguments of
        [] -> Right current
        ["--time-mode"] -> Left "--time-mode requires a value"
        "--time-mode" : value : remaining -> parseMode value >>= \timeMode -> go timeMode remaining
        argument : remaining ->
          case Text.stripPrefix "--time-mode=" (Text.pack argument) of
            Just value -> parseMode (Text.unpack value) >>= \timeMode -> go timeMode remaining
            Nothing -> go current remaining
    parseMode value =
      case benchmarkTimeModeFromName (Text.pack value) of
        Just timeMode -> Right timeMode
        Nothing -> Left ("unknown benchmark time mode: " <> Text.pack value)

checkBenchmarkCompatibility ::
  CompatibilityPolicy ->
  BenchmarkEnvironment ->
  BenchmarkEnvironment ->
  Either [CompatibilityMismatch] CompatibilityDecision
checkBenchmarkCompatibility policy baseline candidate =
  case benchmarkCompatibilityMismatches baseline candidate of
    [] -> Right CompatibleBenchmarks
    mismatches ->
      case policy of
        RequireCompatible -> Left mismatches
        AllowIncompatible -> Right (IncompatibleBenchmarksAllowed mismatches)

benchmarkCompatibilityMismatches :: BenchmarkEnvironment -> BenchmarkEnvironment -> [CompatibilityMismatch]
benchmarkCompatibilityMismatches baseline candidate =
  concat
    [ mismatch MetadataSchemaField environmentSchemaVersion,
      mismatch EnvironmentLabelField environmentLabel,
      mismatch CorpusSchemaField environmentCorpusSchemaVersion,
      mismatch SelectedCasesField environmentSelectedCases,
      mismatch GhcVersionField environmentGhcVersion,
      mismatch CabalVersionField environmentCabalVersion,
      mismatch PackageVersionField environmentPackageVersion,
      mismatch OperatingSystemField environmentOperatingSystem,
      mismatch ArchitectureField environmentArchitecture,
      mismatch CpuIdentityField environmentCpuIdentity,
      mismatch BuildModeField environmentBuildMode,
      mismatch RtsCapabilitiesField environmentRtsCapabilities,
      mismatch RtsArgumentsField environmentRtsArguments,
      mismatch BenchmarkArgumentsField environmentBenchmarkArguments,
      mismatch TimeModeField environmentTimeMode
    ]
  where
    mismatch :: (Eq value, Show value) => CompatibilityField -> (BenchmarkEnvironment -> value) -> [CompatibilityMismatch]
    mismatch field project
      | project baseline == project candidate = []
      | otherwise =
          [ CompatibilityMismatch
              { compatibilityMismatchField = field,
                compatibilityBaselineValue = Text.pack (show (project baseline)),
                compatibilityCandidateValue = Text.pack (show (project candidate))
              }
          ]

benchmarkBuildModeName :: BenchmarkBuildMode -> Text
benchmarkBuildModeName buildMode =
  case buildMode of
    OptimizedBenchmarkBuild -> "optimized"
    ProfilingBenchmarkBuild -> "profiling"

benchmarkBuildModeFromName :: Text -> Maybe BenchmarkBuildMode
benchmarkBuildModeFromName name =
  case name of
    "optimized" -> Just OptimizedBenchmarkBuild
    "profiling" -> Just ProfilingBenchmarkBuild
    _ -> Nothing

benchmarkBuildModeForProfiling :: Bool -> BenchmarkBuildMode
benchmarkBuildModeForProfiling profilingEnabled =
  if profilingEnabled
    then ProfilingBenchmarkBuild
    else OptimizedBenchmarkBuild

benchmarkTimeModeName :: BenchmarkTimeMode -> Text
benchmarkTimeModeName timeMode =
  case timeMode of
    CpuBenchmarkTime -> "cpu"
    WallBenchmarkTime -> "wall"
    MutatorCpuBenchmarkTime -> "mutcpu"
    MutatorWallBenchmarkTime -> "mutwall"

benchmarkTimeModeFromName :: Text -> Maybe BenchmarkTimeMode
benchmarkTimeModeFromName name =
  case name of
    "cpu" -> Just CpuBenchmarkTime
    "wall" -> Just WallBenchmarkTime
    "mutcpu" -> Just MutatorCpuBenchmarkTime
    "mutwall" -> Just MutatorWallBenchmarkTime
    _ -> Nothing

validatePathSegment :: Text -> Int -> Text -> Either Text Text
validatePathSegment description maximumLength value
  | Text.null value = Left (description <> " must not be empty")
  | Text.length value > maximumLength = Left (description <> " is too long")
  | not (isAsciiAlphaNumeric (Text.head value)) = Left (description <> " must start with an ASCII letter or digit")
  | Text.all isSafeCharacter value = Right value
  | otherwise = Left (description <> " may contain only ASCII letters, digits, '.', '_', and '-'")
  where
    isSafeCharacter character = isAsciiAlphaNumeric character || character `elem` ("._-" :: String)
    isAsciiAlphaNumeric character = isAscii character && isAlphaNum character

renderRunTimestamp :: UTCTime -> Text
renderRunTimestamp = Text.pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ"

captureProcessFact :: Text -> Bool -> FilePath -> [String] -> IO (EnvironmentFact Text)
captureProcessFact description allowEmpty executable arguments = do
  processResult <- try (readProcessWithExitCode executable arguments "")
  case processResult of
    Left exception ->
      pure
        ( UnavailableEnvironmentFact
            (description <> " is unavailable: " <> Text.pack (show (exception :: IOException)))
        )
    Right (ExitSuccess, output, _)
      | let value = Text.strip (Text.pack output),
        allowEmpty || not (Text.null value) ->
          pure (AvailableEnvironmentFact value)
    Right (exitCode, _, standardError) ->
      pure
        ( UnavailableEnvironmentFact
            ( description
                <> " command failed ("
                <> Text.pack (show exitCode)
                <> "): "
                <> Text.strip (Text.pack standardError)
            )
        )

mapEnvironmentFact :: (left -> right) -> EnvironmentFact left -> EnvironmentFact right
mapEnvironmentFact mapValue environmentFact =
  case environmentFact of
    AvailableEnvironmentFact value -> AvailableEnvironmentFact (mapValue value)
    UnavailableEnvironmentFact reason -> UnavailableEnvironmentFact reason

captureCpuIdentity :: IO (EnvironmentFact Text)
captureCpuIdentity =
  case SystemInfo.os of
    "darwin" -> captureProcessFact "CPU identity" False "sysctl" ["-n", "machdep.cpu.brand_string"]
    "linux" -> captureLinuxCpuIdentity
    operatingSystem -> pure (UnavailableEnvironmentFact ("CPU identity is not implemented for " <> Text.pack operatingSystem))

captureLinuxCpuIdentity :: IO (EnvironmentFact Text)
captureLinuxCpuIdentity = do
  let cpuInfoPath = "/proc/cpuinfo"
  exists <- doesFileExist cpuInfoPath
  if not exists
    then pure (UnavailableEnvironmentFact "/proc/cpuinfo is unavailable")
    else do
      cpuInfoResult <- try (TextIO.readFile cpuInfoPath)
      case cpuInfoResult of
        Left exception -> pure (UnavailableEnvironmentFact (Text.pack (show (exception :: IOException))))
        Right cpuInfo ->
          case [value | line <- Text.lines cpuInfo, Just value <- [linuxCpuModelName line]] of
            value : _ | not (Text.null value) -> pure (AvailableEnvironmentFact value)
            _ -> pure (UnavailableEnvironmentFact "model name is absent from /proc/cpuinfo")

linuxCpuModelName :: Text -> Maybe Text
linuxCpuModelName line =
  let (key, valueWithSeparator) = Text.breakOn ":" line
      value = Text.strip (Text.drop 1 valueWithSeparator)
   in if Text.strip key == "model name" && not (Text.null valueWithSeparator) && not (Text.null value)
        then Just value
        else Nothing

extractRtsArguments :: [String] -> [String]
extractRtsArguments arguments =
  case dropWhile (/= "+RTS") arguments of
    [] -> []
    _ : rtsArguments -> takeWhile (/= "-RTS") rtsArguments
