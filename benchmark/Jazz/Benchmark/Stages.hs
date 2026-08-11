{-# LANGUAGE OverloadedStrings #-}

module Jazz.Benchmark.Stages
  ( BenchmarkCommand (benchmarkCommandSelectedCases, benchmarkCommandSelectedScaleCases),
    parseBenchmarkCommand,
    runBenchmarkMain,
    runBenchmarkMainWithEnvironmentCapture,
    runBenchmarkMainWithArguments,
  )
where

import Control.Monad (forM_, void, (<=<))
import Data.List (find)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Jazz.Benchmark.Metadata
  ( BenchmarkArtifactPaths (..),
    BenchmarkEnvironment,
    BenchmarkEnvironmentCapture (..),
    benchmarkArtifactPaths,
    benchmarkBuildModeForProfiling,
    benchmarkRunIdentity,
    benchmarkTimeModeFromArguments,
    captureBenchmarkEnvironment,
    createBenchmarkArtifactDirectory,
    validateEnvironmentLabel,
    writeBenchmarkEnvironment,
  )
import Jazz.Benchmark.ScaleCases
  ( CompilerScaleCase,
    compilerScaleCaseBenchmarks,
    compilerScaleCaseIdentifier,
    compilerScaleCases,
    selectCompilerScaleCases,
  )
import Jazz.Benchmark.StageInputs
  ( prepareBenchmark,
    prepareCompilerScaleBenchmark,
    runPreparedBenchmark,
    runPreparedCompilerScaleBenchmark,
    selectProgramCases,
  )
import Jazz.Compiler.Profiling
  ( BenchmarkGroup (..),
    benchmarkGroupName,
    compilerProfilingEnabled,
  )
import Jazz.ProgramCorpus.Manifest
  ( loadProgramCorpus,
    renderProgramCorpusViolation,
  )
import Jazz.ProgramCorpus.Types
  ( ProgramCase (..),
    ProgramCorpus (..),
    WorkloadClass (FastWorkload),
  )
import System.Environment (getArgs, withArgs)
import System.FilePath ((</>))
import Test.Tasty (defaultMainWithIngredients)
import Test.Tasty.Bench
  ( Benchmark,
    bench,
    benchIngredients,
    bgroup,
    defaultMain,
    env,
    nfIO,
  )
import Test.Tasty.Ingredients
  ( Ingredient (..),
    composeReporters,
  )

data BenchmarkCommand = BenchmarkCommand
  { benchmarkCommandSmoke :: Bool,
    benchmarkCommandEnvironmentLabel :: Maybe Text,
    benchmarkCommandResultRoot :: Maybe FilePath,
    benchmarkCommandSelectedCases :: [Text],
    benchmarkCommandSelectedScaleCases :: [Text],
    benchmarkCommandForwardedArguments :: [String]
  }

runBenchmarkMain :: IO ()
runBenchmarkMain = getArgs >>= runBenchmarkMainWithArguments

runBenchmarkMainWithArguments :: [String] -> IO ()
runBenchmarkMainWithArguments =
  runBenchmarkMainWithEnvironmentCapture captureBenchmarkEnvironment

runBenchmarkMainWithEnvironmentCapture ::
  (BenchmarkEnvironmentCapture -> IO BenchmarkEnvironment) ->
  [String] ->
  IO ()
runBenchmarkMainWithEnvironmentCapture captureEnvironment arguments = do
  benchmarkCommand <- fromEitherText (parseBenchmarkCommand arguments)
  corpus <- loadCorpus
  case benchmarkCommandSelectedScaleCases benchmarkCommand of
    [] -> runCorpusBenchmarks captureEnvironment benchmarkCommand corpus
    selectedIdentifiers -> do
      selectedCases <-
        fromEitherText (selectCompilerScaleCases selectedIdentifiers compilerScaleCases)
      packageRoot <- corpusPackageRoot corpus
      runCompilerScaleBenchmarks
        captureEnvironment
        benchmarkCommand
        (programCorpusSchemaVersion corpus)
        packageRoot
        selectedCases

runCorpusBenchmarks ::
  (BenchmarkEnvironmentCapture -> IO BenchmarkEnvironment) ->
  BenchmarkCommand ->
  ProgramCorpus ->
  IO ()
runCorpusBenchmarks captureEnvironment benchmarkCommand corpus = do
  selectedCases <-
    fromEitherText
      (selectProgramCases (benchmarkCommandSelectedCases benchmarkCommand) (programCorpusCases corpus))
  if benchmarkCommandSmoke benchmarkCommand
    then runSmoke selectedCases
    else case benchmarkCommandEnvironmentLabel benchmarkCommand of
      Nothing ->
        withArgs
          (benchmarkCommandForwardedArguments benchmarkCommand)
          (defaultMain (benchmarkTree selectedCases))
      Just environmentLabel -> do
        packageRoot <- selectedProgramCasePackageRoot selectedCases
        runRecordedBenchmarks
          captureEnvironment
          benchmarkCommand
          environmentLabel
          (programCorpusSchemaVersion corpus)
          packageRoot
          (map programCaseIdentifier selectedCases)
          (benchmarkTree selectedCases)

runCompilerScaleBenchmarks ::
  (BenchmarkEnvironmentCapture -> IO BenchmarkEnvironment) ->
  BenchmarkCommand ->
  Int ->
  FilePath ->
  [CompilerScaleCase] ->
  IO ()
runCompilerScaleBenchmarks captureEnvironment benchmarkCommand corpusSchemaVersion packageRoot programCases =
  case benchmarkCommandEnvironmentLabel benchmarkCommand of
    Nothing ->
      withArgs
        (benchmarkCommandForwardedArguments benchmarkCommand)
        (defaultMain (compilerScaleBenchmarkTree programCases))
    Just environmentLabel ->
      runRecordedBenchmarks
        captureEnvironment
        benchmarkCommand
        environmentLabel
        corpusSchemaVersion
        packageRoot
        (map compilerScaleCaseIdentifier programCases)
        (compilerScaleBenchmarkTree programCases)

loadCorpus :: IO ProgramCorpus
loadCorpus = do
  corpusResult <- loadProgramCorpus
  case corpusResult of
    Left violations ->
      ioError
        ( userError
            ( Text.unpack
                (Text.unlines (map renderProgramCorpusViolation violations))
            )
        )
    Right corpus -> pure corpus

benchmarkTree :: [ProgramCase] -> [Benchmark]
benchmarkTree programCases =
  [ bgroup
      "jazz"
      [ bgroup
          (Text.unpack (benchmarkGroupName benchmarkGroup))
          [ env
              (prepareBenchmark benchmarkGroup programCase)
              ( \preparedBenchmark ->
                  bench
                    (Text.unpack (programCaseIdentifier programCase))
                    (nfIO (runPreparedBenchmark preparedBenchmark))
              )
          | programCase <- programCases,
            benchmarkGroup `elem` programCaseBenchmarks programCase
          ]
      | benchmarkGroup <- [minBound .. maxBound]
      ]
  ]

compilerScaleBenchmarkTree :: [CompilerScaleCase] -> [Benchmark]
compilerScaleBenchmarkTree programCases =
  [ bgroup
      "compiler-scale"
      [ bgroup
          (Text.unpack (benchmarkGroupName benchmarkGroup))
          [ env
              (prepareCompilerScaleBenchmark benchmarkGroup programCase)
              ( \preparedBenchmark ->
                  bench
                    (Text.unpack (compilerScaleCaseIdentifier programCase))
                    (nfIO (runPreparedCompilerScaleBenchmark preparedBenchmark))
              )
          | programCase <- programCases,
            benchmarkGroup `elem` compilerScaleCaseBenchmarks programCase
          ]
      | benchmarkGroup <- [minBound .. maxBound]
      ]
  ]

runSmoke :: [ProgramCase] -> IO ()
runSmoke programCases =
  forM_ ([minBound .. maxBound] :: [BenchmarkGroup]) $ \benchmarkGroup ->
    case find (isFastParticipant benchmarkGroup) programCases of
      Nothing
        | benchmarkGroup == TypedLoweringBenchmark ->
            case find
              ((== "typed-validation-handoff-0064") . compilerScaleCaseIdentifier)
              compilerScaleCases of
              Nothing ->
                ioError (userError "no generated case participates in typed-lowering smoke")
              Just programCase -> do
                TextIO.putStrLn
                  ( "SMOKE "
                      <> benchmarkGroupName benchmarkGroup
                      <> "/"
                      <> compilerScaleCaseIdentifier programCase
                  )
                prepareCompilerScaleBenchmark benchmarkGroup programCase
                  >>= runPreparedCompilerScaleBenchmark
      Nothing ->
        ioError
          ( userError
              ( "no fast corpus case participates in benchmark group: "
                  <> Text.unpack (benchmarkGroupName benchmarkGroup)
              )
          )
      Just programCase -> do
        TextIO.putStrLn
          ( "SMOKE "
              <> benchmarkGroupName benchmarkGroup
              <> "/"
              <> programCaseIdentifier programCase
          )
        prepareBenchmark benchmarkGroup programCase >>= runPreparedBenchmark

isFastParticipant :: BenchmarkGroup -> ProgramCase -> Bool
isFastParticipant benchmarkGroup programCase =
  programCaseWorkload programCase == FastWorkload
    && benchmarkGroup `elem` programCaseBenchmarks programCase

runRecordedBenchmarks ::
  (BenchmarkEnvironmentCapture -> IO BenchmarkEnvironment) ->
  BenchmarkCommand ->
  Text ->
  Int ->
  FilePath ->
  [Text] ->
  [Benchmark] ->
  IO ()
runRecordedBenchmarks captureEnvironment benchmarkCommand environmentLabel corpusSchemaVersion packageRoot selectedCaseIdentifiers benchmarks = do
  timeMode <-
    fromEitherText
      (benchmarkTimeModeFromArguments (benchmarkCommandForwardedArguments benchmarkCommand))
  (runIdentifier, runTimestamp) <- benchmarkRunIdentity
  let resultRoot =
        case benchmarkCommandResultRoot benchmarkCommand of
          Nothing -> packageRoot </> "benchmark-results"
          Just configuredRoot -> configuredRoot
  artifactPaths <-
    fromEitherText (benchmarkArtifactPaths resultRoot environmentLabel runIdentifier)
  environment <-
    captureEnvironment
      BenchmarkEnvironmentCapture
        { capturePackageRoot = packageRoot,
          captureRunIdentifier = runIdentifier,
          captureEnvironmentLabel = environmentLabel,
          captureCorpusSchemaVersion = corpusSchemaVersion,
          captureSelectedCases = selectedCaseIdentifiers,
          captureBuildMode = benchmarkBuildModeForProfiling compilerProfilingEnabled,
          captureBenchmarkArguments = benchmarkCommandForwardedArguments benchmarkCommand,
          captureTimeMode = timeMode,
          captureRunTimestamp = runTimestamp
        }
  createBenchmarkArtifactDirectory artifactPaths
  withArgs
    ( benchmarkCommandForwardedArguments benchmarkCommand
        <> ["--csv=" <> benchmarkArtifactResultsCsv artifactPaths]
    )
    ( defaultMainWithIngredients
        (benchmarkIngredientsWithFinalizer (finalizeRecordedBenchmarks artifactPaths environment))
        (bgroup "All" benchmarks)
    )

selectedProgramCasePackageRoot :: [ProgramCase] -> IO FilePath
selectedProgramCasePackageRoot programCases =
  case programCases of
    [] -> ioError (userError "no corpus cases were selected for the recorded benchmark")
    programCase : _ -> pure (programCasePackageRoot programCase)

corpusPackageRoot :: ProgramCorpus -> IO FilePath
corpusPackageRoot corpus =
  case programCorpusCases corpus of
    [] -> ioError (userError "compiler scale benchmarks require a non-empty corpus")
    programCase : _ -> pure (programCasePackageRoot programCase)

finalizeRecordedBenchmarks :: BenchmarkArtifactPaths -> BenchmarkEnvironment -> IO ()
finalizeRecordedBenchmarks artifactPaths environment = do
  writeBenchmarkEnvironment artifactPaths environment
  TextIO.putStrLn ("RECORDED " <> Text.pack (benchmarkArtifactDirectory artifactPaths))

benchmarkIngredientsWithFinalizer :: IO () -> [Ingredient]
benchmarkIngredientsWithFinalizer finalize = map addFinalizer benchIngredients
  where
    finalizer =
      TestReporter [] $ \_ _ ->
        Just $ \_ ->
          pure $ \_ -> do
            finalize
            pure True
    addFinalizer ingredient =
      case ingredient of
        TestReporter _ _ -> composeReporters ingredient finalizer
        TestManager _ _ -> ingredient

parseBenchmarkCommand :: [String] -> Either Text BenchmarkCommand
parseBenchmarkCommand = finalize <=< go emptyBenchmarkCommand
  where
    go benchmarkCommand arguments =
      case arguments of
        [] -> Right benchmarkCommand
        ["--environment-label"] -> Left "--environment-label requires a value"
        ["--result-root"] -> Left "--result-root requires a value"
        ["--jazz-case"] -> Left "--jazz-case requires a value"
        ["--jazz-scale-case"] -> Left "--jazz-scale-case requires a value"
        "--jazz-smoke" : remaining ->
          go (benchmarkCommand {benchmarkCommandSmoke = True}) remaining
        "--environment-label" : value : remaining ->
          setEnvironmentLabel benchmarkCommand (Text.pack value) >>= \updated -> go updated remaining
        "--result-root" : value : remaining ->
          setResultRoot benchmarkCommand value >>= \updated -> go updated remaining
        "--jazz-case" : value : remaining ->
          addSelectedCase benchmarkCommand (Text.pack value) >>= \updated -> go updated remaining
        "--jazz-scale-case" : value : remaining ->
          addSelectedScaleCase benchmarkCommand (Text.pack value) >>= \updated -> go updated remaining
        argument : remaining
          | Just value <- Text.stripPrefix "--environment-label=" (Text.pack argument) ->
              setEnvironmentLabel benchmarkCommand value >>= \updated -> go updated remaining
          | Just value <- Text.stripPrefix "--result-root=" (Text.pack argument) ->
              setResultRoot benchmarkCommand (Text.unpack value) >>= \updated -> go updated remaining
          | Just value <- Text.stripPrefix "--jazz-case=" (Text.pack argument) ->
              addSelectedCase benchmarkCommand value >>= \updated -> go updated remaining
          | Just value <- Text.stripPrefix "--jazz-scale-case=" (Text.pack argument) ->
              addSelectedScaleCase benchmarkCommand value >>= \updated -> go updated remaining
          | argument == "--csv" || "--csv=" `Text.isPrefixOf` Text.pack argument ->
              Left "use --environment-label to write an owned results.csv and environment.json pair"
          | otherwise ->
              go
                ( benchmarkCommand
                    { benchmarkCommandForwardedArguments =
                        benchmarkCommandForwardedArguments benchmarkCommand <> [argument]
                    }
                )
                remaining
    finalize benchmarkCommand = do
      case benchmarkCommandEnvironmentLabel benchmarkCommand of
        Nothing -> pure ()
        Just label -> void (validateEnvironmentLabel label)
      case (benchmarkCommandEnvironmentLabel benchmarkCommand, benchmarkCommandResultRoot benchmarkCommand) of
        (Nothing, Just _) -> Left "--result-root requires --environment-label"
        _ -> pure ()
      if benchmarkCommandSmoke benchmarkCommand && isJust (benchmarkCommandEnvironmentLabel benchmarkCommand)
        then Left "--jazz-smoke cannot write durable benchmark results"
        else pure ()
      if not (null (benchmarkCommandSelectedCases benchmarkCommand))
        && not (null (benchmarkCommandSelectedScaleCases benchmarkCommand))
        then Left "--jazz-case and --jazz-scale-case cannot be combined"
        else pure ()
      if benchmarkCommandSmoke benchmarkCommand
        && not (null (benchmarkCommandSelectedScaleCases benchmarkCommand))
        then Left "--jazz-smoke cannot select compiler scale cases"
        else Right benchmarkCommand

emptyBenchmarkCommand :: BenchmarkCommand
emptyBenchmarkCommand =
  BenchmarkCommand
    { benchmarkCommandSmoke = False,
      benchmarkCommandEnvironmentLabel = Nothing,
      benchmarkCommandResultRoot = Nothing,
      benchmarkCommandSelectedCases = [],
      benchmarkCommandSelectedScaleCases = [],
      benchmarkCommandForwardedArguments = []
    }

setEnvironmentLabel :: BenchmarkCommand -> Text -> Either Text BenchmarkCommand
setEnvironmentLabel benchmarkCommand label =
  case benchmarkCommandEnvironmentLabel benchmarkCommand of
    Just _ -> Left "--environment-label may be provided only once"
    Nothing -> do
      validLabel <- validateEnvironmentLabel label
      Right (benchmarkCommand {benchmarkCommandEnvironmentLabel = Just validLabel})

setResultRoot :: BenchmarkCommand -> FilePath -> Either Text BenchmarkCommand
setResultRoot benchmarkCommand resultRoot
  | null resultRoot = Left "--result-root must not be empty"
  | isJust (benchmarkCommandResultRoot benchmarkCommand) = Left "--result-root may be provided only once"
  | otherwise = Right (benchmarkCommand {benchmarkCommandResultRoot = Just resultRoot})

addSelectedCase :: BenchmarkCommand -> Text -> Either Text BenchmarkCommand
addSelectedCase benchmarkCommand identifier
  | Text.null identifier = Left "--jazz-case must not be empty"
  | identifier `elem` benchmarkCommandSelectedCases benchmarkCommand =
      Left ("duplicate --jazz-case: " <> identifier)
  | otherwise =
      Right
        ( benchmarkCommand
            { benchmarkCommandSelectedCases =
                benchmarkCommandSelectedCases benchmarkCommand <> [identifier]
            }
        )

addSelectedScaleCase :: BenchmarkCommand -> Text -> Either Text BenchmarkCommand
addSelectedScaleCase benchmarkCommand identifier
  | Text.null identifier = Left "--jazz-scale-case must not be empty"
  | identifier `elem` benchmarkCommandSelectedScaleCases benchmarkCommand =
      Left ("duplicate --jazz-scale-case: " <> identifier)
  | otherwise =
      Right
        ( benchmarkCommand
            { benchmarkCommandSelectedScaleCases =
                benchmarkCommandSelectedScaleCases benchmarkCommand <> [identifier]
            }
        )

fromEitherText :: Either Text value -> IO value
fromEitherText value =
  case value of
    Left message -> ioError (userError (Text.unpack message))
    Right result -> pure result
