{-# LANGUAGE OverloadedStrings #-}

module JazzNext.Benchmark.Stages
  ( runBenchmarkMain,
    runBenchmarkMainWithArguments,
  )
where

import Control.Exception (evaluate)
import Control.Monad (forM_, void, (<=<))
import Data.List (find)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import JazzNext.Benchmark.Force
  ( forceCompiledProgram,
    forceCompiledProgramResult,
    forceExpr,
    forceInferenceResult,
    forceProgramCaseResult,
    forceRuntimeProgramResult,
    forceSurfaceExpr,
    forceTokens,
  )
import JazzNext.Benchmark.Metadata
  ( BenchmarkArtifactPaths (..),
    BenchmarkBuildMode (OptimizedBenchmarkBuild),
    BenchmarkEnvironment,
    BenchmarkEnvironmentCapture (..),
    benchmarkArtifactPaths,
    benchmarkRunIdentity,
    benchmarkTimeModeFromArguments,
    captureBenchmarkEnvironment,
    createBenchmarkArtifactDirectory,
    validateEnvironmentLabel,
    writeBenchmarkEnvironment,
  )
import JazzNext.Compiler.AST (Expr)
import JazzNext.Compiler.Diagnostics (Diagnostic, renderDiagnostic)
import JazzNext.Compiler.ModuleInterface (CompiledProgram (compiledProgramErrors))
import JazzNext.Compiler.ModuleRuntime (evaluateCompiledProgram)
import JazzNext.Compiler.Parser (parseSurfaceProgramTokens)
import JazzNext.Compiler.Parser.Lexer (tokenize)
import JazzNext.Compiler.Parser.Lower (lowerSurfaceExpr)
import JazzNext.Compiler.Profiling
  ( BenchmarkGroup (..),
    CompilerStage (..),
    benchmarkGroupName,
    withCompilerStage,
  )
import JazzNext.Compiler.SourceProgram (parseAndLowerStandaloneSource)
import JazzNext.Compiler.TypeInference (inferExpressionDefault)
import JazzNext.ProgramCorpus.Manifest
  ( loadProgramCorpus,
    renderProgramCorpusViolation,
  )
import JazzNext.ProgramCorpus.Runner
  ( ProgramCaseResult (..),
    loadProgramCaseEntrySource,
    prepareProgramCase,
    runProgramCase,
  )
import JazzNext.ProgramCorpus.Types
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
    nfIO,
  )
import Test.Tasty.Ingredients
  ( Ingredient (..),
    composeReporters,
  )

data PreparedCase = PreparedCase
  { preparedProgramCase :: ProgramCase,
    preparedEntrySource :: Text,
    preparedLoweredEntry :: Expr,
    preparedCompiledProgram :: CompiledProgram
  }

data BenchmarkCommand = BenchmarkCommand
  { benchmarkCommandSmoke :: Bool,
    benchmarkCommandEnvironmentLabel :: Maybe Text,
    benchmarkCommandResultRoot :: Maybe FilePath,
    benchmarkCommandSelectedCases :: [Text],
    benchmarkCommandForwardedArguments :: [String]
  }

runBenchmarkMain :: IO ()
runBenchmarkMain = getArgs >>= runBenchmarkMainWithArguments

runBenchmarkMainWithArguments :: [String] -> IO ()
runBenchmarkMainWithArguments arguments = do
  benchmarkCommand <- fromEitherText (parseBenchmarkCommand arguments)
  (corpus, preparedCases) <- loadPreparedCases
  selectedCases <-
    fromEitherText
      (selectPreparedCases (benchmarkCommandSelectedCases benchmarkCommand) preparedCases)
  if benchmarkCommandSmoke benchmarkCommand
    then runSmoke selectedCases
    else case benchmarkCommandEnvironmentLabel benchmarkCommand of
      Nothing ->
        withArgs
          (benchmarkCommandForwardedArguments benchmarkCommand)
          (defaultMain (benchmarkTree selectedCases))
      Just environmentLabel ->
        runRecordedBenchmarks
          benchmarkCommand
          environmentLabel
          (programCorpusSchemaVersion corpus)
          selectedCases

loadPreparedCases :: IO (ProgramCorpus, [PreparedCase])
loadPreparedCases = do
  corpusResult <- loadProgramCorpus
  corpus <-
    case corpusResult of
      Left violations ->
        ioError
          ( userError
              ( Text.unpack
                  (Text.unlines (map renderProgramCorpusViolation violations))
              )
          )
      Right value -> pure value
  preparedCases <- mapM prepareCase (programCorpusCases corpus)
  pure (corpus, preparedCases)

prepareCase :: ProgramCase -> IO PreparedCase
prepareCase programCase = do
  source <-
    withCompilerStage SourceLoadingStage $ do
      loadedSource <- loadProgramCaseEntrySource programCase
      _ <- evaluate (Text.length loadedSource)
      pure loadedSource
  lowered <-
    case parseAndLowerStandaloneSource source of
      Left diagnostic -> ioError (userError (Text.unpack (renderDiagnostic diagnostic)))
      Right value -> evaluate (forceExpr value) >> pure value
  compiledResult <- prepareProgramCase programCase
  compiled <-
    case compiledResult of
      Left diagnostic -> ioError (userError (Text.unpack (renderDiagnostic diagnostic)))
      Right value -> do
        evaluate (forceCompiledProgram value)
        case compiledProgramErrors value of
          [] -> pure value
          diagnostic : _ -> ioError (userError (Text.unpack (renderDiagnostic diagnostic)))
  pure
    PreparedCase
      { preparedProgramCase = programCase,
        preparedEntrySource = source,
        preparedLoweredEntry = lowered,
        preparedCompiledProgram = compiled
      }

benchmarkTree :: [PreparedCase] -> [Benchmark]
benchmarkTree preparedCases =
  [ bgroup
      "jazz-next"
      [ bgroup
          (Text.unpack (benchmarkGroupName benchmarkGroup))
          [ bench
              (Text.unpack (programCaseIdentifier (preparedProgramCase preparedCase)))
              (nfIO (runStage benchmarkGroup preparedCase))
          | preparedCase <- preparedCases,
            benchmarkGroup `elem` programCaseBenchmarks (preparedProgramCase preparedCase)
          ]
      | benchmarkGroup <- [minBound .. maxBound]
      ]
  ]

runSmoke :: [PreparedCase] -> IO ()
runSmoke preparedCases =
  forM_ ([minBound .. maxBound] :: [BenchmarkGroup]) $ \benchmarkGroup ->
    case find (isFastParticipant benchmarkGroup) preparedCases of
      Nothing ->
        ioError
          ( userError
              ( "no fast corpus case participates in benchmark group: "
                  <> Text.unpack (benchmarkGroupName benchmarkGroup)
              )
          )
      Just preparedCase -> do
        TextIO.putStrLn
          ( "SMOKE "
              <> benchmarkGroupName benchmarkGroup
              <> "/"
              <> programCaseIdentifier (preparedProgramCase preparedCase)
          )
        runStage benchmarkGroup preparedCase

isFastParticipant :: BenchmarkGroup -> PreparedCase -> Bool
isFastParticipant benchmarkGroup preparedCase =
  let programCase = preparedProgramCase preparedCase
   in programCaseWorkload programCase == FastWorkload
        && benchmarkGroup `elem` programCaseBenchmarks programCase

runStage :: BenchmarkGroup -> PreparedCase -> IO ()
runStage benchmarkGroup preparedCase =
  case benchmarkGroup of
    ParseLowerBenchmark -> do
      tokens <-
        withCompilerStage LexingStage $ do
          tokenResult <- evaluate (tokenize (preparedEntrySource preparedCase))
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
        evaluate (forceExpr expression)
    AnalysisBenchmark ->
      withCompilerStage TypeInferenceStage $ do
        inference <- inferExpressionDefault (preparedLoweredEntry preparedCase)
        evaluate (forceInferenceResult inference)
    ModulePreparationBenchmark ->
      withCompilerStage RuntimePreparationStage $ do
        compiledResult <- prepareProgramCase (preparedProgramCase preparedCase)
        evaluate (forceCompiledProgramResult compiledResult)
    RuntimeBenchmark ->
      withCompilerStage EvaluationStage $
        evaluate
          ( forceRuntimeProgramResult
              (evaluateCompiledProgram (preparedCompiledProgram preparedCase))
          )
    WholeProgramBenchmark -> do
      result <- runProgramCase (preparedProgramCase preparedCase)
      evaluate (forceProgramCaseResult result)
      requireExpectedResult (preparedProgramCase preparedCase) result

failBenchmarkDiagnostic :: Diagnostic -> IO value
failBenchmarkDiagnostic diagnostic =
  ioError (userError (Text.unpack (renderDiagnostic diagnostic)))

requireExpectedResult :: ProgramCase -> ProgramCaseResult -> IO ()
requireExpectedResult programCase result
  | programCaseResultTermination result == programCaseExpectedTermination programCase,
    programCaseResultStdout result == programCaseExpectedStdout programCase =
      pure ()
  | otherwise =
      ioError
        ( userError
            ( "benchmark case did not preserve expected behavior: "
                <> Text.unpack (programCaseIdentifier programCase)
            )
        )

runRecordedBenchmarks :: BenchmarkCommand -> Text -> Int -> [PreparedCase] -> IO ()
runRecordedBenchmarks benchmarkCommand environmentLabel corpusSchemaVersion preparedCases = do
  preparedCase <-
    case preparedCases of
      [] -> ioError (userError "no corpus cases were selected for the recorded benchmark")
      value : _ -> pure value
  timeMode <-
    fromEitherText
      (benchmarkTimeModeFromArguments (benchmarkCommandForwardedArguments benchmarkCommand))
  (runIdentifier, runTimestamp) <- benchmarkRunIdentity
  let packageRoot = programCasePackageRoot (preparedProgramCase preparedCase)
      resultRoot =
        case benchmarkCommandResultRoot benchmarkCommand of
          Nothing -> packageRoot </> "benchmark-results"
          Just configuredRoot -> configuredRoot
      selectedCaseIdentifiers =
        map (programCaseIdentifier . preparedProgramCase) preparedCases
  artifactPaths <-
    fromEitherText (benchmarkArtifactPaths resultRoot environmentLabel runIdentifier)
  environment <-
    captureBenchmarkEnvironment
      BenchmarkEnvironmentCapture
        { capturePackageRoot = packageRoot,
          captureRunIdentifier = runIdentifier,
          captureEnvironmentLabel = environmentLabel,
          captureCorpusSchemaVersion = corpusSchemaVersion,
          captureSelectedCases = selectedCaseIdentifiers,
          captureBuildMode = OptimizedBenchmarkBuild,
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
        (bgroup "All" (benchmarkTree preparedCases))
    )

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
        "--jazz-smoke" : remaining ->
          go (benchmarkCommand {benchmarkCommandSmoke = True}) remaining
        "--environment-label" : value : remaining ->
          setEnvironmentLabel benchmarkCommand (Text.pack value) >>= \updated -> go updated remaining
        "--result-root" : value : remaining ->
          setResultRoot benchmarkCommand value >>= \updated -> go updated remaining
        "--jazz-case" : value : remaining ->
          addSelectedCase benchmarkCommand (Text.pack value) >>= \updated -> go updated remaining
        argument : remaining
          | Just value <- Text.stripPrefix "--environment-label=" (Text.pack argument) ->
              setEnvironmentLabel benchmarkCommand value >>= \updated -> go updated remaining
          | Just value <- Text.stripPrefix "--result-root=" (Text.pack argument) ->
              setResultRoot benchmarkCommand (Text.unpack value) >>= \updated -> go updated remaining
          | Just value <- Text.stripPrefix "--jazz-case=" (Text.pack argument) ->
              addSelectedCase benchmarkCommand value >>= \updated -> go updated remaining
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
        else Right benchmarkCommand

emptyBenchmarkCommand :: BenchmarkCommand
emptyBenchmarkCommand =
  BenchmarkCommand
    { benchmarkCommandSmoke = False,
      benchmarkCommandEnvironmentLabel = Nothing,
      benchmarkCommandResultRoot = Nothing,
      benchmarkCommandSelectedCases = [],
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

selectPreparedCases :: [Text] -> [PreparedCase] -> Either Text [PreparedCase]
selectPreparedCases requestedIdentifiers preparedCases
  | null requestedIdentifiers = Right preparedCases
  | not (null missingIdentifiers) =
      Left ("unknown corpus case(s): " <> Text.intercalate ", " missingIdentifiers)
  | otherwise =
      Right
        ( filter
            ((`elem` requestedIdentifiers) . programCaseIdentifier . preparedProgramCase)
            preparedCases
        )
  where
    knownIdentifiers = map (programCaseIdentifier . preparedProgramCase) preparedCases
    missingIdentifiers = filter (`notElem` knownIdentifiers) requestedIdentifiers

fromEitherText :: Either Text value -> IO value
fromEitherText value =
  case value of
    Left message -> ioError (userError (Text.unpack message))
    Right result -> pure result
