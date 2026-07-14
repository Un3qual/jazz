{-# LANGUAGE OverloadedStrings #-}

module JazzNext.Benchmark.Stages
  ( runBenchmarkMain,
  )
where

import Control.Exception (evaluate)
import Control.Monad (forM_)
import Data.List (find)
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
  )
import JazzNext.Compiler.AST (Expr)
import JazzNext.Compiler.Diagnostics (renderDiagnostic)
import JazzNext.Compiler.ModuleInterface (CompiledProgram (compiledProgramErrors))
import JazzNext.Compiler.ModuleRuntime (evaluateCompiledProgram)
import JazzNext.Compiler.Profiling
  ( BenchmarkGroup (..),
    benchmarkGroupName,
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
import Test.Tasty.Bench
  ( Benchmark,
    bench,
    bgroup,
    defaultMain,
    nfIO,
  )

data PreparedCase = PreparedCase
  { preparedProgramCase :: ProgramCase,
    preparedEntrySource :: Text,
    preparedLoweredEntry :: Expr,
    preparedCompiledProgram :: CompiledProgram
  }

runBenchmarkMain :: IO ()
runBenchmarkMain = do
  arguments <- getArgs
  preparedCases <- loadPreparedCases
  if "--jazz-smoke" `elem` arguments
    then runSmoke preparedCases
    else withArgs (filter (/= "--jazz-smoke") arguments) (defaultMain (benchmarkTree preparedCases))

loadPreparedCases :: IO [PreparedCase]
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
  mapM prepareCase (programCorpusCases corpus)

prepareCase :: ProgramCase -> IO PreparedCase
prepareCase programCase = do
  source <- loadProgramCaseEntrySource programCase
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
    ParseLowerBenchmark ->
      evaluate
        ( case parseAndLowerStandaloneSource (preparedEntrySource preparedCase) of
            Left diagnostic -> renderDiagnostic diagnostic `seq` ()
            Right expression -> forceExpr expression
        )
    AnalysisBenchmark -> do
      inference <- inferExpressionDefault (preparedLoweredEntry preparedCase)
      evaluate (forceInferenceResult inference)
    ModulePreparationBenchmark -> do
      compiledResult <- prepareProgramCase (preparedProgramCase preparedCase)
      evaluate (forceCompiledProgramResult compiledResult)
    RuntimeBenchmark ->
      evaluate
        ( forceRuntimeProgramResult
            (evaluateCompiledProgram (preparedCompiledProgram preparedCase))
        )
    WholeProgramBenchmark -> do
      result <- runProgramCase (preparedProgramCase preparedCase)
      evaluate (forceProgramCaseResult result)
      requireExpectedResult (preparedProgramCase preparedCase) result

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
