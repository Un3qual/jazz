{-# LANGUAGE OverloadedStrings #-}

module JazzNext.Compiler.Bootstrap.JazzParserScaleAssertions
  ( ScaleLimits (..),
    assertDeterministicScalePair,
    assertScaleRun,
    fullControlFlowLimits,
    fullDeclarationsLimits,
    fullExpressionLimits,
    fullOperatorLimits,
    smokeControlFlowLimits,
    smokeDeclarationsLimits,
    smokeExpressionLimits,
    smokeOperatorLimits,
  )
where

import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word64)
import JazzNext.Compiler.Driver
  ( RunResult (..),
    runCompileErrors,
    runRuntimeErrors,
    runRuntimeObservation,
  )
import JazzNext.Compiler.Runtime.Observation
  ( RuntimeObservationReport (runtimeObservationStatistics, runtimeObservationTermination),
    RuntimeStatistics (..),
    RuntimeTermination (RuntimeSucceeded),
  )
import JazzNext.TestHarness
  ( assertEqual,
    failTest,
  )

data ScaleLimits = ScaleLimits
  { scaleTransitionCeiling :: Word64,
    scaleApplicationCeiling :: Word64,
    scaleListCellCeiling :: Word64,
    scaleContinuationDepthCeiling :: Word64
  }

assertDeterministicScalePair :: Text -> Int -> ScaleLimits -> IO RunResult -> IO ()
assertDeterministicScalePair label expectedStatementCount limits run = do
  first <- run
  second <- run
  firstStatistics <- assertScaleRun (label <> " first") expectedStatementCount limits first
  secondStatistics <- assertScaleRun (label <> " second") expectedStatementCount limits second
  assertEqual (label <> " deterministic output") (runOutput first) (runOutput second)
  assertEqual (label <> " deterministic statistics") firstStatistics secondStatistics
  putStrLn ("SCALE_STATS " <> Text.unpack label <> " " <> show firstStatistics)

assertScaleRun :: Text -> Int -> ScaleLimits -> RunResult -> IO RuntimeStatistics
assertScaleRun label expectedStatementCount limits result = do
  assertEqual (label <> " compile errors") [] (runCompileErrors result)
  assertEqual (label <> " runtime errors") [] (runRuntimeErrors result)
  assertEqual
    (label <> " structured statement count")
    (Just (Text.pack (show expectedStatementCount)))
    (runOutput result)
  report <- requireObservation label result
  assertEqual (label <> " termination") RuntimeSucceeded (runtimeObservationTermination report)
  let statistics = runtimeObservationStatistics report
  assertEqual (label <> " host operations") 0 (runtimeHostOperations statistics)
  assertAtMost (label <> " evaluator transitions") (scaleTransitionCeiling limits) (runtimeEvaluatorTransitions statistics)
  assertAtMost (label <> " applications") (scaleApplicationCeiling limits) (runtimeApplications statistics)
  assertAtMost (label <> " list cells") (scaleListCellCeiling limits) (runtimeListCellsConstructed statistics)
  assertAtMost (label <> " continuation depth") (scaleContinuationDepthCeiling limits) (runtimeMaximumContinuationDepth statistics)
  pure statistics

smokeExpressionLimits :: ScaleLimits
smokeExpressionLimits =
  ScaleLimits
    { scaleTransitionCeiling = 3000000,
      scaleApplicationCeiling = 350000,
      scaleListCellCeiling = 15000,
      scaleContinuationDepthCeiling = 200
    }

smokeDeclarationsLimits :: ScaleLimits
smokeDeclarationsLimits =
  ScaleLimits
    { scaleTransitionCeiling = 1300000,
      scaleApplicationCeiling = 150000,
      scaleListCellCeiling = 8000,
      scaleContinuationDepthCeiling = 200
    }

smokeControlFlowLimits :: ScaleLimits
smokeControlFlowLimits =
  ScaleLimits
    { scaleTransitionCeiling = 5500000,
      scaleApplicationCeiling = 700000,
      scaleListCellCeiling = 28000,
      scaleContinuationDepthCeiling = 225
    }

smokeOperatorLimits :: ScaleLimits
smokeOperatorLimits =
  ScaleLimits
    { scaleTransitionCeiling = 6500000,
      scaleApplicationCeiling = 800000,
      scaleListCellCeiling = 25000,
      scaleContinuationDepthCeiling = 250
    }

fullExpressionLimits :: ScaleLimits
fullExpressionLimits =
  ScaleLimits
    { scaleTransitionCeiling = 22000000,
      scaleApplicationCeiling = 2700000,
      scaleListCellCeiling = 115000,
      scaleContinuationDepthCeiling = 1100
    }

fullDeclarationsLimits :: ScaleLimits
fullDeclarationsLimits =
  ScaleLimits
    { scaleTransitionCeiling = 80000000,
      scaleApplicationCeiling = 10000000,
      scaleListCellCeiling = 500000,
      scaleContinuationDepthCeiling = 1100
    }

fullControlFlowLimits :: ScaleLimits
fullControlFlowLimits =
  ScaleLimits
    { scaleTransitionCeiling = 45000000,
      scaleApplicationCeiling = 5500000,
      scaleListCellCeiling = 225000,
      scaleContinuationDepthCeiling = 1100
    }

fullOperatorLimits :: ScaleLimits
fullOperatorLimits =
  ScaleLimits
    { scaleTransitionCeiling = 52000000,
      scaleApplicationCeiling = 6300000,
      scaleListCellCeiling = 190000,
      scaleContinuationDepthCeiling = 1150
    }

requireObservation :: Text -> RunResult -> IO RuntimeObservationReport
requireObservation label result =
  case runRuntimeObservation result of
    Nothing -> failTest (label <> " did not produce runtime statistics")
    Just report -> pure report

assertAtMost :: (Ord value, Show value) => Text -> value -> value -> IO ()
assertAtMost label limit actual =
  if actual <= limit
    then pure ()
    else failTest (label <> " exceeded ceiling " <> showText limit <> ": " <> showText actual)

showText :: (Show value) => value -> Text
showText = Text.pack . show
