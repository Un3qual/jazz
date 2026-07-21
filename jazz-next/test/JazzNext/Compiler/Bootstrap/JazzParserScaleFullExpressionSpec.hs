{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import JazzNext.Compiler.Bootstrap.JazzParserScale
  ( runJazzParserScale,
  )
import JazzNext.Compiler.Bootstrap.JazzParserScaleAssertions
  ( assertScaleRun,
    fullExpressionLimits,
    fullScaleWorkload,
    scaleWorkloadBindingCount,
    scaleWorkloadExpectedStatementCount,
  )
import JazzNext.Compiler.Runtime.Observation
  ( RuntimeObservationRequest (RuntimeObservationStatistics),
  )
import JazzNext.TestHarness
  ( runTestSuite,
  )

main :: IO ()
main =
  runTestSuite
    "JazzParserScaleFullExpression"
    [ ("parses the full generated expression program", testFullExpressionScale)
    ]

testFullExpressionScale :: IO ()
testFullExpressionScale = do
  result <- runJazzParserScale RuntimeObservationStatistics (scaleWorkloadBindingCount fullScaleWorkload)
  statistics <- assertScaleRun "full expression" (scaleWorkloadExpectedStatementCount fullScaleWorkload) fullExpressionLimits result
  putStrLn ("SCALE_STATS full-expression " <> show statistics)
