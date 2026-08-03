{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Jazz.Compiler.Bootstrap.JazzParserScale
  ( runJazzParserScale,
  )
import Jazz.Compiler.Bootstrap.JazzParserScaleAssertions
  ( assertScaleRun,
    fullExpressionLimits,
    fullScaleWorkload,
    scaleWorkloadBindingCount,
    scaleWorkloadExpectedStatementCount,
  )
import Jazz.Compiler.Runtime.Observation
  ( RuntimeObservationRequest (RuntimeObservationStatistics),
  )
import Jazz.TestHarness
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
