{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import JazzNext.Compiler.Bootstrap.JazzParserScale
  ( runJazzParserOperatorScale,
  )
import JazzNext.Compiler.Bootstrap.JazzParserScaleAssertions
  ( assertScaleRun,
    fullOperatorLimits,
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
    "JazzParserScaleFullOperator"
    [ ("parses the full generated operator program", testFullOperatorScale)
    ]

testFullOperatorScale :: IO ()
testFullOperatorScale = do
  result <- runJazzParserOperatorScale RuntimeObservationStatistics (scaleWorkloadBindingCount fullScaleWorkload)
  statistics <- assertScaleRun "full operator" (scaleWorkloadExpectedStatementCount fullScaleWorkload) fullOperatorLimits result
  putStrLn ("SCALE_STATS full-operator " <> show statistics)
