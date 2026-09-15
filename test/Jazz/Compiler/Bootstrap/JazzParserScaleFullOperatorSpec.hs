{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (void)
import Jazz.Compiler.Bootstrap.JazzParserScale
  ( runJazzParserOperatorScale,
  )
import Jazz.Compiler.Bootstrap.JazzParserScaleAssertions
  ( assertScaleRun,
    fullOperatorLimits,
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
    "JazzParserScaleFullOperator"
    [ ("parses the full generated operator program", testFullOperatorScale)
    ]

testFullOperatorScale :: IO ()
testFullOperatorScale = do
  result <- runJazzParserOperatorScale RuntimeObservationStatistics (scaleWorkloadBindingCount fullScaleWorkload)
  void $ assertScaleRun "full operator" (scaleWorkloadExpectedStatementCount fullScaleWorkload) fullOperatorLimits result
