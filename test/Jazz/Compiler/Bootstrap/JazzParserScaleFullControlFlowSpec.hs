{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (void)
import Jazz.Compiler.Bootstrap.JazzParserScale
  ( runJazzParserControlFlowScale,
  )
import Jazz.Compiler.Bootstrap.JazzParserScaleAssertions
  ( assertScaleRun,
    fullControlFlowLimits,
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
    "JazzParserScaleFullControlFlow"
    [ ("parses the full generated control-flow program", testFullControlFlowScale)
    ]

testFullControlFlowScale :: IO ()
testFullControlFlowScale = do
  result <- runJazzParserControlFlowScale RuntimeObservationStatistics (scaleWorkloadBindingCount fullScaleWorkload)
  void $ assertScaleRun "full control-flow" (scaleWorkloadExpectedStatementCount fullScaleWorkload) fullControlFlowLimits result
