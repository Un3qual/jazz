{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import JazzNext.Compiler.Bootstrap.JazzParserScale
  ( runJazzParserControlFlowScale,
  )
import JazzNext.Compiler.Bootstrap.JazzParserScaleAssertions
  ( assertScaleRun,
    fullControlFlowLimits,
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
    "JazzParserScaleFullControlFlow"
    [ ("parses the full generated control-flow program", testFullControlFlowScale)
    ]

testFullControlFlowScale :: IO ()
testFullControlFlowScale = do
  result <- runJazzParserControlFlowScale RuntimeObservationStatistics (scaleWorkloadBindingCount fullScaleWorkload)
  statistics <- assertScaleRun "full control-flow" (scaleWorkloadExpectedStatementCount fullScaleWorkload) fullControlFlowLimits result
  putStrLn ("SCALE_STATS full-control-flow " <> show statistics)
