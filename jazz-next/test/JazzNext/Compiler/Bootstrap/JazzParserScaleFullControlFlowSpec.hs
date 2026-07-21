{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import JazzNext.Compiler.Bootstrap.JazzParserScale
  ( runJazzParserControlFlowScale,
  )
import JazzNext.Compiler.Bootstrap.JazzParserScaleAssertions
  ( assertScaleRun,
    fullControlFlowLimits,
  )
import JazzNext.Compiler.Runtime.Observation
  ( RuntimeObservationRequest (RuntimeObservationStatistics),
  )
import JazzNext.TestHarness
  ( runTestSuite,
  )

main :: IO ()
main =
  runTestSuite "JazzParserScaleFullControlFlow"
    [ ("parses the full generated control-flow program", testFullControlFlowScale)
    ]

testFullControlFlowScale :: IO ()
testFullControlFlowScale = do
  result <- runJazzParserControlFlowScale RuntimeObservationStatistics 512
  statistics <- assertScaleRun "full control-flow" 513 fullControlFlowLimits result
  putStrLn ("SCALE_STATS full-control-flow " <> show statistics)
