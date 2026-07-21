{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import JazzNext.Compiler.Bootstrap.JazzParserScale
  ( runJazzParserDeclarationsScale,
  )
import JazzNext.Compiler.Bootstrap.JazzParserScaleAssertions
  ( assertScaleRun,
    fullDeclarationsLimits,
  )
import JazzNext.Compiler.Runtime.Observation
  ( RuntimeObservationRequest (RuntimeObservationStatistics),
  )
import JazzNext.TestHarness
  ( runTestSuite,
  )

main :: IO ()
main =
  runTestSuite "JazzParserScaleFullDeclarations"
    [ ("parses the full generated declarations program", testFullDeclarationsScale)
    ]

testFullDeclarationsScale :: IO ()
testFullDeclarationsScale = do
  result <- runJazzParserDeclarationsScale RuntimeObservationStatistics 128
  statistics <- assertScaleRun "full declarations" 513 fullDeclarationsLimits result
  putStrLn ("SCALE_STATS full-declarations " <> show statistics)
