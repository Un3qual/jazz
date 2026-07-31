{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Jazz.Compiler.Bootstrap.JazzParserScale
  ( runJazzParserDeclarationsScale,
  )
import Jazz.Compiler.Bootstrap.JazzParserScaleAssertions
  ( assertScaleRun,
    fullDeclarationsLimits,
    fullScaleWorkload,
    scaleWorkloadDeclarationGroupCount,
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
    "JazzParserScaleFullDeclarations"
    [ ("parses the full generated declarations program", testFullDeclarationsScale)
    ]

testFullDeclarationsScale :: IO ()
testFullDeclarationsScale = do
  result <- runJazzParserDeclarationsScale RuntimeObservationStatistics (scaleWorkloadDeclarationGroupCount fullScaleWorkload)
  statistics <- assertScaleRun "full declarations" (scaleWorkloadExpectedStatementCount fullScaleWorkload) fullDeclarationsLimits result
  putStrLn ("SCALE_STATS full-declarations " <> show statistics)
