{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import JazzNext.Compiler.Bootstrap.JazzParserScale
  ( runJazzParserControlFlowScale,
    runJazzParserDeclarationsScale,
    runJazzParserOperatorScale,
    runJazzParserScale,
  )
import JazzNext.Compiler.Bootstrap.JazzParserScaleAssertions
  ( assertDeterministicScalePair,
    smokeControlFlowLimits,
    smokeDeclarationsLimits,
    smokeExpressionLimits,
    smokeOperatorLimits,
  )
import JazzNext.Compiler.Runtime.Observation
  ( RuntimeObservationRequest (RuntimeObservationStatistics),
  )
import JazzNext.TestHarness
  ( NamedTest,
    runTestSuite,
  )

main :: IO ()
main = runTestSuite "JazzParserScale" tests

tests :: [NamedTest]
tests =
  [ ("parses a generated expression smoke program deterministically", assertDeterministicScalePair "expression" smokeStatementCount smokeExpressionLimits (runJazzParserScale RuntimeObservationStatistics smokeBindingCount)),
    ("parses generated declaration smoke program deterministically", assertDeterministicScalePair "declarations" smokeStatementCount smokeDeclarationsLimits (runJazzParserDeclarationsScale RuntimeObservationStatistics smokeDeclarationGroupCount)),
    ("parses generated control-flow smoke program deterministically", assertDeterministicScalePair "control-flow" smokeStatementCount smokeControlFlowLimits (runJazzParserControlFlowScale RuntimeObservationStatistics smokeBindingCount)),
    ("parses generated operator smoke program deterministically", assertDeterministicScalePair "operator" smokeStatementCount smokeOperatorLimits (runJazzParserOperatorScale RuntimeObservationStatistics smokeBindingCount))
  ]

smokeBindingCount :: Int
smokeBindingCount = 64

smokeDeclarationGroupCount :: Int
smokeDeclarationGroupCount = 16

smokeStatementCount :: Int
smokeStatementCount = 65
