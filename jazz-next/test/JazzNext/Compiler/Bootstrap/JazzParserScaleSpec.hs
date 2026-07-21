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
    scaleWorkloadBindingCount,
    scaleWorkloadDeclarationGroupCount,
    scaleWorkloadExpectedStatementCount,
    smokeControlFlowLimits,
    smokeDeclarationsLimits,
    smokeExpressionLimits,
    smokeOperatorLimits,
    smokeScaleWorkload,
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
  [ ("parses a generated expression smoke program deterministically", assertDeterministicScalePair "expression" expectedStatementCount smokeExpressionLimits (runJazzParserScale RuntimeObservationStatistics bindingCount)),
    ("parses generated declaration smoke program deterministically", assertDeterministicScalePair "declarations" expectedStatementCount smokeDeclarationsLimits (runJazzParserDeclarationsScale RuntimeObservationStatistics declarationGroupCount)),
    ("parses generated control-flow smoke program deterministically", assertDeterministicScalePair "control-flow" expectedStatementCount smokeControlFlowLimits (runJazzParserControlFlowScale RuntimeObservationStatistics bindingCount)),
    ("parses generated operator smoke program deterministically", assertDeterministicScalePair "operator" expectedStatementCount smokeOperatorLimits (runJazzParserOperatorScale RuntimeObservationStatistics bindingCount))
  ]

bindingCount :: Int
bindingCount = scaleWorkloadBindingCount smokeScaleWorkload

declarationGroupCount :: Int
declarationGroupCount = scaleWorkloadDeclarationGroupCount smokeScaleWorkload

expectedStatementCount :: Int
expectedStatementCount = scaleWorkloadExpectedStatementCount smokeScaleWorkload
