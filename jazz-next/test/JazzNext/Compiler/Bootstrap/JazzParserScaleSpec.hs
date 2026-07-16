{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Text (Text)
import qualified Data.Text as Text
import JazzNext.Compiler.Bootstrap.JazzParserScale
  ( runJazzParserScale,
  )
import JazzNext.Compiler.Driver
  ( RunResult (..),
    runCompileErrors,
    runRuntimeErrors,
    runRuntimeObservation,
  )
import JazzNext.Compiler.Runtime.Observation
  ( RuntimeObservationReport (runtimeObservationStatistics, runtimeObservationTermination),
    RuntimeObservationRequest (RuntimeObservationStatistics),
    RuntimeStatistics (..),
    RuntimeTermination (RuntimeSucceeded),
  )
import JazzNext.TestHarness
  ( NamedTest,
    assertEqual,
    failTest,
    runTestSuite,
  )

main :: IO ()
main = runTestSuite "JazzParserScale" tests

tests :: [NamedTest]
tests =
  [ ("parses a large generated program deterministically", testLargeGeneratedProgram)
  ]

testLargeGeneratedProgram :: IO ()
testLargeGeneratedProgram = do
  first <- runJazzParserScale RuntimeObservationStatistics bindingCount
  second <- runJazzParserScale RuntimeObservationStatistics bindingCount
  assertSuccessfulBatch "large first" first
  assertSuccessfulBatch "large second" second
  assertEqual "large structured statement count" (Just "513") (runOutput first)
  assertEqual "large deterministic output" (runOutput first) (runOutput second)
  firstReport <- requireObservation "large first" first
  secondReport <- requireObservation "large second" second
  assertEqual "large first termination" RuntimeSucceeded (runtimeObservationTermination firstReport)
  assertEqual "large second termination" RuntimeSucceeded (runtimeObservationTermination secondReport)
  let firstStatistics = runtimeObservationStatistics firstReport
      secondStatistics = runtimeObservationStatistics secondReport
  assertEqual "large deterministic statistics" firstStatistics secondStatistics
  assertEqual "large host operations" 0 (runtimeHostOperations firstStatistics)
  assertAtMost "large evaluator transitions" transitionCeiling (runtimeEvaluatorTransitions firstStatistics)
  assertAtMost "large applications" applicationCeiling (runtimeApplications firstStatistics)
  assertAtMost "large list cells" listCellCeiling (runtimeListCellsConstructed firstStatistics)
  assertAtMost "large continuation depth" continuationDepthCeiling (runtimeMaximumContinuationDepth firstStatistics)
  where
    bindingCount = 512
    transitionCeiling = 22000000
    applicationCeiling = 2700000
    listCellCeiling = 115000
    continuationDepthCeiling = 1100

assertSuccessfulBatch :: Text -> RunResult -> IO ()
assertSuccessfulBatch label result = do
  assertEqual (label <> " compile errors") [] (runCompileErrors result)
  assertEqual (label <> " runtime errors") [] (runRuntimeErrors result)

requireObservation :: Text -> RunResult -> IO RuntimeObservationReport
requireObservation label result =
  case runRuntimeObservation result of
    Nothing -> failTest (label <> " did not produce runtime statistics")
    Just report -> pure report

assertAtMost :: (Ord value, Show value) => Text -> value -> value -> IO ()
assertAtMost label limit actual =
  if actual <= limit
    then pure ()
    else failTest (label <> " exceeded ceiling " <> showText limit <> ": " <> showText actual)

showText :: Show value => value -> Text
showText = Text.pack . show
