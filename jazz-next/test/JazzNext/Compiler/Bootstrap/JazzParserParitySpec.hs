{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Text (Text)
import JazzNext.Compiler.Bootstrap.JazzParserParity
  ( expectedSourceBatchRendering,
    expectedTokenBatchRendering,
    loadExpressionFoundationFixtures,
    runJazzParserSourceBatch,
    runJazzParserTokenBatch,
  )
import JazzNext.Compiler.Driver
  ( RunResult (..),
    runCompileErrors,
    runRuntimeErrors,
  )
import JazzNext.TestHarness
  ( NamedTest,
    assertEqual,
    runTestSuite,
  )

main :: IO ()
main = runTestSuite "JazzParserParity" tests

tests :: [NamedTest]
tests =
  [ ("matches exact canonical token-entry results twice", testTokenEntryParity),
    ("matches exact canonical source-entry results twice", testSourceEntryParity)
  ]

testTokenEntryParity :: IO ()
testTokenEntryParity = do
  fixtures <- loadExpressionFoundationFixtures
  expected <- expectedTokenBatchRendering fixtures
  first <- runJazzParserTokenBatch fixtures
  second <- runJazzParserTokenBatch fixtures
  assertSuccessfulBatch "token first" first
  assertSuccessfulBatch "token second" second
  assertEqual "token batch deterministic output" (runOutput first) (runOutput second)
  assertEqual "token batch exact stage-0 parity" (Just expected) (runOutput first)

testSourceEntryParity :: IO ()
testSourceEntryParity = do
  fixtures <- loadExpressionFoundationFixtures
  expected <- expectedSourceBatchRendering fixtures
  first <- runJazzParserSourceBatch fixtures
  second <- runJazzParserSourceBatch fixtures
  assertSuccessfulBatch "source first" first
  assertSuccessfulBatch "source second" second
  assertEqual "source batch deterministic output" (runOutput first) (runOutput second)
  assertEqual "source batch exact stage-0 parity" (Just expected) (runOutput first)

assertSuccessfulBatch :: Text -> RunResult -> IO ()
assertSuccessfulBatch label result = do
  assertEqual (label <> " compile errors") [] (runCompileErrors result)
  assertEqual (label <> " runtime errors") [] (runRuntimeErrors result)
