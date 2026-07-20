{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Text (Text)
import JazzNext.Compiler.Bootstrap.JazzParserParity
  ( expectedSourceBatchRendering,
    expectedTokenBatchRendering,
    loadControlFlowPatternsFixtures,
    loadExpressionFoundationFixtures,
    loadTypesDeclarationsModulesFixtures,
    runJazzParserSourceBatch,
    runJazzParserTokenBatch,
  )
import JazzNext.Compiler.Driver
  ( RunResult (..),
    runCompileErrors,
    runRuntimeErrors,
  )
import JazzNext.Compiler.Parser.FixtureCorpus
  ( ParserFixture,
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
  [ ("matches expression-family token entry twice", testExpressionTokenEntryParity),
    ("matches expression-family source entry twice", testExpressionSourceEntryParity),
    ("matches declarations-family token entry twice", testDeclarationsTokenEntryParity),
    ("matches declarations-family source entry twice", testDeclarationsSourceEntryParity),
    ("matches control-flow-family token entry twice", testControlFlowTokenEntryParity),
    ("matches control-flow-family source entry twice", testControlFlowSourceEntryParity)
  ]

testExpressionTokenEntryParity :: IO ()
testExpressionTokenEntryParity = do
  fixtures <- loadExpressionFoundationFixtures
  assertTokenEntryParity "expression family" fixtures

testExpressionSourceEntryParity :: IO ()
testExpressionSourceEntryParity = do
  fixtures <- loadExpressionFoundationFixtures
  assertSourceEntryParity "expression family" fixtures

testDeclarationsTokenEntryParity :: IO ()
testDeclarationsTokenEntryParity = do
  fixtures <- loadTypesDeclarationsModulesFixtures
  assertTokenEntryParity "declarations family" fixtures

testDeclarationsSourceEntryParity :: IO ()
testDeclarationsSourceEntryParity = do
  fixtures <- loadTypesDeclarationsModulesFixtures
  assertSourceEntryParity "declarations family" fixtures

testControlFlowTokenEntryParity :: IO ()
testControlFlowTokenEntryParity = do
  fixtures <- loadControlFlowPatternsFixtures
  assertTokenEntryParity "control-flow family" fixtures

testControlFlowSourceEntryParity :: IO ()
testControlFlowSourceEntryParity = do
  fixtures <- loadControlFlowPatternsFixtures
  assertSourceEntryParity "control-flow family" fixtures

assertTokenEntryParity :: Text -> [ParserFixture] -> IO ()
assertTokenEntryParity label fixtures = do
  expected <- expectedTokenBatchRendering fixtures
  first <- runJazzParserTokenBatch fixtures
  second <- runJazzParserTokenBatch fixtures
  assertSuccessfulBatch (label <> " token first") first
  assertSuccessfulBatch (label <> " token second") second
  assertEqual (label <> " token deterministic output") (runOutput first) (runOutput second)
  assertEqual (label <> " token exact stage-0 parity") (Just expected) (runOutput first)

assertSourceEntryParity :: Text -> [ParserFixture] -> IO ()
assertSourceEntryParity label fixtures = do
  expected <- expectedSourceBatchRendering fixtures
  first <- runJazzParserSourceBatch fixtures
  second <- runJazzParserSourceBatch fixtures
  assertSuccessfulBatch (label <> " source first") first
  assertSuccessfulBatch (label <> " source second") second
  assertEqual (label <> " source deterministic output") (runOutput first) (runOutput second)
  assertEqual (label <> " source exact stage-0 parity") (Just expected) (runOutput first)

assertSuccessfulBatch :: Text -> RunResult -> IO ()
assertSuccessfulBatch label result = do
  assertEqual (label <> " compile errors") [] (runCompileErrors result)
  assertEqual (label <> " runtime errors") [] (runRuntimeErrors result)
