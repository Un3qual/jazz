{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Text (Text)
import Jazz.Compiler.Bootstrap.JazzParserParity
  ( expectedSourceBatchRendering,
    expectedTokenBatchRendering,
    loadCompleteParserFixtures,
    loadControlFlowPatternsFixtures,
    loadCorpusClosureFixtures,
    loadExpressionFoundationFixtures,
    loadMixedOperatorControlFlowFixtures,
    loadOperatorFixtures,
    loadTypesDeclarationsModulesFixtures,
    runJazzParserSourceBatch,
    runJazzParserTokenBatch,
  )
import Jazz.Compiler.Driver
  ( RunResult (..),
    runCompileErrors,
    runRuntimeErrors,
  )
import Jazz.Compiler.Parser.FixtureCorpus
  ( ParserFixture,
  )
import Jazz.TestHarness
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
    ("matches control-flow-family source entry twice", testControlFlowSourceEntryParity),
    ("matches operator-family token entry twice", testOperatorTokenEntryParity),
    ("matches operator-family source entry twice", testOperatorSourceEntryParity),
    ("matches mixed-operator-family token entry twice", testMixedOperatorTokenEntryParity),
    ("matches mixed-operator-family source entry twice", testMixedOperatorSourceEntryParity),
    ("matches corpus-closure token entry twice", testCorpusClosureTokenEntryParity),
    ("matches corpus-closure source entry twice", testCorpusClosureSourceEntryParity),
    ("matches complete-corpus token entry twice", testCompleteCorpusTokenEntryParity),
    ("matches complete-corpus source entry twice", testCompleteCorpusSourceEntryParity)
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

testOperatorTokenEntryParity :: IO ()
testOperatorTokenEntryParity = do
  fixtures <- loadOperatorFixtures
  assertTokenEntryParity "operator family" fixtures

testOperatorSourceEntryParity :: IO ()
testOperatorSourceEntryParity = do
  fixtures <- loadOperatorFixtures
  assertSourceEntryParity "operator family" fixtures

testMixedOperatorTokenEntryParity :: IO ()
testMixedOperatorTokenEntryParity = do
  fixtures <- loadMixedOperatorControlFlowFixtures
  assertTokenEntryParity "mixed operator family" fixtures

testMixedOperatorSourceEntryParity :: IO ()
testMixedOperatorSourceEntryParity = do
  fixtures <- loadMixedOperatorControlFlowFixtures
  assertSourceEntryParity "mixed operator family" fixtures

testCorpusClosureTokenEntryParity :: IO ()
testCorpusClosureTokenEntryParity = do
  fixtures <- loadCorpusClosureFixtures
  assertTokenEntryParity "corpus closure" fixtures

testCorpusClosureSourceEntryParity :: IO ()
testCorpusClosureSourceEntryParity = do
  fixtures <- loadCorpusClosureFixtures
  assertSourceEntryParity "corpus closure" fixtures

testCompleteCorpusTokenEntryParity :: IO ()
testCompleteCorpusTokenEntryParity = do
  fixtures <- loadCompleteParserFixtures
  assertTokenEntryParity "complete corpus" fixtures

testCompleteCorpusSourceEntryParity :: IO ()
testCompleteCorpusSourceEntryParity = do
  fixtures <- loadCompleteParserFixtures
  assertSourceEntryParity "complete corpus" fixtures

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
