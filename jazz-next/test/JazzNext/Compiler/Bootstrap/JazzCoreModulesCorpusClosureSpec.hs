{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Text as Text
import JazzNext.Compiler.Bootstrap.JazzCoreParity
  ( expectedCanonicalExpressionBatchRendering,
    runJazzCanonicalExpressionBatch,
    runJazzSignaturesDeclarationsOperatorsBatch,
  )
import JazzNext.Compiler.Diagnostics
  ( SourceSpan (..),
  )
import JazzNext.Compiler.Driver
  ( RunResult (..),
    runCompileErrors,
    runRuntimeErrors,
  )
import JazzNext.Compiler.Parser.AST
import JazzNext.TestHarness
  ( NamedTest,
    assertEqual,
    failTest,
    runTestSuite,
  )

main :: IO ()
main = runTestSuite "JazzCoreModulesCorpusClosure" tests

tests :: [NamedTest]
tests =
  [ ("lowers module and import statements through the complete expression entry", testCompleteExpressionParity),
    ("preserves the child-3 module and import deferral boundary", testEarlierProfileBoundary)
  ]

testCompleteExpressionParity :: IO ()
testCompleteExpressionParity = do
  expected <-
    expectRight
      "complete expression expected values"
      (expectedCanonicalExpressionBatchRendering completeExpressions)
  first <- runJazzCanonicalExpressionBatch completeExpressions
  second <- runJazzCanonicalExpressionBatch completeExpressions
  assertSuccessfulOutput "complete expression first run" expected first
  assertSuccessfulOutput "complete expression second run" expected second
  assertEqual "complete expression deterministic output" (runOutput first) (runOutput second)

testEarlierProfileBoundary :: IO ()
testEarlierProfileBoundary = do
  first <- runJazzSignaturesDeclarationsOperatorsBatch completeExpressions
  second <- runJazzSignaturesDeclarationsOperatorsBatch completeExpressions
  let expected = "[Nothing, Nothing]"
  assertSuccessfulOutput "child-3 deferral first run" expected first
  assertSuccessfulOutput "child-3 deferral second run" expected second
  assertEqual "child-3 deferral deterministic output" (runOutput first) (runOutput second)

completeExpressions :: [SurfaceExpr]
completeExpressions =
  [ SEBlock
      [ SSModule span1 ["App", "Main"] (Just []),
        SSImport span2 ["Core", "Text"] (Just "Text") (Just ["length", "uncons"]),
        SSExpr span3 (SELit (SLInt 1))
      ],
    SEBlock
      [ SSLet
          "nested"
          span1
          ( SEBlock
              [ SSImport span2 ["Core", "List"] Nothing Nothing,
                SSExpr span3 (SEVar "value")
              ]
          ),
        SSExpr span3 (SEVar "nested")
      ]
  ]

span1 :: SourceSpan
span1 = SourceSpan 1 1

span2 :: SourceSpan
span2 = SourceSpan 2 3

span3 :: SourceSpan
span3 = SourceSpan 3 5

assertSuccessfulOutput :: Text.Text -> Text.Text -> RunResult -> IO ()
assertSuccessfulOutput label expected result = do
  assertEqual (label <> " compile errors") [] (runCompileErrors result)
  assertEqual (label <> " runtime errors") [] (runRuntimeErrors result)
  assertEqual (label <> " output") (Just expected) (runOutput result)

expectRight :: (Show err) => Text.Text -> Either err value -> IO value
expectRight label value =
  case value of
    Left err -> failTest (label <> ": expected Right, got Left " <> Text.pack (show err))
    Right ok -> pure ok
