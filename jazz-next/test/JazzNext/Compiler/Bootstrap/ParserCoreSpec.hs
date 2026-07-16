{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Text as Text
import JazzNext.Compiler.Driver
  ( RunResult (..),
    runCompileErrors,
    runModuleGraph,
    runRuntimeErrors,
  )
import JazzNext.Compiler.ModuleResolver
  ( ModuleResolutionConfig (..),
  )
import JazzNext.Compiler.WarningConfig
  ( defaultWarningSettings,
  )
import JazzNext.TestHarness
  ( NamedTest,
    assertEqual,
    failTest,
    runTestSuite,
  )
import JazzNext.TestSource
  ( readCheckedInJazzProjectModuleSource,
  )
import System.Timeout
  ( timeout,
  )

main :: IO ()
main = runTestSuite "ParserCore" tests

tests :: [NamedTest]
tests =
  [ ("starts at an immutable zero-offset cursor", testInitialCursor),
    ("takes one token or rejects without moving", testTakeIf),
    ("transforms and sequences replies", testTransformAndThen),
    ("keeps selected sequenced values", testKeepLeftRight),
    ("commits choice after consumption and rolls back only with attempt", testChoiceAndAttempt),
    ("selects the farthest failure and keeps declaration order on ties", testFailureSelection),
    ("looks ahead without consuming", testLookAhead),
    ("makes only unconsumed rejection optional", testOptional),
    ("repeats and preserves nonempty results", testRepetition),
    ("parses separated values", testSeparatedBy),
    ("peeks without consuming", testPeek),
    ("traverses 20000 tokens with an exact final cursor", testLargeTraversal),
    ("selects the exact farthest offset after a long traversal", testLongFarthestFailure),
    ("rejects zero-progress repetition", testZeroProgress),
    ("keeps zero-progress failures nonrecoverable", testZeroProgressRecovery),
    ("renders representative batches deterministically", testDeterministicBatch)
  ]

testInitialCursor :: IO ()
testInitialCursor =
  assertJazzOutput
    "initial cursor"
    "parserRun (parserSucceed \"ok\") [1, 2]"
    "ParserSucceeded(\"ok\", ParserCursor([1, 2], 0), Unconsumed)"

testTakeIf :: IO ()
testTakeIf =
  assertJazzOutput
    "take if"
    "(parserRun (parserTakeIf (\\(token) -> token == 1) \"expected one\") [1, 2], parserRun (parserTakeIf (\\(token) -> token == 1) \"expected one\") [2, 1])"
    "(ParserSucceeded(1, ParserCursor([2], 1), Consumed), ParserFailed(ParserFailure(0, Unconsumed, RejectedProblem(\"expected one\"))))"

testTransformAndThen :: IO ()
testTransformAndThen =
  assertJazzOutput
    "transform and then"
    "parserRun (parserAndThen (\\(first) -> parserTransform (\\(second) -> first + second) (parserTakeIf (\\(token) -> token == 2) \"expected two\")) (parserTakeIf (\\(token) -> token == 1) \"expected one\")) [1, 2, 3]"
    "ParserSucceeded(3, ParserCursor([3], 2), Consumed)"

testKeepLeftRight :: IO ()
testKeepLeftRight =
  assertJazzOutput
    "keep left and right"
    "(parserRun (parserKeepLeft (parserTakeIf (\\(token) -> token == 1) \"one\") (parserTakeIf (\\(token) -> token == 2) \"two\")) [1, 2, 3], parserRun (parserKeepRight (parserTakeIf (\\(token) -> token == 1) \"one\") (parserTakeIf (\\(token) -> token == 2) \"two\")) [1, 2, 3])"
    "(ParserSucceeded(1, ParserCursor([3], 2), Consumed), ParserSucceeded(2, ParserCursor([3], 2), Consumed))"

testChoiceAndAttempt :: IO ()
testChoiceAndAttempt =
  assertJazzOutput
    "choice and attempt"
    "{ left = parserKeepRight (parserTakeIf (\\(token) -> token == 1) \"one\") (parserFail \"left\"). right = parserSucceed \"right\". (parserRun (parserChoice left right) [1, 2], parserRun (parserChoice (parserAttempt left) right) [1, 2]). }"
    "(ParserFailed(ParserFailure(1, Consumed, RejectedProblem(\"left\"))), ParserSucceeded(\"right\", ParserCursor([1, 2], 0), Unconsumed))"

testFailureSelection :: IO ()
testFailureSelection =
  assertJazzOutput
    "failure selection"
    "{ take = \\(expected) -> parserTakeIf (\\(token) -> token == expected) \"token\". one = parserAttempt (parserKeepRight (take 1) (parserFail \"earlier\")). two = parserAttempt (parserKeepRight (take 1) (parserKeepRight (take 2) (parserFail \"farther\"))). tied = parserAttempt (parserKeepRight (take 1) (parserFail \"later tie\")). (parserRun (parserChoice one two) [1, 2, 3], parserRun (parserChoice one tied) [1, 2, 3]). }"
    "(ParserFailed(ParserFailure(2, Unconsumed, RejectedProblem(\"farther\"))), ParserFailed(ParserFailure(1, Unconsumed, RejectedProblem(\"earlier\"))))"

testLookAhead :: IO ()
testLookAhead =
  assertJazzOutput
    "lookahead"
    "{ one = parserTakeIf (\\(token) -> token == 1) \"one\". (parserRun (parserKeepRight (parserLookAhead one) one) [1, 2], parserRun (parserLookAhead one) [2, 1]). }"
    "(ParserSucceeded(1, ParserCursor([2], 1), Consumed), ParserFailed(ParserFailure(0, Unconsumed, RejectedProblem(\"one\"))))"

testOptional :: IO ()
testOptional =
  assertJazzOutput
    "optional"
    "{ one = parserTakeIf (\\(token) -> token == 1) \"one\". (parserRun (parserOptional one) [1, 2], parserRun (parserOptional one) [2, 1]). }"
    "(ParserSucceeded(Just(1), ParserCursor([2], 1), Consumed), ParserSucceeded(Nothing, ParserCursor([2, 1], 0), Unconsumed))"

testRepetition :: IO ()
testRepetition =
  assertJazzOutput
    "repetition"
    "{ positive = parserTakeIf (\\(token) -> token > 0) \"positive\". (parserRun (parserMany positive) [1, 2, 0], parserRun (parserOneOrMore positive) [1, 2, 0]). }"
    "(ParserSucceeded([1, 2], ParserCursor([0], 2), Consumed), ParserSucceeded(NonEmpty(1, [2]), ParserCursor([0], 2), Consumed))"

testSeparatedBy :: IO ()
testSeparatedBy =
  assertJazzOutput
    "separated by"
    "{ item = parserTakeIf (\\(token) -> token > 0) \"item\". separator = parserTakeIf (\\(token) -> token == 0) \"separator\". (parserRun (parserSeparatedBy item separator) [1, 0, 2, 0, 3, 9], parserRun (parserSeparatedBy item separator) []). }"
    "(ParserSucceeded([1, 2, 3], ParserCursor([9], 5), Consumed), ParserSucceeded([], ParserCursor([], 0), Unconsumed))"

testPeek :: IO ()
testPeek =
  assertJazzOutput
    "peek"
    "(parserRun parserPeek [1, 2], parserRun parserPeek [])"
    "(ParserSucceeded(Just(1), ParserCursor([1, 2], 0), Unconsumed), ParserSucceeded(Nothing, ParserCursor([], 0), Unconsumed))"

testLargeTraversal :: IO ()
testLargeTraversal =
  assertJazzOutputWithin
    60000000
    "large traversal"
    """
    { one = parserTakeIf (\\(token) -> token == 1) "one".
      reply = parserRun (parserMany one) (listRepeat 20000 1).
      case reply {
        | ParserSucceeded values cursor consumption -> case cursor {
          | ParserCursor remaining offset -> (listLength values, listLength remaining, offset)
        }
        | ParserFailed failure -> (0, 0, 0)
      }.
    }
    """
    "(20000, 0, 20000)"

testLongFarthestFailure :: IO ()
testLongFarthestFailure =
  assertJazzOutputWithin
    60000000
    "long farthest failure"
    """
    { input = listAppend (listRepeat 10000 1) (listPrepend 2 (listRepeat 9999 1)).
      one = parserTakeIf (\\(token) -> token == 1) "one".
      positive = parserTakeIf (\\(token) -> token > 0) "positive".
      short = parserAttempt (parserKeepRight (parserMany one) (parserFail "short")).
      far = parserAttempt (parserKeepRight (parserMany positive) (parserFail "far")).
      parserRun (parserChoice short far) input.
    }
    """
    "ParserFailed(ParserFailure(20000, Unconsumed, RejectedProblem(\"far\")))"

testZeroProgress :: IO ()
testZeroProgress =
  assertJazzOutput
    "zero progress"
    "parserRun (parserMany (parserSucceed 1)) [1, 2]"
    "ParserFailed(ParserFailure(0, Unconsumed, ZeroProgressProblem))"

testZeroProgressRecovery :: IO ()
testZeroProgressRecovery =
  assertJazzOutput
    "zero progress recovery"
    """
    { repeated = parserMany (parserSucceed 1).
      fallback = parserSucceed [].
      ( parserRun (parserChoice repeated fallback) [1]
      , parserRun (parserOptional repeated) [1]
      , parserRun (parserChoice (parserAttempt repeated) fallback) [1]
      , parserRun (parserChoice (parserLookAhead repeated) fallback) [1]
      ).
    }
    """
    "(ParserFailed(ParserFailure(0, Unconsumed, ZeroProgressProblem)), ParserFailed(ParserFailure(0, Unconsumed, ZeroProgressProblem)), ParserFailed(ParserFailure(0, Unconsumed, ZeroProgressProblem)), ParserFailed(ParserFailure(0, Unconsumed, ZeroProgressProblem)))"

testDeterministicBatch :: IO ()
testDeterministicBatch = do
  let expression =
        """
        { one = parserTakeIf (\\(token) -> token == 1) "one".
          committed = parserKeepRight one (parserFail "after one").
          (parserRun (parserMany one) [1, 1, 2], parserRun committed [1, 2], parserRun (parserMany (parserSucceed 1)) [1]).
        }
        """
      expected =
        "(ParserSucceeded([1, 1], ParserCursor([2], 2), Consumed), ParserFailed(ParserFailure(1, Consumed, RejectedProblem(\"after one\"))), ParserFailed(ParserFailure(0, Unconsumed, ZeroProgressProblem)))"
  first <- runJazzExpression expression
  second <- runJazzExpression expression
  assertRunResult "deterministic batch first" expected first
  assertRunResult "deterministic batch second" expected second
  assertEqual "deterministic batch byte identity" (runOutput first) (runOutput second)

assertJazzOutput :: Text.Text -> Text.Text -> Text.Text -> IO ()
assertJazzOutput label expression expected = do
  result <- runJazzExpression expression
  assertRunResult label expected result

assertJazzOutputWithin :: Int -> Text.Text -> Text.Text -> Text.Text -> IO ()
assertJazzOutputWithin microseconds label expression expected = do
  maybeResult <- timeout microseconds (runJazzExpression expression)
  case maybeResult of
    Nothing -> failTest (label <> " timed out")
    Just result -> assertRunResult label expected result

runJazzExpression :: Text.Text -> IO RunResult
runJazzExpression expression =
  runModuleGraph
    defaultWarningSettings
    resolverConfig
    ["App", "Main"]
    (lookupSource expression)

assertRunResult :: Text.Text -> Text.Text -> RunResult -> IO ()
assertRunResult label expected result = do
  assertEqual (label <> " compile errors") [] (runCompileErrors result)
  assertEqual (label <> " runtime errors") [] (runRuntimeErrors result)
  assertEqual (label <> " output") (Just expected) (runOutput result)

lookupSource :: Text.Text -> FilePath -> IO (Maybe Text.Text)
lookupSource expression sourcePath =
  case sourcePath of
    "src/App/Main.jz" ->
      pure
        ( Just
            ( "module App::Main {\n"
                <> "  import ParserCore.\n"
                <> "  import List.\n"
                <> "  "
                <> expression
                <> ".\n"
                <> "}\n"
            )
        )
    _ -> readCheckedInJazzProjectModuleSource sourcePath

resolverConfig :: ModuleResolutionConfig
resolverConfig = ModuleResolutionConfig {moduleRoots = ["src"], moduleExtension = ".jz"}
