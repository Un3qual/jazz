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
    runTestSuite,
  )
import JazzNext.TestSource
  ( readCheckedInJazzProjectModuleSource,
  )

main :: IO ()
main = runTestSuite "JazzParserComponent" tests

tests :: [NamedTest]
tests =
  [ ("takes canonical tokens through a predicate", testPredicateConsumption),
    ("matches identifiers and punctuation", testTokenMatching),
    ("selects current-token and end-of-input failures", testSelectedFailures),
    ("preserves canonical token payloads", testTokenViews),
    ("distinguishes immediate from spaced adjacency", testAdjacency),
    ("commits after consuming a token", testCommittedFailure),
    ("rejects trailing tokens at the complete boundary", testTrailingToken),
    ("converts zero progress into an invariant failure", testZeroProgress),
    ("keeps public and kernel parser failures unambiguous", testFailureOwnership)
  ]

testPredicateConsumption :: IO ()
testPredicateConsumption =
  assertJazzOutput
    "predicate consumption"
    """
    parserRun
      (tokenTakeIf (\\(token) -> case canonicalTokenKind token {
        | IdentifierKind name -> name == "value"
        | other -> False
      }) "value")
      [CanonicalToken (IdentifierKind "value") "value" (CanonicalSpan 1 1)]
    """
    "ParserSucceeded(CanonicalToken(IdentifierKind(\"value\"), \"value\", CanonicalSpan(1, 1)), ParserCursor([], 1), Consumed)"

testTokenMatching :: IO ()
testTokenMatching =
  assertJazzOutput
    "identifier and punctuation"
    """
    parserRun
      (parserKeepRight tokenIdentifier (tokenPunctuation DotPunctuation))
      [ CanonicalToken (IdentifierKind "value") "value" (CanonicalSpan 1 1)
      , CanonicalToken (PunctuationKind DotPunctuation) "." (CanonicalSpan 1 6)
      ]
    """
    "ParserSucceeded(CanonicalToken(PunctuationKind(DotPunctuation), \".\", CanonicalSpan(1, 6)), ParserCursor([], 2), Consumed)"

testSelectedFailures :: IO ()
testSelectedFailures =
  assertJazzOutput
    "selected failures"
    """
    ( parserRun tokenIdentifier [CanonicalToken (IntegerKind "1") "01" (CanonicalSpan 2 4)]
    , parserRun tokenIdentifier []
    )
    """
    "(ParserFailed(ParserFailure(0, Unconsumed, RejectedProblem(ParserGrammarFailure(Just(CanonicalSpan(2, 4)), ExpectedSyntax(\"identifier\", FoundToken(IntegerKind(\"1\"), \"01\")))))), ParserFailed(ParserFailure(0, Unconsumed, RejectedProblem(ParserGrammarFailure(Nothing, ExpectedSyntax(\"identifier\", EndOfInput))))))"

testTokenViews :: IO ()
testTokenViews =
  assertJazzOutput
    "token views"
    """
    { token = CanonicalToken (IntegerKind "42") "00042" (CanonicalSpan 3 9).
      (canonicalTokenKind token, canonicalTokenLexeme token, canonicalTokenSpan token).
    }
    """
    "(IntegerKind(\"42\"), \"00042\", CanonicalSpan(3, 9))"

testAdjacency :: IO ()
testAdjacency =
  assertJazzOutput
    "adjacency"
    """
    { name = CanonicalToken (IdentifierKind "Alias") "Alias" (CanonicalSpan 1 1).
      immediate = CanonicalToken (PunctuationKind DoubleColonPunctuation) "::" (CanonicalSpan 1 6).
      spaced = CanonicalToken (PunctuationKind DoubleColonPunctuation) "::" (CanonicalSpan 1 7).
      (tokensAreAdjacent name immediate, tokensAreAdjacent name spaced).
    }
    """
    "(True, False)"

testCommittedFailure :: IO ()
testCommittedFailure =
  assertJazzOutput
    "committed failure"
    """
    parserRun
      (parserKeepRight tokenIdentifier (tokenPunctuation DotPunctuation))
      [ CanonicalToken (IdentifierKind "value") "value" (CanonicalSpan 1 1)
      , CanonicalToken (PunctuationKind CommaPunctuation) "," (CanonicalSpan 1 6)
      ]
    """
    "ParserFailed(ParserFailure(1, Consumed, RejectedProblem(ParserGrammarFailure(Just(CanonicalSpan(1, 6)), ExpectedSyntax(\"\\'.\\'\", FoundToken(PunctuationKind(CommaPunctuation), \",\"))))))"

testTrailingToken :: IO ()
testTrailingToken =
  assertJazzOutput
    "trailing token"
    """
    tokenRunComplete
      tokenIdentifier
      [ CanonicalToken (IdentifierKind "value") "value" (CanonicalSpan 1 1)
      , CanonicalToken (PunctuationKind DotPunctuation) "." (CanonicalSpan 1 6)
      ]
    """
    "TokenParseRejected(ParserGrammarFailure(Just(CanonicalSpan(1, 6)), UnexpectedSyntax(FoundToken(PunctuationKind(DotPunctuation), \".\"), \"end of input\")))"

testZeroProgress :: IO ()
testZeroProgress =
  assertJazzOutput
    "zero progress"
    """
    tokenRunComplete
      (parserMany (parserSucceed 1))
      [CanonicalToken (IdentifierKind "value") "value" (CanonicalSpan 1 1)]
    """
    "TokenParseInvariantFailure(ParserGrammarFailure(Just(CanonicalSpan(1, 1)), InternalParserFailure(TokenStreamParseFailure)))"

testFailureOwnership :: IO ()
testFailureOwnership =
  assertJazzOutput
    "failure ownership"
    """
    ParserFailure
      "E0001"
      Nothing
      (ExpectedSyntax "expression" EndOfInput)
    """
    "ParserFailure(\"E0001\", Nothing, ExpectedSyntax(\"expression\", EndOfInput))"

assertJazzOutput :: Text.Text -> Text.Text -> Text.Text -> IO ()
assertJazzOutput label expression expected = do
  result <-
    runModuleGraph
      defaultWarningSettings
      resolverConfig
      ["App", "Main"]
      (lookupSource expression)
  assertEqual (label <> " compile errors") [] (runCompileErrors result)
  assertEqual (label <> " runtime errors") [] (runRuntimeErrors result)
  assertEqual (label <> " output") (Just expected) (runOutput result)

lookupSource :: Text.Text -> FilePath -> IO (Maybe Text.Text)
lookupSource expression sourcePath =
  case sourcePath of
    "src/App/Main.jz" ->
      pure
        ( Just
            ( Text.replace
                "__EXPRESSION__"
                expression
                """
                module App::Main {
                  import LexerTypes.
                  import Maybe (Nothing).
                  import ParserCore (parserRun, parserKeepRight, parserMany, parserSucceed).
                  import ParserToken.
                  import ParserTypes (ParserFailure, ExpectedSyntax, EndOfInput, TokenStreamParseFailure).
                  __EXPRESSION__.
                }

                """
            )
        )
    _ -> readCheckedInJazzProjectModuleSource sourcePath

resolverConfig :: ModuleResolutionConfig
resolverConfig = ModuleResolutionConfig {moduleRoots = ["src"], moduleExtension = ".jz"}
