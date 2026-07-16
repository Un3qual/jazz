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
    ("keeps public and kernel parser failures unambiguous", testFailureOwnership),
    ("parses scalar expressions with source-exact numerics", testScalarExpressions),
    ("parses ordinary and qualified names", testNameExpressions),
    ("parses composite primaries and left-associated application", testCompositeExpressions),
    ("commits expression delimiter and qualifier failures", testExpressionFailures)
  ]

testPredicateConsumption :: IO ()
testPredicateConsumption =
  assertJazzOutput
    "predicate consumption"
    """
    tokenRun
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
    tokenRun
      (tokenKeepRight tokenIdentifier (tokenPunctuation DotPunctuation))
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
    ( tokenRun tokenIdentifier [CanonicalToken (IntegerKind "1") "01" (CanonicalSpan 2 4)]
    , tokenRun tokenIdentifier []
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
    tokenRun
      (tokenKeepRight tokenIdentifier (tokenPunctuation DotPunctuation))
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
      (tokenMany (tokenSucceed 1))
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

testScalarExpressions :: IO ()
testScalarExpressions =
  assertJazzOutput
    "scalar expressions"
    """
    ( parseComponentExpression "00042"
    , parseComponentExpression "1.5f16"
    , parseComponentExpression "1.5f32"
    , parseComponentExpression "1.5f64"
    , parseComponentExpression "179769313486231570814527423731704356798070567525844996598917476803157260780028538760589558632766878171540458953514382464234321326889464182768467546703537516986049910576551282076245490090389328944075868508455133942304583236903222948165808559332123348274797826204144723168738177180919299881250404026184124858368.0"
    , parseComponentExpression "True"
    , parseComponentExpression "'x'"
    , parseComponentExpression "text"
    , parseComponentExpression "\\\"Jazz\\\""
    )
    """
    "(TokenParseSucceeded(LiteralExpression(IntegerLiteral(\"42\"))), TokenParseSucceeded(LiteralExpression(FractionalLiteral(\"1\", \"5\", Just(Float16Type)))), TokenParseSucceeded(LiteralExpression(FractionalLiteral(\"1\", \"5\", Just(Float32Type)))), TokenParseSucceeded(LiteralExpression(FractionalLiteral(\"1\", \"5\", Just(Float64Type)))), TokenParseSucceeded(LiteralExpression(FractionalLiteral(\"179769313486231570814527423731704356798070567525844996598917476803157260780028538760589558632766878171540458953514382464234321326889464182768467546703537516986049910576551282076245490090389328944075868508455133942304583236903222948165808559332123348274797826204144723168738177180919299881250404026184124858368\", \"0\", Nothing))), TokenParseSucceeded(LiteralExpression(BooleanLiteral(True))), TokenParseSucceeded(LiteralExpression(CharacterLiteral('x'))), TokenParseSucceeded(VariableExpression(\"text\")), TokenParseSucceeded(LiteralExpression(TextLiteral(\"Jazz\"))))"

testNameExpressions :: IO ()
testNameExpressions =
  assertJazzOutput
    "name expressions"
    """
    ( parseComponentExpression "value"
    , parseComponentExpression "Alias::member"
    , parseComponentExpression "(value)"
    )
    """
    "(TokenParseSucceeded(VariableExpression(\"value\")), TokenParseSucceeded(QualifiedVariableExpression(\"Alias\", \"member\")), TokenParseSucceeded(VariableExpression(\"value\")))"

testCompositeExpressions :: IO ()
testCompositeExpressions =
  assertJazzOutput
    "composite expressions"
    """
    ( parseComponentExpression "()"
    , parseComponentExpression "[]"
    , parseComponentExpression "[1, True, 'x']"
    , parseComponentExpression "(1, True)"
    , parseComponentExpression "f 1 True"
    )
    """
    "(TokenParseSucceeded(TupleExpression([])), TokenParseSucceeded(ListExpression([])), TokenParseSucceeded(ListExpression([LiteralExpression(IntegerLiteral(\"1\")), LiteralExpression(BooleanLiteral(True)), LiteralExpression(CharacterLiteral('x'))])), TokenParseSucceeded(TupleExpression([LiteralExpression(IntegerLiteral(\"1\")), LiteralExpression(BooleanLiteral(True))])), TokenParseSucceeded(ApplyExpression(ApplyExpression(VariableExpression(\"f\"), LiteralExpression(IntegerLiteral(\"1\"))), LiteralExpression(BooleanLiteral(True)))))"

testExpressionFailures :: IO ()
testExpressionFailures =
  assertJazzOutput
    "expression failures"
    """
    ( parseComponentExpression "[1,]"
    , parseComponentExpression "(1,)"
    , parseComponentExpression "Alias::"
    , parseComponentExpression "Alias ::member"
    , parseComponentExpression "179769313486231570814527423731704356798070567525844996598917476803157260780028538760589558632766878171540458953514382464234321326889464182768467546703537516986049910576551282076245490090389328944075868508455133942304583236903222948165808559332123348274797826204144723168738177180919299881250404026184124858369.0"
    )
    """
    "(TokenParseRejected(ParserGrammarFailure(Just(CanonicalSpan(1, 4)), UnexpectedSyntax(FoundToken(PunctuationKind(RightBracketPunctuation), \"]\"), \"expression\"))), TokenParseRejected(ParserGrammarFailure(Just(CanonicalSpan(1, 4)), UnexpectedSyntax(FoundToken(PunctuationKind(RightParenPunctuation), \")\"), \"expression\"))), TokenParseRejected(ParserGrammarFailure(Nothing, ExpectedSyntax(\"member name\", EndOfInputAfter(\"\\'::\\'\")))), TokenParseRejected(ParserGrammarFailure(Just(CanonicalSpan(1, 7)), UnexpectedSyntax(FoundToken(PunctuationKind(DoubleColonPunctuation), \"::\"), \"end of input\"))), TokenParseRejected(ParserGrammarFailure(Just(CanonicalSpan(1, 1)), InvalidFractionalLiteral(\"179769313486231570814527423731704356798070567525844996598917476803157260780028538760589558632766878171540458953514382464234321326889464182768467546703537516986049910576551282076245490090389328944075868508455133942304583236903222948165808559332123348274797826204144723168738177180919299881250404026184124858369.0\"))))"

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
                  import Lexer (lexSource).
                  import Maybe (Nothing).
                  import ParserExpression (parseFoundationalExpression).
                  import ParserToken.
                  import ParserTypes (ParserFailure, ExpectedSyntax, EndOfInput, TokenStreamParseFailure).
                  expressionBlockFailure = tokenFailAt Nothing (ExpectedSyntax "block" EndOfInput).
                  expressionTokens = \\(source) -> case lexSource (CanonicalSourcePath "fixtures/parser/component.jz") source {
                    | CanonicalLexSuccess path tokens -> tokens
                  }.
                  parseComponentExpression = \\(source) -> tokenRunComplete (parseFoundationalExpression expressionBlockFailure) (expressionTokens source).
                  __EXPRESSION__.
                }

                """
            )
        )
    _ -> readCheckedInJazzProjectModuleSource sourcePath

resolverConfig :: ModuleResolutionConfig
resolverConfig = ModuleResolutionConfig {moduleRoots = ["src"], moduleExtension = ".jz"}
