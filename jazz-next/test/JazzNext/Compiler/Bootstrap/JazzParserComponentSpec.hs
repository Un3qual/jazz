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
    ("commits expression delimiter and qualifier failures", testExpressionFailures),
    ("rejects whitespace after a qualified-name separator", testQualifiedMemberAdjacency),
    ("parses empty and populated programs through both facades", testProgramFacades),
    ("parses recursive blocks and block application", testRecursiveBlocks),
    ("commits binding and statement failures", testProgramFailures),
    ("rejects reserved bindings and unsupported declaration shapes", testProgramBoundaryFailures),
    ("rejects operator declaration candidates before expression fallback", testOperatorDeclarationBoundary),
    ("rejects reserved literal signatures before signature fallback", testReservedSignatureBoundary),
    ("rejects compact signatures in every statement scope", testSignatureBoundary),
    ("uses matching bindings to disambiguate compact signatures", testMatchingBindingSignatureBoundary)
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
    , parseComponentExpression "f 'x'"
    , parseComponentExpression "f \\\"text\\\""
    , parseComponentExpression "f (value)"
    , parseComponentExpression "f []"
    )
    """
    "(TokenParseSucceeded(TupleExpression([])), TokenParseSucceeded(ListExpression([])), TokenParseSucceeded(ListExpression([LiteralExpression(IntegerLiteral(\"1\")), LiteralExpression(BooleanLiteral(True)), LiteralExpression(CharacterLiteral('x'))])), TokenParseSucceeded(TupleExpression([LiteralExpression(IntegerLiteral(\"1\")), LiteralExpression(BooleanLiteral(True))])), TokenParseSucceeded(ApplyExpression(ApplyExpression(VariableExpression(\"f\"), LiteralExpression(IntegerLiteral(\"1\"))), LiteralExpression(BooleanLiteral(True)))), TokenParseSucceeded(ApplyExpression(VariableExpression(\"f\"), LiteralExpression(CharacterLiteral('x')))), TokenParseSucceeded(ApplyExpression(VariableExpression(\"f\"), LiteralExpression(TextLiteral(\"text\")))), TokenParseSucceeded(ApplyExpression(VariableExpression(\"f\"), VariableExpression(\"value\"))), TokenParseSucceeded(ApplyExpression(VariableExpression(\"f\"), ListExpression([]))))"

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

testQualifiedMemberAdjacency :: IO ()
testQualifiedMemberAdjacency =
  assertJazzOutput
    "qualified member adjacency"
    """
    case parseComponentExpression "Alias:: member" {
      | TokenParseRejected failure -> True
      | other -> False
    }
    """
    "True"

testProgramFacades :: IO ()
testProgramFacades =
  assertJazzOutput
    "program facades"
    """
    ( parseComponentTokens ""
    , parseSource componentPath ""
    , parseComponentTokens "value = 1. value."
    , parseSource componentPath "value = 1. value."
    )
    """
    "(CanonicalParserSuccess(CanonicalSourcePath(\"fixtures/parser/component.jz\"), BlockExpression([])), CanonicalSourceSuccess(CanonicalSourcePath(\"fixtures/parser/component.jz\"), BlockExpression([])), CanonicalParserSuccess(CanonicalSourcePath(\"fixtures/parser/component.jz\"), BlockExpression([LetStatement(\"value\", CanonicalSpan(1, 1), LiteralExpression(IntegerLiteral(\"1\"))), ExpressionStatement(CanonicalSpan(1, 12), VariableExpression(\"value\"))])), CanonicalSourceSuccess(CanonicalSourcePath(\"fixtures/parser/component.jz\"), BlockExpression([LetStatement(\"value\", CanonicalSpan(1, 1), LiteralExpression(IntegerLiteral(\"1\"))), ExpressionStatement(CanonicalSpan(1, 12), VariableExpression(\"value\"))])))"

testRecursiveBlocks :: IO ()
testRecursiveBlocks =
  assertJazzOutput
    "recursive blocks"
    """
    ( parseComponentTokens "{}."
    , parseComponentTokens "{{}.} 2."
    , parseComponentTokens "f {}."
    )
    """
    "(CanonicalParserSuccess(CanonicalSourcePath(\"fixtures/parser/component.jz\"), BlockExpression([ExpressionStatement(CanonicalSpan(1, 1), BlockExpression([]))])), CanonicalParserSuccess(CanonicalSourcePath(\"fixtures/parser/component.jz\"), BlockExpression([ExpressionStatement(CanonicalSpan(1, 1), ApplyExpression(BlockExpression([ExpressionStatement(CanonicalSpan(1, 2), BlockExpression([]))]), LiteralExpression(IntegerLiteral(\"2\"))))])), CanonicalParserSuccess(CanonicalSourcePath(\"fixtures/parser/component.jz\"), BlockExpression([ExpressionStatement(CanonicalSpan(1, 1), ApplyExpression(VariableExpression(\"f\"), BlockExpression([])))])))"

testProgramFailures :: IO ()
testProgramFailures =
  assertJazzOutput
    "program failures"
    """
    ( parseComponentTokens "value = ."
    , parseComponentTokens "value = 1"
    , parseComponentTokens "value"
    , parseComponentTokens "{"
    )
    """
    "(CanonicalParserFailure(CanonicalSourcePath(\"fixtures/parser/component.jz\"), ParserFailure(\"E0001\", Just(CanonicalSpan(1, 9)), UnexpectedSyntax(FoundToken(PunctuationKind(DotPunctuation), \".\"), \"expression\"))), CanonicalParserFailure(CanonicalSourcePath(\"fixtures/parser/component.jz\"), ParserFailure(\"E0001\", Nothing, ExpectedSyntax(\"\\'.\\'\", EndOfInput))), CanonicalParserFailure(CanonicalSourcePath(\"fixtures/parser/component.jz\"), ParserFailure(\"E0001\", Nothing, ExpectedSyntax(\"\\'.\\'\", EndOfInput))), CanonicalParserFailure(CanonicalSourcePath(\"fixtures/parser/component.jz\"), ParserFailure(\"E0001\", Nothing, ExpectedSyntax(\"\\'}\\'\", EndOfInput))))"

testProgramBoundaryFailures :: IO ()
testProgramBoundaryFailures =
  assertJazzOutput
    "program boundary failures"
    """
    ( parseComponentTokens "True = 1."
    , parseComponentTokens "False = 1."
    , parseComponentTokens "class Eq(a) { }."
    , parseComponentTokens "impl Eq(Int) { }."
    , parseComponentTokens "trait Eq(a) { }."
    )
    """
    "(CanonicalParserFailure(CanonicalSourcePath(\"fixtures/parser/component.jz\"), ParserFailure(\"E0001\", Just(CanonicalSpan(1, 1)), DeclarationFailure(ReservedLiteralName(BindingName, \"True\")))), CanonicalParserFailure(CanonicalSourcePath(\"fixtures/parser/component.jz\"), ParserFailure(\"E0001\", Just(CanonicalSpan(1, 1)), DeclarationFailure(ReservedLiteralName(BindingName, \"False\")))), CanonicalParserFailure(CanonicalSourcePath(\"fixtures/parser/component.jz\"), ParserFailure(\"E0001\", Just(CanonicalSpan(1, 1)), UnsupportedSyntax(AbstractionSyntax(\"class\")))), CanonicalParserFailure(CanonicalSourcePath(\"fixtures/parser/component.jz\"), ParserFailure(\"E0001\", Just(CanonicalSpan(1, 1)), UnsupportedSyntax(AbstractionSyntax(\"impl\")))), CanonicalParserFailure(CanonicalSourcePath(\"fixtures/parser/component.jz\"), ParserFailure(\"E0001\", Just(CanonicalSpan(1, 1)), UnsupportedSyntax(AbstractionSyntax(\"trait\")))))"

testOperatorDeclarationBoundary :: IO ()
testOperatorDeclarationBoundary = do
  assertJazzOutput
    "operator declaration boundary"
    """
    ( parseComponentTokens "operator plus tier 1."
    , parseComponentTokens "operator plus precedence 1."
    , parseComponentTokens "{ operator plus tier 1. }."
    )
    """
    "(CanonicalParserFailure(CanonicalSourcePath(\"fixtures/parser/component.jz\"), ParserFailure(\"E0001\", Just(CanonicalSpan(1, 10)), ExpectedSyntax(\"operator symbol after \\'operator\\'\", FoundToken(IdentifierKind(\"plus\"), \"plus\")))), CanonicalParserFailure(CanonicalSourcePath(\"fixtures/parser/component.jz\"), ParserFailure(\"E0001\", Just(CanonicalSpan(1, 10)), ExpectedSyntax(\"operator symbol after \\'operator\\'\", FoundToken(IdentifierKind(\"plus\"), \"plus\")))), CanonicalParserFailure(CanonicalSourcePath(\"fixtures/parser/component.jz\"), ParserFailure(\"E0001\", Just(CanonicalSpan(1, 3)), DeclarationFailure(DeclarationOutsideAllowedScope(OperatorDeclaration)))))"
  assertJazzOutput
    "symbolic operator declaration boundary"
    """
    ( isExpressionFoundationOperatorDeclarationFailure (parseComponentTokens "operator %% tier 2.")
    , isExpressionFoundationOperatorDeclarationFailure (parseComponentTokens "operator + tier 1.")
    , isExpressionFoundationOperatorDeclarationFailure (parseComponentTokens "operator -> tier 3.")
    )
    """
    "(True, True, True)"

testReservedSignatureBoundary :: IO ()
testReservedSignatureBoundary =
  assertJazzOutput
    "reserved literal signature boundary"
    """
    ( parseComponentTokens "True::Int."
    , parseComponentTokens "False::Int."
    , parseComponentTokens "True :: Int."
    , parseComponentTokens "False:: Int."
    )
    """
    "(CanonicalParserFailure(CanonicalSourcePath(\"fixtures/parser/component.jz\"), ParserFailure(\"E0001\", Just(CanonicalSpan(1, 1)), DeclarationFailure(ReservedLiteralName(BindingName, \"True\")))), CanonicalParserFailure(CanonicalSourcePath(\"fixtures/parser/component.jz\"), ParserFailure(\"E0001\", Just(CanonicalSpan(1, 1)), DeclarationFailure(ReservedLiteralName(BindingName, \"False\")))), CanonicalParserFailure(CanonicalSourcePath(\"fixtures/parser/component.jz\"), ParserFailure(\"E0001\", Just(CanonicalSpan(1, 1)), DeclarationFailure(ReservedLiteralName(BindingName, \"True\")))), CanonicalParserFailure(CanonicalSourcePath(\"fixtures/parser/component.jz\"), ParserFailure(\"E0001\", Just(CanonicalSpan(1, 1)), DeclarationFailure(ReservedLiteralName(BindingName, \"False\")))))"

testSignatureBoundary :: IO ()
testSignatureBoundary =
  assertJazzOutput
    "signature boundary"
    """
    ( isExpressionFoundationSignatureFailure (parseComponentTokens "value::Int.") 6
    , isExpressionFoundationSignatureFailure (parseComponentTokens "value::Maybe(Int).") 6
    , isExpressionFoundationSignatureFailure (parseComponentTokens "value::Map[Int].") 6
    , isExpressionFoundationSignatureFailure (parseComponentTokens "value::Int -> Int.") 6
    , isExpressionFoundationSignatureFailure (parseComponentTokens "value::[Int].") 6
    , isExpressionFoundationSignatureFailure (parseComponentTokens "unit::().") 5
    , isExpressionFoundationSignatureFailure (parseComponentTokens "value::Module::Type.") 6
    , isExpressionFoundationSignatureFailure (parseComponentTokens "value::@{Eq(a)}: Int.") 6
    , isExpressionFoundationSignatureFailure (parseComponentTokens "value :: Int.") 7
    , isExpressionFoundationSignatureFailure (parseComponentTokens "value:: Int.") 6
    , isExpressionFoundationSignatureFailure (parseComponentTokens "Alias :: member.") 7
    , isExpressionFoundationSignatureFailure (parseComponentTokens "Alias:: member.") 6
    , isExpressionFoundationSignatureFailure (parseComponentTokens "Alias::(member).") 6
    , isExpressionFoundationSignatureFailure (parseComponentTokens "Alias::(a).") 6
    , isExpressionFoundationSignatureFailure (parseComponentTokens "Alias::().") 6
    , isExpressionFoundationSignatureFailure (parseComponentTokens "Alias::((member)).") 6
    , isExpressionFoundationSignatureFailure (parseComponentTokens "Alias::(member) -> Int.") 6
    , parseComponentTokens "Alias::member."
    , isExpressionFoundationSignatureFailure (parseComponentTokens "Alias::member (value).") 6
    , isExpressionFoundationSignatureFailure (parseComponentTokens "Alias::member [value].") 6
    , isExpressionFoundationSignatureFailure (parseComponentTokens "Alias::member {}.") 6
    , isExpressionFoundationSignatureFailure (parseComponentTokens "{ value::Int. }.") 8
    , isExpressionFoundationSignatureFailure (parseComponentTokens "{ Result::a. }.") 9
    , isExpressionFoundationSignatureFailure (parseComponentTokens "{ value::Maybe(Int). }.") 8
    , isExpressionFoundationSignatureFailure (parseComponentTokens "{ value::[Int]. }.") 8
    , parseComponentTokens "{ Alias::member. }."
    )
    """
    "(True, True, True, True, True, True, True, True, True, True, True, False, False, True, True, False, True, CanonicalParserSuccess(CanonicalSourcePath(\"fixtures/parser/component.jz\"), BlockExpression([ExpressionStatement(CanonicalSpan(1, 1), QualifiedVariableExpression(\"Alias\", \"member\"))])), True, True, True, True, True, True, True, CanonicalParserSuccess(CanonicalSourcePath(\"fixtures/parser/component.jz\"), BlockExpression([ExpressionStatement(CanonicalSpan(1, 1), BlockExpression([ExpressionStatement(CanonicalSpan(1, 3), QualifiedVariableExpression(\"Alias\", \"member\"))]))])))"

testMatchingBindingSignatureBoundary :: IO ()
testMatchingBindingSignatureBoundary =
  assertJazzOutput
    "matching binding signature boundary"
    """
    ( isExpressionFoundationSignatureFailure (parseComponentTokens "Result::value. Result = 1.") 7
    , isExpressionFoundationSignatureFailure (parseComponentTokens "{ Result::value. Result = 1. }.") 9
    , isExpressionFoundationSignatureFailure (parseComponentTokens "Result::a b. Result = 1.") 7
    , isExpressionFoundationSignatureFailure (parseComponentTokens "{ Result::a b. Result = 1. }.") 9
    , isCanonicalParserSuccess (parseComponentTokens "Result::value. Other = 1.")
    , isCanonicalParserSuccess (parseComponentTokens "{ Result::value. Other = 1. }.")
    , isCanonicalParserSuccess (parseComponentTokens "Result::a b. Other = 1.")
    , isCanonicalParserSuccess (parseComponentTokens "{ Result::a b. Other = 1. }.")
    , isExpressionFoundationSignatureFailure (parseComponentTokens "Result::a Other = 0. Result = 1.") 7
    , isExpressionFoundationSignatureFailure (parseComponentTokens "Result::(value) Other = 0. Result = 1.") 7
    , isExpressionFoundationSignatureFailure (parseComponentTokens "{ Result::(value) Other = 0. Result = 1. }.") 9
    , isExpressionFoundationSignatureFailure (parseComponentTokens "value::Int -> Other = 0. value = 1.") 6
    , isExpressionFoundationSignatureFailure (parseComponentTokens "{ value::Int -> Other = 0. value = 1. }.") 8
    , isExpressionFoundationSignatureFailure (parseComponentTokens "value::[Int] Other = 0. value = 1.") 6
    , isExpressionFoundationSignatureFailure (parseComponentTokens "Result::(value) -> Other = 0. Result = 1.") 7
    , isExpressionFoundationSignatureFailure (parseComponentTokens "Result::(value). Result = 1.") 7
    )
    """
    "(True, True, True, True, True, True, True, True, False, False, False, False, False, False, False, True)"

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
                  import Maybe (Nothing, Just).
                  import Parser (parseSource, parseTokens).
                  import ParserExpression (parseFoundationalExpression).
                  import ParserToken.
                  import ParserTypes (ParserFailure, ExpectedSyntax, UnexpectedSyntax, FoundToken, EndOfInput, TokenStreamParseFailure, CanonicalParserSuccess, CanonicalParserFailure).
                  expressionBlockFailure = tokenFailAt Nothing (ExpectedSyntax "block" EndOfInput).
                  componentPath = CanonicalSourcePath "fixtures/parser/component.jz".
                  expressionTokens = \\(source) -> case lexSource (CanonicalSourcePath "fixtures/parser/component.jz") source {
                    | CanonicalLexSuccess path tokens -> tokens
                  }.
                  parseComponentExpression = \\(source) -> tokenRunComplete (parseFoundationalExpression expressionBlockFailure) (expressionTokens source).
                  parseComponentTokens = \\(source) -> parseTokens componentPath (expressionTokens source).
                  isCanonicalParserSuccess = \\(result) -> case result {
                    | CanonicalParserSuccess path expression -> True
                    | CanonicalParserFailure path failure -> False
                  }.
                  isExpressionFoundationOperatorDeclarationFailure = \\(result) -> case result {
                    | CanonicalParserSuccess _ _ -> False
                    | CanonicalParserFailure _ failure -> case failure {
                      | ParserFailure code maybeSpan reason -> if code == "E0001" then case reason {
                        | UnexpectedSyntax encountered expected -> expected == "an expression-foundation statement"
                        | other -> False
                      } else False
                    }
                  }.
                  isExpressionFoundationSignatureFailure = \\(result, expectedColumn) -> case result {
                    | CanonicalParserSuccess _ _ -> False
                    | CanonicalParserFailure _ failure -> case failure {
                      | ParserFailure code maybeSpan reason -> if code == "E0001" then case maybeSpan {
                        | Nothing -> False
                        | Just span -> case span {
                          | CanonicalSpan line column -> if line == 1 then if column == expectedColumn then case reason {
                            | UnexpectedSyntax encountered expected -> if expected == "an expression-foundation statement" then case encountered {
                              | FoundToken kind lexeme -> if lexeme == "::" then case kind {
                                | PunctuationKind DoubleColonPunctuation -> True
                                | other -> False
                              } else False
                              | other -> False
                            } else False
                            | other -> False
                          } else False else False
                        }
                      } else False
                    }
                  }.
                  __EXPRESSION__.
                }

                """
            )
        )
    _ -> readCheckedInJazzProjectModuleSource sourcePath

resolverConfig :: ModuleResolutionConfig
resolverConfig = ModuleResolutionConfig {moduleRoots = ["src"], moduleExtension = ".jz"}
