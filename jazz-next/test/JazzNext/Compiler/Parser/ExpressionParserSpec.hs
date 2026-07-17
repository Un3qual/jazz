{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import JazzNext.Compiler.Diagnostics
  ( Diagnostic,
    SourceSpan (..),
  )
import JazzNext.Compiler.Diagnostics.Render
  ( renderDiagnostic,
  )
import JazzNext.Compiler.FractionalLiteral
  ( mkFractionalLiteralSource,
  )
import JazzNext.Compiler.Parser (parseStatementsUntilBrace)
import JazzNext.Compiler.Parser.AST
  ( SurfaceCaseArm (..),
    SurfaceExpr (..),
    SurfaceLambdaParameter (..),
    SurfaceLiteral (..),
    SurfaceNumericType (..),
    SurfacePattern (..),
    SurfaceSignaturePayload (..),
    SurfaceSignatureType (..),
    SurfaceStatement (..),
  )
import JazzNext.Compiler.Parser.Context
  ( ParserContext (..),
    StatementContext (..),
  )
import JazzNext.Compiler.Parser.Declaration (parseStatementParser)
import JazzNext.Compiler.Parser.Expression
  ( parseExpressionParser,
  )
import JazzNext.Compiler.Parser.Failure
  ( ParserEncountered (..),
    ParserFailure (..),
    ParserFailureReason (..),
  )
import JazzNext.Compiler.Parser.Lexer
  ( Token (..),
    TokenKind (..),
  )
import JazzNext.Compiler.Parser.Operator
  ( Associativity (..),
    OperatorInfo (..),
  )
import JazzNext.Compiler.Parser.TestSupport
  ( lexSource,
  )
import JazzNext.Compiler.Parser.TokenParser
  ( runTokenParserPrefix,
    runTokenParserPrefixDetailed,
  )
import JazzNext.TestHarness
  ( NamedTest,
    assertContains,
    assertEqual,
    failTest,
    runTestSuite,
  )

main :: IO ()
main = runTestSuite "ExpressionParser" tests

tests :: [NamedTest]
tests =
  [ ("parses Unit as the empty tuple expression", testParsesUnitExpression),
    ("parses Char and Text expressions", testParsesCharAndTextExpressions),
    ("application binds tighter than infix precedence", testApplicationBeforeInfixPrecedence),
    ("declared operators participate in precedence climbing", testDeclaredOperatorPrecedence),
    ("parses qualified variables with list and tuple arguments", testQualifiedVariablesListsAndTuples),
    ("rejects whitespace after a qualified-name separator", testRejectsWhitespaceAfterQualifiedSeparator),
    ("parses control-flow and block expression starters", testControlFlowAndBlockExpressionStarters),
    ("uses known aliases for block statement disambiguation", testKnownAliasesDisambiguateBlockStatements),
    ("parses operator values and sections", testOperatorValuesAndSections),
    ("parses fractional literal suffix", testFractionalLiteralSuffix),
    ("reports the token that replaces a missing case body", testDetailedMissingCaseBody),
    ("reports invalid fractional literals structurally", testDetailedInvalidFractionalLiteral),
    ("reports invalid fractional literals", testInvalidFractionalLiteralDiagnostic),
    ("reports undeclared infix operators", testUndeclaredOperatorDiagnostic)
  ]

testParsesUnitExpression :: IO ()
testParsesUnitExpression = do
  tokens <- lexSource "()."
  assertExpression
    "Unit expression"
    (SETuple [])
    [TDot]
    (parseExpressionTokens Set.empty [] tokens)

testParsesCharAndTextExpressions :: IO ()
testParsesCharAndTextExpressions = do
  tokens <- lexSource "pair 'a' \"Jazz\"."
  assertExpression
    "Char/Text application"
    (SEApply (SEApply (SEVar "pair") (SELit (SLChar 'a'))) (SELit (SLText "Jazz")))
    [TDot]
    (parseExpressionTokens Set.empty [] tokens)

testApplicationBeforeInfixPrecedence :: IO ()
testApplicationBeforeInfixPrecedence = do
  tokens <- lexSource "f 1 + g 2 * 3."
  assertExpression
    "application before infix"
    ( SEBinary
        "+"
        (SEApply (SEVar "f") (SELit (SLInt 1)))
        (SEBinary "*" (SEApply (SEVar "g") (SELit (SLInt 2))) (SELit (SLInt 3)))
    )
    [TDot]
    (parseExpressionTokens Set.empty [] tokens)

testDeclaredOperatorPrecedence :: IO ()
testDeclaredOperatorPrecedence = do
  tokens <- lexSource "a %% b + c."
  assertExpression
    "declared operator precedence"
    (SEBinary "+" (SEBinary "%%" (SEVar "a") (SEVar "b")) (SEVar "c"))
    [TDot]
    (parseExpressionTokens Set.empty [OperatorInfo "%%" 5 AssocLeft] tokens)

testQualifiedVariablesListsAndTuples :: IO ()
testQualifiedVariablesListsAndTuples = do
  tokens <- lexSource "Alias::value [1, 2] (3, 4)."
  assertExpression
    "qualified variable list and tuple application"
    ( SEApply
        ( SEApply
            (SEQualifiedVar "Alias" "value")
            (SEList [SELit (SLInt 1), SELit (SLInt 2)])
        )
        (SETuple [SELit (SLInt 3), SELit (SLInt 4)])
    )
    [TDot]
    (parseExpressionTokens Set.empty [] tokens)

testRejectsWhitespaceAfterQualifiedSeparator :: IO ()
testRejectsWhitespaceAfterQualifiedSeparator = do
  tokens <- lexSource "Alias:: member."
  case parseExpressionTokensDetailed Set.empty [] tokens of
    Left failure -> do
      assertEqual "spaced qualified member span" (Just (SourceSpan 1 9)) (parserFailureSpan failure)
      assertEqual
        "spaced qualified member reason"
        ( ExpectedSyntax
            "adjacent member name after '::'"
            (ParserFoundToken (TIdentifier "member") "member")
        )
        (parserFailureReason failure)
    Right value ->
      failTest ("spaced qualified member: expected detailed Left, got Right " <> textShow value)

testControlFlowAndBlockExpressionStarters :: IO ()
testControlFlowAndBlockExpressionStarters = do
  ifTokens <- lexSource "if True then 1 else 2."
  assertExpression
    "if expression starter"
    (SEIf (SELit (SLBool True)) (SELit (SLInt 1)) (SELit (SLInt 2)))
    [TDot]
    (parseExpressionTokens Set.empty [] ifTokens)

  caseTokens <- lexSource "case value { | 0 -> 1 | _ -> 2 }."
  assertExpression
    "case expression starter"
    ( SECase
        (SEVar "value")
        [ SurfaceCaseArm (SPLiteral (SLInt 0)) Nothing (SELit (SLInt 1)),
          SurfaceCaseArm SPWildcard Nothing (SELit (SLInt 2))
        ]
    )
    [TDot]
    (parseExpressionTokens Set.empty [] caseTokens)

  lambdaTokens <- lexSource "\\(x) -> x."
  assertExpression
    "lambda expression starter"
    (SELambda (SurfaceLambdaIdentifier "x" :| []) (SEVar "x"))
    [TDot]
    (parseExpressionTokens Set.empty [] lambdaTokens)

  blockTokens <- lexSource "{ x = 1. x. }."
  assertExpression
    "block expression starter"
    ( SEBlock
        [ SSLet "x" (SourceSpan 1 3) (SELit (SLInt 1)),
          SSExpr (SourceSpan 1 10) (SEVar "x")
        ]
    )
    [TDot]
    (parseExpressionTokens Set.empty [] blockTokens)

testKnownAliasesDisambiguateBlockStatements :: IO ()
testKnownAliasesDisambiguateBlockStatements = do
  aliasTokens <- lexSource "{ Result::a. }."
  assertExpression
    "known alias parses compact qualified lookup in block"
    (SEBlock [SSExpr (SourceSpan 1 3) (SEQualifiedVar "Result" "a")])
    [TDot]
    (parseExpressionTokens (Set.singleton "Result") [] aliasTokens)

  nonAliasTokens <- lexSource "{ Result::a. }."
  assertExpression
    "unknown alias keeps compact signature in block"
    (SEBlock [SSSignature "Result" (SourceSpan 1 3) (SurfaceSignatureType (SurfaceTypeVariable "a"))])
    [TDot]
    (parseExpressionTokens Set.empty [] nonAliasTokens)

  qualifiedMethodTokens <- lexSource "{ Make::make. }."
  assertExpression
    "uppercase capability qualifier keeps multi-letter method lookup in block"
    (SEBlock [SSExpr (SourceSpan 1 3) (SEQualifiedVar "Make" "make")])
    [TDot]
    (parseExpressionTokens Set.empty [] qualifiedMethodTokens)

testOperatorValuesAndSections :: IO ()
testOperatorValuesAndSections = do
  tokens <- lexSource "(+) (10 +) (+ 20)."
  assertExpression
    "operator values and sections"
    ( SEApply
        (SEApply (SEOperatorValue "+") (SESectionLeft (SELit (SLInt 10)) "+"))
        (SESectionRight "+" (SELit (SLInt 20)))
    )
    [TDot]
    (parseExpressionTokens Set.empty [] tokens)

testFractionalLiteralSuffix :: IO ()
testFractionalLiteralSuffix = do
  tokens <- lexSource "1.25f32."
  assertExpression
    "fractional suffix"
    (SELit (SLFloat 1.25 (mkFractionalLiteralSource 1 25 2) (Just SurfaceNumericFloat32)))
    [TDot]
    (parseExpressionTokens Set.empty [] tokens)

testDetailedMissingCaseBody :: IO ()
testDetailedMissingCaseBody = do
  tokens <- lexSource "case value."
  case parseExpressionTokensDetailed Set.empty [] tokens of
    Left failure -> do
      assertEqual "missing case body span" (Just (SourceSpan 1 11)) (parserFailureSpan failure)
      assertEqual
        "missing case body reason"
        (ExpectedSyntax "'{'" (ParserFoundToken TDot "."))
        (parserFailureReason failure)
    Right value ->
      failTest ("missing case body: expected detailed Left, got Right " <> textShow value)

testInvalidFractionalLiteralDiagnostic :: IO ()
testInvalidFractionalLiteralDiagnostic = do
  tokens <- lexSource (Text.pack (replicate 400 '9' <> ".0."))
  case parseExpressionTokens Set.empty [] tokens of
    Left diagnostic ->
      assertContains "invalid fractional literal diagnostic" "invalid fractional literal" (renderDiagnostic diagnostic)
    Right value ->
      failTest ("invalid fractional literal: expected Left, got Right " <> textShow value)

testDetailedInvalidFractionalLiteral :: IO ()
testDetailedInvalidFractionalLiteral = do
  let literalText = Text.pack (replicate 400 '9' <> ".0")
  tokens <- lexSource (literalText <> ".")
  case parseExpressionTokensDetailed Set.empty [] tokens of
    Left failure -> do
      assertEqual "invalid fractional detailed span" (Just (SourceSpan 1 1)) (parserFailureSpan failure)
      assertEqual
        "invalid fractional detailed reason"
        (InvalidFractionalLiteral literalText)
        (parserFailureReason failure)
    Right value ->
      failTest ("invalid fractional literal: expected detailed Left, got Right " <> textShow value)

testUndeclaredOperatorDiagnostic :: IO ()
testUndeclaredOperatorDiagnostic = do
  tokens <- lexSource "a %% b."
  case parseExpressionTokens Set.empty [] tokens of
    Left diagnostic ->
      assertContains "undeclared operator diagnostic" "operator '%%' must be declared before use" (renderDiagnostic diagnostic)
    Right value ->
      failTest ("undeclared operator: expected Left, got Right " <> textShow value)

assertExpression ::
  Text ->
  SurfaceExpr ->
  [TokenKind] ->
  Either Diagnostic (SurfaceExpr, [Token]) ->
  IO ()
assertExpression label expectedExpr expectedRemainingKinds actual =
  assertEqual label (Right (expectedExpr, expectedRemainingKinds)) (fmap tokenKinds actual)

tokenKinds :: (SurfaceExpr, [Token]) -> (SurfaceExpr, [TokenKind])
tokenKinds (expr, remaining) = (expr, fmap tokenKind remaining)

textShow :: (Show a) => a -> Text
textShow = fromString . show

parseExpressionTokens :: Set.Set Text -> [OperatorInfo] -> [Token] -> Either Diagnostic (SurfaceExpr, [Token])
parseExpressionTokens knownAliases declaredOperators =
  runTokenParserPrefix "owned expression" (expressionParser initialContext)
  where
    initialContext =
      ParserContext
        { parserKnownAliases = knownAliases,
          parserDeclaredOperators = declaredOperators,
          parserStatementContext = NestedBlockContext
        }
    expressionParser = parseExpressionParser blockParser
    statementParser = parseStatementParser expressionParser blockParser
    blockParser = parseStatementsUntilBrace statementParser

parseExpressionTokensDetailed ::
  Set.Set Text ->
  [OperatorInfo] ->
  [Token] ->
  Either ParserFailure (SurfaceExpr, [Token])
parseExpressionTokensDetailed knownAliases declaredOperators =
  runTokenParserPrefixDetailed "owned expression" (expressionParser initialContext)
  where
    initialContext =
      ParserContext
        { parserKnownAliases = knownAliases,
          parserDeclaredOperators = declaredOperators,
          parserStatementContext = NestedBlockContext
        }
    expressionParser = parseExpressionParser blockParser
    statementParser = parseStatementParser expressionParser blockParser
    blockParser = parseStatementsUntilBrace statementParser

fromString :: String -> Text
fromString = Text.pack
