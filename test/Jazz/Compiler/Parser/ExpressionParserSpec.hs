{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Jazz.Compiler.Diagnostics
  ( Diagnostic,
    SourceSpan (..),
  )
import Jazz.Compiler.Diagnostics.Render
  ( renderDiagnostic,
  )
import Jazz.Compiler.FractionalLiteral
  ( mkFractionalLiteralSource,
  )
import Jazz.Compiler.Parser (parseStatementsUntilBrace)
import Jazz.Compiler.Parser.AST
  ( Literal (..),
    SurfaceCaseArm (..),
    SurfaceExpr (..),
    SurfaceExprForm (..),
    SurfaceLambdaParameter (..),
    SurfacePattern (..),
    SurfacePatternForm (..),
    SurfaceStatement (..),
  )
import Jazz.Compiler.Parser.Context
  ( ParserContext (..),
    StatementContext (..),
  )
import Jazz.Compiler.Parser.Declaration (parseStatementParser)
import Jazz.Compiler.Parser.Expression
  ( parseExpressionParser,
  )
import Jazz.Compiler.Parser.Failure
  ( ParserEncountered (..),
    ParserFailure (..),
    ParserFailureReason (..),
  )
import Jazz.Compiler.Parser.Lexer
  ( Token (..),
    TokenKind (..),
  )
import Jazz.Compiler.Parser.Operator
  ( Associativity (..),
    OperatorInfo (..),
    operatorTableFromDeclarations,
  )
import Jazz.Compiler.Parser.TestSupport
  ( lexSource,
    surfaceExprAt,
    surfacePatternAt,
  )
import Jazz.Compiler.Parser.TokenParser
  ( runTokenParserPrefix,
    runTokenParserPrefixDetailed,
  )
import Jazz.Compiler.TypeRepresentation
  ( NumericType (..),
    SignaturePayload (..),
    SignatureType (..),
  )
import Jazz.TestHarness
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
    ("keeps fractional case bodies before later arms", testFractionalCaseBodyBeforeLaterArm),
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
    (e 1 1 (SETuple []))
    [TDot]
    (parseExpressionTokens Set.empty [] tokens)

testParsesCharAndTextExpressions :: IO ()
testParsesCharAndTextExpressions = do
  tokens <- lexSource "pair 'a' \"Jazz\"."
  assertExpression
    "Char/Text application"
    (e 1 1 (SEApply (e 1 1 (SEApply (e 1 1 (SEVar "pair")) (e 1 6 (SELit (LChar 'a'))))) (e 1 10 (SELit (LText "Jazz")))))
    [TDot]
    (parseExpressionTokens Set.empty [] tokens)

testApplicationBeforeInfixPrecedence :: IO ()
testApplicationBeforeInfixPrecedence = do
  tokens <- lexSource "f 1 + g 2 * 3."
  assertExpression
    "application before infix"
    ( e
        1
        1
        ( SEBinary
            "+"
            (e 1 1 (SEApply (e 1 1 (SEVar "f")) (e 1 3 (SELit (LInt 1)))))
            ( e
                1
                7
                ( SEBinary
                    "*"
                    (e 1 7 (SEApply (e 1 7 (SEVar "g")) (e 1 9 (SELit (LInt 2)))))
                    (e 1 13 (SELit (LInt 3)))
                )
            )
        )
    )
    [TDot]
    (parseExpressionTokens Set.empty [] tokens)

testDeclaredOperatorPrecedence :: IO ()
testDeclaredOperatorPrecedence = do
  tokens <- lexSource "a %% b + c."
  assertExpression
    "declared operator precedence"
    (e 1 1 (SEBinary "+" (e 1 1 (SEBinary "%%" (e 1 1 (SEVar "a")) (e 1 6 (SEVar "b")))) (e 1 10 (SEVar "c"))))
    [TDot]
    (parseExpressionTokens Set.empty [OperatorInfo "%%" 5 AssocLeft] tokens)

testQualifiedVariablesListsAndTuples :: IO ()
testQualifiedVariablesListsAndTuples = do
  tokens <- lexSource "Alias::member [1, 2] (3, 4)."
  assertExpression
    "qualified variable list and tuple application"
    ( e
        1
        1
        ( SEApply
            ( e
                1
                1
                ( SEApply
                    (e 1 1 (SEQualifiedVar "Alias" "member"))
                    (e 1 15 (SEList [e 1 16 (SELit (LInt 1)), e 1 19 (SELit (LInt 2))]))
                )
            )
            (e 1 22 (SETuple [e 1 23 (SELit (LInt 3)), e 1 26 (SELit (LInt 4))]))
        )
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
    (e 1 1 (SEIf (e 1 4 (SELit (LBool True))) (e 1 14 (SELit (LInt 1))) (e 1 21 (SELit (LInt 2)))))
    [TDot]
    (parseExpressionTokens Set.empty [] ifTokens)

  caseTokens <- lexSource "case subject { | 0 -> 1 | _ -> 2 }."
  assertExpression
    "case expression starter"
    ( e
        1
        1
        ( SECase
            (e 1 6 (SEVar "subject"))
            [ SurfaceCaseArm (p 1 18 (SPLiteral (LInt 0))) Nothing (e 1 23 (SELit (LInt 1))),
              SurfaceCaseArm (p 1 27 SPWildcard) Nothing (e 1 32 (SELit (LInt 2)))
            ]
        )
    )
    [TDot]
    (parseExpressionTokens Set.empty [] caseTokens)

  lambdaTokens <- lexSource "\\(x) -> x."
  assertExpression
    "lambda expression starter"
    (e 1 1 (SELambda (SurfaceLambdaIdentifier (SourceSpan 1 3) "x" :| []) (e 1 9 (SEVar "x"))))
    [TDot]
    (parseExpressionTokens Set.empty [] lambdaTokens)

  blockTokens <- lexSource "{ x = 1. x. }."
  assertExpression
    "block expression starter"
    ( e
        1
        1
        ( SEBlock
            [ SSLet "x" (SourceSpan 1 3) (e 1 7 (SELit (LInt 1))),
              SSExpr (SourceSpan 1 10) (e 1 10 (SEVar "x"))
            ]
        )
    )
    [TDot]
    (parseExpressionTokens Set.empty [] blockTokens)

testKnownAliasesDisambiguateBlockStatements :: IO ()
testKnownAliasesDisambiguateBlockStatements = do
  aliasTokens <- lexSource "{ Result::a. }."
  assertExpression
    "known alias parses compact qualified lookup in block"
    (e 1 1 (SEBlock [SSExpr (SourceSpan 1 3) (e 1 3 (SEQualifiedVar "Result" "a"))]))
    [TDot]
    (parseExpressionTokens (Set.singleton "Result") [] aliasTokens)

  nonAliasTokens <- lexSource "{ Result::a. }."
  assertExpression
    "unknown alias keeps compact signature in block"
    (e 1 1 (SEBlock [SSSignature "Result" (SourceSpan 1 3) (SignatureType (TypeVariable "a"))]))
    [TDot]
    (parseExpressionTokens Set.empty [] nonAliasTokens)

  qualifiedMethodTokens <- lexSource "{ Make::make. }."
  assertExpression
    "uppercase capability qualifier keeps multi-letter method lookup in block"
    (e 1 1 (SEBlock [SSExpr (SourceSpan 1 3) (e 1 3 (SEQualifiedVar "Make" "make"))]))
    [TDot]
    (parseExpressionTokens Set.empty [] qualifiedMethodTokens)

testFractionalCaseBodyBeforeLaterArm :: IO ()
testFractionalCaseBodyBeforeLaterArm = do
  tokens <- lexSource "case 0 { | _ -> 1.2 | _ -> 3 }."
  assertExpression
    "fractional case body before later arm"
    ( e
        1
        1
        ( SECase
            (e 1 6 (SELit (LInt 0)))
            [ SurfaceCaseArm
                (p 1 12 SPWildcard)
                Nothing
                (e 1 17 (SELit (LFloat 1.2 (mkFractionalLiteralSource 1 2 1) Nothing))),
              SurfaceCaseArm (p 1 23 SPWildcard) Nothing (e 1 28 (SELit (LInt 3)))
            ]
        )
    )
    [TDot]
    (parseExpressionTokens Set.empty [] tokens)

testOperatorValuesAndSections :: IO ()
testOperatorValuesAndSections = do
  tokens <- lexSource "(+) (10 +) (+ 20)."
  assertExpression
    "operator values and sections"
    ( e
        1
        1
        ( SEApply
            (e 1 1 (SEApply (e 1 1 (SEOperatorValue "+")) (e 1 5 (SESectionLeft (e 1 6 (SELit (LInt 10))) "+"))))
            (e 1 12 (SESectionRight "+" (e 1 15 (SELit (LInt 20)))))
        )
    )
    [TDot]
    (parseExpressionTokens Set.empty [] tokens)

testFractionalLiteralSuffix :: IO ()
testFractionalLiteralSuffix = do
  tokens <- lexSource "1.25f32."
  assertExpression
    "fractional suffix"
    (e 1 1 (SELit (LFloat 1.25 (mkFractionalLiteralSource 1 25 2) (Just NumericFloat32))))
    [TDot]
    (parseExpressionTokens Set.empty [] tokens)

testDetailedMissingCaseBody :: IO ()
testDetailedMissingCaseBody = do
  tokens <- lexSource "case subject."
  case parseExpressionTokensDetailed Set.empty [] tokens of
    Left failure -> do
      assertEqual "missing case body span" (Just (SourceSpan 1 13)) (parserFailureSpan failure)
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

e :: Int -> Int -> SurfaceExprForm -> SurfaceExpr
e = surfaceExprAt

p :: Int -> Int -> SurfacePatternForm -> SurfacePattern
p = surfacePatternAt

parseExpressionTokens :: Set.Set Text -> [OperatorInfo] -> [Token] -> Either Diagnostic (SurfaceExpr, [Token])
parseExpressionTokens knownAliases declaredOperators =
  runTokenParserPrefix "owned expression" (expressionParser initialContext)
  where
    initialContext =
      ParserContext
        { parserKnownAliases = knownAliases,
          parserDeclaredOperators = operatorTableFromDeclarations declaredOperators,
          parserStatementContext = NestedBlockContext
        }
    expressionParser = parseExpressionParser blockParser
    statementParser = parseStatementParser (parserDeclaredOperators initialContext) expressionParser blockParser
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
          parserDeclaredOperators = operatorTableFromDeclarations declaredOperators,
          parserStatementContext = NestedBlockContext
        }
    expressionParser = parseExpressionParser blockParser
    statementParser = parseStatementParser (parserDeclaredOperators initialContext) expressionParser blockParser
    blockParser = parseStatementsUntilBrace statementParser

fromString :: String -> Text
fromString = Text.pack
