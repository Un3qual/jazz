{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import JazzNext.Compiler.Diagnostics
  ( Diagnostic,
    renderDiagnostic
  )
import JazzNext.Compiler.FractionalLiteral
  ( mkFractionalLiteralSource
  )
import JazzNext.Compiler.Parser.AST
  ( SurfaceExpr (..),
    SurfaceLiteral (..),
    SurfaceNumericType (..)
  )
import JazzNext.Compiler.Parser.Expression
  ( parseExpressionTokens
  )
import JazzNext.Compiler.Parser.Lexer
  ( Token (..),
    TokenKind (..),
    tokenize
  )
import JazzNext.Compiler.Parser.Operator
  ( Associativity (..),
    OperatorInfo (..)
  )
import JazzNext.TestHarness
  ( NamedTest,
    assertContains,
    assertEqual,
    failTest,
    runTestSuite
  )

main :: IO ()
main = runTestSuite "ExpressionParser" tests

tests :: [NamedTest]
tests =
  [ ("application binds tighter than infix precedence", testApplicationBeforeInfixPrecedence),
    ("declared operators participate in precedence climbing", testDeclaredOperatorPrecedence),
    ("parses qualified variables with list and tuple arguments", testQualifiedVariablesListsAndTuples),
    ("parses operator values and sections", testOperatorValuesAndSections),
    ("parses fractional literal suffix", testFractionalLiteralSuffix),
    ("reports invalid fractional literals", testInvalidFractionalLiteralDiagnostic),
    ("reports undeclared infix operators", testUndeclaredOperatorDiagnostic)
  ]

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

testInvalidFractionalLiteralDiagnostic :: IO ()
testInvalidFractionalLiteralDiagnostic = do
  tokens <- lexSource (Text.pack (replicate 400 '9' <> ".0."))
  case parseExpressionTokens Set.empty [] tokens of
    Left diagnostic ->
      assertContains "invalid fractional literal diagnostic" "invalid fractional literal" (renderDiagnostic diagnostic)
    Right value ->
      failTest ("invalid fractional literal: expected Left, got Right " <> textShow value)

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

lexSource :: Text -> IO [Token]
lexSource source =
  case tokenize source of
    Right tokens -> pure tokens
    Left diagnostic -> failTest ("tokenize: expected Right, got " <> renderDiagnostic diagnostic)

textShow :: Show a => a -> Text
textShow = fromString . show

fromString :: String -> Text
fromString = Text.pack
