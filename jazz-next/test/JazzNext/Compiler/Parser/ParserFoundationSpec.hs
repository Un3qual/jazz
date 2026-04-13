{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Text (Text)
import JazzNext.Compiler.AST
  ( Expr (..),
    Literal (..),
    SignaturePayload (..),
    SignatureType (..),
    Statement (..)
  )
import JazzNext.Compiler.Diagnostics
  ( SourceSpan (..)
  )
import JazzNext.Compiler.Parser
  ( parseSurfaceProgram
  )
import JazzNext.Compiler.Parser.AST
  ( SurfaceExpr (..),
    SurfaceLiteral (..),
    SurfaceSignaturePayload (..),
    SurfaceSignatureType (..),
    SurfaceStatement (..)
  )
import JazzNext.Compiler.Parser.Lower
  ( lowerSurfaceExpr
  )
import JazzNext.TestHarness
  ( NamedTest,
    assertEqual,
    assertLeftDiagnosticContains,
    assertRight,
    runTestSuite
  )

main :: IO ()
main = runTestSuite "ParserFoundation" tests

tests :: [NamedTest]
tests =
  [ ("parses let binding and expression statement", testParseLetAndExpr),
    ("parseSurfaceProgram accepts Text input", testParseSurfaceProgramAcceptsTextInput),
    ("parses signature statement with source span", testParseSignatureSpan),
    ("parses parenthesized function signature into structured nodes", testParseParenthesizedFunctionSignature),
    ("parses chained function signature right associatively", testParseChainedFunctionSignature),
    ("parses parenthesized function override into structured nodes", testParseParenthesizedFunctionOverrideSignature),
    ("parses list of parenthesized function types", testParseFunctionListSignature),
    ("ignores hash line comments between statements", testIgnoresHashLineComments),
    ("tracks tab-aligned expression spans", testTabAlignedExpressionSpan),
    ("parses nested scope expression", testParseNestedScopeExpression),
    ("lowers parsed surface AST into analyzer AST", testLowerSurfaceProgram),
    ("lowers structured signature payload into analyzer AST", testLowerStructuredSignatureProgram),
    ("lowers right-associated function signature into analyzer AST", testLowerRightAssociativeFunctionSignatureProgram),
    ("lowers list of function signature into analyzer AST", testLowerFunctionListSignatureProgram),
    ("rejects missing statement terminator", testRejectsMissingDotTerminator),
    ("rejects signature missing terminator before next statement", testRejectsMissingSignatureDot),
    ("rejects integer literal overflow", testRejectsIntOverflow),
    ("rejects negative literal syntax for now", testRejectsNegativeLiteralSyntax)
  ]

testParseLetAndExpr :: IO ()
testParseLetAndExpr =
  assertEqual
    "surface AST"
    ( Right
        ( SEBlock
            [ SSLet "x" (SourceSpan 1 1) (SELit (SLInt 1)),
              SSExpr (SourceSpan 2 1) (SEVar "x")
            ]
        )
    )
    (parseSurfaceProgram "x = 1.\nx.")

testParseSurfaceProgramAcceptsTextInput :: IO ()
testParseSurfaceProgramAcceptsTextInput = do
  let sourceText :: Text
      sourceText = "x = 1.\nx."
  assertEqual
    "surface AST from Text source"
    ( Right
        ( SEBlock
            [ SSLet "x" (SourceSpan 1 1) (SELit (SLInt 1)),
              SSExpr (SourceSpan 2 1) (SEVar "x")
            ]
        )
    )
    (parseSurfaceProgram sourceText)

testParseSignatureSpan :: IO ()
testParseSignatureSpan =
  assertEqual
    "signature span"
    ( Right
        ( SEBlock
            [ SSSignature "x" (SourceSpan 1 1) (SurfaceSignatureType (SurfaceTypeInt)),
              SSLet "x" (SourceSpan 2 1) (SELit (SLInt 1))
            ]
        )
    )
    (parseSurfaceProgram "x :: Int.\nx = 1.")

testParseParenthesizedFunctionSignature :: IO ()
testParseParenthesizedFunctionSignature =
  assertEqual
    "parenthesized function signature"
    ( Right
        ( SEBlock
            [ SSSignature
                "f"
                (SourceSpan 1 1)
                ( SurfaceSignatureType
                    (SurfaceTypeFunction
                      (SurfaceTypeList SurfaceTypeInt)
                      (SurfaceTypeList SurfaceTypeInt)
                    )
                ),
              SSLet "f" (SourceSpan 2 1) (SEOperatorValue "+")
            ]
        )
    )
    (parseSurfaceProgram "f :: ([Int]) -> ([Int]).\nf = (+).")

testParseChainedFunctionSignature :: IO ()
testParseChainedFunctionSignature =
  assertEqual
    "right-associated function signature"
    ( Right
        ( SEBlock
            [ SSSignature
                "f"
                (SourceSpan 1 1)
                ( SurfaceSignatureType
                    (SurfaceTypeFunction SurfaceTypeInt (SurfaceTypeFunction SurfaceTypeInt SurfaceTypeInt))
                ),
              SSLet "f" (SourceSpan 2 1) (SEOperatorValue "+")
            ]
        )
    )
    (parseSurfaceProgram "f :: Int -> Int -> Int.\nf = (+).")

testParseParenthesizedFunctionOverrideSignature :: IO ()
testParseParenthesizedFunctionOverrideSignature =
  assertEqual
    "parenthesized function override signature"
    ( Right
        ( SEBlock
            [ SSSignature
                "f"
                (SourceSpan 1 1)
                ( SurfaceSignatureType
                    (SurfaceTypeFunction (SurfaceTypeFunction SurfaceTypeInt SurfaceTypeInt) SurfaceTypeInt)
                ),
              SSLet "f" (SourceSpan 2 1) (SEVar "applyToOne")
            ]
        )
    )
    (parseSurfaceProgram "f :: (Int -> Int) -> Int.\nf = applyToOne.")

testParseFunctionListSignature :: IO ()
testParseFunctionListSignature =
  assertEqual
    "list of parenthesized function types"
    ( Right
        ( SEBlock
            [ SSSignature
                "fns"
                (SourceSpan 1 1)
                ( SurfaceSignatureType
                    (SurfaceTypeList (SurfaceTypeFunction SurfaceTypeInt SurfaceTypeInt))
                ),
              SSLet "fns" (SourceSpan 2 1) (SEList [SESectionRight "+" (SELit (SLInt 1))])
            ]
        )
    )
    (parseSurfaceProgram "fns :: [(Int -> Int)].\nfns = [(+ 1)].")

testIgnoresHashLineComments :: IO ()
testIgnoresHashLineComments =
  assertEqual
    "comments ignored"
    ( Right
        ( SEBlock
            [ SSLet "x" (SourceSpan 1 1) (SELit (SLInt 1)),
              SSExpr (SourceSpan 3 1) (SEVar "x")
            ]
        )
    )
    (parseSurfaceProgram "x = 1.\n# parser should ignore this line comment\nx.")

testTabAlignedExpressionSpan :: IO ()
testTabAlignedExpressionSpan =
  assertEqual
    "tab-aligned span"
    ( Right
        ( SEBlock
            [ SSExpr (SourceSpan 1 9) (SEVar "x")
            ]
        )
    )
    (parseSurfaceProgram "\tx.")

testParseNestedScopeExpression :: IO ()
testParseNestedScopeExpression =
  assertEqual
    "nested block AST"
    ( Right
        ( SEBlock
            [ SSLet "x" (SourceSpan 1 1) (SELit (SLInt 1)),
              SSExpr
                (SourceSpan 2 1)
                ( SEBlock
                    [SSExpr (SourceSpan 2 3) (SEVar "x")]
                )
            ]
        )
    )
    (parseSurfaceProgram "x = 1.\n{ x. }.")

testLowerSurfaceProgram :: IO ()
testLowerSurfaceProgram =
  assertRight
    "parse + lower"
    (parseSurfaceProgram "x = 1.\nx.")
    (\surfaceProgram -> assertEqual "lowered AST" expectedProgram (lowerSurfaceExpr surfaceProgram))
  where
    expectedProgram =
      EBlock
        [ SLet "x" (SourceSpan 1 1) (ELit (LInt 1)),
          SExpr (SourceSpan 2 1) (EVar "x")
        ]

testLowerStructuredSignatureProgram :: IO ()
testLowerStructuredSignatureProgram =
  assertRight
    "parse + lower structured signature"
    (parseSurfaceProgram "x :: [[Bool]].\nx = [[True], [False]].")
    ( \surfaceProgram ->
        assertEqual
          "lowered signature AST"
          ( EBlock
              [ SSignature
                  "x"
                  (SourceSpan 1 1)
                  (SignatureType (TypeList (TypeList TypeBool))),
                SLet
                  "x"
                  (SourceSpan 2 1)
                  (EList [EList [ELit (LBool True)], EList [ELit (LBool False)]])
              ]
          )
          (lowerSurfaceExpr surfaceProgram)
    )

testLowerRightAssociativeFunctionSignatureProgram :: IO ()
testLowerRightAssociativeFunctionSignatureProgram =
  assertRight
    "parse + lower right-associated function signature"
    (parseSurfaceProgram "f :: Int -> Int -> Int.\nf = (+).")
    ( \surfaceProgram ->
        assertEqual
          "lowered right-associated signature AST"
          ( EBlock
              [ SSignature
                  "f"
                  (SourceSpan 1 1)
                  (SignatureType (TypeFunction TypeInt (TypeFunction TypeInt TypeInt))),
                SLet "f" (SourceSpan 2 1) (EOperatorValue "+")
              ]
          )
          (lowerSurfaceExpr surfaceProgram)
    )

testLowerFunctionListSignatureProgram :: IO ()
testLowerFunctionListSignatureProgram =
  assertRight
    "parse + lower list of function signature"
    (parseSurfaceProgram "fns :: [(Int -> Int)].\nfns = [(+ 1)].")
    ( \surfaceProgram ->
        assertEqual
          "lowered list of function signature AST"
          ( EBlock
              [ SSignature
                  "fns"
                  (SourceSpan 1 1)
                  (SignatureType (TypeList (TypeFunction TypeInt TypeInt))),
                SLet
                  "fns"
                  (SourceSpan 2 1)
                  (EList [ESectionRight "+" (ELit (LInt 1))])
              ]
          )
          (lowerSurfaceExpr surfaceProgram)
    )

testRejectsMissingDotTerminator :: IO ()
testRejectsMissingDotTerminator =
  assertLeftDiagnosticContains
    "missing dot error"
    "expected '.'"
    (parseSurfaceProgram "x = 1 y = 2.")

testRejectsMissingSignatureDot :: IO ()
testRejectsMissingSignatureDot =
  assertLeftDiagnosticContains
    "missing signature dot error"
    "expected '.'"
    (parseSurfaceProgram "x :: Int\nx = 1.")

testRejectsIntOverflow :: IO ()
testRejectsIntOverflow =
  assertLeftDiagnosticContains
    "integer overflow"
    "integer literal out of range"
    (parseSurfaceProgram "x = 9999999999999999999999999999999999999.")

testRejectsNegativeLiteralSyntax :: IO ()
testRejectsNegativeLiteralSyntax =
  assertLeftDiagnosticContains
    "negative literal unsupported"
    "expected expression"
    (parseSurfaceProgram "x = -1.")
