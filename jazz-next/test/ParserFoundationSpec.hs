{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Text (Text)
import JazzNext.Compiler.AST
  ( Expr (..),
    Literal (..),
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
    ("ignores hash line comments between statements", testIgnoresHashLineComments),
    ("tracks tab-aligned expression spans", testTabAlignedExpressionSpan),
    ("parses nested scope expression", testParseNestedScopeExpression),
    ("lowers parsed surface AST into analyzer AST", testLowerSurfaceProgram),
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
            [ SSSignature "x" (SourceSpan 1 1) "Int",
              SSLet "x" (SourceSpan 2 1) (SELit (SLInt 1))
            ]
        )
    )
    (parseSurfaceProgram "x :: Int.\nx = 1.")

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
