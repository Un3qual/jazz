module Main (main) where

import JazzNext.Compiler.AST
  ( Expr (..),
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
    SurfaceStatement (..)
  )
import JazzNext.Compiler.Parser.Lower
  ( lowerSurfaceExpr
  )
import JazzNext.TestHarness
  ( NamedTest,
    assertEqual,
    assertLeftContains,
    assertRight,
    runTestSuite
  )

main :: IO ()
main = runTestSuite "ParserFoundation" tests

tests :: [NamedTest]
tests =
  [ ("parses let binding and expression statement", testParseLetAndExpr),
    ("parses signature statement with source span", testParseSignatureSpan),
    ("parses nested scope expression", testParseNestedScopeExpression),
    ("lowers parsed surface AST into analyzer AST", testLowerSurfaceProgram),
    ("rejects missing statement terminator", testRejectsMissingDotTerminator),
    ("rejects signature missing terminator before next statement", testRejectsMissingSignatureDot),
    ("rejects integer literal overflow", testRejectsIntOverflow)
  ]

testParseLetAndExpr :: IO ()
testParseLetAndExpr =
  assertEqual
    "surface AST"
    ( Right
        ( SEScope
            [ SSLet "x" (SourceSpan 1 1) (SEInt 1),
              SSExpr (SourceSpan 2 1) (SEVar "x")
            ]
        )
    )
    (parseSurfaceProgram "x = 1.\nx.")

testParseSignatureSpan :: IO ()
testParseSignatureSpan =
  assertEqual
    "signature span"
    ( Right
        ( SEScope
            [ SSSignature "x" (SourceSpan 1 1) "Int",
              SSLet "x" (SourceSpan 2 1) (SEInt 1)
            ]
        )
    )
    (parseSurfaceProgram "x :: Int.\nx = 1.")

testParseNestedScopeExpression :: IO ()
testParseNestedScopeExpression =
  assertEqual
    "nested scope AST"
    ( Right
        ( SEScope
            [ SSLet "x" (SourceSpan 1 1) (SEInt 1),
              SSExpr
                (SourceSpan 2 1)
                ( SEScope
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
      EScope
        [ SLet "x" (SourceSpan 1 1) (EInt 1),
          SExpr (EVar "x")
        ]

testRejectsMissingDotTerminator :: IO ()
testRejectsMissingDotTerminator =
  assertLeftContains
    "missing dot error"
    "expected '.'"
    (parseSurfaceProgram "x = 1\nx.")

testRejectsMissingSignatureDot :: IO ()
testRejectsMissingSignatureDot =
  assertLeftContains
    "missing signature dot error"
    "expected '.'"
    (parseSurfaceProgram "x :: Int\nx = 1.")

testRejectsIntOverflow :: IO ()
testRejectsIntOverflow =
  assertLeftContains
    "integer overflow"
    "integer literal out of range"
    (parseSurfaceProgram "x = 9999999999999999999999999999999999999.")
