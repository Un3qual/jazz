{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

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
    assertRight,
    runTestSuite
  )

main :: IO ()
main = runTestSuite "OperatorSection" tests

tests :: [NamedTest]
tests =
  [ ("parses left section form", testParsesLeftSection),
    ("parses right section form", testParsesRightSection),
    ("grouped infix expression is not treated as section", testGroupedExpressionIsNotSection),
    ("lowering preserves explicit section nodes", testLowerPreservesSectionNodes)
  ]

testParsesLeftSection :: IO ()
testParsesLeftSection =
  assertEqual
    "left section AST"
    ( Right
        ( SEBlock
            [ SSLet "f" (SourceSpan 1 1) (SESectionLeft (SELit (SLInt 10)) "+")
            ]
        )
    )
    (parseSurfaceProgram "f = (10 +).")

testParsesRightSection :: IO ()
testParsesRightSection =
  assertEqual
    "right section AST"
    ( Right
        ( SEBlock
            [ SSLet "f" (SourceSpan 1 1) (SESectionRight "+" (SELit (SLInt 10)))
            ]
        )
    )
    (parseSurfaceProgram "f = (+ 10).")

testGroupedExpressionIsNotSection :: IO ()
testGroupedExpressionIsNotSection =
  assertEqual
    "grouped binary expression"
    ( Right
        ( SEBlock
            [ SSLet "x" (SourceSpan 1 1) (SEBinary "+" (SELit (SLInt 1)) (SELit (SLInt 2)))
            ]
        )
    )
    (parseSurfaceProgram "x = (1 + 2).")

testLowerPreservesSectionNodes :: IO ()
testLowerPreservesSectionNodes =
  assertRight
    "parse + lower section"
    (parseSurfaceProgram "f = (+ 10).")
    (\surfaceProgram -> assertEqual "lowered AST" expectedProgram (lowerSurfaceExpr surfaceProgram))
  where
    expectedProgram =
      EBlock
        [ SLet "f" (SourceSpan 1 1) (ESectionRight "+" (ELit (LInt 10)))
        ]
