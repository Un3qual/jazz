{-# LANGUAGE OverloadedStrings #-}

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
    assertRight,
    runTestSuite
  )

main :: IO ()
main = runTestSuite "OperatorSection" tests

tests :: [NamedTest]
tests =
  [ ("parses bare operator value form", testParsesBareOperatorValue),
    ("parses bare operator value application", testParsesBareOperatorValueApplication),
    ("parses left section form", testParsesLeftSection),
    ("parses right section form", testParsesRightSection),
    ("grouped infix expression is not treated as section", testGroupedExpressionIsNotSection),
    ("lowering preserves bare operator value nodes", testLowerPreservesBareOperatorValue),
    ("lowering preserves explicit section nodes", testLowerPreservesSectionNodes)
  ]

testParsesBareOperatorValue :: IO ()
testParsesBareOperatorValue =
  assertEqual
    "bare operator value AST"
    ( Right
        ( SEScope
            [ SSLet "f" (SourceSpan 1 1) (SEOperatorValue "+")
            ]
        )
    )
    (parseSurfaceProgram "f = (+).")

testParsesBareOperatorValueApplication :: IO ()
testParsesBareOperatorValueApplication =
  assertEqual
    "bare operator value application AST"
    ( Right
        ( SEScope
            [ SSLet
                "f"
                (SourceSpan 1 1)
                (SEApply (SEApply (SEOperatorValue "+") (SEInt 1)) (SEInt 2))
            ]
        )
    )
    (parseSurfaceProgram "f = (+) 1 2.")

testParsesLeftSection :: IO ()
testParsesLeftSection =
  assertEqual
    "left section AST"
    ( Right
        ( SEScope
            [ SSLet "f" (SourceSpan 1 1) (SESectionLeft (SEInt 10) "+")
            ]
        )
    )
    (parseSurfaceProgram "f = (10 +).")

testParsesRightSection :: IO ()
testParsesRightSection =
  assertEqual
    "right section AST"
    ( Right
        ( SEScope
            [ SSLet "f" (SourceSpan 1 1) (SESectionRight "+" (SEInt 10))
            ]
        )
    )
    (parseSurfaceProgram "f = (+ 10).")

testGroupedExpressionIsNotSection :: IO ()
testGroupedExpressionIsNotSection =
  assertEqual
    "grouped binary expression"
    ( Right
        ( SEScope
            [ SSLet "x" (SourceSpan 1 1) (SEBinary "+" (SEInt 1) (SEInt 2))
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
      EScope
        [ SLet "f" (SourceSpan 1 1) (ESectionRight "+" (EInt 10))
        ]

testLowerPreservesBareOperatorValue :: IO ()
testLowerPreservesBareOperatorValue =
  assertRight
    "parse + lower bare operator value"
    (parseSurfaceProgram "f = (+).")
    (\surfaceProgram -> assertEqual "lowered AST" expectedProgram (lowerSurfaceExpr surfaceProgram))
  where
    expectedProgram =
      EScope
        [ SLet "f" (SourceSpan 1 1) (EOperatorValue "+")
        ]
