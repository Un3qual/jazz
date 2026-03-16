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
main = runTestSuite "OperatorFixity" tests

tests :: [NamedTest]
tests =
  [ ("multiplication binds tighter than addition", testMultiplicationBeforeAddition),
    ("equality binds looser than arithmetic", testEqualityAfterArithmetic),
    ("dollar is right associative", testDollarRightAssociative),
    ("subtraction is left associative", testSubtractionLeftAssociative),
    ("lowering preserves parsed fixity tree", testLowerFixityTree)
  ]

testMultiplicationBeforeAddition :: IO ()
testMultiplicationBeforeAddition =
  assertEqual
    "fixity tree"
    ( Right
        ( SEBlock
            [ SSLet
                "x"
                (SourceSpan 1 1)
                (SEBinary "+" (SELit (SLInt 1)) (SEBinary "*" (SELit (SLInt 2)) (SELit (SLInt 3))))
            ]
        )
    )
    (parseSurfaceProgram "x = 1 + 2 * 3.")

testEqualityAfterArithmetic :: IO ()
testEqualityAfterArithmetic =
  assertEqual
    "comparison precedence"
    ( Right
        ( SEBlock
            [ SSLet
                "ok"
                (SourceSpan 1 1)
                (SEBinary "==" (SEBinary "+" (SELit (SLInt 1)) (SELit (SLInt 2))) (SELit (SLInt 3)))
            ]
        )
    )
    (parseSurfaceProgram "ok = 1 + 2 == 3.")

testDollarRightAssociative :: IO ()
testDollarRightAssociative =
  assertEqual
    "dollar associativity"
    ( Right
        ( SEBlock
            [ SSLet
                "x"
                (SourceSpan 1 1)
                (SEBinary "$" (SEVar "f") (SEBinary "$" (SEVar "g") (SEVar "z")))
            ]
        )
    )
    (parseSurfaceProgram "x = f $ g $ z.")

testSubtractionLeftAssociative :: IO ()
testSubtractionLeftAssociative =
  assertEqual
    "subtraction associativity"
    ( Right
        ( SEBlock
            [ SSLet
                "x"
                (SourceSpan 1 1)
                (SEBinary "-" (SEBinary "-" (SELit (SLInt 10)) (SELit (SLInt 3))) (SELit (SLInt 1)))
            ]
        )
    )
    (parseSurfaceProgram "x = 10 - 3 - 1.")

testLowerFixityTree :: IO ()
testLowerFixityTree =
  assertRight
    "parse + lower fixity"
    (parseSurfaceProgram "x = 1 + 2 * 3.")
    (\surfaceProgram -> assertEqual "lowered AST" expectedProgram (lowerSurfaceExpr surfaceProgram))
  where
    expectedProgram =
      EBlock
        [ SLet
            "x"
            (SourceSpan 1 1)
            (EBinary "+" (ELit (LInt 1)) (EBinary "*" (ELit (LInt 2)) (ELit (LInt 3))))
        ]
