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
  [ ("declared tier 2 operator inherits additive precedence", testDeclaredTier2OperatorPrecedence),
    ("declared tier 5 operator inherits dollar associativity", testDeclaredTier5OperatorAssociativity),
    ("declared arrow-prefixed operator parses as a single user operator", testDeclaredArrowPrefixedOperator),
    ("declared operator value and sections parse after declaration", testDeclaredOperatorValueAndSections),
    ("multiplication binds tighter than addition", testMultiplicationBeforeAddition),
    ("equality binds looser than arithmetic", testEqualityAfterArithmetic),
    ("dollar is right associative", testDollarRightAssociative),
    ("subtraction is left associative", testSubtractionLeftAssociative),
    ("same-precedence arithmetic operators associate left", testSamePrecedenceArithmeticAssociatesLeft),
    ("application binds tighter than infix operators", testApplicationBeforeInfix),
    ("operator value application participates in infix precedence", testOperatorValueApplicationBeforeInfix),
    ("lowering preserves parsed fixity tree", testLowerFixityTree)
  ]

testDeclaredTier2OperatorPrecedence :: IO ()
testDeclaredTier2OperatorPrecedence =
  assertEqual
    "declared tier 2 fixity tree"
    ( Right
        ( SEBlock
            [ SSLet
                "x"
                (SourceSpan 2 1)
                ( SEBinary
                    "%%"
                    (SEBinary "+" (SELit (SLInt 1)) (SELit (SLInt 2)))
                    (SEBinary "*" (SELit (SLInt 3)) (SELit (SLInt 4)))
                )
            ]
        )
    )
    (parseSurfaceProgram "operator %% tier 2.\nx = 1 + 2 %% 3 * 4.")

testDeclaredTier5OperatorAssociativity :: IO ()
testDeclaredTier5OperatorAssociativity =
  assertEqual
    "declared tier 5 associativity"
    ( Right
        ( SEBlock
            [ SSLet
                "x"
                (SourceSpan 2 1)
                (SEBinary "~~" (SEVar "f") (SEBinary "~~" (SEVar "g") (SEVar "z")))
            ]
        )
    )
    (parseSurfaceProgram "operator ~~ tier 5.\nx = f ~~ g ~~ z.")

testDeclaredArrowPrefixedOperator :: IO ()
testDeclaredArrowPrefixedOperator =
  assertEqual
    "declared arrow-prefixed operator"
    ( Right
        ( SEBlock
            [ SSLet
                "x"
                (SourceSpan 2 1)
                (SEBinary "->?" (SELit (SLInt 1)) (SELit (SLInt 2)))
            ]
        )
    )
    (parseSurfaceProgram "operator ->? tier 4.\nx = 1 ->? 2.")

testDeclaredOperatorValueAndSections :: IO ()
testDeclaredOperatorValueAndSections =
  assertEqual
    "declared operator values and sections"
    ( Right
        ( SEBlock
            [ SSLet "op" (SourceSpan 2 1) (SEOperatorValue "%%"),
              SSLet "left" (SourceSpan 3 1) (SESectionLeft (SELit (SLInt 10)) "%%"),
              SSLet "right" (SourceSpan 4 1) (SESectionRight "%%" (SELit (SLInt 10)))
            ]
        )
    )
    (parseSurfaceProgram "operator %% tier 2.\nop = (%%).\nleft = (10 %%).\nright = (%% 10).")

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

testSamePrecedenceArithmeticAssociatesLeft :: IO ()
testSamePrecedenceArithmeticAssociatesLeft =
  assertEqual
    "same-precedence arithmetic associativity"
    ( Right
        ( SEBlock
            [ SSLet
                "x"
                (SourceSpan 1 1)
                (SEBinary "-" (SEBinary "+" (SELit (SLInt 1)) (SELit (SLInt 2))) (SELit (SLInt 3)))
            ]
        )
    )
    (parseSurfaceProgram "x = 1 + 2 - 3.")

testApplicationBeforeInfix :: IO ()
testApplicationBeforeInfix =
  assertEqual
    "application before infix"
    ( Right
        ( SEBlock
            [ SSLet
                "x"
                (SourceSpan 1 1)
                ( SEBinary
                    "+"
                    (SEApply (SEVar "f") (SEVar "x"))
                    (SEBinary "*" (SEApply (SEVar "g") (SEVar "y")) (SEVar "z"))
                )
            ]
        )
    )
    (parseSurfaceProgram "x = f x + g y * z.")

testOperatorValueApplicationBeforeInfix :: IO ()
testOperatorValueApplicationBeforeInfix =
  assertEqual
    "operator value application before infix"
    ( Right
        ( SEBlock
            [ SSLet
                "x"
                (SourceSpan 1 1)
                ( SEBinary
                    "*"
                    (SEApply (SEApply (SEOperatorValue "+") (SELit (SLInt 1))) (SELit (SLInt 2)))
                    (SELit (SLInt 3))
                )
            ]
        )
    )
    (parseSurfaceProgram "x = (+) 1 2 * 3.")

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
