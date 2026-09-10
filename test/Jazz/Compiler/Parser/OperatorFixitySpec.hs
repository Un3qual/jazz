{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Jazz.Compiler.AST
  ( Literal (..),
  )
import Jazz.Compiler.Diagnostics
  ( SourceSpan (..),
  )
import Jazz.Compiler.Name
  ( Identifier,
  )
import Jazz.Compiler.Parser.AST
  ( SurfaceExpr (..),
    SurfaceExprForm (..),
    SurfaceLambdaParameter (..),
    SurfaceLiteral (..),
    SurfaceStatement (..),
  )
import Jazz.Compiler.Parser.Lower
  ( lowerSurfaceExpr,
  )
import Jazz.Compiler.TypeRepresentation
  ( SignaturePayload (..),
    SignatureType (..),
  )
import Jazz.TestCore
  ( assertLoweredCoreEqual,
    loweredBinary,
    loweredBlock,
    loweredLet,
    loweredLiteral,
    parseSurfaceProgramPoints,
  )
import Jazz.TestHarness
  ( NamedTest,
    assertEqual,
    assertRight,
    runTestSuite,
  )

main :: IO ()
main = runTestSuite "OperatorFixity" tests

tests :: [NamedTest]
tests =
  [ ("declared tier 2 operator inherits additive precedence", testDeclaredTier2OperatorPrecedence),
    ("declared custom precedence operator binds tighter than builtins", testDeclaredCustomPrecedenceOperatorPrecedence),
    ("declared custom precedence operator defaults left associative", testDeclaredCustomPrecedenceOperatorAssociativity),
    ("declared operator accepts explicit left associativity", testDeclaredOperatorExplicitLeftAssociativity),
    ("declared operator accepts explicit right associativity", testDeclaredOperatorExplicitRightAssociativity),
    ("declared operator binding parses as hidden ordinary binding", testDeclaredOperatorBindingParsesAsHiddenBinding),
    ("declared operator signature parses as hidden ordinary signature", testDeclaredOperatorSignatureParsesAsHiddenSignature),
    ("declared operator binding parses inside module body", testDeclaredOperatorBindingParsesInsideModuleBody),
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
        ( blockAt
            1
            1
            [ SSLet
                "x"
                (SourceSpan 2 1)
                ( binary
                    "%%"
                    (binary "+" (intAt 2 5 1) (intAt 2 9 2))
                    (binary "*" (intAt 2 14 3) (intAt 2 18 4))
                )
            ]
        )
    )
    ( parseSurfaceProgramPoints
        """
        operator %% tier 2.
        x = 1 + 2 %% 3 * 4.
        """
    )

testDeclaredCustomPrecedenceOperatorPrecedence :: IO ()
testDeclaredCustomPrecedenceOperatorPrecedence =
  assertEqual
    "declared custom precedence fixity tree"
    ( Right
        ( blockAt
            1
            1
            [ SSLet
                "x"
                (SourceSpan 2 1)
                ( binary
                    "+"
                    (intAt 2 5 1)
                    (binary "*" (binary "%%" (intAt 2 9 2) (intAt 2 14 3)) (intAt 2 18 4))
                )
            ]
        )
    )
    ( parseSurfaceProgramPoints
        """
        operator %% precedence 99.
        x = 1 + 2 %% 3 * 4.
        """
    )

testDeclaredCustomPrecedenceOperatorAssociativity :: IO ()
testDeclaredCustomPrecedenceOperatorAssociativity =
  assertEqual
    "declared custom precedence left associativity"
    ( Right
        ( blockAt
            1
            1
            [ SSLet
                "x"
                (SourceSpan 2 1)
                (binary "%%" (binary "%%" (intAt 2 5 10) (intAt 2 11 3)) (intAt 2 16 1))
            ]
        )
    )
    ( parseSurfaceProgramPoints
        """
        operator %% precedence 25.
        x = 10 %% 3 %% 1.
        """
    )

testDeclaredOperatorExplicitLeftAssociativity :: IO ()
testDeclaredOperatorExplicitLeftAssociativity =
  assertEqual
    "declared explicit left associativity"
    ( Right
        ( blockAt
            1
            1
            [ SSLet
                "x"
                (SourceSpan 2 1)
                (binary "%%" (binary "%%" (intAt 2 5 10) (intAt 2 11 3)) (intAt 2 16 1))
            ]
        )
    )
    ( parseSurfaceProgramPoints
        """
        operator %% tier 2 left.
        x = 10 %% 3 %% 1.
        """
    )

testDeclaredOperatorExplicitRightAssociativity :: IO ()
testDeclaredOperatorExplicitRightAssociativity =
  assertEqual
    "declared explicit right associativity"
    ( Right
        ( blockAt
            1
            1
            [ SSLet
                "x"
                (SourceSpan 2 1)
                (binary "<|" (varAt 2 5 "a") (binary "<|" (varAt 2 10 "b") (varAt 2 15 "c")))
            ]
        )
    )
    ( parseSurfaceProgramPoints
        """
        operator <| precedence 10 right.
        x = a <| b <| c.
        """
    )

testDeclaredOperatorBindingParsesAsHiddenBinding :: IO ()
testDeclaredOperatorBindingParsesAsHiddenBinding =
  assertEqual
    "declared operator binding parse tree"
    ( Right
        ( blockAt
            1
            1
            [ SSLet
                "$operator:%25%25"
                (SourceSpan 2 2)
                ( e
                    2
                    8
                    ( SELambda
                        (SurfaceLambdaIdentifier (SourceSpan 2 10) "left" :| [SurfaceLambdaIdentifier (SourceSpan 2 16) "right"])
                        (binary "+" (varAt 2 26 "left") (varAt 2 33 "right"))
                    )
                ),
              SSLet
                "result"
                (SourceSpan 3 1)
                (binary "%%" (intAt 3 10 1) (binary "*" (intAt 3 15 2) (intAt 3 19 3)))
            ]
        )
    )
    ( parseSurfaceProgramPoints
        """
        operator %% tier 2.
        (%%) = \\(left, right) -> left + right.
        result = 1 %% 2 * 3.
        """
    )

testDeclaredOperatorSignatureParsesAsHiddenSignature :: IO ()
testDeclaredOperatorSignatureParsesAsHiddenSignature =
  assertEqual
    "declared operator signature parse tree"
    ( Right
        ( blockAt
            1
            1
            [ SSSignature
                "$operator:%25%25"
                (SourceSpan 2 2)
                ( SignatureType
                    ( TypeFunction
                        TypeInt
                        (TypeFunction TypeInt TypeInt)
                    )
                ),
              SSLet
                "$operator:%25%25"
                (SourceSpan 3 2)
                ( e
                    3
                    8
                    ( SELambda
                        (SurfaceLambdaIdentifier (SourceSpan 3 10) "left" :| [SurfaceLambdaIdentifier (SourceSpan 3 16) "right"])
                        (binary "+" (varAt 3 26 "left") (varAt 3 33 "right"))
                    )
                ),
              SSLet
                "result"
                (SourceSpan 4 1)
                (binary "%%" (intAt 4 10 1) (intAt 4 15 2))
            ]
        )
    )
    ( parseSurfaceProgramPoints
        """
        operator %% tier 2.
        (%%) :: Int -> Int -> Int.
        (%%) = \\(left, right) -> left + right.
        result = 1 %% 2.
        """
    )

testDeclaredOperatorBindingParsesInsideModuleBody :: IO ()
testDeclaredOperatorBindingParsesInsideModuleBody =
  assertEqual
    "declared operator binding in module body parse tree"
    ( Right
        ( blockAt
            1
            1
            [ SSModule (SourceSpan 1 1) ["Demo"] Nothing,
              SSLet
                "$operator:%25%25"
                (SourceSpan 3 2)
                ( e
                    3
                    8
                    ( SELambda
                        (SurfaceLambdaIdentifier (SourceSpan 3 10) "left" :| [SurfaceLambdaIdentifier (SourceSpan 3 16) "right"])
                        (binary "+" (varAt 3 26 "left") (varAt 3 33 "right"))
                    )
                )
            ]
        )
    )
    ( parseSurfaceProgramPoints
        """
        module Demo {
        operator %% tier 2.
        (%%) = \\(left, right) -> left + right.
        }
        """
    )

testDeclaredTier5OperatorAssociativity :: IO ()
testDeclaredTier5OperatorAssociativity =
  assertEqual
    "declared tier 5 associativity"
    ( Right
        ( blockAt
            1
            1
            [ SSLet
                "x"
                (SourceSpan 2 1)
                (binary "~~" (varAt 2 5 "f") (binary "~~" (varAt 2 10 "g") (varAt 2 15 "z")))
            ]
        )
    )
    ( parseSurfaceProgramPoints
        """
        operator ~~ tier 5.
        x = f ~~ g ~~ z.
        """
    )

testDeclaredArrowPrefixedOperator :: IO ()
testDeclaredArrowPrefixedOperator =
  assertEqual
    "declared arrow-prefixed operator"
    ( Right
        ( blockAt
            1
            1
            [ SSLet
                "x"
                (SourceSpan 2 1)
                (binary "->?" (intAt 2 5 1) (intAt 2 11 2))
            ]
        )
    )
    ( parseSurfaceProgramPoints
        """
        operator ->? tier 4.
        x = 1 ->? 2.
        """
    )

testDeclaredOperatorValueAndSections :: IO ()
testDeclaredOperatorValueAndSections =
  assertEqual
    "declared operator values and sections"
    ( Right
        ( blockAt
            1
            1
            [ SSLet "op" (SourceSpan 2 1) (e 2 6 (SEOperatorValue "%%")),
              SSLet "left" (SourceSpan 3 1) (e 3 8 (SESectionLeft (intAt 3 9 10) "%%")),
              SSLet "right" (SourceSpan 4 1) (e 4 9 (SESectionRight "%%" (intAt 4 13 10)))
            ]
        )
    )
    ( parseSurfaceProgramPoints
        """
        operator %% tier 2.
        op = (%%).
        left = (10 %%).
        right = (%% 10).
        """
    )

testMultiplicationBeforeAddition :: IO ()
testMultiplicationBeforeAddition =
  assertEqual
    "fixity tree"
    ( Right
        ( blockAt
            1
            1
            [ SSLet
                "x"
                (SourceSpan 1 1)
                (binary "+" (intAt 1 5 1) (binary "*" (intAt 1 9 2) (intAt 1 13 3)))
            ]
        )
    )
    (parseSurfaceProgramPoints "x = 1 + 2 * 3.")

testEqualityAfterArithmetic :: IO ()
testEqualityAfterArithmetic =
  assertEqual
    "comparison precedence"
    ( Right
        ( blockAt
            1
            1
            [ SSLet
                "ok"
                (SourceSpan 1 1)
                (binary "==" (binary "+" (intAt 1 6 1) (intAt 1 10 2)) (intAt 1 15 3))
            ]
        )
    )
    (parseSurfaceProgramPoints "ok = 1 + 2 == 3.")

testDollarRightAssociative :: IO ()
testDollarRightAssociative =
  assertEqual
    "dollar associativity"
    ( Right
        ( blockAt
            1
            1
            [ SSLet
                "x"
                (SourceSpan 1 1)
                (binary "$" (varAt 1 5 "f") (binary "$" (varAt 1 9 "g") (varAt 1 13 "z")))
            ]
        )
    )
    (parseSurfaceProgramPoints "x = f $ g $ z.")

testSubtractionLeftAssociative :: IO ()
testSubtractionLeftAssociative =
  assertEqual
    "subtraction associativity"
    ( Right
        ( blockAt
            1
            1
            [ SSLet
                "x"
                (SourceSpan 1 1)
                (binary "-" (binary "-" (intAt 1 5 10) (intAt 1 10 3)) (intAt 1 14 1))
            ]
        )
    )
    (parseSurfaceProgramPoints "x = 10 - 3 - 1.")

testSamePrecedenceArithmeticAssociatesLeft :: IO ()
testSamePrecedenceArithmeticAssociatesLeft =
  assertEqual
    "same-precedence arithmetic associativity"
    ( Right
        ( blockAt
            1
            1
            [ SSLet
                "x"
                (SourceSpan 1 1)
                (binary "-" (binary "+" (intAt 1 5 1) (intAt 1 9 2)) (intAt 1 13 3))
            ]
        )
    )
    (parseSurfaceProgramPoints "x = 1 + 2 - 3.")

testApplicationBeforeInfix :: IO ()
testApplicationBeforeInfix =
  assertEqual
    "application before infix"
    ( Right
        ( blockAt
            1
            1
            [ SSLet
                "x"
                (SourceSpan 1 1)
                ( binary
                    "+"
                    (apply (varAt 1 5 "f") (varAt 1 7 "x"))
                    (binary "*" (apply (varAt 1 11 "g") (varAt 1 13 "y")) (varAt 1 17 "z"))
                )
            ]
        )
    )
    (parseSurfaceProgramPoints "x = f x + g y * z.")

testOperatorValueApplicationBeforeInfix :: IO ()
testOperatorValueApplicationBeforeInfix =
  assertEqual
    "operator value application before infix"
    ( Right
        ( blockAt
            1
            1
            [ SSLet
                "x"
                (SourceSpan 1 1)
                ( binary
                    "*"
                    (apply (apply (e 1 5 (SEOperatorValue "+")) (intAt 1 9 1)) (intAt 1 11 2))
                    (intAt 1 15 3)
                )
            ]
        )
    )
    (parseSurfaceProgramPoints "x = (+) 1 2 * 3.")

testLowerFixityTree :: IO ()
testLowerFixityTree =
  assertRight
    "parse + lower fixity"
    (parseSurfaceProgramPoints "x = 1 + 2 * 3.")
    (\surfaceProgram -> assertLoweredCoreEqual "lowered AST" expectedProgram (lowerSurfaceExpr surfaceProgram))
  where
    expectedProgram =
      loweredBlock
        [ loweredLet
            "x"
            (SourceSpan 1 1)
            (loweredBinary "+" (loweredLiteral (LInt 1)) (loweredBinary "*" (loweredLiteral (LInt 2)) (loweredLiteral (LInt 3))))
        ]

blockAt :: Int -> Int -> [SurfaceStatement] -> SurfaceExpr
blockAt line column = e line column . SEBlock

intAt :: Int -> Int -> Integer -> SurfaceExpr
intAt line column = e line column . SELit . SLInt

varAt :: Int -> Int -> Identifier -> SurfaceExpr
varAt line column name = e line column (SEVar name)

binary :: Text -> SurfaceExpr -> SurfaceExpr -> SurfaceExpr
binary operator left right = SurfaceExpr (surfaceExprSpan left) (SEBinary operator left right)

apply :: SurfaceExpr -> SurfaceExpr -> SurfaceExpr
apply functionExpr argumentExpr = SurfaceExpr (surfaceExprSpan functionExpr) (SEApply functionExpr argumentExpr)

e :: Int -> Int -> SurfaceExprForm -> SurfaceExpr
e line column = SurfaceExpr (SourceSpan line column)
