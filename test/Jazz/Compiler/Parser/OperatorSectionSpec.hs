{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Jazz.Compiler.AST
  ( Literal (..),
  )
import Jazz.Compiler.Diagnostics
  ( SourceSpan (..),
  )
import Jazz.Compiler.Parser.AST
  ( SurfaceExpr (..),
    SurfaceExprForm (..),
    SurfaceStatement (..),
  )
import Jazz.Compiler.Parser.Lower
  ( lowerSurfaceExpr,
  )
import Jazz.TestCore
  ( assertLoweredCoreEqual,
    loweredBlock,
    loweredLet,
    loweredLiteral,
    loweredOperatorValue,
    loweredSectionLeft,
    loweredSectionRight,
    parseSurfaceProgramPoints,
  )
import Jazz.TestHarness
  ( NamedTest,
    assertEqual,
    assertRight,
    runTestSuite,
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
    ("section application binds before infix operators", testSectionApplicationBeforeInfix),
    ("lowering preserves bare operator value nodes", testLowerPreservesBareOperatorValue),
    ("lowered bare operator values need no post-pass", testLoweredBareOperatorValueIsCanonical),
    ("lowering preserves explicit left section nodes", testLowerPreservesLeftSectionNodes),
    ("lowering preserves explicit section nodes", testLowerPreservesSectionNodes)
  ]

testParsesBareOperatorValue :: IO ()
testParsesBareOperatorValue =
  assertEqual
    "bare operator value AST"
    ( Right
        ( e
            1
            1
            ( SEBlock
                [SSLet "f" (SourceSpan 1 1) (e 1 5 (SEOperatorValue "+"))]
            )
        )
    )
    (parseSurfaceProgramPoints "f = (+).")

testParsesBareOperatorValueApplication :: IO ()
testParsesBareOperatorValueApplication =
  assertEqual
    "bare operator value application AST"
    ( Right
        ( e
            1
            1
            ( SEBlock
                [ SSLet
                    "f"
                    (SourceSpan 1 1)
                    ( e
                        1
                        5
                        ( SEApply
                            (e 1 5 (SEApply (e 1 5 (SEOperatorValue "+")) (e 1 9 (SELit (LInt 1)))))
                            (e 1 11 (SELit (LInt 2)))
                        )
                    )
                ]
            )
        )
    )
    (parseSurfaceProgramPoints "f = (+) 1 2.")

testParsesLeftSection :: IO ()
testParsesLeftSection =
  assertEqual
    "left section AST"
    ( Right
        ( e
            1
            1
            (SEBlock [SSLet "f" (SourceSpan 1 1) (e 1 5 (SESectionLeft (e 1 6 (SELit (LInt 10))) "+"))])
        )
    )
    (parseSurfaceProgramPoints "f = (10 +).")

testParsesRightSection :: IO ()
testParsesRightSection =
  assertEqual
    "right section AST"
    ( Right
        ( e
            1
            1
            (SEBlock [SSLet "f" (SourceSpan 1 1) (e 1 5 (SESectionRight "+" (e 1 8 (SELit (LInt 10)))))])
        )
    )
    (parseSurfaceProgramPoints "f = (+ 10).")

testGroupedExpressionIsNotSection :: IO ()
testGroupedExpressionIsNotSection =
  assertEqual
    "grouped binary expression"
    ( Right
        ( e
            1
            1
            (SEBlock [SSLet "x" (SourceSpan 1 1) (e 1 6 (SEBinary "+" (e 1 6 (SELit (LInt 1))) (e 1 10 (SELit (LInt 2)))))])
        )
    )
    (parseSurfaceProgramPoints "x = (1 + 2).")

testSectionApplicationBeforeInfix :: IO ()
testSectionApplicationBeforeInfix =
  assertEqual
    "section application before infix"
    ( Right
        ( e
            1
            1
            ( SEBlock
                [ SSLet
                    "x"
                    (SourceSpan 1 1)
                    ( e
                        1
                        5
                        ( SEBinary
                            "*"
                            (e 1 5 (SEApply (e 1 5 (SESectionRight "+" (e 1 8 (SELit (LInt 1))))) (e 1 11 (SELit (LInt 2)))))
                            (e 1 15 (SELit (LInt 3)))
                        )
                    )
                ]
            )
        )
    )
    (parseSurfaceProgramPoints "x = (+ 1) 2 * 3.")

testLowerPreservesLeftSectionNodes :: IO ()
testLowerPreservesLeftSectionNodes =
  assertRight
    "parse + lower left section"
    (parseSurfaceProgramPoints "f = (10 +).")
    (\surfaceProgram -> assertLoweredCoreEqual "lowered AST" expectedProgram (lowerSurfaceExpr surfaceProgram))
  where
    expectedProgram =
      loweredBlock
        [ loweredLet "f" (SourceSpan 1 1) (loweredSectionLeft (loweredLiteral (LInt 10)) "+")
        ]

testLowerPreservesSectionNodes :: IO ()
testLowerPreservesSectionNodes =
  assertRight
    "parse + lower section"
    (parseSurfaceProgramPoints "f = (+ 10).")
    (\surfaceProgram -> assertLoweredCoreEqual "lowered AST" expectedProgram (lowerSurfaceExpr surfaceProgram))
  where
    expectedProgram =
      loweredBlock
        [ loweredLet "f" (SourceSpan 1 1) (loweredSectionRight "+" (loweredLiteral (LInt 10)))
        ]

testLowerPreservesBareOperatorValue :: IO ()
testLowerPreservesBareOperatorValue =
  assertRight
    "parse + lower bare operator value"
    (parseSurfaceProgramPoints "f = (+).")
    (\surfaceProgram -> assertLoweredCoreEqual "lowered AST" expectedProgram (lowerSurfaceExpr surfaceProgram))
  where
    expectedProgram =
      loweredBlock
        [ loweredLet "f" (SourceSpan 1 1) (loweredOperatorValue "+")
        ]

testLoweredBareOperatorValueIsCanonical :: IO ()
testLoweredBareOperatorValueIsCanonical =
  assertRight
    "parse + canonical lower bare operator value"
    (parseSurfaceProgramPoints "f = (+).")
    (\surfaceProgram -> assertLoweredCoreEqual "canonical lowered AST" expectedProgram (lowerSurfaceExpr surfaceProgram))
  where
    expectedProgram =
      loweredBlock
        [ loweredLet "f" (SourceSpan 1 1) (loweredOperatorValue "+")
        ]

e :: Int -> Int -> SurfaceExprForm -> SurfaceExpr
e line column = SurfaceExpr (SourceSpan line column)
