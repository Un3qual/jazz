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
    loweredIf,
    loweredLet,
    loweredLiteral,
    parseSurfaceProgramPoints,
  )
import Jazz.TestHarness
  ( NamedTest,
    assertEqual,
    assertLeftDiagnosticContains,
    assertRight,
    runTestSuite,
  )

main :: IO ()
main = runTestSuite "IfExpressionParser" tests

tests :: [NamedTest]
tests =
  [ ("parses basic if expression", testParsesBasicIfExpression),
    ("parses nested if with nearest else binding", testParsesNestedIfNearestElse),
    ("parses if with infix condition up to then", testParsesIfInfixConditionBoundary),
    ("parses application in an if condition up to then", testParsesIfApplicationConditionBoundary),
    ("rejects the old if syntax without then", testRejectsMissingThen),
    ("rejects missing else branch", testRejectsMissingElse),
    ("rejects extra else branch", testRejectsExtraElse),
    ("treats if and else as reserved keywords", testRejectsKeywordAsBindingName),
    ("rejects True as binding name", testRejectsTrueAsBindingName),
    ("rejects False as signature name", testRejectsFalseAsSignatureName),
    ("lowers parsed if surface nodes into canonical core", testLowerIfExpression)
  ]

testParsesBasicIfExpression :: IO ()
testParsesBasicIfExpression =
  assertEqual
    "surface if AST"
    ( Right
        ( e
            1
            1
            ( SEBlock
                [ SSLet
                    "x"
                    (SourceSpan 1 1)
                    (e 1 5 (SEIf (e 1 8 (SELit (LBool True))) (e 1 18 (SELit (LInt 1))) (e 1 25 (SELit (LInt 2)))))
                ]
            )
        )
    )
    (parseSurfaceProgramPoints "x = if True then 1 else 2.")

testParsesNestedIfNearestElse :: IO ()
testParsesNestedIfNearestElse =
  assertEqual
    "nested if nearest else"
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
                        ( SEIf
                            (e 1 8 (SEVar "cond"))
                            (e 1 18 (SEIf (e 1 21 (SEVar "inner")) (e 1 32 (SEVar "a")) (e 1 39 (SEVar "b"))))
                            (e 1 46 (SEVar "c"))
                        )
                    )
                ]
            )
        )
    )
    (parseSurfaceProgramPoints "x = if cond then if inner then a else b else c.")

testParsesIfInfixConditionBoundary :: IO ()
testParsesIfInfixConditionBoundary =
  assertEqual
    "if infix condition boundary"
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
                        ( SEIf
                            (e 1 8 (SEBinary ">" (e 1 8 (SEVar "x")) (e 1 12 (SELit (LInt 0)))))
                            (e 1 19 (SELit (LInt 1)))
                            (e 1 26 (SELit (LInt 2)))
                        )
                    )
                ]
            )
        )
    )
    (parseSurfaceProgramPoints "x = if x > 0 then 1 else 2.")

testParsesIfApplicationConditionBoundary :: IO ()
testParsesIfApplicationConditionBoundary =
  assertEqual
    "if application condition boundary"
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
                        ( SEIf
                            (e 1 8 (SEApply (e 1 8 (SEVar "predicate")) (e 1 18 (SEVar "subject"))))
                            (e 1 31 (SEVar "yes"))
                            (e 1 40 (SEVar "no"))
                        )
                    )
                ]
            )
        )
    )
    (parseSurfaceProgramPoints "x = if predicate subject then yes else no.")

testRejectsMissingThen :: IO ()
testRejectsMissingThen =
  assertLeftDiagnosticContains
    "missing then keyword"
    "expected 'then'"
    (parseSurfaceProgramPoints "x = if cond yes else no.")

testRejectsMissingElse :: IO ()
testRejectsMissingElse =
  assertLeftDiagnosticContains
    "missing else branch"
    "expected 'else'"
    (parseSurfaceProgramPoints "x = if cond then x.")

testRejectsExtraElse :: IO ()
testRejectsExtraElse =
  assertLeftDiagnosticContains
    "extra else branch"
    "expected '.'"
    (parseSurfaceProgramPoints "x = if cond then x else y else z.")

testRejectsKeywordAsBindingName :: IO ()
testRejectsKeywordAsBindingName =
  assertLeftDiagnosticContains
    "keyword binding name"
    "expected expression"
    (parseSurfaceProgramPoints "if = 1.")

testRejectsTrueAsBindingName :: IO ()
testRejectsTrueAsBindingName =
  assertLeftDiagnosticContains
    "True binding rejection"
    "reserved literal 'True' cannot be used as a binding name"
    (parseSurfaceProgramPoints "True = 1.")

testRejectsFalseAsSignatureName :: IO ()
testRejectsFalseAsSignatureName =
  assertLeftDiagnosticContains
    "False signature rejection"
    "reserved literal 'False' cannot be used as a binding name"
    (parseSurfaceProgramPoints "False :: Bool.")

testLowerIfExpression :: IO ()
testLowerIfExpression =
  assertRight
    "parse + lower if"
    (parseSurfaceProgramPoints "x = if True then 1 else 2.")
    (\surfaceProgram -> assertLoweredCoreEqual "lowered if AST" expectedProgram (lowerSurfaceExpr surfaceProgram))
  where
    expectedProgram =
      loweredBlock
        [ loweredLet
            "x"
            (SourceSpan 1 1)
            (loweredIf (loweredLiteral (LBool True)) (loweredLiteral (LInt 1)) (loweredLiteral (LInt 2)))
        ]

e :: Int -> Int -> SurfaceExprForm -> SurfaceExpr
e line column = SurfaceExpr (SourceSpan line column)
