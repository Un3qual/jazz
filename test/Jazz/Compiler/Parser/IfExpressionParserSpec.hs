{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Jazz.Compiler.AST
  ( Expr (..),
    Literal (..),
    Statement (..)
  )
import Jazz.Compiler.Diagnostics
  ( SourceSpan (..)
  )
import Jazz.Compiler.Parser
  ( parseSurfaceProgram
  )
import Jazz.Compiler.Parser.AST
  ( SurfaceExpr (..),
    SurfaceLiteral (..),
    SurfaceStatement (..)
  )
import Jazz.Compiler.Parser.Lower
  ( lowerSurfaceExpr
  )
import Jazz.TestHarness
  ( NamedTest,
    assertEqual,
    assertLeftDiagnosticContains,
    assertRight,
    runTestSuite
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
    ("lowers parsed if surface nodes into analyzer AST", testLowerIfExpression),
    ("keeps lowered if nodes in canonical if form", testLoweredIfIsCanonical)
  ]

testParsesBasicIfExpression :: IO ()
testParsesBasicIfExpression =
  assertEqual
    "surface if AST"
    ( Right
        ( SEBlock
            [ SSLet
                "x"
                (SourceSpan 1 1)
                (SEIf (SELit (SLBool True)) (SELit (SLInt 1)) (SELit (SLInt 2)))
            ]
        )
    )
    (parseSurfaceProgram "x = if True then 1 else 2.")

testParsesNestedIfNearestElse :: IO ()
testParsesNestedIfNearestElse =
  assertEqual
    "nested if nearest else"
    ( Right
        ( SEBlock
            [ SSLet
                "x"
                (SourceSpan 1 1)
                ( SEIf
                    (SEVar "cond")
                    (SEIf (SEVar "inner") (SEVar "a") (SEVar "b"))
                    (SEVar "c")
                )
            ]
        )
    )
    (parseSurfaceProgram "x = if cond then if inner then a else b else c.")

testParsesIfInfixConditionBoundary :: IO ()
testParsesIfInfixConditionBoundary =
  assertEqual
    "if infix condition boundary"
    ( Right
        ( SEBlock
            [ SSLet
                "x"
                (SourceSpan 1 1)
                (SEIf (SEBinary ">" (SEVar "x") (SELit (SLInt 0))) (SELit (SLInt 1)) (SELit (SLInt 2)))
            ]
        )
    )
    (parseSurfaceProgram "x = if x > 0 then 1 else 2.")

testParsesIfApplicationConditionBoundary :: IO ()
testParsesIfApplicationConditionBoundary =
  assertEqual
    "if application condition boundary"
    ( Right
        ( SEBlock
            [ SSLet
                "x"
                (SourceSpan 1 1)
                (SEIf (SEApply (SEVar "predicate") (SEVar "subject")) (SEVar "yes") (SEVar "no"))
            ]
        )
    )
    (parseSurfaceProgram "x = if predicate subject then yes else no.")

testRejectsMissingThen :: IO ()
testRejectsMissingThen =
  assertLeftDiagnosticContains
    "missing then keyword"
    "expected 'then'"
    (parseSurfaceProgram "x = if cond yes else no.")

testRejectsMissingElse :: IO ()
testRejectsMissingElse =
  assertLeftDiagnosticContains
    "missing else branch"
    "expected 'else'"
    (parseSurfaceProgram "x = if cond then x.")

testRejectsExtraElse :: IO ()
testRejectsExtraElse =
  assertLeftDiagnosticContains
    "extra else branch"
    "expected '.'"
    (parseSurfaceProgram "x = if cond then x else y else z.")

testRejectsKeywordAsBindingName :: IO ()
testRejectsKeywordAsBindingName =
  assertLeftDiagnosticContains
    "keyword binding name"
    "expected expression"
    (parseSurfaceProgram "if = 1.")

testRejectsTrueAsBindingName :: IO ()
testRejectsTrueAsBindingName =
  assertLeftDiagnosticContains
    "True binding rejection"
    "reserved literal 'True' cannot be used as a binding name"
    (parseSurfaceProgram "True = 1.")

testRejectsFalseAsSignatureName :: IO ()
testRejectsFalseAsSignatureName =
  assertLeftDiagnosticContains
    "False signature rejection"
    "reserved literal 'False' cannot be used as a binding name"
    (parseSurfaceProgram "False :: Bool.")

testLowerIfExpression :: IO ()
testLowerIfExpression =
  assertRight
    "parse + lower if"
    (parseSurfaceProgram "x = if True then 1 else 2.")
    (\surfaceProgram -> assertEqual "lowered if AST" expectedProgram (lowerSurfaceExpr surfaceProgram))
  where
    expectedProgram =
      EBlock
        [ SLet
            "x"
            (SourceSpan 1 1)
            (EIf (ELit (LBool True)) (ELit (LInt 1)) (ELit (LInt 2)))
        ]

testLoweredIfIsCanonical :: IO ()
testLoweredIfIsCanonical =
  assertRight
    "parse + canonical lower if"
    (parseSurfaceProgram "x = if True then 1 else 2.")
    ( \surfaceProgram ->
        assertEqual
          "canonical lowered if AST"
          expectedProgram
          (lowerSurfaceExpr surfaceProgram)
    )
  where
    expectedProgram =
      EBlock
        [ SLet
            "x"
            (SourceSpan 1 1)
            (EIf (ELit (LBool True)) (ELit (LInt 1)) (ELit (LInt 2)))
        ]
