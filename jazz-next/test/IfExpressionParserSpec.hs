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
import JazzNext.Compiler.Desugar
  ( desugarExpr
  )
import JazzNext.TestHarness
  ( NamedTest,
    assertEqual,
    assertLeftContains,
    assertRight,
    runTestSuite
  )

main :: IO ()
main = runTestSuite "IfExpressionParser" tests

tests :: [NamedTest]
tests =
  [ ("parses basic if expression", testParsesBasicIfExpression),
    ("parses nested if with nearest else binding", testParsesNestedIfNearestElse),
    ("parses if with infix condition without swallowing then branch", testParsesIfInfixConditionBoundary),
    ("rejects missing else branch", testRejectsMissingElse),
    ("rejects extra else branch", testRejectsExtraElse),
    ("treats if and else as reserved keywords", testRejectsKeywordAsBindingName),
    ("rejects True as binding name", testRejectsTrueAsBindingName),
    ("rejects False as signature name", testRejectsFalseAsSignatureName),
    ("lowers parsed if surface nodes into analyzer AST", testLowerIfExpression),
    ("desugars lowered if nodes into canonical case form", testDesugarIfExpression)
  ]

testParsesBasicIfExpression :: IO ()
testParsesBasicIfExpression =
  assertEqual
    "surface if AST"
    ( Right
        ( SEScope
            [ SSLet
                "x"
                (SourceSpan 1 1)
                (SEIf (SEBool True) (SEInt 1) (SEInt 2))
            ]
        )
    )
    (parseSurfaceProgram "x = if True 1 else 2.")

testParsesNestedIfNearestElse :: IO ()
testParsesNestedIfNearestElse =
  assertEqual
    "nested if nearest else"
    ( Right
        ( SEScope
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
    (parseSurfaceProgram "x = if cond if inner a else b else c.")

testParsesIfInfixConditionBoundary :: IO ()
testParsesIfInfixConditionBoundary =
  assertEqual
    "if infix condition boundary"
    ( Right
        ( SEScope
            [ SSLet
                "x"
                (SourceSpan 1 1)
                (SEIf (SEBinary ">" (SEVar "x") (SEInt 0)) (SEInt 1) (SEInt 2))
            ]
        )
    )
    (parseSurfaceProgram "x = if x > 0 1 else 2.")

testRejectsMissingElse :: IO ()
testRejectsMissingElse =
  assertLeftContains
    "missing else branch"
    "expected 'else'"
    (parseSurfaceProgram "x = if cond x.")

testRejectsExtraElse :: IO ()
testRejectsExtraElse =
  assertLeftContains
    "extra else branch"
    "expected '.'"
    (parseSurfaceProgram "x = if cond x else y else z.")

testRejectsKeywordAsBindingName :: IO ()
testRejectsKeywordAsBindingName =
  assertLeftContains
    "keyword binding name"
    "expected expression"
    (parseSurfaceProgram "if = 1.")

testRejectsTrueAsBindingName :: IO ()
testRejectsTrueAsBindingName =
  assertLeftContains
    "True binding rejection"
    "reserved literal 'True' cannot be used as a binding name"
    (parseSurfaceProgram "True = 1.")

testRejectsFalseAsSignatureName :: IO ()
testRejectsFalseAsSignatureName =
  assertLeftContains
    "False signature rejection"
    "reserved literal 'False' cannot be used as a binding name"
    (parseSurfaceProgram "False :: Bool.")

testLowerIfExpression :: IO ()
testLowerIfExpression =
  assertRight
    "parse + lower if"
    (parseSurfaceProgram "x = if True 1 else 2.")
    (\surfaceProgram -> assertEqual "lowered if AST" expectedProgram (lowerSurfaceExpr surfaceProgram))
  where
    expectedProgram =
      EScope
        [ SLet
            "x"
            (SourceSpan 1 1)
            (EIf (EBool True) (EInt 1) (EInt 2))
        ]

testDesugarIfExpression :: IO ()
testDesugarIfExpression =
  assertRight
    "parse + lower + desugar if"
    (parseSurfaceProgram "x = if True 1 else 2.")
    ( \surfaceProgram ->
        assertEqual
          "desugared if AST"
          expectedProgram
          (desugarExpr (lowerSurfaceExpr surfaceProgram))
    )
  where
    expectedProgram =
      EScope
        [ SLet
            "x"
            (SourceSpan 1 1)
            (ECase (EBool True) (EInt 1) (EInt 2))
        ]
