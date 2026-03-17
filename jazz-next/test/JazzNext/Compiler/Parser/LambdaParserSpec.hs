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
    assertLeftDiagnosticContains,
    assertRight,
    runTestSuite
  )

main :: IO ()
main = runTestSuite "LambdaParser" tests

tests :: [NamedTest]
tests =
  [ ("parses single-argument lambda binding", testParsesSingleArgumentLambda),
    ("parses multi-argument lambda binding", testParsesMultiArgumentLambda),
    ("parses lambda body application", testParsesLambdaBodyApplication),
    ("parses parenthesized lambda in application position", testParsesParenthesizedLambdaApplication),
    ("lowering nests multi-argument lambdas into unary core nodes", testLowerNestsMultiArgumentLambda),
    ("rejects empty lambda parameter list", testRejectsEmptyLambdaParameters),
    ("rejects lambda without parenthesized parameters", testRejectsUnparenthesizedLambda),
    ("rejects lambda parameter trailing comma", testRejectsTrailingCommaParameterList),
    ("rejects reserved keyword as lambda parameter", testRejectsKeywordLambdaParameter)
  ]

testParsesSingleArgumentLambda :: IO ()
testParsesSingleArgumentLambda =
  assertEqual
    "single-argument lambda AST"
    ( Right
        ( SEBlock
            [ SSLet "id" (SourceSpan 1 1) (SELambda ["x"] (SEVar "x"))
            ]
        )
    )
    (parseSurfaceProgram "id = \\(x) -> x.")

testParsesMultiArgumentLambda :: IO ()
testParsesMultiArgumentLambda =
  assertEqual
    "multi-argument lambda AST"
    ( Right
        ( SEBlock
            [ SSLet "const" (SourceSpan 1 1) (SELambda ["x", "y"] (SEVar "x"))
            ]
        )
    )
    (parseSurfaceProgram "const = \\(x, y) -> x.")

testParsesLambdaBodyApplication :: IO ()
testParsesLambdaBodyApplication =
  assertEqual
    "lambda application body AST"
    ( Right
        ( SEBlock
            [ SSLet
                "apply"
                (SourceSpan 1 1)
                ( SELambda
                    ["f", "x"]
                    (SEApply (SEVar "f") (SEVar "x"))
                )
            ]
        )
    )
    (parseSurfaceProgram "apply = \\(f, x) -> f x.")

testParsesParenthesizedLambdaApplication :: IO ()
testParsesParenthesizedLambdaApplication =
  assertEqual
    "parenthesized lambda application AST"
    ( Right
        ( SEBlock
            [ SSLet
                "run"
                (SourceSpan 1 1)
                (SEApply (SELambda ["x"] (SEVar "x")) (SELit (SLInt 1)))
            ]
        )
    )
    (parseSurfaceProgram "run = (\\(x) -> x) 1.")

testLowerNestsMultiArgumentLambda :: IO ()
testLowerNestsMultiArgumentLambda =
  assertRight
    "parse + lower multi-argument lambda"
    (parseSurfaceProgram "const = \\(x, y) -> x.")
    (\surfaceProgram -> assertEqual "lowered AST" expectedProgram (lowerSurfaceExpr surfaceProgram))
  where
    expectedProgram =
      EBlock
        [ SLet
            "const"
            (SourceSpan 1 1)
            (ELambda "x" (ELambda "y" (EVar "x")))
        ]

testRejectsEmptyLambdaParameters :: IO ()
testRejectsEmptyLambdaParameters =
  assertLeftDiagnosticContains
    "empty lambda parameters"
    "expected lambda parameter"
    (parseSurfaceProgram "f = \\() -> x.")

testRejectsUnparenthesizedLambda :: IO ()
testRejectsUnparenthesizedLambda =
  assertLeftDiagnosticContains
    "lambda without parameter parens"
    "expected '('"
    (parseSurfaceProgram "f = \\x -> x.")

testRejectsTrailingCommaParameterList :: IO ()
testRejectsTrailingCommaParameterList =
  assertLeftDiagnosticContains
    "lambda trailing comma"
    "expected identifier"
    (parseSurfaceProgram "f = \\(x,) -> x.")

testRejectsKeywordLambdaParameter :: IO ()
testRejectsKeywordLambdaParameter =
  assertLeftDiagnosticContains
    "lambda keyword parameter"
    "expected identifier"
    (parseSurfaceProgram "f = \\(if) -> if.")
