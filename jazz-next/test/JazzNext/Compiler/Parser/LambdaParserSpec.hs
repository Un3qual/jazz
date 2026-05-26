{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Exception
  ( SomeException,
    evaluate,
    try
  )
import qualified Data.Text as Text
import JazzNext.Compiler.AST
  ( CaseArm (..),
    Expr (..),
    Literal (..),
    Pattern (..),
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
    SurfaceLambdaParameter (..),
    SurfaceLiteral (..),
    SurfaceStatement (..)
  )
import JazzNext.Compiler.Parser.Lower
  ( lowerSurfaceExpr
  )
import JazzNext.TestHarness
  ( NamedTest,
    assertContains,
    assertEqual,
    assertLeftDiagnosticContains,
    assertRight,
    failTest,
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
    ("lowering desugars pattern parameters through case nodes", testLowerDesugarsPatternParametersThroughCase),
    ("lowering preserves duplicate parameter shadowing", testLowerPreservesDuplicateParameterShadowing),
    ("lowering rejects impossible empty lambda surface nodes", testLowerRejectsImpossibleEmptyLambda),
    ("rejects empty lambda parameter list", testRejectsEmptyLambdaParameters),
    ("rejects lambda without parenthesized parameters", testRejectsUnparenthesizedLambda),
    ("rejects lambda parameter trailing comma", testRejectsTrailingCommaParameterList),
    ("parses wildcard lambda parameter patterns", testParsesWildcardLambdaParameterPattern),
    ("parses tuple-shaped lambda parameter patterns", testParsesTupleLambdaParameterPattern),
    ("parses bracketed-list lambda parameter patterns", testParsesListLambdaParameterPattern),
    ("parses cons-like lambda parameter patterns", testParsesConsLikeListLambdaParameterPattern),
    ("parses boolean literal lambda parameter patterns", testParsesBooleanLiteralLambdaParameterPattern),
    ("parses constructor-like lambda parameter patterns", testParsesConstructorLikeLambdaParameterPattern),
    ("rejects reserved keyword as lambda parameter", testRejectsKeywordLambdaParameter)
  ]

testParsesSingleArgumentLambda :: IO ()
testParsesSingleArgumentLambda =
  assertEqual
    "single-argument lambda AST"
    ( Right
        ( SEBlock
            [ SSLet "id" (SourceSpan 1 1) (SELambda [SurfaceLambdaIdentifier "x"] (SEVar "x"))
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
            [ SSLet "const" (SourceSpan 1 1) (SELambda [SurfaceLambdaIdentifier "x", SurfaceLambdaIdentifier "y"] (SEVar "x"))
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
                    [SurfaceLambdaIdentifier "f", SurfaceLambdaIdentifier "x"]
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
                (SEApply (SELambda [SurfaceLambdaIdentifier "x"] (SEVar "x")) (SELit (SLInt 1)))
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

testLowerDesugarsPatternParametersThroughCase :: IO ()
testLowerDesugarsPatternParametersThroughCase =
  assertRight
    "parse + lower tuple-pattern lambda"
    (parseSurfaceProgram "sumPair = \\((left, right)) -> left + right.")
    (\surfaceProgram -> assertEqual "lowered pattern lambda AST" expectedProgram (lowerSurfaceExpr surfaceProgram))
  where
    generatedName = "$lambda_pattern_arg_1"
    expectedProgram =
      EBlock
        [ SLet
            "sumPair"
            (SourceSpan 1 1)
            ( ELambda
                generatedName
                ( EPatternCase
                    (EVar generatedName)
                    [ CaseArm
                        (PTuple [PVariable "left", PVariable "right"])
                        (EBinary "+" (EVar "left") (EVar "right"))
                    ]
                )
            )
        ]

testLowerPreservesDuplicateParameterShadowing :: IO ()
testLowerPreservesDuplicateParameterShadowing =
  assertRight
    "parse + lower duplicate-parameter lambda"
    (parseSurfaceProgram "shadow = \\(x, x) -> x.")
    (\surfaceProgram -> assertEqual "lowered AST" expectedProgram (lowerSurfaceExpr surfaceProgram))
  where
    expectedProgram =
      EBlock
        [ SLet
            "shadow"
            (SourceSpan 1 1)
            (ELambda "x" (ELambda "x" (EVar "x")))
        ]

testLowerRejectsImpossibleEmptyLambda :: IO ()
testLowerRejectsImpossibleEmptyLambda = do
  result <- try (evaluate (lowerSurfaceExpr (SELambda [] (SEVar "x")))) :: IO (Either SomeException Expr)
  case result of
    Left err ->
      assertContains
        "empty lambda lowering failure"
        "empty lambda parameter list"
        (Text.pack (show err))
    Right loweredExpr ->
      failTest
        ( "expected empty lambda lowering to fail, got "
            <> Text.pack (show loweredExpr)
        )

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

testParsesWildcardLambdaParameterPattern :: IO ()
testParsesWildcardLambdaParameterPattern =
  assertRight
    "wildcard lambda parameter pattern"
    (parseSurfaceProgram "f = \\(_) -> 1.")
    (\_ -> pure ())

testParsesTupleLambdaParameterPattern :: IO ()
testParsesTupleLambdaParameterPattern =
  assertRight
    "tuple lambda parameter pattern"
    (parseSurfaceProgram "f = \\((left, right)) -> left.")
    (\_ -> pure ())

testParsesListLambdaParameterPattern :: IO ()
testParsesListLambdaParameterPattern =
  assertRight
    "list lambda parameter pattern"
    (parseSurfaceProgram "f = \\([head, tail]) -> head.")
    (\_ -> pure ())

testParsesConsLikeListLambdaParameterPattern :: IO ()
testParsesConsLikeListLambdaParameterPattern =
  assertRight
    "cons-like list lambda parameter pattern"
    (parseSurfaceProgram "f = \\([head | tail]) -> head.")
    (\_ -> pure ())

testParsesBooleanLiteralLambdaParameterPattern :: IO ()
testParsesBooleanLiteralLambdaParameterPattern =
  assertRight
    "boolean literal lambda parameter pattern"
    (parseSurfaceProgram "f = \\(True) -> 1.")
    (\_ -> pure ())

testParsesConstructorLikeLambdaParameterPattern :: IO ()
testParsesConstructorLikeLambdaParameterPattern =
  assertRight
    "constructor-like lambda parameter pattern"
    (parseSurfaceProgram "f = \\(Just item) -> item.")
    (\_ -> pure ())

testRejectsKeywordLambdaParameter :: IO ()
testRejectsKeywordLambdaParameter =
  assertLeftDiagnosticContains
    "lambda keyword parameter"
    "expected identifier"
    (parseSurfaceProgram "f = \\(if) -> if.")
