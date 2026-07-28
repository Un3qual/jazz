{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.List.NonEmpty (NonEmpty (..))
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
import JazzNext.Compiler.Name
  ( GeneratedNameKind (..),
    generatedName
  )
import JazzNext.Compiler.Parser
  ( parseSurfaceProgram
  )
import JazzNext.Compiler.Parser.AST
  ( SurfaceExpr (..),
    SurfaceLambdaParameter (..),
    SurfaceLiteral (..),
    SurfacePattern (..),
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
    ("lowering desugars pattern parameters through case nodes", testLowerDesugarsPatternParametersThroughCase),
    ("lowering preserves duplicate parameter shadowing", testLowerPreservesDuplicateParameterShadowing),
    ("parses Unit lambda shorthand as one pattern parameter", testParsesUnitLambdaShorthand),
    ("parses explicit nested Unit lambda parameter", testParsesExplicitUnitLambdaParameter),
    ("lowers Unit lambda shorthand to one core lambda", testLowersUnitLambdaShorthand),
    ("rejects lambda without parenthesized parameters", testRejectsUnparenthesizedLambda),
    ("rejects lambda parameter trailing comma", testRejectsTrailingCommaParameterList),
    ("rejects trailing comma after Unit lambda parameter", testRejectsTrailingCommaAfterUnitParameter),
    ("parses wildcard lambda parameter patterns", testParsesWildcardLambdaParameterPattern),
    ("parses tuple-shaped lambda parameter patterns", testParsesTupleLambdaParameterPattern),
    ("parses bracketed-list lambda parameter patterns", testParsesListLambdaParameterPattern),
    ("parses cons-like lambda parameter patterns", testParsesConsLikeListLambdaParameterPattern),
    ("parses boolean literal lambda parameter patterns", testParsesBooleanLiteralLambdaParameterPattern),
    ("parses constructor-like lambda parameter patterns", testParsesConstructorLikeLambdaParameterPattern),
    ("parses or-pattern lambda parameter alternatives", testParsesOrPatternLambdaParameter),
    ("parses comma after or-pattern lambda parameter alternatives", testParsesCommaAfterOrPatternLambdaParameter),
    ("lowering desugars or-pattern parameters through case nodes", testLowerDesugarsOrPatternParameterThroughCase),
    ("rejects grouped or-pattern lambda parameters", testRejectsGroupedOrPatternLambdaParameter),
    ("rejects lambda parameter or-pattern guards", testRejectsLambdaOrPatternParameterGuard),
    ("rejects reserved keyword as lambda parameter", testRejectsKeywordLambdaParameter)
  ]

testParsesSingleArgumentLambda :: IO ()
testParsesSingleArgumentLambda =
  assertEqual
    "single-argument lambda AST"
    ( Right
        ( SEBlock
            [ SSLet "id" (SourceSpan 1 1) (SELambda (SurfaceLambdaIdentifier "x" :| []) (SEVar "x"))
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
            [ SSLet "const" (SourceSpan 1 1) (SELambda (SurfaceLambdaIdentifier "x" :| [SurfaceLambdaIdentifier "y"]) (SEVar "x"))
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
                    (SurfaceLambdaIdentifier "f" :| [SurfaceLambdaIdentifier "x"])
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
                (SEApply (SELambda (SurfaceLambdaIdentifier "x" :| []) (SEVar "x")) (SELit (SLInt 1)))
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
    generatedParameter = generatedName (LambdaPatternArgument 1)
    expectedProgram =
      EBlock
        [ SLet
            "sumPair"
            (SourceSpan 1 1)
            ( ELambda
                generatedParameter
                ( EPatternCase
                    (EVar generatedParameter)
                    [ CaseArm
                        (PTuple [PVariable "left", PVariable "right"])
                        Nothing
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

testParsesUnitLambdaShorthand :: IO ()
testParsesUnitLambdaShorthand =
  assertEqual
    "Unit lambda shorthand AST"
    ( Right
        ( SEBlock
            [ SSLet
                "thunk"
                (SourceSpan 1 1)
                ( SELambda
                    (SurfaceLambdaPattern (SPTuple []) :| [])
                    (SELit (SLInt 42))
                )
            ]
        )
    )
    (parseSurfaceProgram "thunk = \\() -> 42.")

testParsesExplicitUnitLambdaParameter :: IO ()
testParsesExplicitUnitLambdaParameter =
  assertEqual
    "explicit Unit lambda AST"
    ( Right
        ( SEBlock
            [ SSLet
                "thunk"
                (SourceSpan 1 1)
                ( SELambda
                    (SurfaceLambdaPattern (SPTuple []) :| [])
                    (SELit (SLInt 42))
                )
            ]
        )
    )
    (parseSurfaceProgram "thunk = \\(()) -> 42.")

testLowersUnitLambdaShorthand :: IO ()
testLowersUnitLambdaShorthand =
  assertRight
    "parse + lower Unit lambda"
    (parseSurfaceProgram "thunk = \\() -> 42.")
    (\surfaceProgram -> assertEqual "lowered Unit lambda" expectedProgram (lowerSurfaceExpr surfaceProgram))
  where
    generatedParameter = generatedName (LambdaPatternArgument 1)
    expectedProgram =
      EBlock
        [ SLet
            "thunk"
            (SourceSpan 1 1)
            ( ELambda
                generatedParameter
                ( EPatternCase
                    (EVar generatedParameter)
                    [CaseArm (PTuple []) Nothing (ELit (LInt 42))]
                )
            )
        ]

testRejectsTrailingCommaAfterUnitParameter :: IO ()
testRejectsTrailingCommaAfterUnitParameter =
  assertLeftDiagnosticContains
    "Unit lambda trailing comma"
    "expected"
    (parseSurfaceProgram "thunk = \\((),) -> 42.")

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

testParsesOrPatternLambdaParameter :: IO ()
testParsesOrPatternLambdaParameter =
  assertEqual
    "or-pattern lambda parameter AST"
    ( Right
        ( SEBlock
            [ SSLet
                "choose"
                (SourceSpan 1 1)
                ( SELambda
                    ( SurfaceLambdaPattern
                        ( SPOr
                            [ SPConstructor "Just" [SPVariable "item"],
                              SPConstructor "Also" [SPVariable "item"]
                            ]
                        )
                        :| []
                    )
                    (SEVar "item")
                )
            ]
        )
    )
    (parseSurfaceProgram "choose = \\(Just item | Also item) -> item.")

testParsesCommaAfterOrPatternLambdaParameter :: IO ()
testParsesCommaAfterOrPatternLambdaParameter =
  assertEqual
    "comma after or-pattern lambda parameter AST"
    ( Right
        ( SEBlock
            [ SSLet
                "choose"
                (SourceSpan 1 1)
                ( SELambda
                    ( SurfaceLambdaPattern
                        ( SPOr
                            [ SPConstructor "Just" [SPVariable "item"],
                              SPConstructor "Also" [SPVariable "item"]
                            ]
                        )
                        :| [SurfaceLambdaIdentifier "extra"]
                    )
                    (SEVar "item")
                )
            ]
        )
    )
    (parseSurfaceProgram "choose = \\(Just item | Also item, extra) -> item.")

testLowerDesugarsOrPatternParameterThroughCase :: IO ()
testLowerDesugarsOrPatternParameterThroughCase =
  assertRight
    "parse + lower or-pattern lambda"
    (parseSurfaceProgram "choose = \\(Just item | Also item) -> item.")
    (\surfaceProgram -> assertEqual "lowered or-pattern lambda AST" expectedProgram (lowerSurfaceExpr surfaceProgram))
  where
    generatedParameter = generatedName (LambdaPatternArgument 1)
    expectedProgram =
      EBlock
        [ SLet
            "choose"
            (SourceSpan 1 1)
            ( ELambda
                generatedParameter
                ( EPatternCase
                    (EVar generatedParameter)
                    [ CaseArm
                        ( POr
                            [ PConstructor "Just" [PVariable "item"],
                              PConstructor "Also" [PVariable "item"]
                            ]
                        )
                        Nothing
                        (EVar "item")
                    ]
                )
            )
        ]

testRejectsGroupedOrPatternLambdaParameter :: IO ()
testRejectsGroupedOrPatternLambdaParameter =
  assertLeftDiagnosticContains
    "grouped lambda or-pattern"
    "expected ')', found '|'"
    (parseSurfaceProgram "f = \\((Just item | Also item)) -> item.")

testRejectsLambdaOrPatternParameterGuard :: IO ()
testRejectsLambdaOrPatternParameterGuard =
  assertLeftDiagnosticContains
    "lambda or-pattern guard"
    "expected ',' or ')'"
    (parseSurfaceProgram "f = \\(Just item | Also item if item > 0) -> item.")

testRejectsKeywordLambdaParameter :: IO ()
testRejectsKeywordLambdaParameter =
  assertLeftDiagnosticContains
    "lambda keyword parameter"
    "expected identifier"
    (parseSurfaceProgram "f = \\(if) -> if.")
