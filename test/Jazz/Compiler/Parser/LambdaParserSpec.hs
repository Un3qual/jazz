{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.List.NonEmpty (NonEmpty (..))
import Jazz.Compiler.AST
  ( Literal (..),
  )
import Jazz.Compiler.Diagnostics
  ( SourceSpan (..),
  )
import Jazz.Compiler.Name
  ( GeneratedNameKind (..),
    generatedName,
  )
import Jazz.Compiler.Parser.AST
  ( SurfaceExpr (..),
    SurfaceExprForm (..),
    SurfaceLambdaParameter (..),
    SurfaceLiteral (..),
    SurfacePattern (..),
    SurfacePatternForm (..),
    SurfacePatternLambdaClause (..),
    SurfaceStatement (..),
  )
import Jazz.Compiler.Parser.Lower
  ( lowerSurfaceExpr,
  )
import Jazz.TestCore
  ( assertLoweredCoreEqual,
    loweredBinary,
    loweredBlock,
    loweredCaseArm,
    loweredConstructorPattern,
    loweredLambda,
    loweredLet,
    loweredLiteral,
    loweredOrPattern,
    loweredPatternCase,
    loweredTuple,
    loweredTuplePattern,
    loweredVariable,
    loweredVariablePattern,
    loweredWildcardPattern,
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
    ("rejects reserved keyword as lambda parameter", testRejectsKeywordLambdaParameter),
    ("accepts ordered pattern-lambda clauses", testAcceptsPatternLambdaClauses),
    ("parses ordered pattern-lambda clauses structurally", testParsesPatternLambdaClausesStructurally),
    ("lowers ordered pattern-lambda clauses to one case", testLowersPatternLambdaClausesToOneCase),
    ("rejects pattern-lambda clause arity mismatch", testRejectsPatternLambdaClauseArityMismatch),
    ("keeps pipe operators in pattern-lambda bodies", testKeepsPipeOperatorInPatternLambdaBody),
    ("rejects pattern lambda without a head", testRejectsPatternLambdaWithoutHead),
    ("rejects pattern lambda head without an arrow", testRejectsPatternLambdaHeadWithoutArrow),
    ("rejects pattern lambda without a body", testRejectsPatternLambdaWithoutBody)
  ]

testParsesSingleArgumentLambda :: IO ()
testParsesSingleArgumentLambda =
  assertEqual
    "single-argument lambda AST"
    ( Right
        ( e
            1
            1
            ( SEBlock
                [ SSLet
                    "id"
                    (SourceSpan 1 1)
                    (e 1 6 (SELambda (SurfaceLambdaIdentifier (SourceSpan 1 8) "x" :| []) (e 1 14 (SEVar "x"))))
                ]
            )
        )
    )
    (parseSurfaceProgramPoints "id = \\(x) -> x.")

testParsesMultiArgumentLambda :: IO ()
testParsesMultiArgumentLambda =
  assertEqual
    "multi-argument lambda AST"
    ( Right
        ( e
            1
            1
            ( SEBlock
                [ SSLet
                    "const"
                    (SourceSpan 1 1)
                    ( e
                        1
                        9
                        ( SELambda
                            (SurfaceLambdaIdentifier (SourceSpan 1 11) "x" :| [SurfaceLambdaIdentifier (SourceSpan 1 14) "y"])
                            (e 1 20 (SEVar "x"))
                        )
                    )
                ]
            )
        )
    )
    (parseSurfaceProgramPoints "const = \\(x, y) -> x.")

testParsesLambdaBodyApplication :: IO ()
testParsesLambdaBodyApplication =
  assertEqual
    "lambda application body AST"
    ( Right
        ( e
            1
            1
            ( SEBlock
                [ SSLet
                    "apply"
                    (SourceSpan 1 1)
                    ( e
                        1
                        9
                        ( SELambda
                            (SurfaceLambdaIdentifier (SourceSpan 1 11) "f" :| [SurfaceLambdaIdentifier (SourceSpan 1 14) "x"])
                            (e 1 20 (SEApply (e 1 20 (SEVar "f")) (e 1 22 (SEVar "x"))))
                        )
                    )
                ]
            )
        )
    )
    (parseSurfaceProgramPoints "apply = \\(f, x) -> f x.")

testParsesParenthesizedLambdaApplication :: IO ()
testParsesParenthesizedLambdaApplication =
  assertEqual
    "parenthesized lambda application AST"
    ( Right
        ( e
            1
            1
            ( SEBlock
                [ SSLet
                    "run"
                    (SourceSpan 1 1)
                    ( e
                        1
                        7
                        ( SEApply
                            (e 1 8 (SELambda (SurfaceLambdaIdentifier (SourceSpan 1 10) "x" :| []) (e 1 16 (SEVar "x"))))
                            (e 1 19 (SELit (SLInt 1)))
                        )
                    )
                ]
            )
        )
    )
    (parseSurfaceProgramPoints "run = (\\(x) -> x) 1.")

testLowerNestsMultiArgumentLambda :: IO ()
testLowerNestsMultiArgumentLambda =
  assertRight
    "parse + lower multi-argument lambda"
    (parseSurfaceProgramPoints "const = \\(x, y) -> x.")
    (\surfaceProgram -> assertLoweredCoreEqual "lowered AST" expectedProgram (lowerSurfaceExpr surfaceProgram))
  where
    expectedProgram =
      loweredBlock
        [ loweredLet
            "const"
            (SourceSpan 1 1)
            (loweredLambda "x" (loweredLambda "y" (loweredVariable "x")))
        ]

testLowerDesugarsPatternParametersThroughCase :: IO ()
testLowerDesugarsPatternParametersThroughCase =
  assertRight
    "parse + lower tuple-pattern lambda"
    (parseSurfaceProgramPoints "sumPair = \\((left, right)) -> left + right.")
    (\surfaceProgram -> assertLoweredCoreEqual "lowered pattern lambda AST" expectedProgram (lowerSurfaceExpr surfaceProgram))
  where
    generatedParameter = generatedName (LambdaPatternArgument 1)
    expectedProgram =
      loweredBlock
        [ loweredLet
            "sumPair"
            (SourceSpan 1 1)
            ( loweredLambda
                generatedParameter
                ( loweredPatternCase
                    (loweredVariable generatedParameter)
                    [ loweredCaseArm
                        (loweredTuplePattern [loweredVariablePattern "left", loweredVariablePattern "right"])
                        Nothing
                        (loweredBinary "+" (loweredVariable "left") (loweredVariable "right"))
                    ]
                )
            )
        ]

testLowerPreservesDuplicateParameterShadowing :: IO ()
testLowerPreservesDuplicateParameterShadowing =
  assertRight
    "parse + lower duplicate-parameter lambda"
    (parseSurfaceProgramPoints "shadow = \\(x, x) -> x.")
    (\surfaceProgram -> assertLoweredCoreEqual "lowered AST" expectedProgram (lowerSurfaceExpr surfaceProgram))
  where
    expectedProgram =
      loweredBlock
        [ loweredLet
            "shadow"
            (SourceSpan 1 1)
            (loweredLambda "x" (loweredLambda "x" (loweredVariable "x")))
        ]

testParsesUnitLambdaShorthand :: IO ()
testParsesUnitLambdaShorthand =
  assertEqual
    "Unit lambda shorthand AST"
    ( Right
        ( e
            1
            1
            ( SEBlock
                [ SSLet
                    "thunk"
                    (SourceSpan 1 1)
                    ( e
                        1
                        9
                        ( SELambda
                            (SurfaceLambdaPattern (p 1 10 (SPTuple [])) :| [])
                            (e 1 16 (SELit (SLInt 42)))
                        )
                    )
                ]
            )
        )
    )
    (parseSurfaceProgramPoints "thunk = \\() -> 42.")

testParsesExplicitUnitLambdaParameter :: IO ()
testParsesExplicitUnitLambdaParameter =
  assertEqual
    "explicit Unit lambda AST"
    ( Right
        ( e
            1
            1
            ( SEBlock
                [ SSLet
                    "thunk"
                    (SourceSpan 1 1)
                    ( e
                        1
                        9
                        ( SELambda
                            (SurfaceLambdaPattern (p 1 11 (SPTuple [])) :| [])
                            (e 1 18 (SELit (SLInt 42)))
                        )
                    )
                ]
            )
        )
    )
    (parseSurfaceProgramPoints "thunk = \\(()) -> 42.")

testLowersUnitLambdaShorthand :: IO ()
testLowersUnitLambdaShorthand =
  assertRight
    "parse + lower Unit lambda"
    (parseSurfaceProgramPoints "thunk = \\() -> 42.")
    (\surfaceProgram -> assertLoweredCoreEqual "lowered Unit lambda" expectedProgram (lowerSurfaceExpr surfaceProgram))
  where
    generatedParameter = generatedName (LambdaPatternArgument 1)
    expectedProgram =
      loweredBlock
        [ loweredLet
            "thunk"
            (SourceSpan 1 1)
            ( loweredLambda
                generatedParameter
                ( loweredPatternCase
                    (loweredVariable generatedParameter)
                    [loweredCaseArm (loweredTuplePattern []) Nothing (loweredLiteral (LInt 42))]
                )
            )
        ]

testRejectsTrailingCommaAfterUnitParameter :: IO ()
testRejectsTrailingCommaAfterUnitParameter =
  assertLeftDiagnosticContains
    "Unit lambda trailing comma"
    "expected"
    (parseSurfaceProgramPoints "thunk = \\((),) -> 42.")

testRejectsUnparenthesizedLambda :: IO ()
testRejectsUnparenthesizedLambda =
  assertLeftDiagnosticContains
    "lambda without parameter parens"
    "expected '('"
    (parseSurfaceProgramPoints "f = \\x -> x.")

testRejectsTrailingCommaParameterList :: IO ()
testRejectsTrailingCommaParameterList =
  assertLeftDiagnosticContains
    "lambda trailing comma"
    "expected identifier"
    (parseSurfaceProgramPoints "f = \\(x,) -> x.")

testParsesWildcardLambdaParameterPattern :: IO ()
testParsesWildcardLambdaParameterPattern =
  assertRight
    "wildcard lambda parameter pattern"
    (parseSurfaceProgramPoints "f = \\(_) -> 1.")
    (\_ -> pure ())

testParsesTupleLambdaParameterPattern :: IO ()
testParsesTupleLambdaParameterPattern =
  assertRight
    "tuple lambda parameter pattern"
    (parseSurfaceProgramPoints "f = \\((left, right)) -> left.")
    (\_ -> pure ())

testParsesListLambdaParameterPattern :: IO ()
testParsesListLambdaParameterPattern =
  assertRight
    "list lambda parameter pattern"
    (parseSurfaceProgramPoints "f = \\([head, tail]) -> head.")
    (\_ -> pure ())

testParsesConsLikeListLambdaParameterPattern :: IO ()
testParsesConsLikeListLambdaParameterPattern =
  assertRight
    "cons-like list lambda parameter pattern"
    (parseSurfaceProgramPoints "f = \\([head | tail]) -> head.")
    (\_ -> pure ())

testParsesBooleanLiteralLambdaParameterPattern :: IO ()
testParsesBooleanLiteralLambdaParameterPattern =
  assertRight
    "boolean literal lambda parameter pattern"
    (parseSurfaceProgramPoints "f = \\(True) -> 1.")
    (\_ -> pure ())

testParsesConstructorLikeLambdaParameterPattern :: IO ()
testParsesConstructorLikeLambdaParameterPattern =
  assertRight
    "constructor-like lambda parameter pattern"
    (parseSurfaceProgramPoints "f = \\(Just item) -> item.")
    (\_ -> pure ())

testParsesOrPatternLambdaParameter :: IO ()
testParsesOrPatternLambdaParameter =
  assertEqual
    "or-pattern lambda parameter AST"
    ( Right
        ( e
            1
            1
            ( SEBlock
                [ SSLet
                    "choose"
                    (SourceSpan 1 1)
                    ( e
                        1
                        10
                        ( SELambda
                            ( SurfaceLambdaPattern
                                ( p
                                    1
                                    12
                                    ( SPOr
                                        [ p 1 12 (SPConstructor "Just" [p 1 17 (SPVariable "item")]),
                                          p 1 24 (SPConstructor "Also" [p 1 29 (SPVariable "item")])
                                        ]
                                    )
                                )
                                :| []
                            )
                            (e 1 38 (SEVar "item"))
                        )
                    )
                ]
            )
        )
    )
    (parseSurfaceProgramPoints "choose = \\(Just item | Also item) -> item.")

testParsesCommaAfterOrPatternLambdaParameter :: IO ()
testParsesCommaAfterOrPatternLambdaParameter =
  assertEqual
    "comma after or-pattern lambda parameter AST"
    ( Right
        ( e
            1
            1
            ( SEBlock
                [ SSLet
                    "choose"
                    (SourceSpan 1 1)
                    ( e
                        1
                        10
                        ( SELambda
                            ( SurfaceLambdaPattern
                                ( p
                                    1
                                    12
                                    ( SPOr
                                        [ p 1 12 (SPConstructor "Just" [p 1 17 (SPVariable "item")]),
                                          p 1 24 (SPConstructor "Also" [p 1 29 (SPVariable "item")])
                                        ]
                                    )
                                )
                                :| [SurfaceLambdaIdentifier (SourceSpan 1 35) "extra"]
                            )
                            (e 1 45 (SEVar "item"))
                        )
                    )
                ]
            )
        )
    )
    (parseSurfaceProgramPoints "choose = \\(Just item | Also item, extra) -> item.")

testLowerDesugarsOrPatternParameterThroughCase :: IO ()
testLowerDesugarsOrPatternParameterThroughCase =
  assertRight
    "parse + lower or-pattern lambda"
    (parseSurfaceProgramPoints "choose = \\(Just item | Also item) -> item.")
    (\surfaceProgram -> assertLoweredCoreEqual "lowered or-pattern lambda AST" expectedProgram (lowerSurfaceExpr surfaceProgram))
  where
    generatedParameter = generatedName (LambdaPatternArgument 1)
    expectedProgram =
      loweredBlock
        [ loweredLet
            "choose"
            (SourceSpan 1 1)
            ( loweredLambda
                generatedParameter
                ( loweredPatternCase
                    (loweredVariable generatedParameter)
                    [ loweredCaseArm
                        ( loweredOrPattern
                            [ loweredConstructorPattern "Just" [loweredVariablePattern "item"],
                              loweredConstructorPattern "Also" [loweredVariablePattern "item"]
                            ]
                        )
                        Nothing
                        (loweredVariable "item")
                    ]
                )
            )
        ]

testRejectsGroupedOrPatternLambdaParameter :: IO ()
testRejectsGroupedOrPatternLambdaParameter =
  assertLeftDiagnosticContains
    "grouped lambda or-pattern"
    "expected ',' or ')', found '|'"
    (parseSurfaceProgramPoints "f = \\((Just item | Also item)) -> item.")

testRejectsLambdaOrPatternParameterGuard :: IO ()
testRejectsLambdaOrPatternParameterGuard =
  assertLeftDiagnosticContains
    "lambda or-pattern guard"
    "expected ',' or ')'"
    (parseSurfaceProgramPoints "f = \\(Just item | Also item if item > 0) -> item.")

testRejectsKeywordLambdaParameter :: IO ()
testRejectsKeywordLambdaParameter =
  assertLeftDiagnosticContains
    "lambda keyword parameter"
    "expected identifier"
    (parseSurfaceProgramPoints "f = \\(if) -> if.")

testAcceptsPatternLambdaClauses :: IO ()
testAcceptsPatternLambdaClauses =
  assertRight
    "multi-body pattern lambda"
    ( parseSurfaceProgramPoints
        "choose = \\|(Nothing, fallback) -> fallback |(Just item, _) -> item."
    )
    (\_ -> pure ())

testParsesPatternLambdaClausesStructurally :: IO ()
testParsesPatternLambdaClausesStructurally =
  assertEqual
    "pattern-lambda surface AST"
    ( Right
        ( e
            1
            1
            ( SEBlock
                [ SSLet
                    "choose"
                    (SourceSpan 1 1)
                    ( e
                        1
                        10
                        ( SEPatternLambda
                            ( SurfacePatternLambdaClause
                                (SourceSpan 1 11)
                                (p 1 13 (SPConstructor "Nothing" []) :| [p 1 22 (SPVariable "fallback")])
                                (e 1 35 (SEVar "fallback"))
                                :| [ SurfacePatternLambdaClause
                                       (SourceSpan 1 44)
                                       (p 1 46 (SPConstructor "Just" [p 1 51 (SPVariable "item")]) :| [p 1 57 SPWildcard])
                                       (e 1 63 (SEVar "item"))
                                   ]
                            )
                        )
                    )
                ]
            )
        )
    )
    (parseSurfaceProgramPoints "choose = \\|(Nothing, fallback) -> fallback |(Just item, _) -> item.")

testLowersPatternLambdaClausesToOneCase :: IO ()
testLowersPatternLambdaClausesToOneCase =
  assertRight
    "parse + lower pattern-lambda clauses"
    (parseSurfaceProgramPoints "choose = \\|(Nothing, fallback) -> fallback |(Just item, _) -> item.")
    (\surfaceProgram -> assertLoweredCoreEqual "lowered pattern-lambda AST" expectedProgram (lowerSurfaceExpr surfaceProgram))
  where
    firstArgument = generatedName (LambdaPatternArgument 1)
    secondArgument = generatedName (LambdaPatternArgument 2)
    expectedProgram =
      loweredBlock
        [ loweredLet
            "choose"
            (SourceSpan 1 1)
            ( loweredLambda
                firstArgument
                ( loweredLambda
                    secondArgument
                    ( loweredPatternCase
                        (loweredTuple [loweredVariable firstArgument, loweredVariable secondArgument])
                        [ loweredCaseArm
                            (loweredTuplePattern [loweredConstructorPattern "Nothing" [], loweredVariablePattern "fallback"])
                            Nothing
                            (loweredVariable "fallback"),
                          loweredCaseArm
                            (loweredTuplePattern [loweredConstructorPattern "Just" [loweredVariablePattern "item"], loweredWildcardPattern])
                            Nothing
                            (loweredVariable "item")
                        ]
                    )
                )
            )
        ]

testRejectsPatternLambdaClauseArityMismatch :: IO ()
testRejectsPatternLambdaClauseArityMismatch =
  assertLeftDiagnosticContains
    "pattern lambda clause arity"
    "pattern-lambda clauses must all have 1 parameter(s), found 2"
    (parseSurfaceProgramPoints "choose = \\|([]) -> 0 |([item | rest], fallback) -> item.")

testKeepsPipeOperatorInPatternLambdaBody :: IO ()
testKeepsPipeOperatorInPatternLambdaBody =
  assertRight
    "pipe operator before next lambda clause"
    ( parseSurfaceProgramPoints
        "operator (|) tier 4 precedence 20 left. choose = \\|(0) -> 1 | 2 |(_) -> 3."
    )
    (\_ -> pure ())

testRejectsPatternLambdaWithoutHead :: IO ()
testRejectsPatternLambdaWithoutHead =
  assertLeftDiagnosticContains
    "pattern lambda without head"
    "expected '('"
    (parseSurfaceProgramPoints "choose = \\|.")

testRejectsPatternLambdaHeadWithoutArrow :: IO ()
testRejectsPatternLambdaHeadWithoutArrow =
  assertLeftDiagnosticContains
    "pattern lambda head without arrow"
    "expected '->'"
    (parseSurfaceProgramPoints "choose = \\|(item) item.")

testRejectsPatternLambdaWithoutBody :: IO ()
testRejectsPatternLambdaWithoutBody =
  assertLeftDiagnosticContains
    "pattern lambda without body"
    "expected expression"
    (parseSurfaceProgramPoints "choose = \\|(item) ->.")

e :: Int -> Int -> SurfaceExprForm -> SurfaceExpr
e line column = SurfaceExpr (SourceSpan line column)

p :: Int -> Int -> SurfacePatternForm -> SurfacePattern
p line column = SurfacePattern (SourceSpan line column)
