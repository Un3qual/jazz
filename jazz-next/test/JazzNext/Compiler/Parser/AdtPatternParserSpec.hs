{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.List.NonEmpty (NonEmpty (..))
import JazzNext.Compiler.AST
  ( CaseArm (..),
    DataConstructorArgument (..),
    DataConstructor (..),
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
  ( SurfaceCaseArm (..),
    SurfaceDataConstructorArgument (..),
    SurfaceDataConstructor (..),
    SurfaceExpr (..),
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
main = runTestSuite "AdtPatternParser" tests

tests :: [NamedTest]
tests =
  [ ("parses basic case expression with literal and wildcard arms", testParsesBasicCaseExpression),
    ("parses variable pattern case arm", testParsesVariablePatternCaseArm),
    ("parses as-pattern case arms", testParsesAsPatternCaseArm),
    ("parses guarded case arms", testParsesGuardedCaseArm),
    ("parses case-arm or-patterns and lowers them", testParsesCaseArmOrPatterns),
    ("keeps all-literal pipe body before literal arm boundary", testKeepsAllLiteralPipeBodyBeforeLiteralArmBoundary),
    ("parses wildcard-led later or-pattern arm after body", testParsesWildcardLedLaterOrPatternArmAfterBody),
    ("parses variable-led later or-pattern arm after body", testParsesVariableLedLaterOrPatternArmAfterBody),
    ("parses variable-led mixed later or-pattern arm after body", testParsesVariableLedMixedLaterOrPatternArmAfterBody),
    ("keeps pipe operator in or-pattern arm body", testKeepsPipeOperatorInOrPatternArmBody),
    ("parses guarded case arm with pipe expression guard after previous arm", testParsesGuardedCaseArmWithPipeExpressionAfterPreviousArm),
    ("parses guarded case arms with definite pipe RHS guards", testParsesGuardedCaseArmWithDefinitePipeRhsGuards),
    ("keeps higher-precedence pipe in comparison guard RHS", testKeepsHigherPrecedencePipeInComparisonGuardRhs),
    ("keeps literal pipe operand in equality guard RHS", testKeepsLiteralPipeOperandInEqualityGuardRhs),
    ("keeps literal pipe operand in inequality guard RHS", testKeepsLiteralPipeOperandInInequalityGuardRhs),
    ("keeps literal pipe operand in ordering guard RHS", testKeepsLiteralPipeOperandInOrderingGuardRhs),
    ("keeps constructor if-expression pipe RHS before arm arrow", testKeepsConstructorIfExpressionPipeRhsBeforeArmArrow),
    ("rejects missing guard arrow before guarded constructor arm", testRejectsMissingGuardArrowBeforeGuardedConstructorArm),
    ("keeps as-pattern constructor arguments atomic", testKeepsAsPatternConstructorArgumentsAtomic),
    ("parses as-pattern lambda parameters", testParsesAsPatternLambdaParameter),
    ("parses constructor pattern case arms", testParsesConstructorPatternCaseArms),
    ("parses multi-argument constructor patterns with nullary subpatterns", testParsesMultiArgumentConstructorPatternsWithNullarySubpatterns),
    ("parses nullary constructor subpatterns without losing the outer argument", testParsesNullaryConstructorSubpatterns),
    ("parses list pattern case arms", testParsesListPatternCaseArms),
    ("parses canonical data declaration and lowers constructor arities", testParsesCanonicalDataDeclarationAndLowersConstructorArities),
    ("parses generic data declaration parameters", testParsesGenericDataDeclarationParameters),
    ("parses nested case expression", testParsesNestedCaseExpression),
    ("parses unparenthesized if expression inside case arm body", testParsesIfExpressionInsideCaseArmBody),
    ("parses unparenthesized lambda expression inside case arm body", testParsesLambdaExpressionInsideCaseArmBody),
    ("parses mixed literal-wildcard later or-pattern arm after body", testParsesMixedLiteralWildcardLaterOrPatternArmAfterBody),
    ("keeps pipe operator inside body before constructor arm boundary", testKeepsPipeOperatorInsideBodyBeforeConstructorArmBoundary),
    ("keeps pipe operator inside body before literal arm boundary", testKeepsPipeOperatorInsideBodyBeforeLiteralArmBoundary),
    ("keeps bare list literal after pipe operator inside body", testKeepsBareListLiteralAfterPipeOperator),
    ("keeps bare constructor value after pipe operator inside body", testKeepsBareConstructorValueAfterPipeOperator),
    ("keeps list application after pipe operator inside body", testKeepsListApplicationAfterPipeOperator),
    ("keeps constructor application after pipe operator inside body", testKeepsConstructorApplicationAfterPipeOperator),
    ("keeps lambda application after pipe operator inside body", testKeepsLambdaApplicationAfterPipeOperator),
    ("keeps underscore application after pipe operator inside body", testKeepsUnderscoreApplicationAfterPipeOperator),
    ("keeps underscore boolean application after pipe operator inside body", testKeepsUnderscoreBooleanApplicationAfterPipeOperator),
    ("parses case scrutinee with block argument", testParsesCaseScrutineeWithBlockArgument),
    ("reports missing case body for block-valued scrutinee", testReportsMissingCaseBodyForBlockScrutinee),
    ("reports block parse error for unterminated fractional block scrutinee", testReportsBlockErrorForUnterminatedFractionalBlockScrutinee),
    ("reports missing arm arrow for block-valued scrutinee", testReportsMissingArmArrowForBlockScrutinee),
    ("reports invalid case scrutinee syntax before body diagnostics", testReportsInvalidCaseScrutineeSyntax),
    ("rejects case expression without leading pipe", testRejectsCaseExpressionWithoutPipe),
    ("rejects case expression without arm arrow", testRejectsCaseExpressionWithoutArrow),
    ("rejects data declaration without constructors", testRejectsDataDeclarationWithoutConstructors),
    ("rejects duplicate constructor names in one data declaration", testRejectsDuplicateConstructorsInDataDeclaration),
    ("rejects duplicate data type parameters", testRejectsDuplicateDataTypeParameters),
    ("rejects undeclared generic constructor payload names", testRejectsUndeclaredGenericConstructorPayloadNames),
    ("rejects data declaration with malformed pipe placement", testRejectsDataDeclarationWithMalformedPipePlacement),
    ("rejects data declaration missing terminator", testRejectsDataDeclarationMissingTerminator),
    ("parses tuple pattern case arms", testParsesTuplePatternCaseArms),
    ("parses cons-like list patterns", testParsesConsLikeListPattern),
    ("parses cons-like list patterns inside constructor patterns", testParsesConsLikeListPatternInsideConstructorPattern),
    ("rejects malformed parenthesized list-like patterns without tuple diagnostic", testRejectsMalformedParenthesizedListLikePattern),
    ("rejects malformed list patterns", testRejectsMalformedListPattern),
    ("rejects malformed later list patterns", testRejectsMalformedLaterListPattern),
    ("rejects malformed guard expression", testRejectsMalformedGuardExpression),
    ("rejects malformed or-pattern alternatives", testRejectsMalformedOrPatternAlternative),
    ("lowers parsed case nodes into core AST", testLowerCaseExpression)
  ]

testParsesBasicCaseExpression :: IO ()
testParsesBasicCaseExpression =
  assertEqual
    "surface case AST"
    ( Right
        ( SEBlock
            [ SSLet
                "x"
                (SourceSpan 1 1)
                ( SECase
                    (SEVar "n")
                    [ SurfaceCaseArm (SPLiteral (SLInt 0)) Nothing (SELit (SLBool True)),
                      SurfaceCaseArm SPWildcard Nothing (SELit (SLBool False))
                    ]
                )
            ]
        )
    )
    (parseSurfaceProgram "x = case n { | 0 -> True | _ -> False }.")

testParsesVariablePatternCaseArm :: IO ()
testParsesVariablePatternCaseArm =
  assertEqual
    "variable pattern case arm"
    ( Right
        ( SEBlock
            [ SSLet
                "x"
                (SourceSpan 1 1)
                (SECase (SEVar "value") [SurfaceCaseArm (SPVariable "item") Nothing (SEVar "item")])
            ]
        )
    )
    (parseSurfaceProgram "x = case value { | item -> item }.")

testParsesAsPatternCaseArm :: IO ()
testParsesAsPatternCaseArm =
  assertRight
    "as-pattern case arm parse + lower"
    (parseSurfaceProgram "x = case value { | whole @ Just item -> whole | _ -> value }.")
    ( \surfaceProgram -> do
        assertEqual "as-pattern surface AST" expectedSurfaceProgram surfaceProgram
        assertEqual "as-pattern lowered AST" expectedLoweredProgram (lowerSurfaceExpr surfaceProgram)
    )
  where
    expectedSurfaceProgram =
      SEBlock
        [ SSLet
            "x"
            (SourceSpan 1 1)
            ( SECase
                (SEVar "value")
                [ SurfaceCaseArm
                    (SPAs "whole" (SPConstructor "Just" [SPVariable "item"]))
                    Nothing
                    (SEVar "whole"),
                  SurfaceCaseArm SPWildcard Nothing (SEVar "value")
                ]
            )
        ]
    expectedLoweredProgram =
      EBlock
        [ SLet
            "x"
            (SourceSpan 1 1)
            ( EPatternCase
                (EVar "value")
                [ CaseArm
                    (PAs "whole" (PConstructor "Just" [PVariable "item"]))
                    Nothing
                    (EVar "whole"),
                  CaseArm PWildcard Nothing (EVar "value")
                ]
            )
        ]

testParsesGuardedCaseArm :: IO ()
testParsesGuardedCaseArm =
  assertRight
    "guarded case arm parse + lower"
    (parseSurfaceProgram "x = case value { | Just item if item > 0 -> item | _ -> 0 }.")
    ( \surfaceProgram -> do
        assertEqual "guarded case arm surface AST" expectedSurfaceProgram surfaceProgram
        assertEqual "guarded case arm lowered AST" expectedLoweredProgram (lowerSurfaceExpr surfaceProgram)
    )
  where
    expectedSurfaceProgram =
      SEBlock
        [ SSLet
            "x"
            (SourceSpan 1 1)
            ( SECase
                (SEVar "value")
                [ SurfaceCaseArm
                    (SPConstructor "Just" [SPVariable "item"])
                    (Just (SEBinary ">" (SEVar "item") (SELit (SLInt 0))))
                    (SEVar "item"),
                  SurfaceCaseArm
                    SPWildcard
                    Nothing
                    (SELit (SLInt 0))
                ]
            )
        ]
    expectedLoweredProgram =
      EBlock
        [ SLet
            "x"
            (SourceSpan 1 1)
            ( EPatternCase
                (EVar "value")
                [ CaseArm
                    (PConstructor "Just" [PVariable "item"])
                    (Just (EBinary ">" (EVar "item") (ELit (LInt 0))))
                    (EVar "item"),
                  CaseArm
                    PWildcard
                    Nothing
                    (ELit (LInt 0))
                ]
            )
        ]

testParsesCaseArmOrPatterns :: IO ()
testParsesCaseArmOrPatterns =
  assertRight
    "or-pattern case arm parse + lower"
    (parseSurfaceProgram "x = case value { | Just item | Also item if item > 0 -> item | Nothing -> 0 }.")
    ( \surfaceProgram -> do
        assertEqual "or-pattern surface AST" expectedSurfaceProgram surfaceProgram
        assertEqual "or-pattern lowered AST" expectedLoweredProgram (lowerSurfaceExpr surfaceProgram)
    )
  where
    expectedSurfaceProgram =
      SEBlock
        [ SSLet
            "x"
            (SourceSpan 1 1)
            ( SECase
                (SEVar "value")
                [ SurfaceCaseArm
                    ( SPOr
                        [ SPConstructor "Just" [SPVariable "item"],
                          SPConstructor "Also" [SPVariable "item"]
                        ]
                    )
                    (Just (SEBinary ">" (SEVar "item") (SELit (SLInt 0))))
                    (SEVar "item"),
                  SurfaceCaseArm
                    (SPConstructor "Nothing" [])
                    Nothing
                    (SELit (SLInt 0))
                ]
            )
        ]
    expectedLoweredProgram =
      EBlock
        [ SLet
            "x"
            (SourceSpan 1 1)
            ( EPatternCase
                (EVar "value")
                [ CaseArm
                    ( POr
                        [ PConstructor "Just" [PVariable "item"],
                          PConstructor "Also" [PVariable "item"]
                        ]
                    )
                    (Just (EBinary ">" (EVar "item") (ELit (LInt 0))))
                    (EVar "item"),
                  CaseArm
                    (PConstructor "Nothing" [])
                    Nothing
                    (ELit (LInt 0))
                ]
            )
        ]

testKeepsAllLiteralPipeBodyBeforeLiteralArmBoundary :: IO ()
testKeepsAllLiteralPipeBodyBeforeLiteralArmBoundary =
  assertRight
    "all-literal pipe body before literal arm boundary parse + lower"
    (parseSurfaceProgram "x = case n { | _ -> 0 | 1 | 2 -> 1 }.")
    ( \surfaceProgram -> do
        assertEqual "all-literal pipe body surface AST" expectedSurfaceProgram surfaceProgram
        assertEqual "all-literal pipe body lowered AST" expectedLoweredProgram (lowerSurfaceExpr surfaceProgram)
    )
  where
    expectedSurfaceProgram =
      SEBlock
        [ SSLet
            "x"
            (SourceSpan 1 1)
            ( SECase
                (SEVar "n")
                [ SurfaceCaseArm
                    SPWildcard
                    Nothing
                    (SEBinary "|" (SELit (SLInt 0)) (SELit (SLInt 1))),
                  SurfaceCaseArm
                    (SPLiteral (SLInt 2))
                    Nothing
                    (SELit (SLInt 1))
                ]
            )
        ]
    expectedLoweredProgram =
      EBlock
        [ SLet
            "x"
            (SourceSpan 1 1)
            ( EPatternCase
                (EVar "n")
                [ CaseArm
                    PWildcard
                    Nothing
                    (EBinary "|" (ELit (LInt 0)) (ELit (LInt 1))),
                  CaseArm
                    (PLiteral (LInt 2))
                    Nothing
                    (ELit (LInt 1))
                ]
            )
        ]

testParsesWildcardLedLaterOrPatternArmAfterBody :: IO ()
testParsesWildcardLedLaterOrPatternArmAfterBody =
  assertRight
    "wildcard-led later or-pattern case arm parse + lower"
    (parseSurfaceProgram "x = case n { | 0 -> 0 | _ | 2 -> 1 }.")
    ( \surfaceProgram -> do
        assertEqual "wildcard-led later or-pattern surface AST" expectedSurfaceProgram surfaceProgram
        assertEqual "wildcard-led later or-pattern lowered AST" expectedLoweredProgram (lowerSurfaceExpr surfaceProgram)
    )
  where
    expectedSurfaceProgram =
      SEBlock
        [ SSLet
            "x"
            (SourceSpan 1 1)
            ( SECase
                (SEVar "n")
                [ SurfaceCaseArm
                    (SPLiteral (SLInt 0))
                    Nothing
                    (SELit (SLInt 0)),
                  SurfaceCaseArm
                    (SPOr [SPWildcard, SPLiteral (SLInt 2)])
                    Nothing
                    (SELit (SLInt 1))
                ]
            )
        ]
    expectedLoweredProgram =
      EBlock
        [ SLet
            "x"
            (SourceSpan 1 1)
            ( EPatternCase
                (EVar "n")
                [ CaseArm
                    (PLiteral (LInt 0))
                    Nothing
                    (ELit (LInt 0)),
                  CaseArm
                    (POr [PWildcard, PLiteral (LInt 2)])
                    Nothing
                    (ELit (LInt 1))
                ]
            )
        ]

testParsesVariableLedLaterOrPatternArmAfterBody :: IO ()
testParsesVariableLedLaterOrPatternArmAfterBody =
  assertRight
    "variable-led later or-pattern case arm parse + lower"
    (parseSurfaceProgram "x = case n { | 0 -> 0 | item | other -> item }.")
    ( \surfaceProgram -> do
        assertEqual "variable-led later or-pattern surface AST" expectedSurfaceProgram surfaceProgram
        assertEqual "variable-led later or-pattern lowered AST" expectedLoweredProgram (lowerSurfaceExpr surfaceProgram)
    )
  where
    expectedSurfaceProgram =
      SEBlock
        [ SSLet
            "x"
            (SourceSpan 1 1)
            ( SECase
                (SEVar "n")
                [ SurfaceCaseArm
                    (SPLiteral (SLInt 0))
                    Nothing
                    (SELit (SLInt 0)),
                  SurfaceCaseArm
                    (SPOr [SPVariable "item", SPVariable "other"])
                    Nothing
                    (SEVar "item")
                ]
            )
        ]
    expectedLoweredProgram =
      EBlock
        [ SLet
            "x"
            (SourceSpan 1 1)
            ( EPatternCase
                (EVar "n")
                [ CaseArm
                    (PLiteral (LInt 0))
                    Nothing
                    (ELit (LInt 0)),
                  CaseArm
                    (POr [PVariable "item", PVariable "other"])
                    Nothing
                    (EVar "item")
                ]
            )
        ]

testParsesVariableLedMixedLaterOrPatternArmAfterBody :: IO ()
testParsesVariableLedMixedLaterOrPatternArmAfterBody =
  assertRight
    "variable-led mixed later or-pattern case arm parse + lower"
    (parseSurfaceProgram "x = case n { | 0 -> 0 | item | item @ _ -> item }.")
    ( \surfaceProgram -> do
        assertEqual "variable-led mixed later or-pattern surface AST" expectedSurfaceProgram surfaceProgram
        assertEqual "variable-led mixed later or-pattern lowered AST" expectedLoweredProgram (lowerSurfaceExpr surfaceProgram)
    )
  where
    expectedSurfaceProgram =
      SEBlock
        [ SSLet
            "x"
            (SourceSpan 1 1)
            ( SECase
                (SEVar "n")
                [ SurfaceCaseArm
                    (SPLiteral (SLInt 0))
                    Nothing
                    (SELit (SLInt 0)),
                  SurfaceCaseArm
                    (SPOr [SPVariable "item", SPAs "item" SPWildcard])
                    Nothing
                    (SEVar "item")
                ]
            )
        ]
    expectedLoweredProgram =
      EBlock
        [ SLet
            "x"
            (SourceSpan 1 1)
            ( EPatternCase
                (EVar "n")
                [ CaseArm
                    (PLiteral (LInt 0))
                    Nothing
                    (ELit (LInt 0)),
                  CaseArm
                    (POr [PVariable "item", PAs "item" PWildcard])
                    Nothing
                    (EVar "item")
                ]
            )
        ]

testKeepsPipeOperatorInOrPatternArmBody :: IO ()
testKeepsPipeOperatorInOrPatternArmBody =
  assertRight
    "or-pattern arm body keeps infix pipe operator"
    (parseSurfaceProgram "x = case value { | Just item | Also item -> item | f | Nothing -> 0 }.")
    (\surfaceProgram -> assertEqual "or-pattern pipe body lowered AST" expectedLoweredProgram (lowerSurfaceExpr surfaceProgram))
  where
    expectedLoweredProgram =
      EBlock
        [ SLet
            "x"
            (SourceSpan 1 1)
            ( EPatternCase
                (EVar "value")
                [ CaseArm
                    ( POr
                        [ PConstructor "Just" [PVariable "item"],
                          PConstructor "Also" [PVariable "item"]
                        ]
                    )
                    Nothing
                    (EBinary "|" (EVar "item") (EVar "f")),
                  CaseArm
                    (PConstructor "Nothing" [])
                    Nothing
                    (ELit (LInt 0))
                ]
            )
        ]

testParsesGuardedCaseArmWithPipeExpressionAfterPreviousArm :: IO ()
testParsesGuardedCaseArmWithPipeExpressionAfterPreviousArm =
  assertRight
    "guarded pipe expression after previous arm"
    (parseSurfaceProgram "x = case value { | 0 -> 0 | item if left | right -> 1 }.")
    (\surfaceProgram -> assertEqual "guarded pipe expression surface AST" expectedSurfaceProgram surfaceProgram)
  where
    expectedSurfaceProgram =
      SEBlock
        [ SSLet
            "x"
            (SourceSpan 1 1)
            ( SECase
                (SEVar "value")
                [ SurfaceCaseArm
                    (SPLiteral (SLInt 0))
                    Nothing
                    (SELit (SLInt 0)),
                  SurfaceCaseArm
                    (SPVariable "item")
                    (Just (SEBinary "|" (SEVar "left") (SEVar "right")))
                    (SELit (SLInt 1))
                ]
            )
        ]

testParsesGuardedCaseArmWithDefinitePipeRhsGuards :: IO ()
testParsesGuardedCaseArmWithDefinitePipeRhsGuards =
  assertRight
    "guarded pipe expression with literal and constructor-shaped RHS"
    (parseSurfaceProgram "x = case value { | item if left | True -> 1 | other if left | Nothing -> 2 }.")
    (\surfaceProgram -> assertEqual "guarded definite pipe RHS surface AST" expectedSurfaceProgram surfaceProgram)
  where
    expectedSurfaceProgram =
      SEBlock
        [ SSLet
            "x"
            (SourceSpan 1 1)
            ( SECase
                (SEVar "value")
                [ SurfaceCaseArm
                    (SPVariable "item")
                    (Just (SEBinary "|" (SEVar "left") (SELit (SLBool True))))
                    (SELit (SLInt 1)),
                  SurfaceCaseArm
                    (SPVariable "other")
                    (Just (SEBinary "|" (SEVar "left") (SEVar "Nothing")))
                    (SELit (SLInt 2))
                ]
            )
        ]

testKeepsHigherPrecedencePipeInComparisonGuardRhs :: IO ()
testKeepsHigherPrecedencePipeInComparisonGuardRhs =
  assertRight
    "comparison guard keeps pipe expression in RHS"
    (parseSurfaceProgram "x = case value { | item if left == right | True -> 1 }.")
    (\surfaceProgram -> assertEqual "comparison guard pipe RHS surface AST" expectedSurfaceProgram surfaceProgram)
  where
    expectedSurfaceProgram =
      SEBlock
        [ SSLet
            "x"
            (SourceSpan 1 1)
            ( SECase
                (SEVar "value")
                [ SurfaceCaseArm
                    (SPVariable "item")
                    (Just (SEBinary "==" (SEVar "left") (SEBinary "|" (SEVar "right") (SELit (SLBool True)))))
                    (SELit (SLInt 1))
                ]
            )
        ]

testKeepsLiteralPipeOperandInEqualityGuardRhs :: IO ()
testKeepsLiteralPipeOperandInEqualityGuardRhs =
  assertRight
    "equality guard keeps literal pipe operand in RHS"
    (parseSurfaceProgram "x = case m { | item if item == 0 | Just -> item | _ -> m }.")
    (\surfaceProgram -> assertEqual "equality guard literal pipe RHS surface AST" expectedSurfaceProgram surfaceProgram)
  where
    expectedSurfaceProgram =
      SEBlock
        [ SSLet
            "x"
            (SourceSpan 1 1)
            ( SECase
                (SEVar "m")
                [ SurfaceCaseArm
                    (SPVariable "item")
                    (Just (SEBinary "==" (SEVar "item") (SEBinary "|" (SELit (SLInt 0)) (SEVar "Just"))))
                    (SEVar "item"),
                  SurfaceCaseArm
                    SPWildcard
                    Nothing
                    (SEVar "m")
                ]
            )
        ]

testKeepsLiteralPipeOperandInInequalityGuardRhs :: IO ()
testKeepsLiteralPipeOperandInInequalityGuardRhs =
  assertRight
    "inequality guard keeps literal pipe operand in RHS"
    (parseSurfaceProgram "x = case m { | item if item != 0 | Just -> item | _ -> m }.")
    (\surfaceProgram -> assertEqual "inequality guard literal pipe RHS surface AST" expectedSurfaceProgram surfaceProgram)
  where
    expectedSurfaceProgram =
      SEBlock
        [ SSLet
            "x"
            (SourceSpan 1 1)
            ( SECase
                (SEVar "m")
                [ SurfaceCaseArm
                    (SPVariable "item")
                    (Just (SEBinary "!=" (SEVar "item") (SEBinary "|" (SELit (SLInt 0)) (SEVar "Just"))))
                    (SEVar "item"),
                  SurfaceCaseArm
                    SPWildcard
                    Nothing
                    (SEVar "m")
                ]
            )
        ]

testKeepsLiteralPipeOperandInOrderingGuardRhs :: IO ()
testKeepsLiteralPipeOperandInOrderingGuardRhs = do
  assertOrderingGuard "<" "x = case m { | item if item < 0 | Just -> item | _ -> m }."
  assertOrderingGuard "<=" "x = case m { | item if item <= 0 | Just -> item | _ -> m }."
  assertOrderingGuard ">=" "x = case m { | item if item >= 0 | Just -> item | _ -> m }."
  assertOrderingGuard ">" "x = case m { | item if item > 0 | Just -> item | _ -> m }."
  where
    assertOrderingGuard operator source =
      assertRight
        ("ordering guard keeps literal pipe operand in RHS for " <> operator)
        (parseSurfaceProgram source)
        (\surfaceProgram -> assertEqual "ordering guard literal pipe RHS surface AST" (expectedSurfaceProgram operator) surfaceProgram)

    expectedSurfaceProgram operator =
      SEBlock
        [ SSLet
            "x"
            (SourceSpan 1 1)
            ( SECase
                (SEVar "m")
                [ SurfaceCaseArm
                    (SPVariable "item")
                    (Just (SEBinary operator (SEVar "item") (SEBinary "|" (SELit (SLInt 0)) (SEVar "Just"))))
                    (SEVar "item"),
                  SurfaceCaseArm
                    SPWildcard
                    Nothing
                    (SEVar "m")
                ]
            )
        ]

testKeepsConstructorIfExpressionPipeRhsBeforeArmArrow :: IO ()
testKeepsConstructorIfExpressionPipeRhsBeforeArmArrow =
  assertRight
    "constructor if-expression pipe RHS before arm arrow"
    (parseSurfaceProgram "x = case m { | item if item == 0 | Just if ok 1 else 2 -> item | _ -> m }.")
    (\surfaceProgram -> assertEqual "constructor if-expression pipe RHS surface AST" expectedSurfaceProgram surfaceProgram)
  where
    expectedSurfaceProgram =
      SEBlock
        [ SSLet
            "x"
            (SourceSpan 1 1)
            ( SECase
                (SEVar "m")
                [ SurfaceCaseArm
                    (SPVariable "item")
                    ( Just
                        ( SEBinary
                            "=="
                            (SEVar "item")
                            ( SEBinary
                                "|"
                                (SELit (SLInt 0))
                                (SEApply (SEVar "Just") (SEIf (SEVar "ok") (SELit (SLInt 1)) (SELit (SLInt 2))))
                            )
                        )
                    )
                    (SEVar "item"),
                  SurfaceCaseArm
                    SPWildcard
                    Nothing
                    (SEVar "m")
                ]
            )
        ]

testRejectsMissingGuardArrowBeforeGuardedConstructorArm :: IO ()
testRejectsMissingGuardArrowBeforeGuardedConstructorArm =
  assertLeftDiagnosticContains
    "missing guard arrow before guarded constructor arm"
    "expected '->'"
    (parseSurfaceProgram "x = case m { | item if item < 0 | Just if ok -> item | _ -> m }.")

testKeepsAsPatternConstructorArgumentsAtomic :: IO ()
testKeepsAsPatternConstructorArgumentsAtomic =
  assertRight
    "as-pattern constructor argument parse + lower"
    (parseSurfaceProgram "x = case value { | Pair whole @ Nothing item -> item | _ -> 0 }.")
    ( \surfaceProgram -> do
        assertEqual "as-pattern constructor argument surface AST" expectedSurfaceProgram surfaceProgram
        assertEqual "as-pattern constructor argument lowered AST" expectedLoweredProgram (lowerSurfaceExpr surfaceProgram)
    )
  where
    expectedSurfaceProgram =
      SEBlock
        [ SSLet
            "x"
            (SourceSpan 1 1)
            ( SECase
                (SEVar "value")
                [ SurfaceCaseArm
                    (SPConstructor "Pair" [SPAs "whole" (SPConstructor "Nothing" []), SPVariable "item"])
                    Nothing
                    (SEVar "item"),
                  SurfaceCaseArm
                    SPWildcard
                    Nothing
                    (SELit (SLInt 0))
                ]
            )
        ]
    expectedLoweredProgram =
      EBlock
        [ SLet
            "x"
            (SourceSpan 1 1)
            ( EPatternCase
                (EVar "value")
                [ CaseArm
                    (PConstructor "Pair" [PAs "whole" (PConstructor "Nothing" []), PVariable "item"])
                    Nothing
                    (EVar "item"),
                  CaseArm
                    PWildcard
                    Nothing
                    (ELit (LInt 0))
                ]
            )
        ]

testParsesAsPatternLambdaParameter :: IO ()
testParsesAsPatternLambdaParameter =
  assertEqual
    "as-pattern lambda parameter"
    ( Right
        ( SEBlock
            [ SSLet
                "f"
                (SourceSpan 1 1)
                ( SELambda
                    (SurfaceLambdaPattern (SPAs "whole" (SPConsList (SPVariable "head") (SPVariable "tail"))) :| [])
                    (SEVar "head")
                )
            ]
        )
    )
    (parseSurfaceProgram "f = \\(whole @ [head | tail]) -> head.")

testParsesConstructorPatternCaseArms :: IO ()
testParsesConstructorPatternCaseArms =
  assertRight
    "constructor pattern parse + lower"
    (parseSurfaceProgram "x = case value { | Just item -> item | Nothing -> 0 }.")
    ( \surfaceProgram -> do
        assertEqual "constructor pattern surface AST" expectedSurfaceProgram surfaceProgram
        assertEqual "constructor pattern lowered AST" expectedLoweredProgram (lowerSurfaceExpr surfaceProgram)
    )
  where
    expectedSurfaceProgram =
      SEBlock
        [ SSLet
            "x"
            (SourceSpan 1 1)
            ( SECase
                (SEVar "value")
                [ SurfaceCaseArm
                    (SPConstructor "Just" [SPVariable "item"])
                    Nothing
                    (SEVar "item"),
                  SurfaceCaseArm
                    (SPConstructor "Nothing" [])
                    Nothing
                    (SELit (SLInt 0))
                ]
            )
        ]
    expectedLoweredProgram =
      EBlock
        [ SLet
            "x"
            (SourceSpan 1 1)
            ( EPatternCase
                (EVar "value")
                [ CaseArm
                    (PConstructor "Just" [PVariable "item"])
                    Nothing
                    (EVar "item"),
                  CaseArm
                    (PConstructor "Nothing" [])
                    Nothing
                    (ELit (LInt 0))
                ]
            )
        ]

testParsesMultiArgumentConstructorPatternsWithNullarySubpatterns :: IO ()
testParsesMultiArgumentConstructorPatternsWithNullarySubpatterns =
  assertRight
    "multi-argument constructor pattern parse + lower"
    (parseSurfaceProgram "x = case value { | Pair Nothing item -> item | _ -> 0 }.")
    ( \surfaceProgram -> do
        assertEqual "multi-argument constructor pattern surface AST" expectedSurfaceProgram surfaceProgram
        assertEqual "multi-argument constructor pattern lowered AST" expectedLoweredProgram (lowerSurfaceExpr surfaceProgram)
    )
  where
    expectedSurfaceProgram =
      SEBlock
        [ SSLet
            "x"
            (SourceSpan 1 1)
            ( SECase
                (SEVar "value")
                [ SurfaceCaseArm
                    (SPConstructor "Pair" [SPConstructor "Nothing" [], SPVariable "item"])
                    Nothing
                    (SEVar "item"),
                  SurfaceCaseArm
                    SPWildcard
                    Nothing
                    (SELit (SLInt 0))
                ]
            )
        ]
    expectedLoweredProgram =
      EBlock
        [ SLet
            "x"
            (SourceSpan 1 1)
            ( EPatternCase
                (EVar "value")
                [ CaseArm
                    (PConstructor "Pair" [PConstructor "Nothing" [], PVariable "item"])
                    Nothing
                    (EVar "item"),
                  CaseArm
                    PWildcard
                    Nothing
                    (ELit (LInt 0))
                ]
            )
        ]

testParsesNullaryConstructorSubpatterns :: IO ()
testParsesNullaryConstructorSubpatterns =
  assertRight
    "nullary constructor subpattern parse + lower"
    (parseSurfaceProgram "x = case value { | Just Nothing -> 1 | _ -> 0 }.")
    ( \surfaceProgram -> do
        assertEqual "nullary constructor subpattern surface AST" expectedSurfaceProgram surfaceProgram
        assertEqual "nullary constructor subpattern lowered AST" expectedLoweredProgram (lowerSurfaceExpr surfaceProgram)
    )
  where
    expectedSurfaceProgram =
      SEBlock
        [ SSLet
            "x"
            (SourceSpan 1 1)
            ( SECase
                (SEVar "value")
                [ SurfaceCaseArm
                    (SPConstructor "Just" [SPConstructor "Nothing" []])
                    Nothing
                    (SELit (SLInt 1)),
                  SurfaceCaseArm
                    SPWildcard
                    Nothing
                    (SELit (SLInt 0))
                ]
            )
        ]
    expectedLoweredProgram =
      EBlock
        [ SLet
            "x"
            (SourceSpan 1 1)
            ( EPatternCase
                (EVar "value")
                [ CaseArm
                    (PConstructor "Just" [PConstructor "Nothing" []])
                    Nothing
                    (ELit (LInt 1)),
                  CaseArm
                    PWildcard
                    Nothing
                    (ELit (LInt 0))
                ]
            )
        ]

testParsesListPatternCaseArms :: IO ()
testParsesListPatternCaseArms =
  assertRight
    "list pattern parse + lower"
    (parseSurfaceProgram "x = case values { | [head, _] -> head | [] -> 0 }.")
    ( \surfaceProgram -> do
        assertEqual "list pattern surface AST" expectedSurfaceProgram surfaceProgram
        assertEqual "list pattern lowered AST" expectedLoweredProgram (lowerSurfaceExpr surfaceProgram)
    )
  where
    expectedSurfaceProgram =
      SEBlock
        [ SSLet
            "x"
            (SourceSpan 1 1)
            ( SECase
                (SEVar "values")
                [ SurfaceCaseArm
                    (SPList [SPVariable "head", SPWildcard])
                    Nothing
                    (SEVar "head"),
                  SurfaceCaseArm
                    (SPList [])
                    Nothing
                    (SELit (SLInt 0))
                ]
            )
        ]
    expectedLoweredProgram =
      EBlock
        [ SLet
            "x"
            (SourceSpan 1 1)
            ( EPatternCase
                (EVar "values")
                [ CaseArm
                    (PList [PVariable "head", PWildcard])
                    Nothing
                    (EVar "head"),
                  CaseArm
                    (PList [])
                    Nothing
                    (ELit (LInt 0))
                ]
            )
        ]

testParsesCanonicalDataDeclarationAndLowersConstructorArities :: IO ()
testParsesCanonicalDataDeclarationAndLowersConstructorArities =
  assertRight
    "data declaration parse + lower"
    (parseSurfaceProgram "data Maybe = Just value | Nothing.")
    ( \surfaceProgram -> do
        assertEqual "data declaration surface AST" expectedSurfaceProgram surfaceProgram
        assertEqual "data declaration lowered AST" expectedLoweredProgram (lowerSurfaceExpr surfaceProgram)
    )
  where
    expectedSurfaceProgram =
      SEBlock
        [ SSData
            (SourceSpan 1 1)
            "Maybe"
            []
            [ SurfaceDataConstructor "Just" [SurfaceDataConstructorArgumentName "value"],
              SurfaceDataConstructor "Nothing" []
            ]
        ]
    expectedLoweredProgram =
      EBlock
        [ SData
            (SourceSpan 1 1)
            "Maybe"
            []
            [ DataConstructor "Just" [DataConstructorArgumentName "value"],
              DataConstructor "Nothing" []
            ]
        ]

testParsesGenericDataDeclarationParameters :: IO ()
testParsesGenericDataDeclarationParameters =
  assertRight
    "generic data declaration parse + lower"
    (parseSurfaceProgram "data Maybe a = Nothing | Just a.")
    ( \surfaceProgram -> do
        assertEqual "generic data declaration surface AST" expectedSurfaceProgram surfaceProgram
        assertEqual "generic data declaration lowered AST" expectedLoweredProgram (lowerSurfaceExpr surfaceProgram)
    )
  where
    expectedSurfaceProgram =
      SEBlock
        [ SSData
            (SourceSpan 1 1)
            "Maybe"
            ["a"]
            [ SurfaceDataConstructor "Nothing" [],
              SurfaceDataConstructor "Just" [SurfaceDataConstructorArgumentName "a"]
            ]
        ]
    expectedLoweredProgram =
      EBlock
        [ SData
            (SourceSpan 1 1)
            "Maybe"
            ["a"]
            [ DataConstructor "Nothing" [],
              DataConstructor "Just" [DataConstructorArgumentName "a"]
            ]
        ]

testParsesNestedCaseExpression :: IO ()
testParsesNestedCaseExpression =
  assertRight
    "nested case parse + lower"
    (parseSurfaceProgram "x = case n { | 0 -> case y { | 1 -> True | _ -> False } | _ -> False }.")
    (\surfaceProgram -> assertEqual "nested lowered case AST" expectedProgram (lowerSurfaceExpr surfaceProgram))
  where
    expectedProgram =
      EBlock
        [ SLet
            "x"
            (SourceSpan 1 1)
            ( EPatternCase
                (EVar "n")
                [ CaseArm
                    (PLiteral (LInt 0))
                    Nothing
                    ( EPatternCase
                        (EVar "y")
                        [ CaseArm (PLiteral (LInt 1)) Nothing (ELit (LBool True)),
                          CaseArm PWildcard Nothing (ELit (LBool False))
                        ]
                    ),
                  CaseArm PWildcard Nothing (ELit (LBool False))
                ]
            )
        ]

testParsesIfExpressionInsideCaseArmBody :: IO ()
testParsesIfExpressionInsideCaseArmBody =
  assertRight
    "if expression remains within first case arm"
    (parseSurfaceProgram "x = case n { | 0 -> if True 1 else 2 | _ -> 3 }.")
    (\surfaceProgram -> assertEqual "if-in-arm lowered case AST" expectedProgram (lowerSurfaceExpr surfaceProgram))
  where
    expectedProgram =
      EBlock
        [ SLet
            "x"
            (SourceSpan 1 1)
            ( EPatternCase
                (EVar "n")
                [ CaseArm
                    (PLiteral (LInt 0))
                    Nothing
                    (EIf (ELit (LBool True)) (ELit (LInt 1)) (ELit (LInt 2))),
                  CaseArm PWildcard Nothing (ELit (LInt 3))
                ]
            )
        ]

testParsesLambdaExpressionInsideCaseArmBody :: IO ()
testParsesLambdaExpressionInsideCaseArmBody =
  assertRight
    "lambda expression remains within first case arm"
    (parseSurfaceProgram "x = case n { | 0 -> \\(y) -> y | _ -> 3 }.")
    (\surfaceProgram -> assertEqual "lambda-in-arm lowered case AST" expectedProgram (lowerSurfaceExpr surfaceProgram))
  where
    expectedProgram =
      EBlock
        [ SLet
            "x"
            (SourceSpan 1 1)
            ( EPatternCase
                (EVar "n")
                [ CaseArm
                    (PLiteral (LInt 0))
                    Nothing
                    (ELambda "y" (EVar "y")),
                  CaseArm PWildcard Nothing (ELit (LInt 3))
                ]
            )
        ]

testParsesMixedLiteralWildcardLaterOrPatternArmAfterBody :: IO ()
testParsesMixedLiteralWildcardLaterOrPatternArmAfterBody =
  assertRight
    "mixed literal-wildcard later or-pattern case arm parse + lower"
    (parseSurfaceProgram "x = case n { | 0 -> 1 | 2 | _ -> 3 }.")
    ( \surfaceProgram -> do
        assertEqual "mixed literal-wildcard later or-pattern surface AST" expectedSurfaceProgram surfaceProgram
        assertEqual "mixed literal-wildcard later or-pattern lowered AST" expectedLoweredProgram (lowerSurfaceExpr surfaceProgram)
    )
  where
    expectedSurfaceProgram =
      SEBlock
        [ SSLet
            "x"
            (SourceSpan 1 1)
            ( SECase
                (SEVar "n")
                [ SurfaceCaseArm
                    (SPLiteral (SLInt 0))
                    Nothing
                    (SELit (SLInt 1)),
                  SurfaceCaseArm
                    (SPOr [SPLiteral (SLInt 2), SPWildcard])
                    Nothing
                    (SELit (SLInt 3))
                ]
            )
        ]
    expectedLoweredProgram =
      EBlock
        [ SLet
            "x"
            (SourceSpan 1 1)
            ( EPatternCase
                (EVar "n")
                [ CaseArm
                    (PLiteral (LInt 0))
                    Nothing
                    (ELit (LInt 1)),
                  CaseArm
                    (POr [PLiteral (LInt 2), PWildcard])
                    Nothing
                    (ELit (LInt 3))
                ]
            )
        ]

testKeepsPipeOperatorInsideBodyBeforeConstructorArmBoundary :: IO ()
testKeepsPipeOperatorInsideBodyBeforeConstructorArmBoundary =
  assertRight
    "pipe operator stays in constructor arm body"
    (parseSurfaceProgram "x = case value { | Just item -> 1 | 2 | Nothing -> 3 }.")
    (\surfaceProgram -> assertEqual "constructor arm boundary lowered AST" expectedProgram (lowerSurfaceExpr surfaceProgram))
  where
    expectedProgram =
      EBlock
        [ SLet
            "x"
            (SourceSpan 1 1)
            ( EPatternCase
                (EVar "value")
                [ CaseArm
                    (PConstructor "Just" [PVariable "item"])
                    Nothing
                    (EBinary "|" (ELit (LInt 1)) (ELit (LInt 2))),
                  CaseArm
                    (PConstructor "Nothing" [])
                    Nothing
                    (ELit (LInt 3))
                ]
            )
        ]

testKeepsPipeOperatorInsideBodyBeforeLiteralArmBoundary :: IO ()
testKeepsPipeOperatorInsideBodyBeforeLiteralArmBoundary =
  assertRight
    "pipe operator stays in body before literal arm boundary"
    (parseSurfaceProgram "x = case value { | _ -> 1 | 2 | 3 -> 4 }.")
    (\surfaceProgram -> assertEqual "literal arm boundary lowered AST" expectedProgram (lowerSurfaceExpr surfaceProgram))
  where
    expectedProgram =
      EBlock
        [ SLet
            "x"
            (SourceSpan 1 1)
            ( EPatternCase
                (EVar "value")
                [ CaseArm
                    PWildcard
                    Nothing
                    (EBinary "|" (ELit (LInt 1)) (ELit (LInt 2))),
                  CaseArm
                    (PLiteral (LInt 3))
                    Nothing
                    (ELit (LInt 4))
                ]
            )
        ]

testKeepsBareListLiteralAfterPipeOperator :: IO ()
testKeepsBareListLiteralAfterPipeOperator =
  assertRight
    "bare list literal stays in case arm body"
    (parseSurfaceProgram "x = case value { | _ -> 1 | [2] }.")
    (\surfaceProgram -> assertEqual "list literal in arm body lowered AST" expectedProgram (lowerSurfaceExpr surfaceProgram))
  where
    expectedProgram =
      EBlock
        [ SLet
            "x"
            (SourceSpan 1 1)
            ( EPatternCase
                (EVar "value")
                [ CaseArm
                    PWildcard
                    Nothing
                    (EBinary "|" (ELit (LInt 1)) (EList [ELit (LInt 2)]))
                ]
            )
        ]

testKeepsBareConstructorValueAfterPipeOperator :: IO ()
testKeepsBareConstructorValueAfterPipeOperator =
  assertRight
    "bare constructor value stays in case arm body"
    (parseSurfaceProgram "x = case value { | _ -> 1 | Nothing }.")
    (\surfaceProgram -> assertEqual "constructor value in arm body lowered AST" expectedProgram (lowerSurfaceExpr surfaceProgram))
  where
    expectedProgram =
      EBlock
        [ SLet
            "x"
            (SourceSpan 1 1)
            ( EPatternCase
                (EVar "value")
                [ CaseArm
                    PWildcard
                    Nothing
                    (EBinary "|" (ELit (LInt 1)) (EVar "Nothing"))
                ]
            )
        ]

testKeepsListApplicationAfterPipeOperator :: IO ()
testKeepsListApplicationAfterPipeOperator =
  assertRight
    "list application stays in case arm body"
    (parseSurfaceProgram "x = case values { | _ -> 1 | [head] 2 }.")
    (\surfaceProgram -> assertEqual "list application in arm body lowered AST" expectedProgram (lowerSurfaceExpr surfaceProgram))
  where
    expectedProgram =
      EBlock
        [ SLet
            "x"
            (SourceSpan 1 1)
            ( EPatternCase
                (EVar "values")
                [ CaseArm
                    PWildcard
                    Nothing
                    (EBinary "|" (ELit (LInt 1)) (EApply (EList [EVar "head"]) (ELit (LInt 2))))
                ]
            )
        ]

testKeepsConstructorApplicationAfterPipeOperator :: IO ()
testKeepsConstructorApplicationAfterPipeOperator =
  assertRight
    "constructor application stays in case arm body"
    (parseSurfaceProgram "x = case value { | _ -> 1 | Just a b }.")
    (\surfaceProgram -> assertEqual "constructor application in arm body lowered AST" expectedProgram (lowerSurfaceExpr surfaceProgram))
  where
    expectedProgram =
      EBlock
        [ SLet
            "x"
            (SourceSpan 1 1)
            ( EPatternCase
                (EVar "value")
                [ CaseArm
                    PWildcard
                    Nothing
                    (EBinary "|" (ELit (LInt 1)) (EApply (EApply (EVar "Just") (EVar "a")) (EVar "b")))
                ]
            )
        ]

testKeepsLambdaApplicationAfterPipeOperator :: IO ()
testKeepsLambdaApplicationAfterPipeOperator =
  assertRight
    "lambda application stays in case arm body"
    (parseSurfaceProgram "x = case value { | _ -> 1 | f \\(y) -> y }.")
    (\surfaceProgram -> assertEqual "lambda application in arm body lowered AST" expectedProgram (lowerSurfaceExpr surfaceProgram))
  where
    expectedProgram =
      EBlock
        [ SLet
            "x"
            (SourceSpan 1 1)
            ( EPatternCase
                (EVar "value")
                [ CaseArm
                    PWildcard
                    Nothing
                    ( EBinary
                        "|"
                        (ELit (LInt 1))
                        (EApply (EVar "f") (ELambda "y" (EVar "y")))
                    )
                ]
            )
        ]

testKeepsUnderscoreApplicationAfterPipeOperator :: IO ()
testKeepsUnderscoreApplicationAfterPipeOperator =
  assertRight
    "underscore application stays in case arm body"
    (parseSurfaceProgram "x = case value { | 0 -> 1 | _ y }.")
    (\surfaceProgram -> assertEqual "underscore application in arm body lowered AST" expectedProgram (lowerSurfaceExpr surfaceProgram))
  where
    expectedProgram =
      EBlock
        [ SLet
            "x"
            (SourceSpan 1 1)
            ( EPatternCase
                (EVar "value")
                [ CaseArm
                    (PLiteral (LInt 0))
                    Nothing
                    (EBinary "|" (ELit (LInt 1)) (EApply (EVar "_") (EVar "y")))
                ]
            )
        ]

testKeepsUnderscoreBooleanApplicationAfterPipeOperator :: IO ()
testKeepsUnderscoreBooleanApplicationAfterPipeOperator =
  assertRight
    "underscore boolean application stays in case arm body"
    (parseSurfaceProgram "x = case value { | 0 -> 1 | _ False }.")
    (\surfaceProgram -> assertEqual "underscore boolean application in arm body lowered AST" expectedProgram (lowerSurfaceExpr surfaceProgram))
  where
    expectedProgram =
      EBlock
        [ SLet
            "x"
            (SourceSpan 1 1)
            ( EPatternCase
                (EVar "value")
                [ CaseArm
                    (PLiteral (LInt 0))
                    Nothing
                    (EBinary "|" (ELit (LInt 1)) (EApply (EVar "_") (ELit (LBool False))))
                ]
            )
        ]

testParsesCaseScrutineeWithBlockArgument :: IO ()
testParsesCaseScrutineeWithBlockArgument =
  assertRight
    "case scrutinee keeps block argument"
    (parseSurfaceProgram "x = case f { y = 1. y. } { | 1 -> True | _ -> False }.")
    (\surfaceProgram -> assertEqual "block-argument scrutinee lowered case AST" expectedProgram (lowerSurfaceExpr surfaceProgram))
  where
    expectedProgram =
      EBlock
        [ SLet
            "x"
            (SourceSpan 1 1)
            ( EPatternCase
                ( EApply
                    (EVar "f")
                    ( EBlock
                        [ SLet "y" (SourceSpan 1 14) (ELit (LInt 1)),
                          SExpr (SourceSpan 1 21) (EVar "y")
                        ]
                    )
                )
                [ CaseArm (PLiteral (LInt 1)) Nothing (ELit (LBool True)),
                  CaseArm PWildcard Nothing (ELit (LBool False))
                ]
            )
        ]

testReportsMissingCaseBodyForBlockScrutinee :: IO ()
testReportsMissingCaseBodyForBlockScrutinee =
  assertLeftDiagnosticContains
    "block scrutinee missing case body"
    "expected '{' before end of input after 'case'"
    (parseSurfaceProgram "x = case f { y = 1. y. }.")

testReportsBlockErrorForUnterminatedFractionalBlockScrutinee :: IO ()
testReportsBlockErrorForUnterminatedFractionalBlockScrutinee =
  assertLeftDiagnosticContains
    "unterminated fractional block scrutinee parse error"
    "expected '.'"
    (parseSurfaceProgram "x = case f { y = 1.5 }.")

testReportsMissingArmArrowForBlockScrutinee :: IO ()
testReportsMissingArmArrowForBlockScrutinee =
  assertLeftDiagnosticContains
    "block scrutinee missing arm arrow"
    "expected '->'"
    (parseSurfaceProgram "x = case f { y = 1. y. } { | 1 True }.")

testReportsInvalidCaseScrutineeSyntax :: IO ()
testReportsInvalidCaseScrutineeSyntax =
  assertLeftDiagnosticContains
    "invalid case scrutinee syntax"
    "unexpected token '+'"
    (parseSurfaceProgram "x = case + { | 0 -> True }.")

testRejectsCaseExpressionWithoutPipe :: IO ()
testRejectsCaseExpressionWithoutPipe =
  assertLeftDiagnosticContains
    "missing case-arm pipe"
    "expected '|'"
    (parseSurfaceProgram "x = case n { 0 -> True }.")

testRejectsCaseExpressionWithoutArrow :: IO ()
testRejectsCaseExpressionWithoutArrow =
  assertLeftDiagnosticContains
    "missing case-arm arrow"
    "expected '->'"
    (parseSurfaceProgram "x = case n { | 0 True }.")

testRejectsDataDeclarationWithoutConstructors :: IO ()
testRejectsDataDeclarationWithoutConstructors =
  assertLeftDiagnosticContains
    "empty data constructor list"
    "expected constructor declaration"
    (parseSurfaceProgram "data Maybe = .")

testRejectsDuplicateConstructorsInDataDeclaration :: IO ()
testRejectsDuplicateConstructorsInDataDeclaration =
  assertLeftDiagnosticContains
    "duplicate data constructor"
    "duplicate constructor declaration 'Nothing'"
    (parseSurfaceProgram "data Maybe = Nothing | Nothing value.")

testRejectsDuplicateDataTypeParameters :: IO ()
testRejectsDuplicateDataTypeParameters =
  assertLeftDiagnosticContains
    "duplicate data type parameter diagnostic"
    "duplicate type parameter 'a' in data declaration"
    (parseSurfaceProgram "data Pair a a = Pair a a.")

testRejectsUndeclaredGenericConstructorPayloadNames :: IO ()
testRejectsUndeclaredGenericConstructorPayloadNames =
  assertLeftDiagnosticContains
    "undeclared generic constructor payload diagnostic"
    "constructor payload type parameter 'b' is not declared in data type 'Maybe'"
    (parseSurfaceProgram "data Maybe a = Just b.")

testRejectsDataDeclarationWithMalformedPipePlacement :: IO ()
testRejectsDataDeclarationWithMalformedPipePlacement =
  assertLeftDiagnosticContains
    "malformed constructor separator"
    "expected constructor declaration"
    (parseSurfaceProgram "data Maybe = Just value | .")

testRejectsDataDeclarationMissingTerminator :: IO ()
testRejectsDataDeclarationMissingTerminator =
  assertLeftDiagnosticContains
    "missing data declaration terminator"
    "expected '.'"
    (parseSurfaceProgram "data Maybe = Just value | Nothing")

testParsesTuplePatternCaseArms :: IO ()
testParsesTuplePatternCaseArms =
  assertRight
    "tuple pattern case arm"
    (parseSurfaceProgram "x = case pair { | (left, right) -> left | _ -> 0 }.")
    (\_ -> pure ())

testParsesConsLikeListPattern :: IO ()
testParsesConsLikeListPattern =
  assertRight
    "cons-like list pattern parse + lower"
    (parseSurfaceProgram "x = case values { | [head | tail] -> head | _ -> 0 }.")
    ( \surfaceProgram -> do
        assertEqual "cons-like list pattern surface AST" expectedSurfaceProgram surfaceProgram
        assertEqual "cons-like list pattern lowered AST" expectedLoweredProgram (lowerSurfaceExpr surfaceProgram)
    )
  where
    expectedSurfaceProgram =
      SEBlock
        [ SSLet
            "x"
            (SourceSpan 1 1)
            ( SECase
                (SEVar "values")
                [ SurfaceCaseArm
                    (SPConsList (SPVariable "head") (SPVariable "tail"))
                    Nothing
                    (SEVar "head"),
                  SurfaceCaseArm
                    SPWildcard
                    Nothing
                    (SELit (SLInt 0))
                ]
            )
        ]
    expectedLoweredProgram =
      EBlock
        [ SLet
            "x"
            (SourceSpan 1 1)
            ( EPatternCase
                (EVar "values")
                [ CaseArm
                    (PConsList (PVariable "head") (PVariable "tail"))
                    Nothing
                    (EVar "head"),
                  CaseArm
                    PWildcard
                    Nothing
                    (ELit (LInt 0))
                ]
            )
        ]

testParsesConsLikeListPatternInsideConstructorPattern :: IO ()
testParsesConsLikeListPatternInsideConstructorPattern =
  assertRight
    "cons-like list pattern inside constructor pattern parse + lower"
    (parseSurfaceProgram "x = case value { | Just [head | tail] -> head | _ -> 0 }.")
    ( \surfaceProgram -> do
        assertEqual "cons-like list constructor surface AST" expectedSurfaceProgram surfaceProgram
        assertEqual "cons-like list constructor lowered AST" expectedLoweredProgram (lowerSurfaceExpr surfaceProgram)
    )
  where
    expectedSurfaceProgram =
      SEBlock
        [ SSLet
            "x"
            (SourceSpan 1 1)
            ( SECase
                (SEVar "value")
                [ SurfaceCaseArm
                    (SPConstructor "Just" [SPConsList (SPVariable "head") (SPVariable "tail")])
                    Nothing
                    (SEVar "head"),
                  SurfaceCaseArm
                    SPWildcard
                    Nothing
                    (SELit (SLInt 0))
                ]
            )
        ]
    expectedLoweredProgram =
      EBlock
        [ SLet
            "x"
            (SourceSpan 1 1)
            ( EPatternCase
                (EVar "value")
                [ CaseArm
                    (PConstructor "Just" [PConsList (PVariable "head") (PVariable "tail")])
                    Nothing
                    (EVar "head"),
                  CaseArm
                    PWildcard
                    Nothing
                    (ELit (LInt 0))
                ]
            )
        ]

testRejectsMalformedParenthesizedListLikePattern :: IO ()
testRejectsMalformedParenthesizedListLikePattern =
  assertLeftDiagnosticContains
    "malformed parenthesized list-like pattern"
    "expected ',' or ']'"
    (parseSurfaceProgram "x = case pair { | (left, [right) ]) -> left | _ -> 0 }.")

testRejectsMalformedListPattern :: IO ()
testRejectsMalformedListPattern =
  assertLeftDiagnosticContains
    "malformed list pattern"
    "expected ',' or ']'"
    (parseSurfaceProgram "x = case values { | [head tail] -> head }.")

testRejectsMalformedLaterListPattern :: IO ()
testRejectsMalformedLaterListPattern =
  assertLeftDiagnosticContains
    "malformed later list pattern"
    "expected ',' or ']'"
    (parseSurfaceProgram "x = case values { | 0 -> 1 | [head tail] -> head }.")

testRejectsMalformedGuardExpression :: IO ()
testRejectsMalformedGuardExpression =
  assertLeftDiagnosticContains
    "malformed guard expression"
    "expected guard expression"
    (parseSurfaceProgram "x = case value { | item if -> item }.")

testRejectsMalformedOrPatternAlternative :: IO ()
testRejectsMalformedOrPatternAlternative =
  assertLeftDiagnosticContains
    "malformed or-pattern alternative"
    "expected case pattern"
    (parseSurfaceProgram "x = case value { | Just item | -> item }.")

testLowerCaseExpression :: IO ()
testLowerCaseExpression =
  assertRight
    "parse + lower case"
    (parseSurfaceProgram "x = case n { | 0 -> True | _ -> False }.")
    (\surfaceProgram -> assertEqual "lowered case AST" expectedProgram (lowerSurfaceExpr surfaceProgram))
  where
    expectedProgram =
      EBlock
        [ SLet
            "x"
            (SourceSpan 1 1)
            ( EPatternCase
                (EVar "n")
                [ CaseArm (PLiteral (LInt 0)) Nothing (ELit (LBool True)),
                  CaseArm PWildcard Nothing (ELit (LBool False))
                ]
            )
        ]
