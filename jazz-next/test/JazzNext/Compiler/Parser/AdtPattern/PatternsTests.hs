{-# LANGUAGE OverloadedStrings #-}

module JazzNext.Compiler.Parser.AdtPattern.PatternsTests
  ( patternTests
  ) where

import Data.List.NonEmpty (NonEmpty (..))
import JazzNext.Compiler.AST
  ( CaseArm (..),
    DataConstructor (..),
    Expr (..),
    Literal (..),
    Pattern (..),
    SignatureType (..),
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
    SurfaceDataConstructor (..),
    SurfaceExpr (..),
    SurfaceLambdaParameter (..),
    SurfaceLiteral (..),
    SurfacePattern (..),
    SurfaceSignatureType (..),
    SurfaceStatement (..)
  )
import JazzNext.Compiler.Parser.Lower
  ( lowerSurfaceExpr
  )
import JazzNext.TestHarness
  ( NamedTest,
    assertEqual,
    assertRight
  )

patternTests :: [NamedTest]
patternTests =
  [ ("parses basic case expression with literal and wildcard arms", testParsesBasicCaseExpression)
    , ("parses variable pattern case arm", testParsesVariablePatternCaseArm)
    , ("parses as-pattern case arms", testParsesAsPatternCaseArm)
    , ("parses guarded case arms", testParsesGuardedCaseArm)
    , ("parses case-arm or-patterns and lowers them", testParsesCaseArmOrPatterns)
    , ("keeps all-literal pipe body before literal arm boundary", testKeepsAllLiteralPipeBodyBeforeLiteralArmBoundary)
    , ("parses wildcard-led later or-pattern arm after body", testParsesWildcardLedLaterOrPatternArmAfterBody)
    , ("parses variable-led later or-pattern arm after body", testParsesVariableLedLaterOrPatternArmAfterBody)
    , ("parses variable-led mixed later or-pattern arm after body", testParsesVariableLedMixedLaterOrPatternArmAfterBody)
    , ("keeps pipe operator in or-pattern arm body", testKeepsPipeOperatorInOrPatternArmBody)
    , ("parses guarded case arm with pipe expression guard after previous arm", testParsesGuardedCaseArmWithPipeExpressionAfterPreviousArm)
    , ("parses guarded case arms with definite pipe RHS guards", testParsesGuardedCaseArmWithDefinitePipeRhsGuards)
    , ("keeps constructor if-expression pipe RHS before arm arrow", testKeepsConstructorIfExpressionPipeRhsBeforeArmArrow)
    , ("keeps as-pattern constructor arguments atomic", testKeepsAsPatternConstructorArgumentsAtomic)
    , ("parses as-pattern lambda parameters", testParsesAsPatternLambdaParameter)
    , ("parses constructor pattern case arms", testParsesConstructorPatternCaseArms)
    , ("parses multi-argument constructor patterns with nullary subpatterns", testParsesMultiArgumentConstructorPatternsWithNullarySubpatterns)
    , ("parses nullary constructor subpatterns without losing the outer argument", testParsesNullaryConstructorSubpatterns)
    , ("parses list pattern case arms", testParsesListPatternCaseArms)
    , ("parses canonical data declaration and lowers constructor arities", testParsesCanonicalDataDeclarationAndLowersConstructorArities)
    , ("parses nested case expression", testParsesNestedCaseExpression)
    , ("parses unparenthesized if expression inside case arm body", testParsesIfExpressionInsideCaseArmBody)
    , ("parses unparenthesized lambda expression inside case arm body", testParsesLambdaExpressionInsideCaseArmBody)
    , ("parses mixed literal-wildcard later or-pattern arm after body", testParsesMixedLiteralWildcardLaterOrPatternArmAfterBody)
    , ("keeps pipe operator inside body before constructor arm boundary", testKeepsPipeOperatorInsideBodyBeforeConstructorArmBoundary)
    , ("keeps pipe operator inside body before literal arm boundary", testKeepsPipeOperatorInsideBodyBeforeLiteralArmBoundary)
    , ("keeps bare list literal after pipe operator inside body", testKeepsBareListLiteralAfterPipeOperator)
    , ("keeps bare constructor subject after pipe operator inside body", testKeepsBareConstructorValueAfterPipeOperator)
    , ("keeps list application after pipe operator inside body", testKeepsListApplicationAfterPipeOperator)
    , ("keeps constructor application after pipe operator inside body", testKeepsConstructorApplicationAfterPipeOperator)
    , ("parses case scrutinee with block argument", testParsesCaseScrutineeWithBlockArgument)
    , ("parses tuple pattern case arms", testParsesTuplePatternCaseArms)
    , ("parses cons-like list patterns", testParsesConsLikeListPattern)
    , ("parses cons-like list patterns inside constructor patterns", testParsesConsLikeListPatternInsideConstructorPattern)
    , ("lowers parsed case nodes into core AST", testLowerCaseExpression)
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
                (SECase (SEVar "subject") [SurfaceCaseArm (SPVariable "item") Nothing (SEVar "item")])
            ]
        )
    )
    (parseSurfaceProgram "x = case subject { | item -> item }.")

testParsesAsPatternCaseArm :: IO ()
testParsesAsPatternCaseArm =
  assertRight
    "as-pattern case arm parse + lower"
    (parseSurfaceProgram "x = case subject { | whole @ Just item -> whole | _ -> subject }.")
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
                (SEVar "subject")
                [ SurfaceCaseArm
                    (SPAs "whole" (SPConstructor "Just" [SPVariable "item"]))
                    Nothing
                    (SEVar "whole"),
                  SurfaceCaseArm SPWildcard Nothing (SEVar "subject")
                ]
            )
        ]
    expectedLoweredProgram =
      EBlock
        [ SLet
            "x"
            (SourceSpan 1 1)
            ( EPatternCase
                (EVar "subject")
                [ CaseArm
                    (PAs "whole" (PConstructor "Just" [PVariable "item"]))
                    Nothing
                    (EVar "whole"),
                  CaseArm PWildcard Nothing (EVar "subject")
                ]
            )
        ]

testParsesGuardedCaseArm :: IO ()
testParsesGuardedCaseArm =
  assertRight
    "guarded case arm parse + lower"
    (parseSurfaceProgram "x = case subject { | Just item if item > 0 -> item | _ -> 0 }.")
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
                (SEVar "subject")
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
                (EVar "subject")
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
    (parseSurfaceProgram "x = case subject { | Just item | Also item if item > 0 -> item | Nothing -> 0 }.")
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
                (SEVar "subject")
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
                (EVar "subject")
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
    (parseSurfaceProgram "x = case subject { | Just item | Also item -> item | f | Nothing -> 0 }.")
    (\surfaceProgram -> assertEqual "or-pattern pipe body lowered AST" expectedLoweredProgram (lowerSurfaceExpr surfaceProgram))
  where
    expectedLoweredProgram =
      EBlock
        [ SLet
            "x"
            (SourceSpan 1 1)
            ( EPatternCase
                (EVar "subject")
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
    (parseSurfaceProgram "x = case subject { | 0 -> 0 | item if left | right -> 1 }.")
    (\surfaceProgram -> assertEqual "guarded pipe expression surface AST" expectedSurfaceProgram surfaceProgram)
  where
    expectedSurfaceProgram =
      SEBlock
        [ SSLet
            "x"
            (SourceSpan 1 1)
            ( SECase
                (SEVar "subject")
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
    (parseSurfaceProgram "x = case subject { | item if left | True -> 1 | other if left | Nothing -> 2 }.")
    (\surfaceProgram -> assertEqual "guarded definite pipe RHS surface AST" expectedSurfaceProgram surfaceProgram)
  where
    expectedSurfaceProgram =
      SEBlock
        [ SSLet
            "x"
            (SourceSpan 1 1)
            ( SECase
                (SEVar "subject")
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

testKeepsConstructorIfExpressionPipeRhsBeforeArmArrow :: IO ()
testKeepsConstructorIfExpressionPipeRhsBeforeArmArrow =
  assertRight
    "constructor if-expression pipe RHS before arm arrow"
    (parseSurfaceProgram "x = case m { | item if item == 0 | Just if ok then 1 else 2 -> item | _ -> m }.")
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

testKeepsAsPatternConstructorArgumentsAtomic :: IO ()
testKeepsAsPatternConstructorArgumentsAtomic =
  assertRight
    "as-pattern constructor argument parse + lower"
    (parseSurfaceProgram "x = case subject { | Pair whole @ Nothing item -> item | _ -> 0 }.")
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
                (SEVar "subject")
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
                (EVar "subject")
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
    (parseSurfaceProgram "x = case subject { | Just item -> item | Nothing -> 0 }.")
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
                (SEVar "subject")
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
                (EVar "subject")
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
    (parseSurfaceProgram "x = case subject { | Pair Nothing item -> item | _ -> 0 }.")
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
                (SEVar "subject")
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
                (EVar "subject")
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
    (parseSurfaceProgram "x = case subject { | Just Nothing -> 1 | _ -> 0 }.")
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
                (SEVar "subject")
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
                (EVar "subject")
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
    (parseSurfaceProgram "data Maybe a = Just a | Nothing.")
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
            ["a"]
            [ SurfaceDataConstructor "Just" [SurfaceTypeVariable "a"],
              SurfaceDataConstructor "Nothing" []
            ]
        ]
    expectedLoweredProgram =
      EBlock
        [ SData
            (SourceSpan 1 1)
            "Maybe"
            ["a"]
            [ DataConstructor "Just" [TypeVariable "a"],
              DataConstructor "Nothing" []
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
    (parseSurfaceProgram "x = case n { | 0 -> if True then 1 else 2 | _ -> 3 }.")
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
    (parseSurfaceProgram "x = case subject { | Just item -> 1 | 2 | Nothing -> 3 }.")
    (\surfaceProgram -> assertEqual "constructor arm boundary lowered AST" expectedProgram (lowerSurfaceExpr surfaceProgram))
  where
    expectedProgram =
      EBlock
        [ SLet
            "x"
            (SourceSpan 1 1)
            ( EPatternCase
                (EVar "subject")
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
    (parseSurfaceProgram "x = case subject { | _ -> 1 | 2 | 3 -> 4 }.")
    (\surfaceProgram -> assertEqual "literal arm boundary lowered AST" expectedProgram (lowerSurfaceExpr surfaceProgram))
  where
    expectedProgram =
      EBlock
        [ SLet
            "x"
            (SourceSpan 1 1)
            ( EPatternCase
                (EVar "subject")
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
    (parseSurfaceProgram "x = case subject { | _ -> 1 | [2] }.")
    (\surfaceProgram -> assertEqual "list literal in arm body lowered AST" expectedProgram (lowerSurfaceExpr surfaceProgram))
  where
    expectedProgram =
      EBlock
        [ SLet
            "x"
            (SourceSpan 1 1)
            ( EPatternCase
                (EVar "subject")
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
    "bare constructor subject stays in case arm body"
    (parseSurfaceProgram "x = case subject { | _ -> 1 | Nothing }.")
    (\surfaceProgram -> assertEqual "constructor subject in arm body lowered AST" expectedProgram (lowerSurfaceExpr surfaceProgram))
  where
    expectedProgram =
      EBlock
        [ SLet
            "x"
            (SourceSpan 1 1)
            ( EPatternCase
                (EVar "subject")
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
    (parseSurfaceProgram "x = case subject { | _ -> 1 | Just a b }.")
    (\surfaceProgram -> assertEqual "constructor application in arm body lowered AST" expectedProgram (lowerSurfaceExpr surfaceProgram))
  where
    expectedProgram =
      EBlock
        [ SLet
            "x"
            (SourceSpan 1 1)
            ( EPatternCase
                (EVar "subject")
                [ CaseArm
                    PWildcard
                    Nothing
                    (EBinary "|" (ELit (LInt 1)) (EApply (EApply (EVar "Just") (EVar "a")) (EVar "b")))
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
    (parseSurfaceProgram "x = case subject { | Just [head | tail] -> head | _ -> 0 }.")
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
                (SEVar "subject")
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
                (EVar "subject")
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
