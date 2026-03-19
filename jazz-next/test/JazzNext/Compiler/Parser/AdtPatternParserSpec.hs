{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

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
  ( SurfaceCaseArm (..),
    SurfaceExpr (..),
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
    ("parses constructor pattern case arms", testParsesConstructorPatternCaseArms),
    ("parses list pattern case arms", testParsesListPatternCaseArms),
    ("parses nested case expression", testParsesNestedCaseExpression),
    ("parses unparenthesized if expression inside case arm body", testParsesIfExpressionInsideCaseArmBody),
    ("parses unparenthesized lambda expression inside case arm body", testParsesLambdaExpressionInsideCaseArmBody),
    ("parses pipe operator inside case arm body", testParsesPipeOperatorInsideCaseArmBody),
    ("keeps pipe operator inside body before constructor arm boundary", testKeepsPipeOperatorInsideBodyBeforeConstructorArmBoundary),
    ("parses case scrutinee with block argument", testParsesCaseScrutineeWithBlockArgument),
    ("reports missing case body for block-valued scrutinee", testReportsMissingCaseBodyForBlockScrutinee),
    ("reports missing arm arrow for block-valued scrutinee", testReportsMissingArmArrowForBlockScrutinee),
    ("reports invalid case scrutinee syntax before body diagnostics", testReportsInvalidCaseScrutineeSyntax),
    ("rejects case expression without leading pipe", testRejectsCaseExpressionWithoutPipe),
    ("rejects case expression without arm arrow", testRejectsCaseExpressionWithoutArrow),
    ("rejects missing arrow on later case arm", testRejectsMissingArrowOnLaterCaseArm),
    ("rejects malformed list patterns", testRejectsMalformedListPattern),
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
                    [ SurfaceCaseArm (SPLiteral (SLInt 0)) (SELit (SLBool True)),
                      SurfaceCaseArm SPWildcard (SELit (SLBool False))
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
                (SECase (SEVar "value") [SurfaceCaseArm (SPVariable "item") (SEVar "item")])
            ]
        )
    )
    (parseSurfaceProgram "x = case value { | item -> item }.")

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
                    (SEVar "item"),
                  SurfaceCaseArm
                    (SPConstructor "Nothing" [])
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
                    (EVar "item"),
                  CaseArm
                    (PConstructor "Nothing" [])
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
                    (SEVar "head"),
                  SurfaceCaseArm
                    (SPList [])
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
                    (EVar "head"),
                  CaseArm
                    (PList [])
                    (ELit (LInt 0))
                ]
            )
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
                    ( EPatternCase
                        (EVar "y")
                        [ CaseArm (PLiteral (LInt 1)) (ELit (LBool True)),
                          CaseArm PWildcard (ELit (LBool False))
                        ]
                    ),
                  CaseArm PWildcard (ELit (LBool False))
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
                    (EIf (ELit (LBool True)) (ELit (LInt 1)) (ELit (LInt 2))),
                  CaseArm PWildcard (ELit (LInt 3))
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
                    (ELambda "y" (EVar "y")),
                  CaseArm PWildcard (ELit (LInt 3))
                ]
            )
        ]

testParsesPipeOperatorInsideCaseArmBody :: IO ()
testParsesPipeOperatorInsideCaseArmBody =
  assertRight
    "case arm body keeps infix pipe operator"
    (parseSurfaceProgram "x = case n { | 0 -> 1 | 2 | _ -> 3 }.")
    (\surfaceProgram -> assertEqual "pipe operator lowered case AST" expectedProgram (lowerSurfaceExpr surfaceProgram))
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
                    (EBinary "|" (ELit (LInt 1)) (ELit (LInt 2))),
                  CaseArm PWildcard (ELit (LInt 3))
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
                    (EBinary "|" (ELit (LInt 1)) (ELit (LInt 2))),
                  CaseArm
                    (PConstructor "Nothing" [])
                    (ELit (LInt 3))
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
                [ CaseArm (PLiteral (LInt 1)) (ELit (LBool True)),
                  CaseArm PWildcard (ELit (LBool False))
                ]
            )
        ]

testReportsMissingCaseBodyForBlockScrutinee :: IO ()
testReportsMissingCaseBodyForBlockScrutinee =
  assertLeftDiagnosticContains
    "block scrutinee missing case body"
    "expected '{' before end of input after 'case'"
    (parseSurfaceProgram "x = case f { y = 1. y. }.")

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

testRejectsMissingArrowOnLaterCaseArm :: IO ()
testRejectsMissingArrowOnLaterCaseArm =
  assertLeftDiagnosticContains
    "missing later case-arm arrow"
    "expected '->'"
    (parseSurfaceProgram "x = case n { | 0 -> 1 | _ False }.")

testRejectsMalformedListPattern :: IO ()
testRejectsMalformedListPattern =
  assertLeftDiagnosticContains
    "malformed list pattern"
    "expected ',' or ']'"
    (parseSurfaceProgram "x = case values { | [head tail] -> head }.")

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
                [ CaseArm (PLiteral (LInt 0)) (ELit (LBool True)),
                  CaseArm PWildcard (ELit (LBool False))
                ]
            )
        ]
