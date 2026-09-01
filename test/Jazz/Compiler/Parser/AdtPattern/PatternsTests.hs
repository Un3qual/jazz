{-# LANGUAGE OverloadedStrings #-}

module Jazz.Compiler.Parser.AdtPattern.PatternsTests
  ( patternTests,
  )
where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import qualified Data.Text as Text
import Jazz.Compiler.AST
  ( CaseArm (..),
    DataConstructor (..),
    Expr (..),
    Literal (..),
    Pattern (..),
    Statement (..),
  )
import Jazz.Compiler.Diagnostics
  ( SourceSpan (..),
  )
import Jazz.Compiler.Name
  ( GeneratedNameKind (..),
    IdentifierLike (..),
    generatedName,
  )
import Jazz.Compiler.Parser
  ( parseSurfaceProgram,
  )
import Jazz.Compiler.Parser.AST
  ( SurfaceCaseArm (..),
    SurfaceDataConstructor (..),
    SurfaceExpr (..),
    SurfaceExprForm (..),
    SurfaceLambdaParameter (..),
    SurfaceLiteral (..),
    SurfacePattern (..),
    SurfacePatternForm (..),
    SurfaceStatement (..),
  )
import Jazz.Compiler.Parser.Lower
  ( lowerSurfaceExpr,
  )
import Jazz.Compiler.TypeRepresentation (SignatureType (..))
import Jazz.TestHarness
  ( NamedTest,
    assertEqual,
    assertRight,
    failTest,
  )

patternTests :: [NamedTest]
patternTests =
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
    ("keeps constructor if-expression pipe RHS before arm arrow", testKeepsConstructorIfExpressionPipeRhsBeforeArmArrow),
    ("keeps as-pattern constructor arguments atomic", testKeepsAsPatternConstructorArgumentsAtomic),
    ("parses as-pattern lambda parameters", testParsesAsPatternLambdaParameter),
    ("parses constructor pattern case arms", testParsesConstructorPatternCaseArms),
    ("parses multi-argument constructor patterns with nullary subpatterns", testParsesMultiArgumentConstructorPatternsWithNullarySubpatterns),
    ("parses nullary constructor subpatterns without losing the outer argument", testParsesNullaryConstructorSubpatterns),
    ("parses list pattern case arms", testParsesListPatternCaseArms),
    ("parses canonical data declaration and lowers constructor arities", testParsesCanonicalDataDeclarationAndLowersConstructorArities),
    ("parses nested case expression", testParsesNestedCaseExpression),
    ("parses unparenthesized if expression inside case arm body", testParsesIfExpressionInsideCaseArmBody),
    ("parses unparenthesized lambda expression inside case arm body", testParsesLambdaExpressionInsideCaseArmBody),
    ("parses mixed literal-wildcard later or-pattern arm after body", testParsesMixedLiteralWildcardLaterOrPatternArmAfterBody),
    ("keeps pipe operator inside body before constructor arm boundary", testKeepsPipeOperatorInsideBodyBeforeConstructorArmBoundary),
    ("keeps pipe operator inside body before literal arm boundary", testKeepsPipeOperatorInsideBodyBeforeLiteralArmBoundary),
    ("keeps bare list literal after pipe operator inside body", testKeepsBareListLiteralAfterPipeOperator),
    ("keeps bare constructor subject after pipe operator inside body", testKeepsBareConstructorValueAfterPipeOperator),
    ("keeps list application after pipe operator inside body", testKeepsListApplicationAfterPipeOperator),
    ("keeps constructor application after pipe operator inside body", testKeepsConstructorApplicationAfterPipeOperator),
    ("parses case scrutinee with block argument", testParsesCaseScrutineeWithBlockArgument),
    ("parses tuple pattern case arms", testParsesTuplePatternCaseArms),
    ("parses cons-like list patterns", testParsesConsLikeListPattern),
    ("parses cons-like list patterns inside constructor patterns", testParsesConsLikeListPatternInsideConstructorPattern),
    ("lowers parsed case nodes into core AST", testLowerCaseExpression)
  ]

assertSurfaceCasePatterns :: Text -> [SurfacePattern] -> SurfaceExpr -> IO ()
assertSurfaceCasePatterns label expected surfaceProgram =
  case surfaceExprForm surfaceProgram of
    SEBlock [SSLet _ _ caseExpression] ->
      case surfaceExprForm caseExpression of
        SECase _ arms ->
          assertEqual label expected [pattern' | SurfaceCaseArm pattern' _ _ <- arms]
        _ -> unexpected
    _ -> unexpected
  where
    unexpected = failTest (label <> ": expected a single top-level case binding, got " <> Text.pack (show surfaceProgram))

assertSurfaceLambdaPattern :: Text -> SurfacePattern -> SurfaceExpr -> IO ()
assertSurfaceLambdaPattern label expected surfaceProgram =
  case surfaceExprForm surfaceProgram of
    SEBlock [SSLet _ _ lambdaExpression] ->
      case surfaceExprForm lambdaExpression of
        SELambda (SurfaceLambdaPattern pattern' :| []) _ -> assertEqual label expected pattern'
        _ -> unexpected
    _ -> unexpected
  where
    unexpected = failTest (label <> ": expected a single-pattern lambda binding, got " <> Text.pack (show surfaceProgram))

assertSurfaceDataShape :: Text -> (Text, [Text], [(Text, Int)]) -> SurfaceExpr -> IO ()
assertSurfaceDataShape label expected surfaceProgram =
  case surfaceExprForm surfaceProgram of
    SEBlock [SSData _ name parameters constructors] ->
      assertEqual label expected (identifierText name, map identifierText parameters, map constructorShape constructors)
    _ -> failTest (label <> ": expected a single data declaration, got " <> Text.pack (show surfaceProgram))
  where
    constructorShape (SurfaceDataConstructor name fields) = (identifierText name, length fields)

p :: Int -> Int -> SurfacePatternForm -> SurfacePattern
p line column = SurfacePattern (SourceSpan line column)

testParsesBasicCaseExpression :: IO ()
testParsesBasicCaseExpression =
  assertRight
    "surface case AST"
    (parseSurfaceProgram "x = case n { | 0 -> True | _ -> False }.")
    ( \surfaceProgram -> do
        assertSurfaceCasePatterns
          "surface case patterns"
          [p 1 16 (SPLiteral (SLInt 0)), p 1 28 SPWildcard]
          surfaceProgram
        assertEqual
          "lowered case AST"
          ( EBlock
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
          )
          (lowerSurfaceExpr surfaceProgram)
    )

testParsesVariablePatternCaseArm :: IO ()
testParsesVariablePatternCaseArm =
  assertRight
    "variable pattern case arm"
    (parseSurfaceProgram "x = case subject { | item -> item }.")
    ( \surfaceProgram -> do
        assertSurfaceCasePatterns
          "variable surface pattern"
          [p 1 22 (SPVariable "item")]
          surfaceProgram
        assertEqual
          "lowered variable pattern case arm"
          ( EBlock
              [ SLet
                  "x"
                  (SourceSpan 1 1)
                  (EPatternCase (EVar "subject") [CaseArm (PVariable "item") Nothing (EVar "item")])
              ]
          )
          (lowerSurfaceExpr surfaceProgram)
    )

testParsesAsPatternCaseArm :: IO ()
testParsesAsPatternCaseArm =
  assertRight
    "as-pattern case arm parse + lower"
    (parseSurfaceProgram "x = case subject { | whole @ Just item -> whole | _ -> subject }.")
    ( \surfaceProgram -> do
        assertSurfaceCasePatterns
          "as-pattern surface AST"
          [ p 1 22 (SPAs "whole" (p 1 30 (SPConstructor "Just" [p 1 35 (SPVariable "item")]))),
            p 1 51 SPWildcard
          ]
          surfaceProgram
        assertEqual "as-pattern lowered AST" expectedLoweredProgram (lowerSurfaceExpr surfaceProgram)
    )
  where
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
        assertSurfaceCasePatterns
          "guarded case arm surface AST"
          [ p 1 22 (SPConstructor "Just" [p 1 27 (SPVariable "item")]),
            p 1 54 SPWildcard
          ]
          surfaceProgram
        assertEqual "guarded case arm lowered AST" expectedLoweredProgram (lowerSurfaceExpr surfaceProgram)
    )
  where
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
        assertSurfaceCasePatterns
          "or-pattern surface AST"
          [ p
              1
              22
              ( SPOr
                  [ p 1 22 (SPConstructor "Just" [p 1 27 (SPVariable "item")]),
                    p 1 34 (SPConstructor "Also" [p 1 39 (SPVariable "item")])
                  ]
              ),
            p 1 66 (SPConstructor "Nothing" [])
          ]
          surfaceProgram
        assertEqual "or-pattern lowered AST" expectedLoweredProgram (lowerSurfaceExpr surfaceProgram)
    )
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
        assertSurfaceCasePatterns
          "all-literal pipe body surface AST"
          [p 1 16 SPWildcard, p 1 29 (SPLiteral (SLInt 2))]
          surfaceProgram
        assertEqual "all-literal pipe body lowered AST" expectedLoweredProgram (lowerSurfaceExpr surfaceProgram)
    )
  where
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
        assertSurfaceCasePatterns
          "wildcard-led later or-pattern surface AST"
          [ p 1 16 (SPLiteral (SLInt 0)),
            p 1 25 (SPOr [p 1 25 SPWildcard, p 1 29 (SPLiteral (SLInt 2))])
          ]
          surfaceProgram
        assertEqual "wildcard-led later or-pattern lowered AST" expectedLoweredProgram (lowerSurfaceExpr surfaceProgram)
    )
  where
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
        assertSurfaceCasePatterns
          "variable-led later or-pattern surface AST"
          [ p 1 16 (SPLiteral (SLInt 0)),
            p 1 25 (SPOr [p 1 25 (SPVariable "item"), p 1 32 (SPVariable "other")])
          ]
          surfaceProgram
        assertEqual "variable-led later or-pattern lowered AST" expectedLoweredProgram (lowerSurfaceExpr surfaceProgram)
    )
  where
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
        assertSurfaceCasePatterns
          "variable-led mixed later or-pattern surface AST"
          [ p 1 16 (SPLiteral (SLInt 0)),
            p
              1
              25
              ( SPOr
                  [ p 1 25 (SPVariable "item"),
                    p 1 32 (SPAs "item" (p 1 39 SPWildcard))
                  ]
              )
          ]
          surfaceProgram
        assertEqual "variable-led mixed later or-pattern lowered AST" expectedLoweredProgram (lowerSurfaceExpr surfaceProgram)
    )
  where
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
    ( \surfaceProgram -> do
        assertSurfaceCasePatterns
          "guarded pipe expression surface AST"
          [p 1 22 (SPLiteral (SLInt 0)), p 1 31 (SPVariable "item")]
          surfaceProgram
        assertEqual "guarded pipe expression lowered AST" expectedLoweredProgram (lowerSurfaceExpr surfaceProgram)
    )
  where
    expectedLoweredProgram =
      EBlock
        [ SLet
            "x"
            (SourceSpan 1 1)
            ( EPatternCase
                (EVar "subject")
                [ CaseArm
                    (PLiteral (LInt 0))
                    Nothing
                    (ELit (LInt 0)),
                  CaseArm
                    (PVariable "item")
                    (Just (EBinary "|" (EVar "left") (EVar "right")))
                    (ELit (LInt 1))
                ]
            )
        ]

testParsesGuardedCaseArmWithDefinitePipeRhsGuards :: IO ()
testParsesGuardedCaseArmWithDefinitePipeRhsGuards =
  assertRight
    "guarded pipe expression with literal and constructor-shaped RHS"
    (parseSurfaceProgram "x = case subject { | item if left | True -> 1 | other if left | Nothing -> 2 }.")
    ( \surfaceProgram -> do
        assertSurfaceCasePatterns
          "guarded definite pipe RHS surface AST"
          [p 1 22 (SPVariable "item"), p 1 49 (SPVariable "other")]
          surfaceProgram
        assertEqual "guarded definite pipe RHS lowered AST" expectedLoweredProgram (lowerSurfaceExpr surfaceProgram)
    )
  where
    expectedLoweredProgram =
      EBlock
        [ SLet
            "x"
            (SourceSpan 1 1)
            ( EPatternCase
                (EVar "subject")
                [ CaseArm
                    (PVariable "item")
                    (Just (EBinary "|" (EVar "left") (ELit (LBool True))))
                    (ELit (LInt 1)),
                  CaseArm
                    (PVariable "other")
                    (Just (EBinary "|" (EVar "left") (EVar "Nothing")))
                    (ELit (LInt 2))
                ]
            )
        ]

testKeepsConstructorIfExpressionPipeRhsBeforeArmArrow :: IO ()
testKeepsConstructorIfExpressionPipeRhsBeforeArmArrow =
  assertRight
    "constructor if-expression pipe RHS before arm arrow"
    (parseSurfaceProgram "x = case m { | item if item == 0 | Just if ok then 1 else 2 -> item | _ -> m }.")
    ( \surfaceProgram -> do
        assertSurfaceCasePatterns
          "constructor if-expression pipe RHS surface AST"
          [p 1 16 (SPVariable "item"), p 1 71 SPWildcard]
          surfaceProgram
        assertEqual "constructor if-expression pipe RHS lowered AST" expectedLoweredProgram (lowerSurfaceExpr surfaceProgram)
    )
  where
    expectedLoweredProgram =
      EBlock
        [ SLet
            "x"
            (SourceSpan 1 1)
            ( EPatternCase
                (EVar "m")
                [ CaseArm
                    (PVariable "item")
                    ( Just
                        ( EBinary
                            "=="
                            (EVar "item")
                            ( EBinary
                                "|"
                                (ELit (LInt 0))
                                (EApply (EVar "Just") (EIf (EVar "ok") (ELit (LInt 1)) (ELit (LInt 2))))
                            )
                        )
                    )
                    (EVar "item"),
                  CaseArm
                    PWildcard
                    Nothing
                    (EVar "m")
                ]
            )
        ]

testKeepsAsPatternConstructorArgumentsAtomic :: IO ()
testKeepsAsPatternConstructorArgumentsAtomic =
  assertRight
    "as-pattern constructor argument parse + lower"
    (parseSurfaceProgram "x = case subject { | Pair whole @ Nothing item -> item | _ -> 0 }.")
    ( \surfaceProgram -> do
        assertSurfaceCasePatterns
          "as-pattern constructor argument surface AST"
          [ p
              1
              22
              ( SPConstructor
                  "Pair"
                  [ p 1 27 (SPAs "whole" (p 1 35 (SPConstructor "Nothing" []))),
                    p 1 43 (SPVariable "item")
                  ]
              ),
            p 1 58 SPWildcard
          ]
          surfaceProgram
        assertEqual "as-pattern constructor argument lowered AST" expectedLoweredProgram (lowerSurfaceExpr surfaceProgram)
    )
  where
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
  assertRight
    "as-pattern lambda parameter"
    (parseSurfaceProgram "f = \\(whole @ [head | tail]) -> head.")
    ( \surfaceProgram -> do
        assertSurfaceLambdaPattern
          "as-pattern lambda surface AST"
          ( p
              1
              7
              ( SPAs
                  "whole"
                  (p 1 15 (SPConsList (p 1 16 (SPVariable "head")) (p 1 23 (SPVariable "tail"))))
              )
          )
          surfaceProgram
        assertEqual "lowered as-pattern lambda" expectedProgram (lowerSurfaceExpr surfaceProgram)
    )
  where
    generatedParameter = generatedName (LambdaPatternArgument 1)
    expectedProgram =
      EBlock
        [ SLet
            "f"
            (SourceSpan 1 1)
            ( ELambda
                generatedParameter
                ( EPatternCase
                    (EVar generatedParameter)
                    [ CaseArm
                        (PAs "whole" (PConsList (PVariable "head") (PVariable "tail")))
                        Nothing
                        (EVar "head")
                    ]
                )
            )
        ]

testParsesConstructorPatternCaseArms :: IO ()
testParsesConstructorPatternCaseArms =
  assertRight
    "constructor pattern parse + lower"
    (parseSurfaceProgram "x = case subject { | Just item -> item | Nothing -> 0 }.")
    ( \surfaceProgram -> do
        assertSurfaceCasePatterns
          "constructor pattern surface AST"
          [ p 1 22 (SPConstructor "Just" [p 1 27 (SPVariable "item")]),
            p 1 42 (SPConstructor "Nothing" [])
          ]
          surfaceProgram
        assertEqual "constructor pattern lowered AST" expectedLoweredProgram (lowerSurfaceExpr surfaceProgram)
    )
  where
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
        assertSurfaceCasePatterns
          "multi-argument constructor pattern surface AST"
          [ p
              1
              22
              ( SPConstructor
                  "Pair"
                  [p 1 27 (SPConstructor "Nothing" []), p 1 35 (SPVariable "item")]
              ),
            p 1 50 SPWildcard
          ]
          surfaceProgram
        assertEqual "multi-argument constructor pattern lowered AST" expectedLoweredProgram (lowerSurfaceExpr surfaceProgram)
    )
  where
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
        assertSurfaceCasePatterns
          "nullary constructor subpattern surface AST"
          [ p 1 22 (SPConstructor "Just" [p 1 27 (SPConstructor "Nothing" [])]),
            p 1 42 SPWildcard
          ]
          surfaceProgram
        assertEqual "nullary constructor subpattern lowered AST" expectedLoweredProgram (lowerSurfaceExpr surfaceProgram)
    )
  where
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
        assertSurfaceCasePatterns
          "list pattern surface AST"
          [ p 1 21 (SPList [p 1 22 (SPVariable "head"), p 1 28 SPWildcard]),
            p 1 41 (SPList [])
          ]
          surfaceProgram
        assertEqual "list pattern lowered AST" expectedLoweredProgram (lowerSurfaceExpr surfaceProgram)
    )
  where
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
        assertSurfaceDataShape "data declaration surface AST" ("Maybe", ["a"], [("Just", 1), ("Nothing", 0)]) surfaceProgram
        assertEqual "data declaration lowered AST" expectedLoweredProgram (lowerSurfaceExpr surfaceProgram)
    )
  where
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
        assertSurfaceCasePatterns
          "mixed literal-wildcard later or-pattern surface AST"
          [ p 1 16 (SPLiteral (SLInt 0)),
            p 1 25 (SPOr [p 1 25 (SPLiteral (SLInt 2)), p 1 29 SPWildcard])
          ]
          surfaceProgram
        assertEqual "mixed literal-wildcard later or-pattern lowered AST" expectedLoweredProgram (lowerSurfaceExpr surfaceProgram)
    )
  where
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
        assertSurfaceCasePatterns
          "cons-like list pattern surface AST"
          [ p 1 21 (SPConsList (p 1 22 (SPVariable "head")) (p 1 29 (SPVariable "tail"))),
            p 1 45 SPWildcard
          ]
          surfaceProgram
        assertEqual "cons-like list pattern lowered AST" expectedLoweredProgram (lowerSurfaceExpr surfaceProgram)
    )
  where
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
        assertSurfaceCasePatterns
          "cons-like list constructor surface AST"
          [ p
              1
              22
              ( SPConstructor
                  "Just"
                  [p 1 27 (SPConsList (p 1 28 (SPVariable "head")) (p 1 35 (SPVariable "tail")))]
              ),
            p 1 51 SPWildcard
          ]
          surfaceProgram
        assertEqual "cons-like list constructor lowered AST" expectedLoweredProgram (lowerSurfaceExpr surfaceProgram)
    )
  where
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
