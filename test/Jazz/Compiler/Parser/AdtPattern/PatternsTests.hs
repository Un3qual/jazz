{-# LANGUAGE OverloadedStrings #-}

module Jazz.Compiler.Parser.AdtPattern.PatternsTests
  ( patternTests,
  )
where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import qualified Data.Text as Text
import Jazz.Compiler.AST
  ( Literal (..),
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
import Jazz.TestCore
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
        assertLoweredCoreEqual
          "lowered case AST"
          ( loweredBlock
              [ loweredLet
                  "x"
                  (SourceSpan 1 1)
                  ( loweredPatternCase
                      (loweredVariable "n")
                      [ loweredCaseArm (loweredLiteralPattern (LInt 0)) Nothing (loweredLiteral (LBool True)),
                        loweredCaseArm loweredWildcardPattern Nothing (loweredLiteral (LBool False))
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
        assertLoweredCoreEqual
          "lowered variable pattern case arm"
          ( loweredBlock
              [ loweredLet
                  "x"
                  (SourceSpan 1 1)
                  (loweredPatternCase (loweredVariable "subject") [loweredCaseArm (loweredVariablePattern "item") Nothing (loweredVariable "item")])
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
        assertLoweredCoreEqual "as-pattern lowered AST" expectedLoweredProgram (lowerSurfaceExpr surfaceProgram)
    )
  where
    expectedLoweredProgram =
      loweredBlock
        [ loweredLet
            "x"
            (SourceSpan 1 1)
            ( loweredPatternCase
                (loweredVariable "subject")
                [ loweredCaseArm
                    (loweredAsPattern "whole" (loweredConstructorPattern "Just" [loweredVariablePattern "item"]))
                    Nothing
                    (loweredVariable "whole"),
                  loweredCaseArm loweredWildcardPattern Nothing (loweredVariable "subject")
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
        assertLoweredCoreEqual "guarded case arm lowered AST" expectedLoweredProgram (lowerSurfaceExpr surfaceProgram)
    )
  where
    expectedLoweredProgram =
      loweredBlock
        [ loweredLet
            "x"
            (SourceSpan 1 1)
            ( loweredPatternCase
                (loweredVariable "subject")
                [ loweredCaseArm
                    (loweredConstructorPattern "Just" [loweredVariablePattern "item"])
                    (Just (loweredBinary ">" (loweredVariable "item") (loweredLiteral (LInt 0))))
                    (loweredVariable "item"),
                  loweredCaseArm
                    loweredWildcardPattern
                    Nothing
                    (loweredLiteral (LInt 0))
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
        assertLoweredCoreEqual "or-pattern lowered AST" expectedLoweredProgram (lowerSurfaceExpr surfaceProgram)
    )
  where
    expectedLoweredProgram =
      loweredBlock
        [ loweredLet
            "x"
            (SourceSpan 1 1)
            ( loweredPatternCase
                (loweredVariable "subject")
                [ loweredCaseArm
                    ( loweredOrPattern
                        [ loweredConstructorPattern "Just" [loweredVariablePattern "item"],
                          loweredConstructorPattern "Also" [loweredVariablePattern "item"]
                        ]
                    )
                    (Just (loweredBinary ">" (loweredVariable "item") (loweredLiteral (LInt 0))))
                    (loweredVariable "item"),
                  loweredCaseArm
                    (loweredConstructorPattern "Nothing" [])
                    Nothing
                    (loweredLiteral (LInt 0))
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
        assertLoweredCoreEqual "all-literal pipe body lowered AST" expectedLoweredProgram (lowerSurfaceExpr surfaceProgram)
    )
  where
    expectedLoweredProgram =
      loweredBlock
        [ loweredLet
            "x"
            (SourceSpan 1 1)
            ( loweredPatternCase
                (loweredVariable "n")
                [ loweredCaseArm
                    loweredWildcardPattern
                    Nothing
                    (loweredBinary "|" (loweredLiteral (LInt 0)) (loweredLiteral (LInt 1))),
                  loweredCaseArm
                    (loweredLiteralPattern (LInt 2))
                    Nothing
                    (loweredLiteral (LInt 1))
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
        assertLoweredCoreEqual "wildcard-led later or-pattern lowered AST" expectedLoweredProgram (lowerSurfaceExpr surfaceProgram)
    )
  where
    expectedLoweredProgram =
      loweredBlock
        [ loweredLet
            "x"
            (SourceSpan 1 1)
            ( loweredPatternCase
                (loweredVariable "n")
                [ loweredCaseArm
                    (loweredLiteralPattern (LInt 0))
                    Nothing
                    (loweredLiteral (LInt 0)),
                  loweredCaseArm
                    (loweredOrPattern [loweredWildcardPattern, loweredLiteralPattern (LInt 2)])
                    Nothing
                    (loweredLiteral (LInt 1))
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
        assertLoweredCoreEqual "variable-led later or-pattern lowered AST" expectedLoweredProgram (lowerSurfaceExpr surfaceProgram)
    )
  where
    expectedLoweredProgram =
      loweredBlock
        [ loweredLet
            "x"
            (SourceSpan 1 1)
            ( loweredPatternCase
                (loweredVariable "n")
                [ loweredCaseArm
                    (loweredLiteralPattern (LInt 0))
                    Nothing
                    (loweredLiteral (LInt 0)),
                  loweredCaseArm
                    (loweredOrPattern [loweredVariablePattern "item", loweredVariablePattern "other"])
                    Nothing
                    (loweredVariable "item")
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
        assertLoweredCoreEqual "variable-led mixed later or-pattern lowered AST" expectedLoweredProgram (lowerSurfaceExpr surfaceProgram)
    )
  where
    expectedLoweredProgram =
      loweredBlock
        [ loweredLet
            "x"
            (SourceSpan 1 1)
            ( loweredPatternCase
                (loweredVariable "n")
                [ loweredCaseArm
                    (loweredLiteralPattern (LInt 0))
                    Nothing
                    (loweredLiteral (LInt 0)),
                  loweredCaseArm
                    (loweredOrPattern [loweredVariablePattern "item", loweredAsPattern "item" loweredWildcardPattern])
                    Nothing
                    (loweredVariable "item")
                ]
            )
        ]

testKeepsPipeOperatorInOrPatternArmBody :: IO ()
testKeepsPipeOperatorInOrPatternArmBody =
  assertRight
    "or-pattern arm body keeps infix pipe operator"
    (parseSurfaceProgram "x = case subject { | Just item | Also item -> item | f | Nothing -> 0 }.")
    (\surfaceProgram -> assertLoweredCoreEqual "or-pattern pipe body lowered AST" expectedLoweredProgram (lowerSurfaceExpr surfaceProgram))
  where
    expectedLoweredProgram =
      loweredBlock
        [ loweredLet
            "x"
            (SourceSpan 1 1)
            ( loweredPatternCase
                (loweredVariable "subject")
                [ loweredCaseArm
                    ( loweredOrPattern
                        [ loweredConstructorPattern "Just" [loweredVariablePattern "item"],
                          loweredConstructorPattern "Also" [loweredVariablePattern "item"]
                        ]
                    )
                    Nothing
                    (loweredBinary "|" (loweredVariable "item") (loweredVariable "f")),
                  loweredCaseArm
                    (loweredConstructorPattern "Nothing" [])
                    Nothing
                    (loweredLiteral (LInt 0))
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
        assertLoweredCoreEqual "guarded pipe expression lowered AST" expectedLoweredProgram (lowerSurfaceExpr surfaceProgram)
    )
  where
    expectedLoweredProgram =
      loweredBlock
        [ loweredLet
            "x"
            (SourceSpan 1 1)
            ( loweredPatternCase
                (loweredVariable "subject")
                [ loweredCaseArm
                    (loweredLiteralPattern (LInt 0))
                    Nothing
                    (loweredLiteral (LInt 0)),
                  loweredCaseArm
                    (loweredVariablePattern "item")
                    (Just (loweredBinary "|" (loweredVariable "left") (loweredVariable "right")))
                    (loweredLiteral (LInt 1))
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
        assertLoweredCoreEqual "guarded definite pipe RHS lowered AST" expectedLoweredProgram (lowerSurfaceExpr surfaceProgram)
    )
  where
    expectedLoweredProgram =
      loweredBlock
        [ loweredLet
            "x"
            (SourceSpan 1 1)
            ( loweredPatternCase
                (loweredVariable "subject")
                [ loweredCaseArm
                    (loweredVariablePattern "item")
                    (Just (loweredBinary "|" (loweredVariable "left") (loweredLiteral (LBool True))))
                    (loweredLiteral (LInt 1)),
                  loweredCaseArm
                    (loweredVariablePattern "other")
                    (Just (loweredBinary "|" (loweredVariable "left") (loweredVariable "Nothing")))
                    (loweredLiteral (LInt 2))
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
        assertLoweredCoreEqual "constructor if-expression pipe RHS lowered AST" expectedLoweredProgram (lowerSurfaceExpr surfaceProgram)
    )
  where
    expectedLoweredProgram =
      loweredBlock
        [ loweredLet
            "x"
            (SourceSpan 1 1)
            ( loweredPatternCase
                (loweredVariable "m")
                [ loweredCaseArm
                    (loweredVariablePattern "item")
                    ( Just
                        ( loweredBinary
                            "=="
                            (loweredVariable "item")
                            ( loweredBinary
                                "|"
                                (loweredLiteral (LInt 0))
                                (loweredApply (loweredVariable "Just") (loweredIf (loweredVariable "ok") (loweredLiteral (LInt 1)) (loweredLiteral (LInt 2))))
                            )
                        )
                    )
                    (loweredVariable "item"),
                  loweredCaseArm
                    loweredWildcardPattern
                    Nothing
                    (loweredVariable "m")
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
        assertLoweredCoreEqual "as-pattern constructor argument lowered AST" expectedLoweredProgram (lowerSurfaceExpr surfaceProgram)
    )
  where
    expectedLoweredProgram =
      loweredBlock
        [ loweredLet
            "x"
            (SourceSpan 1 1)
            ( loweredPatternCase
                (loweredVariable "subject")
                [ loweredCaseArm
                    (loweredConstructorPattern "Pair" [loweredAsPattern "whole" (loweredConstructorPattern "Nothing" []), loweredVariablePattern "item"])
                    Nothing
                    (loweredVariable "item"),
                  loweredCaseArm
                    loweredWildcardPattern
                    Nothing
                    (loweredLiteral (LInt 0))
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
        assertLoweredCoreEqual "lowered as-pattern lambda" expectedProgram (lowerSurfaceExpr surfaceProgram)
    )
  where
    generatedParameter = generatedName (LambdaPatternArgument 1)
    expectedProgram =
      loweredBlock
        [ loweredLet
            "f"
            (SourceSpan 1 1)
            ( loweredLambda
                generatedParameter
                ( loweredPatternCase
                    (loweredVariable generatedParameter)
                    [ loweredCaseArm
                        (loweredAsPattern "whole" (loweredConsListPattern (loweredVariablePattern "head") (loweredVariablePattern "tail")))
                        Nothing
                        (loweredVariable "head")
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
        assertLoweredCoreEqual "constructor pattern lowered AST" expectedLoweredProgram (lowerSurfaceExpr surfaceProgram)
    )
  where
    expectedLoweredProgram =
      loweredBlock
        [ loweredLet
            "x"
            (SourceSpan 1 1)
            ( loweredPatternCase
                (loweredVariable "subject")
                [ loweredCaseArm
                    (loweredConstructorPattern "Just" [loweredVariablePattern "item"])
                    Nothing
                    (loweredVariable "item"),
                  loweredCaseArm
                    (loweredConstructorPattern "Nothing" [])
                    Nothing
                    (loweredLiteral (LInt 0))
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
        assertLoweredCoreEqual "multi-argument constructor pattern lowered AST" expectedLoweredProgram (lowerSurfaceExpr surfaceProgram)
    )
  where
    expectedLoweredProgram =
      loweredBlock
        [ loweredLet
            "x"
            (SourceSpan 1 1)
            ( loweredPatternCase
                (loweredVariable "subject")
                [ loweredCaseArm
                    (loweredConstructorPattern "Pair" [loweredConstructorPattern "Nothing" [], loweredVariablePattern "item"])
                    Nothing
                    (loweredVariable "item"),
                  loweredCaseArm
                    loweredWildcardPattern
                    Nothing
                    (loweredLiteral (LInt 0))
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
        assertLoweredCoreEqual "nullary constructor subpattern lowered AST" expectedLoweredProgram (lowerSurfaceExpr surfaceProgram)
    )
  where
    expectedLoweredProgram =
      loweredBlock
        [ loweredLet
            "x"
            (SourceSpan 1 1)
            ( loweredPatternCase
                (loweredVariable "subject")
                [ loweredCaseArm
                    (loweredConstructorPattern "Just" [loweredConstructorPattern "Nothing" []])
                    Nothing
                    (loweredLiteral (LInt 1)),
                  loweredCaseArm
                    loweredWildcardPattern
                    Nothing
                    (loweredLiteral (LInt 0))
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
        assertLoweredCoreEqual "list pattern lowered AST" expectedLoweredProgram (lowerSurfaceExpr surfaceProgram)
    )
  where
    expectedLoweredProgram =
      loweredBlock
        [ loweredLet
            "x"
            (SourceSpan 1 1)
            ( loweredPatternCase
                (loweredVariable "values")
                [ loweredCaseArm
                    (loweredListPattern [loweredVariablePattern "head", loweredWildcardPattern])
                    Nothing
                    (loweredVariable "head"),
                  loweredCaseArm
                    (loweredListPattern [])
                    Nothing
                    (loweredLiteral (LInt 0))
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
        assertLoweredCoreEqual "data declaration lowered AST" expectedLoweredProgram (lowerSurfaceExpr surfaceProgram)
    )
  where
    expectedLoweredProgram =
      loweredBlock
        [ loweredData
            (SourceSpan 1 1)
            "Maybe"
            ["a"]
            [ loweredConstructor "Just" [TypeVariable "a"],
              loweredConstructor "Nothing" []
            ]
        ]

testParsesNestedCaseExpression :: IO ()
testParsesNestedCaseExpression =
  assertRight
    "nested case parse + lower"
    (parseSurfaceProgram "x = case n { | 0 -> case y { | 1 -> True | _ -> False } | _ -> False }.")
    (\surfaceProgram -> assertLoweredCoreEqual "nested lowered case AST" expectedProgram (lowerSurfaceExpr surfaceProgram))
  where
    expectedProgram =
      loweredBlock
        [ loweredLet
            "x"
            (SourceSpan 1 1)
            ( loweredPatternCase
                (loweredVariable "n")
                [ loweredCaseArm
                    (loweredLiteralPattern (LInt 0))
                    Nothing
                    ( loweredPatternCase
                        (loweredVariable "y")
                        [ loweredCaseArm (loweredLiteralPattern (LInt 1)) Nothing (loweredLiteral (LBool True)),
                          loweredCaseArm loweredWildcardPattern Nothing (loweredLiteral (LBool False))
                        ]
                    ),
                  loweredCaseArm loweredWildcardPattern Nothing (loweredLiteral (LBool False))
                ]
            )
        ]

testParsesIfExpressionInsideCaseArmBody :: IO ()
testParsesIfExpressionInsideCaseArmBody =
  assertRight
    "if expression remains within first case arm"
    (parseSurfaceProgram "x = case n { | 0 -> if True then 1 else 2 | _ -> 3 }.")
    (\surfaceProgram -> assertLoweredCoreEqual "if-in-arm lowered case AST" expectedProgram (lowerSurfaceExpr surfaceProgram))
  where
    expectedProgram =
      loweredBlock
        [ loweredLet
            "x"
            (SourceSpan 1 1)
            ( loweredPatternCase
                (loweredVariable "n")
                [ loweredCaseArm
                    (loweredLiteralPattern (LInt 0))
                    Nothing
                    (loweredIf (loweredLiteral (LBool True)) (loweredLiteral (LInt 1)) (loweredLiteral (LInt 2))),
                  loweredCaseArm loweredWildcardPattern Nothing (loweredLiteral (LInt 3))
                ]
            )
        ]

testParsesLambdaExpressionInsideCaseArmBody :: IO ()
testParsesLambdaExpressionInsideCaseArmBody =
  assertRight
    "lambda expression remains within first case arm"
    (parseSurfaceProgram "x = case n { | 0 -> \\(y) -> y | _ -> 3 }.")
    (\surfaceProgram -> assertLoweredCoreEqual "lambda-in-arm lowered case AST" expectedProgram (lowerSurfaceExpr surfaceProgram))
  where
    expectedProgram =
      loweredBlock
        [ loweredLet
            "x"
            (SourceSpan 1 1)
            ( loweredPatternCase
                (loweredVariable "n")
                [ loweredCaseArm
                    (loweredLiteralPattern (LInt 0))
                    Nothing
                    (loweredLambda "y" (loweredVariable "y")),
                  loweredCaseArm loweredWildcardPattern Nothing (loweredLiteral (LInt 3))
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
        assertLoweredCoreEqual "mixed literal-wildcard later or-pattern lowered AST" expectedLoweredProgram (lowerSurfaceExpr surfaceProgram)
    )
  where
    expectedLoweredProgram =
      loweredBlock
        [ loweredLet
            "x"
            (SourceSpan 1 1)
            ( loweredPatternCase
                (loweredVariable "n")
                [ loweredCaseArm
                    (loweredLiteralPattern (LInt 0))
                    Nothing
                    (loweredLiteral (LInt 1)),
                  loweredCaseArm
                    (loweredOrPattern [loweredLiteralPattern (LInt 2), loweredWildcardPattern])
                    Nothing
                    (loweredLiteral (LInt 3))
                ]
            )
        ]

testKeepsPipeOperatorInsideBodyBeforeConstructorArmBoundary :: IO ()
testKeepsPipeOperatorInsideBodyBeforeConstructorArmBoundary =
  assertRight
    "pipe operator stays in constructor arm body"
    (parseSurfaceProgram "x = case subject { | Just item -> 1 | 2 | Nothing -> 3 }.")
    (\surfaceProgram -> assertLoweredCoreEqual "constructor arm boundary lowered AST" expectedProgram (lowerSurfaceExpr surfaceProgram))
  where
    expectedProgram =
      loweredBlock
        [ loweredLet
            "x"
            (SourceSpan 1 1)
            ( loweredPatternCase
                (loweredVariable "subject")
                [ loweredCaseArm
                    (loweredConstructorPattern "Just" [loweredVariablePattern "item"])
                    Nothing
                    (loweredBinary "|" (loweredLiteral (LInt 1)) (loweredLiteral (LInt 2))),
                  loweredCaseArm
                    (loweredConstructorPattern "Nothing" [])
                    Nothing
                    (loweredLiteral (LInt 3))
                ]
            )
        ]

testKeepsPipeOperatorInsideBodyBeforeLiteralArmBoundary :: IO ()
testKeepsPipeOperatorInsideBodyBeforeLiteralArmBoundary =
  assertRight
    "pipe operator stays in body before literal arm boundary"
    (parseSurfaceProgram "x = case subject { | _ -> 1 | 2 | 3 -> 4 }.")
    (\surfaceProgram -> assertLoweredCoreEqual "literal arm boundary lowered AST" expectedProgram (lowerSurfaceExpr surfaceProgram))
  where
    expectedProgram =
      loweredBlock
        [ loweredLet
            "x"
            (SourceSpan 1 1)
            ( loweredPatternCase
                (loweredVariable "subject")
                [ loweredCaseArm
                    loweredWildcardPattern
                    Nothing
                    (loweredBinary "|" (loweredLiteral (LInt 1)) (loweredLiteral (LInt 2))),
                  loweredCaseArm
                    (loweredLiteralPattern (LInt 3))
                    Nothing
                    (loweredLiteral (LInt 4))
                ]
            )
        ]

testKeepsBareListLiteralAfterPipeOperator :: IO ()
testKeepsBareListLiteralAfterPipeOperator =
  assertRight
    "bare list literal stays in case arm body"
    (parseSurfaceProgram "x = case subject { | _ -> 1 | [2] }.")
    (\surfaceProgram -> assertLoweredCoreEqual "list literal in arm body lowered AST" expectedProgram (lowerSurfaceExpr surfaceProgram))
  where
    expectedProgram =
      loweredBlock
        [ loweredLet
            "x"
            (SourceSpan 1 1)
            ( loweredPatternCase
                (loweredVariable "subject")
                [ loweredCaseArm
                    loweredWildcardPattern
                    Nothing
                    (loweredBinary "|" (loweredLiteral (LInt 1)) (loweredList [loweredLiteral (LInt 2)]))
                ]
            )
        ]

testKeepsBareConstructorValueAfterPipeOperator :: IO ()
testKeepsBareConstructorValueAfterPipeOperator =
  assertRight
    "bare constructor subject stays in case arm body"
    (parseSurfaceProgram "x = case subject { | _ -> 1 | Nothing }.")
    (\surfaceProgram -> assertLoweredCoreEqual "constructor subject in arm body lowered AST" expectedProgram (lowerSurfaceExpr surfaceProgram))
  where
    expectedProgram =
      loweredBlock
        [ loweredLet
            "x"
            (SourceSpan 1 1)
            ( loweredPatternCase
                (loweredVariable "subject")
                [ loweredCaseArm
                    loweredWildcardPattern
                    Nothing
                    (loweredBinary "|" (loweredLiteral (LInt 1)) (loweredVariable "Nothing"))
                ]
            )
        ]

testKeepsListApplicationAfterPipeOperator :: IO ()
testKeepsListApplicationAfterPipeOperator =
  assertRight
    "list application stays in case arm body"
    (parseSurfaceProgram "x = case values { | _ -> 1 | [head] 2 }.")
    (\surfaceProgram -> assertLoweredCoreEqual "list application in arm body lowered AST" expectedProgram (lowerSurfaceExpr surfaceProgram))
  where
    expectedProgram =
      loweredBlock
        [ loweredLet
            "x"
            (SourceSpan 1 1)
            ( loweredPatternCase
                (loweredVariable "values")
                [ loweredCaseArm
                    loweredWildcardPattern
                    Nothing
                    (loweredBinary "|" (loweredLiteral (LInt 1)) (loweredApply (loweredList [loweredVariable "head"]) (loweredLiteral (LInt 2))))
                ]
            )
        ]

testKeepsConstructorApplicationAfterPipeOperator :: IO ()
testKeepsConstructorApplicationAfterPipeOperator =
  assertRight
    "constructor application stays in case arm body"
    (parseSurfaceProgram "x = case subject { | _ -> 1 | Just a b }.")
    (\surfaceProgram -> assertLoweredCoreEqual "constructor application in arm body lowered AST" expectedProgram (lowerSurfaceExpr surfaceProgram))
  where
    expectedProgram =
      loweredBlock
        [ loweredLet
            "x"
            (SourceSpan 1 1)
            ( loweredPatternCase
                (loweredVariable "subject")
                [ loweredCaseArm
                    loweredWildcardPattern
                    Nothing
                    (loweredBinary "|" (loweredLiteral (LInt 1)) (loweredApply (loweredApply (loweredVariable "Just") (loweredVariable "a")) (loweredVariable "b")))
                ]
            )
        ]

testParsesCaseScrutineeWithBlockArgument :: IO ()
testParsesCaseScrutineeWithBlockArgument =
  assertRight
    "case scrutinee keeps block argument"
    (parseSurfaceProgram "x = case f { y = 1. y. } { | 1 -> True | _ -> False }.")
    (\surfaceProgram -> assertLoweredCoreEqual "block-argument scrutinee lowered case AST" expectedProgram (lowerSurfaceExpr surfaceProgram))
  where
    expectedProgram =
      loweredBlock
        [ loweredLet
            "x"
            (SourceSpan 1 1)
            ( loweredPatternCase
                ( loweredApply
                    (loweredVariable "f")
                    ( loweredBlock
                        [ loweredLet "y" (SourceSpan 1 14) (loweredLiteral (LInt 1)),
                          loweredExpression (SourceSpan 1 21) (loweredVariable "y")
                        ]
                    )
                )
                [ loweredCaseArm (loweredLiteralPattern (LInt 1)) Nothing (loweredLiteral (LBool True)),
                  loweredCaseArm loweredWildcardPattern Nothing (loweredLiteral (LBool False))
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
        assertLoweredCoreEqual "cons-like list pattern lowered AST" expectedLoweredProgram (lowerSurfaceExpr surfaceProgram)
    )
  where
    expectedLoweredProgram =
      loweredBlock
        [ loweredLet
            "x"
            (SourceSpan 1 1)
            ( loweredPatternCase
                (loweredVariable "values")
                [ loweredCaseArm
                    (loweredConsListPattern (loweredVariablePattern "head") (loweredVariablePattern "tail"))
                    Nothing
                    (loweredVariable "head"),
                  loweredCaseArm
                    loweredWildcardPattern
                    Nothing
                    (loweredLiteral (LInt 0))
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
        assertLoweredCoreEqual "cons-like list constructor lowered AST" expectedLoweredProgram (lowerSurfaceExpr surfaceProgram)
    )
  where
    expectedLoweredProgram =
      loweredBlock
        [ loweredLet
            "x"
            (SourceSpan 1 1)
            ( loweredPatternCase
                (loweredVariable "subject")
                [ loweredCaseArm
                    (loweredConstructorPattern "Just" [loweredConsListPattern (loweredVariablePattern "head") (loweredVariablePattern "tail")])
                    Nothing
                    (loweredVariable "head"),
                  loweredCaseArm
                    loweredWildcardPattern
                    Nothing
                    (loweredLiteral (LInt 0))
                ]
            )
        ]

testLowerCaseExpression :: IO ()
testLowerCaseExpression =
  assertRight
    "parse + lower case"
    (parseSurfaceProgram "x = case n { | 0 -> True | _ -> False }.")
    (\surfaceProgram -> assertLoweredCoreEqual "lowered case AST" expectedProgram (lowerSurfaceExpr surfaceProgram))
  where
    expectedProgram =
      loweredBlock
        [ loweredLet
            "x"
            (SourceSpan 1 1)
            ( loweredPatternCase
                (loweredVariable "n")
                [ loweredCaseArm (loweredLiteralPattern (LInt 0)) Nothing (loweredLiteral (LBool True)),
                  loweredCaseArm loweredWildcardPattern Nothing (loweredLiteral (LBool False))
                ]
            )
        ]
