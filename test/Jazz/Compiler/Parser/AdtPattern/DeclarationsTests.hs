{-# LANGUAGE OverloadedStrings #-}

module Jazz.Compiler.Parser.AdtPattern.DeclarationsTests
  ( declarationTests,
  )
where

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
import Jazz.Compiler.Parser
  ( parseSurfaceProgram,
  )
import Jazz.Compiler.Parser.Lower
  ( lowerSurfaceExpr,
  )
import Jazz.Compiler.TypeRepresentation (SignatureType (..))
import Jazz.TestHarness
  ( NamedTest,
    assertEqual,
    assertRight,
  )

declarationTests :: [NamedTest]
declarationTests =
  [ ("keeps higher-precedence pipe in comparison guard RHS", testKeepsHigherPrecedencePipeInComparisonGuardRhs),
    ("keeps literal pipe operand in equality guard RHS", testKeepsLiteralPipeOperandInEqualityGuardRhs),
    ("keeps literal pipe operand in inequality guard RHS", testKeepsLiteralPipeOperandInInequalityGuardRhs),
    ("keeps literal pipe operand in ordering guard RHS", testKeepsLiteralPipeOperandInOrderingGuardRhs),
    ("parses generic data declaration parameters", testParsesGenericDataDeclarationParameters),
    ("parses structured data constructor field types", testParsesStructuredDataConstructorFieldTypes),
    ("keeps lambda application after pipe operator inside body", testKeepsLambdaApplicationAfterPipeOperator),
    ("keeps underscore application after pipe operator inside body", testKeepsUnderscoreApplicationAfterPipeOperator),
    ("keeps underscore boolean application after pipe operator inside body", testKeepsUnderscoreBooleanApplicationAfterPipeOperator)
  ]

testKeepsHigherPrecedencePipeInComparisonGuardRhs :: IO ()
testKeepsHigherPrecedencePipeInComparisonGuardRhs =
  assertRight
    "comparison guard keeps pipe expression in RHS"
    (parseSurfaceProgram "x = case subject { | item if left == right | True -> 1 }.")
    (\surfaceProgram -> assertEqual "comparison guard pipe RHS lowered AST" expectedSurfaceProgram (lowerSurfaceExpr surfaceProgram))
  where
    expectedSurfaceProgram =
      EBlock
        [ SLet
            "x"
            (SourceSpan 1 1)
            ( EPatternCase
                (EVar "subject")
                [ CaseArm
                    (PVariable "item")
                    (Just (EBinary "==" (EVar "left") (EBinary "|" (EVar "right") (ELit (LBool True)))))
                    (ELit (LInt 1))
                ]
            )
        ]

testKeepsLiteralPipeOperandInEqualityGuardRhs :: IO ()
testKeepsLiteralPipeOperandInEqualityGuardRhs =
  assertRight
    "equality guard keeps literal pipe operand in RHS"
    (parseSurfaceProgram "x = case m { | item if item == 0 | Just -> item | _ -> m }.")
    (\surfaceProgram -> assertEqual "equality guard literal pipe RHS lowered AST" expectedSurfaceProgram (lowerSurfaceExpr surfaceProgram))
  where
    expectedSurfaceProgram =
      EBlock
        [ SLet
            "x"
            (SourceSpan 1 1)
            ( EPatternCase
                (EVar "m")
                [ CaseArm
                    (PVariable "item")
                    (Just (EBinary "==" (EVar "item") (EBinary "|" (ELit (LInt 0)) (EVar "Just"))))
                    (EVar "item"),
                  CaseArm
                    PWildcard
                    Nothing
                    (EVar "m")
                ]
            )
        ]

testKeepsLiteralPipeOperandInInequalityGuardRhs :: IO ()
testKeepsLiteralPipeOperandInInequalityGuardRhs =
  assertRight
    "inequality guard keeps literal pipe operand in RHS"
    (parseSurfaceProgram "x = case m { | item if item != 0 | Just -> item | _ -> m }.")
    (\surfaceProgram -> assertEqual "inequality guard literal pipe RHS lowered AST" expectedSurfaceProgram (lowerSurfaceExpr surfaceProgram))
  where
    expectedSurfaceProgram =
      EBlock
        [ SLet
            "x"
            (SourceSpan 1 1)
            ( EPatternCase
                (EVar "m")
                [ CaseArm
                    (PVariable "item")
                    (Just (EBinary "!=" (EVar "item") (EBinary "|" (ELit (LInt 0)) (EVar "Just"))))
                    (EVar "item"),
                  CaseArm
                    PWildcard
                    Nothing
                    (EVar "m")
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
        (\surfaceProgram -> assertEqual "ordering guard literal pipe RHS lowered AST" (expectedSurfaceProgram operator) (lowerSurfaceExpr surfaceProgram))

    expectedSurfaceProgram operator =
      EBlock
        [ SLet
            "x"
            (SourceSpan 1 1)
            ( EPatternCase
                (EVar "m")
                [ CaseArm
                    (PVariable "item")
                    (Just (EBinary operator (EVar "item") (EBinary "|" (ELit (LInt 0)) (EVar "Just"))))
                    (EVar "item"),
                  CaseArm
                    PWildcard
                    Nothing
                    (EVar "m")
                ]
            )
        ]

testParsesGenericDataDeclarationParameters :: IO ()
testParsesGenericDataDeclarationParameters =
  assertRight
    "generic data declaration parse + lower"
    (parseSurfaceProgram "data Maybe a = Nothing | Just a.")
    ( \surfaceProgram -> do
        assertEqual "generic data declaration lowered AST" expectedSurfaceProgram (lowerSurfaceExpr surfaceProgram)
        assertEqual "generic data declaration lowered AST" expectedLoweredProgram (lowerSurfaceExpr surfaceProgram)
    )
  where
    expectedSurfaceProgram =
      EBlock
        [ SData
            (SourceSpan 1 1)
            "Maybe"
            ["a"]
            [ DataConstructor "Nothing" [],
              DataConstructor "Just" [TypeVariable "a"]
            ]
        ]
    expectedLoweredProgram =
      EBlock
        [ SData
            (SourceSpan 1 1)
            "Maybe"
            ["a"]
            [ DataConstructor "Nothing" [],
              DataConstructor "Just" [TypeVariable "a"]
            ]
        ]

testParsesStructuredDataConstructorFieldTypes :: IO ()
testParsesStructuredDataConstructorFieldTypes =
  assertRight
    "structured data constructor fields parse + lower"
    ( parseSurfaceProgram
        """
        data Tree a
          = Leaf a
          | Branch Tree(a) Tree(a).
        data Callback a b
          = Callback (a -> b).
        data Forest a
          = Forest [Tree(a)].
        """
    )
    ( \surfaceProgram -> do
        assertEqual "structured constructor field lowered AST" expectedSurfaceProgram (lowerSurfaceExpr surfaceProgram)
        assertEqual "structured constructor field lowered AST" expectedLoweredProgram (lowerSurfaceExpr surfaceProgram)
    )
  where
    treeOfA = TypeApplication "Tree" [TypeVariable "a"]
    loweredTreeOfA = TypeApplication "Tree" [TypeVariable "a"]
    expectedSurfaceProgram =
      EBlock
        [ SData
            (SourceSpan 1 1)
            "Tree"
            ["a"]
            [ DataConstructor "Leaf" [TypeVariable "a"],
              DataConstructor "Branch" [treeOfA, treeOfA]
            ],
          SData
            (SourceSpan 4 1)
            "Callback"
            ["a", "b"]
            [ DataConstructor
                "Callback"
                [TypeFunction (TypeVariable "a") (TypeVariable "b")]
            ],
          SData
            (SourceSpan 6 1)
            "Forest"
            ["a"]
            [DataConstructor "Forest" [TypeList treeOfA]]
        ]
    expectedLoweredProgram =
      EBlock
        [ SData
            (SourceSpan 1 1)
            "Tree"
            ["a"]
            [ DataConstructor "Leaf" [TypeVariable "a"],
              DataConstructor "Branch" [loweredTreeOfA, loweredTreeOfA]
            ],
          SData
            (SourceSpan 4 1)
            "Callback"
            ["a", "b"]
            [ DataConstructor
                "Callback"
                [TypeFunction (TypeVariable "a") (TypeVariable "b")]
            ],
          SData
            (SourceSpan 6 1)
            "Forest"
            ["a"]
            [DataConstructor "Forest" [TypeList loweredTreeOfA]]
        ]

testKeepsLambdaApplicationAfterPipeOperator :: IO ()
testKeepsLambdaApplicationAfterPipeOperator =
  assertRight
    "lambda application stays in case arm body"
    (parseSurfaceProgram "x = case subject { | _ -> 1 | f \\(y) -> y }.")
    (\surfaceProgram -> assertEqual "lambda application in arm body lowered AST" expectedProgram (lowerSurfaceExpr surfaceProgram))
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
    (parseSurfaceProgram "x = case subject { | 0 -> 1 | _ y }.")
    (\surfaceProgram -> assertEqual "underscore application in arm body lowered AST" expectedProgram (lowerSurfaceExpr surfaceProgram))
  where
    expectedProgram =
      EBlock
        [ SLet
            "x"
            (SourceSpan 1 1)
            ( EPatternCase
                (EVar "subject")
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
    (parseSurfaceProgram "x = case subject { | 0 -> 1 | _ False }.")
    (\surfaceProgram -> assertEqual "underscore boolean application in arm body lowered AST" expectedProgram (lowerSurfaceExpr surfaceProgram))
  where
    expectedProgram =
      EBlock
        [ SLet
            "x"
            (SourceSpan 1 1)
            ( EPatternCase
                (EVar "subject")
                [ CaseArm
                    (PLiteral (LInt 0))
                    Nothing
                    (EBinary "|" (ELit (LInt 1)) (EApply (EVar "_") (ELit (LBool False))))
                ]
            )
        ]
