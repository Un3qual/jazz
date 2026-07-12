{-# LANGUAGE OverloadedStrings #-}

module JazzNext.Compiler.Parser.AdtPattern.DeclarationsTests
  ( declarationTests
) where

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
    assertRight
  )

declarationTests :: [NamedTest]
declarationTests =
  [ ("keeps higher-precedence pipe in comparison guard RHS", testKeepsHigherPrecedencePipeInComparisonGuardRhs)
    , ("keeps literal pipe operand in equality guard RHS", testKeepsLiteralPipeOperandInEqualityGuardRhs)
    , ("keeps literal pipe operand in inequality guard RHS", testKeepsLiteralPipeOperandInInequalityGuardRhs)
    , ("keeps literal pipe operand in ordering guard RHS", testKeepsLiteralPipeOperandInOrderingGuardRhs)
    , ("parses generic data declaration parameters", testParsesGenericDataDeclarationParameters)
    , ("keeps lambda application after pipe operator inside body", testKeepsLambdaApplicationAfterPipeOperator)
    , ("keeps underscore application after pipe operator inside body", testKeepsUnderscoreApplicationAfterPipeOperator)
    , ("keeps underscore boolean application after pipe operator inside body", testKeepsUnderscoreBooleanApplicationAfterPipeOperator)
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
