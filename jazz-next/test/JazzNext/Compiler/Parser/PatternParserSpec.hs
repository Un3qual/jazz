{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Text as Text
import JazzNext.Compiler.Diagnostics
  ( renderDiagnostic
  )
import JazzNext.Compiler.Parser.AST
  ( SurfaceLambdaParameter (..),
    SurfaceLiteral (..),
    SurfacePattern (..)
  )
import JazzNext.Compiler.Parser.Lexer
  ( Token (..),
    TokenKind (..)
  )
import JazzNext.Compiler.Parser.Pattern
  ( parseCaseArmPatternTokens,
    parseLambdaParameterTokens
  )
import JazzNext.Compiler.Parser.TestSupport
  ( lexSource
  )
import JazzNext.TestHarness
  ( NamedTest,
    assertContains,
    assertEqual,
    failTest,
    runTestSuite
  )

main :: IO ()
main = runTestSuite "PatternGrammar" tests

tests :: [NamedTest]
tests =
  [ ("parses Unit case-arm pattern tokens", testParsesUnitCaseArmPatternTokens),
    ("parses case-arm pattern tokens and preserves remainder", testParsesCaseArmPatternTokens),
    ("parses lambda parameter tokens", testParsesLambdaParameterTokens),
    ("rejects fractional literal patterns", testRejectsFractionalLiteralPatterns)
  ]

testParsesUnitCaseArmPatternTokens :: IO ()
testParsesUnitCaseArmPatternTokens = do
  tokens <- lexSource "() -> body"
  assertEqual
    "Unit case-arm pattern"
    (Right (SPTuple [], [TArrow, TIdentifier "body"]))
    (fmap (fmap tokenKinds) (parseCaseArmPatternTokens tokens))

testParsesCaseArmPatternTokens :: IO ()
testParsesCaseArmPatternTokens = do
  tokens <- lexSource "whole @ Pair (left, right) [1, True, _, item] [head | tail] | Nothing -> body"
  assertEqual
    "case-arm pattern tokens"
    (Right (expectedPattern, [TArrow, TIdentifier "body"]))
    (fmap (fmap tokenKinds) (parseCaseArmPatternTokens tokens))
  where
    expectedPattern =
      SPOr
        [ SPAs
            "whole"
            ( SPConstructor
                "Pair"
                [ SPTuple [SPVariable "left", SPVariable "right"],
                  SPList
                    [ SPLiteral (SLInt 1),
                      SPLiteral (SLBool True),
                      SPWildcard,
                      SPVariable "item"
                    ],
                  SPConsList (SPVariable "head") (SPVariable "tail")
                ]
            ),
          SPConstructor "Nothing" []
        ]

testParsesLambdaParameterTokens :: IO ()
testParsesLambdaParameterTokens = do
  patternTokens <- lexSource "Just item, next"
  assertEqual
    "pattern lambda parameter"
    (Right (SurfaceLambdaPattern (SPConstructor "Just" [SPVariable "item"]), [TComma, TIdentifier "next"]))
    (fmap (fmap tokenKinds) (parseLambdaParameterTokens patternTokens))

  identifierTokens <- lexSource "item, next"
  assertEqual
    "identifier lambda parameter"
    (Right (SurfaceLambdaIdentifier "item", [TComma, TIdentifier "next"]))
    (fmap (fmap tokenKinds) (parseLambdaParameterTokens identifierTokens))

testRejectsFractionalLiteralPatterns :: IO ()
testRejectsFractionalLiteralPatterns = do
  tokens <- lexSource "1.5 -> body"
  case parseCaseArmPatternTokens tokens of
    Left diagnostic ->
      assertContains
        "fractional literal pattern diagnostic"
        "fractional literal patterns are not supported"
        (renderDiagnostic diagnostic)
    Right value ->
      failTest ("expected fractional pattern rejection, got " <> Text.pack (show value))

tokenKinds :: [Token] -> [TokenKind]
tokenKinds = map tokenKind
