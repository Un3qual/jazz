{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Text as Text
import Jazz.Compiler.Diagnostics
  ( SourceSpan (..),
    diagnosticPrimarySpan,
  )
import Jazz.Compiler.Diagnostics.Render
  ( renderDiagnostic,
  )
import Jazz.Compiler.Parser.AST
  ( SurfaceLambdaParameter (..),
    SurfaceLiteral (..),
    SurfacePattern (..),
    SurfacePatternForm (..),
  )
import Jazz.Compiler.Parser.Failure
  ( ParserFailure (..),
    ParserFailureReason (..),
    ParserUnsupportedFeature (..),
  )
import Jazz.Compiler.Parser.Lexer
  ( Token (..),
    TokenKind (..),
  )
import Jazz.Compiler.Parser.Pattern
  ( parseCaseArmPatternParser,
    parseCaseArmPatternTokens,
    parseLambdaParameterTokens,
  )
import Jazz.Compiler.Parser.TestSupport
  ( lexSource,
    surfacePatternAt,
  )
import Jazz.Compiler.Parser.TokenParser (runTokenParserPrefixDetailed)
import Jazz.TestHarness
  ( NamedTest,
    assertContains,
    assertEqual,
    failTest,
    runTestSuite,
  )

main :: IO ()
main = runTestSuite "PatternGrammar" tests

tests :: [NamedTest]
tests =
  [ ("parses Unit case-arm pattern tokens", testParsesUnitCaseArmPatternTokens),
    ("parses case-arm pattern tokens and preserves remainder", testParsesCaseArmPatternTokens),
    ("tracks every nested pattern location", testTracksEveryNestedPatternLocation),
    ("tracks grouped-leading or-pattern location", testTracksGroupedLeadingOrPatternLocation),
    ("parses Char and Text literal patterns", testParsesCharAndTextLiteralPatterns),
    ("parses lambda parameter tokens", testParsesLambdaParameterTokens),
    ("reports fractional pattern rejection structurally", testDetailedFractionalLiteralPattern),
    ("rejects fractional literal patterns", testRejectsFractionalLiteralPatterns),
    ("reports the token missing a tuple-pattern comma", testReportsMissingTuplePatternComma)
  ]

testTracksEveryNestedPatternLocation :: IO ()
testTracksEveryNestedPatternLocation = do
  tokens <- lexSource "whole @ Pair (left, right) [head | tail] [1, item] | Nothing -> body"
  case parseCaseArmPatternTokens tokens of
    Right (orPattern, _) -> do
      assertPatternLocation "or pattern" (SourceSpan 1 1) orPattern
      case surfacePatternForm orPattern of
        SPOr [asPattern, nothingPattern] -> do
          assertPatternLocation "as pattern" (SourceSpan 1 1) asPattern
          assertPatternLocation "nullary constructor pattern" (SourceSpan 1 54) nothingPattern
          case surfacePatternForm asPattern of
            SPAs _ constructorPattern -> do
              assertPatternLocation "constructor pattern" (SourceSpan 1 9) constructorPattern
              case surfacePatternForm constructorPattern of
                SPConstructor _ constructorArguments -> assertConstructorArgumentLocations constructorArguments
                _ -> unexpected "constructor" constructorPattern
            _ -> unexpected "as pattern" asPattern
        _ -> unexpected "or pattern" orPattern
    Left diagnostic -> failTest ("nested pattern locations: expected Right, got " <> renderDiagnostic diagnostic)
  where
    assertConstructorArgumentLocations constructorArguments =
      case constructorArguments of
        [tuplePattern, consPattern, listPattern] -> do
          assertPatternLocation "tuple pattern" (SourceSpan 1 14) tuplePattern
          assertPatternLocation "cons pattern" (SourceSpan 1 28) consPattern
          assertPatternLocation "list pattern" (SourceSpan 1 42) listPattern
          case (surfacePatternForm tuplePattern, surfacePatternForm consPattern, surfacePatternForm listPattern) of
            (SPTuple [leftPattern, rightPattern], SPConsList headPattern tailPattern, SPList [literalPattern, itemPattern]) -> do
              assertPatternLocation "tuple left pattern" (SourceSpan 1 15) leftPattern
              assertPatternLocation "tuple right pattern" (SourceSpan 1 21) rightPattern
              assertPatternLocation "cons head pattern" (SourceSpan 1 29) headPattern
              assertPatternLocation "cons tail pattern" (SourceSpan 1 36) tailPattern
              assertPatternLocation "list literal pattern" (SourceSpan 1 43) literalPattern
              assertPatternLocation "list variable pattern" (SourceSpan 1 46) itemPattern
            _ -> failTest ("nested pattern locations: unexpected constructor arguments " <> Text.pack (show constructorArguments))
        _ -> failTest ("nested pattern locations: unexpected constructor arguments " <> Text.pack (show constructorArguments))

    assertPatternLocation label expected patternValue =
      assertEqual label expected (surfacePatternSpan patternValue)

    unexpected label value =
      failTest ("nested pattern locations: unexpected " <> label <> " " <> Text.pack (show value))

testTracksGroupedLeadingOrPatternLocation :: IO ()
testTracksGroupedLeadingOrPatternLocation = do
  tokens <- lexSource "(Just item) | Nothing -> body"
  case parseCaseArmPatternTokens tokens of
    Right (orPattern, _) -> do
      assertEqual "grouped-leading or-pattern" (SourceSpan 1 1) (surfacePatternSpan orPattern)
      case surfacePatternForm orPattern of
        SPOr [justPattern, nothingPattern] -> do
          assertEqual "grouped constructor" (SourceSpan 1 2) (surfacePatternSpan justPattern)
          assertEqual "later constructor" (SourceSpan 1 15) (surfacePatternSpan nothingPattern)
        _ -> failTest ("grouped-leading or-pattern: unexpected AST " <> Text.pack (show orPattern))
    Left diagnostic -> failTest ("grouped-leading or-pattern: expected Right, got " <> renderDiagnostic diagnostic)

testParsesUnitCaseArmPatternTokens :: IO ()
testParsesUnitCaseArmPatternTokens = do
  tokens <- lexSource "() -> body"
  assertEqual
    "Unit case-arm pattern"
    (Right (p 1 1 (SPTuple []), [TArrow, TIdentifier "body"]))
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
      p
        1
        1
        ( SPOr
            [ p
                1
                1
                ( SPAs
                    "whole"
                    ( p
                        1
                        9
                        ( SPConstructor
                            "Pair"
                            [ p 1 14 (SPTuple [p 1 15 (SPVariable "left"), p 1 21 (SPVariable "right")]),
                              p
                                1
                                28
                                ( SPList
                                    [ p 1 29 (SPLiteral (SLInt 1)),
                                      p 1 32 (SPLiteral (SLBool True)),
                                      p 1 38 SPWildcard,
                                      p 1 41 (SPVariable "item")
                                    ]
                                ),
                              p 1 47 (SPConsList (p 1 48 (SPVariable "head")) (p 1 55 (SPVariable "tail")))
                            ]
                        )
                    )
                ),
              p 1 63 (SPConstructor "Nothing" [])
            ]
        )

testParsesCharAndTextLiteralPatterns :: IO ()
testParsesCharAndTextLiteralPatterns = do
  charTokens <- lexSource "'a' -> body"
  assertEqual
    "Char literal pattern"
    (Right (p 1 1 (SPLiteral (SLChar 'a')), [TArrow, TIdentifier "body"]))
    (fmap (fmap tokenKinds) (parseCaseArmPatternTokens charTokens))
  textTokens <- lexSource "\"Jazz\" -> body"
  assertEqual
    "Text literal pattern"
    (Right (p 1 1 (SPLiteral (SLText "Jazz")), [TArrow, TIdentifier "body"]))
    (fmap (fmap tokenKinds) (parseCaseArmPatternTokens textTokens))

  nestedTokens <- lexSource "Pair 'a' \"Jazz\" -> body"
  assertEqual
    "nested Char/Text literal patterns"
    ( Right
        ( p 1 1 (SPConstructor "Pair" [p 1 6 (SPLiteral (SLChar 'a')), p 1 10 (SPLiteral (SLText "Jazz"))]),
          [TArrow, TIdentifier "body"]
        )
    )
    (fmap (fmap tokenKinds) (parseCaseArmPatternTokens nestedTokens))

  lambdaTokens <- lexSource "'a', next"
  assertEqual
    "Char literal lambda pattern"
    (Right (SurfaceLambdaPattern (p 1 1 (SPLiteral (SLChar 'a'))), [TComma, TIdentifier "next"]))
    (fmap (fmap tokenKinds) (parseLambdaParameterTokens lambdaTokens))

testParsesLambdaParameterTokens :: IO ()
testParsesLambdaParameterTokens = do
  patternTokens <- lexSource "Just item, next"
  assertEqual
    "pattern lambda parameter"
    (Right (SurfaceLambdaPattern (p 1 1 (SPConstructor "Just" [p 1 6 (SPVariable "item")])), [TComma, TIdentifier "next"]))
    (fmap (fmap tokenKinds) (parseLambdaParameterTokens patternTokens))

  identifierTokens <- lexSource "item, next"
  assertEqual
    "identifier lambda parameter"
    (Right (SurfaceLambdaIdentifier (SourceSpan 1 1) "item", [TComma, TIdentifier "next"]))
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

testDetailedFractionalLiteralPattern :: IO ()
testDetailedFractionalLiteralPattern = do
  tokens <- lexSource "1.5 -> body"
  case runTokenParserPrefixDetailed "case arm pattern" parseCaseArmPatternParser tokens of
    Left failure -> do
      assertEqual "fractional pattern detailed span" (Just (SourceSpan 1 1)) (parserFailureSpan failure)
      assertEqual
        "fractional pattern detailed reason"
        (UnsupportedSyntax FractionalLiteralPattern)
        (parserFailureReason failure)
    Right value ->
      failTest ("expected detailed fractional pattern rejection, got " <> Text.pack (show value))

testReportsMissingTuplePatternComma :: IO ()
testReportsMissingTuplePatternComma = do
  tokens <- lexSource "(left right) -> body"
  case parseCaseArmPatternTokens tokens of
    Left diagnostic -> do
      assertContains
        "missing tuple-pattern comma diagnostic"
        "expected ',' or ')', found 'right'"
        (renderDiagnostic diagnostic)
      assertEqual
        "missing tuple-pattern comma span"
        (Just (SourceSpan 1 7))
        (diagnosticPrimarySpan diagnostic)
    Right value ->
      failTest ("expected missing tuple-pattern comma rejection, got " <> Text.pack (show value))

tokenKinds :: [Token] -> [TokenKind]
tokenKinds = map tokenKind

p :: Int -> Int -> SurfacePatternForm -> SurfacePattern
p = surfacePatternAt
