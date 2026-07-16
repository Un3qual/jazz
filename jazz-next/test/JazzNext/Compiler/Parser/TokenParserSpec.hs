{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Text as Text
import JazzNext.Compiler.DiagnosticCatalog
  ( ErrorCode (E0001),
  )
import JazzNext.Compiler.Diagnostics
  ( SourceSpan (..),
    diagnosticPrimarySpan,
    diagnosticSummary,
  )
import JazzNext.Compiler.Diagnostics.Render
  ( renderDiagnostic,
  )
import JazzNext.Compiler.Parser.Failure
  ( ParserEncountered (..),
    ParserFailure (..),
    ParserFailureReason (..),
  )
import JazzNext.Compiler.Parser.Lexer
  ( Token (..),
    TokenKind (..),
    isImmediatelyAfter,
    tokenize,
  )
import JazzNext.Compiler.Parser.TestSupport
  ( lexSource,
  )
import JazzNext.Compiler.Parser.TokenParser
  ( Parser,
    parseIdentifier,
    parseTokenKind,
    runTokenParser,
    runTokenParserDetailed,
    runTokenParserPrefix,
  )
import JazzNext.TestHarness
  ( NamedTest,
    assertContains,
    assertEqual,
    failTest,
    runTestSuite,
  )

main :: IO ()
main = runTestSuite "TokenParser" tests

tests :: [NamedTest]
tests =
  [ ("runs a Megaparsec token parser over lexer tokens", testRunTokenParser),
    ("prefix parser returns the unconsumed token stream", testRunTokenParserPrefixReturnsRemainder),
    ("detailed token failures preserve expected and found syntax", testDetailedTokenFailure),
    ("detailed end-of-input failures have no token span", testDetailedEndOfInputFailure),
    ("recognizes lexically adjacent tokens", testRecognizesLexicallyAdjacentTokens),
    ("tokenizes then as a reserved keyword", testTokenizesThenKeyword),
    ("tokenizes Char and Text literals", testTokenizesCharAndTextLiterals),
    ("decodes Char and Text escapes", testDecodesCharAndTextEscapes),
    ("preserves quoted literal lexemes and spans", testPreservesQuotedLiteralLexemesAndSpans),
    ("rejects malformed Char and Text literals", testRejectsMalformedCharAndTextLiterals),
    ("renders token parser diagnostics with token spans", testTokenParserDiagnostic),
    ("renders expected Char and Text tokens in diagnostics", testLiteralTokenParserDiagnostics),
    ("renders invalid character lexer diagnostics", testInvalidCharacterLexerDiagnostic)
  ]

testRunTokenParser :: IO ()
testRunTokenParser = do
  tokens <- lexSource "value = 42."
  assertEqual
    "parsed token stream"
    (Right ("value", TEquals, 42, TDot))
    ( runTokenParser
        "token parser spec"
        ((,,,) <$> parseIdentifier <*> parseTokenKind TEquals <*> parseInteger <*> parseTokenKind TDot)
        tokens
    )

testRunTokenParserPrefixReturnsRemainder :: IO ()
testRunTokenParserPrefixReturnsRemainder = do
  tokens <- lexSource "value."
  assertEqual
    "prefix result"
    (Right ("value", [TDot]))
    (fmap (fmap (map tokenKind)) (runTokenParserPrefix "identifier prefix" parseIdentifier tokens))

testDetailedTokenFailure :: IO ()
testDetailedTokenFailure = do
  tokens <- lexSource "value 42."
  assertEqual
    "detailed token failure"
    ( Left
        ParserFailure
          { parserFailureCode = E0001,
            parserFailureSpan = Just (SourceSpan 1 7),
            parserFailureReason =
              ExpectedSyntax
                "'='"
                (ParserFoundToken (TInt 42) "42")
          }
    )
    (runTokenParserDetailed "token parser spec" (parseIdentifier *> parseTokenKind TEquals) tokens)

testDetailedEndOfInputFailure :: IO ()
testDetailedEndOfInputFailure = do
  tokens <- lexSource "value"
  assertEqual
    "detailed end-of-input failure"
    ( Left
        ParserFailure
          { parserFailureCode = E0001,
            parserFailureSpan = Nothing,
            parserFailureReason = ExpectedSyntax "'='" ParserEndOfInput
          }
    )
    (runTokenParserDetailed "token parser spec" (parseIdentifier *> parseTokenKind TEquals) tokens)

testRecognizesLexicallyAdjacentTokens :: IO ()
testRecognizesLexicallyAdjacentTokens = do
  tokens <- lexSource "left::member right :: member"
  case tokens of
    left : compactColon : _ : right : spacedColon : _ -> do
      assertEqual "compact separator adjacency" True (isImmediatelyAfter left compactColon)
      assertEqual "spaced separator adjacency" False (isImmediatelyAfter right spacedColon)
    _ -> failTest "expected two qualified-name token groups"

testTokenizesThenKeyword :: IO ()
testTokenizesThenKeyword = do
  tokens <- lexSource "if condition then yes else no"
  assertEqual
    "conditional keyword token kinds"
    [TIf, TIdentifier "condition", TThen, TIdentifier "yes", TElse, TIdentifier "no"]
    (map tokenKind tokens)

testTokenizesCharAndTextLiterals :: IO ()
testTokenizesCharAndTextLiterals = do
  tokens <- lexSource "'a' \"Jazz\"."
  assertEqual
    "Char/Text token kinds"
    [TChar 'a', TText "Jazz", TDot]
    (map tokenKind tokens)

testDecodesCharAndTextEscapes :: IO ()
testDecodesCharAndTextEscapes = do
  tokens <- lexSource "'\\n' \"quote: \\\"; scalar: \\u{1F3B7}\"."
  assertEqual
    "decoded escapes"
    [TChar '\n', TText "quote: \"; scalar: 🎷", TDot]
    (map tokenKind tokens)

testPreservesQuotedLiteralLexemesAndSpans :: IO ()
testPreservesQuotedLiteralLexemesAndSpans = do
  tokens <- lexSource "'\\u{41}' \"a\\n\""
  assertEqual
    "literal lexemes and spans"
    [("'\\u{41}'", SourceSpan 1 1), ("\"a\\n\"", SourceSpan 1 10)]
    [(tokenLexeme token, tokenSpan token) | token <- tokens]

testRejectsMalformedCharAndTextLiterals :: IO ()
testRejectsMalformedCharAndTextLiterals = do
  let cases =
        [ ("empty Char", "''", "character literal must contain exactly one Unicode scalar"),
          ("multi-scalar Char", "'ab'", "character literal must contain exactly one Unicode scalar"),
          ("unterminated Char", "'a", "unterminated character literal"),
          ("unterminated Text", "\"abc", "unterminated text literal"),
          ("unterminated Char escape", Text.pack ['\'', '\\'], "unterminated character literal"),
          ("unterminated Text escape", Text.pack ['\"', '\\'], "unterminated text literal"),
          ("unterminated Char Unicode escape", Text.pack ['\'', '\\', 'u'], "unterminated Unicode escape"),
          ("unterminated Text Unicode escape", Text.pack ['\"', '\\', 'u'], "unterminated Unicode escape"),
          ("invalid escape", "'\\x'", "invalid escape '\\x'"),
          ("empty scalar escape", "'\\u{}'", "Unicode escape must contain 1-6 hexadecimal digits"),
          ("surrogate scalar", "'\\u{D800}'", "Unicode escape is not a scalar value"),
          ("large scalar", "'\\u{110000}'", "Unicode escape is not a scalar value"),
          ("raw newline", "\"a\nb\"", "raw newline is not allowed in a text literal")
        ]
  mapM_
    ( \(label, source, expected) ->
        case tokenize source of
          Left diagnostic -> do
            assertContains (label <> " code") "E0001" (renderDiagnostic diagnostic)
            assertContains label expected (renderDiagnostic diagnostic)
          Right tokens -> failTest (label <> ": expected failure, got " <> Text.pack (show tokens))
    )
    cases

testTokenParserDiagnostic :: IO ()
testTokenParserDiagnostic = do
  tokens <- lexSource "value 42."
  let result = runTokenParser "token parser spec" (parseIdentifier *> parseTokenKind TEquals) tokens
  case result of
    Left diagnostic -> do
      assertContains "diagnostic" "expected '='" (renderDiagnostic diagnostic)
      assertEqual "diagnostic primary span" (Just (SourceSpan 1 7)) (diagnosticPrimarySpan diagnostic)
      assertEqual "diagnostic summary excludes rendered coordinates" False ("1:7" `Text.isInfixOf` diagnosticSummary diagnostic)
    Right value ->
      fail ("expected diagnostic, got " <> show value)

testLiteralTokenParserDiagnostics :: IO ()
testLiteralTokenParserDiagnostics = do
  tokens <- lexSource "42."
  let cases =
        [ ("Char", TChar 'a', "expected 'a'"),
          ("Text", TText "Jazz", "expected \"Jazz\"")
        ]
  mapM_
    ( \(label, expectedKind, expectedMessage) ->
        case runTokenParser "literal token diagnostic" (parseTokenKind expectedKind) tokens of
          Left diagnostic ->
            assertContains label expectedMessage (renderDiagnostic diagnostic)
          Right value ->
            failTest (label <> ": expected diagnostic, got " <> Text.pack (show value))
    )
    cases

testInvalidCharacterLexerDiagnostic :: IO ()
testInvalidCharacterLexerDiagnostic =
  case tokenize "value ` 42." of
    Left diagnostic -> do
      assertContains "lexer diagnostic" "unexpected character '`'" (renderDiagnostic diagnostic)
      assertEqual "lexer diagnostic primary span" (Just (SourceSpan 1 7)) (diagnosticPrimarySpan diagnostic)
      assertEqual "lexer summary excludes rendered coordinates" False ("1:7" `Text.isInfixOf` diagnosticSummary diagnostic)
    Right tokens ->
      failTest ("expected invalid character diagnostic, got tokens " <> Text.pack (show tokens))

parseInteger :: Parser Integer
parseInteger = do
  token <- parseTokenKind (TInt 42)
  case token of
    TInt value -> pure value
    _ -> fail "expected integer token"
