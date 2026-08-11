{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Text as Text
import Jazz.Compiler.DiagnosticCatalog
  ( ErrorCode (E0001),
  )
import Jazz.Compiler.Diagnostics
  ( SourceSpan (..),
    diagnosticPrimarySpan,
    diagnosticSummary,
  )
import Jazz.Compiler.Diagnostics.Render
  ( renderDiagnostic,
  )
import Jazz.Compiler.Parser.Failure
  ( ParserEncountered (..),
    ParserFailure (..),
    ParserFailureReason (..),
  )
import Jazz.Compiler.Parser.Lexer
  ( Token (..),
    TokenKind (..),
    isImmediatelyAfter,
    tokenize,
  )
import Jazz.Compiler.Parser.TestSupport
  ( lexSource,
  )
import Jazz.Compiler.Parser.TokenParser
  ( Parser,
    parseIdentifier,
    parseTokenKind,
    runTokenParser,
    runTokenParserDetailed,
    runTokenParserPrefix,
    runTokenStreamParserPrefix,
  )
import Jazz.Compiler.Parser.TokenStream
  ( tokenStreamFromList,
    tokenStreamToList,
  )
import Jazz.TestHarness
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
    ("indexed prefix parser returns an exact shared remainder", testIndexedPrefixParserReturnsRemainder),
    ("detailed token failures preserve expected and found syntax", testDetailedTokenFailure),
    ("detailed end-of-input failures have no token span", testDetailedEndOfInputFailure),
    ("detailed trailing-token failures preserve the offending token", testDetailedTrailingTokenFailure),
    ("trailing-token diagnostics preserve the offending token", testTrailingTokenDiagnostic),
    ("recognizes lexically adjacent tokens", testRecognizesLexicallyAdjacentTokens),
    ("tokenizes then as a reserved keyword", testTokenizesThenKeyword),
    ("tokenizes value as a reserved keyword", testTokenizesValueKeyword),
    ("tokenizes Char and Text literals", testTokenizesCharAndTextLiterals),
    ("decodes Char and Text escapes", testDecodesCharAndTextEscapes),
    ("preserves derived and zero-padded lexemes", testPreservesDerivedAndZeroPaddedLexemes),
    ("preserves quoted literal lexemes and spans", testPreservesQuotedLiteralLexemesAndSpans),
    ("preserves first-character lexer dispatch contracts", testFirstCharacterLexerDispatch),
    ("preserves lexer dispatch failure diagnostics", testLexerDispatchFailureDiagnostics),
    ("rejects malformed Char and Text literals", testRejectsMalformedCharAndTextLiterals),
    ("renders token parser diagnostics with token spans", testTokenParserDiagnostic),
    ("renders expected Char and Text tokens in diagnostics", testLiteralTokenParserDiagnostics),
    ("renders invalid character lexer diagnostics", testInvalidCharacterLexerDiagnostic)
  ]

testRunTokenParser :: IO ()
testRunTokenParser = do
  tokens <- lexSource "entry = 42."
  assertEqual
    "parsed token stream"
    (Right ("entry", TEquals, 42, TDot))
    ( runTokenParser
        "token parser spec"
        ((,,,) <$> parseIdentifier <*> parseTokenKind TEquals <*> parseInteger <*> parseTokenKind TDot)
        tokens
    )

testRunTokenParserPrefixReturnsRemainder :: IO ()
testRunTokenParserPrefixReturnsRemainder = do
  tokens <- lexSource "entry."
  assertEqual
    "prefix result"
    (Right ("entry", [TDot]))
    (fmap (fmap (map tokenKind)) (runTokenParserPrefix "identifier prefix" parseIdentifier tokens))

testIndexedPrefixParserReturnsRemainder :: IO ()
testIndexedPrefixParserReturnsRemainder = do
  tokens <- lexSource "entry. trailing."
  assertEqual
    "indexed prefix result"
    (Right ("entry", [TDot, TIdentifier "trailing", TDot]))
    ( fmap
        (fmap (map tokenKind . tokenStreamToList))
        ( runTokenStreamParserPrefix
            "indexed identifier prefix"
            parseIdentifier
            (tokenStreamFromList tokens)
        )
    )

testDetailedTokenFailure :: IO ()
testDetailedTokenFailure = do
  tokens <- lexSource "entry 42."
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
  tokens <- lexSource "entry"
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

testDetailedTrailingTokenFailure :: IO ()
testDetailedTrailingTokenFailure = do
  tokens <- lexSource "entry 42."
  assertEqual
    "detailed trailing-token failure"
    ( Left
        ParserFailure
          { parserFailureCode = E0001,
            parserFailureSpan = Just (SourceSpan 1 7),
            parserFailureReason =
              ExpectedSyntax
                "end of input"
                (ParserFoundToken (TInt 42) "42")
          }
    )
    (runTokenParserDetailed "token parser spec" parseIdentifier tokens)

testTrailingTokenDiagnostic :: IO ()
testTrailingTokenDiagnostic = do
  tokens <- lexSource "entry 42."
  case runTokenParser "token parser spec" parseIdentifier tokens of
    Left diagnostic -> do
      assertEqual "trailing-token diagnostic span" (Just (SourceSpan 1 7)) (diagnosticPrimarySpan diagnostic)
      assertContains "trailing-token diagnostic summary" "expected end of input, found '42'" (diagnosticSummary diagnostic)
    Right value ->
      failTest ("expected trailing-token diagnostic, got " <> value)

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

testTokenizesValueKeyword :: IO ()
testTokenizesValueKeyword = do
  tokens <- lexSource "value answer"
  assertEqual
    "value keyword token kinds"
    [TValue, TIdentifier "answer"]
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

testPreservesDerivedAndZeroPaddedLexemes :: IO ()
testPreservesDerivedAndZeroPaddedLexemes = do
  tokens <- lexSource "value 00042 -> =="
  assertEqual
    "derived and zero-padded lexemes"
    ["value", "00042", "->", "=="]
    (map tokenLexeme tokens)

testPreservesQuotedLiteralLexemesAndSpans :: IO ()
testPreservesQuotedLiteralLexemesAndSpans = do
  tokens <- lexSource "'\\u{41}' \"a\\n\""
  assertEqual
    "literal lexemes and spans"
    [("'\\u{41}'", SourceSpan 1 1), ("\"a\\n\"", SourceSpan 1 10)]
    [(tokenLexeme token, tokenSpan token) | token <- tokens]

testFirstCharacterLexerDispatch :: IO ()
testFirstCharacterLexerDispatch = do
  let source =
        Text.unlines
          [ "# leading comment",
            "  'a'",
            "\"text\"",
            "007",
            "alpha_2 _private # trailing comment",
            ":: : @ = \\ . { } ( ) [ ] ,",
            "== => != !~ <= << >= >> +++ -> -- ** // || %% && ?? ^^ ~~ $",
            "# final comment"
          ]
  tokens <- lexSource source
  assertEqual
    "ordered lexer dispatch tokens"
    [ (TChar 'a', "'a'", SourceSpan 2 3),
      (TText "text", "\"text\"", SourceSpan 3 1),
      (TInt 7, "007", SourceSpan 4 1),
      (TIdentifier "alpha_2", "alpha_2", SourceSpan 5 1),
      (TIdentifier "_private", "_private", SourceSpan 5 9),
      (TColonColon, "::", SourceSpan 6 1),
      (TColon, ":", SourceSpan 6 4),
      (TAt, "@", SourceSpan 6 6),
      (TEquals, "=", SourceSpan 6 8),
      (TLambda, "\\", SourceSpan 6 10),
      (TDot, ".", SourceSpan 6 12),
      (TLBrace, "{", SourceSpan 6 14),
      (TRBrace, "}", SourceSpan 6 16),
      (TLParen, "(", SourceSpan 6 18),
      (TRParen, ")", SourceSpan 6 20),
      (TLBracket, "[", SourceSpan 6 22),
      (TRBracket, "]", SourceSpan 6 24),
      (TComma, ",", SourceSpan 6 26),
      (TOperator "==", "==", SourceSpan 7 1),
      (TOperator "=>", "=>", SourceSpan 7 4),
      (TOperator "!=", "!=", SourceSpan 7 7),
      (TOperator "!~", "!~", SourceSpan 7 10),
      (TOperator "<=", "<=", SourceSpan 7 13),
      (TOperator "<<", "<<", SourceSpan 7 16),
      (TOperator ">=", ">=", SourceSpan 7 19),
      (TOperator ">>", ">>", SourceSpan 7 22),
      (TOperator "+++", "+++", SourceSpan 7 25),
      (TArrow, "->", SourceSpan 7 29),
      (TOperator "--", "--", SourceSpan 7 32),
      (TOperator "**", "**", SourceSpan 7 35),
      (TOperator "//", "//", SourceSpan 7 38),
      (TOperator "||", "||", SourceSpan 7 41),
      (TOperator "%%", "%%", SourceSpan 7 44),
      (TOperator "&&", "&&", SourceSpan 7 47),
      (TOperator "??", "??", SourceSpan 7 50),
      (TOperator "^^", "^^", SourceSpan 7 53),
      (TOperator "~~", "~~", SourceSpan 7 56),
      (TOperator "$", "$", SourceSpan 7 59)
    ]
    [(tokenKind token, tokenLexeme token, tokenSpan token) | token <- tokens]

testLexerDispatchFailureDiagnostics :: IO ()
testLexerDispatchFailureDiagnostics =
  mapM_
    ( \(label, source, expectedSpan, expectedSummary) ->
        case tokenize source of
          Left diagnostic -> do
            assertEqual (label <> " span") (Just expectedSpan) (diagnosticPrimarySpan diagnostic)
            assertEqual (label <> " summary") expectedSummary (diagnosticSummary diagnostic)
          Right tokens ->
            failTest (label <> ": expected failure, got " <> Text.pack (show tokens))
    )
    [ ( "malformed character literal",
        "'ab'",
        SourceSpan 1 1,
        "character literal must contain exactly one Unicode scalar"
      ),
      ( "unterminated text literal",
        "  \"text",
        SourceSpan 1 3,
        "unterminated text literal"
      ),
      ( "unexpected character after ignored input",
        "# ignored\n  `",
        SourceSpan 2 3,
        "unexpected character '`'"
      )
    ]

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
  tokens <- lexSource "entry 42."
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
  case tokenize "entry ` 42." of
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
