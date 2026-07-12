{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Text as Text
import JazzNext.Compiler.Diagnostics
  ( SourceSpan (..),
    diagnosticCode,
    diagnosticPrimarySpan,
    renderDiagnostic
  )
import JazzNext.Compiler.Parser.Lexer
  ( LexicalFailure (..),
    LexicalFailureReason (..),
    LexicalLiteralKind (..),
    tokenize,
    tokenizeDetailed
  )
import JazzNext.TestHarness
  ( NamedTest,
    assertContains,
    assertEqual,
    failTest,
    runTestSuite
  )

main :: IO ()
main = runTestSuite "CanonicalLexerComparison" tests

tests :: [NamedTest]
tests =
  [ ("preserves unexpected characters structurally", testUnexpectedCharacter),
    ("preserves invalid escapes structurally", testInvalidEscape),
    ("preserves non-scalar escapes structurally", testNonScalarEscape),
    ("preserves character length failures structurally", testCharacterLengths),
    ("preserves unterminated literals structurally", testUnterminatedLiterals),
    ("preserves raw newlines structurally", testRawNewline),
    ("preserves malformed Unicode escapes structurally", testMalformedUnicodeEscapes),
    ("keeps the legacy diagnostic wrapper", testLegacyDiagnosticWrapper)
  ]

testUnexpectedCharacter :: IO ()
testUnexpectedCharacter =
  assertEqual
    "unexpected character"
    (Left (LexicalFailure (UnexpectedCharacter '`') (SourceSpan 1 7)))
    (tokenizeDetailed "value ` 42.")

testInvalidEscape :: IO ()
testInvalidEscape =
  assertEqual
    "invalid escape"
    (Left (LexicalFailure (InvalidEscape 'x') (SourceSpan 1 1)))
    (tokenizeDetailed "'\\x'")

testNonScalarEscape :: IO ()
testNonScalarEscape =
  assertEqual
    "surrogate escape"
    (Left (LexicalFailure (NonScalarUnicodeEscape "D800") (SourceSpan 1 1)))
    (tokenizeDetailed "'\\u{D800}'")

testCharacterLengths :: IO ()
testCharacterLengths = do
  assertDetailedFailure
    "empty character"
    (LexicalFailure (InvalidCharacterLength 0) (SourceSpan 1 1))
    "''"
  assertDetailedFailure
    "multi-scalar character"
    (LexicalFailure (InvalidCharacterLength 2) (SourceSpan 1 1))
    "'ab'"

testUnterminatedLiterals :: IO ()
testUnterminatedLiterals = do
  assertDetailedFailure
    "unterminated character"
    (LexicalFailure (UnterminatedLiteral CharacterLiteral) (SourceSpan 1 1))
    "'a"
  assertDetailedFailure
    "unterminated text"
    (LexicalFailure (UnterminatedLiteral TextLiteral) (SourceSpan 1 1))
    "\"abc"
  assertDetailedFailure
    "unterminated Unicode escape"
    (LexicalFailure UnterminatedUnicodeEscape (SourceSpan 1 1))
    "'\\u"

testRawNewline :: IO ()
testRawNewline =
  assertDetailedFailure
    "raw text newline"
    (LexicalFailure (RawNewline TextLiteral) (SourceSpan 1 1))
    "\"a\nb\""

testMalformedUnicodeEscapes :: IO ()
testMalformedUnicodeEscapes = do
  assertDetailedFailure
    "empty Unicode escape"
    (LexicalFailure (MalformedUnicodeEscape "") (SourceSpan 1 1))
    "'\\u{}'"
  assertDetailedFailure
    "non-hex Unicode escape"
    (LexicalFailure (MalformedUnicodeEscape "xyz") (SourceSpan 1 1))
    "'\\u{xyz}'"

testLegacyDiagnosticWrapper :: IO ()
testLegacyDiagnosticWrapper =
  case tokenize "value ` 42." of
    Left diagnostic -> do
      assertEqual "legacy code" "E0001" (diagnosticCode diagnostic)
      assertEqual "legacy span" (Just (SourceSpan 1 7)) (diagnosticPrimarySpan diagnostic)
      assertContains "legacy summary" "unexpected character '`' at 1:7" (renderDiagnostic diagnostic)
    Right tokens -> failTest ("expected lexical failure, got " <> showText tokens)

showText :: Show a => a -> Text.Text
showText = Text.pack . show

assertDetailedFailure :: Text.Text -> LexicalFailure -> Text.Text -> IO ()
assertDetailedFailure label expected source =
  assertEqual label (Left expected) (tokenizeDetailed source)
