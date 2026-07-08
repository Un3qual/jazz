{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Text as Text
import JazzNext.Compiler.Diagnostics
  ( renderDiagnostic
  )
import JazzNext.Compiler.Parser.Lexer
  ( TokenKind (..),
    tokenize
  )
import JazzNext.Compiler.Parser.TestSupport
  ( lexSource
  )
import JazzNext.Compiler.Parser.TokenParser
  ( Parser,
    parseIdentifier,
    parseTokenKind,
    runTokenParser
  )
import JazzNext.TestHarness
  ( NamedTest,
    assertContains,
    assertEqual,
    failTest,
    runTestSuite
  )

main :: IO ()
main = runTestSuite "TokenParser" tests

tests :: [NamedTest]
tests =
  [ ("runs a Megaparsec token parser over lexer tokens", testRunTokenParser),
    ("renders token parser diagnostics with token spans", testTokenParserDiagnostic),
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

testTokenParserDiagnostic :: IO ()
testTokenParserDiagnostic = do
  tokens <- lexSource "value 42."
  let result = runTokenParser "token parser spec" (parseIdentifier *> parseTokenKind TEquals) tokens
  case result of
    Left diagnostic ->
      assertContains "diagnostic" "expected '='" (renderDiagnostic diagnostic)
    Right value ->
      fail ("expected diagnostic, got " <> show value)

testInvalidCharacterLexerDiagnostic :: IO ()
testInvalidCharacterLexerDiagnostic =
  case tokenize "value ` 42." of
    Left diagnostic ->
      assertContains "lexer diagnostic" "unexpected character '`' at 1:7" (renderDiagnostic diagnostic)
    Right tokens ->
      failTest ("expected invalid character diagnostic, got tokens " <> Text.pack (show tokens))

parseInteger :: Parser Integer
parseInteger = do
  token <- parseTokenKind (TInt 42)
  case token of
    TInt value -> pure value
    _ -> fail "expected integer token"
