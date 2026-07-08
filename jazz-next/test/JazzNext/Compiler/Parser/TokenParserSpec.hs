{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Text (Text)
import JazzNext.Compiler.Diagnostics
  ( renderDiagnostic
  )
import JazzNext.Compiler.Parser.Lexer
  ( Token (..),
    TokenKind (..),
    tokenize
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
    ("renders token parser diagnostics with token spans", testTokenParserDiagnostic)
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

parseInteger :: Parser Integer
parseInteger = do
  token <- parseTokenKind (TInt 42)
  case token of
    TInt value -> pure value
    _ -> fail "expected integer token"

lexSource :: Text -> IO [Token]
lexSource source =
  case tokenize source of
    Right tokens -> pure tokens
    Left diagnostic -> failTest ("tokenize: expected Right, got " <> renderDiagnostic diagnostic)
