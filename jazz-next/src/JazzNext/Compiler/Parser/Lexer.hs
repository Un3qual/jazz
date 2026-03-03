{-# LANGUAGE OverloadedStrings #-}

module JazzNext.Compiler.Parser.Lexer
  ( Token (..),
    TokenKind (..),
    tokenize
  ) where

import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Read as TextRead
import JazzNext.Compiler.Diagnostics
  ( SourceSpan (..)
  )
import JazzNext.Compiler.Parser.Operator
  ( isBuiltinOperatorSymbol
  )

data TokenKind
  = TIdentifier Text
  | TIf
  | TElse
  | TInt Int
  | TEquals
  | TOperator Text
  | TColonColon
  | TDot
  | TLBrace
  | TRBrace
  | TLParen
  | TRParen
  deriving (Eq, Show)

data Token = Token
  { tokenKind :: TokenKind,
    tokenLexeme :: Text,
    tokenSpan :: SourceSpan
  }
  deriving (Eq, Show)

-- Tokenizes the current parser foundation grammar while preserving 1-based
-- line/column spans for diagnostics.
tokenize :: Text -> Either Text [Token]
tokenize = go 1 1
  where
    go :: Int -> Int -> Text -> Either Text [Token]
    go line column source =
      case Text.uncons source of
        Nothing -> Right []
        Just (char, rest)
          | char == '\n' -> go (line + 1) 1 rest
          | char == '\t' -> go line (column + tabStep column) rest
          | isSpace char -> go line (column + 1) rest
          | isDigit char ->
              let (digits, trailing) = Text.span isDigit source
                  width = Text.length digits
               in do
                    value <- parseIntLiteral line column digits
                    let token =
                          Token
                            { tokenKind = TInt value,
                              tokenLexeme = digits,
                              tokenSpan = SourceSpan line column
                            }
                    (token :) <$> go line (column + width) trailing
          | isIdentifierStart char ->
              let (ident, trailing) = Text.span isIdentifierContinuation source
                  width = Text.length ident
                  kind =
                    case ident of
                      "if" -> TIf
                      "else" -> TElse
                      _ -> TIdentifier ident
                  token =
                    Token
                      { tokenKind = kind,
                        tokenLexeme = ident,
                        tokenSpan = SourceSpan line column
                      }
               in (token :) <$> go line (column + width) trailing
          | otherwise ->
              case char of
                ':' ->
                  case Text.uncons rest of
                    Just (':', after) ->
                      withSingleToken TColonColon "::" 2 line column after
                    _ ->
                      Left ("unexpected ':' at " <> renderSpan line column <> "; expected '::'")
                '=' ->
                  case Text.uncons rest of
                    Just ('=', after) -> withOperatorToken "==" 2 line column after
                    _ -> withSingleToken TEquals "=" 1 line column rest
                '!' ->
                  case Text.uncons rest of
                    Just ('=', after) -> withOperatorToken "!=" 2 line column after
                    _ ->
                      Left ("unexpected character '!' at " <> renderSpan line column)
                '<' ->
                  case Text.uncons rest of
                    Just ('=', after) -> withOperatorToken "<=" 2 line column after
                    _ -> withOperatorToken "<" 1 line column rest
                '>' ->
                  case Text.uncons rest of
                    Just ('=', after) -> withOperatorToken ">=" 2 line column after
                    _ -> withOperatorToken ">" 1 line column rest
                '+' -> withOperatorToken "+" 1 line column rest
                '-' -> withOperatorToken "-" 1 line column rest
                '*' -> withOperatorToken "*" 1 line column rest
                '/' -> withOperatorToken "/" 1 line column rest
                '|' -> withOperatorToken "|" 1 line column rest
                '$' -> withOperatorToken "$" 1 line column rest
                '.' -> withSingleToken TDot "." 1 line column rest
                '{' -> withSingleToken TLBrace "{" 1 line column rest
                '}' -> withSingleToken TRBrace "}" 1 line column rest
                '(' -> withSingleToken TLParen "(" 1 line column rest
                ')' -> withSingleToken TRParen ")" 1 line column rest
                _ ->
                  Left
                    ( "unexpected character '"
                        <> Text.singleton char
                        <> "' at "
                        <> renderSpan line column
                    )

    withSingleToken ::
      TokenKind ->
      Text ->
      Int ->
      Int ->
      Int ->
      Text ->
      Either Text [Token]
    withSingleToken kind lexeme width line column trailing =
      let token =
            Token
              { tokenKind = kind,
                tokenLexeme = lexeme,
                tokenSpan = SourceSpan line column
              }
       in (token :) <$> go line (column + width) trailing

    withOperatorToken ::
      Text ->
      Int ->
      Int ->
      Int ->
      Text ->
      Either Text [Token]
    withOperatorToken symbol width line column trailing =
      if isBuiltinOperatorSymbol symbol
        then
          let token =
                Token
                  { tokenKind = TOperator symbol,
                    tokenLexeme = symbol,
                    tokenSpan = SourceSpan line column
                  }
           in (token :) <$> go line (column + width) trailing
        else
          Left
            ( "unsupported operator '"
                <> symbol
                <> "' at "
                <> renderSpan line column
            )

    isIdentifierStart :: Char -> Bool
    isIdentifierStart char = isAlpha char || char == '_'

    isIdentifierContinuation :: Char -> Bool
    isIdentifierContinuation char =
      isAlphaNum char || char == '_' || char == '\'' || char == '!'

    tabWidth :: Int
    tabWidth = 8

    -- Keep tab alignment deterministic across lexer and parser span tests.
    tabStep :: Int -> Int
    tabStep column = tabWidth - ((column - 1) `mod` tabWidth)

renderSpan :: Int -> Int -> Text
renderSpan line column = Text.pack (show line) <> ":" <> Text.pack (show column)

parseIntLiteral :: Int -> Int -> Text -> Either Text Int
parseIntLiteral line column digits =
  case TextRead.decimal digits :: Either String (Integer, Text) of
    Right (value, trailing)
      | Text.null trailing ->
          if value < minInt || value > maxInt
            then
              -- Parse into Integer first so out-of-range literals become diagnostics
              -- instead of overflowing during conversion to Int.
              Left
                ( "integer literal out of range at "
                    <> renderSpan line column
                    <> ": '"
                    <> digits
                    <> "'"
                )
            else Right (fromInteger value)
      | otherwise ->
          Left
            ( "invalid integer literal '"
                <> digits
                <> "' at "
                <> renderSpan line column
            )
    Left _ ->
      Left
        ( "invalid integer literal '"
            <> digits
            <> "' at "
            <> renderSpan line column
        )
  where
    minInt = toInteger (minBound :: Int)
    maxInt = toInteger (maxBound :: Int)
