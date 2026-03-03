module JazzNext.Compiler.Parser.Lexer
  ( Token (..),
    TokenKind (..),
    tokenize
  ) where

import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)
import JazzNext.Compiler.Diagnostics
  ( SourceSpan (..)
  )
import Text.Read (readMaybe)

data TokenKind
  = TIdentifier String
  | TInt Int
  | TEquals
  | TColonColon
  | TDot
  | TLBrace
  | TRBrace
  | TLParen
  | TRParen
  deriving (Eq, Show)

data Token = Token
  { tokenKind :: TokenKind,
    tokenLexeme :: String,
    tokenSpan :: SourceSpan
  }
  deriving (Eq, Show)

tokenize :: String -> Either String [Token]
tokenize = go 1 1
  where
    go :: Int -> Int -> String -> Either String [Token]
    go _ _ [] = Right []
    go line column (char : rest)
      | char == '\n' = go (line + 1) 1 rest
      | isSpace char = go line (column + 1) rest
      | isDigit char =
          let (digits, trailing) = span isDigit (char : rest)
              width = length digits
           in do
                value <- parseIntLiteral line column digits
                let token =
                      Token
                        { tokenKind = TInt value,
                          tokenLexeme = digits,
                          tokenSpan = SourceSpan line column
                        }
                (token :) <$> go line (column + width) trailing
      | isIdentifierStart char =
          let (ident, trailing) = span isIdentifierContinuation (char : rest)
              width = length ident
              token =
                Token
                  { tokenKind = TIdentifier ident,
                    tokenLexeme = ident,
                    tokenSpan = SourceSpan line column
                  }
           in (token :) <$> go line (column + width) trailing
      | otherwise =
          case char of
            ':' ->
              case rest of
                ':' : after ->
                  withSingleToken TColonColon "::" 2 line column after
                _ ->
                  Left ("unexpected ':' at " ++ renderSpan line column ++ "; expected '::'")
            '=' -> withSingleToken TEquals "=" 1 line column rest
            '.' -> withSingleToken TDot "." 1 line column rest
            '{' -> withSingleToken TLBrace "{" 1 line column rest
            '}' -> withSingleToken TRBrace "}" 1 line column rest
            '(' -> withSingleToken TLParen "(" 1 line column rest
            ')' -> withSingleToken TRParen ")" 1 line column rest
            _ ->
              Left
                ( "unexpected character '"
                    ++ [char]
                    ++ "' at "
                    ++ renderSpan line column
                )

    withSingleToken ::
      TokenKind ->
      String ->
      Int ->
      Int ->
      Int ->
      String ->
      Either String [Token]
    withSingleToken kind lexeme width line column trailing =
      let token =
            Token
              { tokenKind = kind,
                tokenLexeme = lexeme,
                tokenSpan = SourceSpan line column
              }
       in (token :) <$> go line (column + width) trailing

    isIdentifierStart :: Char -> Bool
    isIdentifierStart char = isAlpha char || char == '_'

    isIdentifierContinuation :: Char -> Bool
    isIdentifierContinuation char =
      isAlphaNum char || char == '_' || char == '\'' || char == '!'

renderSpan :: Int -> Int -> String
renderSpan line column = show line ++ ":" ++ show column

parseIntLiteral :: Int -> Int -> String -> Either String Int
parseIntLiteral line column digits =
  case readMaybe digits :: Maybe Integer of
    Nothing ->
      Left
        ( "invalid integer literal '"
            ++ digits
            ++ "' at "
            ++ renderSpan line column
        )
    Just value
      | value < minInt || value > maxInt ->
          Left
            ( "integer literal out of range at "
                ++ renderSpan line column
                ++ ": '"
                ++ digits
                ++ "'"
            )
      | otherwise -> Right (fromInteger value)
  where
    minInt = toInteger (minBound :: Int)
    maxInt = toInteger (maxBound :: Int)
