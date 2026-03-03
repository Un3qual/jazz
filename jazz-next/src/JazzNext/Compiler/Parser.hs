module JazzNext.Compiler.Parser
  ( parseSurfaceProgram
  ) where

import JazzNext.Compiler.Diagnostics
  ( SourceSpan (..)
  )
import JazzNext.Compiler.Parser.AST
  ( SurfaceExpr (..),
    SurfaceStatement (..)
  )
import JazzNext.Compiler.Parser.Lexer
  ( Token (..),
    TokenKind (..),
    tokenize
  )

-- Parses the current minimal surface language into a scope-wrapped program.
-- Every top-level form is dot-terminated and represented as a statement.
parseSurfaceProgram :: String -> Either String SurfaceExpr
parseSurfaceProgram source = do
  tokens <- tokenize source
  (statements, remaining) <- parseStatementsUntilEnd tokens
  case remaining of
    [] -> Right (SEScope statements)
    token : _ ->
      Left
        ( "unexpected token '"
            ++ tokenLexeme token
            ++ "' at "
            ++ renderSpan (tokenSpan token)
        )

parseStatementsUntilEnd :: [Token] -> Either String ([SurfaceStatement], [Token])
parseStatementsUntilEnd = go []
  where
    go acc [] = Right (reverse acc, [])
    go acc tokens = do
      (statement, remaining) <- parseStatement tokens
      go (statement : acc) remaining

parseStatementsUntilBrace :: [Token] -> Either String ([SurfaceStatement], [Token])
parseStatementsUntilBrace = go []
  where
    go acc [] = Left "expected '}' before end of input"
    go acc allTokens@(token : rest) =
      case tokenKind token of
        TRBrace -> Right (reverse acc, rest)
        _ -> do
          (statement, remaining) <- parseStatement allTokens
          go (statement : acc) remaining

parseStatement :: [Token] -> Either String (SurfaceStatement, [Token])
parseStatement tokens =
  case tokens of
    -- Statement-level forms take precedence over expression parsing when the
    -- leading identifier is followed by declaration syntax.
    (nameToken : afterName@(Token {tokenKind = TColonColon} : _))
      | TIdentifier name <- tokenKind nameToken -> parseSignature name nameToken afterName
    (nameToken : afterName@(Token {tokenKind = TEquals} : _))
      | TIdentifier name <- tokenKind nameToken -> parseLet name nameToken afterName
    _ -> parseExprStatement tokens

parseSignature :: String -> Token -> [Token] -> Either String (SurfaceStatement, [Token])
parseSignature name nameToken tokensAfterName =
  case tokensAfterName of
    Token {tokenKind = TColonColon} : rest -> do
      (signatureTokens, remainingAfterDot) <- collectUntilDot rest
      let signatureText = unwords (map tokenLexeme signatureTokens)
      pure
        ( SSSignature name (tokenSpan nameToken) signatureText,
          remainingAfterDot
        )
    _ ->
      Left
        ( "internal parser error at "
            ++ renderSpan (tokenSpan nameToken)
            ++ ": expected '::' after signature name"
        )

parseLet :: String -> Token -> [Token] -> Either String (SurfaceStatement, [Token])
parseLet name nameToken tokensAfterName =
  case tokensAfterName of
    Token {tokenKind = TEquals} : rest -> do
      (valueExpr, afterExpr) <- parseExpr rest
      remaining <- consumeDot afterExpr
      pure (SSLet name (tokenSpan nameToken) valueExpr, remaining)
    _ ->
      Left
        ( "internal parser error at "
            ++ renderSpan (tokenSpan nameToken)
            ++ ": expected '=' after binding name"
        )

parseExprStatement :: [Token] -> Either String (SurfaceStatement, [Token])
parseExprStatement tokens = do
  case tokens of
    [] -> Left "expected expression before end of input"
    firstToken : _ -> do
      (expr, afterExpr) <- parseExpr tokens
      remaining <- consumeDot afterExpr
      pure (SSExpr (tokenSpan firstToken) expr, remaining)

parseExpr :: [Token] -> Either String (SurfaceExpr, [Token])
parseExpr tokens =
  case tokens of
    [] -> Left "expected expression before end of input"
    token : rest ->
      case tokenKind token of
        TInt value -> Right (SEInt value, rest)
        TIdentifier name -> Right (SEVar name, rest)
        TLParen -> do
          -- Parentheses are grouping only; the parser returns the inner node.
          (innerExpr, afterInner) <- parseExpr rest
          remaining <- consumeRightParen afterInner
          Right (innerExpr, remaining)
        TLBrace -> do
          (statements, afterBrace) <- parseStatementsUntilBrace rest
          Right (SEScope statements, afterBrace)
        _ ->
          Left
            ( "unexpected token '"
                ++ tokenLexeme token
                ++ "' at "
                ++ renderSpan (tokenSpan token)
                ++ "; expected expression"
            )

collectUntilDot :: [Token] -> Either String ([Token], [Token])
collectUntilDot = go []
  where
    -- Type signatures currently keep the type text as raw tokens joined by
    -- spaces. This helper stops exactly at the signature terminator and guards
    -- against accidentally consuming the next statement start.
    go acc [] = Left "expected '.' before end of input"
    go acc allTokens@(token : rest) =
      case tokenKind token of
        TDot
          | null acc ->
              Left
                ( "expected signature text before '.' at "
                    ++ renderSpan (tokenSpan token)
                )
          | otherwise -> Right (reverse acc, rest)
        _
          | not (null acc) && beginsStatement allTokens ->
              Left
                ( "expected '.' before '"
                    ++ tokenLexeme token
                    ++ "' at "
                    ++ renderSpan (tokenSpan token)
                )
          | otherwise -> go (token : acc) rest

    beginsStatement :: [Token] -> Bool
    beginsStatement tokens =
      case tokens of
        Token {tokenKind = TIdentifier _} : Token {tokenKind = TEquals} : _ -> True
        Token {tokenKind = TIdentifier _} : Token {tokenKind = TColonColon} : _ -> True
        _ -> False

consumeDot :: [Token] -> Either String [Token]
consumeDot tokens =
  case tokens of
    Token {tokenKind = TDot} : rest -> Right rest
    [] -> Left "expected '.' before end of input"
    token : _ ->
      Left
        ( "expected '.' at "
            ++ renderSpan (tokenSpan token)
            ++ ", found '"
            ++ tokenLexeme token
            ++ "'"
        )

consumeRightParen :: [Token] -> Either String [Token]
consumeRightParen tokens =
  case tokens of
    Token {tokenKind = TRParen} : rest -> Right rest
    [] -> Left "expected ')' before end of input"
    token : _ ->
      Left
        ( "expected ')' at "
            ++ renderSpan (tokenSpan token)
            ++ ", found '"
            ++ tokenLexeme token
            ++ "'"
        )

renderSpan :: SourceSpan -> String
renderSpan spanValue = show (spanLine spanValue) ++ ":" ++ show (spanColumn spanValue)
