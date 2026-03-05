{-# LANGUAGE OverloadedStrings #-}

module JazzNext.Compiler.Parser
  ( parseSurfaceProgram
  ) where

import Data.Text (Text)
import qualified Data.Text as Text
import JazzNext.Compiler.Diagnostics
  ( SourceSpan (..),
    renderSourceSpan
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
import JazzNext.Compiler.Parser.Operator
  ( Associativity (..),
    OperatorInfo (..),
    lookupOperatorInfo
  )

-- Parses the current minimal surface language into a scope-wrapped program.
-- Every top-level form is dot-terminated and represented as a statement.
parseSurfaceProgram :: Text -> Either Text SurfaceExpr
parseSurfaceProgram source = do
  tokens <- tokenize source
  (statements, remaining) <- parseStatementsUntilEnd tokens
  case remaining of
    [] -> Right (SEScope statements)
    token : _ ->
      Left
        ( "unexpected token '"
            <> tokenLexeme token
            <> "' at "
            <> renderSourceSpan (tokenSpan token)
        )

parseStatementsUntilEnd :: [Token] -> Either Text ([SurfaceStatement], [Token])
parseStatementsUntilEnd = go []
  where
    go acc [] = Right (reverse acc, [])
    go acc tokens = do
      (statement, remaining) <- parseStatement tokens
      go (statement : acc) remaining

parseStatementsUntilBrace :: [Token] -> Either Text ([SurfaceStatement], [Token])
parseStatementsUntilBrace = go []
  where
    go _ [] = Left "expected '}' before end of input"
    go acc allTokens@(token : rest) =
      case tokenKind token of
        TRBrace -> Right (reverse acc, rest)
        _ -> do
          (statement, remaining) <- parseStatement allTokens
          go (statement : acc) remaining

parseStatement :: [Token] -> Either Text (SurfaceStatement, [Token])
parseStatement tokens =
  case tokens of
    moduleToken@(Token {tokenKind = TModule}) : rest ->
      parseModuleStatement moduleToken rest
    importToken@(Token {tokenKind = TImport}) : rest ->
      parseImportStatement importToken rest
    -- Statement-level forms take precedence over expression parsing when the
    -- leading identifier is followed by declaration syntax.
    (nameToken : afterName@(Token {tokenKind = TColonColon} : _))
      | TIdentifier name <- tokenKind nameToken,
        isReservedLiteralName name ->
          Left
            ( "reserved literal '"
                <> name
                <> "' cannot be used as a binding name at "
                <> renderSourceSpan (tokenSpan nameToken)
            )
      | TIdentifier name <- tokenKind nameToken -> parseSignature name nameToken afterName
    (nameToken : afterName@(Token {tokenKind = TEquals} : _))
      | TIdentifier name <- tokenKind nameToken,
        isReservedLiteralName name ->
          Left
            ( "reserved literal '"
                <> name
                <> "' cannot be used as a binding name at "
                <> renderSourceSpan (tokenSpan nameToken)
            )
      | TIdentifier name <- tokenKind nameToken -> parseLet name nameToken afterName
    _ -> parseExprStatement tokens

isReservedLiteralName :: Text -> Bool
isReservedLiteralName name = name == "True" || name == "False"

parseModuleStatement :: Token -> [Token] -> Either Text (SurfaceStatement, [Token])
parseModuleStatement moduleToken tokensAfterModuleKeyword = do
  (modulePath, afterModulePath) <- parseModulePath tokensAfterModuleKeyword
  remaining <- consumeDot afterModulePath
  pure (SSModule (tokenSpan moduleToken) modulePath, remaining)

parseImportStatement :: Token -> [Token] -> Either Text (SurfaceStatement, [Token])
parseImportStatement importToken tokensAfterImportKeyword = do
  (modulePath, afterModulePath) <- parseModulePath tokensAfterImportKeyword
  parseImportTail importToken modulePath afterModulePath

parseImportTail :: Token -> [Text] -> [Token] -> Either Text (SurfaceStatement, [Token])
parseImportTail importToken modulePath tokensAfterModulePath =
  case tokensAfterModulePath of
    Token {tokenKind = TDot} : rest ->
      pure (SSImport (tokenSpan importToken) modulePath Nothing Nothing, rest)
    Token {tokenKind = TAs} : rest ->
      case rest of
        aliasToken@(Token {tokenKind = TIdentifier aliasName}) : afterAlias -> do
          case afterAlias of
            Token {tokenKind = TLParen} : _ ->
              Left
                ( "cannot combine import alias and symbol list at "
                    <> renderSourceSpan (tokenSpan aliasToken)
                )
            _ -> do
              remaining <- consumeDot afterAlias
              pure
                ( SSImport
                    (tokenSpan importToken)
                    modulePath
                    (Just aliasName)
                    Nothing,
                  remaining
                )
        [] ->
          Left
            ( "expected import alias before end of input after 'as' at "
                <> renderSourceSpan (tokenSpan importToken)
            )
        token : _ ->
          Left
            ( "expected import alias at "
                <> renderSourceSpan (tokenSpan token)
                <> ", found '"
                <> tokenLexeme token
                <> "'"
            )
    Token {tokenKind = TLParen} : rest -> do
      (symbols, afterSymbols) <- parseImportSymbolList rest
      remaining <- consumeDot afterSymbols
      pure
        ( SSImport
            (tokenSpan importToken)
            modulePath
            Nothing
            (Just symbols),
          remaining
        )
    [] ->
      Left
        ( "expected '.', 'as', or '(' before end of input after import path at "
            <> renderSourceSpan (tokenSpan importToken)
        )
    token : _ ->
      Left
        ( "expected '.', 'as', or '(' at "
            <> renderSourceSpan (tokenSpan token)
            <> ", found '"
            <> tokenLexeme token
            <> "'"
        )

parseModulePath :: [Token] -> Either Text ([Text], [Token])
parseModulePath tokens =
  case tokens of
    [] -> Left "expected module path before end of input"
    Token {tokenKind = TIdentifier firstSegment} : rest ->
      go [firstSegment] rest
      where
        go segments allTokens =
          case allTokens of
            Token {tokenKind = TColonColon} : Token {tokenKind = TIdentifier nextSegment} : remaining ->
              go (segments ++ [nextSegment]) remaining
            separatorToken@(Token {tokenKind = TColonColon}) : [] ->
              Left
                ( "expected module path segment before end of input at "
                    <> renderSourceSpan (tokenSpan separatorToken)
                )
            separatorToken@(Token {tokenKind = TColonColon}) : token : _
              | tokenKind token == TDot ->
                  Left
                    ( "expected module path segment at "
                        <> renderSourceSpan (tokenSpan separatorToken)
                        <> ", found '"
                        <> tokenLexeme separatorToken
                        <> "'"
                    )
              | otherwise ->
                  Left
                    ( "expected module path segment at "
                        <> renderSourceSpan (tokenSpan token)
                        <> ", found '"
                        <> tokenLexeme token
                        <> "'"
                    )
            _ -> Right (segments, allTokens)
    token : _ ->
      Left
        ( "expected module path segment at "
            <> renderSourceSpan (tokenSpan token)
            <> ", found '"
            <> tokenLexeme token
            <> "'"
        )

parseImportSymbolList :: [Token] -> Either Text ([Text], [Token])
parseImportSymbolList tokensAfterLeftParen =
  case tokensAfterLeftParen of
    Token {tokenKind = TRParen} : _ ->
      Left "expected at least one import symbol before ')'"
    _ -> do
      (firstSymbol, afterFirstSymbol) <- parseImportSymbol tokensAfterLeftParen
      go [firstSymbol] afterFirstSymbol
  where
    go symbols allTokens =
      case allTokens of
        Token {tokenKind = TComma} : rest -> do
          (nextSymbol, afterNextSymbol) <- parseImportSymbol rest
          go (symbols ++ [nextSymbol]) afterNextSymbol
        Token {tokenKind = TRParen} : rest ->
          Right (symbols, rest)
        [] ->
          Left "expected ')' before end of input in import symbol list"
        token : _ ->
          Left
            ( "expected ',' or ')' at "
                <> renderSourceSpan (tokenSpan token)
                <> ", found '"
                <> tokenLexeme token
                <> "'"
            )

parseImportSymbol :: [Token] -> Either Text (Text, [Token])
parseImportSymbol tokens =
  case tokens of
    Token {tokenKind = TIdentifier symbolName} : rest ->
      Right (symbolName, rest)
    [] ->
      Left "expected import symbol before end of input"
    token : _ ->
      Left
        ( "expected import symbol at "
            <> renderSourceSpan (tokenSpan token)
            <> ", found '"
            <> tokenLexeme token
            <> "'"
        )

parseSignature :: Text -> Token -> [Token] -> Either Text (SurfaceStatement, [Token])
parseSignature name nameToken tokensAfterName =
  case tokensAfterName of
    Token {tokenKind = TColonColon} : rest -> do
      (signatureTokens, remainingAfterDot) <- collectUntilDot rest
      let signatureText = Text.unwords (map tokenLexeme signatureTokens)
      pure
        ( SSSignature name (tokenSpan nameToken) signatureText,
          remainingAfterDot
        )
    _ ->
      Left
        ( "internal parser error at "
            <> renderSourceSpan (tokenSpan nameToken)
            <> ": expected '::' after signature name"
        )

parseLet :: Text -> Token -> [Token] -> Either Text (SurfaceStatement, [Token])
parseLet name nameToken tokensAfterName =
  case tokensAfterName of
    Token {tokenKind = TEquals} : rest -> do
      (valueExpr, afterExpr) <- parseExpr rest
      remaining <- consumeDot afterExpr
      pure (SSLet name (tokenSpan nameToken) valueExpr, remaining)
    _ ->
      Left
        ( "internal parser error at "
            <> renderSourceSpan (tokenSpan nameToken)
            <> ": expected '=' after binding name"
        )

parseExprStatement :: [Token] -> Either Text (SurfaceStatement, [Token])
parseExprStatement tokens = do
  case tokens of
    [] -> Left "expected expression before end of input"
    firstToken : _ -> do
      (expr, afterExpr) <- parseExpr tokens
      remaining <- consumeDot afterExpr
      pure (SSExpr (tokenSpan firstToken) expr, remaining)

parseExpr :: [Token] -> Either Text (SurfaceExpr, [Token])
parseExpr = parseExprWithMinPrecedence 1

parseExprWithMinPrecedence :: Int -> [Token] -> Either Text (SurfaceExpr, [Token])
parseExprWithMinPrecedence minPrecedence tokens = do
  (leftExpr, remainingTokens) <- parseApplicationExpr tokens
  parseInfixTailWith parseExprWithMinPrecedence minPrecedence leftExpr remainingTokens

-- Used by `if` parsing to preserve the existing compact `if cond then else`
-- surface form without introducing a `then` delimiter.
parseExprWithoutApplication :: [Token] -> Either Text (SurfaceExpr, [Token])
parseExprWithoutApplication = parseExprWithoutApplicationWithMinPrecedence 1

parseExprWithoutApplicationWithMinPrecedence :: Int -> [Token] -> Either Text (SurfaceExpr, [Token])
parseExprWithoutApplicationWithMinPrecedence minPrecedence tokens = do
  (leftExpr, remainingTokens) <- parsePrimaryExpr tokens
  parseInfixTailWith parseExprWithoutApplicationWithMinPrecedence minPrecedence leftExpr remainingTokens

parseApplicationExpr :: [Token] -> Either Text (SurfaceExpr, [Token])
parseApplicationExpr tokens = do
  (functionExpr, remainingTokens) <- parsePrimaryExpr tokens
  parseApplicationTail functionExpr remainingTokens

parseApplicationTail :: SurfaceExpr -> [Token] -> Either Text (SurfaceExpr, [Token])
parseApplicationTail functionExpr tokens =
  case startsPrimaryExpr tokens of
    True -> do
      (argumentExpr, remainingAfterArgument) <- parsePrimaryExpr tokens
      parseApplicationTail (SEApply functionExpr argumentExpr) remainingAfterArgument
    False -> Right (functionExpr, tokens)
  where
    startsPrimaryExpr allTokens =
      case allTokens of
        Token {tokenKind = TInt _} : _ -> True
        Token {tokenKind = TIdentifier _} : _ -> True
        Token {tokenKind = TIf} : _ -> True
        Token {tokenKind = TLParen} : _ -> True
        Token {tokenKind = TLBrace} : _ -> True
        Token {tokenKind = TLBracket} : _ -> True
        _ -> False

parseInfixTailWith ::
  (Int -> [Token] -> Either Text (SurfaceExpr, [Token])) ->
  Int ->
  SurfaceExpr ->
  [Token] ->
  Either Text (SurfaceExpr, [Token])
-- `if` conditions need infix parsing without enabling application; injecting
-- the RHS parser keeps the boundary explicit for both modes.
parseInfixTailWith parseRhs minPrecedence leftExpr tokens =
  case tokens of
    operatorToken@(Token {tokenKind = TOperator operatorSymbol}) : tokensAfterOperator
      | shouldStopForSectionBoundary tokensAfterOperator ->
          Right (leftExpr, tokens)
      | otherwise ->
          case lookupOperatorInfo operatorSymbol of
            Nothing ->
              Left
                ( "unsupported operator '"
                    <> operatorSymbol
                    <> "' at "
                    <> renderSourceSpan (tokenSpan operatorToken)
                )
            Just operatorInfo
              | operatorPrecedence operatorInfo < minPrecedence ->
                  Right (leftExpr, tokens)
              | otherwise -> do
                  let nextMinPrecedence =
                        case operatorAssociativity operatorInfo of
                          AssocLeft -> operatorPrecedence operatorInfo + 1
                          AssocRight -> operatorPrecedence operatorInfo
                  (rightExpr, remainingAfterRight) <-
                    parseRhs nextMinPrecedence tokensAfterOperator
                  parseInfixTailWith
                    parseRhs
                    minPrecedence
                    (SEBinary operatorSymbol leftExpr rightExpr)
                    remainingAfterRight
    _ -> Right (leftExpr, tokens)
  where
    shouldStopForSectionBoundary remainingAfterOperator =
      case remainingAfterOperator of
        Token {tokenKind = TRParen} : _ -> True
        _ -> False

parsePrimaryExpr :: [Token] -> Either Text (SurfaceExpr, [Token])
parsePrimaryExpr tokens =
  case tokens of
    [] -> Left "expected expression before end of input"
    token : rest ->
      case tokenKind token of
        TInt value -> Right (SEInt value, rest)
        TIdentifier name ->
          case name of
            "True" -> Right (SEBool True, rest)
            "False" -> Right (SEBool False, rest)
            _ -> Right (SEVar name, rest)
        TIf -> parseIfExpr token rest
        TLParen -> parseParenExpr rest
        TLBrace -> do
          (statements, afterBrace) <- parseStatementsUntilBrace rest
          Right (SEScope statements, afterBrace)
        TLBracket -> parseListExpr rest
        _ ->
          Left
            ( "unexpected token '"
                <> tokenLexeme token
                <> "' at "
                <> renderSourceSpan (tokenSpan token)
                <> "; expected expression"
            )

parseParenExpr :: [Token] -> Either Text (SurfaceExpr, [Token])
parseParenExpr tokensAfterLeftParen =
  case tokensAfterLeftParen of
    Token {tokenKind = TOperator operatorSymbol} : rest -> do
      (rightExpr, afterRightExpr) <- parseExpr rest
      remaining <- consumeRightParen afterRightExpr
      pure (SESectionRight operatorSymbol rightExpr, remaining)
    _ -> do
      (innerExpr, afterInnerExpr) <- parseExpr tokensAfterLeftParen
      case afterInnerExpr of
        Token {tokenKind = TOperator operatorSymbol} : Token {tokenKind = TRParen} : rest ->
          Right (SESectionLeft innerExpr operatorSymbol, rest)
        _ -> do
          remaining <- consumeRightParen afterInnerExpr
          Right (innerExpr, remaining)

parseListExpr :: [Token] -> Either Text (SurfaceExpr, [Token])
parseListExpr tokensAfterLeftBracket =
  case tokensAfterLeftBracket of
    Token {tokenKind = TRBracket} : rest ->
      Right (SEList [], rest)
    _ -> do
      (elements, afterElements) <- parseListElements tokensAfterLeftBracket
      remaining <- consumeRightBracket afterElements
      Right (SEList elements, remaining)

parseListElements :: [Token] -> Either Text ([SurfaceExpr], [Token])
parseListElements tokens = do
  (firstElement, remainingAfterFirst) <- parseExpr tokens
  go [firstElement] remainingAfterFirst
  where
    go elements allTokens =
      case allTokens of
        Token {tokenKind = TComma} : rest -> do
          (nextElement, remainingAfterNext) <- parseExpr rest
          go (nextElement : elements) remainingAfterNext
        _ ->
          Right (reverse elements, allTokens)

parseIfExpr :: Token -> [Token] -> Either Text (SurfaceExpr, [Token])
parseIfExpr ifToken tokensAfterIf = do
  (conditionExpr, afterCondition) <- parseExprWithoutApplication tokensAfterIf
  (thenExpr, afterThenExpr) <- parseExpr afterCondition
  case afterThenExpr of
    Token {tokenKind = TElse} : afterElse -> do
      (elseExpr, remaining) <- parseExpr afterElse
      pure (SEIf conditionExpr thenExpr elseExpr, remaining)
    [] ->
      Left
        ( "expected 'else' before end of input after 'if' at "
            <> renderSourceSpan (tokenSpan ifToken)
        )
    token : _ ->
      Left
        ( "expected 'else' at "
            <> renderSourceSpan (tokenSpan token)
            <> ", found '"
            <> tokenLexeme token
            <> "'"
        )

collectUntilDot :: [Token] -> Either Text ([Token], [Token])
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
                    <> renderSourceSpan (tokenSpan token)
                )
          | otherwise -> Right (reverse acc, rest)
        _
          | not (null acc) && beginsStatement allTokens ->
              Left
                ( "expected '.' before '"
                    <> tokenLexeme token
                    <> "' at "
                    <> renderSourceSpan (tokenSpan token)
                )
          | otherwise -> go (token : acc) rest

    beginsStatement :: [Token] -> Bool
    beginsStatement tokens =
      case tokens of
        Token {tokenKind = TModule} : _ -> True
        Token {tokenKind = TImport} : _ -> True
        Token {tokenKind = TIdentifier _} : Token {tokenKind = TEquals} : _ -> True
        Token {tokenKind = TIdentifier _} : Token {tokenKind = TColonColon} : _ -> True
        _ -> False

consumeDot :: [Token] -> Either Text [Token]
consumeDot tokens =
  case tokens of
    Token {tokenKind = TDot} : rest -> Right rest
    [] -> Left "expected '.' before end of input"
    token : _ ->
      Left
        ( "expected '.' at "
            <> renderSourceSpan (tokenSpan token)
            <> ", found '"
            <> tokenLexeme token
            <> "'"
        )

consumeRightParen :: [Token] -> Either Text [Token]
consumeRightParen tokens =
  case tokens of
    Token {tokenKind = TRParen} : rest -> Right rest
    [] -> Left "expected ')' before end of input"
    token : _ ->
      Left
        ( "expected ')' at "
            <> renderSourceSpan (tokenSpan token)
            <> ", found '"
            <> tokenLexeme token
            <> "'"
        )

consumeRightBracket :: [Token] -> Either Text [Token]
consumeRightBracket tokens =
  case tokens of
    Token {tokenKind = TRBracket} : rest -> Right rest
    [] -> Left "expected ']' before end of input"
    token : _ ->
      Left
        ( "expected ']' at "
            <> renderSourceSpan (tokenSpan token)
            <> ", found '"
            <> tokenLexeme token
            <> "'"
        )
