{-# LANGUAGE OverloadedStrings #-}

-- | Surface parser for the current `jazz-next` language slice. It turns the
-- token stream into a block-wrapped surface AST while enforcing the current
-- statement and operator grammar.
module JazzNext.Compiler.Parser
  ( parseSurfaceProgram
  ) where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Set as Set
import Data.Set (Set)
import JazzNext.Compiler.Diagnostics
  ( Diagnostic,
    SourceSpan (..),
    mkDiagnostic,
    renderSourceSpan
  )
import JazzNext.Compiler.Identifier
  ( Identifier,
    mkIdentifier
  )
import JazzNext.Compiler.Parser.AST
  ( SurfaceExpr (..),
    SurfaceLiteral (..),
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

-- Parses the current minimal surface language into a block-wrapped program.
-- Every top-level form is dot-terminated and represented as a statement.
parseSurfaceProgram :: Text -> Either Diagnostic SurfaceExpr
parseSurfaceProgram source =
  case tokenize source of
    Left err ->
      Left err
    Right tokens ->
      case parseStatementsUntilEnd tokens of
        Left err ->
          Left err
        Right (statements, remaining) ->
          case remaining of
            [] -> Right (SEBlock statements)
            token : _ ->
              Left
                ( parseDiagnostic
                    ( "unexpected token '"
                        <> tokenLexeme token
                        <> "' at "
                        <> renderSourceSpan (tokenSpan token)
                    )
                )

-- | Parse a complete sequence of dot-terminated statements until the token
-- stream is exhausted.
parseStatementsUntilEnd :: [Token] -> Either Diagnostic ([SurfaceStatement], [Token])
parseStatementsUntilEnd = go []
  where
    go acc [] = Right (reverse acc, [])
    go acc tokens = do
      (statement, remaining) <- parseStatement tokens
      go (statement : acc) remaining

-- | Parse statements inside `{ ... }`, stopping as soon as the closing brace is
-- encountered so block parsing can hand the remaining tokens back to callers.
parseStatementsUntilBrace :: [Token] -> Either Diagnostic ([SurfaceStatement], [Token])
parseStatementsUntilBrace = go []
  where
    go _ [] = Left (parseDiagnostic "expected '}' before end of input")
    go acc allTokens@(token : rest) =
      case tokenKind token of
        TRBrace -> Right (reverse acc, rest)
        _ -> do
          (statement, remaining) <- parseStatement allTokens
          go (statement : acc) remaining

-- | Disambiguate statement-level forms before expression parsing so leading
-- identifiers can become signatures or bindings when followed by `::` or `=`.
parseStatement :: [Token] -> Either Diagnostic (SurfaceStatement, [Token])
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
            ( parseDiagnostic
                ( "reserved literal '"
                    <> name
                    <> "' cannot be used as a binding name at "
                    <> renderSourceSpan (tokenSpan nameToken)
                )
            )
      | TIdentifier name <- tokenKind nameToken -> parseSignature (mkIdentifier name) nameToken afterName
    (nameToken : afterName@(Token {tokenKind = TEquals} : _))
      | TIdentifier name <- tokenKind nameToken,
        isReservedLiteralName name ->
          Left
            ( parseDiagnostic
                ( "reserved literal '"
                    <> name
                    <> "' cannot be used as a binding name at "
                    <> renderSourceSpan (tokenSpan nameToken)
                )
            )
      | TIdentifier name <- tokenKind nameToken -> parseLet (mkIdentifier name) nameToken afterName
    _ -> parseExprStatement tokens

isReservedLiteralName :: Text -> Bool
isReservedLiteralName name = name == "True" || name == "False"

parseModuleStatement :: Token -> [Token] -> Either Diagnostic (SurfaceStatement, [Token])
parseModuleStatement moduleToken tokensAfterModuleKeyword = do
  (modulePath, afterModulePath) <- parseModulePath tokensAfterModuleKeyword
  remaining <- consumeDot afterModulePath
  pure (SSModule (tokenSpan moduleToken) modulePath, remaining)

parseImportStatement :: Token -> [Token] -> Either Diagnostic (SurfaceStatement, [Token])
parseImportStatement importToken tokensAfterImportKeyword = do
  (modulePath, afterModulePath) <- parseModulePath tokensAfterImportKeyword
  parseImportTail importToken modulePath afterModulePath

parseImportTail :: Token -> [Text] -> [Token] -> Either Diagnostic (SurfaceStatement, [Token])
parseImportTail importToken modulePath tokensAfterModulePath =
  case tokensAfterModulePath of
    Token {tokenKind = TDot} : rest ->
      pure (SSImport (tokenSpan importToken) modulePath Nothing Nothing, rest)
    asToken@(Token {tokenKind = TAs}) : rest ->
      case rest of
        aliasToken@(Token {tokenKind = TIdentifier aliasName}) : afterAlias -> do
          case afterAlias of
            parenToken@(Token {tokenKind = TLParen}) : _ ->
              Left
                ( parseDiagnostic
                    ( "cannot combine import alias and symbol list at "
                        <> renderSourceSpan (tokenSpan parenToken)
                    )
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
            ( parseDiagnostic
                ( "expected import alias before end of input after 'as' at "
                    <> renderSourceSpan (tokenSpan asToken)
                )
            )
        token : _ ->
          Left
            ( parseDiagnostic
                ( "expected import alias at "
                    <> renderSourceSpan (tokenSpan token)
                    <> ", found '"
                    <> tokenLexeme token
                    <> "'"
                )
            )
    Token {tokenKind = TLParen} : rest -> do
      (symbols, afterSymbols) <- parseImportSymbolList rest
      case afterSymbols of
        asToken@(Token {tokenKind = TAs}) : _ ->
          Left
            ( parseDiagnostic
                ( "cannot combine import alias and symbol list at "
                    <> renderSourceSpan (tokenSpan asToken)
                )
            )
        _ -> do
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
        ( parseDiagnostic
            ( "expected '.', 'as', or '(' before end of input after import path at "
                <> renderSourceSpan (tokenSpan importToken)
            )
        )
    token : _ ->
      Left
        ( parseDiagnostic
            ( "expected '.', 'as', or '(' at "
                <> renderSourceSpan (tokenSpan token)
                <> ", found '"
                <> tokenLexeme token
                <> "'"
            )
        )

-- | Parse `Foo::Bar` style module paths and leave the first non-path token
-- untouched for the caller.
parseModulePath :: [Token] -> Either Diagnostic ([Text], [Token])
parseModulePath tokens =
  case tokens of
    [] -> Left (parseDiagnostic "expected module path before end of input")
    Token {tokenKind = TIdentifier firstSegment} : rest ->
      go [firstSegment] rest
      where
        -- Accumulate in reverse to avoid repeated list appends.
        go revSegments allTokens =
          case allTokens of
            Token {tokenKind = TColonColon} : Token {tokenKind = TIdentifier nextSegment} : remaining ->
              go (nextSegment : revSegments) remaining
            separatorToken@(Token {tokenKind = TColonColon}) : [] ->
              Left
                ( parseDiagnostic
                    ( "expected module path segment before end of input at "
                        <> renderSourceSpan (tokenSpan separatorToken)
                    )
                )
            separatorToken@(Token {tokenKind = TColonColon}) : token : _
              | tokenKind token == TDot ->
                  Left
                    ( parseDiagnostic
                        ( "expected module path segment at "
                            <> renderSourceSpan (tokenSpan separatorToken)
                            <> ", found '"
                            <> tokenLexeme token
                            <> "'"
                        )
                    )
              | otherwise ->
                  Left
                    ( parseDiagnostic
                        ( "expected module path segment at "
                            <> renderSourceSpan (tokenSpan token)
                            <> ", found '"
                            <> tokenLexeme token
                            <> "'"
                        )
                    )
            _ -> Right (reverse revSegments, allTokens)
    token : _ ->
      Left
        ( parseDiagnostic
            ( "expected module path segment at "
                <> renderSourceSpan (tokenSpan token)
                <> ", found '"
                <> tokenLexeme token
                <> "'"
            )
        )

-- | Parse import symbol lists and reject duplicates immediately so later module
-- resolution can assume the list is unique.
parseImportSymbolList :: [Token] -> Either Diagnostic ([Text], [Token])
parseImportSymbolList tokensAfterLeftParen =
  case tokensAfterLeftParen of
    token@(Token {tokenKind = TRParen}) : _ ->
      Left
        ( parseDiagnostic
            ( "expected at least one import symbol before ')' at "
                <> renderSourceSpan (tokenSpan token)
            )
        )
    _ -> do
      (firstSymbol, firstSpan, afterFirstSymbol) <- parseImportSymbol tokensAfterLeftParen
      go [firstSymbol] (Set.singleton firstSymbol) afterFirstSymbol
  where
    -- Accumulate in reverse to keep symbol-list parsing linear.
    go revSymbols seenSymbols allTokens =
      case allTokens of
        Token {tokenKind = TComma} : rest -> do
          (nextSymbol, symbolSpan, afterNextSymbol) <- parseImportSymbol rest
          if Set.member nextSymbol seenSymbols
            then
              Left
                ( parseDiagnostic
                    ( "duplicate import symbol '"
                        <> nextSymbol
                        <> "' at "
                        <> renderSourceSpan symbolSpan
                    )
                )
            else
              go
                (nextSymbol : revSymbols)
                (Set.insert nextSymbol seenSymbols)
                afterNextSymbol
        Token {tokenKind = TRParen} : rest ->
          Right (reverse revSymbols, rest)
        [] ->
          Left (parseDiagnostic "expected ')' before end of input in import symbol list")
        token : _ ->
          Left
            ( parseDiagnostic
                ( "expected ',' or ')' at "
                    <> renderSourceSpan (tokenSpan token)
                    <> ", found '"
                    <> tokenLexeme token
                    <> "'"
                )
            )

parseImportSymbol :: [Token] -> Either Diagnostic (Text, SourceSpan, [Token])
parseImportSymbol tokens =
  case tokens of
    Token {tokenKind = TIdentifier symbolName, tokenSpan = symbolSpan} : rest ->
      Right (symbolName, symbolSpan, rest)
    [] ->
      Left (parseDiagnostic "expected import symbol before end of input")
    token : _ ->
      Left
        ( parseDiagnostic
            ( "expected import symbol at "
                <> renderSourceSpan (tokenSpan token)
                <> ", found '"
                <> tokenLexeme token
                <> "'"
            )
        )
parseSignature :: Identifier -> Token -> [Token] -> Either Diagnostic (SurfaceStatement, [Token])
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
        ( parseDiagnostic
            ( "internal parser error at "
                <> renderSourceSpan (tokenSpan nameToken)
                <> ": expected '::' after signature name"
            )
        )

parseLet :: Identifier -> Token -> [Token] -> Either Diagnostic (SurfaceStatement, [Token])
parseLet name nameToken tokensAfterName =
  case tokensAfterName of
    Token {tokenKind = TEquals} : rest -> do
      (valueExpr, afterExpr) <- parseExpr rest
      remaining <- consumeDot afterExpr
      pure (SSLet name (tokenSpan nameToken) valueExpr, remaining)
    _ ->
      Left
        ( parseDiagnostic
            ( "internal parser error at "
                <> renderSourceSpan (tokenSpan nameToken)
                <> ": expected '=' after binding name"
            )
        )

parseExprStatement :: [Token] -> Either Diagnostic (SurfaceStatement, [Token])
parseExprStatement tokens = do
  case tokens of
    [] -> Left (parseDiagnostic "expected expression before end of input")
    firstToken : _ -> do
      (expr, afterExpr) <- parseExpr tokens
      remaining <- consumeDot afterExpr
      pure (SSExpr (tokenSpan firstToken) expr, remaining)

parseExpr :: [Token] -> Either Diagnostic (SurfaceExpr, [Token])
parseExpr = parseExprWithMinPrecedence 1

-- | Entry point for precedence-climbing expression parsing after application
-- folding has already happened.
parseExprWithMinPrecedence :: Int -> [Token] -> Either Diagnostic (SurfaceExpr, [Token])
parseExprWithMinPrecedence minPrecedence tokens = do
  (leftExpr, remainingTokens) <- parseApplicationExpr tokens
  parseInfixTailWith parseExprWithMinPrecedence minPrecedence leftExpr remainingTokens

-- Used by `if` parsing to preserve the existing compact `if cond then else`
-- surface form without introducing a `then` delimiter.
parseExprWithoutApplication :: [Token] -> Either Diagnostic (SurfaceExpr, [Token])
parseExprWithoutApplication = parseExprWithoutApplicationWithMinPrecedence 1

parseExprWithoutApplicationWithMinPrecedence :: Int -> [Token] -> Either Diagnostic (SurfaceExpr, [Token])
parseExprWithoutApplicationWithMinPrecedence minPrecedence tokens = do
  (leftExpr, remainingTokens) <- parsePrimaryExpr tokens
  parseInfixTailWith parseExprWithoutApplicationWithMinPrecedence minPrecedence leftExpr remainingTokens

parseApplicationExpr :: [Token] -> Either Diagnostic (SurfaceExpr, [Token])
parseApplicationExpr tokens = do
  (functionExpr, remainingTokens) <- parsePrimaryExpr tokens
  parseApplicationTail functionExpr remainingTokens

-- | Function application binds tighter than infix operators, so adjacent
-- primary expressions are folded into left-associated applications first.
parseApplicationTail :: SurfaceExpr -> [Token] -> Either Diagnostic (SurfaceExpr, [Token])
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

-- | Shared precedence climber used by both regular expression parsing and the
-- restricted `if` condition parser.
parseInfixTailWith ::
  (Int -> [Token] -> Either Diagnostic (SurfaceExpr, [Token])) ->
  Int ->
  SurfaceExpr ->
  [Token] ->
  Either Diagnostic (SurfaceExpr, [Token])
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
                ( parseDiagnostic
                    ( "unsupported operator '"
                        <> operatorSymbol
                        <> "' at "
                        <> renderSourceSpan (tokenSpan operatorToken)
                    )
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

parsePrimaryExpr :: [Token] -> Either Diagnostic (SurfaceExpr, [Token])
parsePrimaryExpr tokens =
  case tokens of
    [] -> Left (parseDiagnostic "expected expression before end of input")
    token : rest ->
      case tokenKind token of
        TInt value -> Right (SELit (SLInt value), rest)
        TIdentifier name ->
          case name of
            "True" -> Right (SELit (SLBool True), rest)
            "False" -> Right (SELit (SLBool False), rest)
            _ -> Right (SEVar (mkIdentifier name), rest)
        TIf -> parseIfExpr token rest
        TLParen -> parseParenExpr rest
        TLBrace -> do
          (statements, afterBrace) <- parseStatementsUntilBrace rest
          Right (SEBlock statements, afterBrace)
        TLBracket -> parseListExpr rest
        _ ->
          Left
            ( parseDiagnostic
                ( "unexpected token '"
                    <> tokenLexeme token
                    <> "' at "
                    <> renderSourceSpan (tokenSpan token)
                    <> "; expected expression"
                )
            )

-- | Parenthesized forms cover ordinary grouping, operator values like `(+)`,
-- and left/right operator sections.
parseParenExpr :: [Token] -> Either Diagnostic (SurfaceExpr, [Token])
parseParenExpr tokensAfterLeftParen =
  case tokensAfterLeftParen of
    Token {tokenKind = TOperator operatorSymbol} : rest ->
      case rest of
        Token {tokenKind = TRParen} : remaining ->
          Right (SEOperatorValue operatorSymbol, remaining)
        _ -> do
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

parseListExpr :: [Token] -> Either Diagnostic (SurfaceExpr, [Token])
parseListExpr tokensAfterLeftBracket =
  case tokensAfterLeftBracket of
    Token {tokenKind = TRBracket} : rest ->
      Right (SEList [], rest)
    _ -> do
      (elements, afterElements) <- parseListElements tokensAfterLeftBracket
      remaining <- consumeRightBracket afterElements
      Right (SEList elements, remaining)

parseListElements :: [Token] -> Either Diagnostic ([SurfaceExpr], [Token])
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

-- | Parse the compact `if cond thenExpr else elseExpr` surface form.
parseIfExpr :: Token -> [Token] -> Either Diagnostic (SurfaceExpr, [Token])
parseIfExpr ifToken tokensAfterIf = do
  (conditionExpr, afterCondition) <- parseExprWithoutApplication tokensAfterIf
  (thenExpr, afterThenExpr) <- parseExpr afterCondition
  case afterThenExpr of
    Token {tokenKind = TElse} : afterElse -> do
      (elseExpr, remaining) <- parseExpr afterElse
      pure (SEIf conditionExpr thenExpr elseExpr, remaining)
    [] ->
      Left
        ( parseDiagnostic
            ( "expected 'else' before end of input after 'if' at "
                <> renderSourceSpan (tokenSpan ifToken)
            )
        )
    token : _ ->
      Left
        ( parseDiagnostic
            ( "expected 'else' at "
                <> renderSourceSpan (tokenSpan token)
                <> ", found '"
                <> tokenLexeme token
                <> "'"
            )
        )

collectUntilDot :: [Token] -> Either Diagnostic ([Token], [Token])
collectUntilDot = go []
  where
    -- Type signatures currently keep the type text as raw tokens joined by
    -- spaces. This helper stops exactly at the signature terminator and guards
    -- against accidentally consuming the next statement start.
    go acc [] = Left (parseDiagnostic "expected '.' before end of input")
    go acc allTokens@(token : rest) =
      case tokenKind token of
        TDot
          | null acc ->
              Left
                ( parseDiagnostic
                    ( "expected signature text before '.' at "
                        <> renderSourceSpan (tokenSpan token)
                    )
                )
          | otherwise -> Right (reverse acc, rest)
        _
          | not (null acc) && beginsStatement allTokens ->
              Left
                ( parseDiagnostic
                    ( "expected '.' before '"
                        <> tokenLexeme token
                        <> "' at "
                        <> renderSourceSpan (tokenSpan token)
                    )
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

consumeDot :: [Token] -> Either Diagnostic [Token]
consumeDot tokens =
  case tokens of
    Token {tokenKind = TDot} : rest -> Right rest
    [] -> Left (parseDiagnostic "expected '.' before end of input")
    token : _ ->
      Left
        ( parseDiagnostic
            ( "expected '.' at "
                <> renderSourceSpan (tokenSpan token)
                <> ", found '"
                <> tokenLexeme token
                <> "'"
            )
        )

consumeRightParen :: [Token] -> Either Diagnostic [Token]
consumeRightParen tokens =
  case tokens of
    Token {tokenKind = TRParen} : rest -> Right rest
    [] -> Left (parseDiagnostic "expected ')' before end of input")
    token : _ ->
      Left
        ( parseDiagnostic
            ( "expected ')' at "
                <> renderSourceSpan (tokenSpan token)
                <> ", found '"
                <> tokenLexeme token
                <> "'"
            )
        )

consumeRightBracket :: [Token] -> Either Diagnostic [Token]
consumeRightBracket tokens =
  case tokens of
    Token {tokenKind = TRBracket} : rest -> Right rest
    [] -> Left (parseDiagnostic "expected ']' before end of input")
    token : _ ->
      Left
        ( parseDiagnostic
            ( "expected ']' at "
                <> renderSourceSpan (tokenSpan token)
                <> ", found '"
                <> tokenLexeme token
                <> "'"
            )
        )

parseDiagnostic :: Text -> Diagnostic
parseDiagnostic = mkDiagnostic "E0001"
