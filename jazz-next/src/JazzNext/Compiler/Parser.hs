{-# LANGUAGE OverloadedStrings #-}

-- | Surface parser for the current `jazz-next` language slice. It turns the
-- token stream into a block-wrapped surface AST while enforcing the current
-- statement and operator grammar.
module JazzNext.Compiler.Parser
  ( parseSurfaceProgram
  ) where

import Data.Char
  ( isUpper
  )
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
    identifierText,
    mkIdentifier
  )
import JazzNext.Compiler.Parser.AST
  ( SurfaceCaseArm (..),
    SurfaceConstrainedSignatureType (..),
    SurfaceDataConstructor (..),
    SurfaceExpr (..),
    SurfaceLiteral (..),
    SurfacePattern (..),
    SurfaceSignatureConstraint (..),
    SurfaceSignaturePayload (..),
    SurfaceSignatureToken (..),
    SurfaceSignatureType (..),
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
-- Most top-level forms are dot-terminated; module declarations instead own a
-- brace-delimited body and must be the first top-level form.
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

-- | Parse a complete sequence of statements until the token stream is
-- exhausted.
parseStatementsUntilEnd :: [Token] -> Either Diagnostic ([SurfaceStatement], [Token])
parseStatementsUntilEnd tokens = go (collectImportAliasesUntilEnd tokens) [] tokens
  where
    go _ acc [] = Right (reverse acc, [])
    go knownAliases acc tokens = do
      (statements, remaining) <- parseStatement TopLevelContext knownAliases tokens
      case leadingModuleDeclaration statements of
        Just moduleSpan
          | not (null acc) ->
              Left
                ( parseDiagnostic
                    ( "module declaration must be the first top-level form at "
                        <> renderSourceSpan moduleSpan
                    )
                )
          | otherwise ->
              case remaining of
                [] -> go (registerImportAliases knownAliases statements) (prependStatements statements acc) remaining
                token : _ ->
                  Left
                    ( parseDiagnostic
                        ( "unexpected token '"
                            <> tokenLexeme token
                            <> "' at "
                            <> renderSourceSpan (tokenSpan token)
                            <> " after module declaration"
                        )
                    )
        Nothing ->
          go (registerImportAliases knownAliases statements) (prependStatements statements acc) remaining

-- | Parse statements inside `{ ... }`, stopping as soon as the closing brace is
-- encountered so block parsing can hand the remaining tokens back to callers.
parseStatementsUntilBrace :: StatementContext -> Set Text -> [Token] -> Either Diagnostic ([SurfaceStatement], [Token])
parseStatementsUntilBrace context inheritedAliases tokens =
  go (Set.union inheritedAliases (collectImportAliasesUntilBrace tokens)) [] tokens
  where
    go _ _ [] = Left (parseDiagnostic "expected '}' before end of input")
    go knownAliases acc allTokens@(token : rest) =
      case tokenKind token of
        TRBrace -> Right (reverse acc, rest)
        _ -> do
          (statements, remaining) <- parseStatement context knownAliases allTokens
          go (registerImportAliases knownAliases statements) (prependStatements statements acc) remaining

data StatementContext
  = TopLevelContext
  -- Module bodies can contain bindings/imports but must not introduce a second
  -- module declaration.
  | ModuleBodyContext
  -- Ordinary expression blocks must not introduce module declarations.
  | NestedBlockContext

-- | Disambiguate statement-level forms before expression parsing so leading
-- identifiers can become signatures or bindings when followed by `::` or `=`.
parseStatement :: StatementContext -> Set Text -> [Token] -> Either Diagnostic ([SurfaceStatement], [Token])
parseStatement context knownAliases tokens =
  case tokens of
    moduleToken@(Token {tokenKind = TModule}) : rest ->
      case context of
        TopLevelContext ->
          parseModuleStatement moduleToken rest
        ModuleBodyContext ->
          rejectNestedModuleDeclaration moduleToken
        NestedBlockContext ->
          rejectNestedModuleDeclaration moduleToken
    importToken@(Token {tokenKind = TImport}) : rest ->
      fmap singleStatement (parseImportStatement importToken rest)
    dataToken@(Token {tokenKind = TData}) : rest ->
      case context of
        TopLevelContext ->
          fmap singleStatement (parseDataStatement dataToken rest)
        ModuleBodyContext ->
          fmap singleStatement (parseDataStatement dataToken rest)
        NestedBlockContext ->
          rejectNestedDataDeclaration dataToken
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
      | TIdentifier name <- tokenKind nameToken,
        shouldParseQualifiedAliasStatement knownAliases name afterName ->
          fmap singleStatement (parseExprStatement knownAliases tokens)
      | TIdentifier name <- tokenKind nameToken ->
          fmap singleStatement (parseSignature (mkIdentifier name) nameToken afterName)
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
      | TIdentifier name <- tokenKind nameToken ->
          fmap singleStatement (parseLet knownAliases (mkIdentifier name) nameToken afterName)
    _ -> fmap singleStatement (parseExprStatement knownAliases tokens)
  where
    singleStatement (statement, remaining) = ([statement], remaining)

registerImportAliases :: Set Text -> [SurfaceStatement] -> Set Text
registerImportAliases =
  foldl registerImportAlias
  where
    registerImportAlias knownAliases statement =
      case statement of
        SSImport _ _ (Just aliasName) Nothing ->
          Set.insert aliasName knownAliases
        _ -> knownAliases

shouldParseQualifiedAliasStatement :: Set Text -> Text -> [Token] -> Bool
shouldParseQualifiedAliasStatement knownAliases name tokensAfterName =
  case tokensAfterName of
    Token {tokenKind = TColonColon} : Token {tokenKind = TIdentifier memberName} : _ ->
      Set.member name knownAliases || isValueIdentifierText memberName
    Token {tokenKind = TColonColon} : _ ->
      Set.member name knownAliases
    _ ->
      False

isValueIdentifierText :: Text -> Bool
isValueIdentifierText name =
  not (isConstructorIdentifierText name) && not (isReservedLiteralName name)

collectImportAliasesUntilEnd :: [Token] -> Set Text
collectImportAliasesUntilEnd = collectImportAliasesInStatementList False

collectImportAliasesUntilBrace :: [Token] -> Set Text
collectImportAliasesUntilBrace = collectImportAliasesInStatementList True

collectImportAliasesInStatementList :: Bool -> [Token] -> Set Text
collectImportAliasesInStatementList stopAtRightBrace = go 0 Set.empty
  where
    go _ aliases [] = aliases
    go depth aliases (token : rest)
      | stopAtRightBrace && depth == 0 && tokenKind token == TRBrace = aliases
      | otherwise =
          case tokenKind token of
            TImport
              | depth == 0 ->
                  go depth (maybe aliases (`Set.insert` aliases) (collectImportAlias rest)) rest
            TLBrace ->
              go (depth + 1) aliases rest
            TRBrace ->
              go (max 0 (depth - 1)) aliases rest
            _ ->
              go depth aliases rest

    collectImportAlias importTail =
      case importTail of
        [] -> Nothing
        Token {tokenKind = TDot} : _ -> Nothing
        Token {tokenKind = TAs} : Token {tokenKind = TIdentifier aliasName} : _ -> Just aliasName
        _ : rest -> collectImportAlias rest

rejectNestedModuleDeclaration :: Token -> Either Diagnostic a
rejectNestedModuleDeclaration moduleToken =
  Left
    ( parseDiagnostic
        ( "module declaration must remain top-level at "
            <> renderSourceSpan (tokenSpan moduleToken)
        )
    )

rejectNestedDataDeclaration :: Token -> Either Diagnostic a
rejectNestedDataDeclaration dataToken =
  Left
    ( parseDiagnostic
        ( "data declaration must remain top-level at "
            <> renderSourceSpan (tokenSpan dataToken)
        )
    )

prependStatements :: [SurfaceStatement] -> [SurfaceStatement] -> [SurfaceStatement]
prependStatements statements acc = foldl (flip (:)) acc statements

leadingModuleDeclaration :: [SurfaceStatement] -> Maybe SourceSpan
leadingModuleDeclaration statements =
  case statements of
    SSModule spanValue _ : _ -> Just spanValue
    _ -> Nothing

isReservedLiteralName :: Text -> Bool
isReservedLiteralName name = name == "True" || name == "False"

parseModuleStatement :: Token -> [Token] -> Either Diagnostic ([SurfaceStatement], [Token])
parseModuleStatement moduleToken tokensAfterModuleKeyword = do
  (modulePath, afterModulePath) <- parseModulePath tokensAfterModuleKeyword
  case afterModulePath of
    Token {tokenKind = TLBrace} : tokensAfterLeftBrace -> do
      -- Keep downstream resolver/driver code on the current flat statement
      -- contract by replaying module-body statements after the declaration.
      (bodyStatements, remaining) <- parseStatementsUntilBrace ModuleBodyContext Set.empty tokensAfterLeftBrace
      pure (SSModule (tokenSpan moduleToken) modulePath : bodyStatements, remaining)
    [] ->
      Left
        ( parseDiagnostic
            ( "expected '{' before end of input after module path at "
                <> renderSourceSpan (tokenSpan moduleToken)
            )
        )
    token : _ ->
      Left
        ( parseDiagnostic
            ( "expected '{' at "
                <> renderSourceSpan (tokenSpan token)
                <> ", found '"
                <> tokenLexeme token
                <> "'"
            )
        )

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

parseDataStatement :: Token -> [Token] -> Either Diagnostic (SurfaceStatement, [Token])
parseDataStatement dataToken tokensAfterDataKeyword = do
  (typeName, afterTypeName) <- parseDataTypeName tokensAfterDataKeyword
  afterEquals <-
    consumeEquals
      afterTypeName
      ( "expected '=' before end of input after data type name at "
          <> renderSourceSpan (tokenSpan dataToken)
      )
  (constructors, remaining) <- parseDataConstructors afterEquals
  pure (SSData (tokenSpan dataToken) typeName constructors, remaining)

parseDataTypeName :: [Token] -> Either Diagnostic (Identifier, [Token])
parseDataTypeName tokens =
  case tokens of
    Token {tokenKind = TIdentifier typeName, tokenSpan = typeSpan} : rest
      | isConstructorIdentifierText typeName ->
          Right (mkIdentifier typeName, rest)
      | otherwise ->
          Left
            ( parseDiagnostic
                ( "expected type constructor name at "
                    <> renderSourceSpan typeSpan
                    <> ", found '"
                    <> typeName
                    <> "'"
                )
            )
    [] ->
      Left (parseDiagnostic "expected type constructor name before end of input after 'data'")
    token : _ ->
      Left
        ( parseDiagnostic
            ( "expected type constructor name at "
                <> renderSourceSpan (tokenSpan token)
                <> ", found '"
                <> tokenLexeme token
                <> "'"
            )
        )

parseDataConstructors :: [Token] -> Either Diagnostic ([SurfaceDataConstructor], [Token])
parseDataConstructors tokensAfterEquals = do
  (firstConstructor, afterFirstConstructor) <- parseDataConstructor tokensAfterEquals
  go
    (Set.singleton (surfaceDataConstructorName firstConstructor))
    [firstConstructor]
    afterFirstConstructor
  where
    go seenConstructors revConstructors allTokens =
      case allTokens of
        Token {tokenKind = TDot} : rest ->
          Right (reverse revConstructors, rest)
        Token {tokenKind = TOperator "|"} : rest -> do
          (nextConstructor, afterNextConstructor) <- parseDataConstructor rest
          let constructorName = surfaceDataConstructorName nextConstructor
          if Set.member constructorName seenConstructors
            then
              Left
                ( parseDiagnostic
                    ("duplicate constructor declaration '" <> constructorName <> "' in data declaration")
                )
            else
              go
                (Set.insert constructorName seenConstructors)
                (nextConstructor : revConstructors)
                afterNextConstructor
        [] ->
          Left (parseDiagnostic "expected '.' before end of input in data declaration")
        token : _ ->
          Left
            ( parseDiagnostic
                ( "expected '|' or '.' at "
                    <> renderSourceSpan (tokenSpan token)
                    <> ", found '"
                    <> tokenLexeme token
                    <> "'"
                )
            )

    surfaceDataConstructorName :: SurfaceDataConstructor -> Text
    surfaceDataConstructorName (SurfaceDataConstructor constructorName _) =
      identifierText constructorName

parseDataConstructor :: [Token] -> Either Diagnostic (SurfaceDataConstructor, [Token])
parseDataConstructor tokens =
  case tokens of
    Token {tokenKind = TIdentifier constructorName, tokenSpan = constructorSpan} : rest
      | isConstructorIdentifierText constructorName -> do
          (constructorArity, remaining) <- parseDataConstructorArity 0 rest
          Right
            ( SurfaceDataConstructor (mkIdentifier constructorName) constructorArity,
              remaining
            )
      | otherwise ->
          Left
            ( parseDiagnostic
                ( "expected constructor declaration at "
                    <> renderSourceSpan constructorSpan
                    <> ", found '"
                    <> constructorName
                    <> "'"
                )
            )
    [] ->
      Left (parseDiagnostic "expected constructor declaration before end of input in data declaration")
    token : _ ->
      Left
        ( parseDiagnostic
            ( "expected constructor declaration at "
                <> renderSourceSpan (tokenSpan token)
                <> ", found '"
                <> tokenLexeme token
                <> "'"
            )
        )

parseDataConstructorArity :: Int -> [Token] -> Either Diagnostic (Int, [Token])
parseDataConstructorArity arity allTokens =
  case allTokens of
    Token {tokenKind = TOperator "|"} : _ ->
      Right (arity, allTokens)
    Token {tokenKind = TDot} : _ ->
      Right (arity, allTokens)
    [] ->
      Right (arity, allTokens)
    _ -> do
      remaining <- parseDataConstructorArgument allTokens
      parseDataConstructorArity (arity + 1) remaining

parseDataConstructorArgument :: [Token] -> Either Diagnostic [Token]
parseDataConstructorArgument tokens =
  case tokens of
    Token {tokenKind = TIdentifier _} : rest ->
      Right rest
    Token {tokenKind = TLParen} : rest ->
      consumeBalancedDataConstructorGroup 1 0 rest
    Token {tokenKind = TLBracket} : rest ->
      consumeBalancedDataConstructorGroup 0 1 rest
    [] ->
      Left (parseDiagnostic "expected constructor argument before end of input in data declaration")
    token : _ ->
      Left
        ( parseDiagnostic
            ( "expected constructor argument at "
                <> renderSourceSpan (tokenSpan token)
                <> ", found '"
                <> tokenLexeme token
                <> "'"
            )
        )

consumeBalancedDataConstructorGroup :: Int -> Int -> [Token] -> Either Diagnostic [Token]
consumeBalancedDataConstructorGroup parenDepth bracketDepth tokens =
  case tokens of
    [] ->
      Left (parseDiagnostic "expected constructor argument to close before end of input in data declaration")
    token : rest ->
      case tokenKind token of
        TLParen ->
          consumeBalancedDataConstructorGroup (parenDepth + 1) bracketDepth rest
        TRParen
          | parenDepth > 0 ->
              let nextParenDepth = parenDepth - 1
               in
                if nextParenDepth == 0 && bracketDepth == 0
                  then Right rest
                  else consumeBalancedDataConstructorGroup nextParenDepth bracketDepth rest
          | otherwise ->
              Left
                ( parseDiagnostic
                    ( "unexpected ')' at "
                        <> renderSourceSpan (tokenSpan token)
                        <> " in constructor argument"
                    )
                )
        TLBracket ->
          consumeBalancedDataConstructorGroup parenDepth (bracketDepth + 1) rest
        TRBracket
          | bracketDepth > 0 ->
              let nextBracketDepth = bracketDepth - 1
               in
                if parenDepth == 0 && nextBracketDepth == 0
                  then Right rest
                  else consumeBalancedDataConstructorGroup parenDepth nextBracketDepth rest
          | otherwise ->
              Left
                ( parseDiagnostic
                    ( "unexpected ']' at "
                        <> renderSourceSpan (tokenSpan token)
                        <> " in constructor argument"
                    )
                )
        _ ->
          consumeBalancedDataConstructorGroup parenDepth bracketDepth rest

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
      let signaturePayload = parseSignaturePayload signatureTokens
      pure
        ( SSSignature name (tokenSpan nameToken) signaturePayload,
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

parseLet :: Set Text -> Identifier -> Token -> [Token] -> Either Diagnostic (SurfaceStatement, [Token])
parseLet knownAliases name nameToken tokensAfterName =
  case tokensAfterName of
    Token {tokenKind = TEquals} : rest -> do
      (valueExpr, afterExpr) <- parseExpr knownAliases rest
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

parseExprStatement :: Set Text -> [Token] -> Either Diagnostic (SurfaceStatement, [Token])
parseExprStatement knownAliases tokens = do
  case tokens of
    [] -> Left (parseDiagnostic "expected expression before end of input")
    firstToken : _ -> do
      (expr, afterExpr) <- parseExpr knownAliases tokens
      remaining <- consumeDot afterExpr
      pure (SSExpr (tokenSpan firstToken) expr, remaining)

parseExpr :: Set Text -> [Token] -> Either Diagnostic (SurfaceExpr, [Token])
parseExpr knownAliases = parseExprWithMinPrecedenceUntil knownAliases neverStop 1

-- | Entry point for expression parsing that first folds application via
-- `parseApplicationExpr`, then continues with precedence-climbing for infix
-- operators.
parseExprWithMinPrecedence :: Int -> [Token] -> Either Diagnostic (SurfaceExpr, [Token])
parseExprWithMinPrecedence = parseExprWithMinPrecedenceUntil Set.empty neverStop

parseExprWithMinPrecedenceUntil ::
  Set Text ->
  ([Token] -> Bool) ->
  Int ->
  [Token] ->
  Either Diagnostic (SurfaceExpr, [Token])
parseExprWithMinPrecedenceUntil knownAliases stop minPrecedence tokens = do
  (leftExpr, remainingTokens) <- parseApplicationExprUntil knownAliases stop tokens
  parseInfixTailWithUntil stop (parseExprWithMinPrecedenceUntil knownAliases stop) minPrecedence leftExpr remainingTokens

-- Used by `if` parsing to preserve the existing compact `if cond then else`
-- surface form without introducing a `then` delimiter.
parseExprWithoutApplication :: [Token] -> Either Diagnostic (SurfaceExpr, [Token])
parseExprWithoutApplication = parseExprWithoutApplicationWithMinPrecedenceUntil Set.empty neverStop 1

parseExprWithoutApplicationWithMinPrecedence :: Int -> [Token] -> Either Diagnostic (SurfaceExpr, [Token])
parseExprWithoutApplicationWithMinPrecedence =
  parseExprWithoutApplicationWithMinPrecedenceUntil Set.empty neverStop

parseExprWithoutApplicationWithMinPrecedenceUntil ::
  Set Text ->
  ([Token] -> Bool) ->
  Int ->
  [Token] ->
  Either Diagnostic (SurfaceExpr, [Token])
parseExprWithoutApplicationWithMinPrecedenceUntil knownAliases stop minPrecedence tokens = do
  (leftExpr, remainingTokens) <- parsePrimaryExprUntil knownAliases stop tokens
  parseInfixTailWithUntil stop (parseExprWithoutApplicationWithMinPrecedenceUntil knownAliases stop) minPrecedence leftExpr remainingTokens

parseApplicationExpr :: [Token] -> Either Diagnostic (SurfaceExpr, [Token])
parseApplicationExpr = parseApplicationExprUntil Set.empty neverStop

parseApplicationExprUntil :: Set Text -> ([Token] -> Bool) -> [Token] -> Either Diagnostic (SurfaceExpr, [Token])
parseApplicationExprUntil knownAliases stop tokens = do
  (functionExpr, remainingTokens) <- parsePrimaryExprUntil knownAliases stop tokens
  parseApplicationTailUntil knownAliases stop functionExpr remainingTokens

-- | Function application binds tighter than infix operators, so adjacent
-- primary expressions are folded into left-associated applications first.
parseApplicationTail :: SurfaceExpr -> [Token] -> Either Diagnostic (SurfaceExpr, [Token])
parseApplicationTail = parseApplicationTailUntil Set.empty neverStop

parseApplicationTailUntil ::
  Set Text ->
  ([Token] -> Bool) ->
  SurfaceExpr ->
  [Token] ->
  Either Diagnostic (SurfaceExpr, [Token])
parseApplicationTailUntil knownAliases stop functionExpr tokens
  | stop tokens = Right (functionExpr, tokens)
  | otherwise =
      case startsPrimaryExprTokens tokens of
        True -> do
          (argumentExpr, remainingAfterArgument) <- parsePrimaryExprUntil knownAliases stop tokens
          parseApplicationTailUntil knownAliases stop (SEApply functionExpr argumentExpr) remainingAfterArgument
        False -> Right (functionExpr, tokens)

neverStop :: [Token] -> Bool
neverStop _ = False

startsPrimaryExprTokens :: [Token] -> Bool
startsPrimaryExprTokens allTokens =
  case allTokens of
    Token {tokenKind = TInt _} : _ -> True
    Token {tokenKind = TIdentifier _} : _ -> True
    Token {tokenKind = TIf} : _ -> True
    Token {tokenKind = TCase} : _ -> True
    Token {tokenKind = TLambda} : _ -> True
    Token {tokenKind = TLParen} : _ -> True
    Token {tokenKind = TLBrace} : _ -> True
    Token {tokenKind = TLBracket} : _ -> True
    _ -> False

-- | Shared precedence climber used by both regular expression parsing and the
-- restricted `if` condition parser.
parseInfixTailWithUntil ::
  ([Token] -> Bool) ->
  (Int -> [Token] -> Either Diagnostic (SurfaceExpr, [Token])) ->
  Int ->
  SurfaceExpr ->
  [Token] ->
  Either Diagnostic (SurfaceExpr, [Token])
parseInfixTailWithUntil stop parseRhs minPrecedence leftExpr tokens
  | stop tokens = Right (leftExpr, tokens)
  | otherwise =
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
                      parseInfixTailWithUntil
                        stop
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

-- | Shared precedence climber used by both regular expression parsing and the
-- restricted `if` condition parser.
parseInfixTailWith ::
  (Int -> [Token] -> Either Diagnostic (SurfaceExpr, [Token])) ->
  Int ->
  SurfaceExpr ->
  [Token] ->
  Either Diagnostic (SurfaceExpr, [Token])
parseInfixTailWith = parseInfixTailWithUntil neverStop

parsePrimaryExpr :: [Token] -> Either Diagnostic (SurfaceExpr, [Token])
parsePrimaryExpr = parsePrimaryExprUntil Set.empty neverStop

parsePrimaryExprUntil :: Set Text -> ([Token] -> Bool) -> [Token] -> Either Diagnostic (SurfaceExpr, [Token])
parsePrimaryExprUntil knownAliases stop tokens =
  case tokens of
    [] -> Left (parseDiagnostic "expected expression before end of input")
    token : rest ->
      case tokenKind token of
        TInt value -> Right (SELit (SLInt value), rest)
        TIdentifier name ->
          case name of
            "True" -> Right (SELit (SLBool True), rest)
            "False" -> Right (SELit (SLBool False), rest)
            _ ->
              case rest of
                Token {tokenKind = TColonColon} : Token {tokenKind = TIdentifier memberName} : afterMember ->
                  Right (SEQualifiedVar (mkIdentifier name) (mkIdentifier memberName), afterMember)
                Token {tokenKind = TColonColon} : [] ->
                  Left (parseDiagnostic "expected member name after '::' before end of input")
                Token {tokenKind = TColonColon} : memberToken : _ ->
                  Left
                    ( parseDiagnostic
                        ( "expected member name after '::' at "
                            <> renderSourceSpan (tokenSpan memberToken)
                            <> ", found '"
                            <> tokenLexeme memberToken
                            <> "'"
                        )
                    )
                _ -> Right (SEVar (mkIdentifier name), rest)
        TIf -> parseIfExprUntil knownAliases stop token rest
        TCase -> parseCaseExpr knownAliases token rest
        TLambda -> parseLambdaExprUntil knownAliases stop token rest
        TLParen -> parseParenExpr knownAliases rest
        TLBrace -> do
          (statements, afterBrace) <- parseStatementsUntilBrace NestedBlockContext knownAliases rest
          Right (SEBlock statements, afterBrace)
        TLBracket -> parseListExpr knownAliases rest
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
parseParenExpr :: Set Text -> [Token] -> Either Diagnostic (SurfaceExpr, [Token])
parseParenExpr knownAliases tokensAfterLeftParen =
  case tokensAfterLeftParen of
    Token {tokenKind = TOperator operatorSymbol} : rest ->
      case rest of
        Token {tokenKind = TRParen} : remaining ->
          Right (SEOperatorValue operatorSymbol, remaining)
        _ -> do
          (rightExpr, afterRightExpr) <- parseExpr knownAliases rest
          remaining <- consumeRightParen afterRightExpr
          pure (SESectionRight operatorSymbol rightExpr, remaining)
    _ -> do
      (innerExpr, afterInnerExpr) <- parseExpr knownAliases tokensAfterLeftParen
      case afterInnerExpr of
        Token {tokenKind = TOperator operatorSymbol} : Token {tokenKind = TRParen} : rest ->
          Right (SESectionLeft innerExpr operatorSymbol, rest)
        _ -> do
          remaining <- consumeRightParen afterInnerExpr
          Right (innerExpr, remaining)

parseListExpr :: Set Text -> [Token] -> Either Diagnostic (SurfaceExpr, [Token])
parseListExpr knownAliases tokensAfterLeftBracket =
  case tokensAfterLeftBracket of
    Token {tokenKind = TRBracket} : rest ->
      Right (SEList [], rest)
    _ -> do
      (elements, afterElements) <- parseListElements knownAliases tokensAfterLeftBracket
      remaining <- consumeRightBracket afterElements
      Right (SEList elements, remaining)

parseListElements :: Set Text -> [Token] -> Either Diagnostic ([SurfaceExpr], [Token])
parseListElements knownAliases tokens = do
  (firstElement, remainingAfterFirst) <- parseExpr knownAliases tokens
  go [firstElement] remainingAfterFirst
  where
    go elements allTokens =
      case allTokens of
        Token {tokenKind = TComma} : rest -> do
          (nextElement, remainingAfterNext) <- parseExpr knownAliases rest
          go (nextElement : elements) remainingAfterNext
        _ ->
          Right (reverse elements, allTokens)

-- | Parse the compact `if cond thenExpr else elseExpr` surface form.
parseIfExpr :: Token -> [Token] -> Either Diagnostic (SurfaceExpr, [Token])
parseIfExpr = parseIfExprUntil Set.empty neverStop

parseIfExprUntil :: Set Text -> ([Token] -> Bool) -> Token -> [Token] -> Either Diagnostic (SurfaceExpr, [Token])
parseIfExprUntil knownAliases stop ifToken tokensAfterIf = do
  (conditionExpr, afterCondition) <- parseExprWithoutApplicationWithMinPrecedenceUntil knownAliases stop 1 tokensAfterIf
  (thenExpr, afterThenExpr) <- parseExprWithMinPrecedenceUntil knownAliases stop 1 afterCondition
  case afterThenExpr of
    Token {tokenKind = TElse} : afterElse -> do
      (elseExpr, remaining) <- parseExprWithMinPrecedenceUntil knownAliases stop 1 afterElse
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

parseCaseExpr :: Set Text -> Token -> [Token] -> Either Diagnostic (SurfaceExpr, [Token])
parseCaseExpr knownAliases caseToken tokensAfterCase =
    case tryCaseBodyCandidates Nothing [] tokensAfterCase of
      Right parsedCaseExpr ->
        Right parsedCaseExpr
      Left maybeBodyDiagnostic ->
        case maybeBodyDiagnostic of
          Just diagnostic ->
            Left diagnostic
          Nothing ->
            case parseExpr knownAliases tokensAfterCase of
              Left scrutineeDiagnostic ->
                Left scrutineeDiagnostic
              Right _ ->
                Left (parseDiagnostic caseBodyMissingMessage)
  where
    caseBodyMissingMessage =
      "expected '{' before end of input after 'case' at " <> renderSourceSpan (tokenSpan caseToken)

    tryCaseBodyCandidates ::
      Maybe Diagnostic ->
      [Token] ->
      [Token] ->
      Either (Maybe Diagnostic) (SurfaceExpr, [Token])
    -- Block expressions and case bodies both start with `{`, so try each brace
    -- as the body boundary and keep the first split that yields a full
    -- scrutinee expression plus a parseable case-arm list.
    tryCaseBodyCandidates firstBodyDiagnostic revPrefix remainingTokens =
      case remainingTokens of
        [] ->
          case firstBodyDiagnostic of
            Just diagnostic -> Left (Just diagnostic)
            Nothing -> Left Nothing
        candidateTokens@(token@(Token {tokenKind = TLBrace}) : rest) ->
          let scrutineeTokens = reverse revPrefix
           in
            case parseExpr knownAliases scrutineeTokens of
              Right (scrutineeExpr, []) ->
                case parseCaseBodyTokens candidateTokens of
                  Right (caseArms, remainingAfterCase) ->
                    Right (SECase scrutineeExpr caseArms, remainingAfterCase)
                  Left diagnostic ->
                    if braceLooksLikeScrutineeBlock candidateTokens
                      then tryCaseBodyCandidates firstBodyDiagnostic (token : revPrefix) rest
                      else
                        tryCaseBodyCandidates
                          (rememberLatestDiagnostic firstBodyDiagnostic diagnostic)
                          (token : revPrefix)
                          rest
              _ ->
                tryCaseBodyCandidates firstBodyDiagnostic (token : revPrefix) rest
        token : rest ->
          tryCaseBodyCandidates firstBodyDiagnostic (token : revPrefix) rest

    parseCaseBodyTokens :: [Token] -> Either Diagnostic ([SurfaceCaseArm], [Token])
    parseCaseBodyTokens bodyTokens = do
      tokensAfterLeftBrace <- consumeLeftBrace bodyTokens caseBodyMissingMessage
      parseCaseArms knownAliases tokensAfterLeftBrace

    rememberLatestDiagnostic :: Maybe Diagnostic -> Diagnostic -> Maybe Diagnostic
    rememberLatestDiagnostic _ newDiagnostic = Just newDiagnostic

    braceLooksLikeScrutineeBlock :: [Token] -> Bool
    braceLooksLikeScrutineeBlock tokens =
      case tokens of
        Token {tokenKind = TLBrace} : rest -> go rest
        _ -> False
      where
        go allTokens =
          case allTokens of
            [] -> False
            Token {tokenKind = TDot} : _ -> True
            Token {tokenKind = TRBrace} : _ -> False
            _ : remaining -> go remaining

parseCaseArms :: Set Text -> [Token] -> Either Diagnostic ([SurfaceCaseArm], [Token])
parseCaseArms knownAliases tokensAfterLeftBrace =
  case tokensAfterLeftBrace of
    Token {tokenKind = TRBrace, tokenSpan = rightBraceSpan} : _ ->
      Left
        ( parseDiagnostic
            ( "expected case arm before '}' at "
                <> renderSourceSpan rightBraceSpan
            )
        )
    _ -> do
      (firstArm, afterFirstArm) <- parseCaseArm knownAliases tokensAfterLeftBrace
      go [firstArm] afterFirstArm
  where
    go revArms allTokens =
      case allTokens of
        Token {tokenKind = TRBrace} : rest ->
          Right (reverse revArms, rest)
        _ -> do
          (nextArm, afterNextArm) <- parseCaseArm knownAliases allTokens
          go (nextArm : revArms) afterNextArm

parseCaseArm :: Set Text -> [Token] -> Either Diagnostic (SurfaceCaseArm, [Token])
parseCaseArm knownAliases tokens = do
  tokensAfterPipe <- consumeCaseArmPipe tokens
  (casePattern, afterPattern) <- parseCasePattern tokensAfterPipe
  afterArrow <- consumeArrow afterPattern "expected '->' before end of input after case pattern"
  (bodyExpr, remaining) <- parseExprWithMinPrecedenceUntil knownAliases stopsBeforeCaseArmBoundary 1 afterArrow
  pure (SurfaceCaseArm casePattern bodyExpr, remaining)
  where
    stopsBeforeCaseArmBoundary allTokens =
      case allTokens of
        Token {tokenKind = TOperator "|"} : rest ->
          startsDefiniteCaseArm rest
        Token {tokenKind = TRBrace} : _ -> True
        _ -> False

    startsDefiniteCaseArm remainingTokens =
      case parseCasePattern remainingTokens of
        Right (_, Token {tokenKind = TArrow} : _) -> True
        Left _
          | startsCasePatternTokens remainingTokens ->
              hasTopLevelArrowBeforeCaseArmBoundary remainingTokens
        _ -> False

    hasTopLevelArrowBeforeCaseArmBoundary = go 0 0 0
      where
        go parenDepth braceDepth bracketDepth allTokens =
          case allTokens of
            []
              -> False
            Token {tokenKind = TArrow} : _
              | atTopLevel -> True
            Token {tokenKind = TOperator "|"} : _
              | atTopLevel -> False
            Token {tokenKind = TRBrace} : _
              | atTopLevel -> False
            Token {tokenKind = TLParen} : rest ->
              go (parenDepth + 1) braceDepth bracketDepth rest
            Token {tokenKind = TRParen} : rest ->
              go (decrementIfPositive parenDepth) braceDepth bracketDepth rest
            Token {tokenKind = TLBrace} : rest ->
              go parenDepth (braceDepth + 1) bracketDepth rest
            Token {tokenKind = TRBrace} : rest ->
              go parenDepth (decrementIfPositive braceDepth) bracketDepth rest
            Token {tokenKind = TLBracket} : rest ->
              go parenDepth braceDepth (bracketDepth + 1) rest
            Token {tokenKind = TRBracket} : rest ->
              go parenDepth braceDepth (decrementIfPositive bracketDepth) rest
            _ : rest ->
              go parenDepth braceDepth bracketDepth rest
          where
            atTopLevel =
              parenDepth == 0 && braceDepth == 0 && bracketDepth == 0

        decrementIfPositive depth
          | depth > 0 = depth - 1
          | otherwise = 0

parseCasePattern :: [Token] -> Either Diagnostic (SurfacePattern, [Token])
parseCasePattern tokens =
  case tokens of
    Token {tokenKind = TInt value} : rest ->
      Right (SPLiteral (SLInt value), rest)
    Token {tokenKind = TLBracket} : rest ->
      parseListPattern rest
    Token {tokenKind = TIdentifier name} : rest ->
      case name of
        "_" -> Right (SPWildcard, rest)
        "True" -> Right (SPLiteral (SLBool True), rest)
        "False" -> Right (SPLiteral (SLBool False), rest)
        _
          | isConstructorIdentifierText name ->
              parseConstructorPattern (mkIdentifier name) rest
          | otherwise ->
              Right (SPVariable (mkIdentifier name), rest)
    [] ->
      Left (parseDiagnostic "expected case pattern before end of input")
    token : _ ->
      Left
        ( parseDiagnostic
            ( "expected case pattern at "
                <> renderSourceSpan (tokenSpan token)
                <> ", found '"
                <> tokenLexeme token
                <> "'"
            )
        )

parseConstructorPattern :: Identifier -> [Token] -> Either Diagnostic (SurfacePattern, [Token])
parseConstructorPattern constructorName tokensAfterName =
  go [] tokensAfterName
  where
    go revArguments remainingTokens
      | patternArgumentBoundary remainingTokens =
          Right (SPConstructor constructorName (reverse revArguments), remainingTokens)
      -- Constructor arguments currently use atomic subpatterns so ambiguous
      -- forms like `Pair Nothing item` stay as two outer arguments.
      | startsCasePatternTokens remainingTokens = do
          (nextArgument, afterArgument) <- parseConstructorArgumentPattern remainingTokens
          go (nextArgument : revArguments) afterArgument
      | otherwise =
          Right (SPConstructor constructorName (reverse revArguments), remainingTokens)

parseConstructorArgumentPattern :: [Token] -> Either Diagnostic (SurfacePattern, [Token])
parseConstructorArgumentPattern tokens =
  case tokens of
    Token {tokenKind = TInt value} : rest ->
      Right (SPLiteral (SLInt value), rest)
    Token {tokenKind = TIdentifier name} : rest ->
      case name of
        "True" ->
          Right (SPLiteral (SLBool True), rest)
        "False" ->
          Right (SPLiteral (SLBool False), rest)
        "_" ->
          Right (SPWildcard, rest)
        _
          | isConstructorIdentifierText name ->
              Right (SPConstructor (mkIdentifier name) [], rest)
          | otherwise ->
              Right (SPVariable (mkIdentifier name), rest)
    Token {tokenKind = TLBracket} : rest ->
      parseListPattern rest
    [] ->
      Left (parseDiagnostic "expected constructor pattern argument before end of input")
    token : _ ->
      Left
        ( parseDiagnostic
            ( "expected constructor pattern argument at "
                <> renderSourceSpan (tokenSpan token)
                <> ", found '"
                <> tokenLexeme token
                <> "'"
            )
        )

patternArgumentBoundary :: [Token] -> Bool
patternArgumentBoundary tokens =
  case tokens of
    [] -> True
    Token {tokenKind = TArrow} : _ -> True
    Token {tokenKind = TComma} : _ -> True
    Token {tokenKind = TRBracket} : _ -> True
    Token {tokenKind = TRBrace} : _ -> True
    _ -> False

startsCasePatternTokens :: [Token] -> Bool
startsCasePatternTokens tokens =
  case tokens of
    Token {tokenKind = TInt _} : _ -> True
    Token {tokenKind = TIdentifier _} : _ -> True
    Token {tokenKind = TLBracket} : _ -> True
    _ -> False

parseListPattern :: [Token] -> Either Diagnostic (SurfacePattern, [Token])
parseListPattern tokensAfterLeftBracket =
  case tokensAfterLeftBracket of
    Token {tokenKind = TRBracket} : rest ->
      Right (SPList [], rest)
    _ -> do
      (firstPattern, afterFirstPattern) <- parseCasePattern tokensAfterLeftBracket
      go [firstPattern] afterFirstPattern
  where
    go revPatterns remainingTokens =
      case remainingTokens of
        Token {tokenKind = TComma} : rest -> do
          (nextPattern, afterNextPattern) <- parseCasePattern rest
          go (nextPattern : revPatterns) afterNextPattern
        Token {tokenKind = TRBracket} : rest ->
          Right (SPList (reverse revPatterns), rest)
        [] ->
          Left (parseDiagnostic "expected ']' before end of input in list pattern")
        token : _ ->
          Left
            ( parseDiagnostic
                ( "expected ',' or ']' at "
                    <> renderSourceSpan (tokenSpan token)
                    <> ", found '"
                    <> tokenLexeme token
                    <> "'"
                )
            )

isConstructorIdentifierText :: Text -> Bool
isConstructorIdentifierText name =
  case Text.uncons name of
    Just (firstChar, _) -> isUpper firstChar
    Nothing -> False

parseLambdaExpr :: Token -> [Token] -> Either Diagnostic (SurfaceExpr, [Token])
parseLambdaExpr = parseLambdaExprUntil Set.empty neverStop

parseLambdaExprUntil :: Set Text -> ([Token] -> Bool) -> Token -> [Token] -> Either Diagnostic (SurfaceExpr, [Token])
parseLambdaExprUntil knownAliases stop lambdaToken tokensAfterLambda =
  case tokensAfterLambda of
    Token {tokenKind = TLParen} : afterLeftParen -> do
      (parameters, afterParameters) <- parseLambdaParameters afterLeftParen
      case afterParameters of
        Token {tokenKind = TArrow} : afterArrow -> do
          (bodyExpr, remaining) <- parseExprWithMinPrecedenceUntil knownAliases stop 1 afterArrow
          pure (SELambda parameters bodyExpr, remaining)
        [] ->
          Left
            ( parseDiagnostic
                ( "expected '->' before end of input after lambda parameters at "
                    <> renderSourceSpan (tokenSpan lambdaToken)
                )
            )
        token : _ ->
          Left
            ( parseDiagnostic
                ( "expected '->' at "
                    <> renderSourceSpan (tokenSpan token)
                    <> ", found '"
                    <> tokenLexeme token
                    <> "'"
                )
            )
    [] ->
      Left
        ( parseDiagnostic
            ( "expected '(' before end of input after lambda introducer at "
                <> renderSourceSpan (tokenSpan lambdaToken)
            )
        )
    token : _ ->
      Left
        ( parseDiagnostic
            ( "expected '(' at "
                <> renderSourceSpan (tokenSpan token)
                <> " after lambda introducer"
            )
        )

parseLambdaParameters :: [Token] -> Either Diagnostic ([Identifier], [Token])
parseLambdaParameters tokensAfterLeftParen =
  case tokensAfterLeftParen of
    token@(Token {tokenKind = TRParen}) : _ ->
      Left
        ( parseDiagnostic
            ( "expected lambda parameter before ')' at "
                <> renderSourceSpan (tokenSpan token)
            )
        )
    _ -> do
      (firstParameter, afterFirstParameter) <- parseLambdaParameter tokensAfterLeftParen
      go [firstParameter] afterFirstParameter
  where
    go revParameters allTokens =
      case allTokens of
        Token {tokenKind = TComma} : rest -> do
          (nextParameter, afterNextParameter) <- parseLambdaParameter rest
          go (nextParameter : revParameters) afterNextParameter
        Token {tokenKind = TRParen} : rest ->
          Right (reverse revParameters, rest)
        [] ->
          Left (parseDiagnostic "expected ')' before end of input in lambda parameter list")
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

parseLambdaParameter :: [Token] -> Either Diagnostic (Identifier, [Token])
parseLambdaParameter tokens =
  case tokens of
    Token {tokenKind = TIdentifier parameterName, tokenSpan = parameterSpan} : rest
      | isReservedLiteralName parameterName ->
          Left
            ( parseDiagnostic
                ( "reserved literal '"
                    <> parameterName
                    <> "' cannot be used as a lambda parameter at "
                    <> renderSourceSpan parameterSpan
                )
            )
      | otherwise ->
          Right (mkIdentifier parameterName, rest)
    [] ->
      Left (parseDiagnostic "expected identifier before end of input in lambda parameter list")
    token : _ ->
      Left
        ( parseDiagnostic
            ( "expected identifier at "
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
        Token {tokenKind = TData} : _ -> True
        Token {tokenKind = TIdentifier _} : Token {tokenKind = TEquals} : _ -> True
        Token {tokenKind = TIdentifier _} : Token {tokenKind = TColonColon} : _ -> True
        _ -> False

parseSignaturePayload :: [Token] -> SurfaceSignaturePayload
parseSignaturePayload signatureTokens =
  case parseSupportedSignaturePayload signatureTokens of
    Just signaturePayload -> signaturePayload
    Nothing -> SurfaceUnsupportedSignature (map surfaceSignatureTokenFromToken signatureTokens)

parseSupportedSignaturePayload :: [Token] -> Maybe SurfaceSignaturePayload
parseSupportedSignaturePayload signatureTokens =
  case parseConstrainedSignaturePayload signatureTokens of
    Just signaturePayload ->
      Just signaturePayload
    Nothing ->
      surfaceSignaturePayloadFromType <$> parseSupportedSignatureType signatureTokens

parseConstrainedSignaturePayload :: [Token] -> Maybe SurfaceSignaturePayload
parseConstrainedSignaturePayload signatureTokens =
  case signatureTokens of
    Token {tokenKind = TAt} : Token {tokenKind = TLBrace} : rest -> do
      (constraintTokens, afterConstraintBlock) <- splitConstraintBlockTokens rest
      constraintGroups <-
        if null constraintTokens
          then Just []
          else splitTopLevelCommaTokens constraintTokens
      constraints <- traverse parseSignatureConstraint constraintGroups
      case afterConstraintBlock of
        Token {tokenKind = TColon} : typeTokens -> do
          signatureType <- parseConstrainedSignatureType typeTokens
          Just (SurfaceConstrainedSignature constraints signatureType)
        _ ->
          Nothing
    _ ->
      Nothing

parseSignatureConstraint :: [Token] -> Maybe SurfaceSignatureConstraint
parseSignatureConstraint constraintTokens =
  case parseConstrainedSignatureType constraintTokens of
    Just (SurfaceConstrainedTypeApplication constraintName arguments) ->
      Just (SurfaceSignatureConstraint constraintName arguments)
    Just (SurfaceConstrainedTypeName constraintName) ->
      Just (SurfaceSignatureConstraint constraintName [])
    _ ->
      Nothing

parseConstrainedSignatureType :: [Token] -> Maybe SurfaceConstrainedSignatureType
parseConstrainedSignatureType signatureTokens =
  case splitFirstTopLevelArrowTokens signatureTokens of
    Left () -> Nothing
    Right (Just (argumentTokens, resultTokens)) ->
      SurfaceConstrainedTypeFunction
        <$> parseConstrainedFunctionOperandType argumentTokens
        <*> parseConstrainedSignatureType resultTokens
    Right Nothing ->
      parseConstrainedFunctionOperandType signatureTokens

parseConstrainedFunctionOperandType :: [Token] -> Maybe SurfaceConstrainedSignatureType
parseConstrainedFunctionOperandType signatureTokens =
  case parseConstrainedTypeApplication signatureTokens of
    Just signatureType ->
      Just signatureType
    Nothing ->
      case signatureTokens of
        [Token {tokenKind = TIdentifier name}] ->
          Just (SurfaceConstrainedTypeName (mkIdentifier name))
        _ ->
          case stripWrappedSignatureTokens isLBracketToken isRBracketToken signatureTokens of
            Just innerTokens ->
              SurfaceConstrainedTypeList <$> parseConstrainedSignatureType innerTokens
            Nothing ->
              case stripWrappedSignatureTokens isLParenToken isRParenToken signatureTokens of
                Just innerTokens ->
                  parseConstrainedSignatureType innerTokens
                Nothing ->
                  Nothing

parseConstrainedTypeApplication :: [Token] -> Maybe SurfaceConstrainedSignatureType
parseConstrainedTypeApplication signatureTokens =
  case signatureTokens of
    Token {tokenKind = TIdentifier typeName} : argumentTokens -> do
      argumentTokenGroups <-
        stripWrappedSignatureTokens isLParenToken isRParenToken argumentTokens
          >>= splitTopLevelCommaTokens
      arguments <- traverse parseConstrainedSignatureType argumentTokenGroups
      Just (SurfaceConstrainedTypeApplication (mkIdentifier typeName) arguments)
    _ ->
      Nothing

splitConstraintBlockTokens :: [Token] -> Maybe ([Token], [Token])
splitConstraintBlockTokens = go 0 0 []
  where
    go _ _ _ [] = Nothing
    go parenDepth bracketDepth acc (token : rest)
      | isRBraceToken kind && parenDepth == 0 && bracketDepth == 0 =
          Just (reverse acc, rest)
      | isLParenToken kind =
          go (parenDepth + 1) bracketDepth (token : acc) rest
      | isRParenToken kind =
          if parenDepth > 0
            then go (parenDepth - 1) bracketDepth (token : acc) rest
            else Nothing
      | isLBracketToken kind =
          go parenDepth (bracketDepth + 1) (token : acc) rest
      | isRBracketToken kind =
          if bracketDepth > 0
            then go parenDepth (bracketDepth - 1) (token : acc) rest
            else Nothing
      | otherwise =
          go parenDepth bracketDepth (token : acc) rest
      where
        kind = tokenKind token

splitTopLevelCommaTokens :: [Token] -> Maybe [[Token]]
splitTopLevelCommaTokens tokens =
  if null tokens
    then Nothing
    else go 0 0 [] [] tokens
  where
    go parenDepth bracketDepth currentRev groupsRev remainingTokens =
      case remainingTokens of
        []
          | parenDepth == 0 && bracketDepth == 0 && not (null currentRev) ->
              Just (reverse (reverse currentRev : groupsRev))
          | otherwise ->
              Nothing
        token : rest
          | tokenKind token == TComma && parenDepth == 0 && bracketDepth == 0 ->
              if null currentRev
                then Nothing
                else go parenDepth bracketDepth [] (reverse currentRev : groupsRev) rest
          | isLParenToken kind ->
              go (parenDepth + 1) bracketDepth nextCurrentRev groupsRev rest
          | isRParenToken kind ->
              if parenDepth > 0
                then go (parenDepth - 1) bracketDepth nextCurrentRev groupsRev rest
                else Nothing
          | isLBracketToken kind ->
              go parenDepth (bracketDepth + 1) nextCurrentRev groupsRev rest
          | isRBracketToken kind ->
              if bracketDepth > 0
                then go parenDepth (bracketDepth - 1) nextCurrentRev groupsRev rest
                else Nothing
          | otherwise ->
              go parenDepth bracketDepth nextCurrentRev groupsRev rest
          where
            kind = tokenKind token
            nextCurrentRev = token : currentRev

parseSupportedSignatureType :: [Token] -> Maybe SurfaceSignatureType
parseSupportedSignatureType signatureTokens =
  case splitFirstTopLevelArrowTokens signatureTokens of
    Left () -> Nothing
    Right (Just (argumentTokens, resultTokens)) ->
      SurfaceTypeFunction
        <$> parseFunctionOperandType argumentTokens
        <*> parseSupportedSignatureType resultTokens
    Right Nothing ->
      parseFunctionOperandType signatureTokens

parseFunctionOperandType :: [Token] -> Maybe SurfaceSignatureType
parseFunctionOperandType signatureTokens =
  case signatureTokens of
    [Token {tokenKind = TIdentifier "Int"}] ->
      Just SurfaceTypeInt
    [Token {tokenKind = TIdentifier "Bool"}] ->
      Just SurfaceTypeBool
    _ ->
      case stripWrappedSignatureTokens isLBracketToken isRBracketToken signatureTokens of
        Just innerTokens ->
          SurfaceTypeList <$> parseNonFunctionSignatureType innerTokens
        Nothing ->
          case stripWrappedSignatureTokens isLParenToken isRParenToken signatureTokens of
            Just innerTokens ->
              parseSupportedSignatureType innerTokens
            Nothing ->
              Nothing

parseNonFunctionSignatureType :: [Token] -> Maybe SurfaceSignatureType
parseNonFunctionSignatureType signatureTokens =
  case signatureTokens of
    [Token {tokenKind = TIdentifier "Int"}] ->
      Just SurfaceTypeInt
    [Token {tokenKind = TIdentifier "Bool"}] ->
      Just SurfaceTypeBool
    _ ->
      case stripWrappedSignatureTokens isLBracketToken isRBracketToken signatureTokens of
        Just innerTokens ->
          SurfaceTypeList <$> parseNonFunctionSignatureType innerTokens
        Nothing ->
          case stripWrappedSignatureTokens isLParenToken isRParenToken signatureTokens of
            Just innerTokens ->
              parseSupportedSignatureType innerTokens
            Nothing ->
              Nothing

surfaceSignaturePayloadFromType :: SurfaceSignatureType -> SurfaceSignaturePayload
surfaceSignaturePayloadFromType = SurfaceSignatureType

splitFirstTopLevelArrowTokens :: [Token] -> Either () (Maybe ([Token], [Token]))
splitFirstTopLevelArrowTokens tokens = go 0 0 [] tokens
  where
    go 0 0 _ [] =
      Right Nothing
    go _ _ _ [] =
      Left ()
    go parenDepth bracketDepth beforeArrowRev (token : rest)
      | isArrowToken kind && parenDepth == 0 && bracketDepth == 0 =
          Right (Just (reverse beforeArrowRev, rest))
      | isLParenToken kind =
          go (parenDepth + 1) bracketDepth nextBeforeArrowRev rest
      | isRParenToken kind =
          if parenDepth > 0
            then go (parenDepth - 1) bracketDepth nextBeforeArrowRev rest
            else Left ()
      | isLBracketToken kind =
          go parenDepth (bracketDepth + 1) nextBeforeArrowRev rest
      | isRBracketToken kind =
          if bracketDepth > 0
            then go parenDepth (bracketDepth - 1) nextBeforeArrowRev rest
            else Left ()
      | otherwise =
          go parenDepth bracketDepth nextBeforeArrowRev rest
      where
        kind = tokenKind token
        nextBeforeArrowRev = token : beforeArrowRev

stripWrappedSignatureTokens ::
  (TokenKind -> Bool) ->
  (TokenKind -> Bool) ->
  [Token] ->
  Maybe [Token]
stripWrappedSignatureTokens isOpenToken isCloseToken tokens =
  case tokens of
    firstToken : rest
      | isOpenToken (tokenKind firstToken) ->
          go 0 0 [] rest
    _ ->
      Nothing
  where
    go _ _ _ [] = Nothing
    go parenDepth bracketDepth acc (token : rest)
      | isCloseToken kind && parenDepth == 0 && bracketDepth == 0 =
          if null acc || not (null rest)
            then Nothing
            else Just (reverse acc)
      | isLParenToken kind =
          go (parenDepth + 1) bracketDepth (token : acc) rest
      | isRParenToken kind =
          if parenDepth > 0
            then go (parenDepth - 1) bracketDepth (token : acc) rest
            else Nothing
      | isLBracketToken kind =
          go parenDepth (bracketDepth + 1) (token : acc) rest
      | isRBracketToken kind =
          if bracketDepth > 0
            then go parenDepth (bracketDepth - 1) (token : acc) rest
            else Nothing
      | otherwise =
          go parenDepth bracketDepth (token : acc) rest
      where
        kind = tokenKind token

surfaceSignatureTokenFromToken :: Token -> SurfaceSignatureToken
surfaceSignatureTokenFromToken token =
  case tokenKind token of
    TIdentifier name -> SurfaceSignatureNameToken name
    TInt value -> SurfaceSignatureIntToken value
    TArrow -> SurfaceSignatureArrowToken
    TAt -> SurfaceSignatureAtToken
    TColon -> SurfaceSignatureColonToken
    TLParen -> SurfaceSignatureLParenToken
    TRParen -> SurfaceSignatureRParenToken
    TLBrace -> SurfaceSignatureLBraceToken
    TRBrace -> SurfaceSignatureRBraceToken
    TLBracket -> SurfaceSignatureLBracketToken
    TRBracket -> SurfaceSignatureRBracketToken
    TComma -> SurfaceSignatureCommaToken
    TOperator symbol -> SurfaceSignatureOperatorToken symbol
    _ -> SurfaceSignatureOtherToken (tokenLexeme token)

isArrowToken :: TokenKind -> Bool
isArrowToken kind =
  case kind of
    TArrow -> True
    _ -> False

isLParenToken :: TokenKind -> Bool
isLParenToken kind =
  case kind of
    TLParen -> True
    _ -> False

isRParenToken :: TokenKind -> Bool
isRParenToken kind =
  case kind of
    TRParen -> True
    _ -> False

isLBracketToken :: TokenKind -> Bool
isLBracketToken kind =
  case kind of
    TLBracket -> True
    _ -> False

isRBracketToken :: TokenKind -> Bool
isRBracketToken kind =
  case kind of
    TRBracket -> True
    _ -> False

isRBraceToken :: TokenKind -> Bool
isRBraceToken kind =
  case kind of
    TRBrace -> True
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

consumeLeftBrace :: [Token] -> Text -> Either Diagnostic [Token]
consumeLeftBrace tokens endOfInputMessage =
  case tokens of
    Token {tokenKind = TLBrace} : rest -> Right rest
    [] -> Left (parseDiagnostic endOfInputMessage)
    token : _ ->
      Left
        ( parseDiagnostic
            ( "expected '{' at "
                <> renderSourceSpan (tokenSpan token)
                <> ", found '"
                <> tokenLexeme token
                <> "'"
            )
        )

consumeArrow :: [Token] -> Text -> Either Diagnostic [Token]
consumeArrow tokens endOfInputMessage =
  case tokens of
    Token {tokenKind = TArrow} : rest -> Right rest
    [] -> Left (parseDiagnostic endOfInputMessage)
    token : _ ->
      Left
        ( parseDiagnostic
            ( "expected '->' at "
                <> renderSourceSpan (tokenSpan token)
                <> ", found '"
                <> tokenLexeme token
                <> "'"
            )
        )

consumeEquals :: [Token] -> Text -> Either Diagnostic [Token]
consumeEquals tokens endOfInputMessage =
  case tokens of
    Token {tokenKind = TEquals} : rest -> Right rest
    [] -> Left (parseDiagnostic endOfInputMessage)
    token : _ ->
      Left
        ( parseDiagnostic
            ( "expected '=' at "
                <> renderSourceSpan (tokenSpan token)
                <> ", found '"
                <> tokenLexeme token
                <> "'"
            )
        )

consumeCaseArmPipe :: [Token] -> Either Diagnostic [Token]
consumeCaseArmPipe tokens =
  case tokens of
    Token {tokenKind = TOperator "|"} : rest -> Right rest
    [] -> Left (parseDiagnostic "expected '|' before end of input in case expression")
    token : _ ->
      Left
        ( parseDiagnostic
            ( "expected '|' at "
                <> renderSourceSpan (tokenSpan token)
                <> " to start case arm"
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
