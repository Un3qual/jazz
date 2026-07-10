{-# LANGUAGE OverloadedStrings #-}

-- | Declaration-level token-stream parsers for the surface parser.
module JazzNext.Compiler.Parser.Declaration
  ( collectImportAliasesUntilBrace,
    collectImportAliasesUntilEnd,
    parseCapabilityDeclarationTokens,
    parseDataStatementParser,
    parseDataStatementTokens,
    parseImportStatementParser,
    parseImportStatementTokens,
    parseStatementParser
  ) where

import Data.Char
  ( isLower,
    isUpper
  )
import Data.Set
  ( Set
  )
import qualified Data.Set as Set
import Data.Text
  ( Text
  )
import qualified Data.Text as Text
import JazzNext.Compiler.Diagnostics
  ( Diagnostic (..),
    SourceSpan,
    mkDiagnostic,
    renderSourceSpan
  )
import JazzNext.Compiler.Name
  ( Identifier,
    NameNamespace (..),
    identifierText,
    mkIdentifier,
    mkOperatorBindingIdentifier
  )
import JazzNext.Compiler.ModuleExports
  ( ModuleExportSelector (..),
    renderModuleExportSelector
  )
import JazzNext.Compiler.Parser.AST
  ( SurfaceClassMethodSignature (..),
    SurfaceConstrainedSignatureType (..),
    SurfaceDataConstructor (..),
    SurfaceDataConstructorArgument (..),
    SurfaceExpr,
    SurfaceImplMethod (..),
    SurfaceSignaturePayload (..),
    SurfaceSignatureToken (..),
    SurfaceStatement (..)
  )
import JazzNext.Compiler.Parser.Context
  ( ExpressionParser,
    ParserContext (..),
    StatementBlockParser,
    StatementContext (..)
  )
import JazzNext.Compiler.Parser.Lexer
  ( Token (..),
    TokenKind (..),
    isImmediatelyAfter
  )
import JazzNext.Compiler.Parser.Operator
  ( Associativity (..),
    OperatorInfo (..),
    declaredOperatorInfoForPrecedence,
    declaredOperatorInfoForTier,
    isBuiltinOperatorSymbol,
    isReservedOperatorSymbol,
    isValidUserOperatorSymbol
  )
import JazzNext.Compiler.Parser.Signature
  ( parseConstrainedSignatureType,
    parseSignaturePayload,
    splitTopLevelCommaTokens
  )
import JazzNext.Compiler.Parser.TokenParser
  ( Parser,
    failTokenParser,
    parseAnyToken,
    runTokenParserPrefix
  )
import qualified Text.Megaparsec as MP

type ModuleBodyParser = [Token] -> Either Diagnostic ([SurfaceStatement], [Token])

type ImplExpressionParser = [Token] -> Either Diagnostic (SurfaceExpr, [Token])

data CapabilityDeclarationBody
  = CapabilityClassBody [SurfaceClassMethodSignature]
  | CapabilityImplBody [SurfaceImplMethod]

data OperatorDeclarationFixityKeyword
  = OperatorTierKeyword
  | OperatorPrecedenceKeyword

parseImportStatementTokens :: [Token] -> Either Diagnostic (SurfaceStatement, [Token])
parseImportStatementTokens = parseImportStatementFromTokens

parseDataStatementTokens :: [Token] -> Either Diagnostic (SurfaceStatement, [Token])
parseDataStatementTokens = parseDataStatementFromTokens

parseCapabilityDeclarationTokens ::
  ImplExpressionParser ->
  [Token] ->
  Either Diagnostic (SurfaceStatement, [Token])
parseCapabilityDeclarationTokens parseImplExpression =
  parseCapabilityDeclarationFromTokens parseImplExpression

parseImportStatementParser :: Parser SurfaceStatement
parseImportStatementParser =
  parseOwnedPrefix parseImportStatementFromTokens

parseDataStatementParser :: Parser SurfaceStatement
parseDataStatementParser =
  parseOwnedPrefix parseDataStatementFromTokens

parseOwnedPrefix :: ([Token] -> Either Diagnostic (a, [Token])) -> Parser a
parseOwnedPrefix parseDeclaration = do
  tokens <- MP.getInput
  case parseDeclaration tokens of
    Left diagnostic -> failTokenParser (diagnosticSummary diagnostic)
    Right (value, remaining) ->
      value <$ consumeParsedPrefix remaining

consumeParsedPrefix :: [Token] -> Parser ()
consumeParsedPrefix remaining =
  case remaining of
    [] -> () <$ MP.takeWhileP Nothing (const True)
    nextToken : _ ->
      () <$ MP.takeWhileP Nothing (/= nextToken)

-- | Parse one statement and return the context visible to the following
-- statement in the same scope. Expressions and nested blocks are supplied by
-- their owning grammars, which keeps this module responsible only for
-- declaration and statement syntax.
parseStatementParser ::
  ExpressionParser ->
  StatementBlockParser ->
  ParserContext ->
  Parser ([SurfaceStatement], ParserContext)
parseStatementParser parseExpression parseBlock context = do
  tokens <- MP.getInput
  let knownAliases = parserKnownAliases context
      declaredOperators = parserDeclaredOperators context
      parseExpressionTokens =
        runTokenParserPrefix "statement expression" (parseExpression context)
      moduleBodyContext =
        ParserContext
          { parserKnownAliases = Set.empty,
            parserDeclaredOperators = [],
            parserStatementContext = ModuleBodyContext
          }
      parseModuleBody =
        runTokenParserPrefix "module body" (parseBlock moduleBodyContext)
  case tokens of
    operatorToken@Token {tokenKind = TIdentifier "operator"} : rest
      | looksLikeOperatorDeclaration rest -> do
          (operatorInfo, remaining) <-
            liftOwnedResult
              (parseOperatorDeclaration (parserStatementContext context) declaredOperators operatorToken rest)
          consumeParsedPrefix remaining
          pure
            ( [],
              context
                { parserDeclaredOperators =
                    operatorInfo : declaredOperators
                }
            )
    _ -> do
      (statements, remaining) <-
        liftOwnedResult
          ( parseStatementFromTokens
              parseExpressionTokens
              parseModuleBody
              context
              tokens
          )
      consumeParsedPrefix remaining
      pure
        ( statements,
          context
            { parserKnownAliases =
                registerImportAliases knownAliases statements
            }
        )

liftOwnedResult :: Either Diagnostic a -> Parser a
liftOwnedResult result =
  case result of
    Left diagnostic -> failTokenParser (diagnosticSummary diagnostic)
    Right value -> pure value

parseOperatorDeclaration :: StatementContext -> [OperatorInfo] -> Token -> [Token] -> Either Diagnostic (OperatorInfo, [Token])
parseOperatorDeclaration context declaredOperators operatorToken tokensAfterKeyword =
  case context of
    NestedBlockContext ->
      rejectNestedOperatorDeclaration operatorToken
    TopLevelContext ->
      parseVisibleOperatorDeclaration
    ModuleBodyContext ->
      parseVisibleOperatorDeclaration
  where
    parseVisibleOperatorDeclaration = do
      (operatorSymbol, afterSymbol) <- parseOperatorDeclarationSymbol tokensAfterKeyword
      validateDeclaredOperatorSymbol declaredOperators operatorToken operatorSymbol
      (fixityKeyword, afterFixityKeyword) <- consumeOperatorFixityKeyword operatorToken afterSymbol
      (operatorInfo, afterFixity) <-
        parseOperatorDeclarationFixity operatorToken operatorSymbol fixityKeyword afterFixityKeyword
      (operatorInfoWithAssociativity, afterAssociativity) <-
        parseOptionalOperatorAssociativity operatorInfo afterFixity
      remaining <-
        consumeOperatorDeclarationDot
          (operatorDeclarationFixityLabel fixityKeyword)
          afterAssociativity
      pure (operatorInfoWithAssociativity, remaining)

parseOperatorDeclarationSymbol :: [Token] -> Either Diagnostic (Text, [Token])
parseOperatorDeclarationSymbol tokens =
  case tokens of
    Token {tokenKind = TOperator operatorSymbol} : rest ->
      Right (operatorSymbol, rest)
    Token {tokenKind = TArrow, tokenLexeme = arrowLexeme} : rest ->
      Right (arrowLexeme, rest)
    token : _ ->
      Left
        ( parseDiagnostic
            ( "expected operator symbol after 'operator' at "
                <> renderSourceSpan (tokenSpan token)
                <> ", found '"
                <> tokenLexeme token
                <> "'"
            )
        )
    [] ->
      Left (parseDiagnostic "expected operator symbol after 'operator' before end of input")

validateDeclaredOperatorSymbol :: [OperatorInfo] -> Token -> Text -> Either Diagnostic ()
validateDeclaredOperatorSymbol declaredOperators operatorToken declaredSymbol
  | isBuiltinOperatorSymbol declaredSymbol =
      Left
        ( parseDiagnostic
            ( "cannot redeclare built-in operator '"
                <> declaredSymbol
                <> "' at "
                <> renderSourceSpan (tokenSpan operatorToken)
            )
        )
  | isReservedOperatorSymbol declaredSymbol =
      Left
        ( parseDiagnostic
            ( "reserved operator symbol '"
                <> declaredSymbol
                <> "' at "
                <> renderSourceSpan (tokenSpan operatorToken)
            )
        )
  | any ((== declaredSymbol) . operatorSymbol) declaredOperators =
      Left
        ( parseDiagnostic
            ( "duplicate operator declaration '"
                <> declaredSymbol
                <> "' at "
                <> renderSourceSpan (tokenSpan operatorToken)
            )
        )
  | isValidUserOperatorSymbol declaredSymbol = Right ()
  | otherwise =
      Left
        ( parseDiagnostic
            ( "invalid operator symbol '"
                <> declaredSymbol
                <> "' at "
                <> renderSourceSpan (tokenSpan operatorToken)
            )
        )

consumeOperatorFixityKeyword :: Token -> [Token] -> Either Diagnostic (OperatorDeclarationFixityKeyword, [Token])
consumeOperatorFixityKeyword operatorToken tokens =
  case tokens of
    Token {tokenKind = TIdentifier "tier"} : rest -> Right (OperatorTierKeyword, rest)
    Token {tokenKind = TIdentifier "precedence"} : rest -> Right (OperatorPrecedenceKeyword, rest)
    token : _ ->
      Left
        ( parseDiagnostic
            ( "expected 'tier' or 'precedence' in operator declaration at "
                <> renderSourceSpan (tokenSpan token)
                <> ", found '"
                <> tokenLexeme token
                <> "'"
            )
        )
    [] ->
      Left
        ( parseDiagnostic
            ( "expected 'tier' or 'precedence' before end of input in operator declaration at "
                <> renderSourceSpan (tokenSpan operatorToken)
            )
        )

parseOperatorDeclarationFixity :: Token -> Text -> OperatorDeclarationFixityKeyword -> [Token] -> Either Diagnostic (OperatorInfo, [Token])
parseOperatorDeclarationFixity operatorToken operatorSymbol fixityKeyword tokens =
  case fixityKeyword of
    OperatorTierKeyword -> parseOperatorDeclarationTier operatorToken operatorSymbol tokens
    OperatorPrecedenceKeyword -> parseOperatorDeclarationPrecedence operatorToken operatorSymbol tokens

parseOperatorDeclarationTier :: Token -> Text -> [Token] -> Either Diagnostic (OperatorInfo, [Token])
parseOperatorDeclarationTier operatorToken operatorSymbol tokens =
  case tokens of
    Token {tokenKind = TInt tier} : rest ->
      case declaredOperatorInfoForTier operatorSymbol tier of
        Just operatorInfo -> Right (operatorInfo, rest)
        Nothing ->
          Left
            ( parseDiagnostic
                ( "operator tier must be between 1 and 5 at "
                    <> renderSourceSpan (tokenSpan operatorToken)
                )
            )
    token : _ ->
      Left
        ( parseDiagnostic
            ( "expected operator tier 1-5 at "
                <> renderSourceSpan (tokenSpan token)
                <> ", found '"
                <> tokenLexeme token
                <> "'"
            )
        )
    [] ->
      Left
        ( parseDiagnostic
            ( "expected operator tier 1-5 before end of input in operator declaration at "
                <> renderSourceSpan (tokenSpan operatorToken)
            )
        )

parseOperatorDeclarationPrecedence :: Token -> Text -> [Token] -> Either Diagnostic (OperatorInfo, [Token])
parseOperatorDeclarationPrecedence operatorToken operatorSymbol tokens =
  case tokens of
    Token {tokenKind = TInt precedence} : rest ->
      case declaredOperatorInfoForPrecedence operatorSymbol precedence of
        Just operatorInfo -> Right (operatorInfo, rest)
        Nothing ->
          Left
            ( parseDiagnostic
                ( "operator precedence must be between 1 and 99 at "
                    <> renderSourceSpan (tokenSpan operatorToken)
                )
            )
    token : _ ->
      Left
        ( parseDiagnostic
            ( "expected operator precedence 1-99 at "
                <> renderSourceSpan (tokenSpan token)
                <> ", found '"
                <> tokenLexeme token
                <> "'"
            )
        )
    [] ->
      Left
        ( parseDiagnostic
            ( "expected operator precedence 1-99 before end of input in operator declaration at "
                <> renderSourceSpan (tokenSpan operatorToken)
            )
        )

parseOptionalOperatorAssociativity :: OperatorInfo -> [Token] -> Either Diagnostic (OperatorInfo, [Token])
parseOptionalOperatorAssociativity operatorInfo tokens =
  case tokens of
    Token {tokenKind = TIdentifier "left"} : rest ->
      Right (operatorInfo {operatorAssociativity = AssocLeft}, rest)
    Token {tokenKind = TIdentifier "right"} : rest ->
      Right (operatorInfo {operatorAssociativity = AssocRight}, rest)
    Token {tokenKind = TIdentifier "nonassoc"} : rest ->
      Right (operatorInfo {operatorAssociativity = AssocNonAssoc}, rest)
    token@Token {tokenKind = TIdentifier {}} : _ ->
      Left
        ( parseDiagnostic
            ( "expected operator associativity 'left', 'right', or 'nonassoc' at "
                <> renderSourceSpan (tokenSpan token)
                <> ", found '"
                <> tokenLexeme token
                <> "'"
            )
        )
    _ -> Right (operatorInfo, tokens)

operatorDeclarationFixityLabel :: OperatorDeclarationFixityKeyword -> Text
operatorDeclarationFixityLabel fixityKeyword =
  case fixityKeyword of
    OperatorTierKeyword -> "tier"
    OperatorPrecedenceKeyword -> "precedence"

consumeOperatorDeclarationDot :: Text -> [Token] -> Either Diagnostic [Token]
consumeOperatorDeclarationDot fixityLabel tokens =
  case tokens of
    Token {tokenKind = TDot} : rest -> Right rest
    token : _ ->
      Left
        ( parseDiagnostic
            ( "expected '.' after operator declaration "
                <> fixityLabel
                <> " at "
                <> renderSourceSpan (tokenSpan token)
                <> ", found '"
                <> tokenLexeme token
                <> "'"
            )
        )
    [] ->
      Left
        ( parseDiagnostic
            ("expected '.' after operator declaration " <> fixityLabel <> " before end of input")
        )

parseStatementFromTokens ::
  ImplExpressionParser ->
  ModuleBodyParser ->
  ParserContext ->
  [Token] ->
  Either Diagnostic ([SurfaceStatement], [Token])
parseStatementFromTokens parseExpression parseModuleBody context tokens =
  case tokens of
    Token {tokenKind = TLParen} :
      operatorToken@Token {tokenKind = TOperator {}} :
      Token {tokenKind = TRParen} :
      afterName@(Token {tokenKind = TColonColon} : _) ->
        singleStatement <$> parseOperatorSignature statementContext declaredOperators operatorToken afterName
    Token {tokenKind = TLParen} :
      operatorToken@Token {tokenKind = TOperator {}} :
      Token {tokenKind = TRParen} :
      Token {tokenKind = TEquals} :
      afterEquals ->
        singleStatement
          <$> parseOperatorBinding
            parseExpression
            statementContext
            declaredOperators
            operatorToken
            afterEquals
    abstractionToken@(Token {tokenKind = TIdentifier name}) : rest
      | isDeclarationContext statementContext,
        looksLikeSupportedCapabilityDeclaration name rest ->
          singleStatement
            <$> parseCapabilityDeclarationTokens
              parseExpression
              (abstractionToken : rest)
      | isDeclarationContext statementContext,
        looksLikeReservedAbstractionDeclaration name rest ->
          rejectReservedAbstractionSyntax abstractionToken
    moduleToken@Token {tokenKind = TModule} : rest ->
      case statementContext of
        TopLevelContext ->
          parseModuleStatementFromTokens parseModuleBody (moduleToken : rest)
        ModuleBodyContext -> rejectNestedModuleDeclaration moduleToken
        NestedBlockContext -> rejectNestedModuleDeclaration moduleToken
    importToken@Token {tokenKind = TImport} : rest ->
      singleStatement <$> parseImportStatementTokens (importToken : rest)
    dataToken@Token {tokenKind = TData} : rest ->
      case statementContext of
        TopLevelContext -> singleStatement <$> parseDataStatementTokens (dataToken : rest)
        ModuleBodyContext -> singleStatement <$> parseDataStatementTokens (dataToken : rest)
        NestedBlockContext -> rejectNestedDataDeclaration dataToken
    nameToken : afterName@(Token {tokenKind = TColonColon} : _)
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
        shouldParseQualifiedAliasStatement knownAliases name nameToken afterName ->
          singleStatement <$> parseExprStatement parseExpression tokens
      | TIdentifier name <- tokenKind nameToken ->
          singleStatement <$> parseSignature (mkIdentifier name) nameToken afterName
    nameToken : afterName@(Token {tokenKind = TEquals} : _)
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
          singleStatement <$> parseLet parseExpression (mkIdentifier name) nameToken afterName
    _ -> singleStatement <$> parseExprStatement parseExpression tokens
  where
    knownAliases = parserKnownAliases context
    declaredOperators = parserDeclaredOperators context
    statementContext = parserStatementContext context
    singleStatement (statement, remaining) = ([statement], remaining)

parseOperatorBinding ::
  ImplExpressionParser ->
  StatementContext ->
  [OperatorInfo] ->
  Token ->
  [Token] ->
  Either Diagnostic (SurfaceStatement, [Token])
parseOperatorBinding parseExpression context declaredOperators operatorToken tokensAfterEquals =
  case context of
    NestedBlockContext -> rejectNestedOperatorBinding operatorToken
    TopLevelContext -> parseVisibleOperatorBinding
    ModuleBodyContext -> parseVisibleOperatorBinding
  where
    parseVisibleOperatorBinding =
      case tokenKind operatorToken of
        TOperator bindingSymbol
          | isBuiltinOperatorSymbol bindingSymbol ->
              Left
                ( parseDiagnostic
                    ( "cannot bind built-in operator '"
                        <> bindingSymbol
                        <> "' at "
                        <> renderSourceSpan (tokenSpan operatorToken)
                    )
                )
          | not (operatorDeclared bindingSymbol) ->
              Left
                ( parseDiagnostic
                    ( "operator '"
                        <> bindingSymbol
                        <> "' must be declared before binding at "
                        <> renderSourceSpan (tokenSpan operatorToken)
                    )
                )
          | otherwise -> do
              (valueExpr, afterExpr) <- parseExpression tokensAfterEquals
              remaining <- consumeDot afterExpr
              pure
                ( SSLet
                    (mkOperatorBindingIdentifier bindingSymbol)
                    (tokenSpan operatorToken)
                    valueExpr,
                  remaining
                )
        _ ->
          Left
            ( parseDiagnostic
                ( "internal parser error at "
                    <> renderSourceSpan (tokenSpan operatorToken)
                    <> ": expected operator token in operator binding"
                )
            )

    operatorDeclared bindingSymbol =
      any ((== bindingSymbol) . operatorSymbol) declaredOperators

parseOperatorSignature ::
  StatementContext ->
  [OperatorInfo] ->
  Token ->
  [Token] ->
  Either Diagnostic (SurfaceStatement, [Token])
parseOperatorSignature context declaredOperators operatorToken tokensAfterName =
  case context of
    NestedBlockContext -> rejectNestedOperatorSignature operatorToken
    TopLevelContext -> parseVisibleOperatorSignature
    ModuleBodyContext -> parseVisibleOperatorSignature
  where
    parseVisibleOperatorSignature =
      case tokenKind operatorToken of
        TOperator signatureSymbol
          | isBuiltinOperatorSymbol signatureSymbol ->
              Left
                ( parseDiagnostic
                    ( "cannot sign built-in operator '"
                        <> signatureSymbol
                        <> "' at "
                        <> renderSourceSpan (tokenSpan operatorToken)
                    )
                )
          | not (operatorDeclared signatureSymbol) ->
              Left
                ( parseDiagnostic
                    ( "operator '"
                        <> signatureSymbol
                        <> "' must be declared before signature at "
                        <> renderSourceSpan (tokenSpan operatorToken)
                    )
                )
          | otherwise ->
              parseSignature
                (mkOperatorBindingIdentifier signatureSymbol)
                operatorToken
                tokensAfterName
        _ ->
          Left
            ( parseDiagnostic
                ( "internal parser error at "
                    <> renderSourceSpan (tokenSpan operatorToken)
                    <> ": expected operator token in operator signature"
                )
            )

    operatorDeclared signatureSymbol =
      any ((== signatureSymbol) . operatorSymbol) declaredOperators

parseSignature :: Identifier -> Token -> [Token] -> Either Diagnostic (SurfaceStatement, [Token])
parseSignature name nameToken tokensAfterName =
  case tokensAfterName of
    Token {tokenKind = TColonColon} : rest -> do
      (signatureTokens, remainingAfterDot) <- collectUntilDot rest
      pure
        ( SSSignature name (tokenSpan nameToken) (parseSignaturePayload signatureTokens),
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

parseLet ::
  ImplExpressionParser ->
  Identifier ->
  Token ->
  [Token] ->
  Either Diagnostic (SurfaceStatement, [Token])
parseLet parseExpression name nameToken tokensAfterName =
  case tokensAfterName of
    Token {tokenKind = TEquals} : rest -> do
      (valueExpr, afterExpr) <- parseExpression rest
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

parseExprStatement :: ImplExpressionParser -> [Token] -> Either Diagnostic (SurfaceStatement, [Token])
parseExprStatement parseExpression tokens =
  case tokens of
    [] -> Left (parseDiagnostic "expected expression before end of input")
    firstToken : _ -> do
      (expr, afterExpr) <- parseExpression tokens
      remaining <- consumeDot afterExpr
      pure (SSExpr (tokenSpan firstToken) expr, remaining)

parseModuleStatementFromTokens ::
  ModuleBodyParser ->
  [Token] ->
  Either Diagnostic ([SurfaceStatement], [Token])
parseModuleStatementFromTokens parseModuleBody tokens =
  case tokens of
    moduleToken@Token {tokenKind = TModule} : tokensAfterModuleKeyword -> do
      (modulePath, afterModulePath) <- parseModulePath tokensAfterModuleKeyword
      (moduleExports, beforeModuleBody) <-
        case afterModulePath of
          Token {tokenKind = TLParen} : afterLeftParen -> do
            (exportNames, remaining) <- parseModuleExportList afterLeftParen
            pure (Just exportNames, remaining)
          _ -> pure (Nothing, afterModulePath)
      case beforeModuleBody of
        Token {tokenKind = TLBrace} : tokensAfterLeftBrace -> do
          (bodyStatements, remaining) <- parseModuleBody tokensAfterLeftBrace
          pure
            ( SSModule (tokenSpan moduleToken) modulePath moduleExports
                : bodyStatements,
              remaining
            )
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
    [] ->
      Left (parseDiagnostic "expected 'module' before end of input")
    token : _ ->
      Left
        ( parseDiagnostic
            ( "expected 'module' at "
                <> renderSourceSpan (tokenSpan token)
                <> ", found '"
                <> tokenLexeme token
                <> "'"
            )
        )

parseImportStatementFromTokens :: [Token] -> Either Diagnostic (SurfaceStatement, [Token])
parseImportStatementFromTokens tokens =
  case tokens of
    importToken@Token {tokenKind = TImport} : tokensAfterImportKeyword -> do
      (modulePath, afterModulePath) <- parseModulePath tokensAfterImportKeyword
      parseImportTail importToken modulePath afterModulePath
    [] ->
      Left (parseDiagnostic "expected 'import' before end of input")
    token : _ ->
      Left
        ( parseDiagnostic
            ( "expected 'import' at "
                <> renderSourceSpan (tokenSpan token)
                <> ", found '"
                <> tokenLexeme token
                <> "'"
            )
        )

parseImportTail :: Token -> [Text] -> [Token] -> Either Diagnostic (SurfaceStatement, [Token])
parseImportTail importToken modulePath tokensAfterModulePath =
  case tokensAfterModulePath of
    Token {tokenKind = TDot} : rest ->
      pure (SSImport (tokenSpan importToken) modulePath Nothing Nothing, rest)
    asToken@Token {tokenKind = TAs} : rest ->
      case rest of
        aliasToken@Token {tokenKind = TIdentifier aliasName} : afterAlias
          | isReservedLiteralName aliasName ->
              Left
                ( parseDiagnostic
                    ( "reserved literal '"
                        <> aliasName
                        <> "' cannot be used as an import alias at "
                        <> renderSourceSpan (tokenSpan aliasToken)
                    )
                )
          | otherwise ->
              case afterAlias of
                parenToken@Token {tokenKind = TLParen} : _ ->
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
        asToken@Token {tokenKind = TAs} : _ ->
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

parseDataStatementFromTokens :: [Token] -> Either Diagnostic (SurfaceStatement, [Token])
parseDataStatementFromTokens tokens =
  case tokens of
    dataToken@Token {tokenKind = TData} : tokensAfterDataKeyword -> do
      (typeName, afterTypeName) <- parseDataTypeName tokensAfterDataKeyword
      (typeParameters, afterTypeParameters) <- parseDataTypeParameters afterTypeName
      afterEquals <-
        consumeEquals
          afterTypeParameters
          ( "expected '=' before end of input after data type name at "
              <> renderSourceSpan (tokenSpan dataToken)
          )
      (constructors, remaining) <- parseDataConstructors typeName typeParameters afterEquals
      pure (SSData (tokenSpan dataToken) typeName typeParameters constructors, remaining)
    [] ->
      Left (parseDiagnostic "expected 'data' before end of input")
    token : _ ->
      Left
        ( parseDiagnostic
            ( "expected 'data' at "
                <> renderSourceSpan (tokenSpan token)
                <> ", found '"
                <> tokenLexeme token
                <> "'"
            )
        )

parseCapabilityDeclarationFromTokens ::
  ImplExpressionParser ->
  [Token] ->
  Either Diagnostic (SurfaceStatement, [Token])
parseCapabilityDeclarationFromTokens parseImplExpression tokens =
  case tokens of
    declarationToken@Token {tokenKind = TIdentifier declarationKind} : tokensAfterKeyword ->
      case declarationKind of
        "class" ->
          parseCapabilityDeclaration parseImplExpression declarationKind declarationToken tokensAfterKeyword
        "impl" ->
          parseCapabilityDeclaration parseImplExpression declarationKind declarationToken tokensAfterKeyword
        _ ->
          rejectReservedAbstractionSyntax declarationToken
    [] ->
      Left (parseDiagnostic "expected capability declaration before end of input")
    token : _ ->
      Left
        ( parseDiagnostic
            ( "expected capability declaration at "
                <> renderSourceSpan (tokenSpan token)
                <> ", found '"
                <> tokenLexeme token
                <> "'"
            )
        )

parseCapabilityDeclaration ::
  ImplExpressionParser ->
  Text ->
  Token ->
  [Token] ->
  Either Diagnostic (SurfaceStatement, [Token])
parseCapabilityDeclaration parseImplExpression declarationKind declarationToken tokensAfterKeyword = do
  (capabilityName, maybeHeaderArguments, headerRemaining) <-
    parseCapabilityHeaderName declarationKind declarationToken tokensAfterKeyword
  let headerArguments =
        case maybeHeaderArguments of
          Just arguments -> arguments
          Nothing -> []
  case declarationKind of
    "class" -> do
      classParameters <- validateClassHeaderParameters declarationToken maybeHeaderArguments
      (capabilityBody, afterBody) <- parseCapabilityDeclarationBody parseImplExpression declarationKind declarationToken headerRemaining
      remaining <- consumeDot afterBody
      case capabilityBody of
        CapabilityClassBody methodSignatures ->
          Right (SSClass (tokenSpan declarationToken) capabilityName classParameters methodSignatures, remaining)
        CapabilityImplBody {} ->
          rejectReservedAbstractionSyntax declarationToken
    "impl" -> do
      (capabilityBody, afterBody) <- parseCapabilityDeclarationBody parseImplExpression declarationKind declarationToken headerRemaining
      remaining <- consumeDot afterBody
      case capabilityBody of
        CapabilityImplBody methods ->
          if surfaceConcreteImplArguments headerArguments
            then Right (SSImpl (tokenSpan declarationToken) capabilityName headerArguments methods, remaining)
            else
              Left
                ( parseDiagnostic
                    ( "impl declarations require a concrete impl target at "
                        <> renderSourceSpan (tokenSpan declarationToken)
                    )
                )
        CapabilityClassBody {} ->
          rejectReservedAbstractionSyntax declarationToken
    _ ->
      rejectReservedAbstractionSyntax declarationToken

parseCapabilityHeaderName :: Text -> Token -> [Token] -> Either Diagnostic (Identifier, Maybe [SurfaceConstrainedSignatureType], [Token])
parseCapabilityHeaderName declarationKind declarationToken tokensAfterKeyword =
  case tokensAfterKeyword of
    Token {tokenKind = TIdentifier candidateName, tokenSpan = nameSpan} : rest
      | isConstructorIdentifierText candidateName ->
          parseCapabilityHeaderTail (mkIdentifier candidateName) rest
      | otherwise ->
          Left
            ( parseDiagnostic
                ( "expected uppercase capability name at "
                    <> renderSourceSpan nameSpan
                    <> ", found '"
                    <> candidateName
                    <> "'"
                )
            )
    Token {tokenKind = TLBrace} : _ ->
      Left
        ( parseDiagnostic
            ( "expected capability name before '{' in "
                <> declarationKind
                <> " declaration at "
                <> renderSourceSpan (tokenSpan declarationToken)
            )
        )
    Token {tokenKind = TDot} : _ ->
      Left
        ( parseDiagnostic
            ( "expected capability name before '.' in "
                <> declarationKind
                <> " declaration at "
                <> renderSourceSpan (tokenSpan declarationToken)
            )
        )
    token : _ ->
      Left
        ( parseDiagnostic
            ( "expected capability name at "
                <> renderSourceSpan (tokenSpan token)
                <> ", found '"
                <> tokenLexeme token
                <> "'"
            )
        )
    [] ->
      Left
        ( parseDiagnostic
            ( "expected capability name before end of input in "
                <> declarationKind
                <> " declaration at "
                <> renderSourceSpan (tokenSpan declarationToken)
            )
        )
  where
    parseCapabilityHeaderTail capabilityName tokens =
      case tokens of
        Token {tokenKind = TLParen} : rest -> do
          (headerArguments, afterHeaderParameters) <- parseParenthesizedCapabilityHeader rest
          requireCapabilityBodyStart capabilityName (Just headerArguments) afterHeaderParameters
        _ -> requireCapabilityBodyStart capabilityName Nothing tokens

    requireCapabilityBodyStart capabilityName headerArguments tokens =
      case tokens of
        Token {tokenKind = TLBrace} : _ ->
          Right (capabilityName, headerArguments, tokens)
        Token {tokenKind = TDot} : _ ->
          Left
            ( parseDiagnostic
                ( "expected '{' before '.' in "
                    <> declarationKind
                    <> " declaration at "
                    <> renderSourceSpan (tokenSpan declarationToken)
                )
            )
        token : _ ->
          Left
            ( parseDiagnostic
                ( "unexpected token '"
                    <> tokenLexeme token
                    <> "' in "
                    <> declarationKind
                    <> " declaration header at "
                    <> renderSourceSpan (tokenSpan token)
                )
            )
        [] ->
          Left
            ( parseDiagnostic
                ( "expected '{' before end of input in "
                    <> declarationKind
                    <> " declaration at "
                    <> renderSourceSpan (tokenSpan declarationToken)
                )
            )

    parseParenthesizedCapabilityHeader tokens = do
      (argumentTokens, remaining) <- collectParenthesizedCapabilityHeader tokens
      headerArguments <-
        if null argumentTokens
          then Right []
          else
            case splitTopLevelCommaTokens argumentTokens >>= traverse parseConstrainedSignatureType of
              Right parsedArguments -> Right parsedArguments
              Left _ ->
                Left
                  ( parseDiagnostic
                      ( "unsupported "
                          <> declarationKind
                          <> " declaration header arguments at "
                          <> renderSourceSpan (tokenSpan declarationToken)
                      )
                  )
      Right (headerArguments, remaining)

    collectParenthesizedCapabilityHeader tokens =
      go 1 [] tokens
      where
        go depth acc remaining =
          case remaining of
            token@Token {tokenKind = TLParen} : rest ->
              go (depth + 1) (token : acc) rest
            token@Token {tokenKind = TRParen} : rest
              | depth == 1 -> Right (reverse acc, rest)
              | otherwise -> go (depth - 1) (token : acc) rest
            Token {tokenKind = TLBrace, tokenSpan = braceSpan} : _ ->
              Left
                ( parseDiagnostic
                    ( "expected ')' before '{' in "
                        <> declarationKind
                        <> " declaration header at "
                        <> renderSourceSpan braceSpan
                    )
                )
            token : rest ->
              go depth (token : acc) rest
            [] ->
              Left
                ( parseDiagnostic
                    ( "expected ')' before end of input in "
                        <> declarationKind
                        <> " declaration header at "
                        <> renderSourceSpan (tokenSpan declarationToken)
                    )
                )

parseCapabilityDeclarationBody ::
  ImplExpressionParser ->
  Text ->
  Token ->
  [Token] ->
  Either Diagnostic (CapabilityDeclarationBody, [Token])
parseCapabilityDeclarationBody parseImplExpression declarationKind declarationToken tokens =
  case tokens of
    Token {tokenKind = TLBrace} : rest ->
      case declarationKind of
        "class" -> do
          (methodSignatures, afterBody) <- consumeClassBody Set.empty [] rest
          Right (CapabilityClassBody methodSignatures, afterBody)
        "impl" -> do
          (methods, afterBody) <- consumeImplBody Set.empty [] rest
          Right (CapabilityImplBody methods, afterBody)
        _ ->
          rejectReservedAbstractionSyntax declarationToken
    [] ->
      Left
        ( parseDiagnostic
            ( "expected '{' before end of input in "
                <> declarationKind
                <> " declaration at "
                <> renderSourceSpan (tokenSpan declarationToken)
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
  where
    consumeClassBody seenMethodNames reversedMethods remainingTokens =
      case remainingTokens of
        [] ->
          Left
            ( parseDiagnostic
                ( "expected '}' before end of input in "
                    <> declarationKind
                    <> " declaration at "
                    <> renderSourceSpan (tokenSpan declarationToken)
                )
            )
        Token {tokenKind = TRBrace} : rest ->
          Right (reverse reversedMethods, rest)
        operatorToken@Token {tokenKind = TIdentifier "operator"} : _ ->
          rejectNestedOperatorDeclaration operatorToken
        methodToken@Token {tokenKind = TIdentifier methodName, tokenSpan = methodSpan} : Token {tokenKind = TColonColon} : rest
          | Set.member methodName seenMethodNames ->
              Left
                ( parseDiagnostic
                    ( "duplicate method signature '"
                        <> methodName
                        <> "' in class declaration at "
                        <> renderSourceSpan methodSpan
                    )
                )
          | otherwise -> do
              (signatureTokens, afterSignature) <- collectUntilDot rest
              let methodSignature =
                    SurfaceClassMethodSignature
                      (mkIdentifier methodName)
                      (tokenSpan methodToken)
                      (parseSignaturePayload signatureTokens)
              consumeClassBody
                (Set.insert methodName seenMethodNames)
                (methodSignature : reversedMethods)
                afterSignature
        Token {tokenKind = TIdentifier methodName, tokenSpan = methodSpan} : Token {tokenKind = TEquals} : _ ->
          Left
            ( parseDiagnostic
                ( "unsupported class method body/default syntax for '"
                    <> methodName
                    <> "' at "
                    <> renderSourceSpan methodSpan
                    <> ": only signature-only method declarations are implemented in jazz-next"
                )
            )
        token : _ ->
          Left
            ( parseDiagnostic
                ( "expected signature-only method declaration or '}' in "
                    <> declarationKind
                    <> " declaration body at "
                    <> renderSourceSpan (tokenSpan token)
                    <> ", found '"
                    <> tokenLexeme token
                    <> "'"
                )
            )

    consumeImplBody seenMethodNames reversedMethods remainingTokens =
      case remainingTokens of
        [] ->
          Left
            ( parseDiagnostic
                ( "expected '}' before end of input in "
                    <> declarationKind
                    <> " declaration at "
                    <> renderSourceSpan (tokenSpan declarationToken)
                )
            )
        Token {tokenKind = TRBrace} : rest ->
          Right (reverse reversedMethods, rest)
        operatorToken@Token {tokenKind = TIdentifier "operator"} : _ ->
          rejectNestedOperatorDeclaration operatorToken
        methodToken@Token {tokenKind = TIdentifier methodName, tokenSpan = methodSpan} :
          Token {tokenKind = TEquals} :
          afterEquals
            | Set.member methodName seenMethodNames ->
                Left
                  ( parseDiagnostic
                      ( "duplicate method binding '"
                          <> methodName
                          <> "' in impl declaration at "
                          <> renderSourceSpan methodSpan
                      )
                  )
            | otherwise -> do
                (methodExpr, afterExpr) <- parseImplExpression afterEquals
                afterMethod <- consumeDot afterExpr
                let method =
                      SurfaceImplMethod
                        (mkIdentifier methodName)
                        (tokenSpan methodToken)
                        methodExpr
                consumeImplBody
                  (Set.insert methodName seenMethodNames)
                  (method : reversedMethods)
                  afterMethod
        methodToken@Token {tokenKind = TIdentifier methodName, tokenSpan = methodSpan} : Token {tokenKind = TColonColon} : _ ->
          Left
            ( parseDiagnostic
                ( "expected ordinary method binding for '"
                    <> methodName
                    <> "' in impl declaration body at "
                    <> renderSourceSpan methodSpan
                )
            )
        token : _ ->
          Left
            ( parseDiagnostic
                ( "expected ordinary method binding or '}' in impl declaration body at "
                    <> renderSourceSpan (tokenSpan token)
                    <> ", found '"
                    <> tokenLexeme token
                    <> "'"
                )
            )

surfaceConcreteImplArguments :: [SurfaceConstrainedSignatureType] -> Bool
surfaceConcreteImplArguments arguments =
  case arguments of
    [argument] -> surfaceConcreteConstraintArgument argument
    _ -> False

surfaceConcreteConstraintArgument :: SurfaceConstrainedSignatureType -> Bool
surfaceConcreteConstraintArgument signatureType =
  case signatureType of
    SurfaceConstrainedTypeName name ->
      not (surfaceIdentifierLooksLikeTypeVariable name)
    SurfaceConstrainedTypeApplication name arguments ->
      not (surfaceIdentifierLooksLikeTypeVariable name) && all surfaceConcreteConstraintArgument arguments
    SurfaceConstrainedTypeList innerType ->
      surfaceConcreteConstraintArgument innerType
    SurfaceConstrainedTypeTuple elementTypes ->
      all surfaceConcreteConstraintArgument elementTypes
    SurfaceConstrainedTypeFunction {} ->
      False

surfaceIdentifierLooksLikeTypeVariable :: Identifier -> Bool
surfaceIdentifierLooksLikeTypeVariable name =
  case Text.uncons (identifierText name) of
    Just (c, _) -> isLower c
    Nothing -> False

validateClassHeaderParameters :: Token -> Maybe [SurfaceConstrainedSignatureType] -> Either Diagnostic [Identifier]
validateClassHeaderParameters declarationToken maybeHeaderArguments =
  case maybeHeaderArguments of
    Nothing ->
      Left
        ( parseDiagnostic
            ( "class declarations require an explicit parameter list at "
                <> renderSourceSpan (tokenSpan declarationToken)
            )
        )
    Just [] ->
      Left
        ( parseDiagnostic
            ( "class declarations require at least one explicit lowercase parameter at "
                <> renderSourceSpan (tokenSpan declarationToken)
            )
        )
    Just headerArguments -> do
      classParameters <- traverse classParameterFromHeaderArgument headerArguments
      case duplicateClassParameterName classParameters of
        Just duplicateName ->
          Left
            ( parseDiagnostic
                ( "duplicate class parameter '"
                    <> duplicateName
                    <> "' at "
                    <> renderSourceSpan (tokenSpan declarationToken)
                )
            )
        Nothing ->
          case classParameters of
            [_] -> Right classParameters
            _ ->
              Left
                ( parseDiagnostic
                    ( "class declarations currently support exactly one parameter at "
                        <> renderSourceSpan (tokenSpan declarationToken)
                    )
                )
  where
    classParameterFromHeaderArgument argument =
      case argument of
        SurfaceConstrainedTypeName parameterName
          | surfaceIdentifierLooksLikeTypeVariable parameterName ->
              Right parameterName
        _ ->
          Left
            ( parseDiagnostic
                ( "class parameters must be lowercase type variables at "
                    <> renderSourceSpan (tokenSpan declarationToken)
                )
            )

    duplicateClassParameterName classParameters =
      go Set.empty classParameters

    go seen remaining =
      case remaining of
        [] -> Nothing
        parameter : rest ->
          let parameterText = identifierText parameter
           in if Set.member parameterText seen
                then Just parameterText
                else go (Set.insert parameterText seen) rest

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

parseDataTypeParameters :: [Token] -> Either Diagnostic ([Identifier], [Token])
parseDataTypeParameters tokens = go Set.empty [] tokens
  where
    go seenParameters revParameters allTokens =
      case allTokens of
        Token {tokenKind = TEquals} : _ ->
          Right (reverse revParameters, allTokens)
        Token {tokenKind = TIdentifier parameterName, tokenSpan = parameterSpan} : rest
          | isTypeParameterIdentifierText parameterName ->
              if Set.member parameterName seenParameters
                then
                  Left
                    ( parseDiagnostic
                        ("duplicate type parameter '" <> parameterName <> "' in data declaration")
                    )
                else
                  go
                    (Set.insert parameterName seenParameters)
                    (mkIdentifier parameterName : revParameters)
                    rest
          | otherwise ->
              Left
                ( parseDiagnostic
                    ( "expected lowercase type parameter or '=' at "
                        <> renderSourceSpan parameterSpan
                        <> ", found '"
                        <> parameterName
                        <> "'"
                    )
                )
        _ ->
          Right (reverse revParameters, allTokens)

parseDataConstructors :: Identifier -> [Identifier] -> [Token] -> Either Diagnostic ([SurfaceDataConstructor], [Token])
parseDataConstructors typeName typeParameters tokensAfterEquals = do
  (firstConstructor, afterFirstConstructor) <- parseDataConstructor typeName typeParameterNames tokensAfterEquals
  go
    (Set.singleton (surfaceDataConstructorName firstConstructor))
    [firstConstructor]
    afterFirstConstructor
  where
    typeParameterNames = Set.fromList (map identifierText typeParameters)

    go seenConstructors revConstructors allTokens =
      case allTokens of
        Token {tokenKind = TDot} : rest ->
          Right (reverse revConstructors, rest)
        Token {tokenKind = TOperator "|"} : rest -> do
          (nextConstructor, afterNextConstructor) <- parseDataConstructor typeName typeParameterNames rest
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

parseDataConstructor :: Identifier -> Set Text -> [Token] -> Either Diagnostic (SurfaceDataConstructor, [Token])
parseDataConstructor typeName typeParameterNames tokens =
  case tokens of
    Token {tokenKind = TIdentifier constructorName, tokenSpan = constructorSpan} : rest
      | isConstructorIdentifierText constructorName -> do
          (constructorArguments, remaining) <- parseDataConstructorArguments typeName typeParameterNames [] rest
          Right
            ( SurfaceDataConstructor (mkIdentifier constructorName) constructorArguments,
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

parseDataConstructorArguments ::
  Identifier ->
  Set Text ->
  [SurfaceDataConstructorArgument] ->
  [Token] ->
  Either Diagnostic ([SurfaceDataConstructorArgument], [Token])
parseDataConstructorArguments typeName typeParameterNames revArguments allTokens =
  case allTokens of
    Token {tokenKind = TOperator "|"} : _ ->
      Right (reverse revArguments, allTokens)
    Token {tokenKind = TDot} : _ ->
      Right (reverse revArguments, allTokens)
    [] ->
      Right (reverse revArguments, allTokens)
    _ -> do
      (constructorArgument, remaining) <- parseDataConstructorArgument typeName typeParameterNames allTokens
      parseDataConstructorArguments typeName typeParameterNames (constructorArgument : revArguments) remaining

parseDataConstructorArgument :: Identifier -> Set Text -> [Token] -> Either Diagnostic (SurfaceDataConstructorArgument, [Token])
parseDataConstructorArgument typeName typeParameterNames tokens =
  case tokens of
    Token {tokenKind = TIdentifier argumentName} : rest
      | not (Set.null typeParameterNames)
          && isTypeParameterIdentifierText argumentName
          && Set.notMember argumentName typeParameterNames ->
          Left
            ( parseDiagnostic
                ( "constructor payload type parameter '"
                    <> argumentName
                    <> "' is not declared in data type '"
                    <> identifierText typeName
                    <> "'"
                )
            )
      | otherwise ->
          Right (SurfaceDataConstructorArgumentName (mkIdentifier argumentName), rest)
    Token {tokenKind = TLParen} : rest ->
      fmap ((,) SurfaceDataConstructorArgumentOpaque) (consumeBalancedDataConstructorGroup 1 0 rest)
    Token {tokenKind = TLBracket} : rest ->
      fmap ((,) SurfaceDataConstructorArgumentOpaque) (consumeBalancedDataConstructorGroup 0 1 rest)
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

parseModulePath :: [Token] -> Either Diagnostic ([Text], [Token])
parseModulePath tokens =
  case tokens of
    [] -> Left (parseDiagnostic "expected module path before end of input")
    Token {tokenKind = TIdentifier firstSegment} : rest ->
      go [firstSegment] rest
      where
        go revSegments allTokens =
          case allTokens of
            Token {tokenKind = TColonColon} : Token {tokenKind = TIdentifier nextSegment} : remaining ->
              go (nextSegment : revSegments) remaining
            separatorToken@Token {tokenKind = TColonColon} : [] ->
              Left
                ( parseDiagnostic
                    ( "expected module path segment before end of input at "
                        <> renderSourceSpan (tokenSpan separatorToken)
                    )
                )
            separatorToken@Token {tokenKind = TColonColon} : token : _
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

parseImportSymbolList :: [Token] -> Either Diagnostic ([Text], [Token])
parseImportSymbolList tokensAfterLeftParen =
  case tokensAfterLeftParen of
    token@Token {tokenKind = TRParen} : _ ->
      Left
        ( parseDiagnostic
            ( "expected at least one import symbol before ')' at "
                <> renderSourceSpan (tokenSpan token)
            )
        )
    _ ->
      parseNonEmptyUniqueList
        "import symbol"
        "import symbol list"
        (\name -> "'" <> name <> "'")
        parseImportSymbol
        tokensAfterLeftParen

parseModuleExportList :: [Token] -> Either Diagnostic ([ModuleExportSelector], [Token])
parseModuleExportList tokensAfterLeftParen =
  case tokensAfterLeftParen of
    Token {tokenKind = TRParen} : rest -> Right ([], rest)
    _ ->
      parseNonEmptyUniqueList
        "module export"
        "module export list"
        renderModuleExportSelector
        parseModuleExport
        tokensAfterLeftParen

parseNonEmptyUniqueList ::
  Ord item =>
  Text ->
  Text ->
  (item -> Text) ->
  ([Token] -> Either Diagnostic (item, SourceSpan, [Token])) ->
  [Token] ->
  Either Diagnostic ([item], [Token])
parseNonEmptyUniqueList itemDescription listDescription renderItem parseItem tokens = do
  (firstItem, _, afterFirstItem) <- parseItem tokens
  go [firstItem] (Set.singleton firstItem) afterFirstItem
  where
    go reversedItems seenItems allTokens =
      case allTokens of
        Token {tokenKind = TComma} : rest -> do
          (nextItem, itemSpan, afterNextItem) <- parseItem rest
          if Set.member nextItem seenItems
            then
              Left
                ( parseDiagnostic
                    ( "duplicate "
                        <> itemDescription
                        <> " "
                        <> renderItem nextItem
                        <> " at "
                        <> renderSourceSpan itemSpan
                    )
                )
            else
              go
                (nextItem : reversedItems)
                (Set.insert nextItem seenItems)
                afterNextItem
        Token {tokenKind = TRParen} : rest -> Right (reverse reversedItems, rest)
        [] ->
          Left
            ( parseDiagnostic
                ("expected ')' before end of input in " <> listDescription)
            )
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

parseModuleExport :: [Token] -> Either Diagnostic (ModuleExportSelector, SourceSpan, [Token])
parseModuleExport tokens =
  case tokens of
    Token {tokenKind = TIdentifier prefix} : Token {tokenKind = TIdentifier exportName, tokenSpan = exportSpan} : rest
      | Just namespace <- moduleExportNamespacePrefix prefix ->
          Right (ModuleExportSelector (Just namespace) exportName, exportSpan, rest)
    Token {tokenKind = TIdentifier exportName, tokenSpan = exportSpan} : rest ->
      Right (ModuleExportSelector Nothing exportName, exportSpan, rest)
    [] -> Left (parseDiagnostic "expected module export name before end of input")
    token : _ ->
      Left
        ( parseDiagnostic
            ( "expected module export name at "
                <> renderSourceSpan (tokenSpan token)
                <> ", found '"
                <> tokenLexeme token
                <> "'"
            )
        )

moduleExportNamespacePrefix :: Text -> Maybe NameNamespace
moduleExportNamespacePrefix prefix =
  case prefix of
    "value" -> Just ValueNamespace
    "constructor" -> Just ConstructorNamespace
    "type" -> Just TypeNamespace
    "class" -> Just CapabilityNamespace
    _ -> Nothing

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

collectUntilDot :: [Token] -> Either Diagnostic ([Token], [Token])
collectUntilDot = go []
  where
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
    Token {tokenKind = TIdentifier "operator"} : rest
      | looksLikeOperatorDeclaration rest -> True
    Token {tokenKind = TLParen} :
      Token {tokenKind = TOperator {}} :
      Token {tokenKind = TRParen} :
      Token {tokenKind = TColonColon} :
      _ -> True
    Token {tokenKind = TLParen} :
      Token {tokenKind = TOperator {}} :
      Token {tokenKind = TRParen} :
      Token {tokenKind = TEquals} :
      _ -> True
    Token {tokenKind = TIdentifier name} : rest
      | looksLikeReservedAbstractionDeclaration name rest -> True
    Token {tokenKind = TIdentifier _} : Token {tokenKind = TEquals} : _ -> True
    Token {tokenKind = TIdentifier _} : Token {tokenKind = TColonColon} : _ -> True
    _ -> False

isDeclarationContext :: StatementContext -> Bool
isDeclarationContext context =
  case context of
    TopLevelContext -> True
    ModuleBodyContext -> True
    NestedBlockContext -> False

looksLikeSupportedCapabilityDeclaration :: Text -> [Token] -> Bool
looksLikeSupportedCapabilityDeclaration name tokensAfterKeyword =
  case name of
    "class" -> looksLikeAbstractionDeclaration tokensAfterKeyword
    "impl" -> looksLikeAbstractionDeclaration tokensAfterKeyword
    _ -> False

registerImportAliases :: Set Text -> [SurfaceStatement] -> Set Text
registerImportAliases =
  foldl registerImportAlias
  where
    registerImportAlias knownAliases statement =
      case statement of
        SSImport _ _ (Just aliasName) Nothing -> Set.insert aliasName knownAliases
        _ -> knownAliases

shouldParseQualifiedAliasStatement :: Set Text -> Text -> Token -> [Token] -> Bool
shouldParseQualifiedAliasStatement knownAliases name nameToken tokensAfterName =
  case tokensAfterName of
    colonToken@Token {tokenKind = TColonColon} : _ ->
      isImmediatelyAfter nameToken colonToken
        && ( Set.member name knownAliases
               || not (shouldParseCompactSignature name nameToken tokensAfterName)
           )
    _ -> False

shouldParseCompactSignature :: Text -> Token -> [Token] -> Bool
shouldParseCompactSignature name nameToken tokensAfterName =
  case parseSignature (mkIdentifier name) nameToken tokensAfterName of
    Right (SSSignature _ _ signaturePayload, remaining) ->
      isSupportedSignaturePayload signaturePayload
        || isLikelyUnsupportedSignaturePayload signaturePayload
        || not (isConstructorIdentifierText name)
        || nextStatementStartsMatchingBinding name remaining
    Left _ -> False

isSupportedSignaturePayload :: SurfaceSignaturePayload -> Bool
isSupportedSignaturePayload signaturePayload =
  case signaturePayload of
    SurfaceSignatureType _ -> True
    SurfaceConstrainedSignature _ _ -> True
    SurfaceUnsupportedSignature _ -> False

isLikelyUnsupportedSignaturePayload :: SurfaceSignaturePayload -> Bool
isLikelyUnsupportedSignaturePayload signaturePayload =
  case signaturePayload of
    SurfaceUnsupportedSignature [SurfaceSignatureNameToken name] ->
      isSingleLetterTypeVariable name
    SurfaceUnsupportedSignature tokens -> any isSignatureSyntaxToken tokens
    _ -> False

isSingleLetterTypeVariable :: Text -> Bool
isSingleLetterTypeVariable name =
  case Text.uncons name of
    Just (firstChar, rest) -> Text.null rest && isLower firstChar
    Nothing -> False

isSignatureSyntaxToken :: SurfaceSignatureToken -> Bool
isSignatureSyntaxToken signatureToken =
  case signatureToken of
    SurfaceSignatureArrowToken -> True
    SurfaceSignatureAtToken -> True
    SurfaceSignatureColonToken -> True
    SurfaceSignatureLParenToken -> True
    SurfaceSignatureRParenToken -> True
    SurfaceSignatureLBraceToken -> True
    SurfaceSignatureRBraceToken -> True
    SurfaceSignatureLBracketToken -> True
    SurfaceSignatureRBracketToken -> True
    SurfaceSignatureCommaToken -> True
    _ -> False

nextStatementStartsMatchingBinding :: Text -> [Token] -> Bool
nextStatementStartsMatchingBinding name tokens =
  case tokens of
    Token {tokenKind = TIdentifier nextName} : Token {tokenKind = TEquals} : _ ->
      nextName == name
    _ -> False

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
            TLBrace -> go (depth + 1) aliases rest
            TRBrace -> go (max 0 (depth - 1)) aliases rest
            _ -> go depth aliases rest

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

rejectNestedOperatorBinding :: Token -> Either Diagnostic a
rejectNestedOperatorBinding operatorToken =
  Left
    ( parseDiagnostic
        ( "operator bindings are only allowed at file scope or directly in module bodies at "
            <> renderSourceSpan (tokenSpan operatorToken)
        )
    )

rejectNestedOperatorSignature :: Token -> Either Diagnostic a
rejectNestedOperatorSignature operatorToken =
  Left
    ( parseDiagnostic
        ( "operator signatures are only allowed at file scope or directly in module bodies at "
            <> renderSourceSpan (tokenSpan operatorToken)
        )
    )

looksLikeOperatorDeclaration :: [Token] -> Bool
looksLikeOperatorDeclaration tokensAfterKeyword =
  case tokensAfterKeyword of
    Token {tokenKind = TOperator {}} : _ -> True
    Token {tokenKind = TArrow} : _ -> True
    Token {tokenKind = TIdentifier {}} : rest -> hasOperatorFixityKeywordBeforeTerminator rest
    _ -> False

hasOperatorFixityKeywordBeforeTerminator :: [Token] -> Bool
hasOperatorFixityKeywordBeforeTerminator tokens =
  case tokens of
    [] -> False
    Token {tokenKind = TDot} : _ -> False
    Token {tokenKind = TIdentifier "tier"} : _ -> True
    Token {tokenKind = TIdentifier "precedence"} : _ -> True
    _ : rest -> hasOperatorFixityKeywordBeforeTerminator rest

looksLikeReservedAbstractionDeclaration :: Text -> [Token] -> Bool
looksLikeReservedAbstractionDeclaration name tokensAfterKeyword =
  case name of
    "class" -> looksLikeAbstractionDeclaration tokensAfterKeyword
    "impl" -> looksLikeAbstractionDeclaration tokensAfterKeyword
    "trait" -> looksLikeAbstractionDeclaration tokensAfterKeyword
    _ -> False

looksLikeAbstractionDeclaration :: [Token] -> Bool
looksLikeAbstractionDeclaration tokensAfterKeyword =
  case tokensAfterKeyword of
    Token {tokenKind = TIdentifier {}} : rest -> hasAbstractionBodyBeforeTerminator rest
    Token {tokenKind = TAt} : rest -> hasAbstractionBodyBeforeTerminator rest
    _ -> False

hasAbstractionBodyBeforeTerminator :: [Token] -> Bool
hasAbstractionBodyBeforeTerminator tokens =
  case tokens of
    [] -> False
    Token {tokenKind = TDot} : _ -> False
    Token {tokenKind = TLBrace} : _ -> True
    _ : rest -> hasAbstractionBodyBeforeTerminator rest

rejectReservedAbstractionSyntax :: Token -> Either Diagnostic a
rejectReservedAbstractionSyntax abstractionToken =
  Left (parseDiagnostic (abstractionSyntaxDiagnosticText abstractionToken))

abstractionSyntaxDiagnosticText :: Token -> Text
abstractionSyntaxDiagnosticText abstractionToken =
  let abstractionName = tokenLexeme abstractionToken
      location = renderSourceSpan (tokenSpan abstractionToken)
   in case abstractionName of
        "trait" ->
          "unsupported abstraction syntax 'trait' at "
            <> location
            <> ": trait declarations are non-canonical; use class/impl once abstraction semantics land in jazz-next"
        _ ->
          "unsupported abstraction syntax '"
            <> abstractionName
            <> "' at "
            <> location
            <> ": executable class/impl abstraction semantics are deferred in jazz-next"

rejectNestedOperatorDeclaration :: Token -> Either Diagnostic a
rejectNestedOperatorDeclaration operatorToken =
  Left
    ( parseDiagnostic
        ( "operator declarations are only allowed at file scope or directly in module bodies at "
            <> renderSourceSpan (tokenSpan operatorToken)
        )
    )

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

isReservedLiteralName :: Text -> Bool
isReservedLiteralName name = name == "True" || name == "False"

isConstructorIdentifierText :: Text -> Bool
isConstructorIdentifierText name =
  case Text.uncons name of
    Just (firstChar, _) -> isUpper firstChar
    Nothing -> False

isTypeParameterIdentifierText :: Text -> Bool
isTypeParameterIdentifierText name =
  case Text.uncons name of
    Just (firstChar, _) -> isLower firstChar
    Nothing -> False

parseDiagnostic :: Text -> Diagnostic
parseDiagnostic = mkDiagnostic "E0001"
