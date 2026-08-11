{-# LANGUAGE OverloadedStrings #-}

-- | Declaration-level token-stream parsers for the surface parser.
module Jazz.Compiler.Parser.Declaration
  ( collectImportAliasesUntilBrace,
    collectImportAliasesUntilEnd,
    parseCapabilityDeclarationTokens,
    parseCapabilityDeclarationTokensDetailed,
    parseDataStatementParser,
    parseDataStatementTokens,
    parseImportStatementParser,
    parseImportStatementTokens,
    parseStatementParser,
  )
where

import Data.Char
  ( isLower,
    isUpper,
  )
import qualified Data.List.NonEmpty as NonEmpty
import Data.Set
  ( Set,
  )
import qualified Data.Set as Set
import Data.Text
  ( Text,
  )
import qualified Data.Text as Text
import Jazz.Compiler.Diagnostics
  ( Diagnostic,
    SourceSpan,
  )
import Jazz.Compiler.ModuleExports
  ( LocatedModuleExportName (..),
    ModuleExportSelector (..),
    ModuleTypeConstructorSelector (..),
    renderModuleExportSelector,
  )
import Jazz.Compiler.Name
  ( Identifier,
    NameNamespace (..),
    identifierText,
    mkIdentifier,
    mkOperatorBindingIdentifier,
    splitQualifiedIdentifierText,
  )
import Jazz.Compiler.Parser.AST
  ( SurfaceClassMethodSignature (..),
    SurfaceDataConstructor (..),
    SurfaceExpr,
    SurfaceImplMethod (..),
    SurfaceSignaturePayload (..),
    SurfaceSignatureToken (..),
    SurfaceSignatureType (..),
    SurfaceStatement (..),
  )
import Jazz.Compiler.Parser.Context
  ( ExpressionParser,
    ParserContext (..),
    StatementBlockParser,
    StatementContext (..),
  )
import Jazz.Compiler.Parser.Failure
  ( ParserDeclarationFailure (..),
    ParserDeclarationKind (..),
    ParserDuplicateNameRole (..),
    ParserEncountered (..),
    ParserFailure,
    ParserFailureReason (..),
    ParserInternalInvariant (..),
    ParserListKind (..),
    ParserNameRole (..),
    ParserOperatorUse (..),
    ParserUnsupportedFeature (..),
    parserFailure,
    parserFailureAt,
    parserFailureDiagnostic,
  )
import Jazz.Compiler.Parser.Lexer
  ( Token (..),
    TokenKind (..),
    isImmediatelyAfter,
  )
import Jazz.Compiler.Parser.Operator
  ( Associativity (..),
    OperatorInfo (..),
    OperatorTable,
    declaredOperatorInfoForPrecedence,
    declaredOperatorInfoForTier,
    emptyOperatorTable,
    insertDeclaredOperator,
    isBuiltinOperatorSymbol,
    isDeclaredOperator,
    isReservedOperatorSymbol,
    isValidUserOperatorSymbol,
  )
import Jazz.Compiler.Parser.Signature
  ( parseConstrainedSignatureTypeDetailed,
    parseSignatureTypePrefixDetailed,
    parseSignaturePayload,
    splitTopLevelCommaTokensDetailed,
  )
import Jazz.Compiler.Parser.TokenParser
  ( Parser,
    failParserFailure,
    runTokenParserPrefixDetailed,
  )
import qualified Text.Megaparsec as MP

type ModuleBodyParser = [Token] -> Either ParserFailure ([SurfaceStatement], [Token])

type ImplExpressionParser error = [Token] -> Either error (SurfaceExpr, [Token])

data CapabilityFailure error
  = CapabilityParserFailure ParserFailure
  | CapabilityExpressionFailure error

data CapabilityDeclarationBody
  = CapabilityClassBody [SurfaceClassMethodSignature]
  | CapabilityImplBody [SurfaceImplMethod]

data OperatorDeclarationFixityKeyword
  = OperatorTierKeyword
  | OperatorPrecedenceKeyword

capabilityDeclarationKind :: Text -> ParserDeclarationKind
capabilityDeclarationKind declarationKind =
  case declarationKind of
    "impl" -> ImplDeclaration
    _ -> ClassDeclaration

parseImportStatementTokens :: [Token] -> Either Diagnostic (SurfaceStatement, [Token])
parseImportStatementTokens =
  mapLeft parserFailureDiagnostic . parseImportStatementFromTokens

parseDataStatementTokens :: [Token] -> Either Diagnostic (SurfaceStatement, [Token])
parseDataStatementTokens =
  mapLeft parserFailureDiagnostic . parseDataStatementFromTokens

parseCapabilityDeclarationTokens ::
  ImplExpressionParser Diagnostic ->
  [Token] ->
  Either Diagnostic (SurfaceStatement, [Token])
parseCapabilityDeclarationTokens parseImplExpression =
  mapLeft capabilityFailureDiagnostic
    . parseCapabilityDeclarationFromTokens parseImplExpression

parseCapabilityDeclarationTokensDetailed ::
  ImplExpressionParser ParserFailure ->
  [Token] ->
  Either ParserFailure (SurfaceStatement, [Token])
parseCapabilityDeclarationTokensDetailed parseImplExpression =
  mapLeft capabilityFailureDetailed
    . parseCapabilityDeclarationFromTokens parseImplExpression

capabilityFailureDiagnostic :: CapabilityFailure Diagnostic -> Diagnostic
capabilityFailureDiagnostic capabilityFailure =
  case capabilityFailure of
    CapabilityParserFailure failure -> parserFailureDiagnostic failure
    CapabilityExpressionFailure diagnostic -> diagnostic

capabilityFailureDetailed :: CapabilityFailure ParserFailure -> ParserFailure
capabilityFailureDetailed capabilityFailure =
  case capabilityFailure of
    CapabilityParserFailure failure -> failure
    CapabilityExpressionFailure failure -> failure

liftCapabilityParserResult :: Either ParserFailure value -> Either (CapabilityFailure error) value
liftCapabilityParserResult = mapLeft CapabilityParserFailure

liftCapabilityExpressionResult :: Either error value -> Either (CapabilityFailure error) value
liftCapabilityExpressionResult = mapLeft CapabilityExpressionFailure

parseImportStatementParser :: Parser SurfaceStatement
parseImportStatementParser =
  parseOwnedPrefix parseImportStatementFromTokens

parseDataStatementParser :: Parser SurfaceStatement
parseDataStatementParser =
  parseOwnedPrefix parseDataStatementFromTokens

parseOwnedPrefix :: ([Token] -> Either ParserFailure (a, [Token])) -> Parser a
parseOwnedPrefix parseDeclaration = do
  tokens <- MP.getInput
  case parseDeclaration tokens of
    Left failure -> failParserFailure failure
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
        runTokenParserPrefixDetailed "statement expression" (parseExpression context)
      moduleBodyContext =
        ParserContext
          { parserKnownAliases = Set.empty,
            parserDeclaredOperators = emptyOperatorTable,
            parserStatementContext = ModuleBodyContext
          }
      parseModuleBody =
        runTokenParserPrefixDetailed "module body" (parseBlock moduleBodyContext)
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
                    insertDeclaredOperator operatorInfo declaredOperators
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

liftOwnedResult :: Either ParserFailure a -> Parser a
liftOwnedResult result =
  case result of
    Left failure -> failParserFailure failure
    Right value -> pure value

parseOperatorDeclaration :: StatementContext -> OperatorTable -> Token -> [Token] -> Either ParserFailure (OperatorInfo, [Token])
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
      (declaredSymbol, afterSymbol) <- parseOperatorDeclarationSymbol tokensAfterKeyword
      validateDeclaredOperatorSymbol declaredOperators operatorToken declaredSymbol
      (fixityKeyword, afterFixityKeyword) <- consumeOperatorFixityKeyword operatorToken afterSymbol
      (operatorInfo, afterFixity) <-
        parseOperatorDeclarationFixity operatorToken declaredSymbol fixityKeyword afterFixityKeyword
      (operatorInfoWithAssociativity, afterAssociativity) <-
        parseOptionalOperatorAssociativity operatorInfo afterFixity
      remaining <-
        consumeOperatorDeclarationDot
          operatorToken
          (operatorDeclarationFixityLabel fixityKeyword)
          afterAssociativity
      pure (operatorInfoWithAssociativity, remaining)

parseOperatorDeclarationSymbol :: [Token] -> Either ParserFailure (Text, [Token])
parseOperatorDeclarationSymbol tokens =
  case tokens of
    Token {tokenKind = TOperator declaredSymbol} : rest ->
      Right (declaredSymbol, rest)
    Token {tokenKind = TArrow, tokenLexeme = arrowLexeme} : rest ->
      Right (arrowLexeme, rest)
    token : _ ->
      Left
        ( parserFailureAt
            (tokenSpan token)
            ( ExpectedSyntax
                "operator symbol after 'operator'"
                (ParserFoundToken (tokenKind token) (tokenLexeme token))
            )
        )
    [] ->
      Left
        (parserFailure (ExpectedSyntax "operator symbol after 'operator'" ParserEndOfInput))

validateDeclaredOperatorSymbol :: OperatorTable -> Token -> Text -> Either ParserFailure ()
validateDeclaredOperatorSymbol declaredOperators operatorToken declaredSymbol
  | isBuiltinOperatorSymbol declaredSymbol =
      Left
        ( parserFailureAt
            (tokenSpan operatorToken)
            (DeclarationFailure (BuiltinOperatorCannotBeRedeclared declaredSymbol))
        )
  | isReservedOperatorSymbol declaredSymbol =
      Left
        ( parserFailureAt
            (tokenSpan operatorToken)
            (DeclarationFailure (ReservedOperatorSymbol declaredSymbol))
        )
  | isDeclaredOperator declaredSymbol declaredOperators =
      Left
        ( parserFailureAt
            (tokenSpan operatorToken)
            (DeclarationFailure (DuplicateOperatorDeclaration declaredSymbol))
        )
  | isValidUserOperatorSymbol declaredSymbol = Right ()
  | otherwise =
      Left
        ( parserFailureAt
            (tokenSpan operatorToken)
            (DeclarationFailure (InvalidOperatorSymbol declaredSymbol))
        )

consumeOperatorFixityKeyword :: Token -> [Token] -> Either ParserFailure (OperatorDeclarationFixityKeyword, [Token])
consumeOperatorFixityKeyword operatorToken tokens =
  case tokens of
    Token {tokenKind = TIdentifier "tier"} : rest -> Right (OperatorTierKeyword, rest)
    Token {tokenKind = TIdentifier "precedence"} : rest -> Right (OperatorPrecedenceKeyword, rest)
    token : _ ->
      Left
        ( parserFailureAt
            (tokenSpan token)
            ( ExpectedSyntax
                "'tier' or 'precedence' in operator declaration"
                (ParserFoundToken (tokenKind token) (tokenLexeme token))
            )
        )
    [] ->
      Left
        ( parserFailureAt
            (tokenSpan operatorToken)
            (ExpectedSyntax "'tier' or 'precedence'" (ParserEndOfInputIn "operator declaration"))
        )

parseOperatorDeclarationFixity :: Token -> Text -> OperatorDeclarationFixityKeyword -> [Token] -> Either ParserFailure (OperatorInfo, [Token])
parseOperatorDeclarationFixity operatorToken declaredSymbol fixityKeyword tokens =
  case fixityKeyword of
    OperatorTierKeyword -> parseOperatorDeclarationTier operatorToken declaredSymbol tokens
    OperatorPrecedenceKeyword -> parseOperatorDeclarationPrecedence operatorToken declaredSymbol tokens

parseOperatorDeclarationTier :: Token -> Text -> [Token] -> Either ParserFailure (OperatorInfo, [Token])
parseOperatorDeclarationTier operatorToken declaredSymbol tokens =
  case tokens of
    Token {tokenKind = TInt tier} : rest ->
      case declaredOperatorInfoForTier declaredSymbol tier of
        Just operatorInfo -> Right (operatorInfo, rest)
        Nothing ->
          Left
            ( parserFailureAt
                (tokenSpan operatorToken)
                (DeclarationFailure OperatorTierOutOfRange)
            )
    token : _ ->
      Left
        ( parserFailureAt
            (tokenSpan token)
            ( ExpectedSyntax
                "operator tier 1-5"
                (ParserFoundToken (tokenKind token) (tokenLexeme token))
            )
        )
    [] ->
      Left
        ( parserFailureAt
            (tokenSpan operatorToken)
            (ExpectedSyntax "operator tier 1-5" (ParserEndOfInputIn "operator declaration"))
        )

parseOperatorDeclarationPrecedence :: Token -> Text -> [Token] -> Either ParserFailure (OperatorInfo, [Token])
parseOperatorDeclarationPrecedence operatorToken declaredSymbol tokens =
  case tokens of
    Token {tokenKind = TInt precedence} : rest ->
      case declaredOperatorInfoForPrecedence declaredSymbol precedence of
        Just operatorInfo -> Right (operatorInfo, rest)
        Nothing ->
          Left
            ( parserFailureAt
                (tokenSpan operatorToken)
                (DeclarationFailure OperatorPrecedenceOutOfRange)
            )
    token : _ ->
      Left
        ( parserFailureAt
            (tokenSpan token)
            ( ExpectedSyntax
                "operator precedence 1-99"
                (ParserFoundToken (tokenKind token) (tokenLexeme token))
            )
        )
    [] ->
      Left
        ( parserFailureAt
            (tokenSpan operatorToken)
            (ExpectedSyntax "operator precedence 1-99" (ParserEndOfInputIn "operator declaration"))
        )

parseOptionalOperatorAssociativity :: OperatorInfo -> [Token] -> Either ParserFailure (OperatorInfo, [Token])
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
        ( parserFailureAt
            (tokenSpan token)
            ( ExpectedSyntax
                "operator associativity 'left', 'right', or 'nonassoc'"
                (ParserFoundToken (tokenKind token) (tokenLexeme token))
            )
        )
    _ -> Right (operatorInfo, tokens)

operatorDeclarationFixityLabel :: OperatorDeclarationFixityKeyword -> Text
operatorDeclarationFixityLabel fixityKeyword =
  case fixityKeyword of
    OperatorTierKeyword -> "tier"
    OperatorPrecedenceKeyword -> "precedence"

consumeOperatorDeclarationDot :: Token -> Text -> [Token] -> Either ParserFailure [Token]
consumeOperatorDeclarationDot operatorToken fixityLabel tokens =
  case tokens of
    Token {tokenKind = TDot} : rest -> Right rest
    token : _ ->
      Left
        ( parserFailureAt
            (tokenSpan token)
            ( ExpectedSyntax
                ("'.' after operator declaration " <> fixityLabel)
                (ParserFoundToken (tokenKind token) (tokenLexeme token))
            )
        )
    [] ->
      Left
        ( parserFailureAt
            (tokenSpan operatorToken)
            (ExpectedSyntax ("'.' after operator declaration " <> fixityLabel) ParserEndOfInput)
        )

parseStatementFromTokens ::
  ImplExpressionParser ParserFailure ->
  ModuleBodyParser ->
  ParserContext ->
  [Token] ->
  Either ParserFailure ([SurfaceStatement], [Token])
parseStatementFromTokens parseExpression parseModuleBody context tokens =
  case tokens of
    Token {tokenKind = TLParen}
      : operatorToken@Token {tokenKind = TOperator {}}
      : Token {tokenKind = TRParen}
      : afterName@(Token {tokenKind = TColonColon} : _) ->
        singleStatement <$> parseOperatorSignature statementContext declaredOperators operatorToken afterName
    Token {tokenKind = TLParen}
      : operatorToken@Token {tokenKind = TOperator {}}
      : Token {tokenKind = TRParen}
      : Token {tokenKind = TEquals}
      : afterEquals ->
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
            <$> parseCapabilityDeclarationTokensDetailed
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
      case statementContext of
        NestedBlockContext -> rejectNestedImportDeclaration importToken
        TopLevelContext -> singleStatement <$> parseImportStatementFromTokens (importToken : rest)
        ModuleBodyContext -> singleStatement <$> parseImportStatementFromTokens (importToken : rest)
    dataToken@Token {tokenKind = TData} : rest ->
      case statementContext of
        TopLevelContext -> singleStatement <$> parseDataStatementFromTokens (dataToken : rest)
        ModuleBodyContext -> singleStatement <$> parseDataStatementFromTokens (dataToken : rest)
        NestedBlockContext -> rejectNestedDataDeclaration dataToken
    nameToken : afterName@(Token {tokenKind = TColonColon} : _)
      | TIdentifier name <- tokenKind nameToken,
        isReservedLiteralName name ->
          Left
            ( parserFailureAt
                (tokenSpan nameToken)
                (DeclarationFailure (ReservedLiteralName BindingName name))
            )
      | TIdentifier name <- tokenKind nameToken ->
          singleStatement
            <$> parseSignatureOrQualifiedAlias
              parseExpression
              knownAliases
              name
              nameToken
              afterName
              tokens
    nameToken : afterName@(Token {tokenKind = TEquals} : _)
      | TIdentifier name <- tokenKind nameToken,
        isReservedLiteralName name ->
          Left
            ( parserFailureAt
                (tokenSpan nameToken)
                (DeclarationFailure (ReservedLiteralName BindingName name))
            )
      | TIdentifier name <- tokenKind nameToken ->
          singleStatement <$> parseLet parseExpression (mkIdentifier name) nameToken afterName
    _ -> singleStatement <$> parseExprStatement parseExpression tokens
  where
    knownAliases = parserKnownAliases context
    declaredOperators = parserDeclaredOperators context
    statementContext = parserStatementContext context
    singleStatement (statement, remaining) = ([statement], remaining)

parseSignatureOrQualifiedAlias ::
  ImplExpressionParser ParserFailure ->
  Set Text ->
  Text ->
  Token ->
  [Token] ->
  [Token] ->
  Either ParserFailure (SurfaceStatement, [Token])
parseSignatureOrQualifiedAlias parseExpression knownAliases name nameToken tokensAfterName allTokens =
  let parsedSignature = parseSignature (mkIdentifier name) nameToken tokensAfterName
   in if shouldParseQualifiedAliasStatement knownAliases name nameToken tokensAfterName parsedSignature
        then parseExprStatement parseExpression allTokens
        else parsedSignature

parseOperatorBinding ::
  ImplExpressionParser ParserFailure ->
  StatementContext ->
  OperatorTable ->
  Token ->
  [Token] ->
  Either ParserFailure (SurfaceStatement, [Token])
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
                ( parserFailureAt
                    (tokenSpan operatorToken)
                    (DeclarationFailure (BuiltinOperatorCannotBeBound bindingSymbol))
                )
          | not (operatorDeclared bindingSymbol) ->
              Left
                ( parserFailureAt
                    (tokenSpan operatorToken)
                    (UndeclaredOperator bindingSymbol OperatorUseInBinding)
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
            ( parserFailureAt
                (tokenSpan operatorToken)
                (InternalParserFailure (ExpectedOperatorToken OperatorUseInBinding))
            )

    operatorDeclared bindingSymbol = isDeclaredOperator bindingSymbol declaredOperators

parseOperatorSignature ::
  StatementContext ->
  OperatorTable ->
  Token ->
  [Token] ->
  Either ParserFailure (SurfaceStatement, [Token])
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
                ( parserFailureAt
                    (tokenSpan operatorToken)
                    (DeclarationFailure (BuiltinOperatorCannotBeSigned signatureSymbol))
                )
          | not (operatorDeclared signatureSymbol) ->
              Left
                ( parserFailureAt
                    (tokenSpan operatorToken)
                    (UndeclaredOperator signatureSymbol OperatorUseInSignature)
                )
          | otherwise ->
              parseSignature
                (mkOperatorBindingIdentifier signatureSymbol)
                operatorToken
                tokensAfterName
        _ ->
          Left
            ( parserFailureAt
                (tokenSpan operatorToken)
                (InternalParserFailure (ExpectedOperatorToken OperatorUseInSignature))
            )

    operatorDeclared signatureSymbol = isDeclaredOperator signatureSymbol declaredOperators

parseSignature :: Identifier -> Token -> [Token] -> Either ParserFailure (SurfaceStatement, [Token])
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
        ( parserFailureAt
            (tokenSpan nameToken)
            (InternalParserFailure ExpectedSignatureSeparator)
        )

parseLet ::
  ImplExpressionParser ParserFailure ->
  Identifier ->
  Token ->
  [Token] ->
  Either ParserFailure (SurfaceStatement, [Token])
parseLet parseExpression name nameToken tokensAfterName =
  case tokensAfterName of
    Token {tokenKind = TEquals} : rest -> do
      (valueExpr, afterExpr) <- parseExpression rest
      remaining <- consumeDot afterExpr
      pure (SSLet name (tokenSpan nameToken) valueExpr, remaining)
    _ ->
      Left
        ( parserFailureAt
            (tokenSpan nameToken)
            (InternalParserFailure ExpectedBindingEquals)
        )

parseExprStatement :: ImplExpressionParser ParserFailure -> [Token] -> Either ParserFailure (SurfaceStatement, [Token])
parseExprStatement parseExpression tokens =
  case tokens of
    [] -> Left (parserFailure (ExpectedSyntax "expression" ParserEndOfInput))
    firstToken : _ -> do
      (expr, afterExpr) <- parseExpression tokens
      remaining <- consumeDot afterExpr
      pure (SSExpr (tokenSpan firstToken) expr, remaining)

parseModuleStatementFromTokens ::
  ModuleBodyParser ->
  [Token] ->
  Either ParserFailure ([SurfaceStatement], [Token])
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
            ( parserFailureAt
                (tokenSpan moduleToken)
                (ExpectedSyntax "'{'" (ParserEndOfInputAfter "module path"))
            )
        token : _ ->
          Left
            ( parserFailureAt
                (tokenSpan token)
                (ExpectedSyntax "'{'" (ParserFoundToken (tokenKind token) (tokenLexeme token)))
            )
    [] ->
      Left (parserFailure (ExpectedSyntax "'module'" ParserEndOfInput))
    token : _ ->
      Left
        ( parserFailureAt
            (tokenSpan token)
            (ExpectedSyntax "'module'" (ParserFoundToken (tokenKind token) (tokenLexeme token)))
        )

parseImportStatementFromTokens :: [Token] -> Either ParserFailure (SurfaceStatement, [Token])
parseImportStatementFromTokens tokens =
  case tokens of
    importToken@Token {tokenKind = TImport} : tokensAfterImportKeyword -> do
      (modulePath, afterModulePath) <- parseModulePath tokensAfterImportKeyword
      parseImportTail importToken modulePath afterModulePath
    [] ->
      Left (parserFailure (ExpectedSyntax "'import'" ParserEndOfInput))
    token : _ ->
      Left
        ( parserFailureAt
            (tokenSpan token)
            (ExpectedSyntax "'import'" (ParserFoundToken (tokenKind token) (tokenLexeme token)))
        )

parseImportTail :: Token -> [Text] -> [Token] -> Either ParserFailure (SurfaceStatement, [Token])
parseImportTail importToken modulePath tokensAfterModulePath =
  case tokensAfterModulePath of
    Token {tokenKind = TDot} : rest ->
      pure (SSImport (tokenSpan importToken) modulePath Nothing Nothing, rest)
    asToken@Token {tokenKind = TAs} : rest ->
      case rest of
        aliasToken@Token {tokenKind = TIdentifier aliasName} : afterAlias
          | isReservedLiteralName aliasName ->
              Left
                ( parserFailureAt
                    (tokenSpan aliasToken)
                    (DeclarationFailure (ReservedLiteralName ImportAlias aliasName))
                )
          | otherwise ->
              case afterAlias of
                parenToken@Token {tokenKind = TLParen} : _ ->
                  Left
                    ( parserFailureAt
                        (tokenSpan parenToken)
                        (DeclarationFailure ImportAliasCombinedWithSymbolList)
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
            ( parserFailureAt
                (tokenSpan asToken)
                (ExpectedSyntax "import alias" (ParserEndOfInputAfter "'as'"))
            )
        token : _ ->
          Left
            ( parserFailureAt
                (tokenSpan token)
                (ExpectedSyntax "import alias" (ParserFoundToken (tokenKind token) (tokenLexeme token)))
            )
    Token {tokenKind = TLParen} : rest -> do
      (symbols, afterSymbols) <- parseImportSymbolList rest
      case afterSymbols of
        asToken@Token {tokenKind = TAs} : _ ->
          Left
            ( parserFailureAt
                (tokenSpan asToken)
                (DeclarationFailure ImportAliasCombinedWithSymbolList)
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
        ( parserFailureAt
            (tokenSpan importToken)
            (ExpectedSyntax "'.', 'as', or '('" (ParserEndOfInputAfter "import path"))
        )
    token : _ ->
      Left
        ( parserFailureAt
            (tokenSpan token)
            ( ExpectedSyntax
                "'.', 'as', or '('"
                (ParserFoundToken (tokenKind token) (tokenLexeme token))
            )
        )

parseDataStatementFromTokens :: [Token] -> Either ParserFailure (SurfaceStatement, [Token])
parseDataStatementFromTokens tokens =
  case tokens of
    dataToken@Token {tokenKind = TData} : tokensAfterDataKeyword -> do
      (typeName, afterTypeName) <- parseDataTypeName tokensAfterDataKeyword
      (typeParameters, afterTypeParameters) <- parseDataTypeParameters afterTypeName
      afterEquals <-
        consumeEquals
          (tokenSpan dataToken)
          afterTypeParameters
          (ExpectedSyntax "'='" (ParserEndOfInputAfter "data type name"))
      (constructors, remaining) <- parseDataConstructors typeName typeParameters afterEquals
      pure (SSData (tokenSpan dataToken) typeName typeParameters constructors, remaining)
    [] ->
      Left (parserFailure (ExpectedSyntax "'data'" ParserEndOfInput))
    token : _ ->
      Left
        ( parserFailureAt
            (tokenSpan token)
            (ExpectedSyntax "'data'" (ParserFoundToken (tokenKind token) (tokenLexeme token)))
        )

parseCapabilityDeclarationFromTokens ::
  ImplExpressionParser error ->
  [Token] ->
  Either (CapabilityFailure error) (SurfaceStatement, [Token])
parseCapabilityDeclarationFromTokens parseImplExpression tokens =
  case tokens of
    declarationToken@Token {tokenKind = TIdentifier declarationKind} : tokensAfterKeyword ->
      case declarationKind of
        "class" ->
          parseCapabilityDeclaration parseImplExpression declarationKind declarationToken tokensAfterKeyword
        "impl" ->
          parseCapabilityDeclaration parseImplExpression declarationKind declarationToken tokensAfterKeyword
        _ ->
          liftCapabilityParserResult (rejectReservedAbstractionSyntax declarationToken)
    [] ->
      Left
        ( CapabilityParserFailure
            (parserFailure (ExpectedSyntax "capability declaration" ParserEndOfInput))
        )
    token : _ ->
      Left
        ( CapabilityParserFailure
            ( parserFailureAt
                (tokenSpan token)
                ( ExpectedSyntax
                    "capability declaration"
                    (ParserFoundToken (tokenKind token) (tokenLexeme token))
                )
            )
        )

parseCapabilityDeclaration ::
  ImplExpressionParser error ->
  Text ->
  Token ->
  [Token] ->
  Either (CapabilityFailure error) (SurfaceStatement, [Token])
parseCapabilityDeclaration parseImplExpression declarationKind declarationToken tokensAfterKeyword = do
  (capabilityName, maybeHeaderArguments, headerRemaining) <-
    liftCapabilityParserResult
      (parseCapabilityHeaderName declarationKind declarationToken tokensAfterKeyword)
  let headerArguments =
        case maybeHeaderArguments of
          Just arguments -> arguments
          Nothing -> []
  case declarationKind of
    "class" -> do
      classParameters <-
        liftCapabilityParserResult
          (validateClassHeaderParameters declarationToken maybeHeaderArguments)
      (capabilityBody, afterBody) <- parseCapabilityDeclarationBody parseImplExpression declarationKind declarationToken headerRemaining
      remaining <- liftCapabilityParserResult (consumeDot afterBody)
      case capabilityBody of
        CapabilityClassBody methodSignatures ->
          Right (SSClass (tokenSpan declarationToken) capabilityName classParameters methodSignatures, remaining)
        CapabilityImplBody {} ->
          liftCapabilityParserResult (rejectReservedAbstractionSyntax declarationToken)
    "impl" -> do
      (capabilityBody, afterBody) <- parseCapabilityDeclarationBody parseImplExpression declarationKind declarationToken headerRemaining
      remaining <- liftCapabilityParserResult (consumeDot afterBody)
      case capabilityBody of
        CapabilityImplBody methods ->
          if surfaceConcreteImplArguments headerArguments
            then Right (SSImpl (tokenSpan declarationToken) capabilityName headerArguments methods, remaining)
            else
              Left
                ( CapabilityParserFailure
                    ( parserFailureAt
                        (tokenSpan declarationToken)
                        (DeclarationFailure ImplRequiresConcreteTarget)
                    )
                )
        CapabilityClassBody {} ->
          liftCapabilityParserResult (rejectReservedAbstractionSyntax declarationToken)
    _ ->
      liftCapabilityParserResult (rejectReservedAbstractionSyntax declarationToken)

parseCapabilityHeaderName :: Text -> Token -> [Token] -> Either ParserFailure (Identifier, Maybe [SurfaceSignatureType], [Token])
parseCapabilityHeaderName declarationKind declarationToken tokensAfterKeyword =
  case tokensAfterKeyword of
    Token {tokenKind = TIdentifier candidateName, tokenSpan = nameSpan} : rest
      | isConstructorIdentifierText candidateName ->
          parseCapabilityHeaderTail (mkIdentifier candidateName) rest
      | otherwise ->
          Left
            ( parserFailureAt
                nameSpan
                ( ExpectedSyntax
                    "uppercase capability name"
                    (ParserFoundToken (TIdentifier candidateName) candidateName)
                )
            )
    Token {tokenKind = TLBrace} : _ ->
      Left
        ( parserFailureAt
            (tokenSpan declarationToken)
            ( ExpectedSyntax
                "capability name"
                ( ParserBeforeToken
                    TLBrace
                    "{"
                    (Just (declarationKind <> " declaration"))
                )
            )
        )
    Token {tokenKind = TDot} : _ ->
      Left
        ( parserFailureAt
            (tokenSpan declarationToken)
            ( ExpectedSyntax
                "capability name"
                (ParserBeforeToken TDot "." (Just (declarationKind <> " declaration")))
            )
        )
    token : _ ->
      Left
        ( parserFailureAt
            (tokenSpan token)
            (ExpectedSyntax "capability name" (ParserFoundToken (tokenKind token) (tokenLexeme token)))
        )
    [] ->
      Left
        ( parserFailureAt
            (tokenSpan declarationToken)
            (ExpectedSyntax "capability name" (ParserEndOfInputIn (declarationKind <> " declaration")))
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
            ( parserFailureAt
                (tokenSpan declarationToken)
                ( ExpectedSyntax
                    "'{'"
                    (ParserBeforeToken TDot "." (Just (declarationKind <> " declaration")))
                )
            )
        token : _ ->
          Left
            ( parserFailureAt
                (tokenSpan token)
                ( UnexpectedSyntaxIn
                    (ParserFoundToken (tokenKind token) (tokenLexeme token))
                    (declarationKind <> " declaration header")
                )
            )
        [] ->
          Left
            ( parserFailureAt
                (tokenSpan declarationToken)
                (ExpectedSyntax "'{'" (ParserEndOfInputIn (declarationKind <> " declaration")))
            )

    parseParenthesizedCapabilityHeader tokens = do
      (argumentTokens, remaining) <- collectParenthesizedCapabilityHeader tokens
      headerArguments <-
        if null argumentTokens
          then Right []
          else case splitTopLevelCommaTokensDetailed argumentTokens >>= traverse parseConstrainedSignatureTypeDetailed of
            Right parsedArguments -> Right parsedArguments
            Left _ ->
              Left
                ( parserFailureAt
                    (tokenSpan declarationToken)
                    (UnsupportedSyntax (DeclarationHeaderArguments (capabilityDeclarationKind declarationKind)))
                )
      Right (headerArguments, remaining)

    collectParenthesizedCapabilityHeader tokens =
      go (1 :: Int) [] tokens
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
                ( parserFailureAt
                    braceSpan
                    ( ExpectedSyntax
                        "')'"
                        ( ParserBeforeToken
                            TLBrace
                            "{"
                            (Just (declarationKind <> " declaration header"))
                        )
                    )
                )
            token : rest ->
              go depth (token : acc) rest
            [] ->
              Left
                ( parserFailureAt
                    (tokenSpan declarationToken)
                    (ExpectedSyntax "')'" (ParserEndOfInputIn (declarationKind <> " declaration header")))
                )

parseCapabilityDeclarationBody ::
  ImplExpressionParser error ->
  Text ->
  Token ->
  [Token] ->
  Either (CapabilityFailure error) (CapabilityDeclarationBody, [Token])
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
          liftCapabilityParserResult (rejectReservedAbstractionSyntax declarationToken)
    [] ->
      Left
        ( CapabilityParserFailure
            ( parserFailureAt
                (tokenSpan declarationToken)
                (ExpectedSyntax "'{'" (ParserEndOfInputIn (declarationKind <> " declaration")))
            )
        )
    token : _ ->
      Left
        ( CapabilityParserFailure
            ( parserFailureAt
                (tokenSpan token)
                (ExpectedSyntax "'{'" (ParserFoundToken (tokenKind token) (tokenLexeme token)))
            )
        )
  where
    consumeClassBody seenMethodNames reversedMethods remainingTokens =
      case remainingTokens of
        [] ->
          Left
            ( CapabilityParserFailure
                ( parserFailureAt
                    (tokenSpan declarationToken)
                    (ExpectedSyntax "'}'" (ParserEndOfInputIn (declarationKind <> " declaration")))
                )
            )
        Token {tokenKind = TRBrace} : rest ->
          Right (reverse reversedMethods, rest)
        operatorToken@Token {tokenKind = TIdentifier "operator"} : _ ->
          liftCapabilityParserResult (rejectNestedOperatorDeclaration operatorToken)
        methodToken@Token {tokenKind = TIdentifier methodName, tokenSpan = methodSpan} : Token {tokenKind = TColonColon} : rest
          | Set.member methodName seenMethodNames ->
              Left
                ( CapabilityParserFailure
                    ( parserFailureAt
                        methodSpan
                        (DeclarationFailure (DuplicateName ClassMethodName methodName ClassDeclaration))
                    )
                )
          | otherwise -> do
              (signatureTokens, afterSignature) <-
                liftCapabilityParserResult (collectUntilDot rest)
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
            ( CapabilityParserFailure
                ( parserFailureAt
                    methodSpan
                    (UnsupportedSyntax (ClassMethodBody methodName))
                )
            )
        token : _ ->
          Left
            ( CapabilityParserFailure
                ( parserFailureAt
                    (tokenSpan token)
                    ( ExpectedSyntax
                        ("signature-only method declaration or '}' in " <> declarationKind <> " declaration body")
                        (ParserFoundToken (tokenKind token) (tokenLexeme token))
                    )
                )
            )

    consumeImplBody seenMethodNames reversedMethods remainingTokens =
      case remainingTokens of
        [] ->
          Left
            ( CapabilityParserFailure
                ( parserFailureAt
                    (tokenSpan declarationToken)
                    (ExpectedSyntax "'}'" (ParserEndOfInputIn (declarationKind <> " declaration")))
                )
            )
        Token {tokenKind = TRBrace} : rest ->
          Right (reverse reversedMethods, rest)
        operatorToken@Token {tokenKind = TIdentifier "operator"} : _ ->
          liftCapabilityParserResult (rejectNestedOperatorDeclaration operatorToken)
        methodToken@Token {tokenKind = TIdentifier methodName, tokenSpan = methodSpan}
          : Token {tokenKind = TEquals}
          : afterEquals
            | Set.member methodName seenMethodNames ->
                Left
                  ( CapabilityParserFailure
                      ( parserFailureAt
                          methodSpan
                          (DeclarationFailure (DuplicateName ImplMethodName methodName ImplDeclaration))
                      )
                  )
            | otherwise -> do
                (methodExpr, afterExpr) <-
                  liftCapabilityExpressionResult (parseImplExpression afterEquals)
                afterMethod <- liftCapabilityParserResult (consumeDot afterExpr)
                let method =
                      SurfaceImplMethod
                        (mkIdentifier methodName)
                        (tokenSpan methodToken)
                        methodExpr
                consumeImplBody
                  (Set.insert methodName seenMethodNames)
                  (method : reversedMethods)
                  afterMethod
        Token {tokenKind = TIdentifier methodName, tokenSpan = methodSpan} : Token {tokenKind = TColonColon} : _ ->
          Left
            ( CapabilityParserFailure
                ( parserFailureAt
                    methodSpan
                    (DeclarationFailure (ExpectedOrdinaryImplMethodBinding methodName))
                )
            )
        token : _ ->
          Left
            ( CapabilityParserFailure
                ( parserFailureAt
                    (tokenSpan token)
                    ( ExpectedSyntax
                        "ordinary method binding or '}' in impl declaration body"
                        (ParserFoundToken (tokenKind token) (tokenLexeme token))
                    )
                )
            )

surfaceConcreteImplArguments :: [SurfaceSignatureType] -> Bool
surfaceConcreteImplArguments arguments =
  case arguments of
    [argument] -> surfaceConcreteConstraintArgument argument
    _ -> False

surfaceConcreteConstraintArgument :: SurfaceSignatureType -> Bool
surfaceConcreteConstraintArgument signatureType =
  case signatureType of
    SurfaceTypeVariable {} -> False
    SurfaceTypeName name ->
      not (surfaceIdentifierLooksLikeTypeVariable name)
    SurfaceTypeApplication name arguments ->
      not (surfaceIdentifierLooksLikeTypeVariable name) && all surfaceConcreteConstraintArgument arguments
    SurfaceTypeList innerType ->
      surfaceConcreteConstraintArgument innerType
    SurfaceTypeTuple elementTypes ->
      all surfaceConcreteConstraintArgument elementTypes
    SurfaceTypeFunction {} ->
      False
    _ -> True

surfaceIdentifierLooksLikeTypeVariable :: Identifier -> Bool
surfaceIdentifierLooksLikeTypeVariable name =
  case Text.uncons memberName of
    Just (c, _) -> isLower c
    Nothing -> False
  where
    fullName = identifierText name
    memberName = maybe fullName snd (splitQualifiedIdentifierText fullName)

validateClassHeaderParameters :: Token -> Maybe [SurfaceSignatureType] -> Either ParserFailure [Identifier]
validateClassHeaderParameters declarationToken maybeHeaderArguments =
  case maybeHeaderArguments of
    Nothing ->
      Left
        ( parserFailureAt
            (tokenSpan declarationToken)
            (DeclarationFailure ClassRequiresExplicitParameterList)
        )
    Just [] ->
      Left
        ( parserFailureAt
            (tokenSpan declarationToken)
            (DeclarationFailure ClassRequiresLowercaseParameter)
        )
    Just headerArguments -> do
      classParameters <- traverse classParameterFromHeaderArgument headerArguments
      case duplicateClassParameterName classParameters of
        Just duplicateName ->
          Left
            ( parserFailureAt
                (tokenSpan declarationToken)
                (DeclarationFailure (DuplicateClassParameter duplicateName))
            )
        Nothing ->
          case classParameters of
            [_] -> Right classParameters
            _ ->
              Left
                ( parserFailureAt
                    (tokenSpan declarationToken)
                    (DeclarationFailure ClassSupportsExactlyOneParameter)
                )
  where
    classParameterFromHeaderArgument argument =
      case argument of
        SurfaceTypeVariable parameterName ->
          Right parameterName
        _ ->
          Left
            ( parserFailureAt
                (tokenSpan declarationToken)
                (DeclarationFailure ClassParameterMustBeLowercase)
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

parseDataTypeName :: [Token] -> Either ParserFailure (Identifier, [Token])
parseDataTypeName tokens =
  case tokens of
    Token {tokenKind = TIdentifier typeName, tokenSpan = typeSpan} : rest
      | isConstructorIdentifierText typeName ->
          Right (mkIdentifier typeName, rest)
      | otherwise ->
          Left
            ( parserFailureAt
                typeSpan
                (ExpectedSyntax "type constructor name" (ParserFoundToken (TIdentifier typeName) typeName))
            )
    [] ->
      Left (parserFailure (ExpectedSyntax "type constructor name" (ParserEndOfInputAfter "'data'")))
    token : _ ->
      Left
        ( parserFailureAt
            (tokenSpan token)
            (ExpectedSyntax "type constructor name" (ParserFoundToken (tokenKind token) (tokenLexeme token)))
        )

parseDataTypeParameters :: [Token] -> Either ParserFailure ([Identifier], [Token])
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
                    ( parserFailureAt
                        parameterSpan
                        (DeclarationFailure (DuplicateName DataTypeParameter parameterName DataDeclaration))
                    )
                else
                  go
                    (Set.insert parameterName seenParameters)
                    (mkIdentifier parameterName : revParameters)
                    rest
          | otherwise ->
              Left
                ( parserFailureAt
                    parameterSpan
                    ( ExpectedSyntax
                        "lowercase type parameter or '='"
                        (ParserFoundToken (TIdentifier parameterName) parameterName)
                    )
                )
        _ ->
          Right (reverse revParameters, allTokens)

parseDataConstructors :: Identifier -> [Identifier] -> [Token] -> Either ParserFailure ([SurfaceDataConstructor], [Token])
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
                ( parserFailure
                    (DeclarationFailure (DuplicateName DataConstructorName constructorName DataDeclaration))
                )
            else
              go
                (Set.insert constructorName seenConstructors)
                (nextConstructor : revConstructors)
                afterNextConstructor
        [] ->
          Left (parserFailure (ExpectedSyntax "'.'" (ParserEndOfInputIn "data declaration")))
        token : _ ->
          Left
            ( parserFailureAt
                (tokenSpan token)
                (ExpectedSyntax "'|' or '.'" (ParserFoundToken (tokenKind token) (tokenLexeme token)))
            )

    surfaceDataConstructorName :: SurfaceDataConstructor -> Text
    surfaceDataConstructorName (SurfaceDataConstructor constructorName _) =
      identifierText constructorName

parseDataConstructor :: Identifier -> Set Text -> [Token] -> Either ParserFailure (SurfaceDataConstructor, [Token])
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
            ( parserFailureAt
                constructorSpan
                ( ExpectedSyntax
                    "constructor declaration"
                    (ParserFoundToken (TIdentifier constructorName) constructorName)
                )
            )
    [] ->
      Left (parserFailure (ExpectedSyntax "constructor declaration" (ParserEndOfInputIn "data declaration")))
    token : _ ->
      Left
        ( parserFailureAt
            (tokenSpan token)
            ( ExpectedSyntax
                "constructor declaration"
                (ParserFoundToken (tokenKind token) (tokenLexeme token))
            )
        )

parseDataConstructorArguments ::
  Identifier ->
  Set Text ->
  [SurfaceSignatureType] ->
  [Token] ->
  Either ParserFailure ([SurfaceSignatureType], [Token])
parseDataConstructorArguments typeName typeParameterNames revArguments allTokens =
  case allTokens of
    Token {tokenKind = TOperator "|"} : _ ->
      Right (reverse revArguments, allTokens)
    Token {tokenKind = TDot} : _ ->
      Right (reverse revArguments, allTokens)
    [] ->
      Right (reverse revArguments, allTokens)
    firstToken : _ -> do
      let fieldSpan = tokenSpan firstToken
      (fieldType, remaining) <- parseSignatureTypePrefixDetailed allTokens
      case Set.toList (surfaceSignatureTypeVariables fieldType `Set.difference` typeParameterNames) of
        undeclaredName : _ ->
          Left
            ( parserFailureAt
                fieldSpan
                ( DeclarationFailure
                    (UndeclaredConstructorTypeParameter undeclaredName (identifierText typeName))
                )
            )
        [] ->
          parseDataConstructorArguments typeName typeParameterNames (fieldType : revArguments) remaining

surfaceSignatureTypeVariables :: SurfaceSignatureType -> Set Text
surfaceSignatureTypeVariables signatureType =
  case signatureType of
    SurfaceTypeInt -> Set.empty
    SurfaceTypeFloat -> Set.empty
    SurfaceTypeNumeric _ -> Set.empty
    SurfaceTypeBool -> Set.empty
    SurfaceTypeChar -> Set.empty
    SurfaceTypeText -> Set.empty
    SurfaceTypeVariable name -> Set.singleton (identifierText name)
    SurfaceTypeName _ -> Set.empty
    SurfaceTypeApplication _ arguments ->
      Set.unions (map surfaceSignatureTypeVariables arguments)
    SurfaceTypeList elementType ->
      surfaceSignatureTypeVariables elementType
    SurfaceTypeTuple elementTypes ->
      Set.unions (map surfaceSignatureTypeVariables elementTypes)
    SurfaceTypeFunction argumentType resultType ->
      surfaceSignatureTypeVariables argumentType
        `Set.union` surfaceSignatureTypeVariables resultType

parseModulePath :: [Token] -> Either ParserFailure ([Text], [Token])
parseModulePath tokens =
  case tokens of
    [] -> Left (parserFailure (ExpectedSyntax "module path" ParserEndOfInput))
    Token {tokenKind = TIdentifier firstSegment} : rest ->
      go [firstSegment] rest
      where
        go revSegments allTokens =
          case allTokens of
            Token {tokenKind = TColonColon} : Token {tokenKind = TIdentifier nextSegment} : remaining ->
              go (nextSegment : revSegments) remaining
            separatorToken@Token {tokenKind = TColonColon} : [] ->
              Left
                ( parserFailureAt
                    (tokenSpan separatorToken)
                    (ExpectedSyntax "module path segment" ParserEndOfInput)
                )
            separatorToken@Token {tokenKind = TColonColon} : token : _
              | tokenKind token == TDot ->
                  Left
                    ( parserFailureAt
                        (tokenSpan separatorToken)
                        ( ExpectedSyntax
                            "module path segment"
                            (ParserFoundToken (tokenKind token) (tokenLexeme token))
                        )
                    )
              | otherwise ->
                  Left
                    ( parserFailureAt
                        (tokenSpan token)
                        ( ExpectedSyntax
                            "module path segment"
                            (ParserFoundToken (tokenKind token) (tokenLexeme token))
                        )
                    )
            _ -> Right (reverse revSegments, allTokens)
    token : _ ->
      Left
        ( parserFailureAt
            (tokenSpan token)
            ( ExpectedSyntax
                "module path segment"
                (ParserFoundToken (tokenKind token) (tokenLexeme token))
            )
        )

parseImportSymbolList :: [Token] -> Either ParserFailure ([Text], [Token])
parseImportSymbolList tokensAfterLeftParen =
  case tokensAfterLeftParen of
    token@Token {tokenKind = TRParen} : _ ->
      Left
        ( parserFailureAt
            (tokenSpan token)
            (ExpectedSyntax "at least one import symbol" (ParserBeforeToken TRParen ")" Nothing))
        )
    _ ->
      parseNonEmptyUniqueList
        ImportSymbolList
        "import symbol list"
        (\name -> "'" <> name <> "'")
        parseImportSymbol
        tokensAfterLeftParen

parseModuleExportList :: [Token] -> Either ParserFailure ([ModuleExportSelector], [Token])
parseModuleExportList tokensAfterLeftParen =
  case tokensAfterLeftParen of
    Token {tokenKind = TRParen} : rest -> Right ([], rest)
    _ ->
      parseNonEmptyUniqueList
        ModuleExportList
        "module export list"
        renderModuleExportSelector
        parseModuleExport
        tokensAfterLeftParen

parseNonEmptyUniqueList ::
  ParserListKind ->
  Text ->
  (item -> Text) ->
  ([Token] -> Either ParserFailure (item, SourceSpan, [Token])) ->
  [Token] ->
  Either ParserFailure ([item], [Token])
parseNonEmptyUniqueList listKind listDescription renderItem parseItem tokens = do
  (firstItem, _, afterFirstItem) <- parseItem tokens
  go [firstItem] (Set.singleton (renderItem firstItem)) afterFirstItem
  where
    go reversedItems seenItems allTokens =
      case allTokens of
        Token {tokenKind = TComma} : rest -> do
          (nextItem, itemSpan, afterNextItem) <- parseItem rest
          let nextItemKey = renderItem nextItem
          if Set.member nextItemKey seenItems
            then
              Left
                ( parserFailureAt
                    itemSpan
                    (DeclarationFailure (DuplicateListItem listKind (renderItem nextItem)))
                )
            else
              go
                (nextItem : reversedItems)
                (Set.insert nextItemKey seenItems)
                afterNextItem
        Token {tokenKind = TRParen} : rest -> Right (reverse reversedItems, rest)
        [] ->
          Left
            (parserFailure (ExpectedSyntax "')'" (ParserEndOfInputIn listDescription)))
        token : _ ->
          Left
            ( parserFailureAt
                (tokenSpan token)
                (ExpectedSyntax "',' or ')'" (ParserFoundToken (tokenKind token) (tokenLexeme token)))
            )

parseModuleExport :: [Token] -> Either ParserFailure (ModuleExportSelector, SourceSpan, [Token])
parseModuleExport tokens =
  case tokens of
    Token {tokenKind = TValue}
      : Token {tokenKind = TIdentifier exportName, tokenSpan = exportSpan}
      : rest ->
        Right
          ( ModuleExportSelector (Just ValueNamespace) exportName,
            exportSpan,
            rest
          )
    Token {tokenKind = TIdentifier prefix} : Token {tokenKind = TIdentifier exportName, tokenSpan = exportSpan} : rest
      | Just TypeNamespace <- moduleExportNamespacePrefix prefix ->
          parseTypeModuleExport exportName exportSpan rest
      | Just namespace <- moduleExportNamespacePrefix prefix ->
          Right (ModuleExportSelector (Just namespace) exportName, exportSpan, rest)
    Token {tokenKind = TIdentifier exportName, tokenSpan = exportSpan} : rest ->
      Right (ModuleExportSelector Nothing exportName, exportSpan, rest)
    [] -> Left (parserFailure (ExpectedSyntax "module export name" ParserEndOfInput))
    token : _ ->
      Left
        ( parserFailureAt
            (tokenSpan token)
            (ExpectedSyntax "module export name" (ParserFoundToken (tokenKind token) (tokenLexeme token)))
        )

parseTypeModuleExport :: Text -> SourceSpan -> [Token] -> Either ParserFailure (ModuleExportSelector, SourceSpan, [Token])
parseTypeModuleExport typeName typeSpan tokens =
  case tokens of
    Token {tokenKind = TLParen} : afterLeftParen ->
      case afterLeftParen of
        token@Token {tokenKind = TRParen} : _ ->
          Left
            ( parserFailureAt
                (tokenSpan token)
                (ExpectedSyntax "'..' or at least one constructor export" (ParserAtToken TRParen ")"))
            )
        dotToken@Token {tokenKind = TDot} : afterFirstDot ->
          parseAllTypeConstructors typeName typeSpan (tokenSpan dotToken) afterFirstDot
        _ -> do
          (constructors, remaining) <-
            parseNonEmptyUniqueList
              ConstructorExportList
              "constructor export group"
              (\locatedName -> "'" <> locatedModuleExportName locatedName <> "'")
              parseLocatedModuleExportName
              afterLeftParen
          case NonEmpty.nonEmpty constructors of
            Nothing ->
              Left
                (parserFailureAt typeSpan (ExpectedSyntax "at least one constructor export" ParserImplicitBoundary))
            Just nonEmptyConstructors ->
              Right
                ( ModuleTypeExportSelector typeName typeSpan (SelectedTypeConstructors nonEmptyConstructors),
                  typeSpan,
                  remaining
                )
    _ -> Right (ModuleTypeExportSelector typeName typeSpan AbstractType, typeSpan, tokens)

parseAllTypeConstructors :: Text -> SourceSpan -> SourceSpan -> [Token] -> Either ParserFailure (ModuleExportSelector, SourceSpan, [Token])
parseAllTypeConstructors typeName typeSpan allSpan tokens =
  case tokens of
    Token {tokenKind = TDot} : Token {tokenKind = TRParen} : rest ->
      Right
        ( ModuleTypeExportSelector typeName typeSpan (AllTypeConstructors allSpan),
          typeSpan,
          rest
        )
    token : _ ->
      Left
        ( parserFailureAt
            (tokenSpan token)
            (DeclarationFailure ConstructorExportGroupRequiresAll)
        )
    [] ->
      Left (parserFailureAt allSpan (DeclarationFailure ConstructorExportGroupRequiresAll))

parseLocatedModuleExportName :: [Token] -> Either ParserFailure (LocatedModuleExportName, SourceSpan, [Token])
parseLocatedModuleExportName tokens =
  case tokens of
    Token {tokenKind = TIdentifier constructorName, tokenSpan = constructorSpan} : rest ->
      Right (LocatedModuleExportName constructorName constructorSpan, constructorSpan, rest)
    [] -> Left (parserFailure (ExpectedSyntax "constructor export" ParserEndOfInput))
    token : _ ->
      Left
        ( parserFailureAt
            (tokenSpan token)
            (ExpectedSyntax "constructor export name" (ParserFoundToken (tokenKind token) (tokenLexeme token)))
        )

moduleExportNamespacePrefix :: Text -> Maybe NameNamespace
moduleExportNamespacePrefix prefix =
  case prefix of
    "constructor" -> Just ConstructorNamespace
    "type" -> Just TypeNamespace
    "class" -> Just CapabilityNamespace
    _ -> Nothing

parseImportSymbol :: [Token] -> Either ParserFailure (Text, SourceSpan, [Token])
parseImportSymbol tokens =
  case tokens of
    Token {tokenKind = TIdentifier symbolName, tokenSpan = symbolSpan} : rest ->
      Right (symbolName, symbolSpan, rest)
    [] ->
      Left (parserFailure (ExpectedSyntax "import symbol" ParserEndOfInput))
    token : _ ->
      Left
        ( parserFailureAt
            (tokenSpan token)
            (ExpectedSyntax "import symbol" (ParserFoundToken (tokenKind token) (tokenLexeme token)))
        )

collectUntilDot :: [Token] -> Either ParserFailure ([Token], [Token])
collectUntilDot = go []
  where
    go _ [] = Left (parserFailure (ExpectedSyntax "'.'" ParserEndOfInput))
    go acc allTokens@(token : rest) =
      case tokenKind token of
        TDot
          | null acc ->
              Left
                ( parserFailureAt
                    (tokenSpan token)
                    (ExpectedSyntax "signature text" (ParserBeforeToken TDot "." Nothing))
                )
          | otherwise -> Right (reverse acc, rest)
        _
          | not (null acc) && beginsStatement allTokens ->
              Left
                ( parserFailureAt
                    (tokenSpan token)
                    (ExpectedSyntax "'.'" (ParserBeforeToken (tokenKind token) (tokenLexeme token) Nothing))
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
    Token {tokenKind = TLParen}
      : Token {tokenKind = TOperator {}}
      : Token {tokenKind = TRParen}
      : Token {tokenKind = TColonColon}
      : _ -> True
    Token {tokenKind = TLParen}
      : Token {tokenKind = TOperator {}}
      : Token {tokenKind = TRParen}
      : Token {tokenKind = TEquals}
      : _ -> True
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

shouldParseQualifiedAliasStatement ::
  Set Text ->
  Text ->
  Token ->
  [Token] ->
  Either ParserFailure (SurfaceStatement, [Token]) ->
  Bool
shouldParseQualifiedAliasStatement knownAliases name nameToken tokensAfterName parsedSignature =
  case tokensAfterName of
    colonToken@Token {tokenKind = TColonColon} : _ ->
      isImmediatelyAfter nameToken colonToken
        && ( Set.member name knownAliases
               || not (isCompactSignatureCandidate name parsedSignature)
           )
    _ -> False

isCompactSignatureCandidate :: Text -> Either ParserFailure (SurfaceStatement, [Token]) -> Bool
isCompactSignatureCandidate name parsedSignature =
  case parsedSignature of
    Right (SSSignature _ _ signaturePayload, remaining) ->
      if isConstructorIdentifierText name
        then
          isConstructorStyleSignaturePayload signaturePayload
            || nextStatementStartsMatchingBinding name remaining
        else
          isSupportedSignaturePayload signaturePayload
            || isLikelyUnsupportedSignaturePayload signaturePayload
            || nextStatementStartsMatchingBinding name remaining
    Right _ -> False
    Left _ -> False

isConstructorStyleSignaturePayload :: SurfaceSignaturePayload -> Bool
isConstructorStyleSignaturePayload signaturePayload =
  case signaturePayload of
    SurfaceSignatureType (SurfaceTypeVariable variableName) ->
      isSingleLetterTypeVariable (identifierText variableName)
    SurfaceSignatureType _ -> True
    SurfaceConstrainedSignature {} -> True
    SurfaceUnsupportedSignature _ ->
      isLikelyUnsupportedSignaturePayload signaturePayload

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
collectImportAliasesInStatementList stopAtRightBrace = go (0 :: Int) Set.empty
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

rejectNestedModuleDeclaration :: Token -> Either ParserFailure a
rejectNestedModuleDeclaration moduleToken =
  Left
    ( parserFailureAt
        (tokenSpan moduleToken)
        (DeclarationFailure (DeclarationOutsideAllowedScope ModuleDeclaration))
    )

rejectNestedImportDeclaration :: Token -> Either ParserFailure a
rejectNestedImportDeclaration importToken =
  Left
    ( parserFailureAt
        (tokenSpan importToken)
        (DeclarationFailure (DeclarationOutsideAllowedScope ImportDeclaration))
    )

rejectNestedDataDeclaration :: Token -> Either ParserFailure a
rejectNestedDataDeclaration dataToken =
  Left
    ( parserFailureAt
        (tokenSpan dataToken)
        (DeclarationFailure (DeclarationOutsideAllowedScope DataDeclaration))
    )

rejectNestedOperatorBinding :: Token -> Either ParserFailure a
rejectNestedOperatorBinding operatorToken =
  Left
    ( parserFailureAt
        (tokenSpan operatorToken)
        (DeclarationFailure (DeclarationOutsideAllowedScope OperatorBinding))
    )

rejectNestedOperatorSignature :: Token -> Either ParserFailure a
rejectNestedOperatorSignature operatorToken =
  Left
    ( parserFailureAt
        (tokenSpan operatorToken)
        (DeclarationFailure (DeclarationOutsideAllowedScope OperatorSignature))
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

rejectReservedAbstractionSyntax :: Token -> Either ParserFailure a
rejectReservedAbstractionSyntax abstractionToken =
  Left
    ( parserFailureAt
        (tokenSpan abstractionToken)
        (UnsupportedSyntax (AbstractionSyntax (tokenLexeme abstractionToken)))
    )

rejectNestedOperatorDeclaration :: Token -> Either ParserFailure a
rejectNestedOperatorDeclaration operatorToken =
  Left
    ( parserFailureAt
        (tokenSpan operatorToken)
        (DeclarationFailure (DeclarationOutsideAllowedScope OperatorDeclaration))
    )

consumeDot :: [Token] -> Either ParserFailure [Token]
consumeDot tokens =
  case tokens of
    Token {tokenKind = TDot} : rest -> Right rest
    [] -> Left (parserFailure (ExpectedSyntax "'.'" ParserEndOfInput))
    token : _ ->
      Left
        ( parserFailureAt
            (tokenSpan token)
            (ExpectedSyntax "'.'" (ParserFoundToken (tokenKind token) (tokenLexeme token)))
        )

consumeEquals :: SourceSpan -> [Token] -> ParserFailureReason -> Either ParserFailure [Token]
consumeEquals endOfInputSpan tokens endOfInputReason =
  case tokens of
    Token {tokenKind = TEquals} : rest -> Right rest
    [] -> Left (parserFailureAt endOfInputSpan endOfInputReason)
    token : _ ->
      Left
        ( parserFailureAt
            (tokenSpan token)
            (ExpectedSyntax "'='" (ParserFoundToken (tokenKind token) (tokenLexeme token)))
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

mapLeft :: (errorA -> errorB) -> Either errorA value -> Either errorB value
mapLeft transform result =
  case result of
    Left failure -> Left (transform failure)
    Right value -> Right value
