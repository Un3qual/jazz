{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Declaration-level token-stream parsers for the surface parser.
module Jazz.Compiler.Parser.Declaration
  ( collectImportAliasesUntilBrace,
    collectImportAliasesUntilEnd,
    parseCapabilityDeclarationTokensDetailed,
    parseDataStatementParser,
    parseStatementParser,
  )
where

import Data.Char
  ( isLower,
  )
import Data.Set
  ( Set,
  )
import qualified Data.Set as Set
import Data.Text
  ( Text,
  )
import qualified Data.Text as Text
import Jazz.Compiler.Name
  ( Identifier,
    identifierText,
    mkIdentifier,
    mkOperatorBindingIdentifier,
  )
import Jazz.Compiler.Parser.AST
  ( SurfaceDataConstructor (..),
    SurfaceExpr,
    SurfaceSignaturePayload,
    SurfaceSignatureToken,
    SurfaceSignatureType,
    SurfaceStatement (..),
  )
import Jazz.Compiler.Parser.CapabilityDeclaration
  ( looksLikeSupportedCapabilityDeclaration,
    parseCapabilityDeclarationTokensDetailed,
    rejectReservedAbstractionSyntax,
  )
import Jazz.Compiler.Parser.Context
  ( ExpressionParser,
    ParserContext (..),
    StatementBlockParser,
    StatementContext (..),
  )
import Jazz.Compiler.Parser.DeclarationTokens
  ( collectUntilDotParser,
    consumeEquals,
    isConstructorIdentifierText,
    isReservedLiteralName,
    isTypeParameterIdentifierText,
    looksLikeOperatorDeclaration,
    looksLikeReservedAbstractionDeclaration,
    rejectNestedOperatorDeclaration,
  )
import Jazz.Compiler.Parser.Failure
  ( ParserDeclarationFailure (..),
    ParserDeclarationKind (..),
    ParserDuplicateNameRole (..),
    ParserEncountered (..),
    ParserFailure,
    ParserFailureReason (..),
    ParserInternalInvariant (..),
    ParserNameRole (..),
    ParserOperatorUse (..),
    parserFailure,
    parserFailureAt,
  )
import Jazz.Compiler.Parser.Lexer
  ( Token (..),
    TokenKind (..),
    isImmediatelyAfter,
  )
import Jazz.Compiler.Parser.ModuleDeclaration
  ( collectImportAliasesUntilBrace,
    collectImportAliasesUntilEnd,
    parseImportStatementParser,
    parseModuleStatementParser,
    registerImportAliases,
    rejectNestedImportDeclaration,
    rejectNestedModuleDeclaration,
  )
import Jazz.Compiler.Parser.Operator
  ( Associativity (..),
    OperatorInfo (..),
    OperatorTable,
    builtinOperatorTable,
    declaredOperatorInfoForPrecedence,
    declaredOperatorInfoForTier,
    insertDeclaredOperator,
    isBuiltinOperatorSymbol,
    isDeclaredOperator,
    isReservedOperatorSymbol,
    isValidUserOperatorSymbol,
  )
import Jazz.Compiler.Parser.Signature
  ( parseSignaturePayloadDetailed,
    parseSignatureTypeParser,
  )
import Jazz.Compiler.Parser.TokenParser
  ( Parser,
    failParserFailure,
    failTokenParser,
    failTokenParserAt,
    parseAnyToken,
    parseToken,
    runTokenStreamParserPrefixDetailed,
  )
import Jazz.Compiler.Parser.TokenStream
  ( TokenStream,
    tokenStreamLength,
    pattern EmptyTokens,
    pattern (:<),
  )
import Jazz.Compiler.TypeRepresentation
  ( pattern ConstrainedSignature,
    pattern SignatureArrowToken,
    pattern SignatureAtToken,
    pattern SignatureColonToken,
    pattern SignatureCommaToken,
    pattern SignatureLBraceToken,
    pattern SignatureLBracketToken,
    pattern SignatureLParenToken,
    pattern SignatureNameToken,
    pattern SignatureRBraceToken,
    pattern SignatureRBracketToken,
    pattern SignatureRParenToken,
    pattern SignatureType,
    pattern TypeApplication,
    pattern TypeBool,
    pattern TypeChar,
    pattern TypeFloat,
    pattern TypeFunction,
    pattern TypeInt,
    pattern TypeList,
    pattern TypeName,
    pattern TypeNumeric,
    pattern TypeText,
    pattern TypeTuple,
    pattern TypeVariable,
    pattern UnsupportedSignature,
  )
import qualified Text.Megaparsec as MP

data OperatorDeclarationFixityKeyword
  = OperatorTierKeyword
  | OperatorPrecedenceKeyword

parseDataStatementParser :: Parser SurfaceStatement
parseDataStatementParser =
  parseOwnedPrefix parseDataStatementFromTokens

parseOwnedPrefix :: (TokenStream -> Either ParserFailure (a, TokenStream)) -> Parser a
parseOwnedPrefix parseDeclaration = do
  tokens <- MP.getInput
  case parseDeclaration tokens of
    Left failure -> failParserFailure failure
    Right (value, remaining) ->
      value <$ consumeParsedPrefix remaining

consumeParsedPrefix :: TokenStream -> Parser ()
consumeParsedPrefix remaining = do
  current <- MP.getInput
  let consumedCount = tokenStreamLength current - tokenStreamLength remaining
  () <$ MP.takeP Nothing consumedCount

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
      moduleBodyContext =
        ParserContext
          { parserKnownAliases = Set.empty,
            parserDeclaredOperators = builtinOperatorTable,
            parserStatementContext = ModuleBodyContext
          }
      finish statements =
        (statements, context {parserKnownAliases = registerImportAliases knownAliases statements})
  case tokens of
    moduleToken@Token {tokenKind = TModule} :< _ ->
      case parserStatementContext context of
        TopLevelContext -> finish <$> parseModuleStatementParser (parseBlock moduleBodyContext)
        _ -> liftOwnedResult (rejectNestedModuleDeclaration moduleToken)
    importToken@Token {tokenKind = TImport} :< _ ->
      case parserStatementContext context of
        NestedBlockContext -> liftOwnedResult (rejectNestedImportDeclaration importToken)
        _ -> finish . pure <$> parseImportStatementParser
    operatorToken@Token {tokenKind = TIdentifier "operator"} :< rest
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
    _ -> finish <$> parseStatement (parseExpression context) context

liftOwnedResult :: Either ParserFailure a -> Parser a
liftOwnedResult result =
  case result of
    Left failure -> failParserFailure failure
    Right value -> pure value

parseOperatorDeclaration :: StatementContext -> OperatorTable -> Token -> TokenStream -> Either ParserFailure (OperatorInfo, TokenStream)
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

parseOperatorDeclarationSymbol :: TokenStream -> Either ParserFailure (Text, TokenStream)
parseOperatorDeclarationSymbol tokens =
  case tokens of
    Token {tokenKind = TOperator declaredSymbol} :< rest ->
      Right (declaredSymbol, rest)
    Token {tokenKind = TArrow, tokenLexeme = arrowLexeme} :< rest ->
      Right (arrowLexeme, rest)
    token :< _ ->
      Left
        ( parserFailureAt
            (tokenSpan token)
            ( ExpectedSyntax
                "operator symbol after 'operator'"
                (ParserFoundToken (tokenKind token) (tokenLexeme token))
            )
        )
    EmptyTokens ->
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

consumeOperatorFixityKeyword :: Token -> TokenStream -> Either ParserFailure (OperatorDeclarationFixityKeyword, TokenStream)
consumeOperatorFixityKeyword operatorToken tokens =
  case tokens of
    Token {tokenKind = TIdentifier "tier"} :< rest -> Right (OperatorTierKeyword, rest)
    Token {tokenKind = TIdentifier "precedence"} :< rest -> Right (OperatorPrecedenceKeyword, rest)
    token :< _ ->
      Left
        ( parserFailureAt
            (tokenSpan token)
            ( ExpectedSyntax
                "'tier' or 'precedence' in operator declaration"
                (ParserFoundToken (tokenKind token) (tokenLexeme token))
            )
        )
    EmptyTokens ->
      Left
        ( parserFailureAt
            (tokenSpan operatorToken)
            (ExpectedSyntax "'tier' or 'precedence'" (ParserEndOfInputIn "operator declaration"))
        )

parseOperatorDeclarationFixity :: Token -> Text -> OperatorDeclarationFixityKeyword -> TokenStream -> Either ParserFailure (OperatorInfo, TokenStream)
parseOperatorDeclarationFixity operatorToken declaredSymbol fixityKeyword tokens =
  case fixityKeyword of
    OperatorTierKeyword -> parseOperatorDeclarationTier operatorToken declaredSymbol tokens
    OperatorPrecedenceKeyword -> parseOperatorDeclarationPrecedence operatorToken declaredSymbol tokens

parseOperatorDeclarationTier :: Token -> Text -> TokenStream -> Either ParserFailure (OperatorInfo, TokenStream)
parseOperatorDeclarationTier operatorToken declaredSymbol tokens =
  case tokens of
    Token {tokenKind = TInt tier} :< rest ->
      case declaredOperatorInfoForTier declaredSymbol tier of
        Just operatorInfo -> Right (operatorInfo, rest)
        Nothing ->
          Left
            ( parserFailureAt
                (tokenSpan operatorToken)
                (DeclarationFailure OperatorTierOutOfRange)
            )
    token :< _ ->
      Left
        ( parserFailureAt
            (tokenSpan token)
            ( ExpectedSyntax
                "operator tier 1-5"
                (ParserFoundToken (tokenKind token) (tokenLexeme token))
            )
        )
    EmptyTokens ->
      Left
        ( parserFailureAt
            (tokenSpan operatorToken)
            (ExpectedSyntax "operator tier 1-5" (ParserEndOfInputIn "operator declaration"))
        )

parseOperatorDeclarationPrecedence :: Token -> Text -> TokenStream -> Either ParserFailure (OperatorInfo, TokenStream)
parseOperatorDeclarationPrecedence operatorToken declaredSymbol tokens =
  case tokens of
    Token {tokenKind = TInt precedence} :< rest ->
      case declaredOperatorInfoForPrecedence declaredSymbol precedence of
        Just operatorInfo -> Right (operatorInfo, rest)
        Nothing ->
          Left
            ( parserFailureAt
                (tokenSpan operatorToken)
                (DeclarationFailure OperatorPrecedenceOutOfRange)
            )
    token :< _ ->
      Left
        ( parserFailureAt
            (tokenSpan token)
            ( ExpectedSyntax
                "operator precedence 1-99"
                (ParserFoundToken (tokenKind token) (tokenLexeme token))
            )
        )
    EmptyTokens ->
      Left
        ( parserFailureAt
            (tokenSpan operatorToken)
            (ExpectedSyntax "operator precedence 1-99" (ParserEndOfInputIn "operator declaration"))
        )

parseOptionalOperatorAssociativity :: OperatorInfo -> TokenStream -> Either ParserFailure (OperatorInfo, TokenStream)
parseOptionalOperatorAssociativity operatorInfo tokens =
  case tokens of
    Token {tokenKind = TIdentifier "left"} :< rest ->
      Right (operatorInfo {operatorAssociativity = AssocLeft}, rest)
    Token {tokenKind = TIdentifier "right"} :< rest ->
      Right (operatorInfo {operatorAssociativity = AssocRight}, rest)
    Token {tokenKind = TIdentifier "nonassoc"} :< rest ->
      Right (operatorInfo {operatorAssociativity = AssocNonAssoc}, rest)
    token@Token {tokenKind = TIdentifier {}} :< _ ->
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

consumeOperatorDeclarationDot :: Token -> Text -> TokenStream -> Either ParserFailure TokenStream
consumeOperatorDeclarationDot operatorToken fixityLabel tokens =
  case tokens of
    Token {tokenKind = TDot} :< rest -> Right rest
    token :< _ ->
      Left
        ( parserFailureAt
            (tokenSpan token)
            ( ExpectedSyntax
                ("'.' after operator declaration " <> fixityLabel)
                (ParserFoundToken (tokenKind token) (tokenLexeme token))
            )
        )
    EmptyTokens ->
      Left
        ( parserFailureAt
            (tokenSpan operatorToken)
            (ExpectedSyntax ("'.' after operator declaration " <> fixityLabel) ParserEndOfInput)
        )

parseStatement :: Parser SurfaceExpr -> ParserContext -> Parser [SurfaceStatement]
parseStatement expression context = do
  tokens <- MP.getInput
  case tokens of
    Token {tokenKind = TLParen}
      :< operatorToken@Token {tokenKind = TOperator {}}
      :< Token {tokenKind = TRParen}
      :< Token {tokenKind = TColonColon}
      :< _ ->
        MP.takeP Nothing 3 *> (pure <$> parseOperatorSignature statementContext declaredOperators operatorToken)
    Token {tokenKind = TLParen}
      :< operatorToken@Token {tokenKind = TOperator {}}
      :< Token {tokenKind = TRParen}
      :< Token {tokenKind = TEquals}
      :< _ ->
        MP.takeP Nothing 4 *> (pure <$> parseOperatorBinding expression statementContext declaredOperators operatorToken)
    abstractionToken@Token {tokenKind = TIdentifier name} :< rest
      | isDeclarationContext statementContext,
        looksLikeSupportedCapabilityDeclaration name rest ->
          -- Temporary adapter for the capability declaration family only.
          pure <$> parseOwnedPrefix (parseCapabilityDeclarationTokensDetailed (runTokenStreamParserPrefixDetailed "impl expression" expression))
      | isDeclarationContext statementContext,
        looksLikeReservedAbstractionDeclaration name rest ->
          liftOwnedResult (rejectReservedAbstractionSyntax abstractionToken)
    dataToken@Token {tokenKind = TData} :< _ ->
      case statementContext of
        NestedBlockContext -> liftOwnedResult (rejectNestedDataDeclaration dataToken)
        _ -> pure <$> parseDataStatementParser
    nameToken@Token {tokenKind = TIdentifier name} :< afterName@(Token {tokenKind = TColonColon} :< _)
      | isReservedLiteralName name -> rejectName nameToken name
      | otherwise -> pure <$> parseSignatureOrQualifiedAlias expression knownAliases name nameToken afterName
    nameToken@Token {tokenKind = TIdentifier name} :< Token {tokenKind = TEquals} :< _
      | isReservedLiteralName name -> rejectName nameToken name
      | otherwise -> MP.takeP Nothing 2 *> (pure <$> parseLet expression (mkIdentifier name) nameToken)
    _ -> pure <$> parseExprStatement expression
  where
    knownAliases = parserKnownAliases context
    declaredOperators = parserDeclaredOperators context
    statementContext = parserStatementContext context
    rejectName token name =
      failTokenParserAt (tokenSpan token) (DeclarationFailure (ReservedLiteralName BindingName name))

parseSignatureOrQualifiedAlias :: Parser SurfaceExpr -> Set Text -> Text -> Token -> TokenStream -> Parser SurfaceStatement
parseSignatureOrQualifiedAlias expression knownAliases name nameToken tokensAfterName = do
  original <- MP.getParserState
  result <- MP.observing $ do
    _ <- parseAnyToken
    statement <- parseSignature (mkIdentifier name) nameToken
    remaining <- MP.getInput
    pure (statement, remaining)
  if shouldParseQualifiedAliasStatement knownAliases name nameToken tokensAfterName result
    then MP.setParserState original *> parseExprStatement expression
    else either MP.parseError (pure . fst) result

parseOperatorBinding :: Parser SurfaceExpr -> StatementContext -> OperatorTable -> Token -> Parser SurfaceStatement
parseOperatorBinding expression context declaredOperators operatorToken =
  case context of
    NestedBlockContext -> liftOwnedResult (rejectNestedOperatorBinding operatorToken)
    _ -> case tokenKind operatorToken of
      TOperator symbol
        | isBuiltinOperatorSymbol symbol ->
            failTokenParserAt (tokenSpan operatorToken) (DeclarationFailure (BuiltinOperatorCannotBeBound symbol))
        | not (isDeclaredOperator symbol declaredOperators) ->
            failTokenParserAt (tokenSpan operatorToken) (UndeclaredOperator symbol OperatorUseInBinding)
        | otherwise -> parseLet expression (mkOperatorBindingIdentifier symbol) operatorToken
      _ -> failTokenParserAt (tokenSpan operatorToken) (InternalParserFailure (ExpectedOperatorToken OperatorUseInBinding))

parseOperatorSignature :: StatementContext -> OperatorTable -> Token -> Parser SurfaceStatement
parseOperatorSignature context declaredOperators operatorToken =
  case context of
    NestedBlockContext -> liftOwnedResult (rejectNestedOperatorSignature operatorToken)
    _ -> case tokenKind operatorToken of
      TOperator symbol
        | isBuiltinOperatorSymbol symbol ->
            failTokenParserAt (tokenSpan operatorToken) (DeclarationFailure (BuiltinOperatorCannotBeSigned symbol))
        | not (isDeclaredOperator symbol declaredOperators) ->
            failTokenParserAt (tokenSpan operatorToken) (UndeclaredOperator symbol OperatorUseInSignature)
        | otherwise -> parseSignature (mkOperatorBindingIdentifier symbol) operatorToken
      _ -> failTokenParserAt (tokenSpan operatorToken) (InternalParserFailure (ExpectedOperatorToken OperatorUseInSignature))

parseSignature :: Identifier -> Token -> Parser SurfaceStatement
parseSignature name nameToken = do
  _ <- parseToken TColonColon
  signatureTokens <- collectUntilDotParser
  SSSignature name (tokenSpan nameToken) <$> liftOwnedResult (parseSignaturePayloadDetailed signatureTokens)

parseLet :: Parser SurfaceExpr -> Identifier -> Token -> Parser SurfaceStatement
parseLet expression name nameToken =
  SSLet name (tokenSpan nameToken) <$> expression <* parseToken TDot

parseExprStatement :: Parser SurfaceExpr -> Parser SurfaceStatement
parseExprStatement expression = do
  tokens <- MP.getInput
  case tokens of
    EmptyTokens -> failTokenParser (ExpectedSyntax "expression" ParserEndOfInput)
    firstToken :< _ -> SSExpr (tokenSpan firstToken) <$> expression <* parseToken TDot

parseDataStatementFromTokens :: TokenStream -> Either ParserFailure (SurfaceStatement, TokenStream)
parseDataStatementFromTokens tokens =
  case tokens of
    dataToken@Token {tokenKind = TData} :< tokensAfterDataKeyword -> do
      (typeName, afterTypeName) <- parseDataTypeName tokensAfterDataKeyword
      (typeParameters, afterTypeParameters) <- parseDataTypeParameters afterTypeName
      afterEquals <-
        consumeEquals
          (tokenSpan dataToken)
          afterTypeParameters
          (ExpectedSyntax "'='" (ParserEndOfInputAfter "data type name"))
      (constructors, remaining) <- parseDataConstructors typeName typeParameters afterEquals
      pure (SSData (tokenSpan dataToken) typeName typeParameters constructors, remaining)
    EmptyTokens ->
      Left (parserFailure (ExpectedSyntax "'data'" ParserEndOfInput))
    token :< _ ->
      Left
        ( parserFailureAt
            (tokenSpan token)
            (ExpectedSyntax "'data'" (ParserFoundToken (tokenKind token) (tokenLexeme token)))
        )

parseDataTypeName :: TokenStream -> Either ParserFailure (Identifier, TokenStream)
parseDataTypeName tokens =
  case tokens of
    Token {tokenKind = TIdentifier typeName, tokenSpan = typeSpan} :< rest
      | isConstructorIdentifierText typeName ->
          Right (mkIdentifier typeName, rest)
      | otherwise ->
          Left
            ( parserFailureAt
                typeSpan
                (ExpectedSyntax "type constructor name" (ParserFoundToken (TIdentifier typeName) typeName))
            )
    EmptyTokens ->
      Left (parserFailure (ExpectedSyntax "type constructor name" (ParserEndOfInputAfter "'data'")))
    token :< _ ->
      Left
        ( parserFailureAt
            (tokenSpan token)
            (ExpectedSyntax "type constructor name" (ParserFoundToken (tokenKind token) (tokenLexeme token)))
        )

parseDataTypeParameters :: TokenStream -> Either ParserFailure ([Identifier], TokenStream)
parseDataTypeParameters tokens = go Set.empty [] tokens
  where
    go seenParameters revParameters allTokens =
      case allTokens of
        Token {tokenKind = TEquals} :< _ ->
          Right (reverse revParameters, allTokens)
        Token {tokenKind = TIdentifier parameterName, tokenSpan = parameterSpan} :< rest
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

parseDataConstructors :: Identifier -> [Identifier] -> TokenStream -> Either ParserFailure ([SurfaceDataConstructor], TokenStream)
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
        Token {tokenKind = TDot} :< rest ->
          Right (reverse revConstructors, rest)
        Token {tokenKind = TOperator "|"} :< rest -> do
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
        EmptyTokens ->
          Left (parserFailure (ExpectedSyntax "'.'" (ParserEndOfInputIn "data declaration")))
        token :< _ ->
          Left
            ( parserFailureAt
                (tokenSpan token)
                (ExpectedSyntax "'|' or '.'" (ParserFoundToken (tokenKind token) (tokenLexeme token)))
            )

    surfaceDataConstructorName :: SurfaceDataConstructor -> Text
    surfaceDataConstructorName (SurfaceDataConstructor constructorName _) =
      identifierText constructorName

parseDataConstructor :: Identifier -> Set Text -> TokenStream -> Either ParserFailure (SurfaceDataConstructor, TokenStream)
parseDataConstructor typeName typeParameterNames tokens =
  case tokens of
    Token {tokenKind = TIdentifier constructorName, tokenSpan = constructorSpan} :< rest
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
    EmptyTokens ->
      Left (parserFailure (ExpectedSyntax "constructor declaration" (ParserEndOfInputIn "data declaration")))
    token :< _ ->
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
  TokenStream ->
  Either ParserFailure ([SurfaceSignatureType], TokenStream)
parseDataConstructorArguments typeName typeParameterNames revArguments allTokens =
  case allTokens of
    Token {tokenKind = TOperator "|"} :< _ ->
      Right (reverse revArguments, allTokens)
    Token {tokenKind = TDot} :< _ ->
      Right (reverse revArguments, allTokens)
    EmptyTokens ->
      Right (reverse revArguments, allTokens)
    firstToken :< _ -> do
      let fieldSpan = tokenSpan firstToken
      (fieldType, remaining) <-
        runTokenStreamParserPrefixDetailed "signature type" parseSignatureTypeParser allTokens
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
    TypeInt -> Set.empty
    TypeFloat -> Set.empty
    TypeNumeric _ -> Set.empty
    TypeBool -> Set.empty
    TypeChar -> Set.empty
    TypeText -> Set.empty
    TypeVariable name -> Set.singleton (identifierText name)
    TypeName _ -> Set.empty
    TypeApplication _ arguments ->
      Set.unions (map surfaceSignatureTypeVariables arguments)
    TypeList elementType ->
      surfaceSignatureTypeVariables elementType
    TypeTuple elementTypes ->
      Set.unions (map surfaceSignatureTypeVariables elementTypes)
    TypeFunction argumentType resultType ->
      surfaceSignatureTypeVariables argumentType
        `Set.union` surfaceSignatureTypeVariables resultType

isDeclarationContext :: StatementContext -> Bool
isDeclarationContext context =
  case context of
    TopLevelContext -> True
    ModuleBodyContext -> True
    NestedBlockContext -> False

shouldParseQualifiedAliasStatement ::
  Set Text ->
  Text ->
  Token ->
  TokenStream ->
  Either failure (SurfaceStatement, TokenStream) ->
  Bool
shouldParseQualifiedAliasStatement knownAliases name nameToken tokensAfterName parsedSignature =
  case tokensAfterName of
    colonToken@Token {tokenKind = TColonColon} :< _ ->
      isImmediatelyAfter nameToken colonToken
        && ( Set.member name knownAliases
               || not (isCompactSignatureCandidate name parsedSignature)
           )
    _ -> False

isCompactSignatureCandidate :: Text -> Either failure (SurfaceStatement, TokenStream) -> Bool
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
    SignatureType (TypeVariable variableName) ->
      isSingleLetterTypeVariable (identifierText variableName)
    SignatureType _ -> True
    ConstrainedSignature {} -> True
    UnsupportedSignature _ ->
      isLikelyUnsupportedSignaturePayload signaturePayload

isSupportedSignaturePayload :: SurfaceSignaturePayload -> Bool
isSupportedSignaturePayload signaturePayload =
  case signaturePayload of
    SignatureType _ -> True
    ConstrainedSignature _ _ -> True
    UnsupportedSignature _ -> False

isLikelyUnsupportedSignaturePayload :: SurfaceSignaturePayload -> Bool
isLikelyUnsupportedSignaturePayload signaturePayload =
  case signaturePayload of
    UnsupportedSignature [SignatureNameToken name] ->
      isSingleLetterTypeVariable name
    UnsupportedSignature tokens -> any isSignatureSyntaxToken tokens
    _ -> False

isSingleLetterTypeVariable :: Text -> Bool
isSingleLetterTypeVariable name =
  case Text.uncons name of
    Just (firstChar, rest) -> Text.null rest && isLower firstChar
    Nothing -> False

isSignatureSyntaxToken :: SurfaceSignatureToken -> Bool
isSignatureSyntaxToken signatureToken =
  case signatureToken of
    SignatureArrowToken -> True
    SignatureAtToken -> True
    SignatureColonToken -> True
    SignatureLParenToken -> True
    SignatureRParenToken -> True
    SignatureLBraceToken -> True
    SignatureRBraceToken -> True
    SignatureLBracketToken -> True
    SignatureRBracketToken -> True
    SignatureCommaToken -> True
    _ -> False

nextStatementStartsMatchingBinding :: Text -> TokenStream -> Bool
nextStatementStartsMatchingBinding name tokens =
  case tokens of
    Token {tokenKind = TIdentifier nextName} :< Token {tokenKind = TEquals} :< _ ->
      nextName == name
    _ -> False

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
