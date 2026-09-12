{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Declaration-level token-stream parsers for the surface parser.
module Jazz.Compiler.Parser.Declaration
  ( collectImportAliasesUntilBrace,
    collectImportAliasesUntilEnd,
    parseCapabilityDeclarationParser,
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
import Jazz.Compiler.Diagnostics (SourceSpan)
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
    parseCapabilityDeclarationParser,
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
    foundToken,
    parseAnyToken,
    parseToken,
    peekToken,
  )
import Jazz.Compiler.Parser.TokenStream
  ( TokenStream,
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
          _ <- parseAnyToken
          operatorInfo <- parseOperatorDeclaration (parserStatementContext context) declaredOperators operatorToken
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

parseOperatorDeclaration :: StatementContext -> OperatorTable -> Token -> Parser OperatorInfo
parseOperatorDeclaration context declaredOperators operatorToken =
  case context of
    NestedBlockContext -> liftOwnedResult (rejectNestedOperatorDeclaration operatorToken)
    _ -> do
      symbol <- parseOperatorDeclarationSymbol
      liftOwnedResult (validateDeclaredOperatorSymbol declaredOperators operatorToken symbol)
      keyword <- parseOperatorFixityKeyword operatorToken
      info <- parseOperatorFixity operatorToken symbol keyword
      associated <- parseOptionalOperatorAssociativity info
      next <- peekToken
      let expected = "'.' after operator declaration " <> operatorDeclarationFixityLabel keyword
      case next of
        Just Token {tokenKind = TDot} -> associated <$ parseAnyToken
        Just token -> failTokenParserAt (tokenSpan token) (ExpectedSyntax expected (foundToken token))
        Nothing -> failTokenParserAt (tokenSpan operatorToken) (ExpectedSyntax expected ParserEndOfInput)

parseOperatorDeclarationSymbol :: Parser Text
parseOperatorDeclarationSymbol = do
  next <- peekToken
  case next of
    Just Token {tokenKind = TOperator symbol} -> symbol <$ parseAnyToken
    Just Token {tokenKind = TArrow, tokenLexeme = symbol} -> symbol <$ parseAnyToken
    Just token -> failTokenParserAt (tokenSpan token) (ExpectedSyntax "operator symbol after 'operator'" (foundToken token))
    Nothing -> failTokenParser (ExpectedSyntax "operator symbol after 'operator'" ParserEndOfInput)

parseOperatorFixityKeyword :: Token -> Parser OperatorDeclarationFixityKeyword
parseOperatorFixityKeyword operatorToken = do
  next <- peekToken
  case next of
    Just Token {tokenKind = TIdentifier "tier"} -> OperatorTierKeyword <$ parseAnyToken
    Just Token {tokenKind = TIdentifier "precedence"} -> OperatorPrecedenceKeyword <$ parseAnyToken
    Just token -> failTokenParserAt (tokenSpan token) (ExpectedSyntax "'tier' or 'precedence' in operator declaration" (foundToken token))
    Nothing -> failTokenParserAt (tokenSpan operatorToken) (ExpectedSyntax "'tier' or 'precedence'" (ParserEndOfInputIn "operator declaration"))

parseOperatorFixity :: Token -> Text -> OperatorDeclarationFixityKeyword -> Parser OperatorInfo
parseOperatorFixity operatorToken symbol keyword = do
  let (expected, construct, rangeFailure) = case keyword of
        OperatorTierKeyword -> ("operator tier 1-5", declaredOperatorInfoForTier symbol, OperatorTierOutOfRange)
        OperatorPrecedenceKeyword -> ("operator precedence 1-99", declaredOperatorInfoForPrecedence symbol, OperatorPrecedenceOutOfRange)
  next <- peekToken
  case next of
    Just Token {tokenKind = TInt value} -> case construct value of
      Just info -> info <$ parseAnyToken
      Nothing -> failTokenParserAt (tokenSpan operatorToken) (DeclarationFailure rangeFailure)
    Just token -> failTokenParserAt (tokenSpan token) (ExpectedSyntax expected (foundToken token))
    Nothing -> failTokenParserAt (tokenSpan operatorToken) (ExpectedSyntax expected (ParserEndOfInputIn "operator declaration"))

parseOptionalOperatorAssociativity :: OperatorInfo -> Parser OperatorInfo
parseOptionalOperatorAssociativity info = do
  next <- peekToken
  case next of
    Just Token {tokenKind = TIdentifier "left"} -> info {operatorAssociativity = AssocLeft} <$ parseAnyToken
    Just Token {tokenKind = TIdentifier "right"} -> info {operatorAssociativity = AssocRight} <$ parseAnyToken
    Just Token {tokenKind = TIdentifier "nonassoc"} -> info {operatorAssociativity = AssocNonAssoc} <$ parseAnyToken
    Just token@Token {tokenKind = TIdentifier {}} -> failTokenParserAt (tokenSpan token) (ExpectedSyntax "operator associativity 'left', 'right', or 'nonassoc'" (foundToken token))
    _ -> pure info

operatorDeclarationFixityLabel :: OperatorDeclarationFixityKeyword -> Text
operatorDeclarationFixityLabel keyword = case keyword of
  OperatorTierKeyword -> "tier"
  OperatorPrecedenceKeyword -> "precedence"

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
          pure <$> parseCapabilityDeclarationParser expression
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

parseDataStatementParser :: Parser SurfaceStatement
parseDataStatementParser = do
  dataToken <- parseToken TData
  typeName <- parseDataTypeName
  parameters <- parseDataTypeParameters
  next <- peekToken
  case next of
    Nothing -> failTokenParserAt (tokenSpan dataToken) (ExpectedSyntax "'='" (ParserEndOfInputAfter "data type name"))
    _ -> do
      _ <- parseToken TEquals
      SSData (tokenSpan dataToken) typeName parameters <$> parseDataConstructors typeName parameters

parseDataTypeName :: Parser Identifier
parseDataTypeName = do
  next <- peekToken
  case next of
    Just token@Token {tokenKind = TIdentifier name}
      | isConstructorIdentifierText name -> mkIdentifier name <$ parseAnyToken
      | otherwise -> failTokenParserAt (tokenSpan token) (ExpectedSyntax "type constructor name" (foundToken token))
    Nothing -> failTokenParser (ExpectedSyntax "type constructor name" (ParserEndOfInputAfter "'data'"))
    Just token -> failTokenParserAt (tokenSpan token) (ExpectedSyntax "type constructor name" (foundToken token))

parseDataTypeParameters :: Parser [Identifier]
parseDataTypeParameters = go Set.empty []
  where
    go seen reversed = do
      next <- peekToken
      case next of
        Just token@Token {tokenKind = TIdentifier name}
          | isTypeParameterIdentifierText name ->
              if Set.member name seen
                then failTokenParserAt (tokenSpan token) (DeclarationFailure (DuplicateName DataTypeParameter name DataDeclaration))
                else parseAnyToken *> go (Set.insert name seen) (mkIdentifier name : reversed)
          | otherwise -> failTokenParserAt (tokenSpan token) (ExpectedSyntax "lowercase type parameter or '='" (foundToken token))
        _ -> pure (reverse reversed)

parseDataConstructors :: Identifier -> [Identifier] -> Parser [SurfaceDataConstructor]
parseDataConstructors typeName parameters = do
  (_, first) <- parseDataConstructor typeName parameterNames
  go (Set.singleton (constructorName first)) [first]
  where
    parameterNames = Set.fromList (map identifierText parameters)
    constructorName (SurfaceDataConstructor name _) = identifierText name
    go seen reversed = do
      next <- peekToken
      case next of
        Just Token {tokenKind = TDot} -> reverse reversed <$ parseAnyToken
        Just Token {tokenKind = TOperator "|"} -> do
          _ <- parseAnyToken
          (constructorSpan, constructor) <- parseDataConstructor typeName parameterNames
          let name = constructorName constructor
          if Set.member name seen
            then failTokenParserAt constructorSpan (DeclarationFailure (DuplicateName DataConstructorName name DataDeclaration))
            else go (Set.insert name seen) (constructor : reversed)
        Nothing -> failTokenParser (ExpectedSyntax "'.'" (ParserEndOfInputIn "data declaration"))
        Just token -> failTokenParserAt (tokenSpan token) (ExpectedSyntax "'|' or '.'" (foundToken token))

parseDataConstructor :: Identifier -> Set Text -> Parser (SourceSpan, SurfaceDataConstructor)
parseDataConstructor typeName parameters = do
  next <- peekToken
  case next of
    Just token@Token {tokenKind = TIdentifier name}
      | isConstructorIdentifierText name -> do
          _ <- parseAnyToken
          fields <- arguments []
          pure (tokenSpan token, SurfaceDataConstructor (mkIdentifier name) fields)
    Nothing -> failTokenParser (ExpectedSyntax "constructor declaration" (ParserEndOfInputIn "data declaration"))
    Just token -> failTokenParserAt (tokenSpan token) (ExpectedSyntax "constructor declaration" (foundToken token))
  where
    arguments reversed = do
      next <- peekToken
      case next of
        Just Token {tokenKind = TOperator "|"} -> pure (reverse reversed)
        Just Token {tokenKind = TDot} -> pure (reverse reversed)
        Nothing -> pure (reverse reversed)
        Just token -> do
          field <- parseSignatureTypeParser
          case Set.toList (surfaceSignatureTypeVariables field `Set.difference` parameters) of
            name : _ -> failTokenParserAt (tokenSpan token) (DeclarationFailure (UndeclaredConstructorTypeParameter name (identifierText typeName)))
            [] -> arguments (field : reversed)

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
