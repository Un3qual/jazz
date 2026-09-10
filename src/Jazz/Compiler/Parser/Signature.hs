{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Signature grammar helpers for the surface parser.
module Jazz.Compiler.Parser.Signature
  ( parseConstrainedSignatureTypeDetailed,
    parseConstraintBlockHeadsDetailed,
    parseSignatureTypeParser,
    parseSignatureTypePrefixDetailed,
    parseSignaturePayload,
    parseSignaturePayloadDetailed,
    splitTopLevelCommaTokensDetailed,
  )
where

import Control.Applicative ((<|>))
import Control.Monad (void)
import Data.Bifunctor (first)
import Data.Char (isLower)
import Data.Text (Text)
import qualified Data.Text as Text
import Jazz.Compiler.Name
  ( Identifier,
    identifierText,
    mkIdentifier,
    mkQualifiedIdentifier,
  )
import Jazz.Compiler.Parser.AST
  ( SurfaceNumericType,
    SurfaceSignatureConstraint,
    SurfaceSignaturePayload,
    SurfaceSignatureToken,
    SurfaceSignatureType,
  )
import Jazz.Compiler.Parser.Failure
  ( ParserEncountered (..),
    ParserFailure,
    ParserFailureReason (..),
    parserFailureAt,
  )
import Jazz.Compiler.Parser.Lexer
  ( Token (..),
    TokenKind (..),
    isImmediatelyAfter,
  )
import qualified Jazz.Compiler.Parser.TokenParser as TokenParser
import Jazz.Compiler.TypeRepresentation
  ( NumericType (..),
    pattern ConstrainedSignature,
    pattern SignatureArrowToken,
    pattern SignatureAtToken,
    pattern SignatureColonToken,
    pattern SignatureCommaToken,
    pattern SignatureConstraint,
    pattern SignatureIntToken,
    pattern SignatureLBraceToken,
    pattern SignatureLBracketToken,
    pattern SignatureLParenToken,
    pattern SignatureNameToken,
    pattern SignatureOperatorToken,
    pattern SignatureOtherToken,
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

parseSignaturePayload :: [Token] -> SurfaceSignaturePayload
parseSignaturePayload signatureTokens =
  case parseSupportedSignaturePayload signatureTokens of
    Just signaturePayload -> signaturePayload
    Nothing -> UnsupportedSignature (map surfaceSignatureTokenFromToken signatureTokens)

-- Unsupported legacy signature forms remain representable. New qualified
-- constraint heads, however, have a precise two-component grammar.
parseSignaturePayloadDetailed :: [Token] -> Either ParserFailure SurfaceSignaturePayload
parseSignaturePayloadDetailed tokens = do
  case tokens of
    Token {tokenKind = TAt} : Token {tokenKind = TLBrace} : rest -> void (parseConstraintBlockHeadsDetailed rest)
    _ -> Right ()
  pure (parseSignaturePayload tokens)

-- | Validate and retain qualified heads in a constraint block, leaving the
-- tokens after its closing brace. Type arguments are not class references;
-- an unfinished legacy payload must not consume the next statement.
parseConstraintBlockHeadsDetailed :: [Token] -> Either ParserFailure ([(Token, Token)], [Token])
parseConstraintBlockHeadsDetailed = validateHead 0
  where
    validateHead depth (Token {tokenKind = TLParen} : rest) = validateHead (depth + 1) rest
    validateHead depth (alias : colon@Token {tokenKind = TColonColon} : member : rest) = do
      if isImmediatelyAfter alias colon && isImmediatelyAfter colon member
        then Right ()
        else invalid colon "adjacent alias-qualified class name"
      case tokenKind member of
        TIdentifier {} -> Right ()
        _ -> invalid member "class name after '::'"
      case rest of
        extra@Token {tokenKind = TColonColon} : _ -> invalid extra "two-component class name"
        _ -> first ((alias, member) :) <$> scan depth rest
    validateHead depth rest = scan depth rest

    scan :: Int -> [Token] -> Either ParserFailure ([(Token, Token)], [Token])
    scan _ [] = Right ([], [])
    scan depth (token : rest) = case tokenKind token of
      TDot -> Right ([], token : rest)
      TLParen -> scan (depth + 1) rest
      TLBracket -> scan (depth + 1) rest
      TRParen -> scan (max 0 (depth - 1)) rest
      TRBracket -> scan (max 0 (depth - 1)) rest
      TComma | depth == 0 -> validateHead 0 rest
      TRBrace | depth == 0 -> Right ([], rest)
      _ -> scan depth rest

    invalid token expected =
      Left
        ( parserFailureAt
            (tokenSpan token)
            (ExpectedSyntax expected (ParserFoundToken (tokenKind token) (tokenLexeme token)))
        )

parseSupportedSignaturePayload :: [Token] -> Maybe SurfaceSignaturePayload
parseSupportedSignaturePayload tokens =
  case TokenParser.runTokenParser "signature payload" signaturePayloadParser tokens of
    Right signaturePayload -> Just signaturePayload
    Left _ -> Nothing

parseConstrainedSignatureTypeDetailed :: [Token] -> Either ParserFailure SurfaceSignatureType
parseConstrainedSignatureTypeDetailed =
  TokenParser.runTokenParserDetailed "constrained signature type" signatureTypeParser

parseSignatureTypePrefixDetailed :: [Token] -> Either ParserFailure (SurfaceSignatureType, [Token])
parseSignatureTypePrefixDetailed =
  TokenParser.runTokenParserPrefixDetailed "signature type" signatureTypeParser

splitTopLevelCommaTokensDetailed :: [Token] -> Either ParserFailure [[Token]]
splitTopLevelCommaTokensDetailed =
  TokenParser.runTokenParserDetailed "top-level comma list" topLevelCommaTokensParser

signaturePayloadParser :: TokenParser.Parser SurfaceSignaturePayload
signaturePayloadParser =
  constrainedSignaturePayloadParser
    <|> (SignatureType <$> signatureTypeParser)

constrainedSignaturePayloadParser :: TokenParser.Parser SurfaceSignaturePayload
constrainedSignaturePayloadParser = do
  _ <- TokenParser.parseTokenKind TAt
  _ <- TokenParser.parseTokenKind TLBrace
  constraints <- constraintBlockParser
  _ <- TokenParser.parseTokenKind TRBrace
  _ <- TokenParser.parseTokenKind TColon
  ConstrainedSignature constraints <$> signatureTypeParser

constraintBlockParser :: TokenParser.Parser [SurfaceSignatureConstraint]
constraintBlockParser =
  emptyConstraintBlockParser
    <|> signatureConstraintParser `MP.sepBy1` commaParser
  where
    emptyConstraintBlockParser =
      MP.lookAhead (TokenParser.parseTokenKind TRBrace) *> pure []

signatureConstraintParser :: TokenParser.Parser SurfaceSignatureConstraint
signatureConstraintParser = do
  signatureType <- signatureTypeParser
  case signatureType of
    TypeApplication constraintName arguments ->
      pure (SignatureConstraint constraintName arguments)
    TypeName constraintName ->
      pure (SignatureConstraint constraintName [])
    _ ->
      MP.empty

signatureTypeParser :: TokenParser.Parser SurfaceSignatureType
signatureTypeParser = do
  argumentType <- functionOperandTypeParser
  parseFunctionResult argumentType <|> pure argumentType

parseSignatureTypeParser :: TokenParser.Parser SurfaceSignatureType
parseSignatureTypeParser = signatureTypeParser

parseFunctionResult :: SurfaceSignatureType -> TokenParser.Parser SurfaceSignatureType
parseFunctionResult argumentType = do
  _ <- TokenParser.parseTokenKind TArrow
  TypeFunction argumentType <$> signatureTypeParser

functionOperandTypeParser :: TokenParser.Parser SurfaceSignatureType
functionOperandTypeParser =
  MP.try typeApplicationParser
    <|> namedSignatureTypeParser
    <|> listSignatureTypeParser
    <|> parenthesizedSignatureTypeParser

listSignatureTypeParser :: TokenParser.Parser SurfaceSignatureType
listSignatureTypeParser =
  TypeList
    <$> betweenTokenKinds TLBracket TRBracket signatureTypeParser

parenthesizedSignatureTypeParser :: TokenParser.Parser SurfaceSignatureType
parenthesizedSignatureTypeParser =
  betweenTokenKinds TLParen TRParen $
    ( MP.lookAhead (TokenParser.parseTokenKind TRParen)
        *> pure (TypeTuple [])
    )
      <|> do
        firstElement <- signatureTypeParser
        remainingElements <- MP.many (commaParser *> signatureTypeParser)
        case remainingElements of
          [] ->
            pure firstElement
          _ ->
            pure (TypeTuple (firstElement : remainingElements))

namedSignatureTypeParser :: TokenParser.Parser SurfaceSignatureType
namedSignatureTypeParser = do
  (typeNameToken, typeNameIdentifier) <- signatureTypeHeadParser
  maybeNextToken <- TokenParser.peekToken
  case maybeNextToken of
    Just nextToken
      | tokenKind nextToken == TLParen,
        isImmediatelyAfter typeNameToken nextToken ->
          MP.empty
    _ -> pure ()
  let typeName = identifierText typeNameIdentifier
      typeMemberName = tokenLexeme typeNameToken
  case parseNamedSignatureType typeName of
    Just signatureType ->
      pure signatureType
    Nothing ->
      pure
        ( if identifierStartsLower typeMemberName
            then TypeVariable typeNameIdentifier
            else TypeName typeNameIdentifier
        )

typeApplicationParser :: TokenParser.Parser SurfaceSignatureType
typeApplicationParser = do
  (_, typeNameIdentifier) <- signatureTypeHeadParser
  arguments <-
    betweenTokenKinds
      TLParen
      TRParen
      (signatureTypeParser `MP.sepBy1` commaParser)
  pure
    ( case (identifierText typeNameIdentifier, arguments) of
        ("List", [elementType]) -> TypeList elementType
        _ -> TypeApplication typeNameIdentifier arguments
    )

signatureTypeHeadParser :: TokenParser.Parser (Token, Identifier)
signatureTypeHeadParser = do
  firstToken <- identifierTokenParser
  maybeQualifiedMember <-
    MP.optional $ do
      _ <- TokenParser.parseTokenKind TColonColon
      memberToken <- identifierTokenParser
      pure memberToken
  case maybeQualifiedMember of
    Just memberToken ->
      pure
        ( memberToken,
          mkQualifiedIdentifier (tokenLexeme firstToken) (tokenLexeme memberToken)
        )
    Nothing ->
      pure (firstToken, mkIdentifier (tokenLexeme firstToken))

identifierTokenParser :: TokenParser.Parser Token
identifierTokenParser =
  TokenParser.parseTokenWhere
    ( \token ->
        case tokenKind token of
          TIdentifier {} -> True
          _ -> False
    )
    "identifier"

identifierStartsLower :: Text -> Bool
identifierStartsLower identifier =
  case Text.uncons identifier of
    Just (firstCharacter, _) -> isLower firstCharacter
    Nothing -> False

topLevelCommaTokensParser :: TokenParser.Parser [[Token]]
topLevelCommaTokensParser = commaTokenGroupParser `MP.sepBy1` commaParser

commaTokenGroupParser :: TokenParser.Parser [Token]
commaTokenGroupParser =
  concat <$> MP.some topLevelCommaGroupPartParser

topLevelCommaGroupPartParser :: TokenParser.Parser [Token]
topLevelCommaGroupPartParser =
  wrappedCommaTokensParser TLParen TRParen
    <|> wrappedCommaTokensParser TLBracket TRBracket
    <|> singleTopLevelCommaTokenParser

nestedCommaGroupPartParser :: TokenParser.Parser [Token]
nestedCommaGroupPartParser =
  wrappedCommaTokensParser TLParen TRParen
    <|> wrappedCommaTokensParser TLBracket TRBracket
    <|> singleNestedCommaTokenParser

wrappedCommaTokensParser :: TokenKind -> TokenKind -> TokenParser.Parser [Token]
wrappedCommaTokensParser openKind closeKind = do
  openToken <- TokenParser.parseToken openKind
  innerTokens <- concat <$> MP.many nestedCommaGroupPartParser
  closeToken <- TokenParser.parseToken closeKind
  pure (openToken : innerTokens ++ [closeToken])

singleTopLevelCommaTokenParser :: TokenParser.Parser [Token]
singleTopLevelCommaTokenParser =
  singleton
    <$> TokenParser.parseTokenWhere isTopLevelCommaGroupToken "top-level comma group token"

singleNestedCommaTokenParser :: TokenParser.Parser [Token]
singleNestedCommaTokenParser =
  singleton
    <$> TokenParser.parseTokenWhere isNestedCommaGroupToken "nested comma group token"

isTopLevelCommaGroupToken :: Token -> Bool
isTopLevelCommaGroupToken token =
  case tokenKind token of
    TComma -> False
    TRParen -> False
    TRBracket -> False
    _ -> True

isNestedCommaGroupToken :: Token -> Bool
isNestedCommaGroupToken token =
  case tokenKind token of
    TRParen -> False
    TRBracket -> False
    _ -> True

commaParser :: TokenParser.Parser TokenKind
commaParser =
  TokenParser.parseTokenKind TComma

betweenTokenKinds :: TokenKind -> TokenKind -> TokenParser.Parser a -> TokenParser.Parser a
betweenTokenKinds openKind closeKind =
  MP.between (TokenParser.parseTokenKind openKind) (TokenParser.parseTokenKind closeKind)

singleton :: a -> [a]
singleton value = [value]

parseNamedSignatureType :: Text -> Maybe SurfaceSignatureType
parseNamedSignatureType typeName =
  case typeName of
    "Int" -> Just TypeInt
    "Float" -> Just TypeFloat
    "Bool" -> Just TypeBool
    "Char" -> Just TypeChar
    "Text" -> Just TypeText
    _ -> TypeNumeric <$> parseSurfaceNumericType typeName

parseSurfaceNumericType :: Text -> Maybe SurfaceNumericType
parseSurfaceNumericType typeName =
  case typeName of
    "Int8" -> Just NumericInt8
    "Int16" -> Just NumericInt16
    "Int32" -> Just NumericInt32
    "Int64" -> Just NumericInt64
    "UInt8" -> Just NumericUInt8
    "UInt16" -> Just NumericUInt16
    "UInt32" -> Just NumericUInt32
    "UInt64" -> Just NumericUInt64
    "Float16" -> Just NumericFloat16
    "Float32" -> Just NumericFloat32
    "Float64" -> Just NumericFloat64
    _ -> Nothing

surfaceSignatureTokenFromToken :: Token -> SurfaceSignatureToken
surfaceSignatureTokenFromToken token =
  case tokenKind token of
    TIdentifier name -> SignatureNameToken name
    TInt value -> SignatureIntToken value
    TArrow -> SignatureArrowToken
    TAt -> SignatureAtToken
    TColon -> SignatureColonToken
    TLParen -> SignatureLParenToken
    TRParen -> SignatureRParenToken
    TLBrace -> SignatureLBraceToken
    TRBrace -> SignatureRBraceToken
    TLBracket -> SignatureLBracketToken
    TRBracket -> SignatureRBracketToken
    TComma -> SignatureCommaToken
    TOperator symbol -> SignatureOperatorToken symbol
    _ -> SignatureOtherToken (tokenLexeme token)
