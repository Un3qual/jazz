{-# LANGUAGE OverloadedStrings #-}

-- | Signature grammar helpers for the surface parser.
module Jazz.Compiler.Parser.Signature
  ( parseConstrainedSignatureType,
    parseConstrainedSignatureTypeDetailed,
    parseSignatureTypeParser,
    parseSignatureTypePrefix,
    parseSignatureTypePrefixDetailed,
    parseSignaturePayload,
    splitTopLevelCommaTokens,
    splitTopLevelCommaTokensDetailed,
  )
where

import Control.Applicative ((<|>))
import Data.Char (isLower)
import Data.Text (Text)
import qualified Data.Text as Text
import Jazz.Compiler.Diagnostics (Diagnostic)
import Jazz.Compiler.Name
  ( Identifier,
    identifierText,
    mkIdentifier,
    mkQualifiedIdentifier,
  )
import Jazz.Compiler.Parser.AST
  ( SurfaceNumericType (..),
    SurfaceSignatureConstraint (..),
    SurfaceSignaturePayload (..),
    SurfaceSignatureToken (..),
    SurfaceSignatureType (..),
  )
import Jazz.Compiler.Parser.Failure
  ( ParserFailure,
    parserFailureDiagnostic,
  )
import Jazz.Compiler.Parser.Lexer
  ( Token (..),
    TokenKind (..),
    isImmediatelyAfter,
  )
import qualified Jazz.Compiler.Parser.TokenParser as TokenParser
import qualified Text.Megaparsec as MP

parseSignaturePayload :: [Token] -> SurfaceSignaturePayload
parseSignaturePayload signatureTokens =
  case parseSupportedSignaturePayload signatureTokens of
    Just signaturePayload -> signaturePayload
    Nothing -> SurfaceUnsupportedSignature (map surfaceSignatureTokenFromToken signatureTokens)

parseSupportedSignaturePayload :: [Token] -> Maybe SurfaceSignaturePayload
parseSupportedSignaturePayload tokens =
  case TokenParser.runTokenParser "signature payload" signaturePayloadParser tokens of
    Right signaturePayload -> Just signaturePayload
    Left _ -> Nothing

parseConstrainedSignatureType :: [Token] -> Either Diagnostic SurfaceSignatureType
parseConstrainedSignatureType =
  mapLeft parserFailureDiagnostic . parseConstrainedSignatureTypeDetailed

parseConstrainedSignatureTypeDetailed :: [Token] -> Either ParserFailure SurfaceSignatureType
parseConstrainedSignatureTypeDetailed =
  TokenParser.runTokenParserDetailed "constrained signature type" signatureTypeParser

parseSignatureTypePrefix :: [Token] -> Either Diagnostic (SurfaceSignatureType, [Token])
parseSignatureTypePrefix =
  mapLeft parserFailureDiagnostic . parseSignatureTypePrefixDetailed

parseSignatureTypePrefixDetailed :: [Token] -> Either ParserFailure (SurfaceSignatureType, [Token])
parseSignatureTypePrefixDetailed =
  TokenParser.runTokenParserPrefixDetailed "signature type" signatureTypeParser

splitTopLevelCommaTokens :: [Token] -> Either Diagnostic [[Token]]
splitTopLevelCommaTokens =
  mapLeft parserFailureDiagnostic . splitTopLevelCommaTokensDetailed

splitTopLevelCommaTokensDetailed :: [Token] -> Either ParserFailure [[Token]]
splitTopLevelCommaTokensDetailed =
  TokenParser.runTokenParserDetailed "top-level comma list" topLevelCommaTokensParser

mapLeft :: (errorA -> errorB) -> Either errorA value -> Either errorB value
mapLeft transform result =
  case result of
    Left failure -> Left (transform failure)
    Right value -> Right value

signaturePayloadParser :: TokenParser.Parser SurfaceSignaturePayload
signaturePayloadParser =
  constrainedSignaturePayloadParser
    <|> (SurfaceSignatureType <$> signatureTypeParser)

constrainedSignaturePayloadParser :: TokenParser.Parser SurfaceSignaturePayload
constrainedSignaturePayloadParser = do
  _ <- TokenParser.parseTokenKind TAt
  _ <- TokenParser.parseTokenKind TLBrace
  constraints <- constraintBlockParser
  _ <- TokenParser.parseTokenKind TRBrace
  _ <- TokenParser.parseTokenKind TColon
  SurfaceConstrainedSignature constraints <$> signatureTypeParser

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
    SurfaceTypeApplication constraintName arguments ->
      pure (SurfaceSignatureConstraint constraintName arguments)
    SurfaceTypeName constraintName ->
      pure (SurfaceSignatureConstraint constraintName [])
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
  SurfaceTypeFunction argumentType <$> signatureTypeParser

functionOperandTypeParser :: TokenParser.Parser SurfaceSignatureType
functionOperandTypeParser =
  MP.try typeApplicationParser
    <|> namedSignatureTypeParser
    <|> listSignatureTypeParser
    <|> parenthesizedSignatureTypeParser

listSignatureTypeParser :: TokenParser.Parser SurfaceSignatureType
listSignatureTypeParser =
  SurfaceTypeList
    <$> betweenTokenKinds TLBracket TRBracket signatureTypeParser

parenthesizedSignatureTypeParser :: TokenParser.Parser SurfaceSignatureType
parenthesizedSignatureTypeParser =
  betweenTokenKinds TLParen TRParen $
    ( MP.lookAhead (TokenParser.parseTokenKind TRParen)
        *> pure (SurfaceTypeTuple [])
    )
      <|> do
        firstElement <- signatureTypeParser
        remainingElements <- MP.many (commaParser *> signatureTypeParser)
        case remainingElements of
          [] ->
            pure firstElement
          _ ->
            pure (SurfaceTypeTuple (firstElement : remainingElements))

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
            then SurfaceTypeVariable typeNameIdentifier
            else SurfaceTypeName typeNameIdentifier
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
        ("List", [elementType]) -> SurfaceTypeList elementType
        _ -> SurfaceTypeApplication typeNameIdentifier arguments
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
topLevelCommaTokensParser = do
  firstGroup <- commaTokenGroupParser
  remainingGroups <- MP.many (commaParser *> commaTokenGroupParser)
  pure (firstGroup : remainingGroups)

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
betweenTokenKinds openKind closeKind parser = do
  _ <- TokenParser.parseTokenKind openKind
  value <- parser
  _ <- TokenParser.parseTokenKind closeKind
  pure value

singleton :: a -> [a]
singleton value = [value]

parseNamedSignatureType :: Text -> Maybe SurfaceSignatureType
parseNamedSignatureType typeName =
  case typeName of
    "Int" -> Just SurfaceTypeInt
    "Float" -> Just SurfaceTypeFloat
    "Bool" -> Just SurfaceTypeBool
    "Char" -> Just SurfaceTypeChar
    "Text" -> Just SurfaceTypeText
    _ -> SurfaceTypeNumeric <$> parseSurfaceNumericType typeName

parseSurfaceNumericType :: Text -> Maybe SurfaceNumericType
parseSurfaceNumericType typeName =
  case typeName of
    "Int8" -> Just SurfaceNumericInt8
    "Int16" -> Just SurfaceNumericInt16
    "Int32" -> Just SurfaceNumericInt32
    "Int64" -> Just SurfaceNumericInt64
    "UInt8" -> Just SurfaceNumericUInt8
    "UInt16" -> Just SurfaceNumericUInt16
    "UInt32" -> Just SurfaceNumericUInt32
    "UInt64" -> Just SurfaceNumericUInt64
    "Float16" -> Just SurfaceNumericFloat16
    "Float32" -> Just SurfaceNumericFloat32
    "Float64" -> Just SurfaceNumericFloat64
    _ -> Nothing

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
