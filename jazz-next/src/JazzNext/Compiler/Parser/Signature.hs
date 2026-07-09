{-# LANGUAGE OverloadedStrings #-}

-- | Signature grammar helpers for the surface parser.
module JazzNext.Compiler.Parser.Signature
  ( parseConstrainedSignatureType,
    parseSignatureTypePrefix,
    parseSignaturePayload,
    splitTopLevelCommaTokens
  ) where

import Control.Applicative ((<|>))
import Data.Text (Text)
import qualified Data.Text as Text
import JazzNext.Compiler.Identifier
  ( mkIdentifier
  )
import JazzNext.Compiler.Parser.AST
  ( SurfaceConstrainedSignatureType (..),
    SurfaceNumericType (..),
    SurfaceSignatureConstraint (..),
    SurfaceSignaturePayload (..),
    SurfaceSignatureToken (..),
    SurfaceSignatureType (..)
  )
import JazzNext.Compiler.Parser.Lexer
  ( Token (..),
    TokenKind (..)
  )
import qualified JazzNext.Compiler.Parser.TokenParser as TokenParser
import qualified Text.Megaparsec as MP

parseSignaturePayload :: [Token] -> SurfaceSignaturePayload
parseSignaturePayload signatureTokens =
  case parseSupportedSignaturePayload signatureTokens of
    Just signaturePayload -> signaturePayload
    Nothing -> SurfaceUnsupportedSignature (map surfaceSignatureTokenFromToken signatureTokens)

parseSupportedSignaturePayload :: [Token] -> Maybe SurfaceSignaturePayload
parseSupportedSignaturePayload =
  parseTokenStreamMaybe "signature payload" signaturePayloadParser

parseConstrainedSignatureType :: [Token] -> Maybe SurfaceConstrainedSignatureType
parseConstrainedSignatureType =
  parseTokenStreamMaybe "constrained signature type" constrainedSignatureTypeParser

parseSignatureTypePrefix :: [Token] -> Maybe (SurfaceSignatureType, [Token])
parseSignatureTypePrefix =
  parseTokenStreamPrefixMaybe "signature type" signatureTypeParser

splitTopLevelCommaTokens :: [Token] -> Maybe [[Token]]
splitTopLevelCommaTokens =
  parseTokenStreamMaybe "top-level comma list" topLevelCommaTokensParser

parseTokenStreamMaybe :: Text -> TokenParser.Parser a -> [Token] -> Maybe a
parseTokenStreamMaybe label parser tokens =
  case TokenParser.runTokenParser label parser tokens of
    Right value -> Just value
    Left _ -> Nothing

parseTokenStreamPrefixMaybe :: Text -> TokenParser.Parser a -> [Token] -> Maybe (a, [Token])
parseTokenStreamPrefixMaybe label parser tokens =
  case MP.runParser ((,) <$> parser <*> MP.getInput) (Text.unpack label) tokens of
    Right value -> Just value
    Left _ -> Nothing

signaturePayloadParser :: TokenParser.Parser SurfaceSignaturePayload
signaturePayloadParser =
  constrainedSignaturePayloadParser
    <|> (surfaceSignaturePayloadFromType <$> signatureTypeParser)

constrainedSignaturePayloadParser :: TokenParser.Parser SurfaceSignaturePayload
constrainedSignaturePayloadParser = do
  _ <- TokenParser.parseTokenKind TAt
  _ <- TokenParser.parseTokenKind TLBrace
  constraints <- constraintBlockParser
  _ <- TokenParser.parseTokenKind TRBrace
  _ <- TokenParser.parseTokenKind TColon
  SurfaceConstrainedSignature constraints <$> constrainedSignatureTypeParser

constraintBlockParser :: TokenParser.Parser [SurfaceSignatureConstraint]
constraintBlockParser =
  emptyConstraintBlockParser
    <|> signatureConstraintParser `MP.sepBy1` commaParser
  where
    emptyConstraintBlockParser =
      MP.lookAhead (TokenParser.parseTokenKind TRBrace) *> pure []

signatureConstraintParser :: TokenParser.Parser SurfaceSignatureConstraint
signatureConstraintParser = do
  signatureType <- constrainedSignatureTypeParser
  case signatureType of
    SurfaceConstrainedTypeApplication constraintName arguments ->
      pure (SurfaceSignatureConstraint constraintName arguments)
    SurfaceConstrainedTypeName constraintName ->
      pure (SurfaceSignatureConstraint constraintName [])
    _ ->
      MP.empty

constrainedSignatureTypeParser :: TokenParser.Parser SurfaceConstrainedSignatureType
constrainedSignatureTypeParser = do
  argumentType <- constrainedFunctionOperandTypeParser
  parseConstrainedFunctionResult argumentType <|> pure argumentType

parseConstrainedFunctionResult ::
  SurfaceConstrainedSignatureType ->
  TokenParser.Parser SurfaceConstrainedSignatureType
parseConstrainedFunctionResult argumentType = do
  _ <- TokenParser.parseTokenKind TArrow
  SurfaceConstrainedTypeFunction argumentType <$> constrainedSignatureTypeParser

constrainedFunctionOperandTypeParser :: TokenParser.Parser SurfaceConstrainedSignatureType
constrainedFunctionOperandTypeParser =
  MP.try constrainedTypeApplicationParser
    <|> constrainedTypeNameParser
    <|> constrainedListTypeParser
    <|> constrainedParenthesizedTypeParser

constrainedTypeApplicationParser :: TokenParser.Parser SurfaceConstrainedSignatureType
constrainedTypeApplicationParser = do
  typeName <- mkIdentifier <$> TokenParser.parseIdentifier
  arguments <-
    betweenTokenKinds TLParen TRParen
      (constrainedSignatureTypeParser `MP.sepBy1` commaParser)
  pure (SurfaceConstrainedTypeApplication typeName arguments)

constrainedTypeNameParser :: TokenParser.Parser SurfaceConstrainedSignatureType
constrainedTypeNameParser =
  SurfaceConstrainedTypeName . mkIdentifier <$> TokenParser.parseIdentifier

constrainedListTypeParser :: TokenParser.Parser SurfaceConstrainedSignatureType
constrainedListTypeParser =
  SurfaceConstrainedTypeList
    <$> betweenTokenKinds TLBracket TRBracket constrainedSignatureTypeParser

constrainedParenthesizedTypeParser :: TokenParser.Parser SurfaceConstrainedSignatureType
constrainedParenthesizedTypeParser =
  betweenTokenKinds TLParen TRParen $ do
    firstElement <- constrainedSignatureTypeParser
    remainingElements <- MP.many (commaParser *> constrainedSignatureTypeParser)
    case remainingElements of
      [] ->
        pure firstElement
      _ ->
        pure (SurfaceConstrainedTypeTuple (firstElement : remainingElements))

signatureTypeParser :: TokenParser.Parser SurfaceSignatureType
signatureTypeParser = do
  argumentType <- functionOperandTypeParser
  parseFunctionResult argumentType <|> pure argumentType

parseFunctionResult :: SurfaceSignatureType -> TokenParser.Parser SurfaceSignatureType
parseFunctionResult argumentType = do
  _ <- TokenParser.parseTokenKind TArrow
  SurfaceTypeFunction argumentType <$> signatureTypeParser

functionOperandTypeParser :: TokenParser.Parser SurfaceSignatureType
functionOperandTypeParser =
  namedSignatureTypeParser
    <|> listSignatureTypeParser
    <|> parenthesizedSignatureTypeParser

nonFunctionSignatureTypeParser :: TokenParser.Parser SurfaceSignatureType
nonFunctionSignatureTypeParser =
  namedSignatureTypeParser
    <|> listSignatureTypeParser
    <|> parenthesizedSignatureTypeParser

listSignatureTypeParser :: TokenParser.Parser SurfaceSignatureType
listSignatureTypeParser =
  SurfaceTypeList
    <$> betweenTokenKinds TLBracket TRBracket nonFunctionSignatureTypeParser

parenthesizedSignatureTypeParser :: TokenParser.Parser SurfaceSignatureType
parenthesizedSignatureTypeParser =
  betweenTokenKinds TLParen TRParen $ do
    firstElement <- signatureTypeParser
    remainingElements <- MP.many (commaParser *> signatureTypeParser)
    case remainingElements of
      [] ->
        pure firstElement
      _ ->
        pure (SurfaceTypeTuple (firstElement : remainingElements))

namedSignatureTypeParser :: TokenParser.Parser SurfaceSignatureType
namedSignatureTypeParser = do
  typeName <- TokenParser.parseIdentifier
  case parseNamedSignatureType typeName of
    Just signatureType ->
      pure signatureType
    Nothing ->
      MP.empty

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

surfaceSignaturePayloadFromType :: SurfaceSignatureType -> SurfaceSignaturePayload
surfaceSignaturePayloadFromType = SurfaceSignatureType

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
