{-# LANGUAGE OverloadedStrings #-}

-- | Signature grammar helpers for the surface parser.
module JazzNext.Compiler.Parser.Signature
  ( parseConstrainedSignatureType,
    parseSignaturePayload,
    splitTopLevelCommaTokens
  ) where

import Control.Applicative ((<|>))
import Data.Text (Text)
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
                  parseConstrainedTupleSignatureType innerTokens
                    <|> parseConstrainedSignatureType innerTokens
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

parseConstrainedTupleSignatureType :: [Token] -> Maybe SurfaceConstrainedSignatureType
parseConstrainedTupleSignatureType signatureTokens =
  case splitTopLevelCommaTokens signatureTokens of
    Just elementTokenGroups
      | length elementTokenGroups >= 2 ->
          SurfaceConstrainedTypeTuple <$> traverse parseConstrainedSignatureType elementTokenGroups
    _ -> Nothing

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
    [Token {tokenKind = TIdentifier typeName}] ->
      parseNamedSignatureType typeName
    _ ->
      case stripWrappedSignatureTokens isLBracketToken isRBracketToken signatureTokens of
        Just innerTokens ->
          SurfaceTypeList <$> parseNonFunctionSignatureType innerTokens
        Nothing ->
          case stripWrappedSignatureTokens isLParenToken isRParenToken signatureTokens of
            Just innerTokens ->
              parseTupleSignatureType innerTokens
                <|> parseSupportedSignatureType innerTokens
            Nothing ->
              Nothing

parseNonFunctionSignatureType :: [Token] -> Maybe SurfaceSignatureType
parseNonFunctionSignatureType signatureTokens =
  case signatureTokens of
    [Token {tokenKind = TIdentifier typeName}] ->
      parseNamedSignatureType typeName
    _ ->
      case stripWrappedSignatureTokens isLBracketToken isRBracketToken signatureTokens of
        Just innerTokens ->
          SurfaceTypeList <$> parseNonFunctionSignatureType innerTokens
        Nothing ->
          case stripWrappedSignatureTokens isLParenToken isRParenToken signatureTokens of
            Just innerTokens ->
              parseTupleSignatureType innerTokens
                <|> parseSupportedSignatureType innerTokens
            Nothing ->
              Nothing

parseTupleSignatureType :: [Token] -> Maybe SurfaceSignatureType
parseTupleSignatureType signatureTokens =
  case splitTopLevelCommaTokens signatureTokens of
    Just elementTokenGroups
      | length elementTokenGroups >= 2 ->
          SurfaceTypeTuple <$> traverse parseSupportedSignatureType elementTokenGroups
    _ -> Nothing

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

