{-# LANGUAGE OverloadedStrings #-}

-- | Shared helpers for the first class/impl environment-validation slice.
module JazzNext.Compiler.CapabilityFacts
  ( concreteConstraintArgument,
    concreteImplFactKey,
    constraintImplFactKey,
    identifierLooksLikeTypeVariable,
    qualifiedMethodKey,
    splitQualifiedMethodKey,
    substituteClassMethodSignature,
    constraintFunctionArgumentTypes,
    renderConstraintSignatureType
  ) where

import Data.Char (isLower)
import Data.Text (Text)
import qualified Data.Text as Text
import JazzNext.Compiler.AST
  ( ConstraintSignatureType (..),
    NumericType (..),
    SignaturePayload (..),
    SignatureToken (..),
    SignatureType (..)
  )
import JazzNext.Compiler.Identifier
  ( Identifier,
    identifierText,
    mkIdentifier
  )

concreteImplFactKey :: Identifier -> [ConstraintSignatureType] -> Maybe Text
concreteImplFactKey capabilityName arguments =
  case arguments of
    [argument]
      | concreteConstraintArgument argument ->
          Just (constraintImplFactKey capabilityName argument)
    _ -> Nothing

constraintImplFactKey :: Identifier -> ConstraintSignatureType -> Text
constraintImplFactKey constraintName argument =
  identifierText constraintName <> "(" <> renderConstraintSignatureType argument <> ")"

qualifiedMethodKey :: Identifier -> Identifier -> Text
qualifiedMethodKey capabilityName methodName =
  identifierText capabilityName <> "::" <> identifierText methodName

splitQualifiedMethodKey :: Text -> Maybe (Text, Text)
splitQualifiedMethodKey nameText =
  case Text.splitOn "::" nameText of
    [capabilityName, methodName]
      | not (Text.null capabilityName),
        not (Text.null methodName) ->
          Just (capabilityName, methodName)
    _ -> Nothing

concreteConstraintArgument :: ConstraintSignatureType -> Bool
concreteConstraintArgument signatureType =
  case signatureType of
    ConstraintTypeName name ->
      not (identifierLooksLikeTypeVariable name)
    ConstraintTypeApplication name arguments ->
      not (identifierLooksLikeTypeVariable name) && all concreteConstraintArgument arguments
    ConstraintTypeList innerType ->
      concreteConstraintArgument innerType
    ConstraintTypeTuple elementTypes ->
      all concreteConstraintArgument elementTypes
    ConstraintTypeFunction {} ->
      False

substituteClassMethodSignature :: Text -> ConstraintSignatureType -> SignaturePayload -> Maybe ConstraintSignatureType
substituteClassMethodSignature classParameter implTarget methodSignature =
  substituteConstraintSignatureType classParameter implTarget
    <$> classMethodPayloadConstraintType methodSignature

classMethodPayloadConstraintType :: SignaturePayload -> Maybe ConstraintSignatureType
classMethodPayloadConstraintType methodSignature =
  case methodSignature of
    SignatureType signatureType ->
      Just (signatureTypeToConstraintSignatureType signatureType)
    ConstrainedSignature [] signatureType ->
      Just signatureType
    ConstrainedSignature {} ->
      Nothing
    UnsupportedSignature signatureTokens ->
      unsupportedSignatureTokensToConstraintType signatureTokens

signatureTypeToConstraintSignatureType :: SignatureType -> ConstraintSignatureType
signatureTypeToConstraintSignatureType signatureType =
  case signatureType of
    TypeInt ->
      ConstraintTypeName (mkIdentifier "Int")
    TypeFloat ->
      ConstraintTypeName (mkIdentifier "Float")
    TypeNumeric numericType ->
      ConstraintTypeName (mkIdentifier (numericSignatureTypeName numericType))
    TypeBool ->
      ConstraintTypeName (mkIdentifier "Bool")
    TypeList innerType ->
      ConstraintTypeList (signatureTypeToConstraintSignatureType innerType)
    TypeTuple elementTypes ->
      ConstraintTypeTuple (map signatureTypeToConstraintSignatureType elementTypes)
    TypeFunction argumentType resultType ->
      ConstraintTypeFunction
        (signatureTypeToConstraintSignatureType argumentType)
        (signatureTypeToConstraintSignatureType resultType)

numericSignatureTypeName :: NumericType -> Text
numericSignatureTypeName numericType =
  case numericType of
    NumericInt8 -> "Int8"
    NumericInt16 -> "Int16"
    NumericInt32 -> "Int32"
    NumericInt64 -> "Int64"
    NumericUInt8 -> "UInt8"
    NumericUInt16 -> "UInt16"
    NumericUInt32 -> "UInt32"
    NumericUInt64 -> "UInt64"
    NumericFloat16 -> "Float16"
    NumericFloat32 -> "Float32"
    NumericFloat64 -> "Float64"

unsupportedSignatureTokensToConstraintType :: [SignatureToken] -> Maybe ConstraintSignatureType
unsupportedSignatureTokensToConstraintType tokens =
  case break (== SignatureArrowToken) tokens of
    (_, []) ->
      unsupportedSignatureAtomToConstraintType tokens
    (argumentTokens, SignatureArrowToken : resultTokens) ->
      ConstraintTypeFunction
        <$> unsupportedSignatureAtomToConstraintType argumentTokens
        <*> unsupportedSignatureTokensToConstraintType resultTokens
    _ -> Nothing

unsupportedSignatureAtomToConstraintType :: [SignatureToken] -> Maybe ConstraintSignatureType
unsupportedSignatureAtomToConstraintType tokens =
  case tokens of
    [SignatureNameToken typeName] ->
      Just (ConstraintTypeName (mkIdentifier typeName))
    SignatureLParenToken : rest ->
      case reverse rest of
        SignatureRParenToken : reversedInnerTokens ->
          unsupportedSignatureTokensToConstraintType (reverse reversedInnerTokens)
        _ -> Nothing
    SignatureLBracketToken : rest ->
      case reverse rest of
        SignatureRBracketToken : reversedInnerTokens ->
          ConstraintTypeList <$> unsupportedSignatureTokensToConstraintType (reverse reversedInnerTokens)
        _ -> Nothing
    _ -> Nothing

substituteConstraintSignatureType :: Text -> ConstraintSignatureType -> ConstraintSignatureType -> ConstraintSignatureType
substituteConstraintSignatureType classParameter implTarget signatureType =
  case signatureType of
    ConstraintTypeName name
      | identifierText name == classParameter -> implTarget
      | otherwise -> signatureType
    ConstraintTypeApplication name arguments ->
      ConstraintTypeApplication name (map (substituteConstraintSignatureType classParameter implTarget) arguments)
    ConstraintTypeList innerType ->
      ConstraintTypeList (substituteConstraintSignatureType classParameter implTarget innerType)
    ConstraintTypeTuple elementTypes ->
      ConstraintTypeTuple (map (substituteConstraintSignatureType classParameter implTarget) elementTypes)
    ConstraintTypeFunction argumentType resultType ->
      ConstraintTypeFunction
        (substituteConstraintSignatureType classParameter implTarget argumentType)
        (substituteConstraintSignatureType classParameter implTarget resultType)

constraintFunctionArgumentTypes :: ConstraintSignatureType -> ([ConstraintSignatureType], ConstraintSignatureType)
constraintFunctionArgumentTypes signatureType =
  case signatureType of
    ConstraintTypeFunction argumentType resultType ->
      let (argumentTypes, finalResultType) = constraintFunctionArgumentTypes resultType
       in (argumentType : argumentTypes, finalResultType)
    _ ->
      ([], signatureType)

renderConstraintSignatureType :: ConstraintSignatureType -> Text
renderConstraintSignatureType signatureType =
  case signatureType of
    ConstraintTypeName name ->
      identifierText name
    ConstraintTypeApplication name arguments ->
      identifierText name
        <> "("
        <> Text.intercalate ", " (map renderConstraintSignatureType arguments)
        <> ")"
    ConstraintTypeList innerType ->
      "[" <> renderConstraintListElementType innerType <> "]"
    ConstraintTypeTuple elementTypes ->
      "(" <> Text.intercalate ", " (map renderConstraintSignatureType elementTypes) <> ")"
    ConstraintTypeFunction argumentType resultType ->
      renderConstraintFunctionArgumentType argumentType <> " -> " <> renderConstraintSignatureType resultType

renderConstraintFunctionArgumentType :: ConstraintSignatureType -> Text
renderConstraintFunctionArgumentType signatureType =
  case signatureType of
    ConstraintTypeFunction {} ->
      "(" <> renderConstraintSignatureType signatureType <> ")"
    _ ->
      renderConstraintSignatureType signatureType

renderConstraintListElementType :: ConstraintSignatureType -> Text
renderConstraintListElementType signatureType =
  case signatureType of
    ConstraintTypeFunction {} ->
      "(" <> renderConstraintSignatureType signatureType <> ")"
    _ ->
      renderConstraintSignatureType signatureType

identifierLooksLikeTypeVariable :: Identifier -> Bool
identifierLooksLikeTypeVariable name =
  case Text.uncons (identifierText name) of
    Just (firstChar, _) -> isLower firstChar
    Nothing -> False
