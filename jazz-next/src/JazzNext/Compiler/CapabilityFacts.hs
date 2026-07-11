{-# LANGUAGE OverloadedStrings #-}

-- | Shared helpers for the first class/impl environment-validation slice.
module JazzNext.Compiler.CapabilityFacts
  ( concreteConstraintArgument,
    concreteImplFactKey,
    concreteImplFactClassName,
    constraintImplFactKey,
    constraintSignatureAliasNames,
    constraintSignatureAliasVariants,
    constraintSignatureTypeContainsClassParameter,
    constraintSignatureTypeVariableNamesInOrder,
    constraintSignatureTypesCompatible,
    identifierLooksLikeTypeVariable,
    normalizeConstraintSignatureName,
    qualifiedMethodKey,
    splitQualifiedMethodKey,
    signaturePayloadConstraintType,
    substituteClassMethodSignature,
    constraintFunctionArgumentTypes,
    renderCapabilityType
  ) where

import Data.Char (isLower)
import Data.Text (Text)
import qualified Data.Text as Text
import JazzNext.Compiler.AST
  ( NumericType (..),
    SignaturePayload (..),
    SignatureToken (..),
    SignatureType (..)
  )
import JazzNext.Compiler.BuiltinCatalog
  ( numericTypeFromName,
    renderNumericTypeName
  )
import JazzNext.Compiler.Name
  ( Name,
    renderName
  )

concreteImplFactKey :: Name -> [SignatureType] -> Maybe Text
concreteImplFactKey capabilityName arguments =
  case arguments of
    [argument]
      | concreteConstraintArgument argument ->
          Just (constraintImplFactKey capabilityName argument)
    _ -> Nothing

constraintImplFactKey :: Name -> SignatureType -> Text
constraintImplFactKey constraintName argument =
  renderName constraintName <> "(" <> renderCapabilityType argument <> ")"

concreteImplFactClassName :: Text -> Text
concreteImplFactClassName implKey =
  fst (Text.breakOn "(" implKey)

qualifiedMethodKey :: Name -> Name -> Text
qualifiedMethodKey capabilityName methodName =
  renderName capabilityName <> "::" <> renderName methodName

splitQualifiedMethodKey :: Text -> Maybe (Text, Text)
splitQualifiedMethodKey nameText =
  case Text.breakOnEnd "::" nameText of
    (capabilityNameWithSeparator, methodName)
      | not (Text.null capabilityName),
        not (Text.null methodName) ->
          Just (capabilityName, methodName)
      where
        capabilityName = Text.dropEnd 2 capabilityNameWithSeparator
    _ -> Nothing

concreteConstraintArgument :: SignatureType -> Bool
concreteConstraintArgument signatureType =
  case signatureType of
    TypeVariable {} -> False
    TypeName name ->
      not (identifierLooksLikeTypeVariable name)
    TypeApplication name arguments ->
      not (identifierLooksLikeTypeVariable name) && all concreteConstraintArgument arguments
    TypeList innerType ->
      concreteConstraintArgument innerType
    TypeTuple elementTypes ->
      all concreteConstraintArgument elementTypes
    TypeFunction {} ->
      False
    _ -> True

substituteClassMethodSignature :: Text -> SignatureType -> SignaturePayload -> Maybe SignatureType
substituteClassMethodSignature classParameter implTarget methodSignature =
  substituteSignatureType classParameter implTarget
    <$> signaturePayloadConstraintType methodSignature

signaturePayloadConstraintType :: SignaturePayload -> Maybe SignatureType
signaturePayloadConstraintType methodSignature =
  case methodSignature of
    SignatureType signatureType ->
      Just signatureType
    ConstrainedSignature [] signatureType ->
      Just signatureType
    ConstrainedSignature {} ->
      Nothing
    UnsupportedSignature signatureTokens ->
      unsupportedSignatureTokensToConstraintType signatureTokens

unsupportedSignatureTokensToConstraintType :: [SignatureToken] -> Maybe SignatureType
unsupportedSignatureTokensToConstraintType tokens =
  case splitTopLevelArrow tokens of
    Nothing ->
      unsupportedSignatureAtomToConstraintType tokens
    Just (argumentTokens, resultTokens) ->
      TypeFunction
        <$> unsupportedSignatureAtomToConstraintType argumentTokens
        <*> unsupportedSignatureTokensToConstraintType resultTokens

splitTopLevelArrow :: [SignatureToken] -> Maybe ([SignatureToken], [SignatureToken])
splitTopLevelArrow =
  go 0 0 0 []
  where
    go _ _ _ _ [] = Nothing
    go parenDepth bracketDepth braceDepth argumentTokens (token : rest) =
      case token of
        SignatureArrowToken
          | parenDepth == 0 && bracketDepth == 0 && braceDepth == 0 ->
              Just (reverse argumentTokens, rest)
        SignatureLParenToken ->
          go (parenDepth + 1) bracketDepth braceDepth (token : argumentTokens) rest
        SignatureRParenToken ->
          go (parenDepth - 1) bracketDepth braceDepth (token : argumentTokens) rest
        SignatureLBracketToken ->
          go parenDepth (bracketDepth + 1) braceDepth (token : argumentTokens) rest
        SignatureRBracketToken ->
          go parenDepth (bracketDepth - 1) braceDepth (token : argumentTokens) rest
        SignatureLBraceToken ->
          go parenDepth bracketDepth (braceDepth + 1) (token : argumentTokens) rest
        SignatureRBraceToken ->
          go parenDepth bracketDepth (braceDepth - 1) (token : argumentTokens) rest
        _ ->
          go parenDepth bracketDepth braceDepth (token : argumentTokens) rest

unsupportedSignatureAtomToConstraintType :: [SignatureToken] -> Maybe SignatureType
unsupportedSignatureAtomToConstraintType tokens =
  case tokens of
    [SignatureNameToken typeName] ->
      Just (signatureTypeForName typeName)
    SignatureLParenToken : rest ->
      case reverse rest of
        SignatureRParenToken : reversedInnerTokens ->
          unsupportedSignatureTokensToConstraintType (reverse reversedInnerTokens)
        _ -> Nothing
    SignatureLBracketToken : rest ->
      case reverse rest of
        SignatureRBracketToken : reversedInnerTokens ->
          TypeList <$> unsupportedSignatureTokensToConstraintType (reverse reversedInnerTokens)
        _ -> Nothing
    _ -> Nothing

signatureTypeForName :: Name -> SignatureType
signatureTypeForName name =
  case renderName name of
    "Int" -> TypeInt
    "Float" -> TypeFloat
    "Bool" -> TypeBool
    "Char" -> TypeChar
    "Text" -> TypeText
    typeName ->
      case numericTypeFromName typeName of
        Just numericType -> TypeNumeric numericType
        Nothing
          | identifierLooksLikeTypeVariable name -> TypeVariable name
          | otherwise -> TypeName name

substituteSignatureType :: Text -> SignatureType -> SignatureType -> SignatureType
substituteSignatureType classParameter implTarget signatureType =
  case signatureType of
    TypeVariable name
      | renderName name == classParameter -> implTarget
      | otherwise -> signatureType
    TypeName name
      | renderName name == classParameter -> implTarget
      | otherwise -> signatureType
    TypeApplication name arguments ->
      TypeApplication name (map (substituteSignatureType classParameter implTarget) arguments)
    TypeList innerType ->
      TypeList (substituteSignatureType classParameter implTarget innerType)
    TypeTuple elementTypes ->
      TypeTuple (map (substituteSignatureType classParameter implTarget) elementTypes)
    TypeFunction argumentType resultType ->
      TypeFunction
        (substituteSignatureType classParameter implTarget argumentType)
        (substituteSignatureType classParameter implTarget resultType)
    _ -> signatureType

constraintFunctionArgumentTypes :: SignatureType -> ([SignatureType], SignatureType)
constraintFunctionArgumentTypes signatureType =
  case signatureType of
    TypeFunction argumentType resultType ->
      let (argumentTypes, finalResultType) = constraintFunctionArgumentTypes resultType
       in (argumentType : argumentTypes, finalResultType)
    _ ->
      ([], signatureType)

constraintSignatureTypeContainsClassParameter :: Text -> SignatureType -> Bool
constraintSignatureTypeContainsClassParameter classParameter signatureType =
  case signatureType of
    TypeApplication _ arguments ->
      any (constraintSignatureTypeContainsClassParameter classParameter) arguments
    TypeList innerType ->
      constraintSignatureTypeContainsClassParameter classParameter innerType
    TypeTuple elementTypes ->
      any (constraintSignatureTypeContainsClassParameter classParameter) elementTypes
    TypeFunction argumentType resultType ->
      constraintSignatureTypeContainsClassParameter classParameter argumentType
        || constraintSignatureTypeContainsClassParameter classParameter resultType
    TypeVariable typeName ->
      renderName typeName == classParameter
    TypeName typeName ->
      renderName typeName == classParameter
    _ -> False

constraintSignatureTypesCompatible :: SignatureType -> SignatureType -> Bool
constraintSignatureTypesCompatible leftType rightType =
  case (leftType, rightType) of
    _ | leftType == rightType -> True
    (TypeInt, TypeNumeric NumericInt64) -> True
    (TypeNumeric NumericInt64, TypeInt) -> True
    (TypeFloat, TypeNumeric NumericFloat64) -> True
    (TypeNumeric NumericFloat64, TypeFloat) -> True
    (TypeInt, TypeName name) -> normalizeConstraintSignatureName (renderName name) == "Int64"
    (TypeName name, TypeInt) -> normalizeConstraintSignatureName (renderName name) == "Int64"
    (TypeFloat, TypeName name) -> normalizeConstraintSignatureName (renderName name) == "Float64"
    (TypeName name, TypeFloat) -> normalizeConstraintSignatureName (renderName name) == "Float64"
    (TypeNumeric numericType, TypeName name) -> renderNumericTypeName numericType == normalizeConstraintSignatureName (renderName name)
    (TypeName name, TypeNumeric numericType) -> normalizeConstraintSignatureName (renderName name) == renderNumericTypeName numericType
    (TypeBool, TypeName name) -> renderName name == "Bool"
    (TypeName name, TypeBool) -> renderName name == "Bool"
    (TypeChar, TypeName name) -> renderName name == "Char"
    (TypeName name, TypeChar) -> renderName name == "Char"
    (TypeText, TypeName name) -> renderName name == "Text"
    (TypeName name, TypeText) -> renderName name == "Text"
    (TypeVariable leftName, TypeVariable rightName) -> renderName leftName == renderName rightName
    (TypeName leftName, TypeName rightName) ->
      normalizeConstraintSignatureName (renderName leftName)
        == normalizeConstraintSignatureName (renderName rightName)
    (TypeApplication leftName leftArguments, TypeApplication rightName rightArguments)
      | normalizeConstraintSignatureName (renderName leftName)
          == normalizeConstraintSignatureName (renderName rightName),
        length leftArguments == length rightArguments ->
          and (zipWith constraintSignatureTypesCompatible leftArguments rightArguments)
    (TypeList leftElementType, TypeList rightElementType) ->
      constraintSignatureTypesCompatible leftElementType rightElementType
    (TypeTuple leftElementTypes, TypeTuple rightElementTypes)
      | length leftElementTypes == length rightElementTypes ->
          and (zipWith constraintSignatureTypesCompatible leftElementTypes rightElementTypes)
    (TypeFunction leftArgumentType leftResultType, TypeFunction rightArgumentType rightResultType) ->
      constraintSignatureTypesCompatible leftArgumentType rightArgumentType
        && constraintSignatureTypesCompatible leftResultType rightResultType
    _ -> False

normalizeConstraintSignatureName :: Text -> Text
normalizeConstraintSignatureName typeName =
  case typeName of
    "Int" -> "Int64"
    "Float" -> "Float64"
    _ -> typeName

constraintSignatureAliasVariants :: SignatureType -> [SignatureType]
constraintSignatureAliasVariants signatureType =
  case signatureType of
    TypeInt -> [TypeInt, TypeNumeric NumericInt64]
    TypeNumeric NumericInt64 -> [TypeNumeric NumericInt64, TypeInt]
    TypeFloat -> [TypeFloat, TypeNumeric NumericFloat64]
    TypeNumeric NumericFloat64 -> [TypeNumeric NumericFloat64, TypeFloat]
    TypeName name ->
      map TypeName (constraintSignatureAliasNames name)
    TypeApplication name arguments ->
      [ TypeApplication name variantArguments
        | variantArguments <- traverse constraintSignatureAliasVariants arguments
      ]
    TypeList elementType ->
      map TypeList (constraintSignatureAliasVariants elementType)
    TypeTuple elementTypes ->
      map TypeTuple (traverse constraintSignatureAliasVariants elementTypes)
    TypeFunction argumentType resultType ->
      [ TypeFunction variantArgument variantResult
        | variantArgument <- constraintSignatureAliasVariants argumentType,
          variantResult <- constraintSignatureAliasVariants resultType
      ]
    _ -> [signatureType]

constraintSignatureAliasNames :: Name -> [Name]
constraintSignatureAliasNames name =
  case renderName name of
    "Int" -> ["Int", "Int64"]
    "Int64" -> ["Int64", "Int"]
    "Float" -> ["Float", "Float64"]
    "Float64" -> ["Float64", "Float"]
    _ -> [name]

renderCapabilityType :: SignatureType -> Text
renderCapabilityType signatureType =
  case signatureType of
    TypeInt -> "Int"
    TypeFloat -> "Float"
    TypeNumeric numericType -> renderNumericTypeName numericType
    TypeBool -> "Bool"
    TypeChar -> "Char"
    TypeText -> "Text"
    TypeVariable name -> renderName name
    TypeName name ->
      renderName name
    TypeApplication name arguments ->
      renderName name
        <> "("
        <> Text.intercalate ", " (map renderCapabilityType arguments)
        <> ")"
    TypeList innerType ->
      "[" <> renderConstraintListElementType innerType <> "]"
    TypeTuple elementTypes ->
      "(" <> Text.intercalate ", " (map renderCapabilityType elementTypes) <> ")"
    TypeFunction argumentType resultType ->
      renderConstraintFunctionArgumentType argumentType <> " -> " <> renderCapabilityType resultType

renderConstraintFunctionArgumentType :: SignatureType -> Text
renderConstraintFunctionArgumentType signatureType =
  case signatureType of
    TypeFunction {} ->
      "(" <> renderCapabilityType signatureType <> ")"
    _ ->
      renderCapabilityType signatureType

renderConstraintListElementType :: SignatureType -> Text
renderConstraintListElementType signatureType =
  case signatureType of
    TypeFunction {} ->
      "(" <> renderCapabilityType signatureType <> ")"
    _ ->
      renderCapabilityType signatureType

identifierLooksLikeTypeVariable :: Name -> Bool
identifierLooksLikeTypeVariable name =
  case Text.uncons (renderName name) of
    Just (firstChar, _) -> isLower firstChar
    Nothing -> False

constraintSignatureTypeVariableNamesInOrder :: SignatureType -> [Text]
constraintSignatureTypeVariableNamesInOrder =
  dedupe . go
  where
    go signatureType =
      case signatureType of
        TypeVariable name -> [renderName name]
        TypeName name
          | identifierLooksLikeTypeVariable name ->
              [renderName name]
          | otherwise ->
              []
        TypeApplication _ arguments ->
          concatMap go arguments
        TypeList innerType ->
          go innerType
        TypeTuple elementTypes ->
          concatMap go elementTypes
        TypeFunction argumentType resultType ->
          go argumentType ++ go resultType
        _ -> []

    dedupe =
      goDedupe []

    goDedupe _ [] = []
    goDedupe seen (name : rest)
      | name `elem` seen = goDedupe seen rest
      | otherwise = name : goDedupe (name : seen) rest
