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
    signatureTypeToConstraintSignatureType,
    substituteClassMethodSignature,
    constraintFunctionArgumentTypes,
    renderConstraintSignatureType
  ) where

import Data.Char (isLower)
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as Text
import JazzNext.Compiler.AST
  ( ConstraintSignatureType (..),
    SignaturePayload (..),
    SignatureToken (..),
    SignatureType (..)
  )
import JazzNext.Compiler.BuiltinCatalog
  ( renderNumericTypeName
  )
import JazzNext.Compiler.Name
  ( Name,
    renderName
  )

concreteImplFactKey :: Name -> [ConstraintSignatureType] -> Maybe Text
concreteImplFactKey capabilityName arguments =
  case arguments of
    [argument]
      | concreteConstraintArgument argument ->
          Just (constraintImplFactKey capabilityName argument)
    _ -> Nothing

constraintImplFactKey :: Name -> ConstraintSignatureType -> Text
constraintImplFactKey constraintName argument =
  renderName constraintName <> "(" <> renderConstraintSignatureType argument <> ")"

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
    <$> signaturePayloadConstraintType methodSignature

signaturePayloadConstraintType :: SignaturePayload -> Maybe ConstraintSignatureType
signaturePayloadConstraintType methodSignature =
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
      ConstraintTypeName "Int"
    TypeFloat ->
      ConstraintTypeName "Float"
    TypeNumeric numericType ->
      ConstraintTypeName (fromString (Text.unpack (renderNumericTypeName numericType)))
    TypeBool ->
      ConstraintTypeName "Bool"
    TypeList innerType ->
      ConstraintTypeList (signatureTypeToConstraintSignatureType innerType)
    TypeTuple elementTypes ->
      ConstraintTypeTuple (map signatureTypeToConstraintSignatureType elementTypes)
    TypeFunction argumentType resultType ->
      ConstraintTypeFunction
        (signatureTypeToConstraintSignatureType argumentType)
        (signatureTypeToConstraintSignatureType resultType)

unsupportedSignatureTokensToConstraintType :: [SignatureToken] -> Maybe ConstraintSignatureType
unsupportedSignatureTokensToConstraintType tokens =
  case splitTopLevelArrow tokens of
    Nothing ->
      unsupportedSignatureAtomToConstraintType tokens
    Just (argumentTokens, resultTokens) ->
      ConstraintTypeFunction
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

unsupportedSignatureAtomToConstraintType :: [SignatureToken] -> Maybe ConstraintSignatureType
unsupportedSignatureAtomToConstraintType tokens =
  case tokens of
    [SignatureNameToken typeName] ->
      Just (ConstraintTypeName typeName)
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
      | renderName name == classParameter -> implTarget
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

constraintSignatureTypeContainsClassParameter :: Text -> ConstraintSignatureType -> Bool
constraintSignatureTypeContainsClassParameter classParameter signatureType =
  case signatureType of
    ConstraintTypeApplication _ arguments ->
      any (constraintSignatureTypeContainsClassParameter classParameter) arguments
    ConstraintTypeList innerType ->
      constraintSignatureTypeContainsClassParameter classParameter innerType
    ConstraintTypeTuple elementTypes ->
      any (constraintSignatureTypeContainsClassParameter classParameter) elementTypes
    ConstraintTypeFunction argumentType resultType ->
      constraintSignatureTypeContainsClassParameter classParameter argumentType
        || constraintSignatureTypeContainsClassParameter classParameter resultType
    ConstraintTypeName typeName ->
      renderName typeName == classParameter

constraintSignatureTypesCompatible :: ConstraintSignatureType -> ConstraintSignatureType -> Bool
constraintSignatureTypesCompatible leftType rightType =
  case (leftType, rightType) of
    (ConstraintTypeName leftName, ConstraintTypeName rightName) ->
      normalizeConstraintSignatureName (renderName leftName)
        == normalizeConstraintSignatureName (renderName rightName)
    (ConstraintTypeApplication leftName leftArguments, ConstraintTypeApplication rightName rightArguments)
      | normalizeConstraintSignatureName (renderName leftName)
          == normalizeConstraintSignatureName (renderName rightName),
        length leftArguments == length rightArguments ->
          and (zipWith constraintSignatureTypesCompatible leftArguments rightArguments)
    (ConstraintTypeList leftElementType, ConstraintTypeList rightElementType) ->
      constraintSignatureTypesCompatible leftElementType rightElementType
    (ConstraintTypeTuple leftElementTypes, ConstraintTypeTuple rightElementTypes)
      | length leftElementTypes == length rightElementTypes ->
          and (zipWith constraintSignatureTypesCompatible leftElementTypes rightElementTypes)
    (ConstraintTypeFunction leftArgumentType leftResultType, ConstraintTypeFunction rightArgumentType rightResultType) ->
      constraintSignatureTypesCompatible leftArgumentType rightArgumentType
        && constraintSignatureTypesCompatible leftResultType rightResultType
    _ -> False

normalizeConstraintSignatureName :: Text -> Text
normalizeConstraintSignatureName typeName =
  case typeName of
    "Int" -> "Int64"
    "Float" -> "Float64"
    _ -> typeName

constraintSignatureAliasVariants :: ConstraintSignatureType -> [ConstraintSignatureType]
constraintSignatureAliasVariants signatureType =
  case signatureType of
    ConstraintTypeName name ->
      map ConstraintTypeName (constraintSignatureAliasNames name)
    ConstraintTypeApplication name arguments ->
      [ ConstraintTypeApplication name variantArguments
        | variantArguments <- traverse constraintSignatureAliasVariants arguments
      ]
    ConstraintTypeList elementType ->
      map ConstraintTypeList (constraintSignatureAliasVariants elementType)
    ConstraintTypeTuple elementTypes ->
      map ConstraintTypeTuple (traverse constraintSignatureAliasVariants elementTypes)
    ConstraintTypeFunction argumentType resultType ->
      [ ConstraintTypeFunction variantArgument variantResult
        | variantArgument <- constraintSignatureAliasVariants argumentType,
          variantResult <- constraintSignatureAliasVariants resultType
      ]

constraintSignatureAliasNames :: Name -> [Name]
constraintSignatureAliasNames name =
  case renderName name of
    "Int" -> ["Int", "Int64"]
    "Int64" -> ["Int64", "Int"]
    "Float" -> ["Float", "Float64"]
    "Float64" -> ["Float64", "Float"]
    _ -> [name]

renderConstraintSignatureType :: ConstraintSignatureType -> Text
renderConstraintSignatureType signatureType =
  case signatureType of
    ConstraintTypeName name ->
      renderName name
    ConstraintTypeApplication name arguments ->
      renderName name
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

identifierLooksLikeTypeVariable :: Name -> Bool
identifierLooksLikeTypeVariable name =
  case Text.uncons (renderName name) of
    Just (firstChar, _) -> isLower firstChar
    Nothing -> False

constraintSignatureTypeVariableNamesInOrder :: ConstraintSignatureType -> [Text]
constraintSignatureTypeVariableNamesInOrder =
  dedupe . go
  where
    go signatureType =
      case signatureType of
        ConstraintTypeName name
          | identifierLooksLikeTypeVariable name ->
              [renderName name]
          | otherwise ->
              []
        ConstraintTypeApplication _ arguments ->
          concatMap go arguments
        ConstraintTypeList innerType ->
          go innerType
        ConstraintTypeTuple elementTypes ->
          concatMap go elementTypes
        ConstraintTypeFunction argumentType resultType ->
          go argumentType ++ go resultType

    dedupe =
      goDedupe []

    goDedupe _ [] = []
    goDedupe seen (name : rest)
      | name `elem` seen = goDedupe seen rest
      | otherwise = name : goDedupe (name : seen) rest
