{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Shared helpers for the first class/impl environment-validation slice.
module Jazz.Compiler.CapabilityFacts
  ( ConcreteImplFact (..),
    concreteConstraintArgument,
    concreteImplFact,
    concreteImplFactClassName,
    constraintSignatureAliasNames,
    constraintSignatureAliasVariants,
    constraintSignatureTypeContainsClassParameter,
    constraintSignatureTypeVariableNamesInOrder,
    constraintSignatureTypesCompatible,
    identifierLooksLikeTypeVariable,
    normalizeConstraintSignatureName,
    qualifiedMethodKey,
    renderConcreteImplFact,
    splitQualifiedMethodKey,
    signaturePayloadConstraintType,
    substituteClassMethodSignature,
    constraintFunctionArgumentTypes,
  )
where

import Control.DeepSeq (NFData)
import Data.Char (isLower)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import qualified Jazz.Compiler.AST as AST
import Jazz.Compiler.BuiltinCatalog
  ( numericTypeFromName,
    renderNumericTypeName,
  )
import Jazz.Compiler.Name
  ( IdentifierLike (identifierText),
    Name (..),
    ResolvedName,
    ResolvedUserName (..),
    mkIdentifier,
    renderName,
  )
import Jazz.Compiler.SignatureRendering
  ( renderSignatureType,
  )
import Jazz.Compiler.TypeRepresentation
  ( NumericType (..),
    pattern ConstrainedSignature,
    pattern SignatureArrowToken,
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

type SignaturePayload = AST.SignaturePayload 'AST.Resolved

type SignatureToken = AST.SignatureToken 'AST.Resolved

type SignatureType = AST.SignatureType 'AST.Resolved

data ConcreteImplFact = ConcreteImplFact ResolvedName SignatureType
  deriving stock (Generic, Show)
  deriving anyclass (NFData)

-- | Concrete facts preserve the legacy text-key collision semantics: the
-- capability and complete argument compare by rendered identity, rather than
-- the implementation-specific 'ResolvedName' origins used to construct them. Keeping
-- the rendered argument also retains legacy collisions such as 'TypeInt' and
-- @TypeName "Int"@.
instance Eq ConcreteImplFact where
  leftFact == rightFact = concreteImplFactIdentity leftFact == concreteImplFactIdentity rightFact

instance Ord ConcreteImplFact where
  compare leftFact rightFact = compare (concreteImplFactIdentity leftFact) (concreteImplFactIdentity rightFact)

concreteImplFactIdentity :: ConcreteImplFact -> (Text, Text)
concreteImplFactIdentity (ConcreteImplFact capabilityName argument) =
  (renderName capabilityName, renderSignatureType argument)

concreteImplFact :: ResolvedName -> [SignatureType] -> Maybe ConcreteImplFact
concreteImplFact capabilityName arguments =
  case arguments of
    [argument]
      | concreteConstraintArgument argument ->
          Just (ConcreteImplFact capabilityName argument)
    _ -> Nothing

renderConcreteImplFact :: ConcreteImplFact -> Text
renderConcreteImplFact (ConcreteImplFact capabilityName argument) =
  renderName capabilityName <> "(" <> renderSignatureType argument <> ")"

concreteImplFactClassName :: ConcreteImplFact -> Text
concreteImplFactClassName (ConcreteImplFact capabilityName _) = renderName capabilityName

qualifiedMethodKey :: ResolvedName -> ResolvedName -> Text
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
    go :: Int -> Int -> Int -> [SignatureToken] -> [SignatureToken] -> Maybe ([SignatureToken], [SignatureToken])
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

signatureTypeForName :: ResolvedName -> SignatureType
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

constraintSignatureAliasNames :: ResolvedName -> [ResolvedName]
constraintSignatureAliasNames name =
  case renderName name of
    "Int" -> map (`renameResolved` name) ["Int", "Int64"]
    "Int64" -> map (`renameResolved` name) ["Int64", "Int"]
    "Float" -> map (`renameResolved` name) ["Float", "Float64"]
    "Float64" -> map (`renameResolved` name) ["Float64", "Float"]
    _ -> [name]

renameResolved :: Text -> ResolvedName -> ResolvedName
renameResolved replacement name =
  case name of
    UserName (ResolvedUserName origin namespace _) ->
      UserName (ResolvedUserName origin namespace (mkIdentifier replacement))
    BuiltinName _ -> BuiltinName (mkIdentifier replacement)
    GeneratedName {} -> name

identifierLooksLikeTypeVariable :: ResolvedName -> Bool
identifierLooksLikeTypeVariable name =
  case Text.uncons (terminalIdentifierText name) of
    Just (firstChar, _) -> isLower firstChar
    Nothing -> False
  where
    terminalIdentifierText candidate =
      case candidate of
        UserName (ResolvedUserName _ _ identifier) -> identifierText identifier
        BuiltinName identifier -> identifierText identifier
        GeneratedName {} -> ""

constraintSignatureTypeVariableNamesInOrder :: SignatureType -> [Text]
constraintSignatureTypeVariableNamesInOrder =
  stableUnique . go
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

    stableUnique = reverse . snd . foldl' keep (Set.empty, [])
      where
        keep (seen, reversedNames) name
          | Set.member name seen = (seen, reversedNames)
          | otherwise = (Set.insert name seen, name : reversedNames)
