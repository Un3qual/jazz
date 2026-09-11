{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Shared helpers for the first class/impl environment-validation slice.
module Jazz.Compiler.CapabilityFacts
  ( ConcreteImplFact (..),
    concreteConstraintArgument,
    concreteImplFact,
    concreteImplFactCapability,
    constraintSignatureTypeVariableNamesInOrder,
    identifierLooksLikeTypeVariable,
    qualifiedMethodKey,
    renderConcreteImplFact,
    splitQualifiedMethodKey,
    signaturePayloadConstraintType,
  )
where

import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Jazz.Compiler.AST as AST
import Jazz.Compiler.BuiltinCatalog (numericTypeFromName)
import Jazz.Compiler.CoreIdentity (CapabilityId (..), CapabilityMethodKey, renderCapabilityId)
import Jazz.Compiler.Name
  ( ResolvedName,
    identifierLooksLikeTypeVariable,
    mkIdentifier,
    renderName,
  )
import Jazz.Compiler.SemanticDeclarations (ConcreteImplFact (..), concreteSignatureType, implementationTargetSignature)
import Jazz.Compiler.SignatureRendering
  ( renderSignatureType,
  )
import Jazz.Compiler.TypeRepresentation
  ( pattern ConstrainedSignature,
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

concreteImplFact :: ResolvedName -> [SignatureType] -> Maybe ConcreteImplFact
concreteImplFact capabilityName arguments =
  case arguments of
    [argument] -> ConcreteImplFact (CapabilityId capabilityName) <$> concreteSignatureType argument
    _ -> Nothing

renderConcreteImplFact :: ConcreteImplFact -> Text
renderConcreteImplFact (ConcreteImplFact capabilityName argument) =
  renderCapabilityId capabilityName <> "(" <> renderSignatureType (implementationTargetSignature argument) <> ")"

concreteImplFactCapability :: ConcreteImplFact -> CapabilityId
concreteImplFactCapability (ConcreteImplFact capabilityName _) = capabilityName

qualifiedMethodKey :: ResolvedName -> ResolvedName -> CapabilityMethodKey
qualifiedMethodKey capabilityName methodName =
  (CapabilityId capabilityName, mkIdentifier (renderName methodName))

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
