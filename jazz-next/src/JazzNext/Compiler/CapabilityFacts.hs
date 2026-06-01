{-# LANGUAGE OverloadedStrings #-}

-- | Shared helpers for the first class/impl environment-validation slice.
module JazzNext.Compiler.CapabilityFacts
  ( concreteConstraintArgument,
    concreteImplFactKey,
    constraintImplFactKey,
    identifierLooksLikeTypeVariable,
    renderConstraintSignatureType
  ) where

import Data.Char (isLower)
import Data.Text (Text)
import qualified Data.Text as Text
import JazzNext.Compiler.AST
  ( ConstraintSignatureType (..)
  )
import JazzNext.Compiler.Identifier
  ( Identifier,
    identifierText
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
