{-# LANGUAGE OverloadedStrings #-}

-- | Canonical conversion between surface signature types and inference types.
module JazzNext.Compiler.TypeInference.Signature
  ( SignatureTypeFailure (..),
    expressionTypeToRuntimeTemplate,
    renderSignatureTypeFailure,
    signatureTypeToExpressionType,
    validateSignatureType
  ) where

import Control.Applicative ((<|>))
import Data.Functor (void)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import JazzNext.Compiler.AST (SignatureType (..))
import JazzNext.Compiler.BuiltinCatalog (numericTypeFromName)
import JazzNext.Compiler.CapabilityFacts
  ( constraintSignatureTypeVariableNamesInOrder,
    identifierLooksLikeTypeVariable
  )
import JazzNext.Compiler.Name (Name, identifierText)
import JazzNext.Compiler.TypeInference.State (InferState, inferDataTypes)
import JazzNext.Compiler.TypeInference.Types
  ( DataTypeBinding (..),
    ExpressionType (..)
  )

data SignatureTypeFailure
  = UnknownNamedType Name
  | NamedTypeArityMismatch Name Int Int
  | TypeVariableApplicationHead Name
  | UnboundSignatureTypeVariable Name
  deriving (Eq, Show)

signatureTypeToExpressionType ::
  InferState ->
  Map Text ExpressionType ->
  SignatureType ->
  Either SignatureTypeFailure ExpressionType
signatureTypeToExpressionType state =
  convertSignatureType (inferDataTypes state)

-- | Validate a declaration signature while treating its free variables as
-- universally quantified placeholders. Callers that require a concrete type
-- use 'signatureTypeToExpressionType' with an empty variable environment.
validateSignatureType :: InferState -> SignatureType -> Either SignatureTypeFailure ()
validateSignatureType state signatureType =
  void (signatureTypeToExpressionType state variables signatureType)
  where
    variables =
      Map.fromList
        [ (variableName, TVarType (negate position - 1))
          | (position, variableName) <-
              zip [0 :: Int ..] (constraintSignatureTypeVariableNamesInOrder signatureType)
        ]

convertSignatureType ::
  Map Text DataTypeBinding ->
  Map Text ExpressionType ->
  SignatureType ->
  Either SignatureTypeFailure ExpressionType
convertSignatureType dataTypes variables signatureType =
  case signatureType of
    TypeInt -> Right TIntType
    TypeFloat -> Right TFloatType
    TypeNumeric numericType -> Right (TNumericType numericType)
    TypeBool -> Right TBoolType
    TypeChar -> Right TCharType
    TypeText -> Right TTextType
    TypeVariable name ->
      maybe (Left (UnboundSignatureTypeVariable name)) Right
        (Map.lookup (identifierText name) variables)
    TypeName name ->
      case builtinOrVariableType name of
        Just expressionType -> Right expressionType
        Nothing -> namedType name []
    TypeApplication name arguments
      | identifierLooksLikeTypeVariable name ->
          Left (TypeVariableApplicationHead name)
      | otherwise -> namedType name arguments
    TypeList innerType ->
      TListType <$> convert innerType
    TypeTuple elementTypes ->
      TTupleType <$> traverse convert elementTypes
    TypeFunction argumentType resultType ->
      TFunctionType <$> convert argumentType <*> convert resultType
  where
    convert = convertSignatureType dataTypes variables

    builtinOrVariableType name =
      case identifierText name of
        "Int" -> Just TIntType
        "Float" -> Just TFloatType
        "Bool" -> Just TBoolType
        "Char" -> Just TCharType
        "Text" -> Just TTextType
        typeName ->
          (TNumericType <$> numericTypeFromName typeName)
            <|> Map.lookup typeName variables

    namedType name arguments =
      case Map.lookup (identifierText name) dataTypes of
        Nothing -> Left (UnknownNamedType name)
        Just (DataTypeBinding parameters _)
          | length parameters /= length arguments ->
              Left (NamedTypeArityMismatch name (length parameters) (length arguments))
          | otherwise ->
              TDataType name <$> traverse convert arguments

renderSignatureTypeFailure :: SignatureTypeFailure -> Text
renderSignatureTypeFailure failure =
  case failure of
    UnknownNamedType name ->
      "unknown named type '" <> identifierText name <> "'"
    NamedTypeArityMismatch name expected received ->
      "type '"
        <> identifierText name
        <> "' expects "
        <> tshow expected
        <> " argument(s), found "
        <> tshow received
    TypeVariableApplicationHead name ->
      "type variable '" <> identifierText name <> "' cannot be used as an application head"
    UnboundSignatureTypeVariable name ->
      "unbound type variable '" <> identifierText name <> "'"

-- | Preserve quantified variables as actual signature variables when building
-- runtime templates. This avoids disguising them as zero-arity data types.
expressionTypeToRuntimeTemplate :: Map Int Name -> ExpressionType -> Maybe SignatureType
expressionTypeToRuntimeTemplate variableNames expressionType =
  case expressionType of
    TIntType -> Just TypeInt
    TIntegerLiteralType {} -> Nothing
    TFloatType -> Just TypeFloat
    TNumericType numericType -> Just (TypeNumeric numericType)
    TBoolType -> Just TypeBool
    TCharType -> Just TypeChar
    TTextType -> Just TypeText
    TListType elementType ->
      TypeList <$> expressionTypeToRuntimeTemplate variableNames elementType
    TTupleType elementTypes ->
      TypeTuple <$> traverse (expressionTypeToRuntimeTemplate variableNames) elementTypes
    TDataType typeName typeArguments ->
      case traverse (expressionTypeToRuntimeTemplate variableNames) typeArguments of
        Just [] -> Just (TypeName typeName)
        Just argumentTemplates -> Just (TypeApplication typeName argumentTemplates)
        Nothing -> Nothing
    TFunctionType inputType outputType ->
      TypeFunction
        <$> expressionTypeToRuntimeTemplate variableNames inputType
        <*> expressionTypeToRuntimeTemplate variableNames outputType
    TVarType typeVar ->
      TypeVariable <$> Map.lookup typeVar variableNames

tshow :: Show a => a -> Text
tshow = Text.pack . show
