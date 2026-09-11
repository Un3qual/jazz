{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Validated declaration templates, independent of a checker's solver state.
module Jazz.Compiler.SemanticDeclarations
  ( ClassMethodType (..),
    ConstructorArgumentType (..),
    DataTypeBinding (..),
    ImplMethodType (..),
    SignatureTypeFailure (..),
    instantiateDeclarationType,
    normalizeSignatureType,
    semanticFunctionArguments,
  )
where

import Control.Applicative ((<|>))
import Control.DeepSeq (NFData)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import GHC.Generics (Generic)
import Jazz.Compiler.BuiltinCatalog (numericTypeFromName)
import Jazz.Compiler.CapabilityFacts (identifierLooksLikeTypeVariable)
import Jazz.Compiler.CoreIdentity (CapabilityId, MethodId)
import Jazz.Compiler.Name (ResolvedName, identifierText)
import Jazz.Compiler.TypeRepresentation (SemanticType (..), SignatureType (..), substituteSemanticVariables)

-- | A checked method type with its class parameter explicitly bound.
data ClassMethodType = ClassMethodType Text (SemanticType ResolvedName Text)
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

-- | The declaration selected by method checking also owns its evidence identity.
data ImplMethodType = ImplMethodType
  { implMethodTarget :: SignatureType ResolvedName ResolvedName,
    implMethodCapability :: CapabilityId,
    implMethodIdentity :: MethodId
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

-- | Parameters are bound by the enclosing data declaration. Invalid fields
-- remain only during diagnostic recovery and cannot reach successful analysis.
data ConstructorArgumentType
  = ConstructorArgumentType (SemanticType ResolvedName Text)
  | ConstructorArgumentFresh
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

data DataTypeBinding = DataTypeBinding [ResolvedName] [[ConstructorArgumentType]]
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

instantiateDeclarationType :: Map Text (SemanticType ResolvedName variable) -> SemanticType ResolvedName Text -> Maybe (SemanticType ResolvedName variable)
instantiateDeclarationType parameters field =
  substituteSemanticVariables id <$> traverse (`Map.lookup` parameters) field

data SignatureTypeFailure
  = UnknownNamedType ResolvedName
  | NamedTypeArityMismatch ResolvedName Int Int
  | TypeVariableApplicationHead ResolvedName
  | UnboundSignatureTypeVariable ResolvedName
  deriving (Eq, Show)

normalizeSignatureType ::
  Map Text DataTypeBinding ->
  Map Text (SemanticType ResolvedName variable) ->
  SignatureType ResolvedName ResolvedName ->
  Either SignatureTypeFailure (SemanticType ResolvedName variable)
normalizeSignatureType dataTypes variables signatureType =
  case signatureType of
    TypeInt -> Right SemanticInt
    TypeFloat -> Right SemanticFloat
    TypeNumeric numericType -> Right (SemanticNumeric numericType)
    TypeBool -> Right SemanticBool
    TypeChar -> Right SemanticChar
    TypeText -> Right SemanticText
    TypeVariable name ->
      maybe
        (Left (UnboundSignatureTypeVariable name))
        Right
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
      SemanticList <$> convert innerType
    TypeTuple elementTypes ->
      SemanticTuple <$> traverse convert elementTypes
    TypeFunction argumentType resultType ->
      SemanticFunction <$> convert argumentType <*> convert resultType
  where
    convert = normalizeSignatureType dataTypes variables

    builtinOrVariableType name =
      case identifierText name of
        "Int" -> Just SemanticInt
        "Float" -> Just SemanticFloat
        "Bool" -> Just SemanticBool
        "Char" -> Just SemanticChar
        "Text" -> Just SemanticText
        typeName ->
          (SemanticNumeric <$> numericTypeFromName typeName)
            <|> Map.lookup typeName variables

    namedType name arguments =
      case Map.lookup (identifierText name) dataTypes of
        Nothing -> Left (UnknownNamedType name)
        Just (DataTypeBinding parameters _)
          | length parameters /= length arguments ->
              Left (NamedTypeArityMismatch name (length parameters) (length arguments))
          | otherwise ->
              SemanticData name <$> traverse convert arguments

semanticFunctionArguments :: SemanticType name variable -> ([SemanticType name variable], SemanticType name variable)
semanticFunctionArguments (SemanticFunction argument result) =
  let (arguments, finalResult) = semanticFunctionArguments result
   in (argument : arguments, finalResult)
semanticFunctionArguments result = ([], result)
