{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Shared recursive type syntax used across compiler stages.
module Jazz.Compiler.TypeRepresentation
  ( InferenceVariable (..),
    NumericType (..),
    SemanticType (..),
    substituteSemanticVariables,
    semanticTypeToSignature,
    SignatureConstraint (..),
    SignaturePayload (..),
    SignatureToken (..),
    SignatureType (..),
  )
where

import Control.DeepSeq (NFData)
import Data.Bifoldable (Bifoldable (bifoldMap))
import Data.Bifunctor (Bifunctor (bimap))
import Data.Bitraversable
  ( Bitraversable (bitraverse),
    bifoldMapDefault,
    bimapDefault,
  )
import Data.Text (Text)
import GHC.Generics (Generic)

-- | Nominal identity for a solver-owned type variable.
--
-- Keeping this distinct from incidental counters and source positions makes
-- inference state ownership explicit while preserving concise numeric
-- construction at solver call sites.
newtype InferenceVariable = InferenceVariable Int
  deriving stock (Eq, Generic, Ord)
  deriving newtype (Enum, Num)
  deriving anyclass (NFData)

instance Show InferenceVariable where
  show (InferenceVariable variable) = show variable

-- | Fixed-width numeric types supported throughout the compiler pipeline.
data NumericType
  = NumericInt8
  | NumericInt16
  | NumericInt32
  | NumericInt64
  | NumericUInt8
  | NumericUInt16
  | NumericUInt32
  | NumericUInt64
  | NumericFloat16
  | NumericFloat32
  | NumericFloat64
  deriving stock (Bounded, Enum, Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

-- | Recursive semantic type shape used by inference and analyzed facts.
-- Parameters distinguish nominal type identities and inference variables.
data SemanticType typeName variable
  = SemanticInt
  | SemanticFloat
  | SemanticNumeric NumericType
  | SemanticBool
  | SemanticChar
  | SemanticText
  | SemanticList (SemanticType typeName variable)
  | SemanticTuple [SemanticType typeName variable]
  | SemanticData typeName [SemanticType typeName variable]
  | SemanticFunction
      (SemanticType typeName variable)
      (SemanticType typeName variable)
  | SemanticVariable variable
  deriving stock (Eq, Foldable, Functor, Generic, Ord, Show, Traversable)
  deriving anyclass (NFData)

instance Bifunctor SemanticType where
  bimap = bimapDefault

instance Bifoldable SemanticType where
  bifoldMap = bifoldMapDefault

instance Bitraversable SemanticType where
  bitraverse mapTypeName mapVariable semanticType =
    case semanticType of
      SemanticInt -> pure SemanticInt
      SemanticFloat -> pure SemanticFloat
      SemanticNumeric numericType -> pure (SemanticNumeric numericType)
      SemanticBool -> pure SemanticBool
      SemanticChar -> pure SemanticChar
      SemanticText -> pure SemanticText
      SemanticList elementType ->
        SemanticList <$> bitraverse mapTypeName mapVariable elementType
      SemanticTuple elementTypes ->
        SemanticTuple <$> traverse (bitraverse mapTypeName mapVariable) elementTypes
      SemanticData typeName arguments ->
        SemanticData
          <$> mapTypeName typeName
          <*> traverse (bitraverse mapTypeName mapVariable) arguments
      SemanticFunction argumentType resultType ->
        SemanticFunction
          <$> bitraverse mapTypeName mapVariable argumentType
          <*> bitraverse mapTypeName mapVariable resultType
      SemanticVariable variable -> SemanticVariable <$> mapVariable variable

-- | Substitute variables without changing semantic type names or structure.
substituteSemanticVariables :: (variable -> SemanticType name replacement) -> SemanticType name variable -> SemanticType name replacement
substituteSemanticVariables replace typeValue = case typeValue of
  SemanticInt -> SemanticInt
  SemanticFloat -> SemanticFloat
  SemanticNumeric numeric -> SemanticNumeric numeric
  SemanticBool -> SemanticBool
  SemanticChar -> SemanticChar
  SemanticText -> SemanticText
  SemanticList element -> SemanticList (recur element)
  SemanticTuple elements -> SemanticTuple (map recur elements)
  SemanticData name arguments -> SemanticData name (map recur arguments)
  SemanticFunction argument result -> SemanticFunction (recur argument) (recur result)
  SemanticVariable variable -> replace variable
  where
    recur = substituteSemanticVariables replace

-- | Reify a checked type for signature diagnostics and authored constraint views.
semanticTypeToSignature :: SemanticType name variable -> SignatureType name variable
semanticTypeToSignature semanticType = case semanticType of
  SemanticInt -> TypeInt
  SemanticFloat -> TypeFloat
  SemanticNumeric numeric -> TypeNumeric numeric
  SemanticBool -> TypeBool
  SemanticChar -> TypeChar
  SemanticText -> TypeText
  SemanticVariable variable -> TypeVariable variable
  SemanticList element -> TypeList (recur element)
  SemanticTuple elements -> TypeTuple (map recur elements)
  SemanticData name [] -> TypeName name
  SemanticData name arguments -> TypeApplication name (map recur arguments)
  SemanticFunction argument result -> TypeFunction (recur argument) (recur result)
  where
    recur = semanticTypeToSignature

-- | Recursive syntax shared by surface and resolved signatures. The first
-- parameter identifies named types; the second identifies type variables.
data SignatureType typeName variable
  = TypeInt
  | TypeFloat
  | TypeNumeric NumericType
  | TypeBool
  | TypeChar
  | TypeText
  | TypeVariable variable
  | TypeName typeName
  | TypeApplication typeName [SignatureType typeName variable]
  | TypeList (SignatureType typeName variable)
  | TypeTuple [SignatureType typeName variable]
  | TypeFunction
      (SignatureType typeName variable)
      (SignatureType typeName variable)
  deriving stock (Eq, Foldable, Functor, Generic, Ord, Show, Traversable)
  deriving anyclass (NFData)

instance Bifunctor SignatureType where
  bimap = bimapDefault

instance Bifoldable SignatureType where
  bifoldMap = bifoldMapDefault

instance Bitraversable SignatureType where
  bitraverse mapTypeName mapVariable signatureType =
    case signatureType of
      TypeInt -> pure TypeInt
      TypeFloat -> pure TypeFloat
      TypeNumeric numericType -> pure (TypeNumeric numericType)
      TypeBool -> pure TypeBool
      TypeChar -> pure TypeChar
      TypeText -> pure TypeText
      TypeVariable variable -> TypeVariable <$> mapVariable variable
      TypeName typeName -> TypeName <$> mapTypeName typeName
      TypeApplication typeName arguments ->
        TypeApplication
          <$> mapTypeName typeName
          <*> traverse (bitraverse mapTypeName mapVariable) arguments
      TypeList innerType ->
        TypeList <$> bitraverse mapTypeName mapVariable innerType
      TypeTuple elementTypes ->
        TypeTuple <$> traverse (bitraverse mapTypeName mapVariable) elementTypes
      TypeFunction argumentType resultType ->
        TypeFunction
          <$> bitraverse mapTypeName mapVariable argumentType
          <*> bitraverse mapTypeName mapVariable resultType

-- | A capability name applied to signature arguments.
data SignatureConstraint typeName variable
  = SignatureConstraint typeName [SignatureType typeName variable]
  deriving stock (Eq, Foldable, Functor, Generic, Ord, Show, Traversable)
  deriving anyclass (NFData)

instance Bifunctor SignatureConstraint where
  bimap = bimapDefault

instance Bifoldable SignatureConstraint where
  bifoldMap = bifoldMapDefault

instance Bitraversable SignatureConstraint where
  bitraverse mapTypeName mapVariable (SignatureConstraint typeName arguments) =
    SignatureConstraint
      <$> mapTypeName typeName
      <*> traverse (bitraverse mapTypeName mapVariable) arguments

-- | Tokenized fallback for unsupported signature surfaces.
data SignatureToken name
  = SignatureNameToken name
  | SignatureIntToken Integer
  | SignatureArrowToken
  | SignatureAtToken
  | SignatureColonToken
  | SignatureLParenToken
  | SignatureRParenToken
  | SignatureLBraceToken
  | SignatureRBraceToken
  | SignatureLBracketToken
  | SignatureRBracketToken
  | SignatureCommaToken
  | SignatureOperatorToken Text
  | SignatureOtherToken Text
  deriving stock (Eq, Foldable, Functor, Generic, Ord, Show, Traversable)
  deriving anyclass (NFData)

-- | Structured supported signatures plus tokenized unsupported syntax.
data SignaturePayload typeName variable tokenName
  = SignatureType (SignatureType typeName variable)
  | ConstrainedSignature
      [SignatureConstraint typeName variable]
      (SignatureType typeName variable)
  | UnsupportedSignature [SignatureToken tokenName]
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)
