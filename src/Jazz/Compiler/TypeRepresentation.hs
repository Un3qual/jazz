{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Shared recursive type syntax used from parsing through type inference.
module Jazz.Compiler.TypeRepresentation
  ( InferenceVariable (..),
    NumericType (..),
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
