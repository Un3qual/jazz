{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Internal type model shared by inference subsystems.
module Jazz.Compiler.TypeInference.Types
  ( ClassMethodType (..),
    ConstructorArgumentType (..),
    DataTypeBinding (..),
    ExpressionType (..),
    ImplMethodType (..),
    IntegerLiteralRange (..),
    NumericConstraint (..),
    ScopeCapabilityFacts (..),
    TypeBinding (..),
    TypeEnv,
    TypeScheme (..),
    TypeSchemeConstraint (..),
    TypeSchemePrimitiveConstraint (..),
    emptyScopeCapabilityFacts,
    instantiateConstructorFieldType,
  )
where

import Control.DeepSeq (NFData)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import GHC.Generics (Generic)
import Jazz.Compiler.AST
  ( NumericType,
    SignaturePayload,
    SignatureType (..),
  )
import Jazz.Compiler.BuiltinCatalog
  ( BuiltinSymbol,
    numericTypeFromName,
  )
import Jazz.Compiler.Name
  ( Name,
    identifierText,
  )

data ExpressionType
  = TIntType
  | TIntegerLiteralType IntegerLiteralRange
  | TFloatType
  | TNumericType NumericType
  | TBoolType
  | TCharType
  | TTextType
  | TListType ExpressionType
  | TTupleType [ExpressionType]
  | TDataType Name [ExpressionType]
  | TFunctionType ExpressionType ExpressionType
  | TVarType Int
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

data ConstructorArgumentType
  = ConstructorArgumentMonomorphic ExpressionType
  | ConstructorArgumentParameter Text
  | ConstructorArgumentStructured SignatureType
  | ConstructorArgumentFresh
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

instantiateConstructorFieldType ::
  Map Text ExpressionType ->
  SignatureType ->
  Maybe ExpressionType
instantiateConstructorFieldType typeParameterBindings fieldType =
  case fieldType of
    TypeInt -> Just TIntType
    TypeFloat -> Just TFloatType
    TypeNumeric numericType -> Just (TNumericType numericType)
    TypeBool -> Just TBoolType
    TypeChar -> Just TCharType
    TypeText -> Just TTextType
    TypeVariable name -> Map.lookup (identifierText name) typeParameterBindings
    TypeName name ->
      Just
        ( case identifierText name of
            "Int" -> TIntType
            "Float" -> TFloatType
            "Bool" -> TBoolType
            "Char" -> TCharType
            "Text" -> TTextType
            namedTypeText ->
              maybe
                (TDataType name [])
                TNumericType
                (numericTypeFromName namedTypeText)
        )
    TypeApplication name arguments ->
      TDataType name <$> traverse (instantiateConstructorFieldType typeParameterBindings) arguments
    TypeList elementType ->
      TListType <$> instantiateConstructorFieldType typeParameterBindings elementType
    TypeTuple elementTypes ->
      TTupleType <$> traverse (instantiateConstructorFieldType typeParameterBindings) elementTypes
    TypeFunction argumentType resultType ->
      TFunctionType
        <$> instantiateConstructorFieldType typeParameterBindings argumentType
        <*> instantiateConstructorFieldType typeParameterBindings resultType

data IntegerLiteralRange = IntegerLiteralRange Integer Integer
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

data NumericConstraint
  = AnyNumericConstraint
  | RuntimeArithmeticNumericConstraint
  | RuntimeComparisonNumericConstraint
  | IntegralNumericConstraint
  | IntegralLiteralNumericConstraint IntegerLiteralRange
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

data TypeBinding
  = PlainTypeBinding ExpressionType
  | SchemeTypeBinding TypeScheme
  | BuiltinAliasTypeBinding BuiltinSymbol
  | BuiltinOperatorAliasTypeBinding Text
  | OperatorAliasSchemeTypeBinding Text TypeScheme
  | ConstructorTypeBinding Name [Name] [ConstructorArgumentType]
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

data TypeScheme = TypeScheme
  { schemeQuantifiedVariables :: Set Int,
    schemeQuantifiedOrder :: [Int],
    schemeClassConstraints :: [TypeSchemeConstraint],
    schemePrimitiveConstraints :: [TypeSchemePrimitiveConstraint],
    schemeDefiningCapabilities :: ScopeCapabilityFacts,
    schemeResultType :: ExpressionType
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

data TypeSchemePrimitiveConstraint
  = TypeSchemeNumericConstraint NumericConstraint ExpressionType
  | TypeSchemeStrictEqualityConstraint ExpressionType
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

data TypeSchemeConstraint
  = TypeSchemeConstraint Text ExpressionType
  | TypeSchemeInferredConstraint Text ExpressionType
  | TypeSchemeMethodConstraint Text Text ExpressionType
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

type TypeEnv = Map Name TypeBinding

data DataTypeBinding = DataTypeBinding [Name] [[ConstructorArgumentType]]
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

data ClassMethodType = ClassMethodType Text SignaturePayload
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

data ImplMethodType = ImplMethodType SignatureType
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

data ScopeCapabilityFacts = ScopeCapabilityFacts
  { scopeClassFacts :: Map Text Int,
    scopeGeneratedEqualityClassFacts :: Set Text,
    scopeConcreteImplFacts :: Set Text,
    scopeClassMethodSignatures :: Map Text ClassMethodType,
    scopeConcreteImplMethods :: Map Text [ImplMethodType]
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

instance Semigroup ScopeCapabilityFacts where
  leftFacts <> rightFacts =
    ScopeCapabilityFacts
      { scopeClassFacts = Map.union (scopeClassFacts leftFacts) (scopeClassFacts rightFacts),
        scopeGeneratedEqualityClassFacts =
          Set.union
            (scopeGeneratedEqualityClassFacts leftFacts)
            (scopeGeneratedEqualityClassFacts rightFacts),
        scopeConcreteImplFacts =
          Set.union
            (scopeConcreteImplFacts leftFacts)
            (scopeConcreteImplFacts rightFacts),
        scopeClassMethodSignatures =
          Map.union
            (scopeClassMethodSignatures leftFacts)
            (scopeClassMethodSignatures rightFacts),
        scopeConcreteImplMethods =
          Map.unionWith
            (<>)
            (scopeConcreteImplMethods leftFacts)
            (scopeConcreteImplMethods rightFacts)
      }

instance Monoid ScopeCapabilityFacts where
  mempty =
    ScopeCapabilityFacts
      { scopeClassFacts = Map.empty,
        scopeGeneratedEqualityClassFacts = Set.empty,
        scopeConcreteImplFacts = Set.empty,
        scopeClassMethodSignatures = Map.empty,
        scopeConcreteImplMethods = Map.empty
      }

emptyScopeCapabilityFacts :: ScopeCapabilityFacts
emptyScopeCapabilityFacts = mempty
