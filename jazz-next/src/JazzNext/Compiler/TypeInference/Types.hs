{-# LANGUAGE OverloadedStrings #-}

-- | Internal type model shared by inference subsystems.
module JazzNext.Compiler.TypeInference.Types
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
    instantiateConstructorFieldType
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import JazzNext.Compiler.AST
  ( NumericType,
    SignaturePayload,
    SignatureType (..)
  )
import JazzNext.Compiler.BuiltinCatalog
  ( BuiltinSymbol,
    numericTypeFromName
  )
import JazzNext.Compiler.Name
  ( Name,
    identifierText
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
  deriving (Eq, Show)

data ConstructorArgumentType
  = ConstructorArgumentMonomorphic ExpressionType
  | ConstructorArgumentParameter Text
  | ConstructorArgumentStructured SignatureType
  | ConstructorArgumentFresh
  deriving (Eq, Show)

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
  deriving (Eq, Show)

data NumericConstraint
  = AnyNumericConstraint
  | RuntimeArithmeticNumericConstraint
  | RuntimeComparisonNumericConstraint
  | IntegralNumericConstraint
  | IntegralLiteralNumericConstraint IntegerLiteralRange
  deriving (Eq, Show)

data TypeBinding
  = PlainTypeBinding ExpressionType
  | SchemeTypeBinding TypeScheme
  | BuiltinAliasTypeBinding BuiltinSymbol
  | BuiltinOperatorAliasTypeBinding Text
  | OperatorAliasSchemeTypeBinding Text TypeScheme
  | ConstructorTypeBinding Name [Name] [ConstructorArgumentType]
  deriving (Eq, Show)

data TypeScheme = TypeScheme
  { schemeQuantifiedVariables :: Set Int,
    schemeQuantifiedOrder :: [Int],
    schemeClassConstraints :: [TypeSchemeConstraint],
    schemePrimitiveConstraints :: [TypeSchemePrimitiveConstraint],
    schemeDefiningCapabilities :: ScopeCapabilityFacts,
    schemeResultType :: ExpressionType
  }
  deriving (Eq, Show)

data TypeSchemePrimitiveConstraint
  = TypeSchemeNumericConstraint NumericConstraint ExpressionType
  | TypeSchemeStrictEqualityConstraint ExpressionType
  deriving (Eq, Show)

data TypeSchemeConstraint
  = TypeSchemeConstraint Text ExpressionType
  | TypeSchemeInferredConstraint Text ExpressionType
  | TypeSchemeMethodConstraint Text Text ExpressionType
  deriving (Eq, Show)

type TypeEnv = Map Name TypeBinding

data DataTypeBinding = DataTypeBinding [Name] [[ConstructorArgumentType]]
  deriving (Eq, Show)

data ClassMethodType = ClassMethodType Text SignaturePayload
  deriving (Eq, Show)

data ImplMethodType = ImplMethodType SignatureType
  deriving (Eq, Show)

data ScopeCapabilityFacts = ScopeCapabilityFacts
  { scopeClassFacts :: Map Text Int,
    scopeGeneratedEqualityClassFacts :: Set Text,
    scopeConcreteImplFacts :: Set Text,
    scopeClassMethodSignatures :: Map Text ClassMethodType,
    scopeConcreteImplMethods :: Map Text [ImplMethodType]
  }
  deriving (Eq, Show)

emptyScopeCapabilityFacts :: ScopeCapabilityFacts
emptyScopeCapabilityFacts =
  ScopeCapabilityFacts
    { scopeClassFacts = Map.empty,
      scopeGeneratedEqualityClassFacts = Set.empty,
      scopeConcreteImplFacts = Set.empty,
      scopeClassMethodSignatures = Map.empty,
      scopeConcreteImplMethods = Map.empty
    }
