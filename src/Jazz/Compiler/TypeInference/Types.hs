{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Internal type model shared by inference subsystems.
module Jazz.Compiler.TypeInference.Types
  ( ClassMethodType (..),
    ConstructorArgumentType (..),
    DataTypeBinding (..),
    ExpressionType,
    ImplMethodType (..),
    InferenceVariable (..),
    IntegerLiteralRange (..),
    NumericConstraint (..),
    QuantifiedVariables,
    ScopeCapabilityFacts (..),
    SemanticType (..),
    TypeBinding (..),
    TypeEnv,
    TypeEnvKey (..),
    typeEnvBindingKey,
    typeEnvReferenceKey,
    TypeScheme (..),
    TypeSchemeConstraint,
    SchemeConstraint (..),
    TypeSchemePrimitiveConstraint,
    SchemePrimitiveConstraint (..),
    emptyScopeCapabilityFacts,
    instantiateConstructorFieldType,
    quantifiedVariablesFromPreferred,
    quantifiedVariablesMembershipSet,
    quantifiedVariablesOrderedList,
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
  ( CorePhase (Resolved),
    SignaturePayload,
    SignatureType,
  )
import Jazz.Compiler.BuiltinCatalog
  ( BuiltinSymbol,
    numericTypeFromName,
  )
import Jazz.Compiler.CapabilityFacts (ConcreteImplFact)
import Jazz.Compiler.CoreIdentity (ResolvedNodeFacts, ResolvedReference, resolvedBinderReference, resolvedValueReference)
import Jazz.Compiler.Name
  ( ResolvedName,
    identifierText,
  )
import Jazz.Compiler.StableSet
  ( StableSet,
    stableSetFromPreferred,
    stableSetMembershipSet,
    stableSetOrderedList,
  )
import Jazz.Compiler.TypeRepresentation
  ( InferenceVariable (..),
    SemanticType (..),
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
  )

type ExpressionType = SemanticType ResolvedName InferenceVariable

data ConstructorArgumentType
  = ConstructorArgumentMonomorphic ExpressionType
  | ConstructorArgumentParameter Text
  | ConstructorArgumentStructured (SignatureType 'Resolved)
  | ConstructorArgumentFresh
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

instantiateConstructorFieldType ::
  Map Text ExpressionType ->
  SignatureType 'Resolved ->
  Maybe ExpressionType
instantiateConstructorFieldType typeParameterBindings fieldType =
  case fieldType of
    TypeInt -> Just SemanticInt
    TypeFloat -> Just SemanticFloat
    TypeNumeric numericType -> Just (SemanticNumeric numericType)
    TypeBool -> Just SemanticBool
    TypeChar -> Just SemanticChar
    TypeText -> Just SemanticText
    TypeVariable name -> Map.lookup (identifierText name) typeParameterBindings
    TypeName name ->
      Just
        ( case identifierText name of
            "Int" -> SemanticInt
            "Float" -> SemanticFloat
            "Bool" -> SemanticBool
            "Char" -> SemanticChar
            "Text" -> SemanticText
            namedTypeText ->
              maybe
                (SemanticData name [])
                SemanticNumeric
                (numericTypeFromName namedTypeText)
        )
    TypeApplication name arguments ->
      SemanticData name <$> traverse (instantiateConstructorFieldType typeParameterBindings) arguments
    TypeList elementType ->
      SemanticList <$> instantiateConstructorFieldType typeParameterBindings elementType
    TypeTuple elementTypes ->
      SemanticTuple <$> traverse (instantiateConstructorFieldType typeParameterBindings) elementTypes
    TypeFunction argumentType resultType ->
      SemanticFunction
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
  | ConstructorTypeBinding ResolvedName [ResolvedName] [ConstructorArgumentType]
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

newtype QuantifiedVariables = QuantifiedVariables (StableSet InferenceVariable)
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

quantifiedVariablesFromPreferred :: [InferenceVariable] -> Set InferenceVariable -> QuantifiedVariables
quantifiedVariablesFromPreferred preferred variables =
  QuantifiedVariables (stableSetFromPreferred preferred variables)

quantifiedVariablesMembershipSet :: QuantifiedVariables -> Set InferenceVariable
quantifiedVariablesMembershipSet (QuantifiedVariables variables) =
  stableSetMembershipSet variables

quantifiedVariablesOrderedList :: QuantifiedVariables -> [InferenceVariable]
quantifiedVariablesOrderedList (QuantifiedVariables variables) =
  stableSetOrderedList variables

data TypeScheme = TypeScheme
  { schemeQuantifiedVariables :: QuantifiedVariables,
    schemeClassConstraints :: [TypeSchemeConstraint],
    schemePrimitiveConstraints :: [TypeSchemePrimitiveConstraint],
    schemeDefiningCapabilities :: ScopeCapabilityFacts,
    schemeResultType :: ExpressionType
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

type TypeSchemePrimitiveConstraint = SchemePrimitiveConstraint ExpressionType

data SchemePrimitiveConstraint typeValue
  = TypeSchemeNumericConstraint NumericConstraint typeValue
  | TypeSchemeStrictEqualityConstraint typeValue
  deriving stock (Eq, Foldable, Functor, Generic, Show, Traversable)
  deriving anyclass (NFData)

type TypeSchemeConstraint = SchemeConstraint ExpressionType

data SchemeConstraint typeValue
  = TypeSchemeConstraint Text typeValue
  | TypeSchemeInferredConstraint Text typeValue
  | TypeSchemeMethodConstraint Text Text typeValue
  deriving stock (Eq, Foldable, Functor, Generic, Ord, Show, Traversable)
  deriving anyclass (NFData)

-- The spelling is retained for diagnostic and public-interface projection.
-- Environment equality and ordering use only the selected reference identity.
data TypeEnvKey = TypeEnvKey
  { typeEnvReference :: ResolvedReference,
    typeEnvName :: ResolvedName
  }
  deriving stock (Generic, Show)
  deriving anyclass (NFData)

instance Eq TypeEnvKey where
  left == right = typeEnvReference left == typeEnvReference right

instance Ord TypeEnvKey where
  compare left right = compare (typeEnvReference left) (typeEnvReference right)

type TypeEnv = Map TypeEnvKey TypeBinding

typeEnvBindingKey :: ResolvedNodeFacts -> ResolvedName -> TypeEnvKey
typeEnvBindingKey facts = TypeEnvKey (resolvedBinderReference facts)

typeEnvReferenceKey :: ResolvedNodeFacts -> ResolvedName -> TypeEnvKey
typeEnvReferenceKey facts = TypeEnvKey (resolvedValueReference facts)

data DataTypeBinding = DataTypeBinding [ResolvedName] [[ConstructorArgumentType]]
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

data ClassMethodType = ClassMethodType Text (SignaturePayload 'Resolved)
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

newtype ImplMethodType = ImplMethodType (SignatureType 'Resolved)
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

data ScopeCapabilityFacts = ScopeCapabilityFacts
  { scopeClassFacts :: Map Text Int,
    scopeGeneratedEqualityClassFacts :: Set Text,
    scopeConcreteImplFacts :: Set ConcreteImplFact,
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
