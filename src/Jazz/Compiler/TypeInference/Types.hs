{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Internal type model shared by inference subsystems.
module Jazz.Compiler.TypeInference.Types
  ( ClassMethodType (..),
    ClassDefinition (..),
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
    TypeBinding,
    SemanticBinding (..),
    TypeEnv,
    TypeEnvKey (..),
    typeEnvBindingKey,
    typeEnvReferenceKey,
    TypeScheme,
    SemanticScheme (..),
    TypeSchemeConstraint,
    SchemeConstraint (..),
    TypeSchemePrimitiveConstraint,
    SchemePrimitiveConstraint (..),
    emptyScopeCapabilityFacts,
    instantiateDeclarationType,
    quantifiedVariablesFromPreferred,
    quantifiedVariablesMembershipSet,
    quantifiedVariablesOrderedList,
  )
where

import Data.Map.Strict (Map)
import Jazz.Compiler.CoreIdentity (ResolvedNodeFacts, ResolvedReference, resolvedBinderReference, resolvedValueReference)
import Jazz.Compiler.Name
  ( ResolvedName,
  )
import Jazz.Compiler.SemanticDeclarations
  ( ClassDefinition (..),
    ClassMethodType (..),
    ConstructorArgumentType (..),
    DataTypeBinding (..),
    ImplMethodType (..),
    IntegerLiteralRange (..),
    NumericConstraint (..),
    QuantifiedVariables,
    SchemeConstraint (..),
    SchemePrimitiveConstraint (..),
    ScopeCapabilityFacts (..),
    SemanticBinding (..),
    SemanticScheme (..),
    emptyScopeCapabilityFacts,
    instantiateDeclarationType,
    quantifiedVariablesFromPreferred,
    quantifiedVariablesMembershipSet,
    quantifiedVariablesOrderedList,
  )
import Jazz.Compiler.TypeRepresentation (InferenceVariable (..), SemanticType (..))

type ExpressionType = SemanticType ResolvedName InferenceVariable

type TypeBinding = SemanticBinding InferenceVariable

type TypeScheme = SemanticScheme InferenceVariable

type TypeSchemePrimitiveConstraint = SchemePrimitiveConstraint ExpressionType

type TypeSchemeConstraint = SchemeConstraint ExpressionType

-- The spelling is retained for diagnostic and public-interface projection.
-- Environment equality and ordering use only the selected reference identity.
data TypeEnvKey = TypeEnvKey
  { typeEnvReference :: ResolvedReference,
    typeEnvName :: ResolvedName
  }
  deriving stock (Show)

instance Eq TypeEnvKey where
  left == right = typeEnvReference left == typeEnvReference right

instance Ord TypeEnvKey where
  compare left right = compare (typeEnvReference left) (typeEnvReference right)

type TypeEnv = Map TypeEnvKey TypeBinding

typeEnvBindingKey :: ResolvedNodeFacts -> ResolvedName -> TypeEnvKey
typeEnvBindingKey facts name = TypeEnvKey (resolvedBinderReference facts name) name

typeEnvReferenceKey :: ResolvedNodeFacts -> ResolvedName -> TypeEnvKey
typeEnvReferenceKey facts name = TypeEnvKey (resolvedValueReference facts name) name
