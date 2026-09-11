{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

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

import Control.DeepSeq (NFData)
import Data.Map.Strict (Map)
import GHC.Generics (Generic)
import Jazz.Compiler.CoreIdentity (ResolvedNodeFacts, ResolvedReference, resolvedBinderReference, resolvedValueReference)
import Jazz.Compiler.Name
  ( ResolvedName,
  )
import Jazz.Compiler.SemanticDeclarations
  ( ClassMethodType (..),
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
