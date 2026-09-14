{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

-- | Complete semantic decisions owned by analyzed nodes. Syntax imports this
-- neutral vocabulary; checking constructs the facts and execution consumes them.
module Jazz.Compiler.SemanticFacts
  ( AnalyzedType,
    AnalyzedMethodSignature (..),
    AnalyzedNumericConstraint (..),
    AnalyzedPrimitiveConstraint (..),
    AnalyzedScheme (..),
    AnalyzedSchemeConstraint (..),
    BinaryOperation (..),
    BinaryOperandTyping (..),
    EvidenceReference (..),
    mapEvidenceTypes,
    ExpressionFacts (..),
    PatternConstructorFact (..),
    PatternFacts (..),
    PatternRefutability (..),
    SemanticFactInvariantFailure (..),
    SemanticInstantiation (..),
    InstantiationTarget (..),
    StatementDeclarationFact (..),
    StatementFacts (..),
  )
where

import Control.DeepSeq (NFData)
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Text (Text)
import GHC.Generics (Generic)
import Jazz.Compiler.CoreIdentity (CapabilityId, CapabilityMethodKey, CoreBinderId, CoreNodeId, ImplId, MethodId, ResolvedNodeFacts, ResolvedReference)
import Jazz.Compiler.ModuleIdentity (ModulePath)
import Jazz.Compiler.Name (Identifier, ResolvedName)
import Jazz.Compiler.TypeRepresentation
  ( InferenceVariable,
    SemanticType,
  )

type AnalyzedType = SemanticType ResolvedName InferenceVariable

data InstantiationTarget
  = LexicalInstantiation CoreBinderId
  | MethodInstantiation CapabilityMethodKey
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

data SemanticInstantiation = SemanticInstantiation
  { instantiatedTarget :: InstantiationTarget,
    instantiatedTypes :: NonEmpty AnalyzedType
  }
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

data EvidenceReference
  = EvidenceReference
      { evidenceCapability :: CapabilityId,
        evidenceImplementation :: ImplId,
        evidenceMethod :: Maybe MethodId,
        evidenceType :: AnalyzedType,
        evidencePrerequisites :: [EvidenceReference]
      }
  | PendingEvidence
      { evidenceCapability :: CapabilityId,
        evidenceMember :: Maybe Identifier,
        evidenceType :: AnalyzedType
      }
  | ParameterEvidence
      { evidenceParameterOwner :: CoreBinderId,
        evidenceParameterIndex :: Int,
        evidenceProjection :: [CapabilityId],
        evidenceCapability :: CapabilityId,
        evidenceMember :: Maybe Identifier,
        evidenceType :: AnalyzedType
      }
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

mapEvidenceTypes :: (AnalyzedType -> AnalyzedType) -> EvidenceReference -> EvidenceReference
mapEvidenceTypes transform reference = case reference of
  EvidenceReference {} ->
    reference
      { evidenceType = transform (evidenceType reference),
        evidencePrerequisites = map (mapEvidenceTypes transform) (evidencePrerequisites reference)
      }
  _ -> reference {evidenceType = transform (evidenceType reference)}

-- | The primitive operation selected by inference, including the original
-- operand identities when application syntax or an alias selected the operator.
-- This is a decision attached to the existing tree, not a second expression.
data BinaryOperation = BinaryOperation
  { binaryOperationSymbol :: Text,
    binaryOperationOperandTyping :: BinaryOperandTyping,
    binaryOperationLeftOperand :: CoreNodeId,
    binaryOperationRightOperand :: CoreNodeId
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

data BinaryOperandTyping
  = UniformBinaryOperands AnalyzedType
  | Float64PromotedOperands
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

data ExpressionFacts = ExpressionFacts
  { expressionResolution :: ResolvedNodeFacts,
    expressionSemanticType :: AnalyzedType,
    expressionBinaryOperation :: Maybe BinaryOperation,
    expressionNumericConstraints :: Map InferenceVariable AnalyzedNumericConstraint,
    expressionInstantiations :: [SemanticInstantiation],
    expressionEvidence :: [EvidenceReference],
    -- | Dictionary cells needed by a lambda body, prepared during analysis.
    expressionEvidenceCaptures :: Set ResolvedReference,
    -- | Closed representation enforced on return. Generalized definitions may
    -- suppress this even when a particular checked use has a concrete type.
    expressionResultRepresentation :: Maybe AnalyzedType
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

data PatternConstructorFact
  = PatternHasNoConstructor
  | PatternConstructor ResolvedName
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

data PatternRefutability
  = IrrefutablePattern
  | RefutablePattern
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

data PatternFacts = PatternFacts
  { patternResolution :: ResolvedNodeFacts,
    patternBindingTypes :: Map ResolvedName AnalyzedType,
    patternConstructorFact :: PatternConstructorFact,
    patternRefutability :: PatternRefutability
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

data StatementFacts = StatementFacts
  { statementResolution :: ResolvedNodeFacts,
    statementBinding :: Maybe (CoreBinderId, AnalyzedScheme),
    statementDeclarationFact :: StatementDeclarationFact
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

-- | The declaration role owned by a statement. Keeping this non-optional makes
-- analyzed statement facts exhaustive without retaining source syntax as a
-- semantic side channel.
data StatementDeclarationFact
  = ValueDeclaration ResolvedName
  | SignatureDeclaration ResolvedName
  | DataDeclaration ResolvedName [ResolvedName]
  | CapabilityDeclaration ResolvedName [ResolvedName]
  | MethodDeclaration ResolvedName AnalyzedMethodSignature
  | ImplementationDeclaration ResolvedName [AnalyzedType]
  | ModuleDeclaration ModulePath
  | ImportDeclaration ModulePath
  | ExpressionDeclaration
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

-- | Violations detected while finalizing checked facts. These are
-- compiler invariant failures, not source diagnostics.
data SemanticFactInvariantFailure
  = MissingScopeFacts CoreNodeId
  | MissingExpressionFacts CoreNodeId
  | UnresolvedExpressionReference CoreNodeId ResolvedName
  | MissingExpressionEvidence CoreNodeId
  | MissingExplicitInstantiationSeed CoreNodeId
  | MismatchedExplicitInstantiationSeed CoreNodeId ResolvedName ResolvedName
  | UnexpectedExplicitInstantiationSeed CoreNodeId
  | MissingExplicitInstantiationBinder CoreNodeId ResolvedName
  | UnidentifiedExplicitInstantiationBinder CoreNodeId
  | MissingPatternFacts CoreNodeId
  | MissingStatementFacts CoreNodeId
  | MissingStatementBinder CoreNodeId
  | MissingStatementScheme CoreNodeId CoreBinderId
  | AnalyzedModuleRootNotBlock CoreNodeId
  | InvalidAnalyzedMethodSignature Text
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

-- | Neutral, post-inference scheme projection stored on analyzed statements.
-- Solver bookkeeping remains inference-owned; this retains every semantic
-- component needed by later compiler phases without importing inference state.
data AnalyzedScheme = AnalyzedScheme
  { analyzedSchemeVariables :: [InferenceVariable],
    analyzedSchemeConstraints :: [AnalyzedSchemeConstraint],
    analyzedSchemePrimitiveConstraints :: [AnalyzedPrimitiveConstraint],
    analyzedSchemeType :: AnalyzedType
  }
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

data AnalyzedSchemeConstraint
  = AnalyzedExplicitCapabilityConstraint CapabilityId AnalyzedType
  | AnalyzedInferredCapabilityConstraint CapabilityId AnalyzedType
  | AnalyzedMethodCapabilityConstraint CapabilityId CapabilityMethodKey AnalyzedType
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

data AnalyzedPrimitiveConstraint
  = AnalyzedNumericPrimitiveConstraint AnalyzedNumericConstraint AnalyzedType
  | AnalyzedStrictEqualityPrimitiveConstraint AnalyzedType
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

data AnalyzedNumericConstraint
  = AnalyzedAnyNumericConstraint
  | AnalyzedRuntimeArithmeticNumericConstraint
  | AnalyzedRuntimeComparisonNumericConstraint
  | AnalyzedIntegralNumericConstraint
  | AnalyzedIntegralLiteralNumericConstraint Integer Integer
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

data AnalyzedMethodSignature = AnalyzedMethodSignature
  { analyzedMethodClassParameter :: InferenceVariable,
    analyzedMethodType :: AnalyzedType
  }
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)
