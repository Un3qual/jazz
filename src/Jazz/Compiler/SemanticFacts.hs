{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Phase-local semantic identities and the complete facts attached by
-- analysis. This module is intentionally neutral: syntax imports it, while
-- inference and runtime populate or consume it in later phases.
module Jazz.Compiler.SemanticFacts
  ( AnalyzedType,
    AnalyzedCapabilityFacts (..),
    AnalyzedConcreteImplFact (..),
    AnalyzedMethodSignature (..),
    AnalyzedNumericConstraint (..),
    AnalyzedPrimitiveConstraint (..),
    AnalyzedScheme (..),
    AnalyzedSchemeConstraint (..),
    BinaryOperation (..),
    BinaryOperandTyping (..),
    CapabilityId (..),
    CoreBinderId (..),
    CoreNodeId (..),
    EvidenceReference (..),
    ExpressionFacts (..),
    ImplId (..),
    MethodId (..),
    NumericTarget (..),
    PatternConstructorFact (..),
    PatternFacts (..),
    PatternRefutability (..),
    RuntimeObligation (..),
    RuntimePlan (..),
    SemanticFactInvariantFailure (..),
    SemanticInstantiation (..),
    StatementDeclarationFact (..),
    StatementFacts (..),
  )
where

import Control.DeepSeq (NFData)
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import Data.Sequence (Seq)
import Data.Set (Set)
import Data.Text (Text)
import GHC.Generics (Generic)
import Jazz.Compiler.ModuleIdentity (ModulePath)
import Jazz.Compiler.Name (Identifier, ResolvedName)
import Jazz.Compiler.TypeRepresentation
  ( InferenceVariable,
    NumericType,
    SemanticType,
  )

type AnalyzedType = SemanticType ResolvedName InferenceVariable

newtype CoreNodeId = CoreNodeId Int
  deriving stock (Eq, Generic, Ord, Show)
  deriving newtype (Enum)
  deriving anyclass (NFData)

newtype CoreBinderId = CoreBinderId (ModulePath, CoreNodeId)
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

newtype CapabilityId = CapabilityId ResolvedName
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

newtype ImplId = ImplId (ModulePath, CoreNodeId)
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

newtype MethodId = MethodId (ImplId, Identifier)
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

data SemanticInstantiation = SemanticInstantiation
  { instantiatedBinder :: CoreBinderId,
    instantiatedTypes :: NonEmpty AnalyzedType
  }
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

data EvidenceReference = EvidenceReference
  { evidenceCapability :: CapabilityId,
    evidenceImplementation :: ImplId,
    evidenceMethod :: Maybe MethodId,
    evidenceType :: AnalyzedType
  }
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

data NumericTarget
  = DefaultIntegerTarget
  | ConcreteNumericTarget NumericType
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

newtype RuntimePlan = RuntimePlan (Seq RuntimeObligation)
  deriving stock (Eq, Generic, Show)
  deriving newtype (Semigroup, Monoid)
  deriving anyclass (NFData)

data RuntimeObligation
  = InstantiateTypes (NonEmpty AnalyzedType)
  | SupplyEvidence (NonEmpty EvidenceReference)
  | SpecializeNumericLiteral NumericTarget
  | ConstrainResult AnalyzedType
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

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
  { expressionSemanticType :: AnalyzedType,
    expressionBinaryOperation :: Maybe BinaryOperation,
    expressionNumericConstraints :: Map InferenceVariable AnalyzedNumericConstraint,
    expressionInstantiations :: [SemanticInstantiation],
    expressionEvidence :: [EvidenceReference],
    expressionRuntimePlan :: RuntimePlan
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
  { patternBindingTypes :: Map ResolvedName AnalyzedType,
    patternConstructorFact :: PatternConstructorFact,
    patternRefutability :: PatternRefutability
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

data StatementFacts = StatementFacts
  { statementBinderIds :: [CoreBinderId],
    statementGeneralizedSchemes :: Map CoreBinderId AnalyzedScheme,
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
  | ModuleDeclaration [Text]
  | ImportDeclaration [Text]
  | ExpressionDeclaration
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

-- | Violations detected while collecting or attaching facts. These are
-- compiler invariant failures, not source diagnostics.
data SemanticFactInvariantFailure
  = DuplicateExpressionFacts CoreNodeId
  | DuplicateExplicitInstantiationSeed CoreNodeId
  | DuplicatePatternFacts CoreNodeId
  | DuplicateStatementFacts CoreNodeId
  | MissingExpressionFacts CoreNodeId
  | MissingExpressionEvidence CoreNodeId
  | AmbiguousExpressionEvidence CoreNodeId
  | MissingExplicitInstantiationSeed CoreNodeId
  | MismatchedExplicitInstantiationSeed CoreNodeId ResolvedName ResolvedName
  | UnexpectedExplicitInstantiationSeed CoreNodeId
  | MissingExplicitInstantiationBinder CoreNodeId ResolvedName
  | UnidentifiedExplicitInstantiationBinder CoreNodeId
  | MissingPatternFacts CoreNodeId
  | MissingStatementFacts CoreNodeId
  | MissingStatementScheme CoreNodeId CoreBinderId
  | AnalyzedModuleRootNotBlock CoreNodeId
  | InvalidAnalyzedMethodSignature Text
  | InvalidAnalyzedImplementationTarget CoreNodeId
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

-- | Neutral, post-inference scheme projection stored on analyzed statements.
-- Solver bookkeeping remains inference-owned; this retains every semantic
-- component needed by later compiler phases without importing inference state.
data AnalyzedScheme = AnalyzedScheme
  { analyzedSchemeVariables :: [InferenceVariable],
    analyzedSchemeConstraints :: [AnalyzedSchemeConstraint],
    analyzedSchemePrimitiveConstraints :: [AnalyzedPrimitiveConstraint],
    analyzedSchemeDefiningCapabilities :: AnalyzedCapabilityFacts,
    analyzedSchemeType :: AnalyzedType
  }
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

data AnalyzedSchemeConstraint
  = AnalyzedExplicitCapabilityConstraint Text AnalyzedType
  | AnalyzedInferredCapabilityConstraint Text AnalyzedType
  | AnalyzedMethodCapabilityConstraint Text Text AnalyzedType
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

-- | Neutral semantic projection of the capability environment captured by a
-- generalized scheme. Task 9 constructs this projection from inference-owned
-- facts; retaining it here prevents analyzed statements from depending on
-- solver state while preserving every entailment input the current scheme
-- representation owns.
data AnalyzedCapabilityFacts = AnalyzedCapabilityFacts
  { analyzedClassArities :: Map Text Int,
    analyzedGeneratedEqualityClasses :: Set Text,
    analyzedConcreteImplementations :: Set AnalyzedConcreteImplFact,
    analyzedClassMethodSignatures :: Map Text AnalyzedMethodSignature,
    analyzedConcreteImplMethods :: Map Text [AnalyzedType]
  }
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

data AnalyzedConcreteImplFact = AnalyzedConcreteImplFact CapabilityId AnalyzedType
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

-- | The class parameter is an explicit binder local to this signature, even
-- when the method does not use it. No other free variables are admitted by
-- checked projection; consumers never infer parameter identity from type shape.
data AnalyzedMethodSignature = AnalyzedMethodSignature
  { analyzedMethodClassParameter :: InferenceVariable,
    analyzedMethodType :: AnalyzedType
  }
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)
