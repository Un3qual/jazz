-- | Explicitly separated state for inference traversal and solver operations.
module Jazz.Compiler.TypeInference.State
  ( DeclarationState (..),
    DeferredExplicitConstraint (..),
    ExplicitInstantiationSeed (..),
    ExplicitInstantiationTarget (..),
    EvidenceReference (..),
    InferState (..),
    InferenceOutput (..),
    ModuleInferenceState (..),
    SolverState (..),
    inferClassFacts,
    inferClassMethodSignatures,
    inferConstructorWitnessNames,
    inferDataTypes,
    inferDeferredExplicitConstraintCount,
    inferDeferredExplicitConstraints,
    inferErrorCount,
    inferErrorsRev,
    inferInferredClassConstraintCount,
    inferInferredClassConstraints,
    inferNextTypeVar,
    inferNumericVars,
    inferPatternCoverageSites,
    inferRigidTypeVars,
    inferStrictEqualityVars,
    inferSubst,
    inferVisibleTypes,
    initialInferState,
    previewInference,
    rejectPatternAttempt,
    modifyDeclarationState,
    modifyInferenceOutput,
    recordPatternCoverageSite,
    reservePatternCoverageSite,
  )
where

import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Jazz.Compiler.CoreIdentity (CapabilityId, CapabilityMethodKey, CoreBinderId)
import Jazz.Compiler.Diagnostics (Diagnostic)
import Jazz.Compiler.Name (ResolvedName, UnresolvedName)
import Jazz.Compiler.PatternCoverage (PatternCoverageSite)
import Jazz.Compiler.SemanticDeclarations (ClassDefinition, DeclarationVariable)
import Jazz.Compiler.SemanticFacts (EvidenceReference (..))
import Jazz.Compiler.TypeInference.Types
  ( ClassMethodType,
    DataTypeBinding,
    ExpressionType,
    InferenceVariable,
    NumericConstraint,
    ScopeCapabilityFacts (..),
    TypeEnv,
    TypeSchemeConstraint,
    emptyScopeCapabilityFacts,
  )

data SolverState = SolverState
  { solverNextTypeVar :: InferenceVariable,
    solverSubstitution :: Map InferenceVariable ExpressionType,
    solverStrictEqualityVars :: Set InferenceVariable,
    solverNumericVars :: Map InferenceVariable NumericConstraint,
    solverRigidTypeVars :: Set InferenceVariable
  }
  deriving (Eq, Show)

data DeclarationState = DeclarationState
  { declarationDataTypes :: Map ResolvedName DataTypeBinding,
    declarationCapabilities :: ScopeCapabilityFacts
  }
  deriving (Eq, Show)

data ModuleInferenceState = ModuleInferenceState
  { inferenceDeclarationParameters :: Map InferenceVariable DeclarationVariable,
    inferenceConstructorWitnessNames :: Map ResolvedName UnresolvedName,
    inferenceEvidenceParameters :: Map (CapabilityId, ExpressionType) (CoreBinderId, Int, [CapabilityId]),
    inferenceRecursiveEvidence :: Map CoreBinderId [EvidenceReference],
    inferenceVisibleTypes :: TypeEnv
  }
  deriving (Eq, Show)

data InferenceOutput = InferenceOutput
  { outputDeferredConstraints :: Seq DeferredExplicitConstraint,
    outputInferredConstraints :: [TypeSchemeConstraint],
    outputInferredConstraintCount :: Int,
    outputErrorsRev :: [Diagnostic],
    outputErrorCount :: Int,
    outputPatternCoverageSites :: Seq PatternCoverageSite,
    outputNextPatternCoverageOrdinal :: Int,
    outputEvidence :: Map EvidenceReference EvidenceReference
  }
  deriving (Eq, Show)

data ExplicitInstantiationTarget
  = ExplicitBinderInstantiation ResolvedName
  | ExplicitQualifiedMethodInstantiation ResolvedName

data ExplicitInstantiationSeed = ExplicitInstantiationSeed
  { explicitInstantiationSeedTarget :: ExplicitInstantiationTarget,
    explicitInstantiationSeedArguments :: NonEmpty ExpressionType
  }

data InferState = InferState
  { inferSolver :: SolverState,
    inferDeclarations :: DeclarationState,
    inferModule :: ModuleInferenceState,
    inferOutput :: InferenceOutput
  }
  deriving (Eq, Show)

data DeferredExplicitConstraint = DeferredExplicitConstraint
  { deferredConstraintName :: CapabilityId,
    deferredMethodKey :: Maybe CapabilityMethodKey,
    deferredArgumentType :: ExpressionType,
    deferredVisibleFacts :: ScopeCapabilityFacts
  }
  deriving (Eq, Show)

modifyDeclarationState :: (DeclarationState -> DeclarationState) -> InferState -> InferState
modifyDeclarationState update state =
  state {inferDeclarations = update (inferDeclarations state)}

modifyInferenceOutput :: (InferenceOutput -> InferenceOutput) -> InferState -> InferState
modifyInferenceOutput update state =
  state {inferOutput = update (inferOutput state)}

initialInferState :: InferState
initialInferState =
  InferState
    { inferSolver =
        SolverState
          { solverNextTypeVar = 0,
            solverSubstitution = Map.empty,
            solverStrictEqualityVars = Set.empty,
            solverNumericVars = Map.empty,
            solverRigidTypeVars = Set.empty
          },
      inferDeclarations =
        DeclarationState
          { declarationDataTypes = Map.empty,
            declarationCapabilities = emptyScopeCapabilityFacts
          },
      inferModule =
        ModuleInferenceState
          { inferenceDeclarationParameters = Map.empty,
            inferenceConstructorWitnessNames = Map.empty,
            inferenceEvidenceParameters = Map.empty,
            inferenceRecursiveEvidence = Map.empty,
            inferenceVisibleTypes = Map.empty
          },
      inferOutput =
        InferenceOutput
          { outputDeferredConstraints = Seq.empty,
            outputInferredConstraints = [],
            outputInferredConstraintCount = 0,
            outputErrorsRev = [],
            outputErrorCount = 0,
            outputPatternCoverageSites = Seq.empty,
            outputNextPatternCoverageOrdinal = 0,
            outputEvidence = Map.empty
          }
    }

inferNextTypeVar :: InferState -> InferenceVariable
inferNextTypeVar = solverNextTypeVar . inferSolver

inferSubst :: InferState -> Map InferenceVariable ExpressionType
inferSubst = solverSubstitution . inferSolver

inferStrictEqualityVars :: InferState -> Set InferenceVariable
inferStrictEqualityVars = solverStrictEqualityVars . inferSolver

inferNumericVars :: InferState -> Map InferenceVariable NumericConstraint
inferNumericVars = solverNumericVars . inferSolver

inferRigidTypeVars :: InferState -> Set InferenceVariable
inferRigidTypeVars = solverRigidTypeVars . inferSolver

inferDataTypes :: InferState -> Map ResolvedName DataTypeBinding
inferDataTypes = declarationDataTypes . inferDeclarations

inferClassFacts :: InferState -> Map CapabilityId ClassDefinition
inferClassFacts = scopeClassFacts . declarationCapabilities . inferDeclarations

inferClassMethodSignatures :: InferState -> Map CapabilityMethodKey ClassMethodType
inferClassMethodSignatures = scopeClassMethodSignatures . declarationCapabilities . inferDeclarations

inferConstructorWitnessNames :: InferState -> Map ResolvedName UnresolvedName
inferConstructorWitnessNames = inferenceConstructorWitnessNames . inferModule

inferVisibleTypes :: InferState -> TypeEnv
inferVisibleTypes = inferenceVisibleTypes . inferModule

inferDeferredExplicitConstraints :: InferState -> [DeferredExplicitConstraint]
inferDeferredExplicitConstraints = toList . outputDeferredConstraints . inferOutput

inferDeferredExplicitConstraintCount :: InferState -> Int
inferDeferredExplicitConstraintCount = Seq.length . outputDeferredConstraints . inferOutput

inferInferredClassConstraints :: InferState -> [TypeSchemeConstraint]
inferInferredClassConstraints = outputInferredConstraints . inferOutput

inferInferredClassConstraintCount :: InferState -> Int
inferInferredClassConstraintCount = outputInferredConstraintCount . inferOutput

inferErrorsRev :: InferState -> [Diagnostic]
inferErrorsRev = outputErrorsRev . inferOutput

inferErrorCount :: InferState -> Int
inferErrorCount = outputErrorCount . inferOutput

inferPatternCoverageSites :: InferState -> [PatternCoverageSite]
inferPatternCoverageSites = toList . outputPatternCoverageSites . inferOutput

reservePatternCoverageSite :: InferState -> (Int, InferState)
reservePatternCoverageSite state =
  ( ordinal,
    modifyInferenceOutput
      ( \output ->
          output
            { outputNextPatternCoverageOrdinal = ordinal + 1
            }
      )
      state
  )
  where
    ordinal = outputNextPatternCoverageOrdinal (inferOutput state)

recordPatternCoverageSite :: PatternCoverageSite -> InferState -> InferState
recordPatternCoverageSite site =
  modifyInferenceOutput
    ( \output ->
        output
          { outputPatternCoverageSites = outputPatternCoverageSites output Seq.|> site
          }
    )

-- | Return temporary semantic state separately from the continuing traversal.
-- Neither successful nor failed speculation may reuse its allocated IDs. A
-- successful preview can expose solved types, but never its node output or
-- outstanding constraints to a real definition-site generalization.
previewInference :: (InferState -> InferState) -> InferState -> (Maybe InferState, InferState)
previewInference check original =
  ( if inferErrorCount temporary == inferErrorCount original
      then Just temporary {inferOutput = inferOutput original}
      else Nothing,
    original {inferSolver = (inferSolver original) {solverNextTypeVar = max (inferNextTypeVar original) (inferNextTypeVar temporary)}}
  )
  where
    temporary = check original

-- | Rejected patterns contribute diagnostics in source order, but their
-- bindings, solved types, coverage sites and node facts are not accepted.
rejectPatternAttempt :: InferState -> InferState -> InferState
rejectPatternAttempt stable failed =
  stable {inferOutput = (inferOutput stable) {outputErrorsRev = inferErrorsRev failed, outputErrorCount = inferErrorCount failed}}
