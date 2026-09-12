-- | Explicitly separated state for inference traversal and solver operations.
module Jazz.Compiler.TypeInference.State
  ( DeclarationState (..),
    DeferredExplicitConstraint (..),
    ExplicitInstantiationSeed (..),
    ExplicitInstantiationTarget (..),
    ExpressionEvidenceSeed (..),
    InferState (..),
    InferenceOutput (..),
    ModuleInferenceState (..),
    SolverState (..),
    inferClassFacts,
    inferClassMethodSignatures,
    inferConcreteImplFacts,
    inferConcreteImplMethods,
    inferConstructorWitnessNames,
    inferCurrentModuleLocalCapabilityFacts,
    inferCurrentModulePath,
    inferDataTypes,
    inferDeferredExplicitConstraintCount,
    inferDeferredExplicitConstraints,
    inferErrorCount,
    inferErrorsRev,
    inferGeneratedEqualityClassFacts,
    inferInferredClassConstraintCount,
    inferInferredClassConstraints,
    inferModuleCapabilityFacts,
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
    modifyModuleInferenceState,
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
import Jazz.Compiler.CoreIdentity (CapabilityMethodKey)
import Jazz.Compiler.Diagnostics (Diagnostic)
import Jazz.Compiler.ModuleIdentity (ModulePath)
import Jazz.Compiler.Name (ResolvedName, UnresolvedName)
import Jazz.Compiler.PatternCoverage (PatternCoverageSite)
import Jazz.Compiler.SemanticDeclarations (ConcreteImplFact, DeclarationVariable)
import Jazz.Compiler.SemanticFacts
  ( CapabilityId,
    ImplId,
    MethodId,
  )
import Jazz.Compiler.TypeInference.Types
  ( ClassMethodType,
    DataTypeBinding,
    ExpressionType,
    ImplMethodType,
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
  { inferenceModulePath :: Maybe ModulePath,
    inferenceLocalCapabilities :: ScopeCapabilityFacts,
    inferenceModuleCapabilities :: Map (Maybe ModulePath) ScopeCapabilityFacts,
    inferenceDeclarationParameters :: Map InferenceVariable DeclarationVariable,
    inferenceConstructorWitnessNames :: Map ResolvedName UnresolvedName,
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
    outputNextPatternCoverageOrdinal :: Int
  }
  deriving (Eq, Show)

data ExpressionEvidenceSeed = ExpressionEvidenceSeed
  { evidenceSeedCapability :: CapabilityId,
    evidenceSeedImplementation :: ImplId,
    evidenceSeedMethod :: MethodId,
    evidenceSeedType :: ExpressionType
  }

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
    deferredWasInferred :: Bool,
    deferredArgumentType :: ExpressionType,
    deferredVisibleFacts :: ScopeCapabilityFacts,
    deferredStructuralFacts :: ScopeCapabilityFacts
  }
  deriving (Eq, Show)

modifyDeclarationState :: (DeclarationState -> DeclarationState) -> InferState -> InferState
modifyDeclarationState update state =
  state {inferDeclarations = update (inferDeclarations state)}

modifyInferenceOutput :: (InferenceOutput -> InferenceOutput) -> InferState -> InferState
modifyInferenceOutput update state =
  state {inferOutput = update (inferOutput state)}

modifyModuleInferenceState :: (ModuleInferenceState -> ModuleInferenceState) -> InferState -> InferState
modifyModuleInferenceState update state =
  state {inferModule = update (inferModule state)}

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
          { inferenceModulePath = Nothing,
            inferenceLocalCapabilities = emptyScopeCapabilityFacts,
            inferenceModuleCapabilities = Map.empty,
            inferenceDeclarationParameters = Map.empty,
            inferenceConstructorWitnessNames = Map.empty,
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
            outputNextPatternCoverageOrdinal = 0
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

inferClassFacts :: InferState -> Map CapabilityId Int
inferClassFacts = scopeClassFacts . declarationCapabilities . inferDeclarations

inferGeneratedEqualityClassFacts :: InferState -> Set CapabilityId
inferGeneratedEqualityClassFacts = scopeGeneratedEqualityClassFacts . declarationCapabilities . inferDeclarations

inferConcreteImplFacts :: InferState -> Set ConcreteImplFact
inferConcreteImplFacts = scopeConcreteImplFacts . declarationCapabilities . inferDeclarations

inferClassMethodSignatures :: InferState -> Map CapabilityMethodKey ClassMethodType
inferClassMethodSignatures = scopeClassMethodSignatures . declarationCapabilities . inferDeclarations

inferConcreteImplMethods :: InferState -> Map CapabilityMethodKey [ImplMethodType]
inferConcreteImplMethods = scopeConcreteImplMethods . declarationCapabilities . inferDeclarations

inferCurrentModulePath :: InferState -> Maybe ModulePath
inferCurrentModulePath = inferenceModulePath . inferModule

inferCurrentModuleLocalCapabilityFacts :: InferState -> ScopeCapabilityFacts
inferCurrentModuleLocalCapabilityFacts = inferenceLocalCapabilities . inferModule

inferModuleCapabilityFacts :: InferState -> Map (Maybe ModulePath) ScopeCapabilityFacts
inferModuleCapabilityFacts = inferenceModuleCapabilities . inferModule

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
