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
    inferExpressionEvidenceSeeds,
    inferExplicitInstantiationSeeds,
    inferFactInvariantFailures,
    inferGeneratedEqualityClassFacts,
    inferInferredClassConstraintCount,
    inferInferredClassConstraints,
    inferModuleCapabilityFacts,
    inferNextTypeVar,
    inferNumericVars,
    inferPatternCoverageSites,
    inferRigidTypeVars,
    inferStrictEqualityVars,
    inferStatementFactSeeds,
    inferSubst,
    inferVisibleTypes,
    initialInferState,
    previewInference,
    rejectPatternAttempt,
    modifyDeclarationState,
    modifyInferenceOutput,
    modifyModuleInferenceState,
    recordExpressionEvidenceSeed,
    recordExplicitInstantiationSeed,
    recordStatementFactSeed,
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
    CoreNodeId,
    ImplId,
    MethodId,
    SemanticFactInvariantFailure (..),
    StatementDeclarationFact,
  )
import Jazz.Compiler.TypeInference.Types
  ( ClassMethodType,
    DataTypeBinding,
    ExpressionType,
    ImplMethodType,
    InferenceVariable,
    NumericConstraint,
    ScopeCapabilityFacts,
    TypeBinding,
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
    declarationClassFacts :: Map CapabilityId Int,
    declarationGeneratedEqualityClassFacts :: Set CapabilityId,
    declarationConcreteImplFacts :: Set ConcreteImplFact,
    declarationClassMethodSignatures :: Map CapabilityMethodKey ClassMethodType,
    declarationConcreteImplMethods :: Map CapabilityMethodKey [ImplMethodType]
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
  { outputExpressionEvidenceSeeds :: Map CoreNodeId ExpressionEvidenceSeed,
    outputExplicitInstantiationSeeds :: Map CoreNodeId ExplicitInstantiationSeed,
    outputStatementFactSeeds :: Map CoreNodeId ([(ResolvedName, TypeBinding)], StatementDeclarationFact),
    outputFactInvariantFailures :: Seq SemanticFactInvariantFailure,
    outputDeferredConstraints :: Seq DeferredExplicitConstraint,
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
  deriving (Eq, Show)

data ExplicitInstantiationTarget
  = ExplicitBinderInstantiation ResolvedName
  | ExplicitQualifiedMethodInstantiation ResolvedName
  deriving (Eq, Show)

data ExplicitInstantiationSeed = ExplicitInstantiationSeed
  { explicitInstantiationSeedTarget :: ExplicitInstantiationTarget,
    explicitInstantiationSeedArguments :: NonEmpty ExpressionType
  }
  deriving (Eq, Show)

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
            declarationClassFacts = Map.empty,
            declarationGeneratedEqualityClassFacts = Set.empty,
            declarationConcreteImplFacts = Set.empty,
            declarationClassMethodSignatures = Map.empty,
            declarationConcreteImplMethods = Map.empty
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
          { outputExpressionEvidenceSeeds = Map.empty,
            outputExplicitInstantiationSeeds = Map.empty,
            outputStatementFactSeeds = Map.empty,
            outputFactInvariantFailures = Seq.empty,
            outputDeferredConstraints = Seq.empty,
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
inferClassFacts = declarationClassFacts . inferDeclarations

inferGeneratedEqualityClassFacts :: InferState -> Set CapabilityId
inferGeneratedEqualityClassFacts = declarationGeneratedEqualityClassFacts . inferDeclarations

inferConcreteImplFacts :: InferState -> Set ConcreteImplFact
inferConcreteImplFacts = declarationConcreteImplFacts . inferDeclarations

inferClassMethodSignatures :: InferState -> Map CapabilityMethodKey ClassMethodType
inferClassMethodSignatures = declarationClassMethodSignatures . inferDeclarations

inferConcreteImplMethods :: InferState -> Map CapabilityMethodKey [ImplMethodType]
inferConcreteImplMethods = declarationConcreteImplMethods . inferDeclarations

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

inferExpressionEvidenceSeeds :: InferState -> Map CoreNodeId ExpressionEvidenceSeed
inferExpressionEvidenceSeeds = outputExpressionEvidenceSeeds . inferOutput

inferExplicitInstantiationSeeds :: InferState -> Map CoreNodeId ExplicitInstantiationSeed
inferExplicitInstantiationSeeds = outputExplicitInstantiationSeeds . inferOutput

inferStatementFactSeeds :: InferState -> Map CoreNodeId ([(ResolvedName, TypeBinding)], StatementDeclarationFact)
inferStatementFactSeeds = outputStatementFactSeeds . inferOutput

inferFactInvariantFailures :: InferState -> [SemanticFactInvariantFailure]
inferFactInvariantFailures = toList . outputFactInvariantFailures . inferOutput

recordExpressionEvidenceSeed :: CoreNodeId -> ExpressionEvidenceSeed -> InferState -> InferState
recordExpressionEvidenceSeed nodeId seed =
  recordFact
    outputExpressionEvidenceSeeds
    (\seeds output -> output {outputExpressionEvidenceSeeds = seeds})
    DuplicateExpressionFacts
    nodeId
    seed

recordExplicitInstantiationSeed :: CoreNodeId -> ExplicitInstantiationSeed -> InferState -> InferState
recordExplicitInstantiationSeed nodeId seed =
  recordFact
    outputExplicitInstantiationSeeds
    (\seeds output -> output {outputExplicitInstantiationSeeds = seeds})
    DuplicateExplicitInstantiationSeed
    nodeId
    seed

recordStatementFactSeed :: CoreNodeId -> ([(ResolvedName, TypeBinding)], StatementDeclarationFact) -> InferState -> InferState
recordStatementFactSeed nodeId facts =
  recordFact
    outputStatementFactSeeds
    (\seeds output -> output {outputStatementFactSeeds = seeds})
    DuplicateStatementFacts
    nodeId
    facts

recordFact :: (Ord key) => (InferenceOutput -> Map key value) -> (Map key value -> InferenceOutput -> InferenceOutput) -> (key -> SemanticFactInvariantFailure) -> key -> value -> InferState -> InferState
recordFact project replace duplicateFailure key value =
  modifyInferenceOutput $ \output ->
    if Map.member key (project output)
      then
        output
          { outputFactInvariantFailures =
              outputFactInvariantFailures output Seq.|> duplicateFailure key
          }
      else replace (Map.insert key value (project output)) output

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
