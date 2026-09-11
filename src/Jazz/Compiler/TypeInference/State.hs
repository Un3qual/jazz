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
    inferExpressionFactTypes,
    inferBinaryOperations,
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
    inferPatternFactSeeds,
    inferRigidTypeVars,
    inferStrictEqualityVars,
    inferStatementFactSeeds,
    inferSubst,
    inferVisibleTypes,
    initialInferState,
    modifyDeclarationState,
    modifyInferenceOutput,
    modifyModuleInferenceState,
    recordExpressionFactType,
    recordBinaryOperation,
    recordExpressionEvidenceSeed,
    recordExplicitInstantiationSeed,
    recordPatternFactSeed,
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
import Data.Text (Text)
import Jazz.Compiler.CoreIdentity (CapabilityMethodKey)
import Jazz.Compiler.Diagnostics (Diagnostic)
import Jazz.Compiler.Name (ResolvedName, UnresolvedName)
import Jazz.Compiler.PatternCoverage (PatternCoverageSite)
import Jazz.Compiler.SemanticDeclarations (ConcreteImplFact, DeclarationVariable)
import Jazz.Compiler.SemanticFacts
  ( BinaryOperation,
    CapabilityId,
    CoreNodeId,
    ImplId,
    MethodId,
    PatternFacts,
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
  { inferenceModulePath :: Maybe [Text],
    inferenceLocalCapabilities :: ScopeCapabilityFacts,
    inferenceModuleCapabilities :: Map [Text] ScopeCapabilityFacts,
    inferenceDeclarationParameters :: Map InferenceVariable DeclarationVariable,
    inferenceConstructorWitnessNames :: Map ResolvedName UnresolvedName,
    inferenceVisibleTypes :: TypeEnv
  }
  deriving (Eq, Show)

data InferenceOutput = InferenceOutput
  { outputExpressionFactTypes :: Map CoreNodeId ExpressionType,
    outputBinaryOperations :: Map CoreNodeId BinaryOperation,
    outputExpressionEvidenceSeeds :: Map CoreNodeId ExpressionEvidenceSeed,
    outputExplicitInstantiationSeeds :: Map CoreNodeId ExplicitInstantiationSeed,
    outputPatternFactSeeds :: Map CoreNodeId PatternFacts,
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
          { outputExpressionFactTypes = Map.empty,
            outputBinaryOperations = Map.empty,
            outputExpressionEvidenceSeeds = Map.empty,
            outputExplicitInstantiationSeeds = Map.empty,
            outputPatternFactSeeds = Map.empty,
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

inferCurrentModulePath :: InferState -> Maybe [Text]
inferCurrentModulePath = inferenceModulePath . inferModule

inferCurrentModuleLocalCapabilityFacts :: InferState -> ScopeCapabilityFacts
inferCurrentModuleLocalCapabilityFacts = inferenceLocalCapabilities . inferModule

inferModuleCapabilityFacts :: InferState -> Map [Text] ScopeCapabilityFacts
inferModuleCapabilityFacts = inferenceModuleCapabilities . inferModule

inferConstructorWitnessNames :: InferState -> Map ResolvedName UnresolvedName
inferConstructorWitnessNames = inferenceConstructorWitnessNames . inferModule

inferVisibleTypes :: InferState -> TypeEnv
inferVisibleTypes = inferenceVisibleTypes . inferModule

inferExpressionFactTypes :: InferState -> Map CoreNodeId ExpressionType
inferExpressionFactTypes = outputExpressionFactTypes . inferOutput

inferBinaryOperations :: InferState -> Map CoreNodeId BinaryOperation
inferBinaryOperations = outputBinaryOperations . inferOutput

recordBinaryOperation :: CoreNodeId -> BinaryOperation -> InferState -> InferState
recordBinaryOperation nodeId operation =
  recordFact
    outputBinaryOperations
    (\facts output -> output {outputBinaryOperations = facts})
    DuplicateExpressionFacts
    nodeId
    operation

inferExpressionEvidenceSeeds :: InferState -> Map CoreNodeId ExpressionEvidenceSeed
inferExpressionEvidenceSeeds = outputExpressionEvidenceSeeds . inferOutput

inferExplicitInstantiationSeeds :: InferState -> Map CoreNodeId ExplicitInstantiationSeed
inferExplicitInstantiationSeeds = outputExplicitInstantiationSeeds . inferOutput

inferPatternFactSeeds :: InferState -> Map CoreNodeId PatternFacts
inferPatternFactSeeds = outputPatternFactSeeds . inferOutput

inferStatementFactSeeds :: InferState -> Map CoreNodeId ([(ResolvedName, TypeBinding)], StatementDeclarationFact)
inferStatementFactSeeds = outputStatementFactSeeds . inferOutput

inferFactInvariantFailures :: InferState -> [SemanticFactInvariantFailure]
inferFactInvariantFailures = toList . outputFactInvariantFailures . inferOutput

recordExpressionFactType :: CoreNodeId -> ExpressionType -> InferState -> InferState
recordExpressionFactType nodeId expressionType =
  recordFact
    outputExpressionFactTypes
    (\facts output -> output {outputExpressionFactTypes = facts})
    DuplicateExpressionFacts
    nodeId
    expressionType

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

recordPatternFactSeed :: CoreNodeId -> PatternFacts -> InferState -> InferState
recordPatternFactSeed nodeId facts =
  recordFact
    outputPatternFactSeeds
    (\seeds output -> output {outputPatternFactSeeds = seeds})
    DuplicatePatternFacts
    nodeId
    facts

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
