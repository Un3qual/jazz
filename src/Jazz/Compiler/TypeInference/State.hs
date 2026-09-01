{-# LANGUAGE DataKinds #-}

-- | Explicitly separated state for inference traversal and solver operations.
module Jazz.Compiler.TypeInference.State
  ( DeclarationState (..),
    DeferredExplicitConstraint (..),
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
    inferRuntimeHintPath,
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
    inferRuntimeTypeHints,
    inferStrictEqualityVars,
    inferSubst,
    inferVisibleTypes,
    initialInferState,
    modifyDeclarationState,
    modifyInferenceOutput,
    modifyModuleInferenceState,
    recordPatternCoverageSite,
    reservePatternCoverageSite,
  )
where

import Data.Foldable (toList)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Jazz.Compiler.AST (CorePhase (Resolved), SignatureType)
import Jazz.Compiler.CapabilityFacts (ConcreteImplFact)
import Jazz.Compiler.Diagnostics (Diagnostic)
import Jazz.Compiler.ModuleIdentity (ModulePath, preludeModulePath)
import Jazz.Compiler.Name (ResolvedName, UnresolvedName)
import Jazz.Compiler.PatternCoverage (PatternCoverageSite)
import Jazz.Compiler.RuntimeHints (BindingRuntimeHintKey)
import Jazz.Compiler.TypeInference.Types
  ( ClassMethodType,
    DataTypeBinding,
    ExpressionType,
    ImplMethodType,
    InferenceVariable,
    NumericConstraint,
    ScopeCapabilityFacts,
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
  { declarationDataTypes :: Map Text DataTypeBinding,
    declarationClassFacts :: Map Text Int,
    declarationGeneratedEqualityClassFacts :: Set Text,
    declarationConcreteImplFacts :: Set ConcreteImplFact,
    declarationClassMethodSignatures :: Map Text ClassMethodType,
    declarationConcreteImplMethods :: Map Text [ImplMethodType]
  }
  deriving (Eq, Show)

data ModuleInferenceState = ModuleInferenceState
  { moduleInferencePreludePath :: ModulePath,
    inferenceModulePath :: Maybe [Text],
    inferenceRuntimeHintPath :: Maybe [Text],
    inferenceLocalCapabilities :: ScopeCapabilityFacts,
    inferenceModuleCapabilities :: Map [Text] ScopeCapabilityFacts,
    inferenceConstructorWitnessNames :: Map ResolvedName UnresolvedName,
    inferenceVisibleTypes :: TypeEnv
  }
  deriving (Eq, Show)

data InferenceOutput = InferenceOutput
  { outputRuntimeHints :: Map BindingRuntimeHintKey (SignatureType 'Resolved),
    outputDeferredConstraints :: Seq DeferredExplicitConstraint,
    outputInferredConstraints :: [TypeSchemeConstraint],
    outputInferredConstraintCount :: Int,
    outputErrorsRev :: [Diagnostic],
    outputErrorCount :: Int,
    outputPatternCoverageSites :: Seq PatternCoverageSite,
    outputNextPatternCoverageOrdinal :: Int
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
  { deferredConstraintName :: Text,
    deferredMethodKey :: Maybe Text,
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
          { moduleInferencePreludePath = preludeModulePath,
            inferenceModulePath = Nothing,
            inferenceRuntimeHintPath = Nothing,
            inferenceLocalCapabilities = emptyScopeCapabilityFacts,
            inferenceModuleCapabilities = Map.empty,
            inferenceConstructorWitnessNames = Map.empty,
            inferenceVisibleTypes = Map.empty
          },
      inferOutput =
        InferenceOutput
          { outputRuntimeHints = Map.empty,
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

inferDataTypes :: InferState -> Map Text DataTypeBinding
inferDataTypes = declarationDataTypes . inferDeclarations

inferClassFacts :: InferState -> Map Text Int
inferClassFacts = declarationClassFacts . inferDeclarations

inferGeneratedEqualityClassFacts :: InferState -> Set Text
inferGeneratedEqualityClassFacts = declarationGeneratedEqualityClassFacts . inferDeclarations

inferConcreteImplFacts :: InferState -> Set ConcreteImplFact
inferConcreteImplFacts = declarationConcreteImplFacts . inferDeclarations

inferClassMethodSignatures :: InferState -> Map Text ClassMethodType
inferClassMethodSignatures = declarationClassMethodSignatures . inferDeclarations

inferConcreteImplMethods :: InferState -> Map Text [ImplMethodType]
inferConcreteImplMethods = declarationConcreteImplMethods . inferDeclarations

inferCurrentModulePath :: InferState -> Maybe [Text]
inferCurrentModulePath = inferenceModulePath . inferModule

inferRuntimeHintPath :: InferState -> Maybe [Text]
inferRuntimeHintPath = inferenceRuntimeHintPath . inferModule

inferCurrentModuleLocalCapabilityFacts :: InferState -> ScopeCapabilityFacts
inferCurrentModuleLocalCapabilityFacts = inferenceLocalCapabilities . inferModule

inferModuleCapabilityFacts :: InferState -> Map [Text] ScopeCapabilityFacts
inferModuleCapabilityFacts = inferenceModuleCapabilities . inferModule

inferConstructorWitnessNames :: InferState -> Map ResolvedName UnresolvedName
inferConstructorWitnessNames = inferenceConstructorWitnessNames . inferModule

inferVisibleTypes :: InferState -> TypeEnv
inferVisibleTypes = inferenceVisibleTypes . inferModule

inferRuntimeTypeHints :: InferState -> Map BindingRuntimeHintKey (SignatureType 'Resolved)
inferRuntimeTypeHints = outputRuntimeHints . inferOutput

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
