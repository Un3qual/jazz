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
    inferCurrentModuleLocalCapabilityFacts,
    inferCurrentModulePath,
    inferRuntimeHintPath,
    inferDataTypes,
    inferDeferredExplicitConstraints,
    inferErrorCount,
    inferErrorsRev,
    inferGeneratedEqualityClassFacts,
    inferInferredClassConstraints,
    inferModuleCapabilityFacts,
    inferNextTypeVar,
    inferNumericVars,
    inferRigidTypeVars,
    inferRuntimeTypeHints,
    inferStrictEqualityVars,
    inferSubst,
    inferVisibleTypes,
    initialInferState,
    modifyDeclarationState,
    modifyInferenceOutput,
    modifyModuleInferenceState
  ) where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Jazz.Compiler.AST (SignatureType)
import Jazz.Compiler.Diagnostics (Diagnostic)
import Jazz.Compiler.RuntimeHints (BindingRuntimeHintKey)
import Jazz.Compiler.TypeInference.Types
  ( ClassMethodType,
    DataTypeBinding,
    ExpressionType,
    ImplMethodType,
    NumericConstraint,
    ScopeCapabilityFacts,
    TypeEnv,
    TypeSchemeConstraint,
    emptyScopeCapabilityFacts
  )

data SolverState = SolverState
  { solverNextTypeVar :: Int,
    solverSubstitution :: IntMap ExpressionType,
    solverStrictEqualityVars :: Set Int,
    solverNumericVars :: Map Int NumericConstraint,
    solverRigidTypeVars :: Set Int
  }
  deriving (Eq, Show)

data DeclarationState = DeclarationState
  { declarationDataTypes :: Map Text DataTypeBinding,
    declarationClassFacts :: Map Text Int,
    declarationGeneratedEqualityClassFacts :: Set Text,
    declarationConcreteImplFacts :: Set Text,
    declarationClassMethodSignatures :: Map Text ClassMethodType,
    declarationConcreteImplMethods :: Map Text [ImplMethodType]
  }
  deriving (Eq, Show)

data ModuleInferenceState = ModuleInferenceState
  { inferenceModulePath :: Maybe [Text],
    -- Standalone prelude statements use the same synthetic path as compiled preludes.
    inferenceRuntimeHintPath :: Maybe [Text],
    inferenceLocalCapabilities :: ScopeCapabilityFacts,
    inferenceModuleCapabilities :: Map [Text] ScopeCapabilityFacts,
    inferenceVisibleTypes :: TypeEnv
  }
  deriving (Eq, Show)

data InferenceOutput = InferenceOutput
  { outputRuntimeHints :: Map BindingRuntimeHintKey SignatureType,
    outputDeferredConstraints :: [DeferredExplicitConstraint],
    outputInferredConstraints :: [TypeSchemeConstraint],
    outputErrorsRev :: [Diagnostic],
    outputErrorCount :: Int
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
            solverSubstitution = IntMap.empty,
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
            inferenceRuntimeHintPath = Nothing,
            inferenceLocalCapabilities = emptyScopeCapabilityFacts,
            inferenceModuleCapabilities = Map.empty,
            inferenceVisibleTypes = Map.empty
          },
      inferOutput =
        InferenceOutput
          { outputRuntimeHints = Map.empty,
            outputDeferredConstraints = [],
            outputInferredConstraints = [],
            outputErrorsRev = [],
            outputErrorCount = 0
          }
    }

inferNextTypeVar :: InferState -> Int
inferNextTypeVar = solverNextTypeVar . inferSolver

inferSubst :: InferState -> IntMap ExpressionType
inferSubst = solverSubstitution . inferSolver

inferStrictEqualityVars :: InferState -> Set Int
inferStrictEqualityVars = solverStrictEqualityVars . inferSolver

inferNumericVars :: InferState -> Map Int NumericConstraint
inferNumericVars = solverNumericVars . inferSolver

inferRigidTypeVars :: InferState -> Set Int
inferRigidTypeVars = solverRigidTypeVars . inferSolver

inferDataTypes :: InferState -> Map Text DataTypeBinding
inferDataTypes = declarationDataTypes . inferDeclarations

inferClassFacts :: InferState -> Map Text Int
inferClassFacts = declarationClassFacts . inferDeclarations

inferGeneratedEqualityClassFacts :: InferState -> Set Text
inferGeneratedEqualityClassFacts = declarationGeneratedEqualityClassFacts . inferDeclarations

inferConcreteImplFacts :: InferState -> Set Text
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

inferVisibleTypes :: InferState -> TypeEnv
inferVisibleTypes = inferenceVisibleTypes . inferModule

inferRuntimeTypeHints :: InferState -> Map BindingRuntimeHintKey SignatureType
inferRuntimeTypeHints = outputRuntimeHints . inferOutput

inferDeferredExplicitConstraints :: InferState -> [DeferredExplicitConstraint]
inferDeferredExplicitConstraints = outputDeferredConstraints . inferOutput

inferInferredClassConstraints :: InferState -> [TypeSchemeConstraint]
inferInferredClassConstraints = outputInferredConstraints . inferOutput

inferErrorsRev :: InferState -> [Diagnostic]
inferErrorsRev = outputErrorsRev . inferOutput

inferErrorCount :: InferState -> Int
inferErrorCount = outputErrorCount . inferOutput
