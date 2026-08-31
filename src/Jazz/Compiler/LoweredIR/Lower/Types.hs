module Jazz.Compiler.LoweredIR.Lower.Types
  ( LoweredIRLoweringKind (..),
    LoweredIRLoweringDetail (..),
    LoweredIRLoweringFailure (..),
    RuntimeRequirements (..),
    LoweringState (..),
    ResultDestination (..),
    AmbientSlot (..),
    FunctionParameterShape (..),
    FunctionDeclaration (..),
    CaptureShape (..),
    FunctionShape (..),
    FunctionIndex (..),
    ManagedConstructorLayout (..),
    ConstructorTemplate (..),
    ManagedLayoutCatalog (..),
    LoweringAnalysis (..),
    loweredIntegerWidth,
    loweredFloatWidth,
  )
where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import Jazz.Compiler.LoweredIR
import Jazz.Compiler.LoweredIR.RuntimeServiceCatalog (RuntimeServiceKey)
import Jazz.Compiler.TypedCore
import Numeric.Natural (Natural)

loweredIntegerWidth :: Int -> Maybe LoweredIntegerWidth
loweredIntegerWidth bits =
  case bits of
    8 -> Just LoweredIntegerWidth8
    16 -> Just LoweredIntegerWidth16
    32 -> Just LoweredIntegerWidth32
    64 -> Just LoweredIntegerWidth64
    _ -> Nothing

loweredFloatWidth :: Int -> Maybe LoweredFloatWidth
loweredFloatWidth bits =
  case bits of
    16 -> Just LoweredFloatWidth16
    32 -> Just LoweredFloatWidth32
    64 -> Just LoweredFloatWidth64
    _ -> Nothing

data LoweredIRLoweringKind
  = LoweredIRUnsupportedProgram
  | LoweredIRUnsupportedModule
  | LoweredIRUnsupportedStatement
  | LoweredIRUnsupportedExpression
  | LoweredIRUnsupportedPattern
  | LoweredIRIncompletePatternCase
  | LoweredIRUnsupportedRepresentation
  | LoweredIRUnsupportedOperator
  | LoweredIRInvalidFunctionShape
  | LoweredIRDuplicateFunctionIdentity
  | LoweredIRDuplicateGeneratedIdentity
  | LoweredIRDuplicateParameterIdentity
  | LoweredIRCaptureUnsupported
  | LoweredIRRecursiveFunctionUnsupported
  | LoweredIRCallableValueUnsupported
  | LoweredIRCallArityUnsupported
  | LoweredIRNonLocalCallUnsupported
  deriving (Eq, Show)

data LoweredIRLoweringDetail
  = LoweredIRNoFailureDetail
  | LoweredIRRecipeFailureDetail TypedRepresentationRecipe
  | LoweredIROperatorFailureDetail TypedOperatorRef
  | LoweredIRNameFailureDetail TypedCoreName
  | LoweredIRGeneratedIdentityFailureDetail Text
  | LoweredIRArityFailureDetail Int Int
  deriving (Eq, Show)

data LoweredIRLoweringFailure
  = LoweredIRLoweringFailure
      TypedCoreValidationPath
      LoweredIRLoweringKind
      LoweredIRLoweringDetail
  deriving (Eq, Show)

data RuntimeRequirements = RuntimeRequirements
  { runtimeRequiresTextLayout :: Bool,
    runtimeRequiredServices :: Set.Set RuntimeServiceKey
  }
  deriving (Eq, Show)

instance Semigroup RuntimeRequirements where
  left <> right =
    RuntimeRequirements
      { runtimeRequiresTextLayout =
          runtimeRequiresTextLayout left || runtimeRequiresTextLayout right,
        runtimeRequiredServices =
          Set.union
            (runtimeRequiredServices left)
            (runtimeRequiredServices right)
      }

instance Monoid RuntimeRequirements where
  mempty = RuntimeRequirements False Set.empty

data LoweringState = LoweringState
  { loweringNextTemporary :: Int,
    loweringNextCarrier :: Int,
    loweringInstructions :: [LoweredInstruction],
    loweringCompletedBlocks :: [LoweredBlock],
    loweringCurrentBlockId :: LoweredBlockId,
    loweringCurrentBlockParameters :: [LoweredParameter],
    loweringLocalBindings :: Map.Map TypedBinderId LoweredOperand,
    loweringSharedEnvironments :: Map.Map LoweredLayoutId LoweredOperand,
    loweringCarriedOperands :: Map.Map Int LoweredOperand
  }

data ResultDestination
  = ProduceValue
  | FinishFunction LoweredRepresentation

data AmbientSlot
  = AmbientLocalSlot TypedBinderId LoweredRepresentation
  | AmbientSharedEnvironmentSlot LoweredLayoutId LoweredRepresentation
  | AmbientCarriedOperandSlot Int LoweredRepresentation

data FunctionParameterShape = FunctionParameterShape
  { functionParameterBinder :: TypedBinderId,
    functionParameter :: LoweredParameter
  }

data FunctionDeclaration = FunctionDeclaration
  { functionDeclarationBinder :: TypedBinderId,
    functionDeclarationName :: TypedCoreName,
    functionDeclarationStatementIndex :: Int
  }

data CaptureShape = CaptureShape
  { captureShapeBinder :: TypedBinderId,
    captureShapeRepresentation :: LoweredRepresentation
  }

data FunctionShape = FunctionShape
  { functionShapeBinder :: TypedBinderId,
    functionShapeName :: TypedCoreName,
    functionShapeCallableShape :: TypedCallableShape,
    functionShapeId :: LoweredFunctionId,
    functionShapeEnvironmentLayout :: Maybe LoweredLayoutId,
    functionShapeStatementIndex :: Int,
    functionShapeParameters :: [FunctionParameterShape],
    functionShapeCaptures :: [CaptureShape],
    functionShapeResultRepresentation :: LoweredRepresentation,
    functionShapeReversedBodyPath :: [Int],
    functionShapeBody :: TypedExpr,
    functionShapeSourceBinding :: Bool
  }

data FunctionIndex = FunctionIndex
  { indexedFunctionShapes :: Map.Map TypedBinderId FunctionShape,
    indexedFunctionShapesByStatement :: Map.Map Int FunctionShape,
    indexedRecursiveGroupMembers :: Map.Map TypedBinderId [TypedBinderId],
    indexedScalarRepresentations :: Map.Map TypedBinderId LoweredRepresentation,
    indexedManagedLayoutCatalog :: ManagedLayoutCatalog
  }

data ManagedConstructorLayout = ManagedConstructorLayout
  { managedConstructorLayoutId :: LoweredLayoutId,
    managedConstructorTag :: Natural,
    managedConstructorFields :: [LoweredRepresentation]
  }
  deriving (Eq, Show)

data ConstructorTemplate = ConstructorTemplate
  { constructorTemplateDataName :: TypedCoreName,
    constructorTemplateParameters :: [TypedTypeParameterId],
    constructorTemplateTag :: Natural,
    constructorTemplateFieldRecipes :: [TypedRepresentationRecipe]
  }

data ManagedLayoutCatalog = ManagedLayoutCatalog
  { catalogModulePath :: [Text],
    catalogConstructors :: Map.Map TypedBinderId ConstructorTemplate,
    catalogLayoutShapes :: Map.Map LoweredLayoutId LoweredLayoutShape,
    catalogLayouts :: [LoweredLayout]
  }

data LoweringAnalysis = LoweringAnalysis
  { analyzedModulePath :: [Text],
    analyzedStatements :: [TypedStatement],
    analyzedFunctionShapes :: [FunctionShape],
    analyzedFunctionIndex :: FunctionIndex,
    analyzedResultRepresentation :: LoweredRepresentation,
    analyzedRuntimeRequirements :: RuntimeRequirements
  }
