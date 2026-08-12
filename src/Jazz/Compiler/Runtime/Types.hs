{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

-- | Cycle-breaking runtime data shared by the evaluator and pure semantics.
module Jazz.Compiler.Runtime.Types
  ( RuntimeFloatMetadata (..),
    RuntimeIntMetadata (..),
    RuntimeEvidence (..),
    runtimeEvidenceTarget,
    RuntimeMethodCandidate (..),
    DeferredHostScopeId (..),
    DeferredHostBindingKey (..),
    DeferredHostBindingState (..),
    RuntimeControl (..),
    RuntimeHostEvaluationState (..),
    RuntimeHostEvaluationT,
    RuntimeExplicitResultHints,
    RuntimeClosure (..),
    RuntimeValue
      ( VInt,
        VFloat,
        VBool,
        VChar,
        VText,
        VList,
        VTuple,
        VClosure,
        VBuiltin,
        VOperator,
        VSectionLeft,
        VSectionRight,
        VConstructor,
        VConstructorApplication,
        VQualifiedMethod,
        VTyped,
        VExplicitTypeApplication,
        VDeferredHostBinding
      ),
    data VExplicitResultHints,
    prependRuntimeExplicitResultHint,
    attachRuntimeExplicitResultHints,
    runtimeExplicitResultHintsView,
    runtimeExplicitResultHintsInOrder,
    foldRuntimeExplicitResultHints,
    RuntimeConstructorArguments,
    RuntimeConstructorShape,
    appendRuntimeConstructorArgument,
    constructorApplicationIsSaturated,
    foldrRuntimeConstructorArguments,
    runtimeConstructorArgumentCount,
    runtimeConstructorArity,
    runtimeConstructorFieldTypes,
    runtimeConstructorName,
    runtimeConstructorTypeName,
    runtimeConstructorTypeParameters,
    constructorIsSaturated,
    RuntimeCell,
    RuntimeEnv,
    ScopeResult (..),
    ModuleEvaluationMode (..)
  ) where

import Control.Monad.Trans.State.Strict (StateT)
import qualified Data.Foldable as Foldable
import Data.Map.Strict (Map)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import Data.Word (Word64)
import Jazz.Compiler.AST
  ( Expr,
    NumericType,
    SignaturePayload,
    SignatureType
  )
import Jazz.Compiler.BuiltinCatalog (BuiltinSymbol)
import Jazz.Compiler.Diagnostics
  ( Diagnostic,
    SourceSpan
  )
import Jazz.Compiler.FractionalLiteral (FractionalLiteralSource)
import Jazz.Compiler.Name (Name)
import Jazz.Compiler.RecursiveBindings (LambdaCaptureHints)
import Jazz.Compiler.RuntimeHints (BindingRuntimeHintKey)
import Jazz.Compiler.Runtime.Observation
  ( RuntimeCallableIdentity,
    RuntimeObservationState,
  )

data RuntimeFloatMetadata = RuntimeFloatMetadata
  { runtimeFloatLiteralSource :: Maybe FractionalLiteralSource,
    runtimeFloatTargetType :: Maybe NumericType
  }
  deriving (Eq, Show)

data RuntimeIntMetadata = RuntimeIntMetadata
  { runtimeIntTargetType :: Maybe NumericType
  }
  deriving (Eq, Show)

data RuntimeEvidence = RuntimeEvidence Text SignatureType (Maybe Text)
  deriving (Eq, Show)

runtimeEvidenceTarget :: RuntimeEvidence -> SignatureType
runtimeEvidenceTarget (RuntimeEvidence _ implTarget _) = implTarget

data RuntimeMethodCandidate = RuntimeMethodCandidate RuntimeEvidence (Either Diagnostic RuntimeValue)

-- | Ordered explicit result obligations attached to one runtime value. The
-- constructor stays private so callers cannot reintroduce nested hint wrappers.
-- Hints are stored outermost-to-innermost, matching source evaluation order.
newtype RuntimeExplicitResultHints = RuntimeExplicitResultHints (Seq SignatureType)
  deriving (Eq, Show)

newtype DeferredHostScopeId = DeferredHostScopeId Int
  deriving (Eq, Ord, Show)

data DeferredHostBindingKey = DeferredHostBindingKey DeferredHostScopeId (Maybe [Text]) SourceSpan Name
  deriving (Eq, Ord, Show)

data DeferredHostBindingState
  = DeferredHostBindingEvaluating
  | DeferredHostBindingEvaluated (Either RuntimeControl RuntimeValue)

-- | Interpreter-internal non-local control. Runtime diagnostics and requested
-- process exits share the evaluator's unwind path without conflating exit with
-- an error visible to Jazz programs.
data RuntimeControl
  = RuntimeDiagnostic Diagnostic
  | RuntimeExitRequested Integer

data RuntimeHostEvaluationState = RuntimeHostEvaluationState
  { runtimeHostEvaluationBindingCache :: Map DeferredHostBindingKey DeferredHostBindingState,
    runtimeHostEvaluationNextScopeId :: Int,
    runtimeHostEvaluationActiveMachineCount :: Int,
    runtimeHostEvaluationContinuationDepth :: Word64,
    runtimeHostEvaluationObservation :: RuntimeObservationState
  }

type RuntimeHostEvaluationT m = StateT RuntimeHostEvaluationState m

data RuntimeClosure = RuntimeClosure
  { runtimeClosureEnvironment :: RuntimeEnv,
    runtimeClosureEnvironmentMayReachHostCells :: Bool,
    runtimeClosureLambdaCaptureHints :: LambdaCaptureHints,
    runtimeClosureParameter :: Name,
    runtimeClosureBody :: Expr,
    runtimeClosureTypeHint :: Maybe SignatureType,
    runtimeClosureModulePath :: Maybe [Text],
    runtimeClosureCallableIdentity :: RuntimeCallableIdentity
  }

-- | Constructor metadata shared by every partial application. Its constructor
-- stays private so the cached arity cannot disagree with the field types.
data RuntimeConstructorShape = RuntimeConstructorShape Name [Name] Name !Int [SignatureType]
  deriving (Eq)

-- | Append-efficient constructor arguments. 'Seq.length' is constant time, so
-- keeping a second cached count would only duplicate an invariant.
newtype RuntimeConstructorArguments = RuntimeConstructorArguments (Seq RuntimeValue)
  deriving (Eq)

data RuntimeValue
  = VInt Integer RuntimeIntMetadata
  | VFloat Double RuntimeFloatMetadata
  | VBool Bool
  | VChar Char
  | VText Text
  | VList [RuntimeValue] (Maybe SignatureType)
  | VTuple [RuntimeValue]
  | VClosure RuntimeClosure
  | VBuiltin BuiltinSymbol [RuntimeValue]
  | VOperator Text [RuntimeValue]
  | VSectionLeft Text RuntimeValue
  | VSectionRight Text RuntimeValue
  | VConstructorState RuntimeConstructorShape RuntimeConstructorArguments
  | VQualifiedMethod Text Text SignaturePayload [RuntimeMethodCandidate] [RuntimeValue]
  | VTyped SignatureType RuntimeValue
  | VExplicitTypeApplication SignatureType RuntimeValue
  | VRuntimeExplicitResultHints RuntimeExplicitResultHints RuntimeValue
  | VDeferredHostBinding
      DeferredHostBindingKey
      Diagnostic
      (Maybe [Text])
      Expr
      RuntimeEnv
      (Map BindingRuntimeHintKey SignatureType)
      (Maybe NumericType)
      (Maybe SignatureType)

instance Eq RuntimeValue where
  leftValue == rightValue =
    case (leftValue, rightValue) of
      (VTyped _ leftInner, rightInner) -> leftInner == rightInner
      (leftInner, VTyped _ rightInner) -> leftInner == rightInner
      (VExplicitTypeApplication _ leftInner, rightInner) -> leftInner == rightInner
      (leftInner, VExplicitTypeApplication _ rightInner) -> leftInner == rightInner
      (VRuntimeExplicitResultHints _ leftInner, rightInner) -> leftInner == rightInner
      (leftInner, VRuntimeExplicitResultHints _ rightInner) -> leftInner == rightInner
      (VInt leftInt _, VInt rightInt _) -> leftInt == rightInt
      (VFloat leftFloat _, VFloat rightFloat _) -> leftFloat == rightFloat
      (VBool leftBool, VBool rightBool) -> leftBool == rightBool
      (VChar leftChar, VChar rightChar) -> leftChar == rightChar
      (VText leftText, VText rightText) -> leftText == rightText
      (VList leftElements _, VList rightElements _) -> leftElements == rightElements
      (VTuple leftElements, VTuple rightElements) -> leftElements == rightElements
      ( VConstructorApplication leftShape leftArgs,
        VConstructorApplication rightShape rightArgs
        )
          | constructorApplicationIsSaturated leftShape leftArgs,
            constructorApplicationIsSaturated rightShape rightArgs ->
              leftShape == rightShape
                && leftArgs == rightArgs
      _ -> False

instance Eq RuntimeMethodCandidate where
  RuntimeMethodCandidate leftEvidence leftCell == RuntimeMethodCandidate rightEvidence rightCell =
    leftEvidence == rightEvidence && leftCell == rightCell

instance Show RuntimeValue where
  show value =
    case value of
      VInt intValue _ -> "VInt " <> show intValue
      VFloat floatValue _ -> "VFloat " <> show floatValue
      VBool boolValue -> "VBool " <> show boolValue
      VChar charValue -> "VChar " <> show charValue
      VText textValue -> "VText " <> show textValue
      VList elements maybeTypeHint -> "VList " <> show elements <> " " <> show maybeTypeHint
      VTuple elements -> "VTuple " <> show elements
      VClosure closure ->
        "VClosure <env> "
          <> show (runtimeClosureParameter closure)
          <> " "
          <> show (runtimeClosureBody closure)
          <> " "
          <> show (runtimeClosureTypeHint closure)
          <> " "
          <> show (runtimeClosureModulePath closure)
      VBuiltin builtinSymbol capturedArgs ->
        "VBuiltin " <> show builtinSymbol <> " " <> show capturedArgs
      VOperator operatorSymbol capturedArgs ->
        "VOperator " <> show operatorSymbol <> " " <> show capturedArgs
      VSectionLeft operatorSymbol operand ->
        "VSectionLeft " <> show operatorSymbol <> " " <> show operand
      VSectionRight operatorSymbol operand ->
        "VSectionRight " <> show operatorSymbol <> " " <> show operand
      VConstructorState shape capturedArgs ->
        "VConstructor "
          <> show (runtimeConstructorTypeName shape)
          <> " "
          <> show (runtimeConstructorName shape)
          <> " "
          <> show (runtimeConstructorFieldTypes shape)
          <> " "
          <> show (runtimeConstructorArgumentsInOrder capturedArgs)
      VQualifiedMethod methodKey _ _ candidates capturedArgs ->
        "VQualifiedMethod " <> show methodKey <> " " <> show candidates <> " " <> show capturedArgs
      VTyped typeHint innerValue ->
        "VTyped " <> show typeHint <> " " <> show innerValue
      VExplicitTypeApplication typeHint innerValue ->
        "VExplicitTypeApplication " <> show typeHint <> " " <> show innerValue
      VRuntimeExplicitResultHints hints innerValue ->
        "VExplicitResultHints " <> show hints <> " " <> show innerValue
      VDeferredHostBinding {} -> "VDeferredHostBinding <thunk>"

-- | Match an explicit-result-hint wrapper without exposing a constructor that
-- could be used to build nested wrappers.
pattern VExplicitResultHints :: RuntimeExplicitResultHints -> RuntimeValue -> RuntimeValue
pattern VExplicitResultHints hints innerValue <- VRuntimeExplicitResultHints hints innerValue

-- | Historical ordered-list constructor view used by runtime semantics and
-- tests. Construction establishes the shape and argument invariants once.
pattern VConstructor :: Name -> [Name] -> Name -> [SignatureType] -> [RuntimeValue] -> RuntimeValue
pattern VConstructor typeName typeParameters constructorName fieldTypes capturedArgs <-
  VConstructorState
    (RuntimeConstructorShape typeName typeParameters constructorName _ fieldTypes)
    (runtimeConstructorArgumentsInOrder -> capturedArgs)
  where
    VConstructor typeName typeParameters constructorName fieldTypes capturedArgs =
      VConstructorState
        (runtimeConstructorShape typeName typeParameters constructorName fieldTypes)
        (runtimeConstructorArgumentsFromList capturedArgs)

-- | Evaluator view that keeps the invariant-owning shape and append-efficient
-- arguments intact between curried applications. Callers can reuse a shape,
-- but cannot forge its cached arity.
pattern VConstructorApplication :: RuntimeConstructorShape -> RuntimeConstructorArguments -> RuntimeValue
pattern VConstructorApplication shape capturedArgs =
  VConstructorState shape capturedArgs

{-# COMPLETE
  VInt,
  VFloat,
  VBool,
  VChar,
  VText,
  VList,
  VTuple,
  VClosure,
  VBuiltin,
  VOperator,
  VSectionLeft,
  VSectionRight,
  VConstructor,
  VQualifiedMethod,
  VTyped,
  VExplicitTypeApplication,
  VExplicitResultHints,
  VDeferredHostBinding
  #-}

{-# COMPLETE
  VInt,
  VFloat,
  VBool,
  VChar,
  VText,
  VList,
  VTuple,
  VClosure,
  VBuiltin,
  VOperator,
  VSectionLeft,
  VSectionRight,
  VConstructorApplication,
  VQualifiedMethod,
  VTyped,
  VExplicitTypeApplication,
  VExplicitResultHints,
  VDeferredHostBinding
  #-}

prependRuntimeExplicitResultHint :: SignatureType -> RuntimeValue -> RuntimeValue
prependRuntimeExplicitResultHint typeHint runtimeValue =
  case runtimeValue of
    VRuntimeExplicitResultHints (RuntimeExplicitResultHints innerHints) innerValue ->
      VRuntimeExplicitResultHints
        (RuntimeExplicitResultHints (typeHint Seq.<| innerHints))
        innerValue
    _ ->
      VRuntimeExplicitResultHints
        (RuntimeExplicitResultHints (Seq.singleton typeHint))
        runtimeValue

attachRuntimeExplicitResultHints :: RuntimeExplicitResultHints -> RuntimeValue -> RuntimeValue
attachRuntimeExplicitResultHints (RuntimeExplicitResultHints outerHints) runtimeValue =
  case runtimeValue of
    VRuntimeExplicitResultHints (RuntimeExplicitResultHints innerHints) innerValue ->
      VRuntimeExplicitResultHints
        (RuntimeExplicitResultHints (outerHints Seq.>< innerHints))
        innerValue
    _ ->
      VRuntimeExplicitResultHints
        (RuntimeExplicitResultHints outerHints)
        runtimeValue

runtimeExplicitResultHintsView :: RuntimeValue -> Maybe (RuntimeExplicitResultHints, RuntimeValue)
runtimeExplicitResultHintsView runtimeValue =
  case runtimeValue of
    VRuntimeExplicitResultHints hints innerValue -> Just (hints, innerValue)
    _ -> Nothing

runtimeExplicitResultHintsInOrder :: RuntimeValue -> [SignatureType]
runtimeExplicitResultHintsInOrder runtimeValue =
  case runtimeExplicitResultHintsView runtimeValue of
    Just (RuntimeExplicitResultHints hints, _) -> Foldable.toList hints
    Nothing -> []

foldRuntimeExplicitResultHints ::
  (accumulator -> SignatureType -> accumulator) ->
  accumulator ->
  RuntimeExplicitResultHints ->
  accumulator
foldRuntimeExplicitResultHints step initial (RuntimeExplicitResultHints hints) =
  Foldable.foldl' step initial hints

instance Show RuntimeMethodCandidate where
  show (RuntimeMethodCandidate evidence _) =
    "RuntimeMethodCandidate " <> show evidence

type RuntimeCell = Either Diagnostic RuntimeValue

type RuntimeEnv = Map Name RuntimeCell

data ScopeResult = ScopeResult
  { scopeResultEnvironment :: RuntimeEnv,
    scopeResultValue :: Maybe RuntimeValue,
    scopeResultEnvironmentMayReachHostCells :: Bool
  }

data ModuleEvaluationMode
  = EvaluateDependencyModule
  | EvaluateEntryModule
  deriving (Eq, Show)

constructorIsSaturated :: [SignatureType] -> [RuntimeValue] -> Bool
constructorIsSaturated fieldTypes capturedArgs =
  length capturedArgs >= length fieldTypes

runtimeConstructorArgumentsFromList :: [RuntimeValue] -> RuntimeConstructorArguments
runtimeConstructorArgumentsFromList capturedArgs =
  RuntimeConstructorArguments (Seq.fromList capturedArgs)

runtimeConstructorArgumentsInOrder :: RuntimeConstructorArguments -> [RuntimeValue]
runtimeConstructorArgumentsInOrder (RuntimeConstructorArguments capturedArgs) =
  Foldable.toList capturedArgs

runtimeConstructorArgumentCount :: RuntimeConstructorArguments -> Int
runtimeConstructorArgumentCount (RuntimeConstructorArguments capturedArgs) =
  Seq.length capturedArgs

appendRuntimeConstructorArgument :: RuntimeValue -> RuntimeConstructorArguments -> RuntimeConstructorArguments
appendRuntimeConstructorArgument argumentValue (RuntimeConstructorArguments capturedArgs) =
  RuntimeConstructorArguments (capturedArgs Seq.|> argumentValue)

foldrRuntimeConstructorArguments ::
  (RuntimeValue -> accumulator -> accumulator) ->
  accumulator ->
  RuntimeConstructorArguments ->
  accumulator
foldrRuntimeConstructorArguments step initial (RuntimeConstructorArguments capturedArgs) =
  Foldable.foldr step initial capturedArgs

constructorApplicationIsSaturated :: RuntimeConstructorShape -> RuntimeConstructorArguments -> Bool
constructorApplicationIsSaturated shape capturedArgs =
  runtimeConstructorArgumentCount capturedArgs >= runtimeConstructorArity shape

runtimeConstructorShape :: Name -> [Name] -> Name -> [SignatureType] -> RuntimeConstructorShape
runtimeConstructorShape typeName typeParameters constructorName fieldTypes =
  RuntimeConstructorShape typeName typeParameters constructorName (length fieldTypes) fieldTypes

runtimeConstructorTypeName :: RuntimeConstructorShape -> Name
runtimeConstructorTypeName (RuntimeConstructorShape typeName _ _ _ _) = typeName

runtimeConstructorTypeParameters :: RuntimeConstructorShape -> [Name]
runtimeConstructorTypeParameters (RuntimeConstructorShape _ typeParameters _ _ _) = typeParameters

runtimeConstructorName :: RuntimeConstructorShape -> Name
runtimeConstructorName (RuntimeConstructorShape _ _ constructorName _ _) = constructorName

runtimeConstructorArity :: RuntimeConstructorShape -> Int
runtimeConstructorArity (RuntimeConstructorShape _ _ _ arity _) = arity

runtimeConstructorFieldTypes :: RuntimeConstructorShape -> [SignatureType]
runtimeConstructorFieldTypes (RuntimeConstructorShape _ _ _ _ fieldTypes) = fieldTypes
