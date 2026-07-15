{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Cycle-breaking runtime data shared by the evaluator and pure semantics.
module JazzNext.Compiler.Runtime.Types
  ( RuntimeFloatMetadata (..),
    RuntimeIntMetadata (..),
    RuntimeEvidence (..),
    runtimeEvidenceTarget,
    RuntimeMethodCandidate (..),
    DeferredHostScopeId (..),
    DeferredHostBindingKey (..),
    DeferredHostBindingState (..),
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
import JazzNext.Compiler.AST
  ( DataConstructorArgument,
    Expr,
    NumericType,
    SignaturePayload,
    SignatureType
  )
import JazzNext.Compiler.BuiltinCatalog (BuiltinSymbol)
import JazzNext.Compiler.Diagnostics
  ( Diagnostic,
    SourceSpan
  )
import JazzNext.Compiler.FractionalLiteral (FractionalLiteralSource)
import JazzNext.Compiler.Name (Name)
import JazzNext.Compiler.RuntimeHints (BindingRuntimeHintKey)
import JazzNext.Compiler.Runtime.Observation
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
  | DeferredHostBindingEvaluated (Either Diagnostic RuntimeValue)

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
    runtimeClosureParameter :: Name,
    runtimeClosureBody :: Expr,
    runtimeClosureTypeHint :: Maybe SignatureType,
    runtimeClosureModulePath :: Maybe [Text],
    runtimeClosureCallableIdentity :: RuntimeCallableIdentity
  }

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
  | VConstructor Name [Name] Name [DataConstructorArgument] [RuntimeValue]
  | VQualifiedMethod Text Text SignaturePayload [RuntimeMethodCandidate] [RuntimeValue]
  | VTyped SignatureType RuntimeValue
  | VExplicitTypeApplication SignatureType RuntimeValue
  | VRuntimeExplicitResultHints RuntimeExplicitResultHints RuntimeValue
  | VDeferredHostBinding
      DeferredHostBindingKey
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
      ( VConstructor leftTypeName leftTypeParameters leftName leftConstructorArguments leftArgs,
        VConstructor rightTypeName rightTypeParameters rightName rightConstructorArguments rightArgs
        )
          | constructorIsSaturated leftConstructorArguments leftArgs,
            constructorIsSaturated rightConstructorArguments rightArgs ->
              leftTypeName == rightTypeName
                && leftTypeParameters == rightTypeParameters
                && leftName == rightName
                && leftConstructorArguments == rightConstructorArguments
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
      VConstructor typeName _ constructorName constructorArguments capturedArgs ->
        "VConstructor " <> show typeName <> " " <> show constructorName <> " " <> show constructorArguments <> " " <> show capturedArgs
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

constructorIsSaturated :: [DataConstructorArgument] -> [RuntimeValue] -> Bool
constructorIsSaturated constructorArguments capturedArgs =
  length capturedArgs >= length constructorArguments
