{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
        VDeclaredOperatorRightSection,
        VConstructor,
        VConstructorApplication,
        VQualifiedMethod,
        VTyped,
        VExplicitTypeApplication,
        VDeferredHostBinding
      ),
    pattern VExplicitResultHints,
    pattern VQualifiedMethodApplication,
    prependRuntimeExplicitResultHint,
    attachRuntimeExplicitResultHints,
    runtimeExplicitResultHintsView,
    runtimeExplicitResultHintsInOrder,
    foldRuntimeExplicitResultHints,
    RuntimeAppliedArguments,
    RuntimeMethodCandidates,
    RuntimeConstructorShape,
    emptyRuntimeAppliedArguments,
    appendRuntimeAppliedArgument,
    runtimeAppliedArgumentsInOrder,
    emptyRuntimeMethodCandidates,
    appendRuntimeMethodCandidate,
    filterRuntimeMethodCandidates,
    runtimeMethodCandidatesInOrder,
    foldrRuntimeMethodCandidates,
    constructorApplicationIsSaturated,
    foldrRuntimeAppliedArguments,
    runtimeAppliedArgumentCount,
    runtimeConstructorArity,
    runtimeConstructorFieldTypes,
    runtimeConstructorName,
    runtimeConstructorTypeName,
    runtimeConstructorTypeParameters,
    constructorIsSaturated,
    RuntimeCell,
    RuntimeEnv,
    ScopeResult (..),
    ModuleEvaluationMode (..),
  )
where

import Control.Monad.Trans.State.Strict (StateT)
import qualified Data.Foldable as Foldable
import Data.Map.Strict (Map)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import Data.Word (Word64)
import Jazz.Compiler.AST
  ( CorePhase (..),
    Expr,
    NumericType,
    SignaturePayload,
    SignatureType,
  )
import Jazz.Compiler.BuiltinCatalog (BuiltinSymbol)
import Jazz.Compiler.Diagnostics (Diagnostic)
import Jazz.Compiler.FractionalLiteral (FractionalLiteralSource)
import Jazz.Compiler.Name (ResolvedName)
import Jazz.Compiler.RecursiveBindings (LambdaCaptureHints)
import Jazz.Compiler.Runtime.Observation
  ( RuntimeCallableIdentity,
    RuntimeObservationState,
  )
import Jazz.Compiler.Runtime.Outcome (RuntimeControl (..))
import Jazz.Compiler.SemanticFacts (CapabilityId, CoreNodeId, ImplId, MethodId)

data RuntimeFloatMetadata = RuntimeFloatMetadata
  { runtimeFloatLiteralSource :: Maybe FractionalLiteralSource,
    runtimeFloatTargetType :: Maybe NumericType
  }
  deriving (Eq, Show)

newtype RuntimeIntMetadata = RuntimeIntMetadata
  { runtimeIntTargetType :: Maybe NumericType
  }
  deriving stock (Eq, Show)

data RuntimeEvidence = RuntimeEvidence CapabilityId ImplId (Maybe MethodId) (SignatureType 'Resolved) (Maybe Text)
  deriving (Eq, Show)

runtimeEvidenceTarget :: RuntimeEvidence -> SignatureType 'Resolved
runtimeEvidenceTarget (RuntimeEvidence _ _ _ implTarget _) = implTarget

data RuntimeMethodCandidate = RuntimeMethodCandidate RuntimeEvidence (Either Diagnostic RuntimeValue)

-- | Ordered explicit result obligations attached to one runtime value. The
-- constructor stays private so callers cannot reintroduce nested hint wrappers.
-- Hints are stored outermost-to-innermost, matching source evaluation order.
newtype RuntimeExplicitResultHints = RuntimeExplicitResultHints (Seq (SignatureType 'Resolved))
  deriving stock (Eq, Show)
  deriving newtype (Semigroup)

newtype DeferredHostScopeId = DeferredHostScopeId Int
  deriving (Eq, Ord, Show)

data DeferredHostBindingKey = DeferredHostBindingKey DeferredHostScopeId CoreNodeId ResolvedName
  deriving (Eq, Ord, Show)

data DeferredHostBindingState
  = DeferredHostBindingEvaluating
  | DeferredHostBindingEvaluated (Either RuntimeControl RuntimeValue)

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
    runtimeClosureLambdaCaptureHints :: LambdaCaptureHints 'Analyzed,
    runtimeClosureParameter :: ResolvedName,
    runtimeClosureBody :: Expr 'Analyzed,
    runtimeClosureTypeHint :: Maybe (SignatureType 'Resolved),
    runtimeClosureModulePath :: Maybe [Text],
    runtimeClosureCallableIdentity :: RuntimeCallableIdentity
  }

-- | Constructor metadata shared by every partial application. Its constructor
-- stays private so the cached arity cannot disagree with the field types.
data RuntimeConstructorShape = RuntimeConstructorShape ResolvedName [ResolvedName] ResolvedName !Int [SignatureType 'Resolved]
  deriving (Eq)

-- | Append-efficient arguments shared by curried runtime applications.
-- 'Seq.length' is constant time, so a second cached count would only duplicate
-- an invariant.
newtype RuntimeAppliedArguments = RuntimeAppliedArguments (Seq RuntimeValue)

-- | Source-ordered qualified-method candidates. Candidate precedence follows
-- insertion order, so construction stays private and append-only.
newtype RuntimeMethodCandidates = RuntimeMethodCandidates (Seq RuntimeMethodCandidate)

data RuntimeValue
  = VInt Integer RuntimeIntMetadata
  | VFloat Double RuntimeFloatMetadata
  | VBool Bool
  | VChar Char
  | VText Text
  | VList [RuntimeValue] (Maybe (SignatureType 'Resolved))
  | VTuple [RuntimeValue]
  | VClosure RuntimeClosure
  | VBuiltin BuiltinSymbol [RuntimeValue]
  | VOperator Text [RuntimeValue]
  | VSectionLeft Text RuntimeValue
  | VSectionRight Text RuntimeValue
  | VDeclaredOperatorRightSection Text RuntimeValue RuntimeValue
  | VConstructorState RuntimeConstructorShape RuntimeAppliedArguments
  | VQualifiedMethodState Text Text (SignaturePayload 'Resolved) RuntimeMethodCandidates RuntimeAppliedArguments
  | VTyped (SignatureType 'Resolved) RuntimeValue
  | VExplicitTypeApplication (SignatureType 'Resolved) RuntimeValue
  | VRuntimeExplicitResultHints RuntimeExplicitResultHints RuntimeValue
  | VDeferredHostBinding
      DeferredHostBindingKey
      Diagnostic
      (Maybe [Text])
      (Expr 'Analyzed)
      RuntimeEnv

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
      VDeclaredOperatorRightSection operatorSymbol _ rightOperand ->
        "VDeclaredOperatorRightSection " <> show operatorSymbol <> " <operator> " <> show rightOperand
      VConstructorState shape capturedArgs ->
        "VConstructor "
          <> show (runtimeConstructorTypeName shape)
          <> " "
          <> show (runtimeConstructorName shape)
          <> " "
          <> show (runtimeConstructorFieldTypes shape)
          <> " "
          <> show (runtimeAppliedArgumentsInOrder capturedArgs)
      VQualifiedMethodState methodKey _ _ candidates capturedArgs ->
        "VQualifiedMethod "
          <> show methodKey
          <> " "
          <> show (runtimeMethodCandidatesInOrder candidates)
          <> " "
          <> show (runtimeAppliedArgumentsInOrder capturedArgs)
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
pattern VConstructor :: ResolvedName -> [ResolvedName] -> ResolvedName -> [SignatureType 'Resolved] -> [RuntimeValue] -> RuntimeValue
pattern VConstructor typeName typeParameters constructorName fieldTypes capturedArgs <-
  VConstructorState
    (RuntimeConstructorShape typeName typeParameters constructorName _ fieldTypes)
    (runtimeAppliedArgumentsInOrder -> capturedArgs)
  where
    VConstructor typeName typeParameters constructorName fieldTypes capturedArgs =
      VConstructorState
        (runtimeConstructorShape typeName typeParameters constructorName fieldTypes)
        (runtimeAppliedArgumentsFromList capturedArgs)

-- | Evaluator view that keeps the invariant-owning shape and append-efficient
-- arguments intact between curried applications. Callers can reuse a shape,
-- but cannot forge its cached arity.
pattern VConstructorApplication :: RuntimeConstructorShape -> RuntimeAppliedArguments -> RuntimeValue
pattern VConstructorApplication shape capturedArgs =
  VConstructorState shape capturedArgs

-- | Historical ordered-list view retained for public runtime consumers.
pattern VQualifiedMethod :: Text -> Text -> SignaturePayload 'Resolved -> [RuntimeMethodCandidate] -> [RuntimeValue] -> RuntimeValue
pattern VQualifiedMethod methodKey classParameter methodSignature candidates capturedArgs <-
  VQualifiedMethodState
    methodKey
    classParameter
    methodSignature
    (runtimeMethodCandidatesInOrder -> candidates)
    (runtimeAppliedArgumentsInOrder -> capturedArgs)
  where
    VQualifiedMethod methodKey classParameter methodSignature candidates capturedArgs =
      VQualifiedMethodState
        methodKey
        classParameter
        methodSignature
        (runtimeMethodCandidatesFromList candidates)
        (runtimeAppliedArgumentsFromList capturedArgs)

-- | Internal evaluator view retaining append-efficient ordered collections.
pattern VQualifiedMethodApplication :: Text -> Text -> SignaturePayload 'Resolved -> RuntimeMethodCandidates -> RuntimeAppliedArguments -> RuntimeValue
pattern VQualifiedMethodApplication methodKey classParameter methodSignature candidates capturedArgs =
  VQualifiedMethodState methodKey classParameter methodSignature candidates capturedArgs

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
  VDeclaredOperatorRightSection,
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
  VDeclaredOperatorRightSection,
  VConstructorApplication,
  VQualifiedMethodApplication,
  VTyped,
  VExplicitTypeApplication,
  VExplicitResultHints,
  VDeferredHostBinding
  #-}

prependRuntimeExplicitResultHint :: SignatureType 'Resolved -> RuntimeValue -> RuntimeValue
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
attachRuntimeExplicitResultHints outerHints runtimeValue =
  case runtimeValue of
    VRuntimeExplicitResultHints innerHints innerValue ->
      VRuntimeExplicitResultHints
        (outerHints <> innerHints)
        innerValue
    _ ->
      VRuntimeExplicitResultHints
        outerHints
        runtimeValue

runtimeExplicitResultHintsView :: RuntimeValue -> Maybe (RuntimeExplicitResultHints, RuntimeValue)
runtimeExplicitResultHintsView runtimeValue =
  case runtimeValue of
    VRuntimeExplicitResultHints hints innerValue -> Just (hints, innerValue)
    _ -> Nothing

runtimeExplicitResultHintsInOrder :: RuntimeValue -> [SignatureType 'Resolved]
runtimeExplicitResultHintsInOrder runtimeValue =
  case runtimeExplicitResultHintsView runtimeValue of
    Just (RuntimeExplicitResultHints hints, _) -> Foldable.toList hints
    Nothing -> []

foldRuntimeExplicitResultHints ::
  (accumulator -> SignatureType 'Resolved -> accumulator) ->
  accumulator ->
  RuntimeExplicitResultHints ->
  accumulator
foldRuntimeExplicitResultHints step initial (RuntimeExplicitResultHints hints) =
  Foldable.foldl' step initial hints

instance Show RuntimeMethodCandidate where
  show (RuntimeMethodCandidate evidence _) =
    "RuntimeMethodCandidate " <> show evidence

type RuntimeCell = Either Diagnostic RuntimeValue

type RuntimeEnv = Map ResolvedName RuntimeCell

data ScopeResult = ScopeResult
  { scopeResultEnvironment :: RuntimeEnv,
    scopeResultValue :: Maybe RuntimeValue,
    scopeResultEnvironmentMayReachHostCells :: Bool
  }

data ModuleEvaluationMode
  = EvaluateDependencyModule
  | EvaluateEntryModule
  deriving (Eq, Show)

constructorIsSaturated :: [SignatureType 'Resolved] -> [RuntimeValue] -> Bool
constructorIsSaturated fieldTypes capturedArgs =
  length capturedArgs >= length fieldTypes

runtimeAppliedArgumentsFromList :: [RuntimeValue] -> RuntimeAppliedArguments
runtimeAppliedArgumentsFromList capturedArgs =
  RuntimeAppliedArguments (Seq.fromList capturedArgs)

emptyRuntimeAppliedArguments :: RuntimeAppliedArguments
emptyRuntimeAppliedArguments = RuntimeAppliedArguments Seq.empty

runtimeAppliedArgumentsInOrder :: RuntimeAppliedArguments -> [RuntimeValue]
runtimeAppliedArgumentsInOrder (RuntimeAppliedArguments capturedArgs) =
  Foldable.toList capturedArgs

runtimeAppliedArgumentCount :: RuntimeAppliedArguments -> Int
runtimeAppliedArgumentCount (RuntimeAppliedArguments capturedArgs) =
  Seq.length capturedArgs

appendRuntimeAppliedArgument :: RuntimeValue -> RuntimeAppliedArguments -> RuntimeAppliedArguments
appendRuntimeAppliedArgument argumentValue (RuntimeAppliedArguments capturedArgs) =
  RuntimeAppliedArguments (capturedArgs Seq.|> argumentValue)

foldrRuntimeAppliedArguments ::
  (RuntimeValue -> accumulator -> accumulator) ->
  accumulator ->
  RuntimeAppliedArguments ->
  accumulator
foldrRuntimeAppliedArguments step initial (RuntimeAppliedArguments capturedArgs) =
  Foldable.foldr step initial capturedArgs

runtimeMethodCandidatesFromList :: [RuntimeMethodCandidate] -> RuntimeMethodCandidates
runtimeMethodCandidatesFromList candidates =
  RuntimeMethodCandidates (Seq.fromList candidates)

emptyRuntimeMethodCandidates :: RuntimeMethodCandidates
emptyRuntimeMethodCandidates = RuntimeMethodCandidates Seq.empty

appendRuntimeMethodCandidate :: RuntimeMethodCandidate -> RuntimeMethodCandidates -> RuntimeMethodCandidates
appendRuntimeMethodCandidate candidate (RuntimeMethodCandidates candidates) =
  RuntimeMethodCandidates (candidates Seq.|> candidate)

filterRuntimeMethodCandidates :: (RuntimeMethodCandidate -> Bool) -> RuntimeMethodCandidates -> RuntimeMethodCandidates
filterRuntimeMethodCandidates predicate (RuntimeMethodCandidates candidates) =
  RuntimeMethodCandidates (Seq.filter predicate candidates)

runtimeMethodCandidatesInOrder :: RuntimeMethodCandidates -> [RuntimeMethodCandidate]
runtimeMethodCandidatesInOrder (RuntimeMethodCandidates candidates) =
  Foldable.toList candidates

foldrRuntimeMethodCandidates ::
  (RuntimeMethodCandidate -> accumulator -> accumulator) ->
  accumulator ->
  RuntimeMethodCandidates ->
  accumulator
foldrRuntimeMethodCandidates step initial (RuntimeMethodCandidates candidates) =
  Foldable.foldr step initial candidates

constructorApplicationIsSaturated :: RuntimeConstructorShape -> RuntimeAppliedArguments -> Bool
constructorApplicationIsSaturated shape capturedArgs =
  runtimeAppliedArgumentCount capturedArgs >= runtimeConstructorArity shape

runtimeConstructorShape :: ResolvedName -> [ResolvedName] -> ResolvedName -> [SignatureType 'Resolved] -> RuntimeConstructorShape
runtimeConstructorShape typeName typeParameters constructorName fieldTypes =
  RuntimeConstructorShape typeName typeParameters constructorName (length fieldTypes) fieldTypes

runtimeConstructorTypeName :: RuntimeConstructorShape -> ResolvedName
runtimeConstructorTypeName (RuntimeConstructorShape typeName _ _ _ _) = typeName

runtimeConstructorTypeParameters :: RuntimeConstructorShape -> [ResolvedName]
runtimeConstructorTypeParameters (RuntimeConstructorShape _ typeParameters _ _ _) = typeParameters

runtimeConstructorName :: RuntimeConstructorShape -> ResolvedName
runtimeConstructorName (RuntimeConstructorShape _ _ constructorName _ _) = constructorName

runtimeConstructorArity :: RuntimeConstructorShape -> Int
runtimeConstructorArity (RuntimeConstructorShape _ _ _ arity _) = arity

runtimeConstructorFieldTypes :: RuntimeConstructorShape -> [SignatureType 'Resolved]
runtimeConstructorFieldTypes (RuntimeConstructorShape _ _ _ _ fieldTypes) = fieldTypes
