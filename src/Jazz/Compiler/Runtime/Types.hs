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
    RuntimeDictionary (..),
    DeferredHostScopeId (..),
    DeferredHostBindingKey (..),
    DeferredHostBindingState (..),
    RuntimeControl (..),
    RuntimeHostEvaluationState (..),
    RuntimeHostEvaluationT,
    RuntimeExplicitResultHints,
    RuntimeClosure (..),
    RuntimeAnnotation (..),
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
        VConstructor,
        VConstructorApplication,
        VAnnotated,
        VDeferredHostBinding,
        VConstrained,
        VEvidence,
        VCapabilityMethod
      ),
    prependRuntimeExplicitResultHint,
    attachRuntimeExplicitResultHints,
    runtimeExplicitResultHintsView,
    runtimeExplicitResultHintsInOrder,
    foldRuntimeExplicitResultHints,
    RuntimeAppliedArguments,
    RuntimeConstructorShape,
    appendRuntimeAppliedArgument,
    runtimeAppliedArgumentsInOrder,
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
  )
import Jazz.Compiler.BuiltinCatalog (BuiltinSymbol)
import Jazz.Compiler.CoreIdentity (CoreBinderId, CoreNodeId, MethodId, ResolvedReference)
import Jazz.Compiler.Diagnostics (Diagnostic)
import Jazz.Compiler.FractionalLiteral (FractionalLiteralSource)
import Jazz.Compiler.Name (ResolvedName)
import Jazz.Compiler.Runtime.Observation
  ( RuntimeCallableIdentity,
    RuntimeObservationState,
  )
import Jazz.Compiler.Runtime.Outcome (RuntimeControl (..))
import Jazz.Compiler.SemanticFacts (AnalyzedScheme, AnalyzedType, EvidenceReference (..))
import Jazz.Compiler.SourceUnitOwnership (SourceUnitOwner)
import Jazz.Compiler.TypeRepresentation (InferenceVariable)

data RuntimeFloatMetadata = RuntimeFloatMetadata
  { runtimeFloatLiteralSource :: Maybe FractionalLiteralSource,
    runtimeFloatTargetType :: Maybe NumericType
  }
  deriving (Eq, Show)

newtype RuntimeIntMetadata = RuntimeIntMetadata
  { runtimeIntTargetType :: Maybe NumericType
  }
  deriving stock (Eq, Show)

-- A dictionary closes over its selected method cells and prerequisite
-- dictionaries, so a caller's instances remain available in imported closures.
data RuntimeDictionary = RuntimeDictionary
  { runtimeDictionaryEvidence :: EvidenceReference,
    runtimeDictionaryMethods :: Map MethodId RuntimeCell,
    runtimeDictionaryPrerequisites :: [RuntimeDictionary]
  }

-- | Ordered explicit result obligations attached to one runtime value. The
-- constructor stays private so callers cannot reintroduce nested hint wrappers.
-- Hints are stored outermost-to-innermost, matching source evaluation order.
newtype RuntimeExplicitResultHints = RuntimeExplicitResultHints (Seq AnalyzedType)
  deriving stock (Eq, Show)
  deriving newtype (Semigroup)

newtype DeferredHostScopeId = DeferredHostScopeId Int
  deriving (Eq, Ord, Show)

data DeferredHostBindingKey
  = DeferredHostBindingKey DeferredHostScopeId CoreNodeId ResolvedName
  | DictionaryBindingKey DeferredHostScopeId CoreBinderId ResolvedName [EvidenceReference]
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
    runtimeClosureParameter :: ResolvedName,
    runtimeClosureParameterReference :: ResolvedReference,
    runtimeClosureBody :: Expr 'Analyzed,
    runtimeClosureTypeHint :: Maybe AnalyzedType,
    runtimeClosureModulePath :: Maybe SourceUnitOwner,
    runtimeClosureCallableIdentity :: RuntimeCallableIdentity
  }

-- | Constructor metadata shared by every partial application. Its constructor
-- stays private so the cached arity cannot disagree with the field types.
data RuntimeConstructorShape = RuntimeConstructorShape ResolvedName [InferenceVariable] ResolvedName !Int [AnalyzedType]
  deriving (Eq)

-- | Append-efficient arguments shared by curried runtime applications.
-- 'Seq.length' is constant time, so a second cached count would only duplicate
-- an invariant.
newtype RuntimeAppliedArguments = RuntimeAppliedArguments (Seq RuntimeValue)

-- | Value-associated typing information survives storing and partially applying
-- a callable. Operations that only inspect the payload can ignore its kind.
data RuntimeAnnotation
  = RuntimeTypeHint AnalyzedType
  | RuntimeTypeApplication AnalyzedType
  | RuntimeResultHints RuntimeExplicitResultHints
  | RuntimeMethodCall Text

data RuntimeValue
  = VInt Integer RuntimeIntMetadata
  | VFloat Double RuntimeFloatMetadata
  | VBool Bool
  | VChar Char
  | VText Text
  | VList [RuntimeValue] (Maybe AnalyzedType)
  | VTuple [RuntimeValue]
  | VClosure RuntimeClosure
  | VBuiltin BuiltinSymbol [RuntimeValue]
  | VConstructorState RuntimeConstructorShape RuntimeAppliedArguments
  | VCapabilityMethod Text
  | VAnnotatedState RuntimeAnnotation RuntimeValue
  | VConstrained DeferredHostScopeId CoreBinderId AnalyzedScheme ResolvedName (Maybe SourceUnitOwner) (Expr 'Analyzed) RuntimeEnv
  | VEvidence RuntimeDictionary
  | VDeferredHostBinding
      DeferredHostBindingKey
      Diagnostic
      (Maybe SourceUnitOwner)
      (Expr 'Analyzed)
      RuntimeEnv

-- | All annotation construction preserves flat, ordered pending result hints.
pattern VAnnotated :: RuntimeAnnotation -> RuntimeValue -> RuntimeValue
pattern VAnnotated annotation value <- VAnnotatedState annotation value
  where
    VAnnotated annotation value = case annotation of
      RuntimeResultHints hints -> attachRuntimeExplicitResultHints hints value
      _ -> VAnnotatedState annotation value

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
      VConstructorState shape capturedArgs ->
        "VConstructor "
          <> show (runtimeConstructorTypeName shape)
          <> " "
          <> show (runtimeConstructorName shape)
          <> " "
          <> show (runtimeConstructorFieldTypes shape)
          <> " "
          <> show (runtimeAppliedArgumentsInOrder capturedArgs)
      VCapabilityMethod methodKey -> "VCapabilityMethod " <> show methodKey
      VAnnotatedState (RuntimeTypeHint typeHint) innerValue ->
        "VTyped " <> show typeHint <> " " <> show innerValue
      VAnnotatedState (RuntimeTypeApplication typeHint) innerValue ->
        "VExplicitTypeApplication " <> show typeHint <> " " <> show innerValue
      VAnnotatedState (RuntimeMethodCall name) innerValue ->
        "VMethodCall " <> show name <> " " <> show innerValue
      VAnnotatedState (RuntimeResultHints hints) innerValue ->
        "VExplicitResultHints " <> show hints <> " " <> show innerValue
      VDeferredHostBinding {} -> "VDeferredHostBinding <thunk>"
      VConstrained {} -> "VConstrained <function>"
      VEvidence {} -> "VEvidence <dictionary>"

-- | Historical ordered-list constructor view used by runtime semantics and
-- tests. Construction establishes the shape and argument invariants once.
pattern VConstructor :: ResolvedName -> [InferenceVariable] -> ResolvedName -> [AnalyzedType] -> [RuntimeValue] -> RuntimeValue
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
  VConstructorApplication,
  VCapabilityMethod,
  VAnnotated,
  VDeferredHostBinding,
  VConstrained,
  VEvidence
  #-}

prependRuntimeExplicitResultHint :: AnalyzedType -> RuntimeValue -> RuntimeValue
prependRuntimeExplicitResultHint typeHint runtimeValue =
  case runtimeValue of
    VAnnotatedState (RuntimeResultHints (RuntimeExplicitResultHints innerHints)) innerValue ->
      VAnnotatedState
        (RuntimeResultHints (RuntimeExplicitResultHints (typeHint Seq.<| innerHints)))
        innerValue
    _ ->
      VAnnotatedState
        (RuntimeResultHints (RuntimeExplicitResultHints (Seq.singleton typeHint)))
        runtimeValue

attachRuntimeExplicitResultHints :: RuntimeExplicitResultHints -> RuntimeValue -> RuntimeValue
attachRuntimeExplicitResultHints outerHints runtimeValue =
  case runtimeValue of
    VAnnotatedState (RuntimeResultHints innerHints) innerValue ->
      VAnnotatedState
        (RuntimeResultHints (outerHints <> innerHints))
        innerValue
    _ ->
      VAnnotatedState
        (RuntimeResultHints outerHints)
        runtimeValue

runtimeExplicitResultHintsView :: RuntimeValue -> Maybe (RuntimeExplicitResultHints, RuntimeValue)
runtimeExplicitResultHintsView runtimeValue =
  case runtimeValue of
    VAnnotated (RuntimeResultHints hints) innerValue -> Just (hints, innerValue)
    _ -> Nothing

runtimeExplicitResultHintsInOrder :: RuntimeValue -> [AnalyzedType]
runtimeExplicitResultHintsInOrder runtimeValue =
  case runtimeExplicitResultHintsView runtimeValue of
    Just (RuntimeExplicitResultHints hints, _) -> Foldable.toList hints
    Nothing -> []

foldRuntimeExplicitResultHints ::
  (accumulator -> AnalyzedType -> accumulator) ->
  accumulator ->
  RuntimeExplicitResultHints ->
  accumulator
foldRuntimeExplicitResultHints step initial (RuntimeExplicitResultHints hints) =
  Foldable.foldl' step initial hints

type RuntimeCell = Either Diagnostic RuntimeValue

type RuntimeEnv = Map ResolvedReference RuntimeCell

data ScopeResult = ScopeResult
  { scopeResultEnvironment :: RuntimeEnv,
    scopeResultValue :: Maybe RuntimeValue
  }

data ModuleEvaluationMode
  = EvaluateDependencyModule
  | EvaluateEntryModule
  deriving (Eq, Show)

constructorIsSaturated :: [AnalyzedType] -> [RuntimeValue] -> Bool
constructorIsSaturated fieldTypes capturedArgs =
  length capturedArgs >= length fieldTypes

runtimeAppliedArgumentsFromList :: [RuntimeValue] -> RuntimeAppliedArguments
runtimeAppliedArgumentsFromList capturedArgs =
  RuntimeAppliedArguments (Seq.fromList capturedArgs)

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

constructorApplicationIsSaturated :: RuntimeConstructorShape -> RuntimeAppliedArguments -> Bool
constructorApplicationIsSaturated shape capturedArgs =
  runtimeAppliedArgumentCount capturedArgs >= runtimeConstructorArity shape

runtimeConstructorShape :: ResolvedName -> [InferenceVariable] -> ResolvedName -> [AnalyzedType] -> RuntimeConstructorShape
runtimeConstructorShape typeName typeParameters constructorName fieldTypes =
  RuntimeConstructorShape typeName typeParameters constructorName (length fieldTypes) fieldTypes

runtimeConstructorTypeName :: RuntimeConstructorShape -> ResolvedName
runtimeConstructorTypeName (RuntimeConstructorShape typeName _ _ _ _) = typeName

runtimeConstructorTypeParameters :: RuntimeConstructorShape -> [InferenceVariable]
runtimeConstructorTypeParameters (RuntimeConstructorShape _ typeParameters _ _ _) = typeParameters

runtimeConstructorName :: RuntimeConstructorShape -> ResolvedName
runtimeConstructorName (RuntimeConstructorShape _ _ constructorName _ _) = constructorName

runtimeConstructorArity :: RuntimeConstructorShape -> Int
runtimeConstructorArity (RuntimeConstructorShape _ _ _ arity _) = arity

runtimeConstructorFieldTypes :: RuntimeConstructorShape -> [AnalyzedType]
runtimeConstructorFieldTypes (RuntimeConstructorShape _ _ _ _ fieldTypes) = fieldTypes
