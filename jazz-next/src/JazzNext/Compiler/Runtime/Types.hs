{-# LANGUAGE OverloadedStrings #-}

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
    RuntimeValue (..),
    RuntimeCell,
    RuntimeEnv,
    ScopeResult (..),
    ModuleEvaluationMode (..)
  ) where

import Control.Monad.Trans.State.Strict (StateT)
import Data.Map.Strict (Map)
import Data.Text (Text)
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

newtype DeferredHostScopeId = DeferredHostScopeId Int
  deriving (Eq, Ord, Show)

data DeferredHostBindingKey = DeferredHostBindingKey DeferredHostScopeId (Maybe [Text]) SourceSpan Name
  deriving (Eq, Ord, Show)

data DeferredHostBindingState
  = DeferredHostBindingEvaluating
  | DeferredHostBindingEvaluated (Either Diagnostic RuntimeValue)

data RuntimeHostEvaluationState = RuntimeHostEvaluationState
  { runtimeHostEvaluationBindingCache :: Map DeferredHostBindingKey DeferredHostBindingState,
    runtimeHostEvaluationNextScopeId :: Int
  }

type RuntimeHostEvaluationT m = StateT RuntimeHostEvaluationState m

data RuntimeValue
  = VInt Integer RuntimeIntMetadata
  | VFloat Double RuntimeFloatMetadata
  | VBool Bool
  | VChar Char
  | VText Text
  | VList [RuntimeValue] (Maybe SignatureType)
  | VTuple [RuntimeValue]
  | VClosure RuntimeEnv Bool Name Expr (Maybe SignatureType) (Maybe [Text])
  | VBuiltin BuiltinSymbol [RuntimeValue]
  | VOperator Text [RuntimeValue]
  | VSectionLeft Text RuntimeValue
  | VSectionRight Text RuntimeValue
  | VConstructor Name [Name] Name [DataConstructorArgument] [RuntimeValue]
  | VQualifiedMethod Text Text SignaturePayload [RuntimeMethodCandidate] [RuntimeValue]
  | VTyped SignatureType RuntimeValue
  | VExplicitTypeApplication SignatureType RuntimeValue
  | VExplicitResultHint SignatureType RuntimeValue
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
      (VExplicitResultHint _ leftInner, rightInner) -> leftInner == rightInner
      (leftInner, VExplicitResultHint _ rightInner) -> leftInner == rightInner
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
      VClosure _ _ parameterName bodyExpr maybeTypeHint modulePath ->
        "VClosure <env> " <> show parameterName <> " " <> show bodyExpr <> " " <> show maybeTypeHint <> " " <> show modulePath
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
      VExplicitResultHint typeHint innerValue ->
        "VExplicitResultHint " <> show typeHint <> " " <> show innerValue
      VDeferredHostBinding {} -> "VDeferredHostBinding <thunk>"

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
