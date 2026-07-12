{-# LANGUAGE OverloadedStrings #-}

-- | Small interpreter/runtime for the currently-supported core language. It is
-- intentionally simple and mirrors the same builtin/operator contracts enforced
-- by analysis and type inference.
module JazzNext.Compiler.Runtime
  ( ModuleEvaluationMode (..),
    RuntimeCell,
    RuntimeEnv,
    RuntimeHostEvaluationT,
    RuntimeValue (..),
    ScopeResult (..),
    evaluateModuleScope,
    evaluateRuntimeExprWithBuiltinsAndBindingHints,
    evaluateRuntimeExprWithBuiltinsAndBindingHintsAndSourceUnitStatements,
    evaluateRuntimeExprWithBuiltins,
    evaluateRuntimeExpr,
    evaluateRuntimeExprWithHost,
    evaluateRuntimeExprWithHostAndBuiltinsAndBindingHintsAndSourceUnitStatements,
    evaluateModuleScopeWithHost,
    evaluateModuleScopeWithHostAndSourceUnitStatements,
    evaluateModuleScopeWithRequiredHost,
    evaluateModuleScopeWithRequiredEvaluationHost,
    runRuntimeHostEvaluation,
    runtimeExprRequiresHost,
    runtimeValueExactlyMatchesConstraint,
    renderRuntimeValue
  ) where

import Control.Monad (foldM, zipWithM)
import Control.Monad.Trans.Except
  ( ExceptT (..),
    runExceptT,
    throwE
  )
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict
  ( StateT,
    evalStateT,
    get,
    modify',
    put
  )
import Data.Char (isControl, ord, toUpper)
import Data.Functor.Identity (runIdentity)
import Data.List (foldl')
import Data.Maybe (fromMaybe, isJust, listToMaybe)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as Text
import JazzNext.Compiler.AST
  ( CaseArm (..),
    ClassMethodSignature (..),
    DataConstructorArgument (..),
    DataConstructor (..),
    Expr (..),
    ImplMethod (..),
    Literal (..),
    NumericType (..),
    Pattern (..),
    SignaturePayload (..),
    SignatureType (..),
    Statement (..)
  )
import JazzNext.Compiler.Diagnostics
  ( Diagnostic,
    SourceSpan,
    mkDiagnostic
  )
import JazzNext.Compiler.FractionalLiteral
  ( FractionalLiteralSource,
    fractionalLiteralExceedsMagnitude,
    fractionalLiteralIntegralValue
  )
import JazzNext.Compiler.BuiltinCatalog
  ( BuiltinResolutionMode (..),
    BuiltinSymbol (..),
    builtinNamesInMode,
    builtinSymbolArity,
    builtinSymbolName,
    builtinSymbolNumericConversionTarget,
    lookupBuiltinSymbolInMode,
    numericTypeFromName,
    numericTypeFloatMax,
    numericTypeIntegerBounds,
    numericTypeIsIntegral,
    renderNumericTypeName
  )
import JazzNext.Compiler.CapabilityFacts
  ( concreteConstraintArgument,
    constraintFunctionArgumentTypes,
    constraintSignatureTypeContainsClassParameter,
    constraintSignatureTypeVariableNamesInOrder,
    constraintSignatureTypesCompatible,
    identifierLooksLikeTypeVariable,
    qualifiedMethodKey,
    signaturePayloadConstraintType,
    substituteClassMethodSignature
  )
import JazzNext.Compiler.Name
  ( GeneratedNameKind (..),
    Name (..),
    NameNamespace (ConstructorNamespace),
    ResolvedNameOrigin (..),
    generatedName,
    identifierText,
    mkIdentifier,
    operatorBindingName,
    qualifiedMemberName,
    sourceName
  )
import JazzNext.Compiler.Parser.Operator
  ( isBuiltinOperatorSymbol
  )
import JazzNext.Compiler.Pattern
  ( patternBinderNames
  )
import JazzNext.Compiler.RecursiveBindings
  ( collectBindingNames,
    inferRecursiveGroupsOrdered,
    inferSelfRecursiveBindings
  )
import JazzNext.Compiler.RuntimeHints
  ( BindingRuntimeHintKey,
    bindingRuntimeHintKeyInModule,
    explicitTypeApplicationRuntimeHintKeyInModule
  )
import JazzNext.Compiler.RuntimeHost
  ( HostIOFailure (..),
    RuntimeHost (..),
    disabledRuntimeHost,
    hostIOCategoryToken,
    hostIOFailureMessage
  )
import Numeric (showHex)

-- | Runtime values produced by the interpreter, including partially applied
-- builtins/operators.
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
  | VClosure RuntimeEnv Name Expr (Maybe SignatureType) (Maybe [Text])
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
      VClosure _ parameterName bodyExpr maybeTypeHint modulePath ->
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

runRuntimeHostEvaluation ::
  Monad m =>
  RuntimeHost m ->
  (RuntimeHost (RuntimeHostEvaluationT m) -> RuntimeHostEvaluationT m value) ->
  m value
runRuntimeHostEvaluation host action =
  evalStateT
    (action (liftRuntimeHost host))
    RuntimeHostEvaluationState
      { runtimeHostEvaluationBindingCache = Map.empty,
        runtimeHostEvaluationNextScopeId = 0
      }

freshDeferredHostScopeId :: Monad m => RuntimeHostEvaluationT m DeferredHostScopeId
freshDeferredHostScopeId = do
  evaluationState <- get
  let scopeId = runtimeHostEvaluationNextScopeId evaluationState
  put
    evaluationState
      { runtimeHostEvaluationNextScopeId = scopeId + 1
      }
  pure (DeferredHostScopeId scopeId)

modifyDeferredHostBindingCache ::
  Monad m =>
  (Map DeferredHostBindingKey DeferredHostBindingState -> Map DeferredHostBindingKey DeferredHostBindingState) ->
  RuntimeHostEvaluationT m ()
modifyDeferredHostBindingCache updateCache =
  modify'
    ( \evaluationState ->
        evaluationState
          { runtimeHostEvaluationBindingCache =
              updateCache (runtimeHostEvaluationBindingCache evaluationState)
          }
    )

liftRuntimeHost :: Monad m => RuntimeHost m -> RuntimeHost (RuntimeHostEvaluationT m)
liftRuntimeHost host =
  RuntimeHost
    { runtimeHostReadText = lift . runtimeHostReadText host,
      runtimeHostWriteText = \path contents -> lift (runtimeHostWriteText host path contents),
      runtimeHostReadStdin = lift (runtimeHostReadStdin host),
      runtimeHostWriteStdout = lift . runtimeHostWriteStdout host,
      runtimeHostWriteStderr = lift . runtimeHostWriteStderr host,
      runtimeHostArguments = lift (runtimeHostArguments host),
      runtimeHostExit = lift . runtimeHostExit host
    }

evaluateRuntimeExpr :: Expr -> Either Diagnostic (Maybe RuntimeValue)
evaluateRuntimeExpr = evaluateRuntimeExprWithBuiltins ResolveKernelOnly

evaluateRuntimeExprWithHost :: Monad m => RuntimeHost m -> Expr -> m (Either Diagnostic (Maybe RuntimeValue))
evaluateRuntimeExprWithHost host =
  evaluateRuntimeExprWithHostAndBuiltinsAndBindingHintsAndSourceUnitStatements
    host
    Set.empty
    ResolveKernelOnly
    Map.empty

evaluateRuntimeExprWithHostAndBuiltinsAndBindingHintsAndSourceUnitStatements ::
  Monad m =>
  RuntimeHost m ->
  Set Int ->
  BuiltinResolutionMode ->
  Map BindingRuntimeHintKey SignatureType ->
  Expr ->
  m (Either Diagnostic (Maybe RuntimeValue))
evaluateRuntimeExprWithHostAndBuiltinsAndBindingHintsAndSourceUnitStatements host preludeStatementIndices builtinMode bindingTypeHints expr =
  runRuntimeHostEvaluation host $ \evaluationHost ->
    evaluateRuntimeExprWithEvaluationHostAndBuiltinsAndBindingHintsAndSourceUnitStatements
      evaluationHost
      preludeStatementIndices
      builtinMode
      bindingTypeHints
      expr

evaluateRuntimeExprWithEvaluationHostAndBuiltinsAndBindingHintsAndSourceUnitStatements ::
  Monad m =>
  RuntimeHost (RuntimeHostEvaluationT m) ->
  Set Int ->
  BuiltinResolutionMode ->
  Map BindingRuntimeHintKey SignatureType ->
  Expr ->
  RuntimeHostEvaluationT m (Either Diagnostic (Maybe RuntimeValue))
evaluateRuntimeExprWithEvaluationHostAndBuiltinsAndBindingHintsAndSourceUnitStatements host preludeStatementIndices builtinMode bindingTypeHints expr =
  if runtimeExprRequiresHost expr
    then
      case expr of
        EBlock statements ->
          fmap scopeResultValue
            <$> evaluateModuleScopeWithEvaluationHostAndSourceUnitStatements
              host
              preludeStatementIndices
              Nothing
              EvaluateEntryModule
              builtinMode
              bindingTypeHints
              Map.empty
              statements
        _ ->
          runExceptT
            (Just <$> evalValueWithHost host Nothing builtinMode bindingTypeHints Map.empty expr)
    else
      pure
        ( evaluateRuntimeExprPureWithBuiltinsAndBindingHintsAndSourceUnitStatements
            preludeStatementIndices
            builtinMode
            bindingTypeHints
            expr
        )

runtimeExprRequiresHost :: Expr -> Bool
runtimeExprRequiresHost expr =
  case expr of
    ELit _ -> False
    EVar name -> runtimeNameRequiresHost name
    ELambda _ bodyExpr -> runtimeExprRequiresHost bodyExpr
    EOperatorValue _ -> False
    EList elements -> any runtimeExprRequiresHost elements
    ETuple elements -> any runtimeExprRequiresHost elements
    EApply functionExpr argumentExpr ->
      runtimeExprRequiresHost functionExpr || runtimeExprRequiresHost argumentExpr
    ETypeApplication functionExpr _ _ -> runtimeExprRequiresHost functionExpr
    EIf conditionExpr thenExpr elseExpr ->
      any runtimeExprRequiresHost [conditionExpr, thenExpr, elseExpr]
    EPatternCase scrutineeExpr caseArms ->
      runtimeExprRequiresHost scrutineeExpr || any caseArmRequiresHost caseArms
    EBinary _ leftExpr rightExpr ->
      runtimeExprRequiresHost leftExpr || runtimeExprRequiresHost rightExpr
    ESectionLeft leftExpr _ -> runtimeExprRequiresHost leftExpr
    ESectionRight _ rightExpr -> runtimeExprRequiresHost rightExpr
    EBlock statements -> any runtimeStatementRequiresHost statements
  where
    caseArmRequiresHost (CaseArm _ maybeGuard bodyExpr) =
      maybe False runtimeExprRequiresHost maybeGuard || runtimeExprRequiresHost bodyExpr

runtimeStatementRequiresHost :: Statement -> Bool
runtimeStatementRequiresHost statement =
  case statement of
    SLet name _ (EVar referencedName)
      | identifierText name == identifierText referencedName,
        runtimeNameRequiresHost name ->
          False
    SLet _ _ valueExpr -> runtimeExprRequiresHost valueExpr
    SImpl _ _ _ methods -> any implMethodRequiresHost methods
    SExpr _ valueExpr -> runtimeExprRequiresHost valueExpr
    _ -> False
  where
    implMethodRequiresHost (ImplMethod _ _ bodyExpr) = runtimeExprRequiresHost bodyExpr

runtimeNameRequiresHost :: Name -> Bool
runtimeNameRequiresHost name =
  case lookupBuiltinSymbolInMode ResolveKernelOnly (identifierText name) of
    Just BuiltinReadTextRaw -> True
    Just BuiltinWriteTextRaw -> True
    Just BuiltinReadStdinRaw -> True
    Just BuiltinWriteStdoutRaw -> True
    Just BuiltinWriteStderrRaw -> True
    Just BuiltinArguments -> True
    Just BuiltinExit -> True
    _ -> False

-- | Evaluate an expression under the builtin resolution mode chosen by the
-- caller, returning a terminal scope value when one exists.
evaluateRuntimeExprWithBuiltins :: BuiltinResolutionMode -> Expr -> Either Diagnostic (Maybe RuntimeValue)
evaluateRuntimeExprWithBuiltins builtinMode expr =
  evaluateRuntimeExprWithBuiltinsAndBindingHints builtinMode Map.empty expr

evaluateRuntimeExprWithBuiltinsAndBindingHints ::
  BuiltinResolutionMode ->
  Map BindingRuntimeHintKey SignatureType ->
  Expr ->
  Either Diagnostic (Maybe RuntimeValue)
evaluateRuntimeExprWithBuiltinsAndBindingHints builtinMode bindingTypeHints expr =
  evaluateRuntimeExprWithBuiltinsAndBindingHintsAndSourceUnitStatements Set.empty builtinMode bindingTypeHints expr

evaluateRuntimeExprWithBuiltinsAndBindingHintsAndSourceUnitStatements ::
  Set Int ->
  BuiltinResolutionMode ->
  Map BindingRuntimeHintKey SignatureType ->
  Expr ->
  Either Diagnostic (Maybe RuntimeValue)
evaluateRuntimeExprWithBuiltinsAndBindingHintsAndSourceUnitStatements preludeStatementIndices builtinMode bindingTypeHints expr =
  runIdentity
    ( evaluateRuntimeExprWithHostAndBuiltinsAndBindingHintsAndSourceUnitStatements
        disabledRuntimeHost
        preludeStatementIndices
        builtinMode
        bindingTypeHints
        expr
    )

evaluateRuntimeExprPureWithBuiltinsAndBindingHintsAndSourceUnitStatements ::
  Set Int ->
  BuiltinResolutionMode ->
  Map BindingRuntimeHintKey SignatureType ->
  Expr ->
  Either Diagnostic (Maybe RuntimeValue)
evaluateRuntimeExprPureWithBuiltinsAndBindingHintsAndSourceUnitStatements preludeStatementIndices builtinMode bindingTypeHints expr =
  case expr of
    EBlock statements ->
      scopeResultValue
        <$> evaluateModuleScopeWithSourceUnitStatements
          preludeStatementIndices
          Nothing
          EvaluateEntryModule
          builtinMode
          bindingTypeHints
          Map.empty
          statements
    _ -> Just <$> evalValue builtinMode bindingTypeHints Map.empty expr

renderRuntimeValue :: RuntimeValue -> Text
renderRuntimeValue value =
  case value of
    VInt intValue _ -> Text.pack (show intValue)
    VFloat floatValue _ -> Text.pack (show floatValue)
    VBool boolValue ->
      if boolValue
        then "True"
        else "False"
    VChar charValue ->
      "'" <> renderQuotedScalar charValue <> "'"
    VText textValue ->
      "\"" <> Text.concatMap renderQuotedScalar textValue <> "\""
    VList elements _ ->
      "[" <> Text.intercalate ", " (map renderRuntimeValue elements) <> "]"
    VTuple elements ->
      "(" <> Text.intercalate ", " (map renderRuntimeValue elements) <> ")"
    VClosure {} -> "<function>"
    VBuiltin _ _ -> "<function>"
    VOperator {} -> "<function>"
    VSectionLeft {} -> "<function>"
    VSectionRight {} -> "<function>"
    VConstructor _ _ constructorName constructorArguments capturedArgs
      | constructorIsSaturated constructorArguments capturedArgs ->
          renderConstructorValue constructorName capturedArgs
      | otherwise ->
          "<function>"
    VQualifiedMethod {} -> "<function>"
    VTyped _ innerValue -> renderRuntimeValue innerValue
    VExplicitTypeApplication _ innerValue -> renderRuntimeValue innerValue
    VExplicitResultHint _ innerValue -> renderRuntimeValue innerValue
    VDeferredHostBinding {} -> "<deferred-host-binding>"

renderQuotedScalar :: Char -> Text
renderQuotedScalar value =
  case value of
    '\\' -> "\\\\"
    '\'' -> "\\'"
    '"' -> "\\\""
    '\n' -> "\\n"
    '\r' -> "\\r"
    '\t' -> "\\t"
    '\0' -> "\\0"
    _
      | isControl value ->
          "\\u{" <> Text.pack (map toUpper (showHex (ord value) "")) <> "}"
    _ -> Text.singleton value

renderConstructorValue :: Name -> [RuntimeValue] -> Text
renderConstructorValue constructorName arguments =
  case arguments of
    [] -> renderConstructorName constructorName
    _ ->
      renderConstructorName constructorName
        <> "("
        <> Text.intercalate ", " (map renderRuntimeValue arguments)
        <> ")"

renderConstructorName :: Name -> Text
renderConstructorName constructorName =
  case constructorName of
    ResolvedName _ ConstructorNamespace identifier -> identifierText identifier
    _ -> identifierText constructorName

runtimeDefinitionName :: Maybe [Text] -> Name -> Name
runtimeDefinitionName maybeModulePath name =
  case (maybeModulePath, name) of
    (Just modulePath, ResolvedName CurrentModule namespace identifier) ->
      ResolvedName (ImportedModule modulePath) namespace identifier
    _ -> name

runtimeConstructorArgument :: Maybe [Text] -> DataConstructorArgument -> DataConstructorArgument
runtimeConstructorArgument maybeModulePath argument =
  case argument of
    DataConstructorArgumentName name ->
      DataConstructorArgumentName (runtimeTypeName maybeModulePath name)
    DataConstructorArgumentOpaque -> DataConstructorArgumentOpaque

runtimeConstraintType :: Maybe [Text] -> SignatureType -> SignatureType
runtimeConstraintType maybeModulePath signatureType =
  case signatureType of
    TypeVariable name -> TypeVariable (runtimeTypeName maybeModulePath name)
    TypeName name -> TypeName (runtimeTypeName maybeModulePath name)
    TypeApplication name arguments ->
      TypeApplication
        (runtimeTypeName maybeModulePath name)
        (map (runtimeConstraintType maybeModulePath) arguments)
    TypeList elementType -> TypeList (runtimeConstraintType maybeModulePath elementType)
    TypeTuple elementTypes -> TypeTuple (map (runtimeConstraintType maybeModulePath) elementTypes)
    TypeFunction argumentType resultType ->
      TypeFunction
        (runtimeConstraintType maybeModulePath argumentType)
        (runtimeConstraintType maybeModulePath resultType)
    _ -> signatureType

runtimeTypeName :: Maybe [Text] -> Name -> Name
runtimeTypeName maybeModulePath name
  | identifierText name `elem` ["Int", "Float", "Bool", "Char", "Text"] = name
  | Just _ <- numericTypeFromName (identifierText name) = name
  | identifierLooksLikeTypeVariable name = name
  | otherwise = runtimeDefinitionName maybeModulePath name

-- | Runtime cells can hold either a value or the deterministic failure for a
-- recursive binding that cannot be forced safely.
type RuntimeCell = Either Diagnostic RuntimeValue

type RuntimeEnv = Map Name RuntimeCell

data ScopeResult = ScopeResult
  { scopeResultEnvironment :: RuntimeEnv,
    scopeResultValue :: Maybe RuntimeValue
  }

data ModuleEvaluationMode
  = EvaluateDependencyModule
  | EvaluateEntryModule
  deriving (Eq, Show)

-- | Evaluate a block scope in order. Declarations clear `lastExprValue`, so
-- `evalScope` returns `Just` only when the final surviving statement is an
-- `SExpr`; otherwise the block yields `Nothing`.
evalScope :: BuiltinResolutionMode -> Map BindingRuntimeHintKey SignatureType -> RuntimeEnv -> [Statement] -> Either Diagnostic (Maybe RuntimeValue)
evalScope builtinMode bindingTypeHints initialEnv statements =
  scopeResultValue
    <$> evaluateModuleScope
      Nothing
      EvaluateEntryModule
      builtinMode
      bindingTypeHints
      initialEnv
      statements

evalScopeWithModulePath :: Maybe [Text] -> BuiltinResolutionMode -> Map BindingRuntimeHintKey SignatureType -> RuntimeEnv -> [Statement] -> Either Diagnostic (Maybe RuntimeValue)
evalScopeWithModulePath currentModulePath builtinMode bindingTypeHints initialEnv statements =
  scopeResultValue
    <$> evaluateModuleScope
      currentModulePath
      EvaluateEntryModule
      builtinMode
      bindingTypeHints
      initialEnv
      statements

evaluateModuleScope ::
  Maybe [Text] ->
  ModuleEvaluationMode ->
  BuiltinResolutionMode ->
  Map BindingRuntimeHintKey SignatureType ->
  RuntimeEnv ->
  [Statement] ->
  Either Diagnostic ScopeResult
evaluateModuleScope = evaluateModuleScopeWithSourceUnitStatements Set.empty

evaluateModuleScopeWithHost ::
  Monad m =>
  RuntimeHost m ->
  Maybe [Text] ->
  ModuleEvaluationMode ->
  BuiltinResolutionMode ->
  Map BindingRuntimeHintKey SignatureType ->
  RuntimeEnv ->
  [Statement] ->
  m (Either Diagnostic ScopeResult)
evaluateModuleScopeWithHost host =
  evaluateModuleScopeWithHostAndSourceUnitStatements host Set.empty

evaluateModuleScopeWithRequiredHost ::
  Monad m =>
  RuntimeHost m ->
  Maybe [Text] ->
  ModuleEvaluationMode ->
  BuiltinResolutionMode ->
  Map BindingRuntimeHintKey SignatureType ->
  RuntimeEnv ->
  [Statement] ->
  m (Either Diagnostic ScopeResult)
evaluateModuleScopeWithRequiredHost host currentModulePath evaluationMode builtinMode bindingTypeHints initialEnv statements =
  runRuntimeHostEvaluation host $ \evaluationHost ->
    evaluateModuleScopeWithRequiredEvaluationHost
      evaluationHost
      currentModulePath
      evaluationMode
      builtinMode
      bindingTypeHints
      initialEnv
      statements

evaluateModuleScopeWithRequiredEvaluationHost ::
  Monad m =>
  RuntimeHost (RuntimeHostEvaluationT m) ->
  Maybe [Text] ->
  ModuleEvaluationMode ->
  BuiltinResolutionMode ->
  Map BindingRuntimeHintKey SignatureType ->
  RuntimeEnv ->
  [Statement] ->
  RuntimeHostEvaluationT m (Either Diagnostic ScopeResult)
evaluateModuleScopeWithRequiredEvaluationHost host currentModulePath evaluationMode builtinMode bindingTypeHints initialEnv statements =
  runExceptT
    ( evalScopeWithHost
        host
        Set.empty
        currentModulePath
        evaluationMode
        builtinMode
        bindingTypeHints
        initialEnv
        statements
    )

evaluateModuleScopeWithHostAndSourceUnitStatements ::
  Monad m =>
  RuntimeHost m ->
  Set Int ->
  Maybe [Text] ->
  ModuleEvaluationMode ->
  BuiltinResolutionMode ->
  Map BindingRuntimeHintKey SignatureType ->
  RuntimeEnv ->
  [Statement] ->
  m (Either Diagnostic ScopeResult)
evaluateModuleScopeWithHostAndSourceUnitStatements host preludeStatementIndices currentModulePath evaluationMode builtinMode bindingTypeHints initialEnv statements =
  runRuntimeHostEvaluation host $ \evaluationHost ->
    evaluateModuleScopeWithEvaluationHostAndSourceUnitStatements
      evaluationHost
      preludeStatementIndices
      currentModulePath
      evaluationMode
      builtinMode
      bindingTypeHints
      initialEnv
      statements

evaluateModuleScopeWithEvaluationHostAndSourceUnitStatements ::
  Monad m =>
  RuntimeHost (RuntimeHostEvaluationT m) ->
  Set Int ->
  Maybe [Text] ->
  ModuleEvaluationMode ->
  BuiltinResolutionMode ->
  Map BindingRuntimeHintKey SignatureType ->
  RuntimeEnv ->
  [Statement] ->
  RuntimeHostEvaluationT m (Either Diagnostic ScopeResult)
evaluateModuleScopeWithEvaluationHostAndSourceUnitStatements host preludeStatementIndices currentModulePath evaluationMode builtinMode bindingTypeHints initialEnv statements =
  if runtimeExprRequiresHost (EBlock statements)
    then
      runExceptT
        ( evalScopeWithHost
            host
            preludeStatementIndices
            currentModulePath
            evaluationMode
            builtinMode
            bindingTypeHints
            initialEnv
            statements
        )
    else
      pure
        ( evaluateModuleScopePureWithSourceUnitStatements
            preludeStatementIndices
            currentModulePath
            evaluationMode
            builtinMode
            bindingTypeHints
            initialEnv
            statements
        )

evaluateModuleScopeWithSourceUnitStatements ::
  Set Int ->
  Maybe [Text] ->
  ModuleEvaluationMode ->
  BuiltinResolutionMode ->
  Map BindingRuntimeHintKey SignatureType ->
  RuntimeEnv ->
  [Statement] ->
  Either Diagnostic ScopeResult
evaluateModuleScopeWithSourceUnitStatements preludeStatementIndices currentModulePath evaluationMode builtinMode bindingTypeHints initialEnv statements =
  runIdentity
    ( evaluateModuleScopeWithHostAndSourceUnitStatements
        disabledRuntimeHost
        preludeStatementIndices
        currentModulePath
        evaluationMode
        builtinMode
        bindingTypeHints
        initialEnv
        statements
    )

evaluateModuleScopePureWithSourceUnitStatements ::
  Set Int ->
  Maybe [Text] ->
  ModuleEvaluationMode ->
  BuiltinResolutionMode ->
  Map BindingRuntimeHintKey SignatureType ->
  RuntimeEnv ->
  [Statement] ->
  Either Diagnostic ScopeResult
evaluateModuleScopePureWithSourceUnitStatements preludeStatementIndices currentModulePath evaluationMode builtinMode bindingTypeHints initialEnv statements = go initialEnv Nothing indexedStatements
  where
    indexedStatements = zip [0 ..] statements
    statementsByIndex = Map.fromList indexedStatements
    modulePathsByStatement = collectModulePathsByStatement currentModulePath indexedStatements
    recursiveGroups =
      inferRecursiveGroupsOrdered
        (Set.union (Map.keysSet initialEnv) (Set.map (sourceName . mkIdentifier) (builtinNamesInMode builtinMode)))
        indexedStatements
    selfRecursiveFunctionStatements =
      inferSelfRecursiveBindings exprContainsFunctionBranch indexedStatements
    bindingNamesByStatement = collectBindingNames indexedStatements
    bindingCells = map (uncurry cellForStatement) indexedStatements

    go :: RuntimeEnv -> Maybe RuntimeValue -> [(Int, Statement)] -> Either Diagnostic ScopeResult
    go env lastExprValue remainingStatements =
      case remainingStatements of
        [] ->
          -- Declaration-only scopes intentionally remain `Nothing` until a terminal `SExpr` sets a value.
          Right (ScopeResult env lastExprValue)
        (statementIndex, statement) : rest ->
          case statement of
            SSignature {} ->
              go env Nothing rest
            SModule {} ->
              go env Nothing rest
            SImport {} ->
              go env Nothing rest
            SClass _ capabilityName parameters methods ->
              go (insertClassMethods capabilityName parameters methods env) Nothing rest
            SImpl _ capabilityName arguments methods ->
              go (insertImplMethods (modulePathForStatement statementIndex) capabilityName arguments methods env) Nothing rest
            SData _ typeName typeParameters constructors ->
              go (insertDataConstructors (modulePathForStatement statementIndex) typeName typeParameters constructors env) Nothing rest
            SLet name _ _ ->
              case evaluationMode of
                EvaluateDependencyModule ->
                  go (Map.insert name (bindingCellAt statementIndex) env) Nothing rest
                EvaluateEntryModule -> do
                  value <- bindingCellAt statementIndex
                  go (Map.insert name (Right value) env) Nothing rest
            SExpr _ expr ->
              case evaluationMode of
                EvaluateDependencyModule -> go env Nothing rest
                EvaluateEntryModule -> do
                  value <- evalValueAt statementIndex env expr
                  go env (Just value) rest

    modulePathForStatement :: Int -> Maybe [Text]
    modulePathForStatement statementIndex =
      if Set.member statementIndex preludeStatementIndices
        then Just []
        else Map.findWithDefault currentModulePath statementIndex modulePathsByStatement

    evalValueAt :: Int -> RuntimeEnv -> Expr -> Either Diagnostic RuntimeValue
    evalValueAt statementIndex =
      evalValueWithModulePath (modulePathForStatement statementIndex) builtinMode bindingTypeHints

    bindingCellAt :: Int -> RuntimeCell
    bindingCellAt statementIndex =
      case drop statementIndex bindingCells of
        cell : _ -> cell
        [] ->
          Left
            (runtimeDiagnostic "E3020" "internal runtime error: missing binding cell for statement")
    
    cellForStatement :: Int -> Statement -> RuntimeCell
    cellForStatement statementIndex statement =
      case statement of
        SLet bindingName _ valueExpr ->
          bindingCell statementIndex bindingName valueExpr
        _ ->
          Left
            (runtimeDiagnostic "E3020" "internal runtime error: expected binding statement")

    bindingCell :: Int -> Name -> Expr -> RuntimeCell
    bindingCell statementIndex bindingName valueExpr =
      case selectedRecursiveAliasTarget statementIndex visibleEnv valueExpr of
        Left diagnostic ->
          Left diagnostic
        Right (Just targetIndex) ->
          case resolveRecursiveAliasTarget (Set.singleton statementIndex) targetIndex of
            Left diagnostic -> Left diagnostic
            Right resolvedTargetIndex -> bindingCellAt resolvedTargetIndex
        Right Nothing
          | Map.member statementIndex recursiveGroups,
            exprDefinitelyNotFunctionValue valueExpr ->
              Left (runtimeDiagnostic "E3021" "runtime recursive binding has no concrete value")
          | otherwise ->
              do
                evaluatedValue <- evalBindingValue statementIndex bindingName visibleEnv valueExpr
                Right (attachSelfRecursiveBinding statementIndex bindingName evaluatedValue)
      where
        visibleEnv = bindingEnv statementIndex bindingName

    evalBindingValue :: Int -> Name -> RuntimeEnv -> Expr -> Either Diagnostic RuntimeValue
    evalBindingValue statementIndex bindingName env valueExpr =
      case previousSignatureNumericTarget statementIndex bindingName of
        Just targetType -> do
          runtimeValue <- evalNumericSignatureBinding statementIndex targetType env valueExpr
          attachRuntimeTypeHint (previousSignatureRuntimeTypeHint statementIndex bindingName) runtimeValue
            >>= attachDefaultBindingIntegerTarget
        Nothing -> do
          runtimeValue <- evalValueAt statementIndex env valueExpr
          attachRuntimeTypeHint (bindingRuntimeTypeHint statementIndex bindingName) runtimeValue
            >>= attachDefaultBindingIntegerTarget

    evalNumericSignatureBinding :: Int -> NumericType -> RuntimeEnv -> Expr -> Either Diagnostic RuntimeValue
    evalNumericSignatureBinding statementIndex targetType env valueExpr =
      case valueExpr of
        ELit (LInt literalValue) ->
          convertIntegerToNumericTarget conversionBuiltin targetType literalValue
        ELit (LFloat literalValue literalSource _) ->
          convertFloatToNumericTarget conversionBuiltin targetType literalValue (Just literalSource)
        _ -> do
          runtimeValue <- evalValueAt statementIndex env valueExpr
          evalNumericConversion conversionBuiltin targetType runtimeValue
      where
        conversionBuiltin = numericConversionBuiltinForTarget targetType

    previousSignatureNumericTarget :: Int -> Name -> Maybe NumericType
    previousSignatureNumericTarget statementIndex bindingName =
      case Map.lookup (statementIndex - 1) statementsByIndex of
        Just (SSignature signatureName _ signaturePayload)
          | identifierText signatureName == identifierText bindingName ->
              signatureNumericTarget signaturePayload
        _ -> Nothing

    previousSignatureRuntimeTypeHint :: Int -> Name -> Maybe SignatureType
    previousSignatureRuntimeTypeHint statementIndex bindingName =
      case Map.lookup (statementIndex - 1) statementsByIndex of
        Just (SSignature signatureName _ signaturePayload)
          | identifierText signatureName == identifierText bindingName ->
              signaturePayloadConstraintType signaturePayload
        _ -> Nothing

    bindingRuntimeTypeHint :: Int -> Name -> Maybe SignatureType
    bindingRuntimeTypeHint statementIndex bindingName =
      runtimeConstraintType (modulePathForStatement statementIndex) <$> rawHint
      where
        rawHint =
          case previousSignatureRuntimeTypeHint statementIndex bindingName of
            Just signatureHint -> Just signatureHint
            Nothing ->
              case Map.lookup statementIndex statementsByIndex of
                Just (SLet _ bindingSpan _) ->
                  Map.lookup
                    (bindingRuntimeHintKeyInModule (modulePathForStatement statementIndex) bindingName bindingSpan)
                    bindingTypeHints
                _ -> Nothing

    collectModulePathsByStatement :: Maybe [Text] -> [(Int, Statement)] -> Map Int (Maybe [Text])
    collectModulePathsByStatement initialModulePath =
      snd . foldl' collectModulePath (initialModulePath, Map.empty)
      where
        collectModulePath (currentModulePath, pathsByStatement) (statementIndex, statement) =
          let nextModulePath =
                case statement of
                  SModule _ modulePath -> Just modulePath
                  _ -> currentModulePath
           in ( nextModulePath,
                Map.insert statementIndex nextModulePath pathsByStatement
              )

    signatureNumericTarget :: SignaturePayload -> Maybe NumericType
    signatureNumericTarget signaturePayload =
      case signaturePayload of
        SignatureType TypeInt -> Just NumericInt64
        SignatureType TypeFloat -> Just NumericFloat64
        SignatureType (TypeNumeric targetType) -> Just targetType
        ConstrainedSignature _ signatureType ->
          constraintSignatureNumericTarget signatureType
        _ -> Nothing

    constraintSignatureNumericTarget :: SignatureType -> Maybe NumericType
    constraintSignatureNumericTarget signatureType =
      case signatureType of
        TypeInt -> Just NumericInt64
        TypeFloat -> Just NumericFloat64
        TypeNumeric numericType -> Just numericType
        TypeName typeName ->
          case identifierText typeName of
            "Int" -> Just NumericInt64
            "Int8" -> Just NumericInt8
            "Int16" -> Just NumericInt16
            "Int32" -> Just NumericInt32
            "Int64" -> Just NumericInt64
            "UInt8" -> Just NumericUInt8
            "UInt16" -> Just NumericUInt16
            "UInt32" -> Just NumericUInt32
            "UInt64" -> Just NumericUInt64
            "Float" -> Just NumericFloat64
            "Float16" -> Just NumericFloat16
            "Float32" -> Just NumericFloat32
            "Float64" -> Just NumericFloat64
            _ -> Nothing
        _ -> Nothing

    -- Alias bridges can legitimately point across a recursive SCC, but pure
    -- alias loops need a deterministic diagnostic instead of infinite forcing.
    resolveRecursiveAliasTarget :: Set Int -> Int -> Either Diagnostic Int
    resolveRecursiveAliasTarget visited statementIndex
      | Set.member statementIndex visited =
          Left (runtimeDiagnostic "E3021" "runtime recursive alias cycle has no concrete value")
      | otherwise =
          case Map.lookup statementIndex statementsByIndex of
            Just (SLet bindingName _ aliasExpr) ->
              case selectedRecursiveAliasTarget statementIndex (bindingEnv statementIndex bindingName) aliasExpr of
                Left diagnostic ->
                  Left diagnostic
                Right (Just nextTargetIndex) ->
                  resolveRecursiveAliasTarget (Set.insert statementIndex visited) nextTargetIndex
                Right Nothing ->
                  Right statementIndex
            Just _ ->
              Left
                (runtimeDiagnostic "E3020" "internal runtime error: expected binding statement while resolving alias")
            Nothing ->
              Left
                (runtimeDiagnostic "E3020" "internal runtime error: missing binding statement while resolving alias")

    bindingEnv :: Int -> Name -> RuntimeEnv
    bindingEnv statementIndex bindingName =
      case functionSelfReferenceCell statementIndex bindingName of
        Just selfCell ->
          Map.insert
            bindingName
            selfCell
            peerVisibleEnv
        Nothing
          | recursiveBindingNeedsSelf statementIndex ->
              Map.insert
                bindingName
                (bindingCellAt statementIndex)
                peerVisibleEnv
          | otherwise -> peerVisibleEnv
      where
        peerVisibleEnv = recursivePeerEnv statementIndex (envBefore statementIndex)

    functionSelfReferenceCell :: Int -> Name -> Maybe RuntimeCell
    functionSelfReferenceCell statementIndex bindingName
      | recursiveFunctionNeedsSelf statementIndex bindingName =
          Just (Left (runtimeDiagnostic "E3021" "runtime recursive binding has no concrete value"))
      | otherwise =
          Nothing

    recursiveFunctionNeedsSelf :: Int -> Name -> Bool
    recursiveFunctionNeedsSelf statementIndex bindingName =
      Set.member statementIndex selfRecursiveFunctionStatements
        && Map.notMember bindingName (envBefore statementIndex)

    recursiveBindingNeedsSelf :: Int -> Bool
    recursiveBindingNeedsSelf statementIndex =
      -- Function-valued self recursion gets stitched onto the resulting
      -- closure after wrapper evaluation. Pre-seeding `self` here is only
      -- needed for non-function recursive bindings; doing it eagerly for block
      -- alias wrappers can blackhole before the closure is returned.
      Map.member statementIndex recursiveGroups
        && Set.notMember statementIndex selfRecursiveFunctionStatements

    -- Wrapper expressions like `if` and `{ g = \(x) -> f x. g. }` should
    -- evaluate to their closure first, then get their own binding stitched
    -- into the captured env without forcing the whole wrapper through a
    -- self-referential scope during evaluation.
    attachSelfRecursiveBinding :: Int -> Name -> RuntimeValue -> RuntimeValue
    attachSelfRecursiveBinding statementIndex bindingName runtimeValue
      | recursiveFunctionNeedsSelf statementIndex bindingName =
          case runtimeValue of
            VClosure capturedEnv parameterName bodyExpr maybeTypeHint closureModulePath ->
              VClosure
                (Map.insert bindingName (bindingCellAt statementIndex) capturedEnv)
                parameterName
                bodyExpr
                maybeTypeHint
                closureModulePath
            _ -> runtimeValue
      | otherwise =
          runtimeValue

    recursiveAliasTarget :: Set Name -> Int -> Expr -> Maybe Int
    recursiveAliasTarget locallyBoundNames statementIndex valueExpr =
      case peelSingleExprBlock valueExpr of
        EVar targetName ->
          if Set.member targetName locallyBoundNames
            then Nothing
            else
              case Map.lookup statementIndex recursiveGroups of
                Just groupMembers ->
                  lookupRecursivePeer targetName groupMembers
                Nothing -> Nothing
        EOperatorValue operatorSymbol
          | not (isBuiltinOperatorSymbol operatorSymbol) ->
              let targetName = operatorBindingName operatorSymbol
               in
                if Set.member targetName locallyBoundNames
                  then Nothing
                  else
                    case Map.lookup statementIndex recursiveGroups of
                      Just groupMembers ->
                        lookupRecursivePeer targetName groupMembers
                      Nothing -> Nothing
        _ -> Nothing

    -- Preserve wrapper runtime semantics by evaluating the branch condition
    -- first, then following alias resolution only through the selected branch.
    selectedRecursiveAliasTarget :: Int -> RuntimeEnv -> Expr -> Either Diagnostic (Maybe Int)
    selectedRecursiveAliasTarget =
      selectedRecursiveAliasTargetWithBound Set.empty

    selectedRecursiveAliasTargetWithBound ::
      Set Name ->
      Int ->
      RuntimeEnv ->
      Expr ->
      Either Diagnostic (Maybe Int)
    selectedRecursiveAliasTargetWithBound locallyBoundNames statementIndex env expr =
      case peelSingleExprBlock expr of
        EIf conditionExpr thenExpr elseExpr ->
          selectRecursiveAliasTarget locallyBoundNames statementIndex env conditionExpr thenExpr elseExpr
        EPatternCase scrutineeExpr caseArms -> do
          scrutineeValue <- evalValueAt statementIndex env scrutineeExpr
          selectedArm <-
            selectMatchingCaseArmForAlias
              (modulePathForStatement statementIndex)
              (evalValueAt statementIndex)
              env
              scrutineeValue
              caseArms
          case selectedArm of
            Just (newLocallyBoundNames, armEnv, bodyExpr) ->
              selectedRecursiveAliasTargetWithBound
                (Set.union locallyBoundNames newLocallyBoundNames)
                statementIndex
                armEnv
                bodyExpr
            Nothing ->
              Right Nothing
        peeledExpr ->
          Right (recursiveAliasTarget locallyBoundNames statementIndex peeledExpr)

    selectRecursiveAliasTarget :: Set Name -> Int -> RuntimeEnv -> Expr -> Expr -> Expr -> Either Diagnostic (Maybe Int)
    selectRecursiveAliasTarget locallyBoundNames statementIndex env conditionExpr thenExpr elseExpr = do
      conditionValue <- evalValueAt statementIndex env conditionExpr
      case conditionValue of
        VBool True ->
          selectedRecursiveAliasTargetWithBound locallyBoundNames statementIndex env thenExpr
        VBool False ->
          selectedRecursiveAliasTargetWithBound locallyBoundNames statementIndex env elseExpr
        other ->
          Left
            ( runtimeDiagnostic
                "E3003"
                ("runtime branch condition must be Bool, found " <> renderRuntimeType other)
            )

    selectMatchingCaseArmForAlias ::
      Maybe [Text] ->
      (RuntimeEnv -> Expr -> Either Diagnostic RuntimeValue) ->
      RuntimeEnv ->
      RuntimeValue ->
      [CaseArm] ->
      Either Diagnostic (Maybe (Set Name, RuntimeEnv, Expr))
    selectMatchingCaseArmForAlias patternModulePath evalGuard env scrutineeValue =
      go
      where
        go remainingArms =
          case remainingArms of
            [] -> Right Nothing
            caseArm : rest ->
              chooseArm caseArm rest

        chooseArm caseArm rest =
          case matchCaseArm patternModulePath env scrutineeValue caseArm of
            Just (armEnv, guardExpr, bodyExpr) ->
              case guardExpr of
                Nothing ->
                  Right
                    ( Just
                        ( caseArmBoundNames caseArm,
                          armEnv,
                          bodyExpr
                        )
                    )
                Just conditionExpr -> do
                  guardValue <- evalGuard armEnv conditionExpr
                  case guardValue of
                    VBool True ->
                      Right
                        ( Just
                            ( caseArmBoundNames caseArm,
                              armEnv,
                              bodyExpr
                            )
                        )
                    VBool False ->
                      go rest
                    other ->
                      Left
                        ( runtimeDiagnostic
                            "E3003"
                            ("runtime case guard must be Bool, found " <> renderRuntimeType other)
                        )
            Nothing ->
              go rest

    caseArmBoundNames :: CaseArm -> Set Name
    caseArmBoundNames (CaseArm pattern _ _) =
      patternBinderNames pattern

    -- Single-expression blocks are semantically transparent here, so peel
    -- them before following recursive alias edges and cycle detection.
    peelSingleExprBlock :: Expr -> Expr
    peelSingleExprBlock expr =
      case expr of
        EBlock [SExpr _ innerExpr] -> peelSingleExprBlock innerExpr
        _ -> expr

    terminalBlockLocalAliasExpr :: [Statement] -> Maybe ([Statement], Expr)
    terminalBlockLocalAliasExpr statements =
      case reverse statements of
        SExpr _ (EVar aliasName) : precedingStatements ->
          let prefixStatements = reverse precedingStatements
           in fmap
                (\aliasExpr -> (prefixStatements, aliasExpr))
                (followLocalAlias Set.empty aliasName (localAliasBindings prefixStatements))
        _ -> Nothing

    localAliasBindings :: [Statement] -> Map Name Expr
    localAliasBindings =
      foldl' collectBinding Map.empty
      where
        collectBinding bindings statement =
          case statement of
            SLet bindingName _ bindingExpr ->
              Map.insert bindingName bindingExpr bindings
            _ -> bindings

    followLocalAlias :: Set Name -> Name -> Map Name Expr -> Maybe Expr
    followLocalAlias visitedNames aliasName localBindings =
      if Set.member aliasName visitedNames
        then Nothing
        else
          case Map.lookup aliasName localBindings of
            Just aliasExpr ->
              case peelSingleExprBlock aliasExpr of
                EVar nextAliasName
                  | Map.member nextAliasName localBindings ->
                      followLocalAlias (Set.insert aliasName visitedNames) nextAliasName localBindings
                _ -> Just aliasExpr
            Nothing ->
              Nothing

    blockLocalAliasEnv :: Maybe [Text] -> RuntimeEnv -> [Statement] -> RuntimeEnv
    blockLocalAliasEnv blockModulePath blockInitialEnv blockStatements =
      case blockStatements of
        [] -> blockInitialEnv
        _ -> blockEnvAfter (length blockStatements - 1)
      where
        indexedBlockStatements = zip [0 ..] blockStatements
        blockStatementsByIndex = Map.fromList indexedBlockStatements
        blockBindingCells = map (uncurry blockCellForStatement) indexedBlockStatements

        blockEnvBefore statementIndex
          | statementIndex <= 0 = blockInitialEnv
          | otherwise = blockEnvAfter (statementIndex - 1)

        blockEnvAfter statementIndex =
          case Map.lookup statementIndex blockStatementsByIndex of
            Just (SLet bindingName _ _) ->
              Map.insert
                bindingName
                (blockBindingCellAt statementIndex)
                (blockEnvBefore statementIndex)
            Just (SData _ typeName typeParameters constructors) ->
              insertDataConstructors blockModulePath typeName typeParameters constructors (blockEnvBefore statementIndex)
            Just (SClass _ capabilityName parameters methods) ->
              insertClassMethods capabilityName parameters methods (blockEnvBefore statementIndex)
            Just (SImpl _ capabilityName arguments methods) ->
              insertImplMethods blockModulePath capabilityName arguments methods (blockEnvBefore statementIndex)
            Just _ ->
              blockEnvBefore statementIndex
            Nothing ->
              blockEnvBefore statementIndex

        blockBindingCellAt statementIndex =
          case drop statementIndex blockBindingCells of
            cell : _ -> cell
            [] ->
              Left
                (runtimeDiagnostic "E3020" "internal runtime error: missing block binding cell for alias selection")

        blockCellForStatement statementIndex statement =
          case statement of
            SLet bindingName _ valueExpr ->
              evalValueWithModulePath blockModulePath builtinMode bindingTypeHints (blockEnvBefore statementIndex) valueExpr
                >>= attachRuntimeTypeHint (blockBindingRuntimeTypeHint statementIndex bindingName)
                >>= attachDefaultBindingIntegerTarget
            _ ->
              Left
                (runtimeDiagnostic "E3020" "internal runtime error: expected block binding statement for alias selection")

        blockBindingRuntimeTypeHint statementIndex bindingName =
          runtimeConstraintType blockModulePath <$> rawHint
          where
            rawHint =
              case blockPreviousSignatureRuntimeTypeHint statementIndex bindingName of
                Just signatureHint -> Just signatureHint
                Nothing ->
                  case Map.lookup statementIndex blockStatementsByIndex of
                    Just (SLet _ bindingSpan _) ->
                      Map.lookup
                        (bindingRuntimeHintKeyInModule blockModulePath bindingName bindingSpan)
                        bindingTypeHints
                    _ -> Nothing

        blockPreviousSignatureRuntimeTypeHint statementIndex bindingName =
          case Map.lookup (statementIndex - 1) blockStatementsByIndex of
            Just (SSignature signatureName _ signaturePayload)
              | identifierText signatureName == identifierText bindingName ->
                  signaturePayloadConstraintType signaturePayload
            _ -> Nothing

    lookupRecursivePeer :: Name -> [Int] -> Maybe Int
    lookupRecursivePeer targetName =
      foldl' chooseTarget Nothing
      where
        chooseTarget currentChoice peerIndex =
          case Map.lookup peerIndex bindingNamesByStatement of
            Just peerName
              | peerName == targetName ->
                  Just peerIndex
            _ -> currentChoice

    envBefore :: Int -> RuntimeEnv
    envBefore statementIndex
      | statementIndex <= 0 = initialEnv
      | otherwise = envAfter (statementIndex - 1)

    envAfter :: Int -> RuntimeEnv
    envAfter statementIndex =
      case Map.lookup statementIndex statementsByIndex of
        Just (SLet bindingName _ _) ->
          Map.insert
            bindingName
            (bindingCellAt statementIndex)
            (envBefore statementIndex)
        Just (SData _ typeName typeParameters constructors) ->
          insertDataConstructors (modulePathForStatement statementIndex) typeName typeParameters constructors (envBefore statementIndex)
        Just (SClass _ capabilityName parameters methods) ->
          insertClassMethods capabilityName parameters methods (envBefore statementIndex)
        Just (SImpl _ capabilityName arguments methods) ->
          insertImplMethods (modulePathForStatement statementIndex) capabilityName arguments methods (envBefore statementIndex)
        Just _ ->
          envBefore statementIndex
        Nothing ->
          envBefore statementIndex

    recursivePeerEnv :: Int -> RuntimeEnv -> RuntimeEnv
    recursivePeerEnv statementIndex envBeforeValue =
      case Map.lookup statementIndex recursiveGroups of
        Nothing -> envBeforeValue
        Just groupMembers ->
          foldl' insertPeer envBeforeValue groupMembers
      where
        insertPeer envAcc peerIndex
          | peerIndex == statementIndex = envAcc
          | otherwise =
              case
                  Map.lookup peerIndex bindingNamesByStatement of
                Just peerName
                  | Map.notMember peerName envBeforeValue ->
                      Map.insert peerName (bindingCellAt peerIndex) envAcc
                _ ->
                  envAcc

    insertDataConstructors :: Maybe [Text] -> Name -> [Name] -> [DataConstructor] -> RuntimeEnv -> RuntimeEnv
    insertDataConstructors definitionModulePath typeName typeParameters constructors env =
      foldl' insertConstructor env constructors
      where
        insertConstructor envAcc (DataConstructor constructorName constructorArguments) =
          Map.insert
            constructorName
            ( Right
                ( VConstructor
                    (runtimeDefinitionName definitionModulePath typeName)
                    typeParameters
                    (runtimeDefinitionName definitionModulePath constructorName)
                    (map (runtimeConstructorArgument definitionModulePath) constructorArguments)
                    []
                )
            )
            envAcc

    insertClassMethods :: Name -> [Name] -> [ClassMethodSignature] -> RuntimeEnv -> RuntimeEnv
    insertClassMethods capabilityName parameters methods env =
      case parameters of
        [classParameter] ->
          foldl' (insertMethod (identifierText classParameter)) env methods
        _ -> env
      where
        insertMethod classParameter envAcc (ClassMethodSignature methodName _ methodSignature) =
          let methodKey = qualifiedMethodKey capabilityName methodName
              methodName' = qualifiedMemberName capabilityName methodName
           in if Map.member methodName' envAcc
                then envAcc
                else Map.insert methodName' (Right (VQualifiedMethod methodKey classParameter methodSignature [] [])) envAcc

    insertImplMethods :: Maybe [Text] -> Name -> [SignatureType] -> [ImplMethod] -> RuntimeEnv -> RuntimeEnv
    insertImplMethods methodModulePath capabilityName arguments methods env =
      case arguments of
        [implTarget]
          | concreteConstraintArgument implTarget ->
              methodEnv
          where
            runtimeImplTarget = runtimeConstraintType methodModulePath implTarget
            methodEnv = foldl' insertCandidate env methodCandidates
            methodExprsByKey =
              Map.fromList
                [ (qualifiedMethodKey capabilityName methodName, methodExpr)
                  | ImplMethod methodName _ methodExpr <- methods
                ]
            methodCandidates =
              map
                ( \(ImplMethod methodName _ methodExpr) ->
                    let methodKey = qualifiedMethodKey capabilityName methodName
                        methodName' = qualifiedMemberName capabilityName methodName
                        evidence = RuntimeEvidence (identifierText capabilityName) runtimeImplTarget (Just methodKey)
                     in ( methodName',
                          methodKey,
                          RuntimeMethodCandidate evidence (methodCandidateCell runtimeImplTarget methodName' methodKey methodExpr)
                        )
                )
                methods
            methodCandidateCell implTarget methodName methodKey methodExpr =
              case selectedQualifiedMethodAliasTarget methodModulePath methodExprsByKey Set.empty methodEnv methodKey methodExpr of
                Left diagnostic ->
                  Left diagnostic
                Right True ->
                  Left
                    ( runtimeDiagnostic
                        "E3021"
                        ("runtime recursive qualified method alias cycle '" <> methodKey <> "' has no concrete value")
                    )
                Right False ->
                  evalValueWithModulePath methodModulePath builtinMode bindingTypeHints methodEnv methodExpr
                    >>= attachRuntimeMethodSignature methodModulePath methodEnv implTarget methodName
            insertCandidate envAcc (methodName, _, methodCandidate) =
              Map.adjust (addMethodCandidate methodCandidate) methodName envAcc
        _ -> env
      where
        addMethodCandidate methodCandidate methodCell =
          case methodCell of
            Right (VQualifiedMethod methodKey classParameter methodSignature candidates capturedArgs) ->
              Right (VQualifiedMethod methodKey classParameter methodSignature (candidates ++ [methodCandidate]) capturedArgs)
            _ -> methodCell

    attachRuntimeMethodSignature ::
      Maybe [Text] ->
      RuntimeEnv ->
      SignatureType ->
      Name ->
      RuntimeValue ->
      Either Diagnostic RuntimeValue
    attachRuntimeMethodSignature methodModulePath env implTarget methodName methodValue =
      case Map.lookup methodName env of
        Just (Right (VQualifiedMethod _ classParameter methodSignature _ _)) ->
          attachRuntimeTypeHint
            ( runtimeConstraintType signatureModulePath
                <$> substituteClassMethodSignature classParameter implTarget methodSignature
            )
            methodValue
        _ ->
          Right methodValue
      where
        signatureModulePath =
          case methodName of
            ResolvedName (ImportedModule classModulePath) _ _ -> Just classModulePath
            _ -> methodModulePath

    selectedQualifiedMethodAliasTarget :: Maybe [Text] -> Map Text Expr -> Set Text -> RuntimeEnv -> Text -> Expr -> Either Diagnostic Bool
    selectedQualifiedMethodAliasTarget methodModulePath methodExprsByKey visitedMethodKeys env methodKey expr
      | Set.member methodKey visitedMethodKeys =
          Right True
      | otherwise =
          case peelSingleExprBlock expr of
            EIf conditionExpr thenExpr elseExpr ->
              selectQualifiedMethodAliasTarget methodModulePath methodExprsByKey visitedMethodKeys env methodKey conditionExpr thenExpr elseExpr
            EPatternCase scrutineeExpr caseArms -> do
              scrutineeValue <- evalValueWithModulePath methodModulePath builtinMode bindingTypeHints env scrutineeExpr
              selectedArm <-
                selectMatchingCaseArmForAlias
                  methodModulePath
                  (evalValueWithModulePath methodModulePath builtinMode bindingTypeHints)
                  env
                  scrutineeValue
                  caseArms
              case selectedArm of
                Just (_, armEnv, bodyExpr) ->
                  selectedQualifiedMethodAliasTarget methodModulePath methodExprsByKey visitedMethodKeys armEnv methodKey bodyExpr
                Nothing ->
                  Right False
            EBlock statements ->
              case terminalBlockLocalAliasExpr statements of
                Just (prefixStatements, aliasExpr) ->
                  selectedQualifiedMethodAliasTarget
                    methodModulePath
                    methodExprsByKey
                    visitedMethodKeys
                    (blockLocalAliasEnv methodModulePath env prefixStatements)
                    methodKey
                    aliasExpr
                Nothing ->
                  Right False
            EVar aliasName ->
              let aliasNameText = identifierText aliasName
               in case Map.lookup aliasNameText methodExprsByKey of
                    Just aliasExpr ->
                      selectedQualifiedMethodAliasTarget methodModulePath methodExprsByKey nextVisitedMethodKeys env aliasNameText aliasExpr
                    Nothing ->
                      Right (aliasNameText == methodKey)
            _ ->
              Right False
      where
        nextVisitedMethodKeys = Set.insert methodKey visitedMethodKeys

    selectQualifiedMethodAliasTarget :: Maybe [Text] -> Map Text Expr -> Set Text -> RuntimeEnv -> Text -> Expr -> Expr -> Expr -> Either Diagnostic Bool
    selectQualifiedMethodAliasTarget methodModulePath methodExprsByKey visitedMethodKeys env methodKey conditionExpr thenExpr elseExpr = do
      conditionValue <- evalValueWithModulePath methodModulePath builtinMode bindingTypeHints env conditionExpr
      case conditionValue of
        VBool True ->
          selectedQualifiedMethodAliasTarget methodModulePath methodExprsByKey visitedMethodKeys env methodKey thenExpr
        VBool False ->
          selectedQualifiedMethodAliasTarget methodModulePath methodExprsByKey visitedMethodKeys env methodKey elseExpr
        other ->
          Left
            ( runtimeDiagnostic
                "E3003"
                ("runtime branch condition must be Bool, found " <> renderRuntimeType other)
            )

-- Match the type checker: self-seed recursion when any branch exposes a
-- lambda, so wrapped self-recursive closures capture their own binding before
-- runtime branch selection happens.
exprContainsFunctionBranch :: Expr -> Bool
exprContainsFunctionBranch expr =
  case expr of
    ELambda {} -> True
    ETypeApplication functionExpr _ _ ->
      exprContainsFunctionBranch functionExpr
    EIf _ thenExpr elseExpr ->
      exprContainsFunctionBranch thenExpr
        || exprContainsFunctionBranch elseExpr
    EPatternCase _ caseArms ->
      any
        (\(CaseArm _ _ bodyExpr) -> exprContainsFunctionBranch bodyExpr)
        caseArms
    EBlock statements ->
      scopeContainsFunctionBranch statements
    _ -> False

scopeContainsFunctionBranch :: [Statement] -> Bool
scopeContainsFunctionBranch statements =
  case reverse statements of
    SExpr _ expr : _ ->
      exprContainsFunctionBranchViaScopeBindings
        (collectScopeBindingExprs statements)
        Set.empty
        expr
    _ -> False
  where
    -- Block expressions can return a locally-bound alias like `g`, so resolve
    -- same-block alias chains before deciding whether the terminal value is a
    -- lambda-shaped recursive binding.
    exprContainsFunctionBranchViaScopeBindings scopeBindings visitedBindings scopeExpr =
      case scopeExpr of
        EVar bindingName ->
          case Map.lookup bindingName scopeBindings of
            Just bindingExpr
              | Set.notMember bindingName visitedBindings ->
                  exprContainsFunctionBranchViaScopeBindings
                    scopeBindings
                    (Set.insert bindingName visitedBindings)
                    bindingExpr
            _ -> False
        ELambda {} -> True
        ETypeApplication functionExpr _ _ ->
          exprContainsFunctionBranchViaScopeBindings scopeBindings visitedBindings functionExpr
        EIf _ thenExpr elseExpr ->
          exprContainsFunctionBranchViaScopeBindings scopeBindings visitedBindings thenExpr
            || exprContainsFunctionBranchViaScopeBindings scopeBindings visitedBindings elseExpr
        EPatternCase _ caseArms ->
          any
            ( \(CaseArm _ _ bodyExpr) ->
                exprContainsFunctionBranchViaScopeBindings scopeBindings visitedBindings bodyExpr
            )
            caseArms
        EBlock nestedStatements ->
          scopeContainsFunctionBranch nestedStatements
        _ -> False

    collectScopeBindingExprs =
      foldl' collect Map.empty
      where
        collect scopeBindings statement =
          case statement of
            SLet bindingName _ valueExpr ->
              Map.insert bindingName valueExpr scopeBindings
            _ -> scopeBindings

-- Fail fast only when a recursive SCC member is obviously non-function-valued;
-- anything more ambiguous should keep the previous runtime path.
exprDefinitelyNotFunctionValue :: Expr -> Bool
exprDefinitelyNotFunctionValue expr =
  case expr of
    ELit {} -> True
    EList {} -> True
    ETuple {} -> True
    EBinary {} -> True
    ETypeApplication functionExpr _ _ ->
      exprDefinitelyNotFunctionValue functionExpr
    EIf _ thenExpr elseExpr ->
      exprDefinitelyNotFunctionValue thenExpr
        && exprDefinitelyNotFunctionValue elseExpr
    EPatternCase {} ->
      False
    EBlock statements ->
      scopeDefinitelyNotFunctionValue statements
    _ -> False

scopeDefinitelyNotFunctionValue :: [Statement] -> Bool
scopeDefinitelyNotFunctionValue statements =
  case reverse statements of
    SExpr _ expr : _ -> exprDefinitelyNotFunctionValue expr
    _ -> False

evalValue :: BuiltinResolutionMode -> Map BindingRuntimeHintKey SignatureType -> RuntimeEnv -> Expr -> Either Diagnostic RuntimeValue
evalValue =
  evalValueWithModulePath Nothing

evalValueWithModulePath :: Maybe [Text] -> BuiltinResolutionMode -> Map BindingRuntimeHintKey SignatureType -> RuntimeEnv -> Expr -> Either Diagnostic RuntimeValue
evalValueWithModulePath currentModulePath builtinMode bindingTypeHints env expr =
  case expr of
    ELit literal -> Right (literalRuntimeValue literal)
    EVar name ->
      case Map.lookup name env of
        Just value -> value >>= forceQualifiedMethodValue builtinMode bindingTypeHints
        Nothing ->
          case lookupBuiltinSymbolInMode builtinMode nameText of
            Just builtinFunction -> Right (VBuiltin builtinFunction [])
            Nothing ->
              Left
                ( runtimeDiagnostic
                    "E3002"
                    ("runtime unbound variable '" <> nameText <> "'")
                )
      where
        nameText = identifierText name
    ELambda parameterName bodyExpr ->
      Right (VClosure env parameterName bodyExpr Nothing currentModulePath)
    EOperatorValue operatorSymbol
      | isBuiltinOperatorSymbol operatorSymbol ->
          Right (VOperator operatorSymbol [])
      | otherwise ->
          lookupOperatorBindingRuntimeValue builtinMode bindingTypeHints operatorSymbol env
    EList elements ->
      (`VList` Nothing) <$> mapM (evalValueWithModulePath currentModulePath builtinMode bindingTypeHints env) elements
    ETuple elements ->
      VTuple <$> mapM (evalValueWithModulePath currentModulePath builtinMode bindingTypeHints env) elements
    EApply functionExpr argumentExpr -> do
      functionValue <- evalValueWithModulePath currentModulePath builtinMode bindingTypeHints env functionExpr
      argumentValue <- evalValueWithModulePath currentModulePath builtinMode bindingTypeHints env argumentExpr
      applyRuntimeFunction builtinMode bindingTypeHints functionValue argumentValue
    ETypeApplication functionExpr typeArgumentSpan signatureType -> do
      let typeHint = runtimeConstraintType currentModulePath signatureType
      runtimeValue <- evalValueWithModulePath currentModulePath builtinMode bindingTypeHints env functionExpr
      case Map.lookup (explicitTypeApplicationRuntimeHintKeyInModule currentModulePath typeArgumentSpan) bindingTypeHints of
        Just concreteTypeHint ->
          applyRuntimeTypeHint (runtimeConstraintType currentModulePath concreteTypeHint) runtimeValue
        Nothing ->
          if isFunctionValue runtimeValue
            then Right (VExplicitTypeApplication typeHint runtimeValue)
            else applyRuntimeTypeHint (fromMaybe typeHint (explicitTypeApplicationRuntimeValueHint typeHint runtimeValue)) runtimeValue
    EIf conditionExpr thenExpr elseExpr -> do
      conditionValue <- evalValueWithModulePath currentModulePath builtinMode bindingTypeHints env conditionExpr
      case conditionValue of
        VBool True -> evalValueWithModulePath currentModulePath builtinMode bindingTypeHints env thenExpr
        VBool False -> evalValueWithModulePath currentModulePath builtinMode bindingTypeHints env elseExpr
        other ->
          Left
            ( runtimeDiagnostic
                "E3003"
                ("runtime branch condition must be Bool, found " <> renderRuntimeType other)
            )
    EPatternCase scrutineeExpr caseArms -> do
      scrutineeValue <- evalValueWithModulePath currentModulePath builtinMode bindingTypeHints env scrutineeExpr
      evalPatternCase currentModulePath builtinMode bindingTypeHints env scrutineeValue caseArms
    EBinary operatorSymbol leftExpr rightExpr
      | isBuiltinOperatorSymbol operatorSymbol -> do
          leftValue <- evalValueWithModulePath currentModulePath builtinMode bindingTypeHints env leftExpr
          rightValue <- evalValueWithModulePath currentModulePath builtinMode bindingTypeHints env rightExpr
          evalBinary builtinMode bindingTypeHints operatorSymbol leftValue rightValue
      | otherwise -> do
          operatorValue <- lookupOperatorBindingRuntimeValue builtinMode bindingTypeHints operatorSymbol env
          leftValue <- evalValueWithModulePath currentModulePath builtinMode bindingTypeHints env leftExpr
          partialValue <- applyRuntimeFunction builtinMode bindingTypeHints operatorValue leftValue
          rightValue <- evalValueWithModulePath currentModulePath builtinMode bindingTypeHints env rightExpr
          applyRuntimeFunction builtinMode bindingTypeHints partialValue rightValue
    ESectionLeft leftExpr operatorSymbol -> do
      leftValue <- evalValueWithModulePath currentModulePath builtinMode bindingTypeHints env leftExpr
      if isBuiltinOperatorSymbol operatorSymbol
        then Right (VSectionLeft operatorSymbol leftValue)
        else do
          operatorValue <- lookupOperatorBindingRuntimeValue builtinMode bindingTypeHints operatorSymbol env
          applyRuntimeFunction builtinMode bindingTypeHints operatorValue leftValue
    ESectionRight operatorSymbol rightExpr -> do
      rightValue <- evalValueWithModulePath currentModulePath builtinMode bindingTypeHints env rightExpr
      if isBuiltinOperatorSymbol operatorSymbol
        then Right (VSectionRight operatorSymbol rightValue)
        else do
          operatorValue <- lookupOperatorBindingRuntimeValue builtinMode bindingTypeHints operatorSymbol env
          Right (declaredOperatorRightSectionClosure currentModulePath operatorValue rightValue env)
    EBlock statements ->
      case evalScopeWithModulePath currentModulePath builtinMode bindingTypeHints env statements of
        Left err -> Left err
        Right Nothing ->
          Left
            (runtimeDiagnostic "E3006" "block expression has no terminal expression result at runtime")
        Right (Just value) -> Right value

forceQualifiedMethodValue :: BuiltinResolutionMode -> Map BindingRuntimeHintKey SignatureType -> RuntimeValue -> Either Diagnostic RuntimeValue
forceQualifiedMethodValue builtinMode bindingTypeHints runtimeValue =
  case runtimeValue of
    VQualifiedMethod methodKey classParameter methodSignature candidates capturedArgs ->
      applyQualifiedMethod
        builtinMode
        bindingTypeHints
        methodKey
        classParameter
        methodSignature
        candidates
        capturedArgs
    _ ->
      Right runtimeValue

lookupOperatorBindingRuntimeValue ::
  BuiltinResolutionMode ->
  Map BindingRuntimeHintKey SignatureType ->
  Text ->
  RuntimeEnv ->
  Either Diagnostic RuntimeValue
lookupOperatorBindingRuntimeValue builtinMode bindingTypeHints operatorSymbol env =
  case Map.lookup (operatorBindingName operatorSymbol) env of
    Just value ->
      value >>= forceQualifiedMethodValue builtinMode bindingTypeHints
    Nothing ->
      Left
        ( runtimeDiagnostic
            "E3027"
            ("operator '" <> operatorSymbol <> "' has no executable binding")
        )

declaredOperatorRightSectionClosure :: Maybe [Text] -> RuntimeValue -> RuntimeValue -> RuntimeEnv -> RuntimeValue
declaredOperatorRightSectionClosure currentModulePath operatorValue rightValue env =
  VClosure
    capturedEnv
    leftParameter
    (EApply (EApply (EVar functionName) (EVar leftParameter)) (EVar rightParameter))
    Nothing
    currentModulePath
  where
    functionName = generatedName OperatorSectionFunction
    leftParameter = generatedName OperatorSectionLeft
    rightParameter = generatedName OperatorSectionRight
    capturedEnv =
      Map.insert functionName (Right operatorValue) $
        Map.insert rightParameter (Right rightValue) env

literalRuntimeValue :: Literal -> RuntimeValue
literalRuntimeValue literal =
  case literal of
    LInt value -> VInt value untypedIntMetadata
    LFloat value literalSource maybeTargetType ->
      case maybeTargetType of
        Just targetType ->
          VFloat
            (roundFloatTarget targetType value)
            (targetedFloatMetadataWithSource targetType (Just literalSource))
        Nothing ->
          VFloat value (untypedFloatMetadata (Just literalSource))
    LBool value -> VBool value
    LChar value -> VChar value
    LText value -> VText value

attachRuntimeTypeHint :: Maybe SignatureType -> RuntimeValue -> Either Diagnostic RuntimeValue
attachRuntimeTypeHint maybeTypeHint runtimeValue =
  case maybeTypeHint of
    Just typeHint ->
      applyRuntimeTypeHint typeHint runtimeValue
    Nothing ->
      Right runtimeValue

applyRuntimeTypeHint :: SignatureType -> RuntimeValue -> Either Diagnostic RuntimeValue
applyRuntimeTypeHint typeHint runtimeValue =
  case runtimeValue of
    VTyped _ innerValue ->
      applyRuntimeTypeHint typeHint innerValue
    VExplicitTypeApplication _ innerValue ->
      applyRuntimeTypeHint typeHint innerValue
    VExplicitResultHint _ innerValue ->
      applyRuntimeTypeHint typeHint innerValue
    _ ->
      case (typeHint, runtimeValue) of
        (TypeInt, _) -> do
          convertedValue <- evalNumericConversion (numericConversionBuiltinForTarget NumericInt64) NumericInt64 runtimeValue
          Right (VTyped TypeInt convertedValue)
        (TypeFloat, _) -> do
          convertedValue <- evalNumericConversion (numericConversionBuiltinForTarget NumericFloat64) NumericFloat64 runtimeValue
          Right (VTyped TypeFloat convertedValue)
        (TypeNumeric targetType, _) ->
          evalNumericConversion (numericConversionBuiltinForTarget targetType) targetType runtimeValue
        (TypeBool, VBool {}) -> Right runtimeValue
        (TypeChar, VChar {}) -> Right runtimeValue
        (TypeText, VText {}) -> Right runtimeValue
        (TypeName typeName, _)
          | Just targetType <- constraintTypeNameNumericTarget typeName -> do
              convertedValue <- evalNumericConversion (numericConversionBuiltinForTarget targetType) targetType runtimeValue
              if identifierText typeName == "Int" || identifierText typeName == "Float"
                then Right (VTyped typeHint convertedValue)
                else Right convertedValue
        (TypeName typeName, VChar {})
          | identifierText typeName == "Char" ->
              Right runtimeValue
        (TypeName typeName, VText {})
          | identifierText typeName == "Text" ->
              Right runtimeValue
        (TypeName hintedTypeName, VConstructor typeName typeParameters constructorName constructorArguments capturedArgs)
          | identifierText hintedTypeName == identifierText typeName,
            constructorIsSaturated constructorArguments capturedArgs -> do
              hintedCapturedArgs <-
                zipWithM
                  (applyConstructorArgumentRuntimeHint Map.empty)
                  constructorArguments
                  capturedArgs
              Right (VConstructor typeName typeParameters constructorName constructorArguments hintedCapturedArgs)
        (TypeList elementType, VList elements _) -> do
          hintedElements <- mapM (applyRuntimeTypeHint elementType) elements
          Right (VList hintedElements (Just typeHint))
        (TypeTuple elementTypes, VTuple elements)
          | length elementTypes == length elements ->
              VTuple <$> zipWithM applyRuntimeTypeHint elementTypes elements
        (TypeFunction {}, VClosure capturedEnv parameterName bodyExpr _ closureModulePath) ->
          Right (VClosure capturedEnv parameterName bodyExpr (Just typeHint) closureModulePath)
        (TypeFunction {}, _)
          | isFunctionValue runtimeValue ->
              Right (VTyped typeHint runtimeValue)
        (TypeApplication hintedTypeName hintedArguments, VConstructor typeName typeParameters constructorName constructorArguments capturedArgs)
          | identifierText hintedTypeName == identifierText typeName,
            length hintedArguments == length typeParameters -> do
              let typeParameterHints =
                    Map.fromList (zip (map identifierText typeParameters) hintedArguments)
              hintedCapturedArgs <-
                zipWithM
                  (applyConstructorArgumentRuntimeHint typeParameterHints)
                  constructorArguments
                  capturedArgs
              Right (VTyped typeHint (VConstructor typeName typeParameters constructorName constructorArguments hintedCapturedArgs))
        _ ->
          Right runtimeValue

applyConstructorArgumentRuntimeHint ::
  Map Text SignatureType ->
  DataConstructorArgument ->
  RuntimeValue ->
  Either Diagnostic RuntimeValue
applyConstructorArgumentRuntimeHint typeParameterHints constructorArgument runtimeValue =
  case constructorArgument of
    DataConstructorArgumentName argumentName ->
      attachRuntimeTypeHint (constructorArgumentRuntimeHint typeParameterHints argumentName) runtimeValue
    DataConstructorArgumentOpaque ->
      Right runtimeValue

constructorArgumentRuntimeHint :: Map Text SignatureType -> Name -> Maybe SignatureType
constructorArgumentRuntimeHint typeParameterHints argumentName =
  case Map.lookup (identifierText argumentName) typeParameterHints of
    Just hintedType -> Just hintedType
    Nothing -> concreteConstructorPayloadRuntimeHint argumentName

concreteConstructorPayloadRuntimeHint :: Name -> Maybe SignatureType
concreteConstructorPayloadRuntimeHint argumentName
  | identifierText argumentName == "Int" = Just TypeInt
  | identifierText argumentName == "Float" = Just TypeFloat
  | Just numericType <- constraintTypeNameNumericTarget argumentName =
      Just (TypeNumeric numericType)
  | identifierText argumentName == "Bool" =
      Just TypeBool
  | identifierText argumentName == "Char" = Just TypeChar
  | identifierText argumentName == "Text" = Just TypeText
  | otherwise =
      Nothing

constraintTypeNameNumericTarget :: Name -> Maybe NumericType
constraintTypeNameNumericTarget typeName =
  case identifierText typeName of
    "Int" -> Just NumericInt64
    "Int8" -> Just NumericInt8
    "Int16" -> Just NumericInt16
    "Int32" -> Just NumericInt32
    "Int64" -> Just NumericInt64
    "UInt8" -> Just NumericUInt8
    "UInt16" -> Just NumericUInt16
    "UInt32" -> Just NumericUInt32
    "UInt64" -> Just NumericUInt64
    "Float" -> Just NumericFloat64
    "Float16" -> Just NumericFloat16
    "Float32" -> Just NumericFloat32
    "Float64" -> Just NumericFloat64
    _ -> Nothing

untypedIntMetadata :: RuntimeIntMetadata
untypedIntMetadata =
  RuntimeIntMetadata {runtimeIntTargetType = Nothing}

targetedIntMetadata :: NumericType -> RuntimeIntMetadata
targetedIntMetadata targetType =
  RuntimeIntMetadata {runtimeIntTargetType = Just targetType}

untypedFloatMetadata :: Maybe FractionalLiteralSource -> RuntimeFloatMetadata
untypedFloatMetadata literalSource =
  RuntimeFloatMetadata
    { runtimeFloatLiteralSource = literalSource,
      runtimeFloatTargetType = Nothing
    }

targetedFloatMetadata :: NumericType -> RuntimeFloatMetadata
targetedFloatMetadata targetType =
  targetedFloatMetadataWithSource targetType Nothing

targetedFloatMetadataWithSource :: NumericType -> Maybe FractionalLiteralSource -> RuntimeFloatMetadata
targetedFloatMetadataWithSource targetType literalSource =
  RuntimeFloatMetadata
    { runtimeFloatLiteralSource =
        case targetType of
          NumericFloat64 -> literalSource
          _ -> Nothing,
      runtimeFloatTargetType = Just targetType
    }

evalPatternCase ::
  Maybe [Text] ->
  BuiltinResolutionMode ->
  Map BindingRuntimeHintKey SignatureType ->
  RuntimeEnv ->
  RuntimeValue ->
  [CaseArm] ->
  Either Diagnostic RuntimeValue
evalPatternCase currentModulePath builtinMode bindingTypeHints env scrutineeValue caseArms = do
  selectedArm <- selectMatchingCaseArm currentModulePath builtinMode bindingTypeHints env scrutineeValue caseArms
  case selectedArm of
    Just (armEnv, bodyExpr) ->
      evalValueWithModulePath currentModulePath builtinMode bindingTypeHints armEnv bodyExpr
    Nothing ->
      Left
        ( runtimeDiagnostic
            "E3022"
            "pattern case matched no arms"
        )

selectMatchingCaseArm ::
  Maybe [Text] ->
  BuiltinResolutionMode ->
  Map BindingRuntimeHintKey SignatureType ->
  RuntimeEnv ->
  RuntimeValue ->
  [CaseArm] ->
  Either Diagnostic (Maybe (RuntimeEnv, Expr))
selectMatchingCaseArm currentModulePath builtinMode bindingTypeHints env scrutineeValue =
  go
  where
    go remainingArms =
      case remainingArms of
        [] -> Right Nothing
        caseArm : rest ->
          chooseArm caseArm rest

    chooseArm caseArm rest =
      case matchCaseArm currentModulePath env scrutineeValue caseArm of
        Just (armEnv, guardExpr, bodyExpr) ->
          case guardExpr of
            Nothing ->
              Right (Just (armEnv, bodyExpr))
            Just conditionExpr -> do
              guardValue <- evalValueWithModulePath currentModulePath builtinMode bindingTypeHints armEnv conditionExpr
              case guardValue of
                VBool True ->
                  Right (Just (armEnv, bodyExpr))
                VBool False ->
                  go rest
                other ->
                  Left
                    ( runtimeDiagnostic
                        "E3003"
                        ("runtime case guard must be Bool, found " <> renderRuntimeType other)
                    )
        Nothing ->
          go rest

-- | Pattern bindings are prepended to the arm environment so they shadow outer
-- runtime bindings only while evaluating the selected arm body.
matchCaseArm ::
  Maybe [Text] ->
  RuntimeEnv ->
  RuntimeValue ->
  CaseArm ->
  Maybe (RuntimeEnv, Maybe Expr, Expr)
matchCaseArm currentModulePath env scrutineeValue (CaseArm pattern guardExpr bodyExpr) =
  case matchPattern currentModulePath scrutineeValue pattern of
    Just patternBindings ->
      Just (Map.union patternBindings env, guardExpr, bodyExpr)
    Nothing -> Nothing

matchPattern :: Maybe [Text] -> RuntimeValue -> Pattern -> Maybe RuntimeEnv
matchPattern currentModulePath scrutineeValue pattern =
  case pattern of
    PWildcard -> Just Map.empty
    PVariable name ->
      Just
        (Map.singleton name (Right scrutineeValue))
    PLiteral literal
      | scrutineeValue == literalRuntimeValue literal ->
          Just Map.empty
      | otherwise ->
          Nothing
    PConstructor constructorName patterns ->
      case constructorPatternScrutinee scrutineeValue of
        VConstructor _ _ valueConstructorName constructorArguments capturedArgs
          | valueConstructorName == runtimeDefinitionName currentModulePath constructorName,
            constructorIsSaturated constructorArguments capturedArgs,
            length capturedArgs == length patterns ->
              matchPatternList currentModulePath capturedArgs patterns
        _ -> Nothing
    PList patterns ->
      case scrutineeValue of
        VList elements _
          | length elements == length patterns ->
              matchPatternList currentModulePath elements patterns
        _ -> Nothing
    PConsList headPattern tailPattern ->
      case scrutineeValue of
        VList (headValue : tailValues) maybeTypeHint -> do
          headBindings <- matchPattern currentModulePath headValue headPattern
          tailBindings <- matchPattern currentModulePath (VList tailValues maybeTypeHint) tailPattern
          Just (tailBindings `Map.union` headBindings)
        _ -> Nothing
    PTuple patterns ->
      case scrutineeValue of
        VTuple elements
          | length elements == length patterns ->
              matchPatternList currentModulePath elements patterns
        _ -> Nothing
    PAs name pattern -> do
      patternBindings <- matchPattern currentModulePath scrutineeValue pattern
      Just (Map.insert name (Right scrutineeValue) patternBindings)
    POr alternatives ->
      matchFirstAlternative currentModulePath scrutineeValue alternatives

matchFirstAlternative :: Maybe [Text] -> RuntimeValue -> [Pattern] -> Maybe RuntimeEnv
matchFirstAlternative currentModulePath scrutineeValue alternatives =
  case alternatives of
    [] -> Nothing
    alternative : rest ->
      case matchPattern currentModulePath scrutineeValue alternative of
        Just patternBindings -> Just patternBindings
        Nothing -> matchFirstAlternative currentModulePath scrutineeValue rest

matchPatternList :: Maybe [Text] -> [RuntimeValue] -> [Pattern] -> Maybe RuntimeEnv
matchPatternList currentModulePath values patterns =
  foldM step Map.empty (zip values patterns)
  where
    step bindings (value, pattern) =
      case matchPattern currentModulePath value pattern of
        Just patternBindings -> Just (patternBindings `Map.union` bindings)
        Nothing -> Nothing

constructorPatternScrutinee :: RuntimeValue -> RuntimeValue
constructorPatternScrutinee runtimeValue =
  case runtimeValue of
    VTyped _ innerValue -> constructorPatternScrutinee innerValue
    VExplicitTypeApplication _ innerValue -> constructorPatternScrutinee innerValue
    VExplicitResultHint _ innerValue -> constructorPatternScrutinee innerValue
    _ -> runtimeValue

-- | Apply any callable runtime value, including sections, builtin primitives,
-- and curried operator values.
applyRuntimeFunction ::
  BuiltinResolutionMode ->
  Map BindingRuntimeHintKey SignatureType ->
  RuntimeValue ->
  RuntimeValue ->
  Either Diagnostic RuntimeValue
applyRuntimeFunction builtinMode bindingTypeHints functionValue argumentValue =
  case functionValue of
    VExplicitTypeApplication typeHint innerFunctionValue -> do
      case explicitTypeApplicationRuntimeFunctionHint typeHint innerFunctionValue of
        Just instantiatedFunctionHint ->
          applyRuntimeFunction builtinMode bindingTypeHints (VTyped instantiatedFunctionHint innerFunctionValue) argumentValue
        Nothing -> do
          resultValue <- applyRuntimeFunction builtinMode bindingTypeHints innerFunctionValue argumentValue
          applyExplicitTypeApplicationResultHint typeHint resultValue
    VExplicitResultHint typeHint innerFunctionValue -> do
      resultValue <- applyRuntimeFunction builtinMode bindingTypeHints innerFunctionValue argumentValue
      applyExplicitTypeApplicationResultHint typeHint resultValue
    VTyped typeHint innerFunctionValue -> do
      hintedArgumentValue <- applyRuntimeFunctionArgumentHint typeHint argumentValue
      resultValue <- applyRuntimeFunction builtinMode bindingTypeHints innerFunctionValue hintedArgumentValue
      applyRuntimeFunctionResultHint typeHint resultValue
    VSectionLeft operatorSymbol leftValue ->
      evalBinary builtinMode bindingTypeHints operatorSymbol leftValue argumentValue
    VSectionRight operatorSymbol rightValue ->
      evalBinary builtinMode bindingTypeHints operatorSymbol argumentValue rightValue
    VClosure capturedEnv parameterName bodyExpr maybeTypeHint closureModulePath -> do
      hintedArgumentValue <-
        case maybeTypeHint of
          Just typeHint -> applyRuntimeFunctionArgumentHint typeHint argumentValue
          Nothing -> Right argumentValue
      resultValue <-
        evalValueWithModulePath
          closureModulePath
          builtinMode
          bindingTypeHints
          (Map.insert parameterName (Right hintedArgumentValue) capturedEnv)
          bodyExpr
      case maybeTypeHint of
        Just typeHint -> applyRuntimeFunctionResultHint typeHint resultValue
        Nothing -> attachDefaultBindingIntegerTarget resultValue
    VBuiltin builtinFunction capturedArgs ->
      applyBuiltin builtinMode bindingTypeHints builtinFunction (capturedArgs ++ [argumentValue])
    VOperator operatorSymbol capturedArgs ->
      applyOperator builtinMode bindingTypeHints operatorSymbol (capturedArgs ++ [argumentValue])
    VConstructor typeName typeParameters constructorName constructorArguments capturedArgs ->
      applyConstructor typeName typeParameters constructorName constructorArguments (capturedArgs ++ [argumentValue])
    VQualifiedMethod methodKey classParameter methodSignature candidates capturedArgs ->
      applyQualifiedMethod
        builtinMode
        bindingTypeHints
        methodKey
        classParameter
        methodSignature
        candidates
        (capturedArgs ++ [argumentValue])
    _ ->
      Left
        ( runtimeDiagnostic
            "E3008"
            ("runtime cannot apply non-function value of type " <> renderRuntimeType functionValue)
        )

applyRuntimeFunctionResultHint :: SignatureType -> RuntimeValue -> Either Diagnostic RuntimeValue
applyRuntimeFunctionResultHint typeHint runtimeValue =
  case typeHint of
    TypeFunction _ resultType ->
      applyRuntimeTypeHint resultType runtimeValue
    _ ->
      Right runtimeValue

applyRuntimeFunctionArgumentHint :: SignatureType -> RuntimeValue -> Either Diagnostic RuntimeValue
applyRuntimeFunctionArgumentHint typeHint runtimeValue =
  case typeHint of
    TypeFunction argumentType _ ->
      applyRuntimeTypeHint argumentType runtimeValue
    _ ->
      Right runtimeValue

applyExplicitTypeApplicationResultHint :: SignatureType -> RuntimeValue -> Either Diagnostic RuntimeValue
applyExplicitTypeApplicationResultHint typeHint runtimeValue
  | isFunctionValue runtimeValue =
      Right (VExplicitResultHint typeHint runtimeValue)
  | runtimeValueCanAcceptTypeHint typeHint runtimeValue =
      applyRuntimeTypeHint typeHint runtimeValue
  | otherwise =
      Right runtimeValue

runtimeValueCanAcceptTypeHint :: SignatureType -> RuntimeValue -> Bool
runtimeValueCanAcceptTypeHint typeHint runtimeValue =
  case runtimeValue of
    VTyped _ innerValue ->
      runtimeValueCanAcceptTypeHint typeHint innerValue
    VExplicitTypeApplication _ innerValue ->
      runtimeValueCanAcceptTypeHint typeHint innerValue
    VExplicitResultHint _ innerValue ->
      runtimeValueCanAcceptTypeHint typeHint innerValue
    _ ->
      case (typeHint, runtimeValue) of
        (TypeInt, VInt {}) -> True
        (TypeFloat, VFloat {}) -> True
        (TypeNumeric _, VInt {}) -> True
        (TypeNumeric _, VFloat {}) -> True
        (TypeBool, VBool {}) -> True
        (TypeChar, VChar {}) -> True
        (TypeText, VText {}) -> True
        (TypeName typeName, VInt {}) ->
          identifierText typeName == "Int" || isJust (constraintTypeNameNumericTarget typeName)
        (TypeName typeName, VFloat {}) ->
          identifierText typeName == "Float" || isJust (constraintTypeNameNumericTarget typeName)
        (TypeName typeName, VBool {}) ->
          identifierText typeName == "Bool"
        (TypeName typeName, VChar {}) ->
          identifierText typeName == "Char"
        (TypeName typeName, VText {}) ->
          identifierText typeName == "Text"
        (TypeName typeName, VConstructor constructorTypeName _ _ constructorArguments capturedArgs) ->
          identifierText typeName == identifierText constructorTypeName
            && constructorIsSaturated constructorArguments capturedArgs
        (TypeApplication typeName arguments, VConstructor constructorTypeName typeParameters _ constructorArguments capturedArgs) ->
          identifierText typeName == identifierText constructorTypeName
            && length arguments == length typeParameters
            && constructorIsSaturated constructorArguments capturedArgs
        (TypeList {}, VList {}) ->
          True
        (TypeTuple elementTypes, VTuple elements) ->
          length elementTypes == length elements
        (TypeFunction {}, _) ->
          isFunctionValue runtimeValue
        _ ->
          False

explicitTypeApplicationRuntimeFunctionHint :: SignatureType -> RuntimeValue -> Maybe SignatureType
explicitTypeApplicationRuntimeFunctionHint typeHint runtimeValue = do
  explicitTypeApplicationRuntimeTemplateHint typeHint runtimeValue

explicitTypeApplicationRuntimeValueHint :: SignatureType -> RuntimeValue -> Maybe SignatureType
explicitTypeApplicationRuntimeValueHint typeHint runtimeValue =
  case explicitTypeApplicationRuntimeTemplateHint typeHint runtimeValue of
    Just instantiatedTemplate -> Just instantiatedTemplate
    Nothing -> explicitTypeApplicationRuntimeShapeHint typeHint runtimeValue

explicitTypeApplicationRuntimeTemplateHint :: SignatureType -> RuntimeValue -> Maybe SignatureType
explicitTypeApplicationRuntimeTemplateHint typeHint runtimeValue = do
  templateHint <- runtimeValueSignatureHint runtimeValue
  variableName <- listToMaybe (constraintSignatureTypeVariableNamesInOrder templateHint)
  pure (substituteSignatureTypeVariable variableName typeHint templateHint)

explicitTypeApplicationRuntimeShapeHint :: SignatureType -> RuntimeValue -> Maybe SignatureType
explicitTypeApplicationRuntimeShapeHint typeHint runtimeValue =
  case runtimeValue of
    VTyped _ innerValue ->
      explicitTypeApplicationRuntimeShapeHint typeHint innerValue
    VExplicitTypeApplication _ innerValue ->
      explicitTypeApplicationRuntimeShapeHint typeHint innerValue
    VExplicitResultHint _ innerValue ->
      explicitTypeApplicationRuntimeShapeHint typeHint innerValue
    VList {} ->
      Just (TypeList typeHint)
    VConstructor typeName typeParameters _ constructorArguments capturedArgs
      | length typeParameters == 1,
        constructorIsSaturated constructorArguments capturedArgs ->
          Just (TypeApplication typeName [typeHint])
    _ -> Nothing

runtimeValueSignatureHint :: RuntimeValue -> Maybe SignatureType
runtimeValueSignatureHint runtimeValue =
  case runtimeValue of
    VTyped typeHint _ ->
      Just typeHint
    VExplicitTypeApplication _ innerValue ->
      runtimeValueSignatureHint innerValue
    VExplicitResultHint _ innerValue ->
      runtimeValueSignatureHint innerValue
    VClosure _ _ _ maybeTypeHint _ ->
      maybeTypeHint
    VList _ (Just typeHint) ->
      Just typeHint
    _ -> Nothing

substituteSignatureTypeVariable :: Text -> SignatureType -> SignatureType -> SignatureType
substituteSignatureTypeVariable variableName replacementType signatureType =
  case signatureType of
    TypeVariable name
      | identifierText name == variableName -> replacementType
      | otherwise -> signatureType
    TypeName name
      | identifierLooksLikeTypeVariable name,
        identifierText name == variableName ->
          replacementType
      | otherwise ->
          signatureType
    TypeApplication typeName arguments ->
      TypeApplication typeName (map (substituteSignatureTypeVariable variableName replacementType) arguments)
    TypeList innerType ->
      TypeList (substituteSignatureTypeVariable variableName replacementType innerType)
    TypeTuple elementTypes ->
      TypeTuple (map (substituteSignatureTypeVariable variableName replacementType) elementTypes)
    TypeFunction argumentType resultType ->
      TypeFunction
        (substituteSignatureTypeVariable variableName replacementType argumentType)
        (substituteSignatureTypeVariable variableName replacementType resultType)
    _ -> signatureType

applyQualifiedMethod ::
  BuiltinResolutionMode ->
  Map BindingRuntimeHintKey SignatureType ->
  Text ->
  Text ->
  SignaturePayload ->
  [RuntimeMethodCandidate] ->
  [RuntimeValue] ->
  Either Diagnostic RuntimeValue
applyQualifiedMethod builtinMode bindingTypeHints methodKey classParameter methodSignature candidates arguments =
  case preferredCandidates of
    [] ->
      Left (runtimeDiagnostic "E3026" ("no matching qualified method body '" <> methodKey <> "'"))
    [RuntimeMethodCandidate _ methodCell] ->
      applyRuntimeMethodCandidate builtinMode bindingTypeHints methodCell arguments
    _
      | runtimeQualifiedMethodIsFullyApplied classParameter methodSignature arguments preferredCandidates ->
          Left (runtimeDiagnostic "E3026" ("ambiguous qualified method body '" <> methodKey <> "'"))
      | otherwise ->
          Right (VQualifiedMethod methodKey classParameter methodSignature preferredCandidates arguments)
  where
    preferredCandidates =
      case exactMatchingCandidates of
        [] -> matchingCandidates
        exactMatches -> exactMatches

    exactMatchingCandidates =
      filter
        (runtimeMethodCandidateExactlyMatches classParameter methodSignature arguments)
        matchingCandidates

    matchingCandidates =
      filter
        (runtimeMethodCandidateMatches classParameter methodSignature arguments)
        candidates

runtimeQualifiedMethodIsFullyApplied ::
  Text ->
  SignaturePayload ->
  [RuntimeValue] ->
  [RuntimeMethodCandidate] ->
  Bool
runtimeQualifiedMethodIsFullyApplied classParameter methodSignature arguments candidates =
  any candidateIsFullyApplied candidates
  where
    candidateIsFullyApplied (RuntimeMethodCandidate evidence _) =
      case substituteClassMethodSignature classParameter implTarget methodSignature of
        Just substitutedSignature ->
          let (argumentTypes, _) = constraintFunctionArgumentTypes substitutedSignature
           in length arguments >= length argumentTypes
        Nothing ->
          False
      where
        implTarget = runtimeEvidenceTarget evidence

applyRuntimeMethodCandidate ::
  BuiltinResolutionMode ->
  Map BindingRuntimeHintKey SignatureType ->
  Either Diagnostic RuntimeValue ->
  [RuntimeValue] ->
  Either Diagnostic RuntimeValue
applyRuntimeMethodCandidate builtinMode bindingTypeHints methodCell arguments = do
  methodValue <- methodCell
  foldM (applyRuntimeFunction builtinMode bindingTypeHints) methodValue arguments

runtimeMethodCandidateExactlyMatches :: Text -> SignaturePayload -> [RuntimeValue] -> RuntimeMethodCandidate -> Bool
runtimeMethodCandidateExactlyMatches classParameter methodSignature arguments (RuntimeMethodCandidate evidence _) =
  case (signaturePayloadConstraintType methodSignature, substituteClassMethodSignature classParameter implTarget methodSignature) of
    (Just genericSignature, Just substitutedSignature) ->
      let (genericArgumentTypes, _) = constraintFunctionArgumentTypes genericSignature
          (argumentTypes, _) = constraintFunctionArgumentTypes substitutedSignature
          suppliedArgumentCount = length arguments
          suppliedGenericArgumentTypes = take suppliedArgumentCount genericArgumentTypes
          suppliedArgumentTypes = take suppliedArgumentCount argumentTypes
          targetArgumentPositions =
            map (constraintSignatureTypeContainsClassParameter classParameter) suppliedGenericArgumentTypes
       in suppliedArgumentCount <= length genericArgumentTypes
            && suppliedArgumentCount <= length argumentTypes
            && or targetArgumentPositions
            && and
              ( zipWith3
                  runtimeExactCandidateArgumentMatches
                  targetArgumentPositions
                  suppliedArgumentTypes
                  arguments
              )
    _ ->
      False
  where
    implTarget = runtimeEvidenceTarget evidence

runtimeExactCandidateArgumentMatches :: Bool -> SignatureType -> RuntimeValue -> Bool
runtimeExactCandidateArgumentMatches targetArgumentPosition signatureType runtimeValue =
  not targetArgumentPosition || runtimeValueExactlyMatchesConstraint signatureType runtimeValue

runtimeValueExactlyMatchesConstraint :: SignatureType -> RuntimeValue -> Bool
runtimeValueExactlyMatchesConstraint signatureType runtimeValue =
  case runtimeValue of
    VExplicitTypeApplication _ innerValue ->
      runtimeValueExactlyMatchesConstraint signatureType innerValue
    VExplicitResultHint _ innerValue ->
      runtimeValueExactlyMatchesConstraint signatureType innerValue
    VTyped typeHint _ ->
      typeHint == signatureType
    VClosure _ _ _ (Just typeHint) _ ->
      typeHint == signatureType
    VInt _ metadata ->
      case signatureType of
        TypeInt -> runtimeIntTargetType metadata == Nothing
        TypeNumeric numericType -> runtimeIntTargetType metadata == Just numericType
        TypeName typeName ->
          runtimeIntExactlyMatchesTypeName (identifierText typeName) metadata
        _ -> False
    VFloat _ metadata ->
      case signatureType of
        TypeFloat -> runtimeFloatTargetType metadata == Nothing
        TypeNumeric numericType -> runtimeFloatTargetType metadata == Just numericType
        TypeName typeName ->
          runtimeFloatExactlyMatchesTypeName (identifierText typeName) metadata
        _ -> False
    VChar {} ->
      case signatureType of
        TypeChar -> True
        TypeName typeName -> identifierText typeName == "Char"
        _ -> False
    VText {} ->
      case signatureType of
        TypeText -> True
        TypeName typeName -> identifierText typeName == "Text"
        _ -> False
    VBool {} ->
      case signatureType of
        TypeBool -> True
        TypeName typeName -> identifierText typeName == "Bool"
        _ -> False
    VList _ (Just typeHint) ->
      typeHint == signatureType
    VList elements Nothing ->
      case signatureType of
        TypeList elementType ->
          not (null elements)
            && all (runtimeValueExactlyMatchesConstraint elementType) elements
        _ -> False
    VTuple elements ->
      case signatureType of
        TypeTuple elementTypes
          | length elementTypes == length elements ->
              and (zipWith runtimeValueExactlyMatchesConstraint elementTypes elements)
        _ -> False
    VConstructor {} ->
      case signatureType of
        TypeName typeName ->
          runtimeValueExactlyMatchesDataTypeName typeName runtimeValue
        TypeApplication typeName typeArguments ->
          runtimeValueExactlyMatchesDataTypeApplication typeName typeArguments runtimeValue
        _ -> False
    _ -> False

runtimeIntExactlyMatchesTypeName :: Text -> RuntimeIntMetadata -> Bool
runtimeIntExactlyMatchesTypeName typeName metadata =
  case (typeName, runtimeIntTargetType metadata) of
    ("Int", Nothing) -> True
    ("Int8", Just NumericInt8) -> True
    ("Int16", Just NumericInt16) -> True
    ("Int32", Just NumericInt32) -> True
    ("Int64", Just NumericInt64) -> True
    ("UInt8", Just NumericUInt8) -> True
    ("UInt16", Just NumericUInt16) -> True
    ("UInt32", Just NumericUInt32) -> True
    ("UInt64", Just NumericUInt64) -> True
    _ -> False

runtimeFloatExactlyMatchesTypeName :: Text -> RuntimeFloatMetadata -> Bool
runtimeFloatExactlyMatchesTypeName typeName metadata =
  case (typeName, runtimeFloatTargetType metadata) of
    ("Float", Nothing) -> True
    ("Float16", Just NumericFloat16) -> True
    ("Float32", Just NumericFloat32) -> True
    ("Float64", Just NumericFloat64) -> True
    _ -> False

runtimeMethodCandidateMatches :: Text -> SignaturePayload -> [RuntimeValue] -> RuntimeMethodCandidate -> Bool
runtimeMethodCandidateMatches classParameter methodSignature arguments (RuntimeMethodCandidate evidence _) =
  case substituteClassMethodSignature classParameter implTarget methodSignature of
    Just substitutedSignature ->
      let (argumentTypes, _) = constraintFunctionArgumentTypes substitutedSignature
       in length arguments <= length argumentTypes
            && and (zipWith runtimeValueMatchesConstraint argumentTypes arguments)
    Nothing ->
      False
  where
    implTarget = runtimeEvidenceTarget evidence

runtimeValueMatchesConstraint :: SignatureType -> RuntimeValue -> Bool
runtimeValueMatchesConstraint signatureType runtimeValue =
  case runtimeValue of
    VExplicitTypeApplication _ innerValue ->
      runtimeValueMatchesConstraint signatureType innerValue
    VExplicitResultHint _ innerValue ->
      runtimeValueMatchesConstraint signatureType innerValue
    VTyped typeHint _ ->
      constraintSignatureTypesCompatible typeHint signatureType
    _ ->
      case signatureType of
        TypeInt -> runtimeValueMatchesTypeName "Int" runtimeValue
        TypeFloat -> runtimeValueMatchesTypeName "Float" runtimeValue
        TypeNumeric numericType -> runtimeValueMatchesTypeName (renderNumericTypeName numericType) runtimeValue
        TypeBool -> runtimeValueMatchesTypeName "Bool" runtimeValue
        TypeChar -> runtimeValueMatchesTypeName "Char" runtimeValue
        TypeText -> runtimeValueMatchesTypeName "Text" runtimeValue
        TypeVariable {} -> False
        TypeName typeName ->
          runtimeValueMatchesTypeName (identifierText typeName) runtimeValue
        TypeApplication typeName typeArguments ->
          runtimeValueMatchesDataTypeApplication typeName typeArguments runtimeValue
        TypeList elementType ->
          case runtimeValue of
            VList elements maybeTypeHint ->
              case maybeTypeHint of
                Just typeHint -> constraintSignatureTypesCompatible typeHint signatureType
                Nothing -> all (runtimeValueMatchesConstraint elementType) elements
            _ -> False
        TypeTuple elementTypes ->
          case runtimeValue of
            VTuple elements
              | length elementTypes == length elements ->
                  and (zipWith runtimeValueMatchesConstraint elementTypes elements)
            _ -> False
        TypeFunction {} ->
          case runtimeValue of
            VClosure _ _ _ (Just typeHint) _ -> constraintSignatureTypesCompatible typeHint signatureType
            _ -> isFunctionValue runtimeValue

runtimeValueMatchesTypeName :: Text -> RuntimeValue -> Bool
runtimeValueMatchesTypeName typeName runtimeValue =
  case typeName of
    "Int" -> runtimeIntMatchesIntAlias runtimeValue
    "Int8" -> runtimeIntMatchesTarget NumericInt8 runtimeValue
    "Int16" -> runtimeIntMatchesTarget NumericInt16 runtimeValue
    "Int32" -> runtimeIntMatchesTarget NumericInt32 runtimeValue
    "Int64" -> runtimeIntMatchesTarget NumericInt64 runtimeValue
    "UInt8" -> runtimeIntMatchesTarget NumericUInt8 runtimeValue
    "UInt16" -> runtimeIntMatchesTarget NumericUInt16 runtimeValue
    "UInt32" -> runtimeIntMatchesTarget NumericUInt32 runtimeValue
    "UInt64" -> runtimeIntMatchesTarget NumericUInt64 runtimeValue
    "Float" -> runtimeFloatMatchesFloatAlias runtimeValue
    "Float16" -> runtimeFloatHasTarget NumericFloat16 runtimeValue
    "Float32" -> runtimeFloatHasTarget NumericFloat32 runtimeValue
    "Float64" -> runtimeFloatHasTarget NumericFloat64 runtimeValue
    "Bool" -> isRuntimeBool runtimeValue
    "Char" -> isRuntimeChar runtimeValue
    "Text" -> isRuntimeText runtimeValue
    _ -> runtimeValueMatchesDataTypeName typeName runtimeValue

runtimeValueMatchesDataTypeName :: Text -> RuntimeValue -> Bool
runtimeValueMatchesDataTypeName typeName runtimeValue =
  case runtimeValue of
    VConstructor valueTypeName _ _ constructorArguments capturedArgs ->
      identifierText valueTypeName == typeName
        && constructorIsSaturated constructorArguments capturedArgs
    _ -> False

runtimeValueMatchesDataTypeApplication :: Name -> [SignatureType] -> RuntimeValue -> Bool
runtimeValueMatchesDataTypeApplication typeName typeArguments runtimeValue =
  case runtimeValue of
    VConstructor valueTypeName typeParameters _ constructorArguments capturedArgs
      | valueTypeName == typeName,
        length typeParameters == length typeArguments,
        constructorIsSaturated constructorArguments capturedArgs ->
          let typeParameterBindings = Map.fromList (zip (map identifierText typeParameters) typeArguments)
           in and
                ( zipWith
                    (runtimeValueMatchesConstructorArgument typeParameterBindings)
                    constructorArguments
                    capturedArgs
                )
    _ -> False

runtimeValueExactlyMatchesDataTypeName :: Name -> RuntimeValue -> Bool
runtimeValueExactlyMatchesDataTypeName typeName runtimeValue =
  case runtimeValue of
    VConstructor valueTypeName _ _ constructorArguments capturedArgs ->
      valueTypeName == typeName
        && constructorIsSaturated constructorArguments capturedArgs
    _ -> False

runtimeValueExactlyMatchesDataTypeApplication :: Name -> [SignatureType] -> RuntimeValue -> Bool
runtimeValueExactlyMatchesDataTypeApplication typeName typeArguments runtimeValue =
  case runtimeValue of
    VConstructor valueTypeName typeParameters _ constructorArguments capturedArgs
      | valueTypeName == typeName,
        length typeParameters == length typeArguments,
        constructorIsSaturated constructorArguments capturedArgs ->
          let typeParameterBindings = Map.fromList (zip (map identifierText typeParameters) typeArguments)
           in and
                ( zipWith
                    (runtimeValueExactlyMatchesConstructorArgument typeParameterBindings)
                    constructorArguments
                    capturedArgs
                )
    _ -> False

runtimeValueMatchesConstructorArgument :: Map Text SignatureType -> DataConstructorArgument -> RuntimeValue -> Bool
runtimeValueMatchesConstructorArgument typeParameterBindings constructorArgument runtimeValue =
  case constructorArgument of
    DataConstructorArgumentName argumentName ->
      case constructorArgumentRuntimeHint typeParameterBindings argumentName of
        Just concreteArgumentType ->
          runtimeValueMatchesConstraint concreteArgumentType runtimeValue
        Nothing ->
          True
    DataConstructorArgumentOpaque ->
      True

runtimeValueExactlyMatchesConstructorArgument :: Map Text SignatureType -> DataConstructorArgument -> RuntimeValue -> Bool
runtimeValueExactlyMatchesConstructorArgument typeParameterBindings constructorArgument runtimeValue =
  case constructorArgument of
    DataConstructorArgumentName argumentName ->
      case Map.lookup (identifierText argumentName) typeParameterBindings of
        Just concreteArgumentType ->
          runtimeValueExactlyMatchesConstraint concreteArgumentType runtimeValue
        Nothing ->
          True
    DataConstructorArgumentOpaque ->
      True

runtimeIntMatchesIntAlias :: RuntimeValue -> Bool
runtimeIntMatchesIntAlias runtimeValue =
  case runtimeValue of
    VInt _ metadata ->
      case runtimeIntTargetType metadata of
        Just NumericInt64 -> True
        Just _ -> False
        Nothing -> True
    _ -> False

runtimeIntMatchesTarget :: NumericType -> RuntimeValue -> Bool
runtimeIntMatchesTarget targetType runtimeValue =
  case runtimeValue of
    VInt integerValue metadata ->
      case runtimeIntTargetType metadata of
        Just runtimeTarget -> runtimeTarget == targetType
        Nothing -> integerValueMatchesTarget targetType integerValue
    _ -> False

integerValueMatchesTarget :: NumericType -> Integer -> Bool
integerValueMatchesTarget targetType integerValue =
  case numericTypeIntegerBounds targetType of
    Just bounds -> integerValueWithinBounds integerValue bounds
    Nothing -> False

runtimeFloatMatchesFloatAlias :: RuntimeValue -> Bool
runtimeFloatMatchesFloatAlias runtimeValue =
  case runtimeValue of
    VFloat _ metadata ->
      case runtimeFloatTargetType metadata of
        Just NumericFloat64 -> True
        Just _ -> False
        Nothing -> True
    _ -> False

runtimeFloatHasTarget :: NumericType -> RuntimeValue -> Bool
runtimeFloatHasTarget targetType runtimeValue =
  case runtimeValue of
    VFloat _ metadata ->
      case runtimeFloatTargetType metadata of
        Just runtimeTarget -> runtimeTarget == targetType
        Nothing -> targetType == NumericFloat64
    _ -> False

isRuntimeBool :: RuntimeValue -> Bool
isRuntimeBool runtimeValue =
  case runtimeValue of
    VBool {} -> True
    _ -> False

isRuntimeChar :: RuntimeValue -> Bool
isRuntimeChar runtimeValue =
  case runtimeValue of
    VChar {} -> True
    _ -> False

isRuntimeText :: RuntimeValue -> Bool
isRuntimeText runtimeValue =
  case runtimeValue of
    VText {} -> True
    _ -> False

applyOperator :: BuiltinResolutionMode -> Map BindingRuntimeHintKey SignatureType -> Text -> [RuntimeValue] -> Either Diagnostic RuntimeValue
applyOperator builtinMode bindingTypeHints operatorSymbol arguments =
  case arguments of
    [leftValue] ->
      Right (VOperator operatorSymbol [leftValue])
    [leftValue, rightValue] ->
      evalBinary builtinMode bindingTypeHints operatorSymbol leftValue rightValue
    _ ->
      Left
        ( runtimeDiagnostic
            "E3016"
            ("runtime primitive '" <> operatorSymbol <> "' received invalid arguments")
        )

-- | Constructor values are curried like builtins until their declared arity is
-- saturated; extra applications are runtime errors.
applyConstructor :: Name -> [Name] -> Name -> [DataConstructorArgument] -> [RuntimeValue] -> Either Diagnostic RuntimeValue
applyConstructor typeName typeParameters constructorName constructorArguments arguments
  | length arguments <= constructorArity =
      Right (VConstructor typeName typeParameters constructorName constructorArguments arguments)
  | otherwise =
      Left
        ( runtimeDiagnostic
            "E3023"
            ( "runtime constructor '"
                <> identifierText constructorName
                <> "' expected "
                <> renderArityCount constructorArity
                <> " but received "
                <> renderArityCount (length arguments)
            )
        )
  where
    constructorArity = length constructorArguments

renderArityCount :: Int -> Text
renderArityCount count =
  Text.pack (show count) <> " " <> argumentWord
  where
    argumentWord =
      if count == 1
        then "argument"
        else "arguments"

-- | Builtin primitives are curried, so under-applied calls stay as function
-- values and only exact arity triggers evaluation.
applyBuiltin :: BuiltinResolutionMode -> Map BindingRuntimeHintKey SignatureType -> BuiltinSymbol -> [RuntimeValue] -> Either Diagnostic RuntimeValue
applyBuiltin builtinMode bindingTypeHints builtinFunction arguments
  | length arguments < builtinSymbolArity builtinFunction =
      Right (VBuiltin builtinFunction arguments)
  | length arguments == builtinSymbolArity builtinFunction =
      evalBuiltin builtinMode bindingTypeHints builtinFunction arguments
  | otherwise =
      Left
        ( runtimeDiagnostic
            "E3014"
            ("runtime primitive '" <> builtinSymbolName builtinFunction <> "' received too many arguments")
        )

-- | Evaluate builtin semantics once enough arguments have been collected.
evalBuiltin :: BuiltinResolutionMode -> Map BindingRuntimeHintKey SignatureType -> BuiltinSymbol -> [RuntimeValue] -> Either Diagnostic RuntimeValue
evalBuiltin builtinMode bindingTypeHints builtinFunction arguments =
  case (builtinFunction, arguments) of
    (_, [value])
      | Just targetType <- builtinSymbolNumericConversionTarget builtinFunction ->
          evalNumericConversion builtinFunction targetType value
    (BuiltinHd, [VList [] _]) ->
      Left (runtimeDiagnostic "E3009" "runtime primitive 'hd' failed: empty list")
    (BuiltinHd, [VList (headValue : _) maybeTypeHint]) ->
      case maybeTypeHint of
        Just (TypeList elementType) ->
          applyRuntimeTypeHint elementType headValue
        _ ->
          Right headValue
    (BuiltinHd, [other]) ->
      Left
        ( runtimeDiagnostic
            "E3011"
            ("runtime primitive 'hd' expects a list argument, found " <> renderRuntimeType other)
        )
    (BuiltinTl, [VList [] _]) ->
      Left (runtimeDiagnostic "E3010" "runtime primitive 'tl' failed: empty list")
    (BuiltinTl, [VList (_ : tailValues) maybeTypeHint]) ->
      Right (VList tailValues maybeTypeHint)
    (BuiltinTl, [other]) ->
      Left
        ( runtimeDiagnostic
            "E3012"
            ("runtime primitive 'tl' expects a list argument, found " <> renderRuntimeType other)
        )
    (BuiltinMap, [mapper, collection])
      | not (isFunctionValue mapper) ->
          Left
            ( runtimeDiagnostic
                "E3015"
                ("runtime primitive 'map' expects a function as its first argument, found " <> renderRuntimeType mapper)
            )
      | otherwise ->
          case collection of
            VList elements maybeCollectionTypeHint -> do
              mappedElements <- mapM (applyRuntimeFunction builtinMode bindingTypeHints mapper) elements
              let maybeMappedTypeHint = TypeList <$> runtimeMapResultElementType mapper maybeCollectionTypeHint
              Right (VList mappedElements maybeMappedTypeHint)
            other ->
              Left
                ( runtimeDiagnostic
                    "E3013"
                    ("runtime primitive 'map' expects a list as its second argument, found " <> renderRuntimeType other)
                )
    (BuiltinFilter, [predicate, collection])
      | not (isFunctionValue predicate) ->
          Left
            ( runtimeDiagnostic
                "E3017"
                ("runtime primitive 'filter' expects a function as its first argument, found " <> renderRuntimeType predicate)
            )
      | otherwise ->
          case collection of
            VList elements maybeTypeHint ->
              (`VList` maybeTypeHint) <$> filterElements builtinMode bindingTypeHints predicate elements
            other ->
              Left
                ( runtimeDiagnostic
                    "E3018"
                    ("runtime primitive 'filter' expects a list as its second argument, found " <> renderRuntimeType other)
                )
    -- Stub-v1 keeps `print!` side effects out of runtime plumbing; it returns
    -- its evaluated argument so expression pipelines remain deterministic.
    (BuiltinPrint, [value]) ->
      Right value
    (BuiltinTextLength, [VText textValue]) ->
      Right (VInt (fromIntegral (Text.length textValue)) untypedIntMetadata)
    (BuiltinTextLength, [other]) ->
      Left
        ( runtimeDiagnostic
            "E3028"
            ("runtime primitive 'textLength' expects a Text argument, found " <> renderRuntimeType other)
        )
    (BuiltinTextUnconsRaw, [VText textValue]) ->
      let listTypeHint = Just (TypeList (TypeTuple [TypeChar, TypeText]))
       in
        case Text.uncons textValue of
          Nothing ->
            Right (VList [] listTypeHint)
          Just (first, rest) ->
            Right (VList [VTuple [VChar first, VText rest]] listTypeHint)
    (BuiltinTextUnconsRaw, [other]) ->
      Left
        ( runtimeDiagnostic
            "E3029"
            ("runtime primitive 'textUnconsRaw' expects a Text argument, found " <> renderRuntimeType other)
        )
    _ ->
      Left
        ( runtimeDiagnostic
            "E3016"
            ("runtime primitive '" <> builtinSymbolName builtinFunction <> "' received invalid arguments")
        )

evalNumericConversion :: BuiltinSymbol -> NumericType -> RuntimeValue -> Either Diagnostic RuntimeValue
evalNumericConversion builtinFunction targetType value =
  case value of
    VExplicitTypeApplication _ innerValue ->
      evalNumericConversion builtinFunction targetType innerValue
    VExplicitResultHint _ innerValue ->
      evalNumericConversion builtinFunction targetType innerValue
    VTyped _ innerValue ->
      evalNumericConversion builtinFunction targetType innerValue
    VInt integerValue _ ->
      convertIntegerToNumericTarget builtinFunction targetType integerValue
    VFloat floatValue floatMetadata ->
      convertFloatToNumericTarget builtinFunction targetType floatValue (runtimeFloatLiteralSource floatMetadata)
    other ->
      Left
        ( runtimeDiagnostic
            "E3024"
            ( "runtime numeric conversion '"
                <> builtinSymbolName builtinFunction
                <> "' expects a numeric value, found "
                <> renderRuntimeType other
            )
        )

numericConversionBuiltinForTarget :: NumericType -> BuiltinSymbol
numericConversionBuiltinForTarget targetType =
  case targetType of
    NumericInt8 -> BuiltinToInt8
    NumericInt16 -> BuiltinToInt16
    NumericInt32 -> BuiltinToInt32
    NumericInt64 -> BuiltinToInt64
    NumericUInt8 -> BuiltinToUInt8
    NumericUInt16 -> BuiltinToUInt16
    NumericUInt32 -> BuiltinToUInt32
    NumericUInt64 -> BuiltinToUInt64
    NumericFloat16 -> BuiltinToFloat16
    NumericFloat32 -> BuiltinToFloat32
    NumericFloat64 -> BuiltinToFloat64

convertIntegerToNumericTarget :: BuiltinSymbol -> NumericType -> Integer -> Either Diagnostic RuntimeValue
convertIntegerToNumericTarget builtinFunction targetType integerValue =
  case numericTypeIntegerBounds targetType of
    Just bounds ->
      if integerValueWithinBounds integerValue bounds
        then Right (VInt integerValue (targetedIntMetadata targetType))
        else Left (numericConversionRangeDiagnostic builtinFunction targetType integerValue bounds)
    Nothing ->
      convertIntegerToFloatTarget builtinFunction targetType integerValue

convertFloatToNumericTarget :: BuiltinSymbol -> NumericType -> Double -> Maybe FractionalLiteralSource -> Either Diagnostic RuntimeValue
convertFloatToNumericTarget builtinFunction targetType floatValue literalSource
  | isNaN floatValue || isInfinite floatValue =
      Left
        ( runtimeDiagnostic
            "E3024"
            ( "runtime numeric conversion '"
                <> builtinSymbolName builtinFunction
                <> "' cannot convert non-finite Float value"
            )
        )
  | otherwise =
      case numericTypeIntegerBounds targetType of
        Just bounds ->
          convertFloatToIntegerTarget builtinFunction targetType floatValue literalSource bounds
        Nothing ->
          convertFiniteFloatToFloatTarget builtinFunction targetType floatValue literalSource

convertFloatToIntegerTarget ::
  BuiltinSymbol ->
  NumericType ->
  Double ->
  Maybe FractionalLiteralSource ->
  (Integer, Integer) ->
  Either Diagnostic RuntimeValue
convertFloatToIntegerTarget builtinFunction targetType floatValue literalSource bounds =
  case literalSource of
    Just source ->
      case fractionalLiteralIntegralValue source of
        Just integralValue
          | integerValueWithinBounds integralValue bounds ->
              Right (VInt integralValue (targetedIntMetadata targetType))
        _ ->
          Left (numericConversionFloatToIntegralDiagnostic builtinFunction targetType floatValue bounds)
    Nothing ->
      -- `round` is half-to-even, but the equality check below rejects every
      -- non-integral value instead of observing a rounding mode.
      let roundedInteger = round floatValue :: Integer
       in
        if fromInteger roundedInteger == floatValue && integerValueWithinBounds roundedInteger bounds
          then Right (VInt roundedInteger (targetedIntMetadata targetType))
          else Left (numericConversionFloatToIntegralDiagnostic builtinFunction targetType floatValue bounds)

convertIntegerToFloatTarget :: BuiltinSymbol -> NumericType -> Integer -> Either Diagnostic RuntimeValue
convertIntegerToFloatTarget builtinFunction targetType integerValue =
  if integerExceedsFloatTarget targetType integerValue
    then Left (numericConversionFloatOverflowDiagnostic builtinFunction targetType)
    else
      let floatValue = fromInteger integerValue :: Double
       in
        if isInfinite floatValue || exceedsFloatTarget targetType floatValue
          then Left (numericConversionFloatOverflowDiagnostic builtinFunction targetType)
          else Right (VFloat (roundFloatTarget targetType floatValue) (targetedFloatMetadata targetType))

integerExceedsFloatTarget :: NumericType -> Integer -> Bool
integerExceedsFloatTarget targetType integerValue =
  case numericTypeFloatMax targetType of
    Just maxMagnitude ->
      abs integerValue > (floor maxMagnitude :: Integer)
    Nothing -> False

convertFiniteFloatToFloatTarget :: BuiltinSymbol -> NumericType -> Double -> Maybe FractionalLiteralSource -> Either Diagnostic RuntimeValue
convertFiniteFloatToFloatTarget builtinFunction targetType floatValue literalSource =
  if exceedsFloatTarget targetType floatValue || sourceExceedsFloatTarget targetType literalSource
    then Left (numericConversionFloatOverflowDiagnostic builtinFunction targetType)
    else Right (VFloat (roundFloatTarget targetType floatValue) (targetedFloatMetadataWithSource targetType literalSource))

roundFloatTarget :: NumericType -> Double -> Double
roundFloatTarget targetType value =
  case targetType of
    NumericFloat16 -> roundFloat16 value
    NumericFloat32 -> realToFrac (realToFrac value :: Float)
    _ -> value

roundFloat16 :: Double -> Double
roundFloat16 value
  | value == 0 = 0
  | magnitude < (halfMinSubnormal / 2.0) = 0
  | magnitude < halfMinNormal =
      withSign (fromInteger (round (magnitude / halfMinSubnormal) :: Integer) * halfMinSubnormal)
  | otherwise =
      let exponentValue = floor (logBase 2 magnitude) :: Int
          unit = 2.0 ** fromIntegral (exponentValue - 10)
          roundedMagnitude = fromInteger (round (magnitude / unit) :: Integer) * unit
       in withSign (min float16MaxFinite roundedMagnitude)
  where
    magnitude = abs value
    float16MaxFinite = 65504.0 :: Double
    halfMinNormal = 2.0 ** (-14.0 :: Double)
    halfMinSubnormal = 2.0 ** (-24.0 :: Double)
    withSign roundedMagnitude =
      if value < 0
        then negate roundedMagnitude
        else roundedMagnitude

exceedsFloatTarget :: NumericType -> Double -> Bool
exceedsFloatTarget targetType value =
  case numericTypeFloatMax targetType of
    Just maxMagnitude -> abs value > maxMagnitude
    Nothing -> False

sourceExceedsFloatTarget :: NumericType -> Maybe FractionalLiteralSource -> Bool
sourceExceedsFloatTarget targetType literalSource =
  case (numericTypeFloatMax targetType, literalSource) of
    (Just maxMagnitude, Just source) ->
      fractionalLiteralExceedsMagnitude source maxMagnitude
    _ -> False

integerValueWithinBounds :: Integer -> (Integer, Integer) -> Bool
integerValueWithinBounds value (lowerBound, upperBound) =
  value >= lowerBound && value <= upperBound

numericConversionRangeDiagnostic :: BuiltinSymbol -> NumericType -> Integer -> (Integer, Integer) -> Diagnostic
numericConversionRangeDiagnostic builtinFunction targetType value (lowerBound, upperBound) =
  runtimeDiagnostic
    "E3024"
    ( "runtime numeric conversion '"
        <> builtinSymbolName builtinFunction
        <> "' failed: integer value "
        <> Text.pack (show value)
        <> " outside "
        <> renderNumericTypeName targetType
        <> " range "
        <> Text.pack (show lowerBound)
        <> ".."
        <> Text.pack (show upperBound)
    )

numericConversionFloatToIntegralDiagnostic :: BuiltinSymbol -> NumericType -> Double -> (Integer, Integer) -> Diagnostic
numericConversionFloatToIntegralDiagnostic builtinFunction targetType value (lowerBound, upperBound) =
  runtimeDiagnostic
    "E3024"
    ( "runtime numeric conversion '"
        <> builtinSymbolName builtinFunction
        <> "' failed: Float value "
        <> Text.pack (show value)
        <> " must be integral and inside "
        <> renderNumericTypeName targetType
        <> " range "
        <> Text.pack (show lowerBound)
        <> ".."
        <> Text.pack (show upperBound)
    )

numericConversionFloatOverflowDiagnostic :: BuiltinSymbol -> NumericType -> Diagnostic
numericConversionFloatOverflowDiagnostic builtinFunction targetType =
  runtimeDiagnostic
    "E3024"
    ( "runtime numeric conversion '"
        <> builtinSymbolName builtinFunction
        <> "' failed: value cannot be represented as finite "
        <> renderNumericTypeName targetType
    )

-- | Evaluate filter predicates element-by-element and enforce that each
-- predicate application returns a Bool.
filterElements :: BuiltinResolutionMode -> Map BindingRuntimeHintKey SignatureType -> RuntimeValue -> [RuntimeValue] -> Either Diagnostic [RuntimeValue]
filterElements builtinMode bindingTypeHints predicate values = do
  results <- mapM applyPredicate values
  pure [value | (value, True) <- results]
  where
    -- Preserve runtime safety for partially-known function values that can slip
    -- past compile-time checks in direct `evaluateRuntimeExpr` tests.
    applyPredicate :: RuntimeValue -> Either Diagnostic (RuntimeValue, Bool)
    applyPredicate value = do
      predicateResult <- applyRuntimeFunction builtinMode bindingTypeHints predicate value
      case predicateResult of
        VBool shouldKeep -> Right (value, shouldKeep)
        other ->
          Left
            ( runtimeDiagnostic
                "E3019"
                ("runtime primitive 'filter' predicate must return Bool, found " <> renderRuntimeType other)
            )

runtimeFunctionResultType :: RuntimeValue -> Maybe SignatureType
runtimeFunctionResultType runtimeValue =
  case runtimeValue of
    VExplicitTypeApplication _ innerValue ->
      runtimeFunctionResultType innerValue
    VExplicitResultHint _ innerValue ->
      runtimeFunctionResultType innerValue
    VTyped (TypeFunction _ resultType) _ ->
      Just resultType
    VClosure _ _ _ (Just (TypeFunction _ resultType)) _ ->
      Just resultType
    _ ->
      Nothing

runtimeMapResultElementType :: RuntimeValue -> Maybe SignatureType -> Maybe SignatureType
runtimeMapResultElementType mapper maybeCollectionTypeHint =
  case runtimeFunctionResultType mapper of
    Just resultType ->
      Just resultType
    Nothing ->
      runtimeBuiltinMapResultElementType mapper maybeCollectionTypeHint

runtimeBuiltinMapResultElementType :: RuntimeValue -> Maybe SignatureType -> Maybe SignatureType
runtimeBuiltinMapResultElementType mapper maybeCollectionTypeHint =
  case (mapper, maybeCollectionTypeHint) of
    (VBuiltin BuiltinHd [], Just (TypeList (TypeList elementType))) ->
      Just elementType
    (VClosure _ parameterName (EVar resultName) Nothing _, Just (TypeList elementType))
      | resultName == parameterName ->
          Just elementType
    _ ->
      Nothing

attachDefaultBindingIntegerTarget :: RuntimeValue -> Either Diagnostic RuntimeValue
attachDefaultBindingIntegerTarget runtimeValue =
  case runtimeValue of
    VInt integerValue metadata
      | runtimeIntTargetType metadata == Nothing,
        integerValueMatchesTarget NumericInt64 integerValue ->
          Right (VInt integerValue (targetedIntMetadata NumericInt64))
    VList elements maybeTypeHint ->
      (`VList` maybeTypeHint) <$> traverse attachDefaultBindingIntegerTarget elements
    VTuple elements ->
      VTuple <$> traverse attachDefaultBindingIntegerTarget elements
    VBuiltin builtinSymbol capturedArgs ->
      VBuiltin builtinSymbol <$> traverse attachDefaultBindingIntegerTarget capturedArgs
    VOperator operatorSymbol capturedArgs ->
      VOperator operatorSymbol <$> traverse attachDefaultBindingIntegerTarget capturedArgs
    VSectionLeft operatorSymbol operand ->
      VSectionLeft operatorSymbol <$> attachDefaultBindingIntegerTarget operand
    VSectionRight operatorSymbol operand ->
      VSectionRight operatorSymbol <$> attachDefaultBindingIntegerTarget operand
    VConstructor typeName typeParameters constructorName constructorArguments capturedArgs ->
      VConstructor typeName typeParameters constructorName constructorArguments
        <$> traverse attachDefaultBindingIntegerTarget capturedArgs
    VQualifiedMethod methodKey classParameter methodSignature candidates capturedArgs ->
      VQualifiedMethod methodKey classParameter methodSignature candidates
        <$> traverse attachDefaultBindingIntegerTarget capturedArgs
    VTyped typeHint innerValue
      | TypeFunction {} <- typeHint ->
          Right (VTyped typeHint innerValue)
      | otherwise ->
          VTyped typeHint <$> attachDefaultBindingIntegerTarget innerValue
    VExplicitTypeApplication typeHint innerValue ->
      VExplicitTypeApplication typeHint <$> attachDefaultBindingIntegerTarget innerValue
    VExplicitResultHint typeHint innerValue ->
      VExplicitResultHint typeHint <$> attachDefaultBindingIntegerTarget innerValue
    _ ->
      Right runtimeValue

isFunctionValue :: RuntimeValue -> Bool
isFunctionValue value =
  case value of
    VExplicitTypeApplication _ innerValue -> isFunctionValue innerValue
    VExplicitResultHint _ innerValue -> isFunctionValue innerValue
    VTyped _ innerValue -> isFunctionValue innerValue
    VSectionLeft {} -> True
    VSectionRight {} -> True
    VClosure {} -> True
    VBuiltin {} -> True
    VOperator {} -> True
    VConstructor _ _ _ constructorArguments capturedArgs ->
      not (constructorIsSaturated constructorArguments capturedArgs)
    VQualifiedMethod {} -> True
    _ -> False

-- | Evaluate the builtin operator subset supported by the runtime.
evalBinary :: BuiltinResolutionMode -> Map BindingRuntimeHintKey SignatureType -> Text -> RuntimeValue -> RuntimeValue -> Either Diagnostic RuntimeValue
evalBinary builtinMode bindingTypeHints operatorSymbol leftValue rightValue
  | isStrictEqualityOperator operatorSymbol,
    isFunctionValue leftValue || isFunctionValue rightValue =
      Left (runtimeCallableEqualityDiagnostic operatorSymbol leftValue rightValue)
  | otherwise =
  case (operatorSymbol, leftValue, rightValue) of
    ("$", functionValue, argumentValue) ->
      applyRuntimeFunction builtinMode bindingTypeHints functionValue argumentValue
    (_, VTyped leftTypeHint leftInnerValue, _)
      | isStrictEqualityOperator operatorSymbol,
        runtimeTypeHintRequiresStructuralEquality leftTypeHint ->
          evalStructuralEquality operatorSymbol leftValue rightValue
      | otherwise ->
          preserveLeftTypedNumericOperatorResult operatorSymbol leftTypeHint
            =<< evalBinary builtinMode bindingTypeHints operatorSymbol leftInnerValue rightValue
    (_, _, VTyped rightTypeHint rightInnerValue)
      | isStrictEqualityOperator operatorSymbol,
        runtimeTypeHintRequiresStructuralEquality rightTypeHint ->
          evalStructuralEquality operatorSymbol leftValue rightValue
      | otherwise ->
          preserveRightTypedNumericOperatorResult operatorSymbol leftValue rightTypeHint
            =<< evalBinary builtinMode bindingTypeHints operatorSymbol leftValue rightInnerValue
    ("+", VInt leftInt leftMetadata, VInt rightInt rightMetadata) ->
      evalIntegerArithmetic "+" leftMetadata rightMetadata (leftInt + rightInt)
    ("-", VInt leftInt leftMetadata, VInt rightInt rightMetadata) ->
      evalIntegerArithmetic "-" leftMetadata rightMetadata (leftInt - rightInt)
    ("*", VInt leftInt leftMetadata, VInt rightInt rightMetadata) ->
      evalIntegerArithmetic "*" leftMetadata rightMetadata (leftInt * rightInt)
    ("/", VInt _ _, VInt 0 _) ->
      Left (runtimeDiagnostic "E3001" "runtime primitive '/' failed: division by zero")
    ("/", VInt leftInt leftMetadata, VInt rightInt rightMetadata) ->
      evalIntegerArithmetic "/" leftMetadata rightMetadata (leftInt `div` rightInt)
    ("+", VFloat leftFloat leftMetadata, VFloat rightFloat rightMetadata) ->
      evalFloatArithmetic "+" leftMetadata rightMetadata (leftFloat + rightFloat)
    ("-", VFloat leftFloat leftMetadata, VFloat rightFloat rightMetadata) ->
      evalFloatArithmetic "-" leftMetadata rightMetadata (leftFloat - rightFloat)
    ("*", VFloat leftFloat leftMetadata, VFloat rightFloat rightMetadata) ->
      evalFloatArithmetic "*" leftMetadata rightMetadata (leftFloat * rightFloat)
    ("/", VFloat _ _, VFloat rightFloat _)
      | floatIsZero rightFloat ->
          Left (runtimeDiagnostic "E3001" "runtime primitive '/' failed: division by zero")
    ("/", VFloat leftFloat leftMetadata, VFloat rightFloat rightMetadata) ->
      evalFloatArithmetic "/" leftMetadata rightMetadata (leftFloat / rightFloat)
    ("+", VInt leftInt leftMetadata, VFloat rightFloat rightMetadata)
      | runtimeIntFloat64PromotionAccepted leftMetadata rightMetadata ->
          evalIntegerFloat64Arithmetic "+" rightMetadata leftInt rightFloat (+)
    ("+", VFloat leftFloat leftMetadata, VInt rightInt rightMetadata)
      | runtimeIntFloat64PromotionAccepted rightMetadata leftMetadata ->
          evalFloat64IntegerArithmetic "+" leftMetadata leftFloat rightInt (+)
    ("-", VInt leftInt leftMetadata, VFloat rightFloat rightMetadata)
      | runtimeIntFloat64PromotionAccepted leftMetadata rightMetadata ->
          evalIntegerFloat64Arithmetic "-" rightMetadata leftInt rightFloat (-)
    ("-", VFloat leftFloat leftMetadata, VInt rightInt rightMetadata)
      | runtimeIntFloat64PromotionAccepted rightMetadata leftMetadata ->
          evalFloat64IntegerArithmetic "-" leftMetadata leftFloat rightInt (-)
    ("*", VInt leftInt leftMetadata, VFloat rightFloat rightMetadata)
      | runtimeIntFloat64PromotionAccepted leftMetadata rightMetadata ->
          evalIntegerFloat64Arithmetic "*" rightMetadata leftInt rightFloat (*)
    ("*", VFloat leftFloat leftMetadata, VInt rightInt rightMetadata)
      | runtimeIntFloat64PromotionAccepted rightMetadata leftMetadata ->
          evalFloat64IntegerArithmetic "*" leftMetadata leftFloat rightInt (*)
    ("/", VInt _ leftMetadata, VFloat rightFloat rightMetadata)
      | runtimeIntFloat64PromotionAccepted leftMetadata rightMetadata,
        floatIsZero rightFloat ->
          Left (runtimeDiagnostic "E3001" "runtime primitive '/' failed: division by zero")
    ("/", VInt leftInt leftMetadata, VFloat rightFloat rightMetadata)
      | runtimeIntFloat64PromotionAccepted leftMetadata rightMetadata ->
          evalIntegerFloat64Arithmetic "/" rightMetadata leftInt rightFloat (/)
    ("/", VFloat _ leftMetadata, VInt 0 rightMetadata)
      | runtimeIntFloat64PromotionAccepted rightMetadata leftMetadata ->
          Left (runtimeDiagnostic "E3001" "runtime primitive '/' failed: division by zero")
    ("/", VFloat leftFloat leftMetadata, VInt rightInt rightMetadata)
      | runtimeIntFloat64PromotionAccepted rightMetadata leftMetadata ->
          evalFloat64IntegerArithmetic "/" leftMetadata leftFloat rightInt (/)
    ("<", VInt leftInt leftMetadata, VInt rightInt rightMetadata) ->
      evalIntegerPredicate "<" leftInt leftMetadata rightInt rightMetadata (leftInt < rightInt)
    ("<=", VInt leftInt leftMetadata, VInt rightInt rightMetadata) ->
      evalIntegerPredicate "<=" leftInt leftMetadata rightInt rightMetadata (leftInt <= rightInt)
    (">", VInt leftInt leftMetadata, VInt rightInt rightMetadata) ->
      evalIntegerPredicate ">" leftInt leftMetadata rightInt rightMetadata (leftInt > rightInt)
    (">=", VInt leftInt leftMetadata, VInt rightInt rightMetadata) ->
      evalIntegerPredicate ">=" leftInt leftMetadata rightInt rightMetadata (leftInt >= rightInt)
    ("<", VFloat leftFloat leftMetadata, VFloat rightFloat rightMetadata) ->
      evalFloatPredicate "<" leftMetadata rightMetadata (leftFloat < rightFloat)
    ("<=", VFloat leftFloat leftMetadata, VFloat rightFloat rightMetadata) ->
      evalFloatPredicate "<=" leftMetadata rightMetadata (leftFloat <= rightFloat)
    (">", VFloat leftFloat leftMetadata, VFloat rightFloat rightMetadata) ->
      evalFloatPredicate ">" leftMetadata rightMetadata (leftFloat > rightFloat)
    (">=", VFloat leftFloat leftMetadata, VFloat rightFloat rightMetadata) ->
      evalFloatPredicate ">=" leftMetadata rightMetadata (leftFloat >= rightFloat)
    ("<", VInt leftInt leftMetadata, VFloat rightFloat rightMetadata)
      | runtimeIntFloat64ComparisonPromotionAccepted leftMetadata rightMetadata ->
          evalIntegerFloat64Predicate "<" leftInt rightFloat (<)
    ("<", VFloat leftFloat leftMetadata, VInt rightInt rightMetadata)
      | runtimeIntFloat64ComparisonPromotionAccepted rightMetadata leftMetadata ->
          evalFloat64IntegerPredicate "<" leftFloat rightInt (<)
    ("<=", VInt leftInt leftMetadata, VFloat rightFloat rightMetadata)
      | runtimeIntFloat64ComparisonPromotionAccepted leftMetadata rightMetadata ->
          evalIntegerFloat64Predicate "<=" leftInt rightFloat (<=)
    ("<=", VFloat leftFloat leftMetadata, VInt rightInt rightMetadata)
      | runtimeIntFloat64ComparisonPromotionAccepted rightMetadata leftMetadata ->
          evalFloat64IntegerPredicate "<=" leftFloat rightInt (<=)
    (">", VInt leftInt leftMetadata, VFloat rightFloat rightMetadata)
      | runtimeIntFloat64ComparisonPromotionAccepted leftMetadata rightMetadata ->
          evalIntegerFloat64Predicate ">" leftInt rightFloat (>)
    (">", VFloat leftFloat leftMetadata, VInt rightInt rightMetadata)
      | runtimeIntFloat64ComparisonPromotionAccepted rightMetadata leftMetadata ->
          evalFloat64IntegerPredicate ">" leftFloat rightInt (>)
    (">=", VInt leftInt leftMetadata, VFloat rightFloat rightMetadata)
      | runtimeIntFloat64ComparisonPromotionAccepted leftMetadata rightMetadata ->
          evalIntegerFloat64Predicate ">=" leftInt rightFloat (>=)
    (">=", VFloat leftFloat leftMetadata, VInt rightInt rightMetadata)
      | runtimeIntFloat64ComparisonPromotionAccepted rightMetadata leftMetadata ->
          evalFloat64IntegerPredicate ">=" leftFloat rightInt (>=)
    ("==", VInt leftInt leftMetadata, VInt rightInt rightMetadata) ->
      evalIntegerEquality "==" leftInt leftMetadata rightInt rightMetadata
    ("==", VFloat leftFloat leftMetadata, VFloat rightFloat rightMetadata) ->
      evalFloatPredicate "==" leftMetadata rightMetadata (leftFloat == rightFloat)
    ("==", VInt leftInt leftMetadata, VFloat rightFloat rightMetadata)
      | runtimeIntFloat64ComparisonPromotionAccepted leftMetadata rightMetadata ->
          evalIntegerFloat64Equality "==" leftInt rightFloat
    ("==", VFloat leftFloat leftMetadata, VInt rightInt rightMetadata)
      | runtimeIntFloat64ComparisonPromotionAccepted rightMetadata leftMetadata ->
          evalFloat64IntegerEquality "==" leftFloat rightInt
    ("==", VBool leftBool, VBool rightBool) -> Right (VBool (leftBool == rightBool))
    ("==", VChar leftChar, VChar rightChar) -> Right (VBool (leftChar == rightChar))
    ("==", VText leftText, VText rightText) -> Right (VBool (leftText == rightText))
    ("==", VList {}, VList {}) -> evalStructuralEquality "==" leftValue rightValue
    ("==", VTuple {}, VTuple {}) -> evalStructuralEquality "==" leftValue rightValue
    ("==", VConstructor {}, VConstructor {}) -> evalStructuralEquality "==" leftValue rightValue
    ("!=", VInt leftInt leftMetadata, VInt rightInt rightMetadata) ->
      evalIntegerEquality "!=" leftInt leftMetadata rightInt rightMetadata
    ("!=", VFloat leftFloat leftMetadata, VFloat rightFloat rightMetadata) ->
      evalFloatPredicate "!=" leftMetadata rightMetadata (leftFloat /= rightFloat)
    ("!=", VInt leftInt leftMetadata, VFloat rightFloat rightMetadata)
      | runtimeIntFloat64ComparisonPromotionAccepted leftMetadata rightMetadata ->
          evalIntegerFloat64Equality "!=" leftInt rightFloat
    ("!=", VFloat leftFloat leftMetadata, VInt rightInt rightMetadata)
      | runtimeIntFloat64ComparisonPromotionAccepted rightMetadata leftMetadata ->
          evalFloat64IntegerEquality "!=" leftFloat rightInt
    ("!=", VBool leftBool, VBool rightBool) -> Right (VBool (leftBool /= rightBool))
    ("!=", VChar leftChar, VChar rightChar) -> Right (VBool (leftChar /= rightChar))
    ("!=", VText leftText, VText rightText) -> Right (VBool (leftText /= rightText))
    ("!=", VList {}, VList {}) -> evalStructuralEquality "!=" leftValue rightValue
    ("!=", VTuple {}, VTuple {}) -> evalStructuralEquality "!=" leftValue rightValue
    ("!=", VConstructor {}, VConstructor {}) -> evalStructuralEquality "!=" leftValue rightValue
    _ ->
      Left
        ( runtimeDiagnostic
            "E3007"
            ( "runtime primitive '"
                <> operatorSymbol
                <> "' cannot be applied to "
                <> renderRuntimeType leftValue
                <> " and "
                <> renderRuntimeType rightValue
            )
        )

isStrictEqualityOperator :: Text -> Bool
isStrictEqualityOperator operatorSymbol =
  operatorSymbol == "==" || operatorSymbol == "!="

preserveLeftTypedNumericOperatorResult :: Text -> SignatureType -> RuntimeValue -> Either Diagnostic RuntimeValue
preserveLeftTypedNumericOperatorResult operatorSymbol typeHint runtimeValue
  | numericArithmeticOperator operatorSymbol,
    numericAliasTypeHint typeHint,
    runtimeValueMatchesConstraint typeHint runtimeValue =
      applyRuntimeTypeHint typeHint runtimeValue
  | otherwise =
      Right runtimeValue

preserveRightTypedNumericOperatorResult :: Text -> RuntimeValue -> SignatureType -> RuntimeValue -> Either Diagnostic RuntimeValue
preserveRightTypedNumericOperatorResult operatorSymbol leftValue typeHint runtimeValue
  | numericArithmeticOperator operatorSymbol,
    numericAliasTypeHint typeHint,
    not (runtimeValueHasTargetedNumericMetadata leftValue),
    runtimeValueMatchesConstraint typeHint runtimeValue =
      applyRuntimeTypeHint typeHint runtimeValue
  | otherwise =
      Right runtimeValue

numericArithmeticOperator :: Text -> Bool
numericArithmeticOperator operatorSymbol =
  operatorSymbol == "+" || operatorSymbol == "-" || operatorSymbol == "*" || operatorSymbol == "/"

numericAliasTypeHint :: SignatureType -> Bool
numericAliasTypeHint typeHint =
  case typeHint of
    TypeInt -> True
    TypeFloat -> True
    TypeName typeName ->
      identifierText typeName == "Int" || identifierText typeName == "Float"
    _ ->
      False

runtimeValueHasTargetedNumericMetadata :: RuntimeValue -> Bool
runtimeValueHasTargetedNumericMetadata runtimeValue =
  case runtimeValue of
    VInt _ metadata ->
      runtimeIntTargetType metadata /= Nothing
    VFloat _ metadata ->
      runtimeFloatTargetType metadata /= Nothing
    _ ->
      False

runtimeTypeHintRequiresStructuralEquality :: SignatureType -> Bool
runtimeTypeHintRequiresStructuralEquality signatureType =
  case signatureType of
    TypeApplication {} -> True
    TypeList {} -> True
    TypeTuple {} -> True
    _ -> False

runtimeCallableEqualityDiagnostic :: Text -> RuntimeValue -> RuntimeValue -> Diagnostic
runtimeCallableEqualityDiagnostic operatorSymbol leftValue rightValue =
  runtimeDiagnostic
    "E3007"
    ( "runtime primitive '"
        <> operatorSymbol
        <> "' cannot compare callable values; callable values are not equality-supported, found "
        <> renderRuntimeType leftValue
        <> " and "
        <> renderRuntimeType rightValue
    )

evalIntegerArithmetic ::
  Text ->
  RuntimeIntMetadata ->
  RuntimeIntMetadata ->
  Integer ->
  Either Diagnostic RuntimeValue
evalIntegerArithmetic operatorSymbol leftMetadata rightMetadata result = do
  targetType <- selectIntegerBinaryTarget operatorSymbol leftMetadata rightMetadata
  evalIntegerBinary operatorSymbol targetType result

selectIntegerBinaryTarget :: Text -> RuntimeIntMetadata -> RuntimeIntMetadata -> Either Diagnostic (Maybe NumericType)
selectIntegerBinaryTarget operatorSymbol leftMetadata rightMetadata =
  case (runtimeIntTargetType leftMetadata, runtimeIntTargetType rightMetadata) of
    (Just leftTarget, Just rightTarget)
      | leftTarget == rightTarget -> Right (Just leftTarget)
      | otherwise -> Left (mixedIntegerArithmeticDiagnostic operatorSymbol (Just leftTarget) (Just rightTarget))
    (Just leftTarget, Nothing) -> Right (Just leftTarget)
    (Nothing, Just rightTarget) -> Right (Just rightTarget)
    _ -> Right Nothing

evalIntegerBinary :: Text -> Maybe NumericType -> Integer -> Either Diagnostic RuntimeValue
evalIntegerBinary operatorSymbol maybeTarget result =
  case maybeTarget of
    Just targetType ->
      case numericTypeIntegerBounds targetType of
        Just bounds
          | integerValueWithinBounds result bounds ->
              Right (VInt result (targetedIntMetadata targetType))
          | otherwise ->
              Left (runtimeIntegerArithmeticOverflowDiagnostic operatorSymbol targetType result bounds)
        Nothing ->
          Right (VInt result (targetedIntMetadata targetType))
    Nothing ->
      Right (VInt result untypedIntMetadata)

mixedIntegerArithmeticDiagnostic :: Text -> Maybe NumericType -> Maybe NumericType -> Diagnostic
mixedIntegerArithmeticDiagnostic operatorSymbol leftTarget rightTarget =
  runtimeDiagnostic
    "E3007"
    ( "runtime primitive '"
        <> operatorSymbol
        <> "' cannot mix "
        <> renderIntegerOperandTarget leftTarget
        <> " and "
        <> renderIntegerOperandTarget rightTarget
    )

renderIntegerOperandTarget :: Maybe NumericType -> Text
renderIntegerOperandTarget maybeTarget =
  case maybeTarget of
    Just targetType -> renderNumericTypeName targetType
    Nothing -> "Int"

runtimeIntegerArithmeticOverflowDiagnostic :: Text -> NumericType -> Integer -> (Integer, Integer) -> Diagnostic
runtimeIntegerArithmeticOverflowDiagnostic operatorSymbol targetType result (lowerBound, upperBound) =
  runtimeDiagnostic
    "E3025"
    ( "runtime primitive '"
        <> operatorSymbol
        <> "' failed: integer value "
        <> Text.pack (show result)
        <> " outside "
        <> renderNumericTypeName targetType
        <> " range "
        <> Text.pack (show lowerBound)
        <> ".."
        <> Text.pack (show upperBound)
    )

floatIsZero :: Double -> Bool
floatIsZero value =
  -- Jazz's finite runtime primitive subset treats both signed zeroes as
  -- division by zero rather than producing infinities.
  value == 0

evalFloatArithmetic ::
  Text ->
  RuntimeFloatMetadata ->
  RuntimeFloatMetadata ->
  Double ->
  Either Diagnostic RuntimeValue
evalFloatArithmetic operatorSymbol leftMetadata rightMetadata result = do
  targetType <- selectFloatBinaryTarget operatorSymbol leftMetadata rightMetadata
  evalFloatBinary operatorSymbol targetType result

selectFloatBinaryTarget :: Text -> RuntimeFloatMetadata -> RuntimeFloatMetadata -> Either Diagnostic (Maybe NumericType)
selectFloatBinaryTarget operatorSymbol leftMetadata rightMetadata =
  case (runtimeFloatTargetType leftMetadata, runtimeFloatTargetType rightMetadata) of
    (Just leftTarget, Just rightTarget)
      | leftTarget == rightTarget -> Right (Just leftTarget)
      | otherwise -> Left (mixedFloatArithmeticDiagnostic operatorSymbol (Just leftTarget) (Just rightTarget))
    (Just NumericFloat64, Nothing) -> Right (Just NumericFloat64)
    (Nothing, Just NumericFloat64) -> Right (Just NumericFloat64)
    (Just targetType, Nothing) -> Left (mixedFloatArithmeticDiagnostic operatorSymbol (Just targetType) Nothing)
    (Nothing, Just targetType) -> Left (mixedFloatArithmeticDiagnostic operatorSymbol Nothing (Just targetType))
    (Nothing, Nothing) -> Right Nothing

evalFloatBinary :: Text -> Maybe NumericType -> Double -> Either Diagnostic RuntimeValue
evalFloatBinary operatorSymbol targetType result
  | isNaN result || isInfinite result =
      Left
        ( runtimeDiagnostic
            "E3025"
            ("runtime primitive '" <> operatorSymbol <> "' failed: non-finite Float result")
        )
  | Just floatTarget <- targetType,
    exceedsFloatTarget floatTarget result =
      Left (runtimeFloatArithmeticOverflowDiagnostic operatorSymbol floatTarget)
  | Just floatTarget <- targetType =
      Right (VFloat (roundFloatTarget floatTarget result) (targetedFloatMetadata floatTarget))
  | otherwise = Right (VFloat result (untypedFloatMetadata Nothing))

runtimeIntFloat64PromotionAccepted :: RuntimeIntMetadata -> RuntimeFloatMetadata -> Bool
runtimeIntFloat64PromotionAccepted intMetadata floatMetadata =
  runtimeIntMetadataIsIntegral intMetadata
    && runtimeFloatMetadataIsFloat64Domain floatMetadata

runtimeIntFloat64ComparisonPromotionAccepted :: RuntimeIntMetadata -> RuntimeFloatMetadata -> Bool
runtimeIntFloat64ComparisonPromotionAccepted intMetadata floatMetadata =
  runtimeIntMetadataIsIntegral intMetadata
    && runtimeFloatMetadataIsFloat64Domain floatMetadata

runtimeIntMetadataIsIntegral :: RuntimeIntMetadata -> Bool
runtimeIntMetadataIsIntegral intMetadata =
  case runtimeIntTargetType intMetadata of
    Just numericType -> numericTypeIsIntegral numericType
    Nothing -> True

runtimeFloatMetadataIsFloat64Domain :: RuntimeFloatMetadata -> Bool
runtimeFloatMetadataIsFloat64Domain floatMetadata =
  case runtimeFloatTargetType floatMetadata of
    Just NumericFloat64 -> True
    Nothing -> True
    Just _ -> False

evalIntegerFloat64Arithmetic :: Text -> RuntimeFloatMetadata -> Integer -> Double -> (Double -> Double -> Double) -> Either Diagnostic RuntimeValue
evalIntegerFloat64Arithmetic operatorSymbol floatMetadata integerValue floatValue combine = do
  integerFloat <- promotedIntegerFloat64Operand integerValue
  evalFloatBinary operatorSymbol (runtimeFloatTargetType floatMetadata) (combine integerFloat floatValue)

evalFloat64IntegerArithmetic :: Text -> RuntimeFloatMetadata -> Double -> Integer -> (Double -> Double -> Double) -> Either Diagnostic RuntimeValue
evalFloat64IntegerArithmetic operatorSymbol floatMetadata floatValue integerValue combine = do
  integerFloat <- promotedIntegerFloat64Operand integerValue
  evalFloatBinary operatorSymbol (runtimeFloatTargetType floatMetadata) (combine floatValue integerFloat)

evalIntegerFloat64Predicate :: Text -> Integer -> Double -> (Double -> Double -> Bool) -> Either Diagnostic RuntimeValue
evalIntegerFloat64Predicate _ integerValue floatValue predicate = do
  integerFloat <- promotedIntegerFloat64Operand integerValue
  pure (VBool (predicate integerFloat floatValue))

evalFloat64IntegerPredicate :: Text -> Double -> Integer -> (Double -> Double -> Bool) -> Either Diagnostic RuntimeValue
evalFloat64IntegerPredicate _ floatValue integerValue predicate = do
  integerFloat <- promotedIntegerFloat64Operand integerValue
  pure (VBool (predicate floatValue integerFloat))

evalIntegerFloat64Equality :: Text -> Integer -> Double -> Either Diagnostic RuntimeValue
evalIntegerFloat64Equality operatorSymbol integerValue floatValue = do
  integerFloat <- promotedIntegerFloat64Operand integerValue
  pure (VBool (float64MixedEqualityResult operatorSymbol integerFloat floatValue))

evalFloat64IntegerEquality :: Text -> Double -> Integer -> Either Diagnostic RuntimeValue
evalFloat64IntegerEquality operatorSymbol floatValue integerValue = do
  integerFloat <- promotedIntegerFloat64Operand integerValue
  pure (VBool (float64MixedEqualityResult operatorSymbol floatValue integerFloat))

float64MixedEqualityResult :: Text -> Double -> Double -> Bool
float64MixedEqualityResult operatorSymbol leftValue rightValue =
  if operatorSymbol == "!="
    then leftValue /= rightValue
    else leftValue == rightValue

promotedIntegerFloat64Operand :: Integer -> Either Diagnostic Double
promotedIntegerFloat64Operand integerValue =
  case convertIntegerToFloatTarget BuiltinToFloat64 NumericFloat64 integerValue of
    Right (VFloat floatValue _) -> Right floatValue
    Right _ -> Left (numericConversionFloatOverflowDiagnostic BuiltinToFloat64 NumericFloat64)
    Left diagnostic -> Left diagnostic

mixedFloatArithmeticDiagnostic :: Text -> Maybe NumericType -> Maybe NumericType -> Diagnostic
mixedFloatArithmeticDiagnostic operatorSymbol leftTarget rightTarget =
  runtimeDiagnostic
    "E3007"
    ( "runtime primitive '"
        <> operatorSymbol
        <> "' cannot mix "
        <> renderFloatOperandTarget leftTarget
        <> " and "
        <> renderFloatOperandTarget rightTarget
    )

renderFloatOperandTarget :: Maybe NumericType -> Text
renderFloatOperandTarget maybeTarget =
  case maybeTarget of
    Just targetType -> renderNumericTypeName targetType
    Nothing -> "Float"

runtimeFloatArithmeticOverflowDiagnostic :: Text -> NumericType -> Diagnostic
runtimeFloatArithmeticOverflowDiagnostic operatorSymbol targetType =
  runtimeDiagnostic
    "E3025"
    ( "runtime primitive '"
        <> operatorSymbol
        <> "' failed: value cannot be represented as finite "
        <> renderNumericTypeName targetType
    )

evalStructuralEquality :: Text -> RuntimeValue -> RuntimeValue -> Either Diagnostic RuntimeValue
evalStructuralEquality operatorSymbol leftValue rightValue =
  if runtimeValueContainsFunction leftValue || runtimeValueContainsFunction rightValue
    then Left (runtimeCallableEqualityDiagnostic operatorSymbol leftValue rightValue)
    else
      case runtimeStructuralEquality leftValue rightValue of
        Just equalityResult ->
          Right
            ( VBool
                ( if operatorSymbol == "!="
                    then not equalityResult
                    else equalityResult
                )
            )
        Nothing ->
          Left
            ( runtimeDiagnostic
                "E3007"
                ( "runtime primitive '"
                    <> operatorSymbol
                    <> "' cannot be applied to "
                    <> renderRuntimeType leftValue
                    <> " and "
                    <> renderRuntimeType rightValue
                )
            )

runtimeValueContainsFunction :: RuntimeValue -> Bool
runtimeValueContainsFunction value =
  isFunctionValue value
    || runtimeContainerContainsFunction value
  where
    runtimeContainerContainsFunction runtimeValue =
      case runtimeValue of
        VList elements _ ->
          any runtimeValueContainsFunction elements
        VTuple elements ->
          any runtimeValueContainsFunction elements
        VConstructor _ _ _ _ capturedArgs ->
          any runtimeValueContainsFunction capturedArgs
        VTyped _ innerValue ->
          runtimeValueContainsFunction innerValue
        VExplicitTypeApplication _ innerValue ->
          runtimeValueContainsFunction innerValue
        VExplicitResultHint _ innerValue ->
          runtimeValueContainsFunction innerValue
        _ ->
          False

runtimeStructuralEquality :: RuntimeValue -> RuntimeValue -> Maybe Bool
runtimeStructuralEquality leftValue rightValue =
  case (leftValue, rightValue) of
    (VExplicitTypeApplication _ leftInnerValue, _) ->
      runtimeStructuralEquality leftInnerValue rightValue
    (_, VExplicitTypeApplication _ rightInnerValue) ->
      runtimeStructuralEquality leftValue rightInnerValue
    (VExplicitResultHint _ leftInnerValue, _) ->
      runtimeStructuralEquality leftInnerValue rightValue
    (_, VExplicitResultHint _ rightInnerValue) ->
      runtimeStructuralEquality leftValue rightInnerValue
    (VTyped leftTypeHint leftInnerValue, VTyped rightTypeHint rightInnerValue)
      | constraintSignatureTypesCompatible leftTypeHint rightTypeHint ->
          runtimeStructuralEquality leftInnerValue rightInnerValue
      | otherwise ->
          Just False
    (VTyped _ leftInnerValue, _) ->
      runtimeStructuralEquality leftInnerValue rightValue
    (_, VTyped _ rightInnerValue) ->
      runtimeStructuralEquality leftValue rightInnerValue
    (VInt leftInt leftMetadata, VInt rightInt rightMetadata) ->
      runtimeIntegerStructuralEquality leftInt leftMetadata rightInt rightMetadata
    (VFloat leftFloat leftMetadata, VFloat rightFloat rightMetadata) ->
      runtimeFloatStructuralEquality leftFloat leftMetadata rightFloat rightMetadata
    (VBool leftBool, VBool rightBool) -> Just (leftBool == rightBool)
    (VChar leftChar, VChar rightChar) -> Just (leftChar == rightChar)
    (VText leftText, VText rightText) -> Just (leftText == rightText)
    (VList leftElements _, VList rightElements _) ->
      structuralElementEquality leftElements rightElements
    (VTuple leftElements, VTuple rightElements) ->
      structuralElementEquality leftElements rightElements
    ( VConstructor leftTypeName _ leftName leftConstructorArguments leftArgs,
      VConstructor rightTypeName _ rightName rightConstructorArguments rightArgs
      )
      | constructorIsSaturated leftConstructorArguments leftArgs,
        constructorIsSaturated rightConstructorArguments rightArgs,
        leftTypeName == rightTypeName,
        leftName == rightName,
        leftConstructorArguments == rightConstructorArguments ->
          structuralElementEquality leftArgs rightArgs
      | constructorIsSaturated leftConstructorArguments leftArgs,
        constructorIsSaturated rightConstructorArguments rightArgs ->
          Just False
    _ -> Nothing

structuralElementEquality :: [RuntimeValue] -> [RuntimeValue] -> Maybe Bool
structuralElementEquality leftElements rightElements
  | length leftElements /= length rightElements =
      Just False
  | otherwise =
      fmap and
        (traverse (uncurry runtimeStructuralEquality) (zip leftElements rightElements))

evalIntegerPredicate :: Text -> Integer -> RuntimeIntMetadata -> Integer -> RuntimeIntMetadata -> Bool -> Either Diagnostic RuntimeValue
evalIntegerPredicate operatorSymbol leftInt leftMetadata rightInt rightMetadata predicateResult =
  case runtimeIntegerMetadataCompatible leftInt leftMetadata rightInt rightMetadata of
    True ->
      Right (VBool predicateResult)
    False ->
      Left
        ( runtimeDiagnostic
            "E3007"
            ( "runtime primitive '"
                <> operatorSymbol
                <> "' cannot compare "
                <> renderIntegerOperandTarget (runtimeIntTargetType leftMetadata)
                <> " and "
                <> renderIntegerOperandTarget (runtimeIntTargetType rightMetadata)
            )
        )

evalIntegerEquality :: Text -> Integer -> RuntimeIntMetadata -> Integer -> RuntimeIntMetadata -> Either Diagnostic RuntimeValue
evalIntegerEquality operatorSymbol leftInt leftMetadata rightInt rightMetadata =
  case runtimeIntegerMetadataCompatible leftInt leftMetadata rightInt rightMetadata of
    True ->
      Right
        ( VBool
            ( if operatorSymbol == "!="
                then leftInt /= rightInt
                else leftInt == rightInt
            )
        )
    False ->
      Left
        ( runtimeDiagnostic
            "E3007"
            ( "runtime primitive '"
                <> operatorSymbol
                <> "' cannot compare "
                <> renderIntegerOperandTarget (runtimeIntTargetType leftMetadata)
                <> " and "
                <> renderIntegerOperandTarget (runtimeIntTargetType rightMetadata)
            )
        )

evalFloatPredicate :: Text -> RuntimeFloatMetadata -> RuntimeFloatMetadata -> Bool -> Either Diagnostic RuntimeValue
evalFloatPredicate operatorSymbol leftMetadata rightMetadata predicateResult =
  if runtimeFloatMetadataCompatible leftMetadata rightMetadata
    then Right (VBool predicateResult)
    else
      Left
        ( runtimeDiagnostic
            "E3007"
            ( "runtime primitive '"
                <> operatorSymbol
                <> "' cannot compare "
                <> renderFloatOperandTarget (runtimeFloatTargetType leftMetadata)
                <> " and "
                <> renderFloatOperandTarget (runtimeFloatTargetType rightMetadata)
            )
        )

runtimeIntegerStructuralEquality :: Integer -> RuntimeIntMetadata -> Integer -> RuntimeIntMetadata -> Maybe Bool
runtimeIntegerStructuralEquality leftInt leftMetadata rightInt rightMetadata =
  if runtimeIntegerMetadataCompatible leftInt leftMetadata rightInt rightMetadata
    then Just (leftInt == rightInt)
    else Nothing

runtimeFloatStructuralEquality :: Double -> RuntimeFloatMetadata -> Double -> RuntimeFloatMetadata -> Maybe Bool
runtimeFloatStructuralEquality leftFloat leftMetadata rightFloat rightMetadata =
  if runtimeFloatMetadataCompatible leftMetadata rightMetadata
    then Just (leftFloat == rightFloat)
    else Nothing

runtimeIntegerMetadataCompatible :: Integer -> RuntimeIntMetadata -> Integer -> RuntimeIntMetadata -> Bool
runtimeIntegerMetadataCompatible leftInt leftMetadata rightInt rightMetadata =
  case (runtimeIntTargetType leftMetadata, runtimeIntTargetType rightMetadata) of
    (Just leftTarget, Just rightTarget) ->
      leftTarget == rightTarget
    (Just leftTarget, Nothing) ->
      integerValueMatchesTarget leftTarget rightInt
    (Nothing, Just rightTarget) ->
      integerValueMatchesTarget rightTarget leftInt
    (Nothing, Nothing) ->
      True

runtimeFloatMetadataCompatible :: RuntimeFloatMetadata -> RuntimeFloatMetadata -> Bool
runtimeFloatMetadataCompatible leftMetadata rightMetadata =
  case (runtimeFloatTargetType leftMetadata, runtimeFloatTargetType rightMetadata) of
    (Just leftTarget, Just rightTarget) ->
      leftTarget == rightTarget
    (Just NumericFloat64, Nothing) ->
      True
    (Nothing, Just NumericFloat64) ->
      True
    (Nothing, Nothing) ->
      True
    _ ->
      False

liftRuntimeResult :: Monad m => Either Diagnostic value -> ExceptT Diagnostic m value
liftRuntimeResult result =
  case result of
    Left diagnostic -> throwE diagnostic
    Right value -> pure value

evalValueWithHost ::
  Monad m =>
  RuntimeHost (RuntimeHostEvaluationT m) ->
  Maybe [Text] ->
  BuiltinResolutionMode ->
  Map BindingRuntimeHintKey SignatureType ->
  RuntimeEnv ->
  Expr ->
  ExceptT Diagnostic (RuntimeHostEvaluationT m) RuntimeValue
evalValueWithHost host currentModulePath builtinMode bindingTypeHints env expr =
  case expr of
    ELit literal -> pure (literalRuntimeValue literal)
    EVar name ->
      case Map.lookup name env of
        Just value ->
          liftRuntimeResult value
            >>= forceRuntimeValueWithHost host builtinMode bindingTypeHints
        Nothing ->
          case lookupBuiltinSymbolInMode builtinMode (identifierText name) of
            Just builtinFunction -> pure (VBuiltin builtinFunction [])
            Nothing ->
              throwE
                (runtimeDiagnostic "E3002" ("runtime unbound variable '" <> identifierText name <> "'"))
    ELambda parameterName bodyExpr ->
      pure (VClosure env parameterName bodyExpr Nothing currentModulePath)
    EOperatorValue operatorSymbol
      | isBuiltinOperatorSymbol operatorSymbol -> pure (VOperator operatorSymbol [])
      | otherwise ->
          lookupOperatorBindingRuntimeValueWithHost host builtinMode bindingTypeHints operatorSymbol env
    EList elements ->
      (`VList` Nothing) <$> traverse evaluateElement elements
    ETuple elements ->
      VTuple <$> traverse evaluateElement elements
    EApply functionExpr argumentExpr -> do
      functionValue <- evaluateElement functionExpr
      argumentValue <- evaluateElement argumentExpr
      applyRuntimeFunctionWithHost host builtinMode bindingTypeHints functionValue argumentValue
    ETypeApplication functionExpr typeArgumentSpan signatureType -> do
      let typeHint = runtimeConstraintType currentModulePath signatureType
      runtimeValue <- evaluateElement functionExpr
      case Map.lookup (explicitTypeApplicationRuntimeHintKeyInModule currentModulePath typeArgumentSpan) bindingTypeHints of
        Just concreteTypeHint ->
          liftRuntimeResult (applyRuntimeTypeHint (runtimeConstraintType currentModulePath concreteTypeHint) runtimeValue)
        Nothing ->
          if isFunctionValue runtimeValue
            then pure (VExplicitTypeApplication typeHint runtimeValue)
            else
              liftRuntimeResult
                (applyRuntimeTypeHint (fromMaybe typeHint (explicitTypeApplicationRuntimeValueHint typeHint runtimeValue)) runtimeValue)
    EIf conditionExpr thenExpr elseExpr -> do
      conditionValue <- evaluateElement conditionExpr
      case conditionValue of
        VBool True -> evaluateElement thenExpr
        VBool False -> evaluateElement elseExpr
        other ->
          throwE
            (runtimeDiagnostic "E3003" ("runtime branch condition must be Bool, found " <> renderRuntimeType other))
    EPatternCase scrutineeExpr caseArms -> do
      scrutineeValue <- evaluateElement scrutineeExpr
      evalPatternCaseWithHost host currentModulePath builtinMode bindingTypeHints env scrutineeValue caseArms
    EBinary operatorSymbol leftExpr rightExpr
      | isBuiltinOperatorSymbol operatorSymbol -> do
          leftValue <- evaluateElement leftExpr
          rightValue <- evaluateElement rightExpr
          evalBinaryWithHost host builtinMode bindingTypeHints operatorSymbol leftValue rightValue
      | otherwise -> do
          operatorValue <- lookupOperatorBindingRuntimeValueWithHost host builtinMode bindingTypeHints operatorSymbol env
          leftValue <- evaluateElement leftExpr
          partialValue <- applyRuntimeFunctionWithHost host builtinMode bindingTypeHints operatorValue leftValue
          rightValue <- evaluateElement rightExpr
          applyRuntimeFunctionWithHost host builtinMode bindingTypeHints partialValue rightValue
    ESectionLeft leftExpr operatorSymbol -> do
      leftValue <- evaluateElement leftExpr
      if isBuiltinOperatorSymbol operatorSymbol
        then pure (VSectionLeft operatorSymbol leftValue)
        else do
          operatorValue <- lookupOperatorBindingRuntimeValueWithHost host builtinMode bindingTypeHints operatorSymbol env
          applyRuntimeFunctionWithHost host builtinMode bindingTypeHints operatorValue leftValue
    ESectionRight operatorSymbol rightExpr -> do
      rightValue <- evaluateElement rightExpr
      if isBuiltinOperatorSymbol operatorSymbol
        then pure (VSectionRight operatorSymbol rightValue)
        else do
          operatorValue <- lookupOperatorBindingRuntimeValueWithHost host builtinMode bindingTypeHints operatorSymbol env
          pure (declaredOperatorRightSectionClosure currentModulePath operatorValue rightValue env)
    EBlock statements -> do
      scopeResult <-
        evalScopeWithHost
          host
          Set.empty
          currentModulePath
          EvaluateEntryModule
          builtinMode
          bindingTypeHints
          env
          statements
      case scopeResultValue scopeResult of
        Just value -> pure value
        Nothing -> throwE (runtimeDiagnostic "E3006" "block expression has no terminal expression result at runtime")
  where
    evaluateElement = evalValueWithHost host currentModulePath builtinMode bindingTypeHints env

evalScopeWithHost ::
  Monad m =>
  RuntimeHost (RuntimeHostEvaluationT m) ->
  Set Int ->
  Maybe [Text] ->
  ModuleEvaluationMode ->
  BuiltinResolutionMode ->
  Map BindingRuntimeHintKey SignatureType ->
  RuntimeEnv ->
  [Statement] ->
  ExceptT Diagnostic (RuntimeHostEvaluationT m) ScopeResult
evalScopeWithHost host preludeStatementIndices currentModulePath evaluationMode builtinMode bindingTypeHints initialEnv statements = do
  scopeId <- lift freshDeferredHostScopeId
  evalScopeWithHostInstance
    scopeId
    host
    preludeStatementIndices
    currentModulePath
    evaluationMode
    builtinMode
    bindingTypeHints
    initialEnv
    statements

evalScopeWithHostInstance ::
  Monad m =>
  DeferredHostScopeId ->
  RuntimeHost (RuntimeHostEvaluationT m) ->
  Set Int ->
  Maybe [Text] ->
  ModuleEvaluationMode ->
  BuiltinResolutionMode ->
  Map BindingRuntimeHintKey SignatureType ->
  RuntimeEnv ->
  [Statement] ->
  ExceptT Diagnostic (RuntimeHostEvaluationT m) ScopeResult
evalScopeWithHostInstance scopeId host preludeStatementIndices currentModulePath evaluationMode builtinMode bindingTypeHints initialEnv statements =
  go initialEnv Nothing indexedStatements
  where
    indexedStatements = zip [0 ..] statements
    statementsByIndex = Map.fromList indexedStatements
    recursiveGroups =
      inferRecursiveGroupsOrdered
        (Set.union (Map.keysSet initialEnv) (Set.map (sourceName . mkIdentifier) (builtinNamesInMode builtinMode)))
        indexedStatements
    bindingNamesByStatement = collectBindingNames indexedStatements
    hostRecursiveStatementIndices =
      Set.fromList
        [ groupIndex
          | groupMembers <- Map.elems recursiveGroups,
            any bindingRequiresHost groupMembers,
            groupIndex <- groupMembers
        ]
    modulePathsByStatement =
      snd (foldl' collectModulePath (currentModulePath, Map.empty) (zip [0 ..] statements))

    collectModulePath (activeModulePath, pathsByStatement) (statementIndex, statement) =
      let nextModulePath =
            case statement of
              SModule _ modulePath -> Just modulePath
              _ -> activeModulePath
       in (nextModulePath, Map.insert statementIndex nextModulePath pathsByStatement)

    modulePathForStatement statementIndex =
      if Set.member statementIndex preludeStatementIndices
        then Just []
        else Map.findWithDefault currentModulePath statementIndex modulePathsByStatement

    go env lastValue [] = pure (ScopeResult env lastValue)
    go env _ remaining@((statementIndex, statement) : rest)
      | not (statementNeedsDirectHostEvaluation statementIndex statement) = do
          let (pureChunk, remainingAfterChunk) =
                span
                  (\(index, chunkStatement) -> not (statementNeedsDirectHostEvaluation index chunkStatement))
                  remaining
              chunkPreludeStatementIndices =
                Set.fromList
                  [ localIndex
                    | (localIndex, (globalIndex, _)) <- zip [0 ..] pureChunk,
                      Set.member globalIndex preludeStatementIndices
                  ]
          scopeResult <-
            liftRuntimeResult
              ( evaluateModuleScopePureWithSourceUnitStatements
                  chunkPreludeStatementIndices
                  (modulePathForStatement statementIndex)
                  evaluationMode
                  builtinMode
                  bindingTypeHints
                  env
                  (map snd pureChunk)
              )
          go
            (scopeResultEnvironment scopeResult)
            (scopeResultValue scopeResult)
            remainingAfterChunk
      | otherwise =
          case statement of
            SLet name _ _ ->
              let bindingCell = hostBindingCell statementIndex env
               in case evaluationMode of
                    EvaluateDependencyModule ->
                      go
                        (Map.insert name bindingCell env)
                        Nothing
                        rest
                    EvaluateEntryModule -> do
                      value <- forceRuntimeCellWithHost bindingCell
                      go (Map.insert name (Right value) env) Nothing rest
            SImpl _ capabilityName arguments methods ->
              go
                (insertImplMethodsWithHost (modulePathForStatement statementIndex) capabilityName arguments methods env)
                Nothing
                rest
            SExpr _ valueExpr ->
              case evaluationMode of
                EvaluateDependencyModule -> go env Nothing rest
                EvaluateEntryModule -> do
                  value <-
                    evalValueWithHost
                      host
                      (modulePathForStatement statementIndex)
                      builtinMode
                      bindingTypeHints
                      env
                      valueExpr
                  go env (Just value) rest
            _ ->
              throwE
                (runtimeDiagnostic "E3020" "internal runtime error: unsupported direct host statement")

    statementNeedsDirectHostEvaluation statementIndex statement =
      case statement of
        SLet _ _ valueExpr ->
          runtimeExprRequiresHost valueExpr
            || Set.member statementIndex hostRecursiveStatementIndices
            || Map.notMember statementIndex recursiveGroups
        SImpl _ _ _ methods -> any implMethodRequiresHost methods
        SExpr {} -> True
        _ -> False

    bindingRequiresHost statementIndex =
      case Map.lookup statementIndex statementsByIndex of
        Just (SLet _ _ valueExpr) -> runtimeExprRequiresHost valueExpr
        _ -> False

    implMethodRequiresHost (ImplMethod _ _ methodExpr) = runtimeExprRequiresHost methodExpr

    hostBindingCell statementIndex baseEnv =
      case Map.lookup statementIndex recursiveGroups of
        Just groupMembers ->
          makeHostBindingCell statementIndex recursiveEnv
          where
            recursiveEnv = foldl' insertGroupMember baseEnv groupMembers

            insertGroupMember envAcc groupIndex =
              case Map.lookup groupIndex bindingNamesByStatement of
                Just groupName
                  | Map.notMember groupName baseEnv ->
                      Map.insert groupName (makeHostBindingCell groupIndex recursiveEnv) envAcc
                _ -> envAcc
        Nothing -> makeHostBindingCell statementIndex baseEnv

    makeHostBindingCell statementIndex capturedEnv =
      case Map.lookup statementIndex statementsByIndex of
        Just (SLet bindingName bindingSpan valueExpr) ->
          Right
            ( VDeferredHostBinding
                (DeferredHostBindingKey scopeId (modulePathForStatement statementIndex) bindingSpan bindingName)
                (modulePathForStatement statementIndex)
                valueExpr
                capturedEnv
                bindingTypeHints
                (previousSignatureNumericTarget statementIndex bindingName)
                (bindingRuntimeTypeHint statementIndex bindingName)
            )
        _ ->
          Left
            (runtimeDiagnostic "E3020" "internal runtime error: expected host binding statement")

    forceRuntimeCellWithHost bindingCell =
      liftRuntimeResult bindingCell
        >>= forceRuntimeValueWithHost host builtinMode bindingTypeHints

    insertImplMethodsWithHost methodModulePath capabilityName arguments methods env =
      case arguments of
        [implTarget]
          | concreteConstraintArgument implTarget -> methodEnv
          where
            runtimeImplTarget = runtimeConstraintType methodModulePath implTarget
            methodEnv = foldl' insertCandidate env methodCandidates
            methodCandidates =
              map
                ( \(ImplMethod methodName methodSpan methodExpr) ->
                    let methodKey = qualifiedMethodKey capabilityName methodName
                        qualifiedMethodName = qualifiedMemberName capabilityName methodName
                        evidence = RuntimeEvidence (identifierText capabilityName) runtimeImplTarget (Just methodKey)
                     in ( qualifiedMethodName,
                          RuntimeMethodCandidate
                            evidence
                            ( Right
                                ( VDeferredHostBinding
                                    (DeferredHostBindingKey scopeId methodModulePath methodSpan qualifiedMethodName)
                                    methodModulePath
                                    methodExpr
                                    methodEnv
                                    bindingTypeHints
                                    Nothing
                                    (methodRuntimeTypeHint runtimeImplTarget qualifiedMethodName)
                                )
                            )
                        )
                )
                methods

            insertCandidate envAcc (methodName, methodCandidate) =
              Map.adjust (addMethodCandidate methodCandidate) methodName envAcc

            addMethodCandidate methodCandidate methodCell =
              case methodCell of
                Right (VQualifiedMethod methodKey classParameter methodSignature candidates capturedArgs) ->
                  Right (VQualifiedMethod methodKey classParameter methodSignature (candidates <> [methodCandidate]) capturedArgs)
                _ -> methodCell

            methodRuntimeTypeHint implTarget methodName =
              case Map.lookup methodName methodEnv of
                Just (Right (VQualifiedMethod _ classParameter methodSignature _ _)) ->
                  runtimeConstraintType signatureModulePath
                    <$> substituteClassMethodSignature classParameter implTarget methodSignature
                _ -> Nothing
              where
                signatureModulePath =
                  case methodName of
                    ResolvedName (ImportedModule classModulePath) _ _ -> Just classModulePath
                    _ -> methodModulePath
        _ -> env

    previousSignatureNumericTarget statementIndex bindingName =
      case Map.lookup (statementIndex - 1) statementsByIndex of
        Just (SSignature signatureName _ signaturePayload)
          | identifierText signatureName == identifierText bindingName ->
              hostSignatureNumericTarget signaturePayload
        _ -> Nothing

    previousSignatureRuntimeTypeHint statementIndex bindingName =
      case Map.lookup (statementIndex - 1) statementsByIndex of
        Just (SSignature signatureName _ signaturePayload)
          | identifierText signatureName == identifierText bindingName ->
              signaturePayloadConstraintType signaturePayload
        _ -> Nothing

    bindingRuntimeTypeHint statementIndex bindingName =
      runtimeConstraintType (modulePathForStatement statementIndex) <$> rawHint
      where
        rawHint =
          case previousSignatureRuntimeTypeHint statementIndex bindingName of
            Just signatureHint -> Just signatureHint
            Nothing ->
              case Map.lookup statementIndex statementsByIndex of
                Just (SLet _ bindingSpan _) ->
                  Map.lookup
                    (bindingRuntimeHintKeyInModule (modulePathForStatement statementIndex) bindingName bindingSpan)
                    bindingTypeHints
                _ -> Nothing

hostSignatureNumericTarget :: SignaturePayload -> Maybe NumericType
hostSignatureNumericTarget signaturePayload =
  case signaturePayload of
    SignatureType TypeInt -> Just NumericInt64
    SignatureType TypeFloat -> Just NumericFloat64
    SignatureType (TypeNumeric targetType) -> Just targetType
    ConstrainedSignature _ signatureType -> hostConstraintSignatureNumericTarget signatureType
    _ -> Nothing

hostConstraintSignatureNumericTarget :: SignatureType -> Maybe NumericType
hostConstraintSignatureNumericTarget signatureType =
  case signatureType of
    TypeInt -> Just NumericInt64
    TypeFloat -> Just NumericFloat64
    TypeNumeric numericType -> Just numericType
    TypeName typeName ->
      case identifierText typeName of
        "Int" -> Just NumericInt64
        "Float" -> Just NumericFloat64
        typeNameText -> numericTypeFromName typeNameText
    _ -> Nothing

evalHostBindingValue ::
  Monad m =>
  RuntimeHost (RuntimeHostEvaluationT m) ->
  Maybe [Text] ->
  BuiltinResolutionMode ->
  Map BindingRuntimeHintKey SignatureType ->
  RuntimeEnv ->
  Expr ->
  Maybe NumericType ->
  Maybe SignatureType ->
  ExceptT Diagnostic (RuntimeHostEvaluationT m) RuntimeValue
evalHostBindingValue host currentModulePath builtinMode bindingTypeHints env valueExpr maybeNumericTarget maybeTypeHint = do
  value <-
    case maybeNumericTarget of
      Just targetType ->
        evalHostNumericSignatureBinding targetType
      Nothing ->
        evalValueWithHost host currentModulePath builtinMode bindingTypeHints env valueExpr
  liftRuntimeResult
    ( attachRuntimeTypeHint maybeTypeHint value
        >>= attachDefaultBindingIntegerTarget
    )
  where
    evalHostNumericSignatureBinding targetType =
      case valueExpr of
        ELit (LInt literalValue) ->
          liftRuntimeResult
            (convertIntegerToNumericTarget conversionBuiltin targetType literalValue)
        ELit (LFloat literalValue literalSource _) ->
          liftRuntimeResult
            (convertFloatToNumericTarget conversionBuiltin targetType literalValue (Just literalSource))
        _ -> do
          runtimeValue <-
            evalValueWithHost host currentModulePath builtinMode bindingTypeHints env valueExpr
          liftRuntimeResult (evalNumericConversion conversionBuiltin targetType runtimeValue)
      where
        conversionBuiltin = numericConversionBuiltinForTarget targetType

forceQualifiedMethodValueWithHost ::
  Monad m =>
  RuntimeHost (RuntimeHostEvaluationT m) ->
  BuiltinResolutionMode ->
  Map BindingRuntimeHintKey SignatureType ->
  RuntimeValue ->
  ExceptT Diagnostic (RuntimeHostEvaluationT m) RuntimeValue
forceQualifiedMethodValueWithHost host builtinMode bindingTypeHints runtimeValue =
  case runtimeValue of
    VQualifiedMethod methodKey classParameter methodSignature candidates capturedArgs ->
      applyQualifiedMethodWithHost host builtinMode bindingTypeHints methodKey classParameter methodSignature candidates capturedArgs
    _ -> pure runtimeValue

forceRuntimeValueWithHost ::
  Monad m =>
  RuntimeHost (RuntimeHostEvaluationT m) ->
  BuiltinResolutionMode ->
  Map BindingRuntimeHintKey SignatureType ->
  RuntimeValue ->
  ExceptT Diagnostic (RuntimeHostEvaluationT m) RuntimeValue
forceRuntimeValueWithHost host builtinMode bindingTypeHints runtimeValue =
  case runtimeValue of
    VDeferredHostBinding bindingKey currentModulePath valueExpr env capturedBindingTypeHints maybeNumericTarget maybeTypeHint -> do
      cache <- runtimeHostEvaluationBindingCache <$> lift get
      case Map.lookup bindingKey cache of
        Just (DeferredHostBindingEvaluated result) ->
          liftRuntimeResult result
        Just DeferredHostBindingEvaluating ->
          throwE
            (runtimeDiagnostic "E3021" "runtime recursive host binding has no concrete value")
        Nothing -> do
          lift
            ( modifyDeferredHostBindingCache
                (Map.insert bindingKey DeferredHostBindingEvaluating)
            )
          result <-
            lift
              ( runExceptT
                  ( evalHostBindingValue
                      host
                      currentModulePath
                      builtinMode
                      capturedBindingTypeHints
                      env
                      valueExpr
                      maybeNumericTarget
                      maybeTypeHint
                  )
              )
          lift
            ( modifyDeferredHostBindingCache
                (Map.insert bindingKey (DeferredHostBindingEvaluated result))
            )
          liftRuntimeResult result
    VTyped typeHint innerValue ->
      VTyped typeHint <$> forceRuntimeValueWithHost host builtinMode bindingTypeHints innerValue
    VExplicitTypeApplication typeHint innerValue ->
      VExplicitTypeApplication typeHint <$> forceRuntimeValueWithHost host builtinMode bindingTypeHints innerValue
    VExplicitResultHint typeHint innerValue ->
      VExplicitResultHint typeHint <$> forceRuntimeValueWithHost host builtinMode bindingTypeHints innerValue
    _ ->
      forceQualifiedMethodValueWithHost host builtinMode bindingTypeHints runtimeValue

lookupOperatorBindingRuntimeValueWithHost ::
  Monad m =>
  RuntimeHost (RuntimeHostEvaluationT m) ->
  BuiltinResolutionMode ->
  Map BindingRuntimeHintKey SignatureType ->
  Text ->
  RuntimeEnv ->
  ExceptT Diagnostic (RuntimeHostEvaluationT m) RuntimeValue
lookupOperatorBindingRuntimeValueWithHost host builtinMode bindingTypeHints operatorSymbol env =
  case Map.lookup (operatorBindingName operatorSymbol) env of
    Just value ->
      liftRuntimeResult value
        >>= forceRuntimeValueWithHost host builtinMode bindingTypeHints
    Nothing ->
      throwE
        (runtimeDiagnostic "E3002" ("runtime unbound operator binding '(" <> operatorSymbol <> ")'"))

evalPatternCaseWithHost ::
  Monad m =>
  RuntimeHost (RuntimeHostEvaluationT m) ->
  Maybe [Text] ->
  BuiltinResolutionMode ->
  Map BindingRuntimeHintKey SignatureType ->
  RuntimeEnv ->
  RuntimeValue ->
  [CaseArm] ->
  ExceptT Diagnostic (RuntimeHostEvaluationT m) RuntimeValue
evalPatternCaseWithHost host currentModulePath builtinMode bindingTypeHints env scrutineeValue caseArms = do
  selectedArm <- selectMatchingCaseArmWithHost host currentModulePath builtinMode bindingTypeHints env scrutineeValue caseArms
  case selectedArm of
    Just (armEnv, bodyExpr) ->
      evalValueWithHost host currentModulePath builtinMode bindingTypeHints armEnv bodyExpr
    Nothing -> throwE (runtimeDiagnostic "E3022" "pattern case matched no arms")

selectMatchingCaseArmWithHost ::
  Monad m =>
  RuntimeHost (RuntimeHostEvaluationT m) ->
  Maybe [Text] ->
  BuiltinResolutionMode ->
  Map BindingRuntimeHintKey SignatureType ->
  RuntimeEnv ->
  RuntimeValue ->
  [CaseArm] ->
  ExceptT Diagnostic (RuntimeHostEvaluationT m) (Maybe (RuntimeEnv, Expr))
selectMatchingCaseArmWithHost host currentModulePath builtinMode bindingTypeHints env scrutineeValue = go
  where
    go remainingArms =
      case remainingArms of
        [] -> pure Nothing
        caseArm : rest ->
          case matchCaseArm currentModulePath env scrutineeValue caseArm of
            Nothing -> go rest
            Just (armEnv, Nothing, bodyExpr) -> pure (Just (armEnv, bodyExpr))
            Just (armEnv, Just conditionExpr, bodyExpr) -> do
              guardValue <- evalValueWithHost host currentModulePath builtinMode bindingTypeHints armEnv conditionExpr
              case guardValue of
                VBool True -> pure (Just (armEnv, bodyExpr))
                VBool False -> go rest
                other ->
                  throwE
                    (runtimeDiagnostic "E3003" ("runtime case guard must be Bool, found " <> renderRuntimeType other))

applyRuntimeFunctionWithHost ::
  Monad m =>
  RuntimeHost (RuntimeHostEvaluationT m) ->
  BuiltinResolutionMode ->
  Map BindingRuntimeHintKey SignatureType ->
  RuntimeValue ->
  RuntimeValue ->
  ExceptT Diagnostic (RuntimeHostEvaluationT m) RuntimeValue
applyRuntimeFunctionWithHost host builtinMode bindingTypeHints functionValue argumentValue =
  case functionValue of
    VDeferredHostBinding {} -> do
      forcedFunctionValue <-
        forceRuntimeValueWithHost host builtinMode bindingTypeHints functionValue
      applyRuntimeFunctionWithHost
        host
        builtinMode
        bindingTypeHints
        forcedFunctionValue
        argumentValue
    VExplicitTypeApplication typeHint innerFunctionValue ->
      case explicitTypeApplicationRuntimeFunctionHint typeHint innerFunctionValue of
        Just instantiatedFunctionHint ->
          applyRuntimeFunctionWithHost host builtinMode bindingTypeHints (VTyped instantiatedFunctionHint innerFunctionValue) argumentValue
        Nothing -> do
          resultValue <- applyRuntimeFunctionWithHost host builtinMode bindingTypeHints innerFunctionValue argumentValue
          liftRuntimeResult (applyExplicitTypeApplicationResultHint typeHint resultValue)
    VExplicitResultHint typeHint innerFunctionValue -> do
      resultValue <- applyRuntimeFunctionWithHost host builtinMode bindingTypeHints innerFunctionValue argumentValue
      liftRuntimeResult (applyExplicitTypeApplicationResultHint typeHint resultValue)
    VTyped typeHint innerFunctionValue -> do
      hintedArgumentValue <- liftRuntimeResult (applyRuntimeFunctionArgumentHint typeHint argumentValue)
      resultValue <- applyRuntimeFunctionWithHost host builtinMode bindingTypeHints innerFunctionValue hintedArgumentValue
      liftRuntimeResult (applyRuntimeFunctionResultHint typeHint resultValue)
    VSectionLeft operatorSymbol leftValue ->
      evalBinaryWithHost host builtinMode bindingTypeHints operatorSymbol leftValue argumentValue
    VSectionRight operatorSymbol rightValue ->
      evalBinaryWithHost host builtinMode bindingTypeHints operatorSymbol argumentValue rightValue
    VClosure capturedEnv parameterName bodyExpr maybeTypeHint closureModulePath -> do
      hintedArgumentValue <-
        case maybeTypeHint of
          Just typeHint -> liftRuntimeResult (applyRuntimeFunctionArgumentHint typeHint argumentValue)
          Nothing -> pure argumentValue
      resultValue <-
        evalValueWithHost
          host
          closureModulePath
          builtinMode
          bindingTypeHints
          (Map.insert parameterName (Right hintedArgumentValue) capturedEnv)
          bodyExpr
      case maybeTypeHint of
        Just typeHint -> liftRuntimeResult (applyRuntimeFunctionResultHint typeHint resultValue)
        Nothing -> liftRuntimeResult (attachDefaultBindingIntegerTarget resultValue)
    VBuiltin builtinFunction capturedArgs ->
      applyBuiltinWithHost host builtinMode bindingTypeHints builtinFunction (capturedArgs <> [argumentValue])
    VOperator operatorSymbol capturedArgs ->
      applyOperatorWithHost host builtinMode bindingTypeHints operatorSymbol (capturedArgs <> [argumentValue])
    VConstructor typeName typeParameters constructorName constructorArguments capturedArgs ->
      liftRuntimeResult
        (applyConstructor typeName typeParameters constructorName constructorArguments (capturedArgs <> [argumentValue]))
    VQualifiedMethod methodKey classParameter methodSignature candidates capturedArgs ->
      applyQualifiedMethodWithHost host builtinMode bindingTypeHints methodKey classParameter methodSignature candidates (capturedArgs <> [argumentValue])
    _ ->
      throwE
        (runtimeDiagnostic "E3008" ("runtime cannot apply non-function value of type " <> renderRuntimeType functionValue))

applyQualifiedMethodWithHost ::
  Monad m =>
  RuntimeHost (RuntimeHostEvaluationT m) ->
  BuiltinResolutionMode ->
  Map BindingRuntimeHintKey SignatureType ->
  Text ->
  Text ->
  SignaturePayload ->
  [RuntimeMethodCandidate] ->
  [RuntimeValue] ->
  ExceptT Diagnostic (RuntimeHostEvaluationT m) RuntimeValue
applyQualifiedMethodWithHost host builtinMode bindingTypeHints methodKey classParameter methodSignature candidates arguments =
  case preferredCandidates of
    [] -> throwE (runtimeDiagnostic "E3026" ("no matching qualified method body '" <> methodKey <> "'"))
    [RuntimeMethodCandidate _ methodCell] -> do
      methodValue <-
        liftRuntimeResult methodCell
          >>= forceRuntimeValueWithHost host builtinMode bindingTypeHints
      foldM (applyRuntimeFunctionWithHost host builtinMode bindingTypeHints) methodValue arguments
    _
      | runtimeQualifiedMethodIsFullyApplied classParameter methodSignature arguments preferredCandidates ->
          throwE (runtimeDiagnostic "E3026" ("ambiguous qualified method body '" <> methodKey <> "'"))
      | otherwise ->
          pure (VQualifiedMethod methodKey classParameter methodSignature preferredCandidates arguments)
  where
    preferredCandidates =
      case exactMatchingCandidates of
        [] -> matchingCandidates
        exactMatches -> exactMatches
    exactMatchingCandidates =
      filter (runtimeMethodCandidateExactlyMatches classParameter methodSignature arguments) matchingCandidates
    matchingCandidates =
      filter (runtimeMethodCandidateMatches classParameter methodSignature arguments) candidates

applyOperatorWithHost ::
  Monad m =>
  RuntimeHost (RuntimeHostEvaluationT m) ->
  BuiltinResolutionMode ->
  Map BindingRuntimeHintKey SignatureType ->
  Text ->
  [RuntimeValue] ->
  ExceptT Diagnostic (RuntimeHostEvaluationT m) RuntimeValue
applyOperatorWithHost host builtinMode bindingTypeHints operatorSymbol arguments =
  case arguments of
    [leftValue] -> pure (VOperator operatorSymbol [leftValue])
    [leftValue, rightValue] ->
      evalBinaryWithHost host builtinMode bindingTypeHints operatorSymbol leftValue rightValue
    _ ->
      throwE
        (runtimeDiagnostic "E3016" ("runtime primitive '" <> operatorSymbol <> "' received invalid arguments"))

applyBuiltinWithHost ::
  Monad m =>
  RuntimeHost (RuntimeHostEvaluationT m) ->
  BuiltinResolutionMode ->
  Map BindingRuntimeHintKey SignatureType ->
  BuiltinSymbol ->
  [RuntimeValue] ->
  ExceptT Diagnostic (RuntimeHostEvaluationT m) RuntimeValue
applyBuiltinWithHost host builtinMode bindingTypeHints builtinFunction arguments
  | length arguments < builtinSymbolArity builtinFunction =
      pure (VBuiltin builtinFunction arguments)
  | length arguments == builtinSymbolArity builtinFunction =
      evalBuiltinWithHost host builtinMode bindingTypeHints builtinFunction arguments
  | otherwise =
      throwE
        (runtimeDiagnostic "E3014" ("runtime primitive '" <> builtinSymbolName builtinFunction <> "' received too many arguments"))

evalBuiltinWithHost ::
  Monad m =>
  RuntimeHost (RuntimeHostEvaluationT m) ->
  BuiltinResolutionMode ->
  Map BindingRuntimeHintKey SignatureType ->
  BuiltinSymbol ->
  [RuntimeValue] ->
  ExceptT Diagnostic (RuntimeHostEvaluationT m) RuntimeValue
evalBuiltinWithHost host builtinMode bindingTypeHints builtinFunction arguments =
  case (builtinFunction, arguments) of
    (BuiltinReadTextRaw, [VText path]) ->
      rawHostOutcome VText <$> lift (runtimeHostReadText host path)
    (BuiltinWriteTextRaw, [VText path, VText contents]) ->
      rawHostOutcome (const (VText "")) <$> lift (runtimeHostWriteText host path contents)
    (BuiltinReadStdinRaw, [VTuple []]) ->
      rawHostOutcome VText <$> lift (runtimeHostReadStdin host)
    (BuiltinWriteStdoutRaw, [VText contents]) ->
      rawHostOutcome (const (VText "")) <$> lift (runtimeHostWriteStdout host contents)
    (BuiltinWriteStderrRaw, [VText contents]) ->
      rawHostOutcome (const (VText "")) <$> lift (runtimeHostWriteStderr host contents)
    (BuiltinArguments, [VTuple []]) -> do
      argumentsText <- lift (runtimeHostArguments host)
      pure (VList (map VText argumentsText) (Just (TypeList TypeText)))
    (BuiltinExit, [statusValue])
      | Just status <- runtimeHostExitStatus statusValue,
        status >= 0 && status <= 255 -> do
          exitResult <- lift (runtimeHostExit host status)
          case exitResult of
            Right () -> pure (VTuple [])
            Left failure ->
              throwE
                ( runtimeDiagnostic
                    "E3031"
                    ( "runtime host operation 'exit!' failed: "
                        <> hostIOFailureMessage (hostIOFailureCategory failure)
                    )
                )
      | Just status <- runtimeHostExitStatus statusValue ->
          throwE
            ( runtimeDiagnostic
                "E3030"
                ("runtime primitive 'exit!' expects a status in range 0..255, found " <> Text.pack (show status))
            )
    (BuiltinMap, [mapper, VList elements maybeCollectionTypeHint])
      | isFunctionValue mapper -> do
          mappedElements <- traverse (applyRuntimeFunctionWithHost host builtinMode bindingTypeHints mapper) elements
          let maybeMappedTypeHint = TypeList <$> runtimeMapResultElementType mapper maybeCollectionTypeHint
          pure (VList mappedElements maybeMappedTypeHint)
    (BuiltinFilter, [predicate, VList elements maybeTypeHint])
      | isFunctionValue predicate ->
          (`VList` maybeTypeHint) <$> filterElementsWithHost host builtinMode bindingTypeHints predicate elements
    _ -> liftRuntimeResult (evalBuiltin builtinMode bindingTypeHints builtinFunction arguments)

rawHostOutcome :: (success -> RuntimeValue) -> Either HostIOFailure success -> RuntimeValue
rawHostOutcome renderSuccess outcome =
  case outcome of
    Right value ->
      VTuple [VBool True, renderSuccess value, VText "", VText ""]
    Left failure ->
      let category = hostIOFailureCategory failure
       in VTuple
            [ VBool False,
              VText "",
              VText (hostIOCategoryToken category),
              VText (hostIOFailureMessage category)
            ]

runtimeHostExitStatus :: RuntimeValue -> Maybe Integer
runtimeHostExitStatus runtimeValue =
  case runtimeValue of
    VInt status _ -> Just status
    VTyped _ innerValue -> runtimeHostExitStatus innerValue
    VExplicitTypeApplication _ innerValue -> runtimeHostExitStatus innerValue
    VExplicitResultHint _ innerValue -> runtimeHostExitStatus innerValue
    _ -> Nothing

filterElementsWithHost ::
  Monad m =>
  RuntimeHost (RuntimeHostEvaluationT m) ->
  BuiltinResolutionMode ->
  Map BindingRuntimeHintKey SignatureType ->
  RuntimeValue ->
  [RuntimeValue] ->
  ExceptT Diagnostic (RuntimeHostEvaluationT m) [RuntimeValue]
filterElementsWithHost host builtinMode bindingTypeHints predicate values = do
  results <- traverse applyPredicate values
  pure [value | (value, True) <- results]
  where
    applyPredicate value = do
      predicateResult <- applyRuntimeFunctionWithHost host builtinMode bindingTypeHints predicate value
      case predicateResult of
        VBool shouldKeep -> pure (value, shouldKeep)
        other ->
          throwE
            (runtimeDiagnostic "E3019" ("runtime primitive 'filter' predicate must return Bool, found " <> renderRuntimeType other))

evalBinaryWithHost ::
  Monad m =>
  RuntimeHost (RuntimeHostEvaluationT m) ->
  BuiltinResolutionMode ->
  Map BindingRuntimeHintKey SignatureType ->
  Text ->
  RuntimeValue ->
  RuntimeValue ->
  ExceptT Diagnostic (RuntimeHostEvaluationT m) RuntimeValue
evalBinaryWithHost host builtinMode bindingTypeHints operatorSymbol leftValue rightValue =
  case (operatorSymbol, leftValue, rightValue) of
    ("$", functionValue, argumentValue) ->
      applyRuntimeFunctionWithHost host builtinMode bindingTypeHints functionValue argumentValue
    (_, VTyped leftTypeHint leftInnerValue, _)
      | not (isStrictEqualityOperator operatorSymbol) -> do
          result <- evalBinaryWithHost host builtinMode bindingTypeHints operatorSymbol leftInnerValue rightValue
          liftRuntimeResult (preserveLeftTypedNumericOperatorResult operatorSymbol leftTypeHint result)
    (_, _, VTyped rightTypeHint rightInnerValue)
      | not (isStrictEqualityOperator operatorSymbol) -> do
          result <- evalBinaryWithHost host builtinMode bindingTypeHints operatorSymbol leftValue rightInnerValue
          liftRuntimeResult (preserveRightTypedNumericOperatorResult operatorSymbol leftValue rightTypeHint result)
    _ -> liftRuntimeResult (evalBinary builtinMode bindingTypeHints operatorSymbol leftValue rightValue)

-- | Runtime-specific wrapper for mkDiagnostic.
-- This alias exists solely to improve readability and make it clear that
-- diagnostics are being created in a runtime evaluation context rather than
-- during parsing or type checking.
runtimeDiagnostic :: Text -> Text -> Diagnostic
runtimeDiagnostic = mkDiagnostic

-- | Render coarse runtime type names for diagnostics.
renderRuntimeType :: RuntimeValue -> Text
renderRuntimeType value =
  case value of
    VInt _ metadata ->
      case runtimeIntTargetType metadata of
        Just targetType -> renderNumericTypeName targetType
        Nothing -> "Int"
    VFloat {} -> "Float"
    VBool {} -> "Bool"
    VChar {} -> "Char"
    VText {} -> "Text"
    VList {} -> "List"
    VTuple {} -> "Tuple"
    VSectionLeft {} -> "Function"
    VSectionRight {} -> "Function"
    VClosure {} -> "Function"
    VBuiltin {} -> "Function"
    VOperator {} -> "Function"
    VConstructor _ _ _ constructorArguments capturedArgs
      | constructorIsSaturated constructorArguments capturedArgs -> "Data"
      | otherwise -> "Function"
    VQualifiedMethod {} -> "Function"
    VTyped _ innerValue -> renderRuntimeType innerValue
    VExplicitTypeApplication _ innerValue -> renderRuntimeType innerValue
    VExplicitResultHint _ innerValue -> renderRuntimeType innerValue
    VDeferredHostBinding {} -> "Deferred"

constructorIsSaturated :: [DataConstructorArgument] -> [RuntimeValue] -> Bool
constructorIsSaturated constructorArguments capturedArgs =
  length capturedArgs >= length constructorArguments
