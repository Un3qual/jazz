{-# LANGUAGE OverloadedStrings #-}

-- | Small interpreter/runtime for the currently-supported core language. It is
-- intentionally simple and mirrors the same builtin/operator contracts enforced
-- by analysis and type inference.
module JazzNext.Compiler.Runtime
  ( RuntimeValue (..),
    evaluateRuntimeExprWithBuiltins,
    evaluateRuntimeExpr,
    renderRuntimeValue
  ) where

import Control.Monad (foldM)
import Data.List (foldl')
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as Text
import JazzNext.Compiler.AST
  ( CaseArm (..),
    ClassMethodSignature (..),
    ConstraintSignatureType (..),
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
    numericTypeFloatMax,
    numericTypeIntegerBounds
  )
import JazzNext.Compiler.CapabilityFacts
  ( concreteConstraintArgument,
    constraintFunctionArgumentTypes,
    qualifiedMethodKey,
    substituteClassMethodSignature
  )
import JazzNext.Compiler.Identifier
  ( Identifier,
    identifierText
  )
import JazzNext.Compiler.RecursiveBindings
  ( collectBindingNames,
    freeVarsExprWithBound,
    freeVarsScopeWithBound,
    inferRecursiveGroupsOrdered,
    inferSelfRecursiveBindings
  )

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

data RuntimeMethodCandidate = RuntimeMethodCandidate ConstraintSignatureType (Either Diagnostic RuntimeValue)

data RuntimeValue
  = VInt Integer RuntimeIntMetadata
  | VFloat Double RuntimeFloatMetadata
  | VBool Bool
  | VList [RuntimeValue]
  | VTuple [RuntimeValue]
  | VClosure RuntimeEnv Identifier Expr
  | VBuiltin BuiltinSymbol [RuntimeValue]
  | VOperator Text [RuntimeValue]
  | VSectionLeft Text RuntimeValue
  | VSectionRight Text RuntimeValue
  | VConstructor Identifier [Identifier] Identifier [DataConstructorArgument] [RuntimeValue]
  | VQualifiedMethod Text Text SignaturePayload [RuntimeMethodCandidate] [RuntimeValue]

instance Eq RuntimeValue where
  leftValue == rightValue =
    case (leftValue, rightValue) of
      (VInt leftInt _, VInt rightInt _) -> leftInt == rightInt
      (VFloat leftFloat _, VFloat rightFloat _) -> leftFloat == rightFloat
      (VBool leftBool, VBool rightBool) -> leftBool == rightBool
      (VList leftElements, VList rightElements) -> leftElements == rightElements
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
  RuntimeMethodCandidate leftTarget leftCell == RuntimeMethodCandidate rightTarget rightCell =
    leftTarget == rightTarget && leftCell == rightCell

instance Show RuntimeValue where
  show value =
    case value of
      VInt intValue _ -> "VInt " <> show intValue
      VFloat floatValue _ -> "VFloat " <> show floatValue
      VBool boolValue -> "VBool " <> show boolValue
      VList elements -> "VList " <> show elements
      VTuple elements -> "VTuple " <> show elements
      VClosure _ parameterName bodyExpr ->
        "VClosure <env> " <> show parameterName <> " " <> show bodyExpr
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

instance Show RuntimeMethodCandidate where
  show (RuntimeMethodCandidate implTarget _) =
    "RuntimeMethodCandidate " <> show implTarget

evaluateRuntimeExpr :: Expr -> Either Diagnostic (Maybe RuntimeValue)
evaluateRuntimeExpr = evaluateRuntimeExprWithBuiltins ResolveKernelOnly

-- | Evaluate an expression under the builtin resolution mode chosen by the
-- caller, returning a terminal scope value when one exists.
evaluateRuntimeExprWithBuiltins :: BuiltinResolutionMode -> Expr -> Either Diagnostic (Maybe RuntimeValue)
evaluateRuntimeExprWithBuiltins builtinMode expr =
  case expr of
    EBlock statements -> evalScope builtinMode Map.empty statements
    _ -> Just <$> evalValue builtinMode Map.empty expr

renderRuntimeValue :: RuntimeValue -> Text
renderRuntimeValue value =
  case value of
    VInt intValue _ -> Text.pack (show intValue)
    VFloat floatValue _ -> Text.pack (show floatValue)
    VBool boolValue ->
      if boolValue
        then "True"
        else "False"
    VList elements ->
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

renderConstructorValue :: Identifier -> [RuntimeValue] -> Text
renderConstructorValue constructorName arguments =
  case arguments of
    [] -> renderConstructorName constructorName
    _ ->
      renderConstructorName constructorName
        <> "("
        <> Text.intercalate ", " (map renderRuntimeValue arguments)
        <> ")"

renderConstructorName :: Identifier -> Text
renderConstructorName constructorName =
  let nameText = identifierText constructorName
      segments = Text.splitOn "::" nameText
   in case segments of
        "__module" : _ -> last segments
        _ -> nameText

-- | Runtime cells can hold either a value or the deterministic failure for a
-- recursive binding that cannot be forced safely.
type RuntimeCell = Either Diagnostic RuntimeValue

type RuntimeEnv = Map Text RuntimeCell

-- | Evaluate a block scope in order. Declarations clear `lastExprValue`, so
-- `evalScope` returns `Just` only when the final surviving statement is an
-- `SExpr`; otherwise the block yields `Nothing`.
evalScope :: BuiltinResolutionMode -> RuntimeEnv -> [Statement] -> Either Diagnostic (Maybe RuntimeValue)
evalScope builtinMode initialEnv statements = go initialEnv Nothing indexedStatements
  where
    indexedStatements = zip [0 ..] statements
    statementsByIndex = Map.fromList indexedStatements
    recursiveGroups =
      inferRecursiveGroupsOrdered
        (Set.union (Map.keysSet initialEnv) (builtinNamesInMode builtinMode))
        indexedStatements
    selfRecursiveFunctionStatements =
      inferSelfRecursiveBindings exprContainsFunctionBranch indexedStatements
    bindingNamesByStatement = collectBindingNames indexedStatements
    bindingCells = map (uncurry cellForStatement) indexedStatements

    go :: RuntimeEnv -> Maybe RuntimeValue -> [(Int, Statement)] -> Either Diagnostic (Maybe RuntimeValue)
    go env lastExprValue remainingStatements =
      case remainingStatements of
        [] ->
          -- Declaration-only scopes intentionally remain `Nothing` until a terminal `SExpr` sets a value.
          Right lastExprValue
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
              go (insertImplMethods capabilityName arguments methods env) Nothing rest
            SData _ typeName typeParameters constructors ->
              go (insertDataConstructors typeName typeParameters constructors env) Nothing rest
            SLet name _ _ -> do
              value <- bindingCellAt statementIndex
              go (Map.insert (identifierText name) (Right value) env) Nothing rest
            SExpr _ expr -> do
              value <- evalValue builtinMode env expr
              go env (Just value) rest

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

    bindingCell :: Int -> Identifier -> Expr -> RuntimeCell
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

    evalBindingValue :: Int -> Identifier -> RuntimeEnv -> Expr -> Either Diagnostic RuntimeValue
    evalBindingValue statementIndex bindingName env valueExpr =
      case targetedIntegerLiteralBinding statementIndex bindingName valueExpr of
        Just (targetType, literalValue) ->
          convertIntegerToNumericTarget
            (integerConversionBuiltinForTarget targetType)
            targetType
            literalValue
        Nothing ->
          case targetedFractionalLiteralBinding statementIndex bindingName valueExpr of
            Just (targetType, literalValue, literalSource) ->
              convertFiniteFloatToFloatTarget
                (floatConversionBuiltinForTarget targetType)
                targetType
                literalValue
                (Just literalSource)
            Nothing ->
              evalValue builtinMode env valueExpr

    targetedIntegerLiteralBinding :: Int -> Identifier -> Expr -> Maybe (NumericType, Integer)
    targetedIntegerLiteralBinding statementIndex bindingName valueExpr =
      case valueExpr of
        ELit (LInt literalValue) ->
          case previousSignatureIntegerTarget statementIndex bindingName of
            Just targetType -> Just (targetType, literalValue)
            Nothing -> Nothing
        _ -> Nothing

    previousSignatureIntegerTarget :: Int -> Identifier -> Maybe NumericType
    previousSignatureIntegerTarget statementIndex bindingName =
      case Map.lookup (statementIndex - 1) statementsByIndex of
        Just (SSignature signatureName _ signaturePayload)
          | identifierText signatureName == identifierText bindingName ->
              signatureIntegerTarget signaturePayload
        _ -> Nothing

    signatureIntegerTarget :: SignaturePayload -> Maybe NumericType
    signatureIntegerTarget signaturePayload =
      case signaturePayload of
        SignatureType (TypeNumeric targetType)
          | integerNumericType targetType -> Just targetType
        ConstrainedSignature _ signatureType ->
          constraintSignatureIntegerTarget signatureType
        _ -> Nothing

    constraintSignatureIntegerTarget :: ConstraintSignatureType -> Maybe NumericType
    constraintSignatureIntegerTarget signatureType =
      case signatureType of
        ConstraintTypeName typeName ->
          case identifierText typeName of
            "Int8" -> Just NumericInt8
            "Int16" -> Just NumericInt16
            "Int32" -> Just NumericInt32
            "Int64" -> Just NumericInt64
            "UInt8" -> Just NumericUInt8
            "UInt16" -> Just NumericUInt16
            "UInt32" -> Just NumericUInt32
            "UInt64" -> Just NumericUInt64
            _ -> Nothing
        _ -> Nothing

    targetedFractionalLiteralBinding :: Int -> Identifier -> Expr -> Maybe (NumericType, Double, FractionalLiteralSource)
    targetedFractionalLiteralBinding statementIndex bindingName valueExpr =
      case valueExpr of
        ELit (LFloat literalValue literalSource) ->
          case previousSignatureFloatTarget statementIndex bindingName of
            Just targetType -> Just (targetType, literalValue, literalSource)
            Nothing -> Nothing
        _ -> Nothing

    previousSignatureFloatTarget :: Int -> Identifier -> Maybe NumericType
    previousSignatureFloatTarget statementIndex bindingName =
      case Map.lookup (statementIndex - 1) statementsByIndex of
        Just (SSignature signatureName _ signaturePayload)
          | identifierText signatureName == identifierText bindingName ->
              signatureFloatTarget signaturePayload
        _ -> Nothing

    signatureFloatTarget :: SignaturePayload -> Maybe NumericType
    signatureFloatTarget signaturePayload =
      case signaturePayload of
        SignatureType (TypeNumeric NumericFloat16) -> Just NumericFloat16
        SignatureType (TypeNumeric NumericFloat32) -> Just NumericFloat32
        ConstrainedSignature _ signatureType ->
          constraintSignatureFloatTarget signatureType
        _ -> Nothing

    constraintSignatureFloatTarget :: ConstraintSignatureType -> Maybe NumericType
    constraintSignatureFloatTarget signatureType =
      case signatureType of
        ConstraintTypeName typeName ->
          case identifierText typeName of
            "Float16" -> Just NumericFloat16
            "Float32" -> Just NumericFloat32
            _ -> Nothing
        _ -> Nothing

    floatConversionBuiltinForTarget :: NumericType -> BuiltinSymbol
    floatConversionBuiltinForTarget targetType =
      case targetType of
        NumericFloat16 -> BuiltinToFloat16
        NumericFloat32 -> BuiltinToFloat32
        NumericFloat64 -> BuiltinToFloat64

    integerConversionBuiltinForTarget :: NumericType -> BuiltinSymbol
    integerConversionBuiltinForTarget targetType =
      case targetType of
        NumericInt8 -> BuiltinToInt8
        NumericInt16 -> BuiltinToInt16
        NumericInt32 -> BuiltinToInt32
        NumericInt64 -> BuiltinToInt64
        NumericUInt8 -> BuiltinToUInt8
        NumericUInt16 -> BuiltinToUInt16
        NumericUInt32 -> BuiltinToUInt32
        NumericUInt64 -> BuiltinToUInt64

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

    bindingEnv :: Int -> Identifier -> RuntimeEnv
    bindingEnv statementIndex bindingName =
      case recursiveBindingNeedsSelf statementIndex of
        True ->
          Map.insert
            (identifierText bindingName)
            (bindingCellAt statementIndex)
            peerVisibleEnv
        False -> peerVisibleEnv
      where
        peerVisibleEnv = recursivePeerEnv statementIndex (envBefore statementIndex)

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
    attachSelfRecursiveBinding :: Int -> Identifier -> RuntimeValue -> RuntimeValue
    attachSelfRecursiveBinding statementIndex bindingName runtimeValue
      | Set.member statementIndex selfRecursiveFunctionStatements =
          case runtimeValue of
            VClosure capturedEnv parameterName bodyExpr ->
              VClosure
                (Map.insert (identifierText bindingName) (bindingCellAt statementIndex) capturedEnv)
                parameterName
                bodyExpr
            _ -> runtimeValue
      | otherwise =
          runtimeValue

    recursiveAliasTarget :: Set Text -> Int -> Expr -> Maybe Int
    recursiveAliasTarget locallyBoundNames statementIndex valueExpr =
      case peelSingleExprBlock valueExpr of
        EVar targetName ->
          if Set.member (identifierText targetName) locallyBoundNames
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
      Set Text ->
      Int ->
      RuntimeEnv ->
      Expr ->
      Either Diagnostic (Maybe Int)
    selectedRecursiveAliasTargetWithBound locallyBoundNames statementIndex env expr =
      case peelSingleExprBlock expr of
        EIf conditionExpr thenExpr elseExpr ->
          selectRecursiveAliasTarget locallyBoundNames statementIndex env conditionExpr thenExpr elseExpr
        ECase conditionExpr thenExpr elseExpr ->
          selectRecursiveAliasTarget locallyBoundNames statementIndex env conditionExpr thenExpr elseExpr
        EPatternCase scrutineeExpr caseArms -> do
          scrutineeValue <- evalValue builtinMode env scrutineeExpr
          case selectMatchingCaseArmForAlias env scrutineeValue caseArms of
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

    selectRecursiveAliasTarget :: Set Text -> Int -> RuntimeEnv -> Expr -> Expr -> Expr -> Either Diagnostic (Maybe Int)
    selectRecursiveAliasTarget locallyBoundNames statementIndex env conditionExpr thenExpr elseExpr = do
      conditionValue <- evalValue builtinMode env conditionExpr
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
      RuntimeEnv ->
      RuntimeValue ->
      [CaseArm] ->
      Maybe (Set Text, RuntimeEnv, Expr)
    selectMatchingCaseArmForAlias env scrutineeValue =
      foldr chooseArm Nothing
      where
        chooseArm caseArm nextMatch =
          case matchCaseArm env scrutineeValue caseArm of
            Just (armEnv, bodyExpr) ->
              Just
                ( caseArmBoundNames caseArm,
                  armEnv,
                  bodyExpr
                )
            Nothing -> nextMatch

    caseArmBoundNames :: CaseArm -> Set Text
    caseArmBoundNames (CaseArm pattern _) =
      patternBoundNames pattern

    -- Single-expression blocks are semantically transparent here, so peel
    -- them before following recursive alias edges and cycle detection.
    peelSingleExprBlock :: Expr -> Expr
    peelSingleExprBlock expr =
      case expr of
        EBlock [SExpr _ innerExpr] -> peelSingleExprBlock innerExpr
        _ -> expr

    lookupRecursivePeer :: Identifier -> [Int] -> Maybe Int
    lookupRecursivePeer targetName =
      foldl' chooseTarget Nothing
      where
        targetNameText = identifierText targetName

        chooseTarget currentChoice peerIndex =
          case Map.lookup peerIndex bindingNamesByStatement of
            Just peerName
              | peerName == targetNameText ->
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
            (identifierText bindingName)
            (bindingCellAt statementIndex)
            (envBefore statementIndex)
        Just (SData _ typeName typeParameters constructors) ->
          insertDataConstructors typeName typeParameters constructors (envBefore statementIndex)
        Just (SClass _ capabilityName parameters methods) ->
          insertClassMethods capabilityName parameters methods (envBefore statementIndex)
        Just (SImpl _ capabilityName arguments methods) ->
          insertImplMethods capabilityName arguments methods (envBefore statementIndex)
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

    insertDataConstructors :: Identifier -> [Identifier] -> [DataConstructor] -> RuntimeEnv -> RuntimeEnv
    insertDataConstructors typeName typeParameters constructors env =
      foldl' insertConstructor env constructors
      where
        insertConstructor envAcc (DataConstructor constructorName constructorArguments) =
          Map.insert
            (identifierText constructorName)
            (Right (VConstructor typeName typeParameters constructorName constructorArguments []))
            envAcc

    insertClassMethods :: Identifier -> [Identifier] -> [ClassMethodSignature] -> RuntimeEnv -> RuntimeEnv
    insertClassMethods capabilityName parameters methods env =
      case parameters of
        [classParameter] ->
          foldl' (insertMethod (identifierText classParameter)) env methods
        _ -> env
      where
        insertMethod classParameter envAcc (ClassMethodSignature methodName _ methodSignature) =
          let methodKey = qualifiedMethodKey capabilityName methodName
           in Map.insert methodKey (Right (VQualifiedMethod methodKey classParameter methodSignature [] [])) envAcc

    insertImplMethods :: Identifier -> [ConstraintSignatureType] -> [ImplMethod] -> RuntimeEnv -> RuntimeEnv
    insertImplMethods capabilityName arguments methods env =
      case arguments of
        [implTarget]
          | concreteConstraintArgument implTarget ->
              foldl' (insertMethod implTarget) env methods
        _ -> env
      where
        methodEnv = env
        insertMethod implTarget envAcc (ImplMethod methodName _ methodExpr) =
          let methodKey = qualifiedMethodKey capabilityName methodName
              methodCandidate = RuntimeMethodCandidate implTarget (evalValue builtinMode methodEnv methodExpr)
           in Map.adjust (addMethodCandidate methodCandidate) methodKey envAcc

        addMethodCandidate methodCandidate methodCell =
          case methodCell of
            Right (VQualifiedMethod methodKey classParameter methodSignature candidates capturedArgs) ->
              Right (VQualifiedMethod methodKey classParameter methodSignature (candidates ++ [methodCandidate]) capturedArgs)
            _ -> methodCell

-- Match the type checker: self-seed recursion when any branch exposes a
-- lambda, so wrapped self-recursive closures capture their own binding before
-- runtime branch selection happens.
exprContainsFunctionBranch :: Expr -> Bool
exprContainsFunctionBranch expr =
  case expr of
    ELambda {} -> True
    EIf _ thenExpr elseExpr ->
      exprContainsFunctionBranch thenExpr
        || exprContainsFunctionBranch elseExpr
    ECase _ thenExpr elseExpr ->
      exprContainsFunctionBranch thenExpr
        || exprContainsFunctionBranch elseExpr
    EPatternCase _ caseArms ->
      any
        (\(CaseArm _ bodyExpr) -> exprContainsFunctionBranch bodyExpr)
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
          case Map.lookup (identifierText bindingName) scopeBindings of
            Just bindingExpr
              | Set.notMember (identifierText bindingName) visitedBindings ->
                  exprContainsFunctionBranchViaScopeBindings
                    scopeBindings
                    (Set.insert (identifierText bindingName) visitedBindings)
                    bindingExpr
            _ -> False
        ELambda {} -> True
        EIf _ thenExpr elseExpr ->
          exprContainsFunctionBranchViaScopeBindings scopeBindings visitedBindings thenExpr
            || exprContainsFunctionBranchViaScopeBindings scopeBindings visitedBindings elseExpr
        ECase _ thenExpr elseExpr ->
          exprContainsFunctionBranchViaScopeBindings scopeBindings visitedBindings thenExpr
            || exprContainsFunctionBranchViaScopeBindings scopeBindings visitedBindings elseExpr
        EPatternCase _ caseArms ->
          any
            (\(CaseArm _ bodyExpr) ->
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
              Map.insert (identifierText bindingName) valueExpr scopeBindings
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
    EIf _ thenExpr elseExpr ->
      exprDefinitelyNotFunctionValue thenExpr
        && exprDefinitelyNotFunctionValue elseExpr
    ECase _ thenExpr elseExpr ->
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

evalValue builtinMode env expr =
  case expr of
    ELit literal -> Right (literalRuntimeValue literal)
    EVar name ->
      case Map.lookup nameText env of
        Just value -> value
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
      Right (VClosure env parameterName bodyExpr)
    EOperatorValue operatorSymbol ->
      Right (VOperator operatorSymbol [])
    EList elements ->
      VList <$> mapM (evalValue builtinMode env) elements
    ETuple elements ->
      VTuple <$> mapM (evalValue builtinMode env) elements
    EApply functionExpr argumentExpr -> do
      functionValue <- evalValue builtinMode env functionExpr
      argumentValue <- evalValue builtinMode env argumentExpr
      applyRuntimeFunction builtinMode functionValue argumentValue
    EIf conditionExpr thenExpr elseExpr ->
      evalValue builtinMode env (ECase conditionExpr thenExpr elseExpr)
    ECase conditionExpr thenExpr elseExpr -> do
      conditionValue <- evalValue builtinMode env conditionExpr
      case conditionValue of
        VBool True -> evalValue builtinMode env thenExpr
        VBool False -> evalValue builtinMode env elseExpr
        other ->
          Left
            ( runtimeDiagnostic
                "E3003"
                ("runtime branch condition must be Bool, found " <> renderRuntimeType other)
            )
    EPatternCase scrutineeExpr caseArms -> do
      scrutineeValue <- evalValue builtinMode env scrutineeExpr
      evalPatternCase builtinMode env scrutineeValue caseArms
    EBinary operatorSymbol leftExpr rightExpr -> do
      leftValue <- evalValue builtinMode env leftExpr
      rightValue <- evalValue builtinMode env rightExpr
      evalBinary builtinMode operatorSymbol leftValue rightValue
    ESectionLeft leftExpr operatorSymbol -> do
      leftValue <- evalValue builtinMode env leftExpr
      Right (VSectionLeft operatorSymbol leftValue)
    ESectionRight operatorSymbol rightExpr -> do
      rightValue <- evalValue builtinMode env rightExpr
      Right (VSectionRight operatorSymbol rightValue)
    EBlock statements ->
      case evalScope builtinMode env statements of
        Left err -> Left err
        Right Nothing ->
          Left
            (runtimeDiagnostic "E3006" "block expression has no terminal expression result at runtime")
        Right (Just value) -> Right value

literalRuntimeValue :: Literal -> RuntimeValue
literalRuntimeValue literal =
  case literal of
    LInt value -> VInt value untypedIntMetadata
    LFloat value literalSource -> VFloat value (untypedFloatMetadata (Just literalSource))
    LBool value -> VBool value

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
  RuntimeFloatMetadata
    { runtimeFloatLiteralSource = Nothing,
      runtimeFloatTargetType = Just targetType
    }

evalPatternCase ::
  BuiltinResolutionMode ->
  RuntimeEnv ->
  RuntimeValue ->
  [CaseArm] ->
  Either Diagnostic RuntimeValue
evalPatternCase builtinMode env scrutineeValue caseArms =
  case selectMatchingCaseArm env scrutineeValue caseArms of
    Just (armEnv, bodyExpr) ->
      evalValue builtinMode armEnv bodyExpr
    Nothing ->
      Left
        ( runtimeDiagnostic
            "E3022"
            "pattern case matched no arms"
        )

selectMatchingCaseArm ::
  RuntimeEnv ->
  RuntimeValue ->
  [CaseArm] ->
  Maybe (RuntimeEnv, Expr)
selectMatchingCaseArm env scrutineeValue =
  foldr chooseArm Nothing
  where
    chooseArm caseArm nextMatch =
      case matchCaseArm env scrutineeValue caseArm of
        Just matchedArm -> Just matchedArm
        Nothing -> nextMatch

-- | Pattern bindings are prepended to the arm environment so they shadow outer
-- runtime bindings only while evaluating the selected arm body.
matchCaseArm ::
  RuntimeEnv ->
  RuntimeValue ->
  CaseArm ->
  Maybe (RuntimeEnv, Expr)
matchCaseArm env scrutineeValue (CaseArm pattern bodyExpr) =
  case matchPattern scrutineeValue pattern of
    Just patternBindings ->
      Just (Map.union patternBindings env, bodyExpr)
    Nothing -> Nothing

matchPattern :: RuntimeValue -> Pattern -> Maybe RuntimeEnv
matchPattern scrutineeValue pattern =
  case pattern of
    PWildcard -> Just Map.empty
    PVariable name ->
      Just
        (Map.singleton (identifierText name) (Right scrutineeValue))
    PLiteral literal
      | scrutineeValue == literalRuntimeValue literal ->
          Just Map.empty
      | otherwise ->
          Nothing
    PConstructor constructorName patterns ->
      case scrutineeValue of
        VConstructor _ _ valueConstructorName constructorArguments capturedArgs
          | valueConstructorName == constructorName,
            constructorIsSaturated constructorArguments capturedArgs,
            length capturedArgs == length patterns ->
              matchPatternList capturedArgs patterns
        _ -> Nothing
    PList patterns ->
      case scrutineeValue of
        VList elements
          | length elements == length patterns ->
              matchPatternList elements patterns
        _ -> Nothing
    PConsList headPattern tailPattern ->
      case scrutineeValue of
        VList (headValue : tailValues) -> do
          headBindings <- matchPattern headValue headPattern
          tailBindings <- matchPattern (VList tailValues) tailPattern
          Just (tailBindings `Map.union` headBindings)
        _ -> Nothing
    PTuple patterns ->
      case scrutineeValue of
        VTuple elements
          | length elements == length patterns ->
              matchPatternList elements patterns
        _ -> Nothing
    PAs name pattern -> do
      patternBindings <- matchPattern scrutineeValue pattern
      Just (Map.insert (identifierText name) (Right scrutineeValue) patternBindings)

matchPatternList :: [RuntimeValue] -> [Pattern] -> Maybe RuntimeEnv
matchPatternList values patterns =
  foldM step Map.empty (zip values patterns)
  where
    step bindings (value, pattern) =
      case matchPattern value pattern of
        Just patternBindings -> Just (patternBindings `Map.union` bindings)
        Nothing -> Nothing

-- | Apply any callable runtime value, including sections, builtin primitives,
-- and curried operator values.
applyRuntimeFunction :: BuiltinResolutionMode -> RuntimeValue -> RuntimeValue -> Either Diagnostic RuntimeValue
applyRuntimeFunction builtinMode functionValue argumentValue =
  case functionValue of
    VSectionLeft operatorSymbol leftValue ->
      evalBinary builtinMode operatorSymbol leftValue argumentValue
    VSectionRight operatorSymbol rightValue ->
      evalBinary builtinMode operatorSymbol argumentValue rightValue
    VClosure capturedEnv parameterName bodyExpr ->
      evalValue
        builtinMode
        (Map.insert (identifierText parameterName) (Right argumentValue) capturedEnv)
        bodyExpr
    VBuiltin builtinFunction capturedArgs ->
      applyBuiltin builtinMode builtinFunction (capturedArgs ++ [argumentValue])
    VOperator operatorSymbol capturedArgs ->
      applyOperator builtinMode operatorSymbol (capturedArgs ++ [argumentValue])
    VConstructor typeName typeParameters constructorName constructorArguments capturedArgs ->
      applyConstructor typeName typeParameters constructorName constructorArguments (capturedArgs ++ [argumentValue])
    VQualifiedMethod methodKey classParameter methodSignature candidates capturedArgs ->
      applyQualifiedMethod
        builtinMode
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

applyQualifiedMethod ::
  BuiltinResolutionMode ->
  Text ->
  Text ->
  SignaturePayload ->
  [RuntimeMethodCandidate] ->
  [RuntimeValue] ->
  Either Diagnostic RuntimeValue
applyQualifiedMethod builtinMode methodKey classParameter methodSignature candidates arguments =
  case matchingCandidates of
    [] ->
      Left (runtimeDiagnostic "E3026" ("no matching qualified method body '" <> methodKey <> "'"))
    [RuntimeMethodCandidate _ methodCell] ->
      applyRuntimeMethodCandidate builtinMode methodCell arguments
    _
      | runtimeQualifiedMethodIsFullyApplied classParameter methodSignature arguments matchingCandidates ->
          Left (runtimeDiagnostic "E3026" ("ambiguous qualified method body '" <> methodKey <> "'"))
      | otherwise ->
          Right (VQualifiedMethod methodKey classParameter methodSignature matchingCandidates arguments)
  where
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
    candidateIsFullyApplied (RuntimeMethodCandidate implTarget _) =
      case substituteClassMethodSignature classParameter implTarget methodSignature of
        Just substitutedSignature ->
          let (argumentTypes, _) = constraintFunctionArgumentTypes substitutedSignature
           in length arguments >= length argumentTypes
        Nothing ->
          False

applyRuntimeMethodCandidate ::
  BuiltinResolutionMode ->
  Either Diagnostic RuntimeValue ->
  [RuntimeValue] ->
  Either Diagnostic RuntimeValue
applyRuntimeMethodCandidate builtinMode methodCell arguments = do
  methodValue <- methodCell
  foldM (applyRuntimeFunction builtinMode) methodValue arguments

runtimeMethodCandidateMatches :: Text -> SignaturePayload -> [RuntimeValue] -> RuntimeMethodCandidate -> Bool
runtimeMethodCandidateMatches classParameter methodSignature arguments (RuntimeMethodCandidate implTarget _) =
  case substituteClassMethodSignature classParameter implTarget methodSignature of
    Just substitutedSignature ->
      let (argumentTypes, _) = constraintFunctionArgumentTypes substitutedSignature
       in length arguments <= length argumentTypes
            && and (zipWith runtimeValueMatchesConstraint argumentTypes arguments)
    Nothing ->
      False

runtimeValueMatchesConstraint :: ConstraintSignatureType -> RuntimeValue -> Bool
runtimeValueMatchesConstraint signatureType runtimeValue =
  case signatureType of
    ConstraintTypeName typeName ->
      runtimeValueMatchesTypeName (identifierText typeName) runtimeValue
    ConstraintTypeApplication typeName typeArguments ->
      runtimeValueMatchesDataTypeApplication typeName typeArguments runtimeValue
    ConstraintTypeList elementType ->
      case runtimeValue of
        VList elements -> all (runtimeValueMatchesConstraint elementType) elements
        _ -> False
    ConstraintTypeTuple elementTypes ->
      case runtimeValue of
        VTuple elements
          | length elementTypes == length elements ->
              and (zipWith runtimeValueMatchesConstraint elementTypes elements)
        _ -> False
    ConstraintTypeFunction {} ->
      isFunctionValue runtimeValue

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
    _ -> runtimeValueMatchesDataTypeName typeName runtimeValue

runtimeValueMatchesDataTypeName :: Text -> RuntimeValue -> Bool
runtimeValueMatchesDataTypeName typeName runtimeValue =
  case runtimeValue of
    VConstructor valueTypeName _ _ constructorArguments capturedArgs ->
      identifierText valueTypeName == typeName
        && constructorIsSaturated constructorArguments capturedArgs
    _ -> False

runtimeValueMatchesDataTypeApplication :: Identifier -> [ConstraintSignatureType] -> RuntimeValue -> Bool
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

runtimeValueMatchesConstructorArgument :: Map Text ConstraintSignatureType -> DataConstructorArgument -> RuntimeValue -> Bool
runtimeValueMatchesConstructorArgument typeParameterBindings constructorArgument runtimeValue =
  case constructorArgument of
    DataConstructorArgumentName argumentName ->
      case Map.lookup (identifierText argumentName) typeParameterBindings of
        Just concreteArgumentType ->
          runtimeValueMatchesConstraint concreteArgumentType runtimeValue
        Nothing ->
          runtimeValueMatchesConstraint (ConstraintTypeName argumentName) runtimeValue
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

applyOperator :: BuiltinResolutionMode -> Text -> [RuntimeValue] -> Either Diagnostic RuntimeValue
applyOperator builtinMode operatorSymbol arguments =
  case arguments of
    [leftValue] ->
      Right (VOperator operatorSymbol [leftValue])
    [leftValue, rightValue] ->
      evalBinary builtinMode operatorSymbol leftValue rightValue
    _ ->
      Left
        ( runtimeDiagnostic
            "E3016"
            ("runtime primitive '" <> operatorSymbol <> "' received invalid arguments")
        )

-- | Constructor values are curried like builtins until their declared arity is
-- saturated; extra applications are runtime errors.
applyConstructor :: Identifier -> [Identifier] -> Identifier -> [DataConstructorArgument] -> [RuntimeValue] -> Either Diagnostic RuntimeValue
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
applyBuiltin :: BuiltinResolutionMode -> BuiltinSymbol -> [RuntimeValue] -> Either Diagnostic RuntimeValue
applyBuiltin builtinMode builtinFunction arguments
  | length arguments < builtinSymbolArity builtinFunction =
      Right (VBuiltin builtinFunction arguments)
  | length arguments == builtinSymbolArity builtinFunction =
      evalBuiltin builtinMode builtinFunction arguments
  | otherwise =
      Left
        ( runtimeDiagnostic
            "E3014"
            ("runtime primitive '" <> builtinSymbolName builtinFunction <> "' received too many arguments")
        )

-- | Evaluate builtin semantics once enough arguments have been collected.
evalBuiltin :: BuiltinResolutionMode -> BuiltinSymbol -> [RuntimeValue] -> Either Diagnostic RuntimeValue
evalBuiltin builtinMode builtinFunction arguments =
  case (builtinFunction, arguments) of
    (_, [value])
      | Just targetType <- builtinSymbolNumericConversionTarget builtinFunction ->
          evalNumericConversion builtinFunction targetType value
    (BuiltinHd, [VList []]) ->
      Left (runtimeDiagnostic "E3009" "runtime primitive 'hd' failed: empty list")
    (BuiltinHd, [VList (headValue : _)]) ->
      Right headValue
    (BuiltinHd, [other]) ->
      Left
        ( runtimeDiagnostic
            "E3011"
            ("runtime primitive 'hd' expects a list argument, found " <> renderRuntimeType other)
        )
    (BuiltinTl, [VList []]) ->
      Left (runtimeDiagnostic "E3010" "runtime primitive 'tl' failed: empty list")
    (BuiltinTl, [VList (_ : tailValues)]) ->
      Right (VList tailValues)
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
            VList elements ->
              VList <$> mapM (applyRuntimeFunction builtinMode mapper) elements
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
            VList elements ->
              VList <$> filterElements builtinMode predicate elements
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
    _ ->
      Left
        ( runtimeDiagnostic
            "E3016"
            ("runtime primitive '" <> builtinSymbolName builtinFunction <> "' received invalid arguments")
        )

evalNumericConversion :: BuiltinSymbol -> NumericType -> RuntimeValue -> Either Diagnostic RuntimeValue
evalNumericConversion builtinFunction targetType value =
  case value of
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
    else Right (VFloat (roundFloatTarget targetType floatValue) (targetedFloatMetadata targetType))

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

renderNumericTypeName :: NumericType -> Text
renderNumericTypeName numericType =
  case numericType of
    NumericInt8 -> "Int8"
    NumericInt16 -> "Int16"
    NumericInt32 -> "Int32"
    NumericInt64 -> "Int64"
    NumericUInt8 -> "UInt8"
    NumericUInt16 -> "UInt16"
    NumericUInt32 -> "UInt32"
    NumericUInt64 -> "UInt64"
    NumericFloat16 -> "Float16"
    NumericFloat32 -> "Float32"
    NumericFloat64 -> "Float64"

integerNumericType :: NumericType -> Bool
integerNumericType targetType =
  case numericTypeIntegerBounds targetType of
    Just _ -> True
    Nothing -> False

-- | Evaluate filter predicates element-by-element and enforce that each
-- predicate application returns a Bool.
filterElements :: BuiltinResolutionMode -> RuntimeValue -> [RuntimeValue] -> Either Diagnostic [RuntimeValue]
filterElements builtinMode predicate values = do
  results <- mapM applyPredicate values
  pure [value | (value, True) <- results]
  where
    -- Preserve runtime safety for partially-known function values that can slip
    -- past compile-time checks in direct `evaluateRuntimeExpr` tests.
    applyPredicate :: RuntimeValue -> Either Diagnostic (RuntimeValue, Bool)
    applyPredicate value = do
      predicateResult <- applyRuntimeFunction builtinMode predicate value
      case predicateResult of
        VBool shouldKeep -> Right (value, shouldKeep)
        other ->
          Left
            ( runtimeDiagnostic
                "E3019"
                ("runtime primitive 'filter' predicate must return Bool, found " <> renderRuntimeType other)
            )

isFunctionValue :: RuntimeValue -> Bool
isFunctionValue value =
  case value of
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
evalBinary :: BuiltinResolutionMode -> Text -> RuntimeValue -> RuntimeValue -> Either Diagnostic RuntimeValue
evalBinary builtinMode operatorSymbol leftValue rightValue
  | isStrictEqualityOperator operatorSymbol,
    isFunctionValue leftValue || isFunctionValue rightValue =
      Left (runtimeCallableEqualityDiagnostic operatorSymbol leftValue rightValue)
  | otherwise =
  case (operatorSymbol, leftValue, rightValue) of
    ("+", VInt leftInt leftMetadata, VInt rightInt rightMetadata) ->
      Right (VInt (leftInt + rightInt) (integerBinaryMetadata leftMetadata rightMetadata))
    ("-", VInt leftInt leftMetadata, VInt rightInt rightMetadata) ->
      Right (VInt (leftInt - rightInt) (integerBinaryMetadata leftMetadata rightMetadata))
    ("*", VInt leftInt leftMetadata, VInt rightInt rightMetadata) ->
      Right (VInt (leftInt * rightInt) (integerBinaryMetadata leftMetadata rightMetadata))
    ("/", VInt _ _, VInt 0 _) ->
      Left (runtimeDiagnostic "E3001" "runtime primitive '/' failed: division by zero")
    ("/", VInt leftInt leftMetadata, VInt rightInt rightMetadata) ->
      Right (VInt (leftInt `div` rightInt) (integerBinaryMetadata leftMetadata rightMetadata))
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
    ("<", VInt leftInt _, VInt rightInt _) -> Right (VBool (leftInt < rightInt))
    ("<=", VInt leftInt _, VInt rightInt _) -> Right (VBool (leftInt <= rightInt))
    (">", VInt leftInt _, VInt rightInt _) -> Right (VBool (leftInt > rightInt))
    (">=", VInt leftInt _, VInt rightInt _) -> Right (VBool (leftInt >= rightInt))
    ("<", VFloat leftFloat _, VFloat rightFloat _) -> Right (VBool (leftFloat < rightFloat))
    ("<=", VFloat leftFloat _, VFloat rightFloat _) -> Right (VBool (leftFloat <= rightFloat))
    (">", VFloat leftFloat _, VFloat rightFloat _) -> Right (VBool (leftFloat > rightFloat))
    (">=", VFloat leftFloat _, VFloat rightFloat _) -> Right (VBool (leftFloat >= rightFloat))
    ("==", VInt leftInt _, VInt rightInt _) -> Right (VBool (leftInt == rightInt))
    ("==", VFloat leftFloat _, VFloat rightFloat _) -> Right (VBool (leftFloat == rightFloat))
    ("==", VBool leftBool, VBool rightBool) -> Right (VBool (leftBool == rightBool))
    ("==", VList {}, VList {}) -> evalStructuralEquality "==" leftValue rightValue
    ("==", VTuple {}, VTuple {}) -> evalStructuralEquality "==" leftValue rightValue
    ("==", VConstructor {}, VConstructor {}) -> evalStructuralEquality "==" leftValue rightValue
    ("!=", VInt leftInt _, VInt rightInt _) -> Right (VBool (leftInt /= rightInt))
    ("!=", VFloat leftFloat _, VFloat rightFloat _) -> Right (VBool (leftFloat /= rightFloat))
    ("!=", VBool leftBool, VBool rightBool) -> Right (VBool (leftBool /= rightBool))
    ("!=", VList {}, VList {}) -> evalStructuralEquality "!=" leftValue rightValue
    ("!=", VTuple {}, VTuple {}) -> evalStructuralEquality "!=" leftValue rightValue
    ("!=", VConstructor {}, VConstructor {}) -> evalStructuralEquality "!=" leftValue rightValue
    ("$", functionValue, argumentValue) ->
      applyRuntimeFunction builtinMode functionValue argumentValue
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

integerBinaryMetadata :: RuntimeIntMetadata -> RuntimeIntMetadata -> RuntimeIntMetadata
integerBinaryMetadata leftMetadata rightMetadata =
  case (runtimeIntTargetType leftMetadata, runtimeIntTargetType rightMetadata) of
    (Just leftTarget, Just rightTarget)
      | leftTarget == rightTarget -> targetedIntMetadata leftTarget
    (Just leftTarget, Nothing) -> targetedIntMetadata leftTarget
    (Nothing, Just rightTarget) -> targetedIntMetadata rightTarget
    _ -> untypedIntMetadata

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

runtimeStructuralEquality :: RuntimeValue -> RuntimeValue -> Maybe Bool
runtimeStructuralEquality leftValue rightValue =
  case (leftValue, rightValue) of
    (VInt leftInt _, VInt rightInt _) -> Just (leftInt == rightInt)
    (VFloat leftFloat _, VFloat rightFloat _) -> Just (leftFloat == rightFloat)
    (VBool leftBool, VBool rightBool) -> Just (leftBool == rightBool)
    (VList leftElements, VList rightElements) ->
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

constructorIsSaturated :: [DataConstructorArgument] -> [RuntimeValue] -> Bool
constructorIsSaturated constructorArguments capturedArgs =
  length capturedArgs >= length constructorArguments

extendBoundWithPattern :: Pattern -> Set Text -> Set Text
extendBoundWithPattern pattern bound =
  Set.union bound (patternBoundNames pattern)

patternBoundNames :: Pattern -> Set Text
patternBoundNames pattern =
  case pattern of
    PVariable name -> Set.singleton (identifierText name)
    PWildcard -> Set.empty
    PLiteral {} -> Set.empty
    PConstructor _ patterns ->
      Set.unions (map patternBoundNames patterns)
    PList patterns ->
      Set.unions (map patternBoundNames patterns)
    PConsList headPattern tailPattern ->
      Set.union (patternBoundNames headPattern) (patternBoundNames tailPattern)
    PTuple patterns ->
      Set.unions (map patternBoundNames patterns)
    PAs name pattern ->
      Set.insert (identifierText name) (patternBoundNames pattern)
