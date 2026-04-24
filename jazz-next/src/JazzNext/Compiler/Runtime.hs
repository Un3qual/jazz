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

import Data.List (foldl')
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as Text
import JazzNext.Compiler.AST
  ( CaseArm (..),
    DataConstructor (..),
    Expr (..),
    Literal (..),
    Pattern (..),
    Statement (..)
  )
import JazzNext.Compiler.Diagnostics
  ( Diagnostic,
    mkDiagnostic
  )
import JazzNext.Compiler.BuiltinCatalog
  ( BuiltinResolutionMode (..),
    BuiltinSymbol (..),
    builtinNamesInMode,
    builtinSymbolArity,
    builtinSymbolName,
    lookupBuiltinSymbolInMode
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
data RuntimeValue
  = VInt Int
  | VBool Bool
  | VList [RuntimeValue]
  | VClosure RuntimeEnv Identifier Expr
  | VBuiltin BuiltinSymbol [RuntimeValue]
  | VOperator Text [RuntimeValue]
  | VSectionLeft Text RuntimeValue
  | VSectionRight Text RuntimeValue
  | VConstructor Identifier Int [RuntimeValue]

instance Eq RuntimeValue where
  leftValue == rightValue =
    case (leftValue, rightValue) of
      (VInt leftInt, VInt rightInt) -> leftInt == rightInt
      (VBool leftBool, VBool rightBool) -> leftBool == rightBool
      (VList leftElements, VList rightElements) -> leftElements == rightElements
      -- Captured environments are intentionally ignored here because recursive
      -- closures tie the knot through RuntimeEnv; comparing envs would
      -- reintroduce the recursion hazards this custom instance avoids.
      (VClosure _ leftParameter leftBody, VClosure _ rightParameter rightBody) ->
        leftParameter == rightParameter && leftBody == rightBody
      (VBuiltin leftBuiltin leftArgs, VBuiltin rightBuiltin rightArgs) ->
        leftBuiltin == rightBuiltin && leftArgs == rightArgs
      (VOperator leftOperator leftArgs, VOperator rightOperator rightArgs) ->
        leftOperator == rightOperator && leftArgs == rightArgs
      (VSectionLeft leftOperator leftOperand, VSectionLeft rightOperator rightOperand) ->
        leftOperator == rightOperator && leftOperand == rightOperand
      (VSectionRight leftOperator leftOperand, VSectionRight rightOperator rightOperand) ->
        leftOperator == rightOperator && leftOperand == rightOperand
      (VConstructor leftName leftArity leftArgs, VConstructor rightName rightArity rightArgs) ->
        leftName == rightName && leftArity == rightArity && leftArgs == rightArgs
      _ -> False

instance Show RuntimeValue where
  show value =
    case value of
      VInt intValue -> "VInt " <> show intValue
      VBool boolValue -> "VBool " <> show boolValue
      VList elements -> "VList " <> show elements
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
      VConstructor constructorName constructorArity capturedArgs ->
        "VConstructor " <> show constructorName <> " " <> show constructorArity <> " " <> show capturedArgs

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
    VInt intValue -> Text.pack (show intValue)
    VBool boolValue ->
      if boolValue
        then "True"
        else "False"
    VList elements ->
      "[" <> Text.intercalate ", " (map renderRuntimeValue elements) <> "]"
    VClosure {} -> "<function>"
    VBuiltin _ _ -> "<function>"
    VOperator {} -> "<function>"
    VSectionLeft {} -> "<function>"
    VSectionRight {} -> "<function>"
    VConstructor constructorName constructorArity capturedArgs
      | constructorIsSaturated constructorArity capturedArgs ->
          renderConstructorValue constructorName capturedArgs
      | otherwise ->
          "<function>"

renderConstructorValue :: Identifier -> [RuntimeValue] -> Text
renderConstructorValue constructorName arguments =
  case arguments of
    [] -> identifierText constructorName
    _ ->
      identifierText constructorName
        <> "("
        <> Text.intercalate ", " (map renderRuntimeValue arguments)
        <> ")"

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
            SData _ _ constructors ->
              go (insertDataConstructors constructors env) Nothing rest
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
                evaluatedValue <- evalValue builtinMode visibleEnv valueExpr
                Right (attachSelfRecursiveBinding statementIndex bindingName evaluatedValue)
      where
        visibleEnv = bindingEnv statementIndex bindingName

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
        Just (SData _ _ constructors) ->
          insertDataConstructors constructors (envBefore statementIndex)
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

    insertDataConstructors :: [DataConstructor] -> RuntimeEnv -> RuntimeEnv
    insertDataConstructors constructors env =
      foldl' insertConstructor env constructors
      where
        insertConstructor envAcc (DataConstructor constructorName constructorArity) =
          Map.insert
            (identifierText constructorName)
            (Right (VConstructor constructorName constructorArity []))
            envAcc

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
    LInt value -> VInt value
    LBool value -> VBool value

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
        VConstructor valueConstructorName constructorArity capturedArgs
          | valueConstructorName == constructorName,
            constructorIsSaturated constructorArity capturedArgs,
            length capturedArgs == length patterns ->
              matchPatternList capturedArgs patterns
        _ -> Nothing
    PList patterns ->
      case scrutineeValue of
        VList elements
          | length elements == length patterns ->
              matchPatternList elements patterns
        _ -> Nothing

matchPatternList :: [RuntimeValue] -> [Pattern] -> Maybe RuntimeEnv
matchPatternList values patterns =
  foldl' step (Just Map.empty) (zip values patterns)
  where
    step Nothing _ = Nothing
    step (Just bindings) (value, pattern) =
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
    VConstructor constructorName constructorArity capturedArgs ->
      applyConstructor constructorName constructorArity (capturedArgs ++ [argumentValue])
    _ ->
      Left
        ( runtimeDiagnostic
            "E3008"
            ("runtime cannot apply non-function value of type " <> renderRuntimeType functionValue)
        )

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

applyConstructor :: Identifier -> Int -> [RuntimeValue] -> Either Diagnostic RuntimeValue
applyConstructor constructorName constructorArity arguments
  | length arguments <= constructorArity =
      Right (VConstructor constructorName constructorArity arguments)
  | otherwise =
      Left
        ( runtimeDiagnostic
            "E3023"
            ("runtime constructor '" <> identifierText constructorName <> "' received too many arguments")
        )

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
    VConstructor _ constructorArity capturedArgs ->
      not (constructorIsSaturated constructorArity capturedArgs)
    _ -> False

-- | Evaluate the builtin operator subset supported by the runtime.
evalBinary :: BuiltinResolutionMode -> Text -> RuntimeValue -> RuntimeValue -> Either Diagnostic RuntimeValue
evalBinary builtinMode operatorSymbol leftValue rightValue =
  case (operatorSymbol, leftValue, rightValue) of
    ("+", VInt leftInt, VInt rightInt) -> Right (VInt (leftInt + rightInt))
    ("-", VInt leftInt, VInt rightInt) -> Right (VInt (leftInt - rightInt))
    ("*", VInt leftInt, VInt rightInt) -> Right (VInt (leftInt * rightInt))
    ("/", VInt _, VInt 0) ->
      Left (runtimeDiagnostic "E3001" "runtime primitive '/' failed: division by zero")
    ("/", VInt leftInt, VInt rightInt) ->
      Right (VInt (leftInt `div` rightInt))
    ("<", VInt leftInt, VInt rightInt) -> Right (VBool (leftInt < rightInt))
    ("<=", VInt leftInt, VInt rightInt) -> Right (VBool (leftInt <= rightInt))
    (">", VInt leftInt, VInt rightInt) -> Right (VBool (leftInt > rightInt))
    (">=", VInt leftInt, VInt rightInt) -> Right (VBool (leftInt >= rightInt))
    ("==", VInt leftInt, VInt rightInt) -> Right (VBool (leftInt == rightInt))
    ("==", VBool leftBool, VBool rightBool) -> Right (VBool (leftBool == rightBool))
    ("!=", VInt leftInt, VInt rightInt) -> Right (VBool (leftInt /= rightInt))
    ("!=", VBool leftBool, VBool rightBool) -> Right (VBool (leftBool /= rightBool))
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
    VInt {} -> "Int"
    VBool {} -> "Bool"
    VList {} -> "List"
    VSectionLeft {} -> "Function"
    VSectionRight {} -> "Function"
    VClosure {} -> "Function"
    VBuiltin {} -> "Function"
    VOperator {} -> "Function"
    VConstructor _ constructorArity capturedArgs
      | constructorIsSaturated constructorArity capturedArgs -> "Data"
      | otherwise -> "Function"

constructorIsSaturated :: Int -> [RuntimeValue] -> Bool
constructorIsSaturated constructorArity capturedArgs =
  length capturedArgs >= constructorArity

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
