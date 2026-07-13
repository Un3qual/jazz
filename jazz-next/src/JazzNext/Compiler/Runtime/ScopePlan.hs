{-# LANGUAGE OverloadedStrings #-}

-- | AST-only planning shared by pure and host runtime scope execution.
module JazzNext.Compiler.Runtime.ScopePlan
  ( RuntimeScopePlan,
    buildRuntimeScopePlan,
    scopePlanIndexedStatements,
    scopePlanStatementAt,
    scopePlanModulePathForStatement,
    runtimeModulePathAfterStatements,
    scopePlanRecursiveGroupAt,
    scopePlanIsRecursiveBinding,
    scopePlanIsSelfRecursiveFunction,
    scopePlanBindingNameAt,
    scopePlanIsHostRecursiveBinding,
    scopePlanPreviousSignaturePayload,
    runtimeSignatureNumericTarget,
    runtimeExprRequiresHost,
    runtimeStatementRequiresHost,
    exprContainsFunctionBranch,
    exprDefinitelyNotFunctionValue
  ) where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.List (foldl')
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import JazzNext.Compiler.AST
  ( CaseArm (..),
    Expr (..),
    ImplMethod (..),
    NumericType (..),
    SignaturePayload (..),
    SignatureType (..),
    Statement (..)
  )
import JazzNext.Compiler.BuiltinCatalog
  ( BuiltinResolutionMode (..),
    BuiltinSymbol (..),
    builtinNamesInMode,
    lookupBuiltinSymbolInMode,
    numericTypeFromName
  )
import JazzNext.Compiler.Name
  ( Name,
    identifierText,
    mkIdentifier,
    sourceName
  )
import JazzNext.Compiler.RecursiveBindings
  ( collectBindingNames,
    inferRecursiveGroupsOrdered,
    inferSelfRecursiveBindings
  )

data RuntimeScopePlan = RuntimeScopePlan
  { runtimeScopePlanIndexedStatements :: [(Int, Statement)],
    runtimeScopePlanStatementsByIndex :: IntMap Statement,
    runtimeScopePlanModulePathsByStatement :: IntMap (Maybe [Text]),
    runtimeScopePlanRecursiveGroups :: IntMap [Int],
    runtimeScopePlanSelfRecursiveFunctions :: IntSet,
    runtimeScopePlanBindingNames :: IntMap Name,
    runtimeScopePlanHostRecursiveBindings :: IntSet
  }

buildRuntimeScopePlan ::
  Set Int ->
  Maybe [Text] ->
  BuiltinResolutionMode ->
  Set Name ->
  [Statement] ->
  RuntimeScopePlan
buildRuntimeScopePlan preludeStatementIndices initialModulePath builtinMode outerBindingNames statements =
  RuntimeScopePlan
    { runtimeScopePlanIndexedStatements = indexedStatements,
      runtimeScopePlanStatementsByIndex = statementsByIndex,
      runtimeScopePlanModulePathsByStatement = modulePathsByStatement,
      runtimeScopePlanRecursiveGroups = recursiveGroups,
      runtimeScopePlanSelfRecursiveFunctions = selfRecursiveFunctions,
      runtimeScopePlanBindingNames = bindingNames,
      runtimeScopePlanHostRecursiveBindings = hostRecursiveBindings
    }
  where
    indexedStatements = zip [0 ..] statements
    statementsByIndex = IntMap.fromDistinctAscList indexedStatements
    recursiveGroupsMap =
      inferRecursiveGroupsOrdered
        ( Set.union
            outerBindingNames
            (Set.map (sourceName . mkIdentifier) (builtinNamesInMode builtinMode))
        )
        indexedStatements
    recursiveGroups = IntMap.fromDistinctAscList (Map.toAscList recursiveGroupsMap)
    selfRecursiveFunctions =
      IntSet.fromList
        (Set.toList (inferSelfRecursiveBindings exprContainsFunctionBranch indexedStatements))
    bindingNames =
      IntMap.fromDistinctAscList
        (Map.toAscList (collectBindingNames indexedStatements))
    (_, modulePathsByStatement) =
      foldl'
        collectModulePath
        (initialModulePath, IntMap.empty)
        indexedStatements
    collectModulePath (activeModulePath, pathsByStatement) (statementIndex, statement) =
      let declaredModulePath =
            case statement of
              SModule _ modulePath -> Just modulePath
              _ -> activeModulePath
          statementModulePath =
            if Set.member statementIndex preludeStatementIndices
              then Just []
              else declaredModulePath
       in (declaredModulePath, IntMap.insert statementIndex statementModulePath pathsByStatement)
    hostRecursiveBindings =
      IntSet.fromList
        [ groupIndex
          | (representativeIndex, groupMembers@(firstGroupIndex : _)) <- IntMap.toAscList recursiveGroups,
            representativeIndex == firstGroupIndex,
            any bindingRequiresHost groupMembers,
            groupIndex <- groupMembers
        ]
    bindingRequiresHost statementIndex =
      case IntMap.lookup statementIndex statementsByIndex of
        Just (SLet _ _ valueExpr) -> runtimeExprRequiresHost valueExpr
        _ -> False

scopePlanIndexedStatements :: RuntimeScopePlan -> [(Int, Statement)]
scopePlanIndexedStatements = runtimeScopePlanIndexedStatements

scopePlanStatementAt :: RuntimeScopePlan -> Int -> Maybe Statement
scopePlanStatementAt plan statementIndex =
  IntMap.lookup statementIndex (runtimeScopePlanStatementsByIndex plan)

scopePlanModulePathForStatement :: RuntimeScopePlan -> Int -> Maybe [Text]
scopePlanModulePathForStatement plan statementIndex =
  IntMap.findWithDefault Nothing statementIndex (runtimeScopePlanModulePathsByStatement plan)

runtimeModulePathAfterStatements :: Maybe [Text] -> [Statement] -> Maybe [Text]
runtimeModulePathAfterStatements =
  foldl'
    ( \activeModulePath statement ->
        case statement of
          SModule _ modulePath -> Just modulePath
          _ -> activeModulePath
    )

scopePlanRecursiveGroupAt :: RuntimeScopePlan -> Int -> Maybe [Int]
scopePlanRecursiveGroupAt plan statementIndex =
  IntMap.lookup statementIndex (runtimeScopePlanRecursiveGroups plan)

scopePlanIsRecursiveBinding :: RuntimeScopePlan -> Int -> Bool
scopePlanIsRecursiveBinding plan statementIndex =
  IntMap.member statementIndex (runtimeScopePlanRecursiveGroups plan)

scopePlanIsSelfRecursiveFunction :: RuntimeScopePlan -> Int -> Bool
scopePlanIsSelfRecursiveFunction plan statementIndex =
  IntSet.member statementIndex (runtimeScopePlanSelfRecursiveFunctions plan)

scopePlanBindingNameAt :: RuntimeScopePlan -> Int -> Maybe Name
scopePlanBindingNameAt plan statementIndex =
  IntMap.lookup statementIndex (runtimeScopePlanBindingNames plan)

scopePlanIsHostRecursiveBinding :: RuntimeScopePlan -> Int -> Bool
scopePlanIsHostRecursiveBinding plan statementIndex =
  IntSet.member statementIndex (runtimeScopePlanHostRecursiveBindings plan)

scopePlanPreviousSignaturePayload :: RuntimeScopePlan -> Int -> Name -> Maybe SignaturePayload
scopePlanPreviousSignaturePayload plan statementIndex bindingName =
  case scopePlanStatementAt plan (statementIndex - 1) of
    Just (SSignature signatureName _ signaturePayload)
      | identifierText signatureName == identifierText bindingName ->
          Just signaturePayload
    _ -> Nothing

runtimeSignatureNumericTarget :: SignaturePayload -> Maybe NumericType
runtimeSignatureNumericTarget signaturePayload =
  case signaturePayload of
    SignatureType TypeInt -> Just NumericInt64
    SignatureType TypeFloat -> Just NumericFloat64
    SignatureType (TypeNumeric targetType) -> Just targetType
    ConstrainedSignature _ signatureType -> signatureNumericTarget signatureType
    _ -> Nothing
  where
    signatureNumericTarget signatureType =
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
    EPatternCase {} -> False
    EBlock statements -> scopeDefinitelyNotFunctionValue statements
    _ -> False

scopeDefinitelyNotFunctionValue :: [Statement] -> Bool
scopeDefinitelyNotFunctionValue statements =
  case reverse statements of
    SExpr _ expr : _ -> exprDefinitelyNotFunctionValue expr
    _ -> False
