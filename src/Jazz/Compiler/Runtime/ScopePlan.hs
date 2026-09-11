{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- | AST-only planning shared by pure and host runtime scope execution.
module Jazz.Compiler.Runtime.ScopePlan
  ( RuntimeScopePlan,
    buildRuntimeScopePlan,
    scopePlanIndexedStatements,
    scopePlanStatementAt,
    scopePlanModulePathForStatement,
    scopePlanRecursiveGroupAt,
    scopePlanIsRecursiveBinding,
    scopePlanIsSelfRecursiveFunction,
    scopePlanBindingNameAt,
    scopePlanBindingIndex,
    scopePlanBindingReferenceAt,
    scopePlanIsHostRecursiveBinding,
    runtimeExprRequiresHost,
    runtimeStatementRequiresHost,
    exprContainsFunctionBranch,
    exprDefinitelyNotFunctionValue,
  )
where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Jazz.Compiler.AST
  ( CaseArm (..),
    CoreNode (coreNodeFacts),
    CorePhase (..),
    Expr (..),
    ImplMethod (..),
    Statement (..),
    statementNode,
  )
import Jazz.Compiler.BuiltinCatalog
  ( BuiltinSymbol (..),
    lookupKernelBuiltinSymbol,
  )
import Jazz.Compiler.CoreIdentity (CoreBinderId, ResolvedNodeFacts (resolvedNodeOwner), ResolvedReference, ResolvedScopeFacts (..), resolvedBinderReference)
import Jazz.Compiler.Name
  ( ResolvedName,
    identifierText,
  )
import Jazz.Compiler.RecursiveBindings
  ( PreparedRecursiveScope,
    exprContainsFunctionBranch,
    preparedRecursiveScopeFacts,
    preparedRecursiveScopeStatements,
  )
import Jazz.Compiler.SemanticFacts (StatementFacts (statementResolution))
import Jazz.Compiler.SourceUnitOwnership (SourceUnitOwner (..))

data RuntimeScopePlan = RuntimeScopePlan
  { runtimeScopePlanIndexedStatements :: [(Int, Statement 'Analyzed)],
    runtimeScopePlanStatementsByIndex :: IntMap (Statement 'Analyzed),
    runtimeScopePlanModulePathsByStatement :: IntMap (Maybe SourceUnitOwner),
    runtimeScopePlanRecursiveGroups :: IntMap [Int],
    runtimeScopePlanSelfRecursiveFunctions :: IntSet,
    runtimeScopePlanBindingNames :: IntMap ResolvedName,
    runtimeScopePlanBindingIndices :: Map.Map CoreBinderId Int,
    runtimeScopePlanHostRecursiveBindings :: IntSet
  }

buildRuntimeScopePlan ::
  PreparedRecursiveScope 'Analyzed ->
  RuntimeScopePlan
buildRuntimeScopePlan preparedScope =
  RuntimeScopePlan
    { runtimeScopePlanIndexedStatements = indexedStatements,
      runtimeScopePlanStatementsByIndex = statementsByIndex,
      runtimeScopePlanModulePathsByStatement = modulePathsByStatement,
      runtimeScopePlanRecursiveGroups = recursiveGroups,
      runtimeScopePlanSelfRecursiveFunctions = selfRecursiveFunctions,
      runtimeScopePlanBindingNames = bindingNames,
      runtimeScopePlanBindingIndices = Map.fromList [(binder, index) | (index, binder) <- Map.toList (resolvedScopeBinderIds lexicalFacts)],
      runtimeScopePlanHostRecursiveBindings = hostRecursiveBindings
    }
  where
    indexedStatements = zip [0 ..] statements
    statementsByIndex = IntMap.fromDistinctAscList indexedStatements
    statements = preparedRecursiveScopeStatements preparedScope
    lexicalFacts = preparedRecursiveScopeFacts preparedScope
    recursiveGroups = IntMap.fromDistinctAscList (Map.toAscList (resolvedScopeRecursiveGroups lexicalFacts))
    selfRecursiveFunctions = IntSet.fromList (Set.toList (resolvedScopeSelfRecursiveFunctions lexicalFacts))
    bindingNames = IntMap.fromDistinctAscList (Map.toAscList (resolvedScopeBindingNames lexicalFacts))
    modulePathsByStatement =
      IntMap.fromDistinctAscList
        ( zip
            [0 :: Int ..]
            [Just (resolvedNodeOwner (statementResolution (coreNodeFacts (statementNode statement)))) | statement <- statements]
        )
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

scopePlanIndexedStatements :: RuntimeScopePlan -> [(Int, Statement 'Analyzed)]
scopePlanIndexedStatements = runtimeScopePlanIndexedStatements

scopePlanStatementAt :: RuntimeScopePlan -> Int -> Maybe (Statement 'Analyzed)
scopePlanStatementAt plan statementIndex =
  IntMap.lookup statementIndex (runtimeScopePlanStatementsByIndex plan)

scopePlanModulePathForStatement :: RuntimeScopePlan -> Int -> Maybe SourceUnitOwner
scopePlanModulePathForStatement plan statementIndex =
  IntMap.findWithDefault Nothing statementIndex (runtimeScopePlanModulePathsByStatement plan)

scopePlanRecursiveGroupAt :: RuntimeScopePlan -> Int -> Maybe [Int]
scopePlanRecursiveGroupAt plan statementIndex =
  IntMap.lookup statementIndex (runtimeScopePlanRecursiveGroups plan)

scopePlanIsRecursiveBinding :: RuntimeScopePlan -> Int -> Bool
scopePlanIsRecursiveBinding plan statementIndex =
  IntMap.member statementIndex (runtimeScopePlanRecursiveGroups plan)

scopePlanIsSelfRecursiveFunction :: RuntimeScopePlan -> Int -> Bool
scopePlanIsSelfRecursiveFunction plan statementIndex =
  IntSet.member statementIndex (runtimeScopePlanSelfRecursiveFunctions plan)

scopePlanBindingNameAt :: RuntimeScopePlan -> Int -> Maybe ResolvedName
scopePlanBindingNameAt plan statementIndex =
  IntMap.lookup statementIndex (runtimeScopePlanBindingNames plan)

scopePlanBindingIndex :: RuntimeScopePlan -> CoreBinderId -> Maybe Int
scopePlanBindingIndex plan binder = Map.lookup binder (runtimeScopePlanBindingIndices plan)

scopePlanIsHostRecursiveBinding :: RuntimeScopePlan -> Int -> Bool
scopePlanIsHostRecursiveBinding plan statementIndex =
  IntSet.member statementIndex (runtimeScopePlanHostRecursiveBindings plan)

runtimeExprRequiresHost :: Expr 'Analyzed -> Bool
runtimeExprRequiresHost expr =
  case expr of
    ELit _ _ -> False
    EVar _ name -> runtimeNameRequiresHost name
    ELambda _ _ bodyExpr -> runtimeExprRequiresHost bodyExpr
    EOperatorValue _ _ -> False
    EList _ elements -> any runtimeExprRequiresHost elements
    ETuple _ elements -> any runtimeExprRequiresHost elements
    EApply _ functionExpr argumentExpr ->
      runtimeExprRequiresHost functionExpr || runtimeExprRequiresHost argumentExpr
    ETypeApplication _ functionExpr _ _ -> runtimeExprRequiresHost functionExpr
    EIf _ conditionExpr thenExpr elseExpr ->
      any runtimeExprRequiresHost [conditionExpr, thenExpr, elseExpr]
    EPatternCase _ scrutineeExpr caseArms ->
      runtimeExprRequiresHost scrutineeExpr || any caseArmRequiresHost caseArms
    EBinary _ _ leftExpr rightExpr ->
      runtimeExprRequiresHost leftExpr || runtimeExprRequiresHost rightExpr
    ESectionLeft _ leftExpr _ -> runtimeExprRequiresHost leftExpr
    ESectionRight _ _ rightExpr -> runtimeExprRequiresHost rightExpr
    EBlock _ statements -> any runtimeStatementRequiresHost statements
  where
    caseArmRequiresHost (CaseArm _ _ maybeGuard bodyExpr) =
      maybe False runtimeExprRequiresHost maybeGuard || runtimeExprRequiresHost bodyExpr

runtimeStatementRequiresHost :: Statement 'Analyzed -> Bool
runtimeStatementRequiresHost statement =
  case statement of
    SLet _ name (EVar _ referencedName)
      | identifierText name == identifierText referencedName,
        runtimeNameRequiresHost name ->
          False
    SLet _ _ valueExpr -> runtimeExprRequiresHost valueExpr
    SImpl _ _ _ methods -> any implMethodRequiresHost methods
    SExpr _ valueExpr -> runtimeExprRequiresHost valueExpr
    _ -> False
  where
    implMethodRequiresHost (ImplMethod _ _ bodyExpr) = runtimeExprRequiresHost bodyExpr

runtimeNameRequiresHost :: ResolvedName -> Bool
runtimeNameRequiresHost name =
  case lookupKernelBuiltinSymbol (identifierText name) of
    Just BuiltinReadTextRaw -> True
    Just BuiltinWriteTextRaw -> True
    Just BuiltinReadStdinRaw -> True
    Just BuiltinWriteStdoutRaw -> True
    Just BuiltinWriteStderrRaw -> True
    Just BuiltinArguments -> True
    Just BuiltinExit -> True
    _ -> False

exprDefinitelyNotFunctionValue :: Expr 'Analyzed -> Bool
exprDefinitelyNotFunctionValue expr =
  case expr of
    ELit {} -> True
    EList {} -> True
    ETuple {} -> True
    EBinary {} -> True
    ETypeApplication _ functionExpr _ _ ->
      exprDefinitelyNotFunctionValue functionExpr
    EIf _ _ thenExpr elseExpr ->
      exprDefinitelyNotFunctionValue thenExpr
        && exprDefinitelyNotFunctionValue elseExpr
    EPatternCase {} -> False
    EBlock _ statements -> scopeDefinitelyNotFunctionValue statements
    _ -> False

scopeDefinitelyNotFunctionValue :: [Statement 'Analyzed] -> Bool
scopeDefinitelyNotFunctionValue statements =
  case reverse statements of
    SExpr _ expr : _ -> exprDefinitelyNotFunctionValue expr
    _ -> False

scopePlanBindingReferenceAt :: RuntimeScopePlan -> Int -> ResolvedReference
scopePlanBindingReferenceAt plan index = case scopePlanStatementAt plan index of
  Just (SLet node _ _) -> resolvedBinderReference (statementResolution (coreNodeFacts node))
  _ -> error "expected a scope binding declaration"
