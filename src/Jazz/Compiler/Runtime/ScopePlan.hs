{-# LANGUAGE OverloadedStrings #-}

-- | AST-only planning shared by pure and host runtime scope execution.
module Jazz.Compiler.Runtime.ScopePlan
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
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Jazz.Compiler.AST
  ( CaseArm (..),
    Expr (..),
    ImplMethod (..),
    NumericType (..),
    SignaturePayload (..),
    SignatureType (..),
    Statement (..)
  )
import Jazz.Compiler.BuiltinCatalog
  ( BuiltinResolutionMode (..),
    BuiltinSymbol (..),
    builtinNamesInMode,
    lookupBuiltinSymbolInMode,
    numericTypeFromName
  )
import Jazz.Compiler.Name
  ( Name,
    identifierText,
    mkIdentifier,
    sourceName
  )
import Jazz.Compiler.RecursiveBindings
  ( collectBindingNames,
    exprContainsFunctionBranch,
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
    recursionOuterBindingNames =
      Set.union
        outerBindingNames
        (Set.map (sourceName . mkIdentifier) (builtinNamesInMode builtinMode))
    recursiveGroupsMap =
      inferRecursiveGroupsOrdered recursionOuterBindingNames indexedStatements
    recursiveGroups = IntMap.fromDistinctAscList (Map.toAscList recursiveGroupsMap)
    selfRecursiveFunctions =
      IntSet.fromList
        (Set.toList (inferSelfRecursiveBindings recursionOuterBindingNames exprContainsFunctionBranch indexedStatements))
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
