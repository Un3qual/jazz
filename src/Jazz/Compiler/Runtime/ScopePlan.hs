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
    exprDefinitelyNotFunctionValue,
  )
where

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
    CorePhase (..),
    Expr (..),
    ImplMethod (..),
    SignaturePayload,
    Statement (..),
  )
import Jazz.Compiler.BuiltinCatalog
  ( BuiltinResolutionMode (..),
    BuiltinSymbol (..),
    builtinNamesInMode,
    lookupBuiltinSymbolInMode,
    numericTypeFromName,
  )
import Jazz.Compiler.ModuleIdentity (ModulePath)
import Jazz.Compiler.Name
  ( NameNamespace (..),
    ResolvedName,
    identifierText,
    mkIdentifier,
    resolvedAmbientName,
  )
import Jazz.Compiler.RecursiveBindings
  ( buildRecursiveScopeFacts,
    exprContainsFunctionBranch,
    inferSelfRecursiveBindings,
    recursiveScopeBindingNames,
    recursiveScopeGroups,
  )
import Jazz.Compiler.SourceUnitOwnership (sourceUnitStatementRuntimePaths)
import Jazz.Compiler.TypeRepresentation
  ( NumericType (..),
    pattern ConstrainedSignature,
    pattern SignatureType,
    pattern TypeFloat,
    pattern TypeInt,
    pattern TypeName,
    pattern TypeNumeric,
  )

data RuntimeScopePlan = RuntimeScopePlan
  { runtimeScopePlanIndexedStatements :: [(Int, Statement 'Analyzed)],
    runtimeScopePlanStatementsByIndex :: IntMap (Statement 'Analyzed),
    runtimeScopePlanModulePathsByStatement :: IntMap (Maybe [Text]),
    runtimeScopePlanRecursiveGroups :: IntMap [Int],
    runtimeScopePlanSelfRecursiveFunctions :: IntSet,
    runtimeScopePlanBindingNames :: IntMap ResolvedName,
    runtimeScopePlanHostRecursiveBindings :: IntSet
  }

buildRuntimeScopePlan ::
  ModulePath ->
  Set Int ->
  Maybe [Text] ->
  BuiltinResolutionMode ->
  Set ResolvedName ->
  [Statement 'Analyzed] ->
  RuntimeScopePlan
buildRuntimeScopePlan preludePath preludeStatementIndices initialModulePath builtinMode outerBindingNames statements =
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
        (Set.map (resolvedAmbientName ValueNamespace . mkIdentifier) (builtinNamesInMode builtinMode))
    recursiveScopeFactsValue =
      buildRecursiveScopeFacts
        recursionOuterBindingNames
        indexedStatements
    recursiveGroupsMap = recursiveScopeGroups recursiveScopeFactsValue
    recursiveGroups = IntMap.fromDistinctAscList (Map.toAscList recursiveGroupsMap)
    selfRecursiveFunctions =
      IntSet.fromList
        (Set.toList (inferSelfRecursiveBindings recursionOuterBindingNames exprContainsFunctionBranch indexedStatements))
    bindingNames =
      IntMap.fromDistinctAscList
        (Map.toAscList (recursiveScopeBindingNames recursiveScopeFactsValue))
    modulePathsByStatement =
      IntMap.fromDistinctAscList
        ( zip
            [0 :: Int ..]
            (sourceUnitStatementRuntimePaths preludePath preludeStatementIndices initialModulePath statements)
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

scopePlanModulePathForStatement :: RuntimeScopePlan -> Int -> Maybe [Text]
scopePlanModulePathForStatement plan statementIndex =
  IntMap.findWithDefault Nothing statementIndex (runtimeScopePlanModulePathsByStatement plan)

runtimeModulePathAfterStatements :: Maybe [Text] -> [Statement 'Analyzed] -> Maybe [Text]
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

scopePlanBindingNameAt :: RuntimeScopePlan -> Int -> Maybe ResolvedName
scopePlanBindingNameAt plan statementIndex =
  IntMap.lookup statementIndex (runtimeScopePlanBindingNames plan)

scopePlanIsHostRecursiveBinding :: RuntimeScopePlan -> Int -> Bool
scopePlanIsHostRecursiveBinding plan statementIndex =
  IntSet.member statementIndex (runtimeScopePlanHostRecursiveBindings plan)

scopePlanPreviousSignaturePayload :: RuntimeScopePlan -> Int -> ResolvedName -> Maybe (SignaturePayload 'Resolved)
scopePlanPreviousSignaturePayload plan statementIndex bindingName =
  case scopePlanStatementAt plan (statementIndex - 1) of
    Just (SSignature _ signatureName signaturePayload)
      | identifierText signatureName == identifierText bindingName ->
          Just signaturePayload
    _ -> Nothing

runtimeSignatureNumericTarget :: SignaturePayload 'Resolved -> Maybe NumericType
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
  case lookupBuiltinSymbolInMode ResolveKernelOnly (identifierText name) of
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
