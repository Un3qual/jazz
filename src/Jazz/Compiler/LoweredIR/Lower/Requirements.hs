{-# LANGUAGE OverloadedStrings #-}

module Jazz.Compiler.LoweredIR.Lower.Requirements
  ( collectRuntimeRequirements,
    requiredRuntimeLayouts,
    textEqualityOperation,
    textRuntimeServiceApplication,
  )
where

import qualified Data.Set as Set
import Jazz.Compiler.BuiltinCatalog
  ( BuiltinResolutionMode (ResolveKernelOnly),
    builtinSymbolArity,
    lookupBuiltinSymbolInMode,
  )
import Jazz.Compiler.LoweredIR (LoweredLayout)
import Jazz.Compiler.LoweredIR.Lower.Types (RuntimeRequirements (..))
import Jazz.Compiler.LoweredIR.RuntimeServiceCatalog
  ( RuntimeServiceKey (TextEqualService),
    textLayout,
    textOperationService,
  )
import Jazz.Compiler.TypedCore

collectRuntimeRequirements :: TypedModule -> RuntimeRequirements
collectRuntimeRequirements (TypedModule _ _ _ _ moduleInterface _ statements moduleInfo) =
  foldl'
    mergeRuntimeRequirements
    (requirementsForNodeInfo moduleInfo)
    ( requirementsForInterface moduleInterface
        : map requirementsForStatement statements
    )

requiredRuntimeLayouts :: RuntimeRequirements -> [LoweredLayout]
requiredRuntimeLayouts requirements =
  [textLayout | runtimeRequiresTextLayout requirements]

emptyRuntimeRequirements :: RuntimeRequirements
emptyRuntimeRequirements = RuntimeRequirements False Set.empty

mergeRuntimeRequirements :: RuntimeRequirements -> RuntimeRequirements -> RuntimeRequirements
mergeRuntimeRequirements left right =
  RuntimeRequirements
    { runtimeRequiresTextLayout =
        runtimeRequiresTextLayout left || runtimeRequiresTextLayout right,
      runtimeRequiredServices =
        Set.union
          (runtimeRequiredServices left)
          (runtimeRequiredServices right)
    }

requirementsForInterface :: TypedModuleInterface -> RuntimeRequirements
requirementsForInterface (TypedModuleInterface values _ _ _) =
  foldl'
    mergeRuntimeRequirements
    emptyRuntimeRequirements
    [requirementsForScheme scheme | TypedValueInterface _ scheme <- values]

requirementsForStatement :: TypedStatement -> RuntimeRequirements
requirementsForStatement statement =
  case statement of
    TypedLetStatement _ _ _ scheme expression ->
      requirementsForScheme scheme
        `mergeRuntimeRequirements` requirementsForExpression expression
    TypedSignatureStatement _ _ _ scheme -> requirementsForScheme scheme
    TypedExpressionStatement _ expression -> requirementsForExpression expression
    TypedDataStatement {} -> emptyRuntimeRequirements
    TypedClassStatement {} -> emptyRuntimeRequirements
    TypedImplStatement {} -> emptyRuntimeRequirements

requirementsForScheme :: TypedScheme -> RuntimeRequirements
requirementsForScheme (TypedScheme _ _ _ _ _ recipe _) =
  requirementsForRecipe recipe

requirementsForNodeInfo :: TypedNodeInfo -> RuntimeRequirements
requirementsForNodeInfo info = requirementsForRecipe (typedNodeRecipe info)

requirementsForRecipe :: TypedRepresentationRecipe -> RuntimeRequirements
requirementsForRecipe recipe =
  case recipe of
    TypedManagedTextRecipe -> RuntimeRequirements True Set.empty
    TypedClosureRecipe arguments result ->
      foldl'
        mergeRuntimeRequirements
        (requirementsForRecipe result)
        (map requirementsForRecipe arguments)
    _ -> emptyRuntimeRequirements

requirementsForExpression :: TypedExpr -> RuntimeRequirements
requirementsForExpression expression =
  foldl'
    mergeRuntimeRequirements
    ( requirementsForNodeInfo (typedExpressionInfo expression)
        `mergeRuntimeRequirements` requirementsForSemanticExpression expression
    )
    ( case expression of
        TypedLiteralExpr {} -> []
        TypedVariableExpr {} -> []
        TypedLambdaExpr _ _ _ body -> [requirementsForExpression body]
        TypedOperatorValueExpr {} -> []
        TypedListExpr _ values -> map requirementsForExpression values
        TypedTupleExpr _ values -> map requirementsForExpression values
        TypedApplyExpr _ function argument -> map requirementsForExpression [function, argument]
        TypedTypeApplicationExpr _ function _ _ -> [requirementsForExpression function]
        TypedIfExpr _ condition consequent alternative -> map requirementsForExpression [condition, consequent, alternative]
        TypedPatternCaseExpr _ scrutinee arms ->
          requirementsForExpression scrutinee : map requirementsForArm arms
        TypedBinaryExpr _ _ left right -> map requirementsForExpression [left, right]
        TypedLeftSectionExpr _ left _ -> [requirementsForExpression left]
        TypedRightSectionExpr _ _ right -> [requirementsForExpression right]
        TypedBlockExpr _ blockStatements -> map requirementsForStatement blockStatements
    )

requirementsForSemanticExpression :: TypedExpr -> RuntimeRequirements
requirementsForSemanticExpression expression =
  case textEqualityOperation expression of
    Just _ -> runtimeServiceRequirement TextEqualService
    Nothing ->
      case textRuntimeServiceApplication expression of
        Just serviceKey -> runtimeServiceRequirement serviceKey
        Nothing -> emptyRuntimeRequirements

runtimeServiceRequirement :: RuntimeServiceKey -> RuntimeRequirements
runtimeServiceRequirement serviceKey =
  RuntimeRequirements False (Set.singleton serviceKey)

requirementsForArm :: TypedCaseArm -> RuntimeRequirements
requirementsForArm (TypedCaseArm patternValue guard result) =
  foldl'
    mergeRuntimeRequirements
    (requirementsForPattern patternValue)
    (requirementsForExpression result : maybe [] ((: []) . requirementsForExpression) guard)

requirementsForPattern :: TypedPattern -> RuntimeRequirements
requirementsForPattern patternValue =
  foldl'
    mergeRuntimeRequirements
    (requirementsForNodeInfo (patternInfo patternValue))
    (map requirementsForPattern (patternChildren patternValue))
  where
    patternInfo patternNode =
      case patternNode of
        TypedWildcardPattern info -> info
        TypedVariablePattern info _ _ -> info
        TypedLiteralPattern info _ -> info
        TypedConstructorPattern info _ _ -> info
        TypedListPattern info _ -> info
        TypedConsListPattern info _ _ -> info
        TypedTuplePattern info _ -> info
        TypedAsPattern info _ _ _ -> info
        TypedOrPattern info _ -> info
    patternChildren patternNode =
      case patternNode of
        TypedConstructorPattern _ _ children -> children
        TypedListPattern _ children -> children
        TypedConsListPattern _ headPattern tailPattern -> [headPattern, tailPattern]
        TypedTuplePattern _ children -> children
        TypedAsPattern _ _ _ nested -> [nested]
        TypedOrPattern _ alternatives -> alternatives
        _ -> []

textEqualityOperation :: TypedExpr -> Maybe Bool
textEqualityOperation expression =
  case expression of
    TypedBinaryExpr info (TypedBuiltinOperator operator) left right
      | typedNodeRecipe info == TypedBoolRecipe,
        typedNodeRecipe (typedExpressionInfo left) == TypedManagedTextRecipe,
        typedNodeRecipe (typedExpressionInfo right) == TypedManagedTextRecipe ->
          case operator of
            "==" -> Just False
            "!=" -> Just True
            _ -> Nothing
    _ -> Nothing

textRuntimeServiceApplication :: TypedExpr -> Maybe RuntimeServiceKey
textRuntimeServiceApplication expression = do
  let (callee, arguments) = applicationSpine expression
  (identifier, binderReference) <-
    case callee of
      TypedVariableExpr _ (TypedBuiltinName name) binder -> Just (name, binder)
      _ -> Nothing
  case binderReference of
    Just _ -> Nothing
    Nothing -> do
      symbol <- lookupBuiltinSymbolInMode ResolveKernelOnly identifier
      serviceKey <- textOperationService symbol
      if length arguments == builtinSymbolArity symbol
        then Just serviceKey
        else Nothing

applicationSpine :: TypedExpr -> (TypedExpr, [TypedExpr])
applicationSpine = go []
  where
    go arguments expression =
      case expression of
        TypedApplyExpr _ function argument ->
          go (argument : arguments) function
        _ -> (expression, arguments)
