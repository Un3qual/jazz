{-# LANGUAGE OverloadedStrings #-}

module Jazz.Compiler.LoweredIR.Lower.Requirements
  ( collectRuntimeRequirements,
    requirementsForManagedLayouts,
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
import Jazz.Compiler.LoweredIR
  ( LoweredCallSignature (..),
    LoweredLayout (..),
    LoweredLayoutShape (..),
    LoweredRepresentation (..),
    LoweredVariantLayout (..),
  )
import Jazz.Compiler.LoweredIR.Lower.Types (RuntimeRequirements (..))
import Jazz.Compiler.LoweredIR.RuntimeServiceCatalog
  ( RuntimeServiceKey (TextEqualService),
    textLayout,
    textLayoutId,
    textOperationService,
  )
import Jazz.Compiler.TypedCore

collectRuntimeRequirements :: TypedModule -> RuntimeRequirements
collectRuntimeRequirements (TypedModule _ _ _ _ moduleInterface _ statements moduleInfo) =
  requirementsForNodeInfo moduleInfo
    <> requirementsForInterface moduleInterface
    <> foldMap requirementsForStatement statements

requiredRuntimeLayouts :: RuntimeRequirements -> [LoweredLayout]
requiredRuntimeLayouts requirements =
  [textLayout | runtimeRequiresTextLayout requirements]

requirementsForManagedLayouts :: [LoweredLayout] -> RuntimeRequirements
requirementsForManagedLayouts = foldMap requirementsForLayout
  where
    requirementsForLayout (LoweredLayout _ shape) =
      case shape of
        LoweredProductLayout fields -> foldMap requirementsForRepresentation fields
        LoweredVariantLayouts variants -> foldMap requirementsForVariant variants
        _ -> mempty
    requirementsForVariant (LoweredVariantLayout _ fields) =
      foldMap requirementsForRepresentation fields
    requirementsForRepresentation representation =
      case representation of
        LoweredManagedReferenceRepresentation layoutId
          | layoutId == textLayoutId -> RuntimeRequirements True Set.empty
        LoweredClosureRepresentation (LoweredCallSignature arguments result) ->
          foldMap requirementsForRepresentation arguments <> requirementsForRepresentation result
        _ -> mempty

requirementsForInterface :: TypedModuleInterface -> RuntimeRequirements
requirementsForInterface (TypedModuleInterface values _ _ _) =
  foldMap requirementsForValueInterface values
  where
    requirementsForValueInterface (TypedValueInterface _ scheme) =
      requirementsForScheme scheme

requirementsForStatement :: TypedStatement -> RuntimeRequirements
requirementsForStatement statement =
  case statement of
    TypedLetStatement _ _ _ scheme expression ->
      requirementsForScheme scheme
        <> requirementsForExpression expression
    TypedSignatureStatement _ _ _ scheme -> requirementsForScheme scheme
    TypedExpressionStatement _ expression -> requirementsForExpression expression
    TypedDataStatement {} -> mempty
    TypedClassStatement {} -> mempty
    TypedImplStatement {} -> mempty

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
      requirementsForRecipe result <> foldMap requirementsForRecipe arguments
    TypedManagedProductRecipe fields -> foldMap requirementsForRecipe fields
    TypedManagedListRecipe element -> requirementsForRecipe element
    _ -> mempty

requirementsForExpression :: TypedExpr -> RuntimeRequirements
requirementsForExpression expression =
  requirementsForNodeInfo (typedExpressionInfo expression)
    <> requirementsForSemanticExpression expression
    <> requirementsForExpressionChildren expression

requirementsForExpressionChildren :: TypedExpr -> RuntimeRequirements
requirementsForExpressionChildren expression =
  case expression of
    TypedLiteralExpr {} -> mempty
    TypedVariableExpr {} -> mempty
    TypedLambdaExpr _ _ _ body -> requirementsForExpression body
    TypedOperatorValueExpr {} -> mempty
    TypedListExpr _ values -> foldMap requirementsForExpression values
    TypedTupleExpr _ values -> foldMap requirementsForExpression values
    TypedApplyExpr _ function argument ->
      requirementsForExpression function <> requirementsForExpression argument
    TypedTypeApplicationExpr _ function _ _ -> requirementsForExpression function
    TypedIfExpr _ condition consequent alternative ->
      foldMap requirementsForExpression [condition, consequent, alternative]
    TypedPatternCaseExpr _ scrutinee arms ->
      requirementsForExpression scrutinee <> foldMap requirementsForArm arms
    TypedBinaryExpr _ _ left right ->
      requirementsForExpression left <> requirementsForExpression right
    TypedLeftSectionExpr _ left _ -> requirementsForExpression left
    TypedRightSectionExpr _ _ right -> requirementsForExpression right
    TypedBlockExpr _ blockStatements -> foldMap requirementsForStatement blockStatements

requirementsForSemanticExpression :: TypedExpr -> RuntimeRequirements
requirementsForSemanticExpression expression =
  case textEqualityOperation expression of
    Just _ -> runtimeServiceRequirement TextEqualService
    Nothing ->
      case textRuntimeServiceApplication expression of
        Just serviceKey -> runtimeServiceRequirement serviceKey
        Nothing -> mempty

runtimeServiceRequirement :: RuntimeServiceKey -> RuntimeRequirements
runtimeServiceRequirement serviceKey =
  RuntimeRequirements False (Set.singleton serviceKey)

requirementsForArm :: TypedCaseArm -> RuntimeRequirements
requirementsForArm (TypedCaseArm patternValue guard result) =
  requirementsForPattern patternValue
    <> foldMap requirementsForExpression guard
    <> requirementsForExpression result

requirementsForPattern :: TypedPattern -> RuntimeRequirements
requirementsForPattern patternValue =
  requirementsForNodeInfo (patternInfo patternValue)
    <> foldMap requirementsForPattern (patternChildren patternValue)
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
