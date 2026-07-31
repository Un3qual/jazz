{-# LANGUAGE OverloadedStrings #-}

-- | Builtin operator typing rules, isolated from expression orchestration.
module JazzNext.Compiler.TypeInference.Operator
  ( applyOperatorAliasSchemeConstraints,
    binaryNumericPromotionType,
    builtinSectionOperatorSymbol,
    hasOperatorRule,
    inferBinaryType,
    inferSectionLeftType,
    inferSectionRightType,
    instantiateOperatorType
  ) where

import Control.Applicative ((<|>))
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import qualified Data.Set as Set
import Data.Text (Text)
import JazzNext.Compiler.AST
  ( Expr (..),
    Literal (..),
    NumericType (..)
  )
import JazzNext.Compiler.BuiltinCatalog
  ( numericTypeFloatIntegerBounds,
    numericTypeIsIntegral
  )
import JazzNext.Compiler.TypeInference.Capabilities
  ( addInferredEqualityClassConstraintIfVisible,
    applyTypeSchemePrimitiveConstraints,
    capabilityFactsFromState,
    defaultLiteralTypes,
    deferExplicitConstraintsWithFacts,
    mergeCapabilityFacts,
    structuralRuntimeEqualityType
  )
import JazzNext.Compiler.TypeInference.Diagnostics
  ( addTypeError,
    mkApplyTypeError,
    mkBinaryTypeError,
    mkNumericBinaryTypeError,
    mkNumericSectionOperandTypeError,
    mkStrictEqualityTypeError,
    mkStrictEqualityUnsupportedTypeError,
    mkUnsupportedSectionOperatorError
  )
import JazzNext.Compiler.TypeInference.State
  ( InferState,
    inferRigidTypeVars
  )
import JazzNext.Compiler.TypeInference.Solver
  ( addNumericTypeVarConstraint,
    addStrictEqualityTypeVarConstraint,
    combineIntegerLiteralRanges,
    constrainNumericOperatorType,
    freshTypeVar,
    freshTypeVariable,
    integerLiteralRangeBounds,
    integerLiteralRangeFitsNumericType,
    resolveType,
    supportsRuntimeEqualityType,
    typeSatisfiesNumericConstraint,
    unifyTypes
  )
import JazzNext.Compiler.TypeInference.TypeOps
  ( instantiateTypeSchemeConstraint,
    instantiateTypeSchemePrimitiveConstraint
  )
import JazzNext.Compiler.TypeInference.Types
  ( ExpressionType (..),
    IntegerLiteralRange (..),
    NumericConstraint (..),
    TypeScheme (..)
  )

data OperatorRule
  = NumericRule NumericRuleResult
  | StrictEqualityRule
  | ApplicationRule

data NumericRuleResult
  = NumericSameTypeResult
  | NumericBoolResult

lookupOperatorRule :: Text -> Maybe OperatorRule
lookupOperatorRule operatorSymbol =
  case operatorSymbol of
    "+" -> Just (NumericRule NumericSameTypeResult)
    "-" -> Just (NumericRule NumericSameTypeResult)
    "*" -> Just (NumericRule NumericSameTypeResult)
    "/" -> Just (NumericRule NumericSameTypeResult)
    "<" -> Just (NumericRule NumericBoolResult)
    "<=" -> Just (NumericRule NumericBoolResult)
    ">" -> Just (NumericRule NumericBoolResult)
    ">=" -> Just (NumericRule NumericBoolResult)
    "==" -> Just StrictEqualityRule
    "!=" -> Just StrictEqualityRule
    "$" -> Just ApplicationRule
    _ -> Nothing

hasOperatorRule :: Text -> Bool
hasOperatorRule = isJust . lookupOperatorRule

builtinSectionOperatorSymbol :: Text -> Bool
builtinSectionOperatorSymbol operatorSymbol =
  case lookupOperatorRule operatorSymbol of
    Just (NumericRule _) -> True
    Just StrictEqualityRule -> True
    _ -> False

applyOperatorAliasSchemeConstraints :: Text -> TypeScheme -> ExpressionType -> ExpressionType -> InferState -> InferState
applyOperatorAliasSchemeConstraints operatorSymbol typeScheme leftType rightType state =
  case lookupOperatorRule operatorSymbol of
    Just StrictEqualityRule ->
      case operatorAliasEqualityConstraintTarget state leftType rightType of
        Just targetType -> instantiateOperatorAliasSchemeConstraints typeScheme targetType state
        Nothing -> state
    Just (NumericRule _) ->
      -- Numeric operator alias schemes only carry the primitive numeric operand
      -- constraint that inferBinaryType has already applied here. User-written
      -- constrained signatures are stored as ordinary schemes, not operator
      -- aliases, so there are no explicit capability facts to apply.
      state
    _ -> state

operatorAliasEqualityConstraintTarget :: InferState -> ExpressionType -> ExpressionType -> Maybe ExpressionType
operatorAliasEqualityConstraintTarget state leftType rightType
  | isJust (typedIntegerFloat64PromotionOperand state leftType rightType) = Nothing
  | resolvedLeftType == resolvedRightType,
    not (structuralRuntimeEqualityType state resolvedLeftType) =
      Just resolvedLeftType
  | otherwise = Nothing
  where
    resolvedLeftType = defaultLiteralTypes (resolveType state leftType)
    resolvedRightType = defaultLiteralTypes (resolveType state rightType)

instantiateOperatorAliasSchemeConstraints :: TypeScheme -> ExpressionType -> InferState -> InferState
instantiateOperatorAliasSchemeConstraints typeScheme targetType state =
  let replacements =
        Map.fromList
          [ (typeVar, targetType)
            | typeVar <- Set.toList quantifiedVariables
          ]
      instantiatedConstraints =
        map (instantiateTypeSchemeConstraint replacements) explicitConstraints
      instantiatedPrimitiveConstraints =
        map (instantiateTypeSchemePrimitiveConstraint replacements) primitiveConstraints
      stateWithPrimitiveConstraints =
        applyTypeSchemePrimitiveConstraints instantiatedPrimitiveConstraints state
   in deferExplicitConstraintsWithFacts
        (mergeCapabilityFacts definingFacts (capabilityFactsFromState state))
        definingFacts
        instantiatedConstraints
        stateWithPrimitiveConstraints
  where
    quantifiedVariables = schemeQuantifiedVariables typeScheme
    explicitConstraints = schemeClassConstraints typeScheme
    primitiveConstraints = schemePrimitiveConstraints typeScheme
    definingFacts = schemeDefiningCapabilities typeScheme

inferBinaryType ::
  Text ->
  Expr ->
  Expr ->
  ExpressionType ->
  ExpressionType ->
  InferState ->
  (Maybe ExpressionType, InferState)
inferBinaryType operatorSymbol leftExpr rightExpr leftType rightType state =
  case lookupOperatorRule operatorSymbol of
    Just (NumericRule resultType) ->
      applyNumericBinaryRule operatorSymbol resultType leftExpr rightExpr leftType rightType state
    Just StrictEqualityRule ->
      applyStrictEqualityBinaryRule operatorSymbol leftExpr rightExpr leftType rightType state
    Just ApplicationRule ->
      applyApplicationBinaryRule leftType rightType state
    Nothing ->
      ( Nothing,
        addTypeError
          state
          ( mkBinaryTypeError
              operatorSymbol
              (resolveType state leftType)
              (resolveType state rightType)
          )
      )

-- | Report the implicit Float64 operand promotion selected by the ordinary
-- operator rules. Consumers that cannot represent the conversion can reject
-- it explicitly instead of constructing a heterogeneous binary node.
binaryNumericPromotionType ::
  Text ->
  Expr ->
  Expr ->
  ExpressionType ->
  ExpressionType ->
  InferState ->
  Maybe ExpressionType
binaryNumericPromotionType operatorSymbol leftExpr rightExpr leftType rightType state =
  case lookupOperatorRule operatorSymbol of
    Just (NumericRule _) -> promotedType
    Just StrictEqualityRule -> promotedType
    _ -> Nothing
  where
    promotedType =
      fst
        <$> directIntegerFloat64NumericOperand
          NumericSameTypeResult
          state
          leftExpr
          rightExpr
          leftType
          rightType

applyNumericBinaryRule ::
  Text ->
  NumericRuleResult ->
  Expr ->
  Expr ->
  ExpressionType ->
  ExpressionType ->
  InferState ->
  (Maybe ExpressionType, InferState)
applyNumericBinaryRule operatorSymbol resultRule leftExpr rightExpr leftType rightType state =
  case directIntegerFloat64NumericOperand resultRule state leftExpr rightExpr leftType rightType of
    Just (resolvedOperandType, stateAfterFloat64LiteralOperand) ->
      constrainNumericOperand resolvedOperandType stateAfterFloat64LiteralOperand
    Nothing ->
      case rigidNumericOperand of
        Just rigidOperandType ->
          constrainNumericOperand rigidOperandType state
        Nothing ->
          case unifyTypes leftType rightType state of
            Just stateAfterUnify ->
              let resolvedOperandType = numericBinaryOperandType operatorSymbol resultRule stateAfterUnify leftType rightType
               in constrainNumericOperand resolvedOperandType stateAfterUnify
            Nothing -> numericOperandError state
  where
    rigidNumericOperand =
      case (resolveType state leftType, resolveType state rightType) of
        (rigidType@(TVarType typeVar), concreteType)
          | Set.member typeVar (inferRigidTypeVars state),
            typeSatisfiesNumericConstraint (numericRuleConstraint resultRule) concreteType ->
              Just rigidType
        (concreteType, rigidType@(TVarType typeVar))
          | Set.member typeVar (inferRigidTypeVars state),
            typeSatisfiesNumericConstraint (numericRuleConstraint resultRule) concreteType ->
              Just rigidType
        _ -> Nothing

    constrainNumericOperand resolvedOperandType operandState =
      case constrainNumericOperatorType (numericRuleConstraint resultRule) resolvedOperandType operandState of
        Just stateAfterNumericConstraint ->
          (Just (numericRuleResultType resultRule resolvedOperandType), stateAfterNumericConstraint)
        Nothing ->
          numericOperandError operandState
    numericOperandError errState =
      ( Nothing,
        addTypeError
          errState
          ( mkNumericBinaryTypeError
              operatorSymbol
              (resolveType errState leftType)
              (resolveType errState rightType)
        )
      )

directIntegerFloat64NumericOperand :: NumericRuleResult -> InferState -> Expr -> Expr -> ExpressionType -> ExpressionType -> Maybe (ExpressionType, InferState)
directIntegerFloat64NumericOperand _resultRule state leftExpr rightExpr leftType rightType =
  integerLiteralFloat64PromotionOperand state leftExpr rightExpr leftType rightType
    <|> case typedIntegerFloat64PromotionOperand state leftType rightType of
      Just promotedType -> Just (promotedType, state)
      Nothing -> Nothing

numericRuleResultType :: NumericRuleResult -> ExpressionType -> ExpressionType
numericRuleResultType resultRule operandType =
  case resultRule of
    NumericSameTypeResult -> operandType
    NumericBoolResult -> TBoolType

numericRuleConstraint :: NumericRuleResult -> NumericConstraint
numericRuleConstraint resultRule =
  case resultRule of
    NumericSameTypeResult -> RuntimeArithmeticNumericConstraint
    NumericBoolResult -> RuntimeComparisonNumericConstraint

integerLiteralFloat64PromotionOperand :: InferState -> Expr -> Expr -> ExpressionType -> ExpressionType -> Maybe (ExpressionType, InferState)
integerLiteralFloat64PromotionOperand state leftExpr rightExpr leftType rightType =
  case (resolveType state leftType, resolveType state rightType) of
    (TIntegerLiteralType literalRange, floatType)
      | exprIsIntegerLiteral leftExpr,
        integerLiteralRangeFitsFloat64 literalRange,
        expressionTypeIsFloat64Domain floatType ->
          Just (floatType, state)
    (floatType, TIntegerLiteralType literalRange)
      | exprIsIntegerLiteral rightExpr,
        integerLiteralRangeFitsFloat64 literalRange,
        expressionTypeIsFloat64Domain floatType ->
          Just (floatType, state)
    _ -> Nothing

exprIsIntegerLiteral :: Expr -> Bool
exprIsIntegerLiteral expr =
  case expr of
    ELit (LInt _) -> True
    _ -> False

expressionTypeIsFloat64Domain :: ExpressionType -> Bool
expressionTypeIsFloat64Domain expressionType =
  case expressionType of
    TFloatType -> True
    TNumericType NumericFloat64 -> True
    _ -> False

expressionTypeIsConcreteIntegral :: ExpressionType -> Bool
expressionTypeIsConcreteIntegral expressionType =
  case expressionType of
    TIntType -> True
    TNumericType numericType -> numericTypeIsIntegral numericType
    _ -> False

typedIntegerFloat64PromotionOperand :: InferState -> ExpressionType -> ExpressionType -> Maybe ExpressionType
typedIntegerFloat64PromotionOperand state leftType rightType =
  case (resolveType state leftType, resolveType state rightType) of
    (integralType, floatType)
      | expressionTypeIsConcreteIntegral integralType,
        expressionTypeIsFloat64Domain floatType ->
          Just floatType
    (floatType, integralType)
      | expressionTypeIsFloat64Domain floatType,
        expressionTypeIsConcreteIntegral integralType ->
          Just floatType
    _ -> Nothing

integerLiteralRangeFitsFloat64 :: IntegerLiteralRange -> Bool
integerLiteralRangeFitsFloat64 literalRange =
  case numericTypeFloatIntegerBounds NumericFloat64 of
    Just (lowerBound, upperBound) ->
      let (literalMin, literalMax) = integerLiteralRangeBounds literalRange
       in literalMin >= lowerBound && literalMax <= upperBound
    Nothing -> False

numericBinaryOperandType ::
  Text ->
  NumericRuleResult ->
  InferState ->
  ExpressionType ->
  ExpressionType ->
  ExpressionType
numericBinaryOperandType operatorSymbol resultRule state leftType rightType =
  case (resolveType state leftType, resolveType state rightType) of
    (TIntegerLiteralType leftRange, TIntegerLiteralType rightRange) ->
      TIntegerLiteralType (numericLiteralBinaryRange operatorSymbol resultRule leftRange rightRange)
    (TIntegerLiteralType literalRange, numericType@(TNumericType concreteNumericType))
      | integerLiteralRangeFitsNumericType literalRange concreteNumericType -> numericType
    (numericType@(TNumericType concreteNumericType), TIntegerLiteralType literalRange)
      | integerLiteralRangeFitsNumericType literalRange concreteNumericType -> numericType
    (TIntegerLiteralType {}, TIntType) -> TIntType
    (TIntType, TIntegerLiteralType {}) -> TIntType
    (resolvedLeftType, _) -> resolvedLeftType

applyApplicationBinaryRule ::
  ExpressionType ->
  ExpressionType ->
  InferState ->
  (Maybe ExpressionType, InferState)
applyApplicationBinaryRule functionType argumentType state =
  let (resultTypeVar, stateAfterResultVar) = freshTypeVar state
   in case unifyTypes functionType (TFunctionType argumentType resultTypeVar) stateAfterResultVar of
        Just unifiedState ->
          (Just (resolveType unifiedState resultTypeVar), unifiedState)
        Nothing ->
          ( Nothing,
            addTypeError
              stateAfterResultVar
              ( mkApplyTypeError
                  (resolveType stateAfterResultVar functionType)
                  (resolveType stateAfterResultVar argumentType)
              )
          )

applyStrictEqualityBinaryRule ::
  Text ->
  Expr ->
  Expr ->
  ExpressionType ->
  ExpressionType ->
  InferState ->
  (Maybe ExpressionType, InferState)
applyStrictEqualityBinaryRule operatorSymbol leftExpr rightExpr leftType rightType state =
  case integerLiteralFloat64PromotionOperand state leftExpr rightExpr leftType rightType of
    Just _ ->
      (Just TBoolType, state)
    Nothing ->
      case typedIntegerFloat64PromotionOperand state leftType rightType of
        Just _ ->
          (Just TBoolType, state)
        Nothing ->
          strictEqualityFallback
  where
    strictEqualityFallback =
      case unifyTypes leftType rightType state of
        Just unifiedState ->
          let resolvedType = resolveType unifiedState leftType
           in
            case resolvedType of
              TVarType typeVar ->
                ( Just TBoolType,
                  addInferredEqualityClassConstraintIfVisible
                    (TVarType typeVar)
                    (addStrictEqualityTypeVarConstraint typeVar unifiedState)
                )
              _
                | supportsRuntimeEqualityType unifiedState resolvedType ->
                    (Just TBoolType, unifiedState)
                | otherwise ->
                    ( Nothing,
                      addTypeError
                        unifiedState
                        (mkStrictEqualityUnsupportedTypeError operatorSymbol resolvedType)
                    )
        Nothing ->
          ( Nothing,
            addTypeError
              state
              ( mkStrictEqualityTypeError
                  operatorSymbol
                  (resolveType state leftType)
                  (resolveType state rightType)
              )
          )

inferSectionLeftType ::
  Text ->
  ExpressionType ->
  InferState ->
  (Maybe ExpressionType, InferState)
inferSectionLeftType operatorSymbol leftType state =
  case lookupOperatorRule operatorSymbol of
    Just (NumericRule resultType) ->
      applyNumericSectionLeftRule operatorSymbol resultType leftType state
    Just StrictEqualityRule ->
      applyStrictEqualitySectionLeftRule operatorSymbol leftType state
    _ ->
      ( Nothing,
        addTypeError
          state
          (mkUnsupportedSectionOperatorError operatorSymbol)
      )

applyNumericSectionLeftRule ::
  Text ->
  NumericRuleResult ->
  ExpressionType ->
  InferState ->
  (Maybe ExpressionType, InferState)
applyNumericSectionLeftRule operatorSymbol resultRule leftType state =
  let resolvedLeftType = resolveType state leftType
   in case constrainNumericOperatorType (numericRuleConstraint resultRule) resolvedLeftType state of
        Just stateAfterNumericConstraint ->
          let (rightType, stateAfterSectionType) =
                numericSectionCounterpartType resolvedLeftType stateAfterNumericConstraint
           in
            ( Just
                ( TFunctionType
                    rightType
                    (numericRuleResultType resultRule rightType)
                ),
              stateAfterSectionType
            )
        Nothing ->
          ( Nothing,
            addTypeError
              state
              (mkNumericSectionOperandTypeError operatorSymbol (resolveType state leftType))
          )

applyStrictEqualitySectionLeftRule ::
  Text ->
  ExpressionType ->
  InferState ->
  (Maybe ExpressionType, InferState)
applyStrictEqualitySectionLeftRule operatorSymbol leftType state =
  let resolvedLeftType = resolveType state leftType
   in
    case resolvedLeftType of
      TVarType typeVar ->
        ( Just (TFunctionType resolvedLeftType TBoolType),
          addInferredEqualityClassConstraintIfVisible
            resolvedLeftType
            (addStrictEqualityTypeVarConstraint typeVar state)
        )
      _
        | supportsRuntimeEqualityType state resolvedLeftType ->
            (Just (TFunctionType resolvedLeftType TBoolType), state)
        | otherwise ->
            ( Nothing,
              addTypeError
                state
                (mkStrictEqualityUnsupportedTypeError operatorSymbol resolvedLeftType)
            )

inferSectionRightType ::
  Text ->
  ExpressionType ->
  InferState ->
  (Maybe ExpressionType, InferState)
inferSectionRightType operatorSymbol rightType state =
  case lookupOperatorRule operatorSymbol of
    Just (NumericRule resultType) ->
      applyNumericSectionRightRule operatorSymbol resultType rightType state
    Just StrictEqualityRule ->
      applyStrictEqualitySectionRightRule operatorSymbol rightType state
    _ ->
      ( Nothing,
        addTypeError
          state
          (mkUnsupportedSectionOperatorError operatorSymbol)
      )

applyNumericSectionRightRule ::
  Text ->
  NumericRuleResult ->
  ExpressionType ->
  InferState ->
  (Maybe ExpressionType, InferState)
applyNumericSectionRightRule operatorSymbol resultRule rightType state =
  let resolvedRightType = resolveType state rightType
   in case constrainNumericOperatorType (numericRuleConstraint resultRule) resolvedRightType state of
        Just stateAfterNumericConstraint ->
          let (leftType, stateAfterSectionType) =
                numericSectionCounterpartType resolvedRightType stateAfterNumericConstraint
           in
            ( Just
                ( TFunctionType
                    leftType
                    (numericRuleResultType resultRule leftType)
                ),
              stateAfterSectionType
            )
        Nothing ->
          ( Nothing,
            addTypeError
              state
              (mkNumericSectionOperandTypeError operatorSymbol (resolveType state rightType))
          )

applyStrictEqualitySectionRightRule ::
  Text ->
  ExpressionType ->
  InferState ->
  (Maybe ExpressionType, InferState)
applyStrictEqualitySectionRightRule operatorSymbol rightType state =
  let resolvedRightType = resolveType state rightType
   in
    case resolvedRightType of
      TVarType typeVar ->
        ( Just (TFunctionType resolvedRightType TBoolType),
          addInferredEqualityClassConstraintIfVisible
            resolvedRightType
            (addStrictEqualityTypeVarConstraint typeVar state)
        )
      _
        | supportsRuntimeEqualityType state resolvedRightType ->
            (Just (TFunctionType resolvedRightType TBoolType), state)
        | otherwise ->
            ( Nothing,
              addTypeError
                state
                (mkStrictEqualityUnsupportedTypeError operatorSymbol resolvedRightType)
            )

numericSectionCounterpartType :: ExpressionType -> InferState -> (ExpressionType, InferState)
numericSectionCounterpartType sectionOperandType state =
  case sectionOperandType of
    TIntegerLiteralType literalRange ->
      let (typeVar, operandType, stateAfterOperandType) = freshTypeVariable state
       in
        ( operandType,
          addNumericTypeVarConstraint typeVar (IntegralLiteralNumericConstraint literalRange) stateAfterOperandType
        )
    _ -> (sectionOperandType, state)

numericLiteralBinaryRange ::
  Text ->
  NumericRuleResult ->
  IntegerLiteralRange ->
  IntegerLiteralRange ->
  IntegerLiteralRange
numericLiteralBinaryRange operatorSymbol resultRule leftRange rightRange =
  case resultRule of
    NumericSameTypeResult ->
      let operandRange = combineIntegerLiteralRanges leftRange rightRange
       in case integerLiteralArithmeticResultRange operatorSymbol leftRange rightRange of
            Just resultRange -> combineIntegerLiteralRanges operandRange resultRange
            Nothing -> operandRange
    NumericBoolResult ->
      combineIntegerLiteralRanges leftRange rightRange

integerLiteralArithmeticResultRange ::
  Text ->
  IntegerLiteralRange ->
  IntegerLiteralRange ->
  Maybe IntegerLiteralRange
integerLiteralArithmeticResultRange operatorSymbol (IntegerLiteralRange leftMin leftMax) (IntegerLiteralRange rightMin rightMax) =
  case operatorSymbol of
    "+" -> Just (IntegerLiteralRange (leftMin + rightMin) (leftMax + rightMax))
    "-" -> Just (IntegerLiteralRange (leftMin - rightMax) (leftMax - rightMin))
    "*" -> Just (rangeFromValues [leftMin * rightMin, leftMin * rightMax, leftMax * rightMin, leftMax * rightMax])
    "/"
      | rightMin <= 0 && rightMax >= 0 -> Nothing
      | otherwise ->
          Just
            ( rangeFromValues
                [ leftMin `div` rightMin,
                  leftMin `div` rightMax,
                  leftMax `div` rightMin,
                  leftMax `div` rightMax
                ]
            )
    _ -> Nothing

rangeFromValues :: [Integer] -> IntegerLiteralRange
rangeFromValues values = IntegerLiteralRange (minimum values) (maximum values)

instantiateOperatorType :: Text -> InferState -> Maybe (ExpressionType, InferState)
instantiateOperatorType operatorSymbol state =
  case lookupOperatorRule operatorSymbol of
    Just (NumericRule resultRule) ->
      let (typeVar, operandType, stateAfterOperandType) = freshTypeVariable state
          stateAfterNumericConstraint =
            addNumericTypeVarConstraint typeVar (numericRuleConstraint resultRule) stateAfterOperandType
       in
        Just
          ( TFunctionType
              operandType
              (TFunctionType operandType (numericRuleResultType resultRule operandType)),
            stateAfterNumericConstraint
          )
    Just StrictEqualityRule ->
      let (typeVar, operandType, stateAfterOperandType) = freshTypeVariable state
       in
        Just
          ( TFunctionType operandType (TFunctionType operandType TBoolType),
            addInferredEqualityClassConstraintIfVisible
              operandType
              (addStrictEqualityTypeVarConstraint typeVar stateAfterOperandType)
          )
    Just ApplicationRule ->
      let (argumentType, stateAfterArgumentType) = freshTypeVar state
          (resultType, stateAfterResultType) = freshTypeVar stateAfterArgumentType
       in
        Just
          ( TFunctionType
              (TFunctionType argumentType resultType)
              (TFunctionType argumentType resultType),
            stateAfterResultType
          )
    Nothing -> Nothing
