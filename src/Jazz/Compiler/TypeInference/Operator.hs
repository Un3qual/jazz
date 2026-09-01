{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Builtin operator typing rules, isolated from expression orchestration.
module Jazz.Compiler.TypeInference.Operator
  ( applyOperatorAliasSchemeConstraints,
    binaryNumericPromotionType,
    builtinSectionOperatorSymbol,
    hasOperatorRule,
    inferBinaryType,
    inferSectionLeftType,
    inferSectionRightType,
    instantiateOperatorType,
  )
where

import Control.Applicative ((<|>))
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import qualified Data.Set as Set
import Data.Text (Text)
import Jazz.Compiler.AST
  ( CorePhase (..),
    Expr (..),
    Literal (..),
  )
import Jazz.Compiler.BuiltinCatalog
  ( numericTypeFloatIntegerBounds,
    numericTypeIsIntegral,
  )
import Jazz.Compiler.TypeInference.Capabilities
  ( addInferredEqualityClassConstraintIfVisible,
    applyTypeSchemePrimitiveConstraints,
    capabilityFactsFromState,
    defaultLiteralTypes,
    deferExplicitConstraintsWithFacts,
    structuralRuntimeEqualityType,
  )
import Jazz.Compiler.TypeInference.Diagnostics
  ( addTypeError,
    mkApplyTypeError,
    mkBinaryTypeError,
    mkNumericBinaryTypeError,
    mkNumericSectionOperandTypeError,
    mkStrictEqualityTypeError,
    mkStrictEqualityUnsupportedTypeError,
    mkUnsupportedSectionOperatorError,
  )
import Jazz.Compiler.TypeInference.Solver
  ( addNumericTypeVarConstraint,
    addStrictEqualityTypeVarConstraint,
    combineIntegerLiteralRanges,
    constrainNumericOperatorType,
    freshTypeVar,
    freshTypeVariable,
    integerLiteralRangeBounds,
    integerLiteralRangeFor,
    resolveType,
    supportsRuntimeEqualityType,
    typeSatisfiesNumericConstraint,
    unifyTypes,
  )
import Jazz.Compiler.TypeInference.State
  ( InferState,
    inferRigidTypeVars,
  )
import Jazz.Compiler.TypeInference.TypeOps
  ( instantiateTypeSchemeConstraint,
    instantiateTypeSchemePrimitiveConstraint,
  )
import Jazz.Compiler.TypeInference.Types
  ( ExpressionType,
    IntegerLiteralRange (..),
    NumericConstraint (..),
    SemanticType (..),
    TypeScheme (..),
    quantifiedVariablesMembershipSet,
  )
import Jazz.Compiler.TypeRepresentation (NumericType (..))

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
    resolvedLeftType = defaultLiteralTypes state (resolveType state leftType)
    resolvedRightType = defaultLiteralTypes state (resolveType state rightType)

instantiateOperatorAliasSchemeConstraints :: TypeScheme -> ExpressionType -> InferState -> InferState
instantiateOperatorAliasSchemeConstraints typeScheme targetType state =
  let replacements =
        Map.fromList
          [ (typeVar, targetType)
          | typeVar <- Set.toList (quantifiedVariablesMembershipSet quantifiedVariables)
          ]
      instantiatedConstraints =
        map (instantiateTypeSchemeConstraint replacements) explicitConstraints
      instantiatedPrimitiveConstraints =
        map (instantiateTypeSchemePrimitiveConstraint replacements) primitiveConstraints
      stateWithPrimitiveConstraints =
        applyTypeSchemePrimitiveConstraints instantiatedPrimitiveConstraints state
   in deferExplicitConstraintsWithFacts
        (definingFacts <> capabilityFactsFromState state)
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
  Expr 'Resolved ->
  Expr 'Resolved ->
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
              (diagnosticType state leftType)
              (diagnosticType state rightType)
          )
      )

-- | Report the implicit Float64 operand promotion selected by the ordinary
-- operator rules. Consumers that cannot represent the conversion can reject
-- it explicitly instead of constructing a heterogeneous binary node.
binaryNumericPromotionType ::
  Text ->
  Expr 'Resolved ->
  Expr 'Resolved ->
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
  Expr 'Resolved ->
  Expr 'Resolved ->
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
              let (resolvedOperandType, stateAfterResultRange) =
                    numericBinaryOperandType
                      operatorSymbol
                      resultRule
                      leftLiteralRange
                      rightLiteralRange
                      stateAfterUnify
                      leftType
               in constrainNumericOperand resolvedOperandType stateAfterResultRange
            Nothing -> numericOperandError state
  where
    rigidNumericOperand =
      case (resolveType state leftType, resolveType state rightType) of
        (rigidType@(SemanticVariable typeVar), concreteType)
          | Set.member typeVar (inferRigidTypeVars state),
            typeSatisfiesNumericConstraint (numericRuleConstraint resultRule) concreteType ->
              Just rigidType
        (concreteType, rigidType@(SemanticVariable typeVar))
          | Set.member typeVar (inferRigidTypeVars state),
            typeSatisfiesNumericConstraint (numericRuleConstraint resultRule) concreteType ->
              Just rigidType
        _ -> Nothing

    leftLiteralRange = integerLiteralRangeFor state leftType
    rightLiteralRange = integerLiteralRangeFor state rightType

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
              (diagnosticType errState leftType)
              (diagnosticType errState rightType)
          )
      )

directIntegerFloat64NumericOperand :: NumericRuleResult -> InferState -> Expr 'Resolved -> Expr 'Resolved -> ExpressionType -> ExpressionType -> Maybe (ExpressionType, InferState)
directIntegerFloat64NumericOperand _resultRule state leftExpr rightExpr leftType rightType =
  integerLiteralFloat64PromotionOperand state leftExpr rightExpr leftType rightType
    <|> case typedIntegerFloat64PromotionOperand state leftType rightType of
      Just promotedType -> Just (promotedType, state)
      Nothing -> Nothing

numericRuleResultType :: NumericRuleResult -> ExpressionType -> ExpressionType
numericRuleResultType resultRule operandType =
  case resultRule of
    NumericSameTypeResult -> operandType
    NumericBoolResult -> SemanticBool

numericRuleConstraint :: NumericRuleResult -> NumericConstraint
numericRuleConstraint resultRule =
  case resultRule of
    NumericSameTypeResult -> RuntimeArithmeticNumericConstraint
    NumericBoolResult -> RuntimeComparisonNumericConstraint

integerLiteralFloat64PromotionOperand :: InferState -> Expr 'Resolved -> Expr 'Resolved -> ExpressionType -> ExpressionType -> Maybe (ExpressionType, InferState)
integerLiteralFloat64PromotionOperand state leftExpr rightExpr leftType rightType =
  case (integerLiteralRangeFor state leftType, integerLiteralRangeFor state rightType, resolveType state leftType, resolveType state rightType) of
    (Just literalRange, _, _, floatType)
      | exprIsIntegerLiteral leftExpr,
        integerLiteralRangeFitsFloat64 literalRange,
        expressionTypeIsFloat64Domain floatType ->
          Just (floatType, state)
    (_, Just literalRange, floatType, _)
      | exprIsIntegerLiteral rightExpr,
        integerLiteralRangeFitsFloat64 literalRange,
        expressionTypeIsFloat64Domain floatType ->
          Just (floatType, state)
    _ -> Nothing

exprIsIntegerLiteral :: Expr 'Resolved -> Bool
exprIsIntegerLiteral expr =
  case expr of
    ELit _ (LInt _) -> True
    _ -> False

expressionTypeIsFloat64Domain :: ExpressionType -> Bool
expressionTypeIsFloat64Domain expressionType =
  case expressionType of
    SemanticFloat -> True
    SemanticNumeric NumericFloat64 -> True
    _ -> False

expressionTypeIsConcreteIntegral :: ExpressionType -> Bool
expressionTypeIsConcreteIntegral expressionType =
  case expressionType of
    SemanticInt -> True
    SemanticNumeric numericType -> numericTypeIsIntegral numericType
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
  Maybe IntegerLiteralRange ->
  Maybe IntegerLiteralRange ->
  InferState ->
  ExpressionType ->
  (ExpressionType, InferState)
numericBinaryOperandType operatorSymbol resultRule leftLiteralRange rightLiteralRange state leftType =
  case (resultRule, leftLiteralRange, rightLiteralRange, resolvedLeftType) of
    (NumericSameTypeResult, Just leftRange, Just rightRange, SemanticVariable resultVar) ->
      ( resolvedLeftType,
        addNumericTypeVarConstraint
          resultVar
          (IntegralLiteralNumericConstraint (numericLiteralBinaryRange operatorSymbol resultRule leftRange rightRange))
          state
      )
    _ -> (resolvedLeftType, state)
  where
    resolvedLeftType = resolveType state leftType

applyApplicationBinaryRule ::
  ExpressionType ->
  ExpressionType ->
  InferState ->
  (Maybe ExpressionType, InferState)
applyApplicationBinaryRule functionType argumentType state =
  let (resultTypeVar, stateAfterResultVar) = freshTypeVar state
   in case unifyTypes functionType (SemanticFunction argumentType resultTypeVar) stateAfterResultVar of
        Just unifiedState ->
          (Just (resolveType unifiedState resultTypeVar), unifiedState)
        Nothing ->
          ( Nothing,
            addTypeError
              stateAfterResultVar
              ( mkApplyTypeError
                  (diagnosticType stateAfterResultVar functionType)
                  (diagnosticType stateAfterResultVar argumentType)
              )
          )

applyStrictEqualityBinaryRule ::
  Text ->
  Expr 'Resolved ->
  Expr 'Resolved ->
  ExpressionType ->
  ExpressionType ->
  InferState ->
  (Maybe ExpressionType, InferState)
applyStrictEqualityBinaryRule operatorSymbol leftExpr rightExpr leftType rightType state =
  case integerLiteralFloat64PromotionOperand state leftExpr rightExpr leftType rightType of
    Just _ ->
      (Just SemanticBool, state)
    Nothing ->
      case typedIntegerFloat64PromotionOperand state leftType rightType of
        Just _ ->
          (Just SemanticBool, state)
        Nothing ->
          strictEqualityFallback
  where
    strictEqualityFallback =
      case unifyTypes leftType rightType state of
        Just unifiedState ->
          let resolvedType = resolveType unifiedState leftType
           in case resolvedType of
                SemanticVariable typeVar ->
                  ( Just SemanticBool,
                    addInferredEqualityClassConstraintIfVisible
                      (SemanticVariable typeVar)
                      (addStrictEqualityTypeVarConstraint typeVar unifiedState)
                  )
                _
                  | supportsRuntimeEqualityType unifiedState resolvedType ->
                      (Just SemanticBool, unifiedState)
                  | otherwise ->
                      ( Nothing,
                        addTypeError
                          unifiedState
                          (mkStrictEqualityUnsupportedTypeError operatorSymbol (diagnosticType unifiedState resolvedType))
                      )
        Nothing ->
          ( Nothing,
            addTypeError
              state
              ( mkStrictEqualityTypeError
                  operatorSymbol
                  (diagnosticType state leftType)
                  (diagnosticType state rightType)
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
           in ( Just
                  ( SemanticFunction
                      rightType
                      (numericRuleResultType resultRule rightType)
                  ),
                stateAfterSectionType
              )
        Nothing ->
          ( Nothing,
            addTypeError
              state
              (mkNumericSectionOperandTypeError operatorSymbol (diagnosticType state leftType))
          )

applyStrictEqualitySectionLeftRule ::
  Text ->
  ExpressionType ->
  InferState ->
  (Maybe ExpressionType, InferState)
applyStrictEqualitySectionLeftRule operatorSymbol leftType state =
  let resolvedLeftType = resolveType state leftType
   in case resolvedLeftType of
        SemanticVariable typeVar ->
          ( Just (SemanticFunction resolvedLeftType SemanticBool),
            addInferredEqualityClassConstraintIfVisible
              resolvedLeftType
              (addStrictEqualityTypeVarConstraint typeVar state)
          )
        _
          | supportsRuntimeEqualityType state resolvedLeftType ->
              (Just (SemanticFunction resolvedLeftType SemanticBool), state)
          | otherwise ->
              ( Nothing,
                addTypeError
                  state
                  (mkStrictEqualityUnsupportedTypeError operatorSymbol (diagnosticType state resolvedLeftType))
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
           in ( Just
                  ( SemanticFunction
                      leftType
                      (numericRuleResultType resultRule leftType)
                  ),
                stateAfterSectionType
              )
        Nothing ->
          ( Nothing,
            addTypeError
              state
              (mkNumericSectionOperandTypeError operatorSymbol (diagnosticType state rightType))
          )

applyStrictEqualitySectionRightRule ::
  Text ->
  ExpressionType ->
  InferState ->
  (Maybe ExpressionType, InferState)
applyStrictEqualitySectionRightRule operatorSymbol rightType state =
  let resolvedRightType = resolveType state rightType
   in case resolvedRightType of
        SemanticVariable typeVar ->
          ( Just (SemanticFunction resolvedRightType SemanticBool),
            addInferredEqualityClassConstraintIfVisible
              resolvedRightType
              (addStrictEqualityTypeVarConstraint typeVar state)
          )
        _
          | supportsRuntimeEqualityType state resolvedRightType ->
              (Just (SemanticFunction resolvedRightType SemanticBool), state)
          | otherwise ->
              ( Nothing,
                addTypeError
                  state
                  (mkStrictEqualityUnsupportedTypeError operatorSymbol (diagnosticType state resolvedRightType))
              )

numericSectionCounterpartType :: ExpressionType -> InferState -> (ExpressionType, InferState)
numericSectionCounterpartType sectionOperandType state =
  case integerLiteralRangeFor state sectionOperandType of
    Just literalRange ->
      let (typeVar, operandType, stateAfterOperandType) = freshTypeVariable state
       in ( operandType,
            addNumericTypeVarConstraint typeVar (IntegralLiteralNumericConstraint literalRange) stateAfterOperandType
          )
    Nothing -> (sectionOperandType, state)

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

diagnosticType :: InferState -> ExpressionType -> ExpressionType
diagnosticType state = defaultLiteralTypes state . resolveType state

instantiateOperatorType :: Text -> InferState -> Maybe (ExpressionType, InferState)
instantiateOperatorType operatorSymbol state =
  case lookupOperatorRule operatorSymbol of
    Just (NumericRule resultRule) ->
      let (typeVar, operandType, stateAfterOperandType) = freshTypeVariable state
          stateAfterNumericConstraint =
            addNumericTypeVarConstraint typeVar (numericRuleConstraint resultRule) stateAfterOperandType
       in Just
            ( SemanticFunction
                operandType
                (SemanticFunction operandType (numericRuleResultType resultRule operandType)),
              stateAfterNumericConstraint
            )
    Just StrictEqualityRule ->
      let (typeVar, operandType, stateAfterOperandType) = freshTypeVariable state
       in Just
            ( SemanticFunction operandType (SemanticFunction operandType SemanticBool),
              addInferredEqualityClassConstraintIfVisible
                operandType
                (addStrictEqualityTypeVarConstraint typeVar stateAfterOperandType)
            )
    Just ApplicationRule ->
      let (argumentType, stateAfterArgumentType) = freshTypeVar state
          (resultType, stateAfterResultType) = freshTypeVar stateAfterArgumentType
       in Just
            ( SemanticFunction
                (SemanticFunction argumentType resultType)
                (SemanticFunction argumentType resultType),
              stateAfterResultType
            )
    Nothing -> Nothing
