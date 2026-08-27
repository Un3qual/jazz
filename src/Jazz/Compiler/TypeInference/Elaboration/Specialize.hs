module Jazz.Compiler.TypeInference.Elaboration.Specialize
  ( specializeInferredExpression,
    specializeProvisionalExpression,
    specializeProvisionalParameterReferences,
    specializeCompatibleType,
    specializeProvisionalCallableCapture,
    provisionalParameterReferenceTypes,
    specializeCallableCaptureType,
    provisionalExpressionType,
    specializeExpressionType,
    concreteIntegralType,
    defaultScalarLiterals,
  )
where

import Control.Applicative ((<|>))
import qualified Data.Set as Set
import Jazz.Compiler.AST (NumericType (..), Pattern (..))
import Jazz.Compiler.BuiltinCatalog
  ( numericTypeIsIntegral,
  )
import Jazz.Compiler.Name (Name)
import Jazz.Compiler.Pattern (patternBinderNames)
import Jazz.Compiler.TypeInference.Elaboration.Types
  ( InferredExpr (..),
    ProvisionalCallableDeclaration (..),
    ProvisionalPatternCaseArm (..),
    ProvisionalTypedExpr (..),
    ProvisionalTypedStatement (..),
  )
import Jazz.Compiler.TypeInference.Solver
  ( integerLiteralRangeFitsNumericType,
    resolveType,
  )
import Jazz.Compiler.TypeInference.State (InferState)
import Jazz.Compiler.TypeInference.Types (ExpressionType (..))

-- | Commit a successfully selected integral context into the provisional tree.
-- Integer-range unification validates compatibility but deliberately creates no
-- solver substitution, so production must retain the concrete representation
-- selected by a signature, operator operand, or function parameter.
specializeInferredExpression :: InferState -> ExpressionType -> InferredExpr -> InferredExpr
specializeInferredExpression state expectedType inferred =
  inferred
    { inferredExpressionType =
        specializeExpressionType state expectedType
          <$> inferredExpressionType inferred,
      inferredProvisionalExpr =
        specializeProvisionalExpression state (Just expectedType)
          <$> inferredProvisionalExpr inferred
    }

specializeProvisionalExpression :: InferState -> Maybe ExpressionType -> ProvisionalTypedExpr -> ProvisionalTypedExpr
specializeProvisionalExpression state maybeExpected expression =
  case expression of
    ProvisionalUnitExpression -> ProvisionalUnitExpression
    ProvisionalTupleExpression expressionType elements ->
      let resultType = specializedType expressionType
          expectedElements =
            case resultType of
              TTupleType elementTypes
                | length elementTypes == length elements -> map Just elementTypes
              _ -> replicate (length elements) Nothing
       in ProvisionalTupleExpression
            resultType
            (zipWith (specializeProvisionalExpression state) expectedElements elements)
    ProvisionalLiteralExpression literal expressionType ->
      ProvisionalLiteralExpression literal (specializedType expressionType)
    ProvisionalBinaryExpression operatorSymbol expressionType operandType left right ->
      let resultType = specializedType expressionType
          resolvedOperandType = resolveType state operandType
          operandExpected =
            concreteIntegralType resultType
              <|> concreteIntegralType resolvedOperandType
              <|> (maybeExpected >>= concreteIntegralType . resolveType state)
              <|> (provisionalExpressionType state left >>= concreteIntegralType . resolveType state)
              <|> (provisionalExpressionType state right >>= concreteIntegralType . resolveType state)
          specializedOperandType = maybe resolvedOperandType id operandExpected
       in ProvisionalBinaryExpression
            operatorSymbol
            resultType
            specializedOperandType
            (specializeProvisionalExpression state operandExpected left)
            (specializeProvisionalExpression state operandExpected right)
    ProvisionalVariableExpression name expressionType ->
      ProvisionalVariableExpression name (specializedType expressionType)
    ProvisionalLambdaExpression parameterName expressionType body ->
      let specializedFunctionType = specializedType expressionType
          bodyExpected =
            case specializedFunctionType of
              TFunctionType _ resultType -> Just resultType
              _ -> Nothing
       in ProvisionalLambdaExpression
            parameterName
            specializedFunctionType
            (specializeProvisionalExpression state bodyExpected body)
    ProvisionalApplyExpression expressionType function argument ->
      let resultType = specializedType expressionType
          argumentExpected =
            case provisionalExpressionType state function of
              Just (TFunctionType parameterType _) -> Just parameterType
              _ -> Nothing
       in ProvisionalApplyExpression
            resultType
            (specializeProvisionalExpression state Nothing function)
            (specializeProvisionalExpression state argumentExpected argument)
    ProvisionalIfExpression expressionType condition thenExpression elseExpression ->
      let resultType = specializedType expressionType
       in ProvisionalIfExpression
            resultType
            (specializeProvisionalExpression state (Just TBoolType) condition)
            (specializeProvisionalExpression state (Just resultType) thenExpression)
            (specializeProvisionalExpression state (Just resultType) elseExpression)
    ProvisionalPatternCaseExpression expressionType scrutinee arms ->
      let resultType = specializedType expressionType
          initiallySpecializedArms =
            [ ProvisionalPatternCaseArm
                pattern
                (specializeProvisionalExpression state (Just TBoolType) <$> maybeGuard)
                (specializeProvisionalExpression state (Just resultType) body)
            | ProvisionalPatternCaseArm pattern maybeGuard body <- arms
            ]
          initialScrutineeType =
            case provisionalExpressionType state scrutinee of
              Just scrutineeType -> scrutineeType
              Nothing -> TTupleType []
          selectedScrutineeType =
            foldl' selectArmScrutineeType initialScrutineeType initiallySpecializedArms
       in ProvisionalPatternCaseExpression
            resultType
            (specializeProvisionalExpression state (Just selectedScrutineeType) scrutinee)
            (map (specializeArmBinder selectedScrutineeType) initiallySpecializedArms)
    ProvisionalScopeStatements statements -> ProvisionalScopeStatements statements
    ProvisionalUnsupportedExpression kind detail -> ProvisionalUnsupportedExpression kind detail
    ProvisionalRetainedFailures failures -> ProvisionalRetainedFailures failures
  where
    selectArmScrutineeType selectedType (ProvisionalPatternCaseArm pattern maybeGuard body) =
      case pattern of
        PVariable name ->
          foldl'
            (\nextType referenceType -> specializeCompatibleType state referenceType nextType)
            selectedType
            ( maybe [] (provisionalParameterReferenceTypes name) maybeGuard
                <> provisionalParameterReferenceTypes name body
            )
        _ -> selectedType

    specializeArmBinder selectedType (ProvisionalPatternCaseArm pattern maybeGuard body) =
      case pattern of
        PVariable name ->
          ProvisionalPatternCaseArm
            pattern
            (specializeProvisionalParameterReferences state name selectedType <$> maybeGuard)
            (specializeProvisionalParameterReferences state name selectedType body)
        _ -> ProvisionalPatternCaseArm pattern maybeGuard body

    specializedType expressionType =
      case maybeExpected of
        Just expectedType -> specializeExpressionType state expectedType expressionType
        Nothing -> resolveType state expressionType

specializeProvisionalParameterReferences :: InferState -> Name -> ExpressionType -> ProvisionalTypedExpr -> ProvisionalTypedExpr
specializeProvisionalParameterReferences state parameterName selectedType = expressionReferences False
  where
    expressionReferences shadowed expression =
      case expression of
        ProvisionalUnitExpression -> ProvisionalUnitExpression
        ProvisionalTupleExpression expressionType elements ->
          ProvisionalTupleExpression expressionType (map child elements)
        ProvisionalLiteralExpression {} -> expression
        ProvisionalBinaryExpression operatorSymbol expressionType operandType left right ->
          ProvisionalBinaryExpression
            operatorSymbol
            expressionType
            operandType
            (child left)
            (child right)
        ProvisionalVariableExpression name expressionType
          | not shadowed,
            name == parameterName ->
              ProvisionalVariableExpression name (specializeCompatibleType state selectedType expressionType)
          | otherwise -> expression
        ProvisionalLambdaExpression nestedParameterName expressionType body ->
          ProvisionalLambdaExpression
            nestedParameterName
            expressionType
            (expressionReferences (shadowed || nestedParameterName == parameterName) body)
        ProvisionalApplyExpression expressionType function argument ->
          ProvisionalApplyExpression expressionType (child function) (child argument)
        ProvisionalIfExpression expressionType condition thenExpression elseExpression ->
          ProvisionalIfExpression
            expressionType
            (child condition)
            (child thenExpression)
            (child elseExpression)
        ProvisionalPatternCaseExpression expressionType scrutinee arms ->
          ProvisionalPatternCaseExpression
            expressionType
            (child scrutinee)
            [ let armShadowed =
                    shadowed || Set.member parameterName (patternBinderNames pattern)
               in ProvisionalPatternCaseArm
                    pattern
                    (expressionReferences armShadowed <$> maybeGuard)
                    (expressionReferences armShadowed body)
            | ProvisionalPatternCaseArm pattern maybeGuard body <- arms
            ]
        ProvisionalScopeStatements statements -> ProvisionalScopeStatements statements
        ProvisionalUnsupportedExpression {} -> expression
        ProvisionalRetainedFailures {} -> expression
      where
        child = expressionReferences shadowed

specializeCompatibleType :: InferState -> ExpressionType -> ExpressionType -> ExpressionType
specializeCompatibleType state expectedType expressionType =
  case (resolveType state expressionType, resolveType state expectedType) of
    (TFunctionType expressionParameter expressionResult, TFunctionType expectedParameter expectedResult) ->
      TFunctionType
        (specializeCompatibleType state expectedParameter expressionParameter)
        (specializeCompatibleType state expectedResult expressionResult)
    _ -> specializeExpressionType state expectedType expressionType

specializeProvisionalCallableCapture :: InferState -> ExpressionType -> ProvisionalTypedExpr -> ProvisionalTypedExpr
specializeProvisionalCallableCapture state captureType expression =
  case expression of
    ProvisionalLambdaExpression parameterName expressionType body ->
      let specializedBody = specializeProvisionalCallableCapture state captureType body
          fallbackFunctionType = specializeCallableCaptureType state captureType expressionType
          specializedFunctionType =
            case fallbackFunctionType of
              TFunctionType parameterType resultType ->
                let selectedParameterType =
                      foldl'
                        (\selectedType referenceType -> specializeCompatibleType state referenceType selectedType)
                        parameterType
                        ( provisionalParameterApplicationTypes state captureType parameterName specializedBody
                            <> provisionalParameterReferenceTypes parameterName specializedBody
                        )
                    parameterSpecializedBody =
                      specializeProvisionalParameterReferences state parameterName selectedParameterType specializedBody
                 in TFunctionType
                      selectedParameterType
                      (maybe resultType id (provisionalExpressionType state parameterSpecializedBody))
              _ -> fallbackFunctionType
          selectedBody =
            case specializedFunctionType of
              TFunctionType parameterType _ ->
                specializeProvisionalParameterReferences state parameterName parameterType specializedBody
              _ -> specializedBody
       in ProvisionalLambdaExpression parameterName specializedFunctionType selectedBody
    _ -> specializeProvisionalExpression state (Just captureType) expression

provisionalParameterApplicationTypes :: InferState -> ExpressionType -> Name -> ProvisionalTypedExpr -> [ExpressionType]
provisionalParameterApplicationTypes state captureType parameterName = expressionApplicationTypes False
  where
    expressionApplicationTypes shadowed expression =
      case expression of
        ProvisionalUnitExpression -> []
        ProvisionalTupleExpression _ elements -> foldMap child elements
        ProvisionalLiteralExpression {} -> []
        ProvisionalBinaryExpression _ _ _ left right -> child left <> child right
        ProvisionalVariableExpression {} -> []
        ProvisionalLambdaExpression nestedParameterName _ body ->
          expressionApplicationTypes (shadowed || nestedParameterName == parameterName) body
        ProvisionalApplyExpression {} ->
          let (callee, arguments, resultType) = applicationProfile expression
              argumentTypes =
                [ specializeCompatibleType state captureType argumentType
                | argument <- arguments,
                  Just argumentType <- [provisionalExpressionType state argument]
                ]
              selectedResultType = specializeCompatibleType state captureType resultType
              selectedApplicationType = foldr TFunctionType selectedResultType argumentTypes
              applicationType =
                case callee of
                  ProvisionalVariableExpression name _
                    | not shadowed,
                      name == parameterName ->
                        [selectedApplicationType]
                  _ -> []
              childTypes =
                case applicationType of
                  _ : _ -> concatMap child arguments
                  [] -> child callee <> concatMap child arguments
           in applicationType <> childTypes
        ProvisionalIfExpression _ condition thenExpression elseExpression ->
          child condition <> child thenExpression <> child elseExpression
        ProvisionalPatternCaseExpression _ scrutinee arms ->
          child scrutinee <> foldMap armChildren arms
        ProvisionalScopeStatements statements -> scopeApplicationTypes shadowed statements
        ProvisionalUnsupportedExpression {} -> []
        ProvisionalRetainedFailures {} -> []
      where
        child = expressionApplicationTypes shadowed
        armChildren (ProvisionalPatternCaseArm pattern maybeGuard body) =
          let armShadowed =
                shadowed || Set.member parameterName (patternBinderNames pattern)
              armChild = expressionApplicationTypes armShadowed
           in maybe [] armChild maybeGuard <> armChild body

    applicationProfile expression =
      let selectedResultType =
            maybe captureType id (provisionalExpressionType state expression)
       in go [] selectedResultType expression
      where
        go arguments resultType (ProvisionalApplyExpression _ function argument) =
          go (argument : arguments) resultType function
        go arguments resultType callee = (callee, arguments, resultType)

    scopeApplicationTypes _ [] = []
    scopeApplicationTypes shadowed (statement : rest) =
      case statement of
        ProvisionalFunctionBinding declaration nestedExpression ->
          let name = provisionalCallableName declaration
              nextShadowed = shadowed || name == parameterName
           in expressionApplicationTypes nextShadowed nestedExpression <> scopeApplicationTypes nextShadowed rest
        ProvisionalScalarBinding _ name _ _ nestedExpression ->
          expressionApplicationTypes shadowed nestedExpression
            <> scopeApplicationTypes (shadowed || name == parameterName) rest
        ProvisionalTerminalExpression _ _ nestedExpression ->
          expressionApplicationTypes shadowed nestedExpression <> scopeApplicationTypes shadowed rest
        _ -> scopeApplicationTypes shadowed rest

provisionalParameterReferenceTypes :: Name -> ProvisionalTypedExpr -> [ExpressionType]
provisionalParameterReferenceTypes parameterName = expressionReferenceTypes False
  where
    expressionReferenceTypes shadowed expression =
      case expression of
        ProvisionalUnitExpression -> []
        ProvisionalTupleExpression _ elements -> foldMap child elements
        ProvisionalLiteralExpression {} -> []
        ProvisionalBinaryExpression _ _ _ left right -> child left <> child right
        ProvisionalVariableExpression name expressionType
          | not shadowed,
            name == parameterName ->
              [expressionType]
          | otherwise -> []
        ProvisionalLambdaExpression nestedParameterName _ body ->
          expressionReferenceTypes (shadowed || nestedParameterName == parameterName) body
        ProvisionalApplyExpression _ function argument -> child function <> child argument
        ProvisionalIfExpression _ condition thenExpression elseExpression ->
          child condition <> child thenExpression <> child elseExpression
        ProvisionalPatternCaseExpression _ scrutinee arms ->
          child scrutinee <> foldMap armChildren arms
        ProvisionalScopeStatements statements -> scopeReferenceTypes shadowed statements
        ProvisionalUnsupportedExpression {} -> []
        ProvisionalRetainedFailures {} -> []
      where
        child = expressionReferenceTypes shadowed
        armChildren (ProvisionalPatternCaseArm pattern maybeGuard body) =
          let armShadowed =
                shadowed || Set.member parameterName (patternBinderNames pattern)
              armChild = expressionReferenceTypes armShadowed
           in maybe [] armChild maybeGuard <> armChild body

    scopeReferenceTypes _ [] = []
    scopeReferenceTypes shadowed (statement : rest) =
      case statement of
        ProvisionalFunctionBinding declaration expression ->
          let name = provisionalCallableName declaration
              nextShadowed = shadowed || name == parameterName
           in expressionReferenceTypes nextShadowed expression <> scopeReferenceTypes nextShadowed rest
        ProvisionalScalarBinding _ name _ _ expression ->
          expressionReferenceTypes shadowed expression
            <> scopeReferenceTypes (shadowed || name == parameterName) rest
        ProvisionalTerminalExpression _ _ expression ->
          expressionReferenceTypes shadowed expression <> scopeReferenceTypes shadowed rest
        _ -> scopeReferenceTypes shadowed rest

specializeCallableCaptureType :: InferState -> ExpressionType -> ExpressionType -> ExpressionType
specializeCallableCaptureType state captureType expressionType =
  case resolveType state expressionType of
    TFunctionType parameterType resultType ->
      TFunctionType parameterType (specializeCallableCaptureType state captureType resultType)
    resultType -> specializeExpressionType state captureType resultType

provisionalExpressionType :: InferState -> ProvisionalTypedExpr -> Maybe ExpressionType
provisionalExpressionType state expression =
  resolveType state <$> case expression of
    ProvisionalUnitExpression -> Just (TTupleType [])
    ProvisionalTupleExpression expressionType _ -> Just expressionType
    ProvisionalLiteralExpression _ expressionType -> Just expressionType
    ProvisionalBinaryExpression _ expressionType _ _ _ -> Just expressionType
    ProvisionalVariableExpression _ expressionType -> Just expressionType
    ProvisionalLambdaExpression _ expressionType _ -> Just expressionType
    ProvisionalApplyExpression expressionType _ _ -> Just expressionType
    ProvisionalIfExpression expressionType _ _ _ -> Just expressionType
    ProvisionalPatternCaseExpression expressionType _ _ -> Just expressionType
    ProvisionalScopeStatements {} -> Nothing
    ProvisionalUnsupportedExpression {} -> Nothing
    ProvisionalRetainedFailures {} -> Nothing

specializeExpressionType :: InferState -> ExpressionType -> ExpressionType -> ExpressionType
specializeExpressionType state expectedType expressionType =
  let resolvedExpected = resolveType state expectedType
      resolvedExpression = resolveType state expressionType
   in case (resolvedExpression, resolvedExpected) of
        (TTupleType expressionElements, TTupleType expectedElements)
          | length expressionElements == length expectedElements ->
              TTupleType (zipWith (specializeExpressionType state) expectedElements expressionElements)
        (TIntegerLiteralType literalRange, TIntType)
          | integerLiteralRangeFitsNumericType literalRange NumericInt64 -> TIntType
        (TIntegerLiteralType literalRange, numericType@(TNumericType concreteType))
          | integerLiteralRangeFitsNumericType literalRange concreteType -> numericType
        (TIntType, TNumericType NumericInt64) -> resolvedExpected
        (TNumericType NumericInt64, TIntType) -> resolvedExpected
        (TFloatType, TNumericType NumericFloat64) -> resolvedExpected
        (TNumericType NumericFloat64, TFloatType) -> resolvedExpected
        _ -> resolvedExpression

concreteIntegralType :: ExpressionType -> Maybe ExpressionType
concreteIntegralType expressionType =
  case expressionType of
    TIntType -> Just TIntType
    numericType@(TNumericType concreteType)
      | numericTypeIsIntegral concreteType -> Just numericType
    _ -> Nothing

defaultScalarLiterals :: ExpressionType -> ExpressionType
defaultScalarLiterals expressionType =
  case expressionType of
    TIntegerLiteralType literalRange
      | integerLiteralRangeFitsNumericType literalRange NumericInt64 -> TIntType
    _ -> expressionType
