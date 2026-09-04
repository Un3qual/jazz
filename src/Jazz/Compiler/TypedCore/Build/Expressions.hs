{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Checked expression construction from immutable analyzed nodes. Expected
-- types belong to this construction context, not to the input node's facts.
module Jazz.Compiler.TypedCore.Build.Expressions
  ( ExpressionContext (..),
    ExpressionBinding (..),
    ExpressionPurpose (..),
    buildExpression,
    callableTypeInfo,
    expressionFacts,
    selectType,
  )
where

import Control.Applicative ((<|>))
import Data.Either (partitionEithers)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Jazz.Compiler.AST (CaseArm (..), CoreNode (..), CoreNodeId, CorePhase (Analyzed), Expr (..), Literal (..), Statement (..))
import Jazz.Compiler.BuiltinCatalog (numericTypeIntegerBounds, numericTypeIsIntegral)
import Jazz.Compiler.FractionalLiteral (fractionalLiteralSourceParts)
import Jazz.Compiler.Name (ResolvedName, identifierText)
import Jazz.Compiler.SemanticFacts (AnalyzedNumericConstraint (..), AnalyzedType, BinaryOperandTyping (..), BinaryOperation (..), ExpressionFacts (..))
import Jazz.Compiler.TypeRepresentation (NumericType (..), SemanticType (..))
import Jazz.Compiler.TypedCore
import Jazz.Compiler.TypedCore.Build.Result
  ( TypedCoreProductionFailure (..),
    TypedCoreProductionFailureDetail (..),
    TypedCoreProductionFailureKind (..),
    TypedCoreProductionPath (..),
  )
import Jazz.Compiler.TypedCore.Build.StructuredValues (StructuredValueCatalog, structuredNodeInfo)

data ExpressionContext = ExpressionContext
  { expressionModulePath :: [Text],
    expressionStatementIndex :: Int,
    expressionChildPath :: [Int],
    expressionExpectedType :: Maybe AnalyzedType,
    expressionBindings :: Map.Map ResolvedName ExpressionBinding,
    expressionPurpose :: ExpressionPurpose
  }

data ExpressionBinding = ExpressionBinding
  { bindingOwner :: TypedBinderId,
    bindingSemanticType :: AnalyzedType,
    bindingCallable :: Maybe (TypedCallableShape, Int)
  }

data ExpressionPurpose
  = ExpressionValue
  | ExpressionCallee
  | FunctionDefinition TypedCallableShape Int

callableTypeInfo :: StructuredValueCatalog -> TypedCallableShape -> Int -> AnalyzedType -> Maybe TypedNodeInfo
callableTypeInfo catalog shape arity expressionType = case (shape, arity, expressionType) of
  (TypedDirectCallableShape, remaining, SemanticFunction argument result) | remaining > 0 -> do
    argumentInfo <- structuredNodeInfo catalog argument
    resultInfo <- if remaining == 1 then structuredNodeInfo catalog result else callableTypeInfo catalog shape (remaining - 1) result
    let resultRecipe = typedNodeRecipe resultInfo
        recipe = case (remaining, resultRecipe) of
          (count, TypedClosureRecipe arguments finalResult) | count > 1 -> TypedClosureRecipe (typedNodeRecipe argumentInfo : arguments) finalResult
          _ -> TypedClosureRecipe [typedNodeRecipe argumentInfo] resultRecipe
    pure (TypedNodeInfo (SemanticFunction (typedNodeType argumentInfo) (typedNodeType resultInfo)) recipe [] [])
  _ -> structuredNodeInfo catalog expressionType

buildExpression :: StructuredValueCatalog -> ExpressionContext -> Expr 'Analyzed -> Either [TypedCoreProductionFailure] TypedExpr
buildExpression catalog context expression
  | Just operation <- expressionBinaryOperation facts = buildBinary operation
  | otherwise = case expression of
      ELit _ literal -> do
        info <- nodeInfo selectedType
        TypedLiteralExpr info <$> typedLiteral info literal
      ETuple _ [] -> Right (TypedTupleExpr (TypedNodeInfo (SemanticTuple []) TypedUnitRecipe [] []) [])
      ETuple _ elements -> do
        let expectedElements = case selectedType of
              SemanticTuple types | length types == length elements -> map Just types
              _ -> replicate (length elements) Nothing
        children <- collect (zipWith3 buildChild [0 ..] expectedElements elements)
        info <- nodeInfo selectedType
        pure (TypedTupleExpr info children)
      EVar _ name -> case Map.lookup name (expressionBindings context) of
        Just binding -> case (bindingCallable binding, expressionPurpose context) of
          (Just (TypedDirectCallableShape, _), ExpressionValue) -> reject TypedCoreCallableValueUnsupported (TypedCoreNameDetail (identifierText name))
          _ -> do
            info <- case bindingCallable binding of
              Just (shape, arity) -> maybe (reject TypedCoreUnresolvedExpressionType TypedCoreUnsupportedRootDetail) Right (callableTypeInfo catalog shape arity (bindingSemanticType binding))
              Nothing -> callableOrValueInfo (selectType facts (expressionExpectedType context <|> Just (bindingSemanticType binding)) (expressionSemanticType facts))
            pure (TypedVariableExpr info (valueName name) (Just (bindingOwner binding)))
        Nothing -> reject TypedCoreCaptureUnsupported (TypedCoreNameDetail (identifierText name))
      ELambda _ name body -> case selectedType of
        SemanticFunction argumentType resultType -> do
          let (shape, arity) = case expressionPurpose context of
                FunctionDefinition selectedShape remaining -> (selectedShape, remaining)
                _ -> (TypedClosureCallableShape, 1)
          info <- maybe (reject TypedCoreUnresolvedExpressionType TypedCoreUnsupportedRootDetail) Right (callableTypeInfo catalog shape arity selectedType)
          let binder = TypedBinderId (expressionModulePath context, expressionStatementIndex context : expressionChildPath context, valueName name)
              parameterCallable = case argumentType of SemanticFunction {} -> Just (TypedClosureCallableShape, 1); _ -> Nothing
              bodyContext =
                context
                  { expressionChildPath = expressionChildPath context <> [0],
                    expressionExpectedType = Just resultType,
                    expressionBindings = Map.insert name (ExpressionBinding binder argumentType parameterCallable) (expressionBindings context),
                    expressionPurpose = case expressionPurpose context of
                      FunctionDefinition _ remaining -> FunctionDefinition shape (max 0 (remaining - 1))
                      _ -> ExpressionValue
                  }
          TypedLambdaExpr info binder (valueName name) <$> buildExpression catalog bodyContext body
        _ -> reject TypedCoreUnresolvedExpressionType TypedCoreUnsupportedRootDetail
      EApply _ function argument ->
        let functionType = selectType (expressionFacts function) Nothing (expressionSemanticType (expressionFacts function))
            argumentExpected = case functionType of
              SemanticFunction argumentType _ -> Just argumentType
              _ -> Nothing
            functionResult = buildExpression catalog context {expressionChildPath = expressionChildPath context <> [0], expressionExpectedType = Nothing, expressionPurpose = ExpressionCallee} function
            argumentResult = buildChild 1 argumentExpected argument
            resultInfo = callableOrValueInfo selectedType
         in case (resultInfo, functionResult, argumentResult) of
              (Right info, Right typedFunction, Right typedArgument) -> Right (TypedApplyExpr info typedFunction typedArgument)
              _ -> Left (errors resultInfo <> errors functionResult <> errors argumentResult)
      EIf _ condition thenExpression elseExpression ->
        let info = callableOrValueInfo selectedType
            conditionResult = buildChild 0 (Just SemanticBool) condition
            thenResult = buildChild 1 (Just selectedType) thenExpression
            elseResult = buildChild 2 (Just selectedType) elseExpression
         in case (info, conditionResult, thenResult, elseResult) of
              (Right resultInfo, Right typedCondition, Right typedThen, Right typedElse) -> Right (TypedIfExpr resultInfo typedCondition typedThen typedElse)
              _ -> Left (errors info <> errors conditionResult <> errors thenResult <> errors elseResult)
      EList _ elements -> rejectWithChildren TypedCoreStructuredValueUnsupported TypedCoreListValueDetail elements
      EBlock _ statements -> rejectWithChildren TypedCoreNestedBlockUnsupported TypedCoreLocalBlockDetail [child | SExpr _ child <- statements]
      EBinary _ _ left right -> rejectWithChildren TypedCoreUserDefinedOperatorUnsupported TypedCoreUnsupportedRootDetail [left, right]
      EOperatorValue {} -> reject TypedCoreUserDefinedOperatorUnsupported TypedCoreUnsupportedRootDetail
      ESectionLeft {} -> reject TypedCoreUserDefinedOperatorUnsupported TypedCoreUnsupportedRootDetail
      ESectionRight {} -> reject TypedCoreUserDefinedOperatorUnsupported TypedCoreUnsupportedRootDetail
      _ -> reject TypedCoreUnsupportedRootExpression TypedCoreUnsupportedRootDetail
  where
    facts = expressionFacts expression
    selectedType = selectType facts (expressionExpectedType context) (expressionSemanticType facts)
    valueName = TypedResolvedName TypedCurrentModule TypedValueNamespace . identifierText
    reject kind detail = Left [failure context kind detail]
    buildChild index expected = buildExpression catalog context {expressionChildPath = expressionChildPath context <> [index], expressionExpectedType = expected, expressionPurpose = ExpressionValue}
    rejectWithChildren kind detail children =
      Left (failure context kind detail : concat [errors (buildChild index Nothing child) | (index, child) <- zip [0 ..] children])

    nodeInfo expressionType = case expressionType of
      SemanticFunction {} -> reject TypedCoreManagedValueUnsupported TypedCoreUnsupportedRootDetail
      SemanticList {} -> reject TypedCoreStructuredValueUnsupported TypedCoreListValueDetail
      SemanticVariable {} -> reject TypedCoreUnresolvedExpressionType TypedCoreUnsupportedRootDetail
      _ -> callableOrValueInfo expressionType

    callableOrValueInfo expressionType = case structuredNodeInfo catalog expressionType of
      Just info -> Right info
      Nothing -> reject TypedCoreStructuredValueUnsupported (case expressionType of SemanticTuple {} -> TypedCoreTupleValueDetail; _ -> TypedCoreDataValueDetail)

    buildBinary operation = case (findExpression (binaryOperationLeftOperand operation) expression, findExpression (binaryOperationRightOperand operation) expression) of
      (Just left, Just right) -> case binaryOperationOperandTyping operation of
        Float64PromotedOperands -> rejectWithChildren TypedCoreUnsupportedRootExpression TypedCoreUnsupportedRootDetail [left, right]
        UniformBinaryOperands operandType ->
          let symbol = binaryOperationSymbol operation
              expectedOperand =
                concreteIntegral selectedType
                  <|> concreteIntegral operandType
                  <|> (expressionExpectedType context >>= concreteIntegral)
                  <|> concreteIntegral (expressionSemanticType (expressionFacts left))
                  <|> concreteIntegral (expressionSemanticType (expressionFacts right))
              selectedOperand = selectType facts expectedOperand operandType
              leftResult = buildChild 0 (Just selectedOperand) left
              rightResult = buildChild 1 (Just selectedOperand) right
              childFailures = errors leftResult <> errors rightResult
              info = nodeInfo selectedType
              managedEquality =
                symbol `elem` ["==", "!="] && case selectedOperand of
                  SemanticTuple (_ : _) -> True
                  SemanticData {} -> True
                  _ -> False
           in if managedEquality
                then Left (failure context TypedCoreManagedValueUnsupported TypedCoreUnsupportedRootDetail : childFailures)
                else case (info, leftResult, rightResult) of
                  (Right resultInfo, Right typedLeft, Right typedRight) -> Right (TypedBinaryExpr resultInfo (TypedBuiltinOperator symbol) typedLeft typedRight)
                  _ -> Left (errors info <> childFailures)
      _ -> reject TypedCoreUnsupportedRootExpression TypedCoreUnsupportedRootDetail

    typedLiteral info literal = case (literal, typedNodeType info) of
      (LInt value, SemanticInt) -> integer value
      (LInt value, SemanticNumeric _) -> integer value
      (LFloat _ source _, SemanticFloat) -> fractional source Nothing
      (LFloat _ source sourceType, SemanticNumeric numericType) -> fractional source (sourceType <|> Just numericType)
      (LBool value, SemanticBool) -> Right (TypedBooleanLiteral value)
      (LChar value, SemanticChar) -> Right (TypedCharacterLiteral value)
      (LText value, SemanticText) -> Right (TypedTextLiteral value)
      _ -> reject TypedCoreUnsupportedRootExpression TypedCoreUnsupportedRootDetail
    integer = Right . TypedIntegerLiteral . Text.pack . show
    fractional source numericType =
      let (whole, fractionalPart, scale) = fractionalLiteralSourceParts source
          digits = Text.justifyRight (max 0 (length (show scale) - 1)) '0' (Text.pack (show (abs fractionalPart)))
       in Right (TypedFractionalLiteral (Text.pack (show whole)) digits numericType)

failure :: ExpressionContext -> TypedCoreProductionFailureKind -> TypedCoreProductionFailureDetail -> TypedCoreProductionFailure
failure context = TypedCoreProductionFailure (TypedCoreProductionExpressionPath (expressionModulePath context) (expressionStatementIndex context) (expressionChildPath context))

collect :: [Either [failure] value] -> Either [failure] [value]
collect results = case partitionEithers results of
  ([], values) -> Right values
  (failures, _) -> Left (concat failures)

errors :: Either [failure] value -> [failure]
errors = either id (const [])

-- Select a representation only for a compatible literal range. This does not
-- unify types or change instantiations, evidence, or other analyzed facts.
selectType :: ExpressionFacts -> Maybe AnalyzedType -> AnalyzedType -> AnalyzedType
selectType facts expected original = case (original, expected) of
  (SemanticVariable variable, _) -> case Map.lookup variable (expressionNumericConstraints facts) of
    Just (AnalyzedIntegralLiteralNumericConstraint lower upper) ->
      let target = expected >>= concreteIntegral
          fits selected = case selected of
            SemanticInt -> inRange NumericInt64
            SemanticNumeric numericType -> inRange numericType
            _ -> False
          inRange numericType = case numericTypeIntegerBounds numericType of
            Just (minimumValue, maximumValue) -> lower >= minimumValue && upper <= maximumValue
            Nothing -> False
       in case target of
            Just selected | fits selected -> selected
            _ | fits SemanticInt -> SemanticInt
            _ -> original
    _ -> original
  (SemanticTuple elements, Just (SemanticTuple expectedElements))
    | length elements == length expectedElements ->
        SemanticTuple (zipWith (\element target -> selectType facts (Just target) element) elements expectedElements)
  (SemanticData name arguments, Just (SemanticData expectedName expectedArguments))
    | name == expectedName,
      length arguments == length expectedArguments ->
        SemanticData name (zipWith (\argument target -> selectType facts (Just target) argument) arguments expectedArguments)
  (SemanticFunction argument result, Just (SemanticFunction expectedArgument expectedResult)) ->
    SemanticFunction (selectType facts (Just expectedArgument) argument) (selectType facts (Just expectedResult) result)
  (SemanticFunction argument result, _) -> SemanticFunction (selectType facts Nothing argument) (selectType facts Nothing result)
  (SemanticTuple elements, _) -> SemanticTuple (map (selectType facts Nothing) elements)
  (SemanticData name arguments, _) -> SemanticData name (map (selectType facts Nothing) arguments)
  (SemanticInt, Just (SemanticNumeric NumericInt64)) -> SemanticNumeric NumericInt64
  (SemanticNumeric NumericInt64, Just SemanticInt) -> SemanticInt
  (SemanticFloat, Just (SemanticNumeric NumericFloat64)) -> SemanticNumeric NumericFloat64
  (SemanticNumeric NumericFloat64, Just SemanticFloat) -> SemanticFloat
  _ -> original

concreteIntegral :: AnalyzedType -> Maybe AnalyzedType
concreteIntegral expressionType = case expressionType of
  SemanticInt -> Just SemanticInt
  SemanticNumeric numericType | numericTypeIsIntegral numericType -> Just expressionType
  _ -> Nothing

expressionFacts :: Expr 'Analyzed -> ExpressionFacts
expressionFacts expression = case expression of
  ELit node _ -> coreNodeFacts node
  EVar node _ -> coreNodeFacts node
  ELambda node _ _ -> coreNodeFacts node
  EOperatorValue node _ -> coreNodeFacts node
  EList node _ -> coreNodeFacts node
  ETuple node _ -> coreNodeFacts node
  EApply node _ _ -> coreNodeFacts node
  ETypeApplication node _ _ _ -> coreNodeFacts node
  EIf node _ _ _ -> coreNodeFacts node
  EPatternCase node _ _ -> coreNodeFacts node
  EBinary node _ _ _ -> coreNodeFacts node
  ESectionLeft node _ _ -> coreNodeFacts node
  ESectionRight node _ _ -> coreNodeFacts node
  EBlock node _ -> coreNodeFacts node

findExpression :: CoreNodeId -> Expr 'Analyzed -> Maybe (Expr 'Analyzed)
findExpression requested expression =
  let (nodeId, children) = case expression of
        ELit node _ -> (coreNodeId node, [])
        EVar node _ -> (coreNodeId node, [])
        ELambda node _ body -> (coreNodeId node, [body])
        EOperatorValue node _ -> (coreNodeId node, [])
        EList node elements -> (coreNodeId node, elements)
        ETuple node elements -> (coreNodeId node, elements)
        EApply node function argument -> (coreNodeId node, [function, argument])
        ETypeApplication node function _ _ -> (coreNodeId node, [function])
        EIf node condition thenExpression elseExpression -> (coreNodeId node, [condition, thenExpression, elseExpression])
        EPatternCase node scrutinee arms -> (coreNodeId node, scrutinee : concat [maybe [] (: []) guardExpression <> [body] | CaseArm _ _ guardExpression body <- arms])
        EBinary node _ left right -> (coreNodeId node, [left, right])
        ESectionLeft node left _ -> (coreNodeId node, [left])
        ESectionRight node _ right -> (coreNodeId node, [right])
        EBlock node statements -> (coreNodeId node, concat [case statement of SExpr _ child -> [child]; SLet _ _ child -> [child]; _ -> [] | statement <- statements])
   in if nodeId == requested then Just expression else foldr ((<|>) . findExpression requested) Nothing children
