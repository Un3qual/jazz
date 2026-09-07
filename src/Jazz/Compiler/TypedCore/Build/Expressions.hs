{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Checked expression construction from immutable analyzed nodes. Expected
-- types belong to this construction context, not to the input node's facts.
module Jazz.Compiler.TypedCore.Build.Expressions
  ( ExpressionContext (..),
    ExpressionBinding (..),
    ExpressionPurpose (..),
    buildExpression,
    ExpressionFailure (..),
    productionFailure,
    sourceFailures,
    statementExpressions,
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
import Jazz.Compiler.AST (CaseArm (..), CoreNode (..), CoreNodeId, CorePhase (Analyzed), Expr (..), ImplMethod (..), Literal (..), Pattern (..), Statement (..), expressionNode)
import Jazz.Compiler.BuiltinCatalog (BuiltinResolutionMode (ResolveKernelOnly), BuiltinSymbol (BuiltinTextAppend, BuiltinTextAppendChar, BuiltinTextLength), builtinSymbolArity, builtinSymbolKernelName, lookupBuiltinSymbolInMode, numericTypeIntegerBounds, numericTypeIsIntegral)
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
import Jazz.Compiler.TypedCore.Build.StructuredValues

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

-- Source-form failures survive rejection of an enclosing statement. Failures
-- that require a concrete representation belong only to attempted construction.
data ExpressionFailure
  = SourceFormFailure TypedCoreProductionFailure
  | ConstructionFailure TypedCoreProductionFailure
  deriving (Eq, Show)

productionFailure :: ExpressionFailure -> TypedCoreProductionFailure
productionFailure (SourceFormFailure failureValue) = failureValue
productionFailure (ConstructionFailure failureValue) = failureValue

sourceFailures :: [ExpressionFailure] -> [ExpressionFailure]
sourceFailures = filter (\case SourceFormFailure _ -> True; _ -> False)

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

buildExpression :: StructuredValueCatalog -> ExpressionContext -> Expr 'Analyzed -> Either [ExpressionFailure] TypedExpr
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
      EVar _ name
        | Just constructor <- structuredConstructorAtStatement catalog (expressionStatementIndex context) name ->
            if null (structuredConstructorFieldContracts constructor)
              then case concreteConstructorContract catalog constructor selectedType of
                Just (_, TypedNodeInfo t recipe _ evidence, instantiations) -> Right (TypedVariableExpr (TypedNodeInfo t recipe instantiations evidence) (structuredConstructorName constructor) (Just (structuredConstructorBinder constructor)))
                Nothing -> reject TypedCoreUnresolvedExpressionType TypedCoreDataValueDetail
              else reject TypedCoreCallableValueUnsupported (TypedCoreNameDetail (identifierText name))
        | Just binding <- Map.lookup name (expressionBindings context) -> case (bindingCallable binding, expressionPurpose context) of
            (Just (TypedDirectCallableShape, _), ExpressionValue) -> reject TypedCoreCallableValueUnsupported (TypedCoreNameDetail (identifierText name))
            _ -> do
              info <- case bindingCallable binding of
                Just (shape, arity) -> maybe (reject TypedCoreUnresolvedExpressionType TypedCoreUnsupportedRootDetail) Right (callableTypeInfo catalog shape arity (bindingSemanticType binding))
                Nothing -> callableOrValueInfo (selectType facts (expressionExpectedType context <|> Just (bindingSemanticType binding)) (expressionSemanticType facts))
              pure (TypedVariableExpr info (valueName name) (Just (bindingOwner binding)))
        | Just _ <- textBuiltin name -> reject TypedCoreCallableValueUnsupported (TypedCoreNameDetail (identifierText name))
        | SemanticData {} <- selectedType -> reject TypedCoreStructuredValueUnsupported TypedCoreDataValueDetail
        | otherwise -> reject TypedCoreCaptureUnsupported (TypedCoreNameDetail (identifierText name))
      ELambda _ name body -> case selectedType of
        SemanticFunction argumentType resultType -> do
          let (shape, arity) = case expressionPurpose context of
                FunctionDefinition selectedShape remaining -> (selectedShape, remaining)
                _ -> (TypedClosureCallableShape, 1)
          let infoResult = maybe (reject TypedCoreUnresolvedExpressionType TypedCoreUnsupportedRootDetail) Right (callableTypeInfo catalog shape arity selectedType)
              binder = TypedBinderId (expressionModulePath context, expressionStatementIndex context : expressionChildPath context, valueName name)
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
          let bodyResult = buildExpression catalog bodyContext body
              duplicateFailures = [failure context TypedCoreDuplicateParameterUnsupported (TypedCoreNameDetail (identifierText name)) | FunctionDefinition {} <- [expressionPurpose context], Just previous <- [Map.lookup name (expressionBindings context)], TypedBinderId (_, index : _ : _, _) <- [bindingOwner previous], index == expressionStatementIndex context]
              retained = sourceFailures (errors bodyResult)
          case (infoResult, bodyResult, duplicateFailures) of
            (Right info, Right typedBody, []) -> Right (TypedLambdaExpr info binder (valueName name) typedBody)
            _ -> Left (if null retained then duplicateFailures <> errors infoResult <> (if null (errors infoResult) then errors bodyResult else []) else retained)
        _ -> reject TypedCoreUnresolvedExpressionType TypedCoreUnsupportedRootDetail
      EApply {} -> buildApplication
      EPatternCase _ scrutinee arms -> buildCase scrutinee arms
      EIf _ condition thenExpression elseExpression ->
        let info = callableOrValueInfo selectedType
            conditionResult = buildChild 0 (Just SemanticBool) condition
            thenResult = buildChild 1 (Just selectedType) thenExpression
            elseResult = buildChild 2 (Just selectedType) elseExpression
         in case (info, conditionResult, thenResult, elseResult) of
              (Right resultInfo, Right typedCondition, Right typedThen, Right typedElse) -> Right (TypedIfExpr resultInfo typedCondition typedThen typedElse)
              _ ->
                let childFailures = errors conditionResult <> errors thenResult <> errors elseResult
                    retained = sourceFailures childFailures
                 in Left (if null retained then errors info <> childFailures else retained)
      EList _ elements -> rejectWithChildren TypedCoreStructuredValueUnsupported TypedCoreListValueDetail elements
      EBlock _ statements ->
        let (kind, detail) = if any (\case SData {} -> True; _ -> False) statements then (TypedCoreStructuredValueUnsupported, TypedCoreDataValueDetail) else (TypedCoreNestedBlockUnsupported, TypedCoreLocalBlockDetail)
         in Left (sourceFailure context kind detail : concat [sourceFailures (errors (buildAt path Nothing ExpressionValue child)) | (path, child) <- statementExpressions statements])
      EBinary _ _ left right -> rejectWithChildren TypedCoreUserDefinedOperatorUnsupported TypedCoreUnsupportedRootDetail [left, right]
      EOperatorValue {} -> rejectSource TypedCoreUserDefinedOperatorUnsupported TypedCoreUnsupportedRootDetail
      ESectionLeft _ child _ -> rejectWithChildren TypedCoreUserDefinedOperatorUnsupported TypedCoreUnsupportedRootDetail [child]
      ESectionRight _ _ child -> rejectWithChildren TypedCoreUserDefinedOperatorUnsupported TypedCoreUnsupportedRootDetail [child]
      ETypeApplication _ function _ _ -> rejectWithChildren TypedCoreManagedValueUnsupported TypedCoreUnsupportedRootDetail [function]
  where
    facts = expressionFacts expression
    selectedType = selectType facts (expressionExpectedType context) (expressionSemanticType facts)
    valueName = TypedResolvedName TypedCurrentModule TypedValueNamespace . identifierText
    reject kind detail = Left [failure context kind detail]
    rejectSource kind detail = Left [sourceFailure context kind detail]
    buildChild index expected = buildExpression catalog context {expressionChildPath = expressionChildPath context <> [index], expressionExpectedType = expected, expressionPurpose = ExpressionValue}
    rejectWithChildren kind detail children =
      Left (sourceFailure context kind detail : concat [sourceFailures (errors (buildChild index Nothing child)) | (index, child) <- zip [0 ..] children])

    nodeInfo expressionType = case expressionType of
      SemanticFunction {} -> reject TypedCoreManagedValueUnsupported TypedCoreUnsupportedRootDetail
      SemanticList {} -> reject TypedCoreStructuredValueUnsupported TypedCoreListValueDetail
      SemanticVariable {} -> reject TypedCoreUnresolvedExpressionType TypedCoreUnsupportedRootDetail
      _ -> callableOrValueInfo expressionType

    callableOrValueInfo expressionType = case structuredNodeInfo catalog expressionType of
      Just info -> Right info
      Nothing -> reject TypedCoreStructuredValueUnsupported (case expressionType of SemanticTuple {} -> TypedCoreTupleValueDetail; _ -> TypedCoreDataValueDetail)

    buildBinary operation = case (findExpression (binaryOperationLeftOperand operation) expression, findExpression (binaryOperationRightOperand operation) expression) of
      (Just left, Just right)
        | SemanticVariable {} <- selectedType -> reject TypedCoreUnresolvedExpressionType TypedCoreUnsupportedRootDetail
        | otherwise -> case binaryOperationOperandTyping operation of
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

    buildApplication =
      let (callee, arguments, stages) = applicationSpine expression
          count = length arguments
          constructor = case callee of
            EVar _ name -> structuredConstructorAtStatement catalog (expressionStatementIndex context) name
            _ -> Nothing
          binding = case callee of
            EVar _ name -> Map.lookup name (expressionBindings context)
            _ -> Nothing
          builtin = case callee of
            EVar _ name | Nothing <- binding -> textBuiltin name
            _ -> Nothing
          calleeType = maybe (expressionSemanticType (expressionFacts callee)) bindingSemanticType binding
          contracts = case constructor of
            Just selected -> concreteConstructorContract catalog selected selectedType
            Nothing -> Nothing
          argumentTypes = case constructor >>= (`concreteConstructorFieldTypes` selectedType) of
            Just fields -> fields
            _ -> functionArguments calleeType
          argumentResults =
            [ buildAt
                path
                ( case constructor of
                    Just _ -> Just expected
                    Nothing -> case expressionSemanticType (expressionFacts argument) of
                      SemanticVariable {} -> Just expected
                      _ -> Nothing
                )
                ExpressionValue
                argument
            | ((path, argument), expected) <- zip arguments (argumentTypes <> repeat (SemanticTuple []))
            ]
          argumentFailures = concatMap errors argumentResults
          arityFailure expected = [failure context TypedCoreCallArityUnsupported (TypedCoreArityDetail expected count) | count /= expected]
          stagedInfos =
            collect
              [ infoAt path (selectType (expressionFacts stage) (if index == count - 1 then expressionExpectedType context else case expressionSemanticType (expressionFacts stage) of SemanticVariable {} -> Just expected; _ -> Nothing) (expressionSemanticType (expressionFacts stage)))
              | (index, ((path, stage), expected)) <- zip [0 ..] (zip stages (functionResults calleeType <> repeat selectedType))
              ]
          applyStages calleeResult infos = case (calleeResult, collect argumentResults, infos) of
            (Right typedCallee, Right typedArguments, Right resultInfos) -> Right (foldl' (\function (info, argument) -> TypedApplyExpr info function argument) typedCallee (zip resultInfos typedArguments))
            _ -> Left (errors calleeResult <> argumentFailures <> errors infos)
       in if not (null (sourceFailures argumentFailures))
            then Left (sourceFailures argumentFailures)
            else case constructor of
              Just selected
                | failures@(_ : _) <- arityFailure (length (structuredConstructorFieldContracts selected)) -> Left (failures <> argumentFailures)
                | Just (fields, resultInfo, instantiations) <- contracts ->
                    let constructorInfo = TypedNodeInfo (foldr (SemanticFunction . typedNodeType) (typedNodeType resultInfo) fields) (TypedClosureRecipe (map typedNodeRecipe fields) (typedNodeRecipe resultInfo)) instantiations []
                        resultInfoAt applied = case drop applied fields of
                          [] -> resultInfo
                          remaining -> TypedNodeInfo (foldr (SemanticFunction . typedNodeType) (typedNodeType resultInfo) remaining) (TypedClosureRecipe (map typedNodeRecipe remaining) (typedNodeRecipe resultInfo)) [] []
                     in applyStages (Right (TypedVariableExpr constructorInfo (structuredConstructorName selected) (Just (structuredConstructorBinder selected)))) (Right (map resultInfoAt [1 .. count]))
                | otherwise -> Left (failure context TypedCoreStructuredValueUnsupported TypedCoreDataValueDetail : argumentFailures)
              Nothing -> case builtin of
                Just symbol ->
                  let calleeInfo = maybe (reject TypedCoreUnresolvedExpressionType TypedCoreUnsupportedRootDetail) Right (callableTypeInfo catalog TypedDirectCallableShape (builtinSymbolArity symbol) calleeType)
                      calleeResult = (\info -> TypedVariableExpr info (TypedBuiltinName (builtinSymbolKernelName symbol)) Nothing) <$> calleeInfo
                   in case arityFailure (builtinSymbolArity symbol) of
                        [] -> applyStages calleeResult stagedInfos
                        failures -> Left (failures <> errors calleeResult <> argumentFailures)
                Nothing -> case callee of
                  EVar _ name | Nothing <- binding -> Left (failure context TypedCoreNonLocalCallUnsupported (TypedCoreNameDetail (identifierText name)) : argumentFailures)
                  _ ->
                    let calleePath = case binding of
                          Just selected | Just _ <- bindingCallable selected, TypedBinderId (_, [_], _) <- bindingOwner selected -> []
                          _ -> replicate count 0
                     in applyStages (buildAt calleePath Nothing ExpressionCallee callee) stagedInfos

    buildAt path expected purpose = buildExpression catalog context {expressionChildPath = expressionChildPath context <> path, expressionExpectedType = expected, expressionPurpose = purpose}
    infoAt path selected = case structuredNodeInfo catalog selected of
      Just info -> Right info
      Nothing -> Left [failure context {expressionChildPath = expressionChildPath context <> path} TypedCoreUnresolvedExpressionType TypedCoreUnsupportedRootDetail]

    buildCase scrutinee arms =
      let scrutineeResult = buildChild 0 Nothing scrutinee
          scrutineeType = selectType (expressionFacts scrutinee) Nothing (expressionSemanticType (expressionFacts scrutinee))
          supportedScalar = case scrutineeType of
            SemanticInt -> True
            SemanticFloat -> True
            SemanticNumeric {} -> True
            SemanticBool -> True
            SemanticChar -> True
            SemanticTuple [] -> True
            _ -> False
          supportedManaged = case scrutineeType of SemanticTuple (_ : _) -> True; SemanticData {} -> True; _ -> False
          scalarArms = case reverse arms of
            CaseArm _ finalPattern Nothing _ : preceding -> catchAll finalPattern && all (\(CaseArm _ pattern guardExpression _) -> scalarPattern pattern && (not (catchAll pattern) || maybe False (const True) guardExpression)) preceding
            _ -> False
          managedArms = supportedManaged && any (\(CaseArm _ pattern _ _) -> managedPattern pattern) arms
          profileFailures = [sourceFailure context TypedCorePatternCaseUnsupported TypedCorePatternCaseDetail | not ((supportedScalar && scalarArms) || managedArms)]
          armResults = [buildArm scrutineeType index arm | (index, arm) <- zip [0 ..] arms]
          rejectedArmFailures =
            concat
              [ sourceFailures (concatMap errors [buildAt [index + 1, 0] (Just SemanticBool) ExpressionValue guardExpr | Just guardExpr <- [guardExpression]] <> errors (buildAt [index + 1, 1] (Just selectedType) ExpressionValue body))
              | (index, CaseArm _ _ guardExpression body) <- zip [0 ..] arms
              ]
          childFailures = errors scrutineeResult <> concatMap errors armResults
       in if not (null profileFailures)
            then Left (sourceFailures (errors scrutineeResult) <> rejectedArmFailures <> profileFailures)
            else case (profileFailures, callableOrValueInfo selectedType, scrutineeResult, collect armResults) of
              ([], Right info, Right typedScrutinee, Right typedArms) -> Right (TypedPatternCaseExpr info typedScrutinee typedArms)
              _ -> Left (childFailures <> profileFailures <> [err | null childFailures && null profileFailures, err <- errors (callableOrValueInfo selectedType)])

    buildArm scrutineeType index (CaseArm _ pattern guardExpression body) = do
      let owners = Map.fromListWith (\_ first -> first) [(name, TypedBinderId (expressionModulePath context, expressionStatementIndex context : (expressionChildPath context <> path), valueName name)) | (path, name) <- patternBinderPaths [index] pattern]
      (typedPattern, bindings) <- buildPattern owners True [index] scrutineeType pattern
      let armContext = context {expressionBindings = Map.union bindings (expressionBindings context), expressionPurpose = ExpressionValue}
          guardResult = traverse (buildExpression catalog armContext {expressionChildPath = expressionChildPath context <> [index + 1, 0], expressionExpectedType = Just SemanticBool}) guardExpression
          bodyResult = buildExpression catalog armContext {expressionChildPath = expressionChildPath context <> [index + 1, 1], expressionExpectedType = Just selectedType} body
      case (guardResult, bodyResult) of
        (Right typedGuard, Right typedBody) -> Right (TypedCaseArm typedPattern typedGuard typedBody)
        _ -> Left (errors guardResult <> errors bodyResult)

    buildPattern owners isRoot path patternType pattern =
      let unsupported = Left [sourceFailure context {expressionChildPath = expressionChildPath context <> path} TypedCorePatternCaseUnsupported TypedCorePatternCaseDetail]
          owner name = Map.findWithDefault (TypedBinderId (expressionModulePath context, expressionStatementIndex context : (expressionChildPath context <> path), valueName name)) name owners
          binding name = Map.singleton name (ExpressionBinding (owner name) patternType Nothing)
          children types nested construct
            | length types == length nested = do
                results <- collect [buildPattern owners False (path <> [index]) t p | (index, (t, p)) <- zip [0 ..] (zip types nested)]
                pure (construct (map fst results), Map.unions (map snd results))
            | otherwise = unsupported
       in case structuredNodeInfo catalog patternType of
            Nothing -> unsupported
            Just info -> case pattern of
              PWildcard _ -> Right (TypedWildcardPattern info, Map.empty)
              PVariable _ name -> Right (TypedVariablePattern info (owner name) (valueName name), binding name)
              PLiteral _ LText {} -> unsupported
              PLiteral _ literal -> (\value -> (TypedLiteralPattern info value, Map.empty)) <$> typedLiteral info literal
              PTuple _ nested | SemanticTuple types <- patternType -> children types nested (TypedTuplePattern info)
              PConstructor _ name nested | Just constructor <- structuredConstructorAtStatement catalog (expressionStatementIndex context) name, Just fields <- concreteConstructorFieldTypes constructor patternType -> children fields nested (TypedConstructorPattern info (structuredConstructorName constructor))
              PAs _ name nested -> do
                (typedNested, bindings) <- buildPattern owners False (path <> [0]) patternType nested
                pure (TypedAsPattern info (owner name) (valueName name) typedNested, Map.union (binding name) bindings)
              POr _ alternatives | isRoot -> children (replicate (length alternatives) patternType) alternatives (TypedOrPattern info)
              _ -> unsupported

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

failure :: ExpressionContext -> TypedCoreProductionFailureKind -> TypedCoreProductionFailureDetail -> ExpressionFailure
failure context kind detail = ConstructionFailure (TypedCoreProductionFailure (TypedCoreProductionExpressionPath (expressionModulePath context) (expressionStatementIndex context) (expressionChildPath context)) kind detail)

sourceFailure :: ExpressionContext -> TypedCoreProductionFailureKind -> TypedCoreProductionFailureDetail -> ExpressionFailure
sourceFailure context kind detail = SourceFormFailure (productionFailure (failure context kind detail))

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
expressionFacts = coreNodeFacts . expressionNode

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

textBuiltin :: ResolvedName -> Maybe BuiltinSymbol
textBuiltin name = case lookupBuiltinSymbolInMode ResolveKernelOnly (identifierText name) of
  Just symbol@BuiltinTextLength -> Just symbol
  Just symbol@BuiltinTextAppend -> Just symbol
  Just symbol@BuiltinTextAppendChar -> Just symbol
  _ -> Nothing

applicationSpine :: Expr 'Analyzed -> (Expr 'Analyzed, [([Int], Expr 'Analyzed)], [([Int], Expr 'Analyzed)])
applicationSpine = go [] [] []
  where
    go path arguments stages expression = case expression of
      EApply _ function argument -> go (path <> [0]) ((path <> [1], argument) : arguments) ((path, expression) : stages) function
      _ -> (expression, arguments, stages)

functionArguments :: AnalyzedType -> [AnalyzedType]
functionArguments (SemanticFunction argument result) = argument : functionArguments result
functionArguments _ = []

functionResults :: AnalyzedType -> [AnalyzedType]
functionResults (SemanticFunction _ result) = result : functionResults result
functionResults _ = []

catchAll :: Pattern 'Analyzed -> Bool
catchAll PWildcard {} = True
catchAll PVariable {} = True
catchAll _ = False

scalarPattern :: Pattern 'Analyzed -> Bool
scalarPattern PLiteral {} = True
scalarPattern pattern = catchAll pattern

managedPattern :: Pattern 'Analyzed -> Bool
managedPattern pattern = case pattern of
  PConstructor _ _ (_ : _) -> True
  PTuple _ (_ : _) -> True
  PAs _ _ nested -> managedPattern nested
  POr _ alternatives -> any managedPattern alternatives
  _ -> False

-- Paths through an unsupported block or implementation retain the authored
-- statement index even though no typed statement can be emitted there.
statementExpressions :: [Statement 'Analyzed] -> [([Int], Expr 'Analyzed)]
statementExpressions statements =
  concat
    [ case statement of
        SExpr _ expression -> [([index], expression)]
        SLet _ _ expression -> [([index], expression)]
        SImpl _ _ _ methods -> [([index, methodIndex], expression) | (methodIndex, ImplMethod _ _ expression) <- zip [0 ..] methods]
        _ -> []
    | (index, statement) <- zip [0 ..] statements
    ]

patternBinderPaths :: [Int] -> Pattern 'Analyzed -> [([Int], ResolvedName)]
patternBinderPaths path pattern = case pattern of
  PVariable _ name -> [(path, name)]
  PAs _ name nested -> (path, name) : patternBinderPaths (path <> [0]) nested
  PConstructor _ _ nested -> children nested
  PTuple _ nested -> children nested
  POr _ alternatives -> children alternatives
  PList _ nested -> children nested
  PConsList _ headPattern tailPattern -> children [headPattern, tailPattern]
  _ -> []
  where
    children nested = concat [patternBinderPaths (path <> [index]) child | (index, child) <- zip [0 ..] nested]
