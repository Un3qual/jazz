{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Check resolved expressions and normalized declarations. Diagnostic phase
-- ordering and warning policy belong to the module analysis coordinator.
module Jazz.Compiler.TypeInference
  ( InferenceInputs (..),
    CheckedExpr (..),
    InferenceSubject (..),
    inferenceSubjectExpr,
    inferExpressionWork,
    moduleInterfaceFromState,
  )
where

import Data.Bifunctor (first)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Jazz.Compiler.AST
  ( CoreNode,
    CoreNodeId,
    CorePhase (..),
    CoreSort (StatementSort),
    DataConstructor (..),
    Expr (..),
    Literal (..),
    Statement (..),
    coreNodeFacts,
    coreNodeId,
    coreNodeSpan,
    expressionNode,
  )
import Jazz.Compiler.BuiltinCatalog
  ( BuiltinSymbol (BuiltinListPrependRaw),
    builtinSymbolName,
    builtinSymbolNumericConversionTarget,
    lookupKernelBuiltinSymbol,
    numericTypeFloatMax,
    numericTypeIntegerBounds,
    numericTypeLiteralIntegerBounds,
  )
import Jazz.Compiler.CoreIdentity (CapabilityMethodKey, CoreBinderId, ResolvedNodeFacts (..), ResolvedReference (..), capabilityMethodKeyFromReference, resolvedValueReference)
import Jazz.Compiler.Diagnostics (Diagnostic, SourceSpan)
import Jazz.Compiler.FractionalLiteral
  ( FractionalLiteralSource,
    fractionalLiteralExceedsMagnitude,
    fractionalLiteralIntegralValue,
  )
import Jazz.Compiler.ModuleExports (ModuleExportInventory)
import Jazz.Compiler.ModuleIdentity (ModulePath)
import Jazz.Compiler.ModuleInterface
  ( ModuleInterface (..),
    emptyModuleInterface,
    moduleExportForBinding,
    publishModuleInterface,
  )
import Jazz.Compiler.Name
  ( Name (..),
    ResolvedName,
    UnresolvedName,
    identifierText,
    operatorBindingName,
    renderName,
  )
import Jazz.Compiler.Parser.Operator
  ( isBuiltinOperatorSymbol,
  )
import Jazz.Compiler.PatternCoverage
  ( PatternCoverageSite (..),
    constructorInventoryFromBindingsWithWitnessNames,
  )
import Jazz.Compiler.RecursiveBindings
  ( PreparedRecursiveScope,
    prepareResolvedScope,
    preparedRecursiveScopeStatements,
  )
import Jazz.Compiler.SemanticDeclarations (DeclarationVariable)
import Jazz.Compiler.SemanticFacts
  ( BinaryOperation (..),
    StatementDeclarationFact,
  )
import Jazz.Compiler.TypeInference.Analyzed (draftExpressionNode, legacyExpressionDraft, refineListPrependDraft)
import Jazz.Compiler.TypeInference.Capabilities
import Jazz.Compiler.TypeInference.Diagnostics
import Jazz.Compiler.TypeInference.Draft (CheckedExpr (..))
import Jazz.Compiler.TypeInference.Environment (insertResolvedTypeBinding)
import Jazz.Compiler.TypeInference.Interface (closeModuleBindings, importBindingTypes)
import Jazz.Compiler.TypeInference.Operator
  ( applyOperatorAliasSchemeConstraints,
    builtinSectionOperatorSymbol,
    hasOperatorRule,
    inferBinaryType,
    inferSectionLeftType,
    inferSectionRightType,
    instantiateOperatorType,
  )
import Jazz.Compiler.TypeInference.Pattern
  ( inferPatternCaseType,
  )
import Jazz.Compiler.TypeInference.Scope
  ( inferExplicitTypeApplication,
    inferNestedScopeTypeWithMode,
    inferScopeTypeWithModeAndForwardBindingsUsingPreparedScope,
    instantiateNonBuiltinTypeBinding,
  )
import Jazz.Compiler.TypeInference.Solver
  ( addNumericTypeVarConstraint,
    freshIntegerLiteralType,
    freshTypeVar,
    freshTypeVariable,
    resolveType,
    unifyTypes,
  )
import Jazz.Compiler.TypeInference.State
  ( DeclarationState (..),
    InferState (..),
    InferenceOutput (..),
    ModuleInferenceState (..),
    inferConstructorWitnessNames,
    inferDataTypes,
    inferExpressionFactTypes,
    inferModuleCapabilityFacts,
    inferVisibleTypes,
    initialInferState,
    modifyInferenceOutput,
    recordBinaryOperation,
    recordExpressionFactType,
    recordPatternCoverageSite,
    recordStatementFactSeed,
    reservePatternCoverageSite,
  )
import Jazz.Compiler.TypeInference.Traversal (InferenceMode (..))
import Jazz.Compiler.TypeInference.Types
  ( DataTypeBinding,
    ExpressionType,
    IntegerLiteralRange (..),
    NumericConstraint (..),
    ScopeCapabilityFacts (..),
    SemanticBinding (..),
    SemanticType (..),
    TypeBinding,
    TypeEnv,
    TypeEnvKey (..),
    TypeScheme,
    emptyScopeCapabilityFacts,
    typeEnvReferenceKey,
  )
import Jazz.Compiler.TypeRepresentation (NumericType (..))
import Jazz.Compiler.WarningConfig (WarningSettings)

data InferenceInputs = InferenceInputs
  { inferencePublicExports :: Maybe ModuleExportInventory,
    inferenceWarningSettings :: WarningSettings,
    inferenceExternalUses :: Set CoreBinderId,
    inferenceImportedTypes :: Map TypeEnvKey (SemanticBinding DeclarationVariable),
    inferenceImportedDataTypes :: Map ResolvedName DataTypeBinding,
    inferenceImportedConstructorWitnessNames :: Map ResolvedName UnresolvedName,
    inferenceImportedCapabilities :: ScopeCapabilityFacts,
    inferenceImportedClassNames :: Set Text,
    inferenceCurrentModulePath :: Maybe ModulePath
  }

data InferenceSubject
  = InferenceExpression (Expr 'Resolved)
  | InferencePreparedScope (Expr 'Resolved) (PreparedRecursiveScope 'Resolved)

inferenceSubjectExpr :: InferenceSubject -> Expr 'Resolved
inferenceSubjectExpr subject =
  case subject of
    InferenceExpression expr -> expr
    InferencePreparedScope expr preparedScope ->
      preparedRecursiveScopeStatements preparedScope `seq` expr

inferExpressionWork :: InferenceInputs -> [(CoreNode 'Resolved 'StatementSort, StatementDeclarationFact)] -> Expr 'Resolved -> (CheckedExpr, InferState, InferenceSubject)
inferExpressionWork inputs moduleStatementFacts expr =
  let (importedEnvironment, importedState) = importBindingTypes (inferenceImportedTypes inputs) (initialStateForInference inputs)
      initialState =
        foldl'
          (\state (node, declarationFact) -> recordStatementFactSeed (coreNodeId node) ([], declarationFact) state)
          importedState
          moduleStatementFacts
   in case expr of
        EBlock node statements ->
          let preparedScope =
                prepareResolvedScope node statements
              (blockResult, rawBlockState, _) =
                inferScopeTypeWithModeAndForwardBindingsUsingPreparedScope
                  preparedScope
                  inferExprTypeWithMode
                  InferenceOnly
                  importedEnvironment
                  initialState
              blockState =
                recordExpressionFactType
                  (coreNodeId (expressionNode expr))
                  ( case blockResult of
                      Just expressionType -> expressionType
                      Nothing -> unitType
                  )
                  rawBlockState
           in (CheckedExpr blockResult (legacyExpressionDraft expr), blockState, InferencePreparedScope expr preparedScope)
        _ ->
          let (result, resultState) =
                inferExprTypeDetailed
                  importedEnvironment
                  initialState
                  expr
           in (result, resultState, InferenceExpression expr)

initialStateForInference :: InferenceInputs -> InferState
initialStateForInference inputs =
  applyCapabilityFacts
    (inferenceImportedCapabilities inputs)
    initialInferState
      { inferDeclarations =
          (inferDeclarations initialInferState)
            { declarationDataTypes = inferenceImportedDataTypes inputs
            },
        inferModule =
          (inferModule initialInferState)
            { inferenceModulePath = inferenceCurrentModulePath inputs,
              inferenceConstructorWitnessNames =
                inferenceImportedConstructorWitnessNames inputs
            }
      }

moduleInterfaceFromState :: InferenceInputs -> Expr 'Resolved -> InferState -> ModuleInterface
moduleInterfaceFromState inputs expr state =
  publishModuleInterface (inferencePublicExports inputs) (inferDataTypes state) $
    emptyModuleInterface
      { interfaceValueBindings =
          closeModuleBindings
            state
            [ (moduleExportForBinding (renderName name) binding, binder, binding)
            | (name, binder) <- Map.toList declaredValues,
              Just binding <- [Map.lookup (TypeEnvKey (LexicalReference binder) name) (inferVisibleTypes state)]
            ],
        interfaceDataTypes = Map.restrictKeys (inferDataTypes state) declaredDataTypes,
        interfaceClassFacts = scopeClassFacts localCapabilities,
        interfaceGeneratedEqualityClassFacts = scopeGeneratedEqualityClassFacts localCapabilities,
        interfaceConcreteImplFacts = scopeConcreteImplFacts localCapabilities,
        interfaceClassMethods = scopeClassMethodSignatures localCapabilities,
        interfaceConcreteImplMethods = scopeConcreteImplMethods localCapabilities
      }
  where
    (declaredValues, declaredDataTypes) = declaredModuleBindings expr
    localCapabilities =
      Map.findWithDefault emptyScopeCapabilityFacts (inferenceCurrentModulePath inputs) (inferModuleCapabilityFacts state)

declaredModuleBindings :: Expr 'Resolved -> (Map ResolvedName CoreBinderId, Set ResolvedName)
declaredModuleBindings expression =
  case expression of
    EBlock _ statements -> foldl' collect (Map.empty, Set.empty) statements
    _ -> (Map.empty, Set.empty)
  where
    collect :: (Map ResolvedName CoreBinderId, Set ResolvedName) -> Statement 'Resolved -> (Map ResolvedName CoreBinderId, Set ResolvedName)
    collect (valueNames, dataTypeNames) statement =
      case statement of
        SLet node name _
          | publicModuleValue name -> (insertBinder node name valueNames, dataTypeNames)
          | otherwise -> (valueNames, dataTypeNames)
        SData _ typeName _ constructors ->
          ( foldl'
              (\names (DataConstructor node constructorName _) -> insertBinder node constructorName names)
              valueNames
              constructors,
            Set.insert typeName dataTypeNames
          )
        _ -> (valueNames, dataTypeNames)

    insertBinder node name bindings = maybe bindings (\binder -> Map.insert name binder bindings) (resolvedNodeBinder (coreNodeFacts node))

    publicModuleValue name =
      case name of
        GeneratedName {} -> False
        _ -> True

instantiateEnvBinding :: TypeBinding -> InferState -> (Maybe ExpressionType, InferState)
instantiateEnvBinding binding state =
  case binding of
    BuiltinAliasTypeBinding builtinSymbol ->
      case instantiateBuiltinSymbolType builtinSymbol state of
        Just (expressionType, nextState) -> (Just expressionType, nextState)
        Nothing -> (Nothing, state)
    BuiltinOperatorAliasTypeBinding operatorSymbol ->
      case instantiateOperatorType operatorSymbol state of
        Just (expressionType, nextState) -> (Just expressionType, nextState)
        Nothing -> (Nothing, state)
    _ -> instantiateNonBuiltinTypeBinding binding state

inferExprTypeWithMode :: InferenceMode -> TypeEnv -> InferState -> Expr 'Resolved -> (CheckedExpr, InferState)
inferExprTypeWithMode mode env state expr = case expr of
  EBlock node statements ->
    let (result, inferredState) = inferNestedScopeTypeWithMode inferExprTypeWithMode mode env state (prepareResolvedScope node statements)
        finalState = maybe inferredState (\value -> recordExpressionFactType (coreNodeId node) value inferredState) result
     in (CheckedExpr result (legacyExpressionDraft expr), finalState)
  _ -> inferExprTypeDetailed env state expr

-- Checking returns the draft subtree alongside its type. Legacy constructor
-- families temporarily retain attachment until their own children are migrated.
inferExprTypeDetailed :: TypeEnv -> InferState -> Expr 'Resolved -> (CheckedExpr, InferState)
inferExprTypeDetailed env state expr = case expr of
  ELit _ literal -> leaf (\node -> ELit node literal)
  EVar _ name -> leaf (\node -> EVar node name)
  EOperatorValue _ symbol -> leaf (\node -> EOperatorValue node symbol)
  ETuple _ [] -> leaf (\node -> ETuple node [])
  EIf _ condition thenExpression elseExpression ->
    let (conditionCheck, stateAfterCondition) = inferExprTypeDetailed env state condition
        (thenCheck, stateAfterThen) = inferExprTypeDetailed env stateAfterCondition thenExpression
        (elseCheck, stateAfterElse) = inferExprTypeDetailed env stateAfterThen elseExpression
        (result, finalState) = inferIfFromResults (checkedExprType conditionCheck) (checkedExprType thenCheck) (checkedExprType elseCheck) stateAfterElse
     in finish result finalState (\node -> EIf <$> node <*> checkedExprTree conditionCheck <*> checkedExprTree thenCheck <*> checkedExprTree elseCheck)
  EList _ elements ->
    let (result, children, finalState) = inferListElements env state elements
     in finish result finalState (\node -> EList <$> node <*> traverse checkedExprTree children)
  ETuple _ elements ->
    let (result, children, finalState) = inferTupleElements env state elements
     in finish result finalState (\node -> ETuple <$> node <*> traverse checkedExprTree children)
  EBinary _ symbol left right -> inferBinaryExpression symbol left right
  ESectionLeft _ left symbol -> inferLeftSection symbol left
  ESectionRight _ symbol right -> inferRightSection symbol right
  EApply _ function argument
    | Nothing <- builtinOperatorApplicationSpine env expr,
      not (qualifiedApplication env state expr) ->
        let (checked, finalState) = inferCheckedApplication env state expr function argument
         in (checked, recordType (checkedExprType checked) finalState)
  ELambda node name body ->
    let (parameterType, stateAfterParameter) = freshTypeVar state
        (bodyCheck, finalState) = inferExprTypeDetailed (insertResolvedTypeBinding (coreNodeFacts node) name (PlainTypeBinding parameterType) env) stateAfterParameter body
        result = SemanticFunction (resolveType finalState parameterType) <$> checkedExprType bodyCheck
     in finish result finalState (\facts -> ELambda <$> facts <*> pure name <*> checkedExprTree bodyCheck)
  _ ->
    let (result, inferredState) = inferExprTypeDetailedRaw env state expr
     in (CheckedExpr result (legacyExpressionDraft expr), recordType result inferredState)
  where
    leaf make =
      let (result, finalState) = inferExprTypeDetailedRaw env state expr
       in finish result finalState (fmap make)
    finish result finalState make =
      (CheckedExpr result (make (draftExpressionNode finalState result expr)), recordType result finalState)
    recordType result finalState = maybe finalState (\value -> recordExpressionFactType (coreNodeId (expressionNode expr)) value finalState) result

    selectedBinaryOperation operatorSymbol leftExpr rightExpr operandTyping =
      BinaryOperation
        operatorSymbol
        operandTyping
        (coreNodeId (expressionNode leftExpr))
        (coreNodeId (expressionNode rightExpr))

    recordSelectedBinaryOperation operation finalState =
      maybe
        finalState
        (\selected -> recordBinaryOperation (coreNodeId (expressionNode expr)) selected finalState)
        operation

    inferBinaryExpression operatorSymbol leftExpr rightExpr
      | hasOperatorRule operatorSymbol || isBuiltinOperatorSymbol operatorSymbol =
          let leftResult = checkedExprType leftCheck
              rightResult = checkedExprType rightCheck
              (leftCheck, stateAfterLeft) =
                inferExprTypeDetailed env state leftExpr
              (rightCheck, stateAfterRight) =
                inferExprTypeDetailed env stateAfterLeft rightExpr
              (expressionType, operandTyping, finalState) =
                case (leftResult, rightResult) of
                  (Just leftType, Just rightType) ->
                    inferBinaryType
                      operatorSymbol
                      leftExpr
                      rightExpr
                      leftType
                      rightType
                      stateAfterRight
                  _ -> (Nothing, Nothing, stateAfterRight)
              operation = selectedBinaryOperation operatorSymbol leftExpr rightExpr <$> operandTyping
           in finish expressionType (recordSelectedBinaryOperation operation finalState) (\node -> EBinary <$> node <*> pure operatorSymbol <*> checkedExprTree leftCheck <*> checkedExprTree rightCheck)
      | otherwise =
          inferDeclaredBinaryExpression env state operatorSymbol leftExpr rightExpr

    inferDeclaredBinaryExpression currentEnv initialState operatorSymbol leftExpr rightExpr =
      let leftResult = checkedExprType leftCheck
          rightResult = checkedExprType rightCheck
          (operatorType, stateAfterOperator) =
            instantiateDeclaredOperatorBindingType currentEnv (coreNodeFacts (expressionNode expr)) operatorSymbol initialState
          operatorResult = operatorType
          (leftCheck, stateAfterLeft) =
            inferExprTypeDetailed currentEnv stateAfterOperator leftExpr
          (intermediateType, stateAfterFirstApplication) =
            inferApplicationFromResultsUnchecked
              initialState
              operatorResult
              leftResult
              stateAfterLeft
          intermediateResult = intermediateType
          (rightCheck, stateAfterRight) =
            inferExprTypeDetailed currentEnv stateAfterFirstApplication rightExpr
          (expressionType, finalState) =
            inferApplicationFromResultsUnchecked
              stateAfterFirstApplication
              intermediateResult
              rightResult
              stateAfterRight
       in finish expressionType finalState (\node -> EBinary <$> node <*> pure operatorSymbol <*> checkedExprTree leftCheck <*> checkedExprTree rightCheck)

    inferLeftSection operatorSymbol leftExpr
      | hasOperatorRule operatorSymbol || isBuiltinOperatorSymbol operatorSymbol =
          let (leftCheck, stateAfterLeft) =
                inferExprTypeDetailed env state leftExpr
              (expressionType, finalState) =
                case checkedExprType leftCheck of
                  Just leftType ->
                    inferSectionLeftType operatorSymbol leftType stateAfterLeft
                  Nothing -> (Nothing, stateAfterLeft)
           in finish expressionType finalState (\node -> ESectionLeft <$> node <*> checkedExprTree leftCheck <*> pure operatorSymbol)
      | otherwise =
          let (operatorType, stateAfterOperator) =
                instantiateDeclaredOperatorBindingType env (coreNodeFacts (expressionNode expr)) operatorSymbol state
              operatorResult = operatorType
              (leftCheck, stateAfterLeft) =
                inferExprTypeDetailed env stateAfterOperator leftExpr
              (expressionType, finalState) =
                inferApplicationFromResultsUnchecked
                  state
                  operatorResult
                  (checkedExprType leftCheck)
                  stateAfterLeft
           in finish expressionType finalState (\node -> ESectionLeft <$> node <*> checkedExprTree leftCheck <*> pure operatorSymbol)

    inferRightSection operatorSymbol rightExpr
      | hasOperatorRule operatorSymbol || isBuiltinOperatorSymbol operatorSymbol =
          let (rightCheck, stateAfterRight) =
                inferExprTypeDetailed env state rightExpr
              (expressionType, finalState) =
                case checkedExprType rightCheck of
                  Just rightType ->
                    inferSectionRightType operatorSymbol rightType stateAfterRight
                  Nothing -> (Nothing, stateAfterRight)
           in finish expressionType finalState (\node -> ESectionRight <$> node <*> pure operatorSymbol <*> checkedExprTree rightCheck)
      | otherwise =
          let (leftType, stateAfterLeftType) = freshTypeVar state
              (operatorType, stateAfterOperator) =
                instantiateDeclaredOperatorBindingType env (coreNodeFacts (expressionNode expr)) operatorSymbol stateAfterLeftType
              operatorResult = operatorType
              leftResult = Just leftType
              (intermediateType, stateAfterFirstApplication) =
                inferApplicationFromResultsUnchecked
                  stateAfterLeftType
                  operatorResult
                  leftResult
                  stateAfterOperator
              intermediateResult = intermediateType
              (rightCheck, stateAfterRight) =
                inferExprTypeDetailed env stateAfterFirstApplication rightExpr
              (bodyType, finalState) =
                inferApplicationFromResultsUnchecked
                  stateAfterFirstApplication
                  intermediateResult
                  (checkedExprType rightCheck)
                  stateAfterRight
              expressionType =
                SemanticFunction (resolveType finalState leftType)
                  <$> bodyType
           in finish expressionType finalState (\node -> ESectionRight <$> node <*> pure operatorSymbol <*> checkedExprTree rightCheck)

inferExprTypeDetailedType :: TypeEnv -> InferState -> Expr 'Resolved -> (Maybe ExpressionType, InferState)
inferExprTypeDetailedType env state expression = first checkedExprType (inferExprTypeDetailed env state expression)

inferExprTypeDetailedRaw ::
  TypeEnv ->
  InferState ->
  Expr 'Resolved ->
  (Maybe ExpressionType, InferState)
inferExprTypeDetailedRaw env state expr =
  case expr of
    ELit _ literal ->
      let (literalType, stateAfterLiteral) = literalExpressionType literal state
       in (Just literalType, checkLiteralType stateAfterLiteral literal)
    ETuple _ [] -> (Just (SemanticTuple []), state)
    EBinary {} -> inferExprTypeDetailedType env state expr
    EIf {} -> inferExprTypeDetailedType env state expr
    EPatternCase _ scrutinee caseArms ->
      let (coverageOrdinal, stateWithOrdinal) = reservePatternCoverageSite state
          (scrutineeResult, stateAfterScrutinee) = inferExprTypeDetailedType env stateWithOrdinal scrutinee
          (scrutineeType, stateWithScrutineeType) = case scrutineeResult of
            Just inferredType -> (inferredType, stateAfterScrutinee)
            Nothing -> freshTypeVar stateAfterScrutinee
          (expressionType, inferredFinalState) = inferPatternCaseType inferExprTypeDetailed env scrutineeType stateWithScrutineeType caseArms
          finalState =
            recordPatternCoverageSite
              ( PatternCoverageSite
                  { patternCoverageSiteOrdinal = coverageOrdinal,
                    patternCoverageSiteConstructorInventory = constructorInventoryFromBindingsWithWitnessNames (inferConstructorWitnessNames inferredFinalState) (inferDataTypes inferredFinalState) env,
                    patternCoverageSiteScrutineeType = scrutineeType,
                    patternCoverageSiteArms = caseArms
                  }
              )
              inferredFinalState
       in (expressionType, finalState)
    EList {} -> inferExprTypeDetailedType env state expr
    ETuple _ (_ : _) -> inferExprTypeDetailedType env state expr
    EBlock node statements -> inferNestedScopeTypeWithMode inferExprTypeWithMode InferConcreteFunctions env state (prepareResolvedScope node statements)
    EVar node name ->
      let (expressionType, finalState) = inferVariableType node name state
       in (expressionType, annotateNewErrorsWithPrimarySpan (coreNodeSpan node) state finalState)
    ELambda node name body ->
      let (parameterType, stateAfterParameter) = freshTypeVar state
          (bodyResult, finalState) = inferExprTypeDetailedType (insertResolvedTypeBinding (coreNodeFacts node) name (PlainTypeBinding parameterType) env) stateAfterParameter body
          expressionType = SemanticFunction (resolveType finalState parameterType) <$> bodyResult
       in (expressionType, finalState)
    EOperatorValue {} ->
      let (expressionType, finalState) = inferLeafType expr state
       in (expressionType, finalState)
    EApply _ function argument
      | Just (symbol, aliasScheme, left, right, sectionFallback) <- builtinOperatorApplicationSpine env expr ->
          if sectionFallback then inferSectionApplicationWithFallback function argument symbol left right else inferBuiltinOperatorApplication symbol aliasScheme left right
      | Just (methodName, methodSpan, methodKey, arguments) <- qualifiedMethodApplicationSpine expr state,
        Map.notMember methodName env ->
          let (expressionType, finalState, argumentResults) = inferQualifiedMethodApplicationWithResults inferLocatedMethodArgument env state (coreNodeId (expressionNode expr)) methodKey arguments
              stateWithSpineFacts = case (expressionType, sequenceA argumentResults) of
                (Just resultType, Just argumentTypes) -> recordQualifiedMethodSpineFacts expr (foldr SemanticFunction (resolveType finalState resultType) (map (resolveType finalState) argumentTypes)) finalState
                _ -> finalState
           in (expressionType, annotateNewErrorsWithPrimarySpan methodSpan state stateWithSpineFacts)
      | otherwise -> inferGenericApplication function argument
    ETypeApplication node function argumentSpan argument ->
      inferExplicitTypeApplication inferExprTypeDetailed env state (coreNodeId node) function argumentSpan argument
    ESectionLeft {} -> inferExprTypeDetailedType env state expr
    ESectionRight {} -> inferExprTypeDetailedType env state expr
  where
    inferLocatedMethodArgument argumentEnv priorState argumentExpr =
      let (argumentType, nextState) = inferExprTypeDetailed argumentEnv priorState argumentExpr
       in (argumentType, annotateNewErrorsWithPrimarySpan (coreNodeSpan (expressionNode argumentExpr)) priorState nextState)

    inferVariableType node name initialState =
      case Map.lookup (typeEnvReferenceKey (coreNodeFacts node) name) env of
        Just localType -> instantiateEnvBinding localType initialState
        Nothing ->
          case instantiateBuiltinType (resolvedValueReference (coreNodeFacts node)) initialState of
            Just (builtinType, nextState) -> (Just builtinType, nextState)
            Nothing ->
              case instantiateQualifiedMethodType (coreNodeId node) (resolvedValueReference (coreNodeFacts node)) initialState of
                Just qualifiedMethodResult -> qualifiedMethodResult
                Nothing -> (Nothing, initialState)

    inferLeafType unsupportedExpr initialState =
      case unsupportedExpr of
        ELit _ literal ->
          let (literalType, stateAfterLiteral) = literalExpressionType literal initialState
           in (Just literalType, checkLiteralType stateAfterLiteral literal)
        EVar node name -> inferVariableType node name initialState
        EOperatorValue node operatorSymbol ->
          case instantiateOperatorType operatorSymbol initialState of
            Just (operatorType, nextState) -> (Just operatorType, nextState)
            Nothing
              | isBuiltinOperatorSymbol operatorSymbol ->
                  (Nothing, addTypeError initialState (mkUnsupportedOperatorValueError operatorSymbol))
            Nothing -> instantiateDeclaredOperatorBindingType env (coreNodeFacts node) operatorSymbol initialState
        _ -> (Nothing, initialState)

    inferBuiltinOperatorApplication operatorSymbol maybeAliasScheme (_, leftExpr) (_, rightExpr) =
      let (leftResult, stateAfterLeft) =
            inferExprTypeDetailedType env state leftExpr
          (rightResult, stateAfterRight) =
            inferExprTypeDetailedType env stateAfterLeft rightExpr
          (expressionType, operandTyping, stateAfterBinary) =
            case (leftResult, rightResult) of
              (Just leftType, Just rightType) ->
                inferBinaryType operatorSymbol leftExpr rightExpr leftType rightType stateAfterRight
              _ -> (Nothing, Nothing, stateAfterRight)
          finalState =
            case (maybeAliasScheme, leftResult, rightResult) of
              (Just aliasScheme, Just leftType, Just rightType)
                | Just _ <- expressionType ->
                    applyOperatorAliasSchemeConstraints
                      operatorSymbol
                      aliasScheme
                      leftType
                      rightType
                      stateAfterBinary
              _ -> stateAfterBinary
          stateWithSpineFacts =
            case (leftResult, rightResult, expressionType) of
              (Just leftType, Just rightType, Just resultType) ->
                recordBuiltinOperatorSpineFacts
                  operatorSymbol
                  expr
                  leftType
                  rightType
                  resultType
                  finalState
              _ -> finalState
          operation = selectedBinaryOperation operatorSymbol leftExpr rightExpr <$> operandTyping
          stateWithOperation = recordSelectedBinaryOperation operation stateWithSpineFacts
       in (expressionType, stateWithOperation)

    selectedBinaryOperation operatorSymbol leftExpr rightExpr operandTyping =
      BinaryOperation
        operatorSymbol
        operandTyping
        (coreNodeId (expressionNode leftExpr))
        (coreNodeId (expressionNode rightExpr))

    recordSelectedBinaryOperation operation finalState =
      maybe
        finalState
        (\selected -> recordBinaryOperation (coreNodeId (expressionNode expr)) selected finalState)
        operation

    -- Optimized builtin operator inference visits the two operands directly.
    -- Record the callable and partial-application nodes it intentionally
    -- bypasses, using the final operand and result decisions.
    recordBuiltinOperatorSpineFacts operatorSymbol expression leftType rightType resultType finalState =
      case expression of
        EApply _ (EApply applicationNode operatorExpr _) _
          | Just (spineSymbol, _) <- builtinOperatorSymbolExpr env operatorExpr,
            spineSymbol == operatorSymbol ->
              record
                applicationNode
                (SemanticFunction resolvedRight resolvedResult)
                (recordBuiltinCallableFacts operatorType operatorExpr finalState)
        _ -> finalState
      where
        resolvedLeft = resolveType finalState leftType
        resolvedRight = resolveType finalState rightType
        resolvedResult = resolveType finalState resultType
        operatorType = SemanticFunction resolvedLeft (SemanticFunction resolvedRight resolvedResult)
        record node expressionType = recordExpressionFactType (coreNodeId node) expressionType

    recordBuiltinCallableFacts operatorType operatorExpr finalState =
      case operatorExpr of
        EOperatorValue node _ -> record node operatorType finalState
        EVar node _ -> record node operatorType finalState
        EApply applicationNode dollarExpr nestedOperator ->
          record
            applicationNode
            operatorType
            ( recordBuiltinCallableFacts
                operatorType
                nestedOperator
                (recordDollarFunctionFacts operatorType dollarExpr finalState)
            )
        _ -> finalState
      where
        record node expressionType = recordExpressionFactType (coreNodeId node) expressionType

    inferSectionApplicationWithFallback functionExpr argumentExpr operatorSymbol leftOperand rightOperand =
      let genericResult@(inferredResult, _) = inferGenericApplication functionExpr argumentExpr
       in case inferredResult of
            Just _ -> genericResult
            Nothing ->
              let (builtinInferredResult, builtinState) =
                    inferBuiltinOperatorApplication operatorSymbol Nothing leftOperand rightOperand
               in case builtinInferredResult of
                    Just resultType ->
                      ( builtinInferredResult,
                        recordSectionFallbackFacts
                          functionExpr
                          argumentExpr
                          resultType
                          builtinState
                      )
                    Nothing -> genericResult

    -- The direct builtin fallback infers the two operands without traversing
    -- the section syntax that presents one operand as a function. Reconstruct
    -- those wrapper facts from the final operand and result decisions so every
    -- successful resolved node still receives an analyzed runtime plan.
    recordSectionFallbackFacts functionExpr argumentExpr resultType finalState =
      case Map.lookup (coreNodeId (expressionNode argumentExpr)) (inferExpressionFactTypes finalState) of
        Nothing -> finalState
        Just argumentType ->
          recordSectionFunctionFacts
            ( SemanticFunction
                (resolveType finalState argumentType)
                (resolveType finalState resultType)
            )
            functionExpr
            finalState

    recordSectionFunctionFacts sectionType functionExpr finalState =
      case functionExpr of
        ESectionLeft node _ _ -> record node sectionType finalState
        ESectionRight node _ _ -> record node sectionType finalState
        EApply applicationNode dollarExpr sectionExpr ->
          record
            applicationNode
            sectionType
            ( recordSectionFunctionFacts
                sectionType
                sectionExpr
                (recordDollarFunctionFacts sectionType dollarExpr finalState)
            )
        _ -> finalState
      where
        record node expressionType = recordExpressionFactType (coreNodeId node) expressionType

    recordDollarFunctionFacts sectionType dollarExpr finalState =
      if builtinDollarOperatorExpr env dollarExpr
        then record (expressionNode dollarExpr) finalState
        else finalState
      where
        record node =
          recordExpressionFactType
            (coreNodeId node)
            (SemanticFunction sectionType sectionType)

    inferGenericApplication functionExpr argumentExpr =
      first checkedExprType (inferCheckedApplication env state expr functionExpr argumentExpr)

-- The raw prepend primitive deliberately adopts the concrete element type
-- carried by its list argument and coerces the prepended value to match.
-- Ordinary left-to-right application inference has already instantiated
-- the polymorphic callable from the head argument, so refine the recorded
-- callable spine from the tail here when it carries the more specific
-- Int64/Float64 representation behind an Int/Float alias.
specializeListPrependRawResult :: Expr 'Resolved -> Maybe ExpressionType -> Maybe ExpressionType -> InferState -> (Maybe ExpressionType, InferState)
specializeListPrependRawResult functionExpr argumentResult expressionType finalState =
  case (functionExpr, argumentResult, expressionType) of
    (EApply applicationNode builtinExpr _, Just (SemanticList tailElementType), Just _)
      | builtinListPrependRawExpr builtinExpr ->
          let resolvedElementType = resolveType finalState tailElementType
              listType = SemanticList resolvedElementType
              partialType = SemanticFunction listType listType
              callableType = SemanticFunction resolvedElementType partialType
              stateWithPartialFact =
                replaceExpressionFactType
                  (coreNodeId applicationNode)
                  partialType
                  finalState
              stateWithCallableFact =
                replaceExpressionFactType
                  (coreNodeId (expressionNode builtinExpr))
                  callableType
                  stateWithPartialFact
           in (Just listType, stateWithCallableFact)
    _ -> (expressionType, finalState)

replaceExpressionFactType :: CoreNodeId -> ExpressionType -> InferState -> InferState
replaceExpressionFactType nodeId expressionType =
  modifyInferenceOutput
    ( \output ->
        output
          { outputExpressionFactTypes =
              Map.insert nodeId expressionType (outputExpressionFactTypes output)
          }
    )

builtinListPrependRawExpr :: Expr 'Resolved -> Bool
builtinListPrependRawExpr expression =
  case expression of
    EVar _ name ->
      lookupKernelBuiltinSymbol (identifierText name)
        == Just BuiltinListPrependRaw
    _ -> False

inferCheckedApplication :: TypeEnv -> InferState -> Expr 'Resolved -> Expr 'Resolved -> Expr 'Resolved -> (CheckedExpr, InferState)
inferCheckedApplication env state expr function argument =
  let (functionCheck, afterFunction) = inferExprTypeDetailed env state function
      (argumentCheck, afterArgument) = inferExprTypeDetailed env afterFunction argument
      (rawType, rawState) = inferApplicationFromResults env state function argument (checkedExprType functionCheck) (checkedExprType argumentCheck) afterArgument
      (result, finalState) = specializeListPrependRawResult function (checkedExprType argumentCheck) rawType rawState
      functionDraft = case (function, checkedExprType argumentCheck, result) of
        (EApply _ builtin _, Just (SemanticList elementType), Just _)
          | builtinListPrependRawExpr builtin -> refineListPrependDraft finalState function (resolveType finalState elementType) (checkedExprTree functionCheck)
        _ -> checkedExprTree functionCheck
      draft = EApply <$> draftExpressionNode finalState result expr <*> functionDraft <*> checkedExprTree argumentCheck
   in (CheckedExpr result draft, finalState)

inferApplicationFromResults :: TypeEnv -> InferState -> Expr 'Resolved -> Expr 'Resolved -> Maybe ExpressionType -> Maybe ExpressionType -> InferState -> (Maybe ExpressionType, InferState)
inferApplicationFromResults currentEnv applicationStartState functionExpr argumentExpr functionResult argumentResult stateAfterArgument =
  case inferApplicationFromResultsUnchecked applicationStartState functionResult argumentResult stateAfterArgument of
    result@(Nothing, _) -> result
    result@(Just _, unifiedState) ->
      case numericConversionLiteralDiagnostic currentEnv functionExpr argumentExpr of
        Just diagnostic -> (Nothing, addTypeError unifiedState diagnostic)
        Nothing -> result

inferApplicationFromResultsUnchecked :: InferState -> Maybe ExpressionType -> Maybe ExpressionType -> InferState -> (Maybe ExpressionType, InferState)
inferApplicationFromResultsUnchecked applicationStartState functionResult argumentResult stateAfterArgument =
  let (resultTypeVar, stateWithResultVar) = freshTypeVar stateAfterArgument
   in case (functionResult, argumentResult) of
        (Just functionType, Just argumentType) ->
          case unifyTypes functionType (SemanticFunction argumentType resultTypeVar) stateWithResultVar of
            Just unifiedState ->
              (Just (resolveType unifiedState resultTypeVar), unifiedState)
            Nothing ->
              ( Nothing,
                addTypeError
                  (discardFailedFunctionApplicationConstraints applicationStartState stateWithResultVar)
                  ( mkApplyTypeError
                      (defaultLiteralTypes stateWithResultVar (resolveType stateWithResultVar functionType))
                      (defaultLiteralTypes stateWithResultVar (resolveType stateWithResultVar argumentType))
                  )
              )
        _ ->
          ( Nothing,
            discardFailedFunctionApplicationConstraints applicationStartState stateWithResultVar
          )

inferListElements :: TypeEnv -> InferState -> [Expr 'Resolved] -> (Maybe ExpressionType, [CheckedExpr], InferState)
inferListElements env initialState elements = case elements of
  [] -> let (elementType, finalState) = freshTypeVar initialState in (Just (SemanticList elementType), [], finalState)
  firstElement : rest ->
    let (firstCheck, stateAfterFirst) = inferExprTypeDetailed env initialState firstElement
        (elementType, reversed, finalState) = foldl' step (checkedExprType firstCheck, [firstCheck], stateAfterFirst) rest
     in (SemanticList <$> elementType, reverse reversed, finalState)
  where
    step (expected, reversed, prior) element =
      let (checked, afterElement) = inferExprTypeDetailed env prior element
          (nextExpected, finalState) = case (expected, checkedExprType checked) of
            (Just expectedType, Just actualType) -> case unifyTypes expectedType actualType afterElement of
              Just unified -> (Just (resolveType unified expectedType), unified)
              Nothing -> (Just expectedType, addTypeError afterElement (mkListElementTypeMismatchError (resolveType afterElement expectedType) (resolveType afterElement actualType)))
            _ -> (expected, afterElement)
       in (nextExpected, checked : reversed, finalState)

inferTupleElements :: TypeEnv -> InferState -> [Expr 'Resolved] -> (Maybe ExpressionType, [CheckedExpr], InferState)
inferTupleElements env = go (Just []) []
  where
    go types children state remaining = case remaining of
      [] -> (SemanticTuple . reverse <$> types, reverse children, state)
      element : rest ->
        let (checked, next) = inferExprTypeDetailed env state element
            nextTypes = ((:) . resolveType next <$> checkedExprType checked) <*> types
         in go nextTypes (checked : children) next rest

inferIfFromResults :: Maybe ExpressionType -> Maybe ExpressionType -> Maybe ExpressionType -> InferState -> (Maybe ExpressionType, InferState)
inferIfFromResults conditionResult thenResult elseResult stateAfterElse =
  let stateAfterConditionCheck =
        case conditionResult of
          Just inferredConditionType ->
            case unifyTypes inferredConditionType SemanticBool stateAfterElse of
              Just unifiedState -> unifiedState
              Nothing ->
                addTypeError
                  stateAfterElse
                  (mkIfConditionTypeError (resolveType stateAfterElse inferredConditionType))
          Nothing -> stateAfterElse
   in case (thenResult, elseResult) of
        (Just inferredThenType, Just inferredElseType) ->
          case unifyTypes inferredThenType inferredElseType stateAfterConditionCheck of
            Just unifiedState ->
              (Just (resolveType unifiedState inferredThenType), unifiedState)
            Nothing ->
              ( Nothing,
                addTypeError
                  stateAfterConditionCheck
                  ( mkIfBranchTypeMismatchError
                      (resolveType stateAfterConditionCheck inferredThenType)
                      (resolveType stateAfterConditionCheck inferredElseType)
                  )
              )
        _ -> (Nothing, stateAfterConditionCheck)

discardFailedFunctionApplicationConstraints :: InferState -> InferState -> InferState
discardFailedFunctionApplicationConstraints stateBeforeFunction stateAfterApplication =
  modifyInferenceOutput
    ( \output ->
        output
          { outputDeferredConstraints =
              outputDeferredConstraints (inferOutput stateBeforeFunction),
            outputInferredConstraints =
              outputInferredConstraints (inferOutput stateBeforeFunction),
            outputInferredConstraintCount =
              outputInferredConstraintCount (inferOutput stateBeforeFunction)
          }
    )
    stateAfterApplication

qualifiedApplication :: TypeEnv -> InferState -> Expr 'Resolved -> Bool
qualifiedApplication env state expr = case qualifiedMethodApplicationSpine expr state of
  Just (name, _, _, _) -> Map.notMember name env
  Nothing -> False

qualifiedMethodApplicationSpine :: Expr 'Resolved -> InferState -> Maybe (TypeEnvKey, SourceSpan, CapabilityMethodKey, [Expr 'Resolved])
qualifiedMethodApplicationSpine expr state =
  case applicationSpine expr of
    Just (methodName, methodSpan, argumentExprs)
      | Just methodKey <- capabilityMethodKeyFromReference (typeEnvReference methodName),
        qualifiedMethodClassIsVisible methodKey state ->
          Just (methodName, methodSpan, methodKey, argumentExprs)
    _ -> Nothing

applicationSpine :: Expr 'Resolved -> Maybe (TypeEnvKey, SourceSpan, [Expr 'Resolved])
applicationSpine expr =
  go [] expr
  where
    go argumentExprs currentExpr =
      case currentExpr of
        EApply _ (EOperatorValue _ "$") functionExpr ->
          go argumentExprs functionExpr
        EApply _ functionExpr argumentExpr ->
          go (argumentExpr : argumentExprs) functionExpr
        EVar node name ->
          Just (typeEnvReferenceKey (coreNodeFacts node) name, coreNodeSpan node, argumentExprs)
        _ ->
          Nothing

recordQualifiedMethodSpineFacts :: Expr 'Resolved -> ExpressionType -> InferState -> InferState
recordQualifiedMethodSpineFacts expression methodType state =
  case expression of
    EApply _ functionExpression _ -> snd (recordSpine functionExpression state)
    _ -> state
  where
    recordSpine currentExpression currentState =
      case currentExpression of
        EApply applicationNode (EOperatorValue dollarNode "$") functionExpression ->
          let (functionType, stateAfterFunction) = recordSpine functionExpression currentState
              stateAfterDollar = record dollarNode (SemanticFunction functionType functionType) stateAfterFunction
           in (functionType, record applicationNode functionType stateAfterDollar)
        EApply applicationNode functionExpression _ ->
          let (functionType, stateAfterFunction) = recordSpine functionExpression currentState
              resultType = applicationResultType functionType
           in (resultType, record applicationNode resultType stateAfterFunction)
        ETypeApplication applicationNode functionExpression _ _ ->
          let (_, stateAfterFunction) = recordSpine functionExpression currentState
           in (methodType, record applicationNode methodType stateAfterFunction)
        _ ->
          ( methodType,
            record (expressionNode currentExpression) methodType currentState
          )
    record node expressionType =
      recordExpressionFactType (coreNodeId node) expressionType
    applicationResultType expressionType =
      case expressionType of
        SemanticFunction _ resultType -> resultType
        _ -> expressionType

builtinOperatorApplicationSpine ::
  TypeEnv ->
  Expr 'Resolved ->
  Maybe (Text, Maybe TypeScheme, ([Int], Expr 'Resolved), ([Int], Expr 'Resolved), Bool)
builtinOperatorApplicationSpine env expr =
  case expr of
    EApply _ (EApply _ dollarExpr sectionExpr) argumentExpr
      | builtinDollarOperatorExpr env dollarExpr ->
          case sectionExpr of
            ESectionLeft _ leftExpr operatorSymbol
              | builtinSectionOperatorSymbol operatorSymbol ->
                  Just (operatorSymbol, Nothing, ([0, 1, 0], leftExpr), ([1], argumentExpr), True)
            ESectionRight _ operatorSymbol rightExpr
              | builtinSectionOperatorSymbol operatorSymbol ->
                  Just (operatorSymbol, Nothing, ([1], argumentExpr), ([0, 1, 0], rightExpr), True)
            _ -> Nothing
    EApply _ (ESectionLeft _ leftExpr operatorSymbol) rightExpr
      | builtinSectionOperatorSymbol operatorSymbol ->
          Just (operatorSymbol, Nothing, ([0, 0], leftExpr), ([1], rightExpr), True)
    EApply _ (ESectionRight _ operatorSymbol rightExpr) leftExpr
      | builtinSectionOperatorSymbol operatorSymbol ->
          Just (operatorSymbol, Nothing, ([1], leftExpr), ([0, 0], rightExpr), True)
    EApply _ (EApply _ operatorExpr leftExpr) rightExpr -> do
      (operatorSymbol, maybeAliasScheme) <- builtinOperatorSymbolExpr env operatorExpr
      if hasOperatorRule operatorSymbol
        then Just (operatorSymbol, maybeAliasScheme, ([0, 1], leftExpr), ([1], rightExpr), False)
        else Nothing
    _ -> Nothing

builtinOperatorSymbolExpr :: TypeEnv -> Expr 'Resolved -> Maybe (Text, Maybe TypeScheme)
builtinOperatorSymbolExpr env expr =
  case expr of
    EOperatorValue _ operatorSymbol
      | isBuiltinOperatorSymbol operatorSymbol ->
          Just (operatorSymbol, Nothing)
    EApply _ dollarExpr operatorExpr
      | builtinDollarOperatorExpr env dollarExpr ->
          builtinOperatorSymbolExpr env operatorExpr
    EVar node name ->
      case Map.lookup (typeEnvReferenceKey (coreNodeFacts node) name) env of
        Just (BuiltinOperatorAliasTypeBinding operatorSymbol) -> Just (operatorSymbol, Nothing)
        Just (OperatorAliasSchemeTypeBinding operatorSymbol typeScheme) -> Just (operatorSymbol, Just typeScheme)
        _ -> Nothing
    _ -> Nothing

literalExpressionType :: Literal -> InferState -> (ExpressionType, InferState)
literalExpressionType literal state =
  case literal of
    LInt value -> freshIntegerLiteralType (singletonIntegerLiteralRange value) state
    LFloat _ _ maybeTargetType ->
      ( case maybeTargetType of
          Just targetType -> SemanticNumeric targetType
          Nothing -> SemanticFloat,
        state
      )
    LBool _ -> (SemanticBool, state)
    LChar _ -> (SemanticChar, state)
    LText _ -> (SemanticText, state)

checkLiteralType :: InferState -> Literal -> InferState
checkLiteralType state literal =
  case literal of
    LFloat literalValue literalSource (Just targetType) ->
      case targetedFloatLiteralDiagnostic targetType literalValue literalSource of
        Just diagnostic -> addTypeError state diagnostic
        Nothing -> state
    _ -> state

numericConversionLiteralDiagnostic :: TypeEnv -> Expr 'Resolved -> Expr 'Resolved -> Maybe Diagnostic
numericConversionLiteralDiagnostic env functionExpr argumentExpr =
  case (functionExpr, argumentExpr) of
    (EVar node functionName, ELit _ (LInt literalValue)) ->
      case numericConversionTargetFromCallable env (typeEnvReferenceKey (coreNodeFacts node) functionName) of
        Just targetType ->
          case numericTypeLiteralIntegerBounds targetType of
            Just bounds@(lowerBound, upperBound)
              | literalValue < lowerBound || literalValue > upperBound ->
                  Just (mkNumericConversionLiteralTypeError (identifierText functionName) literalValue targetType bounds)
            _ -> Nothing
        Nothing -> Nothing
    (EVar node functionName, ELit _ (LFloat literalValue literalSource _)) ->
      case numericConversionTargetFromCallable env (typeEnvReferenceKey (coreNodeFacts node) functionName) of
        Just targetType ->
          numericConversionFloatLiteralDiagnostic
            (identifierText functionName)
            targetType
            literalValue
            literalSource
        Nothing -> Nothing
    _ -> Nothing

numericConversionFloatLiteralDiagnostic :: Text -> NumericType -> Double -> FractionalLiteralSource -> Maybe Diagnostic
numericConversionFloatLiteralDiagnostic conversionName targetType literalValue literalSource =
  case numericTypeIntegerBounds targetType of
    Just bounds@(lowerBound, upperBound) ->
      case fractionalLiteralIntegralValue literalSource of
        Just integralValue
          | finiteFloat literalValue,
            integralValue >= lowerBound,
            integralValue <= upperBound ->
              Nothing
        _ ->
          Just (mkNumericConversionFractionalLiteralTypeError conversionName literalValue targetType bounds)
    Nothing ->
      case numericTypeFloatMax targetType of
        Just maxMagnitude
          | not (finiteFloat literalValue)
              || abs literalValue > maxMagnitude
              || fractionalLiteralExceedsMagnitude literalSource maxMagnitude ->
              Just (mkNumericConversionFloatLiteralOverflowError conversionName literalValue targetType maxMagnitude)
        _ -> Nothing

finiteFloat :: Double -> Bool
finiteFloat value = not (isNaN value) && not (isInfinite value)

numericConversionTargetFromCallable :: TypeEnv -> TypeEnvKey -> Maybe NumericType
numericConversionTargetFromCallable env functionName =
  let nameText = identifierText (typeEnvName functionName)
   in case Map.lookup functionName env of
        Just (BuiltinAliasTypeBinding builtinSymbol) ->
          builtinSymbolNumericConversionTarget builtinSymbol
        Just _ ->
          Nothing
        Nothing ->
          lookupKernelBuiltinSymbol nameText >>= builtinSymbolNumericConversionTarget

singletonIntegerLiteralRange :: Integer -> IntegerLiteralRange
singletonIntegerLiteralRange value = IntegerLiteralRange value value

instantiateBuiltinType :: ResolvedReference -> InferState -> Maybe (ExpressionType, InferState)
instantiateBuiltinType (BuiltinReference name) state = lookupKernelBuiltinSymbol (identifierText name) >>= (`instantiateBuiltinSymbolType` state)
instantiateBuiltinType _ _ = Nothing

instantiateDeclaredOperatorBindingType :: TypeEnv -> ResolvedNodeFacts -> Text -> InferState -> (Maybe ExpressionType, InferState)
instantiateDeclaredOperatorBindingType env facts operatorSymbol state =
  case Map.lookup (typeEnvReferenceKey facts (operatorBindingName operatorSymbol)) env of
    Just binding ->
      instantiateEnvBinding binding state
    Nothing ->
      ( Nothing,
        addTypeError state (mkMissingOperatorBindingError operatorSymbol)
      )

-- | Instantiate builtin symbol types on demand so each use site gets fresh type
-- variables instead of sharing one global schematic type.
instantiateBuiltinSymbolType :: BuiltinSymbol -> InferState -> Maybe (ExpressionType, InferState)
instantiateBuiltinSymbolType builtinSymbol state =
  -- Use catalog names here so newly-added symbols safely fall back to `Nothing`
  -- until an explicit type-instantiation rule is defined.
  case builtinSymbolNumericConversionTarget builtinSymbol of
    Just targetType ->
      let (sourceTypeVar, sourceType, stateAfterSourceType) = freshTypeVariable state
          stateAfterNumericConstraint =
            addNumericTypeVarConstraint sourceTypeVar AnyNumericConstraint stateAfterSourceType
       in Just (SemanticFunction sourceType (SemanticNumeric targetType), stateAfterNumericConstraint)
    Nothing ->
      instantiateBuiltinSymbolTypeByName (builtinSymbolName builtinSymbol) state

instantiateBuiltinSymbolTypeByName :: Text -> InferState -> Maybe (ExpressionType, InferState)
instantiateBuiltinSymbolTypeByName builtinName state =
  case builtinName of
    "hd" ->
      let (elementType, stateAfterElement) = freshTypeVar state
       in Just (SemanticFunction (SemanticList elementType) elementType, stateAfterElement)
    "tl" ->
      let (elementType, stateAfterElement) = freshTypeVar state
       in Just (SemanticFunction (SemanticList elementType) (SemanticList elementType), stateAfterElement)
    "map" ->
      let (sourceType, stateAfterSource) = freshTypeVar state
          (targetType, stateAfterTarget) = freshTypeVar stateAfterSource
       in Just
            ( SemanticFunction
                (SemanticFunction sourceType targetType)
                (SemanticFunction (SemanticList sourceType) (SemanticList targetType)),
              stateAfterTarget
            )
    "filter" ->
      let (elementType, stateAfterElement) = freshTypeVar state
       in Just
            ( SemanticFunction
                (SemanticFunction elementType SemanticBool)
                (SemanticFunction (SemanticList elementType) (SemanticList elementType)),
              stateAfterElement
            )
    "print!" ->
      -- Stub-v1 runtime keeps `print!` as an impure primitive that returns the
      -- evaluated argument value unchanged so compile/runtime paths stay simple.
      let (valueType, stateAfterValueType) = freshTypeVar state
       in Just (SemanticFunction valueType valueType, stateAfterValueType)
    "listPrependRaw" ->
      let (elementType, stateAfterElement) = freshTypeVar state
       in Just
            ( SemanticFunction
                elementType
                (SemanticFunction (SemanticList elementType) (SemanticList elementType)),
              stateAfterElement
            )
    "listReverseRaw" ->
      let (elementType, stateAfterElement) = freshTypeVar state
       in Just (SemanticFunction (SemanticList elementType) (SemanticList elementType), stateAfterElement)
    "charToUInt32" ->
      Just (SemanticFunction SemanticChar (SemanticNumeric NumericUInt32), state)
    "charFromUInt32Raw" ->
      Just (SemanticFunction (SemanticNumeric NumericUInt32) (SemanticList SemanticChar), state)
    "charIsAlpha" ->
      Just (SemanticFunction SemanticChar SemanticBool, state)
    "charIsAlphaNum" ->
      Just (SemanticFunction SemanticChar SemanticBool, state)
    "charIsDigit" ->
      Just (SemanticFunction SemanticChar SemanticBool, state)
    "charIsSpace" ->
      Just (SemanticFunction SemanticChar SemanticBool, state)
    "charIsHexDigit" ->
      Just (SemanticFunction SemanticChar SemanticBool, state)
    "charIsLower" ->
      Just (SemanticFunction SemanticChar SemanticBool, state)
    "charIsUpper" ->
      Just (SemanticFunction SemanticChar SemanticBool, state)
    "charToLower" ->
      Just (SemanticFunction SemanticChar SemanticChar, state)
    "charToUpper" ->
      Just (SemanticFunction SemanticChar SemanticChar, state)
    "textLength" ->
      Just (SemanticFunction SemanticText SemanticInt, state)
    "textUnconsRaw" ->
      Just
        ( SemanticFunction
            SemanticText
            (SemanticList (SemanticTuple [SemanticChar, SemanticText])),
          state
        )
    "textAppend" ->
      Just (SemanticFunction SemanticText (SemanticFunction SemanticText SemanticText), state)
    "textAppendChar" ->
      Just (SemanticFunction SemanticText (SemanticFunction SemanticChar SemanticText), state)
    "textFromChars" ->
      Just (SemanticFunction (SemanticList SemanticChar) SemanticText, state)
    "textConcat" ->
      Just (SemanticFunction (SemanticList SemanticText) SemanticText, state)
    "renderValue" ->
      let (valueType, stateAfterValueType) = freshTypeVar state
       in Just (SemanticFunction valueType SemanticText, stateAfterValueType)
    "readTextRaw!" ->
      Just (SemanticFunction SemanticText hostIOOutcomeType, state)
    "writeTextRaw!" ->
      Just
        ( SemanticFunction
            SemanticText
            (SemanticFunction SemanticText hostIOOutcomeType),
          state
        )
    "readStdinRaw!" ->
      Just (SemanticFunction unitType hostIOOutcomeType, state)
    "writeStdoutRaw!" ->
      Just (SemanticFunction SemanticText hostIOOutcomeType, state)
    "writeStderrRaw!" ->
      Just (SemanticFunction SemanticText hostIOOutcomeType, state)
    "arguments!" ->
      Just (SemanticFunction unitType (SemanticList SemanticText), state)
    "exit!" ->
      Just (SemanticFunction SemanticInt unitType, state)
    _ -> Nothing

hostIOOutcomeType :: ExpressionType
hostIOOutcomeType = SemanticTuple [SemanticBool, SemanticText, SemanticText, SemanticText]

unitType :: ExpressionType
unitType = SemanticTuple []
