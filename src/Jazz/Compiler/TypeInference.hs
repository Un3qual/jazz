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

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Jazz.Compiler.AST
  ( ClassMethodSignature (..),
    CorePhase (..),
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
import Jazz.Compiler.CoreIdentity (CapabilityId (..), CoreBinderId, ResolvedNodeFacts (..), ResolvedReference (..), resolvedValueReference)
import Jazz.Compiler.Diagnostics (Diagnostic)
import Jazz.Compiler.FractionalLiteral
  ( FractionalLiteralSource,
    fractionalLiteralExceedsMagnitude,
    fractionalLiteralIntegralValue,
  )
import Jazz.Compiler.ModuleExports (ModuleExport (..), ModuleExportInventory, exportInventory, withClassMethods)
import Jazz.Compiler.ModuleInterface
  ( ModuleInterface (..),
    emptyModuleInterface,
    moduleExportForBinding,
    publishModuleInterface,
  )
import Jazz.Compiler.Name
  ( Name (..),
    NameNamespace (..),
    ResolvedName,
    UnresolvedName,
    identifierText,
    mkIdentifier,
    renderName,
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
  ( SemanticFactInvariantFailure (MissingExpressionFacts, MissingScopeFacts),
  )
import Jazz.Compiler.TypeInference.Analyzed (ExpressionDecision (..), draftDecidedExpressionNode, draftExpressionNode, draftLambda, noExpressionDecision, refineListPrependDraft)
import Jazz.Compiler.TypeInference.Capabilities
import Jazz.Compiler.TypeInference.Diagnostics
import Jazz.Compiler.TypeInference.Draft (CheckedExpr (..), CheckedScope (..), rejectedDraft)
import Jazz.Compiler.TypeInference.Environment (insertResolvedTypeBinding)
import Jazz.Compiler.TypeInference.Instantiation (instantiateTypeScheme)
import Jazz.Compiler.TypeInference.Interface (closeModuleBindings, importBindingTypes)
import Jazz.Compiler.TypeInference.Pattern
  ( inferPatternCaseType,
  )
import Jazz.Compiler.TypeInference.Scope
  ( inferExplicitTypeApplication,
    inferNestedScopeTypeWithMode,
    inferScopeTypeWithMode,
    instantiateNonBuiltinTypeBinding,
  )
import Jazz.Compiler.TypeInference.Solver
  ( addNumericTypeVarConstraint,
    addStrictEqualityTypeVarConstraint,
    freshIntegerLiteralType,
    freshTypeVar,
    freshTypeVariable,
    resolveType,
    unifyTypes,
  )
import Jazz.Compiler.TypeInference.State
  ( DeclarationState (..),
    EvidenceReference,
    InferState (..),
    InferenceOutput (..),
    ModuleInferenceState (..),
    inferConstructorWitnessNames,
    inferDataTypes,
    inferVisibleTypes,
    initialInferState,
    modifyInferenceOutput,
    recordPatternCoverageSite,
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
    inferenceImportedClassNames :: Set Text
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

inferExpressionWork :: InferenceInputs -> Expr 'Resolved -> (CheckedExpr, InferState, InferenceSubject)
inferExpressionWork inputs expr =
  let (importedEnvironment, initialState) = importBindingTypes (inferenceImportedTypes inputs) (initialStateForInference inputs)
   in case expr of
        EBlock node statements
          | Right preparedScope <- prepareResolvedScope node statements ->
              let (blockCheck, rawBlockState) =
                    inferScopeTypeWithMode
                      inferExprTypeWithMode
                      InferenceOnly
                      importedEnvironment
                      initialState
                      preparedScope
                  blockResult = checkedScopeType blockCheck
                  blockType = fromMaybe unitType blockResult
                  blockState = rawBlockState
               in (CheckedExpr blockResult (EBlock <$> draftExpressionNode (Just blockType) expr <*> checkedScopeTree blockCheck), blockState, InferencePreparedScope expr preparedScope)
        EBlock node _ -> (CheckedExpr Nothing (rejectedDraft (MissingScopeFacts (coreNodeId node))), initialState, InferenceExpression expr)
        _ ->
          let (result, resultState) =
                inferExprTypeDetailed
                  importedEnvironment
                  initialState
                  expr
           in (result, resultState, InferenceExpression expr)

initialStateForInference :: InferenceInputs -> InferState
initialStateForInference inputs =
  validateImplementationCoherence $
    applyCapabilityFacts
      (inferenceImportedCapabilities inputs)
      initialInferState
        { inferDeclarations =
            (inferDeclarations initialInferState)
              { declarationDataTypes = inferenceImportedDataTypes inputs
              },
          inferModule =
            (inferModule initialInferState)
              { inferenceConstructorWitnessNames =
                  inferenceImportedConstructorWitnessNames inputs
              }
        }

moduleInterfaceFromState :: InferenceInputs -> Expr 'Resolved -> InferState -> ModuleInterface
moduleInterfaceFromState inputs expr state =
  publishModuleInterface (Just (fromMaybe declaredInventory (inferencePublicExports inputs))) (inferDataTypes state) $
    emptyModuleInterface
      { interfaceValueBindings =
          closeModuleBindings
            state
            [ (moduleExportForBinding (renderName name) binding, binder, binding)
            | (name, binder) <- Map.toList declaredValues,
              Just binding <- [Map.lookup (TypeEnvKey binder name) (inferVisibleTypes state)]
            ],
        interfaceDataTypes = Map.restrictKeys (inferDataTypes state) declaredDataTypes,
        interfaceCapabilities = capabilityFactsFromState state
      }
  where
    (declaredValues, declaredDataTypes) = declaredModuleBindings expr
    declaredClasses = [(capability, methods) | SClass _ capability _ methods _ _ <- case expr of EBlock _ statements -> statements; _ -> []]
    declaredInventory =
      withClassMethods
        (Map.fromList [(identifierText capability, Set.fromList [identifierText name | ClassMethodSignature _ name _ <- methods]) | (capability, methods) <- declaredClasses])
        $ exportInventory
          ( [moduleExportForBinding (renderName name) binding | (name, reference) <- Map.toList declaredValues, Just binding <- [Map.lookup (TypeEnvKey reference name) (inferVisibleTypes state)]]
              <> [ModuleExport TypeNamespace (renderName name) | name <- Set.toList declaredDataTypes]
              <> [ModuleExport CapabilityNamespace (renderName capability) | (capability, _) <- declaredClasses]
          )

declaredModuleBindings :: Expr 'Resolved -> (Map ResolvedName ResolvedReference, Set ResolvedName)
declaredModuleBindings expression =
  case expression of
    EBlock _ statements -> foldl' collect (Map.empty, Set.empty) statements
    _ -> (Map.empty, Set.empty)
  where
    collect :: (Map ResolvedName ResolvedReference, Set ResolvedName) -> Statement 'Resolved -> (Map ResolvedName ResolvedReference, Set ResolvedName)
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
        SClass _ capability _ methods _ _ ->
          (foldl' (\names (ClassMethodSignature _ name _) -> Map.insert name (CapabilityMethodReference (CapabilityId capability) (mkIdentifier (identifierText name))) names) valueNames methods, dataTypeNames)
        _ -> (valueNames, dataTypeNames)

    insertBinder node name bindings = maybe bindings (\binder -> Map.insert name (LexicalReference binder) bindings) (resolvedNodeBinder (coreNodeFacts node))

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
    _ -> instantiateNonBuiltinTypeBinding binding state

inferExprTypeWithMode :: InferenceMode -> TypeEnv -> InferState -> Expr 'Resolved -> (CheckedExpr, InferState)
inferExprTypeWithMode mode env state expr = case expr of
  EBlock node statements
    | Right prepared <- prepareResolvedScope node statements ->
        let (scope, inferredState) = inferNestedScopeTypeWithMode inferExprTypeWithMode mode env state prepared
            result = checkedScopeType scope
            finalState = inferredState
         in (CheckedExpr result (EBlock <$> draftExpressionNode result expr <*> checkedScopeTree scope), finalState)
  EBlock node _ -> (CheckedExpr Nothing (rejectedDraft (MissingScopeFacts (coreNodeId node))), state)
  _ -> inferExprTypeDetailed env state expr

-- Checking returns each draft subtree alongside its inferred type.
inferExprTypeDetailed :: TypeEnv -> InferState -> Expr 'Resolved -> (CheckedExpr, InferState)
inferExprTypeDetailed env state expr = case expr of
  ELit _ literal -> leaf (\node -> ELit node literal)
  EVar _ name -> leaf (\node -> EVar node name)
  EOperatorValue {} -> (CheckedExpr Nothing (rejectedDraft (MissingExpressionFacts (coreNodeId (expressionNode expr)))), state)
  ETuple _ [] -> leaf (\node -> ETuple node [])
  EIf _ condition thenExpression elseExpression ->
    let (conditionCheck, stateAfterCondition) = inferExprTypeDetailed env state condition
        (thenCheck, stateAfterThen) = inferExprTypeDetailed env stateAfterCondition thenExpression
        (elseCheck, stateAfterElse) = inferExprTypeDetailed env stateAfterThen elseExpression
        (result, finalState) = inferIfFromResults (checkedExprType conditionCheck) (checkedExprType thenCheck) (checkedExprType elseCheck) stateAfterElse
     in finish result finalState (\node -> EIf <$> node <*> checkedExprTree conditionCheck <*> checkedExprTree thenCheck <*> checkedExprTree elseCheck)
  EList _ elements ->
    let (result, children, finalState) = inferListElements env state elements
     in finish result (annotateNewErrorsWithPrimarySpan (coreNodeSpan (expressionNode expr)) state finalState) (\node -> EList <$> node <*> traverse checkedExprTree children)
  ETuple _ elements ->
    let (result, children, finalState) = inferTupleElements env state elements
     in finish result finalState (\node -> ETuple <$> node <*> traverse checkedExprTree children)
  EPatternCase _ scrutinee caseArms ->
    let (coverageOrdinal, stateWithOrdinal) = reservePatternCoverageSite state
        (scrutineeCheck, stateAfterScrutinee) = inferExprTypeDetailed env stateWithOrdinal scrutinee
        (scrutineeType, stateWithScrutineeType) = case checkedExprType scrutineeCheck of
          Just inferredType -> (inferredType, stateAfterScrutinee)
          Nothing -> freshTypeVar stateAfterScrutinee
        (expressionType, armDrafts, inferredFinalState) = inferPatternCaseType inferExprTypeDetailed env scrutineeType stateWithScrutineeType caseArms
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
     in finish expressionType finalState (\node -> EPatternCase <$> node <*> checkedExprTree scrutineeCheck <*> armDrafts)
  EBinary {} -> unresolvedOperator
  ESectionLeft {} -> unresolvedOperator
  ESectionRight {} -> unresolvedOperator
  EApply _ function argument -> inferCheckedApplication env state expr function argument
  ETypeApplication {} -> inferExplicitTypeApplication inferExprTypeDetailed env state expr
  ELambda node name body ->
    let (parameterType, stateAfterParameter) = freshTypeVar state
        (bodyCheck, finalState) = inferExprTypeDetailed (insertResolvedTypeBinding (coreNodeFacts node) name (PlainTypeBinding parameterType) env) stateAfterParameter body
        result = SemanticFunction (resolveType finalState parameterType) <$> checkedExprType bodyCheck
     in finish result finalState (\facts -> draftLambda (capabilityFactsFromState finalState) facts name (checkedExprTree bodyCheck))
  EBlock {} -> inferExprTypeWithMode InferConcreteFunctions env state expr
  where
    leaf make =
      let (result, evidence, finalState) = inferLeafExpression env state expr
          decision = noExpressionDecision {decisionEvidence = evidence}
       in (CheckedExpr result (make <$> draftDecidedExpressionNode decision result expr), finalState)
    finish result finalState make =
      (CheckedExpr result (make (draftExpressionNode result expr)), finalState)
    unresolvedOperator =
      (CheckedExpr Nothing (rejectedDraft (MissingExpressionFacts (coreNodeId (expressionNode expr)))), state)

inferLeafExpression :: TypeEnv -> InferState -> Expr 'Resolved -> (Maybe ExpressionType, [EvidenceReference], InferState)
inferLeafExpression env state expr = case expr of
  ELit _ literal ->
    let (literalType, afterLiteral) = literalExpressionType literal state
     in (Just literalType, [], checkLiteralType afterLiteral literal)
  ETuple _ [] -> (Just (SemanticTuple []), [], state)
  EVar node name ->
    let (result, evidence, finalState) = case Map.lookup (typeEnvReferenceKey (coreNodeFacts node) name) env of
          Just binding -> ordinary (instantiateEnvBinding binding state)
          Nothing | Just symbol <- resolvedOperatorSpelling (coreNodeFacts node) -> (Nothing, [], addTypeError state (if symbol == "|" then mkUnsupportedOperatorValueError symbol else mkMissingOperatorBindingError symbol))
          Nothing -> case instantiateBuiltinType (resolvedValueReference (coreNodeFacts node) name) state of
            Just (builtinType, next) -> (Just builtinType, [], next)
            Nothing -> case instantiateQualifiedMethodType instantiateTypeScheme (resolvedValueReference (coreNodeFacts node) name) state of
              Just (selection, next) -> (selectedMethodType selection, selectedMethodEvidence selection, next)
              Nothing -> (Nothing, [], state)
     in (result, evidence, annotateNewErrorsWithPrimarySpan (coreNodeSpan node) state finalState)
  _ -> (Nothing, [], state)
  where
    ordinary (result, next) = (result, newConstraintEvidence state next, next)

-- The raw prepend primitive deliberately adopts the concrete element type
-- carried by its list argument and coerces the prepended value to match.
-- Ordinary left-to-right application inference has already instantiated
-- the polymorphic callable from the head argument, so refine the recorded
-- callable spine from the tail here when it carries the more specific
-- Int64/Float64 representation behind an Int/Float alias.
builtinListPrependRawExpr :: TypeEnv -> Expr 'Resolved -> Bool
builtinListPrependRawExpr env expression =
  case expression of
    EVar node name ->
      case Map.lookup (typeEnvReferenceKey (coreNodeFacts node) name) env of
        Just (BuiltinAliasTypeBinding symbol) -> symbol == BuiltinListPrependRaw
        Just _ -> False
        Nothing -> case resolvedValueReference (coreNodeFacts node) name of
          BuiltinReference identifier -> lookupKernelBuiltinSymbol (identifierText identifier) == Just BuiltinListPrependRaw
          _ -> False
    _ -> False

inferCheckedApplication :: TypeEnv -> InferState -> Expr 'Resolved -> Expr 'Resolved -> Expr 'Resolved -> (CheckedExpr, InferState)
inferCheckedApplication env state expr function argument =
  let (functionCheck, afterFunction) = inferExprTypeDetailed env state function
      (argumentCheck, afterArgument) = inferExprTypeDetailed env afterFunction argument
      (rawType, finalState) = inferApplicationFromResults env state function argument (checkedExprType functionCheck) (checkedExprType argumentCheck) afterArgument
      (result, functionDraft) = case (function, checkedExprType argumentCheck, rawType) of
        (EApply _ builtin _, Just (SemanticList elementType), Just _)
          | builtinListPrependRawExpr env builtin ->
              let resolvedElement = resolveType finalState elementType
               in (Just (SemanticList resolvedElement), refineListPrependDraft function resolvedElement (checkedExprTree functionCheck))
        _ -> (rawType, checkedExprTree functionCheck)
      draft = EApply <$> draftExpressionNode result expr <*> functionDraft <*> checkedExprTree argumentCheck
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
    "add" -> numericBinary RuntimeArithmeticNumericConstraint id
    "subtract" -> numericBinary RuntimeArithmeticNumericConstraint id
    "multiply" -> numericBinary RuntimeArithmeticNumericConstraint id
    "divide" -> numericBinary RuntimeArithmeticNumericConstraint id
    "lessThan" -> numericBinary RuntimeComparisonNumericConstraint (const SemanticBool)
    "greaterThan" -> numericBinary RuntimeComparisonNumericConstraint (const SemanticBool)
    "equals" ->
      let (variable, operand, next) = freshTypeVariable state
       in Just (SemanticFunction operand (SemanticFunction operand SemanticBool), addStrictEqualityTypeVarConstraint variable next)
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
  where
    numericBinary constraint result =
      let (variable, operand, next) = freshTypeVariable state
       in Just (SemanticFunction operand (SemanticFunction operand (result operand)), addNumericTypeVarConstraint variable constraint next)

hostIOOutcomeType :: ExpressionType
hostIOOutcomeType = SemanticTuple [SemanticBool, SemanticText, SemanticText, SemanticText]

unitType :: ExpressionType
unitType = SemanticTuple []
