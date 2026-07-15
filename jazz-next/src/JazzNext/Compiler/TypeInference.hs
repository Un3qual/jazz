{-# LANGUAGE OverloadedStrings #-}

-- | Lightweight type inference layer for the current compiler subset. It
-- canonicalizes the lowered AST, reuses analyzer diagnostics, and adds the
-- small collection of type/runtime-compatibility checks implemented so far.
module JazzNext.Compiler.TypeInference
  ( InferenceInputs (..),
    InferenceResult (..),
    inferExpressionWithBuiltinsAndHiddenStatements,
    inferExpressionWithBuiltinsAndSourceUnitStatements,
    inferExpressionWithBuiltins,
    inferExpressionWithInputs,
    inferExpressionWithInputsAndHiddenStatements,
    inferExpression,
    inferExpressionDefault
  ) where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Text (Text)
import JazzNext.Compiler.Analyzer
  ( AnalysisBinding (..),
    AnalysisInputs (..),
    AnalysisResult (..),
    analyzeProgramWithInputs
  )
import JazzNext.Compiler.AST
  ( SignatureType (..),
    DataConstructor (..),
    Expr (..),
    Literal (..),
    NumericType (..),
    Statement (..)
  )
import JazzNext.Compiler.BuiltinCatalog
  ( BuiltinResolutionMode (..),
    BuiltinSymbol,
    builtinSymbolName,
    builtinSymbolNumericConversionTarget,
    lookupBuiltinSymbolInMode,
    numericTypeFloatMax,
    numericTypeIntegerBounds,
    numericTypeLiteralIntegerBounds
  )
import JazzNext.Compiler.Diagnostics
  ( Diagnostic (..),
    WarningRecord
  )
import JazzNext.Compiler.FractionalLiteral
  ( FractionalLiteralSource,
    fractionalLiteralExceedsMagnitude,
    fractionalLiteralIntegralValue
  )
import JazzNext.Compiler.Name
  ( GeneratedNameKind (..),
    Name (..),
    generatedName,
    identifierText,
    mkIdentifier,
    operatorBindingName,
    renderName,
    sourceName
  )
import JazzNext.Compiler.Parser.Operator
  ( isBuiltinOperatorSymbol
  )
import JazzNext.Compiler.RuntimeHints
  ( BindingRuntimeHintKey
  )
import JazzNext.Compiler.TypeInference.Capabilities
import JazzNext.Compiler.TypeInference.Diagnostics
import JazzNext.Compiler.TypeInference.Operator
  ( applyOperatorAliasSchemeConstraints,
    builtinSectionOperatorSymbol,
    hasOperatorRule,
    inferBinaryType,
    inferSectionLeftType,
    inferSectionRightType,
    instantiateOperatorType
  )
import JazzNext.Compiler.TypeInference.Pattern
  ( inferPatternCaseType
  )
import JazzNext.Compiler.TypeInference.Scope
  ( inferExplicitTypeApplication,
    inferScopeType,
    instantiateNonBuiltinTypeBinding
  )
import JazzNext.Compiler.TypeInference.State
  ( DeclarationState (..),
    InferState (..),
    InferenceOutput (..),
    ModuleInferenceState (..),
    inferDataTypes,
    inferDeferredExplicitConstraints,
    inferErrorsRev,
    inferInferredClassConstraints,
    inferModuleCapabilityFacts,
    inferRuntimeTypeHints,
    inferVisibleTypes,
    initialInferState,
    modifyInferenceOutput
  )
import JazzNext.Compiler.TypeInference.Solver
  ( addNumericTypeVarConstraint,
    combineIntegerLiteralRanges,
    freshTypeVar,
    freshTypeVariable,
    integerLiteralRangeFitsNumericType,
    resolveType,
    unifyTypes
  )
import JazzNext.Compiler.TypeInference.Types
  ( DataTypeBinding,
    ExpressionType (..),
    IntegerLiteralRange (..),
    NumericConstraint (..),
    TypeBinding (..),
    ScopeCapabilityFacts (..),
    TypeScheme (..),
    TypeEnv,
    emptyScopeCapabilityFacts
  )
import JazzNext.Compiler.ModuleInterface
  ( ModuleInterface (..),
    emptyModuleInterface,
    moduleExportForBinding
  )
import JazzNext.Compiler.WarningConfig
  ( WarningSettings,
    defaultWarningSettings
  )

-- | `InferenceResult` keeps the canonicalized expression plus analyzer warnings
-- and an `inferredErrors` list that contains both analyzer diagnostics and
-- local type errors discovered during checking.
data InferenceResult = InferenceResult
  { inferredExpr :: Expr,
    inferredWarnings :: [WarningRecord],
    inferredErrors :: [Diagnostic],
    inferredRuntimeTypeHints :: Map BindingRuntimeHintKey SignatureType,
    inferredModuleInterface :: ModuleInterface
  }
  deriving (Eq, Show)

data InferenceInputs = InferenceInputs
  { inferenceBuiltinMode :: BuiltinResolutionMode,
    inferenceWarningSettings :: WarningSettings,
    inferenceImportedTypes :: TypeEnv,
    inferenceImportedDataTypes :: Map Text DataTypeBinding,
    inferenceImportedCapabilities :: ScopeCapabilityFacts,
    inferenceImportedClassNames :: Set Text,
    inferenceCurrentModulePath :: Maybe [Text]
  }

-- This currently forwards analyzer diagnostics while the richer inference/type
-- pipeline is still being built in jazz-next.
inferExpression :: WarningSettings -> Expr -> IO InferenceResult
inferExpression = inferExpressionWithBuiltins ResolveKernelOnly

inferExpressionWithBuiltins :: BuiltinResolutionMode -> WarningSettings -> Expr -> IO InferenceResult
inferExpressionWithBuiltins builtinMode =
  inferExpressionWithBuiltinsAndHiddenStatements builtinMode Set.empty

inferExpressionWithBuiltinsAndHiddenStatements ::
  BuiltinResolutionMode ->
  Set Int ->
  WarningSettings ->
  Expr ->
  IO InferenceResult
inferExpressionWithBuiltinsAndHiddenStatements builtinMode hiddenStatementIndices settings =
  inferExpressionWithBuiltinsAndSourceUnitStatements
    builtinMode
    hiddenStatementIndices
    hiddenStatementIndices
    settings

inferExpressionWithBuiltinsAndSourceUnitStatements ::
  BuiltinResolutionMode ->
  Set Int ->
  Set Int ->
  WarningSettings ->
  Expr ->
  IO InferenceResult
inferExpressionWithBuiltinsAndSourceUnitStatements builtinMode hiddenStatementIndices preludeStatementIndices settings =
  inferExpressionWithInputsAndSourceUnitStatements
    (emptyInferenceInputs builtinMode settings)
    hiddenStatementIndices
    preludeStatementIndices

inferExpressionWithInputs :: InferenceInputs -> Expr -> IO InferenceResult
inferExpressionWithInputs inputs =
  inferExpressionWithInputsAndHiddenStatements inputs Set.empty

inferExpressionWithInputsAndHiddenStatements :: InferenceInputs -> Set Int -> Expr -> IO InferenceResult
inferExpressionWithInputsAndHiddenStatements inputs hiddenStatementIndices expr =
  inferExpressionWithInputsAndSourceUnitStatements inputs hiddenStatementIndices hiddenStatementIndices expr

inferExpressionWithInputsAndSourceUnitStatements :: InferenceInputs -> Set Int -> Set Int -> Expr -> IO InferenceResult
inferExpressionWithInputsAndSourceUnitStatements inputs hiddenStatementIndices preludeStatementIndices expr =
  {-# SCC "jazz-stage:type-inference" #-}
  do
  AnalysisResult _ warnings errors <-
    analyzeProgramWithInputs
      (analysisInputsForInference inputs)
      hiddenStatementIndices
      expr
  let (_, finalState) =
        inferExprTypeWithSourceUnitStatements
          preludeStatementIndices
          (inferenceBuiltinMode inputs)
          (inferenceImportedTypes inputs)
          (initialStateForInference inputs)
          expr
      typeErrors = reverse (inferErrorsRev finalState)
      runtimeTypeHints = inferRuntimeTypeHints finalState
  pure
    InferenceResult
      { inferredExpr = expr,
        inferredWarnings = warnings,
        inferredErrors = errors ++ typeErrors,
        inferredRuntimeTypeHints = runtimeTypeHints,
        inferredModuleInterface = moduleInterfaceFromState inputs expr finalState
      }

emptyInferenceInputs :: BuiltinResolutionMode -> WarningSettings -> InferenceInputs
emptyInferenceInputs builtinMode settings =
  InferenceInputs
    { inferenceBuiltinMode = builtinMode,
      inferenceWarningSettings = settings,
      inferenceImportedTypes = Map.empty,
      inferenceImportedDataTypes = Map.empty,
      inferenceImportedCapabilities = emptyScopeCapabilityFacts,
      inferenceImportedClassNames = Set.empty,
      inferenceCurrentModulePath = Nothing
    }

analysisInputsForInference :: InferenceInputs -> AnalysisInputs
analysisInputsForInference inputs =
  AnalysisInputs
    { analysisBuiltinMode = inferenceBuiltinMode inputs,
      analysisWarningSettings = inferenceWarningSettings inputs,
      analysisImportedValues =
        Map.map (const (AnalysisBinding Nothing True)) (inferenceImportedTypes inputs),
      analysisImportedClasses =
        Set.map
          (sourceName . mkIdentifier)
          ( Set.union
              (inferenceImportedClassNames inputs)
              (Map.keysSet (scopeClassFacts (inferenceImportedCapabilities inputs)))
          ),
      analysisModulePath = inferenceCurrentModulePath inputs
    }

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
              inferenceRuntimeHintPath = inferenceCurrentModulePath inputs
            }
      }

moduleInterfaceFromState :: InferenceInputs -> Expr -> InferState -> ModuleInterface
moduleInterfaceFromState inputs expr state =
  emptyModuleInterface
    { interfaceValueTypes =
        Map.fromList
          [ (moduleExportForBinding (renderName name) binding, binding)
            | name <- Set.toList declaredValues,
              Just binding <- [Map.lookup name (inferVisibleTypes state)]
          ],
      interfaceDataTypes = Map.restrictKeys (inferDataTypes state) declaredDataTypes,
      interfaceClassFacts = scopeClassFacts localCapabilities,
      interfaceGeneratedEqualityClassFacts = scopeGeneratedEqualityClassFacts localCapabilities,
      interfaceConcreteImplFacts = scopeConcreteImplFacts localCapabilities,
      interfaceClassMethods = scopeClassMethodSignatures localCapabilities,
      interfaceConcreteImplMethods = scopeConcreteImplMethods localCapabilities,
      interfaceRuntimeHints = inferRuntimeTypeHints state
    }
  where
    (declaredValues, declaredDataTypes) = declaredModuleNames expr
    localCapabilities =
      case inferenceCurrentModulePath inputs of
        Just modulePath -> Map.findWithDefault emptyScopeCapabilityFacts modulePath (inferModuleCapabilityFacts state)
        Nothing -> capabilityFactsFromState state

declaredModuleNames :: Expr -> (Set Name, Set Text)
declaredModuleNames expression =
  case expression of
    EBlock statements -> foldl' collect (Set.empty, Set.empty) statements
    _ -> (Set.empty, Set.empty)
  where
    collect (valueNames, dataTypeNames) statement =
      case statement of
        SLet name _ _
          | publicModuleValue name -> (Set.insert name valueNames, dataTypeNames)
          | otherwise -> (valueNames, dataTypeNames)
        SData _ typeName _ constructors ->
          ( foldl'
              (\names (DataConstructor constructorName _) -> Set.insert constructorName names)
              valueNames
              constructors,
            Set.insert (renderName typeName) dataTypeNames
          )
        _ -> (valueNames, dataTypeNames)

    publicModuleValue name =
      case name of
        GeneratedName {} -> False
        _ -> True

inferExpressionDefault :: Expr -> IO InferenceResult
inferExpressionDefault = inferExpression defaultWarningSettings

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

-- Core expressions do not retain inner-node source spans yet, so inference
-- reuses the enclosing statement span as the best available location metadata.
inferExprType ::
  BuiltinResolutionMode ->
  TypeEnv ->
  InferState ->
  Expr ->
  (Maybe ExpressionType, InferState)
inferExprType builtinMode env state expr =
  inferExprTypeWithSourceUnitStatements Set.empty builtinMode env state expr

inferExprTypeWithSourceUnitStatements ::
  Set Int ->
  BuiltinResolutionMode ->
  TypeEnv ->
  InferState ->
  Expr ->
  (Maybe ExpressionType, InferState)
inferExprTypeWithSourceUnitStatements preludeStatementIndices builtinMode env state expr =
  case expr of
    ELit literal -> (Just (literalExpressionType literal), checkLiteralType state literal)
    EVar name ->
      case Map.lookup name env of
        Just localType -> instantiateEnvBinding localType state
        Nothing ->
          case instantiateBuiltinType builtinMode nameText state of
            Just (builtinType, nextState) -> (Just builtinType, nextState)
            Nothing ->
              case instantiateQualifiedMethodType nameText state of
                Just qualifiedMethodResult -> qualifiedMethodResult
                Nothing -> (Nothing, state)
      where
        nameText = identifierText name
    ELambda parameterName bodyExpr ->
      let (parameterType, stateAfterParameter) = freshTypeVar state
          extendedEnv =
            Map.insert
              parameterName
              (PlainTypeBinding parameterType)
              env
          (bodyType, stateAfterBody) =
            inferExprType builtinMode extendedEnv stateAfterParameter bodyExpr
       in
        case bodyType of
          Just inferredBodyType ->
            ( Just (TFunctionType (resolveType stateAfterBody parameterType) inferredBodyType),
              stateAfterBody
            )
          Nothing -> (Nothing, stateAfterBody)
    EOperatorValue operatorSymbol ->
      case instantiateOperatorType operatorSymbol state of
        Just (operatorType, nextState) -> (Just operatorType, nextState)
        Nothing
          | isBuiltinOperatorSymbol operatorSymbol ->
              ( Nothing,
                addTypeError state (mkUnsupportedOperatorValueError operatorSymbol)
              )
        Nothing -> instantiateDeclaredOperatorBindingType env operatorSymbol state
    EList elements -> inferListType builtinMode env state elements
    ETuple elements -> inferTupleType builtinMode env state elements
    EApply functionExpr argumentExpr ->
      case qualifiedMethodApplicationSpine expr state of
        Just (methodName, methodKey, argumentExprs)
          | Map.notMember methodName env ->
              inferQualifiedMethodApplication inferExprType builtinMode env state methodKey argumentExprs
        Nothing ->
          inferBuiltinOperatorApplyOrGenericApply functionExpr argumentExpr
        _ ->
          inferBuiltinOperatorApplyOrGenericApply functionExpr argumentExpr
    ETypeApplication functionExpr typeArgumentSpan typeArgument ->
      inferExplicitTypeApplication inferExprType builtinMode env state functionExpr typeArgumentSpan typeArgument
    EIf conditionExpr thenExpr elseExpr ->
      let (conditionType, stateAfterCondition) =
            inferExprType builtinMode env state conditionExpr
          (thenType, stateAfterThen) =
            inferExprType builtinMode env stateAfterCondition thenExpr
          (elseType, stateAfterElse) =
            inferExprType builtinMode env stateAfterThen elseExpr
          stateAfterConditionCheck =
            case conditionType of
              Just inferredConditionType ->
                case unifyTypes inferredConditionType TBoolType stateAfterElse of
                  Just unifiedState -> unifiedState
                  Nothing ->
                    addTypeError
                      stateAfterElse
                      (mkIfConditionTypeError (resolveType stateAfterElse inferredConditionType))
              Nothing -> stateAfterElse
       in
        case (thenType, elseType) of
          (Just inferredThenType, Just inferredElseType) ->
            case unifyTypes inferredThenType inferredElseType stateAfterConditionCheck of
              Just unifiedState ->
                (Just (mergedUnifiedType unifiedState inferredThenType inferredElseType), unifiedState)
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
    EPatternCase scrutineeExpr caseArms ->
      let (maybeScrutineeType, stateAfterScrutinee) =
            inferExprType builtinMode env state scrutineeExpr
          (scrutineeType, stateWithScrutineeType) =
            case maybeScrutineeType of
              Just inferredScrutineeType ->
                (inferredScrutineeType, stateAfterScrutinee)
              Nothing ->
                freshTypeVar stateAfterScrutinee
       in inferPatternCaseType inferExprType builtinMode env scrutineeType stateWithScrutineeType caseArms
    EBinary operatorSymbol leftExpr rightExpr
      | hasOperatorRule operatorSymbol ->
          inferBuiltinBinaryOperatorType operatorSymbol leftExpr rightExpr
      | isBuiltinOperatorSymbol operatorSymbol ->
          inferBuiltinBinaryOperatorType operatorSymbol leftExpr rightExpr
      | otherwise ->
          inferExprType
            builtinMode
            env
            state
            (EApply (EApply (EOperatorValue operatorSymbol) leftExpr) rightExpr)
    ESectionLeft leftExpr operatorSymbol
      | hasOperatorRule operatorSymbol ->
          inferBuiltinSectionLeftOperatorType operatorSymbol leftExpr
      | isBuiltinOperatorSymbol operatorSymbol ->
          inferBuiltinSectionLeftOperatorType operatorSymbol leftExpr
      | otherwise ->
          inferExprType
            builtinMode
            env
            state
            (EApply (EOperatorValue operatorSymbol) leftExpr)
    ESectionRight operatorSymbol rightExpr
      | hasOperatorRule operatorSymbol ->
          inferBuiltinSectionRightOperatorType operatorSymbol rightExpr
      | isBuiltinOperatorSymbol operatorSymbol ->
          inferBuiltinSectionRightOperatorType operatorSymbol rightExpr
      | otherwise ->
          inferExprType
            builtinMode
            env
            state
            (declaredOperatorRightSectionExpr operatorSymbol rightExpr)
    EBlock statements -> inferScopeType preludeStatementIndices inferExprType builtinMode env state statements
  where
    inferBuiltinBinaryOperatorType operatorSymbol leftExpr rightExpr =
      let (binaryResult, _, _) =
            inferBuiltinBinaryOperatorTypeWithOperands operatorSymbol leftExpr rightExpr
       in binaryResult

    inferBuiltinBinaryOperatorTypeWithOperands operatorSymbol leftExpr rightExpr =
      let (leftType, stateAfterLeft) =
            inferExprType builtinMode env state leftExpr
          (rightType, stateAfterRight) =
            inferExprType builtinMode env stateAfterLeft rightExpr
       in case (leftType, rightType) of
            (Just inferredLeftType, Just inferredRightType) ->
              ( inferBinaryType
                  operatorSymbol
                  leftExpr
                  rightExpr
                  inferredLeftType
                  inferredRightType
                  stateAfterRight,
                Just inferredLeftType,
                Just inferredRightType
              )
            _ -> ((Nothing, stateAfterRight), leftType, rightType)

    inferBuiltinSectionLeftOperatorType operatorSymbol leftExpr =
      let (leftType, stateAfterLeft) =
            inferExprType builtinMode env state leftExpr
       in case leftType of
            Just inferredLeftType ->
              inferSectionLeftType
                operatorSymbol
                inferredLeftType
                stateAfterLeft
            Nothing -> (Nothing, stateAfterLeft)

    inferBuiltinSectionRightOperatorType operatorSymbol rightExpr =
      let (rightType, stateAfterRight) =
            inferExprType builtinMode env state rightExpr
       in case rightType of
            Just inferredRightType ->
              inferSectionRightType
                operatorSymbol
                inferredRightType
                stateAfterRight
            Nothing -> (Nothing, stateAfterRight)

    inferBuiltinOperatorApplyOrGenericApply functionExpr argumentExpr =
      case builtinOperatorApplicationSpine env expr of
        Just (operatorSymbol, maybeAliasScheme, leftExpr, rightExpr) ->
          let (binaryResult@(maybeBinaryType, stateAfterBinary), maybeLeftType, maybeRightType) =
                inferBuiltinBinaryOperatorTypeWithOperands operatorSymbol leftExpr rightExpr
           in case maybeBinaryType of
                Just _
                  | Just leftType <- maybeLeftType,
                    Just rightType <- maybeRightType ->
                  ( maybeBinaryType,
                    maybe
                      stateAfterBinary
                      (\aliasScheme -> applyOperatorAliasSchemeConstraints operatorSymbol aliasScheme leftType rightType stateAfterBinary)
                      maybeAliasScheme
                  )
                Nothing -> binaryResult
                _ -> binaryResult
        Nothing ->
          inferGenericApplyWithSectionFallback functionExpr argumentExpr

    inferGenericApplyWithSectionFallback functionExpr argumentExpr =
      let genericResult@(maybeGenericType, _) =
            inferGenericApplyType builtinMode env state functionExpr argumentExpr
       in case (maybeGenericType, builtinOperatorSectionApplication expr) of
            (Nothing, Just (operatorSymbol, leftExpr, rightExpr)) ->
              let binaryResult@(maybeBinaryType, _) =
                    inferBuiltinBinaryOperatorType operatorSymbol leftExpr rightExpr
               in case maybeBinaryType of
                    Just _ -> binaryResult
                    Nothing -> genericResult
            _ -> genericResult

inferGenericApplyType ::
  BuiltinResolutionMode ->
  TypeEnv ->
  InferState ->
  Expr ->
  Expr ->
  (Maybe ExpressionType, InferState)
inferGenericApplyType builtinMode env state functionExpr argumentExpr =
  let (functionType, stateAfterFunction) =
        inferExprType builtinMode env state functionExpr
      (argumentType, stateAfterArgument) =
        inferExprType builtinMode env stateAfterFunction argumentExpr
      (resultTypeVar, stateWithResultVar) = freshTypeVar stateAfterArgument
   in case (functionType, argumentType) of
        (Just inferredFunctionType, Just inferredArgumentType) ->
          case
              unifyTypes
                inferredFunctionType
                (TFunctionType inferredArgumentType resultTypeVar)
                stateWithResultVar of
            Just unifiedState ->
              case numericConversionLiteralDiagnostic builtinMode env functionExpr argumentExpr of
                Just diagnostic ->
                  (Nothing, addTypeError unifiedState diagnostic)
                Nothing ->
                  (Just (resolveType unifiedState resultTypeVar), unifiedState)
            Nothing ->
              ( Nothing,
                addTypeError
                  (discardFailedFunctionApplicationConstraints state stateWithResultVar)
                  ( mkApplyTypeError
                      (resolveType stateWithResultVar inferredFunctionType)
                      (resolveType stateWithResultVar inferredArgumentType)
                  )
              )
        _ -> (Nothing, discardFailedFunctionApplicationConstraints state stateWithResultVar)

discardFailedFunctionApplicationConstraints :: InferState -> InferState -> InferState
discardFailedFunctionApplicationConstraints stateBeforeFunction stateAfterApplication =
  modifyInferenceOutput
    ( \output ->
        output
          { outputDeferredConstraints =
              inferDeferredExplicitConstraints stateBeforeFunction,
            outputInferredConstraints =
              inferInferredClassConstraints stateBeforeFunction
          }
    )
    stateAfterApplication

qualifiedMethodApplicationSpine :: Expr -> InferState -> Maybe (Name, Text, [Expr])
qualifiedMethodApplicationSpine expr state =
  case applicationSpine expr of
    Just (methodName, argumentExprs)
      | let methodKey = identifierText methodName,
        qualifiedMethodClassIsVisible methodKey state ->
          Just (methodName, methodKey, argumentExprs)
    _ -> Nothing

applicationSpine :: Expr -> Maybe (Name, [Expr])
applicationSpine expr =
  go [] expr
  where
    go argumentExprs currentExpr =
      case currentExpr of
        EApply (EOperatorValue "$") functionExpr ->
          go argumentExprs functionExpr
        EApply functionExpr argumentExpr ->
          go (argumentExpr : argumentExprs) functionExpr
        EVar name ->
          Just (name, argumentExprs)
        _ ->
          Nothing

builtinOperatorApplicationSpine :: TypeEnv -> Expr -> Maybe (Text, Maybe TypeScheme, Expr, Expr)
builtinOperatorApplicationSpine env expr =
  case dollarAppliedBuiltinOperatorSectionApplication env expr of
    Just (operatorSymbol, leftExpr, rightExpr) ->
      Just (operatorSymbol, Nothing, leftExpr, rightExpr)
    Nothing ->
      case expr of
        EApply (EApply operatorExpr leftExpr) rightExpr -> do
          (operatorSymbol, maybeAliasScheme) <- builtinOperatorSymbolExpr env operatorExpr
          if hasOperatorRule operatorSymbol
            then Just (operatorSymbol, maybeAliasScheme, leftExpr, rightExpr)
            else Nothing
        _ -> Nothing

dollarAppliedBuiltinOperatorSectionApplication :: TypeEnv -> Expr -> Maybe (Text, Expr, Expr)
dollarAppliedBuiltinOperatorSectionApplication env expr =
  case expr of
    EApply (EApply dollarExpr sectionExpr) argumentExpr
      | builtinDollarOperatorExpr env dollarExpr ->
          builtinOperatorSectionApplication (EApply sectionExpr argumentExpr)
    _ -> Nothing

builtinOperatorSectionApplication :: Expr -> Maybe (Text, Expr, Expr)
builtinOperatorSectionApplication expr =
  case expr of
    EApply (ESectionLeft leftExpr operatorSymbol) rightExpr
      | builtinSectionOperatorSymbol operatorSymbol ->
          Just (operatorSymbol, leftExpr, rightExpr)
    EApply (ESectionRight operatorSymbol rightExpr) leftExpr
      | builtinSectionOperatorSymbol operatorSymbol ->
          Just (operatorSymbol, leftExpr, rightExpr)
    _ -> Nothing

builtinOperatorSymbolExpr :: TypeEnv -> Expr -> Maybe (Text, Maybe TypeScheme)
builtinOperatorSymbolExpr env expr =
  case expr of
    EOperatorValue operatorSymbol
      | isBuiltinOperatorSymbol operatorSymbol ->
          Just (operatorSymbol, Nothing)
    EApply dollarExpr operatorExpr
      | builtinDollarOperatorExpr env dollarExpr ->
          builtinOperatorSymbolExpr env operatorExpr
    EVar name ->
      case Map.lookup name env of
        Just (BuiltinOperatorAliasTypeBinding operatorSymbol) -> Just (operatorSymbol, Nothing)
        Just (OperatorAliasSchemeTypeBinding operatorSymbol typeScheme) -> Just (operatorSymbol, Just typeScheme)
        _ -> Nothing
    _ -> Nothing

literalExpressionType :: Literal -> ExpressionType
literalExpressionType literal =
  case literal of
    LInt value -> TIntegerLiteralType (singletonIntegerLiteralRange value)
    LFloat _ _ maybeTargetType ->
      case maybeTargetType of
        Just targetType -> TNumericType targetType
        Nothing -> TFloatType
    LBool _ -> TBoolType
    LChar _ -> TCharType
    LText _ -> TTextType

checkLiteralType :: InferState -> Literal -> InferState
checkLiteralType state literal =
  case literal of
    LFloat literalValue literalSource (Just targetType) ->
      case targetedFloatLiteralDiagnostic targetType literalValue literalSource of
        Just diagnostic -> addTypeError state diagnostic
        Nothing -> state
    _ -> state

inferListType ::
  BuiltinResolutionMode ->
  TypeEnv ->
  InferState ->
  [Expr] ->
  (Maybe ExpressionType, InferState)
inferListType builtinMode env state elements =
  case elements of
    [] ->
      let (elementType, nextState) = freshTypeVar state
       in (Just (TListType elementType), nextState)
    firstElement : restElements ->
      let (firstType, stateAfterFirst) =
            inferExprType builtinMode env state firstElement
          (finalElementType, finalState) =
            foldl
              step
              (firstType, stateAfterFirst)
              restElements
       in (TListType <$> finalElementType, finalState)
  where
    step :: (Maybe ExpressionType, InferState) -> Expr -> (Maybe ExpressionType, InferState)
    step (expectedType, stateAcc) element =
      let (actualType, stateAfterElement) =
            inferExprType builtinMode env stateAcc element
       in case (expectedType, actualType) of
            (Just inferredExpectedType, Just inferredActualType) ->
              case unifyTypes inferredExpectedType inferredActualType stateAfterElement of
                Just unifiedState ->
                  ( Just
                      ( mergeIntegerLiteralRanges
                          (resolveType unifiedState inferredExpectedType)
                          (resolveType unifiedState inferredActualType)
                      ),
                    unifiedState
                  )
                Nothing ->
                  ( Just inferredExpectedType,
                    addTypeError
                      stateAfterElement
                      ( mkListElementTypeMismatchError
                          (resolveType stateAfterElement inferredExpectedType)
                          (resolveType stateAfterElement inferredActualType)
                      )
                  )
            _ -> (expectedType, stateAfterElement)

inferTupleType ::
  BuiltinResolutionMode ->
  TypeEnv ->
  InferState ->
  [Expr] ->
  (Maybe ExpressionType, InferState)
inferTupleType builtinMode env state elements =
  go (Just []) state elements
  where
    go maybeReversedTypes stateAcc remainingElements =
      case remainingElements of
        [] ->
          (TTupleType . reverse <$> maybeReversedTypes, stateAcc)
        element : rest ->
          let (elementType, stateAfterElement) =
                inferExprType builtinMode env stateAcc element
              nextReversedTypes =
                case (maybeReversedTypes, elementType) of
                  (Just reversedTypes, Just inferredElementType) ->
                    Just (resolveType stateAfterElement inferredElementType : reversedTypes)
                  _ -> Nothing
           in go nextReversedTypes stateAfterElement rest

-- | Scope/type-signature handling for block expressions. This mirrors the
-- statement-order rules enforced by the analyzer while threading inferred types.
numericConversionLiteralDiagnostic :: BuiltinResolutionMode -> TypeEnv -> Expr -> Expr -> Maybe Diagnostic
numericConversionLiteralDiagnostic builtinMode env functionExpr argumentExpr =
  case (functionExpr, argumentExpr) of
    (EVar functionName, ELit (LInt literalValue)) ->
      case numericConversionTargetFromCallable builtinMode env functionName of
        Just targetType ->
          case numericTypeLiteralIntegerBounds targetType of
            Just bounds@(lowerBound, upperBound)
              | literalValue < lowerBound || literalValue > upperBound ->
                  Just (mkNumericConversionLiteralTypeError (identifierText functionName) literalValue targetType bounds)
            _ -> Nothing
        Nothing -> Nothing
    (EVar functionName, ELit (LFloat literalValue literalSource _)) ->
      case numericConversionTargetFromCallable builtinMode env functionName of
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

targetedFloatLiteralDiagnostic :: NumericType -> Double -> FractionalLiteralSource -> Maybe Diagnostic
targetedFloatLiteralDiagnostic targetType literalValue literalSource =
  case numericTypeFloatMax targetType of
    Just maxMagnitude
      | not (finiteFloat literalValue)
          || abs literalValue > maxMagnitude
          || fractionalLiteralExceedsMagnitude literalSource maxMagnitude ->
          Just (mkTargetedFractionalLiteralOverflowError literalValue targetType maxMagnitude)
    _ -> Nothing

finiteFloat :: Double -> Bool
finiteFloat value = not (isNaN value) && not (isInfinite value)

numericConversionTargetFromCallable :: BuiltinResolutionMode -> TypeEnv -> Name -> Maybe NumericType
numericConversionTargetFromCallable builtinMode env functionName =
  let nameText = identifierText functionName
   in case Map.lookup functionName env of
        Just (BuiltinAliasTypeBinding builtinSymbol) ->
          builtinSymbolNumericConversionTarget builtinSymbol
        Just _ ->
          Nothing
        Nothing ->
          lookupBuiltinSymbolInMode builtinMode nameText >>= builtinSymbolNumericConversionTarget

singletonIntegerLiteralRange :: Integer -> IntegerLiteralRange
singletonIntegerLiteralRange value = IntegerLiteralRange value value

mergedUnifiedType :: InferState -> ExpressionType -> ExpressionType -> ExpressionType
mergedUnifiedType state leftType rightType =
  mergeIntegerLiteralRanges (resolveType state leftType) (resolveType state rightType)

mergeIntegerLiteralRanges :: ExpressionType -> ExpressionType -> ExpressionType
mergeIntegerLiteralRanges leftType rightType =
  case (leftType, rightType) of
    (TIntegerLiteralType leftRange, TIntegerLiteralType rightRange) ->
      TIntegerLiteralType (combineIntegerLiteralRanges leftRange rightRange)
    (TIntegerLiteralType literalRange, numericType@(TNumericType concreteNumericType))
      | integerLiteralRangeFitsNumericType literalRange concreteNumericType -> numericType
    (numericType@(TNumericType concreteNumericType), TIntegerLiteralType literalRange)
      | integerLiteralRangeFitsNumericType literalRange concreteNumericType -> numericType
    (TIntegerLiteralType {}, TIntType) -> TIntType
    (TIntType, TIntegerLiteralType {}) -> TIntType
    (TListType leftElementType, TListType rightElementType) ->
      TListType (mergeIntegerLiteralRanges leftElementType rightElementType)
    (TTupleType leftElementTypes, TTupleType rightElementTypes)
      | length leftElementTypes == length rightElementTypes ->
          TTupleType (zipWith mergeIntegerLiteralRanges leftElementTypes rightElementTypes)
    (TDataType leftName leftArguments, TDataType rightName rightArguments)
      | leftName == rightName,
        length leftArguments == length rightArguments ->
          TDataType leftName (zipWith mergeIntegerLiteralRanges leftArguments rightArguments)
    (TFunctionType leftInputType leftOutputType, TFunctionType rightInputType rightOutputType) ->
      TFunctionType
        (mergeIntegerLiteralRanges leftInputType rightInputType)
        (mergeIntegerLiteralRanges leftOutputType rightOutputType)
    _ -> leftType

instantiateBuiltinType :: BuiltinResolutionMode -> Text -> InferState -> Maybe (ExpressionType, InferState)
instantiateBuiltinType builtinMode name state =
  case lookupBuiltinSymbolInMode builtinMode name of
    Just builtinSymbol -> instantiateBuiltinSymbolType builtinSymbol state
    Nothing -> Nothing

instantiateDeclaredOperatorBindingType :: TypeEnv -> Text -> InferState -> (Maybe ExpressionType, InferState)
instantiateDeclaredOperatorBindingType env operatorSymbol state =
  case Map.lookup (operatorBindingName operatorSymbol) env of
    Just binding ->
      instantiateEnvBinding binding state
    Nothing ->
      ( Nothing,
        addTypeError state (mkMissingOperatorBindingError operatorSymbol)
      )

declaredOperatorRightSectionExpr :: Text -> Expr -> Expr
declaredOperatorRightSectionExpr operatorSymbol rightExpr =
  ELambda
    leftParameter
    (EApply (EApply (EOperatorValue operatorSymbol) (EVar leftParameter)) rightExpr)
  where
    leftParameter = generatedName OperatorSectionLeft

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
       in Just (TFunctionType sourceType (TNumericType targetType), stateAfterNumericConstraint)
    Nothing ->
      instantiateBuiltinSymbolTypeByName (builtinSymbolName builtinSymbol) state

instantiateBuiltinSymbolTypeByName :: Text -> InferState -> Maybe (ExpressionType, InferState)
instantiateBuiltinSymbolTypeByName builtinName state =
  case builtinName of
    "hd" ->
      let (elementType, stateAfterElement) = freshTypeVar state
       in Just (TFunctionType (TListType elementType) elementType, stateAfterElement)
    "tl" ->
      let (elementType, stateAfterElement) = freshTypeVar state
       in Just (TFunctionType (TListType elementType) (TListType elementType), stateAfterElement)
    "map" ->
      let (sourceType, stateAfterSource) = freshTypeVar state
          (targetType, stateAfterTarget) = freshTypeVar stateAfterSource
       in
        Just
          ( TFunctionType
              (TFunctionType sourceType targetType)
              (TFunctionType (TListType sourceType) (TListType targetType)),
            stateAfterTarget
          )
    "filter" ->
      let (elementType, stateAfterElement) = freshTypeVar state
       in
        Just
          ( TFunctionType
              (TFunctionType elementType TBoolType)
              (TFunctionType (TListType elementType) (TListType elementType)),
            stateAfterElement
          )
    "print!" ->
      -- Stub-v1 runtime keeps `print!` as an impure primitive that returns the
      -- evaluated argument value unchanged so compile/runtime paths stay simple.
      let (valueType, stateAfterValueType) = freshTypeVar state
       in Just (TFunctionType valueType valueType, stateAfterValueType)
    "listPrependRaw" ->
      let (elementType, stateAfterElement) = freshTypeVar state
       in
        Just
          ( TFunctionType
              elementType
              (TFunctionType (TListType elementType) (TListType elementType)),
            stateAfterElement
          )
    "listReverseRaw" ->
      let (elementType, stateAfterElement) = freshTypeVar state
       in Just (TFunctionType (TListType elementType) (TListType elementType), stateAfterElement)
    "charToUInt32" ->
      Just (TFunctionType TCharType (TNumericType NumericUInt32), state)
    "charFromUInt32Raw" ->
      Just (TFunctionType (TNumericType NumericUInt32) (TListType TCharType), state)
    "charIsAlpha" ->
      Just (TFunctionType TCharType TBoolType, state)
    "charIsAlphaNum" ->
      Just (TFunctionType TCharType TBoolType, state)
    "charIsDigit" ->
      Just (TFunctionType TCharType TBoolType, state)
    "charIsSpace" ->
      Just (TFunctionType TCharType TBoolType, state)
    "charIsHexDigit" ->
      Just (TFunctionType TCharType TBoolType, state)
    "textLength" ->
      Just (TFunctionType TTextType TIntType, state)
    "textUnconsRaw" ->
      Just
        ( TFunctionType
            TTextType
            (TListType (TTupleType [TCharType, TTextType])),
          state
        )
    "textAppend" ->
      Just (TFunctionType TTextType (TFunctionType TTextType TTextType), state)
    "textAppendChar" ->
      Just (TFunctionType TTextType (TFunctionType TCharType TTextType), state)
    "textFromChars" ->
      Just (TFunctionType (TListType TCharType) TTextType, state)
    "readTextRaw!" ->
      Just (TFunctionType TTextType hostIOOutcomeType, state)
    "writeTextRaw!" ->
      Just
        ( TFunctionType
            TTextType
            (TFunctionType TTextType hostIOOutcomeType),
          state
        )
    "readStdinRaw!" ->
      Just (TFunctionType unitType hostIOOutcomeType, state)
    "writeStdoutRaw!" ->
      Just (TFunctionType TTextType hostIOOutcomeType, state)
    "writeStderrRaw!" ->
      Just (TFunctionType TTextType hostIOOutcomeType, state)
    "arguments!" ->
      Just (TFunctionType unitType (TListType TTextType), state)
    "exit!" ->
      Just (TFunctionType TIntType unitType, state)
    _ -> Nothing

hostIOOutcomeType :: ExpressionType
hostIOOutcomeType = TTupleType [TBoolType, TTextType, TTextType, TTextType]

unitType :: ExpressionType
unitType = TTupleType []
