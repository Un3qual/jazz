{-# LANGUAGE OverloadedStrings #-}

-- | Lightweight type inference layer for the current compiler subset. It
-- canonicalizes the lowered AST, reuses analyzer diagnostics, and adds the
-- small collection of type/runtime-compatibility checks implemented so far.
module JazzNext.Compiler.TypeInference
  ( InferenceInputs (..),
    InferenceResult (..),
    inferExpressionWithBuiltinsAndHiddenStatements,
    inferExpressionWithBuiltins,
    inferExpressionWithInputs,
    inferExpressionWithInputsAndHiddenStatements,
    inferExpression,
    inferExpressionDefault
  ) where

import Control.Applicative ((<|>))
import Data.Maybe (isJust)
import Data.List (foldl')
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
  ( ConstraintSignatureType (..),
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
    numericTypeFloatIntegerBounds,
    numericTypeFloatMax,
    numericTypeIntegerBounds,
    numericTypeIsIntegral,
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
    inferModuleCapabilityFacts,
    inferRuntimeTypeHints,
    inferVisibleTypes,
    initialInferState
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
    emptyModuleInterface
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
    inferredRuntimeTypeHints :: Map BindingRuntimeHintKey ConstraintSignatureType,
    inferredModuleInterface :: ModuleInterface
  }
  deriving (Eq, Show)

data InferenceInputs = InferenceInputs
  { inferenceBuiltinMode :: BuiltinResolutionMode,
    inferenceWarningSettings :: WarningSettings,
    inferenceImportedTypes :: TypeEnv,
    inferenceImportedDataTypes :: Map Text DataTypeBinding,
    inferenceImportedCapabilities :: ScopeCapabilityFacts,
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
  inferExpressionWithInputsAndHiddenStatements
    (emptyInferenceInputs builtinMode settings)
    hiddenStatementIndices

inferExpressionWithInputs :: InferenceInputs -> Expr -> IO InferenceResult
inferExpressionWithInputs inputs =
  inferExpressionWithInputsAndHiddenStatements inputs Set.empty

inferExpressionWithInputsAndHiddenStatements :: InferenceInputs -> Set Int -> Expr -> IO InferenceResult
inferExpressionWithInputsAndHiddenStatements inputs hiddenStatementIndices expr = do
  AnalysisResult _ warnings errors <-
    analyzeProgramWithInputs
      (analysisInputsForInference inputs)
      hiddenStatementIndices
      expr
  let (_, finalState) =
        inferExprType
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
          (Map.keysSet (scopeClassFacts (inferenceImportedCapabilities inputs))),
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
            { inferenceModulePath = inferenceCurrentModulePath inputs
            }
      }

moduleInterfaceFromState :: InferenceInputs -> Expr -> InferState -> ModuleInterface
moduleInterfaceFromState inputs expr state =
  emptyModuleInterface
    { interfaceValueTypes =
        Map.fromList
          [ (renderName name, binding)
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

modifyInferenceOutput :: (InferenceOutput -> InferenceOutput) -> InferState -> InferState
modifyInferenceOutput update state =
  state {inferOutput = update (inferOutput state)}

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
    ETypeApplication functionExpr typeArgument ->
      inferExplicitTypeApplication inferExprType builtinMode env state functionExpr typeArgument
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
    EBinary operatorSymbol leftExpr rightExpr ->
      case lookupOperatorRule operatorSymbol of
        Just _ ->
          inferBuiltinBinaryOperatorType operatorSymbol leftExpr rightExpr
        Nothing
          | isBuiltinOperatorSymbol operatorSymbol ->
              inferBuiltinBinaryOperatorType operatorSymbol leftExpr rightExpr
        Nothing ->
          inferExprType
            builtinMode
            env
            state
            (EApply (EApply (EOperatorValue operatorSymbol) leftExpr) rightExpr)
    ESectionLeft leftExpr operatorSymbol ->
      case lookupOperatorRule operatorSymbol of
        Just _ ->
          inferBuiltinSectionLeftOperatorType operatorSymbol leftExpr
        Nothing
          | isBuiltinOperatorSymbol operatorSymbol ->
              inferBuiltinSectionLeftOperatorType operatorSymbol leftExpr
        Nothing ->
          inferExprType
            builtinMode
            env
            state
            (EApply (EOperatorValue operatorSymbol) leftExpr)
    ESectionRight operatorSymbol rightExpr ->
      case lookupOperatorRule operatorSymbol of
        Just _ ->
          inferBuiltinSectionRightOperatorType operatorSymbol rightExpr
        Nothing
          | isBuiltinOperatorSymbol operatorSymbol ->
              inferBuiltinSectionRightOperatorType operatorSymbol rightExpr
        Nothing ->
          inferExprType
            builtinMode
            env
            state
            (declaredOperatorRightSectionExpr operatorSymbol rightExpr)
    EBlock statements -> inferScopeType inferExprType builtinMode env state statements
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
                  (discardFailedFunctionApplicationConstraints state stateAfterFunction stateWithResultVar)
                  ( mkApplyTypeError
                      (resolveType stateWithResultVar inferredFunctionType)
                      (resolveType stateWithResultVar inferredArgumentType)
                  )
              )
        _ -> (Nothing, stateWithResultVar)

discardFailedFunctionApplicationConstraints :: InferState -> InferState -> InferState -> InferState
discardFailedFunctionApplicationConstraints stateBeforeFunction _ stateAfterApplication =
  modifyInferenceOutput
    ( \output ->
        output
          { outputDeferredConstraints =
              inferDeferredExplicitConstraints stateBeforeFunction
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
          case lookupOperatorRule operatorSymbol of
            Just _ -> Just (operatorSymbol, maybeAliasScheme, leftExpr, rightExpr)
            Nothing -> Nothing
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

builtinSectionOperatorSymbol :: Text -> Bool
builtinSectionOperatorSymbol operatorSymbol =
  case lookupOperatorRule operatorSymbol of
    Just (NumericRule _) -> True
    Just StrictEqualityRule -> True
    _ -> False

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

literalExpressionType :: Literal -> ExpressionType
literalExpressionType literal =
  case literal of
    LInt value -> TIntegerLiteralType (singletonIntegerLiteralRange value)
    LFloat _ _ maybeTargetType ->
      case maybeTargetType of
        Just targetType -> TNumericType targetType
        Nothing -> TFloatType
    LBool _ -> TBoolType

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
      case unifyTypes leftType rightType state of
        Just stateAfterUnify ->
          let resolvedOperandType = numericBinaryOperandType operatorSymbol resultRule stateAfterUnify leftType rightType
           in constrainNumericOperand resolvedOperandType stateAfterUnify
        Nothing -> numericOperandError state
  where
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
    _ -> Nothing
