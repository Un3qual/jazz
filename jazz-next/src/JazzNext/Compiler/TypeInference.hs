{-# LANGUAGE OverloadedStrings #-}

-- | Lightweight type inference layer for the current compiler subset. It
-- canonicalizes the lowered AST, reuses analyzer diagnostics, and adds the
-- small collection of type/runtime-compatibility checks implemented so far.
module JazzNext.Compiler.TypeInference
  ( InferenceResult (..),
    inferExpressionWithBuiltinsAndHiddenStatements,
    inferExpressionWithBuiltins,
    inferExpression,
    inferExpressionDefault
  ) where

import Control.Applicative ((<|>))
import Data.List (foldl')
import Data.Maybe (isJust, isNothing)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as Text
import JazzNext.Compiler.Analyzer
  ( AnalysisResult (..),
    analyzeProgramWithBuiltinsAndHiddenStatements
  )
import JazzNext.Compiler.AST
  ( CaseArm (..),
    ClassMethodSignature (..),
    ConstraintSignatureType (..),
    DataConstructorArgument (..),
    DataConstructor (..),
    Expr (..),
    ImplMethod (..),
    Literal (..),
    NumericType (..),
    Pattern (..),
    SignatureConstraint (..),
    SignaturePayload (..),
    SignatureToken (..),
    SignatureType (..),
    Statement (..)
  )
import JazzNext.Compiler.BuiltinCatalog
  ( BuiltinResolutionMode (..),
    BuiltinSymbol,
    builtinNamesInMode,
    builtinSymbolName,
    builtinSymbolNumericConversionTarget,
    lookupBuiltinSymbolInMode,
    numericTypeFloatIntegerBounds,
    numericTypeFloatMax,
    numericTypeFromName,
    numericTypeIntegerBounds,
    numericTypeIsIntegral,
    numericTypeLiteralIntegerBounds,
    renderNumericTypeName
  )
import JazzNext.Compiler.CapabilityFacts
  ( concreteConstraintArgument,
    concreteImplFactKey,
    concreteImplFactClassName,
    constraintFunctionArgumentTypes,
    constraintImplFactKey,
    constraintSignatureAliasVariants,
    constraintSignatureTypeContainsClassParameter,
    constraintSignatureTypeVariableNamesInOrder,
    constraintSignatureTypesCompatible,
    identifierLooksLikeTypeVariable,
    normalizeConstraintSignatureName,
    qualifiedMethodKey,
    renderConstraintSignatureType,
    signaturePayloadConstraintType,
    splitQualifiedMethodKey,
    substituteClassMethodSignature
  )
import JazzNext.Compiler.Diagnostics
  ( Diagnostic (..),
    SourceSpan,
    WarningRecord,
    mkDiagnostic,
    setDiagnosticPrimarySpan,
    setDiagnosticRelatedSpan,
    setDiagnosticSubject
  )
import JazzNext.Compiler.FractionalLiteral
  ( FractionalLiteralSource,
    fractionalLiteralExceedsMagnitude,
    fractionalLiteralIntegralValue
  )
import JazzNext.Compiler.Identifier
  ( identifierText,
    mkIdentifier,
    qualifiedIdentifierText
  )
import JazzNext.Compiler.Name
  ( GeneratedNameKind (..),
    Name (..),
    NameNamespace (..),
    generatedName,
    operatorBindingName,
    qualifiedMemberName,
    renderName,
    sourceName
  )
import JazzNext.Compiler.RecursiveBindings
  ( collectBindingNames,
    freeVarsExprWithBound,
    freeVarsScopeWithBound,
    inferRecursiveGroupsOrdered,
    inferSelfRecursiveBindings
  )
import JazzNext.Compiler.Parser.Operator
  ( isBuiltinOperatorSymbol
  )
import JazzNext.Compiler.Pattern
  ( commonPatternBinderNames,
    extendBoundWithPattern,
    patternBinderNames
  )
import JazzNext.Compiler.RuntimeHints
  ( BindingRuntimeHintKey,
    bindingRuntimeHintKeyInModule
  )
import JazzNext.Compiler.TypeInference.Capabilities
import JazzNext.Compiler.TypeInference.Diagnostics
import JazzNext.Compiler.TypeInference.Pattern
  ( inferPatternCaseType,
    instantiateConstructorBinding
  )
import JazzNext.Compiler.TypeInference.State
  ( DeclarationState (..),
    DeferredExplicitConstraint (..),
    InferState (..),
    InferenceOutput (..),
    ModuleInferenceState (..),
    inferClassFacts,
    inferClassMethodSignatures,
    inferConcreteImplFacts,
    inferConcreteImplMethods,
    inferCurrentModuleLocalCapabilityFacts,
    inferCurrentModulePath,
    inferDataTypes,
    inferDeferredExplicitConstraints,
    inferErrorCount,
    inferErrorsRev,
    inferGeneratedEqualityClassFacts,
    inferInferredClassConstraints,
    inferModuleCapabilityFacts,
    inferNumericVars,
    inferRuntimeTypeHints,
    inferStrictEqualityVars,
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
  ( ClassMethodType (..),
    ConstructorArgumentType (..),
    DataTypeBinding (..),
    ExpressionType (..),
    ImplMethodType (..),
    IntegerLiteralRange (..),
    NumericConstraint (..),
    ScopeCapabilityFacts (..),
    TypeBinding (..),
    TypeEnv,
    TypeScheme (..),
    TypeSchemeConstraint (..),
    TypeSchemePrimitiveConstraint (..),
    emptyScopeCapabilityFacts
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
    inferredRuntimeTypeHints :: Map BindingRuntimeHintKey ConstraintSignatureType
  }
  deriving (Eq, Show)

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
inferExpressionWithBuiltinsAndHiddenStatements builtinMode hiddenStatementIndices settings expr = do
  AnalysisResult _ warnings errors <-
    analyzeProgramWithBuiltinsAndHiddenStatements
      builtinMode
      hiddenStatementIndices
      settings
      expr
  let (typeErrors, runtimeTypeHints) = collectExprTypeInfo builtinMode expr
  pure
    InferenceResult
      { inferredExpr = expr,
        inferredWarnings = warnings,
        inferredErrors = errors ++ typeErrors,
        inferredRuntimeTypeHints = runtimeTypeHints
      }

inferExpressionDefault :: Expr -> IO InferenceResult
inferExpressionDefault = inferExpression defaultWarningSettings

modifyDeclarationState :: (DeclarationState -> DeclarationState) -> InferState -> InferState
modifyDeclarationState update state =
  state {inferDeclarations = update (inferDeclarations state)}

modifyModuleInferenceState :: (ModuleInferenceState -> ModuleInferenceState) -> InferState -> InferState
modifyModuleInferenceState update state =
  state {inferModule = update (inferModule state)}

modifyInferenceOutput :: (InferenceOutput -> InferenceOutput) -> InferState -> InferState
modifyInferenceOutput update state =
  state {inferOutput = update (inferOutput state)}

collectExprTypeErrors :: BuiltinResolutionMode -> Expr -> [Diagnostic]
collectExprTypeErrors builtinMode expr =
  fst (collectExprTypeInfo builtinMode expr)

collectExprTypeInfo :: BuiltinResolutionMode -> Expr -> ([Diagnostic], Map BindingRuntimeHintKey ConstraintSignatureType)
collectExprTypeInfo builtinMode expr =
  let (_, finalState) =
        inferExprType
          builtinMode
          Map.empty
          initialInferState
          expr
   in (reverse (inferErrorsRev finalState), inferRuntimeTypeHints finalState)

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
        Just localType -> instantiateTypeBinding localType state
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
              inferQualifiedMethodApplication builtinMode env state methodKey argumentExprs
        Nothing ->
          inferBuiltinOperatorApplyOrGenericApply functionExpr argumentExpr
        _ ->
          inferBuiltinOperatorApplyOrGenericApply functionExpr argumentExpr
    ETypeApplication functionExpr typeArgument ->
      inferExplicitTypeApplication builtinMode env state functionExpr typeArgument
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
    EBlock statements -> inferScopeType builtinMode env state statements
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

inferExprTypeWithExpected ::
  BuiltinResolutionMode ->
  TypeEnv ->
  InferState ->
  ExpressionType ->
  Expr ->
  (Maybe ExpressionType, InferState)
inferExprTypeWithExpected builtinMode env state expectedType expr =
  case (resolveType state expectedType, expr) of
    (TFunctionType argumentType resultType, ELambda parameterName bodyExpr) ->
      let extendedEnv =
            Map.insert
              parameterName
              (PlainTypeBinding argumentType)
              env
          (bodyType, stateAfterBody) =
            inferExprTypeWithExpected builtinMode extendedEnv state resultType bodyExpr
       in
        case bodyType of
          Just inferredBodyType ->
            ( Just
                ( TFunctionType
                    (resolveType stateAfterBody argumentType)
                    inferredBodyType
                ),
              stateAfterBody
            )
          Nothing -> (Nothing, stateAfterBody)
    _ ->
      inferExprType builtinMode env state expr

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

builtinDollarOperatorExpr :: TypeEnv -> Expr -> Bool
builtinDollarOperatorExpr env expr =
  case expr of
    EOperatorValue "$" -> True
    EVar name ->
      case Map.lookup name env of
        Just (BuiltinOperatorAliasTypeBinding "$") -> True
        Just (OperatorAliasSchemeTypeBinding "$" _) -> True
        _ -> False
    _ -> False

builtinOperatorAliasSymbol :: Text -> Bool
builtinOperatorAliasSymbol operatorSymbol =
  case lookupOperatorRule operatorSymbol of
    Just (NumericRule _) -> True
    Just StrictEqualityRule -> True
    Just ApplicationRule -> True
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
      -- aliases, so there are no explicit capability facts to replay.
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

qualifiedMethodClassIsVisible :: Text -> InferState -> Bool
qualifiedMethodClassIsVisible methodKey state =
  case splitQualifiedMethodKey methodKey of
    Just (capabilityName, _) -> Map.member capabilityName (inferClassFacts state)
    Nothing -> False

inferQualifiedMethodApplication ::
  BuiltinResolutionMode ->
  TypeEnv ->
  InferState ->
  Text ->
  [Expr] ->
  (Maybe ExpressionType, InferState)
inferQualifiedMethodApplication builtinMode env state methodKey argumentExprs =
  let (argumentTypes, stateAfterArguments) =
        inferQualifiedMethodArguments builtinMode env state argumentExprs
   in case sequence argumentTypes of
        Nothing -> (Nothing, stateAfterArguments)
        Just typedArgumentTypes ->
          resolveQualifiedMethodApplicationType
            methodKey
            env
            stateAfterArguments
            (zip argumentExprs typedArgumentTypes)

inferQualifiedMethodArguments ::
  BuiltinResolutionMode ->
  TypeEnv ->
  InferState ->
  [Expr] ->
  ([Maybe ExpressionType], InferState)
inferQualifiedMethodArguments builtinMode env state argumentExprs =
  let (reversedTypes, finalState) = foldl' step ([], state) argumentExprs
   in (reverse reversedTypes, finalState)
  where
    step (typesAcc, stateAcc) argumentExpr =
      let (argumentType, stateAfterArgument) =
            inferExprType builtinMode env stateAcc argumentExpr
       in (argumentType : typesAcc, stateAfterArgument)

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
inferScopeType :: BuiltinResolutionMode -> TypeEnv -> InferState -> [Statement] -> (Maybe ExpressionType, InferState)
inferScopeType builtinMode initialEnv initialState statements =
  let (scopeType, finalState) =
        go initialEnv Nothing Nothing Map.empty Map.empty initialModuleBaselineFacts stateAfterBindingSeeds indexedStatements
   in (scopeType, restoreCapabilityFacts initialState finalState)
  where
    indexedStatements = zip [0 ..] statements
    recursiveGroupsByStatement =
      inferRecursiveGroupsOrdered
        ( Set.union
            (Map.keysSet initialEnv)
            (Set.map (sourceName . mkIdentifier) (builtinNamesInMode builtinMode))
        )
        indexedStatements
    selfRecursiveFunctionStatements =
      inferSelfRecursiveBindings exprContainsFunctionBranch indexedStatements
    bindingNamesByStatement = collectBindingNames indexedStatements
    signedBindingStatements = collectSignedBindingStatements indexedStatements
    statementsByIndex = Map.fromList indexedStatements
    (bindingSeedsByStatement, seededState) =
      allocateBindingSeeds indexedStatements initialState
    stateAfterBindingSeeds = seededState
    initialModuleBaselineFacts = capabilityFactsFromState initialState

    go env lastExprType pendingSignatureType pendingSignaturesByStatement recursiveGroupStartStates moduleBaselineFacts state remainingStatements =
      case remainingStatements of
        [] -> (lastExprType, state)
        (statementIndex, statement) : rest ->
          case statement of
            SModule _ modulePath ->
              go env lastExprType pendingSignatureType pendingSignaturesByStatement recursiveGroupStartStates moduleBaselineFacts (enterModuleCapabilityScope moduleBaselineFacts modulePath state) rest
            SImport _ modulePath maybeAlias maybeSymbolNames ->
              go env lastExprType pendingSignatureType pendingSignaturesByStatement recursiveGroupStartStates moduleBaselineFacts (importModuleCapabilityFacts modulePath maybeAlias maybeSymbolNames state) rest
            SClass {} ->
              let nextState = seedStatementCapabilityFact state statement
                  nextModuleBaselineFacts =
                    updateRootModuleBaselineFacts moduleBaselineFacts state nextState
               in go env lastExprType Nothing pendingSignaturesByStatement recursiveGroupStartStates nextModuleBaselineFacts nextState rest
            SImpl _ capabilityName arguments methods ->
              let seededState = seedStatementCapabilityFact state statement
                  nextState =
                    checkImplMethodBodies builtinMode env seededState capabilityName arguments methods
                  nextModuleBaselineFacts =
                    updateRootModuleBaselineFacts moduleBaselineFacts state nextState
               in go env lastExprType Nothing pendingSignaturesByStatement recursiveGroupStartStates nextModuleBaselineFacts nextState rest
            SData spanValue typeName typeParameters constructors ->
              let (nextEnv, nextState) =
                    registerDataConstructors spanValue typeName typeParameters constructors env state
               in go nextEnv lastExprType Nothing pendingSignaturesByStatement recursiveGroupStartStates moduleBaselineFacts nextState rest
            SSignature name signatureSpan signaturePayload ->
              let (nextPendingSignature, nextState) =
                    case signaturePayloadToSignatureType signaturePayload signatureState of
                      (Just signatureType, stateAfterSignature) ->
                        ( Just
                            ( PendingSignatureType
                                (identifierText name)
                                signatureSpan
                                (signaturePayloadDeclaredType signatureType)
                                (signaturePayloadExplicitConstraints signatureType)
                                (signaturePayloadVariableOrder signatureType)
                            ),
                          restoreCapabilityFacts state stateAfterSignature
                        )
                      (Nothing, stateAfterSignature) ->
                        ( Nothing,
                          addTypeError
                            (restoreCapabilityFacts state stateAfterSignature)
                            (mkInvalidSignatureTypeError signatureState (identifierText name) signatureSpan signaturePayload)
                        )
                  signatureState = state
               in go env lastExprType nextPendingSignature pendingSignaturesByStatement recursiveGroupStartStates moduleBaselineFacts nextState rest
            SLet name bindingSpan valueExpr ->
              let nameText = identifierText name
                  (envForStatement, stateForStatement) =
                    exposeVisibleRecursiveGroupSchemes statementIndex env state
                  recursiveGroupStartStatesForStatement =
                    rememberRecursiveGroupStart statementIndex stateForStatement recursiveGroupStartStates
                  matchingPendingSignature =
                    case pendingSignatureType of
                      Just pendingSignature
                        | pendingSignatureName pendingSignature == nameText ->
                            Just pendingSignature
                      _ -> Nothing
                  envWithRecursiveBindings =
                    recursiveBindingEnv
                      statementIndex
                      envForStatement
                      recursiveGroupsByStatement
                      bindingNamesByStatement
                      bindingSeedsByStatement
                  envWithBindingSeed =
                    case
                        ( shouldSeedSelfRecursiveFunction statementIndex name envForStatement,
                          Map.lookup statementIndex bindingSeedsByStatement
                        ) of
                      (True, Just bindingSeed) ->
                        Map.insert name (PlainTypeBinding bindingSeed) envWithRecursiveBindings
                      _ -> envWithRecursiveBindings
                  envWithPendingSignature =
                    case matchingPendingSignature of
                      Just pendingSignature ->
                        Map.insert
                          name
                          (PlainTypeBinding (pendingSignatureDeclaredType pendingSignature))
                          envWithBindingSeed
                      Nothing -> envWithBindingSeed
                  maybePreservedSchemeAliasBinding =
                    schemePreservingAliasBinding name envWithPendingSignature valueExpr
                  maybeExpectedValueType =
                    pendingSignatureDeclaredType <$> matchingPendingSignature
                  (rawValueType, rawStateAfterValue) =
                    case maybePreservedSchemeAliasBinding of
                      Just (SchemeTypeBinding typeScheme) ->
                        (Just (schemeResultType typeScheme), stateForStatement)
                      Just (OperatorAliasSchemeTypeBinding _ typeScheme) ->
                        (Just (schemeResultType typeScheme), stateForStatement)
                      _ ->
                        case maybeExpectedValueType of
                          Just expectedValueType ->
                            inferExprTypeWithExpected builtinMode envWithPendingSignature stateForStatement expectedValueType valueExpr
                          Nothing ->
                            inferExprType builtinMode envWithPendingSignature stateForStatement valueExpr
                  valueType =
                    targetedFractionalLiteralBindingType
                      nameText
                      matchingPendingSignature
                      valueExpr
                      rawValueType
                  stateAfterTargetedLiteralCheck =
                    case targetedFractionalLiteralDiagnostic nameText matchingPendingSignature valueExpr rawValueType of
                      Just diagnostic -> addTypeError rawStateAfterValue diagnostic
                      Nothing -> rawStateAfterValue
                  stateAfterValue =
                    annotateNewErrorsWithPrimarySpan bindingSpan stateForStatement stateAfterTargetedLiteralCheck
                  stateAfterBindingSeedCheck =
                    case (Map.lookup statementIndex bindingSeedsByStatement, valueType) of
                      (Just bindingSeed, Just inferredType) ->
                        case unifyTypes bindingSeed inferredType stateAfterValue of
                          Just unifiedState -> unifiedState
                          Nothing ->
                            addTypeError
                              stateAfterValue
                              ( mkBindingTypeMismatchError
                                  nameText
                                  (resolveType stateAfterValue bindingSeed)
                                  bindingSpan
                                  (resolveType stateAfterValue inferredType)
                              )
                      _ -> stateAfterValue
                  stateAfterSignatureCheck =
                    case (matchingPendingSignature, valueType) of
                      (Just pendingSignature, Just inferredType) ->
                        case
                            unifyTypes
                              (pendingSignatureDeclaredType pendingSignature)
                              inferredType
                              stateAfterBindingSeedCheck of
                          Just unifiedState -> unifiedState
                          Nothing ->
                            addTypeError
                              stateAfterBindingSeedCheck
                              ( mkSignatureTypeMismatchError
                                  nameText
                                  (pendingSignatureSpan pendingSignature)
                                  (resolveType stateAfterBindingSeedCheck (pendingSignatureDeclaredType pendingSignature))
                                  bindingSpan
                                  (resolveType stateAfterBindingSeedCheck inferredType)
                              )
                      _ -> stateAfterBindingSeedCheck
                  stateAfterExplicitConstraintCheck =
                    finalizeDeferredExplicitConstraintsAt
                      bindingSpan
                      stateForStatement
                      stateAfterSignatureCheck
                  nextBindingType =
                    case matchingPendingSignature of
                      Just pendingSignature ->
                        Just (resolveType stateAfterExplicitConstraintCheck (pendingSignatureDeclaredType pendingSignature))
                      _ ->
                        fmap
                          (defaultBindingLiteralTypes . resolveType stateAfterExplicitConstraintCheck)
                          (Map.lookup statementIndex bindingSeedsByStatement)
                  generalizationEnv =
                    generalizationEnvForStatement statementIndex envForStatement
                  droppedInferredSchemeVariables =
                    case (matchingPendingSignature, nextBindingType) of
                      (Just pendingSignature, Just _)
                        | shouldGeneralizeExplicitSignatureBinding generalizationEnv valueExpr pendingSignature ->
                            explicitBindingSchemeVariables generalizationEnv stateAfterExplicitConstraintCheck pendingSignature
                      (_, Just inferredType)
                        | shouldGeneralizeOrdinaryBinding statementIndex generalizationEnv valueExpr matchingPendingSignature ->
                            ordinaryBindingSchemeVariables generalizationEnv stateAfterExplicitConstraintCheck inferredType
                      _ -> Set.empty
                  stateAfterDroppedInferredMethodCheck =
                    case nextBindingType of
                      Just bindingType ->
                        addUnpreservedInferredMethodConstraintErrors
                          bindingSpan
                          generalizationEnv
                          stateForStatement
                          stateAfterExplicitConstraintCheck
                          bindingType
                          droppedInferredSchemeVariables
                      Nothing -> stateAfterExplicitConstraintCheck
                  maybeNextBinding =
                    maybePreservedSchemeAliasBinding
                      <|> nextBindingForValue
                        statementIndex
                        name
                        envForStatement
                        valueExpr
                        nextBindingType
                        matchingPendingSignature
                        stateAfterDroppedInferredMethodCheck
                  stateAfterRuntimeHint =
                    case
                        runtimeHintForBinding
                          stateAfterDroppedInferredMethodCheck
                          maybeNextBinding
                          nextBindingType
                      of
                        Just runtimeHint ->
                          modifyInferenceOutput
                            ( \output ->
                                output
                                  { outputRuntimeHints =
                                      Map.insert
                                        (bindingRuntimeHintKeyInModule (inferCurrentModulePath stateAfterDroppedInferredMethodCheck) name bindingSpan)
                                        runtimeHint
                                        (inferRuntimeTypeHints stateAfterDroppedInferredMethodCheck)
                                  }
                            )
                            stateAfterDroppedInferredMethodCheck
                        Nothing -> stateAfterDroppedInferredMethodCheck
                  stateAfterCapturedConstraintPrune =
                    case maybeNextBinding of
                      Just binding ->
                        pruneCapturedInferredClassConstraints stateForStatement binding stateAfterRuntimeHint
                      Nothing -> stateAfterRuntimeHint
                  nextPendingSignaturesByStatement =
                    case matchingPendingSignature of
                      Just pendingSignature ->
                        Map.insert statementIndex pendingSignature pendingSignaturesByStatement
                      Nothing -> pendingSignaturesByStatement
                  nextEnvBeforeRecursiveGroupGeneralization =
                    case maybeNextBinding of
                      Just binding -> Map.insert name binding env
                      Nothing -> env
                  (nextEnv, stateAfterRecursiveGroupPrune) =
                    generalizeCompletedRecursiveGroup
                      nextPendingSignaturesByStatement
                      statementIndex
                      nextEnvBeforeRecursiveGroupGeneralization
                      recursiveGroupStartStatesForStatement
                      stateAfterCapturedConstraintPrune
               in go nextEnv lastExprType Nothing nextPendingSignaturesByStatement recursiveGroupStartStatesForStatement moduleBaselineFacts stateAfterRecursiveGroupPrune rest
            SExpr exprSpan expr ->
              let (envForStatement, stateForStatement) =
                    exposeVisibleRecursiveGroupSchemes statementIndex env state
                  (exprType, rawStateAfterExpr) = inferExprType builtinMode envForStatement stateForStatement expr
                  stateAfterExpr =
                    annotateNewErrorsWithPrimarySpan exprSpan stateForStatement rawStateAfterExpr
                  stateAfterExplicitConstraintCheck =
                    finalizeDeferredExplicitConstraintsAt
                      exprSpan
                      stateForStatement
                      stateAfterExpr
                  stateAfterDroppedInferredMethodCheck =
                    case exprType of
                      Just resultType ->
                        addUnpreservedInferredMethodConstraintErrors
                          exprSpan
                          envForStatement
                          stateForStatement
                          stateAfterExplicitConstraintCheck
                          resultType
                          Set.empty
                      Nothing -> stateAfterExplicitConstraintCheck
               in go env exprType Nothing pendingSignaturesByStatement recursiveGroupStartStates moduleBaselineFacts stateAfterDroppedInferredMethodCheck rest

    nextBindingForValue ::
      Int ->
      Name ->
      TypeEnv ->
      Expr ->
      Maybe ExpressionType ->
      Maybe PendingSignatureType ->
      InferState ->
      Maybe TypeBinding
    nextBindingForValue statementIndex bindingName currentEnv valueExpr maybeInferredType maybePendingSignature state =
      let monomorphicBinding =
            if Set.member statementIndex (Map.keysSet recursiveGroupsByStatement)
              then PlainTypeBinding <$> maybeInferredType
              else ordinaryBindingForValue statementIndex currentEnv valueExpr maybeInferredType maybePendingSignature state
       in case valueExpr of
            EOperatorValue operatorSymbol
              | isNothing maybePendingSignature,
                builtinOperatorAliasSymbol operatorSymbol ->
                  Just (operatorAliasBinding operatorSymbol monomorphicBinding)
            EApply _ _
              | isNothing maybePendingSignature,
                Just (operatorSymbol, maybeAliasScheme) <- builtinOperatorSymbolExpr currentEnv valueExpr ->
                  Just (operatorAliasBinding operatorSymbol (SchemeTypeBinding <$> maybeAliasScheme))
            EVar builtinName ->
              let referencedName = identifierText builtinName
               in case Map.lookup builtinName currentEnv of
                    Just (BuiltinAliasTypeBinding builtinSymbol) ->
                      Just (BuiltinAliasTypeBinding builtinSymbol)
                    Just (BuiltinOperatorAliasTypeBinding operatorSymbol)
                      | isNothing maybePendingSignature ->
                          Just (BuiltinOperatorAliasTypeBinding operatorSymbol)
                    Just binding@(OperatorAliasSchemeTypeBinding _ _)
                      | isNothing maybePendingSignature ->
                          Just binding
                    Just constructorBinding@ConstructorTypeBinding {}
                      | isNothing maybePendingSignature,
                        isSyntheticAliasConstructorBinding bindingName builtinName ->
                          Just constructorBinding
                    Just _ ->
                      monomorphicBinding
                    Nothing ->
                      case lookupBuiltinSymbolInMode builtinMode referencedName of
                        Just builtinSymbol -> Just (BuiltinAliasTypeBinding builtinSymbol)
                        Nothing -> monomorphicBinding
            _ -> monomorphicBinding

    schemePreservingAliasBinding :: Name -> TypeEnv -> Expr -> Maybe TypeBinding
    schemePreservingAliasBinding bindingName currentEnv valueExpr =
      case valueExpr of
        EVar referencedName
          | isSyntheticModuleSchemeBridge bindingName referencedName ->
              case Map.lookup referencedName currentEnv of
                Just binding@(SchemeTypeBinding _) -> Just binding
                Just binding@(OperatorAliasSchemeTypeBinding _ _) -> Just binding
                _ -> Nothing
        _ -> Nothing

    isSyntheticModuleSchemeBridge :: Name -> Name -> Bool
    isSyntheticModuleSchemeBridge bindingName referencedName =
      isModuleReplayBridge bindingName || isModuleReplayBridge referencedName

    isSyntheticAliasConstructorBinding bindingName referencedName =
      isQualifiedName bindingName && isModuleReplayBridge referencedName

    isModuleReplayBridge name =
      case name of
        GeneratedName ModuleReplayBridge {} -> True
        _ -> False

    isQualifiedName name =
      case name of
        QualifiedName {} -> True
        _ -> False

    operatorAliasBinding :: Text -> Maybe TypeBinding -> TypeBinding
    operatorAliasBinding operatorSymbol maybeBinding =
      case maybeBinding of
        Just (SchemeTypeBinding typeScheme) ->
          OperatorAliasSchemeTypeBinding operatorSymbol typeScheme
        Just (OperatorAliasSchemeTypeBinding _ typeScheme) ->
          OperatorAliasSchemeTypeBinding operatorSymbol typeScheme
        _ ->
          BuiltinOperatorAliasTypeBinding operatorSymbol

    ordinaryBindingForValue ::
      Int ->
      TypeEnv ->
      Expr ->
      Maybe ExpressionType ->
      Maybe PendingSignatureType ->
      InferState ->
      Maybe TypeBinding
    ordinaryBindingForValue statementIndex currentEnv valueExpr maybeInferredType maybePendingSignature state =
      case maybeInferredType of
        Just _
          | Just pendingSignature <- maybePendingSignature,
            shouldGeneralizeExplicitSignatureBinding currentEnv valueExpr pendingSignature ->
              Just (generalizedExplicitSignatureBinding currentEnv state pendingSignature)
        Just inferredType
          | shouldGeneralizeOrdinaryBinding statementIndex currentEnv valueExpr maybePendingSignature ->
              Just (generalizedOrdinaryBinding currentEnv state inferredType)
        _ -> PlainTypeBinding <$> maybeInferredType

    shouldGeneralizeExplicitSignatureBinding ::
      TypeEnv ->
      Expr ->
      PendingSignatureType ->
      Bool
    shouldGeneralizeExplicitSignatureBinding currentEnv valueExpr pendingSignature =
      not (null (pendingSignatureExplicitConstraints pendingSignature))
        && not (isDirectConstructorAlias currentEnv valueExpr)

    shouldGeneralizeOrdinaryBinding ::
      Int ->
      TypeEnv ->
      Expr ->
      Maybe PendingSignatureType ->
      Bool
    shouldGeneralizeOrdinaryBinding statementIndex currentEnv valueExpr maybePendingSignature =
      isNothing maybePendingSignature
        && Set.notMember statementIndex signedBindingStatements
        && not (isDirectConstructorAlias currentEnv valueExpr)

    generalizationEnvForStatement :: Int -> TypeEnv -> TypeEnv
    generalizationEnvForStatement statementIndex currentEnv =
      case Map.lookup statementIndex recursiveGroupsByStatement of
        Just groupMembers ->
          foldl' (flip Map.delete) currentEnv (recursiveGroupBindingNames groupMembers)
        Nothing ->
          currentEnv

    recursiveGroupBindingNames :: [Int] -> Set Name
    recursiveGroupBindingNames groupMembers =
      Set.fromList
        [ bindingName
          | memberIndex <- groupMembers,
            Just bindingName <- [Map.lookup memberIndex bindingNamesByStatement]
        ]

    rememberRecursiveGroupStart :: Int -> InferState -> Map Int InferState -> Map Int InferState
    rememberRecursiveGroupStart statementIndex state groupStartStates =
      case Map.lookup statementIndex recursiveGroupsByStatement of
        Just groupMembers
          | not (null groupMembers),
            statementIndex == head groupMembers ->
              Map.insert (head groupMembers) state groupStartStates
        _ -> groupStartStates

    generalizeCompletedRecursiveGroup :: Map Int PendingSignatureType -> Int -> TypeEnv -> Map Int InferState -> InferState -> (TypeEnv, InferState)
    generalizeCompletedRecursiveGroup pendingSignatures statementIndex currentEnv groupStartStates state =
      case Map.lookup statementIndex recursiveGroupsByStatement of
        Just groupMembers
          | not (null groupMembers),
            statementIndex == last groupMembers ->
              let groupBindingNames =
                    Set.fromList
                      [ bindingName
                        | memberIndex <- groupMembers,
                          Just bindingName <- [Map.lookup memberIndex bindingNamesByStatement]
                      ]
                  envOutsideGroup =
                    foldl' (flip Map.delete) currentEnv groupBindingNames
                  nextEnv =
                    foldl'
                      (generalizeRecursiveGroupMember pendingSignatures envOutsideGroup state)
                      currentEnv
                      groupMembers
                  groupStartState =
                    Map.findWithDefault state (head groupMembers) groupStartStates
                  groupBindings =
                    [ binding
                      | memberIndex <- groupMembers,
                        Just bindingName <- [Map.lookup memberIndex bindingNamesByStatement],
                        Just binding <- [Map.lookup bindingName nextEnv]
                    ]
               in
                ( nextEnv,
                  pruneCapturedInferredClassConstraintsForBindings groupStartState groupBindings state
                )
        _ -> (currentEnv, state)

    exposeVisibleRecursiveGroupSchemes :: Int -> TypeEnv -> InferState -> (TypeEnv, InferState)
    exposeVisibleRecursiveGroupSchemes statementIndex currentEnv state =
      foldl' exposeGroup (currentEnv, state) recursiveGroups
      where
        recursiveGroups =
          Set.toList (Set.fromList (Map.elems recursiveGroupsByStatement))

        exposeGroup (envAcc, stateAcc) groupMembers
          | null groupMembers =
              (envAcc, stateAcc)
          | statementIndex `elem` groupMembers =
              (envAcc, stateAcc)
          | statementIndex > last groupMembers =
              (envAcc, stateAcc)
          | any (`Set.member` signedBindingStatements) groupMembers =
              (envAcc, stateAcc)
          | interleavedBindingFeedsLaterGroup statementIndex groupMembers =
              (envAcc, stateAcc)
          | laterGroupMemberDependsOnInterveningBinding statementIndex groupMembers =
              (envAcc, stateAcc)
          | null processedMembers =
              (envAcc, stateAcc)
          | otherwise =
              case previewRecursiveGroupState envAcc stateAcc statementIndex groupMembers of
                Nothing ->
                  (envAcc, stateAcc)
                Just previewState ->
                  let groupBindingNames =
                        Set.fromList
                          [ bindingName
                            | memberIndex <- groupMembers,
                              Just bindingName <- [Map.lookup memberIndex bindingNamesByStatement]
                          ]
                      envOutsideGroup =
                        foldl' (flip Map.delete) envAcc groupBindingNames
                      nextEnv =
                        foldl'
                          (exposeRecursiveGroupMember statementIndex envOutsideGroup previewState)
                          envAcc
                          processedMembers
                   in (nextEnv, previewState)
          where
            processedMembers = filter (< statementIndex) groupMembers

    interleavedBindingFeedsLaterGroup :: Int -> [Int] -> Bool
    interleavedBindingFeedsLaterGroup statementIndex groupMembers =
      case Map.lookup statementIndex statementsByIndex of
        Just (SLet bindingName _ _) ->
          any
                (laterGroupMemberReferences bindingName)
                (filter (> statementIndex) groupMembers)
        _ -> False

    laterGroupMemberReferences :: Name -> Int -> Bool
    laterGroupMemberReferences bindingName memberIndex =
      case Map.lookup memberIndex statementsByIndex of
        Just (SLet _ _ valueExpr) ->
          Set.member bindingName (freeVarsExprWithBound Set.empty valueExpr)
        _ -> False

    laterGroupMemberDependsOnInterveningBinding :: Int -> [Int] -> Bool
    laterGroupMemberDependsOnInterveningBinding statementIndex groupMembers =
      any memberDependsOnInterveningBinding (filter (> statementIndex) groupMembers)
      where
        groupMemberSet = Set.fromList groupMembers

        memberDependsOnInterveningBinding memberIndex =
          case Map.lookup memberIndex statementsByIndex of
            Just (SLet _ _ valueExpr) ->
              let referencedNames = freeVarsExprWithBound Set.empty valueExpr
               in any
                    (interveningBindingIsReferenced referencedNames memberIndex)
                    (Map.toList bindingNamesByStatement)
            _ -> False

        interveningBindingIsReferenced referencedNames memberIndex (bindingIndex, bindingName) =
          bindingIndex > statementIndex
            && bindingIndex < memberIndex
            && Set.notMember bindingIndex groupMemberSet
            && Set.member bindingName referencedNames

    previewRecursiveGroupState :: TypeEnv -> InferState -> Int -> [Int] -> Maybe InferState
    previewRecursiveGroupState currentEnv state statementIndex groupMembers =
      let previewState = foldl' previewMember state (filter (> statementIndex) groupMembers)
       in if previewIntroducedDiagnostics state previewState
            then Nothing
            else Just (discardPreviewDiagnostics state previewState)
      where
        previewMember stateAcc memberIndex =
          case Map.lookup memberIndex statementsByIndex of
            Just (SLet bindingName bindingSpan valueExpr) ->
              let nameText = identifierText bindingName
                  envWithRecursiveBindings =
                    recursiveBindingEnv
                      memberIndex
                      currentEnv
                      recursiveGroupsByStatement
                      bindingNamesByStatement
                      bindingSeedsByStatement
                  envWithBindingSeed =
                    case
                        ( shouldSeedSelfRecursiveFunction memberIndex bindingName currentEnv,
                          Map.lookup memberIndex bindingSeedsByStatement
                        ) of
                      (True, Just bindingSeed) ->
                        Map.insert bindingName (PlainTypeBinding bindingSeed) envWithRecursiveBindings
                      _ -> envWithRecursiveBindings
                  (valueType, rawStateAfterValue) =
                    inferExprType builtinMode envWithBindingSeed stateAcc valueExpr
                  stateAfterValue =
                    annotateNewErrorsWithPrimarySpan bindingSpan stateAcc rawStateAfterValue
               in case (Map.lookup memberIndex bindingSeedsByStatement, valueType) of
                    (Just bindingSeed, Just inferredType) ->
                      case unifyTypes bindingSeed inferredType stateAfterValue of
                        Just unifiedState -> unifiedState
                        Nothing ->
                          addTypeError
                            stateAfterValue
                            ( mkBindingTypeMismatchError
                                nameText
                                (resolveType stateAfterValue bindingSeed)
                                bindingSpan
                                (resolveType stateAfterValue inferredType)
                            )
                    _ -> stateAfterValue
            _ -> stateAcc

        discardPreviewDiagnostics originalState previewState =
          modifyInferenceOutput
            ( \output ->
                output
                  { outputErrorsRev = inferErrorsRev originalState,
                    outputRuntimeHints = inferRuntimeTypeHints originalState,
                    outputDeferredConstraints = inferDeferredExplicitConstraints originalState
                  }
            )
            previewState

        previewIntroducedDiagnostics originalState previewState =
          length (inferErrorsRev previewState) /= length (inferErrorsRev originalState)

    shouldSeedSelfRecursiveFunction :: Int -> Name -> TypeEnv -> Bool
    shouldSeedSelfRecursiveFunction statementIndex bindingName visibleEnv =
      Set.member statementIndex selfRecursiveFunctionStatements
        && Map.notMember bindingName visibleEnv

    exposeRecursiveGroupMember :: Int -> TypeEnv -> InferState -> TypeEnv -> Int -> TypeEnv
    exposeRecursiveGroupMember statementIndex envOutsideGroup state currentEnv memberIndex =
      case Map.lookup memberIndex bindingNamesByStatement of
        Just bindingName
          | latestBindingIndexBefore statementIndex bindingName == Just memberIndex ->
              generalizeRecursiveGroupMember Map.empty envOutsideGroup state currentEnv memberIndex
        _ -> currentEnv

    latestBindingIndexBefore :: Int -> Name -> Maybe Int
    latestBindingIndexBefore statementIndex bindingName =
      foldl' latest Nothing (Map.toList bindingNamesByStatement)
      where
        latest currentLatest (memberIndex, memberName)
          | memberIndex < statementIndex,
            memberName == bindingName =
              case currentLatest of
                Just previousIndex
                  | previousIndex > memberIndex -> currentLatest
                _ -> Just memberIndex
          | otherwise = currentLatest

    generalizeRecursiveGroupMember :: Map Int PendingSignatureType -> TypeEnv -> InferState -> TypeEnv -> Int -> TypeEnv
    generalizeRecursiveGroupMember pendingSignatures envOutsideGroup state currentEnv memberIndex =
      case (Map.lookup memberIndex statementsByIndex, Map.lookup memberIndex bindingNamesByStatement) of
        (Just (SLet _ _ valueExpr), Just bindingName)
          | Just pendingSignature <- Map.lookup memberIndex pendingSignatures,
            shouldGeneralizeExplicitSignatureBinding envOutsideGroup valueExpr pendingSignature ->
              Map.insert
                bindingName
                (generalizedExplicitSignatureBinding envOutsideGroup state pendingSignature)
                currentEnv
        (Just (SLet _ _ valueExpr), Just bindingName)
          | shouldGeneralizeOrdinaryBinding memberIndex envOutsideGroup valueExpr Nothing ->
              case Map.lookup memberIndex bindingSeedsByStatement of
                Just bindingSeed ->
                  Map.insert
                    bindingName
                    (generalizedOrdinaryBinding envOutsideGroup state bindingSeed)
                    currentEnv
                _ -> currentEnv
        _ -> currentEnv

checkImplMethodBodies ::
  BuiltinResolutionMode ->
  TypeEnv ->
  InferState ->
  Name ->
  [ConstraintSignatureType] ->
  [ImplMethod] ->
  InferState
checkImplMethodBodies builtinMode env state capabilityName arguments methods =
  case arguments of
    [implTarget]
      | concreteConstraintArgument implTarget,
        not (implMethodNamesHaveDuplicates methods) ->
          let implMethodEnv stateForBindings =
                Map.union env (currentImplMethodBindings implTarget stateForBindings)
              checkMethod stateAcc (ImplMethod methodName methodSpan methodExpr) =
                let methodKey = qualifiedMethodKey capabilityName methodName
                 in case Map.lookup methodKey (inferClassMethodSignatures stateAcc) of
                      Nothing ->
                        addTypeError
                          stateAcc
                          (mkImplMethodMissingClassMethodError methodKey methodSpan)
                      Just classMethodType ->
                        let (maybeExpectedType, stateAfterExpectedType) =
                              qualifiedMethodSignatureType
                                methodKey
                                classMethodType
                                (ImplMethodType implTarget)
                                stateAcc
                         in case maybeExpectedType of
                              Nothing ->
                                stateAfterExpectedType
                              Just expectedType ->
                                let (maybeMethodType, rawStateAfterMethod) =
                                      inferExprTypeWithExpected
                                        builtinMode
                                        (implMethodEnv stateAcc)
                                        stateAfterExpectedType
                                        expectedType
                                        methodExpr
                                    stateAfterMethod =
                                      annotateNewErrorsWithPrimarySpan methodSpan stateAfterExpectedType rawStateAfterMethod
                                    stateAfterMethodCheck =
                                      case maybeMethodType of
                                        Just methodType ->
                                          case unifyTypes expectedType methodType stateAfterMethod of
                                            Just unifiedState -> unifiedState
                                            Nothing ->
                                              addTypeError
                                                stateAfterMethod
                                                ( mkImplMethodTypeMismatchError
                                                    methodKey
                                                    methodSpan
                                                    (resolveType stateAfterMethod expectedType)
                                                    (resolveType stateAfterMethod methodType)
                                                )
                                        Nothing ->
                                          stateAfterMethod
                                 in finalizeDeferredExplicitConstraintsAt
                                      methodSpan
                                      stateAfterExpectedType
                                      stateAfterMethodCheck
           in foldl' checkMethod state methods
    _ -> state
  where
    implMethodNamesHaveDuplicates :: [ImplMethod] -> Bool
    implMethodNamesHaveDuplicates implMethods =
      let methodNames = map (\(ImplMethod methodName _ _) -> identifierText methodName) implMethods
       in length methodNames /= Set.size (Set.fromList methodNames)

    currentImplMethodBindings :: ConstraintSignatureType -> InferState -> TypeEnv
    currentImplMethodBindings implTarget stateForBindings =
      Map.fromList
        [ (qualifiedMemberName capabilityName methodName, PlainTypeBinding methodType)
          | ImplMethod methodName _ _ <- methods,
            let methodKey = qualifiedMethodKey capabilityName methodName,
            Just (ClassMethodType classParameter methodSignature) <- [Map.lookup methodKey (inferClassMethodSignatures stateForBindings)],
            Just methodType <- [classMethodPayloadToExpressionType stateForBindings classParameter implTarget methodSignature]
        ]

allocateBindingSeeds ::
  [(Int, Statement)] ->
  InferState ->
  (Map Int ExpressionType, InferState)
allocateBindingSeeds indexedStatements initialState =
  foldl' step (Map.empty, initialState) indexedStatements
  where
    step (bindingSeeds, state) (statementIndex, statement) =
      case statement of
        SLet {} ->
          let (bindingSeed, nextState) = freshTypeVar state
           in (Map.insert statementIndex bindingSeed bindingSeeds, nextState)
        _ -> (bindingSeeds, state)

-- Seed self-recursion before branch typing so mixed wrappers like
-- `if True \(x) -> f x else 0` do not skip recursive calls just because only
-- one branch exposes a function value.
exprContainsFunctionBranch :: Expr -> Bool
exprContainsFunctionBranch expr =
  case expr of
    ELambda {} -> True
    EIf _ thenExpr elseExpr ->
      exprContainsFunctionBranch thenExpr
        || exprContainsFunctionBranch elseExpr
    EPatternCase _ caseArms ->
      any
        (\(CaseArm _ _ bodyExpr) -> exprContainsFunctionBranch bodyExpr)
        caseArms
    EBlock statements ->
      scopeContainsFunctionBranch statements
    _ -> False

scopeContainsFunctionBranch :: [Statement] -> Bool
scopeContainsFunctionBranch statements =
  case reverse statements of
    SExpr _ expr : _ ->
      exprContainsFunctionBranchViaScopeBindings
        (collectScopeBindingExprs statements)
        Set.empty
        expr
    _ -> False
  where
    -- Mirror runtime block-shape detection so recursive lambda seeding stays
    -- aligned when a block returns a locally-bound lambda alias.
    exprContainsFunctionBranchViaScopeBindings scopeBindings visitedBindings scopeExpr =
      case scopeExpr of
        EVar bindingName ->
          case Map.lookup bindingName scopeBindings of
            Just bindingExpr
              | Set.notMember bindingName visitedBindings ->
                  exprContainsFunctionBranchViaScopeBindings
                    scopeBindings
                    (Set.insert bindingName visitedBindings)
                    bindingExpr
            _ -> False
        ELambda {} -> True
        EIf _ thenExpr elseExpr ->
          exprContainsFunctionBranchViaScopeBindings scopeBindings visitedBindings thenExpr
            || exprContainsFunctionBranchViaScopeBindings scopeBindings visitedBindings elseExpr
        EPatternCase _ caseArms ->
          any
            ( \(CaseArm _ _ bodyExpr) ->
                exprContainsFunctionBranchViaScopeBindings scopeBindings visitedBindings bodyExpr
            )
            caseArms
        EBlock nestedStatements ->
          scopeContainsFunctionBranch nestedStatements
        _ -> False

    collectScopeBindingExprs =
      foldl' collect Map.empty
      where
        collect scopeBindings statement =
          case statement of
            SLet bindingName _ valueExpr ->
              Map.insert bindingName valueExpr scopeBindings
            _ -> scopeBindings

recursiveBindingEnv ::
  Int ->
  TypeEnv ->
  Map Int [Int] ->
  Map Int Name ->
  Map Int ExpressionType ->
  TypeEnv
recursiveBindingEnv statementIndex env recursiveGroupsByStatement bindingNamesByStatement bindingSeedsByStatement =
  case Map.lookup statementIndex recursiveGroupsByStatement of
    Nothing -> env
    Just groupMembers ->
      foldl' insertBindingSeed env groupMembers
  where
    -- Preserve the declaration-time snapshot already visible in `env`; only
    -- missing peer names should be seeded into the recursive inference scope.
    insertBindingSeed envAcc memberIndex =
      case
          ( Map.lookup memberIndex bindingNamesByStatement,
            Map.lookup memberIndex bindingSeedsByStatement
          ) of
        (Just bindingNameText, Just bindingSeed)
          | Map.notMember bindingNameText env ->
              Map.insert bindingNameText (PlainTypeBinding bindingSeed) envAcc
        _ -> envAcc

collectSignedBindingStatements :: [(Int, Statement)] -> Set Int
collectSignedBindingStatements statements =
  case statements of
    (_, SSignature signatureName _ _) : (bindingIndex, SLet bindingName _ _) : rest
      | identifierText signatureName == identifierText bindingName ->
          Set.insert bindingIndex (collectSignedBindingStatements rest)
    _ : rest -> collectSignedBindingStatements rest
    [] -> Set.empty

isDirectConstructorAlias :: TypeEnv -> Expr -> Bool
isDirectConstructorAlias env expr =
  case expr of
    EVar referencedName ->
      case Map.lookup referencedName env of
        Just ConstructorTypeBinding {} -> True
        _ -> False
    _ -> False

generalizedOrdinaryBinding :: TypeEnv -> InferState -> ExpressionType -> TypeBinding
generalizedOrdinaryBinding env state expressionType =
  let resolvedType = defaultBindingLiteralTypes (resolveType state expressionType)
      schemeVariables = ordinaryBindingSchemeVariables env state expressionType
      schemeVariableOrder = orderedSchemeVariables (expressionTypeVariableOrder resolvedType) schemeVariables
      inferredClassConstraints = typeSchemeInferredClassConstraints state schemeVariables
      primitiveConstraints = typeSchemePrimitiveConstraints state schemeVariables
   in
    if Set.null schemeVariables
        && null inferredClassConstraints
        && null primitiveConstraints
      then PlainTypeBinding resolvedType
      else
        SchemeTypeBinding
          TypeScheme
            { schemeQuantifiedVariables = schemeVariables,
              schemeQuantifiedOrder = schemeVariableOrder,
              schemeClassConstraints = inferredClassConstraints,
              schemePrimitiveConstraints = primitiveConstraints,
              schemeDefiningCapabilities = typeSchemeDefiningFactsFromState state inferredClassConstraints,
              schemeResultType = resolvedType
            }

ordinaryBindingSchemeVariables :: TypeEnv -> InferState -> ExpressionType -> Set Int
ordinaryBindingSchemeVariables env state expressionType =
  let resolvedType = defaultBindingLiteralTypes (resolveType state expressionType)
      freeVariables = freeTypeVariables resolvedType
      environmentVariables = freeTypeVariablesInEnv state env
      quantifiedVariables = Set.difference freeVariables environmentVariables
   in Set.difference
        quantifiedVariables
        (numericConstrainedTypeVariables state)

addUnpreservedInferredMethodConstraintErrors ::
  SourceSpan ->
  TypeEnv ->
  InferState ->
  InferState ->
  ExpressionType ->
  Set Int ->
  InferState
addUnpreservedInferredMethodConstraintErrors spanValue env statementStartState state statementResultType schemeVariables =
  foldl'
    addUnpreservedClassConstraintError
    ( foldl'
        addUnpreservedMethodError
        (foldl' addUnpreservedConcreteMethodConstraintError state droppedConcreteMethodConstraints)
        droppedAmbiguousMethodKeys
    )
    droppedClassConstraints
  where
    droppedClassConstraints =
      dedupeTypeSchemeConstraints
        [ TypeSchemeInferredConstraint constraintName argumentType
          | TypeSchemeInferredConstraint constraintName argumentType <-
              newInferredClassConstraints statementStartState state,
            not (inferredConstraintTargetPreserved state schemeVariables argumentType),
            not (inferredConstraintTargetStillVisibleInEnv state env argumentType),
            inferredConstraintTargetConcrete state argumentType
              || ( not statementIntroducedErrors
                     && inferredConstraintTargetEscapesResult state statementResultType argumentType
                 )
        ]

    droppedMethodConstraints =
      dedupeTypeSchemeConstraints
        [ TypeSchemeMethodConstraint constraintName methodKey argumentType
          | TypeSchemeMethodConstraint constraintName methodKey argumentType <-
              newInferredClassConstraints statementStartState state,
            not (inferredConstraintTargetPreserved state schemeVariables argumentType),
            not (inferredConstraintTargetStillVisibleInEnv state env argumentType),
            not (concreteInferredMethodConstraintSatisfied state constraintName methodKey argumentType)
        ]

    droppedConcreteMethodConstraints =
      [ methodConstraint
        | methodConstraint@(TypeSchemeMethodConstraint _ _ argumentType) <- droppedMethodConstraints,
          inferredConstraintTargetConcrete state argumentType
      ]

    droppedAmbiguousMethodKeys =
      Set.toList
        ( Set.fromList
            [ methodKey
              | TypeSchemeMethodConstraint _ methodKey argumentType <- droppedMethodConstraints,
                not (inferredConstraintTargetConcrete state argumentType)
            ]
        )

    addUnpreservedMethodError stateAcc methodKey =
      addTypeError
        stateAcc
        (setDiagnosticPrimarySpan spanValue (mkAmbiguousQualifiedMethodBodyError methodKey))

    addUnpreservedConcreteMethodConstraintError stateAcc constraint =
      annotateNewErrorsWithPrimarySpan
        spanValue
        stateAcc
        ( resolveDeferredExplicitConstraint
            stateAcc
            (typeSchemeConstraintToDeferredExplicitConstraint (capabilityFactsFromState state) (capabilityFactsFromState state) constraint)
        )

    addUnpreservedClassConstraintError stateAcc constraint =
      annotateNewErrorsWithPrimarySpan
        spanValue
        stateAcc
        ( resolveDeferredExplicitConstraint
            stateAcc
            (typeSchemeConstraintToDeferredExplicitConstraint (capabilityFactsFromState state) (capabilityFactsFromState state) constraint)
        )

    statementIntroducedErrors =
      inferErrorCount state > inferErrorCount statementStartState

newInferredClassConstraints :: InferState -> InferState -> [TypeSchemeConstraint]
newInferredClassConstraints previousState state =
  take newConstraintCount (inferInferredClassConstraints state)
  where
    previousConstraintCount = length (inferInferredClassConstraints previousState)
    currentConstraintCount = length (inferInferredClassConstraints state)
    newConstraintCount = max 0 (currentConstraintCount - previousConstraintCount)

inferredConstraintTargetPreserved :: InferState -> Set Int -> ExpressionType -> Bool
inferredConstraintTargetPreserved state schemeVariables argumentType =
  let targetType = resolveType state argumentType
      targetVariables = freeTypeVariables targetType
   in not (Set.null targetVariables)
        && targetVariables `Set.isSubsetOf` schemeVariables

inferredConstraintTargetConcrete :: InferState -> ExpressionType -> Bool
inferredConstraintTargetConcrete state argumentType =
  let resolvedArgumentType = defaultLiteralTypes (resolveType state argumentType)
   in Set.null (freeTypeVariables resolvedArgumentType)
        && case expressionTypeToRuntimeHint resolvedArgumentType of
          Just _ -> True
          Nothing -> False

inferredConstraintTargetStillVisibleInEnv :: InferState -> TypeEnv -> ExpressionType -> Bool
inferredConstraintTargetStillVisibleInEnv state env argumentType =
  let targetType = resolveType state argumentType
      targetVariables = freeTypeVariables targetType
      environmentVariables = freeTypeVariablesInEnv state env
   in not (Set.null targetVariables)
        && targetVariables `Set.isSubsetOf` environmentVariables

inferredConstraintTargetEscapesResult :: InferState -> ExpressionType -> ExpressionType -> Bool
inferredConstraintTargetEscapesResult state statementResultType argumentType =
  let targetVariables = freeTypeVariables (resolveType state argumentType)
      resultVariables = freeTypeVariables (resolveType state statementResultType)
   in not (Set.null targetVariables)
        && not (Set.null (Set.intersection targetVariables resultVariables))

concreteInferredMethodConstraintSatisfied :: InferState -> Text -> Text -> ExpressionType -> Bool
concreteInferredMethodConstraintSatisfied state constraintName methodKey argumentType =
  let resolvedArgumentType = resolveType state argumentType
      facts = capabilityFactsFromState state
   in Set.null (freeTypeVariables resolvedArgumentType)
        && concreteInferredMethodConstraintHasUniqueCandidate facts state constraintName methodKey resolvedArgumentType

concreteInferredMethodConstraintHasUniqueCandidate :: ScopeCapabilityFacts -> InferState -> Text -> Text -> ExpressionType -> Bool
concreteInferredMethodConstraintHasUniqueCandidate facts state constraintName methodKey argumentType =
  case satisfyingMethodHints of
    [] -> False
    [_] -> True
    _
      | expressionTypeContainsUncommittedIntegerLiteral argumentType ->
          uniqueExactRuntimeCandidateHint state argumentType satisfyingMethodHints
      | otherwise -> True
  where
    satisfyingMethodHints =
      [ argumentHint
        | argumentHint <- inferredConstraintCandidateRuntimeHints facts state (Just methodKey) argumentType,
          concreteImplFactExists constraintName argumentHint facts,
          concreteImplMethodBodyExists methodKey argumentHint facts
      ]

uniqueExactRuntimeCandidateHint :: InferState -> ExpressionType -> [ConstraintSignatureType] -> Bool
uniqueExactRuntimeCandidateHint state argumentType candidateHints =
  case
      [ candidateHint
        | candidateHint <- candidateHints,
          constraintSignatureTypeExactlyMatchesExpressionType state candidateHint argumentType
      ] of
    [candidateHint] ->
      not (constraintSignatureTypeContainsList candidateHint)
    _ -> False

generalizedExplicitSignatureBinding ::
  TypeEnv ->
  InferState ->
  PendingSignatureType ->
  TypeBinding
generalizedExplicitSignatureBinding env state pendingSignature =
  let resolvedType = resolveType state (pendingSignatureDeclaredType pendingSignature)
      resolvedConstraints =
        map (resolveTypeSchemeConstraint state) (pendingSignatureExplicitConstraints pendingSignature)
      schemeVariables = explicitBindingSchemeVariables env state pendingSignature
      inferredClassConstraints =
        typeSchemeInferredClassConstraints state schemeVariables
      schemeConstraints =
        dedupeTypeSchemeConstraints (resolvedConstraints ++ inferredClassConstraints)
      primitiveConstraints = typeSchemePrimitiveConstraints state schemeVariables
   in if Set.null schemeVariables && null schemeConstraints && null primitiveConstraints
        then PlainTypeBinding resolvedType
        else
          SchemeTypeBinding
            TypeScheme
              { schemeQuantifiedVariables = schemeVariables,
                schemeQuantifiedOrder = orderedSchemeVariables (pendingSignatureVariableOrder pendingSignature) schemeVariables,
                schemeClassConstraints = schemeConstraints,
                schemePrimitiveConstraints = primitiveConstraints,
                schemeDefiningCapabilities = typeSchemeDefiningFactsFromState state schemeConstraints,
                schemeResultType = resolvedType
              }

pruneCapturedInferredClassConstraints :: InferState -> TypeBinding -> InferState -> InferState
pruneCapturedInferredClassConstraints statementStartState binding =
  pruneCapturedInferredClassConstraintsForBindings statementStartState [binding]

pruneCapturedInferredClassConstraintsForBindings :: InferState -> [TypeBinding] -> InferState -> InferState
pruneCapturedInferredClassConstraintsForBindings statementStartState bindings state =
  if null capturedConstraints
    then state
    else
      modifyInferenceOutput
        ( \output ->
            output
              { outputInferredConstraints =
                  filter
                    (not . capturedInScheme . resolveTypeSchemeConstraint state)
                    statementConstraints
                    ++ priorConstraints
              }
        )
        state
  where
    priorConstraintCount = length (inferInferredClassConstraints statementStartState)
    currentConstraints = inferInferredClassConstraints state
    statementConstraintCount = max 0 (length currentConstraints - priorConstraintCount)
    statementConstraints = take statementConstraintCount currentConstraints
    priorConstraints = drop statementConstraintCount currentConstraints
    capturedConstraints =
      [ resolveTypeSchemeConstraint state constraint
        | binding <- bindings,
          Just typeScheme <- [typeBindingScheme binding],
          constraint <- schemeClassConstraints typeScheme,
          typeSchemeConstraintIsInferred constraint
      ]
    capturedInScheme constraint =
      constraint `elem` capturedConstraints

typeBindingScheme :: TypeBinding -> Maybe TypeScheme
typeBindingScheme binding =
  case binding of
    SchemeTypeBinding typeScheme -> Just typeScheme
    OperatorAliasSchemeTypeBinding _ typeScheme -> Just typeScheme
    _ -> Nothing

typeSchemeConstraintIsInferred :: TypeSchemeConstraint -> Bool
typeSchemeConstraintIsInferred constraint =
  case constraint of
    TypeSchemeInferredConstraint {} -> True
    TypeSchemeMethodConstraint {} -> True
    TypeSchemeConstraint {} -> False

explicitBindingSchemeVariables :: TypeEnv -> InferState -> PendingSignatureType -> Set Int
explicitBindingSchemeVariables env state pendingSignature =
  let resolvedType = resolveType state (pendingSignatureDeclaredType pendingSignature)
      resolvedConstraints =
        map (resolveTypeSchemeConstraint state) (pendingSignatureExplicitConstraints pendingSignature)
      freeVariables =
        Set.union
          (freeTypeVariables resolvedType)
          (freeTypeVariablesInTypeSchemeConstraints resolvedConstraints)
      environmentVariables = freeTypeVariablesInEnv state env
   in Set.difference freeVariables environmentVariables

orderedSchemeVariables :: [Int] -> Set Int -> [Int]
orderedSchemeVariables preferredOrder schemeVariables =
  orderedVariables ++ Set.toList unorderedVariables
  where
    orderedVariables =
      filter (`Set.member` schemeVariables) preferredOrder
    unorderedVariables =
      Set.difference schemeVariables (Set.fromList orderedVariables)

expressionTypeVariableOrder :: ExpressionType -> [Int]
expressionTypeVariableOrder =
  dedupe . go
  where
    go expressionType =
      case expressionType of
        TIntType -> []
        TIntegerLiteralType {} -> []
        TFloatType -> []
        TNumericType {} -> []
        TBoolType -> []
        TListType elementType ->
          go elementType
        TTupleType elementTypes ->
          concatMap go elementTypes
        TDataType _ typeArguments ->
          concatMap go typeArguments
        TFunctionType inputType outputType ->
          go inputType ++ go outputType
        TVarType typeVar ->
          [typeVar]

    dedupe =
      goDedupe Set.empty

    goDedupe _ [] = []
    goDedupe seen (typeVar : rest)
      | Set.member typeVar seen = goDedupe seen rest
      | otherwise = typeVar : goDedupe (Set.insert typeVar seen) rest

typeSchemePrimitiveConstraints :: InferState -> Set Int -> [TypeSchemePrimitiveConstraint]
typeSchemePrimitiveConstraints state schemeVariables =
  numericConstraints ++ equalityConstraints
  where
    targetTypeFor typeVar =
      let targetType = resolveType state (TVarType typeVar)
          targetVariables = freeTypeVariables targetType
       in if not (Set.null targetVariables) && targetVariables `Set.isSubsetOf` schemeVariables
            then Just targetType
            else Nothing

    numericConstraints =
      [ TypeSchemeNumericConstraint numericConstraint targetType
        | (typeVar, numericConstraint) <- Map.toList (inferNumericVars state),
          Just targetType <- [targetTypeFor typeVar]
      ]

    equalityConstraints =
      [ TypeSchemeStrictEqualityConstraint targetType
        | typeVar <- Set.toList (inferStrictEqualityVars state),
          Just targetType <- [targetTypeFor typeVar]
      ]

numericConstrainedTypeVariables :: InferState -> Set Int
numericConstrainedTypeVariables =
  Map.keysSet . inferNumericVars

typeSchemeInferredClassConstraints :: InferState -> Set Int -> [TypeSchemeConstraint]
typeSchemeInferredClassConstraints state schemeVariables =
  dedupeTypeSchemeConstraints qualifiedMethodConstraints
  where
    qualifiedMethodConstraints =
      [ resolvedConstraint
        | constraint <- reverse (inferInferredClassConstraints state),
          Just resolvedConstraint <- [constraintForScheme constraint]
      ]

    constraintForScheme constraint =
      case constraint of
        TypeSchemeConstraint constraintName argumentType ->
          TypeSchemeConstraint constraintName <$> targetTypeFor argumentType
        TypeSchemeInferredConstraint constraintName argumentType ->
          TypeSchemeInferredConstraint constraintName <$> targetTypeFor argumentType
        TypeSchemeMethodConstraint constraintName methodKey argumentType ->
          TypeSchemeMethodConstraint constraintName methodKey <$> targetTypeFor argumentType

    targetTypeFor argumentType =
      let targetType = resolveType state argumentType
          targetVariables = freeTypeVariables targetType
       in if not (Set.null targetVariables) && targetVariables `Set.isSubsetOf` schemeVariables
            then Just targetType
            else Nothing

dedupeTypeSchemeConstraints :: [TypeSchemeConstraint] -> [TypeSchemeConstraint]
dedupeTypeSchemeConstraints =
  foldr insertIfMissing []
  where
    insertIfMissing constraint constraints
      | constraint `elem` constraints = constraints
      | otherwise = constraint : constraints

resolveTypeSchemeConstraint :: InferState -> TypeSchemeConstraint -> TypeSchemeConstraint
resolveTypeSchemeConstraint state constraint =
  case constraint of
    TypeSchemeConstraint constraintName argumentType ->
      TypeSchemeConstraint constraintName (resolveType state argumentType)
    TypeSchemeInferredConstraint constraintName argumentType ->
      TypeSchemeInferredConstraint constraintName (resolveType state argumentType)
    TypeSchemeMethodConstraint constraintName methodKey argumentType ->
      TypeSchemeMethodConstraint constraintName methodKey (resolveType state argumentType)

freeTypeVariablesInTypeSchemeConstraints :: [TypeSchemeConstraint] -> Set Int
freeTypeVariablesInTypeSchemeConstraints constraints =
  Set.unions (map freeTypeVariablesInTypeSchemeConstraint constraints)

freeTypeVariablesInTypeSchemeConstraint :: TypeSchemeConstraint -> Set Int
freeTypeVariablesInTypeSchemeConstraint constraint =
  case constraint of
    TypeSchemeConstraint _ argumentType ->
      freeTypeVariables argumentType
    TypeSchemeInferredConstraint _ argumentType ->
      freeTypeVariables argumentType
    TypeSchemeMethodConstraint _ _ argumentType ->
      freeTypeVariables argumentType

resolveTypeSchemePrimitiveConstraint :: InferState -> TypeSchemePrimitiveConstraint -> TypeSchemePrimitiveConstraint
resolveTypeSchemePrimitiveConstraint state primitiveConstraint =
  case primitiveConstraint of
    TypeSchemeNumericConstraint numericConstraint argumentType ->
      TypeSchemeNumericConstraint numericConstraint (resolveType state argumentType)
    TypeSchemeStrictEqualityConstraint argumentType ->
      TypeSchemeStrictEqualityConstraint (resolveType state argumentType)

freeTypeVariablesInTypeSchemePrimitiveConstraints :: [TypeSchemePrimitiveConstraint] -> Set Int
freeTypeVariablesInTypeSchemePrimitiveConstraints primitiveConstraints =
  Set.unions (map freeTypeVariablesInTypeSchemePrimitiveConstraint primitiveConstraints)

freeTypeVariablesInTypeSchemePrimitiveConstraint :: TypeSchemePrimitiveConstraint -> Set Int
freeTypeVariablesInTypeSchemePrimitiveConstraint primitiveConstraint =
  case primitiveConstraint of
    TypeSchemeNumericConstraint _ argumentType -> freeTypeVariables argumentType
    TypeSchemeStrictEqualityConstraint argumentType -> freeTypeVariables argumentType

freeTypeVariablesInEnv :: InferState -> TypeEnv -> Set Int
freeTypeVariablesInEnv state =
  Set.unions . map (freeTypeVariablesInBinding state) . Map.elems

freeTypeVariablesInBinding :: InferState -> TypeBinding -> Set Int
freeTypeVariablesInBinding state binding =
  case binding of
    PlainTypeBinding expressionType ->
      freeTypeVariables (resolveType state expressionType)
    SchemeTypeBinding typeScheme ->
      Set.difference
        ( Set.unions
            [ freeTypeVariables (resolveType state (schemeResultType typeScheme)),
              freeTypeVariablesInTypeSchemeConstraints (map (resolveTypeSchemeConstraint state) (schemeClassConstraints typeScheme)),
              freeTypeVariablesInTypeSchemePrimitiveConstraints (map (resolveTypeSchemePrimitiveConstraint state) (schemePrimitiveConstraints typeScheme))
            ]
        )
        (schemeQuantifiedVariables typeScheme)
    OperatorAliasSchemeTypeBinding _ typeScheme ->
      Set.difference
        ( Set.unions
            [ freeTypeVariables (resolveType state (schemeResultType typeScheme)),
              freeTypeVariablesInTypeSchemeConstraints (map (resolveTypeSchemeConstraint state) (schemeClassConstraints typeScheme)),
              freeTypeVariablesInTypeSchemePrimitiveConstraints (map (resolveTypeSchemePrimitiveConstraint state) (schemePrimitiveConstraints typeScheme))
            ]
        )
        (schemeQuantifiedVariables typeScheme)
    BuiltinAliasTypeBinding {} -> Set.empty
    BuiltinOperatorAliasTypeBinding {} -> Set.empty
    ConstructorTypeBinding _ _ argumentTypes ->
      Set.unions (map (freeTypeVariablesInConstructorArgument state) argumentTypes)

freeTypeVariablesInConstructorArgument :: InferState -> ConstructorArgumentType -> Set Int
freeTypeVariablesInConstructorArgument state argumentType =
  case argumentType of
    ConstructorArgumentMonomorphic expressionType ->
      freeTypeVariables (resolveType state expressionType)
    ConstructorArgumentParameter {} -> Set.empty
    ConstructorArgumentFresh -> Set.empty

freeTypeVariables :: ExpressionType -> Set Int
freeTypeVariables expressionType =
  case expressionType of
    TIntType -> Set.empty
    TIntegerLiteralType {} -> Set.empty
    TFloatType -> Set.empty
    TNumericType {} -> Set.empty
    TBoolType -> Set.empty
    TListType elementType ->
      freeTypeVariables elementType
    TTupleType elementTypes ->
      Set.unions (map freeTypeVariables elementTypes)
    TDataType _ typeArguments ->
      Set.unions (map freeTypeVariables typeArguments)
    TFunctionType inputType outputType ->
      Set.union (freeTypeVariables inputType) (freeTypeVariables outputType)
    TVarType typeVar ->
      Set.singleton typeVar

replaceTypeVariables :: Map Int ExpressionType -> ExpressionType -> ExpressionType
replaceTypeVariables replacements expressionType =
  case expressionType of
    TIntType -> TIntType
    TIntegerLiteralType literalRange -> TIntegerLiteralType literalRange
    TFloatType -> TFloatType
    TNumericType numericType -> TNumericType numericType
    TBoolType -> TBoolType
    TListType elementType ->
      TListType (replaceTypeVariables replacements elementType)
    TTupleType elementTypes ->
      TTupleType (map (replaceTypeVariables replacements) elementTypes)
    TDataType typeName typeArguments ->
      TDataType typeName (map (replaceTypeVariables replacements) typeArguments)
    TFunctionType inputType outputType ->
      TFunctionType
        (replaceTypeVariables replacements inputType)
        (replaceTypeVariables replacements outputType)
    TVarType typeVar ->
      Map.findWithDefault expressionType typeVar replacements

-- | Pending type signature state mirrors analyzer adjacency rules while
-- carrying the normalized declaration type for the next binding.
data PendingSignatureType = PendingSignatureType
  { pendingSignatureName :: Text,
    pendingSignatureSpan :: SourceSpan,
    pendingSignatureDeclaredType :: ExpressionType,
    pendingSignatureExplicitConstraints :: [TypeSchemeConstraint],
    pendingSignatureVariableOrder :: [Int]
  }

targetedFractionalLiteralBindingType ::
  Text ->
  Maybe PendingSignatureType ->
  Expr ->
  Maybe ExpressionType ->
  Maybe ExpressionType
targetedFractionalLiteralBindingType bindingName maybePendingSignature valueExpr maybeInferredType =
  case targetedFractionalLiteralType bindingName maybePendingSignature valueExpr maybeInferredType of
    Just targetType -> Just (TNumericType targetType)
    Nothing -> maybeInferredType

targetedFractionalLiteralDiagnostic ::
  Text ->
  Maybe PendingSignatureType ->
  Expr ->
  Maybe ExpressionType ->
  Maybe Diagnostic
targetedFractionalLiteralDiagnostic bindingName maybePendingSignature valueExpr maybeInferredType =
  case (targetedFractionalLiteralType bindingName maybePendingSignature valueExpr maybeInferredType, valueExpr) of
    (Just targetType, ELit (LFloat literalValue literalSource Nothing)) ->
      targetedFloatLiteralDiagnostic targetType literalValue literalSource
    _ -> Nothing

targetedFractionalLiteralType ::
  Text ->
  Maybe PendingSignatureType ->
  Expr ->
  Maybe ExpressionType ->
  Maybe NumericType
targetedFractionalLiteralType bindingName maybePendingSignature valueExpr maybeInferredType =
  case (maybePendingSignature, valueExpr, maybeInferredType) of
    (Just pendingSignature, ELit (LFloat _ _ Nothing), Just TFloatType)
      | pendingSignatureName pendingSignature == bindingName ->
          concreteFloatNumericType (pendingSignatureDeclaredType pendingSignature)
    _ -> Nothing

concreteFloatNumericType :: ExpressionType -> Maybe NumericType
concreteFloatNumericType expressionType =
  case expressionType of
    TNumericType NumericFloat16 -> Just NumericFloat16
    TNumericType NumericFloat32 -> Just NumericFloat32
    TNumericType NumericFloat64 -> Just NumericFloat64
    _ -> Nothing

registerDataConstructors :: SourceSpan -> Name -> [Name] -> [DataConstructor] -> TypeEnv -> InferState -> (TypeEnv, InferState)
registerDataConstructors spanValue typeName typeParameters constructors env initialState =
  case Map.lookup typeNameText (inferDataTypes initialState) of
    Just _ ->
      ( env,
        addTypeError
          initialState
          (mkDuplicateDataTypeDeclarationError typeNameText spanValue)
      )
    Nothing ->
      let (nextEnv, nextState, constructorPayloadsRev) =
            foldl' register (env, initialState, []) constructors
       in
        ( nextEnv,
          modifyDeclarationState
            ( \declarations ->
                declarations
                  { declarationDataTypes =
                      Map.insert
                        typeNameText
                        (DataTypeBinding typeParameters (reverse constructorPayloadsRev))
                        (inferDataTypes nextState)
                  }
            )
            nextState
        )
  where
    typeNameText = identifierText typeName

    register (envAcc, stateAcc, constructorPayloadsAcc) (DataConstructor constructorName constructorArguments) =
      let (argumentTypes, nextState) =
            constructorArgumentTypes typeParameters constructorArguments stateAcc
       in
        ( Map.insert
            constructorName
            (ConstructorTypeBinding typeName typeParameters argumentTypes)
            envAcc,
          nextState,
          argumentTypes : constructorPayloadsAcc
        )

constructorArgumentTypes :: [Name] -> [DataConstructorArgument] -> InferState -> ([ConstructorArgumentType], InferState)
constructorArgumentTypes typeParameters constructorArguments state
  | null typeParameters =
      let (argumentTypes, nextState) = freshTypeVars (length constructorArguments) state
       in (map ConstructorArgumentMonomorphic argumentTypes, nextState)
  | otherwise =
      foldl' collectArgument ([], state) constructorArguments
  where
    typeParameterNames = Set.fromList (map identifierText typeParameters)

    collectArgument (argumentTypes, stateAcc) constructorArgument =
      case constructorArgument of
        DataConstructorArgumentName argumentName
          | Set.member (identifierText argumentName) typeParameterNames ->
              (argumentTypes ++ [ConstructorArgumentParameter (identifierText argumentName)], stateAcc)
          | Just payloadType <- namedConstructorPayloadType argumentName ->
              (argumentTypes ++ [ConstructorArgumentMonomorphic payloadType], stateAcc)
          | otherwise ->
              ( argumentTypes ++ [ConstructorArgumentFresh],
                addTypeError stateAcc (mkUnknownConstructorPayloadTypeError argumentName)
              )
        DataConstructorArgumentOpaque ->
          (argumentTypes ++ [ConstructorArgumentFresh], stateAcc)

namedConstructorPayloadType :: Name -> Maybe ExpressionType
namedConstructorPayloadType =
  constraintSignatureTypeToExpressionType . ConstraintTypeName

-- | Instantiate local bindings and constructors at use sites. Constructors are
-- rendered as curried functions ending in their declared data type.
instantiateTypeBinding :: TypeBinding -> InferState -> (Maybe ExpressionType, InferState)
instantiateTypeBinding binding state =
  case binding of
    PlainTypeBinding expressionType ->
      (Just (resolveType state expressionType), state)
    SchemeTypeBinding typeScheme ->
      instantiateTypeScheme typeScheme state
    BuiltinAliasTypeBinding builtinSymbol ->
      case instantiateBuiltinSymbolType builtinSymbol state of
        Just (expressionType, nextState) -> (Just expressionType, nextState)
        Nothing -> (Nothing, state)
    BuiltinOperatorAliasTypeBinding operatorSymbol ->
      case instantiateOperatorType operatorSymbol state of
        Just (operatorType, nextState) -> (Just operatorType, nextState)
        Nothing -> (Nothing, state)
    OperatorAliasSchemeTypeBinding _ typeScheme ->
      instantiateTypeScheme typeScheme state
    ConstructorTypeBinding {} ->
      case instantiateConstructorBinding binding state of
        Just (constructorArgumentTypes', constructorResultType, nextState) ->
          ( Just
              (foldr TFunctionType constructorResultType constructorArgumentTypes'),
            nextState
          )
        Nothing -> (Nothing, state)

instantiateTypeScheme :: TypeScheme -> InferState -> (Maybe ExpressionType, InferState)
instantiateTypeScheme typeScheme state =
  let (freshBindings, nextState) =
        foldl'
          allocateFreshBinding
          (Map.empty, state)
          (orderedSchemeVariables quantifiedOrder quantifiedVariables)
      instantiatedType =
        replaceTypeVariables freshBindings expressionType
      instantiatedConstraints =
        map (instantiateTypeSchemeConstraint freshBindings) explicitConstraints
      instantiatedPrimitiveConstraints =
        map (instantiateTypeSchemePrimitiveConstraint freshBindings) primitiveConstraints
      stateWithPrimitiveConstraints =
        applyTypeSchemePrimitiveConstraints instantiatedPrimitiveConstraints nextState
      stateWithDeferredConstraints =
        deferExplicitConstraintsWithFacts
          (mergeCapabilityFacts definingFacts (capabilityFactsFromState state))
          definingFacts
          instantiatedConstraints
          stateWithPrimitiveConstraints
   in (Just (resolveType stateWithDeferredConstraints instantiatedType), stateWithDeferredConstraints)
  where
    quantifiedVariables = schemeQuantifiedVariables typeScheme
    quantifiedOrder = schemeQuantifiedOrder typeScheme
    explicitConstraints = schemeClassConstraints typeScheme
    primitiveConstraints = schemePrimitiveConstraints typeScheme
    definingFacts = schemeDefiningCapabilities typeScheme
    expressionType = schemeResultType typeScheme

    allocateFreshBinding (bindings, stateAcc) typeVar =
      let (freshType, nextState) = freshTypeVar stateAcc
       in (Map.insert typeVar freshType bindings, nextState)

inferExplicitTypeApplication ::
  BuiltinResolutionMode ->
  TypeEnv ->
  InferState ->
  Expr ->
  SignatureType ->
  (Maybe ExpressionType, InferState)
inferExplicitTypeApplication builtinMode env state functionExpr typeArgument =
  case explicitTypeApplicationScheme env functionExpr of
    Just typeScheme ->
      instantiateTypeSchemeWithExplicitArgument
        typeScheme
        (signatureTypeToExpressionType typeArgument)
        state
    Nothing ->
      let (maybeFunctionType, stateAfterFunction) =
            inferExprType builtinMode env state functionExpr
       in
        case maybeFunctionType of
          Just _ ->
            (Nothing, addTypeError stateAfterFunction mkExplicitTypeApplicationTargetError)
          Nothing -> (Nothing, stateAfterFunction)

explicitTypeApplicationScheme :: TypeEnv -> Expr -> Maybe TypeScheme
explicitTypeApplicationScheme env functionExpr =
  case functionExpr of
    EVar name ->
      Map.lookup name env >>= typeBindingScheme
    EOperatorValue operatorSymbol ->
      Map.lookup (operatorBindingName operatorSymbol) env >>= typeBindingScheme
    _ -> Nothing

instantiateTypeSchemeWithExplicitArgument ::
  TypeScheme ->
  ExpressionType ->
  InferState ->
  (Maybe ExpressionType, InferState)
instantiateTypeSchemeWithExplicitArgument typeScheme explicitArgumentType state =
  case orderedSchemeVariables quantifiedOrder quantifiedVariables of
    [] ->
      (Nothing, addTypeError state mkExplicitTypeApplicationTargetError)
    explicitTypeVar : remainingTypeVars ->
      let (freshBindings, nextState) =
            foldl'
              allocateFreshBinding
              (Map.singleton explicitTypeVar explicitArgumentType, state)
              remainingTypeVars
          instantiatedType =
            replaceTypeVariables freshBindings expressionType
          instantiatedConstraints =
            map (instantiateTypeSchemeConstraint freshBindings) explicitConstraints
          instantiatedPrimitiveConstraints =
            map (instantiateTypeSchemePrimitiveConstraint freshBindings) primitiveConstraints
          stateWithPrimitiveConstraints =
            applyTypeSchemePrimitiveConstraints instantiatedPrimitiveConstraints nextState
          stateWithDeferredConstraints =
            deferExplicitConstraintsWithFacts
              (mergeCapabilityFacts definingFacts (capabilityFactsFromState state))
              definingFacts
              instantiatedConstraints
              stateWithPrimitiveConstraints
       in (Just (resolveType stateWithDeferredConstraints instantiatedType), stateWithDeferredConstraints)
  where
    quantifiedVariables = schemeQuantifiedVariables typeScheme
    quantifiedOrder = schemeQuantifiedOrder typeScheme
    explicitConstraints = schemeClassConstraints typeScheme
    primitiveConstraints = schemePrimitiveConstraints typeScheme
    definingFacts = schemeDefiningCapabilities typeScheme
    expressionType = schemeResultType typeScheme

    allocateFreshBinding (bindings, stateAcc) typeVar =
      let (freshType, nextState) = freshTypeVar stateAcc
       in (Map.insert typeVar freshType bindings, nextState)

instantiateTypeSchemeConstraint :: Map Int ExpressionType -> TypeSchemeConstraint -> TypeSchemeConstraint
instantiateTypeSchemeConstraint replacements constraint =
  case constraint of
    TypeSchemeConstraint constraintName argumentType ->
      TypeSchemeConstraint constraintName (replaceTypeVariables replacements argumentType)
    TypeSchemeInferredConstraint constraintName argumentType ->
      TypeSchemeInferredConstraint constraintName (replaceTypeVariables replacements argumentType)
    TypeSchemeMethodConstraint constraintName methodKey argumentType ->
      TypeSchemeMethodConstraint constraintName methodKey (replaceTypeVariables replacements argumentType)

instantiateTypeSchemePrimitiveConstraint :: Map Int ExpressionType -> TypeSchemePrimitiveConstraint -> TypeSchemePrimitiveConstraint
instantiateTypeSchemePrimitiveConstraint replacements primitiveConstraint =
  case primitiveConstraint of
    TypeSchemeNumericConstraint numericConstraint argumentType ->
      TypeSchemeNumericConstraint numericConstraint (replaceTypeVariables replacements argumentType)
    TypeSchemeStrictEqualityConstraint argumentType ->
      TypeSchemeStrictEqualityConstraint (replaceTypeVariables replacements argumentType)

applyTypeSchemePrimitiveConstraints :: [TypeSchemePrimitiveConstraint] -> InferState -> InferState
applyTypeSchemePrimitiveConstraints primitiveConstraints state =
  foldl' applyPrimitiveConstraint state primitiveConstraints
  where
    applyPrimitiveConstraint stateAcc primitiveConstraint =
      case primitiveConstraint of
        TypeSchemeNumericConstraint numericConstraint argumentType ->
          case constrainNumericOperatorType numericConstraint argumentType stateAcc of
            Just nextState -> nextState
            Nothing ->
              addTypeError
                stateAcc
                (mkTypeSchemeNumericConstraintError numericConstraint (resolveType stateAcc argumentType))
        TypeSchemeStrictEqualityConstraint argumentType ->
          case resolveType stateAcc argumentType of
            TVarType typeVar ->
              addStrictEqualityTypeVarConstraint typeVar stateAcc
            resolvedType
              | supportsRuntimeEqualityType stateAcc resolvedType ->
                  stateAcc
              | otherwise ->
                  addTypeError stateAcc (mkTypeSchemeStrictEqualityConstraintError resolvedType)

deferExplicitConstraints :: [TypeSchemeConstraint] -> InferState -> InferState
deferExplicitConstraints explicitConstraints state =
  deferExplicitConstraintsWithFacts (capabilityFactsFromState state) (capabilityFactsFromState state) explicitConstraints state

deferExplicitConstraintsWithFacts :: ScopeCapabilityFacts -> ScopeCapabilityFacts -> [TypeSchemeConstraint] -> InferState -> InferState
deferExplicitConstraintsWithFacts facts structuralFacts explicitConstraints state
  | null explicitConstraints = state
  | otherwise =
      modifyInferenceOutput
        ( \output ->
            output
              { outputDeferredConstraints =
                  inferDeferredExplicitConstraints state
                    ++ map (typeSchemeConstraintToDeferredExplicitConstraint facts structuralFacts) explicitConstraints
              }
        )
        state

typeSchemeConstraintToDeferredExplicitConstraint :: ScopeCapabilityFacts -> ScopeCapabilityFacts -> TypeSchemeConstraint -> DeferredExplicitConstraint
typeSchemeConstraintToDeferredExplicitConstraint facts structuralFacts constraint =
  case constraint of
    TypeSchemeConstraint constraintName argumentType ->
      DeferredExplicitConstraint
        { deferredConstraintName = constraintName,
          deferredMethodKey = Nothing,
          deferredWasInferred = False,
          deferredArgumentType = argumentType,
          deferredVisibleFacts = facts,
          deferredStructuralFacts = structuralFacts
        }
    TypeSchemeInferredConstraint constraintName argumentType ->
      DeferredExplicitConstraint
        { deferredConstraintName = constraintName,
          deferredMethodKey = Nothing,
          deferredWasInferred = True,
          deferredArgumentType = argumentType,
          deferredVisibleFacts = facts,
          deferredStructuralFacts = structuralFacts
        }
    TypeSchemeMethodConstraint constraintName methodKey argumentType ->
      DeferredExplicitConstraint
        { deferredConstraintName = constraintName,
          deferredMethodKey = Just methodKey,
          deferredWasInferred = True,
          deferredArgumentType = argumentType,
          deferredVisibleFacts = facts,
          deferredStructuralFacts = structuralFacts
        }

finalizeDeferredExplicitConstraintsAt :: SourceSpan -> InferState -> InferState -> InferState
finalizeDeferredExplicitConstraintsAt spanValue statementStartState state =
  annotateNewErrorsWithPrimarySpan
    spanValue
    state
    (resolveStatementDeferredExplicitConstraints statementStartState state)

resolveStatementDeferredExplicitConstraints :: InferState -> InferState -> InferState
resolveStatementDeferredExplicitConstraints statementStartState state =
  foldl' resolveDeferredExplicitConstraint stateWithoutStatementConstraints statementConstraints
  where
    priorConstraints = inferDeferredExplicitConstraints statementStartState
    currentConstraints = inferDeferredExplicitConstraints state
    statementConstraints =
      drop (length priorConstraints) currentConstraints
    stateWithoutStatementConstraints =
      modifyInferenceOutput
        (\output -> output {outputDeferredConstraints = priorConstraints})
        state

resolveDeferredExplicitConstraint :: InferState -> DeferredExplicitConstraint -> InferState
resolveDeferredExplicitConstraint state deferredConstraint =
  let unresolvedArgumentType =
        resolveType state argumentType
      resolvedArgumentType =
        defaultLiteralTypes unresolvedArgumentType
   in
    if not (Set.null (freeTypeVariables unresolvedArgumentType))
      then addTypeError state (mkAmbiguousDeferredConstraintError inferredConstraint constraintName resolvedArgumentType)
      else
        case Map.lookup constraintName (scopeClassFacts facts) of
          Nothing ->
            addTypeError state (mkMissingExplicitConstraintClassError constraintName)
          Just classArity
            | classArity /= 1 ->
                addTypeError state (mkExplicitConstraintArityError constraintName classArity)
            | otherwise ->
                case constraintRuntimeHintsForDeferred facts state inferredConstraint constraintName maybeMethodKey unresolvedArgumentType of
                  [] ->
                    addTypeError state (mkAmbiguousDeferredConstraintError inferredConstraint constraintName resolvedArgumentType)
                  argumentHints ->
                    let implFactHints =
                          filter
                            (constraintImplFactExistsForDeferred facts inferredConstraint constraintName)
                            argumentHints
                        methodBodyHints methodKey =
                          filter
                            (\argumentHint -> concreteImplMethodBodyExists methodKey argumentHint facts)
                            implFactHints
                        ambiguousMethodBodyHints methodKey =
                          inferredConstraint
                            && expressionTypeContainsUncommittedIntegerLiteral unresolvedArgumentType
                            && length (methodBodyHints methodKey) > 1
                            && not (uniqueExactRuntimeCandidateHint state unresolvedArgumentType (methodBodyHints methodKey))
                        renderedImplFactKey =
                          constraintName <> "(" <> renderConstraintSignatureType (head argumentHints) <> ")"
                     in case maybeMethodKey of
                          Nothing
                            | not (null implFactHints) ->
                                state
                            | inferredConstraint
                                && inferredEqualityConstraintCanUseStructuralRuntimeEquality state structuralFacts maybeMethodKey constraintName resolvedArgumentType ->
                                state
                            | otherwise ->
                                addTypeError state (mkMissingExplicitConstraintImplFactError renderedImplFactKey)
                          Just methodKey
                            | null implFactHints ->
                                addTypeError state (mkMissingExplicitConstraintImplFactError renderedImplFactKey)
                            | ambiguousMethodBodyHints methodKey ->
                                addTypeError state (mkAmbiguousQualifiedMethodBodyError methodKey)
                            | not (null (methodBodyHints methodKey)) ->
                                state
                            | otherwise ->
                                addTypeError state (mkMissingImplMethodBodyError methodKey)
  where
    constraintName = deferredConstraintName deferredConstraint
    maybeMethodKey = deferredMethodKey deferredConstraint
    inferredConstraint = deferredWasInferred deferredConstraint
    argumentType = deferredArgumentType deferredConstraint
    facts = deferredVisibleFacts deferredConstraint
    structuralFacts = deferredStructuralFacts deferredConstraint

expressionTypeContainsUncommittedIntegerLiteral :: ExpressionType -> Bool
expressionTypeContainsUncommittedIntegerLiteral expressionType =
  case expressionType of
    TIntegerLiteralType {} -> True
    TListType elementType ->
      expressionTypeContainsUncommittedIntegerLiteral elementType
    TTupleType elementTypes ->
      any expressionTypeContainsUncommittedIntegerLiteral elementTypes
    TDataType _ typeArguments ->
      any expressionTypeContainsUncommittedIntegerLiteral typeArguments
    TFunctionType argumentType resultType ->
      expressionTypeContainsUncommittedIntegerLiteral argumentType
        || expressionTypeContainsUncommittedIntegerLiteral resultType
    _ -> False

constraintRuntimeHintsForDeferred ::
  ScopeCapabilityFacts ->
  InferState ->
  Bool ->
  Text ->
  Maybe Text ->
  ExpressionType ->
  [ConstraintSignatureType]
constraintRuntimeHintsForDeferred facts state inferredConstraint _ maybeMethodKey argumentType
  | inferredConstraint =
      inferredConstraintCandidateRuntimeHints facts state maybeMethodKey argumentType
  | otherwise =
      case expressionTypeToRuntimeHint (defaultLiteralTypes argumentType) of
        Just argumentHint -> [argumentHint]
        Nothing -> []

constraintImplFactExistsForDeferred :: ScopeCapabilityFacts -> Bool -> Text -> ConstraintSignatureType -> Bool
constraintImplFactExistsForDeferred facts inferredConstraint constraintName argumentHint =
  if inferredConstraint
    then concreteImplFactExists constraintName argumentHint facts
    else Set.member implFactKey (scopeConcreteImplFacts facts)
  where
    implFactKey = constraintName <> "(" <> renderConstraintSignatureType argumentHint <> ")"

inferredConstraintCandidateRuntimeHints :: ScopeCapabilityFacts -> InferState -> Maybe Text -> ExpressionType -> [ConstraintSignatureType]
inferredConstraintCandidateRuntimeHints facts state maybeMethodKey argumentType =
  dedupeConstraintSignatureTypes (defaultHint ++ methodCandidateHints)
  where
    defaultHint =
      case expressionTypeToRuntimeHint (defaultLiteralTypes argumentType) of
        Just argumentHint -> [argumentHint]
        Nothing -> []

    methodCandidateHints =
      case maybeMethodKey of
        Nothing -> []
        Just methodKey ->
          [ implTarget
            | ImplMethodType implTarget <- Map.findWithDefault [] methodKey (scopeConcreteImplMethods facts),
              constraintSignatureTypeMatchesExpressionType state implTarget argumentType
          ]

dedupeConstraintSignatureTypes :: [ConstraintSignatureType] -> [ConstraintSignatureType]
dedupeConstraintSignatureTypes =
  go Set.empty
  where
    go _ [] = []
    go seen (signatureType : rest)
      | Set.member rendered seen = go seen rest
      | otherwise = signatureType : go (Set.insert rendered seen) rest
      where
        rendered = renderConstraintSignatureType signatureType

constraintSignatureTypeMatchesExpressionType :: InferState -> ConstraintSignatureType -> ExpressionType -> Bool
constraintSignatureTypeMatchesExpressionType state signatureType expressionType =
  case (signatureType, resolveType state expressionType) of
    (ConstraintTypeName signatureName, TIntegerLiteralType literalRange) ->
      case numericTypeFromConstraintSignatureName (identifierText signatureName) of
        Just numericType ->
          numericTypeIsIntegral numericType
            && integerLiteralRangeFitsNumericType literalRange numericType
        Nothing ->
          False
    (ConstraintTypeList signatureElementType, TListType elementType) ->
      constraintSignatureTypeMatchesExpressionType state signatureElementType elementType
    (ConstraintTypeTuple signatureElementTypes, TTupleType elementTypes)
      | length signatureElementTypes == length elementTypes ->
          and (zipWith (constraintSignatureTypeMatchesExpressionType state) signatureElementTypes elementTypes)
    (ConstraintTypeApplication signatureName signatureArguments, TDataType typeName typeArguments)
      | normalizeConstraintSignatureName (identifierText signatureName)
          == normalizeConstraintSignatureName (identifierText typeName),
        length signatureArguments == length typeArguments ->
          and (zipWith (constraintSignatureTypeMatchesExpressionType state) signatureArguments typeArguments)
    (ConstraintTypeFunction signatureArgument signatureResult, TFunctionType argumentType resultType) ->
      constraintSignatureTypeMatchesExpressionType state signatureArgument argumentType
        && constraintSignatureTypeMatchesExpressionType state signatureResult resultType
    _ ->
      case expressionTypeToRuntimeHint (defaultLiteralTypes (resolveType state expressionType)) of
        Just argumentHint -> constraintSignatureTypesCompatible signatureType argumentHint
        Nothing -> False

numericTypeFromConstraintSignatureName :: Text -> Maybe NumericType
numericTypeFromConstraintSignatureName =
  numericTypeFromName . normalizeConstraintSignatureName

concreteImplFactExists :: Text -> ConstraintSignatureType -> ScopeCapabilityFacts -> Bool
concreteImplFactExists constraintName argumentHint facts =
  any
    (\candidateHint -> Set.member (constraintName <> "(" <> renderConstraintSignatureType candidateHint <> ")") (scopeConcreteImplFacts facts))
    (constraintSignatureAliasVariants argumentHint)

concreteImplMethodBodyExists :: Text -> ConstraintSignatureType -> ScopeCapabilityFacts -> Bool
concreteImplMethodBodyExists methodKey argumentHint facts =
  any
    (\(ImplMethodType implTarget) -> constraintSignatureTypesCompatible implTarget argumentHint)
    (Map.findWithDefault [] methodKey (scopeConcreteImplMethods facts))

inferredEqualityConstraintCanUseStructuralRuntimeEquality :: InferState -> ScopeCapabilityFacts -> Maybe Text -> Text -> ExpressionType -> Bool
inferredEqualityConstraintCanUseStructuralRuntimeEquality state facts maybeMethodKey constraintName argumentType =
  maybeMethodKey == Nothing
    && equalityConstraintNameCanUseStructuralRuntimeEquality state facts constraintName
    && structuralRuntimeEqualityType state argumentType

equalityConstraintNameCanUseStructuralRuntimeEquality :: InferState -> ScopeCapabilityFacts -> Text -> Bool
equalityConstraintNameCanUseStructuralRuntimeEquality state facts constraintName =
  activeEqualityClassName state == Just constraintName
    || generatedHiddenEqualityClassFact constraintName facts

generatedHiddenEqualityClassFact :: Text -> ScopeCapabilityFacts -> Bool
generatedHiddenEqualityClassFact constraintName facts =
  Set.member constraintName (scopeGeneratedEqualityClassFacts facts)
    && Map.lookup constraintName (scopeClassFacts facts) == Just 1

structuralRuntimeEqualityType :: InferState -> ExpressionType -> Bool
structuralRuntimeEqualityType state argumentType =
  case resolveType state argumentType of
    TListType elementType ->
      supportsRuntimeEqualityType state elementType
    TTupleType elementTypes ->
      all (supportsRuntimeEqualityType state) elementTypes
    TDataType typeName typeArguments ->
      supportsRuntimeEqualityType state (TDataType typeName typeArguments)
    _ ->
      False

instantiateQualifiedMethodType :: Text -> InferState -> Maybe (Maybe ExpressionType, InferState)
instantiateQualifiedMethodType nameText state =
  case splitQualifiedMethodKey nameText of
    Just (capabilityName, _)
      | Map.member capabilityName (inferClassFacts state) ->
          Just (resolveQualifiedMethodType nameText state)
    _ -> Nothing

resolveQualifiedMethodType :: Text -> InferState -> (Maybe ExpressionType, InferState)
resolveQualifiedMethodType methodKey state =
  case Map.lookup methodKey (inferClassMethodSignatures state) of
    Nothing
      | not (null (Map.findWithDefault [] methodKey (inferConcreteImplMethods state))) ->
          (Nothing, state)
      | otherwise ->
          (Nothing, addTypeError state (mkMissingClassMethodError methodKey))
    Just classMethodType ->
      case Map.findWithDefault [] methodKey (inferConcreteImplMethods state) of
        [] ->
          (Nothing, addTypeError state (mkMissingImplMethodBodyError methodKey))
        [implMethodType] ->
          qualifiedMethodSignatureType methodKey classMethodType implMethodType state
        _ ->
          (Nothing, addTypeError state (mkAmbiguousQualifiedMethodBodyError methodKey))

resolveQualifiedMethodApplicationType ::
  Text ->
  TypeEnv ->
  InferState ->
  [(Expr, ExpressionType)] ->
  (Maybe ExpressionType, InferState)
resolveQualifiedMethodApplicationType methodKey env state typedArguments =
  case Map.lookup methodKey (inferClassMethodSignatures state) of
    Nothing
      | not (null (Map.findWithDefault [] methodKey (inferConcreteImplMethods state))) ->
          (Nothing, state)
      | otherwise ->
          (Nothing, addTypeError state (mkMissingClassMethodError methodKey))
    Just classMethodType ->
      case inferQualifiedMethodRequirement methodKey classMethodType state argumentTypes of
        Just inferredRequirement ->
          inferredRequirement
        Nothing ->
          case Map.findWithDefault [] methodKey (inferConcreteImplMethods state) of
            [] ->
              (Nothing, addTypeError state (mkMissingImplMethodBodyError methodKey))
            [implMethodType] ->
              applyQualifiedMethodCandidateWithErrors methodKey classMethodType implMethodType state argumentTypes
            implMethodTypes ->
              selectQualifiedMethodCandidate methodKey classMethodType implMethodTypes env state typedArguments
  where
    argumentTypes = map snd typedArguments

inferQualifiedMethodRequirement ::
  Text ->
  ClassMethodType ->
  InferState ->
  [ExpressionType] ->
  Maybe (Maybe ExpressionType, InferState)
inferQualifiedMethodRequirement methodKey (ClassMethodType classParameter methodSignature) state argumentTypes = do
  (capabilityName, _) <- splitQualifiedMethodKey methodKey
  classArity <- Map.lookup capabilityName (inferClassFacts state)
  if classArity /= 1
    then Nothing
    else
      if not (classMethodSignatureHasTargetArgument classParameter methodSignature)
        then Nothing
        else
          let (classTarget, stateAfterClassTarget) = freshTypeVar state
           in do
                methodType <-
                  classMethodPayloadToGenericExpressionType
                    stateAfterClassTarget
                    classParameter
                    classTarget
                    methodSignature
                let (maybeResultType, stateAfterArguments) =
                      applyKnownFunctionArguments methodType argumentTypes stateAfterClassTarget
                    resolvedClassTarget = resolveType stateAfterArguments classTarget
                case maybeResultType of
                  Just resultType
                    | not (Set.null (freeTypeVariables resolvedClassTarget)) ->
                        Just
                          ( Just resultType,
                            addInferredMethodClassConstraint capabilityName methodKey resolvedClassTarget stateAfterArguments
                          )
                  _ ->
                    Nothing

classMethodSignatureHasTargetArgument :: Text -> SignaturePayload -> Bool
classMethodSignatureHasTargetArgument classParameter methodSignature =
  case signaturePayloadConstraintType methodSignature of
    Just signatureType ->
      let (argumentTypes, _) = constraintFunctionArgumentTypes signatureType
       in any (constraintSignatureTypeContainsClassParameter classParameter) argumentTypes
    Nothing ->
      False

selectQualifiedMethodCandidate ::
  Text ->
  ClassMethodType ->
  [ImplMethodType] ->
  TypeEnv ->
  InferState ->
  [(Expr, ExpressionType)] ->
  (Maybe ExpressionType, InferState)
selectQualifiedMethodCandidate methodKey classMethodType implMethodTypes env state typedArguments =
  case preferredCandidates of
    [] ->
      ( Nothing,
        addTypeError state (mkNoMatchingQualifiedMethodBodyError methodKey (resolvedArgumentTypes state))
      )
    [(matchedType, matchedState)] ->
      (Just matchedType, matchedState)
    _ ->
      ( Nothing,
        addTypeError state (mkAmbiguousQualifiedMethodBodyForArgumentsError methodKey (resolvedArgumentTypes state))
      )
  where
    preferredCandidates =
      case exactMatchingCandidates of
        [] -> matchingCandidates
        exactMatches -> exactMatches

    exactMatchingCandidates =
      filterExactMatches matchingCandidatesWithTargets

    matchingCandidates =
      map
        (\(_, matchedType, matchedState) -> (matchedType, matchedState))
        matchingCandidatesWithTargets

    matchingCandidatesWithTargets =
      foldr collectMatch [] implMethodTypes

    collectMatch implMethodType matches =
      case applyQualifiedMethodCandidate methodKey classMethodType implMethodType state argumentTypes of
        (Just matchedType, matchedState) -> (implMethodType, matchedType, matchedState) : matches
        (Nothing, _) -> matches

    filterExactMatches candidates =
      [ (matchedType, matchedState)
        | (implMethodType, matchedType, matchedState) <- candidates,
          qualifiedMethodCandidateExactlyMatchesArguments state env classMethodType implMethodType typedArguments
      ]

    resolvedArgumentTypes stateForRendering =
      map (resolveType stateForRendering) argumentTypes

    argumentTypes = map snd typedArguments

qualifiedMethodCandidateExactlyMatchesArguments ::
  InferState ->
  TypeEnv ->
  ClassMethodType ->
  ImplMethodType ->
  [(Expr, ExpressionType)] ->
  Bool
qualifiedMethodCandidateExactlyMatchesArguments state env (ClassMethodType classParameter methodSignature) (ImplMethodType implTarget) typedArguments =
  case (signaturePayloadConstraintType methodSignature, substituteClassMethodSignature classParameter implTarget methodSignature) of
    (Just genericSignature, Just substitutedSignature) ->
      let (genericArgumentTypes, _) = constraintFunctionArgumentTypes genericSignature
          (candidateArgumentTypes, _) = constraintFunctionArgumentTypes substitutedSignature
          suppliedArgumentCount = length typedArguments
          suppliedGenericArgumentTypes = take suppliedArgumentCount genericArgumentTypes
          suppliedCandidateArgumentTypes = take suppliedArgumentCount candidateArgumentTypes
          targetArgumentPositions =
            map (constraintSignatureTypeContainsClassParameter classParameter) suppliedGenericArgumentTypes
       in suppliedArgumentCount <= length genericArgumentTypes
            && suppliedArgumentCount <= length candidateArgumentTypes
            && or targetArgumentPositions
            && and
              ( zipWith3
                  exactCandidateArgumentMatches
                  targetArgumentPositions
                  suppliedCandidateArgumentTypes
                  typedArguments
              )
    _ ->
      False
  where
    exactCandidateArgumentMatches targetArgumentPosition signatureType (argumentExpr, expressionType) =
      not targetArgumentPosition
        || constraintSignatureTypeExactlyMatchesExpressionType state signatureType expressionType
          && constraintSignatureExpressionHasExactEvidence env signatureType argumentExpr

constraintSignatureExpressionHasExactEvidence :: TypeEnv -> ConstraintSignatureType -> Expr -> Bool
constraintSignatureExpressionHasExactEvidence env signatureType argumentExpr =
  case (signatureType, argumentExpr) of
    (ConstraintTypeList elementType, EList elements) ->
      not (null elements)
        && all (constraintSignatureExpressionHasExactEvidence env elementType) elements
    (ConstraintTypeTuple elementTypes, ETuple elements)
      | length elementTypes == length elements ->
          and (zipWith (constraintSignatureExpressionHasExactEvidence env) elementTypes elements)
    (ConstraintTypeApplication typeName typeArguments, EApply {}) ->
      constructorApplicationExpressionHasExactEvidence env typeName typeArguments argumentExpr
        || constraintSignatureExpressionRuntimeHintMatches env signatureType argumentExpr
    (ConstraintTypeFunction {}, _) ->
      constraintSignatureExpressionRuntimeHintMatches env signatureType argumentExpr
    (_, EVar {})
      | constraintSignatureTypeContainsList signatureType ->
          constraintSignatureExpressionRuntimeHintMatches env signatureType argumentExpr
    (_, EApply {})
      | constraintSignatureTypeContainsList signatureType ->
          constraintSignatureExpressionRuntimeHintMatches env signatureType argumentExpr
    (_, EIf {}) ->
      constraintSignatureExpressionRuntimeHintMatches env signatureType argumentExpr
    (_, EPatternCase {}) ->
      constraintSignatureExpressionRuntimeHintMatches env signatureType argumentExpr
    (_, EBlock {})
      | constraintSignatureTypeContainsList signatureType ->
          constraintSignatureExpressionRuntimeHintMatches env signatureType argumentExpr
    _ -> True

constraintSignatureExpressionRuntimeHintMatches :: TypeEnv -> ConstraintSignatureType -> Expr -> Bool
constraintSignatureExpressionRuntimeHintMatches env signatureType argumentExpr =
  case constraintSignatureExpressionRuntimeHint env argumentExpr of
    Just runtimeHint -> runtimeHint == signatureType
    Nothing -> False

constraintSignatureExpressionRuntimeHint :: TypeEnv -> Expr -> Maybe ConstraintSignatureType
constraintSignatureExpressionRuntimeHint env argumentExpr =
  constraintSignatureExpressionRuntimeHintWithLocalHints env Map.empty argumentExpr

constraintSignatureExpressionRuntimeHintWithLocalHints ::
  TypeEnv ->
  Map Text ConstraintSignatureType ->
  Expr ->
  Maybe ConstraintSignatureType
constraintSignatureExpressionRuntimeHintWithLocalHints env localHints argumentExpr =
  case argumentExpr of
    EVar referencedName ->
      Map.lookup (identifierText referencedName) localHints
        <|> (Map.lookup referencedName env >>= typeBindingRuntimeHint)
    EApply (EApply dollarExpr functionExpr) _
      | builtinDollarOperatorExpr env dollarExpr ->
          case constraintSignatureExpressionRuntimeHintWithLocalHints env localHints functionExpr of
            Just (ConstraintTypeFunction _ resultType) -> Just resultType
            _ -> Nothing
    EApply functionExpr _ ->
      case constraintSignatureExpressionRuntimeHintWithLocalHints env localHints functionExpr of
        Just (ConstraintTypeFunction _ resultType) -> Just resultType
        _ -> Nothing
    EIf _ thenExpr elseExpr ->
      commonConstraintSignatureExpressionRuntimeHint env localHints [thenExpr, elseExpr]
    EPatternCase _ caseArms ->
      commonConstraintSignatureExpressionRuntimeHint env localHints [bodyExpr | CaseArm _ _ bodyExpr <- caseArms]
    EBlock statements ->
      constraintSignatureBlockRuntimeHint env localHints statements
    _ -> Nothing

commonConstraintSignatureExpressionRuntimeHint ::
  TypeEnv ->
  Map Text ConstraintSignatureType ->
  [Expr] ->
  Maybe ConstraintSignatureType
commonConstraintSignatureExpressionRuntimeHint _ _ [] = Nothing
commonConstraintSignatureExpressionRuntimeHint env localHints (firstExpr : restExprs) = do
  firstHint <- constraintSignatureExpressionRuntimeHintWithLocalHints env localHints firstExpr
  if all
    (\expr -> constraintSignatureExpressionRuntimeHintWithLocalHints env localHints expr == Just firstHint)
    restExprs
    then Just firstHint
    else Nothing

constraintSignatureBlockRuntimeHint ::
  TypeEnv ->
  Map Text ConstraintSignatureType ->
  [Statement] ->
  Maybe ConstraintSignatureType
constraintSignatureBlockRuntimeHint env initialLocalHints statements =
  go initialLocalHints Map.empty statements
  where
    go localHints _ [] =
      Nothing
    go localHints _ [SExpr _ expr] =
      constraintSignatureExpressionRuntimeHintWithLocalHints env localHints expr
    go localHints pendingHints (statement : rest) =
      case statement of
        SSignature name _ signaturePayload ->
          let nameText = identifierText name
              nextPendingHints =
                case signaturePayloadRuntimeHint signaturePayload of
                  Just runtimeHint -> Map.insert nameText runtimeHint pendingHints
                  Nothing -> Map.delete nameText pendingHints
           in go localHints nextPendingHints rest
        SLet name _ valueExpr ->
          let nameText = identifierText name
              bindingHint =
                Map.lookup nameText pendingHints
                  <|> constraintSignatureExpressionRuntimeHintWithLocalHints env localHints valueExpr
              nextLocalHints =
                case bindingHint of
                  Just runtimeHint -> Map.insert nameText runtimeHint localHints
                  Nothing -> localHints
           in go nextLocalHints (Map.delete nameText pendingHints) rest
        _ ->
          go localHints pendingHints rest

signaturePayloadRuntimeHint :: SignaturePayload -> Maybe ConstraintSignatureType
signaturePayloadRuntimeHint signaturePayload =
  case signaturePayload of
    SignatureType signatureType ->
      expressionTypeToRuntimeHint (signatureTypeToExpressionType signatureType)
    ConstrainedSignature _ signatureType
      | Set.null (constraintSignatureTypeVariableNames signatureType) ->
          Just signatureType
    ConstrainedSignature _ signatureType ->
      constraintSignatureTypeToExpressionType signatureType >>= expressionTypeToRuntimeHint
    UnsupportedSignature {} ->
      Nothing

typeBindingRuntimeHint :: TypeBinding -> Maybe ConstraintSignatureType
typeBindingRuntimeHint binding =
  case binding of
    PlainTypeBinding bindingType ->
      expressionTypeToRuntimeHint (defaultLiteralTypes bindingType)
    SchemeTypeBinding typeScheme
      | Set.null (schemeQuantifiedVariables typeScheme) ->
          expressionTypeToRuntimeHint (defaultLiteralTypes (schemeResultType typeScheme))
    OperatorAliasSchemeTypeBinding _ typeScheme
      | Set.null (schemeQuantifiedVariables typeScheme) ->
          expressionTypeToRuntimeHint (defaultLiteralTypes (schemeResultType typeScheme))
    _ -> Nothing

constraintSignatureTypeContainsList :: ConstraintSignatureType -> Bool
constraintSignatureTypeContainsList signatureType =
  case signatureType of
    ConstraintTypeList {} -> True
    ConstraintTypeTuple elementTypes ->
      any constraintSignatureTypeContainsList elementTypes
    ConstraintTypeApplication _ typeArguments ->
      any constraintSignatureTypeContainsList typeArguments
    ConstraintTypeFunction argumentType resultType ->
      constraintSignatureTypeContainsList argumentType
        || constraintSignatureTypeContainsList resultType
    ConstraintTypeName {} -> False

constructorApplicationExpressionHasExactEvidence :: TypeEnv -> Name -> [ConstraintSignatureType] -> Expr -> Bool
constructorApplicationExpressionHasExactEvidence env typeName typeArguments argumentExpr =
  case constructorExpressionSpine argumentExpr of
    Just (constructorName, constructorArgumentExprs) ->
      case Map.lookup constructorName env of
        Just (ConstructorTypeBinding constructorTypeName typeParameters constructorArgumentTypes)
          | constructorTypeName == typeName,
            length typeParameters == length typeArguments,
            length constructorArgumentTypes == length constructorArgumentExprs ->
              let typeParameterBindings =
                    Map.fromList (zip (map identifierText typeParameters) typeArguments)
               in and
                    ( zipWith
                        (constructorArgumentExpressionHasExactEvidence env typeParameterBindings)
                        constructorArgumentTypes
                        constructorArgumentExprs
                    )
        _ -> False
    Nothing -> False

constructorExpressionSpine :: Expr -> Maybe (Name, [Expr])
constructorExpressionSpine expr =
  go [] expr
  where
    go argumentExprs currentExpr =
      case currentExpr of
        EApply functionExpr argumentExpr ->
          go (argumentExpr : argumentExprs) functionExpr
        EVar constructorName ->
          Just (constructorName, argumentExprs)
        _ ->
          Nothing

constructorArgumentExpressionHasExactEvidence :: TypeEnv -> Map Text ConstraintSignatureType -> ConstructorArgumentType -> Expr -> Bool
constructorArgumentExpressionHasExactEvidence env typeParameterBindings constructorArgument argumentExpr =
  case constructorArgument of
    ConstructorArgumentParameter parameterName ->
      case Map.lookup parameterName typeParameterBindings of
        Just concreteArgumentType ->
          constraintSignatureExpressionHasExactEvidence env concreteArgumentType argumentExpr
        Nothing ->
          True
    ConstructorArgumentMonomorphic {} ->
      True
    ConstructorArgumentFresh ->
      True

constraintSignatureTypeExactlyMatchesExpressionType :: InferState -> ConstraintSignatureType -> ExpressionType -> Bool
constraintSignatureTypeExactlyMatchesExpressionType state signatureType expressionType =
  case constraintSignatureTypeToExpressionTypeWithState state Map.empty signatureType of
    Just signatureExpressionType ->
      resolveType state signatureExpressionType == defaultLiteralTypes (resolveType state expressionType)
    Nothing ->
      False

applyQualifiedMethodCandidate ::
  Text ->
  ClassMethodType ->
  ImplMethodType ->
  InferState ->
  [ExpressionType] ->
  (Maybe ExpressionType, InferState)
applyQualifiedMethodCandidate methodKey classMethodType implMethodType state argumentTypes =
  case qualifiedMethodSignatureType methodKey classMethodType implMethodType state of
    (Nothing, nextState) ->
      (Nothing, nextState)
    (Just methodType, stateAfterMethodType) ->
      applyKnownFunctionArguments methodType argumentTypes stateAfterMethodType

applyQualifiedMethodCandidateWithErrors ::
  Text ->
  ClassMethodType ->
  ImplMethodType ->
  InferState ->
  [ExpressionType] ->
  (Maybe ExpressionType, InferState)
applyQualifiedMethodCandidateWithErrors methodKey classMethodType implMethodType state argumentTypes =
  case qualifiedMethodSignatureType methodKey classMethodType implMethodType state of
    (Nothing, nextState) ->
      (Nothing, nextState)
    (Just methodType, stateAfterMethodType) ->
      applyKnownFunctionArgumentsWithErrors methodType argumentTypes stateAfterMethodType

applyKnownFunctionArguments ::
  ExpressionType ->
  [ExpressionType] ->
  InferState ->
  (Maybe ExpressionType, InferState)
applyKnownFunctionArguments functionType argumentTypes state =
  foldl' step (Just functionType, state) argumentTypes
  where
    step (Nothing, stateAcc) _ =
      (Nothing, stateAcc)
    step (Just currentFunctionType, stateAcc) argumentType =
      let (resultTypeVar, stateWithResultVar) = freshTypeVar stateAcc
       in case unifyTypes currentFunctionType (TFunctionType argumentType resultTypeVar) stateWithResultVar of
            Just unifiedState ->
              (Just (resolveType unifiedState resultTypeVar), unifiedState)
            Nothing ->
              (Nothing, stateAcc)

applyKnownFunctionArgumentsWithErrors ::
  ExpressionType ->
  [ExpressionType] ->
  InferState ->
  (Maybe ExpressionType, InferState)
applyKnownFunctionArgumentsWithErrors functionType argumentTypes state =
  foldl' step (Just functionType, state) argumentTypes
  where
    step (Nothing, stateAcc) _ =
      (Nothing, stateAcc)
    step (Just currentFunctionType, stateAcc) argumentType =
      let (resultTypeVar, stateWithResultVar) = freshTypeVar stateAcc
       in case unifyTypes currentFunctionType (TFunctionType argumentType resultTypeVar) stateWithResultVar of
            Just unifiedState ->
              (Just (resolveType unifiedState resultTypeVar), unifiedState)
            Nothing ->
              ( Nothing,
                addTypeError
                  stateWithResultVar
                  ( mkApplyTypeError
                      (resolveType stateWithResultVar currentFunctionType)
                      (resolveType stateWithResultVar argumentType)
                  )
              )

qualifiedMethodSignatureType ::
  Text ->
  ClassMethodType ->
  ImplMethodType ->
  InferState ->
  (Maybe ExpressionType, InferState)
qualifiedMethodSignatureType methodKey (ClassMethodType classParameter methodSignature) (ImplMethodType implTarget) state =
  case classMethodPayloadToExpressionType state classParameter implTarget methodSignature of
    Just methodType -> (Just methodType, state)
    Nothing ->
      (Nothing, addTypeError state (mkInvalidQualifiedMethodSignatureError methodKey methodSignature))

classMethodPayloadToExpressionType ::
  InferState ->
  Text ->
  ConstraintSignatureType ->
  SignaturePayload ->
  Maybe ExpressionType
classMethodPayloadToExpressionType state classParameter implTarget methodSignature =
  substituteClassMethodSignature classParameter implTarget methodSignature
    >>= constraintSignatureTypeToExpressionTypeWithState state Map.empty

classMethodPayloadToGenericExpressionType ::
  InferState ->
  Text ->
  ExpressionType ->
  SignaturePayload ->
  Maybe ExpressionType
classMethodPayloadToGenericExpressionType state classParameter classTarget methodSignature =
  signaturePayloadConstraintType methodSignature
    >>= constraintSignatureTypeToExpressionTypeWithState
      state
      (Map.singleton classParameter classTarget)

constraintSignatureTypeToExpressionTypeWithState ::
  InferState ->
  Map Text ExpressionType ->
  ConstraintSignatureType ->
  Maybe ExpressionType
constraintSignatureTypeToExpressionTypeWithState state signatureVariables signatureType =
  case signatureType of
    ConstraintTypeName name ->
      case identifierText name of
        "Int" -> Just TIntType
        "Float" -> Just TFloatType
        "Bool" -> Just TBoolType
        typeName ->
          case numericTypeNameToExpressionType typeName of
            Just numericType -> Just numericType
            Nothing ->
              case Map.lookup typeName signatureVariables of
                Just variableType -> Just variableType
                Nothing
                  | Map.member typeName (inferDataTypes state) ->
                      Just (TDataType name [])
                  | otherwise ->
                      Nothing
    ConstraintTypeApplication name arguments
      | Map.member (identifierText name) (inferDataTypes state) ->
          TDataType name <$> traverse (constraintSignatureTypeToExpressionTypeWithState state signatureVariables) arguments
      | otherwise ->
          Nothing
    ConstraintTypeList innerType ->
      TListType <$> constraintSignatureTypeToExpressionTypeWithState state signatureVariables innerType
    ConstraintTypeTuple elementTypes ->
      TTupleType <$> traverse (constraintSignatureTypeToExpressionTypeWithState state signatureVariables) elementTypes
    ConstraintTypeFunction argumentType resultType ->
      TFunctionType
        <$> constraintSignatureTypeToExpressionTypeWithState state signatureVariables argumentType
        <*> constraintSignatureTypeToExpressionTypeWithState state signatureVariables resultType

freshTypeVars :: Int -> InferState -> ([ExpressionType], InferState)
freshTypeVars count initialState =
  go count [] initialState
  where
    go remaining acc state
      | remaining <= 0 = (reverse acc, state)
      | otherwise =
          let (typeVar, nextState) = freshTypeVar state
           in go (remaining - 1) (typeVar : acc) nextState

data SignaturePayloadType = SignaturePayloadType
  { signaturePayloadDeclaredType :: ExpressionType,
    signaturePayloadExplicitConstraints :: [TypeSchemeConstraint],
    signaturePayloadVariableOrder :: [Int]
  }

-- | Normalize the currently accepted signature subset. Unsupported surfaces
-- return `Nothing` so callers can emit the stable signature diagnostic.
signaturePayloadToSignatureType :: SignaturePayload -> InferState -> (Maybe SignaturePayloadType, InferState)
signaturePayloadToSignatureType signaturePayload state =
  case signaturePayload of
    SignatureType signatureType ->
      (Just (SignaturePayloadType (signatureTypeToExpressionType signatureType) [] []), state)
    ConstrainedSignature [] signatureType ->
      (fmap (\declaredType -> SignaturePayloadType declaredType [] []) (constraintSignatureTypeToExpressionType signatureType), state)
    ConstrainedSignature constraints signatureType
      | supportedConcreteConstraints state constraints ->
          (fmap (\declaredType -> SignaturePayloadType declaredType [] []) (constraintSignatureTypeToExpressionType signatureType), state)
      | supportedVariableConstraints state constraints signatureType ->
          variableConstraintSignaturePayloadToExpressionType constraints signatureType state
      | otherwise ->
          (Nothing, state)
    UnsupportedSignature {} ->
      (Nothing, state)

signatureTypeToExpressionType :: SignatureType -> ExpressionType
signatureTypeToExpressionType signatureType =
  case signatureType of
    TypeInt -> TIntType
    TypeFloat -> TFloatType
    TypeNumeric numericType -> TNumericType numericType
    TypeBool -> TBoolType
    TypeList innerType ->
      TListType (signatureTypeToExpressionType innerType)
    TypeTuple elementTypes ->
      TTupleType (map signatureTypeToExpressionType elementTypes)
    TypeFunction argumentType resultType ->
      TFunctionType
        (signatureTypeToExpressionType argumentType)
        (signatureTypeToExpressionType resultType)

constraintSignatureTypeToExpressionType :: ConstraintSignatureType -> Maybe ExpressionType
constraintSignatureTypeToExpressionType =
  constraintSignatureTypeToExpressionTypeWithVariables Map.empty

constraintSignatureTypeToExpressionTypeWithVariables ::
  Map Text ExpressionType ->
  ConstraintSignatureType ->
  Maybe ExpressionType
constraintSignatureTypeToExpressionTypeWithVariables signatureVariables signatureType =
  case signatureType of
    ConstraintTypeName name ->
      case identifierText name of
        "Int" -> Just TIntType
        "Float" -> Just TFloatType
        "Bool" -> Just TBoolType
        typeName ->
          case numericTypeNameToExpressionType typeName of
            Just numericType -> Just numericType
            Nothing -> Map.lookup typeName signatureVariables
    ConstraintTypeApplication {} ->
      Nothing
    ConstraintTypeList innerType ->
      TListType <$> constraintSignatureTypeToExpressionTypeWithVariables signatureVariables innerType
    ConstraintTypeTuple elementTypes ->
      TTupleType <$> traverse (constraintSignatureTypeToExpressionTypeWithVariables signatureVariables) elementTypes
    ConstraintTypeFunction argumentType resultType ->
      TFunctionType
        <$> constraintSignatureTypeToExpressionTypeWithVariables signatureVariables argumentType
        <*> constraintSignatureTypeToExpressionTypeWithVariables signatureVariables resultType

variableConstraintSignaturePayloadToExpressionType ::
  [SignatureConstraint] ->
  ConstraintSignatureType ->
  InferState ->
  (Maybe SignaturePayloadType, InferState)
variableConstraintSignaturePayloadToExpressionType constraints signatureType state =
  let variableNames = constraintSignatureTypeVariableNamesInOrder signatureType
      (signatureVariables, nextState) = allocateSignatureTypeVariables variableNames state
      convertedType =
        constraintSignatureTypeToExpressionTypeWithVariables signatureVariables signatureType
      convertedConstraints =
        traverse (variableConstraintToTypeSchemeConstraint signatureVariables) constraints
      variableOrder =
        [ typeVar
          | variableName <- variableNames,
            Just (TVarType typeVar) <- [Map.lookup variableName signatureVariables]
        ]
   in
    case (convertedType, convertedConstraints) of
      (Just expressionType, Just explicitConstraints) ->
        (Just (SignaturePayloadType expressionType explicitConstraints variableOrder), nextState)
      _ -> (Nothing, state)

variableConstraintToTypeSchemeConstraint ::
  Map Text ExpressionType ->
  SignatureConstraint ->
  Maybe TypeSchemeConstraint
variableConstraintToTypeSchemeConstraint signatureVariables (SignatureConstraint constraintName arguments) =
  case arguments of
    [ConstraintTypeName argumentName] ->
      TypeSchemeConstraint (identifierText constraintName)
        <$> Map.lookup (identifierText argumentName) signatureVariables
    _ -> Nothing

allocateSignatureTypeVariables :: [Text] -> InferState -> (Map Text ExpressionType, InferState)
allocateSignatureTypeVariables variableNames state =
  foldl' allocate (Map.empty, state) variableNames
  where
    allocate (signatureVariables, stateAcc) variableName =
      let (variableType, nextState) = freshTypeVar stateAcc
       in (Map.insert variableName variableType signatureVariables, nextState)

supportedConcreteConstraints :: InferState -> [SignatureConstraint] -> Bool
supportedConcreteConstraints state constraints =
  not (null constraints)
    && isNothing (duplicateConstraintName constraints)
    && all (supportedConcreteConstraint state) constraints

-- | Variable constrained signatures are accepted when every constrained
-- variable appears in the body; extra body variables remain unconstrained.
supportedVariableConstraints :: InferState -> [SignatureConstraint] -> ConstraintSignatureType -> Bool
supportedVariableConstraints state constraints signatureType =
  not (null constraints)
    && isNothing (duplicateConstraintName constraints)
    && all (supportedVariableConstraint state) constraints
    && constraintSignatureTypeSupportsVariableBody signatureType
    && not (Set.null signatureVariableNames)
    && constraintVariableNames `Set.isSubsetOf` signatureVariableNames
  where
    signatureVariableNames =
      constraintSignatureTypeVariableNames signatureType
    constraintVariableNames =
      Set.unions (map constraintVariableNamesInSupportedConstraint constraints)

supportedConcreteConstraint :: InferState -> SignatureConstraint -> Bool
supportedConcreteConstraint state (SignatureConstraint constraintName arguments) =
  case (Map.lookup (identifierText constraintName) (inferClassFacts state), arguments) of
    (Just 1, [argument]) ->
      concreteConstraintArgument argument
        && Set.member
          (constraintImplFactKey constraintName argument)
          (inferConcreteImplFacts state)
    _ -> False

supportedVariableConstraint :: InferState -> SignatureConstraint -> Bool
supportedVariableConstraint state (SignatureConstraint constraintName arguments) =
  case (Map.lookup (identifierText constraintName) (inferClassFacts state), arguments) of
    (Just 1, [ConstraintTypeName argumentName]) ->
      identifierLooksLikeTypeVariable argumentName
    _ -> False

constraintVariableNamesInSupportedConstraint :: SignatureConstraint -> Set Text
constraintVariableNamesInSupportedConstraint constraint =
  case constraint of
    SignatureConstraint _ [ConstraintTypeName argumentName]
      | identifierLooksLikeTypeVariable argumentName ->
          Set.singleton (identifierText argumentName)
    _ -> Set.empty

constraintSignatureTypeVariableNames :: ConstraintSignatureType -> Set Text
constraintSignatureTypeVariableNames signatureType =
  case signatureType of
    ConstraintTypeName name
      | identifierLooksLikeTypeVariable name ->
          Set.singleton (identifierText name)
      | otherwise ->
          Set.empty
    ConstraintTypeApplication _ arguments ->
      Set.unions (map constraintSignatureTypeVariableNames arguments)
    ConstraintTypeList innerType ->
      constraintSignatureTypeVariableNames innerType
    ConstraintTypeTuple elementTypes ->
      Set.unions (map constraintSignatureTypeVariableNames elementTypes)
    ConstraintTypeFunction argumentType resultType ->
      Set.union
        (constraintSignatureTypeVariableNames argumentType)
        (constraintSignatureTypeVariableNames resultType)

constraintSignatureTypeSupportsVariableBody :: ConstraintSignatureType -> Bool
constraintSignatureTypeSupportsVariableBody signatureType =
  case signatureType of
    ConstraintTypeName {} -> True
    ConstraintTypeApplication {} -> False
    ConstraintTypeList innerType ->
      constraintSignatureTypeSupportsVariableBody innerType
    ConstraintTypeTuple elementTypes ->
      all constraintSignatureTypeSupportsVariableBody elementTypes
    ConstraintTypeFunction argumentType resultType ->
      constraintSignatureTypeSupportsVariableBody argumentType
        && constraintSignatureTypeSupportsVariableBody resultType

numericTypeNameToExpressionType :: Text -> Maybe ExpressionType
numericTypeNameToExpressionType typeName =
  TNumericType <$> numericTypeFromName typeName

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
      instantiateTypeBinding binding state
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

defaultLiteralTypes :: ExpressionType -> ExpressionType
defaultLiteralTypes =
  defaultLiteralTypesWith TIntType

defaultBindingLiteralTypes :: ExpressionType -> ExpressionType
defaultBindingLiteralTypes =
  defaultLiteralTypesWith (TNumericType NumericInt64)

defaultLiteralTypesWith :: ExpressionType -> ExpressionType -> ExpressionType
defaultLiteralTypesWith integerLiteralDefault expressionType =
  case expressionType of
    TIntegerLiteralType {} -> integerLiteralDefault
    TListType elementType ->
      TListType (defaultLiteralTypesWith integerLiteralDefault elementType)
    TTupleType elementTypes ->
      TTupleType (map (defaultLiteralTypesWith integerLiteralDefault) elementTypes)
    TDataType typeName typeArguments ->
      TDataType typeName (map (defaultLiteralTypesWith integerLiteralDefault) typeArguments)
    TFunctionType inputType outputType ->
      TFunctionType
        (defaultLiteralTypesWith integerLiteralDefault inputType)
        (defaultLiteralTypesWith integerLiteralDefault outputType)
    _ -> expressionType

runtimeHintForBinding :: InferState -> Maybe TypeBinding -> Maybe ExpressionType -> Maybe ConstraintSignatureType
runtimeHintForBinding state maybeBinding maybeExpressionType =
  case maybeBinding >>= runtimeHintForTypeBinding state of
    Just runtimeHint -> Just runtimeHint
    Nothing -> maybeExpressionType >>= runtimeHintFromExpressionType state

runtimeHintForTypeBinding :: InferState -> TypeBinding -> Maybe ConstraintSignatureType
runtimeHintForTypeBinding state binding =
  case binding of
    PlainTypeBinding expressionType ->
      runtimeHintFromExpressionType state expressionType
    SchemeTypeBinding typeScheme ->
      typeSchemeRuntimeHint state typeScheme
    OperatorAliasSchemeTypeBinding _ typeScheme ->
      typeSchemeRuntimeHint state typeScheme
    _ -> Nothing

typeSchemeRuntimeHint :: InferState -> TypeScheme -> Maybe ConstraintSignatureType
typeSchemeRuntimeHint state typeScheme =
  case resolvedSchemeType of
    TFunctionType {} ->
      expressionTypeToRuntimeHintWithVariables
        variableHints
        resolvedSchemeType
    _ -> Nothing
  where
    schemeVariables = schemeQuantifiedVariables typeScheme
    schemeVariableOrder = schemeQuantifiedOrder typeScheme
    expressionType = schemeResultType typeScheme
    resolvedSchemeType =
      defaultLiteralTypes (resolveType state expressionType)
    orderedVariables =
      orderedSchemeVariables schemeVariableOrder schemeVariables
    variableHints =
      Map.fromList
        [ (typeVar, ConstraintTypeName (sourceName (mkIdentifier (typeSchemeRuntimeVariableName index))))
          | (index, typeVar) <- zip [0 :: Int ..] orderedVariables
        ]

typeSchemeRuntimeVariableName :: Int -> Text
typeSchemeRuntimeVariableName index
  | index >= 0 && index < length variableNames =
      Text.singleton (variableNames !! index)
  | otherwise =
      "t" <> Text.pack (show index)
  where
    variableNames = ['a' .. 'z']

runtimeHintFromExpressionType :: InferState -> ExpressionType -> Maybe ConstraintSignatureType
runtimeHintFromExpressionType state expressionType =
  expressionTypeToRuntimeHint (defaultLiteralTypes (resolveType state expressionType))

expressionTypeToRuntimeHint :: ExpressionType -> Maybe ConstraintSignatureType
expressionTypeToRuntimeHint expressionType =
  case expressionType of
    TIntType -> Just (ConstraintTypeName "Int")
    TIntegerLiteralType literalRange
      | integerLiteralRangeFitsNumericType literalRange NumericInt64 ->
          Just (ConstraintTypeName "Int")
      | otherwise -> Nothing
    TFloatType -> Just (ConstraintTypeName "Float")
    TNumericType numericType -> Just (ConstraintTypeName (sourceName (mkIdentifier (renderNumericTypeName numericType))))
    TBoolType -> Just (ConstraintTypeName "Bool")
    TListType elementType ->
      ConstraintTypeList <$> expressionTypeToRuntimeHint elementType
    TTupleType elementTypes ->
      ConstraintTypeTuple <$> traverse expressionTypeToRuntimeHint elementTypes
    TDataType typeName typeArguments ->
      case traverse expressionTypeToRuntimeHint typeArguments of
        Just [] -> Just (ConstraintTypeName typeName)
        Just argumentHints ->
          Just (ConstraintTypeApplication typeName argumentHints)
        Nothing -> Nothing
    TFunctionType inputType outputType ->
      ConstraintTypeFunction
        <$> expressionTypeToRuntimeHint inputType
        <*> expressionTypeToRuntimeHint outputType
    TVarType {} -> Nothing

expressionTypeToRuntimeHintWithVariables :: Map Int ConstraintSignatureType -> ExpressionType -> Maybe ConstraintSignatureType
expressionTypeToRuntimeHintWithVariables variableHints expressionType =
  case expressionType of
    TIntType -> Just (ConstraintTypeName "Int")
    TIntegerLiteralType literalRange
      | integerLiteralRangeFitsNumericType literalRange NumericInt64 ->
          Just (ConstraintTypeName "Int")
      | otherwise -> Nothing
    TFloatType -> Just (ConstraintTypeName "Float")
    TNumericType numericType -> Just (ConstraintTypeName (sourceName (mkIdentifier (renderNumericTypeName numericType))))
    TBoolType -> Just (ConstraintTypeName "Bool")
    TListType elementType ->
      ConstraintTypeList <$> expressionTypeToRuntimeHintWithVariables variableHints elementType
    TTupleType elementTypes ->
      ConstraintTypeTuple <$> traverse (expressionTypeToRuntimeHintWithVariables variableHints) elementTypes
    TDataType typeName typeArguments ->
      case traverse (expressionTypeToRuntimeHintWithVariables variableHints) typeArguments of
        Just [] -> Just (ConstraintTypeName typeName)
        Just argumentHints ->
          Just (ConstraintTypeApplication typeName argumentHints)
        Nothing -> Nothing
    TFunctionType inputType outputType ->
      ConstraintTypeFunction
        <$> expressionTypeToRuntimeHintWithVariables variableHints inputType
        <*> expressionTypeToRuntimeHintWithVariables variableHints outputType
    TVarType typeVar ->
      Map.lookup typeVar variableHints

addInferredClassConstraint :: Text -> ExpressionType -> InferState -> InferState
addInferredClassConstraint constraintName argumentType state =
  modifyInferenceOutput
    ( \output ->
        output
          { outputInferredConstraints =
              TypeSchemeInferredConstraint constraintName argumentType : inferInferredClassConstraints state
          }
    )
    state

addInferredMethodClassConstraint :: Text -> Text -> ExpressionType -> InferState -> InferState
addInferredMethodClassConstraint constraintName methodKey argumentType state =
  modifyInferenceOutput
    ( \output ->
        output
          { outputInferredConstraints =
              TypeSchemeMethodConstraint constraintName methodKey argumentType : inferInferredClassConstraints state
          }
    )
    state

addInferredEqualityClassConstraintIfVisible :: ExpressionType -> InferState -> InferState
addInferredEqualityClassConstraintIfVisible argumentType state =
  case activeEqualityClassName state of
    Just equalityClassName -> addInferredClassConstraint equalityClassName argumentType state
    Nothing -> state

activeEqualityClassName :: InferState -> Maybe Text
activeEqualityClassName state =
  case inferCurrentModulePath state of
    Just modulePath
      | let replayQualifiedEqName = moduleReplayQualifiedName modulePath "Eq",
        classFactIsUnary replayQualifiedEqName ->
          Just replayQualifiedEqName
    _ ->
      if classFactIsUnary "Eq"
        then Just "Eq"
        else Nothing
  where
    classFactIsUnary className =
      Map.lookup className (inferClassFacts state) == Just 1

moduleReplayQualifiedName :: [Text] -> Text -> Text
moduleReplayQualifiedName modulePath name =
  qualifiedIdentifierText "__module" (Text.intercalate "::" modulePath <> "::" <> name)

mkInvalidSignatureTypeError :: InferState -> Text -> SourceSpan -> SignaturePayload -> Diagnostic
mkInvalidSignatureTypeError state symbol signatureSpan signaturePayload =
  setDiagnosticSubject symbol $
    setDiagnosticPrimarySpan
      signatureSpan
      ( mkDiagnostic
          "E2009"
          (invalidSignatureSummary state symbol signaturePayload)
      )

invalidSignatureSummary :: InferState -> Text -> SignaturePayload -> Text
invalidSignatureSummary state symbol signaturePayload =
  case signaturePayload of
    ConstrainedSignature constraints _
      | Just duplicateName <- duplicateConstraintName constraints ->
          "invalid or unsupported signature for '"
            <> symbol
            <> "': duplicate constraint '"
            <> duplicateName
            <> "' in '"
            <> renderSignaturePayload signaturePayload
            <> "'"
    ConstrainedSignature constraints signatureType
      | constrainedSignatureHasTypeVariable constraints signatureType ->
          "invalid or unsupported signature for '"
            <> symbol
            <> "': type-variable constrained signatures require every constrained variable to appear in the signature body before inference can accept '"
            <> renderSignaturePayload signaturePayload
            <> "'"
    ConstrainedSignature constraints _
      | Just reason <- concreteConstraintFailureSummary state constraints ->
          "invalid or unsupported signature for '"
            <> symbol
            <> "': "
            <> reason
            <> " in '"
            <> renderSignaturePayload signaturePayload
            <> "'"
    _ ->
      "invalid or unsupported signature for '"
        <> symbol
        <> "': '"
        <> renderSignaturePayload signaturePayload
        <> "'"

concreteConstraintFailureSummary :: InferState -> [SignatureConstraint] -> Maybe Text
concreteConstraintFailureSummary state constraints
  | null constraints = Nothing
  | otherwise = firstJust (map constraintFailureSummary constraints)
  where
    constraintFailureSummary (SignatureConstraint constraintName arguments)
      | Nothing <- maybeClassArity =
          Just ("missing class declaration '" <> constraintNameText <> "'")
      | Just expectedArity <- maybeClassArity,
        expectedArity /= length arguments =
          Just
            ( "constraint '"
                <> constraintNameText
                <> "' expects "
                <> Text.pack (show expectedArity)
                <> " argument(s), got "
                <> Text.pack (show (length arguments))
            )
      | [argument] <- arguments,
        concreteConstraintArgument argument,
        let implFactKey = constraintImplFactKey constraintName argument,
        Set.notMember implFactKey (inferConcreteImplFacts state) =
          Just ("missing impl fact '" <> implFactKey <> "'")
      | otherwise =
          Nothing
      where
        constraintNameText = identifierText constraintName
        maybeClassArity = Map.lookup constraintNameText (inferClassFacts state)

    firstJust results =
      case results of
        [] -> Nothing
        Just result : _ -> Just result
        Nothing : rest -> firstJust rest

constrainedSignatureHasTypeVariable :: [SignatureConstraint] -> ConstraintSignatureType -> Bool
constrainedSignatureHasTypeVariable constraints signatureType =
  any constraintHasTypeVariable constraints
    || constraintTypeHasTypeVariable signatureType

constraintHasTypeVariable :: SignatureConstraint -> Bool
constraintHasTypeVariable (SignatureConstraint _ arguments) =
  any constraintTypeHasTypeVariable arguments

constraintTypeHasTypeVariable :: ConstraintSignatureType -> Bool
constraintTypeHasTypeVariable signatureType =
  case signatureType of
    ConstraintTypeName name ->
      identifierLooksLikeTypeVariable name
    ConstraintTypeApplication name arguments ->
      identifierLooksLikeTypeVariable name || any constraintTypeHasTypeVariable arguments
    ConstraintTypeList innerType ->
      constraintTypeHasTypeVariable innerType
    ConstraintTypeTuple elementTypes ->
      any constraintTypeHasTypeVariable elementTypes
    ConstraintTypeFunction argumentType resultType ->
      constraintTypeHasTypeVariable argumentType || constraintTypeHasTypeVariable resultType

duplicateConstraintName :: [SignatureConstraint] -> Maybe Text
duplicateConstraintName constraints =
  go Set.empty constraints
  where
    go seen remainingConstraints =
      case remainingConstraints of
        [] -> Nothing
        SignatureConstraint constraintName _ : rest ->
          let constraintNameText = identifierText constraintName
           in if Set.member constraintNameText seen
                then Just constraintNameText
                else go (Set.insert constraintNameText seen) rest
