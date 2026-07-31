{-# LANGUAGE OverloadedStrings #-}

-- | Scope, binding, signature, and constructor inference.  Typed-core
-- production selects and finalizes only the root scope after this traversal,
-- leaving ordinary scope inference and its runtime-hint ownership unchanged.
module JazzNext.Compiler.TypeInference.Scope
  ( inferExplicitTypeApplication,
    inferExplicitTypeApplicationWithResult,
    inferNestedScopeTypeWithMode,
    inferScopeType,
    inferScopeTypeWithMode,
    inferScopeTypeWithModeAndForwardBindings,
    instantiateNonBuiltinTypeBinding,
  )
where

import Data.List (uncons, unsnoc)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isNothing)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import JazzNext.Compiler.AST
  ( CaseArm (..),
    ClassMethodSignature (..),
    DataConstructor (..),
    Expr (..),
    Literal (..),
    NumericType (..),
    SignaturePayload (ConstrainedSignature),
    SignatureType (..),
    Statement (..),
  )
import JazzNext.Compiler.BuiltinCatalog
  ( BuiltinResolutionMode (..),
    builtinNamesInMode,
    lookupBuiltinSymbolInMode,
    numericTypeFloatMax,
  )
import JazzNext.Compiler.CapabilityFacts
  ( constraintSignatureTypeVariableNamesInOrder,
    signaturePayloadConstraintType,
  )
import JazzNext.Compiler.Diagnostics
  ( Diagnostic,
    SourceSpan,
    setDiagnosticPrimarySpan,
  )
import JazzNext.Compiler.FractionalLiteral
  ( FractionalLiteralSource,
    fractionalLiteralExceedsMagnitude,
  )
import JazzNext.Compiler.Name
  ( Name (..),
    identifierText,
    mkIdentifier,
    operatorBindingName,
    sourceName,
  )
import JazzNext.Compiler.Parser.Operator (isBuiltinOperatorSymbol)
import JazzNext.Compiler.RecursiveBindings
  ( collectBindingNames,
    freeVarsExprWithBound,
    inferRecursiveGroupsOrdered,
    inferSelfRecursiveBindings,
  )
import JazzNext.Compiler.RuntimeHints
  ( bindingRuntimeHintKeyInModule,
    explicitTypeApplicationRuntimeHintKeyInModule,
  )
import JazzNext.Compiler.TypeInference.Capabilities
import JazzNext.Compiler.TypeInference.Diagnostics
import JazzNext.Compiler.TypeInference.Elaboration
  ( InferredExpr (..),
    InferredProductionFailure (..),
    ProvisionalTypedExpr (..),
    ProvisionalTypedStatement (..),
    TypedCoreProductionFailureKind (..),
    TypedCoreProductionMode (..),
    blockProductionFailureKindAndDetail,
  )
import JazzNext.Compiler.TypeInference.Pattern (instantiateConstructorBinding)
import qualified JazzNext.Compiler.TypeInference.Signature as Signature
import JazzNext.Compiler.TypeInference.Solver
  ( freshTypeVar,
    resolveType,
    unifyTypes,
  )
import JazzNext.Compiler.TypeInference.State
  ( DeclarationState (..),
    InferState (..),
    InferenceOutput (..),
    ModuleInferenceState (..),
    SolverState (..),
    inferDataTypes,
    inferDeferredExplicitConstraints,
    inferErrorCount,
    inferErrorsRev,
    inferInferredClassConstraints,
    inferNumericVars,
    inferRigidTypeVars,
    inferRuntimeHintPath,
    inferRuntimeTypeHints,
    inferStrictEqualityVars,
    modifyDeclarationState,
    modifyInferenceOutput,
    modifyModuleInferenceState,
  )
import JazzNext.Compiler.TypeInference.TypeOps
  ( dedupeTypeSchemeConstraints,
    freeTypeVariables,
    freeTypeVariablesInTypeSchemeConstraints,
    instantiateTypeSchemeConstraint,
    instantiateTypeSchemePrimitiveConstraint,
    replaceTypeVariables,
  )
import JazzNext.Compiler.TypeInference.Types
  ( ConstructorArgumentType (..),
    DataTypeBinding (..),
    ExpressionType (..),
    TypeBinding (..),
    TypeEnv,
    TypeScheme (..),
    TypeSchemeConstraint (..),
    TypeSchemePrimitiveConstraint (..),
  )

inferExprTypeWithExpected ::
  InferExprFn ->
  BuiltinResolutionMode ->
  TypeEnv ->
  InferState ->
  ExpressionType ->
  Expr ->
  (Maybe ExpressionType, InferState)
inferExprTypeWithExpected inferExpression builtinMode env state expectedType expr =
  case (resolveType state expectedType, expr) of
    (_, EVar name)
      | Map.notMember name env,
        Just qualifiedMethodResult <-
          instantiateQualifiedMethodTypeWithExpected
            (identifierText name)
            expectedType
            state ->
          qualifiedMethodResult
    (TFunctionType argumentType resultType, ELambda parameterName bodyExpr) ->
      let extendedEnv = Map.insert parameterName (PlainTypeBinding argumentType) env
          (bodyType, stateAfterBody) =
            inferExprTypeWithExpected inferExpression builtinMode extendedEnv state resultType bodyExpr
       in case bodyType of
            Just inferredBodyType ->
              ( Just
                  ( TFunctionType
                      (resolveType stateAfterBody argumentType)
                      inferredBodyType
                  ),
                stateAfterBody
              )
            Nothing -> (Nothing, stateAfterBody)
    (TNumericType numericType, ELit (LFloat literalValue literalSource Nothing))
      | Just _ <- numericTypeFloatMax numericType ->
          ( Just (TNumericType numericType),
            maybe state (addTypeError state) (targetedFloatLiteralDiagnostic numericType literalValue literalSource)
          )
    _ -> inferExpression builtinMode env state expr

inferExprTypeWithExpectedMode ::
  InferExprWithModeFn ->
  TypedCoreProductionMode ->
  BuiltinResolutionMode ->
  TypeEnv ->
  InferState ->
  ExpressionType ->
  Expr ->
  (InferredExpr, InferState)
inferExprTypeWithExpectedMode inferExpression mode builtinMode env state expectedType expr =
  if mode == InferenceOnly
    then
      let (expressionType, nextState) =
            inferExprTypeWithExpected
              ( \builtin childEnv childState childExpr ->
                  let (result, resultState) = inferExpression InferenceOnly builtin childEnv childState childExpr
                   in (inferredExpressionType result, resultState)
              )
              builtinMode
              env
              state
              expectedType
              expr
       in (InferredExpr expressionType Nothing [], nextState)
    else case (resolveType state expectedType, expr) of
    (_, EVar name)
      | Map.notMember name env,
        Just (expressionType, nextState) <-
          instantiateQualifiedMethodTypeWithExpected
            (identifierText name)
            expectedType
            state ->
          ( InferredExpr
              expressionType
              (if mode == ProduceTypedCoreExpressionDirectCall then ProvisionalVariableExpression name <$> expressionType else Nothing)
              [],
            nextState
          )
    (TFunctionType argumentType resultType, ELambda parameterName bodyExpr) ->
      let extendedEnv = Map.insert parameterName (PlainTypeBinding argumentType) env
          (bodyResult, stateAfterBody) =
            inferExprTypeWithExpectedMode inferExpression mode builtinMode extendedEnv state resultType bodyExpr
          functionType =
            TFunctionType
              (resolveType stateAfterBody argumentType)
              (maybe resultType id (inferredExpressionType bodyResult))
          provisional =
            ProvisionalLambdaExpression parameterName functionType
              <$> inferredProvisionalExpr bodyResult
          failures =
            [ InferredProductionFailure (0 : childPath) kind detail
            | InferredProductionFailure childPath kind detail <- inferredProductionFailures bodyResult
            ]
       in (InferredExpr (Just functionType) provisional failures, stateAfterBody)
    (TNumericType _, ELit literal@(LInt _)) ->
      let (literalResult, nextState) = inferExpression mode builtinMode env state (ELit literal)
       in case inferredExpressionType literalResult of
            Just literalType
              | Just checkedState <- unifyTypes expectedType literalType nextState ->
                  let concreteType = resolveType checkedState expectedType
                   in ( InferredExpr
                          (Just concreteType)
                          (if mode == ProduceTypedCoreExpressionDirectCall then Just (ProvisionalLiteralExpression literal concreteType) else Nothing)
                          [],
                        checkedState
                      )
            _ -> (literalResult, nextState)
    (TNumericType numericType, ELit literal@(LFloat literalValue literalSource Nothing))
      | Just _ <- numericTypeFloatMax numericType ->
          let nextState =
                maybe state (addTypeError state) (targetedFloatLiteralDiagnostic numericType literalValue literalSource)
              concreteType = TNumericType numericType
           in ( InferredExpr
                  (Just concreteType)
                  (if mode == ProduceTypedCoreExpressionDirectCall then Just (ProvisionalLiteralExpression literal concreteType) else Nothing)
                  [],
                nextState
              )
    _ -> inferExpression mode builtinMode env state expr

setStatementRuntimeHintPath :: Set Int -> Int -> InferState -> InferState
setStatementRuntimeHintPath preludeStatementIndices statementIndex state =
  modifyModuleInferenceState
    ( \moduleState ->
        moduleState
          { inferenceRuntimeHintPath =
              if Set.member statementIndex preludeStatementIndices
                then Just []
                else
                  if Set.null preludeStatementIndices
                    then inferenceRuntimeHintPath moduleState
                    else inferenceModulePath moduleState
          }
    )
    state

firstInvalidImplTarget :: InferState -> SourceSpan -> [SignatureType] -> Maybe Diagnostic
firstInvalidImplTarget state implSpan =
  go
  where
    go signatureTypes =
      case signatureTypes of
        [] -> Nothing
        signatureType : rest ->
          case mkInvalidImplTargetError state implSpan signatureType of
            Just diagnostic -> Just diagnostic
            Nothing -> go rest

firstInvalidClassMethodSignature :: InferState -> Name -> [Name] -> [ClassMethodSignature] -> Maybe Diagnostic
firstInvalidClassMethodSignature state capabilityName parameters =
  go
  where
    classParameterNames = Set.fromList (map identifierText parameters)

    go methods =
      case methods of
        [] -> Nothing
        ClassMethodSignature methodName methodSpan methodPayload : rest ->
          let methodKey = identifierText capabilityName <> "::" <> identifierText methodName
              methodVariables =
                maybe [] constraintSignatureTypeVariableNamesInOrder (signaturePayloadConstraintType methodPayload)
              methodLocalVariables = filter (`Set.notMember` classParameterNames) methodVariables
              invalidMethodSignature =
                mkInvalidSignatureTypeError
                  state
                  methodKey
                  methodSpan
                  methodPayload
           in case methodPayload of
                ConstrainedSignature (_ : _) _ ->
                  Just
                    ( setDiagnosticPrimarySpan
                        methodSpan
                        (mkInvalidQualifiedMethodSignatureError methodKey methodPayload)
                    )
                _ ->
                  case methodLocalVariables of
                    variableName : _ ->
                      Just (mkMethodLocalTypeVariableError methodKey variableName methodSpan)
                    [] ->
                      case Signature.signaturePayloadToSignatureType methodPayload state of
                        (Just _, _) -> go rest
                        (Nothing, _) -> Just invalidMethodSignature

publishVisibleTypes :: TypeEnv -> InferState -> InferState
publishVisibleTypes env state =
  state
    { inferModule =
        (inferModule state) {inferenceVisibleTypes = env}
    }

inferScopeTypeWithMode :: Set Int -> InferExprWithModeFn -> TypedCoreProductionMode -> BuiltinResolutionMode -> TypeEnv -> InferState -> [Statement] -> (InferredExpr, InferState)
inferScopeTypeWithMode preludeStatementIndices inferExpression mode builtinMode initialEnv initialState statements =
  let (inferredResult, finalState, _) =
        inferScopeTypeWithModeAndForwardBindings
          preludeStatementIndices
          inferExpression
          mode
          builtinMode
          initialEnv
          initialState
          statements
   in (inferredResult, finalState)

inferScopeTypeWithModeAndForwardBindings ::
  Set Int ->
  InferExprWithModeFn ->
  TypedCoreProductionMode ->
  BuiltinResolutionMode ->
  TypeEnv ->
  InferState ->
  [Statement] ->
  (InferredExpr, InferState, Map Int (Name, SourceSpan))
inferScopeTypeWithModeAndForwardBindings preludeStatementIndices inferExpression mode builtinMode initialEnv initialState statements =
  inferScopeTypeInternal True preludeStatementIndices inferExpression mode builtinMode initialEnv initialState statements

inferNestedScopeTypeWithMode :: Set Int -> InferExprWithModeFn -> TypedCoreProductionMode -> BuiltinResolutionMode -> TypeEnv -> InferState -> [Statement] -> (InferredExpr, InferState)
inferNestedScopeTypeWithMode preludeStatementIndices inferExpression mode builtinMode initialEnv initialState statements =
  let (inferredResult, finalState, _) =
        inferScopeTypeInternal False preludeStatementIndices inferExpression mode builtinMode initialEnv initialState statements
   in (inferredResult, finalState)

inferScopeType :: Set Int -> InferExprFn -> BuiltinResolutionMode -> TypeEnv -> InferState -> [Statement] -> (Maybe ExpressionType, InferState)
inferScopeType preludeStatementIndices inferExpression builtinMode initialEnv initialState statements =
  let (inferredResult, finalState, _) =
        inferScopeTypeInternal
          False
          preludeStatementIndices
          ( \_mode builtin env state expr ->
              let (expressionType, nextState) = inferExpression builtin env state expr
               in (InferredExpr expressionType Nothing [], nextState)
          )
          InferenceOnly
          builtinMode
          initialEnv
          initialState
          statements
   in (inferredExpressionType inferredResult, finalState)

inferScopeTypeInternal :: Bool -> Set Int -> InferExprWithModeFn -> TypedCoreProductionMode -> BuiltinResolutionMode -> TypeEnv -> InferState -> [Statement] -> (InferredExpr, InferState, Map Int (Name, SourceSpan))
inferScopeTypeInternal allowForwardSignedFunctions preludeStatementIndices inferExpression mode builtinMode initialEnv initialState statements =
  let (scopeType, finalState, provisionalStatements, productionFailures) =
        go initialEnv Nothing Nothing Map.empty Map.empty initialModuleBaselineFacts stateAfterBindingSeeds indexedStatements
      stateWithPublishedModuleFacts = flushCurrentModuleCapabilityFacts finalState
      provisionalExpr =
        case mode of
          ProduceTypedCoreExpressionDirectCall -> Just (ProvisionalScopeStatements provisionalStatements)
          InferenceOnly -> Nothing
   in ( InferredExpr scopeType provisionalExpr productionFailures,
        restoreCapabilityFacts initialState stateWithPublishedModuleFacts,
        forwardAnalysisBindings
      )
  where
    inferPlain builtin env state expr =
      let (result, nextState) = inferExpression mode builtin env state expr
       in (inferredExpressionType result, nextState)

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
    predeclaredDataTypes =
      predeclareScopeDataTypes indexedStatements initialState
    scopePreparation =
      prepareScope allowForwardSignedFunctions mode predeclaredDataTypes indexedStatements initialState
    bindingSeedsByStatement = preparedBindingSeeds scopePreparation
    preparedSignaturesByStatement = preparedSignatures scopePreparation
    forwardFunctionBindings = preparedForwardFunctions scopePreparation
    forwardAnalysisBindings =
      Map.fromList
        [ (statementIndex, (bindingName, bindingSpan))
        | (statementIndex, SLet bindingName bindingSpan _) <- indexedStatements,
          Map.member statementIndex forwardFunctionBindings
        ]
    stateAfterBindingSeeds = preparedScopeState scopePreparation
    initialModuleBaselineFacts = capabilityFactsFromState initialState

    go env lastExprType pendingSignatureType pendingSignaturesByStatement recursiveGroupStartStates moduleBaselineFacts state remainingStatements =
      case remainingStatements of
        [] -> (lastExprType, publishVisibleTypes env state, [], [])
        (statementIndex, statement) : rest ->
          let stateForSource = setStatementRuntimeHintPath preludeStatementIndices statementIndex state
           in case statement of
            SModule _ modulePath ->
              go env lastExprType pendingSignatureType pendingSignaturesByStatement recursiveGroupStartStates moduleBaselineFacts (enterModuleCapabilityScope moduleBaselineFacts modulePath state) rest
            SImport _ modulePath maybeAlias maybeSymbolNames ->
              go env lastExprType pendingSignatureType pendingSignaturesByStatement recursiveGroupStartStates moduleBaselineFacts (importModuleCapabilityFacts modulePath maybeAlias maybeSymbolNames state) rest
            SClass classSpan capabilityName parameters methods ->
              let validationState =
                    seedStatementCapabilityFact
                      stateForSource
                      (SClass classSpan capabilityName parameters [])
                  maybeInvalidMethod =
                    firstInvalidClassMethodSignature validationState capabilityName parameters methods
                  nextState =
                    case maybeInvalidMethod of
                      Just diagnostic -> addTypeError stateForSource diagnostic
                      Nothing -> seedStatementCapabilityFact stateForSource statement
                  nextModuleBaselineFacts =
                    updateRootModuleBaselineFacts moduleBaselineFacts state nextState
                  (scopeResultType, resultState, provisionalRest, productionFailures) =
                    go env lastExprType Nothing pendingSignaturesByStatement recursiveGroupStartStates nextModuleBaselineFacts nextState rest
                  provisional =
                    case mode of
                      ProduceTypedCoreExpressionDirectCall -> ProvisionalUnsupportedStatement statementIndex : provisionalRest
                      InferenceOnly -> provisionalRest
               in (scopeResultType, resultState, provisional, productionFailures)
            SImpl implSpan capabilityName arguments methods ->
              let maybeInvalidTarget = firstInvalidImplTarget stateForSource implSpan arguments
                  nextState =
                    case maybeInvalidTarget of
                      Just diagnostic -> addTypeError stateForSource diagnostic
                      Nothing ->
                        let implSeededState = seedStatementCapabilityFact stateForSource statement
                         in checkImplMethodBodies inferPlain builtinMode env implSeededState capabilityName arguments methods
                  nextModuleBaselineFacts =
                    updateRootModuleBaselineFacts moduleBaselineFacts state nextState
                  (scopeResultType, resultState, provisionalRest, productionFailures) =
                    go env lastExprType Nothing pendingSignaturesByStatement recursiveGroupStartStates nextModuleBaselineFacts nextState rest
                  provisional =
                    case mode of
                      ProduceTypedCoreExpressionDirectCall -> ProvisionalUnsupportedStatement statementIndex : provisionalRest
                      InferenceOnly -> provisionalRest
               in (scopeResultType, resultState, provisional, productionFailures)
            SData spanValue typeName typeParameters constructors ->
              let (nextEnv, nextState) =
                    registerDataConstructors predeclaredDataTypes spanValue typeName typeParameters constructors env state
               in go nextEnv lastExprType Nothing pendingSignaturesByStatement recursiveGroupStartStates moduleBaselineFacts nextState rest
            SSignature name signatureSpan signaturePayload ->
              let (nextPendingSignature, nextState) =
                    case Map.lookup statementIndex preparedSignaturesByStatement of
                      Just (PreparedSignature (Just pendingSignature) _) ->
                        (Just pendingSignature, state)
                      _ ->
                        ( Nothing,
                          addTypeError
                            state
                            (mkInvalidSignatureTypeError signatureState (identifierText name) signatureSpan signaturePayload)
                        )
                  signatureState = state
                  (scopeResultType, resultState, provisionalRest, productionFailures) =
                    go env lastExprType nextPendingSignature pendingSignaturesByStatement recursiveGroupStartStates moduleBaselineFacts nextState rest
                  provisional =
                    case (mode, nextPendingSignature) of
                      (ProduceTypedCoreExpressionDirectCall, Just pendingSignature)
                        | TFunctionType {} <- pendingSignatureDeclaredType pendingSignature ->
                            [ProvisionalSignature statementIndex name signatureSpan (pendingSignatureDeclaredType pendingSignature)]
                      _ -> []
               in (scopeResultType, resultState, provisional <> provisionalRest, productionFailures)
            SLet name bindingSpan valueExpr ->
              let nameText = identifierText name
                  (envForStatement, stateForStatement) =
                    exposeVisibleRecursiveGroupSchemes statementIndex env stateForSource
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
                        case ( shouldSeedSelfRecursiveFunction statementIndex name envForStatement,
                          Map.lookup statementIndex bindingSeedsByStatement
                        ) of
                      (True, Just bindingSeed) ->
                        Map.insert name (PlainTypeBinding bindingSeed) envWithRecursiveBindings
                      _ -> envWithRecursiveBindings
                  envWithForwardSignedBindings =
                    case Map.lookup statementIndex forwardFunctionBindings of
                      Nothing -> envWithBindingSeed
                      Just _ ->
                        foldl'
                          ( \currentEnv (forwardStatementIndex, forwardBinding) ->
                              if forwardStatementIndex > statementIndex
                                then
                                  Map.insertWith
                                    (\_ existing -> existing)
                                    (forwardFunctionName forwardBinding)
                                    (PlainTypeBinding (forwardFunctionType forwardBinding))
                                    currentEnv
                                else currentEnv
                          )
                          envWithBindingSeed
                          (Map.toAscList forwardFunctionBindings)
                  envWithPendingSignature =
                    case matchingPendingSignature of
                      Just pendingSignature ->
                        Map.insert
                          name
                          (PlainTypeBinding (pendingSignatureDeclaredType pendingSignature))
                          envWithForwardSignedBindings
                      Nothing -> envWithForwardSignedBindings
                  maybeExpectedValueType =
                    pendingSignatureDeclaredType <$> matchingPendingSignature
                  stateForSignatureCheck =
                    case matchingPendingSignature of
                      Just pendingSignature ->
                        setRigidTypeVariables
                          ( Set.union
                              (inferRigidTypeVars stateForStatement)
                              (Set.fromList (pendingSignatureVariableOrder pendingSignature))
                          )
                          stateForStatement
                      Nothing -> stateForStatement
                  (rawValueResult, rawStateAfterValue) =
                    case maybeExpectedValueType of
                      Just expectedValueType ->
                        inferExprTypeWithExpectedMode inferExpression mode builtinMode envWithPendingSignature stateForSignatureCheck expectedValueType valueExpr
                      Nothing ->
                        inferExpression mode builtinMode envWithPendingSignature stateForStatement valueExpr
                  valueProductionFailures =
                    nestedBlockProductionFailures valueExpr rawValueResult
                  rawValueType = inferredExpressionType rawValueResult
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
                            case unifyTypes
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
                    restoreRigidTypeVariables stateForStatement $
                      finalizeDeferredExplicitConstraintsAtWithEntailments
                        bindingSpan
                        (maybe [] pendingSignatureExplicitConstraints matchingPendingSignature)
                        stateForStatement
                        stateAfterSignatureCheck
                  stateAfterSignatureContractCheck =
                    case matchingPendingSignature of
                      Just pendingSignature ->
                        addUndeclaredSignatureConstraintErrors
                          nameText
                          stateForStatement
                          pendingSignature
                          stateAfterExplicitConstraintCheck
                      Nothing -> stateAfterExplicitConstraintCheck
                  nextBindingType =
                    case matchingPendingSignature of
                      Just pendingSignature ->
                        Just (resolveType stateAfterSignatureContractCheck (pendingSignatureDeclaredType pendingSignature))
                      _ ->
                        fmap
                          (defaultBindingLiteralTypes . resolveType stateAfterSignatureContractCheck)
                          (Map.lookup statementIndex bindingSeedsByStatement)
                  generalizationEnv =
                    generalizationEnvForStatement statementIndex envForStatement
                  droppedInferredSchemeVariables =
                    case (matchingPendingSignature, nextBindingType) of
                      (Just pendingSignature, Just _)
                        | shouldGeneralizeExplicitSignatureBinding pendingSignature ->
                            explicitBindingSchemeVariables generalizationEnv stateAfterSignatureContractCheck pendingSignature
                      (_, Just inferredType)
                        | shouldGeneralizeOrdinaryBinding statementIndex generalizationEnv valueExpr matchingPendingSignature ->
                            ordinaryBindingSchemeVariables generalizationEnv stateAfterSignatureContractCheck inferredType
                      _ -> Set.empty
                  stateAfterDroppedInferredMethodCheck =
                    case nextBindingType of
                      Just bindingType ->
                        addUnpreservedInferredMethodConstraintErrors
                          bindingSpan
                          generalizationEnv
                          stateForStatement
                          stateAfterSignatureContractCheck
                          bindingType
                          droppedInferredSchemeVariables
                      Nothing -> stateAfterSignatureContractCheck
                  maybeNextBinding =
                    nextBindingForValue
                      statementIndex
                      envForStatement
                      valueExpr
                      nextBindingType
                      matchingPendingSignature
                      stateAfterDroppedInferredMethodCheck
                  stateAfterRuntimeHint =
                        case runtimeHintForBinding
                          stateAfterDroppedInferredMethodCheck
                          maybeNextBinding
                          nextBindingType of
                        Just runtimeHint ->
                          modifyInferenceOutput
                            ( \output ->
                                output
                                  { outputRuntimeHints =
                                      Map.insert
                                        (bindingRuntimeHintKeyInModule (inferRuntimeHintPath stateAfterDroppedInferredMethodCheck) name bindingSpan)
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
                  (scopeResultType, resultState, provisionalRest, restProductionFailures) =
                    go nextEnv lastExprType Nothing nextPendingSignaturesByStatement recursiveGroupStartStatesForStatement moduleBaselineFacts stateAfterRecursiveGroupPrune rest
                  provisional =
                    case (mode, valueProductionFailures, nextBindingType, inferredProvisionalExpr rawValueResult) of
                      (ProduceTypedCoreExpressionDirectCall, _, Just bindingType, Just expression)
                        | ProvisionalLambdaExpression {} <- expression ->
                            [ ProvisionalFunctionBinding
                                statementIndex
                                name
                                bindingSpan
                                bindingType
                                maybeNextBinding
                                expression
                            ]
                      (ProduceTypedCoreExpressionDirectCall, failures@(_ : _), _, _) ->
                        [ProvisionalFunctionFailures statementIndex failures]
                      (ProduceTypedCoreExpressionDirectCall, [], _, _) ->
                        [ProvisionalUnsupportedStatement statementIndex]
                      _ -> []
                  productionFailures =
                    qualifyStatementProductionFailures statementIndex valueProductionFailures
                      <> restProductionFailures
               in (scopeResultType, resultState, provisional <> provisionalRest, productionFailures)
            SExpr exprSpan expr ->
              let (envForStatement, stateForStatement) =
                    exposeVisibleRecursiveGroupSchemes statementIndex env stateForSource
                  (exprResult, rawStateAfterExpr) = inferExpression mode builtinMode envForStatement stateForStatement expr
                  expressionProductionFailures =
                    nestedBlockProductionFailures expr exprResult
                  exprType = inferredExpressionType exprResult
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
                  (scopeResultType, resultState, provisionalRest, restProductionFailures) =
                    go env exprType Nothing pendingSignaturesByStatement recursiveGroupStartStates moduleBaselineFacts stateAfterDroppedInferredMethodCheck rest
                  provisional =
                    case (mode, expressionProductionFailures, inferredProvisionalExpr exprResult) of
                      (ProduceTypedCoreExpressionDirectCall, failures@(_ : _), Just ProvisionalScopeStatements {}) ->
                        [ ProvisionalTerminalExpression
                            statementIndex
                            exprSpan
                            (ProvisionalRetainedFailures failures)
                        ]
                      (ProduceTypedCoreExpressionDirectCall, _, Just expression) ->
                        [ProvisionalTerminalExpression statementIndex exprSpan expression]
                      (ProduceTypedCoreExpressionDirectCall, _, Nothing) ->
                        [ProvisionalUnsupportedStatement statementIndex]
                      _ -> []
                  productionFailures =
                    qualifyStatementProductionFailures statementIndex expressionProductionFailures
                      <> restProductionFailures
               in (scopeResultType, resultState, provisional <> provisionalRest, productionFailures)

    nestedBlockProductionFailures expression result =
      if mode /= ProduceTypedCoreExpressionDirectCall
        then inferredProductionFailures result
        else case expression of
            EBlock blockStatements ->
              case blockProductionFailureKindAndDetail blockStatements of
                (TypedCoreStructuredValueUnsupported, _) ->
                  inferredProductionFailures result
                (failureKind, failureDetail) ->
                  InferredProductionFailure
                    []
                    failureKind
                    failureDetail
                    : inferredProductionFailures result
            _ -> inferredProductionFailures result

    qualifyStatementProductionFailures statementIndex failures =
      [ InferredProductionFailure (statementIndex : childPath) kind detail
      | InferredProductionFailure childPath kind detail <- failures
      ]

    builtinOperatorSymbolExpr :: TypeEnv -> Expr -> Maybe (Text, Maybe TypeScheme)
    builtinOperatorSymbolExpr currentEnv expression =
      case expression of
        EOperatorValue operatorSymbol
          | isBuiltinOperatorSymbol operatorSymbol ->
              Just (operatorSymbol, Nothing)
        EApply dollarExpr operatorExpr
          | builtinDollarOperatorExpr currentEnv dollarExpr ->
              builtinOperatorSymbolExpr currentEnv operatorExpr
        EVar name ->
          case Map.lookup name currentEnv of
            Just (BuiltinOperatorAliasTypeBinding operatorSymbol) ->
              Just (operatorSymbol, Nothing)
            Just (OperatorAliasSchemeTypeBinding operatorSymbol typeScheme) ->
              Just (operatorSymbol, Just typeScheme)
            _ -> Nothing
        _ -> Nothing

    builtinOperatorAliasSymbol :: Text -> Bool
    builtinOperatorAliasSymbol operatorSymbol =
      isBuiltinOperatorSymbol operatorSymbol && operatorSymbol /= "|"

    nextBindingForValue ::
      Int ->
      TypeEnv ->
      Expr ->
      Maybe ExpressionType ->
      Maybe PendingSignatureType ->
      InferState ->
      Maybe TypeBinding
    nextBindingForValue statementIndex currentEnv valueExpr maybeInferredType maybePendingSignature state =
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
                    Just _ ->
                      monomorphicBinding
                    Nothing ->
                      case lookupBuiltinSymbolInMode builtinMode referencedName of
                        Just builtinSymbol -> Just (BuiltinAliasTypeBinding builtinSymbol)
                        Nothing -> monomorphicBinding
            _ -> monomorphicBinding

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
            shouldGeneralizeExplicitSignatureBinding pendingSignature ->
              Just (generalizedExplicitSignatureBinding currentEnv state pendingSignature)
        Just inferredType
          | shouldGeneralizeOrdinaryBinding statementIndex currentEnv valueExpr maybePendingSignature ->
              Just (generalizedOrdinaryBinding currentEnv state inferredType)
        _ -> PlainTypeBinding <$> maybeInferredType

    shouldGeneralizeExplicitSignatureBinding :: PendingSignatureType -> Bool
    shouldGeneralizeExplicitSignatureBinding pendingSignature =
      ( not (null (pendingSignatureExplicitConstraints pendingSignature))
          || not (null (pendingSignatureVariableOrder pendingSignature))
      )

    setRigidTypeVariables rigidVariables state =
      state
        { inferSolver =
            (inferSolver state) {solverRigidTypeVars = rigidVariables}
        }

    restoreRigidTypeVariables originalState =
      setRigidTypeVariables (inferRigidTypeVars originalState)

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
          | Just (firstMember, _) <- uncons groupMembers,
            statementIndex == firstMember ->
              Map.insert firstMember state groupStartStates
        _ -> groupStartStates

    generalizeCompletedRecursiveGroup :: Map Int PendingSignatureType -> Int -> TypeEnv -> Map Int InferState -> InferState -> (TypeEnv, InferState)
    generalizeCompletedRecursiveGroup pendingSignatures statementIndex currentEnv groupStartStates state =
      case Map.lookup statementIndex recursiveGroupsByStatement of
        Just groupMembers
          | Just (firstMember, _) <- uncons groupMembers,
            Just (_, lastMember) <- unsnoc groupMembers,
            statementIndex == lastMember ->
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
                    Map.findWithDefault state firstMember groupStartStates
                  groupBindings =
                    [ binding
                      | memberIndex <- groupMembers,
                        Just bindingName <- [Map.lookup memberIndex bindingNamesByStatement],
                        Just binding <- [Map.lookup bindingName nextEnv]
                    ]
               in ( nextEnv,
                  pruneCapturedInferredClassConstraintsForBindings groupStartState groupBindings state
                )
        _ -> (currentEnv, state)

    exposeVisibleRecursiveGroupSchemes :: Int -> TypeEnv -> InferState -> (TypeEnv, InferState)
    exposeVisibleRecursiveGroupSchemes statementIndex currentEnv state =
      foldl' exposeGroup (currentEnv, state) recursiveGroups
      where
        recursiveGroups =
          Set.toList (Set.fromList (Map.elems recursiveGroupsByStatement))

        exposeGroup (envAcc, stateAcc) groupMembers =
          case unsnoc groupMembers of
            Nothing ->
              (envAcc, stateAcc)
            Just (_, lastMember)
              | statementIndex `elem` groupMembers ->
                  (envAcc, stateAcc)
              | statementIndex > lastMember ->
                  (envAcc, stateAcc)
              | any (`Set.member` signedBindingStatements) groupMembers ->
                  (envAcc, stateAcc)
              | interleavedBindingFeedsLaterGroup statementIndex groupMembers ->
                  (envAcc, stateAcc)
              | laterGroupMemberDependsOnInterveningBinding statementIndex groupMembers ->
                  (envAcc, stateAcc)
              | null processedMembers ->
                  (envAcc, stateAcc)
              | otherwise ->
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
                          nextState = rollbackPreviewState stateAcc previewState
                       in (nextEnv, nextState)
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
            else Just (discardPreviewOutput state previewState)
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
                    case ( shouldSeedSelfRecursiveFunction memberIndex bindingName currentEnv,
                          Map.lookup memberIndex bindingSeedsByStatement
                        ) of
                      (True, Just bindingSeed) ->
                        Map.insert bindingName (PlainTypeBinding bindingSeed) envWithRecursiveBindings
                      _ -> envWithRecursiveBindings
                  (valueType, rawStateAfterValue) =
                    inferPlain builtinMode envWithBindingSeed stateAcc valueExpr
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

        discardPreviewOutput originalState previewState =
          modifyInferenceOutput
            ( \output ->
                output
                  { outputErrorsRev = inferErrorsRev originalState,
                    outputRuntimeHints = inferRuntimeTypeHints originalState,
                    outputDeferredConstraints = inferDeferredExplicitConstraints originalState,
                    outputInferredConstraints = inferInferredClassConstraints originalState
                  }
            )
            previewState

        previewIntroducedDiagnostics originalState previewState =
          length (inferErrorsRev previewState) /= length (inferErrorsRev originalState)

    -- Preview inference is a transaction: its resolved types may be used to
    -- expose a temporary scheme, but none of its semantic state belongs to
    -- the real traversal. Keep only the allocation watermark so type-variable
    -- identifiers embedded in that temporary scheme cannot be reused.
    rollbackPreviewState originalState previewState =
      originalState
        { inferSolver =
            (inferSolver originalState)
              { solverNextTypeVar = solverNextTypeVar (inferSolver previewState)
              }
        }

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
        (Just (SLet _ _ _), Just bindingName)
          | Just pendingSignature <- Map.lookup memberIndex pendingSignatures,
            shouldGeneralizeExplicitSignatureBinding pendingSignature ->
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

data ForwardFunctionBinding = ForwardFunctionBinding
  { forwardFunctionName :: Name,
    forwardFunctionType :: ExpressionType
  }

data PreparedSignature
  = PreparedSignature (Maybe PendingSignatureType) Bool

data ScopePreparation = ScopePreparation
  { preparedBindingSeeds :: Map Int ExpressionType,
    preparedSignatures :: Map Int PreparedSignature,
    preparedForwardFunctions :: Map Int ForwardFunctionBinding,
    preparedScopeState :: InferState
  }

prepareScope ::
  Bool ->
  TypedCoreProductionMode ->
  Map Text DataTypeBinding ->
  [(Int, Statement)] ->
  InferState ->
  ScopePreparation
prepareScope allowForwardSignedFunctions mode predeclaredDataTypes indexedStatements initialState =
  let (bindingSeeds, signatures, forwardFunctions, _, _, finalPreparationState) =
        foldl'
          step
          ( Map.empty,
            Map.empty,
            Map.empty,
            Nothing,
            capabilityFactsFromState initialState,
            initialState
          )
          indexedStatements
   in ScopePreparation
      { preparedBindingSeeds = bindingSeeds,
        preparedSignatures = signatures,
        preparedForwardFunctions = forwardFunctions,
        preparedScopeState =
          initialState
            { inferSolver =
                (inferSolver initialState)
                  { solverNextTypeVar = solverNextTypeVar (inferSolver finalPreparationState)
                  }
            }
      }
  where
    step
      (bindingSeeds, signatures, forwardFunctions, pendingSignature, moduleBaselineFacts, state)
      (statementIndex, statement) =
      case statement of
        SModule _ modulePath ->
          ( bindingSeeds,
            signatures,
            forwardFunctions,
            Nothing,
            moduleBaselineFacts,
            enterModuleCapabilityScope moduleBaselineFacts modulePath state
          )
        SImport _ modulePath maybeAlias maybeSymbolNames ->
          ( bindingSeeds,
            signatures,
            forwardFunctions,
            Nothing,
            moduleBaselineFacts,
            importModuleCapabilityFacts modulePath maybeAlias maybeSymbolNames state
          )
        SClass classSpan capabilityName parameters methods ->
          let validationState =
                seedStatementCapabilityFact
                  state
                  (SClass classSpan capabilityName parameters [])
              nextState =
                case firstInvalidClassMethodSignature validationState capabilityName parameters methods of
                  Just _ -> state
                  Nothing -> seedStatementCapabilityFact state statement
             in ( bindingSeeds,
              signatures,
              forwardFunctions,
              Nothing,
              updateRootModuleBaselineFacts moduleBaselineFacts state nextState,
              nextState
            )
        SImpl implSpan _capabilityName arguments _ ->
          let nextState =
                case firstInvalidImplTarget state implSpan arguments of
                  Just _ -> state
                  Nothing -> seedStatementCapabilityFact state statement
             in ( bindingSeeds,
              signatures,
              forwardFunctions,
              Nothing,
              updateRootModuleBaselineFacts moduleBaselineFacts state nextState,
              nextState
            )
        SData spanValue typeName typeParameters constructors ->
          let (_, nextState) =
                registerDataConstructors
                  predeclaredDataTypes
                  spanValue
                  typeName
                  typeParameters
                  constructors
                  Map.empty
                  state
             in ( bindingSeeds,
              signatures,
              forwardFunctions,
              Nothing,
              moduleBaselineFacts,
              nextState
            )
        SSignature name signatureSpan signaturePayload ->
          let (maybeSignatureType, stateAfterSignature) =
                Signature.signaturePayloadToSignatureType signaturePayload state
              maybePendingSignature =
                fmap
                  ( \signatureType ->
                      PendingSignatureType
                        (identifierText name)
                        signatureSpan
                        (Signature.signaturePayloadDeclaredType signatureType)
                        (Signature.signaturePayloadExplicitConstraints signatureType)
                        (Signature.signaturePayloadVariableOrder signatureType)
                  )
                  maybeSignatureType
              preparedSignature =
                PreparedSignature
                  maybePendingSignature
                    ( maybe
                        False
                      (\signature -> unconstrainedSignaturePayload signaturePayload && eligibleForwardSignature signature)
                      maybePendingSignature
                  )
             in ( bindingSeeds,
              Map.insert statementIndex preparedSignature signatures,
              forwardFunctions,
              Just preparedSignature,
              moduleBaselineFacts,
              restoreCapabilityFacts state stateAfterSignature
            )
        SLet bindingName _bindingSpan bindingExpression ->
          let (bindingSeed, nextState) = freshTypeVar state
              nextForwardFunctions =
                case pendingSignature of
                  Just (PreparedSignature (Just signature) True)
                    | allowForwardSignedFunctions,
                      mode == ProduceTypedCoreExpressionDirectCall,
                      pendingSignatureName signature == identifierText bindingName,
                      ELambda {} <- bindingExpression,
                      concreteForwardFunctionType (pendingSignatureDeclaredType signature) ->
                        Map.insert
                          statementIndex
                          (ForwardFunctionBinding bindingName (pendingSignatureDeclaredType signature))
                          forwardFunctions
                  _ -> forwardFunctions
             in ( Map.insert statementIndex bindingSeed bindingSeeds,
              signatures,
              nextForwardFunctions,
              Nothing,
              moduleBaselineFacts,
              nextState
            )
        _ ->
          ( bindingSeeds,
            signatures,
            forwardFunctions,
            Nothing,
            moduleBaselineFacts,
            state
          )

    unconstrainedSignaturePayload signaturePayload =
      case signaturePayload of
        ConstrainedSignature constraints _ -> null constraints
        _ -> True

    eligibleForwardSignature signature =
      null (pendingSignatureVariableOrder signature)
        && null (pendingSignatureExplicitConstraints signature)
        && concreteForwardFunctionType (pendingSignatureDeclaredType signature)

    concreteForwardFunctionType expressionType =
      case expressionType of
        TFunctionType argumentType resultType ->
          concreteForwardScalarType argumentType
            && (concreteForwardScalarType resultType || concreteForwardFunctionType resultType)
        _ -> False

    concreteForwardScalarType expressionType =
      case expressionType of
        TIntType -> True
        TFloatType -> True
        TNumericType {} -> True
        TBoolType -> True
        TCharType -> True
        TTupleType [] -> True
        _ -> False

predeclareScopeDataTypes ::
  [(Int, Statement)] ->
  InferState ->
  Map Text DataTypeBinding
predeclareScopeDataTypes indexedStatements initialState =
  foldl' step Map.empty indexedStatements
  where
    step predeclaredDataTypes (_, statement) =
      case statement of
        SData _ typeName typeParameters _
          | Map.notMember typeNameText (inferDataTypes initialState),
            Map.notMember typeNameText predeclaredDataTypes ->
              Map.insert
                typeNameText
                (DataTypeBinding typeParameters [])
                predeclaredDataTypes
          where
            typeNameText = identifierText typeName
        _ -> predeclaredDataTypes

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
      case ( Map.lookup memberIndex bindingNamesByStatement,
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
   in if Set.null schemeVariables
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

addUndeclaredSignatureConstraintErrors :: Text -> InferState -> PendingSignatureType -> InferState -> InferState
addUndeclaredSignatureConstraintErrors bindingName statementStartState pendingSignature state
  | inferErrorCount state > inferErrorCount statementStartState = state
  | otherwise = foldl' addMissingConstraint state missingObligations
  where
    signatureVariables = Set.fromList (pendingSignatureVariableOrder pendingSignature)
    declaredConstraints =
      map (resolveTypeSchemeConstraint state) (pendingSignatureExplicitConstraints pendingSignature)

    inferredObligations =
      [ (False, constraintName, targetType)
        | constraint <- newInferredClassConstraints statementStartState state,
          Just (constraintName, targetType) <- [constraintIdentity (resolveTypeSchemeConstraint state constraint)],
          targetUsesSignatureVariables targetType
      ]

    primitiveObligations =
      [ case primitiveConstraint of
          TypeSchemeNumericConstraint _ targetType -> (True, "Num", targetType)
          TypeSchemeStrictEqualityConstraint targetType -> (True, "Eq", targetType)
        | primitiveConstraint <- typeSchemePrimitiveConstraints state signatureVariables
      ]

    missingObligations =
      dedupeObligations
        [ obligation
          | obligation@(_, constraintName, targetType) <- inferredObligations ++ primitiveObligations,
            not (declaredConstraintEntails constraintName targetType)
        ]

    targetUsesSignatureVariables targetType =
      let targetVariables = freeTypeVariables (resolveType state targetType)
       in not (Set.null targetVariables)
            && targetVariables `Set.isSubsetOf` signatureVariables

    declaredConstraintEntails requiredName requiredTarget =
      any matches declaredConstraints
      where
        matches declaredConstraint =
          case constraintIdentity declaredConstraint of
            Just (declaredName, declaredTarget) ->
              declaredName == requiredName
                && resolveType state declaredTarget == resolveType state requiredTarget
            Nothing -> False

    constraintIdentity constraint =
      case constraint of
        TypeSchemeConstraint constraintName targetType -> Just (constraintName, targetType)
        TypeSchemeInferredConstraint constraintName targetType -> Just (constraintName, targetType)
        TypeSchemeMethodConstraint constraintName _ targetType -> Just (constraintName, targetType)

    dedupeObligations =
      foldl' insertObligation []
      where
        insertObligation obligations obligation@(_, constraintName, targetType)
          | any (sameObligation constraintName targetType) obligations = obligations
          | otherwise = obligations ++ [obligation]
        sameObligation constraintName targetType (_, existingName, existingTarget) =
          constraintName == existingName
            && resolveType state targetType == resolveType state existingTarget

    addMissingConstraint stateAcc (primitive, constraintName, targetType) =
      addTypeError
        stateAcc
        ( mkUndeclaredSignatureConstraintError
            bindingName
            primitive
            constraintName
            (resolveType state targetType)
            (pendingSignatureSpan pendingSignature)
        )

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
        TCharType -> []
        TTextType -> []
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

targetedFloatLiteralDiagnostic :: NumericType -> Double -> FractionalLiteralSource -> Maybe Diagnostic
targetedFloatLiteralDiagnostic targetType literalValue literalSource =
  case numericTypeFloatMax targetType of
    Just maxMagnitude
      | not (finiteFloat literalValue)
          || abs literalValue > maxMagnitude
          || fractionalLiteralExceedsMagnitude literalSource maxMagnitude ->
          Just (mkTargetedFractionalLiteralOverflowError literalValue targetType maxMagnitude)
    _ -> Nothing
  where
    finiteFloat value = not (isNaN value) && not (isInfinite value)

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

registerDataConstructors :: Map Text DataTypeBinding -> SourceSpan -> Name -> [Name] -> [DataConstructor] -> TypeEnv -> InferState -> (TypeEnv, InferState)
registerDataConstructors predeclaredDataTypes spanValue typeName typeParameters constructors env initialState =
  case Map.lookup typeNameText (inferDataTypes initialState) of
    Just _ ->
      ( env,
        addTypeError
          initialState
          (mkDuplicateDataTypeDeclarationError typeNameText spanValue)
      )
    Nothing -> registerInto initialState
  where
    typeNameText = identifierText typeName

    registerInto stateBeforeConstructors =
      let (nextEnv, nextState, constructorPayloadsRev) =
            foldl' register (env, stateBeforeConstructors, []) constructors
       in ( nextEnv,
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

    register (envAcc, stateAcc, constructorPayloadsAcc) (DataConstructor constructorName constructorArguments) =
      let (argumentTypes, nextState) =
            constructorArgumentTypes predeclaredDataTypes typeParameters constructorArguments stateAcc
       in ( Map.insert
            constructorName
            (ConstructorTypeBinding typeName typeParameters argumentTypes)
            envAcc,
          nextState,
          argumentTypes : constructorPayloadsAcc
        )

constructorArgumentTypes :: Map Text DataTypeBinding -> [Name] -> [SignatureType] -> InferState -> ([ConstructorArgumentType], InferState)
constructorArgumentTypes predeclaredDataTypes typeParameters fieldTypes initialState =
  foldl' collectField ([], initialState) fieldTypes
  where
    signatureVariables =
      Map.fromList
        [ (identifierText parameterName, TVarType (negate position - 1))
          | (position, parameterName) <- zip [0 :: Int ..] typeParameters
        ]

    collectField (argumentTypes, stateAcc) fieldType =
      case Signature.signatureTypeToExpressionType (stateWithPredeclaredDataTypes stateAcc) signatureVariables fieldType of
        Right _ ->
          ( argumentTypes
              ++ [ConstructorArgumentStructured fieldType],
            stateAcc
          )
        Left (Signature.UnknownNamedType payloadName) ->
          ( argumentTypes ++ [ConstructorArgumentFresh],
            addTypeError stateAcc (mkUnknownConstructorPayloadTypeError payloadName)
          )
        Left failure ->
          ( argumentTypes ++ [ConstructorArgumentFresh],
            addTypeError
              stateAcc
              (mkInvalidConstructorPayloadTypeError (Signature.renderSignatureTypeFailure failure))
          )

    stateWithPredeclaredDataTypes state =
      modifyDeclarationState
        ( \declarations ->
            declarations
              { declarationDataTypes =
                  Map.union
                    (inferDataTypes state)
                    predeclaredDataTypes
              }
        )
        state

-- | Instantiate non-builtin local bindings and constructors at use sites.
-- Builtin aliases stay with the top-level dispatcher because their rules share
-- the operator and primitive catalog owned there.
instantiateNonBuiltinTypeBinding :: TypeBinding -> InferState -> (Maybe ExpressionType, InferState)
instantiateNonBuiltinTypeBinding binding state =
  case binding of
    PlainTypeBinding expressionType ->
      (Just (resolveType state expressionType), state)
    SchemeTypeBinding typeScheme ->
      instantiateTypeScheme typeScheme state
    BuiltinAliasTypeBinding {} -> (Nothing, state)
    BuiltinOperatorAliasTypeBinding {} -> (Nothing, state)
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
  InferExprFn ->
  BuiltinResolutionMode ->
  TypeEnv ->
  InferState ->
  Expr ->
  SourceSpan ->
  SignatureType ->
  (Maybe ExpressionType, InferState)
inferExplicitTypeApplication inferExpression builtinMode env state functionExpr typeArgumentSpan typeArgument =
  let (expressionType, finalState, _) =
        inferExplicitTypeApplicationInternal
          ( \builtin childEnv childState childExpr ->
              let (childType, nextState) =
                    inferExpression builtin childEnv childState childExpr
               in (childType, nextState, Nothing)
          )
          builtinMode
          env
          state
          functionExpr
          typeArgumentSpan
          typeArgument
   in (expressionType, finalState)

inferExplicitTypeApplicationWithResult ::
  (BuiltinResolutionMode -> TypeEnv -> InferState -> Expr -> (InferredExpr, InferState)) ->
  BuiltinResolutionMode ->
  TypeEnv ->
  InferState ->
  Expr ->
  SourceSpan ->
  SignatureType ->
  (Maybe ExpressionType, InferState, Maybe InferredExpr)
inferExplicitTypeApplicationWithResult inferExpression builtinMode env state functionExpr typeArgumentSpan typeArgument =
  inferExplicitTypeApplicationInternal
    ( \builtin childEnv childState childExpr ->
        let (childResult, nextState) =
              inferExpression builtin childEnv childState childExpr
         in (inferredExpressionType childResult, nextState, Just childResult)
    )
    builtinMode
    env
    state
    functionExpr
    typeArgumentSpan
    typeArgument

inferExplicitTypeApplicationInternal ::
  (BuiltinResolutionMode -> TypeEnv -> InferState -> Expr -> (Maybe ExpressionType, InferState, Maybe result)) ->
  BuiltinResolutionMode ->
  TypeEnv ->
  InferState ->
  Expr ->
  SourceSpan ->
  SignatureType ->
  (Maybe ExpressionType, InferState, Maybe result)
inferExplicitTypeApplicationInternal inferExpression builtinMode env state functionExpr typeArgumentSpan typeArgument =
  case (explicitTypeApplicationScheme env functionExpr, Signature.constraintSignatureTypeToExpressionTypeWithState state Map.empty typeArgument) of
    (_, Just explicitArgumentType)
      | Just methodKey <- explicitQualifiedMethodTypeApplicationKey env state functionExpr ->
          let (maybeInstantiatedType, nextState) =
                instantiateQualifiedMethodTypeWithExplicitTarget methodKey explicitArgumentType state
           in ( maybeInstantiatedType,
                recordExplicitTypeApplicationRuntimeHint typeArgumentSpan maybeInstantiatedType nextState,
                Nothing
            )
    (Just typeScheme, Just explicitArgumentType) ->
      let (maybeInstantiatedType, nextState) =
            instantiateTypeSchemeWithExplicitArgument typeScheme explicitArgumentType state
       in ( maybeInstantiatedType,
            recordExplicitTypeApplicationRuntimeHint typeArgumentSpan maybeInstantiatedType nextState,
            Nothing
        )
    (Just _, Nothing) ->
      (Nothing, addTypeError state (mkInvalidExplicitTypeApplicationArgumentError state typeArgumentSpan typeArgument), Nothing)
    (Nothing, _) ->
      let (maybeFunctionType, stateAfterFunction, maybeFunctionResult) =
            inferExpression builtinMode env state functionExpr
       in case maybeFunctionType of
          Just _ ->
            (Nothing, addTypeError stateAfterFunction mkExplicitTypeApplicationTargetError, maybeFunctionResult)
          Nothing -> (Nothing, stateAfterFunction, maybeFunctionResult)

explicitQualifiedMethodTypeApplicationKey :: TypeEnv -> InferState -> Expr -> Maybe Text
explicitQualifiedMethodTypeApplicationKey env state functionExpr =
  case functionExpr of
    EVar name
      | Map.notMember name env,
        qualifiedMethodClassIsVisible methodKey state ->
          Just methodKey
      where
        methodKey = identifierText name
    _ -> Nothing

recordExplicitTypeApplicationRuntimeHint :: SourceSpan -> Maybe ExpressionType -> InferState -> InferState
recordExplicitTypeApplicationRuntimeHint typeArgumentSpan maybeExpressionType state =
  case maybeExpressionType >>= runtimeHintFromExpressionType state of
    Just runtimeHint ->
      modifyInferenceOutput
        ( \output ->
            output
              { outputRuntimeHints =
                  Map.insert
                    (explicitTypeApplicationRuntimeHintKeyInModule (inferRuntimeHintPath state) typeArgumentSpan)
                    runtimeHint
                    (inferRuntimeTypeHints state)
              }
        )
        state
    Nothing -> state

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

runtimeHintForBinding :: InferState -> Maybe TypeBinding -> Maybe ExpressionType -> Maybe SignatureType
runtimeHintForBinding state maybeBinding maybeExpressionType =
  case maybeBinding >>= runtimeHintForTypeBinding state of
    Just runtimeHint -> Just runtimeHint
    Nothing -> maybeExpressionType >>= runtimeHintFromExpressionType state

runtimeHintForTypeBinding :: InferState -> TypeBinding -> Maybe SignatureType
runtimeHintForTypeBinding state binding =
  case binding of
    PlainTypeBinding expressionType ->
      runtimeHintFromExpressionType state expressionType
    SchemeTypeBinding typeScheme ->
      typeSchemeRuntimeHint state typeScheme
    OperatorAliasSchemeTypeBinding _ typeScheme ->
      typeSchemeRuntimeHint state typeScheme
    _ -> Nothing

typeSchemeRuntimeHint :: InferState -> TypeScheme -> Maybe SignatureType
typeSchemeRuntimeHint state typeScheme =
  case resolvedSchemeType of
    TFunctionType {} ->
      Signature.expressionTypeToRuntimeTemplate runtimeTemplateVariables resolvedSchemeType
    _ -> Nothing
  where
    expressionType = schemeResultType typeScheme
    resolvedSchemeType =
      defaultLiteralTypes (resolveType state expressionType)
    orderedVariables =
      orderedSchemeVariables
        (schemeQuantifiedOrder typeScheme)
        (schemeQuantifiedVariables typeScheme)
    runtimeTemplateVariables =
      Map.fromList
        [ (typeVar, sourceName (mkIdentifier ("t" <> Text.pack (show position))))
          | (position, typeVar) <- zip [0 :: Int ..] orderedVariables
        ]

runtimeHintFromExpressionType :: InferState -> ExpressionType -> Maybe SignatureType
runtimeHintFromExpressionType state expressionType =
  Signature.expressionTypeToRuntimeHint (defaultLiteralTypes (resolveType state expressionType))
