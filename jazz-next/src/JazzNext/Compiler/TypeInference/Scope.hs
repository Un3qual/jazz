{-# LANGUAGE OverloadedStrings #-}

-- | Scope, binding, signature, and constructor inference.
module JazzNext.Compiler.TypeInference.Scope
  ( inferExplicitTypeApplication,
    inferScopeType,
    instantiateNonBuiltinTypeBinding
  ) where

import Control.Applicative ((<|>))
import Data.List (foldl')
import Data.Maybe (isNothing)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import JazzNext.Compiler.AST
  ( CaseArm (..),
    ConstraintSignatureType (..),
    DataConstructor (..),
    DataConstructorArgument (..),
    Expr (..),
    Literal (..),
    NumericType (..),
    SignatureType,
    Statement (..)
  )
import JazzNext.Compiler.BuiltinCatalog
  ( BuiltinResolutionMode (..),
    builtinNamesInMode,
    lookupBuiltinSymbolInMode,
    numericTypeFloatMax
  )
import JazzNext.Compiler.Diagnostics
  ( Diagnostic (..),
    SourceSpan
  )
import JazzNext.Compiler.FractionalLiteral
  ( FractionalLiteralSource,
    fractionalLiteralExceedsMagnitude
  )
import JazzNext.Compiler.Identifier
  ( identifierText,
    mkIdentifier
  )
import JazzNext.Compiler.Name
  ( GeneratedNameKind (..),
    Name (..),
    operatorBindingName,
    sourceName
  )
import JazzNext.Compiler.Parser.Operator (isBuiltinOperatorSymbol)
import JazzNext.Compiler.RecursiveBindings
  ( collectBindingNames,
    freeVarsExprWithBound,
    inferRecursiveGroupsOrdered,
    inferSelfRecursiveBindings
  )
import JazzNext.Compiler.RuntimeHints (bindingRuntimeHintKeyInModule)
import JazzNext.Compiler.TypeInference.Capabilities
import JazzNext.Compiler.TypeInference.Diagnostics
import JazzNext.Compiler.TypeInference.Pattern (instantiateConstructorBinding)
import JazzNext.Compiler.TypeInference.Solver
  ( freshTypeVar,
    resolveType,
    unifyTypes
  )
import JazzNext.Compiler.TypeInference.State
  ( DeclarationState (..),
    InferState (..),
    InferenceOutput (..),
    ModuleInferenceState (..),
    inferCurrentModulePath,
    inferDataTypes,
    inferDeferredExplicitConstraints,
    inferErrorsRev,
    inferInferredClassConstraints,
    inferNumericVars,
    inferRuntimeTypeHints,
    inferStrictEqualityVars
  )
import JazzNext.Compiler.TypeInference.Types
  ( ConstructorArgumentType (..),
    DataTypeBinding (..),
    ExpressionType (..),
    TypeBinding (..),
    TypeEnv,
    TypeScheme (..),
    TypeSchemeConstraint (..),
    TypeSchemePrimitiveConstraint (..)
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
    _ -> inferExpression builtinMode env state expr

modifyDeclarationState :: (DeclarationState -> DeclarationState) -> InferState -> InferState
modifyDeclarationState update state =
  state {inferDeclarations = update (inferDeclarations state)}

modifyInferenceOutput :: (InferenceOutput -> InferenceOutput) -> InferState -> InferState
modifyInferenceOutput update state =
  state {inferOutput = update (inferOutput state)}

publishVisibleTypes :: TypeEnv -> InferState -> InferState
publishVisibleTypes env state =
  state
    { inferModule =
        (inferModule state) {inferenceVisibleTypes = env}
    }

inferScopeType :: InferExprFn -> BuiltinResolutionMode -> TypeEnv -> InferState -> [Statement] -> (Maybe ExpressionType, InferState)
inferScopeType inferExpression builtinMode initialEnv initialState statements =
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
        [] -> (lastExprType, publishVisibleTypes env state)
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
                    checkImplMethodBodies inferExpression builtinMode env seededState capabilityName arguments methods
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
                            inferExprTypeWithExpected inferExpression builtinMode envWithPendingSignature stateForStatement expectedValueType valueExpr
                          Nothing ->
                            inferExpression builtinMode envWithPendingSignature stateForStatement valueExpr
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
                  (exprType, rawStateAfterExpr) = inferExpression builtinMode envForStatement stateForStatement expr
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
                    inferExpression builtinMode envWithBindingSeed stateAcc valueExpr
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
  SignatureType ->
  (Maybe ExpressionType, InferState)
inferExplicitTypeApplication inferExpression builtinMode env state functionExpr typeArgument =
  case explicitTypeApplicationScheme env functionExpr of
    Just typeScheme ->
      instantiateTypeSchemeWithExplicitArgument
        typeScheme
        (signatureTypeToExpressionType typeArgument)
        state
    Nothing ->
      let (maybeFunctionType, stateAfterFunction) =
            inferExpression builtinMode env state functionExpr
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
