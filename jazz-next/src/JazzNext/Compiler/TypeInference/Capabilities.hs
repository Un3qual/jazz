{-# LANGUAGE OverloadedStrings #-}

module JazzNext.Compiler.TypeInference.Capabilities
  ( SignaturePayloadType (..),
    applyCapabilityFacts,
    addInferredEqualityClassConstraintIfVisible,
    addUnpreservedInferredMethodConstraintErrors,
    applyTypeSchemePrimitiveConstraints,
    builtinDollarOperatorExpr,
    capabilityFactsFromState,
    checkImplMethodBodies,
    classMethodPayloadToExpressionType,
    defaultBindingLiteralTypes,
    defaultLiteralTypes,
    deferExplicitConstraints,
    deferExplicitConstraintsWithFacts,
    enterModuleCapabilityScope,
    expressionTypeToRuntimeHint,
    finalizeDeferredExplicitConstraintsAt,
    flushCurrentModuleCapabilityFacts,
    freeTypeVariables,
    freeTypeVariablesInEnv,
    freeTypeVariablesInTypeSchemeConstraints,
    freshTypeVars,
    importModuleCapabilityFacts,
    inferQualifiedMethodApplication,
    instantiateQualifiedMethodType,
    instantiateTypeSchemeConstraint,
    instantiateTypeSchemePrimitiveConstraint,
    mergeCapabilityFacts,
    qualifiedMethodClassIsVisible,
    replaceTypeVariables,
    resolveTypeSchemeConstraint,
    restoreCapabilityFacts,
    seedFacts,
    seedStatementCapabilityFact,
    typeSchemeDefiningFactsFromState,
    typeSchemeReferencedCapabilityFacts,
    constraintSignatureTypeToExpressionType,
    constraintSignatureTypeToExpressionTypeWithState,
    dedupeTypeSchemeConstraints,
    signaturePayloadToSignatureType,
    structuralRuntimeEqualityType,
    updateRootModuleBaselineFacts
  ) where

import Control.Applicative ((<|>))
import Data.List (foldl')
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
    Expr (..),
    ImplMethod (..),
    NumericType (..),
    SignatureConstraint (..),
    SignaturePayload (..),
    SignatureType (..),
    Statement (..)
  )
import JazzNext.Compiler.BuiltinCatalog
  ( BuiltinResolutionMode,
    numericTypeFromName,
    numericTypeIsIntegral,
    renderNumericTypeName
  )
import JazzNext.Compiler.CapabilityFacts
  ( concreteConstraintArgument,
    concreteImplFactClassName,
    concreteImplFactKey,
    constraintImplFactKey,
    constraintFunctionArgumentTypes,
    constraintSignatureTypeVariableNamesInOrder,
    constraintSignatureAliasVariants,
    constraintSignatureTypeContainsClassParameter,
    constraintSignatureTypesCompatible,
    identifierLooksLikeTypeVariable,
    normalizeConstraintSignatureName,
    qualifiedMethodKey,
    renderCapabilityType,
    signaturePayloadConstraintType,
    splitQualifiedMethodKey,
    substituteClassMethodSignature
  )
import JazzNext.Compiler.Diagnostics
  ( SourceSpan,
    setDiagnosticPrimarySpan
  )
import JazzNext.Compiler.Name
  ( Name,
    identifierText,
    qualifiedMemberName,
    renderName
  )
import JazzNext.Compiler.TypeInference.Diagnostics
  ( InferExprFn,
    addTypeError,
    annotateNewErrorsWithPrimarySpan,
    duplicateConstraintName,
    mkAmbiguousDeferredConstraintError,
    mkAmbiguousQualifiedMethodBodyError,
    mkAmbiguousQualifiedMethodBodyForArgumentsError,
    mkApplyTypeError,
    mkExplicitConstraintArityError,
    mkImplMethodMissingClassMethodError,
    mkImplMethodTypeMismatchError,
    mkInvalidQualifiedMethodSignatureError,
    mkMissingClassMethodError,
    mkMissingExplicitConstraintClassError,
    mkMissingExplicitConstraintImplFactError,
    mkMissingImplMethodBodyError,
    mkNoMatchingQualifiedMethodBodyError,
    mkTypeSchemeNumericConstraintError,
    mkTypeSchemeStrictEqualityConstraintError
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
    inferGeneratedEqualityClassFacts,
    inferInferredClassConstraints,
    inferModuleCapabilityFacts,
  )
import JazzNext.Compiler.TypeInference.Solver
  ( addStrictEqualityTypeVarConstraint,
    constrainNumericOperatorType,
    freshTypeVar,
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
    ScopeCapabilityFacts (..),
    TypeBinding (..),
    TypeEnv,
    TypeScheme (..),
    TypeSchemeConstraint (..),
    TypeSchemePrimitiveConstraint (..),
    emptyScopeCapabilityFacts
  )

capabilityFactsFromState :: InferState -> ScopeCapabilityFacts
capabilityFactsFromState state =
  ScopeCapabilityFacts
    { scopeClassFacts = inferClassFacts state,
      scopeGeneratedEqualityClassFacts = inferGeneratedEqualityClassFacts state,
      scopeConcreteImplFacts = inferConcreteImplFacts state,
      scopeClassMethodSignatures = inferClassMethodSignatures state,
      scopeConcreteImplMethods = inferConcreteImplMethods state
    }

typeSchemeDefiningFactsFromState :: InferState -> [TypeSchemeConstraint] -> ScopeCapabilityFacts
typeSchemeDefiningFactsFromState state schemeConstraints =
  capturedFacts
    { scopeGeneratedEqualityClassFacts =
        Set.union
          inferredStructuralEqualityClasses
          (scopeGeneratedEqualityClassFacts capturedFacts)
    }
  where
    capturedFacts =
      case inferCurrentModulePath state of
        Just _ -> typeSchemeReferencedCapabilityFacts schemeConstraints (capabilityFactsFromState state)
        Nothing -> capabilityFactsFromState state
    inferredStructuralEqualityClasses =
      Set.fromList
        [ capabilityName
          | TypeSchemeInferredConstraint capabilityName _ <- schemeConstraints,
            activeEqualityClassName state == Just capabilityName
        ]

typeSchemeReferencedCapabilityFacts :: [TypeSchemeConstraint] -> ScopeCapabilityFacts -> ScopeCapabilityFacts
typeSchemeReferencedCapabilityFacts schemeConstraints facts =
  facts
    { scopeClassFacts =
        Map.filterWithKey
          (\className _ -> Set.member className referencedCapabilityNames)
          (scopeClassFacts facts),
      scopeConcreteImplFacts =
        Set.filter
          (\implKey -> Set.member (concreteImplFactClassName implKey) referencedCapabilityNames)
          (scopeConcreteImplFacts facts),
      scopeGeneratedEqualityClassFacts =
        Set.filter
          (`Set.member` referencedCapabilityNames)
          (scopeGeneratedEqualityClassFacts facts),
      scopeClassMethodSignatures =
        Map.filterWithKey
          (\methodKey _ -> methodKeyReferencesCapturedCapability methodKey)
          (scopeClassMethodSignatures facts),
      scopeConcreteImplMethods =
        Map.filterWithKey
          (\methodKey _ -> methodKeyReferencesCapturedCapability methodKey)
          (scopeConcreteImplMethods facts)
    }
  where
    referencedCapabilityNames =
      Set.fromList
        [ constraintName
          | schemeConstraint <- schemeConstraints,
            let constraintName = typeSchemeConstraintCapabilityName schemeConstraint
        ]

    methodKeyReferencesCapturedCapability methodKey =
      case splitQualifiedMethodKey methodKey of
        Just (className, _) -> Set.member className referencedCapabilityNames
        Nothing -> False

typeSchemeConstraintCapabilityName :: TypeSchemeConstraint -> Text
typeSchemeConstraintCapabilityName constraint =
  case constraint of
    TypeSchemeConstraint constraintName _ -> constraintName
    TypeSchemeInferredConstraint constraintName _ -> constraintName
    TypeSchemeMethodConstraint constraintName _ _ -> constraintName

applyCapabilityFacts :: ScopeCapabilityFacts -> InferState -> InferState
applyCapabilityFacts facts state =
  modifyDeclarationState
    ( \declarations ->
        declarations
          { declarationClassFacts = scopeClassFacts facts,
            declarationGeneratedEqualityClassFacts = scopeGeneratedEqualityClassFacts facts,
            declarationConcreteImplFacts = scopeConcreteImplFacts facts,
            declarationClassMethodSignatures = scopeClassMethodSignatures facts,
            declarationConcreteImplMethods = scopeConcreteImplMethods facts
          }
    )
    state

restoreCapabilityFacts :: InferState -> InferState -> InferState
restoreCapabilityFacts previousState nextState =
  modifyModuleInferenceState
    ( \moduleState ->
        moduleState
          { inferenceLocalCapabilities =
              inferCurrentModuleLocalCapabilityFacts previousState
          }
    )
    ( modifyDeclarationState
        ( \declarations ->
            declarations
              { declarationClassFacts = inferClassFacts previousState,
                declarationGeneratedEqualityClassFacts = inferGeneratedEqualityClassFacts previousState,
                declarationConcreteImplFacts = inferConcreteImplFacts previousState,
                declarationClassMethodSignatures = inferClassMethodSignatures previousState,
                declarationConcreteImplMethods = inferConcreteImplMethods previousState
              }
        )
        nextState
    )

mergeCapabilityFacts :: ScopeCapabilityFacts -> ScopeCapabilityFacts -> ScopeCapabilityFacts
mergeCapabilityFacts leftFacts rightFacts =
  ScopeCapabilityFacts
    { scopeClassFacts = Map.union (scopeClassFacts leftFacts) (scopeClassFacts rightFacts),
      scopeGeneratedEqualityClassFacts =
        Set.union
          (scopeGeneratedEqualityClassFacts leftFacts)
          (scopeGeneratedEqualityClassFacts rightFacts),
      scopeConcreteImplFacts =
        Set.union
          (scopeConcreteImplFacts leftFacts)
          (scopeConcreteImplFacts rightFacts),
      scopeClassMethodSignatures =
        Map.union
          (scopeClassMethodSignatures leftFacts)
          (scopeClassMethodSignatures rightFacts),
      scopeConcreteImplMethods =
        Map.unionWith
          (++)
          (scopeConcreteImplMethods leftFacts)
          (scopeConcreteImplMethods rightFacts)
    }

updateRootModuleBaselineFacts :: ScopeCapabilityFacts -> InferState -> InferState -> ScopeCapabilityFacts
updateRootModuleBaselineFacts moduleBaselineFacts previousState nextState =
  case inferCurrentModulePath previousState of
    Nothing -> capabilityFactsFromState nextState
    Just _ -> moduleBaselineFacts

flushCurrentModuleCapabilityFacts :: InferState -> InferState
flushCurrentModuleCapabilityFacts state =
  case inferCurrentModulePath state of
    Just modulePath ->
      modifyModuleInferenceState
        ( \moduleState ->
            moduleState
              { inferenceModuleCapabilities =
                  Map.insert
                    modulePath
                    (inferCurrentModuleLocalCapabilityFacts state)
                    (inferModuleCapabilityFacts state)
              }
        )
        state
    Nothing -> state

enterModuleCapabilityScope :: ScopeCapabilityFacts -> [Text] -> InferState -> InferState
enterModuleCapabilityScope baselineFacts modulePath state =
  modifyModuleInferenceState
    ( \moduleState ->
        moduleState
          { inferenceModulePath = Just modulePath,
            inferenceLocalCapabilities = emptyScopeCapabilityFacts
          }
    )
    (applyCapabilityFacts baselineFacts (flushCurrentModuleCapabilityFacts state))

importModuleCapabilityFacts :: [Text] -> Maybe Text -> Maybe [Text] -> InferState -> InferState
importModuleCapabilityFacts modulePath maybeAlias maybeSymbolNames state =
  applyCapabilityFacts
    ( mergeCapabilityFacts
        (capabilityFactsFromState state)
        (filterImportedCapabilityFacts maybeAlias maybeSymbolNames (Map.findWithDefault emptyScopeCapabilityFacts modulePath (inferModuleCapabilityFacts state)))
    )
    state

filterImportedCapabilityFacts :: Maybe Text -> Maybe [Text] -> ScopeCapabilityFacts -> ScopeCapabilityFacts
filterImportedCapabilityFacts maybeAlias maybeSymbolNames facts =
  case maybeAlias of
    Just _ -> emptyScopeCapabilityFacts
    Nothing ->
      case maybeSymbolNames of
        Nothing -> facts
        Just symbolNames ->
          facts
            { scopeClassFacts =
                Map.filterWithKey
                  (\className _ -> Set.member className visibleSymbols)
                  (scopeClassFacts facts),
              scopeGeneratedEqualityClassFacts =
                Set.filter (`Set.member` visibleSymbols) (scopeGeneratedEqualityClassFacts facts),
              scopeConcreteImplFacts =
                Set.filter
                  (\implKey -> Set.member (concreteImplFactClassName implKey) visibleSymbols)
                  (scopeConcreteImplFacts facts),
              scopeClassMethodSignatures =
                Map.filterWithKey
                  (\methodKey _ -> qualifiedMethodClassIsVisible methodKey)
                  (scopeClassMethodSignatures facts),
              scopeConcreteImplMethods =
                Map.filterWithKey
                  (\methodKey _ -> qualifiedMethodClassIsVisible methodKey)
                  (scopeConcreteImplMethods facts)
            }
          where
            visibleSymbols = Set.fromList symbolNames
            qualifiedMethodClassIsVisible methodKey =
              case splitQualifiedMethodKey methodKey of
                Just (className, _) -> Set.member className visibleSymbols
                Nothing -> False

seedStatementCapabilityFact :: InferState -> Statement -> InferState
seedStatementCapabilityFact state statement =
  let facts = seedFacts (capabilityFactsFromState state) (0, statement)
      stateWithVisibleFacts = applyCapabilityFacts facts state
   in case inferCurrentModulePath state of
        Just _ ->
          modifyModuleInferenceState
            ( \moduleState ->
                moduleState
                  { inferenceLocalCapabilities =
                      seedFacts (inferCurrentModuleLocalCapabilityFacts state) (0, statement)
                  }
            )
            stateWithVisibleFacts
        Nothing ->
          stateWithVisibleFacts

seedFacts :: ScopeCapabilityFacts -> (Int, Statement) -> ScopeCapabilityFacts
seedFacts facts (_, statement) =
  case statement of
    SClass _ capabilityName parameters methods ->
      seedClassMethodFacts
        capabilityName
        parameters
        methods
        facts
          { scopeClassFacts = Map.insert (identifierText capabilityName) (length parameters) (scopeClassFacts facts),
            scopeGeneratedEqualityClassFacts = scopeGeneratedEqualityClassFacts facts
          }
    SImpl _ capabilityName arguments methods ->
      seedImplMethodFacts capabilityName arguments methods $
        case concreteImplFactKey capabilityName arguments of
          Just implFactKey ->
            facts {scopeConcreteImplFacts = Set.insert implFactKey (scopeConcreteImplFacts facts)}
          Nothing ->
            facts
    _ -> facts

seedClassMethodFacts ::
  Name ->
  [Name] ->
  [ClassMethodSignature] ->
  ScopeCapabilityFacts ->
  ScopeCapabilityFacts
seedClassMethodFacts capabilityName parameters methods facts =
  case parameters of
    [classParameter] ->
      facts
        { scopeClassMethodSignatures =
            foldl'
              insertMethodSignature
              (scopeClassMethodSignatures facts)
              methods
        }
      where
        classParameterText = identifierText classParameter
        insertMethodSignature acc (ClassMethodSignature methodName _ methodSignature) =
          Map.insert
            (qualifiedMethodKey capabilityName methodName)
            (ClassMethodType classParameterText methodSignature)
            acc
    _ -> facts

seedImplMethodFacts ::
  Name ->
  [SignatureType] ->
  [ImplMethod] ->
  ScopeCapabilityFacts ->
  ScopeCapabilityFacts
seedImplMethodFacts capabilityName arguments methods facts =
  case arguments of
    [implTarget]
      | concreteConstraintArgument implTarget ->
          facts
            { scopeConcreteImplMethods =
                foldl'
                  insertImplMethod
                  (scopeConcreteImplMethods facts)
                  methods
            }
      where
        insertImplMethod acc (ImplMethod methodName _ _) =
          Map.insertWith
            (\newMethods existingMethods -> existingMethods ++ newMethods)
            (qualifiedMethodKey capabilityName methodName)
            [ImplMethodType implTarget]
            acc
    _ -> facts


modifyDeclarationState :: (DeclarationState -> DeclarationState) -> InferState -> InferState
modifyDeclarationState update state =
  state {inferDeclarations = update (inferDeclarations state)}

modifyModuleInferenceState :: (ModuleInferenceState -> ModuleInferenceState) -> InferState -> InferState
modifyModuleInferenceState update state =
  state {inferModule = update (inferModule state)}

modifyInferenceOutput :: (InferenceOutput -> InferenceOutput) -> InferState -> InferState
modifyInferenceOutput update state =
  state {inferOutput = update (inferOutput state)}

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

qualifiedMethodClassIsVisible :: Text -> InferState -> Bool
qualifiedMethodClassIsVisible methodKey state =
  case splitQualifiedMethodKey methodKey of
    Just (capabilityName, _) -> Map.member capabilityName (inferClassFacts state)
    Nothing -> False

inferQualifiedMethodApplication ::
  InferExprFn ->
  BuiltinResolutionMode ->
  TypeEnv ->
  InferState ->
  Text ->
  [Expr] ->
  (Maybe ExpressionType, InferState)
inferQualifiedMethodApplication inferExpression builtinMode env state methodKey argumentExprs =
  let (argumentTypes, stateAfterArguments) =
        inferQualifiedMethodArguments inferExpression builtinMode env state argumentExprs
   in case sequence argumentTypes of
        Nothing -> (Nothing, stateAfterArguments)
        Just typedArgumentTypes ->
          resolveQualifiedMethodApplicationType
            methodKey
            env
            stateAfterArguments
            (zip argumentExprs typedArgumentTypes)

inferQualifiedMethodArguments ::
  InferExprFn ->
  BuiltinResolutionMode ->
  TypeEnv ->
  InferState ->
  [Expr] ->
  ([Maybe ExpressionType], InferState)
inferQualifiedMethodArguments inferExpression builtinMode env state argumentExprs =
  let (reversedTypes, finalState) = foldl' step ([], state) argumentExprs
   in (reverse reversedTypes, finalState)
  where
    step (typesAcc, stateAcc) argumentExpr =
      let (argumentType, stateAfterArgument) =
            inferExpression builtinMode env stateAcc argumentExpr
       in (argumentType : typesAcc, stateAfterArgument)

checkImplMethodBodies ::
  InferExprFn ->
  BuiltinResolutionMode ->
  TypeEnv ->
  InferState ->
  Name ->
  [SignatureType] ->
  [ImplMethod] ->
  InferState
checkImplMethodBodies inferExpression builtinMode env state capabilityName arguments methods =
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
                                      inferExprTypeWithExpected inferExpression
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

    currentImplMethodBindings :: SignatureType -> InferState -> TypeEnv
    currentImplMethodBindings implTarget stateForBindings =
      Map.fromList
        [ (qualifiedMemberName capabilityName methodName, PlainTypeBinding methodType)
          | ImplMethod methodName _ _ <- methods,
            let methodKey = qualifiedMethodKey capabilityName methodName,
            Just (ClassMethodType classParameter methodSignature) <- [Map.lookup methodKey (inferClassMethodSignatures stateForBindings)],
            Just methodType <- [classMethodPayloadToExpressionType stateForBindings classParameter implTarget methodSignature]
        ]

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
      let extendedEnv =
            Map.insert parameterName (PlainTypeBinding argumentType) env
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

uniqueExactRuntimeCandidateHint :: InferState -> ExpressionType -> [SignatureType] -> Bool
uniqueExactRuntimeCandidateHint state argumentType candidateHints =
  case
      [ candidateHint
        | candidateHint <- candidateHints,
          constraintSignatureTypeExactlyMatchesExpressionType state candidateHint argumentType
      ] of
    [candidateHint] ->
      not (constraintSignatureTypeContainsList candidateHint)
    _ -> False

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
    TCharType -> Set.empty
    TTextType -> Set.empty
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
    TCharType -> TCharType
    TTextType -> TTextType
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
                          constraintName <> "(" <> renderCapabilityType (head argumentHints) <> ")"
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
  [SignatureType]
constraintRuntimeHintsForDeferred facts state inferredConstraint _ maybeMethodKey argumentType
  | inferredConstraint =
      inferredConstraintCandidateRuntimeHints facts state maybeMethodKey argumentType
  | otherwise =
      case expressionTypeToRuntimeHint (defaultLiteralTypes argumentType) of
        Just argumentHint -> [argumentHint]
        Nothing -> []

constraintImplFactExistsForDeferred :: ScopeCapabilityFacts -> Bool -> Text -> SignatureType -> Bool
constraintImplFactExistsForDeferred facts inferredConstraint constraintName argumentHint =
  if inferredConstraint
    then concreteImplFactExists constraintName argumentHint facts
    else Set.member implFactKey (scopeConcreteImplFacts facts)
  where
    implFactKey = constraintName <> "(" <> renderCapabilityType argumentHint <> ")"

inferredConstraintCandidateRuntimeHints :: ScopeCapabilityFacts -> InferState -> Maybe Text -> ExpressionType -> [SignatureType]
inferredConstraintCandidateRuntimeHints facts state maybeMethodKey argumentType =
  dedupeSignatureTypes (defaultHint ++ methodCandidateHints)
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

dedupeSignatureTypes :: [SignatureType] -> [SignatureType]
dedupeSignatureTypes =
  go Set.empty
  where
    go _ [] = []
    go seen (signatureType : rest)
      | Set.member rendered seen = go seen rest
      | otherwise = signatureType : go (Set.insert rendered seen) rest
      where
        rendered = renderCapabilityType signatureType

constraintSignatureTypeMatchesExpressionType :: InferState -> SignatureType -> ExpressionType -> Bool
constraintSignatureTypeMatchesExpressionType state signatureType expressionType =
  case (signatureType, resolveType state expressionType) of
    (TypeInt, TIntegerLiteralType literalRange) ->
      integerLiteralRangeFitsNumericType literalRange NumericInt64
    (TypeNumeric numericType, TIntegerLiteralType literalRange) ->
      numericTypeIsIntegral numericType
        && integerLiteralRangeFitsNumericType literalRange numericType
    (TypeName signatureName, TIntegerLiteralType literalRange) ->
      case numericTypeFromConstraintSignatureName (identifierText signatureName) of
        Just numericType ->
          numericTypeIsIntegral numericType
            && integerLiteralRangeFitsNumericType literalRange numericType
        Nothing ->
          False
    (TypeList signatureElementType, TListType elementType) ->
      constraintSignatureTypeMatchesExpressionType state signatureElementType elementType
    (TypeTuple signatureElementTypes, TTupleType elementTypes)
      | length signatureElementTypes == length elementTypes ->
          and (zipWith (constraintSignatureTypeMatchesExpressionType state) signatureElementTypes elementTypes)
    (TypeApplication signatureName signatureArguments, TDataType typeName typeArguments)
      | normalizeConstraintSignatureName (identifierText signatureName)
          == normalizeConstraintSignatureName (identifierText typeName),
        length signatureArguments == length typeArguments ->
          and (zipWith (constraintSignatureTypeMatchesExpressionType state) signatureArguments typeArguments)
    (TypeFunction signatureArgument signatureResult, TFunctionType argumentType resultType) ->
      constraintSignatureTypeMatchesExpressionType state signatureArgument argumentType
        && constraintSignatureTypeMatchesExpressionType state signatureResult resultType
    _ ->
      case expressionTypeToRuntimeHint (defaultLiteralTypes (resolveType state expressionType)) of
        Just argumentHint -> constraintSignatureTypesCompatible signatureType argumentHint
        Nothing -> False

numericTypeFromConstraintSignatureName :: Text -> Maybe NumericType
numericTypeFromConstraintSignatureName =
  numericTypeFromName . normalizeConstraintSignatureName

concreteImplFactExists :: Text -> SignatureType -> ScopeCapabilityFacts -> Bool
concreteImplFactExists constraintName argumentHint facts =
  any
    (\candidateHint -> Set.member (constraintName <> "(" <> renderCapabilityType candidateHint <> ")") (scopeConcreteImplFacts facts))
    (constraintSignatureAliasVariants argumentHint)

concreteImplMethodBodyExists :: Text -> SignatureType -> ScopeCapabilityFacts -> Bool
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

constraintSignatureExpressionHasExactEvidence :: TypeEnv -> SignatureType -> Expr -> Bool
constraintSignatureExpressionHasExactEvidence env signatureType argumentExpr =
  case (signatureType, argumentExpr) of
    (TypeList elementType, EList elements) ->
      not (null elements)
        && all (constraintSignatureExpressionHasExactEvidence env elementType) elements
    (TypeTuple elementTypes, ETuple elements)
      | length elementTypes == length elements ->
          and (zipWith (constraintSignatureExpressionHasExactEvidence env) elementTypes elements)
    (TypeApplication typeName typeArguments, EApply {}) ->
      constructorApplicationExpressionHasExactEvidence env typeName typeArguments argumentExpr
        || constraintSignatureExpressionRuntimeHintMatches env signatureType argumentExpr
    (TypeFunction {}, _) ->
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

constraintSignatureExpressionRuntimeHintMatches :: TypeEnv -> SignatureType -> Expr -> Bool
constraintSignatureExpressionRuntimeHintMatches env signatureType argumentExpr =
  case constraintSignatureExpressionRuntimeHint env argumentExpr of
    Just runtimeHint -> runtimeHint == signatureType
    Nothing -> False

constraintSignatureExpressionRuntimeHint :: TypeEnv -> Expr -> Maybe SignatureType
constraintSignatureExpressionRuntimeHint env argumentExpr =
  constraintSignatureExpressionRuntimeHintWithLocalHints env Map.empty argumentExpr

constraintSignatureExpressionRuntimeHintWithLocalHints ::
  TypeEnv ->
  Map Text SignatureType ->
  Expr ->
  Maybe SignatureType
constraintSignatureExpressionRuntimeHintWithLocalHints env localHints argumentExpr =
  case argumentExpr of
    EVar referencedName ->
      Map.lookup (identifierText referencedName) localHints
        <|> (Map.lookup referencedName env >>= typeBindingRuntimeHint)
    EApply (EApply dollarExpr functionExpr) _
      | builtinDollarOperatorExpr env dollarExpr ->
          case constraintSignatureExpressionRuntimeHintWithLocalHints env localHints functionExpr of
            Just (TypeFunction _ resultType) -> Just resultType
            _ -> Nothing
    EApply functionExpr _ ->
      case constraintSignatureExpressionRuntimeHintWithLocalHints env localHints functionExpr of
        Just (TypeFunction _ resultType) -> Just resultType
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
  Map Text SignatureType ->
  [Expr] ->
  Maybe SignatureType
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
  Map Text SignatureType ->
  [Statement] ->
  Maybe SignatureType
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

signaturePayloadRuntimeHint :: SignaturePayload -> Maybe SignatureType
signaturePayloadRuntimeHint signaturePayload =
  case signaturePayload of
    SignatureType signatureType
      | Set.null (constraintSignatureTypeVariableNames signatureType) -> Just signatureType
    SignatureType {} -> Nothing
    ConstrainedSignature _ signatureType
      | Set.null (constraintSignatureTypeVariableNames signatureType) ->
          Just signatureType
    ConstrainedSignature _ signatureType ->
      constraintSignatureTypeToExpressionType signatureType >>= expressionTypeToRuntimeHint
    UnsupportedSignature {} ->
      Nothing

typeBindingRuntimeHint :: TypeBinding -> Maybe SignatureType
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

constraintSignatureTypeContainsList :: SignatureType -> Bool
constraintSignatureTypeContainsList signatureType =
  case signatureType of
    TypeList {} -> True
    TypeTuple elementTypes ->
      any constraintSignatureTypeContainsList elementTypes
    TypeApplication _ typeArguments ->
      any constraintSignatureTypeContainsList typeArguments
    TypeFunction argumentType resultType ->
      constraintSignatureTypeContainsList argumentType
        || constraintSignatureTypeContainsList resultType
    TypeName {} -> False
    _ -> False

constructorApplicationExpressionHasExactEvidence :: TypeEnv -> Name -> [SignatureType] -> Expr -> Bool
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

constructorArgumentExpressionHasExactEvidence :: TypeEnv -> Map Text SignatureType -> ConstructorArgumentType -> Expr -> Bool
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

constraintSignatureTypeExactlyMatchesExpressionType :: InferState -> SignatureType -> ExpressionType -> Bool
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
  SignatureType ->
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
  SignatureType ->
  Maybe ExpressionType
constraintSignatureTypeToExpressionTypeWithState state signatureVariables signatureType =
  case signatureType of
    TypeInt -> Just TIntType
    TypeFloat -> Just TFloatType
    TypeNumeric numericType -> Just (TNumericType numericType)
    TypeBool -> Just TBoolType
    TypeChar -> Just TCharType
    TypeText -> Just TTextType
    TypeVariable name -> Map.lookup (identifierText name) signatureVariables
    TypeName name ->
      case identifierText name of
        "Int" -> Just TIntType
        "Float" -> Just TFloatType
        "Bool" -> Just TBoolType
        "Char" -> Just TCharType
        "Text" -> Just TTextType
        typeName ->
          case numericTypeNameToExpressionType typeName of
            Just numericType -> Just numericType
            Nothing ->
              case Map.lookup typeName signatureVariables of
                Just variableType -> Just variableType
                Nothing ->
                  case Map.lookup typeName (inferDataTypes state) of
                    Just (DataTypeBinding [] _) -> Just (TDataType name [])
                    _ -> Nothing
    TypeApplication name arguments ->
      case Map.lookup (identifierText name) (inferDataTypes state) of
        Just (DataTypeBinding parameters _)
          | length parameters == length arguments ->
              TDataType name <$> traverse (constraintSignatureTypeToExpressionTypeWithState state signatureVariables) arguments
        _ -> Nothing
    TypeList innerType ->
      TListType <$> constraintSignatureTypeToExpressionTypeWithState state signatureVariables innerType
    TypeTuple elementTypes ->
      TTupleType <$> traverse (constraintSignatureTypeToExpressionTypeWithState state signatureVariables) elementTypes
    TypeFunction argumentType resultType ->
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
      signaturePayloadFromType [] signatureType state
    ConstrainedSignature [] signatureType ->
      signaturePayloadFromType [] signatureType state
    ConstrainedSignature constraints signatureType
      | supportedVariableConstraints state constraints signatureType ->
          variableConstraintSignaturePayloadToExpressionType constraints signatureType state
      | supportedConcreteConstraints state constraints ->
          signaturePayloadFromType [] signatureType state
      | otherwise ->
          (Nothing, state)
    UnsupportedSignature {} ->
      (Nothing, state)

signaturePayloadFromType ::
  [TypeSchemeConstraint] ->
  SignatureType ->
  InferState ->
  (Maybe SignaturePayloadType, InferState)
signaturePayloadFromType explicitConstraints signatureType state =
  let variableNames = constraintSignatureTypeVariableNamesInOrder signatureType
      (signatureVariables, nextState) = allocateSignatureTypeVariables variableNames state
      variableOrder =
        [ typeVar
          | variableName <- variableNames,
            Just (TVarType typeVar) <- [Map.lookup variableName signatureVariables]
        ]
   in case constraintSignatureTypeToExpressionTypeWithState nextState signatureVariables signatureType of
        Just expressionType ->
          (Just (SignaturePayloadType expressionType explicitConstraints variableOrder), nextState)
        Nothing -> (Nothing, state)

constraintSignatureTypeToExpressionType :: SignatureType -> Maybe ExpressionType
constraintSignatureTypeToExpressionType =
  constraintSignatureTypeToExpressionTypeWithVariables Map.empty

constraintSignatureTypeToExpressionTypeWithVariables ::
  Map Text ExpressionType ->
  SignatureType ->
  Maybe ExpressionType
constraintSignatureTypeToExpressionTypeWithVariables signatureVariables signatureType =
  case signatureType of
    TypeInt -> Just TIntType
    TypeFloat -> Just TFloatType
    TypeNumeric numericType -> Just (TNumericType numericType)
    TypeBool -> Just TBoolType
    TypeChar -> Just TCharType
    TypeText -> Just TTextType
    TypeVariable name -> Map.lookup (identifierText name) signatureVariables
    TypeName name ->
      case identifierText name of
        "Int" -> Just TIntType
        "Float" -> Just TFloatType
        "Bool" -> Just TBoolType
        "Char" -> Just TCharType
        "Text" -> Just TTextType
        typeName ->
          case numericTypeNameToExpressionType typeName of
            Just numericType -> Just numericType
            Nothing -> Map.lookup typeName signatureVariables
    TypeApplication {} ->
      Nothing
    TypeList innerType ->
      TListType <$> constraintSignatureTypeToExpressionTypeWithVariables signatureVariables innerType
    TypeTuple elementTypes ->
      TTupleType <$> traverse (constraintSignatureTypeToExpressionTypeWithVariables signatureVariables) elementTypes
    TypeFunction argumentType resultType ->
      TFunctionType
        <$> constraintSignatureTypeToExpressionTypeWithVariables signatureVariables argumentType
        <*> constraintSignatureTypeToExpressionTypeWithVariables signatureVariables resultType

variableConstraintSignaturePayloadToExpressionType ::
  [SignatureConstraint] ->
  SignatureType ->
  InferState ->
  (Maybe SignaturePayloadType, InferState)
variableConstraintSignaturePayloadToExpressionType constraints signatureType state =
  let variableNames = constraintSignatureTypeVariableNamesInOrder signatureType
      (signatureVariables, nextState) = allocateSignatureTypeVariables variableNames state
      convertedType =
        constraintSignatureTypeToExpressionTypeWithState nextState signatureVariables signatureType
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
    [TypeVariable argumentName] ->
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
supportedVariableConstraints :: InferState -> [SignatureConstraint] -> SignatureType -> Bool
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
    (Just 1, [TypeVariable {}]) -> True
    _ -> False

constraintVariableNamesInSupportedConstraint :: SignatureConstraint -> Set Text
constraintVariableNamesInSupportedConstraint constraint =
  case constraint of
    SignatureConstraint _ [TypeVariable argumentName] ->
      Set.singleton (identifierText argumentName)
    _ -> Set.empty

constraintSignatureTypeVariableNames :: SignatureType -> Set Text
constraintSignatureTypeVariableNames signatureType =
  case signatureType of
    TypeVariable name -> Set.singleton (identifierText name)
    TypeName name
      | identifierLooksLikeTypeVariable name ->
          Set.singleton (identifierText name)
      | otherwise ->
          Set.empty
    TypeApplication _ arguments ->
      Set.unions (map constraintSignatureTypeVariableNames arguments)
    TypeList innerType ->
      constraintSignatureTypeVariableNames innerType
    TypeTuple elementTypes ->
      Set.unions (map constraintSignatureTypeVariableNames elementTypes)
    TypeFunction argumentType resultType ->
      Set.union
        (constraintSignatureTypeVariableNames argumentType)
        (constraintSignatureTypeVariableNames resultType)
    _ -> Set.empty

constraintSignatureTypeSupportsVariableBody :: SignatureType -> Bool
constraintSignatureTypeSupportsVariableBody signatureType =
  case signatureType of
    TypeVariable {} -> True
    TypeName {} -> True
    TypeApplication _ arguments -> all constraintSignatureTypeSupportsVariableBody arguments
    TypeList innerType ->
      constraintSignatureTypeSupportsVariableBody innerType
    TypeTuple elementTypes ->
      all constraintSignatureTypeSupportsVariableBody elementTypes
    TypeFunction argumentType resultType ->
      constraintSignatureTypeSupportsVariableBody argumentType
        && constraintSignatureTypeSupportsVariableBody resultType
    _ -> True

numericTypeNameToExpressionType :: Text -> Maybe ExpressionType
numericTypeNameToExpressionType typeName =
  TNumericType <$> numericTypeFromName typeName

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

expressionTypeToRuntimeHint :: ExpressionType -> Maybe SignatureType
expressionTypeToRuntimeHint expressionType =
  case expressionType of
    TIntType -> Just TypeInt
    TIntegerLiteralType literalRange
      | integerLiteralRangeFitsNumericType literalRange NumericInt64 ->
          Just TypeInt
      | otherwise -> Nothing
    TFloatType -> Just TypeFloat
    TNumericType numericType -> Just (TypeNumeric numericType)
    TBoolType -> Just TypeBool
    TCharType -> Just TypeChar
    TTextType -> Just TypeText
    TListType elementType ->
      TypeList <$> expressionTypeToRuntimeHint elementType
    TTupleType elementTypes ->
      TypeTuple <$> traverse expressionTypeToRuntimeHint elementTypes
    TDataType typeName typeArguments ->
      case traverse expressionTypeToRuntimeHint typeArguments of
        Just [] -> Just (TypeName typeName)
        Just argumentHints ->
          Just (TypeApplication typeName argumentHints)
        Nothing -> Nothing
    TFunctionType inputType outputType ->
      TypeFunction
        <$> expressionTypeToRuntimeHint inputType
        <*> expressionTypeToRuntimeHint outputType
    TVarType {} -> Nothing

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
  if classFactIsUnary "Eq"
    then Just "Eq"
    else
      case filter importedEqualityClass (Map.toList (inferClassFacts state)) of
        [(className, _)] -> Just className
        _ -> Nothing
  where
    classFactIsUnary className =
      Map.lookup className (inferClassFacts state) == Just 1
    importedEqualityClass (className, arity) =
      arity == 1 && "::Eq" `Text.isSuffixOf` className
