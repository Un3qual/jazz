{-# LANGUAGE DataKinds #-}

-- | Use-site instantiation of ordinary schemes and explicit type applications.
module Jazz.Compiler.TypeInference.Instantiation
  ( instantiateNonBuiltinTypeBinding,
    inferExplicitTypeApplication,
    typeBindingScheme,
  )
where

import Data.Bifunctor (first)
import Data.List.NonEmpty
  ( NonEmpty (..),
  )
import qualified Data.Map.Strict as Map
import Jazz.Compiler.AST
  ( CoreNode (coreNodeFacts, coreNodeId, coreNodeSpan),
    CoreNodeId,
    CorePhase (..),
    Expr (..),
    SignatureType,
    expressionNode,
  )
import Jazz.Compiler.CoreIdentity (CapabilityMethodKey, capabilityMethodKeyFromReference, resolvedValueReference)
import Jazz.Compiler.Diagnostics
  ( SourceSpan,
  )
import Jazz.Compiler.Name
  ( ResolvedName,
    operatorBindingName,
  )
import Jazz.Compiler.TypeInference.Capabilities
  ( applyTypeSchemePrimitiveConstraints,
    capabilityFactsFromState,
    deferExplicitConstraintsWithFacts,
    instantiateQualifiedMethodTypeWithExplicitTarget,
    qualifiedMethodClassIsVisible,
  )
import Jazz.Compiler.TypeInference.Diagnostics
  ( addTypeError,
    annotateNewErrorsWithPrimarySpan,
    mkExplicitTypeApplicationTargetError,
    mkInvalidExplicitTypeApplicationArgumentError,
  )
import Jazz.Compiler.TypeInference.Draft (checkedExprType)
import Jazz.Compiler.TypeInference.Pattern
  ( instantiateConstructorBinding,
  )
import qualified Jazz.Compiler.TypeInference.Signature as Signature
import Jazz.Compiler.TypeInference.Solver
  ( freshTypeVar,
    resolveType,
  )
import Jazz.Compiler.TypeInference.State
  ( ExplicitInstantiationSeed (..),
    ExplicitInstantiationTarget (..),
    InferState (..),
    recordExplicitInstantiationSeed,
    recordExpressionFactType,
  )
import Jazz.Compiler.TypeInference.Traversal
  ( InferExprFn,
  )
import Jazz.Compiler.TypeInference.TypeOps
  ( instantiateTypeSchemeConstraint,
    instantiateTypeSchemePrimitiveConstraint,
    replaceTypeVariables,
  )
import Jazz.Compiler.TypeInference.Types
  ( ExpressionType,
    SemanticBinding (..),
    SemanticScheme (..),
    SemanticType (..),
    TypeBinding,
    TypeEnv,
    TypeScheme,
    quantifiedVariablesOrderedList,
    typeEnvReferenceKey,
  )

typeBindingScheme :: TypeBinding -> Maybe TypeScheme
typeBindingScheme binding =
  case binding of
    SchemeTypeBinding typeScheme -> Just typeScheme
    OperatorAliasSchemeTypeBinding _ typeScheme -> Just typeScheme
    _ -> Nothing

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
              (foldr SemanticFunction constructorResultType constructorArgumentTypes'),
            nextState
          )
        Nothing -> (Nothing, state)

instantiateTypeScheme :: TypeScheme -> InferState -> (Maybe ExpressionType, InferState)
instantiateTypeScheme typeScheme state =
  let (freshBindings, nextState) =
        foldl'
          allocateFreshBinding
          (Map.empty, state)
          (quantifiedVariablesOrderedList (schemeQuantifiedVariables typeScheme))
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
          (definingFacts <> capabilityFactsFromState state)
          definingFacts
          instantiatedConstraints
          stateWithPrimitiveConstraints
   in (Just (resolveType stateWithDeferredConstraints instantiatedType), stateWithDeferredConstraints)
  where
    explicitConstraints = schemeClassConstraints typeScheme
    primitiveConstraints = schemePrimitiveConstraints typeScheme
    definingFacts = schemeDefiningCapabilities typeScheme
    expressionType = schemeResultType typeScheme

    allocateFreshBinding (bindings, stateAcc) typeVar =
      let (freshType, nextState) = freshTypeVar stateAcc
       in (Map.insert typeVar freshType bindings, nextState)

inferExplicitTypeApplication ::
  InferExprFn ->
  TypeEnv ->
  InferState ->
  CoreNodeId ->
  Expr 'Resolved ->
  SourceSpan ->
  SignatureType 'Resolved ->
  (Maybe ExpressionType, InferState)
inferExplicitTypeApplication inferExpression env state applicationNodeId functionExpr typeArgumentSpan typeArgument =
  case (explicitTypeApplicationScheme env functionExpr, Signature.constraintSignatureTypeToExpressionTypeWithState state Map.empty typeArgument) of
    (_, Just explicitArgumentType)
      | Just methodKey <- explicitQualifiedMethodTypeApplicationKey env state functionExpr,
        Just targetName <- explicitTypeApplicationTargetName functionExpr ->
          let (maybeInstantiatedType, nextState) =
                instantiateQualifiedMethodTypeWithExplicitTarget applicationNodeId methodKey explicitArgumentType state
           in ( maybeInstantiatedType,
                recordExplicitInstantiationDecision
                  applicationNodeId
                  (ExplicitQualifiedMethodInstantiation targetName)
                  explicitArgumentType
                  maybeInstantiatedType
                  ( recordExplicitFunctionFact
                      functionExpr
                      maybeInstantiatedType
                      (annotateNewErrorsWithPrimarySpan (coreNodeSpan (expressionNode functionExpr)) state nextState)
                  )
              )
    (Just typeScheme, Just explicitArgumentType)
      | Just targetName <- explicitTypeApplicationTargetName functionExpr ->
          let (maybeInstantiatedType, nextState) =
                instantiateTypeSchemeWithExplicitArgument typeScheme explicitArgumentType state
           in ( maybeInstantiatedType,
                recordExplicitInstantiationDecision
                  applicationNodeId
                  (ExplicitBinderInstantiation targetName)
                  explicitArgumentType
                  maybeInstantiatedType
                  (recordExplicitFunctionFact functionExpr maybeInstantiatedType nextState)
              )
    (Just _, Just _) ->
      (Nothing, addTypeError state mkExplicitTypeApplicationTargetError)
    (Just _, Nothing) ->
      (Nothing, addTypeError state (mkInvalidExplicitTypeApplicationArgumentError state typeArgumentSpan typeArgument))
    (Nothing, _) ->
      let (functionResult, stateAfterFunction) =
            first checkedExprType (inferExpression env state functionExpr)
       in case functionResult of
            Just _ ->
              (Nothing, addTypeError stateAfterFunction mkExplicitTypeApplicationTargetError)
            Nothing -> (Nothing, stateAfterFunction)

recordExplicitFunctionFact :: Expr 'Resolved -> Maybe ExpressionType -> InferState -> InferState
recordExplicitFunctionFact functionExpr maybeExpressionType state =
  case (functionExpr, maybeExpressionType) of
    (EVar node _, Just expressionType) ->
      recordExpressionFactType (coreNodeId node) expressionType state
    (EOperatorValue node _, Just expressionType) ->
      recordExpressionFactType (coreNodeId node) expressionType state
    _ -> state

recordExplicitInstantiationDecision ::
  CoreNodeId ->
  ExplicitInstantiationTarget ->
  ExpressionType ->
  Maybe ExpressionType ->
  InferState ->
  InferState
recordExplicitInstantiationDecision applicationNodeId target argumentType maybeInstantiatedType state =
  case maybeInstantiatedType of
    Nothing -> state
    Just _ ->
      recordExplicitInstantiationSeed
        applicationNodeId
        ( ExplicitInstantiationSeed
            { explicitInstantiationSeedTarget = target,
              explicitInstantiationSeedArguments = argumentType :| []
            }
        )
        state

explicitTypeApplicationTargetName :: Expr 'Resolved -> Maybe ResolvedName
explicitTypeApplicationTargetName functionExpr =
  case functionExpr of
    EVar _ name -> Just name
    EOperatorValue _ operatorSymbol -> Just (operatorBindingName operatorSymbol)
    _ -> Nothing

explicitQualifiedMethodTypeApplicationKey :: TypeEnv -> InferState -> Expr 'Resolved -> Maybe CapabilityMethodKey
explicitQualifiedMethodTypeApplicationKey env state functionExpr =
  case functionExpr of
    EVar node name
      | Map.notMember (typeEnvReferenceKey (coreNodeFacts node) name) env,
        Just methodKey <- capabilityMethodKeyFromReference (resolvedValueReference (coreNodeFacts node)),
        qualifiedMethodClassIsVisible methodKey state ->
          Just methodKey
    _ -> Nothing

explicitTypeApplicationScheme :: TypeEnv -> Expr 'Resolved -> Maybe TypeScheme
explicitTypeApplicationScheme env functionExpr =
  case functionExpr of
    EVar node name ->
      Map.lookup (typeEnvReferenceKey (coreNodeFacts node) name) env >>= typeBindingScheme
    EOperatorValue node operatorSymbol ->
      Map.lookup (typeEnvReferenceKey (coreNodeFacts node) (operatorBindingName operatorSymbol)) env >>= typeBindingScheme
    _ -> Nothing

instantiateTypeSchemeWithExplicitArgument ::
  TypeScheme ->
  ExpressionType ->
  InferState ->
  (Maybe ExpressionType, InferState)
instantiateTypeSchemeWithExplicitArgument typeScheme explicitArgumentType state =
  case quantifiedVariablesOrderedList (schemeQuantifiedVariables typeScheme) of
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
              (definingFacts <> capabilityFactsFromState state)
              definingFacts
              instantiatedConstraints
              stateWithPrimitiveConstraints
       in (Just (resolveType stateWithDeferredConstraints instantiatedType), stateWithDeferredConstraints)
  where
    explicitConstraints = schemeClassConstraints typeScheme
    primitiveConstraints = schemePrimitiveConstraints typeScheme
    definingFacts = schemeDefiningCapabilities typeScheme
    expressionType = schemeResultType typeScheme

    allocateFreshBinding (bindings, stateAcc) typeVar =
      let (freshType, nextState) = freshTypeVar stateAcc
       in (Map.insert typeVar freshType bindings, nextState)
