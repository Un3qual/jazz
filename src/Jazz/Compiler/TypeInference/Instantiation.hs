{-# LANGUAGE DataKinds #-}

-- | Use-site instantiation of ordinary schemes and explicit type applications.
module Jazz.Compiler.TypeInference.Instantiation
  ( instantiateNonBuiltinTypeBinding,
    inferExplicitTypeApplication,
    typeBindingScheme,
  )
where

import Data.List.NonEmpty
  ( NonEmpty (..),
  )
import qualified Data.Map.Strict as Map
import Jazz.Compiler.AST
  ( CoreNode (coreNodeFacts, coreNodeId, coreNodeSpan),
    CorePhase (..),
    Expr (..),
    expressionNode,
  )
import Jazz.Compiler.CoreIdentity (CapabilityMethodKey, capabilityMethodKeyFromReference, resolvedValueReference)
import Jazz.Compiler.Name
  ( ResolvedName,
  )
import Jazz.Compiler.SemanticFacts (SemanticFactInvariantFailure (MissingExpressionFacts))
import Jazz.Compiler.TypeInference.Analyzed (ExpressionDecision (..), draftDecidedExpressionNode, draftExpressionNode, noExpressionDecision)
import Jazz.Compiler.TypeInference.Capabilities
  ( MethodSelection (..),
    applyTypeSchemePrimitiveConstraints,
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
import Jazz.Compiler.TypeInference.Draft (CheckedExpr (..), rejectedDraft)
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

inferExplicitTypeApplication :: InferExprFn -> TypeEnv -> InferState -> Expr 'Resolved -> (CheckedExpr, InferState)
inferExplicitTypeApplication inferExpression env state expression@(ETypeApplication node functionExpr typeArgumentSpan typeArgument) =
  case (explicitTypeApplicationScheme env functionExpr, Signature.constraintSignatureTypeToExpressionTypeWithState state Map.empty typeArgument) of
    (_, Just explicitArgumentType)
      | Just methodKey <- explicitQualifiedMethodTypeApplicationKey env state functionExpr,
        Just targetName <- explicitTypeApplicationTargetName functionExpr ->
          let (selection, next) = instantiateQualifiedMethodTypeWithExplicitTarget methodKey explicitArgumentType state
           in finish (ExplicitQualifiedMethodInstantiation targetName) explicitArgumentType (selectedMethodType selection) (selectedMethodEvidence selection) (annotateNewErrorsWithPrimarySpan (coreNodeSpan (expressionNode functionExpr)) state next)
    (Just scheme, Just explicitArgumentType)
      | Just targetName <- explicitTypeApplicationTargetName functionExpr ->
          let (result, next) = instantiateTypeSchemeWithExplicitArgument scheme explicitArgumentType state
           in finish (ExplicitBinderInstantiation targetName) explicitArgumentType result Nothing next
    (Just _, Just _) -> failed (addTypeError state mkExplicitTypeApplicationTargetError)
    (Just _, Nothing) -> failed (addTypeError state (mkInvalidExplicitTypeApplicationArgumentError state typeArgumentSpan typeArgument))
    (Nothing, _) ->
      let (functionCheck, next) = inferExpression env state functionExpr
       in failed (case checkedExprType functionCheck of Just _ -> addTypeError next mkExplicitTypeApplicationTargetError; Nothing -> next)
  where
    failed next = (CheckedExpr Nothing (rejectedDraft (MissingExpressionFacts (coreNodeId node))), next)
    finish target argument result evidence next =
      let seed = ExplicitInstantiationSeed target (argument :| []) <$ result
          decision = noExpressionDecision {decisionInstantiation = seed, decisionEvidence = evidence}
          function = case functionExpr of
            EVar _ name -> EVar <$> draftExpressionNode result functionExpr <*> pure name
            _ -> rejectedDraft (MissingExpressionFacts (coreNodeId (expressionNode functionExpr)))
          tree = ETypeApplication <$> draftDecidedExpressionNode decision result expression <*> function <*> pure typeArgumentSpan <*> pure typeArgument
       in (CheckedExpr result tree, next)
inferExplicitTypeApplication _ _ state expression = (CheckedExpr Nothing (rejectedDraft (MissingExpressionFacts (coreNodeId (expressionNode expression)))), addTypeError state mkExplicitTypeApplicationTargetError)

explicitTypeApplicationTargetName :: Expr 'Resolved -> Maybe ResolvedName
explicitTypeApplicationTargetName functionExpr =
  case functionExpr of
    EVar _ name -> Just name
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
