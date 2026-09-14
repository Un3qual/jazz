{-# LANGUAGE DataKinds #-}

-- | Use-site instantiation of ordinary schemes and explicit type applications.
module Jazz.Compiler.TypeInference.Instantiation
  ( instantiateNonBuiltinTypeBinding,
    inferExplicitTypeApplication,
    instantiateTypeScheme,
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
import Jazz.Compiler.SemanticDeclarations (ClassDefinition (..), normalizeSignatureTypeAt, signatureVariableKindsAt)
import Jazz.Compiler.SemanticFacts (SemanticFactInvariantFailure (MissingExpressionFacts))
import Jazz.Compiler.TypeInference.Analyzed (ExpressionDecision (..), draftDecidedExpressionNode, draftExpressionNode, noExpressionDecision)
import Jazz.Compiler.TypeInference.Capabilities
  ( MethodSelection (..),
    addInferredConstraint,
    applyTypeSchemePrimitiveConstraints,
    capabilityFactsFromState,
    deferExplicitConstraintsWithFacts,
    instantiateQualifiedMethodTypeWithExplicitTarget,
    newConstraintEvidence,
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
import Jazz.Compiler.TypeInference.Solver
  ( freshTypeVars,
    resolveType,
  )
import Jazz.Compiler.TypeInference.State
  ( ExplicitInstantiationSeed (..),
    ExplicitInstantiationTarget (..),
    InferState (..),
    inferClassFacts,
    inferDataTypes,
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
    InferenceVariable,
    SchemeConstraint (..),
    SemanticBinding (..),
    SemanticScheme (..),
    SemanticType (..),
    TypeBinding,
    TypeEnv,
    TypeScheme,
    quantifiedVariablesOrderedList,
    typeEnvReferenceKey,
  )
import Jazz.Compiler.TypeRepresentation (Kind (..))

typeBindingScheme :: TypeBinding -> Maybe TypeScheme
typeBindingScheme binding =
  case binding of
    SchemeTypeBinding typeScheme -> Just typeScheme
    _ -> Nothing

-- | Instantiate non-builtin local bindings and constructors at use sites.
-- Kernel aliases use the primitive schemes in the top-level dispatcher.
instantiateNonBuiltinTypeBinding :: TypeBinding -> InferState -> (Maybe ExpressionType, InferState)
instantiateNonBuiltinTypeBinding binding state =
  case binding of
    PlainTypeBinding expressionType ->
      (Just (resolveType state expressionType), state)
    SchemeTypeBinding typeScheme ->
      instantiateTypeScheme typeScheme state
    BuiltinAliasTypeBinding {} -> (Nothing, state)
    ConstructorTypeBinding {} ->
      case instantiateConstructorBinding binding state of
        Just (constructorArgumentTypes', constructorResultType, nextState) ->
          ( Just
              (foldr SemanticFunction constructorResultType constructorArgumentTypes'),
            nextState
          )
        Nothing -> (Nothing, state)

instantiateTypeScheme :: TypeScheme -> InferState -> (Maybe ExpressionType, InferState)
instantiateTypeScheme typeScheme =
  instantiateTypeSchemeWithBindings
    typeScheme
    Map.empty
    (quantifiedVariablesOrderedList (schemeQuantifiedVariables typeScheme))

instantiateTypeSchemeWithBindings ::
  TypeScheme ->
  Map.Map InferenceVariable ExpressionType ->
  [InferenceVariable] ->
  InferState ->
  (Maybe ExpressionType, InferState)
instantiateTypeSchemeWithBindings typeScheme initialBindings remainingVariables state =
  let (freshTypes, nextState) = freshTypeVars (length remainingVariables) state
      freshBindings = Map.union (Map.fromList (zip remainingVariables freshTypes)) initialBindings
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
          instantiatedConstraints
          (foldl' (flip addInferredConstraint) stateWithPrimitiveConstraints instantiatedConstraints)
   in (Just (resolveType stateWithDeferredConstraints instantiatedType), stateWithDeferredConstraints)
  where
    explicitConstraints = schemeClassConstraints typeScheme
    primitiveConstraints = schemePrimitiveConstraints typeScheme
    definingFacts = schemeDefiningCapabilities typeScheme
    expressionType = schemeResultType typeScheme

inferExplicitTypeApplication :: InferExprFn -> TypeEnv -> InferState -> Expr 'Resolved -> (CheckedExpr, InferState)
inferExplicitTypeApplication inferExpression env state expression@(ETypeApplication node functionExpr typeArgumentSpan typeArgument) =
  case (explicitTypeApplicationScheme env functionExpr, checkedTypeArgument) of
    (_, Just explicitArgumentType)
      | Just methodKey <- explicitQualifiedMethodTypeApplicationKey state functionExpr,
        Just targetName <- explicitTypeApplicationTargetName functionExpr ->
          let (selection, next) = instantiateQualifiedMethodTypeWithExplicitTarget instantiateTypeScheme methodKey explicitArgumentType state
           in finish (ExplicitQualifiedMethodInstantiation targetName) explicitArgumentType (selectedMethodType selection) (selectedMethodEvidence selection) (annotateNewErrorsWithPrimarySpan (coreNodeSpan (expressionNode functionExpr)) state next)
    (Just scheme, Just explicitArgumentType)
      | Just targetName <- explicitTypeApplicationTargetName functionExpr ->
          let (result, next) = instantiateTypeSchemeWithExplicitArgument scheme explicitArgumentType state
           in finish (ExplicitBinderInstantiation targetName) explicitArgumentType result (newConstraintEvidence state next) next
    (Just _, Just _) -> failed (addTypeError state mkExplicitTypeApplicationTargetError)
    (_, Nothing) -> failed (addTypeError state (mkInvalidExplicitTypeApplicationArgumentError state typeArgumentSpan typeArgument))
    (Nothing, _) ->
      let (functionCheck, next) = inferExpression env state functionExpr
       in failed (case checkedExprType functionCheck of Just _ -> addTypeError next mkExplicitTypeApplicationTargetError; Nothing -> next)
  where
    checkedTypeArgument = either (const Nothing) Just (normalizeSignatureTypeAt (inferDataTypes state) Map.empty expectedKind typeArgument)
    expectedKind = case explicitQualifiedMethodTypeApplicationKey state functionExpr of
      Just (capability, _) -> maybe TypeKind classParameterKind (Map.lookup capability (inferClassFacts state))
      Nothing -> case explicitTypeApplicationScheme env functionExpr of
        Just scheme
          | variable : _ <- quantifiedVariablesOrderedList (schemeQuantifiedVariables scheme) ->
              let requirements =
                    (schemeResultType scheme, TypeKind)
                      : [ (target, classParameterKind definition)
                        | constraint <- schemeClassConstraints scheme,
                          let (capability, target) = case constraint of TypeSchemeConstraint owner argument -> (owner, argument); TypeSchemeMethodConstraint owner _ argument -> (owner, argument),
                          Just definition <- [Map.lookup capability (inferClassFacts state)]
                        ]
               in either (const TypeKind) (Map.findWithDefault TypeKind variable) (signatureVariableKindsAt (inferDataTypes state) Map.empty requirements)
        _ -> TypeKind
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

explicitQualifiedMethodTypeApplicationKey :: InferState -> Expr 'Resolved -> Maybe CapabilityMethodKey
explicitQualifiedMethodTypeApplicationKey state functionExpr =
  case functionExpr of
    EVar node name
      | Just methodKey <- capabilityMethodKeyFromReference (resolvedValueReference (coreNodeFacts node) name),
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
      instantiateTypeSchemeWithBindings
        typeScheme
        (Map.singleton explicitTypeVar explicitArgumentType)
        remainingTypeVars
        state
