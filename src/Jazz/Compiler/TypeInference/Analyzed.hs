{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

-- | Finalize node-owned checked decisions by applying solved substitutions.
-- Declaration and expression builders retain their checked children.
module Jazz.Compiler.TypeInference.Analyzed
  ( draftExpressionNode,
    draftLambda,
    ExpressionDecision (..),
    noExpressionDecision,
    draftDecidedExpressionNode,
    draftCaseArmNode,
    draftStatementNode,
    constrainBindingRuntimeResult,
    refineListPrependDraft,
    finalizeCheckedExpression,
    withEvidenceParameters,
    withRecursiveBindingEvidence,
    retainCheckedEvidence,
  )
where

import qualified Data.Foldable as Foldable
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Jazz.Compiler.AST
  ( CaseArm (..),
    CoreNode (..),
    CorePhase (..),
    CoreSort (..),
    Expr (..),
    ImplMethod (..),
    Statement (..),
    expressionNode,
  )
import Jazz.Compiler.CoreIdentity (CapabilityId (..), CoreBinderId (..), CoreNodeId, ResolvedNodeFacts (..), ResolvedReference (..))
import Jazz.Compiler.Name (ResolvedName, identifierText)
import Jazz.Compiler.SemanticDeclarations (instantiateDeclarationType)
import Jazz.Compiler.SemanticFacts
  ( AnalyzedNumericConstraint (..),
    AnalyzedPrimitiveConstraint (..),
    AnalyzedScheme (..),
    AnalyzedSchemeConstraint (..),
    EvidenceReference (..),
    ExpressionFacts (..),
    InstantiationTarget (..),
    SemanticFactInvariantFailure (..),
    SemanticInstantiation (..),
    StatementDeclarationFact (..),
    StatementFacts (..),
    mapEvidenceTypes,
  )
import Jazz.Compiler.TypeInference.Capabilities (superclassPath)
import Jazz.Compiler.TypeInference.Draft (Attachment (..), CheckedExpr (..), Draft (..), finalizeDraft, rejectedDraft)
import Jazz.Compiler.TypeInference.Solver (resolveType)
import Jazz.Compiler.TypeInference.State
  ( ExplicitInstantiationSeed (..),
    ExplicitInstantiationTarget (..),
    InferState (..),
    InferenceOutput (..),
    ModuleInferenceState (..),
    inferNumericVars,
  )
import Jazz.Compiler.TypeInference.TypeOps (freeTypeVariables)
import Jazz.Compiler.TypeInference.Types
  ( ClassDefinition (..),
    ConstructorArgumentType (..),
    ExpressionType,
    ImplementationTemplate (..),
    IntegerLiteralRange (..),
    NumericConstraint (..),
    SchemeConstraint (..),
    SchemePrimitiveConstraint (..),
    ScopeCapabilityFacts (..),
    SemanticBinding (..),
    SemanticScheme (..),
    SemanticType (..),
    TypeBinding,
    TypeScheme,
    TypeSchemeConstraint,
    TypeSchemePrimitiveConstraint,
    quantifiedVariablesOrderedList,
  )

missing :: SemanticFactInvariantFailure -> Attachment value
missing failure = AttachmentFailed failure Seq.empty

finalizeCheckedExpression :: InferState -> CheckedExpr -> Either (NonEmpty SemanticFactInvariantFailure) (Expr 'Analyzed)
finalizeCheckedExpression solved checked = finalizeDraft solved (checkedExprTree checked)

data ExpressionNodeDraft = ExpressionNodeDraft
  { draftNodeType :: !(Maybe ExpressionType),
    draftNodeEvidence :: !([EvidenceReference]),
    draftNodeInstantiation :: !(Attachment [SemanticInstantiation])
  }

data ExpressionDecision = ExpressionDecision
  { decisionEvidence :: [EvidenceReference],
    decisionInstantiation :: Maybe ExplicitInstantiationSeed
  }

noExpressionDecision :: ExpressionDecision
noExpressionDecision = ExpressionDecision [] Nothing

draftExpressionNode :: Maybe ExpressionType -> Expr 'Resolved -> Draft (CoreNode 'Analyzed 'ExpressionSort)
draftExpressionNode = draftDecidedExpressionNode noExpressionDecision

draftDecidedExpressionNode :: ExpressionDecision -> Maybe ExpressionType -> Expr 'Resolved -> Draft (CoreNode 'Analyzed 'ExpressionSort)
draftDecidedExpressionNode decision result expression =
  case expression of
    EVar _ _ | Nothing <- resolvedNodeReference resolution -> rejectedDraft (MissingExpressionFacts nodeId)
    ELambda {} | Nothing <- resolvedNodeBinder resolution -> rejectedDraft (MissingExpressionFacts nodeId)
    EBlock _ _ | Nothing <- resolvedNodeScope resolution -> rejectedDraft (MissingScopeFacts nodeId)
    _ ->
      let payload = prepareExpressionNode decision (Just expression) result nodeId
       in payload `seq` Draft (\solved -> finalizeExpressionNode solved payload node)
  where
    node = expressionNode expression
    nodeId = coreNodeId node
    resolution = coreNodeFacts node

-- Finish children first so an enclosing lambda reuses nested capture sets.
-- Checked declaration identities supply complete dictionaries, including defaults.
draftLambda :: ScopeCapabilityFacts -> Draft (CoreNode 'Analyzed 'ExpressionSort) -> ResolvedName -> Draft (Expr 'Analyzed) -> Draft (Expr 'Analyzed)
draftLambda declarations node name body = Draft $ \solved ->
  case (,) <$> runDraft node solved <*> runDraft body solved of
    Attached (analyzedNode, analyzedBody) ->
      case requiredEvidenceReferences declarations analyzedBody of
        Left failure -> missing failure
        Right captures -> pure (ELambda (analyzedNode {coreNodeFacts = (coreNodeFacts analyzedNode) {expressionEvidenceCaptures = captures}}) name analyzedBody)
    AttachmentFailed failure failures -> AttachmentFailed failure failures

requiredEvidenceReferences :: ScopeCapabilityFacts -> Expr 'Analyzed -> Either SemanticFactInvariantFailure (Set.Set ResolvedReference)
requiredEvidenceReferences declarations = expression
  where
    expression :: Expr 'Analyzed -> Either SemanticFactInvariantFailure (Set.Set ResolvedReference)
    expression expr = do
      let node = expressionNode expr
      direct <- mconcat <$> traverse (evidence (coreNodeId node)) (expressionEvidence (coreNodeFacts node))
      children <- case expr of
        ELambda {} -> pure (expressionEvidenceCaptures (coreNodeFacts node))
        EList _ values -> expressions values
        ETuple _ values -> expressions values
        EApply _ function argument -> expressions [function, argument]
        ETypeApplication _ function _ _ -> expression function
        EIf _ condition yes no -> expressions [condition, yes, no]
        EPatternCase _ scrutinee arms -> (<>) <$> expression scrutinee <*> (mconcat <$> traverse arm arms)
        EBinary _ _ left right -> expressions [left, right]
        ESectionLeft _ left _ -> expression left
        ESectionRight _ _ right -> expression right
        EBlock _ statements -> mconcat <$> traverse statement statements
        _ -> pure Set.empty
      pure (direct <> children)
    expressions = fmap mconcat . traverse expression
    arm (CaseArm _ _ guard body) = (<>) . Foldable.fold <$> traverse expression guard <*> expression body
    statement value = case value of
      SLet _ _ body -> expression body
      SExpr _ body -> expression body
      SClass _ _ _ _ _ defaults -> mconcat <$> traverse method defaults
      SImpl _ capability _ methods _ ->
        let defaults = maybe Set.empty (Set.map (DefaultMethodReference (CapabilityId capability)) . classDefaultMethods) (Map.lookup (CapabilityId capability) (scopeClassFacts declarations))
         in (defaults <>) . mconcat <$> traverse method methods
      _ -> pure Set.empty
    method (ImplMethod _ _ body) = expression body
    evidence nodeId reference = case reference of
      ParameterEvidence owner index _ _ _ _ -> pure (Set.singleton (EvidenceParameterReference owner index))
      EvidenceReference {evidenceImplementation = implementation, evidencePrerequisites = prerequisites} ->
        case Map.lookup implementation (scopeImplementations declarations) of
          Nothing -> Left (MissingExpressionEvidence nodeId)
          Just template ->
            let methods = Set.fromList (map ImplementationMethodReference (Map.elems (implementationMethods template)))
             in (methods <>) . mconcat <$> traverse (evidence nodeId) prerequisites
      PendingEvidence {} -> Left (MissingExpressionEvidence nodeId)

draftCaseArmNode :: Maybe ExpressionType -> CoreNode 'Resolved 'ExpressionSort -> Draft (CoreNode 'Analyzed 'ExpressionSort)
draftCaseArmNode result node =
  let payload = prepareExpressionNode noExpressionDecision Nothing result (coreNodeId node)
   in payload `seq` Draft (\solved -> finalizeExpressionNode solved payload node)

refineListPrependDraft :: Expr 'Resolved -> ExpressionType -> Draft (Expr 'Analyzed) -> Draft (Expr 'Analyzed)
refineListPrependDraft function elementType checked = case function of
  EApply _ builtin _ ->
    let listType = SemanticList elementType
        partialType = SemanticFunction listType listType
        callableType = SemanticFunction elementType partialType
        rebuild partial callable expression = case expression of
          EApply _ analyzedBuiltin headValue -> EApply partial (mapExpressionFacts (const (coreNodeFacts callable)) analyzedBuiltin) headValue
          _ -> expression
     in rebuild <$> draftExpressionNode (Just partialType) function <*> draftExpressionNode (Just callableType) builtin <*> checked
  _ -> checked

prepareExpressionNode :: ExpressionDecision -> Maybe (Expr 'Resolved) -> Maybe ExpressionType -> CoreNodeId -> ExpressionNodeDraft
prepareExpressionNode decision expression result nodeId =
  ExpressionNodeDraft
    { draftNodeType = result,
      draftNodeEvidence = evidence,
      draftNodeInstantiation = explicitInstantiationFacts nodeId expression (decisionInstantiation decision) evidence
    }
  where
    evidence = decisionEvidence decision

finalizeExpressionNode :: InferState -> ExpressionNodeDraft -> CoreNode 'Resolved 'ExpressionSort -> Attachment (CoreNode 'Analyzed 'ExpressionSort)
finalizeExpressionNode _ _ (CoreNode nodeId _ ResolvedNodeFacts {resolvedNodeReference = Just (UnresolvedReference name)}) =
  missing (UnresolvedExpressionReference nodeId name)
finalizeExpressionNode state payload (CoreNode nodeId spanValue resolution) =
  case draftNodeType payload of
    Nothing -> missing (MissingExpressionFacts nodeId)
    Just inferredType ->
      let semanticType = resolveType state inferredType
          evidence = expressionEvidenceFacts nodeId state $ case (draftNodeEvidence payload, resolvedNodeReference resolution) of
            ([], Just (LexicalReference binder)) -> Map.findWithDefault [] binder (inferenceRecursiveEvidence (inferModule state))
            (selected, _) -> selected
       in makeNode semanticType <$> evidence <*> draftNodeInstantiation payload
  where
    makeNode semanticType evidence explicitFacts =
      CoreNode
        nodeId
        spanValue
        ExpressionFacts
          { expressionResolution = resolution,
            expressionSemanticType = semanticType,
            expressionNumericConstraints = Map.map projectNumericConstraint (Map.restrictKeys (inferNumericVars state) (freeTypeVariables semanticType)),
            expressionInstantiations = map (\instantiation -> instantiation {instantiatedTypes = fmap (resolveType state) (instantiatedTypes instantiation)}) explicitFacts,
            expressionEvidence = evidence,
            expressionEvidenceCaptures = Set.empty,
            expressionResultRepresentation = resultRepresentation semanticType
          }

explicitInstantiationFacts :: CoreNodeId -> Maybe (Expr 'Resolved) -> Maybe ExplicitInstantiationSeed -> [EvidenceReference] -> Attachment [SemanticInstantiation]
explicitInstantiationFacts nodeId expression instantiation evidence =
  case (expression, instantiation) of
    (Just ETypeApplication {}, Nothing) ->
      missing (MissingExplicitInstantiationSeed nodeId)
    (Just (ETypeApplication _ function _ _), Just seed) ->
      attachSeed function seed
    (_, Nothing) -> pure []
    (_, Just _) -> missing (UnexpectedExplicitInstantiationSeed nodeId)
  where
    attachSeed function seed =
      case referencedName function of
        Nothing -> missing (UnidentifiedExplicitInstantiationBinder nodeId)
        Just referencedTarget
          | referencedTarget /= seededTarget ->
              missing (MismatchedExplicitInstantiationSeed nodeId referencedTarget seededTarget)
          | otherwise ->
              case explicitInstantiationSeedTarget seed of
                ExplicitBinderInstantiation _ ->
                  case resolvedBinder function of
                    Nothing -> missing (MissingExplicitInstantiationBinder nodeId referencedTarget)
                    Just binder ->
                      pure [SemanticInstantiation (LexicalInstantiation binder) resolvedArguments]
                ExplicitQualifiedMethodInstantiation _ ->
                  case (resolvedReference function, evidence) of
                    (Just (CapabilityMethodReference capability method), _ : _) ->
                      pure [SemanticInstantiation (MethodInstantiation (capability, method)) resolvedArguments]
                    (_, []) -> missing (MissingExpressionEvidence nodeId)
                    (_, _ : _) -> missing (UnexpectedExplicitInstantiationSeed nodeId)
      where
        resolvedArguments = explicitInstantiationSeedArguments seed
        seededTarget = explicitInstantiationTargetName (explicitInstantiationSeedTarget seed)

explicitInstantiationTargetName :: ExplicitInstantiationTarget -> ResolvedName
explicitInstantiationTargetName target =
  case target of
    ExplicitBinderInstantiation name -> name
    ExplicitQualifiedMethodInstantiation name -> name

-- Keep the queue's checked choices in the owned draft. Later finalization
-- substitutes types, and does not depend on mutable diagnostic/output storage.
retainCheckedEvidence :: InferState -> Draft value -> Draft value
retainCheckedEvidence checked (Draft build) =
  let selected = Map.filter isSelected (outputEvidence (inferOutput checked))
      isSelected EvidenceReference {} = True
      isSelected _ = False
   in Draft $ \solved -> build solved {inferOutput = (inferOutput solved) {outputEvidence = selected <> outputEvidence (inferOutput solved)}}

-- Recursive occurrences are checked against monomorphic seeds. Once their
-- group is generalized, retain its dictionary forwarding in the owned draft.
-- These references reuse the caller's parameters; they do not select instances.
withRecursiveBindingEvidence :: [(CoreBinderId, TypeScheme)] -> Draft value -> Draft value
withRecursiveBindingEvidence bindings (Draft build) = Draft $ \solved ->
  let recursiveEvidence =
        Map.fromList
          [(binder, concatMap evidence (schemeClassConstraints scheme)) | (binder, scheme) <- bindings, not (null (quantifiedVariablesOrderedList (schemeQuantifiedVariables scheme)))]
      evidence constraint = case constraint of
        TypeSchemeConstraint capability target -> [PendingEvidence capability Nothing target]
        TypeSchemeMethodConstraint capability (_, member) target -> [PendingEvidence capability (Just member) target]
   in build solved {inferModule = (inferModule solved) {inferenceRecursiveEvidence = recursiveEvidence <> inferenceRecursiveEvidence (inferModule solved)}}

withEvidenceParameters :: CoreBinderId -> ScopeCapabilityFacts -> TypeScheme -> Draft value -> Draft value
withEvidenceParameters owner facts scheme (Draft build) = Draft $ \solved ->
  let parameters =
        Map.fromList
          [ ((capability, resolveType solved target), (owner, index, path))
          | (index, constraint) <- zip [0 ..] (schemeClassConstraints scheme),
            (source, target) <- case constraint of TypeSchemeConstraint name argument -> [(name, argument)]; TypeSchemeMethodConstraint name _ argument -> [(name, argument)],
            capability <- source : Map.keys (scopeClassFacts facts),
            Just path <- [superclassPath facts source capability]
          ]
      contextual = solved {inferModule = (inferModule solved) {inferenceEvidenceParameters = parameters <> inferenceEvidenceParameters (inferModule solved)}}
   in build contextual

expressionEvidenceFacts :: CoreNodeId -> InferState -> [EvidenceReference] -> Attachment [EvidenceReference]
expressionEvidenceFacts nodeId state = traverse finalize
  where
    finalize request@PendingEvidence {} = case Map.lookup request (outputEvidence (inferOutput state)) of
      Just selected@EvidenceReference {} -> finalize selected
      Just pending -> parameter pending
      Nothing -> parameter request
    finalize (EvidenceReference capability implementation method target prerequisites) =
      EvidenceReference capability implementation method (resolveType state target) <$> traverse finalize prerequisites
    finalize parameterReference@ParameterEvidence {} = pure (mapEvidenceTypes (resolveType state) parameterReference)
    parameter PendingEvidence {evidenceCapability = capability, evidenceMember = member, evidenceType = target} =
      case Map.lookup (capability, resolveType state target) (inferenceEvidenceParameters (inferModule state)) of
        Just (owner, index, path) -> pure (ParameterEvidence owner index path capability member (resolveType state target))
        Nothing -> missing (MissingExpressionEvidence nodeId)
    parameter _ = missing (MissingExpressionEvidence nodeId)

-- Only a closed representation needs enforcement at the return boundary.
resultRepresentation :: ExpressionType -> Maybe ExpressionType
resultRepresentation semanticType
  | Foldable.null semanticType = Just semanticType
  | otherwise = Nothing

resolvedReference :: Expr 'Resolved -> Maybe ResolvedReference
resolvedReference expression = case expression of
  ETypeApplication _ function _ _ -> resolvedReference function
  _ -> resolvedNodeReference (coreNodeFacts (expressionNode expression))

resolvedBinder :: Expr 'Resolved -> Maybe CoreBinderId
resolvedBinder expression = case resolvedReference expression of
  Just (LexicalReference binder) -> Just binder
  _ -> Nothing

referencedName :: Expr 'Resolved -> Maybe ResolvedName
referencedName expression =
  case expression of
    EVar _ name -> Just name
    ETypeApplication _ function _ _ -> referencedName function
    _ -> Nothing

draftStatementNode :: CoreNode 'Resolved 'StatementSort -> Maybe TypeBinding -> StatementDeclarationFact -> Draft (CoreNode 'Analyzed 'StatementSort)
draftStatementNode (CoreNode nodeId spanValue resolution) binding declaration =
  Draft (\solved -> CoreNode nodeId spanValue <$> projectStatementBinding solved nodeId resolution binding declaration)

constrainBindingRuntimeResult :: StatementFacts -> Expr 'Analyzed -> Expr 'Analyzed
constrainBindingRuntimeResult statementFacts =
  case statementBinding statementFacts of
    Just (_, scheme) -> mapExpressionFacts (\facts -> facts {expressionResultRepresentation = resultRepresentation (analyzedSchemeType scheme)})
    Nothing -> id

mapExpressionFacts :: (ExpressionFacts -> ExpressionFacts) -> Expr 'Analyzed -> Expr 'Analyzed
mapExpressionFacts update expression =
  case expression of
    ELit node literal -> ELit (mapNode node) literal
    EVar node name -> EVar (mapNode node) name
    ELambda node name body -> ELambda (mapNode node) name body
    EOperatorValue node name -> EOperatorValue (mapNode node) name
    EList node elements -> EList (mapNode node) elements
    ETuple node elements -> ETuple (mapNode node) elements
    EApply node function argument -> EApply (mapNode node) function argument
    ETypeApplication node function spanValue argument -> ETypeApplication (mapNode node) function spanValue argument
    EIf node condition thenExpression elseExpression -> EIf (mapNode node) condition thenExpression elseExpression
    EPatternCase node scrutinee arms -> EPatternCase (mapNode node) scrutinee arms
    EBinary node operator left right -> EBinary (mapNode node) operator left right
    ESectionLeft node left operator -> ESectionLeft (mapNode node) left operator
    ESectionRight node operator right -> ESectionRight (mapNode node) operator right
    EBlock node statements -> EBlock (mapNode node) statements
  where
    mapNode (CoreNode nodeId spanValue facts) = CoreNode nodeId spanValue (update facts)

projectStatementBinding :: InferState -> CoreNodeId -> ResolvedNodeFacts -> Maybe TypeBinding -> StatementDeclarationFact -> Attachment StatementFacts
projectStatementBinding _ nodeId resolution _ (ValueDeclaration _)
  | Nothing <- resolvedNodeBinder resolution = missing (MissingStatementBinder nodeId)
projectStatementBinding _ nodeId resolution _ MethodDeclaration {}
  | Nothing <- resolvedNodeReference resolution = missing (MissingStatementFacts nodeId)
projectStatementBinding state nodeId resolution maybeBinding declaration =
  case maybeBinding of
    Nothing -> pure (facts Nothing)
    Just binding -> case resolvedNodeBinder resolution of
      Nothing -> missing (MissingStatementBinder nodeId)
      Just binderId -> case projectTypeBinding state binderId binding of
        Left failure -> missing failure
        Right scheme -> pure (facts (Just (binderId, scheme)))
  where
    facts binding = StatementFacts resolution binding declaration

projectTypeBinding :: InferState -> CoreBinderId -> TypeBinding -> Either SemanticFactInvariantFailure AnalyzedScheme
projectTypeBinding state binderId@(CoreBinderId (_, nodeId)) binding =
  case binding of
    PlainTypeBinding expressionType -> Right (monomorphicScheme state expressionType)
    SchemeTypeBinding scheme -> Right (projectScheme state scheme)
    ConstructorTypeBinding {} -> maybe missingScheme Right (projectConstructorBinding binding)
    BuiltinAliasTypeBinding {} -> missingScheme
  where
    missingScheme = Left (MissingStatementScheme nodeId binderId)

-- Constructor quantifiers are local to their declaration scheme. Projecting
-- a normalized declaration does not allocate or inspect solver variables.
projectConstructorBinding :: TypeBinding -> Maybe AnalyzedScheme
projectConstructorBinding (ConstructorTypeBinding name parameters fields) = do
  arguments <- traverse fieldType fields
  pure
    AnalyzedScheme
      { analyzedSchemeVariables = variables,
        analyzedSchemeConstraints = [],
        analyzedSchemePrimitiveConstraints = [],
        analyzedSchemeType = foldr SemanticFunction (SemanticData name (map SemanticVariable variables)) arguments
      }
  where
    variables = take (length parameters) [0 ..]
    argumentsByName = Map.fromList (zip (map identifierText parameters) (map SemanticVariable variables))
    fieldType (ConstructorArgumentType value) = instantiateDeclarationType argumentsByName value
    fieldType ConstructorArgumentFresh = Nothing
projectConstructorBinding _ = Nothing

monomorphicScheme :: InferState -> ExpressionType -> AnalyzedScheme
monomorphicScheme state expressionType =
  AnalyzedScheme
    { analyzedSchemeVariables = [],
      analyzedSchemeConstraints = [],
      analyzedSchemePrimitiveConstraints = [],
      analyzedSchemeType = resolveType state expressionType
    }

projectScheme :: InferState -> TypeScheme -> AnalyzedScheme
projectScheme state scheme =
  AnalyzedScheme
    { analyzedSchemeVariables = quantifiedVariablesOrderedList (schemeQuantifiedVariables scheme),
      analyzedSchemeConstraints = map (projectSchemeConstraint state) (schemeClassConstraints scheme),
      analyzedSchemePrimitiveConstraints = map (projectPrimitiveConstraint state) (schemePrimitiveConstraints scheme),
      analyzedSchemeType = resolveType state (schemeResultType scheme)
    }

projectSchemeConstraint :: InferState -> TypeSchemeConstraint -> AnalyzedSchemeConstraint
projectSchemeConstraint state constraint =
  case constraint of
    TypeSchemeConstraint name expressionType -> AnalyzedExplicitCapabilityConstraint name (resolveType state expressionType)
    TypeSchemeMethodConstraint name method expressionType -> AnalyzedMethodCapabilityConstraint name method (resolveType state expressionType)

projectPrimitiveConstraint :: InferState -> TypeSchemePrimitiveConstraint -> AnalyzedPrimitiveConstraint
projectPrimitiveConstraint state constraint =
  case constraint of
    TypeSchemeNumericConstraint numericConstraint expressionType ->
      AnalyzedNumericPrimitiveConstraint (projectNumericConstraint numericConstraint) (resolveType state expressionType)
    TypeSchemeStrictEqualityConstraint expressionType ->
      AnalyzedStrictEqualityPrimitiveConstraint (resolveType state expressionType)

projectNumericConstraint :: NumericConstraint -> AnalyzedNumericConstraint
projectNumericConstraint constraint =
  case constraint of
    AnyNumericConstraint -> AnalyzedAnyNumericConstraint
    RuntimeArithmeticNumericConstraint -> AnalyzedRuntimeArithmeticNumericConstraint
    RuntimeComparisonNumericConstraint -> AnalyzedRuntimeComparisonNumericConstraint
    IntegralNumericConstraint -> AnalyzedIntegralNumericConstraint
    IntegralLiteralNumericConstraint (IntegerLiteralRange lower upper) -> AnalyzedIntegralLiteralNumericConstraint lower upper
