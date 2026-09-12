{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

-- | Finalize node-owned checked decisions by applying solved substitutions.
-- Declaration and expression builders retain their checked children.
module Jazz.Compiler.TypeInference.Analyzed
  ( draftExpressionNode,
    draftOperationNode,
    ExpressionDecision (..),
    noExpressionDecision,
    draftDecidedExpressionNode,
    draftCaseArmNode,
    draftStatementNode,
    constrainBindingRuntimeResult,
    refineListPrependDraft,
    finalizeCheckedExpression,
    projectAnalyzedMethodSignature,
  )
where

import qualified Data.Foldable as Foldable
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Text (Text)
import Jazz.Compiler.AST
  ( CoreNode (..),
    CorePhase (..),
    CoreSort (..),
    Expr (..),
    expressionNode,
  )
import Jazz.Compiler.CoreIdentity (ResolvedNodeFacts (..), ResolvedReference (..))
import Jazz.Compiler.Name (ResolvedName, identifierText)
import Jazz.Compiler.SemanticDeclarations (instantiateDeclarationType)
import Jazz.Compiler.SemanticFacts
  ( AnalyzedMethodSignature (..),
    AnalyzedNumericConstraint (..),
    AnalyzedPrimitiveConstraint (..),
    AnalyzedScheme (..),
    AnalyzedSchemeConstraint (..),
    BinaryOperandTyping (..),
    BinaryOperation (..),
    CoreBinderId (..),
    CoreNodeId,
    EvidenceReference (..),
    ExpressionFacts (..),
    InstantiationTarget (..),
    SemanticFactInvariantFailure (..),
    SemanticInstantiation (..),
    StatementDeclarationFact (..),
    StatementFacts (..),
  )
import Jazz.Compiler.TypeInference.Draft (Attachment (..), CheckedExpr (..), Draft (..), finalizeDraft, rejectedDraft)
import Jazz.Compiler.TypeInference.Solver (resolveType)
import Jazz.Compiler.TypeInference.State
  ( ExplicitInstantiationSeed (..),
    ExplicitInstantiationTarget (..),
    ExpressionEvidenceSeed (..),
    InferState,
    inferNumericVars,
  )
import Jazz.Compiler.TypeInference.TypeOps (freeTypeVariables)
import Jazz.Compiler.TypeInference.Types
  ( ClassMethodType (..),
    ConstructorArgumentType (..),
    ExpressionType,
    IntegerLiteralRange (..),
    NumericConstraint (..),
    SchemeConstraint (..),
    SchemePrimitiveConstraint (..),
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
    draftNodeOperation :: !(Maybe BinaryOperation),
    draftNodeEvidence :: !(Maybe ExpressionEvidenceSeed),
    draftNodeInstantiation :: !(Attachment [SemanticInstantiation])
  }

data ExpressionDecision = ExpressionDecision
  { decisionOperation :: Maybe BinaryOperation,
    decisionEvidence :: Maybe ExpressionEvidenceSeed,
    decisionInstantiation :: Maybe ExplicitInstantiationSeed
  }

noExpressionDecision :: ExpressionDecision
noExpressionDecision = ExpressionDecision Nothing Nothing Nothing

draftExpressionNode :: Maybe ExpressionType -> Expr 'Resolved -> Draft (CoreNode 'Analyzed 'ExpressionSort)
draftExpressionNode = draftDecidedExpressionNode noExpressionDecision

draftOperationNode :: Maybe BinaryOperation -> Maybe ExpressionType -> Expr 'Resolved -> Draft (CoreNode 'Analyzed 'ExpressionSort)
draftOperationNode operation = draftDecidedExpressionNode (noExpressionDecision {decisionOperation = operation})

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
      draftNodeOperation = decisionOperation decision,
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
          evidence = expressionEvidenceFacts state (draftNodeEvidence payload)
       in makeNode semanticType evidence <$> draftNodeInstantiation payload
  where
    operation = resolveOperation <$> draftNodeOperation payload
    resolveOperation selected =
      selected
        { binaryOperationOperandTyping = case binaryOperationOperandTyping selected of
            UniformBinaryOperands operandType -> UniformBinaryOperands (resolveType state operandType)
            Float64PromotedOperands -> Float64PromotedOperands
        }
    operandVariables = case binaryOperationOperandTyping <$> operation of
      Just (UniformBinaryOperands operandType) -> freeTypeVariables operandType
      _ -> Set.empty
    makeNode semanticType evidence explicitFacts =
      CoreNode
        nodeId
        spanValue
        ExpressionFacts
          { expressionResolution = resolution,
            expressionSemanticType = semanticType,
            expressionBinaryOperation = operation,
            expressionNumericConstraints = Map.map projectNumericConstraint (Map.restrictKeys (inferNumericVars state) (freeTypeVariables semanticType <> operandVariables)),
            expressionInstantiations = map (\instantiation -> instantiation {instantiatedTypes = fmap (resolveType state) (instantiatedTypes instantiation)}) explicitFacts,
            expressionEvidence = evidence,
            expressionResultRepresentation = resultRepresentation semanticType
          }

explicitInstantiationFacts :: CoreNodeId -> Maybe (Expr 'Resolved) -> Maybe ExplicitInstantiationSeed -> Maybe ExpressionEvidenceSeed -> Attachment [SemanticInstantiation]
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
                    (Just (CapabilityMethodReference capability method), Just _) ->
                      pure [SemanticInstantiation (MethodInstantiation (capability, method)) resolvedArguments]
                    (_, Nothing) -> missing (MissingExpressionEvidence nodeId)
                    (_, Just _) -> missing (UnexpectedExplicitInstantiationSeed nodeId)
      where
        resolvedArguments = explicitInstantiationSeedArguments seed
        seededTarget = explicitInstantiationTargetName (explicitInstantiationSeedTarget seed)

explicitInstantiationTargetName :: ExplicitInstantiationTarget -> ResolvedName
explicitInstantiationTargetName target =
  case target of
    ExplicitBinderInstantiation name -> name
    ExplicitQualifiedMethodInstantiation name -> name

expressionEvidenceFacts :: InferState -> Maybe ExpressionEvidenceSeed -> [EvidenceReference]
expressionEvidenceFacts state evidence =
  case evidence of
    Nothing -> []
    Just (ExpressionEvidenceSeed capability implementation method targetType) ->
      [ EvidenceReference
          { evidenceCapability = capability,
            evidenceImplementation = implementation,
            evidenceMethod = Just method,
            evidenceType = resolveType state targetType
          }
      ]

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
  Draft (\solved -> CoreNode nodeId spanValue <$> projectStatementBindings solved nodeId resolution binding declaration)

constrainBindingRuntimeResult :: StatementFacts -> Expr 'Analyzed -> Expr 'Analyzed
constrainBindingRuntimeResult statementFacts =
  case statementBinderIds statementFacts of
    binder : _ ->
      case Map.lookup binder (statementGeneralizedSchemes statementFacts) of
        Just scheme -> mapExpressionFacts (\facts -> facts {expressionResultRepresentation = resultRepresentation (analyzedSchemeType scheme)})
        Nothing -> id
    [] -> id

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

projectStatementBindings :: InferState -> CoreNodeId -> ResolvedNodeFacts -> Maybe TypeBinding -> StatementDeclarationFact -> Attachment StatementFacts
projectStatementBindings _ nodeId resolution _ (ValueDeclaration _)
  | Nothing <- resolvedNodeBinder resolution = missing (MissingStatementBinder nodeId)
projectStatementBindings _ nodeId resolution _ (MethodDeclaration _ _)
  | Nothing <- resolvedNodeReference resolution = missing (MissingStatementFacts nodeId)
projectStatementBindings state nodeId resolution maybeBinding declaration =
  case maybeBinding of
    Nothing -> pure (facts [] Map.empty)
    Just binding -> case resolvedNodeBinder resolution of
      Nothing -> missing (MissingStatementBinder nodeId)
      Just binderId -> case projectTypeBinding state binderId binding of
        Left failure -> missing failure
        Right scheme -> pure (facts [binderId] (Map.singleton binderId scheme))
  where
    facts binders schemes = StatementFacts resolution binders schemes declaration

projectTypeBinding :: InferState -> CoreBinderId -> TypeBinding -> Either SemanticFactInvariantFailure AnalyzedScheme
projectTypeBinding state binderId@(CoreBinderId (_, nodeId)) binding =
  case binding of
    PlainTypeBinding expressionType -> Right (monomorphicScheme state expressionType)
    SchemeTypeBinding scheme -> Right (projectScheme state scheme)
    OperatorAliasSchemeTypeBinding _ scheme -> Right (projectScheme state scheme)
    ConstructorTypeBinding {} -> maybe missingScheme Right (projectConstructorBinding binding)
    BuiltinAliasTypeBinding {} -> missingScheme
    BuiltinOperatorAliasTypeBinding {} -> missingScheme
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
    TypeSchemeInferredConstraint name expressionType -> AnalyzedInferredCapabilityConstraint name (resolveType state expressionType)
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

projectAnalyzedMethodSignature :: Text -> ClassMethodType -> Either SemanticFactInvariantFailure AnalyzedMethodSignature
projectAnalyzedMethodSignature methodName (ClassMethodType parameter methodType) =
  case instantiateDeclarationType (Map.singleton parameter parameterType) methodType of
    Nothing -> Left (InvalidAnalyzedMethodSignature methodName)
    Just signature -> Right (AnalyzedMethodSignature parameterId signature)
  where
    parameterId = 0
    parameterType = SemanticVariable parameterId
