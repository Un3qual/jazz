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
    draftStatement,
    refineListPrependDraft,
    finalizeCheckedExpression,
    attachAnalyzedStatementFacts,
    projectAnalyzedMethodSignature,
  )
where

import qualified Data.Foldable as Foldable
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Text (Text)
import Jazz.Compiler.AST
  ( ClassMethodSignature (..),
    CoreNode (..),
    CorePhase (..),
    CoreSort (..),
    DataConstructor (..),
    Expr (..),
    ImplMethod (..),
    Literal (..),
    Statement (..),
    expressionNode,
  )
import Jazz.Compiler.CoreIdentity (ResolvedNodeFacts (..), ResolvedReference (..))
import Jazz.Compiler.Name (ResolvedName, identifierText, operatorBindingName)
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
    RuntimeObligation (..),
    RuntimePlan (..),
    SemanticFactInvariantFailure (..),
    SemanticInstantiation (..),
    StatementDeclarationFact,
    StatementFacts (..),
  )
import Jazz.Compiler.TypeInference.Draft (Attachment (..), CheckedExpr (..), Draft (..), attachmentResult)
import Jazz.Compiler.TypeInference.Solver (resolveType)
import Jazz.Compiler.TypeInference.State
  ( ExplicitInstantiationSeed (..),
    ExplicitInstantiationTarget (..),
    ExpressionEvidenceSeed (..),
    InferState,
    inferFactInvariantFailures,
    inferNumericVars,
    inferStatementFactSeeds,
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

data AttachedExplicitInstantiation = AttachedExplicitInstantiation
  { attachedSemanticInstantiations :: [SemanticInstantiation],
    attachedRuntimeArguments :: [NonEmpty ExpressionType]
  }

recordedFailures :: InferState -> Attachment ()
recordedFailures state = case inferFactInvariantFailures state of
  [] -> pure ()
  failure : failures -> AttachmentFailed failure (Seq.fromList failures)

missing :: SemanticFactInvariantFailure -> Attachment value
missing failure = AttachmentFailed failure Seq.empty

finalizeCheckedExpression :: InferState -> CheckedExpr -> Either (NonEmpty SemanticFactInvariantFailure) (Expr 'Analyzed)
finalizeCheckedExpression solved checked = attachmentResult (recordedFailures solved *> runDraft (checkedExprTree checked) solved)

data ExpressionNodeDraft = ExpressionNodeDraft
  { draftNodeType :: !(Maybe ExpressionType),
    draftNodeOperation :: !(Maybe BinaryOperation),
    draftNodeEvidence :: !(Maybe ExpressionEvidenceSeed),
    draftNodeInstantiation :: !(Attachment AttachedExplicitInstantiation),
    draftNodeNumericLiteral :: !Bool
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
  let node = expressionNode expression
      payload = prepareExpressionNode decision (Just expression) result (coreNodeId node)
   in payload `seq` Draft (\solved -> finalizeExpressionNode solved payload node)

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
      draftNodeInstantiation = explicitInstantiationFacts nodeId expression (decisionInstantiation decision) evidence,
      draftNodeNumericLiteral = case expression of Just (ELit _ LInt {}) -> True; Just (ELit _ LFloat {}) -> True; _ -> False
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
          evidenceObligations = maybe Seq.empty (Seq.singleton . SupplyEvidence) (NonEmpty.nonEmpty evidence)
       in makeNode semanticType evidence evidenceObligations <$> draftNodeInstantiation payload
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
    makeNode semanticType evidence evidenceObligations explicitFacts =
      CoreNode
        nodeId
        spanValue
        ExpressionFacts
          { expressionResolution = resolution,
            expressionSemanticType = semanticType,
            expressionBinaryOperation = operation,
            expressionNumericConstraints = Map.map projectNumericConstraint (Map.restrictKeys (inferNumericVars state) (freeTypeVariables semanticType <> operandVariables)),
            expressionInstantiations = map (\instantiation -> instantiation {instantiatedTypes = fmap (resolveType state) (instantiatedTypes instantiation)}) (attachedSemanticInstantiations explicitFacts),
            expressionEvidence = evidence,
            expressionRuntimePlan =
              RuntimePlan
                ( foldMap (Seq.singleton . InstantiateTypes . fmap (resolveType state)) (attachedRuntimeArguments explicitFacts)
                    <> evidenceObligations
                    <> numericLiteralObligations (draftNodeNumericLiteral payload) semanticType
                    <> runtimeResultObligations semanticType
                )
          }

explicitInstantiationFacts :: CoreNodeId -> Maybe (Expr 'Resolved) -> Maybe ExplicitInstantiationSeed -> Maybe ExpressionEvidenceSeed -> Attachment AttachedExplicitInstantiation
explicitInstantiationFacts nodeId expression instantiation evidence =
  case (expression, instantiation) of
    (Just ETypeApplication {}, Nothing) ->
      missing (MissingExplicitInstantiationSeed nodeId)
    (Just (ETypeApplication _ function _ _), Just seed) ->
      attachSeed function seed
    (_, Nothing) -> pure noExplicitInstantiation
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
                      pure
                        AttachedExplicitInstantiation
                          { attachedSemanticInstantiations = [SemanticInstantiation (LexicalInstantiation binder) resolvedArguments],
                            attachedRuntimeArguments = [resolvedArguments]
                          }
                ExplicitQualifiedMethodInstantiation _ ->
                  case (resolvedReference function, evidence) of
                    (Just (CapabilityMethodReference capability method), Just _) ->
                      pure
                        AttachedExplicitInstantiation
                          { attachedSemanticInstantiations = [SemanticInstantiation (MethodInstantiation capability method) resolvedArguments],
                            attachedRuntimeArguments = [resolvedArguments]
                          }
                    (_, Nothing) -> missing (MissingExpressionEvidence nodeId)
                    (_, Just _) -> missing (UnexpectedExplicitInstantiationSeed nodeId)
      where
        resolvedArguments = explicitInstantiationSeedArguments seed
        seededTarget = explicitInstantiationTargetName (explicitInstantiationSeedTarget seed)

    noExplicitInstantiation = AttachedExplicitInstantiation [] []

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

numericLiteralObligations :: Bool -> ExpressionType -> Seq RuntimeObligation
numericLiteralObligations True (SemanticNumeric numericType) = Seq.singleton (SpecializeNumericLiteral numericType)
numericLiteralObligations _ _ = Seq.empty

-- Polymorphic constraints do no runtime work. Ordinary Int defaulting and
-- concrete representation hints remain result obligations.
runtimeResultObligations :: ExpressionType -> Seq RuntimeObligation
runtimeResultObligations semanticType
  | Foldable.null semanticType = Seq.singleton (ConstrainResult semanticType)
  | otherwise = Seq.empty

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
    EOperatorValue _ operatorSymbol -> Just (operatorBindingName operatorSymbol)
    ETypeApplication _ function _ _ -> referencedName function
    _ -> Nothing

draftStatement :: InferState -> Statement 'Resolved -> Maybe CheckedExpr -> [(Int, CheckedExpr)] -> Draft (Statement 'Analyzed)
draftStatement checked statement body methods = case statement of
  SLet node name value -> makeLet name <$> facts node <*> valueDraft value body
  SSignature node name signature -> SSignature <$> facts node <*> pure name <*> pure signature
  SData node name parameters constructors -> SData <$> facts node <*> pure name <*> pure parameters <*> traverse constructor constructors
  SClass node name parameters signatures -> SClass <$> facts node <*> pure name <*> pure parameters <*> traverse classMethod signatures
  SImpl node name arguments declarations -> SImpl <$> facts node <*> pure name <*> pure arguments <*> traverse implMethod (zip [0 ..] declarations)
  SModule node path -> SModule <$> facts node <*> pure path
  SImport node path alias names -> SImport <$> facts node <*> pure path <*> pure alias <*> pure names
  SExpr node value -> SExpr <$> facts node <*> valueDraft value body
  where
    facts (CoreNode nodeId spanValue resolution) =
      let seed = Map.lookup nodeId (inferStatementFactSeeds checked)
       in seed `seq` Draft (\solved -> CoreNode nodeId spanValue <$> projectStatementSeed solved nodeId resolution seed)
    valueDraft _ (Just value) = checkedExprTree value
    valueDraft value Nothing = Draft (const (missing (MissingExpressionFacts (coreNodeId (expressionNode value)))))
    constructor (DataConstructor node name arguments) = DataConstructor <$> facts node <*> pure name <*> pure arguments
    classMethod (ClassMethodSignature node name signature) = ClassMethodSignature <$> facts node <*> pure name <*> pure signature
    implMethod (index, ImplMethod node name value) = ImplMethod <$> facts node <*> pure name <*> valueDraft value (lookup index methods)
    makeLet name node value = SLet node name (constrainBindingRuntimeResult (coreNodeFacts node) value)

constrainBindingRuntimeResult :: StatementFacts -> Expr 'Analyzed -> Expr 'Analyzed
constrainBindingRuntimeResult statementFacts =
  case statementBinderIds statementFacts of
    binder : _ ->
      case Map.lookup binder (statementGeneralizedSchemes statementFacts) of
        Just scheme -> mapExpressionFacts (replaceRuntimeResult (analyzedSchemeType scheme))
        Nothing -> id
    [] -> id

replaceRuntimeResult :: ExpressionType -> ExpressionFacts -> ExpressionFacts
replaceRuntimeResult runtimeType facts =
  facts
    { expressionRuntimePlan =
        RuntimePlan
          (prefix <> runtimeResultObligations runtimeType)
    }
  where
    RuntimePlan obligations = expressionRuntimePlan facts
    prefix = case Seq.viewr obligations of
      rest Seq.:> ConstrainResult _ -> rest
      _ -> obligations

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

attachAnalyzedStatementFacts :: InferState -> [CoreNode 'Resolved 'StatementSort] -> Either (NonEmpty SemanticFactInvariantFailure) (Map CoreNodeId StatementFacts)
attachAnalyzedStatementFacts state nodes =
  attachmentResult $
    Map.fromList <$> traverse (\node -> (,) (coreNodeId node) <$> projectStatementFacts state (coreNodeId node) (coreNodeFacts node)) nodes

projectStatementFacts :: InferState -> CoreNodeId -> ResolvedNodeFacts -> Attachment StatementFacts
projectStatementFacts state nodeId resolution =
  projectStatementSeed state nodeId resolution (Map.lookup nodeId (inferStatementFactSeeds state))

projectStatementSeed :: InferState -> CoreNodeId -> ResolvedNodeFacts -> Maybe ([(ResolvedName, TypeBinding)], StatementDeclarationFact) -> Attachment StatementFacts
projectStatementSeed state nodeId resolution seed =
  case seed of
    Nothing -> missing (MissingStatementFacts nodeId)
    Just ([], declarationFact) -> pure (facts declarationFact [] Map.empty)
    Just (bindings, declarationFact) ->
      case resolvedNodeBinder resolution of
        Nothing -> missing (MissingStatementBinder nodeId)
        Just binderId ->
          case traverse (projectTypeBinding state binderId . snd) bindings of
            Left failure -> missing failure
            Right projectedSchemes -> pure (facts declarationFact [binderId] (Map.fromList [(binderId, scheme) | scheme <- projectedSchemes]))
  where
    facts declaration binders schemes =
      StatementFacts resolution binders schemes declaration

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
