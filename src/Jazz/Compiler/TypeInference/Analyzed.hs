{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE KindSignatures #-}

-- | The single checked Resolved-to-Analyzed reconstruction. Inference records
-- facts in source-unit-local maps; this traversal consumes every entry through
-- the original node identity and span.
module Jazz.Compiler.TypeInference.Analyzed
  ( attachAnalyzedExpression,
    attachAnalyzedSourceUnitExpression,
    attachAnalyzedStatementFacts,
    projectAnalyzedMethodSignature,
  )
where

import Data.Bifunctor (first)
import Data.Foldable (toList)
import Data.List (mapAccumL)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Text (Text)
import Jazz.Compiler.AST
  ( CaseArm (..),
    ClassMethodSignature (..),
    CoreNode (..),
    CorePhase (..),
    CoreSort (..),
    DataConstructor (..),
    Expr (..),
    ImplMethod (..),
    Literal (..),
    Pattern (..),
    Statement (..),
  )
import Jazz.Compiler.ModuleIdentity (ModulePath)
import Jazz.Compiler.Name (ResolvedName, identifierText, operatorBindingName)
import Jazz.Compiler.RecursiveBindings (inferRecursiveGroupsOrdered)
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
    NumericTarget (..),
    PatternFacts (..),
    RuntimeObligation (..),
    RuntimePlan (..),
    SemanticFactInvariantFailure (..),
    SemanticInstantiation (..),
    StatementDeclarationFact (..),
    StatementFacts (..),
  )
import Jazz.Compiler.SourceUnitOwnership
  ( sourceUnitOwnerModulePath,
    sourceUnitStatementOwners,
  )
import Jazz.Compiler.TypeInference.Pattern (instantiateConstructorBinding)
import qualified Jazz.Compiler.TypeInference.Signature as Signature
import Jazz.Compiler.TypeInference.Solver (freshTypeVariable, resolveType)
import Jazz.Compiler.TypeInference.State
  ( ExplicitInstantiationSeed (..),
    ExplicitInstantiationTarget (..),
    ExpressionEvidenceSeed (..),
    InferState,
    inferBinaryOperations,
    inferExplicitInstantiationSeeds,
    inferExpressionEvidenceSeeds,
    inferExpressionFactTypes,
    inferFactInvariantFailures,
    inferNumericVars,
    inferPatternFactSeeds,
    inferStatementFactSeeds,
  )
import Jazz.Compiler.TypeInference.TypeOps (freeTypeVariables)
import Jazz.Compiler.TypeInference.Types
  ( ClassMethodType (..),
    ExpressionType,
    IntegerLiteralRange (..),
    NumericConstraint (..),
    SchemeConstraint (..),
    SchemePrimitiveConstraint (..),
    SemanticType (..),
    TypeBinding (..),
    TypeScheme (..),
    TypeSchemeConstraint,
    TypeSchemePrimitiveConstraint,
    quantifiedVariablesOrderedList,
  )
import Jazz.Compiler.TypeRepresentation (SignaturePayload (..))

data Attachment value
  = Attached value
  | AttachmentFailed SemanticFactInvariantFailure !(Seq SemanticFactInvariantFailure)
  deriving (Functor)

data AttachedExplicitInstantiation = AttachedExplicitInstantiation
  { attachedSemanticInstantiations :: [SemanticInstantiation],
    attachedRuntimeArguments :: [NonEmpty ExpressionType]
  }

instance Applicative Attachment where
  pure = Attached
  Attached project <*> Attached value = Attached (project value)
  AttachmentFailed failure failures <*> AttachmentFailed next rest =
    AttachmentFailed failure (failures Seq.>< (next Seq.<| rest))
  AttachmentFailed failure failures <*> Attached _ = AttachmentFailed failure failures
  Attached _ <*> AttachmentFailed failure failures = AttachmentFailed failure failures

attachmentResult :: Attachment value -> Either (NonEmpty SemanticFactInvariantFailure) value
attachmentResult (Attached value) = Right value
attachmentResult (AttachmentFailed failure failures) = Left (failure NonEmpty.:| toList failures)

recordedFailures :: InferState -> Attachment ()
recordedFailures state = case inferFactInvariantFailures state of
  [] -> pure ()
  failure : failures -> AttachmentFailed failure (Seq.fromList failures)

attachAnalyzedExpression ::
  ModulePath ->
  Map ResolvedName CoreBinderId ->
  InferState ->
  Expr 'Resolved ->
  Either (NonEmpty SemanticFactInvariantFailure) (Expr 'Analyzed)
attachAnalyzedExpression modulePath importedBinders state expression =
  attachmentResult (recordedFailures state *> attachExpr modulePath state importedBinders expression)

attachAnalyzedSourceUnitExpression ::
  ModulePath ->
  ModulePath ->
  Set.Set Int ->
  InferState ->
  Expr 'Resolved ->
  Either (NonEmpty SemanticFactInvariantFailure) (Expr 'Analyzed)
attachAnalyzedSourceUnitExpression sourcePath preludePath preludeStatementIndices state expression =
  attachmentResult (recordedFailures state *> attachedExpression)
  where
    attachedExpression =
      case expression of
        EBlock node statements ->
          EBlock
            <$> attachExpressionNode state Map.empty (Just expression) node
            <*> traverse
              ( \(ownerPath, statementBinders, statement) ->
                  attachStatementNode ownerPath state statementBinders statement
              )
              (statementEnvironmentsByPath sourceUnitStatementPath Map.empty statements)
        _ -> attachExpr sourcePath state Map.empty expression
    sourceUnitStatementPath statementIndex =
      Map.findWithDefault sourcePath statementIndex sourceUnitPathsByStatement
    sourceUnitPathsByStatement =
      Map.fromList
        ( zip
            [0 :: Int ..]
            ( map
                sourceUnitOwnerModulePath
                (sourceUnitStatementOwners sourcePath preludePath preludeStatementIndices sourceUnitStatements)
            )
        )
    sourceUnitStatements =
      case expression of
        EBlock _ statements -> statements
        _ -> []

missing :: SemanticFactInvariantFailure -> Attachment value
missing failure = AttachmentFailed failure Seq.empty

attachExpr :: ModulePath -> InferState -> Map ResolvedName CoreBinderId -> Expr 'Resolved -> Attachment (Expr 'Analyzed)
attachExpr modulePath state binders expression =
  case expression of
    ELit node literal -> ELit <$> attachNode expression node <*> pure literal
    EVar node name -> EVar <$> attachNode expression node <*> pure name
    ELambda node name body ->
      ELambda
        <$> attachNode expression node
        <*> pure name
        <*> attachExpr modulePath state (insertBinder modulePath name node binders) body
    EOperatorValue node name -> EOperatorValue <$> attachNode expression node <*> pure name
    EList node elements -> EList <$> attachNode expression node <*> traverse recur elements
    ETuple node elements -> ETuple <$> attachNode expression node <*> traverse recur elements
    EApply node function argument -> EApply <$> attachNode expression node <*> recur function <*> recur argument
    ETypeApplication node function argumentSpan argument ->
      ETypeApplication
        <$> attachNode expression node
        <*> recur function
        <*> pure argumentSpan
        <*> pure argument
    EIf node condition thenExpression elseExpression ->
      EIf <$> attachNode expression node <*> recur condition <*> recur thenExpression <*> recur elseExpression
    EPatternCase node scrutinee arms ->
      EPatternCase <$> attachNode expression node <*> recur scrutinee <*> traverse attachArm arms
    EBinary node operator left right -> EBinary <$> attachNode expression node <*> pure operator <*> recur left <*> recur right
    ESectionLeft node left operator -> ESectionLeft <$> attachNode expression node <*> recur left <*> pure operator
    ESectionRight node operator right -> ESectionRight <$> attachNode expression node <*> pure operator <*> recur right
    EBlock node statements ->
      EBlock
        <$> attachNode expression node
        <*> traverse (uncurry attachStatement) (statementEnvironments modulePath binders statements)
  where
    recur = attachExpr modulePath state binders
    attachNode value = attachExpressionNode state binders (Just value)
    attachPattern = attachPatternNode state
    attachStatement statementBinders = attachStatementNode modulePath state statementBinders
    attachArm (CaseArm armNode pattern guard body) =
      let armBinders = extendPatternBinders modulePath binders pattern
          recurArm = attachExpr modulePath state armBinders
       in CaseArm
            <$> attachExpressionNode state armBinders Nothing armNode
            <*> attachPattern pattern
            <*> traverse recurArm guard
            <*> recurArm body

attachExpressionNode :: InferState -> Map ResolvedName CoreBinderId -> Maybe (Expr 'Resolved) -> CoreNode 'Resolved 'ExpressionSort -> Attachment (CoreNode 'Analyzed 'ExpressionSort)
attachExpressionNode state binders expression (CoreNode nodeId spanValue ()) =
  case Map.lookup nodeId (inferExpressionFactTypes state) of
    Nothing -> missing (MissingExpressionFacts nodeId)
    Just inferredType ->
      let semanticType = resolveType state inferredType
          evidence = expressionEvidenceFacts state nodeId
          evidenceObligations =
            maybe Seq.empty (Seq.singleton . SupplyEvidence) (NonEmpty.nonEmpty evidence)
       in makeNode semanticType evidence evidenceObligations
            <$> explicitInstantiationFacts state binders nodeId expression
      where
        operation = resolveOperation <$> Map.lookup nodeId (inferBinaryOperations state)
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
              { expressionSemanticType = semanticType,
                expressionBinaryOperation = operation,
                expressionNumericConstraints =
                  Map.map
                    projectNumericConstraint
                    (Map.restrictKeys (inferNumericVars state) (freeTypeVariables semanticType <> operandVariables)),
                expressionInstantiations = attachedSemanticInstantiations explicitFacts,
                expressionEvidence = evidence,
                expressionRuntimePlan =
                  RuntimePlan
                    ( foldMap (Seq.singleton . InstantiateTypes) (attachedRuntimeArguments explicitFacts)
                        <> evidenceObligations
                        <> foldMap (\value -> numericLiteralObligations state value semanticType) expression
                        <> Seq.singleton (ConstrainResult semanticType)
                    )
              }

explicitInstantiationFacts :: InferState -> Map ResolvedName CoreBinderId -> CoreNodeId -> Maybe (Expr 'Resolved) -> Attachment AttachedExplicitInstantiation
explicitInstantiationFacts state binders nodeId expression =
  case (expression, Map.lookup nodeId (inferExplicitInstantiationSeeds state)) of
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
                  case referencedBinder binders function of
                    Nothing -> missing (MissingExplicitInstantiationBinder nodeId referencedTarget)
                    Just binder ->
                      pure
                        AttachedExplicitInstantiation
                          { attachedSemanticInstantiations = [SemanticInstantiation binder resolvedArguments],
                            attachedRuntimeArguments = [resolvedArguments]
                          }
                ExplicitQualifiedMethodInstantiation _ ->
                  case (referencedBinder binders function, Map.lookup nodeId (inferExpressionEvidenceSeeds state)) of
                    (Nothing, Just _) ->
                      pure
                        AttachedExplicitInstantiation
                          { attachedSemanticInstantiations = [],
                            attachedRuntimeArguments = [resolvedArguments]
                          }
                    (_, Nothing) -> missing (MissingExpressionEvidence nodeId)
                    (Just _, Just _) -> missing (UnexpectedExplicitInstantiationSeed nodeId)
      where
        resolvedArguments = fmap (resolveType state) (explicitInstantiationSeedArguments seed)
        seededTarget = explicitInstantiationTargetName (explicitInstantiationSeedTarget seed)

    noExplicitInstantiation = AttachedExplicitInstantiation [] []

explicitInstantiationTargetName :: ExplicitInstantiationTarget -> ResolvedName
explicitInstantiationTargetName target =
  case target of
    ExplicitBinderInstantiation name -> name
    ExplicitQualifiedMethodInstantiation name -> name

expressionEvidenceFacts :: InferState -> CoreNodeId -> [EvidenceReference]
expressionEvidenceFacts state nodeId =
  case Map.lookup nodeId (inferExpressionEvidenceSeeds state) of
    Nothing -> []
    Just (ExpressionEvidenceSeed capability implementation method targetType) ->
      [ EvidenceReference
          { evidenceCapability = capability,
            evidenceImplementation = implementation,
            evidenceMethod = Just method,
            evidenceType = resolveType state targetType
          }
      ]

numericLiteralObligations :: InferState -> Expr 'Resolved -> ExpressionType -> Seq RuntimeObligation
numericLiteralObligations state expression expressionType =
  case expression of
    ELit _ (LInt _) ->
      case expressionType of
        SemanticInt -> Seq.singleton (SpecializeNumericLiteral DefaultIntegerTarget)
        SemanticNumeric numericType -> Seq.singleton (SpecializeNumericLiteral (ConcreteNumericTarget numericType))
        SemanticVariable typeVar
          | Just IntegralLiteralNumericConstraint {} <- Map.lookup typeVar (inferNumericVars state) ->
              Seq.singleton (SpecializeNumericLiteral DefaultIntegerTarget)
        _ -> Seq.empty
    ELit _ (LFloat _ _ _) ->
      case expressionType of
        SemanticNumeric numericType -> Seq.singleton (SpecializeNumericLiteral (ConcreteNumericTarget numericType))
        _ -> Seq.empty
    _ -> Seq.empty

referencedBinder :: Map ResolvedName CoreBinderId -> Expr 'Resolved -> Maybe CoreBinderId
referencedBinder binders expression =
  case expression of
    EVar _ name -> Map.lookup name binders
    EOperatorValue _ operatorSymbol -> Map.lookup (operatorBindingName operatorSymbol) binders
    ETypeApplication _ function _ _ -> referencedBinder binders function
    _ -> Nothing

referencedName :: Expr 'Resolved -> Maybe ResolvedName
referencedName expression =
  case expression of
    EVar _ name -> Just name
    EOperatorValue _ operatorSymbol -> Just (operatorBindingName operatorSymbol)
    ETypeApplication _ function _ _ -> referencedName function
    _ -> Nothing

attachPatternNode :: InferState -> Pattern 'Resolved -> Attachment (Pattern 'Analyzed)
attachPatternNode state pattern =
  case pattern of
    PWildcard node -> PWildcard <$> facts node
    PVariable node name -> PVariable <$> facts node <*> pure name
    PLiteral node literal -> PLiteral <$> facts node <*> pure literal
    PConstructor node name patterns -> PConstructor <$> facts node <*> pure name <*> traverse recur patterns
    PList node patterns -> PList <$> facts node <*> traverse recur patterns
    PConsList node headPattern tailPattern -> PConsList <$> facts node <*> recur headPattern <*> recur tailPattern
    PTuple node patterns -> PTuple <$> facts node <*> traverse recur patterns
    PAs node name nested -> PAs <$> facts node <*> pure name <*> recur nested
    POr node alternatives -> POr <$> facts node <*> traverse recur alternatives
  where
    recur = attachPatternNode state
    facts (CoreNode nodeId spanValue ()) =
      case Map.lookup nodeId (inferPatternFactSeeds state) of
        Nothing -> missing (MissingPatternFacts nodeId)
        Just seed ->
          pure
            ( CoreNode
                nodeId
                spanValue
                seed
                  { patternBindingTypes = Map.map (resolveType state) (patternBindingTypes seed)
                  }
            )

attachStatementNode :: ModulePath -> InferState -> Map ResolvedName CoreBinderId -> Statement 'Resolved -> Attachment (Statement 'Analyzed)
attachStatementNode modulePath state binders statement =
  case statement of
    SLet node name value -> makeLet name <$> facts node <*> recur value
    SSignature node name signature -> SSignature <$> facts node <*> pure name <*> pure signature
    SData node name parameters constructors -> SData <$> facts node <*> pure name <*> pure parameters <*> traverse attachConstructor constructors
    SClass node name parameters methods -> SClass <$> facts node <*> pure name <*> pure parameters <*> traverse (attachClassMethod parameters) methods
    SImpl node name arguments methods -> SImpl <$> attachImplementationFacts name arguments node <*> pure name <*> pure arguments <*> traverse attachImplMethod methods
    SModule node path -> SModule <$> facts node <*> pure path
    SImport node path alias names -> SImport <$> facts node <*> pure path <*> pure alias <*> pure names
    SExpr node value -> SExpr <$> facts node <*> recur value
  where
    recur = attachExpr modulePath state binders
    facts = attachStatementFacts modulePath state
    attachConstructor (DataConstructor node name arguments) = DataConstructor <$> facts node <*> pure name <*> pure arguments
    attachClassMethod parameters (ClassMethodSignature node name signature) =
      ClassMethodSignature <$> analyzedMethodNode <*> pure name <*> pure signature
      where
        analyzedMethodNode = case parameters of
          [parameter] -> case projectAnalyzedMethodSignature state (identifierText name) (ClassMethodType (identifierText parameter) signature) of
            Left failure -> missing failure
            Right method -> setDeclaration (MethodDeclaration name method) <$> facts node
          _ -> missing (InvalidAnalyzedMethodSignature (identifierText name))
    attachImplementationFacts name arguments node =
      case traverse (Signature.signatureTypeToExpressionType state Map.empty) arguments of
        Left _ -> missing (InvalidAnalyzedImplementationTarget (coreNodeId node))
        Right targets -> setDeclaration (ImplementationDeclaration name (map (resolveType state) targets)) <$> facts node
    setDeclaration declaration node =
      node {coreNodeFacts = (coreNodeFacts node) {statementDeclarationFact = declaration}}
    attachImplMethod (ImplMethod node name body) =
      ImplMethod
        <$> facts node
        <*> pure name
        <*> attachExpr modulePath state methodBinders body
    methodBinders =
      case statement of
        SImpl _ _ _ methods ->
          foldl
            (\acc (ImplMethod node name _) -> insertBinder modulePath name node acc)
            binders
            methods
        _ -> binders
    makeLet name analyzedNode analyzedValue =
      SLet analyzedNode name (constrainBindingRuntimeResult (coreNodeFacts analyzedNode) analyzedValue)

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
          ( case Seq.viewr obligations of
              prefix Seq.:> ConstrainResult _ -> prefix Seq.|> ConstrainResult runtimeType
              _ -> obligations Seq.|> ConstrainResult runtimeType
          )
    }
  where
    RuntimePlan obligations = expressionRuntimePlan facts

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

attachStatementFacts :: ModulePath -> InferState -> CoreNode 'Resolved 'StatementSort -> Attachment (CoreNode 'Analyzed 'StatementSort)
attachStatementFacts modulePath state (CoreNode nodeId spanValue ()) =
  CoreNode nodeId spanValue <$> projectStatementFacts modulePath state nodeId

attachAnalyzedStatementFacts ::
  ModulePath ->
  InferState ->
  [CoreNodeId] ->
  Either (NonEmpty SemanticFactInvariantFailure) (Map CoreNodeId StatementFacts)
attachAnalyzedStatementFacts modulePath state nodeIds =
  attachmentResult $
    Map.fromList <$> traverse (\nodeId -> (,) nodeId <$> projectStatementFacts modulePath state nodeId) nodeIds

projectStatementFacts :: ModulePath -> InferState -> CoreNodeId -> Attachment StatementFacts
projectStatementFacts modulePath state nodeId =
  case Map.lookup nodeId (inferStatementFactSeeds state) of
    Nothing -> missing (MissingStatementFacts nodeId)
    Just (bindings, declarationFact) ->
      let binderId = CoreBinderId (modulePath, nodeId)
       in case traverse (projectTypeBinding state binderId . snd) bindings of
            Left failure -> missing failure
            Right projectedSchemes ->
              pure
                StatementFacts
                  { statementBinderIds = [binderId | not (null bindings)],
                    statementGeneralizedSchemes =
                      Map.fromList [(binderId, scheme) | scheme <- projectedSchemes],
                    statementDeclarationFact = declarationFact
                  }

projectTypeBinding :: InferState -> CoreBinderId -> TypeBinding -> Either SemanticFactInvariantFailure AnalyzedScheme
projectTypeBinding state binderId@(CoreBinderId (_, nodeId)) binding =
  case binding of
    PlainTypeBinding expressionType -> Right (monomorphicScheme state expressionType)
    SchemeTypeBinding scheme -> Right (projectScheme state scheme)
    OperatorAliasSchemeTypeBinding _ scheme -> Right (projectScheme state scheme)
    ConstructorTypeBinding {} -> maybe missingScheme Right (projectConstructorBinding state binding)
    BuiltinAliasTypeBinding {} -> missingScheme
    BuiltinOperatorAliasTypeBinding {} -> missingScheme
  where
    missingScheme = Left (MissingStatementScheme nodeId binderId)

projectConstructorBinding :: InferState -> TypeBinding -> Maybe AnalyzedScheme
projectConstructorBinding state binding = do
  (argumentTypes, resultType, instantiatedState) <- instantiateConstructorBinding binding state
  let constructorType = foldr SemanticFunction resultType argumentTypes
      resolvedConstructorType = resolveType instantiatedState constructorType
  pure
    AnalyzedScheme
      { analyzedSchemeVariables = Set.toAscList (freeTypeVariables resolvedConstructorType),
        analyzedSchemeConstraints = [],
        analyzedSchemePrimitiveConstraints = [],
        analyzedSchemeType = resolvedConstructorType
      }

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

projectAnalyzedMethodSignature :: InferState -> Text -> ClassMethodType -> Either SemanticFactInvariantFailure AnalyzedMethodSignature
projectAnalyzedMethodSignature state methodName (ClassMethodType parameter payload) = do
  signatureType <- case payload of
    SignatureType value -> Right value
    ConstrainedSignature [] value -> Right value
    _ -> Left failure
  -- Allocate the declared binder before conversion. The explicit environment
  -- rejects other variables, including future unsupported method polymorphism.
  let (parameterId, parameterType, methodState) = freshTypeVariable state
  methodType <-
    first
      (const failure)
      (Signature.signatureTypeToExpressionType methodState (Map.singleton parameter parameterType) signatureType)
  pure
    AnalyzedMethodSignature
      { analyzedMethodClassParameter = parameterId,
        analyzedMethodType = methodType
      }
  where
    failure = InvalidAnalyzedMethodSignature methodName

statementEnvironments :: ModulePath -> Map ResolvedName CoreBinderId -> [Statement 'Resolved] -> [(Map ResolvedName CoreBinderId, Statement 'Resolved)]
statementEnvironments modulePath outerBinders statements =
  [ (binders, statement)
  | (_, binders, statement) <- statementEnvironmentsByPath (const modulePath) outerBinders statements
  ]

statementEnvironmentsByPath ::
  (Int -> ModulePath) ->
  Map ResolvedName CoreBinderId ->
  [Statement 'Resolved] ->
  [(ModulePath, Map ResolvedName CoreBinderId, Statement 'Resolved)]
statementEnvironmentsByPath pathForIndex outerBinders statements =
  snd (mapAccumL step outerBinders indexedStatements)
  where
    indexedStatements = zip [0 ..] statements
    bindingNodes =
      Map.fromList
        [ (statementIndex, (name, node))
        | (statementIndex, SLet node name _) <- indexedStatements
        ]
    recursiveGroups =
      inferRecursiveGroupsOrdered (Map.keysSet outerBinders) indexedStatements

    step visibleBinders (statementIndex, statement) =
      (publishStatementBinders statementPath visibleBinders statement, (statementPath, definitionBinders, statement))
      where
        statementPath = pathForIndex statementIndex
        definitionBinders =
          foldl
            (\acc peerIndex -> maybe acc (\(name, node) -> insertBinder (pathForIndex peerIndex) name node acc) (Map.lookup peerIndex bindingNodes))
            visibleBinders
            (Map.findWithDefault [] statementIndex recursiveGroups)

    publishStatementBinders statementPath bindings statement =
      case statement of
        SLet node name _ -> insertBinder statementPath name node bindings
        SData _ _ _ constructors ->
          foldl
            (\acc (DataConstructor node name _) -> insertBinder statementPath name node acc)
            bindings
            constructors
        _ -> bindings

extendPatternBinders :: ModulePath -> Map ResolvedName CoreBinderId -> Pattern 'Resolved -> Map ResolvedName CoreBinderId
extendPatternBinders modulePath = go
  where
    go bindings pattern =
      case pattern of
        PVariable node name -> insertBinder modulePath name node bindings
        PConstructor _ _ patterns -> foldl go bindings patterns
        PList _ patterns -> foldl go bindings patterns
        PConsList _ headPattern tailPattern -> go (go bindings headPattern) tailPattern
        PTuple _ patterns -> foldl go bindings patterns
        PAs node name nested -> insertBinder modulePath name node (go bindings nested)
        POr _ alternatives -> foldl go bindings alternatives
        _ -> bindings

insertBinder :: ModulePath -> ResolvedName -> CoreNode 'Resolved (sort :: CoreSort) -> Map ResolvedName CoreBinderId -> Map ResolvedName CoreBinderId
insertBinder modulePath name node =
  Map.insert name (CoreBinderId (modulePath, coreNodeId node))
