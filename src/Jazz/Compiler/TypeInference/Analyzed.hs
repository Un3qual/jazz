{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

-- | The single checked Resolved-to-Analyzed reconstruction. Inference records
-- facts in source-unit-local maps; this traversal consumes every entry through
-- the original node identity and span.
module Jazz.Compiler.TypeInference.Analyzed
  ( attachAnalyzedExpression,
    attachAnalyzedStatementFacts,
    projectAnalyzedCapabilityFacts,
  )
where

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
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
import Jazz.Compiler.CapabilityFacts (ConcreteImplFact (..))
import Jazz.Compiler.ModuleIdentity (ModulePath)
import Jazz.Compiler.Name (ResolvedName)
import Jazz.Compiler.SemanticFacts
  ( AnalyzedCapabilityFacts (..),
    AnalyzedConcreteImplFact (..),
    AnalyzedMethodSignature (..),
    AnalyzedNumericConstraint (..),
    AnalyzedPrimitiveConstraint (..),
    AnalyzedScheme (..),
    AnalyzedSchemeConstraint (..),
    CapabilityId (..),
    CoreBinderId (..),
    CoreNodeId,
    EvidenceReference (..),
    ExpressionFacts (..),
    PatternFacts (..),
    RuntimeObligation (..),
    RuntimePlan (..),
    SemanticFactInvariantFailure (..),
    SemanticInstantiation (..),
    NumericTarget (..),
    StatementFacts (..),
  )
import qualified Jazz.Compiler.TypeInference.Signature as Signature
import Jazz.Compiler.TypeInference.Pattern (instantiateConstructorBinding)
import Jazz.Compiler.TypeInference.Solver (resolveType)
import Jazz.Compiler.TypeInference.State
  ( ExpressionEvidenceSeed (..),
    InferState,
    inferExpressionEvidenceSeeds,
    inferExpressionFactTypes,
    inferFactInvariantFailures,
    inferPatternFactSeeds,
    inferStatementFactSeeds,
  )
import Jazz.Compiler.TypeInference.Types
  ( ClassMethodType (..),
    ExpressionType,
    ImplMethodType (..),
    IntegerLiteralRange (..),
    NumericConstraint (..),
    SemanticType (..),
    ScopeCapabilityFacts (..),
    TypeBinding (..),
    TypeScheme (..),
    TypeSchemeConstraint (..),
    TypeSchemePrimitiveConstraint (..),
    quantifiedVariablesOrderedList,
  )
import Jazz.Compiler.TypeInference.TypeOps (freeTypeVariables)

data Attachment value = Attachment !(Seq SemanticFactInvariantFailure) (Maybe value)

instance Functor Attachment where
  fmap project (Attachment failures value) = Attachment failures (fmap project value)

instance Applicative Attachment where
  pure = Attachment Seq.empty . Just
  Attachment leftFailures maybeFunction <*> Attachment rightFailures maybeValue =
    Attachment
      (leftFailures <> rightFailures)
      (maybeFunction <*> maybeValue)

attachAnalyzedExpression ::
  ModulePath ->
  Map ResolvedName CoreBinderId ->
  InferState ->
  Expr 'Resolved ->
  Either (NonEmpty SemanticFactInvariantFailure) (Expr 'Analyzed)
attachAnalyzedExpression modulePath importedBinders state expression =
  case NonEmpty.nonEmpty (inferFactInvariantFailures state <> foldFailures) of
    Just failures -> Left failures
    Nothing ->
      case maybeAnalyzed of
        Just analyzed -> Right analyzed
        Nothing ->
          Left
            ( NonEmpty.singleton
                (MissingExpressionFacts (coreNodeId (expressionNode expression)))
            )
  where
    Attachment attachmentFailures maybeAnalyzed =
      attachExpr modulePath state (binderIndex modulePath importedBinders expression) expression
    foldFailures = foldr (:) [] attachmentFailures

missing :: SemanticFactInvariantFailure -> Attachment value
missing failure = Attachment (Seq.singleton failure) Nothing

attachExpr :: ModulePath -> InferState -> Map ResolvedName (Maybe CoreBinderId) -> Expr 'Resolved -> Attachment (Expr 'Analyzed)
attachExpr modulePath state binders expression =
  case expression of
    ELit node literal -> ELit <$> attachNode expression node <*> pure literal
    EVar node name -> EVar <$> attachNode expression node <*> pure name
    ELambda node name body -> ELambda <$> attachNode expression node <*> pure name <*> recur body
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
    EBlock node statements -> EBlock <$> attachNode expression node <*> traverse attachStatement statements
  where
    recur = attachExpr modulePath state binders
    attachNode = attachExpressionNode state binders
    attachPattern = attachPatternNode state
    attachStatement = attachStatementNode modulePath state binders
    attachArm (CaseArm armNode pattern guard body) =
      CaseArm
        <$> attachNode body armNode
        <*> attachPattern pattern
        <*> traverse recur guard
        <*> recur body

attachExpressionNode :: InferState -> Map ResolvedName (Maybe CoreBinderId) -> Expr 'Resolved -> CoreNode 'Resolved 'ExpressionSort -> Attachment (CoreNode 'Analyzed 'ExpressionSort)
attachExpressionNode state binders expression (CoreNode nodeId spanValue ()) =
  case Map.lookup nodeId (inferExpressionFactTypes state) of
    Nothing -> missing (MissingExpressionFacts nodeId)
    Just inferredType ->
      let semanticType = resolveType state inferredType
          instantiations = explicitInstantiations state binders expression
          evidence = expressionEvidenceFacts state nodeId
          evidenceObligations =
            maybe Seq.empty (Seq.singleton . SupplyEvidence) (NonEmpty.nonEmpty evidence)
          runtimePlan =
            RuntimePlan
              ( foldMap (Seq.singleton . InstantiateTypes . instantiatedTypes) instantiations
                  <> evidenceObligations
                  <> numericLiteralObligations expression semanticType
                  <> Seq.singleton (ConstrainResult semanticType)
              )
       in pure
            ( CoreNode
                nodeId
                spanValue
                ExpressionFacts
                  { expressionSemanticType = semanticType,
                    expressionInstantiations = instantiations,
                    expressionEvidence = evidence,
                    expressionRuntimePlan = runtimePlan
                  }
            )

explicitInstantiations :: InferState -> Map ResolvedName (Maybe CoreBinderId) -> Expr 'Resolved -> [SemanticInstantiation]
explicitInstantiations state binders expression =
  case expression of
    ETypeApplication _ function _ signatureType ->
      case (referencedBinder binders function, Signature.signatureTypeToExpressionType state Map.empty signatureType) of
        (Just binder, Right argumentType) ->
          [SemanticInstantiation binder (NonEmpty.singleton (resolveType state argumentType))]
        _ -> []
    _ -> []

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

numericLiteralObligations :: Expr 'Resolved -> ExpressionType -> Seq RuntimeObligation
numericLiteralObligations expression expressionType =
  case expression of
    ELit _ (LInt _) ->
      case expressionType of
        SemanticInt -> Seq.singleton (SpecializeNumericLiteral DefaultIntegerTarget)
        SemanticNumeric numericType -> Seq.singleton (SpecializeNumericLiteral (ConcreteNumericTarget numericType))
        _ -> Seq.empty
    ELit _ (LFloat _ _ _) ->
      case expressionType of
        SemanticNumeric numericType -> Seq.singleton (SpecializeNumericLiteral (ConcreteNumericTarget numericType))
        _ -> Seq.empty
    _ -> Seq.empty

referencedBinder :: Map ResolvedName (Maybe CoreBinderId) -> Expr 'Resolved -> Maybe CoreBinderId
referencedBinder binders expression =
  case expression of
    EVar _ name -> Map.lookup name binders >>= id
    ETypeApplication _ function _ _ -> referencedBinder binders function
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

attachStatementNode :: ModulePath -> InferState -> Map ResolvedName (Maybe CoreBinderId) -> Statement 'Resolved -> Attachment (Statement 'Analyzed)
attachStatementNode modulePath state binders statement =
  case statement of
    SLet node name value -> SLet <$> facts node <*> pure name <*> recur value
    SSignature node name signature -> SSignature <$> facts node <*> pure name <*> pure signature
    SData node name parameters constructors -> SData <$> facts node <*> pure name <*> pure parameters <*> traverse attachConstructor constructors
    SClass node name parameters methods -> SClass <$> facts node <*> pure name <*> pure parameters <*> traverse attachClassMethod methods
    SImpl node name arguments methods -> SImpl <$> facts node <*> pure name <*> pure arguments <*> traverse attachImplMethod methods
    SModule node path -> SModule <$> facts node <*> pure path
    SImport node path alias names -> SImport <$> facts node <*> pure path <*> pure alias <*> pure names
    SExpr node value -> SExpr <$> facts node <*> recur value
  where
    recur = attachExpr modulePath state binders
    facts = attachStatementFacts modulePath state
    attachConstructor (DataConstructor node name arguments) = DataConstructor <$> facts node <*> pure name <*> pure arguments
    attachClassMethod (ClassMethodSignature node name signature) = ClassMethodSignature <$> facts node <*> pure name <*> pure signature
    attachImplMethod (ImplMethod node name body) = ImplMethod <$> facts node <*> pure name <*> recur body

attachStatementFacts :: ModulePath -> InferState -> CoreNode 'Resolved 'StatementSort -> Attachment (CoreNode 'Analyzed 'StatementSort)
attachStatementFacts modulePath state (CoreNode nodeId spanValue ()) =
  CoreNode nodeId spanValue <$> projectStatementFacts modulePath state nodeId

attachAnalyzedStatementFacts ::
  ModulePath ->
  InferState ->
  [CoreNodeId] ->
  Either (NonEmpty SemanticFactInvariantFailure) (Map CoreNodeId StatementFacts)
attachAnalyzedStatementFacts modulePath state nodeIds =
  case traverse (\nodeId -> (,) nodeId <$> projectStatementFacts modulePath state nodeId) nodeIds of
    Attachment failures maybeFacts ->
      case NonEmpty.nonEmpty (foldr (:) [] failures) of
        Just invariantFailures -> Left invariantFailures
        Nothing ->
          case maybeFacts of
            Just facts -> Right (Map.fromList facts)
            Nothing -> Right Map.empty

projectStatementFacts :: ModulePath -> InferState -> CoreNodeId -> Attachment StatementFacts
projectStatementFacts modulePath state nodeId =
  case Map.lookup nodeId (inferStatementFactSeeds state) of
    Nothing -> missing (MissingStatementFacts nodeId)
    Just (bindings, declarationFact) ->
      let binderId = CoreBinderId (modulePath, nodeId)
          schemes =
            Map.fromList
              [ (binderId, scheme)
              | (_, binding) <- bindings,
                Just scheme <- [projectTypeBinding state binding]
              ]
       in pure
            StatementFacts
              { statementBinderIds = [binderId | not (null bindings)],
                statementGeneralizedSchemes = schemes,
                statementDeclarationFact = declarationFact
              }

projectTypeBinding :: InferState -> TypeBinding -> Maybe AnalyzedScheme
projectTypeBinding state binding =
  case binding of
    PlainTypeBinding expressionType -> Just (monomorphicScheme state expressionType)
    SchemeTypeBinding scheme -> Just (projectScheme state scheme)
    OperatorAliasSchemeTypeBinding _ scheme -> Just (projectScheme state scheme)
    ConstructorTypeBinding {} -> projectConstructorBinding state binding
    BuiltinAliasTypeBinding {} -> Nothing
    BuiltinOperatorAliasTypeBinding {} -> Nothing

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
        analyzedSchemeDefiningCapabilities = emptyAnalyzedCapabilityFacts,
        analyzedSchemeType = resolvedConstructorType
      }

monomorphicScheme :: InferState -> ExpressionType -> AnalyzedScheme
monomorphicScheme state expressionType =
  AnalyzedScheme
    { analyzedSchemeVariables = [],
      analyzedSchemeConstraints = [],
      analyzedSchemePrimitiveConstraints = [],
      analyzedSchemeDefiningCapabilities = emptyAnalyzedCapabilityFacts,
      analyzedSchemeType = resolveType state expressionType
    }

projectScheme :: InferState -> TypeScheme -> AnalyzedScheme
projectScheme state scheme =
  AnalyzedScheme
    { analyzedSchemeVariables = quantifiedVariablesOrderedList (schemeQuantifiedVariables scheme),
      analyzedSchemeConstraints = map (projectSchemeConstraint state) (schemeClassConstraints scheme),
      analyzedSchemePrimitiveConstraints = map (projectPrimitiveConstraint state) (schemePrimitiveConstraints scheme),
      analyzedSchemeDefiningCapabilities = projectAnalyzedCapabilityFacts state (schemeDefiningCapabilities scheme),
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

projectAnalyzedCapabilityFacts :: InferState -> ScopeCapabilityFacts -> AnalyzedCapabilityFacts
projectAnalyzedCapabilityFacts state facts =
  AnalyzedCapabilityFacts
    { analyzedClassArities = scopeClassFacts facts,
      analyzedGeneratedEqualityClasses = scopeGeneratedEqualityClassFacts facts,
      analyzedConcreteImplementations =
        Set.fromList
          (mapMaybe projectConcreteImpl (Set.toList (scopeConcreteImplFacts facts))),
      analyzedClassMethodSignatures = Map.mapMaybe projectClassMethod (scopeClassMethodSignatures facts),
      analyzedConcreteImplMethods = Map.map (mapMaybe projectImplMethod) (scopeConcreteImplMethods facts)
    }
  where
    projectConcreteImpl (ConcreteImplFact capabilityName signatureType) =
      case Signature.signatureTypeToExpressionType state Map.empty signatureType of
        Left _ -> Nothing
        Right expressionType -> Just (AnalyzedConcreteImplFact (CapabilityId capabilityName) (resolveType state expressionType))
    projectClassMethod (ClassMethodType parameter payload) =
      case Signature.signaturePayloadToSignatureType payload state of
        (Nothing, _) -> Nothing
        (Just payloadType, _) ->
          Just
            AnalyzedMethodSignature
              { analyzedMethodClassParameter = parameter,
                analyzedMethodConstraints = map (projectSchemeConstraint state) (Signature.signaturePayloadExplicitConstraints payloadType),
                analyzedMethodType = resolveType state (Signature.signaturePayloadDeclaredType payloadType)
              }
    projectImplMethod (ImplMethodType signatureType) =
      either (const Nothing) (Just . resolveType state) (Signature.signatureTypeToExpressionType state Map.empty signatureType)

emptyAnalyzedCapabilityFacts :: AnalyzedCapabilityFacts
emptyAnalyzedCapabilityFacts =
  AnalyzedCapabilityFacts Map.empty Set.empty Set.empty Map.empty Map.empty

binderIndex :: ModulePath -> Map ResolvedName CoreBinderId -> Expr 'Resolved -> Map ResolvedName (Maybe CoreBinderId)
binderIndex modulePath importedBinders =
  foldExpressionBindings (Map.map Just importedBinders)
  where
    foldExpressionBindings :: Map ResolvedName (Maybe CoreBinderId) -> Expr 'Resolved -> Map ResolvedName (Maybe CoreBinderId)
    foldExpressionBindings bindings expression =
      case expression of
        ELambda node name body -> insert name node (foldExpressionBindings bindings body)
        EList _ values -> foldl foldExpressionBindings bindings values
        ETuple _ values -> foldl foldExpressionBindings bindings values
        EApply _ function argument -> foldExpressionBindings (foldExpressionBindings bindings function) argument
        ETypeApplication _ function _ _ -> foldExpressionBindings bindings function
        EIf _ condition whenTrue whenFalse -> foldExpressionBindings (foldExpressionBindings (foldExpressionBindings bindings condition) whenTrue) whenFalse
        EPatternCase _ scrutinee arms -> foldl foldArmBindings (foldExpressionBindings bindings scrutinee) arms
        EBinary _ _ left right -> foldExpressionBindings (foldExpressionBindings bindings left) right
        ESectionLeft _ left _ -> foldExpressionBindings bindings left
        ESectionRight _ _ right -> foldExpressionBindings bindings right
        EBlock _ statements -> foldl foldStatementBindings bindings statements
        _ -> bindings
    foldArmBindings :: Map ResolvedName (Maybe CoreBinderId) -> CaseArm 'Resolved -> Map ResolvedName (Maybe CoreBinderId)
    foldArmBindings bindings (CaseArm _ pattern guard body) =
      foldExpressionBindings (maybe bindings (foldExpressionBindings bindings) guard) body
        <> foldPatternBindings Map.empty pattern
    foldStatementBindings :: Map ResolvedName (Maybe CoreBinderId) -> Statement 'Resolved -> Map ResolvedName (Maybe CoreBinderId)
    foldStatementBindings bindings statement =
      case statement of
        SLet node name value -> insert name node (foldExpressionBindings bindings value)
        SData _ _ _ constructors -> foldl (\acc (DataConstructor node name _) -> insert name node acc) bindings constructors
        SImpl _ _ _ methods -> foldl (\acc (ImplMethod _ _ body) -> foldExpressionBindings acc body) bindings methods
        SExpr _ value -> foldExpressionBindings bindings value
        _ -> bindings
    foldPatternBindings :: Map ResolvedName (Maybe CoreBinderId) -> Pattern 'Resolved -> Map ResolvedName (Maybe CoreBinderId)
    foldPatternBindings bindings pattern =
      case pattern of
        PVariable node name -> insert name node bindings
        PConstructor _ _ patterns -> foldl foldPatternBindings bindings patterns
        PList _ patterns -> foldl foldPatternBindings bindings patterns
        PConsList _ headPattern tailPattern -> foldPatternBindings (foldPatternBindings bindings headPattern) tailPattern
        PTuple _ patterns -> foldl foldPatternBindings bindings patterns
        PAs node name nested -> insert name node (foldPatternBindings bindings nested)
        POr _ alternatives -> foldl foldPatternBindings bindings alternatives
        _ -> bindings
    insert :: ResolvedName -> CoreNode 'Resolved (sort :: CoreSort) -> Map ResolvedName (Maybe CoreBinderId) -> Map ResolvedName (Maybe CoreBinderId)
    insert name node = Map.insertWith collide name (Just (CoreBinderId (modulePath, coreNodeId node)))
    collide :: Maybe CoreBinderId -> Maybe CoreBinderId -> Maybe CoreBinderId
    collide _ _ = Nothing

expressionNode :: Expr phase -> CoreNode phase 'ExpressionSort
expressionNode expression =
  case expression of
    ELit node _ -> node
    EVar node _ -> node
    ELambda node _ _ -> node
    EOperatorValue node _ -> node
    EList node _ -> node
    ETuple node _ -> node
    EApply node _ _ -> node
    ETypeApplication node _ _ _ -> node
    EIf node _ _ _ -> node
    EPatternCase node _ _ -> node
    EBinary node _ _ _ -> node
    ESectionLeft node _ _ -> node
    ESectionRight node _ _ -> node
    EBlock node _ -> node
