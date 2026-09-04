{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Jazz.Compiler.Semantics.BindingSignature.InferenceOwnershipTests
  ( inferenceOwnershipTests,
  )
where

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Text (Text)
import Jazz.Compiler.AST
  ( CorePhase (Resolved),
    Expr (..),
    Statement (..),
  )
import Jazz.Compiler.BuiltinCatalog
  ( BuiltinResolutionMode (ResolveKernelOnly),
  )
import Jazz.Compiler.Name
  ( NameNamespace (CapabilityNamespace, TypeNamespace, ValueNamespace),
    ResolvedName,
    mkIdentifier,
    resolvedLocalName,
  )
import Jazz.Compiler.RecursiveBindings
  ( prepareRecursiveScope,
  )
import Jazz.Compiler.SemanticFacts (StatementDeclarationFact (SignatureDeclaration))
import Jazz.Compiler.Semantics.BindingSignature.Shared (resolvedProgram)
import Jazz.Compiler.TypeInference.Capabilities
  ( typeSchemeReferencedCapabilityFacts,
  )
import Jazz.Compiler.TypeInference.Operator
  ( builtinSectionOperatorSymbol,
    hasOperatorRule,
  )
import qualified Jazz.Compiler.TypeInference.Scope as TypeInferenceScope
import Jazz.Compiler.TypeInference.Signature
  ( SignaturePayloadType (..),
    duplicateConstraintName,
    expressionTypeToRuntimeHint,
    expressionTypeToRuntimeTemplate,
    signaturePayloadToSignatureType,
  )
import Jazz.Compiler.TypeInference.Solver
  ( addNumericTypeVarConstraint,
    addStrictEqualityTypeVarConstraint,
    applySubstitution,
    bindTypeVar,
    freshTypeVar,
    resolveType,
    unifyTypes,
  )
import Jazz.Compiler.TypeInference.State
  ( DeclarationState (..),
    DeferredExplicitConstraint (..),
    InferState (..),
    InferenceOutput (..),
    ModuleInferenceState (..),
    SolverState (..),
    inferClassFacts,
    inferCurrentModulePath,
    inferDeferredExplicitConstraintCount,
    inferDeferredExplicitConstraints,
    inferErrorCount,
    inferInferredClassConstraintCount,
    inferInferredClassConstraints,
    inferNextTypeVar,
    inferNumericVars,
    inferStatementFactSeeds,
    inferStrictEqualityVars,
    initialInferState,
    modifyDeclarationState,
    modifyInferenceOutput,
    modifyModuleInferenceState,
  )
import Jazz.Compiler.TypeInference.Traversal
  ( InferExprWithModeFn,
    InferenceMode (..),
  )
import Jazz.Compiler.TypeInference.TypeOps
  ( dedupeTypeSchemeConstraints,
    freeTypeVariables,
    freeTypeVariablesInTypeSchemeConstraints,
    freeTypeVariablesInTypeSchemePrimitiveConstraints,
    instantiateTypeSchemeConstraint,
    instantiateTypeSchemePrimitiveConstraint,
    replaceTypeVariables,
  )
import Jazz.Compiler.TypeInference.Types
  ( ExpressionType,
    NumericConstraint (..),
    ScopeCapabilityFacts,
    SemanticType (..),
    TypeBinding (..),
    TypeSchemeConstraint (..),
    TypeSchemePrimitiveConstraint (..),
    emptyScopeCapabilityFacts,
    schemeResultType,
  )
import Jazz.Compiler.TypeRepresentation
  ( InferenceVariable,
    pattern SignatureConstraint,
    pattern SignatureType,
    pattern TypeBool,
    pattern TypeFunction,
    pattern TypeInt,
    pattern TypeList,
    pattern TypeName,
    pattern TypeVariable,
  )
import Jazz.TestHarness
  ( NamedTest,
    assertEqual,
    failTest,
  )

inferenceOwnershipTests :: [NamedTest]
inferenceOwnershipTests =
  [ ("runtime templates accept only mapped quantified variables", testRuntimeTemplatesAcceptOnlyMappedQuantifiedVariables),
    ("runtime hint child failures propagate through lists and functions", testRuntimeHintChildFailuresPropagate),
    ("runtime template child failures propagate through lists and functions", testRuntimeTemplateChildFailuresPropagate),
    ("duplicate constraints report the first repeated name", testDuplicateConstraintsReportFirstRepeatedName),
    ("state record modifiers update only their owned partitions", testStateRecordModifiers),
    ("inference output preserves constraint order and explicit cursors", testInferenceOutputConstraintCursors),
    ("solver resolves long substitution chains and compound types", testSolverResolvesLongSubstitutionChains),
    ("unification path-compresses traversed substitution chains", testUnificationPathCompressesSubstitutionChains),
    ("solver preserves occurs, rigid, and numeric constraints", testSolverPreservesBindingConstraints),
    ("scheme constraint deduplication preserves last-occurrence order", testSchemeConstraintDeduplicationOrder),
    ("empty scheme constraints do not traverse capability facts", testEmptySchemeConstraintsSkipCapabilityFacts),
    ("type operations collect recursive free variables", testTypeOpsCollectRecursiveFreeVariables),
    ("type operations collect constraint free variables", testTypeOpsCollectConstraintFreeVariables),
    ("type operations replace recursive type variables", testTypeOpsReplaceRecursiveTypeVariables),
    ("type operations instantiate class and primitive constraints", testTypeOpsInstantiateConstraints),
    ("signature payload normalization allocates ordered variables", testSignaturePayloadNormalizationAllocatesOrderedVariables),
    ("failed signature payload normalization rolls back state", testFailedSignaturePayloadNormalizationRollsBackState),
    ("production scope elaborates each signature once in source order", testProductionScopeElaboratesSignatureOnce),
    ("prepared inference scopes rederive facts for current outer bindings", testPreparedInferenceScopeRederivesForOuterBindings),
    ("recursive previews do not expose speculative solver state to intervening bindings", testRecursivePreviewSolverStateIsTransactional),
    ("recursive previews refresh after semantic solver changes", testRecursivePreviewRefreshesAfterSolverChange),
    ("recursive previews refresh after numeric-constraint changes", testRecursivePreviewRefreshesAfterNumericConstraintChange),
    ("recursive previews refresh after strict-equality-constraint changes", testRecursivePreviewRefreshesAfterStrictEqualityConstraintChange),
    ("recursive previews are reused at an unchanged group frontier", testRecursivePreviewReuseAtSameFrontier),
    ("operator rule presence remains distinct from section support", testOperatorRulePresenceAndSectionSupport)
  ]

testRuntimeTemplatesAcceptOnlyMappedQuantifiedVariables :: IO ()
testRuntimeTemplatesAcceptOnlyMappedQuantifiedVariables = do
  let variableName = typeName "a"
  assertEqual
    "mapped variable template"
    (Just (TypeVariable variableName))
    (expressionTypeToRuntimeTemplate (Map.singleton 7 variableName) (SemanticVariable 7))
  assertEqual
    "unmapped variable template"
    Nothing
    (expressionTypeToRuntimeTemplate Map.empty (SemanticVariable 7))

testRuntimeHintChildFailuresPropagate :: IO ()
testRuntimeHintChildFailuresPropagate = do
  assertEqual
    "list child failure"
    Nothing
    (expressionTypeToRuntimeHint (SemanticList (SemanticVariable 1)))
  assertEqual
    "function child failure"
    Nothing
    (expressionTypeToRuntimeHint (SemanticFunction SemanticInt (SemanticVariable 1)))

testRuntimeTemplateChildFailuresPropagate :: IO ()
testRuntimeTemplateChildFailuresPropagate = do
  let unmappedType = SemanticVariable 1
  assertEqual
    "list child failure"
    Nothing
    (expressionTypeToRuntimeTemplate Map.empty (SemanticList unmappedType))
  assertEqual
    "function child failure"
    Nothing
    (expressionTypeToRuntimeTemplate Map.empty (SemanticFunction SemanticInt unmappedType))

testDuplicateConstraintsReportFirstRepeatedName :: IO ()
testDuplicateConstraintsReportFirstRepeatedName =
  assertEqual
    "first duplicate"
    (Just "Eq")
    ( duplicateConstraintName
        [ SignatureConstraint (capabilityName "Eq") [TypeInt],
          SignatureConstraint (capabilityName "Ord") [TypeInt],
          SignatureConstraint (capabilityName "Eq") [TypeBool],
          SignatureConstraint (capabilityName "Ord") [TypeBool]
        ]
    )

testStateRecordModifiers :: IO ()
testStateRecordModifiers = do
  assertEqual "declaration update" (Map.singleton "Eq" 1) (inferClassFacts updatedState)
  assertEqual "module update" (Just ["App", "Main"]) (inferCurrentModulePath updatedState)
  assertEqual "output update" 3 (inferErrorCount updatedState)
  where
    updatedState =
      modifyInferenceOutput
        (\output -> output {outputErrorCount = 3})
        ( modifyModuleInferenceState
            (\moduleState -> moduleState {inferenceModulePath = Just ["App", "Main"]})
            ( modifyDeclarationState
                (\declarations -> declarations {declarationClassFacts = Map.singleton "Eq" 1})
                initialInferState
            )
        )

testInferenceOutputConstraintCursors :: IO ()
testInferenceOutputConstraintCursors = do
  assertEqual
    "chronological deferred constraints"
    [firstDeferred, secondDeferred]
    (inferDeferredExplicitConstraints stateWithConstraints)
  assertEqual
    "deferred cursor"
    2
    (inferDeferredExplicitConstraintCount stateWithConstraints)
  assertEqual
    "newest-first inferred constraints"
    [secondInferred, firstInferred]
    (inferInferredClassConstraints stateWithConstraints)
  assertEqual
    "inferred cursor"
    2
    (inferInferredClassConstraintCount stateWithConstraints)
  where
    stateWithConstraints =
      modifyInferenceOutput
        ( \output ->
            output
              { outputDeferredConstraints = Seq.fromList [firstDeferred, secondDeferred],
                outputInferredConstraints = [secondInferred, firstInferred],
                outputInferredConstraintCount = 2
              }
        )
        initialInferState
    firstDeferred = deferredConstraint "Eq" SemanticInt
    secondDeferred = deferredConstraint "Show" SemanticText
    firstInferred = TypeSchemeInferredConstraint "Eq" SemanticInt
    secondInferred = TypeSchemeMethodConstraint "Show" "Show::show" SemanticText

deferredConstraint :: Text -> ExpressionType -> DeferredExplicitConstraint
deferredConstraint constraintName argumentType =
  DeferredExplicitConstraint
    { deferredConstraintName = constraintName,
      deferredMethodKey = Nothing,
      deferredWasInferred = False,
      deferredArgumentType = argumentType,
      deferredVisibleFacts = emptyFacts,
      deferredStructuralFacts = emptyFacts
    }
  where
    emptyFacts :: ScopeCapabilityFacts
    emptyFacts = emptyScopeCapabilityFacts

testSchemeConstraintDeduplicationOrder :: IO ()
testSchemeConstraintDeduplicationOrder =
  assertEqual
    "stable-last constraint order"
    [middleConstraint, repeatedConstraint]
    (dedupeTypeSchemeConstraints [repeatedConstraint, middleConstraint, repeatedConstraint])
  where
    repeatedConstraint = TypeSchemeConstraint "Eq" (SemanticVariable 0)
    middleConstraint = TypeSchemeInferredConstraint "Ord" (SemanticVariable 1)

testEmptySchemeConstraintsSkipCapabilityFacts :: IO ()
testEmptySchemeConstraintsSkipCapabilityFacts =
  assertEqual
    "empty constraints own no capability facts"
    emptyScopeCapabilityFacts
    (typeSchemeReferencedCapabilityFacts [] (error "empty constraints forced capability facts"))

testTypeOpsCollectRecursiveFreeVariables :: IO ()
testTypeOpsCollectRecursiveFreeVariables =
  assertEqual
    "recursive free variables"
    (Set.fromList [1, 2, 3])
    ( freeTypeVariables
        (SemanticFunction (SemanticList (SemanticVariable 1)) (SemanticTuple [SemanticVariable 2, SemanticList (SemanticVariable 3)]))
    )

testSolverResolvesLongSubstitutionChains :: IO ()
testSolverResolvesLongSubstitutionChains =
  assertEqual
    "resolved compound substitution"
    (SemanticTuple [SemanticList SemanticInt, SemanticFunction SemanticInt SemanticBool])
    ( applySubstitution
        substitution
        (SemanticTuple [SemanticList (SemanticVariable 0), SemanticFunction (SemanticVariable 0) SemanticBool])
    )
  where
    substitution =
      Map.fromList
        ([(typeVar, SemanticVariable (typeVar + 1)) | typeVar <- [0 .. 62]] ++ [(63, SemanticInt)])

testUnificationPathCompressesSubstitutionChains :: IO ()
testUnificationPathCompressesSubstitutionChains =
  case unifyTypes (SemanticVariable 0) SemanticInt chainState of
    Nothing -> failTest "expected chained variable to unify with Int"
    Just nextState -> do
      assertEqual
        "compressed root substitution"
        (Just SemanticInt)
        (Map.lookup 0 (solverSubstitution (inferSolver nextState)))
      assertEqual
        "compressed middle substitution"
        (Just SemanticInt)
        (Map.lookup 1 (solverSubstitution (inferSolver nextState)))
      assertEqual "resolved root type" SemanticInt (resolveType nextState (SemanticVariable 0))
  where
    chainState =
      initialInferState
        { inferSolver =
            (inferSolver initialInferState)
              { solverSubstitution =
                  Map.fromList
                    [ (0, SemanticVariable 1),
                      (1, SemanticVariable 2),
                      (2, SemanticInt)
                    ]
              }
        }

testSolverPreservesBindingConstraints :: IO ()
testSolverPreservesBindingConstraints = do
  case bindTypeVar 0 (SemanticList (SemanticVariable 0)) initialInferState of
    Nothing -> pure ()
    Just _ -> failTest "expected occurs check to reject a recursive type"
  case unifyTypes (SemanticVariable 0) SemanticInt rigidState of
    Nothing -> pure ()
    Just _ -> failTest "expected rigid type variable unification to fail"
  case unifyTypes (SemanticVariable 0) (SemanticVariable 1) numericState of
    Nothing -> failTest "expected constrained variables to unify"
    Just linkedState -> do
      case unifyTypes (SemanticVariable 1) SemanticFloat linkedState of
        Nothing -> pure ()
        Just _ -> failTest "expected integral constraint to reject Float"
      case unifyTypes (SemanticVariable 1) SemanticInt linkedState of
        Nothing -> failTest "expected integral constraint to accept Int"
        Just resolvedState ->
          assertEqual "resolved constrained root" SemanticInt (resolveType resolvedState (SemanticVariable 0))
  where
    rigidState =
      initialInferState
        { inferSolver =
            (inferSolver initialInferState)
              { solverRigidTypeVars = Set.singleton 0
              }
        }
    numericState =
      addNumericTypeVarConstraint 0 IntegralNumericConstraint initialInferState

testTypeOpsCollectConstraintFreeVariables :: IO ()
testTypeOpsCollectConstraintFreeVariables = do
  assertEqual
    "class constraint free variables"
    (Set.fromList [1, 2])
    ( freeTypeVariablesInTypeSchemeConstraints
        [ TypeSchemeConstraint "Eq" (SemanticList (SemanticVariable 1)),
          TypeSchemeMethodConstraint "Show" "Show::show" (SemanticVariable 2)
        ]
    )
  assertEqual
    "primitive constraint free variables"
    (Set.fromList [3, 4])
    ( freeTypeVariablesInTypeSchemePrimitiveConstraints
        [ TypeSchemeNumericConstraint AnyNumericConstraint (SemanticVariable 3),
          TypeSchemeStrictEqualityConstraint (SemanticList (SemanticVariable 4))
        ]
    )

testTypeOpsReplaceRecursiveTypeVariables :: IO ()
testTypeOpsReplaceRecursiveTypeVariables =
  assertEqual
    "recursive replacement"
    (SemanticFunction (SemanticList SemanticInt) (SemanticTuple [SemanticVariable 2, SemanticBool]))
    ( replaceTypeVariables
        (Map.fromList [(1, SemanticInt), (3, SemanticBool)])
        (SemanticFunction (SemanticList (SemanticVariable 1)) (SemanticTuple [SemanticVariable 2, SemanticVariable 3]))
    )

testTypeOpsInstantiateConstraints :: IO ()
testTypeOpsInstantiateConstraints = do
  let replacements = Map.singleton 1 SemanticText
  assertEqual
    "class constraint instantiation"
    (TypeSchemeMethodConstraint "Show" "Show::show" (SemanticList SemanticText))
    ( instantiateTypeSchemeConstraint
        replacements
        (TypeSchemeMethodConstraint "Show" "Show::show" (SemanticList (SemanticVariable 1)))
    )
  assertEqual
    "primitive constraint instantiation"
    (TypeSchemeStrictEqualityConstraint (SemanticFunction SemanticText (SemanticVariable 2)))
    ( instantiateTypeSchemePrimitiveConstraint
        replacements
        (TypeSchemeStrictEqualityConstraint (SemanticFunction (SemanticVariable 1) (SemanticVariable 2)))
    )

testSignaturePayloadNormalizationAllocatesOrderedVariables :: IO ()
testSignaturePayloadNormalizationAllocatesOrderedVariables =
  case signaturePayloadToSignatureType payload initialInferState of
    (Nothing, _) -> failTest "expected signature payload normalization"
    (Just normalized, nextState) -> do
      assertEqual
        "normalized signature type"
        (SemanticFunction (SemanticVariable 0) (SemanticList (SemanticVariable 0)))
        (signaturePayloadDeclaredType normalized)
      assertEqual "normalized constraints" [] (signaturePayloadExplicitConstraints normalized)
      assertEqual "variable order" [0] (signaturePayloadVariableOrder normalized)
      assertEqual "next type variable" 1 (inferNextTypeVar nextState)
  where
    variableName = typeName "a"
    payload = SignatureType (TypeFunction (TypeVariable variableName) (TypeList (TypeVariable variableName)))

testFailedSignaturePayloadNormalizationRollsBackState :: IO ()
testFailedSignaturePayloadNormalizationRollsBackState =
  case signaturePayloadToSignatureType payload initialInferState of
    (Nothing, nextState) -> assertEqual "rollback state" initialInferState nextState
    (Just _, _) -> failTest "expected signature payload normalization failure"
  where
    payload = SignatureType (TypeName (typeName "Missing"))

testProductionScopeElaboratesSignatureOnce :: IO ()
testProductionScopeElaboratesSignatureOnce = do
  assertEqual
    "source-ordered prepared signature"
    [SemanticFunction (SemanticVariable 0) (SemanticVariable 0)]
    [signatureType | (bindings, SignatureDeclaration _) <- Map.elems (inferStatementFactSeeds finalState), (_, binding) <- bindings, signatureType <- case binding of PlainTypeBinding t -> [t]; SchemeTypeBinding scheme -> [schemeResultType scheme]; _ -> []]
  assertEqual "one signature allocation plus one binding seed" 2 (inferNextTypeVar finalState)
  where
    (_, finalState) =
      TypeInferenceScope.inferScopeTypeWithMode
        syntheticProductionInfer
        InferConcreteFunctions
        ResolveKernelOnly
        Map.empty
        initialInferState
        (programStatements (resolvedProgram "identity :: a -> a.\nidentity = \\(item) -> item."))

    syntheticProductionInfer :: InferExprWithModeFn
    syntheticProductionInfer mode _ env state expression =
      case mode of
        InferConcreteFunctions ->
          case expression of
            EVar _ name ->
              case Map.lookup name env of
                Just (PlainTypeBinding expressionType) ->
                  ((Just expressionType), state)
                _ -> (Nothing, state)
            _ -> (Nothing, state)
        InferenceOnly ->
          error "expected production callback invocation"

testPreparedInferenceScopeRederivesForOuterBindings :: IO ()
testPreparedInferenceScopeRederivesForOuterBindings = do
  assertEqual
    "ordinary inference errors"
    0
    (inferErrorCount ordinaryState)
  assertEqual
    "prepared inference errors"
    0
    (inferErrorCount preparedState)
  where
    statements = programStatements (resolvedProgram "self = self.")
    (_, ordinaryState, _) =
      TypeInferenceScope.inferScopeTypeWithModeAndForwardBindings
        syntheticProductionInfer
        InferenceOnly
        ResolveKernelOnly
        Map.empty
        initialInferState
        statements
    (_, preparedState, _) =
      TypeInferenceScope.inferScopeTypeWithModeAndForwardBindingsUsingPreparedScope
        (prepareRecursiveScope (Set.singleton (valueName "self")) statements)
        syntheticProductionInfer
        InferenceOnly
        ResolveKernelOnly
        Map.empty
        initialInferState

    syntheticProductionInfer :: InferExprWithModeFn
    syntheticProductionInfer mode _ env state expression =
      case mode of
        InferenceOnly ->
          case expression of
            EVar _ name ->
              case Map.lookup name env of
                Just (PlainTypeBinding expressionType) ->
                  ((Just expressionType), state)
                _ ->
                  ( Nothing,
                    modifyInferenceOutput
                      (\output -> output {outputErrorCount = outputErrorCount output + 1})
                      state
                  )
            _ -> (Nothing, state)
        InferConcreteFunctions ->
          error "expected inference-only callback invocation"

testRecursivePreviewSolverStateIsTransactional :: IO ()
testRecursivePreviewSolverStateIsTransactional =
  assertEqual
    "intervening binding error count"
    0
    (inferErrorCount finalState)
  where
    (_, finalState) =
      TypeInferenceScope.inferScopeType
        syntheticPreviewInfer
        ResolveKernelOnly
        Map.empty
        initialInferState
        (programStatements (resolvedProgram "left = right.\nearly = probe.\nright = left."))

    syntheticPreviewInfer :: InferExprWithModeFn
    syntheticPreviewInfer mode _ _ state expression =
      case expression of
        EVar _ name
          | name == valueName "left" ->
              inferenceOnlyResult
                mode
                (Just SemanticBool)
                state
                  { inferSolver =
                      (inferSolver state)
                        { solverSubstitution =
                            Map.insert previewSentinel SemanticInt (solverSubstitution (inferSolver state))
                        }
                  }
        EVar _ name
          | name == valueName "probe",
            Map.member previewSentinel (solverSubstitution (inferSolver state)) ->
              inferenceOnlyResult
                mode
                (Just SemanticBool)
                ( modifyInferenceOutput
                    (\output -> output {outputErrorCount = outputErrorCount output + 1})
                    state
                )
        _ -> inferenceOnlyResult mode (Just SemanticBool) state

    previewSentinel = 1000000

testRecursivePreviewRefreshesAfterSolverChange :: IO ()
testRecursivePreviewRefreshesAfterSolverChange =
  assertEqual
    "intervening binding observes the refreshed recursive scheme"
    0
    (inferErrorCount finalState)
  where
    (_, finalState) =
      TypeInferenceScope.inferScopeType
        syntheticPreviewInfer
        ResolveKernelOnly
        (Map.singleton (valueName "shared") (PlainTypeBinding (SemanticVariable sharedTypeVar)))
        initialInferState
        (programStatements (resolvedProgram "left = right.\nadvance = advanceSolver.\nprobe = probeLeft.\nright = left shared."))

    syntheticPreviewInfer :: InferExprWithModeFn
    syntheticPreviewInfer mode _ env state expression =
      case expression of
        EVar _ name
          | name == valueName "right" ->
              inferenceOnlyResult mode (bindingType =<< Map.lookup (valueName "right") env) state
        EApply _ (EVar _ functionName) (EVar _ argumentName)
          | functionName == valueName "left",
            argumentName == valueName "shared" ->
              inferenceOnlyResult
                mode
                (resolveType state <$> (bindingType =<< Map.lookup (valueName "shared") env))
                state
        EVar _ name
          | name == valueName "advanceSolver" ->
              inferenceOnlyResult
                mode
                (Just SemanticBool)
                ( case bindTypeVar sharedTypeVar SemanticBool state of
                    Just nextState -> nextState
                    Nothing -> state
                )
        EVar _ name
          | name == valueName "probeLeft" ->
              inferenceOnlyResult
                mode
                (Just SemanticBool)
                ( case Map.lookup (valueName "left") env of
                    Just (PlainTypeBinding SemanticBool) -> state
                    _ ->
                      modifyInferenceOutput
                        (\output -> output {outputErrorCount = outputErrorCount output + 1})
                        state
                )
        _ -> inferenceOnlyResult mode (Just SemanticBool) state

    bindingType binding =
      case binding of
        PlainTypeBinding expressionType -> Just expressionType
        _ -> Nothing

    sharedTypeVar = 1000000

testRecursivePreviewRefreshesAfterNumericConstraintChange :: IO ()
testRecursivePreviewRefreshesAfterNumericConstraintChange =
  assertRecursivePreviewRefreshesAfterConstraintChange
    "numeric constraint"
    (\typeVar -> addNumericTypeVarConstraint typeVar AnyNumericConstraint)
    (\typeVar -> Map.member typeVar . inferNumericVars)

testRecursivePreviewRefreshesAfterStrictEqualityConstraintChange :: IO ()
testRecursivePreviewRefreshesAfterStrictEqualityConstraintChange =
  assertRecursivePreviewRefreshesAfterConstraintChange
    "strict-equality constraint"
    addStrictEqualityTypeVarConstraint
    (\typeVar -> Set.member typeVar . inferStrictEqualityVars)

assertRecursivePreviewRefreshesAfterConstraintChange ::
  Text ->
  (InferenceVariable -> InferState -> InferState) ->
  (InferenceVariable -> InferState -> Bool) ->
  IO ()
assertRecursivePreviewRefreshesAfterConstraintChange label addConstraint hasConstraint =
  assertEqual
    (label <> " refreshes the exposed recursive binding")
    0
    (inferErrorCount finalState)
  where
    (_, finalState) =
      TypeInferenceScope.inferScopeType
        syntheticPreviewInfer
        ResolveKernelOnly
        (Map.singleton (valueName "shared") (PlainTypeBinding (SemanticVariable sharedTypeVar)))
        initialInferState
        (programStatements (resolvedProgram "left = right.\nadvance = advanceConstraint.\nprobe = probeLeft.\nright = left constraintSensitive."))

    syntheticPreviewInfer :: InferExprWithModeFn
    syntheticPreviewInfer mode _ env state expression =
      case expression of
        EVar _ name
          | name == valueName "right" ->
              inferenceOnlyResult mode (bindingType =<< Map.lookup (valueName "right") env) state
        EApply _ (EVar _ functionName) (EVar _ argumentName)
          | functionName == valueName "left",
            argumentName == valueName "constraintSensitive" ->
              inferenceOnlyResult
                mode
                ( Just
                    ( if hasConstraint sharedTypeVar state
                        then SemanticBool
                        else SemanticVariable sharedTypeVar
                    )
                )
                state
        EVar _ name
          | name == valueName "advanceConstraint" ->
              inferenceOnlyResult mode (Just SemanticBool) (addConstraint sharedTypeVar state)
        EVar _ name
          | name == valueName "probeLeft" ->
              inferenceOnlyResult
                mode
                (Just SemanticBool)
                ( case Map.lookup (valueName "left") env of
                    Just (PlainTypeBinding SemanticBool) -> state
                    _ ->
                      modifyInferenceOutput
                        (\output -> output {outputErrorCount = outputErrorCount output + 1})
                        state
                )
        _ -> inferenceOnlyResult mode (Just SemanticBool) state

    bindingType binding =
      case binding of
        PlainTypeBinding expressionType -> Just expressionType
        _ -> Nothing

    sharedTypeVar = 1000000

testRecursivePreviewReuseAtSameFrontier :: IO ()
testRecursivePreviewReuseAtSameFrontier =
  assertEqual
    "five binding seeds, five source bodies, and one reusable preview"
    11
    (inferNextTypeVar finalState)
  where
    (_, finalState) =
      TypeInferenceScope.inferScopeType
        allocatingInfer
        ResolveKernelOnly
        Map.empty
        initialInferState
        (programStatements (resolvedProgram "left = right.\nearlyOne = probe.\nearlyTwo = probe.\nearlyThree = probe.\nright = left."))

    allocatingInfer :: InferExprWithModeFn
    allocatingInfer mode _ _ state _ =
      let (_, nextState) = freshTypeVar state
       in inferenceOnlyResult mode (Just SemanticBool) nextState

programStatements :: Expr 'Resolved -> [Statement 'Resolved]
programStatements (EBlock _ statements) = statements
programStatements expression = error ("expected resolved block, got " <> show expression)

valueName :: Text -> ResolvedName
valueName = resolvedLocalName ValueNamespace . mkIdentifier

typeName :: Text -> ResolvedName
typeName = resolvedLocalName TypeNamespace . mkIdentifier

capabilityName :: Text -> ResolvedName
capabilityName = resolvedLocalName CapabilityNamespace . mkIdentifier

inferenceOnlyResult :: InferenceMode -> Maybe ExpressionType -> InferState -> (Maybe ExpressionType, InferState)
inferenceOnlyResult mode expressionType state =
  case mode of
    InferenceOnly -> (expressionType, state)
    InferConcreteFunctions ->
      error "expected inference-only callback invocation"

testOperatorRulePresenceAndSectionSupport :: IO ()
testOperatorRulePresenceAndSectionSupport = do
  mapM_
    (assertEqual "operator rule" True . hasOperatorRule)
    ["+", "-", "*", "/", "<", "<=", ">", ">=", "==", "!=", "$"]
  mapM_ (assertEqual "missing operator rule" False . hasOperatorRule) ["|", "%%"]
  mapM_
    (assertEqual "section support" True . builtinSectionOperatorSymbol)
    ["+", "-", "*", "/", "<", "<=", ">", ">=", "==", "!="]
  mapM_
    (assertEqual "unsupported section" False . builtinSectionOperatorSymbol)
    ["$", "|", "%%"]
