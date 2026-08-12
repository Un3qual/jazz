{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Jazz.Compiler.Semantics.BindingSignature.InferenceOwnershipTests
  ( inferenceOwnershipTests
  ) where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Text (Text)
import Jazz.Compiler.AST
  ( Expr (..),
    SignatureConstraint (..),
    SignaturePayload (..),
    SignatureType (..),
    Statement (..)
  )
import Jazz.Compiler.BuiltinCatalog
  ( BuiltinResolutionMode (ResolveKernelOnly)
  )
import Jazz.Compiler.Diagnostics
  ( SourceSpan (..)
  )
import Jazz.Compiler.Name
  ( mkIdentifier,
    sourceName
  )
import Jazz.Compiler.RecursiveBindings
  ( prepareRecursiveScope
  )
import Jazz.Compiler.TypeInference.Signature
  ( SignaturePayloadType (..),
    duplicateConstraintName,
    expressionTypeToRuntimeHint,
    expressionTypeToRuntimeTemplate,
    signaturePayloadToSignatureType
  )
import Jazz.Compiler.TypeInference.Capabilities
  ( typeSchemeReferencedCapabilityFacts
  )
import Jazz.Compiler.TypeInference.Operator
  ( builtinSectionOperatorSymbol,
    hasOperatorRule
  )
import Jazz.Compiler.TypeInference.Diagnostics
  ( InferExprFn,
    InferExprWithModeFn
  )
import Jazz.Compiler.TypeInference.Elaboration
  ( InferredExpr (..),
    ProvisionalTypedExpr (..),
    ProvisionalTypedStatement (..),
    TypedCoreProductionMode (..)
  )
import qualified Jazz.Compiler.TypeInference.Scope as TypeInferenceScope
import Jazz.Compiler.TypeInference.Solver
  ( addNumericTypeVarConstraint,
    applySubstitution,
    bindTypeVar,
    freshTypeVar,
    resolveType,
    unifyTypes
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
    initialInferState,
    modifyDeclarationState,
    modifyInferenceOutput,
    modifyModuleInferenceState
  )
import Jazz.Compiler.TypeInference.TypeOps
  ( dedupeTypeSchemeConstraints,
    freeTypeVariables,
    freeTypeVariablesInTypeSchemeConstraints,
    freeTypeVariablesInTypeSchemePrimitiveConstraints,
    instantiateTypeSchemeConstraint,
    instantiateTypeSchemePrimitiveConstraint,
    replaceTypeVariables
  )
import Jazz.Compiler.TypeInference.Types
  ( ExpressionType (..),
    IntegerLiteralRange (..),
    NumericConstraint (..),
    ScopeCapabilityFacts,
    TypeBinding (..),
    TypeSchemeConstraint (..),
    TypeSchemePrimitiveConstraint (..),
    emptyScopeCapabilityFacts
  )
import Jazz.TestHarness
  ( NamedTest,
    assertEqual,
    failTest
  )
import Language.Haskell.TH
  ( lookupValueName
  )

$( do
     legacyEntryPoint <- lookupValueName "TypeInferenceScope.inferScopeTypeWithModeAndForwardBindingsUsingFacts"
     case legacyEntryPoint of
       Nothing -> pure []
       Just _ ->
         fail
           "TypeInferenceScope.inferScopeTypeWithModeAndForwardBindingsUsingFacts must remain unavailable; use the owned PreparedRecursiveScope entry point"
 )

inferenceOwnershipTests :: [NamedTest]
inferenceOwnershipTests =
  [ ("runtime hints accept Int64-fitting integer ranges", testRuntimeHintsAcceptInt64FittingIntegerRanges),
    ("runtime hints reject overflowing integer ranges", testRuntimeHintsRejectOverflowingIntegerRanges),
    ("runtime templates reject integer literals", testRuntimeTemplatesRejectIntegerLiterals),
    ("runtime templates accept only mapped quantified variables", testRuntimeTemplatesAcceptOnlyMappedQuantifiedVariables),
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
    ("recursive previews are reused at an unchanged group frontier", testRecursivePreviewReuseAtSameFrontier),
    ("operator rule presence remains distinct from section support", testOperatorRulePresenceAndSectionSupport)
  ]

testRuntimeHintsAcceptInt64FittingIntegerRanges :: IO ()
testRuntimeHintsAcceptInt64FittingIntegerRanges =
  assertEqual
    "Int64 range hint"
    (Just TypeInt)
    ( expressionTypeToRuntimeHint
        (TIntegerLiteralType (IntegerLiteralRange (-9223372036854775808) 9223372036854775807))
    )

testRuntimeHintsRejectOverflowingIntegerRanges :: IO ()
testRuntimeHintsRejectOverflowingIntegerRanges = do
  assertEqual
    "positive Int64 overflow"
    Nothing
    ( expressionTypeToRuntimeHint
        (TIntegerLiteralType (IntegerLiteralRange 0 9223372036854775808))
    )
  assertEqual
    "negative Int64 overflow"
    Nothing
    ( expressionTypeToRuntimeHint
        (TIntegerLiteralType (IntegerLiteralRange (-9223372036854775809) 0))
    )

testRuntimeTemplatesRejectIntegerLiterals :: IO ()
testRuntimeTemplatesRejectIntegerLiterals =
  assertEqual
    "literal template"
    Nothing
    ( expressionTypeToRuntimeTemplate
        Map.empty
        (TIntegerLiteralType (IntegerLiteralRange 1 1))
    )

testRuntimeTemplatesAcceptOnlyMappedQuantifiedVariables :: IO ()
testRuntimeTemplatesAcceptOnlyMappedQuantifiedVariables = do
  let variableName = sourceName (mkIdentifier "a")
  assertEqual
    "mapped variable template"
    (Just (TypeVariable variableName))
    (expressionTypeToRuntimeTemplate (Map.singleton 7 variableName) (TVarType 7))
  assertEqual
    "unmapped variable template"
    Nothing
    (expressionTypeToRuntimeTemplate Map.empty (TVarType 7))

testRuntimeHintChildFailuresPropagate :: IO ()
testRuntimeHintChildFailuresPropagate = do
  assertEqual
    "list child failure"
    Nothing
    (expressionTypeToRuntimeHint (TListType (TVarType 1)))
  assertEqual
    "function child failure"
    Nothing
    (expressionTypeToRuntimeHint (TFunctionType TIntType (TVarType 1)))

testRuntimeTemplateChildFailuresPropagate :: IO ()
testRuntimeTemplateChildFailuresPropagate = do
  let literalType = TIntegerLiteralType (IntegerLiteralRange 1 1)
  assertEqual
    "list child failure"
    Nothing
    (expressionTypeToRuntimeTemplate Map.empty (TListType literalType))
  assertEqual
    "function child failure"
    Nothing
    (expressionTypeToRuntimeTemplate Map.empty (TFunctionType TIntType literalType))

testDuplicateConstraintsReportFirstRepeatedName :: IO ()
testDuplicateConstraintsReportFirstRepeatedName =
  assertEqual
    "first duplicate"
    (Just "Eq")
    ( duplicateConstraintName
        [ SignatureConstraint "Eq" [TypeInt],
          SignatureConstraint "Ord" [TypeInt],
          SignatureConstraint "Eq" [TypeBool],
          SignatureConstraint "Ord" [TypeBool]
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
    firstDeferred = deferredConstraint "Eq" TIntType
    secondDeferred = deferredConstraint "Show" TTextType
    firstInferred = TypeSchemeInferredConstraint "Eq" TIntType
    secondInferred = TypeSchemeMethodConstraint "Show" "Show::show" TTextType

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
    repeatedConstraint = TypeSchemeConstraint "Eq" (TVarType 0)
    middleConstraint = TypeSchemeInferredConstraint "Ord" (TVarType 1)

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
        (TFunctionType (TListType (TVarType 1)) (TTupleType [TVarType 2, TListType (TVarType 3)]))
    )

testSolverResolvesLongSubstitutionChains :: IO ()
testSolverResolvesLongSubstitutionChains =
  assertEqual
    "resolved compound substitution"
    (TTupleType [TListType TIntType, TFunctionType TIntType TBoolType])
    ( applySubstitution
        substitution
        (TTupleType [TListType (TVarType 0), TFunctionType (TVarType 0) TBoolType])
    )
  where
    substitution =
      IntMap.fromList
        ([(typeVar, TVarType (typeVar + 1)) | typeVar <- [0 .. 62]] ++ [(63, TIntType)])

testUnificationPathCompressesSubstitutionChains :: IO ()
testUnificationPathCompressesSubstitutionChains =
  case unifyTypes (TVarType 0) TIntType chainState of
    Nothing -> failTest "expected chained variable to unify with Int"
    Just nextState -> do
      assertEqual
        "compressed root substitution"
        (Just TIntType)
        (IntMap.lookup 0 (solverSubstitution (inferSolver nextState)))
      assertEqual
        "compressed middle substitution"
        (Just TIntType)
        (IntMap.lookup 1 (solverSubstitution (inferSolver nextState)))
      assertEqual "resolved root type" TIntType (resolveType nextState (TVarType 0))
  where
    chainState =
      initialInferState
        { inferSolver =
            (inferSolver initialInferState)
              { solverSubstitution =
                  IntMap.fromList
                    [ (0, TVarType 1),
                      (1, TVarType 2),
                      (2, TIntType)
                    ]
              }
        }

testSolverPreservesBindingConstraints :: IO ()
testSolverPreservesBindingConstraints = do
  case bindTypeVar 0 (TListType (TVarType 0)) initialInferState of
    Nothing -> pure ()
    Just _ -> failTest "expected occurs check to reject a recursive type"
  case unifyTypes (TVarType 0) TIntType rigidState of
    Nothing -> pure ()
    Just _ -> failTest "expected rigid type variable unification to fail"
  case unifyTypes (TVarType 0) (TVarType 1) numericState of
    Nothing -> failTest "expected constrained variables to unify"
    Just linkedState -> do
      case unifyTypes (TVarType 1) TFloatType linkedState of
        Nothing -> pure ()
        Just _ -> failTest "expected integral constraint to reject Float"
      case unifyTypes (TVarType 1) TIntType linkedState of
        Nothing -> failTest "expected integral constraint to accept Int"
        Just resolvedState ->
          assertEqual "resolved constrained root" TIntType (resolveType resolvedState (TVarType 0))
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
        [ TypeSchemeConstraint "Eq" (TListType (TVarType 1)),
          TypeSchemeMethodConstraint "Show" "Show::show" (TVarType 2)
        ]
    )
  assertEqual
    "primitive constraint free variables"
    (Set.fromList [3, 4])
    ( freeTypeVariablesInTypeSchemePrimitiveConstraints
        [ TypeSchemeNumericConstraint AnyNumericConstraint (TVarType 3),
          TypeSchemeStrictEqualityConstraint (TListType (TVarType 4))
        ]
    )

testTypeOpsReplaceRecursiveTypeVariables :: IO ()
testTypeOpsReplaceRecursiveTypeVariables =
  assertEqual
    "recursive replacement"
    (TFunctionType (TListType TIntType) (TTupleType [TVarType 2, TBoolType]))
    ( replaceTypeVariables
        (Map.fromList [(1, TIntType), (3, TBoolType)])
        (TFunctionType (TListType (TVarType 1)) (TTupleType [TVarType 2, TVarType 3]))
    )

testTypeOpsInstantiateConstraints :: IO ()
testTypeOpsInstantiateConstraints = do
  let replacements = Map.singleton 1 TTextType
  assertEqual
    "class constraint instantiation"
    (TypeSchemeMethodConstraint "Show" "Show::show" (TListType TTextType))
    ( instantiateTypeSchemeConstraint
        replacements
        (TypeSchemeMethodConstraint "Show" "Show::show" (TListType (TVarType 1)))
    )
  assertEqual
    "primitive constraint instantiation"
    (TypeSchemeStrictEqualityConstraint (TFunctionType TTextType (TVarType 2)))
    ( instantiateTypeSchemePrimitiveConstraint
        replacements
        (TypeSchemeStrictEqualityConstraint (TFunctionType (TVarType 1) (TVarType 2)))
    )

testSignaturePayloadNormalizationAllocatesOrderedVariables :: IO ()
testSignaturePayloadNormalizationAllocatesOrderedVariables =
  case signaturePayloadToSignatureType payload initialInferState of
    (Nothing, _) -> failTest "expected signature payload normalization"
    (Just normalized, nextState) -> do
      assertEqual
        "normalized signature type"
        (TFunctionType (TVarType 0) (TListType (TVarType 0)))
        (signaturePayloadDeclaredType normalized)
      assertEqual "normalized constraints" [] (signaturePayloadExplicitConstraints normalized)
      assertEqual "variable order" [0] (signaturePayloadVariableOrder normalized)
      assertEqual "next type variable" 1 (inferNextTypeVar nextState)
  where
    variableName = sourceName (mkIdentifier "a")
    payload = SignatureType (TypeFunction (TypeVariable variableName) (TypeList (TypeVariable variableName)))

testFailedSignaturePayloadNormalizationRollsBackState :: IO ()
testFailedSignaturePayloadNormalizationRollsBackState =
  case signaturePayloadToSignatureType payload initialInferState of
    (Nothing, nextState) -> assertEqual "rollback state" initialInferState nextState
    (Just _, _) -> failTest "expected signature payload normalization failure"
  where
    payload = SignatureType (TypeName (sourceName (mkIdentifier "Missing")))

testProductionScopeElaboratesSignatureOnce :: IO ()
testProductionScopeElaboratesSignatureOnce =
  case inferredProvisionalExpr inferredScope of
    Just (ProvisionalScopeStatements (ProvisionalSignature _ _ _ signatureType : _)) -> do
      assertEqual
        "source-ordered prepared signature"
        (TFunctionType (TVarType 0) (TVarType 0))
        signatureType
      assertEqual
        "one signature allocation plus one binding seed"
        2
        (inferNextTypeVar finalState)
    _ -> failTest "expected a retained provisional signature"
  where
    (inferredScope, finalState) =
      TypeInferenceScope.inferScopeTypeWithMode
        Set.empty
        syntheticProductionInfer
        ProduceTypedCoreExpressionDirectCall
        ResolveKernelOnly
        Map.empty
        initialInferState
        [ SSignature
            "identity"
            (SourceSpan 1 1)
            (SignatureType (TypeFunction (TypeVariable "a") (TypeVariable "a"))),
          SLet "identity" (SourceSpan 2 1) (ELambda "value" (EVar "value"))
        ]

    syntheticProductionInfer :: InferExprWithModeFn
    syntheticProductionInfer _ _ env state expression =
      case expression of
        EVar name ->
          case Map.lookup name env of
            Just (PlainTypeBinding expressionType) ->
              ( InferredExpr
                  (Just expressionType)
                  (Just (ProvisionalVariableExpression name expressionType))
                  [],
                state
              )
            _ -> (InferredExpr Nothing Nothing [], state)
        _ -> (InferredExpr Nothing Nothing [], state)

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
    statements =
      [SLet "self" (SourceSpan 1 1) (EVar "self")]
    (_, ordinaryState, _) =
      TypeInferenceScope.inferScopeTypeWithModeAndForwardBindings
        Set.empty
        syntheticProductionInfer
        InferenceOnly
        ResolveKernelOnly
        Map.empty
        initialInferState
        statements
    (_, preparedState, _) =
      TypeInferenceScope.inferScopeTypeWithModeAndForwardBindingsUsingPreparedScope
        (prepareRecursiveScope (Set.singleton "self") statements)
        Set.empty
        syntheticProductionInfer
        InferenceOnly
        ResolveKernelOnly
        Map.empty
        initialInferState

    syntheticProductionInfer :: InferExprWithModeFn
    syntheticProductionInfer _ _ env state expression =
      case expression of
        EVar name ->
          case Map.lookup name env of
            Just (PlainTypeBinding expressionType) ->
              (InferredExpr (Just expressionType) Nothing [], state)
            _ ->
              ( InferredExpr Nothing Nothing [],
                modifyInferenceOutput
                  (\output -> output {outputErrorCount = outputErrorCount output + 1})
                  state
              )
        _ -> (InferredExpr Nothing Nothing [], state)

testRecursivePreviewSolverStateIsTransactional :: IO ()
testRecursivePreviewSolverStateIsTransactional =
  assertEqual
    "intervening binding error count"
    0
    (inferErrorCount finalState)
  where
    (_, finalState) =
      TypeInferenceScope.inferScopeType
        Set.empty
        syntheticPreviewInfer
        ResolveKernelOnly
        Map.empty
        initialInferState
        [ SLet "left" (SourceSpan 1 1) (EVar "right"),
          SLet "early" (SourceSpan 2 1) (EVar "probe"),
          SLet "right" (SourceSpan 3 1) (EVar "left")
        ]

    syntheticPreviewInfer :: InferExprFn
    syntheticPreviewInfer _ _ state expression =
      case expression of
        EVar "left" ->
          ( Just TBoolType,
            state
              { inferSolver =
                  (inferSolver state)
                    { solverSubstitution =
                        IntMap.insert previewSentinel TIntType (solverSubstitution (inferSolver state))
                    }
              }
          )
        EVar "probe"
          | IntMap.member previewSentinel (solverSubstitution (inferSolver state)) ->
              ( Just TBoolType,
                modifyInferenceOutput
                  (\output -> output {outputErrorCount = outputErrorCount output + 1})
                  state
              )
        _ -> (Just TBoolType, state)

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
        Set.empty
        syntheticPreviewInfer
        ResolveKernelOnly
        (Map.singleton "shared" (PlainTypeBinding (TVarType sharedTypeVar)))
        initialInferState
        [ SLet "left" (SourceSpan 1 1) (EVar "right"),
          SLet "advance" (SourceSpan 2 1) (EVar "advanceSolver"),
          SLet "probe" (SourceSpan 3 1) (EVar "probeLeft"),
          SLet "right" (SourceSpan 4 1) (EApply (EVar "left") (EVar "shared"))
        ]

    syntheticPreviewInfer :: InferExprFn
    syntheticPreviewInfer _ env state expression =
      case expression of
        EVar "right" ->
          (bindingType =<< Map.lookup "right" env, state)
        EApply (EVar "left") (EVar "shared") ->
          (resolveType state <$> (bindingType =<< Map.lookup "shared" env), state)
        EVar "advanceSolver" ->
          ( Just TBoolType,
            case bindTypeVar sharedTypeVar TBoolType state of
              Just nextState -> nextState
              Nothing -> state
          )
        EVar "probeLeft" ->
          ( Just TBoolType,
            case Map.lookup "left" env of
              Just (PlainTypeBinding TBoolType) -> state
              _ ->
                modifyInferenceOutput
                  (\output -> output {outputErrorCount = outputErrorCount output + 1})
                  state
          )
        _ -> (Just TBoolType, state)

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
        Set.empty
        allocatingInfer
        ResolveKernelOnly
        Map.empty
        initialInferState
        [ SLet "left" (SourceSpan 1 1) (EVar "right"),
          SLet "earlyOne" (SourceSpan 2 1) (EVar "probe"),
          SLet "earlyTwo" (SourceSpan 3 1) (EVar "probe"),
          SLet "earlyThree" (SourceSpan 4 1) (EVar "probe"),
          SLet "right" (SourceSpan 5 1) (EVar "left")
        ]

    allocatingInfer :: InferExprFn
    allocatingInfer _ _ state _ =
      let (_, nextState) = freshTypeVar state
       in (Just TBoolType, nextState)

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
