{-# LANGUAGE OverloadedStrings #-}

module JazzNext.Compiler.Semantics.BindingSignature.InferenceOwnershipTests
  ( inferenceOwnershipTests
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import JazzNext.Compiler.AST
  ( Expr (..),
    SignatureConstraint (..),
    SignaturePayload (..),
    SignatureType (..),
    Statement (..)
  )
import JazzNext.Compiler.BuiltinCatalog
  ( BuiltinResolutionMode (ResolveKernelOnly)
  )
import JazzNext.Compiler.Diagnostics
  ( SourceSpan (..)
  )
import JazzNext.Compiler.Name
  ( mkIdentifier,
    sourceName
  )
import JazzNext.Compiler.TypeInference.Signature
  ( SignaturePayloadType (..),
    duplicateConstraintName,
    expressionTypeToRuntimeHint,
    expressionTypeToRuntimeTemplate,
    signaturePayloadToSignatureType
  )
import JazzNext.Compiler.TypeInference.Operator
  ( builtinSectionOperatorSymbol,
    hasOperatorRule
  )
import JazzNext.Compiler.TypeInference.Diagnostics
  ( InferExprFn,
    InferExprWithModeFn
  )
import JazzNext.Compiler.TypeInference.Elaboration
  ( InferredExpr (..),
    ProvisionalTypedExpr (..),
    ProvisionalTypedStatement (..),
    TypedCoreProductionMode (..)
  )
import JazzNext.Compiler.TypeInference.Scope
  ( inferScopeType,
    inferScopeTypeWithMode
  )
import JazzNext.Compiler.TypeInference.State
  ( DeclarationState (..),
    InferState (..),
    InferenceOutput (..),
    ModuleInferenceState (..),
    SolverState (..),
    inferClassFacts,
    inferCurrentModulePath,
    inferErrorCount,
    inferNextTypeVar,
    initialInferState,
    modifyDeclarationState,
    modifyInferenceOutput,
    modifyModuleInferenceState
  )
import JazzNext.Compiler.TypeInference.TypeOps
  ( dedupeTypeSchemeConstraints,
    freeTypeVariables,
    freeTypeVariablesInTypeSchemeConstraints,
    freeTypeVariablesInTypeSchemePrimitiveConstraints,
    instantiateTypeSchemeConstraint,
    instantiateTypeSchemePrimitiveConstraint,
    replaceTypeVariables
  )
import JazzNext.Compiler.TypeInference.Types
  ( ExpressionType (..),
    IntegerLiteralRange (..),
    NumericConstraint (..),
    TypeBinding (..),
    TypeSchemeConstraint (..),
    TypeSchemePrimitiveConstraint (..)
  )
import JazzNext.TestHarness
  ( NamedTest,
    assertEqual,
    failTest
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
    ("scheme constraint deduplication preserves last-occurrence order", testSchemeConstraintDeduplicationOrder),
    ("type operations collect recursive free variables", testTypeOpsCollectRecursiveFreeVariables),
    ("type operations collect constraint free variables", testTypeOpsCollectConstraintFreeVariables),
    ("type operations replace recursive type variables", testTypeOpsReplaceRecursiveTypeVariables),
    ("type operations instantiate class and primitive constraints", testTypeOpsInstantiateConstraints),
    ("signature payload normalization allocates ordered variables", testSignaturePayloadNormalizationAllocatesOrderedVariables),
    ("failed signature payload normalization rolls back state", testFailedSignaturePayloadNormalizationRollsBackState),
    ("production scope elaborates each signature once in source order", testProductionScopeElaboratesSignatureOnce),
    ("recursive previews do not expose speculative solver state to intervening bindings", testRecursivePreviewSolverStateIsTransactional),
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

testSchemeConstraintDeduplicationOrder :: IO ()
testSchemeConstraintDeduplicationOrder =
  assertEqual
    "stable-last constraint order"
    [middleConstraint, repeatedConstraint]
    (dedupeTypeSchemeConstraints [repeatedConstraint, middleConstraint, repeatedConstraint])
  where
    repeatedConstraint = TypeSchemeConstraint "Eq" (TVarType 0)
    middleConstraint = TypeSchemeInferredConstraint "Ord" (TVarType 1)

testTypeOpsCollectRecursiveFreeVariables :: IO ()
testTypeOpsCollectRecursiveFreeVariables =
  assertEqual
    "recursive free variables"
    (Set.fromList [1, 2, 3])
    ( freeTypeVariables
        (TFunctionType (TListType (TVarType 1)) (TTupleType [TVarType 2, TListType (TVarType 3)]))
    )

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
      inferScopeTypeWithMode
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

testRecursivePreviewSolverStateIsTransactional :: IO ()
testRecursivePreviewSolverStateIsTransactional =
  assertEqual
    "intervening binding error count"
    0
    (inferErrorCount finalState)
  where
    (_, finalState) =
      inferScopeType
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
                        Map.insert previewSentinel TIntType (solverSubstitution (inferSolver state))
                    }
              }
          )
        EVar "probe"
          | Map.member previewSentinel (solverSubstitution (inferSolver state)) ->
              ( Just TBoolType,
                modifyInferenceOutput
                  (\output -> output {outputErrorCount = outputErrorCount output + 1})
                  state
              )
        _ -> (Just TBoolType, state)

    previewSentinel = 1000000

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
