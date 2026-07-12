{-# LANGUAGE OverloadedStrings #-}

module JazzNext.Compiler.Semantics.BindingSignature.InferenceOwnershipTests
  ( inferenceOwnershipTests
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import JazzNext.Compiler.AST
  ( SignatureConstraint (..),
    SignatureType (..)
  )
import JazzNext.Compiler.Name
  ( mkIdentifier,
    sourceName
  )
import JazzNext.Compiler.TypeInference.Capabilities
  ( expressionTypeToRuntimeHint
  )
import JazzNext.Compiler.TypeInference.Diagnostics
  ( duplicateConstraintName
  )
import JazzNext.Compiler.TypeInference.Signature
  ( expressionTypeToRuntimeTemplate
  )
import JazzNext.Compiler.TypeInference.State
  ( DeclarationState (..),
    InferenceOutput (..),
    ModuleInferenceState (..),
    inferClassFacts,
    inferCurrentModulePath,
    inferErrorCount,
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
    TypeSchemeConstraint (..),
    TypeSchemePrimitiveConstraint (..)
  )
import JazzNext.TestHarness
  ( NamedTest,
    assertEqual
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
    ("type operations instantiate class and primitive constraints", testTypeOpsInstantiateConstraints)
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
