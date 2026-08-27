{-# LANGUAGE OverloadedStrings #-}

module Jazz.Compiler.Bootstrap.TypedCoreExpressionDirectCallSpec.ManagedProductsVariantsTests where

import Data.Text (Text)
import Jazz.Compiler.Bootstrap.TypedCoreExpressionDirectCallFixtures
import Jazz.Compiler.Bootstrap.TypedCoreExpressionDirectCallSpec.Support
import Jazz.Compiler.TypeInference
import Jazz.TestHarness (assertEqual)

testManagedProductVariantRetention :: IO ()
testManagedProductVariantRetention = do
  assertBoundary
    "managed-tuple-child-failure"
    [expressionFailure 0 [1] TypedCoreStructuredValueUnsupported TypedCoreListValueDetail]
  assertBoundary
    "managed-data-sibling-failure"
    [ statementFailure 0 TypedCoreStructuredValueUnsupported TypedCoreDataValueDetail,
      expressionFailure 1 [] TypedCoreStructuredValueUnsupported TypedCoreListValueDetail
    ]

testManagedProductVariantFinalizationBoundary :: IO ()
testManagedProductVariantFinalizationBoundary = do
  assertBoundary
    "managed-tuple"
    [expressionFailure 0 [] TypedCoreStructuredValueUnsupported TypedCoreTupleValueDetail]
  assertBoundary
    "managed-option"
    [ statementFailure 0 TypedCoreStructuredValueUnsupported TypedCoreDataValueDetail,
      expressionFailure 1 [] TypedCoreNonLocalCallUnsupported (TypedCoreNameDetail "Some")
    ]
  assertBoundary
    "managed-tree"
    [ statementFailure 0 TypedCoreStructuredValueUnsupported TypedCoreDataValueDetail,
      expressionFailure 1 [] TypedCoreNonLocalCallUnsupported (TypedCoreNameDetail "Branch"),
      expressionFailure 1 [0, 1] TypedCoreNonLocalCallUnsupported (TypedCoreNameDetail "Leaf"),
      expressionFailure 1 [1] TypedCoreNonLocalCallUnsupported (TypedCoreNameDetail "Leaf")
    ]

assertBoundary :: Text -> [TypedCoreProductionFailure] -> IO ()
assertBoundary name expectedFailures = do
  let fixture = managedProductVariantFixture name
  ordinary <- inferFixture fixture
  firstRun <- produceFixture fixture
  secondRun <- produceFixture fixture
  assertEqual (name <> " inference compatibility") ordinary (typedCoreProductionInferenceResult firstRun)
  assertEqual (name <> " repeatable production") firstRun secondRun
  assertEqual
    (name <> " exact producer boundary")
    (TypedCoreProductionUnsupported expectedFailures)
    (typedCoreProductionStatus firstRun)

expressionFailure :: Int -> [Int] -> TypedCoreProductionFailureKind -> TypedCoreProductionFailureDetail -> TypedCoreProductionFailure
expressionFailure statementIndex childPath kind detail =
  TypedCoreProductionFailure
    (TypedCoreProductionExpressionPath ["App", "Main"] statementIndex childPath)
    kind
    detail

statementFailure :: Int -> TypedCoreProductionFailureKind -> TypedCoreProductionFailureDetail -> TypedCoreProductionFailure
statementFailure statementIndex kind detail =
  TypedCoreProductionFailure
    (TypedCoreProductionStatementPath ["App", "Main"] statementIndex)
    kind
    detail
