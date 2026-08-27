{-# LANGUAGE OverloadedStrings #-}

module Jazz.Compiler.Bootstrap.TypedCoreExpressionDirectCallSpec.ManagedProductsVariantsTests where

import Data.Text (Text)
import qualified Data.Text as Text
import Jazz.Compiler.Bootstrap.TypedCoreExpressionDirectCallFixtures
import Jazz.Compiler.Bootstrap.TypedCoreExpressionDirectCallSpec.Support
import Jazz.Compiler.TypeInference
import Jazz.Compiler.TypedCore.Validate (validateTypedProgram)
import Jazz.TestHarness (assertEqual, failTest)

testManagedProductVariantRetention :: IO ()
testManagedProductVariantRetention = do
  assertBoundary
    "managed-tuple-child-failure"
    [expressionFailure 0 [1] TypedCoreStructuredValueUnsupported TypedCoreListValueDetail]
  assertBoundary
    "managed-data-sibling-failure"
    [expressionFailure 1 [] TypedCoreStructuredValueUnsupported TypedCoreListValueDetail]
  assertBoundary
    "managed-bare-constructor-failure"
    [expressionFailure 1 [] TypedCoreCallableValueUnsupported (TypedCoreNameDetail "Box")]
  assertBoundary
    "managed-partial-constructor-failure"
    [expressionFailure 1 [] TypedCoreCallArityUnsupported (TypedCoreArityDetail 2 1)]
  assertBoundary
    "managed-list-field-failure"
    [statementFailure 0 TypedCoreStructuredValueUnsupported TypedCoreDataValueDetail]
  assertBoundary
    "managed-unresolved-constructor-failure"
    [expressionFailure 1 [] TypedCoreUnresolvedExpressionType TypedCoreDataValueDetail]

testManagedProductVariantProduction :: IO ()
testManagedProductVariantProduction =
  mapM_ assertProduced managedProductVariantExpectedPrograms
  where
    assertProduced (name, expectedProgram) = do
      firstRun <- produceFixture (managedProductVariantFixture name)
      secondRun <- produceFixture (managedProductVariantFixture name)
      assertEqual (name <> " repeatable exact production") firstRun secondRun
      assertEqual (name <> " expected typed validation") [] (validateTypedProgram expectedProgram)
      case typedCoreProductionStatus firstRun of
        TypedCoreProductionSucceeded actualProgram ->
          assertEqual (name <> " exact typed program") expectedProgram actualProgram
        status -> failTest (name <> " did not produce typed core: " <> Text.pack (show status))

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
