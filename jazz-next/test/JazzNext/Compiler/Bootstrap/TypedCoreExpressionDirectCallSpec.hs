{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import JazzNext.Compiler.Bootstrap.TypedCoreExpressionDirectCallFixtures
import JazzNext.Compiler.AST (Expr (..), Literal (..), Statement (..))
import JazzNext.Compiler.Diagnostics (SourceSpan (..))
import JazzNext.Compiler.ModuleGraph (CoreModule (..), ResolvedModule (resolvedModuleCore))
import JazzNext.Compiler.TypeInference
import JazzNext.Compiler.TypeInference.Types
  ( DataTypeBinding (..),
    ExpressionType (TBoolType),
    ScopeCapabilityFacts (..),
    TypeBinding (PlainTypeBinding),
    emptyScopeCapabilityFacts
  )
import JazzNext.TestHarness (NamedTest, assertEqual, failTest, runTestSuite)

main :: IO ()
main = runTestSuite "TypedCoreExpressionDirectCall" tests

tests :: [NamedTest]
tests =
  [ ("audits the partial foundation fixture manifest", testFixtureManifest),
    ("produces unit and preserves ordinary inference", testUnitProduction),
    ("produces the complete scalar expression profile twice", testScalarProduction),
    ("reports rejected scalar profile nodes twice", testRejectedScalarProfile),
    ("diagnostics take precedence over profile failures", testDiagnosticPrecedence),
    ("reports the initial input profile failures", testInputFailures),
    ("reports every additional foundation profile failure", testAdditionalProfileFailures)
  ]

testFixtureManifest :: IO ()
testFixtureManifest =
  assertEqual
    "complete scalar expression fixture names"
    [ "unit-entry",
      "bool-entry",
      "char-entry",
      "default-int-entry",
      "default-float-entry",
      "arithmetic-operators",
      "ordering-operators",
      "equality-operators",
      "text-value",
      "list-value",
      "non-unit-tuple",
      "data-value",
      "conditional",
      "pattern-case",
      "local-block-binding",
      "source-diagnostic",
      "invalid-portable-source-path",
      "resolved-import",
      "ambient-prelude-input"
    ]
    fixtureNames
  >> assertEqual
    "admitted operators"
    ["+", "-", "*", "/", "<", "<=", ">", ">=", "==", "!="]
    admittedOperators

testScalarProduction :: IO ()
testScalarProduction =
  mapM_ assertScalar scalarExpectedPrograms
  where
    assertScalar (name, expectedProgram) =
      case filter ((== name) . fixtureName) scalarFixtures of
        [fixture] -> do
          firstRun <- produce fixture
          secondRun <- produce fixture
          assertEqual (name <> " repeatable production") firstRun secondRun
          assertEqual (name <> " scalar program") (TypedCoreProductionSucceeded expectedProgram) firstRun
        _ -> failTest (name <> " scalar fixture is missing")

    produce fixture =
      typedCoreProductionStatus
        <$> inferResolvedModuleTypedCoreWithProfile
          TypedCoreExpressionDirectCallProfile
          (fixtureInputs fixture)
          (fixtureSourcePath fixture)
          (fixtureModule fixture)

testRejectedScalarProfile :: IO ()
testRejectedScalarProfile =
  mapM_ assertRejected
    [ ("text-value", TypedCoreManagedValueUnsupported, TypedCoreTextValueDetail),
      ("list-value", TypedCoreStructuredValueUnsupported, TypedCoreListValueDetail),
      ("non-unit-tuple", TypedCoreStructuredValueUnsupported, TypedCoreTupleValueDetail),
      ("data-value", TypedCoreStructuredValueUnsupported, TypedCoreDataValueDetail),
      ("conditional", TypedCoreControlFlowUnsupported, TypedCoreConditionalDetail),
      ("pattern-case", TypedCorePatternCaseUnsupported, TypedCorePatternCaseDetail),
      ("local-block-binding", TypedCoreNestedBlockUnsupported, TypedCoreLocalBlockDetail)
    ]
  where
    assertRejected (name, failureKind, failureDetail) =
      case filter ((== name) . fixtureName) rejectedScalarFixtures of
        [fixture] -> do
          firstRun <- produce fixture
          secondRun <- produce fixture
          assertEqual (name <> " repeatable rejection") firstRun secondRun
          assertEqual
            (name <> " production failure")
            (TypedCoreProductionUnsupported [TypedCoreProductionFailure (TypedCoreProductionExpressionPath ["App", "Main"] 0 []) failureKind failureDetail])
            firstRun
        _ -> failTest (name <> " rejected fixture is missing")

    produce fixture =
      typedCoreProductionStatus
        <$> inferResolvedModuleTypedCoreWithProfile
          TypedCoreExpressionDirectCallProfile
          (fixtureInputs fixture)
          (fixtureSourcePath fixture)
          (fixtureModule fixture)

testUnitProduction :: IO ()
testUnitProduction =
  case fixtures of
    [] -> failTest "unit fixture is missing"
    actualFixture : _ -> do
      beforeInferenceResult <- inferExpressionWithInputs (fixtureInputs actualFixture) (coreModuleExpr (resolvedModuleCore (fixtureModule actualFixture)))
      actualUnitResult <-
        inferResolvedModuleTypedCoreWithProfile
          TypedCoreExpressionDirectCallProfile
          (fixtureInputs actualFixture)
          (fixtureSourcePath actualFixture)
          (fixtureModule actualFixture)
      assertEqual "unit production" (TypedCoreProductionSucceeded expectedUnitProgram) (typedCoreProductionStatus actualUnitResult)
      assertEqual "ordinary inference is unchanged" beforeInferenceResult (typedCoreProductionInferenceResult actualUnitResult)

testDiagnosticPrecedence :: IO ()
testDiagnosticPrecedence = do
  let sourceDiagnosticFixture = fixtureByName "source-diagnostic"
  sourceDiagnosticResult <-
    inferResolvedModuleTypedCoreWithProfile
      TypedCoreExpressionDirectCallProfile
      (fixtureInputs sourceDiagnosticFixture)
      (fixtureSourcePath sourceDiagnosticFixture)
      (fixtureModule sourceDiagnosticFixture)
  assertEqual "diagnostics take precedence" TypedCoreProductionBlockedByDiagnostics (typedCoreProductionStatus sourceDiagnosticResult)

testInputFailures :: IO ()
testInputFailures =
  case map fixtureByName ["invalid-portable-source-path", "resolved-import", "ambient-prelude-input"] of
    [invalidPathFixture, importFixture, ambientFixture] -> do
      assertUnsupported
        invalidPathFixture
        [TypedCoreProductionFailure TypedCoreProductionInputPath TypedCoreInvalidPortableSourcePath TypedCoreNoFailureDetail]
      assertUnsupported
        importFixture
        [TypedCoreProductionFailure (TypedCoreProductionModulePath ["App", "Main"]) TypedCoreResolvedImportsUnsupported TypedCoreNoFailureDetail]
      assertUnsupported
        ambientFixture
        [TypedCoreProductionFailure TypedCoreProductionInputPath TypedCoreAmbientPreludeInputUnsupported TypedCoreNoFailureDetail]
    _ -> failTest "foundation input fixtures are missing"

assertUnsupported :: Fixture -> [TypedCoreProductionFailure] -> IO ()
assertUnsupported fixture expectedFailures = do
  result <-
    inferResolvedModuleTypedCoreWithProfile
      TypedCoreExpressionDirectCallProfile
      (fixtureInputs fixture)
      (fixtureSourcePath fixture)
      (fixtureModule fixture)
  assertEqual
    (fixtureName fixture <> " production status")
    (TypedCoreProductionUnsupported expectedFailures)
    (typedCoreProductionStatus result)

testAdditionalProfileFailures :: IO ()
testAdditionalProfileFailures =
  case fixtures of
    unitFixture : _ -> do
      let pathMismatch =
            unitFixture
              { fixtureInputs = (fixtureInputs unitFixture) {inferenceCurrentModulePath = Just ["Other", "Main"]}
              }
          importedValue =
            unitFixture
              { fixtureInputs =
                  (fixtureInputs unitFixture)
                    { inferenceImportedTypes = Map.singleton "foreign" (PlainTypeBinding TBoolType)
                    }
              }
          importedData =
            unitFixture
              { fixtureInputs =
                  (fixtureInputs unitFixture)
                    { inferenceImportedDataTypes = Map.singleton "Foreign" (DataTypeBinding [] [])
                    }
              }
          importedCapabilities =
            unitFixture
              { fixtureInputs =
                  (fixtureInputs unitFixture)
                    { inferenceImportedCapabilities = emptyScopeCapabilityFacts {scopeClassFacts = Map.singleton "Foreign" 0}
                    }
              }
          unsupportedRoot = unitFixture {fixtureModule = withExpression (ELit (LBool True)) unitFixture}
          leadingStatement =
            unitFixture
              { fixtureModule =
                  withExpression
                    (EBlock [SLet "ignored" (SourceSpan 1 1) (ETuple []), SExpr (SourceSpan 2 1) (ETuple [])])
                    unitFixture
              }
          inputFailure = [TypedCoreProductionFailure TypedCoreProductionInputPath TypedCoreImportedInputsUnsupported TypedCoreNoFailureDetail]
          rootExpressionFailure = [TypedCoreProductionFailure (TypedCoreProductionExpressionPath ["App", "Main"] 0 []) TypedCoreUnsupportedRootExpression TypedCoreUnsupportedRootDetail]
          moduleFailure = [TypedCoreProductionFailure (TypedCoreProductionModulePath ["App", "Main"]) TypedCoreUnsupportedRootExpression TypedCoreUnsupportedRootDetail]
      assertUnsupported pathMismatch [TypedCoreProductionFailure TypedCoreProductionInputPath TypedCoreModulePathMismatch TypedCoreNoFailureDetail]
      assertUnsupported importedValue inputFailure
      assertUnsupported importedData inputFailure
      assertUnsupported importedCapabilities inputFailure
      assertUnsupported unsupportedRoot rootExpressionFailure
      assertUnsupported leadingStatement moduleFailure
    [] -> failTest "unit fixture is missing"

withExpression :: Expr -> Fixture -> ResolvedModule
withExpression expression fixture =
  let moduleValue = fixtureModule fixture
   in moduleValue
        { resolvedModuleCore =
            CoreModule
              (Just ["App", "Main"])
              Nothing
              []
              expression
        }

fixtureByName :: Text -> Fixture
fixtureByName name =
  case filter ((== name) . fixtureName) fixtures of
    [fixture] -> fixture
    _ -> error "fixture is missing"
