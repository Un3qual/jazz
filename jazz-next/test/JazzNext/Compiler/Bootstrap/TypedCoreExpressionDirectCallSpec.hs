{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Map.Strict as Map
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
    ("diagnostics take precedence over profile failures", testDiagnosticPrecedence),
    ("reports the initial input profile failures", testInputFailures),
    ("reports every additional foundation profile failure", testAdditionalProfileFailures)
  ]

testFixtureManifest :: IO ()
testFixtureManifest =
  assertEqual
    "partial foundation fixture names"
    [ "unit-entry",
      "source-diagnostic",
      "invalid-portable-source-path",
      "resolved-import",
      "ambient-prelude-input"
    ]
    fixtureNames

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
  let sourceDiagnosticFixture = fixtures !! 1
  sourceDiagnosticResult <-
    inferResolvedModuleTypedCoreWithProfile
      TypedCoreExpressionDirectCallProfile
      (fixtureInputs sourceDiagnosticFixture)
      (fixtureSourcePath sourceDiagnosticFixture)
      (fixtureModule sourceDiagnosticFixture)
  assertEqual "diagnostics take precedence" TypedCoreProductionBlockedByDiagnostics (typedCoreProductionStatus sourceDiagnosticResult)

testInputFailures :: IO ()
testInputFailures =
  case drop 2 fixtures of
    [invalidPathFixture, importFixture, ambientFixture] -> do
      assertUnsupported
        invalidPathFixture
        [TypedCoreProductionFailure TypedCoreProductionInputPath TypedCoreInvalidPortableSourcePath]
      assertUnsupported
        importFixture
        [TypedCoreProductionFailure (TypedCoreProductionModulePath ["App", "Main"]) TypedCoreResolvedImportsUnsupported]
      assertUnsupported
        ambientFixture
        [TypedCoreProductionFailure TypedCoreProductionInputPath TypedCoreAmbientPreludeInputUnsupported]
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
          inputFailure = [TypedCoreProductionFailure TypedCoreProductionInputPath TypedCoreImportedInputsUnsupported]
          moduleFailure = [TypedCoreProductionFailure (TypedCoreProductionModulePath ["App", "Main"]) TypedCoreUnsupportedRootExpression]
      assertUnsupported pathMismatch [TypedCoreProductionFailure TypedCoreProductionInputPath TypedCoreModulePathMismatch]
      assertUnsupported importedValue inputFailure
      assertUnsupported importedData inputFailure
      assertUnsupported importedCapabilities inputFailure
      assertUnsupported unsupportedRoot moduleFailure
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
