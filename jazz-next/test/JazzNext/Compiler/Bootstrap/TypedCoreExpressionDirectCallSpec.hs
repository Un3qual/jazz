{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import JazzNext.Compiler.Bootstrap.TypedCoreExpressionDirectCallFixtures
import JazzNext.Compiler.ModuleGraph (CoreModule (coreModuleExpr), ResolvedModule (resolvedModuleCore))
import JazzNext.Compiler.TypeInference
import JazzNext.TestHarness (NamedTest, assertEqual, failTest, runTestSuite)

main :: IO ()
main = runTestSuite "TypedCoreExpressionDirectCall" tests

tests :: [NamedTest]
tests =
  [ ("audits the partial foundation fixture manifest", testFixtureManifest),
    ("produces unit and preserves ordinary inference", testUnitProduction),
    ("diagnostics take precedence over profile failures", testDiagnosticPrecedence),
    ("reports the initial input profile failures", testInputFailures)
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
