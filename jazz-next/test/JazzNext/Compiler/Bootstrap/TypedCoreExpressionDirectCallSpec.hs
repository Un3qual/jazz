{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import JazzNext.Compiler.Bootstrap.TypedCoreExpressionDirectCallFixtures
import JazzNext.Compiler.AST (Expr (..), Literal (..), Statement (..))
import JazzNext.Compiler.Diagnostics (SourceSpan (..))
import JazzNext.Compiler.ModuleExports (ModuleExport (..), exportInventory)
import JazzNext.Compiler.ModuleGraph (CoreModule (..), ResolvedModule (..))
import JazzNext.Compiler.Name (NameNamespace (ValueNamespace))
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
  [ ("audits the complete producer fixture manifest", testFixtureManifest),
    ("produces unit and preserves ordinary inference", testUnitProduction),
    ("produces the complete scalar expression profile twice", testScalarProduction),
    ("produces monomorphic functions and fully saturated direct calls twice", testDirectCallProduction),
    ("rejects the complete callable profile twice", testRejectedCallableProfile),
    ("reports rejected scalar profile nodes twice", testRejectedScalarProfile),
    ("diagnostics take precedence over profile failures", testDiagnosticPrecedence),
    ("reports the initial input profile failures", testInputFailures),
    ("reports every additional foundation profile failure", testAdditionalProfileFailures)
  ]

testFixtureManifest :: IO ()
testFixtureManifest =
  assertEqual
    "accepted fixture names"
    [ "unit-entry",
      "bool-entry",
      "char-entry",
      "default-int-entry",
      "default-float-entry",
      "explicit-numeric-widths",
      "arithmetic-operators",
      "ordering-operators",
      "equality-operators",
      "scalar-parameter-return",
      "single-argument-direct-call",
      "curried-multi-argument-direct-call",
      "forward-direct-call-dag",
      "nested-direct-calls",
      "dollar-direct-call",
      "exported-direct-function"
    ]
    acceptedFixtureNames
    >> assertEqual
      "rejected fixture names"
      [ "source-diagnostic",
        "invalid-portable-source-path",
        "resolved-import",
        "ambient-prelude-input",
        "text-value",
        "list-value",
        "non-unit-tuple",
        "data-value",
        "conditional",
        "pattern-case",
        "local-block-binding",
        "bare-function-value",
        "partial-direct-call",
        "oversaturated-direct-call",
        "capturing-function",
        "self-recursive-function",
        "mutually-recursive-functions",
        "polymorphic-or-evidence-function",
        "imported-direct-call",
        "user-defined-operator-call"
      ]
      rejectedFixtureNames
    >> assertEqual "fixture order" (acceptedFixtureNames <> rejectedFixtureNames) fixtureNames
    >> assertEqual "accepted fixture count" 16 (length acceptedFixtureNames)
    >> assertEqual "rejected fixture count" 20 (length rejectedFixtureNames)
    >> assertEqual "unique fixture count" 36 (Set.size (Set.fromList fixtureNames))
    >> assertEqual
      "explicit numeric widths"
      ["Int8", "Int16", "Int32", "Int64", "UInt8", "UInt16", "UInt32", "UInt64", "Float16", "Float32", "Float64"]
      explicitNumericTypes
    >> assertEqual
      "admitted operators"
      ["+", "-", "*", "/", "<", "<=", ">", ">=", "==", "!="]
      admittedOperators

testDirectCallProduction :: IO ()
testDirectCallProduction =
  mapM_ assertProduced directCallExpectedPrograms
  where
    assertProduced (name, expectedProgram) = do
      let fixture = fixtureByName name
      ordinary <- inferExpressionWithInputs (fixtureInputs fixture) (coreModuleExpr (resolvedModuleCore (fixtureModule fixture)))
      firstRun <- produceFixture fixture
      secondRun <- produceFixture fixture
      assertEqual (name <> " repeatable production") firstRun secondRun
      assertEqual (name <> " inference compatibility") ordinary (typedCoreProductionInferenceResult firstRun)
      assertEqual
        (name <> " complete typed program")
        (TypedCoreProductionSucceeded expectedProgram)
        (typedCoreProductionStatus firstRun)

testRejectedCallableProfile :: IO ()
testRejectedCallableProfile =
  mapM_ assertRejected callableExpectedStatuses
    >> assertEqual
      "callable rejection failure-kind coverage"
      True
      ( all (`elem` actualKinds) expectedKinds
          && all (`elem` expectedKinds) actualKinds
      )
  where
    expectedKinds =
      [ TypedCoreImportedInputsUnsupported,
        TypedCoreUnsupportedRootExpression,
        TypedCoreUserDefinedOperatorUnsupported,
        TypedCoreCallableValueUnsupported,
        TypedCoreCallArityUnsupported,
        TypedCoreCaptureUnsupported,
        TypedCoreRecursiveFunctionUnsupported,
        TypedCoreNonMonomorphicFunctionUnsupported,
        TypedCoreUnresolvedExpressionType
      ]
    actualKinds =
      [ kind
      | (_, TypedCoreProductionUnsupported failures) <- callableExpectedStatuses,
        TypedCoreProductionFailure _ kind _ <- failures
      ]
    assertRejected (name, expectedStatus) = do
      let fixture = fixtureByName name
      ordinary <- inferExpressionWithInputs (fixtureInputs fixture) (coreModuleExpr (resolvedModuleCore (fixtureModule fixture)))
      firstRun <- produceFixture fixture
      secondRun <- produceFixture fixture
      assertEqual (name <> " repeatable rejection") firstRun secondRun
      assertEqual (name <> " rejection inference compatibility") ordinary (typedCoreProductionInferenceResult firstRun)
      assertEqual (name <> " complete rejection") expectedStatus (typedCoreProductionStatus firstRun)

callableExpectedStatuses :: [(Text, TypedCoreProductionStatus)]
callableExpectedStatuses =
  [ ( "bare-function-value",
      unsupported
        [expressionFailure 2 [] TypedCoreCallableValueUnsupported (TypedCoreNameDetail "identity")]
    ),
    ( "partial-direct-call",
      unsupported
        [expressionFailure 2 [] TypedCoreCallArityUnsupported (TypedCoreArityDetail 2 1)]
    ),
    ( "oversaturated-direct-call",
      unsupported
        [ expressionFailure 1 [0, 0] TypedCoreUserDefinedOperatorUnsupported TypedCoreUnsupportedRootDetail,
          expressionFailure 2 [] TypedCoreCallArityUnsupported (TypedCoreArityDetail 1 2)
        ]
    ),
    ( "capturing-function",
      unsupported
        [ statementFailure 1 TypedCoreUnsupportedRootExpression TypedCoreUnsupportedRootDetail,
          expressionFailure 3 [0, 0, 1] TypedCoreCaptureUnsupported (TypedCoreNameDetail "seed")
        ]
    ),
    ( "self-recursive-function",
      unsupported
        [statementFailure 1 TypedCoreRecursiveFunctionUnsupported (TypedCoreNameDetail "loop")]
    ),
    ( "mutually-recursive-functions",
      unsupported
        [ statementFailure 1 TypedCoreRecursiveFunctionUnsupported (TypedCoreNameDetail "left"),
          statementFailure 3 TypedCoreRecursiveFunctionUnsupported (TypedCoreNameDetail "right")
        ]
    ),
    ( "polymorphic-or-evidence-function",
      unsupported
        [ expressionFailure 0 [] TypedCoreUnresolvedExpressionType TypedCoreUnsupportedRootDetail,
          statementFailure 1 TypedCoreNonMonomorphicFunctionUnsupported (TypedCoreNameDetail "identity"),
          expressionFailure 1 [] TypedCoreUnresolvedExpressionType TypedCoreUnsupportedRootDetail,
          expressionFailure 1 [0] TypedCoreUnresolvedExpressionType TypedCoreUnsupportedRootDetail,
          expressionFailure 2 [] TypedCoreUnresolvedExpressionType TypedCoreUnsupportedRootDetail
        ]
    ),
    ( "imported-direct-call",
      unsupported
        [TypedCoreProductionFailure TypedCoreProductionInputPath TypedCoreImportedInputsUnsupported TypedCoreNoFailureDetail]
    ),
    ( "user-defined-operator-call",
      unsupported
        [expressionFailure 2 [] TypedCoreUserDefinedOperatorUnsupported TypedCoreUnsupportedRootDetail]
    )
  ]
  where
    unsupported = TypedCoreProductionUnsupported
    expressionFailure statementIndex childPath kind detail =
      TypedCoreProductionFailure
        (TypedCoreProductionExpressionPath ["App", "Main"] statementIndex childPath)
        kind
        detail
    statementFailure statementIndex kind detail =
      TypedCoreProductionFailure
        (TypedCoreProductionStatementPath ["App", "Main"] statementIndex)
        kind
        detail

produceFixture :: Fixture -> IO TypedCoreProductionResult
produceFixture fixture =
  inferResolvedModuleTypedCoreWithProfile
    TypedCoreExpressionDirectCallProfile
    (fixtureInputs fixture)
    (fixtureSourcePath fixture)
    (fixtureModule fixture)

testScalarProduction :: IO ()
testScalarProduction =
  mapM_ assertScalar scalarExpectedPrograms
  where
    assertScalar (name, expectedProgram) =
      case filter ((== name) . fixtureName) scalarFixtures of
        [fixture] -> do
          ordinary <- inferExpressionWithInputs (fixtureInputs fixture) (coreModuleExpr (resolvedModuleCore (fixtureModule fixture)))
          firstResult <- produceFixture fixture
          secondResult <- produceFixture fixture
          let firstRun = typedCoreProductionStatus firstResult
              secondRun = typedCoreProductionStatus secondResult
          assertEqual (name <> " repeatable production") firstRun secondRun
          assertEqual (name <> " scalar inference compatibility") ordinary (typedCoreProductionInferenceResult firstResult)
          assertEqual (name <> " scalar program") (TypedCoreProductionSucceeded expectedProgram) firstRun
        _ -> failTest (name <> " scalar fixture is missing")

testRejectedScalarProfile :: IO ()
testRejectedScalarProfile =
  mapM_ assertRejected
    [ ( "text-value",
        [ profileFailure 0 TypedCoreManagedValueUnsupported TypedCoreTextValueDetail,
          profileFailure 1 TypedCoreStructuredValueUnsupported TypedCoreListValueDetail
        ]
      ),
      ("list-value", [profileFailure 0 TypedCoreStructuredValueUnsupported TypedCoreListValueDetail]),
      ("non-unit-tuple", [profileFailure 0 TypedCoreStructuredValueUnsupported TypedCoreTupleValueDetail]),
      ("data-value", [profileFailure 0 TypedCoreStructuredValueUnsupported TypedCoreDataValueDetail]),
      ("conditional", [profileFailure 0 TypedCoreControlFlowUnsupported TypedCoreConditionalDetail]),
      ("pattern-case", [profileFailure 0 TypedCorePatternCaseUnsupported TypedCorePatternCaseDetail]),
      ("local-block-binding", [profileFailure 0 TypedCoreNestedBlockUnsupported TypedCoreLocalBlockDetail])
    ]
  where
    assertRejected (name, expectedFailures) =
      case filter ((== name) . fixtureName) rejectedScalarFixtures of
        [fixture] -> do
          ordinary <- inferExpressionWithInputs (fixtureInputs fixture) (coreModuleExpr (resolvedModuleCore (fixtureModule fixture)))
          firstResult <- produceFixture fixture
          secondResult <- produceFixture fixture
          let firstRun = typedCoreProductionStatus firstResult
              secondRun = typedCoreProductionStatus secondResult
          assertEqual (name <> " repeatable rejection") firstRun secondRun
          assertEqual (name <> " scalar rejection inference compatibility") ordinary (typedCoreProductionInferenceResult firstResult)
          assertEqual
            (name <> " production failure")
            (TypedCoreProductionUnsupported expectedFailures)
            firstRun
        _ -> failTest (name <> " rejected fixture is missing")

    profileFailure statementIndex failureKind failureDetail =
      TypedCoreProductionFailure
        (TypedCoreProductionExpressionPath ["App", "Main"] statementIndex [])
        failureKind
        failureDetail

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
  ordinary <-
    inferExpressionWithInputs
      (fixtureInputs sourceDiagnosticFixture)
      (coreModuleExpr (resolvedModuleCore (fixtureModule sourceDiagnosticFixture)))
  sourceDiagnosticResult <-
    inferResolvedModuleTypedCoreWithProfile
      TypedCoreExpressionDirectCallProfile
      (fixtureInputs sourceDiagnosticFixture)
      (fixtureSourcePath sourceDiagnosticFixture)
      (fixtureModule sourceDiagnosticFixture)
  assertEqual "diagnostics take precedence" TypedCoreProductionBlockedByDiagnostics (typedCoreProductionStatus sourceDiagnosticResult)
  assertEqual "diagnostic inference compatibility" ordinary (typedCoreProductionInferenceResult sourceDiagnosticResult)

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
          nonLocalCall =
            unitFixture
              { fixtureModule =
                  withExpression
                    ( EBlock
                        [ SExpr
                            (SourceSpan 1 1)
                            (EApply (EVar "__kernel_toInt8") (ELit (LInt 1)))
                        ]
                    )
                    unitFixture
              }
          unsupportedExport =
            unitFixture
              { fixtureModule =
                  (fixtureModule unitFixture)
                    { resolvedModuleExportInventory =
                        exportInventory [ModuleExport ValueNamespace "missing"]
                    }
              }
          leadingStatement =
            unitFixture
              { fixtureModule =
                  withExpression
                    (EBlock [SLet "ignored" (SourceSpan 1 1) (ETuple []), SExpr (SourceSpan 2 1) (ETuple [])])
                    unitFixture
              }
          inputFailure = [TypedCoreProductionFailure TypedCoreProductionInputPath TypedCoreImportedInputsUnsupported TypedCoreNoFailureDetail]
          rootExpressionFailure = [TypedCoreProductionFailure (TypedCoreProductionExpressionPath ["App", "Main"] 0 []) TypedCoreUnsupportedRootExpression TypedCoreUnsupportedRootDetail]
          statementFailure = [TypedCoreProductionFailure (TypedCoreProductionStatementPath ["App", "Main"] 0) TypedCoreUnsupportedRootExpression TypedCoreUnsupportedRootDetail]
      assertUnsupported pathMismatch [TypedCoreProductionFailure TypedCoreProductionInputPath TypedCoreModulePathMismatch TypedCoreNoFailureDetail]
      assertUnsupported importedValue inputFailure
      assertUnsupported importedData inputFailure
      assertUnsupported importedCapabilities inputFailure
      assertUnsupported unsupportedRoot rootExpressionFailure
      assertUnsupported
        nonLocalCall
        [TypedCoreProductionFailure (TypedCoreProductionExpressionPath ["App", "Main"] 0 []) TypedCoreNonLocalCallUnsupported (TypedCoreNameDetail "__kernel_toInt8")]
      assertUnsupported
        unsupportedExport
        [TypedCoreProductionFailure (TypedCoreProductionModulePath ["App", "Main"]) TypedCoreUnsupportedExport (TypedCoreNameDetail "missing")]
      assertUnsupported leadingStatement statementFailure
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
