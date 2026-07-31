{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import JazzNext.Compiler.Bootstrap.TypedCoreExpressionDirectCallFixtures
import JazzNext.Compiler.AST (Expr (..), Literal (..), Statement (..))
import JazzNext.Compiler.DiagnosticCatalog (diagnosticCodeText)
import JazzNext.Compiler.Diagnostics
  ( SourceSpan (..),
    diagnosticCode,
    diagnosticSubject,
    isErrorDiagnostic,
  )
import JazzNext.Compiler.LoweredIR.Lower
import JazzNext.Compiler.LoweredIR.Validate (validateLoweredProgram)
import JazzNext.Compiler.ModuleExports (ModuleExport (..), exportInventory)
import JazzNext.Compiler.ModuleGraph (CoreModule (..), ResolvedModule (..))
import JazzNext.Compiler.Name (NameNamespace (ValueNamespace))
import JazzNext.Compiler.TypedCore
import JazzNext.Compiler.TypedCore.Validate (validateTypedProgram)
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
    ("lowers the complete scalar expression profile twice", testScalarLowering),
    ("retains every explicit numeric width while lowering", testExplicitNumericWidthLowering),
    ("lowers the full valid UInt64 domain twice", testFullUInt64Lowering),
    ("lowers nested scalar operands from left to right", testNestedScalarLowering),
    ("validates typed core before checking the lowering profile", testLoweringPrecedence),
    ("produces monomorphic functions and fully saturated direct calls twice", testDirectCallProduction),
    ("lowers monomorphic functions and fully saturated direct calls twice", testDirectCallLowering),
    ("rechecks every callable restriction on arbitrary valid typed programs", testLowererCallableBoundary),
    ("keeps forward visibility inside the typed-core production profile", testForwardVisibilityBoundary),
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
      "supplemental forward visibility fixtures"
      [ "forward-polymorphic-function-invisibility",
        "forward-constrained-function-invisibility",
        "forward-signed-scalar-invisibility",
        "forward-unsigned-lambda-invisibility"
      ]
      (map fixtureName forwardVisibilityNegativeFixtures)
    >> assertEqual
      "ordinary forward visibility fixture"
      "ordinary-unsigned-forward-caller-invisibility"
      (fixtureName ordinaryForwardVisibilityFixture)
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
      if name == "forward-direct-call-dag"
        then do
          assertUnboundName "ordinary forward direct call" "second" ordinary
          assertEqual
            "typed-core forward direct call diagnostics"
            []
            (filter isErrorDiagnostic (inferredDiagnostics (typedCoreProductionInferenceResult firstRun)))
        else assertEqual (name <> " inference compatibility") ordinary (typedCoreProductionInferenceResult firstRun)
      assertEqual
        (name <> " complete typed program")
        (TypedCoreProductionSucceeded expectedProgram)
        (typedCoreProductionStatus firstRun)

testDirectCallLowering :: IO ()
testDirectCallLowering =
  mapM_ assertLowered directCallExpectedLoweredPrograms
  where
    assertLowered (name, expectedProgram) = do
      let fixture = fixtureByName name
      firstProduction <- produceFixture fixture
      secondProduction <- produceFixture fixture
      case (typedCoreProductionStatus firstProduction, typedCoreProductionStatus secondProduction) of
        (TypedCoreProductionSucceeded firstProgram, TypedCoreProductionSucceeded secondProgram) -> do
          let firstLowering = lowerTypedCoreExpressionDirectCall firstProgram
              secondLowering = lowerTypedCoreExpressionDirectCall secondProgram
          assertEqual (name <> " permanently valid expected lowering") [] (validateLoweredProgram expectedProgram)
          assertEqual (name <> " repeatable lowering") firstLowering secondLowering
          assertEqual (name <> " complete lowered program") (LoweredIRSucceeded expectedProgram) firstLowering
        _ -> failTest (name <> " did not produce typed core for lowering")

testLowererCallableBoundary :: IO ()
testLowererCallableBoundary =
  mapM_ assertBoundary expectedResults
  where
    assertBoundary (name, expectedFailures) =
      case lookup name lowererBoundaryPrograms of
        Nothing -> failTest (name <> " lowerer boundary program is missing")
        Just programValue -> do
          let firstRun = lowerTypedCoreExpressionDirectCall programValue
              secondRun = lowerTypedCoreExpressionDirectCall programValue
          assertEqual (name <> " is permanently valid typed core") [] (validateTypedProgram programValue)
          assertEqual (name <> " repeatable lowerer rejection") firstRun secondRun
          assertEqual (name <> " exact lowerer rejection") (LoweredIRUnsupported expectedFailures) firstRun

    expectedResults =
      [ ( "invalid-function-shape",
          [ statementFailure 0
              LoweredIRInvalidFunctionShape
              (LoweredIRNameFailureDetail (currentName "seed"))
          ]
        ),
        ( "capturing-function",
          [ statementFailure 0
              LoweredIRInvalidFunctionShape
              (LoweredIRNameFailureDetail (currentName "seed")),
            expressionFailure 2 [0, 0, 1]
              LoweredIRCaptureUnsupported
              (LoweredIRNameFailureDetail (currentName "seed"))
          ]
        ),
        ( "duplicate-parameter-function",
          [ statementFailure 1
              LoweredIRInvalidFunctionShape
              (LoweredIRNameFailureDetail (currentName "chooseSecond"))
          ]
        ),
        ( "self-recursive-function",
          [ statementFailure 1
              LoweredIRRecursiveFunctionUnsupported
              (LoweredIRNameFailureDetail (currentName "loop"))
          ]
        ),
        ( "mutually-recursive-functions",
          [ statementFailure 1
              LoweredIRRecursiveFunctionUnsupported
              (LoweredIRNameFailureDetail (currentName "left")),
            statementFailure 3
              LoweredIRRecursiveFunctionUnsupported
              (LoweredIRNameFailureDetail (currentName "right"))
          ]
        ),
        ( "bare-function-value",
          [ callableModuleResultFailure,
            expressionFailure 2 [0]
              LoweredIRCallableValueUnsupported
              (LoweredIRNameFailureDetail (currentName "identity"))
          ]
        ),
        ( "partial-direct-call",
          [ callableModuleResultFailure,
            expressionFailure 2 [0]
              LoweredIRCallArityUnsupported
              (LoweredIRArityFailureDetail 2 1)
          ]
        ),
        ( "imported-direct-call",
          [ LoweredIRLoweringFailure
              TypedProgramPath
              LoweredIRUnsupportedProgram
              LoweredIRNoFailureDetail,
            LoweredIRLoweringFailure
              (TypedModulePath ["App", "Main"])
              LoweredIRUnsupportedModule
              LoweredIRNoFailureDetail,
            expressionFailure 0 [0]
              LoweredIRNonLocalCallUnsupported
              ( LoweredIRNameFailureDetail
                  (TypedResolvedName (TypedImportedModule ["Library", "Functions"]) TypedValueNamespace "foreign")
              )
          ]
        )
      ]
    statementFailure index kind detail =
      LoweredIRLoweringFailure
        (TypedStatementPath ["App", "Main"] [index])
        kind
        detail
    expressionFailure statementIndex childPath kind detail =
      LoweredIRLoweringFailure
        (TypedExpressionPath ["App", "Main"] [statementIndex] childPath)
        kind
        detail
    currentName = TypedResolvedName TypedCurrentModule TypedValueNamespace
    callableModuleResultFailure =
      LoweredIRLoweringFailure
        (TypedModulePath ["App", "Main"])
        LoweredIRUnsupportedRepresentation
        ( LoweredIRRecipeFailureDetail
            (TypedClosureRecipe [TypedSignedIntegerRecipe 64] (TypedSignedIntegerRecipe 64))
        )

testForwardVisibilityBoundary :: IO ()
testForwardVisibilityBoundary = do
  ordinary <-
    inferExpressionWithInputs
      (fixtureInputs ordinaryForwardVisibilityFixture)
      (coreModuleExpr (resolvedModuleCore (fixtureModule ordinaryForwardVisibilityFixture)))
  firstRun <- produceFixture ordinaryForwardVisibilityFixture
  secondRun <- produceFixture ordinaryForwardVisibilityFixture
  assertUnboundLater "ordinary unsigned forward caller" ordinary
  assertEqual
    "ordinary unsigned forward caller inference compatibility"
    ordinary
    (typedCoreProductionInferenceResult firstRun)
  assertEqual "ordinary unsigned forward caller repeatable production" firstRun secondRun
  assertEqual
    "ordinary unsigned forward caller blocks typed-core production"
    TypedCoreProductionBlockedByDiagnostics
    (typedCoreProductionStatus firstRun)
  mapM_ assertInvisible forwardVisibilityNegativeFixtures
  where
    assertInvisible fixture = do
      ordinary <-
        inferExpressionWithInputs
          (fixtureInputs fixture)
          (coreModuleExpr (resolvedModuleCore (fixtureModule fixture)))
      firstRun <- produceFixture fixture
      secondRun <- produceFixture fixture
      assertUnboundLater (fixtureName fixture <> " ordinary invisibility") ordinary
      assertEqual
        (fixtureName fixture <> " inference compatibility")
        ordinary
        (typedCoreProductionInferenceResult firstRun)
      assertEqual
        (fixtureName fixture <> " repeatable production")
        firstRun
        secondRun
      assertEqual
        (fixtureName fixture <> " blocks typed-core production")
        TypedCoreProductionBlockedByDiagnostics
        (typedCoreProductionStatus firstRun)

assertUnboundLater :: Text -> InferenceResult -> IO ()
assertUnboundLater label = assertUnboundName label "later"

assertUnboundName :: Text -> Text -> InferenceResult -> IO ()
assertUnboundName label name inferenceResult =
  assertEqual
    (label <> " reports " <> name <> " as unbound")
    True
    ( ("E1001", Just name)
        `elem` [ (diagnosticCodeText (diagnosticCode diagnostic), diagnosticSubject diagnostic)
               | diagnostic <- inferredDiagnostics inferenceResult,
                 isErrorDiagnostic diagnostic
               ]
    )

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

testScalarLowering :: IO ()
testScalarLowering =
  mapM_ assertLowered scalarExpectedLoweredPrograms
  where
    assertLowered (name, expectedProgram) = do
      let fixture = fixtureByName name
      firstProduction <- produceFixture fixture
      secondProduction <- produceFixture fixture
      case (typedCoreProductionStatus firstProduction, typedCoreProductionStatus secondProduction) of
        (TypedCoreProductionSucceeded firstProgram, TypedCoreProductionSucceeded secondProgram) -> do
          let firstLowering = lowerTypedCoreExpressionDirectCall firstProgram
              secondLowering = lowerTypedCoreExpressionDirectCall secondProgram
          assertEqual (name <> " repeatable lowering") firstLowering secondLowering
          assertEqual (name <> " complete lowered program") (LoweredIRSucceeded expectedProgram) firstLowering
        _ -> failTest (name <> " did not produce typed core for lowering")

testNestedScalarLowering :: IO ()
testNestedScalarLowering =
  assertEqual
    "nested scalar lowering"
    (LoweredIRSucceeded expectedNestedScalarLoweredProgram)
    (lowerTypedCoreExpressionDirectCall nestedScalarTypedProgram)

testExplicitNumericWidthLowering :: IO ()
testExplicitNumericWidthLowering =
  mapM_
    ( \(name, typedProgram, expectedProgram) ->
        let firstRun = lowerTypedCoreExpressionDirectCall typedProgram
            secondRun = lowerTypedCoreExpressionDirectCall typedProgram
         in assertEqual (name <> " repeatable lowering") firstRun secondRun
              >> assertEqual
                (name <> " exact lowering")
                (LoweredIRSucceeded expectedProgram)
                firstRun
    )
    explicitNumericScalarLoweringPrograms

testFullUInt64Lowering :: IO ()
testFullUInt64Lowering =
  mapM_
    ( \(name, typedProgram, expectedProgram) ->
        let firstRun = lowerTypedCoreExpressionDirectCall typedProgram
            secondRun = lowerTypedCoreExpressionDirectCall typedProgram
         in assertEqual (name <> " permanent validation") [] (validateLoweredProgram expectedProgram)
              >> assertEqual (name <> " repeatable lowering") firstRun secondRun
              >> assertEqual
                (name <> " exact lowering")
                (LoweredIRSucceeded expectedProgram)
                firstRun
    )
    fullUInt64ScalarLoweringPrograms

testLoweringPrecedence :: IO ()
testLoweringPrecedence =
  assertEqual
    "typed-core validation precedes lowering profile checks"
    ( LoweredIRTypedCoreFailures
        [ TypedCoreValidationFailure
            TypedProgramPath
            TypedUnknownEntryModule
            (TypedTextDetail "Missing::Entry")
        ]
    )
    (lowerTypedCoreExpressionDirectCall (TypedProgram Nothing [] ["Missing", "Entry"]))

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
