{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.IORef (modifyIORef', newIORef, readIORef)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Jazz.Compiler.AST (DataConstructor (..), Expr (..), Literal (..), Statement (..))
import Jazz.Compiler.Bootstrap.TypedCoreExpressionDirectCallFixtures
import Jazz.Compiler.DiagnosticCatalog (diagnosticCodeText)
import Jazz.Compiler.Diagnostics
  ( SourceSpan (..),
    diagnosticCode,
    diagnosticSubject,
    isErrorDiagnostic,
  )
import Jazz.Compiler.LoweredIR.Lower
import Jazz.Compiler.LoweredIR.Validate (validateLoweredProgram)
import Jazz.Compiler.ModuleExports (ModuleExport (..), ModuleExportSelector (..), exportInventory)
import Jazz.Compiler.ModuleGraph (CoreModule (..), DeclaredModuleExports (..), ResolvedModule (..))
import Jazz.Compiler.Name (NameNamespace (ValueNamespace))
import Jazz.Compiler.TypeInference
import Jazz.Compiler.TypeInference.Types
  ( DataTypeBinding (..),
    ExpressionType (TBoolType),
    ScopeCapabilityFacts (..),
    TypeBinding (PlainTypeBinding),
    emptyScopeCapabilityFacts,
  )
import Jazz.Compiler.TypedCore
import Jazz.Compiler.TypedCore.Validate (validateTypedProgram)
import Jazz.TestHarness (NamedTest, assertEqual, failTest, runTestSuite)

main :: IO ()
main = runTestSuite "TypedCoreExpressionDirectCall" tests

tests :: [NamedTest]
tests =
  [ ("audits the complete producer fixture manifest", testFixtureManifest),
    ("audits the independent typed-core lowerer manifests", testIndependentLowererManifest),
    ("audits every producer failure kind used by the rejected manifest", testRejectedManifestProducerFailures),
    ("runs every accepted manifest fixture through its current opt-in boundary", testAcceptedManifestPipeline),
    ("retains every explicit numeric width while lowering", testExplicitNumericWidthLowering),
    ("lowers the full valid UInt64 domain twice", testFullUInt64Lowering),
    ("lowers nested scalar operands from left to right", testNestedScalarLowering),
    ("validates typed core before checking the lowering profile", testLoweringPrecedence),
    ("uses the exact RFC closure environment identity grammar", testRfcClosureEnvironmentIdentity),
    ("lowers every independent unary closure boundary twice", testSupportedClosureLowererBoundary),
    ("rechecks every callable restriction on arbitrary valid typed programs", testLowererCallableBoundary),
    ("rejects malformed callable recipes and binder references before lowering", testInvalidLowererTypedCoreBoundary),
    ("rechecks lowerer-only structural boundaries on arbitrary valid typed programs", testLowererStructuralBoundary),
    ("keeps forward visibility inside the typed-core production profile", testForwardVisibilityBoundary),
    ("admits concrete unit-typed forward functions", testUnitForwardVisibility),
    ("reports curried argument failures at their real expression paths", testCurriedArgumentFailurePath),
    ("retains supplied argument failures when direct-call arity is invalid", testInvalidArityArgumentFailureAccumulation),
    ("retains supplied argument failures when non-local calls are rejected", testNonLocalCallArgumentFailureAccumulation),
    ("retains closure-call argument and later sibling failures in source order", testClosureUseArgumentFailureOrder),
    ("collapses mixed callable-use reasons to one closure classification", testClosureShapeClassificationCollapse),
    ("specializes integer literals to direct-call parameter types", testNarrowLiteralDirectCall),
    ("specializes computed integer results to declared return types", testNarrowCompositeFunctionResult),
    ("specializes comparison operands to their unified numeric type", testNarrowComparisonOperand),
    ("specializes terminal binary operands to their unified numeric type", testNarrowRootBinaryDirectCall),
    ("normalizes equivalent scalar aliases to the expected type", testEquivalentScalarAliasSpecialization),
    ("rejects unused user-defined operator bindings", testUnusedUserDefinedOperatorBinding),
    ("retains root data failures with their real statement paths", testRootDataFailureAccumulation),
    ("retains nested data-block child failures in structural order", testNestedDataFailureAccumulation),
    ("rejects anonymous lambdas as module results", testAnonymousLambdaResultRejection),
    ("keeps invalid signed forward declarations visible to analysis", testInvalidForwardDeclarationAnalysisVisibility),
    ("preserves ordinary diagnostics while producing typed core", testProductionDiagnosticCompatibility),
    ("rejects class and impl declarations from the scalar direct-call profile", testUnsupportedDeclarationProfile),
    ("retains impl method body profile failures", testImplMethodBodyFailureAccumulation),
    ("retains unsupported binding markers with initializer failures", testUnsupportedBindingFailureAccumulation),
    ("preserves qualified-method application inference in rejected profiles", testQualifiedMethodInferenceCompatibility),
    ("rejects uncommitted integer ranges outside Int64", testOutOfRangeDefaultIntegerRejection),
    ("rejects numeric promotions that typed core cannot represent", testNumericPromotionRejection),
    ("rejects the complete callable profile twice", testRejectedCallableProfile),
    ("reports rejected scalar profile nodes twice", testRejectedScalarProfile),
    ("rejects modules without an executable result twice", testMissingModuleResultProduction),
    ("retains statement failures when the module result is missing", testMissingResultFailureAccumulation),
    ("ranks module failures before statement failures in authored export order", testModuleFailureOrder),
    ("keeps exported callable-info failures statement-owned and unique", testExportedCallableFailureOwnership),
    ("retains unsupported compound child failures in structural order", testCompoundFailureAccumulation),
    ("retains every unsupported composite child failure in structural order", testUnsupportedCompositeFailureAccumulation),
    ("rejects ambiguous producer binder identities twice", testProducerIdentityBoundary),
    ("orders same-statement recursion before rebinding and descendants", testSameStatementFailureKindOrder),
    ("preserves recursion dependencies through rejected producer trees", testRejectedProducerDependencyTransport),
    ("diagnostics take precedence over profile failures", testDiagnosticPrecedence),
    ("reports the initial input profile failures", testInputFailures),
    ("ranks every input failure before resolved-module failures", testInputModuleFailureOrder),
    ("reports every additional foundation profile failure", testAdditionalProfileFailures)
  ]

testFixtureManifest :: IO ()
testFixtureManifest = do
  let acceptedSet = Set.fromList acceptedFixtureNames
      rejectedSet = Set.fromList rejectedFixtureNames
      completeSet = Set.fromList fixtureNames
      priorSet = Set.fromList priorScalarDirectCallFixtureNames
      expectedAcceptedNames =
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
          "exported-direct-function",
          "named-function-value",
          "higher-order-call",
          "closure-result",
          "callable-parameter-shadows-named-function",
          "callable-parameter-shadows-enclosing-function",
          "mixed-direct-and-value-use",
          "callable-parameter-value-shadows-enclosing-function"
        ]
      expectedRejectedNames =
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
          "partial-direct-call",
          "oversaturated-direct-call",
          "capturing-function",
          "self-recursive-function",
          "mutually-recursive-functions",
          "closure-value-mutual-recursion",
          "closure-value-self-recursion",
          "polymorphic-or-evidence-function",
          "imported-direct-call",
          "user-defined-operator-call"
        ]
  assertEqual "accepted source fixture names" expectedAcceptedNames acceptedFixtureNames
  assertEqual "rejected source fixture names" expectedRejectedNames rejectedFixtureNames
  assertEqual "fixture order" (acceptedFixtureNames <> rejectedFixtureNames) fixtureNames
    >> assertEqual "accepted fixture count" 23 (length acceptedFixtureNames)
    >> assertEqual "rejected fixture count" 21 (length rejectedFixtureNames)
    >> assertEqual "unique fixture count" 44 (Set.size (Set.fromList fixtureNames))
    >> assertEqual "accepted and rejected source fixtures are disjoint" Set.empty (Set.intersection acceptedSet rejectedSet)
    >> assertEqual "accepted and rejected source fixtures are exhaustive" (Set.fromList (expectedAcceptedNames <> expectedRejectedNames)) (Set.union acceptedSet rejectedSet)
    >> assertEqual "prior scalar/direct-call inventory count" 36 (Set.size priorSet)
    >> assertEqual "every prior scalar/direct-call fixture remains present" True (priorSet `Set.isSubsetOf` completeSet)
    >> assertEqual
      "supplemental forward visibility fixtures"
      [ "forward-polymorphic-function-invisibility",
        "forward-constrained-function-invisibility",
        "forward-signed-scalar-invisibility",
        "forward-unsigned-lambda-invisibility",
        "nested-forward-signed-function-invisibility"
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
    >> assertEqual "fixture entries follow the complete ordered manifest" fixtureNames (map fixtureName fixtures)

testIndependentLowererManifest :: IO ()
testIndependentLowererManifest = do
  let validNames = map fst validIndependentLowererPrograms
      invalidNames = map fst invalidLowererBoundaryPrograms
      completeNames = map fst independentLowererPrograms
      validSet = Set.fromList validNames
      invalidSet = Set.fromList invalidNames
      expectedValidNames =
        [ "invalid-function-shape",
          "invalid-function-shape-rhs",
          "combined-statement-failure-order",
          "recursion-descendant-failure-order",
          "closure-valued-parameter",
          "closure-valued-result",
          "closure-shaped-named-function",
          "closure-shaped-named-application",
          "callable-parameter-shadows-top-level-lowerer",
          "callable-parameter-value-shadows-enclosing-function-lowerer",
          "direct-flattened-representation",
          "non-concrete-closure-representation",
          "callable-shape-body-disagreement",
          "duplicate-parameter-function",
          "duplicate-function-identity",
          "capturing-function",
          "self-recursive-function",
          "closure-shaped-self-recursive-function",
          "mutually-recursive-functions",
          "closure-value-mutual-recursion",
          "closure-value-self-recursion",
          "nested-lambda-closure-value-self-recursion",
          "direct-shaped-closure-value-self-recursion",
          "shape-rejected-self-recursion",
          "shape-rejected-mutual-recursion",
          "shape-rejected-binder-shadow-control",
          "bare-function-value",
          "partial-direct-call",
          "imported-direct-call",
          "managed-scalar-entry",
          "conditional-entry"
        ]
      expectedInvalidNames =
        [ "closure-shape-flattened-recipe",
          "direct-shape-staged-recipe",
          "variable-binder-reference-mismatch"
        ]
  assertEqual "valid independent typed-core fixture names" expectedValidNames validNames
  assertEqual "invalid independent typed-core fixture names" expectedInvalidNames invalidNames
  assertEqual "independent typed-core fixture names are unique" (length completeNames) (Set.size (Set.fromList completeNames))
  assertEqual "valid and invalid independent typed-core fixtures are disjoint" Set.empty (Set.intersection validSet invalidSet)
  assertEqual "valid and invalid independent typed-core fixtures are exhaustive" (expectedValidNames <> expectedInvalidNames) completeNames
  assertEqual
    "valid independent lowerer fixtures contain no malformed typed core"
    []
    [ (name, failures)
    | (name, programValue) <- validIndependentLowererPrograms,
      let failures = validateTypedProgram programValue,
      not (null failures)
    ]
  assertEqual
    "invalid independent lowerer fixtures all fail typed-core validation"
    []
    [ name
    | (name, programValue) <- invalidLowererBoundaryPrograms,
      null (validateTypedProgram programValue)
    ]

testRejectedManifestProducerFailures :: IO ()
testRejectedManifestProducerFailures = do
  outcomes <- mapM runTwice rejectedManifestExpectedStatuses
  assertEqual "rejected manifest has complete outcome coverage" rejectedFixtureNames (map fst rejectedManifestExpectedStatuses)
  assertEqual
    "rejected manifest producer failure kinds"
    rejectedManifestFailureKinds
    (map (\(name, _, result) -> (name, statusFailureKinds result)) outcomes)
  where
    runTwice (name, expectedStatus) = do
      let fixture = fixtureByName name
      firstRun <- produceFixture fixture
      secondRun <- produceFixture fixture
      assertEqual (name <> " manifest rejection repeatability") firstRun secondRun
      assertEqual (name <> " first complete rejection") expectedStatus (typedCoreProductionStatus firstRun)
      assertEqual (name <> " second complete rejection") expectedStatus (typedCoreProductionStatus secondRun)
      pure (name, expectedStatus, typedCoreProductionStatus firstRun)

testAcceptedManifestPipeline :: IO ()
testAcceptedManifestPipeline =
  mapM_ assertAccepted acceptedFixtureNames
  where
    expectedTypedPrograms =
      [("unit-entry", expectedUnitProgram)]
        <> scalarExpectedPrograms
        <> directCallExpectedPrograms
        <> closedCallableExpectedPrograms
    expectedLoweredPrograms =
      scalarExpectedLoweredPrograms
        <> directCallExpectedLoweredPrograms
        <> closedCallableExpectedLoweredPrograms

    assertAccepted name =
      case lookup name expectedTypedPrograms of
        Just expectedTypedProgram -> do
          let fixture = fixtureByName name
          ordinary <- inferFixture fixture
          (firstProduction, lookupPaths) <- produceFixtureWithTrace fixture
          secondProduction <- produceFixture fixture
          assertEqual (name <> " resolver source lookup") ["src/App/Main.jz"] lookupPaths
          assertEqual (name <> " repeatable typed production") firstProduction secondProduction
          if name == "forward-direct-call-dag"
            then do
              assertUnboundName "ordinary forward direct call" "second" ordinary
              assertEqual
                "typed-core forward direct call diagnostics"
                []
                (filter isErrorDiagnostic (inferredDiagnostics (typedCoreProductionInferenceResult firstProduction)))
            else assertEqual (name <> " inference compatibility") ordinary (typedCoreProductionInferenceResult firstProduction)
          assertEqual
            (name <> " complete typed production")
            (TypedCoreProductionSucceeded expectedTypedProgram)
            (typedCoreProductionStatus firstProduction)
          assertEqual (name <> " expected typed validation") [] (validateTypedProgram expectedTypedProgram)
          case typedCoreProductionStatus firstProduction of
            TypedCoreProductionSucceeded typedProgram -> do
              assertEqual (name <> " produced typed validation") [] (validateTypedProgram typedProgram)
              case lookup name expectedLoweredPrograms of
                Just expectedLoweredProgram -> do
                  let lowering = lowerTypedCoreExpressionDirectCall typedProgram
                  assertEqual
                    (name <> " complete lowered production")
                    (LoweredIRSucceeded expectedLoweredProgram)
                    lowering
                  case lowering of
                    LoweredIRSucceeded loweredProgram ->
                      assertEqual (name <> " lowered validation") [] (validateLoweredProgram loweredProgram)
                    _ -> failTest (name <> " did not produce lowered IR")
                Nothing -> failTest (name <> " is missing a lowered-program expectation")
            _ -> failTest (name <> " did not produce typed core")
        Nothing -> failTest (name <> " is missing a typed-program expectation")

testRfcClosureEnvironmentIdentity :: IO ()
testRfcClosureEnvironmentIdentity = do
  let (typedProgram, expectedProgram) = rfcClosureEnvironmentIdentityProgram
      firstRun = lowerTypedCoreExpressionDirectCall typedProgram
      secondRun = lowerTypedCoreExpressionDirectCall typedProgram
  assertEqual "RFC identity typed-core validation" [] (validateTypedProgram typedProgram)
  assertEqual "RFC identity repeatable lowering" firstRun secondRun
  assertEqual "RFC identity exact lowering" (LoweredIRSucceeded expectedProgram) firstRun

testSupportedClosureLowererBoundary :: IO ()
testSupportedClosureLowererBoundary =
  mapM_ assertSupported independentClosureExpectedLoweredPrograms
  where
    assertSupported (name, expectedProgram) =
      case lookup name lowererBoundaryPrograms of
        Nothing -> failTest (name <> " supported lowerer program is missing")
        Just programValue -> do
          let firstRun = lowerTypedCoreExpressionDirectCall programValue
              secondRun = lowerTypedCoreExpressionDirectCall programValue
          assertEqual (name <> " is permanently valid typed core") [] (validateTypedProgram programValue)
          assertEqual (name <> " repeatable closure lowering") firstRun secondRun
          assertEqual (name <> " exact closure lowering") (LoweredIRSucceeded expectedProgram) firstRun
          assertEqual (name <> " expected lowered validation") [] (validateLoweredProgram expectedProgram)

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
          [ statementFailure
              0
              LoweredIRInvalidFunctionShape
              (LoweredIRNameFailureDetail (currentName "seed"))
          ]
        ),
        ( "invalid-function-shape-rhs",
          [ statementFailure
              0
              LoweredIRInvalidFunctionShape
              (LoweredIRNameFailureDetail (currentName "seed")),
            expressionFailure
              0
              [0]
              LoweredIRUnsupportedExpression
              LoweredIRNoFailureDetail
          ]
        ),
        ( "combined-statement-failure-order",
          [ statementFailure
              0
              LoweredIRInvalidFunctionShape
              (LoweredIRNameFailureDetail (currentName "seed")),
            expressionFailure
              0
              [0]
              LoweredIRUnsupportedExpression
              LoweredIRNoFailureDetail,
            statementFailure
              1
              LoweredIRInvalidFunctionShape
              (LoweredIRNameFailureDetail (currentName "message")),
            expressionFailure
              1
              [0]
              LoweredIRUnsupportedRepresentation
              (LoweredIRRecipeFailureDetail TypedManagedTextRecipe)
          ]
        ),
        ( "recursion-descendant-failure-order",
          [ statementFailure
              0
              LoweredIRInvalidFunctionShape
              (LoweredIRNameFailureDetail (currentName "seed")),
            statementFailure
              2
              LoweredIRRecursiveFunctionUnsupported
              (LoweredIRNameFailureDetail (currentName "loop")),
            expressionFailure
              2
              [0, 0, 1]
              LoweredIRCaptureUnsupported
              (LoweredIRNameFailureDetail (currentName "seed"))
          ]
        ),
        ( "direct-flattened-representation",
          [ expressionFailure
              2
              [0]
              LoweredIRCallableValueUnsupported
              (LoweredIRNameFailureDetail (currentName "combine"))
          ]
        ),
        ( "non-concrete-closure-representation",
          [ statementFailure
              1
              LoweredIRInvalidFunctionShape
              (LoweredIRNameFailureDetail (currentName "identity"))
          ]
        ),
        ( "callable-shape-body-disagreement",
          [ statementFailure
              3
              LoweredIRInvalidFunctionShape
              (LoweredIRNameFailureDetail (currentName "choose"))
          ]
        ),
        ( "capturing-function",
          [ statementFailure
              0
              LoweredIRInvalidFunctionShape
              (LoweredIRNameFailureDetail (currentName "seed")),
            expressionFailure
              2
              [0, 0, 1]
              LoweredIRCaptureUnsupported
              (LoweredIRNameFailureDetail (currentName "seed"))
          ]
        ),
        ( "duplicate-parameter-function",
          [ expressionFailure
              1
              [0, 0]
              LoweredIRDuplicateParameterIdentity
              (LoweredIRNameFailureDetail (currentName "item"))
          ]
        ),
        ( "duplicate-function-identity",
          [ statementFailure
              3
              LoweredIRDuplicateFunctionIdentity
              (LoweredIRNameFailureDetail (currentName "identity"))
          ]
        ),
        ( "self-recursive-function",
          [ statementFailure
              1
              LoweredIRRecursiveFunctionUnsupported
              (LoweredIRNameFailureDetail (currentName "loop"))
          ]
        ),
        ( "closure-shaped-self-recursive-function",
          [ statementFailure
              1
              LoweredIRRecursiveFunctionUnsupported
              (LoweredIRNameFailureDetail (currentName "loop"))
          ]
        ),
        ( "mutually-recursive-functions",
          [ statementFailure
              1
              LoweredIRRecursiveFunctionUnsupported
              (LoweredIRNameFailureDetail (currentName "left")),
            statementFailure
              3
              LoweredIRRecursiveFunctionUnsupported
              (LoweredIRNameFailureDetail (currentName "right"))
          ]
        ),
        ( "closure-value-mutual-recursion",
          [ statementFailure
              3
              LoweredIRRecursiveFunctionUnsupported
              (LoweredIRNameFailureDetail (currentName "left")),
            statementFailure
              5
              LoweredIRRecursiveFunctionUnsupported
              (LoweredIRNameFailureDetail (currentName "right"))
          ]
        ),
        ( "closure-value-self-recursion",
          [ statementFailure
              3
              LoweredIRRecursiveFunctionUnsupported
              (LoweredIRNameFailureDetail (currentName "loop"))
          ]
        ),
        ( "nested-lambda-closure-value-self-recursion",
          [ statementFailure
              3
              LoweredIRRecursiveFunctionUnsupported
              (LoweredIRNameFailureDetail (currentName "loop")),
            expressionFailure
              3
              [0, 0, 1]
              LoweredIRUnsupportedExpression
              LoweredIRNoFailureDetail
          ]
        ),
        ( "direct-shaped-closure-value-self-recursion",
          [ statementFailure
              3
              LoweredIRRecursiveFunctionUnsupported
              (LoweredIRNameFailureDetail (currentName "loop")),
            expressionFailure
              3
              [0, 0, 1]
              LoweredIRCallableValueUnsupported
              (LoweredIRNameFailureDetail (currentName "loop"))
          ]
        ),
        ( "shape-rejected-self-recursion",
          [ statementFailure
              1
              LoweredIRRecursiveFunctionUnsupported
              (LoweredIRNameFailureDetail (currentName "loop")),
            statementFailure
              1
              LoweredIRInvalidFunctionShape
              (LoweredIRNameFailureDetail (currentName "loop")),
            expressionFailure
              1
              [0]
              LoweredIRUnsupportedExpression
              LoweredIRNoFailureDetail
          ]
        ),
        ( "shape-rejected-mutual-recursion",
          [ statementFailure
              1
              LoweredIRRecursiveFunctionUnsupported
              (LoweredIRNameFailureDetail (currentName "left")),
            statementFailure
              1
              LoweredIRInvalidFunctionShape
              (LoweredIRNameFailureDetail (currentName "left")),
            expressionFailure
              1
              [0]
              LoweredIRUnsupportedExpression
              LoweredIRNoFailureDetail,
            statementFailure
              3
              LoweredIRRecursiveFunctionUnsupported
              (LoweredIRNameFailureDetail (currentName "right")),
            statementFailure
              3
              LoweredIRInvalidFunctionShape
              (LoweredIRNameFailureDetail (currentName "right")),
            expressionFailure
              3
              [0]
              LoweredIRUnsupportedExpression
              LoweredIRNoFailureDetail
          ]
        ),
        ( "shape-rejected-binder-shadow-control",
          [ statementFailure
              1
              LoweredIRInvalidFunctionShape
              (LoweredIRNameFailureDetail (currentName "loop")),
            expressionFailure
              1
              [0]
              LoweredIRUnsupportedExpression
              LoweredIRNoFailureDetail
          ]
        ),
        ( "bare-function-value",
          [ expressionFailure
              2
              [0]
              LoweredIRCallableValueUnsupported
              (LoweredIRNameFailureDetail (currentName "identity"))
          ]
        ),
        ( "partial-direct-call",
          [ expressionFailure
              2
              [0]
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
            expressionFailure
              0
              [0]
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

testInvalidLowererTypedCoreBoundary :: IO ()
testInvalidLowererTypedCoreBoundary =
  mapM_ assertBoundary expectedResults
  where
    assertBoundary (name, expectedFailures) =
      case lookup name invalidLowererBoundaryPrograms of
        Nothing -> failTest (name <> " invalid lowerer boundary program is missing")
        Just programValue -> do
          let firstRun = lowerTypedCoreExpressionDirectCall programValue
              secondRun = lowerTypedCoreExpressionDirectCall programValue
          assertEqual (name <> " repeatable typed-core rejection") firstRun secondRun
          assertEqual (name <> " exact typed-core rejection") (LoweredIRTypedCoreFailures expectedFailures) firstRun

    expectedResults =
      [ ( "closure-shape-flattened-recipe",
          [ callableShapeFailure 0,
            callableShapeFailure 1
          ]
        ),
        ( "direct-shape-staged-recipe",
          [ callableShapeFailure 0,
            callableShapeFailure 1
          ]
        ),
        ( "variable-binder-reference-mismatch",
          [ TypedCoreValidationFailure
              (TypedExpressionPath ["App", "Main"] [2] [0])
              TypedBinderReferenceMismatch
              (TypedBinderDetail (TypedBinderId (["App", "Main"], [999], currentName "identity")))
          ]
        )
      ]
    callableShapeFailure statementIndex =
      TypedCoreValidationFailure
        (TypedStatementPath ["App", "Main"] [statementIndex])
        TypedCallableShapeMismatch
        ( TypedBinderDetail
            (TypedBinderId (["App", "Main"], [statementIndex], currentName "combine"))
        )
    currentName = TypedResolvedName TypedCurrentModule TypedValueNamespace

testLowererStructuralBoundary :: IO ()
testLowererStructuralBoundary =
  mapM_ assertBoundary expectedResults
  where
    assertBoundary (name, expectedFailures) =
      case lookup name lowererStructuralBoundaryPrograms of
        Nothing -> failTest (name <> " lowerer structural boundary program is missing")
        Just programValue -> do
          let firstRun = lowerTypedCoreExpressionDirectCall programValue
              secondRun = lowerTypedCoreExpressionDirectCall programValue
          assertEqual (name <> " is permanently valid typed core") [] (validateTypedProgram programValue)
          assertEqual (name <> " repeatable lowerer rejection") firstRun secondRun
          assertEqual (name <> " exact lowerer rejection") (LoweredIRUnsupported expectedFailures) firstRun

    expectedResults =
      [ ( "managed-scalar-entry",
          [ LoweredIRLoweringFailure
              (TypedModulePath ["App", "Main"])
              LoweredIRUnsupportedRepresentation
              (LoweredIRRecipeFailureDetail TypedManagedTextRecipe),
            LoweredIRLoweringFailure
              (TypedExpressionPath ["App", "Main"] [0] [0])
              LoweredIRUnsupportedRepresentation
              (LoweredIRRecipeFailureDetail TypedManagedTextRecipe)
          ]
        ),
        ( "conditional-entry",
          [ LoweredIRLoweringFailure
              (TypedExpressionPath ["App", "Main"] [0] [0])
              LoweredIRUnsupportedExpression
              LoweredIRNoFailureDetail
          ]
        )
      ]

testForwardVisibilityBoundary :: IO ()
testForwardVisibilityBoundary = do
  ordinary <- inferFixture ordinaryForwardVisibilityFixture
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
      ordinary <- inferFixture fixture
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

testUnitForwardVisibility :: IO ()
testUnitForwardVisibility = do
  let fixture = producerEdgeFixture "unit-forward-function"
  result <- produceFixture fixture
  assertEqual
    "unit forward function diagnostics"
    []
    (filter isErrorDiagnostic (inferredDiagnostics (typedCoreProductionInferenceResult result)))
  case typedCoreProductionStatus result of
    TypedCoreProductionSucceeded programValue ->
      assertEqual "unit forward function typed-core validation" [] (validateTypedProgram programValue)
    _ -> failTest "unit forward function did not produce typed core"

testCurriedArgumentFailurePath :: IO ()
testCurriedArgumentFailurePath = do
  let fixture = producerEdgeFixture "curried-first-argument-capture"
      expected =
        TypedCoreProductionUnsupported
          [ TypedCoreProductionFailure
              (TypedCoreProductionStatementPath ["App", "Main"] 1)
              TypedCoreUnsupportedRootExpression
              TypedCoreUnsupportedRootDetail,
            TypedCoreProductionFailure
              (TypedCoreProductionExpressionPath ["App", "Main"] 5 [0, 0, 0, 1])
              TypedCoreCaptureUnsupported
              (TypedCoreNameDetail "seed")
          ]
  result <- produceFixture fixture
  assertEqual "curried first-argument capture path" expected (typedCoreProductionStatus result)

testInvalidArityArgumentFailureAccumulation :: IO ()
testInvalidArityArgumentFailureAccumulation = do
  let fixture = producerEdgeFixture "partial-call-argument-capture"
      expected =
        TypedCoreProductionUnsupported
          [ TypedCoreProductionFailure
              (TypedCoreProductionStatementPath ["App", "Main"] 1)
              TypedCoreUnsupportedRootExpression
              TypedCoreUnsupportedRootDetail,
            TypedCoreProductionFailure
              (TypedCoreProductionExpressionPath ["App", "Main"] 4 [])
              TypedCoreCallArityUnsupported
              (TypedCoreArityDetail 2 1),
            TypedCoreProductionFailure
              (TypedCoreProductionExpressionPath ["App", "Main"] 4 [1])
              TypedCoreCaptureUnsupported
              (TypedCoreNameDetail "seed")
          ]
  ordinary <- inferFixture fixture
  firstRun <- produceFixture fixture
  secondRun <- produceFixture fixture
  assertEqual
    "partial-call argument failure inference compatibility"
    ordinary
    (typedCoreProductionInferenceResult firstRun)
  assertEqual "partial-call argument failure repeatability" firstRun secondRun
  assertEqual "partial-call argument failure accumulation" expected (typedCoreProductionStatus firstRun)

testNonLocalCallArgumentFailureAccumulation :: IO ()
testNonLocalCallArgumentFailureAccumulation = do
  let fixture = producerEdgeFixture "non-local-call-argument-capture"
      expected =
        TypedCoreProductionUnsupported
          [ TypedCoreProductionFailure
              (TypedCoreProductionStatementPath ["App", "Main"] 1)
              TypedCoreUnsupportedRootExpression
              TypedCoreUnsupportedRootDetail,
            TypedCoreProductionFailure
              (TypedCoreProductionExpressionPath ["App", "Main"] 2 [])
              TypedCoreNonLocalCallUnsupported
              (TypedCoreNameDetail "__kernel_toFloat64"),
            TypedCoreProductionFailure
              (TypedCoreProductionExpressionPath ["App", "Main"] 2 [1])
              TypedCoreCaptureUnsupported
              (TypedCoreNameDetail "seed")
          ]
  ordinary <- inferFixture fixture
  firstRun <- produceFixture fixture
  secondRun <- produceFixture fixture
  assertEqual
    "non-local-call argument failure inference compatibility"
    ordinary
    (typedCoreProductionInferenceResult firstRun)
  assertEqual "non-local-call argument failure repeatability" firstRun secondRun
  assertEqual "non-local-call argument failure accumulation" expected (typedCoreProductionStatus firstRun)

testClosureUseArgumentFailureOrder :: IO ()
testClosureUseArgumentFailureOrder = do
  let fixture = producerEdgeFixture "closure-use-argument-failure-order"
      expected =
        TypedCoreProductionUnsupported
          [ TypedCoreProductionFailure
              (TypedCoreProductionStatementPath ["App", "Main"] 1)
              TypedCoreUnsupportedRootExpression
              TypedCoreUnsupportedRootDetail,
            TypedCoreProductionFailure
              (TypedCoreProductionExpressionPath ["App", "Main"] 3 [0, 0, 1])
              TypedCoreCaptureUnsupported
              (TypedCoreNameDetail "seed"),
            TypedCoreProductionFailure
              (TypedCoreProductionExpressionPath ["App", "Main"] 7 [])
              TypedCoreStructuredValueUnsupported
              TypedCoreListValueDetail
          ]
  ordinary <- inferFixture fixture
  firstRun <- produceFixture fixture
  secondRun <- produceFixture fixture
  assertEqual
    "closure-use argument failure inference compatibility"
    ordinary
    (typedCoreProductionInferenceResult firstRun)
  assertEqual "closure-use argument failure repeatability" firstRun secondRun
  assertEqual "closure-use argument and later sibling failure order" expected (typedCoreProductionStatus firstRun)

testClosureShapeClassificationCollapse :: IO ()
testClosureShapeClassificationCollapse = do
  let fixture = fixtureByName "mixed-direct-and-value-use"
      expectedShapes =
        [ ("apply", TypedDirectCallableShape),
          ("apply", TypedDirectCallableShape),
          ("identity", TypedClosureCallableShape),
          ("identity", TypedClosureCallableShape)
        ]
  firstRun <- produceFixture fixture
  secondRun <- produceFixture fixture
  assertEqual "mixed callable-use classification repeatability" firstRun secondRun
  case typedCoreProductionStatus firstRun of
    TypedCoreProductionSucceeded (TypedProgram _ [TypedModule _ _ _ _ _ statements _] _) ->
      assertEqual "mixed callable-use scheme classifications" expectedShapes (callableSchemeShapes statements)
    status -> failTest ("mixed callable-use fixture did not produce typed core: " <> Text.pack (show status))
  where
    callableSchemeShapes statements =
      [ (identifier, shape)
      | statement <- statements,
        (name, TypedScheme _ _ _ _ _ _ (Just shape)) <- case statement of
          TypedSignatureStatement _ name _ schemeValue -> [(name, schemeValue)]
          TypedLetStatement _ name _ schemeValue _ -> [(name, schemeValue)]
          _ -> [],
        TypedResolvedName TypedCurrentModule TypedValueNamespace identifier <- [name]
      ]

testNarrowLiteralDirectCall :: IO ()
testNarrowLiteralDirectCall =
  assertCompleteProduction "narrow literal direct call" (producerEdgeFixture "narrow-literal-direct-call")

testNarrowCompositeFunctionResult :: IO ()
testNarrowCompositeFunctionResult =
  assertCompleteProduction "narrow composite function result" (producerEdgeFixture "narrow-composite-function-result")

testNarrowComparisonOperand :: IO ()
testNarrowComparisonOperand =
  assertCompleteProduction "narrow comparison operand" (producerEdgeFixture "narrow-comparison-operand")

testNarrowRootBinaryDirectCall :: IO ()
testNarrowRootBinaryDirectCall =
  assertCompleteProduction "narrow root binary direct call" (producerEdgeFixture "narrow-root-binary-direct-call")

testEquivalentScalarAliasSpecialization :: IO ()
testEquivalentScalarAliasSpecialization =
  assertCompleteProduction
    "equivalent scalar alias specialization"
    (producerEdgeFixture "equivalent-scalar-alias-specialization")

testUnusedUserDefinedOperatorBinding :: IO ()
testUnusedUserDefinedOperatorBinding = do
  let fixture = producerEdgeFixture "unused-user-defined-operator"
      expected =
        TypedCoreProductionUnsupported
          [ TypedCoreProductionFailure
              (TypedCoreProductionStatementPath ["App", "Main"] 1)
              TypedCoreUserDefinedOperatorUnsupported
              TypedCoreUnsupportedRootDetail
          ]
  ordinary <- inferFixture fixture
  firstRun <- produceFixture fixture
  secondRun <- produceFixture fixture
  assertEqual
    "unused user-defined operator inference compatibility"
    ordinary
    (typedCoreProductionInferenceResult firstRun)
  assertEqual "unused user-defined operator repeatable production" firstRun secondRun
  assertEqual "unused user-defined operator binding rejection" expected (typedCoreProductionStatus firstRun)

testRootDataFailureAccumulation :: IO ()
testRootDataFailureAccumulation = do
  let fixture = producerEdgeFixture "root-data-failure-accumulation"
      expected =
        TypedCoreProductionUnsupported
          [ TypedCoreProductionFailure
              (TypedCoreProductionExpressionPath ["App", "Main"] 0 [])
              TypedCoreStructuredValueUnsupported
              TypedCoreListValueDetail,
            TypedCoreProductionFailure
              (TypedCoreProductionStatementPath ["App", "Main"] 1)
              TypedCoreStructuredValueUnsupported
              TypedCoreDataValueDetail
          ]
  ordinary <- inferFixture fixture
  firstRun <- produceFixture fixture
  secondRun <- produceFixture fixture
  assertEqual
    "root data failure inference compatibility"
    ordinary
    (typedCoreProductionInferenceResult firstRun)
  assertEqual "root data failure repeatable production" firstRun secondRun
  assertEqual "root data failure structural order" expected (typedCoreProductionStatus firstRun)

testNestedDataFailureAccumulation :: IO ()
testNestedDataFailureAccumulation =
  case fixtures of
    fixture : _ -> do
      resolvedModule <- resolveFixtureModule fixture
      let spanValue = SourceSpan 1 1
          nestedBlock =
            EBlock
              [ SExpr spanValue (EList [ELit (LInt 1)]),
                SData spanValue "Box" [] [DataConstructor "Box" []],
                SExpr spanValue (ETuple [])
              ]
          forgedModule =
            withExpression
              (EBlock [SExpr spanValue nestedBlock])
              resolvedModule
          expected =
            TypedCoreProductionUnsupported
              [ TypedCoreProductionFailure
                  (TypedCoreProductionExpressionPath ["App", "Main"] 0 [])
                  TypedCoreStructuredValueUnsupported
                  TypedCoreDataValueDetail,
                TypedCoreProductionFailure
                  (TypedCoreProductionExpressionPath ["App", "Main"] 0 [0])
                  TypedCoreStructuredValueUnsupported
                  TypedCoreListValueDetail
              ]
      firstRun <- produceResolvedFixture fixture forgedModule
      secondRun <- produceResolvedFixture fixture forgedModule
      assertEqual "nested data failure repeatable production" firstRun secondRun
      assertEqual "nested data failure structural order" expected (typedCoreProductionStatus firstRun)
    [] -> failTest "unit fixture is missing"

testAnonymousLambdaResultRejection :: IO ()
testAnonymousLambdaResultRejection = do
  let fixture = producerEdgeFixture "anonymous-lambda-result"
      expected =
        TypedCoreProductionUnsupported
          [ TypedCoreProductionFailure
              (TypedCoreProductionExpressionPath ["App", "Main"] 0 [])
              TypedCoreCallableValueUnsupported
              TypedCoreUnsupportedRootDetail
          ]
  ordinary <- inferFixture fixture
  firstRun <- produceFixture fixture
  secondRun <- produceFixture fixture
  assertEqual
    "anonymous lambda result inference compatibility"
    ordinary
    (typedCoreProductionInferenceResult firstRun)
  assertEqual "anonymous lambda result repeatable production" firstRun secondRun
  assertEqual "anonymous lambda module result rejection" expected (typedCoreProductionStatus firstRun)

assertCompleteProduction :: Text -> Fixture -> IO ()
assertCompleteProduction label fixture = do
  ordinary <- inferFixture fixture
  firstRun <- produceFixture fixture
  secondRun <- produceFixture fixture
  assertEqual (label <> " repeatable production") firstRun secondRun
  assertEqual
    (label <> " inference compatibility")
    ordinary
    (typedCoreProductionInferenceResult firstRun)
  assertEqual
    (label <> " diagnostics")
    []
    (filter isErrorDiagnostic (inferredDiagnostics (typedCoreProductionInferenceResult firstRun)))
  case typedCoreProductionStatus firstRun of
    TypedCoreProductionSucceeded programValue -> do
      assertEqual (label <> " typed-core validation") [] (validateTypedProgram programValue)
      case lowerTypedCoreExpressionDirectCall programValue of
        LoweredIRSucceeded loweredProgram ->
          assertEqual (label <> " lowered-IR validation") [] (validateLoweredProgram loweredProgram)
        _ -> failTest (label <> " did not lower successfully")
    status ->
      failTest
        ( label
            <> " did not produce typed core: "
            <> Text.pack (show status)
        )

testProductionDiagnosticCompatibility :: IO ()
testProductionDiagnosticCompatibility = do
  let fixture = producerEdgeFixture "out-of-range-signed-function-literal"
  ordinary <- inferFixture fixture
  firstRun <- produceFixture fixture
  secondRun <- produceFixture fixture
  assertEqual
    "out-of-range signed function reports one ordinary type error"
    ["E2005"]
    [ diagnosticCodeText (diagnosticCode diagnostic)
    | diagnostic <- inferredDiagnostics ordinary,
      isErrorDiagnostic diagnostic
    ]
  assertEqual
    "out-of-range signed function inference compatibility"
    ordinary
    (typedCoreProductionInferenceResult firstRun)
  assertEqual "out-of-range signed function repeatable production" firstRun secondRun
  assertEqual
    "out-of-range signed function blocks typed-core production"
    TypedCoreProductionBlockedByDiagnostics
    (typedCoreProductionStatus firstRun)

testInvalidForwardDeclarationAnalysisVisibility :: IO ()
testInvalidForwardDeclarationAnalysisVisibility = do
  let fixture = producerEdgeFixture "invalid-forward-signed-function"
  firstRun <- produceFixture fixture
  secondRun <- produceFixture fixture
  assertEqual
    "invalid signed forward declaration production diagnostics"
    ["E2006"]
    [ diagnosticCodeText (diagnosticCode diagnostic)
    | diagnostic <- inferredDiagnostics (typedCoreProductionInferenceResult firstRun),
      isErrorDiagnostic diagnostic
    ]
  assertEqual "invalid signed forward declaration repeatable production" firstRun secondRun
  assertEqual
    "invalid signed forward declaration blocks typed-core production"
    TypedCoreProductionBlockedByDiagnostics
    (typedCoreProductionStatus firstRun)

testUnsupportedDeclarationProfile :: IO ()
testUnsupportedDeclarationProfile = do
  let fixture = producerEdgeFixture "class-impl-declarations"
      expected =
        TypedCoreProductionUnsupported
          [ unsupportedStatement 0,
            unsupportedStatement 1
          ]
  ordinary <- inferFixture fixture
  firstRun <- produceFixture fixture
  secondRun <- produceFixture fixture
  assertEqual
    "class and impl declaration ordinary diagnostics"
    []
    (filter isErrorDiagnostic (inferredDiagnostics ordinary))
  assertEqual
    "class and impl declaration inference compatibility"
    ordinary
    (typedCoreProductionInferenceResult firstRun)
  assertEqual "class and impl declaration repeatable production" firstRun secondRun
  assertEqual "class and impl declaration failures" expected (typedCoreProductionStatus firstRun)
  where
    unsupportedStatement statementIndex =
      TypedCoreProductionFailure
        (TypedCoreProductionStatementPath ["App", "Main"] statementIndex)
        TypedCoreUnsupportedRootExpression
        TypedCoreUnsupportedRootDetail

testImplMethodBodyFailureAccumulation :: IO ()
testImplMethodBodyFailureAccumulation = do
  let fixture = producerEdgeFixture "impl-method-profile-failure"
      expected =
        TypedCoreProductionUnsupported
          [ unsupportedStatement 0,
            unsupportedStatement 1,
            TypedCoreProductionFailure
              (TypedCoreProductionExpressionPath ["App", "Main"] 1 [0, 0])
              TypedCoreStructuredValueUnsupported
              TypedCoreListValueDetail
          ]
  ordinary <- inferFixture fixture
  firstRun <- produceFixture fixture
  secondRun <- produceFixture fixture
  assertEqual
    "impl method body ordinary diagnostics"
    []
    (filter isErrorDiagnostic (inferredDiagnostics ordinary))
  assertEqual
    "impl method body inference compatibility"
    ordinary
    (typedCoreProductionInferenceResult firstRun)
  assertEqual "impl method body repeatable production" firstRun secondRun
  assertEqual "impl method body complete failure accumulation" expected (typedCoreProductionStatus firstRun)
  where
    unsupportedStatement statementIndex =
      TypedCoreProductionFailure
        (TypedCoreProductionStatementPath ["App", "Main"] statementIndex)
        TypedCoreUnsupportedRootExpression
        TypedCoreUnsupportedRootDetail

testUnsupportedBindingFailureAccumulation :: IO ()
testUnsupportedBindingFailureAccumulation = do
  let fixture = producerEdgeFixture "unsupported-binding-child-failure"
      expected =
        TypedCoreProductionUnsupported
          [ TypedCoreProductionFailure
              (TypedCoreProductionStatementPath ["App", "Main"] 0)
              TypedCoreUnsupportedRootExpression
              TypedCoreUnsupportedRootDetail,
            TypedCoreProductionFailure
              (TypedCoreProductionExpressionPath ["App", "Main"] 0 [0])
              TypedCoreStructuredValueUnsupported
              TypedCoreListValueDetail
          ]
  ordinary <- inferFixture fixture
  firstRun <- produceFixture fixture
  secondRun <- produceFixture fixture
  assertEqual
    "unsupported binding ordinary diagnostics"
    []
    (filter isErrorDiagnostic (inferredDiagnostics ordinary))
  assertEqual
    "unsupported binding inference compatibility"
    ordinary
    (typedCoreProductionInferenceResult firstRun)
  assertEqual "unsupported binding repeatable production" firstRun secondRun
  assertEqual "unsupported binding complete failure accumulation" expected (typedCoreProductionStatus firstRun)

testQualifiedMethodInferenceCompatibility :: IO ()
testQualifiedMethodInferenceCompatibility = do
  let fixture = producerEdgeFixture "qualified-method-profile-rejection"
      expected =
        TypedCoreProductionUnsupported
          [ unsupportedStatement 0,
            unsupportedStatement 1,
            unsupportedStatement 2,
            TypedCoreProductionFailure
              (TypedCoreProductionExpressionPath ["App", "Main"] 3 [])
              TypedCoreNonLocalCallUnsupported
              (TypedCoreNameDetail "Choice::pick")
          ]
  ordinary <- inferFixture fixture
  firstRun <- produceFixture fixture
  secondRun <- produceFixture fixture
  assertEqual
    "qualified method ordinary diagnostics"
    []
    (filter isErrorDiagnostic (inferredDiagnostics ordinary))
  assertEqual
    "qualified method inference compatibility"
    ordinary
    (typedCoreProductionInferenceResult firstRun)
  assertEqual "qualified method repeatable production" firstRun secondRun
  assertEqual "qualified method profile failures" expected (typedCoreProductionStatus firstRun)
  where
    unsupportedStatement statementIndex =
      TypedCoreProductionFailure
        (TypedCoreProductionStatementPath ["App", "Main"] statementIndex)
        TypedCoreUnsupportedRootExpression
        TypedCoreUnsupportedRootDetail

testOutOfRangeDefaultIntegerRejection :: IO ()
testOutOfRangeDefaultIntegerRejection =
  mapM_ assertRejected ["out-of-range-default-integer", "out-of-range-default-integer-binary"]
  where
    assertRejected name = do
      let fixture = producerEdgeFixture name
          expected =
            TypedCoreProductionUnsupported
              [ TypedCoreProductionFailure
                  (TypedCoreProductionExpressionPath ["App", "Main"] 0 [])
                  TypedCoreUnresolvedExpressionType
                  TypedCoreUnsupportedRootDetail
              ]
      ordinary <- inferFixture fixture
      firstRun <- produceFixture fixture
      secondRun <- produceFixture fixture
      assertEqual
        (name <> " ordinary diagnostics")
        []
        (filter isErrorDiagnostic (inferredDiagnostics ordinary))
      assertEqual
        (name <> " inference compatibility")
        ordinary
        (typedCoreProductionInferenceResult firstRun)
      assertEqual (name <> " repeatable production") firstRun secondRun
      assertEqual (name <> " structured rejection") expected (typedCoreProductionStatus firstRun)

testNumericPromotionRejection :: IO ()
testNumericPromotionRejection =
  mapM_
    assertRejected
    [ ("integer-literal-float64-promotion", 0, []),
      ("integer-literal-float64-equality", 0, []),
      ("signed-parameter-float64-promotion", 1, [0, 0, 0])
    ]
  where
    assertRejected (name, statementIndex, childPath) = do
      let fixture = producerEdgeFixture name
          expected =
            TypedCoreProductionUnsupported
              [ TypedCoreProductionFailure
                  (TypedCoreProductionExpressionPath ["App", "Main"] statementIndex childPath)
                  TypedCoreUnsupportedRootExpression
                  TypedCoreUnsupportedRootDetail
              ]
      ordinary <- inferFixture fixture
      firstRun <- produceFixture fixture
      secondRun <- produceFixture fixture
      assertEqual
        (name <> " ordinary diagnostics")
        []
        (filter isErrorDiagnostic (inferredDiagnostics ordinary))
      assertEqual
        (name <> " inference compatibility")
        ordinary
        (typedCoreProductionInferenceResult firstRun)
      assertEqual (name <> " repeatable production") firstRun secondRun
      assertEqual (name <> " structured rejection") expected (typedCoreProductionStatus firstRun)

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
      ordinary <- inferFixture fixture
      firstRun <- produceFixture fixture
      secondRun <- produceFixture fixture
      assertEqual (name <> " repeatable rejection") firstRun secondRun
      assertEqual (name <> " rejection inference compatibility") ordinary (typedCoreProductionInferenceResult firstRun)
      assertEqual (name <> " complete rejection") expectedStatus (typedCoreProductionStatus firstRun)

callableExpectedStatuses :: [(Text, TypedCoreProductionStatus)]
callableExpectedStatuses =
  map expectedStatus callableRejectionNames
  where
    expectedStatus name =
      case lookup name rejectedManifestExpectedStatuses of
        Just status -> (name, status)
        Nothing -> error ("callable rejection is missing from the rejected manifest: " <> show name)

callableRejectionNames :: [Text]
callableRejectionNames =
  [ "partial-direct-call",
    "oversaturated-direct-call",
    "capturing-function",
    "self-recursive-function",
    "mutually-recursive-functions",
    "closure-value-mutual-recursion",
    "closure-value-self-recursion",
    "polymorphic-or-evidence-function",
    "imported-direct-call",
    "user-defined-operator-call"
  ]

rejectedManifestExpectedStatuses :: [(Text, TypedCoreProductionStatus)]
rejectedManifestExpectedStatuses =
  [ ("source-diagnostic", TypedCoreProductionBlockedByDiagnostics),
    ( "invalid-portable-source-path",
      unsupported [TypedCoreProductionFailure TypedCoreProductionInputPath TypedCoreInvalidPortableSourcePath TypedCoreNoFailureDetail]
    ),
    ( "resolved-import",
      unsupported [TypedCoreProductionFailure (TypedCoreProductionModulePath ["App", "Main"]) TypedCoreResolvedImportsUnsupported TypedCoreNoFailureDetail]
    ),
    ( "ambient-prelude-input",
      unsupported [TypedCoreProductionFailure TypedCoreProductionInputPath TypedCoreAmbientPreludeInputUnsupported TypedCoreNoFailureDetail]
    ),
    ( "text-value",
      unsupported
        [ expressionFailure 0 [] TypedCoreManagedValueUnsupported TypedCoreTextValueDetail,
          expressionFailure 1 [] TypedCoreStructuredValueUnsupported TypedCoreListValueDetail
        ]
    ),
    ("list-value", unsupported [expressionFailure 0 [] TypedCoreStructuredValueUnsupported TypedCoreListValueDetail]),
    ("non-unit-tuple", unsupported [expressionFailure 0 [] TypedCoreStructuredValueUnsupported TypedCoreTupleValueDetail]),
    ( "data-value",
      unsupported
        [ statementFailure 0 TypedCoreStructuredValueUnsupported TypedCoreDataValueDetail,
          expressionFailure 1 [] TypedCoreStructuredValueUnsupported TypedCoreDataValueDetail
        ]
    ),
    ("conditional", unsupported [expressionFailure 0 [] TypedCoreControlFlowUnsupported TypedCoreConditionalDetail]),
    ("pattern-case", unsupported [expressionFailure 0 [] TypedCorePatternCaseUnsupported TypedCorePatternCaseDetail]),
    ("local-block-binding", unsupported [expressionFailure 0 [] TypedCoreNestedBlockUnsupported TypedCoreLocalBlockDetail]),
    ("partial-direct-call", unsupported [expressionFailure 2 [] TypedCoreCallArityUnsupported (TypedCoreArityDetail 2 1)]),
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
    ("self-recursive-function", unsupported [statementFailure 1 TypedCoreRecursiveFunctionUnsupported (TypedCoreNameDetail "loop")]),
    ( "mutually-recursive-functions",
      unsupported
        [ statementFailure 1 TypedCoreRecursiveFunctionUnsupported (TypedCoreNameDetail "left"),
          statementFailure 3 TypedCoreRecursiveFunctionUnsupported (TypedCoreNameDetail "right")
        ]
    ),
    ( "closure-value-mutual-recursion",
      unsupported
        [ statementFailure 3 TypedCoreRecursiveFunctionUnsupported (TypedCoreNameDetail "left"),
          statementFailure 5 TypedCoreRecursiveFunctionUnsupported (TypedCoreNameDetail "right")
        ]
    ),
    ( "closure-value-self-recursion",
      unsupported [statementFailure 3 TypedCoreRecursiveFunctionUnsupported (TypedCoreNameDetail "loop")]
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
      unsupported [TypedCoreProductionFailure TypedCoreProductionInputPath TypedCoreImportedInputsUnsupported TypedCoreNoFailureDetail]
    ),
    ( "user-defined-operator-call",
      unsupported
        [ statementFailure 1 TypedCoreUserDefinedOperatorUnsupported TypedCoreUnsupportedRootDetail,
          expressionFailure 2 [] TypedCoreUserDefinedOperatorUnsupported TypedCoreUnsupportedRootDetail
        ]
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

rejectedManifestFailureKinds :: [(Text, [TypedCoreProductionFailureKind])]
rejectedManifestFailureKinds =
  [ (name, statusFailureKinds status)
  | (name, status) <- rejectedManifestExpectedStatuses
  ]

statusFailureKinds :: TypedCoreProductionStatus -> [TypedCoreProductionFailureKind]
statusFailureKinds status =
  case status of
    TypedCoreProductionBlockedByDiagnostics -> []
    TypedCoreProductionUnsupported failures ->
      [ kind
      | TypedCoreProductionFailure _ kind _ <- failures
      ]
    TypedCoreProductionInvariantFailures _ -> []
    TypedCoreProductionSucceeded _ -> []

resolveFixtureModule :: Fixture -> IO ResolvedModule
resolveFixtureModule fixture = do
  result <- resolveFixture fixture
  case result of
    Left failures ->
      failTest
        ( fixtureName fixture
            <> " did not resolve through the module resolver: "
            <> Text.pack (show failures)
        )
    Right resolvedModule -> pure resolvedModule

inferFixture :: Fixture -> IO InferenceResult
inferFixture fixture = do
  resolvedModule <- resolveFixtureModule fixture
  inferExpressionWithInputs (fixtureInputs fixture) (coreModuleExpr (resolvedModuleCore resolvedModule))

produceFixture :: Fixture -> IO TypedCoreProductionResult
produceFixture fixture = do
  resolvedModule <- resolveFixtureModule fixture
  produceResolvedFixture fixture resolvedModule

produceFixtureWithTrace :: Fixture -> IO (TypedCoreProductionResult, [FilePath])
produceFixtureWithTrace fixture = do
  lookupPaths <- newIORef []
  resolvedResult <- resolveFixtureWithLookup fixture $ \path -> do
    modifyIORef' lookupPaths (<> [path])
    pure (Map.lookup path (fixtureSourceFiles fixture))
  resolvedModule <-
    case resolvedResult of
      Left _ -> failTest (fixtureName fixture <> " did not resolve through the module resolver")
      Right value -> pure value
  productionResult <- produceResolvedFixture fixture resolvedModule
  lookupPathsValue <- readIORef lookupPaths
  pure (productionResult, lookupPathsValue)

produceResolvedFixture :: Fixture -> ResolvedModule -> IO TypedCoreProductionResult
produceResolvedFixture fixture resolvedModule =
  inferResolvedModuleTypedCoreExpressionDirectCall
    (fixtureInputs fixture)
    (fixtureSourcePath fixture)
    resolvedModule

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
  mapM_ assertRejected rejectedScalarFixtures
  where
    assertRejected fixture = do
      let name = fixtureName fixture
          expectedStatus =
            case lookup name rejectedManifestExpectedStatuses of
              Just status -> status
              Nothing -> error ("scalar rejection is missing from the rejected manifest: " <> show name)
      ordinary <- inferFixture fixture
      firstResult <- produceFixture fixture
      secondResult <- produceFixture fixture
      let firstRun = typedCoreProductionStatus firstResult
          secondRun = typedCoreProductionStatus secondResult
      assertEqual (name <> " repeatable rejection") firstRun secondRun
      assertEqual (name <> " scalar rejection inference compatibility") ordinary (typedCoreProductionInferenceResult firstResult)
      assertEqual (name <> " production failure") expectedStatus firstRun

testMissingModuleResultProduction :: IO ()
testMissingModuleResultProduction =
  mapM_ assertMissing ["empty-module", "signed-function-only"]
  where
    expected =
      TypedCoreProductionUnsupported
        [ TypedCoreProductionFailure
            (TypedCoreProductionModulePath ["App", "Main"])
            TypedCoreUnsupportedRootExpression
            TypedCoreUnsupportedRootDetail
        ]
    assertMissing name = do
      let fixture = producerEdgeFixture name
      firstRun <- produceFixture fixture
      secondRun <- produceFixture fixture
      assertEqual (name <> " ordinary diagnostics") [] (filter isErrorDiagnostic (inferredDiagnostics (typedCoreProductionInferenceResult firstRun)))
      assertEqual (name <> " repeatable production") firstRun secondRun
      assertEqual (name <> " exact missing-result failure") expected (typedCoreProductionStatus firstRun)

testMissingResultFailureAccumulation :: IO ()
testMissingResultFailureAccumulation = do
  let fixture = producerEdgeFixture "missing-result-failure-accumulation"
      expected =
        TypedCoreProductionUnsupported
          [ TypedCoreProductionFailure
              (TypedCoreProductionModulePath ["App", "Main"])
              TypedCoreUnsupportedRootExpression
              TypedCoreUnsupportedRootDetail,
            TypedCoreProductionFailure
              (TypedCoreProductionStatementPath ["App", "Main"] 1)
              TypedCoreUnsupportedRootExpression
              TypedCoreUnsupportedRootDetail,
            TypedCoreProductionFailure
              (TypedCoreProductionExpressionPath ["App", "Main"] 3 [0, 0, 1])
              TypedCoreCaptureUnsupported
              (TypedCoreNameDetail "seed")
          ]
  ordinary <- inferFixture fixture
  firstRun <- produceFixture fixture
  secondRun <- produceFixture fixture
  assertEqual
    "missing-result failure inference compatibility"
    ordinary
    (typedCoreProductionInferenceResult firstRun)
  assertEqual "missing-result failure repeatability" firstRun secondRun
  assertEqual "missing-result complete failure accumulation" expected (typedCoreProductionStatus firstRun)

testModuleFailureOrder :: IO ()
testModuleFailureOrder = do
  let fixture = fixtureByName "unit-entry"
      selectors =
        [ ModuleExportSelector (Just ValueNamespace) "zeta",
          ModuleExportSelector (Just ValueNamespace) "alpha"
        ]
      inventory =
        exportInventory
          [ ModuleExport ValueNamespace "zeta",
            ModuleExport ValueNamespace "alpha"
          ]
      expected =
        TypedCoreProductionUnsupported
          [ TypedCoreProductionFailure
              (TypedCoreProductionModulePath ["App", "Main"])
              TypedCoreUnsupportedRootExpression
              TypedCoreUnsupportedRootDetail,
            TypedCoreProductionFailure
              (TypedCoreProductionModulePath ["App", "Main"])
              TypedCoreUnsupportedExport
              (TypedCoreNameDetail "zeta"),
            TypedCoreProductionFailure
              (TypedCoreProductionModulePath ["App", "Main"])
              TypedCoreUnsupportedExport
              (TypedCoreNameDetail "alpha"),
            TypedCoreProductionFailure
              (TypedCoreProductionStatementPath ["App", "Main"] 0)
              TypedCoreUnsupportedRootExpression
              TypedCoreUnsupportedRootDetail
          ]
  resolvedModule <- resolveFixtureModule fixture
  let coreModule = resolvedModuleCore resolvedModule
      mutatedModule =
        resolvedModule
          { resolvedModuleExportInventory = inventory,
            resolvedModuleCore =
              coreModule
                { coreModuleDeclaredExports =
                    Just (DeclaredModuleExports (SourceSpan 1 1) selectors),
                  coreModuleExpr = EBlock [SLet "ignored" (SourceSpan 2 1) (ETuple [])]
                }
          }
  firstRun <- produceResolvedFixture fixture mutatedModule
  secondRun <- produceResolvedFixture fixture mutatedModule
  assertEqual "module failure order repeatability" firstRun secondRun
  assertEqual "module failures precede statements in authored export order" expected (typedCoreProductionStatus firstRun)

testExportedCallableFailureOwnership :: IO ()
testExportedCallableFailureOwnership = do
  let fixture = producerEdgeFixture "default-exported-polymorphic-callable"
      expected =
        TypedCoreProductionUnsupported
          [ TypedCoreProductionFailure
              (TypedCoreProductionModulePath ["App", "Main"])
              TypedCoreUnsupportedExport
              (TypedCoreNameDetail "seed"),
            TypedCoreProductionFailure
              (TypedCoreProductionStatementPath ["App", "Main"] 1)
              TypedCoreUnsupportedRootExpression
              TypedCoreUnsupportedRootDetail,
            TypedCoreProductionFailure
              (TypedCoreProductionExpressionPath ["App", "Main"] 2 [])
              TypedCoreUnresolvedExpressionType
              TypedCoreUnsupportedRootDetail,
            TypedCoreProductionFailure
              (TypedCoreProductionStatementPath ["App", "Main"] 3)
              TypedCoreNonMonomorphicFunctionUnsupported
              (TypedCoreNameDetail "identity"),
            TypedCoreProductionFailure
              (TypedCoreProductionExpressionPath ["App", "Main"] 3 [])
              TypedCoreUnresolvedExpressionType
              TypedCoreUnsupportedRootDetail,
            TypedCoreProductionFailure
              (TypedCoreProductionExpressionPath ["App", "Main"] 3 [0])
              TypedCoreUnresolvedExpressionType
              TypedCoreUnsupportedRootDetail
          ]
  ordinary <- inferFixture fixture
  firstRun <- produceFixture fixture
  secondRun <- produceFixture fixture
  assertEqual
    "default-exported polymorphic callable inference compatibility"
    ordinary
    (typedCoreProductionInferenceResult firstRun)
  assertEqual "default-exported polymorphic callable repeatability" firstRun secondRun
  assertEqual "export callable failure remains statement-owned and unique" expected (typedCoreProductionStatus firstRun)

testCompoundFailureAccumulation :: IO ()
testCompoundFailureAccumulation = do
  let fixture = producerEdgeFixture "nested-unsupported-children"
      expected =
        TypedCoreProductionUnsupported
          [ expressionFailure [] TypedCoreControlFlowUnsupported TypedCoreConditionalDetail,
            expressionFailure [1] TypedCoreStructuredValueUnsupported TypedCoreListValueDetail,
            expressionFailure [2] TypedCoreStructuredValueUnsupported TypedCoreListValueDetail
          ]
  firstRun <- produceFixture fixture
  secondRun <- produceFixture fixture
  assertEqual "compound failure ordinary diagnostics" [] (filter isErrorDiagnostic (inferredDiagnostics (typedCoreProductionInferenceResult firstRun)))
  assertEqual "compound failure repeatable production" firstRun secondRun
  assertEqual "compound failure structural preorder" expected (typedCoreProductionStatus firstRun)
  where
    expressionFailure childPath kind detail =
      TypedCoreProductionFailure
        (TypedCoreProductionExpressionPath ["App", "Main"] 0 childPath)
        kind
        detail

testUnsupportedCompositeFailureAccumulation :: IO ()
testUnsupportedCompositeFailureAccumulation =
  mapM_ assertCompositeFailure expectedResults
  where
    expectedResults =
      [ ( "pattern-case-unsupported-children",
          [ expressionFailure 0 [] TypedCorePatternCaseUnsupported TypedCorePatternCaseDetail,
            expressionFailure 0 [0] TypedCoreStructuredValueUnsupported TypedCoreListValueDetail,
            expressionFailure 0 [1] TypedCoreStructuredValueUnsupported TypedCoreListValueDetail
          ]
        ),
        ( "nested-block-unsupported-child",
          [ expressionFailure 0 [] TypedCoreNestedBlockUnsupported TypedCoreLocalBlockDetail,
            expressionFailure 0 [0] TypedCoreStructuredValueUnsupported TypedCoreListValueDetail,
            expressionFailure 0 [1] TypedCoreStructuredValueUnsupported TypedCoreListValueDetail
          ]
        ),
        ( "guarded-pattern-case-unsupported-children",
          [ expressionFailure 0 [] TypedCorePatternCaseUnsupported TypedCorePatternCaseDetail,
            expressionFailure 0 [0] TypedCoreStructuredValueUnsupported TypedCoreListValueDetail,
            expressionFailure 0 [1] TypedCoreControlFlowUnsupported TypedCoreConditionalDetail,
            expressionFailure 0 [2] TypedCoreStructuredValueUnsupported TypedCoreListValueDetail
          ]
        ),
        ( "unsupported-binary-child",
          [ statementFailure 1 TypedCoreUserDefinedOperatorUnsupported TypedCoreUnsupportedRootDetail,
            expressionFailure 2 [] TypedCoreUserDefinedOperatorUnsupported TypedCoreUnsupportedRootDetail,
            expressionFailure 2 [0] TypedCoreControlFlowUnsupported TypedCoreConditionalDetail,
            expressionFailure 2 [1] TypedCoreControlFlowUnsupported TypedCoreConditionalDetail
          ]
        ),
        ( "left-section-unsupported-child",
          [ expressionFailure 0 [] TypedCoreUserDefinedOperatorUnsupported TypedCoreUnsupportedRootDetail,
            expressionFailure 0 [0] TypedCoreControlFlowUnsupported TypedCoreConditionalDetail
          ]
        ),
        ( "right-section-unsupported-child",
          [ expressionFailure 0 [] TypedCoreUserDefinedOperatorUnsupported TypedCoreUnsupportedRootDetail,
            expressionFailure 0 [0] TypedCoreControlFlowUnsupported TypedCoreConditionalDetail
          ]
        ),
        ( "type-application-composite",
          [ expressionFailure 0 [] TypedCoreUnresolvedExpressionType TypedCoreUnsupportedRootDetail,
            statementFailure 1 TypedCoreNonMonomorphicFunctionUnsupported (TypedCoreNameDetail "identity"),
            expressionFailure 1 [] TypedCoreUnresolvedExpressionType TypedCoreUnsupportedRootDetail,
            expressionFailure 1 [0] TypedCoreUnresolvedExpressionType TypedCoreUnsupportedRootDetail,
            expressionFailure 2 [0] TypedCoreManagedValueUnsupported TypedCoreUnsupportedRootDetail
          ]
        )
      ]

    assertCompositeFailure (name, expectedFailures) = do
      let fixture = producerEdgeFixture name
      ordinary <- inferFixture fixture
      firstRun <- produceFixture fixture
      secondRun <- produceFixture fixture
      assertEqual
        (name <> " ordinary inference compatibility")
        ordinary
        (typedCoreProductionInferenceResult firstRun)
      assertEqual (name <> " repeatable production") firstRun secondRun
      assertEqual
        (name <> " structural preorder")
        (TypedCoreProductionUnsupported expectedFailures)
        (typedCoreProductionStatus firstRun)

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

testProducerIdentityBoundary :: IO ()
testProducerIdentityBoundary =
  mapM_ assertIdentity expectedResults
  where
    expectedResults =
      [ ( "signed-function-rebinding",
          [ TypedCoreProductionFailure
              (TypedCoreProductionStatementPath ["App", "Main"] 3)
              TypedCoreFunctionRebindingUnsupported
              (TypedCoreNameDetail "identity")
          ]
        ),
        ( "duplicate-leading-parameters",
          [ parameterFailure [0, 0] ]
        ),
        ( "curried-shadowed-parameter",
          [ parameterFailure [0, 0] ]
        )
      ]
    parameterFailure childPath =
      TypedCoreProductionFailure
        (TypedCoreProductionExpressionPath ["App", "Main"] 1 childPath)
        TypedCoreDuplicateParameterUnsupported
        (TypedCoreNameDetail "item")
    assertIdentity (name, expectedFailures) = do
      let fixture = producerEdgeFixture name
      firstRun <- produceFixture fixture
      secondRun <- produceFixture fixture
      assertEqual (name <> " ordinary diagnostics") [] (filter isErrorDiagnostic (inferredDiagnostics (typedCoreProductionInferenceResult firstRun)))
      assertEqual (name <> " repeatable production") firstRun secondRun
      assertEqual
        (name <> " exact identity failure")
        (TypedCoreProductionUnsupported expectedFailures)
        (typedCoreProductionStatus firstRun)

testSameStatementFailureKindOrder :: IO ()
testSameStatementFailureKindOrder = do
  let fixture = producerEdgeFixture "self-recursive-function-rebinding"
      expected =
        TypedCoreProductionUnsupported
          [ TypedCoreProductionFailure
              (TypedCoreProductionStatementPath ["App", "Main"] 1)
              TypedCoreRecursiveFunctionUnsupported
              (TypedCoreNameDetail "loop"),
            TypedCoreProductionFailure
              (TypedCoreProductionStatementPath ["App", "Main"] 3)
              TypedCoreRecursiveFunctionUnsupported
              (TypedCoreNameDetail "loop"),
            TypedCoreProductionFailure
              (TypedCoreProductionStatementPath ["App", "Main"] 3)
              TypedCoreFunctionRebindingUnsupported
              (TypedCoreNameDetail "loop"),
            TypedCoreProductionFailure
              (TypedCoreProductionExpressionPath ["App", "Main"] 3 [0, 0, 1])
              TypedCoreControlFlowUnsupported
              TypedCoreConditionalDetail
          ]
  firstRun <- produceFixture fixture
  secondRun <- produceFixture fixture
  assertEqual "self-recursive rebinding ordinary diagnostics" [] (filter isErrorDiagnostic (inferredDiagnostics (typedCoreProductionInferenceResult firstRun)))
  assertEqual "self-recursive rebinding repeatability" firstRun secondRun
  assertEqual "same-statement failures follow declared kind order" expected (typedCoreProductionStatus firstRun)

testRejectedProducerDependencyTransport :: IO ()
testRejectedProducerDependencyTransport =
  mapM_ assertExact
    [ ( "rejected-conditional-self-recursion",
        [ statementFailure 1 "loop",
          expressionFailure 1 [0, 0] TypedCoreControlFlowUnsupported TypedCoreConditionalDetail
        ]
      ),
      ( "rejected-block-conditional-mutual-recursion",
        [ statementFailure 1 "left",
          expressionFailure 1 [0, 0] TypedCoreNestedBlockUnsupported TypedCoreLocalBlockDetail,
          statementFailure 3 "right",
          expressionFailure 3 [0, 0] TypedCoreControlFlowUnsupported TypedCoreConditionalDetail
        ]
      ),
      ( "rejected-block-parameter-shadow-control",
        [expressionFailure 3 [0, 0] TypedCoreNestedBlockUnsupported TypedCoreLocalBlockDetail]
      ),
      ( "rejected-block-later-shadow-control",
        [ statementFailure 1 "loop",
          expressionFailure 1 [0, 0] TypedCoreNestedBlockUnsupported TypedCoreLocalBlockDetail
        ]
      )
    ]
  where
    assertExact (name, expectedFailures) = do
      let fixture = producerEdgeFixture name
      firstRun <- produceFixture fixture
      secondRun <- produceFixture fixture
      assertEqual (name <> " ordinary diagnostics") [] (filter isErrorDiagnostic (inferredDiagnostics (typedCoreProductionInferenceResult firstRun)))
      assertEqual (name <> " repeatable production") firstRun secondRun
      assertEqual
        (name <> " exact rejected-tree recursion result")
        (TypedCoreProductionUnsupported expectedFailures)
        (typedCoreProductionStatus firstRun)
    statementFailure statementIndex name =
      TypedCoreProductionFailure
        (TypedCoreProductionStatementPath ["App", "Main"] statementIndex)
        TypedCoreRecursiveFunctionUnsupported
        (TypedCoreNameDetail name)
    expressionFailure statementIndex childPath kind detail =
      TypedCoreProductionFailure
        (TypedCoreProductionExpressionPath ["App", "Main"] statementIndex childPath)
        kind
        detail

producerEdgeFixture :: Text -> Fixture
producerEdgeFixture name =
  case lookup name producerEdgeFixtures of
    Just fixture -> fixture
    Nothing -> error "producer edge fixture is missing"

testDiagnosticPrecedence :: IO ()
testDiagnosticPrecedence = do
  let sourceDiagnosticFixture = fixtureByName "source-diagnostic"
  ordinary <- inferFixture sourceDiagnosticFixture
  sourceDiagnosticResult <-
    inferResolvedModuleTypedCoreExpressionDirectCall
      (fixtureInputs sourceDiagnosticFixture)
      (fixtureSourcePath sourceDiagnosticFixture)
      =<< resolveFixtureModule sourceDiagnosticFixture
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

testInputModuleFailureOrder :: IO ()
testInputModuleFailureOrder = do
  let fixture = fixtureByName "resolved-import"
      combinedFixture =
        fixture
          { fixtureSourcePath = TypedSourcePath "/private/host/Main.jz",
            fixtureInputs =
              (fixtureInputs fixture)
                { inferenceCurrentModulePath = Just ["Other", "Main"],
                  inferenceImportedTypes = Map.singleton "foreign" (PlainTypeBinding TBoolType),
                  inferenceImportedClassNames = Set.singleton "PreludeClass"
                }
          }
      expected =
        TypedCoreProductionUnsupported
          [ TypedCoreProductionFailure TypedCoreProductionInputPath TypedCoreModulePathMismatch TypedCoreNoFailureDetail,
            TypedCoreProductionFailure TypedCoreProductionInputPath TypedCoreInvalidPortableSourcePath TypedCoreNoFailureDetail,
            TypedCoreProductionFailure TypedCoreProductionInputPath TypedCoreImportedInputsUnsupported TypedCoreNoFailureDetail,
            TypedCoreProductionFailure TypedCoreProductionInputPath TypedCoreAmbientPreludeInputUnsupported TypedCoreNoFailureDetail,
            TypedCoreProductionFailure
              (TypedCoreProductionModulePath ["App", "Main"])
              TypedCoreResolvedImportsUnsupported
              TypedCoreNoFailureDetail
          ]
  firstRun <- produceFixture combinedFixture
  secondRun <- produceFixture combinedFixture
  assertEqual "input/module failure order repeatability" firstRun secondRun
  assertEqual "all input failures precede module failures" expected (typedCoreProductionStatus firstRun)

assertUnsupported :: Fixture -> [TypedCoreProductionFailure] -> IO ()
assertUnsupported fixture expectedFailures = do
  result <- produceFixture fixture
  assertEqual
    (fixtureName fixture <> " production status")
    (TypedCoreProductionUnsupported expectedFailures)
    (typedCoreProductionStatus result)

testAdditionalProfileFailures :: IO ()
testAdditionalProfileFailures =
  case fixtures of
    unitFixture : _ -> do
      resolvedUnitModule <- resolveFixtureModule unitFixture
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
          unsupportedRoot = withExpression (ELit (LBool True)) resolvedUnitModule
          nonLocalCall =
            withExpression
              ( EBlock
                  [ SExpr
                      (SourceSpan 1 1)
                      (EApply (EVar "__kernel_toInt8") (ELit (LInt 1)))
                  ]
              )
              resolvedUnitModule
          unsupportedExport =
            resolvedUnitModule
              { resolvedModuleExportInventory =
                  exportInventory [ModuleExport ValueNamespace "missing"],
                resolvedModuleCore =
                  (resolvedModuleCore resolvedUnitModule)
                    { coreModuleDeclaredExports =
                        Just
                          ( DeclaredModuleExports
                              (SourceSpan 1 1)
                              [ModuleExportSelector (Just ValueNamespace) "missing"]
                          )
                    }
              }
          leadingStatement =
            withExpression
              (EBlock [SLet "ignored" (SourceSpan 1 1) (ETuple []), SExpr (SourceSpan 2 1) (ETuple [])])
              resolvedUnitModule
          inputFailure = [TypedCoreProductionFailure TypedCoreProductionInputPath TypedCoreImportedInputsUnsupported TypedCoreNoFailureDetail]
          rootExpressionFailure = [TypedCoreProductionFailure (TypedCoreProductionExpressionPath ["App", "Main"] 0 []) TypedCoreUnsupportedRootExpression TypedCoreUnsupportedRootDetail]
          statementFailure = [TypedCoreProductionFailure (TypedCoreProductionStatementPath ["App", "Main"] 0) TypedCoreUnsupportedRootExpression TypedCoreUnsupportedRootDetail]
      assertUnsupported pathMismatch [TypedCoreProductionFailure TypedCoreProductionInputPath TypedCoreModulePathMismatch TypedCoreNoFailureDetail]
      assertUnsupported importedValue inputFailure
      assertUnsupported importedData inputFailure
      assertUnsupported importedCapabilities inputFailure
      assertUnsupportedResolved unitFixture unsupportedRoot rootExpressionFailure
      assertUnsupportedResolved
        unitFixture
        nonLocalCall
        [TypedCoreProductionFailure (TypedCoreProductionExpressionPath ["App", "Main"] 0 []) TypedCoreNonLocalCallUnsupported (TypedCoreNameDetail "__kernel_toInt8")]
      assertUnsupportedResolved
        unitFixture
        unsupportedExport
        [TypedCoreProductionFailure (TypedCoreProductionModulePath ["App", "Main"]) TypedCoreUnsupportedExport (TypedCoreNameDetail "missing")]
      assertUnsupportedResolved unitFixture leadingStatement statementFailure
    [] -> failTest "unit fixture is missing"

assertUnsupportedResolved :: Fixture -> ResolvedModule -> [TypedCoreProductionFailure] -> IO ()
assertUnsupportedResolved fixture resolvedModule expectedFailures = do
  result <- produceResolvedFixture fixture resolvedModule
  assertEqual
    (fixtureName fixture <> " production status")
    (TypedCoreProductionUnsupported expectedFailures)
    (typedCoreProductionStatus result)

withExpression :: Expr -> ResolvedModule -> ResolvedModule
withExpression expression moduleValue =
  moduleValue
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
