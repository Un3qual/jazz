{-# LANGUAGE OverloadedStrings #-}

module Jazz.Compiler.Bootstrap.TypedCoreExpressionDirectCallSpec.ManifestTests where

import qualified Data.Set as Set
import Jazz.Compiler.Bootstrap.TypedCoreExpressionDirectCallFixtures
import Jazz.Compiler.Bootstrap.TypedCoreExpressionDirectCallSpec.BoundaryTests
import Jazz.Compiler.Diagnostics (isErrorDiagnostic)
import Jazz.Compiler.LoweredIR.Lower
import Jazz.Compiler.LoweredIR.Validate (validateLoweredProgram)
import Jazz.Compiler.TypeInference hiding (InferenceResult (..))
import Jazz.Compiler.TypeInference.Result (InferenceResult (..))
import Jazz.Compiler.TypedCore.Validate (validateTypedProgram)
import Jazz.TestHarness (assertEqual, failTest)

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
          "conditional",
          "pattern-case",
          "scalar-parameter-return",
          "single-argument-direct-call",
          "curried-multi-argument-direct-call",
          "three-argument-direct-call",
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
          "callable-parameter-value-shadows-enclosing-function",
          "capturing-function",
          "partial-direct-call",
          "self-recursive-function",
          "mutually-recursive-functions",
          "closure-value-mutual-recursion",
          "closure-value-self-recursion",
          "capturing-self-recursion",
          "capturing-mutual-recursion"
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
          "local-block-binding",
          "oversaturated-direct-call",
          "later-capture-mutual-recursion",
          "transitive-later-capture-mutual-recursion",
          "interleaved-rebound-capture-mutual-recursion",
          "polymorphic-or-evidence-function",
          "imported-direct-call",
          "user-defined-operator-call"
        ]
  assertEqual "accepted source fixture names" expectedAcceptedNames acceptedFixtureNames
  assertEqual "rejected source fixture names" expectedRejectedNames rejectedFixtureNames
  assertEqual "fixture order" (acceptedFixtureNames <> rejectedFixtureNames) fixtureNames
    >> assertEqual "accepted fixture count" 34 (length acceptedFixtureNames)
    >> assertEqual "rejected fixture count" 16 (length rejectedFixtureNames)
    >> assertEqual "unique fixture count" 50 (Set.size (Set.fromList fixtureNames))
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
        [ "scalar-binding-literal",
          "scalar-binding-ordered-reuse",
          "scalar-binding-direct-call-result",
          "self-recursive-function",
          "mutually-recursive-functions",
          "closure-value-mutual-recursion",
          "closure-value-self-recursion",
          "capturing-self-recursion",
          "capturing-mutual-recursion",
          "scalar-binding-unsupported-rhs",
          "combined-statement-failure-order",
          "recursion-descendant-failure-order",
          "interleaved-capture-mutual-recursion",
          "closure-valued-parameter",
          "closure-valued-result",
          "closure-shaped-named-function",
          "closure-shaped-named-application",
          "callable-parameter-shadows-top-level-lowerer",
          "callable-parameter-value-shadows-enclosing-function-lowerer",
          "non-concrete-closure-representation",
          "duplicate-parameter-function",
          "self-recursive-duplicate-parameter-function",
          "duplicate-function-identity",
          "capturing-function",
          "closure-shaped-self-recursive-function",
          "nested-lambda-closure-value-self-recursion",
          "imported-direct-call",
          "managed-scalar-entry",
          "conditional-entry",
          "managed-pattern-scrutinee",
          "pattern-case-constructor-lowerer",
          "pattern-case-list-lowerer",
          "pattern-case-tuple-lowerer",
          "pattern-case-as-lowerer",
          "pattern-case-or-lowerer",
          "pattern-case-final-literal-lowerer",
          "pattern-case-final-guarded-catch-all-lowerer",
          "pattern-case-unguarded-non-final-wildcard-lowerer",
          "pattern-case-unguarded-non-final-variable-lowerer"
        ]
      expectedInvalidNames =
        [ "closure-shape-flattened-recipe",
          "direct-shape-staged-recipe",
          "callable-shape-body-disagreement",
          "variable-binder-reference-mismatch",
          "direct-flattened-representation",
          "direct-shaped-closure-value-self-recursion",
          "shape-rejected-self-recursion",
          "shape-rejected-mutual-recursion",
          "shape-rejected-binder-shadow-control",
          "bare-function-value",
          "partial-direct-call"
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
        <> scalarPatternCaseExpectedPrograms
        <> directCallExpectedPrograms
        <> directRecursionExpectedPrograms
        <> closureRecursionExpectedPrograms
        <> closedCallableExpectedPrograms
        <> lexicalCaptureExpectedPrograms
        <> curriedApplicationExpectedPrograms
    expectedLoweredPrograms =
      scalarExpectedLoweredPrograms
        <> [(name, lowered) | (name, _, lowered) <- scalarPatternCaseExpectedLoweredPrograms]
        <> directCallExpectedLoweredPrograms
        <> [(name, lowered) | (name, _, lowered) <- directRecursionExpectedLoweredPrograms]
        <> [(name, lowered) | (name, _, lowered) <- closureRecursionExpectedLoweredPrograms]
        <> closedCallableExpectedLoweredPrograms
        <> [(name, lowered) | (name, _, lowered) <- lexicalCaptureExpectedLoweredPrograms]
        <> [(name, lowered) | (name, _, lowered) <- curriedApplicationExpectedLoweredPrograms]
    expectedLoweringFailures = []

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
              case (typedCoreProductionValidatedProgram firstProduction, lookup name expectedLoweredPrograms, lookup name expectedLoweringFailures) of
                (Just validatedProgram, Just expectedLoweredProgram, Nothing) -> do
                  let lowering = lowerTypedCoreExpressionDirectCall typedProgram
                      trustedLowering = lowerValidatedTypedCoreExpressionDirectCall validatedProgram
                  assertEqual (name <> " trusted lowering matches checked lowering") lowering trustedLowering
                  assertEqual
                    (name <> " complete lowered production")
                    (LoweredIRSucceeded expectedLoweredProgram)
                    lowering
                  case lowering of
                    LoweredIRSucceeded loweredProgram ->
                      assertEqual (name <> " lowered validation") [] (validateLoweredProgram loweredProgram)
                    _ -> failTest (name <> " did not produce lowered IR")
                (Just validatedProgram, Nothing, Just expectedLoweringFailure) -> do
                  let lowering = lowerTypedCoreExpressionDirectCall typedProgram
                      trustedLowering = lowerValidatedTypedCoreExpressionDirectCall validatedProgram
                  assertEqual (name <> " trusted lowering matches checked lowering") lowering trustedLowering
                  assertEqual (name <> " retained lowering boundary") expectedLoweringFailure lowering
                (Nothing, _, _) -> failTest (name <> " did not retain its validation proof")
                (_, _, _) -> failTest (name <> " has ambiguous or missing lowering expectations")
            _ -> failTest (name <> " did not produce typed core")
        Nothing -> failTest (name <> " is missing a typed-program expectation")
