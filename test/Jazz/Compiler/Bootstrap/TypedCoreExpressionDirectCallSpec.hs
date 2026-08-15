{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.IORef (modifyIORef', newIORef, readIORef)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Jazz.Compiler.AST (CaseArm (..), DataConstructor (..), Expr (..), Literal (..), NumericType (NumericUInt8), Pattern (..), Statement (..))
import Jazz.Compiler.Bootstrap.TypedCoreExpressionDirectCallFixtures
import Jazz.Compiler.BuiltinCatalog (BuiltinResolutionMode (ResolveKernelOnly))
import Jazz.Compiler.DiagnosticCatalog (diagnosticCodeText)
import Jazz.Compiler.Diagnostics
  ( SourceSpan (..),
    diagnosticCode,
    diagnosticSubject,
    isErrorDiagnostic,
  )
import Jazz.Compiler.LoweredIR
import Jazz.Compiler.LoweredIR.Lower
import Jazz.Compiler.LoweredIR.Validate (validateLoweredProgram)
import Jazz.Compiler.ModuleExports (ModuleExport (..), ModuleExportSelector (..), exportInventory)
import Jazz.Compiler.ModuleGraph (CoreModule (..), DeclaredModuleExports (..), ResolvedModule (..))
import Jazz.Compiler.Name (NameNamespace (ValueNamespace), operatorBindingName)
import Jazz.Compiler.TypeInference
import Jazz.Compiler.TypeInference.Elaboration
  ( InferredExpr (..),
    ProvisionalCallableDeclaration (..),
    ProvisionalTypedExpr (..),
    ProvisionalTypedStatement (..),
    expressionDependencyNames,
    finalizeValidatedTypedCoreExpressionDirectCall,
    typedCoreProductionOutcomeStatus,
  )
import Jazz.Compiler.TypeInference.Pattern (InferredPatternCaseArm (..), inferPatternCaseTypeWithResults)
import Jazz.Compiler.TypeInference.State (initialInferState)
import Jazz.Compiler.TypeInference.Types
  ( DataTypeBinding (..),
    ExpressionType (TBoolType, TFunctionType, TIntegerLiteralType, TNumericType),
    IntegerLiteralRange (..),
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
    ("keeps function-body consumed calls as ordinary operations", testFunctionResultNegativeTerminators),
    ("preserves function-body partial-application returns", testFunctionBodyPartialApplicationResult),
    ("lowers nested function-result control flow exactly", testNestedTailControlFlow),
    ("produces exact scalar pattern cases before lowering", testScalarPatternCaseProduction),
    ("rechecks the scalar pattern-case lowerer profile", testScalarPatternCaseLowererBoundary),
    ("rejects scalar pattern cases outside the bounded producer profile", testScalarPatternCaseProducerBoundaries),
    ("retains scalar pattern-case arm positions after pattern typing failures", testScalarPatternCaseArmResultPositions),
    ("preserves pattern-case captures and closure-valued arm profiles", testScalarPatternCaseAnalysisProduction),
    ("transports nested and in-flight scalar pattern-case values", testScalarPatternCaseTransportLowering),
    ("produces and lowers conditional profile combinations", testConditionalProfileCoverage),
    ("produces concrete scalar bindings in source order", testScalarBindingProduction),
    ("produces managed Text literals and bindings exactly", testManagedTextProduction),
    ("produces exact managed Text operations", testManagedTextOperationProduction),
    ("lowers exact managed Text runtime services", testManagedTextOperationLowering),
    ("keeps managed Text kernel values and arities bounded", testManagedTextKernelBoundaries),
    ("lowers managed Text literals with one layout and no services", testManagedTextLowering),
    ("produces binder-resolved lexical closures", testLexicalCaptureProduction),
    ("produces staged curried partial applications", testCurriedApplicationProduction),
    ("lowers staged curried applications", testCurriedApplicationLowering),
    ("keeps non-callable oversaturation at the source diagnostic boundary", testNonCallableOversaturationDiagnostic),
    ("lowers lexical closures with exact environments", testLexicalCaptureLowering),
    ("supports the complete lexical capture fixture matrix", testLexicalCaptureFixtureMatrix),
    ("dispatches captured closure callees from nested named closures", testClosureCaptureReviewRegression "named-nested-captured-closure-call"),
    ("propagates transitive named closure environments", testClosureCaptureReviewRegression "transitive-named-closure-capture"),
    ("preserves lifted lambda failure preorder", testLiftedLambdaFailurePreorder),
    ("rejects lifted lambda aliases for module metadata", testLiftedLambdaMetadataAlias),
    ("lowers scalar bindings once for ordered entry reuse", testScalarBindingLowering),
    ("preserves flattened recipes through a named direct leading-lambda chain", testThreeArgumentDirectLeadingLambdaRecipe),
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
    ("admits captured arguments inside direct-call bodies", testCurriedArgumentCapture),
    ("retains supplied arguments inside partial applications", testPartialApplicationArgumentCapture),
    ("retains managed arguments inside valid partial applications", testPartialApplicationManagedArgumentProduction),
    ("retains supplied argument failures when non-local calls are rejected", testNonLocalCallArgumentFailureAccumulation),
    ("retains later sibling failures after accepting a captured closure call", testClosureUseArgumentFailureOrder),
    ("collapses mixed callable-use reasons to one closure classification", testClosureShapeClassificationCollapse),
    ("specializes integer literals to direct-call parameter types", testNarrowLiteralDirectCall),
    ("specializes computed integer results to declared return types", testNarrowCompositeFunctionResult),
    ("specializes comparison operands to their unified numeric type", testNarrowComparisonOperand),
    ("specializes terminal binary operands to their unified numeric type", testNarrowRootBinaryDirectCall),
    ("normalizes equivalent scalar aliases to the expected type", testEquivalentScalarAliasSpecialization),
    ("rejects inherited recursive captures unavailable at an earlier caller", testEarlierCallerTransitiveCaptureAvailability),
    ("specializes every use of a captured numeric scalar", testCapturedNumericScalarReferenceSpecialization),
    ("specializes enclosing expressions that reuse captured numeric scalars", testCapturedCompositeScalarSpecialization),
    ("specializes independent scalar binders recolored with recursive captures", testCapturedCompositeScalarBinderSpecialization),
    ("preserves capture expectations for non-integral comparison operands", testCapturedComparisonResultSpecialization),
    ("specializes enclosing function expressions that reuse captured numeric scalars", testCapturedFunctionBodySpecialization),
    ("specializes callable parameters recolored with captured numeric scalars", testCapturedFunctionParameterSpecialization),
    ("specializes callable parameters invoked with captured numeric scalars", testCapturedCallableParameterApplicationSpecialization),
    ("specializes scalar binders recolored inside callable bodies", testCapturedFunctionScalarBinderSpecialization),
    ("specializes scalar binders recolored as callable arguments", testCapturedFunctionArgumentScalarBinderSpecialization),
    ("specializes scalar binders initialized from callable results", testCapturedFunctionResultScalarBinderSpecialization),
    ("specializes higher-order profiles from callable arguments", testCapturedHigherOrderCallableArgumentSpecialization),
    ("specializes forwarded higher-order profiles from callable arguments", testCapturedForwardedHigherOrderCallableArgumentSpecialization),
    ("specializes terminal anonymous callable bodies", testCapturedTerminalAnonymousCallableSpecialization),
    ("respecializes callers of specialized named functions", testCapturedNamedCallerSpecialization),
    ("specializes scalar alias sources captured by recursive closures", testCapturedScalarAliasSourceSpecialization),
    ("specializes recursive scalar alias captures across omitted source statements", testRecordedScalarStatementIndices),
    ("rejects eager recursive closure calls before their captures exist", testEagerRecursiveClosureCaptureAvailability),
    ("rejects eager nested closure construction before transitive captures exist", testEagerNestedClosureCaptureAvailability),
    ("rejects unused user-defined operator bindings", testUnusedUserDefinedOperatorBinding),
    ("retains root data failures with their real statement paths", testRootDataFailureAccumulation),
    ("retains nested data-block child failures in structural order", testNestedDataFailureAccumulation),
    ("accepts anonymous lambdas as module results", testAnonymousLambdaResultAcceptance),
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
    ("rejects incomplete transported recursive group ownership", testIncompleteRecursiveGroupOwnership),
    ("orders same-statement recursion before rebinding and descendants", testSameStatementFailureKindOrder),
    ("binds a later callable rebinding to the nearest prior declaration", testCanonicalCallableRebindingDependencies "later-callable-rebinding-calls-nearest-prior"),
    ("trusts canonical ownership across an intervening scalar declaration", testInterveningScalarCanonicalOwnership "intervening-scalar-canonical-ownership"),
    ("trusts canonical ownership across multiple scalar shadows", testInterveningScalarCanonicalOwnership "multiple-intervening-scalars-canonical-ownership"),
    ("trusts canonical ownership across callable and scalar shadows", testInterveningScalarCanonicalOwnership "interleaved-callable-scalar-canonical-ownership"),
    ("retains every established canonical recursion owner exactly once", testCanonicalRecursionTransportControls),
    ("resolves a nested alias to its nearest prior outer declaration", testNestedPriorOuterAliasOwnership "nested-prior-outer-alias-mutual-recursion"),
    ("resolves a nested conditional alias to its nearest prior outer declaration", testNestedPriorOuterAliasOwnership "nested-prior-outer-conditional-alias-mutual-recursion"),
    ("keeps a nested self-recursive lambda local to its block", testNestedSelfRecursiveLambdaOwnership),
    ("rejects direct recursion that escapes through a nested lambda", testNestedLambdaRecursiveAdmission),
    ("classifies an accepted then rejected callable rebinding", testRejectedCallableRebinding "accepted-then-rejected-callable-rebinding"),
    ("orders rejected callable recursion before rebinding and descendants", testRejectedCallableRebinding "rejected-recursive-callable-rebinding-order"),
    ("classifies a rejected then accepted callable rebinding", testRejectedCallableRebinding "rejected-then-accepted-callable-rebinding"),
    ("classifies repeated rejected callable rebindings", testRejectedCallableRebinding "repeated-rejected-callable-rebinding"),
    ("keeps a prior scalar out of rejected callable rebinding", testRejectedCallableRebinding "scalar-then-rejected-callable-control"),
    ("retains accepted callable ownership across a scalar before rejection", testRejectedCallableRebinding "accepted-scalar-rejected-callable-rebinding"),
    ("retains rejected callable ownership across a scalar before acceptance", testRejectedCallableRebinding "rejected-scalar-accepted-callable-rebinding"),
    ("selects the nearest of three same-name declarations", testCanonicalCallableRebindingDependencies "three-same-name-nearest-prior-mutual-recursion"),
    ("preserves canonical self recursion when no prior binding exists", testCanonicalCallableRebindingDependencies "canonical-self-recursion-no-prior"),
    ("preserves canonical mutual recursion between peers", testCanonicalCallableRebindingDependencies "canonical-mutual-recursion-peers"),
    ("keeps nearest-rebinding mutual references acyclic", testCanonicalCallableRebindingDependencies "nearest-rebinding-mutual-control"),
    ("keeps callable rebinding parameter shadows out of recursion", testCanonicalCallableRebindingDependencies "rebinding-parameter-shadow-control"),
    ("keeps callable rebinding local shadows out of recursion", testCanonicalCallableRebindingDependencies "rebinding-local-shadow-control"),
    ("preserves rejected self-alias declarations before profile acceptance", testRejectedCallableDeclarationTransport "rejected-self-alias-recursion"),
    ("preserves rejected mutual-alias declarations before profile acceptance", testRejectedCallableDeclarationTransport "rejected-mutual-alias-recursion"),
    ("preserves rejected conditional-root declarations before profile acceptance", testRejectedCallableDeclarationTransport "rejected-alias-conditional-mutual-recursion"),
    ("preserves rejected operator-alias declarations before profile acceptance", testRejectedCallableDeclarationTransport "rejected-operator-alias-self-recursion"),
    ("keeps eager operator conditions out of alias-only recursion", testRejectedCallableDeclarationTransport "rejected-eager-operator-conditional-control"),
    ("keeps rejected callable parameter shadows out of declaration cycles", testRejectedCallableDeclarationTransport "rejected-alias-parameter-shadow-control"),
    ("keeps rejected callable local shadows out of declaration cycles", testRejectedCallableDeclarationTransport "rejected-alias-local-shadow-control"),
    ("keeps eager self use outside an unrelated callable result", testRejectedCallableDeclarationTransport "rejected-eager-self-before-callable-result-control"),
    ("preserves nearest-prior callable ownership through a block alias rebinding", testRejectedCallableDeclarationTransport "rejected-block-nearest-prior-callable-rebinding-recursion"),
    ("preserves recursion dependencies through rejected producer trees", testRejectedProducerDependencyTransport),
    ("maps non-builtin operator forms and excludes builtins", testOperatorDependencyNames),
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

testFunctionResultNegativeTerminators :: IO ()
testFunctionResultNegativeTerminators = do
  let (typedProgram, expectedProgram) = functionBodyConsumedCallExpectedProgram
  assertEqual
    "function-body consumed calls use valid typed core"
    []
    (validateTypedProgram typedProgram)
  assertEqual
    "function-body consumed calls use valid Lowered IR"
    []
    (validateLoweredProgram expectedProgram)
  assertEqual
    "function-body consumed direct and closure calls lower exactly"
    (LoweredIRSucceeded expectedProgram)
    (lowerTypedCoreExpressionDirectCall typedProgram)

testFunctionBodyPartialApplicationResult :: IO ()
testFunctionBodyPartialApplicationResult = do
  let (typedProgram, expectedProgram) = functionBodyPartialApplicationExpectedProgram
  assertEqual
    "function-body partial application uses valid typed core"
    []
    (validateTypedProgram typedProgram)
  assertEqual
    "function-body partial application uses valid Lowered IR"
    []
    (validateLoweredProgram expectedProgram)
  assertEqual
    "function-body partial application lowers exactly"
    (LoweredIRSucceeded expectedProgram)
    (lowerTypedCoreExpressionDirectCall typedProgram)

testNestedTailControlFlow :: IO ()
testNestedTailControlFlow =
  mapM_ assertExact nestedTailControlFlowExpectedLoweredPrograms
  where
    assertExact (name, expectedProgram) = do
      firstProduction <- produceFixture (producerEdgeFixture name)
      secondProduction <- produceFixture (producerEdgeFixture name)
      assertEqual (name <> " repeatable production") firstProduction secondProduction
      assertEqual (name <> " expected lowered validation") [] (validateLoweredProgram expectedProgram)
      case typedCoreProductionStatus firstProduction of
        TypedCoreProductionSucceeded typedProgram -> do
          assertEqual (name <> " typed validation") [] (validateTypedProgram typedProgram)
          assertEqual
            (name <> " exact nested tail lowering")
            (LoweredIRSucceeded expectedProgram)
            (lowerTypedCoreExpressionDirectCall typedProgram)
        other -> failTest (name <> " did not produce typed core: " <> Text.pack (show other))

testScalarPatternCaseProduction :: IO ()
testScalarPatternCaseProduction =
  mapM_ assertProduced scalarPatternCaseExpectedPrograms
  where
    assertProduced (name, expectedProgram) = do
      let fixture =
            if name == "pattern-case"
              then fixtureByName name
              else producerEdgeFixture name
      firstProduction <- produceFixture fixture
      secondProduction <- produceFixture fixture
      assertEqual (name <> " repeatable production") firstProduction secondProduction
      assertEqual
        (name <> " exact typed production")
        (TypedCoreProductionSucceeded expectedProgram)
        (typedCoreProductionStatus firstProduction)
      assertEqual (name <> " typed validation") [] (validateTypedProgram expectedProgram)
      case lookup name expectedLowerings of
        Just expectedLowering ->
          assertEqual
            (name <> " exact scalar pattern-case lowering")
            (LoweredIRSucceeded expectedLowering)
            (lowerTypedCoreExpressionDirectCall expectedProgram)
        Nothing ->
          case lowerTypedCoreExpressionDirectCall expectedProgram of
            LoweredIRSucceeded loweredProgram ->
              assertEqual (name <> " lowered validation") [] (validateLoweredProgram loweredProgram)
            lowering ->
              failTest (name <> " did not lower: " <> Text.pack (show lowering))
    expectedLowerings =
      [(name, lowered) | (name, _, lowered) <- scalarPatternCaseExpectedLoweredPrograms]

testScalarPatternCaseLowererBoundary :: IO ()
testScalarPatternCaseLowererBoundary =
  mapM_ assertBoundary expectedResults
  where
    assertBoundary (name, expectedFailures) =
      case lookup name scalarPatternCaseLowererBoundaryPrograms of
        Nothing -> failTest (name <> " pattern-case lowerer boundary program is missing")
        Just programValue -> do
          let firstLowering = lowerTypedCoreExpressionDirectCall programValue
              secondLowering = lowerTypedCoreExpressionDirectCall programValue
          assertEqual (name <> " valid typed core") [] (validateTypedProgram programValue)
          assertEqual (name <> " repeatable lowerer rejection") firstLowering secondLowering
          assertEqual
            (name <> " exact lowerer rejection")
            (LoweredIRUnsupported expectedFailures)
            firstLowering

    expectedResults =
      [ ( "pattern-case-constructor-lowerer",
          [ LoweredIRLoweringFailure
              (TypedStatementPath ["App", "Main"] [0])
              LoweredIRUnsupportedStatement
              LoweredIRNoFailureDetail,
            patternFailure [1] [0, 0]
          ]
        ),
        unsupportedPattern "pattern-case-list-lowerer" [0] [0, 0],
        unsupportedPattern "pattern-case-tuple-lowerer" [0] [0, 0],
        unsupportedPattern "pattern-case-as-lowerer" [0] [0, 0],
        unsupportedPattern "pattern-case-or-lowerer" [0] [0, 0],
        incompleteCase "pattern-case-final-literal-lowerer",
        incompleteCase "pattern-case-final-guarded-catch-all-lowerer",
        incompleteCase "pattern-case-unguarded-non-final-wildcard-lowerer",
        incompleteCase "pattern-case-unguarded-non-final-variable-lowerer"
      ]
    unsupportedPattern name statementPath patternPath =
      (name, [patternFailure statementPath patternPath])
    patternFailure statementPath patternPath =
      LoweredIRLoweringFailure
        (TypedPatternPath ["App", "Main"] statementPath patternPath)
        LoweredIRUnsupportedPattern
        LoweredIRNoFailureDetail
    incompleteCase name =
      ( name,
        [ LoweredIRLoweringFailure
            (TypedExpressionPath ["App", "Main"] [0] [0])
            LoweredIRIncompletePatternCase
            LoweredIRNoFailureDetail
        ]
      )

testScalarPatternCaseProducerBoundaries :: IO ()
testScalarPatternCaseProducerBoundaries = do
  mapM_ assertSourceBoundary expectedSourceFailures
  mapM_ assertDiagnosticBoundary expectedDiagnosticFailures
  assertEmptyArmBoundary
  where
    expectedSourceFailures =
      [ ("pattern-case-managed-scrutinee", [profileFailure 0]),
        ( "pattern-case-constructor-pattern",
          [ statementFailure 0 TypedCoreStructuredValueUnsupported TypedCoreDataValueDetail,
            profileFailure 1
          ]
        ),
        ( "pattern-case-list-pattern",
          [ expressionFailure 0 [0] TypedCoreStructuredValueUnsupported TypedCoreListValueDetail,
            profileFailure 0
          ]
        ),
        ( "pattern-case-tuple-pattern",
          [ expressionFailure 0 [0] TypedCoreStructuredValueUnsupported TypedCoreTupleValueDetail,
            profileFailure 0
          ]
        ),
        ("pattern-case-as-pattern", [profileFailure 0]),
        ("pattern-case-or-pattern", [profileFailure 0])
      ]
    expectedDiagnosticFailures =
      [ ("pattern-case-final-guarded-catch-all", "E2018"),
        ("pattern-case-missing-final-catch-all", "E2018"),
        ("pattern-case-unguarded-non-final-wildcard", "E2019"),
        ("pattern-case-unguarded-non-final-variable", "E2019"),
        ("pattern-case-non-bool-guard", "E2001"),
        ("pattern-case-incompatible-arm-results", "E2012")
      ]

    assertSourceBoundary (name, expectedFailures) = do
      let fixture = producerEdgeFixture name
      firstProduction <- produceFixture fixture
      secondProduction <- produceFixture fixture
      assertEqual (name <> " repeatable rejection") firstProduction secondProduction
      assertEqual
        (name <> " exact producer-profile rejection")
        (TypedCoreProductionUnsupported expectedFailures)
        (typedCoreProductionStatus firstProduction)

    assertDiagnosticBoundary (name, expectedDiagnosticCode) = do
      let fixture = producerEdgeFixture name
      ordinary <- inferFixture fixture
      firstProduction <- produceFixture fixture
      secondProduction <- produceFixture fixture
      assertEqual (name <> " inference compatibility") ordinary (typedCoreProductionInferenceResult firstProduction)
      assertEqual (name <> " repeatable diagnostic block") firstProduction secondProduction
      assertEqual
        (name <> " remains owned by inference diagnostics")
        TypedCoreProductionBlockedByDiagnostics
        (typedCoreProductionStatus firstProduction)
      assertEqual
        (name <> " exact inference diagnostic")
        [expectedDiagnosticCode]
        [ diagnosticCodeText (diagnosticCode diagnostic)
        | diagnostic <- inferredDiagnostics ordinary,
          isErrorDiagnostic diagnostic
        ]

    assertEmptyArmBoundary = do
      let fixture = fixtureByName "unit-entry"
      resolvedModule <- resolveFixtureModule fixture
      let emptyCaseModule =
            resolvedModule
              { resolvedModuleCore =
                  replaceTerminalExpression
                    (EPatternCase (ELit (LBool True)) [])
                    (resolvedModuleCore resolvedModule)
              }
      firstProduction <- produceResolvedFixture fixture emptyCaseModule
      secondProduction <- produceResolvedFixture fixture emptyCaseModule
      assertEqual "empty pattern-case repeatable rejection" firstProduction secondProduction
      assertEqual
        "empty pattern-case exact diagnostic rejection"
        TypedCoreProductionBlockedByDiagnostics
        (typedCoreProductionStatus firstProduction)
      assertEqual
        "empty pattern-case exact diagnostic"
        ["E2018"]
        [ diagnosticCodeText (diagnosticCode diagnostic)
        | diagnostic <- inferredDiagnostics (typedCoreProductionInferenceResult firstProduction),
          isErrorDiagnostic diagnostic
        ]

    replaceTerminalExpression replacement coreModule =
      case coreModuleExpr coreModule of
        EBlock [SExpr spanValue _] ->
          coreModule {coreModuleExpr = EBlock [SExpr spanValue replacement]}
        other ->
          error ("unexpected unit fixture core shape: " <> show other)

    profileFailure statementIndex =
      expressionFailure
        statementIndex
        []
        TypedCorePatternCaseUnsupported
        TypedCorePatternCaseDetail

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

testScalarPatternCaseArmResultPositions :: IO ()
testScalarPatternCaseArmResultPositions =
  assertEqual
    "pattern inference retains one result slot per authored arm"
    [PLiteral (LInt 1), PWildcard]
    (map inferredArmPattern armResults)
  where
    (_, _, armResults) =
      inferPatternCaseTypeWithResults
        inferChild
        ResolveKernelOnly
        Map.empty
        TBoolType
        initialInferState
        [ CaseArm (PLiteral (LInt 1)) Nothing (ELit (LBool False)),
          CaseArm PWildcard Nothing (ELit (LBool True))
        ]
    inferChild _ _ state _ =
      (InferredExpr (Just TBoolType) Nothing [], state)
    inferredArmPattern (InferredPatternCaseArm pattern _ _) = pattern

testScalarPatternCaseAnalysisProduction :: IO ()
testScalarPatternCaseAnalysisProduction =
  mapM_ assertProduced scalarPatternCaseAnalysisExpectedPrograms
  where
    assertProduced (name, expectedProgram) = do
      let fixture = producerEdgeFixture name
      firstProduction <- produceFixture fixture
      secondProduction <- produceFixture fixture
      assertEqual (name <> " repeatable production") firstProduction secondProduction
      assertEqual
        (name <> " exact analysis-preserving production")
        (TypedCoreProductionSucceeded expectedProgram)
        (typedCoreProductionStatus firstProduction)
      assertEqual (name <> " typed validation") [] (validateTypedProgram expectedProgram)

testScalarPatternCaseTransportLowering :: IO ()
testScalarPatternCaseTransportLowering = do
  mapM_
    assertExpectationKeys
    [ ("control flow", map fst expectedPatternCaseControlFlows),
      ("transport shape", map fst expectedPatternCaseTransportShapes),
      ("join operations", map fst expectedPatternCaseJoinOperations),
      ("closure call count", map fst expectedClosureCallCounts)
    ]
  mapM_ assertTransported names
  where
    assertExpectationKeys (label, keys) =
      assertEqual
        (label <> " expectation keys are exercised")
        []
        [key | key <- keys, key `notElem` names]
    names =
      [ "pattern-case-in-conditional-branch",
        "conditional-in-pattern-case-guard",
        "pattern-case-in-pattern-case-body",
        "pattern-case-scrutinee-pattern-case",
        "pattern-case-ambient-scalar",
        "pattern-case-captured-scalar",
        "scalar-pattern-case-closure-result",
        "scalar-pattern-case-tail-function",
        "pattern-case-call-argument"
      ]
    assertTransported name = do
      let fixture = producerEdgeFixture name
      firstProduction <- produceFixture fixture
      secondProduction <- produceFixture fixture
      assertEqual (name <> " repeatable production") firstProduction secondProduction
      case typedCoreProductionStatus firstProduction of
        TypedCoreProductionSucceeded typedProgram -> do
          assertEqual (name <> " typed validation") [] (validateTypedProgram typedProgram)
          let firstLowering = lowerTypedCoreExpressionDirectCall typedProgram
              secondLowering = lowerTypedCoreExpressionDirectCall typedProgram
          assertEqual (name <> " repeatable lowering") firstLowering secondLowering
          case firstLowering of
            LoweredIRSucceeded loweredProgram -> do
              assertEqual (name <> " lowered validation") [] (validateLoweredProgram loweredProgram)
              case lookup name expectedPatternCaseControlFlows of
                Just expectedControlFlow ->
                  assertEqual
                    (name <> " exact pattern-case control flow")
                    expectedControlFlow
                    (patternCaseControlFlow loweredProgram)
                Nothing -> pure ()
              case lookup name expectedPatternCaseTransportShapes of
                Just expectedTransportShape ->
                  assertEqual
                    (name <> " exact pattern-case transport shape")
                    expectedTransportShape
                    (patternCaseTransportShape loweredProgram)
                Nothing -> pure ()
              case lookup name expectedPatternCaseJoinOperations of
                Just (joinBlockId, expectedOperations) ->
                  assertEqual
                    (name <> " exact post-join operations")
                    [expectedOperations]
                    (blockOperations loweredProgram joinBlockId)
                Nothing -> pure ()
              case lookup name expectedClosureCallCounts of
                Just expectedCount ->
                  assertEqual
                    (name <> " closure application count")
                    expectedCount
                    (closureCallCount loweredProgram)
                Nothing -> pure ()
            other -> failTest (name <> " did not lower: " <> Text.pack (show other))
        other -> failTest (name <> " did not produce typed core: " <> Text.pack (show other))

patternCaseControlFlow :: LoweredProgram -> [(LoweredFunctionId, [LoweredBlock])]
patternCaseControlFlow (LoweredProgram _ _ _ functions _) =
  [ (functionId, blocks)
  | LoweredFunction functionId _ _ _ blocks _ <- functions,
    any isPatternCaseBlock blocks
  ]
  where
    isPatternCaseBlock (LoweredBlock (LoweredBlockId blockId) _ _ _) =
      "case$" `Text.isPrefixOf` blockId

patternCaseTransportShape :: LoweredProgram -> [(LoweredFunctionId, [(LoweredBlockId, [LoweredRepresentation], [(LoweredBlockId, [LoweredRepresentation])])])]
patternCaseTransportShape (LoweredProgram _ _ _ functions _) =
  [ (functionId, map blockShape caseBlocks)
  | LoweredFunction functionId _ _ _ blocks _ <- functions,
    let caseBlocks = filter isPatternCaseBlock blocks,
    not (null caseBlocks)
  ]
  where
    isPatternCaseBlock (LoweredBlock (LoweredBlockId blockId) _ _ _) =
      "case$" `Text.isPrefixOf` blockId
    blockShape (LoweredBlock blockId parameters _ terminator) =
      (blockId, map parameterRepresentation parameters, maybe [] successorShapes terminator)
    parameterRepresentation (LoweredParameter _ representation) = representation
    successorShapes terminator =
      case terminator of
        LoweredJump target arguments -> [(target, map operandRepresentation arguments)]
        LoweredBranch _ trueTarget trueArguments falseTarget falseArguments ->
          [ (trueTarget, map operandRepresentation trueArguments),
            (falseTarget, map operandRepresentation falseArguments)
          ]
        _ -> []
    operandRepresentation operand =
      case operand of
        LoweredFunctionParameterOperand _ representation -> representation
        LoweredBlockParameterOperand _ representation -> representation
        LoweredTemporaryOperand _ representation -> representation
        LoweredImmediateOperand immediate ->
          case immediate of
            LoweredUnitImmediate -> LoweredUnitRepresentation
            LoweredBoolImmediate {} -> LoweredBoolRepresentation
            LoweredSignedIntegerImmediate width _ -> LoweredSignedIntegerRepresentation width
            LoweredUnsignedIntegerImmediate width _ -> LoweredUnsignedIntegerRepresentation width
            LoweredFloatImmediate width _ -> LoweredFloatRepresentation width
            LoweredCharImmediate {} -> LoweredCharRepresentation

blockOperations :: LoweredProgram -> LoweredBlockId -> [[LoweredOperation]]
blockOperations (LoweredProgram _ _ _ functions _) targetBlockId =
  [ operations
  | LoweredFunction _ _ _ _ blocks _ <- functions,
    LoweredBlock blockId _ instructions _ <- blocks,
    blockId == targetBlockId,
    let operations = [operation | LoweredInstruction _ _ operation <- instructions]
  ]

closureCallCount :: LoweredProgram -> Int
closureCallCount (LoweredProgram _ _ _ functions _) =
  length
    [ ()
    | LoweredFunction _ _ _ _ blocks _ <- functions,
      LoweredBlock _ _ instructions _ <- blocks,
      LoweredInstruction _ _ LoweredClosureCall {} <- instructions
    ]

expectedPatternCaseJoinOperations :: [(Text, (LoweredBlockId, [LoweredOperation]))]
expectedPatternCaseJoinOperations =
  [ ( "pattern-case-ambient-scalar",
      ( LoweredBlockId "case$s1$3$e1$0$join",
        [ LoweredPrimitiveOperation
            (LoweredArithmeticPrimitive LoweredAdd)
            [ LoweredBlockParameterOperand (LoweredParameterId "result") intRepresentation,
              LoweredBlockParameterOperand (LoweredParameterId "live1") intRepresentation
            ]
        ]
      )
    ),
    ( "pattern-case-call-argument",
      ( LoweredBlockId "case$s1$0$e2$0,1$join",
        [ LoweredClosureCall
            (LoweredBlockParameterOperand (LoweredParameterId "live1") closureRepresentation)
            [LoweredBlockParameterOperand (LoweredParameterId "result") intRepresentation]
        ]
      )
    )
  ]
  where
    intRepresentation = LoweredSignedIntegerRepresentation LoweredIntegerWidth64
    closureRepresentation =
      LoweredClosureRepresentation
        (LoweredCallSignature [intRepresentation] intRepresentation)

expectedClosureCallCounts :: [(Text, Int)]
expectedClosureCallCounts =
  [ ("pattern-case-captured-scalar", 1),
    ("scalar-pattern-case-closure-result", 1),
    ("pattern-case-call-argument", 1)
  ]

expectedPatternCaseTransportShapes :: [(Text, [(LoweredFunctionId, [(LoweredBlockId, [LoweredRepresentation], [(LoweredBlockId, [LoweredRepresentation])])])])]
expectedPatternCaseTransportShapes =
  [ ( "pattern-case-ambient-scalar",
      [ ( entryFunction,
          [ shape ambientPrefix "$a0$guard" intPair [("$a0$body", intPair), ("$a1$body", intPair)],
            shape ambientPrefix "$a0$body" intPair [("$join", intPair)],
            shape ambientPrefix "$a1$body" intPair [("$join", intPair)],
            shape ambientPrefix "$join" intPair []
          ]
        )
      ]
    ),
    ( "pattern-case-captured-scalar",
      [ ( LoweredFunctionId "App::Main::choose",
          [ shape capturedPrefix "$a0$guard" intSingle [("$a0$body", intSingle), ("$a1$body", intSingle)],
            shape capturedPrefix "$a0$body" intSingle [],
            shape capturedPrefix "$a1$body" intSingle []
          ]
        )
      ]
    ),
    ( "scalar-pattern-case-closure-result",
      [ ( LoweredFunctionId "App::Main::choose",
          [ shape closureResultPrefix "$a0$body" [] [],
            shape closureResultPrefix "$a1$body" [] []
          ]
        )
      ]
    ),
    ( "pattern-case-call-argument",
      [ ( entryFunction,
          [ shape callArgumentPrefix "$a0$body" closureSingle [("$join", closureAndInt)],
            shape callArgumentPrefix "$a1$body" closureSingle [("$join", closureAndInt)],
            shape callArgumentPrefix "$join" closureAndInt []
          ]
        )
      ]
    )
  ]
  where
    entryFunction = LoweredFunctionId "App::Main::$entry"
    intRepresentation = LoweredSignedIntegerRepresentation LoweredIntegerWidth64
    intSingle = [intRepresentation]
    intPair = [intRepresentation, intRepresentation]
    closureRepresentation =
      LoweredClosureRepresentation
        (LoweredCallSignature [intRepresentation] intRepresentation)
    closureSingle = [closureRepresentation]
    closureAndInt = [closureRepresentation, intRepresentation]
    ambientPrefix = "case$s1$3$e1$0"
    capturedPrefix = "case$s1$1$e2$0,0"
    closureResultPrefix = "case$s1$0$e2$0,0"
    callArgumentPrefix = "case$s1$0$e2$0,1"
    shape prefix suffix parameters successors =
      ( LoweredBlockId (prefix <> suffix),
        parameters,
        [(LoweredBlockId (prefix <> targetSuffix), arguments) | (targetSuffix, arguments) <- successors]
      )

expectedPatternCaseControlFlows :: [(Text, [(LoweredFunctionId, [LoweredBlock])])]
expectedPatternCaseControlFlows =
  [ ( "pattern-case-in-conditional-branch",
      [ ( functionId "$entry",
          [ LoweredBlock
              entryBlockId
              []
              []
              (Just (LoweredBranch (boolImmediate True) outerThenBlockId [] outerElseBlockId [])),
            LoweredBlock
              outerThenBlockId
              []
              [comparisonInstruction 1 (intImmediate 1) (intImmediate 1)]
              ( Just
                  ( LoweredBranch
                      (temporary 1 LoweredBoolRepresentation)
                      nestedFirstBodyBlockId
                      []
                      nestedFinalBodyBlockId
                      []
                  )
              ),
            LoweredBlock
              nestedFirstBodyBlockId
              []
              []
              (Just (LoweredJump nestedJoinBlockId [intImmediate 10])),
            LoweredBlock
              nestedFinalBodyBlockId
              []
              []
              (Just (LoweredJump nestedJoinBlockId [intImmediate 20])),
            LoweredBlock
              nestedJoinBlockId
              [parameter "result" intRepresentation]
              []
              (Just (LoweredJump outerJoinBlockId [blockParameter "result" intRepresentation])),
            LoweredBlock
              outerElseBlockId
              []
              []
              (Just (LoweredJump outerJoinBlockId [intImmediate 30])),
            LoweredBlock
              outerJoinBlockId
              [parameter "result" intRepresentation]
              []
              (Just (LoweredReturn (blockParameter "result" intRepresentation)))
          ]
        )
      ]
    ),
    ( "conditional-in-pattern-case-guard",
      [ ( functionId "$entry",
          [ LoweredBlock
              entryBlockId
              []
              [comparisonInstruction 1 (intImmediate 1) (intImmediate 1)]
              ( Just
                  ( LoweredBranch
                      (temporary 1 LoweredBoolRepresentation)
                      guardedArmGuardBlockId
                      []
                      guardedFinalBodyBlockId
                      []
                  )
              ),
            LoweredBlock
              guardedArmGuardBlockId
              []
              []
              (Just (LoweredBranch (boolImmediate True) guardThenBlockId [] guardElseBlockId [])),
            LoweredBlock
              guardThenBlockId
              []
              []
              (Just (LoweredJump guardJoinBlockId [boolImmediate False])),
            LoweredBlock
              guardElseBlockId
              []
              []
              (Just (LoweredJump guardJoinBlockId [boolImmediate True])),
            LoweredBlock
              guardJoinBlockId
              [parameter "result" LoweredBoolRepresentation]
              []
              ( Just
                  ( LoweredBranch
                      (blockParameter "result" LoweredBoolRepresentation)
                      guardedArmBodyBlockId
                      []
                      guardedFinalBodyBlockId
                      []
                  )
              ),
            LoweredBlock
              guardedArmBodyBlockId
              []
              []
              (Just (LoweredJump guardedJoinBlockId [intImmediate 10])),
            LoweredBlock
              guardedFinalBodyBlockId
              []
              []
              (Just (LoweredJump guardedJoinBlockId [intImmediate 20])),
            LoweredBlock
              guardedJoinBlockId
              [parameter "result" intRepresentation]
              []
              (Just (LoweredReturn (blockParameter "result" intRepresentation)))
          ]
        )
      ]
    ),
    ( "pattern-case-in-pattern-case-body",
      [ ( functionId "$entry",
          [ LoweredBlock
              entryBlockId
              []
              [comparisonInstruction 1 (boolImmediate True) (boolImmediate True)]
              ( Just
                  ( LoweredBranch
                      (temporary 1 LoweredBoolRepresentation)
                      outerCaseFirstBodyBlockId
                      []
                      outerCaseFinalBodyBlockId
                      []
                  )
              ),
            LoweredBlock
              outerCaseFirstBodyBlockId
              []
              [comparisonInstruction 1 (intImmediate 1) (intImmediate 1)]
              ( Just
                  ( LoweredBranch
                      (temporary 1 LoweredBoolRepresentation)
                      bodyCaseFirstBodyBlockId
                      []
                      bodyCaseFinalBodyBlockId
                      []
                  )
              ),
            LoweredBlock
              bodyCaseFirstBodyBlockId
              []
              []
              (Just (LoweredJump bodyCaseJoinBlockId [intImmediate 10])),
            LoweredBlock
              bodyCaseFinalBodyBlockId
              []
              []
              (Just (LoweredJump bodyCaseJoinBlockId [intImmediate 20])),
            LoweredBlock
              bodyCaseJoinBlockId
              [parameter "result" intRepresentation]
              []
              (Just (LoweredJump outerCaseJoinBlockId [blockParameter "result" intRepresentation])),
            LoweredBlock
              outerCaseFinalBodyBlockId
              []
              []
              (Just (LoweredJump outerCaseJoinBlockId [intImmediate 30])),
            LoweredBlock
              outerCaseJoinBlockId
              [parameter "result" intRepresentation]
              []
              (Just (LoweredReturn (blockParameter "result" intRepresentation)))
          ]
        )
      ]
    ),
    ( "pattern-case-scrutinee-pattern-case",
      [ ( functionId "$entry",
          [ LoweredBlock
              entryBlockId
              []
              [comparisonInstruction 1 (boolImmediate True) (boolImmediate True)]
              ( Just
                  ( LoweredBranch
                      (temporary 1 LoweredBoolRepresentation)
                      scrutineeCaseFirstBodyBlockId
                      []
                      scrutineeCaseFinalBodyBlockId
                      []
                  )
              ),
            LoweredBlock
              scrutineeCaseFirstBodyBlockId
              []
              []
              (Just (LoweredJump scrutineeCaseJoinBlockId [intImmediate 1])),
            LoweredBlock
              scrutineeCaseFinalBodyBlockId
              []
              []
              (Just (LoweredJump scrutineeCaseJoinBlockId [intImmediate 2])),
            LoweredBlock
              scrutineeCaseJoinBlockId
              [parameter "result" intRepresentation]
              [ comparisonInstruction
                  1
                  (blockParameter "result" intRepresentation)
                  (intImmediate 1)
              ]
              ( Just
                  ( LoweredBranch
                      (temporary 1 LoweredBoolRepresentation)
                      scrutineeOuterFirstBodyBlockId
                      [blockParameter "result" intRepresentation]
                      scrutineeOuterFinalBodyBlockId
                      [blockParameter "result" intRepresentation]
                  )
              ),
            LoweredBlock
              scrutineeOuterFirstBodyBlockId
              [parameter "live1" intRepresentation]
              []
              (Just (LoweredJump scrutineeOuterJoinBlockId [intImmediate 10])),
            LoweredBlock
              scrutineeOuterFinalBodyBlockId
              [parameter "live1" intRepresentation]
              []
              (Just (LoweredJump scrutineeOuterJoinBlockId [intImmediate 20])),
            LoweredBlock
              scrutineeOuterJoinBlockId
              [parameter "result" intRepresentation]
              []
              (Just (LoweredReturn (blockParameter "result" intRepresentation)))
          ]
        )
      ]
    ),
    ( "scalar-pattern-case-tail-function",
      [ ( functionId "loop",
          [ LoweredBlock
              entryBlockId
              []
              [comparisonInstruction 1 (functionParameter "arg1" intRepresentation) (intImmediate 0)]
              ( Just
                  ( LoweredBranch
                      (temporary 1 LoweredBoolRepresentation)
                      tailCaseFirstBodyBlockId
                      []
                      tailCaseSecondTestBlockId
                      []
                  )
              ),
            LoweredBlock
              tailCaseFirstBodyBlockId
              []
              []
              (Just (LoweredReturn (intImmediate 0))),
            LoweredBlock
              tailCaseSecondTestBlockId
              []
              [comparisonInstruction 1 (functionParameter "arg1" intRepresentation) (intImmediate 1)]
              ( Just
                  ( LoweredBranch
                      (temporary 1 LoweredBoolRepresentation)
                      tailCaseSecondBodyBlockId
                      []
                      tailCaseGuardBlockId
                      []
                  )
              ),
            LoweredBlock
              tailCaseSecondBodyBlockId
              []
              []
              (Just (LoweredDirectTailCall (functionId "loop") [intImmediate 0])),
            LoweredBlock
              tailCaseGuardBlockId
              []
              [comparisonInstruction 1 (functionParameter "arg1" intRepresentation) (intImmediate 2)]
              ( Just
                  ( LoweredBranch
                      (temporary 1 LoweredBoolRepresentation)
                      tailCaseGuardBodyBlockId
                      []
                      tailCaseFinalBodyBlockId
                      []
                  )
              ),
            LoweredBlock
              tailCaseGuardBodyBlockId
              []
              []
              (Just (LoweredReturn (functionParameter "arg1" intRepresentation))),
            LoweredBlock
              tailCaseFinalBodyBlockId
              []
              []
              (Just (LoweredReturn (intImmediate 3)))
          ]
        )
      ]
    )
  ]
  where
    functionId :: Text -> LoweredFunctionId
    functionId name = LoweredFunctionId ("App::Main::" <> name)
    blockId :: Text -> LoweredBlockId
    blockId = LoweredBlockId
    parameter :: Text -> LoweredRepresentation -> LoweredParameter
    parameter name representation =
      LoweredParameter (LoweredParameterId name) representation
    blockParameter :: Text -> LoweredRepresentation -> LoweredOperand
    blockParameter name representation =
      LoweredBlockParameterOperand (LoweredParameterId name) representation
    functionParameter :: Text -> LoweredRepresentation -> LoweredOperand
    functionParameter name representation =
      LoweredFunctionParameterOperand (LoweredParameterId name) representation
    temporary :: Int -> LoweredRepresentation -> LoweredOperand
    temporary index representation =
      LoweredTemporaryOperand
        (LoweredTemporaryId ("t" <> Text.pack (show index)))
        representation
    boolImmediate :: Bool -> LoweredOperand
    boolImmediate = LoweredImmediateOperand . LoweredBoolImmediate
    intImmediate :: Integer -> LoweredOperand
    intImmediate =
      LoweredImmediateOperand
        . LoweredSignedIntegerImmediate LoweredIntegerWidth64
    comparisonInstruction :: Int -> LoweredOperand -> LoweredOperand -> LoweredInstruction
    comparisonInstruction index left right =
      LoweredInstruction
        (LoweredTemporaryId ("t" <> Text.pack (show index)))
        LoweredBoolRepresentation
        ( LoweredPrimitiveOperation
            (LoweredComparisonPrimitive LoweredEqual)
            [left, right]
        )
    intRepresentation =
      LoweredSignedIntegerRepresentation LoweredIntegerWidth64
    entryBlockId = blockId "entry"
    outerThenBlockId = blockId "if$s1$0$e1$0$then"
    outerElseBlockId = blockId "if$s1$0$e1$0$else"
    outerJoinBlockId = blockId "if$s1$0$e1$0$join"
    nestedFirstBodyBlockId = blockId "case$s1$0$e2$0,1$a0$body"
    nestedFinalBodyBlockId = blockId "case$s1$0$e2$0,1$a1$body"
    nestedJoinBlockId = blockId "case$s1$0$e2$0,1$join"
    guardedArmGuardBlockId = blockId "case$s1$0$e1$0$a0$guard"
    guardedArmBodyBlockId = blockId "case$s1$0$e1$0$a0$body"
    guardedFinalBodyBlockId = blockId "case$s1$0$e1$0$a1$body"
    guardedJoinBlockId = blockId "case$s1$0$e1$0$join"
    guardThenBlockId = blockId "if$s1$0$e3$0,1,0$then"
    guardElseBlockId = blockId "if$s1$0$e3$0,1,0$else"
    guardJoinBlockId = blockId "if$s1$0$e3$0,1,0$join"
    outerCaseFirstBodyBlockId = blockId "case$s1$0$e1$0$a0$body"
    outerCaseFinalBodyBlockId = blockId "case$s1$0$e1$0$a1$body"
    outerCaseJoinBlockId = blockId "case$s1$0$e1$0$join"
    bodyCaseFirstBodyBlockId = blockId "case$s1$0$e3$0,1,1$a0$body"
    bodyCaseFinalBodyBlockId = blockId "case$s1$0$e3$0,1,1$a1$body"
    bodyCaseJoinBlockId = blockId "case$s1$0$e3$0,1,1$join"
    scrutineeCaseFirstBodyBlockId = blockId "case$s1$0$e2$0,0$a0$body"
    scrutineeCaseFinalBodyBlockId = blockId "case$s1$0$e2$0,0$a1$body"
    scrutineeCaseJoinBlockId = blockId "case$s1$0$e2$0,0$join"
    scrutineeOuterFirstBodyBlockId = blockId "case$s1$0$e1$0$a0$body"
    scrutineeOuterFinalBodyBlockId = blockId "case$s1$0$e1$0$a1$body"
    scrutineeOuterJoinBlockId = blockId "case$s1$0$e1$0$join"
    tailCaseFirstBodyBlockId = blockId "case$s1$1$e2$0,0$a0$body"
    tailCaseSecondTestBlockId = blockId "case$s1$1$e2$0,0$a1$test"
    tailCaseSecondBodyBlockId = blockId "case$s1$1$e2$0,0$a1$body"
    tailCaseGuardBlockId = blockId "case$s1$1$e2$0,0$a2$guard"
    tailCaseGuardBodyBlockId = blockId "case$s1$1$e2$0,0$a2$body"
    tailCaseFinalBodyBlockId = blockId "case$s1$1$e2$0,0$a3$body"

testConditionalProfileCoverage :: IO ()
testConditionalProfileCoverage =
  mapM_ assertConditionalProfile names
  where
    names =
      [ "conditional-function-parameter",
        "conditional-captured-scalar",
        "conditional-tail-call-function",
        "conditional-closure-result-application",
        "nested-conditionals"
      ]
    assertConditionalProfile name = do
      let fixture = producerEdgeFixture name
      ordinary <- inferFixture fixture
      firstProduction <- produceFixture fixture
      secondProduction <- produceFixture fixture
      assertEqual (name <> " inference compatibility") ordinary (typedCoreProductionInferenceResult firstProduction)
      assertEqual (name <> " repeatable production") firstProduction secondProduction
      case typedCoreProductionStatus firstProduction of
        TypedCoreProductionSucceeded typedProgram -> do
          assertEqual (name <> " typed validation") [] (validateTypedProgram typedProgram)
          let firstLowering = lowerTypedCoreExpressionDirectCall typedProgram
              secondLowering = lowerTypedCoreExpressionDirectCall typedProgram
          assertEqual (name <> " repeatable lowering") firstLowering secondLowering
          case firstLowering of
            LoweredIRSucceeded loweredProgram -> do
              assertEqual (name <> " lowered validation") [] (validateLoweredProgram loweredProgram)
              case lookup name expectedConditionalControlFlows of
                Just expectedControlFlow ->
                  assertEqual
                    (name <> " exact conditional control flow")
                    expectedControlFlow
                    (conditionalControlFlow loweredProgram)
                Nothing -> failTest (name <> " is missing a conditional control-flow expectation")
            other -> failTest (name <> " did not lower: " <> Text.pack (show other))
        other -> failTest (name <> " did not produce typed core: " <> Text.pack (show other))

conditionalControlFlow :: LoweredProgram -> [(LoweredFunctionId, [LoweredBlock])]
conditionalControlFlow (LoweredProgram _ _ _ functions _) =
  [ (functionId, blocks)
  | LoweredFunction functionId _ _ _ blocks _ <- functions,
    any containsBranch blocks
  ]
  where
    containsBranch (LoweredBlock _ _ _ (Just (LoweredBranch _ _ _ _ _))) = True
    containsBranch _ = False

expectedConditionalControlFlows :: [(Text, [(LoweredFunctionId, [LoweredBlock])])]
expectedConditionalControlFlows =
  [ ( "conditional-function-parameter",
      [ ( functionId "choose",
          [ LoweredBlock
              entryBlockId
              []
              []
              ( Just
                  ( LoweredBranch
                      (functionParameter "arg1" LoweredBoolRepresentation)
                      parameterThenBlockId
                      []
                      parameterElseBlockId
                      []
                  )
              ),
            LoweredBlock
              parameterThenBlockId
              []
              []
              (Just (LoweredReturn (functionParameter "arg2" intRepresentation))),
            LoweredBlock
              parameterElseBlockId
              []
              []
              (Just (LoweredReturn (intImmediate 0)))
          ]
        )
      ]
    ),
    ( "conditional-captured-scalar",
      [ ( functionId "choose",
          [ LoweredBlock
              entryBlockId
              []
              [ LoweredInstruction
                  (LoweredTemporaryId "t1")
                  intRepresentation
                  ( LoweredProjectField
                      capturedSeedLayoutId
                      0
                      (functionParameter "environment" (LoweredManagedReferenceRepresentation capturedSeedLayoutId))
                  )
              ]
              ( Just
                  ( LoweredBranch
                      (functionParameter "arg1" LoweredBoolRepresentation)
                      capturedThenBlockId
                      [temporary 1 intRepresentation]
                      capturedElseBlockId
                      [temporary 1 intRepresentation]
                  )
              ),
            LoweredBlock
              capturedThenBlockId
              [parameter "live1" intRepresentation]
              []
              (Just (LoweredReturn (blockParameter "live1" intRepresentation))),
            LoweredBlock
              capturedElseBlockId
              [parameter "live1" intRepresentation]
              [ LoweredInstruction
                  (LoweredTemporaryId "t1")
                  intRepresentation
                  ( LoweredPrimitiveOperation
                      (LoweredArithmeticPrimitive LoweredAdd)
                      [blockParameter "live1" intRepresentation, intImmediate 2]
                  )
              ]
              (Just (LoweredReturn (temporary 1 intRepresentation)))
          ]
        )
      ]
    ),
    ( "conditional-tail-call-function",
      [ ( functionId "loop",
          [ LoweredBlock
              entryBlockId
              []
              []
              ( Just
                  ( LoweredBranch
                      (functionParameter "arg1" LoweredBoolRepresentation)
                      tailThenBlockId
                      []
                      tailElseBlockId
                      []
                  )
              ),
            LoweredBlock
              tailThenBlockId
              []
              []
              (Just (LoweredReturn (functionParameter "arg2" intRepresentation))),
            LoweredBlock
              tailElseBlockId
              []
              []
              ( Just
                  ( LoweredDirectTailCall
                      (functionId "loop")
                      [boolImmediate True, functionParameter "arg2" intRepresentation]
                  )
              )
          ]
        )
      ]
    ),
    ( "conditional-closure-result-application",
      [ ( functionId "$entry",
          [ LoweredBlock
              entryBlockId
              []
              []
              ( Just
                  ( LoweredBranch
                      (boolImmediate True)
                      closureThenBlockId
                      []
                      closureElseBlockId
                      []
                  )
              ),
            LoweredBlock
              closureThenBlockId
              []
              [ LoweredInstruction
                  (LoweredTemporaryId "t1")
                  (LoweredManagedReferenceRepresentation identityLayoutId)
                  (LoweredConstructProduct identityLayoutId []),
                LoweredInstruction
                  (LoweredTemporaryId "t2")
                  boolClosureRepresentation
                  ( LoweredConstructClosure
                      (functionId "identity")
                      (temporary 1 (LoweredManagedReferenceRepresentation identityLayoutId))
                  )
              ]
              (Just (LoweredJump closureJoinBlockId [temporary 2 boolClosureRepresentation])),
            LoweredBlock
              closureElseBlockId
              []
              [ LoweredInstruction
                  (LoweredTemporaryId "t1")
                  (LoweredManagedReferenceRepresentation alwaysFalseLayoutId)
                  (LoweredConstructProduct alwaysFalseLayoutId []),
                LoweredInstruction
                  (LoweredTemporaryId "t2")
                  boolClosureRepresentation
                  ( LoweredConstructClosure
                      (functionId "alwaysFalse")
                      (temporary 1 (LoweredManagedReferenceRepresentation alwaysFalseLayoutId))
                  )
              ]
              (Just (LoweredJump closureJoinBlockId [temporary 2 boolClosureRepresentation])),
            LoweredBlock
              closureJoinBlockId
              [parameter "result" boolClosureRepresentation]
              [ LoweredInstruction
                  (LoweredTemporaryId "t1")
                  LoweredBoolRepresentation
                  ( LoweredClosureCall
                      (blockParameter "result" boolClosureRepresentation)
                      [boolImmediate True]
                  )
              ]
              (Just (LoweredReturn (temporary 1 LoweredBoolRepresentation)))
          ]
        )
      ]
    ),
    ( "nested-conditionals",
      [ ( functionId "$entry",
          [ LoweredBlock
              entryBlockId
              []
              []
              (Just (LoweredBranch (boolImmediate True) nestedConditionThenBlockId [] nestedConditionElseBlockId [])),
            LoweredBlock
              nestedConditionThenBlockId
              []
              []
              (Just (LoweredJump nestedConditionJoinBlockId [boolImmediate False])),
            LoweredBlock
              nestedConditionElseBlockId
              []
              []
              (Just (LoweredJump nestedConditionJoinBlockId [boolImmediate True])),
            LoweredBlock
              nestedConditionJoinBlockId
              [parameter "result" LoweredBoolRepresentation]
              []
              ( Just
                  ( LoweredBranch
                      (blockParameter "result" LoweredBoolRepresentation)
                      nestedOuterThenBlockId
                      []
                      nestedOuterElseBlockId
                      []
                  )
              ),
            LoweredBlock
              nestedOuterThenBlockId
              []
              []
              (Just (LoweredBranch (boolImmediate True) nestedThenThenBlockId [] nestedThenElseBlockId [])),
            LoweredBlock
              nestedThenThenBlockId
              []
              []
              (Just (LoweredJump nestedThenJoinBlockId [intImmediate 1])),
            LoweredBlock
              nestedThenElseBlockId
              []
              []
              (Just (LoweredJump nestedThenJoinBlockId [intImmediate 2])),
            LoweredBlock
              nestedThenJoinBlockId
              [parameter "result" intRepresentation]
              []
              (Just (LoweredJump nestedOuterJoinBlockId [blockParameter "result" intRepresentation])),
            LoweredBlock
              nestedOuterElseBlockId
              []
              []
              (Just (LoweredBranch (boolImmediate False) nestedElseThenBlockId [] nestedElseElseBlockId [])),
            LoweredBlock
              nestedElseThenBlockId
              []
              []
              (Just (LoweredJump nestedElseJoinBlockId [intImmediate 3])),
            LoweredBlock
              nestedElseElseBlockId
              []
              []
              (Just (LoweredJump nestedElseJoinBlockId [intImmediate 4])),
            LoweredBlock
              nestedElseJoinBlockId
              [parameter "result" intRepresentation]
              []
              (Just (LoweredJump nestedOuterJoinBlockId [blockParameter "result" intRepresentation])),
            LoweredBlock
              nestedOuterJoinBlockId
              [parameter "result" intRepresentation]
              []
              (Just (LoweredReturn (blockParameter "result" intRepresentation)))
          ]
        )
      ]
    )
  ]
  where
    functionId :: Text -> LoweredFunctionId
    functionId name = LoweredFunctionId ("App::Main::" <> name)
    blockId :: Text -> LoweredBlockId
    blockId = LoweredBlockId
    parameter :: Text -> LoweredRepresentation -> LoweredParameter
    parameter name representation = LoweredParameter (LoweredParameterId name) representation
    functionParameter :: Text -> LoweredRepresentation -> LoweredOperand
    functionParameter name representation = LoweredFunctionParameterOperand (LoweredParameterId name) representation
    blockParameter :: Text -> LoweredRepresentation -> LoweredOperand
    blockParameter name representation = LoweredBlockParameterOperand (LoweredParameterId name) representation
    temporary :: Int -> LoweredRepresentation -> LoweredOperand
    temporary index representation = LoweredTemporaryOperand (LoweredTemporaryId ("t" <> Text.pack (show index))) representation
    boolImmediate :: Bool -> LoweredOperand
    boolImmediate = LoweredImmediateOperand . LoweredBoolImmediate
    intImmediate :: Integer -> LoweredOperand
    intImmediate = LoweredImmediateOperand . LoweredSignedIntegerImmediate LoweredIntegerWidth64
    intRepresentation :: LoweredRepresentation
    intRepresentation = LoweredSignedIntegerRepresentation LoweredIntegerWidth64
    boolClosureRepresentation :: LoweredRepresentation
    boolClosureRepresentation =
      LoweredClosureRepresentation
        (LoweredCallSignature [LoweredBoolRepresentation] LoweredBoolRepresentation)
    entryBlockId = blockId "entry"
    parameterThenBlockId = blockId "if$s1$1$e3$0,0,0$then"
    parameterElseBlockId = blockId "if$s1$1$e3$0,0,0$else"
    capturedSeedLayoutId =
      LoweredLayoutId "$jz1$closure-env$m2$3:App$4:Main$p1$3$n6:choose"
    capturedThenBlockId = blockId "if$s1$3$e2$0,0$then"
    capturedElseBlockId = blockId "if$s1$3$e2$0,0$else"
    tailThenBlockId = blockId "if$s1$1$e3$0,0,0$then"
    tailElseBlockId = blockId "if$s1$1$e3$0,0,0$else"
    identityLayoutId =
      LoweredLayoutId "$jz1$closure-env$m2$3:App$4:Main$p1$1$n8:identity"
    alwaysFalseLayoutId =
      LoweredLayoutId "$jz1$closure-env$m2$3:App$4:Main$p1$3$n11:alwaysFalse"
    closureThenBlockId = blockId "if$s1$4$e2$0,0$then"
    closureElseBlockId = blockId "if$s1$4$e2$0,0$else"
    closureJoinBlockId = blockId "if$s1$4$e2$0,0$join"
    nestedConditionThenBlockId = blockId "if$s1$0$e2$0,0$then"
    nestedConditionElseBlockId = blockId "if$s1$0$e2$0,0$else"
    nestedConditionJoinBlockId = blockId "if$s1$0$e2$0,0$join"
    nestedOuterThenBlockId = blockId "if$s1$0$e1$0$then"
    nestedOuterElseBlockId = blockId "if$s1$0$e1$0$else"
    nestedOuterJoinBlockId = blockId "if$s1$0$e1$0$join"
    nestedThenThenBlockId = blockId "if$s1$0$e2$0,1$then"
    nestedThenElseBlockId = blockId "if$s1$0$e2$0,1$else"
    nestedThenJoinBlockId = blockId "if$s1$0$e2$0,1$join"
    nestedElseThenBlockId = blockId "if$s1$0$e2$0,2$then"
    nestedElseElseBlockId = blockId "if$s1$0$e2$0,2$else"
    nestedElseJoinBlockId = blockId "if$s1$0$e2$0,2$join"

testScalarBindingProduction :: IO ()
testScalarBindingProduction = do
  mapM_ assertProduced scalarBindingExpectedPrograms
  assertFailedBindingHidden
  where
    assertProduced (name, expectedProgram) = do
      let fixture = producerEdgeFixture name
      ordinary <- inferFixture fixture
      firstRun <- produceFixture fixture
      secondRun <- produceFixture fixture
      assertEqual (name <> " inference compatibility") ordinary (typedCoreProductionInferenceResult firstRun)
      assertEqual (name <> " repeatable production") firstRun secondRun
      assertEqual
        (name <> " exact typed program")
        (TypedCoreProductionSucceeded expectedProgram)
        (typedCoreProductionStatus firstRun)
      assertEqual (name <> " expected typed validation") [] (validateTypedProgram expectedProgram)

    assertFailedBindingHidden = do
      let fixture = producerEdgeFixture "scalar-binding-failed-initializer-hidden"
          expected =
            TypedCoreProductionUnsupported
              [ expressionFailure 0 [0] TypedCoreNonLocalCallUnsupported (TypedCoreNameDetail "__kernel_toFloat64"),
                expressionFailure 1 [] TypedCoreCaptureUnsupported (TypedCoreNameDetail "failed")
              ]
      firstRun <- produceFixture fixture
      secondRun <- produceFixture fixture
      assertEqual "failed scalar binding repeatable rejection" firstRun secondRun
      assertEqual "failed scalar binding remains hidden" expected (typedCoreProductionStatus firstRun)
    expressionFailure statementIndex childPath kind detail =
      TypedCoreProductionFailure
        (TypedCoreProductionExpressionPath ["App", "Main"] statementIndex childPath)
        kind
        detail

testManagedTextProduction :: IO ()
testManagedTextProduction =
  mapM_ assertProduced managedTextExpectedPrograms
  where
    assertProduced (name, expectedProgram) = do
      let fixture = producerEdgeFixture name
      ordinary <- inferFixture fixture
      firstRun <- produceFixture fixture
      secondRun <- produceFixture fixture
      assertEqual (name <> " inference compatibility") ordinary (typedCoreProductionInferenceResult firstRun)
      assertEqual (name <> " repeatable production") firstRun secondRun
      assertEqual
        (name <> " exact typed program")
        (TypedCoreProductionSucceeded expectedProgram)
        (typedCoreProductionStatus firstRun)
      assertEqual (name <> " expected typed validation") [] (validateTypedProgram expectedProgram)

testManagedTextOperationProduction :: IO ()
testManagedTextOperationProduction =
  mapM_ assertProduced managedTextOperationExpectedPrograms
  where
    assertProduced (name, expectedProgram) = do
      let fixture = producerEdgeFixture name
      ordinary <- inferFixture fixture
      firstRun <- produceFixture fixture
      secondRun <- produceFixture fixture
      assertEqual (name <> " inference compatibility") ordinary (typedCoreProductionInferenceResult firstRun)
      assertEqual (name <> " repeatable production") firstRun secondRun
      assertEqual
        (name <> " exact typed program")
        (TypedCoreProductionSucceeded expectedProgram)
        (typedCoreProductionStatus firstRun)
      assertEqual (name <> " expected typed validation") [] (validateTypedProgram expectedProgram)

testManagedTextOperationLowering :: IO ()
testManagedTextOperationLowering =
  mapM_ assertLowered managedTextOperationExpectedLoweredPrograms
  where
    assertLowered (name, typedProgram, expectedProgram) = do
      let firstRun = lowerTypedCoreExpressionDirectCall typedProgram
          secondRun = lowerTypedCoreExpressionDirectCall typedProgram
      assertEqual (name <> " valid typed core") [] (validateTypedProgram typedProgram)
      assertEqual (name <> " repeatable lowering") firstRun secondRun
      assertEqual (name <> " exact service lowering") (LoweredIRSucceeded expectedProgram) firstRun
      assertEqual (name <> " valid expected Lowered IR") [] (validateLoweredProgram expectedProgram)

testManagedTextKernelBoundaries :: IO ()
testManagedTextKernelBoundaries = do
  mapM_ assertKernelUnsupported unsupportedExpectations
  assertBlockedByDiagnostics "managed-text-oversaturated-length"
  where
    unsupportedExpectations =
      [ ( "managed-text-bare-length",
          TypedCoreCallableValueUnsupported,
          TypedCoreNameDetail "__kernel_textLength"
        ),
        ( "managed-text-partial-append",
          TypedCoreCallArityUnsupported,
          TypedCoreArityDetail 2 1
        ),
        ( "managed-text-partial-append-char",
          TypedCoreCallArityUnsupported,
          TypedCoreArityDetail 2 1
        )
      ]
    assertKernelUnsupported (name, kind, detail) = do
      let fixture = producerEdgeFixture name
          expected =
            TypedCoreProductionUnsupported
              [ TypedCoreProductionFailure
                  (TypedCoreProductionExpressionPath ["App", "Main"] 0 [])
                  kind
                  detail
              ]
      ordinary <- inferFixture fixture
      firstRun <- produceFixture fixture
      secondRun <- produceFixture fixture
      assertEqual (name <> " inference compatibility") ordinary (typedCoreProductionInferenceResult firstRun)
      assertEqual (name <> " repeatable rejection") firstRun secondRun
      assertEqual (name <> " exact producer boundary") expected (typedCoreProductionStatus firstRun)
    assertBlockedByDiagnostics name = do
      let fixture = producerEdgeFixture name
      ordinary <- inferFixture fixture
      firstRun <- produceFixture fixture
      secondRun <- produceFixture fixture
      assertEqual (name <> " inference compatibility") ordinary (typedCoreProductionInferenceResult firstRun)
      assertEqual (name <> " repeatable rejection") firstRun secondRun
      assertEqual (name <> " diagnostic precedence") TypedCoreProductionBlockedByDiagnostics (typedCoreProductionStatus firstRun)

testManagedTextLowering :: IO ()
testManagedTextLowering =
  mapM_ assertLowered managedTextExpectedLoweredPrograms
  where
    assertLowered (name, typedProgram, expectedProgram) = do
      let firstRun = lowerTypedCoreExpressionDirectCall typedProgram
          secondRun = lowerTypedCoreExpressionDirectCall typedProgram
      assertEqual (name <> " valid typed core") [] (validateTypedProgram typedProgram)
      assertEqual (name <> " repeatable lowering") firstRun secondRun
      assertEqual (name <> " exact managed Text lowering") (LoweredIRSucceeded expectedProgram) firstRun
      assertEqual (name <> " valid expected Lowered IR") [] (validateLoweredProgram expectedProgram)

testLexicalCaptureProduction :: IO ()
testLexicalCaptureProduction =
  mapM_ assertProduced lexicalCaptureExpectedPrograms
  where
    assertProduced (name, expectedProgram) = do
      let fixture =
            case lookup name producerEdgeFixtures of
              Just edgeFixture -> edgeFixture
              Nothing -> fixtureByName name
      ordinary <- inferFixture fixture
      firstRun <- produceFixture fixture
      secondRun <- produceFixture fixture
      assertEqual (name <> " inference compatibility") ordinary (typedCoreProductionInferenceResult firstRun)
      assertEqual (name <> " repeatable lexical production") firstRun secondRun
      assertEqual
        (name <> " exact lexical typed program")
        (TypedCoreProductionSucceeded expectedProgram)
        (typedCoreProductionStatus firstRun)
      assertEqual (name <> " expected lexical typed validation") [] (validateTypedProgram expectedProgram)

testCurriedApplicationProduction :: IO ()
testCurriedApplicationProduction =
  mapM_ assertProduced curriedApplicationExpectedPrograms
  where
    assertProduced (name, expectedProgram) = do
      let fixture = curriedApplicationFixture name
      firstRun <- produceFixture fixture
      secondRun <- produceFixture fixture
      assertEqual (name <> " repeatable production") firstRun secondRun
      assertEqual
        (name <> " exact typed program")
        (TypedCoreProductionSucceeded expectedProgram)
        (typedCoreProductionStatus firstRun)
      assertEqual (name <> " expected typed validation") [] (validateTypedProgram expectedProgram)

testCurriedApplicationLowering :: IO ()
testCurriedApplicationLowering =
  mapM_ assertLowered curriedApplicationExpectedLoweredPrograms
  where
    assertLowered (name, typedProgram, expectedProgram) = do
      let firstRun = lowerTypedCoreExpressionDirectCall typedProgram
          secondRun = lowerTypedCoreExpressionDirectCall typedProgram
      assertEqual (name <> " valid typed input") [] (validateTypedProgram typedProgram)
      assertEqual (name <> " repeatable lowering") firstRun secondRun
      assertEqual (name <> " exact lowered program") (LoweredIRSucceeded expectedProgram) firstRun
      case firstRun of
        LoweredIRSucceeded loweredProgram ->
          assertEqual (name <> " lowered validation") [] (validateLoweredProgram loweredProgram)
        other -> failTest (name <> " did not lower: " <> Text.pack (show other))

curriedApplicationFixture :: Text -> Fixture
curriedApplicationFixture name
  | name `elem` fixtureNames = fixtureByName name
  | otherwise = producerEdgeFixture name

testNonCallableOversaturationDiagnostic :: IO ()
testNonCallableOversaturationDiagnostic = do
  let fixture = producerEdgeFixture "non-callable-oversaturation-diagnostic"
  ordinary <- inferFixture fixture
  firstRun <- produceFixture fixture
  secondRun <- produceFixture fixture
  assertEqual "non-callable oversaturation inference compatibility" ordinary (typedCoreProductionInferenceResult firstRun)
  assertEqual "non-callable oversaturation repeatability" firstRun secondRun
  assertEqual
    "non-callable oversaturation has one ordinary type error"
    1
    (length (filter isErrorDiagnostic (inferredDiagnostics ordinary)))
  assertEqual
    "non-callable oversaturation blocks typed-core production"
    TypedCoreProductionBlockedByDiagnostics
    (typedCoreProductionStatus firstRun)
  assertEqual "blocked oversaturation has no validation proof" Nothing (typedCoreProductionValidatedProgram firstRun)

testLexicalCaptureLowering :: IO ()
testLexicalCaptureLowering =
  mapM_ assertLowered lexicalCaptureExpectedLoweredPrograms
  where
    assertLowered (name, typedProgram, expectedProgram) = do
      let firstRun = lowerTypedCoreExpressionDirectCall typedProgram
          secondRun = lowerTypedCoreExpressionDirectCall typedProgram
      assertEqual (name <> " is valid lexical typed core") [] (validateTypedProgram typedProgram)
      assertEqual (name <> " repeatable lexical lowering") firstRun secondRun
      assertEqual (name <> " exact lexical lowering") (LoweredIRSucceeded expectedProgram) firstRun
      assertEqual (name <> " expected lexical lowered validation") [] (validateLoweredProgram expectedProgram)

testLexicalCaptureFixtureMatrix :: IO ()
testLexicalCaptureFixtureMatrix = do
  mapM_ assertSupported lexicalABIs
  where
    assertSupported (name, expectedBinders, expectedLayouts, expectedFunctionIds, expectedNestedEnvironment) = do
      let fixture = producerEdgeFixture name
      firstRun <- produceFixture fixture
      secondRun <- produceFixture fixture
      assertEqual (name <> " repeatable typed production") firstRun secondRun
      case typedCoreProductionStatus firstRun of
        TypedCoreProductionSucceeded typedProgram -> do
          assertEqual (name <> " typed validation") [] (validateTypedProgram typedProgram)
          assertEqual (name <> " exact lambda binders") expectedBinders (typedLambdaBinders typedProgram)
          let firstLowering = lowerTypedCoreExpressionDirectCall typedProgram
              secondLowering = lowerTypedCoreExpressionDirectCall typedProgram
          assertEqual (name <> " repeatable lowering") firstLowering secondLowering
          case firstLowering of
            LoweredIRSucceeded loweredProgram -> do
              assertEqual (name <> " lowered validation") [] (validateLoweredProgram loweredProgram)
              assertEqual (name <> " exact environment layouts") expectedLayouts (loweredLayouts loweredProgram)
              assertEqual (name <> " exact function identities") expectedFunctionIds (loweredFunctionIds loweredProgram)
              case expectedNestedEnvironment of
                Just (nestedLayoutId, fields) ->
                  assertEqual (name <> " exact nested environment field order") [fields] (constructedEnvironmentFields nestedLayoutId loweredProgram)
                Nothing -> pure ()
            other -> failTest (name <> " did not lower: " <> Text.pack (show other))
        other -> failTest (name <> " did not produce typed core: " <> Text.pack (show other))

    lexicalABIs :: [(Text, [TypedBinderId], [LoweredLayout], [LoweredFunctionId], Maybe (LoweredLayoutId, [LoweredOperand]))]
    lexicalABIs =
      [ ( "inline-anonymous-lambda-call",
          [binder [0, 0] "item"],
          [layout [0, 0] "item" []],
          [lambdaFunction [0, 0] "item", loweredEntryFunctionId],
          Nothing
        ),
        ( "nested-scalar-capture",
          [binder [2] "outer", binder [2, 0] "item"],
          [layout [2] "outer" [int64Representation], layout [2, 0] "item" [int64Representation, int64Representation]],
          [lambdaFunction [2] "outer", lambdaFunction [2, 0] "item", loweredEntryFunctionId],
          Just
            ( layoutId [2, 0] "item",
              [ loweredParameter 1 int64Representation,
                loweredTemporary 1 int64Representation
              ]
            )
        ),
        ( "nested-shadow-capture-order",
          [binder [4] "outer", binder [4, 0] "left"],
          [layout [4] "outer" [int64Representation], layout [4, 0] "left" [int64Representation, int64Representation]],
          [lambdaFunction [4] "outer", lambdaFunction [4, 0] "left", loweredEntryFunctionId],
          Just
            ( layoutId [4, 0] "left",
              [ loweredTemporary 1 int64Representation,
                loweredParameter 1 int64Representation
              ]
            )
        ),
        ( "nested-closure-valued-capture",
          [binder [0] "predicate", binder [0, 0] "item"],
          [layout [0] "predicate" [], layout [0, 0] "item" [boolClosureRepresentation]],
          [lambdaFunction [0] "predicate", lambdaFunction [0, 0] "item", loweredEntryFunctionId],
          Just
            ( layoutId [0, 0] "item",
              [loweredParameter 1 boolClosureRepresentation]
            )
        )
      ]

    binder :: [Int] -> Text -> TypedBinderId
    binder path name =
      TypedBinderId (["App", "Main"], path, TypedResolvedName TypedCurrentModule TypedValueNamespace name)
    layout :: [Int] -> Text -> [LoweredRepresentation] -> LoweredLayout
    layout path name fields = LoweredLayout (layoutId path name) (LoweredClosureEnvironmentLayout fields)
    layoutId :: [Int] -> Text -> LoweredLayoutId
    layoutId path name = LoweredLayoutId (generatedIdentity "closure-env" path name)
    lambdaFunction :: [Int] -> Text -> LoweredFunctionId
    lambdaFunction path name = LoweredFunctionId (generatedIdentity "lambda-fn" path name)
    generatedIdentity :: Text -> [Int] -> Text -> Text
    generatedIdentity domain path name =
      "$jz1$"
        <> domain
        <> "$m2$3:App$4:Main$p"
        <> Text.pack (show (length path))
        <> "$"
        <> Text.intercalate "," (map (Text.pack . show) path)
        <> "$n"
        <> Text.pack (show (Text.length name))
        <> ":"
        <> name
    int64Representation = LoweredSignedIntegerRepresentation LoweredIntegerWidth64
    boolClosureRepresentation =
      LoweredClosureRepresentation (LoweredCallSignature [LoweredBoolRepresentation] LoweredBoolRepresentation)
    loweredEntryFunctionId = LoweredFunctionId "App::Main::$entry"
    loweredParameter :: Int -> LoweredRepresentation -> LoweredOperand
    loweredParameter index representation =
      LoweredFunctionParameterOperand (LoweredParameterId ("arg" <> Text.pack (show index))) representation
    loweredTemporary :: Int -> LoweredRepresentation -> LoweredOperand
    loweredTemporary index representation =
      LoweredTemporaryOperand (LoweredTemporaryId ("t" <> Text.pack (show index))) representation

    typedLambdaBinders (TypedProgram _ modules _) =
      concat
        [ concatMap statementLambdaBinders statements
        | TypedModule _ _ _ _ _ _ statements _ <- modules
        ]
    statementLambdaBinders statement =
      case statement of
        TypedLetStatement _ _ _ _ expression -> expressionLambdaBinders expression
        TypedExpressionStatement _ expression -> expressionLambdaBinders expression
        _ -> []
    expressionLambdaBinders expression =
      case expression of
        TypedLambdaExpr _ parameterBinder _ body -> parameterBinder : expressionLambdaBinders body
        TypedApplyExpr _ function argument -> expressionLambdaBinders function <> expressionLambdaBinders argument
        TypedBinaryExpr _ _ left right -> expressionLambdaBinders left <> expressionLambdaBinders right
        _ -> []
    loweredLayouts (LoweredProgram _ layouts _ _ _) = layouts
    loweredFunctionIds (LoweredProgram _ _ _ functions _) =
      [functionId | LoweredFunction functionId _ _ _ _ _ <- functions]
    constructedEnvironmentFields targetLayout (LoweredProgram _ _ _ functions _) =
      [ fields
      | LoweredFunction _ _ _ _ blocks _ <- functions,
        LoweredBlock _ _ instructions _ <- blocks,
        LoweredInstruction _ _ (LoweredConstructProduct layoutValue fields) <- instructions,
        layoutValue == targetLayout
      ]

testClosureCaptureReviewRegression :: Text -> IO ()
testClosureCaptureReviewRegression name = do
  let fixture = producerEdgeFixture name
  firstProduction <- produceFixture fixture
  secondProduction <- produceFixture fixture
  assertEqual (name <> " repeatable typed production") firstProduction secondProduction
  case typedCoreProductionStatus firstProduction of
    TypedCoreProductionSucceeded typedProgram -> do
      assertEqual (name <> " valid typed core") [] (validateTypedProgram typedProgram)
      let firstLowering = lowerTypedCoreExpressionDirectCall typedProgram
          secondLowering = lowerTypedCoreExpressionDirectCall typedProgram
      assertEqual (name <> " repeatable lowering") firstLowering secondLowering
      case firstLowering of
        LoweredIRSucceeded loweredProgram ->
          assertEqual (name <> " valid lowered IR") [] (validateLoweredProgram loweredProgram)
        other -> failTest (name <> " did not lower: " <> Text.pack (show other))
    other -> failTest (name <> " did not produce typed core: " <> Text.pack (show other))

testLiftedLambdaFailurePreorder :: IO ()
testLiftedLambdaFailurePreorder =
  case lookup "lifted-lambda-failure-preorder" reviewLowererBoundaryPrograms of
    Just programValue -> do
      let firstRun = lowerTypedCoreExpressionDirectCall programValue
          secondRun = lowerTypedCoreExpressionDirectCall programValue
      assertEqual "lifted conditional fixture is valid typed core" [] (validateTypedProgram programValue)
      assertEqual "lifted conditional lowering is repeatable" firstRun secondRun
      case firstRun of
        LoweredIRSucceeded loweredProgram ->
          assertEqual "lifted conditional lowered validation" [] (validateLoweredProgram loweredProgram)
        other -> failTest ("lifted conditional did not lower: " <> Text.pack (show other))
    Nothing -> failTest "lifted conditional regression fixture is missing"

testLiftedLambdaMetadataAlias :: IO ()
testLiftedLambdaMetadataAlias =
  case lookup "exported-scalar-lifted-lambda-name-collision" reviewLowererBoundaryPrograms of
    Just programValue -> do
      assertEqual "metadata collision fixture is valid typed core" [] (validateTypedProgram programValue)
      assertEqual
        "lifted lambda names cannot satisfy scalar module metadata"
        ( LoweredIRUnsupported
            [ LoweredIRLoweringFailure
                (TypedModulePath ["App", "Main"])
                LoweredIRUnsupportedModule
                LoweredIRNoFailureDetail
            ]
        )
        (lowerTypedCoreExpressionDirectCall programValue)
    Nothing -> failTest "metadata collision regression fixture is missing"

testScalarBindingLowering :: IO ()
testScalarBindingLowering =
  mapM_ assertLowered scalarBindingExpectedLoweredPrograms
  where
    assertLowered (name, typedProgram, expectedProgram) = do
      let firstRun = lowerTypedCoreExpressionDirectCall typedProgram
          secondRun = lowerTypedCoreExpressionDirectCall typedProgram
      assertEqual (name <> " is permanently valid typed core") [] (validateTypedProgram typedProgram)
      assertEqual (name <> " repeatable scalar lowering") firstRun secondRun
      assertEqual (name <> " exact scalar lowering") (LoweredIRSucceeded expectedProgram) firstRun
      assertEqual (name <> " expected lowered validation") [] (validateLoweredProgram expectedProgram)

testThreeArgumentDirectLeadingLambdaRecipe :: IO ()
testThreeArgumentDirectLeadingLambdaRecipe =
  case lookup "three-argument-direct-call" directCallExpectedPrograms of
    Just programValue ->
      assertEqual
        "three-argument direct leading-lambda validation"
        []
        (validateTypedProgram programValue)
    Nothing -> failTest "three-argument direct-call typed program is missing"

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
      [ ( "recursion-descendant-failure-order",
          [ expressionFailure
              2
              [0, 0, 1]
              LoweredIRCaptureUnsupported
              (LoweredIRNameFailureDetail (currentName "seed"))
          ]
        ),
        ( "interleaved-capture-mutual-recursion",
          [ statementFailure
              4
              LoweredIRRecursiveFunctionUnsupported
              (LoweredIRNameFailureDetail (currentName "right"))
          ]
        ),
        ( "non-concrete-closure-representation",
          [ statementFailure
              1
              LoweredIRInvalidFunctionShape
              (LoweredIRNameFailureDetail (currentName "identity"))
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
        ( "self-recursive-duplicate-parameter-function",
          [ statementFailure
              1
              LoweredIRRecursiveFunctionUnsupported
              (LoweredIRNameFailureDetail (currentName "loop")),
            expressionFailure
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
        ( "nested-lambda-closure-value-self-recursion",
          [ statementFailure
              3
              LoweredIRRecursiveFunctionUnsupported
              (LoweredIRNameFailureDetail (currentName "loop"))
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
            callableShapeFailure 1,
            TypedCoreValidationFailure
              (TypedExpressionPath ["App", "Main"] [1] [0])
              TypedCallableRecipeMismatch
              ( TypedRecipeDetail
                  (TypedClosureRecipe [TypedBoolRecipe] (TypedClosureRecipe [TypedBoolRecipe] TypedBoolRecipe))
                  (TypedClosureRecipe [TypedBoolRecipe, TypedBoolRecipe] TypedBoolRecipe)
              )
          ]
        ),
        ( "direct-shape-staged-recipe",
          [callableShapeFailure 1]
        ),
        ( "callable-shape-body-disagreement",
          [ TypedCoreValidationFailure
              (TypedExpressionPath ["App", "Main"] [3] [0])
              TypedLambdaResultMismatch
              ( TypedRecipeDetail
                  (TypedClosureRecipe [TypedBoolRecipe] (TypedClosureRecipe [TypedBoolRecipe] TypedBoolRecipe))
                  (TypedClosureRecipe [TypedBoolRecipe, TypedBoolRecipe] TypedBoolRecipe)
              ),
            TypedCoreValidationFailure
              (TypedExpressionPath ["App", "Main"] [3] [0, 0])
              TypedCallableShapeMismatch
              (TypedBinderDetail (TypedBinderId (["App", "Main"], [1], currentName "combine")))
          ]
        ),
        ( "variable-binder-reference-mismatch",
          [ TypedCoreValidationFailure
              (TypedExpressionPath ["App", "Main"] [2] [0])
              TypedBinderReferenceMismatch
              (TypedBinderDetail (TypedBinderId (["App", "Main"], [999], currentName "identity")))
          ]
        ),
        ( "direct-flattened-representation",
          [ TypedCoreValidationFailure
              (TypedExpressionPath ["App", "Main"] [2] [0])
              TypedCallableShapeMismatch
              (TypedBinderDetail (TypedBinderId (["App", "Main"], [1], currentName "combine")))
          ]
        ),
        ( "direct-shaped-closure-value-self-recursion",
          [ TypedCoreValidationFailure
              (TypedExpressionPath ["App", "Main"] [3] [0, 0, 1])
              TypedCallableShapeMismatch
              (TypedBinderDetail (TypedBinderId (["App", "Main"], [3], currentName "loop")))
          ]
        ),
        ( "shape-rejected-self-recursion",
          [callableShapeFailureFor 1 "loop"]
        ),
        ( "shape-rejected-mutual-recursion",
          [ callableShapeFailureFor 1 "left",
            callableShapeFailureFor 3 "right"
          ]
        ),
        ( "shape-rejected-binder-shadow-control",
          [callableShapeFailureFor 1 "loop"]
        ),
        ( "bare-function-value",
          [ TypedCoreValidationFailure
              (TypedExpressionPath ["App", "Main"] [2] [0])
              TypedCallableShapeMismatch
              (TypedBinderDetail (TypedBinderId (["App", "Main"], [1], currentName "identity")))
          ]
        ),
        ( "partial-direct-call",
          [ TypedCoreValidationFailure
              (TypedExpressionPath ["App", "Main"] [2] [0, 0])
              TypedCallableShapeMismatch
              (TypedBinderDetail (TypedBinderId (["App", "Main"], [1], currentName "combine")))
          ]
        )
      ]
    callableShapeFailure statementIndex =
      callableShapeFailureFor statementIndex "combine"
    callableShapeFailureFor statementIndex identifier =
      TypedCoreValidationFailure
        (TypedStatementPath ["App", "Main"] [statementIndex])
        TypedCallableShapeMismatch
        ( TypedBinderDetail
            (TypedBinderId (["App", "Main"], [statementIndex], currentName identifier))
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
      [ ( "managed-pattern-scrutinee",
          [ LoweredIRLoweringFailure
              (TypedExpressionPath ["App", "Main"] [0] [0])
              LoweredIRUnsupportedPattern
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

testCurriedArgumentCapture :: IO ()
testCurriedArgumentCapture = do
  let fixture = producerEdgeFixture "curried-first-argument-capture"
  firstRun <- produceFixture fixture
  secondRun <- produceFixture fixture
  assertEqual "captured direct-call argument repeatability" firstRun secondRun
  case typedCoreProductionStatus firstRun of
    TypedCoreProductionSucceeded typedProgram -> do
      assertEqual "captured direct-call argument typed validation" [] (validateTypedProgram typedProgram)
      case lowerTypedCoreExpressionDirectCall typedProgram of
        LoweredIRSucceeded loweredProgram ->
          assertEqual "captured direct-call argument lowered validation" [] (validateLoweredProgram loweredProgram)
        other -> failTest ("captured direct-call argument did not lower: " <> Text.pack (show other))
    other -> failTest ("captured direct-call argument did not produce typed core: " <> Text.pack (show other))

testPartialApplicationArgumentCapture :: IO ()
testPartialApplicationArgumentCapture = do
  let fixture = producerEdgeFixture "partial-call-argument-capture"
  ordinary <- inferFixture fixture
  firstRun <- produceFixture fixture
  secondRun <- produceFixture fixture
  assertEqual
    "partial-call argument inference compatibility"
    ordinary
    (typedCoreProductionInferenceResult firstRun)
  assertEqual "partial-call argument repeatability" firstRun secondRun
  case typedCoreProductionStatus firstRun of
    TypedCoreProductionSucceeded typedProgram -> do
      assertEqual "partial-call argument typed validation" [] (validateTypedProgram typedProgram)
      case lowerTypedCoreExpressionDirectCall typedProgram of
        LoweredIRSucceeded loweredProgram ->
          assertEqual "partial-call argument lowered validation" [] (validateLoweredProgram loweredProgram)
        other -> failTest ("partial-call argument did not lower: " <> Text.pack (show other))
    other -> failTest ("partial-call argument did not produce typed core: " <> Text.pack (show other))

testPartialApplicationManagedArgumentProduction :: IO ()
testPartialApplicationManagedArgumentProduction = do
  let name = "partial-call-managed-argument-failure"
      fixture = producerEdgeFixture name
      expected =
        case lookup name managedTextExpectedPrograms of
          Just program -> program
          Nothing -> error "managed Text partial-application expectation is missing"
  firstRun <- produceFixture fixture
  secondRun <- produceFixture fixture
  assertEqual "partial managed-argument repeatability" firstRun secondRun
  assertEqual
    "partial managed-argument exact typed program"
    (TypedCoreProductionSucceeded expected)
    (typedCoreProductionStatus firstRun)

testNonLocalCallArgumentFailureAccumulation :: IO ()
testNonLocalCallArgumentFailureAccumulation = do
  let fixture = producerEdgeFixture "non-local-call-argument-capture"
      expected =
        TypedCoreProductionUnsupported
          [ TypedCoreProductionFailure
              (TypedCoreProductionExpressionPath ["App", "Main"] 2 [])
              TypedCoreNonLocalCallUnsupported
              (TypedCoreNameDetail "__kernel_toFloat64")
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
    TypedCoreProductionSucceeded (TypedProgram _ [TypedModule _ _ _ _ _ _ statements _] _) ->
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

testEarlierCallerTransitiveCaptureAvailability :: IO ()
testEarlierCallerTransitiveCaptureAvailability = do
  let fixture = producerEdgeFixture "earlier-caller-transitive-recursive-capture"
      expected =
        TypedCoreProductionUnsupported
          [ TypedCoreProductionFailure
              (TypedCoreProductionStatementPath ["App", "Main"] 1)
              TypedCoreCaptureUnsupported
              (TypedCoreNameDetail "caller")
          ]
  firstRun <- produceFixture fixture
  secondRun <- produceFixture fixture
  assertEqual "earlier caller transitive capture repeatability" firstRun secondRun
  assertEqual "earlier caller transitive capture rejection" expected (typedCoreProductionStatus firstRun)

testCapturedNumericScalarReferenceSpecialization :: IO ()
testCapturedNumericScalarReferenceSpecialization = do
  let spanValue = SourceSpan 1 1
      literalType = TIntegerLiteralType (IntegerLiteralRange 1 1)
      uint8Type = TNumericType NumericUInt8
      functionType = TFunctionType uint8Type uint8Type
      loopDeclaration =
        ProvisionalCallableDeclaration
          1
          "loop"
          spanValue
          functionType
          (Just (PlainTypeBinding functionType))
          (Just [1])
      provisionalScope =
        ProvisionalScopeStatements
          [ ProvisionalScalarBinding
              0
              "seed"
              spanValue
              literalType
              (ProvisionalLiteralExpression (LInt 1) literalType),
            ProvisionalFunctionBinding
              loopDeclaration
              ( ProvisionalLambdaExpression
                  "item"
                  functionType
                  ( ProvisionalApplyExpression
                      uint8Type
                      (ProvisionalVariableExpression "loop" functionType)
                      (ProvisionalVariableExpression "seed" uint8Type)
                  )
              ),
            ProvisionalScalarBinding
              2
              "copy"
              spanValue
              literalType
              (ProvisionalVariableExpression "seed" literalType),
            ProvisionalTerminalExpression
              3
              spanValue
              ( ProvisionalApplyExpression
                  uint8Type
                  (ProvisionalVariableExpression "loop" functionType)
                  (ProvisionalLiteralExpression (LInt 1) uint8Type)
              )
          ]
  assertProvisionalProductionCompletes "captured numeric scalar" provisionalScope

testCapturedCompositeScalarSpecialization :: IO ()
testCapturedCompositeScalarSpecialization = do
  let spanValue = SourceSpan 1 1
      literalType = TIntegerLiteralType (IntegerLiteralRange 1 1)
      uint8Type = TNumericType NumericUInt8
      functionType = TFunctionType uint8Type uint8Type
      loopDeclaration =
        ProvisionalCallableDeclaration
          1
          "loop"
          spanValue
          functionType
          (Just (PlainTypeBinding functionType))
          (Just [1])
      provisionalScope =
        ProvisionalScopeStatements
          [ ProvisionalScalarBinding
              0
              "seed"
              spanValue
              literalType
              (ProvisionalLiteralExpression (LInt 1) literalType),
            ProvisionalFunctionBinding
              loopDeclaration
              ( ProvisionalLambdaExpression
                  "item"
                  functionType
                  ( ProvisionalApplyExpression
                      uint8Type
                      (ProvisionalVariableExpression "loop" functionType)
                      (ProvisionalVariableExpression "seed" uint8Type)
                  )
              ),
            ProvisionalTerminalExpression
              2
              spanValue
              ( ProvisionalBinaryExpression
                  "+"
                  literalType
                  literalType
                  (ProvisionalVariableExpression "seed" literalType)
                  (ProvisionalLiteralExpression (LInt 1) literalType)
              )
          ]
  assertProvisionalProductionCompletes "captured composite scalar specialization" provisionalScope

testCapturedCompositeScalarBinderSpecialization :: IO ()
testCapturedCompositeScalarBinderSpecialization = do
  let spanValue = SourceSpan 1 1
      seedType = TIntegerLiteralType (IntegerLiteralRange 1 1)
      otherType = TIntegerLiteralType (IntegerLiteralRange 2 2)
      uint8Type = TNumericType NumericUInt8
      functionType = TFunctionType uint8Type uint8Type
      loopDeclaration =
        ProvisionalCallableDeclaration
          2
          "loop"
          spanValue
          functionType
          (Just (PlainTypeBinding functionType))
          (Just [2])
      provisionalScope =
        ProvisionalScopeStatements
          [ ProvisionalScalarBinding
              0
              "seed"
              spanValue
              seedType
              (ProvisionalLiteralExpression (LInt 1) seedType),
            ProvisionalScalarBinding
              1
              "other"
              spanValue
              otherType
              (ProvisionalLiteralExpression (LInt 2) otherType),
            ProvisionalFunctionBinding
              loopDeclaration
              ( ProvisionalLambdaExpression
                  "item"
                  functionType
                  ( ProvisionalApplyExpression
                      uint8Type
                      (ProvisionalVariableExpression "loop" functionType)
                      (ProvisionalVariableExpression "seed" uint8Type)
                  )
              ),
            ProvisionalTerminalExpression
              3
              spanValue
              ( ProvisionalBinaryExpression
                  "+"
                  seedType
                  seedType
                  (ProvisionalVariableExpression "seed" seedType)
                  (ProvisionalVariableExpression "other" otherType)
              )
          ]
  assertProvisionalProductionCompletes "captured composite scalar binder specialization" provisionalScope

testCapturedComparisonResultSpecialization :: IO ()
testCapturedComparisonResultSpecialization = do
  let spanValue = SourceSpan 1 1
      seedType = TIntegerLiteralType (IntegerLiteralRange 1 1)
      otherType = TIntegerLiteralType (IntegerLiteralRange 2 2)
      comparisonOperandType = TIntegerLiteralType (IntegerLiteralRange 1 2)
      uint8Type = TNumericType NumericUInt8
      functionType = TFunctionType uint8Type uint8Type
      loopDeclaration =
        ProvisionalCallableDeclaration
          1
          "loop"
          spanValue
          functionType
          (Just (PlainTypeBinding functionType))
          (Just [1])
      provisionalScope =
        ProvisionalScopeStatements
          [ ProvisionalScalarBinding
              0
              "seed"
              spanValue
              seedType
              (ProvisionalLiteralExpression (LInt 1) seedType),
            ProvisionalFunctionBinding
              loopDeclaration
              ( ProvisionalLambdaExpression
                  "item"
                  functionType
                  ( ProvisionalApplyExpression
                      uint8Type
                      (ProvisionalVariableExpression "loop" functionType)
                      (ProvisionalVariableExpression "seed" uint8Type)
                  )
              ),
            ProvisionalScalarBinding
              2
              "flag"
              spanValue
              TBoolType
              ( ProvisionalBinaryExpression
                  "<"
                  TBoolType
                  comparisonOperandType
                  (ProvisionalVariableExpression "seed" seedType)
                  (ProvisionalLiteralExpression (LInt 2) otherType)
              ),
            ProvisionalTerminalExpression
              3
              spanValue
              (ProvisionalVariableExpression "flag" TBoolType)
          ]
  assertProvisionalProductionCompletes "captured comparison result specialization" provisionalScope

testCapturedFunctionBodySpecialization :: IO ()
testCapturedFunctionBodySpecialization = do
  let spanValue = SourceSpan 1 1
      literalType = TIntegerLiteralType (IntegerLiteralRange 1 1)
      uint8Type = TNumericType NumericUInt8
      recursiveFunctionType = TFunctionType uint8Type uint8Type
      helperFunctionType = TFunctionType literalType literalType
      loopDeclaration =
        ProvisionalCallableDeclaration
          1
          "loop"
          spanValue
          recursiveFunctionType
          (Just (PlainTypeBinding recursiveFunctionType))
          (Just [1])
      helperDeclaration =
        ProvisionalCallableDeclaration
          2
          "helper"
          spanValue
          helperFunctionType
          (Just (PlainTypeBinding helperFunctionType))
          Nothing
      provisionalScope =
        ProvisionalScopeStatements
          [ ProvisionalScalarBinding
              0
              "seed"
              spanValue
              literalType
              (ProvisionalLiteralExpression (LInt 1) literalType),
            ProvisionalFunctionBinding
              loopDeclaration
              ( ProvisionalLambdaExpression
                  "item"
                  recursiveFunctionType
                  ( ProvisionalApplyExpression
                      uint8Type
                      (ProvisionalVariableExpression "loop" recursiveFunctionType)
                      (ProvisionalVariableExpression "seed" uint8Type)
                  )
              ),
            ProvisionalFunctionBinding
              helperDeclaration
              ( ProvisionalLambdaExpression
                  "item"
                  helperFunctionType
                  ( ProvisionalBinaryExpression
                      "+"
                      literalType
                      literalType
                      (ProvisionalVariableExpression "seed" literalType)
                      (ProvisionalLiteralExpression (LInt 1) literalType)
                  )
              ),
            ProvisionalTerminalExpression
              3
              spanValue
              ( ProvisionalApplyExpression
                  literalType
                  (ProvisionalVariableExpression "helper" helperFunctionType)
                  (ProvisionalLiteralExpression (LInt 1) literalType)
              )
          ]
  assertProvisionalProductionCompletes "captured function body specialization" provisionalScope

testCapturedFunctionParameterSpecialization :: IO ()
testCapturedFunctionParameterSpecialization = do
  let spanValue = SourceSpan 1 1
      literalType = TIntegerLiteralType (IntegerLiteralRange 1 1)
      uint8Type = TNumericType NumericUInt8
      recursiveFunctionType = TFunctionType uint8Type uint8Type
      helperFunctionType = TFunctionType literalType literalType
      loopDeclaration =
        ProvisionalCallableDeclaration
          1
          "loop"
          spanValue
          recursiveFunctionType
          (Just (PlainTypeBinding recursiveFunctionType))
          (Just [1])
      helperDeclaration =
        ProvisionalCallableDeclaration
          2
          "helper"
          spanValue
          helperFunctionType
          (Just (PlainTypeBinding helperFunctionType))
          Nothing
      provisionalScope =
        ProvisionalScopeStatements
          [ ProvisionalScalarBinding
              0
              "seed"
              spanValue
              literalType
              (ProvisionalLiteralExpression (LInt 1) literalType),
            ProvisionalFunctionBinding
              loopDeclaration
              ( ProvisionalLambdaExpression
                  "item"
                  recursiveFunctionType
                  ( ProvisionalApplyExpression
                      uint8Type
                      (ProvisionalVariableExpression "loop" recursiveFunctionType)
                      (ProvisionalVariableExpression "seed" uint8Type)
                  )
              ),
            ProvisionalFunctionBinding
              helperDeclaration
              ( ProvisionalLambdaExpression
                  "item"
                  helperFunctionType
                  ( ProvisionalBinaryExpression
                      "+"
                      literalType
                      literalType
                      (ProvisionalVariableExpression "seed" literalType)
                      (ProvisionalVariableExpression "item" literalType)
                  )
              ),
            ProvisionalTerminalExpression
              3
              spanValue
              ( ProvisionalApplyExpression
                  literalType
                  (ProvisionalVariableExpression "helper" helperFunctionType)
                  (ProvisionalLiteralExpression (LInt 1) literalType)
              )
          ]
  assertProvisionalProductionTypes
    "captured function parameter specialization"
    [("helper", typedUInt8UnaryType)]
    Nothing
    provisionalScope

testCapturedCallableParameterApplicationSpecialization :: IO ()
testCapturedCallableParameterApplicationSpecialization = do
  let spanValue = SourceSpan 1 1
      literalType = TIntegerLiteralType (IntegerLiteralRange 1 1)
      uint8Type = TNumericType NumericUInt8
      recursiveFunctionType = TFunctionType uint8Type uint8Type
      callbackFunctionType = TFunctionType literalType literalType
      helperFunctionType = TFunctionType callbackFunctionType literalType
      loopDeclaration =
        ProvisionalCallableDeclaration
          1
          "loop"
          spanValue
          recursiveFunctionType
          (Just (PlainTypeBinding recursiveFunctionType))
          (Just [1])
      helperDeclaration =
        ProvisionalCallableDeclaration
          2
          "helper"
          spanValue
          helperFunctionType
          (Just (PlainTypeBinding helperFunctionType))
          Nothing
      provisionalScope =
        ProvisionalScopeStatements
          [ ProvisionalScalarBinding
              0
              "seed"
              spanValue
              literalType
              (ProvisionalLiteralExpression (LInt 1) literalType),
            ProvisionalFunctionBinding
              loopDeclaration
              ( ProvisionalLambdaExpression
                  "item"
                  recursiveFunctionType
                  ( ProvisionalApplyExpression
                      uint8Type
                      (ProvisionalVariableExpression "loop" recursiveFunctionType)
                      (ProvisionalVariableExpression "seed" uint8Type)
                  )
              ),
            ProvisionalFunctionBinding
              helperDeclaration
              ( ProvisionalLambdaExpression
                  "function"
                  helperFunctionType
                  ( ProvisionalBinaryExpression
                      "+"
                      literalType
                      literalType
                      ( ProvisionalApplyExpression
                          literalType
                          (ProvisionalVariableExpression "function" callbackFunctionType)
                          (ProvisionalVariableExpression "seed" literalType)
                      )
                      (ProvisionalVariableExpression "seed" literalType)
                  )
              ),
            ProvisionalTerminalExpression
              3
              spanValue
              (ProvisionalVariableExpression "helper" helperFunctionType)
          ]
  assertProvisionalProductionTypes
    "captured callable parameter application specialization"
    [("helper", typedUInt8HigherOrderType)]
    (Just typedUInt8HigherOrderType)
    provisionalScope

testCapturedFunctionScalarBinderSpecialization :: IO ()
testCapturedFunctionScalarBinderSpecialization = do
  let spanValue = SourceSpan 1 1
      literalType = TIntegerLiteralType (IntegerLiteralRange 1 1)
      uint8Type = TNumericType NumericUInt8
      recursiveFunctionType = TFunctionType uint8Type uint8Type
      helperFunctionType = TFunctionType literalType literalType
      loopDeclaration =
        ProvisionalCallableDeclaration
          2
          "loop"
          spanValue
          recursiveFunctionType
          (Just (PlainTypeBinding recursiveFunctionType))
          (Just [2])
      helperDeclaration =
        ProvisionalCallableDeclaration
          3
          "helper"
          spanValue
          helperFunctionType
          (Just (PlainTypeBinding helperFunctionType))
          Nothing
      provisionalScope =
        ProvisionalScopeStatements
          [ ProvisionalScalarBinding
              0
              "seed"
              spanValue
              literalType
              (ProvisionalLiteralExpression (LInt 1) literalType),
            ProvisionalScalarBinding
              1
              "other"
              spanValue
              literalType
              (ProvisionalLiteralExpression (LInt 2) literalType),
            ProvisionalFunctionBinding
              loopDeclaration
              ( ProvisionalLambdaExpression
                  "item"
                  recursiveFunctionType
                  ( ProvisionalApplyExpression
                      uint8Type
                      (ProvisionalVariableExpression "loop" recursiveFunctionType)
                      (ProvisionalVariableExpression "seed" uint8Type)
                  )
              ),
            ProvisionalFunctionBinding
              helperDeclaration
              ( ProvisionalLambdaExpression
                  "item"
                  helperFunctionType
                  ( ProvisionalBinaryExpression
                      "+"
                      literalType
                      literalType
                      (ProvisionalVariableExpression "seed" literalType)
                      (ProvisionalVariableExpression "other" literalType)
                  )
              ),
            ProvisionalTerminalExpression
              4
              spanValue
              ( ProvisionalApplyExpression
                  literalType
                  (ProvisionalVariableExpression "helper" helperFunctionType)
                  (ProvisionalLiteralExpression (LInt 1) literalType)
              )
          ]
  assertProvisionalProductionTypes
    "captured function scalar binder specialization"
    [("other", typedUInt8Type), ("helper", typedIntToUInt8Type)]
    Nothing
    provisionalScope

testCapturedFunctionArgumentScalarBinderSpecialization :: IO ()
testCapturedFunctionArgumentScalarBinderSpecialization = do
  let spanValue = SourceSpan 1 1
      literalType = TIntegerLiteralType (IntegerLiteralRange 1 1)
      otherType = TIntegerLiteralType (IntegerLiteralRange 2 2)
      uint8Type = TNumericType NumericUInt8
      recursiveFunctionType = TFunctionType uint8Type uint8Type
      helperFunctionType = TFunctionType literalType literalType
      loopDeclaration =
        ProvisionalCallableDeclaration
          2
          "loop"
          spanValue
          recursiveFunctionType
          (Just (PlainTypeBinding recursiveFunctionType))
          (Just [2])
      helperDeclaration =
        ProvisionalCallableDeclaration
          3
          "helper"
          spanValue
          helperFunctionType
          (Just (PlainTypeBinding helperFunctionType))
          Nothing
      provisionalScope =
        ProvisionalScopeStatements
          [ ProvisionalScalarBinding
              0
              "seed"
              spanValue
              literalType
              (ProvisionalLiteralExpression (LInt 1) literalType),
            ProvisionalScalarBinding
              1
              "other"
              spanValue
              otherType
              (ProvisionalLiteralExpression (LInt 2) otherType),
            ProvisionalFunctionBinding
              loopDeclaration
              ( ProvisionalLambdaExpression
                  "item"
                  recursiveFunctionType
                  ( ProvisionalApplyExpression
                      uint8Type
                      (ProvisionalVariableExpression "loop" recursiveFunctionType)
                      (ProvisionalVariableExpression "seed" uint8Type)
                  )
              ),
            ProvisionalFunctionBinding
              helperDeclaration
              ( ProvisionalLambdaExpression
                  "item"
                  helperFunctionType
                  ( ProvisionalBinaryExpression
                      "+"
                      literalType
                      literalType
                      (ProvisionalVariableExpression "seed" literalType)
                      (ProvisionalVariableExpression "item" literalType)
                  )
              ),
            ProvisionalTerminalExpression
              4
              spanValue
              ( ProvisionalApplyExpression
                  literalType
                  (ProvisionalVariableExpression "helper" helperFunctionType)
                  (ProvisionalVariableExpression "other" otherType)
              )
          ]
  assertProvisionalProductionTypes
    "captured function argument scalar binder specialization"
    [("other", typedUInt8Type), ("helper", typedUInt8UnaryType)]
    Nothing
    provisionalScope

testCapturedFunctionResultScalarBinderSpecialization :: IO ()
testCapturedFunctionResultScalarBinderSpecialization = do
  let spanValue = SourceSpan 1 1
      literalType = TIntegerLiteralType (IntegerLiteralRange 1 1)
      uint8Type = TNumericType NumericUInt8
      recursiveFunctionType = TFunctionType uint8Type uint8Type
      helperFunctionType = TFunctionType literalType literalType
      loopDeclaration =
        ProvisionalCallableDeclaration
          1
          "loop"
          spanValue
          recursiveFunctionType
          (Just (PlainTypeBinding recursiveFunctionType))
          (Just [1])
      helperDeclaration =
        ProvisionalCallableDeclaration
          2
          "helper"
          spanValue
          helperFunctionType
          (Just (PlainTypeBinding helperFunctionType))
          Nothing
      provisionalScope =
        ProvisionalScopeStatements
          [ ProvisionalScalarBinding
              0
              "seed"
              spanValue
              literalType
              (ProvisionalLiteralExpression (LInt 1) literalType),
            ProvisionalFunctionBinding
              loopDeclaration
              ( ProvisionalLambdaExpression
                  "item"
                  recursiveFunctionType
                  ( ProvisionalApplyExpression
                      uint8Type
                      (ProvisionalVariableExpression "loop" recursiveFunctionType)
                      (ProvisionalVariableExpression "seed" uint8Type)
                  )
              ),
            ProvisionalFunctionBinding
              helperDeclaration
              ( ProvisionalLambdaExpression
                  "item"
                  helperFunctionType
                  ( ProvisionalBinaryExpression
                      "+"
                      literalType
                      literalType
                      (ProvisionalVariableExpression "seed" literalType)
                      (ProvisionalVariableExpression "item" literalType)
                  )
              ),
            ProvisionalScalarBinding
              3
              "result"
              spanValue
              literalType
              ( ProvisionalApplyExpression
                  literalType
                  (ProvisionalVariableExpression "helper" helperFunctionType)
                  (ProvisionalLiteralExpression (LInt 1) literalType)
              ),
            ProvisionalTerminalExpression
              4
              spanValue
              (ProvisionalVariableExpression "result" literalType)
          ]
  assertProvisionalProductionTypes
    "captured function result scalar binder specialization"
    [("helper", typedUInt8UnaryType), ("result", typedUInt8Type)]
    Nothing
    provisionalScope

testCapturedHigherOrderCallableArgumentSpecialization :: IO ()
testCapturedHigherOrderCallableArgumentSpecialization = do
  let spanValue = SourceSpan 1 1
      literalType = TIntegerLiteralType (IntegerLiteralRange 1 1)
      uint8Type = TNumericType NumericUInt8
      recursiveFunctionType = TFunctionType uint8Type uint8Type
      helperFunctionType = TFunctionType literalType literalType
      applyFunctionType = TFunctionType helperFunctionType literalType
      loopDeclaration =
        ProvisionalCallableDeclaration
          1
          "loop"
          spanValue
          recursiveFunctionType
          (Just (PlainTypeBinding recursiveFunctionType))
          (Just [1])
      helperDeclaration =
        ProvisionalCallableDeclaration
          2
          "helper"
          spanValue
          helperFunctionType
          (Just (PlainTypeBinding helperFunctionType))
          Nothing
      applyDeclaration =
        ProvisionalCallableDeclaration
          3
          "apply"
          spanValue
          applyFunctionType
          (Just (PlainTypeBinding applyFunctionType))
          Nothing
      provisionalScope =
        ProvisionalScopeStatements
          [ ProvisionalScalarBinding
              0
              "seed"
              spanValue
              literalType
              (ProvisionalLiteralExpression (LInt 1) literalType),
            ProvisionalFunctionBinding
              loopDeclaration
              ( ProvisionalLambdaExpression
                  "item"
                  recursiveFunctionType
                  ( ProvisionalApplyExpression
                      uint8Type
                      (ProvisionalVariableExpression "loop" recursiveFunctionType)
                      (ProvisionalVariableExpression "seed" uint8Type)
                  )
              ),
            ProvisionalFunctionBinding
              helperDeclaration
              ( ProvisionalLambdaExpression
                  "item"
                  helperFunctionType
                  ( ProvisionalBinaryExpression
                      "+"
                      literalType
                      literalType
                      (ProvisionalVariableExpression "seed" literalType)
                      (ProvisionalVariableExpression "item" literalType)
                  )
              ),
            ProvisionalFunctionBinding
              applyDeclaration
              ( ProvisionalLambdaExpression
                  "function"
                  applyFunctionType
                  ( ProvisionalApplyExpression
                      literalType
                      (ProvisionalVariableExpression "function" helperFunctionType)
                      (ProvisionalLiteralExpression (LInt 1) literalType)
                  )
              ),
            ProvisionalTerminalExpression
              4
              spanValue
              ( ProvisionalApplyExpression
                  literalType
                  (ProvisionalVariableExpression "apply" applyFunctionType)
                  (ProvisionalVariableExpression "helper" helperFunctionType)
              )
          ]
  assertProvisionalProductionTypes
    "captured higher-order callable argument specialization"
    [("helper", typedUInt8UnaryType), ("apply", typedUInt8HigherOrderType)]
    Nothing
    provisionalScope

testCapturedForwardedHigherOrderCallableArgumentSpecialization :: IO ()
testCapturedForwardedHigherOrderCallableArgumentSpecialization = do
  let spanValue = SourceSpan 1 1
      literalType = TIntegerLiteralType (IntegerLiteralRange 1 1)
      uint8Type = TNumericType NumericUInt8
      recursiveFunctionType = TFunctionType uint8Type uint8Type
      helperFunctionType = TFunctionType literalType literalType
      applyFunctionType = TFunctionType helperFunctionType literalType
      forwardFunctionType = TFunctionType helperFunctionType literalType
      loopDeclaration =
        ProvisionalCallableDeclaration
          1
          "loop"
          spanValue
          recursiveFunctionType
          (Just (PlainTypeBinding recursiveFunctionType))
          (Just [1])
      helperDeclaration =
        ProvisionalCallableDeclaration
          2
          "helper"
          spanValue
          helperFunctionType
          (Just (PlainTypeBinding helperFunctionType))
          Nothing
      applyDeclaration =
        ProvisionalCallableDeclaration
          3
          "apply"
          spanValue
          applyFunctionType
          (Just (PlainTypeBinding applyFunctionType))
          Nothing
      forwardDeclaration =
        ProvisionalCallableDeclaration
          4
          "forward"
          spanValue
          forwardFunctionType
          (Just (PlainTypeBinding forwardFunctionType))
          Nothing
      provisionalScope =
        ProvisionalScopeStatements
          [ ProvisionalScalarBinding
              0
              "seed"
              spanValue
              literalType
              (ProvisionalLiteralExpression (LInt 1) literalType),
            ProvisionalFunctionBinding
              loopDeclaration
              ( ProvisionalLambdaExpression
                  "item"
                  recursiveFunctionType
                  ( ProvisionalApplyExpression
                      uint8Type
                      (ProvisionalVariableExpression "loop" recursiveFunctionType)
                      (ProvisionalVariableExpression "seed" uint8Type)
                  )
              ),
            ProvisionalFunctionBinding
              helperDeclaration
              ( ProvisionalLambdaExpression
                  "item"
                  helperFunctionType
                  ( ProvisionalBinaryExpression
                      "+"
                      literalType
                      literalType
                      (ProvisionalVariableExpression "seed" literalType)
                      (ProvisionalVariableExpression "item" literalType)
                  )
              ),
            ProvisionalFunctionBinding
              applyDeclaration
              ( ProvisionalLambdaExpression
                  "function"
                  applyFunctionType
                  ( ProvisionalApplyExpression
                      literalType
                      (ProvisionalVariableExpression "function" helperFunctionType)
                      (ProvisionalLiteralExpression (LInt 1) literalType)
                  )
              ),
            ProvisionalFunctionBinding
              forwardDeclaration
              ( ProvisionalLambdaExpression
                  "function"
                  forwardFunctionType
                  ( ProvisionalApplyExpression
                      literalType
                      (ProvisionalVariableExpression "apply" applyFunctionType)
                      (ProvisionalVariableExpression "function" helperFunctionType)
                  )
              ),
            ProvisionalTerminalExpression
              5
              spanValue
              ( ProvisionalApplyExpression
                  literalType
                  (ProvisionalVariableExpression "forward" forwardFunctionType)
                  (ProvisionalVariableExpression "helper" helperFunctionType)
              )
          ]
  assertProvisionalProductionTypes
    "captured forwarded higher-order callable argument specialization"
    [ ("helper", typedUInt8UnaryType),
      ("apply", typedUInt8HigherOrderType),
      ("forward", typedUInt8HigherOrderType)
    ]
    Nothing
    provisionalScope

testCapturedTerminalAnonymousCallableSpecialization :: IO ()
testCapturedTerminalAnonymousCallableSpecialization = do
  let spanValue = SourceSpan 1 1
      literalType = TIntegerLiteralType (IntegerLiteralRange 1 1)
      uint8Type = TNumericType NumericUInt8
      recursiveFunctionType = TFunctionType uint8Type uint8Type
      anonymousFunctionType = TFunctionType literalType literalType
      loopDeclaration =
        ProvisionalCallableDeclaration
          1
          "loop"
          spanValue
          recursiveFunctionType
          (Just (PlainTypeBinding recursiveFunctionType))
          (Just [1])
      provisionalScope =
        ProvisionalScopeStatements
          [ ProvisionalScalarBinding
              0
              "seed"
              spanValue
              literalType
              (ProvisionalLiteralExpression (LInt 1) literalType),
            ProvisionalFunctionBinding
              loopDeclaration
              ( ProvisionalLambdaExpression
                  "item"
                  recursiveFunctionType
                  ( ProvisionalApplyExpression
                      uint8Type
                      (ProvisionalVariableExpression "loop" recursiveFunctionType)
                      (ProvisionalVariableExpression "seed" uint8Type)
                  )
              ),
            ProvisionalTerminalExpression
              2
              spanValue
              ( ProvisionalLambdaExpression
                  "item"
                  anonymousFunctionType
                  ( ProvisionalBinaryExpression
                      "+"
                      literalType
                      literalType
                      (ProvisionalVariableExpression "seed" literalType)
                      (ProvisionalVariableExpression "item" literalType)
                  )
              )
          ]
  assertProvisionalProductionTypes
    "captured terminal anonymous callable specialization"
    []
    (Just typedUInt8UnaryType)
    provisionalScope

testCapturedNamedCallerSpecialization :: IO ()
testCapturedNamedCallerSpecialization = do
  let spanValue = SourceSpan 1 1
      literalType = TIntegerLiteralType (IntegerLiteralRange 1 1)
      uint8Type = TNumericType NumericUInt8
      recursiveFunctionType = TFunctionType uint8Type uint8Type
      helperFunctionType = TFunctionType literalType literalType
      consumerFunctionType = TFunctionType literalType literalType
      loopDeclaration =
        ProvisionalCallableDeclaration
          1
          "loop"
          spanValue
          recursiveFunctionType
          (Just (PlainTypeBinding recursiveFunctionType))
          (Just [1])
      helperDeclaration =
        ProvisionalCallableDeclaration
          2
          "helper"
          spanValue
          helperFunctionType
          (Just (PlainTypeBinding helperFunctionType))
          Nothing
      consumerDeclaration =
        ProvisionalCallableDeclaration
          3
          "consumer"
          spanValue
          consumerFunctionType
          (Just (PlainTypeBinding consumerFunctionType))
          Nothing
      provisionalScope =
        ProvisionalScopeStatements
          [ ProvisionalScalarBinding
              0
              "seed"
              spanValue
              literalType
              (ProvisionalLiteralExpression (LInt 1) literalType),
            ProvisionalFunctionBinding
              loopDeclaration
              ( ProvisionalLambdaExpression
                  "item"
                  recursiveFunctionType
                  ( ProvisionalApplyExpression
                      uint8Type
                      (ProvisionalVariableExpression "loop" recursiveFunctionType)
                      (ProvisionalVariableExpression "seed" uint8Type)
                  )
              ),
            ProvisionalFunctionBinding
              helperDeclaration
              ( ProvisionalLambdaExpression
                  "item"
                  helperFunctionType
                  ( ProvisionalBinaryExpression
                      "+"
                      literalType
                      literalType
                      (ProvisionalVariableExpression "seed" literalType)
                      (ProvisionalVariableExpression "item" literalType)
                  )
              ),
            ProvisionalFunctionBinding
              consumerDeclaration
              ( ProvisionalLambdaExpression
                  "item"
                  consumerFunctionType
                  ( ProvisionalApplyExpression
                      literalType
                      (ProvisionalVariableExpression "helper" helperFunctionType)
                      (ProvisionalVariableExpression "item" literalType)
                  )
              ),
            ProvisionalTerminalExpression
              4
              spanValue
              ( ProvisionalApplyExpression
                  literalType
                  (ProvisionalVariableExpression "consumer" consumerFunctionType)
                  (ProvisionalLiteralExpression (LInt 1) literalType)
              )
          ]
  assertProvisionalProductionTypes
    "captured named caller specialization"
    [("helper", typedUInt8UnaryType), ("consumer", typedUInt8UnaryType)]
    Nothing
    provisionalScope

testCapturedScalarAliasSourceSpecialization :: IO ()
testCapturedScalarAliasSourceSpecialization = do
  let spanValue = SourceSpan 1 1
      literalType = TIntegerLiteralType (IntegerLiteralRange 1 1)
      uint8Type = TNumericType NumericUInt8
      functionType = TFunctionType uint8Type uint8Type
      loopDeclaration =
        ProvisionalCallableDeclaration
          2
          "loop"
          spanValue
          functionType
          (Just (PlainTypeBinding functionType))
          (Just [2])
      provisionalScope =
        ProvisionalScopeStatements
          [ ProvisionalScalarBinding
              0
              "seed"
              spanValue
              literalType
              (ProvisionalLiteralExpression (LInt 1) literalType),
            ProvisionalScalarBinding
              1
              "copy"
              spanValue
              literalType
              (ProvisionalVariableExpression "seed" literalType),
            ProvisionalFunctionBinding
              loopDeclaration
              ( ProvisionalLambdaExpression
                  "item"
                  functionType
                  ( ProvisionalApplyExpression
                      uint8Type
                      (ProvisionalVariableExpression "loop" functionType)
                      (ProvisionalVariableExpression "copy" uint8Type)
                  )
              ),
            ProvisionalTerminalExpression
              3
              spanValue
              ( ProvisionalApplyExpression
                  uint8Type
                  (ProvisionalVariableExpression "loop" functionType)
                  (ProvisionalLiteralExpression (LInt 1) uint8Type)
              )
          ]
  assertProvisionalProductionCompletes "captured scalar alias source specialization" provisionalScope

testRecordedScalarStatementIndices :: IO ()
testRecordedScalarStatementIndices = do
  let spanValue = SourceSpan 1 1
      literalType = TIntegerLiteralType (IntegerLiteralRange 1 1)
      uint8Type = TNumericType NumericUInt8
      functionType = TFunctionType uint8Type uint8Type
      loopDeclaration =
        ProvisionalCallableDeclaration
          3
          "loop"
          spanValue
          functionType
          (Just (PlainTypeBinding functionType))
          (Just [3])
      provisionalScope =
        ProvisionalScopeStatements
          [ ProvisionalScalarBinding
              1
              "seed"
              spanValue
              literalType
              (ProvisionalLiteralExpression (LInt 1) literalType),
            ProvisionalScalarBinding
              2
              "copy"
              spanValue
              literalType
              (ProvisionalVariableExpression "seed" literalType),
            ProvisionalFunctionBinding
              loopDeclaration
              ( ProvisionalLambdaExpression
                  "item"
                  functionType
                  ( ProvisionalApplyExpression
                      uint8Type
                      (ProvisionalVariableExpression "loop" functionType)
                      (ProvisionalVariableExpression "copy" uint8Type)
                  )
              ),
            ProvisionalTerminalExpression
              4
              spanValue
              ( ProvisionalApplyExpression
                  uint8Type
                  (ProvisionalVariableExpression "loop" functionType)
                  (ProvisionalLiteralExpression (LInt 1) uint8Type)
              )
          ]
  assertProvisionalProductionCompletes "recorded scalar statement indices" provisionalScope

testEagerRecursiveClosureCaptureAvailability :: IO ()
testEagerRecursiveClosureCaptureAvailability = do
  resolvedModule <- resolveFixtureModule (fixtureByName "unit-entry")
  let spanValue = SourceSpan 1 1
      literalType = TIntegerLiteralType (IntegerLiteralRange 1 1)
      uint8Type = TNumericType NumericUInt8
      functionType = TFunctionType uint8Type uint8Type
      loopDeclaration =
        ProvisionalCallableDeclaration
          3
          "loop"
          spanValue
          functionType
          (Just (PlainTypeBinding functionType))
          (Just [3])
      provisionalScope =
        ProvisionalScopeStatements
          [ ProvisionalScalarBinding
              0
              "result"
              spanValue
              uint8Type
              ( ProvisionalApplyExpression
                  uint8Type
                  (ProvisionalVariableExpression "loop" functionType)
                  (ProvisionalLiteralExpression (LInt 1) uint8Type)
              ),
            ProvisionalScalarBinding
              1
              "seed"
              spanValue
              literalType
              (ProvisionalLiteralExpression (LInt 1) literalType),
            ProvisionalSignature 2 "loop" spanValue functionType,
            ProvisionalFunctionBinding
              loopDeclaration
              ( ProvisionalLambdaExpression
                  "item"
                  functionType
                  ( ProvisionalApplyExpression
                      uint8Type
                      (ProvisionalVariableExpression "loop" functionType)
                      (ProvisionalVariableExpression "seed" uint8Type)
                  )
              ),
            ProvisionalTerminalExpression
              4
              spanValue
              ProvisionalUnitExpression
          ]
      expected =
        TypedCoreProductionUnsupported
          [ TypedCoreProductionFailure
              (TypedCoreProductionExpressionPath ["App", "Main"] 0 [0])
              TypedCoreCaptureUnsupported
              (TypedCoreNameDetail "loop")
          ]
      status =
        typedCoreProductionOutcomeStatus
          ( finalizeValidatedTypedCoreExpressionDirectCall
              (TypedSourcePath "src/App/Main.jz")
              resolvedModule
              initialInferState
              provisionalScope
          )
  assertEqual "eager recursive closure capture rejection" expected status

testEagerNestedClosureCaptureAvailability :: IO ()
testEagerNestedClosureCaptureAvailability = do
  resolvedModule <- resolveFixtureModule (fixtureByName "unit-entry")
  let spanValue = SourceSpan 1 1
      uint8Type = TNumericType NumericUInt8
      callbackType = TFunctionType uint8Type uint8Type
      invokeType = TFunctionType callbackType uint8Type
      invokeDeclaration =
        ProvisionalCallableDeclaration
          0
          "invoke"
          spanValue
          invokeType
          (Just (PlainTypeBinding invokeType))
          Nothing
      loopDeclaration =
        ProvisionalCallableDeclaration
          4
          "loop"
          spanValue
          callbackType
          (Just (PlainTypeBinding callbackType))
          (Just [4])
      provisionalScope =
        ProvisionalScopeStatements
          [ ProvisionalFunctionBinding
              invokeDeclaration
              ( ProvisionalLambdaExpression
                  "callback"
                  invokeType
                  ( ProvisionalApplyExpression
                      uint8Type
                      (ProvisionalVariableExpression "callback" callbackType)
                      (ProvisionalLiteralExpression (LInt 1) uint8Type)
                  )
              ),
            ProvisionalScalarBinding
              1
              "result"
              spanValue
              uint8Type
              ( ProvisionalApplyExpression
                  uint8Type
                  (ProvisionalVariableExpression "invoke" invokeType)
                  ( ProvisionalLambdaExpression
                      "item"
                      callbackType
                      ( ProvisionalApplyExpression
                          uint8Type
                          (ProvisionalVariableExpression "loop" callbackType)
                          (ProvisionalVariableExpression "item" uint8Type)
                      )
                  )
              ),
            ProvisionalScalarBinding
              2
              "seed"
              spanValue
              uint8Type
              (ProvisionalLiteralExpression (LInt 1) uint8Type),
            ProvisionalSignature 3 "loop" spanValue callbackType,
            ProvisionalFunctionBinding
              loopDeclaration
              ( ProvisionalLambdaExpression
                  "item"
                  callbackType
                  ( ProvisionalApplyExpression
                      uint8Type
                      (ProvisionalVariableExpression "loop" callbackType)
                      (ProvisionalVariableExpression "seed" uint8Type)
                  )
              ),
            ProvisionalTerminalExpression
              5
              spanValue
              ProvisionalUnitExpression
          ]
      expected =
        TypedCoreProductionUnsupported
          [ TypedCoreProductionFailure
              (TypedCoreProductionExpressionPath ["App", "Main"] 1 [0, 1])
              TypedCoreCaptureUnsupported
              (TypedCoreNameDetail "loop")
          ]
      status =
        typedCoreProductionOutcomeStatus
          ( finalizeValidatedTypedCoreExpressionDirectCall
              (TypedSourcePath "src/App/Main.jz")
              resolvedModule
              initialInferState
              provisionalScope
          )
  assertEqual "eager nested closure capture rejection" expected status

assertProvisionalProductionCompletes :: Text -> ProvisionalTypedExpr -> IO ()
assertProvisionalProductionCompletes label =
  assertProvisionalProductionTypes label [] Nothing

assertProvisionalProductionTypes :: Text -> [(Text, TypedType)] -> Maybe TypedType -> ProvisionalTypedExpr -> IO ()
assertProvisionalProductionTypes label expectedBindingTypes expectedTerminalType provisionalScope = do
  resolvedModule <- resolveFixtureModule (fixtureByName "unit-entry")
  let status =
        typedCoreProductionOutcomeStatus
          ( finalizeValidatedTypedCoreExpressionDirectCall
              (TypedSourcePath "src/App/Main.jz")
              resolvedModule
              initialInferState
              provisionalScope
          )
  case status of
    TypedCoreProductionSucceeded programValue -> do
      assertEqual (label <> " typed-core validation") [] (validateTypedProgram programValue)
      case programValue of
        TypedProgram _ [TypedModule _ _ _ _ _ _ statements _] _ -> do
          let bindingTypes =
                Map.fromList
                  [ (identifier, typeValue)
                  | TypedLetStatement
                      _
                      (TypedResolvedName TypedCurrentModule TypedValueNamespace identifier)
                      _
                      (TypedScheme _ _ _ _ typeValue _ _)
                      _ <-
                      statements
                  ]
              selectedBindingTypes =
                [ (identifier, Map.lookup identifier bindingTypes)
                | (identifier, _) <- expectedBindingTypes
                ]
          assertEqual
            (label <> " specialized binding types")
            [(identifier, Just typeValue) | (identifier, typeValue) <- expectedBindingTypes]
            selectedBindingTypes
          case expectedTerminalType of
            Just expectedType ->
              case reverse statements of
                TypedExpressionStatement _ expression : _ ->
                  assertEqual
                    (label <> " specialized terminal type")
                    expectedType
                    (typedNodeType (typedExpressionInfo expression))
                _ -> failTest (label <> " typed program has no terminal expression")
            Nothing -> pure ()
        _ -> failTest (label <> " typed program has an unexpected module shape")
      case lowerTypedCoreExpressionDirectCall programValue of
        LoweredIRSucceeded loweredProgram ->
          assertEqual (label <> " lowered-IR validation") [] (validateLoweredProgram loweredProgram)
        other -> failTest (label <> " did not lower: " <> Text.pack (show other))
    other -> failTest (label <> " did not produce typed core: " <> Text.pack (show other))

typedUInt8Type :: TypedType
typedUInt8Type = TypedNumericType TypedUInt8Type

typedUInt8UnaryType :: TypedType
typedUInt8UnaryType = TypedFunctionType typedUInt8Type typedUInt8Type

typedIntToUInt8Type :: TypedType
typedIntToUInt8Type = TypedFunctionType TypedIntType typedUInt8Type

typedUInt8HigherOrderType :: TypedType
typedUInt8HigherOrderType = TypedFunctionType typedUInt8UnaryType typedUInt8Type

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

testAnonymousLambdaResultAcceptance :: IO ()
testAnonymousLambdaResultAcceptance = do
  let fixture = producerEdgeFixture "anonymous-lambda-result"
      expected = lookup "anonymous-lambda-result" lexicalCaptureExpectedPrograms
  ordinary <- inferFixture fixture
  firstRun <- produceFixture fixture
  secondRun <- produceFixture fixture
  assertEqual
    "anonymous lambda result inference compatibility"
    ordinary
    (typedCoreProductionInferenceResult firstRun)
  assertEqual "anonymous lambda result repeatable production" firstRun secondRun
  case expected of
    Just expectedProgram ->
      assertEqual "anonymous lambda module result acceptance" (TypedCoreProductionSucceeded expectedProgram) (typedCoreProductionStatus firstRun)
    Nothing -> failTest "anonymous lambda result expectation is missing"

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
  assertEqual
    "blocked production does not retain a validation proof"
    Nothing
    (typedCoreProductionValidatedProgram firstRun)

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
        TypedCoreUserDefinedOperatorUnsupported,
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
  [ "oversaturated-direct-call",
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
        [expressionFailure 1 [] TypedCoreStructuredValueUnsupported TypedCoreListValueDetail]
    ),
    ("list-value", unsupported [expressionFailure 0 [] TypedCoreStructuredValueUnsupported TypedCoreListValueDetail]),
    ("non-unit-tuple", unsupported [expressionFailure 0 [] TypedCoreStructuredValueUnsupported TypedCoreTupleValueDetail]),
    ( "data-value",
      unsupported
        [ statementFailure 0 TypedCoreStructuredValueUnsupported TypedCoreDataValueDetail,
          expressionFailure 1 [] TypedCoreStructuredValueUnsupported TypedCoreDataValueDetail
        ]
    ),
    ("local-block-binding", unsupported [expressionFailure 0 [] TypedCoreNestedBlockUnsupported TypedCoreLocalBlockDetail]),
    ( "oversaturated-direct-call",
      unsupported
        [expressionFailure 1 [0, 0] TypedCoreUserDefinedOperatorUnsupported TypedCoreUnsupportedRootDetail]
    ),
    ( "later-capture-mutual-recursion",
      unsupported
        [statementFailure 4 TypedCoreRecursiveFunctionUnsupported (TypedCoreNameDetail "right")]
    ),
    ( "transitive-later-capture-mutual-recursion",
      unsupported
        [statementFailure 6 TypedCoreRecursiveFunctionUnsupported (TypedCoreNameDetail "right")]
    ),
    ( "interleaved-rebound-capture-mutual-recursion",
      unsupported
        [statementFailure 5 TypedCoreRecursiveFunctionUnsupported (TypedCoreNameDetail "right")]
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
              TypedCoreUnsupportedRootDetail
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
              (TypedCoreNameDetail "alpha")
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
          [ expressionFailure [1] TypedCoreStructuredValueUnsupported TypedCoreListValueDetail,
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
          [ expressionFailure 0 [0] TypedCoreStructuredValueUnsupported TypedCoreListValueDetail,
            expressionFailure 0 [1, 1] TypedCoreStructuredValueUnsupported TypedCoreListValueDetail,
            expressionFailure 0 [] TypedCorePatternCaseUnsupported TypedCorePatternCaseDetail
          ]
        ),
        ( "nested-block-unsupported-child",
          [ expressionFailure 0 [] TypedCoreNestedBlockUnsupported TypedCoreLocalBlockDetail,
            expressionFailure 0 [0] TypedCoreStructuredValueUnsupported TypedCoreListValueDetail,
            expressionFailure 0 [1] TypedCoreStructuredValueUnsupported TypedCoreListValueDetail
          ]
        ),
        ( "guarded-pattern-case-unsupported-children",
          [ expressionFailure 0 [0] TypedCoreStructuredValueUnsupported TypedCoreListValueDetail,
            expressionFailure 0 [1, 0] TypedCoreNestedBlockUnsupported TypedCoreLocalBlockDetail,
            expressionFailure 0 [1, 0, 0] TypedCoreStructuredValueUnsupported TypedCoreListValueDetail,
            expressionFailure 0 [1, 1] TypedCoreStructuredValueUnsupported TypedCoreListValueDetail,
            expressionFailure 0 [2, 1] TypedCoreStructuredValueUnsupported TypedCoreListValueDetail,
            expressionFailure 0 [] TypedCorePatternCaseUnsupported TypedCorePatternCaseDetail
          ]
        ),
        ( "unsupported-binary-child",
          [ statementFailure 1 TypedCoreUserDefinedOperatorUnsupported TypedCoreUnsupportedRootDetail,
            expressionFailure 2 [] TypedCoreUserDefinedOperatorUnsupported TypedCoreUnsupportedRootDetail
          ]
        ),
        ( "left-section-unsupported-child",
          [expressionFailure 0 [] TypedCoreUserDefinedOperatorUnsupported TypedCoreUnsupportedRootDetail]
        ),
        ( "right-section-unsupported-child",
          [expressionFailure 0 [] TypedCoreUserDefinedOperatorUnsupported TypedCoreUnsupportedRootDetail]
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
          [parameterFailure [0, 0]]
        ),
        ( "curried-shadowed-parameter",
          [parameterFailure [0, 0]]
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

testIncompleteRecursiveGroupOwnership :: IO ()
testIncompleteRecursiveGroupOwnership = do
  resolvedModule <- resolveFixtureModule (fixtureByName "unit-entry")
  let spanValue = SourceSpan 1 1
      functionType = TFunctionType TBoolType TBoolType
      loopDeclaration =
        ProvisionalCallableDeclaration
          1
          "loop"
          spanValue
          functionType
          (Just (PlainTypeBinding functionType))
          (Just [1, 3])
      loopExpression =
        ProvisionalLambdaExpression
          "item"
          functionType
          ( ProvisionalApplyExpression
              TBoolType
              (ProvisionalVariableExpression "loop" functionType)
              (ProvisionalVariableExpression "item" TBoolType)
          )
      provisionalScope =
        ProvisionalScopeStatements
          [ ProvisionalFunctionBinding loopDeclaration loopExpression,
            ProvisionalTerminalExpression 2 spanValue (ProvisionalLiteralExpression (LBool True) TBoolType)
          ]
      status =
        typedCoreProductionOutcomeStatus
          ( finalizeValidatedTypedCoreExpressionDirectCall
              (TypedSourcePath "src/App/Main.jz")
              resolvedModule
              initialInferState
              provisionalScope
          )
  assertEqual
    "missing recursive declaration owner rejects the complete group"
    ( TypedCoreProductionUnsupported
        [ TypedCoreProductionFailure
            (TypedCoreProductionStatementPath ["App", "Main"] 1)
            TypedCoreRecursiveFunctionUnsupported
            (TypedCoreNameDetail "loop")
        ]
    )
    status

testSameStatementFailureKindOrder :: IO ()
testSameStatementFailureKindOrder = do
  let fixture = producerEdgeFixture "self-recursive-function-rebinding"
      expected =
        TypedCoreProductionUnsupported
          [ TypedCoreProductionFailure
              (TypedCoreProductionStatementPath ["App", "Main"] 3)
              TypedCoreFunctionRebindingUnsupported
              (TypedCoreNameDetail "loop")
          ]
  firstRun <- produceFixture fixture
  secondRun <- produceFixture fixture
  assertEqual "self-recursive rebinding ordinary diagnostics" [] (filter isErrorDiagnostic (inferredDiagnostics (typedCoreProductionInferenceResult firstRun)))
  assertEqual "self-recursive rebinding repeatability" firstRun secondRun
  assertEqual "same-statement failures follow declared kind order" expected (typedCoreProductionStatus firstRun)

testInterveningScalarCanonicalOwnership :: Text -> IO ()
testInterveningScalarCanonicalOwnership requestedName =
  case lookup requestedName expectedResults of
    Just expectedFailures -> assertExact requestedName expectedFailures
    Nothing -> error "intervening scalar canonical ownership fixture has no expected result"
  where
    expectedResults =
      [ ( "intervening-scalar-canonical-ownership",
          [rootFailure 2]
        ),
        ( "multiple-intervening-scalars-canonical-ownership",
          [ rootFailure 2,
            rootFailure 3
          ]
        ),
        ( "interleaved-callable-scalar-canonical-ownership",
          [ rootFailure 2,
            rebindingFailure 4 "a",
            rootFailure 5
          ]
        )
      ]
    assertExact name expectedFailures = do
      let fixture = producerEdgeFixture name
      firstRun <- produceFixture fixture
      secondRun <- produceFixture fixture
      assertEqual
        (name <> " ordinary diagnostics")
        []
        ( filter
            isErrorDiagnostic
            (inferredDiagnostics (typedCoreProductionInferenceResult firstRun))
        )
      assertEqual (name <> " repeatability") firstRun secondRun
      assertEqual
        (name <> " exact canonical scalar-owner result")
        (TypedCoreProductionUnsupported expectedFailures)
        (typedCoreProductionStatus firstRun)
    rootFailure statementIndex =
      TypedCoreProductionFailure
        (TypedCoreProductionStatementPath ["App", "Main"] statementIndex)
        TypedCoreUnsupportedRootExpression
        TypedCoreUnsupportedRootDetail
    rebindingFailure statementIndex name =
      TypedCoreProductionFailure
        (TypedCoreProductionStatementPath ["App", "Main"] statementIndex)
        TypedCoreFunctionRebindingUnsupported
        (TypedCoreNameDetail name)

testCanonicalRecursionTransportControls :: IO ()
testCanonicalRecursionTransportControls =
  mapM_
    assertOwners
    [ ("self-recursive-function-rebinding", []),
      ("three-same-name-nearest-prior-mutual-recursion", [(5, "identity"), (7, "peer")]),
      ("canonical-self-recursion-no-prior", []),
      ("canonical-mutual-recursion-peers", []),
      ("rejected-self-alias-recursion", [(1, "loop")]),
      ("rejected-mutual-alias-recursion", [(1, "left"), (3, "right")]),
      ("rejected-alias-conditional-mutual-recursion", [(1, "left"), (3, "right")]),
      ("rejected-operator-alias-self-recursion", [(1, "$operator:%25%25")]),
      ("rejected-conditional-self-recursion", []),
      ("rejected-block-conditional-mutual-recursion", [(1, "left"), (3, "right")]),
      ("rejected-block-later-shadow-control", [(1, "loop")]),
      ("rejected-block-initializer-self-recursion", [(1, "loop")]),
      ("rejected-block-initializer-mutual-recursion", [(1, "left"), (3, "right")]),
      ("rejected-block-later-signed-shadow-control", [(1, "loop")]),
      ("rejected-operator-value-self-recursion", [(1, "$operator:%25%25")]),
      ("rejected-infix-operator-mutual-recursion", [(1, "$operator:%25%25"), (3, "$operator:%7E%7E")]),
      ("rejected-section-operator-mutual-recursion", [(1, "$operator:%25%25"), (3, "$operator:%7E%7E")])
    ]
  where
    assertOwners (name, expectedOwners) = do
      let fixture = producerEdgeFixture name
      firstRun <- produceFixture fixture
      secondRun <- produceFixture fixture
      assertEqual
        (name <> " canonical transport ordinary diagnostics")
        []
        ( filter
            isErrorDiagnostic
            (inferredDiagnostics (typedCoreProductionInferenceResult firstRun))
        )
      assertEqual (name <> " canonical transport repeatability") firstRun secondRun
      assertEqual
        (name <> " exact recursive owner order and multiplicity")
        expectedOwners
        (recursiveOwners (typedCoreProductionStatus firstRun))
    recursiveOwners status =
      case status of
        TypedCoreProductionUnsupported failures ->
          [ (statementIndex, name)
          | TypedCoreProductionFailure
              (TypedCoreProductionStatementPath _ statementIndex)
              TypedCoreRecursiveFunctionUnsupported
              (TypedCoreNameDetail name) <-
              failures
          ]
        _ -> []

testNestedPriorOuterAliasOwnership :: Text -> IO ()
testNestedPriorOuterAliasOwnership requestedName = do
  let fixture = producerEdgeFixture requestedName
  firstRun <- produceFixture fixture
  secondRun <- produceFixture fixture
  assertEqual
    (requestedName <> " ordinary diagnostics")
    []
    ( filter
        isErrorDiagnostic
        (inferredDiagnostics (typedCoreProductionInferenceResult firstRun))
    )
  assertEqual (requestedName <> " repeatability") firstRun secondRun
  assertEqual
    (requestedName <> " exact prior-outer recursive owner order and multiplicity")
    [(1, "left"), (3, "right")]
    (recursiveOwners (typedCoreProductionStatus firstRun))
  where
    recursiveOwners status =
      case status of
        TypedCoreProductionUnsupported failures ->
          [ (statementIndex, name)
          | TypedCoreProductionFailure
              (TypedCoreProductionStatementPath _ statementIndex)
              TypedCoreRecursiveFunctionUnsupported
              (TypedCoreNameDetail name) <-
              failures
          ]
        _ -> []

testNestedSelfRecursiveLambdaOwnership :: IO ()
testNestedSelfRecursiveLambdaOwnership = do
  let fixture = producerEdgeFixture "nested-self-recursive-lambda-local-ownership"
      expected =
        TypedCoreProductionUnsupported
          [ TypedCoreProductionFailure
              (TypedCoreProductionExpressionPath ["App", "Main"] 1 [0, 0])
              TypedCoreNestedBlockUnsupported
              TypedCoreLocalBlockDetail
          ]
  firstRun <- produceFixture fixture
  secondRun <- produceFixture fixture
  assertEqual
    "nested self-recursive lambda ordinary diagnostics"
    []
    ( filter
        isErrorDiagnostic
        (inferredDiagnostics (typedCoreProductionInferenceResult firstRun))
    )
  assertEqual "nested self-recursive lambda repeatability" firstRun secondRun
  assertEqual "nested self-recursive lambda exact local ownership" expected (typedCoreProductionStatus firstRun)

testNestedLambdaRecursiveAdmission :: IO ()
testNestedLambdaRecursiveAdmission = do
  let fixture = producerEdgeFixture "nested-lambda-direct-recursion"
      expected =
        TypedCoreProductionUnsupported
          [ TypedCoreProductionFailure
              (TypedCoreProductionStatementPath ["App", "Main"] 3)
              TypedCoreRecursiveFunctionUnsupported
              (TypedCoreNameDetail "loop")
          ]
  ordinary <- inferFixture fixture
  firstRun <- produceFixture fixture
  secondRun <- produceFixture fixture
  assertEqual
    "nested-lambda direct recursion ordinary diagnostics"
    []
    (filter isErrorDiagnostic (inferredDiagnostics ordinary))
  assertEqual
    "nested-lambda direct recursion inference compatibility"
    ordinary
    (typedCoreProductionInferenceResult firstRun)
  assertEqual "nested-lambda direct recursion repeatability" firstRun secondRun
  assertEqual "nested-lambda direct recursion producer rejection" expected (typedCoreProductionStatus firstRun)

testRejectedCallableRebinding :: Text -> IO ()
testRejectedCallableRebinding requestedName =
  case lookup requestedName expectedResults of
    Just expectedFailures -> assertExact requestedName expectedFailures
    Nothing -> error "rejected callable rebinding fixture has no expected result"
  where
    expectedResults =
      [ ( "accepted-then-rejected-callable-rebinding",
          [ rebindingFailure 3,
            rootFailure 3
          ]
        ),
        ( "rejected-recursive-callable-rebinding-order",
          [ recursionFailure 3 "f",
            rebindingFailure 3,
            rootFailure 3,
            recursionFailure 5 "g"
          ]
        ),
        ( "rejected-then-accepted-callable-rebinding",
          [ rootFailure 1,
            rebindingFailure 3
          ]
        ),
        ( "repeated-rejected-callable-rebinding",
          [ rootFailure 1,
            rebindingFailure 3,
            rootFailure 3
          ]
        ),
        ( "scalar-then-rejected-callable-control",
          [rootFailure 2]
        ),
        ( "accepted-scalar-rejected-callable-rebinding",
          [ rootFailure 2,
            rebindingFailure 4,
            rootFailure 4
          ]
        ),
        ( "rejected-scalar-accepted-callable-rebinding",
          [ rootFailure 1,
            rootFailure 2,
            rebindingFailure 4
          ]
        )
      ]
    assertExact name expectedFailures = do
      let fixture = producerEdgeFixture name
      firstRun <- produceFixture fixture
      secondRun <- produceFixture fixture
      assertEqual
        (name <> " ordinary diagnostics")
        []
        ( filter
            isErrorDiagnostic
            (inferredDiagnostics (typedCoreProductionInferenceResult firstRun))
        )
      assertEqual (name <> " repeatability") firstRun secondRun
      assertEqual
        (name <> " exact callable rebinding result")
        (TypedCoreProductionUnsupported expectedFailures)
        (typedCoreProductionStatus firstRun)
    rebindingFailure statementIndex =
      TypedCoreProductionFailure
        (TypedCoreProductionStatementPath ["App", "Main"] statementIndex)
        TypedCoreFunctionRebindingUnsupported
        (TypedCoreNameDetail "f")
    recursionFailure statementIndex name =
      TypedCoreProductionFailure
        (TypedCoreProductionStatementPath ["App", "Main"] statementIndex)
        TypedCoreRecursiveFunctionUnsupported
        (TypedCoreNameDetail name)
    rootFailure statementIndex =
      TypedCoreProductionFailure
        (TypedCoreProductionStatementPath ["App", "Main"] statementIndex)
        TypedCoreUnsupportedRootExpression
        TypedCoreUnsupportedRootDetail

testCanonicalCallableRebindingDependencies :: Text -> IO ()
testCanonicalCallableRebindingDependencies requestedName =
  case lookup requestedName acceptedGroups of
    Just groups -> assertAccepted requestedName groups
    Nothing ->
      case lookup requestedName expectedResults of
        Just expectedFailures -> assertExact requestedName expectedFailures
        Nothing -> error "canonical callable rebinding fixture has no expected result"
  where
    acceptedGroups =
      [ ("canonical-self-recursion-no-prior", [[(1, "loop")]]),
        ("canonical-mutual-recursion-peers", [[(1, "left"), (3, "right")]])
      ]
    expectedResults =
      [ ( "later-callable-rebinding-calls-nearest-prior",
          [rebindingFailure 3 "identity"]
        ),
        ( "three-same-name-nearest-prior-mutual-recursion",
          [ rebindingFailure 3 "identity",
            recursionFailure 5 "identity",
            rebindingFailure 5 "identity",
            recursionFailure 7 "peer"
          ]
        ),
        ( "nearest-rebinding-mutual-control",
          [rebindingFailure 5 "left"]
        ),
        ( "rebinding-parameter-shadow-control",
          [rebindingFailure 3 "apply"]
        ),
        ( "rebinding-local-shadow-control",
          [ rebindingFailure 3 "loop",
            expressionFailure 3 [0, 0] TypedCoreNestedBlockUnsupported TypedCoreLocalBlockDetail
          ]
        )
      ]
    assertExact name expectedFailures = do
      let fixture = producerEdgeFixture name
      firstRun <- produceFixture fixture
      secondRun <- produceFixture fixture
      assertEqual
        (name <> " ordinary diagnostics")
        []
        ( filter
            isErrorDiagnostic
            (inferredDiagnostics (typedCoreProductionInferenceResult firstRun))
        )
      assertEqual (name <> " repeatability") firstRun secondRun
      assertEqual
        (name <> " exact canonical owners, order, and multiplicity")
        (TypedCoreProductionUnsupported expectedFailures)
        (typedCoreProductionStatus firstRun)
    assertAccepted name expectedGroups = do
      let fixture = producerEdgeFixture name
      firstRun <- produceFixture fixture
      secondRun <- produceFixture fixture
      assertEqual
        (name <> " ordinary diagnostics")
        []
        (filter isErrorDiagnostic (inferredDiagnostics (typedCoreProductionInferenceResult firstRun)))
      assertEqual (name <> " repeatability") firstRun secondRun
      case typedCoreProductionStatus firstRun of
        TypedCoreProductionSucceeded program -> do
          assertEqual (name <> " accepted recursive validation") [] (validateTypedProgram program)
          assertEqual (name <> " exact typed recursive groups") expectedGroups (typedRecursiveGroupOwners program)
        status -> failTest (name <> " did not produce typed recursion: " <> Text.pack (show status))
    typedRecursiveGroupOwners program =
      case program of
        TypedProgram _ [TypedModule _ _ _ _ _ groups _ _] _ ->
          [ [ (statementIndex, name)
            | TypedBinderId (_, statementIndex : _, TypedResolvedName _ _ name) <- members
            ]
          | TypedRecursiveGroup members <- groups
          ]
        _ -> error "canonical direct recursion expected one typed module"
    recursionFailure statementIndex name =
      TypedCoreProductionFailure
        (TypedCoreProductionStatementPath ["App", "Main"] statementIndex)
        TypedCoreRecursiveFunctionUnsupported
        (TypedCoreNameDetail name)
    rebindingFailure statementIndex name =
      TypedCoreProductionFailure
        (TypedCoreProductionStatementPath ["App", "Main"] statementIndex)
        TypedCoreFunctionRebindingUnsupported
        (TypedCoreNameDetail name)
    expressionFailure statementIndex childPath kind detail =
      TypedCoreProductionFailure
        (TypedCoreProductionExpressionPath ["App", "Main"] statementIndex childPath)
        kind
        detail

testRejectedCallableDeclarationTransport :: Text -> IO ()
testRejectedCallableDeclarationTransport requestedName =
  case lookup requestedName expectedResults of
    Just expectedFailures -> assertExact requestedName expectedFailures
    Nothing -> error "rejected callable declaration fixture has no expected result"
  where
    expectedResults =
      [ ( "rejected-self-alias-recursion",
          [ recursionFailure 1 "loop",
            rootFailure 1
          ]
        ),
        ( "rejected-mutual-alias-recursion",
          [ recursionFailure 1 "left",
            rootFailure 1,
            recursionFailure 3 "right",
            rootFailure 3
          ]
        ),
        ( "rejected-alias-conditional-mutual-recursion",
          [ recursionFailure 1 "left",
            rootFailure 1,
            recursionFailure 3 "right",
            rootFailure 3
          ]
        ),
        ( "rejected-operator-alias-self-recursion",
          [ recursionFailure 1 "$operator:%25%25",
            rootFailure 1,
            expressionFailure 1 [0] TypedCoreUserDefinedOperatorUnsupported TypedCoreUnsupportedRootDetail
          ]
        ),
        ( "rejected-eager-operator-conditional-control",
          [ rootFailure 1,
            expressionFailure 1 [0, 0] TypedCoreUserDefinedOperatorUnsupported TypedCoreUnsupportedRootDetail,
            expressionFailure 1 [0, 1] TypedCoreUserDefinedOperatorUnsupported TypedCoreUnsupportedRootDetail,
            expressionFailure 1 [0, 2] TypedCoreUserDefinedOperatorUnsupported TypedCoreUnsupportedRootDetail
          ]
        ),
        ( "rejected-alias-parameter-shadow-control",
          [rootFailure 3]
        ),
        ( "rejected-alias-local-shadow-control",
          [ rootFailure 1,
            expressionFailure 1 [0] TypedCoreNestedBlockUnsupported TypedCoreLocalBlockDetail
          ]
        ),
        ( "rejected-eager-self-before-callable-result-control",
          [ rootFailure 1,
            expressionFailure 1 [0] TypedCoreNestedBlockUnsupported TypedCoreLocalBlockDetail
          ]
        ),
        ( "rejected-block-nearest-prior-callable-rebinding-recursion",
          [ recursionFailure 1 "f",
            rootFailure 1,
            expressionFailure 1 [0] TypedCoreNestedBlockUnsupported TypedCoreLocalBlockDetail
          ]
        )
      ]
    assertExact name expectedFailures = do
      let fixture = producerEdgeFixture name
      firstRun <- produceFixture fixture
      secondRun <- produceFixture fixture
      assertEqual
        (name <> " ordinary diagnostics")
        []
        ( filter
            isErrorDiagnostic
            (inferredDiagnostics (typedCoreProductionInferenceResult firstRun))
        )
      assertEqual (name <> " repeatable production") firstRun secondRun
      assertEqual
        (name <> " exact declaration-owned rejection")
        (TypedCoreProductionUnsupported expectedFailures)
        (typedCoreProductionStatus firstRun)
    recursionFailure statementIndex name =
      TypedCoreProductionFailure
        (TypedCoreProductionStatementPath ["App", "Main"] statementIndex)
        TypedCoreRecursiveFunctionUnsupported
        (TypedCoreNameDetail name)
    rootFailure statementIndex =
      TypedCoreProductionFailure
        (TypedCoreProductionStatementPath ["App", "Main"] statementIndex)
        TypedCoreUnsupportedRootExpression
        TypedCoreUnsupportedRootDetail
    expressionFailure statementIndex childPath kind detail =
      TypedCoreProductionFailure
        (TypedCoreProductionExpressionPath ["App", "Main"] statementIndex childPath)
        kind
        detail

testRejectedProducerDependencyTransport :: IO ()
testRejectedProducerDependencyTransport =
  mapM_
    assertExact
    [ ( "rejected-block-conditional-mutual-recursion",
        [ statementFailure 1 "left",
          expressionFailure 1 [0, 0] TypedCoreNestedBlockUnsupported TypedCoreLocalBlockDetail,
          statementFailure 3 "right"
        ]
      ),
      ( "rejected-block-parameter-shadow-control",
        [expressionFailure 3 [0, 0] TypedCoreNestedBlockUnsupported TypedCoreLocalBlockDetail]
      ),
      ( "rejected-block-later-shadow-control",
        [ statementFailure 1 "loop",
          expressionFailure 1 [0, 0] TypedCoreNestedBlockUnsupported TypedCoreLocalBlockDetail
        ]
      ),
      ( "rejected-block-initializer-self-recursion",
        [ statementFailure 1 "loop",
          expressionFailure 1 [0, 0] TypedCoreNestedBlockUnsupported TypedCoreLocalBlockDetail
        ]
      ),
      ( "rejected-block-initializer-mutual-recursion",
        [ statementFailure 1 "left",
          expressionFailure 1 [0, 0] TypedCoreNestedBlockUnsupported TypedCoreLocalBlockDetail,
          statementFailure 3 "right",
          expressionFailure 3 [0, 0] TypedCoreNestedBlockUnsupported TypedCoreLocalBlockDetail
        ]
      ),
      ( "rejected-block-later-signed-shadow-control",
        [ statementFailure 1 "loop",
          expressionFailure 1 [0, 0] TypedCoreNestedBlockUnsupported TypedCoreLocalBlockDetail
        ]
      ),
      ( "rejected-block-local-shadow-cycle-control",
        [expressionFailure 3 [0, 0] TypedCoreNestedBlockUnsupported TypedCoreLocalBlockDetail]
      ),
      ( "rejected-block-parameter-shadow-cycle-control",
        [expressionFailure 1 [0, 0] TypedCoreNestedBlockUnsupported TypedCoreLocalBlockDetail]
      ),
      ( "rejected-operator-value-self-recursion",
        [ generatedOperatorFailure 1,
          statementFailure 1 "$operator:%25%25",
          userOperatorFailure 1 [0, 0, 0, 0, 0]
        ]
      ),
      ( "rejected-infix-operator-mutual-recursion",
        [ generatedOperatorFailure 1,
          statementFailure 1 "$operator:%25%25",
          userOperatorFailure 1 [0, 0, 0],
          generatedOperatorFailure 3,
          statementFailure 3 "$operator:%7E%7E",
          userOperatorFailure 3 [0, 0, 0]
        ]
      ),
      ( "rejected-section-operator-mutual-recursion",
        [ generatedOperatorFailure 1,
          statementFailure 1 "$operator:%25%25",
          userOperatorFailure 1 [0, 0, 0, 0],
          generatedOperatorFailure 3,
          statementFailure 3 "$operator:%7E%7E",
          userOperatorFailure 3 [0, 0, 0, 0]
        ]
      )
    ]
  where
    assertExact (name, expectedFailures) = do
      let fixture = producerEdgeFixture name
      firstRun <- produceFixture fixture
      secondRun <- produceFixture fixture
      assertEqual
        (name <> " ordinary diagnostics")
        []
        ( filter
            isErrorDiagnostic
            (inferredDiagnostics (typedCoreProductionInferenceResult firstRun))
        )
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
    generatedOperatorFailure statementIndex =
      TypedCoreProductionFailure
        (TypedCoreProductionStatementPath ["App", "Main"] statementIndex)
        TypedCoreUserDefinedOperatorUnsupported
        TypedCoreUnsupportedRootDetail
    userOperatorFailure statementIndex childPath =
      expressionFailure
        statementIndex
        childPath
        TypedCoreUserDefinedOperatorUnsupported
        TypedCoreUnsupportedRootDetail
    expressionFailure statementIndex childPath kind detail =
      TypedCoreProductionFailure
        (TypedCoreProductionExpressionPath ["App", "Main"] statementIndex childPath)
        kind
        detail

testOperatorDependencyNames :: IO ()
testOperatorDependencyNames = do
  let userOperator = operatorBindingName "%%"
      literal = ELit (LInt 1)
  assertEqual
    "operator value dependency"
    (Set.singleton userOperator)
    (expressionDependencyNames (EOperatorValue "%%"))
  assertEqual
    "infix operator dependency"
    (Set.singleton userOperator)
    (expressionDependencyNames (EBinary "%%" literal literal))
  assertEqual
    "left section operator dependency"
    (Set.singleton userOperator)
    (expressionDependencyNames (ESectionLeft literal "%%"))
  assertEqual
    "right section operator dependency"
    (Set.singleton userOperator)
    (expressionDependencyNames (ESectionRight "%%" literal))
  assertEqual
    "builtin operator forms are dependency free"
    Set.empty
    ( foldMap
        expressionDependencyNames
        [ EOperatorValue "+",
          EBinary "+" literal literal,
          ESectionLeft literal "+",
          ESectionRight "+" literal
        ]
    )

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
      leadingStatementResult <- produceResolvedFixture unitFixture leadingStatement
      case typedCoreProductionStatus leadingStatementResult of
        TypedCoreProductionSucceeded programValue ->
          assertEqual "unit scalar binding typed-core validation" [] (validateTypedProgram programValue)
        _ -> failTest "unit scalar binding did not produce typed core"
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
    [] -> error ("fixture is missing: " <> Text.unpack name)
    _ -> error ("fixture name is ambiguous: " <> Text.unpack name)
