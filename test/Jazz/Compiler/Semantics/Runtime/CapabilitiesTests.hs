{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Jazz.Compiler.Semantics.Runtime.CapabilitiesTests
  ( capabilityTests,
  )
where

import Control.Exception
  ( SomeException,
    try,
  )
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Jazz.Compiler.AST
  ( CaseArm (..),
    CoreNode (..),
    CorePhase (Analyzed),
    Expr (..),
    ImplMethod (..),
    Literal (..),
    Statement (..),
  )
import Jazz.Compiler.CoreIdentity (CapabilityId (..), ImplId (..), MethodId (..), ResolvedReference (..))
import Jazz.Compiler.Diagnostics
  ( SourceSpan (..),
    isErrorDiagnostic,
  )
import Jazz.Compiler.Diagnostics.Render
  ( renderDiagnostic,
  )
import Jazz.Compiler.Driver
  ( RunResult,
    runCompileErrors,
    runOutput,
    runRuntimeErrors,
    runSource,
    runSourceWithPrelude,
  )
import Jazz.Compiler.ModuleAnalysis
  ( analyzeResolvedExpression,
  )
import Jazz.Compiler.ModuleExports (exportInventory)
import Jazz.Compiler.ModuleIdentity (SourceUnitOwner (..), mkModulePath, standaloneModulePath)
import Jazz.Compiler.ModuleResolver (resolveStandaloneExprNames)
import Jazz.Compiler.Name (NameNamespace (ConstructorNamespace, TypeNamespace), ResolvedName, mkIdentifier, resolveDeclarationOwner, resolvedImportedName, resolvedLocalName)
import Jazz.Compiler.Runtime
  ( ModuleEvaluationMode (..),
    RuntimeAnnotation (..),
    RuntimeValue (..),
    ScopeResult (..),
    evaluateModuleScopePure,
    evaluateRuntimeExpr,
    renderRuntimeValue,
    runtimeValueExactlyMatchesConstraint,
  )
import Jazz.Compiler.Runtime.Semantics (applyExplicitTypeApplicationResultHint, applyRuntimeTypeHint, runtimeValueMatchesConstraint)
import Jazz.Compiler.Runtime.Types
  ( RuntimeMethodCandidate (..),
    runtimeMethodCandidatesInOrder,
    pattern VQualifiedMethodApplication,
  )
import Jazz.Compiler.SemanticFacts
  ( AnalyzedType,
    EvidenceReference (..),
    ExpressionFacts (..),
    SemanticInstantiation (..),
  )
import Jazz.Compiler.Semantics.Runtime.Fixtures
import Jazz.Compiler.Semantics.Runtime.ResolvedFixture
import Jazz.Compiler.Semantics.Runtime.Shared
import Jazz.Compiler.SourceProgram (parseAndLowerStandaloneSource)
import Jazz.Compiler.TypeInference.Result (InferenceResult, inferredDiagnostics)
import Jazz.Compiler.TypeRepresentation
  ( InferenceVariable (..),
    SemanticType (..),
    SignaturePayload (..),
    SignatureType (..),
  )
import Jazz.Compiler.WarningConfig
  ( defaultWarningSettings,
  )
import Jazz.TestHarness
  ( NamedTest,
    assertContains,
    assertEqual,
    assertSingleDiagnosticContains,
    failTest,
  )
import System.Timeout
  ( timeout,
  )

capabilityTests :: [NamedTest]
capabilityTests =
  [ ("method-local constructor constraints support nested traversal", testMethodLocalTraversal),
    ("recursive constrained helpers forward dictionaries through their group", testRecursiveHelperEvidence),
    ("superclass evidence and generic method aliases retain their parameters", testSuperclassAndAliasEvidence),
    ("generic methods carry recursive evidence, defaults, and ordinary aliases", testGenericDefaultsAndAliases),
    ("generic mapping preserves constructors and instantiates method locals independently", testGenericConstructorMapping),
    ("runtime rejects qualified methods without checked evidence", testRuntimeFallbackRejectsQualifiedMethodStructuralEquality),
    ("scope with only capability declarations has no runtime output", testCapabilityDeclarationOnlyScopeHasNoOutput),
    ("capability declarations are inert at runtime", testCapabilityDeclarationsRuntimeInert),
    ("qualified method candidates carry compiler-owned runtime evidence", testQualifiedMethodCandidateCarriesRuntimeEvidence),
    ("selected method evidence rejects a mismatched target type", testSelectedMethodRejectsMismatchedEvidence),
    ("runtime data constraints accept defining and importing views of one owner", testRuntimeDataConstraintsAcceptNominalViews),
    ("runtime data constraints reject identical names from different owners", testRuntimeDataConstraintsRejectDifferentOwners),
    ("runtime data hints preserve nominal identity", testRuntimeDataHintsPreserveNominalIdentity),
    ("unrelated explicit result hints preserve phantom type arguments", testExplicitResultHintsPreserveUnrelatedPhantomArguments),
    ("qualified method application preserves argument order", testQualifiedMethodApplicationPreservesArgumentOrder),
    ("qualified method dispatch executes selected impl body", testQualifiedMethodDispatchExecutesImplBody),
    ("let-bound qualified method dispatch executes selected impl body", testLetBoundQualifiedMethodDispatchExecutesImplBody),
    ("qualified method dispatch selects runtime body by argument types", testQualifiedMethodDispatchSelectsRuntimeBodyByArgumentTypes),
    ("qualified method dispatch executes same-impl qualified method call", testQualifiedMethodDispatchExecutesSameImplQualifiedMethodCall),
    ("qualified method dispatch selects width-specific integer body", testQualifiedMethodDispatchSelectsWidthSpecificIntegerBody),
    ("qualified method dispatch selects width-specific integer body for direct literals", testQualifiedMethodDispatchSelectsWidthSpecificIntegerBodyForDirectLiterals),
    ("qualified method dispatch preserves direct explicit type application hints", testQualifiedMethodDispatchPreservesDirectExplicitTypeApplicationHint),
    ("qualified method dispatch selects a nullary body by explicit target", testQualifiedMethodDispatchSelectsNullaryBodyByExplicitTarget),
    ("qualified method dispatch selects a nullary body by binding result type", testQualifiedMethodDispatchSelectsNullaryBodyByBindingResultType),
    ("nullary method selection records canonical analyzed evidence", testNullaryMethodSelectionRecordsCanonicalAnalyzedEvidence),
    ("qualified method dispatch preserves inferred explicit type application tuple hints", testQualifiedMethodDispatchPreservesInferredExplicitTypeApplicationTupleHint),
    ("qualified method dispatch applies explicit type argument to matching parameter", testQualifiedMethodDispatchAppliesExplicitTypeArgumentToMatchingParameter),
    ("qualified method dispatch preserves partially instantiated function templates", testQualifiedMethodDispatchPreservesPartiallyInstantiatedFunctionTemplate),
    ("qualified method dispatch preserves non-literal integer signature targets", testQualifiedMethodDispatchPreservesNonLiteralIntegerSignatureTarget),
    ("qualified method dispatch preserves direct closure result signatures", testQualifiedMethodDispatchPreservesDirectClosureResultSignature),
    ("qualified method dispatch preserves tuple binding signatures", testQualifiedMethodDispatchPreservesTupleBindingSignature),
    ("qualified method dispatch preserves tuple exact signatures", testQualifiedMethodDispatchPreservesTupleExactSignature),
    ("qualified method dispatch preserves section binding signatures", testQualifiedMethodDispatchPreservesSectionBindingSignature),
    ("qualified method dispatch treats Float as Float64 alias at runtime", testQualifiedMethodDispatchTreatsFloatAsFloat64Alias),
    ("qualified method dispatch prefers Float alias body for typed Float values", testQualifiedMethodDispatchPrefersFloatAliasBody),
    ("qualified method dispatch preserves concrete left Float64 over right Float aliases", testQualifiedMethodDispatchPreservesConcreteLeftFloat64OverRightFloatAlias),
    ("qualified method dispatch mirrors runtime Float64-domain arithmetic", testQualifiedMethodDispatchMirrorsRuntimeFloat64DomainArithmetic),
    ("qualified method dispatch executes Float equality body", testQualifiedMethodDispatchExecutesFloatEqualityBody),
    ("qualified method dispatch executes Float16 equality body", testQualifiedMethodDispatchExecutesFloat16EqualityBody),
    ("qualified method dispatch executes Float32 equality body", testQualifiedMethodDispatchExecutesFloat32EqualityBody),
    ("qualified method dispatch executes Float64 equality body", testQualifiedMethodDispatchExecutesFloat64EqualityBody),
    ("qualified method dispatch treats Int as Int64 alias at runtime", testQualifiedMethodDispatchTreatsIntAsInt64Alias),
    ("qualified method dispatch re-hints Int aliases for Int64 parameters", testQualifiedMethodDispatchRehintsIntAliasForInt64Parameter),
    ("qualified method dispatch prefers Int alias body for typed Int values", testQualifiedMethodDispatchPrefersIntAliasBody),
    ("qualified method dispatch prefers Int alias body for direct integer literals", testQualifiedMethodDispatchPrefersIntAliasBodyForDirectLiteral),
    ("qualified method dispatch prefers list alias body for typed list values", testQualifiedMethodDispatchPrefersListAliasBody),
    ("qualified method dispatch prefers list alias body for direct list literals", testQualifiedMethodDispatchPrefersListAliasBodyForDirectLiteral),
    ("raw list prepend re-hints the head to the concrete tail element type", testRawListPrependRehintsHeadToConcreteTailElementType),
    ("qualified method dispatch preserves bound nested list runtime hints", testQualifiedMethodDispatchPreservesBoundNestedListRuntimeHint),
    ("qualified method dispatch instantiates explicit empty list type application hints", testQualifiedMethodDispatchInstantiatesExplicitEmptyListTypeApplicationHint),
    ("qualified method dispatch infers nested list helper targets", testQualifiedMethodDispatchInfersNestedListHelperExactSelection),
    ("qualified method dispatch does not exact-match untyped empty list literals", testQualifiedMethodDispatchDoesNotExactMatchUntypedEmptyListLiteral),
    ("qualified method dispatch prefers constructor alias body for direct constructor literals", testQualifiedMethodDispatchPrefersConstructorAliasBodyForDirectLiteral),
    ("qualified method dispatch uses structured constructor payloads for exact selection", testQualifiedMethodDispatchUsesStructuredConstructorPayloadForExactSelection),
    ("qualified method dispatch preserves the inferred Int type through applications", testQualifiedMethodDispatchPreservesInferredIntegerType),
    ("qualified method dispatch preserves higher-order binding signatures", testQualifiedMethodDispatchPreservesHigherOrderBindingSignature),
    ("qualified method dispatch preserves higher-order exact signatures", testQualifiedMethodDispatchPreservesHigherOrderExactSignature),
    ("qualified method dispatch infers function argument targets", testQualifiedMethodDispatchInfersFunctionArgumentExactSelection),
    ("qualified method dispatch defers exact filtering until target argument", testQualifiedMethodDispatchDefersExactFilteringUntilTargetArgument),
    ("qualified method dispatch preserves selected method signatures", testQualifiedMethodDispatchPreservesSelectedMethodSignature),
    ("qualified method dispatch applies typed callable argument hints", testQualifiedMethodDispatchAppliesTypedCallableArgumentHint),
    ("qualified method dispatch applies typed callable argument hints through prefix dollar", testQualifiedMethodDispatchAppliesTypedCallableArgumentHintThroughPrefixDollar),
    ("qualified method dispatch applies closure argument signature hints", testQualifiedMethodDispatchAppliesClosureArgumentSignatureHint),
    ("qualified method dispatch preserves defaulted closure result metadata", testQualifiedMethodDispatchPreservesDefaultedClosureResultMetadata),
    ("qualified method dispatch preserves empty list binding signatures", testQualifiedMethodDispatchPreservesEmptyListBindingSignature),
    ("qualified method dispatch preserves list-returning application signatures", testQualifiedMethodDispatchPreservesListReturningApplicationSignature),
    ("qualified method dispatch preserves dollar-applied list-returning signatures", testQualifiedMethodDispatchPreservesDollarAppliedListReturningSignature),
    ("qualified method dispatch preserves ADT-returning application signatures", testQualifiedMethodDispatchPreservesAdtReturningApplicationSignature),
    ("qualified method dispatch preserves branch result signatures", testQualifiedMethodDispatchPreservesBranchResultSignature),
    ("qualified method dispatch preserves block result signatures", testQualifiedMethodDispatchPreservesBlockResultSignature),
    ("qualified method dispatch preserves mapped empty list result signatures", testQualifiedMethodDispatchPreservesMappedEmptyListResultSignature),
    ("qualified method dispatch preserves identity-mapped empty list result signatures", testQualifiedMethodDispatchPreservesIdentityMappedEmptyListResultSignature),
    ("qualified method dispatch preserves mapped hd empty nested list result signatures", testQualifiedMethodDispatchPreservesMappedHdEmptyNestedListResultSignature),
    ("qualified method dispatch preserves hd element signatures", testQualifiedMethodDispatchPreservesHdElementSignature),
    ("qualified method dispatch normalizes hinted list aliases", testQualifiedMethodDispatchNormalizesHintedListAliases),
    ("qualified method dispatch normalizes hinted function aliases", testQualifiedMethodDispatchNormalizesHintedFunctionAliases),
    ("qualified method dispatch treats defaulted integer bindings as Int64", testQualifiedMethodDispatchTreatsDefaultedIntegerBindingAsInt64),
    ("qualified method dispatch treats plain integer bindings as Int64 when exact candidates overlap", testQualifiedMethodDispatchTreatsPlainIntegerBindingAsInt64WithExactCandidates),
    ("qualified method dispatch treats inferred direct integer literals as exact Int", testQualifiedMethodDispatchTreatsInferredDirectIntegerLiteralAsExactInt),
    ("successful direct-driver execution projects complete plans for inferred narrow bindings", testQualifiedMethodDispatchPreservesInferredNarrowIntegerBinding),
    ("qualified method dispatch preserves ADT application binding hints", testQualifiedMethodDispatchPreservesAdtApplicationBindingHint),
    ("qualified method dispatch preserves phantom ADT application binding hints", testQualifiedMethodDispatchPreservesPhantomAdtApplicationBindingHint),
    ("qualified method dispatch preserves ADT concrete payload hints", testQualifiedMethodDispatchPreservesAdtConcretePayloadHint),
    ("qualified method dispatch preserves monomorphic ADT concrete payload hints", testQualifiedMethodDispatchPreservesMonomorphicAdtConcretePayloadHint),
    ("qualified method dispatch keeps nested inferred hints scoped", testQualifiedMethodDispatchKeepsNestedInferredHintsScoped),
    ("authored module transitions own standalone plans and evidence", testAuthoredModuleTransitionOwnsFactsAndEvidence),
    ("qualified method dispatch prefers alias binding over method sentinel at runtime", testQualifiedMethodDispatchPrefersAliasBindingOverMethodSentinelAtRuntime),
    ("qualified zero-argument method dispatch returns itemValue", testQualifiedZeroArgumentMethodDispatchReturnsValue),
    ("qualified method dispatch rejects direct self alias", testQualifiedMethodDispatchRejectsDirectSelfAlias),
    ("qualified method dispatch rejects wrapped self alias", testQualifiedMethodDispatchRejectsWrappedSelfAlias),
    ("qualified method dispatch rejects block-local self alias", testQualifiedMethodDispatchRejectsBlockLocalSelfAlias),
    ("qualified method dispatch follows block-local alias branches with local bindings", testQualifiedMethodDispatchFollowsBlockLocalAliasBranchesWithLocalBindings),
    ("qualified method dispatch follows block-local alias branches with local signature hints", testQualifiedMethodDispatchFollowsBlockLocalAliasBranchesWithLocalSignatureHints),
    ("qualified method dispatch requires checked full-arity evidence", testQualifiedMethodDispatchRejectsFullArityRuntimeAmbiguity),
    ("qualified method dispatch executes local ADT impl body", testQualifiedMethodDispatchExecutesLocalAdtImplBody),
    ("method-bearing capability declarations are inert at runtime", testMethodBearingCapabilityDeclarationsRuntimeInert)
  ]

testRuntimeFallbackRejectsQualifiedMethodStructuralEquality :: IO ()
testRuntimeFallbackRejectsQualifiedMethodStructuralEquality = do
  let result = evaluateFixture qualifiedMethodStructuralEqualityExpr
  assertRuntimeErrorContains "runtime fallback qualified method structural equality" "E3026" result
  assertRuntimeErrorContains
    "runtime fallback qualified method structural equality callable text"
    "missing or inconsistent checked capability evidence"
    result

testCapabilityDeclarationOnlyScopeHasNoOutput :: IO ()
testCapabilityDeclarationOnlyScopeHasNoOutput = do
  result <-
    runSource
      defaultWarningSettings
      """
      class RuntimeOnly(a) { }.
      impl RuntimeOnly(Int) { }.
      """
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "declaration-only capability scope produces no output" Nothing (runOutput result)

testCapabilityDeclarationsRuntimeInert :: IO ()
testCapabilityDeclarationsRuntimeInert = do
  result <-
    runSource
      defaultWarningSettings
      """
      class RuntimeOnly(a) { }.
      impl RuntimeOnly(Int) { }.
      x = 1.
      x.
      """
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "capability declarations do not affect runtime output" (Just "1") (runOutput result)

testRuntimeDataConstraintsAcceptNominalViews :: IO ()
testRuntimeDataConstraintsAcceptNominalViews = do
  let (local, imported, _) = runtimeNominalTypeViews
      value name = VConstructor name [] (resolvedLocalName ConstructorNamespace (mkIdentifier "Wrap")) [] []
  assertEqual "imported constraint accepts defining value" True (runtimeValueMatchesConstraint (SemanticData imported []) (value local))
  assertEqual "defining constraint accepts imported value" True (runtimeValueMatchesConstraint (SemanticData local []) (value imported))

testRuntimeDataConstraintsRejectDifferentOwners :: IO ()
testRuntimeDataConstraintsRejectDifferentOwners = do
  let (local, _, unrelated) = runtimeNominalTypeViews
      value = VConstructor local [] (resolvedLocalName ConstructorNamespace (mkIdentifier "Wrap")) [] []
  assertEqual "same spelling does not identify the same data type" False (runtimeValueMatchesConstraint (SemanticData unrelated []) value)

testRuntimeDataHintsPreserveNominalIdentity :: IO ()
testRuntimeDataHintsPreserveNominalIdentity =
  mapM_
    checkShape
    [ ("monomorphic", [], [], [], []),
      ("parameterized", [parameter], [SemanticVariable parameter], [VBool True], [SemanticBool])
    ]
  where
    parameter = InferenceVariable 0
    (local, imported, unrelated) = runtimeNominalTypeViews
    checkShape (label, parameters, fields, arguments, typeArguments) = do
      let value name = VConstructor name parameters (resolvedLocalName ConstructorNamespace (mkIdentifier "Wrap")) fields arguments
          hint name = SemanticData name typeArguments
      checkHint applyRuntimeTypeHint (label <> " imported hint accepts defining value") (Just (hint imported)) (hint imported) (value local)
      checkHint applyRuntimeTypeHint (label <> " defining hint accepts imported value") (Just (hint local)) (hint local) (value imported)
      checkHint applyRuntimeTypeHint (label <> " unrelated hint leaves value unannotated") Nothing (hint unrelated) (value local)
      checkHint applyExplicitTypeApplicationResultHint (label <> " imported explicit hint accepts defining value") (Just (hint imported)) (hint imported) (value local)
      checkHint applyExplicitTypeApplicationResultHint (label <> " defining explicit hint accepts imported value") (Just (hint local)) (hint local) (value imported)
      checkHint applyExplicitTypeApplicationResultHint (label <> " unrelated explicit hint leaves value unannotated") Nothing (hint unrelated) (value local)
    checkHint applyHint label expected hint value =
      case applyHint hint value of
        Right hinted ->
          assertEqual label expected $ case hinted of
            VAnnotated (RuntimeTypeHint actual) _ -> Just actual
            _ -> Nothing
        Left diagnostic -> failTest (label <> ": " <> renderDiagnostic diagnostic)

testExplicitResultHintsPreserveUnrelatedPhantomArguments :: IO ()
testExplicitResultHintsPreserveUnrelatedPhantomArguments = do
  let (local, _, unrelated) = runtimeNominalTypeViews
      value =
        VAnnotated
          (RuntimeTypeHint (SemanticData local [SemanticBool]))
          (VConstructor local [InferenceVariable 0] (resolvedLocalName ConstructorNamespace (mkIdentifier "Wrap")) [] [])
  case applyExplicitTypeApplicationResultHint (SemanticData unrelated [SemanticBool]) value of
    Right hinted -> do
      assertEqual "existing phantom Bool argument still matches" True (runtimeValueExactlyMatchesConstraint (SemanticData local [SemanticBool]) hinted)
      assertEqual "unrelated hint does not erase the phantom argument" False (runtimeValueExactlyMatchesConstraint (SemanticData local [SemanticInt]) hinted)
    Left diagnostic -> failTest ("unrelated explicit result hint: " <> renderDiagnostic diagnostic)

runtimeNominalTypeViews :: (ResolvedName, ResolvedName, ResolvedName)
runtimeNominalTypeViews =
  ( resolveDeclarationOwner (NamedSourceUnit definingModule) local,
    resolvedImportedName definingModule TypeNamespace (mkIdentifier "Box"),
    resolveDeclarationOwner (NamedSourceUnit unrelatedModule) local
  )
  where
    local = resolvedLocalName TypeNamespace (mkIdentifier "Box")
    definingModule = mkModulePath (mkIdentifier "Lib" NonEmpty.:| [mkIdentifier "One"])
    unrelatedModule = mkModulePath (mkIdentifier "Lib" NonEmpty.:| [mkIdentifier "Two"])

testSelectedMethodRejectsMismatchedEvidence :: IO ()
testSelectedMethodRejectsMismatchedEvidence = do
  (_, analyzed) <- analyzeRuntimeFacts "class RuntimeDefault(a) { defaultValue :: a. }. impl RuntimeDefault(Int) { defaultValue = 41. }. (RuntimeDefault::defaultValue @Int)."
  case analyzed of
    EBlock root statements -> case reverse statements of
      SExpr statement (ETypeApplication node function spanValue argument) : prefix -> do
        let facts = coreNodeFacts node
            mismatched = node {coreNodeFacts = facts {expressionEvidence = [evidence {evidenceType = SemanticBool} | evidence <- expressionEvidence facts]}}
            expression = EBlock root (reverse prefix <> [SExpr statement (ETypeApplication mismatched function spanValue argument)])
        assertRuntimeErrorContains "mismatched selected evidence" "inconsistent selected method evidence" (evaluateRuntimeExpr expression)
      _ -> failTest "expected terminal explicit method instantiation"
    _ -> failTest "expected analyzed block"

testQualifiedMethodCandidateCarriesRuntimeEvidence :: IO ()
testQualifiedMethodCandidateCarriesRuntimeEvidence =
  case candidateValue of
    Right (Just methodValue@(VQualifiedMethodApplication _ _ _ candidateSet _)) -> do
      let candidates = runtimeMethodCandidatesInOrder candidateSet
      assertEqual
        "runtime candidate evidence target order"
        [SemanticInt, SemanticBool]
        [evidenceType evidence | RuntimeMethodCandidate evidence _ <- candidates]
      assertContains
        "runtime candidate evidence record"
        "EvidenceReference"
        (Text.pack (show methodValue))
      assertContains
        "runtime candidate evidence class"
        "Eq"
        (Text.pack (show methodValue))
      assertContains
        "runtime candidate evidence target"
        "Int"
        (Text.pack (show methodValue))
      assertEqual "runtime evidence stays non-user-visible" "<function>" (renderRuntimeValue methodValue)
    Right otherValue ->
      failTest ("expected qualified method runtime itemValue, got " <> Text.pack (show otherValue))
    Left runtimeError ->
      failTest ("expected qualified method runtime itemValue, got " <> renderDiagnostic runtimeError)
  where
    candidateValue = do
      scope <- evaluateModuleScopePure EvaluateDependencyModule Map.empty (resolveRuntimeFixture qualifiedMethodEvidenceExpr)
      case [cell | (CapabilityMethodReference {}, cell) <- Map.toList (scopeResultEnvironment scope)] of
        [cell] -> Just <$> cell
        _ -> error "expected one declared method"
    qualifiedMethodEvidenceExpr =
      expressionBlock
        [ statementClass
            (SourceSpan 1 1)
            "Eq"
            ["a"]
            [ classMethodSignature
                "equals"
                (SourceSpan 2 1)
                ( ConstrainedSignature
                    []
                    ( TypeFunction
                        (fixtureTypeVariable "a")
                        (TypeFunction (fixtureTypeVariable "a") (TypeBool))
                    )
                )
            ],
          statementImpl
            (SourceSpan 3 1)
            "Eq"
            [TypeInt]
            [ implMethod
                "equals"
                (SourceSpan 4 1)
                (expressionLambda "left" (expressionLambda "right" (expressionLiteral (LBool True))))
            ],
          statementImpl
            (SourceSpan 5 1)
            "Eq"
            [TypeBool]
            [ implMethod
                "equals"
                (SourceSpan 6 1)
                (expressionLambda "left" (expressionLambda "right" (expressionLiteral (LBool True))))
            ]
        ]

testQualifiedMethodApplicationPreservesArgumentOrder :: IO ()
testQualifiedMethodApplicationPreservesArgumentOrder = do
  result <-
    runSource
      defaultWarningSettings
      """
      class RuntimeOrder(a) {
      order :: Int -> a -> Int.
      }.
      impl RuntimeOrder(Int) {
      order = \\(left, right) -> left * 10 + right.
      }.
      impl RuntimeOrder(Bool) {
      order = \\(left, right) -> left.
      }.
      RuntimeOrder::order 1 2.
      """
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "12") (runOutput result)

testQualifiedMethodDispatchExecutesImplBody :: IO ()
testQualifiedMethodDispatchExecutesImplBody = do
  result <- runSource defaultWarningSettings (runtimeEqSource <> "RuntimeEq::equals 1 1.")
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "True") (runOutput result)

testLetBoundQualifiedMethodDispatchExecutesImplBody :: IO ()
testLetBoundQualifiedMethodDispatchExecutesImplBody = do
  result <-
    runSource
      defaultWarningSettings
      ( runtimeEqSource
          <> """
             result = RuntimeEq::equals 1 1.
             result.
             """
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "True") (runOutput result)

testQualifiedMethodDispatchSelectsRuntimeBodyByArgumentTypes :: IO ()
testQualifiedMethodDispatchSelectsRuntimeBodyByArgumentTypes = do
  result <-
    runSource
      defaultWarningSettings
      ( runtimeEqSource
          <> """
             impl RuntimeEq(Bool) {
             equals = \\(left, right) -> left != right.
             }.
             (RuntimeEq::equals 1 2, RuntimeEq::equals True False).
             """
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "(False, True)") (runOutput result)

testQualifiedMethodDispatchExecutesSameImplQualifiedMethodCall :: IO ()
testQualifiedMethodDispatchExecutesSameImplQualifiedMethodCall = do
  result <-
    runSource
      defaultWarningSettings
      ( """
        class RuntimeEq(a) {
        equals :: a -> a -> Bool.
        notEquals :: a -> a -> Bool.
        }.
        impl RuntimeEq(Int) {
        equals = \\(left, right) -> left == right.
        notEquals = \\(left, right) -> RuntimeEq::equals left right != True.
        }.
        RuntimeEq::notEquals 1 2.
        """
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "True") (runOutput result)

testQualifiedMethodDispatchSelectsWidthSpecificIntegerBody :: IO ()
testQualifiedMethodDispatchSelectsWidthSpecificIntegerBody = do
  result <-
    runSource
      defaultWarningSettings
      ( """
        class RuntimeEq(a) {
        equals :: a -> a -> Bool.
        }.
        impl RuntimeEq(Int8) {
        equals = \\(left, right) -> True.
        }.
        impl RuntimeEq(Int16) {
        equals = \\(left, right) -> False.
        }.
        left :: Int8.
        left = 1.
        right :: Int8.
        right = 2.
        RuntimeEq::equals left right.
        """
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "True") (runOutput result)

testQualifiedMethodDispatchSelectsWidthSpecificIntegerBodyForDirectLiterals :: IO ()
testQualifiedMethodDispatchSelectsWidthSpecificIntegerBodyForDirectLiterals = do
  result <-
    runSource
      defaultWarningSettings
      ( """
        class RuntimeEq(a) {
        equals :: a -> a -> Bool.
        }.
        impl RuntimeEq(Int8) {
        equals = \\(left, right) -> True.
        }.
        impl RuntimeEq(Int16) {
        equals = \\(left, right) -> False.
        }.
        right :: Int8.
        right = 2.
        RuntimeEq::equals 1 right.
        """
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "True") (runOutput result)

testQualifiedMethodDispatchPreservesDirectExplicitTypeApplicationHint :: IO ()
testQualifiedMethodDispatchPreservesDirectExplicitTypeApplicationHint = do
  result <-
    runSource
      defaultWarningSettings
      ( """
        class RuntimeEq(a) {
        equals :: a -> a -> Bool.
        }.
        impl RuntimeEq(Int) {
        equals = \\(left, right) -> True.
        }.
        impl RuntimeEq(UInt8) {
        equals = \\(left, right) -> False.
        }.
        id :: @{RuntimeEq(a)}: a -> a.
        id = \\(itemValue) -> itemValue.
        result = RuntimeEq::equals (id @UInt8 1) (id @UInt8 2).
        result.
        """
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "False") (runOutput result)

testQualifiedMethodDispatchSelectsNullaryBodyByExplicitTarget :: IO ()
testQualifiedMethodDispatchSelectsNullaryBodyByExplicitTarget = do
  result <-
    runSource
      defaultWarningSettings
      ( """
        class RuntimeDefault(a) {
        defaultValue :: a.
        }.
        impl RuntimeDefault(Int) {
        defaultValue = 41.
        }.
        impl RuntimeDefault(Bool) {
        defaultValue = True.
        }.
        (RuntimeDefault::defaultValue @Int, RuntimeDefault::defaultValue @Bool).
        """
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "(41, True)") (runOutput result)

testQualifiedMethodDispatchSelectsNullaryBodyByBindingResultType :: IO ()
testQualifiedMethodDispatchSelectsNullaryBodyByBindingResultType = do
  result <-
    runSource
      defaultWarningSettings
      """
      class RuntimeDefault(a) {
      defaultValue :: a.
      }.
      impl RuntimeDefault(Int) {
      defaultValue = 41.
      }.
      impl RuntimeDefault(Bool) {
      defaultValue = True.
      }.
      itemValue :: Int.
      itemValue = RuntimeDefault::defaultValue.
      itemValue.
      """
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "41") (runOutput result)

testNullaryMethodSelectionRecordsCanonicalAnalyzedEvidence :: IO ()
testNullaryMethodSelectionRecordsCanonicalAnalyzedEvidence = do
  (inference, analyzedExpression) <-
    analyzeRuntimeFacts
      """
      class RuntimeDefault(a) {
      defaultValue :: a.
      }.
      impl RuntimeDefault(Int) {
      defaultValue = 41.
      }.
      impl RuntimeDefault(Bool) {
      defaultValue = True.
      }.
      expected :: Int.
      expected = RuntimeDefault::defaultValue.
      (expected, RuntimeDefault::defaultValue @Bool).
      """
  assertEqual "nullary evidence inference errors" [] (filter isErrorDiagnostic (inferredDiagnostics inference))
  case implementationIdentities analyzedExpression of
    [(runtimeCapability, runtimeIntImpl), (_, runtimeBoolImpl)] -> do
      let expectedResultEvidence = evidenceReference runtimeCapability runtimeIntImpl SemanticInt
          explicitTargetEvidence = evidenceReference runtimeCapability runtimeBoolImpl SemanticBool
      assertEqual
        "all nullary selection modes retain their exact selected evidence"
        [expectedResultEvidence, explicitTargetEvidence]
        (expressionEvidenceInventory analyzedExpression)
      assertEqual
        "nullary selection retains instantiation and result facts"
        [ ([expectedResultEvidence], [], Just SemanticInt),
          ([explicitTargetEvidence], [SemanticBool NonEmpty.:| []], Just SemanticBool)
        ]
        (expressionEvidenceFactsInventory analyzedExpression)
    implementations ->
      failTest ("expected two nullary implementation identities, got " <> Text.pack (show implementations))
  where
    implementationIdentities expression =
      [ (capabilityName, ImplId (StandaloneSourceUnit standaloneModulePath, coreNodeId implementationNode))
      | SImpl implementationNode capabilityName [_] _ _ <- sourceUnitStatements expression
      ]
    evidenceReference capabilityName implementationId targetType =
      EvidenceReference
        { evidenceCapability = CapabilityId capabilityName,
          evidenceImplementation = implementationId,
          evidenceMethod = Just (MethodId (implementationId, mkIdentifier "defaultValue")),
          evidenceSubstitution = Map.empty,
          evidencePrerequisites = [],
          evidenceType = targetType
        }

testQualifiedMethodDispatchPreservesInferredExplicitTypeApplicationTupleHint :: IO ()
testQualifiedMethodDispatchPreservesInferredExplicitTypeApplicationTupleHint = do
  result <-
    runSource
      defaultWarningSettings
      ( """
        class RuntimeEq(a) {
        equals :: a -> a -> Bool.
        }.
        impl RuntimeEq((Int, Bool)) {
        equals = \\(left, right) -> True.
        }.
        impl RuntimeEq((UInt8, Bool)) {
        equals = \\(left, right) -> False.
        }.
        pair = \\(itemValue) -> (itemValue, True).
        result = RuntimeEq::equals (pair @UInt8 1) (pair @UInt8 2).
        result.
        """
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "False") (runOutput result)

testQualifiedMethodDispatchAppliesExplicitTypeArgumentToMatchingParameter :: IO ()
testQualifiedMethodDispatchAppliesExplicitTypeArgumentToMatchingParameter = do
  result <-
    runSource
      defaultWarningSettings
      ( """
        class RuntimeEq(a) {
        equals :: a -> a -> Bool.
        }.
        impl RuntimeEq(Int) {
        equals = \\(left, right) -> True.
        }.
        impl RuntimeEq(UInt8) {
        equals = \\(left, right) -> False.
        }.
        select :: @{RuntimeEq(b)}: Int16 -> b -> b.
        select = \\(width, itemValue) -> itemValue.
        result = RuntimeEq::equals (select @UInt8 300 1) (select @UInt8 300 2).
        result.
        """
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "False") (runOutput result)

testQualifiedMethodDispatchPreservesPartiallyInstantiatedFunctionTemplate :: IO ()
testQualifiedMethodDispatchPreservesPartiallyInstantiatedFunctionTemplate = do
  result <-
    runSource
      defaultWarningSettings
      ( """
        class RuntimeFlag(a) {
        flag :: a -> Bool.
        }.
        impl RuntimeFlag(Int32) {
        flag = \\(itemValue) -> True.
        }.
        impl RuntimeFlag(Int64) {
        flag = \\(itemValue) -> False.
        }.
        use :: @{RuntimeFlag(a)}: a -> b -> Bool.
        use = \\(itemValue, ignored) -> RuntimeFlag::flag itemValue.
        use @Int32 1 True.
        """
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "True") (runOutput result)

testQualifiedMethodDispatchPreservesNonLiteralIntegerSignatureTarget :: IO ()
testQualifiedMethodDispatchPreservesNonLiteralIntegerSignatureTarget = do
  result <-
    runSource
      defaultWarningSettings
      ( """
        class RuntimeEq(a) {
        equals :: a -> a -> Bool.
        }.
        impl RuntimeEq(Int) {
        equals = \\(left, right) -> True.
        }.
        impl RuntimeEq(UInt8) {
        equals = \\(left, right) -> False.
        }.
        id8 :: UInt8 -> UInt8.
        id8 = \\(itemValue) -> itemValue.
        left :: UInt8.
        left = id8 1.
        right :: UInt8.
        right = id8 2.
        RuntimeEq::equals left right.
        """
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "False") (runOutput result)

testQualifiedMethodDispatchPreservesDirectClosureResultSignature :: IO ()
testQualifiedMethodDispatchPreservesDirectClosureResultSignature = do
  result <-
    runSource
      defaultWarningSettings
      ( """
        class RuntimeEq(a) {
        equals :: a -> a -> Bool.
        }.
        impl RuntimeEq(Int) {
        equals = \\(left, right) -> True.
        }.
        impl RuntimeEq(UInt8) {
        equals = \\(left, right) -> False.
        }.
        id8 :: UInt8 -> UInt8.
        id8 = \\(itemValue) -> itemValue.
        left = id8 1.
        right = id8 2.
        RuntimeEq::equals left right.
        """
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "False") (runOutput result)

testQualifiedMethodDispatchPreservesTupleBindingSignature :: IO ()
testQualifiedMethodDispatchPreservesTupleBindingSignature = do
  result <-
    runSource
      defaultWarningSettings
      ( """
        class RuntimePick(a) {
        pick :: a -> Bool.
        }.
        impl RuntimePick((Int, Int)) {
        pick = \\(itemValue) -> True.
        }.
        impl RuntimePick((UInt8, UInt8)) {
        pick = \\(itemValue) -> False.
        }.
        pair :: (UInt8, UInt8).
        pair = (1, 2).
        RuntimePick::pick pair.
        """
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "False") (runOutput result)

testQualifiedMethodDispatchPreservesTupleExactSignature :: IO ()
testQualifiedMethodDispatchPreservesTupleExactSignature = do
  result <-
    runSource
      defaultWarningSettings
      ( """
        class RuntimePick(a) {
        pick :: a -> Bool.
        }.
        impl RuntimePick((Int, Int)) {
        pick = \\(itemValue) -> True.
        }.
        impl RuntimePick((Int64, Int64)) {
        pick = \\(itemValue) -> False.
        }.
        pair :: (Int64, Int64).
        pair = (1, 2).
        RuntimePick::pick pair.
        """
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "False") (runOutput result)

testQualifiedMethodDispatchPreservesSectionBindingSignature :: IO ()
testQualifiedMethodDispatchPreservesSectionBindingSignature = do
  result <-
    runSource
      defaultWarningSettings
      ( """
        class RuntimeApply(a) {
        apply :: (a -> a) -> Bool.
        }.
        impl RuntimeApply(Int) {
        apply = \\(fn) -> True.
        }.
        impl RuntimeApply(UInt8) {
        apply = \\(fn) -> False.
        }.
        inc8 :: UInt8 -> UInt8.
        inc8 = (+ 1).
        RuntimeApply::apply inc8.
        """
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "False") (runOutput result)

testQualifiedMethodDispatchTreatsFloatAsFloat64Alias :: IO ()
testQualifiedMethodDispatchTreatsFloatAsFloat64Alias = do
  result <-
    runSource
      defaultWarningSettings
      ( """
        class RuntimeEq(a) {
        equals :: a -> a -> Bool.
        }.
        impl RuntimeEq(Float) {
        equals = \\(left, right) -> True.
        }.
        left :: Float64.
        left = toFloat64 1.
        right :: Float64.
        right = toFloat64 1.
        RuntimeEq::equals left right.
        """
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "True") (runOutput result)

testQualifiedMethodDispatchPrefersFloatAliasBody :: IO ()
testQualifiedMethodDispatchPrefersFloatAliasBody = do
  result <-
    runSource
      defaultWarningSettings
      ( """
        class RuntimeFlag(a) {
        flag :: a -> Bool.
        }.
        impl RuntimeFlag(Float) {
        flag = \\(itemValue) -> True.
        }.
        impl RuntimeFlag(Float64) {
        flag = \\(itemValue) -> False.
        }.
        itemValue :: Float.
        itemValue = 1.5.
        RuntimeFlag::flag itemValue.
        """
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "True") (runOutput result)

testQualifiedMethodDispatchPreservesConcreteLeftFloat64OverRightFloatAlias :: IO ()
testQualifiedMethodDispatchPreservesConcreteLeftFloat64OverRightFloatAlias = do
  result <-
    runSource
      defaultWarningSettings
      ( """
        class RuntimeFlag(a) {
        flag :: a -> Bool.
        }.
        impl RuntimeFlag(Float) {
        flag = \\(itemValue) -> True.
        }.
        impl RuntimeFlag(Float64) {
        flag = \\(itemValue) -> False.
        }.
        left :: Float64.
        left = toFloat64 1.
        right :: Float.
        right = 2.5.
        (RuntimeFlag::flag) (left + right).
        """
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "False") (runOutput result)

testQualifiedMethodDispatchMirrorsRuntimeFloat64DomainArithmetic :: IO ()
testQualifiedMethodDispatchMirrorsRuntimeFloat64DomainArithmetic = do
  result <-
    runSource
      defaultWarningSettings
      ( """
        class RuntimeFlag(a) {
        flag :: a -> Bool.
        }.
        impl RuntimeFlag(Float) {
        flag = \\(itemValue) -> True.
        }.
        impl RuntimeFlag(Float64) {
        flag = \\(itemValue) -> False.
        }.
        floating :: Float64.
        floating = toFloat64 2.
        (RuntimeFlag::flag) (1.5 + floating).
        """
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "False") (runOutput result)

testQualifiedMethodDispatchExecutesFloatEqualityBody :: IO ()
testQualifiedMethodDispatchExecutesFloatEqualityBody = do
  result <-
    runSource
      defaultWarningSettings
      ( """
        class RuntimeEq(a) {
        equals :: a -> a -> Bool.
        }.
        impl RuntimeEq(Float) {
        equals = \\(left, right) -> left == right.
        }.
        left :: Float.
        left = 1.5.
        same :: Float.
        same = 1.5.
        different :: Float.
        different = 2.25.
        (RuntimeEq::equals left same, RuntimeEq::equals left different).
        """
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "(True, False)") (runOutput result)

testQualifiedMethodDispatchExecutesFloat16EqualityBody :: IO ()
testQualifiedMethodDispatchExecutesFloat16EqualityBody = do
  result <-
    runSource
      defaultWarningSettings
      ( """
        class RuntimeEq(a) {
        equals :: a -> a -> Bool.
        }.
        impl RuntimeEq(Float16) {
        equals = \\(left, right) -> left == right.
        }.
        left :: Float16.
        left = 1.5.
        same :: Float16.
        same = 1.5.
        different :: Float16.
        different = 2.25.
        (RuntimeEq::equals left same, RuntimeEq::equals left different).
        """
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "(True, False)") (runOutput result)

testQualifiedMethodDispatchExecutesFloat32EqualityBody :: IO ()
testQualifiedMethodDispatchExecutesFloat32EqualityBody = do
  result <-
    runSource
      defaultWarningSettings
      ( """
        class RuntimeEq(a) {
        equals :: a -> a -> Bool.
        }.
        impl RuntimeEq(Float32) {
        equals = \\(left, right) -> left == right.
        }.
        left :: Float32.
        left = 1.5.
        same :: Float32.
        same = 1.5.
        different :: Float32.
        different = 2.25.
        (RuntimeEq::equals left same, RuntimeEq::equals left different).
        """
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "(True, False)") (runOutput result)

testQualifiedMethodDispatchExecutesFloat64EqualityBody :: IO ()
testQualifiedMethodDispatchExecutesFloat64EqualityBody = do
  result <-
    runSource
      defaultWarningSettings
      ( """
        class RuntimeEq(a) {
        equals :: a -> a -> Bool.
        }.
        impl RuntimeEq(Float64) {
        equals = \\(left, right) -> left == right.
        }.
        left :: Float64.
        left = toFloat64 1.
        right :: Float64.
        right = toFloat64 1.
        RuntimeEq::equals left right.
        """
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "True") (runOutput result)

testQualifiedMethodDispatchTreatsIntAsInt64Alias :: IO ()
testQualifiedMethodDispatchTreatsIntAsInt64Alias = do
  result <-
    runSource
      defaultWarningSettings
      ( """
        class RuntimeEq(a) {
        equals :: a -> a -> Bool.
        }.
        impl RuntimeEq(Int) {
        equals = \\(left, right) -> True.
        }.
        impl RuntimeEq(UInt8) {
        equals = \\(left, right) -> False.
        }.
        left :: Int.
        left = 1.
        right :: Int.
        right = 2.
        RuntimeEq::equals left right.
        """
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "True") (runOutput result)

testQualifiedMethodDispatchRehintsIntAliasForInt64Parameter :: IO ()
testQualifiedMethodDispatchRehintsIntAliasForInt64Parameter = do
  result <-
    runSource
      defaultWarningSettings
      ( """
        class RuntimeFlag(a) {
        flag :: a -> Bool.
        }.
        impl RuntimeFlag(Int) {
        flag = \\(itemValue) -> True.
        }.
        impl RuntimeFlag(Int64) {
        flag = \\(itemValue) -> False.
        }.
        asInt :: Int.
        asInt = 1.
        asInt64 :: Int64 -> Int64.
        asInt64 = \\(itemValue) -> itemValue.
        (RuntimeFlag::flag) (asInt64 asInt).
        """
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "False") (runOutput result)

testQualifiedMethodDispatchPrefersIntAliasBody :: IO ()
testQualifiedMethodDispatchPrefersIntAliasBody = do
  result <-
    runSource
      defaultWarningSettings
      ( """
        class RuntimeFlag(a) {
        flag :: a -> Bool.
        }.
        impl RuntimeFlag(Int) {
        flag = \\(itemValue) -> True.
        }.
        impl RuntimeFlag(Int64) {
        flag = \\(itemValue) -> False.
        }.
        itemValue :: Int.
        itemValue = 1.
        RuntimeFlag::flag itemValue.
        """
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "True") (runOutput result)

testQualifiedMethodDispatchPrefersIntAliasBodyForDirectLiteral :: IO ()
testQualifiedMethodDispatchPrefersIntAliasBodyForDirectLiteral = do
  result <-
    runSource
      defaultWarningSettings
      ( """
        class RuntimeFlag(a) {
        flag :: a -> Bool.
        }.
        impl RuntimeFlag(Int) {
        flag = \\(itemValue) -> True.
        }.
        impl RuntimeFlag(Int64) {
        flag = \\(itemValue) -> False.
        }.
        RuntimeFlag::flag 1.
        """
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "True") (runOutput result)

testQualifiedMethodDispatchPrefersListAliasBody :: IO ()
testQualifiedMethodDispatchPrefersListAliasBody = do
  result <-
    runSource
      defaultWarningSettings
      ( """
        class RuntimeFlag(a) {
        flag :: a -> Bool.
        }.
        impl RuntimeFlag([Int]) {
        flag = \\(values) -> True.
        }.
        impl RuntimeFlag([Int64]) {
        flag = \\(values) -> False.
        }.
        values :: [Int].
        values = [1, 2].
        RuntimeFlag::flag values.
        """
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "True") (runOutput result)

testQualifiedMethodDispatchPrefersListAliasBodyForDirectLiteral :: IO ()
testQualifiedMethodDispatchPrefersListAliasBodyForDirectLiteral = do
  result <-
    runSource
      defaultWarningSettings
      ( """
        class RuntimeFlag(a) {
        flag :: a -> Bool.
        }.
        impl RuntimeFlag([Int]) {
        flag = \\(values) -> True.
        }.
        impl RuntimeFlag([Int64]) {
        flag = \\(values) -> False.
        }.
        (RuntimeFlag::flag) [1].
        """
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "True") (runOutput result)

testRawListPrependRehintsHeadToConcreteTailElementType :: IO ()
testRawListPrependRehintsHeadToConcreteTailElementType = do
  result <-
    runSource
      defaultWarningSettings
      ( """
        class RuntimeFlag(a) {
        flag :: a -> Bool.
        }.
        impl RuntimeFlag(Int) {
        flag = \\(itemValue) -> True.
        }.
        impl RuntimeFlag(Int64) {
        flag = \\(itemValue) -> False.
        }.
        headValue :: Int.
        headValue = 1.
        tailValues :: [Int64].
        tailValues = [2].
        case __kernel_listPrependRaw headValue tailValues {
        | [] -> True
        | [first | _] -> RuntimeFlag::flag first
        }.
        """
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "False") (runOutput result)

testQualifiedMethodDispatchPreservesBoundNestedListRuntimeHint :: IO ()
testQualifiedMethodDispatchPreservesBoundNestedListRuntimeHint = do
  result <-
    runSource
      defaultWarningSettings
      ( """
        class RuntimeFlag(a) {
        flag :: a -> Bool.
        }.
        impl RuntimeFlag([[Int]]) {
        flag = \\(values) -> True.
        }.
        impl RuntimeFlag([[Int64]]) {
        flag = \\(values) -> False.
        }.
        values = [[1], []].
        (RuntimeFlag::flag) values.
        """
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "False") (runOutput result)

testQualifiedMethodDispatchInstantiatesExplicitEmptyListTypeApplicationHint :: IO ()
testQualifiedMethodDispatchInstantiatesExplicitEmptyListTypeApplicationHint = do
  result <-
    runSource
      defaultWarningSettings
      ( """
        class RuntimeFlag(a) {
        flag :: a -> Bool.
        }.
        impl RuntimeFlag([Int]) {
        flag = \\(values) -> True.
        }.
        impl RuntimeFlag([Bool]) {
        flag = \\(values) -> False.
        }.
        empty = [].
        (RuntimeFlag::flag) (empty @Int).
        """
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "True") (runOutput result)

testQualifiedMethodDispatchInfersNestedListHelperExactSelection :: IO ()
testQualifiedMethodDispatchInfersNestedListHelperExactSelection = do
  result <-
    runSource
      defaultWarningSettings
      ( """
        class RuntimeFlag(a) {
        flag :: a -> Bool.
        }.
        impl RuntimeFlag([[Int]]) {
        flag = \\(values) -> True.
        }.
        impl RuntimeFlag([[Int64]]) {
        flag = \\(values) -> False.
        }.
        f = \\(x) -> RuntimeFlag::flag x.
        result = f [[1], []].
        result.
        """
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "True") (runOutput result)

testQualifiedMethodDispatchDoesNotExactMatchUntypedEmptyListLiteral :: IO ()
testQualifiedMethodDispatchDoesNotExactMatchUntypedEmptyListLiteral =
  assertEqual
    "untyped empty list exact match"
    False
    (runtimeValueExactlyMatchesConstraint (SemanticList (SemanticInt)) (VList [] Nothing))

testQualifiedMethodDispatchPrefersConstructorAliasBodyForDirectLiteral :: IO ()
testQualifiedMethodDispatchPrefersConstructorAliasBodyForDirectLiteral = do
  result <-
    runSource
      defaultWarningSettings
      ( """
        data Box a = Box a.
        class RuntimeFlag(a) {
        flag :: a -> Bool.
        }.
        impl RuntimeFlag(Box(Int)) {
        flag = \\(box) -> True.
        }.
        impl RuntimeFlag(Box(Int64)) {
        flag = \\(box) -> False.
        }.
        (RuntimeFlag::flag) (Box 1).
        """
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "True") (runOutput result)

testQualifiedMethodDispatchUsesStructuredConstructorPayloadForExactSelection :: IO ()
testQualifiedMethodDispatchUsesStructuredConstructorPayloadForExactSelection = do
  result <-
    runSource
      defaultWarningSettings
      ( """
        data Wrap a = Wrap Int64 a.
        class RuntimeFlag(a) {
        flag :: a -> Bool.
        }.
        impl RuntimeFlag(Wrap(Int)) {
        flag = \\(wrap) -> True.
        }.
        impl RuntimeFlag(Wrap(Int64)) {
        flag = \\(wrap) -> False.
        }.
        wrapped :: Wrap(Int).
        wrapped = Wrap 1 1.
        (RuntimeFlag::flag) wrapped.
        """
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "True") (runOutput result)

testQualifiedMethodDispatchPreservesInferredIntegerType :: IO ()
testQualifiedMethodDispatchPreservesInferredIntegerType = do
  result <-
    runSource
      defaultWarningSettings
      ( """
        class RuntimeFlag(a) {
        flag :: a -> Bool.
        }.
        impl RuntimeFlag(Int) {
        flag = \\(itemValue) -> True.
        }.
        impl RuntimeFlag(Int64) {
        flag = \\(itemValue) -> False.
        }.
        (RuntimeFlag::flag) ((\\(x) -> x) 1).
        """
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "True") (runOutput result)

testQualifiedMethodDispatchPreservesHigherOrderBindingSignature :: IO ()
testQualifiedMethodDispatchPreservesHigherOrderBindingSignature = do
  result <-
    runSource
      defaultWarningSettings
      ( """
        class RuntimeApply(a) {
        apply :: (a -> a) -> Bool.
        }.
        impl RuntimeApply(Int) {
        apply = \\(fn) -> True.
        }.
        impl RuntimeApply(Bool) {
        apply = \\(fn) -> False.
        }.
        idInt :: Int -> Int.
        idInt = \\(itemValue) -> itemValue.
        RuntimeApply::apply idInt.
        """
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "True") (runOutput result)

testQualifiedMethodDispatchPreservesHigherOrderExactSignature :: IO ()
testQualifiedMethodDispatchPreservesHigherOrderExactSignature = do
  result <-
    runSource
      defaultWarningSettings
      ( """
        class RuntimeApply(a) {
        apply :: (a -> a) -> Bool.
        }.
        impl RuntimeApply(Int) {
        apply = \\(fn) -> True.
        }.
        impl RuntimeApply(Int64) {
        apply = \\(fn) -> False.
        }.
        id64 :: Int64 -> Int64.
        id64 = \\(itemValue) -> itemValue.
        RuntimeApply::apply id64.
        """
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "False") (runOutput result)

testQualifiedMethodDispatchInfersFunctionArgumentExactSelection :: IO ()
testQualifiedMethodDispatchInfersFunctionArgumentExactSelection = do
  result <-
    runSource
      defaultWarningSettings
      ( """
        class RuntimeApply(a) {
        apply :: (a -> a) -> Bool.
        }.
        impl RuntimeApply(Int) {
        apply = \\(fn) -> True.
        }.
        impl RuntimeApply(Int64) {
        apply = \\(fn) -> False.
        }.
        (RuntimeApply::apply) (\\(itemValue) -> itemValue + 1).
        """
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "True") (runOutput result)

testQualifiedMethodDispatchDefersExactFilteringUntilTargetArgument :: IO ()
testQualifiedMethodDispatchDefersExactFilteringUntilTargetArgument = do
  result <-
    runSource
      defaultWarningSettings
      ( """
        class RuntimePick(a) {
        pick :: Int -> a -> Bool.
        }.
        impl RuntimePick(Int) {
        pick = \\(index, itemValue) -> False.
        }.
        impl RuntimePick(Bool) {
        pick = \\(index, itemValue) -> True.
        }.
        one :: Int.
        one = 1.
        pickOne = RuntimePick::pick one.
        pickOne True.
        """
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "True") (runOutput result)

testQualifiedMethodDispatchPreservesSelectedMethodSignature :: IO ()
testQualifiedMethodDispatchPreservesSelectedMethodSignature = do
  result <-
    runSource
      defaultWarningSettings
      ( """
        class Id(a) {
        id :: a -> a.
        }.
        impl Id(Int) {
        id = \\(itemValue) -> itemValue.
        }.
        class RuntimeApply(a) {
        apply :: (a -> a) -> Bool.
        }.
        impl RuntimeApply(Int) {
        apply = \\(fn) -> True.
        }.
        impl RuntimeApply(Bool) {
        apply = \\(fn) -> False.
        }.
        (RuntimeApply::apply) (Id::id @Int).
        """
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "True") (runOutput result)

testQualifiedMethodDispatchAppliesTypedCallableArgumentHint :: IO ()
testQualifiedMethodDispatchAppliesTypedCallableArgumentHint = do
  result <- runTypedCallablePlanCase "RuntimePick::pick" "choose 1"
  assertTypedCallablePlanResult "typed callable argument plan" result

testQualifiedMethodDispatchAppliesTypedCallableArgumentHintThroughPrefixDollar :: IO ()
testQualifiedMethodDispatchAppliesTypedCallableArgumentHintThroughPrefixDollar = do
  result <- runTypedCallablePlanCase "RuntimePick::pick" "($) choose 1"
  assertTypedCallablePlanResult "typed callable argument plan through prefix dollar" result

testQualifiedMethodDispatchAppliesClosureArgumentSignatureHint :: IO ()
testQualifiedMethodDispatchAppliesClosureArgumentSignatureHint = do
  result <- runTypedCallablePlanCase "\\(itemValue) -> RuntimePick::pick itemValue" "choose 1"
  assertTypedCallablePlanResult "closure argument signature plan" result

runTypedCallablePlanCase :: Text.Text -> Text.Text -> IO RunResult
runTypedCallablePlanCase chooseExpression resultExpression =
  runSource
    defaultWarningSettings
    ( "class RuntimePick(a) { pick :: a -> Bool. }.\n"
        <> "impl RuntimePick(Int) { pick = \\(itemValue) -> True. }.\n"
        <> "impl RuntimePick(UInt8) { pick = \\(itemValue) -> False. }.\n"
        <> "choose :: UInt8 -> Bool.\nchoose = "
        <> chooseExpression
        <> ".\n"
        <> resultExpression
        <> "."
    )

assertTypedCallablePlanResult :: Text.Text -> RunResult -> IO ()
assertTypedCallablePlanResult label result = do
  assertEqual (label <> " compile errors") [] (runCompileErrors result)
  assertEqual (label <> " runtime errors") [] (runRuntimeErrors result)
  assertEqual (label <> " output") (Just "False") (runOutput result)

testQualifiedMethodDispatchPreservesDefaultedClosureResultMetadata :: IO ()
testQualifiedMethodDispatchPreservesDefaultedClosureResultMetadata = do
  result <-
    runSource
      defaultWarningSettings
      ( """
        class RuntimePick(a) {
        pick :: a -> Bool.
        }.
        impl RuntimePick(Int) {
        pick = \\(itemValue) -> True.
        }.
        impl RuntimePick(UInt8) {
        pick = \\(itemValue) -> False.
        }.
        f = \\(itemValue) -> 1.
        result = RuntimePick::pick (f True).
        result.
        """
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "True") (runOutput result)

testQualifiedMethodDispatchPreservesEmptyListBindingSignature :: IO ()
testQualifiedMethodDispatchPreservesEmptyListBindingSignature = do
  result <-
    runSource
      defaultWarningSettings
      ( """
        class RuntimePick(a) {
        pick :: [a] -> Bool.
        }.
        impl RuntimePick(Int) {
        pick = \\(values) -> True.
        }.
        impl RuntimePick(Bool) {
        pick = \\(values) -> False.
        }.
        values :: [Int].
        values = [].
        RuntimePick::pick values.
        """
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "True") (runOutput result)

testQualifiedMethodDispatchPreservesListReturningApplicationSignature :: IO ()
testQualifiedMethodDispatchPreservesListReturningApplicationSignature = do
  result <-
    runSource
      defaultWarningSettings
      ( """
        class RuntimeFlag(a) {
        flag :: a -> Bool.
        }.
        impl RuntimeFlag([[Int]]) {
        flag = \\(values) -> True.
        }.
        impl RuntimeFlag([[Int64]]) {
        flag = \\(values) -> False.
        }.
        make :: Bool -> [[Int64]].
        make = \\(enabled) -> [[1], []].
        (RuntimeFlag::flag) (make True).
        """
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "False") (runOutput result)

testQualifiedMethodDispatchPreservesDollarAppliedListReturningSignature :: IO ()
testQualifiedMethodDispatchPreservesDollarAppliedListReturningSignature = do
  result <-
    runSource
      defaultWarningSettings
      ( """
        class RuntimeFlag(a) {
        flag :: a -> Bool.
        }.
        impl RuntimeFlag([[Int]]) {
        flag = \\(values) -> True.
        }.
        impl RuntimeFlag([[Int64]]) {
        flag = \\(values) -> False.
        }.
        make :: Bool -> [[Int64]].
        make = \\(enabled) -> [[1], []].
        (RuntimeFlag::flag) (($) make True).
        """
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "False") (runOutput result)

testQualifiedMethodDispatchPreservesAdtReturningApplicationSignature :: IO ()
testQualifiedMethodDispatchPreservesAdtReturningApplicationSignature = do
  result <-
    runSource
      defaultWarningSettings
      ( """
        data Box a = Box a.
        class RuntimeFlag(a) {
        flag :: a -> Bool.
        }.
        impl RuntimeFlag(Box([[Int]])) {
        flag = \\(box) -> True.
        }.
        impl RuntimeFlag(Box([[Int64]])) {
        flag = \\(box) -> False.
        }.
        make = \\(enabled) -> if enabled then (Box [[toInt64 1], []]) else (Box [[toInt64 2], []]).
        (RuntimeFlag::flag) (make True).
        """
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "False") (runOutput result)

testQualifiedMethodDispatchPreservesBranchResultSignature :: IO ()
testQualifiedMethodDispatchPreservesBranchResultSignature = do
  result <-
    runSource
      defaultWarningSettings
      ( """
        class RuntimeFlag(a) {
        flag :: a -> Bool.
        }.
        impl RuntimeFlag([[Int]]) {
        flag = \\(values) -> True.
        }.
        impl RuntimeFlag([[Int64]]) {
        flag = \\(values) -> False.
        }.
        make64 :: Bool -> [[Int64]].
        make64 = \\(enabled) -> [[1], []].
        (RuntimeFlag::flag) (if True then (make64 True) else (make64 False)).
        """
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "False") (runOutput result)

testQualifiedMethodDispatchPreservesBlockResultSignature :: IO ()
testQualifiedMethodDispatchPreservesBlockResultSignature = do
  result <-
    runSource
      defaultWarningSettings
      ( """
        class RuntimeFlag(a) {
        flag :: a -> Bool.
        }.
        impl RuntimeFlag([[Int]]) {
        flag = \\(values) -> True.
        }.
        impl RuntimeFlag([[Int64]]) {
        flag = \\(values) -> False.
        }.
        (RuntimeFlag::flag) {
        values :: [[Int64]].
        values = [[1], []].
        values.
        }.
        """
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "False") (runOutput result)

testQualifiedMethodDispatchPreservesMappedEmptyListResultSignature :: IO ()
testQualifiedMethodDispatchPreservesMappedEmptyListResultSignature = do
  result <-
    runSource
      defaultWarningSettings
      ( """
        class RuntimePick(a) {
        pick :: [a] -> Bool.
        }.
        impl RuntimePick(Int) {
        pick = \\(values) -> True.
        }.
        impl RuntimePick(UInt8) {
        pick = \\(values) -> False.
        }.
        id8 :: UInt8 -> UInt8.
        id8 = \\(itemValue) -> itemValue.
        values :: [UInt8].
        values = [].
        mapped = map id8 values.
        RuntimePick::pick mapped.
        """
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "False") (runOutput result)

testQualifiedMethodDispatchPreservesIdentityMappedEmptyListResultSignature :: IO ()
testQualifiedMethodDispatchPreservesIdentityMappedEmptyListResultSignature = do
  result <-
    runSource
      defaultWarningSettings
      ( """
        class RuntimePick(a) {
        pick :: [a] -> Bool.
        }.
        impl RuntimePick(Int) {
        pick = \\(values) -> True.
        }.
        impl RuntimePick(UInt8) {
        pick = \\(values) -> False.
        }.
        values :: [UInt8].
        values = [].
        mapped = map (\\(itemValue) -> itemValue) values.
        RuntimePick::pick mapped.
        """
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "False") (runOutput result)

testQualifiedMethodDispatchPreservesMappedHdEmptyNestedListResultSignature :: IO ()
testQualifiedMethodDispatchPreservesMappedHdEmptyNestedListResultSignature = do
  result <-
    runSource
      defaultWarningSettings
      ( """
        class RuntimePick(a) {
        pick :: [a] -> Bool.
        }.
        impl RuntimePick(Int) {
        pick = \\(values) -> True.
        }.
        impl RuntimePick(UInt8) {
        pick = \\(values) -> False.
        }.
        values :: [[UInt8]].
        values = [].
        mapped = map hd values.
        RuntimePick::pick mapped.
        """
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "False") (runOutput result)

testQualifiedMethodDispatchPreservesHdElementSignature :: IO ()
testQualifiedMethodDispatchPreservesHdElementSignature = do
  result <-
    runSource
      defaultWarningSettings
      ( """
        class RuntimeEq(a) {
        equals :: a -> a -> Bool.
        }.
        impl RuntimeEq(Int) {
        equals = \\(left, right) -> True.
        }.
        impl RuntimeEq(UInt8) {
        equals = \\(left, right) -> False.
        }.
        values :: [UInt8].
        values = [1].
        left = hd values.
        right = hd values.
        RuntimeEq::equals left right.
        """
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "False") (runOutput result)

testQualifiedMethodDispatchNormalizesHintedListAliases :: IO ()
testQualifiedMethodDispatchNormalizesHintedListAliases = do
  result <-
    runSource
      defaultWarningSettings
      ( """
        class RuntimePick(a) {
        pick :: [a] -> Bool.
        }.
        impl RuntimePick(Int64) {
        pick = \\(values) -> True.
        }.
        impl RuntimePick(Bool) {
        pick = \\(values) -> False.
        }.
        values :: [Int].
        values = [].
        RuntimePick::pick values.
        """
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "True") (runOutput result)

testQualifiedMethodDispatchNormalizesHintedFunctionAliases :: IO ()
testQualifiedMethodDispatchNormalizesHintedFunctionAliases = do
  result <-
    runSource
      defaultWarningSettings
      ( """
        class RuntimeApply(a) {
        apply :: (a -> a) -> Bool.
        }.
        impl RuntimeApply(Int64) {
        apply = \\(fn) -> True.
        }.
        impl RuntimeApply(Bool) {
        apply = \\(fn) -> False.
        }.
        idInt :: Int -> Int.
        idInt = \\(itemValue) -> itemValue.
        RuntimeApply::apply idInt.
        """
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "True") (runOutput result)

testQualifiedMethodDispatchTreatsDefaultedIntegerBindingAsInt64 :: IO ()
testQualifiedMethodDispatchTreatsDefaultedIntegerBindingAsInt64 = do
  result <-
    runSource
      defaultWarningSettings
      ( """
        class RuntimePick(a) {
        pick :: a -> Bool.
        }.
        impl RuntimePick(Int) {
        pick = \\(itemValue) -> True.
        }.
        impl RuntimePick(UInt8) {
        pick = \\(itemValue) -> False.
        }.
        itemValue = 1.
        RuntimePick::pick itemValue.
        """
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "True") (runOutput result)

testQualifiedMethodDispatchTreatsPlainIntegerBindingAsInt64WithExactCandidates :: IO ()
testQualifiedMethodDispatchTreatsPlainIntegerBindingAsInt64WithExactCandidates = do
  result <-
    runSource
      defaultWarningSettings
      ( """
        class RuntimeFlag(a) {
        flag :: a -> Bool.
        }.
        impl RuntimeFlag(Int) {
        flag = \\(itemValue) -> True.
        }.
        impl RuntimeFlag(Int64) {
        flag = \\(itemValue) -> False.
        }.
        itemValue = 1.
        RuntimeFlag::flag itemValue.
        """
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "False") (runOutput result)

testQualifiedMethodDispatchTreatsInferredDirectIntegerLiteralAsExactInt :: IO ()
testQualifiedMethodDispatchTreatsInferredDirectIntegerLiteralAsExactInt = do
  result <-
    runSource
      defaultWarningSettings
      ( """
        class RuntimeFlag(a) {
        flag :: a -> Bool.
        }.
        impl RuntimeFlag(Int) {
        flag = \\(itemValue) -> True.
        }.
        impl RuntimeFlag(Int64) {
        flag = \\(itemValue) -> False.
        }.
        result = (\\(itemValue) -> RuntimeFlag::flag itemValue) 1.
        result.
        """
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "True") (runOutput result)

testQualifiedMethodDispatchPreservesInferredNarrowIntegerBinding :: IO ()
testQualifiedMethodDispatchPreservesInferredNarrowIntegerBinding = do
  result <-
    runSource
      defaultWarningSettings
      ( """
        class RuntimePick(a) {
        pick :: a -> Bool.
        }.
        impl RuntimePick(Int) {
        pick = \\(itemValue) -> True.
        }.
        impl RuntimePick(UInt8) {
        pick = \\(itemValue) -> False.
        }.
        itemValue = if True then 1 else toUInt8 2.
        RuntimePick::pick itemValue.
        """
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "False") (runOutput result)

testQualifiedMethodDispatchPreservesAdtApplicationBindingHint :: IO ()
testQualifiedMethodDispatchPreservesAdtApplicationBindingHint = do
  result <-
    runSource
      defaultWarningSettings
      ( """
        data Box a = Box a.
        class RuntimePick(a) {
        pick :: a -> Bool.
        }.
        impl RuntimePick(Box(Int)) {
        pick = \\(box) -> True.
        }.
        impl RuntimePick(Box(UInt8)) {
        pick = \\(box) -> False.
        }.
        box = if True then (Box 1) else (Box (toUInt8 2)).
        RuntimePick::pick box.
        """
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "False") (runOutput result)

testQualifiedMethodDispatchPreservesPhantomAdtApplicationBindingHint :: IO ()
testQualifiedMethodDispatchPreservesPhantomAdtApplicationBindingHint = do
  result <-
    runSource
      defaultWarningSettings
      """
      data Tag a = Tag.
      class RuntimePick(a) { pick :: a -> Bool. }.
      impl RuntimePick(Tag(Int)) { pick = \\(tag) -> True. }.
      impl RuntimePick(Tag(UInt8)) { pick = \\(tag) -> False. }.
      tag :: Tag(UInt8).
      tag = Tag.
      RuntimePick::pick tag.
      """
  assertTypedCallablePlanResult "phantom ADT application plan" result

testQualifiedMethodDispatchPreservesAdtConcretePayloadHint :: IO ()
testQualifiedMethodDispatchPreservesAdtConcretePayloadHint = do
  result <-
    runSource
      defaultWarningSettings
      """
      data Box a = Box Float32 a.
      class RuntimePick(a) { pick :: a -> Bool. }.
      impl RuntimePick(Box(UInt8)) { pick = \\(box) -> False. }.
      box :: Box(UInt8).
      box = Box 1.5f32 (toUInt8 2).
      RuntimePick::pick box.
      """
  assertTypedCallablePlanResult "ADT concrete payload plan" result

testQualifiedMethodDispatchPreservesMonomorphicAdtConcretePayloadHint :: IO ()
testQualifiedMethodDispatchPreservesMonomorphicAdtConcretePayloadHint = do
  result <-
    runSource
      defaultWarningSettings
      """
      data Token = Token UInt8.
      class RuntimePick(a) { pick :: a -> Bool. }.
      impl RuntimePick(Int) { pick = \\(itemValue) -> True. }.
      impl RuntimePick(UInt8) { pick = \\(itemValue) -> False. }.
      token = Token 1.
      case token { | Token itemValue -> RuntimePick::pick itemValue }.
      """
  assertTypedCallablePlanResult "monomorphic ADT concrete payload plan" result

testQualifiedMethodDispatchKeepsNestedInferredHintsScoped :: IO ()
testQualifiedMethodDispatchKeepsNestedInferredHintsScoped = do
  result <-
    runSourceWithPrelude
      defaultWarningSettings
      Nothing
      ( """
        dummy = 0.
        z = 1.
        x = { y :: UInt8.
        y = 1.
        y. }.
        class RuntimePick(a) {
        pick :: a -> Bool.
        }.
        impl RuntimePick(Int) {
        pick = \\(itemValue) -> True.
        }.
        impl RuntimePick(UInt8) {
        pick = \\(itemValue) -> False.
        }.
        RuntimePick::pick z.
        """
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "True") (runOutput result)

testAuthoredModuleTransitionOwnsFactsAndEvidence :: IO ()
testAuthoredModuleTransitionOwnsFactsAndEvidence = do
  (inference, analyzedExpression) <-
    analyzeRuntimeFacts
      """
      module App::Main {
      class RuntimePick(a) {
      pick :: a -> Bool.
      }.
      impl RuntimePick(Int) {
      pick = \\(itemValue) -> True.
      }.
      owned = 1.
      RuntimePick::pick owned.
      }
      """
  assertEqual "authored-module inference errors" [] (filter isErrorDiagnostic (inferredDiagnostics inference))
  let authoredPath = mkModulePath (mkIdentifier "App" NonEmpty.:| [mkIdentifier "Main"])
      implementationIds =
        [ ImplId (NamedSourceUnit authoredPath, coreNodeId node)
        | SImpl node _ _ _ _ <- sourceUnitStatements analyzedExpression
        ]
      selectedEvidence = expressionEvidenceInventory analyzedExpression
  assertEqual "one authored implementation" 1 (length implementationIds)
  assertEqual
    "qualified method evidence uses the authored module implementation id"
    implementationIds
    ([identity | EvidenceReference {evidenceImplementation = identity} <- selectedEvidence])

sourceUnitStatements :: Expr 'Analyzed -> [Statement 'Analyzed]
sourceUnitStatements expression =
  case expression of
    EBlock _ statements -> statements
    _ -> []

expressionEvidenceInventory :: Expr 'Analyzed -> [EvidenceReference]
expressionEvidenceInventory = foldMap (\(evidence, _, _) -> evidence) . expressionEvidenceFactsInventory

expressionEvidenceFactsInventory :: Expr 'Analyzed -> [([EvidenceReference], [NonEmpty.NonEmpty AnalyzedType], Maybe AnalyzedType)]
expressionEvidenceFactsInventory expression =
  nodeEvidenceFacts expression
    <> case expression of
      ELambda _ _ body -> expressionEvidenceFactsInventory body
      EList _ elements -> foldMap expressionEvidenceFactsInventory elements
      ETuple _ elements -> foldMap expressionEvidenceFactsInventory elements
      EApply _ function argument -> expressionEvidenceFactsInventory function <> expressionEvidenceFactsInventory argument
      ETypeApplication _ function _ _ -> expressionEvidenceFactsInventory function
      EIf _ condition whenTrue whenFalse -> foldMap expressionEvidenceFactsInventory [condition, whenTrue, whenFalse]
      EPatternCase _ scrutinee arms -> expressionEvidenceFactsInventory scrutinee <> foldMap armEvidenceFacts arms
      EBinary _ _ left right -> expressionEvidenceFactsInventory left <> expressionEvidenceFactsInventory right
      ESectionLeft _ left _ -> expressionEvidenceFactsInventory left
      ESectionRight _ _ right -> expressionEvidenceFactsInventory right
      EBlock _ statements -> foldMap statementEvidenceFacts statements
      _ -> []
  where
    nodeEvidenceFacts value =
      case value of
        ELit (CoreNode _ _ facts) _ -> plan facts
        EVar (CoreNode _ _ facts) _ -> plan facts
        ELambda (CoreNode _ _ facts) _ _ -> plan facts
        EOperatorValue (CoreNode _ _ facts) _ -> plan facts
        EList (CoreNode _ _ facts) _ -> plan facts
        ETuple (CoreNode _ _ facts) _ -> plan facts
        EApply (CoreNode _ _ facts) _ _ -> plan facts
        ETypeApplication (CoreNode _ _ facts) _ _ _ -> plan facts
        EIf (CoreNode _ _ facts) _ _ _ -> plan facts
        EPatternCase (CoreNode _ _ facts) _ _ -> plan facts
        EBinary (CoreNode _ _ facts) _ _ _ -> plan facts
        ESectionLeft (CoreNode _ _ facts) _ _ -> plan facts
        ESectionRight (CoreNode _ _ facts) _ _ -> plan facts
        EBlock (CoreNode _ _ facts) _ -> plan facts
    plan facts =
      case expressionEvidence facts of
        [] -> []
        evidence ->
          [(evidence, map instantiatedTypes (expressionInstantiations facts), expressionResultRepresentation facts)]
    armEvidenceFacts (CaseArm (CoreNode _ _ facts) _ guard body) =
      plan facts <> foldMap expressionEvidenceFactsInventory guard <> expressionEvidenceFactsInventory body
    statementEvidenceFacts statement =
      case statement of
        SLet _ _ value -> expressionEvidenceFactsInventory value
        SImpl _ _ _ methods _ -> foldMap (\(ImplMethod _ _ body) -> expressionEvidenceFactsInventory body) methods
        SExpr _ value -> expressionEvidenceFactsInventory value
        _ -> []

analyzeRuntimeFacts :: Text.Text -> IO (InferenceResult, Expr 'Analyzed)
analyzeRuntimeFacts source = do
  expression <-
    case parseAndLowerStandaloneSource source of
      Left diagnostic ->
        failTest ("runtime-facts fixture failed to lower: " <> renderDiagnostic diagnostic)
      Right lowered -> pure (resolveStandaloneExprNames (exportInventory []) lowered)
  (inference, attachment) <-
    analyzeResolvedExpression
      defaultWarningSettings
      expression
  analyzedExpression <-
    case attachment of
      Left failures ->
        failTest ("analyzed runtime-facts attachment failed: " <> Text.pack (show failures))
      Right Nothing ->
        failTest ("analyzed runtime-facts attachment produced no expression: " <> Text.unlines (map renderDiagnostic (inferredDiagnostics inference)))
      Right (Just analyzed) -> pure analyzed
  pure (inference, analyzedExpression)

testQualifiedMethodDispatchPrefersAliasBindingOverMethodSentinelAtRuntime :: IO ()
testQualifiedMethodDispatchPrefersAliasBindingOverMethodSentinelAtRuntime = do
  let result =
        evaluateFixture
          ( runtimeExpr
              ( expressionBlock
                  [ statementLet "Eq::helper" (SourceSpan 1 1) (expressionLambda "itemValue" (expressionLiteral (LBool True))),
                    statementClass
                      (SourceSpan 2 1)
                      "Eq"
                      ["a"]
                      [ classMethodSignature
                          "helper"
                          (SourceSpan 3 1)
                          ( ConstrainedSignature
                              []
                              (TypeFunction (fixtureTypeVariable "a") (TypeBool))
                          )
                      ],
                    statementImpl
                      (SourceSpan 4 1)
                      "Eq"
                      [TypeInt]
                      [implMethod "helper" (SourceSpan 5 1) (expressionLambda "itemValue" (expressionLiteral (LBool False)))],
                    statementExpression
                      (SourceSpan 6 1)
                      (expressionApply (expressionVariable "Eq::helper") (expressionLiteral (LInt 1)))
                  ]
              )
          )
  assertRuntimeBool "alias binding runtime result" True result

testQualifiedZeroArgumentMethodDispatchReturnsValue :: IO ()
testQualifiedZeroArgumentMethodDispatchReturnsValue = do
  result <-
    runSource
      defaultWarningSettings
      ( """
        class RuntimeFlag(a) {
        enabled :: Bool.
        }.
        impl RuntimeFlag(Int) {
        enabled = True.
        }.
        (RuntimeFlag::enabled @Int).
        """
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "True") (runOutput result)

testQualifiedMethodDispatchRejectsDirectSelfAlias :: IO ()
testQualifiedMethodDispatchRejectsDirectSelfAlias = do
  maybeResult <-
    timeout
      1000000
      ( try
          ( runSource
              defaultWarningSettings
              ( """
                class RuntimeEq(a) {
                equals :: a -> a -> Bool.
                }.
                impl RuntimeEq(Int) {
                equals = RuntimeEq::equals.
                }.
                RuntimeEq::equals 1 1.
                """
              )
          ) ::
          IO (Either SomeException RunResult)
      )
  case maybeResult of
    Nothing ->
      failTest "expected direct qualified method self alias to terminate with a runtime diagnostic, but evaluation timed out"
    Just (Left err) ->
      failTest ("expected deterministic runtime diagnostic for direct qualified method self alias, but evaluation raised " <> Text.pack (show err))
    Just (Right result) -> do
      assertEqual "compile errors" [] (runCompileErrors result)
      assertSingleDiagnosticContains
        "direct qualified method self alias runtime code"
        "E3021"
        (runRuntimeErrors result)
      assertSingleDiagnosticContains
        "direct qualified method self alias runtime text"
        "recursive dictionary binding"
        (runRuntimeErrors result)
      assertEqual "runtime output is suppressed on runtime failure" Nothing (runOutput result)

testQualifiedMethodDispatchRejectsWrappedSelfAlias :: IO ()
testQualifiedMethodDispatchRejectsWrappedSelfAlias = do
  maybeResult <-
    timeout
      1000000
      ( try
          ( runSource
              defaultWarningSettings
              ( """
                class RuntimeEq(a) {
                equals :: a -> a -> Bool.
                }.
                impl RuntimeEq(Int) {
                equals = if True then RuntimeEq::equals else \\(left, right) -> left == right.
                }.
                RuntimeEq::equals 1 1.
                """
              )
          ) ::
          IO (Either SomeException RunResult)
      )
  case maybeResult of
    Nothing ->
      failTest "expected wrapped qualified method self alias to terminate with a runtime diagnostic, but evaluation timed out"
    Just (Left err) ->
      failTest ("expected deterministic runtime diagnostic for wrapped qualified method self alias, but evaluation raised " <> Text.pack (show err))
    Just (Right result) -> do
      assertEqual "compile errors" [] (runCompileErrors result)
      assertSingleDiagnosticContains
        "wrapped qualified method self alias runtime code"
        "E3021"
        (runRuntimeErrors result)
      assertSingleDiagnosticContains
        "wrapped qualified method self alias runtime text"
        "recursive dictionary binding"
        (runRuntimeErrors result)
      assertEqual "runtime output is suppressed on runtime failure" Nothing (runOutput result)

testQualifiedMethodDispatchRejectsBlockLocalSelfAlias :: IO ()
testQualifiedMethodDispatchRejectsBlockLocalSelfAlias = do
  maybeResult <-
    timeout
      1000000
      ( try
          ( runSource
              defaultWarningSettings
              ( """
                class RuntimeFlag(a) {
                enabled :: Bool.
                }.
                impl RuntimeFlag(Int) {
                enabled = { helper = (RuntimeFlag::enabled @Int).
                helper. }.
                }.
                (RuntimeFlag::enabled @Int).
                """
              )
          ) ::
          IO (Either SomeException RunResult)
      )
  case maybeResult of
    Nothing ->
      failTest "expected block-local qualified method self alias to terminate with a runtime diagnostic, but evaluation timed out"
    Just (Left err) ->
      failTest ("expected deterministic runtime diagnostic for block-local qualified method self alias, but evaluation raised " <> Text.pack (show err))
    Just (Right result) -> do
      assertEqual "compile errors" [] (runCompileErrors result)
      assertSingleDiagnosticContains
        "block-local qualified method self alias runtime code"
        "E3021"
        (runRuntimeErrors result)
      assertSingleDiagnosticContains
        "block-local qualified method self alias runtime text"
        "recursive dictionary binding"
        (runRuntimeErrors result)
      assertEqual "runtime output is suppressed on runtime failure" Nothing (runOutput result)

testQualifiedMethodDispatchFollowsBlockLocalAliasBranchesWithLocalBindings :: IO ()
testQualifiedMethodDispatchFollowsBlockLocalAliasBranchesWithLocalBindings = do
  result <-
    runSource
      defaultWarningSettings
      ( """
        class RuntimeFlag(a) {
        enabled :: Bool.
        on :: Bool.
        off :: Bool.
        }.
        impl RuntimeFlag(Int) {
        enabled = { flag = True.
        target = if flag then (RuntimeFlag::on @Int) else (RuntimeFlag::off @Int).
        target.
        }.
        on = True.
        off = False.
        }.
        (RuntimeFlag::enabled @Int).
        """
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "True") (runOutput result)

testQualifiedMethodDispatchFollowsBlockLocalAliasBranchesWithLocalSignatureHints :: IO ()
testQualifiedMethodDispatchFollowsBlockLocalAliasBranchesWithLocalSignatureHints = do
  result <-
    runSource
      defaultWarningSettings
      ( """
        class RuntimeFlag(a) {
        flag :: a -> Bool.
        }.
        impl RuntimeFlag([[Int]]) {
        flag = \\(values) -> False.
        }.
        impl RuntimeFlag([[Int64]]) {
        flag = \\(values) -> True.
        }.
        class RuntimeChoice(a) {
        enabled :: Bool.
        on :: Bool.
        off :: Bool.
        }.
        impl RuntimeChoice(Int) {
        enabled = { itemValue :: [[Int64]].
        itemValue = [[1], []].
        target = if ((RuntimeFlag::flag) itemValue) then (RuntimeChoice::on @Int) else (RuntimeChoice::off @Int).
        target.
        }.
        on = True.
        off = False.
        }.
        (RuntimeChoice::enabled @Int).
        """
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "True") (runOutput result)

testQualifiedMethodDispatchRejectsFullArityRuntimeAmbiguity :: IO ()
testQualifiedMethodDispatchRejectsFullArityRuntimeAmbiguity =
  assertRuntimeErrorContains
    "fully applied ambiguous qualified method"
    "missing or inconsistent checked capability evidence"
    (evaluateFixture ambiguousQualifiedMethodRuntimeExpr)

testQualifiedMethodDispatchExecutesLocalAdtImplBody :: IO ()
testQualifiedMethodDispatchExecutesLocalAdtImplBody = do
  result <-
    runSource
      defaultWarningSettings
      ( runtimeEqSource
          <> """
             data Token = Token Int.
             data Box a = Box a.
             impl RuntimeEq(Token) {
             equals = \\(left, right) -> True.
             }.
             impl RuntimeEq(Box(Int)) {
             equals = \\(left, right) -> True.
             }.
             result = (RuntimeEq::equals (Token 1) (Token 2), RuntimeEq::equals (Box 1) (Box 2)).
             result.
             """
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "(True, True)") (runOutput result)

testMethodBearingCapabilityDeclarationsRuntimeInert :: IO ()
testMethodBearingCapabilityDeclarationsRuntimeInert = do
  result <-
    runSource
      defaultWarningSettings
      ( runtimeEqSource
          <> """
             x = 1.
             x.
             """
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "method-bearing capability declarations do not affect runtime output" (Just "1") (runOutput result)

testGenericDefaultsAndAliases :: IO ()
testGenericDefaultsAndAliases = do
  result <-
    runSource
      defaultWarningSettings
      """
      privateHelper = \\(x) -> if x then False else True.
      class Same(a) {
        same :: a -> a -> Bool.
        different :: a -> a -> Bool.
        different = \\(left, right) -> privateHelper (same left right).
      }.
      impl Same(Int) { same = \\(x,y) -> x == y. }.
      impl Same(Bool) {
        same = \\(x,y) -> x == y.
        different = \\(x,y) -> False.
      }.
      impl @{Same(a)}: Same([a]) {
        same = \\(left,right) -> case (left,right) {
          | ([],[]) -> True
          | ([x | xs], [y | ys]) -> if same x y then same xs ys else False
          | _ -> False
        }.
      }.
      check = \\(x,y) -> same x y.
      saved = different.
      shadow = { same = \\(x,y) -> 42. same 1 2. }.
      (check [[1]] [[1]], check [[1]] [[2]], saved 1 2, saved True False, different [1] [2], shadow).
      """
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "(True, False, True, False, True, 42)") (runOutput result)

testGenericConstructorMapping :: IO ()
testGenericConstructorMapping = do
  result <-
    runSource
      defaultWarningSettings
      """
      class Transforming(f) { transform :: (a -> b) -> f(a) -> f(b). }.
      impl Transforming(List) {
        transform = \\(change, xs) -> case xs {
          | [] -> []
          | [x | rest] -> __kernel_listPrependRaw (change x) (Transforming::transform change rest)
        }.
      }.
      data Result error a = Error error | Success a.
      impl Transforming(Result(error)) {
        transform = \\(change, result) -> case result {
          | Error problem -> Error problem
          | Success x -> Success (change x)
        }.
      }.
      success :: Result(Text, Int).
      success = Success 1.
      (Transforming::transform (\\(x) -> x == 1) [1, 2], Transforming::transform (\\(x) -> x == 1) (Error "bad"), Transforming::transform (\\(x) -> x == 1) success).
      """
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "([True, False], Error(\"bad\"), Success(True))") (runOutput result)

testSuperclassAndAliasEvidence :: IO ()
testSuperclassAndAliasEvidence = do
  result <-
    runSourceWithPrelude
      defaultWarningSettings
      Nothing
      """
      data Box a = Box a.
      class Parent(a) { parentSame :: a -> a -> Bool. }.
      impl Parent(Int) { parentSame = \\(x, y) -> x == y. }.
      impl @{Parent(a)}: Parent(Box(a)) { parentSame = \\(Box x, Box y) -> parentSame x y. }.
      class @{Parent(a)}: Child(a) {
        childSame :: a -> a -> Bool.
        childSame = \\(x, y) -> parentSame x y.
      }.
      impl Child(Int) { }.
      impl @{Child(a)}: Child(Box(a)) { }.
      check :: @{Child(a)}: a -> a -> Bool.
      check = \\(x, y) -> if childSame x y then parentSame x y else False.
      class Transforming(f) { transform :: (a -> b) -> f(a) -> f(b). }.
      impl Transforming(Box) { transform = \\(change, Box x) -> Box (change x). }.
      saved = transform.
      (check (Box 1) (Box 1), check (Box 1) (Box 2), saved @Box (\\(x) -> x == 1) (Box 1)).
      """
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "(True, False, Box(True))") (runOutput result)

testRecursiveHelperEvidence :: IO ()
testRecursiveHelperEvidence = do
  result <-
    runSourceWithPrelude
      defaultWarningSettings
      Nothing
      """
      class Same(a) { same :: a -> Bool. }.
      impl Same(Int) { same = \\(x) -> True. }.
      impl Same(Bool) { same = \\(x) -> False. }.
      signed :: @{Same(a)}: [a] -> Bool.
      signed = \\(xs) -> case xs { | [] -> True | [x | rest] -> if same x then signed rest else False }.
      first = \\(xs) -> case xs { | [] -> True | [x | rest] -> if same x then second rest else False }.
      second = \\(xs) -> case xs { | [] -> True | [x | rest] -> if same x then first rest else False }.
      (signed [1,2,3], signed [True], first [1,2,3,4], second [False]).
      """
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "(True, False, True, False)") (runOutput result)

testMethodLocalTraversal :: IO ()
testMethodLocalTraversal = do
  result <-
    runSourceWithPrelude
      defaultWarningSettings
      Nothing
      """
      data Box a = Box a.
      data Wrap a = Wrap a.
      class Transforming(f) { transform :: (a -> b) -> f(a) -> f(b). }.
      impl Transforming(Wrap) { transform = \\(change, Wrap x) -> Wrap (change x). }.
      class Traversing(f) {
        traverse :: @{Transforming(g)}: (a -> g(b)) -> f(a) -> g(f(b)).
      }.
      impl Traversing(Box) {
        traverse = \\(change, Box x) -> transform Box (change x).
      }.
      (traverse (\\(x) -> Wrap (x == 1)) (Box 1),
       traverse (\\(x) -> Wrap [x]) (Box True)).
      """
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "(Wrap(Box(True)), Wrap(Box([True])))") (runOutput result)
