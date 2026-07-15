{-# LANGUAGE OverloadedStrings #-}

module JazzNext.Compiler.Semantics.Runtime.CapabilitiesTests
  ( capabilityTests
  ) where

import Control.Exception
  ( SomeException,
    try
  )
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import JazzNext.Compiler.AST
  ( CaseArm (..),
    ClassMethodSignature (..),
    SignatureType (..),
    DataConstructorArgument (..),
    DataConstructor (..),
    Expr (..),
    ImplMethod (..),
    Literal (..),
    NumericType (..),
    Pattern (..),
    SignaturePayload (..),
    Statement (..)
  )
import JazzNext.Compiler.Diagnostics
  ( SourceSpan (..),
    isErrorDiagnostic
  )
import JazzNext.Compiler.Diagnostics.Render
  ( renderDiagnostic
  )
import JazzNext.Compiler.BuiltinCatalog
  ( BuiltinResolutionMode (..)
  )
import JazzNext.Compiler.Driver
  ( RunResult (..),
    runCompileErrors,
    runRuntimeErrors,
    runSource,
    runSourceWithPrelude
  )
import JazzNext.Compiler.FractionalLiteral
  ( mkFractionalLiteralSource
  )
import JazzNext.Compiler.Name (qualifiedName)
import JazzNext.Compiler.Runtime
  ( RuntimeValue (..),
    evaluateRuntimeExpr,
    evaluateRuntimeExprWithBuiltinsAndBindingHints,
    renderRuntimeValue,
    runtimeValueExactlyMatchesConstraint
  )
import JazzNext.Compiler.RuntimeHints
  ( bindingRuntimeHintKey,
    bindingRuntimeHintKeyInModule,
    explicitTypeApplicationRuntimeHintKeyInModule
  )
import JazzNext.Compiler.TypeInference
  ( InferenceResult (..),
    inferExpressionWithBuiltins,
    inferExpressionWithBuiltinsAndSourceUnitStatements
  )
import JazzNext.Compiler.WarningConfig
  ( defaultWarningSettings
  )
import JazzNext.TestHarness
  ( NamedTest,
    assertContains,
    assertEqual,
    assertSingleDiagnosticContains,
    failTest
  )
import System.Timeout
  ( timeout
  )
import JazzNext.Compiler.Semantics.Runtime.Shared

capabilityTests :: [NamedTest]
capabilityTests =
  [ ("runtime fallback rejects structural equality over qualified methods", testRuntimeFallbackRejectsQualifiedMethodStructuralEquality)
    , ("scope with only capability declarations has no runtime output", testCapabilityDeclarationOnlyScopeHasNoOutput)
    , ("capability declarations are inert at runtime", testCapabilityDeclarationsRuntimeInert)
    , ("qualified method candidates carry compiler-owned runtime evidence", testQualifiedMethodCandidateCarriesRuntimeEvidence)
    , ("qualified method dispatch executes selected impl body", testQualifiedMethodDispatchExecutesImplBody)
    , ("let-bound qualified method dispatch executes selected impl body", testLetBoundQualifiedMethodDispatchExecutesImplBody)
    , ("qualified method dispatch selects runtime body by argument types", testQualifiedMethodDispatchSelectsRuntimeBodyByArgumentTypes)
    , ("qualified method dispatch executes same-impl qualified method call", testQualifiedMethodDispatchExecutesSameImplQualifiedMethodCall)
    , ("qualified method dispatch selects width-specific integer body", testQualifiedMethodDispatchSelectsWidthSpecificIntegerBody)
    , ("qualified method dispatch selects width-specific integer body for direct literals", testQualifiedMethodDispatchSelectsWidthSpecificIntegerBodyForDirectLiterals)
    , ("qualified method dispatch preserves direct explicit type application hints", testQualifiedMethodDispatchPreservesDirectExplicitTypeApplicationHint)
    , ("qualified method dispatch preserves inferred explicit type application tuple hints", testQualifiedMethodDispatchPreservesInferredExplicitTypeApplicationTupleHint)
    , ("qualified method dispatch applies explicit type argument to matching parameter", testQualifiedMethodDispatchAppliesExplicitTypeArgumentToMatchingParameter)
    , ("qualified method dispatch preserves partially instantiated function templates", testQualifiedMethodDispatchPreservesPartiallyInstantiatedFunctionTemplate)
    , ("qualified method dispatch preserves non-literal integer signature targets", testQualifiedMethodDispatchPreservesNonLiteralIntegerSignatureTarget)
    , ("qualified method dispatch preserves direct closure result signatures", testQualifiedMethodDispatchPreservesDirectClosureResultSignature)
    , ("qualified method dispatch preserves tuple binding signatures", testQualifiedMethodDispatchPreservesTupleBindingSignature)
    , ("qualified method dispatch preserves tuple exact signatures", testQualifiedMethodDispatchPreservesTupleExactSignature)
    , ("qualified method dispatch preserves section binding signatures", testQualifiedMethodDispatchPreservesSectionBindingSignature)
    , ("qualified method dispatch treats Float as Float64 alias at runtime", testQualifiedMethodDispatchTreatsFloatAsFloat64Alias)
    , ("qualified method dispatch prefers Float alias body for typed Float values", testQualifiedMethodDispatchPrefersFloatAliasBody)
    , ("qualified method dispatch preserves concrete left Float64 over right Float aliases", testQualifiedMethodDispatchPreservesConcreteLeftFloat64OverRightFloatAlias)
    , ("qualified method dispatch mirrors runtime Float64-domain arithmetic", testQualifiedMethodDispatchMirrorsRuntimeFloat64DomainArithmetic)
    , ("qualified method dispatch executes Float equality body", testQualifiedMethodDispatchExecutesFloatEqualityBody)
    , ("qualified method dispatch executes Float16 equality body", testQualifiedMethodDispatchExecutesFloat16EqualityBody)
    , ("qualified method dispatch executes Float32 equality body", testQualifiedMethodDispatchExecutesFloat32EqualityBody)
    , ("qualified method dispatch executes Float64 equality body", testQualifiedMethodDispatchExecutesFloat64EqualityBody)
    , ("qualified method dispatch treats Int as Int64 alias at runtime", testQualifiedMethodDispatchTreatsIntAsInt64Alias)
    , ("qualified method dispatch re-hints Int aliases for Int64 parameters", testQualifiedMethodDispatchRehintsIntAliasForInt64Parameter)
    , ("qualified method dispatch prefers Int alias body for typed Int values", testQualifiedMethodDispatchPrefersIntAliasBody)
    , ("qualified method dispatch prefers Int alias body for direct integer literals", testQualifiedMethodDispatchPrefersIntAliasBodyForDirectLiteral)
    , ("qualified method dispatch prefers list alias body for typed list values", testQualifiedMethodDispatchPrefersListAliasBody)
    , ("qualified method dispatch prefers list alias body for direct list literals", testQualifiedMethodDispatchPrefersListAliasBodyForDirectLiteral)
    , ("raw list prepend re-hints the head to the concrete tail element type", testRawListPrependRehintsHeadToConcreteTailElementType)
    , ("qualified method dispatch preserves bound nested list runtime hints", testQualifiedMethodDispatchPreservesBoundNestedListRuntimeHint)
    , ("qualified method dispatch instantiates explicit empty list type application hints", testQualifiedMethodDispatchInstantiatesExplicitEmptyListTypeApplicationHint)
    , ("qualified method dispatch omits plain polymorphic empty list runtime hints", testQualifiedMethodDispatchOmitsPlainPolymorphicEmptyListRuntimeHint)
    , ("qualified method dispatch records signed polymorphic function runtime templates", testQualifiedMethodDispatchRecordsSignedPolymorphicFunctionRuntimeTemplate)
    , ("qualified method dispatch records concrete explicit named application hints", testQualifiedMethodDispatchRecordsConcreteExplicitNamedApplicationHint)
    , ("qualified method dispatch rejects unhinted nested list helper exact selection", testQualifiedMethodDispatchRejectsUnhintedNestedListHelperExactSelection)
    , ("qualified method dispatch does not exact-match untyped empty list literals", testQualifiedMethodDispatchDoesNotExactMatchUntypedEmptyListLiteral)
    , ("qualified method dispatch prefers constructor alias body for direct constructor literals", testQualifiedMethodDispatchPrefersConstructorAliasBodyForDirectLiteral)
    , ("qualified method dispatch ignores monomorphic constructor payloads for exact selection", testQualifiedMethodDispatchIgnoresMonomorphicConstructorPayloadForExactSelection)
    , ("qualified method dispatch treats non-literal integer results as Int64", testQualifiedMethodDispatchTreatsNonLiteralIntegerResultsAsInt64)
    , ("qualified method dispatch preserves higher-order binding signatures", testQualifiedMethodDispatchPreservesHigherOrderBindingSignature)
    , ("qualified method dispatch preserves higher-order exact signatures", testQualifiedMethodDispatchPreservesHigherOrderExactSignature)
    , ("qualified method dispatch rejects unhinted function argument exact selection", testQualifiedMethodDispatchRejectsUnhintedFunctionArgumentExactSelection)
    , ("qualified method dispatch defers exact filtering until target argument", testQualifiedMethodDispatchDefersExactFilteringUntilTargetArgument)
    , ("qualified method dispatch preserves selected method signatures", testQualifiedMethodDispatchPreservesSelectedMethodSignature)
    , ("qualified method dispatch applies typed callable argument hints", testQualifiedMethodDispatchAppliesTypedCallableArgumentHint)
    , ("qualified method dispatch applies typed callable argument hints through prefix dollar", testQualifiedMethodDispatchAppliesTypedCallableArgumentHintThroughPrefixDollar)
    , ("qualified method dispatch applies closure argument signature hints", testQualifiedMethodDispatchAppliesClosureArgumentSignatureHint)
    , ("qualified method dispatch preserves defaulted closure result metadata", testQualifiedMethodDispatchPreservesDefaultedClosureResultMetadata)
    , ("qualified method dispatch preserves empty list binding signatures", testQualifiedMethodDispatchPreservesEmptyListBindingSignature)
    , ("qualified method dispatch preserves list-returning application signatures", testQualifiedMethodDispatchPreservesListReturningApplicationSignature)
    , ("qualified method dispatch preserves dollar-applied list-returning signatures", testQualifiedMethodDispatchPreservesDollarAppliedListReturningSignature)
    , ("qualified method dispatch preserves ADT-returning application signatures", testQualifiedMethodDispatchPreservesAdtReturningApplicationSignature)
    , ("qualified method dispatch preserves branch result signatures", testQualifiedMethodDispatchPreservesBranchResultSignature)
    , ("qualified method dispatch preserves block result signatures", testQualifiedMethodDispatchPreservesBlockResultSignature)
    , ("qualified method dispatch preserves mapped empty list result signatures", testQualifiedMethodDispatchPreservesMappedEmptyListResultSignature)
    , ("qualified method dispatch preserves identity-mapped empty list result signatures", testQualifiedMethodDispatchPreservesIdentityMappedEmptyListResultSignature)
    , ("qualified method dispatch preserves mapped hd empty nested list result signatures", testQualifiedMethodDispatchPreservesMappedHdEmptyNestedListResultSignature)
    , ("qualified method dispatch preserves hd element signatures", testQualifiedMethodDispatchPreservesHdElementSignature)
    , ("qualified method dispatch normalizes hinted list aliases", testQualifiedMethodDispatchNormalizesHintedListAliases)
    , ("qualified method dispatch normalizes hinted function aliases", testQualifiedMethodDispatchNormalizesHintedFunctionAliases)
    , ("qualified method dispatch treats defaulted integer bindings as Int64", testQualifiedMethodDispatchTreatsDefaultedIntegerBindingAsInt64)
    , ("qualified method dispatch treats plain integer bindings as Int64 when exact candidates overlap", testQualifiedMethodDispatchTreatsPlainIntegerBindingAsInt64WithExactCandidates)
    , ("qualified method dispatch treats inferred direct integer literals as exact Int", testQualifiedMethodDispatchTreatsInferredDirectIntegerLiteralAsExactInt)
    , ("qualified method dispatch preserves inferred narrow integer bindings", testQualifiedMethodDispatchPreservesInferredNarrowIntegerBinding)
    , ("qualified method dispatch preserves ADT application binding hints", testQualifiedMethodDispatchPreservesAdtApplicationBindingHint)
    , ("qualified method dispatch preserves phantom ADT application binding hints", testQualifiedMethodDispatchPreservesPhantomAdtApplicationBindingHint)
    , ("qualified method dispatch preserves ADT concrete payload hints", testQualifiedMethodDispatchPreservesAdtConcretePayloadHint)
    , ("qualified method dispatch preserves monomorphic ADT concrete payload hints", testQualifiedMethodDispatchPreservesMonomorphicAdtConcretePayloadHint)
    , ("qualified method dispatch ignores unknown constructor field hint names", testQualifiedMethodDispatchIgnoresUnknownConstructorFieldHintName)
    , ("qualified method dispatch keeps nested inferred hints scoped", testQualifiedMethodDispatchKeepsNestedInferredHintsScoped)
    , ("nested binding hints retain their enclosing source unit", testNestedBindingHintsRetainEnclosingSourceUnit)
    , ("qualified method dispatch prefers alias binding over method sentinel at runtime", testQualifiedMethodDispatchPrefersAliasBindingOverMethodSentinelAtRuntime)
    , ("qualified zero-argument method dispatch returns value", testQualifiedZeroArgumentMethodDispatchReturnsValue)
    , ("qualified method dispatch rejects direct self alias", testQualifiedMethodDispatchRejectsDirectSelfAlias)
    , ("qualified method dispatch rejects wrapped self alias", testQualifiedMethodDispatchRejectsWrappedSelfAlias)
    , ("qualified method dispatch rejects block-local self alias", testQualifiedMethodDispatchRejectsBlockLocalSelfAlias)
    , ("qualified method dispatch follows block-local alias branches with local bindings", testQualifiedMethodDispatchFollowsBlockLocalAliasBranchesWithLocalBindings)
    , ("qualified method dispatch follows block-local alias branches with local signature hints", testQualifiedMethodDispatchFollowsBlockLocalAliasBranchesWithLocalSignatureHints)
    , ("qualified method dispatch rejects full-arity runtime ambiguity", testQualifiedMethodDispatchRejectsFullArityRuntimeAmbiguity)
    , ("qualified method dispatch executes local ADT impl body", testQualifiedMethodDispatchExecutesLocalAdtImplBody)
    , ("method-bearing capability declarations are inert at runtime", testMethodBearingCapabilityDeclarationsRuntimeInert)
  ]

testRuntimeFallbackRejectsQualifiedMethodStructuralEquality :: IO ()
testRuntimeFallbackRejectsQualifiedMethodStructuralEquality = do
  let result = evaluateRuntimeExpr qualifiedMethodStructuralEqualityExpr
  assertRuntimeErrorContains "runtime fallback qualified method structural equality" "E3007" result
  assertRuntimeErrorContains
    "runtime fallback qualified method structural equality callable text"
    "callable values are not equality-supported"
    result

testCapabilityDeclarationOnlyScopeHasNoOutput :: IO ()
testCapabilityDeclarationOnlyScopeHasNoOutput = do
  result <- runSource defaultWarningSettings """
  class RuntimeOnly(a) { }.
  impl RuntimeOnly(Int) { }.
  """
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "declaration-only capability scope produces no output" Nothing (runOutput result)

testCapabilityDeclarationsRuntimeInert :: IO ()
testCapabilityDeclarationsRuntimeInert = do
  result <- runSource defaultWarningSettings """
  class RuntimeOnly(a) { }.
  impl RuntimeOnly(Int) { }.
  x = 1.
  x.
  """
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "capability declarations do not affect runtime output" (Just "1") (runOutput result)

testQualifiedMethodCandidateCarriesRuntimeEvidence :: IO ()
testQualifiedMethodCandidateCarriesRuntimeEvidence =
  case evaluateRuntimeExpr qualifiedMethodEvidenceExpr of
    Right (Just methodValue@VQualifiedMethod {}) -> do
      assertContains
        "runtime candidate evidence record"
        "RuntimeEvidence"
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
      failTest ("expected qualified method runtime value, got " <> Text.pack (show otherValue))
    Left runtimeError ->
      failTest ("expected qualified method runtime value, got " <> renderDiagnostic runtimeError)
  where
    qualifiedMethodEvidenceExpr =
      EBlock
        [ SClass
            (SourceSpan 1 1)
            "Eq"
            ["a"]
            [ ClassMethodSignature
                "equals"
                (SourceSpan 2 1)
                ( ConstrainedSignature
                    []
                    ( TypeFunction
                        (TypeVariable "a")
                        (TypeFunction (TypeVariable "a") (TypeBool))
                    )
                )
            ],
          SImpl
            (SourceSpan 3 1)
            "Eq"
            [TypeInt]
            [ ImplMethod
                "equals"
                (SourceSpan 4 1)
                (ELambda "left" (ELambda "right" (ELit (LBool True))))
            ],
          SImpl
            (SourceSpan 5 1)
            "Eq"
            [TypeBool]
            [ ImplMethod
                "equals"
                (SourceSpan 6 1)
                (ELambda "left" (ELambda "right" (ELit (LBool True))))
            ],
          SExpr (SourceSpan 7 1) (EVar (qualifiedName "Eq" "equals"))
        ]

testQualifiedMethodDispatchExecutesImplBody :: IO ()
testQualifiedMethodDispatchExecutesImplBody = do
  result <- runSource defaultWarningSettings (runtimeEqSource <> "RuntimeEq::equals 1 1.")
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "True") (runOutput result)

testLetBoundQualifiedMethodDispatchExecutesImplBody :: IO ()
testLetBoundQualifiedMethodDispatchExecutesImplBody = do
  result <-
    runSource defaultWarningSettings
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
    runSource defaultWarningSettings
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
      id = \\(value) -> value.
      result = RuntimeEq::equals (id @UInt8 1) (id @UInt8 2).
      result.
      """
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "False") (runOutput result)

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
      pair = \\(value) -> (value, True).
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
      select = \\(width, value) -> value.
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
      flag = \\(value) -> True.
      }.
      impl RuntimeFlag(Int64) {
      flag = \\(value) -> False.
      }.
      use :: @{RuntimeFlag(a)}: a -> b -> Bool.
      use = \\(value, ignored) -> RuntimeFlag::flag value.
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
      id8 = \\(value) -> value.
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
      id8 = \\(value) -> value.
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
      pick = \\(value) -> True.
      }.
      impl RuntimePick((UInt8, UInt8)) {
      pick = \\(value) -> False.
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
      pick = \\(value) -> True.
      }.
      impl RuntimePick((Int64, Int64)) {
      pick = \\(value) -> False.
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
      flag = \\(value) -> True.
      }.
      impl RuntimeFlag(Float64) {
      flag = \\(value) -> False.
      }.
      value :: Float.
      value = 1.5.
      RuntimeFlag::flag value.
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
      flag = \\(value) -> True.
      }.
      impl RuntimeFlag(Float64) {
      flag = \\(value) -> False.
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
      flag = \\(value) -> True.
      }.
      impl RuntimeFlag(Float64) {
      flag = \\(value) -> False.
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
      flag = \\(value) -> True.
      }.
      impl RuntimeFlag(Int64) {
      flag = \\(value) -> False.
      }.
      asInt :: Int.
      asInt = 1.
      asInt64 :: Int64 -> Int64.
      asInt64 = \\(value) -> value.
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
      flag = \\(value) -> True.
      }.
      impl RuntimeFlag(Int64) {
      flag = \\(value) -> False.
      }.
      value :: Int.
      value = 1.
      RuntimeFlag::flag value.
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
      flag = \\(value) -> True.
      }.
      impl RuntimeFlag(Int64) {
      flag = \\(value) -> False.
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
      flag = \\(value) -> True.
      }.
      impl RuntimeFlag(Int64) {
      flag = \\(value) -> False.
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

testQualifiedMethodDispatchOmitsPlainPolymorphicEmptyListRuntimeHint :: IO ()
testQualifiedMethodDispatchOmitsPlainPolymorphicEmptyListRuntimeHint = do
  let expr =
        EBlock
          [ SLet "empty" (SourceSpan 1 1) (EList []),
            SExpr (SourceSpan 2 1) (EVar "empty")
          ]
  inference <- inferExpressionWithBuiltins ResolveKernelOnly defaultWarningSettings expr
  assertEqual "inference errors" [] (filter isErrorDiagnostic (inferredDiagnostics inference))
  assertEqual "plain polymorphic empty list runtime hints" Map.empty (inferredRuntimeTypeHints inference)

testQualifiedMethodDispatchRecordsSignedPolymorphicFunctionRuntimeTemplate :: IO ()
testQualifiedMethodDispatchRecordsSignedPolymorphicFunctionRuntimeTemplate = do
  let expr =
        EBlock
          [ SSignature "identity" (SourceSpan 1 1) (SignatureType (TypeFunction (TypeVariable "a") (TypeVariable "a"))),
            SLet "identity" (SourceSpan 2 1) (ELambda "value" (EVar "value")),
            SExpr (SourceSpan 3 1) (EVar "identity")
          ]
  inference <- inferExpressionWithBuiltins ResolveKernelOnly defaultWarningSettings expr
  assertEqual "inference errors" [] (filter isErrorDiagnostic (inferredDiagnostics inference))
  assertEqual
    "signed polymorphic function runtime template"
    (Just (TypeFunction (TypeVariable "t0") (TypeVariable "t0")))
    (Map.lookup (bindingRuntimeHintKey "identity" (SourceSpan 2 1)) (inferredRuntimeTypeHints inference))

testQualifiedMethodDispatchRecordsConcreteExplicitNamedApplicationHint :: IO ()
testQualifiedMethodDispatchRecordsConcreteExplicitNamedApplicationHint = do
  let typeArgumentSpan = SourceSpan 4 12
      boxCharType = TypeApplication "Box" [TypeChar]
      expr =
        EBlock
          [ SData (SourceSpan 1 1) "Box" ["a"] [DataConstructor "Box" [DataConstructorArgumentName "a"]],
            SSignature "identity" (SourceSpan 2 1) (SignatureType (TypeFunction (TypeVariable "a") (TypeVariable "a"))),
            SLet "identity" (SourceSpan 3 1) (ELambda "value" (EVar "value")),
            SExpr (SourceSpan 4 1) (ETypeApplication (EVar "identity") typeArgumentSpan boxCharType)
          ]
  inference <- inferExpressionWithBuiltins ResolveKernelOnly defaultWarningSettings expr
  assertEqual "inference errors" [] (filter isErrorDiagnostic (inferredDiagnostics inference))
  assertEqual
    "concrete explicit named application hint"
    (Just (TypeFunction boxCharType boxCharType))
    ( Map.lookup
        (explicitTypeApplicationRuntimeHintKeyInModule Nothing typeArgumentSpan)
        (inferredRuntimeTypeHints inference)
    )

testQualifiedMethodDispatchRejectsUnhintedNestedListHelperExactSelection :: IO ()
testQualifiedMethodDispatchRejectsUnhintedNestedListHelperExactSelection = do
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
  assertSingleDiagnosticContains
    "unhinted nested list helper exact selection"
    "ambiguous qualified method body 'RuntimeFlag::flag'"
    (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" Nothing (runOutput result)

testQualifiedMethodDispatchDoesNotExactMatchUntypedEmptyListLiteral :: IO ()
testQualifiedMethodDispatchDoesNotExactMatchUntypedEmptyListLiteral =
  assertEqual
    "untyped empty list exact match"
    False
    (runtimeValueExactlyMatchesConstraint (TypeList (TypeInt)) (VList [] Nothing))

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

testQualifiedMethodDispatchIgnoresMonomorphicConstructorPayloadForExactSelection :: IO ()
testQualifiedMethodDispatchIgnoresMonomorphicConstructorPayloadForExactSelection = do
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
      (RuntimeFlag::flag) (Wrap 1 1).
      """
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "True") (runOutput result)

testQualifiedMethodDispatchTreatsNonLiteralIntegerResultsAsInt64 :: IO ()
testQualifiedMethodDispatchTreatsNonLiteralIntegerResultsAsInt64 = do
  result <-
    runSource
      defaultWarningSettings
      ( """
      class RuntimeFlag(a) {
      flag :: a -> Bool.
      }.
      impl RuntimeFlag(Int) {
      flag = \\(value) -> True.
      }.
      impl RuntimeFlag(Int64) {
      flag = \\(value) -> False.
      }.
      (RuntimeFlag::flag) ((\\(x) -> x) 1).
      """
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "False") (runOutput result)

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
      idInt = \\(value) -> value.
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
      id64 = \\(value) -> value.
      RuntimeApply::apply id64.
      """
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "False") (runOutput result)

testQualifiedMethodDispatchRejectsUnhintedFunctionArgumentExactSelection :: IO ()
testQualifiedMethodDispatchRejectsUnhintedFunctionArgumentExactSelection = do
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
      (RuntimeApply::apply) (\\(value) -> value + 1).
      """
      )
  assertSingleDiagnosticContains
    "unhinted function argument exact selection"
    "ambiguous qualified method body 'RuntimeApply::apply'"
    (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" Nothing (runOutput result)

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
      pick = \\(index, value) -> False.
      }.
      impl RuntimePick(Bool) {
      pick = \\(index, value) -> True.
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
      id = \\(value) -> value.
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
      RuntimeApply::apply Id::id.
      """
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "True") (runOutput result)

testQualifiedMethodDispatchAppliesTypedCallableArgumentHint :: IO ()
testQualifiedMethodDispatchAppliesTypedCallableArgumentHint = do
  let result =
        evaluateRuntimeExprWithBuiltinsAndBindingHints
          ResolveKernelOnly
          (Map.singleton (bindingRuntimeHintKey "choose" (SourceSpan 9 1)) (TypeFunction (TypeNumeric NumericUInt8) (TypeBool)))
          (runtimeTypedCallableArgumentHintExpr (EVar (qualifiedName "RuntimePick" "pick")))
  assertEqual "typed callable argument hint runtime result" (Right (Just (VBool False))) result

testQualifiedMethodDispatchAppliesTypedCallableArgumentHintThroughPrefixDollar :: IO ()
testQualifiedMethodDispatchAppliesTypedCallableArgumentHintThroughPrefixDollar = do
  let result =
        evaluateRuntimeExprWithBuiltinsAndBindingHints
          ResolveKernelOnly
          (Map.singleton (bindingRuntimeHintKey "choose" (SourceSpan 9 1)) (TypeFunction (TypeNumeric NumericUInt8) (TypeBool)))
          (runtimeTypedCallableArgumentHintThroughPrefixDollarExpr (EVar (qualifiedName "RuntimePick" "pick")))
  assertEqual "typed callable argument hint through prefix dollar runtime result" (Right (Just (VBool False))) result

testQualifiedMethodDispatchAppliesClosureArgumentSignatureHint :: IO ()
testQualifiedMethodDispatchAppliesClosureArgumentSignatureHint = do
  let result =
        evaluateRuntimeExprWithBuiltinsAndBindingHints
          ResolveKernelOnly
          (Map.singleton (bindingRuntimeHintKey "choose" (SourceSpan 9 1)) (TypeFunction (TypeNumeric NumericUInt8) (TypeBool)))
          ( runtimeTypedCallableArgumentHintExpr
              (ELambda "value" (EApply (EVar (qualifiedName "RuntimePick" "pick")) (EVar "value")))
          )
  assertEqual "closure argument signature hint runtime result" (Right (Just (VBool False))) result

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
      pick = \\(value) -> True.
      }.
      impl RuntimePick(UInt8) {
      pick = \\(value) -> False.
      }.
      f = \\(value) -> 1.
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
      id8 = \\(value) -> value.
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
      mapped = map (\\(value) -> value) values.
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
      idInt = \\(value) -> value.
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
      pick = \\(value) -> True.
      }.
      impl RuntimePick(UInt8) {
      pick = \\(value) -> False.
      }.
      value = 1.
      RuntimePick::pick value.
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
      flag = \\(value) -> True.
      }.
      impl RuntimeFlag(Int64) {
      flag = \\(value) -> False.
      }.
      value = 1.
      RuntimeFlag::flag value.
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
      flag = \\(value) -> True.
      }.
      impl RuntimeFlag(Int64) {
      flag = \\(value) -> False.
      }.
      result = (\\(value) -> RuntimeFlag::flag value) 1.
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
      pick = \\(value) -> True.
      }.
      impl RuntimePick(UInt8) {
      pick = \\(value) -> False.
      }.
      value = if True then 1 else toUInt8 2.
      RuntimePick::pick value.
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
  let result =
        evaluateRuntimeExprWithBuiltinsAndBindingHints
          ResolveKernelOnly
          (Map.singleton (bindingRuntimeHintKey "tag" (SourceSpan 6 1)) (TypeApplication "Tag" [TypeNumeric NumericUInt8]))
          ( EBlock
              [ SData
                  (SourceSpan 1 1)
                  "Tag"
                  ["a"]
                  [DataConstructor "Tag" []],
                SClass
                  (SourceSpan 2 1)
                  "RuntimePick"
                  ["a"]
                  [ ClassMethodSignature
                      "pick"
                      (SourceSpan 3 1)
                      (ConstrainedSignature [] (TypeFunction (TypeVariable "a") (TypeBool)))
                  ],
                SImpl
                  (SourceSpan 4 1)
                  "RuntimePick"
                  [TypeApplication "Tag" [TypeInt]]
                  [ImplMethod "pick" (SourceSpan 5 1) (ELambda "tag" (ELit (LBool True)))],
                SImpl
                  (SourceSpan 4 1)
                  "RuntimePick"
                  [TypeApplication "Tag" [TypeNumeric NumericUInt8]]
                  [ImplMethod "pick" (SourceSpan 5 1) (ELambda "tag" (ELit (LBool False)))],
                SLet
                  "tag"
                  (SourceSpan 6 1)
                  (EVar "Tag"),
                SExpr (SourceSpan 7 1) (EApply (EVar (qualifiedName "RuntimePick" "pick")) (EVar "tag"))
              ]
          )
  assertEqual "phantom ADT application hint runtime result" (Right (Just (VBool False))) result

testQualifiedMethodDispatchPreservesAdtConcretePayloadHint :: IO ()
testQualifiedMethodDispatchPreservesAdtConcretePayloadHint = do
  let result =
        evaluateRuntimeExprWithBuiltinsAndBindingHints
          ResolveKernelOnly
          (Map.singleton (bindingRuntimeHintKey "box" (SourceSpan 6 1)) (TypeApplication "Box" [TypeNumeric NumericUInt8]))
          ( EBlock
              [ SData
                  (SourceSpan 1 1)
                  "Box"
                  ["a"]
                  [DataConstructor "Box" [DataConstructorArgumentName "Float32", DataConstructorArgumentName "a"]],
                SClass
                  (SourceSpan 2 1)
                  "RuntimePick"
                  ["a"]
                  [ ClassMethodSignature
                      "pick"
                      (SourceSpan 3 1)
                      (ConstrainedSignature [] (TypeFunction (TypeVariable "a") (TypeBool)))
                  ],
                SImpl
                  (SourceSpan 4 1)
                  "RuntimePick"
                  [TypeApplication "Box" [TypeNumeric NumericUInt8]]
                  [ImplMethod "pick" (SourceSpan 5 1) (ELambda "box" (ELit (LBool False)))],
                SLet
                  "box"
                  (SourceSpan 6 1)
                  ( EApply
                      (EApply (EVar "Box") (ELit (LFloat 1.5 (mkFractionalLiteralSource 1 5 1) Nothing)))
                      (EApply (EVar "__kernel_toUInt8") (ELit (LInt 2)))
                  ),
                SExpr (SourceSpan 7 1) (EApply (EVar (qualifiedName "RuntimePick" "pick")) (EVar "box"))
              ]
          )
  assertEqual "ADT concrete payload hint runtime result" (Right (Just (VBool False))) result

testQualifiedMethodDispatchPreservesMonomorphicAdtConcretePayloadHint :: IO ()
testQualifiedMethodDispatchPreservesMonomorphicAdtConcretePayloadHint = do
  let result =
        evaluateRuntimeExprWithBuiltinsAndBindingHints
          ResolveKernelOnly
          (Map.singleton (bindingRuntimeHintKey "token" (SourceSpan 6 1)) (TypeName "Token"))
          ( EBlock
              ( [ SData
                    (SourceSpan 1 1)
                    "Token"
                    []
                    [DataConstructor "Token" [DataConstructorArgumentName "UInt8"]]
                ]
                  ++ runtimePickStatements
                  ++ [ SLet
                         "token"
                         (SourceSpan 6 1)
                         (EApply (EVar "Token") (ELit (LInt 1))),
                       SExpr
                         (SourceSpan 7 1)
                         ( EPatternCase
                             (EVar "token")
                             [ CaseArm
                                 (PConstructor "Token" [PVariable "value"])
                                 Nothing
                                 (EApply (EVar (qualifiedName "RuntimePick" "pick")) (EVar "value"))
                             ]
                         )
                     ]
              )
          )
  assertEqual "monomorphic ADT concrete payload hint runtime result" (Right (Just (VBool False))) result

testQualifiedMethodDispatchIgnoresUnknownConstructorFieldHintName :: IO ()
testQualifiedMethodDispatchIgnoresUnknownConstructorFieldHintName = do
  let result =
        evaluateRuntimeExprWithBuiltinsAndBindingHints
          ResolveKernelOnly
          (Map.singleton (bindingRuntimeHintKey "box" (SourceSpan 6 1)) (TypeApplication "Box" [TypeNumeric NumericUInt8]))
          ( EBlock
              [ SData
                  (SourceSpan 1 1)
                  "Box"
                  ["a"]
                  [DataConstructor "Box" [DataConstructorArgumentName "value", DataConstructorArgumentName "a"]],
                SClass
                  (SourceSpan 2 1)
                  "RuntimePick"
                  ["a"]
                  [ ClassMethodSignature
                      "pick"
                      (SourceSpan 3 1)
                      (ConstrainedSignature [] (TypeFunction (TypeVariable "a") (TypeBool)))
                  ],
                SImpl
                  (SourceSpan 4 1)
                  "RuntimePick"
                  [TypeApplication "Box" [TypeNumeric NumericUInt8]]
                  [ImplMethod "pick" (SourceSpan 5 1) (ELambda "box" (ELit (LBool False)))],
                SLet
                  "box"
                  (SourceSpan 6 1)
                  ( EApply
                      (EApply (EVar "Box") (ELit (LInt 1)))
                      (EApply (EVar "__kernel_toUInt8") (ELit (LInt 2)))
                  ),
                SExpr (SourceSpan 7 1) (EApply (EVar (qualifiedName "RuntimePick" "pick")) (EVar "box"))
              ]
          )
  assertEqual "unknown constructor field hint runtime result" (Right (Just (VBool False))) result

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
      pick = \\(value) -> True.
      }.
      impl RuntimePick(UInt8) {
      pick = \\(value) -> False.
      }.
      RuntimePick::pick z.
      """
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "True") (runOutput result)

testNestedBindingHintsRetainEnclosingSourceUnit :: IO ()
testNestedBindingHintsRetainEnclosingSourceUnit = do
  let bindingSpan = SourceSpan 5 3
      expr =
        EBlock
          [ SLet "seed" (SourceSpan 1 1) (ELit (LInt 0)),
            SExpr
              (SourceSpan 2 1)
              ( EBlock
                  [ SLet "value" bindingSpan (ELit (LInt 1)),
                    SExpr (SourceSpan 6 3) (EVar "value")
                  ]
              )
          ]
  inference <-
    inferExpressionWithBuiltinsAndSourceUnitStatements
      ResolveKernelOnly
      Set.empty
      (Set.singleton 0)
      defaultWarningSettings
      expr
  assertEqual "inference errors" [] (filter isErrorDiagnostic (inferredDiagnostics inference))
  assertEqual
    "nested binding hint source-unit path"
    (Just (TypeNumeric NumericInt64))
    ( Map.lookup
        (bindingRuntimeHintKeyInModule Nothing "value" bindingSpan)
        (inferredRuntimeTypeHints inference)
    )
  assertEqual
    "nested binding hint does not reuse the prelude path"
    Nothing
    ( Map.lookup
        (bindingRuntimeHintKeyInModule (Just []) "value" bindingSpan)
        (inferredRuntimeTypeHints inference)
    )

testQualifiedMethodDispatchPrefersAliasBindingOverMethodSentinelAtRuntime :: IO ()
testQualifiedMethodDispatchPrefersAliasBindingOverMethodSentinelAtRuntime = do
  let result =
        evaluateRuntimeExpr
          ( runtimeExpr
              ( EBlock
                  [ SLet "Eq::helper" (SourceSpan 1 1) (ELambda "value" (ELit (LBool True))),
                    SClass
                      (SourceSpan 2 1)
                      "Eq"
                      ["a"]
                      [ ClassMethodSignature
                          "helper"
                          (SourceSpan 3 1)
                          ( ConstrainedSignature
                              []
                              (TypeFunction (TypeVariable "a") (TypeBool))
                          )
                      ],
                    SImpl
                      (SourceSpan 4 1)
                      "Eq"
                      [TypeInt]
                      [ImplMethod "helper" (SourceSpan 5 1) (ELambda "value" (ELit (LBool False)))],
                    SExpr
                      (SourceSpan 6 1)
                      (EApply (EVar "Eq::helper") (ELit (LInt 1)))
                  ]
              )
          )
  assertEqual "alias binding runtime result" (Right (Just (VBool True))) result

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
      RuntimeFlag::enabled.
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
        "recursive qualified method alias cycle"
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
        "recursive qualified method alias cycle"
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
              enabled = { helper = RuntimeFlag::enabled.
              helper. }.
              }.
              RuntimeFlag::enabled.
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
        "recursive qualified method alias cycle"
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
      target = if flag then RuntimeFlag::on else RuntimeFlag::off.
      target.
      }.
      on = True.
      off = False.
      }.
      RuntimeFlag::enabled.
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
      enabled = { value :: [[Int64]].
      value = [[1], []].
      target = if ((RuntimeFlag::flag) value) then RuntimeChoice::on else RuntimeChoice::off.
      target.
      }.
      on = True.
      off = False.
      }.
      RuntimeChoice::enabled.
      """
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "True") (runOutput result)

testQualifiedMethodDispatchRejectsFullArityRuntimeAmbiguity :: IO ()
testQualifiedMethodDispatchRejectsFullArityRuntimeAmbiguity =
  assertRuntimeErrorContains
    "fully applied ambiguous qualified method"
    "ambiguous qualified method body 'RuntimePick::choose'"
    (evaluateRuntimeExpr ambiguousQualifiedMethodRuntimeExpr)

testQualifiedMethodDispatchExecutesLocalAdtImplBody :: IO ()
testQualifiedMethodDispatchExecutesLocalAdtImplBody = do
  result <-
    runSource defaultWarningSettings
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
    runSource defaultWarningSettings
      ( runtimeEqSource
          <> """
          x = 1.
          x.
          """
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "method-bearing capability declarations do not affect runtime output" (Just "1") (runOutput result)
