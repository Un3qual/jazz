{-# LANGUAGE OverloadedStrings #-}

module JazzNext.Compiler.Semantics.Runtime.CapabilitiesTests
  ( capabilityTests
  ) where


import Control.Exception
  ( SomeException,
    try
  )
import qualified Data.Map.Strict as Map
import Data.Text (Text)
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
  ( Diagnostic,
    SourceSpan (..),
    renderDiagnostic
  )
import JazzNext.Compiler.BuiltinCatalog
  ( BuiltinResolutionMode (..)
  )
import JazzNext.Compiler.Driver
  ( RunResult (..),
    runSource,
    runSourceWithPrelude
  )
import JazzNext.Compiler.FractionalLiteral
  ( mkFractionalLiteralSource
  )
import JazzNext.Compiler.Name (Name, qualifiedName)
import JazzNext.Compiler.Runtime
  ( RuntimeValue (..),
    evaluateRuntimeExpr,
    evaluateRuntimeExprWithBuiltinsAndBindingHints,
    renderRuntimeValue,
    runtimeValueExactlyMatchesConstraint
  )
import JazzNext.Compiler.RuntimeHints
  ( bindingRuntimeHintKey,
    explicitTypeApplicationRuntimeHintKeyInModule
  )
import JazzNext.Compiler.TypeInference
  ( InferenceResult (..),
    inferExpressionWithBuiltins
  )
import JazzNext.Compiler.WarningConfig
  ( defaultWarningSettings
  )
import JazzNext.TestHarness
  ( NamedTest,
    assertContains,
    assertLeftDiagnosticCodeAndContains,
    assertEqual,
    assertSingleDiagnosticContains,
    failTest,
    runTestSuite
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
    , ("qualified method dispatch prefers Int alias body for typed Int values", testQualifiedMethodDispatchPrefersIntAliasBody)
    , ("qualified method dispatch prefers Int alias body for direct integer literals", testQualifiedMethodDispatchPrefersIntAliasBodyForDirectLiteral)
    , ("qualified method dispatch prefers list alias body for typed list values", testQualifiedMethodDispatchPrefersListAliasBody)
    , ("qualified method dispatch prefers list alias body for direct list literals", testQualifiedMethodDispatchPrefersListAliasBodyForDirectLiteral)
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
  result <- runSource defaultWarningSettings "class RuntimeOnly(a) { }.\nimpl RuntimeOnly(Int) { }."
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "declaration-only capability scope produces no output" Nothing (runOutput result)

testCapabilityDeclarationsRuntimeInert :: IO ()
testCapabilityDeclarationsRuntimeInert = do
  result <- runSource defaultWarningSettings "class RuntimeOnly(a) { }.\nimpl RuntimeOnly(Int) { }.\nx = 1.\nx."
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
  result <- runSource defaultWarningSettings (runtimeEqSource <> "result = RuntimeEq::equals 1 1.\nresult.")
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "True") (runOutput result)

testQualifiedMethodDispatchSelectsRuntimeBodyByArgumentTypes :: IO ()
testQualifiedMethodDispatchSelectsRuntimeBodyByArgumentTypes = do
  result <- runSource defaultWarningSettings (runtimeEqSource <> "impl RuntimeEq(Bool) {\nequals = \\(left) -> \\(right) -> left != right.\n}.\n(RuntimeEq::equals 1 2, RuntimeEq::equals True False).")
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "(False, True)") (runOutput result)

testQualifiedMethodDispatchExecutesSameImplQualifiedMethodCall :: IO ()
testQualifiedMethodDispatchExecutesSameImplQualifiedMethodCall = do
  result <-
    runSource
      defaultWarningSettings
      ( "class RuntimeEq(a) {\nequals :: a -> a -> Bool.\nnotEquals :: a -> a -> Bool.\n}.\n"
          <> "impl RuntimeEq(Int) {\nequals = \\(left) -> \\(right) -> left == right.\nnotEquals = \\(left) -> \\(right) -> RuntimeEq::equals left right != True.\n}.\n"
          <> "RuntimeEq::notEquals 1 2."
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "True") (runOutput result)

testQualifiedMethodDispatchSelectsWidthSpecificIntegerBody :: IO ()
testQualifiedMethodDispatchSelectsWidthSpecificIntegerBody = do
  result <-
    runSource
      defaultWarningSettings
      ( "class RuntimeEq(a) {\nequals :: a -> a -> Bool.\n}.\n"
          <> "impl RuntimeEq(Int8) {\nequals = \\(left) -> \\(right) -> True.\n}.\n"
          <> "impl RuntimeEq(Int16) {\nequals = \\(left) -> \\(right) -> False.\n}.\n"
          <> "left :: Int8.\nleft = 1.\n"
          <> "right :: Int8.\nright = 2.\n"
          <> "RuntimeEq::equals left right."
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "True") (runOutput result)

testQualifiedMethodDispatchSelectsWidthSpecificIntegerBodyForDirectLiterals :: IO ()
testQualifiedMethodDispatchSelectsWidthSpecificIntegerBodyForDirectLiterals = do
  result <-
    runSource
      defaultWarningSettings
      ( "class RuntimeEq(a) {\nequals :: a -> a -> Bool.\n}.\n"
          <> "impl RuntimeEq(Int8) {\nequals = \\(left) -> \\(right) -> True.\n}.\n"
          <> "impl RuntimeEq(Int16) {\nequals = \\(left) -> \\(right) -> False.\n}.\n"
          <> "right :: Int8.\nright = 2.\n"
          <> "RuntimeEq::equals 1 right."
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "True") (runOutput result)

testQualifiedMethodDispatchPreservesDirectExplicitTypeApplicationHint :: IO ()
testQualifiedMethodDispatchPreservesDirectExplicitTypeApplicationHint = do
  result <-
    runSource
      defaultWarningSettings
      ( "class RuntimeEq(a) {\nequals :: a -> a -> Bool.\n}.\n"
          <> "impl RuntimeEq(Int) {\nequals = \\(left) -> \\(right) -> True.\n}.\n"
          <> "impl RuntimeEq(UInt8) {\nequals = \\(left) -> \\(right) -> False.\n}.\n"
          <> "id :: @{RuntimeEq(a)}: a -> a.\nid = \\(value) -> value.\n"
          <> "result = RuntimeEq::equals (id @UInt8 1) (id @UInt8 2).\nresult."
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "False") (runOutput result)

testQualifiedMethodDispatchPreservesInferredExplicitTypeApplicationTupleHint :: IO ()
testQualifiedMethodDispatchPreservesInferredExplicitTypeApplicationTupleHint = do
  result <-
    runSource
      defaultWarningSettings
      ( "class RuntimeEq(a) {\nequals :: a -> a -> Bool.\n}.\n"
          <> "impl RuntimeEq((Int, Bool)) {\nequals = \\(left) -> \\(right) -> True.\n}.\n"
          <> "impl RuntimeEq((UInt8, Bool)) {\nequals = \\(left) -> \\(right) -> False.\n}.\n"
          <> "pair = \\(value) -> (value, True).\n"
          <> "result = RuntimeEq::equals (pair @UInt8 1) (pair @UInt8 2).\nresult."
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "False") (runOutput result)

testQualifiedMethodDispatchAppliesExplicitTypeArgumentToMatchingParameter :: IO ()
testQualifiedMethodDispatchAppliesExplicitTypeArgumentToMatchingParameter = do
  result <-
    runSource
      defaultWarningSettings
      ( "class RuntimeEq(a) {\nequals :: a -> a -> Bool.\n}.\n"
          <> "impl RuntimeEq(Int) {\nequals = \\(left) -> \\(right) -> True.\n}.\n"
          <> "impl RuntimeEq(UInt8) {\nequals = \\(left) -> \\(right) -> False.\n}.\n"
          <> "select :: @{RuntimeEq(b)}: Int16 -> b -> b.\n"
          <> "select = \\(width) -> \\(value) -> value.\n"
          <> "result = RuntimeEq::equals (select @UInt8 300 1) (select @UInt8 300 2).\nresult."
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "False") (runOutput result)

testQualifiedMethodDispatchPreservesPartiallyInstantiatedFunctionTemplate :: IO ()
testQualifiedMethodDispatchPreservesPartiallyInstantiatedFunctionTemplate = do
  result <-
    runSource
      defaultWarningSettings
      ( "class RuntimeFlag(a) {\nflag :: a -> Bool.\n}.\n"
          <> "impl RuntimeFlag(Int32) {\nflag = \\(value) -> True.\n}.\n"
          <> "impl RuntimeFlag(Int64) {\nflag = \\(value) -> False.\n}.\n"
          <> "use :: @{RuntimeFlag(a)}: a -> b -> Bool.\n"
          <> "use = \\(value) -> \\(ignored) -> RuntimeFlag::flag value.\n"
          <> "use @Int32 1 True."
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "True") (runOutput result)

testQualifiedMethodDispatchPreservesNonLiteralIntegerSignatureTarget :: IO ()
testQualifiedMethodDispatchPreservesNonLiteralIntegerSignatureTarget = do
  result <-
    runSource
      defaultWarningSettings
      ( "class RuntimeEq(a) {\nequals :: a -> a -> Bool.\n}.\n"
          <> "impl RuntimeEq(Int) {\nequals = \\(left) -> \\(right) -> True.\n}.\n"
          <> "impl RuntimeEq(UInt8) {\nequals = \\(left) -> \\(right) -> False.\n}.\n"
          <> "id8 :: UInt8 -> UInt8.\nid8 = \\(value) -> value.\n"
          <> "left :: UInt8.\nleft = id8 1.\n"
          <> "right :: UInt8.\nright = id8 2.\n"
          <> "RuntimeEq::equals left right."
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "False") (runOutput result)

testQualifiedMethodDispatchPreservesDirectClosureResultSignature :: IO ()
testQualifiedMethodDispatchPreservesDirectClosureResultSignature = do
  result <-
    runSource
      defaultWarningSettings
      ( "class RuntimeEq(a) {\nequals :: a -> a -> Bool.\n}.\n"
          <> "impl RuntimeEq(Int) {\nequals = \\(left) -> \\(right) -> True.\n}.\n"
          <> "impl RuntimeEq(UInt8) {\nequals = \\(left) -> \\(right) -> False.\n}.\n"
          <> "id8 :: UInt8 -> UInt8.\nid8 = \\(value) -> value.\n"
          <> "left = id8 1.\n"
          <> "right = id8 2.\n"
          <> "RuntimeEq::equals left right."
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "False") (runOutput result)

testQualifiedMethodDispatchPreservesTupleBindingSignature :: IO ()
testQualifiedMethodDispatchPreservesTupleBindingSignature = do
  result <-
    runSource
      defaultWarningSettings
      ( "class RuntimePick(a) {\npick :: a -> Bool.\n}.\n"
          <> "impl RuntimePick((Int, Int)) {\npick = \\(value) -> True.\n}.\n"
          <> "impl RuntimePick((UInt8, UInt8)) {\npick = \\(value) -> False.\n}.\n"
          <> "pair :: (UInt8, UInt8).\npair = (1, 2).\n"
          <> "RuntimePick::pick pair."
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "False") (runOutput result)

testQualifiedMethodDispatchPreservesTupleExactSignature :: IO ()
testQualifiedMethodDispatchPreservesTupleExactSignature = do
  result <-
    runSource
      defaultWarningSettings
      ( "class RuntimePick(a) {\npick :: a -> Bool.\n}.\n"
          <> "impl RuntimePick((Int, Int)) {\npick = \\(value) -> True.\n}.\n"
          <> "impl RuntimePick((Int64, Int64)) {\npick = \\(value) -> False.\n}.\n"
          <> "pair :: (Int64, Int64).\npair = (1, 2).\n"
          <> "RuntimePick::pick pair."
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "False") (runOutput result)

testQualifiedMethodDispatchPreservesSectionBindingSignature :: IO ()
testQualifiedMethodDispatchPreservesSectionBindingSignature = do
  result <-
    runSource
      defaultWarningSettings
      ( "class RuntimeApply(a) {\napply :: (a -> a) -> Bool.\n}.\n"
          <> "impl RuntimeApply(Int) {\napply = \\(fn) -> True.\n}.\n"
          <> "impl RuntimeApply(UInt8) {\napply = \\(fn) -> False.\n}.\n"
          <> "inc8 :: UInt8 -> UInt8.\ninc8 = (+ 1).\n"
          <> "RuntimeApply::apply inc8."
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "False") (runOutput result)

testQualifiedMethodDispatchTreatsFloatAsFloat64Alias :: IO ()
testQualifiedMethodDispatchTreatsFloatAsFloat64Alias = do
  result <-
    runSource
      defaultWarningSettings
      ( "class RuntimeEq(a) {\nequals :: a -> a -> Bool.\n}.\n"
          <> "impl RuntimeEq(Float) {\nequals = \\(left) -> \\(right) -> True.\n}.\n"
          <> "left :: Float64.\nleft = toFloat64 1.\n"
          <> "right :: Float64.\nright = toFloat64 1.\n"
          <> "RuntimeEq::equals left right."
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "True") (runOutput result)

testQualifiedMethodDispatchPrefersFloatAliasBody :: IO ()
testQualifiedMethodDispatchPrefersFloatAliasBody = do
  result <-
    runSource
      defaultWarningSettings
      ( "class RuntimeFlag(a) {\nflag :: a -> Bool.\n}.\n"
          <> "impl RuntimeFlag(Float) {\nflag = \\(value) -> True.\n}.\n"
          <> "impl RuntimeFlag(Float64) {\nflag = \\(value) -> False.\n}.\n"
          <> "value :: Float.\nvalue = 1.5.\n"
          <> "RuntimeFlag::flag value."
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "True") (runOutput result)

testQualifiedMethodDispatchPreservesConcreteLeftFloat64OverRightFloatAlias :: IO ()
testQualifiedMethodDispatchPreservesConcreteLeftFloat64OverRightFloatAlias = do
  result <-
    runSource
      defaultWarningSettings
      ( "class RuntimeFlag(a) {\nflag :: a -> Bool.\n}.\n"
          <> "impl RuntimeFlag(Float) {\nflag = \\(value) -> True.\n}.\n"
          <> "impl RuntimeFlag(Float64) {\nflag = \\(value) -> False.\n}.\n"
          <> "left :: Float64.\nleft = toFloat64 1.\n"
          <> "right :: Float.\nright = 2.5.\n"
          <> "(RuntimeFlag::flag) (left + right)."
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "False") (runOutput result)

testQualifiedMethodDispatchMirrorsRuntimeFloat64DomainArithmetic :: IO ()
testQualifiedMethodDispatchMirrorsRuntimeFloat64DomainArithmetic = do
  result <-
    runSource
      defaultWarningSettings
      ( "class RuntimeFlag(a) {\nflag :: a -> Bool.\n}.\n"
          <> "impl RuntimeFlag(Float) {\nflag = \\(value) -> True.\n}.\n"
          <> "impl RuntimeFlag(Float64) {\nflag = \\(value) -> False.\n}.\n"
          <> "floating :: Float64.\nfloating = toFloat64 2.\n"
          <> "(RuntimeFlag::flag) (1.5 + floating)."
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "False") (runOutput result)

testQualifiedMethodDispatchExecutesFloatEqualityBody :: IO ()
testQualifiedMethodDispatchExecutesFloatEqualityBody = do
  result <-
    runSource
      defaultWarningSettings
      ( "class RuntimeEq(a) {\nequals :: a -> a -> Bool.\n}.\n"
          <> "impl RuntimeEq(Float) {\nequals = \\(left) -> \\(right) -> left == right.\n}.\n"
          <> "left :: Float.\nleft = 1.5.\n"
          <> "same :: Float.\nsame = 1.5.\n"
          <> "different :: Float.\ndifferent = 2.25.\n"
          <> "(RuntimeEq::equals left same, RuntimeEq::equals left different)."
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "(True, False)") (runOutput result)

testQualifiedMethodDispatchExecutesFloat16EqualityBody :: IO ()
testQualifiedMethodDispatchExecutesFloat16EqualityBody = do
  result <-
    runSource
      defaultWarningSettings
      ( "class RuntimeEq(a) {\nequals :: a -> a -> Bool.\n}.\n"
          <> "impl RuntimeEq(Float16) {\nequals = \\(left) -> \\(right) -> left == right.\n}.\n"
          <> "left :: Float16.\nleft = 1.5.\n"
          <> "same :: Float16.\nsame = 1.5.\n"
          <> "different :: Float16.\ndifferent = 2.25.\n"
          <> "(RuntimeEq::equals left same, RuntimeEq::equals left different)."
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "(True, False)") (runOutput result)

testQualifiedMethodDispatchExecutesFloat32EqualityBody :: IO ()
testQualifiedMethodDispatchExecutesFloat32EqualityBody = do
  result <-
    runSource
      defaultWarningSettings
      ( "class RuntimeEq(a) {\nequals :: a -> a -> Bool.\n}.\n"
          <> "impl RuntimeEq(Float32) {\nequals = \\(left) -> \\(right) -> left == right.\n}.\n"
          <> "left :: Float32.\nleft = 1.5.\n"
          <> "same :: Float32.\nsame = 1.5.\n"
          <> "different :: Float32.\ndifferent = 2.25.\n"
          <> "(RuntimeEq::equals left same, RuntimeEq::equals left different)."
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "(True, False)") (runOutput result)

testQualifiedMethodDispatchExecutesFloat64EqualityBody :: IO ()
testQualifiedMethodDispatchExecutesFloat64EqualityBody = do
  result <-
    runSource
      defaultWarningSettings
      ( "class RuntimeEq(a) {\nequals :: a -> a -> Bool.\n}.\n"
          <> "impl RuntimeEq(Float64) {\nequals = \\(left) -> \\(right) -> left == right.\n}.\n"
          <> "left :: Float64.\nleft = toFloat64 1.\n"
          <> "right :: Float64.\nright = toFloat64 1.\n"
          <> "RuntimeEq::equals left right."
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "True") (runOutput result)

testQualifiedMethodDispatchTreatsIntAsInt64Alias :: IO ()
testQualifiedMethodDispatchTreatsIntAsInt64Alias = do
  result <-
    runSource
      defaultWarningSettings
      ( "class RuntimeEq(a) {\nequals :: a -> a -> Bool.\n}.\n"
          <> "impl RuntimeEq(Int) {\nequals = \\(left) -> \\(right) -> True.\n}.\n"
          <> "impl RuntimeEq(UInt8) {\nequals = \\(left) -> \\(right) -> False.\n}.\n"
          <> "left :: Int.\nleft = 1.\n"
          <> "right :: Int.\nright = 2.\n"
          <> "RuntimeEq::equals left right."
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "True") (runOutput result)

testQualifiedMethodDispatchPrefersIntAliasBody :: IO ()
testQualifiedMethodDispatchPrefersIntAliasBody = do
  result <-
    runSource
      defaultWarningSettings
      ( "class RuntimeFlag(a) {\nflag :: a -> Bool.\n}.\n"
          <> "impl RuntimeFlag(Int) {\nflag = \\(value) -> True.\n}.\n"
          <> "impl RuntimeFlag(Int64) {\nflag = \\(value) -> False.\n}.\n"
          <> "value :: Int.\nvalue = 1.\n"
          <> "RuntimeFlag::flag value."
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "True") (runOutput result)

testQualifiedMethodDispatchPrefersIntAliasBodyForDirectLiteral :: IO ()
testQualifiedMethodDispatchPrefersIntAliasBodyForDirectLiteral = do
  result <-
    runSource
      defaultWarningSettings
      ( "class RuntimeFlag(a) {\nflag :: a -> Bool.\n}.\n"
          <> "impl RuntimeFlag(Int) {\nflag = \\(value) -> True.\n}.\n"
          <> "impl RuntimeFlag(Int64) {\nflag = \\(value) -> False.\n}.\n"
          <> "RuntimeFlag::flag 1."
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "True") (runOutput result)

testQualifiedMethodDispatchPrefersListAliasBody :: IO ()
testQualifiedMethodDispatchPrefersListAliasBody = do
  result <-
    runSource
      defaultWarningSettings
      ( "class RuntimeFlag(a) {\nflag :: a -> Bool.\n}.\n"
          <> "impl RuntimeFlag([Int]) {\nflag = \\(values) -> True.\n}.\n"
          <> "impl RuntimeFlag([Int64]) {\nflag = \\(values) -> False.\n}.\n"
          <> "values :: [Int].\nvalues = [1, 2].\n"
          <> "RuntimeFlag::flag values."
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "True") (runOutput result)

testQualifiedMethodDispatchPrefersListAliasBodyForDirectLiteral :: IO ()
testQualifiedMethodDispatchPrefersListAliasBodyForDirectLiteral = do
  result <-
    runSource
      defaultWarningSettings
      ( "class RuntimeFlag(a) {\nflag :: a -> Bool.\n}.\n"
          <> "impl RuntimeFlag([Int]) {\nflag = \\(values) -> True.\n}.\n"
          <> "impl RuntimeFlag([Int64]) {\nflag = \\(values) -> False.\n}.\n"
          <> "(RuntimeFlag::flag) [1]."
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "True") (runOutput result)

testQualifiedMethodDispatchPreservesBoundNestedListRuntimeHint :: IO ()
testQualifiedMethodDispatchPreservesBoundNestedListRuntimeHint = do
  result <-
    runSource
      defaultWarningSettings
      ( "class RuntimeFlag(a) {\nflag :: a -> Bool.\n}.\n"
          <> "impl RuntimeFlag([[Int]]) {\nflag = \\(values) -> True.\n}.\n"
          <> "impl RuntimeFlag([[Int64]]) {\nflag = \\(values) -> False.\n}.\n"
          <> "values = [[1], []].\n"
          <> "(RuntimeFlag::flag) values."
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "False") (runOutput result)

testQualifiedMethodDispatchInstantiatesExplicitEmptyListTypeApplicationHint :: IO ()
testQualifiedMethodDispatchInstantiatesExplicitEmptyListTypeApplicationHint = do
  result <-
    runSource
      defaultWarningSettings
      ( "class RuntimeFlag(a) {\nflag :: a -> Bool.\n}.\n"
          <> "impl RuntimeFlag([Int]) {\nflag = \\(values) -> True.\n}.\n"
          <> "impl RuntimeFlag([Bool]) {\nflag = \\(values) -> False.\n}.\n"
          <> "empty = [].\n"
          <> "(RuntimeFlag::flag) (empty @Int)."
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
  assertEqual "inference errors" [] (inferredErrors inference)
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
  assertEqual "inference errors" [] (inferredErrors inference)
  assertEqual
    "signed polymorphic function runtime template"
    (Just (TypeFunction (TypeName "t0") (TypeName "t0")))
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
  assertEqual "inference errors" [] (inferredErrors inference)
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
      ( "class RuntimeFlag(a) {\nflag :: a -> Bool.\n}.\n"
          <> "impl RuntimeFlag([[Int]]) {\nflag = \\(values) -> True.\n}.\n"
          <> "impl RuntimeFlag([[Int64]]) {\nflag = \\(values) -> False.\n}.\n"
          <> "f = \\(x) -> RuntimeFlag::flag x.\n"
          <> "result = f [[1], []].\n"
          <> "result."
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
      ( "data Box a = Box a.\n"
          <> "class RuntimeFlag(a) {\nflag :: a -> Bool.\n}.\n"
          <> "impl RuntimeFlag(Box(Int)) {\nflag = \\(box) -> True.\n}.\n"
          <> "impl RuntimeFlag(Box(Int64)) {\nflag = \\(box) -> False.\n}.\n"
          <> "(RuntimeFlag::flag) (Box 1)."
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "True") (runOutput result)

testQualifiedMethodDispatchIgnoresMonomorphicConstructorPayloadForExactSelection :: IO ()
testQualifiedMethodDispatchIgnoresMonomorphicConstructorPayloadForExactSelection = do
  result <-
    runSource
      defaultWarningSettings
      ( "data Wrap a = Wrap Int64 a.\n"
          <> "class RuntimeFlag(a) {\nflag :: a -> Bool.\n}.\n"
          <> "impl RuntimeFlag(Wrap(Int)) {\nflag = \\(wrap) -> True.\n}.\n"
          <> "impl RuntimeFlag(Wrap(Int64)) {\nflag = \\(wrap) -> False.\n}.\n"
          <> "(RuntimeFlag::flag) (Wrap 1 1)."
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "True") (runOutput result)

testQualifiedMethodDispatchTreatsNonLiteralIntegerResultsAsInt64 :: IO ()
testQualifiedMethodDispatchTreatsNonLiteralIntegerResultsAsInt64 = do
  result <-
    runSource
      defaultWarningSettings
      ( "class RuntimeFlag(a) {\nflag :: a -> Bool.\n}.\n"
          <> "impl RuntimeFlag(Int) {\nflag = \\(value) -> True.\n}.\n"
          <> "impl RuntimeFlag(Int64) {\nflag = \\(value) -> False.\n}.\n"
          <> "(RuntimeFlag::flag) ((\\(x) -> x) 1)."
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "False") (runOutput result)

testQualifiedMethodDispatchPreservesHigherOrderBindingSignature :: IO ()
testQualifiedMethodDispatchPreservesHigherOrderBindingSignature = do
  result <-
    runSource
      defaultWarningSettings
      ( "class RuntimeApply(a) {\napply :: (a -> a) -> Bool.\n}.\n"
          <> "impl RuntimeApply(Int) {\napply = \\(fn) -> True.\n}.\n"
          <> "impl RuntimeApply(Bool) {\napply = \\(fn) -> False.\n}.\n"
          <> "idInt :: Int -> Int.\nidInt = \\(value) -> value.\n"
          <> "RuntimeApply::apply idInt."
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "True") (runOutput result)

testQualifiedMethodDispatchPreservesHigherOrderExactSignature :: IO ()
testQualifiedMethodDispatchPreservesHigherOrderExactSignature = do
  result <-
    runSource
      defaultWarningSettings
      ( "class RuntimeApply(a) {\napply :: (a -> a) -> Bool.\n}.\n"
          <> "impl RuntimeApply(Int) {\napply = \\(fn) -> True.\n}.\n"
          <> "impl RuntimeApply(Int64) {\napply = \\(fn) -> False.\n}.\n"
          <> "id64 :: Int64 -> Int64.\nid64 = \\(value) -> value.\n"
          <> "RuntimeApply::apply id64."
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "False") (runOutput result)

testQualifiedMethodDispatchRejectsUnhintedFunctionArgumentExactSelection :: IO ()
testQualifiedMethodDispatchRejectsUnhintedFunctionArgumentExactSelection = do
  result <-
    runSource
      defaultWarningSettings
      ( "class RuntimeApply(a) {\napply :: (a -> a) -> Bool.\n}.\n"
          <> "impl RuntimeApply(Int) {\napply = \\(fn) -> True.\n}.\n"
          <> "impl RuntimeApply(Int64) {\napply = \\(fn) -> False.\n}.\n"
          <> "(RuntimeApply::apply) (\\(value) -> value + 1)."
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
      ( "class RuntimePick(a) {\npick :: Int -> a -> Bool.\n}.\n"
          <> "impl RuntimePick(Int) {\npick = \\(index) -> \\(value) -> False.\n}.\n"
          <> "impl RuntimePick(Bool) {\npick = \\(index) -> \\(value) -> True.\n}.\n"
          <> "one :: Int.\none = 1.\n"
          <> "pickOne = RuntimePick::pick one.\n"
          <> "pickOne True."
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "True") (runOutput result)

testQualifiedMethodDispatchPreservesSelectedMethodSignature :: IO ()
testQualifiedMethodDispatchPreservesSelectedMethodSignature = do
  result <-
    runSource
      defaultWarningSettings
      ( "class Id(a) {\nid :: a -> a.\n}.\n"
          <> "impl Id(Int) {\nid = \\(value) -> value.\n}.\n"
          <> "class RuntimeApply(a) {\napply :: (a -> a) -> Bool.\n}.\n"
          <> "impl RuntimeApply(Int) {\napply = \\(fn) -> True.\n}.\n"
          <> "impl RuntimeApply(Bool) {\napply = \\(fn) -> False.\n}.\n"
          <> "RuntimeApply::apply Id::id."
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
      ( "class RuntimePick(a) {\npick :: a -> Bool.\n}.\n"
          <> "impl RuntimePick(Int) {\npick = \\(value) -> True.\n}.\n"
          <> "impl RuntimePick(UInt8) {\npick = \\(value) -> False.\n}.\n"
          <> "f = \\(value) -> 1.\n"
          <> "result = RuntimePick::pick (f True).\n"
          <> "result."
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "True") (runOutput result)

testQualifiedMethodDispatchPreservesEmptyListBindingSignature :: IO ()
testQualifiedMethodDispatchPreservesEmptyListBindingSignature = do
  result <-
    runSource
      defaultWarningSettings
      ( "class RuntimePick(a) {\npick :: [a] -> Bool.\n}.\n"
          <> "impl RuntimePick(Int) {\npick = \\(values) -> True.\n}.\n"
          <> "impl RuntimePick(Bool) {\npick = \\(values) -> False.\n}.\n"
          <> "values :: [Int].\nvalues = [].\n"
          <> "RuntimePick::pick values."
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "True") (runOutput result)

testQualifiedMethodDispatchPreservesListReturningApplicationSignature :: IO ()
testQualifiedMethodDispatchPreservesListReturningApplicationSignature = do
  result <-
    runSource
      defaultWarningSettings
      ( "class RuntimeFlag(a) {\nflag :: a -> Bool.\n}.\n"
          <> "impl RuntimeFlag([[Int]]) {\nflag = \\(values) -> True.\n}.\n"
          <> "impl RuntimeFlag([[Int64]]) {\nflag = \\(values) -> False.\n}.\n"
          <> "make :: Bool -> [[Int64]].\nmake = \\(enabled) -> [[1], []].\n"
          <> "(RuntimeFlag::flag) (make True)."
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "False") (runOutput result)

testQualifiedMethodDispatchPreservesDollarAppliedListReturningSignature :: IO ()
testQualifiedMethodDispatchPreservesDollarAppliedListReturningSignature = do
  result <-
    runSource
      defaultWarningSettings
      ( "class RuntimeFlag(a) {\nflag :: a -> Bool.\n}.\n"
          <> "impl RuntimeFlag([[Int]]) {\nflag = \\(values) -> True.\n}.\n"
          <> "impl RuntimeFlag([[Int64]]) {\nflag = \\(values) -> False.\n}.\n"
          <> "make :: Bool -> [[Int64]].\nmake = \\(enabled) -> [[1], []].\n"
          <> "(RuntimeFlag::flag) (($) make True)."
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "False") (runOutput result)

testQualifiedMethodDispatchPreservesAdtReturningApplicationSignature :: IO ()
testQualifiedMethodDispatchPreservesAdtReturningApplicationSignature = do
  result <-
    runSource
      defaultWarningSettings
      ( "data Box a = Box a.\n"
          <> "class RuntimeFlag(a) {\nflag :: a -> Bool.\n}.\n"
          <> "impl RuntimeFlag(Box([[Int]])) {\nflag = \\(box) -> True.\n}.\n"
          <> "impl RuntimeFlag(Box([[Int64]])) {\nflag = \\(box) -> False.\n}.\n"
          <> "make = \\(enabled) -> if enabled (Box [[toInt64 1], []]) else (Box [[toInt64 2], []]).\n"
          <> "(RuntimeFlag::flag) (make True)."
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "False") (runOutput result)

testQualifiedMethodDispatchPreservesBranchResultSignature :: IO ()
testQualifiedMethodDispatchPreservesBranchResultSignature = do
  result <-
    runSource
      defaultWarningSettings
      ( "class RuntimeFlag(a) {\nflag :: a -> Bool.\n}.\n"
          <> "impl RuntimeFlag([[Int]]) {\nflag = \\(values) -> True.\n}.\n"
          <> "impl RuntimeFlag([[Int64]]) {\nflag = \\(values) -> False.\n}.\n"
          <> "make64 :: Bool -> [[Int64]].\nmake64 = \\(enabled) -> [[1], []].\n"
          <> "(RuntimeFlag::flag) (if True (make64 True) else (make64 False))."
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "False") (runOutput result)

testQualifiedMethodDispatchPreservesBlockResultSignature :: IO ()
testQualifiedMethodDispatchPreservesBlockResultSignature = do
  result <-
    runSource
      defaultWarningSettings
      ( "class RuntimeFlag(a) {\nflag :: a -> Bool.\n}.\n"
          <> "impl RuntimeFlag([[Int]]) {\nflag = \\(values) -> True.\n}.\n"
          <> "impl RuntimeFlag([[Int64]]) {\nflag = \\(values) -> False.\n}.\n"
          <> "(RuntimeFlag::flag) {\nvalues :: [[Int64]].\nvalues = [[1], []].\nvalues.\n}."
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "False") (runOutput result)

testQualifiedMethodDispatchPreservesMappedEmptyListResultSignature :: IO ()
testQualifiedMethodDispatchPreservesMappedEmptyListResultSignature = do
  result <-
    runSource
      defaultWarningSettings
      ( "class RuntimePick(a) {\npick :: [a] -> Bool.\n}.\n"
          <> "impl RuntimePick(Int) {\npick = \\(values) -> True.\n}.\n"
          <> "impl RuntimePick(UInt8) {\npick = \\(values) -> False.\n}.\n"
          <> "id8 :: UInt8 -> UInt8.\nid8 = \\(value) -> value.\n"
          <> "values :: [UInt8].\nvalues = [].\n"
          <> "mapped = map id8 values.\n"
          <> "RuntimePick::pick mapped."
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "False") (runOutput result)

testQualifiedMethodDispatchPreservesIdentityMappedEmptyListResultSignature :: IO ()
testQualifiedMethodDispatchPreservesIdentityMappedEmptyListResultSignature = do
  result <-
    runSource
      defaultWarningSettings
      ( "class RuntimePick(a) {\npick :: [a] -> Bool.\n}.\n"
          <> "impl RuntimePick(Int) {\npick = \\(values) -> True.\n}.\n"
          <> "impl RuntimePick(UInt8) {\npick = \\(values) -> False.\n}.\n"
          <> "values :: [UInt8].\nvalues = [].\n"
          <> "mapped = map (\\(value) -> value) values.\n"
          <> "RuntimePick::pick mapped."
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "False") (runOutput result)

testQualifiedMethodDispatchPreservesMappedHdEmptyNestedListResultSignature :: IO ()
testQualifiedMethodDispatchPreservesMappedHdEmptyNestedListResultSignature = do
  result <-
    runSource
      defaultWarningSettings
      ( "class RuntimePick(a) {\npick :: [a] -> Bool.\n}.\n"
          <> "impl RuntimePick(Int) {\npick = \\(values) -> True.\n}.\n"
          <> "impl RuntimePick(UInt8) {\npick = \\(values) -> False.\n}.\n"
          <> "values :: [[UInt8]].\nvalues = [].\n"
          <> "mapped = map hd values.\n"
          <> "RuntimePick::pick mapped."
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "False") (runOutput result)

testQualifiedMethodDispatchPreservesHdElementSignature :: IO ()
testQualifiedMethodDispatchPreservesHdElementSignature = do
  result <-
    runSource
      defaultWarningSettings
      ( "class RuntimeEq(a) {\nequals :: a -> a -> Bool.\n}.\n"
          <> "impl RuntimeEq(Int) {\nequals = \\(left) -> \\(right) -> True.\n}.\n"
          <> "impl RuntimeEq(UInt8) {\nequals = \\(left) -> \\(right) -> False.\n}.\n"
          <> "values :: [UInt8].\nvalues = [1].\n"
          <> "left = hd values.\n"
          <> "right = hd values.\n"
          <> "RuntimeEq::equals left right."
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "False") (runOutput result)

testQualifiedMethodDispatchNormalizesHintedListAliases :: IO ()
testQualifiedMethodDispatchNormalizesHintedListAliases = do
  result <-
    runSource
      defaultWarningSettings
      ( "class RuntimePick(a) {\npick :: [a] -> Bool.\n}.\n"
          <> "impl RuntimePick(Int64) {\npick = \\(values) -> True.\n}.\n"
          <> "impl RuntimePick(Bool) {\npick = \\(values) -> False.\n}.\n"
          <> "values :: [Int].\nvalues = [].\n"
          <> "RuntimePick::pick values."
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "True") (runOutput result)

testQualifiedMethodDispatchNormalizesHintedFunctionAliases :: IO ()
testQualifiedMethodDispatchNormalizesHintedFunctionAliases = do
  result <-
    runSource
      defaultWarningSettings
      ( "class RuntimeApply(a) {\napply :: (a -> a) -> Bool.\n}.\n"
          <> "impl RuntimeApply(Int64) {\napply = \\(fn) -> True.\n}.\n"
          <> "impl RuntimeApply(Bool) {\napply = \\(fn) -> False.\n}.\n"
          <> "idInt :: Int -> Int.\nidInt = \\(value) -> value.\n"
          <> "RuntimeApply::apply idInt."
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "True") (runOutput result)

testQualifiedMethodDispatchTreatsDefaultedIntegerBindingAsInt64 :: IO ()
testQualifiedMethodDispatchTreatsDefaultedIntegerBindingAsInt64 = do
  result <-
    runSource
      defaultWarningSettings
      ( "class RuntimePick(a) {\npick :: a -> Bool.\n}.\n"
          <> "impl RuntimePick(Int) {\npick = \\(value) -> True.\n}.\n"
          <> "impl RuntimePick(UInt8) {\npick = \\(value) -> False.\n}.\n"
          <> "value = 1.\n"
          <> "RuntimePick::pick value."
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "True") (runOutput result)

testQualifiedMethodDispatchTreatsPlainIntegerBindingAsInt64WithExactCandidates :: IO ()
testQualifiedMethodDispatchTreatsPlainIntegerBindingAsInt64WithExactCandidates = do
  result <-
    runSource
      defaultWarningSettings
      ( "class RuntimeFlag(a) {\nflag :: a -> Bool.\n}.\n"
          <> "impl RuntimeFlag(Int) {\nflag = \\(value) -> True.\n}.\n"
          <> "impl RuntimeFlag(Int64) {\nflag = \\(value) -> False.\n}.\n"
          <> "value = 1.\n"
          <> "RuntimeFlag::flag value."
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "False") (runOutput result)

testQualifiedMethodDispatchTreatsInferredDirectIntegerLiteralAsExactInt :: IO ()
testQualifiedMethodDispatchTreatsInferredDirectIntegerLiteralAsExactInt = do
  result <-
    runSource
      defaultWarningSettings
      ( "class RuntimeFlag(a) {\nflag :: a -> Bool.\n}.\n"
          <> "impl RuntimeFlag(Int) {\nflag = \\(value) -> True.\n}.\n"
          <> "impl RuntimeFlag(Int64) {\nflag = \\(value) -> False.\n}.\n"
          <> "result = (\\(value) -> RuntimeFlag::flag value) 1.\n"
          <> "result."
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "True") (runOutput result)

testQualifiedMethodDispatchPreservesInferredNarrowIntegerBinding :: IO ()
testQualifiedMethodDispatchPreservesInferredNarrowIntegerBinding = do
  result <-
    runSource
      defaultWarningSettings
      ( "class RuntimePick(a) {\npick :: a -> Bool.\n}.\n"
          <> "impl RuntimePick(Int) {\npick = \\(value) -> True.\n}.\n"
          <> "impl RuntimePick(UInt8) {\npick = \\(value) -> False.\n}.\n"
          <> "value = if True 1 else toUInt8 2.\n"
          <> "RuntimePick::pick value."
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "False") (runOutput result)

testQualifiedMethodDispatchPreservesAdtApplicationBindingHint :: IO ()
testQualifiedMethodDispatchPreservesAdtApplicationBindingHint = do
  result <-
    runSource
      defaultWarningSettings
      ( "data Box a = Box a.\n"
          <> "class RuntimePick(a) {\npick :: a -> Bool.\n}.\n"
          <> "impl RuntimePick(Box(Int)) {\npick = \\(box) -> True.\n}.\n"
          <> "impl RuntimePick(Box(UInt8)) {\npick = \\(box) -> False.\n}.\n"
          <> "box = if True (Box 1) else (Box (toUInt8 2)).\n"
          <> "RuntimePick::pick box."
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
      ( "dummy = 0.\n"
          <> "z = 1.\n"
          <> "x = { y :: UInt8.\ny = 1.\ny. }.\n"
          <> "class RuntimePick(a) {\npick :: a -> Bool.\n}.\n"
          <> "impl RuntimePick(Int) {\npick = \\(value) -> True.\n}.\n"
          <> "impl RuntimePick(UInt8) {\npick = \\(value) -> False.\n}.\n"
          <> "RuntimePick::pick z."
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "True") (runOutput result)

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
      ( "class RuntimeFlag(a) {\nenabled :: Bool.\n}.\n"
          <> "impl RuntimeFlag(Int) {\nenabled = True.\n}.\n"
          <> "RuntimeFlag::enabled."
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
              ( "class RuntimeEq(a) {\nequals :: a -> a -> Bool.\n}.\n"
                  <> "impl RuntimeEq(Int) {\nequals = RuntimeEq::equals.\n}.\n"
                  <> "RuntimeEq::equals 1 1."
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
              ( "class RuntimeEq(a) {\nequals :: a -> a -> Bool.\n}.\n"
                  <> "impl RuntimeEq(Int) {\nequals = if True RuntimeEq::equals else \\(left) -> \\(right) -> left == right.\n}.\n"
                  <> "RuntimeEq::equals 1 1."
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
              ( "class RuntimeFlag(a) {\nenabled :: Bool.\n}.\n"
                  <> "impl RuntimeFlag(Int) {\nenabled = { helper = RuntimeFlag::enabled.\nhelper. }.\n}.\n"
                  <> "RuntimeFlag::enabled."
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
      ( "class RuntimeFlag(a) {\nenabled :: Bool.\non :: Bool.\noff :: Bool.\n}.\n"
          <> "impl RuntimeFlag(Int) {\nenabled = { flag = True.\ntarget = if flag RuntimeFlag::on else RuntimeFlag::off.\ntarget.\n}.\non = True.\noff = False.\n}.\n"
          <> "RuntimeFlag::enabled."
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "True") (runOutput result)

testQualifiedMethodDispatchFollowsBlockLocalAliasBranchesWithLocalSignatureHints :: IO ()
testQualifiedMethodDispatchFollowsBlockLocalAliasBranchesWithLocalSignatureHints = do
  result <-
    runSource
      defaultWarningSettings
      ( "class RuntimeFlag(a) {\nflag :: a -> Bool.\n}.\n"
          <> "impl RuntimeFlag([[Int]]) {\nflag = \\(values) -> False.\n}.\n"
          <> "impl RuntimeFlag([[Int64]]) {\nflag = \\(values) -> True.\n}.\n"
          <> "class RuntimeChoice(a) {\nenabled :: Bool.\non :: Bool.\noff :: Bool.\n}.\n"
          <> "impl RuntimeChoice(Int) {\n"
          <> "enabled = { value :: [[Int64]].\nvalue = [[1], []].\ntarget = if ((RuntimeFlag::flag) value) RuntimeChoice::on else RuntimeChoice::off.\ntarget.\n}.\n"
          <> "on = True.\n"
          <> "off = False.\n"
          <> "}.\n"
          <> "RuntimeChoice::enabled."
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
  result <- runSource defaultWarningSettings (runtimeEqSource <> "data Token = Token Int.\ndata Box a = Box a.\nimpl RuntimeEq(Token) {\nequals = \\(left) -> \\(right) -> True.\n}.\nimpl RuntimeEq(Box(Int)) {\nequals = \\(left) -> \\(right) -> True.\n}.\nresult = (RuntimeEq::equals (Token 1) (Token 2), RuntimeEq::equals (Box 1) (Box 2)).\nresult.")
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "(True, True)") (runOutput result)

testMethodBearingCapabilityDeclarationsRuntimeInert :: IO ()
testMethodBearingCapabilityDeclarationsRuntimeInert = do
  result <- runSource defaultWarningSettings (runtimeEqSource <> "x = 1.\nx.")
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "method-bearing capability declarations do not affect runtime output" (Just "1") (runOutput result)
