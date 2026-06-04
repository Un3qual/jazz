{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Exception
  ( SomeException,
    try
  )
import Data.Text (Text)
import qualified Data.Text as Text
import JazzNext.Compiler.AST
  ( CaseArm (..),
    ClassMethodSignature (..),
    ConstraintSignatureType (..),
    DataConstructorArgument (..),
    DataConstructor (..),
    Expr (..),
    ImplMethod (..),
    Literal (..),
    Pattern (..),
    SignaturePayload (..),
    Statement (..)
  )
import JazzNext.Compiler.Diagnostics
  ( Diagnostic,
    SourceSpan (..),
    renderDiagnostic
  )
import JazzNext.Compiler.Driver
  ( RunResult (..),
    runSource
  )
import JazzNext.Compiler.FractionalLiteral
  ( mkFractionalLiteralSource
  )
import JazzNext.Compiler.Identifier
  ( Identifier
  )
import JazzNext.Compiler.Runtime
  ( RuntimeValue (..),
    evaluateRuntimeExpr
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

main :: IO ()
main = runTestSuite "RuntimeSemantics" tests

tests :: [NamedTest]
tests =
  [ ("if with False condition skips then branch runtime failure", testIfFalseSkipsThenRuntimeFailure),
    ("if with True condition skips else branch runtime failure", testIfTrueSkipsElseRuntimeFailure),
    ("division by zero produces fatal runtime diagnostic", testDivisionByZeroRuntimeError),
    ("direct self alias produces deterministic runtime diagnostic", testDirectSelfAliasRuntimeError),
    ("alias-only recursive cycle produces deterministic runtime diagnostic", testAliasOnlyRecursiveCycleRuntimeError),
    ("wrapped direct self alias produces deterministic runtime diagnostic", testWrappedDirectSelfAliasRuntimeError),
    ("same-name non-alias self application produces runtime unbound diagnostic", testSameNameNonAliasSelfApplicationTerminates),
    ("mixed wrapper with eager selected branch produces runtime unbound diagnostic", testMixedWrapperWithSelectedNonAliasSelfUseTerminates),
    ("block wrapper with eager statement before alias terminal produces runtime unbound diagnostic", testBlockWrapperWithEagerStatementBeforeAliasTerminalTerminates),
    ("wrapped alias-only recursive cycle produces deterministic runtime diagnostic", testWrappedAliasOnlyRecursiveCycleRuntimeError),
    ("mixed wrapped alias cycle still produces deterministic runtime diagnostic", testMixedWrappedAliasCycleRuntimeError),
    ("wrapped alias cycle still evaluates wrapper condition first", testWrappedAliasCycleConditionRuntimeError),
    ("pattern-case alias-only recursive cycle produces deterministic runtime diagnostic", testPatternCaseAliasOnlyRecursiveCycleRuntimeError),
    ("pattern-case binder shadows recursive peer during alias resolution", testPatternCaseBinderDoesNotAliasRecursivePeer),
    ("block-wrapped alias-only recursive cycle produces deterministic runtime diagnostic", testBlockWrappedAliasOnlyRecursiveCycleRuntimeError),
    ("non-function recursive cycle produces deterministic runtime diagnostic", testNonFunctionRecursiveCycleRuntimeError),
    ("nested block alias cycle ignores later outer peer name", testNestedBlockAliasCycleIgnoresLaterOuterPeer),
    ("pattern-case without a matching arm produces deterministic runtime diagnostic", testPatternCaseNoMatchRuntimeError),
    ("constructor over-application produces arity runtime diagnostic", testConstructorOverApplicationRuntimeError),
    ("bare dollar operator value applies at runtime", testDollarOperatorValueRuntimeSuccess),
    ("bare operator value applies at runtime", testBareOperatorValueRuntimeSuccess),
    ("explicit partial application of bare operator value applies at runtime", testExplicitPartialOperatorValueRuntimeSuccess),
    ("left operator section applies at runtime", testLeftOperatorSectionRuntimeSuccess),
    ("right operator section applies at runtime", testRightOperatorSectionRuntimeSuccess),
    ("right section differs from ordinary partial application for division", testRightSectionDiffersFromOrdinaryPartialApplication),
    ("map + hd evaluates over nested list literals", testMapHdNestedListsRuntimeSuccess),
    ("filter keeps only matching list elements", testFilterRuntimeSuccess),
    ("tl returns the tail of a non-empty list", testTlReturnsTailRuntimeValue),
    ("tuple literal evaluates and renders at runtime", testTupleLiteralRuntimeValue),
    ("hd on empty list produces fatal runtime diagnostic", testHdEmptyListRuntimeError),
    ("tl on empty list produces fatal runtime diagnostic", testTlEmptyListRuntimeError),
    ("direct runtime helper rejects canonical prelude alias without bundled prelude", testRuntimeHelperRejectsCanonicalAlias),
    ("runtime fallback rejects kernel hd on non-list values", testRuntimeFallbackRejectsHdNonList),
    ("runtime fallback rejects kernel tl on non-list values", testRuntimeFallbackRejectsTlNonList),
    ("runtime fallback rejects kernel map with non-function mapper", testRuntimeFallbackRejectsMapNonFunctionMapper),
    ("runtime fallback rejects kernel map with non-list collection", testRuntimeFallbackRejectsMapNonListCollection),
    ("runtime fallback rejects kernel filter with non-function predicate", testRuntimeFallbackRejectsFilterNonFunctionPredicate),
    ("runtime fallback rejects kernel filter with non-list collection", testRuntimeFallbackRejectsFilterNonListCollection),
    ("runtime fallback rejects kernel filter predicate returning non-Bool", testRuntimeFallbackRejectsFilterPredicateNonBool),
    ("print! returns evaluated argument value", testPrintBuiltinReturnsArgument),
    ("target-named integer conversion evaluates at runtime", testIntegerConversionRuntimeSuccess),
    ("target-named integer conversion preserves source-exact integral Float literal", testIntegerConversionSourceExactIntegralFloatRuntimeSuccess),
    ("Float64 signature preserves source-exact integral conversion", testFloat64SignaturePreservesSourceExactIntegralConversion),
    ("Float16 signature converts from rounded runtime value", testFloat16SignatureConvertsFromRoundedRuntimeValue),
    ("width-specific integer arithmetic checks preserved result bounds", testWidthSpecificIntegerArithmeticBoundsRuntimeError),
    ("target-named float conversion evaluates at runtime", testFloatConversionRuntimeSuccess),
    ("dynamic integer-to-Float64 overflow checks source magnitude", testDynamicIntegerToFloat64OverflowRuntimeError),
    ("fractional literal evaluates and renders at runtime", testFractionalLiteralRuntimeSuccess),
    ("Float64 arithmetic evaluates at runtime", testFloat64ArithmeticRuntimeSuccess),
    ("Float16 arithmetic preserves target width at runtime", testFloat16ArithmeticPreservesRuntimeWidth),
    ("Float32 arithmetic preserves target width at runtime", testFloat32ArithmeticPreservesRuntimeWidth),
    ("runtime fallback rejects targeted Float16/Float32 mixed with untyped Float arithmetic", testRuntimeFallbackRejectsTargetedNarrowFloatUntypedFloatArithmetic),
    ("runtime fallback rejects mixed targeted float comparison and equality", testRuntimeFallbackRejectsMixedTargetedFloatComparisonEquality),
    ("targeted Float16 and Float32 fractional literals round at runtime", testTargetedFloat16Float32FractionalLiteralRoundsRuntimeValue),
    ("Float16 arithmetic overflow produces runtime diagnostic", testFloat16ArithmeticOverflowRuntimeError),
    ("Float64 arithmetic overflow produces runtime diagnostic", testFloat64ArithmeticOverflowRuntimeError),
    ("Float64 comparison and equality evaluate at runtime", testFloat64ComparisonEqualityRuntimeSuccess),
    ("Float16 and Float32 comparison and equality evaluate at runtime", testFloat16Float32ComparisonEqualityRuntimeSuccess),
    ("targeted Float16 and Float32 fractional literals evaluate through comparison and equality", testTargetedFloat16Float32FractionalLiteralComparisonEqualityRuntimeSuccess),
    ("structural list equality evaluates at runtime", testStructuralListEqualityRuntimeSuccess),
    ("structural tuple equality evaluates at runtime", testStructuralTupleEqualityRuntimeSuccess),
    ("structural ADT equality evaluates at runtime", testStructuralAdtEqualityRuntimeSuccess),
    ("runtime fallback rejects direct callable equality", testRuntimeFallbackRejectsDirectCallableEquality),
    ("runtime fallback rejects direct callable inequality", testRuntimeFallbackRejectsDirectCallableInequality),
    ("runtime fallback rejects mixed targeted integer equality", testRuntimeFallbackRejectsMixedTargetedIntegerEquality),
    ("runtime fallback rejects mixed targeted integer comparison", testRuntimeFallbackRejectsMixedTargetedIntegerComparison),
    ("runtime fallback rejects structural equality over functions", testRuntimeFallbackRejectsFunctionStructuralEquality),
    ("runtime fallback rejects structural equality over qualified methods", testRuntimeFallbackRejectsQualifiedMethodStructuralEquality),
    ("runtime fallback rejects different-length structural equality over functions", testRuntimeFallbackRejectsDifferentLengthFunctionStructuralEquality),
    ("runtime fallback rejects different saturated ADT constructors with function payloads", testRuntimeFallbackRejectsDifferentSaturatedAdtConstructors),
    ("Float16 conversion rounds to target precision", testFloat16ConversionRoundsRuntimeValue),
    ("dynamic integer conversion range failure reports deterministic diagnostic", testDynamicIntegerConversionRangeRuntimeError),
    ("runtime fallback rejects non-numeric conversion values", testRuntimeFallbackRejectsNonNumericConversionValue),
    ("scope with only declarations has no runtime output", testDeclarationOnlyScopeHasNoOutput),
    ("scope with only capability declarations has no runtime output", testCapabilityDeclarationOnlyScopeHasNoOutput),
    ("capability declarations are inert at runtime", testCapabilityDeclarationsRuntimeInert),
    ("qualified method dispatch executes selected impl body", testQualifiedMethodDispatchExecutesImplBody),
    ("let-bound qualified method dispatch executes selected impl body", testLetBoundQualifiedMethodDispatchExecutesImplBody),
    ("qualified method dispatch selects runtime body by argument types", testQualifiedMethodDispatchSelectsRuntimeBodyByArgumentTypes),
    ("qualified method dispatch executes same-impl qualified method call", testQualifiedMethodDispatchExecutesSameImplQualifiedMethodCall),
    ("qualified method dispatch selects width-specific integer body", testQualifiedMethodDispatchSelectsWidthSpecificIntegerBody),
    ("qualified method dispatch selects width-specific integer body for direct literals", testQualifiedMethodDispatchSelectsWidthSpecificIntegerBodyForDirectLiterals),
    ("qualified method dispatch preserves non-literal integer signature targets", testQualifiedMethodDispatchPreservesNonLiteralIntegerSignatureTarget),
    ("qualified method dispatch preserves direct closure result signatures", testQualifiedMethodDispatchPreservesDirectClosureResultSignature),
    ("qualified method dispatch preserves tuple binding signatures", testQualifiedMethodDispatchPreservesTupleBindingSignature),
    ("qualified method dispatch preserves section binding signatures", testQualifiedMethodDispatchPreservesSectionBindingSignature),
    ("qualified method dispatch treats Float as Float64 alias at runtime", testQualifiedMethodDispatchTreatsFloatAsFloat64Alias),
    ("qualified method dispatch treats Int as Int64 alias at runtime", testQualifiedMethodDispatchTreatsIntAsInt64Alias),
    ("qualified method dispatch preserves higher-order binding signatures", testQualifiedMethodDispatchPreservesHigherOrderBindingSignature),
    ("qualified method dispatch preserves selected method signatures", testQualifiedMethodDispatchPreservesSelectedMethodSignature),
    ("qualified method dispatch preserves empty list binding signatures", testQualifiedMethodDispatchPreservesEmptyListBindingSignature),
    ("qualified method dispatch preserves mapped empty list result signatures", testQualifiedMethodDispatchPreservesMappedEmptyListResultSignature),
    ("qualified method dispatch preserves identity-mapped empty list result signatures", testQualifiedMethodDispatchPreservesIdentityMappedEmptyListResultSignature),
    ("qualified method dispatch preserves mapped hd empty nested list result signatures", testQualifiedMethodDispatchPreservesMappedHdEmptyNestedListResultSignature),
    ("qualified method dispatch preserves hd element signatures", testQualifiedMethodDispatchPreservesHdElementSignature),
    ("qualified method dispatch normalizes hinted list aliases", testQualifiedMethodDispatchNormalizesHintedListAliases),
    ("qualified method dispatch normalizes hinted function aliases", testQualifiedMethodDispatchNormalizesHintedFunctionAliases),
    ("qualified method dispatch treats defaulted integer bindings as Int64", testQualifiedMethodDispatchTreatsDefaultedIntegerBindingAsInt64),
    ("qualified method dispatch preserves inferred narrow integer bindings", testQualifiedMethodDispatchPreservesInferredNarrowIntegerBinding),
    ("qualified method dispatch recursively defaults bound integer literals", testQualifiedMethodDispatchRecursivelyDefaultsBoundIntegerLiterals),
    ("qualified method dispatch preserves ADT application binding hints", testQualifiedMethodDispatchPreservesAdtApplicationBindingHint),
    ("qualified method dispatch prefers alias binding over method sentinel at runtime", testQualifiedMethodDispatchPrefersAliasBindingOverMethodSentinelAtRuntime),
    ("qualified zero-argument method dispatch returns value", testQualifiedZeroArgumentMethodDispatchReturnsValue),
    ("qualified method dispatch rejects direct self alias", testQualifiedMethodDispatchRejectsDirectSelfAlias),
    ("qualified method dispatch rejects wrapped self alias", testQualifiedMethodDispatchRejectsWrappedSelfAlias),
    ("qualified method dispatch rejects mutual method alias cycle", testQualifiedMethodDispatchRejectsMutualMethodAliasCycle),
    ("qualified method dispatch rejects full-arity runtime ambiguity", testQualifiedMethodDispatchRejectsFullArityRuntimeAmbiguity),
    ("qualified method dispatch executes local ADT impl body", testQualifiedMethodDispatchExecutesLocalAdtImplBody),
    ("method-bearing capability declarations are inert at runtime", testMethodBearingCapabilityDeclarationsRuntimeInert),
    ("scope result requires terminal expression", testScopeDeclarationAfterExprClearsResult)
  ]

testIfFalseSkipsThenRuntimeFailure :: IO ()
testIfFalseSkipsThenRuntimeFailure = do
  result <- runSource defaultWarningSettings "if False (1 / 0) else 2."
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "2") (runOutput result)

testIfTrueSkipsElseRuntimeFailure :: IO ()
testIfTrueSkipsElseRuntimeFailure = do
  result <- runSource defaultWarningSettings "if True 1 else (1 / 0)."
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "1") (runOutput result)

testDivisionByZeroRuntimeError :: IO ()
testDivisionByZeroRuntimeError = do
  result <- runSource defaultWarningSettings "1 / 0."
  let runtimeErrors = runRuntimeErrors result
  assertEqual "compile errors" [] (runCompileErrors result)
  assertSingleDiagnosticContains
    "runtime fatal division by zero"
    "E3001"
    runtimeErrors
  case runtimeErrors of
    [] ->
      fail "expected division-by-zero runtime error, but got no runtime errors"
    runtimeError : _ ->
      assertContains
        "runtime fatal mentions division by zero"
        "division by zero"
        (renderDiagnostic runtimeError)
  assertEqual "runtime output is suppressed on runtime failure" Nothing (runOutput result)

testDirectSelfAliasRuntimeError :: IO ()
testDirectSelfAliasRuntimeError = do
  maybeResult <- timeout 1000000 (try (runSource defaultWarningSettings "f = f. f.") :: IO (Either SomeException RunResult))
  case maybeResult of
    Nothing ->
      failTest "expected direct self alias to terminate with a runtime diagnostic, but evaluation timed out"
    Just (Left err) ->
      failTest ("expected deterministic runtime diagnostic for direct self alias, but evaluation raised " <> Text.pack (show err))
    Just (Right result) -> do
      assertEqual "compile errors" [] (runCompileErrors result)
      assertSingleDiagnosticContains
        "direct self alias runtime code"
        "E3021"
        (runRuntimeErrors result)
      assertSingleDiagnosticContains
        "direct self alias runtime text"
        "recursive alias cycle"
        (runRuntimeErrors result)
      assertEqual "runtime output is suppressed on runtime failure" Nothing (runOutput result)

testAliasOnlyRecursiveCycleRuntimeError :: IO ()
testAliasOnlyRecursiveCycleRuntimeError = do
  maybeResult <- timeout 1000000 (try (runSource defaultWarningSettings "even = odd. odd = even. even.") :: IO (Either SomeException RunResult))
  case maybeResult of
    Nothing ->
      failTest "expected alias-only recursive cycle to terminate with a runtime diagnostic, but evaluation timed out"
    Just (Left err) ->
      failTest ("expected deterministic runtime diagnostic, but evaluation raised " <> Text.pack (show err))
    Just (Right result) -> do
      assertEqual "compile errors" [] (runCompileErrors result)
      assertSingleDiagnosticContains
        "alias-only recursive cycle runtime code"
        "E3021"
        (runRuntimeErrors result)
      assertSingleDiagnosticContains
        "alias-only recursive cycle runtime text"
        "recursive alias cycle"
        (runRuntimeErrors result)
      assertEqual "runtime output is suppressed on runtime failure" Nothing (runOutput result)

testWrappedDirectSelfAliasRuntimeError :: IO ()
testWrappedDirectSelfAliasRuntimeError = do
  maybeResult <- timeout 1000000 (try (runSource defaultWarningSettings "f = if True f else 0. f.") :: IO (Either SomeException RunResult))
  case maybeResult of
    Nothing ->
      failTest "expected wrapped direct self alias to terminate with a runtime diagnostic, but evaluation timed out"
    Just (Left err) ->
      failTest ("expected deterministic runtime diagnostic for wrapped direct self alias, but evaluation raised " <> Text.pack (show err))
    Just (Right result) -> do
      assertEqual "compile errors" [] (runCompileErrors result)
      assertSingleDiagnosticContains
        "wrapped direct self alias runtime code"
        "E3021"
        (runRuntimeErrors result)
      assertSingleDiagnosticContains
        "wrapped direct self alias runtime text"
        "recursive alias cycle"
        (runRuntimeErrors result)
      assertEqual "runtime output is suppressed on runtime failure" Nothing (runOutput result)

testSameNameNonAliasSelfApplicationTerminates :: IO ()
testSameNameNonAliasSelfApplicationTerminates = do
  maybeResult <- timeout 1000000 (try (runSource defaultWarningSettings "f = (\\(x) -> x) f. f 1.") :: IO (Either SomeException RunResult))
  case maybeResult of
    Nothing ->
      failTest "expected same-name non-alias self application to terminate with a runtime diagnostic, but evaluation timed out"
    Just (Left err) ->
      failTest ("expected same-name non-alias self application to report a runtime diagnostic, but evaluation raised " <> Text.pack (show err))
    Just (Right result) -> do
      assertEqual "compile errors" [] (runCompileErrors result)
      assertSingleDiagnosticContains
        "same-name non-alias self application runtime code"
        "E3002"
        (runRuntimeErrors result)
      assertSingleDiagnosticContains
        "same-name non-alias self application runtime text"
        "unbound variable 'f'"
        (runRuntimeErrors result)
      assertEqual "runtime output is suppressed on failure" Nothing (runOutput result)

testMixedWrapperWithSelectedNonAliasSelfUseTerminates :: IO ()
testMixedWrapperWithSelectedNonAliasSelfUseTerminates = do
  maybeResult <- timeout 1000000 (try (runSource defaultWarningSettings "f = if True (f + 1) else f. 0.") :: IO (Either SomeException RunResult))
  case maybeResult of
    Nothing ->
      failTest "expected mixed wrapper with eager selected self use to terminate with a runtime diagnostic, but evaluation timed out"
    Just (Left err) ->
      failTest ("expected mixed wrapper with eager selected self use to report a runtime diagnostic, but evaluation raised " <> Text.pack (show err))
    Just (Right result) -> do
      assertEqual "compile errors" [] (runCompileErrors result)
      assertSingleDiagnosticContains
        "mixed wrapper selected branch runtime code"
        "E3002"
        (runRuntimeErrors result)
      assertSingleDiagnosticContains
        "mixed wrapper selected branch runtime text"
        "unbound variable 'f'"
        (runRuntimeErrors result)
      assertEqual "runtime output is suppressed on failure" Nothing (runOutput result)

testBlockWrapperWithEagerStatementBeforeAliasTerminalTerminates :: IO ()
testBlockWrapperWithEagerStatementBeforeAliasTerminalTerminates = do
  maybeResult <- timeout 1000000 (try (runSource defaultWarningSettings "f = { f + 1. f. }. 0.") :: IO (Either SomeException RunResult))
  case maybeResult of
    Nothing ->
      failTest "expected block wrapper with eager statement before alias terminal to terminate with a runtime diagnostic, but evaluation timed out"
    Just (Left err) ->
      failTest ("expected block wrapper with eager statement before alias terminal to report a runtime diagnostic, but evaluation raised " <> Text.pack (show err))
    Just (Right result) -> do
      assertEqual "compile errors" [] (runCompileErrors result)
      assertSingleDiagnosticContains
        "block wrapper eager statement runtime code"
        "E3002"
        (runRuntimeErrors result)
      assertSingleDiagnosticContains
        "block wrapper eager statement runtime text"
        "unbound variable 'f'"
        (runRuntimeErrors result)
      assertEqual "runtime output is suppressed on failure" Nothing (runOutput result)

testWrappedAliasOnlyRecursiveCycleRuntimeError :: IO ()
testWrappedAliasOnlyRecursiveCycleRuntimeError = do
  maybeResult <- timeout 1000000 (try (runSource defaultWarningSettings "f = if True g else g. g = f. f.") :: IO (Either SomeException RunResult))
  case maybeResult of
    Nothing ->
      failTest "expected wrapped alias-only recursive cycle to terminate with a runtime diagnostic, but evaluation timed out"
    Just (Left err) ->
      failTest ("expected deterministic runtime diagnostic for wrapped alias cycle, but evaluation raised " <> Text.pack (show err))
    Just (Right result) -> do
      assertEqual "compile errors" [] (runCompileErrors result)
      assertSingleDiagnosticContains
        "wrapped alias-only recursive cycle runtime code"
        "E3021"
        (runRuntimeErrors result)
      assertSingleDiagnosticContains
        "wrapped alias-only recursive cycle runtime text"
        "recursive alias cycle"
        (runRuntimeErrors result)
      assertEqual "runtime output is suppressed on runtime failure" Nothing (runOutput result)

testMixedWrappedAliasCycleRuntimeError :: IO ()
testMixedWrappedAliasCycleRuntimeError = do
  maybeResult <- timeout 1000000 (try (runSource defaultWarningSettings "f = if True g else \\(x) -> x. g = f. f 1.") :: IO (Either SomeException RunResult))
  case maybeResult of
    Nothing ->
      failTest "expected mixed wrapped alias cycle to terminate with a runtime diagnostic, but evaluation timed out"
    Just (Left err) ->
      failTest ("expected deterministic runtime diagnostic for mixed wrapped alias cycle, but evaluation raised " <> Text.pack (show err))
    Just (Right result) -> do
      assertEqual "compile errors" [] (runCompileErrors result)
      assertSingleDiagnosticContains
        "mixed wrapped alias cycle runtime code"
        "E3021"
        (runRuntimeErrors result)
      assertSingleDiagnosticContains
        "mixed wrapped alias cycle runtime text"
        "recursive alias cycle"
        (runRuntimeErrors result)
      assertEqual "runtime output is suppressed on runtime failure" Nothing (runOutput result)

testWrappedAliasCycleConditionRuntimeError :: IO ()
testWrappedAliasCycleConditionRuntimeError = do
  maybeResult <- timeout 1000000 (try (runSource defaultWarningSettings "f = if (1 / 0 == 0) g else g. g = f. f.") :: IO (Either SomeException RunResult))
  case maybeResult of
    Nothing ->
      failTest "expected wrapped alias cycle condition failure to terminate with a runtime diagnostic, but evaluation timed out"
    Just (Left err) ->
      failTest ("expected wrapped alias cycle condition failure to return a runtime diagnostic, but evaluation raised " <> Text.pack (show err))
    Just (Right result) -> do
      assertEqual "compile errors" [] (runCompileErrors result)
      assertSingleDiagnosticContains
        "wrapped alias cycle condition runtime code"
        "E3001"
        (runRuntimeErrors result)
      assertSingleDiagnosticContains
        "wrapped alias cycle condition runtime text"
        "division by zero"
        (runRuntimeErrors result)
      assertEqual "runtime output is suppressed on runtime failure" Nothing (runOutput result)

testPatternCaseAliasOnlyRecursiveCycleRuntimeError :: IO ()
testPatternCaseAliasOnlyRecursiveCycleRuntimeError = do
  maybeResult <- timeout 1000000 (try (runSource defaultWarningSettings "x = case 0 { | 0 -> y }. y = x. x.") :: IO (Either SomeException RunResult))
  case maybeResult of
    Nothing ->
      failTest "expected pattern-case alias-only recursive cycle to terminate with a runtime diagnostic, but evaluation timed out"
    Just (Left err) ->
      failTest ("expected deterministic runtime diagnostic for pattern-case alias cycle, but evaluation raised " <> Text.pack (show err))
    Just (Right result) -> do
      assertEqual "compile errors" [] (runCompileErrors result)
      assertSingleDiagnosticContains
        "pattern-case alias cycle runtime code"
        "E3021"
        (runRuntimeErrors result)
      assertSingleDiagnosticContains
        "pattern-case alias cycle runtime text"
        "recursive alias cycle"
        (runRuntimeErrors result)
      assertEqual "runtime output is suppressed on runtime failure" Nothing (runOutput result)

testPatternCaseBinderDoesNotAliasRecursivePeer :: IO ()
testPatternCaseBinderDoesNotAliasRecursivePeer = do
  result <- runSource defaultWarningSettings "x = case 0 { | y -> y }. y = x. x."
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "0") (runOutput result)

testBlockWrappedAliasOnlyRecursiveCycleRuntimeError :: IO ()
testBlockWrappedAliasOnlyRecursiveCycleRuntimeError = do
  maybeResult <- timeout 1000000 (try (runSource defaultWarningSettings "a = { b. }. b = { a. }. a.") :: IO (Either SomeException RunResult))
  case maybeResult of
    Nothing ->
      failTest "expected block-wrapped alias-only recursive cycle to terminate with a runtime diagnostic, but evaluation timed out"
    Just (Left err) ->
      failTest ("expected deterministic runtime diagnostic for block-wrapped alias cycle, but evaluation raised " <> Text.pack (show err))
    Just (Right result) -> do
      assertEqual "compile errors" [] (runCompileErrors result)
      assertSingleDiagnosticContains
        "block-wrapped alias-only recursive cycle runtime code"
        "E3021"
        (runRuntimeErrors result)
      assertSingleDiagnosticContains
        "block-wrapped alias-only recursive cycle runtime text"
        "recursive alias cycle"
        (runRuntimeErrors result)
      assertEqual "runtime output is suppressed on runtime failure" Nothing (runOutput result)

testNonFunctionRecursiveCycleRuntimeError :: IO ()
testNonFunctionRecursiveCycleRuntimeError = do
  maybeResult <- timeout 1000000 (try (runSource defaultWarningSettings "x = y + 1. y = x + 1. x.") :: IO (Either SomeException RunResult))
  case maybeResult of
    Nothing ->
      failTest "expected non-function recursive cycle to terminate with a runtime diagnostic, but evaluation timed out"
    Just (Left err) ->
      failTest ("expected deterministic runtime diagnostic for non-function recursive cycle, but evaluation raised " <> Text.pack (show err))
    Just (Right result) -> do
      assertEqual "compile errors" [] (runCompileErrors result)
      assertSingleDiagnosticContains
        "non-function recursive cycle runtime code"
        "E3021"
        (runRuntimeErrors result)
      assertSingleDiagnosticContains
        "non-function recursive cycle runtime text"
        "no concrete value"
        (runRuntimeErrors result)
      assertEqual "runtime output is suppressed on runtime failure" Nothing (runOutput result)

testNestedBlockAliasCycleIgnoresLaterOuterPeer :: IO ()
testNestedBlockAliasCycleIgnoresLaterOuterPeer = do
  maybeResult <- timeout 1000000 (try (runSource defaultWarningSettings "x = { y = z. z = y. y. }. z = x. x.") :: IO (Either SomeException RunResult))
  case maybeResult of
    Nothing ->
      failTest "expected nested block alias cycle to terminate with a runtime diagnostic, but evaluation timed out"
    Just (Left err) ->
      failTest ("expected nested block alias cycle to report a deterministic runtime diagnostic, but evaluation raised " <> Text.pack (show err))
    Just (Right result) -> do
      assertEqual "compile errors" [] (runCompileErrors result)
      assertSingleDiagnosticContains
        "nested block alias cycle runtime code"
        "E3021"
        (runRuntimeErrors result)
      assertSingleDiagnosticContains
        "nested block alias cycle runtime text"
        "recursive alias cycle"
        (runRuntimeErrors result)
      assertEqual "runtime output is suppressed on runtime failure" Nothing (runOutput result)

testPatternCaseNoMatchRuntimeError :: IO ()
testPatternCaseNoMatchRuntimeError = do
  let result = evaluateRuntimeExpr patternCaseNoMatchExpr
  assertLeftDiagnosticCodeAndContains
    "pattern-case no-match runtime code"
    "E3022"
    "matched no arms"
    result

patternCaseNoMatchExpr :: Expr
patternCaseNoMatchExpr =
  EPatternCase
    (ELit (LInt 1))
    [ CaseArm
        (PLiteral (LInt 0))
        (ELit (LInt 2))
    ]

testConstructorOverApplicationRuntimeError :: IO ()
testConstructorOverApplicationRuntimeError = do
  let result = evaluateRuntimeExpr overAppliedConstructorExpr
  assertLeftDiagnosticCodeAndContains
    "constructor over-application runtime code"
    "E3023"
    "constructor 'Just' expected 1 argument but received 2"
    result

overAppliedConstructorExpr :: Expr
overAppliedConstructorExpr =
  EBlock
    [ SData
        (SourceSpan 1 1)
        "Maybe"
        []
        [DataConstructor "Just" [DataConstructorArgumentName "value"]],
      SExpr
        (SourceSpan 1 20)
        (EApply (EApply (EVar "Just") (ELit (LInt 1))) (ELit (LInt 2)))
    ]

testDollarOperatorValueRuntimeSuccess :: IO ()
testDollarOperatorValueRuntimeSuccess = do
  result <- runSource defaultWarningSettings "($) (1 +) 2."
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "3") (runOutput result)

testBareOperatorValueRuntimeSuccess :: IO ()
testBareOperatorValueRuntimeSuccess = do
  result <- runSource defaultWarningSettings "(+) 1 2."
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "3") (runOutput result)

testExplicitPartialOperatorValueRuntimeSuccess :: IO ()
testExplicitPartialOperatorValueRuntimeSuccess = do
  result <- runSource defaultWarningSettings "((+) 1) 2."
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "3") (runOutput result)

testLeftOperatorSectionRuntimeSuccess :: IO ()
testLeftOperatorSectionRuntimeSuccess = do
  result <- runSource defaultWarningSettings "(1 +) 2."
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "3") (runOutput result)

testRightOperatorSectionRuntimeSuccess :: IO ()
testRightOperatorSectionRuntimeSuccess = do
  result <- runSource defaultWarningSettings "(+ 1) 2."
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "3") (runOutput result)

testRightSectionDiffersFromOrdinaryPartialApplication :: IO ()
testRightSectionDiffersFromOrdinaryPartialApplication = do
  rightSectionResult <- runSource defaultWarningSettings "(/ 2) 10."
  partialApplicationResult <- runSource defaultWarningSettings "((/) 2) 10."
  assertEqual "right section compile errors" [] (runCompileErrors rightSectionResult)
  assertEqual "right section runtime errors" [] (runRuntimeErrors rightSectionResult)
  assertEqual "right section runtime output" (Just "5") (runOutput rightSectionResult)
  assertEqual "partial application compile errors" [] (runCompileErrors partialApplicationResult)
  assertEqual "partial application runtime errors" [] (runRuntimeErrors partialApplicationResult)
  assertEqual "partial application runtime output" (Just "0") (runOutput partialApplicationResult)

testMapHdNestedListsRuntimeSuccess :: IO ()
testMapHdNestedListsRuntimeSuccess = do
  result <- runSource defaultWarningSettings "map hd [[1, 2], [3], [4, 5]]."
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "[1, 3, 4]") (runOutput result)

testFilterRuntimeSuccess :: IO ()
testFilterRuntimeSuccess = do
  result <- runSource defaultWarningSettings "filter (> 1) [1, 2, 3, 1]."
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "[2, 3]") (runOutput result)

testTlReturnsTailRuntimeValue :: IO ()
testTlReturnsTailRuntimeValue = do
  result <- runSource defaultWarningSettings "tl [1, 2, 3]."
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "[2, 3]") (runOutput result)

testTupleLiteralRuntimeValue :: IO ()
testTupleLiteralRuntimeValue = do
  result <- runSource defaultWarningSettings "(1, True)."
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "(1, True)") (runOutput result)

testHdEmptyListRuntimeError :: IO ()
testHdEmptyListRuntimeError = do
  result <- runSource defaultWarningSettings "hd []."
  let runtimeErrors = runRuntimeErrors result
  assertEqual "compile errors" [] (runCompileErrors result)
  assertSingleDiagnosticContains
    "runtime fatal empty-list hd"
    "E3009"
    runtimeErrors
  case runtimeErrors of
    [] ->
      fail "expected empty-list hd runtime error, but got no runtime errors"
    runtimeError : _ ->
      assertContains
        "runtime fatal mentions empty list"
        "empty list"
        (renderDiagnostic runtimeError)
  assertEqual "runtime output is suppressed on runtime failure" Nothing (runOutput result)

testTlEmptyListRuntimeError :: IO ()
testTlEmptyListRuntimeError = do
  result <- runSource defaultWarningSettings "tl []."
  let runtimeErrors = runRuntimeErrors result
  assertEqual "compile errors" [] (runCompileErrors result)
  assertSingleDiagnosticContains
    "runtime fatal empty-list tl"
    "E3010"
    runtimeErrors
  case runtimeErrors of
    [] ->
      fail "expected empty-list tl runtime error, but got no runtime errors"
    runtimeError : _ ->
      assertContains
        "runtime fatal mentions empty list"
        "empty list"
        (renderDiagnostic runtimeError)
  assertEqual "runtime output is suppressed on runtime failure" Nothing (runOutput result)

testRuntimeHelperRejectsCanonicalAlias :: IO ()
testRuntimeHelperRejectsCanonicalAlias = do
  let result = evaluateRuntimeExpr (runtimeExpr (EVar "map"))
  assertRuntimeErrorContains "runtime helper canonical alias rejected" "E3002" result

testRuntimeFallbackRejectsHdNonList :: IO ()
testRuntimeFallbackRejectsHdNonList = do
  let result = evaluateRuntimeExpr (runtimeExpr (EApply (EVar "__kernel_hd") (ELit (LInt 1))))
  assertRuntimeErrorContains "runtime fallback hd non-list" "E3011" result

testRuntimeFallbackRejectsTlNonList :: IO ()
testRuntimeFallbackRejectsTlNonList = do
  let result = evaluateRuntimeExpr (runtimeExpr (EApply (EVar "__kernel_tl") (ELit (LInt 1))))
  assertRuntimeErrorContains "runtime fallback tl non-list" "E3012" result

testRuntimeFallbackRejectsMapNonFunctionMapper :: IO ()
testRuntimeFallbackRejectsMapNonFunctionMapper = do
  let result = evaluateRuntimeExpr (runtimeExpr (EApply (EApply (EVar "__kernel_map") (ELit (LInt 1))) (EList [ELit (LInt 1)])))
  assertRuntimeErrorContains "runtime fallback map mapper" "E3015" result

testRuntimeFallbackRejectsMapNonListCollection :: IO ()
testRuntimeFallbackRejectsMapNonListCollection = do
  let result = evaluateRuntimeExpr (runtimeExpr (EApply (EApply (EVar "__kernel_map") (EVar "__kernel_hd")) (ELit (LInt 1))))
  assertRuntimeErrorContains "runtime fallback map collection" "E3013" result

testRuntimeFallbackRejectsFilterNonFunctionPredicate :: IO ()
testRuntimeFallbackRejectsFilterNonFunctionPredicate = do
  let result = evaluateRuntimeExpr (runtimeExpr (EApply (EApply (EVar "__kernel_filter") (ELit (LInt 1))) (EList [ELit (LInt 1)])))
  assertRuntimeErrorContains "runtime fallback filter predicate" "E3017" result

testRuntimeFallbackRejectsFilterNonListCollection :: IO ()
testRuntimeFallbackRejectsFilterNonListCollection = do
  let result = evaluateRuntimeExpr (runtimeExpr (EApply (EApply (EVar "__kernel_filter") (ESectionLeft (ELit (LInt 1)) "<")) (ELit (LInt 1))))
  assertRuntimeErrorContains "runtime fallback filter collection" "E3018" result

testRuntimeFallbackRejectsFilterPredicateNonBool :: IO ()
testRuntimeFallbackRejectsFilterPredicateNonBool = do
  let result = evaluateRuntimeExpr (runtimeExpr (EApply (EApply (EVar "__kernel_filter") (ESectionLeft (ELit (LInt 1)) "+")) (EList [ELit (LInt 1)])))
  assertRuntimeErrorContains "runtime fallback filter predicate bool result" "E3019" result

testPrintBuiltinReturnsArgument :: IO ()
testPrintBuiltinReturnsArgument = do
  result <- runSource defaultWarningSettings "print! 1."
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "1") (runOutput result)

testIntegerConversionRuntimeSuccess :: IO ()
testIntegerConversionRuntimeSuccess = do
  result <- runSource defaultWarningSettings "toUInt8 255."
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "255") (runOutput result)

testIntegerConversionSourceExactIntegralFloatRuntimeSuccess :: IO ()
testIntegerConversionSourceExactIntegralFloatRuntimeSuccess = do
  result <- runSource defaultWarningSettings "toInt64 9223372036854775807.0."
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "9223372036854775807") (runOutput result)

testFloat64SignaturePreservesSourceExactIntegralConversion :: IO ()
testFloat64SignaturePreservesSourceExactIntegralConversion = do
  result <- runSource defaultWarningSettings "value :: Float64.\nvalue = 9223372036854775807.0.\ntoInt64 value."
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "9223372036854775807") (runOutput result)

testFloat16SignatureConvertsFromRoundedRuntimeValue :: IO ()
testFloat16SignatureConvertsFromRoundedRuntimeValue = do
  result <- runSource defaultWarningSettings "value :: Float16.\nvalue = 2049.0.\ntoInt64 value."
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "2048") (runOutput result)

testWidthSpecificIntegerArithmeticBoundsRuntimeError :: IO ()
testWidthSpecificIntegerArithmeticBoundsRuntimeError = do
  result <- runSource defaultWarningSettings "value = toUInt8 255.\nvalue + 1."
  assertEqual "compile errors" [] (runCompileErrors result)
  assertSingleDiagnosticContains
    "UInt8 arithmetic overflow runtime code"
    "E3025"
    (runRuntimeErrors result)
  assertSingleDiagnosticContains
    "UInt8 arithmetic overflow runtime text"
    "outside UInt8 range"
    (runRuntimeErrors result)
  assertEqual "runtime output is suppressed on runtime failure" Nothing (runOutput result)

testFloatConversionRuntimeSuccess :: IO ()
testFloatConversionRuntimeSuccess = do
  result <- runSource defaultWarningSettings "toFloat64 1."
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "1.0") (runOutput result)

testDynamicIntegerToFloat64OverflowRuntimeError :: IO ()
testDynamicIntegerToFloat64OverflowRuntimeError = do
  let justAboveFloat64MaxInteger = show ((floor (1.7976931348623157e308 :: Double) :: Integer) + 1)
      source = Text.pack ("x = " <> justAboveFloat64MaxInteger <> ".\ntoFloat64 x.")
  result <- runSource defaultWarningSettings source
  assertEqual "compile errors" [] (runCompileErrors result)
  assertSingleDiagnosticContains
    "dynamic integer-to-Float64 overflow runtime code"
    "E3024"
    (runRuntimeErrors result)
  assertSingleDiagnosticContains
    "dynamic integer-to-Float64 overflow runtime text"
    "finite Float64"
    (runRuntimeErrors result)
  assertEqual "runtime output is suppressed on runtime failure" Nothing (runOutput result)

testFractionalLiteralRuntimeSuccess :: IO ()
testFractionalLiteralRuntimeSuccess = do
  result <- runSource defaultWarningSettings "1.25."
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "1.25") (runOutput result)

testFloat64ArithmeticRuntimeSuccess :: IO ()
testFloat64ArithmeticRuntimeSuccess = do
  result <- runSource defaultWarningSettings "((7.5 - 1.5) * 2.0) / 3.0 + 0.25."
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "4.25") (runOutput result)

testFloat16ArithmeticPreservesRuntimeWidth :: IO ()
testFloat16ArithmeticPreservesRuntimeWidth = do
  result <- runSource defaultWarningSettings "left :: Float16.\nleft = 2048.0.\none :: Float16.\none = 1.0.\nthree :: Float16.\nthree = 3.0.\nmulLeft :: Float16.\nmulLeft = 683.0.\nadd16 :: Float16.\nadd16 = left + one.\nsub16 :: Float16.\nsub16 = add16 - one.\nmul16 :: Float16.\nmul16 = mulLeft * three.\ndiv16 :: Float16.\ndiv16 = add16 / one.\n(add16, sub16, mul16, div16)."
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "(2048.0, 2047.0, 2048.0, 2048.0)") (runOutput result)

testFloat32ArithmeticPreservesRuntimeWidth :: IO ()
testFloat32ArithmeticPreservesRuntimeWidth = do
  result <- runSource defaultWarningSettings "one :: Float32.\none = 1.0.\nepsilon :: Float32.\nepsilon = 0.00000001.\nadd32 :: Float32.\nadd32 = one + epsilon.\nsub32 :: Float32.\nsub32 = add32 - epsilon.\nmul32 :: Float32.\nmul32 = one * add32.\ndiv32 :: Float32.\ndiv32 = add32 / one.\n(add32, sub32, mul32, div32)."
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "(1.0, 1.0, 1.0, 1.0)") (runOutput result)

testRuntimeFallbackRejectsTargetedNarrowFloatUntypedFloatArithmetic :: IO ()
testRuntimeFallbackRejectsTargetedNarrowFloatUntypedFloatArithmetic = do
  assertRuntimeErrorContains
    "runtime fallback Float16 plus untyped Float"
    "E3007"
    (evaluateRuntimeExpr (runtimeExpr (EBinary "+" (targetedFloat "__kernel_toFloat16") untypedFloatOne)))
  assertRuntimeErrorContains
    "runtime fallback untyped Float plus Float32"
    "E3007"
    (evaluateRuntimeExpr (runtimeExpr (EBinary "+" untypedFloatOne (targetedFloat "__kernel_toFloat32"))))

testRuntimeFallbackRejectsMixedTargetedFloatComparisonEquality :: IO ()
testRuntimeFallbackRejectsMixedTargetedFloatComparisonEquality = do
  assertRuntimeErrorContains
    "runtime fallback Float16 less-than Float32"
    "E3007"
    (evaluateRuntimeExpr (runtimeExpr (EBinary "<" (targetedFloat "__kernel_toFloat16") (targetedFloat "__kernel_toFloat32"))))
  assertRuntimeErrorContains
    "runtime fallback Float16 equality Float64"
    "E3007"
    (evaluateRuntimeExpr (runtimeExpr (EBinary "==" (targetedFloat "__kernel_toFloat16") (targetedFloat "__kernel_toFloat64"))))
  assertRuntimeErrorContains
    "runtime fallback Float32 inequality untyped Float"
    "E3007"
    (evaluateRuntimeExpr (runtimeExpr (EBinary "!=" (targetedFloat "__kernel_toFloat32") untypedFloatOne)))

testTargetedFloat16Float32FractionalLiteralRoundsRuntimeValue :: IO ()
testTargetedFloat16Float32FractionalLiteralRoundsRuntimeValue = do
  result <- runSource defaultWarningSettings "x16 :: Float16.\nx16 = 2049.0.\nx32 :: Float32.\nx32 = 1.00000001.\ny16 :: @{}: Float16.\ny16 = 2049.0.\ny32 :: @{}: Float32.\ny32 = 1.00000001.\n(x16, x32, y16, y32)."
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "(2048.0, 1.0, 2048.0, 1.0)") (runOutput result)

testFloat16ArithmeticOverflowRuntimeError :: IO ()
testFloat16ArithmeticOverflowRuntimeError = do
  result <- runSource defaultWarningSettings "left = toFloat16 65504.\nright = toFloat16 65504.\nleft + right."
  assertEqual "compile errors" [] (runCompileErrors result)
  assertSingleDiagnosticContains
    "Float16 arithmetic overflow runtime code"
    "E3025"
    (runRuntimeErrors result)
  assertSingleDiagnosticContains
    "Float16 arithmetic overflow runtime text"
    "finite Float16"
    (runRuntimeErrors result)
  assertEqual "runtime output is suppressed on runtime failure" Nothing (runOutput result)

testFloat64ArithmeticOverflowRuntimeError :: IO ()
testFloat64ArithmeticOverflowRuntimeError = do
  let hugeInteger = "1" <> replicate 200 '0'
      source =
        Text.pack
          ( "left = toFloat64 "
              <> hugeInteger
              <> ".\nright = toFloat64 "
              <> hugeInteger
              <> ".\nleft * right."
          )
  result <- runSource defaultWarningSettings source
  assertEqual "compile errors" [] (runCompileErrors result)
  assertSingleDiagnosticContains
    "Float64 arithmetic overflow runtime code"
    "E3025"
    (runRuntimeErrors result)
  assertSingleDiagnosticContains
    "Float64 arithmetic overflow runtime text"
    "non-finite Float result"
    (runRuntimeErrors result)
  assertEqual "runtime output is suppressed on runtime failure" Nothing (runOutput result)

testFloat64ComparisonEqualityRuntimeSuccess :: IO ()
testFloat64ComparisonEqualityRuntimeSuccess = do
  result <- runSource defaultWarningSettings "lt = 1.5 < 2.0.\nle = 2.0 <= 2.0.\ngt = 3.0 > 2.0.\nge = 3.0 >= 3.0.\neq = 2.0 == 2.0.\nne = 2.0 != 3.0.\n[lt, le, gt, ge, eq, ne]."
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "[True, True, True, True, True, True]") (runOutput result)

testFloat16Float32ComparisonEqualityRuntimeSuccess :: IO ()
testFloat16Float32ComparisonEqualityRuntimeSuccess = do
  result <- runSource defaultWarningSettings "a16 = toFloat16 1.\nb16 = toFloat16 2.\na32 = toFloat32 1.\nb32 = toFloat32 2.\nlt16 = a16 < b16.\nle16 = a16 <= a16.\ngt16 = b16 > a16.\nge16 = b16 >= b16.\neq16 = a16 == a16.\nne16 = a16 != b16.\nlt32 = a32 < b32.\nle32 = a32 <= a32.\ngt32 = b32 > a32.\nge32 = b32 >= b32.\neq32 = a32 == a32.\nne32 = a32 != b32.\n[lt16, le16, gt16, ge16, eq16, ne16, lt32, le32, gt32, ge32, eq32, ne32]."
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "[True, True, True, True, True, True, True, True, True, True, True, True]") (runOutput result)

testTargetedFloat16Float32FractionalLiteralComparisonEqualityRuntimeSuccess :: IO ()
testTargetedFloat16Float32FractionalLiteralComparisonEqualityRuntimeSuccess = do
  result <- runSource defaultWarningSettings "a16 :: Float16.\na16 = 1.5.\nb16 :: Float16.\nb16 = 2.25.\na32 :: Float32.\na32 = 1.5.\nb32 :: Float32.\nb32 = 2.25.\n[a16 < b16, a16 <= a16, b16 > a16, b16 >= b16, a16 == a16, a16 != b16, a32 < b32, a32 <= a32, b32 > a32, b32 >= b32, a32 == a32, a32 != b32]."
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "[True, True, True, True, True, True, True, True, True, True, True, True]") (runOutput result)

testStructuralListEqualityRuntimeSuccess :: IO ()
testStructuralListEqualityRuntimeSuccess = do
  result <- runSource defaultWarningSettings "same = [1, 2] == [1, 2].\ndifferent = [1, 2] != [1, 3].\nshorter = [1] == [1, 2].\nnested = [[True], [False]] == [[True], [False]].\n[same, different, shorter, nested]."
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "[True, True, False, True]") (runOutput result)

testStructuralTupleEqualityRuntimeSuccess :: IO ()
testStructuralTupleEqualityRuntimeSuccess = do
  result <- runSource defaultWarningSettings "same = (1, True) == (1, True).\ndifferent = (1, (True, 2)) != (1, (True, 3)).\n[same, different]."
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "[True, True]") (runOutput result)

testStructuralAdtEqualityRuntimeSuccess :: IO ()
testStructuralAdtEqualityRuntimeSuccess = do
  result <- runSource defaultWarningSettings "data Maybe a = Nothing | Just a.\nsame = Just 1 == Just 1.\ndifferentPayload = Just 1 != Just 2.\ndifferentCtor = Just 1 == Nothing.\nnested = Just [True] == Just [True].\n[same, differentPayload, differentCtor, nested]."
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "[True, True, False, True]") (runOutput result)

testRuntimeFallbackRejectsDirectCallableEquality :: IO ()
testRuntimeFallbackRejectsDirectCallableEquality = do
  assertCallableRuntimeEqualityRejected
    "runtime closure equality"
    (EBinary "==" closureValue closureValue)
  assertCallableRuntimeEqualityRejected
    "runtime builtin equality"
    (EBinary "==" builtinValue builtinValue)
  assertCallableRuntimeEqualityRejected
    "runtime operator equality"
    (EBinary "==" operatorValue operatorValue)
  assertCallableRuntimeEqualityRejected
    "runtime left section equality"
    (EBinary "==" leftSectionValue leftSectionValue)

testRuntimeFallbackRejectsDirectCallableInequality :: IO ()
testRuntimeFallbackRejectsDirectCallableInequality = do
  assertCallableRuntimeEqualityRejected
    "runtime closure inequality"
    (EBinary "!=" closureValue closureValue)
  assertCallableRuntimeEqualityRejected
    "runtime right section inequality"
    (EBinary "!=" rightSectionValue rightSectionValue)

testRuntimeFallbackRejectsMixedTargetedIntegerEquality :: IO ()
testRuntimeFallbackRejectsMixedTargetedIntegerEquality =
  assertRuntimeErrorContains
    "runtime fallback mixed targeted integer equality"
    "E3007"
    (evaluateRuntimeExpr (runtimeExpr (EBinary "==" (targetedInt "__kernel_toInt8") (targetedInt "__kernel_toUInt8"))))

testRuntimeFallbackRejectsMixedTargetedIntegerComparison :: IO ()
testRuntimeFallbackRejectsMixedTargetedIntegerComparison = do
  assertRuntimeErrorContains
    "runtime fallback mixed targeted integer less-than"
    "E3007"
    (evaluateRuntimeExpr (runtimeExpr (EBinary "<" (targetedInt "__kernel_toInt8") (targetedInt "__kernel_toUInt8"))))
  assertRuntimeErrorContains
    "runtime fallback targeted UInt8 less-or-equal untyped out-of-range Int"
    "E3007"
    (evaluateRuntimeExpr (runtimeExpr (EBinary "<=" (targetedInt "__kernel_toUInt8") (ELit (LInt 256)))))
  assertRuntimeErrorContains
    "runtime fallback mixed targeted integer greater-than"
    "E3007"
    (evaluateRuntimeExpr (runtimeExpr (EBinary ">" (targetedInt "__kernel_toUInt8") (targetedInt "__kernel_toInt16"))))
  assertRuntimeErrorContains
    "runtime fallback mixed targeted integer greater-or-equal"
    "E3007"
    (evaluateRuntimeExpr (runtimeExpr (EBinary ">=" (targetedInt "__kernel_toUInt16") (targetedInt "__kernel_toInt8"))))

testRuntimeFallbackRejectsFunctionStructuralEquality :: IO ()
testRuntimeFallbackRejectsFunctionStructuralEquality = do
  let identity = ELambda "x" (EVar "x")
      result = evaluateRuntimeExpr (runtimeExpr (EBinary "==" (EList [identity]) (EList [identity])))
  assertRuntimeErrorContains "runtime fallback function structural equality" "E3007" result
  assertRuntimeErrorContains
    "runtime fallback function structural equality callable text"
    "callable values are not equality-supported"
    result

testRuntimeFallbackRejectsQualifiedMethodStructuralEquality :: IO ()
testRuntimeFallbackRejectsQualifiedMethodStructuralEquality = do
  let result = evaluateRuntimeExpr qualifiedMethodStructuralEqualityExpr
  assertRuntimeErrorContains "runtime fallback qualified method structural equality" "E3007" result
  assertRuntimeErrorContains
    "runtime fallback qualified method structural equality callable text"
    "callable values are not equality-supported"
    result

testRuntimeFallbackRejectsDifferentLengthFunctionStructuralEquality :: IO ()
testRuntimeFallbackRejectsDifferentLengthFunctionStructuralEquality = do
  let identity = ELambda "x" (EVar "x")
  assertCallableRuntimeEqualityRejected
    "different-length function structural equality"
    (EBinary "==" (EList [identity]) (EList [identity, identity]))

testRuntimeFallbackRejectsDifferentSaturatedAdtConstructors :: IO ()
testRuntimeFallbackRejectsDifferentSaturatedAdtConstructors = do
  let result = evaluateRuntimeExpr differentSaturatedAdtConstructorEqualityExpr
  assertRuntimeErrorContains "different saturated ADT constructor equality code" "E3007" result
  assertRuntimeErrorContains
    "different saturated ADT constructor equality callable text"
    "callable values are not equality-supported"
    result
  where
    identity = ELambda "x" (EVar "x")

    differentSaturatedAdtConstructorEqualityExpr =
      EBlock
        [ SData
            (SourceSpan 1 1)
            "Maybe"
            []
            [ DataConstructor "Nothing" [],
              DataConstructor "Just" [DataConstructorArgumentName "value"]
            ],
          SExpr
            (SourceSpan 2 1)
            (EBinary "==" (EApply (EVar "Just") identity) (EVar "Nothing"))
        ]

qualifiedMethodStructuralEqualityExpr :: Expr
qualifiedMethodStructuralEqualityExpr =
  EBlock
    [ SClass
        (SourceSpan 1 1)
        "RuntimeEq"
        ["a"]
        [ ClassMethodSignature
            "equals"
            (SourceSpan 2 1)
            ( ConstrainedSignature
                []
                ( ConstraintTypeFunction
                    (ConstraintTypeName "a")
                    (ConstraintTypeFunction (ConstraintTypeName "a") (ConstraintTypeName "Bool"))
                )
            )
        ],
      SImpl
        (SourceSpan 3 1)
        "RuntimeEq"
        [ConstraintTypeName "Int"]
        [ ImplMethod
            "equals"
            (SourceSpan 4 1)
            (ELambda "left" (ELambda "right" (EBinary "==" (EVar "left") (EVar "right"))))
        ],
      SExpr
        (SourceSpan 5 1)
        (EBinary "==" (EList [EVar "RuntimeEq::equals"]) (EList [EVar "RuntimeEq::equals"]))
    ]

testFloat16ConversionRoundsRuntimeValue :: IO ()
testFloat16ConversionRoundsRuntimeValue = do
  result <- runSource defaultWarningSettings "toFloat16 2049."
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "2048.0") (runOutput result)

testDynamicIntegerConversionRangeRuntimeError :: IO ()
testDynamicIntegerConversionRangeRuntimeError = do
  result <- runSource defaultWarningSettings "x :: Int.\nx = 256.\ntoUInt8 x."
  assertEqual "compile errors" [] (runCompileErrors result)
  assertSingleDiagnosticContains
    "dynamic conversion range runtime code"
    "E3024"
    (runRuntimeErrors result)
  assertSingleDiagnosticContains
    "dynamic conversion range runtime text"
    "outside UInt8 range"
    (runRuntimeErrors result)
  assertEqual "runtime output is suppressed on runtime failure" Nothing (runOutput result)

testRuntimeFallbackRejectsNonNumericConversionValue :: IO ()
testRuntimeFallbackRejectsNonNumericConversionValue = do
  let result = evaluateRuntimeExpr (runtimeExpr (EApply (EVar "__kernel_toInt8") (ELit (LBool True))))
  assertRuntimeErrorContains "runtime fallback conversion non-numeric" "E3024" result

testDeclarationOnlyScopeHasNoOutput :: IO ()
testDeclarationOnlyScopeHasNoOutput = do
  result <- runSource defaultWarningSettings "x = 1."
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "declaration-only scope produces no output" Nothing (runOutput result)

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

testQualifiedMethodDispatchRecursivelyDefaultsBoundIntegerLiterals :: IO ()
testQualifiedMethodDispatchRecursivelyDefaultsBoundIntegerLiterals = do
  result <-
    runSource
      defaultWarningSettings
      ( "class RuntimeApply(a) {\napply :: (a -> Bool) -> Bool.\n}.\n"
          <> "impl RuntimeApply(Int) {\napply = \\(fn) -> True.\n}.\n"
          <> "impl RuntimeApply(UInt8) {\napply = \\(fn) -> False.\n}.\n"
          <> "eq1 = (1 ==).\n"
          <> "RuntimeApply::apply eq1."
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "True") (runOutput result)

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
                              (ConstraintTypeFunction (ConstraintTypeName "a") (ConstraintTypeName "Bool"))
                          )
                      ],
                    SImpl
                      (SourceSpan 4 1)
                      "Eq"
                      [ConstraintTypeName "Int"]
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

testQualifiedMethodDispatchRejectsMutualMethodAliasCycle :: IO ()
testQualifiedMethodDispatchRejectsMutualMethodAliasCycle = do
  maybeResult <-
    timeout
      1000000
      ( try
          ( runSource
              defaultWarningSettings
              ( "class RuntimeFlag(a) {\nenabled :: Bool.\nother :: Bool.\n}.\n"
                  <> "impl RuntimeFlag(Int) {\nenabled = RuntimeFlag::other.\nother = RuntimeFlag::enabled.\n}.\n"
                  <> "RuntimeFlag::enabled."
              )
          ) ::
          IO (Either SomeException RunResult)
      )
  case maybeResult of
    Nothing ->
      failTest "expected mutual qualified method alias cycle to terminate with a runtime diagnostic, but evaluation timed out"
    Just (Left err) ->
      failTest ("expected deterministic runtime diagnostic for mutual qualified method alias cycle, but evaluation raised " <> Text.pack (show err))
    Just (Right result) -> do
      assertEqual "compile errors" [] (runCompileErrors result)
      assertSingleDiagnosticContains
        "mutual qualified method alias runtime code"
        "E3021"
        (runRuntimeErrors result)
      assertSingleDiagnosticContains
        "mutual qualified method alias runtime text"
        "recursive qualified method alias cycle"
        (runRuntimeErrors result)
      assertEqual "runtime output is suppressed on runtime failure" Nothing (runOutput result)

testQualifiedMethodDispatchRejectsFullArityRuntimeAmbiguity :: IO ()
testQualifiedMethodDispatchRejectsFullArityRuntimeAmbiguity =
  assertRuntimeErrorContains
    "fully applied ambiguous qualified method"
    "ambiguous qualified method body 'RuntimePick::choose'"
    (evaluateRuntimeExpr ambiguousQualifiedMethodRuntimeExpr)

ambiguousQualifiedMethodRuntimeExpr :: Expr
ambiguousQualifiedMethodRuntimeExpr =
  EBlock
    [ SClass
        (SourceSpan 1 1)
        "RuntimePick"
        ["a"]
        [ ClassMethodSignature
            "choose"
            (SourceSpan 2 1)
            ( ConstrainedSignature
                []
                (ConstraintTypeFunction (ConstraintTypeName "Int") (ConstraintTypeName "Bool"))
            )
        ],
      SImpl
        (SourceSpan 3 1)
        "RuntimePick"
        [ConstraintTypeName "Int"]
        [ImplMethod "choose" (SourceSpan 4 1) (ELambda "value" (ELit (LBool True)))],
      SImpl
        (SourceSpan 5 1)
        "RuntimePick"
        [ConstraintTypeName "Bool"]
        [ImplMethod "choose" (SourceSpan 6 1) (ELambda "value" (ELit (LBool False)))],
      SExpr
        (SourceSpan 7 1)
        (EApply (EVar "RuntimePick::choose") (ELit (LInt 1)))
    ]

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

runtimeEqSource :: Text
runtimeEqSource =
  "class RuntimeEq(a) {\nequals :: a -> a -> Bool.\n}.\nimpl RuntimeEq(Int) {\nequals = \\(left) -> \\(right) -> left == right.\n}.\n"

testScopeDeclarationAfterExprClearsResult :: IO ()
testScopeDeclarationAfterExprClearsResult = do
  result <- runSource defaultWarningSettings "x = 1. x. y = 2."
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "declaration after expression clears scope result" Nothing (runOutput result)

runtimeExpr :: Expr -> Expr
runtimeExpr expr =
  EBlock
    [ SExpr
        (SourceSpan 1 1)
        expr
    ]

closureValue :: Expr
closureValue =
  ELambda "value" (EVar "value")

builtinValue :: Expr
builtinValue =
  EVar "__kernel_hd"

operatorValue :: Expr
operatorValue =
  EOperatorValue "+"

leftSectionValue :: Expr
leftSectionValue =
  ESectionLeft (ELit (LInt 1)) "+"

rightSectionValue :: Expr
rightSectionValue =
  ESectionRight "+" (ELit (LInt 1))

targetedFloat :: Identifier -> Expr
targetedFloat conversionName =
  EApply (EVar conversionName) (ELit (LInt 1))

targetedInt :: Identifier -> Expr
targetedInt conversionName =
  EApply (EVar conversionName) (ELit (LInt 1))

untypedFloatOne :: Expr
untypedFloatOne =
  ELit (LFloat 1.0 (mkFractionalLiteralSource 1 0 1))

assertCallableRuntimeEqualityRejected :: Text -> Expr -> IO ()
assertCallableRuntimeEqualityRejected label expr = do
  let result = evaluateRuntimeExpr (runtimeExpr expr)
  assertRuntimeErrorContains (label <> " code") "E3007" result
  assertRuntimeErrorContains
    (label <> " callable text")
    "callable values are not equality-supported"
    result

assertRuntimeErrorContains :: Text -> Text -> Either Diagnostic (Maybe a) -> IO ()
assertRuntimeErrorContains label expectedCode result =
  case result of
    Left runtimeError ->
      assertContains label expectedCode (renderDiagnostic runtimeError)
    Right _ ->
      failTest ("expected runtime error containing " <> expectedCode <> ", but evaluation succeeded")
