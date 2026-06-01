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
    DataConstructorArgument (..),
    DataConstructor (..),
    Expr (..),
    Literal (..),
    Pattern (..),
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
import JazzNext.Compiler.Runtime
  ( evaluateRuntimeExpr
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
    ("target-named float conversion evaluates at runtime", testFloatConversionRuntimeSuccess),
    ("dynamic integer-to-Float64 overflow checks source magnitude", testDynamicIntegerToFloat64OverflowRuntimeError),
    ("fractional literal evaluates and renders at runtime", testFractionalLiteralRuntimeSuccess),
    ("Float64 arithmetic evaluates at runtime", testFloat64ArithmeticRuntimeSuccess),
    ("Float64 arithmetic overflow produces runtime diagnostic", testFloat64ArithmeticOverflowRuntimeError),
    ("Float16 conversion rounds to target precision", testFloat16ConversionRoundsRuntimeValue),
    ("dynamic integer conversion range failure reports deterministic diagnostic", testDynamicIntegerConversionRangeRuntimeError),
    ("runtime fallback rejects non-numeric conversion values", testRuntimeFallbackRejectsNonNumericConversionValue),
    ("scope with only declarations has no runtime output", testDeclarationOnlyScopeHasNoOutput),
    ("scope with only capability declarations has no runtime output", testCapabilityDeclarationOnlyScopeHasNoOutput),
    ("capability declarations are inert at runtime", testCapabilityDeclarationsRuntimeInert),
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
  result <- runSource defaultWarningSettings "class RuntimeOnly { }.\nimpl RuntimeOnly(Int) { }."
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "declaration-only capability scope produces no output" Nothing (runOutput result)

testCapabilityDeclarationsRuntimeInert :: IO ()
testCapabilityDeclarationsRuntimeInert = do
  result <- runSource defaultWarningSettings "class RuntimeOnly { }.\nimpl RuntimeOnly(Int) { }.\nx = 1.\nx."
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "capability declarations do not affect runtime output" (Just "1") (runOutput result)

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

assertRuntimeErrorContains :: Text -> Text -> Either Diagnostic (Maybe a) -> IO ()
assertRuntimeErrorContains label expectedCode result =
  case result of
    Left runtimeError ->
      assertContains label expectedCode (renderDiagnostic runtimeError)
    Right _ ->
      failTest ("expected runtime error containing " <> expectedCode <> ", but evaluation succeeded")
