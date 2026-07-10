{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Text (Text)
import JazzNext.Compiler.Diagnostics
  ( renderDiagnostic
  )
import JazzNext.Compiler.Driver
  ( CompileResult (..),
    RunResult (..),
    compileSource,
    runSource
  )
import JazzNext.Compiler.WarningConfig
  ( defaultWarningSettings
  )
import JazzNext.TestHarness
  ( NamedTest,
    assertContains,
    assertEqual,
    assertSingleDiagnosticCode,
    failTest,
    runTestSuite
  )

main :: IO ()
main = runTestSuite "LambdaSemantics" tests

tests :: [NamedTest]
tests =
  [ ("Unit lambda signature and repeated applications run", testUnitLambdaRuntime),
    ("Unit lambda rejects a non-Unit argument", testUnitLambdaTypeMismatch),
    ("Unit case pattern runs", testUnitCasePatternRuntime),
    ("single-argument identity lambda runs", testIdentityLambdaRuntime),
    ("multi-argument const lambda runs", testConstLambdaRuntime),
    ("lambda can close over outer variable", testClosureCaptureRuntime),
    ("lambda captures defining scope before later rebinding", testClosureCaptureBeforeRebindingRuntime),
    ("self-recursive lambda runs", testSelfRecursiveLambdaRuntime),
    ("wrapped self-recursive lambda runs", testWrappedSelfRecursiveLambdaRuntime),
    ("wrapped self-recursive lambda can use function-valued variable branch", testWrappedSelfRecursiveLambdaWithFunctionVariableBranchRuntime),
    ("wrapped self-recursive lambda can use section-valued alternate branch", testWrappedSelfRecursiveLambdaWithSectionBranchRuntime),
    ("pattern-case guarded self-recursive lambda body runs", testPatternCaseGuardedSelfRecursiveLambdaBodyRuntime),
    ("block-wrapped self-recursive lambda runs", testBlockWrappedSelfRecursiveLambdaRuntime),
    ("block-returned lambda alias can recurse at runtime", testBlockReturnedLambdaAliasRuntime),
    ("mutually recursive lambdas run", testMutualRecursiveLambdaRuntime),
    ("later recursive peer captures its own declaration environment", testMutualRecursiveCaptureAfterRebindingRuntime),
    ("mutual recursion through alias bridge runs", testMutualRecursiveAliasBridgeRuntime),
    ("recursive type seeding preserves earlier outer rebinding", testRecursiveTypeSeedingPreservesOuterBindingRuntime),
    ("higher-order apply lambda runs", testHigherOrderApplyRuntime),
    ("tuple-pattern lambda parameter runs", testTuplePatternLambdaParameterRuntime),
    ("cons-like list lambda parameter runs", testConsLikeListPatternLambdaParameterRuntime),
    ("constructor-pattern lambda parameter runs", testConstructorPatternLambdaParameterRuntime),
    ("or-pattern lambda parameter runs", testOrPatternLambdaParameterRuntime),
    ("wildcard lambda parameter runs", testWildcardPatternLambdaParameterRuntime),
    ("pattern lambda parameter reports no match at runtime", testPatternLambdaParameterNoMatchRuntime),
    ("or-pattern lambda parameter reports no match at runtime", testOrPatternLambdaParameterNoMatchRuntime),
    ("pattern lambda parameter keeps binder type constraints", testPatternLambdaParameterTypeMismatch),
    ("signature-checked lambda rejects mismatched application", testLambdaSignatureMismatch),
    ("recursive lambda rejects mismatched recursive application", testRecursiveLambdaTypeMismatch),
    ("recursive binding mismatch reports binding-specific diagnostic", testRecursiveBindingMismatchDiagnostic),
    ("wrapped recursive lambda rejects mismatched recursive application", testWrappedRecursiveLambdaTypeMismatch),
    ("mixed wrapped recursive lambda rejects non-function alternate branch", testMixedWrappedRecursiveLambdaTypeMismatch),
    ("block-wrapped recursive lambda rejects mismatched recursive application", testBlockWrappedRecursiveLambdaTypeMismatch),
    ("block-returned lambda alias rejects mismatched recursive application", testBlockReturnedLambdaAliasTypeMismatch),
    ("lambda equality is rejected at source type checking", testLambdaEqualityRejected),
    ("lambda inequality is rejected at source type checking", testLambdaInequalityRejected),
    ("non-callable application still reports apply type error", testRejectsNonCallableApplication)
  ]

testUnitLambdaRuntime :: IO ()
testUnitLambdaRuntime = do
  result <-
    runSource
      defaultWarningSettings
      "thunk :: () -> Int. thunk = \\() -> 42. (thunk (), thunk ())."
  assertEqual "warnings" [] (runWarnings result)
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "(42, 42)") (runOutput result)

testUnitLambdaTypeMismatch :: IO ()
testUnitLambdaTypeMismatch = do
  result <- compileSource defaultWarningSettings "thunk = \\() -> 42. thunk 1."
  assertSingleDiagnosticCode
    "Unit lambda type mismatch code"
    "E2006"
    (compileErrors result)
  case compileErrors result of
    compileError : _ ->
      assertContains
        "Unit lambda type rendering"
        "()"
        (renderDiagnostic compileError)
    [] ->
      failTest "expected Unit lambda type mismatch"

testUnitCasePatternRuntime :: IO ()
testUnitCasePatternRuntime = do
  result <- runSource defaultWarningSettings "case () { | () -> 42 }."
  assertEqual "warnings" [] (runWarnings result)
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "42") (runOutput result)

testIdentityLambdaRuntime :: IO ()
testIdentityLambdaRuntime = do
  result <- runSource defaultWarningSettings "id = \\(x) -> x. id 1."
  assertEqual "warnings" [] (runWarnings result)
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "1") (runOutput result)

testConstLambdaRuntime :: IO ()
testConstLambdaRuntime = do
  result <- runSource defaultWarningSettings "const = \\(x, y) -> x. const 1 2."
  assertEqual "warnings" [] (runWarnings result)
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "1") (runOutput result)

testClosureCaptureRuntime :: IO ()
testClosureCaptureRuntime = do
  result <- runSource defaultWarningSettings "makeAdder = \\(x) -> \\(y) -> x + y. add2 = makeAdder 2. add2 3."
  assertEqual "warnings" [] (runWarnings result)
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "5") (runOutput result)

testClosureCaptureBeforeRebindingRuntime :: IO ()
testClosureCaptureBeforeRebindingRuntime = do
  result <- runSource defaultWarningSettings "x = 1. addX = \\(y) -> x + y. x = 100. addX 2."
  assertEqual "warnings" [] (runWarnings result)
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "3") (runOutput result)

testSelfRecursiveLambdaRuntime :: IO ()
testSelfRecursiveLambdaRuntime = do
  result <- runSource defaultWarningSettings "countdown = \\(n) -> if n == 0 0 else countdown (n - 1). countdown 2."
  assertEqual "warnings" [] (runWarnings result)
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "0") (runOutput result)

testWrappedSelfRecursiveLambdaRuntime :: IO ()
testWrappedSelfRecursiveLambdaRuntime = do
  result <- runSource defaultWarningSettings "countdown = if True \\(n) -> if n == 0 0 else countdown (n - 1) else \\(n) -> n. countdown 2."
  assertEqual "warnings" [] (runWarnings result)
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "0") (runOutput result)

testWrappedSelfRecursiveLambdaWithFunctionVariableBranchRuntime :: IO ()
testWrappedSelfRecursiveLambdaWithFunctionVariableBranchRuntime = do
  result <- runSource defaultWarningSettings "g = \\(n) -> n. countdown = if True \\(n) -> if n == 0 0 else countdown (n - 1) else g. countdown 2."
  assertEqual "warnings" [] (runWarnings result)
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "0") (runOutput result)

testWrappedSelfRecursiveLambdaWithSectionBranchRuntime :: IO ()
testWrappedSelfRecursiveLambdaWithSectionBranchRuntime = do
  result <- runSource defaultWarningSettings "countdown = if True \\(n) -> if n == 0 0 else countdown (n - 1) else (1 +). countdown 2."
  assertEqual "warnings" [] (runWarnings result)
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "0") (runOutput result)

testPatternCaseGuardedSelfRecursiveLambdaBodyRuntime :: IO ()
testPatternCaseGuardedSelfRecursiveLambdaBodyRuntime = do
  result <- runSource defaultWarningSettings "countdown = case 0 { | 0 if True -> \\(n) -> if n == 0 0 else countdown (n - 1) | _ -> \\(n) -> n }. countdown 2."
  assertEqual "warnings" [] (runWarnings result)
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "0") (runOutput result)

testBlockWrappedSelfRecursiveLambdaRuntime :: IO ()
testBlockWrappedSelfRecursiveLambdaRuntime = do
  result <- runSource defaultWarningSettings "countdown = { \\(n) -> if n == 0 0 else countdown (n - 1). }. countdown 2."
  assertEqual "warnings" [] (runWarnings result)
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "0") (runOutput result)

testBlockReturnedLambdaAliasRuntime :: IO ()
testBlockReturnedLambdaAliasRuntime = do
  result <- runSource defaultWarningSettings "countdown = { go = \\(n) -> if n == 0 0 else countdown (n - 1). go. }. countdown 2."
  assertEqual "warnings" [] (runWarnings result)
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "0") (runOutput result)

testMutualRecursiveLambdaRuntime :: IO ()
testMutualRecursiveLambdaRuntime = do
  result <- runSource defaultWarningSettings "even = \\(n) -> if n == 0 True else odd (n - 1). odd = \\(n) -> if n == 0 False else even (n - 1). even 4."
  assertEqual "warnings" [] (runWarnings result)
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "True") (runOutput result)

testMutualRecursiveCaptureAfterRebindingRuntime :: IO ()
testMutualRecursiveCaptureAfterRebindingRuntime = do
  result <- runSource defaultWarningSettings "x = 1. f = \\(n) -> if n == 0 0 else g (n - 1). x = 2. g = \\(n) -> if n == 0 x else f (n - 1). f 1."
  assertEqual "warnings" [] (runWarnings result)
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "2") (runOutput result)

testMutualRecursiveAliasBridgeRuntime :: IO ()
testMutualRecursiveAliasBridgeRuntime = do
  result <- runSource defaultWarningSettings "f = \\(n) -> if n == 0 0 else h (n - 1). h = g. g = \\(n) -> if n == 0 1 else f (n - 1). f 1."
  assertEqual "warnings" [] (runWarnings result)
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "1") (runOutput result)

testRecursiveTypeSeedingPreservesOuterBindingRuntime :: IO ()
testRecursiveTypeSeedingPreservesOuterBindingRuntime = do
  result <- runSource defaultWarningSettings "x = 1. f = \\(n) -> if n == 0 x + 1 else g (n - 1). x = f. g = x. f 0."
  assertEqual "warnings" [] (runWarnings result)
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "2") (runOutput result)

testHigherOrderApplyRuntime :: IO ()
testHigherOrderApplyRuntime = do
  result <- runSource defaultWarningSettings "apply = \\(f, x) -> f x. apply (\\(n) -> n + 1) 2."
  assertEqual "warnings" [] (runWarnings result)
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "3") (runOutput result)

testTuplePatternLambdaParameterRuntime :: IO ()
testTuplePatternLambdaParameterRuntime = do
  result <- runSource defaultWarningSettings "sumPair = \\((left, right)) -> left + right. sumPair (1, 2)."
  assertEqual "warnings" [] (runWarnings result)
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "3") (runOutput result)

testConsLikeListPatternLambdaParameterRuntime :: IO ()
testConsLikeListPatternLambdaParameterRuntime = do
  result <- runSource defaultWarningSettings "sumFirstTwo = \\([head | tail]) -> head + hd tail. sumFirstTwo [1, 2]."
  assertEqual "warnings" [] (runWarnings result)
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "3") (runOutput result)

testConstructorPatternLambdaParameterRuntime :: IO ()
testConstructorPatternLambdaParameterRuntime = do
  result <- runSource defaultWarningSettings "data Maybe = Nothing | Just value. get = \\(Just item) -> item. get (Just 41)."
  assertEqual "warnings" [] (runWarnings result)
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "41") (runOutput result)

testOrPatternLambdaParameterRuntime :: IO ()
testOrPatternLambdaParameterRuntime = do
  result <- runSource defaultWarningSettings "data Maybe = Nothing | Just value | Also value. get = \\(Just item | Also item) -> item. get (Also 41)."
  assertEqual "warnings" [] (runWarnings result)
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "41") (runOutput result)

testWildcardPatternLambdaParameterRuntime :: IO ()
testWildcardPatternLambdaParameterRuntime = do
  result <- runSource defaultWarningSettings "ignore = \\(_) -> 1. ignore True."
  assertEqual "warnings" [] (runWarnings result)
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "1") (runOutput result)

testPatternLambdaParameterNoMatchRuntime :: IO ()
testPatternLambdaParameterNoMatchRuntime = do
  result <- runSource defaultWarningSettings "first = \\([head | tail]) -> head. first []."
  assertEqual "warnings" [] (runWarnings result)
  assertEqual "compile errors" [] (runCompileErrors result)
  assertSingleDiagnosticCode
    "pattern lambda no-match runtime code"
    "E3022"
    (runRuntimeErrors result)
  assertEqual "runtime output" Nothing (runOutput result)

testOrPatternLambdaParameterNoMatchRuntime :: IO ()
testOrPatternLambdaParameterNoMatchRuntime = do
  result <- runSource defaultWarningSettings "data Maybe = Nothing | Just value | Also value. get = \\(Just item | Also item) -> item. get Nothing."
  assertEqual "warnings" [] (runWarnings result)
  assertEqual "compile errors" [] (runCompileErrors result)
  assertSingleDiagnosticCode
    "or-pattern lambda no-match runtime code"
    "E3022"
    (runRuntimeErrors result)
  assertEqual "runtime output" Nothing (runOutput result)

testPatternLambdaParameterTypeMismatch :: IO ()
testPatternLambdaParameterTypeMismatch = do
  result <- compileSource defaultWarningSettings "sumPair = \\((left, right)) -> left + right. sumPair (True, 1)."
  assertSingleDiagnosticCode
    "pattern lambda type mismatch code"
    "E2006"
    (compileErrors result)

testLambdaSignatureMismatch :: IO ()
testLambdaSignatureMismatch = do
  result <- compileSource defaultWarningSettings "id :: Int -> Int. id = \\(x) -> x. id True."
  assertSingleDiagnosticCode
    "signature mismatch code"
    "E2006"
    (compileErrors result)

testRecursiveLambdaTypeMismatch :: IO ()
testRecursiveLambdaTypeMismatch = do
  result <- compileSource defaultWarningSettings "f = \\(x) -> f True. f 1."
  assertSingleDiagnosticCode
    "recursive lambda type mismatch code"
    "E2006"
    (compileErrors result)

testRecursiveBindingMismatchDiagnostic :: IO ()
testRecursiveBindingMismatchDiagnostic = do
  result <- compileSource defaultWarningSettings "f = \\(x) -> f + 1. f 1."
  case compileErrors result of
    compileError : _ -> do
      assertContains
        "recursive binding mismatch code"
        "E2006"
        (renderDiagnostic compileError)
      assertContains
        "recursive binding mismatch text"
        "used recursively as type"
        (renderDiagnostic compileError)
    [] ->
      failTest "expected recursive binding mismatch diagnostic"

testWrappedRecursiveLambdaTypeMismatch :: IO ()
testWrappedRecursiveLambdaTypeMismatch = do
  result <- compileSource defaultWarningSettings "f = if True \\(x) -> f True else \\(x) -> x. f 1."
  assertSingleDiagnosticCode
    "wrapped recursive lambda type mismatch code"
    "E2006"
    (compileErrors result)

testMixedWrappedRecursiveLambdaTypeMismatch :: IO ()
testMixedWrappedRecursiveLambdaTypeMismatch = do
  result <- compileSource defaultWarningSettings "f = if True \\(x) -> f x else 0. f 1."
  case compileErrors result of
    [] ->
      failTest "expected mixed wrapped recursive lambda to fail compilation"
    compileError : _ ->
      assertContains
        "mixed wrapped recursive lambda type mismatch code"
        "E2002"
        (renderDiagnostic compileError)

testBlockWrappedRecursiveLambdaTypeMismatch :: IO ()
testBlockWrappedRecursiveLambdaTypeMismatch = do
  result <- compileSource defaultWarningSettings "f = { \\(x) -> f True. }. f 1."
  assertSingleDiagnosticCode
    "block-wrapped recursive lambda type mismatch code"
    "E2006"
    (compileErrors result)

testBlockReturnedLambdaAliasTypeMismatch :: IO ()
testBlockReturnedLambdaAliasTypeMismatch = do
  result <- compileSource defaultWarningSettings "f = { g = \\(x) -> f True. g. }. f 1."
  assertSingleDiagnosticCode
    "block-returned lambda alias type mismatch code"
    "E2006"
    (compileErrors result)

testLambdaEqualityRejected :: IO ()
testLambdaEqualityRejected = do
  result <- compileSource defaultWarningSettings "f = \\(x) -> x.\ng = \\(x) -> x.\nsame = f == g."
  assertCallableEqualityDiagnostic "lambda equality" result

testLambdaInequalityRejected :: IO ()
testLambdaInequalityRejected = do
  result <- compileSource defaultWarningSettings "f = \\(x) -> x.\ng = \\(x) -> x.\ndifferent = f != g."
  assertCallableEqualityDiagnostic "lambda inequality" result

testRejectsNonCallableApplication :: IO ()
testRejectsNonCallableApplication = do
  result <- compileSource defaultWarningSettings "x = 1 2."
  case compileErrors result of
    [] ->
      failTest "expected non-callable application to fail compilation"
    compileError : _ -> do
      assertContains
        "apply error code"
        "E2006"
        (renderDiagnostic compileError)
      assertContains
        "apply error text"
        "cannot apply function"
        (renderDiagnostic compileError)

assertCallableEqualityDiagnostic :: Text -> CompileResult -> IO ()
assertCallableEqualityDiagnostic label result =
  case compileErrors result of
    compileError : _ -> do
      assertContains
        (label <> " code")
        "E2004"
        (renderDiagnostic compileError)
      assertContains
        (label <> " callable text")
        "callable values are not equality-supported"
        (renderDiagnostic compileError)
    [] ->
      failTest ("expected " <> label <> " to fail compilation")
