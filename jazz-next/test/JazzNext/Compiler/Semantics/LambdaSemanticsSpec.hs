{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Text (Text)
import JazzNext.Compiler.Diagnostics.Render
  ( renderDiagnostic,
  )
import JazzNext.Compiler.Driver
  ( CompileResult,
    RunResult (..),
    compileErrors,
    compileSource,
    runCompileErrors,
    runRuntimeErrors,
    runSource,
    runWarnings,
  )
import JazzNext.Compiler.WarningConfig
  ( defaultWarningSettings,
  )
import JazzNext.TestHarness
  ( NamedTest,
    assertContains,
    assertEqual,
    assertSingleDiagnosticCode,
    failTest,
    runTestSuite,
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
    ("explicit nested lambdas capture between curry boundaries", testExplicitNestedLambdaClosureCaptureRuntime),
    ("lambda captures defining scope before later rebinding", testClosureCaptureBeforeRebindingRuntime),
    ("lambda captures an outer value used by a local rebinding", testClosureCaptureForLocalRebindingRuntime),
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
    ("multiple pattern-shaped lambda parameters run", testMultiplePatternLambdaParametersRuntime),
    ("or-pattern lambda parameter runs", testOrPatternLambdaParameterRuntime),
    ("or-pattern lambda head followed by another parameter runs", testMultiParameterOrPatternLambdaRuntime),
    ("ordered pattern-lambda clauses run", testPatternLambdaClauseOrderRuntime),
    ("pattern-lambda clauses preserve partial application", testPatternLambdaClausePartialApplicationRuntime),
    ("recursive pattern-lambda clauses run", testRecursivePatternLambdaClausesRuntime),
    ("pattern-lambda clauses report no match at runtime", testPatternLambdaClausesNoMatchRuntime),
    ("explicit case dispatch inside an ordinary function runs", testExplicitCaseDispatchRuntime),
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

testExplicitNestedLambdaClosureCaptureRuntime :: IO ()
testExplicitNestedLambdaClosureCaptureRuntime = do
  -- The nested surface is intentional: this test covers closure capture at
  -- each explicit unary lambda boundary rather than compact syntax.
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

testClosureCaptureForLocalRebindingRuntime :: IO ()
testClosureCaptureForLocalRebindingRuntime = do
  result <-
    runSource
      defaultWarningSettings
      """
      x = 41.
      increment = \\() -> {
        x = x + 1.
        x.
      }.
      increment ().
      """
  assertEqual "warnings" [] (runWarnings result)
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "42") (runOutput result)

testSelfRecursiveLambdaRuntime :: IO ()
testSelfRecursiveLambdaRuntime = do
  result <- runSource defaultWarningSettings "countdown = \\(n) -> if n == 0 then 0 else countdown (n - 1). countdown 2."
  assertEqual "warnings" [] (runWarnings result)
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "0") (runOutput result)

testWrappedSelfRecursiveLambdaRuntime :: IO ()
testWrappedSelfRecursiveLambdaRuntime = do
  result <- runSource defaultWarningSettings "countdown = if True then \\(n) -> if n == 0 then 0 else countdown (n - 1) else \\(n) -> n. countdown 2."
  assertEqual "warnings" [] (runWarnings result)
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "0") (runOutput result)

testWrappedSelfRecursiveLambdaWithFunctionVariableBranchRuntime :: IO ()
testWrappedSelfRecursiveLambdaWithFunctionVariableBranchRuntime = do
  result <- runSource defaultWarningSettings "g = \\(n) -> n. countdown = if True then \\(n) -> if n == 0 then 0 else countdown (n - 1) else g. countdown 2."
  assertEqual "warnings" [] (runWarnings result)
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "0") (runOutput result)

testWrappedSelfRecursiveLambdaWithSectionBranchRuntime :: IO ()
testWrappedSelfRecursiveLambdaWithSectionBranchRuntime = do
  result <- runSource defaultWarningSettings "countdown = if True then \\(n) -> if n == 0 then 0 else countdown (n - 1) else (1 +). countdown 2."
  assertEqual "warnings" [] (runWarnings result)
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "0") (runOutput result)

testPatternCaseGuardedSelfRecursiveLambdaBodyRuntime :: IO ()
testPatternCaseGuardedSelfRecursiveLambdaBodyRuntime = do
  result <- runSource defaultWarningSettings "countdown = case 0 { | 0 if True -> \\(n) -> if n == 0 then 0 else countdown (n - 1) | _ -> \\(n) -> n }. countdown 2."
  assertEqual "warnings" [] (runWarnings result)
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "0") (runOutput result)

testBlockWrappedSelfRecursiveLambdaRuntime :: IO ()
testBlockWrappedSelfRecursiveLambdaRuntime = do
  result <- runSource defaultWarningSettings "countdown = { \\(n) -> if n == 0 then 0 else countdown (n - 1). }. countdown 2."
  assertEqual "warnings" [] (runWarnings result)
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "0") (runOutput result)

testBlockReturnedLambdaAliasRuntime :: IO ()
testBlockReturnedLambdaAliasRuntime = do
  result <- runSource defaultWarningSettings "countdown = { go = \\(n) -> if n == 0 then 0 else countdown (n - 1). go. }. countdown 2."
  assertEqual "warnings" [] (runWarnings result)
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "0") (runOutput result)

testMutualRecursiveLambdaRuntime :: IO ()
testMutualRecursiveLambdaRuntime = do
  result <- runSource defaultWarningSettings "even = \\(n) -> if n == 0 then True else odd (n - 1). odd = \\(n) -> if n == 0 then False else even (n - 1). even 4."
  assertEqual "warnings" [] (runWarnings result)
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "True") (runOutput result)

testMutualRecursiveCaptureAfterRebindingRuntime :: IO ()
testMutualRecursiveCaptureAfterRebindingRuntime = do
  result <- runSource defaultWarningSettings "x = 1. f = \\(n) -> if n == 0 then 0 else g (n - 1). x = 2. g = \\(n) -> if n == 0 then x else f (n - 1). f 1."
  assertEqual "warnings" [] (runWarnings result)
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "2") (runOutput result)

testMutualRecursiveAliasBridgeRuntime :: IO ()
testMutualRecursiveAliasBridgeRuntime = do
  result <- runSource defaultWarningSettings "f = \\(n) -> if n == 0 then 0 else h (n - 1). h = g. g = \\(n) -> if n == 0 then 1 else f (n - 1). f 1."
  assertEqual "warnings" [] (runWarnings result)
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "1") (runOutput result)

testRecursiveTypeSeedingPreservesOuterBindingRuntime :: IO ()
testRecursiveTypeSeedingPreservesOuterBindingRuntime = do
  result <- runSource defaultWarningSettings "x = 1. f = \\(n) -> if n == 0 then x + 1 else g (n - 1). x = f. g = x. f 0."
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
  result <- runSource defaultWarningSettings "data Maybe = Nothing | Just Int. get = \\(Just item) -> item. get (Just 41)."
  assertEqual "warnings" [] (runWarnings result)
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "41") (runOutput result)

testMultiplePatternLambdaParametersRuntime :: IO ()
testMultiplePatternLambdaParametersRuntime = do
  result <-
    runSource
      defaultWarningSettings
      "data Maybe = Nothing | Just Int. add = \\([head | _], Just item) -> head + item. add [1, 2] (Just 41)."
  assertEqual "warnings" [] (runWarnings result)
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "42") (runOutput result)

testOrPatternLambdaParameterRuntime :: IO ()
testOrPatternLambdaParameterRuntime = do
  result <- runSource defaultWarningSettings "data Maybe = Nothing | Just Int | Also Int. get = \\(Just item | Also item) -> item. get (Also 41)."
  assertEqual "warnings" [] (runWarnings result)
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "41") (runOutput result)

testMultiParameterOrPatternLambdaRuntime :: IO ()
testMultiParameterOrPatternLambdaRuntime = do
  result <-
    runSource
      defaultWarningSettings
      "data Maybe = Nothing | Just Int | Also Int. add = \\(Just item | Also item, extra) -> item + extra. add (Also 40) 2."
  assertEqual "warnings" [] (runWarnings result)
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "42") (runOutput result)

testPatternLambdaClauseOrderRuntime :: IO ()
testPatternLambdaClauseOrderRuntime = do
  result <-
    runSource
      defaultWarningSettings
      "pick = \\|(0) -> 10 |(_) -> 20. (pick 0, pick 1)."
  assertSuccessfulPatternLambdaRuntime "ordered pattern lambda clauses" (Just "(10, 20)") result

testPatternLambdaClausePartialApplicationRuntime :: IO ()
testPatternLambdaClausePartialApplicationRuntime = do
  result <-
    runSource
      defaultWarningSettings
      "data Maybe = Nothing | Just Int. choose = \\|(Nothing, fallback) -> fallback |(Just item, _) -> item. keep = choose (Just 42). keep 0."
  assertSuccessfulPatternLambdaRuntime "partial pattern lambda application" (Just "42") result

testRecursivePatternLambdaClausesRuntime :: IO ()
testRecursivePatternLambdaClausesRuntime = do
  result <-
    runSource
      defaultWarningSettings
      "length = \\|([]) -> 0 |([_ | rest]) -> 1 + length rest. length [1, 2, 3, 4]."
  assertSuccessfulPatternLambdaRuntime "recursive pattern lambda clauses" (Just "4") result

testPatternLambdaClausesNoMatchRuntime :: IO ()
testPatternLambdaClausesNoMatchRuntime = do
  result <- runSource defaultWarningSettings "onlyZero = \\|(0) -> 1. onlyZero 1."
  assertEqual "non-exhaustive compile errors" [] (runCompileErrors result)
  assertSingleDiagnosticCode
    "pattern lambda clause no-match code"
    "E3022"
    (runRuntimeErrors result)
  assertEqual "non-exhaustive runtime output" Nothing (runOutput result)

testExplicitCaseDispatchRuntime :: IO ()
testExplicitCaseDispatchRuntime = do
  result <-
    runSource
      defaultWarningSettings
      """
      length =
        \\(items) ->
          case items {
            | [] -> 0
            | [_ | rest] -> 1 + length rest
          }.
      length [1, 2, 3].
      """
  assertEqual "warnings" [] (runWarnings result)
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "3") (runOutput result)

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
  result <- runSource defaultWarningSettings "data Maybe = Nothing | Just Int | Also Int. get = \\(Just item | Also item) -> item. get Nothing."
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
  result <- compileSource defaultWarningSettings "f = if True then \\(x) -> f True else \\(x) -> x. f 1."
  assertSingleDiagnosticCode
    "wrapped recursive lambda type mismatch code"
    "E2006"
    (compileErrors result)

testMixedWrappedRecursiveLambdaTypeMismatch :: IO ()
testMixedWrappedRecursiveLambdaTypeMismatch = do
  result <- compileSource defaultWarningSettings "f = if True then \\(x) -> f x else 0. f 1."
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
  result <-
    compileSource
      defaultWarningSettings
      """
      f = \\(x) -> x.
      g = \\(x) -> x.
      same = f == g.
      """
  assertCallableEqualityDiagnostic "lambda equality" result

testLambdaInequalityRejected :: IO ()
testLambdaInequalityRejected = do
  result <-
    compileSource
      defaultWarningSettings
      """
      f = \\(x) -> x.
      g = \\(x) -> x.
      different = f != g.
      """
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

assertSuccessfulPatternLambdaRuntime :: Text -> Maybe Text -> RunResult -> IO ()
assertSuccessfulPatternLambdaRuntime label expectedOutput result = do
  assertEqual (label <> " warnings") [] (runWarnings result)
  assertEqual (label <> " compile errors") [] (runCompileErrors result)
  assertEqual (label <> " runtime errors") [] (runRuntimeErrors result)
  assertEqual (label <> " output") expectedOutput (runOutput result)

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
