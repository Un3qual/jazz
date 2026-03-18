{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

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
  [ ("single-argument identity lambda runs", testIdentityLambdaRuntime),
    ("multi-argument const lambda runs", testConstLambdaRuntime),
    ("lambda can close over outer variable", testClosureCaptureRuntime),
    ("lambda captures defining scope before later rebinding", testClosureCaptureBeforeRebindingRuntime),
    ("self-recursive lambda runs", testSelfRecursiveLambdaRuntime),
    ("wrapped self-recursive lambda runs", testWrappedSelfRecursiveLambdaRuntime),
    ("block-wrapped self-recursive lambda runs", testBlockWrappedSelfRecursiveLambdaRuntime),
    ("mutually recursive lambdas run", testMutualRecursiveLambdaRuntime),
    ("later recursive peer captures its own declaration environment", testMutualRecursiveCaptureAfterRebindingRuntime),
    ("mutual recursion through alias bridge runs", testMutualRecursiveAliasBridgeRuntime),
    ("recursive type seeding preserves earlier outer rebinding", testRecursiveTypeSeedingPreservesOuterBindingRuntime),
    ("higher-order apply lambda runs", testHigherOrderApplyRuntime),
    ("signature-checked lambda rejects mismatched application", testLambdaSignatureMismatch),
    ("recursive lambda rejects mismatched recursive application", testRecursiveLambdaTypeMismatch),
    ("wrapped recursive lambda rejects mismatched recursive application", testWrappedRecursiveLambdaTypeMismatch),
    ("block-wrapped recursive lambda rejects mismatched recursive application", testBlockWrappedRecursiveLambdaTypeMismatch),
    ("non-callable application still reports apply type error", testRejectsNonCallableApplication)
  ]

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

testBlockWrappedSelfRecursiveLambdaRuntime :: IO ()
testBlockWrappedSelfRecursiveLambdaRuntime = do
  result <- runSource defaultWarningSettings "countdown = { \\(n) -> if n == 0 0 else countdown (n - 1). }. countdown 2."
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

testLambdaSignatureMismatch :: IO ()
testLambdaSignatureMismatch = do
  result <- compileSource defaultWarningSettings "id :: Int -> Int. id = \\(x) -> x. id True."
  assertSingleDiagnosticCode
    "signature mismatch code"
    "E2006"
    (compileErrors result)
  assertEqual "generated JS suppressed on compile error" Nothing (generatedJs result)

testRecursiveLambdaTypeMismatch :: IO ()
testRecursiveLambdaTypeMismatch = do
  result <- compileSource defaultWarningSettings "f = \\(x) -> f True. f 1."
  assertSingleDiagnosticCode
    "recursive lambda type mismatch code"
    "E2006"
    (compileErrors result)
  assertEqual "generated JS suppressed on compile error" Nothing (generatedJs result)

testWrappedRecursiveLambdaTypeMismatch :: IO ()
testWrappedRecursiveLambdaTypeMismatch = do
  result <- compileSource defaultWarningSettings "f = if True \\(x) -> f True else \\(x) -> x. f 1."
  assertSingleDiagnosticCode
    "wrapped recursive lambda type mismatch code"
    "E2006"
    (compileErrors result)
  assertEqual "generated JS suppressed on compile error" Nothing (generatedJs result)

testBlockWrappedRecursiveLambdaTypeMismatch :: IO ()
testBlockWrappedRecursiveLambdaTypeMismatch = do
  result <- compileSource defaultWarningSettings "f = { \\(x) -> f True. }. f 1."
  assertSingleDiagnosticCode
    "block-wrapped recursive lambda type mismatch code"
    "E2006"
    (compileErrors result)
  assertEqual "generated JS suppressed on compile error" Nothing (generatedJs result)

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
  assertEqual "generated JS suppressed on compile error" Nothing (generatedJs result)
