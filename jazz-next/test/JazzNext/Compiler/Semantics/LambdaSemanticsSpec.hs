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
    assertJust,
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
    ("higher-order apply lambda runs", testHigherOrderApplyRuntime),
    ("signature-checked lambda rejects mismatched application", testLambdaSignatureMismatch),
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
  assertJust "compile errors present" (case compileErrors result of [] -> Nothing; errs -> Just errs)
