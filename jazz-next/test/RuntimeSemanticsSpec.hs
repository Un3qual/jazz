{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import JazzNext.Compiler.Driver
  ( RunResult (..),
    runSource
  )
import JazzNext.Compiler.WarningConfig
  ( defaultWarningSettings
  )
import JazzNext.TestHarness
  ( NamedTest,
    assertContains,
    assertEqual,
    assertSingleErrorContains,
    runTestSuite
  )

main :: IO ()
main = runTestSuite "RuntimeSemantics" tests

tests :: [NamedTest]
tests =
  [ ("if with False condition skips then branch runtime failure", testIfFalseSkipsThenRuntimeFailure),
    ("if with True condition skips else branch runtime failure", testIfTrueSkipsElseRuntimeFailure),
    ("division by zero produces fatal runtime diagnostic", testDivisionByZeroRuntimeError),
    ("scope with only declarations has no runtime output", testDeclarationOnlyScopeHasNoOutput),
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
  assertSingleErrorContains
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
        runtimeError
  assertEqual "runtime output is suppressed on runtime failure" Nothing (runOutput result)

testDeclarationOnlyScopeHasNoOutput :: IO ()
testDeclarationOnlyScopeHasNoOutput = do
  result <- runSource defaultWarningSettings "x = 1."
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "declaration-only scope produces no output" Nothing (runOutput result)

testScopeDeclarationAfterExprClearsResult :: IO ()
testScopeDeclarationAfterExprClearsResult = do
  result <- runSource defaultWarningSettings "x = 1. x. y = 2."
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "declaration after expression clears scope result" Nothing (runOutput result)
