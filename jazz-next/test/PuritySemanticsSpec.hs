{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import JazzNext.Compiler.Driver
  ( CompileResult (..),
    compileSource
  )
import JazzNext.Compiler.WarningConfig
  ( defaultWarningSettings
  )
import JazzNext.TestHarness
  ( NamedTest,
    assertEqual,
    assertSingleErrorContains,
    assertJust,
    runTestSuite
  )

main :: IO ()
main = runTestSuite "PuritySemantics" tests

tests :: [NamedTest]
tests =
  [ ("pure binding cannot call impure callee", testPureBindingCannotCallImpureCallee),
    ("impure binding can call impure callee", testImpureBindingCanCallImpureCallee),
    ("pure binding can call pure callee", testPureBindingCanCallPureCallee),
    ("top-level expression may call impure callee", testTopLevelExpressionCanCallImpureCallee)
  ]

testPureBindingCannotCallImpureCallee :: IO ()
testPureBindingCannotCallImpureCallee = do
  result <- compileSource defaultWarningSettings "inc! = (+ 1).\nx = inc! 1.\nx."
  assertSingleErrorContains
    "pure binding calling impure callee"
    "E1010"
    (compileErrors result)

testImpureBindingCanCallImpureCallee :: IO ()
testImpureBindingCanCallImpureCallee = do
  result <- compileSource defaultWarningSettings "inc! = (+ 1).\nx! = inc! 1.\nx!."
  assertEqual "compile errors" [] (compileErrors result)
  assertJust "generated JS is present" (generatedJs result)

testPureBindingCanCallPureCallee :: IO ()
testPureBindingCanCallPureCallee = do
  result <- compileSource defaultWarningSettings "inc = (+ 1).\nx = inc 1.\nx."
  assertEqual "compile errors" [] (compileErrors result)
  assertJust "generated JS is present" (generatedJs result)

testTopLevelExpressionCanCallImpureCallee :: IO ()
testTopLevelExpressionCanCallImpureCallee = do
  result <- compileSource defaultWarningSettings "inc! = (+ 1).\ninc! 1."
  assertEqual "compile errors" [] (compileErrors result)
  assertJust "generated JS is present" (generatedJs result)
