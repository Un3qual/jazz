{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Text (Text)
import JazzNext.Compiler.Driver
  ( CompileResult (..),
    compileSource,
    compileSourceWithPrelude
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
  [ ("pure binding cannot call impure builtin", testPureBindingCannotCallImpureBuiltin),
    ("impure binding can call impure builtin", testImpureBindingCanCallImpureBuiltin),
    ("pure binding cannot call impure callee", testPureBindingCannotCallImpureCallee),
    ("impure binding can call impure callee", testImpureBindingCanCallImpureCallee),
    ("pure binding can call pure callee", testPureBindingCanCallPureCallee),
    ("top-level expression may call impure builtin", testTopLevelExpressionCanCallImpureBuiltin),
    ("top-level expression may call impure callee", testTopLevelExpressionCanCallImpureCallee)
  ]

testPureBindingCannotCallImpureBuiltin :: IO ()
testPureBindingCannotCallImpureBuiltin = do
  result <- compileWithBundledPrelude "x = print! 1.\nx."
  assertSingleErrorContains
    "pure binding calling impure builtin"
    "E1010"
    (compileErrors result)

testImpureBindingCanCallImpureBuiltin :: IO ()
testImpureBindingCanCallImpureBuiltin = do
  result <- compileWithBundledPrelude "x! = print! 1.\nx!."
  assertEqual "compile errors" [] (compileErrors result)
  assertJust "generated JS is present" (generatedJs result)

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

testTopLevelExpressionCanCallImpureBuiltin :: IO ()
testTopLevelExpressionCanCallImpureBuiltin = do
  result <- compileWithBundledPrelude "print! 1."
  assertEqual "compile errors" [] (compileErrors result)
  assertJust "generated JS is present" (generatedJs result)

compileWithBundledPrelude :: Text -> IO CompileResult
compileWithBundledPrelude =
  compileSourceWithPrelude defaultWarningSettings (Just bundledPreludeSource)

bundledPreludeSource :: Text
bundledPreludeSource =
  "map = __kernel_map.\n\
  \filter = __kernel_filter.\n\
  \hd = __kernel_hd.\n\
  \tl = __kernel_tl.\n\
  \print! = __kernel_print!."
