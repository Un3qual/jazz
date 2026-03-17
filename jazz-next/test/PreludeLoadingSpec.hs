{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Text (Text)
import JazzNext.Compiler.Driver
  ( CompileResult (..),
    RunResult (..),
    compileSourceWithPrelude,
    runSourceWithPrelude
  )
import JazzNext.Compiler.WarningConfig
  ( defaultWarningSettings
  )
import JazzNext.TestHarness
  ( NamedTest,
    assertEqual,
    assertSingleErrorContains,
    runTestSuite
  )

main :: IO ()
main = runTestSuite "PreludeLoading" tests

tests :: [NamedTest]
tests =
  [ ("compile source can reference prelude-defined bindings", testCompileWithPreludeBindingVisibility),
    ("run source can apply prelude-defined section functions", testRunWithPreludeSectionFunction),
    ("run source can use bundled builtin aliases from prelude", testRunWithPreludeBuiltinAliases),
    ("invalid prelude source produces prelude parse diagnostic", testPreludeParseDiagnostic),
    ("prelude alias with unknown kernel symbol fails conformance checks", testPreludeUnknownBridgeSymbolDiagnostic),
    ("prelude alias with missing kernel suffix fails conformance checks", testPreludeBridgeMissingSuffixDiagnostic),
    ("prelude builtin alias must be a direct kernel reference", testPreludeMalformedBridgeDiagnostic),
    ("prelude cannot redefine kernel bridge names directly", testPreludeRejectsKernelBridgeRebinding),
    ("prelude builtin alias must target its matching kernel symbol", testPreludeRejectsMismatchedKernelAlias),
    ("compile without prelude keeps missing binding behavior unchanged", testCompileWithoutPreludeStillFailsMissingBinding)
  ]

testCompileWithPreludeBindingVisibility :: IO ()
testCompileWithPreludeBindingVisibility = do
  result <- compileSourceWithPrelude defaultWarningSettings (Just "seed = 41.") "seed + 1."
  assertEqual "compile errors" [] (compileErrors result)

testRunWithPreludeSectionFunction :: IO ()
testRunWithPreludeSectionFunction = do
  result <- runSourceWithPrelude defaultWarningSettings (Just "inc = (+ 1).") "inc 2."
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "3") (runOutput result)

testRunWithPreludeBuiltinAliases :: IO ()
testRunWithPreludeBuiltinAliases = do
  result <- runSourceWithPrelude defaultWarningSettings (Just bundledPreludeSource) "map hd [[1, 2], [3]]."
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "[1, 3]") (runOutput result)

testPreludeParseDiagnostic :: IO ()
testPreludeParseDiagnostic = do
  result <- compileSourceWithPrelude defaultWarningSettings (Just "broken = .") "1."
  assertSingleErrorContains
    "prelude parse error code"
    "E0002"
    (compileErrors result)

testPreludeUnknownBridgeSymbolDiagnostic :: IO ()
testPreludeUnknownBridgeSymbolDiagnostic = do
  result <- compileSourceWithPrelude defaultWarningSettings (Just "map = __kernel_unknown.") "1."
  assertSingleErrorContains
    "unknown kernel bridge symbol code"
    "E0004"
    (compileErrors result)

testPreludeBridgeMissingSuffixDiagnostic :: IO ()
testPreludeBridgeMissingSuffixDiagnostic = do
  result <- compileSourceWithPrelude defaultWarningSettings (Just "map = __kernel_.") "1."
  assertSingleErrorContains
    "missing kernel bridge suffix code"
    "E0005"
    (compileErrors result)

testPreludeMalformedBridgeDiagnostic :: IO ()
testPreludeMalformedBridgeDiagnostic = do
  result <- compileSourceWithPrelude defaultWarningSettings (Just "inc = __kernel_map. map = inc.") "1."
  assertSingleErrorContains
    "malformed kernel bridge code"
    "E0005"
    (compileErrors result)

testPreludeRejectsKernelBridgeRebinding :: IO ()
testPreludeRejectsKernelBridgeRebinding = do
  result <- compileSourceWithPrelude defaultWarningSettings (Just "__kernel_map = map.") "1."
  assertSingleErrorContains
    "prelude cannot redefine kernel bridge names"
    "E0005"
    (compileErrors result)

testPreludeRejectsMismatchedKernelAlias :: IO ()
testPreludeRejectsMismatchedKernelAlias = do
  result <- compileSourceWithPrelude defaultWarningSettings (Just "map = __kernel_hd.") "1."
  assertSingleErrorContains
    "prelude builtin alias must target matching kernel symbol"
    "E0005"
    (compileErrors result)

testCompileWithoutPreludeStillFailsMissingBinding :: IO ()
testCompileWithoutPreludeStillFailsMissingBinding = do
  result <- compileSourceWithPrelude defaultWarningSettings Nothing "seed + 1."
  assertSingleErrorContains
    "missing prelude binding still reports unbound variable"
    "E1001"
    (compileErrors result)

bundledPreludeSource :: Text
bundledPreludeSource =
  "map = __kernel_map.\n\
  \filter = __kernel_filter.\n\
  \hd = __kernel_hd.\n\
  \tl = __kernel_tl.\n\
  \print! = __kernel_print!."
