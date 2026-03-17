{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import JazzNext.Compiler.Driver
  ( CompileResult (..),
    compileSource,
    RunResult (..),
    compileSourceWithPrelude,
    runSourceWithPrelude
  )
import JazzNext.Compiler.Diagnostics
  ( SourceSpan (..),
    renderDiagnostic
  )
import JazzNext.Compiler.WarningConfig
  ( defaultWarningSettings
  )
import JazzNext.TestHarness
  ( NamedTest,
    assertContains,
    assertEqual,
    assertSingleErrorContains,
    assertSingleDiagnosticCode,
    assertSingleDiagnosticPrimarySpan,
    assertSingleDiagnosticRelatedSpan,
    assertSingleDiagnosticSubject,
    runTestSuite
  )

main :: IO ()
main = runTestSuite "PreludeLoading" tests

tests :: [NamedTest]
tests =
  [ ("compile source can reference prelude-defined bindings", testCompileWithPreludeBindingVisibility),
    ("run source can apply prelude-defined section functions", testRunWithPreludeSectionFunction),
    ("bundled default prelude preserves user diagnostic spans", testBundledPreludePreservesUserDiagnosticSpans),
    ("invalid prelude source produces prelude parse diagnostic", testPreludeParseDiagnostic),
    ("prelude bridge with unknown kernel symbol fails conformance checks", testPreludeUnknownBridgeSymbolDiagnostic),
    ("prelude bridge with missing kernel suffix fails conformance checks", testPreludeBridgeMissingSuffixDiagnostic),
    ("prelude bridge must be direct symbol reference", testPreludeMalformedBridgeDiagnostic),
    ("prelude bridge rejects canonical alias in bridge declaration", testPreludeBridgeRejectsCanonicalAlias),
    ("prelude bridge rebinding reports current and previous bridge spans", testPreludeBridgeRebindingDiagnostic),
    ("prelude bridge allows canonical alias after kernel self-bridge", testPreludeBridgeAllowsCanonicalAliasAfterBridge),
    ("compile without prelude keeps compatibility aliases available", testCompileWithoutPreludeKeepsCompatibilityAliases),
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

testBundledPreludePreservesUserDiagnosticSpans :: IO ()
testBundledPreludePreservesUserDiagnosticSpans = do
  result <- compileSource defaultWarningSettings "x :: Int. y = 1."
  case map renderDiagnostic (compileErrors result) of
    [rendered] -> do
      assertContains "bundled default prelude keeps signature code" "E1003" rendered
      assertContains "bundled default prelude keeps user spans anchored to user source" "1:1:" rendered
    renderedErrors ->
      assertEqual "single rendered diagnostic" 1 (length renderedErrors)

testPreludeParseDiagnostic :: IO ()
testPreludeParseDiagnostic = do
  result <- compileSourceWithPrelude defaultWarningSettings (Just "broken = .") "1."
  assertSingleErrorContains
    "prelude parse error code"
    "E0002"
    (compileErrors result)

testPreludeUnknownBridgeSymbolDiagnostic :: IO ()
testPreludeUnknownBridgeSymbolDiagnostic = do
  result <- compileSourceWithPrelude defaultWarningSettings (Just "__kernel_unknown = unknown.") "1."
  let diagnostics = compileErrors result
  assertSingleDiagnosticCode
    "unknown kernel bridge symbol code"
    "E0004"
    diagnostics
  assertSingleDiagnosticPrimarySpan
    "unknown kernel bridge primary span"
    (SourceSpan 1 1)
    diagnostics
  assertSingleDiagnosticSubject
    "unknown kernel bridge subject"
    "__kernel_unknown"
    diagnostics

testPreludeBridgeMissingSuffixDiagnostic :: IO ()
testPreludeBridgeMissingSuffixDiagnostic = do
  result <- compileSourceWithPrelude defaultWarningSettings (Just "__kernel_ = __kernel_map.") "1."
  assertSingleErrorContains
    "missing kernel bridge suffix code"
    "E0005"
    (compileErrors result)

testPreludeMalformedBridgeDiagnostic :: IO ()
testPreludeMalformedBridgeDiagnostic = do
  result <- compileSourceWithPrelude defaultWarningSettings (Just "__kernel_map = inc. inc = (+ 1).") "1."
  assertSingleErrorContains
    "malformed kernel bridge code"
    "E0005"
    (compileErrors result)

testPreludeBridgeRejectsCanonicalAlias :: IO ()
testPreludeBridgeRejectsCanonicalAlias = do
  result <- compileSourceWithPrelude defaultWarningSettings (Just "map = (+ 1). __kernel_map = map.") "1."
  assertSingleErrorContains
    "bridge cannot reference canonical alias name"
    "E0005"
    (compileErrors result)

testPreludeBridgeRebindingDiagnostic :: IO ()
testPreludeBridgeRebindingDiagnostic = do
  result <-
    compileSourceWithPrelude
      defaultWarningSettings
      (Just "__kernel_map = __kernel_map.\n__kernel_map = __kernel_map.")
      "1."
  let diagnostics = compileErrors result
  assertSingleDiagnosticCode
    "bridge rebinding code"
    "E0005"
    diagnostics
  assertSingleDiagnosticPrimarySpan
    "bridge rebinding primary span"
    (SourceSpan 2 1)
    diagnostics
  assertSingleDiagnosticRelatedSpan
    "bridge rebinding related span"
    (SourceSpan 1 1)
    diagnostics
  assertSingleDiagnosticSubject
    "bridge rebinding subject"
    "__kernel_map"
    diagnostics

testPreludeBridgeAllowsCanonicalAliasAfterBridge :: IO ()
testPreludeBridgeAllowsCanonicalAliasAfterBridge = do
  result <- compileSourceWithPrelude defaultWarningSettings (Just "__kernel_map = __kernel_map. map = __kernel_map.") "1."
  assertEqual
    "bridge validation accepts canonical alias after kernel self-bridge"
    []
    (compileErrors result)

testCompileWithoutPreludeKeepsCompatibilityAliases :: IO ()
testCompileWithoutPreludeKeepsCompatibilityAliases = do
  result <- compileSourceWithPrelude defaultWarningSettings Nothing "x = map hd [[1], [2]]."
  assertEqual
    "compatibility aliases remain available without prelude"
    []
    (compileErrors result)

testCompileWithoutPreludeStillFailsMissingBinding :: IO ()
testCompileWithoutPreludeStillFailsMissingBinding = do
  result <- compileSourceWithPrelude defaultWarningSettings Nothing "seed + 1."
  assertSingleErrorContains
    "missing prelude binding still reports unbound variable"
    "E1001"
    (compileErrors result)
