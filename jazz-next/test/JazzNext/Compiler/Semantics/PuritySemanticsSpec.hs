{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Text (Text)
import JazzNext.Compiler.BundledPrelude
  ( bundledPreludeSource
  )
import JazzNext.Compiler.Diagnostics
  ( SourceSpan (..)
  )
import JazzNext.Compiler.Driver
  ( CompileResult (..),
    compileSource,
    compileSourceWithPrelude
  )
import JazzNext.Compiler.Name
  ( identifierPurity,
    identifierText,
    mkIdentifier
  )
import JazzNext.Compiler.Purity
  ( Purity (..)
  )
import JazzNext.Compiler.WarningConfig
  ( defaultWarningSettings
  )
import JazzNext.TestHarness
  ( NamedTest,
    assertEqual,
    assertSingleErrorContains,
    assertSingleDiagnosticPrimarySpan,
    assertSingleDiagnosticRelatedSpan,
    assertSingleDiagnosticSubject,
    runTestSuite
  )

main :: IO ()
main = runTestSuite "PuritySemantics" tests

tests :: [NamedTest]
tests =
  [ ("pure binding cannot call impure builtin", testPureBindingCannotCallImpureBuiltin),
    ("pure binding cannot call impure builtin through dollar application", testPureBindingCannotCallImpureBuiltinThroughDollarApplication),
    ("pure binding cannot call impure qualified method", testPureBindingCannotCallImpureQualifiedMethod),
    ("pure binding cannot call impure callee through explicit type application", testPureBindingCannotCallImpureCalleeThroughExplicitTypeApplication),
    ("pure impl method cannot call impure callee", testPureImplMethodCannotCallImpureCallee),
    ("impure impl method can call impure callee", testImpureImplMethodCanCallImpureCallee),
    ("impure binding can call impure builtin", testImpureBindingCanCallImpureBuiltin),
    ("pure binding cannot call impure callee", testPureBindingCannotCallImpureCallee),
    ("impure binding can call impure callee", testImpureBindingCanCallImpureCallee),
    ("pure binding can call pure callee", testPureBindingCanCallPureCallee),
    ("mkIdentifier keeps source text", testMkIdentifierKeepsSourceText),
    ("mkIdentifier marks bang-suffixed names impure", testMkIdentifierMarksBangSuffixedNamesImpure),
    ("mkIdentifier marks plain names pure", testMkIdentifierMarksPlainNamesPure),
    ("top-level expression may call impure builtin", testTopLevelExpressionCanCallImpureBuiltin),
    ("pure bindings reject private host IO primitives", testPureBindingsRejectPrivateHostIOPrimitives),
    ("impure bindings accept private host IO primitives", testImpureBindingsAcceptPrivateHostIOPrimitives),
    ("top-level expressions accept private host IO primitives", testTopLevelExpressionsAcceptPrivateHostIOPrimitives),
    ("top-level expression may call impure callee", testTopLevelExpressionCanCallImpureCallee)
  ]

testPureBindingCannotCallImpureBuiltin :: IO ()
testPureBindingCannotCallImpureBuiltin = do
  result <- compileWithBundledPrelude """
  x = print! 1.
  x.
  """
  assertSingleErrorContains
    "pure binding calling impure builtin"
    "E1010"
    (compileErrors result)

testPureBindingCannotCallImpureBuiltinThroughDollarApplication :: IO ()
testPureBindingCannotCallImpureBuiltinThroughDollarApplication = do
  result <- compileSource defaultWarningSettings """
  x = print! $ 1.
  x.
  """
  assertSingleErrorContains
    "pure binding calling impure builtin through dollar application"
    "E1010"
    (compileErrors result)

testPureBindingCannotCallImpureQualifiedMethod :: IO ()
testPureBindingCannotCallImpureQualifiedMethod = do
  result <-
    compileSource
      defaultWarningSettings
      """
      class Effect(a) {
      run! :: a -> a.
      }.
      impl Effect(Int) {
      run! = \\(value) -> value.
      }.
      x = Effect::run! 1.
      x.
      """
  assertSingleErrorContains
    "pure binding calling impure qualified method"
    "E1010"
    (compileErrors result)

testPureBindingCannotCallImpureCalleeThroughExplicitTypeApplication :: IO ()
testPureBindingCannotCallImpureCalleeThroughExplicitTypeApplication = do
  result <-
    compileSource
      defaultWarningSettings
      """
      class Need(a) { }.
      impl Need(Int) { }.
      f! :: @{Need(a)}: a -> a.
      f! = \\(value) -> value.
      x = f! @Int 1.
      x.
      """
  assertSingleErrorContains
    "pure binding calling impure callee through explicit type application"
    "E1010"
    (compileErrors result)

testPureImplMethodCannotCallImpureCallee :: IO ()
testPureImplMethodCannotCallImpureCallee = do
  result <-
    compileSource
      defaultWarningSettings
      """
      class Runner(a) {
      run :: a -> a.
      }.
      inc! = (+ 1).
      impl Runner(Int) {
      run = \\(value) -> inc! value.
      }.
      Runner::run 1.
      """
  assertSingleErrorContains
    "pure impl method calling impure callee code"
    "E1010"
    (compileErrors result)
  assertSingleErrorContains
    "pure impl method calling impure callee"
    "impl method 'run' cannot call impure callee 'inc!'"
    (compileErrors result)
  assertSingleDiagnosticSubject
    "pure impl method diagnostic subject"
    "run"
    (compileErrors result)

testImpureImplMethodCanCallImpureCallee :: IO ()
testImpureImplMethodCanCallImpureCallee = do
  result <-
    compileSource
      defaultWarningSettings
      """
      class Runner(a) {
      run! :: a -> a.
      }.
      inc! = (+ 1).
      impl Runner(Int) {
      run! = \\(value) -> inc! value.
      }.
      Runner::run! 1.
      """
  assertEqual "compile errors" [] (compileErrors result)

testImpureBindingCanCallImpureBuiltin :: IO ()
testImpureBindingCanCallImpureBuiltin = do
  result <- compileWithBundledPrelude """
  x! = print! 1.
  x!.
  """
  assertEqual "compile errors" [] (compileErrors result)

testPureBindingCannotCallImpureCallee :: IO ()
testPureBindingCannotCallImpureCallee = do
  result <- compileSource defaultWarningSettings """
  inc! = (+ 1).
  x = inc! 1.
  x.
  """
  assertSingleErrorContains
    "pure binding calling impure callee"
    "E1010"
    (compileErrors result)
  assertSingleDiagnosticPrimarySpan
    "pure binding diagnostic primary span"
    (SourceSpan 2 1)
    (compileErrors result)
  assertSingleDiagnosticRelatedSpan
    "pure binding diagnostic related span"
    (SourceSpan 1 1)
    (compileErrors result)
  assertSingleDiagnosticSubject
    "pure binding diagnostic subject"
    "x"
    (compileErrors result)

testImpureBindingCanCallImpureCallee :: IO ()
testImpureBindingCanCallImpureCallee = do
  result <- compileSource defaultWarningSettings """
  inc! = (+ 1).
  x! = inc! 1.
  x!.
  """
  assertEqual "compile errors" [] (compileErrors result)

testPureBindingCanCallPureCallee :: IO ()
testPureBindingCanCallPureCallee = do
  result <- compileSource defaultWarningSettings """
  inc = (+ 1).
  x = inc 1.
  x.
  """
  assertEqual "compile errors" [] (compileErrors result)

testMkIdentifierKeepsSourceText :: IO ()
testMkIdentifierKeepsSourceText = do
  let identifier = mkIdentifier "inc!"
  assertEqual "identifier text" "inc!" (identifierText identifier)

testMkIdentifierMarksBangSuffixedNamesImpure :: IO ()
testMkIdentifierMarksBangSuffixedNamesImpure = do
  let identifier = mkIdentifier "inc!"
  assertEqual "identifier purity" Impure (identifierPurity identifier)

testMkIdentifierMarksPlainNamesPure :: IO ()
testMkIdentifierMarksPlainNamesPure = do
  let identifier = mkIdentifier "inc"
  assertEqual "identifier purity" Pure (identifierPurity identifier)

testTopLevelExpressionCanCallImpureCallee :: IO ()
testTopLevelExpressionCanCallImpureCallee = do
  result <- compileSource defaultWarningSettings """
  inc! = (+ 1).
  inc! 1.
  """
  assertEqual "compile errors" [] (compileErrors result)

testTopLevelExpressionCanCallImpureBuiltin :: IO ()
testTopLevelExpressionCanCallImpureBuiltin = do
  result <- compileWithBundledPrelude "print! 1."
  assertEqual "compile errors" [] (compileErrors result)

testPureBindingsRejectPrivateHostIOPrimitives :: IO ()
testPureBindingsRejectPrivateHostIOPrimitives =
  mapM_ assertRejected privateHostIOCalls
  where
    assertRejected call = do
      result <- compileSource defaultWarningSettings ("result = " <> call <> ".")
      assertSingleErrorContains
        ("pure binding rejects " <> call)
        "E1010"
        (compileErrors result)

testImpureBindingsAcceptPrivateHostIOPrimitives :: IO ()
testImpureBindingsAcceptPrivateHostIOPrimitives =
  mapM_ assertAccepted privateHostIOCalls
  where
    assertAccepted call = do
      result <- compileSource defaultWarningSettings ("result! = " <> call <> ".")
      assertEqual ("impure binding accepts " <> call) [] (compileErrors result)

testTopLevelExpressionsAcceptPrivateHostIOPrimitives :: IO ()
testTopLevelExpressionsAcceptPrivateHostIOPrimitives =
  mapM_ assertAccepted privateHostIOCalls
  where
    assertAccepted call = do
      result <- compileSource defaultWarningSettings (call <> ".")
      assertEqual ("top-level expression accepts " <> call) [] (compileErrors result)

privateHostIOCalls :: [Text]
privateHostIOCalls =
  [ "__kernel_readTextRaw! \"source.jz\"",
    "__kernel_writeTextRaw! \"output.txt\" \"Jazz\"",
    "__kernel_readStdinRaw! ()",
    "__kernel_writeStdoutRaw! \"out\"",
    "__kernel_writeStderrRaw! \"err\"",
    "__kernel_arguments! ()",
    "__kernel_exit! 0"
  ]

compileWithBundledPrelude :: Text -> IO CompileResult
compileWithBundledPrelude =
  compileSourceWithPrelude defaultWarningSettings (Just bundledPreludeSource)
