{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import JazzNext.Compiler.Driver
  ( CompileResult (..),
    compileSource
  )
import JazzNext.Compiler.Identifier
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
    assertJust,
    runTestSuite
  )

main :: IO ()
main = runTestSuite "PuritySemantics" tests

tests :: [NamedTest]
tests =
  [ ("pure binding cannot call impure builtin", testPureBindingCannotCallImpureBuiltin),
    ("pure binding cannot call impure builtin through dollar application", testPureBindingCannotCallImpureBuiltinThroughDollarApplication),
    ("impure binding can call impure builtin", testImpureBindingCanCallImpureBuiltin),
    ("pure binding cannot call impure callee", testPureBindingCannotCallImpureCallee),
    ("impure binding can call impure callee", testImpureBindingCanCallImpureCallee),
    ("pure binding can call pure callee", testPureBindingCanCallPureCallee),
    ("mkIdentifier keeps source text", testMkIdentifierKeepsSourceText),
    ("mkIdentifier marks bang-suffixed names impure", testMkIdentifierMarksBangSuffixedNamesImpure),
    ("mkIdentifier marks plain names pure", testMkIdentifierMarksPlainNamesPure),
    ("top-level expression may call impure builtin", testTopLevelExpressionCanCallImpureBuiltin),
    ("top-level expression may call impure callee", testTopLevelExpressionCanCallImpureCallee)
  ]

testPureBindingCannotCallImpureBuiltin :: IO ()
testPureBindingCannotCallImpureBuiltin = do
  result <- compileSource defaultWarningSettings "x = print! 1.\nx."
  assertSingleErrorContains
    "pure binding calling impure builtin"
    "E1010"
    (compileErrors result)

testPureBindingCannotCallImpureBuiltinThroughDollarApplication :: IO ()
testPureBindingCannotCallImpureBuiltinThroughDollarApplication = do
  result <- compileSource defaultWarningSettings "x = print! $ 1.\nx."
  assertSingleErrorContains
    "pure binding calling impure builtin through dollar application"
    "E1010"
    (compileErrors result)

testImpureBindingCanCallImpureBuiltin :: IO ()
testImpureBindingCanCallImpureBuiltin = do
  result <- compileSource defaultWarningSettings "x! = print! 1.\nx!."
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
  result <- compileSource defaultWarningSettings "inc! = (+ 1).\ninc! 1."
  assertEqual "compile errors" [] (compileErrors result)
  assertJust "generated JS is present" (generatedJs result)

testTopLevelExpressionCanCallImpureBuiltin :: IO ()
testTopLevelExpressionCanCallImpureBuiltin = do
  result <- compileSource defaultWarningSettings "print! 1."
  assertEqual "compile errors" [] (compileErrors result)
  assertJust "generated JS is present" (generatedJs result)
