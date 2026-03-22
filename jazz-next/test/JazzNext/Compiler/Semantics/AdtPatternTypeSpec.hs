{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Text
  ( Text
  )
import JazzNext.Compiler.Driver
  ( CompileResult (..),
    compileSource,
    compileErrors
  )
import JazzNext.Compiler.WarningConfig
  ( defaultWarningSettings
  )
import JazzNext.TestHarness
  ( NamedTest,
    assertEqual,
    assertJust,
    assertSingleDiagnosticCode,
    assertSingleDiagnosticContains,
    runTestSuite
  )

main :: IO ()
main = runTestSuite "AdtPatternType" tests

tests :: [NamedTest]
tests =
  [ ( "source pipeline accepts variable pattern binders with the scrutinee type",
      testSourcePipelineAcceptsVariableBinder
    ),
    ( "source pipeline accepts literal and wildcard case patterns",
      testSourcePipelineAcceptsLiteralAndWildcardPatterns
    ),
    ( "source pipeline reports only deferred diagnostics for constructor patterns",
      testSourcePipelineDefersConstructorPatternBodies
    ),
    ( "source pipeline skips branch mismatch diagnostics for deferred constructor patterns",
      testSourcePipelineSkipsConstructorBranchMismatch
    ),
    ( "source pipeline reports only deferred diagnostics for list patterns",
      testSourcePipelineDefersListPatternBodies
    ),
    ( "source pipeline skips branch mismatch diagnostics for deferred list patterns",
      testSourcePipelineSkipsListBranchMismatch
    ),
    ( "source pipeline rejects incompatible literal pattern types",
      testSourcePipelineRejectsIncompatibleLiteralPattern
    ),
    ( "source pipeline rejects mismatched case arm result types",
      testSourcePipelineRejectsMismatchedArmResultTypes
    )
  ]

testSourcePipelineAcceptsVariableBinder :: IO ()
testSourcePipelineAcceptsVariableBinder = do
  result <- compileSource defaultWarningSettings "x = case 1 { | item -> item + 1 }."
  assertCompiles "variable binder result" result

testSourcePipelineAcceptsLiteralAndWildcardPatterns :: IO ()
testSourcePipelineAcceptsLiteralAndWildcardPatterns = do
  result <- compileSource defaultWarningSettings "x = case 1 { | 0 -> False | _ -> True }."
  assertCompiles "literal wildcard result" result

testSourcePipelineDefersConstructorPatternBodies :: IO ()
testSourcePipelineDefersConstructorPatternBodies = do
  result <- compileSource defaultWarningSettings "value = [1]. x = case value { | Just item -> item + 1 | _ -> 0 }."
  assertSingleDiagnosticCode
    "constructor deferred error code"
    "E2011"
    (compileErrors result)
  assertSingleDiagnosticContains
    "constructor deferred error text"
    "constructor case patterns remain deferred"
    (compileErrors result)

testSourcePipelineSkipsConstructorBranchMismatch :: IO ()
testSourcePipelineSkipsConstructorBranchMismatch = do
  result <- compileSource defaultWarningSettings "value = [1]. x = case value { | Just item -> 1 | _ -> False }."
  assertSingleDiagnosticCode
    "constructor deferred branch mismatch code"
    "E2011"
    (compileErrors result)
  assertSingleDiagnosticContains
    "constructor deferred branch mismatch text"
    "constructor case patterns remain deferred"
    (compileErrors result)

testSourcePipelineDefersListPatternBodies :: IO ()
testSourcePipelineDefersListPatternBodies = do
  result <- compileSource defaultWarningSettings "values = [True]. x = case values { | [head] -> head + 1 | _ -> 0 }."
  assertSingleDiagnosticCode
    "list deferred error code"
    "E2011"
    (compileErrors result)
  assertSingleDiagnosticContains
    "list deferred error text"
    "list case patterns remain deferred"
    (compileErrors result)

testSourcePipelineSkipsListBranchMismatch :: IO ()
testSourcePipelineSkipsListBranchMismatch = do
  result <- compileSource defaultWarningSettings "values = [[1]]. x = case values { | [head] -> 1 | _ -> False }."
  assertSingleDiagnosticCode
    "list deferred branch mismatch code"
    "E2011"
    (compileErrors result)
  assertSingleDiagnosticContains
    "list deferred branch mismatch text"
    "list case patterns remain deferred"
    (compileErrors result)

testSourcePipelineRejectsIncompatibleLiteralPattern :: IO ()
testSourcePipelineRejectsIncompatibleLiteralPattern = do
  result <- compileSource defaultWarningSettings "x = case True { | 0 -> 1 | _ -> 2 }."
  assertSingleDiagnosticCode
    "pattern type error code"
    "E2011"
    (compileErrors result)
  assertSingleDiagnosticContains
    "pattern type error text"
    "does not match scrutinee type"
    (compileErrors result)

testSourcePipelineRejectsMismatchedArmResultTypes :: IO ()
testSourcePipelineRejectsMismatchedArmResultTypes = do
  result <- compileSource defaultWarningSettings "x = case 1 { | 0 -> True | _ -> 2 }."
  assertSingleDiagnosticCode
    "pattern branch mismatch code"
    "E2012"
    (compileErrors result)
  assertSingleDiagnosticContains
    "pattern branch mismatch text"
    "case arms must have matching types"
    (compileErrors result)

assertCompiles :: Text -> CompileResult -> IO ()
assertCompiles label result = do
  assertEqual (label <> " compile errors") [] (compileErrors result)
  assertJust (label <> " generated JS") (generatedJs result)
