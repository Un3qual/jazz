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
    ( "source pipeline accepts data constructor values",
      testSourcePipelineAcceptsDataConstructorValues
    ),
    ( "source pipeline accepts data constructor application",
      testSourcePipelineAcceptsDataConstructorApplication
    ),
    ( "source pipeline rejects over-applied nullary constructors",
      testSourcePipelineRejectsOverAppliedNullaryConstructor
    ),
    ( "source pipeline accepts data constructor patterns",
      testSourcePipelineAcceptsDataConstructorPatterns
    ),
    ( "source pipeline types constructor pattern binders as payload types",
      testSourcePipelineTypesConstructorPatternBinders
    ),
    ( "source pipeline rejects constructor patterns for incompatible scrutinees",
      testSourcePipelineRejectsConstructorPatternScrutineeMismatch
    ),
    ( "source pipeline rejects unknown constructor patterns",
      testSourcePipelineRejectsUnknownConstructorPatterns
    ),
    ( "source pipeline rejects constructor pattern arity mismatches",
      testSourcePipelineRejectsConstructorPatternArityMismatch
    ),
    ( "source pipeline skips constructor subpatterns after scrutinee mismatch",
      testSourcePipelineSkipsConstructorSubpatternsAfterScrutineeMismatch
    ),
    ( "source pipeline stops constructor argument checks after payload mismatch",
      testSourcePipelineStopsConstructorArgumentChecksAfterPayloadMismatch
    ),
    ( "source pipeline rejects constructor arm result mismatches",
      testSourcePipelineRejectsConstructorBranchMismatch
    ),
    ( "source pipeline treats constructor payloads as monomorphic",
      testSourcePipelineTreatsConstructorPayloadsAsMonomorphic
    ),
    ( "source pipeline accepts list patterns",
      testSourcePipelineAcceptsListPatterns
    ),
    ( "source pipeline types list pattern binders as element types",
      testSourcePipelineTypesListPatternBinders
    ),
    ( "source pipeline rejects list patterns for incompatible scrutinees",
      testSourcePipelineRejectsListPatternScrutineeMismatch
    ),
    ( "source pipeline rejects list arm result mismatches",
      testSourcePipelineRejectsListBranchMismatch
    ),
    ( "source pipeline rejects duplicate pattern binders",
      testSourcePipelineRejectsDuplicatePatternBinders
    ),
    ( "source pipeline rejects incompatible literal pattern types",
      testSourcePipelineRejectsIncompatibleLiteralPattern
    ),
    ( "source pipeline skips invalid pattern arm bodies",
      testSourcePipelineSkipsInvalidPatternArmBodies
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

testSourcePipelineAcceptsDataConstructorValues :: IO ()
testSourcePipelineAcceptsDataConstructorValues = do
  result <- compileSource defaultWarningSettings "data Maybe = Nothing | Just. x = Nothing."
  assertCompiles "data constructor value" result

testSourcePipelineAcceptsDataConstructorApplication :: IO ()
testSourcePipelineAcceptsDataConstructorApplication = do
  result <- compileSource defaultWarningSettings "data Maybe = Nothing | Just value. x = Just 1."
  assertCompiles "data constructor application" result

testSourcePipelineRejectsOverAppliedNullaryConstructor :: IO ()
testSourcePipelineRejectsOverAppliedNullaryConstructor = do
  result <- compileSource defaultWarningSettings "data Maybe = Nothing. x = Nothing 1."
  assertSingleDiagnosticCode
    "over-applied nullary constructor code"
    "E2006"
    (compileErrors result)
  assertSingleDiagnosticContains
    "over-applied nullary constructor text"
    "cannot apply function of type Maybe"
    (compileErrors result)

testSourcePipelineAcceptsDataConstructorPatterns :: IO ()
testSourcePipelineAcceptsDataConstructorPatterns = do
  result <- compileSource defaultWarningSettings "data Maybe = Nothing | Just value. value = Just 1. x = case value { | Just item -> item + 1 | Nothing -> 0 }."
  assertCompiles "data constructor pattern" result

testSourcePipelineTypesConstructorPatternBinders :: IO ()
testSourcePipelineTypesConstructorPatternBinders = do
  result <- compileSource defaultWarningSettings "data Maybe = Nothing | Just value. value = Just True. x = case value { | Just item -> item + 1 | Nothing -> 0 }."
  assertSingleDiagnosticCode
    "constructor pattern binder type error code"
    "E2003"
    (compileErrors result)
  assertSingleDiagnosticContains
    "constructor pattern binder type error text"
    "cannot apply operator '+' to operands of type Bool and Int"
    (compileErrors result)

testSourcePipelineRejectsConstructorPatternScrutineeMismatch :: IO ()
testSourcePipelineRejectsConstructorPatternScrutineeMismatch = do
  result <- compileSource defaultWarningSettings "data Maybe = Nothing | Just value. value = 1. x = case value { | Just item -> item | _ -> 0 }."
  assertSingleDiagnosticCode
    "constructor pattern scrutinee mismatch code"
    "E2011"
    (compileErrors result)
  assertSingleDiagnosticContains
    "constructor pattern scrutinee mismatch text"
    "case pattern of type Maybe does not match scrutinee type Int"
    (compileErrors result)

testSourcePipelineRejectsUnknownConstructorPatterns :: IO ()
testSourcePipelineRejectsUnknownConstructorPatterns = do
  result <- compileSource defaultWarningSettings "value = [1]. x = case value { | Just item -> item + 1 | _ -> 0 }."
  assertSingleDiagnosticCode
    "unknown constructor pattern error code"
    "E2011"
    (compileErrors result)
  assertSingleDiagnosticContains
    "unknown constructor pattern error text"
    "unknown constructor case pattern 'Just'"
    (compileErrors result)

testSourcePipelineRejectsConstructorPatternArityMismatch :: IO ()
testSourcePipelineRejectsConstructorPatternArityMismatch = do
  result <- compileSource defaultWarningSettings "data Maybe = Nothing | Just value. value = Just 1. x = case value { | Just -> 1 | Nothing -> 0 }."
  assertSingleDiagnosticCode
    "constructor pattern arity mismatch code"
    "E2011"
    (compileErrors result)
  assertSingleDiagnosticContains
    "constructor pattern arity mismatch text"
    "constructor case pattern 'Just' expects 1 argument(s), found 0"
    (compileErrors result)

testSourcePipelineSkipsConstructorSubpatternsAfterScrutineeMismatch :: IO ()
testSourcePipelineSkipsConstructorSubpatternsAfterScrutineeMismatch = do
  result <- compileSource defaultWarningSettings "data Maybe = Nothing | Just value. value = 1. x = case value { | Just True -> 0 | _ -> 0 }. y = Just 1."
  assertSingleDiagnosticCode
    "constructor subpattern skip code"
    "E2011"
    (compileErrors result)
  assertSingleDiagnosticContains
    "constructor subpattern skip text"
    "case pattern of type Maybe does not match scrutinee type Int"
    (compileErrors result)

testSourcePipelineStopsConstructorArgumentChecksAfterPayloadMismatch :: IO ()
testSourcePipelineStopsConstructorArgumentChecksAfterPayloadMismatch = do
  result <- compileSource defaultWarningSettings "data Pair = Pair left right. seed = Pair 1 []. x = case seed { | Pair True [False] -> 0 | _ -> 0 }. ok = Pair 1 [1]."
  assertSingleDiagnosticCode
    "constructor payload mismatch short-circuit code"
    "E2011"
    (compileErrors result)
  assertSingleDiagnosticContains
    "constructor payload mismatch short-circuit text"
    "case pattern of type Bool does not match scrutinee type Int"
    (compileErrors result)

testSourcePipelineRejectsConstructorBranchMismatch :: IO ()
testSourcePipelineRejectsConstructorBranchMismatch = do
  result <- compileSource defaultWarningSettings "data Maybe = Nothing | Just value. value = Just 1. x = case value { | Just item -> 1 | Nothing -> False }."
  assertSingleDiagnosticCode
    "constructor branch mismatch code"
    "E2012"
    (compileErrors result)
  assertSingleDiagnosticContains
    "constructor branch mismatch text"
    "case arms must have matching types"
    (compileErrors result)

testSourcePipelineTreatsConstructorPayloadsAsMonomorphic :: IO ()
testSourcePipelineTreatsConstructorPayloadsAsMonomorphic = do
  result <- compileSource defaultWarningSettings "data Box = Box value. first = Box 1. second = Box True."
  assertSingleDiagnosticCode
    "monomorphic constructor payload code"
    "E2006"
    (compileErrors result)
  assertSingleDiagnosticContains
    "monomorphic constructor payload text"
    "cannot apply function of type Int -> Box to argument of type Bool"
    (compileErrors result)

testSourcePipelineAcceptsListPatterns :: IO ()
testSourcePipelineAcceptsListPatterns = do
  result <- compileSource defaultWarningSettings "values = [1]. x = case values { | [head] -> head + 1 | [] -> 0 }."
  assertCompiles "list pattern" result

testSourcePipelineTypesListPatternBinders :: IO ()
testSourcePipelineTypesListPatternBinders = do
  result <- compileSource defaultWarningSettings "values = [True]. x = case values { | [head] -> head + 1 | _ -> 0 }."
  assertSingleDiagnosticCode
    "list pattern binder type error code"
    "E2003"
    (compileErrors result)
  assertSingleDiagnosticContains
    "list pattern binder type error text"
    "cannot apply operator '+' to operands of type Bool and Int"
    (compileErrors result)

testSourcePipelineRejectsListPatternScrutineeMismatch :: IO ()
testSourcePipelineRejectsListPatternScrutineeMismatch = do
  result <- compileSource defaultWarningSettings "value = 1. x = case value { | [head] -> head | _ -> 0 }."
  assertSingleDiagnosticCode
    "list pattern scrutinee mismatch code"
    "E2011"
    (compileErrors result)
  assertSingleDiagnosticContains
    "list pattern scrutinee mismatch text"
    "does not match scrutinee type Int"
    (compileErrors result)

testSourcePipelineRejectsListBranchMismatch :: IO ()
testSourcePipelineRejectsListBranchMismatch = do
  result <- compileSource defaultWarningSettings "values = [1]. x = case values { | [head] -> 1 | [] -> False }."
  assertSingleDiagnosticCode
    "list branch mismatch code"
    "E2012"
    (compileErrors result)
  assertSingleDiagnosticContains
    "list branch mismatch text"
    "case arms must have matching types"
    (compileErrors result)

testSourcePipelineRejectsDuplicatePatternBinders :: IO ()
testSourcePipelineRejectsDuplicatePatternBinders = do
  result <- compileSource defaultWarningSettings "values = [1, 2]. x = case values { | [item, item] -> item | _ -> 0 }."
  assertSingleDiagnosticCode
    "duplicate pattern binder code"
    "E2011"
    (compileErrors result)
  assertSingleDiagnosticContains
    "duplicate pattern binder text"
    "duplicate case pattern binder 'item'"
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

testSourcePipelineSkipsInvalidPatternArmBodies :: IO ()
testSourcePipelineSkipsInvalidPatternArmBodies = do
  result <- compileSource defaultWarningSettings "x = case True { | 0 -> 1 + False | _ -> 0 }."
  assertSingleDiagnosticCode
    "invalid pattern arm body is skipped code"
    "E2011"
    (compileErrors result)
  assertSingleDiagnosticContains
    "invalid pattern arm body is skipped text"
    "case pattern of type Int does not match scrutinee type Bool"
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
