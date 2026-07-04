{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Text
  ( Text
  )
import JazzNext.Compiler.Driver
  ( RunResult (..),
    runSource
  )
import JazzNext.Compiler.WarningConfig
  ( defaultWarningSettings
  )
import JazzNext.TestHarness
  ( NamedTest,
    assertEqual,
    assertSingleDiagnosticCode,
    assertSingleDiagnosticContains,
    runTestSuite
  )

main :: IO ()
main = runTestSuite "AdtPatternRuntime" tests

tests :: [NamedTest]
tests =
  [ ("runtime selects the first matching literal arm", testRuntimeSelectsLiteralArm),
    ("runtime binds variable patterns in the selected arm", testRuntimeBindsVariablePattern),
    ("runtime uses wildcard fallback when literals do not match", testRuntimeUsesWildcardFallback),
    ("runtime binds nullary data constructor values", testRuntimeBindsNullaryDataConstructor),
    ("runtime applies data constructors", testRuntimeAppliesDataConstructor),
    ("runtime matches constructor patterns", testRuntimeMatchesConstructorPatterns),
    ("runtime matches nullary constructor patterns", testRuntimeMatchesNullaryConstructorPatterns),
    ("runtime matches list patterns", testRuntimeMatchesListPatterns),
    ("runtime requires exact list pattern length", testRuntimeRequiresExactListPatternLength),
    ("runtime matches cons-like list patterns", testRuntimeMatchesConsLikeListPatterns),
    ("runtime falls back when cons-like list pattern tail does not match", testRuntimeFallsBackWhenConsLikeTailDoesNotMatch),
    ("runtime falls back when cons-like list pattern sees an empty list", testRuntimeFallsBackWhenConsLikeListSeesEmptyList),
    ("runtime matches tuple patterns", testRuntimeMatchesTuplePatterns),
    ("runtime falls back when tuple element patterns do not match", testRuntimeFallsBackWhenTupleElementPatternsDoNotMatch),
    ("runtime binds as-patterns after inner matches", testRuntimeBindsAsPatternAfterInnerMatch),
    ("runtime falls back when as-pattern inner pattern does not match", testRuntimeFallsBackWhenAsPatternInnerDoesNotMatch),
    ("runtime supports as-pattern lambda parameters", testRuntimeSupportsAsPatternLambdaParameters),
    ("runtime compares constructor values inside pattern arms", testRuntimeComparesConstructorValuesInsidePatternArms),
    ("runtime selects the first matching or-pattern alternative", testRuntimeSelectsFirstMatchingOrPatternAlternative),
    ("runtime uses bindings from the first matching or-pattern alternative", testRuntimeUsesFirstMatchingOrPatternAlternativeBindings),
    ("runtime uses bindings from the first matching lambda or-pattern alternative", testRuntimeUsesFirstMatchingLambdaOrPatternAlternativeBindings),
    ("runtime falls back when all or-pattern alternatives fail", testRuntimeFallsBackWhenAllOrPatternAlternativesFail),
    ("runtime selects mixed literal-led later or-pattern arm after guarded fallback", testRuntimeSelectsMixedLiteralLedLaterOrPatternArmAfterGuardedFallback),
    ("runtime selects variable-led mixed later or-pattern arm after prior body", testRuntimeSelectsVariableLedMixedLaterOrPatternArmAfterPriorBody),
    ("runtime falls through when or-pattern guard is False", testRuntimeFallsThroughWhenOrPatternGuardIsFalse),
    ("runtime reports no match when no or-pattern alternative matches", testRuntimeReportsNoMatchWhenNoOrPatternAlternativeMatches),
    ("runtime falls through when pattern guard is False", testRuntimeFallsThroughWhenPatternGuardIsFalse),
    ("runtime skips pattern guard when pattern fails", testRuntimeSkipsPatternGuardWhenPatternFails),
    ("runtime reports no match when matching pattern guard is False", testRuntimeReportsNoMatchWhenPatternGuardIsFalse),
    ("runtime reports a deterministic error when no case arm matches", testRuntimeReportsNoMatchingArm)
  ]

testRuntimeSelectsLiteralArm :: IO ()
testRuntimeSelectsLiteralArm = do
  result <- runSource defaultWarningSettings "case 1 { | 0 -> 10 | 1 -> 20 | _ -> 30 }."
  assertSuccessfulRuntime "literal arm" (Just "20") result

testRuntimeBindsVariablePattern :: IO ()
testRuntimeBindsVariablePattern = do
  result <- runSource defaultWarningSettings "case 2 { | item -> item + 1 }."
  assertSuccessfulRuntime "variable binder" (Just "3") result

testRuntimeUsesWildcardFallback :: IO ()
testRuntimeUsesWildcardFallback = do
  result <- runSource defaultWarningSettings "case 2 { | 1 -> 10 | _ -> 20 }."
  assertSuccessfulRuntime "wildcard fallback" (Just "20") result

testRuntimeBindsNullaryDataConstructor :: IO ()
testRuntimeBindsNullaryDataConstructor = do
  result <- runSource defaultWarningSettings "data Maybe = Nothing. x = Nothing. x."
  assertSuccessfulRuntime "nullary constructor binding" (Just "Nothing") result

testRuntimeAppliesDataConstructor :: IO ()
testRuntimeAppliesDataConstructor = do
  result <- runSource defaultWarningSettings "data Maybe = Just value. x = Just 1. x."
  assertSuccessfulRuntime "constructor application" (Just "Just(1)") result

testRuntimeMatchesConstructorPatterns :: IO ()
testRuntimeMatchesConstructorPatterns = do
  result <- runSource defaultWarningSettings "data Maybe = Nothing | Just value. value = Just 41. case value { | Just item -> item + 1 | Nothing -> 0 }."
  assertSuccessfulRuntime "constructor pattern match" (Just "42") result

testRuntimeMatchesNullaryConstructorPatterns :: IO ()
testRuntimeMatchesNullaryConstructorPatterns = do
  result <- runSource defaultWarningSettings "data Maybe = Nothing | Just value. value = Nothing. case value { | Just item -> item | Nothing -> 7 }."
  assertSuccessfulRuntime "nullary constructor pattern match" (Just "7") result

testRuntimeMatchesListPatterns :: IO ()
testRuntimeMatchesListPatterns = do
  result <- runSource defaultWarningSettings "values = [1]. case values { | [head] -> head + 1 | [] -> 0 }."
  assertSuccessfulRuntime "list pattern match" (Just "2") result

testRuntimeRequiresExactListPatternLength :: IO ()
testRuntimeRequiresExactListPatternLength = do
  result <- runSource defaultWarningSettings "values = [1, 2]. case values { | [head] -> head | _ -> 9 }."
  assertSuccessfulRuntime "list pattern length mismatch" (Just "9") result

testRuntimeMatchesConsLikeListPatterns :: IO ()
testRuntimeMatchesConsLikeListPatterns = do
  result <- runSource defaultWarningSettings "values = [1, 2, 3]. case values { | [head | tail] -> head + hd tail | [] -> 0 }."
  assertSuccessfulRuntime "cons-like list pattern match" (Just "3") result

testRuntimeFallsBackWhenConsLikeTailDoesNotMatch :: IO ()
testRuntimeFallsBackWhenConsLikeTailDoesNotMatch = do
  result <- runSource defaultWarningSettings "values = [1, 2, 3]. case values { | [head | [second]] -> head + second | _ -> 9 }."
  assertSuccessfulRuntime "cons-like tail pattern mismatch" (Just "9") result

testRuntimeFallsBackWhenConsLikeListSeesEmptyList :: IO ()
testRuntimeFallsBackWhenConsLikeListSeesEmptyList = do
  result <- runSource defaultWarningSettings "values = []. case values { | [head | tail] -> head | [] -> 9 }."
  assertSuccessfulRuntime "cons-like empty-list mismatch" (Just "9") result

testRuntimeMatchesTuplePatterns :: IO ()
testRuntimeMatchesTuplePatterns = do
  result <- runSource defaultWarningSettings "pair = (41, True). case pair { | (item, True) -> item + 1 | _ -> 0 }."
  assertSuccessfulRuntime "tuple pattern match" (Just "42") result

testRuntimeFallsBackWhenTupleElementPatternsDoNotMatch :: IO ()
testRuntimeFallsBackWhenTupleElementPatternsDoNotMatch = do
  result <- runSource defaultWarningSettings "pair = (1, 2). case pair { | (1, 3) -> 10 | _ -> 9 }."
  assertSuccessfulRuntime "tuple pattern element mismatch" (Just "9") result

testRuntimeBindsAsPatternAfterInnerMatch :: IO ()
testRuntimeBindsAsPatternAfterInnerMatch = do
  result <- runSource defaultWarningSettings "data Maybe = Nothing | Just value. value = Just 41. case value { | whole @ Just item -> case whole { | Just nested -> item + nested | Nothing -> 0 } | Nothing -> 0 }."
  assertSuccessfulRuntime "as-pattern whole binding" (Just "82") result

testRuntimeFallsBackWhenAsPatternInnerDoesNotMatch :: IO ()
testRuntimeFallsBackWhenAsPatternInnerDoesNotMatch = do
  result <- runSource defaultWarningSettings "values = [1, 2]. case values { | whole @ [only] -> hd whole | _ -> 9 }."
  assertSuccessfulRuntime "as-pattern inner mismatch fallback" (Just "9") result

testRuntimeSupportsAsPatternLambdaParameters :: IO ()
testRuntimeSupportsAsPatternLambdaParameters = do
  result <- runSource defaultWarningSettings "f = \\(whole @ [head | tail]) -> head + hd tail. f [1, 2]."
  assertSuccessfulRuntime "as-pattern lambda parameter" (Just "3") result

testRuntimeComparesConstructorValuesInsidePatternArms :: IO ()
testRuntimeComparesConstructorValuesInsidePatternArms = do
  result <- runSource defaultWarningSettings "data Maybe a = Nothing | Just a. value = Just 41. case value { | whole @ Just item -> whole == Just item | Nothing -> False }."
  assertSuccessfulRuntime "constructor equality in pattern arm" (Just "True") result

testRuntimeSelectsFirstMatchingOrPatternAlternative :: IO ()
testRuntimeSelectsFirstMatchingOrPatternAlternative = do
  result <- runSource defaultWarningSettings "data Maybe = Nothing | Just value | Also value. value = Also 41. case value { | Just item | Also item -> item + 1 | Nothing -> 0 }."
  assertSuccessfulRuntime "or-pattern alternative match" (Just "42") result

testRuntimeUsesFirstMatchingOrPatternAlternativeBindings :: IO ()
testRuntimeUsesFirstMatchingOrPatternAlternativeBindings = do
  result <- runSource defaultWarningSettings "pair = (1, 2). case pair { | (item, _) | (_, item) -> item | _ -> 0 }."
  assertSuccessfulRuntime "or-pattern alternative binding order" (Just "1") result

testRuntimeUsesFirstMatchingLambdaOrPatternAlternativeBindings :: IO ()
testRuntimeUsesFirstMatchingLambdaOrPatternAlternativeBindings = do
  result <- runSource defaultWarningSettings "pick = \\((item, _) | (_, item)) -> item. pick (1, 2)."
  assertSuccessfulRuntime "lambda or-pattern alternative binding order" (Just "1") result

testRuntimeFallsBackWhenAllOrPatternAlternativesFail :: IO ()
testRuntimeFallsBackWhenAllOrPatternAlternativesFail = do
  result <- runSource defaultWarningSettings "case 3 { | 1 | 2 -> 10 | _ -> 20 }."
  assertSuccessfulRuntime "or-pattern fallback" (Just "20") result

testRuntimeSelectsMixedLiteralLedLaterOrPatternArmAfterGuardedFallback :: IO ()
testRuntimeSelectsMixedLiteralLedLaterOrPatternArmAfterGuardedFallback = do
  result <- runSource defaultWarningSettings "case 2 { | _ if False -> 0 | 2 | _ -> 10 }."
  assertSuccessfulRuntime "mixed literal-led later or-pattern guarded fallback" (Just "10") result

testRuntimeSelectsVariableLedMixedLaterOrPatternArmAfterPriorBody :: IO ()
testRuntimeSelectsVariableLedMixedLaterOrPatternArmAfterPriorBody = do
  result <- runSource defaultWarningSettings "case 1 { | 0 -> 0 | item | item @ _ -> item }."
  assertSuccessfulRuntime "variable-led mixed later or-pattern" (Just "1") result

testRuntimeFallsThroughWhenOrPatternGuardIsFalse :: IO ()
testRuntimeFallsThroughWhenOrPatternGuardIsFalse = do
  result <- runSource defaultWarningSettings "data Maybe = Nothing | Just value | Also value. value = Also 2. case value { | Just item | Also item if item > 3 -> 1 | _ -> 0 }."
  assertSuccessfulRuntime "or-pattern false guard fallback" (Just "0") result

testRuntimeReportsNoMatchWhenNoOrPatternAlternativeMatches :: IO ()
testRuntimeReportsNoMatchWhenNoOrPatternAlternativeMatches = do
  result <- runSource defaultWarningSettings "case 3 { | 1 | 2 -> 10 }."
  assertEqual "or-pattern no-match compile errors" [] (runCompileErrors result)
  assertSingleDiagnosticCode
    "or-pattern no-match runtime code"
    "E3022"
    (runRuntimeErrors result)
  assertSingleDiagnosticContains
    "or-pattern no-match runtime text"
    "matched no arms"
    (runRuntimeErrors result)
  assertEqual "or-pattern no-match runtime output" Nothing (runOutput result)

testRuntimeFallsThroughWhenPatternGuardIsFalse :: IO ()
testRuntimeFallsThroughWhenPatternGuardIsFalse = do
  result <- runSource defaultWarningSettings "data Maybe = Nothing | Just value. value = Just 1. case value { | Just item if item > 1 -> item | Just item -> item + 1 | Nothing -> 0 }."
  assertSuccessfulRuntime "pattern guard false fallback" (Just "2") result

testRuntimeSkipsPatternGuardWhenPatternFails :: IO ()
testRuntimeSkipsPatternGuardWhenPatternFails = do
  result <- runSource defaultWarningSettings "values = [1, 2]. case values { | [only] if only == hd [] -> only | [head | tail] -> head }."
  assertSuccessfulRuntime "pattern guard skipped after pattern failure" (Just "1") result

testRuntimeReportsNoMatchWhenPatternGuardIsFalse :: IO ()
testRuntimeReportsNoMatchWhenPatternGuardIsFalse = do
  result <- runSource defaultWarningSettings "case 1 { | item if item > 1 -> item }."
  assertEqual "false-guard no-match compile errors" [] (runCompileErrors result)
  assertSingleDiagnosticCode
    "false-guard no-match runtime code"
    "E3022"
    (runRuntimeErrors result)
  assertSingleDiagnosticContains
    "false-guard no-match runtime text"
    "matched no arms"
    (runRuntimeErrors result)
  assertEqual "false-guard no-match runtime output" Nothing (runOutput result)

testRuntimeReportsNoMatchingArm :: IO ()
testRuntimeReportsNoMatchingArm = do
  result <- runSource defaultWarningSettings "case 2 { | 1 -> 10 }."
  assertEqual "no-match compile errors" [] (runCompileErrors result)
  assertSingleDiagnosticCode
    "no-match runtime code"
    "E3022"
    (runRuntimeErrors result)
  assertSingleDiagnosticContains
    "no-match runtime text"
    "matched no arms"
    (runRuntimeErrors result)
  assertEqual "no-match runtime output" Nothing (runOutput result)

assertSuccessfulRuntime :: Text -> Maybe Text -> RunResult -> IO ()
assertSuccessfulRuntime label expectedOutput result = do
  assertEqual (label <> " compile errors") [] (runCompileErrors result)
  assertEqual (label <> " runtime errors") [] (runRuntimeErrors result)
  assertEqual (label <> " runtime output") expectedOutput (runOutput result)
