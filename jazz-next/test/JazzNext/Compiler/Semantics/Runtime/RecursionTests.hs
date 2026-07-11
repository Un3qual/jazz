{-# LANGUAGE OverloadedStrings #-}

module JazzNext.Compiler.Semantics.Runtime.RecursionTests
  ( recursionTests
  ) where


import Control.Exception
  ( SomeException,
    try
  )
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import JazzNext.Compiler.AST
  ( CaseArm (..),
    ClassMethodSignature (..),
    SignatureType (..),
    DataConstructorArgument (..),
    DataConstructor (..),
    Expr (..),
    ImplMethod (..),
    Literal (..),
    Pattern (..),
    SignaturePayload (..),
    Statement (..)
  )
import JazzNext.Compiler.Diagnostics
  ( Diagnostic,
    SourceSpan (..),
    renderDiagnostic
  )
import JazzNext.Compiler.BuiltinCatalog
  ( BuiltinResolutionMode (..)
  )
import JazzNext.Compiler.Driver
  ( RunResult (..),
    runSource,
    runSourceWithPrelude
  )
import JazzNext.Compiler.FractionalLiteral
  ( mkFractionalLiteralSource
  )
import JazzNext.Compiler.Name (Name, qualifiedName)
import JazzNext.Compiler.Runtime
  ( RuntimeValue (..),
    evaluateRuntimeExpr,
    evaluateRuntimeExprWithBuiltinsAndBindingHints,
    renderRuntimeValue,
    runtimeValueExactlyMatchesConstraint
  )
import JazzNext.Compiler.RuntimeHints
  ( bindingRuntimeHintKey
  )
import JazzNext.Compiler.TypeInference
  ( InferenceResult (..),
    inferExpressionWithBuiltins
  )
import JazzNext.Compiler.WarningConfig
  ( defaultWarningSettings
  )
import JazzNext.TestHarness
  ( NamedTest,
    assertContains,
    assertLeftDiagnosticCodeAndContains,
    assertEqual,
    assertSingleDiagnosticContains,
    failTest,
    runTestSuite
  )
import System.Timeout
  ( timeout
  )
import JazzNext.Compiler.Semantics.Runtime.Shared

recursionTests :: [NamedTest]
recursionTests =
  [ ("alias-only recursive cycle produces deterministic runtime diagnostic", testAliasOnlyRecursiveCycleRuntimeError)
    , ("wrapped alias-only recursive cycle produces deterministic runtime diagnostic", testWrappedAliasOnlyRecursiveCycleRuntimeError)
    , ("mixed wrapped alias cycle still produces deterministic runtime diagnostic", testMixedWrappedAliasCycleRuntimeError)
    , ("wrapped alias cycle still evaluates wrapper condition first", testWrappedAliasCycleConditionRuntimeError)
    , ("pattern-case alias-only recursive cycle produces deterministic runtime diagnostic", testPatternCaseAliasOnlyRecursiveCycleRuntimeError)
    , ("pattern-case binder shadows recursive peer during alias resolution", testPatternCaseBinderDoesNotAliasRecursivePeer)
    , ("pattern-case guard lambda does not classify non-function recursion", testPatternCaseGuardLambdaDoesNotClassifyNonFunctionRecursion)
    , ("function-valued pattern guard self-reference produces recursion diagnostic", testFunctionPatternGuardSelfReferenceRuntimeError)
    , ("block-wrapped alias-only recursive cycle produces deterministic runtime diagnostic", testBlockWrappedAliasOnlyRecursiveCycleRuntimeError)
    , ("non-function recursive cycle produces deterministic runtime diagnostic", testNonFunctionRecursiveCycleRuntimeError)
    , ("nested block alias cycle ignores later outer peer name", testNestedBlockAliasCycleIgnoresLaterOuterPeer)
    , ("recursive declared user operator applies at runtime", testRecursiveDeclaredUserOperatorRuntimeSuccess)
    , ("recursive declared user operator value alias produces deterministic runtime diagnostic", testRecursiveDeclaredUserOperatorValueAliasRuntimeError)
    , ("indirect recursive declared user operator value alias produces deterministic runtime diagnostic", testIndirectRecursiveDeclaredUserOperatorValueAliasRuntimeError)
    , ("qualified method dispatch recursively defaults bound integer literals", testQualifiedMethodDispatchRecursivelyDefaultsBoundIntegerLiterals)
    , ("qualified method dispatch rejects mutual method alias cycle", testQualifiedMethodDispatchRejectsMutualMethodAliasCycle)
  ]

testAliasOnlyRecursiveCycleRuntimeError :: IO ()
testAliasOnlyRecursiveCycleRuntimeError = do
  maybeResult <- timeout 1000000 (try (runSource defaultWarningSettings "even = odd. odd = even. even.") :: IO (Either SomeException RunResult))
  case maybeResult of
    Nothing ->
      failTest "expected alias-only recursive cycle to terminate with a runtime diagnostic, but evaluation timed out"
    Just (Left err) ->
      failTest ("expected deterministic runtime diagnostic, but evaluation raised " <> Text.pack (show err))
    Just (Right result) -> do
      assertEqual "compile errors" [] (runCompileErrors result)
      assertSingleDiagnosticContains
        "alias-only recursive cycle runtime code"
        "E3021"
        (runRuntimeErrors result)
      assertSingleDiagnosticContains
        "alias-only recursive cycle runtime text"
        "recursive alias cycle"
        (runRuntimeErrors result)
      assertEqual "runtime output is suppressed on runtime failure" Nothing (runOutput result)

testWrappedAliasOnlyRecursiveCycleRuntimeError :: IO ()
testWrappedAliasOnlyRecursiveCycleRuntimeError = do
  maybeResult <- timeout 1000000 (try (runSource defaultWarningSettings "f = if True g else g. g = f. f.") :: IO (Either SomeException RunResult))
  case maybeResult of
    Nothing ->
      failTest "expected wrapped alias-only recursive cycle to terminate with a runtime diagnostic, but evaluation timed out"
    Just (Left err) ->
      failTest ("expected deterministic runtime diagnostic for wrapped alias cycle, but evaluation raised " <> Text.pack (show err))
    Just (Right result) -> do
      assertEqual "compile errors" [] (runCompileErrors result)
      assertSingleDiagnosticContains
        "wrapped alias-only recursive cycle runtime code"
        "E3021"
        (runRuntimeErrors result)
      assertSingleDiagnosticContains
        "wrapped alias-only recursive cycle runtime text"
        "recursive alias cycle"
        (runRuntimeErrors result)
      assertEqual "runtime output is suppressed on runtime failure" Nothing (runOutput result)

testMixedWrappedAliasCycleRuntimeError :: IO ()
testMixedWrappedAliasCycleRuntimeError = do
  maybeResult <- timeout 1000000 (try (runSource defaultWarningSettings "f = if True g else \\(x) -> x. g = f. f 1.") :: IO (Either SomeException RunResult))
  case maybeResult of
    Nothing ->
      failTest "expected mixed wrapped alias cycle to terminate with a runtime diagnostic, but evaluation timed out"
    Just (Left err) ->
      failTest ("expected deterministic runtime diagnostic for mixed wrapped alias cycle, but evaluation raised " <> Text.pack (show err))
    Just (Right result) -> do
      assertEqual "compile errors" [] (runCompileErrors result)
      assertSingleDiagnosticContains
        "mixed wrapped alias cycle runtime code"
        "E3021"
        (runRuntimeErrors result)
      assertSingleDiagnosticContains
        "mixed wrapped alias cycle runtime text"
        "recursive alias cycle"
        (runRuntimeErrors result)
      assertEqual "runtime output is suppressed on runtime failure" Nothing (runOutput result)

testWrappedAliasCycleConditionRuntimeError :: IO ()
testWrappedAliasCycleConditionRuntimeError = do
  maybeResult <- timeout 1000000 (try (runSource defaultWarningSettings "f = if (1 / 0 == 0) g else g. g = f. f.") :: IO (Either SomeException RunResult))
  case maybeResult of
    Nothing ->
      failTest "expected wrapped alias cycle condition failure to terminate with a runtime diagnostic, but evaluation timed out"
    Just (Left err) ->
      failTest ("expected wrapped alias cycle condition failure to return a runtime diagnostic, but evaluation raised " <> Text.pack (show err))
    Just (Right result) -> do
      assertEqual "compile errors" [] (runCompileErrors result)
      assertSingleDiagnosticContains
        "wrapped alias cycle condition runtime code"
        "E3001"
        (runRuntimeErrors result)
      assertSingleDiagnosticContains
        "wrapped alias cycle condition runtime text"
        "division by zero"
        (runRuntimeErrors result)
      assertEqual "runtime output is suppressed on runtime failure" Nothing (runOutput result)

testPatternCaseAliasOnlyRecursiveCycleRuntimeError :: IO ()
testPatternCaseAliasOnlyRecursiveCycleRuntimeError = do
  maybeResult <- timeout 1000000 (try (runSource defaultWarningSettings "x = case 0 { | 0 -> y }. y = x. x.") :: IO (Either SomeException RunResult))
  case maybeResult of
    Nothing ->
      failTest "expected pattern-case alias-only recursive cycle to terminate with a runtime diagnostic, but evaluation timed out"
    Just (Left err) ->
      failTest ("expected deterministic runtime diagnostic for pattern-case alias cycle, but evaluation raised " <> Text.pack (show err))
    Just (Right result) -> do
      assertEqual "compile errors" [] (runCompileErrors result)
      assertSingleDiagnosticContains
        "pattern-case alias cycle runtime code"
        "E3021"
        (runRuntimeErrors result)
      assertSingleDiagnosticContains
        "pattern-case alias cycle runtime text"
        "recursive alias cycle"
        (runRuntimeErrors result)
      assertEqual "runtime output is suppressed on runtime failure" Nothing (runOutput result)

testPatternCaseBinderDoesNotAliasRecursivePeer :: IO ()
testPatternCaseBinderDoesNotAliasRecursivePeer = do
  result <- runSource defaultWarningSettings "x = case 0 { | y -> y }. y = x. x."
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "0") (runOutput result)

testPatternCaseGuardLambdaDoesNotClassifyNonFunctionRecursion :: IO ()
testPatternCaseGuardLambdaDoesNotClassifyNonFunctionRecursion = do
  maybeResult <- timeout 1000000 (try (runSource defaultWarningSettings "x = case 1 { | 0 if (\\(value) -> True) 0 -> 0 | _ -> x }. x.") :: IO (Either SomeException RunResult))
  case maybeResult of
    Nothing ->
      failTest "expected pattern-case guard-lambda recursion to terminate with a runtime diagnostic, but evaluation timed out"
    Just (Left err) ->
      failTest ("expected deterministic runtime diagnostic for guard-lambda recursion, but evaluation raised " <> Text.pack (show err))
    Just (Right result) -> do
      assertEqual "compile errors" [] (runCompileErrors result)
      assertSingleDiagnosticContains
        "guard-lambda recursion runtime code"
        "E3021"
        (runRuntimeErrors result)
      assertSingleDiagnosticContains
        "guard-lambda recursion runtime text"
        "no concrete value"
        (runRuntimeErrors result)
      assertEqual "runtime output is suppressed on runtime failure" Nothing (runOutput result)

testFunctionPatternGuardSelfReferenceRuntimeError :: IO ()
testFunctionPatternGuardSelfReferenceRuntimeError = do
  maybeResult <- timeout 1000000 (try (runSource defaultWarningSettings "f = case 1 { | 1 if f 0 == 0 -> \\(x) -> x | _ -> \\(x) -> x }. f 1.") :: IO (Either SomeException RunResult))
  case maybeResult of
    Nothing ->
      failTest "expected function-valued pattern guard self-reference to terminate with a runtime diagnostic, but evaluation timed out"
    Just (Left err) ->
      failTest ("expected deterministic runtime diagnostic for function-valued pattern guard self-reference, but evaluation raised " <> Text.pack (show err))
    Just (Right result) -> do
      assertEqual "compile errors" [] (runCompileErrors result)
      assertSingleDiagnosticContains
        "function-valued pattern guard self-reference runtime code"
        "E3021"
        (runRuntimeErrors result)
      assertSingleDiagnosticContains
        "function-valued pattern guard self-reference runtime text"
        "no concrete value"
        (runRuntimeErrors result)
      assertEqual "runtime output is suppressed on runtime failure" Nothing (runOutput result)

testBlockWrappedAliasOnlyRecursiveCycleRuntimeError :: IO ()
testBlockWrappedAliasOnlyRecursiveCycleRuntimeError = do
  maybeResult <- timeout 1000000 (try (runSource defaultWarningSettings "a = { b. }. b = { a. }. a.") :: IO (Either SomeException RunResult))
  case maybeResult of
    Nothing ->
      failTest "expected block-wrapped alias-only recursive cycle to terminate with a runtime diagnostic, but evaluation timed out"
    Just (Left err) ->
      failTest ("expected deterministic runtime diagnostic for block-wrapped alias cycle, but evaluation raised " <> Text.pack (show err))
    Just (Right result) -> do
      assertEqual "compile errors" [] (runCompileErrors result)
      assertSingleDiagnosticContains
        "block-wrapped alias-only recursive cycle runtime code"
        "E3021"
        (runRuntimeErrors result)
      assertSingleDiagnosticContains
        "block-wrapped alias-only recursive cycle runtime text"
        "recursive alias cycle"
        (runRuntimeErrors result)
      assertEqual "runtime output is suppressed on runtime failure" Nothing (runOutput result)

testNonFunctionRecursiveCycleRuntimeError :: IO ()
testNonFunctionRecursiveCycleRuntimeError = do
  maybeResult <- timeout 1000000 (try (runSource defaultWarningSettings "x = y + 1. y = x + 1. x.") :: IO (Either SomeException RunResult))
  case maybeResult of
    Nothing ->
      failTest "expected non-function recursive cycle to terminate with a runtime diagnostic, but evaluation timed out"
    Just (Left err) ->
      failTest ("expected deterministic runtime diagnostic for non-function recursive cycle, but evaluation raised " <> Text.pack (show err))
    Just (Right result) -> do
      assertEqual "compile errors" [] (runCompileErrors result)
      assertSingleDiagnosticContains
        "non-function recursive cycle runtime code"
        "E3021"
        (runRuntimeErrors result)
      assertSingleDiagnosticContains
        "non-function recursive cycle runtime text"
        "no concrete value"
        (runRuntimeErrors result)
      assertEqual "runtime output is suppressed on runtime failure" Nothing (runOutput result)

testNestedBlockAliasCycleIgnoresLaterOuterPeer :: IO ()
testNestedBlockAliasCycleIgnoresLaterOuterPeer = do
  maybeResult <- timeout 1000000 (try (runSource defaultWarningSettings "x = { y = z. z = y. y. }. z = x. x.") :: IO (Either SomeException RunResult))
  case maybeResult of
    Nothing ->
      failTest "expected nested block alias cycle to terminate with a runtime diagnostic, but evaluation timed out"
    Just (Left err) ->
      failTest ("expected nested block alias cycle to report a deterministic runtime diagnostic, but evaluation raised " <> Text.pack (show err))
    Just (Right result) -> do
      assertEqual "compile errors" [] (runCompileErrors result)
      assertSingleDiagnosticContains
        "nested block alias cycle runtime code"
        "E3021"
        (runRuntimeErrors result)
      assertSingleDiagnosticContains
        "nested block alias cycle runtime text"
        "recursive alias cycle"
        (runRuntimeErrors result)
      assertEqual "runtime output is suppressed on runtime failure" Nothing (runOutput result)

testRecursiveDeclaredUserOperatorRuntimeSuccess :: IO ()
testRecursiveDeclaredUserOperatorRuntimeSuccess = do
  result <-
    runSource
      defaultWarningSettings
      "operator %% tier 2.\n(%%) = \\(left) -> \\(right) -> if left == 0 right else (left - 1) %% right.\nx = 2 %% 3.\nx."
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "3") (runOutput result)

testRecursiveDeclaredUserOperatorValueAliasRuntimeError :: IO ()
testRecursiveDeclaredUserOperatorValueAliasRuntimeError = do
  maybeResult <-
    timeout
      1000000
      ( try
          (runSource defaultWarningSettings "operator %% tier 2.\n(%%) = (%%).\n1 %% 2.")
          :: IO (Either SomeException RunResult)
      )
  case maybeResult of
    Nothing ->
      failTest "expected declared operator value alias cycle to terminate with a runtime diagnostic, but evaluation timed out"
    Just (Left err) ->
      failTest ("expected deterministic runtime diagnostic for declared operator value alias cycle, but evaluation raised " <> Text.pack (show err))
    Just (Right result) -> do
      assertEqual "compile errors" [] (runCompileErrors result)
      assertSingleDiagnosticContains
        "declared operator value alias cycle runtime code"
        "E3021"
        (runRuntimeErrors result)
      assertSingleDiagnosticContains
        "declared operator value alias cycle runtime text"
        "recursive alias cycle"
        (runRuntimeErrors result)
      assertEqual "runtime output is suppressed on runtime failure" Nothing (runOutput result)

testIndirectRecursiveDeclaredUserOperatorValueAliasRuntimeError :: IO ()
testIndirectRecursiveDeclaredUserOperatorValueAliasRuntimeError = do
  maybeResult <-
    timeout
      1000000
      ( try
          (runSource defaultWarningSettings "operator %% tier 2.\n(%%) = alias.\nalias = (%%).\n1 %% 2.")
          :: IO (Either SomeException RunResult)
      )
  case maybeResult of
    Nothing ->
      failTest "expected indirect declared operator value alias cycle to terminate with a runtime diagnostic, but evaluation timed out"
    Just (Left err) ->
      failTest ("expected deterministic runtime diagnostic for indirect declared operator value alias cycle, but evaluation raised " <> Text.pack (show err))
    Just (Right result) -> do
      assertEqual "compile errors" [] (runCompileErrors result)
      assertSingleDiagnosticContains
        "indirect declared operator value alias cycle runtime code"
        "E3021"
        (runRuntimeErrors result)
      assertSingleDiagnosticContains
        "indirect declared operator value alias cycle runtime text"
        "recursive alias cycle"
        (runRuntimeErrors result)
      assertEqual "runtime output is suppressed on runtime failure" Nothing (runOutput result)

testQualifiedMethodDispatchRecursivelyDefaultsBoundIntegerLiterals :: IO ()
testQualifiedMethodDispatchRecursivelyDefaultsBoundIntegerLiterals = do
  result <-
    runSource
      defaultWarningSettings
      ( "class RuntimeApply(a) {\napply :: (a -> Bool) -> Bool.\n}.\n"
          <> "impl RuntimeApply(Int) {\napply = \\(fn) -> True.\n}.\n"
          <> "impl RuntimeApply(UInt8) {\napply = \\(fn) -> False.\n}.\n"
          <> "eq1 = (1 ==).\n"
          <> "RuntimeApply::apply eq1."
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "True") (runOutput result)

testQualifiedMethodDispatchRejectsMutualMethodAliasCycle :: IO ()
testQualifiedMethodDispatchRejectsMutualMethodAliasCycle = do
  maybeResult <-
    timeout
      1000000
      ( try
          ( runSource
              defaultWarningSettings
              ( "class RuntimeFlag(a) {\nenabled :: Bool.\nother :: Bool.\n}.\n"
                  <> "impl RuntimeFlag(Int) {\nenabled = RuntimeFlag::other.\nother = RuntimeFlag::enabled.\n}.\n"
                  <> "RuntimeFlag::enabled."
              )
          ) ::
          IO (Either SomeException RunResult)
      )
  case maybeResult of
    Nothing ->
      failTest "expected mutual qualified method alias cycle to terminate with a runtime diagnostic, but evaluation timed out"
    Just (Left err) ->
      failTest ("expected deterministic runtime diagnostic for mutual qualified method alias cycle, but evaluation raised " <> Text.pack (show err))
    Just (Right result) -> do
      assertEqual "compile errors" [] (runCompileErrors result)
      assertSingleDiagnosticContains
        "mutual qualified method alias runtime code"
        "E3021"
        (runRuntimeErrors result)
      assertSingleDiagnosticContains
        "mutual qualified method alias runtime text"
        "recursive qualified method alias cycle"
        (runRuntimeErrors result)
      assertEqual "runtime output is suppressed on runtime failure" Nothing (runOutput result)
