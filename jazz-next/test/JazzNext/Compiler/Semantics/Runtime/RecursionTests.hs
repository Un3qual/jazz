{-# LANGUAGE OverloadedStrings #-}

module JazzNext.Compiler.Semantics.Runtime.RecursionTests
  ( recursionTests
  ) where

import Control.Exception
  ( SomeException,
    try
  )
import Data.Functor.Identity
  ( Identity,
    runIdentity
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
    evaluateRuntimeExprWithHost,
    renderRuntimeValue,
    runtimeValueExactlyMatchesConstraint
  )
import JazzNext.Compiler.RuntimeHost
  ( RuntimeHost (..)
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
  [ ("tail-recursive closure is stack safe at bootstrap depth", testTailRecursiveClosureIsStackSafe)
    , ("tail-recursive case arm is stack safe", testTailRecursiveCaseArmIsStackSafe)
    , ("typed tail-recursive closure preserves result hints", testTypedTailRecursiveClosureIsStackSafe)
    , ("explicitly hinted tail recursion coalesces result obligations", testExplicitlyHintedTailRecursionCoalescesResultObligations)
    , ("pure and host evaluators preserve diagnostic parity", testPureAndHostDiagnosticsMatch)
    , ("alias-only recursive cycle produces deterministic runtime diagnostic", testAliasOnlyRecursiveCycleRuntimeError)
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

testTailRecursiveClosureIsStackSafe :: IO ()
testTailRecursiveClosureIsStackSafe =
  assertStackSafeRunResult
    "50,000-call pure tail recursion"
    ( runSource
        defaultWarningSettings
        ( "countDown = \\(remaining) -> "
            <> "if remaining == 0 0 else { "
            <> "next = remaining - 1. countDown next. }. "
            <> "countDown 50000."
        )
    )
    (Just "0")

testTailRecursiveCaseArmIsStackSafe :: IO ()
testTailRecursiveCaseArmIsStackSafe =
  assertStackSafeRunResult
    "10,000-call case-arm tail recursion"
    ( runSource
        defaultWarningSettings
        ( "countDown = \\(remaining) -> case remaining { "
            <> "| 0 -> 0 | _ -> countDown (remaining - 1) }. "
            <> "countDown 10000."
        )
    )
    (Just "0")

testTypedTailRecursiveClosureIsStackSafe :: IO ()
testTypedTailRecursiveClosureIsStackSafe =
  assertStackSafeRunResult
    "10,000-call typed tail recursion"
    ( runSource
        defaultWarningSettings
        ( "countDown :: Int -> Int. "
            <> "countDown = \\(remaining) -> "
            <> "if remaining == 0 0 else countDown (remaining - 1). "
            <> "countDown 10000."
        )
    )
    (Just "0")

testExplicitlyHintedTailRecursionCoalescesResultObligations :: IO ()
testExplicitlyHintedTailRecursionCoalescesResultObligations = do
  let recursionDepth = 1000
      isZero = EBinary "==" (EVar "remaining") (ELit (LInt 0))
      recurse =
        EApply
          (ETypeApplication (EVar "collect") (SourceSpan 2 20) TypeInt)
          (EBinary "-" (EVar "remaining") (ELit (LInt 1)))
      expression =
        EBlock
          [ SLet
              "collect"
              (SourceSpan 1 1)
              (ELambda "remaining" (EIf isZero (ELambda "value" (EVar "value")) recurse)),
            SExpr
              (SourceSpan 3 1)
              (EApply (EVar "collect") (ELit (LInt recursionDepth)))
          ]
  case evaluateRuntimeExpr expression of
    Right (Just runtimeValue) ->
      assertEqual
        "repeated explicit tail hint wrapper depth"
        1
        (explicitResultHintDepth runtimeValue)
    Left diagnostic ->
      failTest ("explicitly hinted tail recursion failed: " <> renderDiagnostic diagnostic)
    Right Nothing ->
      failTest "explicitly hinted tail recursion produced no result"
  where
    explicitResultHintDepth runtimeValue =
      case runtimeValue of
        VExplicitResultHint TypeInt innerValue -> 1 + explicitResultHintDepth innerValue
        _ -> 0

assertStackSafeRunResult :: Text -> IO RunResult -> Maybe Text -> IO ()
assertStackSafeRunResult label action expectedOutput = do
  maybeOutcome <-
    timeout
      30000000
      (try action :: IO (Either SomeException RunResult))
  case maybeOutcome of
    Nothing ->
      failTest (label <> " timed out")
    Just (Left err) ->
      failTest (label <> " leaked host exception: " <> Text.pack (show err))
    Just (Right result) -> do
      assertEqual (label <> " compile errors") [] (runCompileErrors result)
      assertEqual (label <> " runtime errors") [] (runRuntimeErrors result)
      assertEqual (label <> " output") expectedOutput (runOutput result)

diagnosticParityExpressions :: [Expr]
diagnosticParityExpressions =
  [ EVar "missing",
    EIf (ELit (LInt 1)) (ELit (LInt 2)) (ELit (LInt 3)),
    EApply (ELit (LInt 1)) (ELit (LInt 2)),
    EPatternCase (ELit (LInt 1)) []
  ]

diagnosticParityHost :: RuntimeHost Identity
diagnosticParityHost =
  RuntimeHost
    { runtimeHostReadText = \_ -> pure (Right ""),
      runtimeHostWriteText = \_ _ -> pure (Right ()),
      runtimeHostReadStdin = pure (Right ""),
      runtimeHostWriteStdout = \_ -> pure (Right ()),
      runtimeHostWriteStderr = \_ -> pure (Right ()),
      runtimeHostArguments = pure [],
      runtimeHostExit = \_ -> pure (Right ())
    }

testPureAndHostDiagnosticsMatch :: IO ()
testPureAndHostDiagnosticsMatch =
  mapM_ assertParity diagnosticParityExpressions
  where
    assertParity expression =
      case
          ( evaluateRuntimeExpr expression,
            runIdentity (evaluateRuntimeExprWithHost diagnosticParityHost expression)
          )
        of
          (Left pureDiagnostic, Left hostDiagnostic) ->
            assertEqual
              "pure/host rendered diagnostic"
              (renderDiagnostic pureDiagnostic)
              (renderDiagnostic hostDiagnostic)
          (pureResult, hostResult) ->
            failTest
              ( "expected matching diagnostic failures, found "
                  <> Text.pack (show pureResult)
                  <> " and "
                  <> Text.pack (show hostResult)
              )

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
