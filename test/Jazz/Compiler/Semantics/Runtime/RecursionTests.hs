{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Jazz.Compiler.Semantics.Runtime.RecursionTests
  ( recursionTests,
    recursionScaleTests,
  )
where

import Control.Exception
  ( SomeException,
    evaluate,
    try,
  )
import Data.Functor.Identity
  ( Identity,
    runIdentity,
  )
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Jazz.Compiler.AST
  ( CorePhase (Analyzed),
    Expr,
    Literal (..),
  )
import Jazz.Compiler.Diagnostics
  ( SourceSpan (..),
  )
import Jazz.Compiler.Diagnostics.Render
  ( renderDiagnostic,
  )
import Jazz.Compiler.Driver
  ( RunResult,
    runCompileErrors,
    runOutput,
    runRuntimeErrors,
    runSource,
  )
import Jazz.Compiler.ModuleExports (exportInventory)
import Jazz.Compiler.ModuleIdentity (preludeModulePath)
import Jazz.Compiler.ModuleResolver (resolveStandaloneExprNames)
import Jazz.Compiler.Name
  ( Name (..),
    NameNamespace (ValueNamespace),
    ResolvedNameOrigin (CurrentModule),
    ResolvedUserName (..),
    mkIdentifier,
  )
import Jazz.Compiler.RecursiveBindings (prepareAnalyzedScope)
import Jazz.Compiler.Runtime
  ( RuntimeValue (..),
    renderRuntimeValue,
    runtimeExplicitResultHintsInOrder,
    runtimeValueExactlyMatchesConstraint,
  )
import Jazz.Compiler.Runtime.ScopePlan
  ( RuntimeScopePlan,
    buildRuntimeScopePlan,
    scopePlanIsRecursiveBinding,
    scopePlanIsSelfRecursiveFunction,
    scopePlanModulePathForStatement,
  )
import Jazz.Compiler.Runtime.Semantics (runtimeDefinitionName)
import Jazz.Compiler.RuntimeHost
  ( RuntimeHost (..),
    RuntimeHostExit (..),
  )
import Jazz.Compiler.Semantics.Runtime.Fixtures
import Jazz.Compiler.Semantics.Runtime.ResolvedFixture
import Jazz.Compiler.SourceProgram
  ( parseAndLowerStandaloneSource,
  )
import Jazz.Compiler.SourceUnitOwnership (SourceUnitOwner (..))
import Jazz.Compiler.TypeInference (analyzeSourceUnitExpression)
import Jazz.Compiler.TypeRepresentation
  ( NumericType (..),
    SemanticType (..),
    SignatureType (..),
  )
import Jazz.Compiler.WarningConfig
  ( defaultWarningSettings,
  )
import Jazz.TestHarness
  ( NamedTest,
    assertEqual,
    assertSingleDiagnosticContains,
    failTest,
  )
import System.Timeout
  ( timeout,
  )

recursionScaleTests :: [NamedTest]
recursionScaleTests =
  [ ("tail-recursive closure is stack safe at bootstrap depth", testTailRecursiveClosureIsStackSafe),
    ("tail-recursive case arm is stack safe", testTailRecursiveCaseArmIsStackSafe),
    ("typed tail-recursive closure preserves result hints", testTypedTailRecursiveClosureIsStackSafe),
    ("explicitly hinted tail recursion preserves result obligations", testExplicitlyHintedTailRecursionPreservesResultObligations),
    ("100,000 explicit result hints render and apply stack safely", testExplicitResultHintsRenderAndApplyStackSafely)
  ]

recursionTests :: [NamedTest]
recursionTests =
  [ ("mixed explicit result hints preserve order and multiplicity", testMixedExplicitResultHintsPreserveOrderAndMultiplicity),
    ("pure and host evaluators preserve diagnostic parity", testPureAndHostDiagnosticsMatch),
    ("alias-only recursive cycle produces deterministic runtime diagnostic", testAliasOnlyRecursiveCycleRuntimeError),
    ("wrapped alias-only recursive cycle produces deterministic runtime diagnostic", testWrappedAliasOnlyRecursiveCycleRuntimeError),
    ("mixed wrapped alias cycle still produces deterministic runtime diagnostic", testMixedWrappedAliasCycleRuntimeError),
    ("wrapped alias cycle still evaluates wrapper condition first", testWrappedAliasCycleConditionRuntimeError),
    ("pattern-case alias-only recursive cycle produces deterministic runtime diagnostic", testPatternCaseAliasOnlyRecursiveCycleRuntimeError),
    ("pattern-case binder shadows recursive peer during alias resolution", testPatternCaseBinderDoesNotAliasRecursivePeer),
    ("pattern-case binder blocks false recursive function visibility", testPatternCaseBinderDoesNotGainRecursiveFunctionVisibility),
    ("prelude scope planning uses its nonempty module path", testPreludeScopePlanUsesNonemptyModulePath),
    ("standalone runtime owners do not impersonate the prelude", testStandaloneRuntimeOwnerIsNotPrelude),
    ("pattern-case binder preserves alias definition recursive visibility", testPatternCaseBinderPreservesAliasDefinitionRecursiveVisibility),
    ("builtin names stay outside self-recursive function visibility", testBuiltinNameDoesNotGainSelfRecursiveVisibility),
    ("pattern-case guard lambda does not classify non-function recursion", testPatternCaseGuardLambdaDoesNotClassifyNonFunctionRecursion),
    ("function-valued pattern guard self-reference produces recursion diagnostic", testFunctionPatternGuardSelfReferenceRuntimeError),
    ("block-wrapped alias-only recursive cycle produces deterministic runtime diagnostic", testBlockWrappedAliasOnlyRecursiveCycleRuntimeError),
    ("non-function recursive cycle produces deterministic runtime diagnostic", testNonFunctionRecursiveCycleRuntimeError),
    ("nested block alias cycle ignores later outer peer name", testNestedBlockAliasCycleIgnoresLaterOuterPeer),
    ( "nested recursive forward alias preserves callable recursion",
      testNestedRecursiveForwardAliasRuntimeSuccess
    ),
    ("recursive declared user operator applies at runtime", testRecursiveDeclaredUserOperatorRuntimeSuccess),
    ("recursive declared user operator itemValue alias produces deterministic runtime diagnostic", testRecursiveDeclaredUserOperatorValueAliasRuntimeError),
    ("indirect recursive declared user operator itemValue alias produces deterministic runtime diagnostic", testIndirectRecursiveDeclaredUserOperatorValueAliasRuntimeError),
    ("qualified method dispatch recursively defaults bound integer literals", testQualifiedMethodDispatchRecursivelyDefaultsBoundIntegerLiterals),
    ("qualified method dispatch rejects mutual method alias cycle", testQualifiedMethodDispatchRejectsMutualMethodAliasCycle)
  ]

testStandaloneRuntimeOwnerIsNotPrelude :: IO ()
testStandaloneRuntimeOwnerIsNotPrelude = do
  let localName =
        UserName
          (ResolvedUserName CurrentModule ValueNamespace (mkIdentifier "itemValue"))
  assertEqual
    "standalone runtime owner"
    localName
    (runtimeDefinitionName Nothing localName)

testTailRecursiveClosureIsStackSafe :: IO ()
testTailRecursiveClosureIsStackSafe =
  assertStackSafeRunResult
    "50,000-call pure tail recursion"
    ( runSource
        defaultWarningSettings
        ( "countDown = \\(remaining) -> "
            <> "if remaining == 0 then 0 else { "
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
            <> "if remaining == 0 then 0 else countDown (remaining - 1). "
            <> "countDown 10000."
        )
    )
    (Just "0")

testExplicitlyHintedTailRecursionPreservesResultObligations :: IO ()
testExplicitlyHintedTailRecursionPreservesResultObligations = do
  let recursionDepth :: Int
      recursionDepth = 1000
      isZero = expressionBinary "==" (expressionVariable "remaining") (expressionLiteral (LInt 0))
      recurse =
        expressionApply
          (expressionTypeApplication (expressionVariable "collect") (SourceSpan 2 20) TypeInt)
          (expressionBinary "-" (expressionVariable "remaining") (expressionLiteral (LInt 1)))
      expression =
        expressionBlock
          [ statementLet
              "collect"
              (SourceSpan 1 1)
              (expressionLambda "remaining" (expressionIf isZero (expressionLambda "itemValue" (expressionVariable "itemValue")) recurse)),
            statementExpression
              (SourceSpan 3 1)
              (expressionApply (expressionVariable "collect") (expressionLiteral (LInt (fromIntegral recursionDepth))))
          ]
  case evaluateFixture expression of
    Right (Just runtimeValue) ->
      assertEqual
        "repeated explicit tail hints in outermost-to-innermost order"
        (replicate recursionDepth SemanticInt)
        (runtimeExplicitResultHintsInOrder runtimeValue)
    Left diagnostic ->
      failTest ("explicitly hinted tail recursion failed: " <> renderDiagnostic diagnostic)
    Right Nothing ->
      failTest "explicitly hinted tail recursion produced no result"

testExplicitResultHintsRenderAndApplyStackSafely :: IO ()
testExplicitResultHintsRenderAndApplyStackSafely = do
  let recursionDepth = 100000
      callableExpression = explicitlyHintedCallable recursionDepth
      appliedExpression = expressionApply callableExpression (expressionLiteral (LInt 7))
  outcome <-
    try
      ( timeout
          30000000
          ( do
              callableValue <- requireRuntimeValue "100,000-hint callable" callableExpression
              let renderedCallable = renderRuntimeValue callableValue
                  observedHints = runtimeExplicitResultHintsInOrder callableValue
              _ <- evaluate (Text.length renderedCallable)
              observedCount <- evaluate (length observedHints)
              allHintsMatch <- evaluate (all (== SemanticInt) observedHints)
              appliedValue <- requireRuntimeValue "100,000-hint application" appliedExpression
              let renderedAppliedValue = renderRuntimeValue appliedValue
              _ <- evaluate (Text.length renderedAppliedValue)
              pure (renderedCallable, observedCount, allHintsMatch, renderedAppliedValue)
          )
      ) ::
      IO (Either SomeException (Maybe (Text, Int, Bool, Text)))
  case outcome of
    Right Nothing ->
      failTest "100,000 explicit result hints timed out while rendering or applying"
    Left err ->
      failTest ("100,000 explicit result hints leaked host exception: " <> Text.pack (show err))
    Right (Just (renderedCallable, observedCount, allHintsMatch, renderedAppliedValue)) -> do
      assertEqual "hinted callable rendering" "<function>" renderedCallable
      assertEqual "explicit result hint multiplicity" recursionDepth observedCount
      assertEqual "explicit result hint identity" True allHintsMatch
      assertEqual "hinted callable application" "7" renderedAppliedValue

testMixedExplicitResultHintsPreserveOrderAndMultiplicity :: IO ()
testMixedExplicitResultHintsPreserveOrderAndMultiplicity = do
  let uint8 = SemanticNumeric NumericUInt8
      callableExpression = mixedExplicitlyHintedCallable 6
      appliedExpression = expressionApply callableExpression (expressionLiteral (LInt 7))
  runtimeValue <- requireRuntimeValue "mixed explicit result hints" callableExpression
  assertEqual
    "mixed hints remain outermost-to-innermost without deduplication"
    [uint8, SemanticInt, SemanticBool, uint8, SemanticInt, SemanticBool]
    (runtimeExplicitResultHintsInOrder runtimeValue)
  case runtimeValue of
    VAnnotated annotation _ ->
      assertEqual
        "reattaching result annotations combines their ordered obligations"
        [uint8, SemanticInt, SemanticBool, uint8, SemanticInt, SemanticBool, uint8, SemanticInt, SemanticBool, uint8, SemanticInt, SemanticBool]
        (runtimeExplicitResultHintsInOrder (VAnnotated annotation runtimeValue))
    _ -> failTest "expected pending result annotations on the callable"
  appliedValue <- requireRuntimeValue "mixed explicit result hint application" appliedExpression
  assertEqual "mixed explicit result hint application renders" "7" (renderRuntimeValue appliedValue)
  assertEqual
    "mixed explicit result hint application retains final UInt8 result"
    True
    (runtimeValueExactlyMatchesConstraint uint8 appliedValue)
  assertEqual
    "mixed explicit result hint application does not retain intermediate Int result"
    False
    (runtimeValueExactlyMatchesConstraint SemanticInt appliedValue)

mixedExplicitlyHintedCallable :: Int -> Expr 'Analyzed
mixedExplicitlyHintedCallable recursionDepth =
  let uint8 = TypeNumeric NumericUInt8
      isZero = expressionBinary "==" (expressionVariable "remaining") (expressionLiteral (LInt 0))
      decrement = expressionBinary "-" (expressionVariable "remaining") (expressionLiteral (LInt 1))
      hintedCall functionName line typeHint =
        expressionApply
          (expressionTypeApplication (expressionVariable functionName) (SourceSpan line 20) typeHint)
          decrement
      collect functionName line nextFunctionName typeHint =
        statementLet
          functionName
          (SourceSpan line 1)
          (expressionLambda "remaining" (expressionIf isZero (expressionLambda "itemValue" (expressionVariable "itemValue")) (hintedCall nextFunctionName line typeHint)))
   in expressionBlock
        [ collect "collectUInt8" 1 "collectInt" uint8,
          collect "collectInt" 2 "collectBool" TypeInt,
          collect "collectBool" 3 "collectUInt8" TypeBool,
          statementExpression
            (SourceSpan 4 1)
            (expressionApply (expressionVariable "collectUInt8") (expressionLiteral (LInt (fromIntegral recursionDepth))))
        ]

explicitlyHintedCallable :: Int -> Expr 'Analyzed
explicitlyHintedCallable recursionDepth =
  let isZero = expressionBinary "==" (expressionVariable "remaining") (expressionLiteral (LInt 0))
      recurse =
        expressionApply
          (expressionTypeApplication (expressionVariable "collect") (SourceSpan 2 20) TypeInt)
          (expressionBinary "-" (expressionVariable "remaining") (expressionLiteral (LInt 1)))
   in expressionBlock
        [ statementLet
            "collect"
            (SourceSpan 1 1)
            (expressionLambda "remaining" (expressionIf isZero (expressionLambda "itemValue" (expressionVariable "itemValue")) recurse)),
          statementExpression
            (SourceSpan 3 1)
            (expressionApply (expressionVariable "collect") (expressionLiteral (LInt (fromIntegral recursionDepth))))
        ]

requireRuntimeValue :: Text -> Expr 'Analyzed -> IO RuntimeValue
requireRuntimeValue label expression =
  case evaluateFixture expression of
    Left diagnostic ->
      failTest (label <> " failed: " <> renderDiagnostic diagnostic)
    Right Nothing ->
      failTest (label <> " produced no result")
    Right (Just runtimeValue) ->
      pure runtimeValue

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

diagnosticParityExpressions :: [Expr 'Analyzed]
diagnosticParityExpressions =
  [ expressionVariable "missing",
    expressionIf (expressionLiteral (LInt 1)) (expressionLiteral (LInt 2)) (expressionLiteral (LInt 3)),
    expressionApply (expressionLiteral (LInt 1)) (expressionLiteral (LInt 2)),
    expressionPatternCase (expressionLiteral (LInt 1)) []
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
      runtimeHostExit = \_ -> pure (Right RuntimeHostExitReturned)
    }

testPureAndHostDiagnosticsMatch :: IO ()
testPureAndHostDiagnosticsMatch =
  mapM_ assertParity diagnosticParityExpressions
  where
    assertParity expression =
      case ( evaluateFixture expression,
             runIdentity (evaluateFixtureWithHost diagnosticParityHost expression)
           ) of
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
  maybeResult <- timeout 1000000 (try (runSource defaultWarningSettings "f = if True then g else g. g = f. f.") :: IO (Either SomeException RunResult))
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
  maybeResult <- timeout 1000000 (try (runSource defaultWarningSettings "f = if True then g else \\(x) -> x. g = f. f 1.") :: IO (Either SomeException RunResult))
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
  maybeResult <- timeout 1000000 (try (runSource defaultWarningSettings "f = if (1 / 0 == 0) then g else g. g = f. f.") :: IO (Either SomeException RunResult))
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
  maybeResult <- timeout 1000000 (try (runSource defaultWarningSettings "x = case 0 { | 0 -> y | _ -> y }. y = x. x.") :: IO (Either SomeException RunResult))
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

testPatternCaseBinderDoesNotGainRecursiveFunctionVisibility :: IO ()
testPatternCaseBinderDoesNotGainRecursiveFunctionVisibility = do
  let plan =
        buildRuntimeScopePlan
          preludeModulePath
          Set.empty
          Nothing
          (prepareAnalyzedScope (resolveRuntimeFixture (expressionBlock witnessStatements)))
  assertEqual "pattern-binder witness is not a runtime recursive group" False (scopePlanIsRecursiveBinding plan 0)
  assertEqual "pattern-binder witness gets no recursive function visibility" False (scopePlanIsSelfRecursiveFunction plan 0)
  result <- runSource defaultWarningSettings witnessSource
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "True") (runOutput result)
  where
    witnessSource =
      "f = { apparent = \\(x) -> x. captured = \\(x) -> f. case True { | apparent -> apparent }. }. f."
    witnessStatements =
      [ statementLet
          "f"
          (SourceSpan 1 1)
          ( expressionBlock
              [ statementLet "apparent" (SourceSpan 1 1) (expressionLambda "x" (expressionVariable "x")),
                statementLet "captured" (SourceSpan 1 1) (expressionLambda "x" (expressionVariable "f")),
                statementExpression
                  (SourceSpan 1 1)
                  ( expressionPatternCase
                      (expressionLiteral (LBool True))
                      [caseArm (patternVariable "apparent") Nothing (expressionVariable "apparent")]
                  )
              ]
          ),
        statementExpression (SourceSpan 1 1) (expressionVariable "f")
      ]

testPreludeScopePlanUsesNonemptyModulePath :: IO ()
testPreludeScopePlanUsesNonemptyModulePath = do
  let plan =
        buildRuntimeScopePlan
          preludeModulePath
          (Set.singleton 0)
          Nothing
          (prepareAnalyzedScope (resolveRuntimeFixture (expressionBlock [statementLet "preludeValue" (SourceSpan 1 1) (expressionLiteral (LInt 1))])))
  assertEqual
    "prelude statement path"
    (Just (InjectedPreludeSourceUnit preludeModulePath Nothing))
    (scopePlanModulePathForStatement plan 0)

testPatternCaseBinderPreservesAliasDefinitionRecursiveVisibility :: IO ()
testPatternCaseBinderPreservesAliasDefinitionRecursiveVisibility = do
  plan <- scopePlanForSource executableWitnessSource
  assertEqual "definition-site witness is a runtime recursive group" True (scopePlanIsRecursiveBinding plan 0)
  assertEqual "definition-site witness gets recursive function visibility" True (scopePlanIsSelfRecursiveFunction plan 0)
  result <- runSource defaultWarningSettings executableWitnessSource
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "0") (runOutput result)
  where
    executableWitnessSource =
      "f = { target = \\(x) -> if x == 0 then 0 else f (x - 1). alias = target. case True { | target -> alias }. }. f 1."

testBuiltinNameDoesNotGainSelfRecursiveVisibility :: IO ()
testBuiltinNameDoesNotGainSelfRecursiveVisibility = do
  result <- runSource defaultWarningSettings witnessSource
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "[1]") (runOutput result)
  where
    witnessSource =
      "map = \\(items) -> map (\\(item) -> item) items. map [1]."

scopePlanForSource :: Text -> IO RuntimeScopePlan
scopePlanForSource source =
  case parseAndLowerStandaloneSource source of
    Left diagnostic ->
      failTest ("expected scope-plan witness source to parse and lower: " <> renderDiagnostic diagnostic)
    Right loweredExpression ->
      case resolveStandaloneExprNames (exportInventory []) loweredExpression of
        Left diagnostics ->
          failTest
            ( "expected scope-plan witness source to resolve: "
                <> Text.intercalate "\n" (map renderDiagnostic (toList diagnostics))
            )
        Right resolvedExpression -> do
          (_, attachment) <-
            analyzeSourceUnitExpression
              preludeModulePath
              Set.empty
              Set.empty
              defaultWarningSettings
              resolvedExpression
          analyzedExpression <-
            case attachment of
              Left failures -> failTest ("scope-plan witness facts failed: " <> Text.pack (show failures))
              Right Nothing -> failTest "scope-plan witness produced no analyzed expression"
              Right (Just expression) -> pure expression
          pure
            ( buildRuntimeScopePlan
                preludeModulePath
                Set.empty
                Nothing
                (prepareAnalyzedScope analyzedExpression)
            )
  where
    toList (diagnostic :| diagnostics) = diagnostic : diagnostics

testPatternCaseGuardLambdaDoesNotClassifyNonFunctionRecursion :: IO ()
testPatternCaseGuardLambdaDoesNotClassifyNonFunctionRecursion = do
  maybeResult <- timeout 1000000 (try (runSource defaultWarningSettings "x = case 1 { | 0 if (\\(itemValue) -> True) 0 -> 0 | _ -> x }. x.") :: IO (Either SomeException RunResult))
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

testNestedRecursiveForwardAliasRuntimeSuccess :: IO ()
testNestedRecursiveForwardAliasRuntimeSuccess = do
  plan <- scopePlanForSource source
  assertEqual "nested forward alias is a runtime recursive group" True (scopePlanIsRecursiveBinding plan 0)
  assertEqual "nested forward alias gets recursive function visibility" True (scopePlanIsSelfRecursiveFunction plan 0)
  result <- runSource defaultWarningSettings source
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "0") (runOutput result)
  where
    source =
      "f = { a = b. b = if False then a else \\(x) -> if x == 0 then 0 else f (x - 1). a. }. f 3."

testRecursiveDeclaredUserOperatorRuntimeSuccess :: IO ()
testRecursiveDeclaredUserOperatorRuntimeSuccess = do
  result <-
    runSource
      defaultWarningSettings
      """
      operator %% tier 2.
      (%%) = \\(left, right) -> if left == 0 then right else (left - 1) %% right.
      x = 2 %% 3.
      x.
      """
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "3") (runOutput result)

testRecursiveDeclaredUserOperatorValueAliasRuntimeError :: IO ()
testRecursiveDeclaredUserOperatorValueAliasRuntimeError = do
  maybeResult <-
    timeout
      1000000
      ( try
          ( runSource
              defaultWarningSettings
              """
              operator %% tier 2.
              (%%) = (%%).
              1 %% 2.
              """
          ) ::
          IO (Either SomeException RunResult)
      )
  case maybeResult of
    Nothing ->
      failTest "expected declared operator itemValue alias cycle to terminate with a runtime diagnostic, but evaluation timed out"
    Just (Left err) ->
      failTest ("expected deterministic runtime diagnostic for declared operator itemValue alias cycle, but evaluation raised " <> Text.pack (show err))
    Just (Right result) -> do
      assertEqual "compile errors" [] (runCompileErrors result)
      assertSingleDiagnosticContains
        "declared operator itemValue alias cycle runtime code"
        "E3021"
        (runRuntimeErrors result)
      assertSingleDiagnosticContains
        "declared operator itemValue alias cycle runtime text"
        "recursive alias cycle"
        (runRuntimeErrors result)
      assertEqual "runtime output is suppressed on runtime failure" Nothing (runOutput result)

testIndirectRecursiveDeclaredUserOperatorValueAliasRuntimeError :: IO ()
testIndirectRecursiveDeclaredUserOperatorValueAliasRuntimeError = do
  maybeResult <-
    timeout
      1000000
      ( try
          ( runSource
              defaultWarningSettings
              """
              operator %% tier 2.
              (%%) = alias.
              alias = (%%).
              1 %% 2.
              """
          ) ::
          IO (Either SomeException RunResult)
      )
  case maybeResult of
    Nothing ->
      failTest "expected indirect declared operator itemValue alias cycle to terminate with a runtime diagnostic, but evaluation timed out"
    Just (Left err) ->
      failTest ("expected deterministic runtime diagnostic for indirect declared operator itemValue alias cycle, but evaluation raised " <> Text.pack (show err))
    Just (Right result) -> do
      assertEqual "compile errors" [] (runCompileErrors result)
      assertSingleDiagnosticContains
        "indirect declared operator itemValue alias cycle runtime code"
        "E3021"
        (runRuntimeErrors result)
      assertSingleDiagnosticContains
        "indirect declared operator itemValue alias cycle runtime text"
        "recursive alias cycle"
        (runRuntimeErrors result)
      assertEqual "runtime output is suppressed on runtime failure" Nothing (runOutput result)

testQualifiedMethodDispatchRecursivelyDefaultsBoundIntegerLiterals :: IO ()
testQualifiedMethodDispatchRecursivelyDefaultsBoundIntegerLiterals = do
  result <-
    runSource
      defaultWarningSettings
      ( """
        class RuntimeApply(a) {
        apply :: (a -> Bool) -> Bool.
        }.
        impl RuntimeApply(Int) {
        apply = \\(fn) -> True.
        }.
        impl RuntimeApply(UInt8) {
        apply = \\(fn) -> False.
        }.
        eq1 = (1 ==).
        RuntimeApply::apply eq1.
        """
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
              ( """
                class RuntimeFlag(a) {
                enabled :: Bool.
                other :: Bool.
                }.
                impl RuntimeFlag(Int) {
                enabled = RuntimeFlag::other.
                other = RuntimeFlag::enabled.
                }.
                RuntimeFlag::enabled.
                """
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
