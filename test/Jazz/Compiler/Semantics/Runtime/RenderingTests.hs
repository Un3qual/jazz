{-# LANGUAGE OverloadedStrings #-}

module Jazz.Compiler.Semantics.Runtime.RenderingTests
  ( renderingTests,
  )
where

import Control.Exception
  ( SomeException,
    try,
  )
import qualified Data.Text as Text
import Jazz.Compiler.AST
  ( Literal (..),
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
import Jazz.Compiler.FractionalLiteral
  ( mkFractionalLiteralSource,
  )
import Jazz.Compiler.Runtime
  ( RuntimeAnnotation (..),
    RuntimeValue (..),
    evaluateRuntimeExpr,
  )
import Jazz.Compiler.Runtime.Semantics
  ( literalRuntimeValue,
    roundFloatTarget,
    runtimeValueMatchesLiteral,
  )
import Jazz.Compiler.Runtime.Types
  ( RuntimeIntMetadata (..),
    prependRuntimeExplicitResultHint,
  )
import Jazz.Compiler.Semantics.Runtime.Fixtures
import Jazz.Compiler.Semantics.Runtime.Shared
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
    assertContains,
    assertEqual,
    assertLeftDiagnosticCodeAndContains,
    assertSingleDiagnosticContains,
    failTest,
  )
import System.Timeout
  ( timeout,
  )

renderingTests :: [NamedTest]
renderingTests =
  [ ("Unit renders and participates in structural equality", testUnitRenderingAndEquality),
    ("Char and Text literals evaluate and render", testCharTextLiteralRendering),
    ("Char and Text strict equality evaluates", testCharTextStrictEquality),
    ("Char and Text literal patterns match", testCharTextLiteralPatterns),
    ("literal matching strips wrappers without comparing callables", testRuntimeValueMatchesLiteral),
    ("private text traversal primitives evaluate Unicode scalars", testPrivateTextTraversalRuntimeSuccess),
    ("private itemValue rendering primitive uses deterministic source rendering", testPrivateValueRenderingRuntimeSuccess),
    ("runtime fallback rejects non-Text traversal arguments", testRuntimeFallbackRejectsNonTextTraversalArguments),
    ("bootstrap collection and scalar primitives evaluate", testBootstrapCollectionScalarRuntimeSuccess),
    ("Unicode case and bulk text primitives evaluate", testUnicodeCaseAndBulkTextRuntimeSuccess),
    ("checked scalar conversion rejects non-scalars", testCheckedScalarConversionRejectsNonScalars),
    ("runtime fallback rejects invalid bootstrap primitive arguments", testRuntimeFallbackRejectsInvalidBootstrapPrimitiveArguments),
    ("direct self alias produces deterministic runtime diagnostic", testDirectSelfAliasRuntimeError),
    ("wrapped direct self alias produces deterministic runtime diagnostic", testWrappedDirectSelfAliasRuntimeError),
    ("same-name non-alias self application produces runtime unbound diagnostic", testSameNameNonAliasSelfApplicationTerminates),
    ("block wrapper with eager statement before alias terminal produces runtime unbound diagnostic", testBlockWrapperWithEagerStatementBeforeAliasTerminalTerminates),
    ("constructor over-application produces arity runtime diagnostic", testConstructorOverApplicationRuntimeError),
    ("bare dollar operator itemValue applies at runtime", testDollarOperatorValueRuntimeSuccess),
    ("bare operator itemValue applies at runtime", testBareOperatorValueRuntimeSuccess),
    ("explicit partial application of bare operator itemValue applies at runtime", testExplicitPartialOperatorValueRuntimeSuccess),
    ("left operator section applies at runtime", testLeftOperatorSectionRuntimeSuccess),
    ("right operator section applies at runtime", testRightOperatorSectionRuntimeSuccess),
    ("declared user operator infix applies at runtime", testDeclaredUserOperatorInfixRuntimeSuccess),
    ("declared custom precedence user operator groups at runtime", testDeclaredCustomPrecedenceUserOperatorRuntimeSuccess),
    ("declared user operator signature applies at runtime", testDeclaredUserOperatorSignatureRuntimeSuccess),
    ("declared user operator itemValue applies at runtime", testDeclaredUserOperatorValueRuntimeSuccess),
    ("declared user left operator section applies at runtime", testDeclaredUserLeftOperatorSectionRuntimeSuccess),
    ("declared user right operator section preserves argument order", testDeclaredUserRightOperatorSectionRuntimeSuccess),
    ("map + hd evaluates over nested list literals", testMapHdNestedListsRuntimeSuccess),
    ("filter keeps only matching list elements", testFilterRuntimeSuccess),
    ("tl returns the tail of a non-empty list", testTlReturnsTailRuntimeValue),
    ("tuple literal evaluates and renders at runtime", testTupleLiteralRuntimeValue),
    ("hd on empty list produces fatal runtime diagnostic", testHdEmptyListRuntimeError),
    ("tl on empty list produces fatal runtime diagnostic", testTlEmptyListRuntimeError),
    ("direct runtime helper rejects canonical prelude alias without bundled prelude", testRuntimeHelperRejectsCanonicalAlias),
    ("runtime fallback rejects kernel hd on non-list values", testRuntimeFallbackRejectsHdNonList),
    ("runtime fallback rejects kernel tl on non-list values", testRuntimeFallbackRejectsTlNonList),
    ("runtime fallback rejects kernel map with non-function mapper", testRuntimeFallbackRejectsMapNonFunctionMapper),
    ("runtime fallback rejects kernel map with non-list collection", testRuntimeFallbackRejectsMapNonListCollection),
    ("runtime fallback rejects kernel filter with non-function predicate", testRuntimeFallbackRejectsFilterNonFunctionPredicate),
    ("runtime fallback rejects kernel filter with non-list collection", testRuntimeFallbackRejectsFilterNonListCollection),
    ("runtime fallback rejects kernel filter predicate returning non-Bool", testRuntimeFallbackRejectsFilterPredicateNonBool),
    ("print! returns evaluated argument itemValue", testPrintBuiltinReturnsArgument),
    ("structural list equality evaluates at runtime", testStructuralListEqualityRuntimeSuccess),
    ("structural tuple equality evaluates at runtime", testStructuralTupleEqualityRuntimeSuccess),
    ("structural ADT equality evaluates at runtime", testStructuralAdtEqualityRuntimeSuccess),
    ("structural ADT equality sees through runtime type hints", testStructuralAdtEqualitySeesThroughRuntimeTypeHints),
    ("structural ADT equality preserves incompatible runtime type hints", testStructuralAdtEqualityPreservesIncompatibleRuntimeTypeHints),
    ("runtime fallback rejects direct callable equality", testRuntimeFallbackRejectsDirectCallableEquality),
    ("runtime fallback rejects direct callable inequality", testRuntimeFallbackRejectsDirectCallableInequality),
    ("runtime fallback rejects structural equality over functions", testRuntimeFallbackRejectsFunctionStructuralEquality),
    ("runtime fallback rejects different-length structural equality over functions", testRuntimeFallbackRejectsDifferentLengthFunctionStructuralEquality),
    ("runtime fallback rejects different saturated ADT constructors with function payloads", testRuntimeFallbackRejectsDifferentSaturatedAdtConstructors),
    ("scope with only declarations has no runtime output", testDeclarationOnlyScopeHasNoOutput),
    ("scope result requires terminal expression", testScopeDeclarationAfterExprClearsResult)
  ]

testUnitRenderingAndEquality :: IO ()
testUnitRenderingAndEquality = do
  result <- runSource defaultWarningSettings "(() == (), ())."
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "(True, ())") (runOutput result)

testCharTextLiteralRendering :: IO ()
testCharTextLiteralRendering = do
  result <- runSource defaultWarningSettings "('a', '\\n', \"Jazz\", \"a\\n\\\"b\")."
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "('a', '\\n', \"Jazz\", \"a\\n\\\"b\")") (runOutput result)

testCharTextStrictEquality :: IO ()
testCharTextStrictEquality = do
  result <- runSource defaultWarningSettings "('a' == 'a', 'a' != 'b', \"Jazz\" == \"Jazz\", \"Jazz\" != \"jazz\", Eq::equals 'a' 'a', Eq::equals \"Jazz\" \"Jazz\")."
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "(True, True, True, True, True, True)") (runOutput result)

testCharTextLiteralPatterns :: IO ()
testCharTextLiteralPatterns = do
  result <- runSource defaultWarningSettings "(case 'a' { | 'a' -> 1 | _ -> 0 }, case \"Jazz\" { | \"Jazz\" -> 1 | _ -> 0 })."
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "(1, 1)") (runOutput result)

testRuntimeValueMatchesLiteral :: IO ()
testRuntimeValueMatchesLiteral = do
  assertEqual
    "typed integer literal matches"
    True
    (runtimeValueMatchesLiteral (VAnnotated (RuntimeTypeHint SemanticInt) (VInt 7 (RuntimeIntMetadata Nothing))) (LInt 7))
  assertEqual
    "nested type wrappers preserve literal matching"
    True
    ( runtimeValueMatchesLiteral
        ( VAnnotated
            (RuntimeTypeHint SemanticInt)
            ( VAnnotated
                (RuntimeTypeApplication SemanticInt)
                (prependRuntimeExplicitResultHint SemanticInt (VInt 7 (RuntimeIntMetadata Nothing)))
            )
        )
        (LInt 7)
    )
  assertEqual
    "different text literal does not match"
    False
    (runtimeValueMatchesLiteral (VText "Jazz") (LText "jazz"))
  let float32Literal = LFloat 1.1 (mkFractionalLiteralSource 1 1 1) (Just NumericFloat32)
  assertEqual
    "targeted Float32 test value rounds away from source Double"
    False
    (1.1 == roundFloatTarget NumericFloat32 1.1)
  assertEqual
    "targeted Float32 literal matches its rounded runtime value"
    True
    (runtimeValueMatchesLiteral (literalRuntimeValue float32Literal) float32Literal)
  case evaluateRuntimeExpr (runtimeExpr (expressionLambda "item" (expressionVariable "item"))) of
    Right (Just closureRuntimeValue) ->
      assertEqual
        "closure is never a literal match"
        False
        (runtimeValueMatchesLiteral closureRuntimeValue (LBool True))
    otherResult ->
      failTest ("expected closure runtime value, got " <> Text.pack (show otherResult))

testPrivateTextTraversalRuntimeSuccess :: IO ()
testPrivateTextTraversalRuntimeSuccess = do
  result <-
    runSource
      defaultWarningSettings
      "(__kernel_textLength \"\", __kernel_textLength \"a🙂é\", __kernel_textUnconsRaw \"\", __kernel_textUnconsRaw \"🙂x\")."
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "(0, 3, [], [('🙂', \"x\")])") (runOutput result)

testPrivateValueRenderingRuntimeSuccess :: IO ()
testPrivateValueRenderingRuntimeSuccess = do
  let result =
        evaluateRuntimeExpr
          ( runtimeExpr
              ( expressionApply
                  (expressionVariable "__kernel_renderValue")
                  (expressionTuple [expressionLiteral (LChar 'a'), expressionLiteral (LText "\n")])
              )
          )
  case result of
    Right (Just (VText renderedValue)) ->
      assertEqual "private itemValue renderer" "('a', \"\\n\")" renderedValue
    Right otherValue ->
      failTest ("expected rendered Text value, got " <> Text.pack (show otherValue))
    Left runtimeError ->
      failTest ("expected rendered Text value, got " <> Text.pack (show runtimeError))

testRuntimeFallbackRejectsNonTextTraversalArguments :: IO ()
testRuntimeFallbackRejectsNonTextTraversalArguments = do
  let lengthResult =
        evaluateRuntimeExpr
          (runtimeExpr (expressionApply (expressionVariable "__kernel_textLength") (expressionLiteral (LInt 1))))
      unconsResult =
        evaluateRuntimeExpr
          (runtimeExpr (expressionApply (expressionVariable "__kernel_textUnconsRaw") (expressionLiteral (LInt 1))))
  assertRuntimeErrorContains "runtime fallback textLength code" "E3028" lengthResult
  assertRuntimeErrorContains "runtime fallback textLength actual type" "Int" lengthResult
  assertRuntimeErrorContains "runtime fallback textUnconsRaw code" "E3029" unconsResult
  assertRuntimeErrorContains "runtime fallback textUnconsRaw actual type" "Int" unconsResult

testBootstrapCollectionScalarRuntimeSuccess :: IO ()
testBootstrapCollectionScalarRuntimeSuccess = do
  result <-
    runSource
      defaultWarningSettings
      ( "(__kernel_listPrependRaw \"first\" [\"second\"], "
          <> "__kernel_listReverseRaw [\"first\", \"second\"], "
          <> "__kernel_charToUInt32 '\\u{1F642}', "
          <> "__kernel_charFromUInt32Raw (toUInt32 128578), "
          <> "(__kernel_charIsAlpha 'é', __kernel_charIsAlphaNum '9', __kernel_charIsDigit '9', __kernel_charIsSpace '\\t', __kernel_charIsHexDigit 'F'), "
          <> "__kernel_textAppendChar (__kernel_textAppend \"Ja\" \"z\") 'z', "
          <> "__kernel_textFromChars ['J', '🙂', 'z'])."
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual
    "bootstrap primitive output"
    (Just "([\"first\", \"second\"], [\"second\", \"first\"], 128578, ['🙂'], (True, True, True, True, True), \"Jazz\", \"J🙂z\")")
    (runOutput result)

testUnicodeCaseAndBulkTextRuntimeSuccess :: IO ()
testUnicodeCaseAndBulkTextRuntimeSuccess = do
  result <-
    runSource
      defaultWarningSettings
      """
      (__kernel_charIsLower 'é', __kernel_charIsUpper 'É',
       __kernel_charToLower 'É', __kernel_charToUpper 'é',
       __kernel_textConcat ["Ja", "zz", "🙂"]).
      """
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual
    "case and bulk text output"
    (Just "(True, True, 'é', 'É', \"Jazz🙂\")")
    (runOutput result)

testCheckedScalarConversionRejectsNonScalars :: IO ()
testCheckedScalarConversionRejectsNonScalars = do
  result <-
    runSource
      defaultWarningSettings
      "(__kernel_charFromUInt32Raw (toUInt32 55296), __kernel_charFromUInt32Raw (toUInt32 1114112))."
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "non-scalar conversion" (Just "([], [])") (runOutput result)

testRuntimeFallbackRejectsInvalidBootstrapPrimitiveArguments :: IO ()
testRuntimeFallbackRejectsInvalidBootstrapPrimitiveArguments = do
  let prependResult =
        evaluateRuntimeExpr
          (runtimeExpr (expressionApply (expressionApply (expressionVariable "__kernel_listPrependRaw") (expressionLiteral (LInt 1))) (expressionLiteral (LInt 2))))
      charToResult =
        evaluateRuntimeExpr
          (runtimeExpr (expressionApply (expressionVariable "__kernel_charToUInt32") (expressionLiteral (LText "a"))))
      charFromResult =
        evaluateRuntimeExpr
          (runtimeExpr (expressionApply (expressionVariable "__kernel_charFromUInt32Raw") (expressionLiteral (LInt (-1)))))
      predicateResult =
        evaluateRuntimeExpr
          (runtimeExpr (expressionApply (expressionVariable "__kernel_charIsAlpha") (expressionLiteral (LText "a"))))
      caseResult =
        evaluateRuntimeExpr
          (runtimeExpr (expressionApply (expressionVariable "__kernel_charToLower") (expressionLiteral (LText "a"))))
      appendResult =
        evaluateRuntimeExpr
          (runtimeExpr (expressionApply (expressionApply (expressionVariable "__kernel_textAppend") (expressionLiteral (LText "a"))) (expressionLiteral (LBool True))))
      appendCharResult =
        evaluateRuntimeExpr
          (runtimeExpr (expressionApply (expressionApply (expressionVariable "__kernel_textAppendChar") (expressionLiteral (LText "a"))) (expressionLiteral (LInt 1))))
      reverseResult =
        evaluateRuntimeExpr
          (runtimeExpr (expressionApply (expressionVariable "__kernel_listReverseRaw") (expressionLiteral (LInt 1))))
      textFromCharsListResult =
        evaluateRuntimeExpr
          (runtimeExpr (expressionApply (expressionVariable "__kernel_textFromChars") (expressionLiteral (LText "Jazz"))))
      textFromCharsElementResult =
        evaluateRuntimeExpr
          (runtimeExpr (expressionApply (expressionVariable "__kernel_textFromChars") (expressionList [expressionLiteral (LInt 1)])))
      textConcatListResult =
        evaluateRuntimeExpr
          (runtimeExpr (expressionApply (expressionVariable "__kernel_textConcat") (expressionLiteral (LText "Jazz"))))
      textConcatElementResult =
        evaluateRuntimeExpr
          (runtimeExpr (expressionApply (expressionVariable "__kernel_textConcat") (expressionList [expressionLiteral (LInt 1)])))
  assertRuntimeErrorContains "list prepend argument" "E3032" prependResult
  assertRuntimeErrorContains "char to scalar argument" "E3033" charToResult
  assertRuntimeErrorContains "scalar to char argument" "E3034" charFromResult
  assertRuntimeErrorContains "char predicate argument" "E3035" predicateResult
  assertRuntimeErrorContains "char case argument" "E3035" caseResult
  assertRuntimeErrorContains "text append argument" "E3036" appendResult
  assertRuntimeErrorContains "text append char argument" "E3037" appendCharResult
  assertRuntimeErrorContains "list reverse argument" "E3038" reverseResult
  assertRuntimeErrorContains "text from chars list argument" "E3039" textFromCharsListResult
  assertRuntimeErrorContains "text from chars element argument" "E3039" textFromCharsElementResult
  assertRuntimeErrorContains "text concat list argument" "E3040" textConcatListResult
  assertRuntimeErrorContains "text concat element argument" "E3040" textConcatElementResult

testDirectSelfAliasRuntimeError :: IO ()
testDirectSelfAliasRuntimeError = do
  maybeResult <- timeout 1000000 (try (runSource defaultWarningSettings "f = f. f.") :: IO (Either SomeException RunResult))
  case maybeResult of
    Nothing ->
      failTest "expected direct self alias to terminate with a runtime diagnostic, but evaluation timed out"
    Just (Left err) ->
      failTest ("expected deterministic runtime diagnostic for direct self alias, but evaluation raised " <> Text.pack (show err))
    Just (Right result) -> do
      assertEqual "compile errors" [] (runCompileErrors result)
      assertSingleDiagnosticContains
        "direct self alias runtime code"
        "E3021"
        (runRuntimeErrors result)
      assertSingleDiagnosticContains
        "direct self alias runtime text"
        "recursive alias cycle"
        (runRuntimeErrors result)
      assertEqual "runtime output is suppressed on runtime failure" Nothing (runOutput result)

testWrappedDirectSelfAliasRuntimeError :: IO ()
testWrappedDirectSelfAliasRuntimeError = do
  maybeResult <- timeout 1000000 (try (runSource defaultWarningSettings "f = if True then f else 0. f.") :: IO (Either SomeException RunResult))
  case maybeResult of
    Nothing ->
      failTest "expected wrapped direct self alias to terminate with a runtime diagnostic, but evaluation timed out"
    Just (Left err) ->
      failTest ("expected deterministic runtime diagnostic for wrapped direct self alias, but evaluation raised " <> Text.pack (show err))
    Just (Right result) -> do
      assertEqual "compile errors" [] (runCompileErrors result)
      assertSingleDiagnosticContains
        "wrapped direct self alias runtime code"
        "E3021"
        (runRuntimeErrors result)
      assertSingleDiagnosticContains
        "wrapped direct self alias runtime text"
        "recursive alias cycle"
        (runRuntimeErrors result)
      assertEqual "runtime output is suppressed on runtime failure" Nothing (runOutput result)

testSameNameNonAliasSelfApplicationTerminates :: IO ()
testSameNameNonAliasSelfApplicationTerminates = do
  maybeResult <- timeout 1000000 (try (runSource defaultWarningSettings "f = (\\(x) -> x) f. f 1.") :: IO (Either SomeException RunResult))
  case maybeResult of
    Nothing ->
      failTest "expected same-name non-alias self application to terminate with a runtime diagnostic, but evaluation timed out"
    Just (Left err) ->
      failTest ("expected same-name non-alias self application to report a runtime diagnostic, but evaluation raised " <> Text.pack (show err))
    Just (Right result) -> do
      assertEqual "compile errors" [] (runCompileErrors result)
      assertSingleDiagnosticContains
        "same-name non-alias self application runtime code"
        "E3002"
        (runRuntimeErrors result)
      assertSingleDiagnosticContains
        "same-name non-alias self application runtime text"
        "unbound variable 'f'"
        (runRuntimeErrors result)
      assertEqual "runtime output is suppressed on failure" Nothing (runOutput result)

testBlockWrapperWithEagerStatementBeforeAliasTerminalTerminates :: IO ()
testBlockWrapperWithEagerStatementBeforeAliasTerminalTerminates = do
  maybeResult <- timeout 1000000 (try (runSource defaultWarningSettings "f = { f + 1. f. }. 0.") :: IO (Either SomeException RunResult))
  case maybeResult of
    Nothing ->
      failTest "expected block wrapper with eager statement before alias terminal to terminate with a runtime diagnostic, but evaluation timed out"
    Just (Left err) ->
      failTest ("expected block wrapper with eager statement before alias terminal to report a runtime diagnostic, but evaluation raised " <> Text.pack (show err))
    Just (Right result) -> do
      assertEqual "compile errors" [] (runCompileErrors result)
      assertSingleDiagnosticContains
        "block wrapper eager statement runtime code"
        "E3002"
        (runRuntimeErrors result)
      assertSingleDiagnosticContains
        "block wrapper eager statement runtime text"
        "unbound variable 'f'"
        (runRuntimeErrors result)
      assertEqual "runtime output is suppressed on failure" Nothing (runOutput result)

testConstructorOverApplicationRuntimeError :: IO ()
testConstructorOverApplicationRuntimeError = do
  let result = evaluateRuntimeExpr overAppliedConstructorExpr
  assertLeftDiagnosticCodeAndContains
    "constructor over-application runtime code"
    "E3023"
    "constructor 'Just' expected 1 argument but received 2"
    result

testDollarOperatorValueRuntimeSuccess :: IO ()
testDollarOperatorValueRuntimeSuccess = do
  result <- runSource defaultWarningSettings "($) (1 +) 2."
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "3") (runOutput result)

testBareOperatorValueRuntimeSuccess :: IO ()
testBareOperatorValueRuntimeSuccess = do
  result <- runSource defaultWarningSettings "(+) 1 2."
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "3") (runOutput result)

testExplicitPartialOperatorValueRuntimeSuccess :: IO ()
testExplicitPartialOperatorValueRuntimeSuccess = do
  result <- runSource defaultWarningSettings "((+) 1) 2."
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "3") (runOutput result)

testLeftOperatorSectionRuntimeSuccess :: IO ()
testLeftOperatorSectionRuntimeSuccess = do
  result <- runSource defaultWarningSettings "(1 +) 2."
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "3") (runOutput result)

testRightOperatorSectionRuntimeSuccess :: IO ()
testRightOperatorSectionRuntimeSuccess = do
  result <- runSource defaultWarningSettings "(+ 1) 2."
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "3") (runOutput result)

testDeclaredUserOperatorInfixRuntimeSuccess :: IO ()
testDeclaredUserOperatorInfixRuntimeSuccess = do
  result <-
    runSource
      defaultWarningSettings
      """
      operator %% tier 2.
      (%%) = \\(left, right) -> left + right.
      1 %% 2.
      """
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "3") (runOutput result)

testDeclaredCustomPrecedenceUserOperatorRuntimeSuccess :: IO ()
testDeclaredCustomPrecedenceUserOperatorRuntimeSuccess = do
  result <-
    runSource
      defaultWarningSettings
      """
      operator %% precedence 99.
      (%%) = \\(left, right) -> left - right.
      20 + 10 %% 3 * 2.
      """
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "34") (runOutput result)

testDeclaredUserOperatorSignatureRuntimeSuccess :: IO ()
testDeclaredUserOperatorSignatureRuntimeSuccess = do
  result <-
    runSource
      defaultWarningSettings
      """
      operator %% tier 2.
      (%%) :: Int -> Int -> Int.
      (%%) = \\(left, right) -> left + right.
      1 %% 2.
      """
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "3") (runOutput result)

testDeclaredUserOperatorValueRuntimeSuccess :: IO ()
testDeclaredUserOperatorValueRuntimeSuccess = do
  result <-
    runSource
      defaultWarningSettings
      """
      operator %% tier 2.
      (%%) = \\(left, right) -> left + right.
      (%%) 1 2.
      """
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "3") (runOutput result)

testDeclaredUserLeftOperatorSectionRuntimeSuccess :: IO ()
testDeclaredUserLeftOperatorSectionRuntimeSuccess = do
  result <-
    runSource
      defaultWarningSettings
      """
      operator %% tier 2.
      (%%) = \\(left, right) -> left - right.
      (2 %%) 10.
      """
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "-8") (runOutput result)

testDeclaredUserRightOperatorSectionRuntimeSuccess :: IO ()
testDeclaredUserRightOperatorSectionRuntimeSuccess = do
  result <-
    runSource
      defaultWarningSettings
      """
      operator %% tier 2.
      (%%) = \\(left, right) -> left - right.
      (%% 2) 10.
      """
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "8") (runOutput result)

testMapHdNestedListsRuntimeSuccess :: IO ()
testMapHdNestedListsRuntimeSuccess = do
  result <- runSource defaultWarningSettings "map hd [[1, 2], [3], [4, 5]]."
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "[1, 3, 4]") (runOutput result)

testFilterRuntimeSuccess :: IO ()
testFilterRuntimeSuccess = do
  result <- runSource defaultWarningSettings "filter (> 1) [1, 2, 3, 1]."
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "[2, 3]") (runOutput result)

testTlReturnsTailRuntimeValue :: IO ()
testTlReturnsTailRuntimeValue = do
  result <- runSource defaultWarningSettings "tl [1, 2, 3]."
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "[2, 3]") (runOutput result)

testTupleLiteralRuntimeValue :: IO ()
testTupleLiteralRuntimeValue = do
  result <- runSource defaultWarningSettings "(1, True)."
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "(1, True)") (runOutput result)

testHdEmptyListRuntimeError :: IO ()
testHdEmptyListRuntimeError = do
  result <- runSource defaultWarningSettings "hd []."
  let runtimeErrors = runRuntimeErrors result
  assertEqual "compile errors" [] (runCompileErrors result)
  assertSingleDiagnosticContains
    "runtime fatal empty-list hd"
    "E3009"
    runtimeErrors
  case runtimeErrors of
    [] ->
      fail "expected empty-list hd runtime error, but got no runtime errors"
    runtimeError : _ ->
      assertContains
        "runtime fatal mentions empty list"
        "empty list"
        (renderDiagnostic runtimeError)
  assertEqual "runtime output is suppressed on runtime failure" Nothing (runOutput result)

testTlEmptyListRuntimeError :: IO ()
testTlEmptyListRuntimeError = do
  result <- runSource defaultWarningSettings "tl []."
  let runtimeErrors = runRuntimeErrors result
  assertEqual "compile errors" [] (runCompileErrors result)
  assertSingleDiagnosticContains
    "runtime fatal empty-list tl"
    "E3010"
    runtimeErrors
  case runtimeErrors of
    [] ->
      fail "expected empty-list tl runtime error, but got no runtime errors"
    runtimeError : _ ->
      assertContains
        "runtime fatal mentions empty list"
        "empty list"
        (renderDiagnostic runtimeError)
  assertEqual "runtime output is suppressed on runtime failure" Nothing (runOutput result)

testRuntimeHelperRejectsCanonicalAlias :: IO ()
testRuntimeHelperRejectsCanonicalAlias = do
  let result = evaluateRuntimeExpr (runtimeExpr (expressionVariable "map"))
  assertRuntimeErrorContains "runtime helper canonical alias rejected" "E3002" result

testRuntimeFallbackRejectsHdNonList :: IO ()
testRuntimeFallbackRejectsHdNonList = do
  let result = evaluateRuntimeExpr (runtimeExpr (expressionApply (expressionVariable "__kernel_hd") (expressionLiteral (LInt 1))))
  assertRuntimeErrorContains "runtime fallback hd non-list" "E3011" result

testRuntimeFallbackRejectsTlNonList :: IO ()
testRuntimeFallbackRejectsTlNonList = do
  let result = evaluateRuntimeExpr (runtimeExpr (expressionApply (expressionVariable "__kernel_tl") (expressionLiteral (LInt 1))))
  assertRuntimeErrorContains "runtime fallback tl non-list" "E3012" result

testRuntimeFallbackRejectsMapNonFunctionMapper :: IO ()
testRuntimeFallbackRejectsMapNonFunctionMapper = do
  let result = evaluateRuntimeExpr (runtimeExpr (expressionApply (expressionApply (expressionVariable "__kernel_map") (expressionLiteral (LInt 1))) (expressionList [expressionLiteral (LInt 1)])))
  assertRuntimeErrorContains "runtime fallback map mapper" "E3015" result

testRuntimeFallbackRejectsMapNonListCollection :: IO ()
testRuntimeFallbackRejectsMapNonListCollection = do
  let result = evaluateRuntimeExpr (runtimeExpr (expressionApply (expressionApply (expressionVariable "__kernel_map") (expressionVariable "__kernel_hd")) (expressionLiteral (LInt 1))))
  assertRuntimeErrorContains "runtime fallback map collection" "E3013" result

testRuntimeFallbackRejectsFilterNonFunctionPredicate :: IO ()
testRuntimeFallbackRejectsFilterNonFunctionPredicate = do
  let result = evaluateRuntimeExpr (runtimeExpr (expressionApply (expressionApply (expressionVariable "__kernel_filter") (expressionLiteral (LInt 1))) (expressionList [expressionLiteral (LInt 1)])))
  assertRuntimeErrorContains "runtime fallback filter predicate" "E3017" result

testRuntimeFallbackRejectsFilterNonListCollection :: IO ()
testRuntimeFallbackRejectsFilterNonListCollection = do
  let result = evaluateRuntimeExpr (runtimeExpr (expressionApply (expressionApply (expressionVariable "__kernel_filter") (expressionSectionLeft (expressionLiteral (LInt 1)) "<")) (expressionLiteral (LInt 1))))
  assertRuntimeErrorContains "runtime fallback filter collection" "E3018" result

testRuntimeFallbackRejectsFilterPredicateNonBool :: IO ()
testRuntimeFallbackRejectsFilterPredicateNonBool = do
  let result = evaluateRuntimeExpr (runtimeExpr (expressionApply (expressionApply (expressionVariable "__kernel_filter") (expressionSectionLeft (expressionLiteral (LInt 1)) "+")) (expressionList [expressionLiteral (LInt 1)])))
  assertRuntimeErrorContains "runtime fallback filter predicate bool result" "E3019" result

testPrintBuiltinReturnsArgument :: IO ()
testPrintBuiltinReturnsArgument = do
  result <- runSource defaultWarningSettings "print! 1."
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "1") (runOutput result)

testStructuralListEqualityRuntimeSuccess :: IO ()
testStructuralListEqualityRuntimeSuccess = do
  result <-
    runSource
      defaultWarningSettings
      """
      same = [1, 2] == [1, 2].
      different = [1, 2] != [1, 3].
      shorter = [1] == [1, 2].
      nested = [[True], [False]] == [[True], [False]].
      [same, different, shorter, nested].
      """
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "[True, True, False, True]") (runOutput result)

testStructuralTupleEqualityRuntimeSuccess :: IO ()
testStructuralTupleEqualityRuntimeSuccess = do
  result <-
    runSource
      defaultWarningSettings
      """
      same = (1, True) == (1, True).
      different = (1, (True, 2)) != (1, (True, 3)).
      [same, different].
      """
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "[True, True]") (runOutput result)

testStructuralAdtEqualityRuntimeSuccess :: IO ()
testStructuralAdtEqualityRuntimeSuccess = do
  result <-
    runSource
      defaultWarningSettings
      """
      data Maybe a = Nothing | Just a.
      same = Just 1 == Just 1.
      differentPayload = Just 1 != Just 2.
      differentCtor = Just 1 == Nothing.
      nested = Just [True] == Just [True].
      [same, differentPayload, differentCtor, nested].
      """
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "[True, True, False, True]") (runOutput result)

testStructuralAdtEqualitySeesThroughRuntimeTypeHints :: IO ()
testStructuralAdtEqualitySeesThroughRuntimeTypeHints = do
  let result =
        evaluateRuntimeExpr
          ( expressionBlock
              [ statementData (SourceSpan 1 1) "Tag" ["a"] [dataConstructor "Tag" []],
                statementLet "left" (SourceSpan 2 1) (typedTag NumericUInt8),
                statementLet "right" (SourceSpan 3 1) (typedTag NumericUInt8),
                statementExpression (SourceSpan 4 1) (expressionBinary "==" (expressionVariable "left") (expressionVariable "right"))
              ]
          )
  assertRuntimeBool "typed ADT structural equality runtime result" True result
  where
    typedTag numericType =
      expressionConstrainedAs
        (TypeApplication (fixtureResolvedTypeName "Tag") [TypeNumeric numericType])
        (expressionConstructor "Tag")

testStructuralAdtEqualityPreservesIncompatibleRuntimeTypeHints :: IO ()
testStructuralAdtEqualityPreservesIncompatibleRuntimeTypeHints = do
  let result =
        evaluateRuntimeExpr
          ( expressionBlock
              [ statementData (SourceSpan 1 1) "Tag" ["a"] [dataConstructor "Tag" []],
                statementLet "left" (SourceSpan 2 1) (typedTag NumericUInt8),
                statementLet "right" (SourceSpan 3 1) (typedTag NumericUInt16),
                statementExpression (SourceSpan 4 1) (expressionBinary "==" (expressionVariable "left") (expressionVariable "right"))
              ]
          )
  assertRuntimeBool "incompatible typed ADT structural equality runtime result" False result
  where
    typedTag numericType =
      expressionConstrainedAs
        (TypeApplication (fixtureResolvedTypeName "Tag") [TypeNumeric numericType])
        (expressionConstructor "Tag")

testRuntimeFallbackRejectsDirectCallableEquality :: IO ()
testRuntimeFallbackRejectsDirectCallableEquality = do
  assertCallableRuntimeEqualityRejected
    "runtime closure equality"
    (expressionBinary "==" closureValue closureValue)
  assertCallableRuntimeEqualityRejected
    "runtime builtin equality"
    (expressionBinary "==" builtinValue builtinValue)
  assertCallableRuntimeEqualityRejected
    "runtime operator equality"
    (expressionBinary "==" operatorValue operatorValue)
  assertCallableRuntimeEqualityRejected
    "runtime left section equality"
    (expressionBinary "==" leftSectionValue leftSectionValue)

testRuntimeFallbackRejectsDirectCallableInequality :: IO ()
testRuntimeFallbackRejectsDirectCallableInequality = do
  assertCallableRuntimeEqualityRejected
    "runtime closure inequality"
    (expressionBinary "!=" closureValue closureValue)
  assertCallableRuntimeEqualityRejected
    "runtime right section inequality"
    (expressionBinary "!=" rightSectionValue rightSectionValue)

testRuntimeFallbackRejectsFunctionStructuralEquality :: IO ()
testRuntimeFallbackRejectsFunctionStructuralEquality = do
  let identity = expressionLambda "x" (expressionVariable "x")
      result = evaluateRuntimeExpr (runtimeExpr (expressionBinary "==" (expressionList [identity]) (expressionList [identity])))
  assertRuntimeErrorContains "runtime fallback function structural equality" "E3007" result
  assertRuntimeErrorContains
    "runtime fallback function structural equality callable text"
    "callable values are not equality-supported"
    result

testRuntimeFallbackRejectsDifferentLengthFunctionStructuralEquality :: IO ()
testRuntimeFallbackRejectsDifferentLengthFunctionStructuralEquality = do
  let identity = expressionLambda "x" (expressionVariable "x")
  assertCallableRuntimeEqualityRejected
    "different-length function structural equality"
    (expressionBinary "==" (expressionList [identity]) (expressionList [identity, identity]))

testRuntimeFallbackRejectsDifferentSaturatedAdtConstructors :: IO ()
testRuntimeFallbackRejectsDifferentSaturatedAdtConstructors = do
  let result = evaluateRuntimeExpr differentSaturatedAdtConstructorEqualityExpr
  assertRuntimeErrorContains "different saturated ADT constructor equality code" "E3007" result
  assertRuntimeErrorContains
    "different saturated ADT constructor equality callable text"
    "callable values are not equality-supported"
    result
  where
    identity = expressionLambda "x" (expressionVariable "x")

    differentSaturatedAdtConstructorEqualityExpr =
      expressionBlock
        [ statementData
            (SourceSpan 1 1)
            "Maybe"
            ["a"]
            [ dataConstructor "Nothing" [],
              dataConstructor "Just" [fixtureTypeVariable "a"]
            ],
          statementExpression
            (SourceSpan 2 1)
            (expressionBinary "==" (expressionApply (expressionConstructor "Just") identity) (expressionConstructor "Nothing"))
        ]

testDeclarationOnlyScopeHasNoOutput :: IO ()
testDeclarationOnlyScopeHasNoOutput = do
  result <- runSource defaultWarningSettings "x = 1."
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "declaration-only scope produces no output" Nothing (runOutput result)

testScopeDeclarationAfterExprClearsResult :: IO ()
testScopeDeclarationAfterExprClearsResult = do
  result <- runSource defaultWarningSettings "x = 1. x. y = 2."
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "declaration after expression clears scope result" Nothing (runOutput result)
