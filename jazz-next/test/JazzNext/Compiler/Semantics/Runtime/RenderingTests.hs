{-# LANGUAGE OverloadedStrings #-}

module JazzNext.Compiler.Semantics.Runtime.RenderingTests
  ( renderingTests
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
    ConstraintSignatureType (..),
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

renderingTests :: [NamedTest]
renderingTests =
  [ ("Unit renders and participates in structural equality", testUnitRenderingAndEquality)
    , ("Char and Text literals evaluate and render", testCharTextLiteralRendering)
    , ("Char and Text strict equality evaluates", testCharTextStrictEquality)
    , ("Char and Text literal patterns match", testCharTextLiteralPatterns)
    , ("direct self alias produces deterministic runtime diagnostic", testDirectSelfAliasRuntimeError)
    , ("wrapped direct self alias produces deterministic runtime diagnostic", testWrappedDirectSelfAliasRuntimeError)
    , ("same-name non-alias self application produces runtime unbound diagnostic", testSameNameNonAliasSelfApplicationTerminates)
    , ("block wrapper with eager statement before alias terminal produces runtime unbound diagnostic", testBlockWrapperWithEagerStatementBeforeAliasTerminalTerminates)
    , ("constructor over-application produces arity runtime diagnostic", testConstructorOverApplicationRuntimeError)
    , ("bare dollar operator value applies at runtime", testDollarOperatorValueRuntimeSuccess)
    , ("bare operator value applies at runtime", testBareOperatorValueRuntimeSuccess)
    , ("explicit partial application of bare operator value applies at runtime", testExplicitPartialOperatorValueRuntimeSuccess)
    , ("left operator section applies at runtime", testLeftOperatorSectionRuntimeSuccess)
    , ("right operator section applies at runtime", testRightOperatorSectionRuntimeSuccess)
    , ("declared user operator infix applies at runtime", testDeclaredUserOperatorInfixRuntimeSuccess)
    , ("declared custom precedence user operator groups at runtime", testDeclaredCustomPrecedenceUserOperatorRuntimeSuccess)
    , ("declared user operator signature applies at runtime", testDeclaredUserOperatorSignatureRuntimeSuccess)
    , ("declared user operator value applies at runtime", testDeclaredUserOperatorValueRuntimeSuccess)
    , ("declared user left operator section applies at runtime", testDeclaredUserLeftOperatorSectionRuntimeSuccess)
    , ("declared user right operator section preserves argument order", testDeclaredUserRightOperatorSectionRuntimeSuccess)
    , ("map + hd evaluates over nested list literals", testMapHdNestedListsRuntimeSuccess)
    , ("filter keeps only matching list elements", testFilterRuntimeSuccess)
    , ("tl returns the tail of a non-empty list", testTlReturnsTailRuntimeValue)
    , ("tuple literal evaluates and renders at runtime", testTupleLiteralRuntimeValue)
    , ("hd on empty list produces fatal runtime diagnostic", testHdEmptyListRuntimeError)
    , ("tl on empty list produces fatal runtime diagnostic", testTlEmptyListRuntimeError)
    , ("direct runtime helper rejects canonical prelude alias without bundled prelude", testRuntimeHelperRejectsCanonicalAlias)
    , ("runtime fallback rejects kernel hd on non-list values", testRuntimeFallbackRejectsHdNonList)
    , ("runtime fallback rejects kernel tl on non-list values", testRuntimeFallbackRejectsTlNonList)
    , ("runtime fallback rejects kernel map with non-function mapper", testRuntimeFallbackRejectsMapNonFunctionMapper)
    , ("runtime fallback rejects kernel map with non-list collection", testRuntimeFallbackRejectsMapNonListCollection)
    , ("runtime fallback rejects kernel filter with non-function predicate", testRuntimeFallbackRejectsFilterNonFunctionPredicate)
    , ("runtime fallback rejects kernel filter with non-list collection", testRuntimeFallbackRejectsFilterNonListCollection)
    , ("runtime fallback rejects kernel filter predicate returning non-Bool", testRuntimeFallbackRejectsFilterPredicateNonBool)
    , ("print! returns evaluated argument value", testPrintBuiltinReturnsArgument)
    , ("structural list equality evaluates at runtime", testStructuralListEqualityRuntimeSuccess)
    , ("structural tuple equality evaluates at runtime", testStructuralTupleEqualityRuntimeSuccess)
    , ("structural ADT equality evaluates at runtime", testStructuralAdtEqualityRuntimeSuccess)
    , ("structural ADT equality sees through runtime type hints", testStructuralAdtEqualitySeesThroughRuntimeTypeHints)
    , ("structural ADT equality preserves incompatible runtime type hints", testStructuralAdtEqualityPreservesIncompatibleRuntimeTypeHints)
    , ("runtime fallback rejects direct callable equality", testRuntimeFallbackRejectsDirectCallableEquality)
    , ("runtime fallback rejects direct callable inequality", testRuntimeFallbackRejectsDirectCallableInequality)
    , ("runtime fallback rejects structural equality over functions", testRuntimeFallbackRejectsFunctionStructuralEquality)
    , ("runtime fallback rejects different-length structural equality over functions", testRuntimeFallbackRejectsDifferentLengthFunctionStructuralEquality)
    , ("runtime fallback rejects different saturated ADT constructors with function payloads", testRuntimeFallbackRejectsDifferentSaturatedAdtConstructors)
    , ("scope with only declarations has no runtime output", testDeclarationOnlyScopeHasNoOutput)
    , ("scope result requires terminal expression", testScopeDeclarationAfterExprClearsResult)
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
  maybeResult <- timeout 1000000 (try (runSource defaultWarningSettings "f = if True f else 0. f.") :: IO (Either SomeException RunResult))
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
  result <- runSource defaultWarningSettings "operator %% tier 2.\n(%%) = \\(left) -> \\(right) -> left + right.\n1 %% 2."
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "3") (runOutput result)

testDeclaredCustomPrecedenceUserOperatorRuntimeSuccess :: IO ()
testDeclaredCustomPrecedenceUserOperatorRuntimeSuccess = do
  result <- runSource defaultWarningSettings "operator %% precedence 99.\n(%%) = \\(left) -> \\(right) -> left - right.\n20 + 10 %% 3 * 2."
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "34") (runOutput result)

testDeclaredUserOperatorSignatureRuntimeSuccess :: IO ()
testDeclaredUserOperatorSignatureRuntimeSuccess = do
  result <- runSource defaultWarningSettings "operator %% tier 2.\n(%%) :: Int -> Int -> Int.\n(%%) = \\(left) -> \\(right) -> left + right.\n1 %% 2."
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "3") (runOutput result)

testDeclaredUserOperatorValueRuntimeSuccess :: IO ()
testDeclaredUserOperatorValueRuntimeSuccess = do
  result <- runSource defaultWarningSettings "operator %% tier 2.\n(%%) = \\(left) -> \\(right) -> left + right.\n(%%) 1 2."
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "3") (runOutput result)

testDeclaredUserLeftOperatorSectionRuntimeSuccess :: IO ()
testDeclaredUserLeftOperatorSectionRuntimeSuccess = do
  result <- runSource defaultWarningSettings "operator %% tier 2.\n(%%) = \\(left) -> \\(right) -> left - right.\n(2 %%) 10."
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "-8") (runOutput result)

testDeclaredUserRightOperatorSectionRuntimeSuccess :: IO ()
testDeclaredUserRightOperatorSectionRuntimeSuccess = do
  result <- runSource defaultWarningSettings "operator %% tier 2.\n(%%) = \\(left) -> \\(right) -> left - right.\n(%% 2) 10."
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
  let result = evaluateRuntimeExpr (runtimeExpr (EVar "map"))
  assertRuntimeErrorContains "runtime helper canonical alias rejected" "E3002" result

testRuntimeFallbackRejectsHdNonList :: IO ()
testRuntimeFallbackRejectsHdNonList = do
  let result = evaluateRuntimeExpr (runtimeExpr (EApply (EVar "__kernel_hd") (ELit (LInt 1))))
  assertRuntimeErrorContains "runtime fallback hd non-list" "E3011" result

testRuntimeFallbackRejectsTlNonList :: IO ()
testRuntimeFallbackRejectsTlNonList = do
  let result = evaluateRuntimeExpr (runtimeExpr (EApply (EVar "__kernel_tl") (ELit (LInt 1))))
  assertRuntimeErrorContains "runtime fallback tl non-list" "E3012" result

testRuntimeFallbackRejectsMapNonFunctionMapper :: IO ()
testRuntimeFallbackRejectsMapNonFunctionMapper = do
  let result = evaluateRuntimeExpr (runtimeExpr (EApply (EApply (EVar "__kernel_map") (ELit (LInt 1))) (EList [ELit (LInt 1)])))
  assertRuntimeErrorContains "runtime fallback map mapper" "E3015" result

testRuntimeFallbackRejectsMapNonListCollection :: IO ()
testRuntimeFallbackRejectsMapNonListCollection = do
  let result = evaluateRuntimeExpr (runtimeExpr (EApply (EApply (EVar "__kernel_map") (EVar "__kernel_hd")) (ELit (LInt 1))))
  assertRuntimeErrorContains "runtime fallback map collection" "E3013" result

testRuntimeFallbackRejectsFilterNonFunctionPredicate :: IO ()
testRuntimeFallbackRejectsFilterNonFunctionPredicate = do
  let result = evaluateRuntimeExpr (runtimeExpr (EApply (EApply (EVar "__kernel_filter") (ELit (LInt 1))) (EList [ELit (LInt 1)])))
  assertRuntimeErrorContains "runtime fallback filter predicate" "E3017" result

testRuntimeFallbackRejectsFilterNonListCollection :: IO ()
testRuntimeFallbackRejectsFilterNonListCollection = do
  let result = evaluateRuntimeExpr (runtimeExpr (EApply (EApply (EVar "__kernel_filter") (ESectionLeft (ELit (LInt 1)) "<")) (ELit (LInt 1))))
  assertRuntimeErrorContains "runtime fallback filter collection" "E3018" result

testRuntimeFallbackRejectsFilterPredicateNonBool :: IO ()
testRuntimeFallbackRejectsFilterPredicateNonBool = do
  let result = evaluateRuntimeExpr (runtimeExpr (EApply (EApply (EVar "__kernel_filter") (ESectionLeft (ELit (LInt 1)) "+")) (EList [ELit (LInt 1)])))
  assertRuntimeErrorContains "runtime fallback filter predicate bool result" "E3019" result

testPrintBuiltinReturnsArgument :: IO ()
testPrintBuiltinReturnsArgument = do
  result <- runSource defaultWarningSettings "print! 1."
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "1") (runOutput result)

testStructuralListEqualityRuntimeSuccess :: IO ()
testStructuralListEqualityRuntimeSuccess = do
  result <- runSource defaultWarningSettings "same = [1, 2] == [1, 2].\ndifferent = [1, 2] != [1, 3].\nshorter = [1] == [1, 2].\nnested = [[True], [False]] == [[True], [False]].\n[same, different, shorter, nested]."
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "[True, True, False, True]") (runOutput result)

testStructuralTupleEqualityRuntimeSuccess :: IO ()
testStructuralTupleEqualityRuntimeSuccess = do
  result <- runSource defaultWarningSettings "same = (1, True) == (1, True).\ndifferent = (1, (True, 2)) != (1, (True, 3)).\n[same, different]."
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "[True, True]") (runOutput result)

testStructuralAdtEqualityRuntimeSuccess :: IO ()
testStructuralAdtEqualityRuntimeSuccess = do
  result <- runSource defaultWarningSettings "data Maybe a = Nothing | Just a.\nsame = Just 1 == Just 1.\ndifferentPayload = Just 1 != Just 2.\ndifferentCtor = Just 1 == Nothing.\nnested = Just [True] == Just [True].\n[same, differentPayload, differentCtor, nested]."
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "[True, True, False, True]") (runOutput result)

testStructuralAdtEqualitySeesThroughRuntimeTypeHints :: IO ()
testStructuralAdtEqualitySeesThroughRuntimeTypeHints = do
  let result =
        evaluateRuntimeExprWithBuiltinsAndBindingHints
          ResolveKernelOnly
          ( Map.fromList
              [ (bindingRuntimeHintKey "left" (SourceSpan 2 1), ConstraintTypeApplication "Tag" [ConstraintTypeName "UInt8"]),
                (bindingRuntimeHintKey "right" (SourceSpan 3 1), ConstraintTypeApplication "Tag" [ConstraintTypeName "UInt8"])
              ]
          )
          ( EBlock
              [ SData (SourceSpan 1 1) "Tag" ["a"] [DataConstructor "Tag" []],
                SLet "left" (SourceSpan 2 1) (EVar "Tag"),
                SLet "right" (SourceSpan 3 1) (EVar "Tag"),
                SExpr (SourceSpan 4 1) (EBinary "==" (EVar "left") (EVar "right"))
              ]
          )
  assertEqual "typed ADT structural equality runtime result" (Right (Just (VBool True))) result

testStructuralAdtEqualityPreservesIncompatibleRuntimeTypeHints :: IO ()
testStructuralAdtEqualityPreservesIncompatibleRuntimeTypeHints = do
  let result =
        evaluateRuntimeExprWithBuiltinsAndBindingHints
          ResolveKernelOnly
          ( Map.fromList
              [ (bindingRuntimeHintKey "left" (SourceSpan 2 1), ConstraintTypeApplication "Tag" [ConstraintTypeName "UInt8"]),
                (bindingRuntimeHintKey "right" (SourceSpan 3 1), ConstraintTypeApplication "Tag" [ConstraintTypeName "UInt16"])
              ]
          )
          ( EBlock
              [ SData (SourceSpan 1 1) "Tag" ["a"] [DataConstructor "Tag" []],
                SLet "left" (SourceSpan 2 1) (EVar "Tag"),
                SLet "right" (SourceSpan 3 1) (EVar "Tag"),
                SExpr (SourceSpan 4 1) (EBinary "==" (EVar "left") (EVar "right"))
              ]
          )
  assertEqual "incompatible typed ADT structural equality runtime result" (Right (Just (VBool False))) result

testRuntimeFallbackRejectsDirectCallableEquality :: IO ()
testRuntimeFallbackRejectsDirectCallableEquality = do
  assertCallableRuntimeEqualityRejected
    "runtime closure equality"
    (EBinary "==" closureValue closureValue)
  assertCallableRuntimeEqualityRejected
    "runtime builtin equality"
    (EBinary "==" builtinValue builtinValue)
  assertCallableRuntimeEqualityRejected
    "runtime operator equality"
    (EBinary "==" operatorValue operatorValue)
  assertCallableRuntimeEqualityRejected
    "runtime left section equality"
    (EBinary "==" leftSectionValue leftSectionValue)

testRuntimeFallbackRejectsDirectCallableInequality :: IO ()
testRuntimeFallbackRejectsDirectCallableInequality = do
  assertCallableRuntimeEqualityRejected
    "runtime closure inequality"
    (EBinary "!=" closureValue closureValue)
  assertCallableRuntimeEqualityRejected
    "runtime right section inequality"
    (EBinary "!=" rightSectionValue rightSectionValue)

testRuntimeFallbackRejectsFunctionStructuralEquality :: IO ()
testRuntimeFallbackRejectsFunctionStructuralEquality = do
  let identity = ELambda "x" (EVar "x")
      result = evaluateRuntimeExpr (runtimeExpr (EBinary "==" (EList [identity]) (EList [identity])))
  assertRuntimeErrorContains "runtime fallback function structural equality" "E3007" result
  assertRuntimeErrorContains
    "runtime fallback function structural equality callable text"
    "callable values are not equality-supported"
    result

testRuntimeFallbackRejectsDifferentLengthFunctionStructuralEquality :: IO ()
testRuntimeFallbackRejectsDifferentLengthFunctionStructuralEquality = do
  let identity = ELambda "x" (EVar "x")
  assertCallableRuntimeEqualityRejected
    "different-length function structural equality"
    (EBinary "==" (EList [identity]) (EList [identity, identity]))

testRuntimeFallbackRejectsDifferentSaturatedAdtConstructors :: IO ()
testRuntimeFallbackRejectsDifferentSaturatedAdtConstructors = do
  let result = evaluateRuntimeExpr differentSaturatedAdtConstructorEqualityExpr
  assertRuntimeErrorContains "different saturated ADT constructor equality code" "E3007" result
  assertRuntimeErrorContains
    "different saturated ADT constructor equality callable text"
    "callable values are not equality-supported"
    result
  where
    identity = ELambda "x" (EVar "x")

    differentSaturatedAdtConstructorEqualityExpr =
      EBlock
        [ SData
            (SourceSpan 1 1)
            "Maybe"
            []
            [ DataConstructor "Nothing" [],
              DataConstructor "Just" [DataConstructorArgumentName "value"]
            ],
          SExpr
            (SourceSpan 2 1)
            (EBinary "==" (EApply (EVar "Just") identity) (EVar "Nothing"))
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
