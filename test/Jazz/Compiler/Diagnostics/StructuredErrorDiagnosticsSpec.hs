{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Maybe (isJust)
import Jazz.Compiler.DiagnosticCatalog
  ( DiagnosticSeverity (..),
    ErrorCode (..),
    WarningCategory (..),
    diagnosticCodeText,
  )
import Jazz.Compiler.Diagnostics
  ( Diagnostic,
    DiagnosticContext (..),
    DiagnosticOrigin (..),
    SourceSpan (..),
    appendDiagnosticContext,
    appendDiagnosticNote,
    appendDiagnosticSecondaryLabel,
    diagnosticCode,
    diagnosticContexts,
    diagnosticHelp,
    diagnosticNotes,
    diagnosticOrigin,
    diagnosticPrimaryLabel,
    diagnosticPrimarySpan,
    diagnosticSecondaryLabels,
    diagnosticSeverity,
    diagnosticSubject,
    diagnosticSummary,
    diagnosticTypeError,
    diagnosticWarningCategory,
    labelMessage,
    labelSpan,
    mkErrorDiagnostic,
    mkWarningDiagnostic,
    prependDiagnosticSummary,
    promoteDiagnostic,
    qualifyDiagnosticSpans,
    setDiagnosticHelp,
    setDiagnosticPrimaryLabel,
    setDiagnosticSubject,
  )
import Jazz.Compiler.Diagnostics.Render
  ( renderDiagnostic,
  )
import Jazz.Compiler.Name (NameNamespace (ValueNamespace), mkIdentifier, resolvedLocalName)
import Jazz.Compiler.TypeInference.DiagnosticCause (TypeErrorCause (..))
import Jazz.Compiler.TypeInference.Diagnostics (addTypeError, annotateNewErrorsWithContext, mkSignatureTypeMismatchError)
import qualified Jazz.Compiler.TypeInference.Diagnostics as Inference
import Jazz.Compiler.TypeInference.State (inferErrorsRev, initialInferState)
import Jazz.Compiler.TypeInference.Types (NumericConstraint (AnyNumericConstraint))
import Jazz.Compiler.TypeRepresentation (SemanticType (..))
import Jazz.TestHarness
  ( NamedTest,
    assertEqual,
    runTestSuite,
  )

main :: IO ()
main = runTestSuite "StructuredErrorDiagnostics" tests

tests :: [NamedTest]
tests =
  [ ("all semantic error reports retain causes and normalize allocation offsets", testSemanticErrorReports),
    ("type errors name variables independently of allocation offsets", testStableTypeVariableNames),
    ("qualified type errors preserve exact primary and secondary ranges", testDiagnosticRanges),
    ("typed errors retain their cause when prefixed", testTypedErrorCause),
    ("checking context belongs only to newly emitted errors", testCheckingContext),
    ("context renders once from inner to outer", testContextRendering),
    ("native errors carry typed code, origin, severity, and labeled detail", testStructuredNativeError),
    ("warning promotion preserves warning identity and detail", testWarningPromotionPreservesDiagnostic),
    ("renderer owns native error severity, labels, notes, and help", testRenderNativeError),
    ("renderer preserves warning identity through promotion", testRenderWarningAndPromotion),
    ("rendered diagnostics include source-qualified spans", testRenderDiagnosticWithSourceQualifiedSpans)
  ]

testStructuredNativeError :: IO ()
testStructuredNativeError = do
  let diagnostic =
        setDiagnosticHelp "rename one of the bindings" $
          appendDiagnosticNote "the last declaration wins" $
            appendDiagnosticSecondaryLabel (SourceSpan 1 1) "previous binding" $
              setDiagnosticPrimaryLabel (SourceSpan 2 1) "rebound here" $
                setDiagnosticSubject "x" $
                  mkErrorDiagnostic E1010 CompilationOrigin "binding 'x' cannot call impure callee 'print!'"
  assertEqual "native error severity" SeverityError (diagnosticSeverity diagnostic)
  assertEqual "native error code" "E1010" (diagnosticCodeText (diagnosticCode diagnostic))
  assertEqual "native error origin" CompilationOrigin (diagnosticOrigin diagnostic)
  assertEqual "native error warning category" Nothing (diagnosticWarningCategory diagnostic)
  assertEqual "native error primary label" (Just (SourceSpan 2 1, "rebound here")) (labelPair <$> diagnosticPrimaryLabel diagnostic)
  assertEqual "native error secondary labels" [(SourceSpan 1 1, "previous binding")] (map labelPair (diagnosticSecondaryLabels diagnostic))
  assertEqual "native error help" (Just "rename one of the bindings") (diagnosticHelp diagnostic)
  where
    labelPair label = (labelSpan label, labelMessage label)

testWarningPromotionPreservesDiagnostic :: IO ()
testWarningPromotionPreservesDiagnostic = do
  let warning =
        setDiagnosticHelp "rename one of the bindings" $
          appendDiagnosticNote "the last declaration wins" $
            appendDiagnosticSecondaryLabel (SourceSpan 1 1) "previous binding" $
              setDiagnosticPrimaryLabel (SourceSpan 2 1) "rebound here" $
                setDiagnosticSubject "x" $
                  mkWarningDiagnostic SameScopeRebinding CompilationOrigin "same-scope rebinding"
      promoted = promoteDiagnostic warning
  assertEqual "promoted severity" SeverityError (diagnosticSeverity promoted)
  assertEqual "promoted code" (diagnosticCode warning) (diagnosticCode promoted)
  assertEqual "promoted category" (diagnosticWarningCategory warning) (diagnosticWarningCategory promoted)
  assertEqual "promoted primary label" (diagnosticPrimaryLabel warning) (diagnosticPrimaryLabel promoted)
  assertEqual "promoted secondary labels" (diagnosticSecondaryLabels warning) (diagnosticSecondaryLabels promoted)
  assertEqual "promoted subject" (diagnosticSubject warning) (diagnosticSubject promoted)
  assertEqual "promoted summary" (diagnosticSummary warning) (diagnosticSummary promoted)
  assertEqual "promoted notes" (diagnosticNotes warning) (diagnosticNotes promoted)
  assertEqual "promoted help" (diagnosticHelp warning) (diagnosticHelp promoted)

testRenderNativeError :: IO ()
testRenderNativeError =
  assertEqual
    "native error line"
    "error: E1010 2:1: binding 'x' cannot call impure callee 'print!' (rebound here; previous binding 1:1; note: the last declaration wins; help: rename one of the bindings)"
    (renderDiagnostic detailedNativeError)

testRenderWarningAndPromotion :: IO ()
testRenderWarningAndPromotion = do
  let warning =
        setDiagnosticHelp "rename one of the bindings" $
          appendDiagnosticNote "the last declaration wins" $
            appendDiagnosticSecondaryLabel (SourceSpan 1 1) "previous binding" $
              setDiagnosticPrimaryLabel (SourceSpan 2 1) "rebound here" $
                mkWarningDiagnostic SameScopeRebinding CompilationOrigin "same-scope rebinding"
  assertEqual
    "warning line"
    "warning: W0001 [same-scope-rebinding] 2:1: same-scope rebinding (rebound here; previous binding 1:1; note: the last declaration wins; help: rename one of the bindings)"
    (renderDiagnostic warning)
  assertEqual
    "promoted warning line"
    "error: W0001 [same-scope-rebinding] 2:1: same-scope rebinding (rebound here; previous binding 1:1; note: the last declaration wins; help: rename one of the bindings)"
    (renderDiagnostic (promoteDiagnostic warning))

testRenderDiagnosticWithSourceQualifiedSpans :: IO ()
testRenderDiagnosticWithSourceQualifiedSpans = do
  let rendered =
        renderDiagnostic $
          appendDiagnosticSecondaryLabel
            (SourceSpanIn "src/Lib/Bad.jz" 2 1)
            "related"
            ( setDiagnosticPrimaryLabel
                (SourceSpanIn "src/Lib/Bad.jz" 1 1)
                "primary"
                (mkErrorDiagnostic E2005 CompilationOrigin "binding 'x' declared as Int but inferred as Bool")
            )
  assertEqual
    "source-qualified line"
    "error: E2005 src/Lib/Bad.jz:1:1: binding 'x' declared as Int but inferred as Bool (primary; related src/Lib/Bad.jz:2:1)"
    rendered

detailedNativeError :: Diagnostic
detailedNativeError =
  setDiagnosticHelp "rename one of the bindings" $
    appendDiagnosticNote "the last declaration wins" $
      appendDiagnosticSecondaryLabel (SourceSpan 1 1) "previous binding" $
        setDiagnosticPrimaryLabel (SourceSpan 2 1) "rebound here" $
          setDiagnosticSubject "x" $
            mkErrorDiagnostic E1010 CompilationOrigin "binding 'x' cannot call impure callee 'print!'"

-- Allocating unrelated variables before this binding must not change its error.
testStableTypeVariableNames :: IO ()
testStableTypeVariableNames = do
  let report offset =
        mkSignatureTypeMismatchError
          "f"
          (SourceSpan 1 1)
          (SemanticFunction (SemanticVariable offset) (SemanticVariable offset))
          (SourceSpan 2 1)
          (SemanticFunction (SemanticVariable (offset + 1)) SemanticBool)
  assertEqual "allocation-independent report" (diagnosticSummary (report 7)) (diagnosticSummary (report 107))
  assertEqual
    "shared names distinguish repeated and distinct variables"
    "binding 'f' declared as t0 -> t0 but inferred as t1 -> Bool"
    (diagnosticSummary (report 7))

testTypedErrorCause :: IO ()
testTypedErrorCause = do
  let cause = SignatureTypeMismatch "f" (SemanticVariable 7) SemanticBool
      report =
        prependDiagnosticSummary "module: " $
          mkSignatureTypeMismatchError "f" (SourceSpan 1 1) (SemanticVariable 7) (SourceSpan 2 1) SemanticBool
  assertEqual "typed cause survives a reporting wrapper" (Just cause) (diagnosticTypeError report)
  assertEqual "prefix preserves stable rendering" "module: binding 'f' declared as t0 but inferred as Bool" (diagnosticSummary report)

testCheckingContext :: IO ()
testCheckingContext = do
  let earlier = mkErrorDiagnostic E2005 CompilationOrigin "earlier error"
      specific = setDiagnosticPrimaryLabel (SourceSpan 4 9) "argument" $ mkErrorDiagnostic E2006 CompilationOrigin "wrong argument"
      unlocated = mkErrorDiagnostic E2005 CompilationOrigin "mismatch"
      checkpoint = addTypeError initialInferState earlier
      emitted = addTypeError (addTypeError checkpoint specific) unlocated
      annotated = annotateNewErrorsWithContext (CheckingBinding "f") (SourceSpan 3 1) checkpoint emitted
  assertEqual
    "only new diagnostics receive the binding context"
    [[CheckingBinding "f"], [CheckingBinding "f"], []]
    (map diagnosticContexts (inferErrorsRev annotated))
  assertEqual
    "specific locations survive and unlocated errors receive the enclosing span"
    [Just (SourceSpan 3 1), Just (SourceSpan 4 9), Nothing]
    (map diagnosticPrimarySpan (inferErrorsRev annotated))

testContextRendering :: IO ()
testContextRendering = do
  let report =
        appendDiagnosticContext (CheckingBinding "f") $
          appendDiagnosticContext (CheckingImplMethod "Eq::equal") $
            appendDiagnosticContext (CheckingImplMethod "Eq::equal") $
              appendDiagnosticContext (SatisfyingConstraint "Eq") $
                mkErrorDiagnostic E2005 CompilationOrigin "mismatch"
  assertEqual
    "inner context precedes its owner without duplicate hints"
    "error: E2005: mismatch (while satisfying constraint 'Eq'; while checking impl method 'Eq::equal'; while checking binding 'f')"
    (renderDiagnostic report)

-- Exercise report constructors, so adding normalization to a renderer without
-- retaining the semantic cause cannot satisfy this contract.
testSemanticErrorReports :: IO ()
testSemanticErrorReports = do
  mapM_ check cases
  assertEqual
    "method argument reports share names across their entire argument list"
    "no matching qualified method body 'Eq::equal' for argument types (t0, t1, t0), t1"
    (diagnosticSummary (Inference.mkNoMatchingQualifiedMethodBodyError "Eq::equal" [pair 7, variable 8]))
  assertEqual
    "pattern and scrutinee share report names in presentation order"
    "case pattern of type (t0, t1, t0) does not match scrutinee type t1"
    (diagnosticSummary (Inference.mkPatternTypeMismatchError (variable 8) (pair 7)))
  assertEqual
    "constraint report preserves code, source, and primitive wording"
    "error: E2009 src/Lib/Check.jz:3:4: signature for 'f' does not declare required primitive constraint 'Numeric((t0, t1, t0))'"
    (renderDiagnostic (Inference.mkUndeclaredSignatureConstraintError "f" True "Numeric" (pair 7) spanValue))
  assertEqual
    "non-report helper retains raw solver identities"
    "t107"
    (Inference.renderType (variable 107))
  where
    check (label, report) = do
      let earlier = report 7
          later = report 107
      assertEqual (label <> " retains semantic cause") True (isJust (diagnosticTypeError earlier))
      assertEqual (label <> " ignores allocation offsets") (renderDiagnostic earlier) (renderDiagnostic later)
    variable = SemanticVariable
    pair offset = SemanticTuple [variable offset, variable (offset + 1), variable offset]
    name = resolvedLocalName ValueNamespace (mkIdentifier "item")
    spanValue = SourceRangeIn "src/Lib/Check.jz" 3 4 3 12
    cases =
      [ ("binary", \n -> Inference.mkBinaryTypeError "+" (pair n) (variable (n + 1))),
        ("strict equality", \n -> Inference.mkStrictEqualityTypeError "==" (pair n) (variable (n + 1))),
        ("unsupported equality", \n -> Inference.mkStrictEqualityUnsupportedTypeError "==" (SemanticFunction (variable n) (variable n))),
        ("numeric section", \n -> Inference.mkNumericSectionOperandTypeError "+" (pair n)),
        ("numeric constraint", \n -> Inference.mkTypeSchemeNumericConstraintError AnyNumericConstraint (pair n)),
        ("equality constraint", \n -> Inference.mkTypeSchemeStrictEqualityConstraintError (pair n)),
        ("missing method match", \n -> Inference.mkNoMatchingQualifiedMethodBodyError "Eq::equal" [pair n, variable (n + 1)]),
        ("ambiguous method match", \n -> Inference.mkAmbiguousQualifiedMethodBodyForArgumentsError "Eq::equal" [pair n, variable (n + 1)]),
        ("undeclared class constraint", \n -> Inference.mkUndeclaredSignatureConstraintError "f" False "Eq" (pair n) spanValue),
        ("undeclared primitive constraint", \n -> Inference.mkUndeclaredSignatureConstraintError "f" True "Numeric" (pair n) spanValue),
        ("ambiguous inferred constraint", \n -> Inference.mkAmbiguousDeferredConstraintError True "Eq" (pair n)),
        ("ambiguous explicit constraint", \n -> Inference.mkAmbiguousDeferredConstraintError False "Eq" (pair n)),
        ("pattern mismatch", \n -> Inference.mkPatternTypeMismatchError (variable (n + 1)) (pair n)),
        ("list pattern mismatch", \n -> Inference.mkListPatternTypeMismatchError (pair n)),
        ("tuple pattern mismatch", \n -> Inference.mkTuplePatternTypeMismatchError (pair n)),
        ("case arms", \n -> Inference.mkPatternBranchTypeMismatchError (pair n) (variable (n + 1))),
        ("if condition", \n -> Inference.mkIfConditionTypeError (pair n)),
        ("case guard", \n -> Inference.mkCaseGuardTypeError (pair n)),
        ("or-pattern binder", \n -> Inference.mkOrPatternBinderTypeMismatchError name (pair n) (variable (n + 1)))
      ]

testDiagnosticRanges :: IO ()
testDiagnosticRanges = do
  let report =
        qualifyDiagnosticSpans "Main.jz" $
          appendDiagnosticContext (CheckingBinding "f") $
            mkSignatureTypeMismatchError "f" (SourceRange 1 1 1 8) SemanticInt (SourceRange 2 1 3 6) SemanticBool
  assertEqual
    "primary range survives qualification and context"
    (Just (SourceRangeIn "Main.jz" 1 1 1 8))
    (diagnosticPrimarySpan report)
  assertEqual
    "related range survives qualification and context"
    [SourceRangeIn "Main.jz" 2 1 3 6]
    (map labelSpan (diagnosticSecondaryLabels report))
  assertEqual
    "type payload survives qualification"
    (Just (SignatureTypeMismatch "f" SemanticInt SemanticBool))
    (diagnosticTypeError report)
