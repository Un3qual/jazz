{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import JazzNext.Compiler.Diagnostics
  ( DiagnosticOrigin (..),
    SourceSpan (..),
    appendDiagnosticNote,
    appendDiagnosticSecondaryLabel,
    diagnosticCode,
    diagnosticHelp,
    diagnosticNotes,
    diagnosticOrigin,
    diagnosticPrimaryLabel,
    diagnosticSecondaryLabels,
    diagnosticSeverity,
    diagnosticSubject,
    diagnosticSummary,
    diagnosticWarningCategory,
    labelMessage,
    labelSpan,
    mkErrorDiagnostic,
    mkWarningDiagnostic,
    promoteDiagnostic,
    renderDiagnostic,
    setDiagnosticHelp,
    setDiagnosticPrimaryLabel,
    setDiagnosticSubject
  )
import JazzNext.Compiler.DiagnosticCatalog
  ( DiagnosticSeverity (..),
    ErrorCode (..),
    WarningCategory (..),
    diagnosticCodeText
  )
import JazzNext.TestHarness
  ( NamedTest,
    assertContains,
    assertEqual,
    runTestSuite
  )

main :: IO ()
main = runTestSuite "StructuredErrorDiagnostics" tests

tests :: [NamedTest]
tests =
  [ ("native errors carry typed code, origin, severity, and labeled detail", testStructuredNativeError),
    ("warning promotion preserves warning identity and detail", testWarningPromotionPreservesDiagnostic),
    ("rendered diagnostics include code, primary span, and related span", testRenderDiagnosticWithPrimaryAndRelatedSpans),
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

testRenderDiagnosticWithPrimaryAndRelatedSpans :: IO ()
testRenderDiagnosticWithPrimaryAndRelatedSpans = do
  let rendered =
        renderDiagnostic $
          setDiagnosticSubject
            "x"
            ( appendDiagnosticSecondaryLabel
                (SourceSpan 1 1)
                "related"
                ( setDiagnosticPrimaryLabel
                    (SourceSpan 2 1)
                    "primary"
                    (mkErrorDiagnostic E1010 CompilationOrigin "binding 'x' cannot call impure callee 'print!'")
                )
            )
  assertContains "rendered error code" "E1010" rendered
  assertContains "rendered primary span" "2:1" rendered
  assertContains "rendered related span" "1:1" rendered

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
  assertContains "source-qualified primary span" "src/Lib/Bad.jz:1:1" rendered
  assertContains "source-qualified related span" "related src/Lib/Bad.jz:2:1" rendered
