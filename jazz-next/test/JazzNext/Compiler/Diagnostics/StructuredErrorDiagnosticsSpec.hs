{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import JazzNext.Compiler.Diagnostics
  ( SourceSpan (..),
    mkDiagnostic,
    renderDiagnostic,
    setDiagnosticPrimarySpan,
    setDiagnosticRelatedSpan,
    setDiagnosticSubject
  )
import JazzNext.TestHarness
  ( NamedTest,
    assertContains,
    runTestSuite
  )

main :: IO ()
main = runTestSuite "StructuredErrorDiagnostics" tests

tests :: [NamedTest]
tests =
  [ ("rendered diagnostics include code, primary span, and related span", testRenderDiagnosticWithPrimaryAndRelatedSpans),
    ("rendered diagnostics include source-qualified spans", testRenderDiagnosticWithSourceQualifiedSpans)
  ]

testRenderDiagnosticWithPrimaryAndRelatedSpans :: IO ()
testRenderDiagnosticWithPrimaryAndRelatedSpans = do
  let rendered =
        renderDiagnostic $
          setDiagnosticSubject
            "x"
            ( setDiagnosticRelatedSpan
                (SourceSpan 1 1)
                ( setDiagnosticPrimarySpan
                    (SourceSpan 2 1)
                    (mkDiagnostic "E1010" "binding 'x' cannot call impure callee 'print!'")
                )
            )
  assertContains "rendered error code" "E1010" rendered
  assertContains "rendered primary span" "2:1" rendered
  assertContains "rendered related span" "1:1" rendered

testRenderDiagnosticWithSourceQualifiedSpans :: IO ()
testRenderDiagnosticWithSourceQualifiedSpans = do
  let rendered =
        renderDiagnostic $
          setDiagnosticRelatedSpan
            (SourceSpanIn "src/Lib/Bad.jz" 2 1)
            ( setDiagnosticPrimarySpan
                (SourceSpanIn "src/Lib/Bad.jz" 1 1)
                (mkDiagnostic "E2005" "binding 'x' declared as Int but inferred as Bool")
            )
  assertContains "source-qualified primary span" "src/Lib/Bad.jz:1:1" rendered
  assertContains "source-qualified related span" "related src/Lib/Bad.jz:2:1" rendered
