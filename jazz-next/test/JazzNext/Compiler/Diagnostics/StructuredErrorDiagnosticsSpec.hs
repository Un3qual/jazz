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
  [ ("rendered diagnostics include code, primary span, and related span", testRenderDiagnosticWithPrimaryAndRelatedSpans)
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
