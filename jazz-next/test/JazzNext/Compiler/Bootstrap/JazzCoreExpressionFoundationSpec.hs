{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text as Text
import JazzNext.Compiler.Bootstrap.JazzCoreParity
  ( expectedFoundationBatchRendering,
    expectedFoundationSourceBatchRendering,
    expectedParserSourceBatchRendering,
    runJazzFoundationBatch,
    runJazzFoundationSourceBatch,
    runJazzParserSourceBatch,
  )
import JazzNext.Compiler.Diagnostics
  ( SourceSpan (..),
  )
import JazzNext.Compiler.Driver
  ( RunResult (..),
    runCompileErrors,
    runRuntimeErrors,
  )
import JazzNext.Compiler.FractionalLiteral
  ( mkFractionalLiteralSource,
  )
import JazzNext.Compiler.Parser.AST
import JazzNext.TestHarness
  ( NamedTest,
    assertContains,
    assertEqual,
    failTest,
    runTestSuite,
  )

main :: IO ()
main = runTestSuite "JazzCoreExpressionFoundation" tests

tests :: [NamedTest]
tests =
  [ ("matches stage 0 for every foundational expression form", testFoundationParity),
    ("rejects every deferred or recursively unsupported form", testUnsupportedBoundary),
    ("matches stage 0 through the hosted parser twice", testComposedParity),
    ("keeps parser failures and deferred lowering distinct", testComposedBoundaries)
  ]

testFoundationParity :: IO ()
testFoundationParity = do
  expected <- expectRight "foundation expected values" (expectedFoundationBatchRendering foundationExpressions)
  assertContains "arbitrary integer" "1234567890123456789012345678901234567890" expected
  assertContains "exact fractional source" "CoreFractionalLiteral(\"1\", \"050\", Just(CoreFloat32Type))" expected
  assertContains "unqualified binding span" "CoreSpan(Nothing, 3, 5)" expected
  first <- runJazzFoundationBatch foundationExpressions
  second <- runJazzFoundationBatch foundationExpressions
  assertSuccessfulOutput "foundation parity first run" expected first
  assertSuccessfulOutput "foundation parity second run" expected second
  assertEqual "foundation parity deterministic output" (runOutput first) (runOutput second)

testUnsupportedBoundary :: IO ()
testUnsupportedBoundary = do
  first <- runJazzFoundationBatch unsupportedExpressions
  second <- runJazzFoundationBatch unsupportedExpressions
  let expected =
        Just
          ( "["
              <> Text.intercalate ", " (replicate (length unsupportedExpressions) "Nothing")
              <> "]"
          )
  assertEqual "unsupported first compile errors" [] (runCompileErrors first)
  assertEqual "unsupported first runtime errors" [] (runRuntimeErrors first)
  assertEqual "unsupported first results" expected (runOutput first)
  assertEqual "unsupported second compile errors" [] (runCompileErrors second)
  assertEqual "unsupported second runtime errors" [] (runRuntimeErrors second)
  assertEqual "unsupported second results" expected (runOutput second)
  assertEqual "unsupported deterministic output" (runOutput first) (runOutput second)

testComposedParity :: IO ()
testComposedParity = do
  expected <- expectRight "composed expected values" (expectedFoundationSourceBatchRendering composedSources)
  first <- runJazzFoundationSourceBatch composedSources
  second <- runJazzFoundationSourceBatch composedSources
  assertSuccessfulOutput "composed parity first run" expected first
  assertSuccessfulOutput "composed parity second run" expected second
  assertEqual "composed deterministic output" (runOutput first) (runOutput second)

testComposedBoundaries :: IO ()
testComposedBoundaries = do
  expectedParserFailure <-
    expectRight
      "parser-owned failure expectation"
      (expectedParserSourceBatchRendering [parserRejectedSource])
  firstParserFailure <- runJazzParserSourceBatch [parserRejectedSource]
  secondParserFailure <- runJazzParserSourceBatch [parserRejectedSource]
  assertContains "parser-owned failure form" "CanonicalSourceParserFailure" expectedParserFailure
  assertSuccessfulOutput "parser-owned failure first run" expectedParserFailure firstParserFailure
  assertSuccessfulOutput "parser-owned failure second run" expectedParserFailure secondParserFailure
  assertEqual "parser failure deterministic output" (runOutput firstParserFailure) (runOutput secondParserFailure)

  expectedDeferredParse <-
    expectRight
      "deferred parser success expectation"
      (expectedParserSourceBatchRendering [deferredSource])
  deferredParse <- runJazzParserSourceBatch [deferredSource]
  assertContains "deferred source parsed" "CanonicalSourceSuccess" expectedDeferredParse
  assertSuccessfulOutput "deferred parser ownership" expectedDeferredParse deferredParse

  firstDeferred <- runJazzFoundationSourceBatch [deferredSource]
  secondDeferred <- runJazzFoundationSourceBatch [deferredSource]
  assertSuccessfulOutput "deferred lowering first run" "[Nothing]" firstDeferred
  assertSuccessfulOutput "deferred lowering second run" "[Nothing]" secondDeferred
  assertEqual "deferred lowering deterministic output" (runOutput firstDeferred) (runOutput secondDeferred)

foundationExpressions :: [SurfaceExpr]
foundationExpressions =
  [ SELit (SLInt 1234567890123456789012345678901234567890),
    SELit (SLFloat 1.05 (mkFractionalLiteralSource 1 50 3) Nothing),
    SELit (SLFloat 1.05 (mkFractionalLiteralSource 1 50 3) (Just SurfaceNumericFloat16)),
    SELit (SLFloat 1.05 (mkFractionalLiteralSource 1 50 3) (Just SurfaceNumericFloat32)),
    SELit (SLFloat 1.05 (mkFractionalLiteralSource 1 50 3) (Just SurfaceNumericFloat64)),
    SELit (SLBool True),
    SELit (SLChar 'x'),
    SELit (SLText "Jazz"),
    SEVar "value",
    SEQualifiedVar "Text" "length",
    SEOperatorValue "+",
    SEList [],
    SEList [seInt 1, SETuple [SEVar "value", SELit (SLBool False)]],
    SETuple [],
    SETuple [seInt 1, SELit (SLText "two")],
    SEApply (SEApply (SEVar "f") (seInt 1)) (SELit (SLBool True)),
    SEBinary "+" (seInt 1) (SEBinary "*" (seInt 2) (seInt 3)),
    SESectionLeft (seInt 1) "+",
    SESectionRight "+" (seInt 2),
    SEBlock
      [ SSLet "answer" (SourceSpan 3 5) (seInt 42),
        SSExpr
          (SourceSpan 4 3)
          (SEApply (SEQualifiedVar "Text" "length") (SEList [SELit (SLText "Jazz")]))
      ],
    SEBlock
      [ SSLet "nested" (SourceSpan 7 2) (SEBlock [SSExpr (SourceSpan 8 4) (SETuple [])]),
        SSExpr (SourceSpan 9 2) (SEVar "nested")
      ]
  ]

unsupportedExpressions :: [SurfaceExpr]
unsupportedExpressions =
  [ SELambda (SurfaceLambdaIdentifier "value" :| []) (SEVar "value"),
    SECase (SEVar "value") [],
    SEIf (SELit (SLBool True)) (seInt 1) (seInt 0),
    SETypeApplication (SEVar "identity") span1 SurfaceTypeInt,
    SEBinary "$" (SEVar "f") (seInt 1),
    SEBlock [SSSignature "value" span1 (SurfaceSignatureType SurfaceTypeInt)],
    SEBlock [SSData span1 "Thing" [] []],
    SEBlock [SSClass span1 "Show" ["a"] []],
    SEBlock [SSImpl span1 "Show" [SurfaceTypeText] []],
    SEBlock [SSModule span1 ["App", "Main"] Nothing],
    SEBlock [SSImport span1 ["Core", "Text"] Nothing Nothing],
    SEBlock [SSLet "$operator:2B" span1 (SEVar "add")],
    SEList [seInt 1, SEIf (SELit (SLBool True)) (seInt 2) (seInt 3)],
    SEApply (SEVar "f") (SELambda (SurfaceLambdaIdentifier "x" :| []) (SEVar "x")),
    SEBlock [SSLet "value" span1 (SECase (SEVar "value") [])]
  ]

composedSources :: [Text.Text]
composedSources =
  [ "answer = 42. answer.",
    "values = [(1, True), (), [\"Jazz\"]]. values.",
    "(Text::length) [\"Jazz\"].",
    "(+).",
    "1 + 2 * 3.",
    "(1 +).",
    "(+ 2).",
    "[].",
    "().",
    "1.050f32."
  ]

parserRejectedSource :: Text.Text
parserRejectedSource = "answer = ."

deferredSource :: Text.Text
deferredSource = "\\(subject) -> subject."

span1 :: SourceSpan
span1 = SourceSpan 1 1

seInt :: Integer -> SurfaceExpr
seInt = SELit . SLInt

assertSuccessfulOutput :: Text.Text -> Text.Text -> RunResult -> IO ()
assertSuccessfulOutput label expected result = do
  assertEqual (label <> " compile errors") [] (runCompileErrors result)
  assertEqual (label <> " runtime errors") [] (runRuntimeErrors result)
  assertEqual (label <> " output") (Just expected) (runOutput result)

expectRight :: Show err => Text.Text -> Either err value -> IO value
expectRight label value =
  case value of
    Left err -> failTest (label <> ": expected Right, got Left " <> Text.pack (show err))
    Right ok -> pure ok
