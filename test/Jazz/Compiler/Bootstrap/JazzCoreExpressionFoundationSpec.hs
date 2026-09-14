{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text as Text
import Jazz.Compiler.Bootstrap.JazzCoreParity
  ( expectedFoundationBatchRendering,
    expectedFoundationSourceBatchRendering,
    expectedParserSourceBatchRendering,
    runJazzFoundationBatch,
    runJazzFoundationSourceBatch,
    runJazzParserSourceBatch,
  )
import Jazz.Compiler.Diagnostics
  ( SourceSpan (..),
  )
import Jazz.Compiler.Driver
  ( RunResult,
    runCompileErrors,
    runOutput,
    runRuntimeErrors,
  )
import Jazz.Compiler.FractionalLiteral
  ( mkFractionalLiteralSource,
  )
import Jazz.Compiler.Name (Identifier)
import Jazz.Compiler.Parser.AST
import Jazz.Compiler.TypeRepresentation
  ( NumericType (..),
    SignaturePayload (..),
    SignatureType (..),
  )
import Jazz.TestHarness
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
  [ seLit (LInt 1234567890123456789012345678901234567890),
    seLit (LFloat 1.05 (mkFractionalLiteralSource 1 50 3) Nothing),
    seLit (LFloat 1.05 (mkFractionalLiteralSource 1 50 3) (Just NumericFloat16)),
    seLit (LFloat 1.05 (mkFractionalLiteralSource 1 50 3) (Just NumericFloat32)),
    seLit (LFloat 1.05 (mkFractionalLiteralSource 1 50 3) (Just NumericFloat64)),
    seLit (LBool True),
    seLit (LChar 'x'),
    seLit (LText "Jazz"),
    seVar "value",
    seQualifiedVar "Text" "length",
    seOperatorValue "+",
    seList [],
    seList [seInt 1, seTuple [seVar "value", seLit (LBool False)]],
    seTuple [],
    seTuple [seInt 1, seLit (LText "two")],
    seApply (seApply (seVar "f") (seInt 1)) (seLit (LBool True)),
    seBinary "+" (seInt 1) (seBinary "*" (seInt 2) (seInt 3)),
    seSectionLeft (seInt 1) "+",
    seSectionRight "+" (seInt 2),
    seBlock
      [ SSLet "answer" (SourceSpan 3 5) (seInt 42),
        SSExpr
          (SourceSpan 4 3)
          (seApply (seQualifiedVar "Text" "length") (seList [seLit (LText "Jazz")]))
      ],
    seBlock
      [ SSLet "nested" (SourceSpan 7 2) (seBlock [SSExpr (SourceSpan 8 4) (seTuple [])]),
        SSExpr (SourceSpan 9 2) (seVar "nested")
      ]
  ]

unsupportedExpressions :: [SurfaceExpr]
unsupportedExpressions =
  [ seLambda (SurfaceLambdaIdentifier span1 "value" :| []) (seVar "value"),
    seCase (seVar "value") [],
    seIf (seLit (LBool True)) (seInt 1) (seInt 0),
    seTypeApplication (seVar "identity") span1 TypeInt,
    seBinary "$" (seVar "f") (seInt 1),
    seBlock [SSSignature "value" span1 (SignatureType TypeInt)],
    seBlock [SSData span1 "Thing" [] []],
    seBlock [SSClass span1 "Show" ["a"] [] [] []],
    seBlock [SSImpl span1 (SurfaceName "Show" span1 Nothing) [TypeText] [] []],
    seBlock [SSModule span1 ["App", "Main"] Nothing],
    seBlock [SSImport span1 ["Core", "Text"] Nothing Nothing],
    seBlock [SSLet "$operator:2B" span1 (seVar "add")],
    seList [seInt 1, seIf (seLit (LBool True)) (seInt 2) (seInt 3)],
    seApply (seVar "f") (seLambda (SurfaceLambdaIdentifier span1 "x" :| []) (seVar "x")),
    seBlock [SSLet "value" span1 (seCase (seVar "value") [])]
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

se :: SurfaceExprForm -> SurfaceExpr
se = SurfaceExpr span1

seApply :: SurfaceExpr -> SurfaceExpr -> SurfaceExpr
seApply function argument = se (SEApply function argument)

seBinary :: Text.Text -> SurfaceExpr -> SurfaceExpr -> SurfaceExpr
seBinary operator left right = se (SEBinary operator left right)

seBlock :: [SurfaceStatement] -> SurfaceExpr
seBlock = se . SEBlock

seCase :: SurfaceExpr -> [SurfaceCaseArm] -> SurfaceExpr
seCase scrutinee arms = se (SECase scrutinee arms)

seIf :: SurfaceExpr -> SurfaceExpr -> SurfaceExpr -> SurfaceExpr
seIf condition thenBranch elseBranch = se (SEIf condition thenBranch elseBranch)

seLambda :: NonEmpty SurfaceLambdaParameter -> SurfaceExpr -> SurfaceExpr
seLambda parameters body = se (SELambda parameters body)

seList :: [SurfaceExpr] -> SurfaceExpr
seList = se . SEList

seLit :: Literal -> SurfaceExpr
seLit = se . SELit

seOperatorValue :: Text.Text -> SurfaceExpr
seOperatorValue = se . SEOperatorValue

seQualifiedVar :: Identifier -> Identifier -> SurfaceExpr
seQualifiedVar qualifier member = se (SEQualifiedVar qualifier member)

seSectionLeft :: SurfaceExpr -> Text.Text -> SurfaceExpr
seSectionLeft left operator = se (SESectionLeft left operator)

seSectionRight :: Text.Text -> SurfaceExpr -> SurfaceExpr
seSectionRight operator right = se (SESectionRight operator right)

seTuple :: [SurfaceExpr] -> SurfaceExpr
seTuple = se . SETuple

seTypeApplication :: SurfaceExpr -> SourceSpan -> SurfaceSignatureType -> SurfaceExpr
seTypeApplication function typeApplicationSpan signatureType =
  se (SETypeApplication function typeApplicationSpan signatureType)

seVar :: Identifier -> SurfaceExpr
seVar = se . SEVar

seInt :: Integer -> SurfaceExpr
seInt = seLit . LInt

assertSuccessfulOutput :: Text.Text -> Text.Text -> RunResult -> IO ()
assertSuccessfulOutput label expected result = do
  assertEqual (label <> " compile errors") [] (runCompileErrors result)
  assertEqual (label <> " runtime errors") [] (runRuntimeErrors result)
  assertEqual (label <> " output") (Just expected) (runOutput result)

expectRight :: (Show err) => Text.Text -> Either err value -> IO value
expectRight label value =
  case value of
    Left err -> failTest (label <> ": expected Right, got Left " <> Text.pack (show err))
    Right ok -> pure ok
