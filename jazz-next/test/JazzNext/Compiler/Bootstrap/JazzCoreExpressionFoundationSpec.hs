{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text as Text
import JazzNext.Compiler.Bootstrap.JazzCoreParity
  ( expectedFoundationBatchRendering,
    runJazzFoundationBatch,
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
    ("rejects every deferred or recursively unsupported form", testUnsupportedBoundary)
  ]

testFoundationParity :: IO ()
testFoundationParity = do
  expected <- expectRight "foundation expected values" (expectedFoundationBatchRendering foundationExpressions)
  assertContains "arbitrary integer" "1234567890123456789012345678901234567890" expected
  assertContains "exact fractional source" "CoreFractionalLiteral(\"1\", \"050\", Just(CoreFloat32Type))" expected
  assertContains "unqualified binding span" "CoreSpan(Nothing, 3, 5)" expected
  actual <- runJazzFoundationBatch foundationExpressions
  assertSuccessfulOutput "foundation parity" expected actual

testUnsupportedBoundary :: IO ()
testUnsupportedBoundary = do
  actual <- runJazzFoundationBatch unsupportedExpressions
  let expected =
        Just
          ( "["
              <> Text.intercalate ", " (replicate (length unsupportedExpressions) "Nothing")
              <> "]"
          )
  assertEqual "unsupported compile errors" [] (runCompileErrors actual)
  assertEqual "unsupported runtime errors" [] (runRuntimeErrors actual)
  assertEqual "unsupported results" expected (runOutput actual)

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
