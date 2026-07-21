{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Text as Text
import JazzNext.Compiler.Bootstrap.JazzCoreParity
  ( expectedSignaturesDeclarationsOperatorsBatchRendering,
    runJazzControlFlowPatternsBatch,
    runJazzSignaturesDeclarationsOperatorsBatch,
  )
import JazzNext.Compiler.Diagnostics
  ( SourceSpan (..),
  )
import JazzNext.Compiler.Driver
  ( RunResult (..),
    runCompileErrors,
    runRuntimeErrors,
  )
import JazzNext.Compiler.Name
  ( Identifier,
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
main = runTestSuite "JazzCoreSignaturesDeclarationsOperators" tests

tests :: [NamedTest]
tests =
  [ ("matches stage 0 for signatures and operator expressions", testDirectParity),
    ("preserves earlier profiles and module deferral", testProfileBoundaries)
  ]

testDirectParity :: IO ()
testDirectParity = do
  assertEqual "direct fixture names" expectedDirectFixtureNames (map fst directFixtures)
  expected <- expectRight "direct expected values" (expectedSignaturesDeclarationsOperatorsBatchRendering directExpressions)
  assertContains
    "explicit type application span"
    "CoreTypeApplicationExpression(CoreVariableExpression(CoreSourceName(\"identity\")), CoreSpan(Nothing, 2, 3), CoreIntType)"
    expected
  assertContains
    "dollar becomes application"
    "CoreApplyExpression(CoreVariableExpression(CoreSourceName(\"function\")), CoreLiteralExpression(CoreIntegerLiteral(\"1\")))"
    expected
  assertContains
    "operator binding keeps exact storage name"
    "CoreGeneratedName(CoreOperatorBinding(\"$operator:%2B%2B\"))"
    expected
  assertContains
    "operator signature keeps exact storage name"
    "CoreGeneratedName(CoreOperatorBinding(\"$operator:%25%25\"))"
    expected
  assertContains
    "qualified signature name"
    "CoreQualifiedName(\"Alias\", \"Result\")"
    expected
  assertContains
    "multiply qualified signature name remains one source name"
    "CoreSourceName(\"Alias::Nested::Result\")"
    expected
  assertContains
    "unsupported signature token inventory"
    "CoreSignatureOtherToken(\"forall\")"
    expected
  first <- runJazzSignaturesDeclarationsOperatorsBatch directExpressions
  second <- runJazzSignaturesDeclarationsOperatorsBatch directExpressions
  assertSuccessfulOutput "direct parity first run" expected first
  assertSuccessfulOutput "direct parity second run" expected second
  assertEqual "direct parity deterministic output" (runOutput first) (runOutput second)

testProfileBoundaries :: IO ()
testProfileBoundaries = do
  earlier <- runJazzControlFlowPatternsBatch earlierChildExpressions
  assertSuccessfulOutput
    "control-flow wrapper child-3 rejection"
    (nothingListRendering (length earlierChildExpressions))
    earlier
  child3 <- runJazzSignaturesDeclarationsOperatorsBatch initialDeferredExpressions
  assertSuccessfulOutput
    "child-3 module/import rejection"
    (nothingListRendering (length initialDeferredExpressions))
    child3

expectedDirectFixtureNames :: [Text.Text]
expectedDirectFixtureNames =
  [ "type-application-primitive",
    "type-application-recursive-qualified",
    "dollar-basic",
    "dollar-nested-control-flow",
    "ordinary-binding",
    "operator-binding",
    "signature-primitives",
    "signature-numeric-widths",
    "signature-recursive-shapes",
    "signature-qualified-names",
    "signature-constraints",
    "unsupported-signature-token-inventory",
    "operator-signature"
  ]

directExpressions :: [SurfaceExpr]
directExpressions = map snd directFixtures

directFixtures :: [(Text.Text, SurfaceExpr)]
directFixtures =
  [ ( "type-application-primitive",
      SETypeApplication (SEVar "identity") span2 SurfaceTypeInt
    ),
    ( "type-application-recursive-qualified",
      SETypeApplication
        (SETypeApplication
          (SEQualifiedVar "Alias" "map")
          span1
          (SurfaceTypeApplication "Alias::Maybe" [SurfaceTypeVariable "a"]))
        span2
        ( SurfaceTypeFunction
            (SurfaceTypeList (SurfaceTypeNumeric SurfaceNumericUInt16))
            (SurfaceTypeTuple [SurfaceTypeBool, SurfaceTypeText])
        )
    ),
    ("dollar-basic", SEBinary "$" (SEVar "function") (seInt 1)),
    ( "dollar-nested-control-flow",
      SEBinary
        "$"
        (SEVar "choose")
        (SEIf (SEVar "condition") (seInt 1) (SEBinary "$" (SEVar "fallback") (seInt 0)))
    ),
    ( "ordinary-binding",
      SEBlock
        [ SSLet "value" span1 (seInt 1),
          SSExpr span2 (SEVar "value")
        ]
    ),
    ( "operator-binding",
      SEBlock
        [ SSLet "$operator:%2B%2B" span1 (SEVar "combine"),
          SSExpr span2 (SEVar "combine")
        ]
    ),
    ( "signature-primitives",
      signatureBlock
        [ ("integer", SurfaceTypeInt),
          ("floating", SurfaceTypeFloat),
          ("boolean", SurfaceTypeBool),
          ("character", SurfaceTypeChar),
          ("text", SurfaceTypeText)
        ]
    ),
    ( "signature-numeric-widths",
      signatureBlock
        [ ("i8", SurfaceTypeNumeric SurfaceNumericInt8),
          ("i16", SurfaceTypeNumeric SurfaceNumericInt16),
          ("i32", SurfaceTypeNumeric SurfaceNumericInt32),
          ("i64", SurfaceTypeNumeric SurfaceNumericInt64),
          ("u8", SurfaceTypeNumeric SurfaceNumericUInt8),
          ("u16", SurfaceTypeNumeric SurfaceNumericUInt16),
          ("u32", SurfaceTypeNumeric SurfaceNumericUInt32),
          ("u64", SurfaceTypeNumeric SurfaceNumericUInt64),
          ("f16", SurfaceTypeNumeric SurfaceNumericFloat16),
          ("f32", SurfaceTypeNumeric SurfaceNumericFloat32),
          ("f64", SurfaceTypeNumeric SurfaceNumericFloat64)
        ]
    ),
    ( "signature-recursive-shapes",
      signatureBlock
        [ ("variable", SurfaceTypeVariable "a"),
          ("named", SurfaceTypeName "Result"),
          ("applied", SurfaceTypeApplication "Result" [SurfaceTypeVariable "a", SurfaceTypeText]),
          ("list", SurfaceTypeList (SurfaceTypeVariable "a")),
          ("unit", SurfaceTypeTuple []),
          ("tuple", SurfaceTypeTuple [SurfaceTypeInt, SurfaceTypeBool]),
          ("function", SurfaceTypeFunction (SurfaceTypeList SurfaceTypeInt) (SurfaceTypeTuple [SurfaceTypeText, SurfaceTypeBool]))
        ]
    ),
    ( "signature-qualified-names",
      signatureBlock
        [ ("qualified", SurfaceTypeName "Alias::Result"),
          ("qualifiedApplied", SurfaceTypeApplication "Alias::Box" [SurfaceTypeName "Other::Item"]),
          ("multiQualified", SurfaceTypeName "Alias::Nested::Result"),
          ("missingQualifier", SurfaceTypeName "::Result"),
          ("missingMember", SurfaceTypeName "Alias::")
        ]
    ),
    ( "signature-constraints",
      SEBlock
        [ SSSignature
            "constrained"
            span1
            ( SurfaceConstrainedSignature
                [ SurfaceSignatureConstraint "Eq" [SurfaceTypeVariable "a"],
                  SurfaceSignatureConstraint "Alias::Ord" [SurfaceTypeList (SurfaceTypeVariable "a")]
                ]
                (SurfaceTypeFunction (SurfaceTypeVariable "a") (SurfaceTypeList (SurfaceTypeVariable "a")))
            )
        ]
    ),
    ( "unsupported-signature-token-inventory",
      SEBlock
        [ SSSignature
            "unsupported"
            span1
            ( SurfaceUnsupportedSignature
                [ SurfaceSignatureNameToken "a",
                  SurfaceSignatureIntToken 12,
                  SurfaceSignatureArrowToken,
                  SurfaceSignatureAtToken,
                  SurfaceSignatureColonToken,
                  SurfaceSignatureLParenToken,
                  SurfaceSignatureRParenToken,
                  SurfaceSignatureLBraceToken,
                  SurfaceSignatureRBraceToken,
                  SurfaceSignatureLBracketToken,
                  SurfaceSignatureRBracketToken,
                  SurfaceSignatureCommaToken,
                  SurfaceSignatureOperatorToken "+",
                  SurfaceSignatureOtherToken "forall"
                ]
            )
        ]
    ),
    ( "operator-signature",
      SEBlock
        [ SSSignature
            "$operator:%25%25"
            span1
            (SurfaceSignatureType (SurfaceTypeFunction SurfaceTypeInt (SurfaceTypeFunction SurfaceTypeInt SurfaceTypeInt)))
        ]
    )
  ]

signatureBlock :: [(Identifier, SurfaceSignatureType)] -> SurfaceExpr
signatureBlock signatures =
  SEBlock
    [ SSSignature name span1 (SurfaceSignatureType signatureType)
      | (name, signatureType) <- signatures
    ]

earlierChildExpressions :: [SurfaceExpr]
earlierChildExpressions =
  [ SETypeApplication (SEVar "identity") span1 SurfaceTypeInt,
    SEBinary "$" (SEVar "function") (seInt 1),
    SEBlock [SSSignature "value" span1 (SurfaceSignatureType SurfaceTypeInt)],
    SEBlock [SSLet "$operator:%2B%2B" span1 (SEVar "combine")]
  ]

initialDeferredExpressions :: [SurfaceExpr]
initialDeferredExpressions =
  [ SEBlock [SSModule span1 ["App", "Main"] Nothing],
    SEBlock [SSImport span1 ["Core", "Text"] Nothing Nothing]
  ]

span1 :: SourceSpan
span1 = SourceSpan 1 1

span2 :: SourceSpan
span2 = SourceSpan 2 3

seInt :: Integer -> SurfaceExpr
seInt = SELit . SLInt

nothingListRendering :: Int -> Text.Text
nothingListRendering count = "[" <> Text.intercalate ", " (replicate count "Nothing") <> "]"

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
