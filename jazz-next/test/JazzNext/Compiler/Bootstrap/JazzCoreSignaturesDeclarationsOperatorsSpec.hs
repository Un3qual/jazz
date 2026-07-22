{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text as Text
import JazzNext.Compiler.Bootstrap.JazzCoreParity
  ( expectedSignaturesDeclarationsOperatorsBatchRendering,
    expectedSignaturesDeclarationsOperatorsSourceBatchRendering,
    runJazzControlFlowPatternsBatch,
    runJazzSignaturesDeclarationsOperatorsBatch,
    runJazzSignaturesDeclarationsOperatorsSourceBatch,
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
    ("preserves earlier profiles and module deferral", testProfileBoundaries),
    ("matches stage 0 through the hosted parser twice", testComposedParity),
    ("rejects modules and imports at every nested boundary", testDeferredBoundary)
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
  assertContains
    "opaque data constructor argument"
    "CoreDataConstructor(CoreSourceName(\"Opaque\"), [CoreOpaqueConstructorArgument])"
    expected
  assertContains
    "class method signature payload"
    "CoreClassMethodSignature(CoreSourceName(\"equals\"), CoreSpan(Nothing, 2, 3), CoreTypeSignature"
    expected
  assertContains
    "impl method recursively lowered body"
    "CoreImplMethod(CoreSourceName(\"apply\"), CoreSpan(Nothing, 2, 3), CoreIfExpression"
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

testComposedParity :: IO ()
testComposedParity = do
  assertEqual "composed fixture names" expectedComposedFixtureNames (map fst composedFixtures)
  expected <-
    expectRight
      "composed expected values"
      (expectedSignaturesDeclarationsOperatorsSourceBatchRendering composedSources)
  first <- runJazzSignaturesDeclarationsOperatorsSourceBatch composedSources
  second <- runJazzSignaturesDeclarationsOperatorsSourceBatch composedSources
  assertSuccessfulOutput "composed parity first run" expected first
  assertSuccessfulOutput "composed parity second run" expected second
  assertEqual "composed parity deterministic output" (runOutput first) (runOutput second)

testDeferredBoundary :: IO ()
testDeferredBoundary = do
  assertEqual "deferred fixture names" expectedDeferredFixtureNames (map fst deferredFixtures)
  first <- runJazzSignaturesDeclarationsOperatorsBatch deferredExpressions
  second <- runJazzSignaturesDeclarationsOperatorsBatch deferredExpressions
  let expected = nothingListRendering (length deferredExpressions)
  assertSuccessfulOutput "deferred boundary first run" expected first
  assertSuccessfulOutput "deferred boundary second run" expected second
  assertEqual "deferred boundary deterministic output" (runOutput first) (runOutput second)

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
    "operator-signature",
    "data-empty",
    "data-constructors",
    "class-empty",
    "class-methods",
    "impl-empty",
    "impl-methods",
    "mixed-block"
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
    ),
    ( "data-empty",
      SEBlock [SSData span1 "Empty" [] []]
    ),
    ( "data-constructors",
      SEBlock
        [ SSData
            span1
            "Result"
            ["error", "value"]
            [ SurfaceDataConstructor "Failure" [SurfaceDataConstructorArgumentName "error"],
              SurfaceDataConstructor "Success" [SurfaceDataConstructorArgumentName "value"],
              SurfaceDataConstructor "Opaque" [SurfaceDataConstructorArgumentOpaque]
            ]
        ]
    ),
    ( "class-empty",
      SEBlock [SSClass span1 "Marker" ["a"] []]
    ),
    ( "class-methods",
      SEBlock
        [ SSClass
            span1
            "Eq"
            ["a"]
            [ SurfaceClassMethodSignature
                "equals"
                span2
                ( SurfaceSignatureType
                    (SurfaceTypeFunction (SurfaceTypeVariable "a") (SurfaceTypeFunction (SurfaceTypeVariable "a") SurfaceTypeBool))
                ),
              SurfaceClassMethodSignature
                "compare"
                span1
                ( SurfaceConstrainedSignature
                    [SurfaceSignatureConstraint "Alias::Ord" [SurfaceTypeVariable "a"]]
                    (SurfaceTypeFunction (SurfaceTypeVariable "a") SurfaceTypeInt)
                )
            ]
        ]
    ),
    ( "impl-empty",
      SEBlock [SSImpl span1 "Show" [SurfaceTypeText] []]
    ),
    ( "impl-methods",
      SEBlock
        [ SSImpl
            span1
            "Transform"
            [SurfaceTypeApplication "Alias::Box" [SurfaceTypeInt]]
            [ SurfaceImplMethod
                "apply"
                span2
                ( SEIf
                    (SEVar "condition")
                    (SETypeApplication (SEVar "identity") span2 SurfaceTypeText)
                    (SEBinary "$" (SEVar "fallback") (SEVar "value"))
                )
            ]
        ]
    ),
    ( "mixed-block",
      SEBlock
        [ SSSignature "convert" span1 (SurfaceSignatureType (SurfaceTypeFunction SurfaceTypeInt SurfaceTypeText)),
          SSData span1 "Wrapped" ["a"] [SurfaceDataConstructor "Wrapped" [SurfaceDataConstructorArgumentName "a"]],
          SSClass span1 "Render" ["a"] [SurfaceClassMethodSignature "render" span2 (SurfaceSignatureType (SurfaceTypeFunction (SurfaceTypeVariable "a") SurfaceTypeText))],
          SSImpl span1 "Render" [SurfaceTypeInt] [SurfaceImplMethod "render" span2 (SEBinary "$" (SEVar "toText") (SEVar "value"))],
          SSLet "convert" span2 (SETypeApplication (SEVar "identity") span2 SurfaceTypeText),
          SSExpr span2 (SEVar "convert")
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

expectedComposedFixtureNames :: [Text.Text]
expectedComposedFixtureNames =
  [ "explicit-type-primitive",
    "explicit-type-applied-chain",
    "dollar-right-associated",
    "signature-primitives",
    "signature-recursive-shapes",
    "signature-qualified",
    "signature-constrained",
    "signature-unsupported-forall",
    "data-nullary",
    "data-parameterized",
    "class-empty",
    "class-method-signature",
    "impl-empty",
    "impl-method-body",
    "operator-signature-binding",
    "mixed-declarations-control-flow"
  ]

composedSources :: [Text.Text]
composedSources = map snd composedFixtures

composedFixtures :: [(Text.Text, Text.Text)]
composedFixtures =
  [ ("explicit-type-primitive", "value = id @Int 1. value."),
    ("explicit-type-applied-chain", "value = id @Maybe(Int) @List(Text) item. value."),
    ("dollar-right-associated", "value = f $ g $ item. value."),
    ("signature-primitives", "integer :: Int. floating :: Float. boolean :: Bool. character :: Char. text :: Text."),
    ( "signature-recursive-shapes",
      "variable :: a. named :: Result. maybe :: Maybe(Char). list :: [a]. tuple :: (Int, Bool). unit :: (). apply :: (Int -> Int) -> Text."
    ),
    ("signature-qualified", "qualified :: Alias::Result."),
    ( "signature-constrained",
      "constrained :: @{Eq(a), Ord(List(a))}: a -> List(a)."
    ),
    ("signature-unsupported-forall", "value :: forall a. value = 1."),
    ("data-nullary", "data Maybe = Nothing | Just."),
    ("data-parameterized", "data Maybe a = None | Some a | Pair (a, a) [a]."),
    ("class-empty", "class Marker(a) { }."),
    ( "class-method-signature",
      "class Eq(a) { equals :: a -> a -> Bool. notEquals :: a -> a -> Bool. }."
    ),
    ("impl-empty", "impl Eq(Int) { }."),
    ( "impl-method-body",
      "impl Eq(Int) { equals = \\(left, right) -> left == right. }."
    ),
    ( "operator-signature-binding",
      "operator %% tier 2. (%%) :: Int -> Int -> Int. (%%) = \\(left, right) -> left + right. value = 1 %% 2."
    ),
    ( "mixed-declarations-control-flow",
      "data Maybe a = Nothing | Just a. class Select(a) { select :: Bool -> a -> a -> a. }. impl Select(Int) { select = \\(condition, left, right) -> if condition then left else right. }. choose :: Int. choose = if True then id @Int 1 else 0. choose."
    )
  ]

expectedDeferredFixtureNames :: [Text.Text]
expectedDeferredFixtureNames =
  [ "module-root",
    "import-root",
    "module-in-if-branch",
    "import-in-case-body",
    "module-in-lambda-body",
    "import-in-let-value",
    "module-in-impl-method",
    "import-in-operator-binding"
  ]

deferredExpressions :: [SurfaceExpr]
deferredExpressions = map snd deferredFixtures

deferredFixtures :: [(Text.Text, SurfaceExpr)]
deferredFixtures =
  [ ("module-root", SEBlock [SSModule span1 ["App", "Main"] Nothing]),
    ("import-root", SEBlock [SSImport span1 ["Core", "Text"] Nothing Nothing]),
    ( "module-in-if-branch",
      SEIf
        (SEVar "condition")
        (SEBlock [SSModule span1 ["App", "Main"] Nothing, SSExpr span2 (seInt 1)])
        (seInt 0)
    ),
    ( "import-in-case-body",
      SECase
        (SEVar "value")
        [ SurfaceCaseArm
            SPWildcard
            Nothing
            (SEBlock [SSImport span1 ["Core", "Text"] Nothing Nothing, SSExpr span2 (seInt 0)])
        ]
    ),
    ( "module-in-lambda-body",
      SELambda
        (SurfaceLambdaIdentifier "value" :| [])
        (SEBlock [SSModule span1 ["App", "Main"] Nothing, SSExpr span2 (SEVar "value")])
    ),
    ( "import-in-let-value",
      SEBlock
        [ SSLet
            "value"
            span1
            (SEBlock [SSImport span1 ["Core", "Text"] Nothing Nothing, SSExpr span2 (seInt 1)])
        ]
    ),
    ( "module-in-impl-method",
      SEBlock
        [ SSImpl
            span1
            "Render"
            [SurfaceTypeInt]
            [ SurfaceImplMethod
                "render"
                span2
                (SEBlock [SSModule span1 ["App", "Main"] Nothing, SSExpr span2 (seInt 1)])
            ]
        ]
    ),
    ( "import-in-operator-binding",
      SEBlock
        [ SSLet
            "$operator:%25%25"
            span1
            (SEBlock [SSImport span1 ["Core", "Text"] Nothing Nothing, SSExpr span2 (seInt 1)])
        ]
    )
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
