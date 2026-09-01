{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text as Text
import Jazz.Compiler.Bootstrap.JazzCoreParity
  ( expectedSignaturesDeclarationsOperatorsBatchRendering,
    expectedSignaturesDeclarationsOperatorsSourceBatchRendering,
    runJazzControlFlowPatternsBatch,
    runJazzSignaturesDeclarationsOperatorsBatch,
    runJazzSignaturesDeclarationsOperatorsSourceBatch,
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
import Jazz.Compiler.Name
  ( Identifier,
  )
import Jazz.Compiler.Parser.AST
import Jazz.Compiler.TypeRepresentation
  ( NumericType (..),
    SignatureConstraint (..),
    SignaturePayload (..),
    SignatureToken (..),
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
    "concrete data constructor field"
    "CoreDataConstructor(CoreSourceName(\"Opaque\"), [CoreIntType])"
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
      SETypeApplication (SEVar "identity") span2 TypeInt
    ),
    ( "type-application-recursive-qualified",
      SETypeApplication
        ( SETypeApplication
            (SEQualifiedVar "Alias" "map")
            span1
            (TypeApplication "Alias::Maybe" [TypeVariable "a"])
        )
        span2
        ( TypeFunction
            (TypeList (TypeNumeric NumericUInt16))
            (TypeTuple [TypeBool, TypeText])
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
        [ SSLet "item" span1 (seInt 1),
          SSExpr span2 (SEVar "item")
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
        [ ("integer", TypeInt),
          ("floating", TypeFloat),
          ("boolean", TypeBool),
          ("character", TypeChar),
          ("text", TypeText)
        ]
    ),
    ( "signature-numeric-widths",
      signatureBlock
        [ ("i8", TypeNumeric NumericInt8),
          ("i16", TypeNumeric NumericInt16),
          ("i32", TypeNumeric NumericInt32),
          ("i64", TypeNumeric NumericInt64),
          ("u8", TypeNumeric NumericUInt8),
          ("u16", TypeNumeric NumericUInt16),
          ("u32", TypeNumeric NumericUInt32),
          ("u64", TypeNumeric NumericUInt64),
          ("f16", TypeNumeric NumericFloat16),
          ("f32", TypeNumeric NumericFloat32),
          ("f64", TypeNumeric NumericFloat64)
        ]
    ),
    ( "signature-recursive-shapes",
      signatureBlock
        [ ("variable", TypeVariable "a"),
          ("named", TypeName "Result"),
          ("applied", TypeApplication "Result" [TypeVariable "a", TypeText]),
          ("list", TypeList (TypeVariable "a")),
          ("unit", TypeTuple []),
          ("tuple", TypeTuple [TypeInt, TypeBool]),
          ("function", TypeFunction (TypeList TypeInt) (TypeTuple [TypeText, TypeBool]))
        ]
    ),
    ( "signature-qualified-names",
      signatureBlock
        [ ("qualified", TypeName "Alias::Result"),
          ("qualifiedApplied", TypeApplication "Alias::Box" [TypeName "Other::Item"]),
          ("multiQualified", TypeName "Alias::Nested::Result"),
          ("missingQualifier", TypeName "::Result"),
          ("missingMember", TypeName "Alias::")
        ]
    ),
    ( "signature-constraints",
      SEBlock
        [ SSSignature
            "constrained"
            span1
            ( ConstrainedSignature
                [ SignatureConstraint "Eq" [TypeVariable "a"],
                  SignatureConstraint "Alias::Ord" [TypeList (TypeVariable "a")]
                ]
                (TypeFunction (TypeVariable "a") (TypeList (TypeVariable "a")))
            )
        ]
    ),
    ( "unsupported-signature-token-inventory",
      SEBlock
        [ SSSignature
            "unsupported"
            span1
            ( UnsupportedSignature
                [ SignatureNameToken "a",
                  SignatureIntToken 12,
                  SignatureArrowToken,
                  SignatureAtToken,
                  SignatureColonToken,
                  SignatureLParenToken,
                  SignatureRParenToken,
                  SignatureLBraceToken,
                  SignatureRBraceToken,
                  SignatureLBracketToken,
                  SignatureRBracketToken,
                  SignatureCommaToken,
                  SignatureOperatorToken "+",
                  SignatureOtherToken "forall"
                ]
            )
        ]
    ),
    ( "operator-signature",
      SEBlock
        [ SSSignature
            "$operator:%25%25"
            span1
            (SignatureType (TypeFunction TypeInt (TypeFunction TypeInt TypeInt)))
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
            ["error", "item"]
            [ SurfaceDataConstructor "Failure" [TypeVariable "error"],
              SurfaceDataConstructor "Success" [TypeVariable "item"],
              SurfaceDataConstructor "Opaque" [TypeInt]
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
                ( SignatureType
                    (TypeFunction (TypeVariable "a") (TypeFunction (TypeVariable "a") TypeBool))
                ),
              SurfaceClassMethodSignature
                "compare"
                span1
                ( ConstrainedSignature
                    [SignatureConstraint "Alias::Ord" [TypeVariable "a"]]
                    (TypeFunction (TypeVariable "a") TypeInt)
                )
            ]
        ]
    ),
    ( "impl-empty",
      SEBlock [SSImpl span1 "Show" [TypeText] []]
    ),
    ( "impl-methods",
      SEBlock
        [ SSImpl
            span1
            "Transform"
            [TypeApplication "Alias::Box" [TypeInt]]
            [ SurfaceImplMethod
                "apply"
                span2
                ( SEIf
                    (SEVar "condition")
                    (SETypeApplication (SEVar "identity") span2 TypeText)
                    (SEBinary "$" (SEVar "fallback") (SEVar "item"))
                )
            ]
        ]
    ),
    ( "mixed-block",
      SEBlock
        [ SSSignature "convert" span1 (SignatureType (TypeFunction TypeInt TypeText)),
          SSData span1 "Wrapped" ["a"] [SurfaceDataConstructor "Wrapped" [TypeVariable "a"]],
          SSClass span1 "Render" ["a"] [SurfaceClassMethodSignature "render" span2 (SignatureType (TypeFunction (TypeVariable "a") TypeText))],
          SSImpl span1 "Render" [TypeInt] [SurfaceImplMethod "render" span2 (SEBinary "$" (SEVar "toText") (SEVar "item"))],
          SSLet "convert" span2 (SETypeApplication (SEVar "identity") span2 TypeText),
          SSExpr span2 (SEVar "convert")
        ]
    )
  ]

signatureBlock :: [(Identifier, SurfaceSignatureType)] -> SurfaceExpr
signatureBlock signatures =
  SEBlock
    [ SSSignature name span1 (SignatureType signatureType)
    | (name, signatureType) <- signatures
    ]

earlierChildExpressions :: [SurfaceExpr]
earlierChildExpressions =
  [ SETypeApplication (SEVar "identity") span1 TypeInt,
    SEBinary "$" (SEVar "function") (seInt 1),
    SEBlock [SSSignature "item" span1 (SignatureType TypeInt)],
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
  [ ("explicit-type-primitive", "item = id @Int 1. item."),
    ("explicit-type-applied-chain", "item = id @Maybe(Int) @List(Text) item. item."),
    ("dollar-right-associated", "item = f $ g $ item. item."),
    ("signature-primitives", "integer :: Int. floating :: Float. boolean :: Bool. character :: Char. text :: Text."),
    ( "signature-recursive-shapes",
      "variable :: a. named :: Result. maybe :: Maybe(Char). list :: [a]. tuple :: (Int, Bool). unit :: (). apply :: (Int -> Int) -> Text."
    ),
    ("signature-qualified", "qualified :: Alias::Result."),
    ( "signature-constrained",
      "constrained :: @{Eq(a), Ord(List(a))}: a -> List(a)."
    ),
    ("signature-unsupported-forall", "item :: forall a. item = 1."),
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
      "operator %% tier 2. (%%) :: Int -> Int -> Int. (%%) = \\(left, right) -> left + right. item = 1 %% 2."
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
    "import-in-let-item",
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
        (SEVar "item")
        [ SurfaceCaseArm
            SPWildcard
            Nothing
            (SEBlock [SSImport span1 ["Core", "Text"] Nothing Nothing, SSExpr span2 (seInt 0)])
        ]
    ),
    ( "module-in-lambda-body",
      SELambda
        (SurfaceLambdaIdentifier "item" :| [])
        (SEBlock [SSModule span1 ["App", "Main"] Nothing, SSExpr span2 (SEVar "item")])
    ),
    ( "import-in-let-item",
      SEBlock
        [ SSLet
            "item"
            span1
            (SEBlock [SSImport span1 ["Core", "Text"] Nothing Nothing, SSExpr span2 (seInt 1)])
        ]
    ),
    ( "module-in-impl-method",
      SEBlock
        [ SSImpl
            span1
            "Render"
            [TypeInt]
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

expectRight :: (Show err) => Text.Text -> Either err value -> IO value
expectRight label value =
  case value of
    Left err -> failTest (label <> ": expected Right, got Left " <> Text.pack (show err))
    Right ok -> pure ok
