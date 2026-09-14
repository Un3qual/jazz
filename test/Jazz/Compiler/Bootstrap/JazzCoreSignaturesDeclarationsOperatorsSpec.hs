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
      se (SETypeApplication (seVar "identity") span2 TypeInt)
    ),
    ( "type-application-recursive-qualified",
      se
        ( SETypeApplication
            ( se
                ( SETypeApplication
                    (se (SEQualifiedVar "Alias" "map"))
                    span1
                    (TypeApplication (SurfaceName "Alias::Maybe" span1 (Just span1)) [TypeVariable "a"])
                )
            )
            span2
            ( TypeFunction
                (TypeList (TypeNumeric NumericUInt16))
                (TypeTuple [TypeBool, TypeText])
            )
        )
    ),
    ("dollar-basic", se (SEBinary "$" (seVar "function") (seInt 1))),
    ( "dollar-nested-control-flow",
      se
        ( SEBinary
            "$"
            (seVar "choose")
            (se (SEIf (seVar "condition") (seInt 1) (se (SEBinary "$" (seVar "fallback") (seInt 0)))))
        )
    ),
    ( "ordinary-binding",
      se
        ( SEBlock
            [ SSLet "item" span1 (seInt 1),
              SSExpr span2 (seVar "item")
            ]
        )
    ),
    ( "operator-binding",
      se
        ( SEBlock
            [ SSLet "$operator:%2B%2B" span1 (seVar "combine"),
              SSExpr span2 (seVar "combine")
            ]
        )
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
          ("named", TypeName (SurfaceName "Result" span1 Nothing)),
          ("applied", TypeApplication (SurfaceName "Result" span1 Nothing) [TypeVariable "a", TypeText]),
          ("list", TypeList (TypeVariable "a")),
          ("unit", TypeTuple []),
          ("tuple", TypeTuple [TypeInt, TypeBool]),
          ("function", TypeFunction (TypeList TypeInt) (TypeTuple [TypeText, TypeBool]))
        ]
    ),
    ( "signature-qualified-names",
      signatureBlock
        [ ("qualified", TypeName (SurfaceName "Alias::Result" span1 (Just span1))),
          ("qualifiedApplied", TypeApplication (SurfaceName "Alias::Box" span1 (Just span1)) [TypeName (SurfaceName "Other::Item" span1 (Just span1))]),
          ("multiQualified", TypeName (SurfaceName "Alias::Nested::Result" span1 (Just span1))),
          ("missingQualifier", TypeName (SurfaceName "::Result" span1 (Just span1))),
          ("missingMember", TypeName (SurfaceName "Alias::" span1 (Just span1)))
        ]
    ),
    ( "signature-constraints",
      se
        ( SEBlock
            [ SSSignature
                "constrained"
                span1
                ( ConstrainedSignature
                    [ SignatureConstraint (SurfaceName "Eq" span1 Nothing) [TypeVariable "a"],
                      SignatureConstraint (SurfaceName "Alias::Ord" span1 (Just span1)) [TypeList (TypeVariable "a")]
                    ]
                    (TypeFunction (TypeVariable "a") (TypeList (TypeVariable "a")))
                )
            ]
        )
    ),
    ( "unsupported-signature-token-inventory",
      se
        ( SEBlock
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
        )
    ),
    ( "operator-signature",
      se
        ( SEBlock
            [ SSSignature
                "$operator:%25%25"
                span1
                (SignatureType (TypeFunction TypeInt (TypeFunction TypeInt TypeInt)))
            ]
        )
    ),
    ( "data-empty",
      se (SEBlock [SSData span1 "Empty" [] []])
    ),
    ( "data-constructors",
      se
        ( SEBlock
            [ SSData
                span1
                "Result"
                ["error", "item"]
                [ SurfaceDataConstructor "Failure" [TypeVariable "error"],
                  SurfaceDataConstructor "Success" [TypeVariable "item"],
                  SurfaceDataConstructor "Opaque" [TypeInt]
                ]
            ]
        )
    ),
    ( "class-empty",
      se (SEBlock [SSClass span1 "Marker" ["a"] [] [] []])
    ),
    ( "class-methods",
      se
        ( SEBlock
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
                        [SignatureConstraint (SurfaceName "Alias::Ord" span1 (Just span1)) [TypeVariable "a"]]
                        (TypeFunction (TypeVariable "a") TypeInt)
                    )
                ]
                []
                []
            ]
        )
    ),
    ( "impl-empty",
      se (SEBlock [SSImpl span1 (SurfaceName "Show" span1 Nothing) [TypeText] [] []])
    ),
    ( "impl-methods",
      se
        ( SEBlock
            [ SSImpl
                span1
                (SurfaceName "Transform" span1 Nothing)
                [TypeApplication (SurfaceName "Alias::Box" span1 (Just span1)) [TypeInt]]
                [ SurfaceImplMethod
                    "apply"
                    span2
                    ( se
                        ( SEIf
                            (seVar "condition")
                            (se (SETypeApplication (seVar "identity") span2 TypeText))
                            (se (SEBinary "$" (seVar "fallback") (seVar "item")))
                        )
                    )
                ]
                []
            ]
        )
    ),
    ( "mixed-block",
      se
        ( SEBlock
            [ SSSignature "convert" span1 (SignatureType (TypeFunction TypeInt TypeText)),
              SSData span1 "Wrapped" ["a"] [SurfaceDataConstructor "Wrapped" [TypeVariable "a"]],
              SSClass span1 "Render" ["a"] [SurfaceClassMethodSignature "render" span2 (SignatureType (TypeFunction (TypeVariable "a") TypeText))] [] [],
              SSImpl span1 (SurfaceName "Render" span1 Nothing) [TypeInt] [SurfaceImplMethod "render" span2 (se (SEBinary "$" (seVar "toText") (seVar "item")))] [],
              SSLet "convert" span2 (se (SETypeApplication (seVar "identity") span2 TypeText)),
              SSExpr span2 (seVar "convert")
            ]
        )
    )
  ]

signatureBlock :: [(Identifier, SurfaceSignatureType)] -> SurfaceExpr
signatureBlock signatures =
  se
    ( SEBlock
        [ SSSignature name span1 (SignatureType signatureType)
        | (name, signatureType) <- signatures
        ]
    )

earlierChildExpressions :: [SurfaceExpr]
earlierChildExpressions =
  [ se (SETypeApplication (seVar "identity") span1 TypeInt),
    se (SEBinary "$" (seVar "function") (seInt 1)),
    se (SEBlock [SSSignature "item" span1 (SignatureType TypeInt)]),
    se (SEBlock [SSLet "$operator:%2B%2B" span1 (seVar "combine")])
  ]

initialDeferredExpressions :: [SurfaceExpr]
initialDeferredExpressions =
  [ se (SEBlock [SSModule span1 ["App", "Main"] Nothing]),
    se (SEBlock [SSImport span1 ["Core", "Text"] Nothing Nothing])
  ]

expectedComposedFixtureNames :: [Text.Text]
expectedComposedFixtureNames =
  [ "explicit-type-primitive",
    "explicit-type-applied-chain",
    "dollar-right-associated",
    "signature-primitives",
    "signature-recursive-shapes",
    "signature-qualified",
    "qualified-method",
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
    ("qualified-method", "result = Alias::Class::method."),
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
  [ ("module-root", se (SEBlock [SSModule span1 ["App", "Main"] Nothing])),
    ("import-root", se (SEBlock [SSImport span1 ["Core", "Text"] Nothing Nothing])),
    ( "module-in-if-branch",
      se
        ( SEIf
            (seVar "condition")
            (se (SEBlock [SSModule span1 ["App", "Main"] Nothing, SSExpr span2 (seInt 1)]))
            (seInt 0)
        )
    ),
    ( "import-in-case-body",
      se
        ( SECase
            (seVar "item")
            [ SurfaceCaseArm
                (sp SPWildcard)
                Nothing
                (se (SEBlock [SSImport span1 ["Core", "Text"] Nothing Nothing, SSExpr span2 (seInt 0)]))
            ]
        )
    ),
    ( "module-in-lambda-body",
      se
        ( SELambda
            (SurfaceLambdaIdentifier span1 "item" :| [])
            (se (SEBlock [SSModule span1 ["App", "Main"] Nothing, SSExpr span2 (seVar "item")]))
        )
    ),
    ( "import-in-let-item",
      se
        ( SEBlock
            [ SSLet
                "item"
                span1
                (se (SEBlock [SSImport span1 ["Core", "Text"] Nothing Nothing, SSExpr span2 (seInt 1)]))
            ]
        )
    ),
    ( "module-in-impl-method",
      se
        ( SEBlock
            [ SSImpl
                span1
                (SurfaceName "Render" span1 Nothing)
                [TypeInt]
                [ SurfaceImplMethod
                    "render"
                    span2
                    (se (SEBlock [SSModule span1 ["App", "Main"] Nothing, SSExpr span2 (seInt 1)]))
                ]
                []
            ]
        )
    ),
    ( "import-in-operator-binding",
      se
        ( SEBlock
            [ SSLet
                "$operator:%25%25"
                span1
                (se (SEBlock [SSImport span1 ["Core", "Text"] Nothing Nothing, SSExpr span2 (seInt 1)]))
            ]
        )
    )
  ]

span1 :: SourceSpan
span1 = SourceSpan 1 1

span2 :: SourceSpan
span2 = SourceSpan 2 3

seInt :: Integer -> SurfaceExpr
seInt = se . SELit . LInt

seVar :: Identifier -> SurfaceExpr
seVar = se . SEVar

se :: SurfaceExprForm -> SurfaceExpr
se = SurfaceExpr span1

sp :: SurfacePatternForm -> SurfacePattern
sp = SurfacePattern span1

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
