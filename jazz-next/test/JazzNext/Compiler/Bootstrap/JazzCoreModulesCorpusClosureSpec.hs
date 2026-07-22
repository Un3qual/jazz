{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text as Text
import JazzNext.Compiler.Bootstrap.JazzCoreParity
  ( expectedCanonicalExpressionBatchRendering,
    expectedCoreSourceBatchRendering,
    expectedModuleBatchRendering,
    runJazzCanonicalExpressionBatch,
    runJazzCoreSourceBatch,
    runJazzModuleBatch,
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
import JazzNext.Compiler.ModuleExports
  ( LocatedModuleExportName (..),
    ModuleExportSelector (..),
    ModuleTypeConstructorSelector (..),
  )
import JazzNext.Compiler.Name
  ( NameNamespace (..),
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
main = runTestSuite "JazzCoreModulesCorpusClosure" tests

tests :: [NamedTest]
tests =
  [ ("lowers module and import statements through the complete expression entry", testCompleteExpressionParity),
    ("preserves the child-3 module and import deferral boundary", testEarlierProfileBoundary),
    ("matches stage 0 for fixed module results twice", testDirectModuleParity),
    ("composes parsing and module lowering for the fixed source family twice", testComposedSourceParity)
  ]

testCompleteExpressionParity :: IO ()
testCompleteExpressionParity = do
  expected <-
    expectRight
      "complete expression expected values"
      (expectedCanonicalExpressionBatchRendering completeExpressions)
  first <- runJazzCanonicalExpressionBatch completeExpressions
  second <- runJazzCanonicalExpressionBatch completeExpressions
  assertSuccessfulOutput "complete expression first run" expected first
  assertSuccessfulOutput "complete expression second run" expected second
  assertEqual "complete expression deterministic output" (runOutput first) (runOutput second)

testEarlierProfileBoundary :: IO ()
testEarlierProfileBoundary = do
  first <- runJazzSignaturesDeclarationsOperatorsBatch completeExpressions
  second <- runJazzSignaturesDeclarationsOperatorsBatch completeExpressions
  let expected = "[Nothing, Nothing]"
  assertSuccessfulOutput "child-3 deferral first run" expected first
  assertSuccessfulOutput "child-3 deferral second run" expected second
  assertEqual "child-3 deferral deterministic output" (runOutput first) (runOutput second)

testDirectModuleParity :: IO ()
testDirectModuleParity = do
  assertEqual "direct module fixture names" expectedDirectModuleFixtureNames (map directFixtureName directModuleFixtures)
  expected <-
    expectRight
      "direct module expected values"
      (expectedModuleBatchRendering directModuleInputs)
  assertContains
    "omitted exports stay absent"
    "CoreModule(Just([\"App\", \"Main\"]), Nothing"
    expected
  assertContains
    "explicit empty exports stay present"
    "CoreDeclaredModuleExports(CoreSpan(Just(CanonicalSourcePath(\"fixtures/core/modules-corpus-closure.jz\")), 1, 1), [])"
    expected
  assertContains "import metadata" "CoreResolvedImport" expected
  assertContains
    "nested import remains executable and qualified"
    "CoreImportStatement(CoreSpan(Just(CanonicalSourcePath(\"fixtures/core/modules-corpus-closure.jz\")), 3, 5), [\"Nested\", \"Level\"]"
    expected
  assertContains "explicit type application span is qualified" "CoreTypeApplicationExpression" expected
  assertContains "path mismatch stays structured" "CoreModulePathMismatchFailure" expected
  assertContains "multiple declarations stay structured" "CoreMultipleModuleDeclarationsFailure" expected
  first <- runJazzModuleBatch directModuleInputs
  second <- runJazzModuleBatch directModuleInputs
  assertSuccessfulOutput "direct module first run" expected first
  assertSuccessfulOutput "direct module second run" expected second
  assertEqual "direct module deterministic output" (runOutput first) (runOutput second)

testComposedSourceParity :: IO ()
testComposedSourceParity = do
  assertEqual "composed source fixture names" expectedComposedSourceFixtureNames (map composedFixtureName composedSourceFixtures)
  expected <-
    expectRight
      "composed source expected values"
      (expectedCoreSourceBatchRendering composedSourceInputs)
  assertContains "facade module result" "CanonicalCoreSourceModuleResult" expected
  assertContains "facade path mismatch" "CoreModulePathMismatchFailure" expected
  assertContains "facade lexical failure" "CanonicalCoreSourceLexicalFailure" expected
  assertContains "facade parser failure" "CanonicalCoreSourceParserFailure" expected
  first <- runJazzCoreSourceBatch composedSourceInputs
  second <- runJazzCoreSourceBatch composedSourceInputs
  assertSuccessfulOutput "composed source first run" expected first
  assertSuccessfulOutput "composed source second run" expected second
  assertEqual "composed source deterministic output" (runOutput first) (runOutput second)

completeExpressions :: [SurfaceExpr]
completeExpressions =
  [ SEBlock
      [ SSModule span1 ["App", "Main"] (Just []),
        SSImport span2 ["Core", "Text"] (Just "Text") (Just ["length", "uncons"]),
        SSExpr span3 (SELit (SLInt 1))
      ],
    SEBlock
      [ SSLet
          "nested"
          span1
          ( SEBlock
              [ SSImport span2 ["Core", "List"] Nothing Nothing,
                SSExpr span3 (SEVar "value")
              ]
          ),
        SSExpr span3 (SEVar "nested")
      ]
  ]

data DirectModuleFixture = DirectModuleFixture
  { directFixtureName :: Text.Text,
    directFixtureSourcePath :: FilePath,
    directFixtureExpectedPath :: [Text.Text],
    directFixtureExpression :: SurfaceExpr
  }

data ComposedSourceFixture = ComposedSourceFixture
  { composedFixtureName :: Text.Text,
    composedFixtureSourcePath :: FilePath,
    composedFixtureExpectedPath :: [Text.Text],
    composedFixtureSource :: Text.Text
  }

expectedComposedSourceFixtureNames :: [Text.Text]
expectedComposedSourceFixtureNames =
  [ "module-free",
    "module-no-exports",
    "module-empty-exports",
    "module-named-exports",
    "module-type-exports",
    "import-plain",
    "import-alias",
    "import-symbols",
    "nested-import",
    "mixed-full-surface",
    "path-mismatch",
    "lexical-failure",
    "parser-failure"
  ]

composedSourceInputs :: [(FilePath, [Text.Text], Text.Text)]
composedSourceInputs =
  map
    (\fixture ->
       ( composedFixtureSourcePath fixture,
         composedFixtureExpectedPath fixture,
         composedFixtureSource fixture
       )
    )
    composedSourceFixtures

composedSourceFixtures :: [ComposedSourceFixture]
composedSourceFixtures =
  [ composedFixture "module-free" "1.",
    composedFixture "module-no-exports" "module App::Main { 1. }",
    composedFixture "module-empty-exports" "module App::Main () { 1. }",
    composedFixture "module-named-exports" "module App::Main (value answer, constructor Some, type Maybe, class Eq) { answer = 1. }",
    composedFixture "module-type-exports" "module App::Main (type Hidden, type Choice(..), type Pair(Pair, Unit)) { 1. }",
    composedFixture "import-plain" "module App::Main { import Core::List. 1. }",
    composedFixture "import-alias" "module App::Main { import Core::Text as Text. Text::length. }",
    composedFixture "import-symbols" "module App::Main { import Core::Text (length, uncons). length. }",
    composedFixture
      "nested-import"
      "module App::Main { import Lib::Math as Math. result = { Math::answer. }. }",
    composedFixture "mixed-full-surface" mixedFullSurfaceSource,
    ComposedSourceFixture
      "path-mismatch"
      composedSourcePath
      moduleExpectedPath
      "module Wrong::Path { 1. }",
    composedFixture "lexical-failure" ";",
    composedFixture "parser-failure" "if True then 1."
  ]

composedFixture :: Text.Text -> Text.Text -> ComposedSourceFixture
composedFixture name source =
  ComposedSourceFixture name composedSourcePath moduleExpectedPath source

mixedFullSurfaceSource :: Text.Text
mixedFullSurfaceSource =
  """
  module App::Main (value main, type Maybe(..), class Eq) {
    import Core::List as List.
    operator %% tier 2.
    main :: Int.
    data Maybe a = None | Some a.
    class Eq(a) { equals :: a -> a -> Bool. }.
    impl Eq(Int) { equals = \\(left, right) -> left == right. }.
    main = case Some 1 {
      | Some value if True -> if False then 0 else value %% 2
      | _ -> 0
    }.
  }
  """

composedSourcePath :: FilePath
composedSourcePath = "fixtures/core/source-facade.jz"

expectedDirectModuleFixtureNames :: [Text.Text]
expectedDirectModuleFixtureNames =
  [ "non-block-no-metadata",
    "block-no-declaration",
    "module-exports-omitted",
    "module-exports-empty",
    "named-export-namespaces",
    "type-export-abstract",
    "type-export-all-constructors",
    "type-export-selected-constructors",
    "import-plain",
    "import-alias",
    "import-symbols",
    "imports-source-order",
    "nested-import-preserved",
    "complete-span-qualification",
    "path-mismatch",
    "multiple-declarations-two",
    "multiple-declarations-three"
  ]

directModuleInputs :: [(FilePath, [Text.Text], SurfaceExpr)]
directModuleInputs =
  map
    (\fixture ->
       ( directFixtureSourcePath fixture,
         directFixtureExpectedPath fixture,
         directFixtureExpression fixture
       )
    )
    directModuleFixtures

directModuleFixtures :: [DirectModuleFixture]
directModuleFixtures =
  [ directFixture "non-block-no-metadata" (SELit (SLInt 1)),
    directFixture "block-no-declaration" (SEBlock [SSExpr span1 (SELit (SLInt 1))]),
    directFixture "module-exports-omitted" (moduleBlock Nothing),
    directFixture "module-exports-empty" (moduleBlock (Just [])),
    directFixture
      "named-export-namespaces"
      ( moduleBlock
          ( Just
              [ ModuleExportSelector Nothing "legacy",
                ModuleExportSelector (Just ValueNamespace) "value",
                ModuleExportSelector (Just ConstructorNamespace) "Some",
                ModuleExportSelector (Just TypeNamespace) "Maybe",
                ModuleExportSelector (Just CapabilityNamespace) "Eq"
              ]
          )
      ),
    directFixture
      "type-export-abstract"
      (moduleBlock (Just [ModuleTypeExportSelector "Opaque" span2 AbstractType])),
    directFixture
      "type-export-all-constructors"
      (moduleBlock (Just [ModuleTypeExportSelector "Choice" span2 (AllTypeConstructors span3)])),
    directFixture
      "type-export-selected-constructors"
      ( moduleBlock
          ( Just
              [ ModuleTypeExportSelector
                  "Maybe"
                  span2
                  ( SelectedTypeConstructors
                      (LocatedModuleExportName "Some" span3 :| [LocatedModuleExportName "None" span4])
                  )
              ]
          )
      ),
    directFixture "import-plain" (SEBlock [SSImport span1 ["Core", "List"] Nothing Nothing]),
    directFixture "import-alias" (SEBlock [SSImport span1 ["Core", "Text"] (Just "Text") Nothing]),
    directFixture "import-symbols" (SEBlock [SSImport span1 ["Core", "Text"] Nothing (Just ["length", "uncons"])]),
    directFixture
      "imports-source-order"
      ( SEBlock
          [ SSImport span1 ["Core", "List"] Nothing Nothing,
            SSImport span2 ["Core", "Text"] (Just "Text") Nothing,
            SSImport span3 ["Core", "Maybe"] Nothing (Just ["map"])
          ]
      ),
    directFixture
      "nested-import-preserved"
      ( SEBlock
          [ SSImport span1 ["Top", "Level"] Nothing Nothing,
            SSLet
              "nested"
              span2
              (SEBlock [SSImport span3 ["Nested", "Level"] Nothing Nothing])
          ]
      ),
    directFixture "complete-span-qualification" completeSpanExpression,
    DirectModuleFixture
      "path-mismatch"
      moduleSourcePath
      moduleExpectedPath
      (SEBlock [SSModule span2 ["Wrong", "Path"] Nothing]),
    directFixture
      "multiple-declarations-two"
      ( SEBlock
          [ SSModule span1 ["App", "First"] Nothing,
            SSModule span2 ["App", "Second"] Nothing
          ]
      ),
    directFixture
      "multiple-declarations-three"
      ( SEBlock
          [ SSModule span1 ["App", "First"] Nothing,
            SSModule span2 ["App", "Second"] Nothing,
            SSModule span3 ["App", "Third"] Nothing
          ]
      )
  ]

directFixture :: Text.Text -> SurfaceExpr -> DirectModuleFixture
directFixture name expression =
  DirectModuleFixture name moduleSourcePath moduleExpectedPath expression

moduleBlock :: Maybe [ModuleExportSelector] -> SurfaceExpr
moduleBlock exports =
  SEBlock
    [ SSModule span1 moduleExpectedPath exports,
      SSExpr span4 (SELit (SLInt 1))
    ]

completeSpanExpression :: SurfaceExpr
completeSpanExpression =
  SEBlock
    [ SSModule
        span1
        moduleExpectedPath
        ( Just
            [ ModuleTypeExportSelector
                "Maybe"
                span2
                ( SelectedTypeConstructors
                    (LocatedModuleExportName "Some" span3 :| [LocatedModuleExportName "None" span4])
                )
            ]
        ),
      SSImport span2 ["Core", "Text"] Nothing (Just ["length"]),
      SSLet "typed" span3 (SETypeApplication (SEVar "identity") span4 SurfaceTypeInt),
      SSSignature "typed" span4 (SurfaceSignatureType SurfaceTypeInt),
      SSData span1 "Box" ["a"] [SurfaceDataConstructor "Box" [SurfaceDataConstructorArgumentName "a"]],
      SSClass
        span2
        "Eq"
        ["a"]
        [SurfaceClassMethodSignature "equals" span3 (SurfaceSignatureType SurfaceTypeBool)],
      SSImpl
        span3
        "Eq"
        [SurfaceTypeInt]
        [SurfaceImplMethod "equals" span4 (SEBlock [SSExpr span1 (SELit (SLBool True))])],
      SSLet
        "nested"
        span4
        (SEBlock [SSImport span1 ["Nested", "Module"] Nothing Nothing]),
      SSExpr
        span2
        ( SECase
            (SEVar "typed")
            [SurfaceCaseArm SPWildcard (Just (SELit (SLBool True))) (SEIf (SELit (SLBool True)) (SELit (SLInt 1)) (SELit (SLInt 0)))]
        )
    ]

moduleSourcePath :: FilePath
moduleSourcePath = "fixtures/core/modules-corpus-closure.jz"

moduleExpectedPath :: [Text.Text]
moduleExpectedPath = ["App", "Main"]

span1 :: SourceSpan
span1 = SourceSpan 1 1

span2 :: SourceSpan
span2 = SourceSpan 2 3

span3 :: SourceSpan
span3 = SourceSpan 3 5

span4 :: SourceSpan
span4 = SourceSpan 4 7

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
