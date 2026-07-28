{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text as Text
import JazzNext.Compiler.Bootstrap.JazzCoreParity
  ( expectedCanonicalExpressionBatchRendering,
    expectedCoreCorpusRendering,
    expectedCoreSourceBatchRendering,
    expectedModuleBatchRendering,
    runJazzCanonicalExpressionBatch,
    runJazzCoreCorpus,
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
import JazzNext.Compiler.Parser.FixtureCorpus
  ( ParserFixture (..),
    ParserFixtureExpectation (..),
    parserFixtureCorpus,
  )
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
    ("composes parsing and module lowering for the fixed source family twice", testComposedSourceParity),
    ("audits the fixed accepted-corpus lowering manifest", testAcceptedCorpusManifest),
    ("matches stage 0 for all 196 accepted parser fixtures twice", testAcceptedCorpusParity)
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
  assertContains "bounded mixed surface class" "CoreClassStatement" expected
  assertContains "bounded mixed surface implementation" "CoreImplStatement" expected
  assertContains "bounded mixed surface operator" "CoreBinaryExpression" expected
  first <- runJazzCoreSourceBatch composedSourceInputs
  second <- runJazzCoreSourceBatch composedSourceInputs
  assertSuccessfulOutput "composed source first run" expected first
  assertSuccessfulOutput "composed source second run" expected second
  assertEqual "composed source deterministic output" (runOutput first) (runOutput second)

testAcceptedCorpusManifest :: IO ()
testAcceptedCorpusManifest = do
  let acceptedFixtures = filter ((== ParserAccepted) . parserFixtureExpectation) parserFixtureCorpus
      rejectedFixtures = filter ((== ParserRejected) . parserFixtureExpectation) parserFixtureCorpus
      syntheticCompleteManifest = map (\fixture -> CoreCorpusManifestEntry (parserFixtureName fixture) []) acceptedFixtures
  assertEqual "fixed parser corpus count" 365 (length parserFixtureCorpus)
  assertEqual "fixed accepted parser corpus count" 196 (length acceptedFixtures)
  assertEqual "fixed rejected parser corpus count" 169 (length rejectedFixtures)
  case (syntheticCompleteManifest, rejectedFixtures) of
    (firstAccepted : secondAccepted : remainingAccepted, firstRejected : _) -> do
      assertManifestViolation
        "duplicate manifest names"
        "DuplicateCoreCorpusManifestName"
        (syntheticCompleteManifest <> [firstAccepted])
      assertManifestViolation
        "missing manifest name"
        "MissingCoreCorpusManifestName"
        (CoreCorpusManifestEntry "" [] : secondAccepted : remainingAccepted)
      assertManifestViolation
        "unknown manifest name"
        "UnknownCoreCorpusManifestName"
        (CoreCorpusManifestEntry "not-a-parser-fixture" [] : secondAccepted : remainingAccepted)
      assertManifestViolation
        "rejected fixture inclusion"
        "RejectedCoreCorpusManifestFixture"
        (CoreCorpusManifestEntry (parserFixtureName firstRejected) [] : secondAccepted : remainingAccepted)
      assertManifestViolation
        "accepted fixture omission"
        "AcceptedCoreCorpusFixtureOmitted"
        (secondAccepted : remainingAccepted)
      assertManifestViolation
        "manifest order drift"
        "CoreCorpusManifestOrderDrift"
        (secondAccepted : firstAccepted : remainingAccepted)
    _ -> failTest "fixed corpus must contain at least two accepted and one rejected fixture"
  assertEqual
    "accepted corpus manifest"
    []
    (validateCoreCorpusManifest parserFixtureCorpus acceptedCoreCorpusManifest)

testAcceptedCorpusParity :: IO ()
testAcceptedCorpusParity = do
  inputs <- expectRight "accepted corpus inputs" (resolveCoreCorpusInputs parserFixtureCorpus acceptedCoreCorpusManifest)
  expected <- expectRight "accepted corpus expected values" (expectedCoreCorpusRendering inputs)
  assertEqual "accepted corpus has no lexical failures" False ("CanonicalCoreSourceLexicalFailure" `Text.isInfixOf` expected)
  assertEqual "accepted corpus has no parser failures" False ("CanonicalCoreSourceParserFailure" `Text.isInfixOf` expected)
  assertEqual "accepted corpus has no module-lowering failures" False ("CoreModuleLoweringFailed" `Text.isInfixOf` expected)
  first <- runJazzCoreCorpus inputs
  second <- runJazzCoreCorpus inputs
  assertSuccessfulOutput "accepted corpus first run" expected first
  assertSuccessfulOutput "accepted corpus second run" expected second
  assertEqual "accepted corpus deterministic output" (runOutput first) (runOutput second)

data CoreCorpusManifestEntry = CoreCorpusManifestEntry
  { coreCorpusFixtureName :: Text.Text,
    coreCorpusExpectedModulePath :: [Text.Text]
  }
  deriving (Eq, Show)

data CoreCorpusManifestViolation
  = DuplicateCoreCorpusManifestName Text.Text
  | MissingCoreCorpusManifestName
  | UnknownCoreCorpusManifestName Text.Text
  | RejectedCoreCorpusManifestFixture Text.Text
  | AcceptedCoreCorpusFixtureOmitted Text.Text
  | CoreCorpusManifestOrderDrift
  | CoreCorpusFixtureCountsChanged Int Int Int
  | CoreCorpusManifestCountChanged Int
  deriving (Eq, Show)

acceptedCoreCorpusManifest :: [CoreCorpusManifestEntry]
acceptedCoreCorpusManifest =
  [ corpusEntry "lexer-leading-zero-integer",
    corpusEntry "lexer-crlf-spans",
    corpusEntry "lexer-unicode-and-escape-values",
    corpusEntry "lexer-arbitrary-precision-integer",
    corpusEntry "lexer-comments-spaces-and-tabs",
    corpusEntry "lexer-lf-spans",
    corpusEntry "lexer-all-supported-escapes",
    corpusEntry "parser-corpus-0001",
    corpusEntry "parser-corpus-0009",
    corpusEntry "parser-corpus-0017",
    corpusEntry "parser-corpus-0024",
    corpusEntry "parser-corpus-0025",
    corpusEntry "parser-corpus-0028",
    corpusEntry "parser-corpus-0029",
    corpusEntry "parser-corpus-0031",
    corpusEntry "parser-corpus-0033",
    corpusEntry "parser-corpus-0038",
    corpusEntry "parser-corpus-0039",
    corpusEntry "parser-corpus-0040",
    corpusEntry "parser-corpus-0042",
    corpusEntry "parser-corpus-0045",
    corpusEntry "parser-corpus-0046",
    corpusEntry "parser-corpus-0047",
    corpusEntry "parser-corpus-0048",
    corpusEntry "parser-corpus-0049",
    corpusEntry "parser-corpus-0050",
    corpusEntry "parser-corpus-0051",
    corpusEntry "parser-corpus-0054",
    corpusEntry "parser-corpus-0058",
    corpusEntry "parser-corpus-0059",
    corpusEntry "parser-corpus-0063",
    corpusEntry "parser-corpus-0067",
    corpusEntry "parser-corpus-0070",
    corpusEntry "parser-corpus-0071",
    corpusEntry "parser-corpus-0073",
    corpusEntry "parser-corpus-0074",
    corpusEntry "parser-corpus-0075",
    corpusEntry "parser-corpus-0076",
    corpusEntry "parser-corpus-0077",
    corpusEntry "parser-corpus-0078",
    corpusEntry "parser-corpus-0079",
    corpusEntry "parser-corpus-0080",
    corpusEntry "parser-corpus-0081",
    corpusEntry "parser-corpus-0082",
    corpusEntry "parser-corpus-0083",
    corpusEntry "parser-corpus-0084",
    corpusEntry "parser-corpus-0085",
    corpusEntry "parser-corpus-0087",
    corpusEntry "parser-corpus-0090",
    corpusEntry "parser-corpus-0091",
    corpusEntry "parser-corpus-0092",
    corpusEntry "parser-corpus-0093",
    corpusEntry "parser-corpus-0094",
    corpusEntry "parser-corpus-0096",
    corpusEntry "parser-corpus-0099",
    corpusEntry "parser-corpus-0100",
    corpusEntry "parser-corpus-0102",
    corpusEntry "parser-corpus-0103",
    corpusEntry "parser-corpus-0106",
    corpusEntry "parser-corpus-0110",
    corpusEntry "parser-corpus-0114",
    corpusEntry "parser-corpus-0115",
    corpusEntry "parser-corpus-0117",
    corpusEntry "parser-corpus-0118",
    corpusEntry "parser-corpus-0119",
    corpusEntry "parser-corpus-0120",
    corpusEntry "parser-corpus-0121",
    corpusEntry "parser-corpus-0122",
    corpusEntry "parser-corpus-0125",
    corpusEntry "parser-corpus-0128",
    corpusEntry "parser-corpus-0131",
    corpusEntry "parser-corpus-0133",
    corpusModuleEntry "parser-corpus-0138" ["App", "Core"],
    corpusModuleEntry "parser-corpus-0139" ["App", "Core"],
    corpusModuleEntry "parser-corpus-0141" ["App", "Core"],
    corpusModuleEntry "parser-corpus-0143" ["App", "Core"],
    corpusModuleEntry "parser-corpus-0146" ["App", "Internal"],
    corpusModuleEntry "parser-corpus-0147" ["App", "Main"],
    corpusModuleEntry "parser-corpus-0148" ["App", "Main"],
    corpusModuleEntry "parser-corpus-0149" ["Demo"],
    corpusModuleEntry "parser-corpus-0150" ["Lib", "Box"],
    corpusModuleEntry "parser-corpus-0151" ["Lib", "Box"],
    corpusModuleEntry "parser-corpus-0153" ["Lib", "Keywords"],
    corpusModuleEntry "parser-corpus-0154" ["Lib", "Maybe"],
    corpusModuleEntry "parser-corpus-0156" ["Lib", "Value"],
    corpusEntry "parser-corpus-0160",
    corpusEntry "parser-corpus-0163",
    corpusEntry "parser-corpus-0164",
    corpusEntry "parser-corpus-0165",
    corpusEntry "parser-corpus-0167",
    corpusEntry "parser-corpus-0168",
    corpusEntry "parser-corpus-0170",
    corpusEntry "parser-corpus-0172",
    corpusEntry "parser-corpus-0179",
    corpusEntry "parser-corpus-0180",
    corpusEntry "parser-corpus-0181",
    corpusEntry "parser-corpus-0182",
    corpusEntry "parser-corpus-0189",
    corpusEntry "parser-corpus-0190",
    corpusEntry "parser-corpus-0191",
    corpusEntry "parser-corpus-0192",
    corpusEntry "parser-corpus-0193",
    corpusEntry "parser-corpus-0194",
    corpusEntry "parser-corpus-0195",
    corpusEntry "parser-corpus-0196",
    corpusEntry "parser-corpus-0197",
    corpusEntry "parser-corpus-0198",
    corpusEntry "parser-corpus-0199",
    corpusEntry "parser-corpus-0201",
    corpusEntry "parser-corpus-0204",
    corpusEntry "parser-corpus-0205",
    corpusEntry "parser-corpus-0206",
    corpusEntry "parser-corpus-0207",
    corpusEntry "parser-corpus-0208",
    corpusEntry "parser-corpus-0209",
    corpusEntry "parser-corpus-0211",
    corpusEntry "parser-corpus-0214",
    corpusEntry "parser-corpus-0215",
    corpusEntry "parser-corpus-0216",
    corpusEntry "parser-corpus-0220",
    corpusEntry "parser-corpus-0221",
    corpusEntry "parser-corpus-0222",
    corpusEntry "parser-corpus-0224",
    corpusEntry "parser-corpus-0225",
    corpusEntry "parser-corpus-0226",
    corpusEntry "parser-corpus-0230",
    corpusEntry "parser-corpus-0231",
    corpusEntry "parser-corpus-0234",
    corpusEntry "parser-corpus-0236",
    corpusEntry "parser-corpus-0237",
    corpusEntry "parser-corpus-0238",
    corpusEntry "parser-corpus-0239",
    corpusEntry "parser-corpus-0241",
    corpusEntry "parser-corpus-0246",
    corpusEntry "parser-corpus-0250",
    corpusEntry "parser-corpus-0251",
    corpusEntry "parser-corpus-0253",
    corpusEntry "parser-corpus-0254",
    corpusEntry "parser-corpus-0255",
    corpusEntry "parser-corpus-0256",
    corpusEntry "parser-corpus-0257",
    corpusEntry "parser-corpus-0259",
    corpusEntry "parser-corpus-0260",
    corpusEntry "parser-corpus-0261",
    corpusEntry "parser-corpus-0262",
    corpusEntry "parser-corpus-0263",
    corpusEntry "parser-corpus-0264",
    corpusEntry "parser-corpus-0265",
    corpusEntry "parser-corpus-0266",
    corpusEntry "parser-corpus-0268",
    corpusEntry "parser-corpus-0270",
    corpusEntry "parser-corpus-0271",
    corpusEntry "parser-corpus-0272",
    corpusEntry "parser-corpus-0273",
    corpusEntry "parser-corpus-0274",
    corpusEntry "parser-corpus-0275",
    corpusEntry "parser-corpus-0276",
    corpusEntry "parser-corpus-0277",
    corpusEntry "parser-corpus-0278",
    corpusEntry "parser-corpus-0280",
    corpusEntry "parser-corpus-0281",
    corpusEntry "parser-corpus-0282",
    corpusEntry "parser-corpus-0283",
    corpusEntry "parser-corpus-0284",
    corpusEntry "parser-corpus-0285",
    corpusEntry "parser-corpus-0286",
    corpusEntry "parser-corpus-0287",
    corpusEntry "parser-corpus-0288",
    corpusEntry "parser-corpus-0289",
    corpusEntry "parser-corpus-0291",
    corpusEntry "parser-corpus-0292",
    corpusEntry "parser-corpus-0293",
    corpusEntry "parser-corpus-0296",
    corpusEntry "parser-corpus-0297",
    corpusEntry "parser-corpus-0298",
    corpusEntry "parser-corpus-0299",
    corpusEntry "parser-corpus-0300",
    corpusEntry "parser-corpus-0301",
    corpusEntry "parser-corpus-0302",
    corpusEntry "parser-corpus-0305",
    corpusEntry "parser-corpus-0309",
    corpusEntry "parser-corpus-0310",
    corpusEntry "parser-corpus-0311",
    corpusEntry "parser-corpus-0312",
    corpusEntry "expression-foundation-empty-program",
    corpusEntry "expression-foundation-empty-block",
    corpusEntry "expression-foundation-grouped-name",
    corpusEntry "expression-foundation-empty-list",
    corpusEntry "expression-foundation-list-literals",
    corpusEntry "expression-foundation-parenthesized-application",
    corpusEntry "expression-foundation-max-float64",
    corpusEntry "types-declarations-modules-unsupported-forall-signature",
    corpusEntry "types-declarations-modules-foundational-impl-method",
    corpusEntry "types-declarations-modules-applied-explicit-type-application",
    corpusEntry "control-flow-patterns-guarded-or-pattern",
    corpusEntry "control-flow-patterns-recursive-block"
  ]

corpusEntry :: Text.Text -> CoreCorpusManifestEntry
corpusEntry name = CoreCorpusManifestEntry name []

corpusModuleEntry :: Text.Text -> [Text.Text] -> CoreCorpusManifestEntry
corpusModuleEntry = CoreCorpusManifestEntry

resolveCoreCorpusInputs ::
  [ParserFixture] ->
  [CoreCorpusManifestEntry] ->
  Either Text.Text [(FilePath, [Text.Text], Text.Text)]
resolveCoreCorpusInputs fixtures = mapM resolveEntry
  where
    resolveEntry entry =
      case lookupParserFixture (coreCorpusFixtureName entry) fixtures of
        Nothing -> Left ("unknown accepted corpus fixture: " <> coreCorpusFixtureName entry)
        Just fixture ->
          Right
            ( parserFixturePath fixture,
              coreCorpusExpectedModulePath entry,
              parserFixtureSource fixture
            )

lookupParserFixture :: Text.Text -> [ParserFixture] -> Maybe ParserFixture
lookupParserFixture name fixtures =
  case fixtures of
    [] -> Nothing
    fixture : remaining
      | parserFixtureName fixture == name -> Just fixture
      | otherwise -> lookupParserFixture name remaining

validateCoreCorpusManifest :: [ParserFixture] -> [CoreCorpusManifestEntry] -> [CoreCorpusManifestViolation]
validateCoreCorpusManifest fixtures manifest =
  map DuplicateCoreCorpusManifestName (duplicateValues manifestNames)
    <> [MissingCoreCorpusManifestName | "" `elem` manifestNames]
    <> map UnknownCoreCorpusManifestName unknownNames
    <> map RejectedCoreCorpusManifestFixture rejectedNames
    <> map AcceptedCoreCorpusFixtureOmitted omittedAcceptedNames
    <> [CoreCorpusManifestOrderDrift | completeNameSet && manifestNames /= acceptedNames]
    <> [CoreCorpusFixtureCountsChanged fixtureCount acceptedCount rejectedCount | (fixtureCount, acceptedCount, rejectedCount) /= (365, 196, 169)]
    <> [CoreCorpusManifestCountChanged (length manifest) | length manifest /= 196]
  where
    manifestNames = map coreCorpusFixtureName manifest
    fixtureNames = map parserFixtureName fixtures
    acceptedNames = map parserFixtureName (filter ((== ParserAccepted) . parserFixtureExpectation) fixtures)
    rejectedFixtureNames = map parserFixtureName (filter ((== ParserRejected) . parserFixtureExpectation) fixtures)
    unknownNames = uniqueValues (filter (not . (`elem` fixtureNames)) manifestNames)
    rejectedNames = uniqueValues (filter (`elem` rejectedFixtureNames) manifestNames)
    omittedAcceptedNames = filter (not . (`elem` manifestNames)) acceptedNames
    completeNameSet =
      null (duplicateValues manifestNames)
        && null unknownNames
        && null rejectedNames
        && null omittedAcceptedNames
        && length manifestNames == length acceptedNames
    fixtureCount = length fixtures
    acceptedCount = length acceptedNames
    rejectedCount = length rejectedFixtureNames

duplicateValues :: (Eq value) => [value] -> [value]
duplicateValues = collectDuplicateValues [] []

collectDuplicateValues :: (Eq value) => [value] -> [value] -> [value] -> [value]
collectDuplicateValues seen duplicates values =
  case values of
    [] -> reverse duplicates
    value : remaining
      | value `elem` seen && value `notElem` duplicates ->
          collectDuplicateValues seen (value : duplicates) remaining
      | otherwise -> collectDuplicateValues (value : seen) duplicates remaining

uniqueValues :: (Eq value) => [value] -> [value]
uniqueValues = collectUniqueValues []

collectUniqueValues :: (Eq value) => [value] -> [value] -> [value]
collectUniqueValues seen values =
  case values of
    [] -> reverse seen
    value : remaining
      | value `elem` seen -> collectUniqueValues seen remaining
      | otherwise -> collectUniqueValues (value : seen) remaining

assertManifestViolation :: Text.Text -> Text.Text -> [CoreCorpusManifestEntry] -> IO ()
assertManifestViolation label expectedViolation manifest =
  assertContains
    label
    expectedViolation
    (Text.pack (show (validateCoreCorpusManifest parserFixtureCorpus manifest)))

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
                SSExpr span3 (SEVar "item")
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
      | Some item if True -> if False then 0 else item %% 2
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
                ModuleExportSelector (Just ValueNamespace) "item",
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
      SSData span1 "Box" ["a"] [SurfaceDataConstructor "Box" [SurfaceTypeVariable "a"]],
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
