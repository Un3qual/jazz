{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (forM, forM_, unless, when)
import Data.Aeson (Value (..), eitherDecodeStrict')
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as ByteString
import Data.Foldable (toList)
import Data.List (sort)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Jazz.Compiler.AST
  ( SignatureType (..),
  )
import Jazz.Compiler.DiagnosticCatalog
  ( ErrorCode (E5001),
  )
import Jazz.Compiler.Diagnostics
  ( DiagnosticOrigin (ToolingOrigin),
    mkErrorDiagnostic,
  )
import Jazz.Compiler.Diagnostics.Render (renderDiagnostic)
import Jazz.Compiler.Parser (parseSurfaceProgram)
import qualified Jazz.Compiler.Parser.AST as Surface
import Jazz.Compiler.SignatureRendering
  ( renderSignatureType,
  )
import qualified Jazz.Repository.AuthoredSources as AuthoredSources
import Jazz.Repository.FeatureInventory
  ( SurfaceFeature (..),
    inventorySurface,
    requiredAuthoredFeatures,
  )
import Jazz.Repository.JazzSourceFormat
  ( JazzSourceFormatViolation (..),
    renderJazzSourceFormatViolation,
    validateJazzModule,
  )
import Jazz.Repository.PackagePolicy
  ( PackagePolicyViolation (..),
    renderPackagePolicyViolation,
    validatePackagePolicy,
  )
import Jazz.Repository.Root (findJazzPackageRoot)
import Jazz.Repository.SourceLayout
  ( JazzSourceModule,
    JazzSourceRole (..),
    SourceLayoutViolation (..),
    renderSourceLayoutViolation,
    sourceModuleFromSurface,
    validateSourceLayering,
  )
import Jazz.TestHarness
  ( NamedTest,
    assertEqual,
    failTest,
    runTestSuite,
  )
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath (makeRelative, takeExtension, (</>))

main :: IO ()
main = runTestSuite "RepositoryAudit" tests

tests :: [NamedTest]
tests =
  [ ("discovers the complete authored Jazz source set", testAuthoredSourceInventory),
    ("covers the implemented Jazz surface across authored sources", testAuthoredFeatureInventory),
    ("distinguishes partial applications from saturated calls", testPartialApplicationInventory),
    ("covers every public standard-library module family", testStandardLibraryModuleInventory),
    ("accepts a valid Jazz source module", testValidJazzModule),
    ("accepts a multiline module export header", testMultilineModuleHeader),
    ("rejects a missing module header", testMissingModuleHeader),
    ("rejects a missing final closing brace", testMissingClosingBrace),
    ("rejects blank lines after the final closing brace", testTrailingBlankLines),
    ("rejects odd or shallow body indentation", testBodyIndentation),
    ("accepts canonical multiline data declarations", testCanonicalMultilineDataDeclaration),
    ("rejects overlong data declaration lines", testOverlongDataDeclarationLine),
    ("rejects shallow data payload continuations", testDataContinuationIndent),
    ("exempts the bundled Prelude source", testPreludeExemption),
    ("accepts only the named private Cabal library", testPrivatePackagePolicy),
    ("rejects an unnamed public Cabal library", testPublicLibraryPolicy),
    ("rejects a named public Cabal library", testNamedPublicLibraryPolicy),
    ("rejects a private library without private visibility", testMissingPrivateVisibility),
    ("rejects stdlib imports of compiler modules", testRejectsStdlibCompilerImport),
    ("accepts compiler imports of stdlib modules", testAcceptsCompilerStdlibImport),
    ("uses the locked checked-in Jazz source tree", testCheckedInJazzSourceTree),
    ("validates the Jazz editor package metadata", testEditorPackageMetadata),
    ("parses the representative editor fixture", testEditorFixtureParses),
    ("uses the canonical repository layout", testCanonicalRepositoryLayout),
    ("locates the Jazz package root", testPackageRoot),
    ("validates all checked-in Jazz source modules", testCheckedInJazzSources),
    ("validates the checked-in Cabal package policy", testCheckedInPackagePolicy),
    ("integrates the unified diagnostic and signature-rendering boundaries", testDiagnosticRenderingBoundaries),
    ("documents the shared program corpus and performance workflows", testPerformanceDocumentation),
    ("uses canonical root-relative documentation paths", testCanonicalDocumentationPaths),
    ("keeps repository infrastructure on the canonical Jazz identity", testCanonicalRepositoryInfrastructure)
  ]

testAuthoredSourceInventory :: IO ()
testAuthoredSourceInventory =
  withPackageRoot $ \packageRoot -> do
    sources <- AuthoredSources.readAuthoredSources packageRoot
    assertEqual
      "authored source paths"
      expectedAuthoredSourcePaths
      (map AuthoredSources.authoredRelativePath sources)
    assertEqual
      "authored source roles"
      [ AuthoredSources.StandardLibrarySource,
        AuthoredSources.CompilerSource,
        AuthoredSources.ProgramSource,
        AuthoredSources.EditorFixtureSource
      ]
      (sort (uniqueValues (map AuthoredSources.authoredRole sources)))

testAuthoredFeatureInventory :: IO ()
testAuthoredFeatureInventory =
  withPackageRoot $ \packageRoot -> do
    sources <- AuthoredSources.readAuthoredSources packageRoot
    let observed =
          Set.unions
            [ inventorySurface
                (AuthoredSources.authoredText source)
                (AuthoredSources.authoredSurface source)
            | source <- sources
            ]
        missing = requiredAuthoredFeatures `Set.difference` observed
    unless (Set.null missing) $
      failTest
        ( "authored Jazz sources do not exercise: "
            <> Text.pack (show (Set.toAscList missing))
        )

testPartialApplicationInventory :: IO ()
testPartialApplicationInventory = do
  saturatedFeatures <- inventoryParsedSource saturatedApplicationSource
  partialFeatures <- inventoryParsedSource partialApplicationSource
  assertEqual
    "saturated multi-argument call is not partial"
    False
    (PartialApplicationFeature `Set.member` saturatedFeatures)
  assertEqual
    "under-applied multi-argument function is partial"
    True
    (PartialApplicationFeature `Set.member` partialFeatures)

inventoryParsedSource :: Text -> IO (Set.Set SurfaceFeature)
inventoryParsedSource source =
  case parseSurfaceProgram source of
    Left diagnostic ->
      failTest
        ( "could not parse feature-inventory fixture: "
            <> renderDiagnostic diagnostic
        )
    Right surface -> pure (inventorySurface source surface)

saturatedApplicationSource :: Text
saturatedApplicationSource =
  """
  combine = \\(left, right) -> left + right.
  result = combine 1 2.
  """

partialApplicationSource :: Text
partialApplicationSource =
  """
  combine = \\(left, right) -> left + right.
  addOne = combine 1.
  """

testStandardLibraryModuleInventory :: IO ()
testStandardLibraryModuleInventory =
  withPackageRoot $ \packageRoot -> do
    sources <- AuthoredSources.readAuthoredSources packageRoot
    let stdlibSources =
          filter
            ((== AuthoredSources.StandardLibrarySource) . AuthoredSources.authoredRole)
            sources
        publicModulePaths =
          Set.unions
            [ surfaceModulePaths (AuthoredSources.authoredSurface source)
            | source <- stdlibSources,
              AuthoredSources.authoredRelativePath source /= "jazz/stdlib/Prelude.jz"
            ]
        expectedModulePaths =
          Set.fromList
            [ ["Char"],
              ["Dictionary"],
              ["IO"],
              ["IOError"],
              ["List"],
              ["Map"],
              ["Maybe"],
              ["NonEmpty"],
              ["Queue"],
              ["Result"],
              ["Set"],
              ["Text"]
            ]
        preludeSources =
          filter
            ((== "jazz/stdlib/Prelude.jz") . AuthoredSources.authoredRelativePath)
            stdlibSources
    assertEqual "public standard-library module paths" expectedModulePaths publicModulePaths
    assertEqual "one ambient Prelude source" 1 (length preludeSources)
    assertEqual
      "ambient Prelude has no module wrapper"
      Set.empty
      (Set.unions (map (surfaceModulePaths . AuthoredSources.authoredSurface) preludeSources))

surfaceModulePaths :: Surface.SurfaceExpr -> Set.Set [Text]
surfaceModulePaths expression =
  case expression of
    Surface.SEBlock statements ->
      Set.fromList
        [ modulePath
        | Surface.SSModule _ modulePath _ <- statements
        ]
    _ -> Set.empty

expectedAuthoredSourcePaths :: [FilePath]
expectedAuthoredSourcePaths =
  [ "editors/vscode-jazz/fixtures/representative.jz",
    "jazz/compiler/Core.jz",
    "jazz/compiler/CoreLower.jz",
    "jazz/compiler/CoreTypes.jz",
    "jazz/compiler/Lexer.jz",
    "jazz/compiler/LexerTypes.jz",
    "jazz/compiler/LoweredIRTypes.jz",
    "jazz/compiler/LoweredIRValidate.jz",
    "jazz/compiler/Parser.jz",
    "jazz/compiler/ParserContext.jz",
    "jazz/compiler/ParserCore.jz",
    "jazz/compiler/ParserDeclaration.jz",
    "jazz/compiler/ParserExpression.jz",
    "jazz/compiler/ParserOperator.jz",
    "jazz/compiler/ParserPattern.jz",
    "jazz/compiler/ParserProgram.jz",
    "jazz/compiler/ParserSignature.jz",
    "jazz/compiler/ParserToken.jz",
    "jazz/compiler/ParserTypes.jz",
    "jazz/compiler/TypedCoreTypes.jz",
    "jazz/compiler/TypedCoreValidate.jz",
    "jazz/stdlib/Char.jz",
    "jazz/stdlib/Dictionary.jz",
    "jazz/stdlib/IO.jz",
    "jazz/stdlib/IOError.jz",
    "jazz/stdlib/List.jz",
    "jazz/stdlib/Map.jz",
    "jazz/stdlib/Maybe.jz",
    "jazz/stdlib/NonEmpty.jz",
    "jazz/stdlib/Prelude.jz",
    "jazz/stdlib/Queue.jz",
    "jazz/stdlib/Result.jz",
    "jazz/stdlib/Set.jz",
    "jazz/stdlib/Text.jz",
    "programs/capability-workflow/Main.jz",
    "programs/capability-workflow/Workflow.jz",
    "programs/collection-boundaries/Collections.jz",
    "programs/collection-boundaries/Main.jz",
    "programs/dependency-planner/Graph.jz",
    "programs/dependency-planner/Main.jz",
    "programs/expression-evaluator/Expression.jz",
    "programs/expression-evaluator/Main.jz",
    "programs/fannkuch/Fannkuch.jz",
    "programs/fannkuch/Main.jz",
    "programs/identifier-classifier/Main.jz",
    "programs/merge-sort/Main.jz",
    "programs/merge-sort/MergeSort.jz",
    "programs/mini-frontend/Analysis.jz",
    "programs/mini-frontend/Evaluation.jz",
    "programs/mini-frontend/Main.jz",
    "programs/mini-frontend/Syntax.jz",
    "programs/mini-frontend/Token.jz",
    "programs/n-queens/Main.jz",
    "programs/n-queens/Queens.jz",
    "programs/prime-sieve/Main.jz",
    "programs/prime-sieve/Sieve.jz",
    "programs/queue-traversal/Main.jz",
    "programs/queue-traversal/Traversal.jz",
    "programs/sorted-index/Index.jz",
    "programs/sorted-index/Main.jz",
    "programs/symbolic-differentiation/Main.jz",
    "programs/symbolic-differentiation/Symbolic.jz",
    "programs/tak/Main.jz",
    "programs/tak/Tak.jz",
    "programs/text-processing/Main.jz",
    "programs/tree-transformations/Main.jz",
    "programs/tree-transformations/Tree.jz",
    "programs/word-frequency/Main.jz"
  ]

uniqueValues :: (Eq value) => [value] -> [value]
uniqueValues values =
  case values of
    [] -> []
    value : rest
      | value `elem` rest -> uniqueValues rest
      | otherwise -> value : uniqueValues rest

validJazzSource :: Text
validJazzSource =
  """
  module Good {
    value = 1.
  }
  """

validPrivatePackage :: Text
validPrivatePackage =
  """
  library jazz-internal
    visibility: private
  """

testValidJazzModule :: IO ()
testValidJazzModule =
  assertEqual
    "valid Jazz source violations"
    []
    (validateJazzModule "jazz/stdlib/Good.jz" validJazzSource)

testMultilineModuleHeader :: IO ()
testMultilineModuleHeader =
  assertEqual
    "multiline module header violations"
    []
    ( validateJazzModule
        "jazz/stdlib/Good.jz"
        """
        module Good (
          type Good,
          value makeGood
        ) {
          makeGood = 1.
        }
        """
    )

testMissingModuleHeader :: IO ()
testMissingModuleHeader =
  assertEqual
    "missing module header"
    [InvalidModuleHeader "jazz/stdlib/Bad.jz"]
    ( validateJazzModule
        "jazz/stdlib/Bad.jz"
        """
        value = 1.
        }
        """
    )

testMissingClosingBrace :: IO ()
testMissingClosingBrace =
  assertEqual
    "missing final closing brace"
    [MissingFinalClosingBrace "jazz/stdlib/Bad.jz"]
    ( validateJazzModule
        "jazz/stdlib/Bad.jz"
        """
        module Bad {
          value = 1.
        """
    )

testTrailingBlankLines :: IO ()
testTrailingBlankLines =
  -- Explicit escapes are intentional: this case directly tests trailing whitespace.
  assertEqual
    "trailing blank lines"
    [MissingFinalClosingBrace "jazz/stdlib/Bad.jz"]
    (validateJazzModule "jazz/stdlib/Bad.jz" "module Bad {\n  value = 1.\n}\n\n")

testBodyIndentation :: IO ()
testBodyIndentation =
  assertEqual
    "invalid body indentation"
    [ InvalidBodyIndentation "jazz/stdlib/Bad.jz" 2,
      InvalidBodyIndentation "jazz/stdlib/Bad.jz" 3
    ]
    ( validateJazzModule
        "jazz/stdlib/Bad.jz"
        """
        module Bad {
         shallow = 1.
           odd = 2.
        }
        """
    )

testCanonicalMultilineDataDeclaration :: IO ()
testCanonicalMultilineDataDeclaration =
  assertEqual
    "canonical multiline data declaration"
    []
    ( validateJazzModule
        "jazz/stdlib/Good.jz"
        """
        module Good {
          data TypedLiteral
            = TypedIntegerLiteral Text
            | TypedFractionalLiteral Text Text Maybe(TypedNumericType)
            | TypedBooleanLiteral Bool.
        }
        """
    )

testOverlongDataDeclarationLine :: IO ()
testOverlongDataDeclarationLine =
  assertEqual
    "overlong data declaration line"
    [OverlongDataDeclarationLine "jazz/stdlib/Bad.jz" 2 101]
    ( validateJazzModule
        "jazz/stdlib/Bad.jz"
        ( "module Bad {\n"
            <> "  data X = X "
            <> Text.replicate 87 "A"
            <> "."
            <> "\n}\n"
        )
    )

testDataContinuationIndent :: IO ()
testDataContinuationIndent =
  assertEqual
    "shallow data payload continuation"
    [InvalidDataContinuationIndent "jazz/stdlib/Bad.jz" 4]
    ( validateJazzModule
        "jazz/stdlib/Bad.jz"
        """
        module Bad {
          data TypedFunction
            = TypedFunction
            TypedFunctionId
              [TypedBlock].
        }
        """
    )

testPreludeExemption :: IO ()
testPreludeExemption =
  assertEqual
    "Prelude exemption"
    []
    (validateJazzModule "jazz/stdlib/Prelude.jz" "class Eq(a) { }.")

testPrivatePackagePolicy :: IO ()
testPrivatePackagePolicy =
  assertEqual "valid private library policy" [] (validatePackagePolicy validPrivatePackage)

testPublicLibraryPolicy :: IO ()
testPublicLibraryPolicy =
  assertEqual
    "public library policy"
    [PublicLibraryStanza, MissingPrivateLibraryStanza]
    ( validatePackagePolicy
        """
        library
          exposed-modules: Public
        """
    )

testNamedPublicLibraryPolicy :: IO ()
testNamedPublicLibraryPolicy =
  assertEqual
    "named public library policy"
    [PublicLibraryStanza]
    ( validatePackagePolicy
        """
        library jazz-internal
          visibility: private

        library jazz-api
          visibility: public
        """
    )

testMissingPrivateVisibility :: IO ()
testMissingPrivateVisibility =
  assertEqual
    "missing private visibility"
    [MissingPrivateLibraryVisibility]
    ( validatePackagePolicy
        """
        library jazz-internal
          exposed-modules: Internal
        """
    )

testRejectsStdlibCompilerImport :: IO ()
testRejectsStdlibCompilerImport = do
  compilerModule <-
    parsedSourceModule
      CompilerSource
      "jazz/compiler/Lexer.jz"
      "module Lexer { 0. }"
  stdlibModule <-
    parsedSourceModule
      StandardLibrarySource
      "jazz/stdlib/Bad.jz"
      "module Bad { import Lexer. 0. }"
  assertEqual
    "stdlib compiler dependency"
    [StandardLibraryImportsCompiler "jazz/stdlib/Bad.jz" ["Lexer"]]
    (validateSourceLayering [compilerModule, stdlibModule])

testAcceptsCompilerStdlibImport :: IO ()
testAcceptsCompilerStdlibImport = do
  stdlibModule <-
    parsedSourceModule
      StandardLibrarySource
      "jazz/stdlib/Text.jz"
      "module Text { 0. }"
  compilerModule <-
    parsedSourceModule
      CompilerSource
      "jazz/compiler/Lexer.jz"
      "module Lexer { import Text. 0. }"
  assertEqual
    "compiler stdlib dependency"
    []
    (validateSourceLayering [stdlibModule, compilerModule])

parsedSourceModule :: JazzSourceRole -> FilePath -> Text -> IO JazzSourceModule
parsedSourceModule role path source =
  case parseSurfaceProgram source of
    Left diagnostic ->
      failTest ("fixture did not parse: " <> renderDiagnostic diagnostic)
    Right surfaceProgram ->
      pure (sourceModuleFromSurface role path surfaceProgram)

testCheckedInJazzSourceTree :: IO ()
testCheckedInJazzSourceTree =
  withPackageRoot $ \packageRoot -> do
    let jazzRoot = packageRoot </> "jazz"
        stdlibRoot = jazzRoot </> "stdlib"
        compilerRoot = jazzRoot </> "compiler"
        legacyRoot = packageRoot </> "stdlib"
    stdlibExists <- doesDirectoryExist stdlibRoot
    compilerExists <- doesDirectoryExist compilerRoot
    legacyExists <- doesDirectoryExist legacyRoot
    assertEqual "stdlib source root exists" True stdlibExists
    assertEqual "compiler source root exists" True compilerExists
    assertEqual "legacy stdlib root is absent" False legacyExists

testEditorPackageMetadata :: IO ()
testEditorPackageMetadata =
  withPackageRoot $ \packageRoot -> do
    let editorRoot = packageRoot </> "editors" </> "vscode-jazz"
        configurationPath = editorRoot </> "language-configuration.json"
        grammarPath = editorRoot </> "syntaxes" </> "jazz.tmLanguage.json"
    manifest <- decodeJsonFile (editorRoot </> "package.json")
    languageConfiguration <- decodeJsonFile configurationPath
    grammar <- decodeJsonFile grammarPath
    let languages = maybe [] jsonArray (jsonPath ["contributes", "languages"] manifest)
        grammars = maybe [] jsonArray (jsonPath ["contributes", "grammars"] manifest)
        language = firstValue languages
        contributedGrammar = firstValue grammars
        extensions = maybe [] jsonArray (language >>= jsonPath ["extensions"])
        rootGrammarPatterns = maybe [] jsonArray (jsonPath ["patterns"] grammar)
        rootGrammarIncludes =
          [ includeName
          | patternValue <- rootGrammarPatterns,
            Just (String includeName) <- [jsonPath ["include"] patternValue]
          ]
        dataDeclarationPatterns =
          maybe
            []
            jsonArray
            (jsonPath ["repository", "data-declarations", "patterns"] grammar)
        dataDeclarationIncludes =
          [ includeName
          | patternValue <- dataDeclarationPatterns,
            Just (String includeName) <- [jsonPath ["include"] patternValue]
          ]
        constructorPattern = firstValue dataDeclarationPatterns
        keywordPatterns =
          maybe
            []
            jsonArray
            (jsonPath ["repository", "keywords", "patterns"] grammar)
        reservedValuePattern =
          firstValue
            [ patternValue
            | patternValue <- keywordPatterns,
              jsonPath ["match"] patternValue == Just (String "\\bvalue\\b")
            ]
        exportRegionBegin = jsonPath ["repository", "exports", "begin"] grammar
        exportPatterns =
          maybe
            []
            jsonArray
            (jsonPath ["repository", "exports", "patterns"] grammar)
        groupedTypeExportPattern = firstValue exportPatterns
        groupedTypeExportMembers =
          maybe [] jsonArray (groupedTypeExportPattern >>= jsonPath ["patterns"])
        groupedConstructorPattern = firstValue groupedTypeExportMembers
        operatorPatterns =
          maybe
            []
            jsonArray
            (jsonPath ["repository", "operators", "patterns"] grammar)
        operatorPattern = firstValue operatorPatterns
        operatorMatch = operatorPattern >>= jsonPath ["match"]
        lambdaPatterns =
          maybe
            []
            jsonArray
            (jsonPath ["repository", "lambdas", "patterns"] grammar)
        patternLambdaIntroducer = firstValue lambdaPatterns
    assertEqual "manifest language id" (Just (String "jazz")) (language >>= jsonPath ["id"])
    assertEqual "manifest .jz extension" True (String ".jz" `elem` extensions)
    assertEqual
      "manifest language configuration path"
      (Just (String "./language-configuration.json"))
      (language >>= jsonPath ["configuration"])
    assertEqual
      "manifest grammar scope"
      (Just (String "source.jazz"))
      (contributedGrammar >>= jsonPath ["scopeName"])
    assertEqual
      "manifest grammar path"
      (Just (String "./syntaxes/jazz.tmLanguage.json"))
      (contributedGrammar >>= jsonPath ["path"])
    configurationExists <- doesFileExist configurationPath
    grammarExists <- doesFileExist grammarPath
    assertEqual "language configuration exists" True configurationExists
    assertEqual "TextMate grammar exists" True grammarExists
    assertEqual
      "language configuration comment marker"
      (Just (String "#"))
      (jsonPath ["comments", "lineComment"] languageConfiguration)
    assertEqual "grammar root scope" (Just (String "source.jazz")) (jsonPath ["scopeName"] grammar)
    assertEqual
      "data declarations have a contextual grammar rule"
      True
      ("#data-declarations" `elem` rootGrammarIncludes)
    assertEqual
      "data declarations retain global reserved-keyword highlighting"
      True
      ("#keywords" `elem` dataDeclarationIncludes)
    assertEqual
      "data constructors have a distinct grammar scope"
      (Just (String "entity.name.function.constructor.jazz"))
      (constructorPattern >>= jsonPath ["captures", "2", "name"])
    assertEqual
      "value is globally highlighted as a reserved keyword"
      (Just (String "keyword.other.reserved.jazz"))
      (reservedValuePattern >>= jsonPath ["name"])
    assertEqual
      "exports are scoped to a module-header region"
      (Just (String "\\b(module)\\s+([A-Z][A-Za-z0-9_']*(?:::[A-Z][A-Za-z0-9_']*)*)\\s*(\\()"))
      exportRegionBegin
    assertEqual
      "grouped exports scope the exported type name"
      (Just (String "entity.name.type.jazz"))
      (groupedTypeExportPattern >>= jsonPath ["beginCaptures", "2", "name"])
    assertEqual
      "grouped exports scope selected constructors independently"
      (Just (String "entity.name.function.constructor.jazz"))
      (groupedConstructorPattern >>= jsonPath ["name"])
    assertEqual
      "export modifiers are nested inside the module-header region"
      True
      ( any
          ( \patternValue ->
              jsonPath ["name"] patternValue == Just (String "storage.modifier.export.jazz")
                && jsonPath ["match"] patternValue
                  == Just (String "\\b(?:value|constructor|type|class)\\b")
          )
          exportPatterns
      )
    assertEqual
      "operator grammar includes the Jazz bang operator symbol"
      True
      ( case operatorMatch of
          Just (String patternText) -> ":=@!]" `Text.isInfixOf` patternText
          _ -> False
      )
    assertEqual
      "pattern lambda introducer has the lambda scope"
      (Just (String "keyword.operator.lambda.jazz"))
      (patternLambdaIntroducer >>= jsonPath ["name"])
    assertEqual
      "pattern lambda introducer is matched before standalone operators"
      (Just (String "\\\\\\|"))
      (patternLambdaIntroducer >>= jsonPath ["match"])

testEditorFixtureParses :: IO ()
testEditorFixtureParses =
  withPackageRoot $ \packageRoot -> do
    let fixturePath =
          packageRoot
            </> "editors"
            </> "vscode-jazz"
            </> "fixtures"
            </> "representative.jz"
    source <- TextIO.readFile fixturePath
    case parseSurfaceProgram source of
      Left diagnostic ->
        failTest
          ( Text.pack fixturePath
              <> ": failed to parse: "
              <> renderDiagnostic diagnostic
          )
      Right _ -> pure ()
    forM_ requiredEditorSyntax $ \(family, spelling) ->
      unless (spelling `Text.isInfixOf` source) $
        failTest
          ( Text.pack fixturePath
              <> ": missing "
              <> family
              <> " syntax (`"
              <> spelling
              <> "`)"
          )

requiredEditorSyntax :: [(Text, Text)]
requiredEditorSyntax =
  [ ("comment", "#"),
    ("module declaration", "module"),
    ("import declaration", "import"),
    ("data declaration", "data"),
    ("class declaration", "class"),
    ("implementation declaration", "impl"),
    ("operator declaration", "operator"),
    ("operator precedence", "precedence"),
    ("right associativity", "right"),
    ("capability requirement", "@{"),
    ("type signature", "::"),
    ("lambda", "\\("),
    ("pattern lambda clauses", "\\|("),
    ("function arrow", "->"),
    ("case expression", "case"),
    ("conditional", "if"),
    ("then branch", "then"),
    ("else branch", "else"),
    ("character literal", "'\\n'"),
    ("text literal", "\"empty\""),
    ("Unicode escape", "\\u{"),
    ("numeric suffix", "i16"),
    ("purity marker", "compare!")
  ]

decodeJsonFile :: FilePath -> IO Value
decodeJsonFile path = do
  bytes <- ByteString.readFile path
  case eitherDecodeStrict' bytes of
    Left message ->
      failTest
        ( Text.pack path
            <> ": invalid JSON: "
            <> Text.pack message
        )
    Right value -> pure value

jsonPath :: [Text] -> Value -> Maybe Value
jsonPath keys value =
  case keys of
    [] -> Just value
    key : remaining ->
      case value of
        Object object -> KeyMap.lookup (Key.fromText key) object >>= jsonPath remaining
        _ -> Nothing

jsonArray :: Value -> [Value]
jsonArray value =
  case value of
    Array values -> toList values
    _ -> []

firstValue :: [Value] -> Maybe Value
firstValue values =
  case values of
    [] -> Nothing
    value : _ -> Just value

testPackageRoot :: IO ()
testPackageRoot =
  withPackageRoot $ \packageRoot -> do
    packageSource <- TextIO.readFile (packageRoot </> "jazz.cabal")
    unless ("name: jazz" `Text.isInfixOf` packageSource) $ do
      failTest "located package root does not contain the jazz package"

testCanonicalRepositoryLayout :: IO ()
testCanonicalRepositoryLayout =
  withPackageRoot $ \repositoryRoot -> do
    forM_ ["jazz-hs", "jazz2", "jazz" <> "-next"] $ \relativePath -> do
      exists <- doesDirectoryExist (repositoryRoot </> relativePath)
      assertEqual (Text.pack relativePath <> " is absent") False exists
    markerExists <- doesFileExist (repositoryRoot </> "jazz.cabal")
    assertEqual "jazz.cabal exists at the repository root" True markerExists
    forM_
      [ "app",
        "benchmark",
        "editors",
        "jazz",
        "program-support",
        "programs",
        "src",
        "test"
      ]
      $ \relativePath -> do
        exists <- doesDirectoryExist (repositoryRoot </> relativePath)
        assertEqual (Text.pack relativePath <> " exists at the repository root") True exists

testCheckedInJazzSources :: IO ()
testCheckedInJazzSources =
  withPackageRoot $ \packageRoot -> do
    (stdlibFormatViolations, stdlibModules) <-
      readSourceRole
        packageRoot
        StandardLibrarySource
        ("jazz" </> "stdlib")
    (compilerFormatViolations, compilerModules) <-
      readSourceRole
        packageRoot
        CompilerSource
        ("jazz" </> "compiler")
    let formatViolations =
          stdlibFormatViolations <> compilerFormatViolations
        layoutViolations =
          validateSourceLayering (stdlibModules <> compilerModules)
        renderedViolations =
          map renderJazzSourceFormatViolation formatViolations
            <> map renderSourceLayoutViolation layoutViolations
    unless (null renderedViolations) $ do
      failTest (Text.intercalate "\n" renderedViolations)

readSourceRole ::
  FilePath ->
  JazzSourceRole ->
  FilePath ->
  IO ([JazzSourceFormatViolation], [JazzSourceModule])
readSourceRole packageRoot role relativeDirectory = do
  let sourceRoot = packageRoot </> relativeDirectory
  exists <- doesDirectoryExist sourceRoot
  unless exists $ do
    failTest (Text.pack relativeDirectory <> ": source directory does not exist")
  paths <- listJazzFiles sourceRoot
  when (null paths) $ do
    failTest (Text.pack relativeDirectory <> ": contains no .jz files")
  results <- forM paths $ \path -> do
    source <- TextIO.readFile path
    let relativePath = makeRelative packageRoot path
        formatViolations = validateJazzModule relativePath source
    sourceModule <-
      case parseSurfaceProgram source of
        Left diagnostic ->
          failTest
            ( Text.pack relativePath
                <> ": failed to parse: "
                <> renderDiagnostic diagnostic
            )
        Right surfaceProgram ->
          pure (sourceModuleFromSurface role relativePath surfaceProgram)
    pure (formatViolations, sourceModule)
  pure (concatMap fst results, map snd results)

listJazzFiles :: FilePath -> IO [FilePath]
listJazzFiles root = sort <$> go root
  where
    go directory = do
      entries <- sort <$> listDirectory directory
      paths <- forM entries $ \entry -> do
        let path = directory </> entry
        isDirectory <- doesDirectoryExist path
        if isDirectory
          then go path
          else pure [path | takeExtension path == ".jz"]
      pure (concat paths)

testCheckedInPackagePolicy :: IO ()
testCheckedInPackagePolicy =
  withPackageRoot $ \packageRoot -> do
    packageSource <- TextIO.readFile (packageRoot </> "jazz.cabal")
    let violations = validatePackagePolicy packageSource
    unless (null violations) $ do
      failTest (Text.intercalate "\n" (map renderPackagePolicyViolation violations))

testDiagnosticRenderingBoundaries :: IO ()
testDiagnosticRenderingBoundaries = do
  assertEqual
    "tooling diagnostic rendering"
    "error: E5001: invalid command-line option"
    (renderDiagnostic (mkErrorDiagnostic E5001 ToolingOrigin "invalid command-line option"))
  assertEqual
    "source-signature rendering"
    "[Int] -> Text"
    (renderSignatureType (TypeFunction (TypeList TypeInt) TypeText))

testPerformanceDocumentation :: IO ()
testPerformanceDocumentation =
  withPackageRoot $ \packageRoot ->
    forM_
      [ "PERFORMANCE.md",
        "editors" </> "vscode-jazz" </> "README.md",
        "programs" </> "README.md",
        "programs" </> "corpus.json"
      ]
      (assertPackageFileExists packageRoot)

testCanonicalDocumentationPaths :: IO ()
testCanonicalDocumentationPaths =
  withPackageRoot $ \packageRoot -> do
    performance <- TextIO.readFile (packageRoot </> "PERFORMANCE.md")
    programDocumentation <- TextIO.readFile (packageRoot </> "programs" </> "README.md")
    editorDocumentation <- TextIO.readFile (packageRoot </> "editors" </> "vscode-jazz" </> "README.md")
    assertTextContains "performance root workflow" "assume the repository root" performance
    assertTextOmits "performance child-directory workflow" "cd jazz" performance
    assertTextOmits "performance child project directory" "--project-dir=jazz" performance
    assertTextContains "program manifest path" "`programs/corpus.json`" programDocumentation
    assertTextContains "program fixture path" "`test/fixtures/`" programDocumentation
    assertTextContains "program case root" "relative to `programs/`" programDocumentation
    assertTextOmits "nested program manifest path" "`jazz/programs/corpus.json`" programDocumentation
    assertTextOmits "nested program fixture path" "`jazz/test/fixtures/`" programDocumentation
    assertTextContains "editor extension path" "`editors/vscode-jazz`" editorDocumentation
    assertTextOmits "nested editor extension path" "`jazz/editors/vscode-jazz`" editorDocumentation

testCanonicalRepositoryInfrastructure :: IO ()
testCanonicalRepositoryInfrastructure =
  withPackageRoot $ \repositoryRoot -> do
    infrastructureSources <-
      forM infrastructurePaths $ \relativePath -> do
        source <- TextIO.readFile (repositoryRoot </> relativePath)
        pure (relativePath, source)
    forM_ infrastructureSources $ \(relativePath, source) ->
      forM_ obsoleteProductIdentities $ \obsoleteIdentity ->
        assertTextOmits
          (Text.pack relativePath <> " omits obsolete product identity " <> obsoleteIdentity)
          obsoleteIdentity
          source
    flakeSource <- TextIO.readFile (repositoryRoot </> "flake.nix")
    assertTextContains "root Nix package" "callCabal2nix \"jazz\" ./. { }" flakeSource
    assertTextContains "root Nix test check" "checks.jazz-test-suite" flakeSource
    assertTextOmits "release package remains deferred" "packages.default" flakeSource
    assertTextOmits "release app remains deferred" "apps.default" flakeSource
    ignoreSource <- TextIO.readFile (repositoryRoot </> ".gitignore")
    forM_
      [ "__pycache__/",
        "*.py[cod]",
        "dist-newstyle/",
        "dist-newstyle-profile-*/",
        "benchmark-results/",
        "profile-results/",
        "website/node_modules/",
        "website/build/"
      ]
      (\entry -> assertTextContains (Text.pack entry <> " is ignored") (Text.pack entry) ignoreSource)
    guidanceSource <- TextIO.readFile (repositoryRoot </> "AGENTS.md")
    forM_
      [ "commit along the way",
        "`src/`",
        "`jazz/`",
        "`app/`",
        "`test/`"
      ]
      (\guidance -> assertTextContains (guidance <> " guidance") guidance guidanceSource)
    cabalSource <- TextIO.readFile (repositoryRoot </> "jazz.cabal")
    assertTextContains "canonical Cabal package" "name: jazz" cabalSource
    assertTextContains "canonical private Cabal library" "library jazz-internal" cabalSource
    assertTextContains "canonical generated Cabal module" "Paths_jazz" cabalSource

infrastructurePaths :: [FilePath]
infrastructurePaths =
  [ "flake.nix",
    ".gitignore",
    "AGENTS.md",
    "jazz.cabal",
    "cabal.project",
    "cabal.project.profile-hotspots",
    "cabal.project.profile-stages",
    "scripts/check-docs.sh",
    "scripts/check-spec-authority.sh",
    "scripts/check-clarification-specs.sh",
    "scripts/check-execution-queue.py",
    "scripts/check-execution-queue.sh",
    "scripts/test-check-execution-queue.sh"
  ]

obsoleteProductIdentities :: [Text]
obsoleteProductIdentities =
  [ "jazz-next",
    "JazzNext",
    "Paths_jazz_next",
    "jazz-hs",
    "jazz2"
  ]

assertTextContains :: Text -> Text -> Text -> IO ()
assertTextContains description expected source =
  assertEqual description True (expected `Text.isInfixOf` source)

assertTextOmits :: Text -> Text -> Text -> IO ()
assertTextOmits description forbidden source =
  assertEqual description False (forbidden `Text.isInfixOf` source)

assertPackageFileExists :: FilePath -> FilePath -> IO ()
assertPackageFileExists packageRoot relativePath = do
  exists <- doesFileExist (packageRoot </> relativePath)
  assertEqual (Text.pack relativePath <> " exists") True exists

withPackageRoot :: (FilePath -> IO ()) -> IO ()
withPackageRoot action = do
  rootResult <- findJazzPackageRoot
  case rootResult of
    Left message -> failTest message
    Right packageRoot -> action packageRoot
