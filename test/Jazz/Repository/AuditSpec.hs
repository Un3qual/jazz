{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Exception (IOException, bracket, try)
import Control.Monad (forM, forM_, unless, when)
import Data.Aeson (Value (..), eitherDecodeStrict')
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as ByteString
import Data.Char (toLower)
import Data.Foldable (toList)
import Data.List (sort, stripPrefix)
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
import System.Directory
  ( createDirectoryIfMissing,
    doesDirectoryExist,
    doesFileExist,
    getTemporaryDirectory,
    listDirectory,
    removeFile,
    removePathForcibly,
  )
import System.Exit (ExitCode (..))
import System.FilePath (makeRelative, normalise, takeExtension, (</>))
import System.IO (hClose, openTempFile)
import System.Process (CreateProcess (cwd), proc, readCreateProcessWithExitCode)

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
    ("rejects incorrect canonical package metadata", testIncorrectPackageMetadata),
    ("rejects empty canonical package URLs", testEmptyPackageUrl),
    ("rejects legacy product identities in package metadata", testLegacyPackageIdentity),
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
    ("packages the complete public source distribution", testSourceDistributionInventory),
    ("ignores untracked files when auditing source-distribution completeness", testSourceDistributionIgnoresUntrackedFiles),
    ("falls back to scoped source-tree inventory without Git metadata", testSourceDistributionFallback),
    ("does not trust an unrelated parent Git repository", testSourceDistributionNestedRepositoryFallback),
    ("normalizes Windows-style Git inventory roots", testSourceDistributionWindowsRoots),
    ("normalizes Windows-style required source paths", testSourceDistributionWindowsRequiredPaths),
    ("rejects cross-platform generated source-distribution paths", testForbiddenSourceDistributionPaths),
    ("ignores generated VS Code extension packages", testVsixArtifactsIgnored),
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
        AuthoredSources.ExampleSource,
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
    "examples/functions/factorial.jz",
    "examples/hello.jz",
    "examples/modules/src/Example/Greeting.jz",
    "examples/modules/src/Example/Main.jz",
    "examples/patterns/result.jz",
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

validPackageMetadata :: Text
validPackageMetadata =
  """
  name: jazz
  synopsis: A statically typed functional language with practical syntax
  homepage: https://un3qual.github.io/jazz/
  bug-reports: https://github.com/un3qual/jazz/issues
  author: un3qual
  maintainer: un3qual
  category: Language
  stability: Experimental
  tested-with: GHC == 9.14.1
  license: GPL-3.0-only
  license-file: LICENSE

  source-repository head
    type: git
    location: https://github.com/un3qual/jazz.git
  """

validPrivatePackage :: Text
validPrivatePackage =
  validPackageMetadata
    <> "\n"
    <> """
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

testIncorrectPackageMetadata :: IO ()
testIncorrectPackageMetadata =
  assertEqual
    "example-domain homepage is rejected"
    True
    ( not
        ( null
            ( validatePackagePolicy
                ( Text.replace
                    "homepage: https://un3qual.github.io/jazz/"
                    "homepage: https://example.com/jazz"
                    validPrivatePackage
                )
            )
        )
    )

testEmptyPackageUrl :: IO ()
testEmptyPackageUrl =
  assertEqual
    "empty bug-report URL is rejected"
    True
    ( not
        ( null
            ( validatePackagePolicy
                ( Text.replace
                    "bug-reports: https://github.com/un3qual/jazz/issues"
                    "bug-reports:"
                    validPrivatePackage
                )
            )
        )
    )

testLegacyPackageIdentity :: IO ()
testLegacyPackageIdentity =
  assertEqual
    "legacy product identity is rejected"
    True
    ( not
        ( null
            ( validatePackagePolicy
                (validPrivatePackage <> "\ndescription: JazzNext compiler\n")
            )
        )
    )

testPublicLibraryPolicy :: IO ()
testPublicLibraryPolicy =
  assertEqual
    "public library policy"
    [PublicLibraryStanza, MissingPrivateLibraryStanza]
    ( validatePackagePolicy
        ( validPackageMetadata
            <> "\n"
            <> """
               library
                 exposed-modules: Public
               """
        )
    )

testNamedPublicLibraryPolicy :: IO ()
testNamedPublicLibraryPolicy =
  assertEqual
    "named public library policy"
    [PublicLibraryStanza]
    ( validatePackagePolicy
        ( validPrivatePackage
            <> "\n"
            <> """
               library jazz-api
                 visibility: public
               """
        )
    )

testMissingPrivateVisibility :: IO ()
testMissingPrivateVisibility =
  assertEqual
    "missing private visibility"
    [MissingPrivateLibraryVisibility]
    ( validatePackagePolicy
        ( validPackageMetadata
            <> "\n"
            <> """
               library jazz-internal
                 exposed-modules: Internal
               """
        )
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
        keywords = maybe [] jsonArray (jsonPath ["keywords"] manifest)
        packagedFiles = maybe [] jsonArray (jsonPath ["files"] manifest)
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
    assertEqual
      "manifest description reflects syntax-only scope"
      (Just (String "Syntax highlighting and editor configuration for the Jazz programming language."))
      (jsonPath ["description"] manifest)
    assertEqual
      "manifest repository type"
      (Just (String "git"))
      (jsonPath ["repository", "type"] manifest)
    assertEqual
      "manifest repository URL"
      (Just (String "https://github.com/un3qual/jazz.git"))
      (jsonPath ["repository", "url"] manifest)
    assertEqual
      "manifest homepage"
      (Just (String "https://un3qual.github.io/jazz/"))
      (jsonPath ["homepage"] manifest)
    assertEqual
      "manifest issue tracker"
      (Just (String "https://github.com/un3qual/jazz/issues"))
      (jsonPath ["bugs", "url"] manifest)
    assertEqual
      "manifest icon"
      (Just (String "icon.png"))
      (jsonPath ["icon"] manifest)
    assertEqual
      "manifest keywords"
      [ String "jazz",
        String "functional",
        String "programming-language",
        String "syntax-highlighting"
      ]
      keywords
    assertEqual
      "manifest package file allowlist"
      [ String "README.md",
        String "LICENSE",
        String "icon.png",
        String "language-configuration.json",
        String "syntaxes",
        String "fixtures"
      ]
      packagedFiles
    assertEqual "syntax-only extension has no runtime entrypoint" Nothing (jsonPath ["main"] manifest)
    assertEqual "syntax-only extension has no browser entrypoint" Nothing (jsonPath ["browser"] manifest)
    configurationExists <- doesFileExist configurationPath
    grammarExists <- doesFileExist grammarPath
    let iconPath = editorRoot </> "icon.png"
        licensePath = editorRoot </> "LICENSE"
    iconExists <- doesFileExist iconPath
    licenseExists <- doesFileExist licensePath
    assertEqual "language configuration exists" True configurationExists
    assertEqual "TextMate grammar exists" True grammarExists
    assertEqual "extension icon exists" True iconExists
    assertEqual "extension license exists" True licenseExists
    repositoryLicense <- ByteString.readFile (packageRoot </> "LICENSE")
    extensionLicense <- ByteString.readFile licensePath
    assertEqual "extension license matches repository license" repositoryLicense extensionLicense
    iconBytes <- ByteString.readFile iconPath
    let expectedPngHeader =
          ByteString.pack
            [ 137,
              80,
              78,
              71,
              13,
              10,
              26,
              10,
              0,
              0,
              0,
              13,
              73,
              72,
              68,
              82,
              0,
              0,
              0,
              128,
              0,
              0,
              0,
              128
            ]
    assertEqual
      "extension icon is a 128 by 128 PNG"
      expectedPngHeader
      (ByteString.take 24 iconBytes)
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

testSourceDistributionInventory :: IO ()
testSourceDistributionInventory =
  withPackageRoot $ \packageRoot -> do
    requiredDirectoryFiles <-
      listRepositoryFiles
        packageRoot
        [ "docs",
          "rfcs",
          "examples",
          "jazz",
          "programs",
          "editors" </> "vscode-jazz",
          "test" </> "fixtures" </> "runtime-observation"
        ]
    let requiredFiles =
          normalizeSourceDistributionPaths
            ( [ "README.md",
                "CHANGELOG.md",
                "CONTRIBUTING.md",
                "SECURITY.md",
                "RELEASING.md",
                "PERFORMANCE.md",
                ".gitignore",
                "LICENSE",
                "editors" </> "vscode-jazz" </> "LICENSE",
                "editors" </> "vscode-jazz" </> "icon.png"
              ]
                <> requiredDirectoryFiles
            )
        command = (proc "cabal" ["sdist", "--list-only", "all"]) {cwd = Just packageRoot}
    (exitCode, standardOutput, standardError) <- readCreateProcessWithExitCode command ""
    case exitCode of
      ExitFailure status ->
        failTest
          ( "cabal sdist --list-only failed with status "
              <> Text.pack (show status)
              <> ":\n"
              <> Text.pack standardError
          )
      ExitSuccess -> do
        let packagedFiles = sort (map normalizeSourceDistributionPath (lines standardOutput))
            missingFiles = filter (`notElem` packagedFiles) requiredFiles
            forbiddenFiles = filter isForbiddenSourceDistributionPath packagedFiles
        assertEqual "required source-distribution files" [] missingFiles
        assertEqual "forbidden source-distribution files" [] forbiddenFiles

testSourceDistributionIgnoresUntrackedFiles :: IO ()
testSourceDistributionIgnoresUntrackedFiles =
  withPackageRoot $ \packageRoot -> do
    actualGitRoot <- isActualGitRoot packageRoot
    when actualGitRoot $ do
      let editorRoot = packageRoot </> "editors" </> "vscode-jazz"
      bracket
        (createUniqueScratchFile editorRoot)
        removeFile
        (const testSourceDistributionInventory)

testSourceDistributionFallback :: IO ()
testSourceDistributionFallback =
  withTemporaryDirectory "jazz-sdist-fallback" $ \packageRoot -> do
    let documentationRoot = packageRoot </> "docs"
        editorRoot = packageRoot </> "editors" </> "vscode-jazz"
    createDirectoryIfMissing True documentationRoot
    createDirectoryIfMissing True editorRoot
    TextIO.writeFile (documentationRoot </> "contract.md") "public contract\n"
    TextIO.writeFile (editorRoot </> "package.json") "{}\n"
    TextIO.writeFile (editorRoot </> "generated.vsix") "generated package\n"
    TextIO.writeFile (editorRoot </> "render.tmp") "render scratch\n"
    files <-
      listRepositoryFiles
        packageRoot
        ["docs", "editors" </> "vscode-jazz"]
    assertEqual
      "fallback inventory contains scoped source files only"
      ["docs/contract.md", "editors/vscode-jazz/package.json"]
      (sort files)

testSourceDistributionNestedRepositoryFallback :: IO ()
testSourceDistributionNestedRepositoryFallback =
  withTemporaryDirectory "jazz-sdist-parent-repository" $ \parentRoot -> do
    let packageRoot = parentRoot </> "nested" </> "jazz"
        documentationRoot = packageRoot </> "docs"
    createDirectoryIfMissing True documentationRoot
    TextIO.writeFile (documentationRoot </> "contract.md") "public contract\n"
    runGit parentRoot ["init", "--quiet"]
    files <- listRepositoryFiles packageRoot ["docs"]
    assertEqual
      "nested package uses scoped fallback instead of its parent index"
      ["docs/contract.md"]
      files

testSourceDistributionWindowsRoots :: IO ()
testSourceDistributionWindowsRoots =
  withTemporaryDirectory "jazz-sdist-windows-roots" $ \packageRoot -> do
    let documentationRoot = packageRoot </> "docs" </> "language"
        documentationPath = documentationRoot </> "contract.md"
    createDirectoryIfMissing True documentationRoot
    TextIO.writeFile documentationPath "public contract\n"
    runGit packageRoot ["init", "--quiet"]
    runGit packageRoot ["add", "--", "docs/language/contract.md"]
    files <- listRepositoryFiles packageRoot ["docs\\language"]
    assertEqual
      "Git inventory normalizes Windows pathspec separators"
      ["docs/language/contract.md"]
      files

testSourceDistributionWindowsRequiredPaths :: IO ()
testSourceDistributionWindowsRequiredPaths =
  assertEqual
    "required source paths use archive separators"
    ["docs/language/contract.md", "editors/vscode-jazz/icon.png"]
    ( normalizeSourceDistributionPaths
        ["editors\\vscode-jazz\\icon.png", "docs/language/contract.md"]
    )

testForbiddenSourceDistributionPaths :: IO ()
testForbiddenSourceDistributionPaths = do
  assertEqual
    "forbidden paths recognize Windows separators and generated artifacts"
    [ True,
      True,
      True,
      True,
      True,
      False,
      False
    ]
    ( map
        isForbiddenSourceDistributionPath
        [ ".codex\\plans\\internal.md",
          "website\\build\\index.html",
          "dist-newstyle\\sdist\\jazz-0.1.0.0.tar.gz",
          "editors\\vscode-jazz\\jazz-language-0.1.0.vsix",
          "editors/vscode-jazz/Jazz-Language.VSIX",
          "website/build-not/generated.html",
          "docs/reference/output.md"
        ]
    )

testVsixArtifactsIgnored :: IO ()
testVsixArtifactsIgnored =
  withPackageRoot $ \packageRoot -> do
    ignoreSource <- TextIO.readFile (packageRoot </> ".gitignore")
    let ignoreRules =
          [ Text.strip line
          | line <- Text.lines ignoreSource,
            let stripped = Text.strip line,
            not (Text.null stripped),
            not ("#" `Text.isPrefixOf` stripped)
          ]
    assertEqual
      "generated .vsix files are ignored at every repository depth"
      True
      ("*.vsix" `elem` ignoreRules)

listRepositoryFiles :: FilePath -> [FilePath] -> IO [FilePath]
listRepositoryFiles packageRoot relativeRoots = do
  let normalizedRoots = map normalizeSourceDistributionPath relativeRoots
  actualGitRoot <- isActualGitRoot packageRoot
  if actualGitRoot
    then listTrackedRepositoryFiles packageRoot normalizedRoots
    else listScopedSourceTreeFiles packageRoot normalizedRoots

isActualGitRoot :: FilePath -> IO Bool
isActualGitRoot packageRoot = do
  let rootCommand = (proc "git" ["rev-parse", "--show-toplevel"]) {cwd = Just packageRoot}
  rootResult <-
    try (readCreateProcessWithExitCode rootCommand "") :: IO (Either IOException (ExitCode, String, String))
  pure $ case rootResult of
    Right (ExitSuccess, standardOutput, _) ->
      normalise (Text.unpack (Text.strip (Text.pack standardOutput))) == normalise packageRoot
    _ -> False

listTrackedRepositoryFiles :: FilePath -> [FilePath] -> IO [FilePath]
listTrackedRepositoryFiles packageRoot relativeRoots = do
  let command =
        (proc "git" (["ls-files", "-z", "--"] <> relativeRoots))
          { cwd = Just packageRoot
          }
  gitResult <-
    try (readCreateProcessWithExitCode command "") :: IO (Either IOException (ExitCode, String, String))
  case gitResult of
    Right (ExitSuccess, standardOutput, _) -> do
      let trackedPaths =
            map
              normalizeSourceDistributionPath
              (filter (not . null) (splitOn '\0' standardOutput))
      existingPaths <-
        forM trackedPaths $ \relativePath -> do
          exists <- doesFileExist (packageRoot </> relativePath)
          pure [relativePath | exists]
      pure (concat existingPaths)
    Left _ -> listScopedSourceTreeFiles packageRoot relativeRoots
    Right (ExitFailure _, _, _) -> listScopedSourceTreeFiles packageRoot relativeRoots

listScopedSourceTreeFiles :: FilePath -> [FilePath] -> IO [FilePath]
listScopedSourceTreeFiles packageRoot relativeRoots =
  concat <$> mapM (go . (packageRoot </>)) relativeRoots
  where
    go directory = do
      entries <- sort <$> listDirectory directory
      concat
        <$> forM
          entries
          ( \entry -> do
              let path = directory </> entry
                  relativePath =
                    normalizeSourceDistributionPath (makeRelative packageRoot path)
              isDirectory <- doesDirectoryExist path
              if isForbiddenSourceDistributionPath relativePath
                then pure []
                else
                  if isDirectory
                    then go path
                    else pure [relativePath]
          )

normalizeSourceDistributionPath :: FilePath -> FilePath
normalizeSourceDistributionPath = dropCurrentDirectory . map normalizeSeparator
  where
    normalizeSeparator '\\' = '/'
    normalizeSeparator character = character
    dropCurrentDirectory path =
      case stripPrefix "./" path of
        Just normalized -> dropCurrentDirectory normalized
        Nothing -> path

normalizeSourceDistributionPaths :: [FilePath] -> [FilePath]
normalizeSourceDistributionPaths =
  Set.toAscList . Set.fromList . map normalizeSourceDistributionPath

isForbiddenSourceDistributionPath :: FilePath -> Bool
isForbiddenSourceDistributionPath path =
  hasForbiddenComponentPrefix components
    || map toLower (takeExtension normalizedPath) `elem` [".vsix", ".tmp"]
  where
    normalizedPath = normalizeSourceDistributionPath path
    components = map (map toLower) (filter (`notElem` ["", "."]) (splitOn '/' normalizedPath))

hasForbiddenComponentPrefix :: [FilePath] -> Bool
hasForbiddenComponentPrefix components =
  any (`isComponentPrefixOf` components) forbiddenPrefixes
    || case components of
      firstComponent : _ ->
        "dist-newstyle-profile-" `isPathPrefixOf` firstComponent
      [] -> False
  where
    forbiddenPrefixes =
      [ [".codex"],
        ["website", "build"],
        ["website", "node_modules"],
        ["website", ".docusaurus"],
        ["benchmark-results"],
        ["profile-results"],
        ["dist-newstyle"]
      ]

isComponentPrefixOf :: [FilePath] -> [FilePath] -> Bool
isComponentPrefixOf prefix components =
  take (length prefix) components == prefix

isPathPrefixOf :: FilePath -> FilePath -> Bool
isPathPrefixOf prefix path =
  case stripPrefix prefix path of
    Just _ -> True
    Nothing -> False

splitOn :: (Eq value) => value -> [value] -> [[value]]
splitOn separator values =
  case break (== separator) values of
    (segment, []) -> [segment]
    (segment, _ : remaining) -> segment : splitOn separator remaining

withTemporaryDirectory :: FilePath -> (FilePath -> IO result) -> IO result
withTemporaryDirectory prefix action =
  bracket acquire removePathForcibly action
  where
    acquire = do
      temporaryRoot <- getTemporaryDirectory
      (path, handle) <- openTempFile temporaryRoot prefix
      hClose handle
      removeFile path
      createDirectoryIfMissing True path
      pure path

createUniqueScratchFile :: FilePath -> IO FilePath
createUniqueScratchFile directory = do
  (path, handle) <- openTempFile directory "source-distribution-audit-scratch-"
  hClose handle
  TextIO.writeFile path "not part of the repository\n"
  pure path

runGit :: FilePath -> [String] -> IO ()
runGit workingDirectory arguments = do
  let command = (proc "git" arguments) {cwd = Just workingDirectory}
  (exitCode, _, standardError) <- readCreateProcessWithExitCode command ""
  case exitCode of
    ExitSuccess -> pure ()
    ExitFailure status ->
      failTest
        ( "git command failed with status "
            <> Text.pack (show status)
            <> ":\n"
            <> Text.pack standardError
        )

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
    assertTextContains "filtered Nix package source" "jazzSource = pkgs.lib.fileset.toSource" flakeSource
    assertTextContains "root Nix package" "callCabal2nix \"jazz\" jazzSource { }" flakeSource
    assertTextContains "root Nix package owns its check tools" "overrideCabal jazzBase" flakeSource
    assertTextContains "root Nix test check shares the package derivation" "checks.jazz-test-suite = jazz" flakeSource
    assertTextContains "release package is exported" "default = jazz" flakeSource
    assertTextContains "release app is exported" "apps.default" flakeSource
    assertTextContains "Nix test check owns test tool dependencies" "testToolDepends" flakeSource
    assertTextContains "Nix test check provides cabal-install" "pkgs.cabal-install" flakeSource
    assertTextContains "Nix test check provides Git" "pkgs.git" flakeSource
    assertTextContains "Nix test check creates a writable home" "mkdir -p \"$HOME\"" flakeSource
    assertTextContains "Nix test check exports its writable home" "export HOME=\"$TMPDIR/home\"" flakeSource
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
    assertTextContains "checked example source inventory" "examples/**/*.jz" cabalSource

infrastructurePaths :: [FilePath]
infrastructurePaths =
  [ "flake.nix",
    ".gitignore",
    "AGENTS.md",
    "jazz.cabal",
    "cabal.project",
    "cabal.project.profile-hotspots",
    "cabal.project.profile-stages",
    "scripts/check-examples.sh",
    "scripts/check-docs.sh",
    "scripts/check-spec-authority.sh",
    "scripts/check-clarification-specs.sh",
    "scripts/test-check-clarification-specs.sh",
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
