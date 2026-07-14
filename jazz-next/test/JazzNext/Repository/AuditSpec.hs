{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (forM, forM_, unless, when)
import Data.Aeson (Value (..), eitherDecodeStrict')
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as ByteString
import Data.Foldable (toList)
import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import JazzNext.Compiler.Diagnostics (renderDiagnostic)
import JazzNext.Compiler.Parser (parseSurfaceProgram)
import JazzNext.Repository.PackagePolicy
  ( PackagePolicyViolation (..),
    renderPackagePolicyViolation,
    validatePackagePolicy,
  )
import JazzNext.Repository.Root (findJazzNextPackageRoot)
import JazzNext.Repository.SourceLayout
  ( JazzSourceModule,
    JazzSourceRole (..),
    SourceLayoutViolation (..),
    renderSourceLayoutViolation,
    sourceModuleFromSurface,
    validateSourceLayering,
  )
import JazzNext.Repository.JazzSourceFormat
  ( JazzSourceFormatViolation (..),
    renderJazzSourceFormatViolation,
    validateJazzModule,
  )
import JazzNext.TestHarness
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
  [ ("accepts a valid Jazz source module", testValidJazzModule),
    ("rejects a missing module header", testMissingModuleHeader),
    ("rejects a missing final closing brace", testMissingClosingBrace),
    ("rejects blank lines after the final closing brace", testTrailingBlankLines),
    ("rejects odd or shallow body indentation", testBodyIndentation),
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
    ("locates the active jazz-next package root", testPackageRoot),
    ("validates all checked-in Jazz source modules", testCheckedInJazzSources),
    ("validates the checked-in Cabal package policy", testCheckedInPackagePolicy)
  ]

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
  library jazz-next-internal
    visibility: private
  """

testValidJazzModule :: IO ()
testValidJazzModule =
  assertEqual
    "valid Jazz source violations"
    []
    (validateJazzModule "jazz/stdlib/Good.jz" validJazzSource)

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
        library jazz-next-internal
          visibility: private

        library jazz-next-api
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
        library jazz-next-internal
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
        constructorPattern = firstValue dataDeclarationPatterns
        operatorPatterns =
          maybe
            []
            jsonArray
            (jsonPath ["repository", "operators", "patterns"] grammar)
        operatorPattern = firstValue operatorPatterns
        operatorMatch = operatorPattern >>= jsonPath ["match"]
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
      "data constructors have a distinct grammar scope"
      (Just (String "entity.name.function.constructor.jazz"))
      (constructorPattern >>= jsonPath ["captures", "2", "name"])
    assertEqual
      "operator grammar includes the Jazz bang operator symbol"
      True
      (case operatorMatch of
        Just (String patternText) -> ":=@!]" `Text.isInfixOf` patternText
        _ -> False)

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
    packageSource <- TextIO.readFile (packageRoot </> "jazz-next.cabal")
    unless ("name: jazz-next" `Text.isInfixOf` packageSource) $ do
      failTest "located package root does not contain the jazz-next package"

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
    packageSource <- TextIO.readFile (packageRoot </> "jazz-next.cabal")
    let violations = validatePackagePolicy packageSource
    unless (null violations) $ do
      failTest (Text.intercalate "\n" (map renderPackagePolicyViolation violations))

withPackageRoot :: (FilePath -> IO ()) -> IO ()
withPackageRoot action = do
  rootResult <- findJazzNextPackageRoot
  case rootResult of
    Left message -> failTest message
    Right packageRoot -> action packageRoot
