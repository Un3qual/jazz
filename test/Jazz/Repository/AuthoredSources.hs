{-# LANGUAGE OverloadedStrings #-}

module Jazz.Repository.AuthoredSources
  ( AuthoredSource (..),
    AuthoredSourceRole (..),
    readAuthoredSources,
  )
where

import Control.Monad (forM)
import Data.Bifunctor (first)
import Data.List (isPrefixOf, sort, sortBy)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Jazz.Compiler.BundledPrelude (bundledPreludeIdentity)
import Jazz.Compiler.Diagnostics (Diagnostic)
import Jazz.Compiler.Diagnostics.Render (renderDiagnostic)
import Jazz.Compiler.ModuleExports (exportInventory)
import Jazz.Compiler.ModuleGraph (CoreModule (..), PreludeArtifact (..), ResolvedModuleFacts (..), coreModulePath, coreProgramModules)
import Jazz.Compiler.ModuleResolver (ModuleResolutionConfig (..), importedOperators, resolveProgramWithAmbientExports)
import Jazz.Compiler.ModuleResolver.Imports (importScopeAliases)
import Jazz.Compiler.Parser (parseSurfaceProgram, parseSurfaceProgramTokensWithContextDetailed)
import Jazz.Compiler.Parser.AST (SurfaceExpr)
import Jazz.Compiler.Parser.Context (ParserContext (..), initialParserContext)
import Jazz.Compiler.Parser.Failure (parserFailureDiagnostic)
import Jazz.Compiler.Parser.Lexer (tokenize)
import Jazz.Compiler.Parser.Operator (operatorTableFromDeclarations)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath (dropExtension, makeRelative, splitDirectories, takeExtension, (</>))

data AuthoredSourceRole
  = StandardLibrarySource
  | ExampleSource
  | ProgramSource
  | EditorFixtureSource
  deriving (Eq, Ord, Show)

data AuthoredSource = AuthoredSource
  { authoredRelativePath :: FilePath,
    authoredRole :: AuthoredSourceRole,
    authoredText :: Text,
    authoredSurface :: SurfaceExpr
  }

readAuthoredSources :: FilePath -> IO [AuthoredSource]
readAuthoredSources packageRoot =
  fmap
    (sortOnRelativePath . concat)
    ( mapM
        (uncurry (readSourceRoot packageRoot))
        [ (StandardLibrarySource, "jazz" </> "stdlib"),
          (ExampleSource, "examples"),
          (ProgramSource, "programs"),
          (EditorFixtureSource, "editors" </> "vscode-jazz" </> "fixtures")
        ]
    )

readSourceRoot :: FilePath -> AuthoredSourceRole -> FilePath -> IO [AuthoredSource]
readSourceRoot packageRoot role relativeRoot = do
  let sourceRoot = packageRoot </> relativeRoot
  exists <- doesDirectoryExist sourceRoot
  if not exists
    then fail (relativeRoot <> ": source directory does not exist")
    else do
      paths <- listJazzFiles sourceRoot
      forM paths $ \path -> do
        source <- TextIO.readFile path
        let relativePath = makeRelative packageRoot path
        parsed <-
          if "examples/modules/src/" `isPrefixOf` relativePath
            then parseModuleExample packageRoot relativePath source
            else pure (parseSurfaceProgram source)
        case parsed of
          Left diagnostic ->
            fail
              ( Text.unpack
                  ( Text.pack relativePath
                      <> ": failed to parse: "
                      <> renderDiagnostic diagnostic
                  )
              )
          Right surface ->
            pure
              AuthoredSource
                { authoredRelativePath = relativePath,
                  authoredRole = role,
                  authoredText = source,
                  authoredSurface = surface
                }

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

sortOnRelativePath :: [AuthoredSource] -> [AuthoredSource]
sortOnRelativePath =
  sortBy
    (\left right -> compare (authoredRelativePath left) (authoredRelativePath right))

-- Module examples use the same dependency-selected operator environment as the
-- compiler. Standalone corpus files retain the standalone parser entrypoint.
parseModuleExample :: FilePath -> FilePath -> Text -> IO (Either Diagnostic SurfaceExpr)
parseModuleExample packageRoot relativePath source = do
  let root = packageRoot </> "examples/modules/src"
      entry = map Text.pack (splitDirectories (dropExtension (makeRelative "examples/modules/src" relativePath)))
      load path = do
        exists <- doesFileExist path
        if exists then Just <$> TextIO.readFile path else pure Nothing
  resolved <-
    resolveProgramWithAmbientExports
      (ModuleResolutionConfig [root, packageRoot </> "jazz/stdlib"] ".jz")
      (PreludeArtifact bundledPreludeIdentity Nothing)
      (exportInventory [])
      load
      entry
  pure $ do
    program <- resolved
    let modules = coreProgramModules program
        facts = coreModuleFacts (NonEmpty.last modules)
        dependencies = Map.fromList [(coreModulePath coreModule, coreModuleFacts coreModule) | coreModule <- NonEmpty.toList modules]
        scope = resolvedModuleImportScope facts
        context =
          initialParserContext
            { parserKnownAliases = Map.keysSet (importScopeAliases scope),
              parserDeclaredOperators = operatorTableFromDeclarations (importedOperators scope dependencies)
            }
    tokens <- tokenize source
    first parserFailureDiagnostic (fst <$> parseSurfaceProgramTokensWithContextDetailed context tokens)
