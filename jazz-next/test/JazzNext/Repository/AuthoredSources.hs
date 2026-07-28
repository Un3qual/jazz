{-# LANGUAGE OverloadedStrings #-}

module JazzNext.Repository.AuthoredSources
  ( AuthoredSource (..),
    AuthoredSourceRole (..),
    readAuthoredSources,
  )
where

import Control.Monad (forM)
import Data.List (sort, sortBy)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import JazzNext.Compiler.Diagnostics.Render (renderDiagnostic)
import JazzNext.Compiler.Parser (parseSurfaceProgram)
import JazzNext.Compiler.Parser.AST (SurfaceExpr)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (makeRelative, takeExtension, (</>))

data AuthoredSourceRole
  = StandardLibrarySource
  | CompilerSource
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
  fmap (sortOnRelativePath . concat)
    ( mapM
        (uncurry (readSourceRoot packageRoot))
        [ (StandardLibrarySource, "jazz" </> "stdlib"),
          (CompilerSource, "jazz" </> "compiler"),
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
        case parseSurfaceProgram source of
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
