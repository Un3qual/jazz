{-# LANGUAGE OverloadedStrings #-}

module JazzNext.TestSource
  ( JazzSourceRole (..),
    checkedInJazzSourcePath,
    readCheckedInJazzSource,
    readCheckedInJazzModuleSource,
    readCheckedInJazzProjectModuleSource,
    readCheckedInJazzTestFixture,
  )
where

import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import JazzNext.Repository.Root (findJazzNextPackageRoot)
import JazzNext.Repository.SourceLayout (JazzSourceRole (..))
import System.Directory (doesFileExist)
import System.FilePath
  ( isAbsolute,
    joinPath,
    normalise,
    splitDirectories,
    (</>)
  )

checkedInJazzSourcePath :: FilePath -> JazzSourceRole -> FilePath -> FilePath
checkedInJazzSourcePath packageRoot role fileName =
  packageRoot </> "jazz" </> roleDirectory role </> fileName
  where
    roleDirectory StandardLibrarySource = "stdlib"
    roleDirectory CompilerSource = "compiler"

readCheckedInJazzSource :: JazzSourceRole -> FilePath -> IO Text.Text
readCheckedInJazzSource role fileName = do
  rootResult <- findJazzNextPackageRoot
  packageRoot <-
    case rootResult of
      Left message -> ioError (userError (Text.unpack message))
      Right root -> pure root
  let path = checkedInJazzSourcePath packageRoot role fileName
  exists <- doesFileExist path
  if exists
    then TextIO.readFile path
    else
      ioError
        ( userError
            ( "could not find checked-in "
                <> roleLabel role
                <> " Jazz source at "
                <> path
            )
        )
  where
    roleLabel StandardLibrarySource = "standard-library"
    roleLabel CompilerSource = "compiler"

readCheckedInJazzModuleSource :: JazzSourceRole -> FilePath -> IO (Maybe Text.Text)
readCheckedInJazzModuleSource role sourcePath =
  case safeSourceRelativePath sourcePath of
    Nothing -> pure Nothing
    Just relativePath -> do
      rootResult <- findJazzNextPackageRoot
      packageRoot <-
        case rootResult of
          Left message -> ioError (userError (Text.unpack message))
          Right root -> pure root
      let path = checkedInJazzSourcePath packageRoot role relativePath
      exists <- doesFileExist path
      if exists
        then Just <$> TextIO.readFile path
        else pure Nothing

readCheckedInJazzProjectModuleSource :: FilePath -> IO (Maybe Text.Text)
readCheckedInJazzProjectModuleSource sourcePath = do
  compilerSource <- readCheckedInJazzModuleSource CompilerSource sourcePath
  case compilerSource of
    Just source -> pure (Just source)
    Nothing -> readCheckedInJazzModuleSource StandardLibrarySource sourcePath

readCheckedInJazzTestFixture :: FilePath -> IO Text.Text
readCheckedInJazzTestFixture relativePath = do
  safeRelativePath <-
    case safeRelativeFixturePath relativePath of
      Nothing -> ioError (userError ("invalid checked-in Jazz fixture path: " <> relativePath))
      Just path -> pure path
  rootResult <- findJazzNextPackageRoot
  packageRoot <-
    case rootResult of
      Left message -> ioError (userError (Text.unpack message))
      Right root -> pure root
  let path = packageRoot </> "test" </> "fixtures" </> safeRelativePath
  exists <- doesFileExist path
  if exists
    then TextIO.readFile path
    else ioError (userError ("could not find checked-in Jazz fixture at " <> path))

safeSourceRelativePath :: FilePath -> Maybe FilePath
safeSourceRelativePath sourcePath
  | isAbsolute sourcePath = Nothing
  | otherwise =
      case splitDirectories (normalise sourcePath) of
        "src" : relativeParts
          | validRelativeParts relativeParts -> Just (joinPath relativeParts)
        _ -> Nothing

safeRelativeFixturePath :: FilePath -> Maybe FilePath
safeRelativeFixturePath relativePath
  | isAbsolute relativePath = Nothing
  | otherwise =
      let parts = splitDirectories (normalise relativePath)
       in if validRelativeParts parts then Just (joinPath parts) else Nothing

validRelativeParts :: [FilePath] -> Bool
validRelativeParts parts =
  not (null parts)
    && all (\part -> part /= "." && part /= ".." && not (null part)) parts
