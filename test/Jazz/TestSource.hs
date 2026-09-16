{-# LANGUAGE OverloadedStrings #-}

module Jazz.TestSource
  ( checkedInJazzSourcePath,
    readCheckedInJazzSource,
    readCheckedInJazzModuleSource,
    readCheckedInJazzTestFixture,
  )
where

import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Jazz.Repository.Root (findJazzPackageRoot)
import System.Directory (doesFileExist)
import System.FilePath
  ( isAbsolute,
    joinPath,
    normalise,
    splitDirectories,
    (</>),
  )

checkedInJazzSourcePath :: FilePath -> FilePath -> FilePath
checkedInJazzSourcePath packageRoot fileName =
  packageRoot </> "jazz" </> "stdlib" </> fileName

readCheckedInJazzSource :: FilePath -> IO Text.Text
readCheckedInJazzSource fileName = do
  rootResult <- findJazzPackageRoot
  packageRoot <-
    case rootResult of
      Left message -> ioError (userError (Text.unpack message))
      Right root -> pure root
  let path = checkedInJazzSourcePath packageRoot fileName
  exists <- doesFileExist path
  if exists
    then TextIO.readFile path
    else
      ioError
        ( userError
            ( "could not find checked-in standard-library Jazz source at "
                <> path
            )
        )

readCheckedInJazzModuleSource :: FilePath -> IO (Maybe Text.Text)
readCheckedInJazzModuleSource sourcePath =
  case safeSourceRelativePath sourcePath of
    Nothing -> pure Nothing
    Just relativePath -> do
      rootResult <- findJazzPackageRoot
      packageRoot <-
        case rootResult of
          Left message -> ioError (userError (Text.unpack message))
          Right root -> pure root
      let path = checkedInJazzSourcePath packageRoot relativePath
      exists <- doesFileExist path
      if exists
        then Just <$> TextIO.readFile path
        else pure Nothing

readCheckedInJazzTestFixture :: FilePath -> IO Text.Text
readCheckedInJazzTestFixture relativePath = do
  safeRelativePath <-
    case safeRelativeFixturePath relativePath of
      Nothing -> ioError (userError ("invalid checked-in Jazz fixture path: " <> relativePath))
      Just path -> pure path
  rootResult <- findJazzPackageRoot
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
