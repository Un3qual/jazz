{-# LANGUAGE OverloadedStrings #-}

module JazzNext.TestSource
  ( JazzSourceRole (..),
    checkedInJazzSourcePath,
    readCheckedInJazzSource,
  )
where

import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import JazzNext.Repository.Root (findJazzNextPackageRoot)
import JazzNext.Repository.SourceLayout (JazzSourceRole (..))
import System.Directory (doesFileExist)
import System.FilePath ((</>))

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
