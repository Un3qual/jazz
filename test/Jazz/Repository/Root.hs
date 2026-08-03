{-# LANGUAGE OverloadedStrings #-}

module Jazz.Repository.Root
  ( findJazzPackageRoot
  )
where

import Data.Text (Text)
import qualified Data.Text as Text
import System.Directory (doesFileExist, getCurrentDirectory)
import System.FilePath ((</>), takeDirectory)

findJazzPackageRoot :: IO (Either Text FilePath)
findJazzPackageRoot = do
  currentDirectory <- getCurrentDirectory
  search currentDirectory (candidateRoots currentDirectory)
  where
    search startingDirectory candidates =
      case candidates of
        [] ->
          pure
            ( Left
                ( "could not locate jazz.cabal from "
                    <> Text.pack startingDirectory
                )
            )
        candidate : remaining -> do
          markerExists <- doesFileExist (candidate </> "jazz.cabal")
          if markerExists
            then pure (Right candidate)
            else search startingDirectory remaining

candidateRoots :: FilePath -> [FilePath]
candidateRoots = ancestors

ancestors :: FilePath -> [FilePath]
ancestors directory =
  let parent = takeDirectory directory
   in directory : if parent == directory then [] else ancestors parent
