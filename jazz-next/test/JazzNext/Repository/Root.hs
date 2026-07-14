{-# LANGUAGE OverloadedStrings #-}

module JazzNext.Repository.Root
  ( findJazzNextPackageRoot
  )
where

import Data.Text (Text)
import qualified Data.Text as Text
import System.Directory (doesFileExist, getCurrentDirectory)
import System.FilePath ((</>), takeDirectory)

findJazzNextPackageRoot :: IO (Either Text FilePath)
findJazzNextPackageRoot = do
  currentDirectory <- getCurrentDirectory
  search currentDirectory (candidateRoots currentDirectory)
  where
    search startingDirectory candidates =
      case candidates of
        [] ->
          pure
            ( Left
                ( "could not locate jazz-next.cabal from "
                    <> Text.pack startingDirectory
                )
            )
        candidate : remaining -> do
          markerExists <- doesFileExist (candidate </> "jazz-next.cabal")
          if markerExists
            then pure (Right candidate)
            else search startingDirectory remaining

candidateRoots :: FilePath -> [FilePath]
candidateRoots currentDirectory =
  concatMap (\ancestor -> [ancestor, ancestor </> "jazz-next"]) (ancestors currentDirectory)

ancestors :: FilePath -> [FilePath]
ancestors directory =
  let parent = takeDirectory directory
   in directory : if parent == directory then [] else ancestors parent
