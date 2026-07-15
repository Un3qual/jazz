{-# LANGUAGE OverloadedStrings #-}

module JazzNext.Compiler.Stdlib.Shared
  ( runStdlibFixture,
    runStdlibSource,
    runStdlibSourceObserved,
  )
where

import Data.Text (Text)
import qualified Data.Text as Text
import JazzNext.Compiler.Driver
  ( RunResult,
    runModuleGraph,
    runModuleGraphObserved,
  )
import JazzNext.Compiler.ModuleResolver
  ( ModuleResolutionConfig (..),
  )
import JazzNext.Compiler.Runtime.Observation
  ( RuntimeObservationRequest,
  )
import JazzNext.Compiler.WarningConfig
  ( defaultWarningSettings,
  )
import JazzNext.Repository.SourceLayout
  ( JazzSourceRole (StandardLibrarySource),
  )
import JazzNext.TestSource
  ( readCheckedInJazzModuleSource,
    readCheckedInJazzTestFixture,
  )

runStdlibFixture :: [Text] -> FilePath -> IO RunResult
runStdlibFixture modulePath fixturePath = do
  source <- readCheckedInJazzTestFixture fixturePath
  runStdlibSource modulePath source

runStdlibSource :: [Text] -> Text -> IO RunResult
runStdlibSource modulePath entrySource =
  runModuleGraph
    defaultWarningSettings
    resolverConfig
    modulePath
    lookupSource
  where
    entryPath = "src/" <> modulePathFile modulePath <> ".jz"

    lookupSource path
      | path == entryPath = pure (Just entrySource)
      | otherwise = readCheckedInJazzModuleSource StandardLibrarySource path

runStdlibSourceObserved :: RuntimeObservationRequest -> [Text] -> Text -> IO RunResult
runStdlibSourceObserved observationRequest modulePath entrySource =
  runModuleGraphObserved
    observationRequest
    defaultWarningSettings
    resolverConfig
    modulePath
    lookupSource
  where
    entryPath = "src/" <> modulePathFile modulePath <> ".jz"

    lookupSource path
      | path == entryPath = pure (Just entrySource)
      | otherwise = readCheckedInJazzModuleSource StandardLibrarySource path

resolverConfig :: ModuleResolutionConfig
resolverConfig =
  ModuleResolutionConfig
    { moduleRoots = ["src"],
      moduleExtension = ".jz"
    }

modulePathFile :: [Text] -> FilePath
modulePathFile =
  foldr1 (\segment suffix -> segment <> "/" <> suffix) . map Text.unpack
