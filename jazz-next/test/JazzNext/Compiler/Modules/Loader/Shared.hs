{-# LANGUAGE OverloadedStrings #-}

module JazzNext.Compiler.Modules.Loader.Shared
  ( moduleGraphProjectedSources,
    lookupSourceIn,
    resolverConfig
  ) where


import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.IORef
  ( newIORef,
    readIORef,
    writeIORef
  )
import Data.Text (Text)
import qualified Data.Text as Text
import JazzNext.Compiler.Diagnostics
  ( renderDiagnostic
  )
import JazzNext.Compiler.Driver
  ( CompileResult (..),
    ResolvedPrelude (..),
    RunResult (..),
    compileModuleGraph,
    compileModuleGraphWithResolvedPrelude,
    compileModuleGraphWithPrelude,
    runModuleGraph,
    runModuleGraphWithResolvedPrelude,
    runModuleGraphWithPrelude
  )
import JazzNext.Compiler.ModuleResolver
  ( ModuleResolutionConfig (..)
  )
import JazzNext.Compiler.WarningConfig
  ( defaultWarningSettings
  )
import JazzNext.TestHarness
  ( NamedTest,
    assertContains,
    assertEqual,
    failTest,
    runTestSuite
  )

moduleGraphProjectedSources :: Text -> Map.Map FilePath Text
moduleGraphProjectedSources projectedExpr =
  Map.fromList
    [ ("src/App/Main.jz", "module App::Main {\nimport Lib::Data.\nprojected.\n}"),
      ("src/Lib/Data.jz", "module Lib::Data {\nvalues = [[1, 2], [3]].\nprojected = " <> projectedExpr <> ".\n}")
    ]

lookupSourceIn :: Map.Map FilePath Text -> FilePath -> IO (Maybe Text)
lookupSourceIn sourceMap path = pure (Map.lookup path sourceMap)

resolverConfig :: ModuleResolutionConfig
resolverConfig =
  ModuleResolutionConfig
    { moduleRoots = ["src"],
      moduleExtension = ".jz"
    }
