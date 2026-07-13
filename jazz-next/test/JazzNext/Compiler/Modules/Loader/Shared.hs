{-# LANGUAGE OverloadedStrings #-}

module JazzNext.Compiler.Modules.Loader.Shared
  ( moduleGraphProjectedSources,
    lookupSourceIn,
    resolverConfig
  ) where

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import JazzNext.Compiler.ModuleResolver
  ( ModuleResolutionConfig (..)
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
