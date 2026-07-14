{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Exception (IOException, try)
import Data.IORef
  ( IORef,
    modifyIORef',
    newIORef,
    readIORef,
  )
import Data.List (nub)
import Data.Text (Text)
import qualified Data.Text as Text
import JazzNext.Compiler.Profiling
  ( CompilerStage (..),
    CompilerStageBoundary (..),
    compilerStageMarkerName,
    compilerStageName,
    withCompilerStageMarkers,
  )
import JazzNext.TestHarness
  ( NamedTest,
    assertEqual,
    runTestSuite,
  )
import System.Directory (doesFileExist)

main :: IO ()
main = runTestSuite "ProfilingSpec" tests

tests :: [NamedTest]
tests =
  [ ("compiler stage names are stable, non-empty, and unique", testCompilerStageNames),
    ("compiler stage markers pair around successful actions", testSuccessfulStageMarkers),
    ("compiler stage markers pair around failed actions", testFailedStageMarkers),
    ("GHC profiling presets are checked in separately", testProfilingPresetsExist)
  ]

testCompilerStageNames :: IO ()
testCompilerStageNames = do
  let stages = [minBound .. maxBound] :: [CompilerStage]
      names = map compilerStageName stages
  assertEqual "stage names are non-empty" True (all (not . Text.null) names)
  assertEqual "stage names are unique" (length names) (length (nub names))

testSuccessfulStageMarkers :: IO ()
testSuccessfulStageMarkers = do
  markers <- newIORef []
  result <-
    withCompilerStageMarkers
      (recordMarker markers)
      ParsingStage
      (pure (42 :: Int))
  recorded <- reverse <$> readIORef markers
  assertEqual "profiled action result" 42 result
  assertEqual
    "successful marker pair"
    [ compilerStageMarkerName CompilerStageBegin ParsingStage,
      compilerStageMarkerName CompilerStageEnd ParsingStage
    ]
    recorded

testFailedStageMarkers :: IO ()
testFailedStageMarkers = do
  markers <- newIORef []
  result <-
    try
      ( withCompilerStageMarkers
          (recordMarker markers)
          EvaluationStage
          (ioError (userError "profiled stage failure"))
      ) ::
      IO (Either IOException ())
  recorded <- reverse <$> readIORef markers
  assertEqual "profiled action failed" True (either (const True) (const False) result)
  assertEqual
    "failed marker pair"
    [ compilerStageMarkerName CompilerStageBegin EvaluationStage,
      compilerStageMarkerName CompilerStageEnd EvaluationStage
    ]
    recorded

testProfilingPresetsExist :: IO ()
testProfilingPresetsExist = do
  stagePreset <- doesFileExist "cabal.project.profile-stages"
  hotspotPreset <- doesFileExist "cabal.project.profile-hotspots"
  assertEqual "stage profiling preset" True stagePreset
  assertEqual "hotspot profiling preset" True hotspotPreset

recordMarker :: IORef [Text] -> Text -> IO ()
recordMarker markers marker =
  modifyIORef' markers (marker :)
