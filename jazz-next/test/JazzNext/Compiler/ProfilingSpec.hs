{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Exception (IOException, evaluate, throw, try)
import Data.IORef
  ( IORef,
    modifyIORef',
    newIORef,
    readIORef,
  )
import Data.List (nub)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import JazzNext.Compiler.AST
  ( Expr (ELit),
    Literal (LInt),
    SignatureType (TypeList),
  )
import JazzNext.Compiler.Diagnostics (SourceSpan (SourceSpan))
import JazzNext.Compiler.Force (forceInferenceResult)
import JazzNext.Compiler.ModuleInterface (emptyModuleInterface)
import JazzNext.Compiler.Profiling
  ( CompilerStage (..),
    CompilerStageBoundary (..),
    compilerStageMarkerName,
    compilerStageName,
    withCompilerStageMarkers,
  )
import JazzNext.Compiler.RuntimeHints (BindingRuntimeHintKey (ExplicitTypeApplicationRuntimeHintKey))
import JazzNext.Compiler.TypeInference (InferenceResult (..))
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
    ("inference forcing evaluates nested runtime hints", testDeepInferenceForcing),
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

testDeepInferenceForcing :: IO ()
testDeepInferenceForcing = do
  let deferredFailure = throw (userError "nested runtime hint was forced")
      runtimeHintKey = ExplicitTypeApplicationRuntimeHintKey Nothing (SourceSpan 0 0)
      inference =
        InferenceResult
          { inferredExpr = ELit (LInt 0),
            inferredWarnings = [],
            inferredErrors = [],
            inferredRuntimeTypeHints = Map.singleton runtimeHintKey (TypeList deferredFailure),
            inferredModuleInterface = emptyModuleInterface
          }
  result <- try (evaluate (forceInferenceResult inference)) :: IO (Either IOException ())
  case result of
    Left _ -> pure ()
    Right () -> ioError (userError "forceInferenceResult left a nested runtime hint unevaluated")

testProfilingPresetsExist :: IO ()
testProfilingPresetsExist = do
  stagePreset <- doesFileExist "cabal.project.profile-stages"
  hotspotPreset <- doesFileExist "cabal.project.profile-hotspots"
  assertEqual "stage profiling preset" True stagePreset
  assertEqual "hotspot profiling preset" True hotspotPreset

recordMarker :: IORef [Text] -> Text -> IO ()
recordMarker markers marker =
  modifyIORef' markers (marker :)
