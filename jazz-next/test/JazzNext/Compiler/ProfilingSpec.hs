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
  ( DataConstructorArgument (DataConstructorArgumentOpaque),
    Expr (ELit),
    Literal (LInt),
    SignaturePayload (SignatureType),
    SignatureType (TypeList),
  )
import JazzNext.Compiler.Diagnostics (SourceSpan (SourceSpan))
import JazzNext.Compiler.Force
  ( forceInferenceResult,
    forceRuntimeProgramOutputResult,
  )
import JazzNext.Compiler.ModuleExports (ModuleExport (ModuleExport))
import JazzNext.Compiler.ModuleInterface
  ( ModuleInterface (..),
    emptyModuleInterface,
  )
import JazzNext.Compiler.ModuleRuntime
  ( RuntimeExport (RuntimeBindingExport),
    RuntimeModule (RuntimeModule),
    RuntimeProgram (RuntimeProgram),
  )
import JazzNext.Compiler.Name (NameNamespace (ValueNamespace))
import JazzNext.Compiler.Profiling
  ( CompilerStage (..),
    CompilerStageBoundary (..),
    compilerStageMarkerName,
    compilerStageName,
    withCompilerStageMarkers,
  )
import JazzNext.Compiler.Runtime.Types (RuntimeValue (VConstructor))
import JazzNext.Compiler.RuntimeHints (BindingRuntimeHintKey (ExplicitTypeApplicationRuntimeHintKey))
import JazzNext.Compiler.TypeInference (InferenceResult (..))
import JazzNext.Compiler.TypeInference.Types
  ( ClassMethodType (ClassMethodType),
    ConstructorArgumentType (ConstructorArgumentMonomorphic),
    DataTypeBinding (DataTypeBinding),
    ExpressionType (TListType),
    ImplMethodType (ImplMethodType),
    TypeBinding (PlainTypeBinding),
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
    ("inference forcing evaluates nested runtime hints", testDeepInferenceForcing),
    ("inference forcing evaluates nested module interface payloads", testDeepModuleInterfaceForcing),
    ("runtime-result forcing follows rendered-output semantics", testRuntimeResultForcingFollowsRendering),
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

testDeepModuleInterfaceForcing :: IO ()
testDeepModuleInterfaceForcing =
  mapM_
    assertInterfaceForced
    [ ( "value type",
        emptyModuleInterface
          { interfaceValueTypes =
              Map.singleton
                (ModuleExport ValueNamespace "value")
                (PlainTypeBinding (TListType deferredExpressionType))
          }
      ),
      ( "data type",
        emptyModuleInterface
          { interfaceDataTypes =
              Map.singleton
                "Container"
                (DataTypeBinding [] [[ConstructorArgumentMonomorphic (TListType deferredExpressionType)]])
          }
      ),
      ( "class method",
        emptyModuleInterface
          { interfaceClassMethods =
              Map.singleton
                "method"
                (ClassMethodType "Capability" (SignatureType (TypeList deferredSignatureType)))
          }
      ),
      ( "impl method",
        emptyModuleInterface
          { interfaceConcreteImplMethods =
              Map.singleton
                "Capability::method"
                [ImplMethodType (TypeList deferredSignatureType)]
          }
      )
    ]
  where
    deferredExpressionType = throw (userError "nested expression type was forced")
    deferredSignatureType = throw (userError "nested signature type was forced")
    assertInterfaceForced (label, interface) = do
      let inference =
            InferenceResult
              { inferredExpr = ELit (LInt 0),
                inferredWarnings = [],
                inferredErrors = [],
                inferredRuntimeTypeHints = Map.empty,
                inferredModuleInterface = interface
              }
      result <- try (evaluate (forceInferenceResult inference)) :: IO (Either IOException ())
      case result of
        Left _ -> pure ()
        Right () -> ioError (userError (Text.unpack (label <> " payload stayed lazy")))

testRuntimeResultForcingFollowsRendering :: IO ()
testRuntimeResultForcingFollowsRendering = do
  let unusedExport = throw (userError "unused runtime export was forced")
      unrenderedPartialArgument = throw (userError "unrendered partial-constructor argument was forced")
      runtimeProgram =
        RuntimeProgram
          [ RuntimeModule
              ["Lib"]
              ( Map.singleton
                  (RuntimeBindingExport (ModuleExport ValueNamespace "unused"))
                  unusedExport
              )
          ]
          ( Just
              ( VConstructor
                  "Container"
                  []
                  "Partial"
                  [DataConstructorArgumentOpaque, DataConstructorArgumentOpaque]
                  [unrenderedPartialArgument]
              )
          )
  result <- try (evaluate (forceRuntimeProgramOutputResult (Right runtimeProgram))) :: IO (Either IOException ())
  case result of
    Left exception -> throw exception
    Right () -> pure ()

testProfilingPresetsExist :: IO ()
testProfilingPresetsExist = do
  stagePreset <- doesFileExist "cabal.project.profile-stages"
  hotspotPreset <- doesFileExist "cabal.project.profile-hotspots"
  assertEqual "stage profiling preset" True stagePreset
  assertEqual "hotspot profiling preset" True hotspotPreset

recordMarker :: IORef [Text] -> Text -> IO ()
recordMarker markers marker =
  modifyIORef' markers (marker :)
