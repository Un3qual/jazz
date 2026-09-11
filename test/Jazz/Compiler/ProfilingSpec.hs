{-# LANGUAGE DataKinds #-}
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
import Jazz.Compiler.AST
  ( CoreNode (..),
    CoreNodeId (..),
    CorePhase (Resolved),
    Expr (ELit),
    Literal (LInt),
  )
import Jazz.Compiler.CoreIdentity (emptyResolvedNodeFacts)
import Jazz.Compiler.DiagnosticCatalog (ErrorCode (E1001))
import Jazz.Compiler.Diagnostics
  ( DiagnosticOrigin (CompilationOrigin),
    SourceSpan (SourceSpan, SourceSpanIn),
    appendDiagnosticSecondaryLabel,
    mkErrorDiagnostic,
    setDiagnosticPrimaryLabel,
  )
import Jazz.Compiler.Force
  ( forceDiagnostic,
    forceInferenceResult,
    forceRuntimeProgramOutputResult,
  )
import Jazz.Compiler.ModuleExports
  ( ModuleExport (ModuleExport),
  )
import Jazz.Compiler.ModuleIdentity (SourceUnitOwner (StandaloneSourceUnit), standaloneModulePath)
import Jazz.Compiler.ModuleInterface
  ( ModuleInterface (..),
    emptyModuleInterface,
  )
import Jazz.Compiler.ModuleRuntime
  ( RuntimeExport (RuntimeBindingExport),
    RuntimeModule (RuntimeModule),
    RuntimeProgram (RuntimeProgram),
  )
import Jazz.Compiler.Name
  ( NameNamespace (ConstructorNamespace, TypeNamespace, ValueNamespace),
    mkIdentifier,
    resolvedLocalName,
  )
import Jazz.Compiler.Profiling
  ( BenchmarkGroup (..),
    CompilerStage (..),
    CompilerStageBoundary (..),
    benchmarkGroupName,
    benchmarkGroupStages,
    compilerStageMarkerName,
    compilerStageName,
    withCompilerStageMarkers,
  )
import Jazz.Compiler.Runtime.Types (RuntimeValue (VConstructor))
import Jazz.Compiler.TypeInference.Result (InferenceResult (..))
import Jazz.Compiler.TypeInference.Types
  ( ClassMethodType (ClassMethodType),
    ConstructorArgumentType (ConstructorArgumentMonomorphic),
    DataTypeBinding (DataTypeBinding),
    ImplMethodType (ImplMethodType),
    SemanticType (..),
    TypeBinding (PlainTypeBinding),
  )
import Jazz.Compiler.TypeRepresentation
  ( SignaturePayload (..),
    SignatureType (..),
  )
import Jazz.TestHarness
  ( NamedTest,
    assertEqual,
    runTestSuite,
  )
import System.Directory (doesFileExist)

main :: IO ()
main = runTestSuite "ProfilingSpec" tests

tests :: [NamedTest]
tests =
  [ ("benchmark group names and stage mappings are exact", testBenchmarkGroupMetadata),
    ("compiler stage names are stable, non-empty, and unique", testCompilerStageNames),
    ("compiler stage markers pair around successful actions", testSuccessfulStageMarkers),
    ("compiler stage markers pair around failed actions", testFailedStageMarkers),
    ("inference forcing evaluates nested module interface payloads", testDeepModuleInterfaceForcing),
    ("diagnostic forcing evaluates nested spans and labels", testDeepDiagnosticForcing),
    ("runtime-result forcing follows rendered-output semantics", testRuntimeResultForcingFollowsRendering),
    ("GHC profiling presets are checked in separately", testProfilingPresetsExist)
  ]

testBenchmarkGroupMetadata :: IO ()
testBenchmarkGroupMetadata = do
  let groups = [minBound .. maxBound] :: [BenchmarkGroup]
  assertEqual
    "benchmark group names"
    [ "parse-lower",
      "analysis",
      "diagnostic-analysis",
      "module-preparation",
      "runtime",
      "whole-program"
    ]
    (map benchmarkGroupName groups)
  assertEqual
    "benchmark group stage mappings"
    [ (ParseLowerBenchmark, [LexingStage, ParsingStage, LoweringStage]),
      (AnalysisBenchmark, [StaticAnalysisStage, TypeInferenceStage, ConstraintSolvingStage, CapabilitySolvingStage]),
      (DiagnosticAnalysisBenchmark, [StaticAnalysisStage]),
      (ModulePreparationBenchmark, [SourceLoadingStage, ModuleDiscoveryStage, ModuleResolutionStage, RuntimePreparationStage]),
      (RuntimeBenchmark, [EvaluationStage, HostOperationStage]),
      ( WholeProgramBenchmark,
        [ SourceLoadingStage,
          ModuleDiscoveryStage,
          LexingStage,
          ParsingStage,
          LoweringStage,
          ModuleResolutionStage,
          StaticAnalysisStage,
          TypeInferenceStage,
          ConstraintSolvingStage,
          CapabilitySolvingStage,
          RuntimePreparationStage,
          EvaluationStage,
          HostOperationStage
        ]
      )
    ]
    [(group, benchmarkGroupStages group) | group <- groups]

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

testDeepModuleInterfaceForcing :: IO ()
testDeepModuleInterfaceForcing =
  mapM_
    assertInterfaceForced
    [ ( "value type",
        "nested expression type was forced",
        emptyModuleInterface
          { interfaceValueTypes =
              Map.singleton
                (ModuleExport ValueNamespace "value")
                (PlainTypeBinding (SemanticList deferredExpressionType))
          }
      ),
      ( "data type",
        "nested expression type was forced",
        emptyModuleInterface
          { interfaceDataTypes =
              Map.singleton
                "Container"
                (DataTypeBinding [] [[ConstructorArgumentMonomorphic (SemanticList deferredExpressionType)]])
          }
      ),
      ( "class method",
        "nested signature type was forced",
        emptyModuleInterface
          { interfaceClassMethods =
              Map.singleton
                "method"
                (ClassMethodType "Capability" (SignatureType (TypeList deferredSignatureType)))
          }
      ),
      ( "impl method",
        "nested signature type was forced",
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
    assertInterfaceForced (label, marker, interface) = do
      let inference =
            InferenceResult
              { inferredExpr = resolvedZero,
                inferredDiagnostics = [],
                inferredModuleInterface = interface
              }
      assertForcesMarker (label <> " payload") marker (evaluate (forceInferenceResult inference))

testDeepDiagnosticForcing :: IO ()
testDeepDiagnosticForcing =
  mapM_
    (\(label, marker, diagnostic) -> assertForcesMarker label marker (evaluate (forceDiagnostic diagnostic)))
    [ ( "primary label span",
        "diagnostic primary label span was forced",
        setDiagnosticPrimaryLabel
          (SourceSpanIn (throw (userError "diagnostic primary label span was forced")) 1 1)
          "primary"
          baseDiagnostic
      ),
      ( "secondary label message",
        "diagnostic secondary label message was forced",
        appendDiagnosticSecondaryLabel
          (SourceSpan 2 3)
          (throw (userError "diagnostic secondary label message was forced"))
          baseDiagnostic
      )
    ]
  where
    baseDiagnostic = mkErrorDiagnostic E1001 CompilationOrigin "diagnostic"

assertForcesMarker :: Text -> String -> IO () -> IO ()
assertForcesMarker label marker action = do
  result <- try action :: IO (Either IOException ())
  case result of
    Left exception
      | Text.pack marker `Text.isInfixOf` Text.pack (show exception) -> pure ()
      | otherwise -> throw exception
    Right () -> ioError (userError (Text.unpack (label <> " stayed lazy")))

resolvedZero :: Expr 'Resolved
resolvedZero = ELit (CoreNode (CoreNodeId 0) (SourceSpan 1 1) (emptyResolvedNodeFacts (StandaloneSourceUnit standaloneModulePath))) (LInt 0)

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
                  (resolvedLocalName TypeNamespace (mkIdentifier "Container"))
                  []
                  (resolvedLocalName ConstructorNamespace (mkIdentifier "Partial"))
                  [SemanticInt, SemanticInt]
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
