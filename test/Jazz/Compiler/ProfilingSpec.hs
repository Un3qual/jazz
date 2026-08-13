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
  ( Expr (ELit),
    Literal (LInt),
    SignaturePayload (SignatureType),
    SignatureType (TypeInt, TypeList),
  )
import Jazz.Compiler.DiagnosticCatalog (ErrorCode (E1001))
import Jazz.Compiler.Diagnostics
  ( DiagnosticOrigin (CompilationOrigin),
    SourceSpan (SourceSpan),
    mkErrorDiagnostic,
  )
import Jazz.Compiler.Force
  ( forceCompiledModule,
    forceCompiledProgram,
    forceInferenceResult,
    forceLoweredProgram,
    forceResolvedModule,
    forceRuntimeProgramOutputResult,
    forceTypedProgram,
  )
import Jazz.Compiler.LoweredIR
  ( LoweredBlock (LoweredBlock),
    LoweredBlockId (LoweredBlockId),
    LoweredFunction (LoweredFunction),
    LoweredFunctionId (LoweredFunctionId),
    LoweredInstruction (LoweredInstruction),
    LoweredLayout (LoweredLayout),
    LoweredLayoutId (LoweredLayoutId),
    LoweredLayoutShape (LoweredTextLayout),
    LoweredOperand (LoweredTemporaryOperand),
    LoweredOperation (LoweredConstructText),
    LoweredProgram (LoweredProgram),
    LoweredRepresentation (LoweredManagedReferenceRepresentation),
    LoweredTemporaryId (LoweredTemporaryId),
    LoweredTerminator (LoweredReturn),
    supportedLoweredIRVersion,
  )
import Jazz.Compiler.LoweredIR.Validate (validateLoweredProgram)
import Jazz.Compiler.ModuleExports
  ( ModuleExport (ModuleExport),
    exportInventory,
  )
import Jazz.Compiler.ModuleInterface
  ( CompiledModule (..),
    CompiledPrelude (..),
    CompiledProgram (..),
    ModuleInterface (..),
    emptyCompiledPrelude,
    emptyModuleInterface,
  )
import Jazz.Compiler.ModuleGraph
  ( CoreModule (..),
    ResolvedModule (..),
  )
import Jazz.Compiler.ModuleRuntime
  ( RuntimeExport (RuntimeBindingExport),
    RuntimeModule (RuntimeModule),
    RuntimeProgram (RuntimeProgram),
  )
import Jazz.Compiler.Name (NameNamespace (ValueNamespace))
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
import Jazz.Compiler.RuntimeHints (BindingRuntimeHintKey (ExplicitTypeApplicationRuntimeHintKey))
import Jazz.Compiler.TypeInference (InferenceResult (..))
import Jazz.Compiler.TypeInference.Types
  ( ClassMethodType (ClassMethodType),
    ConstructorArgumentType (ConstructorArgumentMonomorphic),
    DataTypeBinding (DataTypeBinding),
    ExpressionType (TListType),
    ImplMethodType (ImplMethodType),
    TypeBinding (PlainTypeBinding),
  )
import qualified Jazz.Compiler.TypedCore as Typed
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
    ("inference forcing evaluates nested runtime hints", testDeepInferenceForcing),
    ("inference forcing evaluates nested module interface payloads", testDeepModuleInterfaceForcing),
    ("compiled-module forcing evaluates compact runtime metadata", testDeepCompiledModuleForcing),
    ("compiled-program forcing owns prelude diagnostics", testDeepCompiledProgramForcing),
    ("resolved modules remain lazy at production WHNF", testResolvedModuleProductionLaziness),
    ("resolved-module forcing evaluates setup-owned content", testDeepResolvedModuleForcing),
    ("lowered-program forcing evaluates payloads validation does not inspect", testDeepLoweredProgramForcing),
    ("typed-program forcing evaluates nested artifact payloads", testDeepTypedProgramForcing),
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
      "typed-validation",
      "lowered-validation",
      "typed-lowering",
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
      (TypedValidationBenchmark, [TypedCoreValidationStage]),
      (LoweredValidationBenchmark, [LoweredIRValidationStage]),
      (TypedLoweringBenchmark, [TypedCoreValidationStage, LoweringStage]),
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

testDeepInferenceForcing :: IO ()
testDeepInferenceForcing = do
  let marker = "nested runtime hint was forced"
      deferredFailure = throw (userError marker)
      runtimeHintKey = ExplicitTypeApplicationRuntimeHintKey Nothing (SourceSpan 0 0)
      inference =
        InferenceResult
          { inferredExpr = ELit (LInt 0),
            inferredDiagnostics = [],
            inferredRuntimeTypeHints = Map.singleton runtimeHintKey (TypeList deferredFailure),
            inferredModuleInterface = emptyModuleInterface
          }
  assertForcesMarker "nested runtime hint" marker (evaluate (forceInferenceResult inference))

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
                (PlainTypeBinding (TListType deferredExpressionType))
          }
      ),
      ( "data type",
        "nested expression type was forced",
        emptyModuleInterface
          { interfaceDataTypes =
              Map.singleton
                "Container"
                (DataTypeBinding [] [[ConstructorArgumentMonomorphic (TListType deferredExpressionType)]])
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
              { inferredExpr = ELit (LInt 0),
                inferredDiagnostics = [],
                inferredRuntimeTypeHints = Map.empty,
                inferredModuleInterface = interface
              }
      assertForcesMarker (label <> " payload") marker (evaluate (forceInferenceResult inference))

testDeepCompiledModuleForcing :: IO ()
testDeepCompiledModuleForcing =
  mapM_
    assertCompiledMetadataForced
    [ ( "imports",
        "compiled imports were forced",
        baseCompiledModule
          { compiledModuleImports = throw (userError "compiled imports were forced")
          }
      ),
      ( "diagnostics",
        "compiled diagnostics were forced",
        baseCompiledModule
          { compiledModuleDiagnostics =
              [ mkErrorDiagnostic
                  E1001
                  CompilationOrigin
                  (throw (userError "compiled diagnostics were forced"))
              ]
          }
      ),
      ( "export inventory",
        "compiled export inventory was forced",
        baseCompiledModule
          { compiledModuleExportInventory = throw (userError "compiled export inventory was forced")
          }
      )
    ]
  where
    baseCompiledModule =
      CompiledModule
        { compiledModulePath = ["App", "Main"],
          compiledModuleImports = [],
          compiledModuleExportInventory = exportInventory [],
          compiledModuleInterface = emptyModuleInterface,
          compiledModuleDiagnostics = [],
          compiledModuleExpr = ELit (LInt 0)
        }
    assertCompiledMetadataForced (label, marker, compiledModule) =
      assertForcesMarker label marker (evaluate (forceCompiledModule compiledModule))

testDeepCompiledProgramForcing :: IO ()
testDeepCompiledProgramForcing = do
  let marker = "compiled prelude diagnostics were forced"
      compiledPrelude =
        emptyCompiledPrelude
          { compiledPreludeDiagnostics =
              [mkErrorDiagnostic E1001 CompilationOrigin (throw (userError marker))]
          }
      compiledProgram =
        CompiledProgram
          { compiledProgramPrelude = compiledPrelude,
            compiledProgramEntryPath = ["App", "Main"],
            compiledProgramModules = []
          }
  assertForcesMarker
    "compiled prelude diagnostic"
    marker
    (evaluate (forceCompiledProgram compiledProgram))

testResolvedModuleProductionLaziness :: IO ()
testResolvedModuleProductionLaziness = do
  let resolvedModule =
        baseResolvedModule
          { resolvedModuleCore = throw (userError "production forced the resolved Core module")
          }
  result <- try (evaluate resolvedModule) :: IO (Either IOException ResolvedModule)
  case result of
    Left exception -> throw exception
    Right _ -> pure ()

testDeepResolvedModuleForcing :: IO ()
testDeepResolvedModuleForcing =
  mapM_
    assertResolvedContentForced
    [ ( "module path",
        "resolved module path was forced",
        baseResolvedModule
          { resolvedModulePath = ["App", throw (userError "resolved module path was forced")]
          }
      ),
      ( "source path",
        "resolved source path was forced",
        baseResolvedModule
          { resolvedSourcePath = "App/" <> throw (userError "resolved source path was forced")
          }
      ),
      ( "imports",
        "resolved import was forced",
        baseResolvedModule
          { resolvedModuleImports = [throw (userError "resolved import was forced")]
          }
      ),
      ( "export inventory",
        "resolved export inventory was forced",
        baseResolvedModule
          { resolvedModuleExportInventory = throw (userError "resolved export inventory was forced")
          }
      ),
      ( "Core expression",
        "resolved Core expression was forced",
        baseResolvedModule
          { resolvedModuleCore =
              (resolvedModuleCore baseResolvedModule)
                { coreModuleExpr = throw (userError "resolved Core expression was forced")
                }
          }
      )
    ]
  where
    assertResolvedContentForced (label, marker, resolvedModule) =
      assertForcesMarker label marker (evaluate (forceResolvedModule resolvedModule))

testDeepLoweredProgramForcing :: IO ()
testDeepLoweredProgramForcing = do
  let marker = "lowered text payload was forced"
      textLayoutId = LoweredLayoutId "text"
      textRepresentation = LoweredManagedReferenceRepresentation textLayoutId
      functionId = LoweredFunctionId "main"
      blockId = LoweredBlockId "entry"
      temporaryId = LoweredTemporaryId "text-value"
      loweredProgram =
        LoweredProgram
          supportedLoweredIRVersion
          [LoweredLayout textLayoutId LoweredTextLayout]
          []
          [ LoweredFunction
              functionId
              Nothing
              []
              textRepresentation
              [ LoweredBlock
                  blockId
                  []
                  [ LoweredInstruction
                      temporaryId
                      textRepresentation
                      (LoweredConstructText textLayoutId (throw (userError marker)))
                  ]
                  (Just (LoweredReturn (LoweredTemporaryOperand temporaryId textRepresentation)))
              ]
              blockId
          ]
          functionId
  assertEqual
    "poisoned lowered program remains structurally valid"
    []
    (validateLoweredProgram loweredProgram)
  assertForcesMarker "lowered text payload" marker (evaluate (forceLoweredProgram loweredProgram))

testDeepTypedProgramForcing :: IO ()
testDeepTypedProgramForcing =
  mapM_ assertTypedPayloadForced typedPayloadCases
  where
    assertTypedPayloadForced (label, marker, typedProgram) =
      assertForcesMarker label marker (evaluate (forceTypedProgram typedProgram))
    typedPayloadCases =
      [ ( "typed source path",
          "typed source path was forced",
          typedProgramWith
            (throw (userError "typed source path was forced"))
            baseExpression
        ),
        ( "typed literal payload",
          "typed literal payload was forced",
          typedProgramWith
            (Typed.TypedSourcePath "Main.jz")
            ( Typed.TypedLiteralExpr
                boolInfo
                (throw (userError "typed literal payload was forced"))
            )
        )
      ]
    typedProgramWith sourcePath expression =
      Typed.TypedProgram
        Nothing
        [ Typed.TypedModule
            ["Main"]
            sourcePath
            []
            []
            (Typed.TypedModuleInterface [] [] [] [])
            []
            [Typed.TypedExpressionStatement (Typed.TypedSpan 1 1) expression]
            boolInfo
        ]
        ["Main"]
    baseExpression = Typed.TypedLiteralExpr boolInfo (Typed.TypedBooleanLiteral True)
    boolInfo = Typed.TypedNodeInfo Typed.TypedBoolType Typed.TypedBoolRecipe [] []

assertForcesMarker :: Text -> String -> IO () -> IO ()
assertForcesMarker label marker action = do
  result <- try action :: IO (Either IOException ())
  case result of
    Left exception
      | Text.pack marker `Text.isInfixOf` Text.pack (show exception) -> pure ()
      | otherwise -> throw exception
    Right () -> ioError (userError (Text.unpack (label <> " stayed lazy")))

baseResolvedModule :: ResolvedModule
baseResolvedModule =
  ResolvedModule
    { resolvedModulePath = ["App", "Main"],
      resolvedSourcePath = "App/Main.jazz",
      resolvedModuleImports = [],
      resolvedModuleExportInventory = exportInventory [],
      resolvedModuleCore = CoreModule Nothing Nothing [] (ELit (LInt 0))
    }

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
                  [TypeInt, TypeInt]
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
