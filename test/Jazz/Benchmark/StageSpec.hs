{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.DeepSeq (NFData (rnf))
import Control.Exception (IOException, evaluate, throw, try)
import Control.Monad (void)
import Data.Text (Text)
import qualified Data.Text as Text
import Jazz.Benchmark.ScaleCases
  ( CompilerScaleCase,
    CompilerScaleScenario (..),
    compilerScaleCaseBenchmarks,
    compilerScaleCaseEntrySource,
    compilerScaleCaseExpectedOutput,
    compilerScaleCaseIdentifier,
    compilerScaleCaseInterfaceWidth,
    compilerScaleCaseScenario,
    compilerScaleCaseSize,
    compilerScaleCaseSourceCount,
    compilerScaleCases,
    selectCompilerScaleCases,
  )
import Jazz.Benchmark.StageInputs
  ( PreparedBenchmark (PreparedAnalysis),
    PreparedCompilerScaleBenchmark (PreparedCompilerScaleAnalysis),
    prepareBenchmark,
    prepareCompilerScaleBenchmark,
    runCompilerScaleCase,
    runPreparedBenchmark,
    runPreparedCompilerScaleBenchmark,
    selectProgramCases,
  )
import Jazz.Compiler.AST (Expr (EList))
import Jazz.Compiler.Diagnostics (SourceSpan (SourceSpan))
import Jazz.Compiler.ModuleExports
  ( ModuleExport (ModuleExport),
    exportInventory,
  )
import Jazz.Compiler.ModuleGraph
  ( CoreModule (..),
    DeclaredModuleExports (..),
    ResolvedImport (..),
    ResolvedModule (..),
  )
import Jazz.Benchmark.Stages
  ( BenchmarkCommand (benchmarkCommandSelectedCases, benchmarkCommandSelectedScaleCases),
    parseBenchmarkCommand,
  )
import Jazz.Compiler.Name (NameNamespace (ValueNamespace), identifierText)
import Jazz.Compiler.Parser (parseSurfaceProgram)
import Jazz.Compiler.Parser.AST
  ( SurfaceCaseArm (..),
    SurfaceExpr (..),
    SurfaceLiteral (..),
    SurfacePattern (..),
    SurfaceStatement (..),
  )
import Jazz.Compiler.Parser.Lexer (tokenize)
import Jazz.Compiler.Profiling (BenchmarkGroup (..))
import Jazz.ProgramCorpus.Manifest (loadProgramCorpus, programCaseById, renderProgramCorpusViolation)
import Jazz.ProgramCorpus.Types (ProgramCase (..), ProgramCorpus (..))
import Jazz.TestHarness (NamedTest, assertEqual, failTest, runTestSuite)

main :: IO ()
main = runTestSuite "BenchmarkStages" tests

tests :: [NamedTest]
tests =
  [ ("registers stable generated scale identities and sizes", testCompilerScaleRegistry),
    ("selects generated scale cases before stage preparation", testCompilerScaleSelection),
    ("parses opt-in generated scale case selectors", testCompilerScaleCommandSelection),
    ("rejects ambiguous generated scale case selectors", testCompilerScaleCommandRejection),
    ("sequential polymorphism preserves exact compiler semantics", testSequentialPolymorphismSemantics),
    ("wide module fanout preserves exact compiler semantics", testWideModuleFanoutSemantics),
    ("resolver fact-rich modules preserve exact compiler semantics", testResolverFactRichSemantics),
    ("typed validation handoff lowers exact valid programs", testTypedValidationHandoffSemantics),
    ("lowered temporary validation scale cases have exact metadata", testLoweredTemporaryValidationRegistry),
    ("smallest lowered temporary validation executes prepared validation", testLoweredTemporaryValidationSmallestCase),
    ("typed recursive statement graph scale cases have exact metadata", testTypedRecursiveStatementGraphRegistry),
    ("smallest typed recursive statement graph executes prepared validation", testTypedRecursiveStatementGraphSmallestCase),
    ("typed forward signed function scale cases have exact metadata", testTypedForwardSignedFunctionsRegistry),
    ("smallest typed forward signed function case executes prepared validation", testTypedForwardSignedFunctionsSmallestCase),
    ("typed wide export provider scale cases have exact metadata", testTypedWideExportProvidersRegistry),
    ("smallest typed wide export provider case executes prepared validation", testTypedWideExportProvidersSmallestCase),
    ("wide constructor scale cases preserve currying and field order", testWideConstructorApplicationSemantics),
    ("capability candidate width scale cases have exact metadata", testCapabilityCandidateWidthRegistry),
    ("smallest capability candidate width case preserves real and prepared semantics", testCapabilityCandidateWidthSemantics),
    ("host-free opaque environments preserve exact runtime output", testHostFreeOpaqueEnvironmentSemantics),
    ("analyzer diagnostic chains preserve exact error counts", testAnalyzerDiagnosticChainSemantics),
    ("interleaved recursive groups preserve exact compiler semantics", testInterleavedRecursiveGroupSemantics),
    ("recursive preview bursts preserve exact compiler semantics", testRecursivePreviewBurstSemantics),
    ("runtime evidence compiler scale families are registered", testRuntimeEvidenceScaleFamilies),
    ("smallest runtime evidence cases execute through prepared and real pipelines", testRuntimeEvidenceSmallestCases),
    ("same-name rebinding bursts preserve exact compiler semantics", testRecursiveRebindingBurstSemantics),
    ("constrained signatures preserve exact compiler semantics", testConstrainedSignatureSemantics),
    ("deferred constraint bursts preserve exact compiler semantics", testDeferredConstraintBurstSemantics),
    ("deep nested lambdas preserve exact compiler semantics", testDeepNestedLambdaSemantics),
    ("large operator tables exercise parse and lower", testLargeOperatorTableParseLower),
    ("nested blocks exercise parse and lower", testNestedBlocksParseLower),
    ("ambiguous case-arm pipes preserve one left-associated body", testAmbiguousCaseArmPipesParseLower),
    ("long token streams have exact token counts", testLongTokenStreamExactSize),
    ("identifier and literal token controls have exact token counts", testTokenStreamControlsExactSize),
    ("selects requested cases before stage preparation", testCaseSelection),
    ("parse-lower setup does not require module compilation", testParseLowerSetupBoundary),
    ("parse-lower setup reports entry-source read failures", testParseLowerSourceReadFailure),
    ("corpus analysis setup deeply owns its resolved module", testPreparedAnalysisForcesResolvedModule),
    ("compiler-scale analysis setup deeply owns its resolved module", testPreparedCompilerScaleAnalysisForcesResolvedModule),
    ("analysis uses module-aware imported interfaces", testModuleAwareAnalysis),
    ("runtime benchmarks reject unexpected results", testRuntimeResultValidation)
  ]

testRuntimeEvidenceScaleFamilies :: IO ()
testRuntimeEvidenceScaleFamilies = do
  assertEqual
    "nested runtime application registry"
    [ ("nested-runtime-applications-0064", 64, 1, [RuntimeBenchmark], "7"),
      ("nested-runtime-applications-0128", 128, 1, [RuntimeBenchmark], "7"),
      ("nested-runtime-applications-0256", 256, 1, [RuntimeBenchmark], "7"),
      ("nested-runtime-applications-0512", 512, 1, [RuntimeBenchmark], "7")
    ]
    [ ( compilerScaleCaseIdentifier programCase,
        compilerScaleCaseSize programCase,
        compilerScaleCaseSourceCount programCase,
        compilerScaleCaseBenchmarks programCase,
        compilerScaleCaseExpectedOutput programCase
      )
      | programCase <- compilerScaleCases,
        compilerScaleCaseScenario programCase == NestedRuntimeApplications
    ]
  assertEqual
    "runtime import width registry"
    [ ("runtime-import-width-0064", 64, Just 64, 2, [RuntimeBenchmark, WholeProgramBenchmark], "7"),
      ("runtime-import-width-0128", 128, Just 128, 2, [RuntimeBenchmark, WholeProgramBenchmark], "7"),
      ("runtime-import-width-0256", 256, Just 256, 2, [RuntimeBenchmark, WholeProgramBenchmark], "7"),
      ("runtime-import-width-0512", 512, Just 512, 2, [RuntimeBenchmark, WholeProgramBenchmark], "7")
    ]
    [ ( compilerScaleCaseIdentifier programCase,
        compilerScaleCaseSize programCase,
        compilerScaleCaseInterfaceWidth programCase,
        compilerScaleCaseSourceCount programCase,
        compilerScaleCaseBenchmarks programCase,
        compilerScaleCaseExpectedOutput programCase
      )
      | programCase <- compilerScaleCases,
        compilerScaleCaseScenario programCase == RuntimeImportWidth
    ]

testRuntimeEvidenceSmallestCases :: IO ()
testRuntimeEvidenceSmallestCases =
  mapM_
    assertRuntimeCase
    [ "nested-runtime-applications-0064",
      "runtime-import-width-0064"
    ]
  where
    assertRuntimeCase identifier = do
      programCase <- loadCompilerScaleCase identifier
      prepared <- prepareCompilerScaleBenchmark RuntimeBenchmark programCase
      runPreparedCompilerScaleBenchmark prepared
      output <- runCompilerScaleCase programCase
      assertEqual (identifier <> " real-pipeline output") "7" output

testCompilerScaleRegistry :: IO ()
testCompilerScaleRegistry =
  assertEqual
    "generated scale registry"
    [ ("sequential-polymorphic-bindings-0064", SequentialPolymorphicBindings, 64, Nothing),
      ("sequential-polymorphic-bindings-0128", SequentialPolymorphicBindings, 128, Nothing),
      ("sequential-polymorphic-bindings-0256", SequentialPolymorphicBindings, 256, Nothing),
      ("sequential-polymorphic-bindings-0512", SequentialPolymorphicBindings, 512, Nothing),
      ("wide-module-fanout-0008x0016", WideModuleFanout, 8, Just 16),
      ("wide-module-fanout-0016x0016", WideModuleFanout, 16, Just 16),
      ("wide-module-fanout-0032x0016", WideModuleFanout, 32, Just 16),
      ("wide-module-fanout-0064x0016", WideModuleFanout, 64, Just 16),
      ("wide-module-fanout-0064x0001", WideModuleFanout, 64, Just 1),
      ("wide-module-fanout-0128x0001", WideModuleFanout, 128, Just 1),
      ("wide-module-fanout-0256x0001", WideModuleFanout, 256, Just 1),
      ("wide-module-fanout-0512x0001", WideModuleFanout, 512, Just 1),
      ("shared-interface-fanout-0016x0016", SharedInterfaceFanout, 16, Just 16),
      ("shared-interface-fanout-0032x0016", SharedInterfaceFanout, 32, Just 16),
      ("shared-interface-fanout-0064x0016", SharedInterfaceFanout, 64, Just 16),
      ("shared-interface-fanout-0128x0016", SharedInterfaceFanout, 128, Just 16),
      ("resolver-fact-rich-0016", ResolverFactRich, 16, Nothing),
      ("resolver-fact-rich-0032", ResolverFactRich, 32, Nothing),
      ("resolver-fact-rich-0064", ResolverFactRich, 64, Nothing),
      ("resolver-fact-rich-0128", ResolverFactRich, 128, Nothing),
      ("typed-validation-handoff-0064", TypedValidationHandoff, 64, Nothing),
      ("typed-validation-handoff-0128", TypedValidationHandoff, 128, Nothing),
      ("typed-validation-handoff-0256", TypedValidationHandoff, 256, Nothing),
      ("typed-validation-handoff-0512", TypedValidationHandoff, 512, Nothing),
      ("lowered-temporary-validation-0064", LoweredTemporaryValidation, 64, Nothing),
      ("lowered-temporary-validation-0256", LoweredTemporaryValidation, 256, Nothing),
      ("lowered-temporary-validation-1024", LoweredTemporaryValidation, 1024, Nothing),
      ("lowered-temporary-validation-4096", LoweredTemporaryValidation, 4096, Nothing),
      ("typed-recursive-statement-graph-0128", TypedRecursiveStatementGraph, 128, Nothing),
      ("typed-recursive-statement-graph-0512", TypedRecursiveStatementGraph, 512, Nothing),
      ("typed-recursive-statement-graph-1024", TypedRecursiveStatementGraph, 1024, Nothing),
      ("typed-recursive-statement-graph-2048", TypedRecursiveStatementGraph, 2048, Nothing),
      ("typed-forward-signed-functions-0128", TypedForwardSignedFunctions, 128, Nothing),
      ("typed-forward-signed-functions-0512", TypedForwardSignedFunctions, 512, Nothing),
      ("typed-forward-signed-functions-1024", TypedForwardSignedFunctions, 1024, Nothing),
      ("typed-forward-signed-functions-2048", TypedForwardSignedFunctions, 2048, Nothing),
      ("typed-wide-export-providers-0128", TypedWideExportProviders, 128, Nothing),
      ("typed-wide-export-providers-0512", TypedWideExportProviders, 512, Nothing),
      ("typed-wide-export-providers-1024", TypedWideExportProviders, 1024, Nothing),
      ("typed-wide-export-providers-2048", TypedWideExportProviders, 2048, Nothing),
      ("wide-constructor-application-0032", WideConstructorApplication, 32, Nothing),
      ("wide-constructor-application-0064", WideConstructorApplication, 64, Nothing),
      ("wide-constructor-application-0128", WideConstructorApplication, 128, Nothing),
      ("wide-constructor-application-0256", WideConstructorApplication, 256, Nothing),
      ("capability-candidate-width-0016", CapabilityCandidateWidth, 16, Nothing),
      ("capability-candidate-width-0032", CapabilityCandidateWidth, 32, Nothing),
      ("capability-candidate-width-0064", CapabilityCandidateWidth, 64, Nothing),
      ("capability-candidate-width-0128", CapabilityCandidateWidth, 128, Nothing),
      ("host-free-opaque-environment-0064", HostFreeOpaqueEnvironment, 64, Nothing),
      ("host-free-opaque-environment-0256", HostFreeOpaqueEnvironment, 256, Nothing),
      ("host-free-opaque-environment-1024", HostFreeOpaqueEnvironment, 1024, Nothing),
      ("host-free-opaque-environment-4096", HostFreeOpaqueEnvironment, 4096, Nothing),
      ("analyzer-diagnostic-chain-0064", AnalyzerDiagnosticChain, 64, Nothing),
      ("analyzer-diagnostic-chain-0128", AnalyzerDiagnosticChain, 128, Nothing),
      ("analyzer-diagnostic-chain-0256", AnalyzerDiagnosticChain, 256, Nothing),
      ("analyzer-diagnostic-chain-0512", AnalyzerDiagnosticChain, 512, Nothing),
      ("interleaved-recursive-groups-0016", InterleavedRecursiveGroups, 16, Nothing),
      ("interleaved-recursive-groups-0032", InterleavedRecursiveGroups, 32, Nothing),
      ("interleaved-recursive-groups-0064", InterleavedRecursiveGroups, 64, Nothing),
      ("interleaved-recursive-groups-0128", InterleavedRecursiveGroups, 128, Nothing),
      ("recursive-preview-burst-0016", InterleavedRecursiveGroups, 16, Nothing),
      ("recursive-preview-burst-0032", InterleavedRecursiveGroups, 32, Nothing),
      ("recursive-preview-burst-0064", InterleavedRecursiveGroups, 64, Nothing),
      ("recursive-preview-burst-0128", InterleavedRecursiveGroups, 128, Nothing),
      ("recursive-rebinding-burst-0128", RecursiveRebindings, 128, Nothing),
      ("recursive-rebinding-burst-0256", RecursiveRebindings, 256, Nothing),
      ("recursive-rebinding-burst-0512", RecursiveRebindings, 512, Nothing),
      ("recursive-rebinding-burst-1024", RecursiveRebindings, 1024, Nothing),
      ("constrained-signatures-0032", ConstrainedSignatures, 32, Nothing),
      ("constrained-signatures-0064", ConstrainedSignatures, 64, Nothing),
      ("constrained-signatures-0128", ConstrainedSignatures, 128, Nothing),
      ("constrained-signatures-0256", ConstrainedSignatures, 256, Nothing),
      ("deferred-constraint-burst-0128", ConstrainedSignatures, 128, Nothing),
      ("deferred-constraint-burst-0256", ConstrainedSignatures, 256, Nothing),
      ("deferred-constraint-burst-0512", ConstrainedSignatures, 512, Nothing),
      ("deferred-constraint-burst-1024", ConstrainedSignatures, 1024, Nothing),
      ("deep-nested-lambdas-0016", DeepNestedLambdas, 16, Nothing),
      ("deep-nested-lambdas-0032", DeepNestedLambdas, 32, Nothing),
      ("deep-nested-lambdas-0064", DeepNestedLambdas, 64, Nothing),
      ("deep-nested-lambdas-0128", DeepNestedLambdas, 128, Nothing),
      ("large-operator-tables-0016", LargeOperatorTables, 16, Nothing),
      ("large-operator-tables-0032", LargeOperatorTables, 32, Nothing),
      ("large-operator-tables-0064", LargeOperatorTables, 64, Nothing),
      ("large-operator-tables-0128", LargeOperatorTables, 128, Nothing),
      ("nested-blocks-0016", NestedBlocks, 16, Nothing),
      ("nested-blocks-0032", NestedBlocks, 32, Nothing),
      ("nested-blocks-0064", NestedBlocks, 64, Nothing),
      ("nested-blocks-0128", NestedBlocks, 128, Nothing),
      ("ambiguous-case-arm-pipes-0064", AmbiguousCaseArmPipes, 64, Nothing),
      ("ambiguous-case-arm-pipes-0128", AmbiguousCaseArmPipes, 128, Nothing),
      ("ambiguous-case-arm-pipes-0256", AmbiguousCaseArmPipes, 256, Nothing),
      ("ambiguous-case-arm-pipes-0512", AmbiguousCaseArmPipes, 512, Nothing),
      ("long-token-stream-01024", LongTokenStream, 1024, Nothing),
      ("long-token-stream-04096", LongTokenStream, 4096, Nothing),
      ("long-token-stream-16384", LongTokenStream, 16384, Nothing),
      ("long-token-stream-65536", LongTokenStream, 65536, Nothing),
      ("identifier-token-stream-01024", IdentifierTokenStream, 1024, Nothing),
      ("identifier-token-stream-04096", IdentifierTokenStream, 4096, Nothing),
      ("identifier-token-stream-16384", IdentifierTokenStream, 16384, Nothing),
      ("identifier-token-stream-65536", IdentifierTokenStream, 65536, Nothing),
      ("literal-token-stream-01024", LiteralTokenStream, 1024, Nothing),
      ("literal-token-stream-04096", LiteralTokenStream, 4096, Nothing),
      ("literal-token-stream-16384", LiteralTokenStream, 16384, Nothing),
      ("literal-token-stream-65536", LiteralTokenStream, 65536, Nothing),
      ("nested-runtime-applications-0064", NestedRuntimeApplications, 64, Nothing),
      ("nested-runtime-applications-0128", NestedRuntimeApplications, 128, Nothing),
      ("nested-runtime-applications-0256", NestedRuntimeApplications, 256, Nothing),
      ("nested-runtime-applications-0512", NestedRuntimeApplications, 512, Nothing),
      ("runtime-import-width-0064", RuntimeImportWidth, 64, Just 64),
      ("runtime-import-width-0128", RuntimeImportWidth, 128, Just 128),
      ("runtime-import-width-0256", RuntimeImportWidth, 256, Just 256),
      ("runtime-import-width-0512", RuntimeImportWidth, 512, Just 512)
    ]
    ( map
        ( \programCase ->
            ( compilerScaleCaseIdentifier programCase,
              compilerScaleCaseScenario programCase,
              compilerScaleCaseSize programCase,
              compilerScaleCaseInterfaceWidth programCase
            )
        )
        compilerScaleCases
    )

testCompilerScaleSelection :: IO ()
testCompilerScaleSelection = do
  selected <-
    case selectCompilerScaleCases ["wide-module-fanout-0016x0016"] compilerScaleCases of
      Left message -> failTest message
      Right value -> pure value
  assertEqual
    "selected generated scale identifiers"
    ["wide-module-fanout-0016x0016"]
    (map compilerScaleCaseIdentifier selected)
  case selectCompilerScaleCases ["missing-scale-case"] compilerScaleCases of
    Left message
      | "missing-scale-case" `Text.isInfixOf` message -> pure ()
    other -> failTest ("expected unknown generated scale case rejection, got " <> Text.pack (show other))

testCompilerScaleCommandSelection :: IO ()
testCompilerScaleCommandSelection =
  case parseBenchmarkCommand
    [ "--jazz-scale-case=sequential-polymorphic-bindings-0064",
      "--jazz-scale-case",
      "wide-module-fanout-0008x0016"
    ] of
    Left message -> failTest message
    Right benchmarkCommand -> do
      assertEqual
        "selected generated scale identifiers"
        ["sequential-polymorphic-bindings-0064", "wide-module-fanout-0008x0016"]
        (benchmarkCommandSelectedScaleCases benchmarkCommand)
      assertEqual
        "generated selector leaves corpus selection empty"
        []
        (benchmarkCommandSelectedCases benchmarkCommand)

testCompilerScaleCommandRejection :: IO ()
testCompilerScaleCommandRejection = do
  assertCommandError
    "missing generated selector value"
    "--jazz-scale-case requires a value"
    ["--jazz-scale-case"]
  assertCommandError
    "duplicate generated selector"
    "duplicate --jazz-scale-case: sequential-polymorphic-bindings-0064"
    [ "--jazz-scale-case=sequential-polymorphic-bindings-0064",
      "--jazz-scale-case=sequential-polymorphic-bindings-0064"
    ]
  assertCommandError
    "mixed corpus and generated selectors"
    "--jazz-case and --jazz-scale-case cannot be combined"
    ["--jazz-case=identifier-classifier", "--jazz-scale-case=sequential-polymorphic-bindings-0064"]
  assertCommandError
    "smoke mode with generated selectors"
    "--jazz-smoke cannot select compiler scale cases"
    ["--jazz-smoke", "--jazz-scale-case=sequential-polymorphic-bindings-0064"]

assertCommandError :: Text -> Text -> [String] -> IO ()
assertCommandError label expected arguments =
  case parseBenchmarkCommand arguments of
    Left message -> assertEqual label expected message
    Right _ -> failTest ("expected benchmark command rejection: " <> label)

testSequentialPolymorphismSemantics :: IO ()
testSequentialPolymorphismSemantics = do
  programCase <- loadCompilerScaleCase "sequential-polymorphic-bindings-0064"
  actualOutput <- runCompilerScaleCase programCase
  assertEqual "sequential polymorphism output" "(42, True)" actualOutput
  prepared <- prepareCompilerScaleBenchmark AnalysisBenchmark programCase
  runPreparedCompilerScaleBenchmark prepared

testWideModuleFanoutSemantics :: IO ()
testWideModuleFanoutSemantics = do
  programCase <- loadCompilerScaleCase "wide-module-fanout-0008x0016"
  assertEqual "wide fanout virtual source count" 9 (compilerScaleCaseSourceCount programCase)
  actualOutput <- runCompilerScaleCase programCase
  assertEqual "wide fanout output" "0" actualOutput
  prepared <- prepareCompilerScaleBenchmark ModulePreparationBenchmark programCase
  runPreparedCompilerScaleBenchmark prepared
  lookupCase <- loadCompilerScaleCase "wide-module-fanout-0064x0001"
  assertEqual "lookup fanout virtual source count" 65 (compilerScaleCaseSourceCount lookupCase)
  lookupOutput <- runCompilerScaleCase lookupCase
  assertEqual "lookup fanout output" "0" lookupOutput
  lookupPrepared <- prepareCompilerScaleBenchmark ModulePreparationBenchmark lookupCase
  runPreparedCompilerScaleBenchmark lookupPrepared
  sharedCase <- loadCompilerScaleCase "shared-interface-fanout-0016x0016"
  assertEqual "shared-interface fanout virtual source count" 18 (compilerScaleCaseSourceCount sharedCase)
  sharedOutput <- runCompilerScaleCase sharedCase
  assertEqual "shared-interface fanout output" "0" sharedOutput
  sharedPrepared <- prepareCompilerScaleBenchmark ModulePreparationBenchmark sharedCase
  runPreparedCompilerScaleBenchmark sharedPrepared

testResolverFactRichSemantics :: IO ()
testResolverFactRichSemantics = do
  programCase <- loadCompilerScaleCase "resolver-fact-rich-0016"
  assertEqual "resolver fact-rich virtual source count" 3 (compilerScaleCaseSourceCount programCase)
  actualOutput <- runCompilerScaleCase programCase
  assertEqual "resolver fact-rich output" "Token" actualOutput
  prepared <- prepareCompilerScaleBenchmark ModulePreparationBenchmark programCase
  runPreparedCompilerScaleBenchmark prepared

testTypedValidationHandoffSemantics :: IO ()
testTypedValidationHandoffSemantics = do
  programCase <- loadCompilerScaleCase "typed-validation-handoff-0064"
  prepared <- prepareCompilerScaleBenchmark TypedLoweringBenchmark programCase
  runPreparedCompilerScaleBenchmark prepared

testLoweredTemporaryValidationRegistry :: IO ()
testLoweredTemporaryValidationRegistry =
  assertEqual
    "lowered temporary validation registry"
    [ ("lowered-temporary-validation-0064", 64, [TypedLoweringBenchmark]),
      ("lowered-temporary-validation-0256", 256, [TypedLoweringBenchmark]),
      ("lowered-temporary-validation-1024", 1024, [TypedLoweringBenchmark]),
      ("lowered-temporary-validation-4096", 4096, [TypedLoweringBenchmark])
    ]
    [ ( compilerScaleCaseIdentifier programCase,
        compilerScaleCaseSize programCase,
        compilerScaleCaseBenchmarks programCase
      )
      | programCase <- compilerScaleCases,
        compilerScaleCaseScenario programCase == LoweredTemporaryValidation
    ]

testLoweredTemporaryValidationSmallestCase :: IO ()
testLoweredTemporaryValidationSmallestCase = do
  programCase <- loadCompilerScaleCase "lowered-temporary-validation-0064"
  prepared <- prepareCompilerScaleBenchmark TypedLoweringBenchmark programCase
  runPreparedCompilerScaleBenchmark prepared

testTypedForwardSignedFunctionsRegistry :: IO ()
testTypedForwardSignedFunctionsRegistry =
  assertEqual
    "typed forward signed function registry"
    [ ("typed-forward-signed-functions-0128", 128, [TypedLoweringBenchmark]),
      ("typed-forward-signed-functions-0512", 512, [TypedLoweringBenchmark]),
      ("typed-forward-signed-functions-1024", 1024, [TypedLoweringBenchmark]),
      ("typed-forward-signed-functions-2048", 2048, [TypedLoweringBenchmark])
    ]
    [ ( compilerScaleCaseIdentifier programCase,
        compilerScaleCaseSize programCase,
        compilerScaleCaseBenchmarks programCase
      )
    | programCase <- compilerScaleCases,
      compilerScaleCaseScenario programCase == TypedForwardSignedFunctions
    ]

testTypedForwardSignedFunctionsSmallestCase :: IO ()
testTypedForwardSignedFunctionsSmallestCase = do
  programCase <- loadCompilerScaleCase "typed-forward-signed-functions-0128"
  prepared <- prepareCompilerScaleBenchmark TypedLoweringBenchmark programCase
  runPreparedCompilerScaleBenchmark prepared

testTypedWideExportProvidersRegistry :: IO ()
testTypedWideExportProvidersRegistry =
  assertEqual
    "typed wide export provider registry"
    [ ("typed-wide-export-providers-0128", 128, [TypedLoweringBenchmark]),
      ("typed-wide-export-providers-0512", 512, [TypedLoweringBenchmark]),
      ("typed-wide-export-providers-1024", 1024, [TypedLoweringBenchmark]),
      ("typed-wide-export-providers-2048", 2048, [TypedLoweringBenchmark])
    ]
    [ ( compilerScaleCaseIdentifier programCase,
        compilerScaleCaseSize programCase,
        compilerScaleCaseBenchmarks programCase
      )
    | programCase <- compilerScaleCases,
      compilerScaleCaseScenario programCase == TypedWideExportProviders
    ]

testTypedWideExportProvidersSmallestCase :: IO ()
testTypedWideExportProvidersSmallestCase = do
  programCase <- loadCompilerScaleCase "typed-wide-export-providers-0128"
  prepared <- prepareCompilerScaleBenchmark TypedLoweringBenchmark programCase
  runPreparedCompilerScaleBenchmark prepared

testTypedRecursiveStatementGraphRegistry :: IO ()
testTypedRecursiveStatementGraphRegistry =
  assertEqual
    "typed recursive statement graph registry"
    [ ("typed-recursive-statement-graph-0128", 128, [TypedLoweringBenchmark]),
      ("typed-recursive-statement-graph-0512", 512, [TypedLoweringBenchmark]),
      ("typed-recursive-statement-graph-1024", 1024, [TypedLoweringBenchmark]),
      ("typed-recursive-statement-graph-2048", 2048, [TypedLoweringBenchmark])
    ]
    [ ( compilerScaleCaseIdentifier programCase,
        compilerScaleCaseSize programCase,
        compilerScaleCaseBenchmarks programCase
      )
      | programCase <- compilerScaleCases,
        compilerScaleCaseScenario programCase == TypedRecursiveStatementGraph
    ]

testTypedRecursiveStatementGraphSmallestCase :: IO ()
testTypedRecursiveStatementGraphSmallestCase = do
  programCase <- loadCompilerScaleCase "typed-recursive-statement-graph-0128"
  prepared <- prepareCompilerScaleBenchmark TypedLoweringBenchmark programCase
  runPreparedCompilerScaleBenchmark prepared

testWideConstructorApplicationSemantics :: IO ()
testWideConstructorApplicationSemantics = do
  programCase <- loadCompilerScaleCase "wide-constructor-application-0032"
  assertEqual
    "wide constructor benchmark groups"
    [AnalysisBenchmark, RuntimeBenchmark, WholeProgramBenchmark]
    (compilerScaleCaseBenchmarks programCase)
  actualOutput <- runCompilerScaleCase programCase
  assertEqual "wide constructor output" "(<function>, (0, 16, 31))" actualOutput
  runtimePrepared <- prepareCompilerScaleBenchmark RuntimeBenchmark programCase
  runPreparedCompilerScaleBenchmark runtimePrepared

testCapabilityCandidateWidthRegistry :: IO ()
testCapabilityCandidateWidthRegistry =
  assertEqual
    "capability candidate width registry"
    [ ("capability-candidate-width-0016", 16, 1, [AnalysisBenchmark, RuntimeBenchmark, WholeProgramBenchmark], "15"),
      ("capability-candidate-width-0032", 32, 1, [AnalysisBenchmark, RuntimeBenchmark, WholeProgramBenchmark], "31"),
      ("capability-candidate-width-0064", 64, 1, [AnalysisBenchmark, RuntimeBenchmark, WholeProgramBenchmark], "63"),
      ("capability-candidate-width-0128", 128, 1, [AnalysisBenchmark, RuntimeBenchmark, WholeProgramBenchmark], "127")
    ]
    [ ( compilerScaleCaseIdentifier programCase,
        compilerScaleCaseSize programCase,
        compilerScaleCaseSourceCount programCase,
        compilerScaleCaseBenchmarks programCase,
        compilerScaleCaseExpectedOutput programCase
      )
      | programCase <- compilerScaleCases,
        compilerScaleCaseScenario programCase == CapabilityCandidateWidth
    ]

testCapabilityCandidateWidthSemantics :: IO ()
testCapabilityCandidateWidthSemantics = do
  programCase <- loadCompilerScaleCase "capability-candidate-width-0016"
  actualOutput <- runCompilerScaleCase programCase
  assertEqual "capability candidate width final-target output" "15" actualOutput
  mapM_
    ( \benchmarkGroup -> do
        prepared <- prepareCompilerScaleBenchmark benchmarkGroup programCase
        runPreparedCompilerScaleBenchmark prepared
    )
    [AnalysisBenchmark, RuntimeBenchmark, WholeProgramBenchmark]

testHostFreeOpaqueEnvironmentSemantics :: IO ()
testHostFreeOpaqueEnvironmentSemantics = do
  programCase <- loadCompilerScaleCase "host-free-opaque-environment-0064"
  assertEqual
    "host-free opaque environment benchmark groups"
    [RuntimeBenchmark, WholeProgramBenchmark]
    (compilerScaleCaseBenchmarks programCase)
  actualOutput <- runCompilerScaleCase programCase
  assertEqual "host-free opaque environment output" "1" actualOutput
  runtimePrepared <- prepareCompilerScaleBenchmark RuntimeBenchmark programCase
  runPreparedCompilerScaleBenchmark runtimePrepared

testAnalyzerDiagnosticChainSemantics :: IO ()
testAnalyzerDiagnosticChainSemantics = do
  programCase <- loadCompilerScaleCase "analyzer-diagnostic-chain-0064"
  assertEqual
    "analyzer diagnostic benchmark boundary"
    [AnalysisBenchmark]
    (compilerScaleCaseBenchmarks programCase)
  prepared <- prepareCompilerScaleBenchmark AnalysisBenchmark programCase
  runPreparedCompilerScaleBenchmark prepared

testInterleavedRecursiveGroupSemantics :: IO ()
testInterleavedRecursiveGroupSemantics = do
  programCase <- loadCompilerScaleCase "interleaved-recursive-groups-0016"
  actualOutput <- runCompilerScaleCase programCase
  assertEqual "interleaved recursive group output" "(1, True)" actualOutput
  prepared <- prepareCompilerScaleBenchmark AnalysisBenchmark programCase
  runPreparedCompilerScaleBenchmark prepared

testRecursivePreviewBurstSemantics :: IO ()
testRecursivePreviewBurstSemantics = do
  programCase <- loadCompilerScaleCase "recursive-preview-burst-0016"
  actualOutput <- runCompilerScaleCase programCase
  assertEqual "recursive preview burst output" "(1, True)" actualOutput
  prepared <- prepareCompilerScaleBenchmark AnalysisBenchmark programCase
  runPreparedCompilerScaleBenchmark prepared

testRecursiveRebindingBurstSemantics :: IO ()
testRecursiveRebindingBurstSemantics = do
  programCase <- loadCompilerScaleCase "recursive-rebinding-burst-0128"
  actualOutput <- runCompilerScaleCase programCase
  assertEqual "same-name rebinding burst output" "127" actualOutput
  prepared <- prepareCompilerScaleBenchmark AnalysisBenchmark programCase
  runPreparedCompilerScaleBenchmark prepared

testConstrainedSignatureSemantics :: IO ()
testConstrainedSignatureSemantics = do
  programCase <- loadCompilerScaleCase "constrained-signatures-0032"
  assertEqual
    "constrained signature benchmark boundaries"
    [ParseLowerBenchmark, AnalysisBenchmark]
    (compilerScaleCaseBenchmarks programCase)
  actualOutput <- runCompilerScaleCase programCase
  assertEqual "constrained signature output" "(1, True)" actualOutput
  preparedParseLower <- prepareCompilerScaleBenchmark ParseLowerBenchmark programCase
  runPreparedCompilerScaleBenchmark preparedParseLower
  prepared <- prepareCompilerScaleBenchmark AnalysisBenchmark programCase
  runPreparedCompilerScaleBenchmark prepared

testDeferredConstraintBurstSemantics :: IO ()
testDeferredConstraintBurstSemantics = do
  programCase <- loadCompilerScaleCase "deferred-constraint-burst-0128"
  actualOutput <- runCompilerScaleCase programCase
  assertEqual
    "deferred constraint burst output"
    ("[" <> Text.intercalate ", " (replicate 128 "1") <> "]")
    actualOutput
  prepared <- prepareCompilerScaleBenchmark AnalysisBenchmark programCase
  runPreparedCompilerScaleBenchmark prepared

testDeepNestedLambdaSemantics :: IO ()
testDeepNestedLambdaSemantics = do
  programCase <- loadCompilerScaleCase "deep-nested-lambdas-0016"
  assertEqual
    "deep nested lambda benchmark boundaries"
    [AnalysisBenchmark, ModulePreparationBenchmark, WholeProgramBenchmark]
    (compilerScaleCaseBenchmarks programCase)
  actualOutput <- runCompilerScaleCase programCase
  assertEqual "deep nested lambda output" "(1, 16)" actualOutput
  prepared <- prepareCompilerScaleBenchmark ModulePreparationBenchmark programCase
  runPreparedCompilerScaleBenchmark prepared
  preparedWholeProgram <- prepareCompilerScaleBenchmark WholeProgramBenchmark programCase
  runPreparedCompilerScaleBenchmark preparedWholeProgram

testLargeOperatorTableParseLower :: IO ()
testLargeOperatorTableParseLower = do
  programCase <- loadCompilerScaleCase "large-operator-tables-0016"
  prepared <- prepareCompilerScaleBenchmark ParseLowerBenchmark programCase
  runPreparedCompilerScaleBenchmark prepared

testNestedBlocksParseLower :: IO ()
testNestedBlocksParseLower = do
  programCase <- loadCompilerScaleCase "nested-blocks-0016"
  prepared <- prepareCompilerScaleBenchmark ParseLowerBenchmark programCase
  runPreparedCompilerScaleBenchmark prepared

testAmbiguousCaseArmPipesParseLower :: IO ()
testAmbiguousCaseArmPipesParseLower = do
  programCase <- loadCompilerScaleCase "ambiguous-case-arm-pipes-0064"
  assertEqual
    "ambiguous case-arm pipe benchmark boundary"
    [ParseLowerBenchmark]
    (compilerScaleCaseBenchmarks programCase)
  assertEqual
    "ambiguous case-arm pipe source count"
    1
    (compilerScaleCaseSourceCount programCase)
  source <-
    case compilerScaleCaseEntrySource programCase of
      Nothing -> failTest "ambiguous case-arm pipe scale case is missing its entry source"
      Just value -> pure value
  surfaceProgram <-
    case parseSurfaceProgram source of
      Left diagnostic -> failTest ("ambiguous case-arm pipe source did not parse: " <> Text.pack (show diagnostic))
      Right value -> pure value
  case surfaceProgram of
    SEBlock
      [ SSLet bindingName bindingSpan
          ( SECase
              (SELit (SLInt scrutinee))
              [SurfaceCaseArm SPWildcard Nothing body]
            )
        ] -> do
          assertEqual "ambiguous case-arm pipe binding" "ambiguousPipe" (identifierText bindingName)
          assertEqual "ambiguous case-arm pipe binding span" (SourceSpan 1 1) bindingSpan
          assertEqual "ambiguous case-arm pipe scrutinee" 0 scrutinee
          case leftAssociatedPipeOperands body of
            Nothing -> failTest ("ambiguous case-arm pipe body was not exactly left-associated: " <> Text.pack (show body))
            Just operands -> assertEqual "ambiguous case-arm pipe operands" [0 .. 63] operands
    other ->
      failTest
        ( "ambiguous case-arm pipe source did not preserve exactly one wildcard arm: "
            <> Text.pack (show other)
        )
  prepared <- prepareCompilerScaleBenchmark ParseLowerBenchmark programCase
  runPreparedCompilerScaleBenchmark prepared

leftAssociatedPipeOperands :: SurfaceExpr -> Maybe [Integer]
leftAssociatedPipeOperands = go []
  where
    go trailingOperands expression =
      case expression of
        SEBinary "|" left (SELit (SLInt rightOperand)) ->
          go (rightOperand : trailingOperands) left
        SELit (SLInt firstOperand) -> Just (firstOperand : trailingOperands)
        _ -> Nothing

testLongTokenStreamExactSize :: IO ()
testLongTokenStreamExactSize = do
  programCase <- loadCompilerScaleCase "long-token-stream-01024"
  source <-
    case compilerScaleCaseEntrySource programCase of
      Nothing -> failTest "long token stream scale case is missing its entry source"
      Just value -> pure value
  tokens <-
    case tokenize source of
      Left diagnostic -> failTest ("long token stream did not tokenize: " <> Text.pack (show diagnostic))
      Right values -> pure values
  assertEqual "long token stream token count" 1024 (length tokens)
  prepared <- prepareCompilerScaleBenchmark ParseLowerBenchmark programCase
  runPreparedCompilerScaleBenchmark prepared

testTokenStreamControlsExactSize :: IO ()
testTokenStreamControlsExactSize =
  mapM_ assertExactTokenCount ["identifier-token-stream-01024", "literal-token-stream-01024"]
  where
    assertExactTokenCount identifier = do
      programCase <- loadCompilerScaleCase identifier
      source <-
        case compilerScaleCaseEntrySource programCase of
          Nothing -> failTest (identifier <> " is missing its entry source")
          Just value -> pure value
      tokens <-
        case tokenize source of
          Left diagnostic -> failTest (identifier <> " did not tokenize: " <> Text.pack (show diagnostic))
          Right values -> pure values
      assertEqual (identifier <> " token count") 1024 (length tokens)
      prepared <- prepareCompilerScaleBenchmark ParseLowerBenchmark programCase
      runPreparedCompilerScaleBenchmark prepared

testCaseSelection :: IO ()
testCaseSelection = do
  corpus <- loadCheckedInCorpus
  selected <-
    case selectProgramCases ["identifier-classifier"] (programCorpusCases corpus) of
      Left message -> failTest message
      Right value -> pure value
  assertEqual "selected identifiers" ["identifier-classifier"] (map programCaseIdentifier selected)
  case selectProgramCases ["missing-case"] (programCorpusCases corpus) of
    Left message
      | "missing-case" `Text.isInfixOf` message -> pure ()
    other -> failTest ("expected unknown case rejection, got " <> Text.pack (show other))

testParseLowerSetupBoundary :: IO ()
testParseLowerSetupBoundary = do
  programCase <- loadCase "identifier-classifier"
  prepared <-
    prepareBenchmark
      ParseLowerBenchmark
      programCase {programCaseModuleRoot = "/path/that/cannot-resolve-modules"}
  runPreparedBenchmark prepared

testParseLowerSourceReadFailure :: IO ()
testParseLowerSourceReadFailure = do
  programCase <- loadCase "identifier-classifier"
  result <-
    try
      ( void
          ( prepareBenchmark
              ParseLowerBenchmark
              programCase {programCaseEntrySource = programCaseEntrySource programCase <> ".missing"}
          )
      ) ::
      IO (Either IOException ())
  case result of
    Left exception
      | "could not read corpus entry source for case 'identifier-classifier'"
          `Text.isInfixOf` Text.pack (show exception) ->
          pure ()
    Left exception ->
      failTest ("expected a structured entry-source read failure, got " <> Text.pack (show exception))
    Right () -> failTest "expected parse-lower setup to reject an unreadable entry source"

testPreparedAnalysisForcesResolvedModule :: IO ()
testPreparedAnalysisForcesResolvedModule = do
  programCase <- loadCase "identifier-classifier"
  prepared <- prepareBenchmark AnalysisBenchmark programCase
  case prepared of
    PreparedAnalysis preparedCase compiledProgram inputs dependencies resolvedModule ->
      mapM_
        ( \(field, marker, poisonedModule) ->
            assertPreparedResolvedModuleForced
              ("corpus analysis " <> field)
              marker
              ( rnf
                  ( PreparedAnalysis
                      preparedCase
                      compiledProgram
                      inputs
                      dependencies
                      poisonedModule
                  )
              )
        )
        (poisonResolvedModuleCases "corpus analysis" resolvedModule)
    _ -> failTest "analysis preparation returned the wrong prepared benchmark variant"

testPreparedCompilerScaleAnalysisForcesResolvedModule :: IO ()
testPreparedCompilerScaleAnalysisForcesResolvedModule = do
  programCase <- loadCompilerScaleCase "sequential-polymorphic-bindings-0064"
  prepared <- prepareCompilerScaleBenchmark AnalysisBenchmark programCase
  case prepared of
    PreparedCompilerScaleAnalysis preparedCase compiledProgram inputs dependencies resolvedModule ->
      mapM_
        ( \(field, marker, poisonedModule) ->
            assertPreparedResolvedModuleForced
              ("compiler-scale analysis " <> field)
              marker
              ( rnf
                  ( PreparedCompilerScaleAnalysis
                      preparedCase
                      compiledProgram
                      inputs
                      dependencies
                      poisonedModule
                  )
              )
        )
        (poisonResolvedModuleCases "compiler-scale analysis" resolvedModule)
    _ -> failTest "analysis preparation returned the wrong prepared compiler-scale variant"

poisonResolvedModuleCases :: Text -> ResolvedModule -> [(Text, Text, ResolvedModule)]
poisonResolvedModuleCases prefix resolvedModule =
  [ poisonCase
      "module path"
      resolvedModule
        { resolvedModulePath = resolvedModulePath resolvedModule <> [deferred "module path"]
        },
    poisonCase
      "source path"
      resolvedModule
        { resolvedSourcePath = resolvedSourcePath resolvedModule <> deferred "source path"
        },
    poisonCase "import span" (withResolvedImport (baseResolvedImport {resolvedImportSpan = deferred "import span"})),
    poisonCase "import path" (withResolvedImport (baseResolvedImport {resolvedImportPath = ["Lib", deferred "import path"]})),
    poisonCase "import alias" (withResolvedImport (baseResolvedImport {resolvedImportAlias = Just (deferred "import alias")})),
    poisonCase "import symbols" (withResolvedImport (baseResolvedImport {resolvedImportSymbols = Just ["value", deferred "import symbols"]})),
    poisonCase
      "export inventory"
      resolvedModule
        { resolvedModuleExportInventory =
            exportInventory [ModuleExport ValueNamespace (deferred "export inventory")]
        },
    poisonCase
      "Core declared path"
      ( withCoreModule
          ( \coreModule ->
              coreModule
                { coreModuleDeclaredPath = Just ["App", deferred "Core declared path"]
                }
          )
      ),
    poisonCase
      "Core declared-export span"
      ( withCoreModule
          ( \coreModule ->
              coreModule
                { coreModuleDeclaredExports =
                    Just
                      ( DeclaredModuleExports
                          (deferred "Core declared-export span")
                          []
                      )
                }
          )
      ),
    poisonCase
      "Core declared-export selectors"
      ( withCoreModule
          ( \coreModule ->
              coreModule
                { coreModuleDeclaredExports =
                    Just
                      ( DeclaredModuleExports
                          (SourceSpan 1 1)
                          [deferred "Core declared-export selectors"]
                      )
                }
          )
      ),
    poisonCase
      "Core imports"
      (withCoreModule (\coreModule -> coreModule {coreModuleImports = [deferred "Core imports"]})),
    poisonCase
      "Core expression"
      (withCoreModule (\coreModule -> coreModule {coreModuleExpr = EList [deferred "Core expression"]}))
  ]
  where
    poisonCase field poisonedModule = (field, marker field, poisonedModule)
    marker field = prefix <> " " <> field <> " was forced"
    deferred field = throw (userError (Text.unpack (marker field)))
    withResolvedImport resolvedImport =
      resolvedModule {resolvedModuleImports = [resolvedImport]}
    withCoreModule updateCore =
      resolvedModule
        { resolvedModuleCore = updateCore (resolvedModuleCore resolvedModule)
        }
    baseResolvedImport =
      ResolvedImport
        { resolvedImportSpan = SourceSpan 1 1,
          resolvedImportPath = ["Lib"],
          resolvedImportAlias = Nothing,
          resolvedImportSymbols = Nothing
        }

assertPreparedResolvedModuleForced :: Text -> Text -> () -> IO ()
assertPreparedResolvedModuleForced label marker forced = do
  result <- try (evaluate forced) :: IO (Either IOException ())
  case result of
    Left exception
      | marker `Text.isInfixOf` Text.pack (show exception) -> pure ()
      | otherwise ->
          failTest
            ( label
                <> " forcing failed before reaching its resolved module: "
                <> Text.pack (show exception)
            )
    Right () -> failTest (label <> " left its resolved module lazy")

testModuleAwareAnalysis :: IO ()
testModuleAwareAnalysis = do
  programCase <- loadCase "mini-frontend"
  prepared <- prepareBenchmark AnalysisBenchmark programCase
  runPreparedBenchmark prepared

testRuntimeResultValidation :: IO ()
testRuntimeResultValidation = do
  programCase <- loadCase "identifier-classifier"
  prepared <-
    prepareBenchmark
      RuntimeBenchmark
      programCase {programCaseExpectedStdout = "not the program output\n"}
  result <- try (runPreparedBenchmark prepared) :: IO (Either IOException ())
  case result of
    Left _ -> pure ()
    Right () -> failTest "expected the runtime benchmark to reject mismatched output"

loadCase :: Text -> IO ProgramCase
loadCase identifier = do
  corpus <- loadCheckedInCorpus
  case programCaseById identifier corpus of
    Nothing -> failTest ("missing checked-in corpus case: " <> identifier)
    Just programCase -> pure programCase

loadCompilerScaleCase :: Text -> IO CompilerScaleCase
loadCompilerScaleCase identifier =
  case selectCompilerScaleCases [identifier] compilerScaleCases of
    Left message -> failTest message
    Right [programCase] -> pure programCase
    Right selected ->
      failTest
        ( "expected one generated scale case for "
            <> identifier
            <> ", got "
            <> Text.pack (show (map compilerScaleCaseIdentifier selected))
        )

loadCheckedInCorpus :: IO ProgramCorpus
loadCheckedInCorpus = do
  result <- loadProgramCorpus
  case result of
    Left violations ->
      failTest
        ("could not load checked-in corpus:\n" <> Text.unlines (map renderProgramCorpusViolation violations))
    Right corpus -> pure corpus
