{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Jazz.Benchmark.StageInputs
  ( PreparedBenchmark,
    PreparedCompilerScaleBenchmark,
    prepareBenchmark,
    prepareCompilerScaleBenchmark,
    runCompilerScaleCase,
    runPreparedBenchmark,
    runPreparedCompilerScaleBenchmark,
    selectProgramCases,
  )
where

import Control.DeepSeq (NFData (rnf))
import Control.Exception (evaluate)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Jazz.Benchmark.Force
  ( forceAnalyzedProgram,
    forceAnalyzedProgramResult,
    forceDiagnostic,
    forceListWith,
    forceLoweredExpr,
    forceLoweredProgram,
    forceProgramCaseResult,
    forceResolvedExpr,
    forceRuntimeProgramOutputResult,
    forceSurfaceExpr,
    forceTokens,
    forceTypedProgram,
  )
import Jazz.Benchmark.ScaleCases
  ( CompilerScaleCase,
    CompilerScaleScenario (..),
    compilerScaleCaseEntryModulePath,
    compilerScaleCaseEntrySource,
    compilerScaleCaseExpectedOutput,
    compilerScaleCaseIdentifier,
    compilerScaleCaseResolutionConfig,
    compilerScaleCaseScenario,
    compilerScaleCaseSize,
    compilerScaleCaseSource,
  )
import Jazz.Compiler.AST
  ( CoreNode (..),
    CoreNodeId (..),
    CorePhase (Analyzed, Resolved),
    Expr (..),
  )
import Jazz.Compiler.Analyzer
  ( AnalysisResult (..),
    analyzeProgram,
  )
import Jazz.Compiler.BundledPrelude (bundledPreludeSource)
import Jazz.Compiler.Diagnostics (Diagnostic, SourceSpan (..), isErrorDiagnostic)
import Jazz.Compiler.Diagnostics.Render (renderDiagnostic)
import Jazz.Compiler.Driver (ResolvedPrelude (PreludeBundled), buildAnalyzedProgram)
import Jazz.Compiler.LoweredIR
import Jazz.Compiler.LoweredIR.Lower
  ( LoweredIRLoweringResult (..),
    lowerValidatedTypedCoreExpressionDirectCall,
    validatedLoweredProgram,
  )
import Jazz.Compiler.LoweredIR.Validate (validateLoweredProgram)
import qualified Jazz.Compiler.ModuleCompiler as ModuleCompiler
import Jazz.Compiler.ModuleGraph
  ( CoreProgram,
  )
import Jazz.Compiler.ModuleInterface (CompileInputs, compileInputs)
import Jazz.Compiler.ModuleRuntime
  ( RuntimeProgram (runtimeProgramOutput),
    evaluateAnalyzedProgram,
  )
import Jazz.Compiler.Name
  ( NameNamespace (ValueNamespace),
    mkIdentifier,
    resolvedLocalName,
  )
import Jazz.Compiler.Parser (parseSurfaceProgramTokens)
import Jazz.Compiler.Parser.Lexer (tokenize)
import Jazz.Compiler.Parser.Lower (lowerSurfaceExpr)
import Jazz.Compiler.Profiling
  ( BenchmarkGroup (..),
    CompilerStage (..),
    withCompilerStage,
  )
import Jazz.Compiler.Runtime (renderRuntimeValue)
import Jazz.Compiler.TypeRepresentation (SemanticType (..))
import Jazz.Compiler.TypedCore
import Jazz.Compiler.TypedCore.Validate
  ( validateTypedProgram,
    validateTypedProgramOnce,
  )
import Jazz.Compiler.WarningConfig (defaultWarningSettings)
import Jazz.ProgramCorpus.Runner
  ( ProgramCaseResult (..),
    loadProgramCaseEntrySource,
    prepareProgramCase,
    runProgramCase,
  )
import Jazz.ProgramCorpus.Types
  ( ProgramBudgets (..),
    ProgramCase (..),
    ProgramTermination (..),
  )

data PreparedBenchmark
  = PreparedParseLower Text
  | PreparedAnalysis CompileInputs (CoreProgram 'Resolved)
  | PreparedModulePreparation ProgramCase
  | PreparedRuntime ExpectedProgramBehavior (CoreProgram 'Analyzed)
  | PreparedWholeProgram ProgramCase

data PreparedCompilerScaleBenchmark
  = PreparedCompilerScaleParseLower Text
  | PreparedCompilerScaleAnalysis CompileInputs (CoreProgram 'Resolved)
  | PreparedCompilerScaleModulePreparation CompilerScaleCase
  | PreparedCompilerScaleRuntime ExpectedCompilerScaleOutput (CoreProgram 'Analyzed)
  | PreparedCompilerScaleLoweredValidation LoweredProgram
  | PreparedCompilerScaleTypedValidation TypedProgram
  | PreparedCompilerScaleTypedLowering TypedProgram
  | PreparedCompilerScaleDiagnosticAnalysis (Expr 'Resolved) Int
  | PreparedCompilerScaleWholeProgram CompilerScaleCase

data ExpectedProgramBehavior = ExpectedProgramBehavior Text ProgramTermination Text

data ExpectedCompilerScaleOutput = ExpectedCompilerScaleOutput Text Text

instance NFData PreparedBenchmark where
  rnf preparedBenchmark =
    case preparedBenchmark of
      PreparedParseLower source -> Text.length source `seq` ()
      PreparedAnalysis inputs resolvedProgram ->
        inputs `seq`
          rnf resolvedProgram
      PreparedModulePreparation programCase -> forceProgramCase programCase
      PreparedRuntime expectedBehavior analyzedProgram ->
        forceExpectedProgramBehavior expectedBehavior `seq` forceAnalyzedProgram analyzedProgram
      PreparedWholeProgram programCase -> forceProgramCase programCase

instance NFData PreparedCompilerScaleBenchmark where
  rnf preparedBenchmark =
    case preparedBenchmark of
      PreparedCompilerScaleParseLower source -> Text.length source `seq` ()
      PreparedCompilerScaleAnalysis inputs resolvedProgram ->
        inputs `seq`
          rnf resolvedProgram
      PreparedCompilerScaleModulePreparation programCase -> rnf programCase
      PreparedCompilerScaleRuntime expectedOutput analyzedProgram ->
        forceExpectedCompilerScaleOutput expectedOutput `seq` forceAnalyzedProgram analyzedProgram
      PreparedCompilerScaleLoweredValidation loweredProgram -> forceLoweredProgram loweredProgram
      PreparedCompilerScaleTypedValidation typedProgram -> forceTypedProgram typedProgram
      PreparedCompilerScaleTypedLowering typedProgram -> forceTypedProgram typedProgram
      PreparedCompilerScaleDiagnosticAnalysis expression expectedDiagnosticCount ->
        forceResolvedExpr expression `seq` rnf expectedDiagnosticCount
      PreparedCompilerScaleWholeProgram programCase -> rnf programCase

prepareBenchmark :: BenchmarkGroup -> ProgramCase -> IO PreparedBenchmark
prepareBenchmark benchmarkGroup programCase =
  case benchmarkGroup of
    ParseLowerBenchmark -> do
      source <-
        withCompilerStage SourceLoadingStage $ do
          sourceResult <- loadProgramCaseEntrySource programCase
          case sourceResult of
            Left diagnostic -> failBenchmarkDiagnostic diagnostic
            Right loadedSource -> do
              _ <- evaluate (Text.length loadedSource)
              pure loadedSource
      prepareFully (PreparedParseLower source)
    AnalysisBenchmark -> do
      (resolvedProgram, _) <- prepareValidProgram programCase
      let inputs = compileInputs defaultWarningSettings Set.empty
      prepareFully
        ( PreparedAnalysis
            inputs
            resolvedProgram
        )
    ModulePreparationBenchmark -> prepareFully (PreparedModulePreparation programCase)
    DiagnosticAnalysisBenchmark -> unsupportedCorpusGroup benchmarkGroup programCase
    TypedValidationBenchmark -> unsupportedCorpusGroup benchmarkGroup programCase
    LoweredValidationBenchmark -> unsupportedCorpusGroup benchmarkGroup programCase
    TypedLoweringBenchmark -> unsupportedCorpusGroup benchmarkGroup programCase
    RuntimeBenchmark -> do
      (_, analyzedProgram) <- prepareValidProgram programCase
      prepareFully
        ( PreparedRuntime
            (expectedProgramBehavior programCase)
            analyzedProgram
        )
    WholeProgramBenchmark -> prepareFully (PreparedWholeProgram programCase)

prepareCompilerScaleBenchmark :: BenchmarkGroup -> CompilerScaleCase -> IO PreparedCompilerScaleBenchmark
prepareCompilerScaleBenchmark benchmarkGroup programCase =
  case benchmarkGroup of
    ParseLowerBenchmark -> do
      source <-
        case compilerScaleCaseEntrySource programCase of
          Nothing ->
            ioError
              ( userError
                  ( "compiler scale case is missing its entry source: "
                      <> Text.unpack (compilerScaleCaseIdentifier programCase)
                  )
              )
          Just value -> evaluate (Text.length value) >> pure value
      prepareFully (PreparedCompilerScaleParseLower source)
    AnalysisBenchmark -> do
      (resolvedProgram, _) <- prepareValidCompilerScaleProgram programCase
      let inputs = compileInputs defaultWarningSettings Set.empty
      prepareFully
        ( PreparedCompilerScaleAnalysis
            inputs
            resolvedProgram
        )
    DiagnosticAnalysisBenchmark ->
      case diagnosticAnalysisInput (compilerScaleCaseScenario programCase) (compilerScaleCaseSize programCase) of
        Left message -> unsupportedCompilerScaleGroup benchmarkGroup programCase message
        Right (expression, expectedDiagnosticCount) ->
          prepareFully
            (PreparedCompilerScaleDiagnosticAnalysis expression expectedDiagnosticCount)
    ModulePreparationBenchmark -> prepareFully (PreparedCompilerScaleModulePreparation programCase)
    TypedValidationBenchmark -> do
      typedProgram <-
        fromDirectArtifact
          benchmarkGroup
          programCase
          (typedValidationProgramForScenario (compilerScaleCaseScenario programCase) (compilerScaleCaseSize programCase))
      evaluate (forceTypedProgram typedProgram)
      case validateTypedProgram typedProgram of
        [] -> prepareFully (PreparedCompilerScaleTypedValidation typedProgram)
        failures ->
          ioError (userError ("typed validation scale fixture is invalid: " <> show failures))
    LoweredValidationBenchmark -> do
      loweredProgram <-
        fromDirectArtifact
          benchmarkGroup
          programCase
          (loweredValidationProgramForScenario (compilerScaleCaseScenario programCase) (compilerScaleCaseSize programCase))
      evaluate (forceLoweredProgram loweredProgram)
      case validateLoweredProgram loweredProgram of
        [] -> prepareFully (PreparedCompilerScaleLoweredValidation loweredProgram)
        failures ->
          ioError (userError ("lowered validation scale fixture is invalid: " <> show failures))
    TypedLoweringBenchmark -> do
      typedProgram <-
        fromDirectArtifact
          benchmarkGroup
          programCase
          (typedLoweringProgramForScenario (compilerScaleCaseScenario programCase) (compilerScaleCaseSize programCase))
      evaluate (forceTypedProgram typedProgram)
      case validateTypedProgram typedProgram of
        [] -> prepareFully (PreparedCompilerScaleTypedLowering typedProgram)
        failures ->
          ioError (userError ("typed-lowering scale fixture is invalid: " <> show failures))
    WholeProgramBenchmark -> prepareFully (PreparedCompilerScaleWholeProgram programCase)
    RuntimeBenchmark -> do
      (_, analyzedProgram) <- prepareValidCompilerScaleProgram programCase
      prepareFully
        ( PreparedCompilerScaleRuntime
            (expectedCompilerScaleOutput programCase)
            analyzedProgram
        )

runPreparedBenchmark :: PreparedBenchmark -> IO ()
runPreparedBenchmark preparedBenchmark =
  case preparedBenchmark of
    PreparedParseLower source -> runParseLower source
    PreparedAnalysis inputs resolvedProgram -> do
      analysisResult <-
        withCompilerStage TypeInferenceStage $ do
          value <- ModuleCompiler.analyzeProgram inputs resolvedProgram
          evaluate (forceAnalyzedProgramResult value)
          pure value
      requireSuccessfulAnalysis analysisResult
    PreparedModulePreparation programCase -> do
      analyzedResult <-
        withCompilerStage RuntimePreparationStage (prepareProgramCase programCase)
      evaluate (forcePreparedProgramResult analyzedResult)
      case analyzedResult of
        Left diagnostic -> failBenchmarkDiagnostic diagnostic
        Right (_, diagnostics, maybeAnalyzedProgram) -> requireSuccessfulAnalysis (diagnostics, maybeAnalyzedProgram)
    PreparedRuntime expectedBehavior analyzedProgram ->
      withCompilerStage EvaluationStage $ do
        let runtimeResult = evaluateAnalyzedProgram analyzedProgram
        evaluate (forceRuntimeProgramOutputResult runtimeResult)
        requireExpectedRuntimeResult expectedBehavior runtimeResult
    PreparedWholeProgram programCase -> do
      result <- runProgramCase programCase
      evaluate (forceProgramCaseResult result)
      requireExpectedProgramResult programCase result

runPreparedCompilerScaleBenchmark :: PreparedCompilerScaleBenchmark -> IO ()
runPreparedCompilerScaleBenchmark preparedBenchmark =
  case preparedBenchmark of
    PreparedCompilerScaleParseLower source -> runParseLower source
    PreparedCompilerScaleAnalysis inputs resolvedProgram -> do
      analysisResult <-
        withCompilerStage TypeInferenceStage $ do
          value <- ModuleCompiler.analyzeProgram inputs resolvedProgram
          evaluate (forceAnalyzedProgramResult value)
          pure value
      requireSuccessfulAnalysis analysisResult
    PreparedCompilerScaleModulePreparation programCase -> do
      analyzedResult <- buildCompilerScaleProgram programCase
      evaluate (forcePreparedProgramResult analyzedResult)
      case analyzedResult of
        Left diagnostic -> failBenchmarkDiagnostic diagnostic
        Right (_, diagnostics, maybeAnalyzedProgram) -> requireSuccessfulAnalysis (diagnostics, maybeAnalyzedProgram)
    PreparedCompilerScaleRuntime expectedOutput analyzedProgram ->
      withCompilerStage EvaluationStage $ do
        let runtimeResult = evaluateAnalyzedProgram analyzedProgram
        evaluate (forceRuntimeProgramOutputResult runtimeResult)
        requireExpectedCompilerScaleRuntimeResult expectedOutput runtimeResult
    PreparedCompilerScaleLoweredValidation loweredProgram ->
      withCompilerStage LoweredIRValidationStage $
        case validateLoweredProgram loweredProgram of
          [] -> pure ()
          failures ->
            ioError (userError ("lowered validation benchmark failed: " <> show failures))
    PreparedCompilerScaleTypedValidation typedProgram ->
      withCompilerStage TypedCoreValidationStage $
        case validateTypedProgram typedProgram of
          [] -> pure ()
          failures ->
            ioError (userError ("typed validation benchmark failed: " <> show failures))
    PreparedCompilerScaleTypedLowering typedProgram -> do
      validatedProgram <-
        withCompilerStage TypedCoreValidationStage $
          case validateTypedProgramOnce typedProgram of
            Left failures ->
              ioError (userError ("trusted typed program failed producer validation: " <> show failures))
            Right value -> pure value
      withCompilerStage LoweringStage $
        case lowerValidatedTypedCoreExpressionDirectCall validatedProgram of
          LoweredIRSucceeded validatedLowered ->
            evaluate (forceLoweredProgram (validatedLoweredProgram validatedLowered))
          loweringResult ->
            ioError (userError ("typed-lowering benchmark failed: " <> show loweringResult))
    PreparedCompilerScaleDiagnosticAnalysis expression expectedDiagnosticCount ->
      withCompilerStage StaticAnalysisStage $ do
        analysisResult <- analyzeProgram defaultWarningSettings expression
        evaluate (forceListWith forceDiagnostic (analysisDiagnostics analysisResult))
        let actualDiagnosticCount = length (filter isErrorDiagnostic (analysisDiagnostics analysisResult))
        if actualDiagnosticCount == expectedDiagnosticCount
          then pure ()
          else
            ioError
              ( userError
                  ( "analyzer diagnostic benchmark produced "
                      <> show actualDiagnosticCount
                      <> " errors; expected "
                      <> show expectedDiagnosticCount
                  )
              )
    PreparedCompilerScaleWholeProgram programCase -> do
      actualOutput <- runCompilerScaleCase programCase
      if actualOutput == compilerScaleCaseExpectedOutput programCase
        then pure ()
        else
          ioError
            ( userError
                ( "compiler scale benchmark did not preserve expected output: "
                    <> Text.unpack (compilerScaleCaseIdentifier programCase)
                )
            )

runParseLower :: Text -> IO ()
runParseLower source = do
  tokens <-
    withCompilerStage LexingStage $ do
      tokenResult <- evaluate (tokenize source)
      case tokenResult of
        Left diagnostic -> failBenchmarkDiagnostic diagnostic
        Right values -> evaluate (forceTokens values) >> pure values
  surfaceProgram <-
    withCompilerStage ParsingStage $ do
      parseResult <- evaluate (parseSurfaceProgramTokens tokens)
      case parseResult of
        Left diagnostic -> failBenchmarkDiagnostic diagnostic
        Right value -> evaluate (forceSurfaceExpr value) >> pure value
  withCompilerStage LoweringStage $ do
    let expression = lowerSurfaceExpr surfaceProgram
    evaluate (forceLoweredExpr expression)

analyzerDiagnosticChainExpression :: Int -> Expr 'Resolved
analyzerDiagnosticChainExpression expressionCount =
  case expressionCount of
    count
      | count <= 0 -> EBlock (diagnosticNode 0) []
      | otherwise ->
          foldl'
            (\left index -> EApply (diagnosticNode (count - 1 - index)) left (missingVariable (count - 1 + index) index))
            (missingVariable (count - 1) 0)
            [1 .. count - 1]
  where
    missingVariable :: Int -> Int -> Expr 'Resolved
    missingVariable nodeId index =
      EVar
        (diagnosticNode nodeId)
        (resolvedLocalName ValueNamespace (mkIdentifier ("missing" <> Text.pack (show index))))

    diagnosticNode :: Int -> CoreNode 'Resolved sort
    diagnosticNode nodeId = CoreNode (CoreNodeId nodeId) (SourceSpan 1 1) ()

diagnosticAnalysisInput :: CompilerScaleScenario -> Int -> Either Text (Expr 'Resolved, Int)
diagnosticAnalysisInput scenario size =
  case scenario of
    AnalyzerDiagnosticChain -> Right (analyzerDiagnosticChainExpression size, size)
    SequentialPolymorphicBindings -> unsupported
    WideModuleFanout -> unsupported
    SharedInterfaceFanout -> unsupported
    NestedRuntimeApplications -> unsupported
    RuntimeImportWidth -> unsupported
    ResolverFactRich -> unsupported
    TypedValidationHandoff -> unsupported
    LoweredTemporaryValidation -> unsupported
    TypedRecursiveStatementGraph -> unsupported
    TypedForwardSignedFunctions -> unsupported
    TypedWideExportProviders -> unsupported
    WideConstructorApplication -> unsupported
    CapabilityCandidateWidth -> unsupported
    HostFreeOpaqueEnvironment -> unsupported
    InterleavedRecursiveGroups -> unsupported
    RecursivePreviewBursts -> unsupported
    RecursiveRebindings -> unsupported
    ConstrainedSignatures -> unsupported
    DeferredConstraintBursts -> unsupported
    DeepNestedLambdas -> unsupported
    LargeOperatorTables -> unsupported
    NestedBlocks -> unsupported
    AmbiguousCaseArmPipes -> unsupported
    LongTokenStream -> unsupported
    IdentifierTokenStream -> unsupported
    LiteralTokenStream -> unsupported
  where
    unsupported = Left "scenario has no direct analyzer-diagnostic artifact"

typedValidationProgramForScenario :: CompilerScaleScenario -> Int -> Either Text TypedProgram
typedValidationProgramForScenario scenario size =
  case scenario of
    TypedRecursiveStatementGraph -> Right (typedRecursiveStatementGraphProgram size)
    TypedWideExportProviders -> Right (typedWideExportProvidersProgram size)
    SequentialPolymorphicBindings -> unsupported
    WideModuleFanout -> unsupported
    SharedInterfaceFanout -> unsupported
    NestedRuntimeApplications -> unsupported
    RuntimeImportWidth -> unsupported
    ResolverFactRich -> unsupported
    TypedValidationHandoff -> unsupported
    LoweredTemporaryValidation -> unsupported
    TypedForwardSignedFunctions -> unsupported
    WideConstructorApplication -> unsupported
    CapabilityCandidateWidth -> unsupported
    HostFreeOpaqueEnvironment -> unsupported
    AnalyzerDiagnosticChain -> unsupported
    InterleavedRecursiveGroups -> unsupported
    RecursivePreviewBursts -> unsupported
    RecursiveRebindings -> unsupported
    ConstrainedSignatures -> unsupported
    DeferredConstraintBursts -> unsupported
    DeepNestedLambdas -> unsupported
    LargeOperatorTables -> unsupported
    NestedBlocks -> unsupported
    AmbiguousCaseArmPipes -> unsupported
    LongTokenStream -> unsupported
    IdentifierTokenStream -> unsupported
    LiteralTokenStream -> unsupported
  where
    unsupported = Left "scenario has no direct Typed Core validation artifact"

loweredValidationProgramForScenario :: CompilerScaleScenario -> Int -> Either Text LoweredProgram
loweredValidationProgramForScenario scenario size =
  case scenario of
    LoweredTemporaryValidation -> Right (loweredTemporaryValidationProgram size)
    SequentialPolymorphicBindings -> unsupported
    WideModuleFanout -> unsupported
    SharedInterfaceFanout -> unsupported
    NestedRuntimeApplications -> unsupported
    RuntimeImportWidth -> unsupported
    ResolverFactRich -> unsupported
    TypedValidationHandoff -> unsupported
    TypedRecursiveStatementGraph -> unsupported
    TypedForwardSignedFunctions -> unsupported
    TypedWideExportProviders -> unsupported
    WideConstructorApplication -> unsupported
    CapabilityCandidateWidth -> unsupported
    HostFreeOpaqueEnvironment -> unsupported
    AnalyzerDiagnosticChain -> unsupported
    InterleavedRecursiveGroups -> unsupported
    RecursivePreviewBursts -> unsupported
    RecursiveRebindings -> unsupported
    ConstrainedSignatures -> unsupported
    DeferredConstraintBursts -> unsupported
    DeepNestedLambdas -> unsupported
    LargeOperatorTables -> unsupported
    NestedBlocks -> unsupported
    AmbiguousCaseArmPipes -> unsupported
    LongTokenStream -> unsupported
    IdentifierTokenStream -> unsupported
    LiteralTokenStream -> unsupported
  where
    unsupported = Left "scenario has no direct Lowered IR validation artifact"

typedLoweringProgramForScenario :: CompilerScaleScenario -> Int -> Either Text TypedProgram
typedLoweringProgramForScenario scenario size =
  case scenario of
    TypedValidationHandoff -> Right (typedValidationBenchmarkProgram size)
    TypedForwardSignedFunctions -> Right (typedForwardSignedFunctionsProgram size)
    SequentialPolymorphicBindings -> unsupported
    WideModuleFanout -> unsupported
    SharedInterfaceFanout -> unsupported
    NestedRuntimeApplications -> unsupported
    RuntimeImportWidth -> unsupported
    ResolverFactRich -> unsupported
    LoweredTemporaryValidation -> unsupported
    TypedRecursiveStatementGraph -> unsupported
    TypedWideExportProviders -> unsupported
    WideConstructorApplication -> unsupported
    CapabilityCandidateWidth -> unsupported
    HostFreeOpaqueEnvironment -> unsupported
    AnalyzerDiagnosticChain -> unsupported
    InterleavedRecursiveGroups -> unsupported
    RecursivePreviewBursts -> unsupported
    RecursiveRebindings -> unsupported
    ConstrainedSignatures -> unsupported
    DeferredConstraintBursts -> unsupported
    DeepNestedLambdas -> unsupported
    LargeOperatorTables -> unsupported
    NestedBlocks -> unsupported
    AmbiguousCaseArmPipes -> unsupported
    LongTokenStream -> unsupported
    IdentifierTokenStream -> unsupported
    LiteralTokenStream -> unsupported
  where
    unsupported = Left "scenario has no direct Typed Core lowering artifact"

prepareValidProgram :: ProgramCase -> IO (CoreProgram 'Resolved, CoreProgram 'Analyzed)
prepareValidProgram programCase = do
  analyzedResult <- prepareProgramCase programCase
  evaluate (forcePreparedProgramResult analyzedResult)
  case analyzedResult of
    Left diagnostic -> failBenchmarkDiagnostic diagnostic
    Right (resolvedProgram, diagnostics, maybeAnalyzedProgram) -> do
      requireSuccessfulAnalysis (diagnostics, maybeAnalyzedProgram)
      case maybeAnalyzedProgram of
        Just analyzedProgram -> pure (resolvedProgram, analyzedProgram)
        Nothing -> ioError (userError "successful analysis did not produce analyzed core")

buildCompilerScaleProgram :: CompilerScaleCase -> IO (Either Diagnostic (CoreProgram 'Resolved, [Diagnostic], Maybe (CoreProgram 'Analyzed)))
buildCompilerScaleProgram programCase =
  buildAnalyzedProgram
    defaultWarningSettings
    (PreludeBundled bundledPreludeSource)
    (compilerScaleCaseResolutionConfig programCase)
    (compilerScaleCaseEntryModulePath programCase)
    (pure . compilerScaleCaseSource programCase)

prepareValidCompilerScaleProgram :: CompilerScaleCase -> IO (CoreProgram 'Resolved, CoreProgram 'Analyzed)
prepareValidCompilerScaleProgram programCase = do
  analyzedResult <- buildCompilerScaleProgram programCase
  evaluate (forcePreparedProgramResult analyzedResult)
  case analyzedResult of
    Left diagnostic -> failBenchmarkDiagnostic diagnostic
    Right (resolvedProgram, diagnostics, maybeAnalyzedProgram) -> do
      requireSuccessfulAnalysis (diagnostics, maybeAnalyzedProgram)
      case maybeAnalyzedProgram of
        Just analyzedProgram -> pure (resolvedProgram, analyzedProgram)
        Nothing -> ioError (userError "successful analysis did not produce analyzed core")

runCompilerScaleCase :: CompilerScaleCase -> IO Text
runCompilerScaleCase programCase = do
  (_, analyzedProgram) <- prepareValidCompilerScaleProgram programCase
  withCompilerStage EvaluationStage $ do
    let runtimeResult = evaluateAnalyzedProgram analyzedProgram
    evaluate (forceRuntimeProgramOutputResult runtimeResult)
    case runtimeResult of
      Left diagnostic -> failBenchmarkDiagnostic diagnostic
      Right runtimeProgram ->
        pure (maybe "" renderRuntimeValue (runtimeProgramOutput runtimeProgram))

typedValidationBenchmarkProgram :: Int -> TypedProgram
typedValidationBenchmarkProgram expressionCount =
  TypedProgram
    Nothing
    [ TypedModule
        modulePath
        (TypedSourcePath "compiler-scale/TypedValidation.jz")
        []
        []
        (TypedModuleInterface [] [] [] [])
        []
        [TypedExpressionStatement (TypedSpan 1 1) expression]
        intInfo
    ]
    modulePath
  where
    modulePath = ["TypedValidation"]
    intInfo = TypedNodeInfo SemanticInt (TypedSignedIntegerRecipe 64) [] []
    intExpression :: Int -> TypedExpr
    intExpression value =
      TypedLiteralExpr intInfo (TypedIntegerLiteral (Text.pack (show value)))
    expression =
      foldl'
        (\left value -> TypedBinaryExpr intInfo (TypedBuiltinOperator "+") left (intExpression value))
        (intExpression 0)
        [1 .. expressionCount]

loweredTemporaryValidationProgram :: Int -> LoweredProgram
loweredTemporaryValidationProgram instructionCount
  | instructionCount <= 0 = error "lowered temporary validation size must be positive"
  | otherwise =
      LoweredProgram
        supportedLoweredIRVersion
        []
        []
        [ LoweredFunction
            functionId
            Nothing
            []
            int64Representation
            [LoweredBlock blockId [] instructions (Just (LoweredReturn finalOperand))]
            blockId
        ]
        functionId
  where
    functionId = LoweredFunctionId "main"
    blockId = LoweredBlockId "entry"
    int64Representation = LoweredSignedIntegerRepresentation LoweredIntegerWidth64
    temporaryId instructionIndex =
      LoweredTemporaryId ("value" <> Text.justifyRight 5 '0' (Text.pack (show instructionIndex)))
    immediate value =
      LoweredImmediateOperand
        (LoweredSignedIntegerImmediate LoweredIntegerWidth64 value)
    temporary instructionIndex =
      LoweredTemporaryOperand (temporaryId instructionIndex) int64Representation
    operandFor instructionIndex
      | instructionIndex == 0 = immediate 0
      | otherwise = temporary (instructionIndex - 1)
    instructions =
      [ LoweredInstruction
          (temporaryId instructionIndex)
          int64Representation
          ( LoweredPrimitiveOperation
              (LoweredArithmeticPrimitive LoweredAdd)
              [operandFor instructionIndex, immediate 1]
          )
      | instructionIndex <- [0 .. instructionCount - 1]
      ]
    finalOperand = temporary (instructionCount - 1)

typedRecursiveStatementGraphProgram :: Int -> TypedProgram
typedRecursiveStatementGraphProgram statementCount
  | statementCount < graphGroupWidth || statementCount `rem` graphGroupWidth /= 0 =
      error "typed recursive statement graph size must be a positive multiple of eight"
  | otherwise =
      TypedProgram
        Nothing
        [ TypedModule
            modulePath
            (TypedSourcePath "compiler-scale/TypedRecursiveStatementGraph.jz")
            []
            []
            (TypedModuleInterface [] [] [] [])
            recursiveGroups
            (bindings <> [TypedExpressionStatement spanValue terminalExpression])
            boolInfo
        ]
        modulePath
  where
    graphGroupWidth = 8
    groupCount = statementCount `div` graphGroupWidth
    recursiveGroups = map recursiveGroup [0 .. groupCount - 1]
    bindings = concatMap graphGroup [0 .. groupCount - 1]
    modulePath = ["TypedRecursiveStatementGraph"]
    spanValue = TypedSpan 1 1
    boolInfo = TypedNodeInfo SemanticBool TypedBoolRecipe [] []
    trueExpression = TypedLiteralExpr boolInfo (TypedBooleanLiteral True)
    historyName =
      TypedResolvedName TypedCurrentModule TypedValueNamespace "history"
    graphName prefix groupIndex =
      TypedResolvedName
        TypedCurrentModule
        TypedValueNamespace
        (prefix <> Text.justifyRight 4 '0' (Text.pack (show groupIndex)))
    graphOwner statementIndex name =
      TypedBinderId (modulePath, [statementIndex], name)
    lambdaOwner statementIndex name =
      TypedBinderId (modulePath, [statementIndex, 0], name)
    variable info owner name = TypedVariableExpr info name (Just owner)
    boundVariable = variable boolInfo
    scalarBinding owner name expression =
      TypedLetStatement
        owner
        name
        spanValue
        (TypedScheme owner [] [] [] SemanticBool TypedBoolRecipe Nothing)
        expression
    functionType = SemanticFunction SemanticBool SemanticBool
    functionRecipe = TypedClosureRecipe [TypedBoolRecipe] TypedBoolRecipe
    functionInfo = TypedNodeInfo functionType functionRecipe [] []
    functionBinding owner name argumentOwner argumentName body =
      TypedLetStatement
        owner
        name
        spanValue
        (TypedScheme owner [] [] [] functionType functionRecipe (Just TypedDirectCallableShape))
        (TypedLambdaExpr functionInfo argumentOwner argumentName body)
    recursiveGroup groupIndex =
      TypedRecursiveGroup [mutualLeftOwner groupIndex, mutualRightOwner groupIndex]
    graphGroup groupIndex =
      [ scalarBinding firstHistoryOwner historyName firstHistoryExpression,
        scalarBinding chainOneOwner chainOneName (boundVariable firstHistoryOwner historyName),
        scalarBinding secondHistoryOwner historyName (boundVariable chainOneOwner chainOneName),
        scalarBinding chainTwoOwner chainTwoName (boundVariable secondHistoryOwner historyName),
        functionBinding
          leftOwner
          leftName
          leftArgumentOwner
          leftArgumentName
          ( TypedApplyExpr
              boolInfo
              (variable functionInfo rightOwner rightName)
              (boundVariable leftArgumentOwner leftArgumentName)
          ),
        scalarBinding chainThreeOwner chainThreeName (boundVariable chainTwoOwner chainTwoName),
        functionBinding
          rightOwner
          rightName
          rightArgumentOwner
          rightArgumentName
          ( TypedApplyExpr
              boolInfo
              (variable functionInfo leftOwner leftName)
              (boundVariable rightArgumentOwner rightArgumentName)
          ),
        scalarBinding
          tailOwner
          tailName
          ( TypedIfExpr
              boolInfo
              (boundVariable chainThreeOwner chainThreeName)
              (TypedApplyExpr boolInfo (variable functionInfo rightOwner rightName) trueExpression)
              (boundVariable chainThreeOwner chainThreeName)
          )
      ]
      where
        baseIndex = groupIndex * graphGroupWidth
        chainOneName = graphName "chainOne" groupIndex
        chainTwoName = graphName "chainTwo" groupIndex
        chainThreeName = graphName "chainThree" groupIndex
        leftName = mutualLeftName groupIndex
        rightName = mutualRightName groupIndex
        leftArgumentName = graphName "leftArgument" groupIndex
        rightArgumentName = graphName "rightArgument" groupIndex
        tailName = graphName "tail" groupIndex
        firstHistoryOwner = graphOwner baseIndex historyName
        chainOneOwner = graphOwner (baseIndex + 1) chainOneName
        secondHistoryOwner = graphOwner (baseIndex + 2) historyName
        chainTwoOwner = graphOwner (baseIndex + 3) chainTwoName
        leftOwner = mutualLeftOwner groupIndex
        leftArgumentOwner = lambdaOwner (baseIndex + 4) leftArgumentName
        chainThreeOwner = graphOwner (baseIndex + 5) chainThreeName
        rightOwner = mutualRightOwner groupIndex
        rightArgumentOwner = lambdaOwner (baseIndex + 6) rightArgumentName
        tailOwner = graphOwner (baseIndex + 7) tailName
        firstHistoryExpression
          | groupIndex == 0 = trueExpression
          | otherwise =
              let previousTailName = graphName "tail" (groupIndex - 1)
                  previousTailOwner = graphOwner (baseIndex - 1) previousTailName
               in boundVariable previousTailOwner previousTailName
    mutualLeftName = graphName "mutualLeft"
    mutualRightName = graphName "mutualRight"
    mutualLeftOwner groupIndex =
      graphOwner (groupIndex * graphGroupWidth + 4) (mutualLeftName groupIndex)
    mutualRightOwner groupIndex =
      graphOwner (groupIndex * graphGroupWidth + 6) (mutualRightName groupIndex)
    terminalName = graphName "tail" (groupCount - 1)
    terminalOwner = graphOwner (statementCount - 1) terminalName
    terminalExpression = boundVariable terminalOwner terminalName

typedWideExportProvidersProgram :: Int -> TypedProgram
typedWideExportProvidersProgram providerCount
  | providerCount <= 0 = error "typed wide export provider count must be positive"
  | otherwise =
      TypedProgram
        Nothing
        [ TypedModule
            modulePath
            (TypedSourcePath "compiler-scale/TypedWideExportProviders.jz")
            []
            exports
            (TypedModuleInterface interfaces [] [] [])
            []
            (bindings <> [TypedExpressionStatement spanValue trueExpression])
            boolInfo
        ]
        modulePath
  where
    modulePath = ["TypedWideExportProviders"]
    spanValue = TypedSpan 1 1
    boolInfo = TypedNodeInfo SemanticBool TypedBoolRecipe [] []
    trueExpression = TypedLiteralExpr boolInfo (TypedBooleanLiteral True)

    providerIdentifier index =
      "provided" <> Text.justifyRight 4 '0' (Text.pack (show index))
    providerName index =
      TypedResolvedName
        TypedCurrentModule
        TypedValueNamespace
        (providerIdentifier index)
    providerOwner index = TypedBinderId (modulePath, [index], providerName index)
    providerScheme index =
      TypedScheme
        (providerOwner index)
        []
        []
        []
        SemanticBool
        TypedBoolRecipe
        Nothing

    bindings =
      [ TypedLetStatement
          (providerOwner index)
          (providerName index)
          spanValue
          (providerScheme index)
          trueExpression
      | index <- [0 .. providerCount - 1]
      ]
    interfaces =
      [TypedValueInterface (providerName index) (providerScheme index) | index <- [0 .. providerCount - 1]]
    exports =
      [TypedModuleExport TypedValueNamespace (providerIdentifier index) | index <- [0 .. providerCount - 1]]

typedForwardSignedFunctionsProgram :: Int -> TypedProgram
typedForwardSignedFunctionsProgram functionCount
  | functionCount <= 0 = error "typed forward signed function count must be positive"
  | otherwise =
      TypedProgram
        Nothing
        [ TypedModule
            modulePath
            source
            []
            []
            (TypedModuleInterface [] [] [] [])
            []
            (concatMap functionPair [0 .. functionCount - 1] <> [terminalStatement])
            boolInfo
        ]
        modulePath
  where
    modulePath = ["TypedForwardSignedFunctions"]
    source = TypedSourcePath "compiler-scale/TypedForwardSignedFunctions.jz"
    statementSpan = TypedSpan 1 1
    boolInfo = TypedNodeInfo SemanticBool TypedBoolRecipe [] []
    functionType = SemanticFunction SemanticBool SemanticBool
    functionRecipe = TypedClosureRecipe [TypedBoolRecipe] TypedBoolRecipe
    functionInfo = TypedNodeInfo functionType functionRecipe [] []

    indexedName :: Text -> Int -> TypedCoreName
    indexedName prefix index =
      TypedResolvedName
        TypedCurrentModule
        TypedValueNamespace
        (prefix <> Text.justifyRight 4 '0' (Text.pack (show index)))

    functionName :: Int -> TypedCoreName
    functionName index = indexedName "forward" index

    argumentName :: Int -> TypedCoreName
    argumentName index = indexedName "argument" index

    signatureOwner index =
      TypedBinderId (modulePath, [2 * index], functionName index)
    bindingOwner index =
      TypedBinderId (modulePath, [2 * index + 1], functionName index)
    argumentOwner index =
      TypedBinderId (modulePath, [2 * index + 1, 0], argumentName index)

    functionScheme owner =
      TypedScheme
        owner
        []
        []
        []
        functionType
        functionRecipe
        (Just TypedDirectCallableShape)

    variable info owner name = TypedVariableExpr info name (Just owner)

    functionBody index
      | index == functionCount - 1 =
          variable boolInfo (argumentOwner index) (argumentName index)
      | otherwise =
          TypedApplyExpr
            boolInfo
            ( variable
                functionInfo
                (bindingOwner (index + 1))
                (functionName (index + 1))
            )
            (variable boolInfo (argumentOwner index) (argumentName index))

    functionPair index =
      [ TypedSignatureStatement
          (signatureOwner index)
          (functionName index)
          statementSpan
          (functionScheme (signatureOwner index)),
        TypedLetStatement
          (bindingOwner index)
          (functionName index)
          statementSpan
          (functionScheme (bindingOwner index))
          ( TypedLambdaExpr
              functionInfo
              (argumentOwner index)
              (argumentName index)
              (functionBody index)
          )
      ]

    terminalStatement =
      TypedExpressionStatement
        statementSpan
        ( TypedApplyExpr
            boolInfo
            (variable functionInfo (bindingOwner 0) (functionName 0))
            (TypedLiteralExpr boolInfo (TypedBooleanLiteral True))
        )

unsupportedCorpusGroup :: BenchmarkGroup -> ProgramCase -> IO value
unsupportedCorpusGroup benchmarkGroup programCase =
  ioError
    ( userError
        ( "unsupported corpus benchmark group for "
            <> Text.unpack (programCaseIdentifier programCase)
            <> ": "
            <> show benchmarkGroup
        )
    )

unsupportedCompilerScaleGroup :: BenchmarkGroup -> CompilerScaleCase -> Text -> IO value
unsupportedCompilerScaleGroup benchmarkGroup programCase reason =
  ioError
    ( userError
        ( "unsupported compiler scale benchmark group for "
            <> Text.unpack (compilerScaleCaseIdentifier programCase)
            <> ": "
            <> show benchmarkGroup
            <> " ("
            <> Text.unpack reason
            <> ")"
        )
    )

fromDirectArtifact :: BenchmarkGroup -> CompilerScaleCase -> Either Text value -> IO value
fromDirectArtifact benchmarkGroup programCase result =
  case result of
    Left reason -> unsupportedCompilerScaleGroup benchmarkGroup programCase reason
    Right value -> pure value

prepareFully :: (NFData prepared) => prepared -> IO prepared
prepareFully prepared = evaluate (rnf prepared) >> pure prepared

expectedProgramBehavior :: ProgramCase -> ExpectedProgramBehavior
expectedProgramBehavior programCase =
  ExpectedProgramBehavior
    (programCaseIdentifier programCase)
    (programCaseExpectedTermination programCase)
    (programCaseExpectedStdout programCase)

forceExpectedProgramBehavior :: ExpectedProgramBehavior -> ()
forceExpectedProgramBehavior (ExpectedProgramBehavior identifier termination stdout) =
  identifier `seq` termination `seq` stdout `seq` ()

expectedCompilerScaleOutput :: CompilerScaleCase -> ExpectedCompilerScaleOutput
expectedCompilerScaleOutput programCase =
  ExpectedCompilerScaleOutput
    (compilerScaleCaseIdentifier programCase)
    (compilerScaleCaseExpectedOutput programCase)

forceExpectedCompilerScaleOutput :: ExpectedCompilerScaleOutput -> ()
forceExpectedCompilerScaleOutput (ExpectedCompilerScaleOutput identifier output) =
  identifier `seq` output `seq` ()

forceProgramCase :: ProgramCase -> ()
forceProgramCase programCase =
  programCaseIdentifier programCase `seq`
    forceString (programCasePackageRoot programCase) `seq`
      forceString (programCaseDirectory programCase) `seq`
        forceString (programCaseEntrySource programCase) `seq`
          forceString (programCaseModuleRoot programCase) `seq`
            forceListWith (`seq` ()) (programCaseEntryModulePath programCase) `seq`
              programCaseExpectedTermination programCase `seq`
                forceString (programCaseExpectedStdoutPath programCase) `seq`
                  programCaseExpectedStdout programCase `seq`
                    programCaseWorkload programCase `seq`
                      forceListWith (`seq` ()) (programCaseFeatures programCase) `seq`
                        forceListWith (`seq` ()) (programCaseBenchmarks programCase) `seq`
                          forceProgramBudgets (programCaseBudgets programCase)

forceProgramBudgets :: ProgramBudgets -> ()
forceProgramBudgets budgets =
  programBudgetSteps budgets `seq`
    programBudgetApplications budgets `seq`
      programBudgetMaxContinuationDepth budgets `seq`
        forceListWith forceBudgetLimit (Map.toList (programBudgetOptionalLimits budgets))
  where
    forceBudgetLimit (metric, limit) = metric `seq` limit `seq` ()

forceString :: String -> ()
forceString = forceListWith (`seq` ())

forcePreparedProgramResult :: Either Diagnostic (CoreProgram 'Resolved, [Diagnostic], Maybe (CoreProgram 'Analyzed)) -> ()
forcePreparedProgramResult result =
  case result of
    Left diagnostic -> forceDiagnostic diagnostic
    Right (resolvedProgram, diagnostics, maybeAnalyzedProgram) ->
      rnf resolvedProgram `seq` forceAnalyzedProgramResult (diagnostics, maybeAnalyzedProgram)

requireSuccessfulAnalysis :: ([Diagnostic], Maybe (CoreProgram 'Analyzed)) -> IO ()
requireSuccessfulAnalysis (diagnostics, maybeAnalyzedProgram) =
  case filter isErrorDiagnostic diagnostics of
    diagnostic : _ -> failBenchmarkDiagnostic diagnostic
    [] ->
      case maybeAnalyzedProgram of
        Just _ -> pure ()
        Nothing -> ioError (userError "analysis produced no errors and no analyzed core")

requireExpectedRuntimeResult :: ExpectedProgramBehavior -> Either Diagnostic RuntimeProgram -> IO ()
requireExpectedRuntimeResult (ExpectedProgramBehavior identifier expectedTermination expectedStdout) runtimeResult =
  let actualTermination =
        case runtimeResult of
          Left _ -> RuntimeFailedProgram
          Right _ -> SuccessfulProgram
      actualStdout =
        case runtimeResult of
          Left _ -> ""
          Right runtimeProgram -> maybe "" ((<> "\n") . renderRuntimeValue) (runtimeProgramOutput runtimeProgram)
   in requireExpectedBehavior identifier expectedTermination expectedStdout actualTermination actualStdout

requireExpectedCompilerScaleRuntimeResult :: ExpectedCompilerScaleOutput -> Either Diagnostic RuntimeProgram -> IO ()
requireExpectedCompilerScaleRuntimeResult (ExpectedCompilerScaleOutput identifier expectedOutput) runtimeResult =
  case runtimeResult of
    Left diagnostic -> failBenchmarkDiagnostic diagnostic
    Right runtimeProgram ->
      let actualOutput = maybe "" renderRuntimeValue (runtimeProgramOutput runtimeProgram)
       in if actualOutput == expectedOutput
            then pure ()
            else
              ioError
                ( userError
                    ( "compiler scale runtime benchmark did not preserve expected output: "
                        <> Text.unpack identifier
                    )
                )

requireExpectedProgramResult :: ProgramCase -> ProgramCaseResult -> IO ()
requireExpectedProgramResult programCase result =
  requireExpectedBehavior
    (programCaseIdentifier programCase)
    (programCaseExpectedTermination programCase)
    (programCaseExpectedStdout programCase)
    (programCaseResultTermination result)
    (programCaseResultStdout result)

requireExpectedBehavior :: Text -> ProgramTermination -> Text -> ProgramTermination -> Text -> IO ()
requireExpectedBehavior identifier expectedTermination expectedStdout actualTermination actualStdout
  | actualTermination == expectedTermination,
    actualStdout == expectedStdout =
      pure ()
  | otherwise =
      ioError
        ( userError
            ( "benchmark case did not preserve expected behavior: "
                <> Text.unpack identifier
            )
        )

failBenchmarkDiagnostic :: Diagnostic -> IO value
failBenchmarkDiagnostic diagnostic =
  ioError (userError (Text.unpack (renderDiagnostic diagnostic)))

selectProgramCases :: [Text] -> [ProgramCase] -> Either Text [ProgramCase]
selectProgramCases requestedIdentifiers programCases
  | null requestedIdentifiers = Right programCases
  | not (null missingIdentifiers) =
      Left ("unknown corpus case(s): " <> Text.intercalate ", " missingIdentifiers)
  | otherwise =
      Right
        ( filter
            ((`elem` requestedIdentifiers) . programCaseIdentifier)
            programCases
        )
  where
    knownIdentifiers = map programCaseIdentifier programCases
    missingIdentifiers = filter (`notElem` knownIdentifiers) requestedIdentifiers
