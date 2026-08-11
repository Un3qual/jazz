{-# LANGUAGE OverloadedStrings #-}

module Jazz.Benchmark.StageInputs
  ( PreparedBenchmark (PreparedAnalysis),
    PreparedCompilerScaleBenchmark (PreparedCompilerScaleAnalysis),
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
import Data.Text (Text)
import qualified Data.Text as Text
import Jazz.Benchmark.Force
  ( forceCompiledModule,
    forceCompiledModules,
    forceCompiledProgram,
    forceCompiledProgramResult,
    forceDiagnostic,
    forceExpr,
    forceListWith,
    forceProgramCaseResult,
    forceResolvedModule,
    forceRuntimeProgramOutputResult,
    forceSurfaceExpr,
    forceTokens,
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
import Jazz.Compiler.AST (Expr (..))
import Jazz.Compiler.Analyzer
  ( AnalysisResult (..),
    analyzeProgram,
  )
import Jazz.Compiler.BundledPrelude (bundledPreludeSource)
import Jazz.Compiler.Diagnostics (Diagnostic, isErrorDiagnostic)
import Jazz.Compiler.Diagnostics.Render (renderDiagnostic)
import Jazz.Compiler.Driver (ResolvedPrelude (PreludeBundled), buildCompiledProgram)
import Jazz.Compiler.LoweredIR
import Jazz.Compiler.LoweredIR.Lower
  ( LoweredIRLoweringResult (..),
    lowerValidatedTypedCoreExpressionDirectCall,
  )
import Jazz.Compiler.LoweredIR.Validate (validateLoweredProgram)
import Jazz.Compiler.ModuleCompiler (compileResolvedModule)
import Jazz.Compiler.ModuleGraph
  ( ResolvedModule (..),
    ResolvedProgram (..),
  )
import Jazz.Compiler.ModuleInterface
  ( CompileInputs,
    CompiledModule (..),
    CompiledProgram (..),
    compileInputs,
    compiledModuleErrors,
    compiledProgramErrors,
  )
import Jazz.Compiler.ModuleResolver
  ( ModuleResolutionConfig,
    resolveProgramWithAmbientExports,
  )
import Jazz.Compiler.ModuleRuntime
  ( RuntimeProgram (runtimeProgramOutput),
    evaluateCompiledProgram,
  )
import Jazz.Compiler.Name (mkIdentifier, sourceName)
import Jazz.Compiler.Parser (parseSurfaceProgramTokens)
import Jazz.Compiler.Parser.Lexer (tokenize)
import Jazz.Compiler.Parser.Lower (lowerSurfaceExpr)
import Jazz.Compiler.Prelude
  ( PreparedPrelude (..),
    preparePrelude,
  )
import Jazz.Compiler.Profiling
  ( BenchmarkGroup (..),
    CompilerStage (..),
    withCompilerStage,
  )
import Jazz.Compiler.Runtime (renderRuntimeValue)
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
    programCaseResolutionConfig,
    readProgramCaseSource,
    runProgramCase,
  )
import Jazz.ProgramCorpus.Types
  ( ProgramCase (..),
    ProgramTermination (..),
  )

data PreparedBenchmark
  = PreparedParseLower ProgramCase Text
  | PreparedAnalysis ProgramCase CompiledProgram CompileInputs [CompiledModule] ResolvedModule
  | PreparedModulePreparation ProgramCase
  | PreparedRuntime ProgramCase CompiledProgram
  | PreparedWholeProgram ProgramCase

data PreparedCompilerScaleBenchmark
  = PreparedCompilerScaleParseLower CompilerScaleCase Text
  | PreparedCompilerScaleAnalysis CompilerScaleCase CompiledProgram CompileInputs [CompiledModule] ResolvedModule
  | PreparedCompilerScaleModulePreparation CompilerScaleCase
  | PreparedCompilerScaleRuntime CompilerScaleCase CompiledProgram
  | PreparedCompilerScaleLoweredValidation CompilerScaleCase LoweredProgram
  | PreparedCompilerScaleTypedLowering CompilerScaleCase TypedProgram
  | PreparedCompilerScaleDiagnosticAnalysis CompilerScaleCase Expr Int
  | PreparedCompilerScaleWholeProgram CompilerScaleCase

instance NFData PreparedBenchmark where
  rnf preparedBenchmark =
    case preparedBenchmark of
      PreparedParseLower programCase source ->
        programCaseIdentifier programCase `seq` Text.length source `seq` ()
      PreparedAnalysis programCase compiledProgram inputs dependencies resolvedModule ->
        programCaseIdentifier programCase `seq`
          forceCompiledProgram compiledProgram `seq`
            inputs `seq`
              forceCompiledModules dependencies `seq`
                forceResolvedModule resolvedModule `seq`
                  ()
      PreparedModulePreparation programCase -> programCaseIdentifier programCase `seq` ()
      PreparedRuntime programCase compiledProgram ->
        programCaseIdentifier programCase `seq` forceCompiledProgram compiledProgram
      PreparedWholeProgram programCase -> programCaseIdentifier programCase `seq` ()

instance NFData PreparedCompilerScaleBenchmark where
  rnf preparedBenchmark =
    case preparedBenchmark of
      PreparedCompilerScaleParseLower programCase source ->
        rnf programCase `seq` Text.length source `seq` ()
      PreparedCompilerScaleAnalysis programCase compiledProgram inputs dependencies resolvedModule ->
        rnf programCase `seq`
          forceCompiledProgram compiledProgram `seq`
            inputs `seq`
              forceCompiledModules dependencies `seq`
                forceResolvedModule resolvedModule `seq`
                  ()
      PreparedCompilerScaleModulePreparation programCase -> rnf programCase
      PreparedCompilerScaleRuntime programCase compiledProgram ->
        rnf programCase `seq` forceCompiledProgram compiledProgram
      PreparedCompilerScaleLoweredValidation programCase loweredProgram ->
        rnf programCase `seq` forceLoweredProgramArtifact loweredProgram
      PreparedCompilerScaleTypedLowering programCase typedProgram ->
        rnf programCase `seq` forceTypedProgramArtifact typedProgram
      PreparedCompilerScaleDiagnosticAnalysis programCase expression expectedDiagnosticCount ->
        rnf programCase `seq` forceExpr expression `seq` rnf expectedDiagnosticCount
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
      pure (PreparedParseLower programCase source)
    AnalysisBenchmark -> do
      compiledProgram <- prepareValidProgram programCase
      resolvedProgram <-
        resolveBenchmarkProgram
          (programCaseResolutionConfig programCase)
          (programCaseEntryModulePath programCase)
          readProgramCaseSource
      entryModule <- requireResolvedEntryModule (programCaseEntryModulePath programCase) resolvedProgram
      let entryPath = compiledProgramEntryPath compiledProgram
          dependencies =
            filter
              ((/= entryPath) . compiledModulePath)
              (compiledProgramModules compiledProgram)
          inputs = compileInputs defaultWarningSettings (compiledProgramPrelude compiledProgram)
      pure
        ( PreparedAnalysis
            programCase
            compiledProgram
            inputs
            dependencies
            entryModule
        )
    ModulePreparationBenchmark -> pure (PreparedModulePreparation programCase)
    TypedLoweringBenchmark -> unsupportedCorpusGroup benchmarkGroup programCase
    RuntimeBenchmark -> PreparedRuntime programCase <$> prepareValidProgram programCase
    WholeProgramBenchmark -> pure (PreparedWholeProgram programCase)

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
      pure (PreparedCompilerScaleParseLower programCase source)
    AnalysisBenchmark ->
      case compilerScaleCaseScenario programCase of
        AnalyzerDiagnosticChain -> do
          let expression = analyzerDiagnosticChainExpression (compilerScaleCaseSize programCase)
          evaluate (forceExpr expression)
          pure
            ( PreparedCompilerScaleDiagnosticAnalysis
                programCase
                expression
                (compilerScaleCaseSize programCase)
            )
        _ -> do
          compiledProgram <- prepareValidCompilerScaleProgram programCase
          resolvedProgram <-
            resolveBenchmarkProgram
              (compilerScaleCaseResolutionConfig programCase)
              (compilerScaleCaseEntryModulePath programCase)
              (pure . compilerScaleCaseSource programCase)
          entryModule <- requireResolvedEntryModule (compilerScaleCaseEntryModulePath programCase) resolvedProgram
          let entryPath = compiledProgramEntryPath compiledProgram
              dependencies =
                filter
                  ((/= entryPath) . compiledModulePath)
                  (compiledProgramModules compiledProgram)
              inputs = compileInputs defaultWarningSettings (compiledProgramPrelude compiledProgram)
          pure
            ( PreparedCompilerScaleAnalysis
                programCase
                compiledProgram
                inputs
                dependencies
                entryModule
            )
    ModulePreparationBenchmark -> pure (PreparedCompilerScaleModulePreparation programCase)
    TypedLoweringBenchmark ->
      case compilerScaleCaseScenario programCase of
        LoweredTemporaryValidation -> do
          let loweredProgram = loweredTemporaryValidationProgram (compilerScaleCaseSize programCase)
          evaluate (forceLoweredProgramArtifact loweredProgram)
          case validateLoweredProgram loweredProgram of
            [] -> pure (PreparedCompilerScaleLoweredValidation programCase loweredProgram)
            failures ->
              ioError (userError ("lowered validation scale fixture is invalid: " <> show failures))
        scenario -> do
          let typedProgram =
                case scenario of
                  TypedRecursiveStatementGraph ->
                    typedRecursiveStatementGraphProgram (compilerScaleCaseSize programCase)
                  TypedForwardSignedFunctions ->
                    typedForwardSignedFunctionsProgram (compilerScaleCaseSize programCase)
                  TypedWideExportProviders ->
                    typedWideExportProvidersProgram (compilerScaleCaseSize programCase)
                  _ -> typedValidationBenchmarkProgram (compilerScaleCaseSize programCase)
          evaluate (forceTypedProgramArtifact typedProgram)
          case validateTypedProgram typedProgram of
            [] -> pure (PreparedCompilerScaleTypedLowering programCase typedProgram)
            failures ->
              ioError
                ( userError
                    ( "typed-lowering scale fixture is invalid: "
                        <> show failures
                    )
                )
    WholeProgramBenchmark -> pure (PreparedCompilerScaleWholeProgram programCase)
    RuntimeBenchmark -> do
      compiledProgram <- prepareValidCompilerScaleProgram programCase
      pure (PreparedCompilerScaleRuntime programCase compiledProgram)

runPreparedBenchmark :: PreparedBenchmark -> IO ()
runPreparedBenchmark preparedBenchmark =
  case preparedBenchmark of
    PreparedParseLower _ source -> runParseLower source
    PreparedAnalysis _ _ inputs dependencies resolvedModule -> do
      compiledModule <-
        withCompilerStage TypeInferenceStage $ do
          value <- compileResolvedModule inputs dependencies resolvedModule
          evaluate (forceCompiledModule value)
          pure value
      case compiledModuleErrors compiledModule of
        [] -> pure ()
        diagnostic : _ -> failBenchmarkDiagnostic diagnostic
    PreparedModulePreparation programCase -> do
      compiledResult <-
        withCompilerStage RuntimePreparationStage (prepareProgramCase programCase)
      evaluate (forceCompiledProgramResult compiledResult)
      case compiledResult of
        Left diagnostic -> failBenchmarkDiagnostic diagnostic
        Right compiledProgram -> requireNoCompileErrors compiledProgram
    PreparedRuntime programCase compiledProgram ->
      withCompilerStage EvaluationStage $ do
        let runtimeResult = evaluateCompiledProgram compiledProgram
        evaluate (forceRuntimeProgramOutputResult runtimeResult)
        requireExpectedRuntimeResult programCase runtimeResult
    PreparedWholeProgram programCase -> do
      result <- runProgramCase programCase
      evaluate (forceProgramCaseResult result)
      requireExpectedProgramResult programCase result

runPreparedCompilerScaleBenchmark :: PreparedCompilerScaleBenchmark -> IO ()
runPreparedCompilerScaleBenchmark preparedBenchmark =
  case preparedBenchmark of
    PreparedCompilerScaleParseLower _ source -> runParseLower source
    PreparedCompilerScaleAnalysis _ _ inputs dependencies resolvedModule -> do
      compiledModule <-
        withCompilerStage TypeInferenceStage $ do
          value <- compileResolvedModule inputs dependencies resolvedModule
          evaluate (forceCompiledModule value)
          pure value
      case compiledModuleErrors compiledModule of
        [] -> pure ()
        diagnostic : _ -> failBenchmarkDiagnostic diagnostic
    PreparedCompilerScaleModulePreparation programCase -> do
      compiledResult <- buildCompilerScaleProgram programCase
      evaluate (forceCompiledProgramResult compiledResult)
      case compiledResult of
        Left diagnostic -> failBenchmarkDiagnostic diagnostic
        Right compiledProgram -> requireNoCompileErrors compiledProgram
    PreparedCompilerScaleRuntime programCase compiledProgram ->
      withCompilerStage EvaluationStage $ do
        let runtimeResult = evaluateCompiledProgram compiledProgram
        evaluate (forceRuntimeProgramOutputResult runtimeResult)
        requireExpectedCompilerScaleRuntimeResult programCase runtimeResult
    PreparedCompilerScaleLoweredValidation _ loweredProgram ->
      withCompilerStage LoweringStage $
        case validateLoweredProgram loweredProgram of
          [] -> pure ()
          failures ->
            ioError (userError ("lowered validation benchmark failed: " <> show failures))
    PreparedCompilerScaleTypedLowering programCase typedProgram ->
      case compilerScaleCaseScenario programCase of
        TypedRecursiveStatementGraph ->
          withCompilerStage TypeInferenceStage $
            case validateTypedProgram typedProgram of
              [] -> pure ()
              failures ->
                ioError (userError ("typed validation benchmark failed: " <> show failures))
        TypedWideExportProviders ->
          withCompilerStage TypeInferenceStage $
            case validateTypedProgram typedProgram of
              [] -> pure ()
              failures ->
                ioError (userError ("typed validation benchmark failed: " <> show failures))
        _ ->
          withCompilerStage LoweringStage $ do
            case validateTypedProgramOnce typedProgram of
              Left failures ->
                ioError (userError ("trusted typed program failed producer validation: " <> show failures))
              Right validatedProgram ->
                case lowerValidatedTypedCoreExpressionDirectCall validatedProgram of
                  LoweredIRSucceeded _ -> pure ()
                  loweringResult ->
                    ioError (userError ("typed-lowering benchmark failed: " <> show loweringResult))
    PreparedCompilerScaleDiagnosticAnalysis _ expression expectedDiagnosticCount ->
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
    evaluate (forceExpr expression)

analyzerDiagnosticChainExpression :: Int -> Expr
analyzerDiagnosticChainExpression expressionCount =
  foldl1 EApply
    [ EVar (sourceName (mkIdentifier ("missing" <> Text.pack (show index))))
    | index <- [0 .. expressionCount - 1]
    ]

prepareValidProgram :: ProgramCase -> IO CompiledProgram
prepareValidProgram programCase = do
  compiledResult <- prepareProgramCase programCase
  evaluate (forceCompiledProgramResult compiledResult)
  case compiledResult of
    Left diagnostic -> failBenchmarkDiagnostic diagnostic
    Right compiledProgram -> requireNoCompileErrors compiledProgram >> pure compiledProgram

buildCompilerScaleProgram :: CompilerScaleCase -> IO (Either Diagnostic CompiledProgram)
buildCompilerScaleProgram programCase =
  buildCompiledProgram
    defaultWarningSettings
    (PreludeBundled bundledPreludeSource)
    (compilerScaleCaseResolutionConfig programCase)
    (compilerScaleCaseEntryModulePath programCase)
    (pure . compilerScaleCaseSource programCase)

prepareValidCompilerScaleProgram :: CompilerScaleCase -> IO CompiledProgram
prepareValidCompilerScaleProgram programCase = do
  compiledResult <- buildCompilerScaleProgram programCase
  evaluate (forceCompiledProgramResult compiledResult)
  case compiledResult of
    Left diagnostic -> failBenchmarkDiagnostic diagnostic
    Right compiledProgram -> requireNoCompileErrors compiledProgram >> pure compiledProgram

resolveBenchmarkProgram :: ModuleResolutionConfig -> [Text] -> (FilePath -> IO (Maybe Text)) -> IO ResolvedProgram
resolveBenchmarkProgram resolutionConfig entryModulePath sourceLookup =
  case preparePrelude (PreludeBundled bundledPreludeSource) of
    Left diagnostic -> failBenchmarkDiagnostic diagnostic
    Right preparedPrelude -> do
      resolvedResult <-
        resolveProgramWithAmbientExports
          resolutionConfig
          (preparedPreludeBuiltinMode preparedPrelude)
          (preparedPreludeVisibleExports preparedPrelude)
          sourceLookup
          entryModulePath
      case resolvedResult of
        Left diagnostic -> failBenchmarkDiagnostic diagnostic
        Right resolvedProgram -> pure resolvedProgram

requireResolvedEntryModule :: [Text] -> ResolvedProgram -> IO ResolvedModule
requireResolvedEntryModule entryModulePath resolvedProgram =
  case filter ((== entryModulePath) . resolvedModulePath) (resolvedProgramModules resolvedProgram) of
    value : _ -> pure value
    [] ->
      ioError
        ( userError
            ( "resolved benchmark program is missing its entry module: "
                <> Text.unpack (Text.intercalate "::" entryModulePath)
            )
        )

runCompilerScaleCase :: CompilerScaleCase -> IO Text
runCompilerScaleCase programCase = do
  compiledProgram <- prepareValidCompilerScaleProgram programCase
  withCompilerStage EvaluationStage $ do
    let runtimeResult = evaluateCompiledProgram compiledProgram
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
        [TypedExpressionStatement (TypedSpan 1 1) expression]
        intInfo
    ]
    modulePath
  where
    modulePath = ["TypedValidation"]
    intInfo = TypedNodeInfo TypedIntType (TypedSignedIntegerRecipe 64) [] []
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
            (bindings <> [TypedExpressionStatement spanValue terminalExpression])
            boolInfo
        ]
        modulePath
  where
    graphGroupWidth = 8
    groupCount = statementCount `div` graphGroupWidth
    bindings = concatMap graphGroup [0 .. groupCount - 1]
    modulePath = ["TypedRecursiveStatementGraph"]
    spanValue = TypedSpan 1 1
    boolInfo = TypedNodeInfo TypedBoolType TypedBoolRecipe [] []
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
    boundVariable owner name = TypedVariableExpr boolInfo name (Just owner)
    binding owner name expression =
      TypedLetStatement
        owner
        name
        spanValue
        (TypedScheme owner [] [] [] TypedBoolType TypedBoolRecipe Nothing)
        expression
    graphGroup groupIndex =
      [ binding firstHistoryOwner historyName firstHistoryExpression,
        binding chainOneOwner chainOneName (boundVariable firstHistoryOwner historyName),
        binding secondHistoryOwner historyName (boundVariable chainOneOwner chainOneName),
        binding chainTwoOwner chainTwoName (boundVariable secondHistoryOwner historyName),
        binding mutualLeftOwner mutualLeftName (boundVariable mutualRightOwner mutualRightName),
        binding chainThreeOwner chainThreeName (boundVariable chainTwoOwner chainTwoName),
        binding mutualRightOwner mutualRightName (boundVariable mutualLeftOwner mutualLeftName),
        binding
          tailOwner
          tailName
          ( TypedIfExpr
              boolInfo
              (boundVariable chainThreeOwner chainThreeName)
              (boundVariable mutualRightOwner mutualRightName)
              (boundVariable chainThreeOwner chainThreeName)
          )
      ]
      where
        baseIndex = groupIndex * graphGroupWidth
        chainOneName = graphName "chainOne" groupIndex
        chainTwoName = graphName "chainTwo" groupIndex
        chainThreeName = graphName "chainThree" groupIndex
        mutualLeftName = graphName "mutualLeft" groupIndex
        mutualRightName = graphName "mutualRight" groupIndex
        tailName = graphName "tail" groupIndex
        firstHistoryOwner = graphOwner baseIndex historyName
        chainOneOwner = graphOwner (baseIndex + 1) chainOneName
        secondHistoryOwner = graphOwner (baseIndex + 2) historyName
        chainTwoOwner = graphOwner (baseIndex + 3) chainTwoName
        mutualLeftOwner = graphOwner (baseIndex + 4) mutualLeftName
        chainThreeOwner = graphOwner (baseIndex + 5) chainThreeName
        mutualRightOwner = graphOwner (baseIndex + 6) mutualRightName
        tailOwner = graphOwner (baseIndex + 7) tailName
        firstHistoryExpression
          | groupIndex == 0 = trueExpression
          | otherwise =
              let previousTailName = graphName "tail" (groupIndex - 1)
                  previousTailOwner = graphOwner (baseIndex - 1) previousTailName
               in boundVariable previousTailOwner previousTailName
    terminalName = graphName "tail" (groupCount - 1)
    terminalOwner = graphOwner (statementCount - 1) terminalName
    terminalExpression = boundVariable terminalOwner terminalName

-- Typed Core deliberately keeps malformed states constructible and therefore
-- has no blanket NFData instance. Derived Show still traverses every artifact
-- field, so forcing its result keeps all generation outside the timed region.
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
            (bindings <> [TypedExpressionStatement spanValue trueExpression])
            boolInfo
        ]
        modulePath
  where
    modulePath = ["TypedWideExportProviders"]
    spanValue = TypedSpan 1 1
    boolInfo = TypedNodeInfo TypedBoolType TypedBoolRecipe [] []
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
        TypedBoolType
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
            (concatMap functionPair [0 .. functionCount - 1] <> [terminalStatement])
            boolInfo
        ]
        modulePath
  where
    modulePath = ["TypedForwardSignedFunctions"]
    source = TypedSourcePath "compiler-scale/TypedForwardSignedFunctions.jz"
    statementSpan = TypedSpan 1 1
    boolInfo = TypedNodeInfo TypedBoolType TypedBoolRecipe [] []
    functionType = TypedFunctionType TypedBoolType TypedBoolType
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

forceTypedProgramArtifact :: TypedProgram -> ()
forceTypedProgramArtifact typedProgram = rnf (show typedProgram)

forceLoweredProgramArtifact :: LoweredProgram -> ()
forceLoweredProgramArtifact loweredProgram = rnf (show loweredProgram)

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

requireNoCompileErrors :: CompiledProgram -> IO ()
requireNoCompileErrors compiledProgram =
  case compiledProgramErrors compiledProgram of
    [] -> pure ()
    diagnostic : _ -> failBenchmarkDiagnostic diagnostic

requireExpectedRuntimeResult :: ProgramCase -> Either Diagnostic RuntimeProgram -> IO ()
requireExpectedRuntimeResult programCase runtimeResult =
  let actualTermination =
        case runtimeResult of
          Left _ -> RuntimeFailedProgram
          Right _ -> SuccessfulProgram
      actualStdout =
        case runtimeResult of
          Left _ -> ""
          Right runtimeProgram -> maybe "" ((<> "\n") . renderRuntimeValue) (runtimeProgramOutput runtimeProgram)
   in requireExpectedBehavior programCase actualTermination actualStdout

requireExpectedCompilerScaleRuntimeResult :: CompilerScaleCase -> Either Diagnostic RuntimeProgram -> IO ()
requireExpectedCompilerScaleRuntimeResult programCase runtimeResult =
  case runtimeResult of
    Left diagnostic -> failBenchmarkDiagnostic diagnostic
    Right runtimeProgram ->
      let actualOutput = maybe "" renderRuntimeValue (runtimeProgramOutput runtimeProgram)
       in if actualOutput == compilerScaleCaseExpectedOutput programCase
            then pure ()
            else
              ioError
                ( userError
                    ( "compiler scale runtime benchmark did not preserve expected output: "
                        <> Text.unpack (compilerScaleCaseIdentifier programCase)
                    )
                )

requireExpectedProgramResult :: ProgramCase -> ProgramCaseResult -> IO ()
requireExpectedProgramResult programCase result =
  requireExpectedBehavior
    programCase
    (programCaseResultTermination result)
    (programCaseResultStdout result)

requireExpectedBehavior :: ProgramCase -> ProgramTermination -> Text -> IO ()
requireExpectedBehavior programCase actualTermination actualStdout
  | actualTermination == programCaseExpectedTermination programCase,
    actualStdout == programCaseExpectedStdout programCase =
      pure ()
  | otherwise =
      ioError
        ( userError
            ( "benchmark case did not preserve expected behavior: "
                <> Text.unpack (programCaseIdentifier programCase)
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
