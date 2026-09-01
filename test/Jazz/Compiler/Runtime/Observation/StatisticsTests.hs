{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Jazz.Compiler.Runtime.Observation.StatisticsTests
  ( tests,
  )
where

import Control.Monad.Trans.State.Strict (get)
import qualified Data.ByteString.Lazy.Char8 as LazyByteString
import Data.Functor.Identity (runIdentity)
import Data.List (isInfixOf)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Jazz.Compiler.AST
  ( CorePhase (Resolved),
    Expr,
    Literal (..),
  )
import Jazz.Compiler.BuiltinCatalog
  ( BuiltinResolutionMode (ResolveKernelOnly),
    BuiltinSymbol (BuiltinArguments, BuiltinMap, BuiltinTextLength, BuiltinTextUnconsRaw),
    builtinSymbolKernelName,
  )
import Jazz.Compiler.Diagnostics
  ( Diagnostic,
    SourceSpan (..),
  )
import Jazz.Compiler.Driver
  ( ResolvedPrelude (PreludeAbsent),
    RunResult,
    runCompileErrors,
    runDiagnostics,
    runExitStatus,
    runModuleGraphObserved,
    runModuleGraphWithResolvedPreludeAndHostObserved,
    runOutput,
    runRuntimeErrors,
    runRuntimeObservation,
    runRuntimeValue,
    runSource,
    runSourceObserved,
  )
import Jazz.Compiler.ModuleResolver (ModuleResolutionConfig (..))
import Jazz.Compiler.Name
  ( Name (BuiltinName),
    mkIdentifier,
    operatorBindingName,
  )
import Jazz.Compiler.Runtime
  ( ModuleEvaluationMode (EvaluateEntryModule),
    RuntimeValue (..),
    ScopeResult (..),
    evaluateModuleScopeWithRequiredEvaluationHost,
    evaluateRuntimeExprObserved,
    renderRuntimeValue,
    runRuntimeHostEvaluation,
  )
import Jazz.Compiler.Runtime.Observation
  ( RuntimeCallableIdentity (..),
    RuntimeObservationReport (..),
    RuntimeObservationRequest (..),
    RuntimeObservationResult (..),
    RuntimeOutcome (..),
    RuntimeProfileEvent (..),
    RuntimeProfileFrame (..),
    RuntimeSemanticProfile (..),
    RuntimeStatistics (..),
    RuntimeTermination (..),
    emptyRuntimeStatistics,
  )
import Jazz.Compiler.Runtime.Observation.Profile (encodeRuntimeSemanticProfile)
import Jazz.Compiler.Runtime.Observation.Render
  ( decodeRuntimeObservationJson,
    encodeRuntimeObservationJson,
    renderRuntimeObservationHuman,
  )
import Jazz.Compiler.Runtime.Types
  ( RuntimeHostEvaluationState
      ( runtimeHostEvaluationActiveMachineCount,
        runtimeHostEvaluationContinuationDepth
      ),
  )
import Jazz.Compiler.RuntimeHost
  ( RuntimeHost (runtimeHostArguments),
    disabledRuntimeHost,
  )
import Jazz.Compiler.Semantics.Runtime.Fixtures
  ( caseArm,
    dataConstructor,
    expressionApply,
    expressionBinary,
    expressionBlock,
    expressionConstructor,
    expressionLambda,
    expressionList,
    expressionLiteral,
    expressionPatternCase,
    expressionSectionRight,
    expressionTuple,
    expressionVariable,
    patternLiteral,
    patternTuple,
    patternVariable,
    statementData,
    statementExpression,
    statementLet,
  )
import Jazz.Compiler.TypeRepresentation (SignatureType (..))
import Jazz.Compiler.WarningConfig (defaultWarningSettings)
import Jazz.TestHarness
  ( NamedTest,
    assertEqual,
    failTest,
  )
import System.Directory (doesFileExist)
import System.FilePath ((</>))

tests :: [NamedTest]
tests =
  [ ("disabled observation preserves ordinary driver behavior", testDisabledBehavior),
    ("observed driver transports a report", testDriverTransport),
    ("observed module runtime shares one report", testModuleRuntimeTransport),
    ("literal evaluation has an exact minimal transition count", testLiteralTransitions),
    ("nested applications preserve exact transition depth and profile accounting", testNestedApplicationAccounting),
    ("disabled observation skips continuation-depth state traffic", testDisabledObservationSkipsContinuationDepthState),
    ("closure application records forcing and continuation depth", testClosureApplication),
    ("nested evaluator machines preserve outer continuation depth", testNestedContinuationDepth),
    ("builtin application is classified independently", testBuiltinApplication),
    ("infix operator evaluation is classified independently", testOperatorApplication),
    ("constructor application is classified independently", testConstructorApplication),
    ("closures capture only the bindings their bodies reference", testClosureCaptureWidths),
    ("declared right sections capture only their generated operands", testDeclaredRightSectionCaptureWidth),
    ("source values record each logical construction category", testSourceConstructions),
    ("builtin results record their logical constructions", testBuiltinConstructions),
    ("case evaluation records attempts, matches, and introduced bindings", testPatternStatistics),
    ("builtin calls and host operations remain distinct", testBuiltinAndHostStatistics),
    ("deferred binding caches distinguish misses and hits", testDeferredCacheHitAndMiss),
    ("recursive deferred evaluation records its own cache outcome", testDeferredCacheRecursion),
    ("human statistics use stable meaningful labels", testHumanRenderer),
    ("JSON statistics are explicit, compact, and round trip", testJsonRenderer),
    ("runtime failure retains a partial report", testRuntimeFailureReport),
    ("compile failure has no runtime report", testCompileFailureHasNoReport)
  ]

testDisabledBehavior :: IO ()
testDisabledBehavior = do
  source <- readFixture "literal-success.jz"
  ordinary <- runSource defaultWarningSettings source
  observed <- runSourceObserved RuntimeObservationDisabled defaultWarningSettings source
  assertEqual "disabled result" (observableRunResult ordinary) (observableRunResult observed)
  assertEqual "disabled report" Nothing (runRuntimeObservation observed)

testDriverTransport :: IO ()
testDriverTransport = do
  source <- readFixture "literal-success.jz"
  result <- runSourceObserved RuntimeObservationStatistics defaultWarningSettings source
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "successful diagnostic stream" [] (runDiagnostics result)
  assertEqual "output" (Just "42") (runOutput result)
  report <- requireReport result
  assertEqual "successful termination" RuntimeSucceeded (runtimeObservationTermination report)
  assertPositive "driver transitions" (runtimeEvaluatorTransitions (runtimeObservationStatistics report))

testModuleRuntimeTransport :: IO ()
testModuleRuntimeTransport = do
  let fixtureRoot = "test/fixtures/runtime-observation/module-success"
      resolutionConfig =
        ModuleResolutionConfig
          { moduleRoots = [fixtureRoot </> "src"],
            moduleExtension = ".jz"
          }
      lookupSource path = do
        exists <- doesFileExist path
        if exists then Just <$> TextIO.readFile path else pure Nothing
  result <-
    runModuleGraphObserved
      RuntimeObservationStatistics
      defaultWarningSettings
      resolutionConfig
      ["App", "Main"]
      lookupSource
  assertEqual "module compile errors" [] (runCompileErrors result)
  assertEqual "module runtime errors" [] (runRuntimeErrors result)
  assertEqual "module output" (Just "42") (runOutput result)
  report <- requireReport result
  assertEqual "module termination" RuntimeSucceeded (runtimeObservationTermination report)
  assertPositive "module transitions" (runtimeEvaluatorTransitions (runtimeObservationStatistics report))

testLiteralTransitions :: IO ()
testLiteralTransitions = do
  let observed = evaluateRuntimeExprObserved RuntimeObservationStatistics (expressionLiteral (LInt 1))
  assertEqual
    "literal result"
    (RuntimeOutcomeCompleted (Just "1"))
    (fmap (fmap renderRuntimeValue) (runtimeObservationOutcome observed))
  report <- requireObservedReport observed
  let statistics = runtimeObservationStatistics report
  assertEqual "literal transitions" 2 (runtimeEvaluatorTransitions statistics)
  assertEqual "literal forced values" 0 (runtimeForcedValues statistics)
  assertEqual "literal applications" 0 (runtimeApplications statistics)
  assertEqual "literal continuation depth" 0 (runtimeMaximumContinuationDepth statistics)

testNestedApplicationAccounting :: IO ()
testNestedApplicationAccounting = do
  let expression = nestedIdentityApplication 64
      observed = evaluateRuntimeExprObserved RuntimeObservationStatisticsAndProfile expression
  assertEqual
    "nested application result"
    (RuntimeOutcomeCompleted (Just "7"))
    (fmap (fmap renderRuntimeValue) (runtimeObservationOutcome observed))
  report <- requireObservedReport observed
  let statistics = runtimeObservationStatistics report
  assertEqual "nested application transitions" 450 (runtimeEvaluatorTransitions statistics)
  assertEqual "nested application count" 64 (runtimeApplications statistics)
  assertEqual "nested closure application count" 64 (runtimeClosureApplications statistics)
  assertEqual "nested final continuation depth" 0 (runtimeCurrentContinuationDepth statistics)
  assertEqual "nested maximum continuation depth" 64 (runtimeMaximumContinuationDepth statistics)
  profile <- requireObservedProfile report
  assertEqual
    "nested exact semantic profile"
    expectedNestedApplicationProfile
    profile
  profileOnlyReport <-
    requireObservedSuccess
      (evaluateRuntimeExprObserved RuntimeObservationProfile expression)
  profileOnly <- requireObservedProfile profileOnlyReport
  assertEqual
    "nested profile bytes are independent of statistics collection"
    (encodeRuntimeSemanticProfile profile)
    (encodeRuntimeSemanticProfile profileOnly)

testDisabledObservationSkipsContinuationDepthState :: IO ()
testDisabledObservationSkipsContinuationDepthState = do
  let inspectingHost =
        disabledRuntimeHost
          { runtimeHostArguments = do
              evaluationState <- get
              pure
                [ Text.pack
                    (show (runtimeHostEvaluationActiveMachineCount evaluationState)),
                  Text.pack
                    (show (runtimeHostEvaluationContinuationDepth evaluationState))
                ]
          }
      expression =
        expressionApply
          (expressionLambda "value" (expressionVariable "value"))
          (expressionApply (kernelBuiltin BuiltinArguments) (expressionTuple []))
      result =
        runIdentity
          ( runRuntimeHostEvaluation disabledRuntimeHost $ \_ ->
              evaluateModuleScopeWithRequiredEvaluationHost
                inspectingHost
                Nothing
                EvaluateEntryModule
                ResolveKernelOnly
                Map.empty
                Map.empty
                [statementExpression (SourceSpan 1 1) expression]
          )
  case result of
    Right ScopeResult {scopeResultValue = Just (VList [VText observedMachineCount, VText observedDepth] _)} -> do
      assertEqual "disabled active-machine state" "0" observedMachineCount
      assertEqual "disabled continuation-depth state" "0" observedDepth
    _ -> failTest "expected observed active-machine and continuation-depth values"

testClosureApplication :: IO ()
testClosureApplication = do
  let expression =
        expressionApply
          (expressionLambda "value" (expressionVariable "value"))
          (expressionLiteral (LInt 7))
      observed = evaluateRuntimeExprObserved RuntimeObservationStatistics expression
  report <- requireObservedSuccess observed
  let statistics = runtimeObservationStatistics report
  assertEqual "closure applications" 1 (runtimeClosureApplications statistics)
  assertEqual "builtin applications" 0 (runtimeBuiltinApplications statistics)
  assertPositive "forced values" (runtimeForcedValues statistics)
  assertPositive "maximum continuation depth" (runtimeMaximumContinuationDepth statistics)
  assertEqual "final continuation depth" 0 (runtimeCurrentContinuationDepth statistics)

testNestedContinuationDepth :: IO ()
testNestedContinuationDepth = do
  let value = expressionLiteral (LInt 7)
      callback =
        expressionLambda
          "value"
          (expressionList [expressionList [expressionList [expressionVariable "value"]]])
  directStatistics <- statisticsFor (expressionApply callback value)
  nestedStatistics <-
    statisticsFor
      ( expressionApply
          (expressionApply (kernelBuiltin BuiltinMap) callback)
          (expressionList [value, value])
      )
  assertEqual
    "higher-order callback adds its implicit outer continuation"
    (runtimeMaximumContinuationDepth directStatistics + 1)
    (runtimeMaximumContinuationDepth nestedStatistics)
  assertEqual
    "nested evaluation restores final continuation depth"
    0
    (runtimeCurrentContinuationDepth nestedStatistics)

testBuiltinApplication :: IO ()
testBuiltinApplication = do
  let expression =
        expressionApply
          (expressionVariable (BuiltinName (mkIdentifier (builtinSymbolKernelName BuiltinTextLength))))
          (expressionLiteral (LText "Jazz"))
      observed = evaluateRuntimeExprObserved RuntimeObservationStatistics expression
  report <- requireObservedSuccess observed
  let statistics = runtimeObservationStatistics report
  assertEqual "builtin applications" 1 (runtimeBuiltinApplications statistics)
  assertEqual "closure applications" 0 (runtimeClosureApplications statistics)
  assertEqual "total applications" 1 (runtimeApplications statistics)

testOperatorApplication :: IO ()
testOperatorApplication = do
  statistics <- statisticsFor (expressionBinary "+" (expressionLiteral (LInt 1)) (expressionLiteral (LInt 2)))
  assertEqual "operator applications" 1 (runtimeOperatorApplications statistics)
  assertEqual "operator total applications" 1 (runtimeApplications statistics)
  assertEqual "operator builtin calls" 0 (runtimeBuiltinCalls statistics)

testConstructorApplication :: IO ()
testConstructorApplication = do
  let expression =
        expressionBlock
          [ statementData
              (SourceSpan 1 1)
              "Box"
              []
              [dataConstructor "Box" [TypeInt]],
            statementExpression
              (SourceSpan 2 1)
              (expressionApply (expressionConstructor "Box") (expressionLiteral (LInt 1)))
          ]
      observed = evaluateRuntimeExprObserved RuntimeObservationStatistics expression
  report <- requireObservedSuccess observed
  let statistics = runtimeObservationStatistics report
  assertEqual "constructor applications" 1 (runtimeConstructorApplications statistics)
  assertEqual "total applications" 1 (runtimeApplications statistics)

testClosureCaptureWidths :: IO ()
testClosureCaptureWidths = do
  zero <- statisticsFor (expressionLambda "value" (expressionVariable "value"))
  one <-
    statisticsFor
      ( expressionBlock
          [ statementLet "first" (SourceSpan 1 1) (expressionLiteral (LInt 1)),
            statementExpression (SourceSpan 2 1) (expressionLambda "value" (expressionVariable "first"))
          ]
      )
  multiple <-
    statisticsFor
      ( expressionBlock
          [ statementLet "first" (SourceSpan 1 1) (expressionLiteral (LInt 1)),
            statementLet "second" (SourceSpan 2 1) (expressionLiteral (LInt 2)),
            statementExpression (SourceSpan 3 1) (expressionLambda "value" (expressionTuple [expressionVariable "first", expressionVariable "second"]))
          ]
      )
  oneAmongUnused <-
    statisticsFor
      ( expressionBlock
          [ statementLet "unusedBefore" (SourceSpan 1 1) (expressionLiteral (LInt 0)),
            statementLet "captured" (SourceSpan 2 1) (expressionLiteral (LInt 1)),
            statementLet "unusedAfter" (SourceSpan 3 1) (expressionLiteral (LInt 2)),
            statementExpression (SourceSpan 4 1) (expressionLambda "value" (expressionVariable "captured"))
          ]
      )
  assertEqual "zero-capture closures" 1 (runtimeClosuresCreated zero)
  assertEqual "zero captured bindings" 0 (runtimeBindingsCaptured zero)
  assertEqual "zero maximum capture width" 0 (runtimeMaximumCaptureWidth zero)
  assertEqual "one-capture closures" 1 (runtimeClosuresCreated one)
  assertEqual "one captured binding" 1 (runtimeBindingsCaptured one)
  assertEqual "one maximum capture width" 1 (runtimeMaximumCaptureWidth one)
  assertEqual "multiple-capture closures" 1 (runtimeClosuresCreated multiple)
  assertEqual "multiple captured bindings" 2 (runtimeBindingsCaptured multiple)
  assertEqual "multiple maximum capture width" 2 (runtimeMaximumCaptureWidth multiple)
  assertEqual "one-among-unused closures" 1 (runtimeClosuresCreated oneAmongUnused)
  assertEqual "one binding captured among unused bindings" 1 (runtimeBindingsCaptured oneAmongUnused)
  assertEqual "one-among-unused maximum capture width" 1 (runtimeMaximumCaptureWidth oneAmongUnused)

testDeclaredRightSectionCaptureWidth :: IO ()
testDeclaredRightSectionCaptureWidth = do
  statistics <-
    statisticsFor
      ( expressionBlock
          [ statementLet "unused" (SourceSpan 1 1) (expressionLiteral (LInt 0)),
            statementLet
              (operatorBindingName "%%")
              (SourceSpan 2 1)
              (expressionLambda "left" (expressionLambda "right" (expressionVariable "left"))),
            statementExpression (SourceSpan 3 1) (expressionSectionRight "%%" (expressionLiteral (LInt 2)))
          ]
      )
  assertEqual "right-section maximum capture width" 2 (runtimeMaximumCaptureWidth statistics)

testSourceConstructions :: IO ()
testSourceConstructions = do
  statistics <-
    statisticsFor
      ( expressionBlock
          [ statementData
              (SourceSpan 1 1)
              "Box"
              []
              [dataConstructor "Box" [TypeInt]],
            statementExpression
              (SourceSpan 2 1)
              ( expressionTuple
                  [ expressionList [expressionLiteral (LInt 1), expressionLiteral (LInt 2)],
                    expressionApply (expressionConstructor "Box") (expressionLiteral (LInt 3))
                  ]
              )
          ]
      )
  assertEqual "list cells" 2 (runtimeListCellsConstructed statistics)
  assertEqual "tuples" 1 (runtimeTuplesConstructed statistics)
  assertEqual "saturated ADT values" 1 (runtimeSaturatedAdtValuesConstructed statistics)

testBuiltinConstructions :: IO ()
testBuiltinConstructions = do
  statistics <-
    statisticsFor
      ( expressionApply
          (kernelBuiltin BuiltinTextUnconsRaw)
          (expressionLiteral (LText "Jazz"))
      )
  assertEqual "builtin list cells" 1 (runtimeListCellsConstructed statistics)
  assertEqual "builtin tuples" 1 (runtimeTuplesConstructed statistics)

testPatternStatistics :: IO ()
testPatternStatistics = do
  statistics <-
    statisticsFor
      ( expressionPatternCase
          (expressionTuple [expressionLiteral (LInt 1), expressionLiteral (LInt 2)])
          [ caseArm (patternLiteral (LInt 0)) Nothing (expressionLiteral (LInt 0)),
            caseArm (patternTuple [patternVariable "left", patternVariable "right"]) Nothing (expressionVariable "left")
          ]
      )
  assertEqual "pattern attempts" 2 (runtimePatternAttempts statistics)
  assertEqual "pattern matches" 1 (runtimePatternMatches statistics)
  assertEqual "pattern bindings" 2 (runtimePatternBindings statistics)

testBuiltinAndHostStatistics :: IO ()
testBuiltinAndHostStatistics = do
  pureBuiltin <-
    statisticsFor
      ( expressionApply
          (kernelBuiltin BuiltinTextLength)
          (expressionLiteral (LText "Jazz"))
      )
  hostBuiltin <-
    statisticsFor
      ( expressionApply
          (kernelBuiltin BuiltinArguments)
          (expressionTuple [])
      )
  assertEqual "pure builtin calls" 1 (runtimeBuiltinCalls pureBuiltin)
  assertEqual "pure builtin host operations" 0 (runtimeHostOperations pureBuiltin)
  assertEqual "host builtin calls" 1 (runtimeBuiltinCalls hostBuiltin)
  assertEqual "host builtin host operations" 1 (runtimeHostOperations hostBuiltin)

testDeferredCacheHitAndMiss :: IO ()
testDeferredCacheHitAndMiss = do
  let fixtureRoot = "test/fixtures/runtime-observation/module-cache"
      resolutionConfig =
        ModuleResolutionConfig
          { moduleRoots = [fixtureRoot </> "src"],
            moduleExtension = ".jz"
          }
      lookupSource path = do
        exists <- doesFileExist path
        if exists then Just <$> TextIO.readFile path else pure Nothing
  result <-
    runModuleGraphWithResolvedPreludeAndHostObserved
      RuntimeObservationStatistics
      disabledRuntimeHost
      defaultWarningSettings
      PreludeAbsent
      resolutionConfig
      ["App", "Main"]
      lookupSource
  report <- requireReport result
  let statistics = runtimeObservationStatistics report
  assertEqual "cache misses" 1 (runtimeDeferredCacheMisses statistics)
  assertEqual "cache hits" 1 (runtimeDeferredCacheHits statistics)
  assertEqual "recursive cache evaluations" 0 (runtimeDeferredCacheRecursiveEvaluations statistics)

testDeferredCacheRecursion :: IO ()
testDeferredCacheRecursion = do
  let observed =
        evaluateRuntimeExprObserved
          RuntimeObservationStatistics
          ( expressionBlock
              [ statementLet "loop" (SourceSpan 1 1) (expressionVariable "loop"),
                statementExpression (SourceSpan 2 1) (expressionVariable "loop")
              ]
          )
  case runtimeObservationOutcome observed of
    RuntimeOutcomeFailed _ -> pure ()
    outcome -> failTest ("expected recursive evaluation failure, got " <> Text.pack (show outcome))
  report <- requireObservedReport observed
  let statistics = runtimeObservationStatistics report
  assertEqual "recursive cache misses" 1 (runtimeDeferredCacheMisses statistics)
  assertEqual "recursive cache evaluations" 1 (runtimeDeferredCacheRecursiveEvaluations statistics)

testHumanRenderer :: IO ()
testHumanRenderer = do
  let rendered = renderRuntimeObservationHuman zeroReport
  assertTextContains "human termination" "termination: succeeded" rendered
  assertTextContains "human transition label" "evaluator transitions: 0" rendered
  assertTextContains "human cache label" "deferred cache recursive evaluations: 0" rendered

testJsonRenderer :: IO ()
testJsonRenderer = do
  let encoded = encodeRuntimeObservationJson zeroReport
  assertEqual "JSON round trip" (Right zeroReport) (decodeRuntimeObservationJson encoded)
  assertLazyBytesContain "JSON schema version" "\"schemaVersion\":1" encoded
  assertLazyBytesContain "JSON explicit zero" "\"closuresCreated\":0" encoded
  assertEqual "compact JSON" False (LazyByteString.elem '\n' encoded)

testRuntimeFailureReport :: IO ()
testRuntimeFailureReport = do
  source <- readFixture "runtime-failure.jz"
  result <- runSourceObserved RuntimeObservationStatistics defaultWarningSettings source
  assertEqual "compile errors" [] (runCompileErrors result)
  case runRuntimeErrors result of
    [] -> failTest "expected a runtime diagnostic"
    _ -> pure ()
  assertEqual "runtime failure stream" (runRuntimeErrors result) (runDiagnostics result)
  report <- requireReport result
  assertEqual "failed termination" RuntimeFailed (runtimeObservationTermination report)
  assertPositive "partial transitions" (runtimeEvaluatorTransitions (runtimeObservationStatistics report))

testCompileFailureHasNoReport :: IO ()
testCompileFailureHasNoReport = do
  source <- readFixture "compile-failure.jz"
  result <- runSourceObserved RuntimeObservationStatistics defaultWarningSettings source
  case runCompileErrors result of
    [] -> failTest "expected a compile diagnostic"
    _ -> pure ()
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "compile failure stream" (runCompileErrors result) (runDiagnostics result)
  assertEqual "runtime report" Nothing (runRuntimeObservation result)

observableRunResult :: RunResult -> ([Diagnostic], Maybe Text, Maybe Text, Maybe Integer, Maybe RuntimeObservationReport)
observableRunResult result =
  ( runDiagnostics result,
    runOutput result,
    renderRuntimeValue <$> runRuntimeValue result,
    runExitStatus result,
    runRuntimeObservation result
  )

requireReport :: RunResult -> IO RuntimeObservationReport
requireReport result =
  case runRuntimeObservation result of
    Nothing -> failTest "expected a runtime observation report"
    Just report -> pure report

requireObservedReport :: RuntimeObservationResult value -> IO RuntimeObservationReport
requireObservedReport observed =
  case runtimeObservationReport observed of
    Nothing -> failTest "expected a direct runtime observation report"
    Just report -> pure report

requireObservedSuccess :: RuntimeObservationResult value -> IO RuntimeObservationReport
requireObservedSuccess observed = do
  case runtimeObservationOutcome observed of
    RuntimeOutcomeCompleted _ -> pure ()
    RuntimeOutcomeFailed diagnostic ->
      failTest ("expected runtime success, got diagnostic " <> Text.pack (show diagnostic))
    RuntimeOutcomeExited status ->
      failTest ("expected runtime success, got exit status " <> Text.pack (show status))
  requireObservedReport observed

requireObservedProfile :: RuntimeObservationReport -> IO RuntimeSemanticProfile
requireObservedProfile report =
  case runtimeObservationProfile report of
    Nothing -> failTest "expected a semantic runtime profile"
    Just profile -> pure profile

assertPositive :: (Ord number, Num number, Show number) => Text -> number -> IO ()
assertPositive label value =
  if value > 0
    then pure ()
    else failTest (label <> ": expected a positive value, got " <> Text.pack (show value))

statisticsFor :: Expr 'Resolved -> IO RuntimeStatistics
statisticsFor expression = do
  report <- requireObservedSuccess (evaluateRuntimeExprObserved RuntimeObservationStatistics expression)
  pure (runtimeObservationStatistics report)

kernelBuiltin :: BuiltinSymbol -> Expr 'Resolved
kernelBuiltin = expressionVariable . BuiltinName . mkIdentifier . builtinSymbolKernelName

nestedIdentityApplication :: Int -> Expr 'Resolved
nestedIdentityApplication depth =
  foldr
    (\_ argument -> expressionApply identity argument)
    (expressionLiteral (LInt 7))
    [1 .. depth]
  where
    identity = expressionLambda "value" (expressionVariable "value")

expectedNestedApplicationProfile :: RuntimeSemanticProfile
expectedNestedApplicationProfile =
  RuntimeSemanticProfile
    { runtimeSemanticProfileTermination = RuntimeSucceeded,
      runtimeSemanticProfileIncomplete = False,
      runtimeSemanticProfileEndValue = 450,
      runtimeSemanticProfileFrames =
        [ RuntimeProfileFrame RootCallable,
          RuntimeProfileFrame (ClosureCallable "<entry>" 1 "value")
        ],
      runtimeSemanticProfileEvents =
        RuntimeProfileOpen 0 0
          : concatMap
            (\openTime -> [RuntimeProfileOpen 1 openTime, RuntimeProfileClose 1 (openTime + 3)])
            [195, 199 .. 447]
            <> [RuntimeProfileClose 0 450]
    }

zeroReport :: RuntimeObservationReport
zeroReport =
  RuntimeObservationReport
    { runtimeObservationTermination = RuntimeSucceeded,
      runtimeObservationStatistics = emptyRuntimeStatistics,
      runtimeObservationProfile = Nothing
    }

assertTextContains :: Text -> Text -> Text -> IO ()
assertTextContains label expected actual =
  if expected `Text.isInfixOf` actual
    then pure ()
    else failTest (label <> ": expected " <> Text.pack (show expected) <> " in " <> Text.pack (show actual))

assertLazyBytesContain :: Text -> LazyByteString.ByteString -> LazyByteString.ByteString -> IO ()
assertLazyBytesContain label expected actual =
  if LazyByteString.unpack expected `isInfixOf` LazyByteString.unpack actual
    then pure ()
    else failTest (label <> ": expected " <> Text.pack (show expected) <> " in " <> Text.pack (show actual))

readFixture :: FilePath -> IO Text
readFixture name = TextIO.readFile ("test/fixtures/runtime-observation/" <> name)
