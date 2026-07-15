{-# LANGUAGE OverloadedStrings #-}

module JazzNext.Compiler.Runtime.Observation.StatisticsTests
  ( tests,
  )
where

import qualified Data.ByteString.Lazy.Char8 as LazyByteString
import Data.List (isInfixOf)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import JazzNext.Compiler.AST
  ( CaseArm (..),
    DataConstructor (..),
    DataConstructorArgument (..),
    Expr (..),
    Literal (..),
    Pattern (..),
    Statement (..),
  )
import JazzNext.Compiler.BuiltinCatalog
  ( BuiltinSymbol (BuiltinArguments, BuiltinMap, BuiltinTextLength, BuiltinTextUnconsRaw),
    builtinSymbolKernelName,
  )
import JazzNext.Compiler.Diagnostics (SourceSpan (..))
import JazzNext.Compiler.Driver
  ( ResolvedPrelude (PreludeAbsent),
    RunResult (..),
    runModuleGraphObserved,
    runModuleGraphWithResolvedPreludeAndHostObserved,
    runSource,
    runSourceObserved,
  )
import JazzNext.Compiler.ModuleResolver (ModuleResolutionConfig (..))
import JazzNext.Compiler.Name (Name (BuiltinName), mkIdentifier)
import JazzNext.Compiler.Runtime
  ( RuntimeValue (..),
    evaluateRuntimeExprObserved,
    untypedIntMetadata,
  )
import JazzNext.Compiler.Runtime.Observation
  ( RuntimeObservationReport (..),
    RuntimeObservationRequest (..),
    RuntimeObservationResult (..),
    RuntimeOutcome (..),
    RuntimeStatistics (..),
    RuntimeTermination (..),
    emptyRuntimeStatistics,
  )
import JazzNext.Compiler.Runtime.Observation.Render
  ( decodeRuntimeObservationJson,
    encodeRuntimeObservationJson,
    renderRuntimeObservationHuman,
  )
import JazzNext.Compiler.RuntimeHost (disabledRuntimeHost)
import JazzNext.Compiler.WarningConfig (defaultWarningSettings)
import JazzNext.TestHarness
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
    ("closure application records forcing and continuation depth", testClosureApplication),
    ("nested evaluator machines preserve outer continuation depth", testNestedContinuationDepth),
    ("builtin application is classified independently", testBuiltinApplication),
    ("infix operator evaluation is classified independently", testOperatorApplication),
    ("constructor application is classified independently", testConstructorApplication),
    ("closure creation records zero, one, and multiple captured bindings", testClosureCaptureWidths),
    ("source values record each logical construction category", testSourceConstructions),
    ("builtin results record their logical constructions", testBuiltinConstructions),
    ("case evaluation records attempts, matches, and introduced bindings", testPatternStatistics),
    ("builtin calls and host operations remain distinct", testBuiltinAndHostStatistics),
    ("deferred binding caches distinguish misses and hits", testDeferredCacheHitAndMiss),
    ("recursive deferred evaluation records its own cache outcome", testDeferredCacheRecursion),
    ("human statistics use stable meaningful labels", testHumanRenderer),
    ("JSON statistics are deterministic, explicit, and round trip", testJsonRenderer),
    ("runtime failure retains a partial report", testRuntimeFailureReport),
    ("compile failure has no runtime report", testCompileFailureHasNoReport)
  ]

testDisabledBehavior :: IO ()
testDisabledBehavior = do
  source <- readFixture "literal-success.jz"
  ordinary <- runSource defaultWarningSettings source
  observed <- runSourceObserved RuntimeObservationDisabled defaultWarningSettings source
  assertEqual "disabled result" ordinary observed
  assertEqual "disabled report" Nothing (runRuntimeObservation observed)

testDriverTransport :: IO ()
testDriverTransport = do
  source <- readFixture "literal-success.jz"
  result <- runSourceObserved RuntimeObservationStatistics defaultWarningSettings source
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
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
  let observed = evaluateRuntimeExprObserved RuntimeObservationStatistics (ELit (LInt 1))
  assertEqual
    "literal result"
    (RuntimeOutcomeCompleted (Just (VInt 1 untypedIntMetadata)))
    (runtimeObservationOutcome observed)
  report <- requireObservedReport observed
  let statistics = runtimeObservationStatistics report
  assertEqual "literal transitions" 2 (runtimeEvaluatorTransitions statistics)
  assertEqual "literal forced values" 0 (runtimeForcedValues statistics)
  assertEqual "literal applications" 0 (runtimeApplications statistics)
  assertEqual "literal continuation depth" 0 (runtimeMaximumContinuationDepth statistics)

testClosureApplication :: IO ()
testClosureApplication = do
  let expression =
        EApply
          (ELambda "value" (EVar "value"))
          (ELit (LInt 7))
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
  let value = ELit (LInt 7)
      callback =
        ELambda
          "value"
          (EList [EList [EList [EVar "value"]]])
  directStatistics <- statisticsFor (EApply callback value)
  nestedStatistics <-
    statisticsFor
      ( EApply
          (EApply (kernelBuiltin BuiltinMap) callback)
          (EList [value, value])
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
        EApply
          (EVar (BuiltinName (mkIdentifier (builtinSymbolKernelName BuiltinTextLength))))
          (ELit (LText "Jazz"))
      observed = evaluateRuntimeExprObserved RuntimeObservationStatistics expression
  report <- requireObservedSuccess observed
  let statistics = runtimeObservationStatistics report
  assertEqual "builtin applications" 1 (runtimeBuiltinApplications statistics)
  assertEqual "closure applications" 0 (runtimeClosureApplications statistics)
  assertEqual "total applications" 1 (runtimeApplications statistics)

testOperatorApplication :: IO ()
testOperatorApplication = do
  statistics <- statisticsFor (EBinary "+" (ELit (LInt 1)) (ELit (LInt 2)))
  assertEqual "operator applications" 1 (runtimeOperatorApplications statistics)
  assertEqual "operator total applications" 1 (runtimeApplications statistics)
  assertEqual "operator builtin calls" 0 (runtimeBuiltinCalls statistics)

testConstructorApplication :: IO ()
testConstructorApplication = do
  let expression =
        EBlock
          [ SData
              (SourceSpan 1 1)
              "Box"
              []
              [DataConstructor "Box" [DataConstructorArgumentName "value"]],
            SExpr
              (SourceSpan 2 1)
              (EApply (EVar "Box") (ELit (LInt 1)))
          ]
      observed = evaluateRuntimeExprObserved RuntimeObservationStatistics expression
  report <- requireObservedSuccess observed
  let statistics = runtimeObservationStatistics report
  assertEqual "constructor applications" 1 (runtimeConstructorApplications statistics)
  assertEqual "total applications" 1 (runtimeApplications statistics)

testClosureCaptureWidths :: IO ()
testClosureCaptureWidths = do
  zero <- statisticsFor (ELambda "value" (EVar "value"))
  one <-
    statisticsFor
      ( EBlock
          [ SLet "first" (SourceSpan 1 1) (ELit (LInt 1)),
            SExpr (SourceSpan 2 1) (ELambda "value" (EVar "first"))
          ]
      )
  multiple <-
    statisticsFor
      ( EBlock
          [ SLet "first" (SourceSpan 1 1) (ELit (LInt 1)),
            SLet "second" (SourceSpan 2 1) (ELit (LInt 2)),
            SExpr (SourceSpan 3 1) (ELambda "value" (ETuple [EVar "first", EVar "second"]))
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

testSourceConstructions :: IO ()
testSourceConstructions = do
  statistics <-
    statisticsFor
      ( EBlock
          [ SData
              (SourceSpan 1 1)
              "Box"
              []
              [DataConstructor "Box" [DataConstructorArgumentName "value"]],
            SExpr
              (SourceSpan 2 1)
              ( ETuple
                  [ EList [ELit (LInt 1), ELit (LInt 2)],
                    EApply (EVar "Box") (ELit (LInt 3))
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
      ( EApply
          (kernelBuiltin BuiltinTextUnconsRaw)
          (ELit (LText "Jazz"))
      )
  assertEqual "builtin list cells" 1 (runtimeListCellsConstructed statistics)
  assertEqual "builtin tuples" 1 (runtimeTuplesConstructed statistics)

testPatternStatistics :: IO ()
testPatternStatistics = do
  statistics <-
    statisticsFor
      ( EPatternCase
          (ETuple [ELit (LInt 1), ELit (LInt 2)])
          [ CaseArm (PLiteral (LInt 0)) Nothing (ELit (LInt 0)),
            CaseArm (PTuple [PVariable "left", PVariable "right"]) Nothing (EVar "left")
          ]
      )
  assertEqual "pattern attempts" 2 (runtimePatternAttempts statistics)
  assertEqual "pattern matches" 1 (runtimePatternMatches statistics)
  assertEqual "pattern bindings" 2 (runtimePatternBindings statistics)

testBuiltinAndHostStatistics :: IO ()
testBuiltinAndHostStatistics = do
  pureBuiltin <-
    statisticsFor
      ( EApply
          (kernelBuiltin BuiltinTextLength)
          (ELit (LText "Jazz"))
      )
  hostBuiltin <-
    statisticsFor
      ( EApply
          (kernelBuiltin BuiltinArguments)
          (ETuple [])
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
          ( EBlock
              [ SLet "loop" (SourceSpan 1 1) (EVar "loop"),
                SExpr (SourceSpan 2 1) (EVar "loop")
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
  let first = encodeRuntimeObservationJson zeroReport
      second = encodeRuntimeObservationJson zeroReport
  assertEqual "deterministic JSON bytes" first second
  assertEqual "JSON round trip" (Right zeroReport) (decodeRuntimeObservationJson first)
  assertLazyBytesContain "JSON schema version" "\"schemaVersion\":1" first
  assertLazyBytesContain "JSON explicit zero" "\"closuresCreated\":0" first
  assertEqual "compact JSON" False (LazyByteString.elem '\n' first)

testRuntimeFailureReport :: IO ()
testRuntimeFailureReport = do
  source <- readFixture "runtime-failure.jz"
  result <- runSourceObserved RuntimeObservationStatistics defaultWarningSettings source
  assertEqual "compile errors" [] (runCompileErrors result)
  case runRuntimeErrors result of
    [] -> failTest "expected a runtime diagnostic"
    _ -> pure ()
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
  assertEqual "runtime report" Nothing (runRuntimeObservation result)

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

assertPositive :: (Ord number, Num number, Show number) => Text -> number -> IO ()
assertPositive label value =
  if value > 0
    then pure ()
    else failTest (label <> ": expected a positive value, got " <> Text.pack (show value))

statisticsFor :: Expr -> IO RuntimeStatistics
statisticsFor expression = do
  report <- requireObservedSuccess (evaluateRuntimeExprObserved RuntimeObservationStatistics expression)
  pure (runtimeObservationStatistics report)

kernelBuiltin :: BuiltinSymbol -> Expr
kernelBuiltin = EVar . BuiltinName . mkIdentifier . builtinSymbolKernelName

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
