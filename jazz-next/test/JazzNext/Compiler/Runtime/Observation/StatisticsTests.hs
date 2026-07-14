{-# LANGUAGE OverloadedStrings #-}

module JazzNext.Compiler.Runtime.Observation.StatisticsTests
  ( tests,
  )
where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import JazzNext.Compiler.AST
  ( DataConstructor (..),
    DataConstructorArgument (..),
    Expr (..),
    Literal (..),
    Statement (..),
  )
import JazzNext.Compiler.BuiltinCatalog
  ( BuiltinSymbol (BuiltinTextLength),
    builtinSymbolKernelName,
  )
import JazzNext.Compiler.Diagnostics (SourceSpan (..))
import JazzNext.Compiler.Driver
  ( RunResult (..),
    runModuleGraphObserved,
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
    RuntimeStatistics (..),
    RuntimeTermination (..),
  )
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
    ("builtin application is classified independently", testBuiltinApplication),
    ("constructor application is classified independently", testConstructorApplication),
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
  assertEqual "literal result" (Right (Just (VInt 1 untypedIntMetadata))) (runtimeObservationOutcome observed)
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
    Left diagnostic -> failTest ("expected runtime success, got " <> Text.pack (show diagnostic))
    Right _ -> pure ()
  requireObservedReport observed

assertPositive :: (Ord number, Num number, Show number) => Text -> number -> IO ()
assertPositive label value =
  if value > 0
    then pure ()
    else failTest (label <> ": expected a positive value, got " <> Text.pack (show value))

readFixture :: FilePath -> IO Text
readFixture name = TextIO.readFile ("test/fixtures/runtime-observation/" <> name)
