{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Jazz.Compiler.Stdlib.Shared
  ( assertStdlibConstructorPrivate,
    assertSuccessfulStdlibOutput,
    runStdlibFixtureExpecting,
    runStdlibPrivateProbeValue,
    runStdlibSource,
    runStdlibSourceObserved,
  )
where

import Data.Text (Text)
import qualified Data.Text as Text
import Jazz.Compiler.BundledPrelude
  ( loadBundledPreludeSource,
  )
import Jazz.Compiler.DiagnosticCatalog
  ( ErrorCode (E4001),
  )
import Jazz.Compiler.Diagnostics
  ( Diagnostic,
    DiagnosticOrigin (CompilationOrigin),
    mkErrorDiagnostic,
  )
import Jazz.Compiler.Diagnostics.Render
  ( renderDiagnostic,
  )
import Jazz.Compiler.Driver
  ( RunResult,
    buildAnalyzedProgram,
    runCompileErrors,
    runModuleGraph,
    runModuleGraphObserved,
    runOutput,
    runRuntimeErrors,
  )
import Jazz.Compiler.ModuleGraph
  ( analyzedProgramErrors,
  )
import Jazz.Compiler.ModuleResolver
  ( ModuleResolutionConfig (..),
  )
import Jazz.Compiler.ModuleRuntime
  ( RuntimeProgram (runtimeProgramOutput),
    evaluateAnalyzedProgram,
  )
import Jazz.Compiler.Prelude
  ( ResolvedPrelude (PreludeBundled),
  )
import Jazz.Compiler.Runtime
  ( RuntimeValue,
  )
import Jazz.Compiler.Runtime.Observation
  ( RuntimeObservationRequest,
  )
import Jazz.Compiler.WarningConfig
  ( defaultWarningSettings,
  )
import Jazz.Repository.SourceLayout
  ( JazzSourceRole (StandardLibrarySource),
  )
import Jazz.TestHarness
  ( assertContains,
    assertEqual,
    failTest,
  )
import Jazz.TestSource
  ( readCheckedInJazzModuleSource,
    readCheckedInJazzTestFixture,
  )

runStdlibFixture :: [Text] -> FilePath -> IO RunResult
runStdlibFixture modulePath fixturePath = do
  source <- readCheckedInJazzTestFixture fixturePath
  runStdlibSource modulePath source

runStdlibFixtureExpecting :: [Text] -> FilePath -> Text -> IO ()
runStdlibFixtureExpecting modulePath fixturePath expectedOutput = do
  result <- runStdlibFixture modulePath fixturePath
  assertSuccessfulStdlibOutput expectedOutput result

assertSuccessfulStdlibOutput :: Text -> RunResult -> IO ()
assertSuccessfulStdlibOutput expectedOutput result = do
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just expectedOutput) (runOutput result)

assertStdlibConstructorPrivate :: [Text] -> Text -> Text -> IO ()
assertStdlibConstructorPrivate modulePath constructorName source = do
  result <- runStdlibSource modulePath source
  case runCompileErrors result of
    [] -> failTest (constructorName <> " constructor was unexpectedly public")
    diagnostics ->
      assertContains
        (constructorName <> " private-constructor diagnostic")
        ("unbound variable '" <> constructorName <> "'")
        (Text.unlines (map renderDiagnostic diagnostics))

runStdlibPrivateProbeValue :: [Text] -> Text -> IO (Either Diagnostic (Maybe RuntimeValue))
runStdlibPrivateProbeValue targetModulePath probeSource = do
  bundledPreludeSource <- loadBundledPreludeSource
  analyzedResult <-
    buildAnalyzedProgram
      defaultWarningSettings
      (PreludeBundled bundledPreludeSource)
      resolverConfig
      targetModulePath
      probeSourceLookup
  pure $ do
    (_, _, maybeAnalyzedProgram) <- analyzedResult
    analyzedProgram <- maybe (Left (privateProbeDiagnostic targetModulePath)) Right maybeAnalyzedProgram
    case analyzedProgramErrors analyzedProgram of
      firstError : _ -> Left firstError
      [] -> runtimeProgramOutput <$> evaluateAnalyzedProgram analyzedProgram
  where
    targetSourcePath = "src/" <> modulePathFile targetModulePath <> ".jz"
    probeSourceLookup sourcePath = do
      maybeSource <- readCheckedInJazzModuleSource StandardLibrarySource sourcePath
      pure
        ( if sourcePath == targetSourcePath
            then injectPrivateProbe probeSource <$> maybeSource
            else maybeSource
        )

runStdlibSource :: [Text] -> Text -> IO RunResult
runStdlibSource modulePath entrySource =
  runModuleGraph
    defaultWarningSettings
    resolverConfig
    modulePath
    lookupSource
  where
    entryPath = "src/" <> modulePathFile modulePath <> ".jz"

    lookupSource path
      | path == entryPath = pure (Just entrySource)
      | otherwise = readCheckedInJazzModuleSource StandardLibrarySource path

runStdlibSourceObserved :: RuntimeObservationRequest -> [Text] -> Text -> IO RunResult
runStdlibSourceObserved observationRequest modulePath entrySource =
  runModuleGraphObserved
    observationRequest
    defaultWarningSettings
    resolverConfig
    modulePath
    lookupSource
  where
    entryPath = "src/" <> modulePathFile modulePath <> ".jz"

    lookupSource path
      | path == entryPath = pure (Just entrySource)
      | otherwise = readCheckedInJazzModuleSource StandardLibrarySource path

resolverConfig :: ModuleResolutionConfig
resolverConfig =
  ModuleResolutionConfig
    { moduleRoots = ["src"],
      moduleExtension = ".jz"
    }

modulePathFile :: [Text] -> FilePath
modulePathFile =
  foldr1 (\segment suffix -> segment <> "/" <> suffix) . map Text.unpack

injectPrivateProbe :: Text -> Text -> Text
injectPrivateProbe probeSource moduleSource =
  case Text.breakOnEnd "}" moduleSource of
    (prefix, suffix)
      | not (Text.null prefix) ->
          Text.dropEnd 1 prefix <> "\n" <> probeSource <> "\n}" <> suffix
    _ -> moduleSource

privateProbeDiagnostic :: [Text] -> Diagnostic
privateProbeDiagnostic modulePath =
  mkErrorDiagnostic
    E4001
    CompilationOrigin
    ("private stdlib probe could not retain module scope '" <> Text.intercalate "::" modulePath <> "'")
