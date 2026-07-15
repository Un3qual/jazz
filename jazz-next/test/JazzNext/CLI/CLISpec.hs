{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Exception (finally, try)
import Data.Aeson (Value, eitherDecode)
import qualified Data.ByteString.Lazy as LazyByteString
import Data.IORef
  ( IORef,
    modifyIORef',
    newIORef,
    readIORef,
    writeIORef
  )
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Text.IO as TextIO
import JazzNext.CLI.Main
  ( CliOptions (..),
    CliOutput (..),
    RuntimeProfileWriter,
    RuntimeStatisticsFormat (..),
    parseCliOptions,
    runCliWith,
    runCliWithHost,
    runCliWithHostAndProfileWriter
  )
import JazzNext.Compiler.Diagnostics
  ( mkMessageDiagnostic,
    renderDiagnostic
  )
import JazzNext.Compiler.BundledPrelude
  ( bundledPreludeSource
  )
import JazzNext.Compiler.RuntimeHost
  ( RuntimeHost (..),
    disabledRuntimeHost,
    productionRuntimeHost
  )
import JazzNext.TestHarness
  ( NamedTest,
    assertContains,
    assertEqual,
    failTest,
    runTestSuite
  )
import System.Directory
  ( createDirectory,
    doesDirectoryExist,
    doesFileExist,
    getTemporaryDirectory,
    listDirectory,
    removeDirectoryRecursive,
    removeFile
  )
import System.FilePath ((</>))
import System.IO
  ( hClose,
    openTempFile
  )
import System.Exit (ExitCode)

main :: IO ()
main = runTestSuite "CLISpec" tests

tests :: [NamedTest]
tests =
  [ ("parseCliOptions captures warning flags and config path", testParseOptions),
    ("parseCliOptions captures run mode", testParseRunMode),
    ("parseCliOptions captures runtime statistics formats", testParseRuntimeStatistics),
    ("parseCliOptions captures runtime profile paths", testParseRuntimeProfile),
    ("parseCliOptions rejects invalid runtime observation flags", testParseInvalidRuntimeObservation),
    ("parseCliOptions rejects runtime observation outside run mode", testRejectRuntimeObservationWithoutRun),
    ("parseCliOptions captures positional source path", testParseSourcePath),
    ("parseCliOptions rejects multiple positional source paths", testParseMultipleSourcePaths),
    ("parseCliOptions captures explicit stdin sentinel", testParseExplicitStdinSentinel),
    ("parseCliOptions rejects explicit stdin plus source file", testParseExplicitStdinWithSourcePath),
    ("parseCliOptions rejects source path with entry module", testParseSourcePathWithEntryModule),
    ("parseCliOptions rejects module roots without entry module", testParseModuleRootWithoutEntryModule),
    ("parseCliOptions captures entry module and module roots", testParseModuleGraphOptions),
    ("parseCliOptions captures prelude path", testParsePreludePath),
    ("parseCliOptions captures no-prelude switch", testParseNoPrelude),
    ("parseCliOptions rejects conflicting prelude switches", testParsePreludeConflict),
    ("cli compile prints warnings to stderr while keeping stdout empty", testCliWarningOnlyBehavior),
    ("cli run returns non-zero and suppresses stdout when warning promoted", testCliPromotedWarningBehavior),
    ("cli --run prints evaluated runtime output", testCliRunModeSuccess),
    ("cli injects host capabilities only in run mode", testCliInjectsHostOnlyInRunMode),
    ("cli --help prints usage without reading inputs", testCliHelpOutput),
    ("cli -h prints usage without reading inputs", testCliShortHelpOutput),
    ("cli help flag preempts other args and input reads", testCliHelpPreemptsOtherArgs),
    ("cli compiles positional source file quietly", testCliCompileSourceFileSuccess),
    ("cli --run executes positional source file", testCliRunSourceFileSuccess),
    ("cli explicit stdin sentinel compiles stdin quietly", testCliCompileExplicitStdinSuccess),
    ("cli --run explicit stdin sentinel executes stdin", testCliRunExplicitStdinSuccess),
    ("cli positional source file reports missing file", testCliSourceFileMissing),
    ("cli --run prints evaluated section runtime output", testCliRunModeSectionSuccess),
    ("cli --run prints evaluated list primitive output", testCliRunModeListPrimitiveSuccess),
    ("cli --run prints evaluated filter primitive output", testCliRunModeFilterPrimitiveSuccess),
    ("cli --run with entry module loads module graph and ignores stdin", testCliRunModeModuleGraphSuccess),
    ("cli module graph uses default dot root when roots are omitted", testCliModuleGraphDefaultRootSuccess),
    ("cli module graph compile succeeds without runtime stdout", testCliModuleGraphCompileSuccess),
    ("cli module graph compile reports resolver diagnostics", testCliModuleGraphCompileError),
    ("cli module graph compile reports missing import symbol diagnostics", testCliModuleGraphMissingImportSymbol),
    ("cli module graph compile reports module declaration mismatch diagnostics", testCliModuleGraphDeclarationMismatch),
    ("cli module graph compile reports fail-fast module parse diagnostics", testCliModuleGraphParseFailure),
    ("cli loads bundled default prelude when no flag or env override is set", testCliLoadsBundledDefaultPrelude),
    ("cli bundled default prelude preserves user diagnostic spans", testCliBundledPreludePreservesUserDiagnosticSpans),
    ("cli loads bundled default prelude without path lookup fallback", testCliLoadsBundledPreludeWithoutPathLookup),
    ("cli explicit prelude matching bundled source still emits rebinding warnings", testCliExplicitPreludeMatchingBundledSourceEmitsWarnings),
    ("cli --run composes explicit prelude source before user source", testCliRunModePreludeFromFlag),
    ("cli --no-prelude disables bundled default prelude", testCliNoPreludeDisablesBundledDefault),
    ("cli prelude load failures return argument/config error", testCliPreludeLoadFailure),
    ("cli prelude parse failures return compile diagnostics", testCliPreludeParseFailure),
    ("cli prelude bridge conformance failures return compile diagnostics", testCliPreludeBridgeFailure),
    ("cli --no-prelude disables env-selected prelude path", testCliNoPreludeOverridesEnvPath),
    ("cli --run reports runtime fatal errors", testCliRunModeFatalRuntimeError),
    ("cli --run reports hd empty-list fatal runtime error", testCliRunModeHdEmptyListRuntimeError),
    ("cli runtime statistics preserve stdout and render on stderr", testCliRuntimeStatisticsOutput),
    ("cli runtime observation preserves pure recursive-alias diagnostics", testCliRuntimeObservationPreservesAliasDiagnostics),
    ("cli separates JSON statistics from unterminated host stderr", testCliRuntimeStatisticsAfterHostStderr),
    ("cli writes deterministic semantic profiles through an injected writer", testCliRuntimeProfileOutput),
    ("cli combines statistics and semantic profiles in one run", testCliCombinedRuntimeObservation),
    ("cli observed exit finalizes statistics and semantic profiles", testCliObservedExitFinalizesArtifacts),
    ("cli observes module-graph execution with the same artifact contract", testCliModuleGraphRuntimeObservation),
    ("cli compile failures emit no runtime artifacts", testCliCompileFailureHasNoRuntimeArtifacts),
    ("cli runtime failures emit partial runtime artifacts", testCliRuntimeFailureHasPartialArtifacts),
    ("cli profile write failures are structured command failures", testCliProfileWriteFailure),
    ("cli atomically replaces requested runtime profile files", testCliAtomicProfileReplacement),
    ("cli atomic profile failures preserve destinations and clean temporary files", testCliAtomicProfileFailureCleanup),
    ("cli precedence keeps CLI over env over config", testCliPrecedenceBehavior),
    ("cli respects --warnings-config path override", testCliConfigPathOverride),
    ("cli explicit --warnings-config read failures return config error", testCliExplicitConfigPathFailure),
    ("cli explicit env warning config read failures return config error", testCliExplicitEnvConfigPathFailure),
    ("cli defers source read until after arg validation", testCliDefersSourceReadOnArgError),
    ("cli rejects source plus entry module before reading source", testCliRejectsSourcePathWithEntryModuleBeforeRead),
    ("cli rejects explicit stdin sentinel with entry module before reading source", testCliRejectsExplicitStdinWithEntryModuleBeforeRead),
    ("cli rejects module roots without entry before reading source", testCliRejectsModuleRootWithoutEntryBeforeRead),
    ("cli rejects nested module declaration in source input", testCliRejectsNestedModuleDeclarationInSourceInput),
    ("cli accepts concrete list signature from source input", testCliAcceptsConcreteListSignature),
    ("cli accepts simple function signature from source input", testCliAcceptsSimpleFunctionSignature),
    ("cli reports signature type mismatch from source input", testCliReportsSignatureTypeMismatch)
  ]

testParseOptions :: IO ()
testParseOptions = do
  options <-
    case parseCliOptions ["-Wsame-scope-rebinding", "--warnings-config", "config/warnings.txt"] of
      Left err -> failTest ("parseCliOptions failed: " <> renderDiagnostic err)
      Right parsed -> pure parsed
  assertEqual "warning flags" ["-Wsame-scope-rebinding"] (cliWarningFlags options)
  assertEqual "config path" (Just "config/warnings.txt") (cliWarningsConfigPath options)
  assertEqual "run mode" False (cliRunMode options)
  assertEqual "prelude path" Nothing (cliPreludePath options)
  assertEqual "prelude disabled" False (cliDisablePrelude options)

testParseRunMode :: IO ()
testParseRunMode = do
  options <-
    case parseCliOptions ["--run"] of
      Left err -> failTest ("parseCliOptions failed: " <> renderDiagnostic err)
      Right parsed -> pure parsed
  assertEqual "run mode" True (cliRunMode options)
  assertEqual "warning flags" [] (cliWarningFlags options)
  assertEqual "prelude path" Nothing (cliPreludePath options)
  assertEqual "prelude disabled" False (cliDisablePrelude options)

testParseRuntimeStatistics :: IO ()
testParseRuntimeStatistics = do
  defaultOptions <- requireParsedOptions ["--run", "--runtime-stats"]
  humanOptions <- requireParsedOptions ["--run", "--runtime-stats=human", "--runtime-stats"]
  jsonOptions <- requireParsedOptions ["--run", "--runtime-stats=json", "--runtime-stats=json"]
  assertEqual
    "default runtime statistics format"
    (Just RuntimeStatisticsHuman)
    (cliRuntimeStatisticsFormat defaultOptions)
  assertEqual
    "explicit human runtime statistics format"
    (Just RuntimeStatisticsHuman)
    (cliRuntimeStatisticsFormat humanOptions)
  assertEqual
    "JSON runtime statistics format"
    (Just RuntimeStatisticsJson)
    (cliRuntimeStatisticsFormat jsonOptions)

testParseRuntimeProfile :: IO ()
testParseRuntimeProfile = do
  equalsOptions <-
    requireParsedOptions
      ["--run", "--runtime-profile=profiles/program.speedscope.json", "--runtime-profile=profiles/program.speedscope.json"]
  spacedOptions <-
    requireParsedOptions
      ["--run", "--runtime-profile", "profiles/program.speedscope.json"]
  assertEqual
    "equals runtime profile path"
    (Just "profiles/program.speedscope.json")
    (cliRuntimeProfilePath equalsOptions)
  assertEqual
    "space-separated runtime profile path"
    (Just "profiles/program.speedscope.json")
    (cliRuntimeProfilePath spacedOptions)

testParseInvalidRuntimeObservation :: IO ()
testParseInvalidRuntimeObservation = do
  assertParseErrorContains
    "empty runtime statistics format"
    "runtime statistics format"
    ["--run", "--runtime-stats="]
  assertParseErrorContains
    "unknown runtime statistics format"
    "runtime statistics format"
    ["--run", "--runtime-stats=xml"]
  assertParseErrorContains
    "conflicting runtime statistics formats"
    "conflicting runtime statistics"
    ["--run", "--runtime-stats=human", "--runtime-stats=json"]
  assertParseErrorContains
    "missing runtime profile path"
    "missing path"
    ["--run", "--runtime-profile"]
  assertParseErrorContains
    "empty runtime profile path"
    "empty runtime profile path"
    ["--run", "--runtime-profile="]
  assertParseErrorContains
    "conflicting runtime profile paths"
    "conflicting runtime profile"
    ["--run", "--runtime-profile=first.json", "--runtime-profile=second.json"]

testRejectRuntimeObservationWithoutRun :: IO ()
testRejectRuntimeObservationWithoutRun = do
  assertParseErrorContains
    "statistics require run mode"
    "requires --run"
    ["--runtime-stats"]
  assertParseErrorContains
    "profile requires run mode"
    "requires --run"
    ["--runtime-profile=profile.json"]

testParseSourcePath :: IO ()
testParseSourcePath = do
  options <-
    case parseCliOptions ["--run", "first.jz"] of
      Left err -> failTest ("parseCliOptions failed: " <> renderDiagnostic err)
      Right parsed -> pure parsed
  assertEqual "run mode" True (cliRunMode options)
  assertEqual "source path" (Just "first.jz") (cliSourcePath options)

testParseMultipleSourcePaths :: IO ()
testParseMultipleSourcePaths =
  case parseCliOptions ["first.jz", "second.jz"] of
    Left err ->
      assertContains "multiple source path message" "multiple source files are not supported" (renderDiagnostic err)
    Right _ ->
      failTest "expected multiple source paths to fail option parsing"

testParseExplicitStdinSentinel :: IO ()
testParseExplicitStdinSentinel = do
  options <-
    case parseCliOptions ["--run", "-"] of
      Left err -> failTest ("parseCliOptions failed: " <> renderDiagnostic err)
      Right parsed -> pure parsed
  assertEqual "run mode" True (cliRunMode options)
  assertEqual "stdin sentinel source selector" (Just "-") (cliSourcePath options)

testParseExplicitStdinWithSourcePath :: IO ()
testParseExplicitStdinWithSourcePath = do
  case parseCliOptions ["-", "first.jz"] of
    Left err ->
      assertContains "stdin plus source path message" "multiple source files are not supported" (renderDiagnostic err)
    Right _ ->
      failTest "expected explicit stdin plus source file to fail option parsing"
  case parseCliOptions ["first.jz", "-"] of
    Left err ->
      assertContains "source path plus stdin message" "multiple source files are not supported" (renderDiagnostic err)
    Right _ ->
      failTest "expected source file plus explicit stdin to fail option parsing"

testParseSourcePathWithEntryModule :: IO ()
testParseSourcePathWithEntryModule = do
  case parseCliOptions ["--entry-module", "App::Main", "first.jz"] of
    Left err ->
      assertContains "source path with entry module message" "cannot combine source file with --entry-module" (renderDiagnostic err)
    Right _ ->
      failTest "expected source path plus entry module to fail option parsing"
  case parseCliOptions ["first.jz", "--entry-module", "App::Main"] of
    Left err ->
      assertContains "source path with entry module reversed message" "cannot combine source file with --entry-module" (renderDiagnostic err)
    Right _ ->
      failTest "expected source path before entry module to fail option parsing"

testParseModuleRootWithoutEntryModule :: IO ()
testParseModuleRootWithoutEntryModule =
  case parseCliOptions ["--module-root", "src"] of
    Left err ->
      assertContains "module root without entry message" "cannot use --module-root without --entry-module" (renderDiagnostic err)
    Right _ ->
      failTest "expected module root without entry module to fail option parsing"

testParseModuleGraphOptions :: IO ()
testParseModuleGraphOptions = do
  options <-
    case parseCliOptions ["--run", "--entry-module", "App::Main", "--module-root", "src", "--module-root", "stdlib"] of
      Left err -> failTest ("parseCliOptions failed: " <> renderDiagnostic err)
      Right parsed -> pure parsed
  assertEqual "run mode" True (cliRunMode options)
  assertEqual "entry module" (Just ["App", "Main"]) (cliEntryModule options)
  assertEqual "module roots" ["src", "stdlib"] (cliModuleRoots options)

testParsePreludePath :: IO ()
testParsePreludePath = do
  options <-
    case parseCliOptions ["--prelude", "stdlib/Prelude.jz"] of
      Left err -> failTest ("parseCliOptions failed: " <> renderDiagnostic err)
      Right parsed -> pure parsed
  assertEqual "prelude path" (Just "stdlib/Prelude.jz") (cliPreludePath options)
  assertEqual "prelude disabled" False (cliDisablePrelude options)

testParseNoPrelude :: IO ()
testParseNoPrelude = do
  options <-
    case parseCliOptions ["--no-prelude"] of
      Left err -> failTest ("parseCliOptions failed: " <> renderDiagnostic err)
      Right parsed -> pure parsed
  assertEqual "prelude path" Nothing (cliPreludePath options)
  assertEqual "prelude disabled" True (cliDisablePrelude options)

testParsePreludeConflict :: IO ()
testParsePreludeConflict =
  case parseCliOptions ["--prelude", "stdlib/Prelude.jz", "--no-prelude"] of
    Left err ->
      assertContains "conflict message" "cannot combine --prelude with --no-prelude" (renderDiagnostic err)
    Right _ ->
      failTest "expected prelude flag conflict to fail option parsing"

testCliWarningOnlyBehavior :: IO ()
testCliWarningOnlyBehavior = do
  output <- runCliWith ["-Wsame-scope-rebinding"] envLookup configLookup (pure sampleSource)
  assertEqual "exit code" 0 (cliExitCode output)
  assertContains "stderr includes warning code" "W0001" (cliStderr output)
  assertContains "stderr includes warning category" "same-scope-rebinding" (cliStderr output)
  assertEqual "stdout stays empty for compile-only success" "" (cliStdout output)
  where
    envLookup _ = pure Nothing
    configLookup _ = pure Nothing

testCliPromotedWarningBehavior :: IO ()
testCliPromotedWarningBehavior = do
  output <- runCliWith ["-Werror=same-scope-rebinding"] envLookup configLookup (pure sampleSource)
  assertEqual "exit code" 1 (cliExitCode output)
  assertContains "stderr includes warning code" "W0001" (cliStderr output)
  assertContains "stderr includes error marker" "error:" (cliStderr output)
  assertEqual "stdout is suppressed" "" (cliStdout output)
  where
    envLookup _ = pure Nothing
    configLookup _ = pure Nothing

testCliRunModeSuccess :: IO ()
testCliRunModeSuccess = do
  output <- runCliWith ["--run"] envLookup configLookup (pure runtimeSuccessSource)
  assertEqual "exit code" 0 (cliExitCode output)
  assertEqual "runtime stdout" "1\n" (cliStdout output)
  assertEqual "stderr is empty" "" (cliStderr output)
  where
    envLookup _ = pure Nothing
    configLookup _ = pure Nothing

testCliInjectsHostOnlyInRunMode :: IO ()
testCliInjectsHostOnlyInRunMode =
  withTemporaryPath $ \path -> do
    TextIO.writeFile path "before"
    let source =
          "__kernel_writeTextRaw! \""
            <> Text.pack path
            <> "\" \"Jazz λ\"."
        envLookup _ = pure Nothing
        fileLookup _ = pure Nothing
    compileOutput <-
      runCliWithHost productionRuntimeHost ["--no-prelude"] envLookup fileLookup (pure source)
    afterCompile <- TextIO.readFile path
    runOutput <-
      runCliWithHost productionRuntimeHost ["--run", "--no-prelude"] envLookup fileLookup (pure source)
    afterRun <- TextIO.readFile path
    assertEqual "compile-only file contents" "before" afterCompile
    assertEqual "compile-only exit" 0 (cliExitCode compileOutput)
    assertEqual "run file contents" "Jazz λ" afterRun
    assertEqual "run host exit" 0 (cliExitCode runOutput)

withTemporaryPath :: (FilePath -> IO a) -> IO a
withTemporaryPath action = do
  temporaryDirectory <- getTemporaryDirectory
  (path, handle) <- openTempFile temporaryDirectory "jazz-next-cli-host"
  hClose handle
  action path `finally` removeFile path

withTemporaryDirectory :: (FilePath -> IO a) -> IO a
withTemporaryDirectory action = do
  temporaryDirectory <- getTemporaryDirectory
  (path, handle) <- openTempFile temporaryDirectory "jazz-next-cli-profile"
  hClose handle
  removeFile path
  createDirectory path
  action path `finally` removeDirectoryRecursive path

testCliHelpOutput :: IO ()
testCliHelpOutput = do
  sourceRead <- newIORef False
  configRead <- newIORef False
  output <-
    runCliWith
      ["--help"]
      envLookup
      (recordConfigRead configRead)
      (recordSourceRead sourceRead)
  didReadSource <- readIORef sourceRead
  didReadConfig <- readIORef configRead
  assertHelpOutput "--help" output
  assertEqual "source is not read" False didReadSource
  assertEqual "config/prelude files are not read" False didReadConfig
  where
    envLookup "JAZZ_WARNING_CONFIG" = pure (Just "config/warnings.txt")
    envLookup "JAZZ_PRELUDE" = pure (Just "stdlib/Prelude.jz")
    envLookup _ = pure Nothing

testCliShortHelpOutput :: IO ()
testCliShortHelpOutput = do
  sourceRead <- newIORef False
  configRead <- newIORef False
  output <-
    runCliWith
      ["-h"]
      envLookup
      (recordConfigRead configRead)
      (recordSourceRead sourceRead)
  didReadSource <- readIORef sourceRead
  didReadConfig <- readIORef configRead
  assertHelpOutput "-h" output
  assertEqual "source is not read" False didReadSource
  assertEqual "config/prelude files are not read" False didReadConfig
  where
    envLookup "JAZZ_WARNING_CONFIG" = pure (Just "config/warnings.txt")
    envLookup "JAZZ_PRELUDE" = pure (Just "stdlib/Prelude.jz")
    envLookup _ = pure Nothing

testCliHelpPreemptsOtherArgs :: IO ()
testCliHelpPreemptsOtherArgs = do
  sourceRead <- newIORef False
  configRead <- newIORef False
  invalidOutput <-
    runCliWith
      ["--help", "--bad-arg"]
      envLookup
      (recordConfigRead configRead)
      (recordSourceRead sourceRead)
  missingSourceOutput <-
    runCliWith
      ["--help", "missing.jz"]
      envLookup
      (recordConfigRead configRead)
      (recordSourceRead sourceRead)
  moduleGraphOutput <-
    runCliWith
      ["--help", "--entry-module", "App::Main", "--module-root", "src"]
      envLookup
      (recordConfigRead configRead)
      (recordSourceRead sourceRead)
  didReadSource <- readIORef sourceRead
  didReadConfig <- readIORef configRead
  assertHelpOutput "invalid arg help" invalidOutput
  assertHelpOutput "missing source help" missingSourceOutput
  assertHelpOutput "module graph help" moduleGraphOutput
  assertEqual "source is not read" False didReadSource
  assertEqual "config/prelude files are not read" False didReadConfig
  where
    envLookup "JAZZ_WARNING_CONFIG" = pure (Just "config/warnings.txt")
    envLookup "JAZZ_PRELUDE" = pure (Just "stdlib/Prelude.jz")
    envLookup _ = pure Nothing

testCliCompileSourceFileSuccess :: IO ()
testCliCompileSourceFileSuccess = do
  sourceRead <- newIORef False
  output <-
    runCliWith
      ["first.jz"]
      envLookup
      fileLookup
      (recordSourceRead sourceRead)
  didRead <- readIORef sourceRead
  assertEqual "exit code" 0 (cliExitCode output)
  assertEqual "compile stdout stays empty" "" (cliStdout output)
  assertEqual "compile stderr stays empty" "" (cliStderr output)
  assertEqual "stdin source is ignored when source file is present" False didRead
  where
    envLookup _ = pure Nothing
    fileLookup "first.jz" = pure (Just firstProgramSource)
    fileLookup _ = pure Nothing

testCliRunSourceFileSuccess :: IO ()
testCliRunSourceFileSuccess = do
  output <- runCliWith ["--run", "first.jz"] envLookup fileLookup (pure "ignored = 1.")
  assertEqual "exit code" 0 (cliExitCode output)
  assertEqual "runtime stdout" "42\n" (cliStdout output)
  assertEqual "stderr is empty" "" (cliStderr output)
  where
    envLookup _ = pure Nothing
    fileLookup "first.jz" = pure (Just firstProgramSource)
    fileLookup _ = pure Nothing

testCliCompileExplicitStdinSuccess :: IO ()
testCliCompileExplicitStdinSuccess = do
  lookedUpPaths <- newIORef []
  sourceRead <- newIORef False
  output <-
    runCliWith
      ["-"]
      envLookup
      (recordLookupPath lookedUpPaths fileLookup)
      (recordSourceReadWith sourceRead firstProgramSource)
  didRead <- readIORef sourceRead
  paths <- readIORef lookedUpPaths
  assertEqual "exit code" 0 (cliExitCode output)
  assertEqual "compile stdout stays empty" "" (cliStdout output)
  assertEqual "compile stderr stays empty" "" (cliStderr output)
  assertEqual "stdin source is read" True didRead
  assertEqual "stdin sentinel is not file-looked-up" False ("-" `elem` paths)
  where
    envLookup _ = pure Nothing
    fileLookup _ = pure Nothing

testCliRunExplicitStdinSuccess :: IO ()
testCliRunExplicitStdinSuccess = do
  lookedUpPaths <- newIORef []
  sourceRead <- newIORef False
  output <-
    runCliWith
      ["--run", "-"]
      envLookup
      (recordLookupPath lookedUpPaths fileLookup)
      (recordSourceReadWith sourceRead firstProgramSource)
  didRead <- readIORef sourceRead
  paths <- readIORef lookedUpPaths
  assertEqual "exit code" 0 (cliExitCode output)
  assertEqual "runtime stdout" "42\n" (cliStdout output)
  assertEqual "stderr is empty" "" (cliStderr output)
  assertEqual "stdin source is read" True didRead
  assertEqual "stdin sentinel is not file-looked-up" False ("-" `elem` paths)
  where
    envLookup _ = pure Nothing
    fileLookup _ = pure Nothing

testCliSourceFileMissing :: IO ()
testCliSourceFileMissing = do
  output <- runCliWith ["--run", "missing.jz"] envLookup fileLookup (pure "ignored = 1.")
  assertEqual "exit code" 2 (cliExitCode output)
  assertContains "missing source diagnostic" "source file could not be read at 'missing.jz'" (cliStderr output)
  assertEqual "stdout is suppressed" "" (cliStdout output)
  compileOutput <- runCliWith ["missing.jz"] envLookup fileLookup (pure "ignored = 1.")
  assertEqual "compile exit code" 2 (cliExitCode compileOutput)
  assertContains "compile missing source diagnostic" "source file could not be read at 'missing.jz'" (cliStderr compileOutput)
  assertEqual "compile stdout is suppressed" "" (cliStdout compileOutput)
  where
    envLookup _ = pure Nothing
    fileLookup _ = pure Nothing

testCliRunModeSectionSuccess :: IO ()
testCliRunModeSectionSuccess = do
  output <- runCliWith ["--run"] envLookup configLookup (pure runtimeSectionSource)
  assertEqual "exit code" 0 (cliExitCode output)
  assertEqual "runtime stdout" "3\n" (cliStdout output)
  assertEqual "stderr is empty" "" (cliStderr output)
  where
    envLookup _ = pure Nothing
    configLookup _ = pure Nothing

testCliRunModeListPrimitiveSuccess :: IO ()
testCliRunModeListPrimitiveSuccess = do
  output <- runCliWith ["--run"] envLookup configLookup (pure runtimeListPrimitiveSource)
  assertEqual "exit code" 0 (cliExitCode output)
  assertEqual "runtime stdout" "[1, 3, 4]\n" (cliStdout output)
  assertEqual "stderr is empty" "" (cliStderr output)
  where
    envLookup _ = pure Nothing
    configLookup _ = pure Nothing

testCliRunModeFilterPrimitiveSuccess :: IO ()
testCliRunModeFilterPrimitiveSuccess = do
  output <- runCliWith ["--run"] envLookup configLookup (pure runtimeFilterPrimitiveSource)
  assertEqual "exit code" 0 (cliExitCode output)
  assertEqual "runtime stdout" "[2, 3]\n" (cliStdout output)
  assertEqual "stderr is empty" "" (cliStderr output)
  where
    envLookup _ = pure Nothing
    configLookup _ = pure Nothing

testCliRunModeModuleGraphSuccess :: IO ()
testCliRunModeModuleGraphSuccess = do
  sourceRead <- newIORef False
  output <-
    runCliWith
      ["--run", "--entry-module", "App::Main", "--module-root", "src"]
      envLookup
      fileLookup
      (recordSourceRead sourceRead)
  didRead <- readIORef sourceRead
  assertEqual "exit code" 0 (cliExitCode output)
  assertEqual "runtime stdout" "1\n" (cliStdout output)
  assertEqual "stderr is empty" "" (cliStderr output)
  assertEqual "stdin source is ignored in module mode" False didRead
  where
    envLookup _ = pure Nothing
    fileLookup key =
      pure
        ( Map.lookup
            key
            ( Map.fromList
                [ ("src/App/Main.jz", """
                module App::Main {
                import Lib::Util.
                util.
                }
                """),
                  ("src/Lib/Util.jz", """
                  module Lib::Util {
                  util = 1.
                  }
                  """)
                ]
            )
        )

testCliModuleGraphDefaultRootSuccess :: IO ()
testCliModuleGraphDefaultRootSuccess = do
  sourceRead <- newIORef False
  output <-
    runCliWith
      ["--entry-module", "App::Main"]
      envLookup
      fileLookup
      (recordSourceRead sourceRead)
  didRead <- readIORef sourceRead
  assertEqual "exit code" 0 (cliExitCode output)
  assertEqual "compile stdout stays empty" "" (cliStdout output)
  assertEqual "compile stderr stays empty" "" (cliStderr output)
  assertEqual "stdin source is ignored in default-root module mode" False didRead
  where
    envLookup _ = pure Nothing
    fileLookup key =
      pure
        ( Map.lookup
            key
            ( Map.fromList
                [ ("App/Main.jz", """
                module App::Main {
                import Lib::Util.
                util.
                }
                """),
                  ("Lib/Util.jz", """
                  module Lib::Util {
                  util = 1.
                  }
                  """)
                ]
            )
        )

testCliModuleGraphCompileSuccess :: IO ()
testCliModuleGraphCompileSuccess = do
  output <-
    runCliWith
      ["--entry-module", "App::Main", "--module-root", "src"]
      envLookup
      fileLookup
      (pure "ignored = 1.")
  assertEqual "exit code" 0 (cliExitCode output)
  assertEqual "compile stdout stays empty" "" (cliStdout output)
  assertEqual "compile stderr stays empty" "" (cliStderr output)
  where
    envLookup _ = pure Nothing
    fileLookup key =
      pure
        ( Map.lookup
            key
            ( Map.fromList
                [ ("src/App/Main.jz", """
                module App::Main {
                import Lib::Util.
                util.
                }
                """),
                  ("src/Lib/Util.jz", """
                  module Lib::Util {
                  util = 1.
                  }
                  """)
                ]
            )
        )

testCliModuleGraphCompileError :: IO ()
testCliModuleGraphCompileError = do
  output <-
    runCliWith
      ["--entry-module", "App::Main", "--module-root", "src"]
      envLookup
      fileLookup
      (pure "ignored = 1.")
  assertEqual "exit code" 1 (cliExitCode output)
  assertContains "resolver error code" "E4001" (cliStderr output)
  assertEqual "stdout is suppressed" "" (cliStdout output)
  where
    envLookup _ = pure Nothing
    fileLookup key = pure (Map.lookup key (Map.fromList [("src/App/Main.jz", """
    import Missing::Thing.
    1.
    """)]))

testCliModuleGraphMissingImportSymbol :: IO ()
testCliModuleGraphMissingImportSymbol = do
  output <-
    runCliWith
      ["--entry-module", "App::Main", "--module-root", "src"]
      envLookup
      fileLookup
      (pure "ignored = 1.")
  assertEqual "exit code" 1 (cliExitCode output)
  assertContains "missing symbol code" "E4007" (cliStderr output)
  assertContains "missing symbol text" "subtract" (cliStderr output)
  assertContains "imported module context" "Lib::Math" (cliStderr output)
  assertEqual "stdout is suppressed" "" (cliStdout output)
  where
    envLookup _ = pure Nothing
    fileLookup key =
      pure
        ( Map.lookup
            key
            ( Map.fromList
                [ ("src/App/Main.jz", """
                import Lib::Math (subtract).
                1.
                """),
                  ("src/Lib/Math.jz", "add = 1.")
                ]
            )
        )

testCliModuleGraphDeclarationMismatch :: IO ()
testCliModuleGraphDeclarationMismatch = do
  output <-
    runCliWith
      ["--entry-module", "App::Main", "--module-root", "src"]
      envLookup
      fileLookup
      (pure "ignored = 1.")
  assertEqual "exit code" 1 (cliExitCode output)
  assertContains "module declaration mismatch code" "E4006" (cliStderr output)
  assertContains "module declaration mismatch details" "Wrong::Name" (cliStderr output)
  assertContains "module declaration expected module" "App::Main" (cliStderr output)
  assertEqual "stdout is suppressed" "" (cliStdout output)
  where
    envLookup _ = pure Nothing
    fileLookup key =
      pure
        ( Map.lookup
            key
            (Map.fromList [("src/App/Main.jz", """
            module Wrong::Name {
            1.
            }
            """)])
        )

testCliModuleGraphParseFailure :: IO ()
testCliModuleGraphParseFailure = do
  output <-
    runCliWith
      ["--entry-module", "App::Main", "--module-root", "src"]
      envLookup
      fileLookup
      (pure "ignored = 1.")
  assertEqual "exit code" 1 (cliExitCode output)
  assertContains "module parse diagnostic code" "E4004" (cliStderr output)
  assertContains "module parse diagnostic path" "src/App/Main.jz" (cliStderr output)
  assertContains "fail-fast module syntax" "expected '{'" (cliStderr output)
  assertEqual "stdout is suppressed" "" (cliStdout output)
  where
    envLookup _ = pure Nothing
    fileLookup key =
      pure
        (Map.lookup key (Map.fromList [("src/App/Main.jz", "module App::Main.")]))

testCliLoadsBundledDefaultPrelude :: IO ()
testCliLoadsBundledDefaultPrelude = do
  output <- runCliWith ["--run"] envLookup configLookup (pure bundledPreludeConsumerSource)
  assertEqual "exit code" 0 (cliExitCode output)
  assertEqual "runtime stdout" "<function>\n" (cliStdout output)
  assertEqual "stderr is empty" "" (cliStderr output)
  where
    envLookup _ = pure Nothing
    configLookup _ = pure Nothing

testCliBundledPreludePreservesUserDiagnosticSpans :: IO ()
testCliBundledPreludePreservesUserDiagnosticSpans = do
  output <- runCliWith [] envLookup configLookup (pure signatureNameMismatchSource)
  assertEqual "exit code" 1 (cliExitCode output)
  assertContains "stderr includes signature mismatch code" "E1003" (cliStderr output)
  assertContains "stderr keeps user line numbers" "E1003: 1:1:" (cliStderr output)
  assertEqual "stdout is suppressed" "" (cliStdout output)
  where
    envLookup _ = pure Nothing
    configLookup _ = pure Nothing

testCliLoadsBundledPreludeWithoutPathLookup :: IO ()
testCliLoadsBundledPreludeWithoutPathLookup = do
  lookupPaths <- newIORef []
  let envLookup _ = pure Nothing
      configLookup path = do
        writeIORef lookupPaths . (path :) =<< readIORef lookupPaths
        pure Nothing
  output <- runCliWith ["--run"] envLookup configLookup (pure bundledPreludeKernelConsumerSource)
  lookedUpPaths <- readIORef lookupPaths
  assertEqual "exit code" 0 (cliExitCode output)
  assertEqual "runtime stdout" "<function>\n" (cliStdout output)
  assertEqual "stderr is empty" "" (cliStderr output)
  assertEqual
    "default bundled prelude should not probe old path-based fallbacks"
    []
    (filter isBundledPreludePath lookedUpPaths)

testCliExplicitPreludeMatchingBundledSourceEmitsWarnings :: IO ()
testCliExplicitPreludeMatchingBundledSourceEmitsWarnings = do
  output <-
    runCliWith
      ["-Werror=same-scope-rebinding", "--prelude", "tmp/Prelude.jz"]
      envLookup
      configLookup
      (pure "map = (+ 1). map 2.")
  assertEqual "exit code" 1 (cliExitCode output)
  assertContains "stderr includes warning code" "W0001" (cliStderr output)
  assertEqual "stdout is suppressed" "" (cliStdout output)
  where
    envLookup _ = pure Nothing
    configLookup key = pure (Map.lookup key (Map.fromList [("tmp/Prelude.jz", bundledPreludeSource)]))

testCliRunModePreludeFromFlag :: IO ()
testCliRunModePreludeFromFlag = do
  output <- runCliWith ["--run", "--prelude", "tmp/Prelude.jz"] envLookup configLookup (pure preludeConsumerSource)
  assertEqual "exit code" 0 (cliExitCode output)
  assertEqual "runtime stdout" "3\n" (cliStdout output)
  assertEqual "stderr is empty" "" (cliStderr output)
  where
    envLookup _ = pure Nothing
    configLookup key = pure (Map.lookup key (Map.fromList [("tmp/Prelude.jz", preludeSource)]))

testCliPreludeLoadFailure :: IO ()
testCliPreludeLoadFailure = do
  sourceRead <- newIORef False
  output <-
    runCliWith
      ["--run", "--prelude", "tmp/missing.jz"]
      envLookup
      configLookup
      (recordSourceRead sourceRead)
  didRead <- readIORef sourceRead
  assertEqual "exit code" 2 (cliExitCode output)
  assertContains "prelude load diagnostic code" "E0003" (cliStderr output)
  assertEqual "source should not be read when prelude load fails" False didRead
  where
    envLookup _ = pure Nothing
    configLookup _ = pure Nothing

testCliPreludeParseFailure :: IO ()
testCliPreludeParseFailure = do
  output <- runCliWith ["--run", "--prelude", "tmp/Prelude.jz"] envLookup configLookup (pure runtimeSuccessSource)
  assertEqual "exit code" 1 (cliExitCode output)
  assertContains "prelude parse diagnostic code" "E0002" (cliStderr output)
  assertEqual "stdout is suppressed on compile failure" "" (cliStdout output)
  where
    envLookup _ = pure Nothing
    configLookup key = pure (Map.lookup key (Map.fromList [("tmp/Prelude.jz", "broken = .")]))

testCliPreludeBridgeFailure :: IO ()
testCliPreludeBridgeFailure = do
  output <- runCliWith ["--run", "--prelude", "tmp/Prelude.jz"] envLookup configLookup (pure runtimeSuccessSource)
  assertEqual "exit code" 1 (cliExitCode output)
  assertContains "prelude bridge diagnostic code" "E0004" (cliStderr output)
  assertEqual "stdout is suppressed on compile failure" "" (cliStdout output)
  where
    envLookup _ = pure Nothing
    configLookup key = pure (Map.lookup key (Map.fromList [("tmp/Prelude.jz", "__kernel_unknown = unknown.")]))

testCliNoPreludeDisablesBundledDefault :: IO ()
testCliNoPreludeDisablesBundledDefault = do
  lookupPaths <- newIORef []
  let envLookup _ = pure Nothing
      configLookup path = do
        writeIORef lookupPaths . (path :) =<< readIORef lookupPaths
        pure Nothing
  output <- runCliWith ["--run", "--no-prelude"] envLookup configLookup (pure bundledPreludeKernelConsumerSource)
  lookedUpPaths <- readIORef lookupPaths
  assertEqual "exit code" 0 (cliExitCode output)
  assertEqual "kernel bridge runtime stdout" "<function>\n" (cliStdout output)
  assertEqual "stderr is empty" "" (cliStderr output)
  assertEqual
    "default bundled prelude lookup is skipped"
    []
    (filter isBundledPreludePath lookedUpPaths)

testCliNoPreludeOverridesEnvPath :: IO ()
testCliNoPreludeOverridesEnvPath = do
  output <- runCliWith ["--run", "--no-prelude"] envLookup configLookup (pure preludeConsumerSource)
  assertEqual "exit code" 1 (cliExitCode output)
  assertContains "unbound variable when prelude disabled" "E1001" (cliStderr output)
  where
    envLookup key = pure (Map.lookup key (Map.fromList [("JAZZ_PRELUDE", "tmp/Prelude.jz")]))
    configLookup key = pure (Map.lookup key (Map.fromList [("tmp/Prelude.jz", preludeSource)]))

testCliRunModeFatalRuntimeError :: IO ()
testCliRunModeFatalRuntimeError = do
  output <- runCliWith ["--run"] envLookup configLookup (pure runtimeDivisionByZeroSource)
  assertEqual "exit code" 1 (cliExitCode output)
  assertContains "runtime fatal code" "E3001" (cliStderr output)
  assertContains "stderr includes error marker" "error:" (cliStderr output)
  assertEqual "stdout is suppressed" "" (cliStdout output)
  where
    envLookup _ = pure Nothing
    configLookup _ = pure Nothing

testCliRunModeHdEmptyListRuntimeError :: IO ()
testCliRunModeHdEmptyListRuntimeError = do
  output <- runCliWith ["--run"] envLookup configLookup (pure runtimeHdEmptySource)
  assertEqual "exit code" 1 (cliExitCode output)
  assertContains "runtime fatal code" "E3009" (cliStderr output)
  assertContains "runtime fatal message" "empty list" (cliStderr output)
  assertEqual "stdout is suppressed" "" (cliStdout output)
  where
    envLookup _ = pure Nothing
    configLookup _ = pure Nothing

testCliRuntimeStatisticsOutput :: IO ()
testCliRuntimeStatisticsOutput = do
  baseline <- runObservationFixture [] literalSuccessFixture
  human <- runObservationFixture ["--runtime-stats"] literalSuccessFixture
  json <- runObservationFixture ["--runtime-stats=json"] literalSuccessFixture
  assertEqual "baseline exit" 0 (cliExitCode baseline)
  assertEqual "human statistics exit" 0 (cliExitCode human)
  assertEqual "JSON statistics exit" 0 (cliExitCode json)
  assertEqual "human statistics preserve stdout" (cliStdout baseline) (cliStdout human)
  assertEqual "JSON statistics preserve stdout" (cliStdout baseline) (cliStdout json)
  assertContains "human statistics heading" "Jazz runtime statistics" (cliStderr human)
  assertFinalStderrJson "JSON runtime statistics" json

testCliRuntimeObservationPreservesAliasDiagnostics :: IO ()
testCliRuntimeObservationPreservesAliasDiagnostics = do
  baseline <- runAliasFixture []
  observed <- runAliasFixture ["--runtime-stats=json"]
  assertEqual "baseline recursive-alias exit" 1 (cliExitCode baseline)
  assertEqual "observed recursive-alias exit" 1 (cliExitCode observed)
  assertContains "baseline recursive-alias diagnostic" "recursive alias cycle" (cliStderr baseline)
  assertContains "observed recursive-alias diagnostic" "recursive alias cycle" (cliStderr observed)
  assertEqual
    "observation does not substitute the host-recursion diagnostic"
    False
    ("recursive host binding" `Text.isInfixOf` cliStderr observed)
  assertFinalStderrJson "recursive-alias runtime statistics" observed
  where
    runAliasFixture observationArguments =
      runCliWithHost
        disabledRuntimeHost
        (["--run", "--no-prelude"] <> observationArguments)
        noEnvironment
        (const (pure Nothing))
        (pure "f = if True then f else 0. f.")

testCliRuntimeStatisticsAfterHostStderr :: IO ()
testCliRuntimeStatisticsAfterHostStderr = do
  hostStderr <- newIORef ""
  let host =
        disabledRuntimeHost
          { runtimeHostWriteStderr = \contents -> do
              modifyIORef' hostStderr (<> contents)
              pure (Right ())
          }
      source = "__kernel_writeStderrRaw! \"unterminated\"."
  output <-
    runCliWithHost
      host
      ["--run", "--no-prelude", "--runtime-stats=json"]
      noEnvironment
      (const (pure Nothing))
      (pure source)
  writtenByHost <- readIORef hostStderr
  assertEqual "host stderr remains byte-for-byte unchanged" "unterminated" writtenByHost
  assertEqual "buffered stderr starts with a separating newline" "\n" (Text.take 1 (cliStderr output))
  assertFinalStderrJson "JSON statistics after host stderr" output

testCliRuntimeProfileOutput :: IO ()
testCliRuntimeProfileOutput = do
  capturedProfile <- newIORef Nothing
  output <-
    runObservationFixtureWithWriter
      (capturingProfileWriter capturedProfile)
      ["--runtime-profile=profile.speedscope.json"]
      literalSuccessFixture
  assertEqual "profile exit" 0 (cliExitCode output)
  assertEqual "profile preserves stdout" "42\n" (cliStdout output)
  assertEqual "profile keeps stderr empty" "" (cliStderr output)
  captured <- readIORef capturedProfile
  case captured of
    Nothing -> failTest "runtime profile writer was not called"
    Just (path, bytes) -> do
      assertEqual "requested profile path" "profile.speedscope.json" path
      assertJsonBytes "semantic profile JSON" bytes

testCliCombinedRuntimeObservation :: IO ()
testCliCombinedRuntimeObservation = do
  capturedProfile <- newIORef Nothing
  output <-
    runObservationFixtureWithWriter
      (capturingProfileWriter capturedProfile)
      ["--runtime-stats=json", "--runtime-profile=combined.speedscope.json"]
      literalSuccessFixture
  assertEqual "combined observation exit" 0 (cliExitCode output)
  assertEqual "combined observation stdout" "42\n" (cliStdout output)
  assertFinalStderrJson "combined runtime statistics" output
  captured <- readIORef capturedProfile
  case captured of
    Nothing -> failTest "combined observation did not write a semantic profile"
    Just (_, bytes) -> assertJsonBytes "combined semantic profile JSON" bytes

testCliObservedExitFinalizesArtifacts :: IO ()
testCliObservedExitFinalizesArtifacts = do
  capturedProfile <- newIORef Nothing
  capturedStdout <- newIORef ""
  let host =
        productionRuntimeHost
          { runtimeHostWriteStdout = \contents -> do
              modifyIORef' capturedStdout (<> contents)
              pure (Right ())
          }
      source = "__kernel_exit! 7. __kernel_writeStdoutRaw! \"after exit\"."
  outputResult <-
    try
      ( runCliWithHostAndProfileWriter
          (capturingProfileWriter capturedProfile)
          host
          [ "--run",
            "--no-prelude",
            "--runtime-stats=json",
            "--runtime-profile=exit.speedscope.json"
          ]
          noEnvironment
          (const (pure Nothing))
          (pure source)
      ) :: IO (Either ExitCode CliOutput)
  output <-
    case outputResult of
      Left exitCode ->
        failTest
          ( "production exit escaped before observation artifacts were finalized: "
              <> Text.pack (show exitCode)
          )
      Right completedOutput -> pure completedOutput
  stdoutWrittenAfterExit <- readIORef capturedStdout
  assertEqual "requested process exit status" 7 (cliExitCode output)
  assertEqual "exit suppresses terminal-expression output" "" (cliStdout output)
  assertEqual "statements after exit are not executed" "" stdoutWrittenAfterExit
  assertFinalStderrJson "exit runtime statistics" output
  captured <- readIORef capturedProfile
  case captured of
    Nothing -> failTest "observed exit did not write a semantic profile"
    Just (path, bytes) -> do
      assertEqual "exit semantic profile path" "exit.speedscope.json" path
      assertJsonBytes "exit semantic profile JSON" bytes

testCliModuleGraphRuntimeObservation :: IO ()
testCliModuleGraphRuntimeObservation = do
  capturedProfile <- newIORef Nothing
  let fixtureRoot = "test/fixtures/runtime-observation/module-success"
      fileLookup path = do
        exists <- doesFileExist path
        if exists then Just <$> TextIO.readFile path else pure Nothing
  output <-
    runCliWithHostAndProfileWriter
      (capturingProfileWriter capturedProfile)
      disabledRuntimeHost
      [ "--run",
        "--runtime-stats=json",
        "--runtime-profile=module.speedscope.json",
        "--entry-module",
        "App::Main",
        "--module-root",
        fixtureRoot <> "/src"
      ]
      noEnvironment
      fileLookup
      (pure "")
  assertEqual "module observation exit" 0 (cliExitCode output)
  assertEqual "module observation stdout" "42\n" (cliStdout output)
  assertFinalStderrJson "module runtime statistics" output
  captured <- readIORef capturedProfile
  case captured of
    Nothing -> failTest "module observation did not write a semantic profile"
    Just (_, bytes) -> assertJsonBytes "module semantic profile JSON" bytes

testCliCompileFailureHasNoRuntimeArtifacts :: IO ()
testCliCompileFailureHasNoRuntimeArtifacts = do
  profileWritten <- newIORef False
  output <-
    runObservationFixtureWithWriter
      (recordingProfileWriter profileWritten)
      ["--runtime-stats=json", "--runtime-profile=compile-failure.json"]
      compileFailureFixture
  didWriteProfile <- readIORef profileWritten
  assertEqual "compile failure exit" 1 (cliExitCode output)
  assertContains "compile failure diagnostic" "error:" (cliStderr output)
  assertEqual "compile failure has no statistics" False ("\"schemaVersion\"" `Text.isInfixOf` cliStderr output)
  assertEqual "compile failure has no profile" False didWriteProfile

testCliRuntimeFailureHasPartialArtifacts :: IO ()
testCliRuntimeFailureHasPartialArtifacts = do
  capturedProfile <- newIORef Nothing
  output <-
    runObservationFixtureWithWriter
      (capturingProfileWriter capturedProfile)
      ["--runtime-stats=json", "--runtime-profile=runtime-failure.json"]
      runtimeFailureFixture
  assertEqual "runtime failure exit" 1 (cliExitCode output)
  assertContains "runtime failure diagnostic" "E3001" (cliStderr output)
  assertFinalStderrJson "partial runtime statistics" output
  captured <- readIORef capturedProfile
  case captured of
    Nothing -> failTest "runtime failure did not write its partial semantic profile"
    Just (_, bytes) -> do
      assertJsonBytes "partial semantic profile JSON" bytes
      assertEqual
        "partial profile is marked incomplete"
        True
        ("incomplete: failed" `Text.isInfixOf` TextEncoding.decodeUtf8 (LazyByteString.toStrict bytes))

testCliProfileWriteFailure :: IO ()
testCliProfileWriteFailure = do
  output <-
    runObservationFixtureWithWriter
      (\_ _ -> pure (Left (mkMessageDiagnostic "runtime profile writer deliberately failed")))
      ["--runtime-profile=unwritable/profile.json"]
      literalSuccessFixture
  assertEqual "profile write failure exit" 1 (cliExitCode output)
  assertEqual "profile write failure preserves program stdout" "42\n" (cliStdout output)
  assertContains "structured profile write diagnostic" "error: runtime profile writer deliberately failed" (cliStderr output)

testCliAtomicProfileReplacement :: IO ()
testCliAtomicProfileReplacement =
  withTemporaryPath $ \path -> do
    TextIO.writeFile path "existing profile"
    output <-
      runObservationFixture
        ["--runtime-profile=" <> path]
        literalSuccessFixture
    bytes <- LazyByteString.readFile path
    assertEqual "atomic profile replacement exit" 0 (cliExitCode output)
    assertJsonBytes "atomically replaced semantic profile" bytes

testCliAtomicProfileFailureCleanup :: IO ()
testCliAtomicProfileFailureCleanup =
  withTemporaryDirectory $ \directoryPath -> do
    let destinationPath = directoryPath </> "profile.json"
    createDirectory destinationPath
    output <-
      runObservationFixture
        ["--runtime-profile=" <> destinationPath]
        literalSuccessFixture
    destinationPreserved <- doesDirectoryExist destinationPath
    remainingEntries <- listDirectory directoryPath
    assertEqual "atomic profile failure exit" 1 (cliExitCode output)
    assertContains "atomic profile failure diagnostic" "runtime profile could not be written" (cliStderr output)
    assertEqual "existing destination is preserved" True destinationPreserved
    assertEqual "temporary profile is cleaned" ["profile.json"] remainingEntries

testCliPrecedenceBehavior :: IO ()
testCliPrecedenceBehavior = do
  let envMap =
        Map.fromList
          [("JAZZ_WARNING_FLAGS", "-same-scope-rebinding")]
      configMap = Map.fromList [(".jazz-warnings", "same-scope-rebinding")]
      envLookup key = pure (Map.lookup key envMap)
      configLookup key = pure (Map.lookup key configMap)
  output <- runCliWith ["-Wsame-scope-rebinding"] envLookup configLookup (pure sampleSource)
  assertEqual "exit code" 0 (cliExitCode output)
  assertContains "CLI precedence keeps warning enabled" "W0001" (cliStderr output)

testCliConfigPathOverride :: IO ()
testCliConfigPathOverride = do
  let configMap =
        Map.fromList
          [ (".jazz-warnings", ""),
            ("config/warnings.txt", "same-scope-rebinding")
          ]
      envLookup _ = pure Nothing
      configLookup key = pure (Map.lookup key configMap)
  output <- runCliWith ["--warnings-config", "config/warnings.txt"] envLookup configLookup (pure sampleSource)
  assertEqual "exit code" 0 (cliExitCode output)
  assertContains "custom config enables warning" "W0001" (cliStderr output)

testCliExplicitConfigPathFailure :: IO ()
testCliExplicitConfigPathFailure = do
  output <- runCliWith ["--warnings-config", "missing/warnings.txt"] envLookup configLookup (pure sampleSource)
  assertEqual "exit code" 2 (cliExitCode output)
  assertContains "stderr reports config read failure" "warning config file could not be read at 'missing/warnings.txt'" (cliStderr output)
  assertEqual "stdout is suppressed" "" (cliStdout output)
  where
    envLookup _ = pure Nothing
    configLookup _ = pure Nothing

testCliExplicitEnvConfigPathFailure :: IO ()
testCliExplicitEnvConfigPathFailure = do
  output <- runCliWith [] envLookup configLookup (pure sampleSource)
  assertEqual "exit code" 2 (cliExitCode output)
  assertContains "stderr reports env config read failure" "warning config file could not be read at 'env/warnings.txt'" (cliStderr output)
  assertEqual "stdout is suppressed" "" (cliStdout output)
  where
    envLookup key = pure (Map.lookup key (Map.fromList [("JAZZ_WARNING_CONFIG", "env/warnings.txt")]))
    configLookup _ = pure Nothing

testCliDefersSourceReadOnArgError :: IO ()
testCliDefersSourceReadOnArgError = do
  sourceRead <- newIORef False
  output <- runCliWith ["--bad-arg"] envLookup configLookup (recordSourceRead sourceRead)
  didRead <- readIORef sourceRead
  assertEqual "exit code" 2 (cliExitCode output)
  assertContains "stderr parse error prefix" "error: unknown argument" (cliStderr output)
  assertEqual "source should not be read when arg parse fails" False didRead
  where
    envLookup _ = pure Nothing
    configLookup _ = pure Nothing

testCliRejectsSourcePathWithEntryModuleBeforeRead :: IO ()
testCliRejectsSourcePathWithEntryModuleBeforeRead = do
  sourceRead <- newIORef False
  output <- runCliWith ["--entry-module", "App::Main", "first.jz"] envLookup configLookup (recordSourceRead sourceRead)
  didRead <- readIORef sourceRead
  assertEqual "exit code" 2 (cliExitCode output)
  assertContains "source plus entry diagnostic" "cannot combine source file with --entry-module" (cliStderr output)
  assertEqual "stdout is suppressed" "" (cliStdout output)
  assertEqual "source should not be read when source selection is invalid" False didRead
  where
    envLookup _ = pure Nothing
    configLookup _ = pure Nothing

testCliRejectsExplicitStdinWithEntryModuleBeforeRead :: IO ()
testCliRejectsExplicitStdinWithEntryModuleBeforeRead = do
  sourceRead <- newIORef False
  output <- runCliWith ["--entry-module", "App::Main", "-"] envLookup configLookup (recordSourceRead sourceRead)
  didRead <- readIORef sourceRead
  assertEqual "exit code" 2 (cliExitCode output)
  assertContains "stdin plus entry diagnostic" "cannot combine source file with --entry-module" (cliStderr output)
  assertEqual "stdout is suppressed" "" (cliStdout output)
  assertEqual "source should not be read when source selection is invalid" False didRead
  where
    envLookup _ = pure Nothing
    configLookup _ = pure Nothing

testCliRejectsModuleRootWithoutEntryBeforeRead :: IO ()
testCliRejectsModuleRootWithoutEntryBeforeRead = do
  sourceRead <- newIORef False
  output <- runCliWith ["--module-root", "src"] envLookup configLookup (recordSourceRead sourceRead)
  didRead <- readIORef sourceRead
  assertEqual "exit code" 2 (cliExitCode output)
  assertContains "module root without entry diagnostic" "cannot use --module-root without --entry-module" (cliStderr output)
  assertEqual "stdout is suppressed" "" (cliStdout output)
  assertEqual "source should not be read when module roots are invalid" False didRead
  where
    envLookup _ = pure Nothing
    configLookup _ = pure Nothing

testCliRejectsNestedModuleDeclarationInSourceInput :: IO ()
testCliRejectsNestedModuleDeclarationInSourceInput = do
  output <- runCliWith [] envLookup configLookup (pure nestedModuleInModuleBodySource)
  assertEqual "exit code" 1 (cliExitCode output)
  assertContains "stderr includes parser error" "module declaration must remain top-level" (cliStderr output)
  assertEqual "stdout is suppressed" "" (cliStdout output)
  where
    envLookup _ = pure Nothing
    configLookup _ = pure Nothing

testCliAcceptsConcreteListSignature :: IO ()
testCliAcceptsConcreteListSignature = do
  output <- runCliWith [] envLookup configLookup (pure concreteListSignatureSource)
  assertEqual "exit code" 0 (cliExitCode output)
  assertEqual "stdout stays empty for compile-only success" "" (cliStdout output)
  assertEqual "stderr is empty" "" (cliStderr output)
  where
    envLookup _ = pure Nothing
    configLookup _ = pure Nothing

testCliAcceptsSimpleFunctionSignature :: IO ()
testCliAcceptsSimpleFunctionSignature = do
  output <- runCliWith [] envLookup configLookup (pure simpleFunctionSignatureSource)
  assertEqual "exit code" 0 (cliExitCode output)
  assertEqual "stdout stays empty for compile-only success" "" (cliStdout output)
  assertEqual "stderr is empty" "" (cliStderr output)
  where
    envLookup _ = pure Nothing
    configLookup _ = pure Nothing

requireParsedOptions :: [String] -> IO CliOptions
requireParsedOptions arguments =
  case parseCliOptions arguments of
    Left diagnostic -> failTest ("parseCliOptions failed: " <> renderDiagnostic diagnostic)
    Right options -> pure options

assertParseErrorContains :: Text -> Text -> [String] -> IO ()
assertParseErrorContains label expected arguments =
  case parseCliOptions arguments of
    Left diagnostic -> assertContains label expected (renderDiagnostic diagnostic)
    Right options -> failTest (label <> ": expected parse failure, got " <> Text.pack (show options))

runObservationFixture :: [String] -> FilePath -> IO CliOutput
runObservationFixture arguments fixturePath =
  runCliWith
    (["--run"] <> arguments <> [fixturePath])
    noEnvironment
    (fixtureLookup fixturePath)
    (pure "")

runObservationFixtureWithWriter :: RuntimeProfileWriter -> [String] -> FilePath -> IO CliOutput
runObservationFixtureWithWriter profileWriter arguments fixturePath =
  runCliWithHostAndProfileWriter
    profileWriter
    disabledRuntimeHost
    (["--run"] <> arguments <> [fixturePath])
    noEnvironment
    (fixtureLookup fixturePath)
    (pure "")

fixtureLookup :: FilePath -> FilePath -> IO (Maybe Text)
fixtureLookup expectedPath requestedPath
  | requestedPath == expectedPath = Just <$> TextIO.readFile requestedPath
  | otherwise = pure Nothing

noEnvironment :: String -> IO (Maybe String)
noEnvironment _ = pure Nothing

capturingProfileWriter :: IORef (Maybe (FilePath, LazyByteString.ByteString)) -> RuntimeProfileWriter
capturingProfileWriter captured path bytes = do
  writeIORef captured (Just (path, bytes))
  pure (Right ())

recordingProfileWriter :: IORef Bool -> RuntimeProfileWriter
recordingProfileWriter profileWritten _ _ = do
  writeIORef profileWritten True
  pure (Right ())

assertFinalStderrJson :: Text -> CliOutput -> IO ()
assertFinalStderrJson label output =
  case reverse (Text.lines (cliStderr output)) of
    [] -> failTest (label <> ": expected a final JSON line")
    finalLine : _ ->
      assertJsonBytes
        label
        (LazyByteString.fromStrict (TextEncoding.encodeUtf8 finalLine))

assertJsonBytes :: Text -> LazyByteString.ByteString -> IO ()
assertJsonBytes label bytes =
  case eitherDecode bytes :: Either String Value of
    Left message -> failTest (label <> ": invalid JSON: " <> Text.pack message)
    Right _ -> pure ()

recordSourceRead :: IORef Bool -> IO Text
recordSourceRead sourceRead =
  recordSourceReadWith sourceRead sampleSource

recordSourceReadWith :: IORef Bool -> Text -> IO Text
recordSourceReadWith sourceRead source = do
  writeIORef sourceRead True
  pure source

recordLookupPath :: IORef [FilePath] -> (FilePath -> IO (Maybe Text)) -> FilePath -> IO (Maybe Text)
recordLookupPath paths lookupPath path = do
  writeIORef paths . (path :) =<< readIORef paths
  lookupPath path

recordConfigRead :: IORef Bool -> FilePath -> IO (Maybe Text)
recordConfigRead configRead _ = do
  writeIORef configRead True
  pure Nothing

assertHelpOutput :: Text -> CliOutput -> IO ()
assertHelpOutput label output = do
  assertEqual (label <> " exit code") 0 (cliExitCode output)
  assertEqual (label <> " stdout") expectedHelpOutput (cliStdout output)
  assertEqual (label <> " stderr") "" (cliStderr output)

expectedHelpOutput :: Text
expectedHelpOutput =
  Text.unlines
    [ "Usage: jazz-next [--run] [options] [source.jz]",
      "       jazz-next [--run] --entry-module Module::Path [--module-root DIR...] [options]",
      "",
      "Modes:",
      "  compile                         Parse/analyze source; success prints no stdout.",
      "  --run                           Execute source and print the final runtime value.",
      "  --runtime-stats[=human|json]    Report deterministic Jazz runtime statistics.",
      "  --runtime-profile=PATH          Write a deterministic Speedscope profile.",
      "",
      "Source:",
      "  source.jz                       Read one source file instead of stdin.",
      "  --entry-module Module::Path      Load a module graph entrypoint.",
      "  --module-root DIR                Add a module graph search root.",
      "",
      "Prelude and warnings:",
      "  --prelude PATH                   Use an explicit Prelude source.",
      "  --no-prelude                     Disable the bundled Prelude.",
      "  --warnings-config PATH           Read warning settings from PATH.",
      "  -W<category>                     Enable a warning category.",
      "  -Werror=<category>               Promote a warning category to an error.",
      "",
      "Help:",
      "  --help, -h                       Show this help text."
    ]

testCliReportsSignatureTypeMismatch :: IO ()
testCliReportsSignatureTypeMismatch = do
  output <- runCliWith [] envLookup configLookup (pure signatureMismatchSource)
  assertEqual "exit code" 1 (cliExitCode output)
  assertContains "stderr includes signature mismatch code" "E2005" (cliStderr output)
  assertContains "stderr includes signature mismatch primary span" "E2005: 1:1:" (cliStderr output)
  assertContains "stderr includes signature mismatch related span" "related 2:1" (cliStderr output)
  assertEqual "stdout is suppressed" "" (cliStdout output)
  where
    envLookup _ = pure Nothing
    configLookup _ = pure Nothing

sampleSource :: Text
sampleSource = "x = 1. x = 2."

literalSuccessFixture :: FilePath
literalSuccessFixture = "test/fixtures/runtime-observation/literal-success.jz"

compileFailureFixture :: FilePath
compileFailureFixture = "test/fixtures/runtime-observation/compile-failure.jz"

runtimeFailureFixture :: FilePath
runtimeFailureFixture = "test/fixtures/runtime-observation/runtime-failure.jz"

firstProgramSource :: Text
firstProgramSource = """
answer = 40 + 2.
answer.
"""

nestedModuleInModuleBodySource :: Text
nestedModuleInModuleBodySource = """
module App::Main {
module Inner::Thing {
x = 1.
}
}
"""

concreteListSignatureSource :: Text
concreteListSignatureSource = """
xs :: [Int].
xs = [1, 2].
"""

simpleFunctionSignatureSource :: Text
simpleFunctionSignatureSource = """
inc :: Int -> Int.
inc = (+ 1).
"""

signatureMismatchSource :: Text
signatureMismatchSource = """
x :: Int.
x = True.
"""

signatureNameMismatchSource :: Text
signatureNameMismatchSource = """
x :: Int.
y = 1.
"""

runtimeSuccessSource :: Text
runtimeSuccessSource = "if True then 1 else 2."

runtimeSectionSource :: Text
runtimeSectionSource = "(+ 1) 2."

runtimeDivisionByZeroSource :: Text
runtimeDivisionByZeroSource = "1 / 0."

runtimeListPrimitiveSource :: Text
runtimeListPrimitiveSource = "map hd [[1, 2], [3], [4, 5]]."

runtimeFilterPrimitiveSource :: Text
runtimeFilterPrimitiveSource = "filter (> 1) [1, 2, 3, 1]."

runtimeHdEmptySource :: Text
runtimeHdEmptySource = "hd []."

preludeSource :: Text
preludeSource = "inc = (+ 1)."

bundledPreludeConsumerSource :: Text
bundledPreludeConsumerSource = "map."

bundledPreludeKernelConsumerSource :: Text
bundledPreludeKernelConsumerSource = "__kernel_map."

preludeConsumerSource :: Text
preludeConsumerSource = "inc 2."

isBundledPreludePath :: FilePath -> Bool
isBundledPreludePath path =
  path == "jazz-next/jazz/stdlib/Prelude.jz"
    || path == "jazz/stdlib/Prelude.jz"
