{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Exception (finally)
import Data.IORef
  ( IORef,
    newIORef,
    readIORef,
    writeIORef
  )
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import JazzNext.CLI.Main
  ( CliOptions (..),
    CliOutput (..),
    parseCliOptions,
    runCliWith,
    runCliWithHost
  )
import JazzNext.Compiler.Diagnostics
  ( renderDiagnostic
  )
import JazzNext.Compiler.BundledPrelude
  ( bundledPreludeSource
  )
import JazzNext.Compiler.RuntimeHost
  ( productionRuntimeHost
  )
import JazzNext.TestHarness
  ( NamedTest,
    assertContains,
    assertEqual,
    failTest,
    runTestSuite
  )
import System.Directory
  ( getTemporaryDirectory,
    removeFile
  )
import System.IO
  ( hClose,
    openTempFile
  )

main :: IO ()
main = runTestSuite "CLISpec" tests

tests :: [NamedTest]
tests =
  [ ("parseCliOptions captures warning flags and config path", testParseOptions),
    ("parseCliOptions captures run mode", testParseRunMode),
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
                [ ("src/App/Main.jz", "module App::Main {\nimport Lib::Util.\nutil.\n}"),
                  ("src/Lib/Util.jz", "module Lib::Util {\nutil = 1.\n}")
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
                [ ("App/Main.jz", "module App::Main {\nimport Lib::Util.\nutil.\n}"),
                  ("Lib/Util.jz", "module Lib::Util {\nutil = 1.\n}")
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
                [ ("src/App/Main.jz", "module App::Main {\nimport Lib::Util.\nutil.\n}"),
                  ("src/Lib/Util.jz", "module Lib::Util {\nutil = 1.\n}")
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
    fileLookup key = pure (Map.lookup key (Map.fromList [("src/App/Main.jz", "import Missing::Thing.\n1.")]))

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
                [ ("src/App/Main.jz", "import Lib::Math (subtract).\n1."),
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
            (Map.fromList [("src/App/Main.jz", "module Wrong::Name {\n1.\n}")])
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

firstProgramSource :: Text
firstProgramSource = "answer = 40 + 2.\nanswer."

nestedModuleInModuleBodySource :: Text
nestedModuleInModuleBodySource = "module App::Main {\nmodule Inner::Thing {\nx = 1.\n}\n}"

concreteListSignatureSource :: Text
concreteListSignatureSource = "xs :: [Int].\nxs = [1, 2]."

simpleFunctionSignatureSource :: Text
simpleFunctionSignatureSource = "inc :: Int -> Int.\ninc = (+ 1)."

signatureMismatchSource :: Text
signatureMismatchSource = "x :: Int.\nx = True."

signatureNameMismatchSource :: Text
signatureNameMismatchSource = "x :: Int.\ny = 1."

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
  path == "jazz-next/stdlib/Prelude.jz" || path == "stdlib/Prelude.jz"
