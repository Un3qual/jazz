{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.IORef
  ( IORef,
    newIORef,
    readIORef,
    writeIORef
  )
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Text (Text)
import JazzNext.CLI.Main
  ( CliOptions (..),
    CliOutput (..),
    parseCliOptions,
    runCliWith
  )
import JazzNext.Compiler.Diagnostics
  ( renderDiagnostic
  )
import JazzNext.Compiler.BundledPrelude
  ( bundledPreludeSource
  )
import JazzNext.TestHarness
  ( NamedTest,
    assertContains,
    assertEqual,
    failTest,
    runTestSuite
  )

main :: IO ()
main = runTestSuite "CLISpec" tests

tests :: [NamedTest]
tests =
  [ ("parseCliOptions captures warning flags and config path", testParseOptions),
    ("parseCliOptions captures run mode", testParseRunMode),
    ("parseCliOptions captures entry module and module roots", testParseModuleGraphOptions),
    ("parseCliOptions captures prelude path", testParsePreludePath),
    ("parseCliOptions captures no-prelude switch", testParseNoPrelude),
    ("parseCliOptions rejects conflicting prelude switches", testParsePreludeConflict),
    ("cli run prints warning to stderr while keeping stdout output", testCliWarningOnlyBehavior),
    ("cli run returns non-zero and suppresses stdout when warning promoted", testCliPromotedWarningBehavior),
    ("cli --run prints evaluated runtime output", testCliRunModeSuccess),
    ("cli --run prints evaluated section runtime output", testCliRunModeSectionSuccess),
    ("cli --run prints evaluated list primitive output", testCliRunModeListPrimitiveSuccess),
    ("cli --run prints evaluated filter primitive output", testCliRunModeFilterPrimitiveSuccess),
    ("cli --run with entry module loads module graph and ignores stdin", testCliRunModeModuleGraphSuccess),
    ("cli module graph compile reports resolver diagnostics", testCliModuleGraphCompileError),
    ("cli module graph compile reports missing import symbol diagnostics", testCliModuleGraphMissingImportSymbol),
    ("cli module graph compile reports module declaration mismatch diagnostics", testCliModuleGraphDeclarationMismatch),
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
  assertContains "stdout includes generated output" "codegen placeholder" (cliStdout output)
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
                [ ("src/App/Main.jz", "import Lib::Util.\nutil."),
                  ("src/Lib/Util.jz", "util = 1.")
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
            (Map.fromList [("src/App/Main.jz", "module Wrong::Name.\n1.")])
        )

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

testCliAcceptsConcreteListSignature :: IO ()
testCliAcceptsConcreteListSignature = do
  output <- runCliWith [] envLookup configLookup (pure concreteListSignatureSource)
  assertEqual "exit code" 0 (cliExitCode output)
  assertContains "stdout includes generated output" "codegen placeholder" (cliStdout output)
  assertEqual "stderr is empty" "" (cliStderr output)
  where
    envLookup _ = pure Nothing
    configLookup _ = pure Nothing

testCliAcceptsSimpleFunctionSignature :: IO ()
testCliAcceptsSimpleFunctionSignature = do
  output <- runCliWith [] envLookup configLookup (pure simpleFunctionSignatureSource)
  assertEqual "exit code" 0 (cliExitCode output)
  assertContains "stdout includes generated output" "codegen placeholder" (cliStdout output)
  assertEqual "stderr is empty" "" (cliStderr output)
  where
    envLookup _ = pure Nothing
    configLookup _ = pure Nothing

recordSourceRead :: IORef Bool -> IO Text
recordSourceRead sourceRead = do
  writeIORef sourceRead True
  pure sampleSource

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

concreteListSignatureSource :: Text
concreteListSignatureSource = "xs :: [Int].\nxs = [1, 2]."

simpleFunctionSignatureSource :: Text
simpleFunctionSignatureSource = "inc :: Int -> Int.\ninc = (+ 1)."

signatureMismatchSource :: Text
signatureMismatchSource = "x :: Int.\nx = True."

signatureNameMismatchSource :: Text
signatureNameMismatchSource = "x :: Int.\ny = 1."

runtimeSuccessSource :: Text
runtimeSuccessSource = "if True 1 else 2."

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
