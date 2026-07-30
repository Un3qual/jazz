{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Exception
  ( SomeException,
    try
  )
import Data.IORef
  ( IORef,
    modifyIORef',
    newIORef,
    readIORef
  )
import Data.Text (Text)
import qualified Data.Text as Text
import JazzNext.Compiler.Driver
  ( RunResult (..),
    runCompileErrors,
    runModuleGraph,
    runModuleGraphWithPrelude,
    runModuleGraphWithPreludeAndHost,
    runRuntimeErrors
  )
import JazzNext.Compiler.Modules.Loader.Shared
  ( resolverConfig
  )
import JazzNext.Compiler.WarningConfig
  ( defaultWarningSettings
  )
import JazzNext.Compiler.RuntimeHost
  ( HostIOCategory (..),
    HostIOFailure (..),
    RuntimeHost (..),
    RuntimeHostExit (..),
    hostIOFailureMessage
  )
import JazzNext.Compiler.Modules.Loader.BasicTests (basicTests)
import JazzNext.Compiler.Modules.Loader.VisibilityTests (visibilityTests)
import JazzNext.Compiler.Modules.Loader.CapabilitiesTests (capabilitiesTests)
import JazzNext.Compiler.Modules.Loader.OperatorsTests (operatorTests)
import JazzNext.Compiler.Modules.Loader.DiagnosticsTests (diagnosticTests)
import JazzNext.TestHarness (NamedTest, assertEqual, failTest, runTestSuite)
import JazzNext.TestSource
  ( JazzSourceRole (StandardLibrarySource),
    readCheckedInJazzSource,
    readCheckedInJazzProjectModuleSource,
  )
import System.Timeout (timeout)

main :: IO ()
main = runTestSuite "Loader" tests

tests :: [NamedTest]
tests =
  [ ( "checked-in Maybe and Result modules transport generic ADTs through the loader",
      testBootstrapMaybeAndResultModules
    ),
    ( "checked-in Text module traverses Unicode through Maybe",
      testBootstrapTextModule
    ),
    ( "checked-in List Char and Text modules expose bootstrap construction APIs",
      testBootstrapCollectionScalarModules
    ),
    ( "checked-in List reverse preserves concrete hints for non-empty and empty lists",
      testBootstrapListReversePreservesConcreteHints
    ),
    ( "checked-in IO modules transport successful host operations",
      testBootstrapIOSuccesses
    ),
    ( "checked-in IO modules decode every host error category",
      testBootstrapIOErrors
    ),
    ( "imported tail-recursive closures are stack safe at bootstrap depth",
      testImportedTailRecursiveClosureIsStackSafe
    )
  ]
    ++ basicTests
    ++ visibilityTests
    ++ capabilitiesTests
    ++ operatorTests
    ++ diagnosticTests

testImportedTailRecursiveClosureIsStackSafe :: IO ()
testImportedTailRecursiveClosureIsStackSafe = do
  maybeResult <-
    timeout
      30000000
      ( try
          ( runModuleGraphWithPrelude
              defaultWarningSettings
              Nothing
              resolverConfig
              ["App", "Main"]
              lookupSource
          )
          :: IO (Either SomeException RunResult)
      )
  case maybeResult of
    Nothing -> failTest "20,000-call imported tail recursion timed out"
    Just (Left err) ->
      failTest ("imported tail recursion leaked host exception: " <> Text.pack (show err))
    Just (Right result) -> do
      assertEqual "imported tail compile errors" [] (runCompileErrors result)
      assertEqual "imported tail runtime errors" [] (runRuntimeErrors result)
      assertEqual "imported tail output" (Just "0") (runOutput result)
  where
    counterSource =
      """
      module Library::Counter (countDown) {
      countDown = \\(remaining) -> case remaining {
      | 0 -> 0
      | _ -> countDown (remaining - 1)
      }.
      }
      """
    entrySource =
      """
      module App::Main {
      import Library::Counter.
      countDown 20000.
      }
      """
    lookupSource "src/Library/Counter.jz" = pure (Just counterSource)
    lookupSource "src/App/Main.jz" = pure (Just entrySource)
    lookupSource _ = pure Nothing

testBootstrapMaybeAndResultModules :: IO ()
testBootstrapMaybeAndResultModules = do
  result <-
    runModuleGraphWithPrelude
      defaultWarningSettings
      Nothing
      resolverConfig
      ["App", "Main"]
      lookupSource
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "(41, 1)") (runOutput result)
  where
    entrySource =
      """
      module App::Main {
      import Maybe.
      import Result.
      maybeValue :: Maybe(Int).
      maybeValue = Just 41.
      resultValue :: Result(Text, Int).
      resultValue = Ok 1.
      maybeNumber = case maybeValue { | Nothing -> 0 | Just item -> item }.
      resultNumber = case resultValue { | Err message -> 0 | Ok item -> item }.
      (maybeNumber, resultNumber).
      }
      """
    lookupSource "src/App/Main.jz" = pure (Just entrySource)
    lookupSource "src/Maybe.jz" = readStdlibSource "Maybe.jz"
    lookupSource "src/Result.jz" = readStdlibSource "Result.jz"
    lookupSource _ = pure Nothing

testBootstrapTextModule :: IO ()
testBootstrapTextModule = do
  result <-
    runModuleGraph
      defaultWarningSettings
      resolverConfig
      ["App", "Main"]
      lookupSource
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "(True, 3, True, '🙂', 'x', True)") (runOutput result)
  where
    entrySource =
      """
      module App::Main {
      import Text.
      import Maybe.
      case textUncons \"🙂x\" {
      | Nothing -> (textIsEmpty textEmpty, textLength \"a🙂é\", textUncons \"\" == Nothing, '?', '?', False)
      | Just (first, rest) -> case textUncons rest {
      | Nothing -> (textIsEmpty textEmpty, textLength \"a🙂é\", textUncons \"\" == Nothing, first, '?', False)
      | Just (second, tail) -> (textIsEmpty textEmpty, textLength \"a🙂é\", textUncons \"\" == Nothing, first, second, textIsEmpty tail)
      }
      }.
      }
      """
    lookupSource "src/App/Main.jz" = pure (Just entrySource)
    lookupSource path = readCheckedInJazzProjectModuleSource path

testBootstrapCollectionScalarModules :: IO ()
testBootstrapCollectionScalarModules = do
  result <-
    runModuleGraph
      defaultWarningSettings
      resolverConfig
      ["App", "Main"]
      lookupSource
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual
    "runtime output"
    (Just "([\"second\", \"first\"], 2, Just('🙂'), Nothing, (True, True, True, True, True, True), \"Jazz\")")
    (runOutput result)
  where
    entrySource =
      """
      module App::Main {
      import List.
      import Char.
      import Text.
      items = listPrepend \"first\" [\"second\"].
      surrogate :: UInt32.
      surrogate = 55296.
      (listReverse items, listLength items, charFromUInt32 (charToUInt32 '🙂'), charFromUInt32 surrogate, (charIsAlpha 'é', charIsAlphaNum '9', charIsDigit '9', charIsSpace '\\t', charIsHexDigit 'F', charIsNewline '\\n'), textAppendChar (textAppend \"Ja\" \"z\") 'z').
      }
      """
    lookupSource "src/App/Main.jz" = pure (Just entrySource)
    lookupSource path = readCheckedInJazzProjectModuleSource path

testBootstrapListReversePreservesConcreteHints :: IO ()
testBootstrapListReversePreservesConcreteHints = do
  result <-
    runModuleGraph
      defaultWarningSettings
      resolverConfig
      ["App", "Main"]
      lookupSource
  assertEqual "list reverse hint compile errors" [] (runCompileErrors result)
  assertEqual "list reverse hint runtime errors" [] (runRuntimeErrors result)
  assertEqual "list reverse hint output" (Just "(False, False)") (runOutput result)
  where
    entrySource =
      """
      module App::Main {
      import List.
      class RuntimePick(a) { pick :: [a] -> Bool. }.
      impl RuntimePick(Bool) { pick = \\(values) -> True. }.
      impl RuntimePick(Int64) { pick = \\(values) -> False. }.
      values :: [Int64].
      values = [1].
      emptyValues :: [Int64].
      emptyValues = [].
      (RuntimePick::pick (listReverse values), RuntimePick::pick (listReverse emptyValues)).
      }
      """
    lookupSource "src/App/Main.jz" = pure (Just entrySource)
    lookupSource "src/List.jz" = readStdlibSource "List.jz"
    lookupSource "src/Maybe.jz" = readStdlibSource "Maybe.jz"
    lookupSource _ = pure Nothing

testBootstrapIOSuccesses :: IO ()
testBootstrapIOSuccesses = do
  callsRef <- newIORef []
  result <-
    runModuleGraphWithPreludeAndHost
      (successfulIOHost callsRef)
      defaultWarningSettings
      Nothing
      resolverConfig
      ["App", "Main"]
      lookupSource
  calls <- readIORef callsRef
  assertEqual "IO success compile errors" [] (runCompileErrors result)
  assertEqual "IO success runtime errors" [] (runRuntimeErrors result)
  assertEqual
    "IO success output"
    (Just "(Ok(\"file text\"), Ok(()), Ok(\"stdin text\"), Ok(()), Ok(()), [\"one\", \"two\"], ())")
    (runOutput result)
  assertEqual
    "IO success host order"
    [ "read:source.jz",
      "write:output.txt:Jazz",
      "stdin",
      "stdout:out",
      "stderr:err",
      "arguments",
      "exit:7"
    ]
    calls
  where
    entrySource =
      """
      module App::Main {
      import IO.
      read! = readText! \"source.jz\".
      write! = writeText! \"output.txt\" \"Jazz\".
      stdin! = readStdin! ().
      stdout! = writeStdout! \"out\".
      stderr! = writeStderr! \"err\".
      (read!, write!, stdin!, stdout!, stderr!, arguments! (), exit! 7).
      }
      """
    lookupSource = lookupIOSource entrySource

testBootstrapIOErrors :: IO ()
testBootstrapIOErrors = do
  result <-
    runModuleGraphWithPreludeAndHost
      failingIOHost
      defaultWarningSettings
      Nothing
      resolverConfig
      ["App", "Main"]
      lookupSource
  assertEqual "IO error compile errors" [] (runCompileErrors result)
  assertEqual "IO error runtime errors" [] (runRuntimeErrors result)
  assertEqual
    "IO error output"
    ( Just
        "(Err(IOError(NotFound, Just(\"not-found\"), \"resource not found\")), Err(IOError(PermissionDenied, Just(\"permission-denied\"), \"permission denied\")), Err(IOError(AlreadyExists, Just(\"already-exists\"), \"resource already exists\")), Err(IOError(InvalidData, Just(\"invalid-data\"), \"input is not valid UTF-8\")), Err(IOError(ResourceExhausted, Just(\"resource-exhausted\"), \"resource exhausted\")), Err(IOError(Interrupted, Just(\"interrupted\"), \"operation interrupted\")), Err(IOError(Unsupported, Just(\"unsupported\"), \"operation unsupported\")), Err(IOError(Other, Just(\"other\"), \"host I/O failed\")), Err(IOError(Other, Nothing, \"host I/O failed\")))"
    )
    (runOutput result)
  where
    entrySource =
      """
      module App::Main {
      import IO.
      (readText! \"not-found\", readText! \"permission-denied\", readText! \"already-exists\", readText! \"invalid-data\", readText! \"resource-exhausted\", readText! \"interrupted\", readText! \"unsupported\", readText! \"other\", readStdin! ()).
      }
      """
    lookupSource = lookupIOSource entrySource

lookupIOSource :: Text -> FilePath -> IO (Maybe Text)
lookupIOSource entrySource path =
  case path of
    "src/App/Main.jz" -> pure (Just entrySource)
    "src/IO.jz" -> readStdlibSource "IO.jz"
    "src/IOError.jz" -> readStdlibSource "IOError.jz"
    "src/Maybe.jz" -> readStdlibSource "Maybe.jz"
    "src/Result.jz" -> readStdlibSource "Result.jz"
    _ -> pure Nothing

successfulIOHost :: IORef [Text] -> RuntimeHost IO
successfulIOHost callsRef =
  RuntimeHost
    { runtimeHostReadText = \path -> record ("read:" <> path) (Right "file text"),
      runtimeHostWriteText = \path contents -> record ("write:" <> path <> ":" <> contents) (Right ()),
      runtimeHostReadStdin = record "stdin" (Right "stdin text"),
      runtimeHostWriteStdout = \contents -> record ("stdout:" <> contents) (Right ()),
      runtimeHostWriteStderr = \contents -> record ("stderr:" <> contents) (Right ()),
      runtimeHostArguments = record "arguments" ["one", "two"],
      runtimeHostExit = \status -> record ("exit:" <> Text.pack (show status)) (Right RuntimeHostExitReturned)
    }
  where
    record call value = do
      modifyIORef' callsRef (<> [call])
      pure value

failingIOHost :: RuntimeHost IO
failingIOHost =
  RuntimeHost
    { runtimeHostReadText = \path -> pure (Left (failureForToken path)),
      runtimeHostWriteText = \_ _ -> pure (Left (failure HostOther)),
      runtimeHostReadStdin = pure (Left (failure HostOther)),
      runtimeHostWriteStdout = \_ -> pure (Left (failure HostOther)),
      runtimeHostWriteStderr = \_ -> pure (Left (failure HostOther)),
      runtimeHostArguments = pure [],
      runtimeHostExit = \_ -> pure (Right RuntimeHostExitReturned)
    }
  where
    failureForToken token =
      failure
        ( case token of
            "not-found" -> HostNotFound
            "permission-denied" -> HostPermissionDenied
            "already-exists" -> HostAlreadyExists
            "invalid-data" -> HostInvalidData
            "resource-exhausted" -> HostResourceExhausted
            "interrupted" -> HostInterrupted
            "unsupported" -> HostUnsupported
            _ -> HostOther
        )
    failure category = HostIOFailure category (hostIOFailureMessage category)

readStdlibSource :: FilePath -> IO (Maybe Text)
readStdlibSource fileName =
  Just <$> readCheckedInJazzSource StandardLibrarySource fileName
