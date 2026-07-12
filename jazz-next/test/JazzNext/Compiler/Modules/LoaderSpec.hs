{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.IORef
  ( IORef,
    modifyIORef',
    newIORef,
    readIORef
  )
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import JazzNext.Compiler.Driver
  ( RunResult (..),
    runModuleGraphWithPrelude,
    runModuleGraphWithPreludeAndHost
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
    hostIOFailureMessage
  )
import JazzNext.Compiler.Modules.Loader.BasicTests (basicTests)
import JazzNext.Compiler.Modules.Loader.VisibilityTests (visibilityTests)
import JazzNext.Compiler.Modules.Loader.CapabilitiesTests (capabilitiesTests)
import JazzNext.Compiler.Modules.Loader.OperatorsTests (operatorTests)
import JazzNext.Compiler.Modules.Loader.DiagnosticsTests (diagnosticTests)
import JazzNext.TestHarness (NamedTest, assertEqual, runTestSuite)
import System.Directory (doesFileExist)

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
    ( "checked-in IO modules transport successful host operations",
      testBootstrapIOSuccesses
    ),
    ( "checked-in IO modules decode every host error category",
      testBootstrapIOErrors
    )
  ]
    ++ basicTests
    ++ visibilityTests
    ++ capabilitiesTests
    ++ operatorTests
    ++ diagnosticTests

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
      "module App::Main {\n"
        <> "import Maybe.\n"
        <> "import Result.\n"
        <> "maybeValue :: Maybe(Int).\n"
        <> "maybeValue = Just 41.\n"
        <> "resultValue :: Result(Text, Int).\n"
        <> "resultValue = Ok 1.\n"
        <> "maybeNumber = case maybeValue { | Nothing -> 0 | Just value -> value }.\n"
        <> "resultNumber = case resultValue { | Err message -> 0 | Ok value -> value }.\n"
        <> "(maybeNumber, resultNumber).\n"
        <> "}"
    lookupSource "src/App/Main.jz" = pure (Just entrySource)
    lookupSource "src/Maybe.jz" = readStdlibSource "Maybe.jz"
    lookupSource "src/Result.jz" = readStdlibSource "Result.jz"
    lookupSource _ = pure Nothing

testBootstrapTextModule :: IO ()
testBootstrapTextModule = do
  result <-
    runModuleGraphWithPrelude
      defaultWarningSettings
      Nothing
      resolverConfig
      ["App", "Main"]
      lookupSource
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "(True, 3, True, '🙂', 'x', True)") (runOutput result)
  where
    entrySource =
      "module App::Main {\n"
        <> "import Text.\n"
        <> "import Maybe.\n"
        <> "case textUncons \"🙂x\" {\n"
        <> "| Nothing -> (textIsEmpty textEmpty, textLength \"a🙂é\", textUncons \"\" == Nothing, '?', '?', False)\n"
        <> "| Just (first, rest) -> case textUncons rest {\n"
        <> "| Nothing -> (textIsEmpty textEmpty, textLength \"a🙂é\", textUncons \"\" == Nothing, first, '?', False)\n"
        <> "| Just (second, tail) -> (textIsEmpty textEmpty, textLength \"a🙂é\", textUncons \"\" == Nothing, first, second, textIsEmpty tail)\n"
        <> "}\n"
        <> "}.\n"
        <> "}"
    lookupSource "src/App/Main.jz" = pure (Just entrySource)
    lookupSource "src/Maybe.jz" = readStdlibSource "Maybe.jz"
    lookupSource "src/Text.jz" = readStdlibSource "Text.jz"
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
      "module App::Main {\n"
        <> "import IO.\n"
        <> "read! = readText! \"source.jz\".\n"
        <> "write! = writeText! \"output.txt\" \"Jazz\".\n"
        <> "stdin! = readStdin! ().\n"
        <> "stdout! = writeStdout! \"out\".\n"
        <> "stderr! = writeStderr! \"err\".\n"
        <> "(read!, write!, stdin!, stdout!, stderr!, arguments! (), exit! 7).\n"
        <> "}"
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
      "module App::Main {\n"
        <> "import IO.\n"
        <> "(readText! \"not-found\", readText! \"permission-denied\", readText! \"already-exists\", readText! \"invalid-data\", readText! \"resource-exhausted\", readText! \"interrupted\", readText! \"unsupported\", readText! \"other\", readStdin! ()).\n"
        <> "}"
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
      runtimeHostExit = \status -> record ("exit:" <> Text.pack (show status)) (Right ())
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
      runtimeHostExit = \_ -> pure (Right ())
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
  readFirstExisting
    [ "stdlib/" <> fileName,
      "jazz-next/stdlib/" <> fileName
    ]

readFirstExisting :: [FilePath] -> IO (Maybe Text)
readFirstExisting [] = pure Nothing
readFirstExisting (candidate : rest) = do
  exists <- doesFileExist candidate
  if exists
    then Just <$> TextIO.readFile candidate
    else readFirstExisting rest
