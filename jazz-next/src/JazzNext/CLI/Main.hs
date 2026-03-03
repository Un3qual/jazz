{-# LANGUAGE OverloadedStrings #-}

module JazzNext.CLI.Main
  ( CliOptions (..),
    CliOutput (..),
    parseCliOptions,
    runCliWith,
    main
  ) where

import Control.Exception (IOException, evaluate, try)
import Data.List (isPrefixOf)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import JazzNext.Compiler.Diagnostics
  ( SourceSpan (..),
    WarningRecord (..),
    renderSourceSpan
  )
import JazzNext.Compiler.Driver
  ( CompileResult (..),
    RunResult (..),
    compileSource,
    runSource
  )
import JazzNext.Compiler.WarningConfig
  ( WarningSettings,
    resolveWarningSettings
  )
import JazzNext.Compiler.Warnings
  ( warningToken
  )
import System.Environment (getArgs, lookupEnv)
import System.Exit (ExitCode (..), exitWith)
import System.IO (stderr, stdout)

data CliOptions = CliOptions
  { cliWarningFlags :: [Text],
    cliWarningsConfigPath :: Maybe FilePath,
    cliRunMode :: Bool
  }
  deriving (Eq, Show)

data CliOutput = CliOutput
  { cliExitCode :: Int,
    cliStdout :: Text,
    cliStderr :: Text
  }
  deriving (Eq, Show)

-- Parse only the currently supported warning-related flags.
parseCliOptions :: [String] -> Either Text CliOptions
parseCliOptions args = finalize <$> go (CliOptions [] Nothing False) args
  where
    finalize options =
      options {cliWarningFlags = reverse (cliWarningFlags options)}
    go options [] = Right options
    go options ("--warnings-config" : path : rest) =
      go options {cliWarningsConfigPath = Just path} rest
    go _ ("--warnings-config" : []) =
      Left "missing path after --warnings-config"
    go options ("--run" : rest) =
      go options {cliRunMode = True} rest
    go options (arg : rest)
      | "-W" `isPrefixOf` arg =
          go options {cliWarningFlags = Text.pack arg : cliWarningFlags options} rest
      | otherwise = Left ("unknown argument: " <> Text.pack arg)

runCliWith ::
  [String] ->
  (String -> IO (Maybe String)) ->
  (FilePath -> IO (Maybe Text)) ->
  IO Text ->
  IO CliOutput
runCliWith args envLookup configLookup loadSource =
  case parseCliOptions args of
    Left parseError ->
      pure
        CliOutput
          { cliExitCode = 2,
            cliStdout = "",
            cliStderr = "error: " <> parseError <> "\n"
          }
    Right options -> do
      settingsResult <- resolveSettings options envLookup configLookup
      case settingsResult of
        Left configError ->
          pure
            CliOutput
              { cliExitCode = 2,
                cliStdout = "",
                cliStderr = "error: " <> configError <> "\n"
              }
        Right settings -> do
          source <- loadSource
          if cliRunMode options
            then runExecute settings source
            else runCompile settings source

main :: IO ()
main = do
  args <- getArgs
  output <- runCliWith args lookupEnv readConfigMaybe TextIO.getContents
  TextIO.hPutStr stdout (cliStdout output)
  TextIO.hPutStr stderr (cliStderr output)
  exitWith (toExitCode (cliExitCode output))

resolveSettings ::
  CliOptions ->
  (String -> IO (Maybe String)) ->
  (FilePath -> IO (Maybe Text)) ->
  IO (Either Text WarningSettings)
resolveSettings options envLookup configLookup = do
  envWarningFlags <- fmap Text.pack <$> envLookup "JAZZ_WARNING_FLAGS"
  envErrorFlags <- fmap Text.pack <$> envLookup "JAZZ_WARNING_ERROR_FLAGS"
  envConfigPath <- envLookup "JAZZ_WARNING_CONFIG"
  let selectedConfigPath =
        case cliWarningsConfigPath options of
          Just cliPath -> Just cliPath
          Nothing ->
            case envConfigPath of
              Just envPath -> Just envPath
              Nothing -> Just ".jazz-warnings"
  configContents <-
    case selectedConfigPath of
      Just configPath -> configLookup configPath
      Nothing -> pure Nothing
  pure (resolveWarningSettings (cliWarningFlags options) envWarningFlags envErrorFlags configContents)

runCompile :: WarningSettings -> Text -> IO CliOutput
runCompile settings source = do
  result <- compileSource settings source
  let warningLines = map formatWarningLine (compileWarnings result)
      errorLines = map ("error: " <>) (compileErrors result)
      stderrOutput = renderLines (warningLines ++ errorLines)
      stdoutOutput =
        case generatedJs result of
          Just js -> js <> "\n"
          Nothing -> ""
      exitCode =
        if null (compileErrors result)
          then 0
          else 1
  pure
    CliOutput
      { cliExitCode = exitCode,
        cliStdout = stdoutOutput,
        cliStderr = stderrOutput
      }

runExecute :: WarningSettings -> Text -> IO CliOutput
runExecute settings source = do
  result <- runSource settings source
  let warningLines = map formatWarningLine (runWarnings result)
      compileErrorLines = map ("error: " <>) (runCompileErrors result)
      runtimeErrorLines = map ("error: " <>) (runRuntimeErrors result)
      stderrOutput = renderLines (warningLines ++ compileErrorLines ++ runtimeErrorLines)
      stdoutOutput =
        case runOutput result of
          Just value -> value <> "\n"
          Nothing -> ""
      exitCode =
        if null (runCompileErrors result) && null (runRuntimeErrors result)
          then 0
          else 1
  pure
    CliOutput
      { cliExitCode = exitCode,
        cliStdout = stdoutOutput,
        cliStderr = stderrOutput
      }

formatWarningLine :: WarningRecord -> Text
formatWarningLine warning =
  warningCodeText warning
    <> " ["
    <> warningToken (warningCategory warning)
    <> "] "
    <> renderSourceSpan (warningPrimarySpan warning)
    <> ": "
    <> warningMessage warning
    <> renderPreviousSpan (warningPreviousSpan warning)

renderLines :: [Text] -> Text
renderLines [] = ""
renderLines linesOut = Text.unlines linesOut

renderPreviousSpan :: Maybe SourceSpan -> Text
renderPreviousSpan previous =
  case previous of
    Nothing -> ""
    Just previousSpan -> " (previous " <> renderSourceSpan previousSpan <> ")"

readConfigMaybe :: FilePath -> IO (Maybe Text)
readConfigMaybe path =
  -- Missing/unreadable config files are treated as absent so default warning
  -- behavior remains usable without setup.
  (eitherToMaybe <$> try readAndForce)
  where
    readAndForce :: IO Text
    readAndForce = do
      contents <- TextIO.readFile path
      _ <- evaluate (Text.length contents)
      pure contents

    eitherToMaybe :: Either IOException Text -> Maybe Text
    eitherToMaybe readResult =
      case readResult of
        Left _ -> Nothing
        Right contents -> Just contents

toExitCode :: Int -> ExitCode
toExitCode code =
  if code == 0
    then ExitSuccess
    else ExitFailure code
