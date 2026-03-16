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
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import JazzNext.Compiler.Diagnostics
  ( Diagnostic,
    RenderDiagnostic (..),
    SourceSpan (..),
    WarningRecord (..),
    mkDiagnostic,
    mkMessageDiagnostic,
    renderDiagnostic,
    renderSourceSpan
  )
import JazzNext.Compiler.Driver
  ( CompileResult (..),
    RunResult (..),
    compileModuleGraphWithPrelude,
    compileSourceWithPrelude,
    runModuleGraphWithPrelude,
    runSourceWithPrelude
  )
import JazzNext.Compiler.ModuleResolver
  ( ModuleResolutionConfig (..),
    parseModulePathText
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
    cliRunMode :: Bool,
    cliPreludePath :: Maybe FilePath,
    cliDisablePrelude :: Bool,
    cliEntryModule :: Maybe [Text],
    cliModuleRoots :: [FilePath]
  }
  deriving (Eq, Show)

data CliOutput = CliOutput
  { cliExitCode :: Int,
    cliStdout :: Text,
    cliStderr :: Text
  }
  deriving (Eq, Show)

-- Parse currently supported warning and prelude-loading flags.
parseCliOptions :: [String] -> Either Diagnostic CliOptions
parseCliOptions args = do
  options <- go (CliOptions [] Nothing False Nothing False Nothing []) args
  finalize options
  where
    finalize options
      | cliDisablePrelude options && isJust (cliPreludePath options) =
          Left (mkMessageDiagnostic "cannot combine --prelude with --no-prelude")
      | null (cliModuleRoots options) =
          Right options {cliWarningFlags = reverse (cliWarningFlags options)}
      | isJust (cliEntryModule options) =
          Right
            options
              { cliWarningFlags = reverse (cliWarningFlags options),
                cliModuleRoots = reverse (cliModuleRoots options)
              }
      | otherwise =
          Left (mkMessageDiagnostic "cannot use --module-root without --entry-module")
    go options [] = Right options
    go options ("--warnings-config" : path : rest) =
      go options {cliWarningsConfigPath = Just path} rest
    go _ ("--warnings-config" : []) =
      Left (mkMessageDiagnostic "missing path after --warnings-config")
    go options ("--prelude" : path : rest) =
      go options {cliPreludePath = Just path} rest
    go _ ("--prelude" : []) =
      Left (mkMessageDiagnostic "missing path after --prelude")
    go options ("--no-prelude" : rest) =
      go options {cliDisablePrelude = True} rest
    go options ("--run" : rest) =
      go options {cliRunMode = True} rest
    go options ("--entry-module" : modulePathText : rest) =
      case parseModulePathText (Text.pack modulePathText) of
        Left err ->
          Left err
        Right modulePath ->
          go options {cliEntryModule = Just modulePath} rest
    go _ ("--entry-module" : []) =
      Left (mkMessageDiagnostic "missing module path after --entry-module")
    go options ("--module-root" : moduleRoot : rest) =
      go options {cliModuleRoots = moduleRoot : cliModuleRoots options} rest
    go _ ("--module-root" : []) =
      Left (mkMessageDiagnostic "missing path after --module-root")
    go options (arg : rest)
      | "-W" `isPrefixOf` arg =
          go options {cliWarningFlags = Text.pack arg : cliWarningFlags options} rest
      | otherwise = Left (mkMessageDiagnostic ("unknown argument: " <> Text.pack arg))

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
            cliStderr = "error: " <> renderDiagnostic parseError <> "\n"
          }
    Right options -> do
      settingsResult <- resolveSettings options envLookup configLookup
      case settingsResult of
        Left configError ->
          pure
            CliOutput
              { cliExitCode = 2,
                cliStdout = "",
                cliStderr = "error: " <> renderDiagnostic configError <> "\n"
              }
        Right settings -> do
          preludeSourceResult <- resolvePreludeSource options envLookup configLookup
          case preludeSourceResult of
            Left preludeError ->
              pure
                CliOutput
                  { cliExitCode = 2,
                    cliStdout = "",
                    cliStderr = "error: " <> renderDiagnostic preludeError <> "\n"
                  }
            Right preludeSource -> do
              case cliEntryModule options of
                Just entryModulePath ->
                  if cliRunMode options
                    then runExecuteModuleGraph settings options preludeSource entryModulePath configLookup
                    else runCompileModuleGraph settings options preludeSource entryModulePath configLookup
                Nothing -> do
                  source <- loadSource
                  if cliRunMode options
                    then runExecute settings preludeSource source
                    else runCompile settings preludeSource source

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
  IO (Either Diagnostic WarningSettings)
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

resolvePreludeSource ::
  CliOptions ->
  (String -> IO (Maybe String)) ->
  (FilePath -> IO (Maybe Text)) ->
  IO (Either Diagnostic (Maybe Text))
resolvePreludeSource options envLookup fileLookup = do
  envPreludePath <- envLookup "JAZZ_PRELUDE"
  if cliDisablePrelude options
    then pure (Right Nothing)
    else
      case cliPreludePath options of
        Just cliPath -> loadRequiredPrelude cliPath
        Nothing ->
          case envPreludePath of
            Just envPath -> loadRequiredPrelude envPath
            Nothing -> Right <$> loadFirstBundledPrelude fileLookup
  where
    loadRequiredPrelude :: FilePath -> IO (Either Diagnostic (Maybe Text))
    loadRequiredPrelude preludePath = do
      preludeContents <- fileLookup preludePath
      pure $
        case preludeContents of
          Just contents -> Right (Just contents)
          Nothing ->
            Left
              ( mkDiagnostic
                  "E0003"
                  ("prelude file could not be read at '" <> Text.pack preludePath <> "'")
              )

    loadFirstBundledPrelude ::
      (FilePath -> IO (Maybe Text)) ->
      IO (Maybe Text)
    loadFirstBundledPrelude lookupPath =
      go bundledPreludePaths
      where
        go candidates =
          case candidates of
            [] -> pure Nothing
            candidate : rest -> do
              maybeContents <- lookupPath candidate
              case maybeContents of
                Just contents -> pure (Just contents)
                Nothing -> go rest

bundledPreludePaths :: [FilePath]
bundledPreludePaths =
  [ "jazz-next/stdlib/Prelude.jz",
    "stdlib/Prelude.jz"
  ]

runCompile :: WarningSettings -> Maybe Text -> Text -> IO CliOutput
runCompile settings preludeSource source = do
  result <- compileSourceWithPrelude settings preludeSource source
  let warningLines = map formatWarningLine (compileWarnings result)
      errorLines = map (("error: " <>) . renderDiagnostic) (compileErrors result)
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

runExecute :: WarningSettings -> Maybe Text -> Text -> IO CliOutput
runExecute settings preludeSource source = do
  result <- runSourceWithPrelude settings preludeSource source
  let warningLines = map formatWarningLine (runWarnings result)
      compileErrorLines = map (("error: " <>) . renderDiagnostic) (runCompileErrors result)
      runtimeErrorLines = map (("error: " <>) . renderDiagnostic) (runRuntimeErrors result)
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

runCompileModuleGraph ::
  WarningSettings ->
  CliOptions ->
  Maybe Text ->
  [Text] ->
  (FilePath -> IO (Maybe Text)) ->
  IO CliOutput
runCompileModuleGraph settings options preludeSource entryModulePath sourceLookup = do
  result <-
    compileModuleGraphWithPrelude
      settings
      preludeSource
      (cliModuleConfig options)
      entryModulePath
      sourceLookup
  let warningLines = map formatWarningLine (compileWarnings result)
      errorLines = map (("error: " <>) . renderDiagnostic) (compileErrors result)
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

runExecuteModuleGraph ::
  WarningSettings ->
  CliOptions ->
  Maybe Text ->
  [Text] ->
  (FilePath -> IO (Maybe Text)) ->
  IO CliOutput
runExecuteModuleGraph settings options preludeSource entryModulePath sourceLookup = do
  result <-
    runModuleGraphWithPrelude
      settings
      preludeSource
      (cliModuleConfig options)
      entryModulePath
      sourceLookup
  let warningLines = map formatWarningLine (runWarnings result)
      compileErrorLines = map (("error: " <>) . renderDiagnostic) (runCompileErrors result)
      runtimeErrorLines = map (("error: " <>) . renderDiagnostic) (runRuntimeErrors result)
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

cliModuleConfig :: CliOptions -> ModuleResolutionConfig
cliModuleConfig options =
  ModuleResolutionConfig
    { moduleRoots =
        case cliModuleRoots options of
          [] -> ["."]
          roots -> roots,
      moduleExtension = ".jz"
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
