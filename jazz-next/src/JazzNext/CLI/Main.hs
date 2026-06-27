{-# LANGUAGE OverloadedStrings #-}

-- | Thin CLI layer that translates arguments, env, and file lookups into the
-- driver entrypoints used by tests and the executable.
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
import JazzNext.Compiler.BundledPrelude
  ( loadBundledPreludeSource
  )
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
    ResolvedPrelude (..),
    RunResult (..),
    compileModuleGraphWithResolvedPrelude,
    compileSourceWithResolvedPrelude,
    runModuleGraphWithResolvedPrelude,
    runSourceWithResolvedPrelude
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

-- | Parsed CLI configuration after argument validation.
data CliOptions = CliOptions
  { cliWarningFlags :: [Text],
    cliWarningsConfigPath :: Maybe FilePath,
    cliRunMode :: Bool,
    cliPreludePath :: Maybe FilePath,
    cliDisablePrelude :: Bool,
    cliEntryModule :: Maybe [Text],
    cliModuleRoots :: [FilePath],
    cliSourcePath :: Maybe FilePath
  }
  deriving (Eq, Show)

-- | Captured CLI side effects, used both by tests and by `main`.
data CliOutput = CliOutput
  { cliExitCode :: Int,
    cliStdout :: Text,
    cliStderr :: Text
  }
  deriving (Eq, Show)

helpUsageText :: Text
helpUsageText =
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

-- | Distinguishes required config paths from the optional default probe so
-- missing files can produce the right CLI behavior.
data WarningConfigSelection
  = ExplicitWarningConfig FilePath
  | DefaultWarningConfigProbe FilePath

isHelpArg :: String -> Bool
isHelpArg arg =
  arg == "--help" || arg == "-h"

-- Parse currently supported warning and prelude-loading flags.
parseCliOptions :: [String] -> Either Diagnostic CliOptions
parseCliOptions args = do
  options <- go (CliOptions [] Nothing False Nothing False Nothing [] Nothing) args
  finalize options
  where
    finalize options
      | cliDisablePrelude options && isJust (cliPreludePath options) =
          Left (mkMessageDiagnostic "cannot combine --prelude with --no-prelude")
      | isJust (cliSourcePath options) && isJust (cliEntryModule options) =
          Left (mkMessageDiagnostic "cannot combine source file with --entry-module")
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
      | isHelpArg arg =
          go options rest
      | "-W" `isPrefixOf` arg =
          go options {cliWarningFlags = Text.pack arg : cliWarningFlags options} rest
      | "-" `isPrefixOf` arg =
          Left (mkMessageDiagnostic ("unknown argument: " <> Text.pack arg))
      | isJust (cliSourcePath options) =
          Left (mkMessageDiagnostic "multiple source files are not supported")
      | otherwise =
          go options {cliSourcePath = Just arg} rest

-- | End-to-end CLI entrypoint with injectable env/config/source lookups so the
-- behavior stays testable without shelling out.
runCliWith ::
  [String] ->
  (String -> IO (Maybe String)) ->
  (FilePath -> IO (Maybe Text)) ->
  IO Text ->
  IO CliOutput
runCliWith args envLookup fileLookup loadSource
  | any isHelpArg args =
      pure
        CliOutput
          { cliExitCode = 0,
            cliStdout = helpUsageText,
            cliStderr = ""
          }
  | otherwise =
      case parseCliOptions args of
        Left parseError ->
          pure
            CliOutput
              { cliExitCode = 2,
                cliStdout = "",
                cliStderr = "error: " <> renderDiagnostic parseError <> "\n"
              }
        Right options -> do
          settingsResult <- resolveSettings options envLookup fileLookup
          case settingsResult of
            Left configError ->
              pure
                CliOutput
                  { cliExitCode = 2,
                    cliStdout = "",
                    cliStderr = "error: " <> renderDiagnostic configError <> "\n"
                  }
            Right settings -> do
              preludeSourceResult <- resolvePreludeSource options envLookup fileLookup
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
                        then runExecuteModuleGraph settings options preludeSource entryModulePath fileLookup
                        else runCompileModuleGraph settings options preludeSource entryModulePath fileLookup
                    Nothing -> do
                      sourceResult <- loadCliSource options fileLookup loadSource
                      case sourceResult of
                        Left sourceError ->
                          pure
                            CliOutput
                              { cliExitCode = 2,
                                cliStdout = "",
                                cliStderr = "error: " <> renderDiagnostic sourceError <> "\n"
                              }
                        Right source ->
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

-- | Resolve warning settings using the published precedence order between CLI,
-- env vars, and config files.
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
          Just cliPath -> ExplicitWarningConfig cliPath
          Nothing ->
            case envConfigPath of
              Just envPath -> ExplicitWarningConfig envPath
              Nothing -> DefaultWarningConfigProbe ".jazz-warnings"
  configContentsResult <- loadWarningConfig selectedConfigPath configLookup
  pure $
    case configContentsResult of
      Left configError -> Left configError
      Right configContents ->
        resolveWarningSettings (cliWarningFlags options) envWarningFlags envErrorFlags configContents

loadWarningConfig ::
  WarningConfigSelection ->
  (FilePath -> IO (Maybe Text)) ->
  IO (Either Diagnostic (Maybe Text))
loadWarningConfig configSelection configLookup =
  case configSelection of
    ExplicitWarningConfig configPath -> do
      configContents <- configLookup configPath
      pure $
        case configContents of
          Just contents -> Right (Just contents)
          Nothing ->
            Left
              ( mkMessageDiagnostic
                  ("warning config file could not be read at '" <> Text.pack configPath <> "'")
              )
    DefaultWarningConfigProbe configPath ->
      Right <$> configLookup configPath

loadCliSource ::
  CliOptions ->
  (FilePath -> IO (Maybe Text)) ->
  IO Text ->
  IO (Either Diagnostic Text)
loadCliSource options fileLookup loadStdin =
  case cliSourcePath options of
    Nothing -> Right <$> loadStdin
    Just sourcePath -> do
      sourceContents <- fileLookup sourcePath
      pure $
        case sourceContents of
          Just contents -> Right contents
          Nothing ->
            Left
              (mkMessageDiagnostic ("source file could not be read at '" <> Text.pack sourcePath <> "'"))

-- | Resolve the prelude source according to CLI/env flags, defaulting to the
-- bundled prelude when neither an explicit path nor `--no-prelude` is given.
resolvePreludeSource ::
  CliOptions ->
  (String -> IO (Maybe String)) ->
  (FilePath -> IO (Maybe Text)) ->
  IO (Either Diagnostic ResolvedPrelude)
resolvePreludeSource options envLookup fileLookup = do
  envPreludePath <- envLookup "JAZZ_PRELUDE"
  if cliDisablePrelude options
    then pure (Right PreludeAbsent)
    else
      case cliPreludePath options of
        Just cliPath -> loadRequiredPrelude cliPath
        Nothing ->
          case envPreludePath of
            Just envPath -> loadRequiredPrelude envPath
            Nothing -> Right . PreludeBundled <$> loadBundledPreludeSource
  where
    loadRequiredPrelude :: FilePath -> IO (Either Diagnostic ResolvedPrelude)
    loadRequiredPrelude preludePath = do
      preludeContents <- fileLookup preludePath
      pure $
        case preludeContents of
          Just contents -> Right (PreludeExplicit contents)
          Nothing ->
            Left
              ( mkDiagnostic
                  "E0003"
                  ("prelude file could not be read at '" <> Text.pack preludePath <> "'")
              )

runCompile :: WarningSettings -> ResolvedPrelude -> Text -> IO CliOutput
runCompile settings resolvedPrelude source = do
  result <- compileSourceWithResolvedPrelude settings resolvedPrelude source
  let warningLines = map formatWarningLine (compileWarnings result)
      errorLines = map (("error: " <>) . renderDiagnostic) (compileErrors result)
      stderrOutput = renderLines (warningLines ++ errorLines)
      -- Compile mode is diagnostics-only; evaluated program output belongs to
      -- `--run`.
      stdoutOutput = ""
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

runExecute :: WarningSettings -> ResolvedPrelude -> Text -> IO CliOutput
runExecute settings resolvedPrelude source = do
  result <- runSourceWithResolvedPrelude settings resolvedPrelude source
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

-- | Compile-mode module graph runs share the same diagnostics-only stdout
-- contract as standalone compile mode.
runCompileModuleGraph ::
  WarningSettings ->
  CliOptions ->
  ResolvedPrelude ->
  [Text] ->
  (FilePath -> IO (Maybe Text)) ->
  IO CliOutput
runCompileModuleGraph settings options resolvedPrelude entryModulePath sourceLookup = do
  result <-
    compileModuleGraphWithResolvedPrelude
      settings
      resolvedPrelude
      (cliModuleConfig options)
      entryModulePath
      sourceLookup
  let warningLines = map formatWarningLine (compileWarnings result)
      errorLines = map (("error: " <>) . renderDiagnostic) (compileErrors result)
      stderrOutput = renderLines (warningLines ++ errorLines)
      -- Keep module-graph compile output aligned with standalone compile mode:
      -- success is quiet unless warnings or errors need to be reported.
      stdoutOutput = ""
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
  ResolvedPrelude ->
  [Text] ->
  (FilePath -> IO (Maybe Text)) ->
  IO CliOutput
runExecuteModuleGraph settings options resolvedPrelude entryModulePath sourceLookup = do
  result <-
    runModuleGraphWithResolvedPrelude
      settings
      resolvedPrelude
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

-- | Translate CLI module-root options into the resolver configuration used by
-- compile/run module-graph entrypoints.
cliModuleConfig :: CliOptions -> ModuleResolutionConfig
cliModuleConfig options =
  ModuleResolutionConfig
    { moduleRoots =
        case cliModuleRoots options of
          [] -> ["."]
          roots -> roots,
      moduleExtension = ".jz"
    }

-- | Render warnings in the CLI's stable single-line format.
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

-- | Read a warning config file as an optional blob. `resolveSettings` decides
-- whether a missing result is acceptable (the implicit default probe) or a
-- user-facing error (an explicit CLI/env config path).
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
