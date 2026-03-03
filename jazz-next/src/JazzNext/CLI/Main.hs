module JazzNext.CLI.Main
  ( CliOptions (..),
    CliOutput (..),
    parseCliOptions,
    runCliWith,
    main
  ) where

import Data.List (isPrefixOf)
import JazzNext.Compiler.AST
  ( Expr (..),
    Statement (..)
  )
import JazzNext.Compiler.Diagnostics
  ( SourceSpan (..),
    WarningRecord (..)
  )
import JazzNext.Compiler.Driver
  ( CompileResult (..),
    compileExpr
  )
import JazzNext.Compiler.WarningConfig
  ( WarningSettings,
    resolveWarningSettings
  )
import JazzNext.Compiler.Warnings
  ( warningToken
  )
import Control.Exception (IOException, evaluate, try)
import System.Environment (getArgs, lookupEnv)
import System.Exit (ExitCode (..), exitWith)
import System.IO (hPutStr, stderr, stdout)

data CliOptions = CliOptions
  { cliWarningFlags :: [String],
    cliWarningsConfigPath :: Maybe FilePath
  }
  deriving (Eq, Show)

data CliOutput = CliOutput
  { cliExitCode :: Int,
    cliStdout :: String,
    cliStderr :: String
  }
  deriving (Eq, Show)

-- Parse only the currently supported warning-related flags.
parseCliOptions :: [String] -> Either String CliOptions
parseCliOptions args = finalize <$> go (CliOptions [] Nothing) args
  where
    finalize options =
      options {cliWarningFlags = reverse (cliWarningFlags options)}
    go options [] = Right options
    go options ("--warnings-config" : path : rest) =
      go options {cliWarningsConfigPath = Just path} rest
    go _ ("--warnings-config" : []) =
      Left "missing path after --warnings-config"
    go options (arg : rest)
      | "-W" `isPrefixOf` arg =
          go options {cliWarningFlags = arg : cliWarningFlags options} rest
      | otherwise = Left ("unknown argument: " ++ arg)

runCliWith ::
  [String] ->
  (String -> IO (Maybe String)) ->
  (FilePath -> IO (Maybe String)) ->
  Expr ->
  IO CliOutput
runCliWith args envLookup configLookup expr =
  case parseCliOptions args of
    Left parseError ->
      pure
        CliOutput
          { cliExitCode = 2,
            cliStdout = "",
            cliStderr = "error: " ++ parseError ++ "\n"
          }
    Right options -> do
      settingsResult <- resolveSettings options envLookup configLookup
      case settingsResult of
        Left configError ->
          pure
            CliOutput
              { cliExitCode = 2,
                cliStdout = "",
                cliStderr = "error: " ++ configError ++ "\n"
              }
        Right settings -> runCompile settings expr

main :: IO ()
main = do
  args <- getArgs
  output <- runCliWith args lookupEnv readConfigMaybe sampleProgram
  hPutStr stdout (cliStdout output)
  hPutStr stderr (cliStderr output)
  exitWith (toExitCode (cliExitCode output))

resolveSettings ::
  CliOptions ->
  (String -> IO (Maybe String)) ->
  (FilePath -> IO (Maybe String)) ->
  IO (Either String WarningSettings)
resolveSettings options envLookup configLookup = do
  envWarningFlags <- envLookup "JAZZ_WARNING_FLAGS"
  envErrorFlags <- envLookup "JAZZ_WARNING_ERROR_FLAGS"
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

runCompile :: WarningSettings -> Expr -> IO CliOutput
runCompile settings expr = do
  result <- compileExpr settings expr
  let warningLines = map formatWarningLine (compileWarnings result)
      errorLines = map ("error: " ++) (compileErrors result)
      stderrOutput = renderLines (warningLines ++ errorLines)
      stdoutOutput =
        case generatedJs result of
          Just js -> js ++ "\n"
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

formatWarningLine :: WarningRecord -> String
formatWarningLine warning =
  warningCodeText warning
    ++ " ["
    ++ warningToken (warningCategory warning)
    ++ "] "
    ++ renderSpan (warningPrimarySpan warning)
    ++ ": "
    ++ warningMessage warning
    ++ renderPreviousSpan (warningPreviousSpan warning)

renderSpan :: SourceSpan -> String
renderSpan spanValue =
  show (spanLine spanValue) ++ ":" ++ show (spanColumn spanValue)

renderLines :: [String] -> String
renderLines [] = ""
renderLines linesOut = unlines linesOut

renderPreviousSpan :: Maybe SourceSpan -> String
renderPreviousSpan previous =
  case previous of
    Nothing -> ""
    Just previousSpan -> " (previous " ++ renderSpan previousSpan ++ ")"

readConfigMaybe :: FilePath -> IO (Maybe String)
readConfigMaybe path =
  -- Missing/unreadable config files are treated as absent so default warning
  -- behavior remains usable without setup.
  (eitherToMaybe <$> try readAndForce)
  where
    readAndForce :: IO String
    readAndForce = do
      contents <- readFile path
      _ <- evaluate (length contents)
      pure contents

    eitherToMaybe :: Either IOException String -> Maybe String
    eitherToMaybe readResult =
      case readResult of
        Left _ -> Nothing
        Right contents -> Just contents

sampleProgram :: Expr
-- Temporary bootstrap program used by the current CLI entrypoint until source
-- file parsing/execution wiring lands.
sampleProgram =
  EScope
    [ SLet "x" (SourceSpan 1 1) (EInt 1),
      SLet "x" (SourceSpan 2 1) (EInt 2)
    ]

toExitCode :: Int -> ExitCode
toExitCode code =
  if code == 0
    then ExitSuccess
    else ExitFailure code
