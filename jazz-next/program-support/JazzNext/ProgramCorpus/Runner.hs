{-# LANGUAGE OverloadedStrings #-}

module JazzNext.ProgramCorpus.Runner
  ( ProgramCaseResult (..),
    loadProgramCaseEntrySource,
    prepareProgramCase,
    programCaseResolutionConfig,
    readProgramCaseSource,
    runProgramCase,
  )
where

import Data.Text (Text)
import qualified Data.Text.IO as TextIO
import JazzNext.Compiler.BundledPrelude (bundledPreludeSource)
import JazzNext.Compiler.Diagnostics (Diagnostic, WarningRecord)
import JazzNext.Compiler.Driver
  ( ResolvedPrelude (PreludeBundled),
    RunResult (..),
    buildCompiledProgram,
    runModuleGraph,
  )
import JazzNext.Compiler.ModuleInterface (CompiledProgram)
import JazzNext.Compiler.ModuleResolver (ModuleResolutionConfig (..))
import JazzNext.Compiler.WarningConfig (defaultWarningSettings)
import JazzNext.ProgramCorpus.Types
  ( ProgramCase (..),
    ProgramTermination (..),
  )
import System.Directory (doesFileExist)
import System.FilePath ((</>))

data ProgramCaseResult = ProgramCaseResult
  { programCaseResultTermination :: ProgramTermination,
    programCaseResultStdout :: Text,
    programCaseResultDiagnostics :: [Diagnostic],
    programCaseResultWarnings :: [WarningRecord]
  }
  deriving (Eq, Show)

runProgramCase :: ProgramCase -> IO ProgramCaseResult
runProgramCase programCase = do
  runResult <-
    runModuleGraph
      defaultWarningSettings
      (programCaseResolutionConfig programCase)
      (programCaseEntryModulePath programCase)
      readProgramCaseSource
  pure (caseResult runResult)

loadProgramCaseEntrySource :: ProgramCase -> IO Text
loadProgramCaseEntrySource = TextIO.readFile . programCaseEntrySource

prepareProgramCase :: ProgramCase -> IO (Either Diagnostic CompiledProgram)
prepareProgramCase programCase =
  buildCompiledProgram
    defaultWarningSettings
    (PreludeBundled bundledPreludeSource)
    (programCaseResolutionConfig programCase)
    (programCaseEntryModulePath programCase)
    readProgramCaseSource

programCaseResolutionConfig :: ProgramCase -> ModuleResolutionConfig
programCaseResolutionConfig programCase =
  ModuleResolutionConfig
    { moduleRoots =
        [ programCaseModuleRoot programCase,
          programCasePackageRoot programCase </> "jazz" </> "stdlib"
        ],
      moduleExtension = ".jz"
    }

readProgramCaseSource :: FilePath -> IO (Maybe Text)
readProgramCaseSource path = do
  exists <- doesFileExist path
  if exists then Just <$> TextIO.readFile path else pure Nothing

caseResult :: RunResult -> ProgramCaseResult
caseResult result
  | not (null (runCompileErrors result)) =
      ProgramCaseResult
        { programCaseResultTermination = CompileFailedProgram,
          programCaseResultStdout = "",
          programCaseResultDiagnostics = runCompileErrors result,
          programCaseResultWarnings = runWarnings result
        }
  | not (null (runRuntimeErrors result)) =
      ProgramCaseResult
        { programCaseResultTermination = RuntimeFailedProgram,
          programCaseResultStdout = "",
          programCaseResultDiagnostics = runRuntimeErrors result,
          programCaseResultWarnings = runWarnings result
        }
  | otherwise =
      ProgramCaseResult
        { programCaseResultTermination = SuccessfulProgram,
          programCaseResultStdout = maybe "" (<> "\n") (runOutput result),
          programCaseResultDiagnostics = [],
          programCaseResultWarnings = runWarnings result
        }
