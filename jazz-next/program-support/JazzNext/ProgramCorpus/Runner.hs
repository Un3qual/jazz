{-# LANGUAGE OverloadedStrings #-}

module JazzNext.ProgramCorpus.Runner
  ( ProgramCaseResult (..),
    runProgramCase,
  )
where

import Data.Text (Text)
import qualified Data.Text.IO as TextIO
import JazzNext.Compiler.Diagnostics (Diagnostic, WarningRecord)
import JazzNext.Compiler.Driver
  ( RunResult (..),
    runModuleGraph,
  )
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
      ModuleResolutionConfig
        { moduleRoots =
            [ programCaseModuleRoot programCase,
              programCasePackageRoot programCase </> "jazz" </> "stdlib"
            ],
          moduleExtension = ".jz"
        }
      (programCaseEntryModulePath programCase)
      readSource
  pure (caseResult runResult)
  where
    readSource path = do
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
