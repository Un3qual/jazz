module JazzNext.Benchmark.Force
  ( module JazzNext.Compiler.Force,
    forceProgramCaseResult,
  )
where

import JazzNext.Compiler.Force
import JazzNext.ProgramCorpus.Runner (ProgramCaseResult (..))

forceProgramCaseResult :: ProgramCaseResult -> ()
forceProgramCaseResult result =
  programCaseResultTermination result `seq`
    programCaseResultStdout result `seq`
      forceListWith forceDiagnostic (programCaseResultDiagnostics result) `seq`
        forceListWith forceWarning (programCaseResultWarnings result) `seq`
          programCaseResultObservation result `seq`
            ()
