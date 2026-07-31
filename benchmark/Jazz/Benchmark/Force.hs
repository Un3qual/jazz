module Jazz.Benchmark.Force
  ( module Jazz.Compiler.Force,
    forceProgramCaseResult,
  )
where

import Jazz.Compiler.Force
import Jazz.ProgramCorpus.Runner (ProgramCaseResult (..))

forceProgramCaseResult :: ProgramCaseResult -> ()
forceProgramCaseResult result =
  programCaseResultTermination result `seq`
    programCaseResultStdout result `seq`
      forceListWith forceDiagnostic (programCaseResultDiagnostics result) `seq`
        forceListWith forceDiagnostic (programCaseResultWarnings result) `seq`
          programCaseResultObservation result `seq`
            ()
