-- | Strictness utilities for diagnostic ownership boundaries.
--
-- Keeping diagnostic forcing here lets compiler phases release their internal
-- state without depending on the broad structural-forcing module.
module Jazz.Compiler.Diagnostics.Strictness
  ( forceDiagnostic,
  )
where

import Control.DeepSeq (rnf)
import Jazz.Compiler.Diagnostics (Diagnostic)

-- | Fully evaluate a diagnostic at a compiler ownership boundary.
forceDiagnostic :: Diagnostic -> ()
forceDiagnostic = rnf
