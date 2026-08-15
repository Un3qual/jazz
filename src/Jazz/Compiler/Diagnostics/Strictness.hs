-- | Strictness utilities for diagnostic ownership boundaries.
--
-- Keeping diagnostic forcing here lets compiler phases release their internal
-- state without depending on the broad structural-forcing module.
module Jazz.Compiler.Diagnostics.Strictness
  ( forceDiagnostic,
  )
where

import Jazz.Compiler.Diagnostics
  ( Diagnostic,
    DiagnosticLabel,
    diagnosticCode,
    diagnosticHelp,
    diagnosticNotes,
    diagnosticOrigin,
    diagnosticPrimaryLabel,
    diagnosticSecondaryLabels,
    diagnosticSeverity,
    diagnosticSubject,
    diagnosticSummary,
    diagnosticWarningCategory,
    labelMessage,
    labelSpan,
  )

-- | Fully evaluate the structural fields of a diagnostic while leaving text
-- payloads at weak head normal form.
forceDiagnostic :: Diagnostic -> ()
forceDiagnostic diagnostic =
  diagnosticSeverity diagnostic `seq`
    diagnosticCode diagnostic `seq`
      diagnosticWarningCategory diagnostic `seq`
        diagnosticOrigin diagnostic `seq`
          diagnosticSummary diagnostic `seq`
            forceMaybeWith forceDiagnosticLabel (diagnosticPrimaryLabel diagnostic) `seq`
              forceListWith forceDiagnosticLabel (diagnosticSecondaryLabels diagnostic) `seq`
                forceMaybeWith (\subject -> subject `seq` ()) (diagnosticSubject diagnostic) `seq`
                  forceListWith (\note -> note `seq` ()) (diagnosticNotes diagnostic) `seq`
                    forceMaybeWith (\helpText -> helpText `seq` ()) (diagnosticHelp diagnostic)

forceDiagnosticLabel :: DiagnosticLabel -> ()
forceDiagnosticLabel diagnosticLabel =
  labelSpan diagnosticLabel `seq`
    labelMessage diagnosticLabel `seq`
      ()

forceListWith :: (value -> ()) -> [value] -> ()
forceListWith forceValue values =
  case values of
    [] -> ()
    value : remaining -> forceValue value `seq` forceListWith forceValue remaining

forceMaybeWith :: (value -> ()) -> Maybe value -> ()
forceMaybeWith = maybe ()
