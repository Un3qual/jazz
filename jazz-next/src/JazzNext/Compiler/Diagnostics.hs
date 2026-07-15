{-# LANGUAGE OverloadedStrings #-}

-- | Shared diagnostic and warning record types used across parser, analyzer,
-- resolver, runtime, and CLI layers.
module JazzNext.Compiler.Diagnostics
  ( Diagnostic (..),
    RenderDiagnostic (..),
    SourceSpan (..),
    WarningRecord (..),
    appendDiagnosticNote,
    mkDiagnostic,
    mkMessageDiagnostic,
    prependDiagnosticSummary,
    qualifySourceSpan,
    setDiagnosticCode,
    setDiagnosticPrimarySpan,
    setDiagnosticRelatedSpan,
    setDiagnosticSubject,
    renderDiagnostic,
    renderDiagnosticRecord,
    renderSourceSpan,
    mkSameScopeRebindingWarning,
    sortWarnings
  ) where

import Data.List (sortOn)
import Data.Text (Text)
import qualified Data.Text as Text
import JazzNext.Compiler.DiagnosticCatalog
  ( WarningCategory (..),
    diagnosticCodeText,
    warningCode
  )

-- | 1-based source location used throughout the compiler. Standalone parsing
-- uses the compact constructor; resolved modules qualify spans with their
-- source paths before per-module semantic analysis.
data SourceSpan
  = SourceSpan
      { spanLine :: Int,
        spanColumn :: Int
      }
  | SourceSpanIn
      { spanSourcePath :: FilePath,
        spanLine :: Int,
        spanColumn :: Int
      }
  deriving (Eq, Ord, Show)

-- | Structured error payload that can carry stable codes, source spans, and
-- extra notes as compilation moves between phases.
data Diagnostic = Diagnostic
  { diagnosticCode :: Text,
    diagnosticSummary :: Text,
    diagnosticPrimarySpan :: Maybe SourceSpan,
    diagnosticRelatedSpan :: Maybe SourceSpan,
    diagnosticSubject :: Maybe Text,
    diagnosticNotes :: [Text]
  }
  deriving (Eq, Show)

-- | Structured warning payload preserved separately from errors so warning
-- policy can decide later whether to render or promote it.
data WarningRecord = WarningRecord
  { warningCategory :: WarningCategory,
    warningCodeText :: Text,
    warningVariableName :: Text,
    warningPrimarySpan :: SourceSpan,
    warningPreviousSpan :: Maybe SourceSpan,
    warningMessage :: Text
  }
  deriving (Eq, Show)

class RenderDiagnostic a where
  toDiagnostic :: a -> Diagnostic

instance RenderDiagnostic Diagnostic where
  toDiagnostic = id

instance RenderDiagnostic Text where
  toDiagnostic = mkMessageDiagnostic

instance RenderDiagnostic WarningRecord where
  toDiagnostic warning =
    Diagnostic
      { diagnosticCode = warningCodeText warning,
        diagnosticSummary = warningMessage warning,
        diagnosticPrimarySpan = Just (warningPrimarySpan warning),
        diagnosticRelatedSpan = Nothing,
        diagnosticSubject = Just (warningVariableName warning),
        diagnosticNotes =
          case warningPreviousSpan warning of
            Nothing -> []
            Just previousSpan ->
              ["previous " <> renderSourceSpan previousSpan]
      }

renderDiagnostic :: RenderDiagnostic a => a -> Text
renderDiagnostic = renderDiagnosticRecord . toDiagnostic

-- | Render diagnostics into the CLI/test string form while preserving source
-- locations and supplemental notes when present.
renderDiagnosticRecord :: Diagnostic -> Text
renderDiagnosticRecord diagnostic =
  {-# SCC "jazz-stage:diagnostic-rendering" #-}
  renderCodePrefix (diagnosticCode diagnostic)
    <> renderPrimarySpan (diagnosticPrimarySpan diagnostic)
    <> diagnosticSummary diagnostic
    <> renderNotes noteTexts
  where
    noteTexts =
      case diagnosticRelatedSpan diagnostic of
        Nothing -> diagnosticNotes diagnostic
        Just relatedSpan ->
          ("related " <> renderSourceSpan relatedSpan) : diagnosticNotes diagnostic

    renderCodePrefix code
      | Text.null code = ""
      | otherwise = code <> ": "

    renderPrimarySpan maybeSpan =
      case maybeSpan of
        Nothing -> ""
        Just spanValue -> renderSourceSpan spanValue <> ": "

    renderNotes notes =
      case notes of
        [] -> ""
        _ -> " (" <> Text.intercalate "; " notes <> ")"

mkDiagnostic :: Text -> Text -> Diagnostic
mkDiagnostic code summary =
  Diagnostic
    { diagnosticCode = code,
      diagnosticSummary = summary,
      diagnosticPrimarySpan = Nothing,
      diagnosticRelatedSpan = Nothing,
      diagnosticSubject = Nothing,
      diagnosticNotes = []
    }

mkMessageDiagnostic :: Text -> Diagnostic
mkMessageDiagnostic = mkDiagnostic ""

setDiagnosticCode :: Text -> Diagnostic -> Diagnostic
setDiagnosticCode code diagnostic =
  diagnostic {diagnosticCode = code}

setDiagnosticPrimarySpan :: SourceSpan -> Diagnostic -> Diagnostic
setDiagnosticPrimarySpan spanValue diagnostic =
  diagnostic {diagnosticPrimarySpan = Just spanValue}

setDiagnosticRelatedSpan :: SourceSpan -> Diagnostic -> Diagnostic
setDiagnosticRelatedSpan spanValue diagnostic =
  diagnostic {diagnosticRelatedSpan = Just spanValue}

setDiagnosticSubject :: Text -> Diagnostic -> Diagnostic
setDiagnosticSubject subject diagnostic =
  diagnostic {diagnosticSubject = Just subject}

prependDiagnosticSummary :: Text -> Diagnostic -> Diagnostic
prependDiagnosticSummary prefix diagnostic =
  diagnostic {diagnosticSummary = prefix <> diagnosticSummary diagnostic}

appendDiagnosticNote :: Text -> Diagnostic -> Diagnostic
appendDiagnosticNote note diagnostic =
  diagnostic {diagnosticNotes = diagnosticNotes diagnostic <> [note]}

qualifySourceSpan :: FilePath -> SourceSpan -> SourceSpan
qualifySourceSpan sourcePath spanValue =
  SourceSpanIn sourcePath (spanLine spanValue) (spanColumn spanValue)

renderSourceSpan :: SourceSpan -> Text
renderSourceSpan spanValue =
  renderSourcePath spanValue
    <> Text.pack (show (spanLine spanValue))
    <> ":"
    <> Text.pack (show (spanColumn spanValue))
  where
    renderSourcePath sourceSpan =
      case sourceSpan of
        SourceSpan {} -> ""
        SourceSpanIn sourcePath _ _ -> Text.pack sourcePath <> ":"

mkSameScopeRebindingWarning :: Text -> SourceSpan -> SourceSpan -> WarningRecord
mkSameScopeRebindingWarning variableName primarySpan previousSpan =
  WarningRecord
    { warningCategory = SameScopeRebinding,
      warningCodeText = diagnosticCodeText (warningCode SameScopeRebinding),
      warningVariableName = variableName,
      warningPrimarySpan = primarySpan,
      warningPreviousSpan = Just previousSpan,
      warningMessage =
        "same-scope rebinding: '"
          <> variableName
          <> "' shadows previous same-scope binding (last declaration wins)"
    }

-- | Sort warnings deterministically so CLI output and tests do not depend on
-- evaluation order inside earlier compiler phases.
sortWarnings :: [WarningRecord] -> [WarningRecord]
sortWarnings =
  sortOn
    ( \warning ->
        ( warningPrimarySpan warning,
          warningCategory warning,
          warningVariableName warning
        )
    )
