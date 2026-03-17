{-# LANGUAGE OverloadedStrings #-}

module JazzNext.Compiler.Diagnostics
  ( Diagnostic (..),
    RenderDiagnostic (..),
    SourceSpan (..),
    WarningRecord (..),
    appendDiagnosticNote,
    diagnosticFromRendered,
    mkDiagnostic,
    mkMessageDiagnostic,
    prependDiagnosticSummary,
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
import JazzNext.Compiler.WarningCatalog
  ( WarningCategory (..),
    warningCode
  )

data SourceSpan = SourceSpan
  { spanLine :: Int,
    spanColumn :: Int
  }
  deriving (Eq, Ord, Show)

data Diagnostic = Diagnostic
  { diagnosticCode :: Text,
    diagnosticSummary :: Text,
    diagnosticPrimarySpan :: Maybe SourceSpan,
    diagnosticRelatedSpan :: Maybe SourceSpan,
    diagnosticSubject :: Maybe Text,
    diagnosticNotes :: [Text]
  }
  deriving (Eq, Show)

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

renderDiagnosticRecord :: Diagnostic -> Text
renderDiagnosticRecord diagnostic =
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

diagnosticFromRendered :: Text -> Diagnostic
diagnosticFromRendered rendered =
  case Text.breakOn ": " rendered of
    (code, summary)
      | not (Text.null code) && not (Text.null summary) && isValidDiagnosticCode code ->
          mkDiagnostic code (Text.drop 2 summary)
    _ ->
      mkMessageDiagnostic rendered
  where
    isValidDiagnosticCode code =
      case Text.uncons code of
        Just (firstChar, rest) ->
          -- Diagnostic codes must start with an uppercase letter followed by digits (e.g., "E1234")
          firstChar >= 'A' && firstChar <= 'Z' && Text.all (\c -> c >= '0' && c <= '9') rest && not (Text.null rest)
        Nothing -> False

renderSourceSpan :: SourceSpan -> Text
renderSourceSpan spanValue =
  Text.pack (show (spanLine spanValue)) <> ":" <> Text.pack (show (spanColumn spanValue))

mkSameScopeRebindingWarning :: Text -> SourceSpan -> SourceSpan -> WarningRecord
mkSameScopeRebindingWarning variableName primarySpan previousSpan =
  WarningRecord
    { warningCategory = SameScopeRebinding,
      warningCodeText = warningCode SameScopeRebinding,
      warningVariableName = variableName,
      warningPrimarySpan = primarySpan,
      warningPreviousSpan = Just previousSpan,
      warningMessage =
        "same-scope rebinding: '"
          <> variableName
          <> "' shadows previous same-scope binding (last declaration wins)"
    }

sortWarnings :: [WarningRecord] -> [WarningRecord]
-- Keep warning output deterministic so tests and CLI behavior are stable.
sortWarnings =
  sortOn
    ( \warning ->
        ( warningPrimarySpan warning,
          warningCategory warning,
          warningVariableName warning
        )
    )
