{-# LANGUAGE OverloadedStrings #-}

-- | Stable human-readable rendering for canonical diagnostics. Compiler
-- phases construct presentation-neutral reports; CLI and test boundaries turn
-- them into text here.
module Jazz.Compiler.Diagnostics.Render
  ( renderDiagnostic,
    renderSourceSpan
  ) where

import Data.Text (Text)
import qualified Data.Text as Text
import Jazz.Compiler.DiagnosticCatalog
  ( DiagnosticSeverity (..),
    diagnosticCodeText,
    warningToken
  )
import Jazz.Compiler.Diagnostics
  ( Diagnostic,
    DiagnosticLabel,
    SourceSpan (..),
    diagnosticCode,
    diagnosticHelp,
    diagnosticNotes,
    diagnosticPrimaryLabel,
    diagnosticSecondaryLabels,
    diagnosticSeverity,
    diagnosticSummary,
    diagnosticWarningCategory,
    labelMessage,
    labelSpan
  )

renderDiagnostic :: Diagnostic -> Text
renderDiagnostic diagnostic =
  {-# SCC "jazz-stage:diagnostic-rendering" #-}
  renderSeverity (diagnosticSeverity diagnostic)
    <> ": "
    <> diagnosticCodeText (diagnosticCode diagnostic)
    <> renderWarningToken diagnostic
    <> renderPrimaryLocation (diagnosticPrimaryLabel diagnostic)
    <> ": "
    <> diagnosticSummary diagnostic
    <> renderDetails diagnostic

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

renderSeverity :: DiagnosticSeverity -> Text
renderSeverity severity =
  case severity of
    SeverityWarning -> "warning"
    SeverityError -> "error"

renderWarningToken :: Diagnostic -> Text
renderWarningToken diagnostic =
  case diagnosticWarningCategory diagnostic of
    Nothing -> ""
    Just category -> " [" <> warningToken category <> "]"

renderPrimaryLocation :: Maybe DiagnosticLabel -> Text
renderPrimaryLocation maybeLabel =
  case maybeLabel of
    Nothing -> ""
    Just diagnosticLabel -> " " <> renderSourceSpan (labelSpan diagnosticLabel)

renderDetails :: Diagnostic -> Text
renderDetails diagnostic =
  case details of
    [] -> ""
    _ -> " (" <> Text.intercalate "; " details <> ")"
  where
    details =
      renderPrimaryMessage (diagnosticPrimaryLabel diagnostic)
        <> map renderSecondaryLabel (diagnosticSecondaryLabels diagnostic)
        <> map ("note: " <>) (diagnosticNotes diagnostic)
        <> maybe [] (\helpText -> ["help: " <> helpText]) (diagnosticHelp diagnostic)

renderPrimaryMessage :: Maybe DiagnosticLabel -> [Text]
renderPrimaryMessage maybeLabel =
  case maybeLabel of
    Just diagnosticLabel
      | not (Text.null (labelMessage diagnosticLabel)) -> [labelMessage diagnosticLabel]
    _ -> []

renderSecondaryLabel :: DiagnosticLabel -> Text
renderSecondaryLabel diagnosticLabel =
  let message = labelMessage diagnosticLabel
   in (if Text.null message then "related" else message)
        <> " "
        <> renderSourceSpan (labelSpan diagnosticLabel)
