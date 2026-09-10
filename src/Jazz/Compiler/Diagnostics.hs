{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Presentation-neutral diagnostics shared by compiler, runtime, and tooling
-- layers. Raw construction stays private so native errors and configurable
-- warnings cannot be assembled in contradictory states.
module Jazz.Compiler.Diagnostics
  ( Diagnostic,
    DiagnosticLabel,
    DiagnosticContext (..),
    appendDiagnosticContext,
    diagnosticContexts,
    diagnosticTypeError,
    mkTypeErrorDiagnostic,
    DiagnosticOrigin (..),
    SourceSpan (..),
    sourceSpanStart,
    sourceSpanEnd,
    spanThrough,
    appendDiagnosticNote,
    appendDiagnosticSecondaryLabel,
    diagnosticCode,
    diagnosticHelp,
    diagnosticNotes,
    diagnosticOrigin,
    diagnosticPrimaryLabel,
    diagnosticPrimarySpan,
    diagnosticRelatedSpan,
    diagnosticSecondaryLabels,
    diagnosticSeverity,
    diagnosticSubject,
    diagnosticSummary,
    diagnosticWarningCategory,
    isErrorDiagnostic,
    isRuntimeDiagnostic,
    isWarningDiagnostic,
    labelMessage,
    labelSpan,
    mkErrorDiagnostic,
    mkWarningDiagnostic,
    prependDiagnosticSummary,
    promoteDiagnostic,
    qualifyDiagnosticSpans,
    qualifySourceSpan,
    setDiagnosticErrorCode,
    setDiagnosticHelp,
    setDiagnosticPrimaryLabel,
    setDiagnosticPrimarySpan,
    setDiagnosticRelatedSpan,
    setDiagnosticSubject,
    mkSameScopeRebindingWarning,
    sortWarnings,
  )
where

import Control.DeepSeq (NFData)
import Data.List (sortOn)
import Data.Text (Text)
import GHC.Generics (Generic)
import Jazz.Compiler.DiagnosticCatalog
  ( DiagnosticCode,
    DiagnosticSeverity (..),
    ErrorCode,
    WarningCategory (..),
    errorCode,
    warningCode,
  )
import Jazz.Compiler.SourceSpan (SourceSpan (..), sourceSpanEnd, sourceSpanStart, spanThrough)
import Jazz.Compiler.TypeInference.DiagnosticCause (DiagnosticType, TypeErrorCause, renderTypeErrorCause)

-- | Broad ownership needed by result filtering without coupling the common
-- report to phase-specific compiler types.
data DiagnosticOrigin
  = CompilationOrigin
  | RuntimeOrigin
  | ToolingOrigin
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

-- | A source span plus phase-owned explanatory text. Presentation punctuation
-- remains the renderer's responsibility.
data DiagnosticLabel = DiagnosticLabel
  { labelSpan :: SourceSpan,
    labelMessage :: Text
  }
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

-- | Enclosing operations, ordered from the innermost operation outwards.
data DiagnosticContext
  = CheckingBinding Text
  | CheckingImplMethod Text
  | SatisfyingConstraint Text
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

data DiagnosticMessage
  = PlainMessage Text
  | TypeErrorMessage (TypeErrorCause DiagnosticType)
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

data Diagnostic = Diagnostic
  { diagnosticSeverity :: DiagnosticSeverity,
    diagnosticCode :: DiagnosticCode,
    diagnosticWarningCategory :: Maybe WarningCategory,
    diagnosticOrigin :: DiagnosticOrigin,
    diagnosticMessage :: DiagnosticMessage,
    diagnosticSummaryPrefix :: Text,
    diagnosticContexts :: [DiagnosticContext],
    diagnosticPrimaryLabel :: Maybe DiagnosticLabel,
    diagnosticSecondaryLabels :: [DiagnosticLabel],
    diagnosticSubject :: Maybe Text,
    diagnosticNotes :: [Text],
    diagnosticHelp :: Maybe Text
  }
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

mkErrorDiagnostic :: ErrorCode -> DiagnosticOrigin -> Text -> Diagnostic
mkErrorDiagnostic code origin summary =
  Diagnostic
    { diagnosticSeverity = SeverityError,
      diagnosticCode = errorCode code,
      diagnosticWarningCategory = Nothing,
      diagnosticOrigin = origin,
      diagnosticMessage = PlainMessage summary,
      diagnosticSummaryPrefix = "",
      diagnosticContexts = [],
      diagnosticPrimaryLabel = Nothing,
      diagnosticSecondaryLabels = [],
      diagnosticSubject = Nothing,
      diagnosticNotes = [],
      diagnosticHelp = Nothing
    }

mkWarningDiagnostic :: WarningCategory -> DiagnosticOrigin -> Text -> Diagnostic
mkWarningDiagnostic category origin summary =
  Diagnostic
    { diagnosticSeverity = SeverityWarning,
      diagnosticCode = warningCode category,
      diagnosticWarningCategory = Just category,
      diagnosticOrigin = origin,
      diagnosticMessage = PlainMessage summary,
      diagnosticSummaryPrefix = "",
      diagnosticContexts = [],
      diagnosticPrimaryLabel = Nothing,
      diagnosticSecondaryLabels = [],
      diagnosticSubject = Nothing,
      diagnosticNotes = [],
      diagnosticHelp = Nothing
    }

-- | Keep the cause structured until a consumer asks for its summary.
mkTypeErrorDiagnostic :: ErrorCode -> TypeErrorCause DiagnosticType -> Diagnostic
mkTypeErrorDiagnostic code cause =
  (mkErrorDiagnostic code CompilationOrigin "") {diagnosticMessage = TypeErrorMessage cause}

diagnosticTypeError :: Diagnostic -> Maybe (TypeErrorCause DiagnosticType)
diagnosticTypeError diagnostic = case diagnosticMessage diagnostic of
  PlainMessage _ -> Nothing
  TypeErrorMessage cause -> Just cause

diagnosticSummary :: Diagnostic -> Text
diagnosticSummary diagnostic =
  diagnosticSummaryPrefix diagnostic <> case diagnosticMessage diagnostic of
    PlainMessage summary -> summary
    TypeErrorMessage cause -> renderTypeErrorCause cause

appendDiagnosticContext :: DiagnosticContext -> Diagnostic -> Diagnostic
appendDiagnosticContext context diagnostic
  | context `elem` diagnosticContexts diagnostic = diagnostic
  | otherwise = diagnostic {diagnosticContexts = diagnosticContexts diagnostic <> [context]}

promoteDiagnostic :: Diagnostic -> Diagnostic
promoteDiagnostic diagnostic =
  case diagnosticWarningCategory diagnostic of
    Nothing -> diagnostic
    Just _ -> diagnostic {diagnosticSeverity = SeverityError}

isWarningDiagnostic :: Diagnostic -> Bool
isWarningDiagnostic diagnostic = diagnosticSeverity diagnostic == SeverityWarning

isErrorDiagnostic :: Diagnostic -> Bool
isErrorDiagnostic diagnostic = diagnosticSeverity diagnostic == SeverityError

isRuntimeDiagnostic :: Diagnostic -> Bool
isRuntimeDiagnostic diagnostic = diagnosticOrigin diagnostic == RuntimeOrigin

-- | Recode a native error after wrapping lower-level compilation context.
-- Warning identity cannot be replaced through this helper.
setDiagnosticErrorCode :: ErrorCode -> Diagnostic -> Diagnostic
setDiagnosticErrorCode code diagnostic =
  case diagnosticWarningCategory diagnostic of
    Nothing -> diagnostic {diagnosticCode = errorCode code}
    Just _ -> diagnostic

setDiagnosticPrimaryLabel :: SourceSpan -> Text -> Diagnostic -> Diagnostic
setDiagnosticPrimaryLabel spanValue message diagnostic =
  diagnostic {diagnosticPrimaryLabel = Just (DiagnosticLabel spanValue message)}

appendDiagnosticSecondaryLabel :: SourceSpan -> Text -> Diagnostic -> Diagnostic
appendDiagnosticSecondaryLabel spanValue message diagnostic =
  diagnostic
    { diagnosticSecondaryLabels =
        diagnosticSecondaryLabels diagnostic <> [DiagnosticLabel spanValue message]
    }

-- | Compatibility helper for producers that do not yet have a useful label.
setDiagnosticPrimarySpan :: SourceSpan -> Diagnostic -> Diagnostic
setDiagnosticPrimarySpan spanValue = setDiagnosticPrimaryLabel spanValue ""

-- | Compatibility helper for the former single related-span representation.
setDiagnosticRelatedSpan :: SourceSpan -> Diagnostic -> Diagnostic
setDiagnosticRelatedSpan spanValue diagnostic =
  diagnostic
    { diagnosticSecondaryLabels =
        DiagnosticLabel spanValue "related" : drop 1 (diagnosticSecondaryLabels diagnostic)
    }

diagnosticPrimarySpan :: Diagnostic -> Maybe SourceSpan
diagnosticPrimarySpan = fmap labelSpan . diagnosticPrimaryLabel

diagnosticRelatedSpan :: Diagnostic -> Maybe SourceSpan
diagnosticRelatedSpan diagnostic =
  case diagnosticSecondaryLabels diagnostic of
    [] -> Nothing
    diagnosticLabel : _ -> Just (labelSpan diagnosticLabel)

setDiagnosticSubject :: Text -> Diagnostic -> Diagnostic
setDiagnosticSubject subject diagnostic =
  diagnostic {diagnosticSubject = Just subject}

prependDiagnosticSummary :: Text -> Diagnostic -> Diagnostic
prependDiagnosticSummary prefix diagnostic =
  diagnostic {diagnosticSummaryPrefix = prefix <> diagnosticSummaryPrefix diagnostic}

appendDiagnosticNote :: Text -> Diagnostic -> Diagnostic
appendDiagnosticNote note diagnostic =
  diagnostic {diagnosticNotes = diagnosticNotes diagnostic <> [note]}

setDiagnosticHelp :: Text -> Diagnostic -> Diagnostic
setDiagnosticHelp helpText diagnostic =
  diagnostic {diagnosticHelp = Just helpText}

qualifyDiagnosticSpans :: FilePath -> Diagnostic -> Diagnostic
qualifyDiagnosticSpans sourcePath diagnostic =
  diagnostic
    { diagnosticPrimaryLabel = qualifyLabel <$> diagnosticPrimaryLabel diagnostic,
      diagnosticSecondaryLabels = map qualifyLabel (diagnosticSecondaryLabels diagnostic)
    }
  where
    qualifyLabel diagnosticLabel =
      diagnosticLabel
        { labelSpan = qualifySourceSpan sourcePath (labelSpan diagnosticLabel)
        }

qualifySourceSpan :: FilePath -> SourceSpan -> SourceSpan
qualifySourceSpan sourcePath spanValue =
  case sourceSpanEnd spanValue of
    Nothing -> SourceSpanIn sourcePath (spanLine spanValue) (spanColumn spanValue)
    Just (endLine, endColumn) -> SourceRangeIn sourcePath (spanLine spanValue) (spanColumn spanValue) endLine endColumn

mkSameScopeRebindingWarning :: Text -> SourceSpan -> SourceSpan -> Diagnostic
mkSameScopeRebindingWarning variableName primarySpan previousSpan =
  appendDiagnosticSecondaryLabel previousSpan "previous" $
    setDiagnosticPrimaryLabel primarySpan "warning emitted here" $
      setDiagnosticSubject variableName $
        mkWarningDiagnostic
          SameScopeRebinding
          CompilationOrigin
          ( "same-scope rebinding: '"
              <> variableName
              <> "' shadows previous same-scope binding (last declaration wins)"
          )

sortWarnings :: [Diagnostic] -> [Diagnostic]
sortWarnings =
  sortOn
    ( \diagnostic ->
        ( diagnosticPrimarySpan diagnostic,
          diagnosticWarningCategory diagnostic,
          diagnosticSubject diagnostic
        )
    )
