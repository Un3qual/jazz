{-# LANGUAGE OverloadedStrings #-}

-- | Stable metadata for every published Jazz diagnostic code.
module JazzNext.Compiler.DiagnosticCatalog
  ( DiagnosticCode,
    DiagnosticMetadata (..),
    DiagnosticSeverity (..),
    DiagnosticSubsystem (..),
    ErrorCode (..),
    WarningCategory (..),
    allDiagnosticMetadata,
    allWarningCategories,
    diagnosticCodeText,
    lookupWarningCategory,
    warningCode,
    warningHasAnalyzerEmitter,
    warningToken
  ) where

import Data.Text (Text)
import qualified Data.Text as Text

-- | Published native-error identities. Constructor names deliberately match
-- the stable user-facing spelling while 'DiagnosticCode' keeps the sum of
-- error and warning identities opaque.
data ErrorCode
  = E0001
  | E0002
  | E0003
  | E0004
  | E0005
  | E1001
  | E1002
  | E1003
  | E1004
  | E1005
  | E1006
  | E1007
  | E1010
  | E2001
  | E2002
  | E2003
  | E2004
  | E2005
  | E2006
  | E2007
  | E2008
  | E2009
  | E2010
  | E2011
  | E2012
  | E2013
  | E2014
  | E2015
  | E2016
  | E2017
  | E3001
  | E3002
  | E3003
  | E3006
  | E3007
  | E3008
  | E3009
  | E3010
  | E3011
  | E3012
  | E3013
  | E3014
  | E3015
  | E3016
  | E3017
  | E3018
  | E3019
  | E3020
  | E3021
  | E3022
  | E3023
  | E3024
  | E3025
  | E3026
  | E3027
  | E3028
  | E3029
  | E3030
  | E3031
  | E3032
  | E3033
  | E3034
  | E3035
  | E3036
  | E3037
  | E3038
  | E3039
  | E4001
  | E4002
  | E4003
  | E4004
  | E4005
  | E4006
  | E4007
  | E4008
  | E4009
  | E4010
  | E4011
  | E4012
  | E4013
  | E4014
  | E4015
  deriving (Bounded, Enum, Eq, Ord, Show)

-- | User-visible warning families. Tokens and codes are compatibility
-- contracts once published.
data WarningCategory
  = SameScopeRebinding
  | ShadowingOuterScope
  | UnusedBinding
  | DeprecatedSyntax
  deriving (Bounded, Enum, Eq, Ord, Show)

-- | Effective severity used by the shared diagnostic model.
data DiagnosticSeverity
  = SeverityWarning
  | SeverityError
  deriving (Eq, Ord, Show)

-- | Broad catalog grouping. These groups remain independent of any future
-- phase-specific diagnostic payload types.
data DiagnosticSubsystem
  = SyntaxDiagnostics
  | AnalysisDiagnostics
  | TypeDiagnostics
  | RuntimeDiagnostics
  | ModuleDiagnostics
  | ToolingDiagnostics
  deriving (Eq, Ord, Show)

-- | Opaque union of native-error and warning identities.
data DiagnosticCode
  = NativeErrorCode ErrorCode
  | ConfigurableWarningCode WarningCategory
  deriving (Eq, Ord, Show)

-- | Published catalog metadata. Warning-only fields are absent for native
-- errors rather than represented by sentinel values.
data DiagnosticMetadata = DiagnosticMetadata
  { metadataCode :: DiagnosticCode,
    metadataDefaultSeverity :: DiagnosticSeverity,
    metadataSubsystem :: DiagnosticSubsystem,
    metadataWarningCategory :: Maybe WarningCategory,
    metadataWarningToken :: Maybe Text,
    metadataHasAnalyzerEmitter :: Maybe Bool
  }
  deriving (Eq, Show)

-- | Deterministic inventory used by help, validation, and repository audits.
allDiagnosticMetadata :: [DiagnosticMetadata]
allDiagnosticMetadata =
  map errorMetadata [minBound .. maxBound]
    <> map warningMetadata allWarningCategories

allWarningCategories :: [WarningCategory]
allWarningCategories = [minBound .. maxBound]

diagnosticCodeText :: DiagnosticCode -> Text
diagnosticCodeText code =
  case code of
    NativeErrorCode errorCode -> Text.pack (show errorCode)
    ConfigurableWarningCode category -> formatCode 'W' (fromEnum category + 1)

warningCode :: WarningCategory -> DiagnosticCode
warningCode = ConfigurableWarningCode

warningToken :: WarningCategory -> Text
warningToken category =
  case category of
    SameScopeRebinding -> "same-scope-rebinding"
    ShadowingOuterScope -> "shadowing-outer-scope"
    UnusedBinding -> "unused-binding"
    DeprecatedSyntax -> "deprecated-syntax"

warningHasAnalyzerEmitter :: WarningCategory -> Bool
warningHasAnalyzerEmitter category =
  case category of
    SameScopeRebinding -> True
    ShadowingOuterScope -> True
    UnusedBinding -> True
    DeprecatedSyntax -> False

-- | Case-insensitive warning token lookup shared by CLI, environment, and file
-- configuration parsing.
lookupWarningCategory :: Text -> Maybe WarningCategory
lookupWarningCategory rawToken =
  lookup normalizedToken
    [ (warningToken category, category)
      | category <- allWarningCategories
    ]
  where
    normalizedToken = Text.toLower (Text.strip rawToken)

errorMetadata :: ErrorCode -> DiagnosticMetadata
errorMetadata code =
  DiagnosticMetadata
    { metadataCode = NativeErrorCode code,
      metadataDefaultSeverity = SeverityError,
      metadataSubsystem = errorSubsystem code,
      metadataWarningCategory = Nothing,
      metadataWarningToken = Nothing,
      metadataHasAnalyzerEmitter = Nothing
    }

warningMetadata :: WarningCategory -> DiagnosticMetadata
warningMetadata category =
  DiagnosticMetadata
    { metadataCode = warningCode category,
      metadataDefaultSeverity = SeverityWarning,
      metadataSubsystem = warningSubsystem category,
      metadataWarningCategory = Just category,
      metadataWarningToken = Just (warningToken category),
      metadataHasAnalyzerEmitter = Just (warningHasAnalyzerEmitter category)
    }

errorSubsystem :: ErrorCode -> DiagnosticSubsystem
errorSubsystem code
  | code <= E0005 = SyntaxDiagnostics
  | code <= E1010 = AnalysisDiagnostics
  | code <= E2017 = TypeDiagnostics
  | code <= E3039 = RuntimeDiagnostics
  | otherwise = ModuleDiagnostics

warningSubsystem :: WarningCategory -> DiagnosticSubsystem
warningSubsystem category =
  case category of
    DeprecatedSyntax -> SyntaxDiagnostics
    _ -> AnalysisDiagnostics

formatCode :: Char -> Int -> Text
formatCode prefix number =
  Text.singleton prefix
    <> Text.justifyRight 4 '0' (Text.pack (show number))
