{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Nominal identities shared by module discovery and name resolution.
module Jazz.Compiler.ModuleIdentity
  ( SourceUnitOwner (..),
    sourceUnitOwnerModulePath,
    ModulePath,
    SourceFile,
    ModuleQualifier,
    ModuleIdentity,
    mkModulePath,
    mkModuleQualifier,
    mkSourceFile,
    sourceFilePath,
    moduleIdentity,
    moduleIdentityPath,
    moduleIdentitySource,
    moduleQualifierIdentifier,
    preludeModulePath,
    standaloneModulePath,
    standaloneSourceFile,
    parseModulePathText,
    modulePathSegments,
    modulePathTextSegments,
    renderModulePath,
    modulePathRelativeFile,
  )
where

import Control.DeepSeq (NFData)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import Jazz.Compiler.DiagnosticCatalog (ErrorCode (E4016))
import Jazz.Compiler.Diagnostics
  ( Diagnostic,
    DiagnosticOrigin (CompilationOrigin),
    mkErrorDiagnostic,
  )
import Jazz.Compiler.Identifier
  ( Identifier,
    IdentifierLike (identifierText),
    isIdentifierContinuationCharacter,
    isIdentifierStartCharacter,
    mkIdentifier,
  )
import System.FilePath (joinPath)

newtype ModulePath = ModulePath (NonEmpty Identifier)
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

-- | Distinguish the ambient prelude from an ordinary module with the same path.
data SourceUnitOwner
  = StandaloneSourceUnit ModulePath
  | NamedSourceUnit ModulePath
  | PreludeSourceUnit ModulePath
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

sourceUnitOwnerModulePath :: SourceUnitOwner -> ModulePath
sourceUnitOwnerModulePath owner =
  case owner of
    StandaloneSourceUnit path -> path
    NamedSourceUnit path -> path
    PreludeSourceUnit path -> path

newtype SourceFile = SourceFile FilePath
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

newtype ModuleQualifier = ModuleQualifier Identifier
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

data ModuleIdentity = ModuleIdentity
  { storedModuleIdentityPath :: ModulePath,
    storedModuleIdentitySource :: SourceFile
  }
  deriving stock (Eq, Generic, Ord)
  deriving anyclass (NFData)

instance Show ModuleIdentity where
  showsPrec precedence identity =
    showParen (precedence > 10) $
      showString "ModuleIdentity {moduleIdentityPath = "
        . shows (moduleIdentityPath identity)
        . showString ", moduleIdentitySource = "
        . shows (moduleIdentitySource identity)
        . showChar '}'

mkModulePath :: NonEmpty Identifier -> ModulePath
mkModulePath = ModulePath

mkModuleQualifier :: Identifier -> ModuleQualifier
mkModuleQualifier = ModuleQualifier

moduleQualifierIdentifier :: ModuleQualifier -> Identifier
moduleQualifierIdentifier (ModuleQualifier identifier) = identifier

-- | The single nominal path shared by bundled, explicit, and absent prelude
-- artifacts and by standalone/module inference and runtime ownership.
preludeModulePath :: ModulePath
preludeModulePath = mkModulePath (NonEmpty.singleton (mkIdentifier "Prelude"))

-- | Nominal owner for declarations compiled from a source without a module
-- header. This is a real non-empty identity, not a sentinel encoding.
standaloneModulePath :: ModulePath
standaloneModulePath = mkModulePath (NonEmpty.singleton (mkIdentifier "Standalone"))

mkSourceFile :: FilePath -> SourceFile
mkSourceFile = SourceFile

sourceFilePath :: SourceFile -> FilePath
sourceFilePath (SourceFile path) = path

moduleIdentity :: ModulePath -> SourceFile -> ModuleIdentity
moduleIdentity = ModuleIdentity

moduleIdentityPath :: ModuleIdentity -> ModulePath
moduleIdentityPath = storedModuleIdentityPath

moduleIdentitySource :: ModuleIdentity -> SourceFile
moduleIdentitySource = storedModuleIdentitySource

parseModulePathText :: Text -> Either Diagnostic ModulePath
parseModulePathText rawModulePath
  | Text.null rawModulePath =
      Left (mkErrorDiagnostic E4016 CompilationOrigin "entry module path cannot be empty")
  | any Text.null segments =
      Left
        ( mkErrorDiagnostic
            E4016
            CompilationOrigin
            ( "invalid entry module path '"
                <> rawModulePath
                <> "': empty path segment"
            )
        )
  | not (all isValidSegment segments) =
      Left
        ( mkErrorDiagnostic
            E4016
            CompilationOrigin
            ( "invalid entry module path '"
                <> rawModulePath
                <> "': segments must be identifiers"
            )
        )
  | otherwise =
      case NonEmpty.nonEmpty segments of
        Just nonEmptySegments -> Right (ModulePath (fmap mkIdentifier nonEmptySegments))
        Nothing -> Left (mkErrorDiagnostic E4016 CompilationOrigin "entry module path cannot be empty")
  where
    segments = Text.splitOn "::" rawModulePath

    isValidSegment segment =
      case Text.uncons segment of
        Nothing -> False
        Just (firstChar, restChars) ->
          isIdentifierStartCharacter firstChar && Text.all isIdentifierContinuationCharacter restChars

modulePathSegments :: ModulePath -> NonEmpty Identifier
modulePathSegments (ModulePath segments) = segments

modulePathTextSegments :: ModulePath -> NonEmpty Text
modulePathTextSegments = fmap identifierText . modulePathSegments

renderModulePath :: ModulePath -> Text
renderModulePath = Text.intercalate "::" . NonEmpty.toList . modulePathTextSegments

modulePathRelativeFile :: String -> ModulePath -> FilePath
modulePathRelativeFile extension =
  (<> extension)
    . joinPath
    . map (Text.unpack . identifierText)
    . NonEmpty.toList
    . modulePathSegments

-- | Synthetic identity for an in-memory source artifact.
standaloneSourceFile :: SourceFile
standaloneSourceFile = mkSourceFile "<standalone>"
