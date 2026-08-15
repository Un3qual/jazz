{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Structured names used after surface syntax is lowered into the core AST.
module Jazz.Compiler.Name
  ( Identifier,
    IdentifierLike (..),
    mkIdentifier,
    mkOperatorBindingIdentifier,
    mkQualifiedIdentifier,
    isOperatorBindingIdentifierText,
    operatorBindingIdentifierText,
    qualifiedIdentifierText,
    splitQualifiedIdentifierText,
    GeneratedNameKind (..),
    Name (..),
    NameNamespace (..),
    ResolvedNameOrigin (..),
    generatedName,
    namePurity,
    operatorBindingName,
    operatorBindingNameFromIdentifier,
    qualifiedMemberName,
    qualifiedName,
    renderName,
    resolvedAmbientName,
    resolvedImportedName,
    resolvedLocalName,
    sourceName,
  )
where

import Control.DeepSeq (NFData)
import Data.Char
  ( ord,
    toUpper,
  )
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import Jazz.Compiler.Purity (Purity (..))
import qualified Jazz.Compiler.Purity as Purity
import Numeric (showHex)

-- | A source identifier paired with the purity implied by its spelling.
data Identifier = Identifier Text Purity
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

class IdentifierLike name where
  identifierText :: name -> Text
  identifierPurity :: name -> Purity

instance IdentifierLike Identifier where
  identifierText (Identifier name _) = name
  identifierPurity (Identifier _ purity) = purity

mkIdentifier :: Text -> Identifier
mkIdentifier name = Identifier name (Purity.namePurity name)

operatorBindingIdentifierText :: Text -> Text
operatorBindingIdentifierText operatorSymbol =
  operatorBindingIdentifierPrefix <> Text.concatMap encodeOperatorChar operatorSymbol
  where
    encodeOperatorChar char =
      let hexText = Text.pack (map toUpper (showHex (ord char) ""))
       in "%" <> Text.justifyRight 2 '0' hexText

operatorBindingIdentifierPrefix :: Text
operatorBindingIdentifierPrefix = "$operator:"

isOperatorBindingIdentifierText :: Text -> Bool
isOperatorBindingIdentifierText = Text.isPrefixOf operatorBindingIdentifierPrefix

mkOperatorBindingIdentifier :: Text -> Identifier
mkOperatorBindingIdentifier = mkIdentifier . operatorBindingIdentifierText

qualifiedIdentifierText :: Text -> Text -> Text
qualifiedIdentifierText qualifier member = qualifier <> "::" <> member

mkQualifiedIdentifier :: Text -> Text -> Identifier
mkQualifiedIdentifier qualifier member = mkIdentifier (qualifiedIdentifierText qualifier member)

splitQualifiedIdentifierText :: Text -> Maybe (Text, Text)
splitQualifiedIdentifierText name =
  case Text.breakOn "::" name of
    (qualifier, rest)
      | Text.null qualifier -> Nothing
      | Text.null rest -> Nothing
      | Text.null member -> Nothing
      | Text.isInfixOf "::" member -> Nothing
      | otherwise -> Just (qualifier, member)
      where
        member = Text.drop 2 rest

instance IsString Identifier where
  fromString = mkIdentifier . Text.pack

data NameNamespace
  = ValueNamespace
  | ConstructorNamespace
  | TypeNamespace
  | CapabilityNamespace
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

data ResolvedNameOrigin
  = CurrentModule
  | ImportedModule [Text]
  | AmbientPrelude
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

-- | `OperatorBinding` retains the canonical hidden storage spelling until the
-- parser surface grows a dedicated operator-binding node.
data GeneratedNameKind
  = LambdaPatternArgument Int
  | OperatorBinding Text
  | OperatorSectionFunction
  | OperatorSectionLeft
  | OperatorSectionRight
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

data Name
  = SourceName Identifier
  | QualifiedName Identifier Identifier
  | ResolvedName ResolvedNameOrigin NameNamespace Identifier
  | BuiltinName Identifier
  | GeneratedName GeneratedNameKind
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

instance IsString Name where
  fromString = SourceName . fromString

instance IdentifierLike Name where
  identifierText = renderName
  identifierPurity = namePurity

sourceName :: Identifier -> Name
sourceName = SourceName

qualifiedName :: Identifier -> Identifier -> Name
qualifiedName = QualifiedName

qualifiedMemberName :: Name -> Name -> Name
qualifiedMemberName qualifier member =
  case (qualifier, member) of
    (SourceName qualifierIdentifier, SourceName memberIdentifier) ->
      QualifiedName qualifierIdentifier memberIdentifier
    (ResolvedName origin CapabilityNamespace qualifierIdentifier, ResolvedName _ ValueNamespace memberIdentifier) ->
      ResolvedName
        origin
        ValueNamespace
        (mkIdentifier (identifierText qualifierIdentifier <> "::" <> identifierText memberIdentifier))
    _ ->
      SourceName (mkIdentifier (renderName qualifier <> "::" <> renderName member))

resolvedLocalName :: NameNamespace -> Identifier -> Name
resolvedLocalName = ResolvedName CurrentModule

resolvedImportedName :: [Text] -> NameNamespace -> Identifier -> Name
resolvedImportedName modulePath = ResolvedName (ImportedModule modulePath)

resolvedAmbientName :: NameNamespace -> Identifier -> Name
resolvedAmbientName = ResolvedName AmbientPrelude

generatedName :: GeneratedNameKind -> Name
generatedName = GeneratedName

operatorBindingName :: Text -> Name
operatorBindingName = GeneratedName . OperatorBinding . operatorBindingIdentifierText

operatorBindingNameFromIdentifier :: Identifier -> Name
operatorBindingNameFromIdentifier = GeneratedName . OperatorBinding . identifierText

renderName :: Name -> Text
renderName name =
  case name of
    SourceName identifier -> identifierText identifier
    QualifiedName qualifier member ->
      identifierText qualifier <> "::" <> identifierText member
    ResolvedName CurrentModule _ member -> identifierText member
    ResolvedName (ImportedModule modulePath) _ member ->
      Text.intercalate "::" (modulePath ++ [identifierText member])
    ResolvedName AmbientPrelude _ member -> identifierText member
    BuiltinName identifier -> identifierText identifier
    GeneratedName (OperatorBinding storageName) -> storageName
    GeneratedName generated -> "<generated:" <> Text.pack (show generated) <> ">"

namePurity :: Name -> Purity
namePurity name =
  case name of
    SourceName identifier -> identifierPurity identifier
    QualifiedName _ member -> identifierPurity member
    ResolvedName _ _ member -> identifierPurity member
    BuiltinName identifier -> identifierPurity identifier
    GeneratedName _ -> Pure
