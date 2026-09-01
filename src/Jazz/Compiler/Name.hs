{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | Phase-safe names used by canonical core.
module Jazz.Compiler.Name
  ( Identifier,
    IdentifierLike (..),
    isIdentifierContinuationCharacter,
    isIdentifierStartCharacter,
    mkIdentifier,
    mkOperatorBindingIdentifier,
    mkQualifiedIdentifier,
    isOperatorBindingIdentifierText,
    operatorBindingIdentifierText,
    qualifiedIdentifierText,
    splitQualifiedIdentifierText,
    GeneratedNameKind (..),
    Name (..),
    SourceName (..),
    ResolvedUserName (..),
    UnresolvedName,
    ResolvedName,
    NameNamespace (..),
    ResolvedNameOrigin (..),
    UserNameLike (..),
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
    resolvedValueScopeName,
    sourceName,
  )
where

import Control.DeepSeq (NFData)
import Data.Char (ord, toUpper)
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import Jazz.Compiler.Identifier
  ( Identifier,
    IdentifierLike (..),
    isIdentifierContinuationCharacter,
    isIdentifierStartCharacter,
    mkIdentifier,
  )
import Jazz.Compiler.ModuleIdentity (ModulePath, renderModulePath)
import Jazz.Compiler.Purity (Purity (..))
import Numeric (showHex)

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

data NameNamespace
  = ValueNamespace
  | ConstructorNamespace
  | TypeNamespace
  | CapabilityNamespace
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

data ResolvedNameOrigin
  = CurrentModule
  | ImportedModule ModulePath
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

data SourceName
  = UnqualifiedSourceName Identifier
  | QualifiedSourceName Identifier Identifier
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

data ResolvedUserName = ResolvedUserName ResolvedNameOrigin NameNamespace Identifier
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

-- | User spelling varies by compiler phase. Builtins and compiler-generated
-- names are phase-independent and therefore live outside the user payload.
data Name user
  = UserName user
  | BuiltinName Identifier
  | GeneratedName GeneratedNameKind
  deriving stock (Eq, Foldable, Functor, Generic, Ord, Show, Traversable)
  deriving anyclass (NFData)

type UnresolvedName = Name SourceName

type ResolvedName = Name ResolvedUserName

class UserNameLike user where
  renderUserName :: user -> Text
  userNamePurity :: user -> Purity

instance UserNameLike SourceName where
  renderUserName source =
    case source of
      UnqualifiedSourceName identifier -> identifierText identifier
      QualifiedSourceName qualifier member ->
        identifierText qualifier <> "::" <> identifierText member
  userNamePurity source =
    case source of
      UnqualifiedSourceName identifier -> identifierPurity identifier
      QualifiedSourceName _ member -> identifierPurity member

instance UserNameLike ResolvedUserName where
  renderUserName (ResolvedUserName origin _ member) =
    case origin of
      CurrentModule -> identifierText member
      ImportedModule modulePath -> renderModulePath modulePath <> "::" <> identifierText member
      AmbientPrelude -> identifierText member
  userNamePurity (ResolvedUserName _ _ member) = identifierPurity member

instance IsString UnresolvedName where
  fromString = sourceName . fromString

instance (UserNameLike user) => IdentifierLike (Name user) where
  identifierText = renderName
  identifierPurity = namePurity

sourceName :: Identifier -> UnresolvedName
sourceName = UserName . UnqualifiedSourceName

qualifiedName :: Identifier -> Identifier -> UnresolvedName
qualifiedName qualifier member = UserName (QualifiedSourceName qualifier member)

qualifiedMemberName :: ResolvedName -> ResolvedName -> ResolvedName
qualifiedMemberName qualifier member =
  case (qualifier, member) of
    ( UserName (ResolvedUserName origin CapabilityNamespace qualifierIdentifier),
      UserName (ResolvedUserName _ ValueNamespace memberIdentifier)
      ) ->
        UserName
          ( ResolvedUserName
              origin
              ValueNamespace
              (mkIdentifier (identifierText qualifierIdentifier <> "::" <> identifierText memberIdentifier))
          )
    _ ->
      UserName
        ( ResolvedUserName
            CurrentModule
            ValueNamespace
            (mkIdentifier (renderName qualifier <> "::" <> renderName member))
        )

resolvedLocalName :: NameNamespace -> Identifier -> ResolvedName
resolvedLocalName namespace = UserName . ResolvedUserName CurrentModule namespace

resolvedImportedName :: ModulePath -> NameNamespace -> Identifier -> ResolvedName
resolvedImportedName modulePath namespace = UserName . ResolvedUserName (ImportedModule modulePath) namespace

resolvedAmbientName :: NameNamespace -> Identifier -> ResolvedName
resolvedAmbientName namespace = UserName . ResolvedUserName AmbientPrelude namespace

-- | Key a resolved name in the analyzer's shared value scope. Constructors
-- are values when checking lexical visibility and rebinding; all other names
-- retain their namespace. Builtin and generated names have no user payload,
-- so the lawful 'Functor' traversal leaves them unchanged.
resolvedValueScopeName :: ResolvedName -> ResolvedName
resolvedValueScopeName = fmap enterValueScope
  where
    enterValueScope userName@(ResolvedUserName origin namespace identifier) =
      case namespace of
        ConstructorNamespace -> ResolvedUserName origin ValueNamespace identifier
        _ -> userName

generatedName :: GeneratedNameKind -> Name user
generatedName = GeneratedName

operatorBindingName :: Text -> Name user
operatorBindingName = GeneratedName . OperatorBinding . operatorBindingIdentifierText

operatorBindingNameFromIdentifier :: Identifier -> Name user
operatorBindingNameFromIdentifier = GeneratedName . OperatorBinding . identifierText

renderName :: (UserNameLike user) => Name user -> Text
renderName name =
  case name of
    UserName user -> renderUserName user
    BuiltinName identifier -> identifierText identifier
    GeneratedName (OperatorBinding storageName) -> storageName
    GeneratedName generated -> "<generated:" <> Text.pack (show generated) <> ">"

namePurity :: (UserNameLike user) => Name user -> Purity
namePurity name =
  case name of
    UserName user -> userNamePurity user
    BuiltinName identifier -> identifierPurity identifier
    GeneratedName _ -> Pure
