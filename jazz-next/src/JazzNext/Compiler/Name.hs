{-# LANGUAGE OverloadedStrings #-}

-- | Structured names used after surface syntax is lowered into the core AST.
module JazzNext.Compiler.Name
  ( GeneratedNameKind (..),
    Name (..),
    NameNamespace (..),
    ResolvedNameOrigin (..),
    builtinName,
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
    sourceName
  ) where

import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text as Text
import JazzNext.Compiler.Identifier
  ( Identifier,
    IdentifierLike (..),
    operatorBindingIdentifierText
  )
import JazzNext.Compiler.Purity (Purity (..))

data NameNamespace
  = ValueNamespace
  | ConstructorNamespace
  | TypeNamespace
  | CapabilityNamespace
  deriving (Eq, Ord, Show)

data ResolvedNameOrigin
  = CurrentModule
  | ImportedModule [Text]
  | AmbientPrelude
  deriving (Eq, Ord, Show)

-- | `OperatorBinding` retains the canonical hidden storage spelling until the
-- parser surface grows a dedicated operator-binding node.
data GeneratedNameKind
  = LambdaPatternArgument Int
  | OperatorBinding Text
  | OperatorSectionFunction
  | OperatorSectionLeft
  | OperatorSectionRight
  | ModuleReplayBridge [Text] NameNamespace Text
  deriving (Eq, Ord, Show)

data Name
  = SourceName Identifier
  | QualifiedName Identifier Identifier
  | ResolvedName ResolvedNameOrigin NameNamespace Identifier
  | BuiltinName Identifier
  | GeneratedName GeneratedNameKind
  deriving (Eq, Ord, Show)

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
    (GeneratedName (ModuleReplayBridge modulePath _ qualifierName), SourceName memberIdentifier) ->
      GeneratedName
        ( ModuleReplayBridge
            modulePath
            ValueNamespace
            (qualifierName <> "::" <> identifierText memberIdentifier)
        )
    _ ->
      SourceName (fromString (Text.unpack (renderName qualifier <> "::" <> renderName member)))

resolvedLocalName :: NameNamespace -> Identifier -> Name
resolvedLocalName = ResolvedName CurrentModule

resolvedImportedName :: [Text] -> NameNamespace -> Identifier -> Name
resolvedImportedName modulePath = ResolvedName (ImportedModule modulePath)

resolvedAmbientName :: NameNamespace -> Identifier -> Name
resolvedAmbientName = ResolvedName AmbientPrelude

builtinName :: Identifier -> Name
builtinName = BuiltinName

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
    GeneratedName (ModuleReplayBridge modulePath _ exportedName) ->
      Text.intercalate "::" ("__module" : modulePath ++ [exportedName])
    GeneratedName generated -> "<generated:" <> Text.pack (show generated) <> ">"

namePurity :: Name -> Purity
namePurity name =
  case name of
    SourceName identifier -> identifierPurity identifier
    QualifiedName _ member -> identifierPurity member
    ResolvedName _ _ member -> identifierPurity member
    BuiltinName identifier -> identifierPurity identifier
    GeneratedName _ -> Pure
