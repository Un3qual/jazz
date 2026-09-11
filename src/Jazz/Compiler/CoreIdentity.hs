{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Declaration and reference identities shared from resolution onwards.
module Jazz.Compiler.CoreIdentity
  ( CoreNodeId (..),
    CoreBinderId (..),
    CapabilityId (..),
    CapabilityMethodKey,
    capabilityMethodKeyFromReference,
    renderCapabilityId,
    renderCapabilityMethodKey,
    ImplId (..),
    MethodId (..),
    ResolvedReference (..),
    ResolvedNodeFacts (..),
    ResolvedScopeFacts (..),
    emptyResolvedNodeFacts,
    resolvedBinderReference,
    resolvedValueReference,
    resolvedImportTarget,
  )
where

import Control.DeepSeq (NFData)
import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Text (Text)
import GHC.Generics (Generic)
import Jazz.Compiler.ModuleIdentity (ModulePath, SourceUnitOwner)
import Jazz.Compiler.Name (Identifier, ResolvedName, identifierText)

newtype CoreNodeId = CoreNodeId Int
  deriving stock (Eq, Generic, Ord, Show)
  deriving newtype (Enum)
  deriving anyclass (NFData)

newtype CoreBinderId = CoreBinderId (SourceUnitOwner, CoreNodeId)
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

newtype CapabilityId = CapabilityId {capabilityResolvedName :: ResolvedName}
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

type CapabilityMethodKey = (CapabilityId, Identifier)

capabilityMethodKeyFromReference :: ResolvedReference -> Maybe CapabilityMethodKey
capabilityMethodKeyFromReference reference = case reference of
  CapabilityMethodReference capability method -> Just (capability, method)
  _ -> Nothing

renderCapabilityId :: CapabilityId -> Text
renderCapabilityId = identifierText . capabilityResolvedName

renderCapabilityMethodKey :: CapabilityMethodKey -> Text
renderCapabilityMethodKey (capability, method) = renderCapabilityId capability <> "::" <> identifierText method

newtype ImplId = ImplId (SourceUnitOwner, CoreNodeId)
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

newtype MethodId = MethodId (ImplId, Identifier)
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

data ResolvedReference
  = LexicalReference CoreBinderId
  | BuiltinReference Identifier
  | BuiltinOperatorReference Text
  | CapabilityMethodReference CapabilityId Identifier
  | UnresolvedReference ResolvedName
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

-- | Facts produced with each node, never a second symbol table carried beside
-- the tree. Unresolved references survive only to preserve inference diagnostics.
data ResolvedNodeFacts = ResolvedNodeFacts
  { resolvedNodeOwner :: SourceUnitOwner,
    resolvedNodeBinder :: Maybe CoreBinderId,
    resolvedNodeShadowedReference :: Maybe ResolvedReference,
    resolvedNodeReference :: Maybe ResolvedReference,
    resolvedNodeImportTarget :: Maybe ModulePath,
    resolvedNodeScope :: Maybe ResolvedScopeFacts,
    resolvedNodeCaptures :: [(ResolvedReference, ResolvedName)]
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

emptyResolvedNodeFacts :: SourceUnitOwner -> ResolvedNodeFacts
emptyResolvedNodeFacts owner = ResolvedNodeFacts owner Nothing Nothing Nothing Nothing Nothing []

resolvedBinderReference :: ResolvedNodeFacts -> ResolvedReference
resolvedBinderReference facts = case resolvedNodeBinder facts of
  Just binder -> LexicalReference binder
  Nothing -> error "declaration has no resolved binder identity"

resolvedValueReference :: ResolvedNodeFacts -> ResolvedReference
resolvedValueReference facts = case resolvedNodeReference facts of
  Just reference -> reference
  Nothing -> error "value use has no resolved reference"

resolvedImportTarget :: ResolvedNodeFacts -> ModulePath
resolvedImportTarget facts = case resolvedNodeImportTarget facts of
  Just target -> target
  Nothing -> error "import has no resolved module target"

-- | Lexical facts for the exact, source-ordered statements of a resolved block.
-- Statement indices are local views; binding identities remain source-owned.
data ResolvedScopeFacts = ResolvedScopeFacts
  { resolvedScopeOuterBindingNames :: Set ResolvedName,
    resolvedScopeBindingNames :: Map Int ResolvedName,
    resolvedScopeBinderIds :: Map Int CoreBinderId,
    resolvedScopeBindingReplacements :: Map Int Int,
    resolvedScopeRecursiveGroups :: Map Int [Int],
    resolvedScopeSelfRecursiveFunctions :: Set Int,
    resolvedScopeSelfReferences :: Set Int
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)
