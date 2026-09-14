{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
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
  )
where

import Control.DeepSeq (NFData)
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Text (Text)
import GHC.Generics (Generic)
import Jazz.Compiler.ModuleIdentity (ModulePath, SourceUnitOwner)
import Jazz.Compiler.Name (Identifier, ResolvedName, identifierText)

newtype CoreNodeId = CoreNodeId Int
  deriving stock (Eq, Generic, Ord, Show)
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
  | CapabilityMethodReference CapabilityId Identifier
  | DefaultMethodReference CapabilityId Identifier
  | ImplementationMethodReference MethodId
  | EvidenceParameterReference CoreBinderId Int
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
    -- | Authored token retained for diagnostics after operator normalization.
    resolvedOperatorSpelling :: Maybe Text,
    resolvedNodeImportTarget :: Maybe ModulePath,
    resolvedNodeScope :: Maybe ResolvedScopeFacts,
    resolvedNodeCaptures :: [(ResolvedReference, ResolvedName)]
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

emptyResolvedNodeFacts :: SourceUnitOwner -> ResolvedNodeFacts
emptyResolvedNodeFacts owner =
  ResolvedNodeFacts
    { resolvedNodeOwner = owner,
      resolvedNodeBinder = Nothing,
      resolvedNodeShadowedReference = Nothing,
      resolvedNodeReference = Nothing,
      resolvedOperatorSpelling = Nothing,
      resolvedNodeImportTarget = Nothing,
      resolvedNodeScope = Nothing,
      resolvedNodeCaptures = []
    }

-- Missing facts retain an unresolved name during diagnostic recovery. The
-- checked-tree boundary rejects these nodes before they can become executable.
resolvedBinderReference :: ResolvedNodeFacts -> ResolvedName -> ResolvedReference
resolvedBinderReference facts name = maybe (UnresolvedReference name) LexicalReference (resolvedNodeBinder facts)

resolvedValueReference :: ResolvedNodeFacts -> ResolvedName -> ResolvedReference
resolvedValueReference facts name = fromMaybe (UnresolvedReference name) (resolvedNodeReference facts)

-- | Lexical facts for the exact, source-ordered statements of a resolved block.
-- Statement indices are local views; binding identities remain source-owned.
data ResolvedScopeFacts = ResolvedScopeFacts
  { resolvedScopeBindingNames :: Map Int ResolvedName,
    resolvedScopeBinderIds :: Map Int CoreBinderId,
    resolvedScopeBindingReplacements :: Map Int Int,
    resolvedScopeRecursiveGroups :: Map Int [Int],
    resolvedScopeSelfRecursiveFunctions :: Set Int,
    resolvedScopeSelfReferences :: Set Int
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)
