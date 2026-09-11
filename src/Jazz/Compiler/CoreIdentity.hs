{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Declaration and reference identities shared from resolution onwards.
module Jazz.Compiler.CoreIdentity
  ( CoreNodeId (..),
    CoreBinderId (..),
    CapabilityId (..),
    ImplId (..),
    MethodId (..),
    ResolvedReference (..),
    ResolvedNodeFacts (..),
    emptyResolvedNodeFacts,
  )
where

import Control.DeepSeq (NFData)
import Data.Text (Text)
import GHC.Generics (Generic)
import Jazz.Compiler.ModuleIdentity (SourceUnitOwner)
import Jazz.Compiler.Name (Identifier, ResolvedName)

newtype CoreNodeId = CoreNodeId Int
  deriving stock (Eq, Generic, Ord, Show)
  deriving newtype (Enum)
  deriving anyclass (NFData)

newtype CoreBinderId = CoreBinderId (SourceUnitOwner, CoreNodeId)
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

newtype CapabilityId = CapabilityId ResolvedName
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

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
    resolvedNodeReference :: Maybe ResolvedReference
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

emptyResolvedNodeFacts :: SourceUnitOwner -> ResolvedNodeFacts
emptyResolvedNodeFacts owner = ResolvedNodeFacts owner Nothing Nothing
