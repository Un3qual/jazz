-- | Source-unit ownership and its name-resolution origin.
module Jazz.Compiler.SourceUnitOwnership
  ( SourceUnitOwner (..),
    sourceUnitOwnerOrigin,
    sourceUnitOwnerModulePath,
  )
where

import Jazz.Compiler.ModuleIdentity (SourceUnitOwner (..), sourceUnitOwnerModulePath)
import Jazz.Compiler.Name (ResolvedNameOrigin (..))

sourceUnitOwnerOrigin :: SourceUnitOwner -> ResolvedNameOrigin
sourceUnitOwnerOrigin owner = case owner of
  StandaloneSourceUnit _ -> CurrentModule
  NamedSourceUnit path -> ImportedModule path
  PreludeSourceUnit _ -> AmbientPrelude
