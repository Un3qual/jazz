{-# LANGUAGE DataKinds #-}

-- | One source unit may contain an injected prelude followed by an authored
-- module declaration. This fold is the single ownership transition used by
-- analysis, evidence identity, runtime-plan projection, and the interpreter.
module Jazz.Compiler.SourceUnitOwnership
  ( SourceUnitOwner (..),
    sourceUnitOwnerOrigin,
    sourceUnitOwnerModulePath,
    sourceUnitStatementRuntimePaths,
    sourceUnitStatementOwners,
  )
where

import Data.List (mapAccumL)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Set (Set)
import qualified Data.Set as Set
import Jazz.Compiler.AST (Statement (..))
import Jazz.Compiler.ModuleIdentity (ModulePath, mkModulePath)
import Jazz.Compiler.Name (ResolvedNameOrigin (..), mkIdentifier)

data SourceUnitOwner
  = StandaloneSourceUnit ModulePath
  | NamedSourceUnit ModulePath
  | PreludeSourceUnit ModulePath
  deriving (Eq, Show)

sourceUnitOwnerModulePath :: SourceUnitOwner -> ModulePath
sourceUnitOwnerModulePath owner =
  case owner of
    StandaloneSourceUnit path -> path
    NamedSourceUnit path -> path
    PreludeSourceUnit path -> path

sourceUnitOwnerOrigin :: SourceUnitOwner -> ResolvedNameOrigin
sourceUnitOwnerOrigin owner =
  case owner of
    StandaloneSourceUnit _ -> CurrentModule
    NamedSourceUnit path -> ImportedModule path
    PreludeSourceUnit _ -> AmbientPrelude

sourceUnitStatementOwners ::
  ModulePath ->
  ModulePath ->
  Set Int ->
  [Statement phase] ->
  [SourceUnitOwner]
sourceUnitStatementOwners sourcePath preludePath preludeStatementIndices statements =
  statementOwners
    (StandaloneSourceUnit sourcePath)
    (PreludeSourceUnit preludePath)
    namedOwner
    preludeStatementIndices
    statements
  where
    namedOwner statement =
      case statement of
        SModule _ modulePathSegments ->
          NamedSourceUnit . mkModulePath . fmap mkIdentifier
            <$> NonEmpty.nonEmpty modulePathSegments
        _ -> Nothing

sourceUnitStatementRuntimePaths ::
  ModulePath ->
  Set Int ->
  Maybe SourceUnitOwner ->
  [Statement phase] ->
  [Maybe SourceUnitOwner]
sourceUnitStatementRuntimePaths preludePath preludeStatementIndices initialModulePath =
  statementOwners
    initialModulePath
    (Just (PreludeSourceUnit preludePath))
    declaredPath
    preludeStatementIndices
  where
    declaredPath statement =
      case statement of
        SModule _ modulePathSegments ->
          Just . NamedSourceUnit . mkModulePath . fmap mkIdentifier
            <$> NonEmpty.nonEmpty modulePathSegments
        _ -> Nothing

statementOwners ::
  owner ->
  owner ->
  (Statement phase -> Maybe owner) ->
  Set Int ->
  [Statement phase] ->
  [owner]
statementOwners initialOwner preludeOwner declaredOwner preludeStatementIndices statements =
  snd (mapAccumL step initialOwner (zip [0 :: Int ..] statements))
  where
    step activeOwner (statementIndex, statement) =
      (nextOwner, statementOwner)
      where
        nextOwner = maybe activeOwner id (declaredOwner statement)
        statementOwner
          | Set.member statementIndex preludeStatementIndices = preludeOwner
          | otherwise = nextOwner
