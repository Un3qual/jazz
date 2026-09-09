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
import Data.Maybe (listToMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Jazz.Compiler.AST (Statement (..))
import Jazz.Compiler.ModuleIdentity (ModulePath, SourceUnitOwner (..), mkModulePath, sourceUnitOwnerModulePath)
import Jazz.Compiler.Name (ResolvedNameOrigin (..), mkIdentifier)

sourceUnitOwnerOrigin :: SourceUnitOwner -> ResolvedNameOrigin
sourceUnitOwnerOrigin owner =
  case owner of
    StandaloneSourceUnit _ -> CurrentModule
    NamedSourceUnit path -> ImportedModule path
    PreludeSourceUnit _ -> AmbientPrelude
    InjectedPreludeSourceUnit _ destination -> maybe CurrentModule ImportedModule destination

sourceUnitStatementOwners ::
  ModulePath ->
  ModulePath ->
  Set Int ->
  [Statement phase] ->
  [SourceUnitOwner]
sourceUnitStatementOwners sourcePath preludePath preludeStatementIndices statements =
  statementOwners
    (StandaloneSourceUnit sourcePath)
    (injectedPreludeOwner preludePath statements)
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
sourceUnitStatementRuntimePaths preludePath preludeStatementIndices initialModulePath statements =
  statementOwners
    initialModulePath
    (Just (injectedPreludeOwner preludePath statements))
    declaredPath
    preludeStatementIndices
    statements
  where
    declaredPath statement =
      case statement of
        SModule _ modulePathSegments ->
          Just . NamedSourceUnit . mkModulePath . fmap mkIdentifier
            <$> NonEmpty.nonEmpty modulePathSegments
        _ -> Nothing

-- Injected declarations are resolved with the authored source, so their runtime
-- names must use its module origin even though their evidence belongs to Prelude.
injectedPreludeOwner :: ModulePath -> [Statement phase] -> SourceUnitOwner
injectedPreludeOwner preludePath statements =
  InjectedPreludeSourceUnit preludePath $
    listToMaybe
      [ mkModulePath (fmap mkIdentifier segments)
      | SModule _ path <- statements,
        Just segments <- [NonEmpty.nonEmpty path]
      ]

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
