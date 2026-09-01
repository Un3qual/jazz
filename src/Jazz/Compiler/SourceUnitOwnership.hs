{-# LANGUAGE DataKinds #-}

-- | One source unit may contain an injected prelude followed by an authored
-- module declaration. This fold is the single ownership transition used by
-- analysis, evidence identity, runtime-plan projection, and the interpreter.
module Jazz.Compiler.SourceUnitOwnership
  ( SourceUnitOwner,
    sourceUnitOwnerModulePath,
    sourceUnitOwnerRuntimePath,
    sourceUnitStatementRuntimePaths,
    sourceUnitStatementOwners,
  )
where

import Data.List (mapAccumL)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Jazz.Compiler.AST (Statement (..))
import Jazz.Compiler.ModuleIdentity (ModulePath, mkModulePath, modulePathTextSegments)
import Jazz.Compiler.Name (mkIdentifier)

data SourceUnitOwner
  = StandaloneSourceUnit ModulePath
  | NamedSourceUnit ModulePath

sourceUnitOwnerModulePath :: SourceUnitOwner -> ModulePath
sourceUnitOwnerModulePath owner =
  case owner of
    StandaloneSourceUnit path -> path
    NamedSourceUnit path -> path

sourceUnitOwnerRuntimePath :: SourceUnitOwner -> Maybe [Text]
sourceUnitOwnerRuntimePath owner =
  case owner of
    StandaloneSourceUnit _ -> Nothing
    NamedSourceUnit path -> Just (NonEmpty.toList (modulePathTextSegments path))

sourceUnitStatementOwners ::
  ModulePath ->
  ModulePath ->
  Set Int ->
  [Statement phase] ->
  [SourceUnitOwner]
sourceUnitStatementOwners sourcePath preludePath preludeStatementIndices statements =
  statementOwners
    (StandaloneSourceUnit sourcePath)
    (NamedSourceUnit preludePath)
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
  Maybe [Text] ->
  [Statement phase] ->
  [Maybe [Text]]
sourceUnitStatementRuntimePaths preludePath preludeStatementIndices initialModulePath =
  statementOwners
    initialModulePath
    (Just (NonEmpty.toList (modulePathTextSegments preludePath)))
    declaredPath
    preludeStatementIndices
  where
    declaredPath statement =
      case statement of
        SModule _ modulePathSegments -> Just (Just modulePathSegments)
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
