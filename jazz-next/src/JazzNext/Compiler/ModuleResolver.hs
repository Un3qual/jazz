{-# LANGUAGE OverloadedStrings #-}

module JazzNext.Compiler.ModuleResolver
  ( ModuleResolutionConfig (..),
    ResolvedModule (..),
    modulePathToRelativeFile,
    resolveModuleGraph
  ) where

import Control.Monad (foldM)
import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as Text
import JazzNext.Compiler.Parser
  ( parseSurfaceProgram
  )
import JazzNext.Compiler.Parser.AST
  ( SurfaceExpr (..),
    SurfaceStatement (..)
  )
import System.FilePath ((</>))

data ModuleResolutionConfig = ModuleResolutionConfig
  { moduleRoots :: [FilePath],
    moduleExtension :: String
  }
  deriving (Eq, Show)

data ResolvedModule = ResolvedModule
  { resolvedModulePath :: [Text],
    resolvedSourcePath :: FilePath,
    resolvedImports :: [[Text]]
  }
  deriving (Eq, Show)

modulePathToRelativeFile :: [Text] -> FilePath
modulePathToRelativeFile = modulePathToRelativeFileWithExt ".jz"

resolveModuleGraph ::
  ModuleResolutionConfig ->
  Map FilePath Text ->
  [Text] ->
  Either Text [ResolvedModule]
resolveModuleGraph config sources entryModulePath =
  reverse . resolveModulesRev <$> visitModule [] Set.empty [] entryModulePath
  where
    visitModule ::
      [[Text]] ->
      Set [Text] ->
      [ResolvedModule] ->
      [Text] ->
      Either Text (Set [Text], [ResolvedModule])
    visitModule callStack resolvedSet resolvedRev modulePath
      | modulePath `Set.member` resolvedSet =
          Right (resolvedSet, resolvedRev)
      | modulePath `elem` callStack =
          Left (mkCycleError modulePath callStack)
      | otherwise = do
          (sourcePath, sourceText) <- loadModuleSource callStack modulePath
          importPaths <- parseModuleImports sourcePath sourceText
          let nextStack = modulePath : callStack
              sortedImports = sortModulePaths importPaths
          (resolvedSetAfterDeps, resolvedAfterDeps) <-
            foldM
              (\(currentSet, currentRev) importPath -> visitModule nextStack currentSet currentRev importPath)
              (resolvedSet, resolvedRev)
              sortedImports
          let resolvedModule =
                ResolvedModule
                  { resolvedModulePath = modulePath,
                    resolvedSourcePath = sourcePath,
                    resolvedImports = sortedImports
                  }
          Right
            ( Set.insert modulePath resolvedSetAfterDeps,
              resolvedModule : resolvedAfterDeps
            )

    resolveModulesRev :: (Set [Text], [ResolvedModule]) -> [ResolvedModule]
    resolveModulesRev (_, resolvedRev) = resolvedRev

    loadModuleSource :: [[Text]] -> [Text] -> Either Text (FilePath, Text)
    loadModuleSource callStack modulePath =
      case matchingCandidates of
        [] ->
          Left
            ( "E4001: unresolved import '"
                <> renderModulePath modulePath
                <> "'"
                <> renderImporterContext callStack
                <> "; looked in "
                <> Text.intercalate ", " (map Text.pack candidatePaths)
            )
        [sourcePath] ->
          case Map.lookup sourcePath sources of
            Just sourceText -> Right (sourcePath, sourceText)
            Nothing ->
              Left
                ( "E4001: unresolved import '"
                    <> renderModulePath modulePath
                    <> "'"
                    <> renderImporterContext callStack
                    <> "; looked in "
                    <> Text.intercalate ", " (map Text.pack candidatePaths)
                )
        _ ->
          Left
            ( "E4002: ambiguous import '"
                <> renderModulePath modulePath
                <> "'"
                <> renderImporterContext callStack
                <> "; matched "
                <> Text.intercalate ", " (map Text.pack matchingCandidates)
            )
      where
        relativePath = modulePathToRelativeFileWithExt (moduleExtension config) modulePath
        candidatePaths = map (appendRelativePath relativePath) (moduleRoots config)
        matchingCandidates = filter (`Map.member` sources) candidatePaths

appendRelativePath :: FilePath -> FilePath -> FilePath
appendRelativePath relativePath root
  | null root = relativePath
  | otherwise = root </> relativePath

modulePathToRelativeFileWithExt :: String -> [Text] -> FilePath
modulePathToRelativeFileWithExt extension modulePath =
  case modulePath of
    [] -> extension
    _ -> foldr1 joinSegments (map Text.unpack modulePath) <> extension
  where
    joinSegments segment acc = segment <> "/" <> acc

parseModuleImports :: FilePath -> Text -> Either Text [[Text]]
parseModuleImports sourcePath sourceText =
  case parseSurfaceProgram sourceText of
    Left parseError ->
      Left
        ( "E4004: module parse error at '"
            <> Text.pack sourcePath
            <> "': "
            <> parseError
        )
    Right surfaceExpr ->
      Right (collectImports surfaceExpr)

collectImports :: SurfaceExpr -> [[Text]]
collectImports surfaceExpr =
  case surfaceExpr of
    SEScope statements ->
      [ modulePath
        | SSImport _ modulePath _ _ <- statements
      ]
    _ -> []

sortModulePaths :: [[Text]] -> [[Text]]
sortModulePaths modulePaths =
  map snd . sortOn fst $ map (\modulePath -> (renderModulePath modulePath, modulePath)) uniquePaths
  where
    uniquePaths = Set.toList (Set.fromList modulePaths)

mkCycleError :: [Text] -> [[Text]] -> Text
mkCycleError repeatedModulePath callStack =
  "E4003: module import cycle detected: "
    <> Text.intercalate " -> " (map renderModulePath cycleTrace)
  where
    rootToLeaf = reverse callStack
    suffixStartingAtRepeat = dropWhile (/= repeatedModulePath) rootToLeaf
    cycleTrace = suffixStartingAtRepeat ++ [repeatedModulePath]

renderImporterContext :: [[Text]] -> Text
renderImporterContext callStack =
  case callStack of
    importerPath : _ -> " imported by '" <> renderModulePath importerPath <> "'"
    [] -> ""

renderModulePath :: [Text] -> Text
renderModulePath segments = Text.intercalate "::" segments
