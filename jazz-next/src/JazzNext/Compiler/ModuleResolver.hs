{-# LANGUAGE OverloadedStrings #-}

module JazzNext.Compiler.ModuleResolver
  ( ModuleResolutionConfig (..),
    ResolvedModule (..),
    modulePathToRelativeFile,
    parseModulePathText,
    resolveModuleGraph,
    resolveModuleGraphWithLookup
  ) where

import Control.Monad (foldM)
import Data.Char (isAlpha, isAlphaNum)
import Data.Functor.Identity
  ( Identity (..),
    runIdentity
  )
import Data.List (foldl', sortOn)
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

parseModulePathText :: Text -> Either Text [Text]
parseModulePathText rawModulePath
  | Text.null rawModulePath =
      Left "entry module path cannot be empty"
  | any Text.null segments =
      Left
        ( "invalid entry module path '"
            <> rawModulePath
            <> "': empty path segment"
        )
  | not (all isValidSegment segments) =
      Left
        ( "invalid entry module path '"
            <> rawModulePath
            <> "': segments must be identifiers"
        )
  | otherwise =
      Right segments
  where
    segments = Text.splitOn "::" rawModulePath

    isValidSegment :: Text -> Bool
    isValidSegment segment =
      case Text.uncons segment of
        Nothing -> False
        Just (firstChar, restChars) ->
          isIdentifierStart firstChar && Text.all isIdentifierRest restChars

    isIdentifierStart ch = isAlpha ch || ch == '_'
    isIdentifierRest ch = isAlphaNum ch || ch == '_' || ch == '\'' || ch == '!'

resolveModuleGraph ::
  ModuleResolutionConfig ->
  Map FilePath Text ->
  [Text] ->
  Either Text [ResolvedModule]
resolveModuleGraph config sources entryModulePath =
  runIdentity $
    resolveModuleGraphWithLookup
      config
      (\path -> pure (Map.lookup path sources))
      entryModulePath

resolveModuleGraphWithLookup ::
  Monad m =>
  ModuleResolutionConfig ->
  (FilePath -> m (Maybe Text)) ->
  [Text] ->
  m (Either Text [ResolvedModule])
resolveModuleGraphWithLookup config loadSource entryModulePath =
  fmap (fmap (reverse . resolveModulesRev)) (visitModule [] Set.empty [] entryModulePath)
  where
    visitModule callStack resolvedSet resolvedRev modulePath
      | modulePath `Set.member` resolvedSet =
          pure (Right (resolvedSet, resolvedRev))
      | modulePath `elem` callStack =
          pure (Left (mkCycleError modulePath callStack))
      | otherwise = do
          sourceResult <- loadModuleSource callStack modulePath
          case sourceResult of
            Left err -> pure (Left err)
            Right (sourcePath, sourceText) ->
              case parseModuleImports sourcePath sourceText of
                Left err -> pure (Left err)
                Right importPaths -> do
                  let nextStack = modulePath : callStack
                      sortedImports = sortModulePaths importPaths
                  resolvedDepsResult <-
                    foldM
                      (visitDependency nextStack)
                      (Right (resolvedSet, resolvedRev))
                      sortedImports
                  pure $
                    case resolvedDepsResult of
                      Left err -> Left err
                      Right (resolvedSetAfterDeps, resolvedAfterDeps) ->
                        let resolvedModule =
                              ResolvedModule
                                { resolvedModulePath = modulePath,
                                  resolvedSourcePath = sourcePath,
                                  resolvedImports = sortedImports
                                }
                         in Right
                              ( Set.insert modulePath resolvedSetAfterDeps,
                                resolvedModule : resolvedAfterDeps
                              )

    resolveModulesRev :: (Set [Text], [ResolvedModule]) -> [ResolvedModule]
    resolveModulesRev (_, resolvedRev) = resolvedRev

    visitDependency nextStack accumulator importPath =
      case accumulator of
        Left err -> pure (Left err)
        Right (currentSet, currentRev) ->
          visitModule nextStack currentSet currentRev importPath

    loadModuleSource callStack modulePath = do
      let relativePath = modulePathToRelativeFileWithExt (moduleExtension config) modulePath
          candidatePaths =
            dedupePreservingOrder
              (map (appendRelativePath relativePath) (moduleRoots config))
      candidatesWithContents <-
        mapM
          (\candidatePath -> do
             sourceText <- loadSource candidatePath
             pure (candidatePath, sourceText))
          candidatePaths
      let matchingCandidates =
            [ (candidatePath, sourceText)
              | (candidatePath, Just sourceText) <- candidatesWithContents
            ]
      pure $
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
          [(sourcePath, sourceText)] ->
            Right (sourcePath, sourceText)
          _ ->
            Left
              ( "E4002: ambiguous import '"
                  <> renderModulePath modulePath
                  <> "'"
                  <> renderImporterContext callStack
                  <> "; matched "
                  <> Text.intercalate ", " (map (Text.pack . fst) matchingCandidates)
              )

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

dedupePreservingOrder :: Ord a => [a] -> [a]
dedupePreservingOrder =
  reverse . fst . foldl' step ([], Set.empty)
  where
    step (uniqueRev, seen) value
      | Set.member value seen = (uniqueRev, seen)
      | otherwise = (value : uniqueRev, Set.insert value seen)
