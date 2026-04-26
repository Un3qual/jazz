{-# LANGUAGE OverloadedStrings #-}

-- | Module graph resolver for `module` and `import` forms. It loads source,
-- validates module declarations/import bindings, and returns modules in
-- dependency order for the driver.
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
import JazzNext.Compiler.Diagnostics
  ( Diagnostic (..),
    SourceSpan,
    mkDiagnostic,
    mkMessageDiagnostic,
    setDiagnosticPrimarySpan,
    setDiagnosticRelatedSpan,
    setDiagnosticSubject
  )
import JazzNext.Compiler.Identifier
  ( identifierText
  )
import JazzNext.Compiler.Parser
  ( parseSurfaceProgram
  )
import JazzNext.Compiler.Parser.AST
  ( SurfaceCaseArm (..),
    SurfacePattern (..),
    SurfaceExpr (..),
    SurfaceStatement (..)
  )
import System.FilePath ((</>))

-- | File-system lookup policy for module loading.
data ModuleResolutionConfig = ModuleResolutionConfig
  { moduleRoots :: [FilePath],
    moduleExtension :: String
  }
  deriving (Eq, Show)

-- | Minimal resolved-module record consumed by the driver when replaying source
-- in dependency order.
data ResolvedModule = ResolvedModule
  { resolvedModulePath :: [Text],
    resolvedSourcePath :: FilePath,
    resolvedImports :: [[Text]]
  }
  deriving (Eq, Show)

data ParsedImport = ParsedImport
  { parsedImportSpan :: SourceSpan,
    parsedImportModulePath :: [Text],
    parsedImportAlias :: Maybe Text,
    parsedImportSymbols :: Maybe [Text]
  }
  deriving (Eq, Show)

data ParsedModule = ParsedModule
  { parsedModuleImports :: [ParsedImport],
    parsedModuleExports :: Set Text,
    parsedModuleReferences :: Set Text
  }

data BindingOrigin = BindingOrigin
  { bindingOriginModulePath :: [Text],
    bindingOriginSpan :: SourceSpan
  }

data ResolvedState = ResolvedState
  { resolvedSetState :: Set [Text],
    resolvedModulesRevState :: [ResolvedModule],
    resolvedExportsState :: Map [Text] (Set Text)
  }

modulePathToRelativeFile :: [Text] -> FilePath
modulePathToRelativeFile = modulePathToRelativeFileWithExt ".jz"

-- | Parse a user-provided module path like `Foo::Bar` and reject empty or
-- non-identifier segments before resolution starts.
parseModulePathText :: Text -> Either Diagnostic [Text]
parseModulePathText rawModulePath
  | Text.null rawModulePath =
      Left (mkMessageDiagnostic "entry module path cannot be empty")
  | any Text.null segments =
      Left
        ( mkMessageDiagnostic
            ( "invalid entry module path '"
                <> rawModulePath
                <> "': empty path segment"
            )
        )
  | not (all isValidSegment segments) =
      Left
        ( mkMessageDiagnostic
            ( "invalid entry module path '"
                <> rawModulePath
                <> "': segments must be identifiers"
            )
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
  Either Diagnostic [ResolvedModule]
resolveModuleGraph config sources entryModulePath =
  runIdentity $
    resolveModuleGraphWithLookup
      config
      (\path -> pure (Map.lookup path sources))
      entryModulePath

-- | Resolve an entry module and all of its imports using an abstract source
-- lookup function so tests and CLI can share the same resolver logic.
resolveModuleGraphWithLookup ::
  Monad m =>
  ModuleResolutionConfig ->
  (FilePath -> m (Maybe Text)) ->
  [Text] ->
  m (Either Diagnostic [ResolvedModule])
resolveModuleGraphWithLookup config loadSource entryModulePath
  | null entryModulePath =
      pure (Left (mkMessageDiagnostic "empty entry module path"))
  | otherwise =
      fmap (fmap (reverse . resolveModulesRev)) (visitModule [] initialState entryModulePath)
  where
    initialState =
      ResolvedState
        { resolvedSetState = Set.empty,
          resolvedModulesRevState = [],
          resolvedExportsState = Map.empty
        }

    visitModule callStack state modulePath
      | modulePath `Set.member` resolvedSetState state =
          pure (Right state)
      | modulePath `elem` callStack =
          pure (Left (mkCycleError modulePath callStack))
      | otherwise = do
          sourceResult <- loadModuleSource callStack modulePath
          case sourceResult of
            Left err -> pure (Left err)
            Right (sourcePath, sourceText) ->
              case parseModuleDetails sourcePath modulePath sourceText of
                Left err -> pure (Left err)
                Right parsedModule -> do
                  let nextStack = modulePath : callStack
                      sortedImports = sortModulePaths (collectImportPaths (parsedModuleImports parsedModule))
                  resolvedDepsResult <-
                    foldM
                      (visitDependency nextStack)
                      (Right state)
                      sortedImports
                  case resolvedDepsResult of
                    Left err -> pure (Left err)
                    Right stateAfterDeps ->
                      case
                          validateImportBindings
                            sourcePath
                            modulePath
                            (parsedModuleImports parsedModule)
                            (parsedModuleReferences parsedModule)
                            (resolvedExportsState stateAfterDeps) of
                        Left err -> pure (Left err)
                        Right () ->
                          let resolvedModule =
                                ResolvedModule
                                  { resolvedModulePath = modulePath,
                                    resolvedSourcePath = sourcePath,
                                    resolvedImports = sortedImports
                                  }
                           in pure
                                ( Right
                                    stateAfterDeps
                                      { resolvedSetState =
                                          Set.insert modulePath (resolvedSetState stateAfterDeps),
                                        resolvedModulesRevState =
                                          resolvedModule : resolvedModulesRevState stateAfterDeps,
                                        resolvedExportsState =
                                          Map.insert
                                            modulePath
                                            (parsedModuleExports parsedModule)
                                            (resolvedExportsState stateAfterDeps)
                                      }
                                )

    resolveModulesRev :: ResolvedState -> [ResolvedModule]
    resolveModulesRev = resolvedModulesRevState

    visitDependency nextStack accumulator importPath =
      case accumulator of
        Left err -> pure (Left err)
        Right currentState ->
          visitModule nextStack currentState importPath

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
              ( mkDiagnostic
                  "E4001"
                  ( "unresolved import '"
                      <> renderModulePath modulePath
                      <> "'"
                      <> renderImporterContext callStack
                      <> "; looked in "
                      <> Text.intercalate ", " (map Text.pack candidatePaths)
                  )
              )
          [(sourcePath, sourceText)] ->
            Right (sourcePath, sourceText)
          _ ->
            Left
              ( mkDiagnostic
                  "E4002"
                  ( "ambiguous import '"
                      <> renderModulePath modulePath
                      <> "'"
                      <> renderImporterContext callStack
                      <> "; matched "
                      <> Text.intercalate ", " (map (Text.pack . fst) matchingCandidates)
                  )
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

-- | Parse a module's surface source and extract only the details needed by the
-- resolver: declarations, imports, and top-level exports.
parseModuleDetails :: FilePath -> [Text] -> Text -> Either Diagnostic ParsedModule
parseModuleDetails sourcePath expectedModulePath sourceText =
  case parseSurfaceProgram sourceText of
    Left parseError ->
      Left
        ( mkDiagnostic
            "E4004"
            ( "module parse error at '"
                <> Text.pack sourcePath
                <> "': "
                <> diagnosticSummary parseError
            )
        )
    Right surfaceExpr -> do
      validateModuleDeclarations sourcePath expectedModulePath surfaceExpr
      let topLevelBindings = collectTopLevelBindings surfaceExpr
      Right
        ParsedModule
          { parsedModuleImports = collectImports surfaceExpr,
            parsedModuleExports = topLevelBindings,
            parsedModuleReferences = collectReferencedNames surfaceExpr Set.\\ topLevelBindings
          }

collectImports :: SurfaceExpr -> [ParsedImport]
collectImports surfaceExpr =
  case surfaceExpr of
    SEBlock statements ->
      [ ParsedImport spanValue modulePath alias importedSymbols
        | SSImport spanValue modulePath alias importedSymbols <- statements
      ]
    _ -> []

collectImportPaths :: [ParsedImport] -> [[Text]]
collectImportPaths imports =
  [ parsedImportModulePath importDecl
    | importDecl <- imports
  ]

collectTopLevelBindings :: SurfaceExpr -> Set Text
collectTopLevelBindings surfaceExpr =
  case surfaceExpr of
    SEBlock statements ->
      Set.fromList
        [ identifierText bindingName
          | SSLet bindingName _ _ <- statements
        ]
    _ -> Set.empty

collectReferencedNames :: SurfaceExpr -> Set Text
collectReferencedNames = collectExprReferences Set.empty

collectExprReferences :: Set Text -> SurfaceExpr -> Set Text
collectExprReferences boundNames surfaceExpr =
  case surfaceExpr of
    SELit _ -> Set.empty
    SEVar name
      | identifierText name `Set.member` boundNames -> Set.empty
      | otherwise -> Set.singleton (identifierText name)
    SELambda params body ->
      collectExprReferences
        (Set.union boundNames (Set.fromList (map identifierText params)))
        body
    SEOperatorValue _ -> Set.empty
    SEList items ->
      Set.unions (map (collectExprReferences boundNames) items)
    SEApply function argument ->
      Set.union
        (collectExprReferences boundNames function)
        (collectExprReferences boundNames argument)
    SEIf condition trueBranch falseBranch ->
      Set.unions
        [ collectExprReferences boundNames condition,
          collectExprReferences boundNames trueBranch,
          collectExprReferences boundNames falseBranch
        ]
    SECase scrutinee arms ->
      Set.union
        (collectExprReferences boundNames scrutinee)
        (Set.unions (map (collectCaseArmReferences boundNames) arms))
    SEBinary _ left right ->
      Set.union
        (collectExprReferences boundNames left)
        (collectExprReferences boundNames right)
    SESectionLeft left _ ->
      collectExprReferences boundNames left
    SESectionRight _ right ->
      collectExprReferences boundNames right
    SEBlock statements ->
      collectBlockReferences boundNames statements

collectBlockReferences :: Set Text -> [SurfaceStatement] -> Set Text
collectBlockReferences boundNames statements =
  Set.unions (map collectStatementReferences statements)
  where
    blockBoundNames =
      Set.union
        boundNames
        ( Set.fromList
            [ identifierText bindingName
              | SSLet bindingName _ _ <- statements
            ]
        )

    collectStatementReferences statement =
      case statement of
        SSLet _ _ valueExpr ->
          collectExprReferences blockBoundNames valueExpr
        SSExpr _ expr ->
          collectExprReferences blockBoundNames expr
        SSSignature {} -> Set.empty
        SSData {} -> Set.empty
        SSModule {} -> Set.empty
        SSImport {} -> Set.empty

collectCaseArmReferences :: Set Text -> SurfaceCaseArm -> Set Text
collectCaseArmReferences boundNames (SurfaceCaseArm patternValue body) =
  collectExprReferences
    (Set.union boundNames (collectPatternBinders patternValue))
    body

collectPatternBinders :: SurfacePattern -> Set Text
collectPatternBinders patternValue =
  case patternValue of
    SPWildcard -> Set.empty
    SPVariable name -> Set.singleton (identifierText name)
    SPLiteral _ -> Set.empty
    SPConstructor _ nestedPatterns ->
      Set.unions (map collectPatternBinders nestedPatterns)
    SPList nestedPatterns ->
      Set.unions (map collectPatternBinders nestedPatterns)

validateModuleDeclarations :: FilePath -> [Text] -> SurfaceExpr -> Either Diagnostic ()
validateModuleDeclarations sourcePath expectedModulePath surfaceExpr =
  case collectModuleDeclarations surfaceExpr of
    [] ->
      Right ()
    [declaredModulePath]
      | declaredModulePath == expectedModulePath ->
          Right ()
      | otherwise ->
          Left
            ( mkDiagnostic
                "E4006"
                ( "module declaration mismatch at '"
                    <> Text.pack sourcePath
                    <> "': expected '"
                    <> renderModulePath expectedModulePath
                    <> "', found '"
                    <> renderModulePath declaredModulePath
                    <> "'"
                )
            )
    declaredModulePaths ->
      Left
        ( mkDiagnostic
            "E4005"
            ( "multiple module declarations in '"
                <> Text.pack sourcePath
                <> "': "
                <> Text.intercalate ", " (map renderModulePath declaredModulePaths)
            )
        )

collectModuleDeclarations :: SurfaceExpr -> [[Text]]
collectModuleDeclarations surfaceExpr =
  case surfaceExpr of
    SEBlock statements ->
      [ modulePath
        | SSModule _ modulePath <- statements
      ]
    _ -> []

-- | Validate alias and explicit-symbol imports after dependencies have been
-- resolved so the exporting module inventories are known.
validateImportBindings ::
  FilePath ->
  [Text] ->
  [ParsedImport] ->
  Set Text ->
  Map [Text] (Set Text) ->
  Either Diagnostic ()
validateImportBindings sourcePath importerPath imports referencedNames exportsByModule = do
  go Map.empty Map.empty imports
  visibleSymbols <- collectVisibleImportSymbols imports
  case findHiddenExplicitImportReference visibleSymbols of
    Just (symbolName, importDecl) ->
      Left (mkHiddenExplicitImportSymbolError symbolName importDecl)
    Nothing ->
      case findHiddenAliasImportReference visibleSymbols of
        Just (symbolName, importDecl, aliasName) ->
          Left (mkHiddenAliasImportSymbolError symbolName importDecl aliasName)
        Nothing -> Right ()
  where
    go seenSymbols seenAliases remainingImports =
      case remainingImports of
        [] ->
          Right ()
        importDecl : rest -> do
          seenAliasesAfterImport <- validateImportAlias seenAliases importDecl
          seenSymbolsAfterImport <- validateImportSymbols seenSymbols importDecl
          go seenSymbolsAfterImport seenAliasesAfterImport rest

    validateImportAlias :: Map Text BindingOrigin -> ParsedImport -> Either Diagnostic (Map Text BindingOrigin)
    validateImportAlias seenAliases importDecl =
      case parsedImportAlias importDecl of
        Nothing ->
          Right seenAliases
        Just aliasName ->
          case Map.lookup aliasName seenAliases of
            Just previousOrigin ->
              Left (mkImportAliasCollisionError aliasName previousOrigin importDecl)
            Nothing ->
              Right
                ( Map.insert
                    aliasName
                    BindingOrigin
                      { bindingOriginModulePath = parsedImportModulePath importDecl,
                        bindingOriginSpan = parsedImportSpan importDecl
                      }
                    seenAliases
                )

    validateImportSymbols :: Map Text BindingOrigin -> ParsedImport -> Either Diagnostic (Map Text BindingOrigin)
    validateImportSymbols seenSymbols importDecl =
      case parsedImportSymbols importDecl of
        Nothing ->
          Right seenSymbols
        Just symbolNames ->
          case Map.lookup (parsedImportModulePath importDecl) exportsByModule of
            Nothing ->
              Left
                ( mkDiagnostic
                    "E4010"
                    ( "internal resolver error while validating imports for '"
                        <> renderModulePath importerPath
                        <> "': missing exports for module '"
                        <> renderModulePath (parsedImportModulePath importDecl)
                        <> "'"
                    )
                )
            Just exportedSymbols ->
              foldM
                (validateImportSymbol importDecl exportedSymbols)
                seenSymbols
                symbolNames

    validateImportSymbol ::
      ParsedImport ->
      Set Text ->
      Map Text BindingOrigin ->
      Text ->
      Either Diagnostic (Map Text BindingOrigin)
    validateImportSymbol importDecl exportedSymbols seenSymbols symbolName
      | not (Set.member symbolName exportedSymbols) =
          Left (mkMissingImportSymbolError symbolName importDecl exportedSymbols)
      | otherwise =
          case Map.lookup symbolName seenSymbols of
            Just previousOrigin ->
              Left (mkImportSymbolCollisionError symbolName previousOrigin importDecl)
            Nothing ->
              Right
                ( Map.insert
                    symbolName
                    BindingOrigin
                      { bindingOriginModulePath = parsedImportModulePath importDecl,
                        bindingOriginSpan = parsedImportSpan importDecl
                      }
                    seenSymbols
                )

    mkMissingImportSymbolError :: Text -> ParsedImport -> Set Text -> Diagnostic
    mkMissingImportSymbolError symbolName importDecl exportedSymbols =
      setDiagnosticSubject symbolName $
        setDiagnosticPrimarySpan
          (parsedImportSpan importDecl)
          ( mkDiagnostic
              "E4007"
              ( "import symbol '"
                  <> symbolName
                  <> "' is not exported by module '"
                  <> renderModulePath (parsedImportModulePath importDecl)
                  <> "' imported by '"
                  <> renderModulePath importerPath
                  <> "' in '"
                  <> Text.pack sourcePath
                  <> "'; available exports: "
                  <> renderExports exportedSymbols
              )
          )

    mkImportSymbolCollisionError :: Text -> BindingOrigin -> ParsedImport -> Diagnostic
    mkImportSymbolCollisionError symbolName previousOrigin importDecl =
      setDiagnosticSubject symbolName $
        setDiagnosticRelatedSpan
          (bindingOriginSpan previousOrigin)
          ( setDiagnosticPrimarySpan
              (parsedImportSpan importDecl)
              ( mkDiagnostic
                  "E4008"
                  ( "import binding collision for symbol '"
                      <> symbolName
                      <> "' in module '"
                      <> renderModulePath importerPath
                      <> "' at '"
                      <> Text.pack sourcePath
                      <> "'; already imported from '"
                      <> renderModulePath (bindingOriginModulePath previousOrigin)
                      <> "', cannot re-import from '"
                      <> renderModulePath (parsedImportModulePath importDecl)
                      <> "'"
                  )
              )
          )

    collectVisibleImportSymbols :: [ParsedImport] -> Either Diagnostic (Set Text)
    collectVisibleImportSymbols =
      foldM collectVisibleImportSymbol Set.empty

    collectVisibleImportSymbol :: Set Text -> ParsedImport -> Either Diagnostic (Set Text)
    collectVisibleImportSymbol visibleSymbols importDecl =
      case Map.lookup (parsedImportModulePath importDecl) exportsByModule of
        Nothing ->
          Left
            ( mkDiagnostic
                "E4010"
                ( "internal resolver error while validating imports for '"
                    <> renderModulePath importerPath
                    <> "': missing exports for module '"
                    <> renderModulePath (parsedImportModulePath importDecl)
                    <> "'"
                )
            )
        Just exportedSymbols ->
          Right
            ( Set.union
                visibleSymbols
                ( case parsedImportAlias importDecl of
                    Just _ -> Set.empty
                    Nothing ->
                      case parsedImportSymbols importDecl of
                        Nothing -> exportedSymbols
                        Just symbolNames -> Set.fromList symbolNames
                )
            )

    findHiddenExplicitImportReference :: Set Text -> Maybe (Text, ParsedImport)
    findHiddenExplicitImportReference visibleSymbols =
      firstMatch
        [ (symbolName, importDecl)
          | importDecl <- imports,
            Just symbolNames <- [parsedImportSymbols importDecl],
            Just exportedSymbols <- [Map.lookup (parsedImportModulePath importDecl) exportsByModule],
            let hiddenSymbols = Set.difference exportedSymbols (Set.fromList symbolNames),
            symbolName <- sortOn id (Set.toList hiddenSymbols),
            Set.member symbolName referencedNames,
            not (Set.member symbolName visibleSymbols)
        ]

    findHiddenAliasImportReference :: Set Text -> Maybe (Text, ParsedImport, Text)
    findHiddenAliasImportReference visibleSymbols =
      firstMatch
        [ (symbolName, importDecl, aliasName)
          | importDecl <- imports,
            Just aliasName <- [parsedImportAlias importDecl],
            Just exportedSymbols <- [Map.lookup (parsedImportModulePath importDecl) exportsByModule],
            symbolName <- sortOn id (Set.toList exportedSymbols),
            Set.member symbolName referencedNames,
            not (Set.member symbolName visibleSymbols)
        ]

    firstMatch :: [a] -> Maybe a
    firstMatch matches =
      case matches of
        [] -> Nothing
        match : _ -> Just match

    mkHiddenExplicitImportSymbolError :: Text -> ParsedImport -> Diagnostic
    mkHiddenExplicitImportSymbolError symbolName importDecl =
      setDiagnosticSubject symbolName $
        setDiagnosticPrimarySpan
          (parsedImportSpan importDecl)
          ( mkDiagnostic
              "E4011"
              ( "import symbol '"
                  <> symbolName
                  <> "' is not visible from explicit import of module '"
                  <> renderModulePath (parsedImportModulePath importDecl)
                  <> "' by '"
                  <> renderModulePath importerPath
                  <> "' in '"
                  <> Text.pack sourcePath
                  <> "'"
              )
          )

    mkHiddenAliasImportSymbolError :: Text -> ParsedImport -> Text -> Diagnostic
    mkHiddenAliasImportSymbolError symbolName importDecl aliasName =
      setDiagnosticSubject symbolName $
        setDiagnosticPrimarySpan
          (parsedImportSpan importDecl)
          ( mkDiagnostic
              "E4012"
              ( "import symbol '"
                  <> symbolName
                  <> "' is not visible unqualified from alias import of module '"
                  <> renderModulePath (parsedImportModulePath importDecl)
                  <> "' as '"
                  <> aliasName
                  <> "' by '"
                  <> renderModulePath importerPath
                  <> "' in '"
                  <> Text.pack sourcePath
                  <> "'"
              )
          )

    mkImportAliasCollisionError :: Text -> BindingOrigin -> ParsedImport -> Diagnostic
    mkImportAliasCollisionError aliasName previousOrigin importDecl =
      setDiagnosticSubject aliasName $
        setDiagnosticRelatedSpan
          (bindingOriginSpan previousOrigin)
          ( setDiagnosticPrimarySpan
              (parsedImportSpan importDecl)
              ( mkDiagnostic
                  "E4009"
                  ( "import alias collision for '"
                      <> aliasName
                      <> "' in module '"
                      <> renderModulePath importerPath
                      <> "' at '"
                      <> Text.pack sourcePath
                      <> "'; already aliased to module '"
                      <> renderModulePath (bindingOriginModulePath previousOrigin)
                      <> "', cannot alias module '"
                      <> renderModulePath (parsedImportModulePath importDecl)
                      <> "'"
                  )
              )
          )

    renderExports :: Set Text -> Text
    renderExports exports
      | Set.null exports = "<none>"
      | otherwise = Text.intercalate ", " (sortOn id (Set.toList exports))

-- | Provide a deterministic lexical import order for traversal and diagnostics.
-- Encounter order is intentionally discarded by `Set`-based deduplication and
-- the final `renderModulePath` sort.
sortModulePaths :: [[Text]] -> [[Text]]
sortModulePaths modulePaths =
  map snd . sortOn fst $ map (\modulePath -> (renderModulePath modulePath, modulePath)) uniquePaths
  where
    uniquePaths = Set.toList (Set.fromList modulePaths)

mkCycleError :: [Text] -> [[Text]] -> Diagnostic
mkCycleError repeatedModulePath callStack =
  mkDiagnostic
    "E4003"
    ("module import cycle detected: " <> Text.intercalate " -> " (map renderModulePath cycleTrace))
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

-- | Preserve the first occurrence of each candidate path so module-root lookup
-- order remains stable while removing duplicates.
dedupePreservingOrder :: Ord a => [a] -> [a]
dedupePreservingOrder =
  reverse . fst . foldl' step ([], Set.empty)
  where
    step (uniqueRev, seen) value
      | Set.member value seen = (uniqueRev, seen)
      | otherwise = (value : uniqueRev, Set.insert value seen)
