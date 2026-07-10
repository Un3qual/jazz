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
    resolveModuleGraphWithLookup,
    resolveModuleGraphWithLookupAndVisibleSymbols,
    resolveProgram
  ) where

import Control.Monad (foldM)
import Data.Char (isAlpha, isAlphaNum)
import Data.Functor.Identity
  ( Identity (..),
    runIdentity
  )
import Data.List (foldl', sortOn)
import qualified Data.List.NonEmpty as NonEmpty
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
  ( identifierText,
    mkIdentifier
  )
import JazzNext.Compiler.AST
  ( CaseArm (..),
    ClassMethodSignature (..),
    ConstraintSignatureType (..),
    DataConstructor (..),
    DataConstructorArgument (..),
    Expr (..),
    ImplMethod (..),
    Pattern (..),
    SignatureConstraint (..),
    SignaturePayload (..),
    SignatureToken (..),
    Statement (..)
  )
import JazzNext.Compiler.BuiltinCatalog
  ( BuiltinResolutionMode (..),
    lookupBuiltinSymbolInMode
  )
import qualified JazzNext.Compiler.ModuleGraph as ModuleGraph
import JazzNext.Compiler.Name
  ( Name (..),
    NameNamespace (..),
    ResolvedNameOrigin (..),
    renderName
  )
import JazzNext.Compiler.Parser
  ( parseSurfaceProgram
  )
import JazzNext.Compiler.Parser.Lower (lowerSurfaceModule)
import JazzNext.Compiler.Parser.AST
  ( SurfaceCaseArm (..),
    SurfaceDataConstructor (..),
    SurfaceLambdaParameter (..),
    SurfacePattern (..),
    SurfaceExpr (..),
    SurfaceStatement (..)
  )
import System.FilePath
  ( normalise,
    (</>)
  )

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

-- | Import declaration details retained after parsing so validation can report
-- diagnostics with the original import span.
data ParsedImport = ParsedImport
  { parsedImportSpan :: SourceSpan,
    parsedImportModulePath :: [Text],
    parsedImportAlias :: Maybe Text,
    parsedImportSymbols :: Maybe [Text]
  }
  deriving (Eq, Show)

-- | Resolver-local view of a parsed module. It deliberately stores only import,
-- export, and reference inventories, not the lowered executable program.
data ParsedModule = ParsedModule
  { parsedModuleImports :: [ParsedImport],
    parsedModuleExports :: Set Text,
    parsedModuleConstructorNames :: Set Text,
    parsedModuleClassNames :: Set Text,
    parsedModuleReferences :: Set Text,
    parsedModuleQualifiedReferences :: Set (Text, Text),
    parsedModuleCore :: ModuleGraph.CoreModule
  }

-- | Origin metadata for imported bindings/aliases used in collision
-- diagnostics.
data BindingOrigin = BindingOrigin
  { bindingOriginModulePath :: [Text],
    bindingOriginSpan :: SourceSpan
  }

data ResolvedState = ResolvedState
  { resolvedSetState :: Set [Text],
    resolvedModulesRevState :: [ResolvedModule],
    resolvedGraphModulesRevState :: [ModuleGraph.ResolvedModule],
    resolvedExportsState :: Map [Text] (Set Text),
    resolvedConstructorExportsState :: Map [Text] (Set Text),
    resolvedClassExportsState :: Map [Text] (Set Text)
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
resolveModuleGraphWithLookup config =
  resolveModuleGraphWithLookupAndVisibleSymbols config Set.empty Set.empty

resolveModuleGraphWithLookupAndVisibleSymbols ::
  Monad m =>
  ModuleResolutionConfig ->
  Set Text ->
  Set Text ->
  (FilePath -> m (Maybe Text)) ->
  [Text] ->
  m (Either Diagnostic [ResolvedModule])
resolveModuleGraphWithLookupAndVisibleSymbols config ambientVisibleSymbols ambientVisibleClassNames loadSource entryModulePath =
  fmap
    (fmap (reverse . resolvedModulesRevState))
    ( resolveStateWithLookupAndVisibleSymbols
        config
        ResolveKernelOnly
        ambientVisibleSymbols
        ambientVisibleClassNames
        loadSource
        entryModulePath
    )

resolveProgram ::
  ModuleResolutionConfig ->
  BuiltinResolutionMode ->
  Set Name ->
  Set Name ->
  (FilePath -> IO (Maybe Text)) ->
  [Text] ->
  IO (Either Diagnostic ModuleGraph.ResolvedProgram)
resolveProgram config builtinMode ambientValues ambientClasses loadSource entryModulePath =
  fmap
    ( fmap
        ( \state ->
            ModuleGraph.ResolvedProgram
              { ModuleGraph.resolvedProgramEntryPath = entryModulePath,
                ModuleGraph.resolvedProgramModules = reverse (resolvedGraphModulesRevState state)
              }
        )
    )
    ( resolveStateWithLookupAndVisibleSymbols
        config
        builtinMode
        (Set.map renderName ambientValues)
        (Set.map renderName ambientClasses)
        loadSource
        entryModulePath
    )

resolveStateWithLookupAndVisibleSymbols ::
  Monad m =>
  ModuleResolutionConfig ->
  BuiltinResolutionMode ->
  Set Text ->
  Set Text ->
  (FilePath -> m (Maybe Text)) ->
  [Text] ->
  m (Either Diagnostic ResolvedState)
resolveStateWithLookupAndVisibleSymbols config builtinMode ambientVisibleSymbols ambientVisibleClassNames loadSource entryModulePath
  | null entryModulePath =
      pure (Left (mkMessageDiagnostic "empty entry module path"))
  | otherwise =
      visitModule [] initialState entryModulePath
  where
    initialState =
      ResolvedState
        { resolvedSetState = Set.empty,
          resolvedModulesRevState = [],
          resolvedGraphModulesRevState = [],
          resolvedExportsState = Map.empty,
          resolvedConstructorExportsState = Map.empty,
          resolvedClassExportsState = Map.empty
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
                            (parsedModuleClassNames parsedModule)
                            (parsedModuleReferences parsedModule)
                            (parsedModuleQualifiedReferences parsedModule)
                            ambientVisibleSymbols
                            ambientVisibleClassNames
                            (resolvedClassExportsState stateAfterDeps)
                            (resolvedExportsState stateAfterDeps) of
                        Left err -> pure (Left err)
                        Right () ->
                          let resolvedModule =
                                ResolvedModule
                                  { resolvedModulePath = modulePath,
                                    resolvedSourcePath = sourcePath,
                                    resolvedImports = sortedImports
                                  }
                              resolvedCore =
                                resolveCoreModuleNames
                                  builtinMode
                                  modulePath
                                  ambientVisibleSymbols
                                  ambientVisibleClassNames
                                  (parsedModuleExports parsedModule)
                                  (parsedModuleConstructorNames parsedModule)
                                  (parsedModuleClassNames parsedModule)
                                  (resolvedExportsState stateAfterDeps)
                                  (resolvedConstructorExportsState stateAfterDeps)
                                  (resolvedClassExportsState stateAfterDeps)
                                  (parsedModuleImports parsedModule)
                                  (parsedModuleCore parsedModule)
                              resolvedGraphModule =
                                ModuleGraph.ResolvedModule
                                  { ModuleGraph.resolvedModulePath = modulePath,
                                    ModuleGraph.resolvedSourcePath = sourcePath,
                                    ModuleGraph.resolvedModuleImports = ModuleGraph.coreModuleImports resolvedCore,
                                    ModuleGraph.resolvedModuleCore = resolvedCore
                                  }
                           in pure
                                ( Right
                                    stateAfterDeps
                                      { resolvedSetState =
                                          Set.insert modulePath (resolvedSetState stateAfterDeps),
                                        resolvedModulesRevState =
                                          resolvedModule : resolvedModulesRevState stateAfterDeps,
                                        resolvedGraphModulesRevState =
                                          resolvedGraphModule : resolvedGraphModulesRevState stateAfterDeps,
                                        resolvedExportsState =
                                          Map.insert
                                            modulePath
                                            (parsedModuleExports parsedModule)
                                            (resolvedExportsState stateAfterDeps),
                                        resolvedConstructorExportsState =
                                          Map.insert
                                            modulePath
                                            (parsedModuleConstructorNames parsedModule)
                                            (resolvedConstructorExportsState stateAfterDeps),
                                        resolvedClassExportsState =
                                          Map.insert
                                            modulePath
                                            (parsedModuleClassNames parsedModule)
                                            (resolvedClassExportsState stateAfterDeps)
                                      }
                                )

    visitDependency nextStack accumulator importPath =
      case accumulator of
        Left err -> pure (Left err)
        Right currentState ->
          visitModule nextStack currentState importPath

    loadModuleSource callStack modulePath = do
      let relativePath = modulePathToRelativeFileWithExt (moduleExtension config) modulePath
          candidatePaths =
            dedupePreservingOrder
              (map (normalise . appendRelativePath relativePath) (moduleRoots config))
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
      coreModule <- lowerSurfaceModule sourcePath expectedModulePath surfaceExpr
      let topLevelBindings = collectTopLevelBindings surfaceExpr
      Right
        ParsedModule
          { parsedModuleImports = collectImports surfaceExpr,
            parsedModuleExports = topLevelBindings,
            parsedModuleConstructorNames = collectTopLevelConstructorNames surfaceExpr,
            parsedModuleClassNames = collectTopLevelClassNames surfaceExpr,
            parsedModuleReferences = collectReferencedNames surfaceExpr Set.\\ topLevelBindings,
            parsedModuleQualifiedReferences = collectQualifiedReferences surfaceExpr,
            parsedModuleCore = coreModule
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
      Set.fromList (concatMap collectStatementBindings statements)
    _ -> Set.empty
  where
    collectStatementBindings statement =
      case statement of
        SSLet bindingName _ _ ->
          [identifierText bindingName]
        SSData _ _ _ constructors ->
          [ identifierText constructorName
            | SurfaceDataConstructor constructorName _ <- constructors
          ]
        _ -> []

collectTopLevelConstructorNames :: SurfaceExpr -> Set Text
collectTopLevelConstructorNames surfaceExpr =
  case surfaceExpr of
    SEBlock statements ->
      Set.fromList
        [ identifierText constructorName
          | SSData _ _ _ constructors <- statements,
            SurfaceDataConstructor constructorName _ <- constructors
        ]
    _ -> Set.empty

collectTopLevelClassNames :: SurfaceExpr -> Set Text
collectTopLevelClassNames surfaceExpr =
  case surfaceExpr of
    SEBlock statements ->
      Set.fromList
        [ identifierText className
          | SSClass _ className _ _ <- statements
        ]
    _ -> Set.empty

resolveCoreModuleNames ::
  BuiltinResolutionMode ->
  [Text] ->
  Set Text ->
  Set Text ->
  Set Text ->
  Set Text ->
  Set Text ->
  Map [Text] (Set Text) ->
  Map [Text] (Set Text) ->
  Map [Text] (Set Text) ->
  [ParsedImport] ->
  ModuleGraph.CoreModule ->
  ModuleGraph.CoreModule
resolveCoreModuleNames builtinMode _modulePath ambientValues ambientClasses localExports localConstructors localClasses exportsByModule constructorsByModule classesByModule imports coreModule =
  coreModule {ModuleGraph.coreModuleExpr = resolveExpr (ModuleGraph.coreModuleExpr coreModule)}
  where
    aliasPaths =
      Map.fromList
        [ (aliasName, parsedImportModulePath importDecl)
          | importDecl <- imports,
            Just aliasName <- [parsedImportAlias importDecl]
        ]

    visibleValueOrigins =
      Map.fromList
        [ (name, modulePath)
          | importDecl <- imports,
            parsedImportAlias importDecl == Nothing,
            let modulePath = parsedImportModulePath importDecl,
            name <- selectedImportNames importDecl (Map.findWithDefault Set.empty modulePath exportsByModule)
        ]

    visibleConstructorOrigins =
      Map.fromList
        [ (name, modulePath)
          | importDecl <- imports,
            parsedImportAlias importDecl == Nothing,
            let modulePath = parsedImportModulePath importDecl,
            name <- selectedImportNames importDecl (Map.findWithDefault Set.empty modulePath constructorsByModule)
        ]

    visibleClassOrigins =
      Map.fromList
        [ (name, modulePath)
          | importDecl <- imports,
            parsedImportAlias importDecl == Nothing,
            let modulePath = parsedImportModulePath importDecl,
            name <- selectedImportNames importDecl (Map.findWithDefault Set.empty modulePath classesByModule)
        ]

    selectedImportNames importDecl availableNames =
      case parsedImportSymbols importDecl of
        Nothing -> Set.toList availableNames
        Just selectedNames -> filter (`Set.member` availableNames) selectedNames

    resolveName namespace name =
      case name of
        SourceName identifier -> resolveUnqualified namespace identifier
        QualifiedName qualifier member ->
          let qualifierText = identifierText qualifier
              memberText = identifierText member
           in case Map.lookup qualifierText aliasPaths of
                Just dependencyPath ->
                  ResolvedName
                    (ImportedModule dependencyPath)
                    (importedNamespace dependencyPath memberText namespace)
                    member
                Nothing ->
                  ResolvedName
                    (classOrigin qualifierText)
                    ValueNamespace
                    (mkIdentifier (qualifierText <> "::" <> memberText))
        _ -> name

    resolveUnqualified namespace identifier
      | localName namespace nameText =
          ResolvedName CurrentModule namespace identifier
      | Just dependencyPath <- importedOrigin namespace nameText =
          ResolvedName (ImportedModule dependencyPath) (importedNamespace dependencyPath nameText namespace) identifier
      | ambientName namespace nameText =
          ResolvedName AmbientPrelude namespace identifier
      | namespace == ValueNamespace,
        Just _ <- lookupBuiltinSymbolInMode builtinMode nameText =
          BuiltinName identifier
      | otherwise =
          ResolvedName CurrentModule namespace identifier
      where
        nameText = identifierText identifier

    localName namespace nameText =
      case namespace of
        ConstructorNamespace -> Set.member nameText localConstructors
        CapabilityNamespace -> Set.member nameText localClasses
        _ -> Set.member nameText localExports

    importedOrigin namespace nameText =
      case namespace of
        ConstructorNamespace -> Map.lookup nameText visibleConstructorOrigins
        CapabilityNamespace -> Map.lookup nameText visibleClassOrigins
        _ -> Map.lookup nameText visibleValueOrigins

    importedNamespace dependencyPath nameText fallbackNamespace
      | Set.member nameText (Map.findWithDefault Set.empty dependencyPath constructorsByModule) = ConstructorNamespace
      | Set.member nameText (Map.findWithDefault Set.empty dependencyPath classesByModule) = CapabilityNamespace
      | otherwise = fallbackNamespace

    ambientName namespace nameText =
      case namespace of
        CapabilityNamespace -> Set.member nameText ambientClasses
        _ -> Set.member nameText ambientValues

    classOrigin className
      | Set.member className localClasses = CurrentModule
      | Just dependencyPath <- Map.lookup className visibleClassOrigins = ImportedModule dependencyPath
      | Set.member className ambientClasses = AmbientPrelude
      | otherwise = CurrentModule

    resolveExpr expression =
      case expression of
        ELit literal -> ELit literal
        EVar name -> EVar (resolveName (referenceNamespace name) name)
        ELambda parameter body ->
          ELambda (resolveBinder ValueNamespace parameter) (resolveExpr body)
        EOperatorValue symbol -> EOperatorValue symbol
        EList items -> EList (map resolveExpr items)
        ETuple items -> ETuple (map resolveExpr items)
        EApply function argument -> EApply (resolveExpr function) (resolveExpr argument)
        ETypeApplication function signatureType -> ETypeApplication (resolveExpr function) signatureType
        EIf condition trueBranch falseBranch ->
          EIf (resolveExpr condition) (resolveExpr trueBranch) (resolveExpr falseBranch)
        EPatternCase scrutinee arms -> EPatternCase (resolveExpr scrutinee) (map resolveCaseArm arms)
        EBinary symbol left right -> EBinary symbol (resolveExpr left) (resolveExpr right)
        ESectionLeft left symbol -> ESectionLeft (resolveExpr left) symbol
        ESectionRight symbol right -> ESectionRight symbol (resolveExpr right)
        EBlock statements -> EBlock (map resolveStatement statements)

    referenceNamespace name =
      case name of
        SourceName identifier
          | Set.member (identifierText identifier) localConstructors -> ConstructorNamespace
          | Map.member (identifierText identifier) visibleConstructorOrigins -> ConstructorNamespace
        _ -> ValueNamespace

    resolveBinder namespace name =
      case name of
        SourceName identifier -> ResolvedName CurrentModule namespace identifier
        _ -> name

    resolveCaseArm (CaseArm patternValue guard body) =
      CaseArm (resolvePattern patternValue) (fmap resolveExpr guard) (resolveExpr body)

    resolvePattern patternValue =
      case patternValue of
        PWildcard -> PWildcard
        PVariable name -> PVariable (resolveBinder ValueNamespace name)
        PLiteral literal -> PLiteral literal
        PConstructor name patterns ->
          PConstructor (resolveName ConstructorNamespace name) (map resolvePattern patterns)
        PList patterns -> PList (map resolvePattern patterns)
        PConsList headPattern tailPattern ->
          PConsList (resolvePattern headPattern) (resolvePattern tailPattern)
        PTuple patterns -> PTuple (map resolvePattern patterns)
        PAs name pattern' -> PAs (resolveBinder ValueNamespace name) (resolvePattern pattern')
        POr patterns -> POr (map resolvePattern patterns)

    resolveStatement statement =
      case statement of
        SLet name spanValue value ->
          SLet (resolveBinder ValueNamespace name) spanValue (resolveExpr value)
        SSignature name spanValue payload ->
          SSignature (resolveBinder ValueNamespace name) spanValue (resolveSignaturePayload payload)
        SData spanValue name parameters constructors ->
          SData
            spanValue
            (resolveBinder TypeNamespace name)
            (map (resolveBinder TypeNamespace) parameters)
            (map resolveDataConstructor constructors)
        SClass spanValue name parameters methods ->
          SClass
            spanValue
            (resolveBinder CapabilityNamespace name)
            (map (resolveBinder TypeNamespace) parameters)
            (map resolveClassMethod methods)
        SImpl spanValue name arguments methods ->
          SImpl
            spanValue
            (resolveName CapabilityNamespace name)
            (map resolveConstraintType arguments)
            (map resolveImplMethod methods)
        SModule spanValue path -> SModule spanValue path
        SImport spanValue path alias symbols -> SImport spanValue path alias symbols
        SExpr spanValue value -> SExpr spanValue (resolveExpr value)

    resolveDataConstructor (DataConstructor name arguments) =
      DataConstructor
        (resolveBinder ConstructorNamespace name)
        (map resolveDataConstructorArgument arguments)

    resolveDataConstructorArgument argument =
      case argument of
        DataConstructorArgumentName name ->
          DataConstructorArgumentName (resolveName TypeNamespace name)
        DataConstructorArgumentOpaque -> DataConstructorArgumentOpaque

    resolveClassMethod (ClassMethodSignature name spanValue payload) =
      ClassMethodSignature (resolveBinder ValueNamespace name) spanValue (resolveSignaturePayload payload)

    resolveImplMethod (ImplMethod name spanValue body) =
      ImplMethod (resolveBinder ValueNamespace name) spanValue (resolveExpr body)

    resolveSignaturePayload payload =
      case payload of
        SignatureType signatureType -> SignatureType signatureType
        ConstrainedSignature constraints signatureType ->
          ConstrainedSignature
            (map resolveSignatureConstraint constraints)
            (resolveConstraintType signatureType)
        UnsupportedSignature tokens -> UnsupportedSignature (map resolveSignatureToken tokens)

    resolveSignatureToken token =
      case token of
        SignatureNameToken name -> SignatureNameToken (resolveName TypeNamespace name)
        _ -> token

    resolveSignatureConstraint (SignatureConstraint name arguments) =
      SignatureConstraint (resolveName CapabilityNamespace name) (map resolveConstraintType arguments)

    resolveConstraintType signatureType =
      case signatureType of
        ConstraintTypeName name -> ConstraintTypeName (resolveName TypeNamespace name)
        ConstraintTypeApplication name arguments ->
          ConstraintTypeApplication (resolveName TypeNamespace name) (map resolveConstraintType arguments)
        ConstraintTypeList innerType -> ConstraintTypeList (resolveConstraintType innerType)
        ConstraintTypeTuple elementTypes -> ConstraintTypeTuple (map resolveConstraintType elementTypes)
        ConstraintTypeFunction argumentType resultType ->
          ConstraintTypeFunction (resolveConstraintType argumentType) (resolveConstraintType resultType)

-- | Collect unqualified free references used to validate explicit and alias
-- import visibility before driver replay rewrites names.
collectReferencedNames :: SurfaceExpr -> Set Text
collectReferencedNames = collectExprReferences Set.empty

collectExprReferences :: Set Text -> SurfaceExpr -> Set Text
collectExprReferences boundNames surfaceExpr =
  case surfaceExpr of
    SELit _ -> Set.empty
    SEVar name
      | identifierText name `Set.member` boundNames -> Set.empty
      | otherwise -> Set.singleton (identifierText name)
    SEQualifiedVar _ _ -> Set.empty
    SELambda params body ->
      let parameterList = NonEmpty.toList params
       in Set.union
            (Set.unions (map collectLambdaParameterReferences parameterList))
            ( collectExprReferences
                (Set.union boundNames (Set.unions (map collectLambdaParameterBinders parameterList)))
                body
            )
    SEOperatorValue _ -> Set.empty
    SEList items ->
      Set.unions (map (collectExprReferences boundNames) items)
    SETuple items ->
      Set.unions (map (collectExprReferences boundNames) items)
    SEApply function argument ->
      Set.union
        (collectExprReferences boundNames function)
        (collectExprReferences boundNames argument)
    SETypeApplication function _ ->
      collectExprReferences boundNames function
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
    -- Match analyzer/runtime recursive binding semantics: all `let` binders in
    -- a block are visible while collecting free import references.
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
        SSClass {} -> Set.empty
        SSImpl {} -> Set.empty
        SSModule {} -> Set.empty
        SSImport {} -> Set.empty

collectCaseArmReferences :: Set Text -> SurfaceCaseArm -> Set Text
collectCaseArmReferences boundNames (SurfaceCaseArm patternValue guard body) =
  let armBoundNames = Set.union boundNames (collectPatternBinders patternValue)
   in Set.unions
        [ collectPatternReferences patternValue,
          maybe Set.empty (collectExprReferences armBoundNames) guard,
          collectExprReferences armBoundNames body
        ]

collectPatternReferences :: SurfacePattern -> Set Text
collectPatternReferences patternValue =
  case patternValue of
    SPWildcard -> Set.empty
    SPVariable _ -> Set.empty
    SPLiteral _ -> Set.empty
    SPConstructor constructorName nestedPatterns ->
      Set.insert (identifierText constructorName) (Set.unions (map collectPatternReferences nestedPatterns))
    SPList nestedPatterns ->
      Set.unions (map collectPatternReferences nestedPatterns)
    SPConsList headPattern tailPattern ->
      Set.union (collectPatternReferences headPattern) (collectPatternReferences tailPattern)
    SPTuple nestedPatterns ->
      Set.unions (map collectPatternReferences nestedPatterns)
    SPAs _ nestedPattern ->
      collectPatternReferences nestedPattern
    SPOr alternatives ->
      Set.unions (map collectPatternReferences alternatives)

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
    SPConsList headPattern tailPattern ->
      Set.union (collectPatternBinders headPattern) (collectPatternBinders tailPattern)
    SPTuple nestedPatterns ->
      Set.unions (map collectPatternBinders nestedPatterns)
    SPAs name nestedPattern ->
      Set.insert (identifierText name) (collectPatternBinders nestedPattern)
    SPOr alternatives ->
      commonPatternBinders alternatives

commonPatternBinders :: [SurfacePattern] -> Set Text
commonPatternBinders alternatives =
  case alternatives of
    [] -> Set.empty
    firstAlternative : rest ->
      foldl'
        Set.intersection
        (collectPatternBinders firstAlternative)
        (map collectPatternBinders rest)

collectLambdaParameterReferences :: SurfaceLambdaParameter -> Set Text
collectLambdaParameterReferences parameter =
  case parameter of
    SurfaceLambdaIdentifier _ -> Set.empty
    SurfaceLambdaPattern patternValue -> collectPatternReferences patternValue

collectLambdaParameterBinders :: SurfaceLambdaParameter -> Set Text
collectLambdaParameterBinders parameter =
  case parameter of
    SurfaceLambdaIdentifier name -> Set.singleton (identifierText name)
    SurfaceLambdaPattern patternValue -> collectPatternBinders patternValue

-- Qualified alias lookups live in the module-alias namespace. Lexical binders
-- intentionally do not shadow aliases, and this traversal should stay aligned
-- with `collectExprReferences` whenever new surface expression forms are added.
collectQualifiedReferences :: SurfaceExpr -> Set (Text, Text)
collectQualifiedReferences surfaceExpr =
  case surfaceExpr of
    SELit _ -> Set.empty
    SEVar _ -> Set.empty
    SEQualifiedVar qualifier member ->
      Set.singleton (identifierText qualifier, identifierText member)
    SELambda _ body ->
      collectQualifiedReferences body
    SEOperatorValue _ -> Set.empty
    SEList items ->
      Set.unions (map collectQualifiedReferences items)
    SETuple items ->
      Set.unions (map collectQualifiedReferences items)
    SEApply function argument ->
      Set.union
        (collectQualifiedReferences function)
        (collectQualifiedReferences argument)
    SETypeApplication function _ ->
      collectQualifiedReferences function
    SEIf condition trueBranch falseBranch ->
      Set.unions
        [ collectQualifiedReferences condition,
          collectQualifiedReferences trueBranch,
          collectQualifiedReferences falseBranch
        ]
    SECase scrutinee arms ->
      Set.union
        (collectQualifiedReferences scrutinee)
        (Set.unions (map collectQualifiedCaseArmReferences arms))
    SEBinary _ left right ->
      Set.union
        (collectQualifiedReferences left)
        (collectQualifiedReferences right)
    SESectionLeft left _ ->
      collectQualifiedReferences left
    SESectionRight _ right ->
      collectQualifiedReferences right
    SEBlock statements ->
      Set.unions (map collectQualifiedStatementReferences statements)

collectQualifiedStatementReferences :: SurfaceStatement -> Set (Text, Text)
collectQualifiedStatementReferences statement =
  case statement of
    SSLet _ _ valueExpr ->
      collectQualifiedReferences valueExpr
    SSExpr _ expr ->
      collectQualifiedReferences expr
    SSSignature {} -> Set.empty
    SSData {} -> Set.empty
    SSClass {} -> Set.empty
    SSImpl {} -> Set.empty
    SSModule {} -> Set.empty
    SSImport {} -> Set.empty

collectQualifiedCaseArmReferences :: SurfaceCaseArm -> Set (Text, Text)
collectQualifiedCaseArmReferences (SurfaceCaseArm _ guard body) =
  Set.union
    (maybe Set.empty collectQualifiedReferences guard)
    (collectQualifiedReferences body)

-- | Validate alias and explicit-symbol imports after dependencies have been
-- resolved so the exporting module inventories are known.
validateImportBindings ::
  FilePath ->
  [Text] ->
  [ParsedImport] ->
  Set Text ->
  Set Text ->
  Set (Text, Text) ->
  Set Text ->
  Set Text ->
  Map [Text] (Set Text) ->
  Map [Text] (Set Text) ->
  Either Diagnostic ()
validateImportBindings sourcePath importerPath imports localClassNames referencedNames qualifiedReferences ambientVisibleSymbols ambientVisibleClassNames classExportsByModule exportsByModule = do
  go Map.empty Map.empty imports
  visibleSymbols <- collectVisibleImportSymbols imports
  visibleClassNames <- collectVisibleImportClassNames imports
  validateQualifiedReferences (Set.unions [localClassNames, visibleClassNames, ambientVisibleClassNames])
  let visibleOrAmbientSymbols = Set.union visibleSymbols ambientVisibleSymbols
  case findHiddenExplicitImportReference visibleOrAmbientSymbols of
    Just (symbolName, importDecl) ->
      Left (mkHiddenExplicitImportSymbolError symbolName importDecl)
    Nothing ->
      case findHiddenAliasImportReference visibleOrAmbientSymbols of
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
      case parsedImportAlias importDecl of
        Just _ ->
          Right seenSymbols
        Nothing ->
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
              let exportedClassNames =
                    Map.findWithDefault Set.empty (parsedImportModulePath importDecl) classExportsByModule
                  exportedImportSymbols =
                    Set.union exportedSymbols exportedClassNames
                  importedSymbolNames =
                    case parsedImportSymbols importDecl of
                      Nothing -> Set.toAscList exportedImportSymbols
                      Just explicitSymbolNames -> explicitSymbolNames
               in
              foldM
                (validateImportSymbol importDecl exportedImportSymbols)
                seenSymbols
                importedSymbolNames

    validateQualifiedReferences :: Set Text -> Either Diagnostic ()
    validateQualifiedReferences visibleClassNames =
      foldM
        validateQualifiedReference
        ()
        (Set.toList qualifiedReferences)
      where
        validateQualifiedReference :: () -> (Text, Text) -> Either Diagnostic ()
        validateQualifiedReference () (aliasName, symbolName)
          | Set.member aliasName visibleClassNames =
              Right ()
          | otherwise =
              case findAliasImport aliasName of
                Nothing ->
                  Left (mkUnknownQualifiedAliasError aliasName symbolName)
                Just importDecl ->
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
                    Just exportedSymbols
                      | Set.member symbolName exportedSymbols -> Right ()
                      | otherwise -> Left (mkMissingQualifiedAliasSymbolError symbolName importDecl aliasName exportedSymbols)

    findAliasImport :: Text -> Maybe ParsedImport
    findAliasImport aliasName =
      firstMatch
        [ importDecl
          | importDecl <- imports,
            parsedImportAlias importDecl == Just aliasName
        ]

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
            Just previousOrigin
              | bindingOriginModulePath previousOrigin == parsedImportModulePath importDecl ->
                  Right seenSymbols
              | otherwise ->
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

    mkUnknownQualifiedAliasError :: Text -> Text -> Diagnostic
    mkUnknownQualifiedAliasError aliasName symbolName =
      setDiagnosticSubject aliasName $
        mkDiagnostic
          "E4013"
          ( "qualified import alias '"
              <> aliasName
              <> "' is not declared in module '"
              <> renderModulePath importerPath
              <> "' while resolving '"
              <> aliasName
              <> "::"
              <> symbolName
              <> "' in '"
              <> Text.pack sourcePath
              <> "'"
          )

    mkMissingQualifiedAliasSymbolError :: Text -> ParsedImport -> Text -> Set Text -> Diagnostic
    mkMissingQualifiedAliasSymbolError symbolName importDecl aliasName exportedSymbols =
      setDiagnosticSubject symbolName $
        setDiagnosticPrimarySpan
          (parsedImportSpan importDecl)
          ( mkDiagnostic
              "E4014"
              ( "qualified import symbol '"
                  <> symbolName
                  <> "' is not exported by module '"
                  <> renderModulePath (parsedImportModulePath importDecl)
                  <> "' imported as '"
                  <> aliasName
                  <> "' by '"
                  <> renderModulePath importerPath
                  <> "' in '"
                  <> Text.pack sourcePath
                  <> "'; available exports: "
                  <> renderExports exportedSymbols
              )
          )

    -- Visible imports include all bare imports and explicit symbol-list imports;
    -- alias-only imports intentionally expose nothing unqualified.
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

    collectVisibleImportClassNames :: [ParsedImport] -> Either Diagnostic (Set Text)
    collectVisibleImportClassNames =
      foldM collectVisibleImportClassName Set.empty

    collectVisibleImportClassName :: Set Text -> ParsedImport -> Either Diagnostic (Set Text)
    collectVisibleImportClassName visibleClassNames importDecl =
      case Map.lookup (parsedImportModulePath importDecl) classExportsByModule of
        Nothing ->
          Right visibleClassNames
        Just exportedClassNames ->
          Right
            ( Set.union
                visibleClassNames
                ( case parsedImportAlias importDecl of
                    Just _ -> Set.empty
                    Nothing ->
                      case parsedImportSymbols importDecl of
                        Nothing -> exportedClassNames
                        Just symbolNames -> Set.intersection exportedClassNames (Set.fromList symbolNames)
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
            symbolName <- Set.toList hiddenSymbols,
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
            symbolName <- Set.toList exportedSymbols,
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
