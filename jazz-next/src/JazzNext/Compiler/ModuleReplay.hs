{-# LANGUAGE OverloadedStrings #-}

-- | Module graph replay and import/export visibility rewriting for `jazz-next`.
module JazzNext.Compiler.ModuleReplay
  ( ModuleGraphExpr (..),
    collectNeededLocalCapabilityExports,
    collectTopLevelBindingNames,
    collectTopLevelClassNames,
    loadLoweredModuleGraph,
    loadModuleGraphSource
  ) where

import Data.IORef
  ( newIORef,
    readIORef,
    writeIORef
  )
import Data.List
  ( foldl'
  )
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
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
import JazzNext.Compiler.Diagnostics
  ( Diagnostic,
    SourceSpan (..),
    mkDiagnostic,
    prependDiagnosticSummary,
    setDiagnosticCode
  )
import JazzNext.Compiler.Identifier
  ( Identifier,
    identifierText,
    isOperatorBindingIdentifierText,
    mkIdentifier,
    mkQualifiedIdentifier,
    operatorBindingIdentifierText,
    qualifiedIdentifierText,
    splitQualifiedIdentifierText
  )
import JazzNext.Compiler.ModuleResolver
  ( ModuleResolutionConfig,
    ResolvedModule (..),
    resolveModuleGraphWithLookup,
    resolveModuleGraphWithLookupAndVisibleSymbols
  )
import JazzNext.Compiler.Parser.Operator
  ( isBuiltinOperatorSymbol
  )
import JazzNext.Compiler.SourceProgram
  ( parseAndLowerStandaloneSource,
    scopeStatements
  )

-- | Module graph replay needs two programs: one that keeps dependency
-- expression statements for validation and one that strips them for runtime.
data ModuleGraphExpr = ModuleGraphExpr
  { moduleGraphValidationExpr :: Expr,
    moduleGraphRuntimeExpr :: Expr
  }

-- | Resolve an entry module graph and replay the source texts in dependency
-- order so the rest of the pipeline can still operate on a single source blob.
loadModuleGraphSource ::
  ModuleResolutionConfig ->
  [Text] ->
  (FilePath -> IO (Maybe Text)) ->
  IO (Either Diagnostic Text)
loadModuleGraphSource resolutionConfig entryModulePath sourceLookup = do
  memoizedSourceLookup <- memoizeSourceLookup sourceLookup
  resolutionResult <-
    resolveModuleGraphWithLookup resolutionConfig memoizedSourceLookup entryModulePath
  case resolutionResult of
    Left resolutionError ->
      pure (Left resolutionError)
    Right resolvedModules -> do
      sourceReplayResult <- replayResolvedSources resolvedModules memoizedSourceLookup
      pure (fmap (Text.intercalate "\n") sourceReplayResult)

-- | Resolve and lower a module graph into validation and runtime replay
-- expressions. Dependency expressions stay present for semantic validation and
-- are stripped only from the runtime replay expression.
loadLoweredModuleGraph ::
  Set Text ->
  Set Text ->
  ModuleResolutionConfig ->
  [Text] ->
  (FilePath -> IO (Maybe Text)) ->
  IO (Either Diagnostic ModuleGraphExpr)
loadLoweredModuleGraph ambientVisibleSymbols ambientVisibleClassNames resolutionConfig entryModulePath sourceLookup = do
  memoizedSourceLookup <- memoizeSourceLookup sourceLookup
  resolutionResult <-
    resolveModuleGraphWithLookupAndVisibleSymbols resolutionConfig ambientVisibleSymbols ambientVisibleClassNames memoizedSourceLookup entryModulePath
  case resolutionResult of
    Left resolutionError ->
      pure (Left resolutionError)
    Right resolvedModules -> do
      sourceReplayResult <- replayResolvedSources resolvedModules memoizedSourceLookup
      pure $
        do
          replayedSources <- sourceReplayResult
          loweredModules <-
            sequence
              [ parseAndLowerResolvedModule resolvedModule sourceText
                | (resolvedModule, sourceText) <- zip resolvedModules replayedSources
              ]
          pure (buildModuleGraphExpr entryModulePath resolvedModules loweredModules)

-- | Replay resolved source files from the memoized lookup so driver errors stay
-- stable even after resolution has already succeeded.
replayResolvedSources ::
  [ResolvedModule] ->
  (FilePath -> IO (Maybe Text)) ->
  IO (Either Diagnostic [Text])
replayResolvedSources resolvedModules sourceLookup =
  go [] resolvedModules
  where
    go acc remainingModules =
      case remainingModules of
        [] -> pure (Right (reverse acc))
        resolvedModule : rest -> do
          maybeSource <- sourceLookup (resolvedSourcePath resolvedModule)
          case maybeSource of
            Nothing ->
              pure
                ( Left
                    ( mkDiagnostic
                        "E4001"
                        ( "unresolved import '"
                            <> renderModulePath (resolvedModulePath resolvedModule)
                            <> "'; expected source at '"
                            <> Text.pack (resolvedSourcePath resolvedModule)
                            <> "'"
                        )
                    )
                )
            Just sourceText ->
              go (sourceText : acc) rest

parseAndLowerResolvedModule :: ResolvedModule -> Text -> Either Diagnostic Expr
parseAndLowerResolvedModule resolvedModule sourceText =
  case parseAndLowerStandaloneSource sourceText of
    Left parseError ->
      Left
        ( setDiagnosticCode
            "E4004"
            ( prependDiagnosticSummary
                ( "module parse error at '"
                    <> Text.pack (resolvedSourcePath resolvedModule)
                    <> "': "
                )
                parseError
            )
        )
    Right loweredSource ->
      Right loweredSource

-- | Build validation/runtime replay programs while preserving module import
-- visibility rules through qualified synthetic bindings.
buildModuleGraphExpr ::
  [Text] ->
  [ResolvedModule] ->
  [Expr] ->
  ModuleGraphExpr
buildModuleGraphExpr entryModulePath resolvedModules loweredModules =
  let exportsByModule = collectModuleExports resolvedModules loweredModules
      capabilityExportsByModule = collectModuleCapabilityExports resolvedModules loweredModules
      aliasReferencesByModule = map collectAliasQualifiedReferences loweredModules
      loweredModulesWithAliasReferences = zip loweredModules aliasReferencesByModule
      neededAliasExportsByModule = collectNeededAliasExports exportsByModule loweredModulesWithAliasReferences
      hiddenImportExportsByModule =
        collectHiddenImportExports
          exportsByModule
          loweredModules
      neededVisibleImportExportsByModule =
        collectNeededVisibleImportExports
          exportsByModule
          loweredModules
      neededVisibleImportCapabilityExportsByModule =
        collectNeededVisibleImportCapabilityExports
          capabilityExportsByModule
          loweredModules
      initialNeededModuleExportsByModule =
        Map.unionWith Set.union neededAliasExportsByModule neededVisibleImportExportsByModule
      neededModuleExportsByModule =
        expandNeededModuleExports
          resolvedModules
          loweredModules
          initialNeededModuleExportsByModule
      neededLocalCapabilityExportsByModule =
        collectNeededLocalCapabilityExports
          resolvedModules
          loweredModules
          neededModuleExportsByModule
          neededVisibleImportCapabilityExportsByModule
      hiddenLocalCapabilityExportsByModule =
        collectHiddenLocalCapabilityExports
          resolvedModules
          loweredModules
          neededModuleExportsByModule
          neededVisibleImportCapabilityExportsByModule
      neededCapabilityExportsByModule =
        Map.unionWith Set.union
          neededVisibleImportCapabilityExportsByModule
          neededLocalCapabilityExportsByModule
      (runtimeNeededModuleExportsByModule, runtimeNeededCapabilityExportsByModule) =
        closeRuntimeReplayNeeds
          resolvedModules
          loweredModules
          neededVisibleImportCapabilityExportsByModule
          neededModuleExportsByModule
          neededCapabilityExportsByModule
      replayBridgeModuleExportsByModule =
        Map.unionWith Set.union neededModuleExportsByModule runtimeNeededModuleExportsByModule
      runtimeHiddenLocalCapabilityExportsByModule =
        collectHiddenLocalCapabilityExports
          resolvedModules
          loweredModules
          runtimeNeededModuleExportsByModule
          neededVisibleImportCapabilityExportsByModule
      loweredModulesWithVisibleImportReferences =
        map
          (rewriteVisibleImportReferences hiddenImportExportsByModule exportsByModule)
          loweredModules
      loweredModulesWithValidationAliasBindings =
        zipWith3
          (addAliasImportBindings exportsByModule neededModuleExportsByModule hiddenImportExportsByModule)
          resolvedModules
          loweredModulesWithVisibleImportReferences
          aliasReferencesByModule
      loweredModulesWithRuntimeAliasBindings =
        zipWith3
          (addAliasImportBindings exportsByModule replayBridgeModuleExportsByModule hiddenImportExportsByModule)
          resolvedModules
          loweredModulesWithVisibleImportReferences
          aliasReferencesByModule
      hiddenImportExportsFor resolvedModule =
        Map.findWithDefault Set.empty (resolvedModulePath resolvedModule) hiddenImportExportsByModule
      neededModuleExportsFor resolvedModule =
        Map.findWithDefault Set.empty (resolvedModulePath resolvedModule) neededModuleExportsByModule
      runtimeNeededModuleExportsFor resolvedModule =
        Map.findWithDefault Set.empty (resolvedModulePath resolvedModule) runtimeNeededModuleExportsByModule
      neededModuleCapabilityExportsFor resolvedModule =
        Map.findWithDefault Set.empty (resolvedModulePath resolvedModule) runtimeNeededCapabilityExportsByModule
      directlyNeededModuleCapabilityExportsFor resolvedModule =
        Map.findWithDefault Set.empty (resolvedModulePath resolvedModule) neededVisibleImportCapabilityExportsByModule
      hiddenLocalCapabilityExportsFor resolvedModule =
        Map.findWithDefault Set.empty (resolvedModulePath resolvedModule) hiddenLocalCapabilityExportsByModule
      runtimeHiddenLocalCapabilityExportsFor resolvedModule =
        Map.findWithDefault Set.empty (resolvedModulePath resolvedModule) runtimeHiddenLocalCapabilityExportsByModule
   in
  ModuleGraphExpr
    { moduleGraphValidationExpr =
        replayLoweredModules
          ( \resolvedModule loweredModule ->
              stripModuleDeclarations
                (resolvedModulePath resolvedModule)
                (resolvedModulePath resolvedModule == entryModulePath)
                (hiddenImportExportsFor resolvedModule)
                (neededModuleExportsFor resolvedModule)
                (neededModuleCapabilityExportsFor resolvedModule)
                (directlyNeededModuleCapabilityExportsFor resolvedModule)
                (hiddenLocalCapabilityExportsFor resolvedModule)
                loweredModule
          )
          resolvedModules
          loweredModulesWithValidationAliasBindings,
      moduleGraphRuntimeExpr =
        replayLoweredModules
          ( \resolvedModule loweredModule ->
              stripModuleRuntimeReplayStatements
                (resolvedModulePath resolvedModule)
                (resolvedModulePath resolvedModule == entryModulePath)
                (hiddenImportExportsFor resolvedModule)
                (runtimeNeededModuleExportsFor resolvedModule)
                (neededModuleCapabilityExportsFor resolvedModule)
                (directlyNeededModuleCapabilityExportsFor resolvedModule)
                (runtimeHiddenLocalCapabilityExportsFor resolvedModule)
                loweredModule
          )
          resolvedModules
          loweredModulesWithRuntimeAliasBindings
    }

collectModuleExports :: [ResolvedModule] -> [Expr] -> Map [Text] [Text]
collectModuleExports resolvedModules loweredModules =
  Map.fromList
    [ (resolvedModulePath resolvedModule, collectModuleExportNames loweredModule)
      | (resolvedModule, loweredModule) <- zip resolvedModules loweredModules
    ]

collectModuleExportNames :: Expr -> [Text]
collectModuleExportNames loweredModule =
  filter (not . isOperatorBindingIdentifierText) (collectTopLevelBindingNames loweredModule)

collectTopLevelBindingNames :: Expr -> [Text]
collectTopLevelBindingNames expr =
  case expr of
    EBlock statements ->
      concatMap statementBindingNames statements
    _ -> []

collectTopLevelClassNames :: Expr -> [Text]
collectTopLevelClassNames expr =
  case expr of
    EBlock statements ->
      [ identifierText className
        | SClass _ className _ _ <- statements
      ]
    _ -> []

collectModuleCapabilityExports :: [ResolvedModule] -> [Expr] -> Map [Text] (Set Text)
collectModuleCapabilityExports resolvedModules loweredModules =
  Map.fromList
    [ (resolvedModulePath resolvedModule, collectTopLevelCapabilityNames loweredModule)
      | (resolvedModule, loweredModule) <- zip resolvedModules loweredModules
    ]

collectHiddenImportExports ::
  Map [Text] [Text] ->
  [Expr] ->
  Map [Text] (Set Text)
collectHiddenImportExports exportsByModule loweredModules =
  Map.filter (not . Set.null) importExposures
  where
    importExposures =
      foldl'
        collectImportExposure
        Map.empty
        [ statement
          | EBlock statements <- loweredModules,
            statement <- statements
        ]

    collectImportExposure exposures statement =
      case statement of
        SImport _ modulePath maybeAlias maybeSymbolNames ->
          Map.insertWith
            Set.union
            modulePath
            (importExposure modulePath maybeAlias maybeSymbolNames)
            exposures
        _ -> exposures

    importExposure modulePath maybeAlias maybeSymbolNames =
      let exportedNames =
            Set.fromList (Map.findWithDefault [] modulePath exportsByModule)
       in case maybeAlias of
            Just _ ->
              exportedNames
            Nothing ->
              case maybeSymbolNames of
                Nothing ->
                  Set.empty
                Just symbolNames ->
                  let visibleExports = Set.intersection exportedNames (Set.fromList symbolNames)
                   in Set.difference exportedNames visibleExports

collectNeededVisibleImportExports ::
  Map [Text] [Text] ->
  [Expr] ->
  Map [Text] (Set Text)
collectNeededVisibleImportExports exportsByModule loweredModules =
  Map.unionsWith Set.union
    (map (visibleImportReferencesForModule exportsByModule) loweredModules)

visibleImportReferencesForModule ::
  Map [Text] [Text] ->
  Expr ->
  Map [Text] (Set Text)
visibleImportReferencesForModule exportsByModule expr =
  case expr of
    EBlock statements ->
      Map.fromListWith Set.union
        [ (modulePath, Set.singleton exportedName)
          | SImport _ modulePath Nothing maybeSymbolNames <- statements,
            exportedName <- Set.toList (visibleImportNames modulePath maybeSymbolNames),
            Set.member exportedName referencedNames
        ]
      where
        referencedNames = collectUnqualifiedReferences expr
        visibleImportNames modulePath maybeSymbolNames =
          let exportedNames = Set.fromList (Map.findWithDefault [] modulePath exportsByModule)
           in case maybeSymbolNames of
                Nothing -> exportedNames
                Just symbolNames -> Set.intersection exportedNames (Set.fromList symbolNames)
    _ -> Map.empty

collectNeededVisibleImportCapabilityExports ::
  Map [Text] (Set Text) ->
  [Expr] ->
  Map [Text] (Set Text)
collectNeededVisibleImportCapabilityExports capabilityExportsByModule loweredModules =
  Map.unionsWith Set.union
    (map (visibleImportCapabilityExportsForModule capabilityExportsByModule) loweredModules)

visibleImportCapabilityExportsForModule ::
  Map [Text] (Set Text) ->
  Expr ->
  Map [Text] (Set Text)
visibleImportCapabilityExportsForModule capabilityExportsByModule expr =
  case expr of
    EBlock statements ->
      Map.fromListWith Set.union
        [ (modulePath, visibleCapabilityNames modulePath maybeSymbolNames)
          | SImport _ modulePath Nothing maybeSymbolNames <- statements,
            not (Set.null (visibleCapabilityNames modulePath maybeSymbolNames))
        ]
      where
        visibleCapabilityNames modulePath maybeSymbolNames =
          let exportedCapabilityNames = Map.findWithDefault Set.empty modulePath capabilityExportsByModule
           in case maybeSymbolNames of
                Nothing -> exportedCapabilityNames
                Just symbolNames -> Set.intersection exportedCapabilityNames (Set.fromList symbolNames)
    _ -> Map.empty

collectTopLevelCapabilityNames :: Expr -> Set Text
collectTopLevelCapabilityNames expr =
  case expr of
    EBlock statements ->
      Set.fromList
        ( concatMap
            ( \statement ->
                case statement of
                  SClass _ className _ _ -> [identifierText className]
                  SImpl _ capabilityName _ _ -> [identifierText capabilityName]
                  _ -> []
            )
            statements
        )
    _ -> Set.empty

-- | Qualify references that came from imports whose other exports must stay
-- hidden, preventing dependency names from shadowing prelude/local bindings.
rewriteVisibleImportReferences ::
  Map [Text] (Set Text) ->
  Map [Text] [Text] ->
  Expr ->
  Expr
rewriteVisibleImportReferences hiddenImportExportsByModule exportsByModule expr =
  case expr of
    EBlock statements ->
      EBlock (rewriteBlockReferences importTargets Set.empty statements)
      where
        importTargets = visibleImportReferenceTargets statements
    _ -> expr
  where
    visibleImportReferenceTargets statements =
      Map.fromList
        [ (exportedName, modulePath)
          | SImport _ modulePath Nothing maybeSymbolNames <- statements,
            let hiddenExports = Map.findWithDefault Set.empty modulePath hiddenImportExportsByModule,
            exportedName <- Set.toList (visibleImportNames modulePath maybeSymbolNames),
            Set.member exportedName hiddenExports
        ]

    visibleImportNames modulePath maybeSymbolNames =
      let exportedNames = Set.fromList (Map.findWithDefault [] modulePath exportsByModule)
       in case maybeSymbolNames of
            Nothing -> exportedNames
            Just symbolNames -> Set.intersection exportedNames (Set.fromList symbolNames)

rewriteBlockReferences :: Map Text [Text] -> Set Text -> [Statement] -> [Statement]
rewriteBlockReferences importTargets outerBoundNames statements =
  map (rewriteStatementReferences importTargets blockBoundNames) statements
  where
    blockBoundNames =
      Set.union
        outerBoundNames
        (Set.fromList (concatMap statementBindingNames statements))

statementBindingNames :: Statement -> [Text]
statementBindingNames statement =
  case statement of
    SLet bindingName _ _ ->
      [identifierText bindingName]
    SData _ _ _ constructors ->
      [ identifierText constructorName
        | DataConstructor constructorName _ <- constructors
      ]
    _ -> []

rewriteStatementReferences :: Map Text [Text] -> Set Text -> Statement -> Statement
rewriteStatementReferences importTargets boundNames statement =
  case statement of
    SLet bindingName spanValue valueExpr ->
      SLet bindingName spanValue (rewriteExprReferences importTargets boundNames valueExpr)
    SExpr spanValue exprValue ->
      SExpr spanValue (rewriteExprReferences importTargets boundNames exprValue)
    SImpl spanValue capabilityName arguments methods ->
      SImpl
        spanValue
        capabilityName
        arguments
        [ ImplMethod methodName methodSpan (rewriteExprReferences importTargets boundNames methodExpr)
          | ImplMethod methodName methodSpan methodExpr <- methods
        ]
    _ -> statement

rewriteExprReferences :: Map Text [Text] -> Set Text -> Expr -> Expr
rewriteExprReferences importTargets boundNames expression =
  case expression of
    ELit _ -> expression
    EVar name ->
      EVar (rewriteReferenceIdentifier importTargets boundNames name)
    ELambda parameterName bodyExpr ->
      ELambda
        parameterName
        (rewriteExprReferences importTargets (Set.insert (identifierText parameterName) boundNames) bodyExpr)
    EOperatorValue _ -> expression
    EList elements ->
      EList (map (rewriteExprReferences importTargets boundNames) elements)
    ETuple elements ->
      ETuple (map (rewriteExprReferences importTargets boundNames) elements)
    EApply functionExpr argumentExpr ->
      EApply
        (rewriteExprReferences importTargets boundNames functionExpr)
        (rewriteExprReferences importTargets boundNames argumentExpr)
    EIf conditionExpr trueBranch falseBranch ->
      EIf
        (rewriteExprReferences importTargets boundNames conditionExpr)
        (rewriteExprReferences importTargets boundNames trueBranch)
        (rewriteExprReferences importTargets boundNames falseBranch)
    ECase conditionExpr trueBranch falseBranch ->
      ECase
        (rewriteExprReferences importTargets boundNames conditionExpr)
        (rewriteExprReferences importTargets boundNames trueBranch)
        (rewriteExprReferences importTargets boundNames falseBranch)
    EPatternCase scrutineeExpr caseArms ->
      EPatternCase
        (rewriteExprReferences importTargets boundNames scrutineeExpr)
        [ CaseArm
            (rewritePatternReferences importTargets boundNames patternValue)
            (fmap (rewriteExprReferences importTargets armBoundNames) guardExpr)
            (rewriteExprReferences importTargets armBoundNames bodyExpr)
          | CaseArm patternValue guardExpr bodyExpr <- caseArms,
            let armBoundNames = Set.union boundNames (patternBinders patternValue)
        ]
    EBinary operatorName leftExpr rightExpr ->
      EBinary
        operatorName
        (rewriteExprReferences importTargets boundNames leftExpr)
        (rewriteExprReferences importTargets boundNames rightExpr)
    ESectionLeft leftExpr operatorName ->
      ESectionLeft (rewriteExprReferences importTargets boundNames leftExpr) operatorName
    ESectionRight operatorName rightExpr ->
      ESectionRight operatorName (rewriteExprReferences importTargets boundNames rightExpr)
    EBlock nestedStatements ->
      EBlock (rewriteBlockReferences importTargets boundNames nestedStatements)

rewriteReferenceIdentifier :: Map Text [Text] -> Set Text -> Identifier -> Identifier
rewriteReferenceIdentifier importTargets boundNames name =
  let nameText = identifierText name
   in case Map.lookup nameText importTargets of
        Just modulePath
          | Set.notMember nameText boundNames ->
              mkIdentifier (moduleExportQualifiedName modulePath nameText)
        _ -> name

collectOperatorBindingNames :: Expr -> Set Text
collectOperatorBindingNames expr =
  case expr of
    EBlock statements ->
      Set.fromList
        [ bindingNameText
          | SLet bindingName _ _ <- statements,
            let bindingNameText = identifierText bindingName,
            isOperatorBindingIdentifierText bindingNameText
        ]
    _ -> Set.empty

rewriteOperatorBindingReferences :: [Text] -> Set Text -> Expr -> Expr
rewriteOperatorBindingReferences modulePath replayedOperatorBindings expression =
  case expression of
    ELit _ -> expression
    EVar _ -> expression
    ELambda parameterName bodyExpr ->
      ELambda parameterName (rewriteOperatorBindingReferences modulePath replayedOperatorBindings bodyExpr)
    EOperatorValue operatorName ->
      case operatorReplayReference operatorName of
        Just operatorReference -> EVar operatorReference
        Nothing -> expression
    EList elements ->
      EList (map rewriteOperatorReference elements)
    ETuple elements ->
      ETuple (map rewriteOperatorReference elements)
    EApply functionExpr argumentExpr ->
      EApply
        (rewriteOperatorReference functionExpr)
        (rewriteOperatorReference argumentExpr)
    EIf conditionExpr trueBranch falseBranch ->
      EIf
        (rewriteOperatorReference conditionExpr)
        (rewriteOperatorReference trueBranch)
        (rewriteOperatorReference falseBranch)
    ECase conditionExpr trueBranch falseBranch ->
      ECase
        (rewriteOperatorReference conditionExpr)
        (rewriteOperatorReference trueBranch)
        (rewriteOperatorReference falseBranch)
    EPatternCase scrutineeExpr caseArms ->
      EPatternCase
        (rewriteOperatorReference scrutineeExpr)
        [ CaseArm
            patternValue
            (fmap rewriteOperatorReference guardExpr)
            (rewriteOperatorReference bodyExpr)
          | CaseArm patternValue guardExpr bodyExpr <- caseArms
        ]
    EBinary operatorName leftExpr rightExpr ->
      let rewrittenLeft = rewriteOperatorReference leftExpr
          rewrittenRight = rewriteOperatorReference rightExpr
       in case operatorReplayReference operatorName of
            Just operatorReference ->
              EApply
                (EApply (EVar operatorReference) rewrittenLeft)
                rewrittenRight
            Nothing ->
              EBinary operatorName rewrittenLeft rewrittenRight
    ESectionLeft leftExpr operatorName ->
      let rewrittenLeft = rewriteOperatorReference leftExpr
       in case operatorReplayReference operatorName of
            Just operatorReference ->
              EApply (EVar operatorReference) rewrittenLeft
            Nothing ->
              ESectionLeft rewrittenLeft operatorName
    ESectionRight operatorName rightExpr ->
      let rewrittenRight = rewriteOperatorReference rightExpr
       in case operatorReplayReference operatorName of
            Just operatorReference ->
              EApply
                ( ELambda
                    operatorReplaySectionRightParameter
                    ( ELambda
                        operatorReplaySectionLeftParameter
                        ( EApply
                            (EApply (EVar operatorReference) (EVar operatorReplaySectionLeftParameter))
                            (EVar operatorReplaySectionRightParameter)
                        )
                    )
                )
                rewrittenRight
            Nothing ->
              ESectionRight operatorName rewrittenRight
    EBlock statements ->
      EBlock (map rewriteOperatorReferenceStatement statements)
  where
    rewriteOperatorReference =
      rewriteOperatorBindingReferences modulePath replayedOperatorBindings

    rewriteOperatorReferenceStatement statement =
      case statement of
        SLet bindingName spanValue valueExpr ->
          SLet bindingName spanValue (rewriteOperatorReference valueExpr)
        SExpr spanValue exprValue ->
          SExpr spanValue (rewriteOperatorReference exprValue)
        SImpl spanValue capabilityName arguments methods ->
          SImpl
            spanValue
            capabilityName
            arguments
            [ ImplMethod methodName methodSpan (rewriteOperatorReference methodExpr)
              | ImplMethod methodName methodSpan methodExpr <- methods
            ]
        _ -> statement

    operatorReplayReference operatorName
      | isBuiltinOperatorSymbol operatorName = Nothing
      | Set.member bindingName replayedOperatorBindings =
          Just (mkIdentifier (moduleExportQualifiedName modulePath bindingName))
      | otherwise = Nothing
      where
        bindingName = operatorBindingIdentifierText operatorName

    operatorReplaySectionLeftParameter =
      mkIdentifier "$operator_replay_section_left"

    operatorReplaySectionRightParameter =
      mkIdentifier "$operator_replay_section_right"

rewritePatternReferences :: Map Text [Text] -> Set Text -> Pattern -> Pattern
rewritePatternReferences importTargets boundNames patternValue =
  case patternValue of
    PWildcard -> PWildcard
    PVariable name -> PVariable name
    PLiteral literalValue -> PLiteral literalValue
    PConstructor constructorName nestedPatterns ->
      PConstructor
        (rewriteReferenceIdentifier importTargets boundNames constructorName)
        (map (rewritePatternReferences importTargets boundNames) nestedPatterns)
    PList nestedPatterns ->
      PList (map (rewritePatternReferences importTargets boundNames) nestedPatterns)
    PConsList headPattern tailPattern ->
      PConsList
        (rewritePatternReferences importTargets boundNames headPattern)
        (rewritePatternReferences importTargets boundNames tailPattern)
    PTuple nestedPatterns ->
      PTuple (map (rewritePatternReferences importTargets boundNames) nestedPatterns)
    PAs name nestedPattern ->
      PAs
        name
        ( rewritePatternReferences
            importTargets
            (Set.insert (identifierText name) boundNames)
            nestedPattern
        )
    POr alternatives ->
      POr (map (rewritePatternReferences importTargets boundNames) alternatives)

patternBinders :: Pattern -> Set Text
patternBinders patternValue =
  case patternValue of
    PWildcard -> Set.empty
    PVariable name -> Set.singleton (identifierText name)
    PLiteral _ -> Set.empty
    PConstructor _ nestedPatterns -> Set.unions (map patternBinders nestedPatterns)
    PList nestedPatterns -> Set.unions (map patternBinders nestedPatterns)
    PConsList headPattern tailPattern ->
      Set.union (patternBinders headPattern) (patternBinders tailPattern)
    PTuple nestedPatterns -> Set.unions (map patternBinders nestedPatterns)
    PAs name nestedPattern ->
      Set.insert (identifierText name) (patternBinders nestedPattern)
    POr alternatives ->
      commonPatternBinders alternatives

commonPatternBinders :: [Pattern] -> Set Text
commonPatternBinders alternatives =
  case alternatives of
    [] -> Set.empty
    firstAlternative : rest ->
      foldl'
        Set.intersection
        (patternBinders firstAlternative)
        (map patternBinders rest)

collectUnqualifiedReferences :: Expr -> Set Text
collectUnqualifiedReferences expr =
  case expr of
    ELit _ -> Set.empty
    EVar name ->
      let nameText = identifierText name
       in case splitQualifiedIdentifierText nameText of
            Just _ -> Set.empty
            Nothing -> Set.singleton nameText
    ELambda parameterName bodyExpr ->
      Set.delete (identifierText parameterName) (collectUnqualifiedReferences bodyExpr)
    EOperatorValue operatorName -> operatorBindingReferences operatorName
    EList elements -> Set.unions (map collectUnqualifiedReferences elements)
    ETuple elements -> Set.unions (map collectUnqualifiedReferences elements)
    EApply functionExpr argumentExpr ->
      Set.union (collectUnqualifiedReferences functionExpr) (collectUnqualifiedReferences argumentExpr)
    EIf conditionExpr trueBranch falseBranch ->
      Set.unions
        [ collectUnqualifiedReferences conditionExpr,
          collectUnqualifiedReferences trueBranch,
          collectUnqualifiedReferences falseBranch
        ]
    ECase conditionExpr trueBranch falseBranch ->
      Set.unions
        [ collectUnqualifiedReferences conditionExpr,
          collectUnqualifiedReferences trueBranch,
          collectUnqualifiedReferences falseBranch
        ]
    EPatternCase scrutineeExpr caseArms ->
      Set.unions
        [ collectUnqualifiedReferences scrutineeExpr,
          Set.unions
            [ Set.union
                (patternConstructorReferences patternValue)
                ( Set.difference
                    ( Set.union
                        (maybe Set.empty collectUnqualifiedReferences guardExpr)
                        (collectUnqualifiedReferences bodyExpr)
                    )
                    (patternBinders patternValue)
                )
              | CaseArm patternValue guardExpr bodyExpr <- caseArms
            ]
        ]
    EBinary operatorName leftExpr rightExpr ->
      Set.unions
        [ operatorBindingReferences operatorName,
          collectUnqualifiedReferences leftExpr,
          collectUnqualifiedReferences rightExpr
        ]
    ESectionLeft leftExpr operatorName ->
      Set.union
        (operatorBindingReferences operatorName)
        (collectUnqualifiedReferences leftExpr)
    ESectionRight operatorName rightExpr ->
      Set.union
        (operatorBindingReferences operatorName)
        (collectUnqualifiedReferences rightExpr)
    EBlock statements ->
      Set.difference
        ( Set.unions
            [ case statement of
                SLet _ _ valueExpr -> collectUnqualifiedReferences valueExpr
                SExpr _ exprValue -> collectUnqualifiedReferences exprValue
                SImpl _ _ _ methods ->
                  Set.unions
                    [ collectUnqualifiedReferences methodExpr
                      | ImplMethod _ _ methodExpr <- methods
                    ]
                _ -> Set.empty
              | statement <- statements
            ]
        )
        (Set.fromList (concatMap statementBindingNames statements))

operatorBindingReferences :: Text -> Set Text
operatorBindingReferences operatorName
  | isBuiltinOperatorSymbol operatorName = Set.empty
  | otherwise = Set.singleton (operatorBindingIdentifierText operatorName)

patternConstructorReferences :: Pattern -> Set Text
patternConstructorReferences patternValue =
  case patternValue of
    PWildcard -> Set.empty
    PVariable _ -> Set.empty
    PLiteral _ -> Set.empty
    PConstructor constructorName nestedPatterns ->
      Set.insert (identifierText constructorName) (Set.unions (map patternConstructorReferences nestedPatterns))
    PList nestedPatterns -> Set.unions (map patternConstructorReferences nestedPatterns)
    PConsList headPattern tailPattern ->
      Set.union (patternConstructorReferences headPattern) (patternConstructorReferences tailPattern)
    PTuple nestedPatterns -> Set.unions (map patternConstructorReferences nestedPatterns)
    PAs _ nestedPattern -> patternConstructorReferences nestedPattern
    POr alternatives -> Set.unions (map patternConstructorReferences alternatives)

expandNeededModuleExports ::
  [ResolvedModule] ->
  [Expr] ->
  Map [Text] (Set Text) ->
  Map [Text] (Set Text)
expandNeededModuleExports resolvedModules loweredModules neededByModule =
  foldl' expandModule neededByModule (zip resolvedModules loweredModules)
  where
    expandModule neededByModule (resolvedModule, loweredModule) =
      let modulePath = resolvedModulePath resolvedModule
          neededExports = Map.findWithDefault Set.empty modulePath neededByModule
          expandedExports = closeExportDependencies (collectExportDependencies loweredModule) neededExports
       in if Set.null expandedExports
            then neededByModule
            else Map.insert modulePath expandedExports neededByModule

collectExportDependencies :: Expr -> Map Text (Set Text)
collectExportDependencies expr =
  case expr of
    EBlock statements ->
      let exportedNames =
            Set.fromList (concatMap statementBindingNames statements)
          constructorNamesByType =
            dataConstructorNamesByType statements
          localDataTypeNames =
            Map.keysSet constructorNamesByType
          dataTypeExports =
            dataTypeDependencyExports constructorNamesByType
          signatureDependencies =
            Map.fromListWith
              Set.union
              [ ( identifierText signatureName,
                  dataTypeExports (signaturePayloadDataTypeReferences signaturePayload)
                )
                | SSignature signatureName _ signaturePayload <- statements
              ]
          constructorDependencies =
            Map.fromListWith
              Set.union
              [ ( identifierText constructorName,
                  dataTypeExports
                    ( Set.unions
                        [ constructorArgumentDataTypeReferences localDataTypeNames constructorArgument
                          | constructorArgument <- constructorArguments
                        ]
                    )
                )
                | SData _ _ _ constructors <- statements,
                  DataConstructor constructorName constructorArguments <- constructors
              ]
       in Map.unionWith
            Set.union
            ( Map.fromList
                [ ( identifierText bindingName,
                    Set.unions
                      [ Set.intersection exportedNames (collectUnqualifiedReferences valueExpr),
                        Map.findWithDefault Set.empty (identifierText bindingName) signatureDependencies
                      ]
                  )
                  | SLet bindingName _ valueExpr <- statements
                ]
            )
            constructorDependencies
    _ -> Map.empty

dataConstructorNamesByType :: [Statement] -> Map Text (Set Text)
dataConstructorNamesByType statements =
  Map.fromList
    [ ( identifierText typeName,
        Set.fromList
          [ identifierText constructorName
            | DataConstructor constructorName _ <- constructors
          ]
      )
      | SData _ typeName _ constructors <- statements
    ]

dataTypeDependencyExports :: Map Text (Set Text) -> Set Text -> Set Text
dataTypeDependencyExports constructorNamesByType typeNames =
  Set.unions
    [ Map.findWithDefault Set.empty typeName constructorNamesByType
      | typeName <- Set.toList typeNames
    ]

signaturePayloadDataTypeReferences :: SignaturePayload -> Set Text
signaturePayloadDataTypeReferences signaturePayload =
  case signaturePayload of
    SignatureType _ -> Set.empty
    ConstrainedSignature constraints signatureType ->
      Set.union
        (Set.unions (map signatureConstraintDataTypeReferences constraints))
        (constraintSignatureTypeReferences signatureType)
    UnsupportedSignature tokens ->
      Set.fromList
        [ name
          | SignatureNameToken name <- tokens
        ]

signatureConstraintDataTypeReferences :: SignatureConstraint -> Set Text
signatureConstraintDataTypeReferences (SignatureConstraint _ arguments) =
  Set.unions (map constraintSignatureTypeReferences arguments)

constraintSignatureTypeReferences :: ConstraintSignatureType -> Set Text
constraintSignatureTypeReferences signatureType =
  case signatureType of
    ConstraintTypeName typeName ->
      Set.singleton (identifierText typeName)
    ConstraintTypeApplication typeName arguments ->
      Set.insert (identifierText typeName) (Set.unions (map constraintSignatureTypeReferences arguments))
    ConstraintTypeList innerType ->
      constraintSignatureTypeReferences innerType
    ConstraintTypeTuple elementTypes ->
      Set.unions (map constraintSignatureTypeReferences elementTypes)
    ConstraintTypeFunction argumentType resultType ->
      Set.union
        (constraintSignatureTypeReferences argumentType)
        (constraintSignatureTypeReferences resultType)

constructorArgumentDataTypeReferences :: Set Text -> DataConstructorArgument -> Set Text
constructorArgumentDataTypeReferences localDataTypeNames constructorArgument =
  case constructorArgument of
    DataConstructorArgumentName argumentName ->
      let argumentNameText = identifierText argumentName
       in Set.fromList [argumentNameText | Set.member argumentNameText localDataTypeNames]
    DataConstructorArgumentOpaque -> Set.empty

closeExportDependencies :: Map Text (Set Text) -> Set Text -> Set Text
closeExportDependencies exportDependencies neededExports =
  let expandedExports =
        Set.union
          neededExports
          ( Set.unions
              [ Map.findWithDefault Set.empty exportName exportDependencies
                | exportName <- Set.toList neededExports
              ]
          )
   in if expandedExports == neededExports
        then neededExports
        else closeExportDependencies exportDependencies expandedExports

closeRuntimeReplayNeeds ::
  [ResolvedModule] ->
  [Expr] ->
  Map [Text] (Set Text) ->
  Map [Text] (Set Text) ->
  Map [Text] (Set Text) ->
  (Map [Text] (Set Text), Map [Text] (Set Text))
closeRuntimeReplayNeeds resolvedModules loweredModules directlyNeededCapabilityExportsByModule neededModuleExportsByModule neededCapabilityExportsByModule =
  let neededImplBodyValueExportsByModule =
        collectNeededImplMethodValueExports
          resolvedModules
          loweredModules
          neededCapabilityExportsByModule
      expandedNeededModuleExportsByModule =
        expandNeededModuleExports
          resolvedModules
          loweredModules
          (Map.unionWith Set.union neededModuleExportsByModule neededImplBodyValueExportsByModule)
      expandedNeededLocalCapabilityExportsByModule =
        collectNeededLocalCapabilityExports
          resolvedModules
          loweredModules
          expandedNeededModuleExportsByModule
          directlyNeededCapabilityExportsByModule
      expandedNeededCapabilityExportsByModule =
        Map.unionWith Set.union
          directlyNeededCapabilityExportsByModule
          expandedNeededLocalCapabilityExportsByModule
   in if expandedNeededModuleExportsByModule == neededModuleExportsByModule
        && expandedNeededCapabilityExportsByModule == neededCapabilityExportsByModule
        then (expandedNeededModuleExportsByModule, expandedNeededCapabilityExportsByModule)
        else
          closeRuntimeReplayNeeds
            resolvedModules
            loweredModules
            directlyNeededCapabilityExportsByModule
            expandedNeededModuleExportsByModule
            expandedNeededCapabilityExportsByModule

collectNeededImplMethodValueExports ::
  [ResolvedModule] ->
  [Expr] ->
  Map [Text] (Set Text) ->
  Map [Text] (Set Text)
collectNeededImplMethodValueExports resolvedModules loweredModules neededCapabilityExportsByModule =
  Map.fromList
    [ (modulePath, neededValueExports)
      | (resolvedModule, loweredModule) <- zip resolvedModules loweredModules,
        let modulePath = resolvedModulePath resolvedModule,
        let neededCapabilities = Map.findWithDefault Set.empty modulePath neededCapabilityExportsByModule,
        let valueExports = Set.fromList (collectTopLevelBindingNames loweredModule),
        let neededValueExports = implMethodValueDependencies loweredModule valueExports neededCapabilities,
        not (Set.null neededValueExports)
    ]

implMethodValueDependencies :: Expr -> Set Text -> Set Text -> Set Text
implMethodValueDependencies expr valueExports neededCapabilities =
  case expr of
    EBlock statements ->
      Set.intersection
        valueExports
        ( Set.unions
            [ collectUnqualifiedReferences methodExpr
              | SImpl _ capabilityName _ methods <- statements,
                Set.member (identifierText capabilityName) neededCapabilities,
                ImplMethod _ _ methodExpr <- methods
            ]
        )
    _ ->
      Set.empty

collectNeededLocalCapabilityExports ::
  [ResolvedModule] ->
  [Expr] ->
  Map [Text] (Set Text) ->
  Map [Text] (Set Text) ->
  Map [Text] (Set Text)
collectNeededLocalCapabilityExports resolvedModules loweredModules neededModuleExportsByModule directlyNeededCapabilityExportsByModule =
  Map.fromList
    [ (modulePath, neededCapabilities)
      | (resolvedModule, loweredModule) <- zip resolvedModules loweredModules,
        let modulePath = resolvedModulePath resolvedModule,
        let neededExports = Map.findWithDefault Set.empty modulePath neededModuleExportsByModule,
        let directlyNeededCapabilities = Map.findWithDefault Set.empty modulePath directlyNeededCapabilityExportsByModule,
        let neededCapabilities = localCapabilityDependenciesForExports loweredModule neededExports directlyNeededCapabilities,
        not (Set.null neededCapabilities)
    ]

collectHiddenLocalCapabilityExports ::
  [ResolvedModule] ->
  [Expr] ->
  Map [Text] (Set Text) ->
  Map [Text] (Set Text) ->
  Map [Text] (Set Text)
collectHiddenLocalCapabilityExports resolvedModules loweredModules neededModuleExportsByModule directlyNeededCapabilityExportsByModule =
  Map.fromList
    [ (modulePath, hiddenCapabilities)
      | (resolvedModule, loweredModule) <- zip resolvedModules loweredModules,
        let modulePath = resolvedModulePath resolvedModule,
        let neededExports = Map.findWithDefault Set.empty modulePath neededModuleExportsByModule,
        let directlyNeededCapabilities = Map.findWithDefault Set.empty modulePath directlyNeededCapabilityExportsByModule,
        let hiddenCapabilities = hiddenLocalCapabilityDependenciesForExports loweredModule neededExports directlyNeededCapabilities,
        not (Set.null hiddenCapabilities)
    ]

localCapabilityDependenciesForExports :: Expr -> Set Text -> Set Text -> Set Text
localCapabilityDependenciesForExports expr neededExports directlyNeededCapabilities =
  localCapabilityDependenciesForExportsWithDirectRoots True expr neededExports directlyNeededCapabilities

hiddenLocalCapabilityDependenciesForExports :: Expr -> Set Text -> Set Text -> Set Text
hiddenLocalCapabilityDependenciesForExports expr neededExports directlyNeededCapabilities =
  localCapabilityDependenciesForExportsWithDirectRoots False expr neededExports directlyNeededCapabilities

localCapabilityDependenciesForExportsWithDirectRoots :: Bool -> Expr -> Set Text -> Set Text -> Set Text
localCapabilityDependenciesForExportsWithDirectRoots includeDirectRoots expr neededExports directlyNeededCapabilities =
  case expr of
    EBlock statements ->
      closeLocalCapabilityDependencies statements localCapabilityNames directDependencies
      where
        localCapabilityNames =
          if includeDirectRoots
            then collectTopLevelCapabilityNames expr
            else Set.fromList (collectTopLevelClassNames expr)
        directDependencies =
          if includeDirectRoots
            then Set.union directlyNeededCapabilities directExportDependencies
            else
              Set.union
                directExportDependencies
                (Set.difference directRootClosure directlyNeededCapabilities)
        directExportDependencies =
          Set.unions
            [ Set.unions
                [ collectLocalCapabilityReferences localCapabilityNames valueExpr
                  | SLet bindingName _ valueExpr <- statements,
                    Set.member (identifierText bindingName) neededExports
                ],
              Set.unions
                [ collectLocalCapabilityReferencesFromSignaturePayload localCapabilityNames signaturePayload
                  | SSignature signatureName _ signaturePayload <- statements,
                    Set.member (identifierText signatureName) neededExports
                ],
              Set.unions
                [ collectLocalCapabilityReferencesFromSignaturePayload localCapabilityNames methodSignature
                  | SClass _ className _ methods <- statements,
                    Set.member (identifierText className) directlyNeededCapabilities,
                    ClassMethodSignature _ _ methodSignature <- methods
                ]
            ]
        directRootClosure =
          closeLocalCapabilityDependencies statements localCapabilityNames directlyNeededCapabilities
    _ -> Set.empty

collectLocalCapabilityReferencesFromSignaturePayload :: Set Text -> SignaturePayload -> Set Text
collectLocalCapabilityReferencesFromSignaturePayload localCapabilityNames signaturePayload =
  case signaturePayload of
    ConstrainedSignature constraints signatureType ->
      Set.union
        (Set.unions (map (collectLocalCapabilityReferencesFromConstraint localCapabilityNames) constraints))
        (collectLocalCapabilityReferencesFromConstraintType localCapabilityNames signatureType)
    _ -> Set.empty

collectLocalCapabilityReferencesFromConstraint :: Set Text -> SignatureConstraint -> Set Text
collectLocalCapabilityReferencesFromConstraint localCapabilityNames (SignatureConstraint constraintName arguments) =
  Set.unions
    ( [ Set.singleton constraintNameText
        | Set.member constraintNameText localCapabilityNames
      ]
        ++ map (collectLocalCapabilityReferencesFromConstraintType localCapabilityNames) arguments
    )
  where
    constraintNameText = identifierText constraintName

collectLocalCapabilityReferencesFromConstraintType :: Set Text -> ConstraintSignatureType -> Set Text
collectLocalCapabilityReferencesFromConstraintType localCapabilityNames signatureType =
  case signatureType of
    ConstraintTypeName typeName ->
      let typeNameText = identifierText typeName
       in Set.fromList [typeNameText | Set.member typeNameText localCapabilityNames]
    ConstraintTypeApplication typeName arguments ->
      Set.filter
        (`Set.member` localCapabilityNames)
        ( Set.insert
            (identifierText typeName)
            (Set.unions (map (collectLocalCapabilityReferencesFromConstraintType localCapabilityNames) arguments))
        )
    ConstraintTypeList innerType ->
      collectLocalCapabilityReferencesFromConstraintType localCapabilityNames innerType
    ConstraintTypeTuple elementTypes ->
      Set.unions (map (collectLocalCapabilityReferencesFromConstraintType localCapabilityNames) elementTypes)
    ConstraintTypeFunction argumentType resultType ->
      Set.union
        (collectLocalCapabilityReferencesFromConstraintType localCapabilityNames argumentType)
        (collectLocalCapabilityReferencesFromConstraintType localCapabilityNames resultType)

closeLocalCapabilityDependencies :: [Statement] -> Set Text -> Set Text -> Set Text
closeLocalCapabilityDependencies statements localCapabilityNames neededCapabilities =
  let expandedCapabilities =
        Set.union
          neededCapabilities
          ( Set.unions
              [ Set.unions
                  [ collectLocalCapabilityReferencesFromSignaturePayload localCapabilityNames methodSignature
                    | SClass _ capabilityName _ methods <- statements,
                      Set.member (identifierText capabilityName) neededCapabilities,
                      ClassMethodSignature _ _ methodSignature <- methods
                  ],
                Set.unions
                  [ Set.unions
                      [ collectLocalCapabilityReferences localCapabilityNames methodExpr
                        | ImplMethod _ _ methodExpr <- methods
                      ]
                    | SImpl _ capabilityName _ methods <- statements,
                      Set.member (identifierText capabilityName) neededCapabilities
                  ]
              ]
          )
   in if expandedCapabilities == neededCapabilities
        then neededCapabilities
        else closeLocalCapabilityDependencies statements localCapabilityNames expandedCapabilities

collectLocalCapabilityReferences :: Set Text -> Expr -> Set Text
collectLocalCapabilityReferences localCapabilityNames expr =
  Set.union
    qualifiedCapabilityReferences
    (collectInferredLocalCapabilityReferences localCapabilityNames expr)
  where
    qualifiedCapabilityReferences =
      Set.fromList
        [ capabilityName
          | (capabilityName, _) <- Set.toList (collectAliasQualifiedReferencePairs expr),
            Set.member capabilityName localCapabilityNames
        ]

collectInferredLocalCapabilityReferences :: Set Text -> Expr -> Set Text
collectInferredLocalCapabilityReferences localCapabilityNames expr
  | Set.member "Eq" localCapabilityNames && expressionUsesStrictEquality expr =
      Set.singleton "Eq"
  | otherwise =
      Set.empty

expressionUsesStrictEquality :: Expr -> Bool
expressionUsesStrictEquality expr =
  case expr of
    ELit _ -> False
    EVar _ -> False
    ELambda _ bodyExpr ->
      expressionUsesStrictEquality bodyExpr
    EOperatorValue operatorName ->
      isStrictEqualityOperator operatorName
    EList elements ->
      any expressionUsesStrictEquality elements
    ETuple elements ->
      any expressionUsesStrictEquality elements
    EApply functionExpr argumentExpr ->
      expressionUsesStrictEquality functionExpr
        || expressionUsesStrictEquality argumentExpr
    EIf conditionExpr trueBranch falseBranch ->
      any
        expressionUsesStrictEquality
        [conditionExpr, trueBranch, falseBranch]
    ECase conditionExpr trueBranch falseBranch ->
      any
        expressionUsesStrictEquality
        [conditionExpr, trueBranch, falseBranch]
    EPatternCase scrutineeExpr caseArms ->
      expressionUsesStrictEquality scrutineeExpr
        || any
          ( \(CaseArm _ guardExpr bodyExpr) ->
              maybe False expressionUsesStrictEquality guardExpr
                || expressionUsesStrictEquality bodyExpr
          )
          caseArms
    EBinary operatorName leftExpr rightExpr ->
      isStrictEqualityOperator operatorName
        || expressionUsesStrictEquality leftExpr
        || expressionUsesStrictEquality rightExpr
    ESectionLeft leftExpr operatorName ->
      isStrictEqualityOperator operatorName
        || expressionUsesStrictEquality leftExpr
    ESectionRight operatorName rightExpr ->
      isStrictEqualityOperator operatorName
        || expressionUsesStrictEquality rightExpr
    EBlock statements ->
      any statementUsesStrictEquality statements

statementUsesStrictEquality :: Statement -> Bool
statementUsesStrictEquality statement =
  case statement of
    SLet _ _ valueExpr ->
      expressionUsesStrictEquality valueExpr
    SExpr _ expr ->
      expressionUsesStrictEquality expr
    SImpl _ _ _ methods ->
      any
        ( \(ImplMethod _ _ methodExpr) ->
            expressionUsesStrictEquality methodExpr
        )
        methods
    _ -> False

isStrictEqualityOperator :: Text -> Bool
isStrictEqualityOperator operatorName =
  operatorName == "==" || operatorName == "!="

collectNeededAliasExports ::
  Map [Text] [Text] ->
  [(Expr, Map Text (Set Text))] ->
  Map [Text] (Set Text)
collectNeededAliasExports exportsByModule =
  foldl' collectModule Map.empty
  where
    collectModule neededExports (expr, aliasReferences) =
      Map.unionWith Set.union neededExports (collectNeededAliasExportsFromModule expr aliasReferences)

    collectNeededAliasExportsFromModule expr aliasReferences =
      case expr of
        EBlock statements ->
          foldl' (collectImportNeededExports aliasReferences) Map.empty statements
        _ -> Map.empty

    collectImportNeededExports aliasReferences neededExports statement =
      case statement of
        SImport _ modulePath (Just aliasName) Nothing ->
          let referencedNames = Map.findWithDefault Set.empty aliasName aliasReferences
              exportedNames = Set.fromList (Map.findWithDefault [] modulePath exportsByModule)
              neededNames = Set.intersection referencedNames exportedNames
           in if Set.null neededNames
                then neededExports
                else Map.insertWith Set.union modulePath neededNames neededExports
        _ -> neededExports

-- | Insert synthetic alias-qualified bridge bindings required by `Alias::name`
-- references without making alias-only exports visible unqualified.
addAliasImportBindings ::
  Map [Text] [Text] ->
  Map [Text] (Set Text) ->
  Map [Text] (Set Text) ->
  ResolvedModule ->
  Expr ->
  Map Text (Set Text) ->
  Expr
addAliasImportBindings exportsByModule neededModuleExportsByModule hiddenImportExportsByModule resolvedModule expr aliasReferences =
  case expr of
    EBlock statements ->
      EBlock (insertAliasBindings (concatMap aliasBindingsForStatement statements) statements)
    _ -> expr
  where
    sourceExportNames =
      Map.findWithDefault Set.empty (resolvedModulePath resolvedModule) neededModuleExportsByModule

    hiddenSourceExportNames =
      Map.findWithDefault Set.empty (resolvedModulePath resolvedModule) hiddenImportExportsByModule

    insertAliasBindings aliasBindings statements =
      case statements of
        moduleStatement@(SModule _ _) : rest ->
          moduleStatement : aliasBindings ++ concatMap expandStatement rest
        _ ->
          aliasBindings ++ concatMap expandStatement statements

    expandStatement statement =
      statement : sourceExportBindingsForStatement statement

    sourceExportBindingsForStatement statement =
      case statement of
        SLet exportedName spanValue valueExpr
          | Set.member (identifierText exportedName) sourceExportNames,
            not (isOperatorBindingIdentifierText (identifierText exportedName)) ->
              [ SLet
                  (mkIdentifier (moduleExportQualifiedName (resolvedModulePath resolvedModule) (identifierText exportedName)))
                  spanValue
                  ( if Set.member (identifierText exportedName) hiddenSourceExportNames
                      then
                        rewriteModuleExportReferences
                          (resolvedModulePath resolvedModule)
                          sourceExportNames
                          valueExpr
                      else EVar exportedName
                  )
              ]
        _ -> []

    aliasBindingsForStatement statement =
      case statement of
        SImport spanValue modulePath (Just aliasName) Nothing ->
          [ SLet
              (mkQualifiedIdentifier aliasName exportedName)
              spanValue
              (EVar (mkIdentifier (moduleExportQualifiedName modulePath exportedName)))
            | let referencedNames = Map.findWithDefault Set.empty aliasName aliasReferences,
              let exportedNames = Set.fromList (Map.findWithDefault [] modulePath exportsByModule),
              exportedName <- Set.toList (Set.intersection referencedNames exportedNames)
          ]
        _ -> []

moduleExportQualifiedName :: [Text] -> Text -> Text
moduleExportQualifiedName modulePath exportedName =
  qualifiedIdentifierText "__module" (renderModulePath modulePath <> "::" <> exportedName)

rewriteModuleExportReferences :: [Text] -> Set Text -> Expr -> Expr
rewriteModuleExportReferences modulePath exportNames =
  rewriteExprReferences importTargets Set.empty
  where
    importTargets =
      Map.fromList
        [ (exportName, modulePath)
          | exportName <- Set.toList exportNames
        ]

collectAliasQualifiedReferences :: Expr -> Map Text (Set Text)
collectAliasQualifiedReferences expr =
  Map.fromListWith Set.union
    [ (aliasName, Set.singleton memberName)
      | (aliasName, memberName) <- Set.toList (collectAliasQualifiedReferencePairs expr)
    ]

collectAliasQualifiedReferencePairs :: Expr -> Set (Text, Text)
collectAliasQualifiedReferencePairs expr =
  case expr of
    ELit _ -> Set.empty
    EVar name ->
      case splitQualifiedIdentifierText (identifierText name) of
        Just qualifiedName -> Set.singleton qualifiedName
        Nothing -> Set.empty
    ELambda _ bodyExpr ->
      collectAliasQualifiedReferencePairs bodyExpr
    EOperatorValue _ -> Set.empty
    EList elements ->
      Set.unions (map collectAliasQualifiedReferencePairs elements)
    ETuple elements ->
      Set.unions (map collectAliasQualifiedReferencePairs elements)
    EApply functionExpr argumentExpr ->
      Set.union
        (collectAliasQualifiedReferencePairs functionExpr)
        (collectAliasQualifiedReferencePairs argumentExpr)
    EIf conditionExpr trueBranch falseBranch ->
      Set.unions
        [ collectAliasQualifiedReferencePairs conditionExpr,
          collectAliasQualifiedReferencePairs trueBranch,
          collectAliasQualifiedReferencePairs falseBranch
        ]
    ECase conditionExpr trueBranch falseBranch ->
      Set.unions
        [ collectAliasQualifiedReferencePairs conditionExpr,
          collectAliasQualifiedReferencePairs trueBranch,
          collectAliasQualifiedReferencePairs falseBranch
        ]
    EPatternCase scrutineeExpr caseArms ->
      Set.unions
        ( collectAliasQualifiedReferencePairs scrutineeExpr :
          [ Set.union
              (collectAliasQualifiedReferencePairsFromPattern patternValue)
              ( Set.union
                  (maybe Set.empty collectAliasQualifiedReferencePairs guardExpr)
                  (collectAliasQualifiedReferencePairs bodyExpr)
              )
          | CaseArm patternValue guardExpr bodyExpr <- caseArms
          ]
        )
    EBinary _ leftExpr rightExpr ->
      Set.union
        (collectAliasQualifiedReferencePairs leftExpr)
        (collectAliasQualifiedReferencePairs rightExpr)
    ESectionLeft leftExpr _ ->
      collectAliasQualifiedReferencePairs leftExpr
    ESectionRight _ rightExpr ->
      collectAliasQualifiedReferencePairs rightExpr
    EBlock statements ->
      Set.unions (map collectAliasQualifiedReferencesFromStatement statements)

collectAliasQualifiedReferencesFromStatement :: Statement -> Set (Text, Text)
collectAliasQualifiedReferencesFromStatement statement =
  case statement of
    SLet _ _ valueExpr ->
      collectAliasQualifiedReferencePairs valueExpr
    SExpr _ expr ->
      collectAliasQualifiedReferencePairs expr
    SImpl _ _ _ methods ->
      Set.unions
        [ collectAliasQualifiedReferencePairs methodExpr
          | ImplMethod _ _ methodExpr <- methods
        ]
    SSignature {} -> Set.empty
    SData {} -> Set.empty
    SClass {} -> Set.empty
    SModule {} -> Set.empty
    SImport {} -> Set.empty

collectAliasQualifiedReferencePairsFromPattern :: Pattern -> Set (Text, Text)
collectAliasQualifiedReferencePairsFromPattern patternValue =
  case patternValue of
    PWildcard -> Set.empty
    PVariable _ -> Set.empty
    PLiteral _ -> Set.empty
    PConstructor constructorName nestedPatterns ->
      Set.union
        ( case splitQualifiedIdentifierText (identifierText constructorName) of
            Just qualifiedName -> Set.singleton qualifiedName
            Nothing -> Set.empty
        )
        (Set.unions (map collectAliasQualifiedReferencePairsFromPattern nestedPatterns))
    PList nestedPatterns ->
      Set.unions (map collectAliasQualifiedReferencePairsFromPattern nestedPatterns)
    PConsList headPattern tailPattern ->
      Set.union
        (collectAliasQualifiedReferencePairsFromPattern headPattern)
        (collectAliasQualifiedReferencePairsFromPattern tailPattern)
    PTuple nestedPatterns ->
      Set.unions (map collectAliasQualifiedReferencePairsFromPattern nestedPatterns)
    PAs _ nestedPattern ->
      collectAliasQualifiedReferencePairsFromPattern nestedPattern
    POr alternatives ->
      Set.unions (map collectAliasQualifiedReferencePairsFromPattern alternatives)

replayLoweredModules ::
  (ResolvedModule -> Expr -> Expr) ->
  [ResolvedModule] ->
  [Expr] ->
  Expr
replayLoweredModules transformModule resolvedModules loweredModules =
  EBlock
    ( concat
        [ scopeStatements (transformModule resolvedModule loweredModule)
          | (resolvedModule, loweredModule) <- zip resolvedModules loweredModules
        ]
    )

stripModuleDeclarations :: [Text] -> Bool -> Set Text -> Set Text -> Set Text -> Set Text -> Set Text -> Expr -> Expr
stripModuleDeclarations modulePath isEntryModule hiddenImportExports neededModuleExports _neededCapabilityExports directlyNeededCapabilityExports hiddenCapabilityExports expr =
  case expr of
    EBlock statements ->
      EBlock (ensureModuleValidationBoundary (concatMap keepModuleValidationStatement statements))
    _ -> expr
  where
    ensureModuleValidationBoundary statements =
      case statements of
        SModule {} : _ -> statements
        _ -> SModule (SourceSpan 1 1) modulePath : statements

    keepModuleValidationStatement statement =
      case statement of
        SModule {} -> [statement]
        SLet bindingName spanValue valueExpr
          | shouldQualifyOperatorBinding bindingName ->
              [ SLet
                  (operatorReplayIdentifier bindingName)
                  spanValue
                  (rewriteValidationReplayExpr valueExpr)
              ]
        SLet bindingName spanValue valueExpr
          | Set.member (identifierText bindingName) hiddenImportExports ->
              if Set.member (identifierText bindingName) neededModuleExports
                then []
                else
                  [ SLet
                      (hiddenValidationIdentifier bindingName)
                      spanValue
                      (rewriteValidationReplayExpr valueExpr)
                  ]
        SLet bindingName spanValue valueExpr ->
          [ SLet
              bindingName
              spanValue
              (rewriteValidationReplayExpr valueExpr)
          ]
        SExpr spanValue exprValue ->
          [ SExpr
              spanValue
              (rewriteValidationReplayExpr exprValue)
          ]
        SSignature signatureName spanValue signatureValue
          | shouldQualifyOperatorBinding signatureName ->
              [ SSignature
                  (operatorReplayIdentifier signatureName)
                  spanValue
                  (rewriteValidationReplaySignaturePayload signatureValue)
              ]
        SSignature signatureName spanValue signatureValue
          | Set.member (identifierText signatureName) hiddenImportExports ->
              [ SSignature
                  (hiddenValidationIdentifier signatureName)
                  spanValue
                  (rewriteValidationReplaySignaturePayload signatureValue)
              ]
        SSignature signatureName spanValue signatureValue ->
          [ SSignature
              signatureName
              spanValue
              (rewriteValidationReplayVisibleSignaturePayload signatureValue)
          ]
        SData spanValue typeName typeParameters constructors ->
          rewriteDataStatementForReplay
            modulePath
            dataTypeNames
            hiddenImportExports
            (Set.union hiddenImportExports neededModuleExports)
            spanValue
            typeName
            typeParameters
            constructors
        SClass spanValue capabilityName parameters methods ->
          [ SClass spanValue replayCapabilityName parameters replayMethods
            | replayCapabilityName <- validationReplayCapabilityNames capabilityName,
              let replayMethods = rewriteValidationReplayClassMethods methods
          ]
        SImpl spanValue capabilityName arguments methods ->
          [ SImpl spanValue replayCapabilityName replayArguments replayMethods
            | replayCapabilityName <- validationReplayCapabilityNames capabilityName,
              let replayArguments = rewriteValidationReplayImplArguments arguments,
              let replayMethods = rewriteValidationReplayImplMethods methods
          ]
        _ -> [statement]
    dataTypeNames = collectDataTypeNames expr
    replayedOperatorBindings =
      if isEntryModule
        then Set.empty
        else collectOperatorBindingNames expr
    hiddenValidationCapabilities =
      if isEntryModule
        then Set.empty
        else hiddenCapabilityExports

    shouldQualifyOperatorBinding bindingName =
      Set.member (identifierText bindingName) replayedOperatorBindings

    operatorReplayIdentifier name =
      mkIdentifier (moduleExportQualifiedName modulePath (identifierText name))

    rewriteValidationReplayExpr =
      rewriteHiddenCapabilityReferences modulePath hiddenValidationCapabilities
        . rewriteOperatorBindingReferences modulePath replayedOperatorBindings
        . rewriteModuleExportReferences modulePath hiddenImportExports

    rewriteValidationReplayClassMethods methods =
      [ ClassMethodSignature methodName methodSpan (rewriteValidationReplaySignaturePayload methodSignature)
        | ClassMethodSignature methodName methodSpan methodSignature <- methods
      ]

    rewriteValidationReplayImplArguments =
      map rewriteValidationReplayConstraintType

    rewriteValidationReplayImplMethods methods =
      [ ImplMethod methodName methodSpan (rewriteValidationReplayExpr methodExpr)
        | ImplMethod methodName methodSpan methodExpr <- methods
      ]

    rewriteValidationReplaySignaturePayload signaturePayload =
      rewriteReplaySignaturePayload
        rewriteValidationReplaySignatureConstraint
        rewriteValidationReplayConstraintType
        rewriteValidationReplaySignatureToken
        signaturePayload

    rewriteValidationReplayVisibleSignaturePayload signaturePayload =
      if isEntryModule
        then signaturePayload
        else rewriteValidationReplaySignaturePayload signaturePayload

    rewriteValidationReplaySignatureConstraint =
      rewriteReplaySignatureConstraint validationReplayCapabilityName rewriteValidationReplayConstraintType

    rewriteValidationReplayConstraintType signatureType =
      rewriteReplayConstraintType modulePath dataTypeNames validationReplayCapabilityName signatureType

    rewriteValidationReplaySignatureToken signatureToken =
      case rewriteModuleExportSignatureToken modulePath dataTypeNames signatureToken of
        SignatureNameToken name
          | Set.member name hiddenValidationCapabilities ->
              SignatureNameToken (moduleExportQualifiedName modulePath name)
        rewrittenToken -> rewrittenToken

    hiddenValidationIdentifier name =
      mkIdentifier (moduleExportQualifiedName modulePath (identifierText name))

    validationReplayCapabilityNames capabilityName =
      let capabilityNameText = identifierText capabilityName
          hiddenNames =
            [ mkIdentifier (moduleExportQualifiedName modulePath capabilityNameText)
              | Set.member capabilityNameText hiddenValidationCapabilities
            ]
          visibleNames =
            [ capabilityName
              | Set.notMember capabilityNameText hiddenValidationCapabilities
                  || Set.member capabilityNameText directlyNeededCapabilityExports
            ]
       in hiddenNames ++ visibleNames

    validationReplayCapabilityName capabilityName =
      case validationReplayCapabilityNames capabilityName of
        replayCapabilityName : _ -> replayCapabilityName
        [] -> capabilityName

stripModuleRuntimeReplayStatements :: [Text] -> Bool -> Set Text -> Set Text -> Set Text -> Set Text -> Set Text -> Expr -> Expr
stripModuleRuntimeReplayStatements modulePath isEntryModule hiddenImportExports neededModuleExports neededCapabilityExports directlyNeededCapabilityExports hiddenCapabilityExports expr =
  case expr of
    EBlock statements ->
      EBlock (ensureModuleRuntimeBoundary (concatMap keepModuleRuntimeReplayStatement statements))
    _ -> expr
  where
    ensureModuleRuntimeBoundary statements =
      case statements of
        SModule {} : _ -> statements
        _ -> SModule (SourceSpan 1 1) modulePath : statements

    keepModuleRuntimeReplayStatement statement =
      case statement of
        SModule {} -> [statement]
        SExpr spanValue exprValue ->
          [ SExpr
              spanValue
              (rewriteRuntimeReplayExpr exprValue)
            | isEntryModule
          ]
        SData spanValue typeName typeParameters constructors ->
          rewriteDataStatementForReplay modulePath dataTypeNames hiddenImportExports neededModuleExports spanValue typeName typeParameters constructors
        SClass spanValue capabilityName parameters methods ->
          [ SClass spanValue replayCapabilityName parameters replayMethods
            | isEntryModule || Set.member (identifierText capabilityName) neededCapabilityExports,
              replayCapabilityName <- runtimeReplayCapabilityNames capabilityName,
              let replayMethods = rewriteRuntimeReplayClassMethods methods
          ]
        SLet bindingName spanValue valueExpr
          | shouldQualifyOperatorBinding bindingName,
            shouldKeepRuntimeBinding bindingName ->
              [ SLet
                  (operatorReplayIdentifier bindingName)
                  spanValue
                  (rewriteRuntimeReplayExpr valueExpr)
              ]
        SLet bindingName _ _
          | shouldQualifyOperatorBinding bindingName -> []
        SLet bindingName spanValue valueExpr
          | shouldKeepRuntimeBinding bindingName,
            Set.notMember (identifierText bindingName) hiddenImportExports ->
              [ SLet
                  bindingName
                  spanValue
                  (rewriteRuntimeReplayExpr valueExpr)
              ]
        SLet {} -> []
        SSignature signatureName spanValue signatureValue
          | shouldQualifyOperatorBinding signatureName,
            shouldKeepRuntimeBinding signatureName ->
              [ SSignature
                  (operatorReplayIdentifier signatureName)
                  spanValue
                  (rewriteRuntimeReplaySignaturePayload signatureValue)
              ]
        SSignature signatureName spanValue signatureValue
          | shouldKeepRuntimeBinding signatureName,
            Set.notMember (identifierText signatureName) hiddenImportExports ->
              [ SSignature
                  signatureName
                  spanValue
                  (rewriteRuntimeReplaySignaturePayload signatureValue)
              ]
        SSignature signatureName spanValue signatureValue
          | Set.member (identifierText signatureName) hiddenImportExports,
            Set.member (identifierText signatureName) neededModuleExports ->
              [ SSignature
                  (hiddenValidationIdentifier signatureName)
                  spanValue
                  (rewriteRuntimeReplaySignaturePayload signatureValue)
              ]
        SSignature {} -> []
        SImpl spanValue capabilityName arguments methods ->
          [ SImpl spanValue replayCapabilityName replayArguments replayMethods
            | isEntryModule || Set.member (identifierText capabilityName) neededCapabilityExports,
              replayCapabilityName <- runtimeReplayCapabilityNames capabilityName,
              let replayArguments = rewriteRuntimeReplayImplArguments arguments,
              let replayMethods = rewriteRuntimeReplayImplMethods methods
          ]
        _ | isHiddenImportExportStatement hiddenImportExports statement -> []
        _ -> [statement]
    dataTypeNames = collectDataTypeNames expr
    hiddenRuntimeCapabilities =
      if isEntryModule
        then Set.empty
        else hiddenCapabilityExports
    replayedOperatorBindings =
      if isEntryModule
        then Set.empty
        else collectOperatorBindingNames expr

    shouldKeepRuntimeBinding bindingName =
      isEntryModule || isNeededRuntimeBindingName (identifierText bindingName)

    isNeededRuntimeBindingName bindingNameText =
      Set.member bindingNameText neededModuleExports
        || case Text.stripPrefix (moduleExportQualifiedPrefix modulePath) bindingNameText of
          Just exportedName -> Set.member exportedName neededModuleExports
          Nothing -> False

    shouldQualifyOperatorBinding bindingName =
      Set.member (identifierText bindingName) replayedOperatorBindings

    operatorReplayIdentifier name =
      mkIdentifier (moduleExportQualifiedName modulePath (identifierText name))

    hiddenValidationIdentifier name =
      mkIdentifier (moduleExportQualifiedName modulePath (identifierText name))

    runtimeReplayCapabilityNames capabilityName =
      let capabilityNameText = identifierText capabilityName
          hiddenNames =
            [ mkIdentifier (moduleExportQualifiedName modulePath capabilityNameText)
              | Set.member capabilityNameText hiddenRuntimeCapabilities
            ]
          visibleNames =
            [ capabilityName
              | Set.notMember capabilityNameText hiddenRuntimeCapabilities
                  || Set.member capabilityNameText directlyNeededCapabilityExports
            ]
       in hiddenNames ++ visibleNames

    runtimeReplayCapabilityName capabilityName =
      case runtimeReplayCapabilityNames capabilityName of
        replayCapabilityName : _ -> replayCapabilityName
        [] -> capabilityName

    rewriteRuntimeReplayClassMethods methods =
      [ ClassMethodSignature methodName methodSpan (rewriteRuntimeReplaySignaturePayload methodSignature)
        | ClassMethodSignature methodName methodSpan methodSignature <- methods
      ]

    rewriteRuntimeReplayImplArguments =
      map rewriteRuntimeReplayConstraintType

    rewriteRuntimeReplayImplMethods methods =
      [ ImplMethod
          methodName
          methodSpan
          (rewriteRuntimeReplayExpr methodExpr)
        | ImplMethod methodName methodSpan methodExpr <- methods
      ]

    rewriteRuntimeReplayExpr =
      rewriteHiddenCapabilityReferences modulePath hiddenRuntimeCapabilities
        . rewriteOperatorBindingReferences modulePath replayedOperatorBindings
        . rewriteModuleExportReferences modulePath hiddenImportExports

    rewriteRuntimeReplaySignaturePayload signaturePayload =
      rewriteReplaySignaturePayload
        rewriteRuntimeReplaySignatureConstraint
        rewriteRuntimeReplayConstraintType
        rewriteRuntimeReplaySignatureToken
        signaturePayload

    rewriteRuntimeReplaySignatureConstraint =
      rewriteReplaySignatureConstraint runtimeReplayCapabilityName rewriteRuntimeReplayConstraintType

    rewriteRuntimeReplayConstraintType signatureType =
      rewriteReplayConstraintType modulePath dataTypeNames runtimeReplayCapabilityName signatureType

    rewriteRuntimeReplaySignatureToken signatureToken =
      rewriteModuleExportSignatureToken modulePath dataTypeNames signatureToken

rewriteReplaySignaturePayload ::
  (SignatureConstraint -> SignatureConstraint) ->
  (ConstraintSignatureType -> ConstraintSignatureType) ->
  (SignatureToken -> SignatureToken) ->
  SignaturePayload ->
  SignaturePayload
rewriteReplaySignaturePayload rewriteConstraint rewriteConstraintType rewriteSignatureToken signaturePayload =
  case signaturePayload of
    ConstrainedSignature constraints signatureType ->
      ConstrainedSignature
        (map rewriteConstraint constraints)
        (rewriteConstraintType signatureType)
    UnsupportedSignature signatureTokens ->
      UnsupportedSignature (map rewriteSignatureToken signatureTokens)
    _ -> signaturePayload

rewriteReplaySignatureConstraint ::
  (Identifier -> Identifier) ->
  (ConstraintSignatureType -> ConstraintSignatureType) ->
  SignatureConstraint ->
  SignatureConstraint
rewriteReplaySignatureConstraint rewriteCapabilityName rewriteConstraintType (SignatureConstraint constraintName arguments) =
  SignatureConstraint
    (rewriteCapabilityName constraintName)
    (map rewriteConstraintType arguments)

rewriteReplayConstraintType ::
  [Text] ->
  Set Text ->
  (Identifier -> Identifier) ->
  ConstraintSignatureType ->
  ConstraintSignatureType
rewriteReplayConstraintType modulePath dataTypeNames rewriteCapabilityName signatureType =
  case rewriteModuleExportImplArgument modulePath dataTypeNames signatureType of
    ConstraintTypeName name ->
      ConstraintTypeName (rewriteCapabilityName name)
    ConstraintTypeApplication name arguments ->
      ConstraintTypeApplication
        (rewriteCapabilityName name)
        (map (rewriteReplayConstraintType modulePath dataTypeNames rewriteCapabilityName) arguments)
    ConstraintTypeList innerType ->
      ConstraintTypeList (rewriteReplayConstraintType modulePath dataTypeNames rewriteCapabilityName innerType)
    ConstraintTypeTuple elementTypes ->
      ConstraintTypeTuple (map (rewriteReplayConstraintType modulePath dataTypeNames rewriteCapabilityName) elementTypes)
    ConstraintTypeFunction argumentType resultType ->
      ConstraintTypeFunction
        (rewriteReplayConstraintType modulePath dataTypeNames rewriteCapabilityName argumentType)
        (rewriteReplayConstraintType modulePath dataTypeNames rewriteCapabilityName resultType)

moduleExportQualifiedPrefix :: [Text] -> Text
moduleExportQualifiedPrefix modulePath =
  qualifiedIdentifierText "__module" (renderModulePath modulePath <> "::")

rewriteHiddenCapabilityReferences :: [Text] -> Set Text -> Expr -> Expr
rewriteHiddenCapabilityReferences modulePath hiddenCapabilities =
  rewriteExprCapabilityReferences Set.empty
  where
    rewriteExprCapabilityReferences boundNames expression =
      case expression of
        ELit _ -> expression
        EVar name ->
          EVar (rewriteCapabilityReferenceIdentifier boundNames name)
        ELambda parameterName bodyExpr ->
          ELambda
            parameterName
            (rewriteExprCapabilityReferences (Set.insert (identifierText parameterName) boundNames) bodyExpr)
        EOperatorValue _ -> expression
        EList elements ->
          EList (map (rewriteExprCapabilityReferences boundNames) elements)
        ETuple elements ->
          ETuple (map (rewriteExprCapabilityReferences boundNames) elements)
        EApply functionExpr argumentExpr ->
          EApply
            (rewriteExprCapabilityReferences boundNames functionExpr)
            (rewriteExprCapabilityReferences boundNames argumentExpr)
        EIf conditionExpr trueBranch falseBranch ->
          EIf
            (rewriteExprCapabilityReferences boundNames conditionExpr)
            (rewriteExprCapabilityReferences boundNames trueBranch)
            (rewriteExprCapabilityReferences boundNames falseBranch)
        ECase conditionExpr trueBranch falseBranch ->
          ECase
            (rewriteExprCapabilityReferences boundNames conditionExpr)
            (rewriteExprCapabilityReferences boundNames trueBranch)
            (rewriteExprCapabilityReferences boundNames falseBranch)
        EPatternCase scrutineeExpr caseArms ->
          EPatternCase
            (rewriteExprCapabilityReferences boundNames scrutineeExpr)
            [ CaseArm
                patternValue
                (fmap (rewriteExprCapabilityReferences armBoundNames) guardExpr)
                (rewriteExprCapabilityReferences armBoundNames bodyExpr)
              | CaseArm patternValue guardExpr bodyExpr <- caseArms,
                let armBoundNames = Set.union boundNames (patternBinders patternValue)
            ]
        EBinary operatorName leftExpr rightExpr ->
          EBinary
            operatorName
            (rewriteExprCapabilityReferences boundNames leftExpr)
            (rewriteExprCapabilityReferences boundNames rightExpr)
        ESectionLeft leftExpr operatorName ->
          ESectionLeft (rewriteExprCapabilityReferences boundNames leftExpr) operatorName
        ESectionRight operatorName rightExpr ->
          ESectionRight operatorName (rewriteExprCapabilityReferences boundNames rightExpr)
        EBlock nestedStatements ->
          EBlock (rewriteBlockCapabilityReferences boundNames nestedStatements)

    rewriteBlockCapabilityReferences outerBoundNames statements =
      map (rewriteStatementCapabilityReferences blockBoundNames) statements
      where
        blockBoundNames =
          Set.union
            outerBoundNames
            (Set.fromList (concatMap statementBindingNames statements))

    rewriteStatementCapabilityReferences boundNames statement =
      case statement of
        SLet bindingName spanValue valueExpr ->
          SLet bindingName spanValue (rewriteExprCapabilityReferences boundNames valueExpr)
        SExpr spanValue exprValue ->
          SExpr spanValue (rewriteExprCapabilityReferences boundNames exprValue)
        SImpl spanValue capabilityName arguments methods ->
          SImpl
            spanValue
            capabilityName
            arguments
            [ ImplMethod methodName methodSpan (rewriteExprCapabilityReferences boundNames methodExpr)
              | ImplMethod methodName methodSpan methodExpr <- methods
            ]
        _ -> statement

    rewriteCapabilityReferenceIdentifier boundNames name =
      let nameText = identifierText name
       in case splitQualifiedIdentifierText nameText of
        Just (capabilityName, methodName)
          | Set.member capabilityName hiddenCapabilities,
            Set.notMember nameText boundNames ->
              mkIdentifier (qualifiedIdentifierText (moduleExportQualifiedName modulePath capabilityName) methodName)
        _ -> name

collectDataTypeNames :: Expr -> Set Text
collectDataTypeNames expr =
  case expr of
    EBlock statements ->
      Set.fromList
        [ identifierText typeName
          | SData _ typeName _ _ <- statements
        ]
    _ -> Set.empty

rewriteModuleExportImplArguments ::
  [Text] ->
  Set Text ->
  [ConstraintSignatureType] ->
  [ConstraintSignatureType]
rewriteModuleExportImplArguments modulePath dataTypeNames arguments =
  map (rewriteModuleExportImplArgument modulePath dataTypeNames) arguments

rewriteModuleExportClassMethods ::
  [Text] ->
  Set Text ->
  [ClassMethodSignature] ->
  [ClassMethodSignature]
rewriteModuleExportClassMethods modulePath dataTypeNames methods =
  [ ClassMethodSignature
      methodName
      methodSpan
      (rewriteModuleExportSignaturePayload modulePath dataTypeNames methodSignature)
    | ClassMethodSignature methodName methodSpan methodSignature <- methods
  ]

rewriteModuleExportSignaturePayload ::
  [Text] ->
  Set Text ->
  SignaturePayload ->
  SignaturePayload
rewriteModuleExportSignaturePayload modulePath dataTypeNames signaturePayload =
  case signaturePayload of
    ConstrainedSignature constraints signatureType ->
      ConstrainedSignature
        (map (rewriteModuleExportSignatureConstraint modulePath dataTypeNames) constraints)
        (rewriteModuleExportImplArgument modulePath dataTypeNames signatureType)
    UnsupportedSignature signatureTokens ->
      UnsupportedSignature (map (rewriteModuleExportSignatureToken modulePath dataTypeNames) signatureTokens)
    _ -> signaturePayload

rewriteModuleExportSignatureConstraint ::
  [Text] ->
  Set Text ->
  SignatureConstraint ->
  SignatureConstraint
rewriteModuleExportSignatureConstraint modulePath dataTypeNames (SignatureConstraint constraintName arguments) =
  SignatureConstraint
    constraintName
    (map (rewriteModuleExportImplArgument modulePath dataTypeNames) arguments)

rewriteModuleExportSignatureToken ::
  [Text] ->
  Set Text ->
  SignatureToken ->
  SignatureToken
rewriteModuleExportSignatureToken modulePath dataTypeNames signatureToken =
  case signatureToken of
    SignatureNameToken name
      | Set.member name dataTypeNames ->
          SignatureNameToken (moduleExportQualifiedName modulePath name)
    _ -> signatureToken

rewriteModuleExportImplArgument ::
  [Text] ->
  Set Text ->
  ConstraintSignatureType ->
  ConstraintSignatureType
rewriteModuleExportImplArgument modulePath dataTypeNames signatureType =
  case signatureType of
    ConstraintTypeName name ->
      ConstraintTypeName (rewriteModuleExportImplTypeName modulePath dataTypeNames name)
    ConstraintTypeApplication name arguments ->
      ConstraintTypeApplication
        (rewriteModuleExportImplTypeName modulePath dataTypeNames name)
        (map (rewriteModuleExportImplArgument modulePath dataTypeNames) arguments)
    ConstraintTypeList innerType ->
      ConstraintTypeList (rewriteModuleExportImplArgument modulePath dataTypeNames innerType)
    ConstraintTypeTuple elementTypes ->
      ConstraintTypeTuple (map (rewriteModuleExportImplArgument modulePath dataTypeNames) elementTypes)
    ConstraintTypeFunction argumentType resultType ->
      ConstraintTypeFunction
        (rewriteModuleExportImplArgument modulePath dataTypeNames argumentType)
        (rewriteModuleExportImplArgument modulePath dataTypeNames resultType)

rewriteModuleExportImplTypeName :: [Text] -> Set Text -> Identifier -> Identifier
rewriteModuleExportImplTypeName modulePath dataTypeNames typeName =
  let typeNameText = identifierText typeName
   in if Set.member typeNameText dataTypeNames
        then mkIdentifier (moduleExportQualifiedName modulePath typeNameText)
        else typeName

rewriteModuleExportImplMethods :: [Text] -> Set Text -> [ImplMethod] -> [ImplMethod]
rewriteModuleExportImplMethods modulePath hiddenImportExports methods =
  [ ImplMethod methodName methodSpan (rewriteModuleExportReferences modulePath hiddenImportExports methodExpr)
    | ImplMethod methodName methodSpan methodExpr <- methods
  ]

rewriteDataStatementForReplay ::
  [Text] ->
  Set Text ->
  Set Text ->
  Set Text ->
  SourceSpan ->
  Identifier ->
  [Identifier] ->
  [DataConstructor] ->
  [Statement]
rewriteDataStatementForReplay modulePath dataTypeNames hiddenImportExports neededModuleExports spanValue typeName typeParameters constructors =
  [ SData spanValue replayTypeName typeParameters replayConstructors
    | not (null replayConstructors)
  ]
  where
    replayTypeName =
      mkIdentifier (moduleExportQualifiedName modulePath (identifierText typeName))

    replayConstructors =
      [ replayConstructor
        | DataConstructor constructorName constructorArguments <- constructors,
          let constructorText = identifierText constructorName,
          let hiddenConstructor = Set.member constructorText hiddenImportExports,
          not hiddenConstructor || Set.member constructorText neededModuleExports,
          let replayConstructorArguments = map replayConstructorArgument constructorArguments,
          let replayConstructor =
                if hiddenConstructor
                  then DataConstructor (mkIdentifier (moduleExportQualifiedName modulePath constructorText)) replayConstructorArguments
                  else DataConstructor constructorName replayConstructorArguments
      ]

    replayConstructorArgument constructorArgument =
      case constructorArgument of
        DataConstructorArgumentName argumentName
          | Set.member (identifierText argumentName) dataTypeNames ->
              DataConstructorArgumentName (rewriteModuleExportImplTypeName modulePath dataTypeNames argumentName)
        _ -> constructorArgument

isHiddenImportExportStatement :: Set Text -> Statement -> Bool
isHiddenImportExportStatement hiddenImportExports statement =
  case statement of
    SLet bindingName _ _ -> Set.member (identifierText bindingName) hiddenImportExports
    SSignature signatureName _ _ -> Set.member (identifierText signatureName) hiddenImportExports
    _ -> False

renderModulePath :: [Text] -> Text
renderModulePath segments = Text.intercalate "::" segments

-- | Memoize source lookups so module resolution and source replay do not read
-- the same file repeatedly.
memoizeSourceLookup ::
  (FilePath -> IO (Maybe Text)) ->
  IO (FilePath -> IO (Maybe Text))
memoizeSourceLookup sourceLookup = do
  cacheRef <- newIORef (Map.empty :: Map FilePath (Maybe Text))
  pure $
    \path -> do
      cache <- readIORef cacheRef
      case Map.lookup path cache of
        Just cachedSource ->
          pure cachedSource
        Nothing -> do
          loadedSource <- sourceLookup path
          writeIORef cacheRef (Map.insert path loadedSource cache)
          pure loadedSource
