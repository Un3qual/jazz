{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Lexical and imported name resolution for lowered expressions.
module Jazz.Compiler.ModuleResolver.Names
  ( ResolutionContext (..),
    resolveNode,
    resolveExprNames,
    resolveStandaloneExprNames,
  )
where

import Data.Bifunctor
  ( bimap,
  )
import Data.List.NonEmpty
  ( NonEmpty,
  )
import Data.Map.Strict
  ( Map,
  )
import qualified Data.Map.Strict as Map
import Data.Maybe
  ( fromMaybe,
  )
import qualified Data.Set as Set
import Jazz.Compiler.AST
  ( CaseArm (..),
    ClassMethodSignature (..),
    CoreNode (..),
    CorePhase (..),
    DataConstructor (..),
    Expr (..),
    ImplMethod (..),
    Pattern (..),
    Statement (..),
  )
import Jazz.Compiler.BuiltinCatalog
  ( kernelBuiltinNames,
    lookupKernelBuiltinSymbol,
  )
import Jazz.Compiler.Diagnostics
  ( Diagnostic,
  )
import Jazz.Compiler.ModuleExports
  ( ModuleExport (..),
    ModuleExportInventory,
    exportInventory,
    exportNamesInNamespace,
    exportOrigin,
    firstExportNamespace,
    selectExportNames,
  )
import qualified Jazz.Compiler.ModuleGraph as ModuleGraph
import Jazz.Compiler.ModuleIdentity
  ( ModulePath,
  )
import Jazz.Compiler.ModuleResolver.Imports
  ( resolverImportAlias,
    resolverImportModulePath,
    resolverImportSymbols,
  )
import Jazz.Compiler.Name
  ( Name (..),
    NameNamespace (..),
    ResolvedNameOrigin (..),
    ResolvedUserName (..),
    SourceName (..),
    identifierText,
    mkIdentifier,
    sourceName,
  )
import Jazz.Compiler.RecursiveBindings
  ( buildRecursiveScopeFacts,
    recursiveScopeBindingNames,
    recursiveScopeGroups,
  )
import Jazz.Compiler.TypeRepresentation
  ( pattern ConstrainedSignature,
    pattern SignatureConstraint,
    pattern SignatureType,
    pattern UnsupportedSignature,
  )

data ResolutionContext = ResolutionContext
  { resolutionAmbientExports :: ModuleExportInventory,
    resolutionLocalInventory :: ModuleExportInventory,
    resolutionInventoriesByModule :: Map ModulePath ModuleExportInventory,
    resolutionImports :: [ModuleGraph.ModuleImport 'Lowered]
  }

resolveNode :: CoreNode 'Lowered sort -> CoreNode 'Resolved sort
resolveNode (CoreNode nodeId spanValue ()) = CoreNode nodeId spanValue ()

resolveExprNames ::
  ResolutionContext ->
  Expr 'Lowered ->
  Either (NonEmpty Diagnostic) (Expr 'Resolved)
resolveExprNames context rootExpression = Right (resolveExpr Map.empty rootExpression)
  where
    ambientExports = resolutionAmbientExports context
    localInventory = resolutionLocalInventory context
    inventoriesByModule = resolutionInventoriesByModule context
    imports = resolutionImports context
    ambientValues = exportNamesInNamespace ValueNamespace ambientExports
    ambientConstructors = exportNamesInNamespace ConstructorNamespace ambientExports
    ambientTypes = exportNamesInNamespace TypeNamespace ambientExports
    ambientClasses = exportNamesInNamespace CapabilityNamespace ambientExports
    localValues = exportNamesInNamespace ValueNamespace localInventory
    localDataTypes = exportNamesInNamespace TypeNamespace localInventory
    localConstructors = exportNamesInNamespace ConstructorNamespace localInventory
    localClasses = exportNamesInNamespace CapabilityNamespace localInventory

    aliasPaths =
      Map.fromList
        [ (aliasName, resolverImportModulePath importDecl)
        | importDecl <- imports,
          Just aliasName <- [resolverImportAlias importDecl]
        ]

    visibleValueOrigins =
      Map.fromList
        [ (name, modulePath)
        | importDecl <- imports,
          resolverImportAlias importDecl == Nothing,
          let modulePath = resolverImportModulePath importDecl,
          name <- Set.toList (exportNamesInNamespace ValueNamespace (visibleDependencyInventory importDecl))
        ]

    visibleConstructorOrigins =
      Map.fromList
        [ (name, modulePath)
        | importDecl <- imports,
          resolverImportAlias importDecl == Nothing,
          let modulePath = resolverImportModulePath importDecl,
          name <- Set.toList (exportNamesInNamespace ConstructorNamespace (visibleDependencyInventory importDecl))
        ]

    visibleTypeOrigins =
      Map.fromList
        [ (name, modulePath)
        | importDecl <- imports,
          resolverImportAlias importDecl == Nothing,
          let modulePath = resolverImportModulePath importDecl,
          name <- Set.toList (exportNamesInNamespace TypeNamespace (visibleDependencyInventory importDecl))
        ]

    visibleClassOrigins =
      Map.fromList
        [ (name, modulePath)
        | importDecl <- imports,
          resolverImportAlias importDecl == Nothing,
          let modulePath = resolverImportModulePath importDecl,
          name <- Set.toList (exportNamesInNamespace CapabilityNamespace (visibleDependencyInventory importDecl))
        ]

    visibleDependencyInventory importDecl =
      case Map.lookup (resolverImportModulePath importDecl) inventoriesByModule of
        Nothing -> exportInventory []
        Just inventory ->
          selectExportNames
            (resolverImportSymbols importDecl)
            inventory

    resolveName boundValues namespace name =
      case name of
        UserName (UnqualifiedSourceName identifier) -> resolveUnqualified boundValues namespace identifier
        UserName (QualifiedSourceName qualifier member) ->
          let qualifierText = identifierText qualifier
              memberText = identifierText member
           in case Map.lookup qualifierText aliasPaths of
                Just dependencyPath ->
                  UserName
                    ( ResolvedUserName
                        (resolvedImportOrigin dependencyPath (importedNamespace dependencyPath memberText namespace) memberText)
                        (importedNamespace dependencyPath memberText namespace)
                        member
                    )
                Nothing ->
                  UserName
                    ( ResolvedUserName
                        (classOrigin qualifierText)
                        ValueNamespace
                        (mkIdentifier (qualifierText <> "::" <> memberText))
                    )
        UserName (QualifiedMethodSourceName alias className method) ->
          let member = mkIdentifier (identifierText className <> "::" <> identifierText method)
           in case Map.lookup (identifierText alias) aliasPaths of
                Just dependencyPath ->
                  UserName (ResolvedUserName (resolvedImportOrigin dependencyPath CapabilityNamespace (identifierText className)) ValueNamespace member)
                Nothing ->
                  -- Keep an unresolved alias in the name rather than falling back
                  -- to a same-spelled local or ambient class.
                  UserName
                    ( ResolvedUserName
                        CurrentModule
                        ValueNamespace
                        (mkIdentifier (identifierText alias <> "::" <> identifierText member))
                    )
        BuiltinName identifier -> BuiltinName identifier
        GeneratedName generatedKind -> GeneratedName generatedKind

    resolveUnqualified boundValues namespace identifier
      | Map.lookup nameText boundValues == Just namespace =
          UserName (ResolvedUserName CurrentModule namespace identifier)
      | localName namespace nameText =
          UserName (ResolvedUserName CurrentModule namespace identifier)
      | Just dependencyPath <- importedOrigin namespace nameText =
          UserName
            ( ResolvedUserName
                (resolvedImportOrigin dependencyPath (importedNamespace dependencyPath nameText namespace) nameText)
                (importedNamespace dependencyPath nameText namespace)
                identifier
            )
      | ambientName namespace nameText =
          UserName (ResolvedUserName AmbientPrelude namespace identifier)
      | namespace == ValueNamespace,
        Just _ <- lookupKernelBuiltinSymbol nameText =
          BuiltinName identifier
      | otherwise =
          UserName (ResolvedUserName CurrentModule namespace identifier)
      where
        nameText = identifierText identifier

    localName namespace nameText =
      case namespace of
        ValueNamespace -> Set.member nameText localValues
        ConstructorNamespace -> Set.member nameText localConstructors
        CapabilityNamespace -> Set.member nameText localClasses
        TypeNamespace -> Set.member nameText localDataTypes

    importedOrigin namespace nameText =
      case namespace of
        ConstructorNamespace -> Map.lookup nameText visibleConstructorOrigins
        TypeNamespace -> Map.lookup nameText visibleTypeOrigins
        CapabilityNamespace -> Map.lookup nameText visibleClassOrigins
        _ -> Map.lookup nameText visibleValueOrigins

    importedNamespace dependencyPath nameText fallbackNamespace
      | fallbackNamespace /= ValueNamespace = fallbackNamespace
      | otherwise =
          fromMaybe
            fallbackNamespace
            ( firstExportNamespace
                [ValueNamespace, ConstructorNamespace, CapabilityNamespace]
                nameText
                dependencyInventory
            )
      where
        dependencyInventory =
          Map.findWithDefault (exportInventory []) dependencyPath inventoriesByModule

    resolvedImportOrigin dependencyPath namespace nameText =
      ImportedModule (exportOrigin dependencyPath (ModuleExport namespace nameText) (Map.findWithDefault mempty dependencyPath inventoriesByModule))

    ambientName namespace nameText =
      case namespace of
        ValueNamespace -> Set.member nameText ambientValues
        ConstructorNamespace -> Set.member nameText ambientConstructors
        TypeNamespace -> Set.member nameText ambientTypes
        CapabilityNamespace -> Set.member nameText ambientClasses

    classOrigin className
      | Set.member className localClasses = CurrentModule
      | Just dependencyPath <- Map.lookup className visibleClassOrigins = resolvedImportOrigin dependencyPath CapabilityNamespace className
      | Set.member className ambientClasses = AmbientPrelude
      | otherwise = CurrentModule

    resolveExpr boundValues expression =
      case expression of
        ELit node literal -> ELit (resolveNode node) literal
        EVar node name -> EVar (resolveNode node) (resolveName boundValues (referenceNamespace boundValues name) name)
        ELambda node parameter body ->
          let lambdaBoundValues = maybe boundValues (\name -> Map.insert name ValueNamespace boundValues) (sourceNameText parameter)
           in ELambda (resolveNode node) (resolveBinder ValueNamespace parameter) (resolveExpr lambdaBoundValues body)
        EOperatorValue node symbol -> EOperatorValue (resolveNode node) symbol
        EList node items -> EList (resolveNode node) (map (resolveExpr boundValues) items)
        ETuple node items -> ETuple (resolveNode node) (map (resolveExpr boundValues) items)
        EApply node function argument ->
          EApply (resolveNode node) (resolveExpr boundValues function) (resolveExpr boundValues argument)
        ETypeApplication node function spanValue signatureType ->
          ETypeApplication (resolveNode node) (resolveExpr boundValues function) spanValue (resolveSignatureType signatureType)
        EIf node condition trueBranch falseBranch ->
          EIf
            (resolveNode node)
            (resolveExpr boundValues condition)
            (resolveExpr boundValues trueBranch)
            (resolveExpr boundValues falseBranch)
        EPatternCase node scrutinee arms ->
          EPatternCase (resolveNode node) (resolveExpr boundValues scrutinee) (map (resolveCaseArm boundValues) arms)
        EBinary node symbol left right ->
          EBinary (resolveNode node) symbol (resolveExpr boundValues left) (resolveExpr boundValues right)
        ESectionLeft node left symbol -> ESectionLeft (resolveNode node) (resolveExpr boundValues left) symbol
        ESectionRight node symbol right -> ESectionRight (resolveNode node) symbol (resolveExpr boundValues right)
        EBlock node statements ->
          EBlock (resolveNode node) (resolveBlockStatements boundValues statements)

    resolveBlockStatements initialBoundValues statements =
      reverse resolvedStatementsRev
      where
        indexedStatements = zip [0 ..] statements
        bindingNamesByStatement = recursiveScopeBindingNames recursiveScopeFactsValue
        outerBindingNames =
          Set.map
            (sourceName . mkIdentifier)
            ( Set.unions
                [ Map.keysSet initialBoundValues,
                  ambientValues,
                  ambientConstructors,
                  Map.keysSet visibleValueOrigins,
                  Map.keysSet visibleConstructorOrigins,
                  kernelBuiltinNames
                ]
            )
        recursiveScopeFactsValue = buildRecursiveScopeFacts outerBindingNames indexedStatements
        recursiveGroupsByStatement = recursiveScopeGroups recursiveScopeFactsValue
        (_, resolvedStatementsRev) = foldl' resolveBlockStatement (initialBoundValues, []) indexedStatements

        resolveBlockStatement (visibleBoundValues, resolvedRev) (statementIndex, statement) =
          let statementBoundValues =
                case statement of
                  SLet _ bindingName _ ->
                    Map.unions
                      [ maybe Map.empty (selfBoundValue visibleBoundValues) (sourceNameText bindingName),
                        Map.fromSet (const ValueNamespace) (Set.filter (\name -> Map.lookup name visibleBoundValues /= Just ConstructorNamespace) (recursivePeerBoundValues statementIndex)),
                        visibleBoundValues
                      ]
                  _ -> visibleBoundValues
              resolvedStatement = resolveStatement statementBoundValues statement
              nextVisibleBoundValues =
                case statement of
                  SLet _ bindingName _ ->
                    maybe visibleBoundValues (\name -> Map.insert name ValueNamespace visibleBoundValues) (sourceNameText bindingName)
                  SData _ _ _ constructors ->
                    Map.union
                      (Map.fromList [(name, ConstructorNamespace) | DataConstructor _ constructor _ <- constructors, Just name <- [sourceNameText constructor]])
                      visibleBoundValues
                  _ -> visibleBoundValues
           in (nextVisibleBoundValues, resolvedStatement : resolvedRev)

        selfBoundValue visibleBindings name
          | Map.lookup name visibleBindings == Just ConstructorNamespace = Map.empty
          | otherwise = Map.singleton name ValueNamespace

        recursivePeerBoundValues statementIndex =
          Set.fromList
            [ peerNameText
            | peerIndex <- Map.findWithDefault [] statementIndex recursiveGroupsByStatement,
              Just peerName <- [Map.lookup peerIndex bindingNamesByStatement],
              Just peerNameText <- [sourceNameText peerName]
            ]

    referenceNamespace boundValues name =
      case name of
        UserName (UnqualifiedSourceName identifier)
          | Just namespace <- Map.lookup nameText boundValues -> namespace
          | Set.member nameText localConstructors -> ConstructorNamespace
          | Set.member nameText localValues -> ValueNamespace
          | Map.member nameText visibleValueOrigins -> ValueNamespace
          | Map.member nameText visibleConstructorOrigins -> ConstructorNamespace
          | Set.member nameText ambientValues -> ValueNamespace
          | Set.member nameText ambientConstructors -> ConstructorNamespace
          where
            nameText = identifierText identifier
        _ -> ValueNamespace

    resolveBinder namespace name =
      case name of
        UserName (UnqualifiedSourceName identifier) ->
          UserName (ResolvedUserName CurrentModule namespace identifier)
        UserName (QualifiedSourceName qualifier member) ->
          resolveName Map.empty namespace (UserName (QualifiedSourceName qualifier member))
        UserName qualified@QualifiedMethodSourceName {} ->
          resolveName Map.empty namespace (UserName qualified)
        BuiltinName identifier -> BuiltinName identifier
        GeneratedName generatedKind -> GeneratedName generatedKind

    resolveCaseArm boundValues (CaseArm node patternValue guard body) =
      let armBoundValues = Map.union (Map.fromSet (const ValueNamespace) (corePatternBinders patternValue)) boundValues
       in CaseArm
            (resolveNode node)
            (resolvePattern patternValue)
            (fmap (resolveExpr armBoundValues) guard)
            (resolveExpr armBoundValues body)

    resolvePattern patternValue =
      case patternValue of
        PWildcard node -> PWildcard (resolveNode node)
        PVariable node name -> PVariable (resolveNode node) (resolveBinder ValueNamespace name)
        PLiteral node literal -> PLiteral (resolveNode node) literal
        PConstructor node name patterns ->
          PConstructor (resolveNode node) (resolveName Map.empty ConstructorNamespace name) (map resolvePattern patterns)
        PList node patterns -> PList (resolveNode node) (map resolvePattern patterns)
        PConsList node headPattern tailPattern ->
          PConsList (resolveNode node) (resolvePattern headPattern) (resolvePattern tailPattern)
        PTuple node patterns -> PTuple (resolveNode node) (map resolvePattern patterns)
        PAs node name pattern' ->
          PAs (resolveNode node) (resolveBinder ValueNamespace name) (resolvePattern pattern')
        POr node patterns -> POr (resolveNode node) (map resolvePattern patterns)

    resolveStatement boundValues statement =
      case statement of
        SLet node name value ->
          SLet
            (resolveNode node)
            (resolveBinder ValueNamespace name)
            (resolveBindingValue boundValues name value)
        SSignature node name payload ->
          SSignature (resolveNode node) (resolveBinder ValueNamespace name) (resolveSignaturePayload payload)
        SData node name parameters constructors ->
          SData
            (resolveNode node)
            (resolveBinder TypeNamespace name)
            (map (resolveBinder TypeNamespace) parameters)
            (map resolveDataConstructor constructors)
        SClass node name parameters methods ->
          SClass
            (resolveNode node)
            (resolveBinder CapabilityNamespace name)
            (map (resolveBinder TypeNamespace) parameters)
            (map resolveClassMethod methods)
        SImpl node name arguments methods ->
          SImpl
            (resolveNode node)
            (resolveName Map.empty CapabilityNamespace name)
            (map resolveSignatureType arguments)
            (map (resolveImplMethod boundValues) methods)
        SModule node path -> SModule (resolveNode node) path
        SImport node path alias symbols -> SImport (resolveNode node) path alias symbols
        SExpr node value -> SExpr (resolveNode node) (resolveExpr boundValues value)

    resolveBindingValue boundValues bindingName value =
      case (bindingName, value) of
        ( UserName (UnqualifiedSourceName bindingIdentifier),
          EVar referenceNode (UserName (UnqualifiedSourceName referenceIdentifier))
          )
            | bindingIdentifier == referenceIdentifier,
              Just _ <- lookupKernelBuiltinSymbol (identifierText referenceIdentifier) ->
                EVar (resolveNode referenceNode) (BuiltinName referenceIdentifier)
        _ -> resolveExpr boundValues value

    resolveDataConstructor (DataConstructor node name fieldTypes) =
      DataConstructor
        (resolveNode node)
        (resolveBinder ConstructorNamespace name)
        (map resolveSignatureType fieldTypes)

    resolveClassMethod (ClassMethodSignature node name payload) =
      ClassMethodSignature (resolveNode node) (resolveBinder ValueNamespace name) (resolveSignaturePayload payload)

    resolveImplMethod boundValues (ImplMethod node name body) =
      ImplMethod (resolveNode node) (resolveBinder ValueNamespace name) (resolveExpr boundValues body)

    resolveSignaturePayload payload =
      case payload of
        SignatureType signatureType -> SignatureType (resolveSignatureType signatureType)
        ConstrainedSignature constraints signatureType ->
          ConstrainedSignature
            (map resolveSignatureConstraint constraints)
            (resolveSignatureType signatureType)
        UnsupportedSignature tokens -> UnsupportedSignature (map resolveSignatureToken tokens)

    resolveSignatureToken = fmap (resolveName Map.empty TypeNamespace)

    resolveSignatureConstraint (SignatureConstraint name arguments) =
      SignatureConstraint (resolveName Map.empty CapabilityNamespace name) (map resolveSignatureType arguments)

    resolveSignatureType =
      bimap
        (resolveName Map.empty TypeNamespace)
        (resolveBinder TypeNamespace)

    sourceNameText name =
      case name of
        UserName (UnqualifiedSourceName identifier) -> Just (identifierText identifier)
        _ -> Nothing

    corePatternBinders patternValue =
      case patternValue of
        PWildcard _ -> Set.empty
        PVariable _ name -> maybe Set.empty Set.singleton (sourceNameText name)
        PLiteral _ _ -> Set.empty
        PConstructor _ _ patterns -> Set.unions (map corePatternBinders patterns)
        PList _ patterns -> Set.unions (map corePatternBinders patterns)
        PConsList _ headPattern tailPattern ->
          Set.union (corePatternBinders headPattern) (corePatternBinders tailPattern)
        PTuple _ patterns -> Set.unions (map corePatternBinders patterns)
        PAs _ name nestedPattern ->
          maybe id Set.insert (sourceNameText name) (corePatternBinders nestedPattern)
        POr _ alternatives ->
          case alternatives of
            [] -> Set.empty
            firstAlternative : rest ->
              foldl' Set.intersection (corePatternBinders firstAlternative) (map corePatternBinders rest)

-- | Resolve a lowered, import-free source unit. The local inventory is derived
-- from its declarations so constructors, types, and capabilities receive the
-- same namespaces as module-graph compilation.
resolveStandaloneExprNames ::
  ModuleExportInventory ->
  Expr 'Lowered ->
  Either (NonEmpty Diagnostic) (Expr 'Resolved)
resolveStandaloneExprNames ambientExports expression =
  resolveExprNames
    ResolutionContext
      { resolutionAmbientExports = ambientExports,
        resolutionLocalInventory = standaloneLocalInventory expression,
        resolutionInventoriesByModule = Map.empty,
        resolutionImports = []
      }
    expression

standaloneLocalInventory :: Expr 'Lowered -> ModuleExportInventory
standaloneLocalInventory expression =
  exportInventory
    ( case expression of
        EBlock _ statements -> concatMap statementExports statements
        _ -> []
    )
  where
    statementExports statement =
      case statement of
        SLet _ name _ -> maybeExport ValueNamespace name
        SData _ typeName _ constructors ->
          maybeExport TypeNamespace typeName
            <> concatMap constructorExports constructors
        SClass _ className _ methods ->
          maybeExport CapabilityNamespace className
            <> concatMap methodExports methods
        _ -> []

    constructorExports (DataConstructor _ name _) =
      maybeExport ConstructorNamespace name

    methodExports (ClassMethodSignature _ name _) =
      maybeExport ValueNamespace name

    maybeExport namespace name =
      case name of
        UserName (UnqualifiedSourceName identifier) ->
          [ModuleExport namespace (identifierText identifier)]
        _ -> []
