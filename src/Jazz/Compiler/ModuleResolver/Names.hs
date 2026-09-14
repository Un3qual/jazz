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
    standaloneLocalInventory,
    resolvedPublicReferences,
  )
where

import Data.Bifunctor
  ( bimap,
  )
import Data.List (mapAccumL)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
  ( fromMaybe,
  )
import qualified Data.Set as Set
import qualified Data.Text as Text
import Jazz.Compiler.AST
  ( CaseArm (..),
    ClassMethodSignature (..),
    CoreNode (..),
    CoreNodeId (..),
    CorePhase (..),
    CoreSort (ExpressionSort),
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
import Jazz.Compiler.CoreIdentity
  ( CapabilityId (..),
    CoreBinderId (..),
    ResolvedNodeFacts (..),
    ResolvedReference (..),
    emptyResolvedNodeFacts,
  )
import Jazz.Compiler.ModuleExports
  ( ModuleExport (..),
    ModuleExportInventory,
    exportInventory,
    exportNamesInNamespace,
    firstExportNamespace,
    inventoryHasExport,
    selectExportNames,
    withClassMethods,
  )
import Jazz.Compiler.ModuleIdentity (SourceUnitOwner (..), mkModulePath, standaloneModulePath)
import Jazz.Compiler.ModuleResolver.Imports
  ( BindingOrigin (..),
    ValidatedImportScope,
    emptyImportScope,
    importScopeAliases,
    importScopeInventories,
    importedNameOrigins,
  )
import Jazz.Compiler.Name
  ( GeneratedNameKind (..),
    Name (..),
    NameNamespace (..),
    ResolvedName,
    ResolvedNameOrigin (..),
    ResolvedUserName (..),
    SourceName (..),
    identifierText,
    mkIdentifier,
    operatorBindingName,
    qualifiedMemberName,
    resolveDeclarationOwner,
    resolvedAmbientName,
    resolvedImportedName,
    sourceName,
  )
import Jazz.Compiler.Parser.Operator (builtinOperatorFunction)
import Jazz.Compiler.RecursiveBindings (publishResolvedCaptures, resolveLexicalScopes)
import Jazz.Compiler.TypeRepresentation
  ( pattern ConstrainedSignature,
    pattern SignatureConstraint,
    pattern SignatureType,
    pattern UnsupportedSignature,
  )

data ResolutionContext = ResolutionContext
  { resolutionSourceOwner :: SourceUnitOwner,
    resolutionExternalReferences :: Map ResolvedName ResolvedReference,
    resolutionAmbientExports :: ModuleExportInventory,
    resolutionLocalInventory :: ModuleExportInventory,
    resolutionImportScope :: ValidatedImportScope
  }

resolveNode :: SourceUnitOwner -> CoreNode 'Lowered sort -> CoreNode 'Resolved sort
resolveNode owner (CoreNode nodeId spanValue ()) = CoreNode nodeId spanValue (emptyResolvedNodeFacts owner)

resolveBinderNode :: SourceUnitOwner -> CoreNode 'Lowered sort -> CoreNode 'Resolved sort
resolveBinderNode owner node =
  (resolveNode owner node) {coreNodeFacts = (emptyResolvedNodeFacts owner) {resolvedNodeBinder = Just (CoreBinderId (owner, coreNodeId node))}}

resolveExprNames ::
  ResolutionContext ->
  Expr 'Lowered ->
  Expr 'Resolved
resolveExprNames context rootExpression = publishResolvedCaptures (resolveLexicalScopes (resolutionExternalReferences context) externalNames (resolveExpr (resolutionSourceOwner context) Map.empty rootExpression))
  where
    ambientExports = resolutionAmbientExports context
    localInventory = resolutionLocalInventory context
    importScope = resolutionImportScope context
    inventoriesByModule = importScopeInventories importScope
    ambientValues = exportNamesInNamespace ValueNamespace ambientExports
    ambientConstructors = exportNamesInNamespace ConstructorNamespace ambientExports
    localValues = exportNamesInNamespace ValueNamespace localInventory
    localConstructors = exportNamesInNamespace ConstructorNamespace localInventory

    aliasPaths = Map.map bindingOriginModulePath (importScopeAliases importScope)
    visibleValueOrigins = importedNameOrigins ValueNamespace importScope
    visibleConstructorOrigins = importedNameOrigins ConstructorNamespace importScope
    visibleTypeOrigins = importedNameOrigins TypeNamespace importScope
    visibleClassOrigins = importedNameOrigins CapabilityNamespace importScope

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
                        (ImportedModule dependencyPath)
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
                  UserName (ResolvedUserName (ImportedModule dependencyPath) ValueNamespace member)
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
                (ImportedModule dependencyPath)
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
      inventoryHasExport (ModuleExport namespace nameText) localInventory

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

    ambientName namespace nameText =
      inventoryHasExport (ModuleExport namespace nameText) ambientExports

    classOrigin className
      | localName CapabilityNamespace className = CurrentModule
      | Just dependencyPath <- Map.lookup className visibleClassOrigins = ImportedModule dependencyPath
      | ambientName CapabilityNamespace className = AmbientPrelude
      | otherwise = CurrentModule

    resolveExpr owner boundValues expression =
      case expression of
        ELit node literal -> ELit (resolveNode owner node) literal
        EVar node name ->
          let targetName = resolveName boundValues (referenceNamespace boundValues name) name
           in EVar (resolveReferenceNode owner targetName node) targetName
        ELambda node parameter body ->
          let lambdaBoundValues = insertVisibleName ValueNamespace parameter boundValues
           in ELambda (resolveBinderNode owner node) (resolveBinder ValueNamespace parameter) (resolveExpr owner lambdaBoundValues body)
        EOperatorValue node symbol -> resolveOperator owner boundValues node symbol
        EList node items -> EList (resolveNode owner node) (map (resolveExpr owner boundValues) items)
        ETuple node items -> ETuple (resolveNode owner node) (map (resolveExpr owner boundValues) items)
        EApply node function argument ->
          EApply (resolveNode owner node) (resolveExpr owner boundValues function) (resolveExpr owner boundValues argument)
        ETypeApplication node function spanValue signatureType ->
          ETypeApplication (resolveNode owner node) (resolveExpr owner boundValues function) spanValue (resolveSignatureType owner signatureType)
        EIf node condition trueBranch falseBranch ->
          EIf
            (resolveNode owner node)
            (resolveExpr owner boundValues condition)
            (resolveExpr owner boundValues trueBranch)
            (resolveExpr owner boundValues falseBranch)
        EPatternCase node scrutinee arms ->
          EPatternCase (resolveNode owner node) (resolveExpr owner boundValues scrutinee) (map (resolveCaseArm owner boundValues) arms)
        EBinary node symbol left right ->
          EApply (resolveNode owner node) (EApply (generatedNode owner node 1) (operatorReference owner boundValues node 2 symbol) (recur left)) (recur right)
        ESectionLeft node left symbol ->
          let name = sectionName OperatorSectionLeft node
           in EApply
                (resolveNode owner node)
                (generatedLambda owner node 1 name (EApply (generatedNode owner node 2) (operatorReference owner boundValues node 3 symbol) (generatedReference owner node 4 name)))
                (recur left)
        ESectionRight node symbol right ->
          let rightName = sectionName OperatorSectionRight node
              leftName = sectionName OperatorSectionLeft node
              functionName = sectionName OperatorSectionFunction node
              call =
                EApply
                  (generatedNode owner node 5)
                  (EApply (generatedNode owner node 6) (generatedReference owner node 7 functionName) (generatedReference owner node 8 leftName))
                  (generatedReference owner node 9 rightName)
           in EApply
                (resolveNode owner node)
                ( generatedLambda
                    owner
                    node
                    1
                    rightName
                    ( EApply
                        (generatedNode owner node 2)
                        (generatedLambda owner node 3 functionName (generatedLambda owner node 4 leftName call))
                        (operatorReference owner boundValues node 10 symbol)
                    )
                )
                (recur right)
        EBlock node statements ->
          let resolvedStatements = resolveBlockStatements owner boundValues statements
           in EBlock (resolveNode owner node) resolvedStatements
      where
        recur = resolveExpr owner boundValues

    -- Lowering allocates nonnegative source IDs. Resolution reserves sixteen
    -- negative IDs per authored operator node; each template uses at most ten.
    -- Existing source nodes and their spans remain unchanged.
    generatedNode :: SourceUnitOwner -> CoreNode 'Lowered 'ExpressionSort -> Int -> CoreNode 'Resolved 'ExpressionSort
    generatedNode owner sourceNode slot =
      let CoreNodeId sourceId = coreNodeId sourceNode
       in CoreNode (CoreNodeId (negate (16 * sourceId + slot))) (coreNodeSpan sourceNode) (emptyResolvedNodeFacts owner)
    generatedLambda :: SourceUnitOwner -> CoreNode 'Lowered 'ExpressionSort -> Int -> ResolvedName -> Expr 'Resolved -> Expr 'Resolved
    generatedLambda owner sourceNode slot name body =
      let node = generatedNode owner sourceNode slot
          binder = CoreBinderId (owner, coreNodeId node)
       in ELambda (node {coreNodeFacts = (coreNodeFacts node) {resolvedNodeBinder = Just binder}}) name body
    generatedReference :: SourceUnitOwner -> CoreNode 'Lowered 'ExpressionSort -> Int -> ResolvedName -> Expr 'Resolved
    generatedReference owner sourceNode slot name =
      let node = generatedNode owner sourceNode slot
       in EVar (node {coreNodeFacts = (coreNodeFacts node) {resolvedNodeReference = Just (UnresolvedReference name)}}) name
    operatorReference owner boundValues sourceNode slot =
      resolveOperator owner boundValues (sourceNode {coreNodeId = coreNodeId (generatedNode owner sourceNode slot)})

    resolveOperator owner boundValues node symbol = case builtinOperatorFunction symbol of
      Just function -> resolveExpr owner boundValues (EVar node (sourceName (mkIdentifier function)))
      Nothing ->
        let name = operatorBindingName symbol
            resolved = resolveNode owner node
         in EVar (resolved {coreNodeFacts = (coreNodeFacts resolved) {resolvedNodeReference = Just (UnresolvedReference name), resolvedOperatorSpelling = Just symbol}}) name

    sectionName kind sourceNode = let CoreNodeId sourceId = coreNodeId sourceNode in GeneratedName (kind sourceId)

    externalNames =
      Set.unions
        [ Set.map (resolvedAmbientName ValueNamespace . mkIdentifier) ambientValues,
          Set.map (resolvedAmbientName ConstructorNamespace . mkIdentifier) ambientConstructors,
          importedNames ValueNamespace visibleValueOrigins,
          importedNames ConstructorNamespace visibleConstructorOrigins,
          Set.map (BuiltinName . mkIdentifier) kernelBuiltinNames,
          Set.map (resolvedAmbientName ValueNamespace . mkIdentifier) kernelBuiltinNames
        ]
    importedNames namespace origins = Set.fromList [resolvedImportedName path namespace (mkIdentifier name) | (name, path) <- Map.toList origins]

    resolveReferenceNode owner name node =
      (resolveNode owner node) {coreNodeFacts = (emptyResolvedNodeFacts owner) {resolvedNodeReference = Just (referenceTarget owner name)}}

    referenceTarget owner name =
      case name of
        BuiltinName identifier -> BuiltinReference identifier
        UserName (ResolvedUserName CurrentModule ValueNamespace identifier)
          | Just _ <- lookupKernelBuiltinSymbol (identifierText identifier) -> BuiltinReference identifier
        _ -> externalTarget
      where
        externalTarget =
          case Map.lookup name (resolutionExternalReferences context) of
            Just reference -> reference
            Nothing -> case name of
              UserName (ResolvedUserName origin ValueNamespace identifier)
                | [className, method] <- Text.splitOn "::" (identifierText identifier) ->
                    CapabilityMethodReference
                      (CapabilityId (resolveDeclarationReference owner (UserName (ResolvedUserName origin CapabilityNamespace (mkIdentifier className)))))
                      (mkIdentifier method)
              _ -> UnresolvedReference name

    -- Legacy expression callers may supply sequential module bodies in one
    -- lowered block. Resolve their imported declaration identities here too;
    -- checking must never recover a target by matching a rendered class name.
    inlineModuleStatements = case rootExpression of
      EBlock _ statements ->
        Map.fromListWith (flip (++)) (snd (mapAccumL ownedStatement (resolutionSourceOwner context) statements))
      _ -> Map.empty
      where
        ownedStatement activeOwner statement =
          let owner = case statement of
                SModule _ segments | Just path <- NonEmpty.nonEmpty (map mkIdentifier segments) -> NamedSourceUnit (mkModulePath path)
                _ -> activeOwner
           in (owner, (owner, [statement]))

    inlineDeclarationOrigins = Map.map importedDeclarations inlineModuleStatements
      where
        inventories = Map.map statementInventory inlineModuleStatements
        importedDeclarations statements =
          Map.fromList
            [ ((namespace, name), ImportedModule path)
            | let localDeclarations = statementInventory statements,
              SImport _ segments Nothing symbols <- statements,
              Just pathSegments <- [NonEmpty.nonEmpty (map mkIdentifier segments)],
              let path = mkModulePath pathSegments,
              Just inventory <- [Map.lookup (NamedSourceUnit path) inventories],
              namespace <- [TypeNamespace, CapabilityNamespace],
              name <- Set.toList (exportNamesInNamespace namespace (selectExportNames symbols inventory)),
              Set.notMember name (exportNamesInNamespace namespace localDeclarations)
            ]

    resolveDeclarationReference owner name = case name of
      UserName (ResolvedUserName CurrentModule namespace identifier)
        | Just origin <- Map.lookup owner inlineDeclarationOrigins >>= Map.lookup (namespace, identifierText identifier) ->
            UserName (ResolvedUserName origin namespace identifier)
      _ -> resolveDeclarationOwner owner name

    resolveBlockStatements owner initialBoundValues statements =
      snd (mapAccumL resolveBlockStatement (owner, initialBoundValues) statements)
      where
        -- Future local values establish their namespace here. The lexical pass
        -- later decides whether their declaration is visible as a recursive peer.
        firstBindings = foldr firstBinding Map.empty statements
        firstBinding (SLet _ name _) bindings
          | Just key <- sourceNameText name,
            Set.notMember key nonlocalNames =
              insertVisibleName ValueNamespace name bindings
        firstBinding _ bindings = bindings
        nonlocalNames = Set.unions [ambientValues, ambientConstructors, Map.keysSet visibleValueOrigins, Map.keysSet visibleConstructorOrigins, kernelBuiltinNames]

        resolveBlockStatement (activeOwner, visibleBoundValues) statement =
          ((statementOwner, publish statement visibleBoundValues), resolveStatement statementOwner definitionBindings statement)
          where
            statementOwner = case statement of
              SModule _ segments | Just path <- NonEmpty.nonEmpty (map mkIdentifier segments) -> NamedSourceUnit (mkModulePath path)
              _ -> activeOwner
            selfBindings = case statement of
              SLet _ name _
                | Just key <- sourceNameText name,
                  Map.notMember key visibleBoundValues ->
                    insertVisibleName ValueNamespace name visibleBoundValues
              _ -> visibleBoundValues
            definitionBindings = Map.union selfBindings firstBindings

        publish statement bindings = case statement of
          SLet _ name _ -> insertVisibleName ValueNamespace name bindings
          SClass _ _ _ methods _ _ -> foldl' (\acc (ClassMethodSignature _ name _) -> insertVisibleName ValueNamespace name acc) bindings methods
          SData _ _ _ constructors ->
            foldl' (\acc (DataConstructor _ name _) -> insertVisibleName ConstructorNamespace name acc) bindings constructors
          _ -> bindings

    insertVisibleName namespace name bindings =
      maybe bindings (\key -> Map.insert key namespace bindings) (sourceNameText name)

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
        _ -> resolveName Map.empty namespace name

    resolveCaseArm owner boundValues (CaseArm node patternValue guard body) =
      let armBoundValues = Map.union (patternBindings patternValue) boundValues
       in CaseArm
            (resolveNode owner node)
            (resolvePattern owner patternValue)
            (fmap (resolveExpr owner armBoundValues) guard)
            (resolveExpr owner armBoundValues body)

    resolvePattern owner patternValue =
      case patternValue of
        PWildcard node -> PWildcard (resolveNode owner node)
        PVariable node name -> PVariable (resolveBinderNode owner node) (resolveBinder ValueNamespace name)
        PLiteral node literal -> PLiteral (resolveNode owner node) literal
        PConstructor node name patterns ->
          let target = resolveName Map.empty ConstructorNamespace name
           in PConstructor (resolveReferenceNode owner target node) target (map (resolvePattern owner) patterns)
        PList node patterns -> PList (resolveNode owner node) (map (resolvePattern owner) patterns)
        PConsList node headPattern tailPattern ->
          PConsList (resolveNode owner node) (resolvePattern owner headPattern) (resolvePattern owner tailPattern)
        PTuple node patterns -> PTuple (resolveNode owner node) (map (resolvePattern owner) patterns)
        PAs node name pattern' ->
          PAs (resolveBinderNode owner node) (resolveBinder ValueNamespace name) (resolvePattern owner pattern')
        POr node patterns -> POr (resolveNode owner node) (map (resolvePattern owner) patterns)

    resolveStatement owner boundValues statement =
      case statement of
        SLet node name value ->
          SLet (resolveBinderNode owner node) (resolveBinder ValueNamespace name) (resolveBindingValue owner boundValues name value)
        SSignature node name payload ->
          SSignature (resolveBinderNode owner node) (resolveBinder ValueNamespace name) (resolveSignaturePayload owner payload)
        SData node name parameters constructors ->
          SData (resolveNode owner node) (resolveDeclarationOwner owner (resolveBinder TypeNamespace name)) (map (resolveBinder TypeNamespace) parameters) (map (resolveDataConstructor owner) constructors)
        SClass node name parameters methods prerequisites defaults ->
          let capability = resolveDeclarationOwner owner (resolveBinder CapabilityNamespace name)
           in SClass
                (resolveNode owner node)
                capability
                (map (resolveBinder TypeNamespace) parameters)
                (map (resolveClassMethod owner capability) methods)
                (map (resolveSignatureConstraint owner) prerequisites)
                (map (resolveImplMethod owner (foldl' (\acc (ClassMethodSignature _ method _) -> insertVisibleName ValueNamespace method acc) boundValues methods) capability) defaults)
        SImpl node name arguments methods prerequisites ->
          let capability = resolveDeclarationReference owner (resolveName Map.empty CapabilityNamespace name)
           in SImpl
                (resolveNode owner node)
                capability
                (map (resolveSignatureType owner) arguments)
                (map (resolveImplMethod owner boundValues capability) methods)
                (map (resolveSignatureConstraint owner) prerequisites)
        SModule node path -> SModule (resolveNode owner node) path
        SImport node path alias symbols ->
          let target = mkModulePath <$> NonEmpty.nonEmpty (map mkIdentifier path)
              resolved = resolveNode owner node
           in SImport (resolved {coreNodeFacts = (coreNodeFacts resolved) {resolvedNodeImportTarget = target}}) path alias symbols
        SExpr node value -> SExpr (resolveNode owner node) (resolveExpr owner boundValues value)

    resolveBindingValue owner boundValues bindingName value =
      case (bindingName, value) of
        ( UserName (UnqualifiedSourceName bindingIdentifier),
          EVar referenceNode (UserName (UnqualifiedSourceName referenceIdentifier))
          )
            | bindingIdentifier == referenceIdentifier,
              Just _ <- lookupKernelBuiltinSymbol (identifierText referenceIdentifier) ->
                EVar (resolveReferenceNode owner (BuiltinName referenceIdentifier) referenceNode) (BuiltinName referenceIdentifier)
        _ -> resolveExpr owner boundValues value

    resolveDataConstructor owner (DataConstructor node name fieldTypes) =
      DataConstructor (resolveBinderNode owner node) (resolveBinder ConstructorNamespace name) (map (resolveSignatureType owner) fieldTypes)

    resolveClassMethod owner capability (ClassMethodSignature node name payload) =
      ClassMethodSignature (resolveMethodNode owner capability name node) (resolveBinder ValueNamespace name) (resolveSignaturePayload owner payload)

    resolveImplMethod owner boundValues capability (ImplMethod node name body) =
      ImplMethod (resolveMethodNode owner capability name node) (resolveBinder ValueNamespace name) (resolveExpr owner boundValues body)

    resolveMethodNode owner capability method node =
      let resolved = resolveBinderNode owner node
          target = referenceTarget owner (qualifiedMemberName capability (resolveBinder ValueNamespace method))
       in resolved {coreNodeFacts = (coreNodeFacts resolved) {resolvedNodeReference = Just target}}

    resolveSignaturePayload owner payload =
      case payload of
        SignatureType signatureType -> SignatureType (resolveSignatureType owner signatureType)
        ConstrainedSignature constraints signatureType ->
          ConstrainedSignature
            (map (resolveSignatureConstraint owner) constraints)
            (resolveSignatureType owner signatureType)
        UnsupportedSignature tokens -> UnsupportedSignature (map (resolveSignatureToken owner) tokens)

    resolveSignatureToken owner = fmap (resolveDeclarationReference owner . resolveName Map.empty TypeNamespace)

    resolveSignatureConstraint owner (SignatureConstraint name arguments) =
      SignatureConstraint (resolveDeclarationReference owner (resolveName Map.empty CapabilityNamespace name)) (map (resolveSignatureType owner) arguments)

    resolveSignatureType owner =
      bimap
        (resolveDeclarationReference owner . resolveName Map.empty TypeNamespace)
        (resolveBinder TypeNamespace)

    sourceNameText name =
      case name of
        UserName (UnqualifiedSourceName identifier) -> Just (identifierText identifier)
        GeneratedName _ -> Just (identifierText name)
        _ -> Nothing

    patternBindings patternValue =
      case patternValue of
        PVariable _ name -> insertVisibleName ValueNamespace name Map.empty
        PConstructor _ _ patterns -> Map.unions (map (patternBindings) patterns)
        PList _ patterns -> Map.unions (map (patternBindings) patterns)
        PConsList _ headPattern tailPattern -> Map.union (patternBindings headPattern) (patternBindings tailPattern)
        PTuple _ patterns -> Map.unions (map (patternBindings) patterns)
        PAs _ name nestedPattern -> insertVisibleName ValueNamespace name (patternBindings nestedPattern)
        POr _ alternatives -> case alternatives of
          [] -> Map.empty
          firstAlternative : rest -> foldl' Map.intersection (patternBindings firstAlternative) (map (patternBindings) rest)
        _ -> Map.empty

-- | Resolve a lowered, import-free source unit. The local inventory is derived
-- from its declarations so constructors, types, and capabilities receive the
-- same namespaces as module-graph compilation.
resolveStandaloneExprNames ::
  ModuleExportInventory ->
  Expr 'Lowered ->
  Expr 'Resolved
resolveStandaloneExprNames ambientExports expression =
  resolveExprNames
    ResolutionContext
      { resolutionSourceOwner = StandaloneSourceUnit standaloneModulePath,
        resolutionExternalReferences = Map.empty,
        resolutionAmbientExports = ambientExports,
        resolutionLocalInventory = standaloneLocalInventory expression,
        resolutionImportScope = emptyImportScope
      }
    expression

standaloneLocalInventory :: Expr 'Lowered -> ModuleExportInventory
standaloneLocalInventory expression =
  statementInventory (case expression of EBlock _ statements -> statements; _ -> [])

statementInventory :: [Statement 'Lowered] -> ModuleExportInventory
statementInventory statements =
  withClassMethods
    (Map.fromList [(identifierText name, Set.fromList [identifierText method | ClassMethodSignature _ method _ <- methods]) | SClass _ name _ methods _ _ <- statements])
    (exportInventory (concatMap statementExports statements))
  where
    statementExports statement =
      case statement of
        SLet _ name _ -> maybeExport ValueNamespace name
        SData _ typeName _ constructors ->
          maybeExport TypeNamespace typeName
            <> concatMap constructorExports constructors
        SClass _ className _ methods _ _ ->
          maybeExport CapabilityNamespace className <> concat [maybeExport ValueNamespace name | ClassMethodSignature _ name _ <- methods]
        _ -> []

    constructorExports (DataConstructor _ name _) =
      maybeExport ConstructorNamespace name

    maybeExport namespace name =
      case name of
        UserName (UnqualifiedSourceName identifier) ->
          [ModuleExport namespace (identifierText identifier)]
        _ -> []

-- | The public identity projection of a resolved unit. Import aliases select
-- these targets without changing the declaration's defining source identity.
resolvedPublicReferences :: ResolvedNameOrigin -> ModuleExportInventory -> [Statement 'Resolved] -> Map ResolvedName ResolvedReference
resolvedPublicReferences origin inventory = Map.fromList . concatMap statementReferences
  where
    key namespace name = UserName (ResolvedUserName origin namespace (mkIdentifier (identifierText name)))
    binding namespace name node =
      [(key namespace name, LexicalReference binder) | Set.member (identifierText name) (exportNamesInNamespace namespace inventory), Just binder <- [resolvedNodeBinder (coreNodeFacts node)]]
    statementReferences :: Statement 'Resolved -> [(ResolvedName, ResolvedReference)]
    statementReferences statement = case statement of
      SLet node name _ -> binding ValueNamespace name node
      SData _ _ _ constructors -> concat [binding ConstructorNamespace name node | DataConstructor node name _ <- constructors]
      SClass _ name _ methods _ _ ->
        concat
          [ [(key ValueNamespace method, target) | Set.member (identifierText method) (exportNamesInNamespace ValueNamespace inventory)]
              <> [(UserName (ResolvedUserName origin ValueNamespace (mkIdentifier (identifierText name <> "::" <> identifierText method))), target) | Set.member (identifierText name) (exportNamesInNamespace CapabilityNamespace inventory)]
          | ClassMethodSignature _ method _ <- methods,
            let target = CapabilityMethodReference (CapabilityId name) (mkIdentifier (identifierText method))
          ]
      _ -> []
