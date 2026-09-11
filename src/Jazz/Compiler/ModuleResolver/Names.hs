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
    resolveSourceUnitExprNames,
    resolvedPublicReferences,
  )
where

import Data.Bifunctor
  ( bimap,
  )
import Data.List (mapAccumL)
import Data.List.NonEmpty
  ( NonEmpty,
  )
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
  ( fromMaybe,
  )
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as Text
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
    expressionNode,
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
import Jazz.Compiler.Diagnostics
  ( Diagnostic,
  )
import Jazz.Compiler.ModuleExports
  ( ModuleExport (..),
    ModuleExportInventory,
    exportInventory,
    exportNamesInNamespace,
    firstExportNamespace,
  )
import Jazz.Compiler.ModuleIdentity (ModulePath, SourceUnitOwner (..), preludeModulePath, standaloneModulePath)
import Jazz.Compiler.ModuleResolver.Imports
  ( BindingOrigin (..),
    ValidatedImportScope,
    emptyImportScope,
    importScopeAliases,
    importScopeInventories,
    importedNameOrigins,
  )
import Jazz.Compiler.Name
  ( Name (..),
    NameNamespace (..),
    ResolvedName,
    ResolvedNameOrigin (..),
    ResolvedUserName (..),
    SourceName (..),
    identifierText,
    mkIdentifier,
    operatorBindingName,
    resolvedAmbientName,
    resolvedImportedName,
  )
import Jazz.Compiler.Parser.Operator (isBuiltinOperatorSymbol)
import Jazz.Compiler.RecursiveBindings (publishResolvedCaptures, resolveLexicalScopes)
import Jazz.Compiler.SourceUnitOwnership (sourceUnitOwnerOrigin, sourceUnitStatementOwners)
import Jazz.Compiler.TypeRepresentation
  ( pattern ConstrainedSignature,
    pattern SignatureConstraint,
    pattern SignatureType,
    pattern UnsupportedSignature,
  )

data ResolutionContext = ResolutionContext
  { resolutionSourceOwner :: SourceUnitOwner,
    resolutionStatementOwners :: Map Int SourceUnitOwner,
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
  Either (NonEmpty Diagnostic) (Expr 'Resolved)
resolveExprNames context rootExpression = Right (publishResolvedCaptures (resolveLexicalScopes externalNames (resolveExpr (resolutionSourceOwner context) Map.empty rootExpression)))
  where
    ambientExports = resolutionAmbientExports context
    localInventory = resolutionLocalInventory context
    importScope = resolutionImportScope context
    inventoriesByModule = importScopeInventories importScope
    ambientValues = exportNamesInNamespace ValueNamespace ambientExports
    ambientConstructors = exportNamesInNamespace ConstructorNamespace ambientExports
    ambientTypes = exportNamesInNamespace TypeNamespace ambientExports
    ambientClasses = exportNamesInNamespace CapabilityNamespace ambientExports
    localValues = exportNamesInNamespace ValueNamespace localInventory
    localDataTypes = exportNamesInNamespace TypeNamespace localInventory
    localConstructors = exportNamesInNamespace ConstructorNamespace localInventory
    localClasses = exportNamesInNamespace CapabilityNamespace localInventory

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

    ambientName namespace nameText =
      case namespace of
        ValueNamespace -> Set.member nameText ambientValues
        ConstructorNamespace -> Set.member nameText ambientConstructors
        TypeNamespace -> Set.member nameText ambientTypes
        CapabilityNamespace -> Set.member nameText ambientClasses

    classOrigin className
      | Set.member className localClasses = CurrentModule
      | Just dependencyPath <- Map.lookup className visibleClassOrigins = ImportedModule dependencyPath
      | Set.member className ambientClasses = AmbientPrelude
      | otherwise = CurrentModule

    resolveExpr owner boundValues expression =
      case expression of
        ELit node literal -> ELit (resolveNode owner node) literal
        EVar node name ->
          let targetName = resolveName boundValues (referenceNamespace boundValues name) name
           in EVar (resolveReferenceNode owner targetName node) targetName
        ELambda node parameter body ->
          let lambdaBoundValues = maybe boundValues (\name -> Map.insert name ValueNamespace boundValues) (sourceNameText parameter)
           in ELambda (resolveBinderNode owner node) (resolveBinder ValueNamespace parameter) (resolveExpr owner lambdaBoundValues body)
        EOperatorValue node symbol -> EOperatorValue (resolveOperatorNode owner boundValues symbol node) symbol
        EList node items -> EList (resolveNode owner node) (map (resolveExpr owner boundValues) items)
        ETuple node items -> ETuple (resolveNode owner node) (map (resolveExpr owner boundValues) items)
        EApply node function argument ->
          EApply (resolveNode owner node) (resolveExpr owner boundValues function) (resolveExpr owner boundValues argument)
        ETypeApplication node function spanValue signatureType ->
          ETypeApplication (resolveNode owner node) (resolveExpr owner boundValues function) spanValue (resolveSignatureType signatureType)
        EIf node condition trueBranch falseBranch ->
          EIf
            (resolveNode owner node)
            (resolveExpr owner boundValues condition)
            (resolveExpr owner boundValues trueBranch)
            (resolveExpr owner boundValues falseBranch)
        EPatternCase node scrutinee arms ->
          EPatternCase (resolveNode owner node) (resolveExpr owner boundValues scrutinee) (map (resolveCaseArm owner boundValues) arms)
        EBinary node symbol left right ->
          EBinary (resolveOperatorNode owner boundValues symbol node) symbol (resolveExpr owner boundValues left) (resolveExpr owner boundValues right)
        ESectionLeft node left symbol -> ESectionLeft (resolveOperatorNode owner boundValues symbol node) (resolveExpr owner boundValues left) symbol
        ESectionRight node symbol right -> ESectionRight (resolveOperatorNode owner boundValues symbol node) symbol (resolveExpr owner boundValues right)
        EBlock node statements ->
          let resolvedStatements = resolveBlockStatements owner (if coreNodeId node == coreNodeId (expressionNode rootExpression) then resolutionStatementOwners context else Map.empty) boundValues statements
           in EBlock (resolveNode owner node) resolvedStatements

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

    resolveOperatorNode owner boundValues symbol node =
      (resolveNode owner node) {coreNodeFacts = (emptyResolvedNodeFacts owner) {resolvedNodeReference = Just target}}
      where
        name :: ResolvedName
        name = operatorBindingName symbol
        target = case Map.lookup (identifierText name) boundValues of
          Just _ -> UnresolvedReference name
          Nothing | isBuiltinOperatorSymbol symbol -> BuiltinOperatorReference symbol
          Nothing -> UnresolvedReference name

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
                      (CapabilityId (UserName (ResolvedUserName (if origin == CurrentModule then sourceUnitOwnerOrigin owner else origin) CapabilityNamespace (mkIdentifier className))))
                      (mkIdentifier method)
              _ -> UnresolvedReference name

    resolveBlockStatements owner statementOwners initialBoundValues statements =
      snd (mapAccumL resolveBlockStatement initialBoundValues indexedStatements)
      where
        indexedStatements = zip [0 ..] statements
        ownerAt index = Map.findWithDefault owner index statementOwners
        -- Future local values establish their namespace here. The lexical pass
        -- later decides whether their declaration is visible as a recursive peer.
        firstBindings = foldr firstBinding Map.empty indexedStatements
        firstBinding (_, SLet _ name _) bindings
          | Just key <- sourceNameText name,
            Set.notMember key nonlocalNames =
              insertVisibleName ValueNamespace name bindings
        firstBinding _ bindings = bindings
        nonlocalNames = Set.unions [ambientValues, ambientConstructors, Map.keysSet visibleValueOrigins, Map.keysSet visibleConstructorOrigins, kernelBuiltinNames]

        resolveBlockStatement visibleBoundValues (statementIndex, statement) =
          (publish statement visibleBoundValues, resolveStatement statementOwner definitionBindings statement)
          where
            statementOwner = ownerAt statementIndex
            selfBindings = case statement of
              SLet _ name _
                | Just key <- sourceNameText name,
                  Map.notMember key visibleBoundValues ->
                    insertVisibleName ValueNamespace name visibleBoundValues
              _ -> visibleBoundValues
            definitionBindings = Map.union selfBindings firstBindings

        publish statement bindings = case statement of
          SLet _ name _ -> insertVisibleName ValueNamespace name bindings
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
        UserName (QualifiedSourceName qualifier member) ->
          resolveName Map.empty namespace (UserName (QualifiedSourceName qualifier member))
        UserName qualified@QualifiedMethodSourceName {} ->
          resolveName Map.empty namespace (UserName qualified)
        BuiltinName identifier -> BuiltinName identifier
        GeneratedName generatedKind -> GeneratedName generatedKind

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
          PConstructor (resolveNode owner node) (resolveName Map.empty ConstructorNamespace name) (map (resolvePattern owner) patterns)
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
          SSignature (resolveBinderNode owner node) (resolveBinder ValueNamespace name) (resolveSignaturePayload payload)
        SData node name parameters constructors ->
          SData (resolveNode owner node) (resolveBinder TypeNamespace name) (map (resolveBinder TypeNamespace) parameters) (map (resolveDataConstructor owner) constructors)
        SClass node name parameters methods ->
          SClass (resolveNode owner node) (resolveBinder CapabilityNamespace name) (map (resolveBinder TypeNamespace) parameters) (map (resolveClassMethod owner) methods)
        SImpl node name arguments methods ->
          let methodBindings = foldl' (\acc (ImplMethod _ methodName _) -> insertVisibleName ValueNamespace methodName acc) boundValues methods
           in SImpl (resolveNode owner node) (resolveName Map.empty CapabilityNamespace name) (map resolveSignatureType arguments) (map (resolveImplMethod owner methodBindings) methods)
        SModule node path -> SModule (resolveNode owner node) path
        SImport node path alias symbols -> SImport (resolveNode owner node) path alias symbols
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
      DataConstructor (resolveBinderNode owner node) (resolveBinder ConstructorNamespace name) (map resolveSignatureType fieldTypes)

    resolveClassMethod owner (ClassMethodSignature node name payload) =
      ClassMethodSignature (resolveBinderNode owner node) (resolveBinder ValueNamespace name) (resolveSignaturePayload payload)

    resolveImplMethod owner boundValues (ImplMethod node name body) =
      ImplMethod (resolveBinderNode owner node) (resolveBinder ValueNamespace name) (resolveExpr owner boundValues body)

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
  Either (NonEmpty Diagnostic) (Expr 'Resolved)
resolveStandaloneExprNames = resolveSourceUnitExprNames preludeModulePath Set.empty

resolveSourceUnitExprNames :: ModulePath -> Set Int -> ModuleExportInventory -> Expr 'Lowered -> Either (NonEmpty Diagnostic) (Expr 'Resolved)
resolveSourceUnitExprNames preludePath preludeIndices ambientExports expression =
  resolveExprNames
    ResolutionContext
      { resolutionSourceOwner = StandaloneSourceUnit standaloneModulePath,
        resolutionStatementOwners = Map.fromList (zip [0 ..] (sourceUnitStatementOwners standaloneModulePath preludePath preludeIndices statements)),
        resolutionExternalReferences = Map.empty,
        resolutionAmbientExports = ambientExports,
        resolutionLocalInventory = standaloneLocalInventory expression,
        resolutionImportScope = emptyImportScope
      }
    expression
  where
    statements = case expression of
      EBlock _ values -> values
      _ -> []

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
      SClass _ name _ methods
        | Set.member (identifierText name) (exportNamesInNamespace CapabilityNamespace inventory) ->
            [(UserName (ResolvedUserName origin ValueNamespace (mkIdentifier (identifierText name <> "::" <> identifierText method))), CapabilityMethodReference (CapabilityId (key CapabilityNamespace name)) (mkIdentifier (identifierText method))) | ClassMethodSignature _ method _ <- methods]
      _ -> []
