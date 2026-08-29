{-# LANGUAGE OverloadedStrings #-}

-- | Typed Core program, module, import, export, and interface validation.
module Jazz.Compiler.TypedCore.Validate.Program
  ( validateTypedProgramInternal,
  )
where

import Data.Graph (SCC (..), stronglyConnComp)
import Data.List (nub)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Jazz.Compiler.TypedCore
import Jazz.Compiler.TypedCore.Validate.Declarations
import Jazz.Compiler.TypedCore.Validate.Evidence
import Jazz.Compiler.TypedCore.Validate.Expressions
import Jazz.Compiler.TypedCore.Validate.Internal
import Jazz.Compiler.TypedCore.Validate.TypeRecipes

validateTypedProgramInternal :: TypedProgram -> [TypedCoreValidationFailure]
validateTypedProgramInternal (TypedProgram prelude modules entryModule) =
  duplicateModuleFailures allModules
    <> importCycleFailures moduleTable allModules
    <> moduleOrderFailures moduleTable allModules
    <> preludePathFailures
    <> regularPreludePathFailures
    <> unknownEntryFailure
    <> maybe [] (validateModule moduleTable prelude True) prelude
    <> concatMap (validateModule moduleTable prelude False) modules
  where
    allModules = maybeToList prelude <> modules
    allModulePaths = Set.fromList (map typedModulePath allModules)
    moduleTable = Map.fromList [(typedModulePath moduleValue, moduleValue) | moduleValue <- allModules]
    preludePathFailures =
      case typedModulePath <$> prelude of
        Just preludePath
          | preludePath /= ["Prelude"] ->
              [ failure
                  TypedPreludePath
                  TypedModuleInterfaceMismatch
                  (TypedTextDetail (renderModulePath preludePath))
              ]
        _ -> []
    regularPreludePathFailures =
      [ failure
          (TypedModulePath ["Prelude"])
          TypedModuleInterfaceMismatch
          (TypedTextDetail "Prelude")
      | any ((== ["Prelude"]) . typedModulePath) modules
      ]
    unknownEntryFailure
      | Set.member entryModule allModulePaths = []
      | otherwise =
          [ failure
              TypedProgramPath
              TypedUnknownEntryModule
              (TypedTextDetail (renderModulePath entryModule))
          ]

importCycleFailures :: Map [Text] TypedModule -> [TypedModule] -> [TypedCoreValidationFailure]
importCycleFailures moduleTable modules =
  [ failure
      (TypedModulePath modulePath)
      TypedModuleInterfaceMismatch
      (TypedTextDetail (renderModulePath importPath))
  | TypedModule modulePath _ imports _ _ _ _ _ <- modules,
    importPath <- nub [path | TypedResolvedImport _ path _ _ <- imports],
    pathsShareCyclicComponent modulePath importPath
  ]
  where
    cyclicComponentByPath =
      Map.fromList
        [ (modulePath, componentIndex)
        | (componentIndex, CyclicSCC componentPaths) <-
            zip [0 :: Int ..] (stronglyConnComp graphNodes),
          modulePath <- componentPaths
        ]
    graphNodes =
      [ (modulePath, modulePath, knownImports imports)
      | TypedModule modulePath _ imports _ _ _ _ _ <- Map.elems moduleTable
      ]
    knownImports imports =
      nub
        [ importPath
        | TypedResolvedImport _ importPath _ _ <- imports,
          Map.member importPath moduleTable
        ]
    pathsShareCyclicComponent leftPath rightPath =
      case (Map.lookup leftPath cyclicComponentByPath, Map.lookup rightPath cyclicComponentByPath) of
        (Just leftComponent, Just rightComponent) -> leftComponent == rightComponent
        _ -> False

modulePathReachable :: Map [Text] TypedModule -> [Text] -> [Text] -> Bool
modulePathReachable moduleTable currentPath targetPath =
  go Set.empty [currentPath]
  where
    go _ [] = False
    go seen (candidatePath : pendingPaths)
      | candidatePath == targetPath = True
      | Set.member candidatePath seen = go seen pendingPaths
      | otherwise =
          let nextSeen = Set.insert candidatePath seen
              nextPaths =
                case Map.lookup candidatePath moduleTable of
                  Nothing -> []
                  Just (TypedModule _ _ imports _ _ _ _ _) ->
                    [nextPath | TypedResolvedImport _ nextPath _ _ <- imports]
           in go nextSeen (nextPaths <> pendingPaths)

moduleOrderFailures :: Map [Text] TypedModule -> [TypedModule] -> [TypedCoreValidationFailure]
moduleOrderFailures moduleTable = go Set.empty
  where
    go _ [] = []
    go precedingPaths (TypedModule modulePath _ imports _ _ _ _ _ : remainingModules) =
      [ failure
          (TypedModulePath modulePath)
          TypedModuleInterfaceMismatch
          (TypedTextDetail (renderModulePath importPath))
      | importPath <- nub [path | TypedResolvedImport _ path _ _ <- imports],
        Map.member importPath moduleTable,
        Set.notMember importPath precedingPaths,
        not (modulePathReachable moduleTable importPath modulePath)
      ]
        <> go (Set.insert modulePath precedingPaths) remainingModules

validateModule :: Map [Text] TypedModule -> Maybe TypedModule -> Bool -> TypedModule -> [TypedCoreValidationFailure]
validateModule moduleTable prelude isPrelude moduleValue@(TypedModule modulePath sourcePath imports _ _ recursiveGroups statements moduleInfo) =
  validateModulePath modulePath
    <> validateSourcePath modulePath sourcePath
    <> validateResolvedImports moduleTable modulePath imports
    <> importBindingCollisionFailures moduleTable modulePath imports
    <> validateModuleInterface moduleTable moduleValue
    <> duplicateDeclarationFailures context (zip (map pure [0 ..]) statements)
    <> duplicateBinderFailures context (zip (map pure [0 ..]) statements)
    <> recursiveGroupFailures
    <> statementFailures
    <> moduleInfoFailures
    <> validateModuleResult (null statementFailures && null moduleInfoFailures) modulePath statements moduleInfo
  where
    moduleValidationPath
      | isPrelude = TypedPreludePath
      | otherwise = TypedModulePath modulePath
    importedPaths = Set.fromList [path | TypedResolvedImport _ path _ _ <- imports]
    visibleModules = Set.insert modulePath (Set.insert ["Prelude"] importedPaths)
    importedModules =
      [ (importPath, names, importedModule)
      | TypedResolvedImport _ importPath _ names <- imports,
        importedModule <- maybeToList (Map.lookup importPath moduleTable)
      ]
    preludeModules
      | isPrelude = []
      | otherwise = [(typedModulePath preludeModule, Nothing, preludeModule) | preludeModule <- maybeToList prelude]
    visibleExternalModules = preludeModules <> importedModules
    importedSchemeEntries = concatMap interfaceSchemeEntries visibleExternalModules
    schemes = Map.fromList importedSchemeEntries
    activeSchemes =
      Map.fromList
        [ (key, scheme)
        | (owner, scheme) <- importedSchemeEntries,
          key <- maybeToList (binderDefinitionKey owner)
        ]
    visibleNames =
      Set.fromList (concatMap interfaceNameKeys visibleExternalModules)
    sourceVisibleCapabilities =
      Set.fromList
        [ key
        | key@(ResolvedNameKey _ TypedCapabilityNamespace _) <-
            concatMap interfaceNameKeys sourceVisibleExternalModules
        ]
    sourceVisibleExternalModules =
      preludeModules
        <> [ (importPath, names, importedModule)
           | TypedResolvedImport _ importPath Nothing names <- imports,
             importedModule <- maybeToList (Map.lookup importPath moduleTable)
           ]
    visibleImpls =
      Set.fromList (concatMap interfaceVisibleImplIds visibleExternalModules)
    implMethods =
      Map.fromListWith Set.union (concatMap interfaceImplMethodEntries visibleExternalModules)
    dataArities =
      Map.fromList
        [ (key, length parameters)
        | (key, (_, TypedDataDeclaration _ _ parameters _)) <-
            Map.toList externalDataMetadata
        ]
    dataContracts =
      Map.fromList
        [ ( key,
            DataContract
              parameters
              [ map (qualifyExternalType declarationModulePath) fields
              | TypedConstructorDeclaration _ _ fields _ <- constructors
              ]
          )
        | (key, (declarationModulePath, TypedDataDeclaration _ _ parameters constructors)) <-
            Map.toList externalDataMetadata
        ]
    constructorContracts =
      Map.fromList (concatMap interfaceConstructorEntries visibleExternalModules)
    capabilityContracts =
      Map.fromList (concatMap interfaceCapabilityEntries visibleExternalModules)
    evidenceCapabilities =
      Map.fromList (concatMap interfaceEvidenceCapabilityEntries visibleExternalModules)
    externalDataMetadata =
      interfaceDataMetadataDeclarations moduleTable visibleExternalModules
    externalContext =
      ModuleContext
        { moduleContextPath = modulePath,
          moduleContextVisibleModules = visibleModules,
          moduleContextSchemes = schemes,
          moduleContextActiveSchemes = activeSchemes,
          moduleContextVisibleNames = visibleNames,
          moduleContextSourceVisibleCapabilities = sourceVisibleCapabilities,
          moduleContextVisibleImpls = visibleImpls,
          moduleContextImplMethods = implMethods,
          moduleContextDataArities = dataArities,
          moduleContextDataContracts = dataContracts,
          moduleContextConstructorContracts = constructorContracts,
          moduleContextCapabilityContracts = capabilityContracts,
          moduleContextEvidenceCapabilities = evidenceCapabilities,
          moduleContextLexicalContracts = Map.empty,
          moduleContextTypeScope = Set.empty,
          moduleContextPrimitiveConstraints = []
        }
    moduleMetadataStatements =
      [ statement
      | statement <- statements,
        case statement of
          TypedDataStatement {} -> True
          _ -> False
      ]
    baseContext = withBlockDeclarations moduleMetadataStatements externalContext
    context = withBlockDeclarations statements externalContext
    statementFailures =
      validateStatementsInOrder rootGroupsByStatement baseContext (zip (map pure [0 ..]) statements)
    recursiveGroupFailures =
      rootRecursiveGroupFailures modulePath statements recursiveGroups
    rootGroupsByStatement =
      rootRecursiveGroupsByStatement statements recursiveGroups
    moduleInfoFailures =
      validateModuleInfo context moduleValidationPath statements moduleInfo

validateModulePath :: [Text] -> [TypedCoreValidationFailure]
validateModulePath modulePath
  | not (null modulePath) && all validModulePathSegment modulePath = []
  | otherwise =
      [ failure
          (TypedModulePath modulePath)
          TypedModuleInterfaceMismatch
          (TypedTextDetail (renderModulePath modulePath))
      ]

validModulePathSegment :: Text -> Bool
validModulePathSegment segment =
  segment `notElem` moduleKeywords
    && validIdentifierSpelling segment
  where
    moduleKeywords =
      ["module", "import", "as", "data", "value", "if", "then", "else", "case"]

validateResolvedImports :: Map [Text] TypedModule -> [Text] -> [TypedResolvedImport] -> [TypedCoreValidationFailure]
validateResolvedImports moduleTable modulePath imports =
  concatMap validateImport imports
  where
    validateImport (TypedResolvedImport spanValue importPath alias selectedNames) =
      validateSpan path spanValue
        <> maybe [] validateAlias alias
        <> validateImportShape alias selectedNames
        <> validateSelectedNameShape selectedNames
        <> case Map.lookup importPath moduleTable of
          Nothing ->
            [ failure
                path
                TypedModuleInterfaceMismatch
                (TypedTextDetail (renderModulePath importPath))
            ]
          Just importedModule ->
            [ failure
                path
                TypedModuleInterfaceMismatch
                (TypedTextDetail selectedName)
            | selectedName <- maybe [] id selectedNames,
              not (moduleExportsImportSelectorName selectedName importedModule)
            ]
      where
        path = TypedModulePath modulePath
        validateAlias aliasName
          | validSourceIdentifier aliasName = []
          | otherwise =
              [failure path TypedUnresolvedName (TypedTextDetail aliasName)]
        validateImportShape (Just _) (Just _) =
          [ failure
              path
              TypedModuleInterfaceMismatch
              (TypedTextDetail "alias and selectors")
          ]
        validateImportShape _ _ = []
        validateSelectedNameShape Nothing = []
        validateSelectedNameShape (Just []) =
          [failure path TypedModuleInterfaceMismatch (TypedArityDetail 1 0)]
        validateSelectedNameShape (Just names) =
          duplicateParameterFailures
            path
            TypedDuplicateDeclaration
            TypedTextDetail
            names

importBindingCollisionFailures :: Map [Text] TypedModule -> [Text] -> [TypedResolvedImport] -> [TypedCoreValidationFailure]
importBindingCollisionFailures moduleTable modulePath imports =
  aliasCollisionFailures
    <> collisionFailures valueBindingIdentifier
    <> collisionFailures typeBindingIdentifier
  where
    aliasCollisionFailures =
      snd (foldl' checkAlias (Map.empty, []) imports)
    checkAlias (origins, failures) (TypedResolvedImport _ importPath alias _) =
      case alias of
        Nothing -> (origins, failures)
        Just aliasName ->
          case Map.lookup aliasName origins of
            Nothing -> (Map.insert aliasName importPath origins, failures)
            Just _ ->
              ( origins,
                failures
                  <> [ failure
                         (TypedModulePath modulePath)
                         TypedDuplicateDeclaration
                         (TypedTextDetail aliasName)
                     ]
              )
    collisionFailures identifierFor =
      snd (foldl' (checkImport identifierFor) (Map.empty, []) imports)
    checkImport identifierFor (origins, failures) (TypedResolvedImport _ importPath alias selectedNames)
      | Just _ <- alias = (origins, failures)
      | otherwise =
          case Map.lookup importPath moduleTable of
            Nothing -> (origins, failures)
            Just importedModule ->
              foldl'
                (checkIdentifier importPath)
                (origins, failures)
                (nub (mapMaybe identifierFor (interfaceNameKeys (importPath, selectedNames, importedModule))))
    checkIdentifier importPath (origins, failures) identifier =
      case Map.lookup identifier origins of
        Nothing -> (Map.insert identifier importPath origins, failures)
        Just originalPath
          | originalPath == importPath -> (origins, failures)
          | otherwise ->
              ( origins,
                failures
                  <> [ failure
                         (TypedModulePath modulePath)
                         TypedDuplicateDeclaration
                         (TypedTextDetail identifier)
                     ]
              )
    valueBindingIdentifier key =
      case key of
        ResolvedNameKey _ TypedValueNamespace identifier -> Just identifier
        ResolvedNameKey _ TypedConstructorNamespace identifier -> Just identifier
        ResolvedNameKey _ TypedCapabilityNamespace identifier -> Just identifier
        _ -> Nothing
    typeBindingIdentifier key =
      case key of
        ResolvedNameKey _ TypedTypeNamespace identifier -> Just identifier
        _ -> Nothing

moduleExportsImportSelectorName :: Text -> TypedModule -> Bool
moduleExportsImportSelectorName expected (TypedModule _ _ _ exports interface _ _ _) =
  any matchesExport exports
  where
    matchesExport export@(TypedModuleExport namespace exportedName) =
      exportedName == expected
        && namespace
          `elem` [ TypedValueNamespace,
                   TypedConstructorNamespace,
                   TypedCapabilityNamespace
                 ]
        && interfaceContainsExport exports export interface

interfaceContainsExport :: [TypedModuleExport] -> TypedModuleExport -> TypedModuleInterface -> Bool
interfaceContainsExport exports (TypedModuleExport namespace expected) (TypedModuleInterface values datas classes _) =
  case namespace of
    TypedValueNamespace -> any (interfaceNameMatches expected) values || any (classInterfaceMethodMatches expected) classes
    TypedTypeNamespace -> any (dataInterfaceNameMatches expected) datas
    TypedConstructorNamespace -> interfaceConstructorOwner exports datas expected /= Nothing
    TypedCapabilityNamespace -> any (classInterfaceNameMatches expected) classes

validateModuleResult :: Bool -> [Text] -> [TypedStatement] -> TypedNodeInfo -> [TypedCoreValidationFailure]
validateModuleResult compareMetadata modulePath statements moduleInfo =
  case reverse statements of
    TypedExpressionStatement _ terminal : _
      | nodeInfoHasCompatibleIntrinsicContract moduleInfo && nodeInfoHasCompatibleIntrinsicContract (typedExpressionInfo terminal) ->
          case nodeContractFailures
            (TypedModulePath modulePath)
            TypedModuleResultMismatch
            moduleInfo
            (typedExpressionInfo terminal) of
            []
              | compareMetadata && moduleInfo /= typedExpressionInfo terminal ->
                  [ failure
                      (TypedModulePath modulePath)
                      TypedModuleResultMismatch
                      TypedNoValidationDetail
                  ]
            failures -> failures
    TypedExpressionStatement {} : _ -> []
    _
      | moduleInfoIsNoResultContract moduleInfo -> []
      | otherwise ->
          [ failure
              (TypedModulePath modulePath)
              TypedModuleResultMismatch
              TypedNoValidationDetail
          ]

moduleInfoIsNoResultContract :: TypedNodeInfo -> Bool
moduleInfoIsNoResultContract (TypedNodeInfo typeValue recipe instantiations evidenceSelections) =
  typeValue == TypedTupleType []
    && recipe == TypedUnitRecipe
    && null instantiations
    && null evidenceSelections

validateModuleInfo :: ModuleContext -> TypedCoreValidationPath -> [TypedStatement] -> TypedNodeInfo -> [TypedCoreValidationFailure]
validateModuleInfo context path statements moduleInfo =
  case reverse statements of
    TypedExpressionStatement _ terminal : _
      | typedExpressionInfo terminal == moduleInfo -> []
    _ -> validateNodeInfo context path Set.empty False Nothing Nothing moduleInfo

validateSourcePath :: [Text] -> TypedSourcePath -> [TypedCoreValidationFailure]
validateSourcePath modulePath sourcePath@(TypedSourcePath sourcePathText)
  | validTypedSourcePath sourcePath = []
  | otherwise =
      [ failure
          (TypedModulePath modulePath)
          TypedInvalidSourcePath
          (TypedTextDetail sourcePathText)
      ]

duplicateModuleFailures :: [TypedModule] -> [TypedCoreValidationFailure]
duplicateModuleFailures = snd . foldl' step (Set.empty, [])
  where
    step (seen, failures) moduleValue =
      let modulePath = typedModulePath moduleValue
       in if Set.member modulePath seen
            then
              ( seen,
                failures
                  <> [ failure
                         (TypedModulePath modulePath)
                         TypedDuplicateModule
                         (TypedTextDetail (renderModulePath modulePath))
                     ]
              )
            else (Set.insert modulePath seen, failures)

interfaceSchemeEntries :: ([Text], Maybe [Text], TypedModule) -> [(TypedBinderId, TypedScheme)]
interfaceSchemeEntries (modulePath, selectedNames, TypedModule _ _ _ exports (TypedModuleInterface values _ classes _) _ _ _) =
  [ (binderId, qualifyExternalScheme modulePath scheme)
  | TypedValueInterface name scheme@(TypedScheme binderId _ _ _ _ _ _) <- values,
    importAllows selectedNames name,
    moduleExportsName TypedValueNamespace name exports
  ]
    <> [ (binderId, qualifyExternalScheme modulePath (generalizeImportedClassMethodScheme className classParameters name scheme))
       | TypedClassInterface (TypedClassDeclaration _ className classParameters methods) <- classes,
         moduleOwnedCapabilityName modulePath className,
         TypedMethodSignature name _ scheme@(TypedScheme binderId _ _ _ _ _ _) <- methods,
         importAllows selectedNames name,
         moduleExportsName TypedValueNamespace name exports
       ]

generalizeClassMethodScheme :: [TypedTypeParameterId] -> TypedScheme -> TypedScheme
generalizeClassMethodScheme classParameters (TypedScheme owner methodParameters evidence primitive resultType resultRecipe callableShape) =
  TypedScheme owner (usedClassParameters <> methodParameters) evidence primitive resultType resultRecipe callableShape
  where
    usedClassParameters =
      [ parameter
      | parameter <- classParameters,
        parameter `notElem` methodParameters,
        schemeMentionsTypeParameter parameter evidence primitive resultType resultRecipe
      ]

generalizeImportedClassMethodScheme :: TypedCoreName -> [TypedTypeParameterId] -> TypedCoreName -> TypedScheme -> TypedScheme
generalizeImportedClassMethodScheme className classParameters methodName scheme =
  case generalizeClassMethodScheme classParameters scheme of
    TypedScheme owner parameters evidence primitive resultType resultRecipe callableShape ->
      TypedScheme
        owner
        importedParameters
        (evidence <> dispatchEvidence importedParameters evidence)
        primitive
        resultType
        resultRecipe
        callableShape
      where
        importedParameters =
          classParameters
            <> filter (`notElem` classParameters) parameters
  where
    dispatchEvidence parameters evidence =
      case ( filter (`elem` parameters) classParameters,
             coreNameIdentifier className,
             coreNameIdentifier methodName
           ) of
        ([targetParameter], Just classIdentifier, Just methodIdentifier) ->
          [ TypedEvidenceParameter
              (TypedEvidenceParameterId (length evidence))
              ( TypedCapabilityConstraint
                  className
                  (Just (classIdentifier <> "::" <> methodIdentifier))
                  (TypedTypeParameterType targetParameter)
              )
          ]
        _ -> []

qualifyExternalScheme :: [Text] -> TypedScheme -> TypedScheme
qualifyExternalScheme modulePath (TypedScheme owner parameters evidence primitive resultType resultRecipe callableShape) =
  TypedScheme
    owner
    parameters
    (map qualifyEvidence evidence)
    primitive
    resultType
    resultRecipe
    callableShape
  where
    qualifyEvidence (TypedEvidenceParameter parameterId (TypedCapabilityConstraint capability method targetType)) =
      TypedEvidenceParameter
        parameterId
        ( TypedCapabilityConstraint
            (qualifyExternalName modulePath capability)
            (qualifyExternalMethodKey modulePath capability method)
            targetType
        )

qualifyExternalMethodKey :: [Text] -> TypedCoreName -> Maybe Text -> Maybe Text
qualifyExternalMethodKey modulePath capability maybeMethod =
  case maybeMethod of
    Just method
      | Just (Just qualifier, methodName) <- methodKeyParts method,
        capabilityMethodQualifier capability == Just qualifier,
        Just externalQualifier <-
          capabilityMethodQualifier (qualifyExternalName modulePath capability) ->
          Just (externalQualifier <> "::" <> methodName)
    _ -> maybeMethod

qualifyExternalClassDeclaration :: [Text] -> TypedClassDeclaration -> TypedClassDeclaration
qualifyExternalClassDeclaration modulePath (TypedClassDeclaration spanValue name parameters methods) =
  TypedClassDeclaration
    spanValue
    (qualifyExternalName modulePath name)
    parameters
    [ TypedMethodSignature
        (qualifyExternalName modulePath methodName)
        methodSpan
        (qualifyExternalScheme modulePath scheme)
    | TypedMethodSignature methodName methodSpan scheme <- methods
    ]

schemeMentionsTypeParameter ::
  TypedTypeParameterId ->
  [TypedEvidenceParameter] ->
  [TypedPrimitiveConstraint] ->
  TypedType ->
  TypedRepresentationRecipe ->
  Bool
schemeMentionsTypeParameter parameter evidence primitive resultType resultRecipe =
  any
    (typeMentionsParameter parameter)
    ( resultType
        : [targetType | TypedEvidenceParameter _ (TypedCapabilityConstraint _ _ targetType) <- evidence]
          <> [ targetType
             | constraint <- primitive,
               targetType <-
                 case constraint of
                   TypedNumericPrimitiveConstraint _ value -> [value]
                   TypedStrictEqualityPrimitiveConstraint value -> [value]
             ]
    )
    || recipeMentionsParameter parameter resultRecipe

interfaceVisibleImplIds :: ([Text], Maybe [Text], TypedModule) -> [TypedImplId]
interfaceVisibleImplIds visibleModule@(modulePath, _, TypedModule _ _ _ _ (TypedModuleInterface _ _ _ impls) _ _ _) =
  [ qualifyExternalImplId modulePath implId
  | TypedImplInterface implId <- impls,
    implImportAllowed visibleModule implId
  ]

interfaceImplMethodEntries :: ([Text], Maybe [Text], TypedModule) -> [(TypedImplId, Set Text)]
interfaceImplMethodEntries visibleModule@(modulePath, _, TypedModule _ _ _ _ (TypedModuleInterface _ _ _ impls) _ statements _) =
  [ ( qualifyExternalImplId modulePath implId,
      Set.fromList [methodKey | TypedMethodDefinition (TypedMethodId _ methodKey) _ _ _ _ <- methods]
    )
  | TypedImplInterface implId <- impls,
    implImportAllowed visibleModule implId,
    TypedImplStatement (TypedImplDeclaration _ declarationImplId methods) <- statements,
    declarationImplId == implId
  ]

implImportAllowed :: ([Text], Maybe [Text], TypedModule) -> TypedImplId -> Bool
implImportAllowed visibleModule@(modulePath, _, TypedModule _ _ _ _ (TypedModuleInterface _ _ classes _) _ _ _) (TypedImplId _ capability _) =
  any capabilityIncluded classes
  where
    capabilityIncluded (TypedClassInterface (TypedClassDeclaration _ name _ methods)) =
      case ( resolvedNameKey modulePath name,
             resolvedNameKey modulePath capability
           ) of
        (Just nameKey, Just capabilityKey) ->
          nameKey == capabilityKey
            && interfaceCapabilityIncluded visibleModule name methods
        _ -> False

interfaceDataMetadataDeclarations ::
  Map [Text] TypedModule ->
  [([Text], Maybe [Text], TypedModule)] ->
  Map ResolvedNameKey ([Text], TypedDataDeclaration)
interfaceDataMetadataDeclarations moduleTable visibleModules =
  Map.restrictKeys catalog (closeDataMetadataKeys catalog rootKeys)
  where
    catalog =
      Map.fromList
        [ (key, (modulePath, declaration))
        | (modulePath, TypedModule _ _ _ _ (TypedModuleInterface _ datas _ _) _ _ _) <-
            Map.toList moduleTable,
          TypedDataInterface declaration@(TypedDataDeclaration _ name _ _) <- datas,
          key <- maybeToList (definitionNameKey modulePath name)
        ]
    rootKeys =
      Set.fromList
        ( concatMap sourceVisibleDataKeys visibleModules
            <> concatMap selectedSchemeDataKeys visibleModules
        )
    sourceVisibleDataKeys (modulePath, selectedNames, TypedModule _ _ _ exports (TypedModuleInterface _ datas _ _) _ _ _) =
      [ key
      | TypedDataInterface (TypedDataDeclaration _ name _ constructors) <- datas,
        sourceVisibleDataIncluded selectedNames exports name constructors,
        key <- maybeToList (definitionNameKey modulePath name)
      ]
    selectedSchemeDataKeys visibleModule =
      concatMap schemeDataTypeKeys selectedSchemes
      where
        selectedSchemes =
          map snd (interfaceSchemeEntries visibleModule)
            <> interfaceCapabilitySchemes visibleModule

sourceVisibleDataIncluded ::
  Maybe [Text] ->
  [TypedModuleExport] ->
  TypedCoreName ->
  [TypedConstructorDeclaration] ->
  Bool
sourceVisibleDataIncluded selectedNames exports name constructors =
  (importAllows selectedNames name && moduleExportsName TypedTypeNamespace name exports)
    || any constructorIncluded constructors
  where
    constructorIncluded (TypedConstructorDeclaration _ constructorName _ _) =
      importAllows selectedNames constructorName
        && moduleExportsName TypedConstructorNamespace constructorName exports

closeDataMetadataKeys ::
  Map ResolvedNameKey ([Text], TypedDataDeclaration) ->
  Set ResolvedNameKey ->
  Set ResolvedNameKey
closeDataMetadataKeys catalog roots = go roots roots
  where
    go seen pending =
      case Set.minView pending of
        Nothing -> seen
        Just (key, rest) ->
          let dependencies =
                case Map.lookup key catalog of
                  Nothing -> Set.empty
                  Just (modulePath, TypedDataDeclaration _ _ _ constructors) ->
                    Set.fromList
                      [ dependencyKey
                      | TypedConstructorDeclaration _ _ fields _ <- constructors,
                        field <- fields,
                        dependencyKey <- typeDataKeys modulePath field
                      ]
              unseen = Set.difference dependencies seen
           in go (Set.union seen unseen) (Set.union rest unseen)

interfaceCapabilitySchemes :: ([Text], Maybe [Text], TypedModule) -> [TypedScheme]
interfaceCapabilitySchemes visibleModule@(_, _, TypedModule _ _ _ _ (TypedModuleInterface _ _ classes _) _ _ _) =
  [ scheme
  | TypedClassInterface (TypedClassDeclaration _ name _ methods) <- classes,
    interfaceCapabilityIncluded visibleModule name methods,
    TypedMethodSignature _ _ scheme <- methods
  ]

schemeDataTypeKeys :: TypedScheme -> [ResolvedNameKey]
schemeDataTypeKeys (TypedScheme owner _ evidence primitive resultType _ _) =
  concatMap (typeDataKeys modulePath) (resultType : evidenceTypes <> primitiveTypes)
  where
    modulePath = binderModulePath owner
    evidenceTypes = [targetType | TypedEvidenceParameter _ (TypedCapabilityConstraint _ _ targetType) <- evidence]
    primitiveTypes =
      [ typeValue
      | constraint <- primitive,
        typeValue <- case constraint of
          TypedNumericPrimitiveConstraint _ value -> [value]
          TypedStrictEqualityPrimitiveConstraint value -> [value]
      ]

typeDataKeys :: [Text] -> TypedType -> [ResolvedNameKey]
typeDataKeys modulePath typeValue =
  case typeValue of
    TypedListType elementType -> typeDataKeys modulePath elementType
    TypedTupleType elementTypes -> concatMap (typeDataKeys modulePath) elementTypes
    TypedDataType name arguments ->
      maybeToList (resolvedNameKey modulePath name)
        <> concatMap (typeDataKeys modulePath) arguments
    TypedFunctionType argument result ->
      typeDataKeys modulePath argument <> typeDataKeys modulePath result
    _ -> []

interfaceNameKeys :: ([Text], Maybe [Text], TypedModule) -> [ResolvedNameKey]
interfaceNameKeys visibleModule@(modulePath, selectedNames, TypedModule _ _ _ exports (TypedModuleInterface values datas classes _) _ _ _) =
  concat
    [ [ key
      | TypedValueInterface name _ <- values,
        importAllows selectedNames name,
        moduleExportsName TypedValueNamespace name exports,
        key <- maybeToList (definitionNameKey modulePath name)
      ],
      [ key
      | TypedDataInterface (TypedDataDeclaration _ name _ _) <- datas,
        importAllows selectedNames name,
        moduleExportsName TypedTypeNamespace name exports,
        key <- maybeToList (definitionNameKey modulePath name)
      ],
      [ key
      | TypedDataInterface (TypedDataDeclaration _ _ _ constructors) <- datas,
        TypedConstructorDeclaration _ name _ _ <- constructors,
        importAllows selectedNames name,
        moduleExportsName TypedConstructorNamespace name exports,
        key <- maybeToList (definitionNameKey modulePath name)
      ],
      [ key
      | TypedClassInterface (TypedClassDeclaration _ name _ _) <- classes,
        interfaceCapabilityNameDirectlyIncluded visibleModule name,
        key <- maybeToList (definitionNameKey modulePath name)
      ],
      [ key
      | TypedClassInterface (TypedClassDeclaration _ className _ methods) <- classes,
        moduleOwnedCapabilityName modulePath className,
        TypedMethodSignature name _ _ <- methods,
        importAllows selectedNames name,
        moduleExportsName TypedValueNamespace name exports,
        key <- maybeToList (definitionNameKey modulePath name)
      ]
    ]

interfaceConstructorEntries :: ([Text], Maybe [Text], TypedModule) -> [(ResolvedNameKey, ConstructorContract)]
interfaceConstructorEntries (modulePath, selectedNames, TypedModule _ _ _ exports (TypedModuleInterface _ datas _ _) _ _ _) =
  [ (constructorKey, ConstructorContract binderId dataKey parameters (map (qualifyExternalType modulePath) fields))
  | TypedDataInterface (TypedDataDeclaration _ dataName parameters constructors) <- datas,
    dataKey <- maybeToList (definitionNameKey modulePath dataName),
    TypedConstructorDeclaration binderId constructorName fields _ <- constructors,
    importAllows selectedNames constructorName,
    moduleExportsName TypedConstructorNamespace constructorName exports,
    constructorIdentifier <- maybeToList (coreNameIdentifier constructorName),
    interfaceConstructorOwner exports datas constructorIdentifier == Just dataName,
    constructorKey <- maybeToList (definitionNameKey modulePath constructorName)
  ]

interfaceCapabilityEntries :: ([Text], Maybe [Text], TypedModule) -> [(ResolvedNameKey, CapabilityContract)]
interfaceCapabilityEntries visibleModule@(modulePath, _, TypedModule _ _ _ _ (TypedModuleInterface _ _ classes _) _ _ _) =
  [ entry
  | TypedClassInterface declaration@(TypedClassDeclaration _ name _ methods) <- classes,
    interfaceCapabilityIncluded visibleModule name methods,
    entry <- maybeToList (capabilityEntry modulePath declaration)
  ]

interfaceClassDeclarations :: ([Text], Maybe [Text], TypedModule) -> [TypedClassDeclaration]
interfaceClassDeclarations visibleModule@(modulePath, _, TypedModule _ _ _ _ (TypedModuleInterface _ _ classes _) _ _ _) =
  [ qualifyExternalClassDeclaration modulePath declaration
  | TypedClassInterface declaration@(TypedClassDeclaration _ name _ methods) <- classes,
    interfaceCapabilityIncluded visibleModule name methods
  ]

interfaceEvidenceCapabilityEntries :: ([Text], Maybe [Text], TypedModule) -> [(TypedEvidenceParameterRef, ResolvedNameKey)]
interfaceEvidenceCapabilityEntries visibleModule =
  evidenceCapabilityEntries
    capabilityContracts
    (Map.keysSet capabilityContracts)
    (interfaceSchemeEntries visibleModule)
  where
    capabilityContracts =
      Map.fromList (interfaceCapabilityEntries visibleModule)

interfaceCapabilityIncluded :: ([Text], Maybe [Text], TypedModule) -> TypedCoreName -> [TypedMethodSignature] -> Bool
interfaceCapabilityIncluded visibleModule@(modulePath, _, _) name methods =
  interfaceCapabilityNameIncluded visibleModule name methods
    || maybe
      False
      (`Set.member` requiredCapabilityKeys visibleModule)
      (resolvedNameKey modulePath name)

interfaceCapabilityNameIncluded :: ([Text], Maybe [Text], TypedModule) -> TypedCoreName -> [TypedMethodSignature] -> Bool
interfaceCapabilityNameIncluded visibleModule@(modulePath, selectedNames, TypedModule _ _ _ exports _ _ _ _) name methods =
  interfaceCapabilityNameDirectlyIncluded visibleModule name
    || (moduleOwnedCapabilityName modulePath name && any methodImported methods)
  where
    methodImported (TypedMethodSignature methodName _ _) =
      importAllows selectedNames methodName && moduleExportsName TypedValueNamespace methodName exports

interfaceCapabilityNameDirectlyIncluded :: ([Text], Maybe [Text], TypedModule) -> TypedCoreName -> Bool
interfaceCapabilityNameDirectlyIncluded (modulePath, selectedNames, TypedModule _ _ _ exports _ _ _ _) name =
  moduleOwnedCapabilityName modulePath name
    && importAllows selectedNames name
    && moduleExportsName TypedCapabilityNamespace name exports

moduleOwnedCapabilityName :: [Text] -> TypedCoreName -> Bool
moduleOwnedCapabilityName modulePath name =
  case name of
    TypedResolvedName TypedCurrentModule TypedCapabilityNamespace _ -> True
    TypedResolvedName TypedAmbientPrelude TypedCapabilityNamespace _ ->
      modulePath == ["Prelude"]
    _ -> False

requiredCapabilityKeys :: ([Text], Maybe [Text], TypedModule) -> Set ResolvedNameKey
requiredCapabilityKeys visibleModule@(modulePath, _, _) =
  Set.fromList
    [ key
    | (_, TypedScheme _ _ evidence _ _ _ _) <- interfaceSchemeEntries visibleModule,
      TypedEvidenceParameter _ (TypedCapabilityConstraint capability _ _) <- evidence,
      key <- maybeToList (resolvedNameKey modulePath capability)
    ]

importAllows :: Maybe [Text] -> TypedCoreName -> Bool
importAllows Nothing _ = True
importAllows (Just selectedNames) name = maybe False (`elem` selectedNames) (coreNameIdentifier name)

moduleExportsName :: TypedNameNamespace -> TypedCoreName -> [TypedModuleExport] -> Bool
moduleExportsName namespace name exports =
  case coreNameIdentifier name of
    Nothing -> False
    Just identifier -> TypedModuleExport namespace identifier `elem` exports

validateModuleInterface :: Map [Text] TypedModule -> TypedModule -> [TypedCoreValidationFailure]
validateModuleInterface moduleTable (TypedModule modulePath _ imports exports (TypedModuleInterface values datas classes impls) _ statements _) =
  duplicateExportFailures
    <> duplicateInterfaceFailures
    <> concatMap validateValueInterface values
    <> concatMap validateDataInterface datas
    <> concatMap validateClassInterface classes
    <> concatMap validateImplInterface impls
    <> missingImplInterfaceFailures
    <> concatMap validateExport exports
  where
    path = TypedInterfacePath modulePath
    activeDeclaredValues =
      Map.fromList
        [ (name, scheme)
        | TypedLetStatement _ name _ scheme _ <- statements
        ]
    exportSet = Set.fromList exports
    declaredDatas = [declaration | TypedDataStatement declaration <- statements]
    declaredClasses = [declaration | TypedClassStatement declaration <- statements]
    declaredImpls = [implId | TypedImplStatement (TypedImplDeclaration _ implId _) <- statements]
    visibleExternalModules = preludeModules <> importedModules
    externalClassDeclarations =
      concatMap interfaceClassDeclarations visibleExternalModules
    externalCapabilityContracts =
      Map.fromList (concatMap interfaceCapabilityEntries visibleExternalModules)
    externalVisibleImpls =
      Set.fromList (concatMap interfaceVisibleImplIds visibleExternalModules)
    declaredCapabilityKeys =
      Set.fromList
        [ key
        | declaration <- declaredClasses,
          (key, _) <- maybeToList (capabilityEntry modulePath declaration)
        ]
    interfaceCapabilityKeys =
      Set.fromList
        [ key
        | TypedClassInterface declaration <- classes,
          (key, _) <- maybeToList (capabilityEntry modulePath declaration)
        ]
    importedModules =
      [ (importPath, selectedNames, importedModule)
      | TypedResolvedImport _ importPath _ selectedNames <- imports,
        importedModule <- maybeToList (Map.lookup importPath moduleTable)
      ]
    preludeModules
      | modulePath == ["Prelude"] = []
      | otherwise =
          [ (["Prelude"], Nothing, preludeModule)
          | preludeModule <- maybeToList (Map.lookup ["Prelude"] moduleTable)
          ]
    validateValueInterface (TypedValueInterface name scheme)
      | Map.lookup name activeDeclaredValues == Just scheme && interfaceExportsName TypedValueNamespace name =
          validateValueInterfaceDependencies scheme
      | otherwise = [failure path TypedModuleInterfaceMismatch (TypedNameDetail name)]
    validateDataInterface (TypedDataInterface declaration@(TypedDataDeclaration _ name _ constructors))
      | declaration `elem` declaredDatas =
          [ failure path TypedModuleInterfaceMismatch (TypedNameDetail dependencyName)
          | dependencyName <- nub (concatMap constructorDependencies constructors),
            not (any (dataInterfaceMatches dependencyName) datas)
          ]
      | otherwise = [failure path TypedModuleInterfaceMismatch (TypedNameDetail name)]
    validateClassInterface (TypedClassInterface declaration@(TypedClassDeclaration _ name _ methods))
      | declaration `elem` declaredClasses =
          concatMap
            (\(TypedMethodSignature _ _ scheme) -> validateValueInterfaceDependencies scheme)
            methods
      | retainedClassInterfaceMatches declaration = []
      | otherwise = [failure path TypedModuleInterfaceMismatch (TypedNameDetail name)]
    validateImplInterface (TypedImplInterface implId)
      | not (any (classInterfaceMatchesImpl implId) classes) =
          [failure path TypedModuleInterfaceMismatch (TypedImplDetail implId)]
      | implId `elem` declaredImpls =
          [ failure path TypedModuleInterfaceMismatch (TypedNameDetail dependencyName)
          | dependencyName <-
              nub
                (concatMap (localDataDependencies modulePath) (implTargetTypes implId)),
            not (any (dataInterfaceMatches dependencyName) datas)
          ]
      | Set.member implId externalVisibleImpls = []
      | otherwise = [failure path TypedModuleInterfaceMismatch (TypedImplDetail implId)]
    retainedClassInterfaceMatches declaration =
      declaration `elem` externalClassDeclarations
    missingImplInterfaceFailures =
      [ failure path TypedModuleInterfaceMismatch (TypedImplDetail implId)
      | implId <- declaredImpls,
        any (classInterfaceMatchesImpl implId) classes,
        TypedImplInterface implId `notElem` impls
      ]
    classInterfaceMatchesImpl (TypedImplId _ capability _) (TypedClassInterface (TypedClassDeclaration _ name _ _)) =
      resolvedNameKey modulePath name == resolvedNameKey modulePath capability
    duplicateExportFailures =
      snd (foldl' checkExport (Set.empty, []) exports)
    checkExport (seen, failures) export@(TypedModuleExport namespace exportedName)
      | Set.member export seen =
          ( seen,
            failures
              <> [ failure
                     path
                     TypedDuplicateDeclaration
                     ( TypedNameDetail
                         (TypedResolvedName TypedCurrentModule namespace exportedName)
                     )
                 ]
          )
      | otherwise = (Set.insert export seen, failures)
    duplicateInterfaceFailures =
      duplicateParameterFailures
        path
        TypedDuplicateDeclaration
        TypedNameDetail
        [name | TypedValueInterface name _ <- values]
        <> duplicateParameterFailures
          path
          TypedDuplicateDeclaration
          TypedNameDetail
          [name | TypedDataInterface (TypedDataDeclaration _ name _ _) <- datas]
        <> duplicateParameterFailures
          path
          TypedDuplicateDeclaration
          TypedNameDetail
          [name | TypedClassInterface (TypedClassDeclaration _ name _ _) <- classes]
        <> duplicateParameterFailures
          path
          TypedDuplicateDeclaration
          TypedImplDetail
          [implId | TypedImplInterface implId <- impls]
    validateValueInterfaceDependencies scheme =
      [ failure path TypedModuleInterfaceMismatch (TypedNameDetail dependencyName)
      | dependencyName <- nub (schemeLocalDataDependencies modulePath scheme),
        not (any (dataInterfaceMatches dependencyName) datas)
      ]
        <> [ failure
               path
               TypedModuleInterfaceMismatch
               (TypedNameDetail capability)
           | (capabilityKey, capability) <-
               nub (schemeCapabilityDependencies modulePath scheme),
             Set.member
               capabilityKey
               (Set.union declaredCapabilityKeys (Map.keysSet externalCapabilityContracts)),
             Set.notMember capabilityKey interfaceCapabilityKeys
           ]
    constructorDependencies (TypedConstructorDeclaration _ _ fields _) =
      concatMap (localDataDependencies modulePath) fields
    validateExport (TypedModuleExport namespace exportedName) =
      case namespace of
        TypedValueNamespace
          | valueExportProviderCount exportedName == 1 ->
              []
        TypedTypeNamespace
          | any (dataInterfaceNameMatches exportedName) datas -> []
        TypedConstructorNamespace
          | interfaceConstructorOwner exports datas exportedName /= Nothing -> []
        TypedCapabilityNamespace
          | any (localClassInterfaceMatches exportedName) classes ->
              []
        _ -> [failure path TypedModuleInterfaceMismatch (TypedNameDetail (TypedResolvedName TypedCurrentModule namespace exportedName))]
    localClassInterfaceMatches exportedName (TypedClassInterface declaration) =
      declaration `elem` declaredClasses
        && classDeclarationMatches exportedName declaration
    interfaceExportsName namespace name =
      case coreNameIdentifier name of
        Nothing -> False
        Just identifier -> Set.member (TypedModuleExport namespace identifier) exportSet
    valueExportProviders =
      Map.fromListWith
        Set.union
        ( [ (identifier, Set.singleton owner)
          | TypedValueInterface name (TypedScheme owner _ _ _ _ _ _) <- values,
            identifier <- maybeToList (coreNameIdentifier name)
          ]
            <> [ (identifier, Set.singleton owner)
               | TypedClassInterface declaration@(TypedClassDeclaration _ _ _ methods) <- classes,
                 declaration `elem` declaredClasses,
                 TypedMethodSignature name _ (TypedScheme owner _ _ _ _ _ _) <- methods,
                 identifier <- maybeToList (coreNameIdentifier name)
               ]
        )
    valueExportProviderCount exportedName =
      maybe 0 Set.size (Map.lookup exportedName valueExportProviders)

schemeLocalDataDependencies :: [Text] -> TypedScheme -> [TypedCoreName]
schemeLocalDataDependencies modulePath (TypedScheme _ _ evidence primitive resultType _ _) =
  concatMap (localDataDependencies modulePath) (resultType : evidenceTypes <> primitiveTypes)
  where
    evidenceTypes =
      [targetType | TypedEvidenceParameter _ (TypedCapabilityConstraint _ _ targetType) <- evidence]
    primitiveTypes =
      [ typeValue
      | constraint <- primitive,
        typeValue <- case constraint of
          TypedNumericPrimitiveConstraint _ value -> [value]
          TypedStrictEqualityPrimitiveConstraint value -> [value]
      ]

localDataDependencies :: [Text] -> TypedType -> [TypedCoreName]
localDataDependencies modulePath typeValue =
  case typeValue of
    TypedListType elementType -> localDataDependencies modulePath elementType
    TypedTupleType elementTypes -> concatMap (localDataDependencies modulePath) elementTypes
    TypedDataType name arguments ->
      [ name
      | case name of
          TypedResolvedName TypedCurrentModule TypedTypeNamespace _ -> True
          TypedResolvedName TypedAmbientPrelude TypedTypeNamespace _ ->
            modulePath == ["Prelude"]
          _ -> False
      ]
        <> concatMap (localDataDependencies modulePath) arguments
    TypedFunctionType argument result ->
      localDataDependencies modulePath argument
        <> localDataDependencies modulePath result
    _ -> []

schemeCapabilityDependencies :: [Text] -> TypedScheme -> [(ResolvedNameKey, TypedCoreName)]
schemeCapabilityDependencies modulePath (TypedScheme _ _ evidence _ _ _ _) =
  [ (key, capability)
  | TypedEvidenceParameter _ (TypedCapabilityConstraint capability _ _) <- evidence,
    key <- maybeToList (resolvedNameKey modulePath capability)
  ]

dataInterfaceMatches :: TypedCoreName -> TypedDataInterface -> Bool
dataInterfaceMatches expected (TypedDataInterface (TypedDataDeclaration _ actual _ _)) =
  expected == actual

classDeclarationMatches :: Text -> TypedClassDeclaration -> Bool
classDeclarationMatches expected (TypedClassDeclaration _ name _ _) =
  coreNameIdentifier name == Just expected

interfaceNameMatches :: Text -> TypedValueInterface -> Bool
interfaceNameMatches expected (TypedValueInterface name _) = coreNameIdentifier name == Just expected

dataInterfaceNameMatches :: Text -> TypedDataInterface -> Bool
dataInterfaceNameMatches expected (TypedDataInterface (TypedDataDeclaration _ name _ _)) = coreNameIdentifier name == Just expected

classInterfaceNameMatches :: Text -> TypedClassInterface -> Bool
classInterfaceNameMatches expected (TypedClassInterface (TypedClassDeclaration _ name _ _)) = coreNameIdentifier name == Just expected

classInterfaceMethodMatches :: Text -> TypedClassInterface -> Bool
classInterfaceMethodMatches expected (TypedClassInterface (TypedClassDeclaration _ _ _ methods)) =
  any methodMatches methods
  where
    methodMatches (TypedMethodSignature name _ _) = coreNameIdentifier name == Just expected

dataInterfaceConstructorMatches :: Text -> TypedDataInterface -> Bool
dataInterfaceConstructorMatches expected (TypedDataInterface (TypedDataDeclaration _ _ _ constructors)) =
  any constructorMatches constructors
  where
    constructorMatches (TypedConstructorDeclaration _ name _ _) = coreNameIdentifier name == Just expected

interfaceConstructorOwner :: [TypedModuleExport] -> [TypedDataInterface] -> Text -> Maybe TypedCoreName
interfaceConstructorOwner exports datas constructorIdentifier =
  case exportedCandidates of
    [owner] -> Just owner
    [] -> listToMaybe (reverse candidates)
    _ -> Nothing
  where
    candidates =
      [ dataName
      | dataInterface@(TypedDataInterface (TypedDataDeclaration _ dataName _ _)) <- datas,
        dataInterfaceConstructorMatches constructorIdentifier dataInterface
      ]
    exportedCandidates =
      [ dataName
      | dataName <- candidates,
        moduleExportsName TypedTypeNamespace dataName exports
      ]
