{-# LANGUAGE OverloadedStrings #-}

-- | Complete structural validation for the semantic typed-core boundary.
-- Validation is deliberately independent of inference, evaluation, and
-- lowering: it accepts an already-constructed contract value and reports all
-- invariant failures in stable structural order.
module Jazz.Compiler.TypedCore.Validate
  ( ValidatedTypedProgram,
    validateTypedProgram,
    validateTypedProgramOnce,
    validatedTypedProgram,
  )
where

import Data.Graph (SCC (..), stronglyConnComp)
import Data.List (find, nub, sort)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isNothing, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Jazz.Compiler.AST
  ( NumericType (..),
  )
import Jazz.Compiler.BuiltinCatalog
  ( BuiltinSymbol (..),
    builtinSymbolNumericConversionTarget,
    lookupBuiltinSymbol,
    lookupKernelBuiltinSymbol,
  )
import Jazz.Compiler.Name (operatorBindingIdentifierText)
import Jazz.Compiler.Parser.Operator (isValidUserOperatorSymbol)
import Jazz.Compiler.TypedCore
import Jazz.Compiler.TypedCore.Validate.Evidence
import Jazz.Compiler.TypedCore.Validate.Internal
import Jazz.Compiler.TypedCore.Validate.Patterns
import Jazz.Compiler.TypedCore.Validate.TypeRecipes

validateTypedProgramOnce :: TypedProgram -> Either [TypedCoreValidationFailure] ValidatedTypedProgram
validateTypedProgramOnce typedProgram =
  case validateTypedProgram typedProgram of
    [] -> Right (ValidatedTypedProgram typedProgram)
    failures -> Left failures

validateTypedProgram :: TypedProgram -> [TypedCoreValidationFailure]
validateTypedProgram (TypedProgram prelude modules entryModule) =
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

modulePathReachable :: Map [Text] TypedModule -> Set [Text] -> [Text] -> [Text] -> Bool
modulePathReachable moduleTable initialSeen currentPath targetPath =
  go initialSeen [currentPath]
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
        not (modulePathReachable moduleTable Set.empty importPath modulePath)
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
        && interfaceContainsExport export interface

interfaceContainsExport :: TypedModuleExport -> TypedModuleInterface -> Bool
interfaceContainsExport (TypedModuleExport namespace expected) (TypedModuleInterface values datas classes _) =
  case namespace of
    TypedValueNamespace -> any (interfaceNameMatches expected) values || any (classInterfaceMethodMatches expected) classes
    TypedTypeNamespace -> any (dataInterfaceNameMatches expected) datas
    TypedConstructorNamespace -> any (dataInterfaceConstructorMatches expected) datas
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

duplicateBinderFailures :: ModuleContext -> [([Int], TypedStatement)] -> [TypedCoreValidationFailure]
duplicateBinderFailures context statements = snd (foldl' step (Set.empty, []) occurrences)
  where
    occurrences = concatMap (statementBinderOccurrences context) statements
    step (seen, failures) (BinderOccurrence path binderId)
      | Set.member binderId seen =
          ( seen,
            failures
              <> [ failure
                     path
                     TypedDuplicateBinder
                     (TypedBinderDetail binderId)
                 ]
          )
      | otherwise = (Set.insert binderId seen, failures)

duplicateDeclarationFailures :: ModuleContext -> [([Int], TypedStatement)] -> [TypedCoreValidationFailure]
duplicateDeclarationFailures context statements = nameFailures <> implFailures
  where
    nameFailures = snd (foldl' step (Map.empty, []) occurrences)
    occurrences = concat (zipWith declarationOccurrences statements nextStatements)
    nextStatements = map (Just . snd) (drop 1 statements) <> [Nothing]
    declarationOccurrences (statementLocation, statement) maybeNextStatement =
      [ ( statementPath,
          key,
          name,
          identity
        )
      | (name, identity) <- duplicateCheckedDeclarations statement maybeNextStatement,
        key <- maybeToList (resolvedNameKey (moduleContextPath context) name)
      ]
      where
        statementPath = TypedStatementPath (moduleContextPath context) statementLocation
    step (seen, failures) (path, key, name, identity) =
      case Map.lookup key seen of
        Just previousIdentity
          | previousIdentity /= identity || identity == Nothing ->
              ( seen,
                failures <> [failure path TypedDuplicateDeclaration (TypedNameDetail name)]
              )
        Just _ -> (seen, failures)
        Nothing -> (Map.insert key identity seen, failures)
    implFailures = snd (foldl' implStep (Set.empty, []) implOccurrences)
    implOccurrences =
      [ ( TypedStatementPath (moduleContextPath context) statementLocation,
          implId,
          qualifyExternalImplId (moduleContextPath context) implId
        )
      | (statementLocation, TypedImplStatement (TypedImplDeclaration _ implId _)) <- statements
      ]
    implStep (seen, failures) (path, implId, normalizedImplId)
      | Set.member normalizedImplId seen =
          (seen, failures <> [failure path TypedDuplicateDeclaration (TypedImplDetail implId)])
      | otherwise = (Set.insert normalizedImplId seen, failures)

duplicateCheckedDeclarations :: TypedStatement -> Maybe TypedStatement -> [(TypedCoreName, Maybe TypedBinderId)]
duplicateCheckedDeclarations statement maybeNextStatement =
  case statement of
    TypedLetStatement {} -> []
    TypedSignatureStatement binderId name _ _
      | Just (TypedLetStatement _ bindingName _ _ _) <- maybeNextStatement,
        name == bindingName ->
          []
      | otherwise -> [(name, Just binderId)]
    TypedDataStatement (TypedDataDeclaration _ name _ constructors) ->
      (name, Nothing)
        : [(constructorName, Just binderId) | TypedConstructorDeclaration binderId constructorName _ _ <- constructors]
    TypedClassStatement (TypedClassDeclaration _ name _ _) -> [(name, Nothing)]
    TypedImplStatement {} -> []
    TypedExpressionStatement {} -> []

statementBinderDefinitions :: TypedStatement -> [TypedBinderId]
statementBinderDefinitions statement =
  case statement of
    TypedLetStatement binderId _ _ _ _ -> [binderId]
    TypedSignatureStatement binderId _ _ _ -> [binderId]
    TypedDataStatement (TypedDataDeclaration _ _ _ constructors) ->
      [binderId | TypedConstructorDeclaration binderId _ _ _ <- constructors]
    TypedClassStatement (TypedClassDeclaration _ _ _ methods) ->
      [binderId | TypedMethodSignature _ _ (TypedScheme binderId _ _ _ _ _ _) <- methods]
    TypedImplStatement (TypedImplDeclaration _ _ methods) ->
      [binderId | TypedMethodDefinition _ binderId _ _ _ <- methods]
    TypedExpressionStatement {} -> []

statementBinderOccurrences :: ModuleContext -> ([Int], TypedStatement) -> [BinderOccurrence]
statementBinderOccurrences context (statementLocation, statement) =
  [BinderOccurrence statementPath binderId | binderId <- statementBinderDefinitions statement]
    <> case statement of
      TypedLetStatement _ _ _ _ expression -> expressionBinderOccurrences context statementLocation [0] expression
      TypedImplStatement (TypedImplDeclaration _ _ methods) ->
        concat
          [ expressionBinderOccurrences context statementLocation [methodIndex] expression
          | (methodIndex, TypedMethodDefinition _ _ _ _ expression) <- zip [0 ..] methods
          ]
      TypedExpressionStatement _ expression -> expressionBinderOccurrences context statementLocation [0] expression
      _ -> []
  where
    statementPath = TypedStatementPath (moduleContextPath context) statementLocation

expressionBinderOccurrences :: ModuleContext -> [Int] -> [Int] -> TypedExpr -> [BinderOccurrence]
expressionBinderOccurrences context statementLocation expressionPath expression =
  ownedOccurrences <> patternOccurrences <> childOccurrences
  where
    expressionValidationPath = TypedExpressionPath (moduleContextPath context) statementLocation expressionPath
    ownedOccurrences =
      case expression of
        TypedLambdaExpr _ binderId _ _ -> [BinderOccurrence expressionValidationPath binderId]
        _ -> []
    patternOccurrences =
      case expression of
        TypedPatternCaseExpr _ _ arms ->
          concat
            [ patternBinderOccurrences (moduleContextPath context) statementLocation (expressionPath <> [armIndex]) patternValue
            | (armIndex, TypedCaseArm patternValue _ _) <- zip [0 ..] arms
            ]
        _ -> []
    childOccurrences =
      case expression of
        TypedBlockExpr _ statements ->
          concat
            [ statementBinderOccurrences
                context
                (nestedStatementLocation statementLocation expressionPath blockIndex, statement)
            | (blockIndex, statement) <- zip [0 ..] statements
            ]
        _ ->
          concat
            [ expressionBinderOccurrences context statementLocation (expressionPath <> [childIndex]) child
            | (childIndex, child) <- zip [0 ..] (expressionChildren expression)
            ]

statementSchemes :: TypedStatement -> [(TypedBinderId, TypedScheme)]
statementSchemes statement =
  case statement of
    TypedLetStatement binderId _ _ scheme _ -> [(binderId, scheme)]
    TypedSignatureStatement {} -> []
    _ -> []

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

statementImplEntries :: TypedStatement -> [(TypedImplId, Set Text)]
statementImplEntries statement =
  case statement of
    TypedImplStatement (TypedImplDeclaration _ implId methods) ->
      [(implId, Set.fromList [methodKey | TypedMethodDefinition (TypedMethodId _ methodKey) _ _ _ _ <- methods])]
    _ -> []

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

statementDefinedNameKeys :: [Text] -> TypedStatement -> [ResolvedNameKey]
statementDefinedNameKeys modulePath statement =
  [ key
  | name <- statementDefinedNames statement,
    key <- maybeToList (resolvedNameKey modulePath name)
  ]

statementDefinedNames :: TypedStatement -> [TypedCoreName]
statementDefinedNames statement =
  case statement of
    TypedLetStatement _ name _ _ _ -> [name]
    TypedSignatureStatement {} -> []
    TypedDataStatement (TypedDataDeclaration _ name _ constructors) ->
      name : [constructorName | TypedConstructorDeclaration _ constructorName _ _ <- constructors]
    TypedClassStatement (TypedClassDeclaration _ name _ _) -> [name]
    TypedImplStatement {} -> []
    TypedExpressionStatement {} -> []

statementDataEntries :: [Text] -> TypedStatement -> [(ResolvedNameKey, Int)]
statementDataEntries modulePath statement =
  case statement of
    TypedDataStatement (TypedDataDeclaration _ name parameters _) ->
      [(key, length parameters) | key <- maybeToList (definitionNameKey modulePath name)]
    _ -> []

statementDataContractEntries :: [Text] -> TypedStatement -> [(ResolvedNameKey, DataContract)]
statementDataContractEntries modulePath statement =
  case statement of
    TypedDataStatement (TypedDataDeclaration _ name parameters constructors) ->
      [ (key, DataContract parameters [fields | TypedConstructorDeclaration _ _ fields _ <- constructors])
      | key <- maybeToList (definitionNameKey modulePath name)
      ]
    _ -> []

statementConstructorEntries :: [Text] -> TypedStatement -> [(ResolvedNameKey, ConstructorContract)]
statementConstructorEntries modulePath statement =
  case statement of
    TypedDataStatement (TypedDataDeclaration _ dataName parameters constructors) ->
      [ (constructorKey, ConstructorContract binderId dataKey parameters fields)
      | dataKey <- maybeToList (definitionNameKey modulePath dataName),
        TypedConstructorDeclaration binderId constructorName fields _ <- constructors,
        constructorKey <- maybeToList (definitionNameKey modulePath constructorName)
      ]
    _ -> []

interfaceConstructorEntries :: ([Text], Maybe [Text], TypedModule) -> [(ResolvedNameKey, ConstructorContract)]
interfaceConstructorEntries (modulePath, selectedNames, TypedModule _ _ _ exports (TypedModuleInterface _ datas _ _) _ _ _) =
  [ (constructorKey, ConstructorContract binderId dataKey parameters (map (qualifyExternalType modulePath) fields))
  | TypedDataInterface (TypedDataDeclaration _ dataName parameters constructors) <- datas,
    dataKey <- maybeToList (definitionNameKey modulePath dataName),
    TypedConstructorDeclaration binderId constructorName fields _ <- constructors,
    importAllows selectedNames constructorName,
    moduleExportsName TypedConstructorNamespace constructorName exports,
    constructorKey <- maybeToList (definitionNameKey modulePath constructorName)
  ]

statementCapabilityEntries :: [Text] -> TypedStatement -> [(ResolvedNameKey, CapabilityContract)]
statementCapabilityEntries modulePath statement =
  case statement of
    TypedClassStatement declaration -> maybeToList (capabilityEntry modulePath declaration)
    _ -> []

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

evidenceCapabilityEntries ::
  Map ResolvedNameKey CapabilityContract ->
  Set ResolvedNameKey ->
  [(TypedBinderId, TypedScheme)] ->
  [(TypedEvidenceParameterRef, ResolvedNameKey)]
evidenceCapabilityEntries capabilityContracts eligibleCapabilities schemes =
  [ (TypedEvidenceParameterRef owner parameterId, capabilityKey)
  | (owner, TypedScheme _ _ parameters _ _ _ _) <- schemes,
    TypedEvidenceParameter parameterId (TypedCapabilityConstraint capability _ _) <- parameters,
    capabilityKey <- maybeToList (resolvedNameKey (binderModulePath owner) capability),
    Map.member capabilityKey capabilityContracts,
    Set.member capabilityKey eligibleCapabilities
  ]

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

capabilityEntry :: [Text] -> TypedClassDeclaration -> Maybe (ResolvedNameKey, CapabilityContract)
capabilityEntry modulePath (TypedClassDeclaration _ name parameters methods) = do
  key <- resolvedNameKey modulePath name
  pure
    ( key,
      CapabilityContract
        parameters
        ( Map.fromList
            [ (methodKey, scheme)
            | TypedMethodSignature methodName _ scheme <- methods,
              methodKey <- maybeToList (coreNameIdentifier methodName)
            ]
        )
    )

importAllows :: Maybe [Text] -> TypedCoreName -> Bool
importAllows Nothing _ = True
importAllows (Just selectedNames) name = maybe False (`elem` selectedNames) (coreNameIdentifier name)

moduleExportsName :: TypedNameNamespace -> TypedCoreName -> [TypedModuleExport] -> Bool
moduleExportsName namespace name exports =
  case coreNameIdentifier name of
    Nothing -> False
    Just identifier -> TypedModuleExport namespace identifier `elem` exports

withBlockDeclarations :: [TypedStatement] -> ModuleContext -> ModuleContext
withBlockDeclarations statements context =
  context
    { moduleContextSchemes = Map.union localSchemes (moduleContextSchemes context),
      moduleContextActiveSchemes = Map.union localActiveSchemes (moduleContextActiveSchemes context),
      moduleContextVisibleNames = Set.union localNames (moduleContextVisibleNames context),
      moduleContextVisibleImpls = Set.union localImpls (moduleContextVisibleImpls context),
      moduleContextImplMethods = Map.unionWith Set.union localImplMethods (moduleContextImplMethods context),
      moduleContextDataArities = Map.union localDataArities (moduleContextDataArities context),
      moduleContextDataContracts = Map.union localDataContracts (moduleContextDataContracts context),
      moduleContextConstructorContracts = Map.union localConstructors (moduleContextConstructorContracts context),
      moduleContextCapabilityContracts = combinedCapabilities,
      moduleContextEvidenceCapabilities =
        Map.union localEvidenceCapabilities (moduleContextEvidenceCapabilities context)
    }
  where
    modulePath = moduleContextPath context
    localSchemeEntries = concatMap statementSchemes statements
    localSchemes = Map.fromList localSchemeEntries
    localActiveSchemes =
      Map.fromList
        [ (key, scheme)
        | (owner, scheme) <- localSchemeEntries,
          key <- maybeToList (binderDefinitionKey owner)
        ]
    localNames = Set.fromList (concatMap (statementDefinedNameKeys modulePath) statements)
    localImplEntries = concatMap statementImplEntries statements
    localImpls = Set.fromList (map fst localImplEntries)
    localImplMethods = Map.fromListWith Set.union localImplEntries
    localDataArities = Map.fromList (concatMap (statementDataEntries modulePath) statements)
    localDataContracts = Map.fromList (concatMap (statementDataContractEntries modulePath) statements)
    localConstructors = Map.fromList (concatMap (statementConstructorEntries modulePath) statements)
    localCapabilities = Map.fromList (concatMap (statementCapabilityEntries modulePath) statements)
    combinedCapabilities =
      Map.union localCapabilities (moduleContextCapabilityContracts context)
    localEvidenceCapabilities =
      Map.fromList
        ( evidenceCapabilityEntries
            combinedCapabilities
            (Map.keysSet combinedCapabilities)
            localSchemeEntries
        )

validateStatementsInOrder :: Map Int [TypedStatement] -> ModuleContext -> [([Int], TypedStatement)] -> [TypedCoreValidationFailure]
validateStatementsInOrder recursiveGroups initialContext locatedStatements =
  validateStatementsInOrderWith
    (\_ _ _ -> Nothing)
    (forwardSignedFunctionDeclarations (map snd locatedStatements))
    recursiveGroups
    initialContext
    locatedStatements

validateBlockStatementsInOrder :: ModuleContext -> [([Int], TypedStatement)] -> [TypedCoreValidationFailure]
validateBlockStatementsInOrder initialContext locatedStatements =
  validateStatementsInOrderWith
    blockStatementScopeFailure
    []
    (recursiveGroupFacts (recursiveGroupDependencies initialContext statements) statements)
    initialContext
    locatedStatements
  where
    statements = map snd locatedStatements

validateStatementsInOrderWith :: (ModuleContext -> [Int] -> TypedStatement -> Maybe TypedCoreValidationFailure) -> [TypedStatement] -> Map Int [TypedStatement] -> ModuleContext -> [([Int], TypedStatement)] -> [TypedCoreValidationFailure]
validateStatementsInOrderWith rejectedStatement forwardSignedFunctions recursiveGroups initialContext locatedStatements =
  validateFrom initialContext 0 locatedStatements
  where
    forwardContext = prepareForwardSignedFunctionContext initialContext forwardSignedFunctions
    validateFrom _ _ [] = []
    validateFrom visibleContext blockIndex ((statementLocation, statement) : rest) =
      case rejectedStatement initialContext statementLocation statement of
        Just scopeFailure ->
          scopeFailure : validateFrom visibleContext (blockIndex + 1) rest
        Nothing ->
          let forwardVisibleContext =
                withForwardSignedFunctionDeclarations forwardContext visibleContext
              statementBaseContext
                | isForwardSignedFunctionDeclaration forwardSignedFunctions statement =
                    forwardVisibleContext
                | otherwise = visibleContext
              recursiveGroup = Map.findWithDefault [] blockIndex recursiveGroups
              statementContext =
                case statement of
                  TypedLetStatement {}
                    | null recursiveGroup -> statementBaseContext
                    | otherwise -> withBlockDeclarations recursiveGroup statementBaseContext
                  _ -> withBlockDeclarations [statement] statementBaseContext
              nextContext = withBlockDeclarations [statement] visibleContext
              nextStatement =
                case rest of
                  (_, candidate) : _ -> Just candidate
                  [] -> Nothing
              statementFailures =
                validateStatement statementContext statementLocation statement
              attachedSignatureFailures =
                validateAttachedSignature
                  initialContext
                  statementLocation
                  statement
                  nextStatement
                  (null statementFailures)
           in attachedSignatureFailures
                <> statementFailures
                <> validateFrom nextContext (blockIndex + 1) rest

prepareForwardSignedFunctionContext :: ModuleContext -> [TypedStatement] -> ForwardSignedFunctionContext
prepareForwardSignedFunctionContext context declarations =
  ForwardSignedFunctionContext
    { forwardFunctionSchemes = Map.fromList schemeEntries,
      forwardFunctionActiveSchemes =
        Map.fromList
          [ (key, scheme)
          | (owner, scheme) <- schemeEntries,
            key <- maybeToList (binderDefinitionKey owner)
          ],
      forwardFunctionVisibleNames =
        Set.fromList (concatMap (statementDefinedNameKeys (moduleContextPath context)) declarations)
    }
  where
    schemeEntries = concatMap statementSchemes declarations

withForwardSignedFunctionDeclarations :: ForwardSignedFunctionContext -> ModuleContext -> ModuleContext
withForwardSignedFunctionDeclarations forwardContext context =
  context
    { moduleContextSchemes =
        Map.union (forwardFunctionSchemes forwardContext) (moduleContextSchemes context),
      moduleContextActiveSchemes =
        Map.union (moduleContextActiveSchemes context) (forwardFunctionActiveSchemes forwardContext),
      moduleContextVisibleNames =
        Set.union (forwardFunctionVisibleNames forwardContext) (moduleContextVisibleNames context)
    }

forwardSignedFunctionDeclarations :: [TypedStatement] -> [TypedStatement]
forwardSignedFunctionDeclarations statements =
  case statements of
    TypedSignatureStatement _ signatureName _ signatureScheme
      : binding@(TypedLetStatement _ bindingName _ bindingScheme expression)
      : rest
        | signatureName == bindingName,
          isNothing (signatureBindingSchemeMismatch signatureScheme bindingScheme),
          concreteMonomorphicFunctionScheme bindingScheme,
          leadingTypedLambda expression ->
            binding : forwardSignedFunctionDeclarations rest
        | otherwise ->
            forwardSignedFunctionDeclarations (binding : rest)
    _ : rest -> forwardSignedFunctionDeclarations rest
    [] -> []

isForwardSignedFunctionDeclaration :: [TypedStatement] -> TypedStatement -> Bool
isForwardSignedFunctionDeclaration declarations statement =
  case statement of
    TypedLetStatement binderId _ _ _ _ -> any (hasBinder binderId) declarations
    _ -> False
  where
    hasBinder binderId declaration =
      case declaration of
        TypedLetStatement declarationBinderId _ _ _ _ -> declarationBinderId == binderId
        _ -> False

concreteMonomorphicFunctionScheme :: TypedScheme -> Bool
concreteMonomorphicFunctionScheme (TypedScheme _ parameters evidence primitive typeValue recipe callableShape) =
  null parameters
    && null evidence
    && null primitive
    && concreteTypedType typeValue
    && concreteTypedRecipe recipe
    && callableShape /= Nothing
    && case (typeValue, recipe) of
      (TypedFunctionType {}, TypedClosureRecipe {}) -> True
      _ -> False

concreteTypedType :: TypedType -> Bool
concreteTypedType typeValue =
  case typeValue of
    TypedListType elementType -> concreteTypedType elementType
    TypedTupleType elementTypes -> all concreteTypedType elementTypes
    TypedDataType _ arguments -> all concreteTypedType arguments
    TypedFunctionType argumentType resultType ->
      concreteTypedType argumentType && concreteTypedType resultType
    TypedTypeParameterType {} -> False
    _ -> True

concreteTypedRecipe :: TypedRepresentationRecipe -> Bool
concreteTypedRecipe recipe =
  case recipe of
    TypedManagedListRecipe elementRecipe -> concreteTypedRecipe elementRecipe
    TypedManagedProductRecipe elementRecipes -> all concreteTypedRecipe elementRecipes
    TypedClosureRecipe argumentRecipes resultRecipe ->
      all concreteTypedRecipe argumentRecipes && concreteTypedRecipe resultRecipe
    TypedRepresentationParameterRecipe {} -> False
    _ -> True

leadingTypedLambda :: TypedExpr -> Bool
leadingTypedLambda expression =
  case expression of
    TypedLambdaExpr {} -> True
    _ -> False

blockStatementScopeFailure :: ModuleContext -> [Int] -> TypedStatement -> Maybe TypedCoreValidationFailure
blockStatementScopeFailure context statementLocation statement =
  case statement of
    TypedDataStatement {} -> scopeFailure "data declaration"
    TypedClassStatement {} -> scopeFailure "class declaration"
    TypedImplStatement {} -> scopeFailure "impl declaration"
    _ -> Nothing
  where
    scopeFailure declarationKind =
      Just
        ( failure
            (TypedStatementPath (moduleContextPath context) statementLocation)
            TypedBlockResultMismatch
            (TypedTextDetail declarationKind)
        )

rootRecursiveGroupFailures :: [Text] -> [TypedStatement] -> [TypedRecursiveGroup] -> [TypedCoreValidationFailure]
rootRecursiveGroupFailures modulePath statements declaredGroups
  | not (null basicFailures) = basicFailures
  | not (null orderingFailures) = orderingFailures
  | callableBinderIdentityAmbiguous = []
  | otherwise = maybeToList reachabilityFailure
  where
    callableDeclarations = rootCallableDeclarations statements
    callableByBinder =
      Map.fromListWith
        (\_ existing -> existing)
        [(binderId, (statementIndex, statement)) | (statementIndex, binderId, statement, _) <- callableDeclarations]
    callableStatementIndex binderId = fst <$> Map.lookup binderId callableByBinder
    memberStatementIndices = traverse callableStatementIndex
    callableBinderIdentityAmbiguous =
      snd (foldl' collectCallableBinder (Set.empty, False) callableDeclarations)
    collectCallableBinder (seen, ambiguous) (_, binderId, _, _)
      | Set.member binderId seen = (seen, True)
      | otherwise = (Set.insert binderId seen, ambiguous)
    (_, basicFailures) =
      foldl' validateBasicGroup (Set.empty, []) (zip [0 :: Int ..] declaredGroups)
    validateBasicGroup (seen, failures) (groupIndex, TypedRecursiveGroup members)
      | null members =
          ( seen,
            failures
              <> [failure (TypedModulePath modulePath) TypedRecursiveGroupMismatch (TypedIndexDetail groupIndex)]
          )
      | otherwise = foldl' validateBasicMember (seen, failures) members
    validateBasicMember (seen, failures) binderId =
      case Map.lookup binderId callableByBinder of
        Nothing ->
          ( seen,
            failures
              <> [failure (TypedModulePath modulePath) TypedUnknownBinder (TypedBinderDetail binderId)]
          )
        Just (statementIndex, _)
          | Set.member binderId seen ->
              ( seen,
                failures
                  <> [ failure
                         (TypedStatementPath modulePath [statementIndex])
                         TypedDuplicateBinder
                         (TypedBinderDetail binderId)
                     ]
              )
          | otherwise -> (Set.insert binderId seen, failures)
    orderingFailures = memberOrderingFailures <> groupOrderingFailures
    memberOrderingFailures =
      [ failure (TypedModulePath modulePath) TypedRecursiveGroupMismatch (TypedIndexDetail groupIndex)
      | (groupIndex, TypedRecursiveGroup members) <- zip [0 :: Int ..] declaredGroups,
        Just memberIndices <- [memberStatementIndices members],
        memberIndices /= sort memberIndices
      ]
    groupOrderingFailures = snd (foldl' validateGroupOrder (Nothing, []) indexedFirstMembers)
    indexedFirstMembers =
      mapMaybe
        ( \(groupIndex, TypedRecursiveGroup members) ->
            (\indices -> (groupIndex, minimum indices)) <$> nonEmptyMemberIndices members
        )
        (zip [0 :: Int ..] declaredGroups)
    nonEmptyMemberIndices members = do
      indices <- memberStatementIndices members
      case indices of
        [] -> Nothing
        _ -> Just indices
    validateGroupOrder (previousIndex, failures) (groupIndex, statementIndex) =
      case previousIndex of
        Just previous
          | statementIndex <= previous ->
              ( Just statementIndex,
                failures
                  <> [failure (TypedModulePath modulePath) TypedRecursiveGroupMismatch (TypedIndexDetail groupIndex)]
              )
        _ -> (Just statementIndex, failures)
    declaredBinderGroups = [members | TypedRecursiveGroup members <- declaredGroups]
    actualBinderGroups = rootCyclicBinderGroups callableDeclarations
    reachabilityFailure = firstGroupMismatch declaredBinderGroups actualBinderGroups
    firstGroupMismatch declared actual =
      case (declared, actual) of
        ([], []) -> Nothing
        (declaredGroup@(declaredMember : _) : restDeclared, actualGroup : restActual)
          | declaredGroup == actualGroup -> firstGroupMismatch restDeclared restActual
          | otherwise -> mismatchFor declaredMember
        ((declaredMember : _) : _, []) -> mismatchFor declaredMember
        ([], (actualMember : _) : _) -> mismatchFor actualMember
        _ -> Nothing
    mismatchFor binderId =
      case Map.lookup binderId callableByBinder of
        Just (statementIndex, _) ->
          Just
            ( failure
                (TypedStatementPath modulePath [statementIndex])
                TypedRecursiveGroupMismatch
                (TypedBinderDetail binderId)
            )
        Nothing -> Nothing

rootRecursiveGroupsByStatement :: [TypedStatement] -> [TypedRecursiveGroup] -> Map Int [TypedStatement]
rootRecursiveGroupsByStatement statements =
  foldl' addGroup Map.empty
  where
    callableDeclarations = rootCallableDeclarations statements
    addGroup groups (TypedRecursiveGroup members) =
      let memberSet = Set.fromList members
          declarations =
            [ statement
            | (_, binderId, statement, _) <- callableDeclarations,
              Set.member binderId memberSet
            ]
          memberIndices =
            [ statementIndex
            | (statementIndex, binderId, _, _) <- callableDeclarations,
              Set.member binderId memberSet
            ]
       in foldl'
            (\result statementIndex -> Map.insertWith (\_ existing -> existing) statementIndex declarations result)
            groups
            memberIndices

rootCallableBinderDependencies :: Set TypedBinderId -> TypedExpr -> Set TypedBinderId
rootCallableBinderDependencies callableBinders expression =
  case expression of
    TypedLiteralExpr {} -> Set.empty
    TypedVariableExpr _ _ maybeBinder ->
      case maybeBinder of
        Just binderId
          | Set.member binderId callableBinders -> Set.singleton binderId
        _ -> Set.empty
    TypedLambdaExpr _ _ _ body -> dependencies body
    TypedOperatorValueExpr {} -> Set.empty
    TypedListExpr _ elements -> Set.unions (map dependencies elements)
    TypedTupleExpr _ elements -> Set.unions (map dependencies elements)
    TypedApplyExpr _ function argument -> Set.union (dependencies function) (dependencies argument)
    TypedTypeApplicationExpr _ function _ _ -> dependencies function
    TypedIfExpr _ condition thenExpression elseExpression ->
      Set.unions (map dependencies [condition, thenExpression, elseExpression])
    TypedPatternCaseExpr _ scrutinee arms ->
      Set.unions (dependencies scrutinee : map caseArmDependencies arms)
    TypedBinaryExpr _ _ left right -> Set.union (dependencies left) (dependencies right)
    TypedLeftSectionExpr _ left _ -> dependencies left
    TypedRightSectionExpr _ _ right -> dependencies right
    TypedBlockExpr _ blockStatements -> Set.unions (map statementDependencies blockStatements)
  where
    dependencies = rootCallableBinderDependencies callableBinders
    caseArmDependencies (TypedCaseArm _ maybeGuard result) =
      Set.union (maybe Set.empty dependencies maybeGuard) (dependencies result)
    statementDependencies statement =
      case statement of
        TypedLetStatement _ _ _ _ value -> dependencies value
        TypedExpressionStatement _ value -> dependencies value
        TypedImplStatement (TypedImplDeclaration _ _ methods) ->
          Set.unions [dependencies body | TypedMethodDefinition _ _ _ _ body <- methods]
        _ -> Set.empty

rootCallableDeclarations :: [TypedStatement] -> [(Int, TypedBinderId, TypedStatement, TypedExpr)]
rootCallableDeclarations statements =
  [ (statementIndex, binderId, statement, expression)
  | (statementIndex, statement@(TypedLetStatement binderId _ _ scheme expression)) <- zip [0 :: Int ..] statements,
    typedSchemeIsCallable scheme
  ]
  where
    typedSchemeIsCallable (TypedScheme _ _ _ _ _ _ callableShape) =
      case callableShape of
        Just _ -> True
        Nothing -> False

rootCyclicBinderGroups :: [(Int, TypedBinderId, TypedStatement, TypedExpr)] -> [[TypedBinderId]]
rootCyclicBinderGroups declarations = collectGroups Set.empty sourceBinders
  where
    sourceBinders = [binderId | (_, binderId, _, _) <- declarations]
    callableBinders = Set.fromList sourceBinders
    directDependencies =
      Map.fromList
        [ (binderId, rootCallableBinderDependencies callableBinders expression)
        | (_, binderId, _, expression) <- declarations
        ]
    graphNodes =
      [ (binderId, binderId, Set.toList (Map.findWithDefault Set.empty binderId directDependencies))
      | binderId <- sourceBinders
      ]
    cyclicComponents =
      [ (componentIndex, Set.fromList members)
      | (componentIndex, component) <- zip [0 :: Int ..] (stronglyConnComp graphNodes),
        members <- maybeToList (cyclicMembers component)
      ]
    cyclicMembers component =
      case component of
        AcyclicSCC binderId
          | Set.member binderId (Map.findWithDefault Set.empty binderId directDependencies) -> Just [binderId]
        CyclicSCC members -> Just members
        _ -> Nothing
    componentByBinder =
      Map.fromList
        [ (binderId, componentIndex)
        | (componentIndex, members) <- cyclicComponents,
          binderId <- Set.toList members
        ]
    collectGroups _ [] = []
    collectGroups seenComponents (binderId : remainingBinders) =
      case Map.lookup binderId componentByBinder of
        Nothing -> collectGroups seenComponents remainingBinders
        Just componentIndex
          | Set.member componentIndex seenComponents -> collectGroups seenComponents remainingBinders
          | otherwise ->
              [ candidate
              | candidate <- sourceBinders,
                Map.lookup candidate componentByBinder == Just componentIndex
              ]
                : collectGroups (Set.insert componentIndex seenComponents) remainingBinders

recursiveGroupFacts :: Map Int (Set Int) -> [TypedStatement] -> Map Int [TypedStatement]
recursiveGroupFacts dependencies statements =
  Map.mapMaybe
    (\componentIndex -> Map.lookup componentIndex sourceOrderedGroups)
    memberComponents
  where
    graphNodes =
      [ (statementIndex, statementIndex, Set.toList directDependencies)
      | (statementIndex, directDependencies) <- Map.toList dependencies
      ]
    cyclicComponents =
      [ (componentIndex, members)
      | (componentIndex, component) <- zip [0 :: Int ..] (stronglyConnComp graphNodes),
        members <- maybeToList (cyclicComponentMembers component)
      ]
    cyclicComponentMembers component =
      case component of
        AcyclicSCC statementIndex
          | selfDependent statementIndex -> Just [statementIndex]
        CyclicSCC members
          | length members > 1 || any selfDependent members -> Just members
        _ -> Nothing
    selfDependent statementIndex =
      Set.member
        statementIndex
        (Map.findWithDefault Set.empty statementIndex dependencies)
    memberComponents =
      Map.fromList
        [ (statementIndex, componentIndex)
        | (componentIndex, members) <- cyclicComponents,
          statementIndex <- members
        ]
    reversedGroups =
      foldl'
        addSourceStatement
        Map.empty
        (zip [0 :: Int ..] statements)
    addSourceStatement groups (statementIndex, statement) =
      case Map.lookup statementIndex memberComponents of
        Nothing -> groups
        Just componentIndex ->
          Map.insertWith (<>) componentIndex [statement] groups
    sourceOrderedGroups = Map.map reverse reversedGroups

recursiveGroupDependencies :: ModuleContext -> [TypedStatement] -> Map Int (Set Int)
recursiveGroupDependencies outerContext statements =
  Map.fromList
    [ ( index,
        Set.fromList
          [ dependency
          | referencedName <- Set.toList (freeExpressionValueNames outerContext Set.empty expression),
            dependency <- maybeToList (resolveDependency index nameKey expression referencedName)
          ]
      )
    | (index, nameKey, expression) <- declarations
    ]
  where
    declarations =
      [ (index, nameKey, expression)
      | (index, TypedLetStatement _ name _ _ expression) <- zip [0 ..] statements,
        nameKey <- maybeToList (resolvedNameKey (moduleContextPath outerContext) name)
      ]
    declarationIndicesByName =
      Map.fromListWith
        Set.union
        [ (nameKey, Set.singleton index)
        | (index, nameKey, _) <- declarations
        ]
    resolveDependency index ownName expression referencedName =
      case Map.lookup referencedName declarationIndicesByName of
        Nothing -> Nothing
        Just declarationIndices ->
          case Set.lookupLT index declarationIndices of
            Just prior -> Just prior
            Nothing
              | Set.member referencedName (moduleContextVisibleNames outerContext) -> Nothing
              | referencedName == ownName ->
                  if expressionCanBeRecursive outerContext ownName expression then Just index else Nothing
              | otherwise ->
                  Set.lookupGT index declarationIndices

expressionCanBeRecursive :: ModuleContext -> ResolvedNameKey -> TypedExpr -> Bool
expressionCanBeRecursive context bindingName expression =
  expressionHasFunctionContract expression
    || selfAliasLikeReference context bindingName expression
  where
    expressionHasFunctionContract candidate =
      case typedNodeType (typedExpressionInfo candidate) of
        TypedFunctionType {} -> True
        _ -> False

selfAliasLikeReference :: ModuleContext -> ResolvedNameKey -> TypedExpr -> Bool
selfAliasLikeReference context bindingName expression =
  case aliasSummary Set.empty Map.empty Set.empty expression of
    (hasAliasPath, hasNonAliasPath) -> hasAliasPath && not hasNonAliasPath
  where
    noSummary = (False, False)

    combineSummaries (leftAliasPath, leftNonAliasPath) (rightAliasPath, rightNonAliasPath) =
      ( leftAliasPath || rightAliasPath,
        leftNonAliasPath || rightNonAliasPath
      )

    combineAll = foldl' combineSummaries noSummary

    nameKey name =
      resolvedNameKey (moduleContextPath context) name

    boundPatternNames patternValue =
      Set.fromList
        [ key
        | BinderContract _ name _ _ <- patternBoundContracts patternValue,
          key <- maybeToList (nameKey name)
        ]

    variableSummary summarizeTarget boundNames scopeBindings visitedBindings name =
      case nameKey name of
        Nothing -> noSummary
        Just key
          | Set.member key boundNames -> noSummary
          | Just bindingExpression <- Map.lookup key scopeBindings,
            Set.notMember key visitedBindings ->
              summarizeTarget
                boundNames
                scopeBindings
                (Set.insert key visitedBindings)
                bindingExpression
          | key == bindingName -> (False, True)
          | otherwise -> noSummary

    aliasVariableSummary boundNames scopeBindings visitedBindings name =
      case nameKey name of
        Nothing -> noSummary
        Just key
          | Set.member key boundNames -> noSummary
          | Just bindingExpression <- Map.lookup key scopeBindings,
            Set.notMember key visitedBindings ->
              aliasSummary
                boundNames
                scopeBindings
                (Set.insert key visitedBindings)
                bindingExpression
          | key == bindingName -> (True, False)
          | otherwise -> noSummary

    aliasOperatorSummary boundNames operator =
      case operator of
        TypedBuiltinOperator _ -> noSummary
        TypedResolvedOperator name _
          | maybe False (`Set.notMember` boundNames) (nameKey name),
            nameKey name == Just bindingName ->
              (True, False)
        _ -> noSummary

    localScopeBindings statements =
      Map.fromList
        [ (key, bindingExpression)
        | TypedLetStatement _ name _ _ bindingExpression <- statements,
          key <- maybeToList (nameKey name)
        ]

    blockStatementNonAliasSummary boundNames scopeBindings statement =
      case statement of
        TypedLetStatement _ _ _ _ bindingExpression ->
          nonAliasSummary boundNames scopeBindings Set.empty bindingExpression
        TypedExpressionStatement _ statementExpression ->
          nonAliasSummary boundNames scopeBindings Set.empty statementExpression
        _ -> noSummary

    caseArmAliasSummary boundNames scopeBindings visitedBindings (TypedCaseArm patternValue maybeGuard result) =
      let armBoundNames = Set.union boundNames (boundPatternNames patternValue)
       in combineSummaries
            (maybe noSummary (nonAliasSummary armBoundNames scopeBindings visitedBindings) maybeGuard)
            (aliasSummary armBoundNames scopeBindings visitedBindings result)

    caseArmNonAliasSummary boundNames scopeBindings visitedBindings (TypedCaseArm patternValue maybeGuard result) =
      let armBoundNames = Set.union boundNames (boundPatternNames patternValue)
       in combineSummaries
            (maybe noSummary (nonAliasSummary armBoundNames scopeBindings visitedBindings) maybeGuard)
            (nonAliasSummary armBoundNames scopeBindings visitedBindings result)

    aliasSummary boundNames scopeBindings visitedBindings candidate =
      case candidate of
        TypedVariableExpr _ name _ ->
          aliasVariableSummary boundNames scopeBindings visitedBindings name
        TypedOperatorValueExpr _ operator ->
          aliasOperatorSummary boundNames operator
        TypedTypeApplicationExpr _ function _ _ ->
          aliasSummary boundNames scopeBindings visitedBindings function
        TypedIfExpr _ condition thenExpression elseExpression ->
          combineAll
            [ nonAliasSummary boundNames scopeBindings visitedBindings condition,
              aliasSummary boundNames scopeBindings visitedBindings thenExpression,
              aliasSummary boundNames scopeBindings visitedBindings elseExpression
            ]
        TypedPatternCaseExpr _ scrutinee arms ->
          combineAll
            ( nonAliasSummary boundNames scopeBindings visitedBindings scrutinee
                : map (caseArmAliasSummary boundNames scopeBindings visitedBindings) arms
            )
        TypedBlockExpr _ statements ->
          let blockScopeBindings =
                Map.union (localScopeBindings statements) scopeBindings
              (eagerStatements, terminalSummary) =
                case reverse statements of
                  TypedExpressionStatement _ terminalExpression : reversedLeadingStatements ->
                    ( reverse reversedLeadingStatements,
                      aliasSummary boundNames blockScopeBindings visitedBindings terminalExpression
                    )
                  _ -> (statements, noSummary)
              eagerSummary =
                combineAll
                  (map (blockStatementNonAliasSummary boundNames blockScopeBindings) eagerStatements)
           in combineSummaries terminalSummary eagerSummary
        _ -> nonAliasSummary boundNames scopeBindings visitedBindings candidate

    nonAliasSummary boundNames scopeBindings visitedBindings candidate =
      case candidate of
        TypedLiteralExpr {} -> noSummary
        TypedVariableExpr _ name _ ->
          variableSummary nonAliasSummary boundNames scopeBindings visitedBindings name
        TypedLambdaExpr {} -> noSummary
        TypedOperatorValueExpr {} -> noSummary
        TypedListExpr _ elements ->
          combineAll (map (nonAliasSummary boundNames scopeBindings visitedBindings) elements)
        TypedTupleExpr _ elements ->
          combineAll (map (nonAliasSummary boundNames scopeBindings visitedBindings) elements)
        TypedApplyExpr _ function argument ->
          combineAll
            [ nonAliasSummary boundNames scopeBindings visitedBindings function,
              nonAliasSummary boundNames scopeBindings visitedBindings argument
            ]
        TypedTypeApplicationExpr _ function _ _ ->
          nonAliasSummary boundNames scopeBindings visitedBindings function
        TypedIfExpr _ condition thenExpression elseExpression ->
          combineAll
            [ nonAliasSummary boundNames scopeBindings visitedBindings condition,
              nonAliasSummary boundNames scopeBindings visitedBindings thenExpression,
              nonAliasSummary boundNames scopeBindings visitedBindings elseExpression
            ]
        TypedPatternCaseExpr _ scrutinee arms ->
          combineAll
            ( nonAliasSummary boundNames scopeBindings visitedBindings scrutinee
                : map (caseArmNonAliasSummary boundNames scopeBindings visitedBindings) arms
            )
        TypedBinaryExpr _ _ left right ->
          combineAll
            [ nonAliasSummary boundNames scopeBindings visitedBindings left,
              nonAliasSummary boundNames scopeBindings visitedBindings right
            ]
        TypedLeftSectionExpr _ left _ ->
          nonAliasSummary boundNames scopeBindings visitedBindings left
        TypedRightSectionExpr _ _ right ->
          nonAliasSummary boundNames scopeBindings visitedBindings right
        TypedBlockExpr _ statements ->
          let blockScopeBindings =
                Map.union (localScopeBindings statements) scopeBindings
           in combineAll
                (map (blockStatementNonAliasSummary boundNames blockScopeBindings) statements)

freeExpressionValueNames :: ModuleContext -> Set ResolvedNameKey -> TypedExpr -> Set ResolvedNameKey
freeExpressionValueNames context boundNames expression =
  case expression of
    TypedVariableExpr _ name _ -> freeName name
    TypedLambdaExpr _ _ name body ->
      freeExpressionValueNames context (Set.union boundNames (nameKeys [name])) body
    TypedOperatorValueExpr _ operator -> freeOperator operator
    TypedPatternCaseExpr _ scrutinee arms ->
      freeExpressionValueNames context boundNames scrutinee
        <> Set.unions
          [ let armBoundNames =
                  Set.union
                    boundNames
                    (nameKeys [name | BinderContract _ name _ _ <- patternBoundContracts patternValue])
             in Set.unions
                  ( map
                      (freeExpressionValueNames context armBoundNames)
                      (maybeToList maybeGuard <> [result])
                  )
          | TypedCaseArm patternValue maybeGuard result <- arms
          ]
    TypedBinaryExpr _ operator left right ->
      freeOperator operator
        <> freeExpressionValueNames context boundNames left
        <> freeExpressionValueNames context boundNames right
    TypedLeftSectionExpr _ left operator ->
      freeExpressionValueNames context boundNames left <> freeOperator operator
    TypedRightSectionExpr _ operator right ->
      freeOperator operator <> freeExpressionValueNames context boundNames right
    TypedBlockExpr _ statements ->
      let localNames =
            nameKeys
              [ name
              | statement <- statements,
                name <- statementDefinedNames statement
              ]
          nestedBoundNames = Set.union boundNames localNames
       in Set.unions
            [ freeExpressionValueNames context nestedBoundNames nestedExpression
            | statement <- statements,
              nestedExpression <- statementExpressions statement
            ]
    _ ->
      Set.unions
        [ freeExpressionValueNames context boundNames child
        | child <- expressionChildren expression
        ]
  where
    nameKeys names =
      Set.fromList
        [ key
        | name <- names,
          key <- maybeToList (resolvedNameKey (moduleContextPath context) name)
        ]
    freeName name =
      case resolvedNameKey (moduleContextPath context) name of
        Just key@(ResolvedNameKey _ TypedValueNamespace _)
          | Set.notMember key boundNames -> Set.singleton key
        Just key@(GeneratedNameKey _)
          | Set.notMember key boundNames -> Set.singleton key
        _ -> Set.empty
    freeOperator operator =
      case operator of
        TypedBuiltinOperator _ -> Set.empty
        TypedResolvedOperator name _ -> freeName name

statementExpressions :: TypedStatement -> [TypedExpr]
statementExpressions statement =
  case statement of
    TypedLetStatement _ _ _ _ expression -> [expression]
    TypedImplStatement (TypedImplDeclaration _ _ methods) ->
      [expression | TypedMethodDefinition _ _ _ _ expression <- methods]
    TypedExpressionStatement _ expression -> [expression]
    _ -> []

withSchemeScope :: TypedScheme -> ModuleContext -> ModuleContext
withSchemeScope (TypedScheme _ typeParameters _ primitiveConstraints _ _ _) context =
  context
    { moduleContextTypeScope = Set.union (Set.fromList typeParameters) (moduleContextTypeScope context),
      moduleContextPrimitiveConstraints = primitiveConstraints <> moduleContextPrimitiveConstraints context
    }

nestedStatementLocation :: [Int] -> [Int] -> Int -> [Int]
nestedStatementLocation statementLocation expressionPath blockIndex =
  statementLocation <> expressionPath <> [blockIndex]

validateStatement :: ModuleContext -> [Int] -> TypedStatement -> [TypedCoreValidationFailure]
validateStatement context statementLocation statement =
  case statement of
    TypedLetStatement binderId name spanValue scheme expression ->
      validateSpan statementPath spanValue
        <> validateLocalDefinitionName context [TypedValueNamespace] statementPath name
        <> validateBinderDefinition context statementPath binderId name
        <> validateInferredScheme context statementPath binderId scheme
        <> validateBindingValue statementPath scheme (typedExpressionInfo expression)
        <> validateCallableBindingShape statementPath scheme expression
        <> validateNamedExpression
          (withSchemeScope scheme context)
          statementLocation
          [0]
          (schemeRequiresStagedLeadingLambdaRecipe scheme)
          expression
    TypedSignatureStatement binderId name spanValue scheme ->
      validateSpan statementPath spanValue
        <> validateLocalDefinitionName context [TypedValueNamespace] statementPath name
        <> validateBinderDefinition context statementPath binderId name
        <> validateScheme context statementPath binderId scheme
        <> validateSourceSchemeDataTypes context statementPath scheme
    TypedDataStatement declaration -> validateDataDeclaration context statementPath declaration
    TypedClassStatement declaration -> validateClassDeclaration context statementPath declaration
    TypedImplStatement declaration -> validateImplDeclaration context statementLocation statementPath declaration
    TypedExpressionStatement spanValue expression ->
      validateSpan statementPath spanValue
        <> validateExpression context statementLocation [0] expression
  where
    statementPath = TypedStatementPath (moduleContextPath context) statementLocation

validateAttachedSignature :: ModuleContext -> [Int] -> TypedStatement -> Maybe TypedStatement -> Bool -> [TypedCoreValidationFailure]
validateAttachedSignature context statementLocation statement maybeNextStatement statementIsValid =
  case (statement, maybeNextStatement) of
    ( TypedSignatureStatement _ signatureName _ signatureScheme,
      Just (TypedLetStatement _ bindingName _ bindingScheme _)
      )
        | signatureName == bindingName ->
            validateSignatureBindingScheme
              (TypedStatementPath (moduleContextPath context) statementLocation)
              signatureScheme
              bindingScheme
    (TypedSignatureStatement _ signatureName _ _, _)
      | statementIsValid ->
          [ failure
              (TypedStatementPath (moduleContextPath context) statementLocation)
              TypedBindingValueMismatch
              (TypedNameDetail signatureName)
          ]
      | otherwise -> []
    _ -> []

validateSignatureBindingScheme :: TypedCoreValidationPath -> TypedScheme -> TypedScheme -> [TypedCoreValidationFailure]
validateSignatureBindingScheme path signatureScheme bindingScheme =
  case signatureBindingSchemeMismatch signatureScheme bindingScheme of
    Nothing -> []
    Just (kind, detail) -> [failure path kind detail]

signatureBindingSchemeMismatch :: TypedScheme -> TypedScheme -> Maybe (TypedCoreValidationKind, TypedCoreValidationDetail)
signatureBindingSchemeMismatch
  (TypedScheme _ signatureParameters signatureEvidence signaturePrimitive signatureType signatureRecipe signatureShape)
  (TypedScheme bindingOwner bindingParameters bindingEvidence bindingPrimitive bindingType bindingRecipe bindingShape)
    | signatureParameters /= bindingParameters =
        bindingMismatch (TypedArityDetail (length signatureParameters) (length bindingParameters))
    | signatureEvidence /= bindingEvidence = bindingMismatch TypedNoValidationDetail
    | signaturePrimitive /= bindingPrimitive = bindingMismatch TypedNoValidationDetail
    | signatureType /= bindingType = bindingMismatch (TypedTypeDetail signatureType bindingType)
    | signatureRecipe /= bindingRecipe = bindingMismatch (TypedRecipeDetail signatureRecipe bindingRecipe)
    | signatureShape /= bindingShape =
        Just (TypedCallableShapeMismatch, TypedBinderDetail bindingOwner)
    | otherwise = Nothing
    where
      bindingMismatch detail = Just (TypedBindingValueMismatch, detail)

validateBindingValue :: TypedCoreValidationPath -> TypedScheme -> TypedNodeInfo -> [TypedCoreValidationFailure]
validateBindingValue path (TypedScheme _ _ _ _ resultType resultRecipe _) info =
  valueFailures resultType resultRecipe
  where
    valueFailures expectedType expectedRecipeValue
      | typedNodeType info /= expectedType =
          [failure path TypedBindingValueMismatch (TypedTypeDetail expectedType (typedNodeType info))]
      | typedNodeRecipe info /= expectedRecipeValue =
          [failure path TypedBindingValueMismatch (TypedRecipeDetail expectedRecipeValue (typedNodeRecipe info))]
      | otherwise = []

validateCallableBindingShape :: TypedCoreValidationPath -> TypedScheme -> TypedExpr -> [TypedCoreValidationFailure]
validateCallableBindingShape path (TypedScheme owner _ _ _ _ recipe callableShape) expression
  | callableShape == Just TypedDirectCallableShape,
    directCallableRecipeArity recipe /= Just (leadingLambdaCount expression) =
      [failure path TypedCallableShapeMismatch (TypedBinderDetail owner)]
  | otherwise = []

leadingLambdaCount :: TypedExpr -> Int
leadingLambdaCount expression =
  case expression of
    TypedLambdaExpr _ _ _ body -> 1 + leadingLambdaCount body
    _ -> 0

validateScheme :: ModuleContext -> TypedCoreValidationPath -> TypedBinderId -> TypedScheme -> [TypedCoreValidationFailure]
validateScheme context path owner = validateSchemeWithOuterScope context path owner (moduleContextTypeScope context)

validateInferredScheme :: ModuleContext -> TypedCoreValidationPath -> TypedBinderId -> TypedScheme -> [TypedCoreValidationFailure]
validateInferredScheme context path owner =
  validateSchemeWithOuterScopeUsing
    validateRetainedCapabilityConstraint
    context
    path
    owner
    (moduleContextTypeScope context)

validateSchemeWithOuterScope :: ModuleContext -> TypedCoreValidationPath -> TypedBinderId -> Set TypedTypeParameterId -> TypedScheme -> [TypedCoreValidationFailure]
validateSchemeWithOuterScope = validateSchemeWithOuterScopeUsing validateCapabilityConstraint

validateSchemeWithOuterScopeUsing ::
  (ModuleContext -> TypedCoreValidationPath -> Set TypedTypeParameterId -> TypedCapabilityConstraint -> [TypedCoreValidationFailure]) ->
  ModuleContext ->
  TypedCoreValidationPath ->
  TypedBinderId ->
  Set TypedTypeParameterId ->
  TypedScheme ->
  [TypedCoreValidationFailure]
validateSchemeWithOuterScopeUsing validateConstraint context path owner outerScope (TypedScheme schemeOwner typeParameters evidenceParameters primitiveConstraints resultType resultRecipe callableShape) =
  ownerFailures
    <> parameterShadowingFailures
    <> parameterOrderFailures
    <> validateOrderedEvidenceParameters path evidenceParameters
    <> concatMap validateEvidenceParameter evidenceParameters
    <> concatMap (validatePrimitiveConstraint context path parameterScope) primitiveConstraints
    <> validateType path parameterScope resultType
    <> concatMap (validateDataTypeApplications context path) (resultType : evidenceTypes <> primitiveTypes)
    <> validateRecipe path parameterScope resultRecipe
    <> validateTypeRecipe path parameterScope resultType resultRecipe
    <> validateCallableShape path schemeOwner resultType resultRecipe callableShape
  where
    ownerFailures
      | owner == schemeOwner = []
      | otherwise = [failure path TypedUnknownBinder (TypedBinderDetail schemeOwner)]
    parameterShadowingFailures =
      [ failure path TypedDuplicateTypeParameter (TypedTypeParameterDetail parameter)
      | parameter <- typeParameters,
        Set.member parameter outerScope
      ]
    parameterOrderFailures
      | null parameterShadowingFailures =
          validateOrderedTypeParametersFrom path (nextTypeParameterOrdinal outerScope) typeParameters
      | otherwise = []
    parameterScope = Set.union outerScope (Set.fromList typeParameters)
    validateEvidenceParameter (TypedEvidenceParameter _ constraint) =
      validateConstraint context path parameterScope constraint
    evidenceTypes = [targetType | TypedEvidenceParameter _ (TypedCapabilityConstraint _ _ targetType) <- evidenceParameters]
    primitiveTypes =
      [ typeValue
      | constraint <- primitiveConstraints,
        typeValue <- case constraint of
          TypedNumericPrimitiveConstraint _ value -> [value]
          TypedStrictEqualityPrimitiveConstraint value -> [value]
      ]

validateOrderedEvidenceParameters :: TypedCoreValidationPath -> [TypedEvidenceParameter] -> [TypedCoreValidationFailure]
validateOrderedEvidenceParameters path parameters =
  duplicateFailures <> duplicateConstraintFailures <> orderFailures
  where
    parameterIds = [parameterId | TypedEvidenceParameter parameterId _ <- parameters]
    duplicateFailures = duplicateParameterFailures path TypedDuplicateEvidenceParameter TypedEvidenceParameterDetail parameterIds
    (_, _, duplicateConstraintFailures) =
      foldl' checkConstraint (Set.empty, Set.empty, []) parameters
    checkConstraint (seenIds, seenConstraints, failures) (TypedEvidenceParameter parameterId constraint)
      | Set.member parameterId seenIds =
          (seenIds, seenConstraints, failures)
      | Set.member constraint seenConstraints =
          ( Set.insert parameterId seenIds,
            seenConstraints,
            failures
              <> [ failure
                     path
                     TypedDuplicateEvidenceParameter
                     (TypedEvidenceParameterDetail parameterId)
                 ]
          )
      | otherwise =
          ( Set.insert parameterId seenIds,
            Set.insert constraint seenConstraints,
            failures
          )
    orderFailures =
      [ failure path TypedInvalidEvidenceParameterOrder (TypedIndexDetail index)
      | (index, TypedEvidenceParameter (TypedEvidenceParameterId actual) _) <- zip [0 ..] parameters,
        actual /= index
      ]

validateDataDeclaration :: ModuleContext -> TypedCoreValidationPath -> TypedDataDeclaration -> [TypedCoreValidationFailure]
validateDataDeclaration context path (TypedDataDeclaration spanValue name parameters constructors) =
  validateSpan path spanValue
    <> validateLocalDefinitionName context [TypedTypeNamespace] path name
    <> validateOrderedTypeParameters path parameters
    <> (if null constructors then [failure path TypedDataRecipeMismatch (TypedArityDetail 1 0)] else [])
    <> concatMap validateConstructor constructors
  where
    scope = Set.fromList parameters
    validateConstructor (TypedConstructorDeclaration binderId constructorName fields recipes) =
      validateLocalDefinitionName context [TypedConstructorNamespace] path constructorName
        <> validateBinderDefinition context path binderId constructorName
        <> concatMap (validateType path scope) fields
        <> concatMap (validateDataTypeApplications context path) fields
        <> concatMap (validateSourceDataTypeApplications context path) fields
        <> concatMap (validateRecipe path scope) recipes
        <> dataRecipeFailures fields recipes
    dataRecipeFailures fields recipes
      | length fields /= length recipes =
          [failure path TypedDataRecipeMismatch (TypedArityDetail (length fields) (length recipes))]
      | otherwise = concat (zipWith fieldFailure fields recipes)
    fieldFailure fieldType recipe =
      case expectedValueRecipe fieldType of
        Just expected
          | validRecipeWidth recipe && expected /= recipe ->
              [failure path TypedDataRecipeMismatch (TypedRecipeDetail expected recipe)]
        _ -> []

validateClassDeclaration :: ModuleContext -> TypedCoreValidationPath -> TypedClassDeclaration -> [TypedCoreValidationFailure]
validateClassDeclaration context path (TypedClassDeclaration spanValue name parameters methods) =
  validateSpan path spanValue
    <> validateLocalDefinitionName context [TypedCapabilityNamespace] path name
    <> visibleClassCollisionFailures context path name
    <> (if length parameters == 1 then [] else [failure path TypedMethodSelectionMismatch (TypedArityDetail 1 (length parameters))])
    <> validateOrderedTypeParameters path parameters
    <> duplicateParameterFailures
      path
      TypedDuplicateDeclaration
      TypedNameDetail
      [methodName | TypedMethodSignature methodName _ _ <- methods]
    <> concatMap validateMethod methods
  where
    validateMethod (TypedMethodSignature methodName methodSpan scheme@(TypedScheme binderId methodParameters evidenceParameters primitiveConstraints _ _ _)) =
      validateSpan path methodSpan
        <> validateLocalDefinitionName context [TypedValueNamespace] path methodName
        <> (case methodName of TypedGeneratedName {} -> [failure path TypedUnresolvedName (TypedNameDetail methodName)]; _ -> [])
        <> validateBinderDefinition context path binderId methodName
        <> classMethodSchemeShapeFailures methodParameters evidenceParameters primitiveConstraints
        <> validateSchemeWithOuterScope context path binderId (Set.fromList parameters) scheme
        <> validateSourceSchemeDataTypes context path scheme
    classMethodSchemeShapeFailures methodParameters evidenceParameters primitiveConstraints =
      let obligationCount = length methodParameters + length evidenceParameters + length primitiveConstraints
       in if obligationCount == 0
            then []
            else [failure path TypedBindingValueMismatch (TypedArityDetail 0 obligationCount)]

visibleClassCollisionFailures :: ModuleContext -> TypedCoreValidationPath -> TypedCoreName -> [TypedCoreValidationFailure]
visibleClassCollisionFailures context path name =
  case localCapabilityIdentifier of
    Just identifier
      | any (externalCapabilityMatches identifier) (Set.toList (moduleContextSourceVisibleCapabilities context)) ->
          [failure path TypedDuplicateDeclaration (TypedNameDetail name)]
    _ -> []
  where
    localCapabilityIdentifier =
      case name of
        TypedResolvedName TypedCurrentModule TypedCapabilityNamespace identifier -> Just identifier
        TypedResolvedName TypedAmbientPrelude TypedCapabilityNamespace identifier
          | moduleContextPath context == ["Prelude"] -> Just identifier
        _ -> Nothing
    externalCapabilityMatches identifier key =
      case key of
        ResolvedNameKey modulePath TypedCapabilityNamespace candidate ->
          modulePath /= moduleContextPath context && candidate == identifier
        _ -> False

validateImplDeclaration :: ModuleContext -> [Int] -> TypedCoreValidationPath -> TypedImplDeclaration -> [TypedCoreValidationFailure]
validateImplDeclaration context statementLocation path (TypedImplDeclaration spanValue implId methods) =
  validateSpan path spanValue
    <> implOwnerFailures
    <> validateImplId context path Set.empty implId
    <> concatMap (validateDataTypeApplications context path) (implTargetTypes implId)
    <> concatMap (validateSourceDataTypeApplications context path) (implTargetTypes implId)
    <> duplicateImplMethodFailures path methods
    <> missingImplMethodFailures context path implId methods
    <> concatMap (uncurry validateMethod) (zip [0 ..] methods)
  where
    implOwnerFailures
      | implModulePath implId == moduleContextPath context = []
      | otherwise = [failure path TypedInvisibleImpl (TypedImplDetail implId)]
    validateMethod methodIndex (TypedMethodDefinition methodId@(TypedMethodId methodImplId methodKey) binderId name methodSpan expression) =
      validateSpan path methodSpan
        <> validateMethodId context path Set.empty methodId
        <> (if methodImplId == implId then [] else [failure path TypedMethodSelectionMismatch (TypedImplDetail methodImplId)])
        <> validateLocalDefinitionName context [TypedValueNamespace] path name
        <> validateBinderDefinition context path binderId name
        <> (if coreNameIdentifier name == Just methodKey then [] else [failure path TypedMethodSelectionMismatch (TypedTextDetail methodKey)])
        <> validateImplMethodContract context path implId methodKey expression
        <> validateNamedExpression
          (implMethodContext context implId methodKey)
          statementLocation
          [methodIndex]
          (implMethodRequiresStagedLeadingLambdaRecipe context implId methodKey)
          expression

implMethodRequiresStagedLeadingLambdaRecipe :: ModuleContext -> TypedImplId -> Text -> Bool
implMethodRequiresStagedLeadingLambdaRecipe context implId methodKey =
  case lookupImplMethodScheme context implId methodKey of
    Right (Just (_, scheme)) -> schemeRequiresStagedLeadingLambdaRecipe scheme
    _ -> True

duplicateImplMethodFailures :: TypedCoreValidationPath -> [TypedMethodDefinition] -> [TypedCoreValidationFailure]
duplicateImplMethodFailures path methods = snd (foldl' step (Set.empty, []) methods)
  where
    step (seen, failures) (TypedMethodDefinition (TypedMethodId _ methodKey) _ name _ _)
      | Set.member methodKey seen =
          (seen, failures <> [failure path TypedDuplicateDeclaration (TypedNameDetail name)])
      | otherwise = (Set.insert methodKey seen, failures)

missingImplMethodFailures :: ModuleContext -> TypedCoreValidationPath -> TypedImplId -> [TypedMethodDefinition] -> [TypedCoreValidationFailure]
missingImplMethodFailures context path (TypedImplId _ capability targets) methods =
  case resolvedNameKey (moduleContextPath context) capability >>= (`Map.lookup` moduleContextCapabilityContracts context) of
    Nothing -> []
    Just (CapabilityContract parameters methodSchemes)
      | length targets /= length parameters -> []
      | otherwise ->
          [ failure path TypedMethodSelectionMismatch (TypedTextDetail methodKey)
          | methodKey <- Map.keys methodSchemes,
            Set.notMember methodKey providedMethodKeys
          ]
  where
    providedMethodKeys =
      Set.fromList
        [ methodKey
        | TypedMethodDefinition (TypedMethodId _ methodKey) _ _ _ _ <- methods
        ]

validateImplMethodContract :: ModuleContext -> TypedCoreValidationPath -> TypedImplId -> Text -> TypedExpr -> [TypedCoreValidationFailure]
validateImplMethodContract context path implId methodKey expression =
  case lookupImplMethodScheme context implId methodKey of
    Left () -> []
    Right Nothing -> [failure path TypedMethodSelectionMismatch (TypedTextDetail methodKey)]
    Right (Just (classParameters, scheme@(TypedScheme owner _ _ _ resultType resultRecipe _)))
      | length classParameters == length targets ->
          validateValueContract
            path
            (typedExpressionInfo expression)
            ( ValueContract
                (substituteTypeParameters substitutions qualifiedType)
                (substituteRepresentationParameters substitutions qualifiedRecipe)
            )
            <> validateCallableBindingShape path scheme expression
      | otherwise -> []
      where
        targets = implTargetTypes implId
        substitutions = Map.fromList (zip classParameters targets)
        ownerPath = binderModulePath owner
        (qualifiedType, qualifiedRecipe)
          | ownerPath == moduleContextPath context = (resultType, resultRecipe)
          | otherwise = (qualifyExternalType ownerPath resultType, qualifyExternalRecipe ownerPath resultRecipe)

implMethodContext :: ModuleContext -> TypedImplId -> Text -> ModuleContext
implMethodContext context implId methodKey =
  case lookupImplMethodScheme context implId methodKey of
    Right (Just (_, scheme)) ->
      withSchemeScope scheme context
    _ -> context

validateExpression :: ModuleContext -> [Int] -> [Int] -> TypedExpr -> [TypedCoreValidationFailure]
validateExpression context statementLocation expressionPath =
  validateExpressionWithParentSpan context statementLocation expressionPath True 0 Nothing

validateNamedExpression :: ModuleContext -> [Int] -> [Int] -> Bool -> TypedExpr -> [TypedCoreValidationFailure]
validateNamedExpression context statementLocation expressionPath requireStagedLeadingLambdaRecipe =
  validateExpressionWithParentSpan context statementLocation expressionPath requireStagedLeadingLambdaRecipe 0 Nothing

validateExpressionWithParentSpan :: ModuleContext -> [Int] -> [Int] -> Bool -> Int -> Maybe TypedSpan -> TypedExpr -> [TypedCoreValidationFailure]
validateExpressionWithParentSpan context statementLocation expressionPath requireStagedLambdaRecipe directCalleeArgumentCount parentExplicitSpan expression =
  validateNodeInfo
    context
    path
    (moduleContextTypeScope context)
    True
    (qualifiedMethodExpressionKey expression)
    (qualifiedMethodCandidateKey context expression)
    (typedExpressionInfo expression)
    <> expressionOwnedFailures
    <> concatMap (uncurry validateChild) (zip [0 ..] (expressionChildrenWithContexts context expression))
  where
    path = TypedExpressionPath (moduleContextPath context) statementLocation expressionPath
    validateChild childIndex (childContext, child) =
      validateExpressionWithParentSpan
        childContext
        statementLocation
        (expressionPath <> [childIndex])
        (childRequiresStagedLambdaRecipe childIndex child)
        (childDirectCalleeArgumentCount childIndex)
        (childExplicitSpan childIndex)
        child
    childRequiresStagedLambdaRecipe childIndex child =
      case (expression, child) of
        (TypedLambdaExpr {}, TypedLambdaExpr {})
          | childIndex == 0 -> requireStagedLambdaRecipe
        _ -> True
    childDirectCalleeArgumentCount childIndex =
      case expression of
        TypedApplyExpr {}
          | childIndex == 0 -> directCalleeArgumentCount + 1
        TypedTypeApplicationExpr {}
          | childIndex == 0 -> directCalleeArgumentCount
        TypedBinaryExpr _ (TypedBuiltinOperator "$") _ _
          | childIndex == 0 -> directCalleeArgumentCount + 1
        _ -> 0
    childExplicitSpan childIndex =
      case expression of
        TypedTypeApplicationExpr _ _ explicitSpan _
          | childIndex == 0 -> Just explicitSpan
        TypedApplyExpr {}
          | childIndex == 0 -> parentExplicitSpan
        _ -> Nothing
    expressionOwnedFailures =
      validateExpressionInstantiationOwners parentExplicitSpan context path expression
        <> case expression of
          TypedLiteralExpr info literal -> validateLiteral path info literal
          TypedVariableExpr info name binderReference -> validateVariableExpression context path requireStagedLambdaRecipe directCalleeArgumentCount info name binderReference
          TypedLambdaExpr info binderId name body -> validateLocalDefinitionName context [TypedValueNamespace] path name <> validateBinderDefinition context path binderId name <> validateLambda path requireStagedLambdaRecipe info body
          TypedOperatorValueExpr info operator -> validateOperatorValue context path requireStagedLambdaRecipe directCalleeArgumentCount info operator
          TypedListExpr info expressions -> validateListShape path info expressions
          TypedTupleExpr info expressions -> validateTupleShape path info expressions
          TypedApplyExpr info function argument -> validateApplication path info function argument
          TypedTypeApplicationExpr info function explicitSpan typeArgument ->
            validateSpan path explicitSpan
              <> validateType path (moduleContextTypeScope context) typeArgument
              <> validateDataTypeApplications context path typeArgument
              <> validateSourceDataTypeApplications context path typeArgument
              <> validateExplicitTypeApplication context path info function explicitSpan typeArgument
          TypedIfExpr info condition thenExpression elseExpression ->
            validateConditional path info condition thenExpression elseExpression
          TypedPatternCaseExpr info scrutinee arms -> validateCase context statementLocation expressionPath path info scrutinee arms
          TypedBinaryExpr info operator left right -> validateBinaryOperator context path info operator left right
          TypedLeftSectionExpr info left operator -> validateLeftSectionOperator context path info left operator
          TypedRightSectionExpr info operator right -> validateRightSectionOperator context path info operator right
          TypedBlockExpr info statements ->
            let locatedStatements =
                  [ (nestedStatementLocation statementLocation expressionPath blockIndex, statement)
                  | (blockIndex, statement) <- zip [0 ..] statements
                  ]
                blockContext = withBlockDeclarations statements context
             in validateBlockResult path info statements
                  <> duplicateDeclarationFailures blockContext locatedStatements
                  <> validateBlockStatementsInOrder context locatedStatements

validateBlockResult :: TypedCoreValidationPath -> TypedNodeInfo -> [TypedStatement] -> [TypedCoreValidationFailure]
validateBlockResult path blockInfo statements =
  case reverse statements of
    TypedExpressionStatement _ terminal : _
      | nodeInfoHasCompatibleIntrinsicContract blockInfo && nodeInfoHasCompatibleIntrinsicContract (typedExpressionInfo terminal) ->
          nodeContractFailures path TypedBlockResultMismatch blockInfo (typedExpressionInfo terminal)
      | otherwise -> []
    _ -> [failure path TypedBlockResultMismatch TypedNoValidationDetail]

validateLambda :: TypedCoreValidationPath -> Bool -> TypedNodeInfo -> TypedExpr -> [TypedCoreValidationFailure]
validateLambda path requireStagedRecipe info body =
  recipeFailures <> resultFailures
  where
    recipeFailures
      | requireStagedRecipe = validateStagedLambdaRecipe path info
      | otherwise = []
    bodyInfo = typedExpressionInfo body
    resultFailures =
      case typedNodeType info of
        TypedFunctionType _ expectedResult
          | expectedResult /= typedNodeType bodyInfo ->
              [failure path TypedLambdaResultMismatch (TypedTypeDetail expectedResult (typedNodeType bodyInfo))]
          | nodeInfoHasCompatibleIntrinsicContract info,
            lambdaBodyHasCompatibleIntrinsicContract requireStagedRecipe body,
            Just expectedBodyRecipe <- callableResultRecipe (typedNodeRecipe info) ->
              recipeContractFailures path TypedLambdaResultMismatch expectedBodyRecipe bodyInfo
          | otherwise -> []
        actual -> [failure path TypedLambdaResultMismatch (TypedTypeDetail (TypedFunctionType (typedNodeType bodyInfo) (typedNodeType bodyInfo)) actual)]

lambdaBodyHasCompatibleIntrinsicContract :: Bool -> TypedExpr -> Bool
lambdaBodyHasCompatibleIntrinsicContract requireStagedRecipe body =
  nodeInfoHasCompatibleIntrinsicContract bodyInfo
    && case body of
      TypedLambdaExpr {}
        | requireStagedRecipe ->
            stagedClosureRecipeCompatible (typedNodeType bodyInfo) (typedNodeRecipe bodyInfo)
      _ -> True
  where
    bodyInfo = typedExpressionInfo body

schemeRequiresStagedLeadingLambdaRecipe :: TypedScheme -> Bool
schemeRequiresStagedLeadingLambdaRecipe (TypedScheme _ _ _ _ _ _ callableShape) =
  case callableShape of
    Just TypedDirectCallableShape -> False
    _ -> True

validateStagedLambdaRecipe :: TypedCoreValidationPath -> TypedNodeInfo -> [TypedCoreValidationFailure]
validateStagedLambdaRecipe path info =
  validateStagedCallableValueRecipe path info

validateStagedCallableValueRecipe :: TypedCoreValidationPath -> TypedNodeInfo -> [TypedCoreValidationFailure]
validateStagedCallableValueRecipe path info =
  case stagedClosureRecipe typeValue of
    Just expected
      | callableRecipeCompatible typeValue actual,
        not (stagedClosureRecipeCompatible typeValue actual) ->
          [failure path TypedCallableRecipeMismatch (TypedRecipeDetail expected actual)]
    _ -> []
  where
    typeValue = typedNodeType info
    actual = typedNodeRecipe info

validateListShape :: TypedCoreValidationPath -> TypedNodeInfo -> [TypedExpr] -> [TypedCoreValidationFailure]
validateListShape path info expressions =
  case (typedNodeType info, typedNodeRecipe info) of
    (TypedListType elementType, TypedManagedListRecipe elementRecipe) ->
      concatMap (collectionElementFailures path elementType elementRecipe) expressions
    (TypedListType elementType, _) ->
      concatMap (collectionElementTypeFailures path elementType) expressions
    (actual, _) -> [failure path TypedCollectionShapeMismatch (TypedTypeDetail (TypedListType actual) actual)]

validateTupleShape :: TypedCoreValidationPath -> TypedNodeInfo -> [TypedExpr] -> [TypedCoreValidationFailure]
validateTupleShape path info expressions =
  case typedNodeType info of
    TypedTupleType expectedTypes
      | length expectedTypes /= length expressions ->
          [failure path TypedCollectionShapeMismatch (TypedArityDetail (length expectedTypes) (length expressions))]
      | otherwise ->
          case typedNodeRecipe info of
            TypedManagedProductRecipe expectedRecipes
              | length expectedRecipes == length expressions ->
                  concat
                    [ collectionElementFailures path expectedType expectedRecipeValue expression
                    | (expectedType, expectedRecipeValue, expression) <- zip3 expectedTypes expectedRecipes expressions
                    ]
            _ -> concat [collectionElementTypeFailures path expectedType expression | (expectedType, expression) <- zip expectedTypes expressions]
    actual -> [failure path TypedCollectionShapeMismatch (TypedTypeDetail (TypedTupleType []) actual)]

collectionElementFailures :: TypedCoreValidationPath -> TypedType -> TypedRepresentationRecipe -> TypedExpr -> [TypedCoreValidationFailure]
collectionElementFailures path expectedType expectedRecipeValue expression
  | typedNodeType expressionInfo /= expectedType =
      collectionElementTypeFailures path expectedType expression
  | otherwise =
      recipeContractFailures path TypedCollectionShapeMismatch expectedRecipeValue expressionInfo
  where
    expressionInfo = typedExpressionInfo expression

collectionElementTypeFailures :: TypedCoreValidationPath -> TypedType -> TypedExpr -> [TypedCoreValidationFailure]
collectionElementTypeFailures path expectedType expression
  | typedNodeType expressionInfo == expectedType = []
  | otherwise =
      [failure path TypedCollectionShapeMismatch (TypedTypeDetail expectedType (typedNodeType expressionInfo))]
  where
    expressionInfo = typedExpressionInfo expression

validateExplicitTypeApplication :: ModuleContext -> TypedCoreValidationPath -> TypedNodeInfo -> TypedExpr -> TypedSpan -> TypedType -> [TypedCoreValidationFailure]
validateExplicitTypeApplication context path info function explicitSpan typeArgument =
  case function of
    TypedVariableExpr functionInfo name _ ->
      case lookupSchemeByName context name of
        Just scheme -> validateSchemeApplication functionInfo (Just scheme)
        Nothing ->
          case name of
            TypedBuiltinName methodKey -> validateQualifiedMethodApplication methodKey
            _ -> [failure path TypedInstantiationMismatch TypedNoValidationDetail]
    TypedOperatorValueExpr functionInfo (TypedResolvedOperator name _) ->
      validateSchemeApplication functionInfo (lookupSchemeByName context name)
    _ -> [failure path TypedInstantiationMismatch TypedNoValidationDetail]
  where
    instantiations = nodeInfoInstantiations info
    validateSchemeApplication functionInfo maybeScheme =
      case maybeScheme of
        Just scheme@(TypedScheme owner (firstParameter : _) _ _ _ _ _)
          | any
              ( \instantiation ->
                  matchingExplicitInstantiation owner firstParameter instantiation
                    && instantiation `elem` nodeInfoInstantiations functionInfo
              )
              instantiations ->
              validateInstantiatedResult scheme
          | otherwise -> [failure path TypedInstantiationMismatch (TypedBinderDetail owner)]
        Just (TypedScheme owner [] _ _ _ _ _) ->
          [failure path TypedInstantiationMismatch (TypedBinderDetail owner)]
        Nothing -> [failure path TypedInstantiationMismatch TypedNoValidationDetail]
    matchingExplicitInstantiation owner firstParameter (TypedInstantiation candidateOwner arguments maybeSpan) =
      owner == candidateOwner
        && maybeSpan == Just explicitSpan
        && any (typeArgumentMatches firstParameter) arguments
    typeArgumentMatches expectedParameter (TypedTypeArgument candidateParameter candidateType) =
      candidateParameter == expectedParameter && candidateType == typeArgument
    validateInstantiatedResult scheme =
      case schemeValueContract context info scheme of
        Just contract -> validateTypeApplicationValueContract path info contract
        Nothing -> []
    validateQualifiedMethodApplication methodKey =
      case qualifiedMethodApplicationContracts context methodKey typeArgument info of
        [contract] -> validateTypeApplicationValueContract path info contract
        _ -> [failure path TypedInstantiationMismatch TypedNoValidationDetail]

validateTypeApplicationValueContract :: TypedCoreValidationPath -> TypedNodeInfo -> ValueContract -> [TypedCoreValidationFailure]
validateTypeApplicationValueContract path info (ValueContract expectedType expectedRecipeValue)
  | typedNodeType info /= expectedType =
      [failure path TypedApplicationResultMismatch (TypedTypeDetail expectedType (typedNodeType info))]
  | typedNodeRecipe info /= expectedRecipeValue =
      [failure path TypedApplicationResultMismatch (TypedRecipeDetail expectedRecipeValue (typedNodeRecipe info))]
  | otherwise = []

qualifiedMethodApplicationContracts :: ModuleContext -> Text -> TypedType -> TypedNodeInfo -> [ValueContract]
qualifiedMethodApplicationContracts context methodKey typeArgument (TypedNodeInfo _ _ _ evidenceSelections) =
  [ ValueContract
      (substituteTypeParameters substitutions qualifiedType)
      (substituteRepresentationParameters substitutions qualifiedRecipe)
  | TypedSelectedEvidence
      ( TypedEvidenceUse
          _
          (TypedCapabilityConstraint capability (Just constraintMethod) targetType)
          implId@(TypedImplId _ capabilityName targetTypes)
          (Just (TypedMethodId methodImplId selectedMethod))
        ) <-
      evidenceSelections,
    methodImplId == implId,
    targetType == typeArgument,
    targetTypes == [targetType],
    capabilityKey <- maybeToList (resolvedNameKey (moduleContextPath context) capability),
    resolvedNameKey (moduleContextPath context) capabilityName == Just capabilityKey,
    methodKeyMatches capability constraintMethod methodKey,
    methodKeyMatches capability selectedMethod methodKey,
    Set.member capabilityKey (moduleContextVisibleNames context),
    CapabilityContract [classParameter] methods <-
      maybeToList (Map.lookup capabilityKey (moduleContextCapabilityContracts context)),
    (contractMethod, TypedScheme owner _ _ _ resultType resultRecipe _) <- Map.toList methods,
    methodKeyMatches capability contractMethod methodKey,
    let substitutions = Map.singleton classParameter typeArgument,
    let ownerPath = binderModulePath owner,
    let qualifiedType =
          if ownerPath == moduleContextPath context
            then resultType
            else qualifyExternalType ownerPath resultType,
    let qualifiedRecipe =
          if ownerPath == moduleContextPath context
            then resultRecipe
            else qualifyExternalRecipe ownerPath resultRecipe
  ]

expressionInstantiationOwners :: ModuleContext -> TypedExpr -> Set TypedBinderId
expressionInstantiationOwners context expression =
  case expression of
    TypedVariableExpr _ name _ ->
      Set.fromList
        ( [owner | TypedScheme owner _ _ _ _ _ _ <- maybeToList (lookupSchemeByName context name)]
            <> [owner | ConstructorContract owner _ _ _ <- maybeToList (lookupConstructorContract context name)]
        )
    TypedOperatorValueExpr _ operator -> operatorSchemeOwners context operator
    TypedApplyExpr _ function _ -> expressionInstantiationOwners context function
    TypedTypeApplicationExpr _ function _ _ -> expressionInstantiationOwners context function
    TypedBinaryExpr _ operator _ _ -> operatorSchemeOwners context operator
    TypedLeftSectionExpr _ _ operator -> operatorSchemeOwners context operator
    TypedRightSectionExpr _ operator _ -> operatorSchemeOwners context operator
    _ -> Set.empty

operatorSchemeOwners :: ModuleContext -> TypedOperatorRef -> Set TypedBinderId
operatorSchemeOwners context operator =
  case operator of
    TypedBuiltinOperator _ -> Set.empty
    TypedResolvedOperator name _ ->
      Set.fromList [owner | TypedScheme owner _ _ _ _ _ _ <- maybeToList (lookupSchemeByName context name)]

bindingExpressionInstantiationOwners :: ModuleContext -> TypedExpr -> Set TypedBinderId
bindingExpressionInstantiationOwners context expression =
  case expression of
    TypedVariableExpr _ name _ ->
      Set.fromList
        ( [owner | TypedScheme owner _ _ _ _ _ _ <- maybeToList (lookupSchemeByName context name)]
            <> [owner | ConstructorContract owner _ _ _ <- maybeToList (lookupConstructorContract context name)]
        )
    TypedOperatorValueExpr _ operator -> operatorSchemeOwners context operator
    _ -> Set.empty

validateExpressionInstantiationOwners :: Maybe TypedSpan -> ModuleContext -> TypedCoreValidationPath -> TypedExpr -> [TypedCoreValidationFailure]
validateExpressionInstantiationOwners parentExplicitSpan context path expression =
  case expression of
    TypedTypeApplicationExpr _ function _ _ ->
      let allowedOwners = bindingExpressionInstantiationOwners context function
       in [ failure path TypedInstantiationMismatch (TypedBinderDetail owner)
          | TypedInstantiation owner arguments _ <- nodeInfoInstantiations (typedExpressionInfo expression),
            contract <- maybeToList (lookupInstantiationContract context owner),
            instantiationContractAcceptsArguments arguments contract,
            Set.notMember owner allowedOwners
          ]
    _ ->
      [ failure path TypedInstantiationMismatch (TypedBinderDetail owner)
      | TypedInstantiation owner arguments maybeSpan <- nodeInfoInstantiations (typedExpressionInfo expression),
        contract <- maybeToList (lookupInstantiationContract context owner),
        instantiationContractAcceptsArguments arguments contract,
        Set.notMember owner (expressionInstantiationOwners context expression)
          || case maybeSpan of
            Just explicitSpan -> parentExplicitSpan /= Just explicitSpan
            Nothing -> False
      ]

lookupSchemeByName :: ModuleContext -> TypedCoreName -> Maybe TypedScheme
lookupSchemeByName context name = do
  expectedKey <- resolvedNameKey (moduleContextPath context) name
  Map.lookup expectedKey (moduleContextActiveSchemes context)

binderDefinitionKey :: TypedBinderId -> Maybe ResolvedNameKey
binderDefinitionKey (TypedBinderId (modulePath, _, name)) = definitionNameKey modulePath name

qualifiedMethodEvidenceTarget :: ModuleContext -> Text -> TypedNodeInfo -> Bool
qualifiedMethodEvidenceTarget context methodKey (TypedNodeInfo _ _ _ evidenceSelections) =
  any selectionMatches evidenceSelections
  where
    selectionMatches selection =
      case selection of
        TypedSelectedEvidence (TypedEvidenceUse _ constraint _ _) -> constraintMatches constraint
        TypedEvidenceCandidates constraint _ -> constraintMatches constraint
    constraintMatches (TypedCapabilityConstraint capability (Just constraintMethod) _) =
      case resolvedNameKey (moduleContextPath context) capability of
        Just key ->
          methodKeyMatches capability constraintMethod methodKey
            && Set.member key (moduleContextVisibleNames context)
            && case Map.lookup key (moduleContextCapabilityContracts context) of
              Just (CapabilityContract _ methods) ->
                any (\contractMethod -> methodKeyMatches capability contractMethod methodKey) (Map.keys methods)
              Nothing -> False
        Nothing -> False
    constraintMatches _ = False

qualifiedMethodValueContracts :: ModuleContext -> Text -> TypedNodeInfo -> [(TypedScheme, ValueContract)]
qualifiedMethodValueContracts context methodKey (TypedNodeInfo _ _ _ evidenceSelections) =
  mapMaybe
    (qualifiedMethodConstraintContract context methodKey)
    (nub (map selectionConstraint evidenceSelections))
  where
    selectionConstraint selection =
      case selection of
        TypedSelectedEvidence (TypedEvidenceUse _ constraint _ _) -> constraint
        TypedEvidenceCandidates constraint _ -> constraint

qualifiedMethodSelectedSchemes :: ModuleContext -> Text -> TypedNodeInfo -> [TypedScheme]
qualifiedMethodSelectedSchemes context methodKey (TypedNodeInfo _ _ _ evidenceSelections) =
  mapMaybe
    (fmap fst . qualifiedMethodConstraintContract context methodKey)
    (nub selectedConstraints)
  where
    selectedConstraints =
      [ constraint
      | TypedSelectedEvidence (TypedEvidenceUse _ constraint _ _) <- evidenceSelections
      ]

qualifiedMethodConstraintContract :: ModuleContext -> Text -> TypedCapabilityConstraint -> Maybe (TypedScheme, ValueContract)
qualifiedMethodConstraintContract context methodKey constraint =
  case matchingMethodContracts of
    [(classParameter, scheme@(TypedScheme owner _ _ _ resultType resultRecipe _))] ->
      let substitutions = Map.singleton classParameter targetType
          ownerPath = binderModulePath owner
          (qualifiedType, qualifiedRecipe)
            | ownerPath == moduleContextPath context = (resultType, resultRecipe)
            | otherwise = (qualifyExternalType ownerPath resultType, qualifyExternalRecipe ownerPath resultRecipe)
       in Just
            ( scheme,
              ValueContract
                (substituteTypeParameters substitutions qualifiedType)
                (substituteRepresentationParameters substitutions qualifiedRecipe)
            )
    _ -> Nothing
  where
    (capability, constraintMethod, targetType) =
      case constraint of
        TypedCapabilityConstraint constraintCapability (Just method) constraintTarget ->
          (constraintCapability, Just method, constraintTarget)
        TypedCapabilityConstraint constraintCapability Nothing constraintTarget ->
          (constraintCapability, Nothing, constraintTarget)
    matchingMethodContracts =
      [ (classParameter, scheme)
      | Just method <- [constraintMethod],
        key <- maybeToList (resolvedNameKey (moduleContextPath context) capability),
        Set.member key (moduleContextVisibleNames context),
        methodKeyMatches capability method methodKey,
        CapabilityContract [classParameter] methods <-
          maybeToList (Map.lookup key (moduleContextCapabilityContracts context)),
        (contractMethod, scheme) <- Map.toList methods,
        methodKeyMatches capability contractMethod methodKey
      ]

validateVariableExpression :: ModuleContext -> TypedCoreValidationPath -> Bool -> Int -> TypedNodeInfo -> TypedCoreName -> Maybe TypedBinderId -> [TypedCoreValidationFailure]
validateVariableExpression context path requireStagedCallableRecipe directCalleeArgumentCount info name binderReference =
  visibilityFailures
    <> case name of
      TypedBuiltinName identifier
        | qualifiedMethodTarget ->
            validateAbsentBinderReference path binderReference
              <> case qualifiedMethodValueContracts context identifier info of
                [] -> []
                [(_, contract)] ->
                  let contractFailures = validateValueContract path info contract
                      shapeFailures
                        | null contractFailures =
                            case qualifiedMethodSelectedSchemes context identifier info of
                              [scheme] -> validateDirectCallableSchemeUse path directCalleeArgumentCount scheme
                              _ -> []
                        | otherwise = []
                   in shapeFailures <> contractFailures
                contracts ->
                  [failure path TypedAmbiguousEvidence (TypedArityDetail 1 (length contracts))]
        | otherwise ->
            validateAbsentBinderReference path binderReference
              <> validateBuiltinValueContract context path requireStagedCallableRecipe directCalleeArgumentCount info identifier
      _ ->
        case resolvedNameKey (moduleContextPath context) name >>= (`Map.lookup` moduleContextLexicalContracts context) of
          Just contract -> validateLexicalBinderReference path info binderReference contract
          Nothing ->
            case name of
              TypedResolvedName _ TypedValueNamespace _ ->
                case lookupSchemeByName context name of
                  Just scheme -> validateVariableSchemeContract context path directCalleeArgumentCount info binderReference scheme
                  Nothing -> validateAbsentBinderReference path binderReference
              TypedResolvedName _ TypedConstructorNamespace _ ->
                case lookupConstructorContract context name of
                  Just contract ->
                    validateConstructorBinderReference path binderReference contract
                      <> validateConstructorExpressionContract context path requireStagedCallableRecipe directCalleeArgumentCount info contract
                  Nothing -> validateAbsentBinderReference path binderReference
              _ -> validateAbsentBinderReference path binderReference
  where
    qualifiedMethodTarget =
      case name of
        TypedBuiltinName methodKey -> qualifiedMethodEvidenceTarget context methodKey info
        _ -> False
    visibilityFailures
      | qualifiedMethodTarget = []
      | otherwise = validateVisibleNameInNamespaces [TypedValueNamespace, TypedConstructorNamespace] context path name

validateAbsentBinderReference :: TypedCoreValidationPath -> Maybe TypedBinderId -> [TypedCoreValidationFailure]
validateAbsentBinderReference _ Nothing = []
validateAbsentBinderReference path (Just binderId) =
  [failure path TypedBinderReferenceMismatch (TypedBinderDetail binderId)]

validateLexicalBinderReference :: TypedCoreValidationPath -> TypedNodeInfo -> Maybe TypedBinderId -> BinderContract -> [TypedCoreValidationFailure]
validateLexicalBinderReference path info binderReference (BinderContract expectedBinder _ expectedType expectedRecipeValue) =
  case binderReference of
    Just actualBinder
      | actualBinder == expectedBinder,
        typedNodeType info == expectedType,
        typedNodeRecipe info == expectedRecipeValue ->
          []
      | otherwise -> mismatch actualBinder
    Nothing -> mismatch expectedBinder
  where
    mismatch binderId = [failure path TypedBinderReferenceMismatch (TypedBinderDetail binderId)]

validateConstructorBinderReference :: TypedCoreValidationPath -> Maybe TypedBinderId -> ConstructorContract -> [TypedCoreValidationFailure]
validateConstructorBinderReference path binderReference (ConstructorContract expectedBinder _ _ _) =
  case binderReference of
    Just actualBinder
      | actualBinder == expectedBinder -> []
      | otherwise -> mismatch actualBinder
    Nothing -> mismatch expectedBinder
  where
    mismatch binderId = [failure path TypedBinderReferenceMismatch (TypedBinderDetail binderId)]

validateVariableSchemeContract :: ModuleContext -> TypedCoreValidationPath -> Int -> TypedNodeInfo -> Maybe TypedBinderId -> TypedScheme -> [TypedCoreValidationFailure]
validateVariableSchemeContract context path directCalleeArgumentCount info binderReference scheme@(TypedScheme owner parameters evidenceParameters _ _ _ _) =
  instantiationFailures
    <> missingEvidenceWithoutInstantiation
    <> validateDirectCallableSchemeUse path directCalleeArgumentCount scheme
    <> validateSchemeBinderReference
    <> case schemeValueContract context info scheme of
      Just contract
        | binderReference == Just owner -> validateReferencedValueContract path owner info contract
        | otherwise -> []
      Nothing -> []
  where
    validateSchemeBinderReference =
      case binderReference of
        Just actualBinder
          | actualBinder == owner -> []
          | otherwise -> [failure path TypedBinderReferenceMismatch (TypedBinderDetail actualBinder)]
        Nothing -> [failure path TypedBinderReferenceMismatch (TypedBinderDetail owner)]
    matchingOwnerInstantiation =
      find (matchingInstantiation owner parameters) (nodeInfoInstantiations info)
    requiresInstantiation = not (null parameters && null evidenceParameters)
    instantiationFailures
      | requiresInstantiation && matchingOwnerInstantiation == Nothing =
          [failure path TypedInstantiationMismatch (TypedBinderDetail owner)]
      | otherwise = []
    missingEvidenceWithoutInstantiation
      | null parameters,
        not (null evidenceParameters),
        matchingOwnerInstantiation == Nothing =
          [ failure path TypedMissingEvidence (TypedEvidenceParameterDetail parameterId)
          | TypedEvidenceParameter parameterId _ <- evidenceParameters
          ]
      | otherwise = []

validateDirectCallableSchemeUse :: TypedCoreValidationPath -> Int -> TypedScheme -> [TypedCoreValidationFailure]
validateDirectCallableSchemeUse path directCalleeArgumentCount (TypedScheme owner _ _ _ _ recipe callableShape)
  | callableShape == Just TypedDirectCallableShape,
    maybe True (> directCalleeArgumentCount) (directCallableRecipeArity recipe) =
      [failure path TypedCallableShapeMismatch (TypedBinderDetail owner)]
  | otherwise = []

validateReferencedValueContract :: TypedCoreValidationPath -> TypedBinderId -> TypedNodeInfo -> ValueContract -> [TypedCoreValidationFailure]
validateReferencedValueContract path owner info (ValueContract expectedType expectedRecipeValue)
  | typedNodeType info == expectedType,
    typedNodeRecipe info == expectedRecipeValue =
      []
  | otherwise = [failure path TypedBinderReferenceMismatch (TypedBinderDetail owner)]

validateBuiltinValueContract :: ModuleContext -> TypedCoreValidationPath -> Bool -> Int -> TypedNodeInfo -> Text -> [TypedCoreValidationFailure]
validateBuiltinValueContract context path requireStagedCallableRecipe directCalleeArgumentCount info identifier =
  case lookupTypedBuiltinSymbol identifier of
    Nothing -> []
    Just builtinSymbol ->
      case builtinConcreteValueType builtinSymbol of
        Just expectedType ->
          case expectedNativeCallableUseRecipe requireStagedCallableRecipe directCalleeArgumentCount expectedType of
            Just expectedRecipeValue ->
              validateValueContract path info (ValueContract expectedType expectedRecipeValue)
            Nothing -> [failure path TypedBindingValueMismatch (TypedTextDetail identifier)]
        Nothing
          | builtinPolymorphicValueTypeMatches context builtinSymbol (typedNodeType info) ->
              case expectedNativeCallableUseRecipe requireStagedCallableRecipe directCalleeArgumentCount (typedNodeType info) of
                Just expectedRecipeValue ->
                  validateValueContract path info (ValueContract (typedNodeType info) expectedRecipeValue)
                Nothing -> [failure path TypedBindingValueMismatch (TypedTextDetail identifier)]
          | otherwise -> [failure path TypedBindingValueMismatch (TypedTextDetail identifier)]

expectedNativeCallableUseRecipe :: Bool -> Int -> TypedType -> Maybe TypedRepresentationRecipe
expectedNativeCallableUseRecipe requireStagedCallableRecipe directCalleeArgumentCount typeValue
  | not requireStagedCallableRecipe = expectedRecipe typeValue
  | otherwise =
      case expectedRecipe typeValue of
        directRecipe@(Just recipe)
          | maybe False (<= directCalleeArgumentCount) (directCallableRecipeArity recipe) -> directRecipe
        _ -> expectedValueRecipe typeValue

lookupTypedBuiltinSymbol :: Text -> Maybe BuiltinSymbol
lookupTypedBuiltinSymbol identifier =
  case lookupBuiltinSymbol identifier of
    Just builtinSymbol -> Just builtinSymbol
    Nothing -> lookupKernelBuiltinSymbol identifier

builtinConcreteValueType :: BuiltinSymbol -> Maybe TypedType
builtinConcreteValueType builtinSymbol =
  case builtinSymbol of
    BuiltinMap -> Nothing
    BuiltinFilter -> Nothing
    BuiltinHd -> Nothing
    BuiltinTl -> Nothing
    BuiltinPrint -> Nothing
    BuiltinToInt8 -> Nothing
    BuiltinToInt16 -> Nothing
    BuiltinToInt32 -> Nothing
    BuiltinToInt64 -> Nothing
    BuiltinToUInt8 -> Nothing
    BuiltinToUInt16 -> Nothing
    BuiltinToUInt32 -> Nothing
    BuiltinToUInt64 -> Nothing
    BuiltinToFloat16 -> Nothing
    BuiltinToFloat32 -> Nothing
    BuiltinToFloat64 -> Nothing
    BuiltinListPrependRaw -> Nothing
    BuiltinListReverseRaw -> Nothing
    BuiltinCharToUInt32 ->
      Just (TypedFunctionType TypedCharType (TypedNumericType TypedUInt32Type))
    BuiltinCharFromUInt32Raw ->
      Just (TypedFunctionType (TypedNumericType TypedUInt32Type) (TypedListType TypedCharType))
    BuiltinCharIsAlpha -> Just charPredicateType
    BuiltinCharIsAlphaNum -> Just charPredicateType
    BuiltinCharIsDigit -> Just charPredicateType
    BuiltinCharIsSpace -> Just charPredicateType
    BuiltinCharIsHexDigit -> Just charPredicateType
    BuiltinCharIsLower -> Just charPredicateType
    BuiltinCharIsUpper -> Just charPredicateType
    BuiltinCharToLower -> Just charTransformType
    BuiltinCharToUpper -> Just charTransformType
    BuiltinTextLength -> Just (TypedFunctionType TypedTextType TypedIntType)
    BuiltinTextUnconsRaw ->
      Just
        ( TypedFunctionType
            TypedTextType
            (TypedListType (TypedTupleType [TypedCharType, TypedTextType]))
        )
    BuiltinTextAppend -> Just (binaryFunctionType TypedTextType TypedTextType TypedTextType)
    BuiltinTextAppendChar -> Just (binaryFunctionType TypedTextType TypedCharType TypedTextType)
    BuiltinTextFromChars -> Just (TypedFunctionType (TypedListType TypedCharType) TypedTextType)
    BuiltinTextConcat -> Just (TypedFunctionType (TypedListType TypedTextType) TypedTextType)
    BuiltinRenderValue -> Nothing
    BuiltinReadTextRaw -> Just (TypedFunctionType TypedTextType hostIOOutcomeTypedType)
    BuiltinWriteTextRaw -> Just (binaryFunctionType TypedTextType TypedTextType hostIOOutcomeTypedType)
    BuiltinReadStdinRaw -> Just (TypedFunctionType typedUnitType hostIOOutcomeTypedType)
    BuiltinWriteStdoutRaw -> Just (TypedFunctionType TypedTextType hostIOOutcomeTypedType)
    BuiltinWriteStderrRaw -> Just (TypedFunctionType TypedTextType hostIOOutcomeTypedType)
    BuiltinArguments -> Just (TypedFunctionType typedUnitType (TypedListType TypedTextType))
    BuiltinExit -> Just (TypedFunctionType TypedIntType typedUnitType)
  where
    charPredicateType = TypedFunctionType TypedCharType TypedBoolType
    charTransformType = TypedFunctionType TypedCharType TypedCharType

builtinPolymorphicValueTypeMatches :: ModuleContext -> BuiltinSymbol -> TypedType -> Bool
builtinPolymorphicValueTypeMatches context builtinSymbol typeValue =
  case (builtinSymbol, typeValue) of
    (BuiltinMap, TypedFunctionType (TypedFunctionType source target) (TypedFunctionType (TypedListType input) (TypedListType output))) ->
      source == input && target == output
    (BuiltinFilter, TypedFunctionType (TypedFunctionType predicateInput TypedBoolType) (TypedFunctionType (TypedListType input) (TypedListType output))) ->
      predicateInput == input && input == output
    (BuiltinHd, TypedFunctionType (TypedListType input) output) -> input == output
    (BuiltinTl, TypedFunctionType (TypedListType input) (TypedListType output)) -> input == output
    (BuiltinPrint, TypedFunctionType input output) -> input == output
    (BuiltinListPrependRaw, TypedFunctionType element (TypedFunctionType (TypedListType input) (TypedListType output))) ->
      element == input && input == output
    (BuiltinListReverseRaw, TypedFunctionType (TypedListType input) (TypedListType output)) ->
      input == output
    (BuiltinRenderValue, TypedFunctionType _ TypedTextType) -> True
    (_, TypedFunctionType source target)
      | Just numericTarget <- builtinSymbolNumericConversionTarget builtinSymbol ->
          numericValueTypeSupported context source
            && Just target == (TypedNumericType <$> typedNumericType numericTarget)
    _ -> False

numericValueTypeSupported :: ModuleContext -> TypedType -> Bool
numericValueTypeSupported context typeValue =
  case typeValue of
    TypedIntType -> True
    TypedFloatType -> True
    TypedNumericType _ -> True
    TypedTypeParameterType _ ->
      any constrainsType (moduleContextPrimitiveConstraints context)
    _ -> False
  where
    constrainsType constraint =
      case constraint of
        TypedNumericPrimitiveConstraint _ targetType -> targetType == typeValue
        _ -> False

typedNumericType :: NumericType -> Maybe TypedNumericType
typedNumericType numericType =
  case numericType of
    NumericInt8 -> Just TypedInt8Type
    NumericInt16 -> Just TypedInt16Type
    NumericInt32 -> Just TypedInt32Type
    NumericInt64 -> Just TypedInt64Type
    NumericUInt8 -> Just TypedUInt8Type
    NumericUInt16 -> Just TypedUInt16Type
    NumericUInt32 -> Just TypedUInt32Type
    NumericUInt64 -> Just TypedUInt64Type
    NumericFloat16 -> Just TypedFloat16Type
    NumericFloat32 -> Just TypedFloat32Type
    NumericFloat64 -> Just TypedFloat64Type

binaryFunctionType :: TypedType -> TypedType -> TypedType -> TypedType
binaryFunctionType first second result =
  TypedFunctionType first (TypedFunctionType second result)

typedUnitType :: TypedType
typedUnitType = TypedTupleType []

hostIOOutcomeTypedType :: TypedType
hostIOOutcomeTypedType =
  TypedTupleType [TypedBoolType, TypedTextType, TypedTextType, TypedTextType]

schemeValueContract :: ModuleContext -> TypedNodeInfo -> TypedScheme -> Maybe ValueContract
schemeValueContract context info (TypedScheme owner parameters _ _ resultType resultRecipe _) =
  case parameters of
    [] -> Just (ValueContract qualifiedType qualifiedRecipe)
    _ -> do
      TypedInstantiation _ arguments _ <- find (matchingInstantiation owner parameters) (nodeInfoInstantiations info)
      let substitutions = Map.fromList [(parameterId, typeValue) | TypedTypeArgument parameterId typeValue <- arguments]
      pure
        ( ValueContract
            (substituteTypeParameters substitutions qualifiedType)
            (substituteRepresentationParameters substitutions qualifiedRecipe)
        )
  where
    ownerModulePath = binderModulePath owner
    (qualifiedType, qualifiedRecipe)
      | ownerModulePath == moduleContextPath context = (resultType, resultRecipe)
      | otherwise = (qualifyExternalType ownerModulePath resultType, qualifyExternalRecipe ownerModulePath resultRecipe)

matchingInstantiation :: TypedBinderId -> [TypedTypeParameterId] -> TypedInstantiation -> Bool
matchingInstantiation expectedOwner expectedParameters (TypedInstantiation actualOwner arguments _) =
  actualOwner == expectedOwner
    && map typeArgumentParameter arguments == expectedParameters
  where
    typeArgumentParameter (TypedTypeArgument parameterId _) = parameterId

validateValueContract :: TypedCoreValidationPath -> TypedNodeInfo -> ValueContract -> [TypedCoreValidationFailure]
validateValueContract path info (ValueContract expectedType expectedRecipeValue)
  | typedNodeType info /= expectedType =
      [failure path TypedBindingValueMismatch (TypedTypeDetail expectedType (typedNodeType info))]
  | typedNodeRecipe info /= expectedRecipeValue =
      [failure path TypedBindingValueMismatch (TypedRecipeDetail expectedRecipeValue (typedNodeRecipe info))]
  | otherwise = []

lookupConstructorContract :: ModuleContext -> TypedCoreName -> Maybe ConstructorContract
lookupConstructorContract context name = do
  key <- resolvedNameKey (moduleContextPath context) name
  Map.lookup key (moduleContextConstructorContracts context)

validateConstructorExpressionContract :: ModuleContext -> TypedCoreValidationPath -> Bool -> Int -> TypedNodeInfo -> ConstructorContract -> [TypedCoreValidationFailure]
validateConstructorExpressionContract context path requireStagedCallableRecipe directCalleeArgumentCount info (ConstructorContract owner dataKey parameters fieldTypes) =
  missingInstantiationFailures
    <> validateValueContract path info (ValueContract expectedType expectedRecipeValue)
  where
    genericResult = TypedDataType (resolvedNameFromKey context dataKey) (map TypedTypeParameterType parameters)
    genericType = foldr TypedFunctionType genericResult fieldTypes
    ownerInstantiation =
      find (matchingInstantiation owner parameters) (nodeInfoInstantiations info)
    missingInstantiationFailures
      | null parameters || ownerInstantiation /= Nothing = []
      | otherwise = [failure path TypedInstantiationMismatch (TypedBinderDetail owner)]
    substitutions =
      case ownerInstantiation of
        Just (TypedInstantiation _ arguments _) ->
          Map.fromList [(parameterId, typeValue) | TypedTypeArgument parameterId typeValue <- arguments]
        Nothing -> inferConstructorSubstitutions context dataKey parameters (length fieldTypes) (typedNodeType info)
    expectedType = substituteTypeParameters substitutions genericType
    expectedRecipeValue =
      maybe
        (typedNodeRecipe info)
        id
        (expectedNativeCallableUseRecipe requireStagedCallableRecipe directCalleeArgumentCount expectedType)

inferConstructorSubstitutions :: ModuleContext -> ResolvedNameKey -> [TypedTypeParameterId] -> Int -> TypedType -> Map TypedTypeParameterId TypedType
inferConstructorSubstitutions context dataKey parameters fieldCount actualType =
  case dropFunctionArguments fieldCount actualType of
    TypedDataType dataName arguments
      | resolvedNameKey (moduleContextPath context) dataName == Just dataKey,
        length parameters == length arguments ->
          Map.fromList (zip parameters arguments)
    _ -> Map.empty

dropFunctionArguments :: Int -> TypedType -> TypedType
dropFunctionArguments count typeValue
  | count <= 0 = typeValue
dropFunctionArguments count (TypedFunctionType _ result) = dropFunctionArguments (count - 1) result
dropFunctionArguments _ typeValue = typeValue

expressionChildrenWithContexts :: ModuleContext -> TypedExpr -> [(ModuleContext, TypedExpr)]
expressionChildrenWithContexts context expression =
  case expression of
    TypedLambdaExpr info binderId name body ->
      [ ( case lambdaArgumentContract info binderId name of
            Just contract -> withLexicalContracts [contract] context
            Nothing -> withVisibleNames [name] context,
          body
        )
      ]
    TypedListExpr _ expressions -> [(context, child) | child <- expressions]
    TypedTupleExpr _ expressions -> [(context, child) | child <- expressions]
    TypedApplyExpr _ function argument -> [(context, function), (context, argument)]
    TypedTypeApplicationExpr _ function _ _ -> [(context, function)]
    TypedIfExpr _ condition thenExpression elseExpression ->
      [(context, condition), (context, thenExpression), (context, elseExpression)]
    TypedPatternCaseExpr _ scrutinee arms ->
      (context, scrutinee)
        : concat
          [ let armContext = withLexicalContracts (patternBoundContracts patternValue) context
             in [(armContext, child) | child <- maybeToList maybeGuard <> [result]]
          | TypedCaseArm patternValue maybeGuard result <- arms
          ]
    TypedBinaryExpr _ _ left right -> [(context, left), (context, right)]
    TypedLeftSectionExpr _ left _ -> [(context, left)]
    TypedRightSectionExpr _ _ right -> [(context, right)]
    TypedLiteralExpr {} -> []
    TypedVariableExpr {} -> []
    TypedOperatorValueExpr {} -> []
    TypedBlockExpr {} -> []

lambdaArgumentContract :: TypedNodeInfo -> TypedBinderId -> TypedCoreName -> Maybe BinderContract
lambdaArgumentContract info binderId name =
  case typedNodeType info of
    TypedFunctionType argumentType _ -> BinderContract binderId name argumentType <$> lambdaArgumentRecipe info argumentType
    _ -> Nothing

lambdaArgumentRecipe :: TypedNodeInfo -> TypedType -> Maybe TypedRepresentationRecipe
lambdaArgumentRecipe info argumentType =
  case typedNodeRecipe info of
    TypedClosureRecipe (argumentRecipe : _) _ -> Just argumentRecipe
    _ -> expectedRecipe argumentType

expressionChildren :: TypedExpr -> [TypedExpr]
expressionChildren expression =
  case expression of
    TypedLiteralExpr {} -> []
    TypedVariableExpr {} -> []
    TypedLambdaExpr _ _ _ body -> [body]
    TypedOperatorValueExpr {} -> []
    TypedListExpr _ expressions -> expressions
    TypedTupleExpr _ expressions -> expressions
    TypedApplyExpr _ function argument -> [function, argument]
    TypedTypeApplicationExpr _ function _ _ -> [function]
    TypedIfExpr _ condition thenExpression elseExpression -> [condition, thenExpression, elseExpression]
    TypedPatternCaseExpr _ scrutinee arms -> scrutinee : concatMap armExpressions arms
    TypedBinaryExpr _ _ left right -> [left, right]
    TypedLeftSectionExpr _ left _ -> [left]
    TypedRightSectionExpr _ _ right -> [right]
    TypedBlockExpr {} -> []
  where
    armExpressions (TypedCaseArm _ guard result) = maybeToList guard <> [result]

validateApplication :: TypedCoreValidationPath -> TypedNodeInfo -> TypedExpr -> TypedExpr -> [TypedCoreValidationFailure]
validateApplication path (TypedNodeInfo resultType resultRecipe _ resultSelections) function argument =
  typeFailures <> candidateProgressionFailures
  where
    typeFailures =
      case functionType of
        TypedFunctionType expectedArgument expectedResult ->
          argumentFailures expectedArgument <> resultFailures expectedResult
        actualFunctionType ->
          [ failure
              path
              TypedApplicationFunctionMismatch
              (TypedTypeDetail (TypedFunctionType (typedNodeType (typedExpressionInfo argument)) resultType) actualFunctionType)
          ]
    actualArgument = typedNodeType (typedExpressionInfo argument)
    argumentInfo = typedExpressionInfo argument
    argumentFailures expected
      | not (applicationTypesCompatible expected actualArgument) =
          [failure path TypedApplicationArgumentMismatch (TypedTypeDetail expected actualArgument)]
      | callableRecipeCompatible functionType functionRecipe,
        Just expectedArgumentRecipe <- callableArgumentRecipe functionRecipe =
          recipeContractFailures path TypedApplicationArgumentMismatch expectedArgumentRecipe argumentInfo
      | otherwise = []
    resultFailures expected
      | not (applicationTypesCompatible expected resultType) =
          [failure path TypedApplicationResultMismatch (TypedTypeDetail expected resultType)]
      | callableRecipeCompatible functionType functionRecipe,
        typeRecipeCompatible resultType resultRecipe,
        Just expectedResultRecipe <- applicationResultRecipe functionRecipe,
        expectedResultRecipe /= resultRecipe =
          [failure path TypedApplicationResultMismatch (TypedRecipeDetail expectedResultRecipe resultRecipe)]
      | otherwise = []
    functionInfo = typedExpressionInfo function
    functionType = typedNodeType functionInfo
    functionRecipe = typedNodeRecipe functionInfo
    functionSelections =
      case functionInfo of
        TypedNodeInfo _ _ _ selections -> selections
    candidateProgressionFailures =
      missingCandidateProgressionFailures
        <> selectedEvidenceProgressionFailures
        <> invalidCandidateSelectionFailures
    missingCandidateProgressionFailures =
      [ failure path TypedMissingEvidence (TypedTextDetail (capabilityConstraintLabel constraint))
      | TypedEvidenceCandidates constraint candidates <- functionSelections,
        not (any (progressesCandidateObligation constraint candidates) resultSelections)
      ]

    progressesCandidateObligation constraint candidates selection =
      case selection of
        TypedEvidenceCandidates resultConstraint resultCandidates ->
          resultConstraint == constraint
            && resultCandidates == candidates
        TypedSelectedEvidence (TypedEvidenceUse Nothing resultConstraint _ _) ->
          resultConstraint == constraint
        _ -> False
    selectedEvidenceProgressionFailures =
      [ failure path TypedMethodSelectionMismatch (TypedImplDetail selectedImpl)
      | selection@(TypedSelectedEvidence (TypedEvidenceUse _ _ selectedImpl _)) <- functionSelections,
        selection `notElem` resultSelections
      ]
    invalidCandidateSelectionFailures =
      [ failure path TypedMethodSelectionMismatch (TypedImplDetail selectedImpl)
      | TypedSelectedEvidence
          (TypedEvidenceUse Nothing constraint selectedImpl maybeSelectedMethod) <-
          resultSelections,
        let selectedCandidate =
              TypedEvidenceCandidate selectedImpl maybeSelectedMethod,
        let matchingCandidateSets =
              [ candidates
              | TypedEvidenceCandidates candidateConstraint candidates <- functionSelections,
                candidateConstraint == constraint
              ],
        not (null matchingCandidateSets),
        any (selectedCandidate `notElem`) matchingCandidateSets
      ]

applicationResultRecipe :: TypedRepresentationRecipe -> Maybe TypedRepresentationRecipe
applicationResultRecipe recipe =
  case recipe of
    TypedClosureRecipe (_ : remainingArguments) resultRecipe ->
      Just (stageRemainingArguments remainingArguments resultRecipe)
    _ -> Nothing
  where
    stageRemainingArguments remainingArguments resultRecipe =
      case remainingArguments of
        [] -> resultRecipe
        argumentRecipe : rest ->
          TypedClosureRecipe [argumentRecipe] (stageRemainingArguments rest resultRecipe)

callableArgumentRecipe :: TypedRepresentationRecipe -> Maybe TypedRepresentationRecipe
callableArgumentRecipe recipe =
  case recipe of
    TypedClosureRecipe (argumentRecipe : _) _ -> Just argumentRecipe
    _ -> Nothing

callableResultRecipe :: TypedRepresentationRecipe -> Maybe TypedRepresentationRecipe
callableResultRecipe recipe =
  case recipe of
    TypedClosureRecipe (_ : remainingArguments) resultRecipe ->
      Just
        ( case remainingArguments of
            [] -> resultRecipe
            _ -> TypedClosureRecipe remainingArguments resultRecipe
        )
    _ -> Nothing

applicationTypesCompatible :: TypedType -> TypedType -> Bool
applicationTypesCompatible expected actual =
  normalizeDefaultScalarAliases expected == normalizeDefaultScalarAliases actual

normalizeDefaultScalarAliases :: TypedType -> TypedType
normalizeDefaultScalarAliases typeValue =
  case typeValue of
    TypedIntType -> TypedNumericType TypedInt64Type
    TypedFloatType -> TypedNumericType TypedFloat64Type
    TypedListType elementType ->
      TypedListType (normalizeDefaultScalarAliases elementType)
    TypedTupleType elementTypes ->
      TypedTupleType (map normalizeDefaultScalarAliases elementTypes)
    TypedDataType name arguments ->
      TypedDataType name (map normalizeDefaultScalarAliases arguments)
    TypedFunctionType argumentType resultType ->
      TypedFunctionType
        (normalizeDefaultScalarAliases argumentType)
        (normalizeDefaultScalarAliases resultType)
    other -> other

validateConditional :: TypedCoreValidationPath -> TypedNodeInfo -> TypedExpr -> TypedExpr -> TypedExpr -> [TypedCoreValidationFailure]
validateConditional path resultInfo@(TypedNodeInfo resultType _ _ _) condition thenExpression elseExpression =
  conditionFailures <> branchFailures <> resultFailures
  where
    thenInfo = typedExpressionInfo thenExpression
    elseInfo = typedExpressionInfo elseExpression
    conditionType = typedNodeType (typedExpressionInfo condition)
    thenType = typedNodeType thenInfo
    elseType = typedNodeType elseInfo
    conditionFailures
      | conditionType == TypedBoolType = []
      | otherwise = [failure path TypedConditionalConditionMismatch (TypedTypeDetail TypedBoolType conditionType)]
    branchFailures
      | thenType /= elseType = [failure path TypedConditionalBranchMismatch (TypedTypeDetail thenType elseType)]
      | nodeInfoHasCompatibleIntrinsicContract thenInfo =
          recipeContractFailures path TypedConditionalBranchMismatch (typedNodeRecipe thenInfo) elseInfo
      | otherwise = []
    resultFailures
      | thenType /= elseType || resultType /= thenType =
          if thenType == elseType
            then [failure path TypedConditionalBranchMismatch (TypedTypeDetail thenType resultType)]
            else []
      | typedNodeRecipe thenInfo /= typedNodeRecipe elseInfo = []
      | nodeInfoHasCompatibleIntrinsicContract thenInfo,
        nodeInfoHasCompatibleIntrinsicContract elseInfo =
          recipeContractFailures path TypedConditionalBranchMismatch (typedNodeRecipe thenInfo) resultInfo
      | otherwise = []

validateCase :: ModuleContext -> [Int] -> [Int] -> TypedCoreValidationPath -> TypedNodeInfo -> TypedExpr -> [TypedCaseArm] -> [TypedCoreValidationFailure]
validateCase context statementLocation expressionPath path resultInfo@(TypedNodeInfo resultType _ _ _) scrutinee arms =
  emptyArmFailures <> concatMap (uncurry validateArm) (zip [0 ..] arms)
  where
    scrutineeInfo = typedExpressionInfo scrutinee
    scrutineeContract = ValueContract (typedNodeType scrutineeInfo) (typedNodeRecipe scrutineeInfo)
    emptyArmFailures
      | null arms = [failure path TypedPatternShapeMismatch (TypedArityDetail 1 0)]
      | otherwise = []
    validateArm armIndex (TypedCaseArm patternValue maybeGuard resultExpression) =
      duplicatePatternNameFailures armIndex patternValue
        <> validatePattern context statementLocation (expressionPath <> [armIndex]) scrutineeContract patternValue
        <> guardFailures armIndex maybeGuard
        <> resultFailures armIndex resultExpression
    duplicatePatternNameFailures armIndex =
      snd . foldl' duplicateNameStep (Set.empty, []) . patternBinderContract
      where
        armPath =
          TypedPatternPath
            (moduleContextPath context)
            statementLocation
            (expressionPath <> [armIndex])
        duplicateNameStep (seen, failures) (PatternBinderContract binderId name _ _)
          | Set.member name seen =
              (seen, failures <> [failure armPath TypedDuplicateBinder (TypedBinderDetail binderId)])
          | otherwise = (Set.insert name seen, failures)
    guardFailures _ Nothing = []
    guardFailures armIndex (Just guard)
      | typedNodeType (typedExpressionInfo guard) == TypedBoolType = []
      | otherwise =
          [ failure
              (TypedPatternPath (moduleContextPath context) statementLocation (expressionPath <> [armIndex]))
              TypedPatternGuardMismatch
              (TypedTypeDetail TypedBoolType (typedNodeType (typedExpressionInfo guard)))
          ]
    resultFailures armIndex resultExpression
      | typedNodeType armInfo /= resultType =
          [ failure
              armPath
              TypedPatternArmResultMismatch
              (TypedTypeDetail resultType (typedNodeType armInfo))
          ]
      | nodeInfoHasCompatibleIntrinsicContract resultInfo =
          recipeContractFailures armPath TypedPatternArmResultMismatch (typedNodeRecipe resultInfo) armInfo
      | otherwise = []
      where
        armInfo = typedExpressionInfo resultExpression
        armPath = TypedPatternPath (moduleContextPath context) statementLocation (expressionPath <> [armIndex])

nodeContractFailures :: TypedCoreValidationPath -> TypedCoreValidationKind -> TypedNodeInfo -> TypedNodeInfo -> [TypedCoreValidationFailure]
nodeContractFailures path kind expected actual
  | typedNodeType expected /= typedNodeType actual = [failure path kind (TypedTypeDetail (typedNodeType expected) (typedNodeType actual))]
  | typedNodeRecipe expected /= typedNodeRecipe actual = [failure path kind (TypedRecipeDetail (typedNodeRecipe expected) (typedNodeRecipe actual))]
  | otherwise = []

validateOperatorRef :: ModuleContext -> TypedCoreValidationPath -> TypedOperatorRef -> [TypedCoreValidationFailure]
validateOperatorRef context path operator =
  case operator of
    TypedBuiltinOperator symbol
      | builtinOperatorHasTypedRule symbol -> []
      | otherwise -> [failure path TypedBindingValueMismatch (TypedTextDetail symbol)]
    TypedResolvedOperator name symbol ->
      validateVisibleNameInNamespaces [TypedValueNamespace] context path name
        <> if resolvedOperatorMatchesSymbol name symbol
          then []
          else [failure path TypedBindingValueMismatch (TypedTextDetail symbol)]

resolvedOperatorMatchesSymbol :: TypedCoreName -> Text -> Bool
resolvedOperatorMatchesSymbol name symbol =
  isValidUserOperatorSymbol symbol
    && case name of
      TypedGeneratedName (TypedOperatorBinding bindingName) ->
        bindingName == operatorBindingIdentifierText symbol
      _ -> False

validateOperatorValue :: ModuleContext -> TypedCoreValidationPath -> Bool -> Int -> TypedNodeInfo -> TypedOperatorRef -> [TypedCoreValidationFailure]
validateOperatorValue context path requireStagedCallableRecipe directCalleeArgumentCount info operator =
  case operator of
    TypedBuiltinOperator symbol ->
      validateOperatorRef context path operator
        <> validateBuiltinOperatorValue context path requireStagedCallableRecipe directCalleeArgumentCount symbol info
    TypedResolvedOperator {} ->
      validateOperatorRef context path operator
        <> ( case operatorValueContract context path info operator of
               (contractFailures, Just contract) ->
                 contractFailures <> validateValueContract path info contract
               (contractFailures, Nothing) -> contractFailures
           )
        <> resolvedOperatorCallableShapeFailures context path directCalleeArgumentCount operator

resolvedOperatorCallableShapeFailures :: ModuleContext -> TypedCoreValidationPath -> Int -> TypedOperatorRef -> [TypedCoreValidationFailure]
resolvedOperatorCallableShapeFailures context path directCalleeArgumentCount operator =
  case operator of
    TypedResolvedOperator name _ ->
      maybe [] (validateDirectCallableSchemeUse path directCalleeArgumentCount) (lookupSchemeByName context name)
    _ -> []

validateBinaryOperator :: ModuleContext -> TypedCoreValidationPath -> TypedNodeInfo -> TypedOperatorRef -> TypedExpr -> TypedExpr -> [TypedCoreValidationFailure]
validateBinaryOperator context path info operator left right =
  case operator of
    TypedBuiltinOperator "$" ->
      validateOperatorRef context path operator
        <> validateApplication path info left right
    TypedBuiltinOperator symbol ->
      validateOperatorRef context path operator
        <> validateBuiltinOperatorApplication context path symbol (typedNodeType (typedExpressionInfo left)) (typedNodeType (typedExpressionInfo right)) (typedNodeType info)
    TypedResolvedOperator {} ->
      validateOperatorRef context path operator
        <> ( case operatorValueContract context path info operator of
               (contractFailures, Just (ValueContract operatorType@(TypedFunctionType expectedLeft (TypedFunctionType expectedRight expectedResult)) operatorRecipe)) ->
                 contractFailures
                   <> typeMismatchFailure TypedApplicationArgumentMismatch expectedLeft (typedNodeType (typedExpressionInfo left))
                   <> typeMismatchFailure TypedApplicationArgumentMismatch expectedRight (typedNodeType (typedExpressionInfo right))
                   <> typeMismatchFailure TypedApplicationResultMismatch expectedResult (typedNodeType info)
                   <> binaryRecipeFailures operatorType operatorRecipe expectedLeft expectedRight expectedResult
               (contractFailures, Just (ValueContract actualType _)) ->
                 contractFailures
                   <> [ failure
                          path
                          TypedApplicationFunctionMismatch
                          ( TypedTypeDetail
                              (TypedFunctionType (typedNodeType (typedExpressionInfo left)) (TypedFunctionType (typedNodeType (typedExpressionInfo right)) (typedNodeType info)))
                              actualType
                          )
                      ]
               (contractFailures, Nothing) -> contractFailures
           )
        <> resolvedOperatorCallableShapeFailures context path 2 operator
  where
    typeMismatchFailure kind expected actual
      | expected == actual = []
      | otherwise = [failure path kind (TypedTypeDetail expected actual)]
    binaryRecipeFailures operatorType operatorRecipe expectedLeft expectedRight expectedResult
      | not (callableRecipeCompatible operatorType operatorRecipe) = []
      | otherwise =
          case applicationResultRecipe operatorRecipe of
            Just afterLeftRecipe ->
              case (callableArgumentRecipe operatorRecipe, callableArgumentRecipe afterLeftRecipe, applicationResultRecipe afterLeftRecipe) of
                (Just expectedLeftRecipe, Just expectedRightRecipe, Just expectedResultRecipe) ->
                  valueRecipeFailures TypedApplicationArgumentMismatch expectedLeft expectedLeftRecipe (typedExpressionInfo left)
                    <> valueRecipeFailures TypedApplicationArgumentMismatch expectedRight expectedRightRecipe (typedExpressionInfo right)
                    <> valueRecipeFailures TypedApplicationResultMismatch expectedResult expectedResultRecipe info
                _ -> []
            Nothing -> []
    valueRecipeFailures kind expectedType expectedRecipeValue actualInfo
      | expectedType == typedNodeType actualInfo =
          recipeContractFailures path kind expectedRecipeValue actualInfo
      | otherwise = []

validateLeftSectionOperator :: ModuleContext -> TypedCoreValidationPath -> TypedNodeInfo -> TypedExpr -> TypedOperatorRef -> [TypedCoreValidationFailure]
validateLeftSectionOperator context path info left operator =
  operatorFailures
    <> resolvedOperatorCallableShapeFailures context path 1 operator
    <> validateStagedCallableValueRecipe path info
  where
    operatorFailures =
      case operator of
        TypedBuiltinOperator symbol ->
          validateOperatorRef context path operator
            <> case typedNodeType info of
              TypedFunctionType rightType resultType ->
                validateBuiltinOperatorApplication context path symbol (typedNodeType (typedExpressionInfo left)) rightType resultType
              actualType -> [failure path TypedApplicationFunctionMismatch (TypedTypeDetail (TypedFunctionType (typedNodeType (typedExpressionInfo left)) actualType) actualType)]
        TypedResolvedOperator {} ->
          validateOperatorRef context path operator
            <> case operatorValueContract context path info operator of
              (contractFailures, Just (ValueContract operatorType@(TypedFunctionType expectedLeft remainder@(TypedFunctionType _ _)) operatorRecipe)) ->
                contractFailures
                  <> mismatch TypedApplicationArgumentMismatch expectedLeft (typedNodeType (typedExpressionInfo left))
                  <> mismatch TypedApplicationResultMismatch remainder (typedNodeType info)
                  <> leftOperandRecipeFailures operatorType operatorRecipe expectedLeft
              (contractFailures, Just (ValueContract actualType _)) ->
                contractFailures
                  <> [failure path TypedApplicationFunctionMismatch (TypedTypeDetail (TypedFunctionType (typedNodeType (typedExpressionInfo left)) (typedNodeType info)) actualType)]
              (contractFailures, Nothing) -> contractFailures
    mismatch kind expected actual
      | expected == actual = []
      | otherwise = [failure path kind (TypedTypeDetail expected actual)]
    leftOperandRecipeFailures operatorType operatorRecipe expectedType
      | not (callableRecipeCompatible operatorType operatorRecipe) = []
      | expectedType /= typedNodeType leftInfo = []
      | Just expectedRecipeValue <- callableArgumentRecipe operatorRecipe =
          recipeContractFailures path TypedApplicationArgumentMismatch expectedRecipeValue leftInfo
      | otherwise = []
    leftInfo = typedExpressionInfo left

validateRightSectionOperator :: ModuleContext -> TypedCoreValidationPath -> TypedNodeInfo -> TypedOperatorRef -> TypedExpr -> [TypedCoreValidationFailure]
validateRightSectionOperator context path info operator right =
  operatorFailures
    <> resolvedOperatorCallableShapeFailures context path 1 operator
    <> validateStagedCallableValueRecipe path info
  where
    operatorFailures =
      case operator of
        TypedBuiltinOperator symbol ->
          validateOperatorRef context path operator
            <> case typedNodeType info of
              TypedFunctionType leftType resultType ->
                validateBuiltinOperatorApplication context path symbol leftType (typedNodeType (typedExpressionInfo right)) resultType
              actualType -> [failure path TypedApplicationFunctionMismatch (TypedTypeDetail (TypedFunctionType actualType (typedNodeType (typedExpressionInfo right))) actualType)]
        TypedResolvedOperator {} ->
          validateOperatorRef context path operator
            <> case operatorValueContract context path info operator of
              (contractFailures, Just (ValueContract operatorType@(TypedFunctionType expectedLeft (TypedFunctionType expectedRight expectedResult)) operatorRecipe)) ->
                let expectedSectionType = TypedFunctionType expectedLeft expectedResult
                 in contractFailures
                      <> mismatch TypedApplicationArgumentMismatch expectedRight (typedNodeType (typedExpressionInfo right))
                      <> mismatch TypedApplicationResultMismatch expectedSectionType (typedNodeType info)
                      <> rightOperandRecipeFailures operatorType operatorRecipe expectedRight
              (contractFailures, Just (ValueContract actualType _)) ->
                contractFailures
                  <> [failure path TypedApplicationFunctionMismatch (TypedTypeDetail (TypedFunctionType (typedNodeType info) (typedNodeType (typedExpressionInfo right))) actualType)]
              (contractFailures, Nothing) -> contractFailures
    mismatch kind expected actual
      | expected == actual = []
      | otherwise = [failure path kind (TypedTypeDetail expected actual)]
    rightOperandRecipeFailures operatorType operatorRecipe expectedType
      | not (callableRecipeCompatible operatorType operatorRecipe) = []
      | expectedType /= typedNodeType rightInfo = []
      | Just remainingRecipe <- applicationResultRecipe operatorRecipe,
        Just expectedRecipeValue <- callableArgumentRecipe remainingRecipe =
          recipeContractFailures path TypedApplicationArgumentMismatch expectedRecipeValue rightInfo
      | otherwise = []
    rightInfo = typedExpressionInfo right

builtinOperatorHasTypedRule :: Text -> Bool
builtinOperatorHasTypedRule symbol =
  symbol `elem` ["+", "-", "*", "/", "<", "<=", ">", ">=", "==", "!=", "$"]

validateBuiltinOperatorValue :: ModuleContext -> TypedCoreValidationPath -> Bool -> Int -> Text -> TypedNodeInfo -> [TypedCoreValidationFailure]
validateBuiltinOperatorValue context path requireStagedCallableRecipe directCalleeArgumentCount symbol info
  | not (builtinOperatorHasTypedRule symbol) = []
  | otherwise =
      typeFailures <> recipeFailures
  where
    operatorType = typedNodeType info
    typeFailures =
      case operatorType of
        TypedFunctionType leftType (TypedFunctionType rightType resultType) ->
          validateBuiltinOperatorApplication context path symbol leftType rightType resultType
        _ -> [failure path TypedApplicationFunctionMismatch (TypedTextDetail symbol)]
    recipeFailures
      | null typeFailures =
          case expectedNativeCallableUseRecipe requireStagedCallableRecipe directCalleeArgumentCount operatorType of
            Just expectedRecipeValue ->
              validateValueContract path info (ValueContract operatorType expectedRecipeValue)
            Nothing -> [failure path TypedBindingValueMismatch (TypedTextDetail symbol)]
      | otherwise = []

validateBuiltinOperatorApplication :: ModuleContext -> TypedCoreValidationPath -> Text -> TypedType -> TypedType -> TypedType -> [TypedCoreValidationFailure]
validateBuiltinOperatorApplication context path symbol leftType rightType resultType
  | not (builtinOperatorHasTypedRule symbol) = []
  | symbol == "$" =
      typeFailure TypedApplicationFunctionMismatch (TypedFunctionType rightType resultType) leftType
  | symbol `elem` ["+", "-", "*", "/"] =
      sameOperandFailure
        <> numericOperandFailure
        <> typeFailure TypedApplicationResultMismatch leftType resultType
  | symbol `elem` ["<", "<=", ">", ">="] =
      sameOperandFailure
        <> numericOperandFailure
        <> typeFailure TypedApplicationResultMismatch TypedBoolType resultType
  | otherwise =
      sameOperandFailure
        <> equalityOperandFailure
        <> typeFailure TypedApplicationResultMismatch TypedBoolType resultType
  where
    sameOperandFailure = typeFailure TypedApplicationArgumentMismatch leftType rightType
    numericOperandFailure
      | numericOperatorType context symbol leftType = []
      | otherwise = [failure path TypedBindingValueMismatch (TypedTextDetail symbol)]
    equalityOperandFailure
      | symbol `elem` ["==", "!="] && not (strictEqualityOperandTypeSupported context leftType) =
          [failure path TypedBindingValueMismatch (TypedTypeDetail TypedBoolType leftType)]
      | otherwise = []
    typeFailure kind expected actual
      | expected == actual = []
      | otherwise = [failure path kind (TypedTypeDetail expected actual)]

numericOperatorType :: ModuleContext -> Text -> TypedType -> Bool
numericOperatorType context symbol typeValue =
  case typeValue of
    TypedIntType -> True
    TypedFloatType -> True
    TypedNumericType _ -> True
    TypedTypeParameterType _ ->
      any activeConstraintSupports (moduleContextPrimitiveConstraints context)
    _ -> False
  where
    activeConstraintSupports constraint =
      case constraint of
        TypedNumericPrimitiveConstraint numericConstraint targetType ->
          targetType == typeValue
            && numericConstraintSupportsOperator symbol numericConstraint
        _ -> False

numericConstraintSupportsOperator :: Text -> TypedNumericConstraint -> Bool
numericConstraintSupportsOperator symbol numericConstraint
  | symbol `elem` ["+", "-", "*", "/"] =
      numericConstraint
        `elem` [ TypedRuntimeArithmeticNumericConstraint,
                 TypedIntegralNumericConstraint
               ]
        || integralLiteralConstraint numericConstraint
  | symbol `elem` ["<", "<=", ">", ">="] =
      numericConstraint
        `elem` [ TypedRuntimeArithmeticNumericConstraint,
                 TypedRuntimeComparisonNumericConstraint,
                 TypedIntegralNumericConstraint
               ]
        || integralLiteralConstraint numericConstraint
  | otherwise = False
  where
    integralLiteralConstraint TypedIntegralLiteralNumericConstraint {} = True
    integralLiteralConstraint _ = False

operatorValueContract :: ModuleContext -> TypedCoreValidationPath -> TypedNodeInfo -> TypedOperatorRef -> ([TypedCoreValidationFailure], Maybe ValueContract)
operatorValueContract _ _ _ (TypedBuiltinOperator _) = ([], Nothing)
operatorValueContract context path info (TypedResolvedOperator name _) =
  case lookupSchemeByName context name of
    Nothing -> ([], Nothing)
    Just scheme@(TypedScheme owner parameters evidenceParameters _ _ _ _) ->
      let matchingOwnerInstantiation =
            find (matchingInstantiation owner parameters) (nodeInfoInstantiations info)
          instantiationFailures
            | not (null parameters && null evidenceParameters),
              matchingOwnerInstantiation == Nothing =
                [failure path TypedInstantiationMismatch (TypedBinderDetail owner)]
            | otherwise = []
          missingEvidenceFailures
            | null parameters,
              not (null evidenceParameters),
              matchingOwnerInstantiation == Nothing =
                [ failure path TypedMissingEvidence (TypedEvidenceParameterDetail parameterId)
                | TypedEvidenceParameter parameterId _ <- evidenceParameters
                ]
            | otherwise = []
          requirementFailures = instantiationFailures <> missingEvidenceFailures
       in (requirementFailures, schemeValueContract context info scheme)

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
          | any (dataInterfaceConstructorMatches exportedName) datas -> []
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
