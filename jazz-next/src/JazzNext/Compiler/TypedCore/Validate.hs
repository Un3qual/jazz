{-# LANGUAGE OverloadedStrings #-}

-- | Complete structural validation for the semantic typed-core boundary.
-- Validation is deliberately independent of inference, evaluation, and
-- lowering: it accepts an already-constructed contract value and reports all
-- invariant failures in stable structural order.
module JazzNext.Compiler.TypedCore.Validate
  ( validateTypedProgram,
  )
where

import Data.List (find, nub, sort, sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Ratio ((%))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import JazzNext.Compiler.AST
  ( NumericType (..),
  )
import JazzNext.Compiler.BuiltinCatalog
  ( BuiltinSymbol (..),
    builtinSymbolNumericConversionTarget,
    isBuiltinSymbolName,
    isKernelBuiltinSymbolName,
    lookupBuiltinSymbol,
    lookupKernelBuiltinSymbol,
    numericTypeFloatMax,
  )
import JazzNext.Compiler.CapabilityFacts (splitQualifiedMethodKey)
import JazzNext.Compiler.Name (operatorBindingIdentifierText)
import JazzNext.Compiler.TypedCore

data ModuleContext = ModuleContext
  { moduleContextPath :: [Text],
    moduleContextVisibleModules :: Set [Text],
    moduleContextSchemes :: Map TypedBinderId TypedScheme,
    moduleContextStatementIndices :: Map [Int] Int,
    moduleContextVisibleNames :: Set ResolvedNameKey,
    moduleContextVisibleImpls :: Set TypedImplId,
    moduleContextImplMethods :: Map TypedImplId (Set Text),
    moduleContextDataArities :: Map ResolvedNameKey Int,
    moduleContextDataContracts :: Map ResolvedNameKey DataContract,
    moduleContextConstructorContracts :: Map ResolvedNameKey ConstructorContract,
    moduleContextCapabilityContracts :: Map ResolvedNameKey CapabilityContract,
    moduleContextLexicalContracts :: Map ResolvedNameKey ValueContract,
    moduleContextTypeScope :: Set TypedTypeParameterId,
    moduleContextPrimitiveConstraints :: [TypedPrimitiveConstraint]
  }

data ResolvedNameKey
  = ResolvedNameKey [Text] TypedNameNamespace Text
  | GeneratedNameKey TypedGeneratedNameKind
  deriving (Eq, Ord, Show)

data BinderOccurrence = BinderOccurrence TypedCoreValidationPath TypedBinderId

data PatternBinderContract = PatternBinderContract TypedBinderId TypedCoreName TypedType TypedRepresentationRecipe

data ValueContract = ValueContract TypedType TypedRepresentationRecipe

data ConstructorContract = ConstructorContract TypedBinderId ResolvedNameKey [TypedTypeParameterId] [TypedType]

data DataContract = DataContract [TypedTypeParameterId] [[TypedType]]

data CapabilityContract = CapabilityContract [TypedTypeParameterId] (Map Text TypedScheme)

data InstantiationContract = InstantiationContract TypedBinderId [TypedTypeParameterId] [TypedPrimitiveConstraint]

validateTypedProgram :: TypedProgram -> [TypedCoreValidationFailure]
validateTypedProgram (TypedProgram prelude modules entryModule) =
  duplicateModuleFailures allModules
    <> importCycleFailures moduleTable allModules
    <> moduleOrderFailures moduleTable allModules
    <> unknownEntryFailure
    <> maybe [] (validateModule moduleTable prelude True) prelude
    <> concatMap (validateModule moduleTable prelude False) modules
  where
    allModules = maybeToList prelude <> modules
    allModulePaths = Set.fromList (map typedModulePath allModules)
    moduleTable = Map.fromList [(typedModulePath moduleValue, moduleValue) | moduleValue <- allModules]
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
  | TypedModule modulePath _ imports _ _ _ _ <- modules,
    importPath <- nub [path | TypedResolvedImport _ path _ _ <- imports],
    modulePathReachable moduleTable Set.empty importPath modulePath
  ]

modulePathReachable :: Map [Text] TypedModule -> Set [Text] -> [Text] -> [Text] -> Bool
modulePathReachable moduleTable seen currentPath targetPath
  | currentPath == targetPath = True
  | Set.member currentPath seen = False
  | otherwise =
      case Map.lookup currentPath moduleTable of
        Nothing -> False
        Just (TypedModule _ _ imports _ _ _ _) ->
          any
            (\(TypedResolvedImport _ nextPath _ _) -> modulePathReachable moduleTable (Set.insert currentPath seen) nextPath targetPath)
            imports

moduleOrderFailures :: Map [Text] TypedModule -> [TypedModule] -> [TypedCoreValidationFailure]
moduleOrderFailures moduleTable = go Set.empty
  where
    go _ [] = []
    go precedingPaths (TypedModule modulePath _ imports _ _ _ _ : remainingModules) =
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
validateModule moduleTable prelude isPrelude moduleValue@(TypedModule modulePath sourcePath imports _ _ statements moduleInfo) =
  validateSourcePath modulePath sourcePath
    <> validateResolvedImports moduleTable modulePath imports
    <> validateModuleInterface moduleTable moduleValue
    <> duplicateDeclarationFailures context (zip (map pure [0 ..]) statements)
    <> duplicateBinderFailures context (zip (map pure [0 ..]) statements)
    <> validateStatementsInOrder baseContext (zip (map pure [0 ..]) statements)
    <> validateModuleInfo context moduleValidationPath statements moduleInfo
    <> validateModuleResult modulePath statements moduleInfo
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
    visibleNames =
      Set.fromList (concatMap interfaceNameKeys visibleExternalModules)
    externalImplEntries = concatMap interfaceImplEntries visibleExternalModules
    visibleImpls = Set.fromList (map fst externalImplEntries)
    implMethods = Map.fromListWith Set.union externalImplEntries
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
    externalDataMetadata =
      interfaceDataMetadataDeclarations moduleTable visibleExternalModules
    statementIndices =
      Map.fromList
        ( zip
            (topLevelStatementLocations statements <> concatMap (uncurry nestedStatementLocations) (zip (map pure [0 ..]) statements))
            [0 ..]
        )
    externalContext =
      ModuleContext
        { moduleContextPath = modulePath,
          moduleContextVisibleModules = visibleModules,
          moduleContextSchemes = schemes,
          moduleContextStatementIndices = statementIndices,
          moduleContextVisibleNames = visibleNames,
          moduleContextVisibleImpls = visibleImpls,
          moduleContextImplMethods = implMethods,
          moduleContextDataArities = dataArities,
          moduleContextDataContracts = dataContracts,
          moduleContextConstructorContracts = constructorContracts,
          moduleContextCapabilityContracts = capabilityContracts,
          moduleContextLexicalContracts = Map.empty,
          moduleContextTypeScope = Set.empty,
          moduleContextPrimitiveConstraints = []
        }
    moduleMetadataStatements =
      [ statement
      | statement <- statements,
        case statement of
          TypedDataStatement {} -> True
          TypedClassStatement {} -> True
          TypedImplStatement {} -> True
          _ -> False
      ]
    baseContext = withBlockDeclarations moduleMetadataStatements externalContext
    context = withBlockDeclarations statements externalContext

validateResolvedImports :: Map [Text] TypedModule -> [Text] -> [TypedResolvedImport] -> [TypedCoreValidationFailure]
validateResolvedImports moduleTable modulePath imports =
  concatMap validateImport imports
  where
    validateImport (TypedResolvedImport _ importPath _ selectedNames) =
      case Map.lookup importPath moduleTable of
        Nothing ->
          [ failure
              (TypedModulePath modulePath)
              TypedModuleInterfaceMismatch
              (TypedTextDetail (renderModulePath importPath))
          ]
        Just importedModule ->
          [ failure
              (TypedModulePath modulePath)
              TypedModuleInterfaceMismatch
              (TypedTextDetail selectedName)
          | selectedName <- maybe [] id selectedNames,
            not (moduleExportsInterfaceName selectedName importedModule)
          ]

moduleExportsInterfaceName :: Text -> TypedModule -> Bool
moduleExportsInterfaceName expected (TypedModule _ _ _ exports interface _ _) =
  any matchesExport exports
  where
    matchesExport export@(TypedModuleExport _ exportedName) =
      exportedName == expected && interfaceContainsExport export interface

interfaceContainsExport :: TypedModuleExport -> TypedModuleInterface -> Bool
interfaceContainsExport (TypedModuleExport namespace expected) (TypedModuleInterface values datas classes _) =
  case namespace of
    TypedValueNamespace -> any (interfaceNameMatches expected) values || any (classInterfaceMethodMatches expected) classes
    TypedTypeNamespace -> any (dataInterfaceNameMatches expected) datas || any (classInterfaceNameMatches expected) classes
    TypedConstructorNamespace -> any (dataInterfaceConstructorMatches expected) datas
    TypedCapabilityNamespace -> any (classInterfaceNameMatches expected) classes

validateModuleResult :: [Text] -> [TypedStatement] -> TypedNodeInfo -> [TypedCoreValidationFailure]
validateModuleResult modulePath statements moduleInfo =
  case reverse statements of
    TypedExpressionStatement _ terminal : _
      | nodeInfoHasValidIntrinsicContract moduleInfo && nodeInfoHasValidIntrinsicContract (expressionInfo terminal) ->
          nodeContractFailures
            (TypedModulePath modulePath)
            TypedModuleResultMismatch
            moduleInfo
            (expressionInfo terminal)
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
      | expressionInfo terminal == moduleInfo -> []
    _ -> validateNodeInfo context path Set.empty Nothing moduleInfo

nodeInfoHasValidIntrinsicContract :: TypedNodeInfo -> Bool
nodeInfoHasValidIntrinsicContract (TypedNodeInfo typeValue recipe _ _) =
  validRecipeWidth recipe && expectedRecipe typeValue == Just recipe

validateSourcePath :: [Text] -> TypedSourcePath -> [TypedCoreValidationFailure]
validateSourcePath modulePath (TypedSourcePath sourcePath)
  | validRelativeSourcePath sourcePath = []
  | otherwise =
      [ failure
          (TypedModulePath modulePath)
          TypedInvalidSourcePath
          (TypedTextDetail sourcePath)
      ]

validRelativeSourcePath :: Text -> Bool
validRelativeSourcePath sourcePath =
  not (Text.null sourcePath)
    && not (Text.isPrefixOf "/" sourcePath)
    && not (Text.any (== '\\') sourcePath)
    && not (isDriveAbsolute sourcePath)
    && all validSegment (Text.splitOn "/" sourcePath)
  where
    validSegment segment = not (Text.null segment) && segment /= "." && segment /= ".."
    isDriveAbsolute path =
      case Text.unpack path of
        _ : ':' : _ -> True
        _ -> False

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
    occurrences = concatMap declarationOccurrences statements
    declarationOccurrences (statementLocation, statement) =
      [ ( statementPath,
          key,
          name,
          identity
        )
      | (name, identity) <- duplicateCheckedDeclarations statement,
        key <- maybeToList (resolvedNameKey (moduleContextPath context) name)
      ]
      where
        statementPath = TypedStatementPath (moduleContextPath context) (statementIndexFor context statementLocation)
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
      [ ( TypedStatementPath (moduleContextPath context) (statementIndexFor context statementLocation),
          implId
        )
      | (statementLocation, TypedImplStatement (TypedImplDeclaration _ implId _)) <- statements
      ]
    implStep (seen, failures) (path, implId)
      | Set.member implId seen =
          (seen, failures <> [failure path TypedDuplicateDeclaration (TypedImplDetail implId)])
      | otherwise = (Set.insert implId seen, failures)

duplicateCheckedDeclarations :: TypedStatement -> [(TypedCoreName, Maybe TypedBinderId)]
duplicateCheckedDeclarations statement =
  case statement of
    TypedLetStatement {} -> []
    TypedSignatureStatement binderId name _ _ -> [(name, Just binderId)]
    TypedDataStatement (TypedDataDeclaration _ name _ constructors) ->
      (name, Nothing)
        : [(constructorName, Just binderId) | TypedConstructorDeclaration binderId constructorName _ _ <- constructors]
    TypedClassStatement (TypedClassDeclaration _ name _ methods) ->
      (name, Nothing)
        : [ (methodName, Just binderId)
          | TypedMethodSignature methodName _ (TypedScheme binderId _ _ _ _ _) <- methods
          ]
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
      [binderId | TypedMethodSignature _ _ (TypedScheme binderId _ _ _ _ _) <- methods]
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
    statementPath = TypedStatementPath (moduleContextPath context) (statementIndexFor context statementLocation)

expressionBinderOccurrences :: ModuleContext -> [Int] -> [Int] -> TypedExpr -> [BinderOccurrence]
expressionBinderOccurrences context statementLocation expressionPath expression =
  ownedOccurrences <> patternOccurrences <> childOccurrences
  where
    statementIndex = statementIndexFor context statementLocation
    expressionValidationPath = TypedExpressionPath (moduleContextPath context) statementIndex expressionPath
    ownedOccurrences =
      case expression of
        TypedLambdaExpr _ binderId _ _ -> [BinderOccurrence expressionValidationPath binderId]
        _ -> []
    patternOccurrences =
      case expression of
        TypedPatternCaseExpr _ _ arms ->
          concat
            [ patternBinderOccurrences (moduleContextPath context) statementIndex (expressionPath <> [armIndex]) patternValue
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

patternBinderOccurrences :: [Text] -> Int -> [Int] -> TypedPattern -> [BinderOccurrence]
patternBinderOccurrences modulePath statementIndex patternPath patternValue =
  ownedOccurrences <> childOccurrences
  where
    patternValidationPath = TypedPatternPath modulePath statementIndex patternPath
    ownedOccurrences =
      case patternValue of
        TypedVariablePattern _ binderId _ -> [BinderOccurrence patternValidationPath binderId]
        TypedAsPattern _ binderId _ _ -> [BinderOccurrence patternValidationPath binderId]
        _ -> []
    childOccurrences =
      case patternValue of
        TypedConstructorPattern _ _ patterns -> indexedChildren patterns
        TypedListPattern _ patterns -> indexedChildren patterns
        TypedConsListPattern _ headPattern tailPattern -> indexedChildren [headPattern, tailPattern]
        TypedTuplePattern _ patterns -> indexedChildren patterns
        TypedAsPattern _ _ _ nested -> patternBinderOccurrences modulePath statementIndex (patternPath <> [0]) nested
        TypedOrPattern _ alternatives -> indexedChildren alternatives
        _ -> []
    indexedChildren patterns =
      concat
        [ patternBinderOccurrences modulePath statementIndex (patternPath <> [childIndex]) child
        | (childIndex, child) <- zip [0 ..] patterns
        ]

statementSchemes :: TypedStatement -> [(TypedBinderId, TypedScheme)]
statementSchemes statement =
  case statement of
    TypedLetStatement binderId _ _ scheme _ -> [(binderId, scheme)]
    TypedSignatureStatement {} -> []
    TypedClassStatement (TypedClassDeclaration _ _ classParameters methods) ->
      [ (binderId, generalizeClassMethodScheme classParameters scheme)
      | TypedMethodSignature _ _ scheme@(TypedScheme binderId _ _ _ _ _) <- methods
      ]
    _ -> []

interfaceSchemeEntries :: ([Text], Maybe [Text], TypedModule) -> [(TypedBinderId, TypedScheme)]
interfaceSchemeEntries (_, selectedNames, TypedModule _ _ _ exports (TypedModuleInterface values _ classes _) _ _) =
  [ (binderId, scheme)
  | TypedValueInterface name scheme@(TypedScheme binderId _ _ _ _ _) <- values,
    importAllows selectedNames name,
    moduleExportsName TypedValueNamespace name exports
  ]
    <> [ (binderId, generalizeClassMethodScheme classParameters scheme)
       | TypedClassInterface (TypedClassDeclaration _ _ classParameters methods) <- classes,
         TypedMethodSignature name _ scheme@(TypedScheme binderId _ _ _ _ _) <- methods,
         importAllows selectedNames name,
         moduleExportsName TypedValueNamespace name exports
       ]

generalizeClassMethodScheme :: [TypedTypeParameterId] -> TypedScheme -> TypedScheme
generalizeClassMethodScheme classParameters (TypedScheme owner methodParameters evidence primitive resultType resultRecipe) =
  TypedScheme owner (usedClassParameters <> methodParameters) evidence primitive resultType resultRecipe
  where
    usedClassParameters =
      [ parameter
      | parameter <- classParameters,
        parameter `notElem` methodParameters,
        schemeMentionsTypeParameter parameter evidence primitive resultType resultRecipe
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

typeMentionsParameter :: TypedTypeParameterId -> TypedType -> Bool
typeMentionsParameter parameter typeValue =
  case typeValue of
    TypedListType elementType -> typeMentionsParameter parameter elementType
    TypedTupleType elementTypes -> any (typeMentionsParameter parameter) elementTypes
    TypedDataType _ arguments -> any (typeMentionsParameter parameter) arguments
    TypedFunctionType argument result ->
      typeMentionsParameter parameter argument || typeMentionsParameter parameter result
    TypedTypeParameterType candidate -> candidate == parameter
    _ -> False

recipeMentionsParameter :: TypedTypeParameterId -> TypedRepresentationRecipe -> Bool
recipeMentionsParameter parameter recipe =
  case recipe of
    TypedManagedListRecipe elementRecipe -> recipeMentionsParameter parameter elementRecipe
    TypedManagedProductRecipe elementRecipes -> any (recipeMentionsParameter parameter) elementRecipes
    TypedClosureRecipe parameters result ->
      any (recipeMentionsParameter parameter) parameters || recipeMentionsParameter parameter result
    TypedRepresentationParameterRecipe candidate -> candidate == parameter
    _ -> False

statementImplEntries :: TypedStatement -> [(TypedImplId, Set Text)]
statementImplEntries statement =
  case statement of
    TypedImplStatement (TypedImplDeclaration _ implId methods) ->
      [(implId, Set.fromList [methodKey | TypedMethodDefinition (TypedMethodId _ methodKey) _ _ _ _ <- methods])]
    _ -> []

interfaceImplEntries :: ([Text], Maybe [Text], TypedModule) -> [(TypedImplId, Set Text)]
interfaceImplEntries visibleModule@(modulePath, _, TypedModule _ _ _ _ (TypedModuleInterface _ _ _ impls) statements _) =
  [ ( qualifyExternalImplId modulePath implId,
      Set.fromList [methodKey | TypedMethodDefinition (TypedMethodId _ methodKey) _ _ _ _ <- methods]
    )
  | TypedImplInterface implId <- impls,
    implImportAllowed visibleModule implId,
    TypedImplStatement (TypedImplDeclaration _ declarationImplId methods) <- statements,
    declarationImplId == implId
  ]

implImportAllowed :: ([Text], Maybe [Text], TypedModule) -> TypedImplId -> Bool
implImportAllowed visibleModule@(_, _, TypedModule _ _ _ _ (TypedModuleInterface _ _ classes _) _ _) (TypedImplId _ capability _) =
  any capabilityIncluded classes
  where
    capabilityIncluded (TypedClassInterface (TypedClassDeclaration _ name _ methods)) =
      coreNameIdentifier name == coreNameIdentifier capability
        && interfaceCapabilityNameIncluded visibleModule name methods

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
        | (modulePath, TypedModule _ _ _ _ (TypedModuleInterface _ datas _ _) _ _) <-
            Map.toList moduleTable,
          TypedDataInterface declaration@(TypedDataDeclaration _ name _ _) <- datas,
          key <- maybeToList (definitionNameKey modulePath name)
        ]
    rootKeys =
      Set.fromList
        ( concatMap sourceVisibleDataKeys visibleModules
            <> concatMap selectedSchemeDataKeys visibleModules
        )
    sourceVisibleDataKeys (modulePath, selectedNames, TypedModule _ _ _ exports (TypedModuleInterface _ datas _ _) _ _) =
      [ key
      | TypedDataInterface (TypedDataDeclaration _ name _ constructors) <- datas,
        sourceVisibleDataIncluded selectedNames exports name constructors,
        key <- maybeToList (definitionNameKey modulePath name)
      ]
    selectedSchemeDataKeys visibleModule@(modulePath, _, _) =
      concatMap (schemeDataTypeKeys modulePath) selectedSchemes
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
interfaceCapabilitySchemes visibleModule@(_, _, TypedModule _ _ _ _ (TypedModuleInterface _ _ classes _) _ _) =
  [ scheme
  | TypedClassInterface (TypedClassDeclaration _ name _ methods) <- classes,
    interfaceCapabilityIncluded visibleModule name methods,
    TypedMethodSignature _ _ scheme <- methods
  ]

schemeDataTypeKeys :: [Text] -> TypedScheme -> [ResolvedNameKey]
schemeDataTypeKeys modulePath (TypedScheme _ _ evidence primitive resultType _) =
  concatMap (typeDataKeys modulePath) (resultType : evidenceTypes <> primitiveTypes)
  where
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
interfaceNameKeys visibleModule@(modulePath, selectedNames, TypedModule _ _ _ exports (TypedModuleInterface values datas classes _) _ _) =
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
      | TypedClassInterface (TypedClassDeclaration _ name _ methods) <- classes,
        interfaceCapabilityNameIncluded visibleModule name methods,
        key <- maybeToList (definitionNameKey modulePath name)
      ],
      [ key
      | TypedClassInterface (TypedClassDeclaration _ _ _ methods) <- classes,
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
    TypedClassStatement (TypedClassDeclaration _ name _ methods) ->
      name : [methodName | TypedMethodSignature methodName _ _ <- methods]
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
interfaceConstructorEntries (modulePath, selectedNames, TypedModule _ _ _ exports (TypedModuleInterface _ datas _ _) _ _) =
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
interfaceCapabilityEntries visibleModule@(modulePath, _, TypedModule _ _ _ _ (TypedModuleInterface _ _ classes _) _ _) =
  [ entry
  | TypedClassInterface declaration@(TypedClassDeclaration _ name _ methods) <- classes,
    interfaceCapabilityIncluded visibleModule name methods,
    entry <- maybeToList (capabilityEntry modulePath declaration)
  ]

interfaceCapabilityIncluded :: ([Text], Maybe [Text], TypedModule) -> TypedCoreName -> [TypedMethodSignature] -> Bool
interfaceCapabilityIncluded visibleModule name methods =
  interfaceCapabilityNameIncluded visibleModule name methods
    || maybe False (`Set.member` requiredCapabilityIdentifiers visibleModule) (coreNameIdentifier name)

interfaceCapabilityNameIncluded :: ([Text], Maybe [Text], TypedModule) -> TypedCoreName -> [TypedMethodSignature] -> Bool
interfaceCapabilityNameIncluded (_, selectedNames, TypedModule _ _ _ exports _ _ _) name methods =
  ( importAllows selectedNames name
      && (moduleExportsName TypedCapabilityNamespace name exports || moduleExportsName TypedTypeNamespace name exports)
  )
    || any methodImported methods
  where
    methodImported (TypedMethodSignature methodName _ _) =
      importAllows selectedNames methodName && moduleExportsName TypedValueNamespace methodName exports

requiredCapabilityIdentifiers :: ([Text], Maybe [Text], TypedModule) -> Set Text
requiredCapabilityIdentifiers visibleModule =
  Set.fromList
    [ capability
    | (_, TypedScheme _ _ evidence _ _ _) <- interfaceSchemeEntries visibleModule,
      TypedEvidenceParameter _ (TypedCapabilityConstraint capability _ _) <- evidence
    ]

capabilityEntry :: [Text] -> TypedClassDeclaration -> Maybe (ResolvedNameKey, CapabilityContract)
capabilityEntry modulePath (TypedClassDeclaration _ name parameters methods) = do
  key <- definitionNameKey modulePath name
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

qualifyExternalType :: [Text] -> TypedType -> TypedType
qualifyExternalType modulePath typeValue =
  case typeValue of
    TypedListType elementType -> TypedListType (qualifyExternalType modulePath elementType)
    TypedTupleType elementTypes -> TypedTupleType (map (qualifyExternalType modulePath) elementTypes)
    TypedDataType name arguments -> TypedDataType (qualifyExternalName modulePath name) (map (qualifyExternalType modulePath) arguments)
    TypedFunctionType argument result -> TypedFunctionType (qualifyExternalType modulePath argument) (qualifyExternalType modulePath result)
    _ -> typeValue

qualifyExternalRecipe :: [Text] -> TypedRepresentationRecipe -> TypedRepresentationRecipe
qualifyExternalRecipe modulePath recipe =
  case recipe of
    TypedManagedListRecipe elementRecipe -> TypedManagedListRecipe (qualifyExternalRecipe modulePath elementRecipe)
    TypedManagedProductRecipe elementRecipes -> TypedManagedProductRecipe (map (qualifyExternalRecipe modulePath) elementRecipes)
    TypedManagedVariantRecipe name arguments ->
      TypedManagedVariantRecipe
        (qualifyExternalName modulePath name)
        (map (qualifyExternalType modulePath) arguments)
    TypedClosureRecipe parameters result ->
      TypedClosureRecipe
        (map (qualifyExternalRecipe modulePath) parameters)
        (qualifyExternalRecipe modulePath result)
    _ -> recipe

qualifyExternalImplId :: [Text] -> TypedImplId -> TypedImplId
qualifyExternalImplId modulePath (TypedImplId implPath capability targetTypes) =
  TypedImplId
    implPath
    (qualifyExternalName modulePath capability)
    (map (qualifyExternalType modulePath) targetTypes)

qualifyExternalName :: [Text] -> TypedCoreName -> TypedCoreName
qualifyExternalName modulePath name =
  case name of
    TypedResolvedName TypedCurrentModule namespace identifier ->
      TypedResolvedName
        (if modulePath == ["Prelude"] then TypedAmbientPrelude else TypedImportedModule modulePath)
        namespace
        identifier
    _ -> name

importAllows :: Maybe [Text] -> TypedCoreName -> Bool
importAllows Nothing _ = True
importAllows (Just selectedNames) name = maybe False (`elem` selectedNames) (coreNameIdentifier name)

moduleExportsName :: TypedNameNamespace -> TypedCoreName -> [TypedModuleExport] -> Bool
moduleExportsName namespace name exports =
  case coreNameIdentifier name of
    Nothing -> False
    Just identifier -> TypedModuleExport namespace identifier `elem` exports

definitionNameKey :: [Text] -> TypedCoreName -> Maybe ResolvedNameKey
definitionNameKey modulePath name =
  case name of
    TypedResolvedName _ namespace identifier -> Just (ResolvedNameKey modulePath namespace identifier)
    TypedGeneratedName kind -> Just (GeneratedNameKey kind)
    _ -> Nothing

resolvedNameKey :: [Text] -> TypedCoreName -> Maybe ResolvedNameKey
resolvedNameKey currentModulePath name =
  case name of
    TypedResolvedName origin namespace identifier ->
      Just (ResolvedNameKey (originModulePath origin) namespace identifier)
    TypedGeneratedName kind -> Just (GeneratedNameKey kind)
    _ -> Nothing
  where
    originModulePath origin =
      case origin of
        TypedCurrentModule -> currentModulePath
        TypedImportedModule modulePath -> modulePath
        TypedAmbientPrelude -> ["Prelude"]

withVisibleNames :: [TypedCoreName] -> ModuleContext -> ModuleContext
withVisibleNames names context =
  context
    { moduleContextVisibleNames =
        Set.union
          (moduleContextVisibleNames context)
          (Set.fromList [key | name <- names, key <- maybeToList (resolvedNameKey (moduleContextPath context) name)])
    }

withLexicalContracts :: [(TypedCoreName, ValueContract)] -> ModuleContext -> ModuleContext
withLexicalContracts contracts context =
  context
    { moduleContextVisibleNames = Set.union localNames (moduleContextVisibleNames context),
      moduleContextLexicalContracts = Map.union localContracts (moduleContextLexicalContracts context)
    }
  where
    entries =
      [ (key, contract)
      | (name, contract) <- contracts,
        key <- maybeToList (resolvedNameKey (moduleContextPath context) name)
      ]
    localNames = Set.fromList (map fst entries)
    localContracts = Map.fromList entries

withBlockDeclarations :: [TypedStatement] -> ModuleContext -> ModuleContext
withBlockDeclarations statements context =
  context
    { moduleContextSchemes = Map.union localSchemes (moduleContextSchemes context),
      moduleContextVisibleNames = Set.union localNames (moduleContextVisibleNames context),
      moduleContextVisibleImpls = Set.union localImpls (moduleContextVisibleImpls context),
      moduleContextImplMethods = Map.unionWith Set.union localImplMethods (moduleContextImplMethods context),
      moduleContextDataArities = Map.union localDataArities (moduleContextDataArities context),
      moduleContextDataContracts = Map.union localDataContracts (moduleContextDataContracts context),
      moduleContextConstructorContracts = Map.union localConstructors (moduleContextConstructorContracts context),
      moduleContextCapabilityContracts = Map.union localCapabilities (moduleContextCapabilityContracts context)
    }
  where
    modulePath = moduleContextPath context
    localSchemes = Map.fromList (concatMap statementSchemes statements)
    localNames = Set.fromList (concatMap (statementDefinedNameKeys modulePath) statements)
    localImplEntries = concatMap statementImplEntries statements
    localImpls = Set.fromList (map fst localImplEntries)
    localImplMethods = Map.fromListWith Set.union localImplEntries
    localDataArities = Map.fromList (concatMap (statementDataEntries modulePath) statements)
    localDataContracts = Map.fromList (concatMap (statementDataContractEntries modulePath) statements)
    localConstructors = Map.fromList (concatMap (statementConstructorEntries modulePath) statements)
    localCapabilities = Map.fromList (concatMap (statementCapabilityEntries modulePath) statements)

validateStatementsInOrder :: ModuleContext -> [([Int], TypedStatement)] -> [TypedCoreValidationFailure]
validateStatementsInOrder initialContext locatedStatements =
  fst (foldl' validateNext ([], initialContext) (zip [0 ..] locatedStatements))
  where
    statements = map snd locatedStatements
    validateNext (failures, visibleContext) (blockIndex, (statementLocation, statement)) =
      let recursiveGroup = recursiveGroupStatements initialContext statements blockIndex
          statementContext =
            case statement of
              TypedLetStatement {}
                | null recursiveGroup -> visibleContext
                | otherwise -> withBlockDeclarations recursiveGroup visibleContext
              _ -> withBlockDeclarations [statement] visibleContext
          nextContext = withBlockDeclarations [statement] visibleContext
       in (failures <> validateStatement statementContext statementLocation statement, nextContext)

recursiveGroupStatements :: ModuleContext -> [TypedStatement] -> Int -> [TypedStatement]
recursiveGroupStatements outerContext statements statementIndex =
  case Map.lookup statementIndex dependencies of
    Nothing -> []
    Just directDependencies
      | length members > 1 || Set.member statementIndex directDependencies ->
          [statement | (index, statement) <- zip [0 ..] statements, index `elem` members]
      | otherwise -> []
  where
    declarations =
      [ (index, nameKey, expression)
      | (index, TypedLetStatement _ name _ _ expression) <- zip [0 ..] statements,
        nameKey <- maybeToList (resolvedNameKey (moduleContextPath outerContext) name)
      ]
    declarationIndicesByName =
      Map.fromListWith (flip (<>))
        [ (nameKey, [index])
        | (index, nameKey, _) <- declarations
        ]
    dependencies =
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
    resolveDependency index ownName expression referencedName =
      case Map.lookup referencedName declarationIndicesByName of
        Nothing -> Nothing
        Just declarationIndices ->
          case reverse (filter (< index) declarationIndices) of
            prior : _ -> Just prior
            []
              | Set.member referencedName (moduleContextVisibleNames outerContext) -> Nothing
              | referencedName == ownName ->
                  if expressionCanBeRecursive expression then Just index else Nothing
              | otherwise ->
                  case filter (> index) declarationIndices of
                    future : _ -> Just future
                    [] -> Nothing
    members =
      [ candidate
      | candidate <- Map.keys dependencies,
        dependencyReachable dependencies statementIndex candidate,
        dependencyReachable dependencies candidate statementIndex
      ]

dependencyReachable :: Map Int (Set Int) -> Int -> Int -> Bool
dependencyReachable dependencies source target = go Set.empty [source]
  where
    go _ [] = False
    go seen (current : rest)
      | current == target = True
      | Set.member current seen = go seen rest
      | otherwise =
          go
            (Set.insert current seen)
            (Set.toList (Map.findWithDefault Set.empty current dependencies) <> rest)

expressionCanBeRecursive :: TypedExpr -> Bool
expressionCanBeRecursive expression =
  case nodeType (expressionInfo expression) of
    TypedFunctionType {} -> True
    _ -> False

freeExpressionValueNames :: ModuleContext -> Set ResolvedNameKey -> TypedExpr -> Set ResolvedNameKey
freeExpressionValueNames context boundNames expression =
  case expression of
    TypedVariableExpr _ name -> freeName name
    TypedLambdaExpr _ _ name body ->
      freeExpressionValueNames context (Set.union boundNames (nameKeys [name])) body
    TypedOperatorValueExpr _ operator -> freeOperator operator
    TypedPatternCaseExpr _ scrutinee arms ->
      freeExpressionValueNames context boundNames scrutinee
        <> Set.unions
          [ let armBoundNames =
                  Set.union
                    boundNames
                    (nameKeys [name | (name, _) <- patternBoundContracts patternValue])
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

withTypeScope :: [TypedTypeParameterId] -> ModuleContext -> ModuleContext
withTypeScope typeParameters context =
  context {moduleContextTypeScope = Set.union (Set.fromList typeParameters) (moduleContextTypeScope context)}

withSchemeScope :: TypedScheme -> ModuleContext -> ModuleContext
withSchemeScope (TypedScheme _ typeParameters _ primitiveConstraints _ _) context =
  context
    { moduleContextTypeScope = Set.union (Set.fromList typeParameters) (moduleContextTypeScope context),
      moduleContextPrimitiveConstraints = primitiveConstraints <> moduleContextPrimitiveConstraints context
    }

topLevelStatementLocations :: [TypedStatement] -> [[Int]]
topLevelStatementLocations statements = map pure [0 .. length statements - 1]

nestedStatementLocations :: [Int] -> TypedStatement -> [[Int]]
nestedStatementLocations statementLocation statement =
  case statement of
    TypedLetStatement _ _ _ _ expression -> nestedExpressionLocations statementLocation [0] expression
    TypedImplStatement (TypedImplDeclaration _ _ methods) ->
      concat
        [ nestedExpressionLocations statementLocation [methodIndex] expression
        | (methodIndex, TypedMethodDefinition _ _ _ _ expression) <- zip [0 ..] methods
        ]
    TypedExpressionStatement _ expression -> nestedExpressionLocations statementLocation [0] expression
    _ -> []

nestedExpressionLocations :: [Int] -> [Int] -> TypedExpr -> [[Int]]
nestedExpressionLocations statementLocation expressionPath expression =
  case expression of
    TypedBlockExpr _ statements ->
      concat
        [ let location = nestedStatementLocation statementLocation expressionPath blockIndex
           in location : nestedStatementLocations location statement
        | (blockIndex, statement) <- zip [0 ..] statements
        ]
    _ ->
      concat
        [ nestedExpressionLocations statementLocation (expressionPath <> [childIndex]) child
        | (childIndex, child) <- zip [0 ..] (expressionChildren expression)
        ]

nestedStatementLocation :: [Int] -> [Int] -> Int -> [Int]
nestedStatementLocation statementLocation expressionPath blockIndex =
  statementLocation <> [-1] <> expressionPath <> [-2, blockIndex]

statementIndexFor :: ModuleContext -> [Int] -> Int
statementIndexFor context statementLocation =
  case Map.lookup statementLocation (moduleContextStatementIndices context) of
    Just statementIndex -> statementIndex
    Nothing -> error "typed-core validator encountered an unindexed statement location"

validateStatement :: ModuleContext -> [Int] -> TypedStatement -> [TypedCoreValidationFailure]
validateStatement context statementLocation statement =
  case statement of
    TypedLetStatement binderId name _ scheme expression ->
      validateLocalDefinitionName context [TypedValueNamespace] statementPath name
        <> validateBinderDefinition context statementPath binderId name
        <> validateScheme context statementPath binderId scheme
        <> validateBindingValue statementPath scheme (expressionInfo expression)
        <> validateExpression (withSchemeScope scheme context) statementLocation [0] expression
    TypedSignatureStatement binderId name _ scheme ->
      validateLocalDefinitionName context [TypedValueNamespace] statementPath name
        <> validateBinderDefinition context statementPath binderId name
        <> validateScheme context statementPath binderId scheme
    TypedDataStatement declaration -> validateDataDeclaration context statementPath declaration
    TypedClassStatement declaration -> validateClassDeclaration context statementPath declaration
    TypedImplStatement declaration -> validateImplDeclaration context statementLocation statementPath declaration
    TypedExpressionStatement _ expression -> validateExpression context statementLocation [0] expression
  where
    statementIndex = statementIndexFor context statementLocation
    statementPath = TypedStatementPath (moduleContextPath context) statementIndex

validateBinderDefinition :: ModuleContext -> TypedCoreValidationPath -> TypedBinderId -> TypedCoreName -> [TypedCoreValidationFailure]
validateBinderDefinition context path binderId@(TypedBinderId (modulePath, _, embeddedName)) publishedName
  | modulePath == moduleContextPath context && embeddedName == publishedName = []
  | otherwise = [failure path TypedUnknownBinder (TypedBinderDetail binderId)]

validateBindingValue :: TypedCoreValidationPath -> TypedScheme -> TypedNodeInfo -> [TypedCoreValidationFailure]
validateBindingValue path (TypedScheme _ _ _ _ resultType resultRecipe) info =
  valueFailures resultType resultRecipe
  where
    valueFailures expectedType expectedRecipeValue
      | nodeType info /= expectedType =
          [failure path TypedBindingValueMismatch (TypedTypeDetail expectedType (nodeType info))]
      | nodeRecipe info /= expectedRecipeValue =
          [failure path TypedBindingValueMismatch (TypedRecipeDetail expectedRecipeValue (nodeRecipe info))]
      | otherwise = []

validateScheme :: ModuleContext -> TypedCoreValidationPath -> TypedBinderId -> TypedScheme -> [TypedCoreValidationFailure]
validateScheme context path owner = validateSchemeWithOuterScope context path owner (moduleContextTypeScope context)

validateSchemeWithOuterScope :: ModuleContext -> TypedCoreValidationPath -> TypedBinderId -> Set TypedTypeParameterId -> TypedScheme -> [TypedCoreValidationFailure]
validateSchemeWithOuterScope context path owner outerScope (TypedScheme schemeOwner typeParameters evidenceParameters primitiveConstraints resultType resultRecipe) =
  ownerFailures
    <> validateOrderedTypeParameters path typeParameters
    <> validateOrderedEvidenceParameters path evidenceParameters
    <> concatMap (validateEvidenceParameter context path parameterScope) evidenceParameters
    <> concatMap (validatePrimitiveConstraint context path parameterScope) primitiveConstraints
    <> validateType path parameterScope resultType
    <> concatMap (validateDataTypeApplications context path) (resultType : evidenceTypes <> primitiveTypes)
    <> validateRecipe path parameterScope resultRecipe
    <> validateTypeRecipe path parameterScope resultType resultRecipe
  where
    ownerFailures
      | owner == schemeOwner = []
      | otherwise = [failure path TypedUnknownBinder (TypedBinderDetail schemeOwner)]
    parameterScope = Set.union outerScope (Set.fromList typeParameters)
    evidenceTypes = [targetType | TypedEvidenceParameter _ (TypedCapabilityConstraint _ _ targetType) <- evidenceParameters]
    primitiveTypes =
      [ typeValue
      | constraint <- primitiveConstraints,
        typeValue <- case constraint of
          TypedNumericPrimitiveConstraint _ value -> [value]
          TypedStrictEqualityPrimitiveConstraint value -> [value]
      ]

validateOrderedTypeParameters :: TypedCoreValidationPath -> [TypedTypeParameterId] -> [TypedCoreValidationFailure]
validateOrderedTypeParameters path parameters = duplicateFailures <> orderFailures
  where
    duplicateFailures = duplicateParameterFailures path TypedDuplicateTypeParameter TypedTypeParameterDetail parameters
    orderFailures =
      [ failure path TypedInvalidTypeParameterOrder (TypedIndexDetail index)
      | (index, TypedTypeParameterId actual) <- zip [0 ..] parameters,
        actual /= index
      ]

validateOrderedEvidenceParameters :: TypedCoreValidationPath -> [TypedEvidenceParameter] -> [TypedCoreValidationFailure]
validateOrderedEvidenceParameters path parameters = duplicateFailures <> orderFailures
  where
    parameterIds = [parameterId | TypedEvidenceParameter parameterId _ <- parameters]
    duplicateFailures = duplicateParameterFailures path TypedDuplicateEvidenceParameter TypedEvidenceParameterDetail parameterIds
    orderFailures =
      [ failure path TypedInvalidEvidenceParameterOrder (TypedIndexDetail index)
      | (index, TypedEvidenceParameter (TypedEvidenceParameterId actual) _) <- zip [0 ..] parameters,
        actual /= index
      ]

duplicateParameterFailures :: (Ord identifier) => TypedCoreValidationPath -> TypedCoreValidationKind -> (identifier -> TypedCoreValidationDetail) -> [identifier] -> [TypedCoreValidationFailure]
duplicateParameterFailures path kind detailOf = snd . foldl' step (Set.empty, [])
  where
    step (seen, failures) identifier
      | Set.member identifier seen =
          (seen, failures <> [failure path kind (detailOf identifier)])
      | otherwise = (Set.insert identifier seen, failures)

validateEvidenceParameter :: ModuleContext -> TypedCoreValidationPath -> Set TypedTypeParameterId -> TypedEvidenceParameter -> [TypedCoreValidationFailure]
validateEvidenceParameter context path scope (TypedEvidenceParameter _ constraint) =
  validateCapabilityConstraint context path scope constraint

validatePrimitiveConstraint :: ModuleContext -> TypedCoreValidationPath -> Set TypedTypeParameterId -> TypedPrimitiveConstraint -> [TypedCoreValidationFailure]
validatePrimitiveConstraint context path scope constraint =
  case constraint of
    TypedNumericPrimitiveConstraint numericConstraint typeValue ->
      validateType path scope typeValue
        <> validateNumericConstraintTarget path numericConstraint typeValue
    TypedStrictEqualityPrimitiveConstraint typeValue ->
      validateType path scope typeValue
        <> validateStrictEqualityTarget context path typeValue

validateStrictEqualityTarget :: ModuleContext -> TypedCoreValidationPath -> TypedType -> [TypedCoreValidationFailure]
validateStrictEqualityTarget context path typeValue
  | strictEqualityTypeSupported context typeValue = []
  | otherwise = [failure path TypedBindingValueMismatch (TypedTypeDetail TypedBoolType typeValue)]

strictEqualityTypeSupported :: ModuleContext -> TypedType -> Bool
strictEqualityTypeSupported context = strictEqualityTypeSupportedWith context (const True)

strictEqualityOperandTypeSupported :: ModuleContext -> TypedType -> Bool
strictEqualityOperandTypeSupported context typeValue =
  activeConstraint typeValue
    || strictEqualityTypeSupportedWith context activeConstraint typeValue
  where
    activeConstraint candidate =
      TypedStrictEqualityPrimitiveConstraint candidate
        `elem` moduleContextPrimitiveConstraints context

strictEqualityTypeSupportedWith :: ModuleContext -> (TypedType -> Bool) -> TypedType -> Bool
strictEqualityTypeSupportedWith context typeParameterSupported = supported Set.empty
  where
    supported seen typeValue =
      case typeValue of
        TypedIntType -> True
        TypedFloatType -> True
        TypedNumericType _ -> True
        TypedBoolType -> True
        TypedCharType -> True
        TypedTextType -> True
        TypedListType elementType -> supported seen elementType
        TypedTupleType elementTypes -> all (supported seen) elementTypes
        TypedTypeParameterType _ -> typeParameterSupported typeValue
        TypedFunctionType {} -> False
        TypedDataType name arguments ->
          case resolvedNameKey (moduleContextPath context) name of
            Nothing -> False
            Just dataKey
              | Set.member (dataKey, arguments) seen -> True
              | otherwise ->
                  case Map.lookup dataKey (moduleContextDataContracts context) of
                    Nothing -> False
                    Just (DataContract parameters constructorFields)
                      | length parameters /= length arguments -> False
                      | otherwise ->
                          let substitutions = Map.fromList (zip parameters arguments)
                              nextSeen = Set.insert (dataKey, arguments) seen
                           in all
                                (all (supported nextSeen . substituteTypeParameters substitutions))
                                constructorFields

validateNumericConstraintTarget :: TypedCoreValidationPath -> TypedNumericConstraint -> TypedType -> [TypedCoreValidationFailure]
validateNumericConstraintTarget path numericConstraint typeValue
  | numericConstraintAcceptsType numericConstraint typeValue = []
  | otherwise = [failure path TypedBindingValueMismatch (TypedTypeDetail TypedIntType typeValue)]

numericConstraintAcceptsType :: TypedNumericConstraint -> TypedType -> Bool
numericConstraintAcceptsType numericConstraint typeValue =
  case numericConstraint of
    TypedIntegralLiteralNumericConstraint lower upper ->
      integralLiteralConstraintAcceptsType lower upper typeValue
    _ ->
      case typeValue of
        TypedTypeParameterType _ -> True
        TypedIntType -> True
        TypedFloatType -> not (integralConstraint numericConstraint)
        TypedNumericType numericType
          | integralConstraint numericConstraint -> numericTypeIsIntegral numericType
          | otherwise -> True
        _ -> False

integralLiteralConstraintAcceptsType :: Text -> Text -> TypedType -> Bool
integralLiteralConstraintAcceptsType lowerText upperText typeValue =
  case (parseDecimalBound lowerText, parseDecimalBound upperText) of
    (Just lower, Just upper)
      | lower <= upper ->
          case typeValue of
            TypedTypeParameterType _ -> True
            _ ->
              case integralTypeBounds typeValue of
                Just (minimumValue, maximumValue) ->
                  minimumValue <= lower
                    && upper <= maximumValue
                Nothing -> False
    _ -> False

parseDecimalBound :: Text -> Maybe Integer
parseDecimalBound value =
  case Text.uncons value of
    Just ('-', digits) -> negate <$> parseDecimalMagnitude digits
    _ -> parseDecimalMagnitude value

parseDecimalMagnitude :: Text -> Maybe Integer
parseDecimalMagnitude digits
  | Text.null digits || Text.any (not . asciiDigit) digits = Nothing
  | otherwise = Just (Text.foldl' accumulate 0 digits)
  where
    asciiDigit character = character >= '0' && character <= '9'
    accumulate result character =
      result * 10 + toInteger (fromEnum character - fromEnum '0')

integralTypeBounds :: TypedType -> Maybe (Integer, Integer)
integralTypeBounds typeValue =
  case typeValue of
    TypedIntType -> signedBounds 64
    TypedNumericType numericType ->
      case numericType of
        TypedInt8Type -> signedBounds 8
        TypedInt16Type -> signedBounds 16
        TypedInt32Type -> signedBounds 32
        TypedInt64Type -> signedBounds 64
        TypedUInt8Type -> unsignedBounds 8
        TypedUInt16Type -> unsignedBounds 16
        TypedUInt32Type -> unsignedBounds 32
        TypedUInt64Type -> unsignedBounds 64
        TypedFloat16Type -> Nothing
        TypedFloat32Type -> Nothing
        TypedFloat64Type -> Nothing
    _ -> Nothing
  where
    signedBounds :: Int -> Maybe (Integer, Integer)
    signedBounds width =
      let limit = 2 ^ (width - 1)
       in Just (-limit, limit - 1)
    unsignedBounds :: Int -> Maybe (Integer, Integer)
    unsignedBounds width = Just (0, 2 ^ width - 1)

integralConstraint :: TypedNumericConstraint -> Bool
integralConstraint numericConstraint =
  case numericConstraint of
    TypedIntegralNumericConstraint -> True
    TypedIntegralLiteralNumericConstraint {} -> True
    _ -> False

numericTypeIsIntegral :: TypedNumericType -> Bool
numericTypeIsIntegral numericType =
  case numericType of
    TypedInt8Type -> True
    TypedInt16Type -> True
    TypedInt32Type -> True
    TypedInt64Type -> True
    TypedUInt8Type -> True
    TypedUInt16Type -> True
    TypedUInt32Type -> True
    TypedUInt64Type -> True
    TypedFloat16Type -> False
    TypedFloat32Type -> False
    TypedFloat64Type -> False

validateCapabilityConstraint :: ModuleContext -> TypedCoreValidationPath -> Set TypedTypeParameterId -> TypedCapabilityConstraint -> [TypedCoreValidationFailure]
validateCapabilityConstraint context path scope (TypedCapabilityConstraint capability maybeMethod targetType) =
  validateCapabilityConstraintTarget path scope targetType
    <> case matchingContracts of
      [] -> [failure path TypedInvisibleName (TypedTextDetail capability)]
      [_] ->
        case maybeMethod of
          Nothing -> []
          Just method
            | any (capabilityHasMethod method) matchingContracts -> []
            | otherwise -> [failure path TypedMethodSelectionMismatch (TypedTextDetail method)]
      _ -> [failure path TypedDuplicateDeclaration (TypedTextDetail capability)]
  where
    matchingContracts =
      [ contract
      | (key@(ResolvedNameKey _ TypedCapabilityNamespace identifier), contract) <-
          Map.toList (moduleContextCapabilityContracts context),
        Set.member key (moduleContextVisibleNames context),
        identifier == capability
      ]
    capabilityHasMethod method (CapabilityContract _ methods) =
      any (methodKeyMatches capability method) (Map.keys methods)

validateCapabilityConstraintTarget :: TypedCoreValidationPath -> Set TypedTypeParameterId -> TypedType -> [TypedCoreValidationFailure]
validateCapabilityConstraintTarget path scope = validateType path scope

validateDataDeclaration :: ModuleContext -> TypedCoreValidationPath -> TypedDataDeclaration -> [TypedCoreValidationFailure]
validateDataDeclaration context path (TypedDataDeclaration _ name parameters constructors) =
  validateLocalDefinitionName context [TypedTypeNamespace] path name
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
        <> concatMap (validateRecipe path scope) recipes
        <> dataRecipeFailures fields recipes
    dataRecipeFailures fields recipes
      | length fields /= length recipes =
          [failure path TypedDataRecipeMismatch (TypedArityDetail (length fields) (length recipes))]
      | otherwise = concat (zipWith fieldFailure fields recipes)
    fieldFailure fieldType recipe =
      case expectedRecipe fieldType of
        Just expected
          | validRecipeWidth recipe && expected /= recipe ->
              [failure path TypedDataRecipeMismatch (TypedRecipeDetail expected recipe)]
        _ -> []

validateClassDeclaration :: ModuleContext -> TypedCoreValidationPath -> TypedClassDeclaration -> [TypedCoreValidationFailure]
validateClassDeclaration context path (TypedClassDeclaration _ name parameters methods) =
  validateLocalDefinitionName context [TypedCapabilityNamespace] path name
    <> visibleClassCollisionFailures context path name
    <> (if length parameters == 1 then [] else [failure path TypedMethodSelectionMismatch (TypedArityDetail 1 (length parameters))])
    <> validateOrderedTypeParameters path parameters
    <> concatMap validateMethod methods
  where
    validateMethod (TypedMethodSignature methodName _ scheme@(TypedScheme binderId methodParameters evidenceParameters primitiveConstraints _ _)) =
      validateLocalDefinitionName context [TypedValueNamespace] path methodName
        <> validateBinderDefinition context path binderId methodName
        <> classMethodSchemeShapeFailures methodParameters evidenceParameters primitiveConstraints
        <> validateSchemeWithOuterScope context path binderId (Set.fromList parameters) scheme
    classMethodSchemeShapeFailures methodParameters evidenceParameters primitiveConstraints =
      let obligationCount = length methodParameters + length evidenceParameters + length primitiveConstraints
       in if obligationCount == 0
            then []
            else [failure path TypedBindingValueMismatch (TypedArityDetail 0 obligationCount)]

visibleClassCollisionFailures :: ModuleContext -> TypedCoreValidationPath -> TypedCoreName -> [TypedCoreValidationFailure]
visibleClassCollisionFailures context path name =
  case localCapabilityIdentifier of
    Just identifier
      | any (externalCapabilityMatches identifier) (Set.toList (moduleContextVisibleNames context)) ->
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
validateImplDeclaration context statementLocation path (TypedImplDeclaration _ implId methods) =
  implOwnerFailures
    <> validateImplId context path Set.empty implId
    <> concatMap (validateDataTypeApplications context path) (implTargetTypes implId)
    <> duplicateImplMethodFailures path methods
    <> missingImplMethodFailures context path implId methods
    <> concatMap (uncurry validateMethod) (zip [0 ..] methods)
  where
    implOwnerFailures
      | implModulePath implId == moduleContextPath context = []
      | otherwise = [failure path TypedInvisibleImpl (TypedImplDetail implId)]
    validateMethod methodIndex (TypedMethodDefinition methodId@(TypedMethodId methodImplId methodKey) binderId name _ expression) =
      validateMethodId context path Set.empty methodId
        <> (if methodImplId == implId then [] else [failure path TypedMethodSelectionMismatch (TypedImplDetail methodImplId)])
        <> validateLocalDefinitionName context [TypedValueNamespace] path name
        <> validateBinderDefinition context path binderId name
        <> (if coreNameIdentifier name == Just methodKey then [] else [failure path TypedMethodSelectionMismatch (TypedTextDetail methodKey)])
        <> validateImplMethodContract context path implId methodKey (expressionInfo expression)
        <> validateExpression (implMethodContext context implId methodKey) statementLocation [methodIndex] expression

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

validateImplMethodContract :: ModuleContext -> TypedCoreValidationPath -> TypedImplId -> Text -> TypedNodeInfo -> [TypedCoreValidationFailure]
validateImplMethodContract context path implId methodKey info =
  case lookupImplMethodScheme context implId methodKey of
    Left () -> []
    Right Nothing -> [failure path TypedMethodSelectionMismatch (TypedTextDetail methodKey)]
    Right (Just (classParameters, TypedScheme owner _ _ _ resultType resultRecipe))
      | length classParameters == length targets ->
          validateValueContract
            path
            info
            ( ValueContract
                (substituteTypeParameters substitutions qualifiedType)
                (substituteRepresentationParameters substitutions qualifiedRecipe)
            )
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
    Right (Just (classParameters, scheme)) ->
      withTypeScope classParameters (withSchemeScope scheme context)
    _ -> context

lookupImplMethodScheme :: ModuleContext -> TypedImplId -> Text -> Either () (Maybe ([TypedTypeParameterId], TypedScheme))
lookupImplMethodScheme context (TypedImplId _ capability _) methodKey =
  case resolvedNameKey (moduleContextPath context) capability >>= (`Map.lookup` moduleContextCapabilityContracts context) of
    Nothing -> Left ()
    Just (CapabilityContract parameters methods) ->
      Right (fmap (\scheme -> (parameters, scheme)) (Map.lookup methodKey methods))

validateExpression :: ModuleContext -> [Int] -> [Int] -> TypedExpr -> [TypedCoreValidationFailure]
validateExpression context statementLocation expressionPath =
  validateExpressionWithParentSpan context statementLocation expressionPath Nothing

validateExpressionWithParentSpan :: ModuleContext -> [Int] -> [Int] -> Maybe TypedSpan -> TypedExpr -> [TypedCoreValidationFailure]
validateExpressionWithParentSpan context statementLocation expressionPath parentExplicitSpan expression =
  validateNodeInfo context path (moduleContextTypeScope context) (qualifiedMethodCandidateKey expression) (expressionInfo expression)
    <> expressionOwnedFailures
    <> concatMap (uncurry validateChild) (zip [0 ..] (expressionChildrenWithContexts context expression))
  where
    statementIndex = statementIndexFor context statementLocation
    path = TypedExpressionPath (moduleContextPath context) statementIndex expressionPath
    validateChild childIndex (childContext, child) =
      validateExpressionWithParentSpan
        childContext
        statementLocation
        (expressionPath <> [childIndex])
        (childExplicitSpan childIndex)
        child
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
          TypedVariableExpr info name -> validateVariableExpression context path info name
          TypedLambdaExpr info binderId name body -> validateLocalDefinitionName context [TypedValueNamespace] path name <> validateBinderDefinition context path binderId name <> validateLambda path info body
          TypedOperatorValueExpr info operator -> validateOperatorValue context path info operator
          TypedListExpr info expressions -> validateListShape path info expressions
          TypedTupleExpr info expressions -> validateTupleShape path info expressions
          TypedApplyExpr info function argument -> validateApplication path info function argument
          TypedTypeApplicationExpr info function explicitSpan typeArgument ->
            validateType path (moduleContextTypeScope context) typeArgument
              <> validateDataTypeApplications context path typeArgument
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
                  <> validateStatementsInOrder context locatedStatements

validateBlockResult :: TypedCoreValidationPath -> TypedNodeInfo -> [TypedStatement] -> [TypedCoreValidationFailure]
validateBlockResult path blockInfo statements =
  case reverse statements of
    TypedExpressionStatement _ terminal : _
      | nodeInfoHasValidIntrinsicContract blockInfo && nodeInfoHasValidIntrinsicContract (expressionInfo terminal) ->
          nodeContractFailures path TypedBlockResultMismatch blockInfo (expressionInfo terminal)
      | otherwise -> []
    _ -> [failure path TypedBlockResultMismatch TypedNoValidationDetail]

validateLambda :: TypedCoreValidationPath -> TypedNodeInfo -> TypedExpr -> [TypedCoreValidationFailure]
validateLambda path info body =
  case nodeType info of
    TypedFunctionType _ expectedResult
      | expectedResult == nodeType (expressionInfo body) -> []
      | otherwise -> [failure path TypedLambdaResultMismatch (TypedTypeDetail expectedResult (nodeType (expressionInfo body)))]
    actual -> [failure path TypedLambdaResultMismatch (TypedTypeDetail (TypedFunctionType (nodeType (expressionInfo body)) (nodeType (expressionInfo body))) actual)]

validateLiteral :: TypedCoreValidationPath -> TypedNodeInfo -> TypedLiteral -> [TypedCoreValidationFailure]
validateLiteral path info literal
  | literalMatchesType literal (nodeType info) = []
  | otherwise = [failure path TypedLiteralTypeMismatch (TypedTypeDetail (literalType literal) (nodeType info))]

literalMatchesType :: TypedLiteral -> TypedType -> Bool
literalMatchesType literal typeValue =
  case (literal, typeValue) of
    (TypedIntegerLiteral value, TypedIntType) -> integerLiteralFitsType value typeValue
    (TypedIntegerLiteral value, TypedNumericType numericType) ->
      not (isFloatingNumericType numericType)
        && integerLiteralFitsType value typeValue
    (TypedFractionalLiteral whole fractional Nothing, TypedFloatType) ->
      fractionalLiteralFitsNumericType whole fractional NumericFloat64
    (TypedFractionalLiteral whole fractional Nothing, TypedNumericType numericType) ->
      isFloatingNumericType numericType
        && fractionalLiteralFitsNumericType whole fractional (numericTypeFromTyped numericType)
    (TypedFractionalLiteral whole fractional (Just expectedType), TypedNumericType actualType) ->
      expectedType == actualType
        && isFloatingNumericType actualType
        && fractionalLiteralFitsNumericType whole fractional (numericTypeFromTyped actualType)
    (TypedBooleanLiteral _, TypedBoolType) -> True
    (TypedCharacterLiteral _, TypedCharType) -> True
    (TypedTextLiteral _, TypedTextType) -> True
    _ -> False

integerLiteralFitsType :: Text -> TypedType -> Bool
integerLiteralFitsType value typeValue =
  case (parseDecimalBound value, integralTypeBounds typeValue) of
    (Just parsedValue, Just (minimumValue, maximumValue)) ->
      minimumValue <= parsedValue && parsedValue <= maximumValue
    _ -> False

fractionalLiteralFitsNumericType :: Text -> Text -> NumericType -> Bool
fractionalLiteralFitsNumericType whole fractional numericType =
  case (parseDecimalBound whole, parseDecimalMagnitude fractional, numericTypeFloatMax numericType) of
    (Just wholeValue, Just fractionalValue, Just maximumMagnitude) ->
      let scale = 10 ^ Text.length fractional
          magnitude = ((abs wholeValue * scale) + fractionalValue) % scale
       in magnitude <= toRational maximumMagnitude
    _ -> False

numericTypeFromTyped :: TypedNumericType -> NumericType
numericTypeFromTyped numericType =
  case numericType of
    TypedInt8Type -> NumericInt8
    TypedInt16Type -> NumericInt16
    TypedInt32Type -> NumericInt32
    TypedInt64Type -> NumericInt64
    TypedUInt8Type -> NumericUInt8
    TypedUInt16Type -> NumericUInt16
    TypedUInt32Type -> NumericUInt32
    TypedUInt64Type -> NumericUInt64
    TypedFloat16Type -> NumericFloat16
    TypedFloat32Type -> NumericFloat32
    TypedFloat64Type -> NumericFloat64

literalType :: TypedLiteral -> TypedType
literalType literal =
  case literal of
    TypedIntegerLiteral _ -> TypedIntType
    TypedFractionalLiteral _ _ Nothing -> TypedFloatType
    TypedFractionalLiteral _ _ (Just numericType) -> TypedNumericType numericType
    TypedBooleanLiteral _ -> TypedBoolType
    TypedCharacterLiteral _ -> TypedCharType
    TypedTextLiteral _ -> TypedTextType

isFloatingNumericType :: TypedNumericType -> Bool
isFloatingNumericType numericType = numericType `elem` [TypedFloat16Type, TypedFloat32Type, TypedFloat64Type]

validateListShape :: TypedCoreValidationPath -> TypedNodeInfo -> [TypedExpr] -> [TypedCoreValidationFailure]
validateListShape path info expressions =
  case nodeType info of
    TypedListType elementType ->
      [ failure path TypedCollectionShapeMismatch (TypedTypeDetail elementType actualType)
      | expression <- expressions,
        let actualType = nodeType (expressionInfo expression),
        actualType /= elementType
      ]
    actual -> [failure path TypedCollectionShapeMismatch (TypedTypeDetail (TypedListType actual) actual)]

validateTupleShape :: TypedCoreValidationPath -> TypedNodeInfo -> [TypedExpr] -> [TypedCoreValidationFailure]
validateTupleShape path info expressions =
  case nodeType info of
    TypedTupleType expectedTypes
      | length expectedTypes /= length expressions ->
          [failure path TypedCollectionShapeMismatch (TypedArityDetail (length expectedTypes) (length expressions))]
      | otherwise ->
          [ failure path TypedCollectionShapeMismatch (TypedTypeDetail expectedType actualType)
          | (expectedType, expression) <- zip expectedTypes expressions,
            let actualType = nodeType (expressionInfo expression),
            actualType /= expectedType
          ]
    actual -> [failure path TypedCollectionShapeMismatch (TypedTypeDetail (TypedTupleType []) actual)]

validateExplicitTypeApplication :: ModuleContext -> TypedCoreValidationPath -> TypedNodeInfo -> TypedExpr -> TypedSpan -> TypedType -> [TypedCoreValidationFailure]
validateExplicitTypeApplication context path info function explicitSpan typeArgument =
  case function of
    TypedVariableExpr _ name -> validateSchemeApplication (lookupSchemeByName context name)
    TypedOperatorValueExpr _ (TypedResolvedOperator name _) -> validateSchemeApplication (lookupSchemeByName context name)
    _ -> [failure path TypedInstantiationMismatch TypedNoValidationDetail]
  where
    instantiations = nodeInfoInstantiations info
    validateSchemeApplication maybeScheme =
      case maybeScheme of
        Just scheme@(TypedScheme owner (firstParameter : _) _ _ _ _)
          | any (matchingExplicitInstantiation owner firstParameter) instantiations ->
              validateInstantiatedResult scheme
          | otherwise -> [failure path TypedInstantiationMismatch (TypedBinderDetail owner)]
        Just (TypedScheme owner [] _ _ _ _) ->
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
        Just (ValueContract expectedType expectedRecipeValue)
          | nodeType info /= expectedType ->
              [failure path TypedApplicationResultMismatch (TypedTypeDetail expectedType (nodeType info))]
          | nodeRecipe info /= expectedRecipeValue ->
              [failure path TypedApplicationResultMismatch (TypedRecipeDetail expectedRecipeValue (nodeRecipe info))]
          | otherwise -> []
        Nothing -> []

expressionInstantiationOwners :: ModuleContext -> TypedExpr -> Set TypedBinderId
expressionInstantiationOwners context expression =
  case expression of
    TypedVariableExpr _ name ->
      Set.fromList
        ( [owner | TypedScheme owner _ _ _ _ _ <- maybeToList (lookupSchemeByName context name)]
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
      Set.fromList [owner | TypedScheme owner _ _ _ _ _ <- maybeToList (lookupSchemeByName context name)]

bindingExpressionInstantiationOwners :: ModuleContext -> TypedExpr -> Set TypedBinderId
bindingExpressionInstantiationOwners context expression =
  case expression of
    TypedVariableExpr _ name ->
      Set.fromList
        ( [owner | TypedScheme owner _ _ _ _ _ <- maybeToList (lookupSchemeByName context name)]
            <> [owner | ConstructorContract owner _ _ _ <- maybeToList (lookupConstructorContract context name)]
        )
    TypedOperatorValueExpr _ operator -> operatorSchemeOwners context operator
    _ -> Set.empty

validateExpressionInstantiationOwners :: Maybe TypedSpan -> ModuleContext -> TypedCoreValidationPath -> TypedExpr -> [TypedCoreValidationFailure]
validateExpressionInstantiationOwners parentExplicitSpan context path expression =
  case expression of
    TypedTypeApplicationExpr _ function _ _ ->
      let allowedOwners = bindingExpressionInstantiationOwners context function
       in if Set.null allowedOwners
            then []
            else
              [ failure path TypedInstantiationMismatch (TypedBinderDetail owner)
              | TypedInstantiation owner _ _ <- nodeInfoInstantiations (expressionInfo expression),
                _ <- maybeToList (lookupInstantiationContract context owner),
                Set.notMember owner allowedOwners
              ]
    _ ->
      [ failure path TypedInstantiationMismatch (TypedBinderDetail owner)
      | TypedInstantiation owner _ maybeSpan <- nodeInfoInstantiations (expressionInfo expression),
        _ <- maybeToList (lookupInstantiationContract context owner),
        Set.notMember owner (expressionInstantiationOwners context expression)
          || case maybeSpan of
            Just explicitSpan -> parentExplicitSpan /= Just explicitSpan
            Nothing -> False
      ]

lookupSchemeByName :: ModuleContext -> TypedCoreName -> Maybe TypedScheme
lookupSchemeByName context name = do
  expectedKey <- resolvedNameKey (moduleContextPath context) name
  let matchingSchemes =
        [ (owner, scheme)
        | (owner, scheme) <- Map.toList (moduleContextSchemes context),
          binderDefinitionKey owner == Just expectedKey
        ]
  case reverse (sortOn (binderScopeRank . fst) matchingSchemes) of
    (_, scheme) : _ -> Just scheme
    [] -> Nothing

binderScopeRank :: TypedBinderId -> (Int, [Int])
binderScopeRank (TypedBinderId (_, lexicalPath, _)) = (length lexicalPath, lexicalPath)

binderDefinitionKey :: TypedBinderId -> Maybe ResolvedNameKey
binderDefinitionKey (TypedBinderId (modulePath, _, name)) = definitionNameKey modulePath name

nodeInfoInstantiations :: TypedNodeInfo -> [TypedInstantiation]
nodeInfoInstantiations (TypedNodeInfo _ _ instantiations _) = instantiations

validateVariableExpression :: ModuleContext -> TypedCoreValidationPath -> TypedNodeInfo -> TypedCoreName -> [TypedCoreValidationFailure]
validateVariableExpression context path info name =
  validateVisibleNameInNamespaces [TypedValueNamespace, TypedConstructorNamespace] context path name
    <> case name of
      TypedBuiltinName identifier -> validateBuiltinValueContract context path info identifier
      _ ->
        case resolvedNameKey (moduleContextPath context) name >>= (`Map.lookup` moduleContextLexicalContracts context) of
          Just contract -> validateValueContract path info contract
          Nothing ->
            case name of
              TypedResolvedName _ TypedValueNamespace _ ->
                maybe [] (validateVariableSchemeContract context path info) (lookupSchemeByName context name)
              TypedResolvedName _ TypedConstructorNamespace _ ->
                maybe [] (validateConstructorExpressionContract context path info) (lookupConstructorContract context name)
              _ -> []

validateVariableSchemeContract :: ModuleContext -> TypedCoreValidationPath -> TypedNodeInfo -> TypedScheme -> [TypedCoreValidationFailure]
validateVariableSchemeContract context path info scheme@(TypedScheme owner parameters evidenceParameters _ _ _) =
  instantiationFailures
    <> missingEvidenceWithoutInstantiation
    <> case schemeValueContract context info scheme of
      Just contract -> validateValueContract path info contract
      Nothing -> []
  where
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

validateBuiltinValueContract :: ModuleContext -> TypedCoreValidationPath -> TypedNodeInfo -> Text -> [TypedCoreValidationFailure]
validateBuiltinValueContract context path info identifier =
  case lookupTypedBuiltinSymbol identifier of
    Nothing -> []
    Just builtinSymbol ->
      case builtinConcreteValueType builtinSymbol of
        Just expectedType ->
          case expectedRecipe expectedType of
            Just expectedRecipeValue ->
              validateValueContract path info (ValueContract expectedType expectedRecipeValue)
            Nothing -> [failure path TypedBindingValueMismatch (TypedTextDetail identifier)]
        Nothing
          | builtinPolymorphicValueTypeMatches context builtinSymbol (nodeType info) -> []
          | otherwise -> [failure path TypedBindingValueMismatch (TypedTextDetail identifier)]

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
schemeValueContract context info (TypedScheme owner parameters _ _ resultType resultRecipe) =
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
  | nodeType info /= expectedType =
      [failure path TypedBindingValueMismatch (TypedTypeDetail expectedType (nodeType info))]
  | nodeRecipe info /= expectedRecipeValue =
      [failure path TypedBindingValueMismatch (TypedRecipeDetail expectedRecipeValue (nodeRecipe info))]
  | otherwise = []

lookupConstructorContract :: ModuleContext -> TypedCoreName -> Maybe ConstructorContract
lookupConstructorContract context name = do
  key <- resolvedNameKey (moduleContextPath context) name
  Map.lookup key (moduleContextConstructorContracts context)

validateConstructorExpressionContract :: ModuleContext -> TypedCoreValidationPath -> TypedNodeInfo -> ConstructorContract -> [TypedCoreValidationFailure]
validateConstructorExpressionContract context path info (ConstructorContract owner dataKey parameters fieldTypes) =
  validateValueContract path info (ValueContract expectedType expectedRecipeValue)
  where
    genericResult = TypedDataType (resolvedNameFromKey context dataKey) (map TypedTypeParameterType parameters)
    genericType = foldr TypedFunctionType genericResult fieldTypes
    substitutions =
      case find (matchingInstantiation owner parameters) (nodeInfoInstantiations info) of
        Just (TypedInstantiation _ arguments _) ->
          Map.fromList [(parameterId, typeValue) | TypedTypeArgument parameterId typeValue <- arguments]
        Nothing -> inferConstructorSubstitutions context dataKey parameters (length fieldTypes) (nodeType info)
    expectedType = substituteTypeParameters substitutions genericType
    expectedRecipeValue = maybe (nodeRecipe info) id (expectedRecipe expectedType)

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

resolvedNameFromKey :: ModuleContext -> ResolvedNameKey -> TypedCoreName
resolvedNameFromKey _ (GeneratedNameKey kind) = TypedGeneratedName kind
resolvedNameFromKey context (ResolvedNameKey modulePath namespace identifier) =
  TypedResolvedName origin namespace identifier
  where
    origin
      | modulePath == moduleContextPath context = TypedCurrentModule
      | modulePath == ["Prelude"] = TypedAmbientPrelude
      | otherwise = TypedImportedModule modulePath

binderModulePath :: TypedBinderId -> [Text]
binderModulePath (TypedBinderId (modulePath, _, _)) = modulePath

expressionChildrenWithContexts :: ModuleContext -> TypedExpr -> [(ModuleContext, TypedExpr)]
expressionChildrenWithContexts context expression =
  case expression of
    TypedLambdaExpr info _ name body ->
      [ ( case lambdaArgumentContract info of
            Just contract -> withLexicalContracts [(name, contract)] context
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
    _ -> []

lambdaArgumentContract :: TypedNodeInfo -> Maybe ValueContract
lambdaArgumentContract info =
  case nodeType info of
    TypedFunctionType argumentType _ -> ValueContract argumentType <$> expectedRecipe argumentType
    _ -> Nothing

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
validateApplication path (TypedNodeInfo resultType _ _ _) function argument =
  case nodeType (expressionInfo function) of
    TypedFunctionType expectedArgument expectedResult ->
      argumentFailures expectedArgument <> resultFailures expectedResult
    actualFunctionType ->
      [ failure
          path
          TypedApplicationFunctionMismatch
          (TypedTypeDetail (TypedFunctionType (nodeType (expressionInfo argument)) resultType) actualFunctionType)
      ]
  where
    actualArgument = nodeType (expressionInfo argument)
    argumentFailures expected
      | expected == actualArgument = []
      | otherwise = [failure path TypedApplicationArgumentMismatch (TypedTypeDetail expected actualArgument)]
    resultFailures expected
      | expected == resultType = []
      | otherwise = [failure path TypedApplicationResultMismatch (TypedTypeDetail expected resultType)]

validateConditional :: TypedCoreValidationPath -> TypedNodeInfo -> TypedExpr -> TypedExpr -> TypedExpr -> [TypedCoreValidationFailure]
validateConditional path (TypedNodeInfo resultType _ _ _) condition thenExpression elseExpression =
  conditionFailures <> branchFailures <> resultFailures
  where
    conditionType = nodeType (expressionInfo condition)
    thenType = nodeType (expressionInfo thenExpression)
    elseType = nodeType (expressionInfo elseExpression)
    conditionFailures
      | conditionType == TypedBoolType = []
      | otherwise = [failure path TypedConditionalConditionMismatch (TypedTypeDetail TypedBoolType conditionType)]
    branchFailures
      | thenType == elseType = []
      | otherwise = [failure path TypedConditionalBranchMismatch (TypedTypeDetail thenType elseType)]
    resultFailures
      | thenType /= elseType || resultType == thenType = []
      | otherwise = [failure path TypedConditionalBranchMismatch (TypedTypeDetail thenType resultType)]

validateCase :: ModuleContext -> [Int] -> [Int] -> TypedCoreValidationPath -> TypedNodeInfo -> TypedExpr -> [TypedCaseArm] -> [TypedCoreValidationFailure]
validateCase context statementLocation expressionPath path (TypedNodeInfo resultType _ _ _) scrutinee arms =
  emptyArmFailures <> concatMap (uncurry validateArm) (zip [0 ..] arms)
  where
    scrutineeType = nodeType (expressionInfo scrutinee)
    emptyArmFailures
      | null arms = [failure path TypedPatternShapeMismatch (TypedArityDetail 1 0)]
      | otherwise = []
    validateArm armIndex (TypedCaseArm patternValue maybeGuard resultExpression) =
      duplicatePatternNameFailures armIndex patternValue
        <> validatePattern context statementLocation (expressionPath <> [armIndex]) scrutineeType patternValue
        <> guardFailures armIndex maybeGuard
        <> resultFailures armIndex resultExpression
    duplicatePatternNameFailures armIndex =
      snd . foldl' duplicateNameStep (Set.empty, []) . patternBinderContract
      where
        armPath =
          TypedPatternPath
            (moduleContextPath context)
            (statementIndexFor context statementLocation)
            (expressionPath <> [armIndex])
        duplicateNameStep (seen, failures) (PatternBinderContract binderId name _ _)
          | Set.member name seen =
              (seen, failures <> [failure armPath TypedDuplicateBinder (TypedBinderDetail binderId)])
          | otherwise = (Set.insert name seen, failures)
    guardFailures _ Nothing = []
    guardFailures armIndex (Just guard)
      | nodeType (expressionInfo guard) == TypedBoolType = []
      | otherwise =
          [ failure
              (TypedPatternPath (moduleContextPath context) (statementIndexFor context statementLocation) (expressionPath <> [armIndex]))
              TypedPatternGuardMismatch
              (TypedTypeDetail TypedBoolType (nodeType (expressionInfo guard)))
          ]
    resultFailures armIndex resultExpression
      | nodeType (expressionInfo resultExpression) == resultType = []
      | otherwise =
          [ failure
              (TypedPatternPath (moduleContextPath context) (statementIndexFor context statementLocation) (expressionPath <> [armIndex]))
              TypedPatternArmResultMismatch
              (TypedTypeDetail resultType (nodeType (expressionInfo resultExpression)))
          ]

validatePattern :: ModuleContext -> [Int] -> [Int] -> TypedType -> TypedPattern -> [TypedCoreValidationFailure]
validatePattern context statementLocation patternPath expectedType patternValue =
  validateNodeInfo context path (moduleContextTypeScope context) Nothing (patternInfo patternValue)
    <> validatePatternMetadata path (patternInfo patternValue)
    <> scrutineeFailures
    <> patternOwnedFailures
    <> concatMap validateChild (patternChildrenWithTypes context patternValue)
  where
    path = TypedPatternPath (moduleContextPath context) (statementIndexFor context statementLocation) patternPath
    actualType = nodeType (patternInfo patternValue)
    scrutineeFailures
      | actualType == expectedType = []
      | otherwise = [failure path TypedPatternScrutineeMismatch (TypedTypeDetail expectedType actualType)]
    patternOwnedFailures =
      case patternValue of
        TypedVariablePattern _ binderId name -> validateLocalDefinitionName context [TypedValueNamespace] path name <> validateBinderDefinition context path binderId name
        TypedLiteralPattern info literal -> validateLiteral path info literal
        TypedConstructorPattern info name patterns ->
          validateVisibleNameInNamespaces [TypedConstructorNamespace] context path name
            <> validateConstructorPatternShape context path info name patterns
        TypedListPattern info _ -> validateListPatternShape path info
        TypedConsListPattern info _ _ -> validateListPatternShape path info
        TypedAsPattern _ binderId name _ -> validateLocalDefinitionName context [TypedValueNamespace] path name <> validateBinderDefinition context path binderId name
        TypedOrPattern _ alternatives -> validateOrPattern path alternatives
        TypedTuplePattern info patterns -> validateTuplePatternShape path info patterns
        _ -> []
    validateChild (childIndex, childType, childPattern) =
      validatePattern context statementLocation (patternPath <> [childIndex]) childType childPattern

validatePatternMetadata :: TypedCoreValidationPath -> TypedNodeInfo -> [TypedCoreValidationFailure]
validatePatternMetadata path (TypedNodeInfo _ _ instantiations evidenceSelections)
  | null instantiations && null evidenceSelections = []
  | otherwise = [failure path TypedPatternShapeMismatch TypedNoValidationDetail]

validateTuplePatternShape :: TypedCoreValidationPath -> TypedNodeInfo -> [TypedPattern] -> [TypedCoreValidationFailure]
validateTuplePatternShape path info patterns =
  case nodeType info of
    TypedTupleType types
      | length types == length patterns -> []
      | otherwise -> [failure path TypedPatternShapeMismatch (TypedArityDetail (length types) (length patterns))]
    actualType -> [failure path TypedPatternShapeMismatch (TypedTypeDetail (TypedTupleType []) actualType)]

validateListPatternShape :: TypedCoreValidationPath -> TypedNodeInfo -> [TypedCoreValidationFailure]
validateListPatternShape path info =
  case nodeType info of
    TypedListType _ -> []
    actualType -> [failure path TypedPatternShapeMismatch (TypedTypeDetail (TypedListType actualType) actualType)]

validateConstructorPatternShape :: ModuleContext -> TypedCoreValidationPath -> TypedNodeInfo -> TypedCoreName -> [TypedPattern] -> [TypedCoreValidationFailure]
validateConstructorPatternShape context path info name patterns =
  case constructorPatternFieldTypes context info name of
    Just fieldTypes
      | length fieldTypes == length patterns -> []
      | otherwise -> [failure path TypedPatternShapeMismatch (TypedArityDetail (length fieldTypes) (length patterns))]
    Nothing ->
      case constructorPatternExpectedType context name of
        Just expectedType -> [failure path TypedPatternShapeMismatch (TypedTypeDetail expectedType (nodeType info))]
        Nothing -> []

constructorPatternExpectedType :: ModuleContext -> TypedCoreName -> Maybe TypedType
constructorPatternExpectedType context constructorName = do
  constructorKey <- resolvedNameKey (moduleContextPath context) constructorName
  ConstructorContract _ dataKey parameters _ <- Map.lookup constructorKey (moduleContextConstructorContracts context)
  pure (TypedDataType (resolvedNameFromKey context dataKey) (map TypedTypeParameterType parameters))

patternChildrenWithTypes :: ModuleContext -> TypedPattern -> [(Int, TypedType, TypedPattern)]
patternChildrenWithTypes context patternValue =
  case patternValue of
    TypedConstructorPattern info name patterns ->
      case constructorPatternFieldTypes context info name of
        Just fieldTypes -> [(index, fieldType, pattern') | (index, (fieldType, pattern')) <- zip [0 ..] (zip fieldTypes patterns)]
        Nothing -> indexedPatternTypes patterns
    TypedListPattern info patterns ->
      case nodeType info of
        TypedListType elementType -> [(index, elementType, pattern') | (index, pattern') <- zip [0 ..] patterns]
        _ -> indexedPatternTypes patterns
    TypedConsListPattern info headPattern tailPattern ->
      case nodeType info of
        listType@(TypedListType elementType) -> [(0, elementType, headPattern), (1, listType, tailPattern)]
        otherType -> [(0, otherType, headPattern), (1, otherType, tailPattern)]
    TypedTuplePattern info patterns ->
      case nodeType info of
        TypedTupleType types ->
          [ (index, typeValue, pattern')
          | (index, (typeValue, pattern')) <- zip [0 ..] (zip types patterns)
          ]
        _ -> indexedPatternTypes patterns
    TypedAsPattern info _ _ nested -> [(0, nodeType info, nested)]
    TypedOrPattern info alternatives -> [(index, nodeType info, alternative) | (index, alternative) <- zip [0 ..] alternatives]
    _ -> []
  where
    indexedPatternTypes patterns =
      [(index, nodeType (patternInfo pattern'), pattern') | (index, pattern') <- zip [0 ..] patterns]

constructorPatternFieldTypes :: ModuleContext -> TypedNodeInfo -> TypedCoreName -> Maybe [TypedType]
constructorPatternFieldTypes context info constructorName = do
  constructorKey <- resolvedNameKey (moduleContextPath context) constructorName
  ConstructorContract _ dataKey parameters fieldTypes <- Map.lookup constructorKey (moduleContextConstructorContracts context)
  case nodeType info of
    TypedDataType dataName arguments -> do
      actualDataKey <- resolvedNameKey (moduleContextPath context) dataName
      if actualDataKey == dataKey && length parameters == length arguments
        then pure (map (substituteTypeParameters (Map.fromList (zip parameters arguments))) fieldTypes)
        else Nothing
    _ -> Nothing

substituteTypeParameters :: Map TypedTypeParameterId TypedType -> TypedType -> TypedType
substituteTypeParameters substitutions typeValue =
  case typeValue of
    TypedListType elementType -> TypedListType (substituteTypeParameters substitutions elementType)
    TypedTupleType elementTypes -> TypedTupleType (map (substituteTypeParameters substitutions) elementTypes)
    TypedDataType name arguments -> TypedDataType name (map (substituteTypeParameters substitutions) arguments)
    TypedFunctionType argument result -> TypedFunctionType (substituteTypeParameters substitutions argument) (substituteTypeParameters substitutions result)
    TypedTypeParameterType parameterId -> Map.findWithDefault typeValue parameterId substitutions
    _ -> typeValue

substituteRepresentationParameters :: Map TypedTypeParameterId TypedType -> TypedRepresentationRecipe -> TypedRepresentationRecipe
substituteRepresentationParameters substitutions recipe =
  case recipe of
    TypedManagedListRecipe elementRecipe -> TypedManagedListRecipe (substituteRepresentationParameters substitutions elementRecipe)
    TypedManagedProductRecipe elementRecipes -> TypedManagedProductRecipe (map (substituteRepresentationParameters substitutions) elementRecipes)
    TypedManagedVariantRecipe name arguments -> TypedManagedVariantRecipe name (map (substituteTypeParameters substitutions) arguments)
    TypedClosureRecipe parameters result ->
      TypedClosureRecipe
        (map (substituteRepresentationParameters substitutions) parameters)
        (substituteRepresentationParameters substitutions result)
    TypedRepresentationParameterRecipe parameterId ->
      case Map.lookup parameterId substitutions >>= expectedRecipe of
        Just substituted -> substituted
        Nothing -> recipe
    _ -> recipe

validateOrPattern :: TypedCoreValidationPath -> [TypedPattern] -> [TypedCoreValidationFailure]
validateOrPattern path [] = [failure path TypedPatternShapeMismatch (TypedArityDetail 1 0)]
validateOrPattern path (firstAlternative : rest) = concatMap compareAlternative rest
  where
    expected = normalizedPatternBinderContract firstAlternative
    compareAlternative alternative
      | patternBinderContractsEqual expected actual = []
      | otherwise =
          case firstMismatchedBinder expected actual of
            Just binderId -> [failure path TypedOrPatternBinderMismatch (TypedBinderDetail binderId)]
            Nothing -> [failure path TypedOrPatternBinderMismatch TypedNoValidationDetail]
      where
        actual = normalizedPatternBinderContract alternative

normalizedPatternBinderContract :: TypedPattern -> [PatternBinderContract]
normalizedPatternBinderContract =
  sortOn contractName . patternBinderContract
  where
    contractName (PatternBinderContract _ name _ _) = name

patternBinderContract :: TypedPattern -> [PatternBinderContract]
patternBinderContract patternValue =
  case patternValue of
    TypedVariablePattern info binderId name ->
      [PatternBinderContract binderId name (nodeType info) (nodeRecipe info)]
    TypedConstructorPattern _ _ patterns -> concatMap patternBinderContract patterns
    TypedListPattern _ patterns -> concatMap patternBinderContract patterns
    TypedConsListPattern _ headPattern tailPattern -> patternBinderContract headPattern <> patternBinderContract tailPattern
    TypedTuplePattern _ patterns -> concatMap patternBinderContract patterns
    TypedAsPattern info binderId name nested ->
      PatternBinderContract binderId name (nodeType info) (nodeRecipe info)
        : patternBinderContract nested
    TypedOrPattern _ [] -> []
    TypedOrPattern _ (alternative : _) -> patternBinderContract alternative
    _ -> []

patternBoundContracts :: TypedPattern -> [(TypedCoreName, ValueContract)]
patternBoundContracts patternValue =
  case patternValue of
    TypedVariablePattern info _ name -> [(name, valueContractFromInfo info)]
    TypedConstructorPattern _ _ patterns -> concatMap patternBoundContracts patterns
    TypedListPattern _ patterns -> concatMap patternBoundContracts patterns
    TypedConsListPattern _ headPattern tailPattern -> patternBoundContracts headPattern <> patternBoundContracts tailPattern
    TypedTuplePattern _ patterns -> concatMap patternBoundContracts patterns
    TypedAsPattern info _ name nested -> (name, valueContractFromInfo info) : patternBoundContracts nested
    TypedOrPattern _ [] -> []
    TypedOrPattern _ (alternative : _) -> patternBoundContracts alternative
    _ -> []

valueContractFromInfo :: TypedNodeInfo -> ValueContract
valueContractFromInfo info = ValueContract (nodeType info) (nodeRecipe info)

patternBinderContractsEqual :: [PatternBinderContract] -> [PatternBinderContract] -> Bool
patternBinderContractsEqual expected actual =
  length expected == length actual
    && and (zipWith contractEqual expected actual)
  where
    contractEqual
      (PatternBinderContract _ expectedName expectedType expectedRecipeValue)
      (PatternBinderContract _ actualName actualType actualRecipeValue) =
        expectedName == actualName
          && expectedType == actualType
          && expectedRecipeValue == actualRecipeValue

firstMismatchedBinder :: [PatternBinderContract] -> [PatternBinderContract] -> Maybe TypedBinderId
firstMismatchedBinder expected actual =
  case dropWhile (uncurry contractsEqual) (zip expected actual) of
    (_, PatternBinderContract binderId _ _ _) : _ -> Just binderId
    [] ->
      case drop (length expected) actual of
        PatternBinderContract binderId _ _ _ : _ -> Just binderId
        [] -> Nothing
  where
    contractsEqual
      (PatternBinderContract _ expectedName expectedType expectedRecipeValue)
      (PatternBinderContract _ actualName actualType actualRecipeValue) =
        expectedName == actualName
          && expectedType == actualType
          && expectedRecipeValue == actualRecipeValue

validateNodeInfo :: ModuleContext -> TypedCoreValidationPath -> Set TypedTypeParameterId -> Maybe Text -> TypedNodeInfo -> [TypedCoreValidationFailure]
validateNodeInfo context path parameterScope qualifiedMethodKey (TypedNodeInfo typeValue recipe instantiations evidenceSelections) =
  validateType path parameterScope typeValue
    <> validateDataTypeApplications context path typeValue
    <> validateRecipe path parameterScope recipe
    <> validateTypeRecipe path parameterScope typeValue recipe
    <> concatMap (validateInstantiation context path parameterScope) instantiations
    <> duplicateInstantiationFailures path instantiations
    <> validateEvidenceSelections context path qualifiedMethodKey evidenceSelections
    <> validateEvidenceParameterBindings context path instantiations evidenceSelections
    <> concatMap (validateEvidenceSelectionDataTypes context path) evidenceSelections

duplicateInstantiationFailures :: TypedCoreValidationPath -> [TypedInstantiation] -> [TypedCoreValidationFailure]
duplicateInstantiationFailures path instantiations = snd (foldl' step (Set.empty, []) instantiations)
  where
    step (seen, failures) (TypedInstantiation owner _ _)
      | Set.member owner seen =
          (seen, failures <> [failure path TypedInstantiationMismatch (TypedBinderDetail owner)])
      | otherwise = (Set.insert owner seen, failures)

validateEvidenceSelectionDataTypes :: ModuleContext -> TypedCoreValidationPath -> TypedEvidenceSelection -> [TypedCoreValidationFailure]
validateEvidenceSelectionDataTypes context path selection =
  case selection of
    TypedSelectedEvidence (TypedEvidenceUse _ (TypedCapabilityConstraint _ _ targetType) implId maybeMethodId) ->
      validateDataTypeApplications context path targetType
        <> concatMap (validateDataTypeApplications context path) (implTargetTypes implId)
        <> maybe [] (concatMap (validateDataTypeApplications context path) . implTargetTypes . methodImplId) maybeMethodId
    TypedEvidenceCandidates (TypedCapabilityConstraint _ _ targetType) candidates ->
      validateDataTypeApplications context path targetType
        <> concat
          [ concatMap (validateDataTypeApplications context path) (implTargetTypes implId)
              <> maybe [] (concatMap (validateDataTypeApplications context path) . implTargetTypes . methodImplId) maybeMethodId
          | TypedEvidenceCandidate implId maybeMethodId <- candidates
          ]
  where
    methodImplId (TypedMethodId implId _) = implId

validateDataTypeApplications :: ModuleContext -> TypedCoreValidationPath -> TypedType -> [TypedCoreValidationFailure]
validateDataTypeApplications context path typeValue =
  case typeValue of
    TypedListType elementType -> validateDataTypeApplications context path elementType
    TypedTupleType elementTypes -> concatMap (validateDataTypeApplications context path) elementTypes
    TypedDataType name arguments ->
      concatMap (validateDataTypeApplications context path) arguments
        <> case resolvedNameKey (moduleContextPath context) name >>= (`Map.lookup` moduleContextDataArities context) of
          Nothing -> [failure path TypedDataTypeMismatch (TypedNameDetail name)]
          Just expectedArity
            | expectedArity /= length arguments -> [failure path TypedDataTypeMismatch (TypedArityDetail expectedArity (length arguments))]
            | otherwise -> []
    TypedFunctionType argument result -> validateDataTypeApplications context path argument <> validateDataTypeApplications context path result
    _ -> []

validateInstantiation :: ModuleContext -> TypedCoreValidationPath -> Set TypedTypeParameterId -> TypedInstantiation -> [TypedCoreValidationFailure]
validateInstantiation context path parameterScope (TypedInstantiation owner arguments _) =
  case lookupInstantiationContract context owner of
    Nothing -> [failure path TypedInstantiationMismatch (TypedBinderDetail owner)]
    Just (InstantiationContract contractOwner parameters primitiveConstraints) ->
      if map typeArgumentParameter arguments == parameters
        then
          concatMap
            (\argument -> validateType path parameterScope (typeArgumentType argument) <> validateDataTypeApplications context path (typeArgumentType argument))
            arguments
            <> concatMap
              (validatePrimitiveConstraint context path parameterScope . instantiatePrimitiveConstraint contractOwner substitutions)
              primitiveConstraints
        else [failure path TypedInstantiationMismatch (TypedBinderDetail owner)]
  where
    substitutions =
      Map.fromList
        [ (parameterId, typeValue)
        | TypedTypeArgument parameterId typeValue <- arguments
        ]
    typeArgumentParameter (TypedTypeArgument parameterId _) = parameterId
    typeArgumentType (TypedTypeArgument _ typeValue) = typeValue
    instantiatePrimitiveConstraint schemeOwner typeSubstitutions primitiveConstraint =
      case primitiveConstraint of
        TypedNumericPrimitiveConstraint numericConstraint typeValue ->
          TypedNumericPrimitiveConstraint numericConstraint (instantiateType schemeOwner typeSubstitutions typeValue)
        TypedStrictEqualityPrimitiveConstraint typeValue ->
          TypedStrictEqualityPrimitiveConstraint (instantiateType schemeOwner typeSubstitutions typeValue)
    instantiateType schemeOwner typeSubstitutions typeValue =
      substituteTypeParameters typeSubstitutions qualifiedType
      where
        ownerPath = binderModulePath schemeOwner
        qualifiedType
          | ownerPath == moduleContextPath context = typeValue
          | otherwise = qualifyExternalType ownerPath typeValue

lookupInstantiationContract :: ModuleContext -> TypedBinderId -> Maybe InstantiationContract
lookupInstantiationContract context owner =
  case Map.lookup owner (moduleContextSchemes context) of
    Just (TypedScheme schemeOwner parameters _ primitiveConstraints _ _) ->
      Just (InstantiationContract schemeOwner parameters primitiveConstraints)
    Nothing ->
      case lookupConstructorContractByOwner context owner of
        Just (ConstructorContract constructorOwner _ parameters _) ->
          Just (InstantiationContract constructorOwner parameters [])
        Nothing -> Nothing

lookupConstructorContractByOwner :: ModuleContext -> TypedBinderId -> Maybe ConstructorContract
lookupConstructorContractByOwner context owner =
  find matchesOwner (Map.elems (moduleContextConstructorContracts context))
  where
    matchesOwner (ConstructorContract candidateOwner _ _ _) = candidateOwner == owner

qualifiedMethodCandidateKey :: TypedExpr -> Maybe Text
qualifiedMethodCandidateKey expression =
  case nodeType (expressionInfo expression) of
    TypedFunctionType {} -> qualifiedMethodExpressionKey expression
    _ -> Nothing

qualifiedMethodExpressionKey :: TypedExpr -> Maybe Text
qualifiedMethodExpressionKey expression =
  case expression of
    TypedVariableExpr _ (TypedBuiltinName identifier) -> Just identifier
    TypedApplyExpr _ function _ -> qualifiedMethodExpressionKey function
    TypedTypeApplicationExpr _ function _ _ -> qualifiedMethodExpressionKey function
    _ -> Nothing

validateEvidenceSelections :: ModuleContext -> TypedCoreValidationPath -> Maybe Text -> [TypedEvidenceSelection] -> [TypedCoreValidationFailure]
validateEvidenceSelections context path qualifiedMethodKey selections =
  concatMap validateSelection selections <> duplicateEvidenceUseFailures path selections
  where
    validateSelection selection =
      case selection of
        TypedSelectedEvidence evidenceUse -> validateEvidenceUse context path evidenceUse
        TypedEvidenceCandidates constraint@(TypedCapabilityConstraint capability constraintMethod _) candidates
          | null candidates -> [failure path TypedMissingEvidence (TypedTextDetail capability)]
          | not (qualifiedMethodCandidates capability constraintMethod qualifiedMethodKey) ->
              [failure path TypedAmbiguousEvidence (TypedArityDetail 1 (length candidates))]
          | otherwise -> concatMap (validateEvidenceCandidate context path constraint) candidates
    qualifiedMethodCandidates capability constraintMethod expressionMethod =
      case (constraintMethod, expressionMethod) of
        (Just expectedMethod, Just actualMethod) -> methodKeyMatches capability expectedMethod actualMethod
        _ -> False

validateEvidenceParameterBindings :: ModuleContext -> TypedCoreValidationPath -> [TypedInstantiation] -> [TypedEvidenceSelection] -> [TypedCoreValidationFailure]
validateEvidenceParameterBindings context path instantiations selections =
  missingBindingFailures <> orderedBindingFailures <> concatMap validateSelection selections
  where
    expectedBindings = concatMap expectedBindingsFor instantiations
    orderedExpectedBindings = nub expectedBindings
    suppliedBindings =
      [ (parameterRef, constraint)
      | TypedSelectedEvidence (TypedEvidenceUse (Just parameterRef) constraint _ _) <- selections
      ]
    missingBindingFailures =
      [ failure path TypedMissingEvidence (TypedEvidenceParameterDetail (evidenceParameterRefId parameterRef))
      | (parameterRef, constraint) <- nub expectedBindings,
        (parameterRef, constraint) `notElem` suppliedBindings
      ]
    orderedBindingFailures
      | sort orderedExpectedBindings == sort suppliedBindings =
          [ failure
              path
              TypedInstantiationMismatch
              (TypedEvidenceParameterDetail (evidenceParameterRefId actualParameterRef))
          | (expectedBinding, actualBinding@(actualParameterRef, _)) <-
              zip orderedExpectedBindings suppliedBindings,
            expectedBinding /= actualBinding
          ]
      | otherwise = []
    validateSelection selection =
      case selection of
        TypedSelectedEvidence (TypedEvidenceUse (Just parameterRef) constraint _ _)
          | (parameterRef, constraint) `elem` expectedBindings -> []
          | otherwise ->
              [ failure
                  path
                  TypedInstantiationMismatch
                  (TypedEvidenceParameterDetail (evidenceParameterRefId parameterRef))
              ]
        _ -> []
    expectedBindingsFor (TypedInstantiation owner arguments _) =
      case Map.lookup owner (moduleContextSchemes context) of
        Just (TypedScheme _ parameters evidenceParameters _ _ _)
          | map typeArgumentParameter arguments == parameters ->
              [ ( TypedEvidenceParameterRef owner parameterId,
                  instantiateConstraint owner substitutions constraint
                )
              | TypedEvidenceParameter parameterId constraint <- evidenceParameters
              ]
          where
            substitutions = Map.fromList [(parameterId, typeValue) | TypedTypeArgument parameterId typeValue <- arguments]
        _ -> []
    instantiateConstraint owner substitutions (TypedCapabilityConstraint capability method targetType) =
      TypedCapabilityConstraint capability method (substituteTypeParameters substitutions qualifiedTarget)
      where
        ownerPath = binderModulePath owner
        qualifiedTarget
          | ownerPath == moduleContextPath context = targetType
          | otherwise = qualifyExternalType ownerPath targetType
    typeArgumentParameter (TypedTypeArgument parameterId _) = parameterId

evidenceParameterRefId :: TypedEvidenceParameterRef -> TypedEvidenceParameterId
evidenceParameterRefId (TypedEvidenceParameterRef _ parameterId) = parameterId

duplicateEvidenceUseFailures :: TypedCoreValidationPath -> [TypedEvidenceSelection] -> [TypedCoreValidationFailure]
duplicateEvidenceUseFailures path selections =
  snd (foldl' parameterStep (Set.empty, []) parameterRefs)
    <> snd (foldl' constraintStep (Set.empty, []) unboundConstraints)
  where
    parameterRefs =
      [ parameterRef
      | TypedSelectedEvidence (TypedEvidenceUse (Just parameterRef) _ _ _) <- selections
      ]
    unboundConstraints =
      [ constraint
      | TypedSelectedEvidence (TypedEvidenceUse Nothing constraint _ _) <- selections
      ]
    parameterStep (seen, failures) parameterRef
      | Set.member parameterRef seen =
          ( seen,
            failures
              <> [ failure
                     path
                     TypedDuplicateEvidence
                     (TypedEvidenceParameterDetail (evidenceParameterRefId parameterRef))
                 ]
          )
      | otherwise = (Set.insert parameterRef seen, failures)
    constraintStep (seen, failures) constraint
      | Set.member constraint seen =
          ( seen,
            failures
              <> [ failure
                     path
                     TypedDuplicateEvidence
                     (TypedTextDetail (capabilityConstraintLabel constraint))
                 ]
          )
      | otherwise = (Set.insert constraint seen, failures)

capabilityConstraintLabel :: TypedCapabilityConstraint -> Text
capabilityConstraintLabel (TypedCapabilityConstraint capability maybeMethod _) =
  case maybeMethod of
    Just method -> method
    Nothing -> capability

validateEvidenceUse :: ModuleContext -> TypedCoreValidationPath -> TypedEvidenceUse -> [TypedCoreValidationFailure]
validateEvidenceUse context path (TypedEvidenceUse _ (TypedCapabilityConstraint capability constraintMethod targetType) implId maybeMethodId) =
  validateCapabilityConstraintTarget path scope targetType
    <> validateImplId context path scope implId
    <> capabilityFailures
    <> targetFailures
    <> visibilityFailures
    <> methodFailures
  where
    scope = moduleContextTypeScope context
    capabilityFailures =
      case implId of
        TypedImplId _ capabilityName _
          | coreNameIdentifier capabilityName == Just capability -> []
          | otherwise -> [failure path TypedMethodSelectionMismatch (TypedTextDetail capability)]
    targetFailures =
      case implTargetTypes implId of
        [target]
          | target == targetType -> []
          | otherwise -> [failure path TypedMethodSelectionMismatch (TypedTypeDetail targetType target)]
        _ -> []
    visibilityFailures
      | Set.member implId (moduleContextVisibleImpls context) = []
      | otherwise = [failure path TypedInvisibleImpl (TypedImplDetail implId)]
    methodFailures =
      case (constraintMethod, maybeMethodId) of
        (Nothing, Nothing) -> []
        (Nothing, Just methodId@(TypedMethodId methodImplId methodName)) ->
          validateMethodId context path scope methodId
            <> (if methodImplId == implId then [] else [failure path TypedMethodSelectionMismatch (TypedImplDetail methodImplId)])
            <> [failure path TypedMethodSelectionMismatch (TypedTextDetail methodName)]
        (Just expectedMethod, Nothing) ->
          [failure path TypedMethodSelectionMismatch (TypedTextDetail expectedMethod)]
        (Just expectedMethod, Just methodId@(TypedMethodId methodImplId methodName)) ->
          validateMethodId context path scope methodId
            <> (if methodImplId == implId then [] else [failure path TypedMethodSelectionMismatch (TypedImplDetail methodImplId)])
            <> (if methodKeyMatches capability expectedMethod methodName then [] else [failure path TypedMethodSelectionMismatch (TypedTextDetail expectedMethod)])
            <> capabilityMethodFailures methodName
            <> implMethodFailures methodName
    capabilityMethodFailures methodName =
      case lookupImplMethodScheme context implId methodName of
        Left () -> []
        Right (Just _) -> []
        Right Nothing -> [failure path TypedMethodSelectionMismatch (TypedTextDetail methodName)]
    implMethodFailures methodName =
      case Map.lookup implId (moduleContextImplMethods context) of
        Nothing -> []
        Just methods
          | Set.member methodName methods -> []
          | otherwise -> [failure path TypedMethodSelectionMismatch (TypedTextDetail methodName)]

methodKeyMatches :: Text -> Text -> Text -> Bool
methodKeyMatches capability expected actual =
  case (methodKeyParts expected, methodKeyParts actual) of
    (Just (expectedQualifier, expectedMethod), Just (actualQualifier, actualMethod)) ->
      expectedMethod == actualMethod
        && maybe True (== capability) expectedQualifier
        && maybe True (== capability) actualQualifier
    _ -> False

methodKeyParts :: Text -> Maybe (Maybe Text, Text)
methodKeyParts methodKey
  | Text.null methodKey = Nothing
  | Just (qualifier, methodName) <- splitQualifiedMethodKey methodKey =
      Just (Just qualifier, methodName)
  | [qualifier, methodName] <- Text.splitOn "." methodKey,
    not (Text.null qualifier),
    not (Text.null methodName) =
      Just (Just qualifier, methodName)
  | Text.any (== '.') methodKey = Nothing
  | otherwise = Just (Nothing, methodKey)

validateEvidenceCandidate :: ModuleContext -> TypedCoreValidationPath -> TypedCapabilityConstraint -> TypedEvidenceCandidate -> [TypedCoreValidationFailure]
validateEvidenceCandidate context path constraint (TypedEvidenceCandidate implId maybeMethodId) =
  validateEvidenceUse context path (TypedEvidenceUse Nothing constraint implId maybeMethodId)

validateType :: TypedCoreValidationPath -> Set TypedTypeParameterId -> TypedType -> [TypedCoreValidationFailure]
validateType path scope typeValue =
  case typeValue of
    TypedIntType -> []
    TypedFloatType -> []
    TypedNumericType _ -> []
    TypedBoolType -> []
    TypedCharType -> []
    TypedTextType -> []
    TypedListType elementType -> validateType path scope elementType
    TypedTupleType elementTypes -> concatMap (validateType path scope) elementTypes
    TypedDataType name arguments -> validateCoreName path name <> concatMap (validateType path scope) arguments
    TypedFunctionType argument result -> validateType path scope argument <> validateType path scope result
    TypedTypeParameterType parameterId
      | Set.member parameterId scope -> []
      | otherwise -> [failure path TypedUnboundTypeParameter (TypedTypeParameterDetail parameterId)]

validateRecipe :: TypedCoreValidationPath -> Set TypedTypeParameterId -> TypedRepresentationRecipe -> [TypedCoreValidationFailure]
validateRecipe path scope recipe =
  widthFailures <> recipeFailures
  where
    widthFailures =
      case invalidRecipeWidth recipe of
        Just width -> [failure path TypedInvalidRepresentationWidth (TypedIndexDetail width)]
        Nothing -> []
    recipeFailures =
      case recipe of
        TypedManagedListRecipe elementRecipe -> validateRecipe path scope elementRecipe
        TypedManagedProductRecipe elementRecipes -> concatMap (validateRecipe path scope) elementRecipes
        TypedManagedVariantRecipe name arguments -> validateCoreName path name <> concatMap (validateType path scope) arguments
        TypedClosureRecipe parameters result -> concatMap (validateRecipe path scope) parameters <> validateRecipe path scope result
        TypedRepresentationParameterRecipe parameterId
          | Set.member parameterId scope -> []
          | otherwise -> [failure path TypedUnboundRepresentationParameter (TypedTypeParameterDetail parameterId)]
        _ -> []

validateTypeRecipe :: TypedCoreValidationPath -> Set TypedTypeParameterId -> TypedType -> TypedRepresentationRecipe -> [TypedCoreValidationFailure]
validateTypeRecipe path scope typeValue recipe
  | not (validRecipeWidth recipe) = []
  | hasUnboundTypeParameter scope typeValue = []
  | hasUnboundRepresentationParameter scope recipe = []
  | otherwise =
      case expectedRecipe typeValue of
        Just expected
          | expected /= recipe ->
              [ failure
                  path
                  (if isFunctionType typeValue then TypedCallableRecipeMismatch else TypedTypeRepresentationMismatch)
                  (TypedRecipeDetail expected recipe)
              ]
        _ -> []

expectedRecipe :: TypedType -> Maybe TypedRepresentationRecipe
expectedRecipe typeValue =
  case typeValue of
    TypedIntType -> Just (TypedSignedIntegerRecipe 64)
    TypedFloatType -> Just (TypedFloatRecipe 64)
    TypedNumericType numericType -> Just (numericRecipe numericType)
    TypedBoolType -> Just TypedBoolRecipe
    TypedCharType -> Just TypedCharRecipe
    TypedTextType -> Just TypedManagedTextRecipe
    TypedListType elementType -> TypedManagedListRecipe <$> expectedRecipe elementType
    TypedTupleType [] -> Just TypedUnitRecipe
    TypedTupleType elementTypes -> TypedManagedProductRecipe <$> traverse expectedRecipe elementTypes
    TypedDataType name arguments -> Just (TypedManagedVariantRecipe name arguments)
    TypedFunctionType {} -> do
      let (parameters, result) = flattenFunctionType typeValue
      parameterRecipes <- traverse expectedRecipe parameters
      resultRecipe <- expectedRecipe result
      pure (TypedClosureRecipe parameterRecipes resultRecipe)
    TypedTypeParameterType parameterId -> Just (TypedRepresentationParameterRecipe parameterId)

numericRecipe :: TypedNumericType -> TypedRepresentationRecipe
numericRecipe numericType =
  case numericType of
    TypedInt8Type -> TypedSignedIntegerRecipe 8
    TypedInt16Type -> TypedSignedIntegerRecipe 16
    TypedInt32Type -> TypedSignedIntegerRecipe 32
    TypedInt64Type -> TypedSignedIntegerRecipe 64
    TypedUInt8Type -> TypedUnsignedIntegerRecipe 8
    TypedUInt16Type -> TypedUnsignedIntegerRecipe 16
    TypedUInt32Type -> TypedUnsignedIntegerRecipe 32
    TypedUInt64Type -> TypedUnsignedIntegerRecipe 64
    TypedFloat16Type -> TypedFloatRecipe 16
    TypedFloat32Type -> TypedFloatRecipe 32
    TypedFloat64Type -> TypedFloatRecipe 64

flattenFunctionType :: TypedType -> ([TypedType], TypedType)
flattenFunctionType typeValue =
  case typeValue of
    TypedFunctionType argument result ->
      let (arguments, finalResult) = flattenFunctionType result
       in (argument : arguments, finalResult)
    _ -> ([], typeValue)

isFunctionType :: TypedType -> Bool
isFunctionType TypedFunctionType {} = True
isFunctionType _ = False

invalidRecipeWidth :: TypedRepresentationRecipe -> Maybe Int
invalidRecipeWidth recipe =
  case recipe of
    TypedSignedIntegerRecipe width
      | width `notElem` [8, 16, 32, 64] -> Just width
    TypedUnsignedIntegerRecipe width
      | width `notElem` [8, 16, 32, 64] -> Just width
    TypedFloatRecipe width
      | width `notElem` [16, 32, 64] -> Just width
    TypedManagedListRecipe elementRecipe -> invalidRecipeWidth elementRecipe
    TypedManagedProductRecipe elementRecipes -> firstJust (map invalidRecipeWidth elementRecipes)
    TypedClosureRecipe parameters result -> firstJust (map invalidRecipeWidth (parameters <> [result]))
    _ -> Nothing

validRecipeWidth :: TypedRepresentationRecipe -> Bool
validRecipeWidth = maybe True (const False) . invalidRecipeWidth

hasUnboundTypeParameter :: Set TypedTypeParameterId -> TypedType -> Bool
hasUnboundTypeParameter scope typeValue =
  case typeValue of
    TypedListType elementType -> hasUnboundTypeParameter scope elementType
    TypedTupleType elementTypes -> any (hasUnboundTypeParameter scope) elementTypes
    TypedDataType _ arguments -> any (hasUnboundTypeParameter scope) arguments
    TypedFunctionType argument result -> hasUnboundTypeParameter scope argument || hasUnboundTypeParameter scope result
    TypedTypeParameterType parameterId -> not (Set.member parameterId scope)
    _ -> False

hasUnboundRepresentationParameter :: Set TypedTypeParameterId -> TypedRepresentationRecipe -> Bool
hasUnboundRepresentationParameter scope recipe =
  case recipe of
    TypedManagedListRecipe elementRecipe -> hasUnboundRepresentationParameter scope elementRecipe
    TypedManagedProductRecipe elementRecipes -> any (hasUnboundRepresentationParameter scope) elementRecipes
    TypedClosureRecipe parameters result -> any (hasUnboundRepresentationParameter scope) parameters || hasUnboundRepresentationParameter scope result
    TypedRepresentationParameterRecipe parameterId -> not (Set.member parameterId scope)
    _ -> False

validateCoreName :: TypedCoreValidationPath -> TypedCoreName -> [TypedCoreValidationFailure]
validateCoreName path name =
  case name of
    TypedUnresolvedSourceName _ -> [failure path TypedUnresolvedName (TypedNameDetail name)]
    TypedUnresolvedQualifiedName _ _ -> [failure path TypedUnresolvedName (TypedNameDetail name)]
    TypedResolvedName _ _ identifier
      | Text.null identifier -> [failure path TypedUnresolvedName (TypedNameDetail name)]
    _ -> []

validateLocalDefinitionName :: ModuleContext -> [TypedNameNamespace] -> TypedCoreValidationPath -> TypedCoreName -> [TypedCoreValidationFailure]
validateLocalDefinitionName context allowedNamespaces path name =
  validateCoreName path name
    <> case name of
      TypedResolvedName TypedCurrentModule namespace _
        | namespace `elem` allowedNamespaces -> []
      TypedResolvedName TypedAmbientPrelude namespace _
        | moduleContextPath context == ["Prelude"], namespace `elem` allowedNamespaces -> []
      TypedGeneratedName {}
        | TypedValueNamespace `elem` allowedNamespaces -> []
      TypedUnresolvedSourceName {} -> []
      TypedUnresolvedQualifiedName {} -> []
      _ -> [failure path TypedInvisibleName (TypedNameDetail name)]

nodeContractFailures :: TypedCoreValidationPath -> TypedCoreValidationKind -> TypedNodeInfo -> TypedNodeInfo -> [TypedCoreValidationFailure]
nodeContractFailures path kind expected actual
  | nodeType expected /= nodeType actual = [failure path kind (TypedTypeDetail (nodeType expected) (nodeType actual))]
  | nodeRecipe expected /= nodeRecipe actual = [failure path kind (TypedRecipeDetail (nodeRecipe expected) (nodeRecipe actual))]
  | otherwise = []

validateVisibleNameInNamespaces :: [TypedNameNamespace] -> ModuleContext -> TypedCoreValidationPath -> TypedCoreName -> [TypedCoreValidationFailure]
validateVisibleNameInNamespaces allowedNamespaces context path name =
  validateCoreName path name
    <> ( case name of
           TypedBuiltinName identifier
             | TypedValueNamespace `notElem` allowedNamespaces || not (knownBuiltinName identifier) ->
                 [failure path TypedInvisibleName (TypedNameDetail name)]
           _ ->
             case resolvedNameKey (moduleContextPath context) name of
               Just key
                 | not (nameUsesAllowedNamespace name) || Set.notMember key (moduleContextVisibleNames context) ->
                     [failure path TypedInvisibleName (TypedNameDetail name)]
               _ -> []
       )
  where
    nameUsesAllowedNamespace candidate =
      case candidate of
        TypedResolvedName _ namespace _ -> namespace `elem` allowedNamespaces
        TypedGeneratedName {} -> TypedValueNamespace `elem` allowedNamespaces
        _ -> True

knownBuiltinName :: Text -> Bool
knownBuiltinName identifier = isBuiltinSymbolName identifier || isKernelBuiltinSymbolName identifier

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
  case name of
    TypedGeneratedName (TypedOperatorBinding bindingName) ->
      bindingName == operatorBindingIdentifierText symbol
    _ -> False

validateOperatorValue :: ModuleContext -> TypedCoreValidationPath -> TypedNodeInfo -> TypedOperatorRef -> [TypedCoreValidationFailure]
validateOperatorValue context path info operator =
  case operator of
    TypedBuiltinOperator symbol ->
      validateOperatorRef context path operator
        <> validateBuiltinOperatorValue context path symbol (nodeType info)
    TypedResolvedOperator {} ->
      validateOperatorRef context path operator
        <> case operatorContractType context path info operator of
          (contractFailures, Just expectedType)
            | expectedType /= nodeType info ->
                contractFailures <> [failure path TypedBindingValueMismatch (TypedTypeDetail expectedType (nodeType info))]
          (contractFailures, _) -> contractFailures

validateBinaryOperator :: ModuleContext -> TypedCoreValidationPath -> TypedNodeInfo -> TypedOperatorRef -> TypedExpr -> TypedExpr -> [TypedCoreValidationFailure]
validateBinaryOperator context path info operator left right =
  case operator of
    TypedBuiltinOperator symbol ->
      validateOperatorRef context path operator
        <> validateBuiltinOperatorApplication context path symbol (nodeType (expressionInfo left)) (nodeType (expressionInfo right)) (nodeType info)
    TypedResolvedOperator {} ->
      validateOperatorRef context path operator
        <> case operatorContractType context path info operator of
          (contractFailures, Just (TypedFunctionType expectedLeft (TypedFunctionType expectedRight expectedResult))) ->
            contractFailures
              <> typeMismatchFailure TypedApplicationArgumentMismatch expectedLeft (nodeType (expressionInfo left))
              <> typeMismatchFailure TypedApplicationArgumentMismatch expectedRight (nodeType (expressionInfo right))
              <> typeMismatchFailure TypedApplicationResultMismatch expectedResult (nodeType info)
          (contractFailures, Just actualType) ->
            contractFailures
              <> [ failure
                     path
                     TypedApplicationFunctionMismatch
                     ( TypedTypeDetail
                         (TypedFunctionType (nodeType (expressionInfo left)) (TypedFunctionType (nodeType (expressionInfo right)) (nodeType info)))
                         actualType
                     )
                 ]
          (contractFailures, Nothing) -> contractFailures
  where
    typeMismatchFailure kind expected actual
      | expected == actual = []
      | otherwise = [failure path kind (TypedTypeDetail expected actual)]

validateLeftSectionOperator :: ModuleContext -> TypedCoreValidationPath -> TypedNodeInfo -> TypedExpr -> TypedOperatorRef -> [TypedCoreValidationFailure]
validateLeftSectionOperator context path info left operator =
  case operator of
    TypedBuiltinOperator symbol ->
      validateOperatorRef context path operator
        <> case nodeType info of
          TypedFunctionType rightType resultType ->
            validateBuiltinOperatorApplication context path symbol (nodeType (expressionInfo left)) rightType resultType
          actualType -> [failure path TypedApplicationFunctionMismatch (TypedTypeDetail (TypedFunctionType (nodeType (expressionInfo left)) actualType) actualType)]
    TypedResolvedOperator {} ->
      validateOperatorRef context path operator
        <> case operatorContractType context path info operator of
          (contractFailures, Just (TypedFunctionType expectedLeft remainder@(TypedFunctionType _ _))) ->
            contractFailures
              <> mismatch TypedApplicationArgumentMismatch expectedLeft (nodeType (expressionInfo left))
              <> mismatch TypedApplicationResultMismatch remainder (nodeType info)
          (contractFailures, Just actualType) ->
            contractFailures
              <> [failure path TypedApplicationFunctionMismatch (TypedTypeDetail (TypedFunctionType (nodeType (expressionInfo left)) (nodeType info)) actualType)]
          (contractFailures, Nothing) -> contractFailures
  where
    mismatch kind expected actual
      | expected == actual = []
      | otherwise = [failure path kind (TypedTypeDetail expected actual)]

validateRightSectionOperator :: ModuleContext -> TypedCoreValidationPath -> TypedNodeInfo -> TypedOperatorRef -> TypedExpr -> [TypedCoreValidationFailure]
validateRightSectionOperator context path info operator right =
  case operator of
    TypedBuiltinOperator symbol ->
      validateOperatorRef context path operator
        <> case nodeType info of
          TypedFunctionType leftType resultType ->
            validateBuiltinOperatorApplication context path symbol leftType (nodeType (expressionInfo right)) resultType
          actualType -> [failure path TypedApplicationFunctionMismatch (TypedTypeDetail (TypedFunctionType actualType (nodeType (expressionInfo right))) actualType)]
    TypedResolvedOperator {} ->
      validateOperatorRef context path operator
        <> case operatorContractType context path info operator of
          (contractFailures, Just (TypedFunctionType expectedLeft (TypedFunctionType expectedRight expectedResult))) ->
            let expectedSectionType = TypedFunctionType expectedLeft expectedResult
             in contractFailures
                  <> mismatch TypedApplicationArgumentMismatch expectedRight (nodeType (expressionInfo right))
                  <> mismatch TypedApplicationResultMismatch expectedSectionType (nodeType info)
          (contractFailures, Just actualType) ->
            contractFailures
              <> [failure path TypedApplicationFunctionMismatch (TypedTypeDetail (TypedFunctionType (nodeType info) (nodeType (expressionInfo right))) actualType)]
          (contractFailures, Nothing) -> contractFailures
  where
    mismatch kind expected actual
      | expected == actual = []
      | otherwise = [failure path kind (TypedTypeDetail expected actual)]

builtinOperatorHasTypedRule :: Text -> Bool
builtinOperatorHasTypedRule symbol =
  symbol `elem` ["+", "-", "*", "/", "<", "<=", ">", ">=", "==", "!=", "$"]

validateBuiltinOperatorValue :: ModuleContext -> TypedCoreValidationPath -> Text -> TypedType -> [TypedCoreValidationFailure]
validateBuiltinOperatorValue context path symbol operatorType
  | not (builtinOperatorHasTypedRule symbol) = []
  | otherwise =
      case operatorType of
        TypedFunctionType leftType (TypedFunctionType rightType resultType) ->
          validateBuiltinOperatorApplication context path symbol leftType rightType resultType
        _ -> [failure path TypedApplicationFunctionMismatch (TypedTextDetail symbol)]

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

operatorContractType :: ModuleContext -> TypedCoreValidationPath -> TypedNodeInfo -> TypedOperatorRef -> ([TypedCoreValidationFailure], Maybe TypedType)
operatorContractType _ _ _ (TypedBuiltinOperator _) = ([], Nothing)
operatorContractType context path info (TypedResolvedOperator name _) =
  case lookupSchemeByName context name of
    Nothing -> ([], Nothing)
    Just (TypedScheme owner parameters evidenceParameters _ resultType _) ->
      let ownerPath = binderModulePath owner
          qualifiedType
            | ownerPath == moduleContextPath context = resultType
            | otherwise = qualifyExternalType ownerPath resultType
          matchingOwnerInstantiation =
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
       in if null parameters
            then (requirementFailures, Just qualifiedType)
            else case matchingOwnerInstantiation of
              Nothing -> (requirementFailures, Nothing)
              Just (TypedInstantiation _ arguments _)
                | map typeArgumentParameter arguments == parameters ->
                    (requirementFailures, Just (substituteTypeParameters (Map.fromList [(parameterId, typeValue) | TypedTypeArgument parameterId typeValue <- arguments]) qualifiedType))
                | otherwise -> (requirementFailures, Nothing)
  where
    typeArgumentParameter (TypedTypeArgument parameterId _) = parameterId

validateImplId :: ModuleContext -> TypedCoreValidationPath -> Set TypedTypeParameterId -> TypedImplId -> [TypedCoreValidationFailure]
validateImplId context path scope (TypedImplId _ capability arguments) =
  validateCapabilityName context path capability
    <> targetArityFailures
    <> concatMap (validateType path scope) arguments
  where
    targetArityFailures =
      case capabilityArity context capability of
        Just expectedArity
          | expectedArity /= length arguments ->
              [failure path TypedMethodSelectionMismatch (TypedArityDetail expectedArity (length arguments))]
        _ -> []

capabilityArity :: ModuleContext -> TypedCoreName -> Maybe Int
capabilityArity context capability =
  case resolvedNameKey (moduleContextPath context) capability >>= (`Map.lookup` moduleContextCapabilityContracts context) of
    Just (CapabilityContract parameters _) -> Just (length parameters)
    Nothing -> Nothing

validateMethodId :: ModuleContext -> TypedCoreValidationPath -> Set TypedTypeParameterId -> TypedMethodId -> [TypedCoreValidationFailure]
validateMethodId context path scope (TypedMethodId implId _) = validateImplId context path scope implId

validateCapabilityName :: ModuleContext -> TypedCoreValidationPath -> TypedCoreName -> [TypedCoreValidationFailure]
validateCapabilityName context path name =
  validateVisibleNameInNamespaces [TypedCapabilityNamespace] context path name

validateModuleInterface :: Map [Text] TypedModule -> TypedModule -> [TypedCoreValidationFailure]
validateModuleInterface moduleTable (TypedModule modulePath _ imports exports (TypedModuleInterface values datas classes impls) statements _) =
  concatMap validateValueInterface values
    <> concatMap validateDataInterface datas
    <> concatMap validateClassInterface classes
    <> concatMap validateImplInterface impls
    <> concatMap validateExport exports
  where
    path = TypedInterfacePath modulePath
    activeDeclaredValues =
      Map.fromList
        [ (name, scheme)
        | TypedLetStatement _ name _ scheme _ <- statements
        ]
    declaredDatas = [declaration | TypedDataStatement declaration <- statements]
    declaredClasses = [declaration | TypedClassStatement declaration <- statements]
    declaredImpls = [implId | TypedImplStatement (TypedImplDeclaration _ implId _) <- statements]
    externalCapabilityIdentifiers =
      Set.fromList
        [ identifier
        | visibleModule@(_, _, TypedModule _ _ _ _ (TypedModuleInterface _ _ visibleClasses _) _ _) <-
            preludeModules <> importedModules,
          TypedClassInterface (TypedClassDeclaration _ name _ methods) <- visibleClasses,
          interfaceCapabilityNameIncluded visibleModule name methods,
          identifier <- maybeToList (coreNameIdentifier name)
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
      | Map.lookup name activeDeclaredValues == Just scheme && moduleExportsName TypedValueNamespace name exports =
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
      | otherwise = [failure path TypedModuleInterfaceMismatch (TypedNameDetail name)]
    validateImplInterface (TypedImplInterface implId)
      | implId `elem` declaredImpls = []
      | otherwise = [failure path TypedModuleInterfaceMismatch (TypedImplDetail implId)]
    validateValueInterfaceDependencies scheme =
      [ failure path TypedModuleInterfaceMismatch (TypedNameDetail dependencyName)
      | dependencyName <- nub (schemeLocalDataDependencies scheme),
        not (any (dataInterfaceMatches dependencyName) datas)
      ]
        <> [ failure
               path
               TypedModuleInterfaceMismatch
               ( TypedNameDetail
                   (TypedResolvedName TypedCurrentModule TypedCapabilityNamespace capability)
               )
           | capability <- nub (schemeLocalCapabilityDependencies declaredClasses scheme),
             not (any (classInterfaceMatches capability) classes)
           ]
        <> [ failure
               path
               TypedModuleInterfaceMismatch
               ( TypedNameDetail
                   (TypedResolvedName TypedCurrentModule TypedCapabilityNamespace capability)
               )
           | capability <- nub (schemeCapabilityDependencies scheme),
             not (any (classDeclarationMatches capability) declaredClasses),
             Set.member capability externalCapabilityIdentifiers
           ]
    constructorDependencies (TypedConstructorDeclaration _ _ fields _) =
      concatMap localDataDependencies fields
    validateExport (TypedModuleExport namespace exportedName) =
      case namespace of
        TypedValueNamespace
          | any (interfaceNameMatches exportedName) values || any (classInterfaceMethodMatches exportedName) classes -> []
        TypedTypeNamespace
          | any (dataInterfaceNameMatches exportedName) datas || any (classInterfaceNameMatches exportedName) classes -> []
        TypedConstructorNamespace
          | any (dataInterfaceConstructorMatches exportedName) datas -> []
        TypedCapabilityNamespace
          | any (classInterfaceNameMatches exportedName) classes -> []
        _ -> [failure path TypedModuleInterfaceMismatch (TypedNameDetail (TypedResolvedName TypedCurrentModule namespace exportedName))]

schemeLocalDataDependencies :: TypedScheme -> [TypedCoreName]
schemeLocalDataDependencies (TypedScheme _ _ evidence primitive resultType _) =
  concatMap localDataDependencies (resultType : evidenceTypes <> primitiveTypes)
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

localDataDependencies :: TypedType -> [TypedCoreName]
localDataDependencies typeValue =
  case typeValue of
    TypedListType elementType -> localDataDependencies elementType
    TypedTupleType elementTypes -> concatMap localDataDependencies elementTypes
    TypedDataType name arguments ->
      [name | TypedResolvedName TypedCurrentModule TypedTypeNamespace _ <- [name]]
        <> concatMap localDataDependencies arguments
    TypedFunctionType argument result ->
      localDataDependencies argument <> localDataDependencies result
    _ -> []

schemeLocalCapabilityDependencies :: [TypedClassDeclaration] -> TypedScheme -> [Text]
schemeLocalCapabilityDependencies declaredClasses (TypedScheme _ _ evidence _ _ _) =
  [ capability
  | TypedEvidenceParameter _ (TypedCapabilityConstraint capability _ _) <- evidence,
    any (classDeclarationMatches capability) declaredClasses
  ]

schemeCapabilityDependencies :: TypedScheme -> [Text]
schemeCapabilityDependencies (TypedScheme _ _ evidence _ _ _) =
  [ capability
  | TypedEvidenceParameter _ (TypedCapabilityConstraint capability _ _) <- evidence
  ]

dataInterfaceMatches :: TypedCoreName -> TypedDataInterface -> Bool
dataInterfaceMatches expected (TypedDataInterface (TypedDataDeclaration _ actual _ _)) =
  expected == actual

classInterfaceMatches :: Text -> TypedClassInterface -> Bool
classInterfaceMatches expected (TypedClassInterface (TypedClassDeclaration _ name _ _)) =
  coreNameIdentifier name == Just expected

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

coreNameIdentifier :: TypedCoreName -> Maybe Text
coreNameIdentifier name =
  case name of
    TypedResolvedName _ _ identifier -> Just identifier
    TypedBuiltinName identifier -> Just identifier
    _ -> Nothing

expressionInfo :: TypedExpr -> TypedNodeInfo
expressionInfo expression =
  case expression of
    TypedLiteralExpr info _ -> info
    TypedVariableExpr info _ -> info
    TypedLambdaExpr info _ _ _ -> info
    TypedOperatorValueExpr info _ -> info
    TypedListExpr info _ -> info
    TypedTupleExpr info _ -> info
    TypedApplyExpr info _ _ -> info
    TypedTypeApplicationExpr info _ _ _ -> info
    TypedIfExpr info _ _ _ -> info
    TypedPatternCaseExpr info _ _ -> info
    TypedBinaryExpr info _ _ _ -> info
    TypedLeftSectionExpr info _ _ -> info
    TypedRightSectionExpr info _ _ -> info
    TypedBlockExpr info _ -> info

patternInfo :: TypedPattern -> TypedNodeInfo
patternInfo patternValue =
  case patternValue of
    TypedWildcardPattern info -> info
    TypedVariablePattern info _ _ -> info
    TypedLiteralPattern info _ -> info
    TypedConstructorPattern info _ _ -> info
    TypedListPattern info _ -> info
    TypedConsListPattern info _ _ -> info
    TypedTuplePattern info _ -> info
    TypedAsPattern info _ _ _ -> info
    TypedOrPattern info _ -> info

nodeType :: TypedNodeInfo -> TypedType
nodeType (TypedNodeInfo typeValue _ _ _) = typeValue

nodeRecipe :: TypedNodeInfo -> TypedRepresentationRecipe
nodeRecipe (TypedNodeInfo _ recipe _ _) = recipe

typedModulePath :: TypedModule -> [Text]
typedModulePath (TypedModule modulePath _ _ _ _ _ _) = modulePath

implModulePath :: TypedImplId -> [Text]
implModulePath (TypedImplId modulePath _ _) = modulePath

implTargetTypes :: TypedImplId -> [TypedType]
implTargetTypes (TypedImplId _ _ targetTypes) = targetTypes

renderModulePath :: [Text] -> Text
renderModulePath = Text.intercalate "::"

failure :: TypedCoreValidationPath -> TypedCoreValidationKind -> TypedCoreValidationDetail -> TypedCoreValidationFailure
failure = TypedCoreValidationFailure

maybeToList :: Maybe value -> [value]
maybeToList maybeValue =
  case maybeValue of
    Nothing -> []
    Just value -> [value]

firstJust :: [Maybe value] -> Maybe value
firstJust values =
  case values of
    [] -> Nothing
    Nothing : rest -> firstJust rest
    Just value : _ -> Just value
