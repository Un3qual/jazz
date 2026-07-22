{-# LANGUAGE OverloadedStrings #-}

-- | Complete structural validation for the semantic typed-core boundary.
-- Validation is deliberately independent of inference, evaluation, and
-- lowering: it accepts an already-constructed contract value and reports all
-- invariant failures in stable structural order.
module JazzNext.Compiler.TypedCore.Validate
  ( validateTypedProgram,
  )
where

import Data.List (find)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import JazzNext.Compiler.TypedCore

data ModuleContext = ModuleContext
  { moduleContextPath :: [Text],
    moduleContextVisibleModules :: Set [Text],
    moduleContextSchemes :: Map TypedBinderId TypedScheme,
    moduleContextStatementIndices :: Map [Int] Int,
    moduleContextVisibleNames :: Set ResolvedNameKey,
    moduleContextVisibleImpls :: Set TypedImplId,
    moduleContextDataArities :: Map ResolvedNameKey Int,
    moduleContextConstructorContracts :: Map ResolvedNameKey ConstructorContract,
    moduleContextTypeScope :: Set TypedTypeParameterId
  }

data ResolvedNameKey = ResolvedNameKey [Text] TypedNameNamespace Text
  deriving (Eq, Ord, Show)

data BinderOccurrence = BinderOccurrence TypedCoreValidationPath TypedBinderId

data ConstructorContract = ConstructorContract ResolvedNameKey [TypedTypeParameterId] [TypedType]

validateTypedProgram :: TypedProgram -> [TypedCoreValidationFailure]
validateTypedProgram (TypedProgram prelude modules entryModule) =
  duplicateModuleFailures allModules
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

validateModule :: Map [Text] TypedModule -> Maybe TypedModule -> Bool -> TypedModule -> [TypedCoreValidationFailure]
validateModule moduleTable prelude isPrelude moduleValue@(TypedModule modulePath sourcePath imports _ _ statements moduleInfo) =
  validateSourcePath modulePath sourcePath
    <> validateResolvedImports moduleTable modulePath imports
    <> validateModuleInterface moduleValue
    <> duplicateBinderFailures context (zip (map pure [0 ..]) statements)
    <> concatMap (uncurry (validateStatement context)) (zip (map pure [0 ..]) statements)
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
    localSchemeEntries = concatMap statementSchemes statements
    importedSchemeEntries = concatMap interfaceSchemeEntries visibleExternalModules
    schemes = Map.fromList (importedSchemeEntries <> localSchemeEntries)
    visibleNames =
      Set.fromList
        ( concatMap (statementDefinedNameKeys modulePath) statements
            <> concatMap interfaceNameKeys visibleExternalModules
        )
    visibleImpls =
      Set.fromList
        ( [implId | TypedImplStatement (TypedImplDeclaration _ implId _) <- statements]
            <> concatMap interfaceImplIds visibleExternalModules
        )
    dataArities =
      Map.fromList
        ( concatMap (statementDataEntries modulePath) statements
            <> concatMap interfaceDataEntries visibleExternalModules
        )
    constructorContracts =
      Map.fromList
        ( concatMap (statementConstructorEntries modulePath) statements
            <> concatMap interfaceConstructorEntries visibleExternalModules
        )
    statementIndices =
      Map.fromList
        ( zip
            (topLevelStatementLocations statements <> concatMap (uncurry nestedStatementLocations) (zip (map pure [0 ..]) statements))
            [0 ..]
        )
    context =
      ModuleContext
        { moduleContextPath = modulePath,
          moduleContextVisibleModules = visibleModules,
          moduleContextSchemes = schemes,
          moduleContextStatementIndices = statementIndices,
          moduleContextVisibleNames = visibleNames,
          moduleContextVisibleImpls = visibleImpls,
          moduleContextDataArities = dataArities,
          moduleContextConstructorContracts = constructorContracts,
          moduleContextTypeScope = Set.empty
        }

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
    TypedValueNamespace -> any (interfaceNameMatches expected) values
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
    _ -> []

validateModuleInfo :: ModuleContext -> TypedCoreValidationPath -> [TypedStatement] -> TypedNodeInfo -> [TypedCoreValidationFailure]
validateModuleInfo context path statements moduleInfo =
  case reverse statements of
    TypedExpressionStatement _ terminal : _
      | expressionInfo terminal == moduleInfo -> []
    _ -> validateNodeInfo context path Set.empty moduleInfo

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
        TypedOrPattern _ [] -> []
        TypedOrPattern _ (firstAlternative : _) -> patternBinderOccurrences modulePath statementIndex (patternPath <> [0]) firstAlternative
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
    TypedSignatureStatement binderId _ _ scheme -> [(binderId, scheme)]
    TypedClassStatement (TypedClassDeclaration _ _ _ methods) ->
      [(binderId, scheme) | TypedMethodSignature _ _ scheme@(TypedScheme binderId _ _ _ _ _) <- methods]
    _ -> []

interfaceSchemeEntries :: ([Text], Maybe [Text], TypedModule) -> [(TypedBinderId, TypedScheme)]
interfaceSchemeEntries (_, selectedNames, TypedModule _ _ _ exports (TypedModuleInterface values _ classes _) _ _) =
  [ (binderId, scheme)
  | TypedValueInterface name scheme@(TypedScheme binderId _ _ _ _ _) <- values,
    importAllows selectedNames name,
    moduleExportsName TypedValueNamespace name exports
  ]
    <> [ (binderId, scheme)
       | TypedClassInterface (TypedClassDeclaration _ _ _ methods) <- classes,
         TypedMethodSignature name _ scheme@(TypedScheme binderId _ _ _ _ _) <- methods,
         importAllows selectedNames name,
         moduleExportsName TypedValueNamespace name exports
       ]

interfaceImplIds :: ([Text], Maybe [Text], TypedModule) -> [TypedImplId]
interfaceImplIds (_, _, TypedModule _ _ _ _ (TypedModuleInterface _ _ _ impls) _ _) =
  [implId | TypedImplInterface implId <- impls]

interfaceDataEntries :: ([Text], Maybe [Text], TypedModule) -> [(ResolvedNameKey, Int)]
interfaceDataEntries (modulePath, selectedNames, TypedModule _ _ _ exports (TypedModuleInterface _ datas _ _) _ _) =
  [ (key, length parameters)
  | TypedDataInterface (TypedDataDeclaration _ name parameters constructors) <- datas,
    (importAllows selectedNames name && moduleExportsName TypedTypeNamespace name exports)
      || any (\constructor -> importAllows selectedNames (constructorName constructor) && moduleExportsName TypedConstructorNamespace (constructorName constructor) exports) constructors,
    key <- maybeToList (definitionNameKey modulePath name)
  ]
  where
    constructorName (TypedConstructorDeclaration _ name _ _) = name

interfaceNameKeys :: ([Text], Maybe [Text], TypedModule) -> [ResolvedNameKey]
interfaceNameKeys (modulePath, selectedNames, TypedModule _ _ _ exports (TypedModuleInterface values datas classes _) _ _) =
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
        importAllows selectedNames name,
        (moduleExportsName TypedCapabilityNamespace name exports || moduleExportsName TypedTypeNamespace name exports),
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
    TypedSignatureStatement _ name _ _ -> [name]
    TypedDataStatement (TypedDataDeclaration _ name _ constructors) ->
      name : [constructorName | TypedConstructorDeclaration _ constructorName _ _ <- constructors]
    TypedClassStatement (TypedClassDeclaration _ name _ methods) ->
      name : [methodName | TypedMethodSignature methodName _ _ <- methods]
    TypedImplStatement (TypedImplDeclaration _ _ methods) ->
      [methodName | TypedMethodDefinition _ _ methodName _ _ <- methods]
    TypedExpressionStatement {} -> []

statementDataEntries :: [Text] -> TypedStatement -> [(ResolvedNameKey, Int)]
statementDataEntries modulePath statement =
  case statement of
    TypedDataStatement (TypedDataDeclaration _ name parameters _) ->
      [(key, length parameters) | key <- maybeToList (definitionNameKey modulePath name)]
    _ -> []

statementConstructorEntries :: [Text] -> TypedStatement -> [(ResolvedNameKey, ConstructorContract)]
statementConstructorEntries modulePath statement =
  case statement of
    TypedDataStatement (TypedDataDeclaration _ dataName parameters constructors) ->
      [ (constructorKey, ConstructorContract dataKey parameters fields)
      | dataKey <- maybeToList (definitionNameKey modulePath dataName),
        TypedConstructorDeclaration _ constructorName fields _ <- constructors,
        constructorKey <- maybeToList (definitionNameKey modulePath constructorName)
      ]
    _ -> []

interfaceConstructorEntries :: ([Text], Maybe [Text], TypedModule) -> [(ResolvedNameKey, ConstructorContract)]
interfaceConstructorEntries (modulePath, selectedNames, TypedModule _ _ _ exports (TypedModuleInterface _ datas _ _) _ _) =
  [ (constructorKey, ConstructorContract dataKey parameters (map (qualifyExternalType modulePath) fields))
  | TypedDataInterface (TypedDataDeclaration _ dataName parameters constructors) <- datas,
    dataKey <- maybeToList (definitionNameKey modulePath dataName),
    TypedConstructorDeclaration _ constructorName fields _ <- constructors,
    importAllows selectedNames constructorName,
    moduleExportsName TypedConstructorNamespace constructorName exports,
    constructorKey <- maybeToList (definitionNameKey modulePath constructorName)
  ]

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
    _ -> Nothing

resolvedNameKey :: [Text] -> TypedCoreName -> Maybe ResolvedNameKey
resolvedNameKey currentModulePath name =
  case name of
    TypedResolvedName origin namespace identifier ->
      Just (ResolvedNameKey (originModulePath origin) namespace identifier)
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

withBlockDeclarations :: [TypedStatement] -> ModuleContext -> ModuleContext
withBlockDeclarations statements context =
  context
    { moduleContextSchemes = Map.union localSchemes (moduleContextSchemes context),
      moduleContextVisibleNames = Set.union localNames (moduleContextVisibleNames context),
      moduleContextVisibleImpls = Set.union localImpls (moduleContextVisibleImpls context),
      moduleContextDataArities = Map.union localDataArities (moduleContextDataArities context),
      moduleContextConstructorContracts = Map.union localConstructors (moduleContextConstructorContracts context)
    }
  where
    modulePath = moduleContextPath context
    localSchemes = Map.fromList (concatMap statementSchemes statements)
    localNames = Set.fromList (concatMap (statementDefinedNameKeys modulePath) statements)
    localImpls = Set.fromList [implId | TypedImplStatement (TypedImplDeclaration _ implId _) <- statements]
    localDataArities = Map.fromList (concatMap (statementDataEntries modulePath) statements)
    localConstructors = Map.fromList (concatMap (statementConstructorEntries modulePath) statements)

withTypeScope :: [TypedTypeParameterId] -> ModuleContext -> ModuleContext
withTypeScope typeParameters context =
  context {moduleContextTypeScope = Set.fromList typeParameters}

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
      validateCoreName statementPath name
        <> validateBinderDefinition context statementPath binderId name
        <> validateScheme context statementPath binderId scheme
        <> validateBindingValue statementPath scheme (expressionInfo expression)
        <> validateExpression (withTypeScope (schemeTypeParameters scheme) context) statementLocation [0] expression
    TypedSignatureStatement binderId name _ scheme ->
      validateCoreName statementPath name
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
validateScheme context path owner = validateSchemeWithOuterScope context path owner Set.empty

validateSchemeWithOuterScope :: ModuleContext -> TypedCoreValidationPath -> TypedBinderId -> Set TypedTypeParameterId -> TypedScheme -> [TypedCoreValidationFailure]
validateSchemeWithOuterScope context path owner outerScope (TypedScheme schemeOwner typeParameters evidenceParameters primitiveConstraints resultType resultRecipe) =
  ownerFailures
    <> validateOrderedTypeParameters path typeParameters
    <> validateOrderedEvidenceParameters path evidenceParameters
    <> concatMap (validateEvidenceParameter path parameterScope) evidenceParameters
    <> concatMap (validatePrimitiveConstraint path parameterScope) primitiveConstraints
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

schemeTypeParameters :: TypedScheme -> [TypedTypeParameterId]
schemeTypeParameters (TypedScheme _ typeParameters _ _ _ _) = typeParameters

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

validateEvidenceParameter :: TypedCoreValidationPath -> Set TypedTypeParameterId -> TypedEvidenceParameter -> [TypedCoreValidationFailure]
validateEvidenceParameter path scope (TypedEvidenceParameter _ constraint) =
  validateCapabilityConstraint path scope constraint

validatePrimitiveConstraint :: TypedCoreValidationPath -> Set TypedTypeParameterId -> TypedPrimitiveConstraint -> [TypedCoreValidationFailure]
validatePrimitiveConstraint path scope constraint =
  case constraint of
    TypedNumericPrimitiveConstraint _ typeValue -> validateType path scope typeValue
    TypedStrictEqualityPrimitiveConstraint typeValue -> validateType path scope typeValue

validateCapabilityConstraint :: TypedCoreValidationPath -> Set TypedTypeParameterId -> TypedCapabilityConstraint -> [TypedCoreValidationFailure]
validateCapabilityConstraint path scope (TypedCapabilityConstraint _ _ targetType) = validateType path scope targetType

validateDataDeclaration :: ModuleContext -> TypedCoreValidationPath -> TypedDataDeclaration -> [TypedCoreValidationFailure]
validateDataDeclaration context path (TypedDataDeclaration _ name parameters constructors) =
  validateCoreName path name
    <> validateOrderedTypeParameters path parameters
    <> concatMap validateConstructor constructors
  where
    scope = Set.fromList parameters
    validateConstructor (TypedConstructorDeclaration binderId constructorName fields recipes) =
      validateCoreName path constructorName
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
  validateCoreName path name
    <> validateOrderedTypeParameters path parameters
    <> concatMap validateMethod methods
  where
    validateMethod (TypedMethodSignature methodName _ scheme@(TypedScheme binderId _ _ _ _ _)) =
      validateCoreName path methodName
        <> validateBinderDefinition context path binderId methodName
        <> validateSchemeWithOuterScope context path binderId (Set.fromList parameters) scheme

validateImplDeclaration :: ModuleContext -> [Int] -> TypedCoreValidationPath -> TypedImplDeclaration -> [TypedCoreValidationFailure]
validateImplDeclaration context statementLocation path (TypedImplDeclaration _ implId methods) =
  validateImplId context path Set.empty implId
    <> concatMap (validateDataTypeApplications context path) (implTargetTypes implId)
    <> concatMap (uncurry validateMethod) (zip [0 ..] methods)
  where
    validateMethod methodIndex (TypedMethodDefinition methodId@(TypedMethodId methodImplId methodKey) binderId name _ expression) =
      validateMethodId context path Set.empty methodId
        <> (if methodImplId == implId then [] else [failure path TypedMethodSelectionMismatch (TypedImplDetail methodImplId)])
        <> validateCoreName path name
        <> validateBinderDefinition context path binderId name
        <> (if coreNameIdentifier name == Just methodKey then [] else [failure path TypedMethodSelectionMismatch (TypedTextDetail methodKey)])
        <> validateExpression context statementLocation [methodIndex] expression

validateExpression :: ModuleContext -> [Int] -> [Int] -> TypedExpr -> [TypedCoreValidationFailure]
validateExpression context statementLocation expressionPath expression =
  validateNodeInfo context path (moduleContextTypeScope context) (expressionInfo expression)
    <> expressionOwnedFailures
    <> concatMap (uncurry validateChild) (zip [0 ..] (expressionChildrenWithContexts context expression))
  where
    statementIndex = statementIndexFor context statementLocation
    path = TypedExpressionPath (moduleContextPath context) statementIndex expressionPath
    validateChild childIndex (childContext, child) = validateExpression childContext statementLocation (expressionPath <> [childIndex]) child
    expressionOwnedFailures =
      case expression of
        TypedLiteralExpr info literal -> validateLiteral path info literal
        TypedVariableExpr info name -> validateVariableExpression context path info name
        TypedLambdaExpr info binderId name body -> validateCoreName path name <> validateBinderDefinition context path binderId name <> validateLambda path info body
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
                <> concatMap (uncurry (validateStatement blockContext)) locatedStatements

validateBlockResult :: TypedCoreValidationPath -> TypedNodeInfo -> [TypedStatement] -> [TypedCoreValidationFailure]
validateBlockResult path blockInfo statements =
  case reverse statements of
    TypedExpressionStatement _ terminal : _
      | nodeInfoHasValidIntrinsicContract blockInfo && nodeInfoHasValidIntrinsicContract (expressionInfo terminal) ->
          nodeContractFailures path TypedBlockResultMismatch blockInfo (expressionInfo terminal)
    _ -> []

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
    (TypedIntegerLiteral _, TypedIntType) -> True
    (TypedIntegerLiteral _, TypedNumericType numericType) -> not (isFloatingNumericType numericType)
    (TypedFractionalLiteral _ _ _, TypedFloatType) -> True
    (TypedFractionalLiteral _ _ _, TypedNumericType numericType) -> isFloatingNumericType numericType
    (TypedBooleanLiteral _, TypedBoolType) -> True
    (TypedCharacterLiteral _, TypedCharType) -> True
    (TypedTextLiteral _, TypedTextType) -> True
    _ -> False

literalType :: TypedLiteral -> TypedType
literalType literal =
  case literal of
    TypedIntegerLiteral _ -> TypedIntType
    TypedFractionalLiteral _ _ _ -> TypedFloatType
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
    TypedVariableExpr _ name ->
      case lookupSchemeByName context name of
        Just (TypedScheme owner parameters _ _ _ _)
          | null parameters || not (any (matchingExplicitInstantiation owner) instantiations) ->
              [failure path TypedInstantiationMismatch (TypedBinderDetail owner)]
          | otherwise -> []
        Nothing -> [failure path TypedInstantiationMismatch TypedNoValidationDetail]
    _
      | any hasMatchingExplicitUse instantiations -> []
      | otherwise -> [failure path TypedInstantiationMismatch TypedNoValidationDetail]
  where
    instantiations = nodeInfoInstantiations info
    matchingExplicitInstantiation owner (TypedInstantiation candidateOwner arguments maybeSpan) =
      owner == candidateOwner
        && maybeSpan == Just explicitSpan
        && any typeArgumentMatches arguments
    hasMatchingExplicitUse (TypedInstantiation owner arguments maybeSpan) =
      maybeSpan == Just explicitSpan
        && any typeArgumentMatches arguments
        && maybe False (not . null . schemeTypeParameters) (Map.lookup owner (moduleContextSchemes context))
    typeArgumentMatches (TypedTypeArgument _ candidateType) = candidateType == typeArgument

lookupSchemeByName :: ModuleContext -> TypedCoreName -> Maybe TypedScheme
lookupSchemeByName context name = do
  expectedKey <- resolvedNameKey (moduleContextPath context) name
  snd
    <$> find
      (\(owner, _) -> binderDefinitionKey owner == Just expectedKey)
      (Map.toList (moduleContextSchemes context))

binderDefinitionKey :: TypedBinderId -> Maybe ResolvedNameKey
binderDefinitionKey (TypedBinderId (modulePath, _, name)) = definitionNameKey modulePath name

nodeInfoInstantiations :: TypedNodeInfo -> [TypedInstantiation]
nodeInfoInstantiations (TypedNodeInfo _ _ instantiations _) = instantiations

validateVariableExpression :: ModuleContext -> TypedCoreValidationPath -> TypedNodeInfo -> TypedCoreName -> [TypedCoreValidationFailure]
validateVariableExpression context path info name =
  validateVisibleNameInNamespaces [TypedValueNamespace, TypedConstructorNamespace] context path name
    <> case name of
      TypedResolvedName _ TypedValueNamespace _ ->
        case lookupSchemeByName context name of
          Just scheme@(TypedScheme _ parameters _ _ _ _)
            | null parameters -> validateVariableSchemeContract context path info scheme
          _ -> []
      _ -> []

validateVariableSchemeContract :: ModuleContext -> TypedCoreValidationPath -> TypedNodeInfo -> TypedScheme -> [TypedCoreValidationFailure]
validateVariableSchemeContract context path info (TypedScheme owner _ _ _ resultType resultRecipe)
  | nodeType info /= expectedType =
      [failure path TypedBindingValueMismatch (TypedTypeDetail expectedType (nodeType info))]
  | nodeRecipe info /= expectedRecipeValue =
      [failure path TypedBindingValueMismatch (TypedRecipeDetail expectedRecipeValue (nodeRecipe info))]
  | otherwise = []
  where
    ownerModulePath = binderModulePath owner
    (expectedType, expectedRecipeValue)
      | ownerModulePath == moduleContextPath context = (resultType, resultRecipe)
      | otherwise = (qualifyExternalType ownerModulePath resultType, qualifyExternalRecipe ownerModulePath resultRecipe)

binderModulePath :: TypedBinderId -> [Text]
binderModulePath (TypedBinderId (modulePath, _, _)) = modulePath

expressionChildrenWithContexts :: ModuleContext -> TypedExpr -> [(ModuleContext, TypedExpr)]
expressionChildrenWithContexts context expression =
  case expression of
    TypedLambdaExpr _ _ name body -> [(withVisibleNames [name] context, body)]
    TypedListExpr _ expressions -> [(context, child) | child <- expressions]
    TypedTupleExpr _ expressions -> [(context, child) | child <- expressions]
    TypedApplyExpr _ function argument -> [(context, function), (context, argument)]
    TypedTypeApplicationExpr _ function _ _ -> [(context, function)]
    TypedIfExpr _ condition thenExpression elseExpression ->
      [(context, condition), (context, thenExpression), (context, elseExpression)]
    TypedPatternCaseExpr _ scrutinee arms ->
      (context, scrutinee)
        : concat
          [ let armContext = withVisibleNames (patternBoundNames patternValue) context
             in [(armContext, child) | child <- maybeToList maybeGuard <> [result]]
          | TypedCaseArm patternValue maybeGuard result <- arms
          ]
    TypedBinaryExpr _ _ left right -> [(context, left), (context, right)]
    TypedLeftSectionExpr _ left _ -> [(context, left)]
    TypedRightSectionExpr _ _ right -> [(context, right)]
    _ -> []

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
validateCase context statementLocation expressionPath _ (TypedNodeInfo resultType _ _ _) scrutinee arms =
  concatMap (uncurry validateArm) (zip [0 ..] arms)
  where
    scrutineeType = nodeType (expressionInfo scrutinee)
    validateArm armIndex (TypedCaseArm patternValue maybeGuard resultExpression) =
      validatePattern context statementLocation (expressionPath <> [armIndex]) scrutineeType patternValue
        <> guardFailures armIndex maybeGuard
        <> resultFailures armIndex resultExpression
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
  validateNodeInfo context path (moduleContextTypeScope context) (patternInfo patternValue)
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
        TypedVariablePattern _ binderId name -> validateCoreName path name <> validateBinderDefinition context path binderId name
        TypedLiteralPattern info literal -> validateLiteral path info literal
        TypedConstructorPattern info name patterns ->
          validateVisibleNameInNamespaces [TypedConstructorNamespace] context path name
            <> validateConstructorPatternShape context path info name patterns
        TypedListPattern info _ -> validateListPatternShape path info
        TypedConsListPattern info _ _ -> validateListPatternShape path info
        TypedAsPattern _ binderId name _ -> validateCoreName path name <> validateBinderDefinition context path binderId name
        TypedOrPattern _ alternatives -> validateOrPattern path alternatives
        TypedTuplePattern info patterns -> validateTuplePatternShape path info patterns
        _ -> []
    validateChild (childIndex, childType, childPattern) =
      validatePattern context statementLocation (patternPath <> [childIndex]) childType childPattern

validateTuplePatternShape :: TypedCoreValidationPath -> TypedNodeInfo -> [TypedPattern] -> [TypedCoreValidationFailure]
validateTuplePatternShape path info patterns =
  case nodeType info of
    TypedTupleType types
      | length types == length patterns -> []
      | otherwise -> [failure path TypedPatternShapeMismatch (TypedArityDetail (length types) (length patterns))]
    _ -> []

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
    Nothing -> []

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
  ConstructorContract dataKey parameters fieldTypes <- Map.lookup constructorKey (moduleContextConstructorContracts context)
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

validateOrPattern :: TypedCoreValidationPath -> [TypedPattern] -> [TypedCoreValidationFailure]
validateOrPattern _ [] = []
validateOrPattern path (firstAlternative : rest) = concatMap compareAlternative rest
  where
    expected = patternBinderContract firstAlternative
    compareAlternative alternative
      | patternBinderContract alternative == expected = []
      | otherwise =
          case firstMismatchedBinder expected (patternBinderContract alternative) of
            Just binderId -> [failure path TypedOrPatternBinderMismatch (TypedBinderDetail binderId)]
            Nothing -> [failure path TypedOrPatternBinderMismatch TypedNoValidationDetail]

patternBinderContract :: TypedPattern -> [(TypedBinderId, TypedType, TypedRepresentationRecipe)]
patternBinderContract patternValue =
  case patternValue of
    TypedVariablePattern info binderId _ -> [(binderId, nodeType info, nodeRecipe info)]
    TypedConstructorPattern _ _ patterns -> concatMap patternBinderContract patterns
    TypedListPattern _ patterns -> concatMap patternBinderContract patterns
    TypedConsListPattern _ headPattern tailPattern -> patternBinderContract headPattern <> patternBinderContract tailPattern
    TypedTuplePattern _ patterns -> concatMap patternBinderContract patterns
    TypedAsPattern info binderId _ nested -> (binderId, nodeType info, nodeRecipe info) : patternBinderContract nested
    TypedOrPattern _ [] -> []
    TypedOrPattern _ (alternative : _) -> patternBinderContract alternative
    _ -> []

patternBoundNames :: TypedPattern -> [TypedCoreName]
patternBoundNames patternValue =
  case patternValue of
    TypedVariablePattern _ _ name -> [name]
    TypedConstructorPattern _ _ patterns -> concatMap patternBoundNames patterns
    TypedListPattern _ patterns -> concatMap patternBoundNames patterns
    TypedConsListPattern _ headPattern tailPattern -> patternBoundNames headPattern <> patternBoundNames tailPattern
    TypedTuplePattern _ patterns -> concatMap patternBoundNames patterns
    TypedAsPattern _ _ name nested -> name : patternBoundNames nested
    TypedOrPattern _ [] -> []
    TypedOrPattern _ (alternative : _) -> patternBoundNames alternative
    _ -> []

firstMismatchedBinder :: [(TypedBinderId, TypedType, TypedRepresentationRecipe)] -> [(TypedBinderId, TypedType, TypedRepresentationRecipe)] -> Maybe TypedBinderId
firstMismatchedBinder expected actual =
  case dropWhile (uncurry (==)) (zip expected actual) of
    (_, (binderId, _, _)) : _ -> Just binderId
    [] ->
      case drop (length expected) actual of
        (binderId, _, _) : _ -> Just binderId
        [] -> Nothing

validateNodeInfo :: ModuleContext -> TypedCoreValidationPath -> Set TypedTypeParameterId -> TypedNodeInfo -> [TypedCoreValidationFailure]
validateNodeInfo context path parameterScope (TypedNodeInfo typeValue recipe instantiations evidenceSelections) =
  validateType path parameterScope typeValue
    <> validateDataTypeApplications context path typeValue
    <> validateRecipe path parameterScope recipe
    <> validateTypeRecipe path parameterScope typeValue recipe
    <> concatMap (validateInstantiation context path) instantiations
    <> validateEvidenceSelections context path typeValue evidenceSelections
    <> validateEvidenceParameterBindings context path instantiations evidenceSelections
    <> concatMap (validateEvidenceSelectionDataTypes context path) evidenceSelections

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

validateInstantiation :: ModuleContext -> TypedCoreValidationPath -> TypedInstantiation -> [TypedCoreValidationFailure]
validateInstantiation context path (TypedInstantiation owner arguments _) =
  case Map.lookup owner (moduleContextSchemes context) of
    Nothing -> [failure path TypedInstantiationMismatch (TypedBinderDetail owner)]
    Just (TypedScheme _ parameters _ _ _ _) ->
      if map typeArgumentParameter arguments == parameters
        then
          concatMap
            (\argument -> validateType path Set.empty (typeArgumentType argument) <> validateDataTypeApplications context path (typeArgumentType argument))
            arguments
        else [failure path TypedInstantiationMismatch (TypedBinderDetail owner)]
  where
    typeArgumentParameter (TypedTypeArgument parameterId _) = parameterId
    typeArgumentType (TypedTypeArgument _ typeValue) = typeValue

validateEvidenceSelections :: ModuleContext -> TypedCoreValidationPath -> TypedType -> [TypedEvidenceSelection] -> [TypedCoreValidationFailure]
validateEvidenceSelections context path expressionType selections =
  concatMap validateSelection selections <> duplicateEvidenceUseFailures path selections
  where
    validateSelection selection =
      case selection of
        TypedSelectedEvidence evidenceUse -> validateEvidenceUse context path evidenceUse
        TypedEvidenceCandidates constraint@(TypedCapabilityConstraint capability _ _) candidates
          | null candidates -> [failure path TypedMissingEvidence (TypedTextDetail capability)]
          | length candidates > 1 && not (isFunctionType expressionType) ->
              [failure path TypedAmbiguousEvidence (TypedArityDetail 1 (length candidates))]
          | otherwise -> concatMap (validateEvidenceCandidate context path constraint) candidates

validateEvidenceParameterBindings :: ModuleContext -> TypedCoreValidationPath -> [TypedInstantiation] -> [TypedEvidenceSelection] -> [TypedCoreValidationFailure]
validateEvidenceParameterBindings context path instantiations selections =
  concatMap validateSelection selections
  where
    expectedBindings = concatMap expectedBindingsFor instantiations
    validateSelection selection =
      case selection of
        TypedSelectedEvidence (TypedEvidenceUse (Just parameterId) constraint _ _)
          | (parameterId, constraint) `elem` expectedBindings -> []
          | otherwise -> [failure path TypedInstantiationMismatch (TypedEvidenceParameterDetail parameterId)]
        _ -> []
    expectedBindingsFor (TypedInstantiation owner arguments _) =
      case Map.lookup owner (moduleContextSchemes context) of
        Just (TypedScheme _ parameters evidenceParameters _ _ _)
          | map typeArgumentParameter arguments == parameters ->
              [ (parameterId, instantiateConstraint owner substitutions constraint)
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

duplicateEvidenceUseFailures :: TypedCoreValidationPath -> [TypedEvidenceSelection] -> [TypedCoreValidationFailure]
duplicateEvidenceUseFailures path selections = snd (foldl' step (Set.empty, []) parameterIds)
  where
    parameterIds =
      [ parameterId
      | TypedSelectedEvidence (TypedEvidenceUse (Just parameterId) _ _ _) <- selections
      ]
    step (seen, failures) parameterId
      | Set.member parameterId seen =
          ( seen,
            failures
              <> [ failure
                     path
                     TypedDuplicateEvidence
                     (TypedEvidenceParameterDetail parameterId)
                 ]
          )
      | otherwise = (Set.insert parameterId seen, failures)

validateEvidenceUse :: ModuleContext -> TypedCoreValidationPath -> TypedEvidenceUse -> [TypedCoreValidationFailure]
validateEvidenceUse context path (TypedEvidenceUse _ constraint@(TypedCapabilityConstraint capability constraintMethod targetType) implId maybeMethodId) =
  validateCapabilityConstraint path Set.empty constraint
    <> validateImplId context path Set.empty implId
    <> capabilityFailures
    <> targetFailures
    <> visibilityFailures
    <> methodFailures
  where
    capabilityFailures =
      case implId of
        TypedImplId _ capabilityName _
          | coreNameIdentifier capabilityName == Just capability -> []
          | otherwise -> [failure path TypedMethodSelectionMismatch (TypedTextDetail capability)]
    targetFailures =
      case implTargetTypes implId of
        target : _
          | target == targetType -> []
          | otherwise -> [failure path TypedMethodSelectionMismatch (TypedTypeDetail targetType target)]
        [] -> [failure path TypedMethodSelectionMismatch (TypedArityDetail 1 0)]
    visibilityFailures
      | implModulePath implId == ["Prelude"] = []
      | Set.member implId (moduleContextVisibleImpls context) = []
      | otherwise = [failure path TypedInvisibleImpl (TypedImplDetail implId)]
    methodFailures =
      case (constraintMethod, maybeMethodId) of
        (Nothing, Nothing) -> []
        (Nothing, Just methodId@(TypedMethodId methodImplId methodName)) ->
          validateMethodId context path Set.empty methodId
            <> (if methodImplId == implId then [] else [failure path TypedMethodSelectionMismatch (TypedImplDetail methodImplId)])
            <> [failure path TypedMethodSelectionMismatch (TypedTextDetail methodName)]
        (Just expectedMethod, Nothing) ->
          [failure path TypedMethodSelectionMismatch (TypedTextDetail expectedMethod)]
        (Just expectedMethod, Just methodId@(TypedMethodId methodImplId methodName)) ->
          validateMethodId context path Set.empty methodId
            <> (if methodImplId == implId then [] else [failure path TypedMethodSelectionMismatch (TypedImplDetail methodImplId)])
            <> (if methodKeyMatches expectedMethod methodName then [] else [failure path TypedMethodSelectionMismatch (TypedTextDetail expectedMethod)])

methodKeyMatches :: Text -> Text -> Bool
methodKeyMatches expected actual = expected == actual || Text.isSuffixOf ("." <> actual) expected

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
    _ -> []

nodeContractFailures :: TypedCoreValidationPath -> TypedCoreValidationKind -> TypedNodeInfo -> TypedNodeInfo -> [TypedCoreValidationFailure]
nodeContractFailures path kind expected actual
  | nodeType expected /= nodeType actual = [failure path kind (TypedTypeDetail (nodeType expected) (nodeType actual))]
  | nodeRecipe expected /= nodeRecipe actual = [failure path kind (TypedRecipeDetail (nodeRecipe expected) (nodeRecipe actual))]
  | otherwise = []

validateVisibleNameInNamespaces :: [TypedNameNamespace] -> ModuleContext -> TypedCoreValidationPath -> TypedCoreName -> [TypedCoreValidationFailure]
validateVisibleNameInNamespaces allowedNamespaces context path name =
  validateCoreName path name
    <> ( case resolvedNameKey (moduleContextPath context) name of
           Just key
             | not (nameUsesAllowedNamespace name) || Set.notMember key (moduleContextVisibleNames context) ->
                 [failure path TypedInvisibleName (TypedNameDetail name)]
           _ -> []
       )
  where
    nameUsesAllowedNamespace candidate =
      case candidate of
        TypedResolvedName _ namespace _ -> namespace `elem` allowedNamespaces
        _ -> True

validateOperatorRef :: ModuleContext -> TypedCoreValidationPath -> TypedOperatorRef -> [TypedCoreValidationFailure]
validateOperatorRef context path operator =
  case operator of
    TypedBuiltinOperator _ -> []
    TypedResolvedOperator name _ -> validateVisibleNameInNamespaces [TypedValueNamespace] context path name

validateOperatorValue :: ModuleContext -> TypedCoreValidationPath -> TypedNodeInfo -> TypedOperatorRef -> [TypedCoreValidationFailure]
validateOperatorValue context path info operator =
  validateOperatorRef context path operator
    <> case operatorContractType context path info operator of
      (contractFailures, Just expectedType)
        | expectedType /= nodeType info ->
            contractFailures <> [failure path TypedBindingValueMismatch (TypedTypeDetail expectedType (nodeType info))]
      (contractFailures, _) -> contractFailures

validateBinaryOperator :: ModuleContext -> TypedCoreValidationPath -> TypedNodeInfo -> TypedOperatorRef -> TypedExpr -> TypedExpr -> [TypedCoreValidationFailure]
validateBinaryOperator context path info operator left right =
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

operatorContractType :: ModuleContext -> TypedCoreValidationPath -> TypedNodeInfo -> TypedOperatorRef -> ([TypedCoreValidationFailure], Maybe TypedType)
operatorContractType _ _ _ (TypedBuiltinOperator _) = ([], Nothing)
operatorContractType context path info (TypedResolvedOperator name _) =
  case lookupSchemeByName context name of
    Nothing -> ([], Nothing)
    Just (TypedScheme owner parameters _ _ resultType _) ->
      let ownerPath = binderModulePath owner
          qualifiedType
            | ownerPath == moduleContextPath context = resultType
            | otherwise = qualifyExternalType ownerPath resultType
       in if null parameters
            then ([], Just qualifiedType)
            else case find (instantiates owner) (nodeInfoInstantiations info) of
              Nothing -> ([failure path TypedInstantiationMismatch (TypedBinderDetail owner)], Nothing)
              Just (TypedInstantiation _ arguments _)
                | map typeArgumentParameter arguments == parameters ->
                    ([], Just (substituteTypeParameters (Map.fromList [(parameterId, typeValue) | TypedTypeArgument parameterId typeValue <- arguments]) qualifiedType))
                | otherwise -> ([], Nothing)
  where
    instantiates expectedOwner (TypedInstantiation actualOwner _ _) = expectedOwner == actualOwner
    typeArgumentParameter (TypedTypeArgument parameterId _) = parameterId

validateImplId :: ModuleContext -> TypedCoreValidationPath -> Set TypedTypeParameterId -> TypedImplId -> [TypedCoreValidationFailure]
validateImplId context path scope (TypedImplId _ capability arguments) =
  validateCapabilityName context path capability <> concatMap (validateType path scope) arguments

validateMethodId :: ModuleContext -> TypedCoreValidationPath -> Set TypedTypeParameterId -> TypedMethodId -> [TypedCoreValidationFailure]
validateMethodId context path scope (TypedMethodId implId _) = validateImplId context path scope implId

validateCapabilityName :: ModuleContext -> TypedCoreValidationPath -> TypedCoreName -> [TypedCoreValidationFailure]
validateCapabilityName context path name =
  case name of
    TypedResolvedName TypedAmbientPrelude TypedCapabilityNamespace _ -> []
    _ -> validateVisibleNameInNamespaces [TypedCapabilityNamespace] context path name

validateModuleInterface :: TypedModule -> [TypedCoreValidationFailure]
validateModuleInterface (TypedModule modulePath _ _ exports (TypedModuleInterface values datas classes impls) statements _) =
  concatMap validateValueInterface values
    <> concatMap validateDataInterface datas
    <> concatMap validateClassInterface classes
    <> concatMap validateImplInterface impls
    <> concatMap validateExport exports
  where
    path = TypedInterfacePath modulePath
    declaredValues =
      [ (name, scheme)
      | statement <- statements,
        (name, scheme) <- statementValueDeclarations statement
      ]
    declaredDatas = [declaration | TypedDataStatement declaration <- statements]
    declaredClasses = [declaration | TypedClassStatement declaration <- statements]
    declaredImpls = [implId | TypedImplStatement (TypedImplDeclaration _ implId _) <- statements]
    validateValueInterface (TypedValueInterface name scheme)
      | (name, scheme) `elem` declaredValues && moduleExportsName TypedValueNamespace name exports = []
      | otherwise = [failure path TypedModuleInterfaceMismatch (TypedNameDetail name)]
    validateDataInterface (TypedDataInterface declaration@(TypedDataDeclaration _ name _ _))
      | declaration `elem` declaredDatas = []
      | otherwise = [failure path TypedModuleInterfaceMismatch (TypedNameDetail name)]
    validateClassInterface (TypedClassInterface declaration@(TypedClassDeclaration _ name _ _))
      | declaration `elem` declaredClasses = []
      | otherwise = [failure path TypedModuleInterfaceMismatch (TypedNameDetail name)]
    validateImplInterface (TypedImplInterface implId)
      | implId `elem` declaredImpls = []
      | otherwise = [failure path TypedModuleInterfaceMismatch (TypedImplDetail implId)]
    validateExport (TypedModuleExport namespace exportedName) =
      case namespace of
        TypedValueNamespace
          | any (interfaceNameMatches exportedName) values -> []
        TypedTypeNamespace
          | any (dataInterfaceNameMatches exportedName) datas || any (classInterfaceNameMatches exportedName) classes -> []
        TypedConstructorNamespace
          | any (dataInterfaceConstructorMatches exportedName) datas -> []
        TypedCapabilityNamespace
          | any (classInterfaceNameMatches exportedName) classes -> []
        _ -> [failure path TypedModuleInterfaceMismatch (TypedNameDetail (TypedResolvedName TypedCurrentModule namespace exportedName))]

statementValueDeclarations :: TypedStatement -> [(TypedCoreName, TypedScheme)]
statementValueDeclarations statement =
  case statement of
    TypedLetStatement _ name _ scheme _ -> [(name, scheme)]
    TypedSignatureStatement _ name _ scheme -> [(name, scheme)]
    _ -> []

interfaceNameMatches :: Text -> TypedValueInterface -> Bool
interfaceNameMatches expected (TypedValueInterface name _) = coreNameIdentifier name == Just expected

dataInterfaceNameMatches :: Text -> TypedDataInterface -> Bool
dataInterfaceNameMatches expected (TypedDataInterface (TypedDataDeclaration _ name _ _)) = coreNameIdentifier name == Just expected

classInterfaceNameMatches :: Text -> TypedClassInterface -> Bool
classInterfaceNameMatches expected (TypedClassInterface (TypedClassDeclaration _ name _ _)) = coreNameIdentifier name == Just expected

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
