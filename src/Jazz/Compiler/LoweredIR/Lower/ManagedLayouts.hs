{-# LANGUAGE OverloadedStrings #-}

-- | Deterministic layout discovery for managed products and local variants.
module Jazz.Compiler.LoweredIR.Lower.ManagedLayouts
  ( ManagedConstructorLayout (..),
    ManagedLayoutCatalog,
    collectManagedLayoutCatalog,
    orderedManagedLayouts,
    managedLayoutShapeFor,
    representationForRecipe,
    constructorLayoutFor,
    constructorApplicationLayout,
    productLayoutFields,
    nodeInstantiations,
  )
where

import Control.Monad (foldM, join)
import Data.Bifunctor (first)
import Data.Foldable (toList)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Jazz.Compiler.LoweredIR
import Jazz.Compiler.LoweredIR.Lower.Types
  ( ConstructorTemplate (..),
    LoweredIRLoweringDetail (..),
    LoweredIRLoweringFailure (..),
    LoweredIRLoweringKind (..),
    ManagedConstructorLayout (..),
    ManagedLayoutCatalog (..),
  )
import Jazz.Compiler.LoweredIR.RuntimeServiceCatalog (textRepresentation)
import Jazz.Compiler.TypedCore
import Numeric.Natural (Natural)

data CatalogBuild = CatalogBuild
  { buildOrder :: Seq LoweredLayoutId,
    buildShapes :: Map LoweredLayoutId (Maybe LoweredLayoutShape)
  }

collectManagedLayoutCatalog :: TypedModule -> Either [LoweredIRLoweringFailure] ManagedLayoutCatalog
collectManagedLayoutCatalog typedModule@(TypedModule modulePath _ _ _ moduleInterface _ statements _) = do
  finalBuild <- collectModuleRecipes declarations modulePath typedModule emptyBuild
  layouts <-
    case traverse
      (\layoutId -> LoweredLayout layoutId <$> join (Map.lookup layoutId (buildShapes finalBuild)))
      (toList (buildOrder finalBuild)) of
      Just values -> Right values
      Nothing ->
        Left
          [ LoweredIRLoweringFailure
              (TypedModulePath modulePath)
              LoweredIRUnsupportedRepresentation
              LoweredIRNoFailureDetail
          ]
  pure
    ManagedLayoutCatalog
      { catalogModulePath = modulePath,
        catalogConstructors = constructors,
        catalogLayoutShapes =
          Map.fromList
            [ (layoutId, shape)
            | LoweredLayout layoutId shape <- layouts
            ],
        catalogLayouts = layouts
      }
  where
    declarationValues = orderedDataDeclarations moduleInterface statements
    declarations = Map.fromList [(dataDeclarationName declaration, declaration) | declaration <- declarationValues]
    constructors =
      Map.fromList
        [ ( binder,
            ConstructorTemplate
              { constructorTemplateDataName = dataDeclarationName declaration,
                constructorTemplateParameters = dataDeclarationParameters declaration,
                constructorTemplateTag = tag,
                constructorTemplateFieldRecipes = fieldRecipes
              }
          )
        | declaration <- declarationValues,
          (tag, TypedConstructorDeclaration binder _ _ fieldRecipes) <- zip [0 :: Natural ..] (dataDeclarationConstructors declaration)
        ]

    emptyBuild = CatalogBuild Seq.empty Map.empty

    collectModuleRecipes dataDeclarations path (TypedModule _ _ _ _ interface _ moduleStatements info) build = do
      afterInterface <- collectInterface dataDeclarations path interface build
      afterStatements <- collectStatements dataDeclarations path 0 moduleStatements afterInterface
      collectObservedRecipe dataDeclarations path (TypedModulePath path) afterStatements (typedNodeRecipe info)

orderedManagedLayouts :: ManagedLayoutCatalog -> [LoweredLayout]
orderedManagedLayouts = catalogLayouts

managedLayoutShapeFor :: ManagedLayoutCatalog -> LoweredLayoutId -> Maybe LoweredLayoutShape
managedLayoutShapeFor catalog layoutId = Map.lookup layoutId (catalogLayoutShapes catalog)

representationForRecipe :: ManagedLayoutCatalog -> TypedRepresentationRecipe -> Maybe LoweredRepresentation
representationForRecipe catalog = representationForKnownRecipe catalog

constructorLayoutFor :: ManagedLayoutCatalog -> TypedBinderId -> [TypedInstantiation] -> Maybe ManagedConstructorLayout
constructorLayoutFor catalog binder instantiations = do
  constructor <- Map.lookup binder (catalogConstructors catalog)
  bindings <- constructorBindings constructor binder instantiations
  concreteRecipes <-
    either
      (const Nothing)
      Just
      (traverse (substituteRecipe bindings) (constructorTemplateFieldRecipes constructor))
  fieldRepresentations <- traverse (representationForKnownRecipe catalog) concreteRecipes
  dataArguments <-
    traverse
      (\parameter -> fst <$> Map.lookup parameter bindings)
      (constructorTemplateParameters constructor)
  let variantRecipe = TypedManagedVariantRecipe (constructorTemplateDataName constructor) dataArguments
  layoutId <- managedLayoutId (catalogModulePath catalog) variantRecipe
  if Map.member layoutId (catalogLayoutShapes catalog)
    then
      Just
        ManagedConstructorLayout
          { managedConstructorLayoutId = layoutId,
            managedConstructorTag = constructorTemplateTag constructor,
            managedConstructorFields = fieldRepresentations
          }
    else Nothing

constructorApplicationLayout :: ManagedLayoutCatalog -> TypedExpr -> Maybe ManagedConstructorLayout
constructorApplicationLayout catalog callee =
  case callee of
    TypedVariableExpr info _ (Just binder) ->
      constructorLayoutFor catalog binder (nodeInstantiations info)
    _ -> Nothing

productLayoutFields :: ManagedLayoutCatalog -> LoweredLayoutId -> Maybe [LoweredRepresentation]
productLayoutFields catalog layoutId =
  case managedLayoutShapeFor catalog layoutId of
    Just (LoweredProductLayout fields) -> Just fields
    _ -> Nothing

nodeInstantiations :: TypedNodeInfo -> [TypedInstantiation]
nodeInstantiations (TypedNodeInfo _ _ instantiations _) = instantiations

orderedDataDeclarations :: TypedModuleInterface -> [TypedStatement] -> [TypedDataDeclaration]
orderedDataDeclarations (TypedModuleInterface _ interfaceDatas _ _) statements =
  stableDeclarations
    ( [declaration | TypedDataStatement declaration <- statements]
        <> [declaration | TypedDataInterface declaration <- interfaceDatas]
    )
  where
    stableDeclarations = reverse . snd . foldl' keep (Set.empty, [])
    keep (seen, reversed) declaration
      | Set.member (dataDeclarationName declaration) seen = (seen, reversed)
      | otherwise = (Set.insert (dataDeclarationName declaration) seen, declaration : reversed)

collectInterface :: Map TypedCoreName TypedDataDeclaration -> [Text] -> TypedModuleInterface -> CatalogBuild -> Either [LoweredIRLoweringFailure] CatalogBuild
collectInterface declarations modulePath (TypedModuleInterface values _ _ _) build =
  foldM
    (\current (TypedValueInterface _ scheme) -> collectScheme declarations modulePath (TypedModulePath modulePath) scheme current)
    build
    values

collectStatements :: Map TypedCoreName TypedDataDeclaration -> [Text] -> Int -> [TypedStatement] -> CatalogBuild -> Either [LoweredIRLoweringFailure] CatalogBuild
collectStatements _ _ _ [] build = Right build
collectStatements declarations modulePath statementIndex (statement : rest) build = do
  nextBuild <-
    case statement of
      TypedLetStatement _ _ _ scheme expression -> do
        afterScheme <- collectScheme declarations modulePath statementPath scheme build
        collectExpression declarations modulePath [statementIndex] [0] expression afterScheme
      TypedSignatureStatement _ _ _ scheme ->
        collectScheme declarations modulePath statementPath scheme build
      TypedExpressionStatement _ expression ->
        collectExpression declarations modulePath [statementIndex] [0] expression build
      TypedImplStatement (TypedImplDeclaration _ _ methods) ->
        foldM
          ( \current (TypedMethodDefinition _ _ _ _ body) -> do
              collectExpression declarations modulePath [statementIndex] [0] body current
          )
          build
          methods
      _ -> Right build
  collectStatements declarations modulePath (statementIndex + 1) rest nextBuild
  where
    statementPath = TypedStatementPath modulePath [statementIndex]

collectScheme :: Map TypedCoreName TypedDataDeclaration -> [Text] -> TypedCoreValidationPath -> TypedScheme -> CatalogBuild -> Either [LoweredIRLoweringFailure] CatalogBuild
collectScheme declarations modulePath path (TypedScheme _ _ _ _ _ recipe _) build =
  collectObservedRecipe declarations modulePath path build recipe

collectExpression :: Map TypedCoreName TypedDataDeclaration -> [Text] -> [Int] -> [Int] -> TypedExpr -> CatalogBuild -> Either [LoweredIRLoweringFailure] CatalogBuild
collectExpression declarations modulePath statementPath expressionPath expression build = do
  afterInfo <- collectObservedRecipe declarations modulePath path build (typedNodeRecipe (typedExpressionInfo expression))
  case expression of
    TypedLiteralExpr {} -> Right afterInfo
    TypedVariableExpr {} -> Right afterInfo
    TypedLambdaExpr _ _ _ body -> child [0] body afterInfo
    TypedOperatorValueExpr {} -> Right afterInfo
    TypedListExpr _ values -> children (zipWith (\index value -> ([index], value)) [0 ..] values) afterInfo
    TypedTupleExpr _ values -> children (zipWith (\index value -> ([index], value)) [0 ..] values) afterInfo
    TypedApplyExpr _ function argument -> children [([0], function), ([1], argument)] afterInfo
    TypedTypeApplicationExpr _ function _ _ -> child [0] function afterInfo
    TypedIfExpr _ condition consequent alternative ->
      children [([0], condition), ([1], consequent), ([2], alternative)] afterInfo
    TypedPatternCaseExpr _ scrutinee arms -> do
      afterScrutinee <- child [0] scrutinee afterInfo
      foldM collectArm afterScrutinee (zip [0 :: Int ..] arms)
    TypedBinaryExpr _ _ left right -> children [([0], left), ([1], right)] afterInfo
    TypedLeftSectionExpr _ left _ -> child [0] left afterInfo
    TypedRightSectionExpr _ _ right -> child [0] right afterInfo
    TypedBlockExpr _ blockStatements -> collectStatements declarations modulePath 0 blockStatements afterInfo
  where
    path = TypedExpressionPath modulePath statementPath expressionPath
    child suffix value = collectExpression declarations modulePath statementPath (expressionPath <> suffix) value
    children values initial = foldM (\current (suffix, value) -> child suffix value current) initial values
    collectArm current (armIndex, TypedCaseArm patternValue maybeGuard result) = do
      afterPattern <- collectPattern declarations modulePath statementPath (expressionPath <> [armIndex]) patternValue current
      afterGuard <-
        case maybeGuard of
          Nothing -> Right afterPattern
          Just guardExpression -> child [armIndex + 1, 0] guardExpression afterPattern
      child [armIndex + 1, 1] result afterGuard

collectPattern :: Map TypedCoreName TypedDataDeclaration -> [Text] -> [Int] -> [Int] -> TypedPattern -> CatalogBuild -> Either [LoweredIRLoweringFailure] CatalogBuild
collectPattern declarations modulePath statementPath patternPath patternValue build = do
  afterInfo <- collectObservedRecipe declarations modulePath path build (typedNodeRecipe (patternInfo patternValue))
  foldM collectChild afterInfo (zip [0 :: Int ..] (patternChildren patternValue))
  where
    path = TypedPatternPath modulePath statementPath patternPath
    collectChild current (index, child) = collectPattern declarations modulePath statementPath (patternPath <> [index]) child current

collectObservedRecipe :: Map TypedCoreName TypedDataDeclaration -> [Text] -> TypedCoreValidationPath -> CatalogBuild -> TypedRepresentationRecipe -> Either [LoweredIRLoweringFailure] CatalogBuild
collectObservedRecipe declarations modulePath path build recipe =
  case recipe of
    TypedManagedProductRecipe {} -> snd <$> collectRecipe declarations modulePath path build recipe
    TypedManagedVariantRecipe {} -> snd <$> collectRecipe declarations modulePath path build recipe
    TypedClosureRecipe arguments result -> do
      afterArguments <- foldM (collectObservedRecipe declarations modulePath path) build arguments
      collectObservedRecipe declarations modulePath path afterArguments result
    _ -> Right build

collectRecipe :: Map TypedCoreName TypedDataDeclaration -> [Text] -> TypedCoreValidationPath -> CatalogBuild -> TypedRepresentationRecipe -> Either [LoweredIRLoweringFailure] (LoweredRepresentation, CatalogBuild)
collectRecipe declarations modulePath path build recipe =
  case recipe of
    TypedUnitRecipe -> scalar LoweredUnitRepresentation
    TypedBoolRecipe -> scalar LoweredBoolRepresentation
    TypedSignedIntegerRecipe bits -> maybeFailure (LoweredSignedIntegerRepresentation <$> integerWidth bits)
    TypedUnsignedIntegerRecipe bits -> maybeFailure (LoweredUnsignedIntegerRepresentation <$> integerWidth bits)
    TypedFloatRecipe bits -> maybeFailure (LoweredFloatRepresentation <$> floatWidth bits)
    TypedCharRecipe -> scalar LoweredCharRepresentation
    TypedManagedTextRecipe -> scalar textRepresentation
    TypedManagedProductRecipe fields -> collectProduct fields
    TypedManagedVariantRecipe dataName arguments -> collectVariant dataName arguments
    TypedClosureRecipe arguments result -> do
      (argumentRepresentations, afterArguments) <- collectRecipes declarations modulePath path build arguments
      (resultRepresentation, afterResult) <- collectRecipe declarations modulePath path afterArguments result
      scalarWith afterResult (LoweredClosureRepresentation (LoweredCallSignature argumentRepresentations resultRepresentation))
    _ -> failure
  where
    scalar representation = Right (representation, build)
    scalarWith current representation = Right (representation, current)
    maybeFailure maybeRepresentation = maybe failure scalar maybeRepresentation
    failure = Left [unsupportedRepresentation path recipe]

    collectProduct fields = do
      layoutId <- maybe failure Right (managedLayoutId modulePath recipe)
      let representation = LoweredManagedReferenceRepresentation layoutId
      if Map.member layoutId (buildShapes build)
        then Right (representation, build)
        else do
          let reserved = reserveLayout layoutId build
          (fieldRepresentations, afterFields) <- collectRecipes declarations modulePath path reserved fields
          Right (representation, defineLayout layoutId (LoweredProductLayout fieldRepresentations) afterFields)

    collectVariant dataName arguments = do
      declaration <- maybe failure Right (Map.lookup dataName declarations)
      let parameters = dataDeclarationParameters declaration
      if length parameters /= length arguments
        then failure
        else do
          layoutId <- maybe failure Right (managedLayoutId modulePath recipe)
          let representation = LoweredManagedReferenceRepresentation layoutId
          if Map.member layoutId (buildShapes build)
            then Right (representation, build)
            else do
              bindings <- maybe failure Right (typeBindings parameters arguments)
              let reserved = reserveLayout layoutId build
              (variants, afterVariants) <-
                collectConstructors declarations modulePath path bindings reserved (dataDeclarationConstructors declaration)
              Right (representation, defineLayout layoutId (LoweredVariantLayouts variants) afterVariants)

collectRecipes :: Map TypedCoreName TypedDataDeclaration -> [Text] -> TypedCoreValidationPath -> CatalogBuild -> [TypedRepresentationRecipe] -> Either [LoweredIRLoweringFailure] ([LoweredRepresentation], CatalogBuild)
collectRecipes declarations modulePath path = go []
  where
    go reversed build [] = Right (reverse reversed, build)
    go reversed build (recipe : rest) = do
      (representation, nextBuild) <- collectRecipe declarations modulePath path build recipe
      go (representation : reversed) nextBuild rest

collectConstructors :: Map TypedCoreName TypedDataDeclaration -> [Text] -> TypedCoreValidationPath -> Map TypedTypeParameterId (TypedType, TypedRepresentationRecipe) -> CatalogBuild -> [TypedConstructorDeclaration] -> Either [LoweredIRLoweringFailure] ([LoweredVariantLayout], CatalogBuild)
collectConstructors declarations modulePath path bindings = go (0 :: Natural) []
  where
    go _ reversed build [] = Right (reverse reversed, build)
    go tag reversed build (TypedConstructorDeclaration _ _ _ fieldRecipes : rest) = do
      concreteRecipes <-
        case traverse (substituteRecipe bindings) fieldRecipes of
          Right recipes -> Right recipes
          Left recipe -> Left [unsupportedRepresentation path recipe]
      (fieldRepresentations, nextBuild) <- collectRecipes declarations modulePath path build concreteRecipes
      go (tag + 1) (LoweredVariantLayout (fromIntegral tag) fieldRepresentations : reversed) nextBuild rest

representationForKnownRecipe :: ManagedLayoutCatalog -> TypedRepresentationRecipe -> Maybe LoweredRepresentation
representationForKnownRecipe catalog recipe =
  case recipe of
    TypedUnitRecipe -> Just LoweredUnitRepresentation
    TypedBoolRecipe -> Just LoweredBoolRepresentation
    TypedSignedIntegerRecipe bits -> LoweredSignedIntegerRepresentation <$> integerWidth bits
    TypedUnsignedIntegerRecipe bits -> LoweredUnsignedIntegerRepresentation <$> integerWidth bits
    TypedFloatRecipe bits -> LoweredFloatRepresentation <$> floatWidth bits
    TypedCharRecipe -> Just LoweredCharRepresentation
    TypedManagedTextRecipe -> Just textRepresentation
    TypedManagedProductRecipe _ -> managedReference
    TypedManagedVariantRecipe _ _ -> managedReference
    TypedClosureRecipe arguments result -> do
      argumentRepresentations <- traverse (representationForKnownRecipe catalog) arguments
      resultRepresentation <- representationForKnownRecipe catalog result
      pure (LoweredClosureRepresentation (LoweredCallSignature argumentRepresentations resultRepresentation))
    _ -> Nothing
  where
    managedReference = do
      layoutId <- managedLayoutId (catalogModulePath catalog) recipe
      if Map.member layoutId (catalogLayoutShapes catalog)
        then Just (LoweredManagedReferenceRepresentation layoutId)
        else Nothing

constructorBindings :: ConstructorTemplate -> TypedBinderId -> [TypedInstantiation] -> Maybe (Map TypedTypeParameterId (TypedType, TypedRepresentationRecipe))
constructorBindings constructor binder instantiations =
  case constructorTemplateParameters constructor of
    [] ->
      if null matchingArguments
        then Just Map.empty
        else Nothing
    parameters -> do
      arguments <- single matchingArguments
      if map typeArgumentParameter arguments == parameters
        then typeBindings parameters (map typeArgumentType arguments)
        else Nothing
  where
    matchingArguments =
      [ arguments
      | TypedInstantiation owner arguments _ <- instantiations,
        owner == binder
      ]

typeBindings :: [TypedTypeParameterId] -> [TypedType] -> Maybe (Map TypedTypeParameterId (TypedType, TypedRepresentationRecipe))
typeBindings parameters arguments = do
  if length parameters == length arguments then pure () else Nothing
  recipes <- traverse recipeForType arguments
  pure (Map.fromList (zip parameters (zip arguments recipes)))

substituteRecipe :: Map TypedTypeParameterId (TypedType, TypedRepresentationRecipe) -> TypedRepresentationRecipe -> Either TypedRepresentationRecipe TypedRepresentationRecipe
substituteRecipe bindings recipe =
  case recipe of
    TypedManagedListRecipe element -> TypedManagedListRecipe <$> child element
    TypedManagedProductRecipe fields -> TypedManagedProductRecipe <$> traverse child fields
    TypedManagedVariantRecipe dataName arguments ->
      TypedManagedVariantRecipe dataName
        <$> maybe (Left recipe) Right (traverse (substituteTypedType bindings) arguments)
    TypedClosureRecipe arguments result -> TypedClosureRecipe <$> traverse child arguments <*> child result
    TypedRepresentationParameterRecipe parameter -> maybe (Left recipe) (Right . snd) (Map.lookup parameter bindings)
    _ -> Right recipe
  where
    child = first (const recipe) . substituteRecipe bindings

substituteTypedType :: Map TypedTypeParameterId (TypedType, TypedRepresentationRecipe) -> TypedType -> Maybe TypedType
substituteTypedType bindings typeValue =
  case typeValue of
    TypedListType element -> TypedListType <$> child element
    TypedTupleType elements -> TypedTupleType <$> traverse child elements
    TypedDataType name arguments -> TypedDataType name <$> traverse child arguments
    TypedFunctionType argument result -> TypedFunctionType <$> child argument <*> child result
    TypedTypeParameterType parameter -> fst <$> Map.lookup parameter bindings
    _ -> Just typeValue
  where
    child = substituteTypedType bindings

recipeForType :: TypedType -> Maybe TypedRepresentationRecipe
recipeForType typeValue =
  case typeValue of
    TypedIntType -> Just (TypedSignedIntegerRecipe 64)
    TypedFloatType -> Just (TypedFloatRecipe 64)
    TypedNumericType numericType -> numericRecipe numericType
    TypedBoolType -> Just TypedBoolRecipe
    TypedCharType -> Just TypedCharRecipe
    TypedTextType -> Just TypedManagedTextRecipe
    TypedListType element -> TypedManagedListRecipe <$> recipeForType element
    TypedTupleType elements ->
      case elements of
        [] -> Just TypedUnitRecipe
        _ -> TypedManagedProductRecipe <$> traverse recipeForType elements
    TypedDataType name arguments -> Just (TypedManagedVariantRecipe name arguments)
    TypedFunctionType argument result -> TypedClosureRecipe <$> ((: []) <$> recipeForType argument) <*> recipeForType result
    TypedTypeParameterType {} -> Nothing

managedLayoutId :: [Text] -> TypedRepresentationRecipe -> Maybe LoweredLayoutId
managedLayoutId modulePath recipe =
  case recipe of
    TypedManagedProductRecipe fields -> do
      fieldEncodings <- traverse (recipeEncoding modulePath) fields
      pure (LoweredLayoutId ("jazz.layout.product.v1$" <> sequenceValue "fields" fieldEncodings))
    TypedManagedVariantRecipe name arguments -> do
      identifier <- currentTypeIdentifier name
      argumentEncodings <- traverse (typeEncoding modulePath) arguments
      pure
        ( LoweredLayoutId
            ( "jazz.layout.variant.v1$"
                <> sequenceValue "module" modulePath
                <> "$name$"
                <> segment identifier
                <> "$"
                <> sequenceValue "args" argumentEncodings
            )
        )
    _ -> Nothing

recipeEncoding :: [Text] -> TypedRepresentationRecipe -> Maybe Text
recipeEncoding modulePath recipe =
  case recipe of
    TypedUnitRecipe -> Just "unit"
    TypedBoolRecipe -> Just "bool"
    TypedSignedIntegerRecipe bits -> Just ("signed" <> decimal bits)
    TypedUnsignedIntegerRecipe bits -> Just ("unsigned" <> decimal bits)
    TypedFloatRecipe bits -> Just ("float" <> decimal bits)
    TypedCharRecipe -> Just "char"
    TypedManagedTextRecipe -> Just "text"
    TypedManagedProductRecipe fields -> sequenceValue "product" <$> traverse (recipeEncoding modulePath) fields
    TypedManagedVariantRecipe name arguments -> do
      identifier <- currentTypeIdentifier name
      encodedArguments <- traverse (typeEncoding modulePath) arguments
      pure
        ( "variant$"
            <> sequenceValue "module" modulePath
            <> "$name$"
            <> segment identifier
            <> "$"
            <> sequenceValue "args" encodedArguments
        )
    TypedClosureRecipe arguments result -> do
      encodedArguments <- traverse (recipeEncoding modulePath) arguments
      encodedResult <- recipeEncoding modulePath result
      pure ("closure$" <> sequenceValue "args" encodedArguments <> "$result$" <> segment encodedResult)
    _ -> Nothing

typeEncoding :: [Text] -> TypedType -> Maybe Text
typeEncoding modulePath typeValue =
  case typeValue of
    TypedIntType -> Just "int"
    TypedFloatType -> Just "float"
    TypedNumericType numericType -> numericTypeEncoding modulePath numericType
    TypedBoolType -> Just "bool"
    TypedCharType -> Just "char"
    TypedTextType -> Just "text"
    TypedListType element -> ("list$" <>) . segment <$> typeEncoding modulePath element
    TypedTupleType elements -> sequenceValue "tuple" <$> traverse (typeEncoding modulePath) elements
    TypedDataType name arguments -> do
      identifier <- currentTypeIdentifier name
      encodedArguments <- traverse (typeEncoding modulePath) arguments
      pure
        ( "data$"
            <> sequenceValue "module" modulePath
            <> "$name$"
            <> segment identifier
            <> "$"
            <> sequenceValue "args" encodedArguments
        )
    TypedFunctionType argument result -> do
      encodedArgument <- typeEncoding modulePath argument
      encodedResult <- typeEncoding modulePath result
      pure ("function$" <> segment encodedArgument <> "$" <> segment encodedResult)
    TypedTypeParameterType {} -> Nothing

numericRecipe :: TypedNumericType -> Maybe TypedRepresentationRecipe
numericRecipe numericType =
  case numericType of
    TypedInt8Type -> Just (TypedSignedIntegerRecipe 8)
    TypedInt16Type -> Just (TypedSignedIntegerRecipe 16)
    TypedInt32Type -> Just (TypedSignedIntegerRecipe 32)
    TypedInt64Type -> Just (TypedSignedIntegerRecipe 64)
    TypedUInt8Type -> Just (TypedUnsignedIntegerRecipe 8)
    TypedUInt16Type -> Just (TypedUnsignedIntegerRecipe 16)
    TypedUInt32Type -> Just (TypedUnsignedIntegerRecipe 32)
    TypedUInt64Type -> Just (TypedUnsignedIntegerRecipe 64)
    TypedFloat16Type -> Just (TypedFloatRecipe 16)
    TypedFloat32Type -> Just (TypedFloatRecipe 32)
    TypedFloat64Type -> Just (TypedFloatRecipe 64)

numericTypeEncoding :: [Text] -> TypedNumericType -> Maybe Text
numericTypeEncoding modulePath numericType =
  case numericRecipe numericType of
    Just recipe -> recipeEncoding modulePath recipe
    Nothing -> Nothing

dataDeclarationName :: TypedDataDeclaration -> TypedCoreName
dataDeclarationName (TypedDataDeclaration _ name _ _) = name

dataDeclarationParameters :: TypedDataDeclaration -> [TypedTypeParameterId]
dataDeclarationParameters (TypedDataDeclaration _ _ parameters _) = parameters

dataDeclarationConstructors :: TypedDataDeclaration -> [TypedConstructorDeclaration]
dataDeclarationConstructors (TypedDataDeclaration _ _ _ constructors) = constructors

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

patternChildren :: TypedPattern -> [TypedPattern]
patternChildren patternValue =
  case patternValue of
    TypedConstructorPattern _ _ children -> children
    TypedListPattern _ children -> children
    TypedConsListPattern _ headPattern tailPattern -> [headPattern, tailPattern]
    TypedTuplePattern _ children -> children
    TypedAsPattern _ _ _ nested -> [nested]
    TypedOrPattern _ alternatives -> alternatives
    _ -> []

reserveLayout :: LoweredLayoutId -> CatalogBuild -> CatalogBuild
reserveLayout layoutId build =
  build
    { buildOrder = buildOrder build |> layoutId,
      buildShapes = Map.insert layoutId Nothing (buildShapes build)
    }

defineLayout :: LoweredLayoutId -> LoweredLayoutShape -> CatalogBuild -> CatalogBuild
defineLayout layoutId shape build =
  build {buildShapes = Map.insert layoutId (Just shape) (buildShapes build)}

unsupportedRepresentation :: TypedCoreValidationPath -> TypedRepresentationRecipe -> LoweredIRLoweringFailure
unsupportedRepresentation path recipe =
  LoweredIRLoweringFailure
    path
    LoweredIRUnsupportedRepresentation
    (LoweredIRRecipeFailureDetail recipe)

currentTypeIdentifier :: TypedCoreName -> Maybe Text
currentTypeIdentifier name =
  case name of
    TypedResolvedName TypedCurrentModule TypedTypeNamespace identifier -> Just identifier
    _ -> Nothing

sequenceValue :: Text -> [Text] -> Text
sequenceValue label values =
  label <> decimal (length values) <> foldMap (("$" <>) . segment) values

segment :: Text -> Text
segment value = decimal (Text.length value) <> ":" <> value

decimal :: (Show value) => value -> Text
decimal = Text.pack . show

integerWidth :: Int -> Maybe LoweredIntegerWidth
integerWidth bits =
  case bits of
    8 -> Just LoweredIntegerWidth8
    16 -> Just LoweredIntegerWidth16
    32 -> Just LoweredIntegerWidth32
    64 -> Just LoweredIntegerWidth64
    _ -> Nothing

floatWidth :: Int -> Maybe LoweredFloatWidth
floatWidth bits =
  case bits of
    16 -> Just LoweredFloatWidth16
    32 -> Just LoweredFloatWidth32
    64 -> Just LoweredFloatWidth64
    _ -> Nothing

single :: [value] -> Maybe value
single values =
  case values of
    [value] -> Just value
    _ -> Nothing

typeArgumentParameter :: TypedTypeArgument -> TypedTypeParameterId
typeArgumentParameter (TypedTypeArgument parameter _) = parameter

typeArgumentType :: TypedTypeArgument -> TypedType
typeArgumentType (TypedTypeArgument _ typeValue) = typeValue
