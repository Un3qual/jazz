-- | Finalization contracts for retained products and local data declarations.
module Jazz.Compiler.TypeInference.Elaboration.StructuredValues
  ( StructuredConstructor (..),
    StructuredValueCatalog,
    buildStructuredValueCatalog,
    concreteConstructorContract,
    concreteConstructorFieldTypes,
    structuredDataStatement,
    structuredNodeInfo,
    structuredConstructorAtStatement,
  )
where

import Control.Monad (guard)
import Data.Functor (unzip)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.List (find)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Jazz.Compiler.AST (NumericType (..))
import Jazz.Compiler.Diagnostics (SourceSpan (..))
import Jazz.Compiler.Name (Name, identifierText)
import Jazz.Compiler.TypeInference.Elaboration.Types
  ( ProvisionalConstructorDeclaration (..),
    ProvisionalDataDeclaration (..),
    ProvisionalTypedStatement (..),
    TypedCoreProductionFailure (..),
    TypedCoreProductionFailureDetail (..),
    TypedCoreProductionFailureKind (..),
    TypedCoreProductionPath (..),
  )
import Jazz.Compiler.TypeInference.Solver (resolveType)
import Jazz.Compiler.TypeInference.State (InferState)
import Jazz.Compiler.TypeInference.Types (ExpressionType (..))
import Jazz.Compiler.TypedCore
import Prelude hiding (unzip)

data StructuredConstructor = StructuredConstructor
  { structuredConstructorSourceName :: Name,
    structuredConstructorStatementIndex :: Int,
    structuredConstructorBinder :: TypedBinderId,
    structuredConstructorName :: TypedCoreName,
    structuredConstructorDataSourceName :: Name,
    structuredConstructorDataName :: TypedCoreName,
    structuredConstructorParameters :: [TypedTypeParameterId],
    structuredConstructorFieldTemplates :: [ExpressionType],
    structuredConstructorFieldContracts :: [(TypedType, TypedRepresentationRecipe)]
  }
  deriving (Eq, Show)

data StructuredDataSkeleton = StructuredDataSkeleton
  { skeletonSourceName :: Name,
    skeletonName :: TypedCoreName,
    skeletonStatementIndex :: Int,
    skeletonSpan :: SourceSpan,
    skeletonParameters :: [TypedTypeParameterId],
    skeletonConstructors :: [ProvisionalConstructorDeclaration]
  }
  deriving (Eq, Show)

data StructuredValueCatalog = StructuredValueCatalog
  { catalogDataSkeletons :: Map Name StructuredDataSkeleton,
    catalogConstructorsBySourceName :: Map Name (NonEmpty StructuredConstructor),
    catalogStatementsByIndex :: IntMap TypedStatement
  }
  deriving (Eq, Show)

buildStructuredValueCatalog ::
  [Text] ->
  InferState ->
  [ProvisionalTypedStatement] ->
  ([TypedCoreProductionFailure], StructuredValueCatalog)
buildStructuredValueCatalog modulePath state statements =
  let skeletons = mapMaybeSkeleton statements
      skeletonMap = Map.fromList [(skeletonSourceName skeleton, skeleton) | skeleton <- skeletons]
      resolvedResults = map (resolveData skeletonMap) skeletons
      (failures, resolvedData) = foldr collectResolution ([], []) resolvedResults
      constructors = concatMap snd resolvedData
      constructorsBySourceName =
        Map.fromListWith
          (flip (<>))
          [ (structuredConstructorSourceName constructor, constructor :| [])
          | constructor <- constructors
          ]
      statementsByIndex =
        IntMap.fromList [statementEntry | (statementEntry, _) <- resolvedData]
   in ( failures,
        StructuredValueCatalog
          { catalogDataSkeletons = skeletonMap,
            catalogConstructorsBySourceName = constructorsBySourceName,
            catalogStatementsByIndex = statementsByIndex
          }
      )
  where
    collectResolution result (failures, resolvedData) =
      case result of
        Left resolutionFailures -> (resolutionFailures <> failures, resolvedData)
        Right dataEntry -> (failures, dataEntry : resolvedData)

    mapMaybeSkeleton = foldr collectSkeleton []
    collectSkeleton statement collected =
      case statement of
        ProvisionalDataStatement (ProvisionalDataDeclaration statementIndex spanValue sourceName parameters constructors) ->
          StructuredDataSkeleton
            { skeletonSourceName = sourceName,
              skeletonName = resolvedTypeName sourceName,
              skeletonStatementIndex = statementIndex,
              skeletonSpan = spanValue,
              skeletonParameters = [TypedTypeParameterId index | index <- [0 .. length parameters - 1]],
              skeletonConstructors = constructors
            }
            : collected
        _ -> collected

    resolveData skeletonMap skeleton = do
      constructors <-
        traverse
          (uncurry (resolveConstructor skeletonMap skeleton))
          (zip [0 :: Int ..] (skeletonConstructors skeleton))
      let declaration =
            TypedDataDeclaration
              (typedSpan (skeletonSpan skeleton))
              (skeletonName skeleton)
              (skeletonParameters skeleton)
              (map constructorDeclaration constructors)
          statementEntry = (skeletonStatementIndex skeleton, TypedDataStatement declaration)
      pure (statementEntry, constructors)

    resolveConstructor skeletonMap skeleton constructorIndex (ProvisionalConstructorDeclaration sourceName fieldTemplates) = do
      let parameterVariables =
            Map.fromList
              [ (negate index - 1, parameter)
              | (index, parameter) <- zip [0 :: Int ..] (skeletonParameters skeleton)
              ]
          contract template = expressionContract skeletonMap parameterVariables state template
      fieldContracts <-
        case traverse contract fieldTemplates of
          Just values -> Right values
          Nothing -> Left [statementFailure (skeletonStatementIndex skeleton)]
      let name = resolvedConstructorName sourceName
          binder = TypedBinderId (modulePath, [skeletonStatementIndex skeleton, constructorIndex], name)
      pure
        StructuredConstructor
          { structuredConstructorSourceName = sourceName,
            structuredConstructorStatementIndex = skeletonStatementIndex skeleton,
            structuredConstructorBinder = binder,
            structuredConstructorName = name,
            structuredConstructorDataSourceName = skeletonSourceName skeleton,
            structuredConstructorDataName = skeletonName skeleton,
            structuredConstructorParameters = skeletonParameters skeleton,
            structuredConstructorFieldTemplates = fieldTemplates,
            structuredConstructorFieldContracts = fieldContracts
          }

    constructorDeclaration constructor =
      let (fieldTypes, fieldRecipes) = unzip (structuredConstructorFieldContracts constructor)
       in TypedConstructorDeclaration
            (structuredConstructorBinder constructor)
            (structuredConstructorName constructor)
            fieldTypes
            fieldRecipes

    statementFailure statementIndex =
      TypedCoreProductionFailure
        (TypedCoreProductionStatementPath modulePath statementIndex)
        TypedCoreStructuredValueUnsupported
        TypedCoreDataValueDetail

structuredDataStatement :: StructuredValueCatalog -> Int -> Maybe TypedStatement
structuredDataStatement catalog statementIndex = IntMap.lookup statementIndex (catalogStatementsByIndex catalog)

structuredNodeInfo :: StructuredValueCatalog -> InferState -> ExpressionType -> Maybe TypedNodeInfo
structuredNodeInfo catalog state expressionType = do
  (typeValue, recipe) <- expressionContract (catalogDataSkeletons catalog) Map.empty state expressionType
  pure (TypedNodeInfo typeValue recipe [] [])

structuredConstructorAtStatement :: StructuredValueCatalog -> Int -> Name -> Maybe StructuredConstructor
structuredConstructorAtStatement catalog statementIndex sourceName = do
  constructors <- Map.lookup sourceName (catalogConstructorsBySourceName catalog)
  find
    ((<= statementIndex) . structuredConstructorStatementIndex)
    (reverse (NonEmpty.toList constructors))

-- | Resolve a constructor's field and result contracts at a concrete data use.
-- The catalog remains the sole owner of declaration-era constructor metadata.
concreteConstructorContract ::
  StructuredValueCatalog ->
  InferState ->
  StructuredConstructor ->
  ExpressionType ->
  Maybe ([TypedNodeInfo], TypedNodeInfo, [TypedInstantiation])
concreteConstructorContract catalog state constructor resultExpressionType = do
  resultInfo@(TypedNodeInfo resultType _ _ _) <- structuredNodeInfo catalog state resultExpressionType
  concreteArguments <-
    case resultType of
      TypedDataType dataName arguments
        | dataName == structuredConstructorDataName constructor -> Just arguments
      _ -> Nothing
  guard (length concreteArguments == length (structuredConstructorParameters constructor))
  parameterContracts <- traverse parameterContract concreteArguments
  let bindings = Map.fromList (zip (structuredConstructorParameters constructor) parameterContracts)
  fieldContracts <- traverse (substituteFieldContract bindings) (structuredConstructorFieldContracts constructor)
  let fieldInfos = [TypedNodeInfo typeValue recipe [] [] | (typeValue, recipe) <- fieldContracts]
      instantiations =
        [ TypedInstantiation
            (structuredConstructorBinder constructor)
            (zipWith TypedTypeArgument (structuredConstructorParameters constructor) concreteArguments)
            Nothing
        | not (null concreteArguments)
        ]
  pure (fieldInfos, resultInfo, instantiations)
  where
    parameterContract typeValue = do
      recipe <- representationRecipeForTypedType typeValue
      pure (typeValue, recipe)
    substituteFieldContract bindings (typeValue, recipe) =
      (,)
        <$> substituteStructuredType bindings typeValue
        <*> substituteStructuredRecipe bindings recipe

concreteConstructorFieldTypes ::
  InferState ->
  StructuredConstructor ->
  ExpressionType ->
  Maybe [ExpressionType]
concreteConstructorFieldTypes state constructor resultExpressionType = do
  concreteArguments <-
    case resolveType state resultExpressionType of
      TDataType dataName arguments
        | dataName == structuredConstructorDataSourceName constructor -> Just arguments
      _ -> Nothing
  guard (length concreteArguments == length (structuredConstructorParameters constructor))
  let parameterVariables =
        Map.fromList
          [ (negate index - 1, resolveType state argument)
          | (index, argument) <- zip [0 :: Int ..] concreteArguments
          ]
  traverse
    (substituteConstructorExpressionType parameterVariables . resolveType state)
    (structuredConstructorFieldTemplates constructor)

expressionContract ::
  Map Name StructuredDataSkeleton ->
  Map Int TypedTypeParameterId ->
  InferState ->
  ExpressionType ->
  Maybe (TypedType, TypedRepresentationRecipe)
expressionContract dataSkeletons parameterVariables state expressionType =
  case resolveType state expressionType of
    TIntType -> scalar TypedIntType (TypedSignedIntegerRecipe 64)
    TIntegerLiteralType {} -> scalar TypedIntType (TypedSignedIntegerRecipe 64)
    TFloatType -> scalar TypedFloatType (TypedFloatRecipe 64)
    TNumericType numericType -> numericContract numericType
    TBoolType -> scalar TypedBoolType TypedBoolRecipe
    TCharType -> scalar TypedCharType TypedCharRecipe
    TTextType -> scalar TypedTextType TypedManagedTextRecipe
    TListType {} -> Nothing
    TTupleType elementTypes -> do
      elementContracts <- traverse child elementTypes
      pure
        ( TypedTupleType (map fst elementContracts),
          case elementContracts of
            [] -> TypedUnitRecipe
            _ -> TypedManagedProductRecipe (map snd elementContracts)
        )
    TDataType sourceName arguments -> do
      skeleton <- Map.lookup sourceName dataSkeletons
      argumentContracts <- traverse child arguments
      let typedArguments = map fst argumentContracts
      pure
        ( TypedDataType (skeletonName skeleton) typedArguments,
          TypedManagedVariantRecipe (skeletonName skeleton) typedArguments
        )
    TFunctionType argument result -> do
      (argumentType, argumentRecipe) <- child argument
      (resultType, resultRecipe) <- child result
      pure
        ( TypedFunctionType argumentType resultType,
          TypedClosureRecipe [argumentRecipe] resultRecipe
        )
    TVarType variable -> do
      parameter <- Map.lookup variable parameterVariables
      pure
        ( TypedTypeParameterType parameter,
          TypedRepresentationParameterRecipe parameter
        )
  where
    child = expressionContract dataSkeletons parameterVariables state
    scalar typeValue recipe = Just (typeValue, recipe)

numericContract :: NumericType -> Maybe (TypedType, TypedRepresentationRecipe)
numericContract numericType =
  case numericType of
    NumericInt8 -> numeric TypedInt8Type (TypedSignedIntegerRecipe 8)
    NumericInt16 -> numeric TypedInt16Type (TypedSignedIntegerRecipe 16)
    NumericInt32 -> numeric TypedInt32Type (TypedSignedIntegerRecipe 32)
    NumericInt64 -> numeric TypedInt64Type (TypedSignedIntegerRecipe 64)
    NumericUInt8 -> numeric TypedUInt8Type (TypedUnsignedIntegerRecipe 8)
    NumericUInt16 -> numeric TypedUInt16Type (TypedUnsignedIntegerRecipe 16)
    NumericUInt32 -> numeric TypedUInt32Type (TypedUnsignedIntegerRecipe 32)
    NumericUInt64 -> numeric TypedUInt64Type (TypedUnsignedIntegerRecipe 64)
    NumericFloat16 -> numeric TypedFloat16Type (TypedFloatRecipe 16)
    NumericFloat32 -> numeric TypedFloat32Type (TypedFloatRecipe 32)
    NumericFloat64 -> numeric TypedFloat64Type (TypedFloatRecipe 64)
  where
    numeric typeValue recipe = Just (TypedNumericType typeValue, recipe)

substituteConstructorExpressionType :: Map Int ExpressionType -> ExpressionType -> Maybe ExpressionType
substituteConstructorExpressionType bindings expressionType =
  case expressionType of
    TListType elementType -> TListType <$> child elementType
    TTupleType elementTypes -> TTupleType <$> traverse child elementTypes
    TDataType dataName arguments -> TDataType dataName <$> traverse child arguments
    TFunctionType argument result -> TFunctionType <$> child argument <*> child result
    TVarType variable -> Map.lookup variable bindings
    _ -> Just expressionType
  where
    child = substituteConstructorExpressionType bindings

substituteStructuredType ::
  Map TypedTypeParameterId (TypedType, TypedRepresentationRecipe) ->
  TypedType ->
  Maybe TypedType
substituteStructuredType bindings typeValue =
  case typeValue of
    TypedListType elementType -> TypedListType <$> child elementType
    TypedTupleType elementTypes -> TypedTupleType <$> traverse child elementTypes
    TypedDataType dataName arguments -> TypedDataType dataName <$> traverse child arguments
    TypedFunctionType argument result -> TypedFunctionType <$> child argument <*> child result
    TypedTypeParameterType parameter -> fst <$> Map.lookup parameter bindings
    _ -> Just typeValue
  where
    child = substituteStructuredType bindings

substituteStructuredRecipe ::
  Map TypedTypeParameterId (TypedType, TypedRepresentationRecipe) ->
  TypedRepresentationRecipe ->
  Maybe TypedRepresentationRecipe
substituteStructuredRecipe bindings recipe =
  case recipe of
    TypedManagedListRecipe elementRecipe -> TypedManagedListRecipe <$> child elementRecipe
    TypedManagedProductRecipe elementRecipes -> TypedManagedProductRecipe <$> traverse child elementRecipes
    TypedManagedVariantRecipe dataName arguments ->
      TypedManagedVariantRecipe dataName <$> traverse (substituteStructuredType bindings) arguments
    TypedClosureRecipe arguments result -> TypedClosureRecipe <$> traverse child arguments <*> child result
    TypedRepresentationParameterRecipe parameter -> snd <$> Map.lookup parameter bindings
    _ -> Just recipe
  where
    child = substituteStructuredRecipe bindings

representationRecipeForTypedType :: TypedType -> Maybe TypedRepresentationRecipe
representationRecipeForTypedType typeValue =
  case typeValue of
    TypedIntType -> Just (TypedSignedIntegerRecipe 64)
    TypedFloatType -> Just (TypedFloatRecipe 64)
    TypedNumericType numericType -> Just (typedNumericRepresentationRecipe numericType)
    TypedBoolType -> Just TypedBoolRecipe
    TypedCharType -> Just TypedCharRecipe
    TypedTextType -> Just TypedManagedTextRecipe
    TypedListType {} -> Nothing
    TypedTupleType elementTypes ->
      case elementTypes of
        [] -> Just TypedUnitRecipe
        _ -> TypedManagedProductRecipe <$> traverse representationRecipeForTypedType elementTypes
    TypedDataType dataName arguments -> Just (TypedManagedVariantRecipe dataName arguments)
    TypedFunctionType argument result ->
      TypedClosureRecipe
        <$> ((: []) <$> representationRecipeForTypedType argument)
        <*> representationRecipeForTypedType result
    TypedTypeParameterType {} -> Nothing

resolvedTypeName :: Name -> TypedCoreName
resolvedTypeName sourceName = TypedResolvedName TypedCurrentModule TypedTypeNamespace (identifierText sourceName)

resolvedConstructorName :: Name -> TypedCoreName
resolvedConstructorName sourceName = TypedResolvedName TypedCurrentModule TypedConstructorNamespace (identifierText sourceName)

typedSpan :: SourceSpan -> TypedSpan
typedSpan spanValue = TypedSpan (spanLine spanValue) (spanColumn spanValue)
