-- | Finalization contracts for retained products and local data declarations.
module Jazz.Compiler.TypeInference.Elaboration.StructuredValues
  ( StructuredConstructor (..),
    StructuredValueCatalog,
    buildStructuredValueCatalog,
    structuredDataStatement,
    structuredNodeInfo,
    structuredConstructorAtStatement,
  )
where

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

resolvedTypeName :: Name -> TypedCoreName
resolvedTypeName sourceName = TypedResolvedName TypedCurrentModule TypedTypeNamespace (identifierText sourceName)

resolvedConstructorName :: Name -> TypedCoreName
resolvedConstructorName sourceName = TypedResolvedName TypedCurrentModule TypedConstructorNamespace (identifierText sourceName)

typedSpan :: SourceSpan -> TypedSpan
typedSpan spanValue = TypedSpan (spanLine spanValue) (spanColumn spanValue)
