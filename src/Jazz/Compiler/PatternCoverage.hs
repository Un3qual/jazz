{-# LANGUAGE OverloadedStrings #-}

-- | Pure exhaustiveness and arm-usefulness analysis for typed Jazz patterns.
module Jazz.Compiler.PatternCoverage
  ( ConstructorInventory,
    PatternCoverageFailure (..),
    PatternCoverageSite (..),
    analyzePatternCoverage,
    constructorInventoryFromBindings,
    emptyConstructorInventory,
    renderCoveragePattern,
  )
where

import Data.List (find, sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Jazz.Compiler.AST
  ( CaseArm (..),
    Literal (..),
    Pattern (..),
  )
import Jazz.Compiler.Name
  ( Name,
    identifierText,
    renderName,
  )
import Jazz.Compiler.TypeInference.Types
  ( ConstructorArgumentType (..),
    DataTypeBinding (..),
    ExpressionType (..),
    TypeBinding (..),
    TypeEnv,
    instantiateConstructorFieldType,
  )

data PatternCoverageFailure
  = NonExhaustivePattern Pattern
  | UnreachablePatternArm Int
  deriving (Eq, Show)

data PatternCoverageSite = PatternCoverageSite
  { patternCoverageSiteOrdinal :: Int,
    patternCoverageSiteConstructorInventory :: ConstructorInventory,
    patternCoverageSiteScrutineeType :: ExpressionType,
    patternCoverageSiteArms :: [CaseArm]
  }
  deriving (Eq, Show)

-- | Constructor information that is safe to treat as a closed domain. A data
-- type remains open unless every declared constructor is visible in the
-- lexical type environment.
newtype ConstructorInventory = ConstructorInventory (Map Text DataConstructorInventory)
  deriving (Eq, Show)

data DataConstructorInventory = DataConstructorInventory
  { inventoryTypeParameters :: [Name],
    inventoryConstructors :: [VisibleConstructor]
  }
  deriving (Eq, Show)

data VisibleConstructor = VisibleConstructor
  { visibleConstructorName :: Name,
    visibleConstructorArguments :: [ConstructorArgumentType]
  }
  deriving (Eq, Show)

emptyConstructorInventory :: ConstructorInventory
emptyConstructorInventory = ConstructorInventory Map.empty

constructorInventoryFromBindings ::
  Map Text DataTypeBinding ->
  TypeEnv ->
  ConstructorInventory
constructorInventoryFromBindings dataTypes env =
  ConstructorInventory (Map.mapMaybeWithKey closedInventory dataTypes)
  where
    closedInventory typeNameText (DataTypeBinding typeParameters declaredConstructors)
      | length visibleConstructors == length declaredConstructors =
          Just
            DataConstructorInventory
              { inventoryTypeParameters = typeParameters,
                inventoryConstructors = visibleConstructors
              }
      | otherwise = Nothing
      where
        visibleConstructors =
          sortOn
            (renderName . visibleConstructorName)
            (mapMaybe (visibleConstructor typeNameText) (Map.toList env))

    visibleConstructor typeNameText (constructorName, binding) =
      case binding of
        ConstructorTypeBinding declaredTypeName _ argumentTypes
          | renderName declaredTypeName == typeNameText ->
              Just
                VisibleConstructor
                  { visibleConstructorName = constructorName,
                    visibleConstructorArguments = argumentTypes
                  }
        _ -> Nothing

-- | Analyze one source-ordered match. Guarded arms are checked for usefulness
-- but never added to the rows that cover later arms.
analyzePatternCoverage ::
  ConstructorInventory ->
  ExpressionType ->
  [CaseArm] ->
  [PatternCoverageFailure]
analyzePatternCoverage inventory expressionType arms =
  unreachableFailures <> exhaustivenessFailure
  where
    (coveredRows, unreachableFailures) =
      foldl' analyzeArm ([], []) (zip [1 ..] arms)

    analyzeArm (previousRows, failures) (armIndex, CaseArm patternValue maybeGuard _) =
      let alternatives = normalizePattern patternValue
          useful =
            any
              ( hasWitness
                  . usefulPatternVector inventory [expressionType] previousRows
                  . pure
              )
              alternatives
          nextFailures =
            if useful
              then failures
              else failures <> [UnreachablePatternArm armIndex]
          nextRows =
            case maybeGuard of
              Nothing -> previousRows <> map pure alternatives
              Just _ -> previousRows
       in (nextRows, nextFailures)

    exhaustivenessFailure =
      case usefulPatternVector inventory [expressionType] coveredRows [CoverageWildcard] of
        Nothing -> []
        Just [missing] -> [NonExhaustivePattern (coveragePatternToPattern missing)]
        Just _ -> [NonExhaustivePattern PWildcard]

hasWitness :: Maybe value -> Bool
hasWitness = maybe False (const True)

data CoveragePattern
  = CoverageWildcard
  | CoverageConstructor CoverageConstructor [CoveragePattern]
  deriving (Eq, Show)

data CoverageConstructor
  = CoverageBool Bool
  | CoverageUnit
  | CoverageListNil
  | CoverageListCons
  | CoverageTuple Int
  | CoverageData Name
  | CoverageLiteral Literal
  deriving (Eq, Show)

data ConstructorShape = ConstructorShape
  { shapeConstructor :: CoverageConstructor,
    shapeFieldTypes :: [ExpressionType]
  }

type PatternMatrix = [[CoveragePattern]]

normalizePattern :: Pattern -> [CoveragePattern]
normalizePattern patternValue =
  case patternValue of
    PWildcard -> [CoverageWildcard]
    PVariable _ -> [CoverageWildcard]
    PLiteral (LBool value) -> [CoverageConstructor (CoverageBool value) []]
    PLiteral literal -> [CoverageConstructor (CoverageLiteral literal) []]
    PConstructor name fields ->
      normalizeConstructor (CoverageData name) fields
    PList elements -> normalizeList elements
    PConsList headPattern tailPattern ->
      normalizeConstructor CoverageListCons [headPattern, tailPattern]
    PTuple [] -> [CoverageConstructor CoverageUnit []]
    PTuple elements -> normalizeConstructor (CoverageTuple (length elements)) elements
    PAs _ innerPattern -> normalizePattern innerPattern
    POr alternatives -> concatMap normalizePattern alternatives

normalizeConstructor :: CoverageConstructor -> [Pattern] -> [CoveragePattern]
normalizeConstructor constructor fields =
  [ CoverageConstructor constructor normalizedFields
  | normalizedFields <- sequence (map normalizePattern fields)
  ]

normalizeList :: [Pattern] -> [CoveragePattern]
normalizeList elements =
  case elements of
    [] -> [CoverageConstructor CoverageListNil []]
    element : rest ->
      [ CoverageConstructor CoverageListCons [normalizedElement, normalizedRest]
      | normalizedElement <- normalizePattern element,
        normalizedRest <- normalizeList rest
      ]

usefulPatternVector ::
  ConstructorInventory ->
  [ExpressionType] ->
  PatternMatrix ->
  [CoveragePattern] ->
  Maybe [CoveragePattern]
usefulPatternVector _ [] matrix [] =
  if null matrix then Just [] else Nothing
usefulPatternVector inventory (expressionType : restTypes) matrix (query : restQuery) =
  case query of
    CoverageConstructor constructor fields -> do
      shape <- constructorShape inventory expressionType constructor (length fields)
      witness <-
        usefulPatternVector
          inventory
          (shapeFieldTypes shape <> restTypes)
          (specializeMatrix shape matrix)
          (fields <> restQuery)
      let (fieldWitnesses, restWitnesses) = splitAt (length (shapeFieldTypes shape)) witness
      pure (CoverageConstructor constructor fieldWitnesses : restWitnesses)
    CoverageWildcard ->
      case constructorShapes inventory expressionType of
        Just shapes
          | allShapeConstructorsPresent shapes matrix ->
              firstUsefulSpecialization shapes
          | otherwise -> do
              restWitness <-
                usefulPatternVector inventory restTypes (defaultMatrix matrix) restQuery
              missingShape <- firstMissingShape shapes matrix
              pure
                ( CoverageConstructor
                    (shapeConstructor missingShape)
                    (replicate (length (shapeFieldTypes missingShape)) CoverageWildcard)
                    : restWitness
                )
        Nothing -> do
          restWitness <-
            usefulPatternVector inventory restTypes (defaultMatrix matrix) restQuery
          pure (CoverageWildcard : restWitness)
      where
        firstUsefulSpecialization shapes =
          firstJust (map usefulSpecialization shapes)

        usefulSpecialization shape = do
          witness <-
            usefulPatternVector
              inventory
              (shapeFieldTypes shape <> restTypes)
              (specializeMatrix shape matrix)
              (replicate (length (shapeFieldTypes shape)) CoverageWildcard <> restQuery)
          let (fieldWitnesses, restWitnesses) = splitAt (length (shapeFieldTypes shape)) witness
          pure
            (CoverageConstructor (shapeConstructor shape) fieldWitnesses : restWitnesses)
usefulPatternVector _ _ _ _ = Nothing

constructorShapes :: ConstructorInventory -> ExpressionType -> Maybe [ConstructorShape]
constructorShapes inventory expressionType =
  case expressionType of
    TBoolType ->
      Just
        [ ConstructorShape (CoverageBool False) [],
          ConstructorShape (CoverageBool True) []
        ]
    TListType elementType ->
      Just
        [ ConstructorShape CoverageListNil [],
          ConstructorShape CoverageListCons [elementType, TListType elementType]
        ]
    TTupleType [] -> Just [ConstructorShape CoverageUnit []]
    TTupleType fields -> Just [ConstructorShape (CoverageTuple (length fields)) fields]
    TDataType typeName actualTypeArguments ->
      dataConstructorShapes inventory typeName actualTypeArguments
    _ -> Nothing

dataConstructorShapes ::
  ConstructorInventory ->
  Name ->
  [ExpressionType] ->
  Maybe [ConstructorShape]
dataConstructorShapes (ConstructorInventory inventories) typeName actualTypeArguments = do
  dataInventory <- Map.lookup (renderName typeName) inventories
  let typeArguments =
        Map.fromList
          [ (identifierText parameter, argument)
          | (parameter, argument) <- zip (inventoryTypeParameters dataInventory) actualTypeArguments
          ]
  pure
    [ ConstructorShape
        (CoverageData (visibleConstructorName constructor))
        (map (instantiateArgument typeArguments) (visibleConstructorArguments constructor))
    | constructor <- inventoryConstructors dataInventory
    ]

instantiateArgument :: Map Text ExpressionType -> ConstructorArgumentType -> ExpressionType
instantiateArgument typeArguments argument =
  case argument of
    ConstructorArgumentMonomorphic expressionType -> expressionType
    ConstructorArgumentParameter parameter ->
      Map.findWithDefault unknownFieldType parameter typeArguments
    ConstructorArgumentStructured signatureType ->
      maybe unknownFieldType id (instantiateConstructorFieldType typeArguments signatureType)
    ConstructorArgumentFresh -> unknownFieldType

unknownFieldType :: ExpressionType
unknownFieldType = TVarType (-1)

constructorShape ::
  ConstructorInventory ->
  ExpressionType ->
  CoverageConstructor ->
  Int ->
  Maybe ConstructorShape
constructorShape inventory expressionType constructor fallbackArity =
  case constructorShapes inventory expressionType of
    Just shapes -> find ((== constructor) . shapeConstructor) shapes
    Nothing -> Just (ConstructorShape constructor (replicate fallbackArity unknownFieldType))

specializeMatrix :: ConstructorShape -> PatternMatrix -> PatternMatrix
specializeMatrix shape = mapMaybe specializeRow
  where
    specializeRow row =
      case row of
        [] -> Nothing
        CoverageWildcard : rest ->
          Just (replicate (length (shapeFieldTypes shape)) CoverageWildcard <> rest)
        CoverageConstructor constructor fields : rest
          | constructor == shapeConstructor shape -> Just (fields <> rest)
          | otherwise -> Nothing

defaultMatrix :: PatternMatrix -> PatternMatrix
defaultMatrix = mapMaybe defaultRow
  where
    defaultRow row =
      case row of
        CoverageWildcard : rest -> Just rest
        _ -> Nothing

allShapeConstructorsPresent :: [ConstructorShape] -> PatternMatrix -> Bool
allShapeConstructorsPresent shapes matrix =
  all (`constructorPresent` matrix) shapes

firstMissingShape :: [ConstructorShape] -> PatternMatrix -> Maybe ConstructorShape
firstMissingShape shapes matrix =
  find (not . (`constructorPresent` matrix)) shapes

constructorPresent :: ConstructorShape -> PatternMatrix -> Bool
constructorPresent shape = any rowHasConstructor
  where
    rowHasConstructor row =
      case row of
        CoverageConstructor constructor _ : _ -> constructor == shapeConstructor shape
        _ -> False

firstJust :: [Maybe value] -> Maybe value
firstJust values =
  case values of
    [] -> Nothing
    Just value : _ -> Just value
    Nothing : rest -> firstJust rest

coveragePatternToPattern :: CoveragePattern -> Pattern
coveragePatternToPattern coveragePattern =
  case coveragePattern of
    CoverageWildcard -> PWildcard
    CoverageConstructor constructor fields ->
      case constructor of
        CoverageBool value -> PLiteral (LBool value)
        CoverageUnit -> PTuple []
        CoverageListNil -> PList []
        CoverageListCons ->
          case fields of
            [headPattern, tailPattern] ->
              PConsList
                (coveragePatternToPattern headPattern)
                (coveragePatternToPattern tailPattern)
            _ -> PWildcard
        CoverageTuple _ -> PTuple (map coveragePatternToPattern fields)
        CoverageData name -> PConstructor name (map coveragePatternToPattern fields)
        CoverageLiteral literal -> PLiteral literal

renderCoveragePattern :: Pattern -> Text
renderCoveragePattern patternValue =
  case patternValue of
    PWildcard -> "_"
    PVariable name -> renderName name
    PLiteral literal -> renderLiteral literal
    PConstructor name fields ->
      Text.unwords (renderName name : map renderCoveragePatternAtom fields)
    PList elements -> "[" <> Text.intercalate ", " (map renderCoveragePatternAtom elements) <> "]"
    PConsList headPattern tailPattern ->
      "[" <> renderCoveragePatternAtom headPattern <> " | " <> renderCoveragePatternAtom tailPattern <> "]"
    PTuple elements -> "(" <> Text.intercalate ", " (map renderCoveragePatternAtom elements) <> ")"
    PAs name innerPattern -> renderName name <> " @ " <> renderCoveragePatternAtom innerPattern
    POr alternatives -> Text.intercalate " | " (map renderCoveragePattern alternatives)

renderCoveragePatternAtom :: Pattern -> Text
renderCoveragePatternAtom patternValue =
  case patternValue of
    PConstructor _ (_ : _) -> grouped
    PAs {} -> grouped
    POr {} -> grouped
    _ -> renderCoveragePattern patternValue
  where
    grouped = "(" <> renderCoveragePattern patternValue <> ")"

renderLiteral :: Literal -> Text
renderLiteral literal =
  case literal of
    LInt value -> Text.pack (show value)
    LFloat value _ _ -> Text.pack (show value)
    LBool value -> if value then "True" else "False"
    LChar value -> Text.pack (show value)
    LText value -> Text.pack (show value)
