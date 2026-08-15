{-# LANGUAGE OverloadedStrings #-}

-- | Pure exhaustiveness and arm-usefulness analysis for typed Jazz patterns.
module Jazz.Compiler.PatternCoverage
  ( ConstructorInventory,
    PatternCoverageFailure (..),
    PatternCoverageSite (..),
    analyzePatternCoverage,
    constructorInventoryFromBindings,
    constructorInventoryFromBindingsWithWitnessNames,
    emptyConstructorInventory,
    renderCoveragePattern,
  )
where

import Data.List (find, nub, sort, sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Jazz.Compiler.AST
  ( CaseArm (..),
    Literal (..),
    Pattern (..),
  )
import Jazz.Compiler.FractionalLiteral (fractionalLiteralSourceParts)
import Jazz.Compiler.Name
  ( Name (..),
    NameNamespace (ConstructorNamespace),
    ResolvedNameOrigin (CurrentModule),
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

-- | Immutable constructor sources captured at a match site. The maps are
-- shared with inference state; analysis materializes only data types reachable
-- from that match's scrutinee.
data ConstructorInventory = ConstructorInventory
  { constructorInventoryWitnessNames :: Map Name Name,
    constructorInventoryDataTypes :: Map Text DataTypeBinding,
    constructorInventoryEnvironment :: TypeEnv
  }
  deriving (Eq, Show)

-- | Constructor shapes reachable from one match's resolved scrutinee type.
-- Visible shapes remain useful even when hidden constructors keep the outer
-- domain open.
newtype PreparedConstructorInventory = PreparedConstructorInventory (Map Text DataConstructorInventory)

data DataConstructorInventory = DataConstructorInventory
  { inventoryTypeParameters :: [Name],
    inventoryConstructors :: [VisibleConstructor],
    inventoryIsClosed :: Bool
  }
  deriving (Eq, Show)

data VisibleConstructor = VisibleConstructor
  { visibleConstructorName :: Name,
    -- Keep diagnostic spelling separate from the canonical matching identity.
    visibleConstructorWitnessName :: Maybe Name,
    visibleConstructorArguments :: [ConstructorArgumentType]
  }
  deriving (Eq, Show)

emptyConstructorInventory :: ConstructorInventory
emptyConstructorInventory = ConstructorInventory Map.empty Map.empty Map.empty

constructorInventoryFromBindings ::
  Map Text DataTypeBinding ->
  TypeEnv ->
  ConstructorInventory
constructorInventoryFromBindings =
  constructorInventoryFromBindingsWithWitnessNames Map.empty

constructorInventoryFromBindingsWithWitnessNames ::
  Map Name Name ->
  Map Text DataTypeBinding ->
  TypeEnv ->
  ConstructorInventory
constructorInventoryFromBindingsWithWitnessNames = ConstructorInventory

prepareConstructorInventory ::
  ConstructorInventory ->
  ExpressionType ->
  PreparedConstructorInventory
prepareConstructorInventory source expressionType =
  PreparedConstructorInventory inventories
  where
    (_, inventories) = collectExpressionType Set.empty Map.empty expressionType

    collectExpressionTypes = foldl' collectExpressionTypePair

    collectExpressionTypePair (visited, collected) nextType =
      collectExpressionType visited collected nextType

    collectExpressionType visited collected currentType =
      case currentType of
        TListType elementType ->
          collectExpressionType visited collected elementType
        TTupleType fieldTypes ->
          collectExpressionTypes (visited, collected) fieldTypes
        TDataType typeName actualTypeArguments ->
          let typeNameText = renderName typeName
              alreadyVisited = Set.member typeNameText visited
              visitedWithType = Set.insert typeNameText visited
              (visitedAfterArguments, collectedAfterArguments) =
                collectExpressionTypes
                  (visitedWithType, collected)
                  actualTypeArguments
           in if alreadyVisited
                then (visitedAfterArguments, collectedAfterArguments)
                else case Map.lookup typeNameText (constructorInventoryDataTypes source) of
                  Nothing -> (visitedAfterArguments, collectedAfterArguments)
                  Just dataTypeBinding ->
                    let preparedDataInventory = dataInventory typeNameText dataTypeBinding
                        collectedWithType =
                          Map.insert typeNameText preparedDataInventory collectedAfterArguments
                        typeArguments =
                          Map.fromList
                            [ (identifierText parameter, argument)
                            | (parameter, argument) <-
                                zip
                                  (inventoryTypeParameters preparedDataInventory)
                                  actualTypeArguments
                            ]
                        reachableFieldTypes =
                          [ instantiateArgument typeArguments argument
                          | constructor <- inventoryConstructors preparedDataInventory,
                            argument <- visibleConstructorArguments constructor
                          ]
                     in collectExpressionTypes
                          (visitedAfterArguments, collectedWithType)
                          reachableFieldTypes
        _ -> (visited, collected)

    dataInventory typeNameText (DataTypeBinding typeParameters declaredConstructors) =
      DataConstructorInventory
        { inventoryTypeParameters = typeParameters,
          inventoryConstructors = visibleConstructors,
          inventoryIsClosed = length visibleConstructors == length declaredConstructors
        }
      where
        visibleConstructors =
          sortOn
            (renderName . visibleConstructorName)
            [ VisibleConstructor
                { visibleConstructorName = constructorName,
                  visibleConstructorWitnessName =
                    accessibleWitnessName
                      constructorName
                      ( Map.findWithDefault
                          constructorName
                          constructorName
                          (constructorInventoryWitnessNames source)
                      ),
                  visibleConstructorArguments = argumentTypes
                }
            | (constructorName, argumentTypes) <-
                Map.findWithDefault [] typeNameText visibleConstructorsByType
            ]

    (visibleConstructorsByType, localConstructorNames) =
      Map.foldlWithKey' indexBinding (Map.empty, Set.empty) (constructorInventoryEnvironment source)

    indexBinding (constructorsByType, localNames) constructorName binding =
      case binding of
        ConstructorTypeBinding declaredTypeName _ argumentTypes ->
          ( Map.insertWith
              (<>)
              (renderName declaredTypeName)
              [(constructorName, argumentTypes)]
              constructorsByType,
            case constructorName of
              ResolvedName CurrentModule ConstructorNamespace member ->
                Set.insert (identifierText member) localNames
              _ -> localNames
          )
        _ -> (constructorsByType, localNames)

    accessibleWitnessName constructorName witnessName =
      case (constructorName, witnessName) of
        (ResolvedName _ ConstructorNamespace _, SourceName member)
          | Set.member (identifierText member) localConstructorNames -> Nothing
        _ -> Just witnessName

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
    preparedInventory = prepareConstructorInventory inventory expressionType

    (coveredRows, unreachableFailures) =
      foldl' analyzeArm ([], []) (zip [1 ..] arms)

    analyzeArm (previousRows, failures) (armIndex, CaseArm patternValue maybeGuard _) =
      let normalizedPattern =
            simplifyCoveragePattern
              preparedInventory
              expressionType
              (normalizePattern patternValue)
          useful =
            hasWitness
              ( usefulPatternVector
                  preparedInventory
                  [expressionType]
                  previousRows
                  [normalizedPattern]
              )
          nextFailures =
            if useful
              then failures
              else failures <> [UnreachablePatternArm armIndex]
          nextRows =
            case maybeGuard of
              Nothing -> previousRows <> [[normalizedPattern]]
              Just _ -> previousRows
       in (nextRows, nextFailures)

    exhaustivenessFailure =
      case usefulPatternVector preparedInventory [expressionType] coveredRows [CoverageWildcard] of
        Nothing -> []
        Just [missing] -> [NonExhaustivePattern (coveragePatternToPattern missing)]
        Just _ -> [NonExhaustivePattern PWildcard]

hasWitness :: Maybe value -> Bool
hasWitness = maybe False (const True)

data CoveragePattern
  = CoverageWildcard
  | CoverageConstructor CoverageConstructor [CoveragePattern]
  | CoverageOr [CoveragePattern]
  deriving (Eq, Ord, Show)

data CoverageConstructor
  = CoverageBool Bool
  | CoverageUnit
  | CoverageListNil
  | CoverageListCons
  | CoverageTuple Int
  | CoverageData Name (Maybe Name)
  | CoverageLiteral Literal
  deriving (Show)

instance Eq CoverageConstructor where
  CoverageBool left == CoverageBool right = left == right
  CoverageUnit == CoverageUnit = True
  CoverageListNil == CoverageListNil = True
  CoverageListCons == CoverageListCons = True
  CoverageTuple left == CoverageTuple right = left == right
  -- Source aliases affect witness rendering, never coverage equality.
  CoverageData left _ == CoverageData right _ = left == right
  CoverageLiteral left == CoverageLiteral right = left == right
  _ == _ = False

instance Ord CoverageConstructor where
  compare (CoverageBool left) (CoverageBool right) = compare left right
  compare CoverageUnit CoverageUnit = EQ
  compare CoverageListNil CoverageListNil = EQ
  compare CoverageListCons CoverageListCons = EQ
  compare (CoverageTuple left) (CoverageTuple right) = compare left right
  -- Source aliases affect witness rendering, never coverage ordering.
  compare (CoverageData left _) (CoverageData right _) = compare left right
  compare (CoverageLiteral left) (CoverageLiteral right) = compareCoverageLiteral left right
  compare left right = compare (coverageConstructorRank left) (coverageConstructorRank right)

coverageConstructorRank :: CoverageConstructor -> Int
coverageConstructorRank constructor =
  case constructor of
    CoverageBool {} -> 0
    CoverageUnit -> 1
    CoverageListNil -> 2
    CoverageListCons -> 3
    CoverageTuple {} -> 4
    CoverageData {} -> 5
    CoverageLiteral {} -> 6

compareCoverageLiteral :: Literal -> Literal -> Ordering
compareCoverageLiteral left right =
  case (left, right) of
    (LInt leftValue, LInt rightValue) -> compare leftValue rightValue
    (LFloat leftValue leftSource leftType, LFloat rightValue rightSource rightType) ->
      compare
        (leftValue, fractionalLiteralSourceParts leftSource, leftType)
        (rightValue, fractionalLiteralSourceParts rightSource, rightType)
    (LBool leftValue, LBool rightValue) -> compare leftValue rightValue
    (LChar leftValue, LChar rightValue) -> compare leftValue rightValue
    (LText leftValue, LText rightValue) -> compare leftValue rightValue
    _ -> compare (literalRank left) (literalRank right)

literalRank :: Literal -> Int
literalRank literal =
  case literal of
    LInt {} -> 0
    LFloat {} -> 1
    LBool {} -> 2
    LChar {} -> 3
    LText {} -> 4

data ConstructorShape = ConstructorShape
  { shapeConstructor :: CoverageConstructor,
    shapeFieldTypes :: [ExpressionType]
  }

type PatternMatrix = [[CoveragePattern]]

normalizePattern :: Pattern -> CoveragePattern
normalizePattern patternValue =
  case patternValue of
    PWildcard -> CoverageWildcard
    PVariable _ -> CoverageWildcard
    PLiteral (LBool value) -> CoverageConstructor (CoverageBool value) []
    PLiteral literal -> CoverageConstructor (CoverageLiteral literal) []
    PConstructor name fields ->
      normalizeConstructor (CoverageData name (Just name)) fields
    PList elements -> normalizeList elements
    PConsList headPattern tailPattern ->
      normalizeConstructor CoverageListCons [headPattern, tailPattern]
    PTuple [] -> CoverageConstructor CoverageUnit []
    PTuple elements -> normalizeConstructor (CoverageTuple (length elements)) elements
    PAs _ innerPattern -> normalizePattern innerPattern
    POr alternatives -> CoverageOr (map normalizePattern alternatives)

normalizeConstructor :: CoverageConstructor -> [Pattern] -> CoveragePattern
normalizeConstructor constructor fields =
  CoverageConstructor constructor (map normalizePattern fields)

normalizeList :: [Pattern] -> CoveragePattern
normalizeList elements =
  case elements of
    [] -> CoverageConstructor CoverageListNil []
    element : rest ->
      CoverageConstructor
        CoverageListCons
        [normalizePattern element, normalizeList rest]

simplifyCoveragePattern ::
  PreparedConstructorInventory ->
  ExpressionType ->
  CoveragePattern ->
  CoveragePattern
simplifyCoveragePattern inventory expressionType patternValue =
  if coveragePatternIsTotal inventory expressionType simplifiedPattern
    then CoverageWildcard
    else simplifiedPattern
  where
    simplifiedPattern =
      case patternValue of
        CoverageWildcard -> CoverageWildcard
        CoverageOr alternatives ->
          CoverageOr
            (sort (nub (map (simplifyCoveragePattern inventory expressionType) alternatives)))
        CoverageConstructor constructor fields ->
          case constructorShape inventory expressionType constructor (length fields) of
            Just shape
              | length fields == length (shapeFieldTypes shape) ->
                  CoverageConstructor
                    constructor
                    ( zipWith
                        (simplifyCoveragePattern inventory)
                        (shapeFieldTypes shape)
                        fields
                    )
            _ -> CoverageConstructor constructor fields

coveragePatternIsTotal ::
  PreparedConstructorInventory ->
  ExpressionType ->
  CoveragePattern ->
  Bool
coveragePatternIsTotal inventory expressionType patternValue =
  case patternValue of
    CoverageWildcard -> True
    CoverageOr alternatives ->
      coveragePatternsAreTotal inventory expressionType alternatives
    CoverageConstructor {} ->
      case constructorShapes inventory expressionType of
        Just [shape] -> coveragePatternCoversShape inventory shape patternValue
        _ -> False

coveragePatternsAreTotal ::
  PreparedConstructorInventory ->
  ExpressionType ->
  [CoveragePattern] ->
  Bool
coveragePatternsAreTotal inventory expressionType patterns =
  not
    ( hasWitness
        ( usefulPatternVector
            inventory
            [expressionType]
            (map (: []) patterns)
            [CoverageWildcard]
        )
    )

coveragePatternCoversShape ::
  PreparedConstructorInventory ->
  ConstructorShape ->
  CoveragePattern ->
  Bool
coveragePatternCoversShape inventory shape patternValue =
  case patternValue of
    CoverageWildcard -> True
    CoverageOr alternatives ->
      any (coveragePatternCoversShape inventory shape) alternatives
    CoverageConstructor constructor fields ->
      constructor == shapeConstructor shape
        && length fields == length (shapeFieldTypes shape)
        && and
          ( zipWith
              (coveragePatternIsTotal inventory)
              (shapeFieldTypes shape)
              fields
          )

usefulPatternVector ::
  PreparedConstructorInventory ->
  [ExpressionType] ->
  PatternMatrix ->
  [CoveragePattern] ->
  Maybe [CoveragePattern]
usefulPatternVector _ _ matrix query
  | query `elem` matrix = Nothing
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
    CoverageOr alternatives ->
      firstJust
        ( map
            ( \alternative ->
                usefulPatternVector
                  inventory
                  (expressionType : restTypes)
                  matrix
                  (alternative : restQuery)
            )
            alternatives
        )
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

constructorShapes :: PreparedConstructorInventory -> ExpressionType -> Maybe [ConstructorShape]
constructorShapes inventory expressionType = do
  domain <- constructorDomain inventory expressionType
  if domainIsClosed domain
    then Just (domainShapes domain)
    else Nothing

data ConstructorDomain = ConstructorDomain
  { domainIsClosed :: Bool,
    domainShapes :: [ConstructorShape]
  }

constructorDomain :: PreparedConstructorInventory -> ExpressionType -> Maybe ConstructorDomain
constructorDomain inventory expressionType =
  case expressionType of
    TBoolType ->
      Just
        ( ConstructorDomain
            True
            [ ConstructorShape (CoverageBool False) [],
              ConstructorShape (CoverageBool True) []
            ]
        )
    TListType elementType ->
      Just
        ( ConstructorDomain
            True
            [ ConstructorShape CoverageListNil [],
              ConstructorShape CoverageListCons [elementType, TListType elementType]
            ]
        )
    TTupleType [] -> Just (ConstructorDomain True [ConstructorShape CoverageUnit []])
    TTupleType fields ->
      Just
        ( ConstructorDomain
            True
            [ConstructorShape (CoverageTuple (length fields)) fields]
        )
    TDataType typeName actualTypeArguments ->
      dataConstructorDomain inventory typeName actualTypeArguments
    _ -> Nothing

dataConstructorDomain ::
  PreparedConstructorInventory ->
  Name ->
  [ExpressionType] ->
  Maybe ConstructorDomain
dataConstructorDomain (PreparedConstructorInventory inventories) typeName actualTypeArguments = do
  dataInventory <- Map.lookup (renderName typeName) inventories
  let typeArguments =
        Map.fromList
          [ (identifierText parameter, argument)
          | (parameter, argument) <- zip (inventoryTypeParameters dataInventory) actualTypeArguments
          ]
      shapes =
        [ ConstructorShape
            ( CoverageData
                (visibleConstructorName constructor)
                (visibleConstructorWitnessName constructor)
            )
            (map (instantiateArgument typeArguments) (visibleConstructorArguments constructor))
        | constructor <- inventoryConstructors dataInventory
        ]
  pure
    ConstructorDomain
      { domainIsClosed = inventoryIsClosed dataInventory,
        domainShapes = shapes
      }

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
  PreparedConstructorInventory ->
  ExpressionType ->
  CoverageConstructor ->
  Int ->
  Maybe ConstructorShape
constructorShape inventory expressionType constructor fallbackArity =
  case constructorDomain inventory expressionType of
    Just domain ->
      case find ((== constructor) . shapeConstructor) (domainShapes domain) of
        Just shape -> Just shape
        Nothing
          | domainIsClosed domain -> Nothing
          | otherwise -> Just fallbackShape
    Nothing -> Just fallbackShape
  where
    fallbackShape =
      ConstructorShape constructor (replicate fallbackArity unknownFieldType)

specializeMatrix :: ConstructorShape -> PatternMatrix -> PatternMatrix
specializeMatrix shape = concatMap specializeRow
  where
    specializeRow row =
      case row of
        [] -> []
        CoverageWildcard : rest ->
          [replicate (length (shapeFieldTypes shape)) CoverageWildcard <> rest]
        CoverageConstructor constructor fields : rest
          | constructor == shapeConstructor shape -> [fields <> rest]
          | otherwise -> []
        CoverageOr alternatives : rest ->
          concatMap (specializeRow . (: rest)) alternatives

defaultMatrix :: PatternMatrix -> PatternMatrix
defaultMatrix = concatMap defaultRow
  where
    defaultRow row =
      case row of
        CoverageWildcard : rest -> [rest]
        CoverageConstructor {} : _ -> []
        CoverageOr alternatives : rest ->
          concatMap (defaultRow . (: rest)) alternatives
        [] -> []

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
        CoverageOr alternatives : rest ->
          any (rowHasConstructor . (: rest)) alternatives
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
    CoverageOr alternatives -> POr (map coveragePatternToPattern alternatives)
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
        CoverageData _ maybeWitnessName ->
          case maybeWitnessName of
            Just witnessName ->
              PConstructor witnessName (map coveragePatternToPattern fields)
            Nothing -> PWildcard
        CoverageLiteral literal -> PLiteral literal

renderCoveragePattern :: Pattern -> Text
renderCoveragePattern patternValue =
  case patternValue of
    PWildcard -> "_"
    PVariable name -> renderName name
    PLiteral literal -> renderLiteral literal
    PConstructor name fields ->
      Text.unwords (renderCoverageConstructorName name : map renderCoveragePatternAtom fields)
    PList elements -> "[" <> Text.intercalate ", " (map renderCoveragePatternAtom elements) <> "]"
    PConsList headPattern tailPattern ->
      "[" <> renderCoveragePatternAtom headPattern <> " | " <> renderCoveragePatternAtom tailPattern <> "]"
    PTuple elements -> "(" <> Text.intercalate ", " (map renderCoveragePatternAtom elements) <> ")"
    PAs name innerPattern -> renderName name <> " @ " <> renderCoveragePatternAtom innerPattern
    POr alternatives -> Text.intercalate " | " (map renderCoveragePattern alternatives)

renderCoverageConstructorName :: Name -> Text
renderCoverageConstructorName name =
  case name of
    ResolvedName _ ConstructorNamespace member -> identifierText member
    _ -> renderName name

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
