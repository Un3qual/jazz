{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Pure exhaustiveness and arm-usefulness analysis for typed Jazz patterns.
module Jazz.Compiler.PatternCoverage
  ( ConstructorInventory,
    CoveragePattern,
    PatternCoverageFailure (..),
    PatternCoverageSite (..),
    analyzePatternCoverage,
    constructorInventoryFromBindings,
    constructorInventoryFromBindingsWithWitnessNames,
    emptyConstructorInventory,
    renderCoveragePattern,
  )
where

import Data.Foldable (asum)
import Data.List (find, sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Jazz.Compiler.AST
  ( CaseArm (..),
    CorePhase (Resolved),
    Literal (..),
    Pattern (..),
  )
import Jazz.Compiler.FractionalLiteral (fractionalLiteralSourceParts)
import Jazz.Compiler.Name
  ( Name (..),
    NameNamespace (ConstructorNamespace),
    ResolvedName,
    ResolvedNameOrigin (CurrentModule),
    ResolvedUserName (..),
    identifierText,
    renderName,
  )
import Jazz.Compiler.TypeInference.Types
  ( ConstructorArgumentType (..),
    DataTypeBinding (..),
    ExpressionType,
    SemanticType (..),
    TypeBinding (..),
    TypeEnv,
    instantiateConstructorFieldType,
  )

data PatternCoverageFailure
  = NonExhaustivePattern CoveragePattern
  | UnreachablePatternArm Int
  deriving (Eq, Show)

data PatternCoverageSite = PatternCoverageSite
  { patternCoverageSiteOrdinal :: Int,
    patternCoverageSiteConstructorInventory :: ConstructorInventory,
    patternCoverageSiteScrutineeType :: ExpressionType,
    patternCoverageSiteArms :: [CaseArm 'Resolved]
  }
  deriving (Eq, Show)

-- | Immutable constructor sources captured at a match site. The maps are
-- shared with inference state; analysis materializes only data types reachable
-- from that match's scrutinee.
data ConstructorInventory = ConstructorInventory
  { constructorInventoryWitnessNames :: Map ResolvedName ResolvedName,
    constructorInventoryDataTypes :: Map Text DataTypeBinding,
    constructorInventoryEnvironment :: TypeEnv
  }
  deriving (Eq, Show)

-- | Constructor shapes reachable from one match's resolved scrutinee type.
-- Visible shapes remain useful even when hidden constructors keep the outer
-- domain open.
newtype PreparedConstructorInventory = PreparedConstructorInventory (Map Text DataConstructorInventory)

data DataConstructorInventory = DataConstructorInventory
  { inventoryTypeParameters :: [ResolvedName],
    inventoryConstructors :: [VisibleConstructor],
    inventoryIsClosed :: Bool
  }
  deriving (Eq, Show)

data VisibleConstructor = VisibleConstructor
  { visibleConstructorName :: ResolvedName,
    -- Keep diagnostic spelling separate from the canonical matching identity.
    visibleConstructorWitnessName :: Maybe ResolvedName,
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
  Map ResolvedName ResolvedName ->
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
        SemanticList elementType ->
          collectExpressionType visited collected elementType
        SemanticTuple fieldTypes ->
          collectExpressionTypes (visited, collected) fieldTypes
        SemanticData typeName actualTypeArguments ->
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
              UserName (ResolvedUserName CurrentModule ConstructorNamespace member) ->
                Set.insert (identifierText member) localNames
              _ -> localNames
          )
        _ -> (constructorsByType, localNames)

    accessibleWitnessName constructorName witnessName =
      case (constructorName, witnessName) of
        ( UserName (ResolvedUserName _ ConstructorNamespace _),
          UserName (ResolvedUserName CurrentModule ConstructorNamespace member)
          )
            | Set.member (identifierText member) localConstructorNames -> Nothing
        _ -> Just witnessName

-- | Analyze one source-ordered match. Guarded arms are checked for usefulness
-- but never added to the rows that cover later arms.
analyzePatternCoverage ::
  ConstructorInventory ->
  ExpressionType ->
  [CaseArm 'Resolved] ->
  [PatternCoverageFailure]
analyzePatternCoverage inventory expressionType arms =
  reverse unreachableFailuresReversed <> exhaustivenessFailure
  where
    preparedInventory = prepareConstructorInventory inventory expressionType

    (coveredRowsReversed, unreachableFailuresReversed) =
      foldl' analyzeArm ([], []) (zip [1 ..] arms)

    analyzeArm (previousRows, failures) (armIndex, CaseArm _ patternValue maybeGuard _) =
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
              else UnreachablePatternArm armIndex : failures
          nextRows =
            case maybeGuard of
              Nothing -> [normalizedPattern] : previousRows
              Just _ -> previousRows
       in (nextRows, nextFailures)

    exhaustivenessFailure =
      case usefulPatternVector preparedInventory [expressionType] (reverse coveredRowsReversed) [CoverageWildcard] of
        Nothing -> []
        Just [missing] -> [NonExhaustivePattern missing]
        Just _ -> [NonExhaustivePattern CoverageWildcard]

hasWitness :: Maybe value -> Bool
hasWitness = isJust

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
  | CoverageData ResolvedName (Maybe ResolvedName)
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

normalizePattern :: Pattern 'Resolved -> CoveragePattern
normalizePattern patternValue =
  case patternValue of
    PWildcard _ -> CoverageWildcard
    PVariable _ _ -> CoverageWildcard
    PLiteral _ (LBool value) -> CoverageConstructor (CoverageBool value) []
    PLiteral _ literal -> CoverageConstructor (CoverageLiteral literal) []
    PConstructor _ name fields ->
      normalizeConstructor (CoverageData name (Just name)) fields
    PList _ elements -> normalizeList elements
    PConsList _ headPattern tailPattern ->
      normalizeConstructor CoverageListCons [headPattern, tailPattern]
    PTuple _ [] -> CoverageConstructor CoverageUnit []
    PTuple _ elements -> normalizeConstructor (CoverageTuple (length elements)) elements
    PAs _ _ innerPattern -> normalizePattern innerPattern
    POr _ alternatives -> CoverageOr (map normalizePattern alternatives)

normalizeConstructor :: CoverageConstructor -> [Pattern 'Resolved] -> CoveragePattern
normalizeConstructor constructor fields =
  CoverageConstructor constructor (map normalizePattern fields)

normalizeList :: [Pattern 'Resolved] -> CoveragePattern
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
            ( Set.toAscList
                (Set.fromList (map (simplifyCoveragePattern inventory expressionType) alternatives))
            )
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
      asum
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
          asum (map usefulSpecialization shapes)

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
    SemanticBool ->
      Just
        ( ConstructorDomain
            True
            [ ConstructorShape (CoverageBool False) [],
              ConstructorShape (CoverageBool True) []
            ]
        )
    SemanticList elementType ->
      Just
        ( ConstructorDomain
            True
            [ ConstructorShape CoverageListNil [],
              ConstructorShape CoverageListCons [elementType, SemanticList elementType]
            ]
        )
    SemanticTuple [] -> Just (ConstructorDomain True [ConstructorShape CoverageUnit []])
    SemanticTuple fields ->
      Just
        ( ConstructorDomain
            True
            [ConstructorShape (CoverageTuple (length fields)) fields]
        )
    SemanticData typeName actualTypeArguments ->
      dataConstructorDomain inventory typeName actualTypeArguments
    _ -> Nothing

dataConstructorDomain ::
  PreparedConstructorInventory ->
  ResolvedName ->
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
unknownFieldType = SemanticVariable (-1)

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

renderCoveragePattern :: CoveragePattern -> Text
renderCoveragePattern patternValue =
  case patternValue of
    CoverageWildcard -> "_"
    CoverageOr alternatives -> Text.intercalate " | " (map renderCoveragePattern alternatives)
    CoverageConstructor constructor fields -> renderCoverageConstructor constructor fields

renderCoverageConstructor :: CoverageConstructor -> [CoveragePattern] -> Text
renderCoverageConstructor constructor fields =
  case constructor of
    CoverageBool value -> if value then "True" else "False"
    CoverageUnit -> "()"
    CoverageListNil -> "[]"
    CoverageListCons ->
      case fields of
        [headPattern, tailPattern] ->
          "[" <> renderCoveragePatternAtom headPattern <> " | " <> renderCoveragePatternAtom tailPattern <> "]"
        _ -> "_"
    CoverageTuple _ -> "(" <> Text.intercalate ", " (map renderCoveragePatternAtom fields) <> ")"
    CoverageData name maybeWitnessName ->
      Text.unwords (renderCoverageConstructorName (maybe name id maybeWitnessName) : map renderCoveragePatternAtom fields)
    CoverageLiteral literal -> renderLiteral literal

renderCoverageConstructorName :: ResolvedName -> Text
renderCoverageConstructorName name =
  case name of
    UserName (ResolvedUserName _ ConstructorNamespace member) -> identifierText member
    _ -> renderName name

renderCoveragePatternAtom :: CoveragePattern -> Text
renderCoveragePatternAtom patternValue =
  case patternValue of
    CoverageConstructor (CoverageData _ _) (_ : _) -> grouped
    CoverageOr {} -> grouped
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
