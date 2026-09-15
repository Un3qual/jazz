{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Jazz.Compiler.Stdlib.OrderedCollectionsTests
  ( orderedCollectionTests,
  )
where

import Control.Monad (guard)
import qualified Data.Map.Strict as ReferenceMap
import Data.Maybe (isJust)
import qualified Data.Set as ReferenceSet
import Data.Text (Text)
import qualified Data.Text as Text
import Jazz.Compiler.Name
  ( IdentifierLike (identifierText),
    Name (..),
    ResolvedName,
    ResolvedUserName (..),
  )
import Jazz.Compiler.Runtime
  ( RuntimeValue (..),
  )
import Jazz.Compiler.Stdlib.Shared
  ( assertStdlibConstructorPrivate,
    assertSuccessfulStdlibOutput,
    runStdlibFixtureExpecting,
    runStdlibPrivateProbeValue,
    runStdlibSource,
  )
import Jazz.TestHarness
  ( NamedTest,
    assertEqual,
  )

orderedCollectionTests :: [NamedTest]
orderedCollectionTests =
  [ ("Map constructs and looks up ordered entries", runStdlibFixtureExpecting ["Stdlib", "OrderedCollections", "Map"] "stdlib/ordered-collections/Map.jz" expectedMapBasics),
    ("Map updates preserve prior versions", runStdlibFixtureExpecting ["Stdlib", "OrderedCollections", "MapPersistence"] "stdlib/ordered-collections/MapPersistence.jz" expectedMapPersistence),
    ("Map extrema expose the remaining persistent map", runStdlibFixtureExpecting ["Stdlib", "OrderedCollections", "MapExtrema"] "stdlib/ordered-collections/MapExtrema.jz" expectedMapExtrema),
    ("Map traversals and transformations preserve order", runStdlibFixtureExpecting ["Stdlib", "OrderedCollections", "MapTraversal"] "stdlib/ordered-collections/MapTraversal.jz" expectedMapTraversal),
    ("Map handles every AVL rotation and removal shape", runStdlibFixtureExpecting ["Stdlib", "OrderedCollections", "MapShapes"] "stdlib/ordered-collections/MapShapes.jz" expectedMapShapes),
    ("Set reuses ordered map behavior for set algebra", runStdlibFixtureExpecting ["Stdlib", "OrderedCollections", "Set"] "stdlib/ordered-collections/Set.jz" expectedSet),
    ("Map matches a deterministic Data.Map trace", testMapModelTrace),
    ("Set matches a deterministic Data.Set trace", testSetModelTrace),
    ("Map invariants hold after every generated trace prefix", testMapInvariants),
    ("ordered collection constructors remain private", testPrivateConstructors)
  ]

testMapModelTrace :: IO ()
testMapModelTrace = do
  result <-
    runStdlibSource
      ["Stdlib", "OrderedCollections", "MapModelTrace"]
      """
      module Stdlib::OrderedCollections::MapModelTrace {
        import Map as Map.
        import Maybe as Maybe. import Maybe (Nothing, Just).
        build = \\(remaining, map) -> case remaining {
          | 0 -> map
          | _ -> build (remaining - 1) (Map::insert map remaining (remaining * 2))
        }.
        remove = \\(remaining, map) -> case remaining {
          | 0 -> map
          | _ -> remove (remaining - 1) (Map::remove map remaining)
        }.
        base = remove 500 (build 1000 Map::empty).
        replaced = Map::insert base 750 9999.
        final = Map::update replaced 1001 (\\(current) -> Just 2002).
        (Map::size final, Map::minimum final, Map::maximum final,
         Map::lookup final 500, Map::lookup final 501, Map::lookup final 750,
         Map::foldLeft final 0 (\\(total, key, entry) -> total + key + entry)).
      }
      """
  assertSuccessfulStdlibOutput expectedMapModel result

testSetModelTrace :: IO ()
testSetModelTrace = do
  result <-
    runStdlibSource
      ["Stdlib", "OrderedCollections", "SetModelTrace"]
      """
      module Stdlib::OrderedCollections::SetModelTrace {
        import Set as Set.
        build = \\(remaining, set) -> case remaining {
          | 0 -> set
          | _ -> build (remaining - 1) (Set::insert set remaining)
        }.
        remove = \\(remaining, set) -> case remaining {
          | 0 -> set
          | _ -> remove (remaining - 1) (Set::remove set remaining)
        }.
        final = Set::insert (remove 500 (build 1000 Set::empty)) 1001.
        (Set::size final, Set::contains final 500, Set::contains final 501,
         Set::foldLeft final 0 (\\(total, element) -> total + element)).
      }
      """
  assertSuccessfulStdlibOutput expectedSetModel result

testMapInvariants :: IO ()
testMapInvariants = do
  result <-
    runStdlibPrivateProbeValue
      ["Map"]
      """
      leftLeft = fromList [(3, "c"), (2, "b"), (1, "a")].
      rightRight = fromList [(1, "a"), (2, "b"), (3, "c")].
      leftRight = fromList [(3, "c"), (1, "a"), (2, "b")].
      rightLeft = fromList [(1, "a"), (3, "c"), (2, "b")].
      build = \\(remaining, map) -> case remaining {
        | 0 -> map
        | _ -> build (remaining - 1) (insert map remaining (remaining * 2))
      }.
      insertTrace = \\(remaining, map, versions) -> case remaining {
        | 0 -> __kernel_listPrependRaw map versions
        | _ -> insertTrace (remaining - 1) (insert map remaining remaining) (__kernel_listPrependRaw map versions)
      }.
      removeTrace = \\(remaining, map, versions) -> case remaining {
        | 0 -> __kernel_listPrependRaw map versions
        | _ -> removeTrace (remaining - 1) (remove map remaining) (__kernel_listPrependRaw map versions)
      }.
      large = build 1000 empty.
      ([leftLeft, rightRight, leftRight, rightLeft],
       insertTrace 100 empty [],
       removeTrace 100 (build 100 empty) [],
       large).
      """
  assertEqual
    "private invariant probe"
    (Right (Just []))
    (fmap (fmap mapProbeInvariantFailures) result)

mapProbeInvariantFailures :: RuntimeValue -> [Text]
mapProbeInvariantFailures runtimeValue =
  case runtimeValueCore runtimeValue of
    VTuple [VList rotationMaps _, VList insertTrace _, VList removeTrace _, largeMap] ->
      invalidMaps "rotation" rotationMaps
        <> invalidMaps "insert trace" insertTrace
        <> invalidMaps "remove trace" removeTrace
        <> invalidMaps "large map" [largeMap]
        <> ["large map height is not below 20" | maybe True ((>= 20) . mapSummaryHeight) (mapSummary largeMap)]
    _ -> ["private probe returned an unexpected runtime shape"]
  where
    invalidMaps label maps =
      [ label
          <> " map "
          <> Text.pack (show index)
          <> " violates an AVL invariant"
      | (index, mapValue) <- zip [0 :: Int ..] maps,
        not (mapInvariantHolds mapValue)
      ]

mapInvariantHolds :: RuntimeValue -> Bool
mapInvariantHolds = isJust . mapSummary

data MapSummary = MapSummary
  { mapSummaryHeight :: Integer,
    mapSummarySize :: Integer,
    mapSummaryMinimum :: Maybe Integer,
    mapSummaryMaximum :: Maybe Integer
  }

mapSummary :: RuntimeValue -> Maybe MapSummary
mapSummary runtimeValue =
  case runtimeValueCore runtimeValue of
    VConstructor _ _ constructorName _ []
      | constructorHasName "MapEmpty" constructorName ->
          Just (MapSummary 0 0 Nothing Nothing)
    VConstructor _ _ constructorName _ [heightValue, sizeValue, leftMap, keyValue, _, rightMap]
      | constructorHasName "MapNode" constructorName -> do
          height <- runtimeInteger heightValue
          size <- runtimeInteger sizeValue
          key <- runtimeInteger keyValue
          leftSummary <- mapSummary leftMap
          rightSummary <- mapSummary rightMap
          guard (height == 1 + max (mapSummaryHeight leftSummary) (mapSummaryHeight rightSummary))
          guard (size == 1 + mapSummarySize leftSummary + mapSummarySize rightSummary)
          guard (abs (mapSummaryHeight leftSummary - mapSummaryHeight rightSummary) <= 1)
          guard (maybe True (< key) (mapSummaryMaximum leftSummary))
          guard (maybe True (> key) (mapSummaryMinimum rightSummary))
          pure
            MapSummary
              { mapSummaryHeight = height,
                mapSummarySize = size,
                mapSummaryMinimum = Just (maybe key id (mapSummaryMinimum leftSummary)),
                mapSummaryMaximum = Just (maybe key id (mapSummaryMaximum rightSummary))
              }
    _ -> Nothing

constructorHasName :: Text -> ResolvedName -> Bool
constructorHasName expectedName constructorName =
  case constructorName of
    UserName (ResolvedUserName _ _ identifier) -> identifierText identifier == expectedName
    BuiltinName identifier -> identifierText identifier == expectedName
    GeneratedName _ -> False

runtimeInteger :: RuntimeValue -> Maybe Integer
runtimeInteger runtimeValue =
  case runtimeValueCore runtimeValue of
    VInt value _ -> Just value
    _ -> Nothing

runtimeValueCore :: RuntimeValue -> RuntimeValue
runtimeValueCore runtimeValue =
  case runtimeValue of
    VAnnotated _ innerValue -> runtimeValueCore innerValue
    _ -> runtimeValue

testPrivateConstructors :: IO ()
testPrivateConstructors = do
  assertStdlibConstructorPrivate
    ["Stdlib", "OrderedCollections", "PrivateMap"]
    "MapNode"
    """
    module Stdlib::OrderedCollections::PrivateMap {
      import Map as Map.
      MapNode.
    }
    """
  assertStdlibConstructorPrivate
    ["Stdlib", "OrderedCollections", "PrivateSet"]
    "Set"
    """
    module Stdlib::OrderedCollections::PrivateSet {
      import Set as Set.
      Set.
    }
    """

expectedMapBasics, expectedMapPersistence, expectedMapExtrema, expectedMapTraversal, expectedMapShapes, expectedSet, expectedMapModel, expectedSetModel :: Text
expectedMapBasics = "([], 0, True, [(9, \"nine\")], [(1, \"one\"), (2, \"TWO\"), (3, \"three\")], 3, Just(\"TWO\"), Nothing, \"fallback\", True)"
expectedMapPersistence = "([(1, \"one\"), (2, \"replaced\"), (3, \"three\")], [(1, \"one\"), (2, \"TWO\"), (3, \"three\")], Just([(1, \"one\"), (2, \"two!\"), (3, \"three\")]), Nothing, [(1, \"one\"), (2, \"TWO\"), (3, \"three\")], [(1, \"one\"), (3, \"three\"), (4, \"four\"), (5, \"five\")], [(2, \"replaced\"), (3, \"THREE\"), (4, \"four\")])"
expectedMapExtrema = "(Just((1, \"one\")), Just((3, \"three\")), Just(((1, \"one\"), [(2, \"TWO\"), (3, \"three\")])), Just(((3, \"three\"), [(1, \"one\"), (2, \"TWO\")])))"
expectedMapTraversal = "([1, 2, 3], [\"one\", \"TWO\", \"three\"], [(1, \"one!\"), (2, \"TWO!\"), (3, \"three!\")], [(2, \"TWO\"), (3, \"three\")], \"123\", \"123\")"
expectedMapShapes = "([(1, \"a\"), (2, \"b\"), (3, \"c\")], [(1, \"a\"), (2, \"b\"), (3, \"c\")], [(1, \"a\"), (2, \"b\"), (3, \"c\")], [(1, \"a\"), (2, \"b\"), (3, \"c\")], [(2, \"b\"), (3, \"c\")], [(1, \"a\"), (2, \"b\"), (4, \"d\")], [(1, \"a\"), (3, \"c\"), (4, \"d\"), (5, \"e\")], [(1, \"c\")])"
expectedSet = "([], 0, True, [9], [1, 2, 3], 3, True, False, [1, 2, 3, 4], [2], [1, 3], True, False, [2, 3], [0, 1], \"123\", \"123\")"
expectedMapModel =
  let built = ReferenceMap.fromList [(key, key * 2) | key <- [1000, 999 .. 1 :: Int]]
      removed = foldl' (flip ReferenceMap.delete) built [500, 499 .. 1]
      replaced = ReferenceMap.insert 750 9999 removed
      final = ReferenceMap.insert 1001 2002 replaced
      total = sum [key + value | (key, value) <- ReferenceMap.toAscList final]
   in "(501, Just((501, 1002)), Just((1001, 2002)), Nothing, Just(1002), Just(9999), "
        <> Text.pack (show total)
        <> ")"
expectedSetModel =
  let built = ReferenceSet.fromList [1000, 999 .. 1 :: Int]
      removed = foldl' (flip ReferenceSet.delete) built [500, 499 .. 1]
      final = ReferenceSet.insert 1001 removed
   in "(501, False, True, " <> Text.pack (show (sum final)) <> ")"
