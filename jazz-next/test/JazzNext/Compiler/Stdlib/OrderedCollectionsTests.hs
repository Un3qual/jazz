{-# LANGUAGE OverloadedStrings #-}

module JazzNext.Compiler.Stdlib.OrderedCollectionsTests
  ( orderedCollectionTests,
  )
where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Map.Strict as ReferenceMap
import qualified Data.Set as ReferenceSet
import JazzNext.Compiler.Diagnostics.Render
  ( renderDiagnostic,
  )
import JazzNext.Compiler.Driver
  ( RunResult (..),
    runCompileErrors,
    runRuntimeErrors,
  )
import JazzNext.Compiler.Stdlib.Shared
  ( runStdlibFixture,
    runStdlibPrivateProbe,
    runStdlibSource,
  )
import JazzNext.TestHarness
  ( NamedTest,
    assertContains,
    assertEqual,
    failTest,
  )

orderedCollectionTests :: [NamedTest]
orderedCollectionTests =
  [ ("Map preserves ordered persistent behavior", fixtureTest ["Stdlib", "OrderedCollections", "Map"] "stdlib/ordered-collections/Map.jz" expectedMap),
    ("Map handles every AVL rotation and removal shape", fixtureTest ["Stdlib", "OrderedCollections", "MapShapes"] "stdlib/ordered-collections/MapShapes.jz" expectedMapShapes),
    ("Set reuses ordered map behavior for set algebra", fixtureTest ["Stdlib", "OrderedCollections", "Set"] "stdlib/ordered-collections/Set.jz" expectedSet),
    ("Map matches a deterministic Data.Map trace", testMapModelTrace),
    ("Set matches a deterministic Data.Set trace", testSetModelTrace),
    ("Map invariants hold after every generated trace prefix", testMapInvariants),
    ("ordered collection constructors remain private", testPrivateConstructors)
  ]

fixtureTest :: [Text] -> FilePath -> Text -> IO ()
fixtureTest modulePath fixturePath expectedOutput = do
  result <- runStdlibFixture modulePath fixturePath
  assertSuccessfulOutput expectedOutput result

testMapModelTrace :: IO ()
testMapModelTrace = do
  result <-
    runStdlibSource
      ["Stdlib", "OrderedCollections", "MapModelTrace"]
      """
      module Stdlib::OrderedCollections::MapModelTrace {
        import Map.
        import Maybe.
        build = \\(remaining, map) -> case remaining {
          | 0 -> map
          | _ -> build (remaining - 1) (mapInsert map remaining (remaining * 2))
        }.
        remove = \\(remaining, map) -> case remaining {
          | 0 -> map
          | _ -> remove (remaining - 1) (mapRemove map remaining)
        }.
        base = remove 500 (build 1000 mapEmpty).
        replaced = mapInsert base 750 9999.
        final = mapUpdate replaced 1001 (\\(current) -> Just 2002).
        (mapSize final, mapMinimum final, mapMaximum final,
         mapLookup final 500, mapLookup final 501, mapLookup final 750,
         mapFoldLeft final 0 (\\(total, key, value) -> total + key + value)).
      }
      """
  assertSuccessfulOutput expectedMapModel result

testSetModelTrace :: IO ()
testSetModelTrace = do
  result <-
    runStdlibSource
      ["Stdlib", "OrderedCollections", "SetModelTrace"]
      """
      module Stdlib::OrderedCollections::SetModelTrace {
        import Set.
        build = \\(remaining, set) -> case remaining {
          | 0 -> set
          | _ -> build (remaining - 1) (setInsert set remaining)
        }.
        remove = \\(remaining, set) -> case remaining {
          | 0 -> set
          | _ -> remove (remaining - 1) (setRemove set remaining)
        }.
        final = setInsert (remove 500 (build 1000 setEmpty)) 1001.
        (setSize final, setContains final 500, setContains final 501,
         setFoldLeft final 0 (\\(total, value) -> total + value)).
      }
      """
  assertSuccessfulOutput expectedSetModel result

testMapInvariants :: IO ()
testMapInvariants = do
  result <-
    runStdlibPrivateProbe
      ["Map"]
      """
      leftLeft = mapFromList [(3, "c"), (2, "b"), (1, "a")].
      rightRight = mapFromList [(1, "a"), (2, "b"), (3, "c")].
      leftRight = mapFromList [(3, "c"), (1, "a"), (2, "b")].
      rightLeft = mapFromList [(1, "a"), (3, "c"), (2, "b")].
      build = \\(remaining, map) -> case remaining {
        | 0 -> map
        | _ -> build (remaining - 1) (mapInsert map remaining (remaining * 2))
      }.
      insertTraceHolds = \\(remaining, map) -> if mapInvariantHolds map then case remaining {
        | 0 -> True
        | _ -> insertTraceHolds (remaining - 1) (mapInsert map remaining remaining)
      } else False.
      removeTraceHolds = \\(remaining, map) -> if mapInvariantHolds map then case remaining {
        | 0 -> True
        | _ -> removeTraceHolds (remaining - 1) (mapRemove map remaining)
      } else False.
      large = build 1000 mapEmpty.
      (mapInvariantHolds leftLeft, mapInvariantHolds rightRight,
       mapInvariantHolds leftRight, mapInvariantHolds rightLeft,
       insertTraceHolds 100 mapEmpty,
       removeTraceHolds 100 (build 100 mapEmpty),
       mapInvariantHolds large, mapHeight large < 20).
      """
  assertEqual
    "private invariant probe"
    (Right (Just "(True, True, True, True, True, True, True, True)"))
    result

testPrivateConstructors :: IO ()
testPrivateConstructors = do
  assertConstructorPrivate
    ["Stdlib", "OrderedCollections", "PrivateMap"]
    "MapNode"
    """
    module Stdlib::OrderedCollections::PrivateMap {
      import Map.
      MapNode.
    }
    """
  assertConstructorPrivate
    ["Stdlib", "OrderedCollections", "PrivateSet"]
    "Set"
    """
    module Stdlib::OrderedCollections::PrivateSet {
      import Set.
      Set.
    }
    """

assertConstructorPrivate :: [Text] -> Text -> Text -> IO ()
assertConstructorPrivate modulePath constructorName source = do
  result <- runStdlibSource modulePath source
  case runCompileErrors result of
    [] -> failTest (constructorName <> " constructor was unexpectedly public")
    diagnostics ->
      assertContains
        (constructorName <> " private-constructor diagnostic")
        ("unbound variable '" <> constructorName <> "'")
        (Text.unlines (map renderDiagnostic diagnostics))

assertSuccessfulOutput :: Text -> RunResult -> IO ()
assertSuccessfulOutput expectedOutput result = do
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just expectedOutput) (runOutput result)

expectedMap, expectedMapShapes, expectedSet, expectedMapModel, expectedSetModel :: Text
expectedMap = "([], 0, True, [(9, \"nine\")], [(1, \"one\"), (2, \"TWO\"), (3, \"three\")], 3, Just(\"TWO\"), Nothing, \"fallback\", True, [(1, \"one\"), (2, \"replaced\"), (3, \"three\")], [(1, \"one\"), (2, \"TWO\"), (3, \"three\")], Just([(1, \"one\"), (2, \"two!\"), (3, \"three\")]), Nothing, [(1, \"one\"), (2, \"TWO\"), (3, \"three\")], [(1, \"one\"), (3, \"three\"), (4, \"four\"), (5, \"five\")], [(2, \"replaced\"), (3, \"THREE\"), (4, \"four\")], Just((1, \"one\")), Just((3, \"three\")), Just(((1, \"one\"), [(2, \"TWO\"), (3, \"three\")])), Just(((3, \"three\"), [(1, \"one\"), (2, \"TWO\")])), [1, 2, 3], [\"one\", \"TWO\", \"three\"], [(1, \"one!\"), (2, \"TWO!\"), (3, \"three!\")], [(2, \"TWO\"), (3, \"three\")], \"123\", \"123\")"
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
