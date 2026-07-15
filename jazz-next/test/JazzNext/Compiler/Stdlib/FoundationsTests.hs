{-# LANGUAGE OverloadedStrings #-}

module JazzNext.Compiler.Stdlib.FoundationsTests
  ( foundationTests,
  )
where

import Data.Text (Text)
import qualified Data.Text as Text
import JazzNext.Compiler.Driver
  ( RunResult (..),
    runCompileErrors,
    runRuntimeErrors,
  )
import JazzNext.Compiler.Runtime.Observation
  ( RuntimeObservationReport (runtimeObservationStatistics),
    RuntimeObservationRequest (RuntimeObservationStatistics),
    RuntimeStatistics (runtimeListCellsConstructed),
  )
import JazzNext.Compiler.Stdlib.Shared
  ( runStdlibFixture,
    runStdlibSource,
    runStdlibSourceObserved,
  )
import JazzNext.TestHarness
  ( NamedTest,
    assertEqual,
    failTest,
  )

foundationTests :: [NamedTest]
foundationTests =
  [ ("list shape and slicing operations are total", fixtureTest ["Stdlib", "Foundations", "ListShape"] "stdlib/foundations/ListShape.jz" expectedListShape),
    ("list transformations and folds preserve order", fixtureTest ["Stdlib", "Foundations", "ListTransform"] "stdlib/foundations/ListTransform.jz" expectedListTransform),
    ("list operations define empty and clamped boundaries", fixtureTest ["Stdlib", "Foundations", "ListBoundaries"] "stdlib/foundations/ListBoundaries.jz" expectedListBoundaries),
    ("list equality and ordering operations are deterministic", fixtureTest ["Stdlib", "Foundations", "ListNormalize"] "stdlib/foundations/ListNormalize.jz" expectedListNormalize),
    ("list partial applications and stable sorting stay generic", fixtureTest ["Stdlib", "Foundations", "ListPartialStable"] "stdlib/foundations/ListPartialStable.jz" expectedListPartialStable),
    ("Maybe and Result helpers preserve branch semantics", fixtureTest ["Stdlib", "Foundations", "MaybeResult"] "stdlib/foundations/MaybeResult.jz" expectedMaybeResult),
    ("Maybe and Result helpers preserve alternate branches", fixtureTest ["Stdlib", "Foundations", "MaybeResultBranches"] "stdlib/foundations/MaybeResultBranches.jz" expectedMaybeResultBranches),
    ("NonEmpty keeps its head-tail invariant", fixtureTest ["Stdlib", "Foundations", "NonEmpty"] "stdlib/foundations/NonEmpty.jz" expectedNonEmpty),
    ("large Jazz-written list traversals stay stack safe", testLargeListTraversal),
    ("stable list sorting stays within its logarithmic work bound", testStableSortWorkBound)
  ]

fixtureTest :: [Text] -> FilePath -> Text -> IO ()
fixtureTest modulePath fixturePath expectedOutput = do
  result <- runStdlibFixture modulePath fixturePath
  assertSuccessfulOutput expectedOutput result

testLargeListTraversal :: IO ()
testLargeListTraversal = do
  result <-
    runStdlibSource
      ["Stdlib", "Foundations", "LargeTraversal"]
      """
      module Stdlib::Foundations::LargeTraversal {
        import List.
        build = \\(remaining, values) -> case remaining {
          | 0 -> values
          | _ -> build (remaining - 1) (listPrepend remaining values)
        }.
        values = build 50000 [].
        listLength (listMap (\\(value) -> value + 1) values).
      }
      """
  assertSuccessfulOutput "50000" result

testStableSortWorkBound :: IO ()
testStableSortWorkBound = do
  result <-
    runStdlibSourceObserved
      RuntimeObservationStatistics
      ["Stdlib", "Foundations", "SortWorkBound"]
      """
      module Stdlib::Foundations::SortWorkBound {
        import List.
        import Maybe.
        build = \\(remaining, values) -> case remaining {
          | 0 -> values
          | _ -> build (remaining - 1) (listPrepend remaining values)
        }.
        sorted = listSort (build 512 []).
        (listLength sorted, listHead sorted, listLast sorted).
      }
      """
  assertSuccessfulOutput "(512, Just(1), Just(512))" result
  case runRuntimeObservation result of
    Nothing -> failTest "sorting work-bound run did not produce runtime statistics"
    Just report ->
      let constructedCells = runtimeListCellsConstructed (runtimeObservationStatistics report)
       in if constructedCells <= 25000
            then pure ()
            else failTest ("sorting constructed too many list cells: " <> Text.pack (show constructedCells))

assertSuccessfulOutput :: Text -> RunResult -> IO ()
assertSuccessfulOutput expectedOutput result = do
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just expectedOutput) (runOutput result)

expectedListShape, expectedListTransform, expectedListBoundaries, expectedListNormalize, expectedListPartialStable, expectedMaybeResult, expectedMaybeResultBranches, expectedNonEmpty :: Text
expectedListShape = "(True, False, Nothing, Just(1), Nothing, Just([2, 3]), Nothing, Just(3), Nothing, Just([1, 2]), Nothing, Just(2), Nothing, [1, 2], [1, 2, 3], [1, 2, 3], [3], [], ([1, 2], [3]))"
expectedListTransform = "([1, 2, 3, 4], [1, 2, 3, 4], [1, 1, 1], [1, 0, 2, 0, 3], [1, 2, 0, 3, 4], [2, 4, 6], [2, 3], [10, 30], ([2], [1, 3]), 10, 10, [0, 1, 3, 6], True, True, True, Just(3), Just(2), [(1, \"a\"), (2, \"b\")], ([1, 2], [\"a\", \"b\"]), [(0, \"a\"), (1, \"b\")])"
expectedListBoundaries = "([0, 1, 2, 3], [3, 2, 1], 3, Just(1), Just([]), [], [1, 2, 3], ([], [1, 2, 3]), [1, 2, 3], [], [], [], [], [1], [], [1, 2], [], [], [], ([], []), 7, 7, [7], False, True, False, Nothing, Nothing, [], ([], []), [], [], [], [], [])"
expectedListNormalize = "([1, 2], [[1, 1], [2, 2], [1]], [[1, 3], [2, 4], [5]], Just(1), Just(3), Nothing, Nothing, [1, 1, 2, 3], [3, 2, 1, 1])"
expectedListPartialStable = "([2, 4, 6], [1, 2], True, [(1, \"b\"), (1, \"d\"), (2, \"a\"), (2, \"c\")], [\"Jazz!\", \"list!\"])"
expectedMaybeResult = "(Just(2), Just(3), 9, Just(4), Just(2), Nothing, True, True, [2], Just(4), Ok(2), Err(\"bad!\"), Ok(3), Ok(4), 9, True, True, Just(2), Just(\"bad\"), Err(\"missing\"), Ok(5))"
expectedMaybeResultBranches = "(Nothing, Nothing, 2, Just(2), Nothing, False, False, [], Nothing, Err(\"bad\"), Ok(2), Err(\"bad\"), Ok(2), 2, False, False, Nothing, Nothing)"
expectedNonEmpty = "([7], 7, Just(NonEmpty(1, [2, 3])), Nothing, [1, 2, 3], 1, [2, 3], 3, [0, 1, 2, 3], [1, 2, 3, 4], [2, 4, 6], 3, 6, 6)"
