{-# LANGUAGE OverloadedStrings #-}

module JazzNext.Compiler.Stdlib.LinearCollectionsTests
  ( linearCollectionTests,
  )
where

import Data.Text (Text)
import qualified Data.Text as Text
import JazzNext.Compiler.Stdlib.Shared
  ( assertStdlibConstructorPrivate,
    assertSuccessfulStdlibOutput,
    runStdlibFixtureExpecting,
    runStdlibSource,
  )
import JazzNext.TestHarness
  ( NamedTest,
  )

linearCollectionTests :: [NamedTest]
linearCollectionTests =
  [ ("Dictionary preserves insertion order and update semantics", runStdlibFixtureExpecting ["Stdlib", "LinearCollections", "Dictionary"] "stdlib/linear-collections/Dictionary.jz" expectedDictionary),
    ("Queue preserves FIFO order and persistent versions", runStdlibFixtureExpecting ["Stdlib", "LinearCollections", "Queue"] "stdlib/linear-collections/Queue.jz" expectedQueue),
    ("Dictionary matches a deterministic Haskell association-list model", testDictionaryModelTrace),
    ("Queue handles a large deterministic sequential workload", testQueueModelTrace),
    ("linear collection constructors remain private", testPrivateConstructors)
  ]

testDictionaryModelTrace :: IO ()
testDictionaryModelTrace = do
  result <-
    runStdlibSource
      ["Stdlib", "LinearCollections", "DictionaryModelTrace"]
      """
      module Stdlib::LinearCollections::DictionaryModelTrace {
        import Dictionary.
        import Maybe.
        build = \\(remaining, dictionary) -> case remaining {
          | 0 -> dictionary
          | _ -> build (remaining - 1) (dictionaryInsert dictionary remaining (remaining * 2))
        }.
        remove = \\(remaining, dictionary) -> case remaining {
          | 0 -> dictionary
          | _ -> remove (remaining - 1) (dictionaryRemove dictionary remaining)
        }.
        dictionary = remove 150 (build 300 dictionaryEmpty).
        (dictionarySize dictionary,
         dictionaryLookup dictionary 1, dictionaryLookup dictionary 150,
         dictionaryLookup dictionary 151, dictionaryLookup dictionary 300,
         dictionaryFoldLeft dictionary 0 (\\(total, key, entry) -> total + key + entry)).
      }
      """
  assertSuccessfulStdlibOutput expectedDictionaryModel result

testQueueModelTrace :: IO ()
testQueueModelTrace = do
  result <-
    runStdlibSource
      ["Stdlib", "LinearCollections", "QueueModelTrace"]
      """
      module Stdlib::LinearCollections::QueueModelTrace {
        import Queue.
        import Maybe.
        build = \\(remaining, queue) -> case remaining {
          | 0 -> queue
          | _ -> build (remaining - 1) (queueEnqueue queue remaining)
        }.
        drain = \\(queue, count, total) -> case queueDequeue queue {
          | Nothing -> (count, total)
          | Just (item, remaining) -> drain remaining (count + 1) (total + item)
        }.
        drain (build 50000 queueEmpty) 0 0.
      }
      """
  assertSuccessfulStdlibOutput expectedQueueModel result

testPrivateConstructors :: IO ()
testPrivateConstructors = do
  assertStdlibConstructorPrivate
    ["Stdlib", "LinearCollections", "PrivateDictionary"]
    "Dictionary"
    """
    module Stdlib::LinearCollections::PrivateDictionary {
      import Dictionary.
      Dictionary.
    }
    """
  assertStdlibConstructorPrivate
    ["Stdlib", "LinearCollections", "PrivateQueue"]
    "Queue"
    """
    module Stdlib::LinearCollections::PrivateQueue {
      import Queue.
      Queue.
    }
    """

expectedDictionary, expectedQueue, expectedDictionaryModel, expectedQueueModel :: Text
expectedDictionary = "([], 0, True, [(\"only\", 7)], [(\"first\", 3), (\"second\", 2)], Just(3), Nothing, 9, True, [(\"first\", 3), (\"second\", 20)], Just([(\"first\", 3), (\"second\", 22)]), Nothing, [(\"second\", 20)], [(\"second\", 20), (\"first\", 30)], [(\"second\", 21), (\"third\", 40)], [\"first\", \"second\"], [3, 2], [(\"first\", 30), (\"second\", 20)], [(\"first\", 3)], \"firstsecond\", \"firstsecond\")"
expectedQueue = "([], 0, True, [7], [1, 2, 3], Just(\"a\"), Just((\"a\", [\"b\"])), [\"a\"], [\"a\", \"b\", \"c\"], [(1, \"x\"), (2, \"y\")], \"abc\", \"abc\")"
expectedDictionaryModel =
  let insertedKeys = [300, 299 .. 1] :: [Int]
      removedKeys = [150, 149 .. 1] :: [Int]
      entries = foldl' insertModel [] [(key, key * 2) | key <- insertedKeys]
      remaining = foldl' (flip removeModel) entries removedKeys
      total = sum [key + value | (key, value) <- remaining]
   in "("
        <> Text.pack (show (length remaining))
        <> ", Nothing, Nothing, Just(302), Just(600), "
        <> Text.pack (show total)
        <> ")"
expectedQueueModel =
  let queue = [50000, 49999 .. 1] :: [Int]
   in "(" <> Text.pack (show (length queue)) <> ", " <> Text.pack (show (sum queue)) <> ")"

insertModel :: (Eq key) => [(key, value)] -> (key, value) -> [(key, value)]
insertModel entries (key, value) =
  case break ((== key) . fst) entries of
    (prefix, []) -> prefix <> [(key, value)]
    (prefix, _ : suffix) -> prefix <> ((key, value) : suffix)

removeModel :: (Eq key) => key -> [(key, value)] -> [(key, value)]
removeModel key = filter ((/= key) . fst)
