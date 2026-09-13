{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (unless)
import Data.Bifunctor (bimap)
import Data.Bitraversable (bitraverse)
import Data.Functor.Compose (Compose (..))
import Data.Functor.Identity (Identity (..))
import Data.List (nub, sort)
import qualified Data.Set as Set
import qualified Data.Text as Text
import Jazz.Compiler.StableSet
import Jazz.Compiler.TypeRepresentation
import Jazz.TestHarness (failTest, runTestSuite)
import System.Environment (lookupEnv)
import Test.QuickCheck
import Text.Read (readMaybe)

-- Bounded recursive generation covers every constructor. Shrinking can remove
-- a layer, remove tuple/data arguments, or shrink a nested name/variable.
newtype GeneratedType = GeneratedType (SemanticType Int Int)
  deriving (Show)

instance Arbitrary GeneratedType where
  arbitrary = GeneratedType <$> sized generateType
  shrink (GeneratedType value) = map GeneratedType (shrinkType value)

generateType :: Int -> Gen (SemanticType Int Int)
generateType size
  | size <= 0 = leaf
  | otherwise = frequency [(3, leaf), (7, nested)]
  where
    leaf =
      oneof
        [ elements [SemanticInt, SemanticFloat, SemanticBool, SemanticChar, SemanticText],
          SemanticNumeric <$> elements [minBound .. maxBound],
          SemanticVariable <$> chooseInt (-4, 4)
        ]
    child = generateType (size `div` 3)
    children = chooseInt (0, 3) >>= (`vectorOf` child)
    nested =
      oneof
        [ SemanticList <$> child,
          SemanticTuple <$> children,
          SemanticData <$> chooseInt (-4, 4) <*> children,
          SemanticFunction <$> child <*> child
        ]

shrinkType :: SemanticType Int Int -> [SemanticType Int Int]
shrinkType value = case value of
  SemanticVariable variable -> SemanticVariable <$> shrink variable
  SemanticList element -> element : map SemanticList (shrinkType element)
  SemanticTuple fields -> fields <> map SemanticTuple (shrinkList shrinkType fields)
  SemanticData name arguments ->
    arguments
      <> [SemanticData smaller arguments | smaller <- shrink name]
      <> map (SemanticData name) (shrinkList shrinkType arguments)
  SemanticFunction argument result ->
    [argument, result]
      <> [SemanticFunction smaller result | smaller <- shrinkType argument]
      <> map (SemanticFunction argument) (shrinkType result)
  _ -> []

traversalIdentity :: GeneratedType -> Property
traversalIdentity (GeneratedType value) =
  conjoin
    [ runIdentity (traverse Identity value) === value,
      runIdentity (bitraverse Identity Identity value) === value,
      bimap id id value === value
    ]

traversalComposition :: GeneratedType -> Fun Int (Maybe Int) -> Fun Int (Either Bool Int) -> Property
traversalComposition (GeneratedType value) (Fun _ first) (Fun _ second) =
  conjoin
    [ getCompose (traverse (Compose . fmap second . first) value)
        === fmap (traverse second) (traverse first value),
      getCompose (bitraverse (Compose . fmap second . first) (Compose . fmap second . first) value)
        === fmap (bitraverse second second) (bitraverse first first value)
    ]

-- The writer records both name and variable visits; this catches skipped or
-- reordered children even when an identity/composition law alone would not.
traversalOrder :: GeneratedType -> Property
traversalOrder (GeneratedType value) =
  fst (bitraverse (\name -> ([Left name], name)) (\variable -> ([Right variable], variable)) value)
    === visits value
  where
    visits semanticType = case semanticType of
      SemanticVariable variable -> [Right variable]
      SemanticData name arguments -> Left name : concatMap visits arguments
      SemanticList element -> visits element
      SemanticTuple fields -> concatMap visits fields
      SemanticFunction argument result -> visits argument <> visits result
      _ -> []

substitutionIdentity :: GeneratedType -> Property
substitutionIdentity (GeneratedType value) =
  substituteSemanticVariables SemanticVariable value === value

-- Identity/composition alone would also admit an implementation that ignores
-- substitutions. Renaming agrees with the independently derived Functor walk.
substitutionRenaming :: GeneratedType -> Fun Int Int -> Property
substitutionRenaming (GeneratedType value) (Fun _ rename) =
  substituteSemanticVariables (SemanticVariable . rename) value === fmap rename value

substitutionComposition :: GeneratedType -> Fun Int GeneratedType -> Fun Int GeneratedType -> Property
substitutionComposition (GeneratedType value) (Fun _ first) (Fun _ second) =
  substituteSemanticVariables replaceSecond (substituteSemanticVariables replaceFirst value)
    === substituteSemanticVariables (substituteSemanticVariables replaceSecond . replaceFirst) value
  where
    replaceFirst variable = let GeneratedType replacement = first variable in replacement
    replaceSecond variable = let GeneratedType replacement = second variable in replacement

-- Generate operation histories, not internal StableSet storage. The reference
-- is a list with first-occurrence semantics and no calls to StableSet helpers.
data SetOperation = Insert Int | Delete Int | Difference [Int] | Append [Int]
  deriving (Show)

instance Arbitrary SetOperation where
  arbitrary =
    oneof
      [ Insert <$> smallValue,
        Delete <$> smallValue,
        Difference <$> listOf smallValue,
        Append <$> listOf smallValue
      ]
    where
      smallValue = chooseInt (-8, 8)
  shrink operation = case operation of
    Insert value -> Insert <$> shrink value
    Delete value -> Delete <$> shrink value
    Difference values -> Difference <$> shrink values
    Append values -> Append <$> shrink values

stableSetHistory :: [SetOperation] -> Property
stableSetHistory operations = conjoin (zipWith check actualStates expectedStates)
  where
    actualStates = scanl apply stableSetEmpty operations
    expectedStates = scanl reference [] operations
    fromList = foldl (flip stableSetInsert) stableSetEmpty
    apply stable operation = case operation of
      Insert value -> stableSetInsert value stable
      Delete value -> stableSetDifference stable (Set.singleton value)
      Difference values -> stableSetDifference stable (Set.fromList values)
      Append values -> stable <> fromList values
    reference values operation = case operation of
      Insert value -> nub (values <> [value])
      Delete value -> filter (/= value) values
      Difference removed -> filter (`notElem` removed) values
      Append appended -> nub (values <> appended)
    check actual expected =
      conjoin
        [ stableSetOrderedList actual === expected,
          Set.toAscList (stableSetMembershipSet actual) === sort expected
        ]

stableSetPreferred :: [Int] -> [Int] -> Property
stableSetPreferred preferred supplied =
  conjoin
    [ stableSetOrderedList actual === expected,
      Set.toAscList (stableSetMembershipSet actual) === members,
      stableSetOrderedList (stableSetFromPreferred [] (Set.fromList supplied)) === members
    ]
  where
    members = sort (nub supplied)
    ordered = nub (filter (`elem` supplied) preferred)
    expected = ordered <> filter (`notElem` ordered) members
    actual = stableSetFromPreferred preferred (Set.fromList supplied)

checkProperty :: (Testable property) => Args -> property -> IO ()
checkProperty args propertyToCheck = do
  result <- quickCheckWithResult args propertyToCheck
  unless (isSuccess result) $
    failTest (Text.pack (output result <> replayMessage result))
  where
    replayMessage Failure {usedSeed = seed, usedSize = size} =
      "\nReplay with JAZZ_QUICKCHECK_REPLAY='" <> show (seed, size) <> "'"
    replayMessage _ = ""

main :: IO ()
main = do
  replaySetting <- lookupEnv "JAZZ_QUICKCHECK_REPLAY"
  replayValue <- case replaySetting of
    Nothing -> pure Nothing
    Just value -> case readMaybe value of
      Just parsed -> pure (Just parsed)
      Nothing -> failTest "JAZZ_QUICKCHECK_REPLAY must be a (QCGen, size) pair from a failure"
  let args = stdArgs {maxSuccess = 1000, maxSize = 40, chatty = False, replay = replayValue}
  runTestSuite
    "GeneratedInvariants"
    [ ("nested semantic traversal identity", checkProperty args traversalIdentity),
      ("nested semantic traversal composition", checkProperty args traversalComposition),
      ("nested semantic traversal visit order", checkProperty args traversalOrder),
      ("semantic substitution identity", checkProperty args substitutionIdentity),
      ("semantic substitution composition", checkProperty args substitutionComposition),
      ("semantic substitution applies variable renaming", checkProperty args substitutionRenaming),
      ("stable set operation histories preserve ordering and membership", checkProperty args stableSetHistory),
      ("stable set preferred ordering and membership", checkProperty args stableSetPreferred)
    ]
