{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

-- | A set with deterministic first-occurrence order.
module Jazz.Compiler.StableSet
  ( StableSet,
    stableSetDelete,
    stableSetDifference,
    stableSetEmpty,
    stableSetFromPreferred,
    stableSetFromSet,
    stableSetInsert,
    stableSetMembershipSet,
    stableSetOrderedList,
    stableSetSingleton,
  )
where

import Control.DeepSeq (NFData)
import Data.Foldable (toList)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics (Generic)

-- | The set contains exactly the values in the sequence, whose order is their
-- first occurrence. The constructor stays private so consumers cannot
-- desynchronize the two projections.
data StableSet a = StableSet !(Set a) !(Seq a)
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

instance (Ord a) => Semigroup (StableSet a) where
  left <> right = foldl' (flip stableSetInsert) left (stableSetOrderedList right)

instance (Ord a) => Monoid (StableSet a) where
  mempty = stableSetEmpty

stableSetEmpty :: StableSet a
stableSetEmpty = StableSet Set.empty Seq.empty

stableSetSingleton :: a -> StableSet a
stableSetSingleton value = StableSet (Set.singleton value) (Seq.singleton value)

stableSetInsert :: (Ord a) => a -> StableSet a -> StableSet a
stableSetInsert value stable@(StableSet members values)
  | Set.member value members = stable
  | otherwise = StableSet (Set.insert value members) (values Seq.|> value)

stableSetDelete :: (Ord a) => a -> StableSet a -> StableSet a
stableSetDelete value (StableSet members values) =
  StableSet (Set.delete value members) (Seq.filter (/= value) values)

stableSetDifference :: (Ord a) => StableSet a -> Set a -> StableSet a
stableSetDifference (StableSet members values) removed =
  StableSet
    (Set.difference members removed)
    (Seq.filter (`Set.notMember` removed) values)

stableSetFromSet :: Set a -> StableSet a
stableSetFromSet members = StableSet members (Seq.fromList (Set.toList members))

stableSetFromPreferred :: (Ord a) => [a] -> Set a -> StableSet a
stableSetFromPreferred preferred members =
  StableSet members (preferredValues Seq.>< Seq.fromList (Set.toList remainingMembers))
  where
    preferredStable =
      foldl'
        ( \stable value ->
            if Set.member value members
              then stableSetInsert value stable
              else stable
        )
        stableSetEmpty
        preferred
    preferredValues = Seq.fromList (stableSetOrderedList preferredStable)
    remainingMembers = Set.difference members (stableSetMembershipSet preferredStable)

stableSetMembershipSet :: StableSet a -> Set a
stableSetMembershipSet (StableSet members _) = members

stableSetOrderedList :: StableSet a -> [a]
stableSetOrderedList (StableSet _ values) = toList values
