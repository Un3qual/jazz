---
title: List
description: Use total list access, transformation, folds, grouping, and stable sorting.
sidebar_position: 3
---

Import `List` for operations on `[a]`. Unless noted otherwise, functions
preserve input order and return new lists without modifying their inputs.

## Shape

### `prepend`

```jazz jazz-signature
prepend :: a -> [a] -> [a].
```

Adds a value to the front of a list in `O(1)`.

### `reverse`

```jazz jazz-signature
reverse :: [a] -> [a].
```

Returns the values in reverse order in `O(n)`.

### `length`

```jazz jazz-signature
length :: [a] -> Int.
```

Counts the values in `O(n)`.

### `isEmpty`

```jazz jazz-signature
isEmpty :: [a] -> Bool.
```

Returns `True` only for `[]`. This is `O(1)`.

## Safe access

### `head`

```jazz jazz-signature
head :: [a] -> Maybe::Maybe(a).
```

Returns `Just` containing the first value, or `Nothing` for `[]`. This is
`O(1)`.

### `tail`

```jazz jazz-signature
tail :: [a] -> Maybe::Maybe([a]).
```

Returns every value after the head, or `Nothing` for `[]`. This is `O(1)`.

### `last`

```jazz jazz-signature
last :: [a] -> Maybe::Maybe(a).
```

Returns the final value, or `Nothing` for `[]`, in `O(n)`.

### `init`

```jazz jazz-signature
init :: [a] -> Maybe::Maybe([a]).
```

Returns every value except the last, or `Nothing` for `[]`, in `O(n)`.

### `at`

```jazz jazz-signature
at :: Int -> [a] -> Maybe::Maybe(a).
```

Returns the zero-based value at the index. Negative and out-of-range indices
return `Nothing`. The cost is `O(min(n, index))` for a non-negative index.

## Slicing

### `take`

```jazz jazz-signature
take :: Int -> [a] -> [a].
```

Returns at most the first `count` values. Counts below zero clamp to zero. The
cost is linear in the returned prefix.

### `drop`

```jazz jazz-signature
drop :: Int -> [a] -> [a].
```

Skips at most the first `count` values. Counts below zero clamp to zero. The
cost is linear in the skipped prefix.

### `splitAt`

```jazz jazz-signature
splitAt :: Int -> [a] -> ([a], [a]).
```

Returns `(take count values, drop count values)`. Negative counts yield
an empty prefix and the original list as the suffix.

## Combining

### `append`

```jazz jazz-signature
append :: [a] -> [a] -> [a].
```

Returns the left list followed by the right list. The cost is `O(n)` in the
left list.

### `concat`

```jazz jazz-signature
concat :: [[a]] -> [a].
```

Flattens lists from left to right. The cost is linear in the fragment count and
the total number of produced values.

### `repeat`

```jazz jazz-signature
repeat :: Int -> a -> [a].
```

Returns `count` copies of a value. Non-positive counts return `[]`. The cost is
linear in the output length.

### `intersperse`

```jazz jazz-signature
intersperse :: a -> [a] -> [a].
```

Places the separator between adjacent values, never before the first or after
the last. This is linear in the output length.

### `intercalate`

```jazz jazz-signature
intercalate :: [a] -> [[a]] -> [a].
```

Inserts the separator list between adjacent fragments and concatenates the
result. Work is linear in fragments plus produced values.

## Transforming

### `map`

```jazz jazz-signature
map :: (a -> b) -> [a] -> [b].
```

Transforms every value from left to right and preserves order. This is `O(n)`
plus callback work.

### `filter`

```jazz jazz-signature
filter :: (a -> Bool) -> [a] -> [a].
```

Keeps values whose predicate returns `True`, preserving their order. This is
`O(n)` plus callback work.

### `filterMap`

```jazz jazz-signature
filterMap :: (a -> Maybe::Maybe(b)) -> [a] -> [b].
```

Calls the transform for each value, keeping values inside `Just` and discarding
`Nothing`. Output order matches input order.

### `partition`

```jazz jazz-signature
partition :: (a -> Bool) -> [a] -> ([a], [a]).
```

Returns matching values first and rejected values second. Both lists preserve
input order. This is `O(n)` plus callback work.

## Folding

### `foldLeft`

```jazz jazz-signature
foldLeft :: (b -> a -> b) -> b -> [a] -> b.
```

Combines values from left to right, starting with the supplied accumulator.
This is `O(n)` plus callback work.

### `foldRight`

```jazz jazz-signature
foldRight :: (a -> b -> b) -> b -> [a] -> b.
```

Combines values from right to left, starting with the supplied terminal value.
This is `O(n)` plus callback work.

### `scanLeft`

```jazz jazz-signature
scanLeft :: (b -> a -> b) -> b -> [a] -> [b].
```

Returns the initial accumulator followed by every successive left-fold result,
so the output has one more value than the input.

Example: `scanLeft (\(sum, value) -> sum + value) 0 [1, 2, 3]` produces
`[0, 1, 3, 6]`.

## Search

### `any`

```jazz jazz-signature
any :: (a -> Bool) -> [a] -> Bool.
```

Returns `True` at the first matching value and short-circuits. It returns
`False` for `[]` and is `O(n)` worst case.

### `all`

```jazz jazz-signature
all :: (a -> Bool) -> [a] -> Bool.
```

Returns `False` at the first rejected value and short-circuits. It returns
`True` for `[]` and is `O(n)` worst case.

### `contains`

```jazz jazz-signature
contains :: @{Equatable(a)}: a -> [a] -> Bool.
```

Tests equality against values from left to right and short-circuits at the
first match. This is `O(n)` worst case and requires `Equatable(a)`.

### `find`

```jazz jazz-signature
find :: (a -> Bool) -> [a] -> Maybe::Maybe(a).
```

Returns the first matching value as `Just`, or `Nothing` when no value matches.
The search short-circuits and is `O(n)` worst case.

### `findIndex`

```jazz jazz-signature
findIndex :: (a -> Bool) -> [a] -> Maybe::Maybe(Int).
```

Returns the zero-based index of the first match, or `Nothing`. The search
short-circuits and is `O(n)` worst case.

## Pair views

### `zip`

```jazz jazz-signature
zip :: [a] -> [b] -> [(a, b)].
```

Pairs corresponding values and stops when either input ends. Example:
`zip [1, 2] ["a"]` produces `[(1, "a")]`.

### `unzip`

```jazz jazz-signature
unzip :: [(a, b)] -> ([a], [b]).
```

Separates pairs into left and right lists while preserving pair order. This is
`O(n)`.

### `indexed`

```jazz jazz-signature
indexed :: [a] -> [(Int, a)].
```

Pairs values with zero-based indices in input order. This is `O(n)`.

## Normalization

### `distinct`

```jazz jazz-signature
distinct :: @{Equatable(a)}: [a] -> [a].
```

Removes repeated values while preserving the first occurrence of each value.
This requires `Equatable(a)` and is `O(n²)` worst case.

### `group`

```jazz jazz-signature
group :: @{Equatable(a)}: [a] -> [[a]].
```

Groups adjacent equal values. Equal values separated by another value remain
in different groups. This is `O(n)` plus equality work.

### `groupBy`

```jazz jazz-signature
groupBy :: (a -> a -> Bool) -> [a] -> [[a]].
```

Groups runs according to adjacent comparisons. A run continues while the
predicate returns `True` for each value and the value immediately following it.
This is `O(n)` plus callback work.

## Ordering

### `minimum`

```jazz jazz-signature
minimum :: @{Comparable(a)}: [a] -> Maybe::Maybe(a).
```

Returns the least value, or `Nothing` for `[]`, in `O(n)`.

### `maximum`

```jazz jazz-signature
maximum :: @{Comparable(a)}: [a] -> Maybe::Maybe(a).
```

Returns the greatest value, or `Nothing` for `[]`, in `O(n)`.

### `sort`

```jazz jazz-signature
sort :: @{Comparable(a)}: [a] -> [a].
```

Returns a stable ascending merge sort using `Comparable(a)`. The cost is
`O(n log n)`.

### `sortBy`

```jazz jazz-signature
sortBy :: (a -> a -> Ordering) -> [a] -> [a].
```

Returns a stable merge sort ordered by the comparator. `LT` places the left
value first; `EQ` preserves the input order of equal values. The cost is
`O(n log n)` plus comparator work.

Functions that may not find a value return [Maybe](maybe.md).

## Generic methods

The Prelude supplies element-based Equatable, Mappable, Reducible, and Combinable instances for List. The specialized functions keep their documented argument order.
