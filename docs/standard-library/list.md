---
title: List
description: Use total list access, transformation, folds, grouping, and stable sorting.
sidebar_position: 3
---

Import `List` for operations on `[a]`. Unless noted otherwise, functions
preserve input order and return new lists without modifying their inputs.

## Shape

### `listPrepend`

<!-- jazz-signature -->

```jazz
listPrepend :: a -> [a] -> [a].
```

Adds a value to the front of a list in `O(1)`.

### `listReverse`

<!-- jazz-signature -->

```jazz
listReverse :: [a] -> [a].
```

Returns the values in reverse order in `O(n)`.

### `listLength`

<!-- jazz-signature -->

```jazz
listLength :: [a] -> Int.
```

Counts the values in `O(n)`.

### `listIsEmpty`

<!-- jazz-signature -->

```jazz
listIsEmpty :: [a] -> Bool.
```

Returns `True` only for `[]`. This is `O(1)`.

## Safe access

### `listHead`

<!-- jazz-signature -->

```jazz
listHead :: [a] -> Maybe(a).
```

Returns `Just` containing the first value, or `Nothing` for `[]`. This is
`O(1)`.

### `listTail`

<!-- jazz-signature -->

```jazz
listTail :: [a] -> Maybe([a]).
```

Returns every value after the head, or `Nothing` for `[]`. This is `O(1)`.

### `listLast`

<!-- jazz-signature -->

```jazz
listLast :: [a] -> Maybe(a).
```

Returns the final value, or `Nothing` for `[]`, in `O(n)`.

### `listInit`

<!-- jazz-signature -->

```jazz
listInit :: [a] -> Maybe([a]).
```

Returns every value except the last, or `Nothing` for `[]`, in `O(n)`.

### `listAt`

<!-- jazz-signature -->

```jazz
listAt :: Int -> [a] -> Maybe(a).
```

Returns the zero-based value at the index. Negative and out-of-range indices
return `Nothing`. The cost is `O(min(n, index))` for a non-negative index.

Example: `listAt 1 [10, 20, 30]` produces `Just 20`.

## Slicing

### `listTake`

<!-- jazz-signature -->

```jazz
listTake :: Int -> [a] -> [a].
```

Returns at most the first `count` values. Counts below zero clamp to zero. The
cost is linear in the returned prefix.

### `listDrop`

<!-- jazz-signature -->

```jazz
listDrop :: Int -> [a] -> [a].
```

Skips at most the first `count` values. Counts below zero clamp to zero. The
cost is linear in the skipped prefix.

### `listSplitAt`

<!-- jazz-signature -->

```jazz
listSplitAt :: Int -> [a] -> ([a], [a]).
```

Returns `(listTake count values, listDrop count values)`. Negative counts yield
an empty prefix and the original list as the suffix.

## Combining

### `listAppend`

<!-- jazz-signature -->

```jazz
listAppend :: [a] -> [a] -> [a].
```

Returns the left list followed by the right list. The cost is `O(n)` in the
left list.

### `listConcat`

<!-- jazz-signature -->

```jazz
listConcat :: [[a]] -> [a].
```

Flattens lists from left to right. The cost is linear in the fragment count and
the total number of produced values.

### `listRepeat`

<!-- jazz-signature -->

```jazz
listRepeat :: Int -> a -> [a].
```

Returns `count` copies of a value. Non-positive counts return `[]`. The cost is
linear in the output length.

### `listIntersperse`

<!-- jazz-signature -->

```jazz
listIntersperse :: a -> [a] -> [a].
```

Places the separator between adjacent values, never before the first or after
the last. This is linear in the output length.

### `listIntercalate`

<!-- jazz-signature -->

```jazz
listIntercalate :: [a] -> [[a]] -> [a].
```

Inserts the separator list between adjacent fragments and concatenates the
result. Work is linear in fragments plus produced values.

## Transforming

### `listMap`

<!-- jazz-signature -->

```jazz
listMap :: (a -> b) -> [a] -> [b].
```

Transforms every value from left to right and preserves order. This is `O(n)`
plus callback work.

### `listFilter`

<!-- jazz-signature -->

```jazz
listFilter :: (a -> Bool) -> [a] -> [a].
```

Keeps values whose predicate returns `True`, preserving their order. This is
`O(n)` plus callback work.

### `listFilterMap`

<!-- jazz-signature -->

```jazz
listFilterMap :: (a -> Maybe(b)) -> [a] -> [b].
```

Calls the transform for each value, keeping values inside `Just` and discarding
`Nothing`. Output order matches input order.

### `listPartition`

<!-- jazz-signature -->

```jazz
listPartition :: (a -> Bool) -> [a] -> ([a], [a]).
```

Returns matching values first and rejected values second. Both lists preserve
input order. This is `O(n)` plus callback work.

## Folding

### `listFoldLeft`

<!-- jazz-signature -->

```jazz
listFoldLeft :: (b -> a -> b) -> b -> [a] -> b.
```

Combines values from left to right, starting with the supplied accumulator.
This is `O(n)` plus callback work.

### `listFoldRight`

<!-- jazz-signature -->

```jazz
listFoldRight :: (a -> b -> b) -> b -> [a] -> b.
```

Combines values from right to left, starting with the supplied terminal value.
This is `O(n)` plus callback work.

### `listScanLeft`

<!-- jazz-signature -->

```jazz
listScanLeft :: (b -> a -> b) -> b -> [a] -> [b].
```

Returns the initial accumulator followed by every successive left-fold result,
so the output has one more value than the input.

Example: `listScanLeft (\(sum, value) -> sum + value) 0 [1, 2, 3]` produces
`[0, 1, 3, 6]`.

## Search

### `listAny`

<!-- jazz-signature -->

```jazz
listAny :: (a -> Bool) -> [a] -> Bool.
```

Returns `True` at the first matching value and short-circuits. It returns
`False` for `[]` and is `O(n)` worst case.

### `listAll`

<!-- jazz-signature -->

```jazz
listAll :: (a -> Bool) -> [a] -> Bool.
```

Returns `False` at the first rejected value and short-circuits. It returns
`True` for `[]` and is `O(n)` worst case.

### `listContains`

<!-- jazz-signature -->

```jazz
listContains :: @{Eq(a)}: a -> [a] -> Bool.
```

Tests equality against values from left to right and short-circuits at the
first match. This is `O(n)` worst case and requires `Eq(a)`.

### `listFind`

<!-- jazz-signature -->

```jazz
listFind :: (a -> Bool) -> [a] -> Maybe(a).
```

Returns the first matching value as `Just`, or `Nothing` when no value matches.
The search short-circuits and is `O(n)` worst case.

### `listFindIndex`

<!-- jazz-signature -->

```jazz
listFindIndex :: (a -> Bool) -> [a] -> Maybe(Int).
```

Returns the zero-based index of the first match, or `Nothing`. The search
short-circuits and is `O(n)` worst case.

## Pair views

### `listZip`

<!-- jazz-signature -->

```jazz
listZip :: [a] -> [b] -> [(a, b)].
```

Pairs corresponding values and stops when either input ends. Example:
`listZip [1, 2] ["a"]` produces `[(1, "a")]`.

### `listUnzip`

<!-- jazz-signature -->

```jazz
listUnzip :: [(a, b)] -> ([a], [b]).
```

Separates pairs into left and right lists while preserving pair order. This is
`O(n)`.

### `listIndexed`

<!-- jazz-signature -->

```jazz
listIndexed :: [a] -> [(Int, a)].
```

Pairs values with zero-based indices in input order. This is `O(n)`.

## Normalization

### `listDistinct`

<!-- jazz-signature -->

```jazz
listDistinct :: @{Eq(a)}: [a] -> [a].
```

Removes repeated values while preserving the first occurrence of each value.
This requires `Eq(a)` and is `O(n²)` worst case.

### `listGroup`

<!-- jazz-signature -->

```jazz
listGroup :: @{Eq(a)}: [a] -> [[a]].
```

Groups adjacent equal values. Equal values separated by another value remain
in different groups. This is `O(n)` plus equality work.

### `listGroupBy`

<!-- jazz-signature -->

```jazz
listGroupBy :: (a -> a -> Bool) -> [a] -> [[a]].
```

Groups each run by comparing the run's first value with following values until
the predicate returns `False`. This is `O(n)` plus callback work.

## Ordering

### `listMinimum`

<!-- jazz-signature -->

```jazz
listMinimum :: @{Ord(a)}: [a] -> Maybe(a).
```

Returns the least value, or `Nothing` for `[]`, in `O(n)`.

### `listMaximum`

<!-- jazz-signature -->

```jazz
listMaximum :: @{Ord(a)}: [a] -> Maybe(a).
```

Returns the greatest value, or `Nothing` for `[]`, in `O(n)`.

### `listSort`

<!-- jazz-signature -->

```jazz
listSort :: @{Ord(a)}: [a] -> [a].
```

Returns a stable ascending merge sort using `Ord(a)`. The cost is
`O(n log n)`.

### `listSortBy`

<!-- jazz-signature -->

```jazz
listSortBy :: (a -> a -> Ordering) -> [a] -> [a].
```

Returns a stable merge sort ordered by the comparator. `LT` places the left
value first; `EQ` preserves the input order of equal values. The cost is
`O(n log n)` plus comparator work.

Functions that may not find a value return [Maybe](maybe.md).
