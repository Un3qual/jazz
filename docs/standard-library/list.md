---
title: List
description: Use total list access, transformation, folds, grouping, and stable sorting.
sidebar_position: 3
---

Import `List` for operations on `[a]`. Unless noted otherwise, functions
preserve input order and return new lists without modifying their inputs.

## Shape

### `listPrepend`

```jazz jazz-signature
listPrepend :: a -> [a] -> [a].
```

Adds a value to the front of a list in `O(1)`.

### `listReverse`

```jazz jazz-signature
listReverse :: [a] -> [a].
```

Returns the values in reverse order in `O(n)`.

### `listLength`

```jazz jazz-signature
listLength :: [a] -> Int.
```

Counts the values in `O(n)`.

### `listIsEmpty`

```jazz jazz-signature
listIsEmpty :: [a] -> Bool.
```

Returns `True` only for `[]`. This is `O(1)`.

## Safe access

### `listHead`

```jazz jazz-signature
listHead :: [a] -> Maybe(a).
```

Returns `Just` containing the first value, or `Nothing` for `[]`. This is
`O(1)`.

### `listTail`

```jazz jazz-signature
listTail :: [a] -> Maybe([a]).
```

Returns every value after the head, or `Nothing` for `[]`. This is `O(1)`.

### `listLast`

```jazz jazz-signature
listLast :: [a] -> Maybe(a).
```

Returns the final value, or `Nothing` for `[]`, in `O(n)`.

### `listInit`

```jazz jazz-signature
listInit :: [a] -> Maybe([a]).
```

Returns every value except the last, or `Nothing` for `[]`, in `O(n)`.

### `listAt`

```jazz jazz-signature
listAt :: Int -> [a] -> Maybe(a).
```

Returns the zero-based value at the index. Negative and out-of-range indices
return `Nothing`. The cost is `O(min(n, index))` for a non-negative index.

## Slicing

### `listTake`

```jazz jazz-signature
listTake :: Int -> [a] -> [a].
```

Returns at most the first `count` values. Counts below zero clamp to zero. The
cost is linear in the returned prefix.

### `listDrop`

```jazz jazz-signature
listDrop :: Int -> [a] -> [a].
```

Skips at most the first `count` values. Counts below zero clamp to zero. The
cost is linear in the skipped prefix.

### `listSplitAt`

```jazz jazz-signature
listSplitAt :: Int -> [a] -> ([a], [a]).
```

Returns `(listTake count values, listDrop count values)`. Negative counts yield
an empty prefix and the original list as the suffix.

## Combining

### `listAppend`

```jazz jazz-signature
listAppend :: [a] -> [a] -> [a].
```

Returns the left list followed by the right list. The cost is `O(n)` in the
left list.

### `listConcat`

```jazz jazz-signature
listConcat :: [[a]] -> [a].
```

Flattens lists from left to right. The cost is linear in the fragment count and
the total number of produced values.

### `listRepeat`

```jazz jazz-signature
listRepeat :: Int -> a -> [a].
```

Returns `count` copies of a value. Non-positive counts return `[]`. The cost is
linear in the output length.

### `listIntersperse`

```jazz jazz-signature
listIntersperse :: a -> [a] -> [a].
```

Places the separator between adjacent values, never before the first or after
the last. This is linear in the output length.

### `listIntercalate`

```jazz jazz-signature
listIntercalate :: [a] -> [[a]] -> [a].
```

Inserts the separator list between adjacent fragments and concatenates the
result. Work is linear in fragments plus produced values.

## Transforming

### `listMap`

```jazz jazz-signature
listMap :: (a -> b) -> [a] -> [b].
```

Transforms every value from left to right and preserves order. This is `O(n)`
plus callback work.

### `listFilter`

```jazz jazz-signature
listFilter :: (a -> Bool) -> [a] -> [a].
```

Keeps values whose predicate returns `True`, preserving their order. This is
`O(n)` plus callback work.

### `listFilterMap`

```jazz jazz-signature
listFilterMap :: (a -> Maybe(b)) -> [a] -> [b].
```

Calls the transform for each value, keeping values inside `Just` and discarding
`Nothing`. Output order matches input order.

### `listPartition`

```jazz jazz-signature
listPartition :: (a -> Bool) -> [a] -> ([a], [a]).
```

Returns matching values first and rejected values second. Both lists preserve
input order. This is `O(n)` plus callback work.

## Folding

### `listFoldLeft`

```jazz jazz-signature
listFoldLeft :: (b -> a -> b) -> b -> [a] -> b.
```

Combines values from left to right, starting with the supplied accumulator.
This is `O(n)` plus callback work.

### `listFoldRight`

```jazz jazz-signature
listFoldRight :: (a -> b -> b) -> b -> [a] -> b.
```

Combines values from right to left, starting with the supplied terminal value.
This is `O(n)` plus callback work.

### `listScanLeft`

```jazz jazz-signature
listScanLeft :: (b -> a -> b) -> b -> [a] -> [b].
```

Returns the initial accumulator followed by every successive left-fold result,
so the output has one more value than the input.

Example: `listScanLeft (\(sum, value) -> sum + value) 0 [1, 2, 3]` produces
`[0, 1, 3, 6]`.

## Search

### `listAny`

```jazz jazz-signature
listAny :: (a -> Bool) -> [a] -> Bool.
```

Returns `True` at the first matching value and short-circuits. It returns
`False` for `[]` and is `O(n)` worst case.

### `listAll`

```jazz jazz-signature
listAll :: (a -> Bool) -> [a] -> Bool.
```

Returns `False` at the first rejected value and short-circuits. It returns
`True` for `[]` and is `O(n)` worst case.

### `listContains`

```jazz jazz-signature
listContains :: @{Eq(a)}: a -> [a] -> Bool.
```

Tests equality against values from left to right and short-circuits at the
first match. This is `O(n)` worst case and requires `Eq(a)`.

### `listFind`

```jazz jazz-signature
listFind :: (a -> Bool) -> [a] -> Maybe(a).
```

Returns the first matching value as `Just`, or `Nothing` when no value matches.
The search short-circuits and is `O(n)` worst case.

### `listFindIndex`

```jazz jazz-signature
listFindIndex :: (a -> Bool) -> [a] -> Maybe(Int).
```

Returns the zero-based index of the first match, or `Nothing`. The search
short-circuits and is `O(n)` worst case.

## Pair views

### `listZip`

```jazz jazz-signature
listZip :: [a] -> [b] -> [(a, b)].
```

Pairs corresponding values and stops when either input ends. Example:
`listZip [1, 2] ["a"]` produces `[(1, "a")]`.

### `listUnzip`

```jazz jazz-signature
listUnzip :: [(a, b)] -> ([a], [b]).
```

Separates pairs into left and right lists while preserving pair order. This is
`O(n)`.

### `listIndexed`

```jazz jazz-signature
listIndexed :: [a] -> [(Int, a)].
```

Pairs values with zero-based indices in input order. This is `O(n)`.

## Normalization

### `listDistinct`

```jazz jazz-signature
listDistinct :: @{Eq(a)}: [a] -> [a].
```

Removes repeated values while preserving the first occurrence of each value.
This requires `Eq(a)` and is `O(n²)` worst case.

### `listGroup`

```jazz jazz-signature
listGroup :: @{Eq(a)}: [a] -> [[a]].
```

Groups adjacent equal values. Equal values separated by another value remain
in different groups. This is `O(n)` plus equality work.

### `listGroupBy`

```jazz jazz-signature
listGroupBy :: (a -> a -> Bool) -> [a] -> [[a]].
```

Groups runs according to adjacent comparisons. A run continues while the
predicate returns `True` for each value and the value immediately following it.
This is `O(n)` plus callback work.

## Ordering

### `listMinimum`

```jazz jazz-signature
listMinimum :: @{Ord(a)}: [a] -> Maybe(a).
```

Returns the least value, or `Nothing` for `[]`, in `O(n)`.

### `listMaximum`

```jazz jazz-signature
listMaximum :: @{Ord(a)}: [a] -> Maybe(a).
```

Returns the greatest value, or `Nothing` for `[]`, in `O(n)`.

### `listSort`

```jazz jazz-signature
listSort :: @{Ord(a)}: [a] -> [a].
```

Returns a stable ascending merge sort using `Ord(a)`. The cost is
`O(n log n)`.

### `listSortBy`

```jazz jazz-signature
listSortBy :: (a -> a -> Ordering) -> [a] -> [a].
```

Returns a stable merge sort ordered by the comparator. `LT` places the left
value first; `EQ` preserves the input order of equal values. The cost is
`O(n log n)` plus comparator work.

Functions that may not find a value return [Maybe](maybe.md).
