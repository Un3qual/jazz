---
title: Set
description: Use a persistent ordered set with logarithmic membership and update.
---

Import `Set` for unique, ordered values. Values require `Ord(a)`, and views
traverse them in ascending order. The representation and constructor are
private.

## Type

### `Set`

`Set(a)` stores at most one occurrence of each value of type `a`.

## Construction

### `setEmpty`

```jazz jazz-signature
setEmpty :: Set(a).
```

The empty set. Construction is `O(1)`.

### `setSingleton`

```jazz jazz-signature
setSingleton :: a -> Set(a).
```

Constructs a one-value set in `O(1)`.

### `setFromList`

```jazz jazz-signature
setFromList :: @{Ord(a)}: [a] -> Set(a).
```

Inserts values from left to right and removes duplicates. This is
`O(n log n)`.

### `setToList`

```jazz jazz-signature
setToList :: Set(a) -> [a].
```

Returns values in ascending order in `O(n)`.

## Querying

### `setSize`

```jazz jazz-signature
setSize :: Set(a) -> Int.
```

Returns the number of distinct values in `O(1)`.

### `setIsEmpty`

```jazz jazz-signature
setIsEmpty :: Set(a) -> Bool.
```

Returns `True` when the set contains no values. This is `O(1)`.

### `setContains`

```jazz jazz-signature
setContains :: @{Ord(a)}: Set(a) -> a -> Bool.
```

Tests membership in `O(log n)`.

## Updating and combining

### `setInsert`

```jazz jazz-signature
setInsert :: @{Ord(a)}: Set(a) -> a -> Set(a).
```

Adds a value and returns a new set. Inserting a duplicate does not change the
size. This is `O(log n)`.

### `setRemove`

```jazz jazz-signature
setRemove :: @{Ord(a)}: Set(a) -> a -> Set(a).
```

Removes a value when present. An absent value returns an equivalent set. This
is `O(log n)`.

### `setUnion`

```jazz jazz-signature
setUnion :: @{Ord(a)}: Set(a) -> Set(a) -> Set(a).
```

Returns every value present in either set. The implementation is
`O(m log(n + m))` for inputs of sizes `n` and `m`.

### `setIntersection`

```jazz jazz-signature
setIntersection :: @{Ord(a)}: Set(a) -> Set(a) -> Set(a).
```

Returns values present in both sets. This is `O(n × (log n + log m))` worst
case.

### `setDifference`

```jazz jazz-signature
setDifference :: @{Ord(a)}: Set(a) -> Set(a) -> Set(a).
```

Returns values from the first set that are absent from the second. This is
`O(n × (log n + log m))` worst case.

### `setIsSubset`

```jazz jazz-signature
setIsSubset :: @{Ord(a)}: Set(a) -> Set(a) -> Bool.
```

Returns whether every value in the first set occurs in the second. After a
missing value is found, later membership lookups are skipped, but traversal of
the first set continues. This is `O(n log m)` worst case.

## Transforming and traversal

### `setFilter`

```jazz jazz-signature
setFilter :: @{Ord(a)}: Set(a) -> (a -> Bool) -> Set(a).
```

Keeps values whose predicate returns `True`. Callbacks run in ascending order;
rebuilding the result is `O(n log n)` worst case.

### `setMap`

```jazz jazz-signature
setMap :: @{Ord(b)}: Set(a) -> (a -> b) -> Set(b).
```

Transforms values in ascending input order and rebuilds ordering for `b`.
Duplicate outputs collapse to one value. This is `O(n log n)` plus callback
work.

### `setFoldLeft`

```jazz jazz-signature
setFoldLeft :: Set(a) -> b -> (b -> a -> b) -> b.
```

Folds values from least to greatest in `O(n)` plus callback work.

### `setFoldRight`

```jazz jazz-signature
setFoldRight :: Set(a) -> b -> (a -> b -> b) -> b.
```

Folds values from greatest to least in `O(n)` plus callback work.
