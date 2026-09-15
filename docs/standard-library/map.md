---
title: Map
description: Use a persistent ordered map with logarithmic lookup and update.
---

Import `Map` for a persistent balanced search tree. Key lookup and update require
`Comparable(k)`; construction with `empty` or `singleton` and read-only traversal
do not. Ordered views traverse keys in ascending order. The representation and
constructor are private. Lookup and update are `O(log n)`; full views and folds
are `O(n)` before callback work.

## Type

### `Map`

`Map(k, v)` associates ordered keys of type `k` with values of type `v`.

## Construction

### `empty`

```jazz jazz-signature
empty :: Map(k, v).
```

### `singleton`

```jazz jazz-signature
singleton :: k -> v -> Map(k, v).
```

### `fromList`

```jazz jazz-signature
fromList :: @{Comparable(k)}: [(k, v)] -> Map(k, v).
```

Inserts pairs from left to right. The last value for a duplicate key wins.
Construction is `O(n log n)`.

### `toList`

```jazz jazz-signature
toList :: Map(k, v) -> [(k, v)].
```

Returns pairs in ascending key order in `O(n)`.

## Size and lookup

### `size`

```jazz jazz-signature
size :: Map(k, v) -> Int.
```

### `isEmpty`

```jazz jazz-signature
isEmpty :: Map(k, v) -> Bool.
```

### `lookup`

```jazz jazz-signature
lookup :: @{Comparable(k)}: Map(k, v) -> k -> Maybe::Maybe(v).
```

Returns the associated value as `Just`, or `Nothing` when absent. Lookup is
`O(log n)`.

### `getOr`

```jazz jazz-signature
getOr :: @{Comparable(k)}: Map(k, v) -> k -> v -> v.
```

Returns the associated value, or the final fallback argument when absent.
Lookup is `O(log n)`.

### `containsKey`

```jazz jazz-signature
containsKey :: @{Comparable(k)}: Map(k, v) -> k -> Bool.
```

## Updating

### `insert`

```jazz jazz-signature
insert :: @{Comparable(k)}: Map(k, v) -> k -> v -> Map(k, v).
```

Adds a key or replaces its value.

### `replace`

```jazz jazz-signature
replace :: @{Comparable(k)}: Map(k, v) -> k -> v -> Maybe::Maybe(Map(k, v)).
```

Replaces an existing value and returns the new map as `Just`. An absent key
returns `Nothing`. This is `O(log n)`.

### `remove`

```jazz jazz-signature
remove :: @{Comparable(k)}: Map(k, v) -> k -> Map(k, v).
```

Removes a key when present. An absent key returns an equivalent map. This is
`O(log n)`.

### `update`

```jazz jazz-signature
update :: @{Comparable(k)}: Map(k, v) -> k -> (Maybe::Maybe(v) -> Maybe::Maybe(v)) -> Map(k, v).
```

Calls the function with the current value as `Just`, or `Nothing` when absent.
Returning `Nothing` removes a key; returning `Just` inserts or replaces it. This
is `O(log n)` plus callback work.

## Ordered boundaries

### `minimum`

```jazz jazz-signature
minimum :: Map(k, v) -> Maybe::Maybe((k, v)).
```

Returns the least key and its value, or `Nothing` for an empty map. This is
`O(log n)`.

### `maximum`

```jazz jazz-signature
maximum :: Map(k, v) -> Maybe::Maybe((k, v)).
```

Returns the greatest key and its value, or `Nothing` for an empty map. This is
`O(log n)`.

### `popMinimum`

```jazz jazz-signature
popMinimum :: Map(k, v) -> Maybe::Maybe(((k, v), Map(k, v))).
```

Returns the least pair and a map without it, or `Nothing` when empty. This is
`O(log n)`.

### `popMaximum`

```jazz jazz-signature
popMaximum :: Map(k, v) -> Maybe::Maybe(((k, v), Map(k, v))).
```

Returns the greatest pair and a map without it, or `Nothing` when empty. This
is `O(log n)`.

## Views and traversal

### `keys`

```jazz jazz-signature
keys :: Map(k, v) -> [k].
```

Returns keys in ascending order in `O(n)`.

### `values`

```jazz jazz-signature
values :: Map(k, v) -> [v].
```

Returns values in ascending key order in `O(n)`.

### `mapValues`

```jazz jazz-signature
mapValues :: Map(k, v) -> (v -> w) -> Map(k, w).
```

Transforms values in ascending key order while preserving keys and tree shape.
This is `O(n)` plus callback work.

### `filter`

```jazz jazz-signature
filter :: @{Comparable(k)}: Map(k, v) -> (k -> v -> Bool) -> Map(k, v).
```

Keeps pairs whose predicate returns `True`. Callbacks run in ascending key
order. Rebuilding the result is `O(n log n)` worst case.

### `foldLeft`

```jazz jazz-signature
foldLeft :: Map(k, v) -> a -> (a -> k -> v -> a) -> a.
```

Folds pairs from least to greatest key in `O(n)` plus callback work.

### `foldRight`

```jazz jazz-signature
foldRight :: Map(k, v) -> a -> (k -> v -> a -> a) -> a.
```

Folds pairs from greatest to least key in `O(n)` plus callback work.

Use [Dictionary](dictionary.md) when insertion order and `Equatable`-only keys matter
more than sorted traversal and logarithmic lookup.

## Generic methods

Importing Map supplies Mappable and Reducible for Map(key). Generic callbacks see values, preserve keys, and traverse in key order. These instances need no key ordering evidence.
