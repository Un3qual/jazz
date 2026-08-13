---
title: Map
description: Use a persistent ordered map with logarithmic lookup and update.
---

Import `Map` for a persistent balanced search tree. Keys require `Ord(k)`, and
ordered views traverse keys in ascending order. The representation and
constructor are private.

## Type

### `Map`

`Map(k, v)` associates ordered keys of type `k` with values of type `v`.

## Construction

### `mapEmpty`

<!-- jazz-signature -->

```jazz
mapEmpty :: Map(k, v).
```

The empty map. Construction is `O(1)`.

### `mapSingleton`

<!-- jazz-signature -->

```jazz
mapSingleton :: k -> v -> Map(k, v).
```

Constructs a map with one pair in `O(1)`.

### `mapFromList`

<!-- jazz-signature -->

```jazz
mapFromList :: @{Ord(k)}: [(k, v)] -> Map(k, v).
```

Inserts pairs from left to right. The last value for a duplicate key wins.
Construction is `O(n log n)`.

### `mapToList`

<!-- jazz-signature -->

```jazz
mapToList :: Map(k, v) -> [(k, v)].
```

Returns pairs in ascending key order in `O(n)`.

## Size and lookup

### `mapSize`

<!-- jazz-signature -->

```jazz
mapSize :: Map(k, v) -> Int.
```

Returns the number of keys in `O(1)`.

### `mapIsEmpty`

<!-- jazz-signature -->

```jazz
mapIsEmpty :: Map(k, v) -> Bool.
```

Returns `True` when the map contains no keys. This is `O(1)`.

### `mapLookup`

<!-- jazz-signature -->

```jazz
mapLookup :: @{Ord(k)}: Map(k, v) -> k -> Maybe(v).
```

Returns the associated value as `Just`, or `Nothing` when absent. Lookup is
`O(log n)`.

### `mapGetOr`

<!-- jazz-signature -->

```jazz
mapGetOr :: @{Ord(k)}: Map(k, v) -> k -> v -> v.
```

Returns the associated value, or the final fallback argument when absent.
Lookup is `O(log n)`.

### `mapContainsKey`

<!-- jazz-signature -->

```jazz
mapContainsKey :: @{Ord(k)}: Map(k, v) -> k -> Bool.
```

Returns whether the key is present in `O(log n)`.

## Updating

### `mapInsert`

<!-- jazz-signature -->

```jazz
mapInsert :: @{Ord(k)}: Map(k, v) -> k -> v -> Map(k, v).
```

Adds a key or replaces its value, returning a new balanced map in `O(log n)`.

### `mapReplace`

<!-- jazz-signature -->

```jazz
mapReplace :: @{Ord(k)}: Map(k, v) -> k -> v -> Maybe(Map(k, v)).
```

Replaces an existing value and returns the new map as `Just`. An absent key
returns `Nothing`. This is `O(log n)`.

### `mapRemove`

<!-- jazz-signature -->

```jazz
mapRemove :: @{Ord(k)}: Map(k, v) -> k -> Map(k, v).
```

Removes a key when present. An absent key returns an equivalent map. This is
`O(log n)`.

### `mapUpdate`

<!-- jazz-signature -->

```jazz
mapUpdate :: @{Ord(k)}: Map(k, v) -> k -> (Maybe(v) -> Maybe(v)) -> Map(k, v).
```

Calls the function with the current value as `Just`, or `Nothing` when absent.
Returning `Nothing` removes a key; returning `Just` inserts or replaces it. This
is `O(log n)` plus callback work.

## Ordered boundaries

### `mapMinimum`

<!-- jazz-signature -->

```jazz
mapMinimum :: Map(k, v) -> Maybe((k, v)).
```

Returns the least key and its value, or `Nothing` for an empty map. This is
`O(log n)`.

### `mapMaximum`

<!-- jazz-signature -->

```jazz
mapMaximum :: Map(k, v) -> Maybe((k, v)).
```

Returns the greatest key and its value, or `Nothing` for an empty map. This is
`O(log n)`.

### `mapPopMinimum`

<!-- jazz-signature -->

```jazz
mapPopMinimum :: Map(k, v) -> Maybe(((k, v), Map(k, v))).
```

Returns the least pair and a map without it, or `Nothing` when empty. This is
`O(log n)`.

### `mapPopMaximum`

<!-- jazz-signature -->

```jazz
mapPopMaximum :: Map(k, v) -> Maybe(((k, v), Map(k, v))).
```

Returns the greatest pair and a map without it, or `Nothing` when empty. This
is `O(log n)`.

## Views and traversal

### `mapKeys`

<!-- jazz-signature -->

```jazz
mapKeys :: Map(k, v) -> [k].
```

Returns keys in ascending order in `O(n)`.

### `mapValues`

<!-- jazz-signature -->

```jazz
mapValues :: Map(k, v) -> [v].
```

Returns values in ascending key order in `O(n)`.

### `mapMapValues`

<!-- jazz-signature -->

```jazz
mapMapValues :: Map(k, v) -> (v -> w) -> Map(k, w).
```

Transforms values in ascending key order while preserving keys and tree shape.
This is `O(n)` plus callback work.

### `mapFilter`

<!-- jazz-signature -->

```jazz
mapFilter :: @{Ord(k)}: Map(k, v) -> (k -> v -> Bool) -> Map(k, v).
```

Keeps pairs whose predicate returns `True`. Callbacks run in ascending key
order. Rebuilding the result is `O(n log n)` worst case.

### `mapFoldLeft`

<!-- jazz-signature -->

```jazz
mapFoldLeft :: Map(k, v) -> a -> (a -> k -> v -> a) -> a.
```

Folds pairs from least to greatest key in `O(n)` plus callback work.

### `mapFoldRight`

<!-- jazz-signature -->

```jazz
mapFoldRight :: Map(k, v) -> a -> (k -> v -> a -> a) -> a.
```

Folds pairs from greatest to least key in `O(n)` plus callback work.

Use [Dictionary](dictionary.md) when insertion order and `Eq`-only keys matter
more than sorted traversal and logarithmic lookup.
