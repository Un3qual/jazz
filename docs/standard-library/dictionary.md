---
title: Dictionary
description: Use an insertion-ordered persistent dictionary with Equatable-only keys.
sidebar_position: 5
---

Import `Dictionary` for an insertion-ordered association structure whose keys
require equality but not ordering. The representation and constructor are
private; every update returns a new dictionary. Basic shape operations are
`O(1)`; key lookup and update are `O(n)` worst case.

## Type

### `Dictionary`

`Dictionary(k, v)` associates keys of type `k` with values of type `v` and
preserves the insertion position of each key.

## Construction

### `empty`

```jazz jazz-signature
empty :: Dictionary(k, v).
```

### `singleton`

```jazz jazz-signature
singleton :: k -> v -> Dictionary(k, v).
```

### `fromList`

```jazz jazz-signature
fromList :: @{Equatable(k)}: [(k, v)] -> Dictionary(k, v).
```

Inserts pairs from left to right. A duplicate key keeps its first position and
its last value. Construction is `O(n²)` worst case.

### `toList`

```jazz jazz-signature
toList :: Dictionary(k, v) -> [(k, v)].
```

Returns key-value pairs in insertion order. This is `O(1)` at the API boundary.

## Size and lookup

### `size`

```jazz jazz-signature
size :: Dictionary(k, v) -> Int.
```

### `isEmpty`

```jazz jazz-signature
isEmpty :: Dictionary(k, v) -> Bool.
```

### `lookup`

```jazz jazz-signature
lookup :: @{Equatable(k)}: Dictionary(k, v) -> k -> Maybe::Maybe(v).
```

Returns the associated value as `Just`, or `Nothing` when the key is absent.
Lookup is `O(n)` worst case.

### `getOr`

```jazz jazz-signature
getOr :: @{Equatable(k)}: Dictionary(k, v) -> k -> v -> v.
```

Returns the associated value, or the final fallback argument when the key is
absent. Lookup is `O(n)` worst case.

### `containsKey`

```jazz jazz-signature
containsKey :: @{Equatable(k)}: Dictionary(k, v) -> k -> Bool.
```

## Updating

### `insert`

```jazz jazz-signature
insert :: @{Equatable(k)}: Dictionary(k, v) -> k -> v -> Dictionary(k, v).
```

Adds a new key at the end or replaces an existing value without moving its key.

### `replace`

```jazz jazz-signature
replace :: @{Equatable(k)}: Dictionary(k, v) -> k -> v -> Maybe::Maybe(Dictionary(k, v)).
```

Replaces an existing value without moving its key. Returns `Nothing` when the
key is absent. The update is `O(n)`.

### `remove`

```jazz jazz-signature
remove :: @{Equatable(k)}: Dictionary(k, v) -> k -> Dictionary(k, v).
```

Removes the key and its value. An absent key returns an equivalent dictionary.
Remaining keys retain their order. The update is `O(n)`.

### `update`

```jazz jazz-signature
update :: @{Equatable(k)}: Dictionary(k, v) -> k -> (Maybe::Maybe(v) -> Maybe::Maybe(v)) -> Dictionary(k, v).
```

Calls the function with the current value as `Just`, or `Nothing` when absent.
Returning `Nothing` removes an existing key; returning `Just` replaces it or
appends a new key. The update is `O(n)` plus callback work.

## Views and traversal

### `keys`

```jazz jazz-signature
keys :: Dictionary(k, v) -> [k].
```

Returns keys in insertion order in `O(n)`.

### `values`

```jazz jazz-signature
values :: Dictionary(k, v) -> [v].
```

Returns values in their keys' insertion order in `O(n)`.

### `mapValues`

```jazz jazz-signature
mapValues :: Dictionary(k, v) -> (v -> w) -> Dictionary(k, w).
```

Transforms every value in insertion order while preserving keys and their
positions. This is `O(n)` plus callback work.

### `filter`

```jazz jazz-signature
filter :: Dictionary(k, v) -> (k -> v -> Bool) -> Dictionary(k, v).
```

Keeps pairs whose predicate returns `True`, preserving their relative insertion
order. This is `O(n)` plus callback work.

### `foldLeft`

```jazz jazz-signature
foldLeft :: Dictionary(k, v) -> a -> (a -> k -> v -> a) -> a.
```

Folds pairs from earliest to latest insertion, starting with the supplied
accumulator. This is `O(n)` plus callback work.

### `foldRight`

```jazz jazz-signature
foldRight :: Dictionary(k, v) -> a -> (k -> v -> a -> a) -> a.
```

Folds pairs from latest to earliest insertion, starting with the supplied
terminal value. This is `O(n)` plus callback work.

Use [Map](map.md) when ascending key order and logarithmic lookup matter more
than insertion order and `Equatable`-only keys.

## Generic methods

Importing Dictionary supplies Mappable and Reducible for Dictionary(key). Generic callbacks see values, preserve keys, and traverse in insertion order. These instances need no key equality evidence.
