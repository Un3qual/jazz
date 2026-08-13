---
title: Dictionary
description: Use an insertion-ordered persistent dictionary with Eq-only keys.
sidebar_position: 5
---

Import `Dictionary` for an insertion-ordered association structure whose keys
require equality but not ordering. The representation and constructor are
private; every update returns a new dictionary.

## Type

### `Dictionary`

`Dictionary(k, v)` associates keys of type `k` with values of type `v` and
preserves the insertion position of each key.

## Construction

### `dictionaryEmpty`

```jazz jazz-signature
dictionaryEmpty :: Dictionary(k, v).
```

The empty dictionary. Construction is `O(1)`.

### `dictionarySingleton`

```jazz jazz-signature
dictionarySingleton :: k -> v -> Dictionary(k, v).
```

Constructs a dictionary with one key-value pair in `O(1)`.

### `dictionaryFromList`

```jazz jazz-signature
dictionaryFromList :: @{Eq(k)}: [(k, v)] -> Dictionary(k, v).
```

Inserts pairs from left to right. A duplicate key keeps its first position and
its last value. Construction is `O(n²)` worst case.

### `dictionaryToList`

```jazz jazz-signature
dictionaryToList :: Dictionary(k, v) -> [(k, v)].
```

Returns key-value pairs in insertion order. This is `O(1)` at the API boundary.

## Size and lookup

### `dictionarySize`

```jazz jazz-signature
dictionarySize :: Dictionary(k, v) -> Int.
```

Returns the number of distinct keys in `O(1)`.

### `dictionaryIsEmpty`

```jazz jazz-signature
dictionaryIsEmpty :: Dictionary(k, v) -> Bool.
```

Returns `True` when the dictionary has no keys. This is `O(1)`.

### `dictionaryLookup`

```jazz jazz-signature
dictionaryLookup :: @{Eq(k)}: Dictionary(k, v) -> k -> Maybe(v).
```

Returns the associated value as `Just`, or `Nothing` when the key is absent.
Lookup is `O(n)` worst case.

### `dictionaryGetOr`

```jazz jazz-signature
dictionaryGetOr :: @{Eq(k)}: Dictionary(k, v) -> k -> v -> v.
```

Returns the associated value, or the final fallback argument when the key is
absent. Lookup is `O(n)` worst case.

### `dictionaryContainsKey`

```jazz jazz-signature
dictionaryContainsKey :: @{Eq(k)}: Dictionary(k, v) -> k -> Bool.
```

Returns whether the key is present in `O(n)` worst case.

## Updating

### `dictionaryInsert`

```jazz jazz-signature
dictionaryInsert :: @{Eq(k)}: Dictionary(k, v) -> k -> v -> Dictionary(k, v).
```

Adds a new key at the end or replaces an existing value without moving its key.
The update is `O(n)`.

### `dictionaryReplace`

```jazz jazz-signature
dictionaryReplace :: @{Eq(k)}: Dictionary(k, v) -> k -> v -> Maybe(Dictionary(k, v)).
```

Replaces an existing value without moving its key. Returns `Nothing` when the
key is absent. The update is `O(n)`.

### `dictionaryRemove`

```jazz jazz-signature
dictionaryRemove :: @{Eq(k)}: Dictionary(k, v) -> k -> Dictionary(k, v).
```

Removes the key and its value. An absent key returns an equivalent dictionary.
Remaining keys retain their order. The update is `O(n)`.

### `dictionaryUpdate`

```jazz jazz-signature
dictionaryUpdate :: @{Eq(k)}: Dictionary(k, v) -> k -> (Maybe(v) -> Maybe(v)) -> Dictionary(k, v).
```

Calls the function with the current value as `Just`, or `Nothing` when absent.
Returning `Nothing` removes an existing key; returning `Just` replaces it or
appends a new key. The update is `O(n)` plus callback work.

## Views and traversal

### `dictionaryKeys`

```jazz jazz-signature
dictionaryKeys :: Dictionary(k, v) -> [k].
```

Returns keys in insertion order in `O(n)`.

### `dictionaryValues`

```jazz jazz-signature
dictionaryValues :: Dictionary(k, v) -> [v].
```

Returns values in their keys' insertion order in `O(n)`.

### `dictionaryMapValues`

```jazz jazz-signature
dictionaryMapValues :: Dictionary(k, v) -> (v -> w) -> Dictionary(k, w).
```

Transforms every value in insertion order while preserving keys and their
positions. This is `O(n)` plus callback work.

### `dictionaryFilter`

```jazz jazz-signature
dictionaryFilter :: Dictionary(k, v) -> (k -> v -> Bool) -> Dictionary(k, v).
```

Keeps pairs whose predicate returns `True`, preserving their relative insertion
order. This is `O(n)` plus callback work.

### `dictionaryFoldLeft`

```jazz jazz-signature
dictionaryFoldLeft :: Dictionary(k, v) -> a -> (a -> k -> v -> a) -> a.
```

Folds pairs from earliest to latest insertion, starting with the supplied
accumulator. This is `O(n)` plus callback work.

### `dictionaryFoldRight`

```jazz jazz-signature
dictionaryFoldRight :: Dictionary(k, v) -> a -> (k -> v -> a -> a) -> a.
```

Folds pairs from latest to earliest insertion, starting with the supplied
terminal value. This is `O(n)` plus callback work.

Use [Map](map.md) when ascending key order and logarithmic lookup matter more
than insertion order and `Eq`-only keys.
