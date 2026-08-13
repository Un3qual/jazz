---
title: Map
description: Use a persistent ordered map with logarithmic lookup and update.
---

Import `Map` to use `Map(k, v)`, a persistent balanced search tree. Keys require
`Ord(k)`. Inserting an existing key replaces its value; list views and folds use
ascending key order. The representation is private.

## Construction

`mapEmpty`, `mapSingleton`, `mapFromList`, and `mapToList` construct or
materialize maps. From-list construction keeps the last value for a duplicate
key.

## Querying and updating

Size and lookup use `mapSize`, `mapIsEmpty`, `mapLookup`, `mapGetOr`, and
`mapContainsKey`. Updates use `mapInsert`, `mapReplace`, `mapRemove`, and
`mapUpdate`. Replace returns `Nothing` when the key is absent.

## Boundaries and traversal

`mapMinimum`, `mapMaximum`, `mapPopMinimum`, and `mapPopMaximum` operate on
ordered boundaries and return `Nothing` for an empty map. Traversal uses
`mapKeys`, `mapValues`, `mapMapValues`, `mapFilter`, `mapFoldLeft`, and
`mapFoldRight`.

Lookup and updates are `O(log n)`. From-list is `O(n log n)`; ordered views and
folds are `O(n)`; filter is `O(n log n)` worst case. For insertion order with
`Eq`-only keys, use [Dictionary](dictionary.md).
