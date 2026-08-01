---
title: Map and Set
description: Use persistent ordered maps and sets with logarithmic lookup and update.
sidebar_position: 7
---

# Map and Set

## Map

`Map(k, v)` is an abstract persistent balanced search tree. Key operations
require `Ord(k)`. Inserting an existing key replaces its value; list views and
folds use ascending key order.

Construction exposes `mapEmpty`, `mapSingleton`, `mapFromList`, and
`mapToList`. Size and lookup use `mapSize`, `mapIsEmpty`, `mapLookup`,
`mapGetOr`, and `mapContainsKey`. Updates are `mapInsert`, `mapReplace`,
`mapRemove`, and `mapUpdate`. Boundary and traversal operations are
`mapMinimum`, `mapMaximum`, `mapPopMinimum`, `mapPopMaximum`, `mapKeys`,
`mapValues`, `mapMapValues`, `mapFilter`, `mapFoldLeft`, and
`mapFoldRight`.

Lookup and updates are `O(log n)`. From-list is `O(n log n)`; ordered views and
folds are `O(n)`; filter is `O(n log n)` worst case. Replace and boundaries
return `Nothing` when absent or empty. Pop returns both the entry and remaining
persistent map.

## Set

`Set(a)` is an abstract persistent ordered set. Duplicate insertions do not
change its size, and list views and folds use ascending order.

The API is `setEmpty`, `setSingleton`, `setFromList`, `setToList`, `setSize`,
`setIsEmpty`, `setContains`, `setInsert`, `setRemove`, `setUnion`,
`setIntersection`, `setDifference`, `setIsSubset`, `setFilter`, `setMap`,
`setFoldLeft`, and `setFoldRight`.

Contains, insert, and remove are `O(log n)`. From-list and value-changing map
are `O(n log n)`; materialization and folds are `O(n)`. Union is
`O(m log(n + m))` for the traversed right set; subset is `O(n log m)`.
Intersection and difference are `O(n * (log n + log m))` worst case because
they combine membership checks with rebuilding. Representations remain private.

For insertion order with `Eq`-only keys, use [Dictionary](dictionary.md).
