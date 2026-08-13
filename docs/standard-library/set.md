---
title: Set
description: Use a persistent ordered set with logarithmic membership and update.
---

Import `Set` to use `Set(a)`, a persistent ordered set whose values require
`Ord(a)`. Duplicate insertions do not change its size, and list views and folds
use ascending order. The representation is private.

## Construction and querying

Use `setEmpty`, `setSingleton`, `setFromList`, and `setToList` for construction
and materialization. `setSize`, `setIsEmpty`, and `setContains` inspect a set.

## Updating and combining

`setInsert` and `setRemove` return new sets. `setUnion`, `setIntersection`, and
`setDifference` combine two sets, while `setIsSubset` tests containment.

## Transforming and traversing

`setFilter`, `setMap`, `setFoldLeft`, and `setFoldRight` traverse values in
ascending order. A value-changing map rebuilds ordering for the output type.

Empty and singleton construction, size, and empty checks are `O(1)`. Contains,
insert, and remove are `O(log n)`. From-list, map, and filter are `O(n log n)`;
materialization and folds are `O(n)`. Union is `O(m log(n + m))`, subset is
`O(n log m)`, and intersection and difference are
`O(n * (log n + log m))` worst case.
