---
title: Set
description: Use a persistent ordered set with logarithmic membership and update.
---

Import `Set` for unique, ordered values. Values require `Comparable(a)`, and views
traverse them in ascending order. The representation and constructor are
private. Membership and single-value updates are `O(log n)`; ordered traversal
is `O(n)`.

## Type

### `Set`

`Set(a)` stores at most one occurrence of each value of type `a`.

## Construction

### `empty`

```jazz jazz-signature
empty :: Set(a).
```

### `singleton`

```jazz jazz-signature
singleton :: a -> Set(a).
```

### `fromList`

```jazz jazz-signature
fromList :: @{Comparable(a)}: [a] -> Set(a).
```

Inserts values from left to right and removes duplicates. This is
`O(n log n)`.

### `toList`

```jazz jazz-signature
toList :: Set(a) -> [a].
```

Returns values in ascending order in `O(n)`.

## Querying

### `size`

```jazz jazz-signature
size :: Set(a) -> Int.
```

### `isEmpty`

```jazz jazz-signature
isEmpty :: Set(a) -> Bool.
```

### `contains`

```jazz jazz-signature
contains :: @{Comparable(a)}: Set(a) -> a -> Bool.
```

## Updating and combining

### `insert`

```jazz jazz-signature
insert :: @{Comparable(a)}: Set(a) -> a -> Set(a).
```

Inserting a duplicate does not change the set.

### `remove`

```jazz jazz-signature
remove :: @{Comparable(a)}: Set(a) -> a -> Set(a).
```

Removes a value when present. An absent value returns an equivalent set. This
is `O(log n)`.

### `union`

```jazz jazz-signature
union :: @{Comparable(a)}: Set(a) -> Set(a) -> Set(a).
```

Returns every value present in either set. The implementation is
`O(m log(n + m))` for inputs of sizes `n` and `m`.

### `intersection`

```jazz jazz-signature
intersection :: @{Comparable(a)}: Set(a) -> Set(a) -> Set(a).
```

Returns values present in both sets. This is `O(n × (log n + log m))` worst
case.

### `difference`

```jazz jazz-signature
difference :: @{Comparable(a)}: Set(a) -> Set(a) -> Set(a).
```

Returns values from the first set that are absent from the second. This is
`O(n × (log n + log m))` worst case.

### `isSubset`

```jazz jazz-signature
isSubset :: @{Comparable(a)}: Set(a) -> Set(a) -> Bool.
```

Returns whether every value in the first set occurs in the second. After a
missing value is found, later membership lookups are skipped, but traversal of
the first set continues. This is `O(n log m)` worst case.

## Transforming and traversal

### `filter`

```jazz jazz-signature
filter :: @{Comparable(a)}: Set(a) -> (a -> Bool) -> Set(a).
```

Keeps values whose predicate returns `True`. Callbacks run in ascending order;
rebuilding the result is `O(n log n)` worst case.

### `map`

```jazz jazz-signature
map :: @{Comparable(b)}: Set(a) -> (a -> b) -> Set(b).
```

Transforms values in ascending input order and rebuilds ordering for `b`.
Duplicate outputs collapse to one value. This is `O(n log n)` plus callback
work.

### `foldLeft`

```jazz jazz-signature
foldLeft :: Set(a) -> b -> (b -> a -> b) -> b.
```

Folds values from least to greatest in `O(n)` plus callback work.

### `foldRight`

```jazz jazz-signature
foldRight :: Set(a) -> b -> (a -> b -> b) -> b.
```

Folds values from greatest to least in `O(n)` plus callback work.

## Generic methods

Importing Set supplies Reducible without an element ordering constraint and Combinable when elements are Comparable. Set has no Mappable instance. Use its collection-first map for ordered, deduplicated output, or convert with Set::toList before generic mapping.
