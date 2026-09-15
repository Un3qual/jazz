---
title: NonEmpty
description: Represent a list with a statically present first value.
---

Import `NonEmpty` when a sequence must contain at least one value.

## Type and constructor

### `NonEmpty`

```jazz jazz-signature
data NonEmpty a = NonEmpty a [a].
```

The public constructor stores the guaranteed head followed by an ordinary list
tail. The same name refers to the type and its constructor.

### `NonEmpty`

The constructor is available to
[patterns](../language/algebraic-data-types-and-patterns.md).

## Construction and conversion

### `singleton`

```jazz jazz-signature
singleton :: a -> NonEmpty(a).
```

### `fromList`

```jazz jazz-signature
fromList :: [a] -> Maybe::Maybe(NonEmpty(a)).
```

Returns `Nothing` for `[]`; otherwise stores the list head and tail without
traversing them. This is `O(1)`.

### `toList`

```jazz jazz-signature
toList :: NonEmpty(a) -> [a].
```

### `prepend`

```jazz jazz-signature
prepend :: a -> NonEmpty(a) -> NonEmpty(a).
```

Adds a new first value in `O(1)`.

### `appendList`

```jazz jazz-signature
appendList :: NonEmpty(a) -> [a] -> NonEmpty(a).
```

Appends an ordinary list after the non-empty sequence. This is `O(n)` in the
original sequence tail.

## Access

### `head`

```jazz jazz-signature
head :: NonEmpty(a) -> a.
```

Total and `O(1)`.

### `tail`

```jazz jazz-signature
tail :: NonEmpty(a) -> [a].
```

The result may be empty. This is `O(1)`.

### `last`

```jazz jazz-signature
last :: NonEmpty(a) -> a.
```

Returns the final value. The function is total and `O(n)`.

### `length`

```jazz jazz-signature
length :: NonEmpty(a) -> Int.
```

Returns the number of values, which is always at least one. This is `O(n)`.

## Transforming and folding

### `map`

```jazz jazz-signature
map :: (a -> b) -> NonEmpty(a) -> NonEmpty(b).
```

Transforms every value in order and preserves non-emptiness. This is `O(n)`
plus callback work.

### `foldLeft`

```jazz jazz-signature
foldLeft :: (b -> a -> b) -> b -> NonEmpty(a) -> b.
```

Combines values from head to last, beginning with the supplied accumulator.
This is `O(n)` plus callback work.

### `foldRight`

```jazz jazz-signature
foldRight :: (a -> b -> b) -> b -> NonEmpty(a) -> b.
```

Combines values from last to head, beginning with the supplied terminal value.
This is `O(n)` plus callback work.

## Generic methods

Importing NonEmpty supplies element-based Equatable, Mappable, Reducible, and Combinable instances. Mapping and concatenation preserve the nonempty invariant.
