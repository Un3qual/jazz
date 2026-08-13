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

`NonEmpty first rest` constructs a non-empty sequence directly and may be used
in [patterns](../language/algebraic-data-types-and-patterns.md).

## Construction and conversion

### `nonEmptySingleton`

```jazz jazz-signature
nonEmptySingleton :: a -> NonEmpty(a).
```

Constructs a one-value sequence. This is `O(1)`.

### `nonEmptyFromList`

```jazz jazz-signature
nonEmptyFromList :: [a] -> Maybe(NonEmpty(a)).
```

Returns `Nothing` for `[]`; otherwise stores the list head and tail without
traversing them. This is `O(1)`.

### `nonEmptyToList`

```jazz jazz-signature
nonEmptyToList :: NonEmpty(a) -> [a].
```

Returns the head followed by the tail. This is `O(1)`.

### `nonEmptyPrepend`

```jazz jazz-signature
nonEmptyPrepend :: a -> NonEmpty(a) -> NonEmpty(a).
```

Adds a new first value and preserves the existing sequence after it. This is
`O(1)`.

### `nonEmptyAppendList`

```jazz jazz-signature
nonEmptyAppendList :: NonEmpty(a) -> [a] -> NonEmpty(a).
```

Appends an ordinary list after the non-empty sequence. This is `O(n)` in the
original sequence tail.

## Access

### `nonEmptyHead`

```jazz jazz-signature
nonEmptyHead :: NonEmpty(a) -> a.
```

Returns the guaranteed first value. The function is total and `O(1)`.

### `nonEmptyTail`

```jazz jazz-signature
nonEmptyTail :: NonEmpty(a) -> [a].
```

Returns every value after the head. The result may be empty. This is `O(1)`.

### `nonEmptyLast`

```jazz jazz-signature
nonEmptyLast :: NonEmpty(a) -> a.
```

Returns the final value. The function is total and `O(n)`.

### `nonEmptyLength`

```jazz jazz-signature
nonEmptyLength :: NonEmpty(a) -> Int.
```

Returns the number of values, which is always at least one. This is `O(n)`.

## Transforming and folding

### `nonEmptyMap`

```jazz jazz-signature
nonEmptyMap :: (a -> b) -> NonEmpty(a) -> NonEmpty(b).
```

Transforms every value in order and preserves non-emptiness. This is `O(n)`
plus callback work.

### `nonEmptyFoldLeft`

```jazz jazz-signature
nonEmptyFoldLeft :: (b -> a -> b) -> b -> NonEmpty(a) -> b.
```

Combines values from head to last, beginning with the supplied accumulator.
This is `O(n)` plus callback work.

### `nonEmptyFoldRight`

```jazz jazz-signature
nonEmptyFoldRight :: (a -> b -> b) -> b -> NonEmpty(a) -> b.
```

Combines values from last to head, beginning with the supplied terminal value.
This is `O(n)` plus callback work.
