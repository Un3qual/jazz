---
title: NonEmpty
description: Represent a list with a statically present first value.
---

Import `NonEmpty` when a sequence must contain at least one value.

## Type and constructor

### `NonEmpty`

<!-- jazz-signature -->

```jazz
data NonEmpty a = NonEmpty a [a].
```

The public constructor stores the guaranteed head followed by an ordinary list
tail. The same name refers to the type and its constructor.

### `NonEmpty`

`NonEmpty first rest` constructs a non-empty sequence directly and may be used
in [patterns](../language/algebraic-data-types-and-patterns.md).

## Construction and conversion

### `nonEmptySingleton`

<!-- jazz-signature -->

```jazz
nonEmptySingleton :: a -> NonEmpty(a).
```

Constructs a one-value sequence. This is `O(1)`.

### `nonEmptyFromList`

<!-- jazz-signature -->

```jazz
nonEmptyFromList :: [a] -> Maybe(NonEmpty(a)).
```

Returns `Nothing` for `[]`; otherwise stores the list head and tail without
traversing them. This is `O(1)`.

### `nonEmptyToList`

<!-- jazz-signature -->

```jazz
nonEmptyToList :: NonEmpty(a) -> [a].
```

Returns the head followed by the tail. This is `O(1)`.

### `nonEmptyPrepend`

<!-- jazz-signature -->

```jazz
nonEmptyPrepend :: a -> NonEmpty(a) -> NonEmpty(a).
```

Adds a new first value and preserves the existing sequence after it. This is
`O(1)`.

### `nonEmptyAppendList`

<!-- jazz-signature -->

```jazz
nonEmptyAppendList :: NonEmpty(a) -> [a] -> NonEmpty(a).
```

Appends an ordinary list after the non-empty sequence. This is `O(n)` in the
original sequence tail.

## Access

### `nonEmptyHead`

<!-- jazz-signature -->

```jazz
nonEmptyHead :: NonEmpty(a) -> a.
```

Returns the guaranteed first value. The function is total and `O(1)`.

### `nonEmptyTail`

<!-- jazz-signature -->

```jazz
nonEmptyTail :: NonEmpty(a) -> [a].
```

Returns every value after the head. The result may be empty. This is `O(1)`.

### `nonEmptyLast`

<!-- jazz-signature -->

```jazz
nonEmptyLast :: NonEmpty(a) -> a.
```

Returns the final value. The function is total and `O(n)`.

### `nonEmptyLength`

<!-- jazz-signature -->

```jazz
nonEmptyLength :: NonEmpty(a) -> Int.
```

Returns the number of values, which is always at least one. This is `O(n)`.

## Transforming and folding

### `nonEmptyMap`

<!-- jazz-signature -->

```jazz
nonEmptyMap :: (a -> b) -> NonEmpty(a) -> NonEmpty(b).
```

Transforms every value in order and preserves non-emptiness. This is `O(n)`
plus callback work.

### `nonEmptyFoldLeft`

<!-- jazz-signature -->

```jazz
nonEmptyFoldLeft :: (b -> a -> b) -> b -> NonEmpty(a) -> b.
```

Combines values from head to last, beginning with the supplied accumulator.
This is `O(n)` plus callback work.

### `nonEmptyFoldRight`

<!-- jazz-signature -->

```jazz
nonEmptyFoldRight :: (a -> b -> b) -> b -> NonEmpty(a) -> b.
```

Combines values from last to head, beginning with the supplied terminal value.
This is `O(n)` plus callback work.
