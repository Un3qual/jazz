---
title: Reduce
description: Fold a collection without an initial value, returning Maybe.
---

Import `Reduce` explicitly. Its helper uses the collection's `Reducible`
instance and returns a value from [Maybe](maybe.md).

## `reduce`

```jazz jazz-signature
reduce :: @{Reducible(f)}: (a -> a -> a) -> f(a) -> Maybe(a).
```

Returns Nothing for an empty collection. A singleton returns Just its element.
For longer collections, the first element becomes the accumulator and each
remaining element is combined from left to right. For example, subtraction on
`[9, 3, 1]` returns `Just 5` because `(9 - 3) - 1` is 5.

The helper performs one `foldLeft` with a Maybe accumulator. Traversal order is
the collection's documented fold order, including FIFO order for Queue and
insertion order for Dictionary. No intermediate List is constructed.
