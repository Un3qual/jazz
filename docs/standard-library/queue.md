---
title: Queue
description: Use a persistent first-in, first-out queue with amortized constant-time removal.
sidebar_position: 6
---

Import `Queue` for a persistent FIFO collection. Enqueueing adds values at the
rear; observation and removal use the oldest value. The representation and
constructor are private. `empty` and `singleton` construction, size,
emptiness, and enqueue are `O(1)`.

## Type

### `Queue`

`Queue(a)` stores values of type `a`. Every update returns a new queue and
leaves older queue values valid.

## Construction and views

### `empty`

```jazz jazz-signature
empty :: Queue(a).
```

### `singleton`

```jazz jazz-signature
singleton :: a -> Queue(a).
```

### `fromList`

```jazz jazz-signature
fromList :: [a] -> Queue(a).
```

Constructs a queue whose FIFO order matches the input order in `O(n)` time.

### `toList`

```jazz jazz-signature
toList :: Queue(a) -> [a].
```

Returns values from oldest to newest in `O(n)`.

### `size`

```jazz jazz-signature
size :: Queue(a) -> Int.
```

### `isEmpty`

```jazz jazz-signature
isEmpty :: Queue(a) -> Bool.
```

## Updating and observing

### `enqueue`

```jazz jazz-signature
enqueue :: Queue(a) -> a -> Queue(a).
```

### `enqueueAll`

```jazz jazz-signature
enqueueAll :: Queue(a) -> [a] -> Queue(a).
```

Adds values at the rear in list order. Enqueueing `m` values is `O(m)`.

### `peek`

```jazz jazz-signature
peek :: Queue(a) -> Maybe::Maybe(a).
```

Returns the oldest value as `Just`, or `Nothing` for an empty queue.
`peek` is `O(1)` when the front is populated and `O(n)` when it must
reverse a non-empty rear to find the oldest value. Because `peek` does not
return the normalized queue, repeated peeks of the same front-empty value repeat
that `O(n)` work.

### `dequeue`

```jazz jazz-signature
dequeue :: Queue(a) -> Maybe::Maybe((a, Queue(a))).
```

Returns the oldest value and the remaining queue, or `Nothing` when empty. A
single call may spend `O(n)` normalizing the rear. `dequeue` is amortized
`O(1)` only across a dequeue sequence that keeps using each returned queue.

## Transforming and folding

### `map`

```jazz jazz-signature
map :: Queue(a) -> (a -> b) -> Queue(b).
```

Transforms every value and preserves FIFO order in the returned queue. Callback
evaluation order is not guaranteed. This is `O(n)` plus callback work.

### `foldLeft`

```jazz jazz-signature
foldLeft :: Queue(a) -> b -> (b -> a -> b) -> b.
```

Folds from oldest to newest, beginning with the supplied accumulator. This is
`O(n)` plus callback work.

### `foldRight`

```jazz jazz-signature
foldRight :: Queue(a) -> b -> (a -> b -> b) -> b.
```

Folds from newest to oldest, beginning with the supplied terminal value. This
is `O(n)` plus callback work.

Empty observations use [Maybe](maybe.md).

## Generic methods

Importing Queue supplies element-based Equatable, Mappable, Reducible, and Combinable instances. Generic methods use FIFO order. Equality compares contents independently of construction history.
