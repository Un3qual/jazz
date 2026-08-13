---
title: Queue
description: Use a persistent first-in, first-out queue with amortized constant-time removal.
sidebar_position: 6
---

Import `Queue` for a persistent FIFO collection. Enqueueing adds values at the
rear; observation and removal use the oldest value. The representation and
constructor are private.

## Type

### `Queue`

`Queue(a)` stores values of type `a`. Every update returns a new queue and
leaves older queue values valid.

## Construction and views

### `queueEmpty`

```jazz jazz-signature
queueEmpty :: Queue(a).
```

The empty queue. Construction is `O(1)`.

### `queueSingleton`

```jazz jazz-signature
queueSingleton :: a -> Queue(a).
```

Constructs a queue containing one value in `O(1)`.

### `queueFromList`

```jazz jazz-signature
queueFromList :: [a] -> Queue(a).
```

Constructs a queue whose FIFO order matches the input order. This is `O(1)` at
the API boundary.

### `queueToList`

```jazz jazz-signature
queueToList :: Queue(a) -> [a].
```

Returns values from oldest to newest in `O(n)`.

### `queueSize`

```jazz jazz-signature
queueSize :: Queue(a) -> Int.
```

Returns the number of values in `O(1)`.

### `queueIsEmpty`

```jazz jazz-signature
queueIsEmpty :: Queue(a) -> Bool.
```

Returns `True` when the queue has no values. This is `O(1)`.

## Updating and observing

### `queueEnqueue`

```jazz jazz-signature
queueEnqueue :: Queue(a) -> a -> Queue(a).
```

Adds a value at the rear and returns the updated queue in `O(1)`.

### `queueEnqueueAll`

```jazz jazz-signature
queueEnqueueAll :: Queue(a) -> [a] -> Queue(a).
```

Adds values at the rear in list order. Enqueueing `m` values is `O(m)`.

### `queuePeek`

```jazz jazz-signature
queuePeek :: Queue(a) -> Maybe(a).
```

Returns the oldest value as `Just`, or `Nothing` for an empty queue.
`queuePeek` is `O(1)` when the front is populated and `O(n)` when it must
reverse a non-empty rear to find the oldest value. Because `queuePeek` does not
return the normalized queue, repeated peeks of the same front-empty value repeat
that `O(n)` work.

### `queueDequeue`

```jazz jazz-signature
queueDequeue :: Queue(a) -> Maybe((a, Queue(a))).
```

Returns the oldest value and the remaining queue, or `Nothing` when empty. A
single call may spend `O(n)` normalizing the rear. `queueDequeue` is amortized
`O(1)` only across a dequeue sequence that keeps using each returned queue.

## Transforming and folding

### `queueMap`

```jazz jazz-signature
queueMap :: Queue(a) -> (a -> b) -> Queue(b).
```

Transforms values from oldest to newest and preserves FIFO order. This is
`O(n)` plus callback work.

### `queueFoldLeft`

```jazz jazz-signature
queueFoldLeft :: Queue(a) -> b -> (b -> a -> b) -> b.
```

Folds from oldest to newest, beginning with the supplied accumulator. This is
`O(n)` plus callback work.

### `queueFoldRight`

```jazz jazz-signature
queueFoldRight :: Queue(a) -> b -> (a -> b -> b) -> b.
```

Folds from newest to oldest, beginning with the supplied terminal value. This
is `O(n)` plus callback work.

Empty observations use [Maybe](maybe.md).
