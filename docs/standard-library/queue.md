---
title: Queue
description: Use a persistent first-in, first-out queue with amortized constant-time removal.
sidebar_position: 6
---

`Queue(a)` is an abstract persistent FIFO queue. `queueEnqueue` adds at the
rear; `queuePeek` and `queueDequeue` observe the oldest value. Empty
observations return `Nothing`, and older queue values remain valid.

## Operations

The public API is `queueEmpty`, `queueSingleton`, `queueFromList`,
`queueToList`, `queueSize`, `queueIsEmpty`, `queueEnqueue`,
`queueEnqueueAll`, `queuePeek`, `queueDequeue`, `queueMap`,
`queueFoldLeft`, and `queueFoldRight`.

## Complexity

Size and empty checks are `O(1)`. Enqueue is `O(1)`. `queuePeek` is `O(1)` when
the front is populated and `O(n)` when it must reverse a non-empty rear to find
the oldest value. Because `queuePeek` does not return the normalized queue,
repeated peeks of the same front-empty value repeat that `O(n)` work.

`queueDequeue` is amortized `O(1)` only across a dequeue sequence that keeps
using each returned queue. One dequeue may still spend `O(n)` normalizing the
rear. Conversion, mapping, and folds are `O(n)`; enqueueing `m` values is
`O(m)`. Views and folds preserve FIFO order.

## Empty queues

The constructor and internal representation are private. Empty observations
integrate with [Maybe](maybe.md).
