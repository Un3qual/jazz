---
title: Queue
description: Use a persistent first-in, first-out queue with amortized constant-time removal.
sidebar_position: 6
---

# Queue

`Queue(a)` is an abstract persistent FIFO queue. `queueEnqueue` adds at the
rear; `queuePeek` and `queueDequeue` observe the oldest value. Empty
observations return `Nothing`, and older queue values remain valid.

The public API is `queueEmpty`, `queueSingleton`, `queueFromList`,
`queueToList`, `queueSize`, `queueIsEmpty`, `queueEnqueue`,
`queueEnqueueAll`, `queuePeek`, `queueDequeue`, `queueMap`,
`queueFoldLeft`, and `queueFoldRight`.

Size and empty checks are `O(1)`. Enqueue is `O(1)`. Peek and dequeue are
amortized `O(1)`, though a normalization step can be `O(n)`. Conversion,
mapping, and folds are `O(n)`; enqueueing `m` values is `O(m)`. Views and
folds preserve FIFO order.

The constructor and internal representation are private. Empty observations
integrate with [Maybe](maybe-result-nonempty.md).
