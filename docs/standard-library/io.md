---
title: IO and errors
description: Perform strict UTF-8 host I/O through stable Jazz error values.
sidebar_position: 9
---

`IOErrorCategory` publishes `NotFound`, `PermissionDenied`, `AlreadyExists`,
`InvalidData`, `ResourceExhausted`, `Interrupted`, `Unsupported`, and `Other`.
`IOError` publishes `IOError category maybePath message`. These values hide
platform exception types and OS error numbers.

The `IO` module exports:

- `readText! :: Text -> Result(IOError, Text)`;
- `writeText! :: Text -> Text -> Result(IOError, ())`;
- `readStdin! :: () -> Result(IOError, Text)`;
- `writeStdout! :: Text -> Result(IOError, ())`;
- `writeStderr! :: Text -> Result(IOError, ())`;
- `arguments! :: () -> [Text]`; and
- `exit! :: Int -> ()`.

Files and streams use strict UTF-8. Recoverable operations return `Result`;
file errors attach a path and stream errors do not. Arguments preserve process
order. Exit delegates to the installed runtime host. Cost depends on the host
and payload and has no collection-style complexity promise.

The `!` suffix participates in Jazz's current [purity contract](../language/purity.md).
Use [Result](maybe-result-nonempty.md) to handle recoverable failures.
