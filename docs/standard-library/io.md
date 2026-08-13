---
title: IO
description: Perform strict UTF-8 file, stream, argument, and process operations.
---

Import `IO` for effectful host operations. The module exports:

- `readText! :: Text -> Result(IOError, Text)`;
- `writeText! :: Text -> Text -> Result(IOError, ())`;
- `readStdin! :: () -> Result(IOError, Text)`;
- `writeStdout! :: Text -> Result(IOError, ())`;
- `writeStderr! :: Text -> Result(IOError, ())`;
- `arguments! :: () -> [Text]`; and
- `exit! :: Int -> ()`.

Files and streams use strict UTF-8. Recoverable operations return [Result](result.md);
file errors attach a path and stream errors do not. Arguments preserve process
order. `exit!` accepts statuses from `0` through `255`; an out-of-range status
fails with fatal diagnostic `E3030` before the host is called. Valid statuses
delegate to the installed runtime host. Cost depends on the host and payload
and has no collection-style complexity promise.

The `!` suffix participates in Jazz's current [purity contract](../language/purity.md).
Error categories and values are documented by [IOError](io-error.md).
