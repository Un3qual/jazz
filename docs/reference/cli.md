---
title: CLI reference
description: Reference Jazz command-line modes, source selection, Prelude control, and runtime observations.
sidebar_position: 4
---

```text
Usage: jazz [--run] [options] [source.jz]
       jazz [--run] --entry-module Module::Path [--module-root DIR...] [options]
```

## Modes and source selection

| Form                  | Behavior                                  |
| --------------------- | ----------------------------------------- |
| no source or `-`      | read standalone source from stdin         |
| `source.jz`           | read one standalone source file           |
| `--entry-module A::B` | load a module graph; default root is `.`  |
| `--run`               | evaluate and print the final value        |
| no `--run`            | parse and analyze; clean success is quiet |

Repeated `--module-root DIR` flags preserve order and require an entry module.
A source selector and entry module are mutually exclusive.

## Prelude and warnings

`--prelude PATH` selects an explicit Prelude; `--no-prelude` disables it. They
cannot be combined. `JAZZ_PRELUDE` is used only when neither CLI form selects a
Prelude.

Warning controls include `-Wcategory`, `-Wno-category`,
`-Werror=category`, `-Werror`, and `-Wnone`. `--warnings-config PATH` selects a
config file. Resolution precedence is CLI, environment, project config, then
the silent default. Environment variables are `JAZZ_WARNING_FLAGS`,
`JAZZ_WARNING_ERROR_FLAGS`, and `JAZZ_WARNING_CONFIG`; the default project file
is `.jazz-warnings`.

## Runtime observations

`--runtime-stats`, `--runtime-stats=human`, and `--runtime-stats=json` report
deterministic semantic statistics on stderr. `--runtime-profile=PATH` writes a
deterministic Speedscope profile atomically. These options require `--run`.

`--help` and `-h` print help to stdout without reading source, Prelude, or
configuration files. Invalid arguments and required-file read failures exit
with status 2; compile or runtime diagnostics exit with status 1. See
[diagnostics](diagnostics.md).
