---
title: Diagnostics
description: Interpret stable Jazz error codes, warning categories, source labels, and severity behavior.
sidebar_position: 5
---

## Diagnostic model and code ranges

Jazz uses one structured diagnostic model for errors and warnings. Reports
carry effective severity, a stable code, origin, summary, source labels, notes,
and help. Warning promotion changes severity but keeps the `W####` code and
does not duplicate the report.

| Range           | Subsystem                                      |
| --------------- | ---------------------------------------------- |
| `E0001`–`E0005` | syntax and Prelude preparation                 |
| `E1001`–`E1010` | name and semantic analysis                     |
| `E2001`–`E2019` | type inference and checking                    |
| `E3001`–`E3040` | runtime evaluation and host operations         |
| `E4001`–`E4016` | module resolution, imports, and exports        |
| `E5001`–`E5005` | CLI, configuration, source input, and profiles |

Pattern coverage uses two strict type diagnostics:

| Code    | Meaning                                                                 |
| ------- | ----------------------------------------------------------------------- |
| `E2018` | a case or pattern lambda is non-exhaustive; the summary gives a witness |
| `E2019` | an entire arm is unreachable after earlier unguarded arms               |

Guarded arms never contribute to exhaustiveness or shadow later arms. A guarded
arm is still checked for reachability, so earlier unguarded arms can make it
report `E2019`. Existing analysis or type errors suppress pattern-coverage
follow-on reports.

## Warning categories and IDs

The warning catalog is:

| Code    | Token                   | Status                                                   |
| ------- | ----------------------- | -------------------------------------------------------- |
| `W0001` | `same-scope-rebinding`  | emitted when enabled                                     |
| `W0002` | `shadowing-outer-scope` | emitted when enabled                                     |
| `W0003` | `unused-binding`        | emitted for ordinary lexical-block bindings when enabled |
| `W0004` | `deprecated-syntax`     | reserved; no analyzer emitter                            |

Warnings are off by default. Syntax rejected by the parser is an error, not a
deprecation warning. CLI, environment, and config selection are described in
the [CLI reference](cli.md).

## Output and severity

Standalone spans render as `line:column`; resolved module spans include the
source path. Cross-module diagnostics can carry related locations. Compile
errors suppress evaluation. Runtime errors suppress the final rendered value;
warning-only runs still evaluate.
