---
title: Module resolution
description: Reference module-to-file mapping, graph traversal, imports, exports, and module diagnostics.
sidebar_position: 3
---

`A::B` maps to `A/B.jz` below each ordered module root. Candidate paths are
lexically normalized and deduplicated while preserving root order. Zero
matches produce `E4001`; more than one distinct match produces `E4002`.

Resolution performs deterministic depth-first traversal. Imports are
deduplicated and visited in lexical rendered-name order, independent of source
order. Completed modules are reused. Encountering a module already on the
active stack produces `E4003` with the minimal cycle. Source parse failure is
`E4004`; multiple module declarations and declaration/path mismatch are
`E4005` and `E4006`. The final graph is dependency-first.

An omitted module declaration is accepted and takes the resolved path as its
identity. A declaration, when present, must be the first top-level form and
must match the requested module path.

## Import binding

- `import A::B.` exposes public values, constructors, type identities, and
  capabilities unqualified.
- `import A::B (x, y).` exposes only selected eligible names.
- `import A::B as Alias.` exposes public values, constructors, and type
  identities only through `Alias::name`.

Aliases and symbol lists are mutually exclusive. User-facing import collision
and visibility diagnostics use `E4007`–`E4009` and `E4011`–`E4014`; `E4010`
reports an internal missing-dependency-inventory invariant. Module export-list
validation uses `E4015`; invalid entry paths use `E4016`.

Module header lists are allowlists. Typed selectors include `value name`,
`type Name`, `type Name(..)`, selected type constructors,
`constructor Name`, and `class Name`. Bare selectors are compatibility
shorthand for all owned same-text entries. Omitted lists export all owned
declarations and `()` exports none. Imported declarations are not eligible for
re-export.

The loader compiles every module against explicit dependency interfaces.
During execution, dependency bindings establish exports but dependency
expression statements are skipped; only entry-module expressions produce the
program result. See the [module guide](../language/modules.md) for usage.
