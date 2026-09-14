---
title: Module resolution
description: Reference module-to-file mapping, graph traversal, imports, exports, and module diagnostics.
sidebar_position: 3
---

`A::B` maps to `A/B.jz` below each ordered module root. Candidate paths are
lexically normalized and deduplicated while preserving root order. Zero
matches produce `E4001`; more than one distinct match produces `E4002`.

Resolution performs a deterministic depth-first traversal. Imports are
deduplicated and visited by rendered name, independent of source order.
Completed modules are reused. A module already on the active stack produces
`E4003` with the minimal cycle. Source parse failure is `E4004`; multiple module
declarations and declaration/path mismatch are `E4005` and `E4006`. The final
graph is dependency-first.

An omitted module declaration is accepted and takes the resolved path as its
identity. A declaration, when present, must be the first top-level form and
must match the requested module path.

## Import binding

- `import A::B.` exposes public values, constructors, type identities, and
  capabilities unqualified.
- `import A::B (x, y).` exposes only selected eligible names.
- `import A::B as Alias.` exposes public values, constructors, type identities,
  and classes only through `Alias::name`. A public class method is referenced
  as `Alias::Class::method`.

Aliases and symbol lists are mutually exclusive. User-facing import collision
and visibility diagnostics use `E4007`–`E4009` and `E4011`–`E4014`; `E4010`
reports an internal missing-dependency-inventory invariant. Module export-list
validation uses `E4015`; invalid entry paths use `E4016`.

Module header lists are allowlists. Typed selectors include `value name`,
`type Name`, `type Name(..)`, selected type constructors,
`constructor Name`, and `class Name`. Bare selectors are compatibility
shorthand for all owned same-text entries. Omitted lists export all owned
declarations and `()` exports none. Imported declarations are not eligible for
re-export. Selecting `class Name` includes the class's ordinary method values;
`value method` can select one method without exposing its class name. Supporting
class and type metadata remains available to check those values and instances.
A module exporting `()` still supplies its implementations when imported.

Each module is checked against explicit dependency interfaces. During
execution, dependencies establish their exported bindings without evaluating
top-level expression statements; only entry-module expressions produce the
program result. See the [module guide](../language/modules.md) for usage.

## Qualified classes

`Alias::Class` names an exported class in signature constraints and impl heads.
`Alias::Class::method` names one of its methods in an expression. Components must
be adjacent, with exactly two for a class and three for a method; the alias must
come from an explicit import. Class declarations still introduce unqualified
names owned by the declaring module.

Qualification preserves the original class identity. Two aliases for one
module, or aliased and unqualified imports together, share its implementation
evidence. Same-spelled classes from different modules remain distinct. Existing
generic-head overlap and method-ambiguity rules apply. An alias exposes neither
the class nor its methods unqualified, and a private class cannot be reached
through an alias or a same-spelled value or type. Importing does not re-export
names. Each dependency supplies its transitive implementations independently of
name selection, including implementations of classes declared elsewhere.

Qualified methods support direct calls, stored values, partial application and
explicit type application. The [module guide](../language/modules.md) contains a
checked example combining a method call, constrained signature and impl head.
Unknown aliases use `E4013`; missing or private classes use `E4014`; missing
methods use `E2015`. Diagnostics identify the failing name component.
