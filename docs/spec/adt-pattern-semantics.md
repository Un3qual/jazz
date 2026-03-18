# ADT Semantics

Status: active (Milestone 1 contract lock; active `jazz-next` implementation still pending parser/type/runtime follow-up)
Locked decisions: 2026-03-18
Primary plan: `docs/plans/2026-03-18-jazz-next-adt-and-pattern-matching-rebase-plan.md`

## Purpose

Define the active-path contract for algebraic data types and constructor values
so upcoming `jazz-next` parser, type, and runtime work converges on one model.

## Implementation Target

- All new ADT implementation work for this contract lands in `jazz-next/`.
- `jazz-hs/` and `jazz2/` are read-only legacy evidence only.

## Current Active-Path Status

1. `jazz-next` does not yet implement surface or lowered `data` declarations.
2. `jazz-next` does not yet implement constructor values, constructor
   application, or constructor-aware runtime values.
3. This document locks the contract for upcoming active-path work; it is not a
   claim that user-defined ADTs already execute today.
4. The only landed `case` subset is the simple pattern-matching slice defined
   in `docs/spec/pattern-matching-semantics.md`.

## ADT Contract

1. A `data` declaration introduces one nominal type constructor and a closed set
   of value constructors.
2. Type names and constructor names use uppercase identifiers.
3. Constructor payloads are positional; field order is semantically relevant.
4. Constructors are first-class values in expression position.
5. Constructor application uses ordinary function application semantics rather
   than a special call form.
6. Pattern matching over constructor values is defined by
   `docs/spec/pattern-matching-semantics.md`.

Canonical shape example:

```jz
data Maybe(a) { Just(a), Nothing }

some = Just 1.
none = Nothing.
```

## Staged First Slice

1. The first executable ADT slice in `jazz-next` is limited to plain sum types
   with positional constructors.
2. Constructor type syntax must align with the active type-grammar rebase work
   before the full surface is executable.
3. Built-in lists remain separately implemented runtime values; user ADTs do
   not redefine list semantics in the first slice.
4. Tuple values and tuple patterns remain outside the first ADT slice until
   tuple ownership is explicitly planned on the active path.

## Decision Cross-References

- Canonical general syntax rules remain in `docs/spec/authoritative-syntax.md`.
- The committed active-path `case` subset is defined in
  `docs/spec/pattern-matching-semantics.md`.
- `if` continues to desugar through the existing boolean-only `ECase` contract
  defined in `docs/spec/control-flow/if-expressions.md`.

## Non-Goals (Milestone 1)

1. GADTs, existential constructors, or record/named-field constructors.
2. Infix constructors or alternate constructor call syntax.
3. Automatic deriving or trait/class synthesis for user ADTs.
4. Tuple-constructor sugar, tuple-pattern semantics, or lambda-parameter
   patterns.
