# ADT Semantics

Status: active (canonical and generic-parameter `data` declarations, generic constructor value/application schemes, constructor over-application diagnostics, constructor/list/tuple/as-pattern typing and runtime matching, and tuple literal values/signature types are implemented in `jazz-next`)
Locked decisions: 2026-03-18
Primary plan: `docs/plans/2026-03-18-jazz-next-adt-and-pattern-matching-rebase-plan.md`

## Purpose

Define the active-path contract for algebraic data types and constructor values
so upcoming `jazz-next` parser, type, and runtime work converges on one model.

## Implementation Target

- All new ADT implementation work for this contract lands in `jazz-next/`.
- `jazz-hs/` and `jazz2/` are read-only legacy evidence only.

## Rebase Closure Status

The active ADT/pattern rebase is closed for the monomorphic
constructor/list/tuple/as-pattern/lambda-pattern subset implemented in
`jazz-next`.

Future ADT typing work beyond the landed generic constructor value/application
scheme slice remains blocked on separate contracts. The generic
declaration-parameter syntax/metadata slice and the fresh per-use direct
constructor scheme slice have both landed. Ordinary binding generalization,
generic constructor pattern typing, class/defaulting solver behavior, explicit
type application, and runtime dispatch changes remain outside the active
subset.

## Current Active-Path Status

1. `jazz-next` implements canonical surface and lowered `data` declarations
   with constructor arity metadata, plus lowercase generic type parameters
   preserved in declaration metadata.
2. `jazz-next` implements constructor values and ordinary constructor
   application with first-order typing and runtime constructor values. Generic
   constructor values/applications instantiate declaration-owned type
   parameters freshly at each direct constructor use.
3. Declared constructor patterns typecheck against ADT scrutinees, bind
   payload variables in arm bodies, and reject unknown or arity-mismatched
   constructor patterns with deterministic `E2011` diagnostics.
4. The active parser/core path accepts constructor, bracketed-list,
   cons-like list, tuple, and `name @ pattern` as-patterns in `case` arms and
   lowers them into `EPatternCase`.
5. Bracketed-list patterns typecheck against list scrutinees, bind element
   variables in arm bodies, and match exact-length runtime lists.
6. Constructor patterns match saturated runtime constructor values with the
   same constructor name and payload count.
7. Runtime constructor over-application emits deterministic `E3023`
   diagnostics with the constructor name plus expected and received arity.
8. The end-to-end runtime-executed `case` subset is defined in
   `docs/spec/pattern-matching-semantics.md`.
9. Tuple literal values, concrete tuple signature types, and fixed-arity tuple
   case patterns are active core runtime/type features in `jazz-next`.
10. Cons-like list patterns such as `[head | tail]` are active `case` patterns:
    the head subpattern matches the first list element, the tail subpattern
    matches the remaining list, and empty lists fall through to later arms.
11. Pattern-shaped lambda parameters reuse the active pattern-matching contract
    through internal single-arm pattern cases.
12. As-patterns delegate to the inner pattern first, bind the whole scrutinee
    value on success, give the as-binder the scrutinee type, and reject
    duplicate binders with deterministic `E2011` diagnostics.

## ADT Contract

1. A `data` declaration introduces one nominal type constructor and a closed set
   of value constructors.
2. Type names and constructor names use uppercase identifiers.
3. Constructor payloads are positional; field order is semantically relevant.
4. Constructors are first-class values in expression position.
5. Constructor application uses ordinary function application semantics rather
   than a special call form.
6. Applying a constructor beyond its declared arity is invalid; runtime-only
   invalid paths emit deterministic `E3023` diagnostics.
7. Pattern matching over constructor values is defined by
   `docs/spec/pattern-matching-semantics.md`.

Canonical shape example:

```jz
data Maybe = Just value | Nothing.

some = Just 1.
none = Nothing.
```

Generic declaration shape:

```jz
data Maybe a = Nothing | Just a.
data Pair a b = Pair a b.
```

In a generic `data` declaration, lowercase identifiers after the type
constructor introduce type parameters scoped to that declaration. Duplicate type
parameters reject deterministically. Lowercase constructor payload identifiers
that appear in a generic declaration must refer to those parameters. Direct
constructor value/application uses instantiate fresh type parameters, and
repeated occurrences of the same payload type parameter are linked within a
single constructor application. Ordinary bindings remain monomorphic: binding a
generic constructor value to a user name does not generalize that alias.

## Staged First Slice

1. The first executable ADT slice in `jazz-next` is limited to plain sum types
   with positional constructors.
2. Constructor type syntax must align with the active type-grammar rebase work
   before the full surface is executable.
3. Built-in lists remain separately implemented runtime values; user ADTs do
   not redefine list semantics in the first slice.
4. Bracketed list patterns share the active parser/core pattern syntax and
   now have active-path type/runtime semantics for exact-length list shapes.
   Cons-style bracketed list patterns now have active-path type/runtime
   semantics for non-empty head/tail list deconstruction.
5. Tuple values are implemented as core runtime values outside the ADT slice,
   and tuple case patterns are implemented through the shared pattern-matching
   subset for fixed-arity tuple scrutinees.

## Decision Cross-References

- Canonical general syntax rules remain in `docs/spec/authoritative-syntax.md`.
- The committed active-path `case` subset is defined in
  `docs/spec/pattern-matching-semantics.md`.
- `if` continues to desugar through the existing boolean-only `ECase` contract
  defined in `docs/spec/control-flow/if-expressions.md`.

## Non-Goals

1. GADTs, existential constructors, or record/named-field constructors.
2. Infix constructors or alternate constructor call syntax.
3. Automatic deriving or trait/class synthesis for user ADTs.
4. Tuple-constructor sugar or pattern features beyond the committed
   constructor/list/tuple/as-pattern subset such as guards, or-patterns, and
   pattern synonyms.
5. Generalized user binding polymorphism, class/defaulting solver behavior,
   explicit type application, or runtime dispatch as part of the first generic
   ADT constructor scheme slice.
