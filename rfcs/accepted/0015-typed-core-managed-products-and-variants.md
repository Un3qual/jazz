# RFC 0015: Typed-core managed products and variants

Status: Accepted
Date: 2026-08-27
Supersedes: None.

## Decision

Jazz will extend the opt-in Typed Core and backend-neutral Lowered IR profile
with non-unit tuple and local algebraic-data construction, transport, and
ordered pattern-case lowering.

Inference remains the sole semantic owner. Production reuses the existing
typed data declarations, tuple expressions, resolved constructor application
spines, product and variant recipes, patterns, and case arms. Local generic
data declarations retain their ordered type parameters, constructors, fields,
interfaces, and export facts. Constructor construction is supported only when
the local constructor is exactly saturated; nullary constructors are already
complete values. Bare or partial non-nullary constructors remain outside the
profile.

Lowered IR version 1 continues to use managed references plus its existing
product and variant layouts, construction operations, field and tag
projections, and tagged switch terminator. Product layouts have structural
identity from their ordered concrete field recipes. Variant layouts have
nominal identity from the current module path, resolved data type name, and
ordered concrete type arguments. Identities use a versioned length-prefixed
semantic encoding and cannot depend on absolute paths, spans, hashes, map
iteration order, pointers, or target layout.

Variant constructor tags are zero-based declaration indices. Concrete fields
are specialized from the retained declaration using the constructor
instantiation. Recursive and mutually recursive layouts reserve identity on
first discovery and may refer to each other. Required layouts are deduplicated
and emitted after catalog-owned runtime layouts and before closure environment
layouts, in first semantic discovery order.

Construction evaluates tuple elements and constructor fields exactly once from
left to right. Products and variants may cross every binding, callable,
closure, capture, control-flow, return, and tail-operand boundary already
supported for managed `Text`. Fields may contain supported scalars, `Text`,
closures, products, or variants. Lists and unresolved representation
parameters remain rejected.

Managed pattern cases support wildcard, variable, immediate scalar literal,
constructor, tuple, as-pattern, and top-level or-pattern forms, including
nested constructor and tuple patterns. Selection retains source order. Tags
are tested before variant fields are projected; tuple fields are projected
from the exact product layout. Pattern binders carry projected operands only
into the selected guard and body. False guards and nested pattern failures
continue to later source arms.

The lowerer independently proves totality for the admitted managed pattern
subset. Unguarded rows contribute coverage and guarded rows do not. Complete
closed constructor sets need no synthetic wildcard. Incomplete arbitrary Typed
Core fails with `LoweredIRIncompletePatternCase`; no trap, unreachable value,
or match-failure runtime service is added.

Delivery is ordered. The first implementation child owns local data retention,
tuple and exactly saturated constructor production, canonical layout
collection, construction, and transport. A later child owns managed pattern
decision trees and projections. Only one child is promoted at a time.

Ordinary `compile` and `run` remain on canonical core and the reference
interpreter. Public language behavior, Typed Core and Lowered IR schemas,
mirrored validators, and Lowered IR version 1 remain unchanged.

## Implementation status

The first ordered child completed on 2026-08-27. The opt-in
`inferResolvedModuleTypedCoreExpressionDirectCall` producer now retains
non-unit tuples and exactly saturated local constructors, and
`lowerTypedCoreExpressionDirectCall` emits their deterministic product and
variant layouts plus `LoweredConstructProduct` and `LoweredConstructVariant`.
Construction is left-to-right and exactly once. The resulting managed
references cross bindings, direct and closure parameters and results, calls and
tail calls, lexical and recursive captures, conditional and scalar-case joins,
and returns.

The second ordered child completed on 2026-08-29. The same opt-in producer and
lowerer now support wildcard, variable, immediate scalar literal, constructor,
tuple, as-, and top-level or-pattern cases over managed products and variants,
including nested constructor and tuple patterns. The lowerer independently
proves totality from admitted Typed Core: guarded rows do not cover, complete
local constructor sets need no synthetic wildcard, and incomplete arbitrary
Typed Core fails with `LoweredIRIncompletePatternCase` before emission. Decision
trees preserve source arm order, test a variant tag before any field projection,
and carry matched binders only into the selected guard and body.

Lists and cons, Text literal patterns, nested or-patterns, pattern lambdas,
imported or multi-module data, product/variant equality, runtime ABI/native
execution, and ordinary compile/run cutover remain excluded. The second child
does not change Typed Core or Lowered IR schemas, validators, runtime services,
or the ordinary canonical-core/reference-interpreter path.

## Context

Typed Core already represents data declarations, products, variants, tuple and
constructor values, and the complete active pattern surface. Lowered IR already
represents product and variant layouts, managed construction, projections, and
switches, and both Haskell and hosted Jazz validators already enforce those
contracts. Before the first implementation child, the opt-in producer and
lowerer made only unit tuples reachable and rejected every ADT value.

Products and variants are the next coherent managed-data boundary after
managed `Text`. They materially advance the ADT-heavy Jazz-authored compiler
without a new runtime service or ABI. Lists are not folded into this decision:
the current IR constructs homogeneous lists but has no list projection
operation, so list inspection requires an independent representation or
runtime-service contract.

Source pattern coverage is already strict, but lowering must also be safe for
independently constructed Typed Core. The retained local data declarations and
concrete pattern recipes let the lowerer build and validate a total decision
tree without trusting an unrecorded source-analysis proof or introducing a
runtime match-failure path.

## Consequences

- Non-unit tuples and local ADT values become the second managed-data family in
  the bounded backend profile.
- Concrete generic and recursive ADTs receive stable semantic layout
  identities without fixing target memory layout or ownership.
- Exactly saturated constructors lower directly to variant construction rather
  than synthetic functions.
- Ordered managed cases reuse existing IR projections, branches, switches,
  block parameters, joins, and tail terminators.
- Complete constructor cases can lower without an artificial wildcard while
  malformed incomplete Typed Core still fails closed.
- No Typed Core or Lowered IR schema, version, hosted validator, runtime
  service, host callback, or native symbol changes.
- Lists, list patterns, `Text` literal patterns, product/variant equality,
  first-class constructors, pattern lambdas, imported data, multi-module
  lowering, runtime ABI, native execution, and ordinary compile/run cutover
  require separate contracts.
