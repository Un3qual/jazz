---
title: Capabilities
description: Understand Jazz class constraints, concrete implementations, and current dispatch limits.
sidebar_position: 9
---

A capability states which operations a type must provide. A constrained
function can use those operations without committing to one concrete type. An
implementation connects the capability to a concrete type.

Fragment:

<!-- jazz-example: fragment -->

```jazz
class Equal(a) {
  equal :: a -> a -> Bool.
}.

impl Equal(Int) {
  equal = \(left, right) -> left == right.
}.
```

Capability bodies currently contain method signatures, and implementation
targets must be concrete. The compiler rejects duplicate capabilities or
implementations, incorrect arity, an unsatisfied constraint, and ambiguous
concrete implementations.

The bundled capabilities are `Eq`, `Ord`, `Num`, `Integral`, `Fractional`,
`Showable`, and `Default`. Their methods and built-in implementations are
documented in [Prelude](../standard-library/prelude.md).

**Partial:** an explicit `Class::method` reference can dispatch when exactly one
visible concrete implementation applies. Cross-module method visibility,
user-visible dictionaries, default methods, superclasses, and a general overlap
policy are not implemented.

See the [expression grammar](../reference/expression-grammar.md) for declaration
and constraint notation.
