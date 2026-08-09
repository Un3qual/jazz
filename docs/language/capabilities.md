---
title: Capabilities
description: Understand Jazz class constraints, concrete implementations, and current dispatch limits.
sidebar_position: 9
---

Jazz uses `class` declarations and concrete `impl` declarations for
capabilities. The bundled vocabulary is `Eq`, `Ord`, `Num`, `Integral`,
`Fractional`, `Showable`, and `Default`.

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

Class parameters are explicit lowercase type variables. Class bodies currently
contain method signatures, and impl targets must be concrete. Constrained
signatures use `@{Class(Type)}:`. Duplicate declarations, wrong arity, missing
facts, and ambiguous concrete implementations are diagnosed.

**Partial:** explicit `Class::method` references can dispatch to exactly one
visible concrete implementation after the method body is type-checked. Broader
cross-module method visibility, user-visible dictionaries, default methods,
superclasses, and general overlap policy are not implemented.

Jazz does not accept an alternate trait declaration keyword. Future semantic
changes require the [governance process](../project/governance.md).
