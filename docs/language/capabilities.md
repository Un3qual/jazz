---
title: Capabilities
description: Declare generic capabilities, constrained functions, superclasses, and default methods.
sidebar_position: 9
---

A capability describes operations a type provides. Each class has one parameter,
whose kind is inferred from its methods and fixed at declaration. Methods are
ordinary constrained values: `same`, `Same::same`, and an imported
`Alias::Same::same` refer to the same declaration. They can be stored, partially
applied, passed to functions, or shadowed in a nested scope. Two declarations
cannot introduce the same ordinary value name in one scope.

Fragment:

<!-- jazz-example: fragment -->

```jazz
class Same(a) {
  same :: a -> a -> Bool.
  different :: a -> a -> Bool.
  different = \(left, right) -> not (same left right).
}.

impl Same(Int) {
  same = \(left, right) -> left == right.
}.

impl @{Same(a)}: Same([a]) {
  same = \(left, right) -> case (left, right) {
    | ([], []) -> True
    | ([x | xs], [y | ys]) -> if same x y then same xs ys else False
    | _ -> False
  }.
}.

check = \(left, right) -> different left right.
```

`check` infers the constraint needed by its method call. Signed helpers declare
that constraint with `@{Same(a)}: a -> a -> Bool`. Constraints survive stored
aliases and module exports. Expected argument and result types participate in
selection; an unresolved target is ambiguous. Empty collections do not choose
an element type. Numeric literals retain their ordinary defaulting rules.

A class may take a type constructor instead of a complete type. For example,
`class Transforming(f) { transform :: (a -> b) -> f(a) -> f(b). }.` infers
`f :: Type -> Type`. The method's `a` and `b` are independently quantified at
each use. Explicit application selects the class parameter first, as in
`transform @List`. Method names inside an implementation retain their overloaded
types, so a recursive list implementation can call the same method on both an
element and the remaining list.

Implementation heads accept concrete types and constructor-headed generic
patterns whose immediate arguments are distinct variables, such as `[a]`,
`Box(a)`, or a partial `Result(error)`. Bare-variable catchalls, function heads,
and nested specialized generic patterns are rejected. A prerequisite applies a
class to one variable bound by the head. Every body must work for all its head
variables and all method-local variables. Overlapping heads are rejected even
when their prerequisites differ; the compiler selects a unique head before
checking its prerequisites.

A superclass uses the same constraint prefix:

Fragment:

<!-- jazz-example: fragment -->

```jazz
class @{Same(a)}: Ordered(a) {
  before :: a -> a -> Bool.
}.
```

Superclass relationships must be acyclic and refer to visible earlier class
declarations. Every `Ordered(t)` implementation must supply `Same(t)` evidence,
even when no method is called. A helper constrained by `Ordered(a)` can use
`Same(a)` operations directly.

A default body follows its method signature in the class. It is checked once
with the class's assumptions and retains the lexical scope where the class was
defined. An implementation may override it; every method without a default
requires an explicit body. Default and supplied bodies use ordinary expression,
recursion, and purity rules.

Importing a module makes its transitive implementation environment available,
including implementations of classes owned by another module. Aliases and name
selections govern visible names; repeated imports retain one implementation
identity. Selecting `class Same` includes its ordinary methods, while selecting
`value same` can expose only that value. See [modules](modules.md) for examples.

The bundled capabilities and their implementations are documented in
[Prelude](../standard-library/prelude.md). User-visible dictionaries,
multi-parameter classes, associated types, and overlapping implementations are
outside the language. See the [expression grammar](../reference/expression-grammar.md)
for declaration and constraint notation.
