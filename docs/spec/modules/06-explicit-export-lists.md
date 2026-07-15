# Explicit Module Export Lists

Status: active explicit module export contract

## Syntax

`module Foo::Bar (name, type Type, type Choice(..), type Result(Ok),
constructor Constructor, value binding, class Class) { ... }` is an allowlist.
An omitted list exports every owned declaration. `()` exports none.

`value`, `constructor`, `type`, and `class` are contextual prefixes inside an
export list. They remain ordinary identifiers elsewhere.

## Namespace Rules

Prefixed selectors publish only the requested namespace. For example,
`type Box` publishes the type identity without its constructors, while
`type Box(..)` publishes the type identity and every constructor owned by it,
and `type Box(Pack, Empty)` publishes the identity plus only the named owned
constructors. Constructor groups are export-side syntax only; each group is
validated against declarations owned by the module and then expanded into the
same flat typed export inventory consumed by compiler and runtime boundaries.

The existing individual forms remain available:
`constructor Box` publishes only that constructor and `value Box` publishes
only the ordinary binding. `class Eq` publishes the class capability and its
attached public method/impl payload.

Existing bare selectors remain compatibility shorthand: bare `Box` selects
every owned same-text typed entry. Same-name prefixed selectors such as
`type Box, constructor Box` are valid together. Repeating the same structured
selector is a parser error.

## Ownership and Re-exports

Only declarations owned by the module may be listed. Imported declarations
are not eligible and produce `E4015`; re-exports are not part of this contract.

## Local and Public Visibility

Unlisted declarations remain available inside the module for inference and
runtime evaluation. Downstream resolver, compiler, and runtime boundaries see
only the public inventory.

## Diagnostics

Malformed and duplicate list syntax uses `E0001`. Unknown, wrong-namespace, or
imported-only exports use `E4015`; wrong-namespace diagnostics retain the
requested prefix. An invalid grouped type points at the type name; an unknown,
imported, or wrong-owner selected constructor points at that constructor.
Downstream import diagnostics remain `E4007`-`E4014`.

## Non-Goals

No import-side namespace prefixes or grouping, body export declarations,
visibility modifiers, re-exports, cross-module operators, packages, default
methods, superclasses, or effect typing.
