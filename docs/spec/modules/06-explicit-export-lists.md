# Explicit Module Export Lists

Status: active explicit module export contract

## Syntax

`module Foo::Bar (name, Type, Constructor, Class) { ... }` is an allowlist.
An omitted list exports every owned declaration. `()` exports none.

## Namespace Rules

Values, constructors, types, and capabilities are export-selectable. One text
selects every same-text typed entry. Types and constructors are independent,
so a type-only export is opaque and a constructor may be exported separately.

## Ownership and Re-exports

Only declarations owned by the module may be listed. Imported declarations
are not eligible and produce `E4015`; re-exports are not part of this contract.

## Local and Public Visibility

Unlisted declarations remain available inside the module for inference and
runtime evaluation. Downstream resolver, compiler, and runtime boundaries see
only the public inventory.

## Diagnostics

Malformed and duplicate list syntax uses `E0001`. Unknown or imported-only
exports use `E4015`. Downstream import diagnostics remain `E4007`-`E4014`.

## Non-Goals

No wildcard or constructor-group shorthand, body export declarations,
visibility modifiers, re-exports, cross-module operators, packages, default
methods, superclasses, or effect typing.
