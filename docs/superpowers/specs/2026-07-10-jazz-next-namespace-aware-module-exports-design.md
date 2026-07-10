# Jazz-Next Namespace-Aware Module Exports Design

## Status

Implemented and verified on `2026-07-10`.

## Goal

Let module authors select an exact export namespace when a type, constructor,
value, or class shares a source name, while preserving the existing bare-name
export syntax as a compatibility shorthand.

## Source Contract

Explicit module export lists accept four namespace prefixes:

```jazz
module Lib::Box (
  type Box,
  constructor Box,
  value Box,
  class Printable
) {
  data Box = Box payload.
  Box = 1.
  class Printable(a) {
    show :: a -> Int.
  }.
}
```

The prefixes map directly to the compiler's typed export namespaces:

| source prefix | inventory namespace |
| --- | --- |
| `value` | `ValueNamespace` |
| `constructor` | `ConstructorNamespace` |
| `type` | `TypeNamespace` |
| `class` | `CapabilityNamespace` |

A prefixed selector publishes only the exact typed entry. Multiple prefixed
selectors may therefore use the same source name in one list.

The existing bare form remains valid:

```jazz
module Lib::Box (Box) { ... }
```

A bare selector preserves the current rule and publishes every owned typed
entry with that text. An omitted list still exports every owned declaration,
and `()` still exports nothing.

Import syntax is unchanged. Explicit import lists remain text-based and retain
their current selector eligibility and same-text retention rules.

## Parsing and Representation

`JazzNext.Compiler.ModuleExports` owns a structured source selector:

```haskell
data ModuleExportSelector = ModuleExportSelector
  { moduleExportSelectorNamespace :: Maybe NameNamespace,
    moduleExportSelectorName :: Text
  }
```

`Nothing` represents a compatibility bare selector. `Just namespace`
represents an explicit prefix. Parser surface statements, lowered module
metadata, and resolver validation carry `[ModuleExportSelector]`; no phase
encodes a namespace into a `Text` sentinel.

`value`, `constructor`, `type`, and `class` remain identifier tokens and keep
their existing meanings outside module export lists. Inside an export list,
each is treated as a prefix only when it is followed by another identifier.

Duplicate detection uses the structured selector identity. Repeating
`type Box` is invalid, while `type Box, constructor Box` is valid. A bare
`Box` and a prefixed `type Box` may coexist but are redundant; selection is
idempotent and this first slice does not add a separate redundancy diagnostic.

## Inventory Selection

The shared export inventory exposes a focused selector function:

```haskell
selectModuleExportSelectors ::
  [ModuleExportSelector] ->
  ModuleExportInventory ->
  ModuleExportInventory
```

For a bare selector, the function retains every entry whose name matches. For
a prefixed selector, it retains only the `ModuleExport` with the requested
namespace and name. Existing `selectExportNames` and `visibleImportInventory`
remain the import-side APIs and do not change behavior.

## Validation and Diagnostics

The resolver validates each structured selector against declarations owned by
the defining module:

- a bare selector is valid when any owned namespace has that text;
- a prefixed selector is valid only when the exact typed entry exists; and
- imported-only declarations remain ineligible.

Failures retain diagnostic code `E4015`, the module declaration span, and the
selector name as the diagnostic subject. Prefixed failures identify the source
namespace in the message, for example:

```text
module export type 'Box' is not declared by module 'Lib::Box'
```

Available-declaration output renders typed entries deterministically when an
exact selector fails, so a missing `type Box` can still show an available
`constructor Box` or `value Box`.

## Compiler and Runtime Boundaries

No compiler-interface or runtime API changes are required after resolution.
Both already consume the public `ModuleExportInventory`. Exact namespace
selection changes the contents of that inventory, and existing compiler/runtime
filtering applies it without reconstructing source selectors.

## Compatibility

This is an additive syntax change:

- all existing module headers remain valid;
- bare selectors keep their same-text expansion;
- import syntax and diagnostics `E4007` through `E4014` remain unchanged;
- `E4015` remains the module-export validation code; and
- no new reserved words are introduced outside module export lists.

## Test Contract

Focused coverage must prove:

- parser and lowering preserve bare and all four prefixed selectors;
- same-name prefixed selectors are accepted and exact duplicates are rejected;
- inventory selection distinguishes type, constructor, value, and capability;
- a prefixed selector rejects a same-name entry in the wrong namespace;
- bare selectors preserve the current all-same-text behavior;
- the resolved public inventory contains only explicitly selected namespaces;
- runtime publication excludes a same-name constructor when only the value is
  selected, and vice versa; and
- existing parser, resolver, compiler, loader, and runtime suites remain green.

## Non-Goals

This child does not add namespace prefixes to import lists, constructor-group
syntax, wildcard exports, re-exports, body-level export declarations,
visibility modifiers, alias-qualified classes, package semantics, or changes
to local name-resolution precedence.
