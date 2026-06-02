# Qualified Imports and Name Binding

Status: active qualified import contract
Primary plan: `docs/plans/spec-clarification/2026-03-02/modules/09-module-loader-and-import-resolution.md`
Depends on:

- `docs/spec/modules/01-file-layout-and-package-roots.md`
- `docs/spec/modules/02-resolution-algorithm-and-cycles.md`
- `docs/spec/modules/03-loader-behavior-and-diagnostics.md`

## Scope

This document defines v1 import binding behavior for active `jazz-next` module-graph compilation and execution. It documents existing parser, resolver, and loader behavior without introducing new compiler behavior.

Future package metadata, additional import forms, export lists, and migration policy remain separate module spec slices.

## Import Forms

V1 supports three import forms:

| form | unqualified visibility | qualified visibility |
| --- | --- | --- |
| `import Foo::Bar.` | all exported value names | none |
| `import Foo::Bar (map, filter).` | only listed exported names | none |
| `import Foo::Bar as Bar.` | none | exported names through `Bar::name` |

Alias and symbol-list forms are mutually exclusive. Both `import Foo::Bar as Bar (x).` and `import Foo::Bar (x) as Bar.` are parser errors.

Symbol lists must be non-empty and cannot repeat a symbol in the same list. Duplicate symbols inside one list are parser errors before module resolution.

Import aliases are parsed as identifiers. Reserved literal names cannot be aliases. Lowercase aliases and abstraction-vocabulary names such as `class` are accepted as ordinary alias identifiers.

## Export Inventory

The resolver validates imports against each dependency module's top-level exported value names.

V1 exports are:

- top-level value binding names;
- top-level `data` constructor names.

Module declarations, import declarations, signatures, data type names, class declarations, and impl declarations are not exported value names for import binding.

## Bare Imports

A bare import exposes every exported value from the imported module as an unqualified name in the importer.

Example:

```jazz
import Lib::Math.

main = subtract.
```

If `Lib::Math` exports `subtract`, the unqualified reference is import-visible.

Bare imports do not create an alias. `Lib::Math::subtract` is not a v1 lookup form, and `Math::subtract` requires an explicit `as Math` import.

## Symbol-List Imports

A symbol-list import exposes only the listed exports as unqualified names.

Example:

```jazz
import Lib::Math (add).

main = add.
```

If a requested symbol is not exported by the imported module, resolution fails with `E4007`.

If two symbol-list imports request the same unqualified symbol, resolution fails with `E4008`.

Example:

```jazz
import A::Ops (map).
import B::Ops (map).
```

Unlisted exports from the imported module are hidden from unqualified lookup. If the importing module references an unlisted export and no local, prelude, or other visible import binding provides the same name, resolution fails with `E4011`.

Pattern constructor references follow the same rule as expression references. A constructor hidden by a symbol list is not visible in a pattern unless another visible binding supplies that constructor name.

## Alias Imports

An alias import exposes no exports as unqualified names. Exported names are available only through `Alias::name`.

Example:

```jazz
import Lib::Math as Math.

main = Math::subtract.
```

Alias imports can be referenced before the import declaration in the same module because the parser and resolver collect module-level alias declarations before validating qualified references.

If the same alias is used for two imported modules in one importing module, resolution fails with `E4009`.

If an unqualified reference names an export that is only available through an alias import, and no local, prelude, or other visible import binding provides the same name, resolution fails with `E4012`.

If a qualified reference uses an undeclared alias, resolution fails with `E4013`.

If a qualified reference names a member not exported by the aliased module, resolution fails with `E4014`.

Alias imports support data constructors as qualified members:

```jazz
import Lib::Maybe as Maybe.

main = Maybe::Just 1.
```

Alias-qualified data constructor values preserve their source module's internal
ADT identity during replay. A value built by `Alias::Box` does not type-unify
with a local `Box` declaration solely because the constructor/type names match.

## Namespaces and Shadowing

Unqualified value lookup and qualified alias lookup are separate namespaces.

Current-module binders are not treated as import references. A local `let` binding with the same name as an imported export takes precedence for resolver visibility checks.

The alias namespace is not shadowed by local value binders. A module can contain a local binding named `math` and an import alias named `math`; `math` is an unqualified value reference, while `math::subtract` is a qualified alias reference.

Prelude and other ambient visible symbols participate in hidden-import checks. If an alias-only or unlisted import export has the same name as a visible prelude binding, an unqualified reference can resolve to the prelude binding instead of failing with `E4011` or `E4012`.

Bare imports and symbol-list imports contribute visible unqualified names. Alias imports do not.

## Diagnostics

Import binding diagnostics are deterministic:

| code | condition |
| --- | --- |
| `E4007` | symbol-list import requests a name not exported by the imported module |
| `E4008` | two symbol-list imports request the same unqualified symbol |
| `E4009` | two alias imports declare the same alias in one importing module |
| `E4011` | unqualified reference targets an export hidden by a symbol-list import |
| `E4012` | unqualified reference targets an export available only through an alias import |
| `E4013` | qualified reference uses an undeclared alias |
| `E4014` | qualified reference uses a declared alias but the member is not exported |

Diagnostics include importer module context, imported module context when applicable, and the relevant symbol or alias. Collision diagnostics carry primary and related spans for the current and previous import declarations.

## Truth Table

| case | result |
| --- | --- |
| `import Lib::Math.` then `subtract` | allowed if `Lib::Math` exports `subtract` |
| `import Lib::Math (add).` then `add` | allowed if `Lib::Math` exports `add` |
| `import Lib::Math (subtract).` but only `add` is exported | `E4007` |
| `import A::Ops (map).` and `import B::Ops (map).` | `E4008` |
| `import Lib::Math (add).` then `subtract` | `E4011` unless another visible binding supplies `subtract` |
| `import Lib::Math as Math.` then `Math::subtract` | allowed if `subtract` is exported |
| `import Lib::Math as Math.` then `subtract` | `E4012` unless another visible binding supplies `subtract` |
| `import A::Ops as Ops.` and `import B::Ops as Ops.` | `E4009` |
| `Math::subtract` with no `as Math` import | `E4013` |
| `import Lib::Math as Math.` then `Math::subtract` when only `add` is exported | `E4014` |
| local binding `math` plus `import Lib::Math as math` | allowed; `math` and `math::name` are different namespaces |

Implementation evidence (2026-05-30): `ModuleResolutionSpec.hs` now locks the
active `jazz-next` harness for the v1 import truth table, including bare import
unqualified visibility, symbol-list visibility and hidden-export diagnostics,
local binding precedence over hidden imports, alias references before alias
declarations, local value names sharing alias identifiers, data-constructor
imports, and the `E4007`/`E4008`/`E4009`/`E4011`/`E4012`/`E4013`/`E4014`
diagnostic contexts and metadata exposed by the resolver.

## Non-Goals

This qualified import slice does not define:

- export lists or explicit module export declarations;
- qualified full-module-path lookup such as `Lib::Math::subtract`;
- wildcard import modifiers or import hiding syntax;
- re-export behavior;
- package-level aliasing;
- compatibility or deprecation windows for historical module syntax.
