# Standard Library Boundary

Status: active (closure verified for the current `jazz-next` runtime subset; bundled/explicit prelude paths expose public aliases while no-prelude paths remain kernel-bridge-only)
Locked decisions (initial `jazz-next` contract): 2026-03-04
Updated: 2026-06-27
Primary plan: `docs/plans/2026-03-18-jazz-next-runtime-architecture-and-interpreter-execution-plan.md`

## Purpose

Define ownership boundaries for standard-library symbols so compiler, typechecker,
and runtime behavior stay consistent while migration to a self-hosted prelude is
planned.

## Implementation Target

- New boundary implementation work lands in `jazz-next/`.
- `jazz-hs/` and `jazz2/` are legacy evidence only.

## Ownership Model (Current Contract)

1. `kernel` symbols are compiler/runtime owned and may be hardcoded.
2. `prelude` symbols are user-visible APIs and are prelude-owned by default.
3. `jazz-next` now supports a bundled default prelude load path in CLI and
   driver-default source/module-graph modes:
   - resolution order: `--prelude` flag > `JAZZ_PRELUDE` env > bundled default path.
   - `--no-prelude` disables all prelude loading.
4. Public names such as `map`, `print!`, and numeric conversion aliases are
   available only through bundled or explicit prelude aliases; direct
   no-prelude flows must reference the `__kernel_*` bridge names.
5. `compileSource`, `runSource`, `compileModuleGraph`, and `runModuleGraph`
   load the bundled prelude by default. Explicit no-prelude entry points
   (`compileSourceWithPrelude Nothing`, `runSourceWithPrelude Nothing`,
   `compileModuleGraphWithPrelude Nothing`, `runModuleGraphWithPrelude Nothing`,
   `--no-prelude`, and low-level AST/runtime helpers) are kernel-only. The
   no-prelude module graph harness covers both compile and run paths across
   imported modules.

## Kernel Catalog (Current `jazz-next` Runtime Subset)

| Symbol | Arity | Type Contract | Current Owner | Migration Target |
| --- | --- | --- | --- | --- |
| `map` | `2` | `(a -> b) -> [a] -> [b]` | `__kernel_map` bridge + bundled prelude alias (`map = __kernel_map`) | prelude-owned public API; kernel bridge retained for explicit no-prelude paths |
| `filter` | `2` | `(a -> Bool) -> [a] -> [a]` | `__kernel_filter` bridge + bundled prelude alias (`filter = __kernel_filter`) | prelude-owned public API; kernel bridge retained for explicit no-prelude paths |
| `hd` | `1` | `[a] -> a` | `__kernel_hd` bridge + bundled prelude alias (`hd = __kernel_hd`) | prelude-owned public API; kernel bridge retained for explicit no-prelude paths |
| `tl` | `1` | `[a] -> [a]` | `__kernel_tl` bridge + bundled prelude alias (`tl = __kernel_tl`) | prelude-owned public API; kernel bridge retained for explicit no-prelude paths |
| `print!` | `1` | `a -> a` (stub-v1) | `__kernel_print!` bridge + bundled prelude alias (`print! = __kernel_print!`) | prelude-owned impure public API; kernel bridge retained for explicit no-prelude paths |

## Bundled Prelude Contract

- Checked-in bundled prelude mirror: `jazz/stdlib/Prelude.jz`, relative to the
  `jazz-next` package root.
- Active tests resolve that mirror through the shared, role-aware
  `JazzNext.TestSource` package-root loader rather than a repository-relative
  compiler constant.
- Current bridge declarations use the `__kernel_` prefix, remain kernel
  self-bridges, and satisfy `PreludeContract` validation.
- Current bundled exports must alias the matching `__kernel_*` bridge names.
- Current bundled exports/aliases are:
  - `map`, `filter`, `hd`, `tl`, `print!`
  - `toInt8`, `toInt16`, `toInt32`, `toInt64`
  - `toUInt8`, `toUInt16`, `toUInt32`, `toUInt64`
  - `toFloat16`, `toFloat32`, `toFloat64`
  - `toInt` as an ordinary source alias for `toInt64`
  - `toFloat` as an ordinary source alias for `toFloat64`
- Current bundled capability declarations include canonical `Eq`, `Ord`, `Num`,
  `Integral`, `Fractional`, `Showable`, and `Default` classes plus inert
  concrete impl facts for the default aliases `Int`, `Float`, and `Bool`, and
  for width-specific numeric signature names `Int8`, `Int16`, `Int32`, `Int64`,
  `UInt8`, `UInt16`, `UInt32`, `UInt64`, `Float16`, `Float32`, and `Float64`.
  Explicit-prelude and no-prelude entry points do not inherit those bundled
  capability facts.
- Catalog ownership metadata (`PreludeTarget` vs future intrinsic-only entries)
  is declared in `jazz-next/src/JazzNext/Compiler/BuiltinCatalog.hs`.

Explicit numeric conversions:

- Conversion APIs such as `toInt8`, `toUInt64`, and `toFloat64` are
  prelude-owned public names backed by catalog/kernel bridge symbols.
- Default conversion aliases `toInt` and `toFloat` are ordinary bundled-prelude
  source bindings for `toInt64` and `toFloat64`; they do not add builtin catalog
  entries, target semantics, or `__kernel_toInt`/`__kernel_toFloat` bridges.
- Direct no-prelude flows may use only the corresponding `__kernel_*` bridge
  names.
- Conversion API changes must update the builtin catalog, bundled prelude
  generation/mirror, prelude contract validation, primitive type/runtime
  semantics, and no-prelude/prelude visibility tests together.

## Decision Cross-References

This boundary contract is constrained by previously locked language decisions:

- Canonical syntax and module/import forms:
  `docs/spec/authoritative-syntax.md`
- Function-first collection combinator order (`map f xs`, `filter p xs`):
  `docs/spec/runtime/primitive-semantics.md`
- Stub-v1 purity enforcement and `print!` effect boundary:
  `docs/spec/semantics/purity-bang-stub-v1.md`

## Intrinsic Bridge Contract

`jazz-next/src/JazzNext/Compiler/BuiltinCatalog.hs` is the single source of truth
for builtin symbol identity, names, and arities.

Required invariants:

1. Analyzer direct builtin visibility must derive from the shared catalog's
   `__kernel_*` bridge names only.
2. Type inference builtin type instantiation must dispatch by shared catalog
   symbol, not duplicated string lists.
3. Runtime builtin lookup/arity must dispatch by shared catalog symbol and only
   through direct `__kernel_*` bridge references.
4. Tests must fail if catalog entries drift from compile/runtime behavior.
5. Tests must fail if the checked-in bundled prelude mirror drifts from the
   catalog-generated bridge and alias source.

## Compatibility Window Policy

1. Current default mode is `prelude-owned`: bundled and explicit-prelude
   compile/run paths, including default module graph helpers, resolve public
   helpers through prelude aliases.
2. Explicit no-prelude mode is `kernel-only`: `--no-prelude`, `Nothing`
   prelude driver entry points, and low-level AST/runtime helpers resolve only
   the `__kernel_*` bridge names.
3. Canonical public aliases (`map`, `filter`, `hd`, `tl`, `print!`, and the
   numeric conversion aliases listed above) are rejected in no-prelude mode and
   require a real prelude source.
4. This ownership-boundary migration is closed for the current runtime subset.
   Future stdlib growth should extend the prelude/catalog intentionally under
   new queue items with concrete API/runtime contracts rather than reopening
   direct public builtin fallback; no future stdlib/catalog API is promoted by
   the current closure.
5. Active coverage lives in `PreludeLoadingSpec.hs`, `BuiltinCatalogSpec.hs`,
   and `LoaderSpec.hs`: source helpers cover no-prelude alias rejection,
   catalog coverage locks bridge/alias generation, and module graph coverage
   locks bundled, explicit, and no-prelude behavior across imports.

## Non-Goals (Current Phase)

1. This document does not define broader package discovery or module-root
   layout semantics beyond the current resolver configuration.
2. This document does not define full stdlib API surface beyond the active
   kernel subset.
3. This document does not guarantee immediate removal dates for kernel aliases.
