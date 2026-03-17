# Standard Library Boundary

Status: active (phase 4 public-builtin migration implemented; phase 5 kernel compatibility cleanup pending)
Locked decisions (initial `jazz-next` contract): 2026-03-04
Updated: 2026-03-16
Primary plan: `docs/plans/spec-clarification/2026-03-02/stdlib/10-stdlib-boundary-selfhosted-vs-hardcoded.md`

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
3. `jazz-next` now supports a bundled default prelude load path in CLI mode:
   - resolution order: `--prelude` flag > `JAZZ_PRELUDE` env > bundled default path.
   - `--no-prelude` disables all prelude loading.
4. Direct canonical builtin aliases remain available only in explicit no-prelude
   compatibility paths during the migration window; public names such as
   `map` and `print!` now resolve through their `__kernel_*` bridge equivalents.
5. Raw library helpers (`compileSource`, `runSource`) remain no-prelude
   low-level APIs; user-facing default behavior is the CLI path with bundled
   prelude loading enabled.

## Kernel Catalog (Current `jazz-next` Runtime Subset)

| Symbol | Arity | Type Contract | Current Owner | Migration Target |
| --- | --- | --- | --- | --- |
| `map` | `2` | `(a -> b) -> [a] -> [b]` | `__kernel_map` bridge + bundled prelude alias (`map = __kernel_map`) | prelude-owned API (remove direct kernel bridge after parity) |
| `filter` | `2` | `(a -> Bool) -> [a] -> [a]` | `__kernel_filter` bridge + bundled prelude alias (`filter = __kernel_filter`) | prelude-owned API (remove direct kernel bridge after parity) |
| `hd` | `1` | `[a] -> a` | `__kernel_hd` bridge + bundled prelude alias (`hd = __kernel_hd`) | prelude-owned API (remove direct kernel bridge after parity) |
| `tl` | `1` | `[a] -> [a]` | `__kernel_tl` bridge + bundled prelude alias (`tl = __kernel_tl`) | prelude-owned API (remove direct kernel bridge after parity) |
| `print!` | `1` | `a -> a` (stub-v1) | `__kernel_print!` bridge + bundled prelude alias (`print! = __kernel_print!`) | prelude-owned impure API after effect-system follow-up |

## Bundled Prelude Contract

- Bundled prelude path: `jazz-next/stdlib/Prelude.jz`
- Current bridge declarations use the `__kernel_` prefix, remain kernel
  self-bridges, and satisfy `PreludeContract` validation.
- Current bundled exports must alias the matching `__kernel_*` bridge names.
- Current bundled exports/aliases are:
  - `map`, `filter`, `hd`, `tl`, `print!`
- Catalog ownership metadata (`PreludeTarget` vs future intrinsic-only entries)
  is declared in `jazz-next/src/JazzNext/Compiler/BuiltinCatalog.hs`.

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

## Compatibility Window Policy

1. Current default mode is `prelude-owned`: prelude-enabled compile/run paths
   resolve builtins through kernel bridge names only.
2. Compatibility mode is explicit: `--no-prelude` in CLI or `Nothing` prelude in
   driver APIs enables legacy canonical builtin alias fallback.
3. Legacy no-prelude canonical aliases (`map`, `filter`, `hd`, `tl`, `print!`)
   are deprecated and migration-only.
4. Removal of compatibility aliases requires:
   - prelude load path enabled in default compile/run pipeline,
   - parity tests for compile and runtime behavior,
   - documentation updates in `docs/spec/*` and status trackers.

## Non-Goals (Current Phase)

1. This document does not define module loader behavior for prelude discovery.
2. This document does not define full stdlib API surface beyond the active
   kernel subset.
3. This document does not guarantee immediate removal dates for kernel aliases.
