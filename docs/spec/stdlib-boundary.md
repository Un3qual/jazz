# Standard Library Boundary

Status: active (phase 4 migration scaffolding in progress)
Locked decisions (initial `jazz-next` contract): 2026-03-04
Updated: 2026-03-05
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
2. `prelude` symbols are user-visible APIs intended to move to `.jz` modules.
3. `jazz-next` now supports a bundled default prelude load path in CLI mode:
   - resolution order: `--prelude` flag > `JAZZ_PRELUDE` env > bundled default path.
   - `--no-prelude` disables all prelude loading.
4. Kernel symbol lookup remains available during the compatibility window while
   prelude-owned defaults are rolled out.

## Kernel Catalog (Current `jazz-next` Runtime Subset)

| Symbol | Arity | Type Contract | Current Owner | Migration Target |
| --- | --- | --- | --- | --- |
| `map` | `2` | `(a -> b) -> [a] -> [b]` | kernel builtin + bundled prelude alias | prelude-owned API (remove direct kernel alias after parity) |
| `filter` | `2` | `(a -> Bool) -> [a] -> [a]` | kernel builtin + bundled prelude alias | prelude-owned API (remove direct kernel alias after parity) |
| `hd` | `1` | `[a] -> a` | kernel builtin + bundled prelude alias | prelude-owned API (remove direct kernel alias after parity) |
| `tl` | `1` | `[a] -> [a]` | kernel builtin + bundled prelude alias | prelude-owned API (remove direct kernel alias after parity) |
| `print!` | `1` | `a -> a` (stub-v1) | kernel builtin + bundled prelude alias | prelude-owned impure API after effect-system follow-up |

## Bundled Prelude Contract

- Bundled prelude path: `jazz-next/stdlib/Prelude.jz`
- Current bridge declarations use the `__kernel_` prefix and must satisfy
  `PreludeContract` validation.
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

1. Analyzer builtin visibility must derive from the shared catalog.
2. Type inference builtin type instantiation must dispatch by shared catalog
   symbol, not duplicated string lists.
3. Runtime builtin lookup/arity must dispatch by shared catalog symbol.
4. Tests must fail if catalog entries drift from compile/runtime behavior.

## Compatibility Window Policy

1. Current mode is `bundled-prelude-by-default` in CLI paths, with kernel alias
   compatibility still enabled.
2. Kernel aliases remain valid until parity tests pass and migration gates are
   closed.
3. Removal of kernel aliases requires:
   - prelude load path enabled in default compile/run pipeline,
   - parity tests for compile and runtime behavior,
   - documentation updates in `docs/spec/*` and status trackers.

## Non-Goals (Current Phase)

1. This document does not define module loader behavior for prelude discovery.
2. This document does not define full stdlib API surface beyond the active
   kernel subset.
3. This document does not guarantee immediate removal dates for kernel aliases.
