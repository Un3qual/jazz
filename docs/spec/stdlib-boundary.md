# Standard Library Boundary

Status: active (phase 1 boundary contract)
Locked decisions (initial `jazz-next` contract): 2026-03-04
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
3. Until prelude loading exists in `jazz-next`, public symbols in the active
   runtime subset remain in the kernel with explicit migration status.

## Kernel Catalog (Current `jazz-next` Runtime Subset)

| Symbol | Arity | Type Contract | Current Owner | Migration Target |
| --- | --- | --- | --- | --- |
| `map` | `2` | `(a -> b) -> [a] -> [b]` | compiler/runtime builtin catalog | prelude-owned API after loader support |
| `hd` | `1` | `[a] -> a` | compiler/runtime builtin catalog | prelude-owned API after loader support |
| `tl` | `1` | `[a] -> [a]` | compiler/runtime builtin catalog | prelude-owned API after loader support |

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

1. Current mode is `kernel-only` for listed symbols.
2. Future prelude loading must keep a compatibility window where kernel aliases
   remain valid until parity tests pass.
3. Removal of kernel aliases requires:
   - prelude load path enabled in default compile/run pipeline,
   - parity tests for compile and runtime behavior,
   - documentation updates in `docs/spec/*` and status trackers.

## Non-Goals (Current Phase)

1. This document does not define module loader behavior for prelude discovery.
2. This document does not define full stdlib API surface beyond the active
   kernel subset.
3. This document does not guarantee immediate removal dates for kernel aliases.
