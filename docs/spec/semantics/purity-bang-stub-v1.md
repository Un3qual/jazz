# Purity Marker `!` (Stub V1)

Status: active (stub-v1 enforcement in `jazz-next`)
Locked decisions: 2026-03-02
Primary plan: `docs/plans/spec-cleanup/2026-03-02/decisions/03-purity-bang-semantics.md`

## Purpose

Define the currently enforced purity contract for `!`-suffixed names in the active compiler pipeline.

## Implementation Target

- Enforcement is implemented in `jazz-next/`.
- `jazz-hs/` and `jazz2/` are legacy reference implementations and are non-normative for this contract.

## Stub-V1 Contract

1. Any binding name ending in `!` is impure.
2. Any binding name without `!` is pure by default.
3. A pure binding body cannot directly call a known impure callee.
4. An impure binding body may call pure or impure callees.
5. Top-level expression statements remain permissive so entry expressions may call impure names.
6. Shared builtin catalog entries with `!` suffix (currently `print!`) participate in the same impure-callee checks.

## Current Enforcement Scope

- Enforced in analyzer/type pipeline diagnostics:
  - `jazz-next/src/JazzNext/Compiler/Purity.hs`
  - `jazz-next/src/JazzNext/Compiler/Analyzer.hs`
- Verified by:
  - `jazz-next/test/PuritySemanticsSpec.hs`

## Non-Goals (Still Planned)

- Effect polymorphism in type signatures.
- Higher-order purity proofs for unknown function values.
- Cross-module purity graph analysis.
- Runtime purity enforcement.
