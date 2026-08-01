# Purity Marker `!` (Stub V1)

Status: active (stub-v1 enforcement in `Jazz`)
Locked decisions: 2026-03-02

## Purpose

Define the currently enforced purity contract for `!`-suffixed names in the active compiler pipeline.

## Implementation Target

- Enforcement is implemented in the repository root.
- Pre-root-canonicalization behavior preserved at archive tag
  `archive/pre-root-canonicalization-2026-07-31` is historical and
  non-normative; the archived implementation trees are absent from the current
  checkout.

## Stub-V1 Contract

1. Any binding name ending in `!` is impure.
2. Any binding name without `!` is pure by default.
3. A pure binding body cannot directly call a known impure callee.
4. An impure binding body may call pure or impure callees.
5. Top-level expression statements remain permissive so entry expressions may call impure names.
6. Shared builtin catalog entries with `!` suffix (currently `print!`) participate in the same impure-callee checks.

## Current Enforcement Scope

- Enforced in analyzer/type pipeline diagnostics:
  - `src/Jazz/Compiler/Purity.hs`
  - `src/Jazz/Compiler/Analyzer.hs`
- Verified by:
  - `test/Jazz/Compiler/Semantics/PuritySemanticsSpec.hs`

## Non-Goals (Still Planned)

- Effect polymorphism in type signatures.
- Higher-order purity proofs for unknown function values.
- Cross-module purity graph analysis.
- Runtime purity enforcement.

These remain blocked under `JN-PURITY-EFFECT-TYPING-PLAN-001` until
module-method/export behavior and a concrete effect-system contract are
clearer.
