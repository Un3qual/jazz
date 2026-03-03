# Primitive Semantics Contract Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Define backend-independent language semantics for primitive operations and values (numeric operations, equality, boolean behavior, list primitives, and basic error behavior).

**Architecture:** Specify primitive semantics in language terms first, then map each backend/runtime path to conformance tests. Prevent backend implementation details (for example JS loose equality) from defining language semantics by accident.

**Tech Stack:** Haskell type environment/runtime modules, docs/spec files, parser/analyzer/runtime tests.

---

## Progress

- [x] Primitive semantic drift identified
- [ ] Primitive inventory and contract table published
- [ ] Decision gates finalized (`==`, numeric behavior, primitive errors)
- [ ] Runtime/typechecker conformance tests added
- [ ] Docs and trackers aligned

## Verification Evidence (Current Ambiguity)

- `jazz-hs/src/Types.hs`: builtin traits and builtin function signatures define only a subset of primitive behavior.
- `jazz-hs/src/CodeGen/Javascript.hs`: `==` lowers to JS loose equality; this may not match intended language-level equality semantics.
- `docs/plans/spec-clarification/2026-03-02/runtime/12-backend-target-strategy.md`: backend strategy is being stabilized, increasing need for backend-independent primitive definitions.

## Scope Guardrails

In scope:
- arithmetic primitives (`+`, `-`, `*`, `/`),
- equality (`==`) semantics,
- list primitives (`hd`, `tl`, `map`) observable contracts,
- primitive failure behavior surface (error categories, not full diagnostics system).

Out of scope:
- full standard library API design,
- advanced algebraic law proofs,
- optimizer-specific rewrite rules.

## Decision Gates

- [ ] Gate A: Equality contract.
  - Option A1: strict type-directed equality only.
  - Option A2: structural equality for compatible value families.
  - Option A3: retain JS-like coercive behavior (not recommended for interpreter-first direction).
- [ ] Gate B: Numeric behavior.
  - Option B1: integer/float operations remain trait-driven with explicit defaulting rules.
  - Option B2: explicit literal suffixing or syntax to avoid defaulting ambiguity.
- [ ] Gate C: Primitive error model.
  - Option C1: recoverable runtime errors with explicit error values.
  - Option C2: fatal runtime diagnostics for invalid primitive calls in v1.

## Phase 0: Primitive Contract Table

- [ ] Build a table of all current primitive names and signatures.
- [ ] For each primitive, define:
  - argument/return contract,
  - valid and invalid inputs,
  - deterministic semantics independent of backend implementation language.

Create:
- `docs/spec/runtime/primitive-semantics.md`

### Commit Checkpoint (Phase 0)

```bash
git add docs/spec/runtime/primitive-semantics.md \
  docs/plans/spec-clarification/2026-03-03/runtime/16-primitive-semantics-contract.md
git commit -m "docs(spec): define primitive semantics contract table"
```

## Phase 1: Conformance Test Design

- [ ] Add tests for primitive behavior contracts in analyzer/runtime path.
- [ ] Include explicit mismatch tests (for equality and invalid primitive uses).
- [ ] Ensure tests can run regardless of active backend by targeting shared semantic entrypoints.

Modify/Create:
- `jazz-hs/test/Analyzer/TypeInferenceSpec.hs`
- `jazz-hs/test/InterpreterSpec.hs` (if interpreter path is active)
- `jazz-hs/test/Spec.hs`

### Commit Checkpoint (Phase 1)

```bash
git add jazz-hs/test/Analyzer/TypeInferenceSpec.hs jazz-hs/test/InterpreterSpec.hs jazz-hs/test/Spec.hs
git commit -m "test(runtime): add primitive semantics conformance tests"
```

(If `InterpreterSpec.hs` does not exist yet, create it or omit it for the selected runtime track.)

## Phase 2: Runtime/Backend Alignment

- [ ] Align primitive implementations with the semantic contract.
- [ ] Remove backend-specific behavior that violates contract (for example coercive equality if disallowed).
- [ ] Keep compatibility note if temporary divergence is intentionally accepted.

Modify (choose active backend path):
- `jazz-hs/src/CodeGen/Javascript.hs`
- `jazz-hs/src/Interpreter.hs`
- `jazz-hs/src/Types.hs`

### Commit Checkpoint (Phase 2)

```bash
git add jazz-hs/src/CodeGen/Javascript.hs jazz-hs/src/Interpreter.hs jazz-hs/src/Types.hs
git commit -m "feat(runtime): align primitive implementations with language contract"
```

## Phase 3: Docs and Tracking Closure

- [ ] Update language-state doc to reference canonical primitive contract.
- [ ] Ensure plan tracker marks primitive semantics no longer implicit.

Modify:
- `docs/jazz-language-state.md`
- `docs/plans/spec-clarification/2026-03-03/README.md`

### Commit Checkpoint (Phase 3)

```bash
git add docs/jazz-language-state.md docs/plans/spec-clarification/2026-03-03/README.md
git commit -m "docs(spec): close primitive semantics clarification"
```

## Verification Commands

```bash
cd jazz-hs
stack test --ta '--match "Type Inference"'
stack test --ta '--match "Interpreter"'
stack test
```

## Definition of Done

- [ ] Primitive semantics are defined in language terms, not backend accident.
- [ ] Equality and numeric behavior are explicitly locked.
- [ ] Conformance tests exist for each primitive family.
