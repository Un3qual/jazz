# Binding and Signature Coherence Implementation Plan

> **For implementers:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Define and enforce one coherent semantic contract for declaration order, type-signature attachment, shadowing, and recursive binding behavior.

**Architecture:** Resolve declaration semantics in docs first, then lock behavior with parser/analyzer tests before implementation changes in type inference. Keep rules explicit enough to avoid order-dependent behavior regressions.

**Tech Stack:** Haskell (`jazz-hs` parser/analyzer/tests), Markdown spec docs, `stack` (optionally via Nix shell).

---

## Progress

- [x] Evidence gathered for declaration/signature drift
- [x] Signature-placement decision gate finalized
- [x] Remaining declaration-semantics gates finalized
- [ ] Normative binding/signature spec doc published
- [ ] Parser/analyzer tests aligned to the chosen contract
- [ ] Analyzer implementation aligned and verified
- [ ] Language-state/docs updated and item closed

## Decision Lock (Approved 2026-03-03)

- [x] Type signatures are optional when types can be inferred.
- [x] When present, a type signature must appear directly above its binding.
- [x] Rebinding is allowed in any scope (top-level and nested), with deterministic same-scope `last wins` semantics.
- [x] Self-recursion is unrestricted (implementation may extend to broader recursion forms under the same gate).

## Verification Evidence (Current Ambiguity)

- `jazz-hs/src/Analyzer/TypeInference.hs`:
  - `ELet` checks pre-existing entry and unifies with inferred body only if found.
  - `ETypeSignature` creates fresh type var and pushes a match constraint.
  - Current behavior is order-sensitive and only partially models signature-to-binding linkage.
- `jazz-hs/test/Analyzer/TypeInferenceSpec.hs`:
  - type-signature test block is commented as `Currently broken`, indicating semantic instability.
- `jazz-hs/src/Parser/Lang.hs`:
  - signatures and declarations parse as independent top-level expressions (`ETypeSignature`, `ELet`) with no grouping construct.

## Scope Guardrails

In scope:
- declaration ordering semantics,
- type-signature attachment semantics,
- same-name rebinding/shadowing policy,
- recursion and forward-reference policy.

Out of scope:
- higher-rank polymorphism,
- effect typing design (covered elsewhere),
- module-level name resolution rules (covered by module plan domains).

## Decision Gates (Must Be Explicit Before Coding)

- [x] Gate A: Signature attachment model.
  - [x] Option A1 (selected): signature must appear immediately before binding.
  - [ ] Option A2: signature may appear before or after, resolved by name within same scope.
  - [ ] Option A3: remove standalone signature syntax from current surface until full binding groups are implemented.
- [x] Gate B: Redeclaration policy.
  - [ ] Option B1: reject same-scope rebinds.
  - [ ] Option B2: allow shadowing in nested scopes only.
  - [x] Option B3 (selected): allow same-scope rebind with deterministic "last wins" rule.
- [x] Gate C: Recursion policy.
  - [ ] Option C1: allow self-recursion only when signature is present.
  - [x] Option C2 (selected): allow unrestricted recursion with fixpoint treatment.
  - [ ] Option C3: disallow recursion in current phase and emit explicit diagnostics.

## Phase 0: Baseline Matrix and Decision Record

- [ ] Create a decision matrix that enumerates current parser/analyzer behavior for:
  - declaration with no signature (inference path),
  - signature directly above declaration,
  - declaration before signature (expected invalid path),
  - signature separated from declaration by non-signature statements (expected invalid path),
  - duplicate declarations,
  - recursive references.
- [ ] Record expected behavior under each candidate decision option.
- [ ] Lock selected gates A/B/C with rationale.

Create:
- `docs/spec/semantics/bindings-and-signatures.md`

Modify:
- `docs/plans/spec-clarification/2026-03-03/semantics/13-binding-and-signature-coherence.md`

### Commit Checkpoint (Phase 0)

```bash
git add docs/spec/semantics/bindings-and-signatures.md \
  docs/plans/spec-clarification/2026-03-03/semantics/13-binding-and-signature-coherence.md
git commit -m "docs(spec): lock binding and signature coherence decisions"
```

## Phase 1: Tests-First Contract Encoding

- [ ] Add parser/analyzer tests that encode the selected model.
- [ ] Ensure tests cover both accepted and rejected forms.
- [ ] Add at least one regression test for the previously broken signature scenario.

Modify:
- `jazz-hs/test/ParserSpec.hs`
- `jazz-hs/test/Analyzer/TypeInferenceSpec.hs`

### Commit Checkpoint (Phase 1)

```bash
git add jazz-hs/test/ParserSpec.hs \
  jazz-hs/test/Analyzer/TypeInferenceSpec.hs
git commit -m "test(type-inference): codify binding and signature contract"
```

## Phase 2: Analyzer Semantics Alignment

- [ ] Refactor declaration/signature handling to enforce the chosen attachment model.
- [ ] Implement diagnostics for invalid ordering/redeclaration/recursion patterns.
- [ ] Remove reliance on implicit order-sensitive behavior where it conflicts with chosen policy.

Modify:
- `jazz-hs/src/Analyzer/TypeInference.hs`
- `jazz-hs/src/Errors.hs` (if new diagnostics are needed)

### Commit Checkpoint (Phase 2)

```bash
git add jazz-hs/src/Analyzer/TypeInference.hs jazz-hs/src/Errors.hs
git commit -m "feat(type-inference): enforce binding and signature coherence rules"
```

(If `Errors.hs` is unchanged, omit it.)

## Phase 3: Docs and Tracker Closure

- [ ] Update language state doc to remove binding/signature ambiguity notes.
- [ ] Link normative spec section and tests as evidence.

Modify:
- `docs/jazz-language-state.md`
- `docs/plans/spec-clarification/2026-03-03/README.md`

### Commit Checkpoint (Phase 3)

```bash
git add docs/jazz-language-state.md \
  docs/plans/spec-clarification/2026-03-03/README.md
git commit -m "docs(spec): close binding and signature coherence clarification"
```

## Verification Commands

```bash
cd jazz-hs
stack test --ta '--match "type signature"'
stack test --ta '--match "multiple expressions"'
stack test
```

## Definition of Done

- [ ] One canonical declaration/signature contract is documented.
- [ ] Broken signature tests are replaced by passing contract tests.
- [ ] Analyzer behavior and diagnostics match the contract.
- [ ] Ambiguity no longer appears in language-state tracking.
