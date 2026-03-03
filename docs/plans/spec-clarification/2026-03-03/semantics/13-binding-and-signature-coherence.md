# Binding and Signature Coherence Implementation Plan

> **For implementers:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Define and enforce one coherent semantic contract for declaration order, type-signature attachment, shadowing, and recursive binding behavior.

**Architecture:** Resolve declaration semantics in docs first, then lock behavior with parser/analyzer tests before implementation changes in type inference. Keep rules explicit enough to avoid order-dependent behavior regressions.

**Tech Stack:** Haskell (`jazz-next` analyzer/tests), Markdown spec docs, shell test runner (`bash jazz-next/scripts/test-warning-config.sh`).

---

## Progress

- [x] Evidence gathered for declaration/signature drift
- [x] Signature-placement decision gate finalized
- [x] Remaining declaration-semantics gates finalized
- [x] Normative binding/signature spec doc published
- [x] Analyzer tests aligned for signature adjacency + use-before-definition in the current `jazz-next` AST subset
- [ ] Full parser + recursion-group semantics alignment in `jazz-next`
- [ ] Language-state/docs updated and item closed

## Decision Lock (Approved 2026-03-03)

- [x] Type signatures are optional when types can be inferred.
- [x] When present, a type signature must appear directly above its binding.
- [x] Rebinding is allowed in any scope (top-level and nested), with deterministic same-scope `last wins` semantics.
- [x] Recursion is unrestricted, including mutual recursion groups.
- [x] Non-recursive use-before-definition is invalid and must fail at compile-time.
- [x] Rebinding warnings are optional and controlled by compiler warning flags; default behavior remains silent.

## Implementation Status Verification (2026-03-02, Batch 1)

- [x] Verified warning-flag rebinding policy is already implemented in `jazz-next` (`W0001`, CLI/env/config controls).
- [x] Added signature-adjacency analyzer checks in `jazz-next` (`SSignature` must immediately precede matching `SLet`).
- [x] Added compile-time diagnostics for use-before-definition (`E1001` unbound variable).
- [x] Added contract tests for valid and invalid signature ordering/name matching.
- [x] Added lexical-scope regression test proving nested scope can resolve outer bindings.
- [x] Recursion-group semantics (self + mutual recursion groups) are now implemented in the current `jazz-next` analyzer AST subset and covered by tests.

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
- [x] Gate D: Forward-reference policy.
  - [x] Option D1 (selected): non-recursive use-before-definition is invalid.
  - [ ] Option D2: allow forward references in the same scope by deferred resolution.
- [x] Gate E: Rebinding warning policy.
  - [ ] Option E1: always warn for same-scope rebinding.
  - [x] Option E2 (selected): warning path exists behind optional compiler flags; default stays silent.
  - [ ] Option E3: never warn and provide no warning option.

## Phase 0: Baseline Matrix and Decision Record

- [x] Create a decision matrix that enumerates current parser/analyzer behavior for:
  - declaration with no signature (inference path),
  - signature directly above declaration,
  - declaration before signature (expected invalid path),
  - signature separated from declaration by non-signature statements (expected invalid path),
  - duplicate declarations,
  - recursive references.
- [x] Record expected behavior under each candidate decision option.
- [x] Lock selected gates A/B/C with rationale.
- [x] Lock forward-reference and warning-policy gates (D/E) with rationale.

Create:
- `docs/spec/semantics/bindings-and-signatures.md`

Modify:
- `docs/plans/spec-clarification/2026-03-03/semantics/13-binding-and-signature-coherence.md`
- `docs/plans/spec-clarification/2026-03-03/tooling/18-compiler-warning-flags.md` (future warning-flag execution plan)

### Commit Checkpoint (Phase 0)

```bash
git add docs/spec/semantics/bindings-and-signatures.md \
  docs/plans/spec-clarification/2026-03-03/semantics/13-binding-and-signature-coherence.md
git commit -m "docs(spec): lock binding and signature coherence decisions"
```

## Phase 1: Tests-First Contract Encoding

- [x] Add analyzer tests that encode the selected model for the current `jazz-next` AST surface.
- [x] Ensure tests cover both accepted and rejected forms.
- [x] Add regression tests for signature ordering/name mismatches.
- [ ] Add parser-level contract tests once parser surface exists in `jazz-next`.

Modify:
- `jazz-next/test/BindingSignatureCoherenceSpec.hs`
- `jazz-next/scripts/test-warning-config.sh`

### Commit Checkpoint (Phase 1)

```bash
git add jazz-next/test/BindingSignatureCoherenceSpec.hs \
  jazz-next/scripts/test-warning-config.sh
git commit -m "test(jazz-next): codify binding/signature coherence contract"
```

## Phase 2: Analyzer Semantics Alignment

- [x] Refactor declaration/signature handling to enforce immediate adjacency in the current AST.
- [x] Implement diagnostics for invalid signature ordering/name mismatch and use-before-definition.
- [x] Implement recursion-group semantics (self + mutual recursion) per locked policy.
- [ ] Remove remaining order-sensitive behavior tied to parser/type-surface gaps.

Modify:
- `jazz-next/src/JazzNext/Compiler/Analyzer.hs`
- `jazz-next/src/JazzNext/Compiler/TypeInference.hs`
- `jazz-next/src/JazzNext/Compiler/Driver.hs`

### Commit Checkpoint (Phase 2)

```bash
git add jazz-next/src/JazzNext/Compiler/Analyzer.hs \
  jazz-next/src/JazzNext/Compiler/TypeInference.hs \
  jazz-next/src/JazzNext/Compiler/Driver.hs
git commit -m "feat(jazz-next): enforce binding/signature adjacency and unbound-use diagnostics"
```

## Phase 3: Docs and Tracker Closure

- [ ] Update language state doc to remove binding/signature ambiguity notes.
- [x] Link normative spec section and tests as evidence.

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
bash jazz-next/scripts/test-warning-config.sh
```

## Definition of Done

- [x] One canonical declaration/signature contract is documented.
- [x] Contract tests for signature ordering/name matching and use-before-definition pass in `jazz-next`.
- [x] Analyzer behavior and diagnostics match the contract for the current `jazz-next` AST surface.
- [ ] Ambiguity no longer appears in language-state tracking.
