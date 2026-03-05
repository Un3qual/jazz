# Spec-Cleanup #3 Implementation Plan: Compiler-Enforced Purity `!` (Stub V1)

> **For executor:** REQUIRED SUB-SKILL: use `executing-plans` to run this plan step-by-step.

**Goal:** Resolve spec-cleanup item #3 by making `!` purity semantics compiler-enforced, delivered first as a deliberately limited stubbed implementation.

**Architecture:** Ship a strict, name-driven purity checker now (minimal enforcement surface), then keep extension points for a later full effect system. Enforce in analyzer; do not rely on runtime behavior.

**Tech Stack:** Haskell (`jazz-next` parser/analyzer/runtime/tests), Markdown docs, Stack, Nix shell.

**Active compiler path guidance:** all actionable implementation/test steps in this plan must target `jazz-next/`; `jazz-hs/` is legacy evidence only.

---

## Progress Checklist

- [x] Decision lock recorded from maintainer approval (2026-03-02)
- [ ] Phase 0 complete: baseline + Nix verification
- [x] Phase 1 complete: failing tests for stub purity rules (`jazz-next`)
- [x] Phase 2 complete: analyzer-level stub enforcement implemented (`jazz-next`)
- [x] Phase 3 complete: docs/spec updated to match enforced behavior
- [ ] Phase 4 complete: reproducible verification + cleanup closure

## 2026-03-05 `jazz-next` Execution Batch

- [x] Re-verified baseline behavior before changes with `bash jazz-next/scripts/test-warning-config.sh`.
- [x] Added failing purity regression tests first in `jazz-next/test/PuritySemanticsSpec.hs`.
- [x] Implemented stub-v1 purity helpers and analyzer enforcement in:
  - `jazz-next/src/JazzNext/Compiler/Purity.hs`
  - `jazz-next/src/JazzNext/Compiler/Analyzer.hs`
- [x] Added purity suite to default verification runner (`jazz-next/scripts/test-warning-config.sh`).
- [x] Updated status/spec docs to reflect implemented stub-v1 scope and limitations.
- [x] Re-ran full `jazz-next` verification after implementation.

## Verification Evidence (Remaining Work)

- `jazz-next` stub-v1 purity enforcement and tests are implemented:
  - `jazz-next/src/JazzNext/Compiler/Purity.hs`
  - `jazz-next/src/JazzNext/Compiler/Analyzer.hs`
  - `jazz-next/test/PuritySemanticsSpec.hs`
- Remaining closure work is reproducible Nix-shell verification path wiring/documentation (Phase 0/4).

## Decision Lock (Approved 2026-03-02)

- [x] `!` purity is compiler-enforced.
- [x] First implementation is a stub (V1) rather than a full effect system.

## Stub V1 Semantics Contract

These rules are the required behavior for item #3 completion:

1. Any binding/function name ending in `!` is **impure**.
2. Any binding/function name without `!` is **pure by default**.
3. A pure binding/function body may not call an impure callee.
4. An impure binding/function body may call pure or impure callees.
5. Top-level root expressions remain executable (treat bare top-level expression context as permissive) so `print! "Hello".` still works as program entry.
6. V1 enforcement is name-driven and direct-call focused; no effect polymorphism or higher-order purity proofs yet.
7. Compiler errors must be explicit and actionable (callee, caller/context, source span if available).

## Stub V1 Non-Goals

- Full effect typing in function types.
- Effect inference through higher-order unknown callees.
- Cross-module purity graph analysis.
- Runtime purity enforcement.

## Concrete Files To Modify During Execution

Likely modify:
- `jazz-next/src/JazzNext/Compiler/Analyzer.hs`
- `jazz-next/src/JazzNext/Compiler/Purity.hs`
- `jazz-next/test/PuritySemanticsSpec.hs`
- `jazz-next/scripts/test-warning-config.sh`
- `README.md`
- `docs/jazz-language-state.md`
- `docs/feature-status.md`

Optional create:
- `docs/spec/semantics/purity-bang-stub-v1.md`

## Phase 0: Baseline and Nix Reproducibility

- [ ] Run baseline tests before changes.
- [ ] Confirm Nix-based command path for repeatable verification.

Commands:

```bash
export NIXPKGS_REF='github:NixOS/nixpkgs/68cc97d306d3187c142cfb2378852f28d47bc098'
nix --extra-experimental-features 'nix-command flakes' shell \
  "$NIXPKGS_REF#stack" \
  "$NIXPKGS_REF#nodejs_20" \
  -c bash -lc 'stack --version && node --version && bash jazz-next/scripts/test-warning-config.sh'
```

Expected: baseline captured; existing failures (if any) recorded unchanged.

**Commit checkpoint (optional):**

```bash
git add docs/plans/spec-cleanup/2026-03-02/decisions/03-purity-bang-semantics.md
git commit -m "docs(plan): lock stub-v1 purity enforcement decision"
```

## Phase 1: TDD for Stub Enforcement

- [ ] Write failing analyzer tests for each V1 contract rule before implementation.

Required test scenarios:

1. Pure binding calling impure builtin fails.
2. Impure binding calling impure builtin passes.
3. Pure binding calling impure user-defined function fails.
4. Pure binding calling pure function passes.
5. Top-level `print!` expression remains valid entry behavior.

Files:
- `jazz-next/test/PuritySemanticsSpec.hs`
- `jazz-next/scripts/test-warning-config.sh` (suite wiring)

Run and verify failure first:

```bash
runghc -i./jazz-next/src -i./jazz-next/test jazz-next/test/PuritySemanticsSpec.hs
```

**Commit checkpoint:**

```bash
git add jazz-next/test/PuritySemanticsSpec.hs jazz-next/scripts/test-warning-config.sh
git commit -m "test(purity): add failing stub-v1 purity coverage in jazz-next"
```

(If runner wiring is unchanged, omit `jazz-next/scripts/test-warning-config.sh`.)

## Phase 2: Implement Analyzer Stub Enforcement

### Step 1: Add minimal purity helpers

- [ ] Add `Purity` helper type and name-classification utility (`isImpureName`/`namePurity`) in a stable shared location.
- [ ] Mark `print!` and other `!`-suffixed names as impure through this helper.

### Step 2: Add purity context during inference

- [ ] Track current purity context while inferring each binding body.
- [ ] For `ELet name body`, set body context by name suffix (`name!` => impure context; otherwise pure context).

### Step 3: Enforce direct-call restriction

- [ ] When inferring an application whose resolved callee is impure, reject if current context is pure.
- [ ] Keep error messaging deterministic and explicit.

### Step 4: Preserve current runtime/codegen behavior

- [ ] Do not change JS runtime semantics for this item; enforcement is compile/analyze time only.

Suggested files:
- `jazz-next/src/JazzNext/Compiler/Analyzer.hs`
- `jazz-next/src/JazzNext/Compiler/Purity.hs`
- optional: `jazz-next/src/JazzNext/Compiler/TypeInference.hs` (if propagation hooks are needed)

Validation:

```bash
runghc -i./jazz-next/src -i./jazz-next/test jazz-next/test/PuritySemanticsSpec.hs
bash jazz-next/scripts/test-warning-config.sh
```

**Commit checkpoint:**

```bash
git add jazz-next/src/JazzNext/Compiler/Analyzer.hs jazz-next/src/JazzNext/Compiler/Purity.hs
git commit -m "feat(analyzer): enforce stub-v1 purity using bang suffix semantics"
```

(If `TypeInference.hs` changed too, include it.)

## Phase 3: Docs and Spec Alignment

- [ ] Update language docs to describe enforced stub-v1 behavior accurately.
- [ ] Document explicit limitations (name-driven checks, no effect polymorphism yet).
- [ ] Mark item #3 as resolved once code/tests/docs are aligned.

Files:
- `README.md`
- `docs/jazz-language-state.md`
- `docs/feature-status.md`
- optional: `docs/spec/semantics/purity-bang-stub-v1.md`

**Commit checkpoint:**

```bash
git add README.md docs/jazz-language-state.md docs/feature-status.md docs/spec/semantics/purity-bang-stub-v1.md
git commit -m "docs(spec): document compiler-enforced stub-v1 purity semantics"
```

(If the optional spec doc is not created, omit it from `git add`.)

## Phase 4: Reproducible Verification and Closure

- [ ] Run full verification in Nix shell.
- [ ] Confirm purity violation tests fail for bad cases and pass for allowed cases.
- [ ] Confirm top-level runnable examples still work.
- [ ] Confirm cleanup item #3 is no longer listed as unresolved.

Commands:

```bash
export NIXPKGS_REF='github:NixOS/nixpkgs/68cc97d306d3187c142cfb2378852f28d47bc098'
nix --extra-experimental-features 'nix-command flakes' shell \
  "$NIXPKGS_REF#stack" \
  "$NIXPKGS_REF#nodejs_20" \
  -c bash -lc '
    set -euo pipefail
    bash jazz-next/scripts/test-warning-config.sh
    runghc -i./jazz-next/src -i./jazz-next/test jazz-next/test/PuritySemanticsSpec.hs
  '
```

**Final commit checkpoint (if cleanup edits remain):**

```bash
git add README.md docs/jazz-language-state.md docs/feature-status.md jazz-next/test/PuritySemanticsSpec.hs jazz-next/src/JazzNext/Compiler/Analyzer.hs jazz-next/src/JazzNext/Compiler/Purity.hs
git commit -m "chore(spec-cleanup): close item #3 with enforced stub-v1 purity"
```

## Definition of Done

- [x] `!` purity is compiler-enforced in analyzer (stub-v1 contract).
- [x] Tests cover allowed/forbidden pure-impure call paths.
- [x] Docs describe enforced behavior and limitations without contradictions.
- [ ] Nix-based verification path is documented and passing.
- [ ] Item #3 is marked resolved in `docs/jazz-language-state.md`.
