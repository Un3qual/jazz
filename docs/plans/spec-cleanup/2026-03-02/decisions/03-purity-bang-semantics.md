# Spec-Cleanup #3 Implementation Plan: Compiler-Enforced Purity `!` (Stub V1)

> **For executor:** REQUIRED SUB-SKILL: use `executing-plans` to run this plan step-by-step.

**Goal:** Resolve spec-cleanup item #3 by making `!` purity semantics compiler-enforced, delivered first as a deliberately limited stubbed implementation.

**Architecture:** Ship a strict, name-driven purity checker now (minimal enforcement surface), then keep extension points for a later full effect system. Enforce in analyzer; do not rely on runtime behavior.

**Tech Stack:** Haskell (`jazz-hs` parser/analyzer/codegen/tests), Markdown docs, Stack, Nix shell.

---

## Progress Checklist

- [x] Decision lock recorded from maintainer approval (2026-03-02)
- [ ] Phase 0 complete: baseline + Nix verification
- [ ] Phase 1 complete: failing tests for stub purity rules
- [ ] Phase 2 complete: analyzer-level stub enforcement implemented
- [ ] Phase 3 complete: docs/spec updated to match enforced behavior
- [ ] Phase 4 complete: reproducible verification + cleanup closure

## Verification Evidence (Item Is Still Unfinished)

- `docs/jazz-language-state.md:424` lists cleanup item #3 as unresolved.
- `docs/jazz-language-state.md:305` states no purity checker exists today and `!` is currently naming syntax.
- `README.md:76` claims pure-by-default + impure restrictions, but compiler does not enforce that today.
- `jazz-hs/src/Parser/Lib.hs:98` allows `!` in identifiers, with no purity semantics attached.
- `jazz-hs/src/Analyzer/TypeInference.hs` has no purity/effect context tracking.

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
- `jazz-hs/src/Analyzer/TypeInference.hs`
- `jazz-hs/src/Types.hs`
- `jazz-hs/src/Errors.hs` (if introducing dedicated purity diagnostics)
- `jazz-hs/test/Analyzer/TypeInferenceSpec.hs`
- `jazz-hs/test/ParserSpec.hs` (only if parser-level coverage is added for `!` naming assumptions)
- `README.md`
- `docs/jazz-language-state.md`

Optional create:
- `docs/spec/purity-semantics.md`

## Phase 0: Baseline and Nix Reproducibility

- [ ] Run baseline tests before changes.
- [ ] Confirm Nix-based command path for repeatable verification.

Commands:

```bash
export NIXPKGS_REF='github:NixOS/nixpkgs/68cc97d306d3187c142cfb2378852f28d47bc098'
nix --extra-experimental-features 'nix-command flakes' shell \
  "$NIXPKGS_REF#stack" \
  "$NIXPKGS_REF#nodejs_20" \
  -c bash -lc 'cd jazz-hs && stack --version && node --version && stack test'
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
- `jazz-hs/test/Analyzer/TypeInferenceSpec.hs`
- (optional) `jazz-hs/test/ParserSpec.hs`

Run and verify failure first:

```bash
cd jazz-hs
stack test --ta '--match "Type Inference"'
```

**Commit checkpoint:**

```bash
git add jazz-hs/test/Analyzer/TypeInferenceSpec.hs jazz-hs/test/ParserSpec.hs
git commit -m "test(purity): add failing tests for bang stub enforcement rules"
```

(If `ParserSpec.hs` is unchanged, omit it.)

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
- `jazz-hs/src/Analyzer/TypeInference.hs`
- `jazz-hs/src/Types.hs`
- `jazz-hs/src/Errors.hs`

Validation:

```bash
cd jazz-hs
stack test --ta '--match "Type Inference"'
stack test
```

**Commit checkpoint:**

```bash
git add jazz-hs/src/Analyzer/TypeInference.hs jazz-hs/src/Types.hs jazz-hs/src/Errors.hs
git commit -m "feat(analyzer): enforce stub-v1 purity using bang suffix semantics"
```

(If `Errors.hs` is unchanged, omit it.)

## Phase 3: Docs and Spec Alignment

- [ ] Update language docs to describe enforced stub-v1 behavior accurately.
- [ ] Document explicit limitations (name-driven checks, no effect polymorphism yet).
- [ ] Mark item #3 as resolved once code/tests/docs are aligned.

Files:
- `README.md`
- `docs/jazz-language-state.md`
- optional: `docs/spec/purity-semantics.md`

**Commit checkpoint:**

```bash
git add README.md docs/jazz-language-state.md docs/spec/purity-semantics.md
git commit -m "docs(spec): document compiler-enforced stub-v1 purity semantics"
```

(If `docs/spec/purity-semantics.md` is not created, omit it.)

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
    cd jazz-hs
    stack test
    ./run.sh ExamplePrograms/ComplexProgram.jz >/tmp/jazz-purity-v1-complex.js
  '
```

**Final commit checkpoint (if cleanup edits remain):**

```bash
git add README.md docs/jazz-language-state.md jazz-hs/test/Analyzer/TypeInferenceSpec.hs jazz-hs/src/Analyzer/TypeInference.hs
git commit -m "chore(spec-cleanup): close item #3 with enforced stub-v1 purity"
```

## Definition of Done

- [ ] `!` purity is compiler-enforced in analyzer (stub-v1 contract).
- [ ] Tests cover allowed/forbidden pure-impure call paths.
- [ ] Docs describe enforced behavior and limitations without contradictions.
- [ ] Nix-based verification path is documented and passing.
- [ ] Item #3 is marked resolved in `docs/jazz-language-state.md`.
