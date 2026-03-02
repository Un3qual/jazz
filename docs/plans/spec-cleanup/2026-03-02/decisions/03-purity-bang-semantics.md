# Spec-Cleanup #3 Implementation Plan: Purity `!` Semantics

> **For executor:** REQUIRED SUB-SKILL: use `executing-plans` to run this plan step-by-step.

**Goal:** Resolve Jazz spec-cleanup item #3 by deciding whether `!` represents enforced purity/effect semantics or naming-only convention, then make spec + implementation behavior unambiguous.

**Architecture:** Drive a deliberate branch decision with explicit impact analysis for parser/typechecker/codegen/runtime/docs, then execute one branch end-to-end with reproducible verification and commit checkpoints.

**Tech Stack:** Haskell (`jazz-hs` parser/analyzer/codegen/tests), JS runtime output, Markdown docs, Stack, Nix shell.

---

## Progress Checklist

- [ ] Phase 0 complete: baseline verification captured and branch decision criteria frozen.
- [ ] Phase 1 complete: both decision branches evaluated against parser/typechecker/codegen/runtime/docs impacts.
- [ ] Phase 2 complete: one branch selected and implemented.
- [ ] Phase 3 complete: reproducible verification run, docs reconciled, final cleanup item status updated.

## Verification Evidence (Item Is Still Unfinished)

Evidence captured from current repo state:

- `/Users/admin/.codex/worktrees/8c77/jazz-main/docs/jazz-language-state.md:424` lists cleanup item `3. Decide whether purity with \`!\` is real semantics or only naming.`
- `/Users/admin/.codex/worktrees/8c77/jazz-main/docs/jazz-language-state.md:305` explicitly states no purity/effect checker exists and `!` is currently only identifier syntax.
- `/Users/admin/.codex/worktrees/8c77/jazz-main/README.md:76` still claims pure-by-default semantics and impure-call restrictions.
- `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/src/Parser/Lib.hs:98` allows `!` as a normal identifier character.
- `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/src/Types.hs:121` includes `print!` as a builtin function name, with no effect type dimension.
- `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/src/CodeGen/Javascript.hs:52` hardcodes `print!` to `console.log(...)` but does not enforce purity boundaries.
- `find docs -type f` currently returns only `/Users/admin/.codex/worktrees/8c77/jazz-main/docs/jazz-language-state.md`; no existing decision doc for this item exists yet.

## Decision Criteria (Must Be Met Before Marking Item #3 Done)

- [ ] Single canonical meaning of `!` is documented (no contradictory statements across docs).
- [ ] Compiler behavior matches that documented meaning.
- [ ] Tests lock in the chosen behavior and prevent regression.
- [ ] End-to-end command path (`parse -> analyze -> optimize -> JS`) is still green.
- [ ] `docs/jazz-language-state.md` no longer lists item #3 as undecided.

## Branch Impact Analysis (Do This Before Choosing)

### Branch A: `!` Is Real Purity/Effect Semantics

**Parser implications**

- Keep `!` in identifiers, but classify `name!` as effectful declaration/usage metadata in AST.
- Candidate files:
  - `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/src/Parser/Lang.hs`
  - `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/src/AST.hs`

**Typechecker implications**

- Add effect tracking to inferred function types (at minimum `Pure` vs `Impure`).
- Enforce rule: pure contexts cannot call impure functions.
- Add explicit diagnostic for purity violations.
- Candidate files:
  - `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/src/Analyzer/TypeInference.hs`
  - `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/src/Types.hs`
  - `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/src/Errors.hs`

**Codegen implications**

- Preserve current lowering behavior, but consume effect metadata for validation consistency and future backend portability.
- Decide whether `print!` remains builtin special-case or moves behind effectful builtin table.
- Candidate file:
  - `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/src/CodeGen/Javascript.hs`

**Runtime implications**

- Current runtime is JS host side-effects (`console.log`), so no runtime purity enforcement required immediately.
- Runtime contract must explicitly state: enforcement occurs at analysis time, not JS runtime.
- Candidate files:
  - `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/run.sh`
  - `/Users/admin/.codex/worktrees/8c77/jazz-main/docs/jazz-language-state.md`

**Docs implications**

- Keep purity claims, but make them true and test-backed.
- Update examples so allowed/disallowed pure/impure calls are explicit and compilable.
- Candidate files:
  - `/Users/admin/.codex/worktrees/8c77/jazz-main/README.md`
  - `/Users/admin/.codex/worktrees/8c77/jazz-main/docs/jazz-language-state.md`

**Complexity/risk profile**

- Medium-high complexity, broad analyzer surface changes, highest long-term language value.

### Branch B: `!` Is Naming-Only Convention (No Purity Semantics)

**Parser implications**

- No semantic parser changes; keep `!` as identifier character.
- Add parser tests to lock this explicitly.
- Candidate files:
  - `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/src/Parser/Lib.hs` (likely unchanged)
  - `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/test/ParserSpec.hs`

**Typechecker implications**

- No purity checker introduced.
- Add tests proving calls to `name!` are typechecked like any other function call.
- Candidate files:
  - `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/src/Analyzer/TypeInference.hs` (likely unchanged)
  - `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/test/Analyzer/TypeInferenceSpec.hs`

**Codegen implications**

- Keep current `print!` lowering behavior, document it as builtin naming convention.
- Optional cleanup: centralize builtin-name mapping to avoid ad-hoc special-cases.
- Candidate file:
  - `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/src/CodeGen/Javascript.hs`

**Runtime implications**

- No change in runtime behavior.
- Clarify that side effects are permitted regardless of `!` naming.
- Candidate files:
  - `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/run.sh` (likely unchanged)
  - `/Users/admin/.codex/worktrees/8c77/jazz-main/docs/jazz-language-state.md`

**Docs implications**

- Remove/replace “pure by default” and “pure cannot call impure” claims.
- Reframe `!` as conventional suffix often used for side-effectful APIs.
- Candidate files:
  - `/Users/admin/.codex/worktrees/8c77/jazz-main/README.md`
  - `/Users/admin/.codex/worktrees/8c77/jazz-main/docs/jazz-language-state.md`

**Complexity/risk profile**

- Low-medium complexity, fastest cleanup, aligns immediately with current implementation reality.

## Recommended Execution Path

**Recommendation:** Execute **Branch B now** (naming-only semantics), and explicitly document Branch A as a future language-design track.

Rationale:

- It resolves the current spec contradiction quickly with minimal implementation risk.
- It matches current compiler behavior already observed in parser/analyzer/codegen.
- It avoids introducing partial or fragile effect machinery while broader analyzer features are still incomplete.

## Detailed Execution Plan (Recommended Branch B)

### Phase 0: Reproducible Environment + Baseline

- [ ] Enter pinned Nix shell and verify toolchain.
- [ ] Capture baseline test status before changes.

Commands:

```bash
export NIXPKGS_REF='github:NixOS/nixpkgs/68cc97d306d3187c142cfb2378852f28d47bc098'
nix --extra-experimental-features 'nix-command flakes' shell \
  "$NIXPKGS_REF#stack" \
  "$NIXPKGS_REF#nodejs_20" \
  -c bash -lc 'cd /Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs && stack --version && node --version && stack test'
```

Expected baseline outcome: current tests pass (or existing failures are recorded verbatim and carried forward unchanged).

**Commit checkpoint (optional, if baseline notes are committed):**

```bash
git add docs/plans/spec-cleanup/2026-03-02/decisions/03-purity-bang-semantics.md
git commit -m "docs(plan): capture purity bang decision baseline evidence"
```

### Phase 1: Spec/Docs Alignment to Naming-Only

- [ ] Update top-level docs to remove purity enforcement claims.
- [ ] Define canonical wording: `!` is naming convention, not compiler-enforced purity.
- [ ] Mark spec-cleanup item #3 as resolved in state-tracking docs.

Files:

- Modify `/Users/admin/.codex/worktrees/8c77/jazz-main/README.md`
- Modify `/Users/admin/.codex/worktrees/8c77/jazz-main/docs/jazz-language-state.md`

**Commit checkpoint:**

```bash
git add README.md docs/jazz-language-state.md
git commit -m "docs(spec): define bang suffix as naming convention only"
```

### Phase 2: Tests to Lock Behavior

- [ ] Add parser tests that identifiers ending with `!` parse as normal identifiers.
- [ ] Add analyzer/type-inference tests confirming calls to `name!` have no purity restriction.
- [ ] Keep tests narrow and behavior-based (no assumptions about future effect system internals).

Files:

- Modify `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/test/ParserSpec.hs`
- Modify `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/test/Analyzer/TypeInferenceSpec.hs`

Suggested test cases:

- Declaration and call of a user-defined `foo!` parses and infers.
- A non-`!` function calling `print!` remains valid.
- No error is raised solely because callee name ends with `!`.

**Commit checkpoint:**

```bash
git add jazz-hs/test/ParserSpec.hs jazz-hs/test/Analyzer/TypeInferenceSpec.hs
git commit -m "test: codify non-semantic bang suffix behavior"
```

### Phase 3: Optional Codegen Cleanup (Only If Needed)

- [ ] Decide whether to keep `print!` special-case as-is or refactor into explicit builtin mapping table.
- [ ] Perform cleanup only if it reduces ambiguity; avoid behavior changes in this cleanup item.

Files (only if changed):

- Modify `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/src/CodeGen/Javascript.hs`
- Modify `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/src/Types.hs`

**Commit checkpoint:**

```bash
git add jazz-hs/src/CodeGen/Javascript.hs jazz-hs/src/Types.hs
git commit -m "refactor(codegen): clarify print! builtin mapping"
```

### Phase 4: Reproducible Verification + Final Status Update

- [ ] Run full reproducible verification in pinned Nix shell.
- [ ] Verify docs and implementation no longer contradict each other.
- [ ] Confirm item #3 is no longer listed as undecided.

Commands:

```bash
export NIXPKGS_REF='github:NixOS/nixpkgs/68cc97d306d3187c142cfb2378852f28d47bc098'
nix --extra-experimental-features 'nix-command flakes' shell \
  "$NIXPKGS_REF#stack" \
  "$NIXPKGS_REF#nodejs_20" \
  -c bash -lc '
    set -euo pipefail
    cd /Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs
    stack test
    ./run.sh ExamplePrograms/TypeError.jz >/tmp/jazz-typeerror.out 2>&1 || true
    ./run.sh ExamplePrograms/MismatchedParens.jz >/tmp/jazz-parseerror.out 2>&1 || true
  '
```

Expected outcomes:

- `stack test` passes.
- Example program behavior is unchanged except where docs were clarified.
- No remaining doc text claims enforced purity semantics.

**Commit checkpoint:**

```bash
git add README.md docs/jazz-language-state.md jazz-hs/test/ParserSpec.hs jazz-hs/test/Analyzer/TypeInferenceSpec.hs
git commit -m "docs+tests: close spec-cleanup item 3 for bang semantics"
```

## Alternative Execution Plan (If Branch A Is Explicitly Chosen Instead)

Use only if product direction now requires enforced purity semantics.

- [ ] Add explicit purity/effect representation in AST/types.
- [ ] Enforce purity rule in analyzer with dedicated diagnostics.
- [ ] Add parser/analyzer tests for allowed vs disallowed call graphs.
- [ ] Keep codegen/runtime behavior aligned with analysis-time enforcement.
- [ ] Update docs/examples to be enforcement-accurate.

Suggested commit checkpoints for Branch A:

```bash
git add jazz-hs/src/AST.hs jazz-hs/src/Types.hs jazz-hs/src/Errors.hs
git commit -m "feat(types): introduce purity metadata for function semantics"
```

```bash
git add jazz-hs/src/Analyzer/TypeInference.hs jazz-hs/test/Analyzer/TypeInferenceSpec.hs
git commit -m "feat(analyzer): enforce pure-to-impure call restrictions"
```

```bash
git add jazz-hs/src/Parser/Lang.hs jazz-hs/test/ParserSpec.hs

git commit -m "test(parser): lock purity bang syntax and semantics"
```

```bash
git add README.md docs/jazz-language-state.md jazz-hs/src/CodeGen/Javascript.hs
git commit -m "docs+codegen: align purity semantics across spec and backend"
```

## Definition of Done

- [ ] Item #3 is no longer an open decision in `/Users/admin/.codex/worktrees/8c77/jazz-main/docs/jazz-language-state.md`.
- [ ] README and state doc agree on exactly one meaning of `!`.
- [ ] Tests cover the chosen meaning and pass in pinned Nix environment.
- [ ] Commit history shows staged, reviewable checkpoints with scoped messages.
