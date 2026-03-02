# Spec Clarification Item #12 Plan: Backend Target Strategy

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to execute this plan phase-by-phase.

**Goal:** Resolve backend ambiguity by adopting one authoritative strategy: `JavaScript only`, `LLVM only`, or `dual target`.

**Architecture:** Separate clarification from implementation. First freeze contradictions and decision criteria, then produce comparable option dossiers, then ratify one ADR-backed strategy and documentation contract before any backend refactor.

**Tech Stack:** Markdown docs, git history evidence, `jazz-hs` sources/tests, Nix shell validation commands.

---

## Progress Tracker

- [x] Baseline contradiction inventory completed from current code/specs (2026-03-02)
- [x] Historical drift sampled from old commits (2026-03-02)
- [ ] Phase 1 complete: Decision rubric and constraints frozen
- [ ] Phase 2 complete: JS-only vs LLVM-only vs dual dossiers completed
- [ ] Phase 3 complete: ADR decision ratified and top-level docs aligned
- [ ] Phase 4 complete: Documentation contract + validation matrix adopted

## Verification Evidence (Path-Based Contradictions)

- **C1: Public docs advertise LLVM direction while executable pipeline is JS-only.**
  - Claim: `/Users/admin/.codex/worktrees/8c77/jazz-main/README.md:10` says "Will generate LLVM IR in the future."
  - Implementation: `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/src/Lib.hs:19` imports only `CodeGen.Javascript`.
  - Implementation: `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/src/Lib.hs:61` defines `generateJSForJazz` as the exposed compile path.
  - Implementation: `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/app/Main.hs:18` always prints JS output from `generateJSForJazz`.

- **C2: Current state doc confirms JS reality but still marks backend target unresolved.**
  - Current-state fact: `/Users/admin/.codex/worktrees/8c77/jazz-main/docs/jazz-language-state.md:26` says current end-to-end implementation compiles to JavaScript.
  - Aspirational/spec signal: `/Users/admin/.codex/worktrees/8c77/jazz-main/docs/jazz-language-state.md:36` says eventually LLVM-backed.
  - Explicit unresolved item: `/Users/admin/.codex/worktrees/8c77/jazz-main/docs/jazz-language-state.md:399` says target is undecided (`JavaScript`, `LLVM`, or both).

- **C3: Repo contains dormant LLVM/QBE-era artifacts without active backend wiring.**
  - Legacy design comment: `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/src/Types.hs:101` references a runtime linked with LLVM code.
  - Stub module: `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/src/CodeGen/Builtins.hs:3` imports `Language.QBE`, but `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/src/CodeGen/Builtins.hs:6` onward is commented scaffolding only.
  - Build config drift: `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/stack.yaml:38` and `:39` keep LLVM local deps commented out; `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/stack.yaml:53` to `:56` keep `llvm-codegen`/`llvm-hs` lines commented.
  - Build deps mismatch signal: `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/jazz.cabal:73`, `:113`, `:159` include `qbe`, while no active non-JS backend module is exposed beyond `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/src/CodeGen/Javascript.hs`.

- **C4: Historical code shows target drift (stronger LLVM package inclusion in old code, absent in current execution path).**
  - Old code evidence: `5888271:jazz-hs/stack.yaml:38` had `- ./local-deps/llvm-hs-pure` uncommented in packages.
  - Current code evidence: `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/stack.yaml:38` has that same line commented (`# - ./local-deps/llvm-hs-pure`).

- **C5: `jazz2` cannot currently resolve backend strategy as an authority.**
  - Incomplete parser pipeline: `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz2/src/Jazz/Parser/Lexer.hs:6` is `undefined`.
  - Incomplete parser exports: `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz2/src/Jazz/Parser.hs:1` to `:8` is mostly commented placeholders.

**Conclusion:** Backend target policy is currently under-specified and contradicted across README claims, active compiler behavior, and legacy artifacts.

## Scope Guardrails

- [ ] This clarification item chooses/documents strategy; it does not implement backend rewrites.
- [ ] Every strategy statement must cite concrete repository evidence (path + line).
- [ ] Decision output must distinguish `implemented now`, `committed next`, and `long-term aspirational`.
- [ ] Do not treat `jazz2` as behavioral authority for current backend decisions.

## Suggested File Split (Recommended)

If this item is split into additional documents, use these exact paths:

- `/Users/admin/.codex/worktrees/8c77/jazz-main/docs/spec/backend-target/00-evidence-inventory.md`
- `/Users/admin/.codex/worktrees/8c77/jazz-main/docs/spec/backend-target/01-decision-rubric.md`
- `/Users/admin/.codex/worktrees/8c77/jazz-main/docs/spec/backend-target/02-option-js-only.md`
- `/Users/admin/.codex/worktrees/8c77/jazz-main/docs/spec/backend-target/03-option-llvm-only.md`
- `/Users/admin/.codex/worktrees/8c77/jazz-main/docs/spec/backend-target/04-option-dual-target.md`
- `/Users/admin/.codex/worktrees/8c77/jazz-main/docs/decisions/adr-0012-backend-target-strategy.md`
- `/Users/admin/.codex/worktrees/8c77/jazz-main/docs/spec/backend-target/05-backend-contract.md`
- `/Users/admin/.codex/worktrees/8c77/jazz-main/docs/spec/backend-target/06-validation-matrix.md`
- `/Users/admin/.codex/worktrees/8c77/jazz-main/docs/spec/backend-target/07-migration-playbook.md`

## Phased Clarification Plan

### Phase 1: Freeze Decision Rubric and Constraints

**Milestone:** A shared scoring model exists before debating options.

**Tasks:**
- [ ] Create evidence inventory with contradiction IDs (`C1..Cn`) and source links.
- [ ] Define mandatory decision criteria (semantics fidelity, implementation cost, tooling burden, test complexity, migration risk, contributor ergonomics).
- [ ] Define scoring policy (for example 1-5) and tie-break rule.
- [ ] Leave criteria weighting open for executor+maintainer choice after evidence review.

**Validation Criteria:**
- [ ] Each criterion has a definition and a measurable acceptance test.
- [ ] Each contradiction in this plan appears in the evidence inventory with exact path anchors.
- [ ] No option-specific recommendation appears before rubric signoff.

**Commit checkpoint:**

Commit message:

`docs(backend): freeze evidence inventory and decision rubric`

Exact `git add` targets:

```bash
git add \
  /Users/admin/.codex/worktrees/8c77/jazz-main/docs/plans/spec-clarification/2026-03-02/runtime/12-backend-target-strategy.md \
  /Users/admin/.codex/worktrees/8c77/jazz-main/docs/spec/backend-target/00-evidence-inventory.md \
  /Users/admin/.codex/worktrees/8c77/jazz-main/docs/spec/backend-target/01-decision-rubric.md
```

### Phase 2: Produce Comparable Option Dossiers

**Milestone:** Three strategy dossiers are complete and scoreable on the same rubric.

**Tasks:**
- [ ] Write JS-only dossier with explicit de-scoping of LLVM artifacts and compatibility impact.
- [ ] Write LLVM-only dossier with migration boundary from current JS pipeline and bootstrap path.
- [ ] Write dual-target dossier with shared IR/runtime contract assumptions and parity obligations.
- [ ] For each option, include short-term (`0-3 months`), medium-term (`3-9 months`), and long-term (`9+ months`) milestones.
- [ ] For each option, include explicit unknowns where executor research is required before a go/no-go.

**Validation Criteria:**
- [ ] All three dossiers include: architecture sketch, migration cost, risk register, test strategy, and rollback plan.
- [ ] All three dossiers use the same rubric headings for apples-to-apples scoring.
- [ ] No dossier claims implementation status without evidence.

**Commit checkpoint:**

Commit message:

`docs(backend): add js-only llvm-only dual-target option dossiers`

Exact `git add` targets:

```bash
git add \
  /Users/admin/.codex/worktrees/8c77/jazz-main/docs/spec/backend-target/02-option-js-only.md \
  /Users/admin/.codex/worktrees/8c77/jazz-main/docs/spec/backend-target/03-option-llvm-only.md \
  /Users/admin/.codex/worktrees/8c77/jazz-main/docs/spec/backend-target/04-option-dual-target.md
```

### Phase 3: Ratify Strategy in ADR + Align Product Docs

**Milestone:** One backend strategy is authoritative and visible in top-level docs.

**Tasks:**
- [ ] Select winning strategy using rubric totals plus maintainer override rationale (if any).
- [ ] Record decision in ADR with rejected alternatives and future revisit triggers.
- [ ] Update top-level docs so strategy language is consistent across README and language-state docs.
- [ ] Explicitly mark whether non-selected strategies are `deferred` or `rejected`.

**Validation Criteria:**
- [ ] ADR contains: status, date, context, decision, consequences, alternatives considered.
- [ ] `/README.md` and `/docs/jazz-language-state.md` no longer conflict on target strategy wording.
- [ ] Unresolved target ambiguity item is replaced with the adopted policy statement.

**Commit checkpoint:**

Commit message:

`docs(decision): adopt backend target strategy and align repository docs`

Exact `git add` targets:

```bash
git add \
  /Users/admin/.codex/worktrees/8c77/jazz-main/docs/decisions/adr-0012-backend-target-strategy.md \
  /Users/admin/.codex/worktrees/8c77/jazz-main/README.md \
  /Users/admin/.codex/worktrees/8c77/jazz-main/docs/jazz-language-state.md
```

### Phase 4: Publish Documentation Contract + Validation Matrix

**Milestone:** Anti-drift contracts and repeatable validation commands are in place.

**Tasks:**
- [ ] Define backend documentation contract (source-of-truth files, required synchronized updates, ownership).
- [ ] Define per-strategy acceptance matrix (what must pass for JS-only, LLVM-only, or dual target).
- [ ] Define migration playbook for legacy artifacts (for example `qbe`/commented LLVM deps/stubs) based on selected strategy.
- [ ] Define review gate checklist required before closing Item #12.

**Validation Criteria:**
- [ ] Contract names exactly which file is authoritative for policy (`ADR`) vs implementation status (`language-state`).
- [ ] Validation matrix includes command-level checks and expected outcomes.
- [ ] Playbook explicitly labels each legacy artifact as keep/remove/archive with rationale.

**Commit checkpoint:**

Commit message:

`docs(contract): define backend target contract validation matrix and migration playbook`

Exact `git add` targets:

```bash
git add \
  /Users/admin/.codex/worktrees/8c77/jazz-main/docs/spec/backend-target/05-backend-contract.md \
  /Users/admin/.codex/worktrees/8c77/jazz-main/docs/spec/backend-target/06-validation-matrix.md \
  /Users/admin/.codex/worktrees/8c77/jazz-main/docs/spec/backend-target/07-migration-playbook.md
```

## Documentation Contracts (What Must Be True After Phase 4)

- [ ] **Contract A (Policy Authority):** `/Users/admin/.codex/worktrees/8c77/jazz-main/docs/decisions/adr-0012-backend-target-strategy.md` is the only canonical target-policy source.
- [ ] **Contract B (Implementation Status):** `/Users/admin/.codex/worktrees/8c77/jazz-main/docs/jazz-language-state.md` tracks what is actually implemented today.
- [ ] **Contract C (Public Summary):** `/Users/admin/.codex/worktrees/8c77/jazz-main/README.md` may summarize strategy, but must link to ADR + language-state and avoid unstated commitments.
- [ ] **Contract D (Change Discipline):** Any PR changing backend behavior must update policy/status docs in the same PR or document a deliberate deferral.
- [ ] **Contract E (Legacy Artifact Registry):** All LLVM/QBE-related remnants are cataloged with `keep/remove/defer` status.

## Nix Reproducibility Commands for Validation

### Baseline command set (works without repo `flake.nix`)

```bash
export NIXPKGS_REF='github:NixOS/nixpkgs/68cc97d306d3187c142cfb2378852f28d47bc098'
nix --extra-experimental-features 'nix-command flakes' shell \
  "$NIXPKGS_REF#git" \
  "$NIXPKGS_REF#ripgrep" \
  "$NIXPKGS_REF#stack" \
  "$NIXPKGS_REF#ghc" \
  "$NIXPKGS_REF#nodejs_22" \
  --command zsh -lc '
    cd /Users/admin/.codex/worktrees/8c77/jazz-main &&
    rg -n "Will generate LLVM IR in the future" README.md &&
    rg -n "compiled to JavaScript|eventual target is JavaScript, LLVM, or both" docs/jazz-language-state.md &&
    rg -n "CodeGen.Javascript|generateJSForJazz" jazz-hs/src/Lib.hs jazz-hs/app/Main.hs &&
    rg -n "llvm|qbe|Language.QBE" jazz-hs/src/Types.hs jazz-hs/stack.yaml jazz-hs/src/CodeGen/Builtins.hs jazz-hs/jazz.cabal &&
    cd jazz-hs &&
    stack test &&
    stack run -- ExamplePrograms/ComplexProgram.jz > /tmp/jazz-backend-check.js &&
    node /tmp/jazz-backend-check.js
  '
```

### Post-ADR strategy validation templates (executor finalizes exact commands)

- [ ] JS-only acceptance command template (must include full `stack test` + generated JS runtime check under pinned Nix shell).
- [ ] LLVM-only acceptance command template (must include backend selection invocation, IR emission check, and compile/link smoke test under pinned Nix shell).
- [ ] Dual-target acceptance command template (must include target parity suite: same input program, same observable semantics across JS and LLVM outputs).

### Future migration to `nix develop`

- [ ] If repo-level `flake.nix` is added later, mirror every command above as `nix develop -c <command>` and keep both command paths documented until legacy path is removed.

## Final Exit Criteria for Item #12

- [ ] One target strategy is explicitly selected (or a time-boxed dual-phase decision is documented).
- [ ] Contradictions `C1..C5` are either resolved or explicitly accepted with owner + follow-up date.
- [ ] ADR + README + language-state are aligned with no target-policy conflict.
- [ ] Validation matrix commands are runnable in pinned Nix environment.
- [ ] Legacy backend artifact policy (`keep/remove/defer`) is published.

## Checkbox Summary

- [x] Verification evidence captured with exact path-based contradictions
- [x] Detailed phased plan with milestones and validation criteria defined
- [x] Commit checkpoints include messages and exact `git add` targets
- [x] Nix reproducibility command set included
- [x] Suggested split file paths provided
