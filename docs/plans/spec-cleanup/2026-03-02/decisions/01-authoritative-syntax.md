# Jazz Spec-Cleanup #1: Authoritative Syntax Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Pick and enforce one authoritative syntax for functions, modules, traits, and collections across parser behavior, examples, and docs.

**Architecture:** Make a single syntax decision record first, then lock it in with parser tests, parser/type/codegen alignment, and docs/example updates. Use a compatibility-first migration path (accept old syntax temporarily only if needed), then remove ambiguity in docs.

**Tech Stack:** Haskell (`stack`, `hspec`, Megaparsec parser in `jazz-hs`), Markdown docs, Nix dev shell/flake checks for reproducibility.

---

## Plan Progress Checklist

- [ ] Discovery complete
- [x] Design decision complete
- [ ] Implementation complete
- [ ] Verification complete
- [ ] Docs updates complete

## Decision Lock (Approved 2026-03-02)

- [x] Canonical abstraction keyword is `class`/`impl`.
- [x] Canonical collection combinator order is function-first (`map f xs`, `filter p xs`).
- [x] Function/module surface continues parser-first style (`name = expr.`, `module A::B { ... }`, `import A::B`).

## Verification Evidence (Item Is Still Unfinished)

- `/Users/admin/.codex/worktrees/8c77/jazz-main/docs/jazz-language-state.md` contains the open cleanup list and explicitly includes: `1. Pick one authoritative syntax for functions, modules, traits, and collections.` (section `## Recommended Next Spec Cleanup`, around lines 418-423).
- `/Users/admin/.codex/worktrees/8c77/jazz-main/docs/jazz-language-state.md` also documents unresolved syntax conflicts, e.g. parser supports `class` while `Prelude.jz` uses `trait`, and collection API order disagreements (around lines 286-346).
- `/Users/admin/.codex/worktrees/8c77/jazz-main/docs` currently contains only one file (`jazz-language-state.md`), so there is no existing authoritative syntax decision doc.
- `/Users/admin/.codex/worktrees/8c77/jazz-main/docs/plans/spec-cleanup/2026-03-02` did not exist before this plan file was created, confirming no prior per-item decision artifact for item #1.
- Repo-wide search for `authoritative syntax` only points to `docs/jazz-language-state.md`, indicating no completed implementation/decision elsewhere.

## Scope

In scope for item #1:
- Choose one canonical syntax for:
  - function declarations/definitions
  - module/import forms
  - trait abstraction keyword and impl form
  - collection syntax surface (literal/type and call style documentation)
- Align implementation/tests/docs to that decision.

Out of scope for item #1 (track separately):
- Full semantic effect system for `!`.
- Full module loader/runtime architecture.
- Completing all parse-only features.
- Deep redesign of `jazz2`.

## Decision Criteria (Use During Execution)

Use these criteria to pick the final syntax if tradeoffs appear:
1. Existing `jazz-hs` parser/test behavior gets highest weight.
2. Minimize immediate runtime/typechecker breakage.
3. Make docs/examples executable against current implementation.
4. Keep migration cost bounded (prefer one canonical form + explicit deprecation note).

Locked canonical syntax:
- Functions: `name = <expr>.` with optional `name :: <type>.`, lambda `\(args) -> expr`.
- Modules/imports: `module A::B { ... }`, `import A::B`, optional `as`/function qualifier.
- Traits: canonical `class`/`impl` (because parser/AST currently use this).
- Collections: list literals/types stay `[x]` / `[a]`; collection combinator style is curried function-first (`map f xs`, `filter p xs`).

## Concrete Files To Modify During Execution

Create:
- `/Users/admin/.codex/worktrees/8c77/jazz-main/docs/spec/authoritative-syntax.md`
- `/Users/admin/.codex/worktrees/8c77/jazz-main/flake.nix`
- `/Users/admin/.codex/worktrees/8c77/jazz-main/flake.lock` (generated)

Modify:
- `/Users/admin/.codex/worktrees/8c77/jazz-main/README.md`
- `/Users/admin/.codex/worktrees/8c77/jazz-main/docs/jazz-language-state.md`
- `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/src/Parser/Lang.hs`
- `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/src/Parser/Lib.hs`
- `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/src/Types.hs`
- `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/src/CodeGen/Javascript.hs` (only if collection call style changes runtime lowering)
- `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/test/ParserSpec.hs`
- `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/static/Prelude.jz`
- `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/ExamplePrograms/ComplexProgram.jz`

### Task 1: Establish Reproducible Nix Dev Environment First

**Files:**
- Create: `/Users/admin/.codex/worktrees/8c77/jazz-main/flake.nix`
- Create/Update: `/Users/admin/.codex/worktrees/8c77/jazz-main/flake.lock`

**Step 1: Add a minimal flake dev shell for this cleanup work**
- Include tools needed for this item: `stack`, `ghc`, `cabal-install`, `ormolu`, `hlint`, `git`.
- Add at least one `checks` entry that runs parser tests (`stack test` in `jazz-hs`).

**Step 2: Validate the shell/checks are usable**
- Run: `cd /Users/admin/.codex/worktrees/8c77/jazz-main && nix flake show`
- Run: `cd /Users/admin/.codex/worktrees/8c77/jazz-main && nix develop -c stack --version`
- Run: `cd /Users/admin/.codex/worktrees/8c77/jazz-main && nix flake check`
- Expected: flake evaluates and check exits 0.

**Step 3: Commit checkpoint**
```bash
git add /Users/admin/.codex/worktrees/8c77/jazz-main/flake.nix /Users/admin/.codex/worktrees/8c77/jazz-main/flake.lock
git commit -m "build(nix): add reproducible dev shell for spec-cleanup"
```

### Task 2: Write and Ratify the Authoritative Syntax Decision Record

**Files:**
- Create: `/Users/admin/.codex/worktrees/8c77/jazz-main/docs/spec/authoritative-syntax.md`
- Modify: `/Users/admin/.codex/worktrees/8c77/jazz-main/docs/jazz-language-state.md`

**Step 1: Build a syntax matrix from current sources**
- Compare syntax in:
  - `/Users/admin/.codex/worktrees/8c77/jazz-main/README.md`
  - `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/src/Parser/Lang.hs`
  - `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/test/ParserSpec.hs`
  - `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/static/Prelude.jz`
  - `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/src/Types.hs`

**Step 2: Record the approved canonical syntax and migration policy**
- Decision record must include:
  - canonical grammar examples
  - accepted legacy forms (if any) and sunset plan
  - explicit non-goals for this item

**Step 4: Link decision from language state doc**
- Update cleanup section to reference the decision doc and mark item #1 as complete only after implementation/tests are done.

**Step 5: Commit checkpoint**
```bash
git add /Users/admin/.codex/worktrees/8c77/jazz-main/docs/spec/authoritative-syntax.md /Users/admin/.codex/worktrees/8c77/jazz-main/docs/jazz-language-state.md
git commit -m "docs(spec): record authoritative syntax decision for item #1"
```

### Task 3: Add Failing Parser/Surface Tests Before Code Changes

**Files:**
- Modify: `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/test/ParserSpec.hs`

**Step 1: Add tests for canonical syntax (and rejection/deprecation of non-canonical forms)**
- Add focused cases for:
  - function declaration/definition style
  - module/import syntax
  - trait/class declaration keyword choice
  - collection call examples consistent with decision

**Step 2: Run targeted tests and confirm failure first**
- Run: `cd /Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs && stack test --ta '--match "Tests of Statements"'`
- Run: `cd /Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs && stack test --ta '--match "typeclass declaration"'`
- Expected: new tests fail before parser updates.

**Step 3: Commit checkpoint (tests-only baseline)**
```bash
git add /Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/test/ParserSpec.hs
git commit -m "test(parser): codify authoritative syntax expectations"
```

### Task 4: Implement Parser/Type/Codegen Alignment to Pass New Tests

**Files:**
- Modify: `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/src/Parser/Lang.hs`
- Modify: `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/src/Parser/Lib.hs`
- Modify: `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/src/Types.hs`
- Modify (conditional): `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/src/CodeGen/Javascript.hs`

**Step 1: Apply minimal parser changes to match the decision**
- Update keyword parsing and grammar entry points only where needed.
- Keep compatibility parsing only if the decision explicitly allows a transition window.

**Step 2: Align builtin collection signatures and lowering if syntax choice requires it**
- Ensure `Types.hs` builtin signatures and JS lowering are consistent with canonical collection style.

**Step 3: Re-run parser tests until green**
- Run: `cd /Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs && stack test --ta '--match ParserSpec'`
- Then run full test suite: `cd /Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs && stack test`
- Expected: all pass.

**Step 4: Commit checkpoint**
```bash
git add /Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/src/Parser/Lang.hs /Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/src/Parser/Lib.hs /Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/src/Types.hs /Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/src/CodeGen/Javascript.hs
git commit -m "feat(parser): enforce authoritative syntax in compiler surface"
```

### Task 5: Sync README, Prelude, and Example Programs with Canonical Syntax

**Files:**
- Modify: `/Users/admin/.codex/worktrees/8c77/jazz-main/README.md`
- Modify: `/Users/admin/.codex/worktrees/8c77/jazz-main/docs/jazz-language-state.md`
- Modify: `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/static/Prelude.jz`
- Modify: `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/ExamplePrograms/ComplexProgram.jz`

**Step 1: Replace contradictory examples with canonical syntax**
- Ensure README examples are executable against `jazz-hs` expectations.
- Remove/annotate stale forms that conflict with item #1 decision.

**Step 2: Ensure prelude/examples no longer advertise conflicting syntax**
- If keeping temporary aliases, document canonical form + alias status clearly.

**Step 3: Commit checkpoint**
```bash
git add /Users/admin/.codex/worktrees/8c77/jazz-main/README.md /Users/admin/.codex/worktrees/8c77/jazz-main/docs/jazz-language-state.md /Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/static/Prelude.jz /Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/ExamplePrograms/ComplexProgram.jz
git commit -m "docs(jazz): align docs and examples with authoritative syntax"
```

### Task 6: Verification, Reproducibility, and Handoff

**Files:**
- Modify (if needed for handoff notes): `/Users/admin/.codex/worktrees/8c77/jazz-main/docs/jazz-language-state.md`

**Step 1: Run formatter/linter/test checks inside Nix shell**
- Run: `cd /Users/admin/.codex/worktrees/8c77/jazz-main && nix develop -c ormolu --mode check jazz-hs/src/Parser/Lang.hs jazz-hs/src/Parser/Lib.hs jazz-hs/src/Types.hs jazz-hs/test/ParserSpec.hs`
- Run: `cd /Users/admin/.codex/worktrees/8c77/jazz-main && nix develop -c hlint jazz-hs/src jazz-hs/test`
- Run: `cd /Users/admin/.codex/worktrees/8c77/jazz-main && nix develop -c bash -lc 'cd jazz-hs && stack test'`

**Step 2: Reproducible smoke check for syntax in an example**
- Run: `cd /Users/admin/.codex/worktrees/8c77/jazz-main && nix develop -c bash -lc 'cd jazz-hs && ./run.sh ExamplePrograms/ComplexProgram.jz'`
- Expected: parser/typecheck/codegen succeeds for the canonical syntax example.

**Step 3: Final repo sanity check**
- Run: `cd /Users/admin/.codex/worktrees/8c77/jazz-main && git status --short`
- Confirm only intended files changed.

**Step 4: Commit checkpoint (if any final edits)**
```bash
git add /Users/admin/.codex/worktrees/8c77/jazz-main/docs/jazz-language-state.md
git commit -m "chore(spec): finalize authoritative syntax verification notes"
```

## Risks

- Backward-compat break risk if non-canonical syntax is removed abruptly.
- Partial alignment risk if parser changes but docs/examples still mix forms.
- Collection surface risk if docs choose style not backed by `Types.hs`/codegen.
- Tooling risk if Nix checks are added but not wired to actual parser tests.

## Rollback Notes

- Revert in reverse checkpoint order to keep repo consistent:
  1. docs/example sync commit
  2. parser/type/codegen commit
  3. test spec commit
  4. decision-doc commit
  5. nix bootstrap commit
- If syntax migration is too disruptive, keep parser dual-accept temporarily but keep docs single-canonical and add explicit deprecation timeline.

## Completion Criteria

- One canonical syntax documented in `/docs/spec/authoritative-syntax.md`.
- Parser tests explicitly encode canonical forms and pass.
- Implementation, examples, and docs no longer contradict each other for functions/modules/traits/collections.
- Nix-based reproducible check path (`nix develop` + `nix flake check`) is available and passing.
