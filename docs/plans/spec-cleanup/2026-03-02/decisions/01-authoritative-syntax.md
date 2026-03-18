# Jazz Spec-Cleanup #1: Authoritative Syntax Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Pick and enforce one authoritative syntax for functions, modules, traits, and collections across parser behavior, examples, and docs.

**Architecture:** Make a single syntax decision record first, then lock it in with parser tests, parser/type/codegen alignment, and docs/example updates. Use a compatibility-first migration path (accept old syntax temporarily only if needed), then remove ambiguity in docs.

**Tech Stack:** Historical execution notes below reference Haskell (`stack`, `hspec`, Megaparsec parser in legacy `jazz-hs`), Markdown docs, and Nix dev shell/flake checks for reproducibility. Active compiler follow-up for this decision now lands in `jazz-next`.

---

## Plan Progress Checklist

- [x] Discovery complete
- [x] Design decision complete
- [ ] Implementation complete
- [ ] Verification complete
- [ ] Docs updates complete

## Implementation Status Verification (2026-03-17, `jazz-next`)

- [x] Re-verified that canonical module declarations were already implemented in active `jazz-next`, so they were not the next code batch for this item.
- [x] Re-verified that active `jazz-next` still lacked canonical lambda parsing/runtime support, which kept first-class functions and currying backed only by legacy evidence in `docs/feature-status.md`.
- [x] Added a `jazz-next` execution plan for the lambda slice at `docs/plans/2026-03-17-jazz-next-lambda-support.md`.
- [x] Added canonical lambda parser/lowering coverage in `jazz-next/test/JazzNext/Compiler/Parser/LambdaParserSpec.hs`.
- [x] Added callable closure/type/runtime coverage in `jazz-next/test/JazzNext/Compiler/Semantics/LambdaSemanticsSpec.hs`.
- [x] Implemented active-path lambda support in:
  - `jazz-next/src/JazzNext/Compiler/Parser/Lexer.hs`
  - `jazz-next/src/JazzNext/Compiler/Parser.hs`
  - `jazz-next/src/JazzNext/Compiler/Parser/AST.hs`
  - `jazz-next/src/JazzNext/Compiler/Parser/Lower.hs`
  - `jazz-next/src/JazzNext/Compiler/AST.hs`
  - `jazz-next/src/JazzNext/Compiler/Analyzer.hs`
  - `jazz-next/src/JazzNext/Compiler/TypeInference.hs`
  - `jazz-next/src/JazzNext/Compiler/Runtime.hs`
- [x] Ran `bash jazz-next/scripts/test-warning-config.sh` after the lambda batch and all suites passed.
- [x] Updated top-level status docs so active `jazz-next/` lambda support no longer depends on legacy-only evidence.

## Decision Lock (Approved 2026-03-02)

- [x] Canonical abstraction keyword is `class`/`impl`.
- [x] Canonical collection combinator order is function-first (`map f xs`, `filter p xs`).
- [x] Function/module surface continues parser-first style (`name = expr.`, `module A::B { ... }`, `import A::B`).

## Verification Evidence (Item Is Still Unfinished)

- `docs/jazz-language-state.md` contains the open cleanup list and explicitly includes: `1. Pick one authoritative syntax for functions, modules, traits, and collections.` (section `## Recommended Next Spec Cleanup`, around lines 418-423).
- `docs/jazz-language-state.md` also documents unresolved syntax conflicts, e.g. parser supports `class` while `Prelude.jz` uses `trait`, and collection API order disagreements (around lines 286-346).
- `docs` currently contains only one file (`jazz-language-state.md`), so there is no existing authoritative syntax decision doc.
- `docs/plans/spec-cleanup/2026-03-02` did not exist before this plan file was created, confirming no prior per-item decision artifact for item #1.
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
1. Existing legacy `jazz-hs` parser/test behavior gets highest weight as historical evidence.
2. Minimize immediate runtime/typechecker breakage.
3. Make docs/examples executable against current implementation.
4. Keep migration cost bounded (prefer one canonical form + explicit deprecation note).

Locked canonical syntax:
- Functions: `name = <expr>.` with optional `name :: <type>.`, lambda `\(args) -> expr`.
- Modules/imports: `module A::B { ... }`, `import A::B`, optional `as`/function qualifier.
- Traits: canonical `class`/`impl` (because parser/AST currently use this).
- Collections: list literals/types stay `[x]` / `[a]`; collection combinator style is curried function-first (`map f xs`, `filter p xs`).

## Historical Legacy Files Modified During 2026-03-02 Execution

The remaining task breakdown below is preserved as a historical execution record for the March 2 legacy `jazz-hs` cleanup path. Treat it as archival context rather than current active implementation guidance; active compiler follow-up for this decision now belongs in `jazz-next`.

Create:
- `docs/spec/authoritative-syntax.md`
- `flake.nix`
- `flake.lock` (generated)

Modify:
- `README.md`
- `docs/jazz-language-state.md`
- `jazz-hs/src/Parser/Lang.hs`
- `jazz-hs/src/Parser/Lib.hs`
- `jazz-hs/src/Types.hs`
- `jazz-hs/src/CodeGen/Javascript.hs` (only if collection call style changes runtime lowering)
- `jazz-hs/test/ParserSpec.hs`
- `jazz-hs/static/Prelude.jz`
- `jazz-hs/ExamplePrograms/ComplexProgram.jz`

### Task 1: Establish Reproducible Nix Dev Environment First

**Files:**
- Create: `flake.nix`
- Create/Update: `flake.lock`

**Step 1: Add a minimal flake dev shell for this cleanup work**
- Include tools needed for this item: `stack`, `ghc`, `cabal-install`, `ormolu`, `hlint`, `git`.
- Add at least one `checks` entry that runs parser tests (`stack test` in `jazz-hs`).

**Step 2: Validate the shell/checks are usable**
- Run: `cd . && nix flake show`
- Run: `cd . && nix develop -c stack --version`
- Run: `cd . && nix flake check`
- Expected: flake evaluates and check exits 0.

**Step 3: Commit checkpoint**
```bash
git add flake.nix flake.lock
git commit -m "build(nix): add reproducible dev shell for spec-cleanup"
```

**Task 1 Execution Tracking (2026-03-02, worktree `5b7d`)**
- [x] Step 1 complete: Added root `flake.nix` with `devShells.default` (`stack`, `ghc`, `cabal-install`, `ormolu`, `hlint`, `git`) and `checks.parser-tests`.
- [x] Step 2 complete: Verified `nix flake show`, `nix develop -c stack --version`, and `nix flake check` using `NIX_CONFIG='experimental-features = nix-command flakes'`.
- [x] Step 3 complete: Committed the Nix bootstrap checkpoint.

### Task 2: Write and Ratify the Authoritative Syntax Decision Record

**Files:**
- Create: `docs/spec/authoritative-syntax.md`
- Modify: `docs/jazz-language-state.md`

**Step 1: Build a syntax matrix from current sources**
- Compare syntax in:
  - `README.md`
  - `jazz-hs/src/Parser/Lang.hs`
  - `jazz-hs/test/ParserSpec.hs`
  - `jazz-hs/static/Prelude.jz`
  - `jazz-hs/src/Types.hs`

**Step 2: Record the approved canonical syntax and migration policy**
- Decision record must include:
  - canonical grammar examples
  - accepted legacy forms (if any) and sunset plan
  - explicit non-goals for this item

**Step 4: Link decision from language state doc**
- Update cleanup section to reference the decision doc and mark item #1 as complete only after implementation/tests are done.

**Step 5: Commit checkpoint**
```bash
git add docs/spec/authoritative-syntax.md docs/jazz-language-state.md
git commit -m "docs(spec): record authoritative syntax decision for item #1"
```

**Task 2 Execution Tracking (2026-03-02, worktree `8fa2`)**
- [x] Step 1 complete: built the syntax matrix from `README.md`, legacy parser/tests, prelude reference, and legacy builtin signatures.
- [x] Step 2 complete: created `docs/spec/authoritative-syntax.md` with canonical syntax, migration notes, and explicit non-goals.
- [x] Step 4 complete: linked decision status from `docs/jazz-language-state.md` while keeping implementation/testing work open.
- [x] Step 5 complete: commit this Task 2 docs checkpoint.

### Task 3: Add Failing Parser/Surface Tests Before Code Changes

**Files:**
- Modify: `jazz-hs/test/ParserSpec.hs`

**Step 1: Add tests for canonical syntax (and rejection/deprecation of non-canonical forms)**
- Add focused cases for:
  - function declaration/definition style
  - module/import syntax
  - trait/class declaration keyword choice
  - collection call examples consistent with decision

**Step 2: Run targeted tests and confirm failure first**
- Run: `cd jazz-hs && stack test --ta '--match "Tests of Statements"'`
- Run: `cd jazz-hs && stack test --ta '--match "typeclass declaration"'`
- Expected: new tests fail before parser updates.

**Step 3: Commit checkpoint (tests-only baseline)**
```bash
git add jazz-hs/test/ParserSpec.hs
git commit -m "test(parser): codify authoritative syntax expectations"
```

### Task 4: Implement Parser/Type/Codegen Alignment to Pass New Tests

**Files:**
- Modify: `jazz-hs/src/Parser/Lang.hs`
- Modify: `jazz-hs/src/Parser/Lib.hs`
- Modify: `jazz-hs/src/Types.hs`
- Modify (conditional): `jazz-hs/src/CodeGen/Javascript.hs`

**Step 1: Apply minimal parser changes to match the decision**
- Update keyword parsing and grammar entry points only where needed.
- Keep compatibility parsing only if the decision explicitly allows a transition window.

**Step 2: Align builtin collection signatures and lowering if syntax choice requires it**
- Ensure `Types.hs` builtin signatures and JS lowering are consistent with canonical collection style.

**Step 3: Re-run parser tests until green**
- Run: `cd jazz-hs && stack test --ta '--match ParserSpec'`
- Then run full test suite: `cd jazz-hs && stack test`
- Expected: all pass.

**Step 4: Commit checkpoint**
```bash
git add jazz-hs/src/Parser/Lang.hs jazz-hs/src/Parser/Lib.hs jazz-hs/src/Types.hs jazz-hs/src/CodeGen/Javascript.hs
git commit -m "feat(parser): enforce authoritative syntax in compiler surface"
```

### Task 5: Sync README, Prelude, and Example Programs with Canonical Syntax

**Files:**
- Modify: `README.md`
- Modify: `docs/jazz-language-state.md`
- Modify: `jazz-hs/static/Prelude.jz`
- Modify: `jazz-hs/ExamplePrograms/ComplexProgram.jz`

**Step 1: Replace contradictory examples with canonical syntax**
- Ensure README examples are executable against `jazz-hs` expectations.
- Remove/annotate stale forms that conflict with item #1 decision.

**Step 2: Ensure prelude/examples no longer advertise conflicting syntax**
- If keeping temporary aliases, document canonical form + alias status clearly.

**Step 3: Commit checkpoint**
```bash
git add README.md docs/jazz-language-state.md jazz-hs/static/Prelude.jz jazz-hs/ExamplePrograms/ComplexProgram.jz
git commit -m "docs(jazz): align docs and examples with authoritative syntax"
```

### Task 6: Verification, Reproducibility, and Handoff

**Files:**
- Modify (if needed for handoff notes): `docs/jazz-language-state.md`

**Step 1: Run formatter/linter/test checks inside Nix shell**
- Run: `cd . && nix develop -c ormolu --mode check jazz-hs/src/Parser/Lang.hs jazz-hs/src/Parser/Lib.hs jazz-hs/src/Types.hs jazz-hs/test/ParserSpec.hs`
- Run: `cd . && nix develop -c hlint jazz-hs/src jazz-hs/test`
- Run: `cd . && nix develop -c bash -lc 'cd jazz-hs && stack test'`

**Step 2: Reproducible smoke check for syntax in an example**
- Run: `cd . && nix develop -c bash -lc 'cd jazz-hs && ./run.sh ExamplePrograms/ComplexProgram.jz'`
- Expected: parser/typecheck/codegen succeeds for the canonical syntax example.

**Step 3: Final repo sanity check**
- Run: `cd . && git status --short`
- Confirm only intended files changed.

**Step 4: Commit checkpoint (if any final edits)**
```bash
git add docs/jazz-language-state.md
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
