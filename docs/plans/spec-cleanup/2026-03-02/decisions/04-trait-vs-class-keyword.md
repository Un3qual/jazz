# Jazz Spec-Cleanup #4 Implementation Plan

> **Execution Note:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Decide one canonical abstraction keyword (`trait` or `class`) for Jazz, then align parser/docs/examples while providing a staged compatibility and deprecation path for the non-canonical keyword.

**Architecture:** Use a docs-first decision gate, then parser normalization that accepts canonical + compatibility alias short-term, emits deprecation warnings for the non-canonical keyword, and eventually enforces canonical-only syntax. Keep AST/type system internals stable while syntax converges.

**Tech Stack:** Markdown docs, Haskell (`megaparsec`, `hspec`, `stack`), Nix shell tooling.

---

## Progress Tracker

- [x] Verify item #4 is still unfinished in current sources.
- [ ] Establish Nix-based execution environment and baseline test status.
- [x] Run decision gate and record canonical keyword + rationale.
- [ ] Implement compatibility parsing/deprecation messaging for non-canonical keyword.
- [ ] Normalize docs/examples to canonical keyword.
- [ ] Validate parser/tests/docs end-to-end and finalize item #4 as resolved.

## Decision Lock (Approved 2026-03-02)

- [x] Canonical abstraction keyword is `class`.
- [x] `trait` is non-canonical and should follow the deprecation path defined in this plan.

## Verification Evidence (Item Is Still Unfinished)

- `docs/jazz-language-state.md:391`
  Observation: explicitly lists this as undecided: "Whether the standard abstraction keyword is `trait`, `class`, or both".
- `docs/jazz-language-state.md:425`
  Observation: "Recommended Next Spec Cleanup" item 4 is exactly "Decide whether `trait` or `class` is the canonical abstraction keyword."
- `jazz-hs/src/Parser/Lang.hs:160`
  Observation: parser declaration path currently hardcodes `symbolP "class"` (no `trait` parse branch).
- `jazz-hs/static/Prelude.jz:24`
  Observation: `Prelude.jz` uses `trait` in multiple declarations (and also `class` at line 64), confirming inconsistent keyword usage across repo sources.

## Scope And Non-Goals

- In scope: deciding canonical keyword, parser/docs consistency, compatibility and deprecation plan for non-canonical keyword.
- Out of scope: redesigning trait/typeclass semantics, changing constraint syntax (`@{...}:`), or broader language feature cleanup items (#1, #2, #3, #5, #6).

## Task 1: Environment Bootstrap + Baseline (Nix First)

**Files:**
- Modify: none (environment + verification only)

**Steps:**

1. [ ] Confirm Nix availability.
   Run: `nix --version`
   Expected: prints Nix version.
2. [ ] Enter the root Nix dev environment for Haskell work (use `nix develop`; keep `nix shell` as an explicit fallback only when needed).
   Run: `nix develop -c zsh -lc 'stack --version && ghc --version'`
   Expected: both tool versions print successfully.
3. [ ] Capture baseline parser/typeclass test behavior.
   Run: `nix develop -c zsh -lc 'cd jazz-hs && stack test --test-arguments=\"--match typeclass\"'`
   Expected: current tests pass (or failures are documented before any change).
4. [ ] Capture full `jazz-hs` baseline.
   Run: `nix develop -c zsh -lc 'cd jazz-hs && stack test'`
   Expected: baseline result recorded for comparison after keyword changes.

**Commit Checkpoint:** none (no repo changes expected).

## Task 2: Decision Gate (Canonical Keyword Selection)

**Files:**
- Modify: `docs/plans/spec-cleanup/2026-03-02/decisions/04-trait-vs-class-keyword.md`
- Modify: `docs/jazz-language-state.md`

**Decision Procedure (must be completed before parser edits):**

1. [ ] Score both options (`trait`, `class`) against explicit criteria:
   - implementation authority alignment (`jazz-hs` parser and tests)
   - docs/readability alignment (`README.md`, `Prelude.jz`, language narrative)
   - migration effort/risk
   - long-term naming clarity
2. [x] Pick one canonical keyword and record:
   - chosen keyword
   - why not the alternative
   - compatibility window start/end conditions
3. [ ] Mark item #4 as resolved in `docs/jazz-language-state.md` once decision text is finalized.

**Executor Choice Rule:**

- [x] Canonical keyword is fixed to parser-aligned `class`.
- [ ] Define migration window specifics for `trait` compatibility and warning-to-error timeline.

**Commit Checkpoint 1: Decision Docs**

Run:

```bash
git add docs/plans/spec-cleanup/2026-03-02/decisions/04-trait-vs-class-keyword.md docs/jazz-language-state.md
git commit -m "docs(spec): decide canonical abstraction keyword for Jazz"
```

## Task 3: Compatibility Layer + Deprecation Warnings (Parser/Test)

**Files:**
- Modify: `jazz-hs/src/Parser/Lang.hs`
- Modify: `jazz-hs/src/Parser/Lib.hs` (if reserved keyword handling needs updates)
- Modify: `jazz-hs/test/ParserSpec.hs`
- Optional Modify: `jazz-hs/src/Errors.hs` (if warning/error messaging is centralized there)

**Steps (TDD-first):**

1. [ ] Add failing parser tests for canonical keyword acceptance.
2. [ ] Add failing parser tests for non-canonical keyword compatibility acceptance during migration window.
3. [ ] Add failing test/assertion for deprecation signal when non-canonical keyword is used (warning text or structured diagnostic).
4. [ ] Implement minimal parser changes to satisfy tests while normalizing AST output to existing nodes (`EClass`, `EClassImpl`).
5. [ ] Keep runtime/typechecker internals keyword-agnostic (syntax-level decision only).
6. [ ] Re-run targeted + full tests.

**Validation Commands:**

- `nix develop -c zsh -lc 'cd jazz-hs && stack test --test-arguments=\"--match typeclass\"'`
- `nix develop -c zsh -lc 'cd jazz-hs && stack test'`

**Commit Checkpoint 2: Parser Compatibility**

Run:

```bash
git add jazz-hs/src/Parser/Lang.hs jazz-hs/src/Parser/Lib.hs jazz-hs/test/ParserSpec.hs jazz-hs/src/Errors.hs
git commit -m "feat(parser): support canonical keyword with deprecating alias"
```

If `jazz-hs/src/Errors.hs` was not changed, omit it from `git add`.

## Task 4: Canonicalization Of Docs And Examples

**Files:**
- Modify: `README.md`
- Modify: `docs/jazz-language-state.md`
- Modify: `jazz-hs/static/Prelude.jz`
- Modify: `docs/plans/spec-cleanup/2026-03-02/decisions/04-trait-vs-class-keyword.md`

**Steps:**

1. [ ] Update all normative examples to canonical keyword only.
2. [ ] Add a short migration note showing non-canonical keyword is deprecated and accepted only temporarily.
3. [ ] Ensure no contradictory statements remain between README, `jazz-language-state`, and `Prelude.jz`.
4. [ ] Re-run text search checks:
   - canonical keyword locations expected in docs/examples
   - non-canonical keyword only in migration/deprecation sections (except legacy compatibility tests)

**Validation Commands:**

- `rg -n "\\btrait\\b|\\bclass\\b" README.md docs jazz-hs/static/Prelude.jz -S`
- `nix develop -c zsh -lc 'cd jazz-hs && stack test --test-arguments=\"--match typeclass\"'`

**Commit Checkpoint 3: Docs/Example Canonicalization**

Run:

```bash
git add README.md docs/jazz-language-state.md jazz-hs/static/Prelude.jz docs/plans/spec-cleanup/2026-03-02/decisions/04-trait-vs-class-keyword.md
git commit -m "docs(lang): canonicalize abstraction keyword and add migration notes"
```

## Task 5: Non-Canonical Keyword Deprecation Path (Explicit Policy)

**Files:**
- Modify: `docs/plans/spec-cleanup/2026-03-02/decisions/04-trait-vs-class-keyword.md`
- Modify: `docs/jazz-language-state.md` (resolution status)

**Policy to implement and document:**

1. [ ] Phase 1 (immediate): canonical keyword is normative; non-canonical keyword is accepted with deprecation warning.
2. [ ] Phase 2 (next cleanup milestone): warnings become hard failures in CI/doc validation unless an explicit legacy mode is enabled.
3. [ ] Phase 3 (subsequent milestone): non-canonical keyword parsing removed by default; legacy mode (if retained) is explicitly temporary and documented with removal target.

**Required wording (must appear in docs):**

- [ ] "`trait` is deprecated; use `class`."

**Commit Checkpoint 4: Deprecation Policy Finalization**

Run:

```bash
git add docs/plans/spec-cleanup/2026-03-02/decisions/04-trait-vs-class-keyword.md docs/jazz-language-state.md
git commit -m "docs(spec): define deprecation timeline for non-canonical keyword"
```

## Final Verification Gate (Before Closing Item #4)

1. [ ] `git status --short` shows only intended files changed.
2. [ ] `nix develop -c zsh -lc 'cd jazz-hs && stack test'` passes.
3. [ ] `rg -n 'Decide whether \`trait\` or \`class\` is the canonical abstraction keyword' docs/jazz-language-state.md -S` no longer lists item #4 as unresolved.
4. [ ] Decision doc clearly states canonical keyword and deprecation timeline.

## Risks And Mitigations

- [ ] Risk: doc/parser drift reappears.
  Mitigation: enforce keyword usage checks in CI grep/test commands.
- [ ] Risk: users rely on old keyword.
  Mitigation: compatibility window + explicit warning text + timeline.
- [ ] Risk: partial updates break examples/tests.
  Mitigation: TDD in parser tests plus full `stack test` verification in Nix shell.
