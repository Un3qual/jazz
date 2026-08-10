# PR 127 Review Follow-Through Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Resolve every current actionable PR 127 bot finding with behavior-level fixes, cluster duplicate threads, and answer stale or invalid findings with repository evidence.

**Architecture:** Keep existing authoritative boundaries: Cabal/package tests own source packaging, Docusaurus and the compact post-build checker own publication output, compiler-backed tests own Jazz syntax, and actionlint plus focused repository policy own workflows. Add no browser/YAML/Markdown parser and do not harden against repository authors who can edit the checker and CI.

**Tech Stack:** Haskell/GHC 9.14.1, Cabal, Python 3, Bash, Node.js 22, Docusaurus, Shiki/TextMate, GitHub Actions, actionlint, Nix

## Global Constraints

- Work only on PR 127 (`codex/project-operations`), based on PR 126 (`codex/docusaurus-website`).
- Treat 55 unresolved threads as duplicate clusters; do not produce one patch per bot comment.
- Preserve the approved trusted-contributor accidental-regression threat model.
- Add behavior tests before production fixes where executable behavior changes.
- Push once after verification, reply in-thread, and do not fetch review state after the push.

---

### Task 1: Public documentation and TextMate correctness

**Files:**

- Modify: `scripts/test-check-public-docs.py`
- Modify: `scripts/check-public-docs.py`
- Modify: `website/scripts/test-experience.mjs`
- Modify: `editors/vscode-jazz/syntaxes/jazz.tmLanguage.json`
- Modify: `.github/workflows/docs-pages.yml`
- Modify: `scripts/test-docs-pages-workflow.py`
- Modify: `scripts/check-docs-pages-workflow.py`
- Modify: `website/src/theme/CodeBlock/Content/index.tsx`

**Interfaces:**

- Consumes: required-page front matter, compiler fragment results, TextMate token scopes, Pages path triggers.
- Produces: literal `draft: true` rejection, operational compiler-failure rejection, contract-correct Unicode/numeric highlighting, and Pages rebuilds for grammar changes.

- [x] **Step 1: Add a public-doc regression where `draft: false` passes and a fake compiler exiting nonzero without a Jazz diagnostic fails.**
- [x] **Step 2: Run `python3 scripts/test-check-public-docs.py` and confirm both cases fail for the reported reasons.**
- [x] **Step 3: Restrict draft detection to literal true and require a successful compiler result or a structured non-syntax Jazz diagnostic.**
- [x] **Step 4: Add TextMate tokenization regressions for Unicode keyword suffixes and invalid integer/whole-number suffixes.**
- [x] **Step 5: Run `pnpm --dir website run test:experience` and confirm the new token expectations fail.**
- [x] **Step 6: Replace ASCII keyword/builtin boundaries with Unicode identifier lookarounds and restrict numeric suffixes to fractional `f16`, `f32`, or `f64`.**
- [x] **Step 7: Add the editor grammar to the Pages trigger contract; reject the word-wrap suggestion after the pinned Docusaurus type contract proved the requested property does not exist.**
- [x] **Step 8: Run focused public-doc, Pages, experience, typecheck, and website build checks.**

### Task 2: Release artifact identity and integrity

**Files:**

- Modify: `scripts/release/test-verify-artifacts.py`
- Modify: `scripts/release/verify-artifacts.py`
- Modify: `scripts/release/build-alpha.sh`
- Modify: `scripts/ci/release-candidate.sh`
- Modify: `scripts/check-website-boundary.py`
- Modify: `jazz.cabal`
- Modify: `release-notes/0.1.0-alpha.1.md`
- Modify: `.github/workflows/release.yml`

**Interfaces:**

- Consumes: requested alpha version, Cabal sdist metadata, Nix closure export, docs archive, release directory.
- Produces: version-consistent archives, CRC/trailer validation, executable Jazz closure roots, publication-boundary-checked docs, and atomic same-version builds.

- [x] **Step 1: Add verifier regressions for mismatched source version, truncated gzip, non-Jazz Nix root, and forbidden docs output.**
- [x] **Step 2: Run the focused verifier tests and confirm each new case fails against the current verifier.**
- [x] **Step 3: Validate gzip streams to EOF, compare the Cabal source version with the alpha line, inspect imported `bin/jazz`, and run the shared generated-output boundary checker on extracted docs.**
- [x] **Step 4: Change the valid Nix fixture to export an executable `bin/jazz` and retain a plain-store-object negative fixture.**
- [x] **Step 5: Add a concurrent-build regression proving one same-version build fails without corrupting the winner.**
- [x] **Step 6: Acquire an atomic per-version directory lock before checking or publishing the release directory.**
- [x] **Step 7: Include `flake.lock` in the sdist and Nix source, validate release/Cabal version alignment before expensive candidate work, and version-scope workflow concurrency.**
- [x] **Step 8: Correct release notes for repository-checkout verification and `/jazz/` static-site mounting.**
- [x] **Step 9: Run verifier tests, release fixture assembly, source-distribution audit, and actionlint.**

### Task 3: Protected CI execution

**Files:**

- Modify: `.github/workflows/ci-pr.yml`
- Modify: `.github/workflows/ci-main.yml`
- Modify: `.github/workflows/ci-extended.yml`
- Modify: `scripts/ci/fast-compiler.sh`
- Modify: `scripts/ci/main-functional.sh`
- Modify: `scripts/test-check-ci-policy.py`
- Modify: `scripts/check-ci-policy.py`

**Interfaces:**

- Consumes: PR/main base SHA, policy/verifier behavior suites, extended evidence outcome.
- Produces: protected execution of checker negative tests, committed-diff whitespace checks, and non-obscuring artifact upload behavior.

- [x] **Step 1: Add policy fixture mutations showing the behavior suites and CI diff base cannot be removed.**
- [x] **Step 2: Run `python3 scripts/test-check-ci-policy.py` and confirm the new mutations are red.**
- [x] **Step 3: Run policy tests in the always-on docs job, verifier tests in Nix-backed compiler/main tiers, and pass explicit GitHub base SHAs to full-history checkouts.**
- [x] **Step 4: Make fast/main whitespace checks compare base-to-HEAD when `JAZZ_DIFF_BASE` is set while retaining local working-tree checks.**
- [x] **Step 5: Change extended artifact absence to a warning because `extended.sh` itself requires a complete manifest on success.**
- [x] **Step 6: Run policy tests, live policy, actionlint, and base-to-HEAD `git diff --check`.**

### Task 4: Package/editor maintenance findings

**Files:**

- Modify: `test/Jazz/Repository/AuditSpec.hs`
- Modify: `test/Jazz/Repository/PackagePolicy.hs`
- Modify: `CONTRIBUTING.md`
- Modify: `CHANGELOG.md`
- Modify: `editors/vscode-jazz/package.json`
- Modify: `editors/vscode-jazz/README.md`
- Modify: `test/Jazz/Compiler/Bootstrap/JazzLexerParitySpec.hs`

**Interfaces:**

- Consumes: valid Cabal field spelling/continuations, source-distribution list, VSIX manifest, long lexer parity runs.
- Produces: normalized Cabal policy fields, removal of a non-behavioral untracked-file test, lean extension packaging, and bounded lexer tests.

- [x] **Step 1: Add package-policy cases for case, whitespace, comments, and continuation formatting and confirm current parsing rejects them.**
- [x] **Step 2: Normalize logical Cabal fields once, compare names case-insensitively, and limit legacy-name matching to token boundaries in metadata values.**
- [x] **Step 3: Delete the misleading untracked-sdist test/helper while retaining the real required/forbidden sdist inventory test.**
- [x] **Step 4: Restore bounded long-run parity, correct the Ormolu example and changelog comparison SHA, use the canonical manual extension directory, and omit fixtures from VSIX files.**
- [x] **Step 5: Run repository audit, lexer parity, Cabal build/check, and editor/brand tests.**

### Task 5: Verification, publication, and replies

**Files:**

- Review: all changes relative to `origin/codex/docusaurus-website`

**Interfaces:**

- Consumes: Tasks 1-4 and the cached 55-thread snapshot.
- Produces: one pushed PR 127 commit series plus evidence-backed thread replies.

- [x] **Step 1: Run focused suites, full docs/site gates, full Nix flake check, compiler/build checks, actionlint, formatters, Python compilation, and `git diff --check`.**
- [x] **Step 2: Review the diff for bot-specific wrappers, duplicated parsing, unreachable branches, and changes that conflict with the approved trust model.**
- [ ] **Step 3: Commit and push `HEAD:codex/project-operations`.**
- [ ] **Step 4: Reply to every actionable/duplicate thread with the pushed fix and verification, and answer stale/invalid threads with concrete current-tree evidence.**
- [ ] **Step 5: Stop without fetching PR review state again.**
