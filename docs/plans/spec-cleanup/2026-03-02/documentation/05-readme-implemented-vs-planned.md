# Spec Cleanup Item #5 Implementation Plan

> **Execution Note:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Separate "implemented today" features from planned/aspirational features in Jazz top-level documentation so readers can trust what works now.

**Architecture:** Treat implementation-backed facts as the source of truth, store a canonical status list in docs, and keep README as a high-signal summary that links to that canonical list. Add lightweight verification and maintenance checks to keep docs aligned with implementation over time.

**Tech Stack:** Markdown (`README.md`, docs pages), git, `rg`/`find` for verification, optional Nix dev shell hooks for docs checks/formatting.

---

## Verification Evidence (2026-03-02)

- `README.md:7-18` contains one `### Features` list with mixed status claims; it includes a future statement (`Will generate LLVM IR in the future`) in the same list as present-tense features.
- `README.md` has no explicit headings separating implemented vs planned features (`rg -n "Implemented|Planned" README.md` returns no matches).
- `docs/jazz-language-state.md:26` says the implemented subset is "much smaller" than broader repo claims.
- `docs/jazz-language-state.md:426` still lists this exact cleanup as pending.
- `docs` currently contains only `docs/jazz-language-state.md`; there is no dedicated top-level implemented-vs-planned status doc.

**Conclusion:** Item #5 is still unfinished.

## Scope Guardrails

- [ ] Only documentation and docs-tooling files are in scope.
- [ ] Do not change compiler/runtime/parser/typechecker behavior for this item.
- [ ] Keep claims evidence-backed by current repository behavior (primarily `jazz-hs`).

## Progress Tracker

- [ ] Task 1: Build feature-status source of truth
- [ ] Task 2: Split top-level README into implemented vs planned
- [ ] Task 3: Align `docs/jazz-language-state.md` with the new split
- [ ] Task 4: Add Nix-aware docs check/formatter tasks (conditional)
- [ ] Task 5: Add anti-drift maintenance checks
- [ ] Task 6: Run verification commands and prepare merge notes

### Task 1: Build feature-status source of truth

**Files:**
- Create: `docs/feature-status.md`
- Read for evidence:
  - `README.md`
  - `docs/jazz-language-state.md`
  - `jazz-hs/src/Lib.hs`
  - `jazz-hs/src/Parser/Lang.hs`
  - `jazz-hs/src/Types.hs`

**Steps:**
- [ ] Create a short rubric defining exactly three labels:
  - `Implemented Today` (end-to-end in current repo)
  - `Partially Implemented / Parse-Only`
  - `Planned / Aspirational`
- [ ] Add a feature table with one row per user-visible feature claim from top-level docs.
- [ ] For each row, include a short evidence pointer (file path + short rationale).
- [ ] Add `Last verified against commit: <sha>` placeholder so future updates can anchor to a revision.

**Commit checkpoint:**
```bash
git add docs/feature-status.md
git commit -m "docs: add canonical feature status matrix"
```

### Task 2: Split top-level README into implemented vs planned

**Files:**
- Modify: `README.md`
- Reference: `docs/feature-status.md`

**Steps:**
- [ ] Replace the current single `### Features` list with two explicit sections:
  - `### Implemented Today (verified)`
  - `### Planned / Aspirational`
- [ ] Move each existing feature bullet into the correct section using Task 1 rubric.
- [ ] Keep copy concise in README; link to `docs/feature-status.md` for detailed status/evidence.
- [ ] Ensure future-tense language appears only in the planned section.
- [ ] If examples include non-implemented behavior, mark them as aspirational or move them under planned notes.

**Commit checkpoint:**
```bash
git add README.md
git commit -m "docs(readme): separate implemented features from planned features"
```

### Task 3: Align `docs/jazz-language-state.md` with the split

**Files:**
- Modify: `docs/jazz-language-state.md`
- Reference: `docs/feature-status.md`

**Steps:**
- [ ] Add a short "Top-level docs contract" section that defines:
  - README = high-level summary
  - `docs/feature-status.md` = canonical status matrix
- [ ] Update the "Recommended Next Spec Cleanup" list so item #5 is marked done once split is merged.
- [ ] Add links between the two docs to keep navigation obvious.

**Commit checkpoint:**
```bash
git add docs/jazz-language-state.md
git commit -m "docs: align language-state doc with implemented-vs-planned split"
```

### Task 4: Nix-aware docs checks and formatter in root dev shell

**Files (root shell + optional):**
- Modify if present: `flake.nix`
- Modify if present: `shell.nix`
- Create/modify if adopted: `scripts/check-docs.sh`
- Modify if present: `.pre-commit-config.yaml`

**Steps:**
- [ ] Use the root `flake.nix` dev shell as the default environment for docs checks (`nix develop`).
- [ ] Add docs formatter/lint tooling to dev shell packages.
- [ ] Add a single docs check command hook (for example, `scripts/check-docs.sh`) that validates status-section structure and markdown formatting.
- [ ] Ensure command is runnable via `nix develop -c <command>`.
- [ ] Provide an equivalent `nix shell` fallback command only when a non-flake environment must be supported.

**Commit checkpoint (only if files changed):**
```bash
git add flake.nix shell.nix scripts/check-docs.sh .pre-commit-config.yaml
git commit -m "chore(nix): add docs check/format hooks for status-doc maintenance"
```

### Task 5: Prevent future drift between docs and implementation

**Files:**
- Modify: `docs/feature-status.md`
- Modify: `README.md`
- Optional if introduced: `CONTRIBUTING.md`

**Steps:**
- [ ] Add a "Maintenance Checklist" section with explicit update triggers:
  - parser/typechecker/codegen feature changes
  - builtin/runtime changes
  - new examples added to README
- [ ] Require each feature-status change to include evidence path(s) and commit SHA.
- [ ] Add a short reviewer checklist item: "Does README status match `docs/feature-status.md`?"

**Commit checkpoint:**
```bash
git add README.md docs/feature-status.md CONTRIBUTING.md
git commit -m "docs: add maintenance checklist to prevent feature-status drift"
```

### Task 6: Verification before completion

**Commands:**
- [ ] Structural split present:
```bash
rg -n "^### Implemented Today|^### Planned / Aspirational" README.md
```
- [ ] Canonical matrix exists and links are present:
```bash
rg -n "Implemented Today|Planned / Aspirational|Last verified against commit" docs/feature-status.md
rg -n "feature-status.md|top-level docs contract" docs/jazz-language-state.md
```
- [ ] Docs-only diff sanity check:
```bash
git diff --name-only -- README.md docs/
```
- [ ] Root flake verification path:
```bash
nix develop -c scripts/check-docs.sh
```
- [ ] Explicit fallback path (only if root flake is unavailable):
```bash
bash scripts/check-docs.sh
```

**Expected result:**
- [ ] README clearly separates implemented vs planned features.
- [ ] Canonical feature status source exists and is linked.
- [ ] Language-state doc no longer lists item #5 as pending.
- [ ] Verification commands succeed (or any deliberate skips are documented with reason).

**Final commit checkpoint (if verification edits were needed):**
```bash
git add README.md docs/jazz-language-state.md docs/feature-status.md scripts/check-docs.sh
git commit -m "docs: finalize spec-cleanup item #5 with verification pass"
```
