# Jazz Next Test Layout Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Reorganize `jazz-next/test` into subsystem folders without changing compiler behavior.

**Architecture:** Keep the existing `runghc`-driven test model and `JazzNext.TestHarness` import root, but move spec entrypoints under `JazzNext/CLI` and `JazzNext/Compiler/*` folders that mirror the active compiler areas. Update the test runner and living docs to reference the new paths.

**Tech Stack:** Haskell test entrypoints run via `runghc`, bash runner script, Markdown docs under `jazz-next/` and `docs/`.

---

### Task 1: Move Specs Into Subsystem Folders

**Files:**
- Create: `jazz-next/test/JazzNext/CLI/`
- Create: `jazz-next/test/JazzNext/Compiler/Config/`
- Create: `jazz-next/test/JazzNext/Compiler/Diagnostics/`
- Create: `jazz-next/test/JazzNext/Compiler/Modules/`
- Create: `jazz-next/test/JazzNext/Compiler/Parser/`
- Create: `jazz-next/test/JazzNext/Compiler/Semantics/`
- Modify: `jazz-next/test/**/*.hs` spec entrypoint locations

**Step 1: Move parser, module, semantics, config, diagnostics, and CLI specs**

Place each spec under the approved subsystem directory while keeping filenames and file contents unchanged.

**Step 2: Inspect the tree**

Run:

```bash
find jazz-next/test -maxdepth 5 -type f | sort
```

Expected: specs live under `JazzNext/CLI` or `JazzNext/Compiler/*`, and `JazzNext/TestHarness.hs` remains in place.

### Task 2: Update Runner And Living Docs

**Files:**
- Modify: `jazz-next/scripts/test-warning-config.sh`
- Modify: `jazz-next/README.md`
- Modify: `docs/feature-status.md`
- Modify: `docs/jazz-language-state.md`
- Modify: `docs/spec/semantics/purity-bang-stub-v1.md`

**Step 1: Repoint the runner**

Update the `TEST_FILES` array to the new nested paths.

**Step 2: Repoint user-facing docs**

Document the grouped test layout in `jazz-next/README.md` and update current non-historical docs that reference moved specs.

**Step 3: Verify stale references are limited**

Run:

```bash
rg -n "jazz-next/test/[A-Za-z/]+Spec\\.hs" jazz-next docs --glob '!docs/plans/**'
```

Expected: only the new nested paths remain outside historical plan snapshots.

### Task 3: Verify The Reorganization

**Files:**
- Modify: none
- Test: `jazz-next/scripts/test-warning-config.sh`

**Step 1: Run the full `jazz-next` test script**

Run:

```bash
bash jazz-next/scripts/test-warning-config.sh
```

Expected: every moved spec still passes from its new location.

**Step 2: Inspect git status**

Run:

```bash
git status --short
```

Expected: only the planned moves and doc/script updates appear.
