---
id: JN-BOOTSTRAP-MAYBE-RESULT-LIBRARIES-001
status: done
completed_on: 2026-07-11
priority: P1
size: S
kind: impl
autonomous_ready: yes
depends_on:
  - JN-BOOTSTRAP-GENERIC-NAMED-TYPES-001
last_verified: 2026-07-11
plan_section: "Implementation Batch: Bootstrap Maybe and Result Libraries"
target_paths:
  - jazz-next/stdlib/Maybe.jz
  - jazz-next/stdlib/Result.jz
  - jazz-next/test/JazzNext/Compiler/Modules/LoaderSpec.hs
  - jazz-next/test/JazzNext/Compiler/Modules/PreludeLoadingSpec.hs
  - docs/jazz-language-state.md
  - docs/execution/blocker-contracts.md
  - docs/execution/queue.md
  - docs/execution/done-archive.md
verification:
  - cabal test loader-spec prelude-loading-spec --test-show-details=failures
  - bash jazz-next/scripts/test-warning-config.sh
  - bash scripts/check-execution-queue.sh
  - bash scripts/check-docs.sh
  - git diff --check
deliverable: "Add ordinary importable Jazz-authored Maybe and Result modules with explicit type and constructor exports, prove their generic types and constructors through the real module loader, and keep both modules outside the bundled prelude and compiler builtin surface."
---

# Jazz-Next Bootstrap Maybe and Result Libraries Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add the first Jazz-authored bootstrap library types as ordinary importable modules without coupling them to the Haskell interpreter or a future LLVM backend.

**Architecture:** Store `Maybe` and `Result` as canonical `.jz` modules in `jazz-next/stdlib/`. Each module declares its generic ADT and an explicit public inventory containing only its type and constructors. Loader tests read the checked-in module sources through the existing `ModuleResolutionConfig`/source-lookup boundary, while prelude tests prove that the same names remain unavailable without explicit module imports. No compiler builtin, catalog entry, special IR node, or runtime primitive is added.

**Tech Stack:** Jazz `.jz` modules, Haskell 2010 test harnesses, the existing `jazz-next` module resolver/compiler/interpreter, Cabal component tests, and repository queue/docs gates.

## Global Constraints

- Modify only `jazz-next/` and active documentation; `jazz-hs/` and `jazz2/` remain read-only.
- Define `Maybe` as `data Maybe a = Nothing | Just a.` and export exactly `type Maybe`, `constructor Nothing`, and `constructor Just`.
- Define `Result` as `data Result e a = Err e | Ok a.` and export exactly `type Result`, `constructor Err`, and `constructor Ok`.
- Keep both files as ordinary modules that require explicit imports; do not add them to `BundledPrelude`, the builtin catalog, or kernel bridges.
- Exercise generic signatures, constructor construction, constructor patterns, nominal module transport, and runtime output by loading the checked-in files through the module graph.
- Prefer behavioral assertions over source-text snapshots; source lookup may locate the checked-in files but tests must validate compiler/runtime behavior.
- Do not add text traversal, host I/O, stack-safe evaluation, lexer/parser modules, backend-neutral lowered IR, LLVM lowering, linking, or a native runtime.
- Implement behavior test-first and commit independently reviewable milestones.

---

## Implementation Batch: Bootstrap Maybe and Result Libraries

### Task 1: Lock the ordinary-module and prelude boundaries with failing tests

**Files:**

- Modify: `jazz-next/test/JazzNext/Compiler/Modules/LoaderSpec.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Modules/PreludeLoadingSpec.hs`

- [x] Add a loader test whose source lookup serves an in-memory entry module and the checked-in `stdlib/Maybe.jz` and `stdlib/Result.jz` files.
- [x] In the entry module, import both modules explicitly, use `Maybe(Int)` and `Result(Text, Int)` signatures, construct values with `Just`/`Ok`, pattern-match all constructors, and assert successful runtime output.
- [x] Add a prelude-loading test proving that `Maybe`, `Result`, `Nothing`, `Just`, `Err`, and `Ok` are not implicitly visible to standalone source compilation with the bundled prelude.
- [x] Run `cabal test loader-spec prelude-loading-spec --test-show-details=failures` and confirm the new checks fail because the two checked-in modules do not exist yet.

### Task 2: Add the Jazz-authored modules

**Files:**

- Create: `jazz-next/stdlib/Maybe.jz`
- Create: `jazz-next/stdlib/Result.jz`

- [x] Add the canonical `Maybe` module and exact explicit export inventory.
- [x] Add the canonical `Result` module and exact explicit export inventory.
- [x] Run `cabal test loader-spec prelude-loading-spec --test-show-details=failures` and confirm the module graph executes while bundled-prelude-only compilation still rejects all six library names.

### Task 3: Close documentation and live dispatch state

**Files:**

- Modify: `docs/jazz-language-state.md`
- Modify: `docs/execution/blocker-contracts.md`
- Modify: `docs/execution/queue.md`
- Modify: `docs/execution/done-archive.md`
- Modify: `docs/superpowers/plans/2026-07-11-jazz-next-bootstrap-maybe-result-libraries.md`

- [x] Record the implemented ordinary-module boundary and exact public APIs in the language state.
- [x] Move the completed queue item to the done archive, update the bootstrap blocker, and state that the next child requires a separately accepted source-backed traversal contract.
- [x] Mark this plan `done` with its completion date.

### Task 4: Verify the completed slice

- [x] Run `cabal test loader-spec prelude-loading-spec --test-show-details=failures` from `jazz-next/`.
- [x] Run `bash jazz-next/scripts/test-warning-config.sh` from the repository root.
- [x] Run `bash scripts/check-execution-queue.sh` and `bash scripts/check-docs.sh`.
- [x] Run `git diff --check` and inspect the final diff for accidental builtin, prelude, legacy, or backend changes.
