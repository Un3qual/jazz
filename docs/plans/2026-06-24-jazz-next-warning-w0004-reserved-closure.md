---
id: JN-WARNING-W0004-RESERVED-CLOSURE-001
status: done
priority: P2
size: S
kind: coordination
autonomous_ready: yes
depends_on:
  - JN-WARNING-DEPRECATED-SYNTAX-CONTRACT-001
last_verified: 2026-06-24
completed_on: 2026-06-24
plan_section: "Completed coordination batch: W0004 reserved-only closure"
target_paths:
  - docs/spec/tooling/compiler-warning-flags.md
  - docs/plans/spec-clarification/2026-03-03/tooling/18-compiler-warning-flags.md
  - docs/execution/blocker-contracts.md
  - docs/execution/queue.md
verification:
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Config/WarningConfigSpec.hs
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Parser/ParserFoundationSpec.hs
  - bash scripts/check-execution-queue.sh
  - bash scripts/check-docs.sh
  - git diff --check
deliverable: "Close the current W0004 decision as reserved-only: no active accepted deprecated syntax surface exists, `trait` stays parser-rejected, `DeprecatedSyntax` keeps no analyzer emitter, and any future W0004 emitter requires a new accepted-surface contract."
---

# Jazz-Next W0004 Reserved-Only Closure Plan

> **Archival note:** This completed closure record is historical context, not a live execution plan. Do not execute the checklist as a current worker directive.

**Goal:** Close the current `deprecated-syntax` / `W0004` warning decision
without adding a warning emitter.

**Architecture:** Treat `W0004` as stable reserved metadata only. Record the
current evidence that no active accepted syntax surface is deprecated, keep
`trait` parser-rejected rather than warning-emitting, and route future W0004
work through a new contract only after a real accepted syntax surface is
intentionally deprecated.

**Tech Stack:** Markdown specs and queue docs, existing `jazz-next`
WarningConfig and parser suites, and repo-root queue/docs validators.

---

## Completed coordination batch: W0004 reserved-only closure

Completed on `2026-06-24`.

Executor-safe scope:

- Do not add analyzer emission.
- Do not accept `trait`.
- Do not treat parser errors as deprecation warnings.
- Keep `deprecated-syntax` / `W0004` parseable through warning config metadata.
- Close the current blocked W0004 contract as reserved-only until a future
  accepted active-path syntax surface is explicitly deprecated.

Evidence:

- `jazz-next/src/JazzNext/Compiler/WarningCatalog.hs` keeps
  `DeprecatedSyntax` mapped to `W0004` / `deprecated-syntax` with
  `metadataHasAnalyzerEmitter = False`.
- `jazz-next/test/JazzNext/Compiler/Config/WarningConfigSpec.hs` locks the
  stable W0004 code/token and verifies config parsing does not imply an
  analyzer emitter.
- `jazz-next/test/JazzNext/Compiler/Parser/ParserFoundationSpec.hs` verifies
  root and module-body `trait` declarations reject with
  `unsupported abstraction syntax 'trait'`.
- Active `class` / `impl` syntax is implemented behavior, not deprecated
  compatibility syntax.

### Task 1: Verify W0004 metadata and parser evidence

**Files:**

- Inspect: `jazz-next/src/JazzNext/Compiler/WarningCatalog.hs`
- Inspect: `jazz-next/test/JazzNext/Compiler/Config/WarningConfigSpec.hs`
- Inspect: `jazz-next/test/JazzNext/Compiler/Parser/ParserFoundationSpec.hs`

- [x] **Step 1: Confirm W0004 is reserved metadata**

Verify `DeprecatedSyntax` still maps to code `W0004`, token
`deprecated-syntax`, and `metadataHasAnalyzerEmitter = False`.

- [x] **Step 2: Confirm config parsing stays metadata-only**

Verify `WarningConfigSpec` asserts `DeprecatedSyntax` can be parsed and enabled
in warning settings while `warningHasAnalyzerEmitter DeprecatedSyntax` remains
`False`.

- [x] **Step 3: Confirm `trait` stays parser-rejected**

Verify parser tests reject `trait Eq { }.` and module-body `trait`
declarations with `unsupported abstraction syntax 'trait'`.

### Task 2: Close W0004 docs and dispatcher state

**Files:**

- Modify: `docs/spec/tooling/compiler-warning-flags.md`
- Modify: `docs/plans/spec-clarification/2026-03-03/tooling/18-compiler-warning-flags.md`
- Modify: `docs/execution/blocker-contracts.md`
- Modify: `docs/execution/queue.md`

- [x] **Step 1: Update warning spec**

Record that the current W0004 decision is closed as reserved-only and future
W0004 work requires a new accepted active syntax surface before any emitter can
be specified.

- [x] **Step 2: Update warning plan**

Mark the current W0004 contract decision complete, add closure evidence, and
keep future emitter work blocked on a new accepted deprecated syntax surface.

- [x] **Step 3: Update queue and blocker contracts**

Move the current W0004 blocker rows out of `Blocked`, record the closure in
`Done`, and seed the next curation candidate from the remaining blocker
contracts.

### Task 3: Run closure verification

**Files:**

- Modify: none

- [x] **Step 1: Run warning metadata coverage**

Run:

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Config/WarningConfigSpec.hs
```

Expected: all `WarningConfigSpec` tests pass.

- [x] **Step 2: Run parser keyword coverage**

Run:

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Parser/ParserFoundationSpec.hs
```

Expected: all `ParserFoundationSpec` tests pass.

- [x] **Step 3: Run queue/docs/diff checks**

Run:

```bash
bash scripts/check-execution-queue.sh
bash scripts/check-docs.sh
git diff --check
```

Expected: all commands pass; `check-docs.sh` may print the existing
Prettier-outside-Nix warning and still pass.
