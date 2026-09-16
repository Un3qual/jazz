---
id: JN-REMOVE-HOSTED-COMPILER-001
status: in_progress
priority: P1
size: L
kind: impl
autonomous_ready: yes
depends_on: []
last_verified: 2026-09-15
plan_section: Implementation
target_paths:
  - jazz/compiler
  - src/Jazz/Compiler/Parser/Lower.hs
  - test/Jazz/Compiler/Bootstrap
  - jazz.cabal
  - scripts
  - docs
  - rfcs
verification:
  - cabal test all --jobs=1 --test-show-details=failures
  - JAZZ_CABAL_JOBS=1 bash scripts/ci/haskell-quality.sh
  - bash scripts/check-docs.sh
  - bash scripts/check-execution-queue.sh
  - git diff --check
deliverable: Remove the hosted compiler and its exclusive support while preserving ordinary Haskell compilation, runtime, and Jazz libraries.
---

# Remove the hosted compiler implementation plan

> **For agentic workers:** Execute inline using `superpowers:executing-plans`.
> The maintainer approved this removal and separate branch before dispatch.

**Goal:** Focus development and verification on the Haskell compiler.

**Architecture:** Delete `jazz/compiler` and consumers exclusive to its comparison
harness. Preserve the production parser, canonical core, module pipeline,
interpreter, standard library, and normal behavioral coverage. No replacement
frontend or new compiler architecture.

**Tech Stack:** Haskell, Jazz standard library, Cabal, pinned Nix quality shell.

**Spec:** Maintainer-approved removal recorded in RFC 0022 by this change.

## Global constraints

- Base: main `d3479d770cc9d89b38cc1023801949352f4ea531`, verified before editing.
- Branch: `codex/remove-hosted-compiler`; no RFC 0021 implementation commits.
- Removal PR merges before the separate RFC 0021 PR; do not merge either here.
- Do not run any `jazz-parser-scale-full-*-spec` or baseline hosted suites.
- Serialize Cabal with `--jobs=1`; use `/nix/var/nix/profiles/default/bin/nix`
  with `--extra-experimental-features 'nix-command flakes'` and `.#quality`.
- Historical RFCs/plans retain history with explicit supersession notices.

## Implementation

- [x] Verify clean worktree/base, create branch, read dispatcher, trace owners.
- [x] Remove 16 `jazz/compiler/*.jz` modules and dedicated Bootstrap tests,
      comparison schemas/encoders, exclusive fixtures, registrations, scale flag,
      Weeder exceptions, and CI policy/wiring. Check actual consumers first.
- [x] Collapse hosted-only lowering failure adapters into ordinary diagnostics;
      retain production lexer/parser errors and phase APIs used by normal tests or
      benchmarks. Inspect deleted tests for unique production regressions and move
      only meaningful missing coverage to an existing owner.
- [x] Run focused parser/module/loader/repository tests and commit removal.
- [x] Add accepted RFC 0022; amend RFCs 0003/0004/0008/0016 and relevant later
      obligations. Reconcile RFC 0021 proposal/plan and dispatcher without importing
      its implementation. Update public compiler/contribution/testing docs and
      AGENTS; self-hosting requires a future explicit execution goal and fresh design.
- [ ] Run surviving default tests, examples, quality/format, docs/queue and CI
      policy checks; run isolated Nix checks. Review production diff and references.
- [ ] Commit verified closeout and prepare independent removal PR description.

## Consumer audit

- `app/Main.hs` uses `Jazz.CLI.Main` / `Jazz.Compiler.Driver`; ordinary programs
  never load the hosted modules.
- `test/Jazz/Compiler/Bootstrap` owns all canonical comparison encoders and
  hosted execution harnesses. Corpus fixtures require per-consumer inspection.
- Keep library/primitive tests even when their historical names say bootstrap.
- Keep normal Haskell parser-stage performance/profiling and program corpus
  workloads when no hosted module is loaded.

## Verification evidence

Eight focused suites and 121 CI-policy tests passed; removal committed as
`e8dd50ea`. No baseline hosted/full-scale suite was run. Full gates pending.
