---
id: JN-REMOVE-HOSTED-COMPILER-001
status: complete
priority: P1
size: L
kind: impl
autonomous_ready: no
depends_on: []
last_verified: 2026-09-15
plan_section: Implementation
target_paths:
  - jazz/compiler
  - src/Jazz/Compiler/Parser
  - test/Jazz
  - jazz.cabal
  - scripts
  - docs
  - rfcs
  - release-notes/0.1.0-alpha.1.md
  - AGENTS.md
  - CONTRIBUTING.md
  - PERFORMANCE.md
  - README.md
  - weeder-production.toml
  - weeder.toml
  - .codex/execution
  - .codex/plans/2026-09-15-module-reexports-and-operator-transport.md
  - .codex/plans/2026-09-15-remove-hosted-compiler.md
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

**Spec:** [RFC 0022](../../rfcs/accepted/0022-hosted-compiler-removal.md), recording the maintainer-approved removal.

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
- [x] Run surviving default tests, examples, quality/format, docs/queue and CI
      policy checks; run isolated Nix checks. Review production diff and references.
- [x] Commit implementation and RFC amendments; prepare independent removal PR description.

## Consumer audit

- `app/Main.hs` uses `Jazz.CLI.Main` / `Jazz.Compiler.Driver`; ordinary programs
  never load the hosted modules.
- The removed `test/Jazz/Compiler/Bootstrap` tree owned all comparison encoders
  and hosted execution harnesses. Its 365-fixture parser snapshot was copied
  from the ordinary parser suites and had no surviving consumer.
- Kept one direct module-lowering regression for invalid surface trees under
  `CoreNormalizationSpec`; preserved the original parser and runtime tests.
- Keep library/primitive tests even when their historical names say bootstrap.
- Keep normal Haskell parser-stage performance/profiling and program corpus
  workloads when no hosted module is loaded.

## Verification evidence

- Base verified as main `d3479d770cc9d89b38cc1023801949352f4ea531`.
- Removal committed in `e8dd50ea`; RFC/public/execution updates in `de736b18`
  and `321e05e5`. No RFC 0021 implementation was inherited.
- All eight focused parser/core/module/loader/repository suites passed.
- Fresh Haskell quality gate passed: HLint, production-only Weeder, all retained
  test/benchmark components, full Weeder, and generated invariants.
- The complete serialized `scripts/ci/main-functional.sh` gate passed:
  all 47 default suites, Cabal package checks, executable examples, repository
  policy regressions, docs/RFC/queue checks, and isolated Nix flake checks.
- Changed Haskell formatting and final `git diff --check` passed.
- Independent read-only review found two missing historical RFC amendments;
  both were fixed and the review closed with no remaining findings.
- The 14 Jazz standard-library modules and normal benchmark/program-corpus
  sources are unchanged. Nineteen hosted/comparison/scale suites were removed.
- No removed hosted suite or full parser-scale baseline was run.

Merge the independent removal PR first. Only after that merge should the
separate RFC 0021 task rebase and reconcile deleted hosted paths. This task
did not modify that branch, merge either PR, or add replacement architecture.
