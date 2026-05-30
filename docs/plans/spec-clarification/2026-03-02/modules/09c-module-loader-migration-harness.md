---
id: JN-MODULE-LOADER-MIGRATION-HARNESS-001
status: done
priority: P1
size: M
kind: impl
autonomous_ready: yes
depends_on: []
last_verified: 2026-05-30
completed_on: 2026-05-30
plan_section: "Batch 1: Loader pipeline and migration harness"
target_paths:
  - jazz-next/test/JazzNext/Compiler/Modules/LoaderSpec.hs
  - jazz-next/test/JazzNext/CLI/CLISpec.hs
  - jazz-next/scripts/test-warning-config.sh
verification:
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Modules/LoaderSpec.hs
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/CLI/CLISpec.hs
  - bash jazz-next/scripts/test-warning-config.sh
  - bash scripts/check-execution-queue.sh
  - bash scripts/check-docs.sh
deliverable: "Add focused active jazz-next coverage for CLI source-selection exclusivity, module-graph default roots, dependency expression validation versus runtime isolation, memoized source lookup, and fail-fast migration diagnostics."
---

# Module Loader And Migration Harness Batch

Primary queue row: `JN-MODULE-LOADER-MIGRATION-HARNESS-001`

Parent plan: `docs/plans/spec-clarification/2026-03-02/modules/09-module-loader-and-import-resolution.md`

Verified source sections:

- `docs/spec/modules/03-loader-behavior-and-diagnostics.md`
- `docs/spec/modules/05-migration-and-compatibility.md`

## Batch 1: Loader pipeline and migration harness

Executor scope:

- Add or tighten loader coverage in `LoaderSpec.hs` for module-graph default roots, dependency expression validation versus runtime isolation, memoized source lookup reuse, and deterministic module graph diagnostics.
- Add or tighten CLI coverage in `CLISpec.hs` for standalone source selection, source file plus `--entry-module`, `--module-root` without `--entry-module`, default `.` module root behavior, and fail-fast migration diagnostics.
- Keep `jazz-next/scripts/test-warning-config.sh` aligned only if the focused module/CLI harness additions require an active default-suite entry.
- Do not add compatibility-mode rewriting or deprecated-syntax warnings for rejected module/import forms.
- Do not inspect or modify `jazz-hs/` or `jazz2/`.

## Closure evidence

Completed on 2026-05-30 without loader, CLI, resolver, or runtime behavior
changes. The active implementation already satisfied the loader pipeline and
migration contracts once the missing harness assertions were added.

Added loader coverage in `LoaderSpec.hs` for run-mode validation of invalid
dependency expression statements before runtime, ambiguous import diagnostics,
and fail-fast module source parse diagnostics for rejected migration syntax.
Existing loader coverage continues to lock dependency expression runtime
isolation and memoized source lookup reuse.

Added CLI coverage in `CLISpec.hs` for `--module-root` without
`--entry-module`, source plus `--entry-module` rejection before source reads,
default `.` module-root behavior, and module-graph parse diagnostics. The
default verification runner already included both `LoaderSpec.hs` and
`CLISpec.hs`, so `jazz-next/scripts/test-warning-config.sh` required no change.

Verification:

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Modules/LoaderSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/CLI/CLISpec.hs
bash jazz-next/scripts/test-warning-config.sh
bash scripts/check-execution-queue.sh
bash scripts/check-docs.sh
```
