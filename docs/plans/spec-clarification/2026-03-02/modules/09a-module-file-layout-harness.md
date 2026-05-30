---
id: JN-MODULE-FILE-LAYOUT-HARNESS-001
status: ready
priority: P1
size: S
kind: impl
autonomous_ready: yes
depends_on: []
last_verified: 2026-05-30
plan_section: "Batch 1: File layout parser and resolver harness"
target_paths:
  - jazz-next/test/JazzNext/Compiler/Parser/ModuleImportParserSpec.hs
  - jazz-next/test/JazzNext/Compiler/Modules/ModuleResolutionSpec.hs
verification:
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Parser/ModuleImportParserSpec.hs
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Modules/ModuleResolutionSpec.hs
  - bash scripts/check-execution-queue.sh
  - bash scripts/check-docs.sh
deliverable: "Add focused active jazz-next coverage for module path segment preservation, canonical .jz mapping, declaration omission/match/mismatch boundaries, duplicate-root dedupe, and rejected legacy declaration forms without changing runtime semantics."
---

# Module File Layout Harness Batch

Primary queue row: `JN-MODULE-FILE-LAYOUT-HARNESS-001`

Parent plan: `docs/plans/spec-clarification/2026-03-02/modules/09-module-loader-and-import-resolution.md`

Verified source sections:

- `docs/spec/modules/01-file-layout-and-package-roots.md`
- `docs/spec/modules/05-migration-and-compatibility.md`

## Batch 1: File layout parser and resolver harness

Executor scope:

- Add or tighten parser coverage in `ModuleImportParserSpec.hs` for the canonical brace-bodied declaration boundary and rejected legacy declaration shapes.
- Add or tighten resolver coverage in `ModuleResolutionSpec.hs` for exact module path segment preservation, canonical `.jz` path mapping, declaration omission, declaration/path match, declaration/path mismatch, duplicate root dedupe, and deterministic candidate order.
- Do not change parser, resolver, loader, or runtime behavior unless a focused RED test proves the active implementation violates the published v1 module layout contract.
- Do not inspect or modify `jazz-hs/` or `jazz2/`.

Verification:

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Parser/ModuleImportParserSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Modules/ModuleResolutionSpec.hs
bash scripts/check-execution-queue.sh
bash scripts/check-docs.sh
```
