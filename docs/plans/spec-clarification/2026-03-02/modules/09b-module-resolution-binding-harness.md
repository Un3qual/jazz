---
id: JN-MODULE-RESOLUTION-BINDING-HARNESS-001
status: ready
priority: P1
size: M
kind: impl
autonomous_ready: yes
depends_on:
  - JN-MODULE-FILE-LAYOUT-HARNESS-001
last_verified: 2026-05-30
plan_section: "Batch 1: Resolution order and import-binding harness"
target_paths:
  - jazz-next/test/JazzNext/Compiler/Modules/ModuleResolutionSpec.hs
verification:
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Modules/ModuleResolutionSpec.hs
  - bash scripts/check-execution-queue.sh
  - bash scripts/check-docs.sh
deliverable: "Add focused active jazz-next coverage for lexical import traversal, duplicate import collapse, already-resolved module reuse, and E4007-E4009/E4011-E4014 import-binding diagnostics from the qualified import contract."
---

# Module Resolution And Binding Harness Batch

Primary queue row: `JN-MODULE-RESOLUTION-BINDING-HARNESS-001`

Parent plan: `docs/plans/spec-clarification/2026-03-02/modules/09-module-loader-and-import-resolution.md`

Verified source sections:

- `docs/spec/modules/02-resolution-algorithm-and-cycles.md`
- `docs/spec/modules/04-qualified-imports-and-binding.md`

## Batch 1: Resolution order and import-binding harness

Executor scope:

- Add or tighten resolver coverage in `ModuleResolutionSpec.hs` for lexical rendered-path traversal, duplicate import collapse, already-resolved module reuse, and minimal cycle traces.
- Add or tighten resolver coverage for the qualified import truth table, including `E4007`, `E4008`, `E4009`, `E4011`, `E4012`, `E4013`, and `E4014` with importer/imported-module context and span metadata where the active diagnostics already expose it.
- Keep this as a harness batch. Do not add new import forms, export lists, wildcard imports, re-exports, package metadata, or compatibility modes.
- Do not inspect or modify `jazz-hs/` or `jazz2/`.

Verification:

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Modules/ModuleResolutionSpec.hs
bash scripts/check-execution-queue.sh
bash scripts/check-docs.sh
```
