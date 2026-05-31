---
id: JN-MODULE-NO-PRELUDE-GRAPH-HARNESS-001
status: done
priority: P1
size: S
kind: impl
autonomous_ready: yes
depends_on: []
last_verified: 2026-05-31
plan_section: "Batch 1: Module graph no-prelude/prelude ownership harness"
target_paths:
  - jazz-next/test/JazzNext/Compiler/Modules/LoaderSpec.hs
  - jazz-next/test/JazzNext/Compiler/Modules/PreludeLoadingSpec.hs
  - jazz-next/test/JazzNext/Compiler/Semantics/BuiltinCatalogSpec.hs
verification:
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Modules/LoaderSpec.hs
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Modules/PreludeLoadingSpec.hs
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/BuiltinCatalogSpec.hs
  - bash scripts/check-execution-queue.sh
  - bash scripts/check-docs.sh
deliverable: "Add focused active jazz-next module-graph coverage proving explicit no-prelude paths reject public prelude aliases, keep __kernel_* bridge names available, and preserve bundled/explicit prelude helper visibility across imports without expanding the stdlib surface."
---

# Jazz Next Module Graph No-Prelude Harness

## Source Verification

This child plan narrows the broad runtime/module Milestone 5 note in
`docs/plans/2026-03-18-jazz-next-runtime-architecture-and-interpreter-execution-plan.md`
to one executor-safe harness batch. The exact source section is "Milestone 5:
Close module/import and stdlib execution semantics", specifically the unchecked
test-extension item after the bundled-prelude module graph and reproducibility
batches landed.

The current stdlib boundary spec already locks the behavior: bundled and
explicit-prelude paths expose public aliases, while explicit no-prelude paths
are kernel-only. This batch adds focused module-graph coverage for that
boundary; it does not add new builtin APIs, new prelude entries, or package
metadata behavior.

## Batch 1: Module graph no-prelude/prelude ownership harness

Completed on `2026-05-31`:

- Added module-graph tests for `compileModuleGraphWithPrelude Nothing` and
  `runModuleGraphWithPrelude Nothing` proving public aliases such as `map` and
  `hd` are rejected across imported modules without a prelude.
- Added paired module-graph tests proving `__kernel_*` bridge names remain
  available in the same explicit no-prelude path.
- Preserved bundled and explicit prelude coverage that exposes public helpers
  across module imports.
- Kept changes focused to `LoaderSpec.hs`; the new harness passed against the
  existing driver/catalog behavior, so no compiler source change was required.

Verification:

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Modules/LoaderSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Modules/PreludeLoadingSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/BuiltinCatalogSpec.hs
bash scripts/check-execution-queue.sh
bash scripts/check-docs.sh
```
