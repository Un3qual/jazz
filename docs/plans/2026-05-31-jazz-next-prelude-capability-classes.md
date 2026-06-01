---
id: JN-CAPABILITY-PRELUDE-CLASSES-001
status: done
priority: P1
size: S
kind: impl
autonomous_ready: yes
depends_on: []
last_verified: 2026-05-31
plan_section: "Batch 1: Bundled prelude canonical capability classes"
target_paths:
  - jazz-next/src/JazzNext/Compiler/BundledPrelude.hs
  - jazz-next/stdlib/Prelude.jz
  - jazz-next/test/JazzNext/Compiler/Semantics/BuiltinCatalogSpec.hs
  - jazz-next/test/JazzNext/Compiler/Modules/PreludeLoadingSpec.hs
verification:
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/BuiltinCatalogSpec.hs
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Modules/PreludeLoadingSpec.hs
  - bash scripts/check-execution-queue.sh
  - bash scripts/check-docs.sh
deliverable: "Generate and check in canonical capability `class` declarations in the bundled prelude before kernel bridges, proving default prelude class facts are visible while leaving impl facts and dispatch out of scope."
---

# Jazz Next Bundled Prelude Capability Classes

## Batch 1: Bundled prelude canonical capability classes

Completed on `2026-05-31`.

This executor-safe batch consumed the already landed active `jazz-next`
class/impl environment-validation slice. It made the bundled prelude the owner
of the canonical capability class facts named in the abstraction vocabulary
contract, without adding impl facts, method lookup, dictionaries, runtime
evidence values, or defaulting behavior.

Implementation delivered:

- Generated canonical prelude class declarations before kernel bridge bindings in
  `JazzNext.Compiler.BundledPrelude`.
- Checked the generated source into `jazz-next/stdlib/Prelude.jz` so the
  reproducibility harness continues to compare against the repository mirror.
- Covered that default-prelude source makes canonical class facts visible to
  constrained-signature validation while still failing when an impl fact is
  absent.
- Kept explicit-prelude and no-prelude behavior deterministic; callers that
  supply their own prelude remain responsible for their own class facts.

Canonical class facts for this batch:

- `class Eq { }.`
- `class Ord { }.`
- `class Num { }.`
- `class Integral { }.`
- `class Fractional { }.`
- `class Showable { }.`
- `class Default { }.`

Out of scope:

- any `impl` declarations,
- method declarations or method bodies inside `class` or `impl`,
- method dispatch, dictionaries, runtime evidence, or superclass semantics,
- inferred constraints, polymorphic generalization, or defaulting behavior.

Focused verification:

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/BuiltinCatalogSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Modules/PreludeLoadingSpec.hs
bash scripts/check-execution-queue.sh
bash scripts/check-docs.sh
```
