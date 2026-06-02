---
id: JN-PRIMITIVE-STRUCTURAL-ADT-EQUALITY-001
status: done
priority: P2
size: M
kind: impl
autonomous_ready: no
depends_on: []
last_verified: 2026-06-01
completed_on: 2026-06-01
plan_section: "Batch 1: Structural ADT equality for declared constructors"
target_paths:
  - jazz-next/src/JazzNext/Compiler/TypeInference.hs
  - jazz-next/src/JazzNext/Compiler/Runtime.hs
  - jazz-next/test/JazzNext/Compiler/Semantics/PrimitiveSemanticsSpec.hs
  - jazz-next/test/JazzNext/Compiler/Semantics/RuntimeSemanticsSpec.hs
  - jazz-next/test/JazzNext/Compiler/Semantics/AdtPatternRuntimeSpec.hs
verification:
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/PrimitiveSemanticsSpec.hs
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/RuntimeSemanticsSpec.hs
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/AdtPatternRuntimeSpec.hs
  - bash scripts/check-execution-queue.sh
  - bash scripts/check-docs.sh
deliverable: "Accept `==` and `!=` for same declared ADT type values when every constructor payload type is equality-supported, comparing constructor tags and payloads while preserving rejection for function payloads, partial constructors, and typeclass dispatch."
---

# Jazz Next Structural ADT Equality

## Source Verification

This child plan narrows
`docs/plans/spec-clarification/2026-03-03/runtime/16-primitive-semantics-contract.md`
to one executor-safe primitive batch. The exact source section is the completed
structural tuple/list equality batch, whose explicit out-of-scope list names
structural ADT equality as the remaining adjacent equality delta. This child
plan extends the same strict, type-directed equality policy to declared ADT
constructor values without adding typeclass dispatch.

This batch does not define function equality, builtin/operator/section equality,
implicit numeric conversion, mixed-width equality, dictionaries, or runtime
dispatch.

## Batch 1: Structural ADT equality for declared constructors

Completed on `2026-06-01` as
`JN-PRIMITIVE-STRUCTURAL-ADT-EQUALITY-001`.

Executor-safe scope:

- Accept `==` and `!=` for operands that resolve to the same declared ADT type
  when every constructor payload type recursively supports equality.
- Compare constructor tags first, then compare payloads structurally using the
  existing equality-supported primitive, list, and tuple cases.
- Return `False` for different constructors of the same ADT.
- Preserve strict type-directed rejection for different ADT types, function
  payloads, partial constructors, unresolved values, and unsupported equality
  families.
- Evaluate accepted ADT equality through an explicit runtime structural helper
  rather than broad `RuntimeValue` equality.

Target paths:

- `jazz-next/src/JazzNext/Compiler/TypeInference.hs`
- `jazz-next/src/JazzNext/Compiler/Runtime.hs`
- `jazz-next/test/JazzNext/Compiler/Semantics/PrimitiveSemanticsSpec.hs`
- `jazz-next/test/JazzNext/Compiler/Semantics/RuntimeSemanticsSpec.hs`
- `jazz-next/test/JazzNext/Compiler/Semantics/AdtPatternRuntimeSpec.hs`

Focused verification:

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/PrimitiveSemanticsSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/RuntimeSemanticsSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/AdtPatternRuntimeSpec.hs
bash scripts/check-execution-queue.sh
bash scripts/check-docs.sh
```
