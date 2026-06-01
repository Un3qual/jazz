---
id: JN-FLOAT16-FLOAT32-SAME-WIDTH-ARITHMETIC-001
status: done
priority: P2
size: M
kind: impl
autonomous_ready: no
depends_on: []
last_verified: 2026-06-01
completed_on: 2026-06-01
plan_section: "Batch 1: Float32 same-width arithmetic and Float16 runtime-width gate"
target_paths:
  - jazz-next/src/JazzNext/Compiler/TypeInference.hs
  - jazz-next/src/JazzNext/Compiler/Runtime.hs
  - jazz-next/test/JazzNext/Compiler/Semantics/PrimitiveSemanticsSpec.hs
  - jazz-next/test/JazzNext/Compiler/Semantics/RuntimeSemanticsSpec.hs
verification:
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/PrimitiveSemanticsSpec.hs
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/RuntimeSemanticsSpec.hs
  - bash scripts/check-execution-queue.sh
  - bash scripts/check-docs.sh
deliverable: "Accept `+`, `-`, `*`, and `/` for same concrete `Float32` operands via explicit-conversion-produced values while rejecting `Float16` arithmetic until runtime float values carry or apply the target width; preserve all mixed-width and implicit-promotion rejections."
---

# Jazz Next Float32 Same-Width Arithmetic And Float16 Gate

## Source Verification

This child plan narrows
`docs/plans/spec-clarification/2026-03-03/runtime/16-primitive-semantics-contract.md`
to one executor-safe primitive batch. The exact source sections are the numeric
width/defaulting rollout, which locks same-concrete numeric operators and
explicit-conversion-only width mixing, plus the completed Float64 arithmetic
batch, which already proved the active target files and verification shape.

This batch does not define literal suffix syntax, `Float16` or `Float32`
literal targeting, implicit integer-to-float promotion, implicit mixed-width
arithmetic, dictionaries, or runtime dispatch.

## Batch 1: Float32 same-width arithmetic and Float16 runtime-width gate

Completed on `2026-06-01`.

Executor-safe scope:

- Accept `+`, `-`, `*`, and `/` when both operands resolve to the same concrete
  `Float32` type.
- Reject `+`, `-`, `*`, and `/` when operands resolve to `Float16` until the
  runtime carries or reapplies the target width for arithmetic results.
- Return the same concrete floating type for accepted `Float32` operations.
- Evaluate accepted operations through the active runtime floating-value path.
- Preserve existing integer arithmetic and same-concrete `Float`/`Float64`
  arithmetic behavior.
- Preserve compile-time rejection for mixed `Int`/floating operands,
  `Float16`/`Float32`, `Float16`/`Float64`, `Float32`/`Float64`, unrelated
  non-numeric operands, and implicit-promotion cases.

Target paths:

- `jazz-next/src/JazzNext/Compiler/TypeInference.hs`
- `jazz-next/src/JazzNext/Compiler/Runtime.hs`
- `jazz-next/test/JazzNext/Compiler/Semantics/PrimitiveSemanticsSpec.hs`
- `jazz-next/test/JazzNext/Compiler/Semantics/RuntimeSemanticsSpec.hs`

Focused verification:

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/PrimitiveSemanticsSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/RuntimeSemanticsSpec.hs
bash scripts/check-execution-queue.sh
bash scripts/check-docs.sh
```
