---
id: JN-FLOAT16-FLOAT32-SAME-WIDTH-COMPARISON-EQUALITY-001
status: ready
priority: P2
size: M
kind: impl
autonomous_ready: yes
depends_on: []
last_verified: 2026-06-01
plan_section: "Batch 1: Float16/Float32 same-width comparison and equality"
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
deliverable: "Accept `==`, `!=`, `<`, `<=`, `>`, and `>=` for same concrete `Float16` and `Float32` operands via explicit-conversion-produced values, returning `Bool` while preserving all mixed-width and implicit-promotion rejections."
---

# Jazz Next Float16/Float32 Same-Width Comparison And Equality

## Source Verification

This child plan narrows
`docs/plans/spec-clarification/2026-03-03/runtime/16-primitive-semantics-contract.md`
to one executor-safe primitive batch. The exact source sections are the strict
type-directed equality decision, the numeric width/defaulting rollout that locks
same-concrete operations with explicit-conversion-only width mixing, and the
completed Float64 comparison/equality batch, which already proved the active
target files and verification shape.

This batch does not define literal suffix syntax, `Float16` or `Float32`
literal targeting, implicit integer-to-float promotion, implicit mixed-width
comparison, structural ADT equality, dictionaries, or runtime dispatch.

## Batch 1: Float16/Float32 same-width comparison and equality

Executor-safe scope:

- Accept `==`, `!=`, `<`, `<=`, `>`, and `>=` when both operands resolve to the
  same concrete `Float16` type.
- Accept `==`, `!=`, `<`, `<=`, `>`, and `>=` when both operands resolve to the
  same concrete `Float32` type.
- Return `Bool` for accepted comparison and equality operations.
- Evaluate accepted operations through the active runtime floating-value path.
- Preserve existing integer comparison/equality, same-concrete `Float`/`Float64`
  comparison/equality, and strict non-coercive equality behavior.
- Preserve compile-time rejection for mixed `Int`/floating operands,
  `Float16`/`Float32`, `Float16`/`Float64`, `Float32`/`Float64`, unrelated
  non-comparable operands, and implicit-promotion cases.

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
