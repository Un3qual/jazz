---
id: JN-FLOAT16-FLOAT32-WIDTH-PRESERVING-ARITHMETIC-001
status: done
priority: P1
size: M
kind: impl
autonomous_ready: no
depends_on:
  - JN-PRIMITIVE-FLOAT16-FLOAT32-LITERAL-TARGETING-001
last_verified: 2026-06-02
completed_on: 2026-06-02
plan_section: "Batch 1: Width-preserving Float16/Float32 arithmetic"
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
deliverable: "Accept `+`, `-`, `*`, and `/` for same concrete `Float16` or `Float32` operands by preserving the target width in runtime arithmetic results using the existing finite-target rounding and overflow diagnostics, while preserving mixed-width rejection, no implicit integer-to-float promotion, no literal suffix syntax, and no solver/runtime-dispatch work."
---

# Jazz Next Float16/Float32 Width-Preserving Arithmetic

## Source Verification

This child plan narrows the primitive umbrella section
`Follow-up: Primitive deltas after literal-targeting landing` to one
executor-safe active-path batch. The relevant source sections are the numeric
width/defaulting rollout in
`docs/plans/spec-clarification/2026-03-03/runtime/16-primitive-semantics-contract.md`,
the completed `Float16`/`Float32` arithmetic-gate child plan, and the completed
literal-targeting child plan.

The primitive spec now records same concrete `Float16` and `Float32`
arithmetic as width-preserving active behavior once runtime float values carry
or reapply the target width for arithmetic results. Literal targeting and
explicit conversions provide the finite-target checks and rounding behavior
this batch reuses.

## Batch 1: Width-preserving Float16/Float32 arithmetic

Scope:

- Accept `+`, `-`, `*`, and `/` when both operands resolve to the same concrete
  `Float16` type.
- Accept `+`, `-`, `*`, and `/` when both operands resolve to the same concrete
  `Float32` type.
- Preserve the concrete target width in runtime arithmetic results by carrying
  or reapplying the selected width after each operation.
- Reuse the existing explicit `toFloat16` / `toFloat32` and direct
  literal-targeting finite-target rounding and overflow diagnostics.
- Preserve existing same concrete `Float`/`Float64` arithmetic and all integer
  arithmetic behavior.
- Preserve compile-time rejection for mixed `Int`/floating operands,
  `Float16`/`Float32`, `Float16`/`Float64`, `Float32`/`Float64`, unrelated
  non-numeric operands, and implicit-promotion cases.

Out of scope:

- literal suffix syntax,
- targeting `Float8`,
- implicit integer-to-float promotion,
- implicit mixed-width arithmetic widening,
- mixed-width comparison widening,
- typeclass dispatch, dictionaries, runtime evidence values, or broader
  numeric solver/defaulting behavior.

Batch target paths:

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

## Completion (2026-06-02)

Batch 1 landed in `jazz-next`. Same concrete `Float16` and `Float32`
arithmetic now type-checks and evaluates for `+`, `-`, `*`, and `/` by
preserving the selected runtime float target width across arithmetic results.
The implementation reuses finite-target rounding and overflow diagnostics,
keeps same concrete `Float`/`Float64` and integer arithmetic behavior intact,
and preserves rejection for mixed-width or implicit-promotion arithmetic,
including direct runtime fallback rejection for targeted narrow floats mixed
with untyped `Float`.
