---
id: JN-PRIMITIVE-FLOAT16-FLOAT32-LITERAL-TARGETING-001
status: done
priority: P1
size: M
kind: impl
autonomous_ready: no
depends_on:
  - JN-FLOAT16-FLOAT32-SAME-WIDTH-COMPARISON-EQUALITY-001
last_verified: 2026-06-01
completed_on: 2026-06-01
plan_section: "Batch 1: Explicit Float16/Float32 fractional literal targeting"
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
deliverable: "Accept explicitly annotated Float16 and Float32 fractional literal values while preserving unannotated Float64 defaults, no literal suffix syntax, no implicit integer-to-float promotion, and mixed-width rejection."
---

# Jazz Next Float16/Float32 Literal Targeting

## Source Verification

This child plan narrows the primitive umbrella plan section
`Follow-up: Primitive deltas after child-plan reseed`. The umbrella lists
`Float16/Float32 literal targeting` as remaining primitive work after the
numeric conversion, arithmetic-gate, and same-width comparison/equality batches
landed.

The executor-safe next batch is limited to explicit type-context targeting for
fractional literals. It does not add suffix syntax or implicit promotion.

## Batch 1: Explicit Float16/Float32 Fractional Literal Targeting

Scope:

- Accept fractional literal bindings with explicit `Float16` and `Float32`
  signatures.
- Ensure the inferred binding type is the annotated concrete float width rather
  than the default `Float64`.
- Preserve the current unannotated fractional literal default of `Float64`.
- Preserve compile-time rejection for integer-to-float promotion and mixed
  concrete-width arithmetic/comparison/equality.
- Add runtime coverage proving explicitly targeted `Float16` and `Float32`
  literal values participate in already landed same-width comparison/equality
  while same-width `Float16`/`Float32` arithmetic remains gated until
  width-preserving runtime arithmetic exists.
- Apply the same finite-target source-exact bounds and runtime rounding used by
  explicit `toFloat16` and `toFloat32` conversions.

Out of scope:

- literal suffix syntax,
- targeting `Float8`,
- implicit integer-to-float promotion,
- implicit mixed-width numeric widening,
- broader numeric solver/defaulting behavior,
- typeclass dispatch, dictionaries, or runtime evidence.

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
