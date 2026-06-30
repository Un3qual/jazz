---
id: JN-PRIMITIVE-TYPED-INT-TO-FLOAT64-PROMOTION-001
status: done
priority: P1
size: M
kind: impl
autonomous_ready: yes
depends_on:
  - JN-PRIMITIVE-FLOAT64-INTEGER-LITERAL-ARITHMETIC-001
last_verified: 2026-06-30
completed_on: 2026-06-30
plan_section: "Task 1: Direct typed integer to Float64 promotion"
target_paths:
  - docs/spec/runtime/primitive-semantics.md
  - jazz-next/src/JazzNext/Compiler/TypeInference.hs
  - jazz-next/src/JazzNext/Compiler/Runtime.hs
  - jazz-next/test/JazzNext/Compiler/Semantics/PrimitiveSemanticsSpec.hs
  - jazz-next/test/JazzNext/Compiler/Semantics/RuntimeSemanticsSpec.hs
  - docs/execution/blocker-contracts.md
  - docs/execution/queue.md
verification:
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/PrimitiveSemanticsSpec.hs
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/RuntimeSemanticsSpec.hs
  - bash scripts/check-execution-queue.sh
  - bash scripts/check-docs.sh
  - git diff --check
deliverable: "Accept direct binary typed integer to Float/Float64 promotion for arithmetic, comparison, and equality when exactly one operand is integral and the other is `Float`/`Float64`, while preserving Float16/Float32 rejection, mixed concrete float-width rejection, operator-value and section rejection, explicit conversion APIs, and broader solver/defaulting behavior."
---

# Jazz-Next Typed Integer To Float64 Promotion Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use
> superpowers:subagent-driven-development (recommended) or
> superpowers:executing-plans to implement this plan task-by-task. Steps use
> checkbox (`- [ ]`) syntax for tracking.

**Goal:** extend the landed direct integer-literal Float64 arithmetic exception
to direct binary typed integral values paired with `Float`/`Float64` operands.

**Architecture:** keep explicit conversions as the normal cross-width API.
Add one local primitive rule for direct binary expressions where one side has a
concrete integral type and the other side is in the `Float`/`Float64` domain.
The rule promotes the integral operand to the peer Float64-domain type for that
expression only. It does not change global unification, operator values,
sections, Float16/Float32 behavior, or the broader solver/defaulting contract.

**Tech Stack:** Haskell `jazz-next` type inference/runtime, active primitive
semantics spec, focused `PrimitiveSemanticsSpec` and `RuntimeSemanticsSpec`
coverage, and repo-root queue/docs validation.

---

## Source Evidence

- `JN-PRIMITIVE-FLOAT64-INTEGER-LITERAL-ARITHMETIC-001` already accepts exactly
  one uncommitted integer literal in direct binary `Float`/`Float64`
  arithmetic.
- The primitive blocker remains open for a separate typed integer-to-float
  promotion contract.
- The maintainer selected int-to-float promotion on 2026-06-30.

## Task 1: Direct typed integer to Float64 promotion

Scope:

- Accept direct binary `+`, `-`, `*`, and `/` when exactly one operand has a
  concrete integral type and the other operand resolves to `Float` or
  `Float64`.
- Accept direct binary `<`, `<=`, `>`, `>=`, `==`, and `!=` under the same
  operand rule, returning `Bool`.
- Keep the result type of arithmetic in the peer Float64 domain: `Float` stays
  `Float`, explicit `Float64` stays `Float64`.
- Preserve range and finite-target checks when converting integral runtime
  values to Float64.
- Preserve existing same-concrete integer, Float, Float16, Float32, and Float64
  behavior when both operands already match.

Invalid and out of scope:

- `Float16` or `Float32` mixed with any integral operand remains a type error.
- Mixed concrete float widths remain type errors.
- Operator values and sections such as `(+) intValue floatValue`,
  `(intValue +)`, and `(+ floatValue)` remain out of scope for this child.
- User-defined operators do not inherit this promotion behavior.
- Broad numeric solver/defaulting, typeclass dispatch, dictionaries, callable
  identity, and implicit conversion APIs remain out of scope.

## Focused Test Changes

In `PrimitiveSemanticsSpec.hs`, add source-pipeline acceptance:

```jazz
leftInt :: Int.
leftInt = 1.
rightFloat :: Float.
rightFloat = 1.5.
sum = leftInt + rightFloat.
comparison = leftInt < rightFloat.
equality = leftInt == rightFloat.
```

In the same suite, add explicit `Float64` acceptance:

```jazz
left64 :: Int64.
left64 = 2.
right64 :: Float64.
right64 = toFloat64 3.
product :: Float64.
product = left64 * right64.
```

Add rejection coverage:

```jazz
left :: Int.
left = 1.
right :: Float32.
right = toFloat32 1.
bad = left + right.
```

Expected diagnostic: numeric operand type error.

Add operator-value and section rejection coverage:

```jazz
left :: Int.
left = 1.
right :: Float.
right = 1.5.
bad = (+) left right.
```

Expected diagnostic: operator-value promotion is out of scope.

In `RuntimeSemanticsSpec.hs`, add runtime success for arithmetic, comparison,
and equality with typed integral values and `Float`/`Float64` peers, and keep
runtime fallback rejections for Float16/Float32 and mixed concrete floats.

## Verification

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/PrimitiveSemanticsSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/RuntimeSemanticsSpec.hs
bash scripts/check-execution-queue.sh
bash scripts/check-docs.sh
git diff --check
```
