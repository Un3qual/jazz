---
id: JN-PRIMITIVE-FUNCTION-EQUALITY-REJECTION-001
status: done
priority: P2
size: S
kind: impl
autonomous_ready: yes
depends_on:
  - JN-PRIMITIVE-STRUCTURAL-ADT-EQUALITY-001
last_verified: 2026-06-02
plan_section: "Batch 1: Function equality rejection"
target_paths:
  - jazz-next/src/JazzNext/Compiler/TypeInference.hs
  - jazz-next/src/JazzNext/Compiler/Runtime.hs
  - jazz-next/test/JazzNext/Compiler/Semantics/PrimitiveSemanticsSpec.hs
  - jazz-next/test/JazzNext/Compiler/Semantics/RuntimeSemanticsSpec.hs
  - jazz-next/test/JazzNext/Compiler/Semantics/LambdaSemanticsSpec.hs
verification:
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/PrimitiveSemanticsSpec.hs
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/RuntimeSemanticsSpec.hs
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/LambdaSemanticsSpec.hs
  - bash scripts/check-execution-queue.sh
  - bash scripts/check-docs.sh
deliverable: "Lock function, operator, and section values out of strict equality by rejecting source equality on callable types and preserving deterministic runtime rejection for direct helper coverage, without adding callable identity semantics."
---

# Function Equality Rejection Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use
> superpowers:subagent-driven-development (recommended) or
> superpowers:executing-plans to implement this plan task-by-task. Steps use
> checkbox (`- [ ]`) syntax for tracking.

**Goal:** Close the primitive equality edge around callable values by making
function, operator-section, and builtin-callable equality rejection explicit and
tested.

**Architecture:** Preserve strict type-directed equality. Equality remains
defined only for the active equality-supported value families. Callable values
must not gain pointer, closure, or identity equality semantics. Source type
checking should reject equality on callable types before runtime when the
source pipeline can see the type, while direct runtime helper tests should keep
the fatal diagnostic deterministic for impossible-to-typecheck helper inputs.

**Tech Stack:** Haskell `jazz-next` type inference/runtime, primitive and
lambda semantic specs, focused `runghc` verification, Markdown queue metadata.

---

## Source Verification

The primitive contract has already landed numeric equality/comparison,
structural tuple/list equality, and structural ADT equality. The remaining
primitive follow-up names function/operator/section equality as a separate
surface. This child batch narrows that item to rejection, not support:
callables are outside the strict equality-supported families.

## Batch 1: Function Equality Rejection

Scope:

- Reject direct equality and inequality between lambda/function values at
  source type checking.
- Reject equality and inequality involving operator sections that infer to
  callable values.
- Reject equality and inequality involving builtin callable values such as
  prelude/kernel functions when used as first-class values.
- Preserve existing accepted equality for integers, booleans, same concrete
  floating types, structural tuples/lists, and structural ADTs.
- Preserve deterministic runtime diagnostics for direct runtime helper coverage
  that constructs callable equality cases outside ordinary source type
  checking.
- Reuse the existing primitive equality diagnostic family where possible
  instead of adding a new callable-identity model.

Out of scope:

- Function identity, pointer, closure, or extensional equality,
- equality for partially applied constructors beyond the already rejected
  callable-family behavior,
- equality for untyped runtime internals outside direct helper tests,
- new overload dispatch, dictionaries, or typeclass solver behavior,
- literal suffixes, implicit integer-to-float promotion, or mixed-width
  widening.

Batch target paths:

- `jazz-next/src/JazzNext/Compiler/TypeInference.hs`
- `jazz-next/src/JazzNext/Compiler/Runtime.hs`
- `jazz-next/test/JazzNext/Compiler/Semantics/PrimitiveSemanticsSpec.hs`
- `jazz-next/test/JazzNext/Compiler/Semantics/RuntimeSemanticsSpec.hs`
- `jazz-next/test/JazzNext/Compiler/Semantics/LambdaSemanticsSpec.hs`

Suggested task order:

- [x] Add source-pipeline coverage for lambda equality and inequality
  rejection.
- [x] Add source-pipeline coverage for operator-section and builtin callable
  equality rejection.
- [x] Add direct runtime helper coverage only for callable equality cases that
  cannot be reached through a well-typed source program.
- [x] Tighten type inference/runtime diagnostics if current behavior is
  generic or inconsistent.
- [x] Re-run existing accepted primitive equality coverage to prove no numeric
  or structural equality regression.
- [x] Run the focused verification commands listed in frontmatter.

Focused verification:

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/PrimitiveSemanticsSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/RuntimeSemanticsSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/LambdaSemanticsSpec.hs
bash scripts/check-execution-queue.sh
bash scripts/check-docs.sh
```

Completion evidence (`2026-06-02`): rejected callable equality and inequality
for lambdas, operator sections, bare operators, bundled callables, unsaturated
constructors, and qualified method values without adding callable identity
semantics. Focused verification passed with `PrimitiveSemanticsSpec`,
`RuntimeSemanticsSpec`, `LambdaSemanticsSpec`, queue validation, and docs
validation.
