---
id: JN-OPERATORS-CUSTOM-ASSOCIATIVITY-001
status: done
priority: P1
size: M
kind: impl
autonomous_ready: yes
depends_on:
  - JN-OPERATORS-CUSTOM-PRECEDENCE-001
last_verified: 2026-07-08
completed_on: 2026-07-08
plan_section: "Batch 3: Custom associativity"
target_paths:
  - docs/spec/syntax/operators.md
  - jazz-next/src/JazzNext/Compiler/Parser.hs
  - jazz-next/src/JazzNext/Compiler/Parser/AST.hs
  - jazz-next/src/JazzNext/Compiler/Parser/Lower.hs
  - jazz-next/test/JazzNext/Compiler/Parser/OperatorFixitySpec.hs
  - jazz-next/test/JazzNext/Compiler/Parser/OperatorInvalidSyntaxSpec.hs
  - docs/execution/blocker-contracts.md
  - docs/execution/queue.md
verification:
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Parser/OperatorFixitySpec.hs
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Parser/OperatorInvalidSyntaxSpec.hs
  - bash scripts/check-execution-queue.sh
  - bash scripts/check-docs.sh
  - git diff --check
deliverable: "Add explicit `left`, `right`, and `nonassoc` associativity syntax for same-source user operators, preserving existing tier and precedence declarations while keeping new precedence ranges, cross-module operator APIs, overload dispatch, and new built-ins out of scope."
---

# Jazz-Next Operator Signatures, Precedence, And Associativity Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use
> superpowers:subagent-driven-development (recommended) or
> superpowers:executing-plans to implement this plan task-by-task. Steps use
> checkbox (`- [ ]`) syntax for tracking.

**Goal:** continue user-defined operators beyond same-source function bindings
with a staged plan for operator-specific signatures first, then custom
precedence and custom associativity.

**Architecture:** keep Stage 2 source-unit-local operator declarations and
`(op) = <expr>.` executable bindings intact. Add one feature per child:
adjacent type signatures for operator bindings, then custom numeric precedence,
then custom associativity. Do not add cross-module operator APIs, overload
dispatch, dictionaries, or new built-in operators in this roadmap.

**Tech Stack:** Haskell `jazz-next` parser/lowering/type inference/runtime,
active operator syntax spec, focused parser and semantic `runghc` suites, and
repo-root queue/docs validation.

---

## Roadmap

1. `JN-OPERATORS-SPECIFIC-TYPE-SIGNATURES-001` (done 2026-06-30)
2. `JN-OPERATORS-CUSTOM-PRECEDENCE-001` (done 2026-07-08)
3. `JN-OPERATORS-CUSTOM-ASSOCIATIVITY-001` (done 2026-07-08)

The queue should promote only one child at a time.

## Batch 1: Operator-specific type signatures

Accepted source form:

```jazz
operator %% tier 2.
(%%) :: Int -> Int -> Int.
(%%) = \(left) -> \(right) -> left + right.
result = 1 %% 2.
```

Rules:

- The signature form is an adjacent signature for the parenthesized operator
  binding name.
- `op` must already be declared in the same source unit.
- Built-in operators cannot receive user operator signatures.
- The signature must immediately precede the `(op) = <expr>.` binding.
- Signature attachment follows the existing immediate-adjacency rule.
- Runtime behavior remains the same as the completed declared-operator binding
  child; the signature constrains the ordinary hidden binding type.

Target paths:

- `docs/spec/syntax/operators.md`
- `jazz-next/src/JazzNext/Compiler/Parser.hs`
- `jazz-next/src/JazzNext/Compiler/Parser/AST.hs`
- `jazz-next/src/JazzNext/Compiler/Parser/Lower.hs`
- `jazz-next/src/JazzNext/Compiler/TypeInference.hs`
- `jazz-next/test/JazzNext/Compiler/Parser/OperatorFixitySpec.hs`
- `jazz-next/test/JazzNext/Compiler/Parser/OperatorInvalidSyntaxSpec.hs`
- `jazz-next/test/JazzNext/Compiler/Semantics/PrimitiveSemanticsSpec.hs`
- `jazz-next/test/JazzNext/Compiler/Semantics/RuntimeSemanticsSpec.hs`

Focused coverage:

```jazz
operator %% tier 2.
(%%) :: Int -> Int -> Int.
(%%) = \(left) -> \(right) -> left + right.
result :: Int.
result = 1 %% 2.
```

Expected: compile and run succeed.

```jazz
operator %% tier 2.
(%%) :: Bool -> Bool -> Bool.
(%%) = \(left) -> \(right) -> left + right.
result = 1 %% 2.
```

Expected: deterministic type error from the binding body or use site.

```jazz
(%%) :: Int -> Int -> Int.
operator %% tier 2.
(%%) = \(left) -> \(right) -> left + right.
```

Expected: parser or signature attachment error because the operator was not
declared before its signature.

Verification:

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Parser/OperatorFixitySpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Parser/OperatorInvalidSyntaxSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/PrimitiveSemanticsSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/RuntimeSemanticsSpec.hs
bash scripts/check-execution-queue.sh
bash scripts/check-docs.sh
git diff --check
```

## Batch 2: Custom precedence

Child id: `JN-OPERATORS-CUSTOM-PRECEDENCE-001`

Status: done 2026-07-08.

Accepted source form:

```jazz
operator %% precedence 25.
(%%) = \(left) -> \(right) -> left + right.
result = 1 + 2 %% 3 * 4.
```

Rules:

- `precedence` accepts an integer from `1` to `99`.
- Higher number means tighter binding.
- Built-in tier anchors keep their current relative ordering.
- A custom-precedence operator without explicit associativity defaults to left
  associativity.
- Existing `tier <1-5>` declarations remain valid.
- Duplicate declarations, built-in symbols, and reserved symbols still reject.

Out of scope:

- custom associativity syntax,
- cross-module operator declarations,
- overload dispatch,
- new built-in operators.

Verification:

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Parser/OperatorFixitySpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Parser/OperatorInvalidSyntaxSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/RuntimeSemanticsSpec.hs
bash scripts/check-execution-queue.sh
bash scripts/check-docs.sh
git diff --check
```

## Batch 3: Custom associativity

Child id: `JN-OPERATORS-CUSTOM-ASSOCIATIVITY-001`

Status: done 2026-07-08.

Accepted source forms:

```jazz
operator <| precedence 10 right.
operator ?> tier 4 nonassoc.
```

Rules:

- Associativity keywords are `left`, `right`, and `nonassoc`.
- `left` and `right` control grouping for adjacent operators with the same
  precedence.
- `nonassoc` rejects unparenthesized chains of the same precedence when both
  sides would need grouping.
- If omitted, associativity defaults to the inherited tier associativity for
  `tier` declarations and `left` for `precedence` declarations.

Out of scope:

- new precedence ranges,
- cross-module operator APIs,
- overload dispatch,
- new built-ins.

Verification:

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Parser/OperatorFixitySpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Parser/OperatorInvalidSyntaxSpec.hs
bash scripts/check-execution-queue.sh
bash scripts/check-docs.sh
git diff --check
```
