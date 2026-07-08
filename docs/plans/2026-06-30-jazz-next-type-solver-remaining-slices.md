---
id: JN-TYPE-SOLVER-EXPLICIT-TYPE-APPLICATION-001
status: done
priority: P1
size: M
kind: impl
autonomous_ready: yes
depends_on:
  - JN-TYPE-SOLVER-FINAL-DEFAULTING-AMBIGUITY-001
last_verified: 2026-07-08
completed_on: 2026-07-08
plan_section: "Batch 2: Explicit Type Application"
target_paths:
  - docs/spec/semantics/bindings-and-signatures.md
  - jazz-next/src/JazzNext/Compiler/Parser.hs
  - jazz-next/src/JazzNext/Compiler/Parser/AST.hs
  - jazz-next/src/JazzNext/Compiler/Parser/Lower.hs
  - jazz-next/src/JazzNext/Compiler/AST.hs
  - jazz-next/src/JazzNext/Compiler/Analyzer.hs
  - jazz-next/src/JazzNext/Compiler/ModuleReplay.hs
  - jazz-next/src/JazzNext/Compiler/RecursiveBindings.hs
  - jazz-next/src/JazzNext/Compiler/Runtime.hs
  - jazz-next/src/JazzNext/Compiler/TypeInference.hs
  - jazz-next/test/JazzNext/Compiler/Parser/ParserFoundationSpec.hs
  - jazz-next/test/JazzNext/Compiler/Semantics/BindingSignatureCoherenceSpec.hs
  - docs/execution/blocker-contracts.md
  - docs/execution/queue.md
verification:
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Parser/ParserFoundationSpec.hs
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/BindingSignatureCoherenceSpec.hs
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/RuntimeSemanticsSpec.hs
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Modules/LoaderSpec.hs
  - bash scripts/check-execution-queue.sh
  - bash scripts/check-docs.sh
  - git diff --check
deliverable: "Add one explicit `@Type` application surface for already-generalized schemes, preserving monomorphic signature type shapes while keeping type lambdas, higher-rank polymorphism, runtime dictionaries, and module import/export behavior out of scope."
---

# Jazz-Next Remaining Type Solver Slices Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use
> superpowers:subagent-driven-development (recommended) or
> superpowers:executing-plans to implement this plan task-by-task. Steps use
> checkbox (`- [ ]`) syntax for tracking.

**Goal:** turn the remaining accepted type-solver semantics into separate,
verifier-backed implementation children.

**Architecture:** keep the completed ordinary-binding schemes and explicit
constrained-signature schemes as the base. Each next child expands one solver
axis only and carries focused type-inference tests; runtime evidence and
dictionaries remain last because they need the compile-time solver behavior to
exist first.

**Tech Stack:** Haskell `jazz-next` type inference, existing class/impl fact
metadata, source-pipeline tests in `BindingSignatureCoherenceSpec.hs`, future
runtime evidence tests in `RuntimeSemanticsSpec.hs`, active bindings/signatures
specs, and repo-root docs/queue validation.

---

## Remaining Solver Slice Order

1. `JN-TYPE-SOLVER-FINAL-DEFAULTING-AMBIGUITY-001` (completed `2026-07-08`)
2. `JN-TYPE-SOLVER-EXPLICIT-TYPE-APPLICATION-001` (completed `2026-07-08`)
3. `JN-TYPE-SOLVER-RUNTIME-EVIDENCE-DICTIONARIES-001`

The inferred class-constraint and final defaulting/ambiguity children are
complete and archived, and explicit type application is now complete and
archived. Runtime evidence/dictionaries remains the only accepted open child.
The remaining order keeps every open child independently testable.

## Completed Prerequisite: Inferred Class Constraints

Child id: `JN-TYPE-SOLVER-INFERRED-CLASS-CONSTRAINTS-001`

Status: completed in this PR and recorded in
`docs/execution/done-archive.md`.

Goal: infer class constraints from supported operators and qualified method
requirements, attach those constraints to generalized schemes, and solve them
against visible class/impl facts at use sites.

Target paths:

- `jazz-next/src/JazzNext/Compiler/TypeInference.hs`
- `jazz-next/test/JazzNext/Compiler/Semantics/BindingSignatureCoherenceSpec.hs`
- `docs/spec/semantics/bindings-and-signatures.md`

Accepted behavior:

```jazz
same = \(left) -> \(right) -> left == right.
intResult = same 1 1.
boolResult = same True False.
```

The binding `same` generalizes with an inferred `Eq(a)` constraint. Each use
instantiates the scheme and solves the concrete `Eq(Int)` or `Eq(Bool)` fact
against visible facts.

Negative behavior:

```jazz
f = \(x) -> x.
bad = f == f.
```

Expected diagnostic: callable equality remains unsupported and must not become
an inferred `Eq(function)` obligation.

Out of scope:

- final defaulting of unresolved constraints,
- runtime evidence values,
- dictionaries,
- explicit type application syntax,
- primitive mixed-width behavior,
- method import/export behavior,
- any `jazz-hs/` or `jazz2` work.

Focused verification:

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/BindingSignatureCoherenceSpec.hs
bash scripts/check-execution-queue.sh
bash scripts/check-docs.sh
git diff --check
```

## Batch 1: Final Defaulting And Ambiguity

Child id: `JN-TYPE-SOLVER-FINAL-DEFAULTING-AMBIGUITY-001`

Status: completed on `2026-07-08` and recorded in
`docs/execution/done-archive.md`.

Goal: run a final solver phase after unification and class/impl solving that
preserves current numeric literal defaults and emits deterministic ambiguity
diagnostics for still-unresolved class constraints.

Target paths:

- `jazz-next/src/JazzNext/Compiler/TypeInference.hs`
- `jazz-next/test/JazzNext/Compiler/Semantics/BindingSignatureCoherenceSpec.hs`
- `docs/spec/semantics/bindings-and-signatures.md`
- `docs/spec/runtime/primitive-semantics.md`

Accepted behavior:

```jazz
numeric = \(x) -> x + 1.
result = numeric 2.
```

The integer literal default remains `Int`/`Int64` when no narrower context is
provided.

Negative behavior:

```jazz
ambiguous = \(x) -> x == x.
ambiguous.
```

Expected diagnostic: unresolved `Eq(a)` ambiguity with the binding or terminal
expression span. The child must not invent a default for arbitrary `Eq(a)`.

Landed behavior: inferred unresolved class constraints now report
`ambiguous/defaulting inferred constraint ...`, while explicit constrained
signature ambiguity continues to report the explicit constrained-signature
diagnostic. Existing integer literal defaults remain preserved through the
final solver path.

Out of scope:

- implicit integer-to-float promotion,
- mixed concrete widths,
- runtime dictionary values,
- explicit type application syntax,
- higher-rank polymorphism.

Focused verification:

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/BindingSignatureCoherenceSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/PrimitiveSemanticsSpec.hs
bash scripts/check-execution-queue.sh
bash scripts/check-docs.sh
git diff --check
```

## Batch 2: Explicit Type Application

Child id: `JN-TYPE-SOLVER-EXPLICIT-TYPE-APPLICATION-001`

Status: completed on `2026-07-08` and recorded in
`docs/execution/done-archive.md`.

Goal: add one explicit type application surface for already-generalized
schemes, without adding higher-rank polymorphism or runtime evidence.

Accepted source form:

```jazz
id :: @{Eq(a)}: a -> a.
id = \(x) -> x.
value = id @Int 1.
```

Target paths:

- `docs/spec/semantics/bindings-and-signatures.md`
- `jazz-next/src/JazzNext/Compiler/Parser.hs`
- `jazz-next/src/JazzNext/Compiler/Parser/AST.hs`
- `jazz-next/src/JazzNext/Compiler/Parser/Lower.hs`
- `jazz-next/src/JazzNext/Compiler/AST.hs`
- `jazz-next/src/JazzNext/Compiler/Analyzer.hs`
- `jazz-next/src/JazzNext/Compiler/ModuleReplay.hs`
- `jazz-next/src/JazzNext/Compiler/RecursiveBindings.hs`
- `jazz-next/src/JazzNext/Compiler/Runtime.hs`
- `jazz-next/src/JazzNext/Compiler/TypeInference.hs`
- `jazz-next/test/JazzNext/Compiler/Parser/ParserFoundationSpec.hs`
- `jazz-next/test/JazzNext/Compiler/Semantics/BindingSignatureCoherenceSpec.hs`

Rules:

- `@Type` applies only to a callable expression whose type is a generalized
  scheme.
- The type argument must be one supported monomorphic type shape already
  accepted by signatures.
- Wrong arity, unsupported type application targets, and unsupported type
  argument shapes produce deterministic diagnostics.

Landed behavior: `expr @Type` parses and lowers into explicit
type-application nodes. The type argument uses the existing monomorphic
signature type grammar. Type inference instantiates generalized scheme
bindings with the explicit type for the first quantified variable, freshens
remaining variables, preserves existing deferred class/primitive obligations,
erases the node at runtime, and rejects monomorphic or already-instantiated
targets with `E2017`.

Out of scope:

- type lambdas,
- higher-rank polymorphism,
- implicit type application,
- runtime dictionaries,
- module import/export behavior.

Focused verification:

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Parser/ParserFoundationSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/BindingSignatureCoherenceSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/RuntimeSemanticsSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Modules/LoaderSpec.hs
bash scripts/check-execution-queue.sh
bash scripts/check-docs.sh
git diff --check
```

## Batch 3: Runtime Evidence And Dictionaries

Child id: `JN-TYPE-SOLVER-RUNTIME-EVIDENCE-DICTIONARIES-001`

Goal: introduce the first runtime evidence representation only after inferred
constraints, defaulting, and explicit type application have compile-time
coverage.

Target paths:

- `docs/spec/abstractions/capability-model.md`
- `docs/spec/semantics/bindings-and-signatures.md`
- `jazz-next/src/JazzNext/Compiler/TypeInference.hs`
- `jazz-next/src/JazzNext/Compiler/Runtime.hs`
- `jazz-next/test/JazzNext/Compiler/Semantics/BindingSignatureCoherenceSpec.hs`
- `jazz-next/test/JazzNext/Compiler/Semantics/RuntimeSemanticsSpec.hs`

Accepted behavior:

- Evidence values are compiler-owned runtime records for selected class/impl
  facts, not user-visible ordinary values.
- Explicit `Class::method` dispatch continues to work and can consume evidence
  internally.
- Missing, duplicate, or ambiguous evidence stays diagnostic-first and
  deterministic.

Out of scope:

- default methods,
- superclasses,
- method import/export behavior,
- orphan/overlap behavior beyond existing duplicate visible facts,
- optimizing dictionary representation.

Focused verification:

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/BindingSignatureCoherenceSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/RuntimeSemanticsSpec.hs
bash scripts/check-execution-queue.sh
bash scripts/check-docs.sh
git diff --check
```
