---
id: JN-LAMBDA-PARAM-PATTERN-SEMANTICS-001
status: done
priority: P1
size: M
kind: impl
autonomous_ready: no
depends_on:
  - JN-CONS-LIST-PATTERN-SEMANTICS-001
last_verified: 2026-05-25
plan_section: "Follow-up Batch: Lambda parameter pattern semantics"
target_paths:
  - jazz-next/src/JazzNext/Compiler/Parser/AST.hs
  - jazz-next/src/JazzNext/Compiler/Parser.hs
  - jazz-next/src/JazzNext/Compiler/Parser/Lower.hs
  - jazz-next/test/JazzNext/Compiler/Parser/LambdaParserSpec.hs
  - jazz-next/test/JazzNext/Compiler/Semantics/LambdaSemanticsSpec.hs
verification:
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Parser/LambdaParserSpec.hs
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/LambdaSemanticsSpec.hs
  - bash jazz-next/scripts/test-warning-config.sh
  - bash scripts/check-execution-queue.sh
  - bash scripts/check-docs.sh
deliverable: "Pattern-shaped lambda parameters parse into surface parameter patterns and lower into ordinary unary core lambdas plus internal pattern-case bodies, reusing active case-pattern type/runtime semantics."
---

# Jazz-Next Lambda Support Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Add canonical lambda syntax and executable user-defined function values to the active `jazz-next` parser, type inference, runtime, and docs.

**Architecture:** Parse canonical `\(args) -> expr` lambdas into a surface node that preserves the written parameter list, then lower multi-argument lambdas into nested unary core lambdas so runtime and type inference only need one callable user-function form. Keep this batch tightly scoped to identifier parameters, lexical closures, and ordinary application; pattern parameters, `class`/`impl`, and richer abstraction syntax remain out of scope.

**Tech Stack:** Haskell (`jazz-next` lexer/parser/analyzer/type/runtime/tests), Markdown docs/trackers, shell verification via `runghc` and `bash jazz-next/scripts/test-warning-config.sh`.

---

## Progress

- [x] Verified recent March 15-17 plans were already implemented or tracker drift, not the next code batch.
- [x] Verified `docs/spec/authoritative-syntax.md` still lists lambda syntax as a remaining alignment gap in active `jazz-next`.
- [x] Verified `jazz-next` currently lacks lambda AST/parser/runtime support, so first-class user-defined functions are still missing in the active implementation path.
- [x] Task 1 complete: failing parser tests lock canonical lambda surface and lowering shape.
- [x] Task 2 complete: failing compile/runtime tests lock callable closure behavior and type inference coverage.
- [x] Task 3 complete: lexer/parser/lowering/runtime/type/analyzer support canonical lambdas.
- [x] Task 4 complete: docs/trackers reflect the active lambda-capable subset and verification passes.
- [x] Review follow-up complete: wrapped self-recursive lambdas now seed/capture recursion through `if`/`case`/block wrappers in `jazz-next`.
- [x] Review follow-up complete: CodeRabbit safety/docs notes addressed for desugaring, invalid lambda lowering, closure-equality intent, and historical plan labeling.
- [x] Review follow-up complete: recursive peer type seeding now preserves earlier visible bindings, and alias-only recursive runtime bridges fail with a deterministic diagnostic.
- [x] Review follow-up complete: mixed `if`/`case` wrappers now seed recursive lambda typing early enough to reject non-function alternate branches deterministically.
- [x] Review follow-up complete: block-wrapped recursive alias loops now hit deterministic `E3021`, and duplicate lambda parameter shadowing is locked as intentional lowering behavior.
- [x] Review follow-up complete: runtime self-recursive wrapper seeding now matches branch-sensitive lambda typing, and obviously non-function recursive SCCs fail with deterministic `E3021`.
- [x] Review follow-up complete: block-returned lambda aliases now seed recursive typing/runtime correctly, including recursive mismatch rejection through same-block alias returns.
- [x] Review follow-up complete: wrapped alias-only recursive cycles now resolve to deterministic `E3021` instead of bypassing the recursive alias guard.
- [x] Review follow-up complete: wrapped recursive alias cycles now preserve `if`/`case` condition evaluation before alias resolution while still returning deterministic `E3021` on the selected alias branch.
- [x] Next task queued and un-deferred: shared recursive-binding helpers landed in `JazzNext.Compiler.RecursiveBindings` per `docs/plans/2026-03-17-jazz-next-shared-recursive-binding-helpers.md`.
- [x] On `2026-05-25`, landed lambda parameter pattern semantics by lowering pattern parameters through ordinary unary lambdas and internal pattern-case bodies.

## Next Task

- [x] Executed `docs/plans/2026-03-17-jazz-next-shared-recursive-binding-helpers.md` before unrelated parser/runtime follow-up work.
- [x] Kept the runtime `exprDefinitelyNotFunctionValue` policy and the type-inference `exprContainsFunctionBranch` policy separate while sharing the graph/free-var core.

## Scope Guardrails

In scope:

- canonical lambda syntax `\(x) -> expr` and `\(x, y) -> expr`
- identifier and pattern-shaped parameter lists
- lowering multi-argument lambdas into nested unary core lambdas, with pattern
  parameters lowering through internal `EPatternCase` bodies
- lexical closure capture in runtime execution
- type inference for lambda introduction and ordinary application
- docs/tracker updates needed to stop relying on legacy-only evidence for first-class functions

Out of scope for the first lambda batch:

- pattern lambda parameters, now handled by the follow-up semantics batch below
- `class` / `impl` parser support
- pattern features beyond the active case-pattern subset
- new warning categories or purity-policy redesign

## Follow-up Batch: Lambda parameter pattern rejection boundary

This batch landed on `2026-05-24`. It keeps lambda support identifier-only and
only makes the deferred parameter-pattern boundary explicit.

- [x] Reject tuple-shaped, bracketed-list, wildcard, and constructor-like lambda
  parameter forms with a deterministic parser diagnostic that names deferred
  lambda-parameter pattern semantics.
- [x] Preserve canonical identifier-only lambda parsing and multi-argument
  lowering behavior.
- [x] Add focused parser coverage in the lambda parser suite.

Batch files:

- `jazz-next/src/JazzNext/Compiler/Parser.hs`
- `jazz-next/test/JazzNext/Compiler/Parser/LambdaParserSpec.hs`

Batch verification:

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Parser/LambdaParserSpec.hs
bash jazz-next/scripts/test-warning-config.sh
bash scripts/check-execution-queue.sh
bash scripts/check-docs.sh
```

## Follow-up Batch: Lambda parameter pattern semantics

This batch landed on `2026-05-25`. It consumed the rejection boundary by
lowering pattern-shaped lambda parameters through the existing `case` pattern
engine instead of adding a second runtime matcher.

- [x] Represent lambda parameters as either identifiers or surface patterns.
- [x] Parse wildcard, tuple, bracketed-list, cons-like list, and constructor
  parameter patterns while preserving the existing identifier-parameter syntax.
- [x] Lower each pattern parameter to an ordinary internal unary core lambda
  whose body performs a single-arm `EPatternCase` over the generated argument,
  then continue nesting multi-argument lambdas in source order.
- [x] Reuse existing pattern typing, binder scoping, and runtime no-match
  diagnostics; do not add a new core callable form.
- [x] Add parser and semantic coverage for accepted destructuring parameters,
  typed binder failures, and runtime no-match behavior.

Batch files:

- `jazz-next/src/JazzNext/Compiler/Parser/AST.hs`
- `jazz-next/src/JazzNext/Compiler/Parser.hs`
- `jazz-next/src/JazzNext/Compiler/Parser/Lower.hs`
- `jazz-next/test/JazzNext/Compiler/Parser/LambdaParserSpec.hs`
- `jazz-next/test/JazzNext/Compiler/Semantics/LambdaSemanticsSpec.hs`

Batch verification:

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Parser/LambdaParserSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/LambdaSemanticsSpec.hs
bash jazz-next/scripts/test-warning-config.sh
bash scripts/check-execution-queue.sh
bash scripts/check-docs.sh
```

## Task 1: Lock Canonical Lambda Surface With Failing Parser Tests

**Files:**
- Create: `jazz-next/test/JazzNext/Compiler/Parser/LambdaParserSpec.hs`
- Modify: `jazz-next/scripts/test-warning-config.sh`

**Step 1: Write the failing parser/lowering tests**

Add coverage for:

- `id = \(x) -> x.` parsing as a lambda surface node
- `const = \(x, y) -> x.` parsing as one lambda node with two parameters
- `apply = \(f, x) -> f x.` preserving application in the body
- `run = (\(x) -> x) 1.` parsing parenthesized lambdas in application position
- lowering converts multi-argument lambdas into nested unary core lambdas
- invalid forms fail deterministically:
  - `\( ) -> x`
  - `\x -> x`
  - `\(x,) -> x`
  - `\(if) -> if`

**Step 2: Run the targeted parser suite to verify RED**

Run:

```bash
runghc -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Parser/LambdaParserSpec.hs
```

Expected:

- the new suite fails because the lexer/parser/lowering paths do not yet recognize lambda syntax.

**Step 3: Add the new suite to the default runner**

Update `jazz-next/scripts/test-warning-config.sh` so the lambda parser suite becomes part of the active verification script once implementation lands.

**Step 4: Commit checkpoint**

```bash
git add docs/plans/2026-03-17-jazz-next-lambda-support.md \
  jazz-next/test/JazzNext/Compiler/Parser/LambdaParserSpec.hs \
  jazz-next/scripts/test-warning-config.sh
git commit -m "test(jazz-next): lock canonical lambda parser surface"
```

## Task 2: Lock Closure and Type Behavior With Failing Compile/Runtime Tests

**Files:**
- Create: `jazz-next/test/JazzNext/Compiler/Semantics/LambdaSemanticsSpec.hs`
- Modify: `jazz-next/scripts/test-warning-config.sh`

**Step 1: Write the failing semantic tests**

Add compile/runtime coverage for:

- `id = \(x) -> x. id 1.` runs to `1`
- `const = \(x, y) -> x. const 1 2.` runs to `1`
- `inc = \(x) -> x + 1. inc 2.` runs to `3`
- `makeAdder = \(x) -> \(y) -> x + y. add2 = makeAdder 2. add2 3.` runs to `5`
- `apply = \(f, x) -> f x. apply (\(n) -> n + 1) 2.` runs to `3`
- compile-time rejection of calling a lambda with mismatched argument types after signature attachment:
  - `id :: Int -> Int. id = \(x) -> x. id True.`
- compile-time rejection of non-callable application still behaves deterministically:
  - `x = 1 2.`

**Step 2: Run the targeted semantic suite to verify RED**

Run:

```bash
runghc -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/LambdaSemanticsSpec.hs
```

Expected:

- the suite fails because user-defined lambdas/closures are not implemented yet.

**Step 3: Add the new suite to the default runner**

Update `jazz-next/scripts/test-warning-config.sh` so the lambda semantics suite runs in full verification.

**Step 4: Commit checkpoint**

```bash
git add docs/plans/2026-03-17-jazz-next-lambda-support.md \
  jazz-next/test/JazzNext/Compiler/Semantics/LambdaSemanticsSpec.hs \
  jazz-next/scripts/test-warning-config.sh
git commit -m "test(jazz-next): lock lambda closure semantics"
```

## Task 3: Implement Canonical Lambdas Across The Active Compiler Path

**Files:**
- Modify: `jazz-next/src/JazzNext/Compiler/Parser/Lexer.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Parser.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Parser/AST.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Parser/Lower.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/AST.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Analyzer.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/TypeInference.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Runtime.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Parser/LambdaParserSpec.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Semantics/LambdaSemanticsSpec.hs`

**Step 1: Add the minimal AST forms**

Introduce:

- one surface lambda constructor that preserves parameter order, for example `SELambda [Identifier] SurfaceExpr`
- one unary core lambda constructor, for example `ELambda Identifier Expr`

Keep lowering responsible for turning multi-argument surface lambdas into nested core lambdas. Do not add a separate multi-argument core callable form.

**Step 2: Extend the lexer and parser**

Add the smallest token/parser changes needed to support:

- backslash lambda introducer `\`
- arrow token `->`
- parameter lists inside `(...)` with comma separation
- lambda bodies parsed as ordinary expressions
- parenthesized lambdas participating in application and infix precedence normally

Keep rejection explicit for non-identifier parameters in this batch.

**Step 3: Extend analyzer visibility**

Allow lambda parameters to bind inside their body for unbound-variable analysis and purity checks. Lambda parameters are ordinary pure local bindings for this phase.

**Step 4: Add runtime closure support**

Introduce a runtime closure value that captures:

- defining environment
- bound parameter identifier
- lambda body expression

Apply closures by extending the captured environment with the call argument before evaluating the body.

**Step 5: Add type inference for lambdas**

Implement the minimal lambda typing rule:

- create a fresh type variable for the parameter
- infer the body under an environment extended with the parameter binding
- return `TFunctionType paramType bodyType`

Keep ordinary application inference unchanged apart from now being able to consume lambda-produced function types.

**Step 6: Run targeted suites to verify GREEN**

Run:

```bash
runghc -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Parser/LambdaParserSpec.hs
runghc -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/LambdaSemanticsSpec.hs
```

Expected:

- both new suites pass.

**Step 7: Commit checkpoint**

```bash
git add docs/plans/2026-03-17-jazz-next-lambda-support.md \
  jazz-next/src/JazzNext/Compiler/Parser/Lexer.hs \
  jazz-next/src/JazzNext/Compiler/Parser.hs \
  jazz-next/src/JazzNext/Compiler/Parser/AST.hs \
  jazz-next/src/JazzNext/Compiler/Parser/Lower.hs \
  jazz-next/src/JazzNext/Compiler/AST.hs \
  jazz-next/src/JazzNext/Compiler/Analyzer.hs \
  jazz-next/src/JazzNext/Compiler/TypeInference.hs \
  jazz-next/src/JazzNext/Compiler/Runtime.hs \
  jazz-next/test/JazzNext/Compiler/Parser/LambdaParserSpec.hs \
  jazz-next/test/JazzNext/Compiler/Semantics/LambdaSemanticsSpec.hs \
  jazz-next/scripts/test-warning-config.sh
git commit -m "feat(jazz-next): add canonical lambda closures"
```

## Task 4: Align Docs, Trackers, And Verification

**Files:**
- Modify: `docs/spec/authoritative-syntax.md`
- Modify: `docs/feature-status.md`
- Modify: `docs/jazz-language-state.md`
- Modify: `docs/plans/spec-cleanup/2026-03-02/decisions/01-authoritative-syntax.md`
- Modify: `docs/plans/2026-03-17-jazz-next-lambda-support.md`

**Step 1: Update docs and trackers**

Reflect that active `jazz-next` now supports:

- canonical lambdas with identifier or pattern-shaped parameters
- first-class user-defined functions
- currying through nested unary lambda lowering plus ordinary application

Keep broader abstraction syntax (`class` / `impl`) explicitly pending.

**Step 2: Reconcile stale authoritative-syntax tracking**

Mark the `jazz-next` lambda slice complete without claiming broader abstraction support is done.

**Step 3: Run focused and full verification**

Run:

```bash
runghc -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Parser/LambdaParserSpec.hs
runghc -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/LambdaSemanticsSpec.hs
bash jazz-next/scripts/test-warning-config.sh
bash scripts/check-docs.sh
```

Expected:

- lambda parser and semantic suites pass
- full `jazz-next` verification passes
- docs checks pass

**Step 4: Commit checkpoint**

```bash
git add docs/plans/2026-03-17-jazz-next-lambda-support.md \
  docs/spec/authoritative-syntax.md \
  docs/feature-status.md \
  docs/jazz-language-state.md \
  docs/plans/spec-cleanup/2026-03-02/decisions/01-authoritative-syntax.md
git commit -m "docs(jazz-next): track active lambda support"
```
