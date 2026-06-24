---
id: JN-PATTERN-GUARD-CONTRACT-001
status: ready
priority: P1
size: S
kind: coordination
autonomous_ready: yes
depends_on:
  - JN-PATTERN-FUTURE-FORMS-PLAN-001
last_verified: 2026-06-24
plan_section: "Contract batch: Pattern guards only"
target_paths:
  - docs/spec/pattern-matching-semantics.md
  - docs/spec/adt-pattern-semantics.md
  - docs/plans/2026-03-18-jazz-next-adt-and-pattern-matching-rebase-plan.md
  - docs/execution/queue.md
verification:
  - bash scripts/check-execution-queue.sh
  - bash scripts/check-docs.sh
deliverable: "Lock a source-backed pattern guard contract only: parser shape, binder scope, type/runtime semantics, diagnostics, and focused follow-up implementation targets without adding or-patterns, pattern synonyms, solver behavior, or parser/runtime code."
supersedes:
  - docs/plans/spec-clarification/2026-03-02/semantics/11-adt-and-pattern-matching-positioning.md
---

# Jazz-Next Pattern Guard Contract

> Active-path coordination child for `JN-PATTERN-FUTURE-FORMS-PLAN-001`. This
> batch locks one future pattern form, guards only, before any parser, type, or
> runtime implementation work.

**Goal:** define the first guard contract for `jazz-next` pattern matching while
keeping the landed literal, wildcard, variable, constructor, list, tuple, and
as-pattern subset stable.

**Architecture:** treat a guard as an optional boolean expression attached to a
case arm after the pattern has matched and before the arm body is selected. The
guard reuses branch-local pattern binders and does not introduce binders or new
solver behavior.

**Tech Stack:** docs/spec updates under `docs/spec/`, the active ADT/pattern
plan under `docs/plans/`, and queue verification through
`bash scripts/check-execution-queue.sh` plus `bash scripts/check-docs.sh`.

---

## Contract batch: Pattern guards only

This coordination batch updates only the target docs and queue state. It must
not edit parser, AST, type inference, runtime, or test implementation files.

Surface contract to lock:

- Accept at most one guard expression after a parsed pattern and before `->`.
- Use the existing `if` keyword as the guard introducer:

  ```jz
  case value {
    | Just item if item > 0 -> item
    | _ -> 0
  }
  ```

- A guarded arm is still one arm. The guard is not a nested pattern and is not a
  second arm separator.
- Unguarded arms keep the current `| <pattern> -> <expr>` form.

Binder and scope contract to lock:

- Pattern matching runs before guard evaluation.
- Binders introduced by the pattern are visible to the guard expression and the
  selected arm body.
- The guard expression introduces no binders.
- Existing duplicate-binder checks remain pattern-owned; a guard must not change
  duplicate-binder behavior.
- A binder introduced by one arm remains invisible to sibling arms and outside
  the `case` expression.

Type contract to lock:

- The pattern is typechecked against the scrutinee using the current pattern
  rules before the guard expression is checked.
- The guard expression must typecheck as `Bool` in the arm environment that
  includes pattern binders.
- Non-`Bool` guards emit a deterministic compile-time diagnostic at the guard
  expression span, using the existing boolean-condition diagnostic family with
  guard-specific text.
- Guard expressions do not participate in arm result agreement; only arm bodies
  continue to determine `E2012` result mismatches.
- Guards do not add inferred class constraints, broad defaulting, solver-backed
  constrained signatures, explicit type application, or runtime evidence.

Runtime contract to lock:

- Arms are tested from top to bottom.
- For each arm, runtime first matches the pattern.
- If the pattern does not match, the guard expression and arm body are not
  evaluated.
- If the pattern matches, runtime evaluates the guard expression in an
  environment containing the pattern binders.
- `True` selects the arm and evaluates its body.
- `False` falls through to the next arm.
- If no arm is selected after pattern and guard checks, runtime emits the
  existing deterministic no-match diagnostic `E3022`.
- Runtime errors from guard evaluation are fatal only for guards that are
  actually evaluated.

Diagnostics to lock:

- Malformed guard syntax is a parser diagnostic that points at the guard
  introducer or malformed guard expression.
- A non-`Bool` guard is a compile-time type diagnostic at the guard expression
  span.
- Pattern mismatch, unknown constructor, arity mismatch, duplicate binder, and
  arm-result mismatch diagnostics keep their existing ownership and codes.
- Exhaustiveness analysis remains out of scope.

Follow-up implementation target seed:

- `jazz-next/src/JazzNext/Compiler/Parser/AST.hs`
- `jazz-next/src/JazzNext/Compiler/Parser.hs`
- `jazz-next/src/JazzNext/Compiler/Parser/Lower.hs`
- `jazz-next/src/JazzNext/Compiler/AST.hs`
- `jazz-next/src/JazzNext/Compiler/Analyzer.hs`
- `jazz-next/src/JazzNext/Compiler/ModuleResolver.hs`
- `jazz-next/src/JazzNext/Compiler/Driver.hs`
- `jazz-next/src/JazzNext/Compiler/RecursiveBindings.hs`
- `jazz-next/src/JazzNext/Compiler/TypeInference.hs`
- `jazz-next/src/JazzNext/Compiler/Runtime.hs`
- `jazz-next/test/JazzNext/Compiler/Parser/AdtPatternParserSpec.hs`
- `jazz-next/test/JazzNext/Compiler/Semantics/AdtPatternTypeSpec.hs`
- `jazz-next/test/JazzNext/Compiler/Semantics/AdtPatternRuntimeSpec.hs`
- `jazz-next/test/JazzNext/Compiler/Modules/LoaderSpec.hs`

Out of scope:

- or-patterns,
- pattern synonyms,
- multiple guard clauses per arm,
- guard-introduced binders,
- exhaustiveness analysis,
- match-compilation optimizations,
- parser/type/runtime implementation in this coordination batch,
- any `jazz-hs/` or `jazz2/` work.

Verification:

```bash
bash scripts/check-execution-queue.sh
bash scripts/check-docs.sh
```
