# Jazz Interpreter Roadmap (2026-03-03)

## Purpose

This document summarizes the current state of Jazz, what has been completed, what remains, which detailed plans already exist, and where the current planning is incomplete for reaching a fully working interpreter.

## Active-Path Plan Status

- Active runtime architecture and execution plan:
  - `docs/plans/2026-03-18-jazz-next-runtime-architecture-and-interpreter-execution-plan.md`
- Legacy runtime implementation plan retained for reference only:
  - `docs/plans/spec-clarification/2026-03-02/runtime/12a-haskell-interpreter-implementation.md`
- Active type-grammar and signature rebase plan:
  - `docs/plans/2026-03-18-jazz-next-type-grammar-and-signature-rebase-plan.md`
- Legacy type-grammar clarification plan retained for reference only:
  - `docs/plans/spec-clarification/2026-03-02/type-system/07-type-grammar-and-arrow-associativity.md`

## Evidence Baseline

This roadmap is based on the current repository state as of 2026-03-03, including:

- `README.md`
- `docs/feature-status.md`
- `docs/jazz-language-state.md`
- `docs/spec/*`
- `docs/plans/spec-cleanup/2026-03-02/*`
- `docs/plans/spec-clarification/2026-03-02/*`
- `docs/plans/spec-clarification/2026-03-03/*`
- `jazz-next/src/*`
- `jazz-next/test/*`

Verification run in this snapshot:

- `bash jazz-next/scripts/test-warning-config.sh` (all tests passing)

## 1) What We Have Done

### Governance and authority
- Established authority policy and drift checks:
  - `docs/spec/governance/spec-authority-policy.md`
  - `scripts/check-spec-authority.sh`
- Locked `jazz2` as reference-only and non-normative.
- Locked hybrid change workflow (semantic changes require decisions first).

### Spec decisions published
- Canonical syntax contract: `docs/spec/authoritative-syntax.md`
- Binding/signature semantics: `docs/spec/semantics/bindings-and-signatures.md`
- If-expression contract: `docs/spec/control-flow/if-expressions.md`
- Operator/fixity/sections contract: `docs/spec/syntax/operators.md`
- Primitive semantics contract: `docs/spec/runtime/primitive-semantics.md`
- Warning-flag contract: `docs/spec/tooling/compiler-warning-flags.md`

### `jazz-next` implementation progress (real code)
- Parser foundation exists for a minimal surface:
  - integers, identifiers, braces scopes, `name = expr.`, `name :: Type.`, expression statements.
- Parse/lower boundary exists:
  - `Parser/AST.hs` + `Parser/Lower.hs` -> analyzer AST.
- Analyzer semantics implemented for current narrow AST:
  - signature adjacency checks,
  - use-before-definition errors,
  - same-scope `last wins` rebinding model,
  - recursion-group support in current statement model.
- Warning tooling implemented end-to-end for `same-scope-rebinding` (`W0001`):
  - warning category model,
  - CLI/env/config resolution with precedence,
  - warning-as-error behavior,
  - deterministic warning ordering.
- CLI + driver integration implemented for warning policy behavior.

## 2) Where We Are Now

### Current working interpreter status

Jazz does **not** yet have a working interpreter in the active implementation path (`jazz-next/`).

Current `jazz-next` behavior is a compiler front-end skeleton with:
- parsing/lowering for a minimal syntax subset,
- structural semantic checks,
- warning policy infrastructure,
- placeholder output (`"/* jazz-next codegen placeholder */"`) instead of runtime execution.

### Important reality check

- The repository contains plans for interpreter work (`12`, legacy `12a`), ADT/pattern runtime work (`11`), module loader (`09`), stdlib boundary (`10`), and legacy type-grammar clarification (`07`). Domains `09` and `12` still need active-path rebasing work; the `12a`, `11`, and `07` follow-ons now have active-path replacement plans, and domain `10` is closed for the current `jazz-next` runtime subset.
- Current workspace guardrails require all net-new compiler/runtime behavior work in `jazz-next/`.

## 3) Detailed Plans Already Written

### Mostly complete or actively executed
- `2026-03-18-jazz-next-runtime-architecture-and-interpreter-execution-plan.md` (active-path replacement for legacy `12a`; defines the `jazz-next` runtime pipeline, milestones, and dependency map)
- `2026-03-18-jazz-next-type-grammar-and-signature-rebase-plan.md` (active-path replacement for legacy `07`; defines the `jazz-next` type/signature ownership boundary, milestones, and verification targets)
- `2026-03-18-jazz-next-adt-and-pattern-matching-rebase-plan.md` (active-path replacement for legacy `11`; defines the `jazz-next` ADT/pattern owner map, milestones, and first executable batch)
- `13-binding-and-signature-coherence.md` (partially executed in `jazz-next`; core analyzer contract implemented)
- `17-jazz2-alignment-and-spec-authority.md` (executed)
- `18-compiler-warning-flags.md` (executed in `jazz-next`)
- `05-readme-implemented-vs-planned.md` (executed docs split/matrix work)

### Decision/spec locked, implementation still pending
- `14-if-expression-surface-and-semantics.md`
- `15-operator-fixity-and-sections.md`
- `16-primitive-semantics-contract.md`
- `01-authoritative-syntax.md`
- `02-map-filter-order.md`
- `03-purity-bang-semantics.md`
- `04-trait-vs-class-keyword.md`
- `06-parse-only-features-resolution.md`

### Detailed but largely not executed (legacy-targeted plans)
- `07-type-grammar-and-arrow-associativity.md` (legacy-targeted; replaced for new work by `2026-03-18-jazz-next-type-grammar-and-signature-rebase-plan.md`)
- `08-trait-vocabulary-and-capability-model.md`
- `09-module-loader-and-import-resolution.md`
- `10-stdlib-boundary-selfhosted-vs-hardcoded.md`
- `11-adt-and-pattern-matching-positioning.md` (legacy-targeted; replaced for new work by `2026-03-18-jazz-next-adt-and-pattern-matching-rebase-plan.md`)
- `12-backend-target-strategy.md`
- `12a-haskell-interpreter-implementation.md` (legacy-targeted; replaced for new work by `2026-03-18-jazz-next-runtime-architecture-and-interpreter-execution-plan.md`)

## 4) What Is Left To Reach a Fully Working Interpreter

### Phase A: Re-anchor execution plans to `jazz-next` (required first)

1. Convert legacy-targeted runtime plans (`12`, `12a`) into `jazz-next` execution plans and keep the new ADT replacement plan aligned with the same runtime architecture.
2. Rebase the remaining module-loader plan (`09`) onto `jazz-next` parser/analyzer/runtime architecture and keep the new active-path type-grammar and ADT plans aligned with it.
3. Explicitly mark legacy `jazz-hs` plans as evidence-only where they are no longer execution targets.

Execution note:
- The `12a` runtime follow-on is now covered by `docs/plans/2026-03-18-jazz-next-runtime-architecture-and-interpreter-execution-plan.md`, the `07` type-grammar follow-on is now covered by `docs/plans/2026-03-18-jazz-next-type-grammar-and-signature-rebase-plan.md`, and the `11` ADT follow-on is now covered by `docs/plans/2026-03-18-jazz-next-adt-and-pattern-matching-rebase-plan.md`; remaining Phase A rebases are `09` and `12`.

### Phase B: Expand parser + AST + lowering to spec contracts

1. Implement `if` surface parse + lowering path (plan `14`).
2. Implement operator table/fixity/section AST forms (plan `15`).
3. Expand expression grammar beyond int/var/scope to support canonical forms required by specs.
4. Add parser contract tests for each new grammar slice before analyzer/runtime work.

### Phase C: Implement actual type system behavior (in `jazz-next`)

1. Replace current analyzer-only structural checks with real type inference/checking slices.
2. Enforce primitive typing and equality contract (`16`).
3. Add type representation for parsed signatures (currently signature text is raw `Text`).
4. Establish error code taxonomy for type errors (parallel to existing `E1001+` binding diagnostics).

### Phase D: Build interpreter runtime core

1. Introduce runtime value model (ints, bools, strings, lists, functions, constructors).
2. Implement evaluator for literals, variables, lexical scope, let bindings, function application.
3. Implement runtime primitive behavior per `docs/spec/runtime/primitive-semantics.md`.
4. Replace placeholder codegen output with interpreter execution path in driver/CLI.

### Phase E: Core language semantics required for "fully working"

1. ADT declarations, constructor values, and pattern matching runtime (`11`).
2. `case` execution semantics and diagnostics.
3. Purity/effect stub enforcement for `!` (`03`).
4. Module/import resolution and loader semantics (`09`).
5. Stdlib ownership boundary and prelude strategy (`10`).

### Phase F: Product hardening and closure

1. CLI should compile/execute real user source (not sample AST only).
2. Add integration test suites that run end-to-end programs through interpreter path.
3. Align README + feature matrix to interpreter-backed reality.
4. Close plan trackers with commit-linked completion evidence.

## 5) Plan Holes and Missing Considerations

### Hole 1: No `jazz-next` native interpreter implementation plan

The main interpreter plan (`12a`) is written for `jazz-hs`, but active execution is now `jazz-next`.

**Status (2026-03-18):** addressed by `docs/plans/2026-03-18-jazz-next-runtime-architecture-and-interpreter-execution-plan.md`.

### Hole 2: Missing architecture doc for `jazz-next` runtime pipeline

There is no explicit runtime architecture for parser -> lowering -> analyzer/typecheck -> interpreter eval in `jazz-next`.

**Status (2026-03-18):** addressed by `docs/plans/2026-03-18-jazz-next-runtime-architecture-and-interpreter-execution-plan.md`.

### Hole 3: Type system plan mismatch with implementation reality

Current `jazz-next` "TypeInference" module forwards analyzer checks; it does not implement real inference.

**Needed:** explicit phased HM-style (or chosen model) implementation plan in `jazz-next`.

### Hole 4: Signature representation is under-specified

Signatures are currently stored as raw text in AST (`Text`), which blocks robust type checking.

**Status (2026-03-18):** active-path replacement planning is now captured in `docs/plans/2026-03-18-jazz-next-type-grammar-and-signature-rebase-plan.md`.

### Hole 5: Runtime diagnostics contract not fully planned

Spec defines primitive/runtime diagnostics expectations, but current implementation uses plain strings and no runtime error framework.

**Needed:** diagnostic model for runtime failures (codes, spans, formatting, fatal behavior).

### Hole 6: CLI product gap

Current CLI runs a hardcoded sample program. It is not yet a real user-facing compiler/interpreter entrypoint.

**Needed:** file/module input pipeline plan with deterministic exit codes and diagnostics.

### Hole 7: Module loader plan is still legacy-implementation bound

Plan `09` is detailed but still structured around `jazz-hs` internals.

**Needed:** a re-authored version targeting `jazz-next` modules and test harness.

### Hole 8: Feature-status narrative can drift from active implementation

Some docs still blend legacy behavior evidence with active target claims.

**Needed:** tighten matrix language to clearly separate:
- active `jazz-next` implemented behavior,
- locked spec contracts,
- legacy `jazz-hs` evidence only.

### Hole 9: No single critical-path dependency map

Plans exist, but there is no consolidated dependency DAG for execution order.

**Status (2026-03-18):** addressed by `docs/plans/2026-03-18-jazz-next-runtime-architecture-and-interpreter-execution-plan.md`.

### Hole 10: Test strategy gap for future domains

There are strong tests for current warning/binding/parser foundation slices, but no committed harness for upcoming domains (`if`, operators, primitives, interpreter runtime, module loader).

**Needed:** agreed test matrix templates and CI gating for each domain before implementation starts.

## 6) Recommended Next Batch (Practical Order)

1. Rebase `11` (ADT/pattern) against the new runtime pipeline and dependency map.
2. Rebase `09` (modules) onto `ModuleResolver.hs` plus `Driver.hs`.
3. Finish stdlib phase-5 hardcoded-kernel reduction (`10`).
4. Execute Milestone 1 of the new active-path type-grammar plan so signature ownership moves out of raw `Text`.
5. Close the compile vs run contract gap and remove placeholder-only success semantics from the active CLI/runtime path.

## Success Criteria for "Fully Working Interpreter"
Jazz should be considered a fully working interpreter only when all are true:

1. `jazz-next` executes user programs end-to-end with interpreter semantics.
2. Core language contracts (`if`, operators, primitives, bindings/signatures, ADT/patterns) are implemented and test-backed.
3. Module/import loading is deterministic and documented.
4. Standard library boundary is explicit and implemented.
5. CLI is production-usable for real source inputs.
6. README, feature matrix, and canonical specs all agree with observed behavior.
