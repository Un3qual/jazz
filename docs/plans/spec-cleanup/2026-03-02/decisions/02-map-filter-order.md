# Jazz Spec-Cleanup Item #2: `map`/`filter` Argument Order Closure

> **Reference-only closure:** This legacy cleanup plan is closed for active
> execution. Do not execute its old `jazz-hs` implementation phases as new work.

**Goal:** Preserve the historical `map`/`filter` argument-order decision while
making the active `jazz-next` policy unambiguous.

**Architecture:** Active Jazz uses function-first collection combinators:
`map f xs` and `filter p xs`. Historical collection-first examples are
non-canonical and are not compatibility syntax. Future changes to collection
primitive semantics must start from active `jazz-next` primitive or stdlib
contracts, not this legacy cleanup plan.

**Tech Stack:** Markdown decision metadata, active `jazz-next` runtime/type
coverage, bundled-prelude aliases, and repo-root queue/docs validators.

---

## Completed coordination batch: map/filter compatibility closure

Completed on `2026-06-24`.

Executor-safe scope:

- Do not edit `jazz-hs/` or `jazz2/`.
- Do not add parser aliases for collection-first calls.
- Do not add a runtime adapter or dual-form behavior.
- Do not create a deprecated-syntax warning path for collection-first calls.
- Keep active examples, specs, and tests in function-first form.

Closure decision:

- Canonical `map` call form is `map f xs`.
- Canonical `filter` call form is `filter p xs`.
- Partial application remains function-first: `map f` and `filter p`.
- Compatibility policy is a hard switch. Active docs/tests must use canonical
  forms, and collection-first forms stay historical/non-canonical.
- No compiler implementation row is promoted from this cleanup item.

Evidence:

- `README.md` now uses function-first examples for both `map` and `filter`.
- `docs/spec/authoritative-syntax.md` records function-first collection
  combinator order and marks collection-first examples as historical.
- `docs/spec/runtime/primitive-semantics.md` and
  `docs/spec/stdlib-boundary.md` describe the active primitive/stdlib boundary
  with function-first collection combinators.
- Active `jazz-next` type/runtime coverage and bundled-prelude aliases support
  `map` and `filter` as function-first helpers.
- Searches over active docs/examples/tests found no active collection-first
  example that requires compatibility behavior.

Historical context:

- This cleanup item was created when older top-level README examples used
  collection-first calls while the legacy `jazz-hs` `map` behavior was already
  function-first and `filter` support was incomplete.
- A `2026-03-05` active `jazz-next` implementation batch verified that `map`
  already used function-first order and added function-first `filter` type and
  runtime support with tests.
- The old plan's `jazz-hs` touch-set, JavaScript codegen phases, and Nix
  command-wiring tasks are superseded under the current workspace policy.

## Non-Executable Historical Tasks

The old plan phases are retained only as historical evidence. In particular,
the following old work must not be promoted from this file:

- editing `jazz-hs/src/Types.hs` or `jazz-hs/src/CodeGen/Javascript.hs`,
- adding legacy `jazz-hs` parser/analyzer/codegen tests,
- adding dual-form parser normalization,
- adding warning/deprecation behavior for collection-first calls,
- adding a repo-level Nix workflow only for this closed legacy item.

## Active Follow-Up Routing

No map/filter compatibility work remains in the queue. If future collection
primitive work is needed, route it through the active primitive-surface,
stdlib-boundary, or runtime-product blocker with concrete `jazz-next` target
paths and focused verification.

## Verification

- [x] Search active docs/examples/tests for collection-first compatibility
  requirements.
- [x] `bash scripts/check-execution-queue.sh`
- [x] `bash scripts/check-docs.sh`
- [x] `git diff --check`
