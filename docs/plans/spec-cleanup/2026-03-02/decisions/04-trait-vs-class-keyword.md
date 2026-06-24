# Jazz Spec-Cleanup #4 Trait/Class Keyword Closure

> **Legacy closure note (2026-06-24):** this plan is now reference-only. The
> original March 2026 cleanup path targeted `jazz-hs` parser, prelude, and test
> edits, but `jazz-hs/` is a read-only legacy reference under the current
> workspace policy. New abstraction syntax, parser, analyzer, runtime, and
> stdlib work belongs in `jazz-next/`.

**Closed decision:** active Jazz syntax uses canonical `class` and `impl`
declarations. Declaration-shaped `trait` syntax is permanently rejected in
active `jazz-next`; it must not be accepted as a compatibility alias, surfaced
through a deprecation warning, or mapped to W0004.

**Current authority:** `docs/spec/authoritative-syntax.md` is the normative
syntax contract for this area. Future abstraction semantics must use active
`jazz-next` class/impl contracts such as
`docs/plans/spec-cleanup/2026-03-02/decisions/01-authoritative-syntax.md` and
the relevant abstraction blocker rows in `docs/execution/queue.md`.

---

## Closure Status

- [x] Canonical active abstraction declaration keywords are `class` and `impl`.
- [x] `trait` declarations are non-canonical and permanently rejected in active
  `jazz-next`.
- [x] No active compatibility parsing path exists for `trait`.
- [x] No deprecation-warning or W0004 path exists for rejected `trait`
  declarations.
- [x] Ordinary uses of `trait` as a binding, signature, or qualified-alias
  identifier remain governed by the active parser rules documented in
  `docs/spec/authoritative-syntax.md`.
- [x] The old legacy `jazz-hs` cleanup tasks below are superseded and must not
  be executed as active work.

## Active Evidence

- `docs/spec/authoritative-syntax.md` states that `trait` is never accepted as
  an active compatibility alias and that active abstraction declarations use
  canonical `class` and `impl`.
- `docs/jazz-language-state.md` records the active `jazz-next` class/impl
  subset, including permanent non-canonical `trait` declaration rejection.
- `jazz-next/test/JazzNext/Compiler/Parser/ParserFoundationSpec.hs` verifies
  top-level and module-body `trait` declaration rejection with unsupported
  abstraction syntax diagnostics.
- `docs/execution/done-archive.md` records `JN-TRAIT-SYNTAX-REJECT-001`, the
  active parser rejection batch for non-canonical `trait` declarations.

## Historical Decision Record

The March 2026 cleanup selected `class` as the canonical abstraction keyword
because the legacy parser already recognized `class` declarations and the
planned direction favored typeclass vocabulary. At that time, the plan still
described a staged migration path where `trait` would be accepted temporarily
with deprecation messaging.

That migration path is superseded. The active `jazz-next` language contract
chose the stricter boundary: declaration-shaped `trait` syntax is rejected
instead of accepted and warned on. This keeps the active parser behavior,
warning catalog, and abstraction roadmap aligned without creating a legacy
compatibility mode.

## Historical Evidence Preserved

- `docs/jazz-language-state.md` previously recorded a mismatch between legacy
  `jazz-hs` parser support for `class` and legacy `Prelude.jz` examples using
  `trait`.
- `jazz-hs/src/Parser/Lang.hs` historically parsed `class` declarations.
- `jazz-hs/static/Prelude.jz` historically included some `trait` examples.
- Those legacy paths remain useful as evidence of past inconsistency only; they
  are not active implementation targets.

## Superseded Legacy Task List

The following original plan tasks are closed as superseded:

- [x] Establish Nix-based execution environment and baseline test status.
  Superseded because the plan no longer sends work to `jazz-hs`.
- [x] Run decision gate and record canonical keyword plus rationale. Closed by
  the active `class`/`impl` syntax contract.
- [x] Implement compatibility parsing/deprecation messaging for non-canonical
  keyword. Superseded by permanent active `trait` declaration rejection.
- [x] Normalize docs/examples to canonical keyword. Active normative docs now
  use `class`/`impl`; legacy references may still quote historical `trait`
  evidence when clearly labeled.
- [x] Validate parser/tests/docs end-to-end and finalize item #4 as resolved.
  Active validation belongs to `jazz-next` parser tests and repo docs/queue
  checks.

## Non-Goals

- Editing `jazz-hs/` or `jazz2/`.
- Adding parser compatibility for `trait`.
- Adding a `trait` deprecation warning or W0004 warning emitter.
- Reopening abstraction semantics such as dictionaries, default methods,
  superclasses, inferred constraints, method imports/exports, or generalized
  runtime evidence.

## Verification For Closure

- `bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Parser/ParserFoundationSpec.hs`
- `bash scripts/check-execution-queue.sh`
- `bash scripts/check-docs.sh`
- `git diff --check`
