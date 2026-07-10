# Module Clarification Matrix

Status: active clarification matrix
Primary plan: `docs/plans/spec-clarification/2026-03-02/modules/09-module-loader-and-import-resolution.md`

## Purpose

Capture the active `jazz-next` module/import baseline and separate locked behavior from follow-up specification work. This matrix is not a product implementation batch.

## Baseline Scope

- Active implementation path: `jazz-next/`.
- Legacy `jazz-hs/` and `jazz2/` module behavior is evidence only.
- Brace-bodied modules remain the canonical/compatible module declaration form.

## Clarification Matrix

| Area | Active `jazz-next` baseline | Invariant to preserve | Remaining spec work | Next safe batch |
| --- | --- | --- | --- | --- |
| Module declaration shape | Parser accepts one top-level brace-bodied declaration such as `module App::Main { ... }`; dot-only module declarations and nested module declarations are rejected. | Keep brace-bodied modules. Keep module declarations top-level only. | File-layout spec now states declarations may be omitted, but if present exactly one declaration must match the resolved module path. | Future resolution/loader specs can reference `docs/spec/modules/01-file-layout-and-package-roots.md`. |
| File layout | Resolver maps `App::Main` to `App/Main.jz` under configured module roots and `.jz` extension. | Preserve deterministic path mapping and repo-relative source roots. | Canonical package-root semantics, extension policy, case sensitivity, and declaration/path mismatch behavior are specified in `docs/spec/modules/01-file-layout-and-package-roots.md`. | `JN-MODULE-RESOLUTION-SPEC-001` |
| Resolution order | Resolver deduplicates roots, finds candidates, reports unresolved imports as `E4001`, ambiguous matches as `E4002`, and cycles as `E4003`. Imports are traversed in deterministic rendered-path order. | Keep deterministic graph order and deterministic diagnostics. | Resolver pseudocode, ambiguity policy, unresolved import diagnostics, cycle traces, and truth-table examples are specified in `docs/spec/modules/02-resolution-algorithm-and-cycles.md`. | `JN-MODULE-LOADER-PIPELINE-SPEC-001` |
| Loader pipeline | `compileModuleGraph` and `runModuleGraph` build a resolved module graph, replay dependency modules before the entry module, and load the bundled prelude by default unless no-prelude APIs are used. | Keep dependency expression isolation: dependency expression statements are validated but only the entry module produces runtime output. | Standalone/module-graph entrypoints, prelude selection, parse/resolve/analyze/run ordering, no-persistent-cache v1 policy, per-invocation source memoization, diagnostics, and output behavior are specified in `docs/spec/modules/03-loader-behavior-and-diagnostics.md`. | `JN-MODULE-QUALIFIED-IMPORT-SPEC-001` |
| Qualified imports | Bare imports expose exports unqualified. Symbol-list imports expose only listed exports. Alias imports expose no unqualified exports and support `Alias::symbol` lookup. Parser rejects alias plus symbol-list combinations. | Preserve explicit import visibility and alias-only isolation. | Import forms, export inventory, local/import shadowing, alias namespace behavior, and binding diagnostics are specified in `docs/spec/modules/04-qualified-imports-and-binding.md`. | `JN-MODULE-MIGRATION-SPEC-001` |
| Explicit exports | Module headers accept optional allowlists such as `module Lib::Maybe (Maybe, Just, mapMaybe) { ... }`; omission preserves export-all and `()` exports nothing. | Keep unlisted owned declarations local while resolver dependencies, compiler imports, and runtime publication consume only the validated public typed inventory. | Syntax, namespace selection, ownership, `E4015`, and no-re-export behavior are specified in `docs/spec/modules/06-explicit-export-lists.md`. | Future module behavior requires a separate accepted contract. |
| Standard library loading | Bundled prelude helpers are loaded by default in module-graph driver helpers and mirrored by `jazz-next/stdlib/Prelude.jz`. | Keep public prelude helpers separated from `__kernel_*` bridge names. | Tie module specs to the stdlib boundary without expanding the stdlib in this module batch. | Future stdlib closure row if needed. |
| Migration compatibility | README examples already use brace-bodied module declarations. Legacy dot-only module declaration syntax is rejected. | Keep brace-bodied modules as the migration target and preserve explicit module-graph opt-in. | Compatibility policy, rejected legacy forms, deterministic failure modes, and the legacy Phase 6 verification boundary are specified in `docs/spec/modules/05-migration-and-compatibility.md`. | Blocked until an active `jazz-next` verification-harness closure contract replaces the stale legacy Phase 6 text. |

## Open Decisions

The next executor does not need a broad docs scan. Normative module spec slices 01 through 05 are now published. Any further module-spec closure should first replace the stale legacy Phase 6 verification checklist with an active `jazz-next` verification-harness contract.

Questions intentionally deferred past the file-layout batch:

- Whether future package metadata changes module-root discovery.
- Whether future cache behavior exists; current v1 can remain no-cache.
- Whether additional qualified-import forms are introduced.
