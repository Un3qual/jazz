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
| Module declaration shape | Parser accepts one top-level brace-bodied declaration such as `module App::Main { ... }`; dot-only module declarations and nested module declarations are rejected. | Keep brace-bodied modules. Keep module declarations top-level only. | Normatively document whether a file may omit the declaration when its path supplies the module name. | `JN-MODULE-FILE-LAYOUT-SPEC-001` |
| File layout | Resolver maps `App::Main` to `App/Main.jz` under configured module roots and `.jz` extension. | Preserve deterministic path mapping and repo-relative source roots. | Define canonical package-root semantics, extension policy, case sensitivity, and declaration/path mismatch behavior. | `JN-MODULE-FILE-LAYOUT-SPEC-001` |
| Resolution order | Resolver deduplicates roots, finds candidates, reports unresolved imports as `E4001`, ambiguous matches as `E4002`, and cycles as `E4003`. Imports are traversed in deterministic rendered-path order. | Keep deterministic graph order and deterministic diagnostics. | Publish normative resolver pseudocode and cycle/ambiguity examples. | Future `JN-MODULE-RESOLUTION-SPEC-*` row after file layout. |
| Loader pipeline | `compileModuleGraph` and `runModuleGraph` build a resolved module graph, replay dependency modules before the entry module, and load the bundled prelude by default unless no-prelude APIs are used. | Keep dependency expression isolation: dependency expression statements are validated but only the entry module produces runtime output. | Define no-cache v1 behavior, cache non-goals, diagnostic import-chain context, and compile/run entrypoint wording. | Future loader-pipeline spec row. |
| Qualified imports | Bare imports expose exports unqualified. Symbol-list imports expose only listed exports. Alias imports expose no unqualified exports and support `Alias::symbol` lookup. Parser rejects alias plus symbol-list combinations. | Preserve explicit import visibility and alias-only isolation. | Publish normative binding/shadowing rules for local names, imports, aliases, and duplicate imports. | Future qualified-import spec row. |
| Standard library loading | Bundled prelude helpers are loaded by default in module-graph driver helpers and mirrored by `jazz-next/stdlib/Prelude.jz`. | Keep public prelude helpers separated from `__kernel_*` bridge names. | Tie module specs to the stdlib boundary without expanding the stdlib in this module batch. | Future stdlib closure row if needed. |
| Migration compatibility | README examples already use brace-bodied module declarations. Legacy dot-only module declaration syntax is rejected. | Keep brace-bodied modules as the migration target. | Document migration examples and invalid legacy examples alongside file-layout rules. | `JN-MODULE-FILE-LAYOUT-SPEC-001` |

## Open Decisions

The next executor does not need a broad docs scan. It should define only the file layout and package-root contract, using this matrix and the active `jazz-next` resolver behavior as evidence.

Questions intentionally deferred past the file-layout batch:

- Whether future package metadata changes module-root discovery.
- Whether future cache behavior exists; current v1 can remain no-cache.
- Whether additional qualified-import forms are introduced.

