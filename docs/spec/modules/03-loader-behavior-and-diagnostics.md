# Module Loader Behavior and Diagnostics

Status: active module loader contract
Primary plan: `docs/plans/spec-clarification/2026-03-02/modules/09-module-loader-and-import-resolution.md`
Depends on:

- `docs/spec/modules/01-file-layout-and-package-roots.md`
- `docs/spec/modules/02-resolution-algorithm-and-cycles.md`

## Scope

This document defines the v1 loader pipeline for active `jazz-next` standalone and module-graph compile/run entrypoints. It documents existing driver and CLI behavior without introducing new compiler behavior.

Qualified import binding details, future package metadata, persistent cache design, and migration policy remain separate module spec slices.

## Entrypoints

The loader has two source-selection modes.

Standalone mode compiles or runs one source blob:

- driver compile: `compileSource`, `compileSourceWithPrelude`, `compileSourceWithResolvedPrelude`;
- driver run: `runSource`, `runSourceWithPrelude`, `runSourceWithResolvedPrelude`;
- CLI compile/run without `--entry-module`, using stdin by default or one positional source file.

Module-graph mode compiles or runs an entry module plus its dependencies:

- driver compile: `compileModuleGraph`, `compileModuleGraphWithPrelude`, `compileModuleGraphWithResolvedPrelude`;
- driver run: `runModuleGraph`, `runModuleGraphWithPrelude`, `runModuleGraphWithResolvedPrelude`;
- CLI compile/run with `--entry-module <A::B>` and optional repeated `--module-root <path>` flags.

CLI source selection is exclusive:

| CLI shape | behavior |
| --- | --- |
| no source file and no `--entry-module` | read stdin as standalone source |
| one positional source file | read that file as standalone source |
| `--entry-module <A::B>` | ignore stdin and load module graph sources |
| source file plus `--entry-module` | option diagnostic |
| `--module-root` without `--entry-module` | option diagnostic |

When module-graph mode has no explicit module roots, CLI configuration supplies `.` as the default module root.

## Prelude Selection

Prelude selection happens before user source parsing or module graph loading:

1. `--no-prelude` selects `PreludeAbsent`.
2. `--prelude <path>` selects an explicit prelude source and fails if the file cannot be read.
3. `JAZZ_PRELUDE` selects an explicit prelude source when no CLI prelude flag is present.
4. Otherwise the bundled prelude source is loaded by default.

`--prelude` and `--no-prelude` are mutually exclusive.

Explicit and bundled prelude sources are parsed and lowered before module graph resolution. In standalone source mode, user source parse failure (`E0001`) takes precedence over prelude parse and bridge validation. Otherwise, prelude parse failures are `E0002`.

No-prelude mode exposes only kernel bridge names such as `__kernel_map`; public prelude aliases such as `map` require an actual prelude source.

## Standalone Pipeline

Standalone compile/run uses this order:

1. Resolve the prelude source.
2. Parse and lower the standalone source.
3. Parse, validate, and lower the selected prelude when present.
4. Merge prelude statements before user statements. Bundled prelude statements are hidden from user-facing warning spans.
5. Run analyzer/type inference and warning collection.
6. Promote warnings according to warning settings.
7. For run mode only, evaluate the canonicalized expression if compile diagnostics are empty.

Standalone parse failures from user source are `E0001`.

Successful compile mode is diagnostics-only. Driver compile results contain warnings and compile errors; CLI compile mode writes no stdout on success.

## Module Graph Pipeline

Module-graph compile/run uses this order:

1. Resolve the prelude source.
2. Parse, validate, and lower the selected prelude when present.
3. Collect visible prelude binding names as ambient symbols for import validation.
4. Resolve the entry module graph using the file-layout and resolution rules.
5. Replay resolved source files in dependency-first order from the memoized source lookup.
6. Parse and lower each resolved module source.
7. Build a validation replay program that contains dependency and entry module statements.
8. Merge prelude statements before the validation replay program.
9. Run analyzer/type inference and warning collection on the validation replay program.
10. Promote warnings according to warning settings.
11. For run mode only, build and analyze a runtime replay program, then evaluate it if compile diagnostics are empty.

Module graph source parse failures are `E4004` and include the resolved source path. Resolution failures use the `E4001` through `E4009` module diagnostic family defined by the resolution and import-binding specs.

## Runtime Replay

Compile mode validates the whole module graph. Dependency module expression statements are kept in the validation replay program so semantic errors in dependency expressions are reported.

Run mode uses a separate runtime replay program:

- dependency module expression statements are removed;
- entry module expression statements are retained;
- dependency bindings and data constructors needed by visible or qualified imports remain available;
- module declarations are removed before analysis/runtime replay.

This preserves dependency expression isolation: dependency expressions are checked for compile-time validity but do not produce runtime output while running an entry module.

## Cache Policy

V1 has no persistent module cache and no cross-invocation invalidation policy.

Within one compile/run invocation, the loader memoizes source lookup results by candidate path. The first lookup result for a path is reused during resolution and replay, including missing-source results. This keeps diagnostics deterministic and prevents a source file from being read once for resolution and then changing before replay.

Future persistent caching must not change the observable diagnostics, dependency order, prelude selection, or dependency expression isolation specified here.

## Diagnostics and Output

Driver results are structured:

- `CompileResult` contains warnings and compile errors.
- `RunResult` contains warnings, compile errors, runtime errors, and optional runtime output.

CLI output is stable:

| mode | success stdout | diagnostics | success exit | diagnostic exit |
| --- | --- | --- | --- | --- |
| standalone compile | empty | warnings/errors on stderr | `0` | `1` |
| module graph compile | empty | warnings/errors on stderr | `0` | `1` |
| standalone run | rendered value plus newline when a value exists | warnings/errors on stderr | `0` | `1` |
| module graph run | rendered entry-module value plus newline when a value exists | warnings/errors on stderr | `0` | `1` |
| option/source/prelude load failure | empty | error on stderr | n/a | `2` |

Compile diagnostics suppress runtime evaluation and runtime stdout. Runtime diagnostics suppress runtime stdout.

Module diagnostics should preserve the context from earlier spec slices:

- unresolved and ambiguous module diagnostics include checked candidates and importer context when present;
- cycles include a minimal cycle trace;
- module parse diagnostics include the source path;
- import-binding diagnostics include importer/imported-module context.

## Truth Table

| case | result |
| --- | --- |
| CLI `--entry-module App::Main --module-root src` | module graph mode with root `src` |
| CLI `--entry-module App::Main` | module graph mode with root `.` |
| CLI source file plus `--entry-module` | option diagnostic, exit `2` |
| CLI `--module-root src` without `--entry-module` | option diagnostic, exit `2` |
| explicit prelude path is missing | `E0003`, exit `2` |
| explicit or bundled prelude does not parse | `E0002`, compile diagnostic exit `1` |
| module source does not parse | `E4004`, compile diagnostic exit `1` |
| module graph compile succeeds | empty stdout, exit `0` |
| module graph run succeeds | entry-module runtime output on stdout, exit `0` |
| dependency module expression would fail at runtime only | no runtime output from dependency expression |
| dependency module expression is semantically invalid | compile diagnostic before runtime |
| same file is requested during resolution and replay | first source lookup result is reused |

Implementation evidence (2026-05-30): `LoaderSpec.hs` and `CLISpec.hs` now
lock the active `jazz-next` harness for module-graph default roots, exclusive
CLI source selection, dependency expression validation before run-mode runtime
evaluation, dependency expression runtime isolation, memoized source lookup
reuse, ambiguous/resolver diagnostics, fail-fast module source parse diagnostics,
and stable compile/run stdout suppression for diagnostics.

## Non-Goals

This loader slice does not define:

- exact qualified import shadowing and collision semantics beyond references needed for the loader pipeline;
- future package manifests or automatic root discovery;
- persistent cache storage, cache keys, or invalidation rules;
- JavaScript code generation or non-interpreter product backends;
- migration/deprecation policy for historical module/import syntax.
