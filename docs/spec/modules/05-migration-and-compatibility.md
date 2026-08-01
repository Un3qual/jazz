# Module Migration and Compatibility

Status: active module migration contract
Primary plan: `docs/plans/spec-clarification/2026-03-02/modules/09-module-loader-and-import-resolution.md`
Depends on:

- `docs/spec/modules/01-file-layout-and-package-roots.md`
- `docs/spec/modules/02-resolution-algorithm-and-cycles.md`
- `docs/spec/modules/03-loader-behavior-and-diagnostics.md`
- `docs/spec/modules/04-qualified-imports-and-binding.md`

## Scope

This document defines the compatibility policy for the active `Jazz` module/import path. It documents existing parser, CLI, resolver, and driver behavior without introducing new compiler behavior.

Legacy `jazz-hs/` and `jazz2/` behavior is historical evidence only. New module/import behavior belongs in the repository root.

## Compatibility Baseline

Existing standalone source behavior is preserved:

- CLI compile reads stdin by default or one positional source file.
- CLI `--run` uses the same source selection and then evaluates the program.
- Successful compile mode is diagnostic-only and writes no stdout.
- Standalone source mode does not use module roots or resolve dependency files.

Module-graph behavior is explicit:

- CLI module graph mode requires `--entry-module <A::B>`.
- Repeated `--module-root <path>` flags are allowed only with `--entry-module`.
- A positional source file cannot be combined with `--entry-module`.
- When no module root is provided, module graph mode uses `.` as the root.

This preserves the existing single-file workflow while allowing multi-file programs to opt into the module loader.

## Accepted Migration Target

The canonical migration target is one module per `.jz` source file under an ordered module root:

```text
src/App/Main.jz      -> App::Main
src/Lib/Math.jz      -> Lib::Math
```

A source file may omit a module declaration. If it contains a module declaration, exactly one top-level brace-bodied declaration is allowed and it must match the resolved path:

```jazz
module App::Main {
  import Lib::Math.
  main = add 1 2.
}
```

Import forms should use the v1 import contract:

```jazz
import Lib::Math.
import Lib::Math (add).
import Lib::Math as Math.
```

## Rejected Legacy and Non-V1 Forms

The active migration policy is fail-fast, not compatibility-mode rewriting.

Rejected forms include:

- dot-only module declarations such as `module App::Main.`;
- nested module declarations;
- module declarations after earlier top-level statements;
- top-level statements after a brace-bodied module declaration;
- empty import symbol lists;
- duplicate symbols inside one import symbol list;
- alias plus symbol-list imports in either order;
- module graph CLI invocations that combine source files with `--entry-module`;
- `--module-root` without `--entry-module`.

Rejected syntax remains a parser or CLI error. It is not accepted with a deprecated-syntax warning in v1.

## Deprecation Policy

There is no active deprecation window for historical module/import forms because the active parser does not accept those forms as legacy-compatible syntax.

The reserved `deprecated-syntax` warning category must not be used for module/import migration until a future spec identifies an accepted form that should warn instead of fail.

Future compatibility modes require a new decision record or queue item that defines:

- accepted old and new forms;
- exact warning or error diagnostics;
- removal gates;
- focused parser/resolver/CLI verification.

## Deterministic Failure Modes

Migration failures should use existing deterministic diagnostics:

| case | failure mode |
| --- | --- |
| invalid standalone source syntax | `E0001` parse diagnostic |
| invalid module source syntax during graph loading | `E4004` module parse diagnostic |
| module path has no source under roots | `E4001` |
| module path matches more than one root candidate | `E4002` |
| module graph contains an import cycle | `E4003` |
| multiple module declarations in one source | `E4005` |
| module declaration does not match resolved path | `E4006` |
| requested symbol-list export is missing | `E4007` |
| symbol-list imports collide | `E4008` |
| alias imports collide | `E4009` |
| hidden explicit-import export is used unqualified | `E4011` |
| alias-only export is used unqualified | `E4012` |
| qualified alias is undeclared | `E4013` |
| qualified alias member is not exported | `E4014` |
| invalid or empty entry module path | `E4016` |
| invalid CLI source/module-root combination | `E5002`, exit `2` |

Diagnostics should not silently fall back to standalone source mode once `--entry-module` is selected.

## Migration Safety Checks

Implementation batches that change module/import behavior must preserve these checks:

- Single-file compile/run remains usable without module roots.
- Module graph mode stays opt-in through explicit entry-module selection.
- Source file paths are not inferred as module paths unless a future spec says so.
- Module-root order stays deterministic.
- Dependency expression statements are validated but do not produce runtime output for the entry module.
- Prelude selection remains independent of module-root lookup.
- Legacy directories remain read-only reference material.

## Phase 6 Follow-Up Boundary

The historical plan's Phase 6 verification-harness checklist has been rewritten
as active `Jazz` child implementation batches:

- `JN-MODULE-FILE-LAYOUT-HARNESS-001`
- `JN-MODULE-RESOLUTION-BINDING-HARNESS-001`
- `JN-MODULE-LOADER-MIGRATION-HARNESS-001`

Those rows name concrete `Jazz` parser, resolver, loader, CLI, and
default-suite targets plus focused verification commands. Legacy `jazz-hs/` and
`jazz2/` paths remain read-only reference material and are not valid targets for
new module verification closure.

Status update (2026-05-30): `JN-MODULE-FILE-LAYOUT-HARNESS-001` is complete.
The active parser/resolver harness now covers the file-layout and rejected-form
boundary for this migration policy. `JN-MODULE-RESOLUTION-BINDING-HARNESS-001`
is also complete; the active resolver harness now covers deterministic
resolution ordering, duplicate import collapse, shared dependency reuse, cycle
traces, and the qualified import diagnostic truth table.
`JN-MODULE-LOADER-MIGRATION-HARNESS-001` is complete as well; the active loader
and CLI harness now covers exclusive source selection, default module roots,
dependency expression validation versus runtime isolation, memoized source
lookup reuse, and fail-fast migration diagnostics. No active Phase 6 harness row
remains in `Ready Now`.

## Non-Goals

This migration slice does not define:

- automatic package discovery;
- compatibility mode for dot-only module declarations;
- warning-based migration for rejected module/import syntax;
- re-export or package-level migration behavior;
- any implementation change in `jazz-hs/` or `jazz2/`.
