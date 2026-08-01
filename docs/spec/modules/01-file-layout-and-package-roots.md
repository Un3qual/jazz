# Module File Layout and Package Roots

Status: active module layout contract

## Scope

This document defines the v1 source layout contract for active `Jazz` module-graph compilation and execution. It documents existing resolver behavior and does not introduce new compiler behavior.

## Module Paths

A module path is an ordered list of identifier segments rendered with `::`, for example `App::Main` or `Lib::Math`.

CLI entry-module paths must be non-empty. Each segment must start with an alphabetic character or `_`; remaining characters may be alphabetic characters, digits, `_`, `'`, or `!`. Empty segments such as `App::::Main` are invalid before resolution begins.

Module path segment text is preserved exactly. The resolver does not normalize case, rewrite separators, canonicalize Unicode, or collapse equivalent-looking names. A source that imports `Lib::Util` and a source that imports `lib::Util` ask for different files on case-sensitive filesystems, and portable code should treat them as distinct module paths on every platform.

## File Mapping

The canonical relative path for a module is its path segments joined by `/` with the `.jz` extension appended:

| module path | relative file |
| --- | --- |
| `App::Main` | `App/Main.jz` |
| `App::Core::Parser` | `App/Core/Parser.jz` |
| `Lib::Math` | `Lib/Math.jz` |

There is no v1 index-file convention, directory module convention, extension search list, or implicit package manifest lookup. The CLI module-graph path uses `.jz`; future source extensions require an explicit spec and resolver contract update before becoming portable source layout.

## Module Roots

A module root is a source-root directory searched for module-relative files. For each requested module, the resolver constructs one candidate path per root:

```text
<module-root>/<module-relative-file>
```

The CLI accepts repeated `--module-root <path>` flags when `--entry-module <A::B>` is present. If no module root is supplied, CLI module-graph mode uses `.` as the sole root. Driver and test helpers may pass an explicit `ModuleResolutionConfig`; the same ordered-root semantics apply.

Candidate paths are lexically normalized before deduplication and lookup.
Roots such as `src`, `src/.`, and paths with reducible `..` components therefore
refer to one candidate. The pure resolver does not resolve symlinks or compare
filesystem identities; physically equivalent symlink roots remain distinct.

Duplicate normalized roots are deduplicated before lookup while preserving the first occurrence. Root order is semantically meaningful:

- no matching candidate is `E4001`;
- one matching candidate is selected;
- more than one matching candidate across distinct roots is `E4002`;
- diagnostics render the candidate paths in deterministic root order.

Module roots are source roots, not package declarations. They do not imply package names, dependency versions, visibility boundaries, or automatic standard-library discovery.

## Module Declarations

A module file may omit a `module ... { ... }` declaration. In that case, the resolved path supplies the module identity.

If a module declaration is present, it must be a single top-level brace-bodied declaration whose path exactly matches the resolved module path. For example, `App/Main.jz` resolved as `App::Main` may contain:

```jz
module App::Main {
  import Lib::Math as Math.
  result = Math::answer.
}
```

Multiple module declarations in one resolved source are `E4005`. A declaration that does not match the resolved module path is `E4006`.

Nested module declarations and legacy dot-only module declarations are invalid parser surfaces. Brace-bodied declarations remain the compatibility target because they are already used in public examples and active parser tests.

Implementation evidence (2026-05-30): `ModuleImportParserSpec.hs` and
`ModuleResolutionSpec.hs` now lock the active `Jazz` harness for
brace-bodied module boundaries, rejected legacy declaration shapes, exact path
segment preservation, canonical nested `.jz` mapping, declaration omission,
declaration/path match and mismatch, duplicate-root dedupe, and deterministic
candidate ordering.

## Single-File Compatibility

Standalone compile/run mode remains a single source input and does not use module roots. CLI source-file mode cannot be combined with `--entry-module`.

Module graph mode is entered explicitly through `--entry-module`; it resolves the entry module and imports through the module-root search contract above.

Each selected module source is parsed and lowered once during resolution. The
resulting core module retains its resolved source path and structured module
identity for later per-module compilation and evaluation; file layout does not
depend on generated wrapper sources or a second source read.

## Non-Goals

The v1 file-layout contract does not define:

- package manifests or package-name namespaces;
- automatic root discovery beyond the CLI default of `.`;
- alternate source extensions;
- generated source lookup;
- filesystem cache semantics;
- qualified import binding rules beyond the file lookup needed to load modules.

Those behaviors require separate normative module specs before implementation changes.
