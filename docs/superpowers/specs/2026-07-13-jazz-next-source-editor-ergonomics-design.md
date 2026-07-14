# Jazz-Next Source and Editor Ergonomics Design

## Status

Approved and implemented on `2026-07-13`.

This is the design checkpoint for Batch 2 of
[`docs/jazz-improvement-backlog.md`](../../jazz-improvement-backlog.md). It
covers items 10, 1, and 9 only: separating shipped Jazz-authored stdlib and
compiler sources, migrating authored code to the existing compact
multi-parameter lambda syntax, and adding syntax highlighting for `.jz` files.

## Decision Summary

All shipped Jazz-authored sources move under the locked root:

```text
jazz-next/jazz/
  stdlib/
  compiler/
```

`stdlib/` owns general user-facing modules. `compiler/` owns the hosted lexer,
its compiler-facing data types, and future Jazz-authored compiler stages.
Compiler modules may import stdlib modules; stdlib modules may not import
compiler modules. The old `jazz-next/stdlib/` directory is removed rather than
kept as a compatibility copy or symlink.

Authored multi-argument functions use the already-supported compact lambda
surface, for example `\(left, right) -> left == right`. Parsing and lowering
remain unchanged: compact parameters continue to lower to nested unary core
lambdas and preserve currying and partial application. Explicitly nested
lambdas remain in tests whose purpose is nested currying, one-argument-at-a-time
closure capture, or the distinction between compact and nested syntax.

Syntax highlighting ships as a dependency-free VS Code extension under
`jazz-next/editors/vscode-jazz/`. Its JSON TextMate grammar uses the portable
`source.jazz` scope, while the VS Code manifest registers `.jz` and a language
configuration. This supplies useful lexical highlighting without defining a
semantic model or introducing a language server.

## Goals

- Give every shipped `.jz` file an unambiguous product role and permanent
  location under `jazz-next/jazz/`.
- Make the stdlib-to-compiler dependency boundary visible in the filesystem and
  protected by repository tests.
- Package all shipped Jazz sources and editor assets in the `jazz-next` source
  distribution.
- Replace nested-lambda noise in authored production sources, generated
  bundled-prelude source, and ordinary test programs with the accepted compact
  surface.
- Preserve deliberate nested-lambda coverage for currying and partial
  application.
- Make `.jz` files recognizable and readable in VS Code and other editors that
  can consume TextMate grammars.
- Keep the grammar synchronized with the active `jazz-next` lexer/parser and
  exercise representative syntax through checked-in fixtures and validation
  tests.

## Non-Goals

- No new Jazz syntax, parser rule, core representation, type rule, runtime
  behavior, or LLVM-facing lowering belongs to this batch.
- No stdlib API is added or renamed. That remains Batch 5.
- No realistic program corpus, benchmark, runtime counter, cost centre, or
  profiling configuration is added. Those remain Batch 3.
- No grouped constructor-export syntax is introduced.
- No language server, formatter, semantic token provider, completion engine,
  diagnostics integration, or editor marketplace publication is included.
- Historical plans and closure evidence are not rewritten to pretend the new
  paths existed when those records were created.
- Nothing under `jazz-hs/` or `jazz2/` is modified.

## Current State

Ten shipped Jazz sources currently share `jazz-next/stdlib/`. Eight are general
library modules: `Char`, `IO`, `IOError`, `List`, `Maybe`, `Prelude`, `Result`,
and `Text`. `Lexer` and `LexerTypes` are hosted compiler modules but are mixed
into that same directory.

Several active consumers spell the old paths directly:

- `JazzNext.Compiler.BundledPrelude` names the checked-in Prelude mirror;
- loader, lexer-parity, canonical-comparison, CLI, and builtin-catalog tests
  locate sources from repository-root and package-root working directories;
- the repository audit enumerates `jazz-next/stdlib/`; and
- active architecture and language-state documentation links to the old root.

The source files and generated Prelude also contain many immediately nested
lambda chains. The compact multi-parameter surface is already parsed, lowered,
typed, and evaluated, so the migration needs no compiler behavior change.

There is no active editor grammar. `.jz` registration, lexical scopes, comment
configuration, bracket behavior, and representative highlighting fixtures are
all absent.

## Approaches Considered

### Keep a compatibility `stdlib/` directory

Copies, forwarding files, or a symlink would make old paths continue to work,
but would create two apparent source roots and undermine the purpose of the
move. It would also leave tests free to pass against the wrong copy. This is
rejected.

### Move files and update only the currently failing paths

This is smaller, but it does not protect source-distribution packaging,
dependency direction, or execution from different working directories. A
future path addition could drift again. This is rejected.

### Move once and make the new layout an audited package contract

This is the chosen source-layout approach. All active consumers move in one
change, Cabal includes the new trees in `sdist`, and repository tests validate
the directory roles and dependency direction.

For editor support, three levels were considered:

1. a bare `.tmLanguage` file, which is portable but does not register `.jz` in
   VS Code by itself;
2. a small VS Code extension containing a portable TextMate grammar, language
   registration, configuration, fixtures, and installation notes; and
3. a language server with semantic highlighting.

The second option is chosen. It fixes the immediate white-text problem without
coupling syntax highlighting to unfinished semantic/compiler services.

## Shipped Source Layout

### Standard library

The following files move to `jazz-next/jazz/stdlib/` without changing module
names or public APIs:

- `Char.jz`
- `IO.jz`
- `IOError.jz`
- `List.jz`
- `Maybe.jz`
- `Prelude.jz`
- `Result.jz`
- `Text.jz`

### Hosted compiler

The following files move to `jazz-next/jazz/compiler/`:

- `Lexer.jz`
- `LexerTypes.jz`

Their Jazz module names remain `Lexer` and `LexerTypes`. The filesystem role
does not add a `Compiler::` module prefix in this batch; changing public module
identity would be a separate module-design decision and would add no value to
the physical separation.

### Dependency rule

Repository validation derives module names from top-level `module`
declarations, derives imports from parsed source, and rejects any stdlib source
that imports a module owned by `jazz/compiler`. Compiler-to-stdlib imports are
allowed. The validation is based on the module graph rather than a fixed list
such as `Lexer` and `LexerTypes`, so future compiler modules inherit the rule.

The Prelude remains the only source exempt from the ordinary brace-bodied
module format because it is a compiler-generated bundled mirror rather than an
ordinary importable module.

### Path ownership and packaging

Test helpers resolve the active `jazz-next` package root first and then append
`jazz/stdlib` or `jazz/compiler`. They do not keep parallel repository-root and
package-root candidate lists for each file.

`jazz-next.cabal` lists the following as source-distribution assets:

```text
jazz/stdlib/*.jz
jazz/compiler/*.jz
editors/vscode-jazz/package.json
editors/vscode-jazz/language-configuration.json
editors/vscode-jazz/syntaxes/*.json
editors/vscode-jazz/fixtures/*.jz
editors/vscode-jazz/README.md
```

The repository audit checks that both source directories exist, contain `.jz`
files, satisfy their format rules, and do not leave a legacy
`jazz-next/stdlib/` directory behind.

## Compact Lambda Migration

### Canonical authored form

An immediately nested chain with only lambda boundaries between parameters is
rewritten as one compact parameter list:

```jazz
# Before
writeText! = \(path) -> \(contents) -> body.

# After
writeText! = \(path, contents) -> body.
```

Longer chains are handled the same way. Patterns remain valid compact
parameters, so tuple, constructor, list, wildcard, and as-pattern parameters
retain their existing meaning.

### Migration scope

The migration includes:

- every shipped source under `jazz-next/jazz/`;
- the generated checked-in Prelude source in
  `JazzNext.Compiler.BundledPrelude` so generation and mirror comparison stay
  identical;
- ordinary hand-authored Jazz programs embedded in Haskell tests; and
- editor fixtures and active documentation examples touched by this batch.

The migration excludes only examples whose asserted behavior depends on
explicitly nested lambdas, including currying one parameter at a time, closure
capture between lambda boundaries, or parser/lowering distinction coverage.
Those cases receive a short local comment or an already-specific test name so
the exception is evident.

No compatibility syntax, parser desugaring change, or warning is added. The
existing compact form is simply used consistently.

## Editor Package

### Package structure

```text
jazz-next/editors/vscode-jazz/
  README.md
  package.json
  language-configuration.json
  syntaxes/
    jazz.tmLanguage.json
  fixtures/
    representative.jz
```

`package.json` contributes language id `jazz`, aliases `Jazz` and `jazz`, the
`.jz` extension, the language configuration, and the `source.jazz` grammar. It
contains no runtime code and no npm dependencies.

The README gives two local-development installation paths: copy/symlink the
directory into the editor's extension directory, or package it later with
standard VS Code extension tooling. Packaging tooling itself is not added as a
repository dependency.

### Lexical scopes

The grammar follows the active lexer and contextual declaration parser:

- `#` line comments;
- single-quoted character and double-quoted text literals;
- valid simple escapes and `\u{1..6 hex digits}` Unicode escapes;
- decimal integers and supported numeric suffix spellings;
- control-flow and module keywords: `module`, `import`, `as`, `data`, `if`,
  `then`, `else`, and `case`;
- contextual declaration keywords: `class`, `impl`, `operator`, `tier`,
  `precedence`, `left`, `right`, and `nonassoc`;
- explicit export namespaces: `value`, `constructor`, `type`, and `class`;
- type and constructor identifiers beginning with an uppercase letter;
- signatures and constraints around `::`, `:`, and `@{...}`;
- lambda punctuation, arrows, pattern-arm bars, delimiters, and operator runs;
- bindings and purity-marked identifiers ending in `!`; and
- built-in scalar, numeric-width, collection, and Unit type names.

Regex ordering prevents comments from highlighting contained tokens and keeps
escape scopes nested inside string scopes. The grammar is intentionally
lexical: it may identify contextual keywords by spelling even where Jazz would
allow the same identifier as an ordinary binding.

### Language configuration

The language configuration declares `#` line comments and matching braces,
brackets, and parentheses. Auto-closing and surrounding pairs also cover
double-quoted text, but omit single quotes because apostrophe is a legal
identifier continuation and editor-side pairing cannot reliably distinguish
that use from a character literal. No indentation engine or formatter-like
rule is invented.

### Grammar validation

Repository tests decode every JSON file, assert the manifest-to-grammar paths
exist, confirm `.jz` and `source.jazz` registration, and check that the fixture
contains every required syntax family. A small grammar-tokenization smoke test
is preferable if a suitable already-resolved Haskell library is present; this
batch will not add a large Node dependency merely to test TextMate regexes.

The representative fixture is also parsed by the active Jazz parser wherever
the fixture uses executable syntax. If a highlighting-only malformed escape or
other error example is needed, it lives in a separately named invalid fixture
and is not presented as an executable program.

## Failure Handling

Source lookup failures report the resolved package-relative path and the source
role (`stdlib` or `compiler`) instead of trying silent legacy fallbacks.
Repository-audit failures aggregate and sort all layout, format, dependency,
manifest, JSON, and missing-file violations for deterministic output.

The editor grammar has no runtime failure path. Invalid JSON, stale contributed
paths, missing `.jz` registration, or missing required fixture coverage fails
the Haskell repository-audit suite.

## Testing and Verification

Implementation follows test-first slices:

1. make repository-audit tests describe the new roots, packaging contract, and
   dependency direction, observe them fail against the old layout, then move
   files and path consumers;
2. add behavior coverage proving compact multi-parameter lambdas retain
   currying/partial application, then migrate authored sources and programs;
3. add editor-package validation tests before adding the manifest, grammar,
   configuration, and fixture; and
4. update active docs and close the backlog batch only after the complete gate
   passes.

Focused verification uses Cabal component names through the pinned GHC 9.14.1
environment. Final verification is:

```sh
nix --extra-experimental-features 'nix-command flakes' develop -c \
  cabal build --project-dir=jazz-next all
nix --extra-experimental-features 'nix-command flakes' develop -c \
  cabal test --project-dir=jazz-next all --test-show-details=failures
bash scripts/check-execution-queue.sh
bash scripts/check-docs.sh
git diff --check
cabal sdist --project-dir=jazz-next
```

The generated source archive is inspected to confirm the Jazz sources and
editor package are present and the old `stdlib/` root is absent.

## Documentation and Closeout

Active path references in `jazz-next/README.md`, active specs,
`docs/jazz-language-state.md`, and repository-audit guidance move to the new
root. Historical implementation plans, archived closure evidence, and legacy
references retain their original paths unless they are active instructions.

On completion, Batch 2 is marked completed in
`docs/jazz-improvement-backlog.md`. The general execution queue remains a
separate dispatcher; this batch must not replace its parser-design curation
target with unrelated implementation work.
