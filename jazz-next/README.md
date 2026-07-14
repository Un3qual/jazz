# jazz-next

This directory is the only active target for new compiler implementation work.

Legacy references:
- `jazz-hs/` is read-only historical reference code.
- `jazz2/` is read-only experimental/reference code.

Do not implement new compiler functionality in legacy directories.

`jazz-next` is currently a CLI/compiler package. Its Haskell implementation is
provided through the private `jazz-next-internal` package library solely for the
executable and test components; there is no supported Haskell embedding API yet.

## Current architecture

- `Compiler.Parser.*` owns surface grammar and one Megaparsec error model;
  `Parser.Lower` produces the canonical core AST.
- `Compiler.AST` has one conditional node (`EIf`), one pattern-case node
  (`EPatternCase`), and ordinary `EApply` for application including `$`.
- `Compiler.Name` owns both source identifiers and structured resolved names;
  compiler phases render names only for diagnostics and user output.
- `Compiler.ModuleResolver` parses and lowers each source once into a
  dependency-ordered `ResolvedProgram` of retained `CoreModule` values.
- `Compiler.ModuleCompiler` analyzes each module against explicit dependency
  `ModuleInterface` values; `Compiler.ModuleRuntime` evaluates modules against
  explicit runtime exports and executes only the entry module's expressions.
- `Compiler.TypeInference` is the public façade over focused internal type,
  state, solver, capability, pattern, scope, and diagnostic modules.
- `Compiler.Driver` coordinates prelude preparation, resolution, per-module
  compilation, warning promotion, and optional runtime evaluation.
- `jazz-next.cabal` defines the private `jazz-next-internal` implementation
  library, the `jazz-next` executable, and the registered test suites.
- Cabal discovers and runs every registered test suite; `repository-audit-spec`
  owns stdlib formatting and private-package policy.

These are implementation boundaries only. The module and import syntax exposed
to Jazz programs is unchanged by the internal module pipeline.

## Test layout

- `test/JazzNext/TestHarness.hs`: shared assertion helpers and test runner plumbing.
- `test/JazzNext/CLI/`: CLI entrypoint coverage.
- `test/JazzNext/Compiler/Config/`: warning/config parsing coverage.
- `test/JazzNext/Compiler/Diagnostics/`: diagnostic rendering and metadata coverage.
- `test/JazzNext/Compiler/Modules/`: prelude loading, module graph, and resolver coverage.
- `test/JazzNext/Compiler/Parser/`: parser, lowering, and operator-surface coverage.
- `test/JazzNext/Compiler/Semantics/`: analyzer, type, runtime, and builtin semantics coverage.

## Run a first program

Create `first.jz`:

```jazz
answer = 40 + 2.
answer.
```

Compile it:

```bash
cabal run --project-dir=jazz-next jazz-next -- first.jz
```

Successful compile output is quiet. Run it:

```bash
cabal run --project-dir=jazz-next jazz-next -- --run first.jz
```

Expected output:

```text
42
```

Run source from stdin explicitly:

```bash
printf '40 + 2.' | cabal run --project-dir=jazz-next jazz-next -- --run -
```

Show CLI help:

```bash
cabal run --project-dir=jazz-next jazz-next -- --help
```

The help path prints usage to stdout and does not read stdin, source files,
warning config files, or Prelude files.

## Build and test

```bash
# from repository root:
nix --extra-experimental-features 'nix-command flakes' develop
cabal build --project-dir=jazz-next all
cabal test --project-dir=jazz-next all --test-show-details=failures
cabal test --project-dir=jazz-next repository-audit-spec --test-show-details=failures
```

Cabal discovers every registered suite. Use a test component name, such as
`repository-audit-spec`, for a focused run. The repository audit owns the
stdlib source-format contract and the private-package policy.
