# jazz-next

This directory is the only active target for new compiler implementation work.

Legacy references:
- `jazz-hs/` is read-only historical reference code.
- `jazz2/` is read-only experimental/reference code.

Do not implement new compiler functionality in legacy directories.

## Current implementation slices

- `src/JazzNext/Compiler/AST.hs`: analyzer-facing core AST used after parser/lowering.
- `src/JazzNext/Compiler/Warnings.hs`: warning category/severity definitions and category parsing.
- `src/JazzNext/Compiler/WarningConfig.hs`: `-W` token parsing and precedence merge (`CLI > env > config > default`).
- `src/JazzNext/Compiler/Diagnostics.hs`: warning payload shape and deterministic warning sorting.
- `src/JazzNext/Compiler/Parser/Lexer.hs`: minimal tokenization for parser bootstrap.
- `src/JazzNext/Compiler/Parser.hs`: minimal surface parser for current syntax slices.
- `src/JazzNext/Compiler/Parser/AST.hs`: parse-surface AST contract.
- `src/JazzNext/Compiler/Parser/Lower.hs`: parse-AST -> analyzer-AST lowering boundary.
- `src/JazzNext/Compiler/Analyzer.hs`: scope-aware rebinding warning analysis (`W0001`) plus binding/signature coherence checks (signature adjacency + unbound variable diagnostics).
- `src/JazzNext/Compiler/TypeInference.hs`: inference result plumbing that carries warnings and semantic errors from analysis.
- `src/JazzNext/Compiler/Driver.hs`: warning-as-error gating plus semantic-error propagation into compile results.
- `src/JazzNext/CLI/Main.hs`: CLI flag/env/config resolution and warning-aware compile output behavior.
- `jazz-next.cabal`: packaged library, `jazz-next` executable, and complete test-suite registration.
- `scripts/test-warning-config.sh`: compatibility runner for executing every spec through the local `runghc` wrapper.

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

## Run tests

```bash
# from repository root:
cabal test --project-dir=jazz-next all

# compatibility runner for the same spec programs:
bash jazz-next/scripts/test-warning-config.sh
```
