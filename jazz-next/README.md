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
- `test/WarningConfigSpec.hs`: unit tests for warning-config parsing and precedence.
- `test/RebindingWarningSpec.hs`: analyzer/driver warning behavior tests.
- `test/CLISpec.hs`: CLI entrypoint behavior tests (parsing, precedence, stderr/stdout policy).
- `test/BindingSignatureCoherenceSpec.hs`: analyzer contract tests for signature adjacency and use-before-definition.
- `test/ParserFoundationSpec.hs`: parser bootstrap tests for accepted syntax slices and parse/lower boundary.
- `scripts/test-warning-config.sh`: local `jazz-next` test entrypoint.

## Run tests

```bash
# from repository root:
bash jazz-next/scripts/test-warning-config.sh
# or from inside jazz-next/:
# bash scripts/test-warning-config.sh
```
