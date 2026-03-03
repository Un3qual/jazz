# jazz-next

This directory is the only active target for new compiler implementation work.

Legacy references:
- `jazz-hs/` is read-only historical reference code.
- `jazz2/` is read-only experimental/reference code.

Do not implement new compiler functionality in legacy directories.

## Current implementation slices

- `src/JazzNext/Compiler/Warnings.hs`: warning category/severity definitions and category parsing.
- `src/JazzNext/Compiler/WarningConfig.hs`: `-W` token parsing and precedence merge (`CLI > env > config > default`).
- `src/JazzNext/Compiler/Diagnostics.hs`: warning payload shape and deterministic warning sorting.
- `src/JazzNext/Compiler/Analyzer.hs`: minimal scope-aware rebinding warning analysis (`W0001`).
- `src/JazzNext/Compiler/TypeInference.hs`: inference result plumbing that carries optional warnings.
- `src/JazzNext/Compiler/Driver.hs`: warning-as-error gating and compile-result shaping.
- `src/JazzNext/CLI/Main.hs`: CLI flag/env/config resolution and warning-aware compile output behavior.
- `test/WarningConfigSpec.hs`: unit tests for warning-config parsing and precedence.
- `test/RebindingWarningSpec.hs`: analyzer/driver warning behavior tests.
- `test/CLISpec.hs`: CLI entrypoint behavior tests (parsing, precedence, stderr/stdout policy).
- `scripts/test-warning-config.sh`: local test entrypoint.

## Run tests

```bash
# from repository root:
bash jazz-next/scripts/test-warning-config.sh
# or from inside jazz-next/:
# bash scripts/test-warning-config.sh
```
