# jazz-next

This directory is the only active target for new compiler implementation work.

Legacy references:
- `jazz-hs/` is read-only historical reference code.
- `jazz2/` is read-only experimental/reference code.

Do not implement new compiler functionality in legacy directories.

## Current implementation slices

- `src/JazzNext/Compiler/Warnings.hs`: warning category/severity definitions and category parsing.
- `src/JazzNext/Compiler/WarningConfig.hs`: `-W` token parsing and precedence merge (`CLI > env > config > default`).
- `test/WarningConfigSpec.hs`: unit tests for warning-config phase-1 behavior.
- `scripts/test-warning-config.sh`: local test entrypoint.

## Run tests

```bash
bash jazz-next/scripts/test-warning-config.sh
```
