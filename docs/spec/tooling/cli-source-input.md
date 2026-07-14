# CLI Source Input

Status: active (implemented in `jazz-next`)
Locked decisions: 2026-05-24
Primary plan: `docs/plans/2026-05-23-jazz-next-first-program-cli-flow.md`

## Purpose

Define the active `jazz-next` CLI source-selection contract for standalone
compile and run invocations.

## Implementation Target

- Source-input behavior is implemented in `jazz-next/src/JazzNext/CLI/Main.hs`.
- `jazz-hs/` and `jazz2/` remain read-only legacy evidence.

## Source Selection Contract

1. With no positional source file, standalone CLI compile and `--run` read the
   program from stdin.
2. With `-` as the positional source selector, standalone CLI compile and
   `--run` read the program from stdin explicitly. `-` counts as the one
   allowed source selector.
3. With one positional `.jz` source file, standalone CLI compile and `--run`
   read that file and do not read stdin.
4. Successful standalone compile remains diagnostics-only: stdout and stderr
   are quiet unless warnings or errors are emitted.
5. `--run` evaluates the selected source and writes the rendered result to
   stdout followed by a newline.

## Rejection Contract

1. More than one positional source file is invalid and reports:
   `multiple source files are not supported`.
2. `-` cannot be combined with a positional `.jz` source file and reports:
   `multiple source files are not supported`.
3. A source selector, whether a positional `.jz` source file or `-`, cannot be
   combined with `--entry-module`; module graph execution remains selected
   through `--entry-module` and `--module-root`.
4. A missing or unreadable source file is an argument/configuration error and
   reports: `source file could not be read at '<path>'`.

## Examples

Compile a file quietly:

```bash
cabal run --project-dir=jazz-next jazz-next -- first.jz
```

Run a file:

```bash
cabal run --project-dir=jazz-next jazz-next -- --run first.jz
```
