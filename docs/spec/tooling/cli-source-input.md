# CLI Source Input

Status: active (implemented in `Jazz`)
Locked decisions: 2026-05-24

## Purpose

Define the active `Jazz` CLI source-selection contract for standalone
compile and run invocations.

## Implementation Target

- Source-input behavior is implemented in `src/Jazz/CLI/Main.hs`.
- Pre-root-canonicalization behavior preserved at archive tag
  `archive/pre-root-canonicalization-2026-07-31` is historical evidence only;
  the archived implementation trees are absent from the current checkout.

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
cabal run jazz -- first.jz
```

Run a file:

```bash
cabal run jazz -- --run first.jz
```
