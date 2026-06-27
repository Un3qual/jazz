---
id: JN-RUNTIME-CLI-PRODUCT-DELTA-CONTRACT-001
status: done
priority: P2
size: S
kind: coordination
autonomous_ready: yes
depends_on:
  - JN-RUNTIME-PRODUCTIZE-CLOSURE-PLAN-001
last_verified: 2026-06-24
completed_on: 2026-06-24
plan_section: "Completed contract batch: CLI help output"
target_paths:
  - docs/plans/2026-03-18-jazz-next-runtime-architecture-and-interpreter-execution-plan.md
  - docs/jazz-language-state.md
  - docs/execution/blocker-contracts.md
  - docs/execution/queue.md
verification:
  - bash scripts/check-execution-queue.sh
  - bash scripts/check-docs.sh
  - git diff --check
deliverable: "Lock one concrete CLI/runtime product delta beyond the closed interpreter-first baseline: explicit help output for the jazz-next CLI, with focused CLISpec ownership and no compile/run semantic changes."
supersedes:
  - docs/plans/spec-clarification/2026-03-02/runtime/12a-haskell-interpreter-implementation.md
---

# Jazz-Next Runtime CLI Product Delta Contract

> Active-path coordination child for `JN-RUNTIME-PRODUCTIZE-CLOSURE-PLAN-001`.
> This batch names one product delta before any runtime behavior changes return
> to `Ready Now`.

Completed on `2026-06-24`. The accepted product delta is CLI help output only.
Implementation is promoted separately as `JN-RUNTIME-CLI-HELP-001`.

**Goal:** make the interpreter-first CLI discoverable without changing compile,
run, module graph, prelude, warning, or runtime semantics.

**Architecture:** treat help as a CLI preflight mode. Explicit help flags
produce stable usage text and exit before reading source input, warning config,
prelude files, or module graph files. All ordinary compile and `--run` paths
keep the existing diagnostic/output contracts.

**Tech Stack:** documentation under `docs/plans/` and `docs/jazz-language-state.md`,
queue verification through `bash scripts/check-execution-queue.sh` and
`bash scripts/check-docs.sh`, with follow-up implementation in
`jazz-next/src/JazzNext/CLI/Main.hs` and `jazz-next/test/JazzNext/CLI/CLISpec.hs`.

---

## Completed contract batch: CLI help output

This coordination batch updates only docs and queue state. It must not edit CLI
implementation or test files.

Accepted product delta:

- Add explicit help output for the `jazz-next` CLI.
- Recognize `--help` and `-h`.
- Do not add a bare `help` subcommand, because positional source files are
  already accepted and a file named `help` must remain a valid source path.
- Keep compile mode diagnostic-only.
- Keep `--run` as the only mode that prints evaluated program output.
- Keep module graph, prelude, warning-config, and source-file behavior
  semantically unchanged.

Help-mode contract:

- `jazz-next --help` and `jazz-next -h` exit `0`.
- Help writes usage text to stdout and writes nothing to stderr.
- Help output ends with exactly one trailing newline.
- Help output includes:
  - standalone stdin/file compile usage,
  - `--run`,
  - `--entry-module`,
  - `--module-root`,
  - `--prelude`,
  - `--no-prelude`,
  - `--warnings-config`,
  - `-W<category>` / `-Werror=<category>`,
  - `--help` and `-h`.
- Help preempts ordinary work: source stdin, positional source files, warning
  config files, prelude files, and module files are not read when a help flag is
  present.
- Help flags win over other arguments for user-facing execution. For example,
  `--help --bad-arg` and `--help missing.jz` still print help and exit `0`
  without argument or source-read diagnostics.

Out of scope:

- changing compile success output,
- changing runtime output formatting,
- adding packaged executable installation,
- adding generated artifacts,
- adding a bare `help` command,
- adding new module graph behavior,
- adding a second backend pipeline,
- changing warning, prelude, or source-selection semantics except for help
  preemption,
- any `jazz-hs/` or `jazz2/` work.

Promoted implementation child:

- `JN-RUNTIME-CLI-HELP-001`

Implementation target seed:

- `jazz-next/src/JazzNext/CLI/Main.hs`
- `jazz-next/test/JazzNext/CLI/CLISpec.hs`
- `jazz-next/README.md`

Focused implementation verification:

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/CLI/CLISpec.hs
bash scripts/check-execution-queue.sh
bash scripts/check-docs.sh
git diff --check
```
