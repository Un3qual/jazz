# Jazz Repository Guidance

Make sure you commit along the way as needed.

## Active Compiler Paths

- `src/` contains the active Haskell compiler and runtime implementation.
- `jazz/` contains the active Jazz-authored standard library and hosted compiler sources.
- `app/` contains the active command-line entry point.
- `test/` contains the active compiler, runtime, CLI, and repository tests.
- These root directories are the only active compiler paths.

## Execution Rules

- Dispatch work from `.codex/execution/queue.md` and keep active implementation plans under `.codex/plans/`.
- Treat `.codex/execution/` and `.codex/plans/` as internal project state; neither location defines public language behavior.
- Implement compiler behavior, APIs, runtime, parser, analyzer, and code generation in the active root paths only.
- Point implementation documentation to root paths such as `src/Jazz/`, `jazz/`, `app/`, and `test/Jazz/`.
- Treat `docs/language/` and `docs/reference/` as the public language contract.
- Treat current `src/`, `jazz/`, and `test/` behavior as implementation evidence, accepted RFCs as authoritative durable decisions, and roadmap material as non-normative.
