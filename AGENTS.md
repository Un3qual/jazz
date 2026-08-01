# Jazz Repository Guidance

Make sure you commit along the way as needed.

## Active Compiler Paths

- `src/` contains the active Haskell compiler and runtime implementation.
- `jazz/` contains the active Jazz-authored standard library and hosted compiler sources.
- `app/` contains the active command-line entry point.
- `test/` contains the active compiler, runtime, CLI, and repository tests.
- These root directories are the only active compiler paths.

## Execution Rules

- Implement compiler behavior, APIs, runtime, parser, analyzer, and code generation in the active root paths only.
- Point implementation documentation to root paths such as `src/Jazz/`, `jazz/`, `app/`, and `test/Jazz/`.
- Treat `docs/spec/` as the transitional public language contract until the documentation reset establishes `docs/language/` and `docs/reference/`.
- Treat current `src/`, `jazz/`, and `test/` behavior as implementation evidence. After Workstream 2, accepted RFCs become authoritative durable decisions; roadmap material remains non-normative.
