# Jazz Repository Guidance

Make sure you commit along the way as needed.

## General Rules
- Prefer concise solutions for current problems. Still keep in mind future plans, but channel "YAGNI". Don't overcomplicate a solution "just in case" things may change or requirements may expand in the future.
- Don't add unnecessary regression tests for every single review comment. Analyze whether it is worth adding a regression test before blindly adding one when the issue may just be something like a one time typo or mistake.
- Overall, be deliberate about each test you add and consider if it is actually needed, if it will prevent future issues, etc.

## Haskell Style

- Use advanced Haskell features and language extensions when they simplify or reduce code, or improve maintainability. Evaluate the concrete benefit rather than avoiding features categorically.

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
