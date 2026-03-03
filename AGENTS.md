# Agent Guardrails

## Compiler Workspace Policy

- `jazz-hs/` is a legacy reference implementation and must be treated as **read-only**.
- `jazz2/` is a legacy/experimental reference and must be treated as **read-only**.
- All new compiler implementation work must be created under `jazz-next/`.

## Execution Rules

- Do not modify files under `jazz-hs/` or `jazz2/` unless the user explicitly asks for legacy maintenance.
- If a task involves net-new compiler behavior, APIs, runtime, parser, analyzer, or codegen changes, implement them only in `jazz-next/`.
- If documentation references implementation work, point to `jazz-next/` as the active compiler path.
