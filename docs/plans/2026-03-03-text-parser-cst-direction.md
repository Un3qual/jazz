# Jazz Next Front-End Direction (Text, Parser Stack, CST)

## Scope

This note records the outcomes of three parallel tasks for the active compiler path (`jazz-next/`):

1. Move compiler-internal textual data to `Data.Text`.
2. Evaluate parser/lexer direction (`handwritten` vs `megaparsec` vs `attoparsec`).
3. Evaluate whether to add a CST stage before the AST now.

Date: 2026-03-03

## Outcome 1: `Data.Text` Refactor

### Decision

Adopt `Data.Text.Text` as the default textual type across compiler internals in `jazz-next/`, and enable `OverloadedStrings` in touched modules for ergonomic literals.

### What Changed

- Parser and lexer APIs now operate on `Text`.
- Core and surface AST textual fields (identifiers/operators/signatures) now use `Text`.
- Analyzer, diagnostics, warning config/warnings, type inference, driver, CLI internals, and related tests were updated to align on `Text`.

### Intentional `String` Boundaries Kept

- CLI argument/environment boundaries from `System.Environment` (`[String]`, `String -> IO (Maybe String)`).
- `Text.Read.decimal` return type (`Either String (Integer, Text)`), dictated by library API.

These are conversion boundaries, not compiler-internal representation choices.

### Verification Evidence

- `bash jazz-next/scripts/test-warning-config.sh` passed after refactor.
- `rg -n "\bString\b|\[Char\]" jazz-next/src jazz-next/test` shows only intentional boundary usages.

## Outcome 2: Parser/Lexer Technology Direction

### Decision

Adopt an incremental migration path toward **Megaparsec** for parser growth. Do **not** adopt Attoparsec for the language front-end.

### Rationale

- Current handwritten parser is acceptable for the current grammar and has low immediate migration pressure.
- Expected language growth raises long-term maintainability and diagnostic quality needs.
- Megaparsec is a better fit for typed/custom errors, richer diagnostics, and grammar evolution.
- Attoparsec prioritizes throughput and incremental protocol/data parsing over diagnostics, which is the wrong optimization target for this front-end stage.

### Migration Shape

1. Freeze current behavior with parser parity tests (AST shape + span/error position expectations).
2. Keep handwritten lexer initially.
3. Introduce a Megaparsec parser over the existing token stream.
4. Preserve external parser API shape during migration.
5. After parity, evolve diagnostics from stringly errors to structured parse diagnostics.
6. Benchmark handwritten vs Megaparsec on representative larger sources before removing old parser code.

## Outcome 3: CST Before AST

### Decision

**Defer** adding a full explicit CST stage for now.

### Rationale

- Current milestones are still focused on parser/analyzer/runtime bring-up and syntax expansion.
- A full CST now adds implementation and maintenance overhead during active grammar churn.
- Current pipeline is sufficient for immediate goals.

### Triggers to Revisit and Likely Introduce CST

- Need parser recovery with multiple actionable diagnostics in a single pass.
- Start formatter/linter/LSP work requiring trivia/comment preservation and precise source edits.
- Add macros or source-to-source transforms that must preserve concrete syntax.
- Syntax surface stabilizes and parser is fully source-driven in production flow.

## Consolidated Direction

- Keep `jazz-next/` as the only active implementation target.
- Continue shipping feature work on current pipeline, now using `Text` internally.
- Plan a staged parser migration to Megaparsec once parity tests are explicit.
- Revisit CST only when tooling and recovery requirements justify the additional layer.
