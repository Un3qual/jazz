# Compiler Warning Flags (Same-Scope Rebinding) Implementation Plan

> **For implementers:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Add compiler warning-flag infrastructure so same-scope rebinding stays semantically valid by default, but can emit optional warnings (and optional warning-as-error behavior) via CLI flags, environment variables, and project config.

**Architecture:** Introduce a small warning-configuration subsystem (warning categories + precedence rules), thread it through CLI -> `Lib` -> analyzer entrypoints, and emit structured same-scope rebinding diagnostics from analyzer plumbing without changing default compile success behavior.

**Tech Stack:** Markdown spec docs, validation scripts, and future `jazz-next` compiler/analyzer/tests work.

Execution note:
- `jazz-hs/` references in this plan are legacy evidence only.
- All net-new implementation and tests for this item must land in `jazz-next/`.

---

## Progress

- [x] Upstream semantic gate locked: same-scope rebinding warnings are optional and default-silent.
- [x] Warning category model and naming locked.
- [x] CLI flag contract locked.
- [x] Config/env contract and precedence locked.
- [x] Phase 1 warning-config plumbing implemented in `jazz-next`.
- [x] Analyzer warning plumbing implemented for same-scope rebinding.
- [x] Warning tests (unit + integration) passing.
- [x] Documentation and migration notes published.

## Decision Lock (Inherited from Item 13)

- [x] `same-scope rebinding` remains **valid** language behavior (`last wins`).
- [x] Warning emission for rebinding is **tooling-controlled**, not semantic invalidation.
- [x] Default behavior remains **silent** until warning flags/config opt in.

Source of truth:
- `docs/spec/semantics/bindings-and-signatures.md`
- `docs/spec/tooling/compiler-warning-flags.md`
- `docs/plans/spec-clarification/2026-03-03/semantics/13-binding-and-signature-coherence.md`

## Scope Guardrails

In scope:
- warning categories and stable warning IDs for compiler diagnostics,
- CLI flags for warning enable/disable/escalation,
- environment variable and project config support with deterministic precedence,
- analyzer plumbing for optional same-scope rebinding warnings,
- tests for category parsing, precedence, analyzer emission, and CLI behavior,
- docs updates and migration notes.

Out of scope:
- introducing unrelated warning families beyond taxonomy placeholders,
- changing rebinding semantics from warning to hard error by default,
- broad CLI redesign unrelated to warning controls.

## Executor Discretion Boundaries

Implementation details may vary if the external contract remains stable:

- Argument parsing may stay manual (`getArgs`) or move to a parser library.
- Config-file format may be simple line-based or key/value, but must be deterministic and documented.
- Analyzer warning accumulation may use `Writer`, explicit state fields, or a dedicated result type.

Non-negotiable invariants:

- CLI > env > config > default precedence.
- Default compile behavior unchanged (no warning output unless enabled).
- Same-scope rebinding warning order is deterministic and test-covered.

## Warning Taxonomy (Phase 0 Lock)

Primary category implemented in this item:

- `same-scope-rebinding` (code: `W0001`)

Reserved (declared but inactive) category namespace for future items:

- `shadowing-outer-scope`
- `unused-binding`
- `deprecated-syntax`

Notes:

- Only `same-scope-rebinding` must emit diagnostics in this item.
- Reserved categories should not emit until explicitly implemented.

## CLI / Config / Env Contract (Target)

CLI flags (compiler executable):

- `-Wsame-scope-rebinding` enables the category.
- `-Wno-same-scope-rebinding` disables the category.
- `-Werror=same-scope-rebinding` promotes this warning category to an error.
- `-Werror` promotes all enabled warning categories to errors.
- `-Wnone` disables all categories for the invocation.
- `--warnings-config <path>` overrides default config path lookup.

Environment variables:

- `JAZZ_WARNING_FLAGS`:
  comma-separated tokens using CLI category names.
  Example: `same-scope-rebinding,-unused-binding`
- `JAZZ_WARNING_ERROR_FLAGS`:
  comma-separated categories promoted to errors.
  Example: `same-scope-rebinding`
- `JAZZ_WARNING_CONFIG`:
  optional config-file path override.

Project config:

- default path: `.jazz-warnings` at workspace root (or executable CWD if root detection is unavailable),
- contains warning tokens matching CLI category names (one token per line or comma-separated list, executor choice),
- comments supported via `#` prefix.

Precedence:

1. CLI flags
2. Environment variables
3. `.jazz-warnings` config file
4. Default (`all warnings off`, no warning-as-error escalation)

## Exact File Targets

Core compiler plumbing:

- Create: `jazz-next/src/JazzNext/Compiler/Warnings.hs`
- Create: `jazz-next/src/JazzNext/Compiler/WarningConfig.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Analyzer.hs` (Phase 2 planned)
- Modify: `jazz-next/src/JazzNext/Compiler/TypeInference.hs` (Phase 2 planned)
- Modify: `jazz-next/src/JazzNext/Compiler/Diagnostics.hs` (Phase 2 planned)
- Modify: `jazz-next/src/JazzNext/CLI/Main.hs` (Phase 3 planned)
- Modify: `jazz-next/src/JazzNext/Compiler/Driver.hs` (Phase 3 planned)

Tests:

- Create: `jazz-next/test/WarningConfigSpec.hs`
- Create: `jazz-next/test/RebindingWarningSpec.hs` (Phase 4 planned)
- Modify: `jazz-next/test/TypeInferenceSpec.hs` (if shared helpers are preferable)
- Modify: `jazz-next/test/Spec.hs` (if suite aggregation is used)
- Create: `jazz-next/scripts/test-warning-config.sh`

Docs:

- Create: `docs/spec/tooling/compiler-warning-flags.md`
- Modify: `docs/spec/semantics/bindings-and-signatures.md`
- Modify: `docs/jazz-language-state.md`
- Modify: `docs/plans/spec-clarification/2026-03-03/README.md`

## Phase 0: Contract Freeze (Docs-First)

- [x] Lock warning category names and IDs (`W0001` for same-scope rebinding).
- [x] Lock CLI/env/config token grammar and precedence.
- [x] Lock warning message shape and minimum location metadata requirements.
- [x] Add examples for enabled/disabled/error-promoted behavior.

Create:

- `docs/spec/tooling/compiler-warning-flags.md`

Modify:

- `docs/spec/semantics/bindings-and-signatures.md` (replace deferred note with active warning-flag reference)
- `docs/plans/spec-clarification/2026-03-03/tooling/18-compiler-warning-flags.md` (mark phase progress as execution advances)

### Commit Checkpoint (Phase 0)

```bash
git add docs/spec/tooling/compiler-warning-flags.md \
  docs/spec/semantics/bindings-and-signatures.md \
  docs/plans/spec-clarification/2026-03-03/tooling/18-compiler-warning-flags.md
git commit -m "docs(tooling): define compiler warning flag contract"
```

## Phase 1: Warning Config and Resolution Plumbing

- [x] Add warning category and severity data types.
- [x] Add parser utilities for category tokens and `-W`-style flags.
- [x] Implement config/env loading and precedence merge (`CLI > env > config > default`).
- [x] Implement unknown-category handling (diagnostic + non-zero exit for invalid flags/config).

Create:

- `jazz-next/src/JazzNext/Compiler/Warnings.hs`
- `jazz-next/src/JazzNext/Compiler/WarningConfig.hs`
- `jazz-next/test/WarningConfigSpec.hs`
- `jazz-next/scripts/test-warning-config.sh`

Modify:

- `docs/plans/spec-clarification/2026-03-03/tooling/18-compiler-warning-flags.md`

### Commit Checkpoint (Phase 1)

```bash
git add jazz-next/src/JazzNext/Compiler/Warnings.hs \
  jazz-next/src/JazzNext/Compiler/WarningConfig.hs \
  jazz-next/test/WarningConfigSpec.hs \
  jazz-next/scripts/test-warning-config.sh \
  docs/plans/spec-clarification/2026-03-03/tooling/18-compiler-warning-flags.md
git commit -m "feat(compiler): add warning category and config resolution plumbing"
```

## Phase 2: Analyzer Warning Emission for Same-Scope Rebinding

- [x] Add analyzer output shape that can carry warnings without breaking default call sites.
- [x] Detect same-scope rebinding in scope-local `let` handling (current scope only, not outer-scope shadowing).
- [x] Emit `W0001` warning payload with variable name and source span(s).
- [x] Apply warning-as-error escalation based on resolved warning config.
- [x] Keep default analyzer behavior unchanged when warning category is disabled.

Modify:

- `jazz-next/src/JazzNext/Compiler/Analyzer.hs`
- `jazz-next/src/JazzNext/Compiler/TypeInference.hs`
- `jazz-next/src/JazzNext/Compiler/Diagnostics.hs`

### Commit Checkpoint (Phase 2)

```bash
git add jazz-next/src/JazzNext/Compiler/Analyzer.hs \
  jazz-next/src/JazzNext/Compiler/TypeInference.hs \
  jazz-next/src/JazzNext/Compiler/Diagnostics.hs
git commit -m "feat(analyzer): emit optional same-scope rebinding warnings"
```

## Phase 3: CLI + Lib Integration

- [x] Parse warning flags in executable entrypoint and resolve config/env inputs.
- [x] Thread resolved warning config into analyzer invocation path.
- [x] Print warnings to stderr in deterministic order while still emitting generated JS on success.
- [x] Ensure warning-as-error returns non-zero and suppresses JS output on promoted warnings.

Modify:

- `jazz-next/src/JazzNext/CLI/Main.hs`
- `jazz-next/src/JazzNext/Compiler/Driver.hs`

### Commit Checkpoint (Phase 3)

```bash
git add jazz-next/src/JazzNext/CLI/Main.hs \
  jazz-next/src/JazzNext/Compiler/Driver.hs
git commit -m "feat(cli): wire warning flags and diagnostics into compiler entrypoint"
```

## Phase 4: Test Matrix (Parser, Precedence, Analyzer, CLI)

- [x] Add warning-config parser tests:
  - category parsing,
  - invalid token handling,
  - precedence resolution order.
- [x] Add analyzer tests for same-scope rebinding:
  - warning disabled -> no warning,
  - warning enabled -> one warning,
  - repeated rebinds -> deterministic warning order,
  - nested-scope shadowing -> no `same-scope-rebinding` warning.
- [x] Add warning-as-error tests:
  - enabled + promoted category fails compilation.
- [x] Wire new spec modules into test runner.

Create:

- `jazz-next/test/WarningConfigSpec.hs`
- `jazz-next/test/RebindingWarningSpec.hs`

Modify:

- `jazz-next/test/Spec.hs`
- `jazz-next/test/TypeInferenceSpec.hs` (optional helper reuse)

### Commit Checkpoint (Phase 4)

```bash
git add jazz-next/test/WarningConfigSpec.hs \
  jazz-next/test/RebindingWarningSpec.hs \
  jazz-next/test/Spec.hs \
  jazz-next/test/TypeInferenceSpec.hs
git commit -m "test(compiler): add warning flag and rebinding warning coverage"
```

(If `TypeInferenceSpec.hs` is unchanged, omit it from the commit.)

## Phase 5: Documentation and Migration Notes

- [x] Document warning flags, config format, env variables, precedence, and examples.
- [x] Add migration notes for teams that want to opt into warnings gradually.
- [x] Update language-state tracker to reflect warning tooling is now available.
- [x] Mark this plan and batch index progress.

Create/Modify:

- `docs/spec/tooling/compiler-warning-flags.md` (include a `Migration Notes` section)
- `docs/jazz-language-state.md`
- `docs/plans/spec-clarification/2026-03-03/README.md`
- `docs/plans/spec-clarification/2026-03-03/tooling/18-compiler-warning-flags.md`

### Commit Checkpoint (Phase 5)

```bash
git add docs/spec/tooling/compiler-warning-flags.md \
  docs/jazz-language-state.md \
  docs/plans/spec-clarification/2026-03-03/README.md \
  docs/plans/spec-clarification/2026-03-03/tooling/18-compiler-warning-flags.md
git commit -m "docs(tooling): publish warning flags guide and migration notes"
```

## Verification Commands

Phase 1 (completed in this batch):

```bash
bash jazz-next/scripts/test-warning-config.sh
```

Phase 2+ (planned after analyzer and CLI integration):

```bash
# Add analyzer + CLI integration checks once Phase 2 and 3 land.
```

## Definition of Done

- [x] `same-scope-rebinding` warning category is implemented with stable ID (`W0001`).
- [x] CLI, env, and config warning controls work with deterministic precedence.
- [x] Analyzer emits optional same-scope rebinding diagnostics without changing default semantics.
- [x] Warning-as-error flow is implemented and tested.
- [x] Documentation includes clear migration notes for phased adoption.
