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
- [ ] Analyzer warning plumbing implemented for same-scope rebinding.
- [ ] Warning tests (unit + integration) passing.
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

- Create: `jazz-hs/src/Compiler/Warnings.hs`
- Create: `jazz-hs/src/Compiler/WarningConfig.hs`
- Modify: `jazz-hs/src/Analyzer.hs`
- Modify: `jazz-hs/src/Analyzer/TypeInference.hs`
- Modify: `jazz-hs/src/Errors.hs`
- Modify: `jazz-hs/src/Lib.hs`
- Modify: `jazz-hs/app/Main.hs`
- Modify: `jazz-hs/package.yaml`
- Modify: `jazz-hs/jazz.cabal` (regenerate or sync with `package.yaml`)

Tests:

- Create: `jazz-hs/test/Compiler/WarningConfigSpec.hs`
- Create: `jazz-hs/test/Analyzer/RebindingWarningSpec.hs`
- Modify: `jazz-hs/test/Analyzer/TypeInferenceSpec.hs` (if shared helpers are preferable)
- Modify: `jazz-hs/test/Spec.hs`

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

- [ ] Add warning category and severity data types.
- [ ] Add parser utilities for category tokens and `-W`-style flags.
- [ ] Implement config/env loading and precedence merge (`CLI > env > config > default`).
- [ ] Implement unknown-category handling (diagnostic + non-zero exit for invalid flags/config).

Create:

- `jazz-hs/src/Compiler/Warnings.hs`
- `jazz-hs/src/Compiler/WarningConfig.hs`

Modify:

- `jazz-hs/package.yaml`
- `jazz-hs/jazz.cabal`

### Commit Checkpoint (Phase 1)

```bash
git add jazz-hs/src/Compiler/Warnings.hs \
  jazz-hs/src/Compiler/WarningConfig.hs \
  jazz-hs/package.yaml jazz-hs/jazz.cabal
git commit -m "feat(compiler): add warning category and config resolution plumbing"
```

## Phase 2: Analyzer Warning Emission for Same-Scope Rebinding

- [ ] Add analyzer output shape that can carry warnings without breaking default call sites.
- [ ] Detect same-scope rebinding in `ELet` handling (current scope only, not outer-scope shadowing).
- [ ] Emit `W0001` warning payload with variable name and source span(s).
- [ ] Apply warning-as-error escalation based on resolved warning config.
- [ ] Keep default analyzer behavior unchanged when warning category is disabled.

Modify:

- `jazz-hs/src/Analyzer.hs`
- `jazz-hs/src/Analyzer/TypeInference.hs`
- `jazz-hs/src/Errors.hs`

### Commit Checkpoint (Phase 2)

```bash
git add jazz-hs/src/Analyzer.hs \
  jazz-hs/src/Analyzer/TypeInference.hs \
  jazz-hs/src/Errors.hs
git commit -m "feat(analyzer): emit optional same-scope rebinding warnings"
```

## Phase 3: CLI + Lib Integration

- [ ] Parse warning flags in executable entrypoint and resolve config/env inputs.
- [ ] Thread resolved warning config into analyzer invocation path.
- [ ] Print warnings to stderr in deterministic order while still emitting generated JS on success.
- [ ] Ensure warning-as-error returns non-zero and suppresses JS output on promoted warnings.

Modify:

- `jazz-hs/app/Main.hs`
- `jazz-hs/src/Lib.hs`

### Commit Checkpoint (Phase 3)

```bash
git add jazz-hs/app/Main.hs jazz-hs/src/Lib.hs
git commit -m "feat(cli): wire warning flags and diagnostics into compiler entrypoint"
```

## Phase 4: Test Matrix (Parser, Precedence, Analyzer, CLI)

- [ ] Add warning-config parser tests:
  - category parsing,
  - invalid token handling,
  - precedence resolution order.
- [ ] Add analyzer tests for same-scope rebinding:
  - warning disabled -> no warning,
  - warning enabled -> one warning,
  - repeated rebinds -> deterministic warning order,
  - nested-scope shadowing -> no `same-scope-rebinding` warning.
- [ ] Add warning-as-error tests:
  - enabled + promoted category fails compilation.
- [ ] Wire new spec modules into test runner.

Create:

- `jazz-hs/test/Compiler/WarningConfigSpec.hs`
- `jazz-hs/test/Analyzer/RebindingWarningSpec.hs`

Modify:

- `jazz-hs/test/Spec.hs`
- `jazz-hs/test/Analyzer/TypeInferenceSpec.hs` (optional helper reuse)

### Commit Checkpoint (Phase 4)

```bash
git add jazz-hs/test/Compiler/WarningConfigSpec.hs \
  jazz-hs/test/Analyzer/RebindingWarningSpec.hs \
  jazz-hs/test/Spec.hs \
  jazz-hs/test/Analyzer/TypeInferenceSpec.hs
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

Baseline and targeted tests:

```bash
cd jazz-hs
stack test --ta '--match "WarningConfig"'
stack test --ta '--match "RebindingWarning"'
stack test
```

CLI behavior checks:

```bash
printf 'x = 1.\nx = 2.\nx.\n' > /tmp/rebind.jz

# Default: silent
stack exec jazz-exe -- /tmp/rebind.jz >/tmp/rebind-default.js 2>/tmp/rebind-default.stderr
test ! -s /tmp/rebind-default.stderr

# Enable warning by CLI flag
stack exec jazz-exe -- -Wsame-scope-rebinding /tmp/rebind.jz >/tmp/rebind-warn.js 2>/tmp/rebind-warn.stderr
rg -n "W0001|same-scope-rebinding" /tmp/rebind-warn.stderr

# Enable warning by env var
JAZZ_WARNING_FLAGS=same-scope-rebinding stack exec jazz-exe -- /tmp/rebind.jz >/tmp/rebind-env.js 2>/tmp/rebind-env.stderr
rg -n "W0001|same-scope-rebinding" /tmp/rebind-env.stderr

# Escalate to error
stack exec jazz-exe -- -Wsame-scope-rebinding -Werror=same-scope-rebinding /tmp/rebind.jz >/tmp/rebind-err.js 2>/tmp/rebind-err.stderr && false || true
rg -n "error|W0001|same-scope-rebinding" /tmp/rebind-err.stderr
```

Config precedence checks:

```bash
cat > /tmp/.jazz-warnings <<'EOF'
same-scope-rebinding
EOF

# Config enables warning
stack exec jazz-exe -- --warnings-config /tmp/.jazz-warnings /tmp/rebind.jz >/tmp/rebind-config.js 2>/tmp/rebind-config.stderr
rg -n "W0001|same-scope-rebinding" /tmp/rebind-config.stderr

# CLI disable overrides config
stack exec jazz-exe -- --warnings-config /tmp/.jazz-warnings -Wno-same-scope-rebinding /tmp/rebind.jz >/tmp/rebind-config-off.js 2>/tmp/rebind-config-off.stderr
test ! -s /tmp/rebind-config-off.stderr
```

## Definition of Done

- [ ] `same-scope-rebinding` warning category is implemented with stable ID (`W0001`).
- [ ] CLI, env, and config warning controls work with deterministic precedence.
- [ ] Analyzer emits optional same-scope rebinding diagnostics without changing default semantics.
- [ ] Warning-as-error flow is implemented and tested.
- [x] Documentation includes clear migration notes for phased adoption.
