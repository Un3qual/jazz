# Compiler Warning Flags

Status: active (same-scope rebinding and outer-scope shadowing warning emission are implemented; unused-binding ordinary `let` emission is queued; remaining reserved warning metadata is locked for future categories)
Locked decisions: 2026-03-03
Primary plan: `docs/plans/spec-clarification/2026-03-03/tooling/18-compiler-warning-flags.md`

## Purpose

Define a stable warning-flag contract so semantically valid same-scope rebinding can emit optional diagnostics without changing default compile success behavior.

## Implementation Target

- New warning infrastructure implementation lands in `jazz-next/`.
- `jazz-hs/` and `jazz2/` remain read-only legacy evidence.

## Warning Categories and IDs

Implemented in this item:

- `same-scope-rebinding` (`W0001`)
- `shadowing-outer-scope` (`W0002`)

Queued for the next implementation batch:

- `unused-binding` (`W0003`)

Reserved namespace for future items:

- `deprecated-syntax` (`W0004`)

`same-scope-rebinding` and `shadowing-outer-scope` have analyzer emitters in
the active implementation. `unused-binding` parses through CLI/env/config and
has a stable ID/token, but it does not emit diagnostics until the queued
ordinary `let` batch lands. Reserved categories parse through CLI/env/config
and keep stable IDs/tokens, but they do not emit diagnostics until a future
implementation batch adds the corresponding analyzer behavior.

## Default Behavior

1. Same-scope rebinding remains semantically valid (`last wins`).
2. Default compilation is warning-silent.
3. Warning output is opt-in via CLI/env/config.

## CLI Contract

1. `-Wsame-scope-rebinding` enables warning category `same-scope-rebinding`.
2. `-Wno-same-scope-rebinding` disables warning category `same-scope-rebinding`.
3. `-Werror=same-scope-rebinding` promotes `W0001` to an error.
4. `-Werror` promotes all enabled warning categories to errors.
5. `-Wnone` disables all warning categories for the invocation.
6. `--warnings-config <path>` overrides default warning config lookup.

## Environment Contract

1. `JAZZ_WARNING_FLAGS`: comma-separated category toggles.
   - Example: `same-scope-rebinding,-unused-binding`
2. `JAZZ_WARNING_ERROR_FLAGS`: comma-separated categories promoted to errors.
   - Example: `same-scope-rebinding`
3. `JAZZ_WARNING_CONFIG`: path override for warning config file.

## Project Config Contract

Default config path:

- `.jazz-warnings` in workspace root (or process CWD when root detection is unavailable).

Config format requirements:

1. Tokens match CLI category names.
2. Tokens may be one-per-line or comma-separated.
3. `#` prefixes comments.

## Precedence Rules

Final warning behavior is resolved in this strict order:

1. CLI flags
2. Environment variables
3. Project config file
4. Default behavior (all warnings off, no warning-as-error)

## Diagnostic Message Contract

Minimum warning payload for `W0001`:

1. warning ID (`W0001`),
2. category (`same-scope-rebinding`),
3. binding name,
4. source span for rebind site,
5. source span for prior binding when available,
6. stable short message:
   - `same-scope rebinding: '<name>' shadows previous same-scope binding (last declaration wins)`

Minimum warning payload for `W0002`:

1. warning ID (`W0002`),
2. category (`shadowing-outer-scope`),
3. binding name,
4. source span for the nested binding or lambda-bearing binding,
5. source span for the visible outer binding when available,
6. stable short message:
   - `outer-scope shadowing: '<name>' shadows a visible binding from an outer scope`

Planned minimum warning payload for `W0003`:

1. warning ID (`W0003`),
2. category (`unused-binding`),
3. binding name,
4. source span for the unused ordinary block `let` binding,
5. stable short message:
   - `unused binding: '<name>' is never referenced in this lexical block`

The first queued `W0003` implementation batch is limited to ordinary block
`let` declarations in lexical block scopes. It warns only when
`unused-binding` is enabled and the declaration's name is not referenced by a
reachable expression in the same block. References from other bindings'
right-hand sides and expression statements in the same block count as usage; the
binding's own right-hand side does not. Lambda parameters, pattern binders, data
constructors, imports/modules, cross-module export analysis, and
`deprecated-syntax` remain outside this `W0003` batch.

## Warning-As-Error Contract

1. If a warning category is enabled and promoted to error, compilation exits non-zero when that warning occurs.
2. Warning-as-error does not change language semantics; it changes tool policy only.
3. The active compile path is diagnostic-only: warning-only success is distinguished by warnings plus no errors, and compile results do not include generated artifacts.
4. When warning-as-error triggers, the warning remains available as a warning record and is also promoted into compile errors.

## Migration Notes

Recommended rollout:

1. Start with default silent mode.
2. Enable `-Wsame-scope-rebinding` in CI as warning-only.
3. Optionally enable `-Wshadowing-outer-scope` to surface nested `let` and
   lambda-parameter shadowing without changing language semantics.
4. After the queued ordinary-`let` emitter lands, optionally enable
   `-Wunused-binding` to surface unused block bindings.
5. Address frequent warning sites.
6. Promote selected categories to `-Werror=<category>` once codebase is clean.

Compatibility notes:

1. Existing projects are unchanged unless they opt in.
2. Teams can adopt warning policies incrementally via config/env/CLI.

## Related Semantic Source

Rebinding semantics authority:

- `docs/spec/semantics/bindings-and-signatures.md`
