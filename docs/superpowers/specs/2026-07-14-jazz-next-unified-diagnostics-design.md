# Jazz-Next Unified Diagnostics and Rendering Design

## Status

Approved and implemented on `2026-07-14`.

This is the design checkpoint for Batch 4 of
[`docs/jazz-improvement-backlog.md`](../../jazz-improvement-backlog.md). It
covers items 14, 13, 11, and 12 only: a unified diagnostic model, focused
rendering consolidation, removal of unjustified Haskell aliases, and stronger
types where this refactor exposes a concrete invariant.

## Decision Summary

Batch 4 replaces the parallel `Diagnostic` and `WarningRecord` presentation
records with one canonical diagnostic representation. Compiler and run results
store ordered diagnostic streams rather than separately storing warnings and
errors. Compatibility accessors filter those streams by effective severity and
origin.

Each diagnostic carries an effective severity, a stable catalog code, an
optional warning category, a broad origin, a summary and optional subject,
labeled source spans, notes, and help text. Native errors are always fatal.
Warning-origin diagnostics may be promoted to errors without losing their
warning category or being duplicated in output.

A shared catalog owns every published `E####` and `W####` code. Rendering is a
reporting-boundary concern rather than a responsibility of semantic compiler
code. The design deliberately permits future parser-, inference-, module-, and
runtime-specific diagnostic ADTs to convert into the canonical report without
making the shared layer depend on those phase-specific types.

## Goals

- Make warnings and errors instances of one structured user-facing diagnostic
  model.
- Preserve diagnostic order while eliminating parallel warning/error storage
  and promoted-warning duplication.
- Centralize published codes, warning categories, warning tokens, and default
  severities.
- Support one primary labeled span, multiple secondary labeled spans, notes,
  and help without embedding presentation punctuation in compiler phases.
- Render diagnostics uniformly at CLI, test, and future machine-reporting
  boundaries.
- Remove rendering duplication when the same domain has one canonical textual
  representation.
- Remove trivial aliases that express no semantic transition while retaining
  helpers that communicate phase or origin ownership.
- Use stronger Haskell types only where they prevent invalid diagnostic states
  or catalog drift.
- Preserve a straightforward migration to phase-specific diagnostic ADTs if
  richer structured payloads become useful later.
- Use the Batch 3 corpus and benchmark tooling to detect semantic or physical
  performance regressions.

## Non-Goals

- No Jazz syntax, type-system, module, warning-policy, or runtime semantic
  change is introduced.
- No source-level diagnostic directives are added; that remains Batch 6 and
  depends on this model.
- No JSON CLI mode, editor protocol, fix-it application engine, or language
  server is added merely to demonstrate extensibility.
- No universal `Renderable`, `Pretty`, or diagnostic-conversion typeclass is
  introduced.
- No phase-specific error hierarchy is added before a phase has structured
  data that consumers need beyond the canonical report.
- No GADT, type family, phantom phase, or unrelated language extension is added
  without a concrete invariant.
- Nothing under `jazz-hs/` or `jazz2/` is modified.

## Approaches Considered

### Minimal record merge

This approach would add severity and category fields to `Diagnostic`, replace
`WarningRecord`, and leave result storage, catalogs, and rendering largely
unchanged. It has the least immediate churn, but it preserves scattered code
literals, separate formatting paths, and duplicate promoted-warning storage.
It does not complete the accepted Batch 4 goal and is rejected.

### Unified diagnostic core and focused catalog

This is the chosen approach. One presentation-neutral report crosses compiler
and driver boundaries. A shared catalog owns stable metadata. Smart
construction prevents invalid native-error and promoted-warning combinations,
while explicit phase helpers keep semantic message construction near the
relevant domain.

This approach is intentionally compatible with a future phase-specific model.
The common diagnostic layer is a target representation, not a dependency that
phase-specific detail types must embed or inherit from.

### Fully typed phase-specific diagnostic families

This approach would create dedicated parser, analyzer, inference, module, and
runtime diagnostic ADTs immediately and convert each through a shared class.
It provides maximum payload precision but would expand Batch 4 into a larger
compiler-wide redesign before JSON, editor, or automated-repair consumers need
those payloads. It remains a valid additive future direction and is not part of
this batch.

## Canonical Diagnostic Model

### Common report

The canonical diagnostic is a presentation-neutral report with these logical
fields:

- effective severity: warning or error;
- stable diagnostic code;
- optional warning category, retained after promotion;
- broad origin: compilation, runtime, or tooling/configuration;
- summary and optional subject;
- optional labeled primary span;
- zero or more labeled secondary spans;
- notes; and
- help text.

The model does not encode terminal prefixes, punctuation, parenthetical notes,
or line breaks. Those belong to renderers.

### Invariants and construction

The raw representation is not an unrestricted record-construction API.
Focused constructors preserve these invariants:

- a native error has error severity and no warning category;
- a warning begins with warning severity and a catalog-backed warning category;
- promotion changes only effective severity;
- a promoted warning retains its `W####` code and configuration token;
- every emitted diagnostic has a catalog-backed code; and
- primary and secondary labels retain their roles without being flattened into
  free-form notes.

Configuration code may decide whether a warning-origin diagnostic is emitted
and whether it is promoted. It does not clone a diagnostic into separate
warning and error records.

### Future phase-specific diagnostics

The common module must not import parser, analyzer, inference, module, runtime,
or CLI-specific diagnostic detail types. A future subsystem may define its own
ADT and an explicit conversion into the canonical report. If several such
types later need genuinely polymorphic consumers, a narrowly named conversion
typeclass may be introduced then.

Adding a phase-specific type must not require changing result storage, catalog
metadata, text rendering, or CLI reporting. This keeps the fully typed option
additive rather than making it a later rewrite of Batch 4.

## Result and Driver Boundaries

### Compile results

`CompileResult` stores one ordered diagnostic stream. Warning and error access
remain available as filtering functions so existing call sites can express
their intent without depending on parallel storage.

A promoted warning appears once in the stream with error severity. It is
returned by the error filter and not the warning filter.

### Run results

`RunResult` also stores one ordered stream. Its diagnostics retain enough
origin information for compatibility functions to distinguish compile-time
and runtime errors. Compile diagnostics precede runtime diagnostics because
evaluation starts only after compilation succeeds.

Runtime observation and exit-status fields remain independent of diagnostic
storage. A runtime failure still preserves any observation report completed
before failure.

### Compiler-internal transport

Analyzer, inference, compiled-module, compiled-prelude, and compiled-program
structures transport canonical diagnostics rather than a mixture of warning
and error records. Phase-local helpers may filter or partition when needed,
but no layer becomes a second source of truth for effective severity.

## Diagnostic Catalog

A shared catalog replaces the warning-only catalog as the source of truth for
published diagnostic metadata. It owns:

- opaque diagnostic codes;
- rendered `E####` and `W####` spellings;
- default severity;
- warning category and configuration token where applicable; and
- a stable subsystem grouping used for maintenance and tests.

Compiler producers refer to named catalog entries rather than repeating raw
code text. `WarningCategory` remains a public configuration identity, but its
code and token derive from the same catalog entry and cannot drift
independently.

Existing code ranges remain stable:

- `E0###`: syntax, lexing, parsing, and lowering;
- `E1###`: static analysis;
- `E2###`: type inference and type-system diagnostics;
- `E3###`: runtime diagnostics;
- `E4###`: module resolution and module contracts;
- `W0###`: configurable warnings; and
- `E5###`: CLI, warning configuration, file I/O, profiling output, and other
  tooling diagnostics.

Currently uncoded module, warning-configuration, CLI, and I/O failures receive
codes in the appropriate existing or new range. Multiple message variants may
share a code only when they describe the same stable diagnostic category.

Catalog tests enforce unique rendered codes, valid formatting, metadata
consistency, warning-category coverage, and deterministic enumeration.

## Rendering Architecture

### Diagnostic rendering

A focused reporting module owns the stable human-readable form for severity,
code, warning token, summary, labeled spans, notes, and help. CLI paths render
the canonical stream uniformly instead of prepending error text and formatting
warnings separately.

Existing human-readable wording remains stable across these intentional
reporting changes:

- promoted warnings render once as errors while retaining their `W####` code
  and warning token; and
- previously uncoded diagnostics gain catalog code prefixes.

Legacy parser summaries that embed rendered coordinates move those coordinates
into primary or secondary diagnostic labels. Their explanatory wording remains
stable, but the reporting module normalizes source-location placement and
punctuation instead of preserving phase-local formatting.

The reporting module is also the future boundary for JSON or editor-specific
rendering. Those formats are not added in this batch.

### Removal of the existing conversion class

The current `RenderDiagnostic` class converts unrelated values into a
diagnostic; it does not render them. It is removed. `renderDiagnostic` accepts
the canonical diagnostic only, and test helpers construct explicit diagnostics
where necessary.

No replacement conversion class is introduced until multiple phase-specific
diagnostic types create a real polymorphic use case.

### Domain rendering audit

The audit found duplicated canonical rendering for `SignatureType` in
capability handling and type-inference diagnostics. That rendering moves to one
shared owner.

The following remain explicit because their output has different context or
meaning:

- inferred expression types versus source signature types;
- runtime values versus runtime types;
- source, qualified, resolved, builtin, and generated names;
- module paths and export selectors; and
- source spans and diagnostic labels.

A universal rendering class would obscure these distinctions without removing
the required exhaustive constructor handling, so none is added.

## Focused Haskell API Cleanup

The alias audit removes helpers that only repeat an exported constructor and do
not communicate a meaningful transition:

- `builtinName`; and
- the parser-local `surfaceSignaturePayloadFromType`.

The following helpers remain because their widespread construction sites use
them as phase and provenance vocabulary:

- `sourceName`;
- `qualifiedName`; and
- `generatedName`.

Resolved-name helpers remain because they encode current-module, imported, or
ambient-prelude origin rather than merely renaming a constructor.

The opaque diagnostic code and constrained diagnostic constructors are the
advanced Haskell structure justified by this batch. Further features are added
only if implementation reveals an invalid state that ordinary ADTs and hidden
constructors cannot express safely.

## Error Handling and Compatibility

- Native errors remain fatal independently of warning settings.
- Disabled warnings are absent rather than present with a hidden severity.
- Promoted warnings are fatal, retain warning identity, and are emitted once.
- Diagnostic ordering is deterministic and preserves phase order.
- Compile failure prevents evaluation and therefore prevents runtime
  diagnostics from appearing later in the same run.
- Runtime and tooling failures use the same report and renderer without being
  misclassified as configurable warnings.
- Existing warning configuration tokens and `W####` codes remain stable.
- Existing `E####` assignments remain stable.
- Parser source coordinates are carried by diagnostic labels rather than
  duplicated inside summary text.
- Compatibility accessors preserve common compiler and test call patterns, but
  the old parallel record fields are not retained as duplicate storage.

## Verification Strategy

Implementation follows test-driven slices:

1. catalog tests lock code uniqueness, format, metadata, warning coverage, and
   the new tooling/module assignments;
2. diagnostic-model tests lock construction invariants, promotion, labeled
   spans, notes, help, ordering, and human rendering;
3. warning analyzer and configuration tests migrate to canonical diagnostics;
4. compile and run result tests lock one-stream filtering and phase behavior;
5. CLI regressions lock warning, native-error, promoted-warning,
   configuration-error, runtime-error, and non-duplication behavior;
6. capability and type-inference tests lock shared signature rendering; and
7. repository audit coverage locks the intended catalog and API boundaries
   where the Haskell type system cannot do so directly.

The completion gate includes:

- focused diagnostics, warning, driver, CLI, module, and runtime suites;
- the complete Cabal test suite;
- a full build and `cabal check`;
- documentation and execution-queue checks;
- `git diff --check`; and
- a clean worktree after commits.

## Performance Verification

Before production changes, record a clean same-machine Batch 3 benchmark result
with a stable environment label. After the refactor, record a compatible result
with the same label and compare the compiler and whole-program groups. Git
revision and timestamp identify the runs but do not invalidate an otherwise
compatible comparison.

Also run:

- the deterministic program-corpus semantic budgets;
- benchmark smoke coverage for every registered stage; and
- the existing GHC profiling/build checks affected by diagnostic-rendering
  cost-centre changes.

Physical timings are review evidence rather than a flaky pass/fail threshold.
Benchmark crashes, incorrect results, incompatible comparison metadata, and
unreviewed deterministic-budget overruns are hard failures. A reproducible
timing or allocation regression larger than normal run-to-run variance is an
investigation trigger, not an automatic failure.

An intentional language feature or stronger correctness guarantee may
legitimately perform more work. Its batch should compare against its immediate
pre-change baseline, explain the measured cost, and update semantic budgets
through ordinary review when the added work is expected. An unexplained,
unrelated, or disproportionate regression remains unacceptable. Batch 4 adds
no language behavior, so a reproducible slowdown is suspicious and must be
understood, but it is still evaluated as evidence rather than against a fixed
percentage gate. Minor noise is reported with the environment and variance
context.

## Documentation and Closeout

After implementation:

- update active diagnostic and warning documentation to describe the unified
  model and code ranges;
- update tests and examples to use the canonical result/accessor vocabulary;
- mark Batch 4 complete in
  [`docs/jazz-improvement-backlog.md`](../../jazz-improvement-backlog.md);
- preserve earlier structured-error plans as historical evidence rather than
  rewriting them; and
- keep the live execution queue consistent without claiming this backlog batch
  is a bootstrap-parser queue child.
