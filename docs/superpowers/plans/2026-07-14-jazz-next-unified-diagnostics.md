# Jazz-Next Unified Diagnostics Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use
> `superpowers:executing-plans` to implement this plan task-by-task. Steps use
> checkbox (`- [ ]`) syntax for tracking. This plan is intended for inline
> execution; do not dispatch subagents unless the user explicitly changes that
> choice.

**Goal:** Replace Jazz's parallel warning and error records with one cataloged,
ordered diagnostic model and reporting boundary while preserving compiler and
runtime semantics.

**Architecture:** `DiagnosticCatalog` owns typed published codes and warning
metadata. `Diagnostics` owns the constrained canonical report, and
`Diagnostics.Render` owns human output. Compiler phases transport ordered
diagnostic streams; driver compatibility functions filter those streams by
severity and origin without duplicating promoted warnings.

**Tech Stack:** Haskell 2010 with GHC 9.14.1, Cabal, the existing Jazz test
harness, `tasty-bench`, Nix development shell, and Markdown repository checks.

**Approved design:**
[`docs/superpowers/specs/2026-07-14-jazz-next-unified-diagnostics-design.md`](../specs/2026-07-14-jazz-next-unified-diagnostics-design.md)

## Global Constraints

- Modify compiler implementation only under `jazz-next/`; treat `jazz-hs/` and
  `jazz2/` as read-only.
- Do not change Jazz syntax, typing, module, warning-policy, or runtime
  semantics.
- Preserve existing `E####`, `W####`, and warning-token assignments.
- Give every currently uncoded user-facing diagnostic a catalog entry; do not
  retain a generic uncoded escape hatch.
- Render promoted warnings once as errors while retaining their warning code
  and category token.
- Preserve deterministic diagnostic and phase ordering.
- Do not add a universal rendering/conversion typeclass, JSON CLI output, a
  phase-specific diagnostic hierarchy, or a new package dependency.
- Use stronger types only for diagnostic/catalog invariants exposed by this
  batch.
- Keep significant embedded Jazz test programs in `MultilineStrings`; this
  batch must not reintroduce concatenated program strings.
- Follow test-driven development and commit each completed task.
- Implementation plans intentionally describe exact interfaces, behaviors,
  paths, commands, and expected outcomes without embedding exact production
  code, per the user's planning preference.
- Treat physical benchmark results as evidence. Benchmark validity and
  deterministic semantic correctness are hard gates; timing/allocation changes
  trigger investigation rather than an automatic percentage failure.

---

## File and Responsibility Map

- Create `jazz-next/src/JazzNext/Compiler/DiagnosticCatalog.hs`: typed error
  inventory, opaque diagnostic codes, severity metadata, warning categories,
  warning tokens, and deterministic catalog enumeration.
- Modify `jazz-next/src/JazzNext/Compiler/Diagnostics.hs`: canonical diagnostic,
  origin, labels, construction, promotion, filtering predicates, and mutation
  helpers that preserve invariants.
- Create `jazz-next/src/JazzNext/Compiler/Diagnostics/Render.hs`: stable human
  rendering for diagnostics, labels, spans, notes, and help.
- Create `jazz-next/src/JazzNext/Compiler/SignatureRendering.hs`: one canonical
  renderer for source `SignatureType` values.
- Delete `jazz-next/src/JazzNext/Compiler/WarningCatalog.hs` and
  `jazz-next/src/JazzNext/Compiler/Warnings.hs` after their responsibilities
  move to the catalog/configuration modules.
- Modify `jazz-next/src/JazzNext/Compiler/WarningConfig.hs`: warning token
  parsing and policy over catalog categories.
- Modify analyzer, inference, module, force, driver, and CLI modules listed in
  the tasks below to transport and report one diagnostic type.
- Add focused catalog and signature-rendering tests; evolve the existing
  structured-diagnostic, warning, driver, CLI, module, and runtime suites.

## Stable Interfaces

The implementation must provide these named concepts; exact constructor
visibility follows the approved design:

- Catalog: `ErrorCode`, `DiagnosticCode`, `DiagnosticSeverity`,
  `WarningCategory`, `DiagnosticMetadata`, `allDiagnosticMetadata`,
  `diagnosticCodeText`, `warningCode`, `warningToken`, and
  `warningHasAnalyzerEmitter`.
- Diagnostic model: `Diagnostic`, `DiagnosticOrigin`, `DiagnosticLabel`,
  `mkErrorDiagnostic`, `mkWarningDiagnostic`, `promoteDiagnostic`, severity and
  origin predicates, primary/secondary label helpers, note/help helpers, and
  read-only field accessors including `diagnosticCode`.
- Compile result: stored `compileDiagnostics` plus filtering functions
  `compileWarnings` and `compileErrors`.
- Run result: stored `runDiagnostics` plus filtering functions `runWarnings`,
  `runCompileErrors`, and `runRuntimeErrors`.
- Reporting: `renderDiagnostic` and `renderSourceSpan` from the reporting
  module.
- Signature rendering: `renderSignatureType` from the shared signature module;
  inferred-expression rendering remains `renderType` in inference diagnostics.

### Task 0: Capture the Pre-Change Performance Evidence

**Files:**

- Read: `jazz-next/PERFORMANCE.md`
- Read: `jazz-next/programs/corpus.json`
- Generated and ignored: `jazz-next/benchmark-results/batch4-unified-diagnostics/`

**Produces:** Two clean same-machine recorded runs for the
`identifier-classifier` case across every benchmark group, plus verified corpus
and smoke baselines. These artifacts are local evidence and are not committed.

- [ ] **Step 1: Verify the branch has no uncommitted production changes**

  Run `git status --short --branch` and record the current commit. Expected:
  only the already committed design and plan are ahead of `origin/main`, with
  no unstaged or staged files.

- [ ] **Step 2: Run the deterministic corpus baseline**

  Run
  `nix --extra-experimental-features 'nix-command flakes' develop -c cabal test program-corpus-spec --project-dir=jazz-next --test-show-details=failures`.
  Expected: PASS with all corpus outputs and semantic budgets satisfied.

- [ ] **Step 3: Run the benchmark-stage smoke baseline**

  Run
  `nix --extra-experimental-features 'nix-command flakes' develop -c cabal bench jazz-next-bench --project-dir=jazz-next --benchmark-options='--jazz-smoke'`.
  Expected: one successful `SMOKE` result for each registered benchmark group.

- [ ] **Step 4: Record two comparable CPU-time baseline runs**

  Run the following command twice, without changing the worktree between runs:
  `nix --extra-experimental-features 'nix-command flakes' develop -c cabal bench jazz-next-bench --project-dir=jazz-next --benchmark-options='--environment-label=batch4-unified-diagnostics --jazz-case=identifier-classifier --time-mode=cpu'`.
  Expected: each run prints a distinct `RECORDED` artifact directory under the
  same environment label, and each directory contains `results.csv` and
  `environment.json`.

- [ ] **Step 5: Confirm baseline compatibility and variance context**

  Compare the two environment documents using the compatibility fields defined
  in `JazzNext.Benchmark.Metadata`; Git revision and timestamp may differ only
  where documented as identifiers. Inspect the two CSVs to establish normal
  same-machine run-to-run variance for later evidence. Expected: compatible
  metadata and complete rows for all five `identifier-classifier` groups.

### Task 1: Establish the Unified Diagnostic Catalog

**Files:**

- Create: `jazz-next/src/JazzNext/Compiler/DiagnosticCatalog.hs`
- Delete: `jazz-next/src/JazzNext/Compiler/WarningCatalog.hs`
- Delete: `jazz-next/src/JazzNext/Compiler/Warnings.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Diagnostics.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/WarningConfig.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Analyzer.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Analyzer/UnusedBindings.hs`
- Modify: `jazz-next/src/JazzNext/CLI/Main.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Config/WarningConfigSpec.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Semantics/RebindingWarningSpec.hs`
- Create: `jazz-next/test/JazzNext/Compiler/Diagnostics/DiagnosticCatalogSpec.hs`
- Modify: `jazz-next/jazz-next.cabal`

**Produces:** One source of truth for all existing codes and warning metadata.
The exact existing error inventory is `E0001`-`E0005`, `E1001`-`E1007`,
`E1010`, `E2001`-`E2017`, `E3001`-`E3003`, `E3006`-`E3039`, and
`E4001`-`E4015`. The warning inventory remains `W0001`-`W0004`.

- [ ] **Step 1: Add failing catalog coverage**

  Register `diagnostic-catalog-spec` and test deterministic enumeration, unique
  code text, `E####`/`W####` formatting, default severity, warning category/token
  round trips, analyzer-emitter metadata, and the exact published inventory.
  Expected before implementation: the suite fails to compile because
  `DiagnosticCatalog` does not exist.

- [ ] **Step 2: Run the failing catalog suite**

  Run
  `nix --extra-experimental-features 'nix-command flakes' develop -c cabal test diagnostic-catalog-spec --project-dir=jazz-next --test-show-details=failures`.
  Expected: FAIL for the missing catalog interface, not for an unrelated test
  harness problem.

- [ ] **Step 3: Implement the catalog and migrate warning ownership**

  Move warning categories, codes, tokens, and emitter metadata into the new
  catalog. Add the typed existing error inventory and deterministic metadata
  enumeration. Move case-insensitive warning-token lookup to the catalog/config
  boundary. Update all listed imports and remove both obsolete warning modules
  rather than leaving re-export shims.

- [ ] **Step 4: Verify catalog and warning behavior**

  Run
  `nix --extra-experimental-features 'nix-command flakes' develop -c cabal test diagnostic-catalog-spec warning-config-spec rebinding-warning-spec --project-dir=jazz-next --test-show-details=failures`.
  Expected: PASS; warning tokens/codes and default-disabled policy are unchanged.

- [ ] **Step 5: Build all targets and commit**

  Run
  `nix --extra-experimental-features 'nix-command flakes' develop -c cabal build all --project-dir=jazz-next`.
  Expected: PASS with no reference to `WarningCatalog` or `Warnings` in the
  Cabal module inventory. Commit as
  `refactor: centralize Jazz diagnostic catalog`.

### Task 2: Introduce Typed, Labeled Error Diagnostics

**Files:**

- Modify: `jazz-next/src/JazzNext/Compiler/Diagnostics.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Analyzer.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/ModuleResolver.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/ModuleRuntime.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Parser/Declaration.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Parser.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Parser/Expression.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Parser/Lexer.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Parser/Lower.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Parser/Pattern.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Parser/TokenParser.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/PreludeContract.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Runtime/Semantics.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/TypeInference/Diagnostics.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/WarningConfig.hs`
- Modify: `jazz-next/src/JazzNext/CLI/Main.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Diagnostics/StructuredErrorDiagnosticsSpec.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Modules/ModuleResolutionSpec.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Parser/DeclarationParserSpec.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Parser/ExpressionParserSpec.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Parser/PatternParserSpec.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Parser/TokenParserSpec.hs`
- Modify: `jazz-next/test/JazzNext/CLI/CLISpec.hs`
- Modify: `jazz-next/test/JazzNext/TestHarness.hs`

**Consumes:** Typed `ErrorCode` and catalog metadata from Task 1.

**Produces:** A constrained `Diagnostic` error representation with typed codes,
origins, labeled spans, notes, help, and no generic uncoded constructor.

- [ ] **Step 1: Add failing model and uncoded-error tests**

  Extend structured-diagnostic coverage for typed code rendering, compile,
  runtime, and tooling origins, labeled primary/secondary spans, ordered notes,
  help text. Add behavior tests that expect module
  path failures to use `E4016` and tooling failures to use the assignments
  below. Expected before implementation: compile failures for missing model
  interfaces and assertion failures for newly coded messages.

- [ ] **Step 2: Lock the new uncoded-diagnostic assignments**

  Add these entries to the catalog and tests:

  - `E4016`: invalid or empty module entry path;
  - `E5001`: invalid warning configuration or warning category;
  - `E5002`: invalid or conflicting CLI arguments;
  - `E5003`: explicit warning-configuration file read failure;
  - `E5004`: source input read failure; and
  - `E5005`: requested runtime-profile production or write failure.

- [ ] **Step 3: Run focused tests to verify the red state**

  Run
  `nix --extra-experimental-features 'nix-command flakes' develop -c cabal test structured-error-diagnostics-spec module-resolution-spec warning-config-spec cli-spec --project-dir=jazz-next --test-show-details=failures`.
  Expected: FAIL only at the new typed/labeled/code expectations.

- [ ] **Step 4: Implement the canonical error model and migrate producers**

  Hide raw diagnostic construction, retain read-only selectors, and make every
  listed producer choose a typed code and origin explicitly. Convert the old
  primary/related span helpers onto labeled primary/secondary storage while
  keeping source-span behavior stable. Add span-aware token-parser failure
  helpers and migrate parser summaries that embed rendered coordinates so the
  coordinates live in labels instead. Remove `mkMessageDiagnostic` and the
  `Text` conversion instance. Preserve existing explanatory wording and code
  assignments except for normalized coordinate placement and the six approved
  additions. Keep `DiagnosticCatalog` and `Diagnostics` independent of parser,
  inference, module, and runtime detail types so future phase-specific
  diagnostic ADTs can convert into the common model without reversing the
  dependency direction.

- [ ] **Step 5: Verify the affected compiler families**

  Run the focused command from Step 3, then run
  `nix --extra-experimental-features 'nix-command flakes' develop -c cabal test declaration-parser-spec token-parser-spec binding-signature-coherence-spec purity-semantics-spec runtime-semantics-spec loader-spec --project-dir=jazz-next --test-show-details=failures`.
  Expected: PASS with unchanged semantic outcomes and source locations.

- [ ] **Step 6: Build all targets and commit**

  Run the full all-target build. Expected: PASS, with no raw-text diagnostic
  constructor remaining in exported APIs. Commit as
  `refactor: type and label Jazz errors`.

### Task 3: Replace Warning Records Throughout Compiler Transport

**Files:**

- Modify: `jazz-next/src/JazzNext/Compiler/Diagnostics.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Analyzer.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Analyzer/UnusedBindings.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/TypeInference.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/ModuleInterface.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/ModuleCompiler.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Force.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Driver.hs`
- Modify: `jazz-next/src/JazzNext/CLI/Main.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Semantics/RebindingWarningSpec.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Config/WarningConfigSpec.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Modules/ModulePipelineContractSpec.hs`
- Modify: `jazz-next/test/JazzNext/TestHarness.hs`

**Consumes:** Canonical typed diagnostics and catalog-backed warning metadata.

**Produces:** Warning-origin `Diagnostic` values and one diagnostic type across
analysis, inference, compiled prelude/module/program, forcing, and driver input
boundaries. `WarningRecord` no longer exists.

- [ ] **Step 1: Add failing warning-shape and promotion tests**

  Change warning tests to assert severity, category, code, subject, primary
  label, and prior-binding secondary label directly on `Diagnostic`. Add a
  model-level test proving promotion changes severity without changing code,
  category, labels, summary, notes, or help. Expected before implementation:
  type failures because warning producers still return `WarningRecord`.

- [ ] **Step 2: Run focused warning suites to verify failure**

  Run
  `nix --extra-experimental-features 'nix-command flakes' develop -c cabal test structured-error-diagnostics-spec warning-config-spec rebinding-warning-spec --project-dir=jazz-next --test-show-details=failures`.
  Expected: FAIL at the new canonical-warning interfaces.

- [ ] **Step 3: Migrate warning producers and internal result structures**

  Replace same-scope rebinding, outer-scope shadowing, and unused-binding
  records with warning diagnostics. Consolidate analyzer, inference, compiled
  prelude, compiled module, and compiled program warning/error transport into
  ordered diagnostic lists. Update forcing functions to force the canonical
  report once. Keep phase order and existing deterministic warning ordering.

- [ ] **Step 4: Update warning policy integration**

  Apply enable/disable policy before diagnostics enter results. Use the
  canonical promotion operation for enabled `-Werror` categories; do not append
  a second error copy. Keep default-disabled and reserved `W0004` behavior.

- [ ] **Step 5: Verify warning, module, and forcing paths**

  Run the focused warning suites plus
  `module-pipeline-contract-spec`, `loader-spec`, `program-corpus-spec`, and
  `benchmark-stage-spec`. Expected: PASS and no `WarningRecord` type remains.

- [ ] **Step 6: Build all targets and commit**

  Run the full all-target build. Expected: PASS with one diagnostic type in
  compiler transport. Commit as
  `refactor: unify warning diagnostic transport`.

### Task 4: Store One Ordered Stream in Compile and Run Results

**Files:**

- Modify: `jazz-next/src/JazzNext/Compiler/Driver.hs`
- Modify: `jazz-next/src/JazzNext/CLI/Main.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Semantics/RebindingWarningSpec.hs`
- Modify: `jazz-next/test/JazzNext/CLI/CLISpec.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Modules/LoaderSpec.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Modules/Loader/DiagnosticsTests.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Runtime/Observation/StatisticsTests.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Runtime/Observation/ProfileTests.hs`
- Modify: `jazz-next/test/JazzNext/ProgramCorpus/ProgramCorpusSpec.hs`

**Produces:** `CompileResult` with stored `compileDiagnostics`, `RunResult` with
stored `runDiagnostics`, and compatibility filtering functions with the names
locked in Stable Interfaces.

- [ ] **Step 1: Add failing result-stream behavior tests**

  Cover warning-only compile success, native compile failure, promoted warning,
  runtime failure after clean compilation, and successful observed execution.
  Assert exact stream order and compatibility-filter membership. Add a CLI
  regression proving a promoted warning appears exactly once.

- [ ] **Step 2: Run driver-facing suites to verify failure**

  Run
  `nix --extra-experimental-features 'nix-command flakes' develop -c cabal test rebinding-warning-spec cli-spec loader-spec runtime-observation-spec program-corpus-spec --project-dir=jazz-next --test-show-details=failures`.
  Expected: FAIL because result records still expose parallel storage.

- [ ] **Step 3: Implement one-stream result storage and filters**

  Preserve the public convenience names as functions, not duplicate record
  fields. Filter warning/error membership by effective severity and compile/run
  membership by diagnostic origin. Preserve compile-before-runtime ordering and
  prevent evaluation after fatal compile diagnostics.

- [ ] **Step 4: Remove promoted-warning duplication at the driver boundary**

  Transform the warning diagnostic in place before storing it. Verify that a
  promoted warning is returned by error filters, excluded from warning filters,
  remains cataloged as `W####`, and produces a failing exit status.

- [ ] **Step 5: Verify result and runtime behavior, then commit**

  Run the suites from Step 2 plus `module-pipeline-contract-spec` and
  `runtime-semantics-spec`. Expected: PASS with identical successful program
  output, runtime observation, and exit behavior. Commit as
  `refactor: store ordered diagnostic result streams`.

### Task 5: Move Human Rendering to the Reporting Boundary

**Files:**

- Create: `jazz-next/src/JazzNext/Compiler/Diagnostics/Render.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Diagnostics.hs`
- Modify: `jazz-next/src/JazzNext/CLI/Main.hs`
- Modify: `jazz-next/test/JazzNext/TestHarness.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Diagnostics/StructuredErrorDiagnosticsSpec.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Config/WarningConfigSpec.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Semantics/RebindingWarningSpec.hs`
- Modify: `jazz-next/test/JazzNext/CLI/CLISpec.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Bootstrap/CanonicalLexerComparisonSpec.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Modules/Loader/BasicTests.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Modules/Loader/CapabilitiesTests.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Modules/Loader/DiagnosticsTests.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Modules/Loader/OperatorsTests.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Modules/Loader/VisibilityTests.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Modules/ModulePipelineContractSpec.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Modules/ModuleResolutionSpec.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Modules/PreludeLoadingSpec.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Parser/ExpressionParserSpec.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Parser/PatternParserSpec.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Parser/TestSupport.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Parser/TokenParserSpec.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Semantics/BindingSignature/GeneralizationTests.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Semantics/BindingSignature/Shared.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Semantics/BuiltinCatalogSpec.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Semantics/LambdaSemanticsSpec.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Semantics/PrimitiveSemantics/EqualityOperator.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Semantics/Runtime/CapabilitiesTests.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Semantics/Runtime/NumericTests.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Semantics/Runtime/RecursionTests.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Semantics/Runtime/RenderingTests.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Semantics/Runtime/ScopeTests.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Semantics/Runtime/Shared.hs`
- Modify: `jazz-next/test/JazzNext/ProgramCorpus/ProgramCorpusSpec.hs`
- Modify: `jazz-next/test/JazzNext/Repository/AuditSpec.hs`
- Modify: `jazz-next/jazz-next.cabal`

**Produces:** One human diagnostic renderer used only at reporting/test
boundaries. The old `RenderDiagnostic` conversion class and CLI-specific
warning formatter are absent.

- [ ] **Step 1: Add failing rendering-boundary tests**

  Lock warning lines, native error lines, promoted warning lines, labeled
  secondary spans, notes, help, source-qualified spans, and previously uncoded
  tooling errors. Assert that severity is rendered once and warning tokens are
  retained after promotion.

- [ ] **Step 2: Run rendering and CLI suites to verify failure**

  Run
  `nix --extra-experimental-features 'nix-command flakes' develop -c cabal test structured-error-diagnostics-spec cli-spec warning-config-spec rebinding-warning-spec --project-dir=jazz-next --test-show-details=failures`.
  Expected: FAIL at the new reporting-module imports and full-line expectations.

- [ ] **Step 3: Implement the reporting module and update call sites**

  Move span and diagnostic text rendering out of the model. Make the CLI render
  ordered diagnostics uniformly, removing manual `error:` prefixing,
  `formatWarningLine`, and `renderPreviousSpan`. Make test helpers accept
  concrete `Diagnostic` errors instead of a conversion class. Keep
  context-sensitive runtime/statistics renderers separate.

- [ ] **Step 4: Verify user-visible output**

  Run the focused suites, then `module-resolution-spec`, `loader-spec`,
  `runtime-semantics-spec`, and `program-corpus-spec`. Expected: existing coded
  messages remain stable; the only intentional differences are single promoted
  warnings, new `E4016`/`E5001`-`E5005` prefixes, and normalized placement of
  parser source coordinates through diagnostic labels.

- [ ] **Step 5: Build all targets and commit**

  Run the full all-target build. Expected: PASS with no `RenderDiagnostic`,
  `formatWarningLine`, or presentation logic in semantic producers. Commit as
  `refactor: centralize Jazz diagnostic rendering`.

### Task 6: Consolidate Signature Rendering and Remove Trivial Aliases

**Files:**

- Create: `jazz-next/src/JazzNext/Compiler/SignatureRendering.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/CapabilityFacts.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/TypeInference/Capabilities.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/TypeInference/Diagnostics.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Name.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Parser/Signature.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Modules/ModulePipelineContractSpec.hs`
- Create: `jazz-next/test/JazzNext/Compiler/Diagnostics/SignatureRenderingSpec.hs`
- Modify: `jazz-next/jazz-next.cabal`

**Produces:** One canonical source-signature renderer, separate inferred-type
rendering, and removal of `builtinName` and
`surfaceSignaturePayloadFromType` only.

- [ ] **Step 1: Add failing shared signature-rendering tests**

  Register `signature-rendering-spec` and cover primitives, numeric types,
  named applications, nested lists/tuples, and right-associative function types
  with required parentheses. Expected before implementation: missing shared
  module/interface failure.

- [ ] **Step 2: Run the new test to verify failure**

  Run
  `nix --extra-experimental-features 'nix-command flakes' develop -c cabal test signature-rendering-spec --project-dir=jazz-next --test-show-details=failures`.
  Expected: FAIL because `SignatureRendering` does not exist.

- [ ] **Step 3: Extract canonical signature rendering**

  Move the duplicated `SignatureType` renderer to the shared module and update
  capability-fact keys and diagnostic messages to use it. Retain the separate
  inferred `ExpressionType` renderer and all runtime/name/module renderers.

- [ ] **Step 4: Remove the two approved trivial aliases**

  Replace the sole `builtinName` test use with `BuiltinName`, remove its export
  and definition, and replace the parser-local signature-payload alias with its
  constructor. Retain `sourceName`, `qualifiedName`, `generatedName`, and the
  resolved-origin helpers.

- [ ] **Step 5: Verify rendering and semantics, then commit**

  Run `signature-rendering-spec`, `binding-signature-coherence-spec`,
  `builtin-catalog-spec`, `module-pipeline-contract-spec`, and
  `runtime-semantics-spec`. Expected: PASS with byte-identical capability keys
  and diagnostic type text. Commit as
  `refactor: share Jazz signature rendering`.

### Task 7: Close Documentation, Audit, and Performance Evidence

**Files:**

- Modify: `jazz-next/test/JazzNext/Repository/AuditSpec.hs`
- Modify: `jazz-next/README.md`
- Modify: `docs/spec/tooling/compiler-warning-flags.md`
- Modify: `docs/spec/modules/03-loader-behavior-and-diagnostics.md`
- Modify: `docs/spec/modules/05-migration-and-compatibility.md`
- Modify: `docs/jazz-improvement-backlog.md`
- Preserve unchanged as historical evidence:
  `docs/plans/2026-03-16-structured-error-diagnostics.md`
- Generated and ignored: final runs under
  `jazz-next/benchmark-results/batch4-unified-diagnostics/`

**Produces:** Active documentation aligned with the unified model, Batch 4
marked complete, structural regression coverage, complete verification, and a
before/after benchmark evidence summary.

- [ ] **Step 1: Add failing repository/catalog boundary assertions**

  Extend behavior-oriented audit coverage for the registered diagnostic suites
  and module inventory. Prefer compilation and catalog enumeration checks over
  broad source-string bans. Expected red state: the audit identifies any
  obsolete warning module registration or missing new suite.

- [ ] **Step 2: Update active documentation**

  Document the one-stream result model, severity/category promotion behavior,
  catalog ranges including `E4016` and `E5001`-`E5005`, and reporting-boundary
  rendering. Mark Batch 4 completed with links to the design, implementation
  plan, and relevant active docs. Do not rewrite the older completed plan as a
  current specification.

- [ ] **Step 3: Run focused and full correctness gates**

  Run all focused suites from Tasks 1-6, then run
  `nix --extra-experimental-features 'nix-command flakes' develop -c cabal test all --project-dir=jazz-next --test-show-details=failures`.
  Expected: every registered suite passes.

- [ ] **Step 4: Run build, package, docs, queue, and diff gates**

  Run
  `nix --extra-experimental-features 'nix-command flakes' develop -c cabal build all --project-dir=jazz-next`,
  then run `nix --extra-experimental-features 'nix-command flakes' develop -c cabal check`
  with `jazz-next/` as the working directory, followed by
  `bash scripts/check-docs.sh`, `bash scripts/check-execution-queue.sh`, and
  `git diff --check` from the repository root. Expected: all commands pass; the
  known docs warning about external Prettier is non-fatal only when the checker
  exits successfully.

- [ ] **Step 5: Re-run deterministic and smoke performance gates**

  Run `program-corpus-spec` and benchmark `--jazz-smoke` exactly as in Task 0.
  Expected: semantic budgets and every stage boundary pass.

- [ ] **Step 6: Record two final compatible benchmark runs**

  Run the recorded benchmark command from Task 0 twice with the same
  `batch4-unified-diagnostics` label and selected case. Validate metadata
  compatibility, compare all five groups against the two pre-change runs, and
  report the delta relative to observed run-to-run variance. Expected: complete
  valid evidence; any reproducible regression is investigated and explained,
  not judged against an automatic percentage failure.

- [ ] **Step 7: Commit closeout and verify cleanliness**

  Commit documentation, audit, and any final verified cleanup as
  `docs: close unified diagnostics batch`. Run `git status --short --branch`.
  Expected: no tracked changes remain; ignored benchmark artifacts do not enter
  the commit.
