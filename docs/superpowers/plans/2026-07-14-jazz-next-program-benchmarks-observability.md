# Jazz-Next Program Corpus, Benchmarks, and Observability Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add a shared realistic Jazz program corpus, stage and whole-program benchmarks, deterministic runtime-owned semantic statistics and Speedscope profiles, and separate GHC profiling workflows for the Haskell compiler.

**Architecture:** A versioned manifest under `jazz-next/programs/` owns production-shaped programs shared by correctness tests and `tasty-bench`. Runtime observation is implemented inside the evaluator and transported through the module runtime, driver, and CLI without changing ordinary execution. Semantic work budgets are deterministic test gates; physical timing, allocation, heap, and cost-centre results are environment-labelled diagnostic artifacts.

**Tech Stack:** GHC 9.14.1, Haskell 2010 plus existing local extensions, Cabal 3.x, `aeson` 2.x, `tasty-bench`, GHC cost-centre and eventlog profiling, Speedscope evented JSON, Jazz `.jz` programs.

**Design checkpoint:** [`docs/superpowers/specs/2026-07-14-jazz-next-program-benchmarks-observability-design.md`](../specs/2026-07-14-jazz-next-program-benchmarks-observability-design.md)

## Planning Convention

This plan deliberately specifies contracts, observable behavior, test cases, verification commands, file ownership, and commit boundaries without pasting implementation bodies. Small interface names and data shapes are included only where independently implemented versions could otherwise be incompatible. The implementing agent owns the exact Haskell and Jazz code within those boundaries.

## Global Constraints

- Modify compiler implementation only under `jazz-next/`; never modify `jazz-hs/` or `jazz2/`.
- Keep all corpus sources under `jazz-next/programs/`, outside the shipped `jazz-next/jazz/{stdlib,compiler}` source tree.
- Use external `.jz` files or constructed compiler/runtime values for behavior tests. A Jazz program may appear as a Haskell string only when the test directly exercises source spelling or whitespace, and those cases use `MultilineStrings` rather than concatenation.
- Keep corpus support out of the production compiler library. Compile it only into the corpus-test and benchmark components.
- Preserve every current unobserved runtime and driver behavior. Observation must be disabled by default.
- Count Jazz semantic work in the evaluator. Do not infer it from benchmark loops, Haskell heap behavior, or rendered output.
- Treat semantic construction counts as logical Jazz operations, never as physical allocations or bytes.
- Make deterministic budgets upper limits. An improvement must not fail because it performs less work.
- Keep wall-clock timing, RTS allocation, heap, and GHC profile comparisons non-gating and label their environment.
- Keep Jazz semantic profiles deterministic and byte-identical for the same program and build inputs.
- Emit partial observation results after runtime failure, but no runtime observation after compilation failure.
- Keep Jazz semantic profiles and GHC profiles separate in naming, storage, documentation, and CLI behavior.
- Use explicit forcing at benchmark boundaries; do not add broad `NFData` instances solely for benchmarking.
- Keep `--runtime-stats` output on stderr and profile output in the requested file so program stdout is unchanged.
- Use behavior tests rather than assertions over source text, implementation names, or incidental formatting.
- Use the repository Nix environment for all recorded build and test evidence.
- Commit after every task once its focused checks pass.

## File Structure

| Path | Responsibility |
| --- | --- |
| `jazz-next/programs/corpus.json` | Versioned case metadata, workload classes, feature tags, benchmark participation, and deterministic budgets. |
| `jazz-next/programs/*/*.jz` | Substantial multi-module Jazz workloads. |
| `jazz-next/programs/*/expected.stdout` | Exact expected program output. |
| `jazz-next/programs/README.md` | Corpus format, authoring rules, coverage, and budget-update policy. |
| `jazz-next/program-support/JazzNext/ProgramCorpus/Types.hs` | Manifest and validated-corpus domain types. |
| `jazz-next/program-support/JazzNext/ProgramCorpus/Manifest.hs` | Root discovery, JSON decoding, path safety, and aggregate validation. |
| `jazz-next/program-support/JazzNext/ProgramCorpus/Runner.hs` | Correctness execution and reusable stage-ready case inputs. |
| `jazz-next/test/JazzNext/ProgramCorpus/ProgramCorpusSpec.hs` | Manifest validation, correctness, and deterministic budget tests. |
| `jazz-next/test/fixtures/runtime-observation/*.jz` | Small external source fixtures whose callable names or module behavior matter to observation tests. |
| `jazz-next/benchmark/Main.hs` | `tasty-bench` component entrypoint and benchmark tree. |
| `jazz-next/benchmark/JazzNext/Benchmark/Stages.hs` | Setup/timed boundaries for parse, analysis, preparation, runtime, and whole-program groups. |
| `jazz-next/benchmark/JazzNext/Benchmark/Force.hs` | Focused result forcing for measured stages. |
| `jazz-next/benchmark/JazzNext/Benchmark/Metadata.hs` | Environment identity, compatibility checks, and result-artifact support. |
| `jazz-next/test/JazzNext/Benchmark/MetadataSpec.hs` | Pure metadata, compatibility, and temporary artifact behavior tests. |
| `jazz-next/src/JazzNext/Compiler/Profiling.hs` | Stable compiler-stage catalog and GHC eventlog bracketing. |
| `jazz-next/src/JazzNext/Compiler/Runtime/Observation.hs` | Observation request, strict semantic counters, report, and centralized recording operations. |
| `jazz-next/src/JazzNext/Compiler/Runtime/Observation/Render.hs` | Stable human and compact JSON statistics encoders. |
| `jazz-next/src/JazzNext/Compiler/Runtime/Observation/Profile.hs` | Deterministic logical call stack, event stream, and Speedscope encoding. |
| `jazz-next/test/JazzNext/Compiler/Runtime/ObservationSpec.hs` | Runtime-observation test entrypoint. |
| `jazz-next/test/JazzNext/Compiler/Runtime/Observation/StatisticsTests.hs` | Counter, disabled-mode, and failure-path behavior. |
| `jazz-next/test/JazzNext/Compiler/Runtime/Observation/ProfileTests.hs` | Call identity, logical clock, balance, determinism, and failure closure. |
| `jazz-next/cabal.project.profile-stages` | Manual stage/sub-stage cost-centre profiling preset. |
| `jazz-next/cabal.project.profile-hotspots` | Late automatic cost-centre profiling preset for hotspot discovery. |
| `jazz-next/PERFORMANCE.md` | Benchmarking, comparison, runtime observation, and GHC profiling guide. |
| `jazz-next/benchmark-results/` | Ignored physical benchmark artifacts. |
| `jazz-next/profile-results/` | Ignored Jazz and GHC profiling artifacts used during local analysis. |

## Stable Contracts

### Corpus contract

- The manifest schema has a versioned top level and stable case identifiers.
- Each case declares a safe relative directory, entry source, module root, expected termination, expected-output file, workload class, feature tags, benchmark groups, and semantic upper budgets.
- Validation accumulates every violation and sorts them by case and path.
- Supported workload classes are `fast` and `full`.
- Benchmark group names decode to a closed shared group type; the harness must register every value of that type.
- The runner exposes both end-to-end execution and already-loaded inputs for benchmark setup.

### Runtime observation contract

- Observation requests represent disabled, statistics, profile, and combined modes.
- An observation report contains strict semantic statistics, optional profile data, and a successful or failed runtime termination marker.
- Existing public entrypoints delegate to observed entrypoints with observation disabled.
- Recording operations accept closed enumerations for application, construction, cache, builtin, and host-operation categories; evaluator code does not mutate report fields directly.
- `RunResult` transports the report without making CLI rendering part of the compiler driver.
- Closure values carry a stable callable origin through a focused closure record rather than expanding the existing positional constructor.

### Profile contract

- The semantic clock advances once per evaluator machine transition.
- The root frame spans the entire observed runtime attempt.
- Named Jazz bindings use fully qualified identities; curried lambda stages use stable derived suffixes.
- Builtins, operators, constructors, methods, generated sections, and host operations use distinct stable categories.
- Every opened frame is closed on success or failure. Failed profiles are marked incomplete while remaining valid Speedscope evented JSON.
- Serialization uses a fixed field and frame order, contains no wall-clock timestamps or absolute paths, and is byte-identical across identical executions.

### Physical measurement contract

- Benchmark groups are `parse-lower`, `analysis`, `module-preparation`, `runtime`, and `whole-program`.
- Loading, parsing used as setup, compilation used as setup, and other prerequisite work stay outside each timed action.
- Results live beneath ignored roots and include an explicit environment label plus machine/build metadata.
- Comparison refuses incompatible environments unless the user deliberately overrides the check.
- GHC stage and hotspot builds use separate Cabal project presets and do not change the default build.

---

### Task 1: Establish the shared corpus contract and first end-to-end case

**Files:**
- Modify: `jazz-next/jazz-next.cabal`
- Create: `jazz-next/program-support/JazzNext/ProgramCorpus/Types.hs`
- Create: `jazz-next/program-support/JazzNext/ProgramCorpus/Manifest.hs`
- Create: `jazz-next/program-support/JazzNext/ProgramCorpus/Runner.hs`
- Create: `jazz-next/test/JazzNext/ProgramCorpus/ProgramCorpusSpec.hs`
- Create: `jazz-next/programs/corpus.json`
- Create: `jazz-next/programs/identifier-classifier/Main.jz`
- Create: `jazz-next/programs/identifier-classifier/expected.stdout`

**Interfaces:**
- Produce a validated corpus value that contains canonical in-root paths and decoded metadata.
- Produce aggregate manifest violations with stable rendering.
- Produce a case runner that returns termination, stdout, and observation data without invoking the CLI renderer.
- Register shared `program-support` source modules in a Cabal common stanza imported only by the corpus test and benchmark components.

- [ ] **Step 1: Add RED manifest and path-safety tests.**

Cover unknown schema versions, duplicate case identifiers/directories, invalid workload and feature names, missing files, absolute paths, parent traversal, and unregistered benchmark groups. Assert all applicable violations are returned in stable order.

- [ ] **Step 2: Add a RED checked-in corpus smoke test.**

Require the real manifest to load and the identifier-classifier case to execute through module discovery, compilation, and the runtime with exact stdout and successful termination.

- [ ] **Step 3: Run the focused suite and confirm RED.**

```sh
nix --extra-experimental-features 'nix-command flakes' develop -c \
  cabal test --project-dir=jazz-next program-corpus-spec --test-show-details=failures
```

Expected: the new component or corpus support modules are absent.

- [ ] **Step 4: Implement the manifest boundary.**

Add the schema types, decoding, validated tag vocabularies, package-root discovery, canonical containment checks, aggregate validation, and stable error rendering. Keep JSON and development-corpus dependencies out of `jazz-next-internal`.

- [ ] **Step 5: Implement the first realistic case and runner.**

Use multiple ordinary Jazz functions and list/text processing representative of `identifierKind` without copying a tiny unit fixture. Resolve files through the manifest and real module lookup path.

- [ ] **Step 6: Re-run the focused suite and confirm GREEN.**

Use the command from Step 3. Confirm malformed fixtures exercise validation behavior and the checked-in case passes end to end.

- [ ] **Step 7: Commit.**

```sh
git add jazz-next/jazz-next.cabal jazz-next/program-support jazz-next/test/JazzNext/ProgramCorpus jazz-next/programs
git commit -m "test: establish shared Jazz program corpus"
```

---

### Task 2: Fill the production-shaped corpus and feature coverage

**Files:**
- Modify: `jazz-next/programs/corpus.json`
- Modify: `jazz-next/test/JazzNext/ProgramCorpus/ProgramCorpusSpec.hs`
- Create: `jazz-next/programs/expression-evaluator/**`
- Create: `jazz-next/programs/tree-transformations/**`
- Create: `jazz-next/programs/dependency-planner/**`
- Create: `jazz-next/programs/capability-workflow/**`
- Create: `jazz-next/programs/mini-frontend/**`

**Interfaces:**
- Preserve the Task 1 schema and runner.
- Associate every required language feature with at least one case through validated tags.
- Give `fast` and `full` meaningful workloads without changing program semantics between test and benchmark use.

- [ ] **Step 1: Add RED coverage and completeness tests.**

Require all six stable case identifiers, every approved feature tag, at least one multi-module case, both workload classes, exact output for every case, and a registered benchmark group for every declared participation entry.

- [ ] **Step 2: Confirm RED.**

Run `program-corpus-spec`; expect the missing programs and feature coverage to fail.

- [ ] **Step 3: Add the expression evaluator and generic tree transformations.**

Exercise recursive ADTs, nested patterns, explicit error values, generics, higher-order mapping/folding, and module boundaries. Keep all inputs deterministic.

- [ ] **Step 4: Add the dependency planner and capability workflow.**

Use an association-list graph and a deterministic capability-driven workflow. Exercise non-trivial traversal and capability dispatch without introducing a new stdlib abstraction in this batch.

- [ ] **Step 5: Add the mini frontend.**

Combine token classification, recursive syntax, analysis, and deterministic output across several modules. Make it large enough to expose pipeline costs while remaining quick enough for the normal correctness gate.

- [ ] **Step 6: Confirm GREEN and record the current semantic results for later budgets.**

Run the focused suite. At this stage, check output and termination; the declared budget fields are validated structurally and become enforced after runtime statistics are implemented in Task 6.

- [ ] **Step 7: Commit.**

```sh
git add jazz-next/programs jazz-next/test/JazzNext/ProgramCorpus
git commit -m "test: add production-shaped Jazz programs"
```

---

### Task 3: Lock the compiler-stage catalog and benchmark boundaries

**Files:**
- Modify: `jazz-next/jazz-next.cabal`
- Modify: `jazz-next/src/JazzNext/Compiler/Driver.hs`
- Create: `jazz-next/src/JazzNext/Compiler/Profiling.hs`
- Create: `jazz-next/benchmark/Main.hs`
- Create: `jazz-next/benchmark/JazzNext/Benchmark/Stages.hs`
- Create: `jazz-next/benchmark/JazzNext/Benchmark/Force.hs`

**Interfaces:**
- Produce a closed compiler-stage catalog with stable human and artifact names.
- Expose the existing compiled-program preparation boundary from the internal driver library for benchmark use; do not duplicate module resolution or compilation.
- Produce one benchmark group for each locked stage and one whole-program group.
- Keep each stage's prerequisite work in benchmark setup and force the measured result before ending the sample.

- [ ] **Step 1: Register a RED benchmark component and boundary tests.**

Add `tasty-bench` only to the benchmark component. Add a smoke mode that selects one fast case per stage, performs one measured evaluation, and reports which corpus case and compiler stage it ran.

- [ ] **Step 2: Confirm RED.**

```sh
nix --extra-experimental-features 'nix-command flakes' develop -c \
  cabal bench --project-dir=jazz-next jazz-next-bench --benchmark-options='--jazz-smoke'
```

Expected: the benchmark component or stage registrations are missing.

- [ ] **Step 3: Implement the stable stage catalog.**

Cover source/module discovery, lex/parse/lower, resolution, analysis, type inference, constraint solving, capability solving, runtime preparation, evaluation/host work, and diagnostics. The coarser benchmark groups map to these stable names without erasing sub-stage identities used by GHC profiling later.

- [ ] **Step 4: Implement stage setup and forcing.**

Use standalone parse/lower for source inputs, existing inference APIs for analysis, the driver's compiled-program builder for module preparation, prebuilt compiled programs for runtime, and the public driver path for whole-program measurement. Add narrow force functions for each returned artifact.

- [ ] **Step 5: Confirm the smoke run and full benchmark discovery.**

Run the Step 2 command, then list the full benchmark tree and confirm every manifest-declared group has a registered case. Do not use timing assertions.

- [ ] **Step 6: Re-run corpus correctness.**

Confirm benchmark exposure did not change the compiler or corpus semantics.

- [ ] **Step 7: Commit.**

```sh
git add jazz-next/jazz-next.cabal jazz-next/src/JazzNext/Compiler/Driver.hs jazz-next/src/JazzNext/Compiler/Profiling.hs jazz-next/benchmark
git commit -m "bench: add staged Jazz benchmark harness"
```

---

### Task 4: Add environment-labelled benchmark artifacts and compatibility checks

**Files:**
- Modify: `jazz-next/benchmark/Main.hs`
- Create: `jazz-next/benchmark/JazzNext/Benchmark/Metadata.hs`
- Create: `jazz-next/test/JazzNext/Benchmark/MetadataSpec.hs`
- Modify: `.gitignore`

**Interfaces:**
- Require an explicit environment label when writing durable results.
- Write `results.csv` and `environment.json` beneath a label/run-specific directory under `jazz-next/benchmark-results/`.
- Include Git revision/dirty state, corpus schema and selected cases, GHC/Cabal/package versions, platform and architecture, explicit environment label, optimized or profiling build mode, RTS capabilities and arguments, benchmark arguments and time mode, and run timestamp. Include CPU identity when available.
- Compare only compatible metadata by default and return a precise mismatch report.

- [ ] **Step 1: Add RED pure metadata tests.**

Cover stable encoding, safe environment labels, required identity fields, exact compatible metadata, individual mismatch categories, multiple accumulated mismatches, and the explicit override path.

- [ ] **Step 2: Add a RED artifact smoke test.**

Run a fast benchmark with a temporary result root and require both files to be written without adding them to Git.

- [ ] **Step 3: Implement metadata and artifact ownership.**

Keep machine-dependent values out of benchmark names. Make result roots configurable for tests, use stable JSON field order, and report unavailable optional platform facts explicitly rather than silently omitting identity.

- [ ] **Step 4: Confirm GREEN.**

Run the metadata tests and one labelled smoke benchmark. Inspect that the CSV and JSON agree on the run label and benchmark selection.

- [ ] **Step 5: Commit.**

```sh
git add .gitignore jazz-next/benchmark
git commit -m "bench: label Jazz benchmark environments"
```

---

### Task 5: Build the runtime-observation state and core counters

**Files:**
- Modify: `jazz-next/jazz-next.cabal`
- Modify: `jazz-next/src/JazzNext/Compiler/Runtime/Types.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Runtime.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/ModuleRuntime.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Driver.hs`
- Create: `jazz-next/src/JazzNext/Compiler/Runtime/Observation.hs`
- Create: `jazz-next/test/JazzNext/Compiler/Runtime/ObservationSpec.hs`
- Create: `jazz-next/test/JazzNext/Compiler/Runtime/Observation/StatisticsTests.hs`
- Create as needed: `jazz-next/test/fixtures/runtime-observation/*.jz`

**Interfaces:**
- Produce observation request, termination, statistics, report, application-kind, and construction-kind domain types.
- Produce observed runtime, module-runtime, and driver entrypoints; preserve old entrypoints as disabled-mode delegates.
- Carry one strict observation state through nested evaluator activity and retain it after runtime failure.
- Count evaluator transitions, forced values, application categories, and maximum continuation depth first.

- [ ] **Step 1: Add RED disabled-mode and transport tests.**

Prove ordinary evaluation produces the same value, diagnostics, and output with observation disabled. Prove an observed driver run returns a report without requiring CLI rendering.

- [ ] **Step 2: Add RED exact micro-program counter tests.**

Use constructed runtime/core values or small external `.jz` fixtures to isolate machine transitions, forcing, closure versus builtin application, constructor saturation, and continuation-depth high-water behavior. Do not add embedded Jazz source strings. Assert relational invariants where incidental step totals would be brittle; assert exact totals only for deliberately minimal evaluator-machine tests.

- [ ] **Step 3: Add RED runtime-failure retention tests.**

Require a failing evaluated program to return its diagnostic plus a partial report whose termination is failed and whose step count is non-zero. Require compilation failure to produce no runtime report.

- [ ] **Step 4: Implement observation state and observed entrypoints.**

Initialize observation alongside the existing runtime host state. Record a transition at the central evaluator-machine step, update maximum continuation depth from the explicit stack, and classify applications at the central callable dispatcher. Ensure nested evaluation shares the same state.

- [ ] **Step 5: Preserve the unobserved fast path.**

Keep disabled recording inexpensive and retain the existing pure module-runtime shortcut where safe. Observed module evaluation must not escape into an unobserved shortcut.

- [ ] **Step 6: Confirm GREEN and run the existing runtime/driver suites.**

```sh
nix --extra-experimental-features 'nix-command flakes' develop -c \
  cabal test --project-dir=jazz-next runtime-observation-spec --test-show-details=failures
```

Then run the existing API-adjacent suites:

```sh
nix --extra-experimental-features 'nix-command flakes' develop -c \
  cabal test --project-dir=jazz-next \
    runtime-semantics-spec \
    module-pipeline-contract-spec \
    loader-spec \
    cli-spec \
    --test-show-details=failures
```

- [ ] **Step 7: Commit.**

```sh
git add jazz-next/jazz-next.cabal jazz-next/src/JazzNext/Compiler/Runtime.hs jazz-next/src/JazzNext/Compiler/Runtime jazz-next/src/JazzNext/Compiler/ModuleRuntime.hs jazz-next/src/JazzNext/Compiler/Driver.hs jazz-next/test/JazzNext/Compiler/Runtime jazz-next/test/fixtures/runtime-observation
git commit -m "feat: add Jazz runtime observation core"
```

---

### Task 6: Complete semantic statistics and enforce corpus budgets

**Files:**
- Modify: `jazz-next/src/JazzNext/Compiler/Runtime.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Runtime/Types.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Runtime/Observation.hs`
- Create: `jazz-next/src/JazzNext/Compiler/Runtime/Observation/Render.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Runtime/Observation/StatisticsTests.hs`
- Modify: `jazz-next/program-support/JazzNext/ProgramCorpus/Runner.hs`
- Modify: `jazz-next/test/JazzNext/ProgramCorpus/ProgramCorpusSpec.hs`
- Modify: `jazz-next/programs/corpus.json`

**Interfaces:**
- Add closure creation/capture width, list cell, tuple, saturated ADT, pattern, binding, builtin, host operation, and deferred-cache counters.
- Render statistics in stable human form and one compact deterministic JSON value.
- Return budget violations containing case, metric, upper limit, actual value, and percentage increase.

- [ ] **Step 1: Add RED category-isolation tests.**

Cover closure creation and captured widths, each logical construction category, case-arm attempts/successes/bindings, builtin versus host operations, and cache hit/miss/recursive outcomes. Include programs that distinguish zero, one, and multiple occurrences.

- [ ] **Step 2: Add RED renderer tests.**

Lock meaningful human labels and stable compact JSON fields. Assert decoding round trips, deterministic bytes, and explicit zero values for supported counters.

- [ ] **Step 3: Instrument semantic boundaries centrally.**

Record closure information at closure creation, constructions when runtime values become logically complete, pattern outcomes around the existing pure matcher, builtin/host work at their dispatch boundaries, and cache results in the existing cache helpers. Keep the matcher pure and avoid counting temporary Haskell values.

- [ ] **Step 4: Add RED budget-policy tests.**

Prove equal and lower actual values pass, higher values fail, optional metrics are enforced when present, and violations are accumulated and stably ordered. Include a deliberate over-budget checked-in case through an in-memory manifest override.

- [ ] **Step 5: Measure and set conservative checked-in budgets.**

Run every corpus case with statistics enabled, review the metrics for plausibility, and set transparent upper bounds above the measured deterministic totals. Do not generate or rewrite the manifest automatically.

- [ ] **Step 6: Confirm GREEN.**

Run `runtime-observation-spec` and `program-corpus-spec` twice. Confirm statistics and pass/fail results are identical across runs.

- [ ] **Step 7: Commit.**

```sh
git add jazz-next/src/JazzNext/Compiler/Runtime jazz-next/src/JazzNext/Compiler/Runtime.hs jazz-next/test/JazzNext/Compiler/Runtime jazz-next/program-support jazz-next/test/JazzNext/ProgramCorpus jazz-next/programs/corpus.json
git commit -m "feat: report deterministic Jazz runtime statistics"
```

---

### Task 7: Produce deterministic Jazz semantic Speedscope profiles

**Files:**
- Modify: `jazz-next/src/JazzNext/Compiler/Runtime/Types.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Runtime.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Runtime/Observation.hs`
- Create: `jazz-next/src/JazzNext/Compiler/Runtime/Observation/Profile.hs`
- Create: `jazz-next/test/JazzNext/Compiler/Runtime/Observation/ProfileTests.hs`
- Modify or create as needed: `jazz-next/test/fixtures/runtime-observation/*.jz`

**Interfaces:**
- Convert positional closure payloads into a focused closure record that includes stable callable origin.
- Carry enclosing callable identity and lambda-stage context while closures are created.
- Produce an ordered frame table and open/close events using the evaluator-transition clock.
- Encode valid Speedscope evented JSON and mark failed attempts incomplete.

- [ ] **Step 1: Add RED profile-format tests.**

Require the Speedscope schema marker, an evented profile, `unit` set to logical/no-time semantics, one root frame, ordered shared frames, balanced events, and an end value equal to the runtime step count.

- [ ] **Step 2: Add RED identity tests.**

Exercise fully qualified named bindings, successive curried stages, builtins, operators, constructors, methods, generated sections, and host operations through constructed values or external fixtures. Require stable display names and categories without source-file absolute paths.

- [ ] **Step 3: Add RED determinism and failure tests.**

Run the same program twice and compare encoded bytes. Run a failing program and require valid JSON, closed frames, failed/incomplete metadata, and a final logical time matching the partial statistics.

- [ ] **Step 4: Refactor closure representation around callable identity.**

Replace the positional closure payload with the focused record and update all construction, application, equality/show, and test sites. Derive identity from module-qualified let bindings and stable generated identities rather than pointer identity or source locations.

- [ ] **Step 5: Record semantic call-stack events.**

Open the root for the whole attempt, open callable frames at semantic entry, and close them at return or unwind. Use the same transition clock as statistics and finalize all outstanding events after failure.

- [ ] **Step 6: Encode deterministic Speedscope JSON.**

Use ordered encoding and deterministic frame interning. Exclude wall time, host timing, temporary paths, randomized identifiers, and unordered map traversal.

- [ ] **Step 7: Confirm GREEN and inspect a real corpus profile.**

Run the observation suite twice, compare a profile artifact byte for byte, and load one corpus profile with Speedscope-compatible validation or documented manual inspection.

- [ ] **Step 8: Commit.**

```sh
git add jazz-next/src/JazzNext/Compiler/Runtime jazz-next/src/JazzNext/Compiler/Runtime.hs jazz-next/test/JazzNext/Compiler/Runtime jazz-next/test/fixtures/runtime-observation
git commit -m "feat: add deterministic Jazz semantic profiles"
```

---

### Task 8: Expose runtime statistics and profiles through the CLI

**Files:**
- Modify: `jazz-next/src/JazzNext/CLI/Main.hs`
- Modify: `jazz-next/test/JazzNext/CLI/CLISpec.hs`
- Modify if component dependencies require it: `jazz-next/jazz-next.cabal`

**Interfaces:**
- Accept `--runtime-stats`, `--runtime-stats=human`, `--runtime-stats=json`, and `--runtime-profile=PATH` only with `--run`.
- Allow statistics and profile flags together.
- Keep program stdout unchanged, write statistics to stderr after diagnostics, and atomically replace the requested profile path.
- Provide an injectable profile writer for CLI behavior tests while keeping the ordinary CLI entrypoint unchanged.

- [ ] **Step 1: Add RED parsing and help tests.**

Cover default human statistics, explicit formats, missing/empty/unknown values, repeated compatible and conflicting flags, profile paths, combination of both modes, and rejection without `--run`.

- [ ] **Step 2: Add RED success-path output tests.**

Require identical program stdout with and without observation, no observation output by default, human/JSON statistics as the final stderr payload, and a valid requested profile file. Reuse external fixtures or the shared corpus rather than adding embedded Jazz programs to `CLISpec.hs`.

- [ ] **Step 3: Add RED failure-path tests.**

Require compilation failure to emit neither runtime statistics nor a profile; require runtime failure to emit diagnostics plus partial statistics/profile; require an atomic-write failure to become a structured CLI diagnostic without a partial destination file.

- [ ] **Step 4: Implement CLI selection and rendering.**

Translate flags into one runtime observation request, pass it through source and module execution paths, render only after the driver returns, and perform profile serialization outside the runtime.

- [ ] **Step 5: Implement atomic profile output.**

Write a sibling temporary file, flush/close it, and rename it into place. Ensure cleanup on all failures and make the write dependency injectable in tests.

- [ ] **Step 6: Confirm GREEN.**

Run `cli-spec` and `runtime-observation-spec`. Manually run one fast corpus program in human, JSON, profile, and combined modes and compare stdout.

- [ ] **Step 7: Commit.**

```sh
git add jazz-next/src/JazzNext/CLI/Main.hs jazz-next/test/JazzNext/CLI/CLISpec.hs jazz-next/jazz-next.cabal
git commit -m "feat: expose Jazz runtime observation flags"
```

---

### Task 9: Add separate GHC stage and hotspot profiling workflows

**Files:**
- Modify: `jazz-next/src/JazzNext/Compiler/Profiling.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/SourceProgram.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/ModuleResolver.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Analyzer.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/TypeInference.hs`
- Modify selected solver/capability modules only where a named sub-stage boundary exists
- Modify: `jazz-next/src/JazzNext/Compiler/ModuleCompiler.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/ModuleRuntime.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Runtime.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Driver.hs`
- Modify diagnostic rendering entrypoint if it has a measurable stage boundary
- Modify: `jazz-next/jazz-next.cabal`
- Create: `jazz-next/cabal.project.profile-stages`
- Create: `jazz-next/cabal.project.profile-hotspots`
- Modify: `.gitignore`

**Interfaces:**
- Use the Task 3 stage catalog for manual cost-centre labels and paired eventlog begin/end markers.
- Keep the manual stage preset free of broad automatic cost centres.
- Use Cabal's late top-level profiling detail for hotspot discovery.
- Allow ordinary executable RTS profiling flags while leaving the default non-profiled build unchanged.

- [ ] **Step 1: Add RED preset and stage-contract tests.**

Validate both project files through Cabal, require all stable stage names to be unique and non-empty, and add a smoke test that brackets a forced stage action with matched eventlog markers even when it fails.

- [ ] **Step 2: Confirm RED.**

Attempt a stage-profile build with the not-yet-created preset and confirm failure is confined to profiling setup.

- [ ] **Step 3: Add explicit stage cost centres and eventlog brackets.**

Place a small number of stable manual cost centres at real compiler phase/sub-phase entrypoints. Wrap IO boundaries that force stage results with paired eventlog markers. Do not annotate every helper or contort pure APIs merely to produce eventlog spans.

- [ ] **Step 4: Add the two Cabal project presets.**

The stage preset enables profiling with automatic profiling detail disabled. The hotspot preset enables late top-level cost centres for both the library and executable. Both inherit the local package and development flag without affecting `cabal.project`.

- [ ] **Step 5: Build and run stage profiling smoke evidence.**

Use the stage preset to run one fast corpus case with RTS time/allocation output, JSON cost-centre output, and an eventlog beneath `jazz-next/profile-results/ghc-stages/`. Confirm the JSON imports into Speedscope and the eventlog contains matched stage markers.

- [ ] **Step 6: Build and run hotspot/heap smoke evidence.**

Use the hotspot preset on the same case and produce a late-cost-centre profile plus at least one heap profile. Confirm expected compiler/runtime frames appear and keep the artifacts ignored.

- [ ] **Step 7: Re-run the default build.**

Confirm normal `cabal test` does not require profiled libraries or inherit profiling flags.

- [ ] **Step 8: Commit.**

```sh
git add .gitignore jazz-next/jazz-next.cabal jazz-next/cabal.project.profile-stages jazz-next/cabal.project.profile-hotspots jazz-next/src/JazzNext/Compiler
git commit -m "build: add GHC profiling workflows for Jazz"
```

---

### Task 10: Document the performance workflow and close repository integration

**Files:**
- Create: `jazz-next/programs/README.md`
- Create: `jazz-next/PERFORMANCE.md`
- Modify: `jazz-next/README.md`
- Modify: `docs/jazz-improvement-backlog.md`
- Modify: `jazz-next/jazz-next.cabal`
- Modify if required by repository audit: `jazz-next/test/JazzNext/Repository/AuditSpec.hs`

**Interfaces:**
- Document one source of truth for corpus authoring and one for measurement/profiling operation.
- Package corpus inputs in source distributions through Cabal `extra-source-files`.
- Explain deterministic semantic budgets versus environment-dependent physical measurements.
- Explain that Speedscope receives either a deterministic Jazz semantic profile or GHC's sampled JSON profile and how to distinguish them.

- [ ] **Step 1: Add RED repository-integration assertions.**

Require the source distribution to contain the manifest, every referenced `.jz` source, expected outputs, and corpus README. Require result/profile directories to remain ignored. If the repository audit already has general helpers for these behaviors, extend them instead of adding source-string checks.

- [ ] **Step 2: Write the corpus guide.**

Document the schema, stable identifiers, path constraints, workload classes, tag vocabulary, correctness expectations, deterministic inputs, benchmark participation, budget interpretation, review policy, and the process for adding a case.

- [ ] **Step 3: Write the performance guide.**

Document fast/full correctness runs, benchmark discovery and filtering, labelled result creation and compatibility, stage meanings, RTS allocation reporting, semantic statistics, semantic Speedscope profiles, GHC stage/hotspot presets, cost-centre JSON, eventlogs, heap profiles, artifact roots, and common interpretation mistakes.

- [ ] **Step 4: Update top-level Jazz-Next documentation and backlog state.**

Replace the stale statement that substantial fixtures and benchmarks have separate owning trees. Point readers to the shared corpus and performance guide. Mark Batch 3 items complete only after the final gate passes.

- [ ] **Step 5: Run the focused documentation and distribution checks.**

```sh
bash scripts/check-docs.sh
nix --extra-experimental-features 'nix-command flakes' develop -c \
  cabal sdist --project-dir=jazz-next
```

Inspect the source archive rather than merely trusting a successful `sdist` exit.

- [ ] **Step 6: Run the complete default verification gate.**

```sh
nix --extra-experimental-features 'nix-command flakes' develop -c \
  cabal test all --project-dir=jazz-next --test-show-details=failures
nix --extra-experimental-features 'nix-command flakes' develop -c \
  cabal build all --project-dir=jazz-next
```

Then run the benchmark smoke command, both profiling preset smoke runs, formatting, and the repository's standard lint/audit commands. Record timing/profile artifacts as evidence but do not commit them.

- [ ] **Step 7: Perform a final requirements review.**

Check every approved design requirement against behavior or documentation. Specifically verify disabled-mode compatibility, runtime-failure partial reports, compile-failure absence, byte-identical semantic profiles, budget upper-limit semantics, stage setup boundaries, metadata compatibility, and separation of semantic versus GHC profiles.

- [ ] **Step 8: Commit.**

```sh
git add jazz-next/programs/README.md jazz-next/PERFORMANCE.md jazz-next/README.md docs/jazz-improvement-backlog.md jazz-next/jazz-next.cabal jazz-next/test/JazzNext/Repository/AuditSpec.hs
git commit -m "docs: explain Jazz performance and profiling workflows"
```

## Final Self-Review Checklist

- [ ] No file under `jazz-hs/` or `jazz2/` changed.
- [ ] All substantial test/benchmark programs live under `jazz-next/programs/`; small observation fixtures are external `.jz` files rather than embedded Haskell strings.
- [ ] Program-support modules are absent from the production compiler library dependency surface.
- [ ] All six corpus workloads execute correctly and cover the approved language features.
- [ ] Every manifest path is contained within the corpus root and every violation is reported stably.
- [ ] All five benchmark groups use setup outside timed actions and explicit forcing inside them.
- [ ] Durable benchmark results include explicit environment identity and refuse incompatible comparisons by default.
- [ ] Observation disabled mode preserves existing runtime results, diagnostics, output, and the intended fast path.
- [ ] Semantic counters are recorded at their owning runtime boundaries and do not claim physical allocation meaning.
- [ ] Runtime failures retain partial statistics/profiles; compile failures produce neither.
- [ ] Corpus budgets are deterministic upper limits and lower work passes.
- [ ] Semantic profile logical end time equals evaluator steps.
- [ ] Semantic profile bytes are identical for two identical runs and contain no unstable paths/timestamps.
- [ ] CLI statistics are on stderr, after diagnostics, and program stdout is unchanged.
- [ ] Profile writes are atomic and leave no partial destination after failure.
- [ ] GHC manual-stage and late-hotspot builds are separate from default builds and from Jazz semantic profiles.
- [ ] Corpus and performance documentation accurately describes commands, artifacts, limitations, and update policy.
- [ ] Full default tests/build, benchmark smoke, profiling smoke, docs, distribution, format, lint, and repository audit pass.

## Reference Documentation

- [Cabal project profiling fields](https://cabal.readthedocs.io/en/stable/cabal-project-description-file.html)
- [GHC 9.14 profiling guide](https://downloads.haskell.org/ghc/9.14.1/docs/users_guide/profiling.html)
- [`tasty-bench` package documentation](https://hackage.haskell.org/package/tasty-bench)
- [Speedscope evented profile format and viewer](https://github.com/jlfwong/speedscope)
