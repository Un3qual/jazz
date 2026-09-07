# Optional Backend Removal Implementation Plan

> Execute inline with the executing-plans workflow. The maintainer approved
> removal and requested this plan before implementation. No subagents.

**Goal:** Remove the unused Typed Core/Lowered IR backend and its maintenance
surface while preserving the working compiler, interpreter, and hosted frontend.

**Architecture:** Ordinary execution stays on the analyzed AST. Delete the
optional backend and its exclusive consumers; retain shared semantic operations
and frontend conformance tests. Native compilation remains a future product
choice, without a mandatory IR schema or interpreter migration today.

**Tech Stack:** Existing Haskell, Jazz, Cabal, Nix, Python, and repository checks.

**Spec:** The maintainer-approved removal recommendation in this task, followed
by a request to present the removal plan first. Implementation baseline is
`9b404009`; compiler verification at `172038df` passed all 63 enabled suites,
executable examples, repository checks, and the isolated Nix gate.

## Constraints and retention boundary

- Preserve public language semantics, CLI compile/run behavior, diagnostics,
  module behavior, standard library, host effects, recursion, and runtime plans.
- Keep the Haskell parser, resolver, analyzer, inference engine, interpreter,
  nominal identities, analyzed facts, shared type representation, and pattern
  coverage. Keep the normal `Lowered` AST phase: it is not Lowered IR.
- Keep Jazz-authored lexing, parsing, canonical-core lowering, their schemas,
  and meaningful differential tests. Keep shared canonical-value adapters.
- `InferConcreteFunctions` also serves ordinary expression inference. Do not
  remove it or its branches merely because the optional producer uses it.
- Do not replace deleted code with interfaces, stubs, feature flags, an archive
  directory, or a new backend abstraction. Git preserves the old implementation.
- Preserve accepted decision history. Explicitly supersede active backend
  commitments instead of silently rewriting past RFCs or deleting all RFCs.
- Review future plans and both retained callers and deleted callers before
  classifying anything as backend-only. Stop to discuss a real behavior or
  design tradeoff; routine deletion and integration choices are authorized.

## 1. Confirm the removal inventory and preserve independent coverage

- [ ] Inventory imports and call sites of `Jazz.Compiler.TypedCore*` and
      `Jazz.Compiler.LoweredIR*`, including Cabal, test, benchmark, documentation,
      script, and CI references. Classify direct consumers and shared helpers.
- [ ] Review the Typed Core/Lowered IR tests for independent language behavior.
      Keep existing equivalent runtime/inference tests; move only genuinely
      unique behavior checks to the corresponding retained suite. Delete IR
      shape, portable-schema, validator, and backend capability fixtures with
      their owner. Do not translate every backend assertion into a new test.
- [ ] Use the existing successful full baseline instead of rerunning it before
      edits. Run any moved behavior tests before deleting their old owner.

Primary ownership checks:

```sh
rg -n 'TypedCore|LoweredIR|inferResolvedModuleTypedCoreExpressionDirectCall' src app test benchmark jazz.cabal
rg -n 'InferConcreteFunctions|InferenceOnly' src/Jazz/Compiler/TypeInference.hs src/Jazz/Compiler/TypeInference
```

## 2. Remove the optional backend and all executable integration together

Delete these backend owners:

- `src/Jazz/Compiler/TypedCore.hs` and `src/Jazz/Compiler/TypedCore/`.
- `src/Jazz/Compiler/LoweredIR.hs` and `src/Jazz/Compiler/LoweredIR/`.
- `jazz/Compiler/TypedCoreTypes.jz`, `jazz/Compiler/TypedCoreValidate.jz`,
  `jazz/Compiler/LoweredIRTypes.jz`, and `jazz/Compiler/LoweredIRValidate.jz`.
- Dedicated Typed Core/Lowered IR suites, adapters, and fixture modules under
  `test/Jazz/Compiler/Bootstrap/`, after the coverage review above. Preserve the
  frontend files in that directory.

Update retained owners:

- `src/Jazz/Compiler/TypeInference.hs`: remove the opt-in producer entry point,
  its profile checks, result handling, exports, and backend imports. Trace any
  newly unused helper before deleting it; preserve normal inference modes.
- `src/Jazz/Compiler/Force.hs`: remove backend-only forcing functions.
- `src/Jazz/Compiler/Profiling.hs`: remove backend validation stages and the
  typed-validation, lowered-validation, and typed-lowering benchmark groups.
  Preserve source-to-AST lowering and ordinary compiler/runtime profiling.
- `benchmark/Jazz/Benchmark/StageInputs.hs`, `Stages.hs`, and `ScaleCases.hs`:
  remove backend cases, generated IR fixtures, preparation, and execution arms.
  Preserve frontend, inference, module, and runtime benchmark families.
- `jazz.cabal`: remove deleted modules and dedicated suites. Remove a package
  dependency only when no retained component needs it.
- `test/Jazz/Compiler/ProfilingSpec.hs`, benchmark tests, and
  `test/Jazz/Repository/AuditSpec.hs`: remove assertions tied to removed backend
  behavior and required-file inventories. Preserve generic formatting tests
  even when their sample data happens to use names such as TypedLiteral.
- `scripts/ci/fast-compiler.sh`, `scripts/check-ci-policy.py`, and
  `scripts/test-check-ci-policy.py`: remove deleted suite requirements while
  retaining the existing checks for surviving compiler behavior.

- [ ] Apply the deletions and integration updates as one buildable milestone.
- [ ] Build all enabled components with development warnings and repair genuine
      residual dependencies without compatibility scaffolding.
- [ ] Run the retained runtime, inference, module, prelude, hosted frontend,
      profiling, benchmark, and repository suites affected by the deletion.
- [ ] Review the diff and commit the green removal milestone.

## 3. Reconcile the durable architecture and active documentation

- [ ] Record the approved change in a concise accepted RFC, with an index entry
      and explicit supersession of the optional backend commitments in RFCs
      0003-0006, 0009-0011, and 0013-0015 as applicable. Preserve the unrelated
      stage-0, hosted frontend, runtime-host, and language decisions.
- [ ] Add a clear supersession notice to affected historical RFCs. Retain their
      files and accepted-history metadata; the current checker requires those
      historical files, so no new RFC lifecycle framework is needed.
- [ ] Update `docs/compiler/bootstrapping.md`, `docs/compiler/pipeline.md`,
      `docs/compiler/architecture.md`, `docs/project/status.md`, relevant roadmap
      and performance documentation, and any other live references found by
      the inventory. Describe the existing analyzed interpreter accurately.
- [ ] Remove active requirements to mirror or extend deleted schemas and the
      assumed future Typed Core interpreter cutover. Keep native compilation
      as future work requiring a fresh concrete design and executable goal.
- [ ] Reconcile `.codex/execution/queue.md` and affected internal plans without
      treating historical completed checklists as new implementation work.
- [ ] Update documentation-policy assertions only where they require removed
      behavior. Preserve authority, link, and documentation validation.
- [ ] Format and run documentation, queue, RFC, and policy checks; commit.

## 4. Verify the final retained project and close the queue item

- [ ] Search for residual backend dependencies; distinguish historical mentions
      from active requirements. Confirm neither executable nor retained test
      and benchmark components import the removed modules.
- [ ] Check formatting and Git whitespace for every changed file.
- [ ] Run the final enabled-component build, all retained default suites, Cabal
      metadata checks, and executable examples. Report the actual remaining
      suite count; a smaller count is expected after deleting feature suites.
- [ ] Run the repository phase and isolated Nix gate once on the final code.
      Repeat only checks invalidated by a correction.
- [ ] Commit final verification evidence and return the queue to its accurate
      state. Report removed scope, retained behavior, test results, and any
      actual limitation. Do not claim native execution or self-hosting.

Use the repository Nix shell for compiler commands:

```sh
cabal build all --enable-tests -fdevelopment --jobs=1
cabal test all -fdevelopment --test-show-details=direct --jobs=1
cabal check
jazz_removal_bin="$(cabal list-bin jazz)"
bash scripts/check-examples.sh --jazz-bin "$jazz_removal_bin"
JAZZ_MAIN_PHASE=repository bash scripts/ci/main-functional.sh
nix --extra-experimental-features 'nix-command flakes' flake check --max-jobs 1 --cores 1
```

## Plan review

The retention boundary preserves all active execution paths and the useful
hosted frontend. The removal includes executable registrations, benchmark and
profiling surfaces, tests, and active architectural promises, so it does not
leave a disabled subsystem behind. Shared inference modes are explicitly
protected. Deleting the optional backend changes the development roadmap and
internal APIs, not Jazz language behavior. No source files have been removed
while preparing this plan.
