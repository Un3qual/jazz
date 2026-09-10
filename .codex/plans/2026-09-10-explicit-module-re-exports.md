---
id: JN-MODULE-EXPLICIT-REEXPORTS-001
status: complete
priority: P1
size: L
kind: impl
autonomous_ready: yes
depends_on: []
plan_section: "Implementation"
target_paths:
  - src/Jazz/Compiler/ModuleExports.hs
  - src/Jazz/Compiler/ModuleResolver.hs
  - src/Jazz/Compiler/ModuleAnalysis.hs
  - src/Jazz/Compiler/ModuleRuntime.hs
verification:
  - cabal test module-exports-spec module-resolution-spec module-pipeline-contract-spec loader-spec --jobs=4 --test-show-details=failures
  - JAZZ_CABAL_JOBS=4 bash scripts/ci/haskell-quality.sh
  - bash scripts/check-docs.sh
  - bash scripts/check-execution-queue.sh
deliverable: "Explicit typed re-exports preserve original declaration identities, public constructor visibility and class evidence across facade modules."
last_verified: 2026-09-10
---

# Explicit Module Re-exports Implementation Plan

Execute the approved work in this existing isolated worktree, using the
executing-plans and test-driven-development workflows. Commit verified
milestones and finish the implementation without another design gate.

**Goal:** Implement accepted RFC 0018 in the Haskell module pipeline.

**Architecture:** Attach original declaring module paths to the existing public
export inventory. Resolve public selectors after dependencies and use that
inventory for canonical name resolution, collision checks and runtime forwarding.
Retain canonical compiler metadata at dependency boundaries so facades do not
rebase already resolved types, binder IDs or implementation evidence.

**Tech stack:** Haskell, Cabal, pinned Nix development and quality shells.

**Spec:** `rfcs/accepted/0018-explicit-module-re-exports.md`.

## Global constraints

- Typed selectors alone may forward explicit unqualified imports.
- Bare selectors and omitted lists remain owned-only; `()` exports nothing.
- Preserve original declaration and implementation identities through diamonds.
- Public inventories control visibility; private semantic metadata never widens it.
- Forward runtime cells, including captured environments, without wrappers.
- Class re-exports carry selected public class payloads and facade-owned impls.
- Keep alias-qualified header selectors, renaming, operators, packages, hosted
  bootstrap work, native execution and new class/effect features out of scope.

## Implementation

### Task 1: Export selection and canonical identity

Owners: `src/Jazz/Compiler/ModuleExports.hs`, `src/Jazz/Compiler/ModuleResolver.hs`,
`src/Jazz/Compiler/ModuleResolver/Imports.hs`, `src/Jazz/Compiler/ModuleResolver/Names.hs`;
`test/Jazz/Compiler/Modules/Loader/ReexportTests.hs`, `test/Jazz/Compiler/Modules/LoaderSpec.hs`,
`test/Jazz/Compiler/Modules/ModuleExportsSpec.hs`, `test/Jazz/Compiler/Modules/ModuleResolutionSpec.hs`, and `jazz.cabal`.

- [x] Add real graph cases for values/closures, ADTs and classes through two
      facades. Start with this failing value program and literal expected `42`:

```jazz
module Library::Source (value answer) { answer = 42. }
module Library::API (value answer) { import Library::Source. }
module App::Main { import Library::API. answer. }
```

Each module is a separate fixture source. Run the loader suite and establish
that the feature fails at export validation with `E4015` before changing code.

- [x] Extend `ModuleExportInventory` with original owners while keeping
      `ModuleExport` as the public namespace/name key. Add
      `exportOrigin :: ModulePath -> ModuleExport -> ModuleExportInventory -> ModulePath`.
      Preserve constructor ownership when an owner type is not publicly selected.
- [x] Move dependency-sensitive selector validation into dependency-first graph
      traversal. Merge explicit unqualified public inventories under local
      declarations, retain owned-only bare selection, and validate constructor
      groups against the chosen nominal type and visible constructors.
- [x] Use original owners in unqualified/aliased name resolution and import
      collision checks. Identical declarations reached through different paths
      deduplicate; distinct conflicting declarations keep existing diagnostics.
- [x] Cover private/filtered/alias-only/ambient-only selection, constructor owners,
      local shadowing, abstract types, bare/default exports and genuine collisions.
      Run resolver/export suites and commit the coherent boundary change once
      compiler/runtime integration below makes the graph tests pass.

### Task 2: Compiler and runtime forwarding

Owners: `src/Jazz/Compiler/ModuleAnalysis.hs`, `src/Jazz/Compiler/ModuleCompiler.hs`,
`src/Jazz/Compiler/ModuleRuntime.hs`; `test/Jazz/Compiler/Modules/ModulePipelineContractSpec.hs`
and `test/Jazz/Compiler/Modules/Loader/ReexportTests.hs`.

- [x] Canonicalize each module's owned interface once using its original owner.
      Store `(ModuleExportInventory, ImportedInterface)` for dependencies;
      `dependencyImportInterface` selects canonical entries by public origin
      instead of rebasing a facade's entire payload as newly owned declarations.
- [x] Carry original binder IDs, hidden datatype metadata and evidence candidates.
      Filter public class payloads by selected canonical class identities;
      include locally declared impls of explicitly re-exported imported classes.
      Deduplicate identical evidence without concealing distinct impl conflicts.
- [x] Resolve imported runtime exports with the same inventory origins. At
      module publication, forward selected exports from direct dependency maps
      alongside owned exports. Reuse existing cells and closure environments.
- [x] Exercise constrained/stored/partial/explicit methods, facade-owned impls,
      direct-plus-facade and diamond imports, nominal ADT pattern use, hidden
      helpers/types, dependency effect counts and suppressed dependency expressions.
      Expected outputs and diagnostics are literal fixture expectations.
- [x] Run all four focused suites; resolve failures against the RFC and commit.

### Task 3: Documentation, verification and closeout

Owners: `docs/language/modules.md`, `docs/language/capabilities.md`,
`docs/reference/module-resolution.md`, `docs/reference/expression-grammar.md`, `docs/reference/diagnostics.md`
as needed, `docs/project/status.md`, `examples/modules/`,
`test/Jazz/Repository/AuditSpec.hs`, `scripts/check-examples.py`, and queue state.

- [x] Add an executable facade example with output `42`; register it with the
      existing example checker and repository source inventory. Document typed
      forwarding, class payloads, identity/collisions, and owned-only defaults.
- [x] Run focused suites, all supported non-bootstrap suites, the Haskell quality
      gate, executable examples, docs/queue checks and `git diff --check` in pinned
      environments. Existing deferred hosted parity is not feature work.
- [x] Review the complete change for RFC coverage and implementation simplicity;
      fix verified issues and rerun affected checks.
- [x] Record actual verification and commits, mark this plan complete, remove the
      ready row, reconcile blocked umbrellas and shipped status, then commit.

## Execution record

- Maintainer approved RFC 0018. Existing detached linked worktree retained.
- Baseline: module export, resolution, pipeline and loader suites all passed in
  the pinned Nix development shell before implementation.

- Accepted RFC and implementation plan committed as `bab9b3fd`.
- The three initial facade programs failed with `E4015` before implementation.
  Closure, nominal ADT and class diamonds now execute through original identities.
- Compiler boundaries retain canonical metadata and original binder/evidence IDs;
  runtime publication forwards existing cells. The obsolete interface-to-public
  inventory helper and its implementation-only test were removed.
- Review verified normal `E2015` rejection for distinct same-target impls and
  `E4008` rejection when a shared class masks a value/constructor collision.
  Repeated original implementations remain idempotent. A host-output test proves
  one deferred binding is forced once through two facade aliases.
- Implementation, public contract, behavior coverage and executable examples
  committed as `31f37a13`.

## Verification

- All 47 non-bootstrap suites selected from `jazz.cabal` passed with
  `cabal test --jobs=4 --test-show-details=failures` and their explicit names.
- After the final collision fix, `module-exports-spec`, `module-resolution-spec`,
  `module-pipeline-contract-spec`, and `loader-spec` all passed again.
- `JAZZ_CABAL_JOBS=4 bash scripts/ci/haskell-quality.sh` passed in the pinned
  quality shell: HLint, production and full Weeder checks, all-target builds
  including opt-in scale suites, and generated invariant tests. Its final
  generated-invariant run rebuilt the final import-resolution change.
- Focused HLint on the final changed import/test files reported no hints;
  `cabal check` reported no errors or warnings.
- `bash scripts/check-examples.sh` passed all six cases, including the new
  `module-reexports` case with output `42`.
- `bash scripts/check-docs.sh`, RFC structure, queue validation, pinned Markdown
  formatting and `git diff --check` passed. Closeout rechecks documentation and
  queue state after removing the completed row.
- Hosted/bootstrap suites were compiled by the all-target quality gate. Their
  execution and existing qualification parity work remain deferred.
