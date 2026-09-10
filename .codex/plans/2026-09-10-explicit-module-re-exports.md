---
id: JN-MODULE-EXPLICIT-REEXPORTS-001
status: ready
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

Owners: `src/Jazz/Compiler/ModuleExports.hs`, `ModuleResolver.hs`,
`ModuleResolver/Imports.hs`, `ModuleResolver/Names.hs`;
`test/Jazz/Compiler/Modules/Loader/ReexportTests.hs`, `LoaderSpec.hs`,
`ModuleExportsSpec.hs`, `ModuleResolutionSpec.hs`, and `jazz.cabal`.

- [ ] Add real graph cases for values/closures, ADTs and classes through two
      facades. Start with this failing value program and literal expected `42`:

```jazz
module Library::Source (value answer) { answer = 42. }
module Library::API (value answer) { import Library::Source. }
module App::Main { import Library::API. answer. }
```

Each module is a separate fixture source. Run the loader suite and establish
that the feature fails at export validation with `E4015` before changing code.

- [ ] Extend `ModuleExportInventory` with original owners while keeping
      `ModuleExport` as the public namespace/name key. Add
      `exportOrigin :: ModulePath -> ModuleExport -> ModuleExportInventory -> ModulePath`.
      Preserve constructor ownership when an owner type is not publicly selected.
- [ ] Move dependency-sensitive selector validation into dependency-first graph
      traversal. Merge explicit unqualified public inventories under local
      declarations, retain owned-only bare selection, and validate constructor
      groups against the chosen nominal type and visible constructors.
- [ ] Use original owners in unqualified/aliased name resolution and import
      collision checks. Identical declarations reached through different paths
      deduplicate; distinct conflicting declarations keep existing diagnostics.
- [ ] Cover private/filtered/alias-only/ambient-only selection, constructor owners,
      local shadowing, abstract types, bare/default exports and genuine collisions.
      Run resolver/export suites and commit the coherent boundary change once
      compiler/runtime integration below makes the graph tests pass.

### Task 2: Compiler and runtime forwarding

Owners: `src/Jazz/Compiler/ModuleAnalysis.hs`, `ModuleCompiler.hs`,
`ModuleRuntime.hs`; `test/Jazz/Compiler/Modules/ModulePipelineContractSpec.hs`
and `Loader/ReexportTests.hs`.

- [ ] Canonicalize each module's owned interface once using its original owner.
      Store `(ModuleExportInventory, ImportedInterface)` for dependencies;
      `dependencyImportInterface` selects canonical entries by public origin
      instead of rebasing a facade's entire payload as newly owned declarations.
- [ ] Carry original binder IDs, hidden datatype metadata and evidence candidates.
      Filter public class payloads by selected canonical class identities;
      include locally declared impls of explicitly re-exported imported classes.
      Deduplicate identical evidence without concealing distinct impl conflicts.
- [ ] Resolve imported runtime exports with the same inventory origins. At
      module publication, forward selected exports from direct dependency maps
      alongside owned exports. Reuse existing cells and closure environments.
- [ ] Exercise constrained/stored/partial/explicit methods, facade-owned impls,
      direct-plus-facade and diamond imports, nominal ADT pattern use, hidden
      helpers/types, dependency effect counts and suppressed dependency expressions.
      Expected outputs and diagnostics are literal fixture expectations.
- [ ] Run all four focused suites; resolve failures against the RFC and commit.

### Task 3: Documentation, verification and closeout

Owners: `docs/language/modules.md`, `capabilities.md`,
`docs/reference/module-resolution.md`, `expression-grammar.md`, diagnostics
as needed, `docs/project/status.md`, `examples/modules/`,
`test/Jazz/Repository/AuditSpec.hs`, `scripts/check-examples.py`, and queue state.

- [ ] Add an executable facade example with output `42`; register it with the
      existing example checker and repository source inventory. Document typed
      forwarding, class payloads, identity/collisions, and owned-only defaults.
- [ ] Run focused suites, all supported non-bootstrap suites, the Haskell quality
      gate, executable examples, docs/queue checks and `git diff --check` in pinned
      environments. Existing deferred hosted parity is not feature work.
- [ ] Review the complete change for RFC coverage and implementation simplicity;
      fix verified issues and rerun affected checks.
- [ ] Record actual verification and commits, mark this plan complete, remove the
      ready row, reconcile blocked umbrellas and shipped status, then commit.

## Execution record

- Maintainer approved RFC 0018. Existing detached linked worktree retained.
- Baseline: module export, resolution, pipeline and loader suites all passed in
  the pinned Nix development shell before implementation.
