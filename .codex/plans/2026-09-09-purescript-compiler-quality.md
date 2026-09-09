---
id: JN-PURESCRIPT-COMPILER-QUALITY-001
status: ready
priority: P2
size: L
kind: impl
autonomous_ready: yes
depends_on: []
plan_section: "Implementation"
target_paths:
  - src/Jazz/Compiler/ModuleCompiler.hs
  - src/Jazz/Compiler/Diagnostics.hs
  - jazz.cabal
verification:
  - cabal test all --jobs=4 --test-show-details=failures
deliverable: "Module boundaries, diagnostics, ranges, generated tests and quality gates"
last_verified: 2026-09-09
---

# PureScript compiler quality implementation

The user approved both comparison recommendations on 2026-09-09. This plan
records that design and its execution; no additional design approval is needed.
Base: `49d815e3`. Work stays in the existing isolated checkout.

## Design and constraints

Preserve phase-indexed core, nominal identities, analyzed runtime plans, import
visibility, constraint provenance, failed-unification rollback, diagnostic order,
and hosted frontend conformance. Use the existing `transformers` package for
bounded state composition. QuickCheck is a test-only dependency. Keep compiler
questions in domain modules with one-way dependencies, not callback frameworks.

Diagnostics retain typed mismatch causes and reusable checking context until
their report boundary. Existing codes and specific primary locations survive.
Source ranges use exclusive end positions, preserve source qualification, and
are carried through lowering and analysis. Point-only legacy fixtures remain
representable; hosted canonical point schemas continue to describe their
existing contract, with separate range coverage for the Haskell frontend.

Normalize only when a concrete downstream case disappears. In particular guards
must survive until coverage analysis; record this invariant beside lowering.
No persistent cache, scheduler, new backend, or new language syntax is included.

## Implementation

### Task 1: Automated Haskell quality and generated invariants

- [ ] Add QuickCheck generation/shrinking for nested semantic types and stable
      sets, testing traversal laws, substitution identity/composition, and
      independent first-occurrence ordering/membership expectations.
- [ ] Run properties through the existing test harness; preserve reproducible
      failure seeds and real shrinking rather than hand-rolled random checks.
- [ ] Establish HLint and Weeder baselines against the pinned GHC. Run Weeder
      against production HIE files before including tests; explicitly identify
      benchmark, generated, and tooling roots. Document justified exclusions.
- [ ] Wire checks into an appropriate CI tier, verify actual tools, commit.

### Task 2: Compiler question boundaries

- [ ] Extract a single-module semantic analysis operation from `ModuleCompiler`
      consuming resolved core, complete imported facts/binders/evidence, and
      compile settings. Keep dependency order, failure handling, and result
      accumulation in `analyzeProgram`. Test against real module fixtures.
- [ ] Move impl-body checking out of `TypeInference.Capabilities` into a focused
      declaration owner with explicit signature/constraint dependencies. Keep
      the existing expression-inference callback and avoid cyclic imports.
- [ ] Trial strict local state composition for sequential impl-body checks,
      preserving checkpoints and rollback; retain only a demonstrable cleanup.
- [ ] Explain guard/operator/recursive-binding normalization constraints beside
      the existing transformation; do not add an unearned compiler pass.
- [ ] Run module, binding-signature, and runtime suites, commit.

### Task 3: Typed diagnostic causes and checking context

- [ ] Add inference-owned mismatch causes and context constructors for binding,
      impl-method, and constraint checking. Keep presentation-neutral shared
      diagnostics independent of the solver state.
- [ ] Render variable names consistently per report from retained semantic
      types. Add ordered context at owning boundaries without duplicate hints
      or overwriting a more specific primary span.
- [ ] Add tests for stable variable names across allocation offsets, structural
      causes, context order, and precise primary-location preservation.
- [ ] Run diagnostic, signature, binding, and module suites, commit.

### Task 4: Complete source ranges

- [ ] Extend source positions with exclusive end positions and preserve legacy
      point constructors. Capture actual lexical and expression endpoints.
- [ ] Carry ranges through parser lowering, qualification, metadata traversals,
      and diagnostics; make range information accessible to tooling.
- [ ] Test multiline and Unicode ranges, source qualification, lowered/analyzed
      preservation, and existing hosted canonical parity. Update the public
      diagnostic reference for the range convention.
- [ ] Run parser, lowering, hosted frontend, and diagnostic suites, commit.

### Task 5: Integration and review

- [ ] Review the complete change for semantic regressions and needless
      abstractions, address verified findings.
- [ ] Run development-warning build, all default suites, both profiling builds,
      Haskell lint/dead-code/property gates, formatter, package/docs/queue checks.
- [ ] Record completion evidence, clear the queue row, and commit the result.

## Execution evidence

Implementation in progress. Task owners and verification results are appended
here as each batch completes.
