---
id: JN-PURESCRIPT-COMPILER-QUALITY-001
status: complete
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

- [x] Add QuickCheck generation/shrinking for nested semantic types and stable
      sets, testing traversal laws, substitution identity/composition, and
      independent first-occurrence ordering/membership expectations.
- [x] Run properties through the existing test harness; preserve reproducible
      failure seeds and real shrinking rather than hand-rolled random checks.
- [x] Establish HLint and Weeder baselines against the pinned GHC. Run Weeder
      against production HIE files before including tests; explicitly identify
      benchmark, generated, and tooling roots. Document justified exclusions.
- [x] Wire checks into an appropriate CI tier, verify actual tools, commit.

### Task 2: Compiler question boundaries

- [x] Extract a single-module semantic analysis operation from `ModuleCompiler`
      consuming resolved core, complete imported facts/binders/evidence, and
      compile settings. Keep dependency order, failure handling, and result
      accumulation in `analyzeProgram`. Test against real module fixtures.
- [x] Move impl-body checking out of `TypeInference.Capabilities` into a focused
      declaration owner with explicit signature/constraint dependencies. Keep
      the existing expression-inference callback and avoid cyclic imports.
- [x] Trial strict local state composition for sequential impl-body checks,
      preserving checkpoints and rollback; retain only a demonstrable cleanup.
- [x] Explain guard/operator/recursive-binding normalization constraints beside
      the existing transformation; do not add an unearned compiler pass.
- [x] Run module, binding-signature, and runtime suites, commit.

### Task 3: Typed diagnostic causes and checking context

- [x] Add inference-owned mismatch causes and context constructors for binding,
      impl-method, and constraint checking. Keep presentation-neutral shared
      diagnostics independent of the solver state.
- [x] Render variable names consistently per report from retained semantic
      types. Add ordered context at owning boundaries without duplicate hints
      or overwriting a more specific primary span.
- [x] Add tests for stable variable names across allocation offsets, structural
      causes, context order, and precise primary-location preservation.
- [x] Run diagnostic, signature, binding, and module suites, commit.

### Task 4: Complete source ranges

- [x] Extend source positions with exclusive end positions and preserve legacy
      point constructors. Capture actual lexical and expression endpoints.
- [x] Carry ranges through parser lowering, qualification, metadata traversals,
      and diagnostics; make range information accessible to tooling.
- [x] Test multiline and Unicode ranges, source qualification, lowered/analyzed
      preservation, and existing hosted canonical parity. Update the public
      diagnostic reference for the range convention.
- [x] Run parser, lowering, hosted frontend, and diagnostic suites, commit.

### Task 5: Integration and review

- [x] Review the complete change for semantic regressions and needless
      abstractions, address verified findings.
- [x] Run development-warning build, all default suites, both profiling builds,
      Haskell lint/dead-code/property gates, formatter, package/docs/queue checks.
- [x] Record completion evidence, clear the queue row, and commit the result.

## Execution evidence

Implemented in `11eb4e58` (compiler and quality foundations) and `9088b55e`
(CI integration and final baselines), against base `49d815e3`.

- Isolated module analysis and impl-body checking preserve imported facts,
  evidence, method ordering, and failed-unification rollback. Real module and
  partial-unification fixtures cover those boundaries.
- Diagnostic causes retain semantic types; report-local variable names remain
  stable across allocation offsets. Binding, method, and constraint contexts
  preserve specific primary locations and ordered hints.
- Lexical, expression, and pattern ranges preserve exclusive endpoints through
  lowering, analysis, qualification, and native diagnostics. Existing hosted
  canonical schemas explicitly retain their point-only contract. Review found
  and fixed range loss in prelude and import adapters.
- Normalization comments record guard coverage, recursive visibility, operator
  identity, and source-metadata requirements without adding another pass.

### Verification

All compiler checks used the repository-pinned GHC 9.14.1. Builds ran serially
after the local bounded-verification rule was identified; no performance
measurements or speedup claims are made.

- `cabal test all --enable-tests --keep-going --jobs=4 --test-show-details=failures --ghc-options=-fwrite-ide-info`:
  all 62 default suites passed, including hosted conformance, program corpus,
  parser scale, runtime, and standard-library suites.
- Both `cabal.project.profile-stages` and
  `cabal.project.profile-hotspots` completed `build all` using their separate
  build directories and `--jobs=4`.
- `JAZZ_CABAL_JOBS=4 nix develop .#quality --command bash scripts/ci/haskell-quality.sh`
  passed from a fresh temporary build directory: HLint, production HIE/Weeder,
  complete test/benchmark/optional-parser HIE/Weeder, and all eight generated
  properties with 1,000 cases each.
- An isolated StableSet mutation failed with a shrunk two-element example;
  replay reproduced the failure. Removing one exact diagnostic API root from a
  temporary Weeder configuration reported precisely that declaration.
- Weeder's pinned GHC-compatible source passed all 22 upstream tests.
  Compatibility overrides retain semantic dependency tests; obsolete
  expected-failure markers use upstream correct expectations. The quality shell
  exposes Weeder through an executable wrapper so its build-time GHC does not
  shadow Jazz's compiler package environment.
- The explicit Weeder baselines document retained inspection/legacy APIs,
  shared component helpers, generated package metadata, and the deliberate
  structural-instance policy. Review verified the names and rationales.
- CI policy: 120 tests passed. Actionlint, shell syntax, Ormolu for all 51 changed
  Haskell files, documentation/authority checks, queue checks, and
  `git diff --check` passed.
- `cabal check`, the documented executable examples, and `cabal sdist` passed.
  The source archive contains all ten checked new compiler/test/config/script
  additions. `nix flake check --no-build` passed for the current
  `aarch64-darwin` system; this is evaluation evidence, not a new Nix package
  build or cross-platform test run.

The final full test log was `/tmp/jazz-full-tests-verified.log`; the fresh quality
gate log was `/tmp/jazz-haskell-quality-verified.log`. Independent review found
no remaining actionable issues. The approved scope is complete; no new queue
candidate is inferred from this cleanup.
