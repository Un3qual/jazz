---
id: JN-GENERIC-CAPABILITIES-CORE-001
status: complete
priority: P1
size: L
kind: impl
autonomous_ready: no
depends_on: []
plan_section: "Implementation"
target_paths:
  - src/Jazz/Compiler/TypeRepresentation.hs
  - src/Jazz/Compiler/KindInference.hs
  - src/Jazz/Compiler/SemanticDeclarations.hs
  - src/Jazz/Compiler/TypeInference/Scope.hs
  - src/Jazz/Compiler/ModuleInterface.hs
  - src/Jazz/Compiler/ModuleResolver/Names.hs
  - src/Jazz/Compiler/Runtime/Engine.hs
  - jazz/stdlib/Prelude.jz
  - jazz/stdlib/Reduce.jz
  - test/Jazz/Compiler/Stdlib/GenericCapabilitiesTests.hs
  - test/Jazz/Compiler/Stdlib/OperatorFunctionsTests.hs
  - docs/language/capabilities.md
  - docs/language/operators.md
verification:
  - cabal test all --jobs=1 --test-show-details=failures
  - JAZZ_CABAL_JOBS=1 bash scripts/ci/haskell-quality.sh
  - bash scripts/check-docs.sh
  - bash scripts/check-execution-queue.sh
  - git diff --check
deliverable: "Generic capabilities, migrated library names, ordinary operator functions, and verified review cleanup."
last_verified: 2026-09-14
---

# Generic capabilities, library names, and operators

> **Retired compiler scope:** [RFC 0016](../../rfcs/accepted/0016-optional-backend-removal.md)
> and [RFC 0022](../../rfcs/accepted/0022-hosted-compiler-removal.md) retired the
> optional backend, hosted compiler, and their exclusive verification. References
> to those paths, schemas, suites, and retention requirements in this record
> (including frontmatter) are historical and impose no current work or test obligations.

Completed implementation and review record for this thread. This consolidates
its core, library, quality-review, legacy-audit, operator, and architecture plans.
[RFC 0019](../../rfcs/accepted/0019-generic-capabilities-and-library-names.md)
and [RFC 0020](../../rfcs/accepted/0020-operators-as-functions.md) retain the
accepted decisions; `docs/language/` and `docs/reference/` define public behavior.
The [183-entry rename inventory](2026-09-13-stdlib-api-renames.csv) remains intact.

## Final decisions

- Classes have one parameter with inferred, declaration-finalized kinds. Generic
  implementations, method-local quantification, superclasses, defaults, and
  inferred constraints use ordinary schemes, nominal identities, and checked
  evidence. Reject overlapping instances and invalid declaration prerequisites;
  check implementation/default bodies with rigid quantified variables.
- `Equatable`, `Comparable`, `Mappable`, `Reducible`, and `Combinable` provide
  the approved collection families. Mapping preserves the collection constructor.
  Text and Set retain separate mapping functions; conversion between collections
  is explicit. The retained `Reduce` module implements safe seedless reduction
  with one fold and a Maybe accumulator. All 183 public renames preserve argument
  order and return contracts, with no old public compatibility aliases.
- All eleven executable operators resolve to ordinary functions in lexical scope,
  including shadowing, imports, constraints, first-class values, and sections.
  `not` is an ordinary Boolean function. `Num` owns arithmetic, ordering helpers
  use `Comparable::compare`, and primitive implementations call private kernels.
  ADT equality requires Equatable evidence; mixed numeric operands require
  explicit conversion. Sections evaluate the captured operand once. Pattern `|`
  and existing custom-operator declaration/transport restrictions remain intact.
- Module interfaces own the complete instance environment. Schemes contain types
  and constraints; analyzed expressions retain selected evidence and prepared
  captures. Deferred constraints retain use-site visibility, and runtime method
  cells retain lexical ownership. One analyzed-core interpreter remains.

## Implementation

- [x] Implement kinds, canonical type applications, generic implementation
      templates, ordinary method values, defaults, superclasses, and transitive
      instance transport, including alias and empty-selection imports.
- [x] Migrate library capabilities, public names, consumers, hosted frontend
      fixtures, API inventories, examples, website tooling, and documentation.
- [x] Add Boolean `not` and resolve every executable operator through ordinary
      function inference and runtime evidence; migrate intentional equality and
      numeric compatibility changes in programs and tests.
- [x] Run Weeder first, then independently trace reachable and non-Haskell
      leftovers. Remove the runtime candidate catalog, duplicate method exports,
      unused declaration/projection payloads, concrete-only signature catalogs,
      test adapters, old operator dispatch, and stale policy exemptions.
- [x] Prepare lambda evidence captures once; use ordinary method inference and
      direct scheme inputs. Replace positional capability-law goldens with named
      checks while retaining meaningful behavioral coverage.
- [x] Check class self-prerequisite kinds before defaulting. Remove sequential
      inline-module replay and per-scheme capability snapshots; migrate synthetic
      module fixtures to the real coordinator. Remove obsolete inferred state.
- [x] Validate the local CodeRabbit review and fix diagnostic application rendering,
      Num documentation, and Queue order wording. Keep the invalid operator
      shadowing, COMPLETE pragma, and duplicate class-arity suggestions rejected.
      A runtime method index remains unjustified without isolated profiling;
      no new index or cache was introduced.

## Verification and commits

| Milestone                  | Commit evidence                                                                    | Result                                                                                                                                                                                                                                                                                                                                            |
| -------------------------- | ---------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Generic compiler core      | `6970d538`                                                                         | Full build/tests, all four full parser-scale executions, quality, examples, docs, and queue checks passed.                                                                                                                                                                                                                                        |
| Library migration          | `99a15625`, `37710e02`                                                             | All 183 renames verified; collection laws, custom instances, Text/Set restrictions, Reduce, authored/bundled Prelude parity, and website checks passed. Full parser-scale and both Weeder graphs passed.                                                                                                                                          |
| Quality and legacy cleanup | `db2439c3`, `7b48bc2a`, `509b6c25`, `8a5555f8`, `27364518`, `3a75cabd`, `83cf09be` | All 62 regular suites, fresh production and whole-tree Weeder, lint/build/invariant checks, and independent leftover scans passed.                                                                                                                                                                                                                |
| Ordinary operators         | `40f78a07`, `7a52c11c`, `ab77447a`, `c841ba11`                                     | All 62 regular suites and four full parser-scale workloads passed at the operator milestone; all 17 corpus outputs and budgets passed. Replaced operator state was removed.                                                                                                                                                                       |
| Architecture fixes         | `e0e6e1f5`, `3a89de6c`                                                             | All 62 regular suites passed on the final architecture implementation; fresh production Weeder, lint, formatting, package, and docs checks passed. The four opt-in full parser stress suites were not rerun for this cleanup.                                                                                                                     |
| CodeRabbit follow-up       | `91558383`                                                                         | All 258 changed files reviewed across 11 scopes. Three confirmed findings fixed; the new source regression failed before the fix and passed after it. Both affected diagnostic suites, three selected runtime benchmarks, lint, formatting, package, and docs checks passed. No full-matrix or production Weeder rerun for this narrow follow-up. |

Verification used pinned GHC 9.14.1 and Cabal 3.16.1.0 through the Nix quality
shell with `--jobs=1`; documentation checks used the docs shell. The table
records which milestones ran full parser stress tests, rather than treating
compilation of those opt-in components as execution.

Ordinary operator dispatch adds method, closure, and kernel calls. Only exceeded
work ceilings were rebased from measurements, with 10% headroom rounded up to
two significant digits. List-allocation and continuation-depth ceilings stayed
unchanged. Final operator-milestone measurements were:

| Family       | Evaluator transitions | Applications | List cells | Maximum continuation depth | Maximum capture width |
| ------------ | --------------------- | ------------ | ---------- | -------------------------- | --------------------- |
| expression   | 23,952,562            | 2,988,713    | 111,351    | 1,061                      | 41                    |
| declarations | 10,646,980            | 1,309,276    | 66,661     | 1,076                      | 35                    |
| control-flow | 44,464,429            | 5,493,722    | 219,783    | 1,096                      | 41                    |
| operator     | 53,027,838            | 6,568,857    | 186,499    | 1,116                      | 41                    |

These are deterministic work counts, not timing ratios. The later capability,
Queue, and N-queens runtime benchmarks establish a baseline, not an A/B result
or evidence that a runtime index would improve performance.

## Deferred scope

Functional dependencies, associated types, multi-parameter classes, higher-rank
and kind polymorphism, deriving, overlapping instances, automatic destination
selection, Empty, parenthesized type-application heads, new class selectors,
new operator spellings/transport, and broader hosted/native compiler work still
require separate accepted contracts. No source-backed next implementation item
remains; the execution queue is explicitly empty.
