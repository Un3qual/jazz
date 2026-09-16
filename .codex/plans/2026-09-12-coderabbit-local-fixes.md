---
id: JN-CODERABBIT-LOCAL-FIXES-20260912
status: complete
priority: P2
size: S
kind: impl
autonomous_ready: yes
depends_on: []
last_verified: 2026-09-12
plan_section: "Approved implementation"
target_paths:
  - src/Jazz/Compiler/TypeInference.hs
  - src/Jazz/Compiler/ModuleAnalysis.hs
  - src/Jazz/Compiler/TypeInference/Pattern.hs
  - src/Jazz/Compiler/ModuleGraph.hs
  - test/Jazz/Compiler/Semantics/Runtime/HostIOTests.hs
  - test/Jazz/Compiler/Semantics/Runtime/ResolvedFixture.hs
  - test/Jazz/Compiler/Semantics/PrimitiveSemantics/EqualityOperator.hs
verification:
  - cabal test primitive-semantics-spec runtime-semantics-spec module-pipeline-contract-spec pattern-coverage-spec structured-error-diagnostics-spec --test-options=--skip-performance
  - JAZZ_CABAL_JOBS=4 bash scripts/ci/haskell-quality.sh
  - bash scripts/check-docs.sh
  - bash scripts/check-execution-queue.sh
deliverable: Fix the confirmed operator diagnostic location, three small cleanups, and two explanatory comments from the local CodeRabbit review.
supersedes: []
---

# Local CodeRabbit review fixes

> **Retired compiler scope:** [RFC 0016](../../rfcs/accepted/0016-optional-backend-removal.md)
> and [RFC 0022](../../rfcs/accepted/0022-hosted-compiler-removal.md) retired the
> optional backend, hosted compiler, and their exclusive verification. References
> to those paths, schemas, suites, and retention requirements in this record
> (including frontmatter) are historical and impose no current work or test obligations.

## Approved implementation

The maintainer requested needed fixes and a push to existing PR 158 after the local CodeRabbit CLI review. Input: `10b63aea`; review base: `b0b7dfca`. CodeRabbit 0.7.6 completed all 132 changed files and reported 14 suggestions (3 minor, 11 trivial). The review log is `/private/tmp/jazz-pr158-coderabbit-local-review.log`; the full initial assessment is `/private/tmp/jazz-pr158-coderabbit-review.md`.

- [x] Finding 14: retain the unsupported operator value node's source span. Add one table-driven source regression covering enclosing tuple and binding locations, verify it fails first, and annotate with the existing helper.
- [x] Finding 4: replace the duplicated diagnostic-list forcing helper with standard `rnf`; `forceDiagnostic` is itself `rnf`. Preserve strictness and the existing narrow module dependency direction.
- [x] Finding 7: bind each of four dependency fixtures once and reuse that artifact for evaluation and declaration lookup.
- [x] Finding 10: remove the redundant `afterBodyFacts` alias.
- [x] Findings 6 and 13: document the synthetic fixture root sentinel and the reason for standalone diagnostic-group merging.
- [x] Run the listed correctness and quality checks, inspect the diff, and prepare the verified implementation for publication to `codex/compiler-architecture-remediation`.

## Findings left unchanged

| Finding                                    | Disposition                                                                                                  |
| ------------------------------------------ | ------------------------------------------------------------------------------------------------------------ |
| 1: run scopeTests under --skip-performance | Both cases are large scale tests with time budgets; exclusion is intentional.                                |
| 2: missing binder defaults to used         | Resolved source SLet nodes always receive a binder; no supported failing input was found.                    |
| 3: named hide/show type                    | Optional API churn across multiple owners, with no misuse identified.                                        |
| 5: regroup promoted warnings               | Existing groups preserve warning category and diagnostic ordering, while severity already drives rejection.  |
| 8: remove prepared-scope seq               | No incorrect result found; removing the forcing point changes evaluation timing for no demonstrated benefit. |
| 9: optional class parameter type           | Broader representation cleanup; unary dispatch already excludes the sentinel case.                           |
| 11: change parameter fallback indices      | Current traversals preallocate every variable; declaration indices are opaque identities, not dense offsets. |
| 12: validate impl header before body       | Changes established diagnostic precedence for malformed declarations.                                        |

Hosted frontend and Bootstrap tests remain retained. No benchmark, performance, profiling, scale, or corpus-budget execution. Those components may be compiled by the clean quality gate. No fresh review run or PR-comment refresh after pushing.

## Verification record

All six selected review items are implemented. The other eight dispositions remain as recorded above. Publication to existing PR 158 is authorized; commit this verified tree, push it, verify the PR head, and stop without fetching another review round.

The source regression first failed at the reproduced wrong location (tuple opening at column 3 instead of operator at column 7). The first post-fix run exposed an overly narrow expected end column: `parsePrimaryExpr` uses `withConsumedSpan`, so the operator node spans all three characters of `(|)`. Expectations now cover the complete operator ranges in the tuple and binding examples, and both pass.

Verification completed successfully:

- Five focused correctness suites: primitive semantics, runtime semantics, module pipeline contracts, pattern coverage, and structured error diagnostics. The primitive suite passed on rerun after correcting the expected range; the other four passed on the first post-fix run.
- The complete clean Haskell quality gate: policy probes, repository-wide HLint, production build and Weeder, all-component build and full Weeder, and generated invariants. Generated invariants is the sixth distinct correctness suite. Excluded performance/scale/profiling/benchmark components were compiled only.
- Ormolu for every changed Haskell file, Prettier for the internal plan/queue, the full documentation check and its regressions, execution-queue validation, and whitespace checks.

Evidence: `/private/tmp/jazz-pr158-coderabbit-fixes-red.log`, `/private/tmp/jazz-pr158-coderabbit-fixes-correctness.log`, `/private/tmp/jazz-pr158-coderabbit-fixes-primitive-final.log`, `/private/tmp/jazz-pr158-coderabbit-fixes-quality.log`, and `/private/tmp/jazz-pr158-coderabbit-fixes-docs.log`. The initial correctness log records the test expectation correction; the primitive-final log records the successful rerun.
