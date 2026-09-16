---
id: JN-COMPILER-ARCHITECTURE-REVIEW-FIXES-20260912
status: complete
priority: P2
size: M
kind: impl
autonomous_ready: yes
depends_on: []
last_verified: 2026-09-12
plan_section: "Approved implementation"
target_paths:
  - src/Jazz/Compiler/TypeInference.hs
  - src/Jazz/Compiler/TypeInference/Scope.hs
  - src/Jazz/Compiler/ModuleAnalysis.hs
  - src/Jazz/Compiler/ModuleInterface.hs
  - src/Jazz/Compiler/Runtime/Semantics.hs
  - src/Jazz/Compiler/Runtime/Types.hs
  - test/Jazz/Compiler/Semantics/Runtime/ResolvedFixture.hs
  - test/Jazz/Compiler/Semantics/Runtime/HostIOTests.hs
  - weeder-production.toml
verification:
  - cabal test name-semantics-spec runtime-semantics-spec module-pipeline-contract-spec --test-options=--skip-performance
  - JAZZ_CABAL_JOBS=4 bash scripts/ci/haskell-quality.sh
  - bash scripts/check-docs.sh
  - bash scripts/check-execution-queue.sh
deliverable: Fix all six architecture review findings and retire the unused VQualifiedMethod compatibility pattern.
supersedes: []
---

# Architecture review fixes

> **Retired compiler scope:** [RFC 0016](../../rfcs/accepted/0016-optional-backend-removal.md)
> and [RFC 0022](../../rfcs/accepted/0022-hosted-compiler-removal.md) retired the
> optional backend, hosted compiler, and their exclusive verification. References
> to those paths, schemas, suites, and retention requirements in this record
> (including frontmatter) are historical and impose no current work or test obligations.

## Approved implementation

Input: `59b5762f`. The maintainer approved all six findings in the [architecture review](2026-09-12-architecture-review.md), plus explicit retirement of the historical `VQualifiedMethod` compatibility pattern. That approval supersedes its earlier retention instruction. All changes remain local; no push or PR-comment refresh is authorized.

- [x] A1: recognize raw prepend from resolved callable identity and genuine builtin aliases; add a source-level shadowing regression that consumes the inferred result.
- [x] A2: compare nominal ADT identities for explicit result hints; cover equivalent views and preservation of unrelated existing annotations.
- [x] A3: remove the unused forward-binding diagnostic result; preserve forward-signature policy and inference modes.
- [x] A4: derive module analysis ownership from the resolved root; preserve independent warning policy and named-header ownership.
- [x] A5: store interface capability facts in `ScopeCapabilityFacts`; preserve public filtering and import deduplication.
- [x] A6: repair synthetic fixture binder/scheme and explicit-instantiation identities locally, preserving intentionally malformed semantic tests.
- [x] Retire `VQualifiedMethod`, its export/completeness alternative, and stale Weeder rationale; retain active candidate helpers and `VConstructor`.
- [x] Verify affected correctness suites, all consumer builds, formatting, HLint, both Weeder policies, documentation and queue checks; review the final diff and commit locally.

## Coordination and constraints

Implementation owners use disjoint files: coordinator owns raw-prepend inference, scope-return cleanup, and name tests; module worker owns module analysis/interface/compiler and related module tests; runtime worker owns nominal hints, runtime pattern retirement, and capability tests; fixture worker owns the resolved fixture adapter and its focused assertions. The coordinator applies the interface worker's requested `TypeInference.hs` projection edit and owns all `.codex`/Weeder changes. Test execution and commits are coordinated centrally.

The two reproduced bugs warrant regression coverage. Structural API/record cleanup reuses existing behavioral tests. Fixture assertions verify agreement between independently resolved references and semantic IDs, without depending on fixed allocation numbers.

Hosted frontend and Bootstrap tests remain retained. No benchmark, performance, profiling, scale, or corpus-budget execution. Existing deferred hosted qualification behavior and unrelated review follow-ups are outside scope. No new generic pass, remapping framework, storage redesign, or published interface is introduced.

## Execution record

Implemented all seven approved changes. The new source-shadowing test first failed with the expected `E2003`; both explicit nominal-hint cases first failed at the intended annotation/phantom-argument assertions; fixture checks first exposed the stale dependency and constructor IDs. These same assertions now pass in the normal suites.

Ten targeted correctness suites passed under pinned GHC 9.14.1 with `--skip-performance`: name semantics, runtime semantics, binding/signature coherence, module pipeline contracts, module exports, loader, prelude loading, recursive bindings, builtin catalog, and standard library. The current library builds. Both independent cross-reviews found no actionable issues; each reviewer examined another worker's implementation.

A1 shares one recognition/refinement decision and preserves builtin aliases. A3 uses the existing `inferScopeTypeWithMode` entrypoint, removing the long obsolete wrapper rather than adding another name. A5 retains the exact import deduplication and public-filter predicates. A6 replaces only synthetic explicit targets and updates constructor binder/scheme keys locally; authored types, evidence, and explicit non-placeholder targets remain untouched. The retired method pattern's historical `Show` label remains a display contract, not a pattern API.

Implementation committed locally as `3e20b7a9`. Final verification passed:

- All ten targeted suites above, plus `haskell-typeclass-contracts-spec` and `generated-invariants-spec`: 12 distinct correctness suites.
- The complete clean Haskell quality gate: policy probes, repository-wide HLint, fresh production build and Weeder, all-component build and full Weeder, and generated invariants. Every test/tooling component compiled, including profiling, opt-in scale, and benchmark components; those excluded workloads were not executed.
- Ormolu for every changed Haskell file, Prettier for the internal records, whitespace checks, the full documentation check and its regressions, and execution-queue checks. The first documentation pass rejected mismatched queue metadata; the row was aligned with its plan and the full check passed on rerun.

Evidence logs are `/private/tmp/jazz-review-fixes-a1-red.log`, `/private/tmp/jazz-review-fixes-a2-red.log`, `/private/tmp/jazz-review-fixes-a6-red.log`, `/private/tmp/jazz-review-fixes-correctness.log`, `/private/tmp/jazz-review-fixes-contracts-docs.log`, `/private/tmp/jazz-review-fixes-quality.log`, and `/private/tmp/jazz-review-fixes-docs-final.log`. The combined contracts/docs log retains the initial queue validation failure; the final docs log records the successful rerun.

All seven approved items are complete. Hosted frontend/Bootstrap retention, deferred hosted qualification behavior, excluded workload execution, and the no-push instruction remain unchanged. No new PR comments were fetched or posted. The full test matrix was not executed.
