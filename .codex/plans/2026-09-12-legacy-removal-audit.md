---
id: JN-PR158-LEGACY-REMOVAL-AUDIT-001
status: complete
priority: P1
size: M
kind: impl
autonomous_ready: yes
depends_on: []
last_verified: 2026-09-12
plan_section: "Removal audit"
target_paths:
  - src/Jazz/Compiler/
  - test/Jazz/
  - weeder-production.toml
  - weeder.toml
verification:
  - JAZZ_CABAL_JOBS=4 bash scripts/ci/haskell-quality.sh
  - cabal test runtime-semantics-spec --test-options=--skip-performance --jobs=4
  - bash scripts/check-execution-queue.sh
deliverable: Remove uncalled migration helpers and stale exemptions, and verify the branch's legacy-removal claims.
supersedes: []
---

# PR 158 legacy-removal audit

Audit input: `f1738904`, including the architecture branch from `b0b7dfca` and the first review-fix pass. This audit follows the existing [architecture deletion criteria](2026-09-10-compiler-architecture-remediation.md) and [quality audit](2026-09-11-compiler-quality-audit.md).

## Removal audit

- Removed 24 uncalled helpers: five compiler forcing wrappers, two parser token adapters, eleven obsolete canonical-value encoders/decoders, five runtime-fixture builders, and one test assertion. The full Weeder policy explicitly rooted these declarations, hiding them from ordinary unused-code reports. Removed their exemptions as well.
- Removed the unreachable `ApplyFunctionResultHint` return-policy constructor and its only discharge helper, `applyRuntimeFunctionResultHint`. Function result hints are normalized at the two callers before storage, as they already were in the policy insertion path. This closes R17 from the earlier quality audit without changing return/defaulting behavior.
- Removed five transitional identity re-exports from `SemanticFacts`; all consumers now import those identities directly from `CoreIdentity`. Kept `tokenStreamDrop` private to the Megaparsec stream implementation and removed its redundant explicit roots.
- Removed eight stale HLint exemptions referring to seven deleted declarations, and corrected the obsolete comment claiming constructor attachment was still awaiting migration.

## Confirmed removals and retained contracts

No active references remain to the stored `RuntimePlan`/`RuntimeObligation` representation, positional `InjectedPreludeSourceUnit` ownership, the deleted inference evidence module, or the six-map semantic-attachment database. Standalone and module entrypoints use the shared program coordinator; pure and host execution use the shared scope traversal. The hosted parser change has two active callers consuming its located constructor result. Every Haskell module/main under the compiler, CLI, tests, benchmark, and corpus support trees is registered in Cabal.

Retained APIs have actual compiler, test, or tooling consumers. In particular, direct expression inspection adapters, source-span/diagnostic compatibility projections, the AST node identity export, inference type aliases, and Megaparsec stream methods remain active. Checked semantic metadata and published artifact formats remain contracts even when the interpreter does not read every field. The distinct lazy/deferred cell stores, runtime outcomes, and dynamic method selection preserve explicit decisions from the architecture plan; they are not duplicate legacy pipelines.

## Verification

Verified with the pinned GHC 9.14.1 toolchain:

- Clean production and full-component Weeder checks passed using freshly generated HIE files. The gate also compiled every component, checked both unused-type policies, ran full HLint, and passed `generated-invariants-spec`.
- Ten affected correctness suites passed: `runtime-semantics-spec`, `binding-signature-coherence-spec`, `module-pipeline-contract-spec`, `loader-spec`, `expression-parser-spec`, `token-parser-spec`, `signature-rendering-spec`, `source-ranges-spec`, `canonical-parser-comparison-spec`, and `canonical-core-comparison-spec`. The first run found an unused import left by the deletions; it was removed and the affected runtime suite passed on rerun.
- Ormolu and `git diff --check` passed for the changed code.

Benchmark, performance, profiling, scale, and corpus-budget execution remains excluded. All components, including opt-in scale and benchmark components, were compiled by the clean Weeder gate. The full test matrix was not executed.
