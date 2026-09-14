---
id: JN-GENERIC-CAPABILITIES-LEFTOVERS-001
status: complete
priority: P2
size: M
kind: impl
autonomous_ready: yes
depends_on: []
plan_section: "Audit and cleanup"
target_paths:
  - src/Jazz/Compiler/CoreIdentity.hs
  - src/Jazz/Compiler/ModuleRuntime.hs
  - src/Jazz/Compiler/Runtime/Semantics.hs
  - src/Jazz/Compiler/SemanticDeclarations.hs
  - src/Jazz/Compiler/SemanticFacts.hs
  - src/Jazz/Compiler/TypeInference/Scope.hs
  - src/Jazz/Compiler/TypeInference/Analyzed.hs
  - src/Jazz/Compiler/TypeInference/Diagnostics.hs
  - jazz/compiler/ParserExpression.jz
  - test/Jazz/Compiler/Modules/ModulePipelineContractSpec.hs
  - test/Jazz/Compiler/Semantics/Runtime/CapabilitiesTests.hs
  - weeder-production.toml
  - weeder.toml
  - .hlint.yaml
verification:
  - "cabal test all -f-full-parser-scale --jobs=1 --test-show-details=failures"
  - "bash scripts/ci/haskell-quality.sh"
  - "bash scripts/check-docs.sh"
  - "bash scripts/check-execution-queue.sh"
  - "git diff --check"
deliverable: "Weeder and independent source audit with confirmed leftovers removed"
last_verified: 2026-09-14
---

# Generic capabilities legacy cleanup

The maintainer requested a fresh Weeder pass followed by an independent source
analysis of the complete branch, including replacements that remain reachable.
Scope starts at the merge base 88d2d9de and audit baseline 82962f01. Preserve
accepted RFC 0019 behavior and historical RFCs. Remove confirmed leftovers in
verified commits; do not add speculative abstractions or tests for simple deletion.

## Audit and cleanup

- [x] Run fresh production and whole-tree Weeder checks before source cleanup.
- [x] Trace the old capability, method, type-application, and instance-import paths
      against their replacements; inspect retained roots and test-only APIs.
- [x] Check all 183 library renames, public map replacement, hosted consumers,
      fixtures, docs, and unused private Jazz helpers outside Weeder coverage.
- [x] Remove confirmed leftovers, run appropriate verification, and commit.
- [x] Record findings, retained contracts, verification, and close the queue row.

## Audit findings

The fresh production and whole-tree Weeder checks both passed at 82962f01,
including policy self-tests, HLint, all component builds, and generated invariants.
The independent source pass identified these reachable or non-Haskell leftovers:

1. `RuntimeCapabilityMethodExport` publishes each class method a second time.
   Ordinary `ModuleValueBinding` entries already carry the same nominal reference;
   validated class selectors already include the method value names.
2. `runtimeValueExactlyMatchesConstraint` has no production caller. Its new
   production exemption and one dispatch-inspection test preserve the removed
   runtime candidate-selection behavior. The useful remaining tests can inspect
   numeric metadata and annotations directly.
3. `implementationParameterKinds` is validated then stored without a consumer.
   `evidenceSubstitution` is copied, finalized, and compared but never interpreted.
   Keep kind checking and selected target/prerequisite evidence; remove their
   unused payloads and the trial substitution return value.
4. Invalid-signature reporting still rebuilds a concrete-only instance catalog
   and describes the removed constraint-variable restriction. Signature parsing
   now validates class existence/arity and kinds; the generic solver owns missing
   instance diagnostics. Remove the obsolete lookup and its derived state views.
5. The `ClassMethodType` pattern manufactures old unconstrained method schemes
   for tests. The sole production match reads only the result type. Use the actual
   class method scheme and direct test fixtures.
6. `stopsAtPipeOr` in the hosted parser has no caller or export. Delete it along
   with two stale HLint exemptions and pass-through aliases left by the migration.

7. Analyzed method signatures and implementation target copies were only read
   by old projection tests. The dictionary evaluator uses neither. Remove the
   extra analyzed type, projection function, invariant variant, and copied target
   payload; retain checked declaration identities and preparation validation.
   Move the method-parameter test to the authoritative checked scheme, and remove
   the synthetic test for the deleted projection boundary. Remove the obsolete
   type from both Weeder retention policies. The final policy scan also removed
   stale entries for `ExpressionEvidenceSeed` and `ModuleImportMode`, which have
   no definitions anywhere in maintained Haskell sources.

## Retained contracts

- `__kernel_map` is used by `Mappable(List)` and by direct kernel tests.
- `ConcreteImplFact` remains an exact nominal key for the analyzer's published
  duplicate-declaration diagnostic (E1005), not an instance solver/catalog.
- Numeric compatibility checks still serve arithmetic and structural equality.
  Constructor/list/function type views and datatype fallback kinds remain active.
- Public library spellings were checked against all 183 rename rows across tracked
  source, tests, consumers, current documentation, and website tooling. Remaining
  same-named tokens are current exports (such as Map.mapValues), Haskell APIs, or
  private kernel primitive names. There are no old public compatibility aliases.
- Historical RFCs and execution receipts remain historical records.

## Verification progress

The first cleanup batch passed all seven focused suites: runtime semantics,
runtime observation, module pipeline contracts, binding/signature coherence,
Haskell typeclass contracts, profiling, and hosted parser control flow/patterns.
Changed-file Ormolu and HLint passed. Compiler warnings exposed two newly unused
imports during cleanup; both were removed before the successful run.

First verified cleanup commit: `27364518`.

All 62 regular suites have now passed against the completed compiler cleanup,
including bounded parser scale, corpus budgets, stdlib performance, and hosted
parity. The full command stopped after 27 passing suites on a missing import in
the revised module-contract test. That test-only import was fixed and the 35
remaining suites passed. No compiler source changed between those successful
runs. The full regular build also passed. Clean final Weeder verification completed successfully.

## Final verification

- Fresh production and whole-tree Weeder both pass, with policy self-tests,
  whole-tree HLint, all component builds, and generated invariants.
- The first final Weeder run exposed `capabilityExportName`, whose only caller
  was the deleted duplicate runtime export path. Removed it and its unused name
  imports, then reran the clean quality gate successfully. This final source
  change has no callers and does not alter the behavior covered by the 62 regular
  suites above.
- Both full regular test inventory and successful test logs were compared by
  suite name: all 62 passed. The four opt-in full-scale workloads were compiled
  by the quality gate but were not rerun for this cleanup.
- Final symbol scanning finds none of the 20 removed declarations in active
  source, tests, HLint exemptions, or Weeder policies. Retained roots were checked
  against actual source declarations. All 183 public library renames are complete.
- Pinned Ormolu, documentation checks, queue checks, and `git diff --check` pass.
- The cleanup removes a net 197 Haskell implementation lines, 9 Jazz implementation
  lines, and 53 test lines. No dependencies or performance budgets changed.

Verified implementation commits: `27364518` and `3a75cabd`. The closeout commit
contains the final caller-free export helper deletion and this receipt. No further
branch-related legacy or unused-code leftovers were found in the audit.
