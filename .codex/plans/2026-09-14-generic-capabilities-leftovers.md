---
id: JN-GENERIC-CAPABILITIES-LEFTOVERS-001
status: ready
priority: P2
size: M
kind: impl
autonomous_ready: yes
depends_on: []
plan_section: "Audit and cleanup"
target_paths:
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
- [ ] Trace the old capability, method, type-application, and instance-import paths
      against their replacements; inspect retained roots and test-only APIs.
- [ ] Check all 183 library renames, public map replacement, hosted consumers,
      fixtures, docs, and unused private Jazz helpers outside Weeder coverage.
- [ ] Remove confirmed leftovers, run appropriate verification, and commit.
- [ ] Record findings, retained contracts, verification, and close the queue row.

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
