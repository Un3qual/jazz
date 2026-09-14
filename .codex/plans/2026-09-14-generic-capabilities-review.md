---
id: JN-GENERIC-CAPABILITIES-REVIEW-001
status: ready
priority: P2
size: M
kind: impl
autonomous_ready: yes
depends_on: []
plan_section: "Implementation"
target_paths:
  - weeder-production.toml
  - src/Jazz/Compiler/Runtime/Engine.hs
  - src/Jazz/Compiler/Runtime/Types.hs
  - src/Jazz/Compiler/Runtime/Semantics.hs
  - src/Jazz/Compiler/TypeInference.hs
  - src/Jazz/Compiler/TypeInference/Capabilities.hs
  - src/Jazz/Compiler/TypeInference/ImplChecking.hs
  - src/Jazz/Compiler/TypeInference/Scope.hs
  - src/Jazz/Compiler/TypeInference/Analyzed.hs
  - src/Jazz/Compiler/SemanticFacts.hs
  - test/Jazz/Compiler/Semantics/Runtime/CapabilitiesTests.hs
  - test/Jazz/Compiler/Semantics/BindingSignature/InferenceOwnershipTests.hs
  - test/Jazz/Compiler/Semantics/Runtime/Fixtures.hs
  - test/Jazz/Compiler/Stdlib/GenericCapabilitiesTests.hs
  - test/fixtures/stdlib/generic/GenericLibrary.jz
  - test/fixtures/stdlib/generic/GenericLaws.jz
verification:
  - "cabal test all -f-full-parser-scale --jobs=1 --test-show-details=failures"
  - "cabal build all -f-full-parser-scale --jobs=1"
  - "bash scripts/ci/haskell-quality.sh"
  - "bash scripts/check-docs.sh"
  - "bash scripts/check-execution-queue.sh"
  - "git diff --check"
deliverable: "Remove redundant capability dispatch machinery, prepare evidence captures once, and make law failures identifiable."
last_verified: 2026-09-14
---

# Generic capabilities review fixes

The maintainer approved remediation of the six findings from the review of
88d2d9de..37710e02. Execute inline in the current worktree. The accepted
[RFC 0019](../../rfcs/accepted/0019-generic-capabilities-and-library-names.md)
remains the public contract; this work preserves behavior.

## Implementation

- [x] Remove the runtime candidate catalog. Retain one qualified-method token
      and the selected dictionary's direct method cells. Remove its inspection
      test while retaining behavioral method/default/host-effect coverage.
- [x] Route method references through ordinary application inference. Remove
      the specialized application spine and unused failure policy. Remove
      pass-through import filtering parameters; visibility stays in resolution.
- [x] Narrow scheme conversion and method-checker inputs to the data consumed;
      stop fabricating class and implementation records for adapters.
- [x] Finalize evidence capture requirements once on analyzed lambdas. Reuse
      checked dictionary identities and the existing draft finalization path;
      runtime capture uses Map.restrictKeys. Preserve dictionary completeness,
      defaults, recursive forwarding, and lexical ownership. Do not add an IR,
      generic pass framework, or runtime evidence-selection fallback.
- [x] Replace positional capability-law goldens with named checks while keeping
      shared fixture execution, changed-element-type coverage, empty cases,
      custom element evidence, and fold order.
- [ ] Run focused suites after each coherent change and commit verified batches.
      Then run the verification list in pinned Nix shells, serializing Cabal.
      Keep runtime budget limits unchanged. The four opt-in full parser-scale
      workloads were run for the original RFC; this refactor runs the normal
      full suite, including bounded parser-scale and corpus budgets.
- [ ] Review the final diff, record verification, close the queue row, and commit.

## Verification receipt

Baseline at 37710e02: the review reran 227 binding-signature, 255 runtime,
and 41 module-pipeline checks successfully. No new tests are required merely
for helper deletion; add a regression only for a concrete uncovered invariant.

Runtime catalog removal: runtime-semantics-spec, runtime-observation-spec,
and module-pipeline-contract-spec pass with serialized pinned Cabal. The
obsolete catalog inspection test was removed; behavioral tests remain.

Ordinary method inference, direct scheme conversion, checker inputs, and import
API cleanup: binding-signature-coherence-spec, runtime-semantics-spec, and
module-pipeline-contract-spec pass with serialized pinned Cabal.

Lambda capture preparation: runtime-semantics-spec, runtime-observation-spec,
module-pipeline-contract-spec, and binding-signature-coherence-spec pass.
Changed compiler modules have no HLint hints. Capture sets are finalized by the
existing lambda drafts using their checked declaration facts; nested lambdas
reuse completed sets. The runtime no longer walks bodies or scans environments
to discover evidence captures. Removed the deleted catalog type from Weeder
policy as well.

Named library checks: stdlib-spec passes, including its performance cases.
The two fixtures now report 70 labeled checks. Deliberately changing one
expectation in each fixture produced exactly two failures with the expected
labels; restoring them returned every stdlib behavior test to passing.
No test harness or repeated module-graph setup was added.
