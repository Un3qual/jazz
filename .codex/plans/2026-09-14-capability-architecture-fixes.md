---
id: JN-CAPABILITY-ARCHITECTURE-FIXES-001
status: complete
priority: P2
size: M
kind: impl
autonomous_ready: yes
depends_on: []
plan_section: "Implementation"
target_paths:
  - src/Jazz/Compiler/KindInference.hs
  - src/Jazz/Compiler/ModuleResolver/Names.hs
  - src/Jazz/Compiler/SemanticDeclarations.hs
  - src/Jazz/Compiler/TypeInference/Scope.hs
  - test/Jazz/Compiler/Semantics/BindingSignature/ConstraintsTests.hs
verification:
  - "cabal test all -f-full-parser-scale --jobs=1 --test-show-details=failures"
  - "fresh production-only Weeder, HLint, Ormolu, cabal check"
  - "bash scripts/check-docs.sh"
  - "git diff --check"
deliverable: "Class self-prerequisites have checked kinds; module interfaces own instance transport without legacy inline-module replay"
last_verified: 2026-09-14
---

# Capability architecture fixes

The maintainer authorized fixing each supported finding from the architecture
review of 88d2d9de..c841ba11. Preserve RFCs 0019 and 0020 and ordinary standalone
expressions. No new IR, runtime dispatch mechanism, or language feature is needed.

## Implementation

- [x] Solve self-prerequisite kind requirements with the class parameter before
      defaulting; cover invalid and constructor-kind-inferencing cases.
- [x] Remove legacy sequential inline-module resolution and inference state;
      migrate synthetic multi-module fixtures to explicit module interfaces.
- [x] Remove redundant instance environments from schemes after verifying caller
      evidence and definition-site concrete evidence remain intact.
- [x] Run affected and full regular correctness suites, fresh production Weeder,
      formatting, lint, package and documentation checks; commit and close out.

Kinds should share unknowns within one class declaration. Module identity and
imports are fixed by the coordinator; inference retains local lexical scope
restoration but does not replay dependencies. Schemes retain types and constraints;
module interfaces carry the complete instance environment. Deferred constraints
still capture use-site visibility and analyzed nodes still retain chosen evidence.

## Verification receipt

- `e0e6e1f5`: class self-prerequisites participate in the existing kind solver
  before defaulting. The new regression failed before the fix and passed after
  it, including the otherwise unconstrained constructor-kind case.
- `3a89de6c`: removed inline-module replay from resolution, analysis, and
  inference, plus per-scheme capability snapshots. The two multi-module fixtures
  now use the real module coordinator. Production/runtime evidence ownership
  remains unchanged.
- All five affected suites passed: binding-signature-coherence-spec, loader-spec,
  module-pipeline-contract-spec, runtime-semantics-spec, and stdlib-spec.
- A fresh production HIE build exposed one newly unused state modifier. Removed
  it, regenerated its metadata, and reran production Weeder successfully.
- Pinned Ormolu, whole-repository HLint, `cabal check`, Weeder policy self-tests,
  and `git diff --check` passed. No new Weeder exemptions were added.

All 62 regular suites passed on the committed implementation with GHC 9.14.1
and Cabal 3.16.1.0, using `-f-full-parser-scale --jobs=1`. This includes hosted
parser/core parity, bounded parser-scale checks, program-corpus budgets, and
standard-library performance cases. The four opt-in full parser stress suites
were not run. Documentation checks passed after synchronizing the plan and queue.

Verification logs for this run:

- `/tmp/jazz-architecture-all-regular.log`
- `/tmp/jazz-architecture-production-final.log`
- `/tmp/jazz-architecture-docs-final.log`

The queue is returned to its explicit empty state. No follow-up implementation
item or language-design decision is required by these fixes.
