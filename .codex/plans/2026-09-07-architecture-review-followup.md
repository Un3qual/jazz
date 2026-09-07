---
id: JN-COMPILER-ARCHITECTURE-REVIEW-001
status: ready
priority: P1
size: M
kind: impl
autonomous_ready: yes
depends_on: []
plan_section: "Approved fixes"
target_paths:
  - src/Jazz/Compiler/AST.hs
  - src/Jazz/Compiler/TypeInference/Analyzed.hs
  - src/Jazz/Compiler/Runtime/Engine.hs
verification:
  - nix --extra-experimental-features 'nix-command flakes' develop --command cabal test all -fdevelopment --test-show-details=direct --jobs=1
deliverable: "Remove unused analyzed data and repeated compiler plumbing; execute runtime evidence once while preserving behavior."
last_verified: 2026-09-07
---

# Architecture Review Follow-up Implementation Plan

**Goal:** Implement the five review recommendations approved on 2026-09-07, inline.

**Architecture:** Keep nominal identities, phase-indexed core, checked construction,
and analyzed-core interpretation. Remove unused projections and share existing
operations. Give runtime evidence one execution point without changing nullary
methods, partial applications, explicit instantiation, or deferred host execution.

**Tech Stack:** Existing Haskell, GHC, Cabal, Nix, containers, and test harness.

**Spec:** The approved review in this task and the preserved behavioral constraints
in [architecture design](2026-08-31-jazz-compiler-architecture-simplification-design.md).

## Approved fixes

- [x] Delete analyzed capability snapshots from schemes and modules and their
      projection functions. Preserve inference environments and checked method
      signatures. Move method-identity tests to declaration facts; remove tests
      that only assert deleted copies.
- [x] Define `CoreNameAt phase = Name (CoreUserNameAt phase)` and replace
      `CorePhaseNames` with the ordinary ordering constraint on its payload.
      Remove identical instances and proxy arguments in recursive-binding code.
- [x] Put expression, pattern, and statement node access beside the AST and use
      those functions for existing fact, identity, and runtime-plan reads.
- [x] Replace `mergedUnifiedType state left right` with `resolveType state left`;
      implement solver substitution through `substituteSemanticVariables`, keeping
      recursive resolution of replacements.
- [ ] Trace evidence selection before forcing and result-plan execution. Retain
      ordered obligations while consuming selection once; remove the evidence-only
      pre-scan. Verify nullary, explicit, polymorphic, partial, stored, imported,
      and deferred method behavior with the runtime capability and host suites.
- [ ] Format changed sources, compile all enabled components with development
      warnings, run focused gates during implementation and all default suites at
      the end. Run executable examples and repository checks. Commit green
      milestones and record final verification.

Future-task check: Tasks 12-15 are complete. No accepted future child owns these
fixes. Typed Core interpreter parity and a cutover remain separate future work.
Stop for a new harmful abstraction or unresolved behavior/design tradeoff;
ordinary implementation choices and the five fixes are already approved.

## Verification progress

The structural cleanup passes the development-warning build and the runtime
semantics, recursive bindings, module pipeline, Haskell typeclass, and binding
signature suites on 2026-09-07. The solver chain regression still resolves
replacements recursively, and method signature checks now read declaration facts.
