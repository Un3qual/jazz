# Jazz-Next Generic Signature Review Remediation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Correct the definition-site semantics and remove the representation duplication found during the PR #100 type-theory and code-quality review.

**Architecture:** Keep the approved single `SignatureType` tree. Check declared variables as rigid skolems while inferring a signed binding, require implementation obligations to be entailed by the declared constraints, and continue instantiating the resulting scheme freshly at use sites. Centralize signature conversion and structured failures in one inference module, and construct runtime templates with real `TypeVariable` nodes.

**Tech Stack:** Haskell 2010, the existing `jazz-next` rank-1 solver, focused `runghc` suites, Cabal, and repository gates.

## Global Constraints

- Modify only `jazz-next/` and active documentation.
- Preserve rank-1, first-order named types and the backend-neutral frontend boundary.
- Keep unsigned direct constructor aliases monomorphic; explicit generic signatures override that policy.
- Reject method-local class variables until class method schemes are explicitly designed.
- Implement every behavior change test-first.

---

### Task 1: Make signed definitions honor universal signatures

**Files:**
- Modify: `jazz-next/src/JazzNext/Compiler/TypeInference/{State,Solver,Scope,Diagnostics}.hs`
- Test: `jazz-next/test/JazzNext/Compiler/Semantics/BindingSignature/{DiagnosticsTests,ConstraintsTests}.hs`

**Interfaces:**
- Consumes: `PendingSignatureType.pendingSignatureVariableOrder` and declared `TypeSchemeConstraint` values.
- Produces: rigid definition-site checking and an `E2009` diagnostic for implementation constraints not declared by the signature.

- [x] Add regressions rejecting specialization, variable collapse, named-ADT specialization, undeclared numeric constraints, and undeclared class/method constraints.
- [x] Run `BindingSignatureCoherenceSpec.hs` and confirm those regressions fail against the current implementation.
- [x] Track rigid type-variable ids in `SolverState`; permit flexible variables to bind to rigid variables, but never bind a rigid variable to a concrete type or a distinct rigid variable.
- [x] During a signed RHS check, mark declared variables rigid and restore the prior rigid set before publishing the binding.
- [x] Require inferred class and primitive obligations on signed variables to be entailed by matching declared constraints; preserve internal evidence only after that check succeeds.
- [x] Run the focused suite and confirm the new regressions pass.

### Task 2: Generalize explicitly signed constructor aliases

**Files:**
- Modify: `jazz-next/src/JazzNext/Compiler/TypeInference/Scope.hs`
- Test: `jazz-next/test/JazzNext/Compiler/Semantics/BindingSignature/GeneralizationTests.hs`

**Interfaces:**
- Consumes: an adjacent explicit generic signature and a direct constructor alias RHS.
- Produces: a `SchemeTypeBinding` instantiated freshly per use.

- [x] Add the `make :: a -> Box(a); make = Box` two-use regression and observe the second use fail.
- [x] Apply the constructor-alias monomorphism exception only to unsigned inference.
- [x] Run the focused suite and confirm both unsigned monomorphism and signed polymorphism tests pass.

### Task 3: Reject unsupported method-local variables at declaration time

**Files:**
- Modify: `jazz-next/src/JazzNext/Compiler/TypeInference/{Scope,Diagnostics}.hs`
- Test: `jazz-next/test/JazzNext/Compiler/Semantics/BindingSignature/ConstraintsTests.hs`

**Interfaces:**
- Consumes: class parameters and every variable in each method signature payload.
- Produces: declaration-site `E2009` when a method mentions variables outside the class parameter set.

- [x] Add a regression for `class C(a) { f :: b -> b. }.` and observe it compile incorrectly.
- [x] Compare method signature variables with the class parameter set before seeding class facts.
- [x] Run the focused suite and confirm the declaration now fails at the method span.

### Task 4: Centralize signature conversion and simplify runtime templates

**Files:**
- Create: `jazz-next/src/JazzNext/Compiler/TypeInference/Signature.hs`
- Modify: `jazz-next/jazz-next.cabal`
- Modify: `jazz-next/src/JazzNext/Compiler/{CapabilityFacts,Runtime}.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/TypeInference/{Capabilities,Diagnostics,Scope}.hs`

**Interfaces:**
- Produces: `SignatureTypeFailure`, one recursive validation/conversion boundary, deterministic failure rendering, variable collection, and direct `TypeVariable` runtime templates.

- [x] Centralize named-type validation, arity checking, and conversion in the new module with `Either SignatureTypeFailure ExpressionType`, reusing the canonical variable collector.
- [x] Replace the parallel diagnostic traversal with structured failure rendering.
- [x] Replace synthetic `TDataType \"tN\" []` template sentinels with direct `TypeVariable` construction.
- [x] Run parser, binding-signature, module-resolution, loader, and runtime focused suites.

### Task 5: Align docs and verify the complete branch

**Files:**
- Modify: `docs/superpowers/specs/2026-07-10-jazz-next-generic-named-types-design.md`
- Modify: `docs/superpowers/plans/2026-07-10-jazz-next-generic-named-types.md`

- [x] Document definition-site skolemization, constraint entailment, signed constructor aliases, and the current class-method-variable boundary.
- [x] Run `git diff --check`, focused suites, `bash jazz-next/scripts/test-warning-config.sh`, and `cabal build all`.
- [x] Commit the remediation and push `codex/bootstrap-generic-named-types`.
