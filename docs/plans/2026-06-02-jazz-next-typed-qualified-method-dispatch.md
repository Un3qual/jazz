---
id: JN-ABSTRACTION-TYPED-QUALIFIED-METHOD-DISPATCH-001
status: done
priority: P1
size: M
kind: impl
autonomous_ready: yes
depends_on:
  - JN-ABSTRACTION-QUALIFIED-METHOD-DISPATCH-001
last_verified: 2026-06-02
plan_section: "Batch 1: Typed qualified method dispatch"
target_paths:
  - jazz-next/src/JazzNext/Compiler/Analyzer.hs
  - jazz-next/src/JazzNext/Compiler/CapabilityFacts.hs
  - jazz-next/src/JazzNext/Compiler/TypeInference.hs
  - jazz-next/src/JazzNext/Compiler/Runtime.hs
  - jazz-next/test/JazzNext/Compiler/Semantics/BindingSignatureCoherenceSpec.hs
  - jazz-next/test/JazzNext/Compiler/Semantics/RuntimeSemanticsSpec.hs
verification:
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/BindingSignatureCoherenceSpec.hs
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/RuntimeSemanticsSpec.hs
  - bash scripts/check-execution-queue.sh
  - bash scripts/check-docs.sh
deliverable: "Select an explicit `Class::method` target by typed argument flow when several visible concrete impl method bodies exist, preserving deterministic unresolved and ambiguous diagnostics and no dictionaries or unqualified overloads."
---

# Typed Qualified Method Dispatch Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use
> superpowers:subagent-driven-development (recommended) or
> superpowers:executing-plans to implement this plan task-by-task. Steps use
> checkbox (`- [ ]`) syntax for tracking.

**Goal:** Extend explicit `Class::method` dispatch beyond the current
single-target rule so a call can select the one visible concrete impl method
body whose substituted signature matches the typed argument flow.

**Architecture:** Keep the method namespace explicit and class-qualified.
Analyzer, type inference, and runtime should continue collecting visible class
method signatures and concrete impl method bodies. When multiple concrete
method bodies exist for the same `Class::method` key, type inference may use
the already-typed call arguments to choose exactly one concrete target. The
runtime should execute the same selected method body that type inference
accepted; no dictionary value or unqualified overload name is introduced.

**Tech Stack:** Haskell `jazz-next` analyzer/type/runtime, shared capability
fact helpers, focused source-pipeline and runtime `runghc` specs, Markdown
queue metadata.

---

## Source Verification

The completed single-target dispatch child plan
`docs/plans/2026-06-01-jazz-next-qualified-method-dispatch.md` left typed
overload selection out of scope. Its follow-up section requires a separate
child plan with an explicit selection rule, unresolved/ambiguous diagnostics,
and focused active-path tests before it returns to `Ready Now`.

## Batch 1: Typed Qualified Method Dispatch

Scope:

- Continue accepting explicit calls such as `Eq::equals left right` only
  through the class-qualified `Class::method` namespace.
- When exactly one visible concrete impl method body exists for the method,
  preserve the current single-target behavior.
- When several visible concrete impl method bodies exist, infer the call by
  substituting each candidate's concrete impl target into the declared class
  method signature and selecting the only candidate whose argument types match
  the call.
- Reject calls when no candidate matches the typed argument flow.
- Reject calls when more than one candidate remains possible after argument
  typing.
- Keep type mismatch diagnostics deterministic and attached to the call site or
  the relevant method reference, consistent with the current source-pipeline
  diagnostics.
- Ensure runtime execution uses the same selected concrete impl method body for
  accepted calls.
- Preserve validation of impl method bodies against the substituted class
  method signature.

Out of scope:

- Unqualified overloaded method names such as `equals left right`,
- dictionary passing or runtime evidence values,
- default methods,
- superclass semantics,
- inferred class constraints,
- generalized constrained signatures or defaulting,
- overlap/orphan policy beyond existing duplicate visible facts,
- bundled-prelude method bodies,
- module graph export/import behavior for class methods.

Batch target paths:

- `jazz-next/src/JazzNext/Compiler/Analyzer.hs`
- `jazz-next/src/JazzNext/Compiler/CapabilityFacts.hs`
- `jazz-next/src/JazzNext/Compiler/TypeInference.hs`
- `jazz-next/src/JazzNext/Compiler/Runtime.hs`
- `jazz-next/test/JazzNext/Compiler/Semantics/BindingSignatureCoherenceSpec.hs`
- `jazz-next/test/JazzNext/Compiler/Semantics/RuntimeSemanticsSpec.hs`

Suggested task order:

- [x] Add source-pipeline coverage with two visible concrete impl method bodies
  for the same class method and a call whose argument types select one target.
- [x] Add source-pipeline coverage for no typed candidate and for ambiguous
  candidates after argument typing.
- [x] Extend shared capability facts if the selected method target needs to be
  represented explicitly across type inference and runtime.
- [x] Update type inference so `Class::method` application can filter method
  candidates by substituted argument types while preserving current
  single-target behavior.
- [x] Update runtime selection to execute the concrete method body accepted by
  the typed selection path.
- [x] Add runtime coverage proving two concrete method bodies can coexist and
  calls execute the target selected by argument types.
- [x] Run the focused verification commands listed in frontmatter.

Focused verification:

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/BindingSignatureCoherenceSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/RuntimeSemanticsSpec.hs
bash scripts/check-execution-queue.sh
bash scripts/check-docs.sh
```

Completion evidence (`2026-06-02`): implemented typed candidate selection for
explicit qualified methods across analyzer/type/runtime state, including
deterministic no-match and ambiguity behavior plus runtime candidate matching
for ADT and width-specific numeric values. Focused verification passed with
`BindingSignatureCoherenceSpec`, `RuntimeSemanticsSpec`, queue validation, and
docs validation.
