---
id: JN-ABSTRACTION-QUALIFIED-METHOD-DISPATCH-001
status: done
priority: P1
size: M
kind: impl
autonomous_ready: no
depends_on:
  - JN-ABSTRACTION-EXPLICIT-CLASS-PARAMETERS-001
last_verified: 2026-06-02
completed_on: 2026-06-02
plan_section: "Batch 1: Single-target qualified method dispatch"
target_paths:
  - jazz-next/src/JazzNext/Compiler/Analyzer.hs
  - jazz-next/src/JazzNext/Compiler/TypeInference.hs
  - jazz-next/src/JazzNext/Compiler/Runtime.hs
  - jazz-next/src/JazzNext/Compiler/CapabilityFacts.hs
  - jazz-next/test/JazzNext/Compiler/Parser/ParserFoundationSpec.hs
  - jazz-next/test/JazzNext/Compiler/Semantics/BindingSignatureCoherenceSpec.hs
  - jazz-next/test/JazzNext/Compiler/Semantics/RuntimeSemanticsSpec.hs
verification:
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Parser/ParserFoundationSpec.hs
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/BindingSignatureCoherenceSpec.hs
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/RuntimeSemanticsSpec.hs
  - bash scripts/check-execution-queue.sh
  - bash scripts/check-docs.sh
deliverable: "Add the first executable qualified method dispatch slice for calls like `Eq::equals 1 1` when exactly one visible concrete impl provides that method body, preserving class-qualified method syntax, source/type/runtime validation, and keeping overloaded dispatch, dictionaries, default methods, superclasses, inferred constraints, bundled-prelude method bodies, module export/import behavior, and broad solver work out of scope."
---

# Qualified Method Dispatch Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Enable the first executable active-path class method call shape:
`Class::method arg...`, backed by a single visible concrete `impl` method body.

**Architecture:** Treat class-qualified method names as an explicit method
namespace, not as unqualified overloaded values. Analyzer/type/runtime phases
should collect method metadata from visible `class` declarations and
method-bearing concrete `impl` declarations, then expose a callable method only
when there is exactly one visible concrete method body for the qualified
`Class::method` key. This keeps the batch useful for direct no-prelude or
explicit-prelude sources without committing to dictionaries, implicit
overload resolution, or a typeclass solver.

**Tech Stack:** Haskell `jazz-next` analyzer/type/runtime, existing parser
qualified-name syntax, focused `runghc` specs, Markdown queue metadata.

---

## Source Verification

The prior explicit-class-parameter batch landed canonical class headers such as
`class Eq(a) { ... }`, inert concrete `impl` method body metadata, declared
class arity facts, and permanent `trait` rejection. The next smallest
implementation step is a direct qualified method call over one visible concrete
method-bearing impl.

The existing parser already lowers `Eq::equals` into a qualified identifier via
the same `SEQualifiedVar` / `EVar "Eq::equals"` representation used by alias
lookup. This batch should lock that parser behavior with a focused test, then
make analyzer/type/runtime understand the class-method namespace.

## Batch 1: Single-target qualified method dispatch

Scope:

- Accept expression calls such as:

  ```jz
  class Eq(a) {
    equals :: a -> a -> Bool.
  }.

  impl Eq(Int) {
    equals = \(left) -> \(right) -> left == right.
  }.

  result = Eq::equals 1 1.
  result.
  ```

- Resolve only class-qualified method names of the form `Class::method`.
- Require a visible `class Class(a)` declaration with a matching
  signature-only method declaration.
- Require exactly one visible method-bearing concrete `impl Class(Target)` body
  defining that method.
- Type the qualified method value from the class method signature with the
  class parameter substituted by the concrete impl target.
- Execute the qualified method value by evaluating the selected impl method
  body as an ordinary callable value.
- Keep class/impl declarations inert as ordinary statements; only explicit
  qualified method references become callable.
- Preserve existing duplicate class, duplicate class method, duplicate impl
  method, duplicate impl fact, non-binding impl item, and non-concrete
  method-bearing impl diagnostics.
- Reject missing class methods, missing concrete method bodies, and ambiguous
  multiple visible method-bearing impl bodies deterministically.
- Keep `class`/`impl`/`trait` ordinary identifier behavior unchanged outside
  declaration-shaped forms.

Out of scope:

- Unqualified overloaded method names such as `equals 1 1`,
- dispatch selected from argument types when multiple concrete impl method
  bodies are visible,
- dictionaries or runtime evidence values,
- default methods,
- superclass semantics,
- inferred class constraints,
- generalized constrained signatures or defaulting,
- bundled-prelude method bodies,
- module graph export/import behavior for class methods,
- overlap/orphan policy beyond existing duplicate visible facts.

Batch target paths:

- `jazz-next/src/JazzNext/Compiler/Analyzer.hs`
- `jazz-next/src/JazzNext/Compiler/TypeInference.hs`
- `jazz-next/src/JazzNext/Compiler/Runtime.hs`
- `jazz-next/src/JazzNext/Compiler/CapabilityFacts.hs`
- `jazz-next/test/JazzNext/Compiler/Parser/ParserFoundationSpec.hs`
- `jazz-next/test/JazzNext/Compiler/Semantics/BindingSignatureCoherenceSpec.hs`
- `jazz-next/test/JazzNext/Compiler/Semantics/RuntimeSemanticsSpec.hs`

Suggested task order:

- [x] Add parser coverage proving `Eq::equals` still lowers to the existing
  class-qualified identifier shape without adding a new AST node.
- [x] Add shared method-key helpers in `CapabilityFacts.hs` if useful for
  rendering and comparing `Class::method` keys.
- [x] Update analyzer scope traversal so `EVar "Class::method"` is not reported
  as unbound when the visible class declares that method and exactly one visible
  concrete impl body defines it.
- [x] Update type inference to collect visible class method signatures and
  method-bearing concrete impl bodies, substitute the class parameter with the
  concrete impl target, and infer the qualified method value from the resulting
  concrete method signature.
- [x] Add deterministic type diagnostics for missing class method metadata,
  missing method-bearing impl bodies, and ambiguous method-bearing impl bodies.
- [x] Update runtime scope traversal so `EVar "Class::method"` evaluates to the
  selected concrete impl method body as an ordinary callable value.
- [x] Add source-pipeline coverage for successful direct `Eq::equals 1 1`,
  type mismatch through the substituted method signature, missing impl method
  body rejection, and ambiguous method-bearing impl body rejection.
- [x] Add runtime coverage proving the selected impl method body executes and
  remains inert unless referenced through `Class::method`.
- [x] Run the focused verification commands listed in frontmatter.

Completion (`2026-06-02`): this batch landed with focused parser, source
pipeline, and runtime coverage. The implementation keeps method lookup
explicit to `Class::method`, requires prior visible class method metadata for
method-bearing impl bodies, validates concrete impl method bodies against the
substituted class method signature, rejects missing metadata, missing impl
method bodies, impl-before-class method metadata, and ambiguous method bodies
deterministically, and evaluates the selected concrete impl body as an ordinary
callable runtime value. Unqualified overloads, typed overload selection across
multiple concrete impls, dictionaries, default methods, superclasses, inferred
constraints, bundled-prelude method bodies, module export/import method
behavior, and broader runtime evidence remain out of scope.

Focused verification:

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Parser/ParserFoundationSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/BindingSignatureCoherenceSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/RuntimeSemanticsSpec.hs
bash scripts/check-execution-queue.sh
bash scripts/check-docs.sh
```

## Follow-up: Typed overload dispatch

On `2026-06-02`, queue curation split typed overload dispatch across multiple
visible concrete impl method bodies into a dedicated child plan:

- `docs/plans/2026-06-02-jazz-next-typed-qualified-method-dispatch.md`

That child owns the explicit selection rule and diagnostics for unresolved or
ambiguous argument types. Module/prelude method behavior remains separate
unless a child plan names exact target paths and focused verification.
