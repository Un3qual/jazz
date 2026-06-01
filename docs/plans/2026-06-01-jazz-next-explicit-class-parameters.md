---
id: JN-ABSTRACTION-EXPLICIT-CLASS-PARAMETERS-001
status: ready
priority: P1
size: M
kind: impl
autonomous_ready: yes
depends_on:
  - JN-ABSTRACTION-IMPL-METHOD-BODY-METADATA-001
last_verified: 2026-06-01
plan_section: "Batch 1: Explicit class parameter metadata"
target_paths:
  - jazz-next/src/JazzNext/Compiler/Parser/AST.hs
  - jazz-next/src/JazzNext/Compiler/Parser.hs
  - jazz-next/src/JazzNext/Compiler/Parser/Lower.hs
  - jazz-next/src/JazzNext/Compiler/AST.hs
  - jazz-next/src/JazzNext/Compiler/Analyzer.hs
  - jazz-next/src/JazzNext/Compiler/TypeInference.hs
  - jazz-next/src/JazzNext/Compiler/BundledPrelude.hs
  - jazz-next/stdlib/Prelude.jz
  - jazz-next/test/JazzNext/Compiler/Parser/ParserFoundationSpec.hs
  - jazz-next/test/JazzNext/Compiler/Semantics/BindingSignatureCoherenceSpec.hs
  - jazz-next/test/JazzNext/Compiler/Modules/PreludeLoadingSpec.hs
  - jazz-next/test/JazzNext/Compiler/Semantics/BuiltinCatalogSpec.hs
verification:
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Parser/ParserFoundationSpec.hs
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/BindingSignatureCoherenceSpec.hs
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Modules/PreludeLoadingSpec.hs
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/BuiltinCatalogSpec.hs
  - bash scripts/check-execution-queue.sh
  - bash scripts/check-docs.sh
deliverable: "Make explicit class parameters the canonical active `jazz-next` abstraction metadata shape by preserving lowercase class parameter names through parser/core AST and fact seeding, rejecting missing, duplicate, or non-variable class parameters, migrating bundled-prelude class declarations to `class Eq(a)`-style headers, and keeping method dispatch, dictionaries, runtime evidence, `Self`, and solver behavior out of scope."
---

# Explicit Class Parameter Metadata Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Lock active `jazz-next` class declarations onto explicit type parameters such as `class Eq(a)` instead of any reserved `Self` type.

**Architecture:** Preserve class parameter names as declaration metadata first, then keep existing class/impl fact behavior wired through that metadata without adding dispatch. This keeps the next dispatch batch grounded in visible class arity and parameter names while avoiding runtime dictionaries or a solver in this batch.

**Tech Stack:** Haskell `jazz-next` parser/core AST/analyzer/type inference/runtime harnesses, bundled-prelude source generation, focused `runghc` specs, Markdown queue metadata.

---

## Source Verification

This child plan narrows the authoritative-syntax follow-up after inert impl
method metadata landed. The design decision is explicit:

- canonical class declarations use explicit lowercase type parameters,
  for example `class Eq(a) { equals :: a -> a -> Bool. }`;
- `Self` is not a reserved type name and must not be introduced by this batch;
- qualified method dispatch remains future work until class parameter metadata
  is represented in the active AST and bundled-prelude class declarations.

## Batch 1: Explicit Class Parameter Metadata

Scope:

- Preserve class header parameters in `SurfaceStatement` and `Statement`
  metadata, for example `SSClass span "Eq" ["a"] methods` and
  `SClass span "Eq" ["a"] methods`.
- Restrict active class parameters to lowercase identifier names.
- Reject class declarations with no explicit parameter list, such as
  `class Eq { }.`.
- Reject duplicate class parameter names deterministically.
- Reject non-variable class parameters such as `class Eq(Int) { }.` for active
  class declarations.
- Migrate bundled-prelude class declarations from empty parameter lists to
  explicit unary parameters, for example `class Eq(a) { }.`.
- Keep current concrete unary `impl` facts such as `impl Eq(Int) { }.`.
- Keep current concrete constrained-signature validation behavior, but make
  class fact seeding remember the declared class arity so wrong-arity
  constrained signatures can continue to reject deterministically.
- Preserve permanent `trait` rejection and ordinary identifier uses.

Out of scope:

- `Class::method` lookup or method dispatch,
- dictionaries or runtime evidence values,
- runtime execution of impl method bodies,
- `Self` as a reserved type,
- default methods,
- superclass semantics,
- overlap/orphan policy beyond duplicate visible facts,
- inferred class constraints,
- broad typeclass/defaulting solver behavior.

Batch target paths:

- `jazz-next/src/JazzNext/Compiler/Parser/AST.hs`
- `jazz-next/src/JazzNext/Compiler/Parser.hs`
- `jazz-next/src/JazzNext/Compiler/Parser/Lower.hs`
- `jazz-next/src/JazzNext/Compiler/AST.hs`
- `jazz-next/src/JazzNext/Compiler/Analyzer.hs`
- `jazz-next/src/JazzNext/Compiler/TypeInference.hs`
- `jazz-next/src/JazzNext/Compiler/BundledPrelude.hs`
- `jazz-next/stdlib/Prelude.jz`
- `jazz-next/test/JazzNext/Compiler/Parser/ParserFoundationSpec.hs`
- `jazz-next/test/JazzNext/Compiler/Semantics/BindingSignatureCoherenceSpec.hs`
- `jazz-next/test/JazzNext/Compiler/Modules/PreludeLoadingSpec.hs`
- `jazz-next/test/JazzNext/Compiler/Semantics/BuiltinCatalogSpec.hs`

Suggested task order:

- [ ] Update parser/core AST constructors so `SSClass` and `SClass` carry a
  `[Identifier]` parameter list before method metadata.
- [ ] Update parser lowering and every `SClass` pattern match in the target
  files to preserve class parameters.
- [ ] Change `parseCapabilityDeclaration` so `class` headers lower only
  lowercase `SurfaceConstrainedTypeName` arguments into class parameters.
- [ ] Add parser coverage proving `class Eq(a) { }.` preserves `a`, missing
  parameter lists reject, duplicate parameters reject, and concrete class
  parameters reject.
- [ ] Update analyzer/type-inference class fact seeding so visible class facts
  retain arity metadata while preserving the existing duplicate-class
  diagnostic shape.
- [ ] Update bundled-prelude generation and checked-in `Prelude.jz` so
  canonical classes use explicit unary parameters.
- [ ] Add source-pipeline/prelude coverage proving `@{Eq(Int)}: Int` still
  validates against visible `class Eq(a) { }.` plus concrete `impl Eq(Int) { }.`.
- [ ] Run the focused verification commands listed in frontmatter.

Focused verification:

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Parser/ParserFoundationSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/BindingSignatureCoherenceSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Modules/PreludeLoadingSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/BuiltinCatalogSpec.hs
bash scripts/check-execution-queue.sh
bash scripts/check-docs.sh
```

## Follow-up: Qualified Method Dispatch

After explicit class parameter metadata lands, a separate child plan should own
the first executable method dispatch batch. The currently intended shape is
qualified calls such as `Eq::equals left right` over visible unary concrete
impls, still without unqualified overloaded method names, exposed dictionaries,
superclasses, default methods, or inferred constraints.
