---
id: JN-ABSTRACTION-BUNDLED-PRELUDE-EQ-INT-METHOD-001
status: done
priority: P1
size: S
kind: impl
autonomous_ready: yes
depends_on:
  - JN-ABSTRACTION-QUALIFIED-METHOD-DISPATCH-001
last_verified: 2026-06-02
plan_section: "Batch 1: Bundled prelude Eq(Int) method body"
target_paths:
  - jazz-next/src/JazzNext/Compiler/BundledPrelude.hs
  - jazz-next/src/JazzNext/Compiler/PreludeContract.hs
  - jazz-next/src/JazzNext/Compiler/TypeInference.hs
  - jazz-next/src/JazzNext/Compiler/Runtime.hs
  - jazz-next/stdlib/Prelude.jz
  - jazz-next/test/JazzNext/Compiler/Modules/PreludeLoadingSpec.hs
  - jazz-next/test/JazzNext/Compiler/Semantics/BindingSignatureCoherenceSpec.hs
  - jazz-next/test/JazzNext/Compiler/Semantics/RuntimeSemanticsSpec.hs
verification:
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Modules/PreludeLoadingSpec.hs
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/BindingSignatureCoherenceSpec.hs
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/RuntimeSemanticsSpec.hs
  - bash scripts/check-execution-queue.sh
  - bash scripts/check-docs.sh
deliverable: "Add one bundled-prelude `Eq(Int).equals` method body and prove default prelude loading exposes it to explicit `Eq::equals` calls while no-prelude mode and empty explicit prelude behavior remain deterministic."
---

# Bundled Prelude Eq(Int) Method Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use
> superpowers:subagent-driven-development (recommended) or
> superpowers:executing-plans to implement this plan task-by-task. Steps use
> checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add the first executable bundled-prelude class method body without
turning class methods into builtin catalog entries or broadening overload
dispatch.

**Architecture:** Keep method bodies as ordinary source-level prelude
metadata. The bundled prelude should declare the `Eq(a).equals` method
signature and provide exactly one concrete `Eq(Int)` method body. Default
prelude loading should make that method visible to explicit `Eq::equals` calls,
while no-prelude mode and an explicit minimal prelude should preserve their
current deterministic missing-method behavior.

**Tech Stack:** Bundled prelude source generation, checked-in `Prelude.jz`,
prelude contract validation, active `jazz-next` type/runtime dispatch, focused
prelude and runtime `runghc` specs, Markdown queue metadata.

---

## Source Verification

The completed single-target qualified dispatch plan made explicit
`Class::method` references executable when one visible concrete impl provides a
method body. It deliberately kept bundled-prelude method bodies out of scope.
The active bundled prelude already declares unary capability classes and empty
concrete impl facts; this child batch narrows that follow-up to one method on
one concrete prelude impl.

## Batch 1: Bundled Prelude Eq(Int) Method Body

Scope:

- Add a signature-only `equals :: a -> a -> Bool.` method declaration to the
  bundled `Eq(a)` class.
- Add exactly one bundled-prelude concrete method body for `impl Eq(Int)`.
- Implement the method body in ordinary source syntax, for example by
  delegating to the already-implemented strict integer equality operator.
- Keep all other bundled `Eq` impls without method bodies in this batch.
- Verify default bundled-prelude loading exposes `Eq::equals 1 1` without
  requiring a source-local class or impl declaration.
- Verify `--no-prelude` / no-prelude compile paths still reject `Eq::equals`
  when no source-local method body exists.
- Verify an explicit minimal prelude does not inherit bundled method bodies.
- Keep prelude kernel bridge validation focused on `__kernel_` bridge aliases;
  class method bodies are ordinary prelude source declarations, not kernel
  bridges.

Out of scope:

- Adding method bodies for `Eq(Bool)`, floating types, width-specific numeric
  types, or other classes,
- typed overload dispatch across multiple bundled method bodies,
- builtin catalog or kernel-backed method bodies,
- unqualified overloaded method names,
- dictionaries or runtime evidence values,
- default methods,
- superclass semantics,
- module export/import behavior beyond the existing default prelude loader.

Batch target paths:

- `jazz-next/src/JazzNext/Compiler/BundledPrelude.hs`
- `jazz-next/src/JazzNext/Compiler/PreludeContract.hs`
- `jazz-next/src/JazzNext/Compiler/TypeInference.hs`
- `jazz-next/src/JazzNext/Compiler/Runtime.hs`
- `jazz-next/stdlib/Prelude.jz`
- `jazz-next/test/JazzNext/Compiler/Modules/PreludeLoadingSpec.hs`
- `jazz-next/test/JazzNext/Compiler/Semantics/BindingSignatureCoherenceSpec.hs`
- `jazz-next/test/JazzNext/Compiler/Semantics/RuntimeSemanticsSpec.hs`

Suggested task order:

- [x] Add focused prelude-loading coverage for default-prelude
  `Eq::equals 1 1` execution.
- [x] Add no-prelude and explicit-minimal-prelude coverage proving bundled
  method bodies are not accidentally available outside default prelude loading.
- [x] Update bundled prelude source generation and checked-in `Prelude.jz` with
  the `Eq(a).equals` method signature and one `Eq(Int)` method body.
- [x] Adjust prelude contract validation only if method bodies are incorrectly
  treated as kernel bridge declarations.
- [x] Add or update source-pipeline/runtime coverage proving the bundled method
  body is type-checked and executed through the existing explicit qualified
  dispatch path.
- [x] Run the focused verification commands listed in frontmatter.

Focused verification:

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Modules/PreludeLoadingSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/BindingSignatureCoherenceSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/RuntimeSemanticsSpec.hs
bash scripts/check-execution-queue.sh
bash scripts/check-docs.sh
```

Completion evidence (`2026-06-02`): added bundled `Eq(a).equals` signature
metadata and exactly one ordinary-source `impl Eq(Int).equals` method body in
the generated bundled prelude and checked-in `Prelude.jz` mirror. Focused
verification passed with `PreludeLoadingSpec`,
`BindingSignatureCoherenceSpec`, `RuntimeSemanticsSpec`, queue validation, and
docs validation; `BuiltinCatalogSpec` was also run to confirm bundled-prelude
mirror parity.
