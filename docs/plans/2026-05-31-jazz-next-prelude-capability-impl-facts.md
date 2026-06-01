---
id: JN-CAPABILITY-PRELUDE-DEFAULT-IMPLS-001
status: done
priority: P1
size: M
kind: impl
autonomous_ready: yes
depends_on:
  - JN-CAPABILITY-PRELUDE-CLASSES-001
last_verified: 2026-05-31
plan_section: "Batch 1: Bundled prelude default concrete impl facts"
target_paths:
  - jazz-next/src/JazzNext/Compiler/BundledPrelude.hs
  - jazz-next/stdlib/Prelude.jz
  - jazz-next/test/JazzNext/Compiler/Modules/PreludeLoadingSpec.hs
  - jazz-next/test/JazzNext/Compiler/Semantics/BindingSignatureCoherenceSpec.hs
verification:
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Modules/PreludeLoadingSpec.hs
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/BindingSignatureCoherenceSpec.hs
  - bash scripts/check-execution-queue.sh
  - bash scripts/check-docs.sh
deliverable: "Add the first default prelude `impl` fact matrix for `Int`, `Float`, and `Bool` capability constraints so concrete constrained signatures can validate against bundled facts without method dispatch or runtime evidence."
---

# Jazz Next Bundled Prelude Capability Impl Facts

## Batch 1: Bundled prelude default concrete impl facts

Completed on `2026-05-31`.

This executor-safe batch depended on the bundled prelude class-fact batch. It
added the first concrete capability facts that the already landed class/impl
environment validation can consume from the default prelude. The batch is fact
visibility only: impl declarations remain inert and do not provide method
lookup, dictionaries, runtime evidence values, or executable class semantics.

Implementation delivered:

- Generate the default concrete `impl` fact matrix after the canonical class
  declarations and before kernel bridge bindings in
  `JazzNext.Compiler.BundledPrelude`.
- Check the generated source into `jazz-next/stdlib/Prelude.jz`.
- Add coverage that default-prelude constrained signatures can validate against
  the bundled class and impl facts without local declarations.
- Preserve explicit-prelude and no-prelude behavior: no-prelude callers still
  have no bundled class or impl facts, and explicit prelude callers use only the
  declarations in the supplied prelude source.

Default concrete impl fact matrix for this batch:

- `impl Eq(Int) { }.`
- `impl Eq(Float) { }.`
- `impl Eq(Bool) { }.`
- `impl Ord(Int) { }.`
- `impl Ord(Float) { }.`
- `impl Num(Int) { }.`
- `impl Num(Float) { }.`
- `impl Integral(Int) { }.`
- `impl Fractional(Float) { }.`
- `impl Default(Int) { }.`
- `impl Default(Float) { }.`
- `impl Default(Bool) { }.`
- `impl Showable(Int) { }.`
- `impl Showable(Float) { }.`
- `impl Showable(Bool) { }.`

Out of scope:

- width-specific impl facts beyond `Int` and `Float` aliases,
- list, tuple, ADT, or user-defined concrete impl facts,
- method declarations or method bodies inside `class` or `impl`,
- method dispatch, dictionaries, runtime evidence, or superclass semantics,
- inferred constraints, polymorphic generalization, or numeric operator
  defaulting.

Focused verification:

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Modules/PreludeLoadingSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/BindingSignatureCoherenceSpec.hs
bash scripts/check-execution-queue.sh
bash scripts/check-docs.sh
```
