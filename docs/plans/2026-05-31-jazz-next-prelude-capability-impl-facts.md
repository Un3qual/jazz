---
id: JN-CAPABILITY-NUMERIC-WIDTH-IMPLS-001
status: done
priority: P1
size: S
kind: impl
autonomous_ready: yes
depends_on:
  - JN-CAPABILITY-PRELUDE-DEFAULT-IMPLS-001
  - JN-NUMERIC-WIDTH-SIGNATURE-TYPES-001
last_verified: 2026-05-31
plan_section: "Batch 2: Bundled prelude width-specific numeric impl facts"
target_paths:
  - jazz-next/src/JazzNext/Compiler/BundledPrelude.hs
  - jazz-next/stdlib/Prelude.jz
  - jazz-next/test/JazzNext/Compiler/Modules/PreludeLoadingSpec.hs
  - jazz-next/test/JazzNext/Compiler/Semantics/BindingSignatureCoherenceSpec.hs
  - jazz-next/test/JazzNext/Compiler/Semantics/BuiltinCatalogSpec.hs
verification:
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Modules/PreludeLoadingSpec.hs
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/BindingSignatureCoherenceSpec.hs
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/BuiltinCatalogSpec.hs
  - bash scripts/check-execution-queue.sh
  - bash scripts/check-docs.sh
deliverable: "Extend inert bundled-prelude capability impl facts from Int and Float aliases to the concrete width-specific numeric signature names, preserving explicit-prelude and no-prelude isolation."
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

## Batch 2: Bundled prelude width-specific numeric impl facts

Completed on `2026-05-31`.

This active-path batch extended the inert fact matrix after the default alias
batch. It depended on `JN-CAPABILITY-PRELUDE-DEFAULT-IMPLS-001` and
`JN-NUMERIC-WIDTH-SIGNATURE-TYPES-001`.

Implementation delivered:

- Add bundled-prelude concrete `impl` facts for signed width-specific integers
  `Int8`, `Int16`, `Int32`, and `Int64`.
- Add bundled-prelude concrete `impl` facts for unsigned width-specific
  integers `UInt8`, `UInt16`, `UInt32`, and `UInt64`.
- Add bundled-prelude concrete `impl` facts for floating width-specific types
  `Float16`, `Float32`, and `Float64`.
- Preserve the existing `Int`, `Float`, and `Bool` default facts.
- Keep explicit-prelude and no-prelude entry points isolated from bundled facts
  unless the supplied prelude source declares the facts itself.

Width-specific fact matrix:

- Signed and unsigned integer widths: `Eq`, `Ord`, `Num`, `Integral`,
  `Default`, and `Showable`.
- Floating widths: `Eq`, `Ord`, `Num`, `Fractional`, `Default`, and
  `Showable`.

Still out of scope:

- list, tuple, ADT, or user-defined concrete impl facts,
- method declarations or method bodies inside `class` or `impl`,
- method dispatch, dictionaries, runtime evidence, or superclass semantics,
- inferred constraints, polymorphic generalization, or numeric operator
  defaulting.

Batch target paths:

- `jazz-next/src/JazzNext/Compiler/BundledPrelude.hs`
- `jazz-next/stdlib/Prelude.jz`
- `jazz-next/test/JazzNext/Compiler/Modules/PreludeLoadingSpec.hs`
- `jazz-next/test/JazzNext/Compiler/Semantics/BindingSignatureCoherenceSpec.hs`
- `jazz-next/test/JazzNext/Compiler/Semantics/BuiltinCatalogSpec.hs`

Batch verification:

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Modules/PreludeLoadingSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/BindingSignatureCoherenceSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/BuiltinCatalogSpec.hs
bash scripts/check-execution-queue.sh
bash scripts/check-docs.sh
```
