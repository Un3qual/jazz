---
id: JN-ABSTRACTION-BUNDLED-PRELUDE-EQ-FLOAT32-METHOD-001
status: done
priority: P1
size: S
kind: impl
autonomous_ready: yes
depends_on:
  - JN-ABSTRACTION-BUNDLED-PRELUDE-EQ-FLOAT16-METHOD-001
last_verified: 2026-06-29
completed_on: 2026-06-29
plan_section: "Task 1: Bundled prelude Eq(Float32) method body"
target_paths:
  - jazz-next/src/JazzNext/Compiler/BundledPrelude.hs
  - jazz-next/stdlib/Prelude.jz
  - jazz-next/test/JazzNext/Compiler/Modules/PreludeLoadingSpec.hs
  - jazz-next/test/JazzNext/Compiler/Semantics/BindingSignatureCoherenceSpec.hs
  - jazz-next/test/JazzNext/Compiler/Semantics/RuntimeSemanticsSpec.hs
  - jazz-next/test/JazzNext/Compiler/Semantics/BuiltinCatalogSpec.hs
  - docs/execution/blocker-contracts.md
  - docs/execution/queue.md
  - docs/plans/spec-cleanup/2026-03-02/decisions/01-authoritative-syntax.md
verification:
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Modules/PreludeLoadingSpec.hs
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/BindingSignatureCoherenceSpec.hs
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/RuntimeSemanticsSpec.hs
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/BuiltinCatalogSpec.hs
  - bash scripts/check-execution-queue.sh
  - bash scripts/check-docs.sh
  - git diff --check
deliverable: "Add exactly one default bundled-prelude `Eq(Float32).equals` method body using same-width equality, prove default-prelude dispatch works and no-prelude/explicit-prelude isolation remains, and refresh stale blocker facts. Do not add dictionaries, runtime evidence, default methods, superclasses, inferred constraints, method import/export behavior, `Eq(Float64)`, or broader method families."
---

# Jazz-Next Bundled Prelude Eq(Float32) Method

**Goal:** extend the landed bundled-prelude `Eq::equals` method-body path from
`Int`, `Bool`, `Float`, and `Float16` to exactly one width-specific `Float32`
method body.

**Architecture:** keep method bodies as ordinary bundled-prelude source
metadata. The bundled prelude already declares `class Eq(a)` with
`equals :: a -> a -> Bool.` and already has visible concrete `Eq(Float32)`
facts through the active numeric-width capability surface. This child fills
that one impl body with ordinary source that delegates to existing same-width
Float32 equality. It intentionally does not choose `Eq(Float64)` because
`Float` is already the public `Float64` alias and alias overlap needs a
separate policy.

**Tech Stack:** Haskell bundled prelude generation, checked-in `Prelude.jz`
mirror, active `jazz-next` qualified method dispatch, focused `runghc` prelude,
type, runtime, and catalog reproducibility specs.

---

## Source Evidence

- `JN-ABSTRACTION-BUNDLED-PRELUDE-EQ-FLOAT16-METHOD-001` landed the previous
  width-specific method child and proves the bundled method-body pattern after
  the default `Float` alias child.
- `docs/spec/stdlib-boundary.md` keeps bundled prelude APIs as the public
  stdlib surface while explicit no-prelude paths stay kernel-only.
- `jazz-next` already supports same-concrete Float32 comparison/equality and
  width-specific numeric signatures.
- The active abstraction blocker asks for future bundled method-family
  expansion only through narrow children with concrete target paths and focused
  verification.

## Task 1: Bundled prelude Eq(Float32) method body

Executor-safe scope:

- Add exactly one bundled-prelude concrete method body for
  `impl Eq(Float32)`.
- Implement the method body in ordinary source syntax:

```jazz
impl Eq(Float32) {
equals = \(left) -> \(right) -> left == right.
}.
```

- Mirror the generated bundled prelude body in `jazz-next/stdlib/Prelude.jz`.
- Add default-prelude dispatch coverage proving `Eq(Float32).equals` executes.
- Add no-prelude and explicit-prelude isolation coverage beside the existing
  bundled method-body isolation tests.
- Add source/runtime coverage for a local `impl Eq(Float32)` body where useful.
- Refresh queue and blocker facts so future abstraction work starts from the
  landed `Eq(Int)`, `Eq(Bool)`, `Eq(Float)`, `Eq(Float16)`, and `Eq(Float32)`
  sequence.

Out of scope:

- `Eq(Float64)`,
- alias-overlap policy for `Float`/`Float64`,
- dictionaries or runtime evidence values,
- default methods, superclasses, or inferred constraints,
- method import/export behavior or unqualified overloads,
- broader bundled-prelude method families,
- any `jazz-hs/` or `jazz2/` work.

## Verification

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Modules/PreludeLoadingSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/BindingSignatureCoherenceSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/RuntimeSemanticsSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/BuiltinCatalogSpec.hs
bash scripts/check-execution-queue.sh
bash scripts/check-docs.sh
git diff --check
```
