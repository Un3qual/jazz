---
id: JN-ABSTRACTION-BUNDLED-PRELUDE-EQ-FLOAT64-METHOD-001
status: ready
priority: P1
size: S
kind: impl
autonomous_ready: yes
depends_on:
  - JN-ABSTRACTION-BUNDLED-PRELUDE-EQ-FLOAT32-METHOD-001
last_verified: 2026-06-30
plan_section: "Task 1: Bundled prelude Eq(Float64) method body"
target_paths:
  - jazz-next/src/JazzNext/Compiler/BundledPrelude.hs
  - jazz-next/stdlib/Prelude.jz
  - jazz-next/test/JazzNext/Compiler/Modules/PreludeLoadingSpec.hs
  - jazz-next/test/JazzNext/Compiler/Semantics/BindingSignatureCoherenceSpec.hs
  - jazz-next/test/JazzNext/Compiler/Semantics/RuntimeSemanticsSpec.hs
  - jazz-next/test/JazzNext/Compiler/Semantics/BuiltinCatalogSpec.hs
  - docs/spec/abstractions/capability-model.md
  - docs/execution/blocker-contracts.md
  - docs/execution/queue.md
verification:
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Modules/PreludeLoadingSpec.hs
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/BindingSignatureCoherenceSpec.hs
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/RuntimeSemanticsSpec.hs
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/BuiltinCatalogSpec.hs
  - bash scripts/check-execution-queue.sh
  - bash scripts/check-docs.sh
  - git diff --check
deliverable: "Accept the narrow Float/Float64 alias-overlap policy for bundled prelude method bodies, then add exactly one default bundled-prelude `Eq(Float64).equals` body using same-width Float64 equality while preserving duplicate visible impl rejection outside the alias pair and keeping dictionaries, default methods, superclasses, inferred constraints, runtime evidence, and method import/export behavior out of scope."
---

# Jazz-Next Bundled Prelude Eq(Float64) Method

> **For agentic workers:** REQUIRED SUB-SKILL: Use
> superpowers:subagent-driven-development (recommended) or
> superpowers:executing-plans to implement this plan task-by-task. Steps use
> checkbox (`- [ ]`) syntax for tracking.

**Goal:** finish the bundled-prelude `Eq::equals` floating family by giving the
existing `Eq(Float64)` fact a method body under an explicit `Float`/`Float64`
alias-overlap policy.

**Architecture:** keep `Float` as the public default alias for `Float64` and
keep both `Eq(Float)` and `Eq(Float64)` facts visible in the bundled prelude.
The accepted policy is that this alias pair may coexist only because both names
normalize to the same default concrete float family; explicit user duplicate
facts and non-alias duplicate concrete impls must continue to reject. Method
execution stays on the existing explicit `Eq::equals` qualified dispatch path.

**Tech Stack:** Haskell `jazz-next` bundled prelude generation, checked-in
`Prelude.jz` mirror, class/impl fact validation and qualified method dispatch,
focused `runghc` prelude/type/runtime/catalog specs, and repo-root queue/docs
validation.

---

## Source Evidence

- The previous bundled method children landed `Eq(Int).equals`,
  `Eq(Bool).equals`, `Eq(Float).equals`, `Eq(Float16).equals`, and
  `Eq(Float32).equals`.
- `jazz-next/src/JazzNext/Compiler/BundledPrelude.hs` already emits inert
  `Eq(Float64)` capability facts through the width-specific floating fact
  matrix, but currently renders that fact without a method body.
- `RuntimeSemanticsSpec` already proves an `impl RuntimeEq(Float)` method can
  serve `Float64`-annotated values, so the alias normalization behavior exists
  for method selection.
- The maintainer accepted `Float`/`Float64` alias overlap for this exact
  bundled-prelude method child on 2026-06-30.

## Task 1: Bundled prelude Eq(Float64) method body

Scope:

- Add exactly one `Eq(Float64)` bundled-prelude method body:

  ```jazz
  impl Eq(Float64) {
  equals = \(left) -> \(right) -> left == right.
  }.
  ```

- Mirror the generated bundled prelude body in `jazz-next/stdlib/Prelude.jz`.
- Update the abstraction/capability docs to state the narrow alias-overlap
  rule: default bundled `Eq(Float)` and `Eq(Float64)` method facts may coexist
  only because `Float` is the public alias for `Float64`.
- Add default-prelude source/runtime coverage proving `Eq::equals` works for
  explicit `Float64` values through the bundled prelude.
- Add no-prelude and explicit-prelude isolation coverage proving a local inert
  `Eq(Float64)` fact still does not gain a method body without the bundled
  prelude source.
- Preserve duplicate visible impl rejection for two explicit `impl Eq(Float64)`
  facts, two explicit `impl Eq(Float)` facts, and non-alias duplicate facts.

Out of scope:

- unqualified overloaded method names,
- dictionaries or runtime evidence values,
- default methods or superclasses,
- inferred constraints or broad solver/defaulting work,
- module export/import behavior for methods,
- broader bundled-prelude method families,
- any `jazz-hs/` or `jazz2/` work.

## Focused Test Changes

Add coverage beside the existing Float, Float16, and Float32 method tests.

In `PreludeLoadingSpec.hs`, add default-prelude success:

```jazz
left :: Float64.
left = toFloat64 1.
right :: Float64.
right = toFloat64 1.
result = Eq::equals left right.
result.
```

Expected runtime output through `runSource`: `True`.

In `PreludeLoadingSpec.hs`, add no-prelude isolation:

```jazz
class Eq(a) {
equals :: a -> a -> Bool.
}.
impl Eq(Float64) { }.
left :: Float64.
left = toFloat64 1.
right :: Float64.
right = toFloat64 1.
result = Eq::equals left right.
result.
```

Expected compile diagnostic: `missing impl method body 'Eq::equals'`.

In `BindingSignatureCoherenceSpec.hs`, add local source-pipeline coverage:

```jazz
class Eq(a) {
equals :: a -> a -> Bool.
}.
impl Eq(Float64) {
equals = \(left) -> \(right) -> left == right.
}.
left :: Float64.
left = toFloat64 1.
right :: Float64.
right = toFloat64 1.
result :: Bool.
result = Eq::equals left right.
result.
```

Expected: compile succeeds.

In `RuntimeSemanticsSpec.hs`, add runtime output coverage equivalent to the
source-pipeline test above and assert `Just "True"`.

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
