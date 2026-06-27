---
id: JN-ABSTRACTION-BUNDLED-PRELUDE-EQ-FLOAT-METHOD-001
status: ready
priority: P1
size: S
kind: impl
autonomous_ready: yes
depends_on: []
last_verified: 2026-06-26
plan_section: "Task 1: Bundled prelude Eq(Float) method body"
target_paths:
  - jazz-next/src/JazzNext/Compiler/BundledPrelude.hs
  - jazz-next/stdlib/Prelude.jz
  - jazz-next/test/JazzNext/Compiler/Modules/PreludeLoadingSpec.hs
  - jazz-next/test/JazzNext/Compiler/Semantics/BindingSignatureCoherenceSpec.hs
  - jazz-next/test/JazzNext/Compiler/Semantics/RuntimeSemanticsSpec.hs
verification:
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Modules/PreludeLoadingSpec.hs
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/BindingSignatureCoherenceSpec.hs
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/RuntimeSemanticsSpec.hs
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/BuiltinCatalogSpec.hs
  - bash scripts/check-execution-queue.sh
  - bash scripts/check-docs.sh
deliverable: "Add exactly one bundled-prelude `Eq(Float).equals` method body through default prelude loading while preserving no-prelude and explicit-prelude isolation and avoiding dictionaries, defaults, superclasses, solver expansion, module method export/import semantics, import syntax, package roots, or public no-prelude fallback."
---

# Bundled Prelude Eq(Float) Method Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use
> superpowers:subagent-driven-development (recommended) or
> superpowers:executing-plans to implement this plan task-by-task. Steps use
> checkbox (`- [ ]`) syntax for tracking.

**Goal:** Extend the landed bundled-prelude `Eq::equals` method-body path from
`Int` and `Bool` to exactly one default `Float` alias method body.

**Architecture:** Keep method bodies as ordinary bundled-prelude source
metadata. The bundled prelude already declares `class Eq(a)` with
`equals :: a -> a -> Bool.` and already contains an inert `impl Eq(Float)` fact.
This child fills that one impl body with ordinary source that delegates to the
existing same-concrete Float equality operator.

**Tech Stack:** Haskell bundled prelude generation, checked-in `Prelude.jz`
mirror, active `jazz-next` qualified method dispatch, focused `runghc` prelude,
type, runtime, and catalog reproducibility specs.

---

## Source Evidence

- `docs/spec/stdlib-boundary.md` makes bundled prelude APIs the public stdlib
  surface and keeps explicit no-prelude paths kernel-only.
- `docs/execution/blocker-contracts.md` keeps abstraction semantics blocked
  until a concrete bundled method-family expansion, method import/export
  behavior, runtime evidence, or dictionary/default/superclass contract is
  accepted; this child selects exactly one bundled method-family expansion.
- `docs/plans/2026-06-02-jazz-next-bundled-prelude-eq-int-method.md` and
  `docs/plans/2026-06-04-jazz-next-bundled-prelude-eq-bool-method.md` establish
  the accepted pattern for one bundled-prelude `Eq::equals` method body at a
  time.
- `jazz-next/src/JazzNext/Compiler/BundledPrelude.hs` already renders
  `class Eq(a)` with the `equals` method signature and already lists
  `("Eq", "Float")` in `defaultAliasCapabilityImplFacts`.
- `jazz-next/stdlib/Prelude.jz` already mirrors an inert `impl Eq(Float) { }.`
  fact.
- `PreludeLoadingSpec.hs` already proves default bundled prelude visibility for
  `Eq(Float)` constrained signatures and no-prelude/explicit-prelude isolation
  for bundled method bodies.
- `RuntimeSemanticsSpec.hs` already proves same-concrete `Float64` equality and
  qualified method dispatch treating `Float` as the `Float64` alias.

## Task 1: Bundled prelude Eq(Float) method body

**Files:**

- Modify: `jazz-next/test/JazzNext/Compiler/Modules/PreludeLoadingSpec.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Semantics/BindingSignatureCoherenceSpec.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Semantics/RuntimeSemanticsSpec.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/BundledPrelude.hs`
- Modify: `jazz-next/stdlib/Prelude.jz`

Scope:

- Add exactly one bundled-prelude concrete method body for `impl Eq(Float)`.
- Implement the method body in ordinary source syntax:

```jazz
impl Eq(Float) {
equals = \(left) -> \(right) -> left == right.
}.
```

- Preserve the existing `Eq(a).equals` method signature.
- Preserve existing `Eq(Int).equals` and `Eq(Bool).equals` behavior.
- Keep width-specific `Eq(Float16)`, `Eq(Float32)`, and `Eq(Float64)` method
  bodies out of this child.
- Keep method bodies out of `BuiltinCatalog.hs` and the kernel bridge contract.

Out of scope:

- Any new import syntax, module graph behavior, package-root semantics, public
  no-prelude builtin fallback, dictionaries, runtime evidence values, default
  methods, superclasses, inferred constraints, broader solver behavior,
  unqualified overloaded method names, or module method export/import behavior.

- [ ] **Step 1: Add failing default-prelude Float coverage**

In `jazz-next/test/JazzNext/Compiler/Modules/PreludeLoadingSpec.hs`, add one
test case next to the existing bundled `Eq Int` and `Eq Bool` method tests:

```haskell
    ("bundled default prelude exposes Eq Float equals method body", testBundledPreludeExposesEqFloatEqualsMethodBody),
```

Add the test body:

```haskell
testBundledPreludeExposesEqFloatEqualsMethodBody :: IO ()
testBundledPreludeExposesEqFloatEqualsMethodBody = do
  result <- runSource defaultWarningSettings "(Eq::equals 1.5 1.5, Eq::equals 1.5 2.25)."
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "(True, False)") (runOutput result)
```

- [ ] **Step 2: Add no-prelude and explicit-prelude Float isolation checks**

Extend `testCompileWithoutPreludeRejectsBundledEqEqualsMethodBodies` with a
third `Float` case:

```haskell
  floatResult <-
    compileSourceWithPrelude
      defaultWarningSettings
      Nothing
      ( Text.unlines
          [ "class Eq(a) {",
            "equals :: a -> a -> Bool.",
            "}.",
            "impl Eq(Float) { }.",
            "left :: Float.",
            "left = 1.5.",
            "right :: Float.",
            "right = 1.5.",
            "result = Eq::equals left right.",
            "result."
          ]
      )
  assertSingleErrorContains
    "no-prelude compile has no bundled Eq(Float).equals method body"
    "missing impl method body 'Eq::equals'"
    (compileErrors floatResult)
```

Extend `testExplicitPreludeDoesNotInheritBundledEqEqualsMethodBodies` with the
same shape, using an explicit prelude that declares `Eq(Float)` without a body:

```haskell
  floatResult <-
    compileSourceWithPrelude
      defaultWarningSettings
      ( Just
          ( Text.unlines
              [ "class Eq(a) {",
                "equals :: a -> a -> Bool.",
                "}.",
                "impl Eq(Float) { }."
              ]
          )
      )
      ( Text.unlines
          [ "left :: Float.",
            "left = 1.5.",
            "right :: Float.",
            "right = 1.5.",
            "result = Eq::equals left right.",
            "result."
          ]
      )
  assertSingleErrorContains
    "explicit prelude has no bundled Eq(Float).equals method body"
    "missing impl method body 'Eq::equals'"
    (compileErrors floatResult)
```

- [ ] **Step 3: Add source-pipeline type coverage for the Float method body**

In `jazz-next/test/JazzNext/Compiler/Semantics/BindingSignatureCoherenceSpec.hs`,
add a focused source-pipeline check near the existing typed qualified method
dispatch tests:

```haskell
testSourceSelectsQualifiedFloatMethodBodyByArgumentTypes :: IO ()
testSourceSelectsQualifiedFloatMethodBodyByArgumentTypes =
  assertSourceOkWithoutPrelude
    ( "class Eq(a) {\nequals :: a -> a -> Bool.\n}.\n"
        <> "impl Eq(Float) {\nequals = \\(left) -> \\(right) -> left == right.\n}.\n"
        <> "left :: Float.\nleft = 1.5.\n"
        <> "right :: Float.\nright = 2.25.\n"
        <> "result :: Bool.\nresult = Eq::equals left right.\nresult."
    )
```

Register it in the `tests` list with a descriptive name:

```haskell
    ("source pipeline selects qualified Float method body by argument types", testSourceSelectsQualifiedFloatMethodBodyByArgumentTypes),
```

- [ ] **Step 4: Add runtime coverage for the Float equality body**

In `jazz-next/test/JazzNext/Compiler/Semantics/RuntimeSemanticsSpec.hs`, add a
runtime test near `testQualifiedMethodDispatchTreatsFloatAsFloat64Alias`:

```haskell
testQualifiedMethodDispatchExecutesFloatEqualityBody :: IO ()
testQualifiedMethodDispatchExecutesFloatEqualityBody = do
  result <-
    runSource
      defaultWarningSettings
      ( "class RuntimeEq(a) {\nequals :: a -> a -> Bool.\n}.\n"
          <> "impl RuntimeEq(Float) {\nequals = \\(left) -> \\(right) -> left == right.\n}.\n"
          <> "left :: Float.\nleft = 1.5.\n"
          <> "same :: Float.\nsame = 1.5.\n"
          <> "different :: Float.\ndifferent = 2.25.\n"
          <> "(RuntimeEq::equals left same, RuntimeEq::equals left different)."
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "(True, False)") (runOutput result)
```

Register it in the `tests` list:

```haskell
    ("qualified method dispatch executes Float equality body", testQualifiedMethodDispatchExecutesFloatEqualityBody),
```

- [ ] **Step 5: Update bundled prelude generation**

In `jazz-next/src/JazzNext/Compiler/BundledPrelude.hs`, extend
`renderDefaultCapabilityImpl` with the `Eq(Float)` body between the existing
`Eq(Int)` and `Eq(Bool)` branches:

```haskell
        ("Eq", "Float") ->
          Text.intercalate
            "\n"
            [ "impl Eq(Float) {",
              "equals = \\(left) -> \\(right) -> left == right.",
              "}."
            ]
```

- [ ] **Step 6: Update the checked-in bundled prelude mirror**

In `jazz-next/stdlib/Prelude.jz`, replace:

```jazz
impl Eq(Float) { }.
```

with:

```jazz
impl Eq(Float) {
equals = \(left) -> \(right) -> left == right.
}.
```

Keep the mirror ordering unchanged so `BuiltinCatalogSpec` continues to compare
the checked-in file against `bundledPreludeSource`.

- [ ] **Step 7: Run focused verification**

Run:

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Modules/PreludeLoadingSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/BindingSignatureCoherenceSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/RuntimeSemanticsSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/BuiltinCatalogSpec.hs
bash scripts/check-execution-queue.sh
bash scripts/check-docs.sh
```

Expected:

- All focused `runghc` suites pass.
- `BuiltinCatalogSpec` proves `jazz-next/stdlib/Prelude.jz` still matches
  `bundledPreludeSource`.
- Queue and docs checks pass after the central queue row is added.

## Self-Review

- Spec coverage: the plan extends only the named bundled-prelude public API
  behavior and preserves the stdlib boundary's no-prelude isolation.
- Placeholder scan: every implementation step names exact files, code shapes,
  commands, and expected outcomes.
- Type consistency: `Eq(Float).equals` uses the existing `a -> a -> Bool`
  method signature, existing `Float` alias handling, and existing same-concrete
  Float equality behavior.
