---
id: JN-STDLIB-PRELUDE-DEFAULT-CONVERSION-ALIASES-001
status: done
priority: P1
size: S
kind: impl
autonomous_ready: yes
depends_on: []
last_verified: 2026-06-27
completed_on: 2026-06-27
plan_section: "Task 1: Default conversion aliases"
target_paths:
  - docs/spec/stdlib-boundary.md
  - jazz-next/src/JazzNext/Compiler/BundledPrelude.hs
  - jazz-next/stdlib/Prelude.jz
  - jazz-next/test/JazzNext/Compiler/Modules/PreludeLoadingSpec.hs
  - jazz-next/test/JazzNext/Compiler/Semantics/BuiltinCatalogSpec.hs
  - jazz-next/test/JazzNext/Compiler/Semantics/PrimitiveSemanticsSpec.hs
  - jazz-next/test/JazzNext/Compiler/Semantics/RuntimeSemanticsSpec.hs
verification:
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Modules/PreludeLoadingSpec.hs
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/BuiltinCatalogSpec.hs
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/PrimitiveSemanticsSpec.hs
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/RuntimeSemanticsSpec.hs
  - bash scripts/check-execution-queue.sh
  - bash scripts/check-docs.sh
  - git diff --check
deliverable: "Add bundled-prelude `toInt` and `toFloat` aliases to the existing target-named conversion APIs as ordinary prelude source aliases for `toInt64` and `toFloat64`, while preserving no-prelude rejection of public aliases and avoiding new import syntax, package-root semantics, public no-prelude fallback, Eq method-family expansion, or solver behavior."
---

# Jazz-Next Default Conversion Aliases Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add `toInt` and `toFloat` as bundled-prelude default-width conversion aliases for the already implemented `toInt64` and `toFloat64` conversion APIs.

**Architecture:** Keep the aliases as ordinary bundled-prelude source bindings, not new builtin symbols. `BuiltinCatalog.hs` remains the owner of target-named kernel conversion builtins; `BundledPrelude.hs` should emit `toInt = toInt64.` and `toFloat = toFloat64.` after the generated public target-named aliases. No-prelude flows continue to reject `toInt` and `toFloat` because these names exist only in real prelude source.

**Tech Stack:** Haskell bundled-prelude generation in `jazz-next/src/JazzNext/Compiler/BundledPrelude.hs`, checked-in prelude mirror `jazz-next/stdlib/Prelude.jz`, active `jazz-next` prelude loading/type/runtime suites, and stdlib boundary documentation.

---

## Queue Safety

This child unblocks the module/prelude API blocker through a concrete public prelude API delta. It does not reopen module graph resolution, import syntax, package-root semantics, CLI source selection, direct public builtin fallback in no-prelude mode, bundled `Eq(Float)` method bodies, broader Eq method-family expansion, solver-backed constrained signatures, implicit numeric conversions, mixed-width arithmetic, or runtime dictionary/evidence behavior.

The implementation must not edit `jazz-hs/` or `jazz2/`. New active compiler work belongs only in `jazz-next/`.

## Source Evidence

- `docs/spec/runtime/primitive-semantics.md` says `toInt` may alias `toInt64` and `toFloat` may alias `toFloat64` only if the prelude/catalog boundary records them as aliases rather than distinct numeric semantics.
- `docs/spec/stdlib-boundary.md` already owns the public prelude conversion list, keeps no-prelude mode kernel-only, and says conversion API changes must update bundled prelude generation/mirror plus prelude/no-prelude visibility tests together.
- `jazz-next/src/JazzNext/Compiler/BuiltinCatalog.hs` already defines target-named conversion builtins for `toInt64` and `toFloat64`, including their `__kernel_*` bridges and numeric targets.
- `jazz-next/src/JazzNext/Compiler/BundledPrelude.hs` generates target-named public aliases from the catalog in deterministic order, and `BuiltinCatalogSpec.hs` verifies the checked-in `Prelude.jz` mirror against that generated source.
- `PreludeLoadingSpec.hs`, `PrimitiveSemanticsSpec.hs`, and `RuntimeSemanticsSpec.hs` already cover target-named conversion visibility, type checking, and runtime conversion behavior; the new aliases should reuse those paths rather than adding new numeric semantics.

## target_paths

- `docs/spec/stdlib-boundary.md`
- `jazz-next/src/JazzNext/Compiler/BundledPrelude.hs`
- `jazz-next/stdlib/Prelude.jz`
- `jazz-next/test/JazzNext/Compiler/Modules/PreludeLoadingSpec.hs`
- `jazz-next/test/JazzNext/Compiler/Semantics/BuiltinCatalogSpec.hs`
- `jazz-next/test/JazzNext/Compiler/Semantics/PrimitiveSemanticsSpec.hs`
- `jazz-next/test/JazzNext/Compiler/Semantics/RuntimeSemanticsSpec.hs`

## Deliverable

Add bundled-prelude `toInt` and `toFloat` aliases to the existing target-named conversion APIs as ordinary prelude source aliases for `toInt64` and `toFloat64`, while preserving no-prelude rejection of public aliases and avoiding new builtin semantics.

## Verification

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Modules/PreludeLoadingSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/BuiltinCatalogSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/PrimitiveSemanticsSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/RuntimeSemanticsSpec.hs
bash scripts/check-execution-queue.sh
bash scripts/check-docs.sh
git diff --check
```

## Task 1: Default conversion aliases

**Files:**

- Modify: `docs/spec/stdlib-boundary.md`
- Modify: `jazz-next/src/JazzNext/Compiler/BundledPrelude.hs`
- Modify: `jazz-next/stdlib/Prelude.jz`
- Modify: `jazz-next/test/JazzNext/Compiler/Modules/PreludeLoadingSpec.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Semantics/BuiltinCatalogSpec.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Semantics/PrimitiveSemanticsSpec.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Semantics/RuntimeSemanticsSpec.hs`

Scope:

- Add `toInt` as an ordinary prelude alias for `toInt64`.
- Add `toFloat` as an ordinary prelude alias for `toFloat64`.
- Keep `BuiltinCatalog.hs` target-named conversion symbols unchanged.
- Do not add `BuiltinToInt`, `BuiltinToFloat`, `__kernel_toInt`, or `__kernel_toFloat`.
- Preserve no-prelude rejection of public aliases `toInt` and `toFloat`.

Out of scope:

- New numeric semantics, implicit conversions, mixed-width acceptance, parser literal suffixes, import syntax, module roots, package discovery, public no-prelude fallback, Eq method bodies, method-family expansion, solver-backed constrained signatures, dictionaries, default methods, superclasses, or module method export/import behavior.

- [x] **Step 1: Lock default-prelude visibility in PreludeLoadingSpec**

In `jazz-next/test/JazzNext/Compiler/Modules/PreludeLoadingSpec.hs`, add a suite entry next to the existing numeric conversion alias test:

```haskell
    ("prelude exposes default numeric conversion aliases", testPreludeExposesDefaultNumericConversionAliases),
```

Add this test near `testPreludeExposesNumericConversionAliases`:

```haskell
testPreludeExposesDefaultNumericConversionAliases :: IO ()
testPreludeExposesDefaultNumericConversionAliases = do
  result <-
    compileSource
      defaultWarningSettings
      ( Text.unlines
          [ "x :: Int.",
            "x = toInt 1.",
            "y :: Float.",
            "y = toFloat 1."
          ]
      )
  assertEqual "bundled prelude exposes default conversion aliases" [] (compileErrors result)
```

Expected before implementation: `E1001` unbound variable diagnostics for `toInt` and `toFloat`.

- [x] **Step 2: Lock no-prelude isolation in PreludeLoadingSpec**

Extend `testCompileWithoutPreludeRejectsNumericConversionAliases` so it checks both default aliases:

```haskell
testCompileWithoutPreludeRejectsNumericConversionAliases :: IO ()
testCompileWithoutPreludeRejectsNumericConversionAliases = do
  result <- compileSourceWithPrelude defaultWarningSettings Nothing "x = toUInt8 1."
  assertEqual
    "public numeric conversion aliases are unavailable without prelude"
    ["E1001: unbound variable 'toUInt8'"]
    (map renderDiagnostic (compileErrors result))
  intResult <- compileSourceWithPrelude defaultWarningSettings Nothing "x = toInt 1."
  assertEqual
    "public default integer conversion alias is unavailable without prelude"
    ["E1001: unbound variable 'toInt'"]
    (map renderDiagnostic (compileErrors intResult))
  floatResult <- compileSourceWithPrelude defaultWarningSettings Nothing "x = toFloat 1."
  assertEqual
    "public default float conversion alias is unavailable without prelude"
    ["E1001: unbound variable 'toFloat'"]
    (map renderDiagnostic (compileErrors floatResult))
```

- [x] **Step 3: Lock alias-not-builtin behavior in BuiltinCatalogSpec**

In `jazz-next/test/JazzNext/Compiler/Semantics/BuiltinCatalogSpec.hs`, add a suite entry next to the kernel bridge target test:

```haskell
    ("default conversion aliases stay ordinary prelude bindings", testDefaultConversionAliasesStayPreludeBindings),
```

Add this test body:

```haskell
testDefaultConversionAliasesStayPreludeBindings :: IO ()
testDefaultConversionAliasesStayPreludeBindings = do
  assertEqual "toInt is not a distinct builtin" Nothing (lookupBuiltinSymbol "toInt")
  assertEqual "toFloat is not a distinct builtin" Nothing (lookupBuiltinSymbol "toFloat")
  assertEqual "toInt does not create a kernel bridge" Nothing (kernelBridgeTargetName "__kernel_toInt")
  assertEqual "toFloat does not create a kernel bridge" Nothing (kernelBridgeTargetName "__kernel_toFloat")
```

Expected behavior: this should pass before and after implementation. It guards the implementation shape while the reproducibility test fails until `BundledPrelude.hs` and `Prelude.jz` are updated together.

- [x] **Step 4: Add source-pipeline conversion checks in PrimitiveSemanticsSpec**

In `jazz-next/test/JazzNext/Compiler/Semantics/PrimitiveSemanticsSpec.hs`, add a suite entry beside the existing conversion tests:

```haskell
    ("source pipeline accepts default conversion aliases", testSourcePipelineAcceptsDefaultConversionAliases),
```

Add this test near `testSourcePipelineAcceptsNumericConversions`:

```haskell
testSourcePipelineAcceptsDefaultConversionAliases :: IO ()
testSourcePipelineAcceptsDefaultConversionAliases =
  assertCompilesWithBundledPrelude
    ( Text.unlines
        [ "x :: Int.",
          "x = toInt 1.",
          "y :: Float.",
          "y = toFloat 1.",
          "z :: Int64.",
          "z = toInt 9223372036854775807.0."
        ]
    )
```

Expected before implementation: unbound-variable compile diagnostics for `toInt` and `toFloat`.

- [x] **Step 5: Add runtime conversion checks in RuntimeSemanticsSpec**

In `jazz-next/test/JazzNext/Compiler/Semantics/RuntimeSemanticsSpec.hs`, add a suite entry near the existing numeric conversion runtime tests:

```haskell
    ("runtime executes default conversion aliases", testRuntimeExecutesDefaultConversionAliases),
```

Add this test near the existing `toInt64` and `toFloat64` runtime tests:

```haskell
testRuntimeExecutesDefaultConversionAliases :: IO ()
testRuntimeExecutesDefaultConversionAliases = do
  intResult <- runSource defaultWarningSettings "toInt 9223372036854775807.0."
  assertEqual "toInt compile errors" [] (runCompileErrors intResult)
  assertEqual "toInt runtime errors" [] (runRuntimeErrors intResult)
  assertEqual "toInt output" (Just "9223372036854775807") (runOutput intResult)
  floatResult <- runSource defaultWarningSettings "toFloat 1."
  assertEqual "toFloat compile errors" [] (runCompileErrors floatResult)
  assertEqual "toFloat runtime errors" [] (runRuntimeErrors floatResult)
  assertEqual "toFloat output" (Just "1.0") (runOutput floatResult)
```

Expected before implementation: unbound-variable compile diagnostics for `toInt` and `toFloat`.

- [x] **Step 6: Emit aliases from BundledPrelude.hs**

In `jazz-next/src/JazzNext/Compiler/BundledPrelude.hs`, append default alias generation after the target-named public aliases:

```haskell
bundledPreludeSource :: Text
bundledPreludeSource =
  Text.unlines $
    map renderCapabilityClass canonicalCapabilityClassNames
      <> [""]
      <> map renderDefaultCapabilityImpl defaultCapabilityImplFacts
      <> [""]
      <> map renderKernelBridge allBuiltinSymbols
      <> [""]
      <> map renderPublicAlias allBuiltinSymbols
      <> defaultConversionAliases
  where
```

Add the alias list near the other top-level prelude generation tables:

```haskell
defaultConversionAliases :: [Text]
defaultConversionAliases =
  [ "toInt = toInt64.",
    "toFloat = toFloat64."
  ]
```

Do not add new `BuiltinSymbol` constructors for `toInt` or `toFloat`.

- [x] **Step 7: Update the checked-in bundled prelude mirror**

In `jazz-next/stdlib/Prelude.jz`, add the ordinary aliases after the generated target-named public conversion aliases:

```jazz
toFloat16 = __kernel_toFloat16.
toFloat32 = __kernel_toFloat32.
toFloat64 = __kernel_toFloat64.
toInt = toInt64.
toFloat = toFloat64.
```

- [x] **Step 8: Record the boundary decision**

In `docs/spec/stdlib-boundary.md`, update the bundled exports list to include:

```markdown
  - `toInt`, an ordinary prelude alias for `toInt64`
  - `toFloat`, an ordinary prelude alias for `toFloat64`
```

Update the explicit numeric conversions paragraph so it says:

```markdown
- Default-width aliases `toInt` and `toFloat` are ordinary prelude bindings that
  delegate to `toInt64` and `toFloat64`; they are not distinct catalog numeric
  semantics and do not create `__kernel_toInt` or `__kernel_toFloat` bridge
  names.
```

- [x] **Step 9: Run focused verification**

Run:

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Modules/PreludeLoadingSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/BuiltinCatalogSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/PrimitiveSemanticsSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/RuntimeSemanticsSpec.hs
```

Expected: all four suites pass.

- [x] **Step 10: Run queue and docs gates**

Run:

```bash
bash scripts/check-execution-queue.sh
bash scripts/check-docs.sh
git diff --check
```

Expected: queue/docs gates pass after the central queue row is integrated, and `git diff --check` reports no whitespace errors.

## Self-Review

- Spec coverage: the plan implements only the optional default conversion aliases already named by the primitive semantics contract, and records them in the stdlib boundary as ordinary prelude aliases.
- Placeholder scan: no unspecified implementation step remains.
- Type consistency: `toInt` delegates to `toInt64` and therefore targets `Int`/`Int64`; `toFloat` delegates to `toFloat64` and therefore targets `Float`/`Float64`.
