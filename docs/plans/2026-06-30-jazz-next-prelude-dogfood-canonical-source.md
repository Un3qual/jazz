---
id: JN-STDLIB-PRELUDE-DOGFOOD-CANONICAL-SOURCE-001
status: blocked
priority: P2
size: M
kind: impl
autonomous_ready: no
depends_on:
  - JN-STDLIB-PRELUDE-NEXT-API-CONTRACT-001
last_verified: 2026-06-30
plan_section: "Batch 1: Jazz-authored bundled prelude source of truth"
target_paths:
  - docs/spec/stdlib-boundary.md
  - jazz-next/src/JazzNext/Compiler/BundledPrelude.hs
  - jazz-next/src/JazzNext/Compiler/BuiltinCatalog.hs
  - jazz-next/src/JazzNext/Compiler/PreludeContract.hs
  - jazz-next/src/JazzNext/Compiler/Driver.hs
  - jazz-next/src/JazzNext/CLI/Main.hs
  - jazz-next/stdlib/Prelude.jz
  - jazz-next/test/JazzNext/Compiler/Modules/PreludeLoadingSpec.hs
  - jazz-next/test/JazzNext/Compiler/Semantics/BuiltinCatalogSpec.hs
verification:
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Modules/PreludeLoadingSpec.hs
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/BuiltinCatalogSpec.hs
  - bash scripts/check-execution-queue.sh
  - bash scripts/check-docs.sh
  - git diff --check
deliverable: "Make the checked-in Jazz prelude file the canonical bundled prelude source loaded by default, keep Haskell ownership limited to kernel builtin inventory and bridge validation, and preserve explicit-prelude and no-prelude behavior."
---

# Jazz-Next Prelude Dogfood Canonical Source Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use
> superpowers:executing-plans to implement this plan task-by-task. Steps use
> checkbox (`- [ ]`) syntax for tracking. Subagent dispatch is intentionally
> not required for this blocked plan.

**Goal:** Dogfood the prelude by making `jazz-next/stdlib/Prelude.jz` the
canonical bundled source instead of regenerating the same Jazz source from
Haskell string templates.

**Architecture:** Treat Haskell as the compiler kernel owner and Jazz source as
the stdlib/prelude owner. `BuiltinCatalog.hs` continues to define the kernel
symbol inventory and numeric conversion targets; `PreludeContract.hs` validates
that Jazz-authored `__kernel_*` bridge bindings are safe; `BundledPrelude.hs`
loads the checked-in Jazz prelude file for default compilation and execution.
Exact source generation tests are replaced with structural contract tests that
prove the Jazz file exposes the required bridges, aliases, class facts, impl
facts, and method bodies.

**Tech Stack:** Haskell `jazz-next` compiler modules, checked-in Jazz stdlib
source, existing prelude loading and builtin catalog `runghc` suites, and
repo-root execution/docs validation.

---

## Current Boundary

The active compiler already has a checked-in Jazz prelude:
`jazz-next/stdlib/Prelude.jz`. That file contains canonical `class`
declarations, concrete `impl` facts, selected `Eq(...).equals` method bodies,
kernel bridge self-bindings, public aliases, and default conversion aliases.

The remaining over-Haskelled part is `BundledPrelude.hs`: it generates the
full prelude source as Haskell text and then tests require the checked-in Jazz
mirror to match that generated text byte-for-byte. This is useful for drift
detection, but it makes Haskell the source of truth for code that Jazz can
already express.

This plan inverts that ownership without moving compiler semantics into Jazz.

## Executor-Safe Scope

- Keep all new compiler implementation work under `jazz-next/`.
- Do not edit `jazz-hs/` or `jazz2/`.
- Do not add parser syntax, solver behavior, runtime evidence, dictionaries,
  default methods, superclasses, operator semantics, numeric promotion, or
  module export/import behavior.
- Do not make public aliases available in no-prelude mode.
- Do not remove kernel bridge validation.
- Do not make explicit preludes inherit bundled prelude declarations.

## Batch 1: Jazz-authored bundled prelude source of truth

Child id: `JN-STDLIB-PRELUDE-DOGFOOD-CANONICAL-SOURCE-001`

Goal: make `Prelude.jz` the canonical bundled prelude source loaded by default
and replace byte-for-byte Haskell generation drift tests with structural
contract tests.

Target paths:

- `docs/spec/stdlib-boundary.md`
- `jazz-next/src/JazzNext/Compiler/BundledPrelude.hs`
- `jazz-next/src/JazzNext/Compiler/BuiltinCatalog.hs`
- `jazz-next/src/JazzNext/Compiler/PreludeContract.hs`
- `jazz-next/src/JazzNext/Compiler/Driver.hs`
- `jazz-next/src/JazzNext/CLI/Main.hs`
- `jazz-next/stdlib/Prelude.jz`
- `jazz-next/test/JazzNext/Compiler/Modules/PreludeLoadingSpec.hs`
- `jazz-next/test/JazzNext/Compiler/Semantics/BuiltinCatalogSpec.hs`

Accepted behavior:

```jazz
result = Eq::equals (toFloat 1) (toFloat64 1).
```

Expected: default `compileSource` and `runSource` load the checked-in
`Prelude.jz`; the expression compiles and evaluates through ordinary prelude
aliases and Jazz-authored `Eq(Float64).equals` method bodies.

No-prelude behavior:

```jazz
result = toFloat 1.
```

Expected under `compileSourceWithPrelude defaultWarningSettings Nothing`:
`E1001: unbound variable 'toFloat'`.

Explicit-prelude behavior:

```jazz
class Eq(a) { }.
```

Expected under `compileSourceWithPrelude defaultWarningSettings (Just ...)`:
explicit prelude calls do not inherit bundled `Eq(Int).equals`,
`Eq(Float64).equals`, public aliases, or default conversion aliases.

### Task 1: Lock the Jazz-file ownership contract

**Files:**

- Modify: `jazz-next/test/JazzNext/Compiler/Semantics/BuiltinCatalogSpec.hs`
- Inspect: `jazz-next/stdlib/Prelude.jz`

- [ ] **Step 1: Replace exact generated-source drift assertions**

Replace the `bundledPreludeSource` equality tests with structural assertions
over the checked-in file. Keep `readCheckedInBundledPrelude` and
`normalizePreludeLineEndings`; remove test expectations that require Haskell to
render the entire file.

The suite entries should include these names:

```haskell
    ("checked-in bundled prelude declares every kernel bridge", testCheckedInPreludeDeclaresEveryKernelBridge),
    ("checked-in bundled prelude declares every public builtin alias", testCheckedInPreludeDeclaresEveryPublicBuiltinAlias),
    ("checked-in bundled prelude keeps default conversion aliases prelude-only", testCheckedInPreludeKeepsDefaultConversionAliases),
    ("checked-in bundled prelude includes Eq Float64 equals method body", testCheckedInPreludeIncludesEqFloat64EqualsMethodBody),
```

- [ ] **Step 2: Add kernel bridge structural checks**

Add this helper and test body:

```haskell
expectedKernelBridgeLine :: BuiltinSymbol -> Text
expectedKernelBridgeLine symbol =
  let bridgeName = builtinSymbolKernelName symbol
   in bridgeName <> " = " <> bridgeName <> "."

testCheckedInPreludeDeclaresEveryKernelBridge :: IO ()
testCheckedInPreludeDeclaresEveryKernelBridge = do
  checkedInPrelude <- normalizePreludeLineEndings <$> readCheckedInBundledPrelude
  mapM_
    ( \symbol ->
        assertContains
          ("checked-in prelude bridge for " <> builtinSymbolKernelName symbol)
          (expectedKernelBridgeLine symbol)
          checkedInPrelude
    )
    allBuiltinSymbols
```

- [ ] **Step 3: Add public alias structural checks**

Add this helper and test body:

```haskell
expectedPublicAliasLine :: BuiltinSymbol -> Text
expectedPublicAliasLine symbol =
  builtinSymbolName symbol <> " = " <> builtinSymbolKernelName symbol <> "."

testCheckedInPreludeDeclaresEveryPublicBuiltinAlias :: IO ()
testCheckedInPreludeDeclaresEveryPublicBuiltinAlias = do
  checkedInPrelude <- normalizePreludeLineEndings <$> readCheckedInBundledPrelude
  mapM_
    ( \symbol ->
        assertContains
          ("checked-in prelude public alias for " <> builtinSymbolName symbol)
          (expectedPublicAliasLine symbol)
          checkedInPrelude
    )
    allBuiltinSymbols
```

- [ ] **Step 4: Add default alias structural checks**

Add this test body:

```haskell
testCheckedInPreludeKeepsDefaultConversionAliases :: IO ()
testCheckedInPreludeKeepsDefaultConversionAliases = do
  checkedInPrelude <- normalizePreludeLineEndings <$> readCheckedInBundledPrelude
  assertContains "checked-in prelude toInt alias" "toInt = toInt64." checkedInPrelude
  assertContains "checked-in prelude toFloat alias" "toFloat = toFloat64." checkedInPrelude
  assertEqual "toInt is not a catalog builtin" Nothing (lookupBuiltinSymbol "toInt")
  assertEqual "toFloat is not a catalog builtin" Nothing (lookupBuiltinSymbol "toFloat")
  assertEqual "toInt has no kernel bridge" Nothing (kernelBridgeTargetName "__kernel_toInt")
  assertEqual "toFloat has no kernel bridge" Nothing (kernelBridgeTargetName "__kernel_toFloat")
```

- [ ] **Step 5: Preserve method-body coverage on the Jazz file**

Change the existing `Eq(Float64).equals` assertion to inspect the checked-in
file:

```haskell
testCheckedInPreludeIncludesEqFloat64EqualsMethodBody :: IO ()
testCheckedInPreludeIncludesEqFloat64EqualsMethodBody = do
  checkedInPrelude <- normalizePreludeLineEndings <$> readCheckedInBundledPrelude
  assertContains
    "checked-in prelude renders Eq(Float64).equals body"
    ( Text.unlines
        [ "impl Eq(Float64) {",
          "equals = \\(left) -> \\(right) -> left == right.",
          "}."
        ]
    )
    checkedInPrelude
```

- [ ] **Step 6: Run the focused prelude contract test**

Run:

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/BuiltinCatalogSpec.hs
```

Expected before implementation: tests that still import or compare
`bundledPreludeSource` fail to compile until Task 2 removes the Haskell source
generator.

### Task 2: Make `BundledPrelude` load the Jazz file

**Files:**

- Modify: `jazz-next/src/JazzNext/Compiler/BundledPrelude.hs`
- Inspect: `jazz-next/src/JazzNext/Compiler/Driver.hs`
- Inspect: `jazz-next/src/JazzNext/CLI/Main.hs`

- [ ] **Step 1: Remove full-source generation from the public API**

Change the module export list from this shape:

```haskell
module JazzNext.Compiler.BundledPrelude
  ( bundledPreludePath,
    bundledPreludeSource,
    loadBundledPreludeSource
  ) where
```

to this shape:

```haskell
module JazzNext.Compiler.BundledPrelude
  ( bundledPreludePath,
    loadBundledPreludeSource
  ) where
```

- [ ] **Step 2: Replace the generator with file-backed loading**

Use this implementation in `BundledPrelude.hs`:

```haskell
{-# LANGUAGE OverloadedStrings #-}

-- | Loads the compiler-owned bundled prelude from the checked-in Jazz source.
module JazzNext.Compiler.BundledPrelude
  ( bundledPreludePath,
    loadBundledPreludeSource
  ) where

import Data.Text (Text)
import qualified Data.Text.IO as TextIO
import System.Directory
  ( doesFileExist,
    getCurrentDirectory
  )
import System.FilePath
  ( (</>),
    takeDirectory
  )

-- | Repository-relative location of the checked-in bundled prelude source.
bundledPreludePath :: FilePath
bundledPreludePath = "jazz-next/stdlib/Prelude.jz"

loadBundledPreludeSource :: IO Text
loadBundledPreludeSource = do
  cwd <- getCurrentDirectory
  maybePath <- findBundledPreludePathFrom cwd
  case maybePath of
    Just path -> TextIO.readFile path
    Nothing ->
      ioError $
        userError $
          "could not find checked-in bundled prelude source '"
            <> bundledPreludePath
            <> "' from current directory '"
            <> cwd
            <> "' or any parent"

findBundledPreludePathFrom :: FilePath -> IO (Maybe FilePath)
findBundledPreludePathFrom directory = do
  let candidate = directory </> bundledPreludePath
  exists <- doesFileExist candidate
  if exists
    then pure (Just candidate)
    else
      let parent = takeDirectory directory
       in
        if parent == directory
          then pure Nothing
          else findBundledPreludePathFrom parent
```

- [ ] **Step 3: Confirm existing call sites keep their API**

`Driver.hs` and `CLI/Main.hs` should continue to call
`loadBundledPreludeSource`. Do not change `compileSource`,
`runSource`, `compileModuleGraph`, `runModuleGraph`, or CLI prelude option
semantics in this batch.

- [ ] **Step 4: Run the focused prelude contract test**

Run:

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/BuiltinCatalogSpec.hs
```

Expected: the suite passes without any import of `bundledPreludeSource`.

### Task 3: Preserve default, explicit, and no-prelude behavior

**Files:**

- Modify: `jazz-next/test/JazzNext/Compiler/Modules/PreludeLoadingSpec.hs`
- Inspect: `jazz-next/src/JazzNext/Compiler/Driver.hs`

- [ ] **Step 1: Add a default-prelude source-of-truth smoke test**

Add this suite entry next to the existing bundled default prelude tests:

```haskell
    ("bundled default prelude loads checked-in Jazz source", testBundledPreludeLoadsCheckedInJazzSource),
```

Add this test body:

```haskell
testBundledPreludeLoadsCheckedInJazzSource :: IO ()
testBundledPreludeLoadsCheckedInJazzSource = do
  result <- runSource defaultWarningSettings "Eq::equals (toFloat 1) (toFloat64 1)."
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "True") (runOutput result)
```

- [ ] **Step 2: Keep no-prelude alias isolation locked**

Keep `testCompileWithoutPreludeRejectsNumericConversionAliases`,
`testCompileWithoutPreludeRejectsPreludeAliases`, and
`testCompileWithoutPreludeKeepsKernelBridgeNamesAvailable` passing unchanged.
These tests prove that file-backed bundled loading does not leak public aliases
into `PreludeAbsent` mode.

- [ ] **Step 3: Keep explicit prelude isolation locked**

Keep `testExplicitPreludeDoesNotInheritBundledImplFacts` and
`testExplicitPreludeDoesNotInheritBundledEqEqualsMethodBodies` passing
unchanged. These tests prove that explicit prelude source remains a replacement
for the bundled prelude, not an extension of it.

- [ ] **Step 4: Run the focused prelude loading test**

Run:

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Modules/PreludeLoadingSpec.hs
```

Expected: the suite passes and default prelude behavior is still hidden from
user diagnostic spans where the current tests require that behavior.

### Task 4: Document the new ownership boundary

**Files:**

- Modify: `docs/spec/stdlib-boundary.md`
- Modify: `docs/execution/blocker-contracts.md`
- Modify: `docs/execution/queue.md`

- [ ] **Step 1: Update stdlib boundary ownership wording**

Record these rules in `docs/spec/stdlib-boundary.md`:

- `jazz-next/stdlib/Prelude.jz` is the canonical default bundled prelude source.
- `BundledPrelude.hs` loads that checked-in Jazz file for default prelude mode.
- `BuiltinCatalog.hs` owns kernel builtin inventory, arity, conversion targets,
  and kernel/public symbol spelling.
- `PreludeContract.hs` validates `__kernel_*` bridge declarations in bundled
  and explicit prelude source.
- Public aliases and default conversion aliases are ordinary prelude bindings.
- No-prelude mode exposes kernel bridge names only, not public prelude aliases.

- [ ] **Step 2: Promote only if the queue is explicitly curated**

If this child is promoted, add exactly one `Ready Now` row for
`JN-STDLIB-PRELUDE-DOGFOOD-CANONICAL-SOURCE-001`; update the corresponding
blocked row and blocker contract in the same pass. Do not edit queue metadata
when only executing Tasks 1-3 as a local prototype.

- [ ] **Step 3: Run docs and queue validators**

Run:

```bash
bash scripts/check-execution-queue.sh
bash scripts/check-docs.sh
git diff --check
```

Expected: all commands pass. `check-docs.sh` may print the known
Prettier-outside-Nix warning and still pass.

## Follow-on Batches

These are separate children and should not be bundled into Batch 1.

### Batch 2: Move new stdlib helpers into Jazz modules

Goal: require future non-kernel stdlib helpers to be written as Jazz source
under `jazz-next/stdlib/` instead of new Haskell catalog entries.

Candidate examples:

```jazz
notEquals = \(left) -> \(right) -> !(Eq::equals left right).
```

This batch is blocked until boolean negation or an accepted equivalent exists
in the active language.

### Batch 3: Jazz-authored operator library surface

Goal: once custom precedence and associativity are implemented, keep
stdlib-level operator declarations in Jazz source where the behavior does not
require parser or runtime kernel changes.

Candidate examples:

```jazz
operator <| precedence 10 right.
(<|) = \(value) -> \(fn) -> fn value.
```

This batch is blocked until custom precedence, custom associativity, and the
chosen function/application semantics are accepted through their own operator
plan rows.

### Batch 4: Split the prelude into stdlib modules

Goal: after module exports and imports have a concrete stdlib contract, split
the monolithic `Prelude.jz` into smaller Jazz-authored files while preserving a
single default prelude import surface.

This batch is blocked until package/module-root semantics and stdlib export
rules have target paths and focused verification.

## Verification Bundle

Run the focused bundle for Batch 1:

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Modules/PreludeLoadingSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/BuiltinCatalogSpec.hs
bash scripts/check-execution-queue.sh
bash scripts/check-docs.sh
git diff --check
```

Expected: all commands pass, public aliases remain unavailable without a
prelude, explicit prelude behavior remains isolated from bundled declarations,
and Haskell no longer owns a full generated copy of the prelude source.
