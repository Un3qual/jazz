---
id: JN-BOOTSTRAP-TEXT-TRAVERSAL-001
status: done
completed_on: 2026-07-11
priority: P1
size: M
kind: impl
autonomous_ready: yes
depends_on:
  - JN-BOOTSTRAP-MAYBE-RESULT-LIBRARIES-001
last_verified: 2026-07-11
plan_section: "Implementation Batch: Bootstrap Text Traversal"
target_paths:
  - docs/spec/runtime/text-character-semantics.md
  - docs/feature-status.md
  - docs/jazz-language-state.md
  - docs/execution/blocker-contracts.md
  - docs/execution/queue.md
  - docs/execution/done-archive.md
  - docs/superpowers/specs/2026-07-11-jazz-next-bootstrap-text-traversal-design.md
  - jazz-next/src/JazzNext/Compiler/BuiltinCatalog.hs
  - jazz-next/src/JazzNext/Compiler/BundledPrelude.hs
  - jazz-next/src/JazzNext/Compiler/TypeInference.hs
  - jazz-next/src/JazzNext/Compiler/Runtime.hs
  - jazz-next/stdlib/Prelude.jz
  - jazz-next/stdlib/Maybe.jz
  - jazz-next/stdlib/Result.jz
  - jazz-next/stdlib/Text.jz
  - jazz-next/scripts/check-stdlib-format.sh
  - jazz-next/scripts/test-warning-config.sh
  - jazz-next/test/JazzNext/Compiler/Semantics/BuiltinCatalogSpec.hs
  - jazz-next/test/JazzNext/Compiler/Semantics/PrimitiveSemanticsSpec.hs
  - jazz-next/test/JazzNext/Compiler/Semantics/Runtime/RenderingTests.hs
  - jazz-next/test/JazzNext/Compiler/Semantics/RuntimeSemanticsSpec.hs
  - jazz-next/test/JazzNext/Compiler/Modules/LoaderSpec.hs
  - jazz-next/test/JazzNext/Compiler/Modules/PreludeLoadingSpec.hs
verification:
  - cabal test builtin-catalog-spec primitive-semantics-spec runtime-semantics-spec loader-spec prelude-loading-spec --test-show-details=failures
  - bash jazz-next/scripts/check-stdlib-format.sh
  - bash jazz-next/scripts/test-warning-config.sh
  - bash scripts/check-execution-queue.sh
  - bash scripts/check-docs.sh
  - git diff --check
deliverable: "Add an ordinary Jazz-authored Text module with scalar-counting length, emptiness, and total Maybe-based uncons through private backend-neutral kernel adapters; preserve explicit-import ownership and enforce two-space stdlib module indentation."
---

# Jazz-Next Bootstrap Text Traversal Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add the smallest ordinary Jazz `Text` module needed to traverse immutable Unicode text safely while keeping kernel adapters private and future LLVM lowering independent.

**Architecture:** Add two arity-one `KernelIntrinsic` catalog symbols: one returns scalar length and one returns a private zero-or-one list of `(Char, Text)`. `Text.jz` imports `Maybe`, wraps the raw decomposition as `Maybe((Char, Text))`, and explicitly exports four values. The bundled prelude emits kernel self-bridges for both symbols but filters public aliases by `PreludeTarget` ownership.

**Tech Stack:** Jazz `.jz`, Haskell 2010, `Data.Text`, the existing builtin catalog/type inference/interpreter/module loader, Cabal component suites, Bash formatting and repository gates.

## Global Constraints

- Modify only `jazz-next/` and active documentation; `jazz-hs/` and `jazz2/` remain read-only.
- Keep `Text` as an ordinary explicit-import module, not a bundled-prelude API.
- Export exactly `textEmpty`, `textLength`, `textIsEmpty`, and `textUncons` as values.
- Lock `textUncons :: Text -> Maybe((Char, Text))`; do not expose the raw adapter.
- Count Unicode scalar values, never UTF-8 bytes or storage code units.
- Give the two raw symbols `KernelIntrinsic` ownership and no compatibility or prelude aliases.
- Preserve all existing `PreludeTarget` aliases and bundled-prelude reproducibility.
- Use exactly two spaces per indentation level in `Maybe.jz`, `Result.jz`, and `Text.jz`.
- Prefer behavioral compiler/runtime/module tests; source inspection is reserved for formatting and generated-prelude reproducibility.
- Do not add indexing, slicing, concatenation, builders, ordering, search, classification, bytes, I/O, stack-safe evaluation, lexer code, lowered IR, LLVM lowering, linking, or a native runtime.
- Implement every behavior test-first and commit each independently reviewable task.

---

## Implementation Batch: Bootstrap Text Traversal

### Task 1: Add private catalog ownership and bundled-prelude bridges

**Files:**

- Modify: `jazz-next/test/JazzNext/Compiler/Semantics/BuiltinCatalogSpec.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/BuiltinCatalog.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/BundledPrelude.hs`
- Modify: `jazz-next/stdlib/Prelude.jz`

**Interfaces:**

- Consumes: existing `BuiltinOwnership`, kernel-name resolution, and generated bundled-prelude ordering.
- Produces: `BuiltinTextLength`, `BuiltinTextUnconsRaw`, their kernel names, arity and ownership, kernel-only lookup behavior, and reproducible self-bridges.

- [ ] **Step 1: Add failing catalog and prelude-ownership tests.**

Extend `expectedBuiltins` with:

```haskell
(BuiltinTextLength, "textLength", 1, KernelIntrinsic),
(BuiltinTextUnconsRaw, "textUnconsRaw", 1, KernelIntrinsic)
```

Update the round-trip test so `PreludeTarget` names resolve through
`lookupBuiltinSymbol`, while `KernelIntrinsic` names return `Nothing` there but
resolve through `lookupKernelBuiltinSymbol`. Add assertions that
`bundledPreludeSource` contains:

```text
__kernel_textLength = __kernel_textLength.
__kernel_textUnconsRaw = __kernel_textUnconsRaw.
```

and does not contain either `textLength = __kernel_textLength.` or
`textUnconsRaw = __kernel_textUnconsRaw.`.

- [ ] **Step 2: Run the focused suite and verify RED.**

Run:

```bash
cd jazz-next && cabal test builtin-catalog-spec --test-show-details=failures
```

Expected: compilation fails because the two `BuiltinSymbol` constructors do not
exist.

- [ ] **Step 3: Implement catalog symbols and ownership-aware public lookup.**

Add the constructors after the numeric builtins, classify both as `KernelIntrinsic`,
render the names `textLength` and `textUnconsRaw`, and assign arity `1`.
Restrict `ResolveCompatibility`, `lookupBuiltinSymbol`, and
`builtinNamesInMode ResolveCompatibility` to `PreludeTarget` symbols while
keeping kernel lookup over every symbol.

In `BundledPrelude.hs`, continue rendering kernel bridges from
`allBuiltinSymbols`, but render public aliases only for symbols satisfying:

```haskell
builtinSymbolOwnership symbol == PreludeTarget
```

Add the two generated self-bridge lines to `stdlib/Prelude.jz` immediately after
the existing numeric kernel bridges and before public aliases.

- [ ] **Step 4: Run the focused suite and verify GREEN.**

Run the command from Step 2. Expected: all BuiltinCatalog tests pass, including
checked-in prelude reproducibility.

- [ ] **Step 5: Commit the catalog boundary.**

```bash
git add jazz-next/src/JazzNext/Compiler/BuiltinCatalog.hs jazz-next/src/JazzNext/Compiler/BundledPrelude.hs jazz-next/stdlib/Prelude.jz jazz-next/test/JazzNext/Compiler/Semantics/BuiltinCatalogSpec.hs
git commit -m "feat: add private text traversal intrinsics"
```

### Task 2: Type and execute the raw text primitives

**Files:**

- Modify: `jazz-next/test/JazzNext/Compiler/Semantics/PrimitiveSemanticsSpec.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Semantics/Runtime/RenderingTests.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/TypeInference.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Runtime.hs`

**Interfaces:**

- Consumes: `BuiltinTextLength` and `BuiltinTextUnconsRaw` from Task 1.
- Produces: `Text -> Int` and `Text -> [(Char, Text)]` inference plus stage-0 scalar length and zero-or-one decomposition semantics.

- [ ] **Step 1: Add failing inference tests.**

Add source-pipeline cases that accept:

```jazz
length :: Int.
length = __kernel_textLength "a\u{1F642}".
parts :: [(Char, Text)].
parts = __kernel_textUnconsRaw "a\u{1F642}".
```

and reject `__kernel_textLength 1` and `__kernel_textUnconsRaw True` with
the canonical application-mismatch diagnostic `E2006`.

- [ ] **Step 2: Add failing runtime tests.**

Add a source runtime case expecting:

```text
(0, 3, [], [('🙂', "x")])
```

from empty length, scalar length of `"a🙂é"`, empty decomposition, and
decomposition of `"🙂x"`. Add low-level `evaluateRuntimeExpr` cases that apply
each kernel primitive to `1` and expect deterministic `E3028` and `E3029`
diagnostics respectively.

- [ ] **Step 3: Run focused suites and verify RED.**

```bash
cd jazz-next && cabal test primitive-semantics-spec runtime-semantics-spec --test-show-details=failures
```

Expected: the new kernel symbols are catalogued but have no type-instantiation
or runtime evaluation rules.

- [ ] **Step 4: Implement exact inference types.**

Extend `instantiateBuiltinSymbolTypeByName` with:

```haskell
"textLength" ->
  Just (TFunctionType TTextType TIntType, state)
"textUnconsRaw" ->
  Just
    ( TFunctionType
        TTextType
        (TListType (TTupleType [TCharType, TTextType])),
      state
    )
```

- [ ] **Step 5: Implement stage-0 runtime behavior.**

Use `Text.length` and `Text.uncons`. Length returns
`VInt (fromIntegral scalarCount) untypedIntMetadata`. Raw uncons returns:

```haskell
VList [] (Just (TypeList (TypeTuple [TypeChar, TypeText])))
```

for empty input, or a singleton `VTuple [VChar first, VText rest]` with the
same list type hint for non-empty input. Non-`Text` values report `E3028` for
length and `E3029` for raw uncons and include the actual runtime type.

- [ ] **Step 6: Run focused suites and verify GREEN.**

Run the command from Step 3. Expected: both suites pass.

- [ ] **Step 7: Commit primitive type/runtime behavior.**

```bash
git add jazz-next/src/JazzNext/Compiler/TypeInference.hs jazz-next/src/JazzNext/Compiler/Runtime.hs jazz-next/test/JazzNext/Compiler/Semantics/PrimitiveSemanticsSpec.hs jazz-next/test/JazzNext/Compiler/Semantics/Runtime/RenderingTests.hs
git commit -m "feat: execute Unicode text traversal primitives"
```

### Task 3: Add the Jazz-authored Text module and enforce indentation

**Files:**

- Create: `jazz-next/stdlib/Text.jz`
- Create: `jazz-next/scripts/check-stdlib-format.sh`
- Modify: `jazz-next/stdlib/Maybe.jz`
- Modify: `jazz-next/stdlib/Result.jz`
- Modify: `jazz-next/scripts/test-warning-config.sh`
- Modify: `jazz-next/test/JazzNext/Compiler/Modules/LoaderSpec.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Modules/PreludeLoadingSpec.hs`

**Interfaces:**

- Consumes: kernel primitives from Tasks 1-2 and the ordinary `Maybe` module.
- Produces: the exact four-value `Text` module API plus a repository guard for two-space stdlib module indentation.

- [ ] **Step 1: Add the failing real-loader behavior test.**

Teach the loader lookup to serve `src/Text.jz` from checked-in
`stdlib/Text.jz`. The entry module imports `Text` and `Maybe`, then verifies
`textEmpty`, `textLength "a🙂é" == 3`, `textUncons "" == Nothing`, non-empty
decomposition, and a second `textUncons` over the remainder. Expected output:

```text
(True, 3, True, '🙂', 'x', True)
```

- [ ] **Step 2: Add the failing prelude-isolation cases.**

Extend the unavailable-name table with `textEmpty`, `textLength`,
`textIsEmpty`, and `textUncons`, each expecting `E1001` under bundled-prelude
standalone compilation.

- [ ] **Step 3: Add and run the formatting guard to verify RED.**

Create `check-stdlib-format.sh` to inspect every module-form `.jz` file under
`jazz-next/stdlib/` except `Prelude.jz`. It must require an unindented module
header and final `}`, and require every nonblank module-body line to have at
least two leading spaces and a leading-space count divisible by two. Run:

```bash
bash jazz-next/scripts/check-stdlib-format.sh
```

Expected: failure on the unindented `Maybe.jz` and `Result.jz` bodies.

- [ ] **Step 4: Run loader/prelude tests and verify RED.**

```bash
cd jazz-next && cabal test loader-spec prelude-loading-spec --test-show-details=failures
```

Expected: loader failure because `Text.jz` is absent; prelude-isolation cases
pass because the public names are not global.

- [ ] **Step 5: Add the canonical Jazz module and fix indentation.**

Create exactly:

```jazz
module Text (value textEmpty, value textLength, value textIsEmpty, value textUncons) {
  import Maybe.

  textEmpty :: Text.
  textEmpty = "".

  textLength :: Text -> Int.
  textLength = __kernel_textLength.

  textIsEmpty :: Text -> Bool.
  textIsEmpty = \(text) -> text == "".

  textUncons :: Text -> Maybe((Char, Text)).
  textUncons = \(text) -> case __kernel_textUnconsRaw text {
    | [] -> Nothing
    | [(first, rest)] -> Just (first, rest)
  }.
}
```

Indent the `data` declaration inside both `Maybe.jz` and `Result.jz` by two
spaces. Invoke the format guard near the start of `test-warning-config.sh`.

- [ ] **Step 6: Run format and focused tests and verify GREEN.**

Run both commands from Steps 3-4. Expected: the format guard and both suites
pass.

- [ ] **Step 7: Commit the public Jazz module.**

```bash
git add jazz-next/stdlib/Maybe.jz jazz-next/stdlib/Result.jz jazz-next/stdlib/Text.jz jazz-next/scripts/check-stdlib-format.sh jazz-next/scripts/test-warning-config.sh jazz-next/test/JazzNext/Compiler/Modules/LoaderSpec.hs jazz-next/test/JazzNext/Compiler/Modules/PreludeLoadingSpec.hs
git commit -m "feat: add Jazz Text traversal module"
```

### Task 4: Close documentation and live dispatch state

**Files:**

- Modify: `docs/spec/runtime/text-character-semantics.md`
- Modify: `docs/feature-status.md`
- Modify: `docs/jazz-language-state.md`
- Modify: `docs/execution/blocker-contracts.md`
- Modify: `docs/execution/queue.md`
- Modify: `docs/execution/done-archive.md`
- Modify: `docs/superpowers/plans/2026-07-11-jazz-next-bootstrap-text-traversal.md`

**Interfaces:**

- Consumes: verified compiler/runtime/module behavior from Tasks 1-3.
- Produces: completed child evidence and an unpromoted host-text-I/O curation handoff.

- [ ] **Step 1: Update normative and status documentation.**

Record the four public signatures, scalar-counting semantics, total
decomposition, private raw adapter, explicit-import requirement, and future
native-runtime equivalence. Remove text length/traversal from staged follow-ups
while leaving indexing, slicing, concatenation, builders, ordering, search,
classification, bytes, I/O, and backend work pending.

- [ ] **Step 2: Close the live queue child.**

Move `JN-BOOTSTRAP-TEXT-TRAVERSAL-001` to `done-archive.md`, mark this plan
`status: done` with `completed_on: 2026-07-11`, and update the bootstrap blocker.
Curate `JN-BOOTSTRAP-HOST-TEXT-IO-001` as the next unpromoted candidate from the
approved profile, requiring a later promotion pass to lock its Jazz module,
`IOError` representation, kernel bridges, purity, runtime, and verification
contract.

- [ ] **Step 3: Run documentation/queue gates.**

```bash
bash scripts/check-execution-queue.sh
bash scripts/check-docs.sh
git diff --check
```

Expected: all commands exit `0`.

- [ ] **Step 4: Commit closeout metadata.**

```bash
git add docs/spec/runtime/text-character-semantics.md docs/feature-status.md docs/jazz-language-state.md docs/execution/blocker-contracts.md docs/execution/queue.md docs/execution/done-archive.md docs/superpowers/plans/2026-07-11-jazz-next-bootstrap-text-traversal.md
git commit -m "docs: close bootstrap text traversal"
```

### Task 5: Verify the complete substantial branch update

**Files:**

- Verify all files changed by Tasks 1-4.

- [ ] **Step 1: Run focused Cabal components.**

```bash
cd jazz-next && cabal test builtin-catalog-spec primitive-semantics-spec runtime-semantics-spec loader-spec prelude-loading-spec --test-show-details=failures
```

- [ ] **Step 2: Run formatting and full warning matrix.**

```bash
bash jazz-next/scripts/check-stdlib-format.sh
bash jazz-next/scripts/test-warning-config.sh
```

- [ ] **Step 3: Run repository gates.**

```bash
bash scripts/check-execution-queue.sh
bash scripts/check-docs.sh
git diff --check
```

- [ ] **Step 4: Review branch scope.**

Confirm `git diff origin/main...HEAD --name-only` contains no `jazz-hs/` or
`jazz2/` paths, no bytecode/LLVM implementation, and only the approved
Maybe/Result plus text-traversal stack and active documentation.
