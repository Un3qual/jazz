# Jazz-Next Canonical Module Declarations Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Align `jazz-next` module declarations with the canonical `module A::B { ... }` syntax without changing existing import or resolver semantics.

**Architecture:** Keep module declarations as top-level statements in the active `jazz-next` surface AST, but change the parser contract from dot-terminated declarations to brace-delimited module blocks. Preserve the current resolver model that reads the declaration for validation while continuing to treat the file's top-level bindings/imports as the exported module body.

**Tech Stack:** Haskell (`jazz-next` lexer/parser/lowering/resolver/tests), `runghc` test entrypoints, Markdown tracker docs

---

## Progress Tracker

- [x] Verified recent March 15-17 implementation plans were already merged and are tracker drift, not missing behavior.
- [x] Verified the active `jazz-next` module parser/tests still enforce non-canonical `module A::B.` syntax.
- [x] Task 1 complete: failing parser/loader/CLI tests lock canonical brace-delimited module declarations.
- [x] Task 2 complete: lexer/parser accept canonical module declarations and keep lowering/resolver behavior intact.
- [ ] Task 3 complete: module docs/trackers reflect the new surface and targeted/full verification pass.

## Scope Guardrails

In scope:

- canonical `module A::B { ... }` declarations in `jazz-next`
- parser/lowering updates needed for that surface form
- module-loader/resolver tests and fixtures affected by the syntax change
- docs/trackers that claim or exemplify the active module declaration syntax

Out of scope:

- lambda syntax
- `class` / `impl`
- changing import surface or module resolution semantics
- adding nested module bodies as a new semantic construct

## Task 1: Lock Canonical Module Declaration Expectations

**Files:**
- Modify: `jazz-next/test/JazzNext/Compiler/Parser/ModuleImportParserSpec.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Modules/ModuleResolutionSpec.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Modules/LoaderSpec.hs`

**Step 1: Write the failing tests**

Change existing module declaration fixtures and parser expectations from:

```jazz
module App::Core.
```

to:

```jazz
module App::Core {
  x = 1.
}
```

Also add at least one invalid parser case that proves the old dot-only declaration is no longer accepted.

**Step 2: Run the targeted suites to verify RED**

Run:

```bash
runghc -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Parser/ModuleImportParserSpec.hs
runghc -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Modules/ModuleResolutionSpec.hs
runghc -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Modules/LoaderSpec.hs
```

Expected:

- parser tests fail because `module ... { ... }` is not yet accepted
- module loader/resolution tests fail anywhere they still parse old module fixtures

**Step 3: Commit checkpoint**

```bash
git add docs/plans/2026-03-17-jazz-next-canonical-module-declarations.md \
  jazz-next/test/JazzNext/Compiler/Parser/ModuleImportParserSpec.hs \
  jazz-next/test/JazzNext/Compiler/Modules/ModuleResolutionSpec.hs \
  jazz-next/test/JazzNext/Compiler/Modules/LoaderSpec.hs
git commit -m "test(jazz-next): lock canonical module declaration syntax"
```

## Task 2: Implement Canonical Module Declaration Parsing

**Files:**
- Modify: `jazz-next/src/JazzNext/Compiler/Parser/Lexer.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Parser.hs`
- Inspect: `jazz-next/src/JazzNext/Compiler/Parser/Lower.hs`
- Inspect: `jazz-next/src/JazzNext/Compiler/ModuleResolver.hs`

**Step 1: Accept module braces in the lexer/parser**

Update `parseModuleStatement` so it accepts:

```jazz
module App::Core {
  x = 1.
}
```

Parse the braced body using the existing dot-terminated statement grammar and keep the downstream contract simple by replaying the body statements into the surrounding surface block alongside the `SSModule` declaration. Do not invent nested module runtime semantics in this batch.

**Step 2: Keep the AST and resolver contract stable**

Continue lowering module declarations to the existing `SSModule` / `SModule` statement forms. Prefer flattening the parsed module body into the surrounding statement list so resolver/driver code can stay on the current top-level statement contract.

**Step 3: Reject the old surface deterministically**

Ensure `module App::Core.` now fails with a clear parser diagnostic rather than being accepted as legacy syntax.

**Step 4: Run the targeted suites to verify GREEN**

Run:

```bash
runghc -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Parser/ModuleImportParserSpec.hs
runghc -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Modules/ModuleResolutionSpec.hs
runghc -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Modules/LoaderSpec.hs
```

Expected: all pass.

**Step 5: Commit checkpoint**

```bash
git add docs/plans/2026-03-17-jazz-next-canonical-module-declarations.md \
  jazz-next/src/JazzNext/Compiler/Parser/Lexer.hs \
  jazz-next/src/JazzNext/Compiler/Parser.hs \
  jazz-next/test/JazzNext/Compiler/Parser/ModuleImportParserSpec.hs \
  jazz-next/test/JazzNext/Compiler/Modules/ModuleResolutionSpec.hs \
  jazz-next/test/JazzNext/Compiler/Modules/LoaderSpec.hs
git commit -m "feat(jazz-next): parse canonical module declarations"
```

## Task 3: Align Docs And Verification

**Files:**
- Modify: `docs/spec/authoritative-syntax.md`
- Modify: `docs/feature-status.md`
- Modify: `docs/jazz-language-state.md`

**Step 1: Update tracker/docs**

- mark the new implementation-plan progress items as completed
- update active-language docs to say `jazz-next` now uses `module A::B { ... }`
- refresh `docs/feature-status.md` verification metadata to the commit you actually verified

**Step 2: Run focused and full verification**

Run:

```bash
runghc -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Parser/ModuleImportParserSpec.hs
runghc -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Modules/ModuleResolutionSpec.hs
runghc -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Modules/LoaderSpec.hs
bash jazz-next/scripts/test-warning-config.sh
```

Expected: all pass.

**Step 3: Commit checkpoint**

```bash
git add docs/plans/2026-03-17-jazz-next-canonical-module-declarations.md \
  docs/spec/authoritative-syntax.md \
  docs/feature-status.md \
  docs/jazz-language-state.md
git commit -m "docs(jazz-next): track canonical module declaration syntax"
```
