# Compiler Comment Pass Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Improve readability of the active `jazz-next` compiler code by adding targeted explanatory comments without changing behavior.

**Architecture:** Treat this as a source-level documentation pass. Add Haddock-style module comments across the active compiler surface, then add focused function/type comments only where control flow, invariants, or phase boundaries are easy to miss when reading the code cold.

**Tech Stack:** Haskell, `runghc` test harness, Haddock-style source comments

---

## Task 1: Document foundational compiler contracts

**Files:**
- Modify: `jazz-next/src/JazzNext/Compiler/AST.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Parser/AST.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Identifier.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Purity.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/BuiltinCatalog.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/BundledPrelude.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/WarningCatalog.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Warnings.hs`

**Step 1: Add module-level context**

```haskell
-- | Canonical builtin inventory shared by analyzer, type inference, and runtime.
module JazzNext.Compiler.BuiltinCatalog
```

**Step 2: Comment exported data and helper contracts**

```haskell
-- | Names annotated with the purity implied by their spelling.
data Identifier = Identifier
```

**Step 3: Review for noise**

Run: `sed -n '1,160p' jazz-next/src/JazzNext/Compiler/BuiltinCatalog.hs`
Expected: comments explain boundaries or invariants, not obvious type signatures

## Task 2: Document phase-boundary modules

**Files:**
- Modify: `jazz-next/src/JazzNext/Compiler/Parser.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Parser/Lexer.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Parser/Lower.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Parser/Operator.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Desugar.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Analyzer.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/TypeInference.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Runtime.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/ModuleResolver.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Driver.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/PreludeContract.hs`
- Modify: `jazz-next/src/JazzNext/CLI/Main.hs`

**Step 1: Add module-level summaries**

```haskell
-- | Coordinates parser, analyzer, prelude loading, and runtime execution flows.
module JazzNext.Compiler.Driver
```

**Step 2: Add targeted helper comments**

```haskell
-- | Signatures must be consumed immediately; reaching any other statement
-- makes the pending declaration invalid.
flushPendingSignature :: Maybe PendingSignature -> [Diagnostic] -> [Diagnostic]
```

**Step 3: Keep implementation-only helpers readable**

Run: `sed -n '1,220p' jazz-next/src/JazzNext/Compiler/Analyzer.hs`
Expected: recursive-scope, signature, and purity rules are explained near the code that enforces them

## Task 3: Verify that the pass is comment-only and behavior-neutral

**Files:**
- Test: `jazz-next/scripts/test-warning-config.sh`

**Step 1: Run the active test harness**

Run: `bash jazz-next/scripts/test-warning-config.sh`
Expected: all listed `runghc` specs pass

**Step 2: Inspect the diff**

Run: `git diff -- docs/plans/2026-03-17-comment-pass.md jazz-next/src`
Expected: only comments/docs change; no executable logic changes
