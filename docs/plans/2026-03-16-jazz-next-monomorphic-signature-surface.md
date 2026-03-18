# Jazz-Next Monomorphic Signature Surface Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Extend `jazz-next` signature parsing beyond the current raw `Int`/`Bool` subset so concrete list types and simple monomorphic function types can be checked in the active compiler path.

**Architecture:** Keep the existing signature-adjacency contract and local `TypeInference` signature parser, but replace the current string match with a tiny recursive parser for the currently supportable type subset. Do not decide unresolved full type-grammar semantics in this batch: chained arrow associativity, constrained signatures, and named polymorphic variables remain explicitly out of scope and must stay rejected.

**Tech Stack:** Haskell (`jazz-next` parser/type inference/tests), Markdown plan/docs, shell verification via `runghc` and `bash jazz-next/scripts/test-warning-config.sh`.

---

## Progress

- [x] Verified recent operator/section and structured-diagnostics plans are already implemented in `jazz-next`; they are not the next code batch.
- [x] Verified active `jazz-next` signature parsing still accepts only raw `Int`/`Bool` in `jazz-next/src/JazzNext/Compiler/TypeInference.hs`.
- [x] Verified richer signature surface is listed as implementation-pending in `docs/jazz-language-state.md`.
- [x] Verified full type-grammar decisions remain unresolved in `docs/plans/2026-03-18-jazz-next-type-grammar-and-signature-rebase-plan.md`, so this batch must stay within a safe monomorphic subset.
- [x] Task 1 complete: failing tests lock the supported monomorphic signature surface and unresolved rejections.
- [x] Task 2 complete: `jazz-next` parses/checks concrete list and simple function signatures.
- [x] Task 3 complete: docs/trackers reflect the new supported subset and verification passes.

## Scope Guardrails

In scope:

- concrete signatures using `Int`, `Bool`, and nested list forms such as `[Int]` and `[[Bool]]`
- simple monomorphic function signatures with exactly one `->`, such as `Int -> Int` and `[Int] -> [Int]`
- parenthesized parameter/result types where needed for the supported single-arrow subset
- deterministic rejection of still-unsupported signature grammar

Out of scope:

- chained arrows such as `Int -> Int -> Int`
- constrained signatures (`@{...}:`)
- named type variables (`a`, `b`)
- tuple types, ADT names, and user-defined type constructors

## Task 1: Lock The Supported Signature Surface With Failing Tests

**Files:**
- Modify: `jazz-next/test/BindingSignatureCoherenceSpec.hs`

**Step 1: Write the failing tests**

Add source-pipeline coverage for:

- `x :: [Int]. x = [1].` compiling
- `x :: [[Bool]]. x = [[True], [False]].` compiling
- `f :: Int -> Int. f = (+ 1).` compiling
- `f :: [Int] -> [Int]. f = filter (> 1).` compiling
- `x :: [Bool]. x = [1].` reporting the existing signature-mismatch path
- `f :: Int -> Int -> Int. f = (+).` remaining rejected as unsupported in this batch

**Step 2: Run the targeted suite to verify RED**

Run:

```bash
runghc -i./jazz-next/src -i./jazz-next/test jazz-next/test/BindingSignatureCoherenceSpec.hs
```

Expected:

- new acceptance cases fail with `E2009`
- unsupported chained-arrow case stays rejected

**Step 3: Commit checkpoint**

```bash
git add docs/plans/2026-03-16-jazz-next-monomorphic-signature-surface.md \
  jazz-next/test/BindingSignatureCoherenceSpec.hs
git commit -m "test(jazz-next): lock monomorphic signature surface"
```

## Task 2: Implement Minimal Signature Parsing For The Supported Subset

**Files:**
- Modify: `jazz-next/src/JazzNext/Compiler/TypeInference.hs`

**Step 1: Add a tiny recursive signature parser**

Replace the raw `Int`/`Bool` string match with a parser that supports:

- base types: `Int`, `Bool`
- list types: `[type]`
- exactly one function arrow: `type -> type`
- parentheses for grouping supported subtypes

Keep rejection explicit for:

- second or later `->`
- unknown identifiers
- malformed brackets/parentheses

**Step 2: Keep diagnostics stable**

Do not add a new error code unless the current `E2009` path becomes ambiguous. Unsupported or malformed signature surface should continue to report through `E2009`.

**Step 3: Run targeted suite to verify GREEN**

Run:

```bash
runghc -i./jazz-next/src -i./jazz-next/test jazz-next/test/BindingSignatureCoherenceSpec.hs
```

Expected:

- new acceptance cases pass
- mismatch and unresolved chained-arrow rejection cases still behave deterministically

**Step 4: Commit checkpoint**

```bash
git add docs/plans/2026-03-16-jazz-next-monomorphic-signature-surface.md \
  jazz-next/src/JazzNext/Compiler/TypeInference.hs \
  jazz-next/test/BindingSignatureCoherenceSpec.hs
git commit -m "feat(jazz-next): parse monomorphic list and function signatures"
```

## Task 3: Align Docs, Trackers, And Verification

**Files:**
- Modify: `docs/jazz-language-state.md`
- Modify: `docs/feature-status.md`

**Step 1: Update docs**

Reflect that active `jazz-next` signature parsing now supports:

- concrete `Int`/`Bool`
- nested concrete list types
- single-arrow monomorphic function signatures

Keep unresolved full type-grammar work explicit.

**Step 2: Run focused and full verification**

Run:

```bash
runghc -i./jazz-next/src -i./jazz-next/test jazz-next/test/BindingSignatureCoherenceSpec.hs
bash jazz-next/scripts/test-warning-config.sh
```

Expected:

- focused suite passes
- full `jazz-next` verification passes

**Step 3: Review scope churn**

Run:

```bash
git diff --stat
git diff -- docs/jazz-language-state.md docs/feature-status.md jazz-next/src/JazzNext/Compiler/TypeInference.hs jazz-next/test/BindingSignatureCoherenceSpec.hs
```

**Step 4: Commit checkpoint**

```bash
git add docs/plans/2026-03-16-jazz-next-monomorphic-signature-surface.md \
  docs/jazz-language-state.md \
  docs/feature-status.md \
  jazz-next/src/JazzNext/Compiler/TypeInference.hs \
  jazz-next/test/BindingSignatureCoherenceSpec.hs
git commit -m "docs(jazz-next): track supported monomorphic signature surface"
```
