# Jazz-Next Shared Recursive Binding Helpers Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Extract the duplicated recursive-binding graph and free-variable helpers used by analyzer, type inference, and runtime into one shared `jazz-next` module without changing current lambda or recursion behavior.

**Architecture:** Introduce a new pure helper module, `JazzNext.Compiler.RecursiveBindings`, that owns declaration-ordered recursive-group discovery, binding-name collection, free-variable walking, and parameterized self-recursive binding detection. Keep phase-specific policy local: analyzer still decides how to store groups, type inference still decides when a wrapper contains any function branch, and runtime still decides when a wrapper reliably yields a function value.

**Tech Stack:** Haskell (`jazz-next` compiler modules and `runghc` test suites), Markdown plan tracking, shell verification via targeted `runghc` commands plus `bash jazz-next/scripts/test-warning-config.sh`.

---

## Progress

- [ ] Shared-helper extraction is queued as the next implementation task after the current lambda review-fix batch.
- [ ] Task 1 complete: characterization tests lock shared recursive-binding helper behavior.
- [ ] Task 2 complete: `JazzNext.Compiler.RecursiveBindings` provides the shared helper API.
- [ ] Task 3 complete: analyzer, type inference, and runtime consume the shared helper module with no behavior drift.
- [ ] Task 4 complete: full verification passes and the lambda tracker records the extraction as complete.

## Scope Guardrails

In scope:

- extracting `inferRecursiveGroups`, `freeVarsExprWithBound`, `freeVarsScopeWithBound`, `collectBindingNames`, and self-recursive binding scanning into a shared `jazz-next` module
- preserving declaration-order behavior for recursive groups so runtime and inference keep the same peer visibility they have today
- keeping the type-inference `exprContainsFunctionBranch` and runtime `exprYieldsFunctionValue` predicates local while sharing the scanner that consumes them
- adding dedicated tests that characterize the shared helper behavior directly

Out of scope:

- new lambda semantics, parser syntax, runtime diagnostics, or analyzer visibility rules
- changing the existing wrapper-policy difference between type inference (`any` function branch) and runtime (`all` branches yield function value)
- refactoring unrelated compiler utilities into the same shared module
- broad documentation rewrites outside the active lambda tracker

## Task 1: Lock Shared Helper Semantics With Direct Tests

**Files:**
- Create: `jazz-next/test/JazzNext/Compiler/Semantics/RecursiveBindingsSpec.hs`
- Modify: `jazz-next/scripts/test-warning-config.sh`

**Step 1: Write the failing helper characterization tests**

Add direct tests for the future shared module that cover:

- free-variable walking treats lambda parameters as bound names
- free-variable walking treats a binding as visible in its own RHS for self-recursion analysis
- recursive-group discovery preserves declaration order for mutual recursion through an alias bridge
- recursive-group discovery prefers the nearest earlier rebinding over a later declaration when both share a name
- self-recursive binding detection is parameterized by a caller-supplied predicate rather than hard-coding runtime or inference policy

Use hand-built `Expr` / `Statement` values so the tests pin the helper API directly instead of depending on parser coverage.

**Step 2: Run the new suite to verify RED**

Run:

```bash
runghc -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/RecursiveBindingsSpec.hs
```

Expected:

- the suite fails because `JazzNext.Compiler.RecursiveBindings` does not exist yet.

**Step 3: Add the suite to the default runner**

Update `jazz-next/scripts/test-warning-config.sh` so the helper suite becomes part of the normal verification path once it lands.

**Step 4: Commit checkpoint**

```bash
git add docs/plans/2026-03-17-jazz-next-shared-recursive-binding-helpers.md \
  jazz-next/test/JazzNext/Compiler/Semantics/RecursiveBindingsSpec.hs \
  jazz-next/scripts/test-warning-config.sh
git commit -m "test(jazz-next): lock recursive binding helper semantics"
```

## Task 2: Introduce A Shared Recursive-Binding Helper Module

**Files:**
- Create: `jazz-next/src/JazzNext/Compiler/RecursiveBindings.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Semantics/RecursiveBindingsSpec.hs`

**Step 1: Implement the shared pure helper API**

Add a new module that exports:

- `collectBindingNames :: [(Int, Statement)] -> Map Int Text`
- `freeVarsExprWithBound :: Set Text -> Expr -> Set Text`
- `freeVarsScopeWithBound :: Set Text -> [Statement] -> Set Text`
- `inferRecursiveGroupsOrdered :: Set Text -> [(Int, Statement)] -> Map Int [Int]`
- `inferSelfRecursiveBindings :: (Expr -> Bool) -> [(Int, Statement)] -> Set Int`

Implementation requirements:

- keep recursive component members in declaration order, not SCC traversal order
- keep the current dependency resolution rule: nearest earlier declaration wins, otherwise outer scope suppresses a local forward edge, otherwise use the first later declaration
- keep self-recursive scanning parameterized so runtime and type inference can supply different wrapper policies safely
- add brief comments only where the declaration-order or rebinding behavior is non-obvious

**Step 2: Run the helper suite to verify GREEN**

Run:

```bash
runghc -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/RecursiveBindingsSpec.hs
```

Expected:

- the helper suite passes with the new shared module in place.

**Step 3: Commit checkpoint**

```bash
git add docs/plans/2026-03-17-jazz-next-shared-recursive-binding-helpers.md \
  jazz-next/src/JazzNext/Compiler/RecursiveBindings.hs \
  jazz-next/test/JazzNext/Compiler/Semantics/RecursiveBindingsSpec.hs \
  jazz-next/scripts/test-warning-config.sh
git commit -m "feat(jazz-next): add shared recursive binding helpers"
```

## Task 3: Migrate Analyzer, Type Inference, And Runtime To The Shared Module

**Files:**
- Modify: `jazz-next/src/JazzNext/Compiler/Analyzer.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/TypeInference.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Runtime.hs`
- Test: `jazz-next/test/JazzNext/Compiler/Semantics/BindingSignatureCoherenceSpec.hs`
- Test: `jazz-next/test/JazzNext/Compiler/Semantics/LambdaSemanticsSpec.hs`
- Test: `jazz-next/test/JazzNext/Compiler/Semantics/RuntimeSemanticsSpec.hs`

**Step 1: Replace duplicated free-var and binding-name helpers**

Import the new shared module in all three phases and delete the duplicated local definitions of:

- `collectBindingNames`
- `freeVarsExprWithBound`
- `freeVarsScopeWithBound`

Keep each phase’s surrounding comments and call sites readable after the extraction.

**Step 2: Replace duplicated recursive-group discovery**

Use `inferRecursiveGroupsOrdered` from the shared module:

- analyzer can convert the returned ordered member list to `Set Int` at the boundary where it stores `recursiveGroupsByStatement`
- type inference and runtime should keep the declaration-ordered list form they already consume

Do not change any peer-environment or binding-seed logic in this step beyond swapping in the shared helper result.

**Step 3: Replace duplicated self-recursive binding scanning**

Use `inferSelfRecursiveBindings` from the shared module while keeping phase-local wrapper policies:

- `TypeInference.hs` keeps `exprContainsFunctionBranch`
- `Runtime.hs` keeps `exprYieldsFunctionValue`

This step must not collapse those two predicates into one helper; the current semantic difference is intentional.

**Step 4: Run targeted regression suites**

Run:

```bash
runghc -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/RecursiveBindingsSpec.hs
runghc -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/BindingSignatureCoherenceSpec.hs
runghc -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/LambdaSemanticsSpec.hs
runghc -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/RuntimeSemanticsSpec.hs
```

Expected:

- all four suites pass with no diagnostic or runtime behavior drift.

**Step 5: Commit checkpoint**

```bash
git add docs/plans/2026-03-17-jazz-next-shared-recursive-binding-helpers.md \
  jazz-next/src/JazzNext/Compiler/RecursiveBindings.hs \
  jazz-next/src/JazzNext/Compiler/Analyzer.hs \
  jazz-next/src/JazzNext/Compiler/TypeInference.hs \
  jazz-next/src/JazzNext/Compiler/Runtime.hs \
  jazz-next/test/JazzNext/Compiler/Semantics/RecursiveBindingsSpec.hs \
  jazz-next/scripts/test-warning-config.sh
git commit -m "refactor(jazz-next): share recursive binding helpers"
```

## Task 4: Finish Verification And Close The Tracker Loop

**Files:**
- Modify: `docs/plans/2026-03-17-jazz-next-lambda-support.md`
- Modify: `docs/plans/2026-03-17-jazz-next-shared-recursive-binding-helpers.md`

**Step 1: Update progress checkboxes**

Mark the shared-helper extraction complete in both plan files once the implementation lands, including a short note in the lambda tracker that the follow-up is no longer pending.

**Step 2: Run full verification**

Run:

```bash
bash jazz-next/scripts/test-warning-config.sh
bash scripts/check-docs.sh
```

Expected:

- the full `jazz-next` verification script passes
- docs checks pass

**Step 3: Commit checkpoint**

```bash
git add docs/plans/2026-03-17-jazz-next-lambda-support.md \
  docs/plans/2026-03-17-jazz-next-shared-recursive-binding-helpers.md
git commit -m "docs(plans): close shared helper extraction follow-up"
```
