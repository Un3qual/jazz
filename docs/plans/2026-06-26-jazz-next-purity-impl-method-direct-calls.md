---
id: JN-PURITY-IMPL-METHOD-DIRECT-CALLS-001
status: done
priority: P2
size: S
kind: impl
autonomous_ready: yes
depends_on: []
last_verified: 2026-06-28
completed_on: 2026-06-28
plan_section: "Task 1: Impl method direct-call purity"
target_paths:
  - jazz-next/src/JazzNext/Compiler/Analyzer.hs
  - jazz-next/test/JazzNext/Compiler/Semantics/PuritySemanticsSpec.hs
verification:
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/PuritySemanticsSpec.hs
  - bash scripts/check-execution-queue.sh
  - bash scripts/check-docs.sh
  - git diff --check
deliverable: "Apply the existing stub-v1 bang-suffix direct-call purity rule to active local impl method bodies with focused acceptance/rejection coverage, without adding effect types, higher-order purity, cross-module graphs, runtime enforcement, inferred effects, or signature effect typing."
---

# Jazz-Next Impl Method Direct-Call Purity Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Prove and, if needed, enforce that local `impl` method bodies obey the
active stub-v1 direct-call purity rule.

**Architecture:** Reuse the existing name-driven `!` purity classifier and
analyzer context machinery. Method bodies already have active local syntax and
metadata, so this child only applies the same direct-call check used for
ordinary bindings to local `impl` method bodies.

**Tech Stack:** Haskell `jazz-next` analyzer and focused runghc semantics
coverage in `PuritySemanticsSpec.hs`.

---

## Source Evidence

- `docs/spec/semantics/purity-bang-stub-v1.md` is the active contract: names
  ending in `!` are impure, pure bodies cannot directly call known impure
  callees, impure bodies can call either pure or impure callees, and top-level
  expression statements remain permissive.
- `docs/plans/spec-cleanup/2026-03-02/decisions/03-purity-bang-semantics.md`
  records stub-v1 as deliberately name-driven and direct-call focused, with
  higher-order purity proofs, effect polymorphism, cross-module purity graphs,
  and runtime purity enforcement left out of scope.
- `docs/spec/authoritative-syntax.md` records local `class`/`impl` method
  metadata and explicit `Class::method` dispatch as active, while dictionaries,
  default methods, inferred constraints, module export/import method behavior,
  and runtime evidence remain future work.
- `jazz-next/src/JazzNext/Compiler/Analyzer.hs` already has the natural
  executor target: `collectImplMethodDiagnostics`, `contextForImplMethod`, and
  `shouldRejectImpureCall`.
- `jazz-next/test/JazzNext/Compiler/Semantics/PuritySemanticsSpec.hs` already
  covers ordinary bindings, `print!`, dollar application, top-level
  permissiveness, and a pure binding calling an impure qualified method. It does
  not yet lock the method-body side of the same direct-call rule.

## Executor-Safe Scope

Implement only local impl-method direct-call purity:

- A pure method body such as `run = \(value) -> inc! value.` must emit `E1010`
  when `inc!` is a known local impure callee.
- An impure method body such as `run! = \(value) -> inc! value.` must compile
  when `inc!` is a known local impure callee.
- Ordinary binding behavior, `print!` behavior, top-level permissiveness, and
  qualified method call-site purity behavior must stay unchanged.

Out of scope:

- Higher-order purity, including passing or returning impure function values.
- Effect types, effect polymorphism, inferred effects, or effect annotations in
  signatures.
- Cross-module purity graphs or module method export/import semantics.
- Runtime purity enforcement, dictionaries, default methods, superclasses, or
  runtime evidence values.
- Any edits under `jazz-hs/` or `jazz2/`.

## Task 1: Impl method direct-call purity

**Files:**

- Modify: `jazz-next/test/JazzNext/Compiler/Semantics/PuritySemanticsSpec.hs`
- Inspect or modify: `jazz-next/src/JazzNext/Compiler/Analyzer.hs`

- [x] **Step 1: Add focused method-body purity tests**

In `jazz-next/test/JazzNext/Compiler/Semantics/PuritySemanticsSpec.hs`, add two
test registrations next to the existing qualified-method purity coverage:

```haskell
    ("pure impl method cannot call impure callee", testPureImplMethodCannotCallImpureCallee),
    ("impure impl method can call impure callee", testImpureImplMethodCanCallImpureCallee),
```

Add the test bodies:

```haskell
testPureImplMethodCannotCallImpureCallee :: IO ()
testPureImplMethodCannotCallImpureCallee = do
  result <-
    compileSource
      defaultWarningSettings
      "class Runner(a) {\nrun :: a -> a.\n}.\ninc! = (+ 1).\nimpl Runner(Int) {\nrun = \\(value) -> inc! value.\n}.\nRunner::run 1."
  assertSingleErrorContains
    "pure impl method calling impure callee"
    "impl method 'run' cannot call impure callee 'inc!'"
    (compileErrors result)
  assertSingleDiagnosticSubject
    "pure impl method diagnostic subject"
    "run"
    (compileErrors result)

testImpureImplMethodCanCallImpureCallee :: IO ()
testImpureImplMethodCanCallImpureCallee = do
  result <-
    compileSource
      defaultWarningSettings
      "class Runner(a) {\nrun! :: a -> a.\n}.\ninc! = (+ 1).\nimpl Runner(Int) {\nrun! = \\(value) -> inc! value.\n}.\nRunner::run! 1."
  assertEqual "compile errors" [] (compileErrors result)
```

- [x] **Step 2: Run the focused purity suite**

Run:

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/PuritySemanticsSpec.hs
```

Expected before implementation: the new pure-method test fails if impl method
bodies do not yet carry method purity context. If both new tests pass, record
that `Analyzer.hs` already had the required behavior and keep this child as a
focused regression-locking change.

- [x] **Step 3: Ensure impl methods use method-name purity context**

If Step 2 fails, update `jazz-next/src/JazzNext/Compiler/Analyzer.hs` so
`collectImplMethodDiagnostics` checks each method body with
`contextForImplMethod methodName methodSpan`, and make sure
`contextForImplMethod` derives permission from `identifierPurity methodName`.
The relevant shape should be:

```haskell
collectImplMethodDiagnostics ::
  BuiltinResolutionMode ->
  WarningSettings ->
  Map Text VisibleBinding ->
  Set Text ->
  [ImplMethod] ->
  ([WarningRecord], [Diagnostic])
collectImplMethodDiagnostics builtinMode settings visibleBindings visibleClassNames methods =
  foldr step ([], []) methods
  where
    step (ImplMethod methodName methodSpan methodExpr) (warningsAcc, errorsAcc) =
      let (methodWarnings, methodErrors) =
            collectExprDiagnostics
              builtinMode
              settings
              visibleBindings
              visibleClassNames
              (contextForImplMethod methodName methodSpan)
              methodExpr
       in (methodWarnings ++ warningsAcc, methodErrors ++ errorsAcc)
```

And:

```haskell
contextForImplMethod :: Identifier -> SourceSpan -> AnalysisContext
contextForImplMethod methodName methodSpan =
  AnalysisContext
    { contextLabel = "impl method '" <> identifierText methodName <> "'",
      contextAllowsImpureCalls = identifierPurity methodName == Impure,
      contextPrimarySpan = Just methodSpan,
      contextSubject = Just (identifierText methodName),
      contextLambdaSpan = Just methodSpan
    }
```

- [x] **Step 4: Re-run focused verification**

Run:

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/PuritySemanticsSpec.hs
```

Expected: the suite exits `0`, with the new pure-method rejection and impure
method acceptance covered.

- [x] **Step 5: Run queue/docs hygiene checks**

Run:

```bash
bash scripts/check-execution-queue.sh
bash scripts/check-docs.sh
git diff --check
```

Expected after the central queue row is added: all three commands exit `0`.
Before central queue integration, the queue/docs checks may fail because this
plan is not yet linked from `docs/execution/queue.md`; report that exact output
instead of broadening scope.

## Handoff Notes

- Queue closure is recorded in `docs/execution/queue.md` with Done evidence for
  the regression-locked impl-method purity rule.
- The plan is executor-safe because it only exercises direct calls to known
  local callees from already-active local impl method bodies.
- If the focused tests pass before any analyzer edit, commit the regression
  coverage only and do not churn `Analyzer.hs`.
