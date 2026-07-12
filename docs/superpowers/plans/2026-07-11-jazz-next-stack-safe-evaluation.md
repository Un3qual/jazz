---
id: JN-BOOTSTRAP-STACK-SAFE-EVALUATION-001
status: ready
priority: P1
size: L
kind: impl
autonomous_ready: yes
depends_on:
  - JN-BOOTSTRAP-HOST-TEXT-IO-001
last_verified: 2026-07-11
plan_section: "Implementation Batch: Stack-Safe Evaluation"
target_paths:
  - docs/execution/blocker-contracts.md
  - docs/execution/queue.md
  - docs/superpowers/specs/2026-07-10-jazz-next-bootstrap-interpreter-profile-design.md
  - docs/superpowers/specs/2026-07-11-jazz-next-stack-safe-evaluation-design.md
  - docs/superpowers/plans/2026-07-11-jazz-next-stack-safe-evaluation.md
  - jazz-next/src/JazzNext/Compiler/Runtime.hs
  - jazz-next/test/JazzNext/Compiler/Semantics/Runtime/RecursionTests.hs
  - jazz-next/test/JazzNext/Compiler/Semantics/Runtime/HostIOTests.hs
  - jazz-next/test/JazzNext/Compiler/Modules/LoaderSpec.hs
verification:
  - cabal test --project-dir=jazz-next runtime-semantics-spec loader-spec --test-show-details=failures
  - bash jazz-next/scripts/test-warning-config.sh
  - bash scripts/check-execution-queue.sh
  - bash scripts/check-docs.sh
  - git diff --check
deliverable: "Replace the duplicated recursive pure and host expression evaluators with one interpreter-private explicit evaluation machine that preserves current semantics and RuntimeHost ordering while completing 50,000 pure, 20,000 host-path, and 20,000 imported-module tail calls without growing the Haskell call stack or introducing bytecode/LLVM coupling."
---

# Jazz-Next Stack-Safe Evaluation Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use
> superpowers:subagent-driven-development (recommended) or
> superpowers:executing-plans to implement this plan task-by-task. Steps use
> checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make bootstrap-scale Jazz tail recursion safe through one shared
pure/host evaluation machine without creating a bytecode or LLVM-adjacent
execution layer.

**Architecture:** Keep the public `Runtime`, `ModuleRuntime`, and `Driver` APIs
stable. Replace the mutually recursive pure and host expression/application
paths inside `Runtime.hs` with an interpreter-private CEK-style control loop;
pure wrappers install the disabled host through `Identity`, while host wrappers
reuse their current `RuntimeHostEvaluationT` state. Scope construction remains
responsible for recursive cells and deferred-host caching, but terminal block
expressions transfer back into the shared loop with the current continuation.

**Tech Stack:** Haskell 2010, `ExceptT`, strict `StateT`, `Identity`, the
existing Jazz AST/runtime/module pipeline, the custom `NamedTest` harness, and
Cabal component suites.

## Global Constraints

- Modify only `jazz-next/` and active documentation; `jazz-hs/` and `jazz2/`
  remain read-only.
- Follow the approved stack-safe evaluation design exactly.
- Keep `Runtime.hs` as the sole owner of evaluator controls, continuations,
  callable application, result policies, and expression-level host dispatch.
- Keep `ModuleRuntime.hs` responsible only for dependency order, imported
  environments, export publication, and entry output.
- Keep `Driver.hs` free of continuations, recursion limits, and host scheduling.
- Preserve call-by-value, left-to-right evaluation and exact existing runtime
  diagnostics, numeric defaulting, runtime hints, lexical environments, module
  paths, capability evidence, and deferred-host cache identity.
- Run every reached host operation exactly once and never run effects from
  unselected branches or case arms.
- Do not add a language-visible recursion limit or a new runtime diagnostic.
- Do not add bytecode, opcodes, a VM, instruction serialization, lowered IR,
  LLVM operations, object emission, linking, or a native runtime.
- Prefer behavior tests over source-string assertions.
- Implement behavior test-first and commit each independently reviewable task.

---

## File Ownership Map

| File | Responsibility in this batch |
| --- | --- |
| `jazz-next/src/JazzNext/Compiler/Runtime.hs` | Private machine model, expression transitions, callable transitions, return policies, block terminal transfer, pure/host wrappers. |
| `jazz-next/test/JazzNext/Compiler/Semantics/Runtime/RecursionTests.hs` | Pure 50,000-call regression, case/block tail-position coverage, leaked-exception and timeout classification, diagnostic parity. |
| `jazz-next/test/JazzNext/Compiler/Semantics/Runtime/HostIOTests.hs` | Host-path 20,000-call regression and exact-once ordering across tail transfer. |
| `jazz-next/test/JazzNext/Compiler/Modules/LoaderSpec.hs` | Dependency-export closure regression at 20,000 calls with the real module graph. |
| Active docs listed in frontmatter | Accepted ownership, queue metadata, implementation evidence, and closeout state. |

`ModuleRuntime.hs`, `Driver.hs`, and `RuntimeSemanticsSpec.hs` are verification
boundaries, not planned edits. The existing runtime suite already imports
`RecursionTests` and `HostIOTests`.

---

## Implementation Batch: Stack-Safe Evaluation

### Task 1: Lock pure and host/module depth regressions in RED

**Files:**

- Modify: `jazz-next/test/JazzNext/Compiler/Semantics/Runtime/RecursionTests.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Semantics/Runtime/HostIOTests.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Modules/LoaderSpec.hs`

**Interfaces:**

- Consumes: `runSource`, `evaluateRuntimeExprWithHost`,
  `runModuleGraphWithPrelude`, `RunResult`, `System.Timeout.timeout`, and
  `Control.Exception.try`.
- Produces: three behavioral regression floors and helpers that classify
  success, timeout, leaked Haskell exception, and Jazz failure separately.

- [ ] **Step 1: Add a reusable result classifier to `RecursionTests.hs`.**

Add `assertStackSafeRunResult` with this exact contract:

```haskell
assertStackSafeRunResult :: Text -> IO RunResult -> Maybe Text -> IO ()
assertStackSafeRunResult label action expectedOutput = do
  maybeOutcome <-
    timeout
      30000000
      (try action :: IO (Either SomeException RunResult))
  case maybeOutcome of
    Nothing ->
      failTest (label <> " timed out")
    Just (Left err) ->
      failTest (label <> " leaked host exception: " <> Text.pack (show err))
    Just (Right result) -> do
      assertEqual (label <> " compile errors") [] (runCompileErrors result)
      assertEqual (label <> " runtime errors") [] (runRuntimeErrors result)
      assertEqual (label <> " output") expectedOutput (runOutput result)
```

Register and implement these tests:

```haskell
testTailRecursiveClosureIsStackSafe :: IO ()
testTailRecursiveClosureIsStackSafe =
  assertStackSafeRunResult
    "50,000-call pure tail recursion"
    ( runSource
        defaultWarningSettings
        ( "countDown = \\(remaining) -> "
            <> "if remaining == 0 0 else { "
            <> "next = remaining - 1. countDown next. }. "
            <> "countDown 50000."
        )
    )
    (Just "0")

testTailRecursiveCaseArmIsStackSafe :: IO ()
testTailRecursiveCaseArmIsStackSafe =
  assertStackSafeRunResult
    "10,000-call case-arm tail recursion"
    ( runSource
        defaultWarningSettings
        ( "countDown = \\(remaining) -> case remaining { "
            <> "| 0 -> 0 | _ -> countDown (remaining - 1) }. "
            <> "countDown 10000."
        )
    )
    (Just "0")

testTypedTailRecursiveClosureIsStackSafe :: IO ()
testTypedTailRecursiveClosureIsStackSafe =
  assertStackSafeRunResult
    "10,000-call typed tail recursion"
    ( runSource
        defaultWarningSettings
        ( "countDown :: Int -> Int. "
            <> "countDown = \\(remaining) -> "
            <> "if remaining == 0 0 else countDown (remaining - 1). "
            <> "countDown 10000."
        )
    )
    (Just "0")
```

The first test covers closure-body, `if`-branch, ordinary-binding, and terminal
block transfer. The second independently covers selected case-arm transfer.
The third prevents result-hint handling from turning typed tail recursion back
into a continuation chain or applying the `Int` result policy at the wrong
boundary.

- [ ] **Step 2: Add the host-path regression to `HostIOTests.hs`.**

Register `testHostTailRecursionIsStackSafe`. Reuse `statefulHost`, build the
AST directly, and require one effect before the recursive result:

```haskell
testHostTailRecursionIsStackSafe :: IO ()
testHostTailRecursionIsStackSafe = do
  callsRef <- newIORef []
  let isZero = EBinary "==" (EVar "remaining") (ELit (LInt 0))
      decrement =
        EApply
          (EVar "countDown!")
          (EBinary "-" (EVar "remaining") (ELit (LInt 1)))
      expression =
        EBlock
          [ SLet
              "countDown!"
              (SourceSpan 1 1)
              (ELambda "remaining" (EIf isZero (ELit (LInt 0)) decrement)),
            SExpr
              (SourceSpan 2 1)
              (hostCall "__kernel_writeStdoutRaw!" [ELit (LText "before")]),
            SExpr
              (SourceSpan 3 1)
              (EApply (EVar "countDown!") (ELit (LInt 20000)))
          ]
  maybeOutcome <-
    timeout
      30000000
      (evaluateRuntimeExprWithHost (recordingIOHost callsRef) expression)
  case maybeOutcome of
    Nothing -> failTest "20,000-call host-path tail recursion timed out"
    Just result -> do
      calls <- readIORef callsRef
      assertEqual
        "host-path tail result"
        (Right (Just "0"))
        (fmap (fmap renderRuntimeValue) result)
      assertEqual "host-path effects execute once" [WriteStdoutCall "before"] calls
```

Reuse the existing `recordingIOHost`, `newIORef`, and `readIORef` definitions.
Add `failTest` and `timeout` to this module's imports.

- [ ] **Step 3: Add the imported-module regression to `LoaderSpec.hs`.**

Register `testImportedTailRecursiveClosureIsStackSafe` and use this exact
module graph:

```haskell
testImportedTailRecursiveClosureIsStackSafe :: IO ()
testImportedTailRecursiveClosureIsStackSafe = do
  maybeResult <-
    timeout
      30000000
      ( try
          ( runModuleGraphWithPrelude
              defaultWarningSettings
              Nothing
              resolverConfig
              ["App", "Main"]
              lookupSource
          )
          :: IO (Either SomeException RunResult)
      )
  case maybeResult of
    Nothing -> failTest "20,000-call imported tail recursion timed out"
    Just (Left err) ->
      failTest ("imported tail recursion leaked host exception: " <> Text.pack (show err))
    Just (Right result) -> do
      assertEqual "imported tail compile errors" [] (runCompileErrors result)
      assertEqual "imported tail runtime errors" [] (runRuntimeErrors result)
      assertEqual "imported tail output" (Just "0") (runOutput result)
  where
    counterSource =
      "module Library::Counter (countDown) {\n"
        <> "countDown = \\(remaining) -> case remaining {\n"
        <> "| 0 -> 0\n"
        <> "| _ -> countDown (remaining - 1)\n"
        <> "}.\n"
        <> "}"
    entrySource =
      "module App::Main {\n"
        <> "import Library::Counter.\n"
        <> "countDown 20000.\n"
        <> "}"
    lookupSource "src/Library/Counter.jz" = pure (Just counterSource)
    lookupSource "src/App/Main.jz" = pure (Just entrySource)
    lookupSource _ = pure Nothing
```

Add `SomeException`, `try`, `failTest`, and `timeout` imports.

- [ ] **Step 4: Run the focused suite and capture RED evidence.**

Run:

```bash
cabal test --project-dir=jazz-next runtime-semantics-spec loader-spec --test-show-details=failures
```

Expected: each new deep regression either times out, leaks a Haskell stack
overflow, or terminates abnormally in the current recursive evaluator. Existing
tests must remain green up to the first failing deep regression. Record the
actual failing test names and failure mode in the plan's execution notes; do
not weaken depths or timeouts to manufacture RED.

Do not commit while the suite is red. Continue directly to Task 2.

### Task 2: Introduce the shared evaluation-machine model

**Files:**

- Modify: `jazz-next/src/JazzNext/Compiler/Runtime.hs`

**Interfaces:**

- Consumes: `Expr`, `RuntimeEnv`, `RuntimeValue`, `RuntimeHost`,
  `RuntimeHostEvaluationT`, `BuiltinResolutionMode`, binding runtime hints, and
  existing pure primitive/type-hint helpers.
- Produces: private `EvaluationContext`, `EvaluationControl`,
  `EvaluationFrame`, `EvaluationContinuation`, `RuntimeResultObligation`,
  `RuntimeReturnPolicy`, and `runEvaluationMachine`. Existing exported
  functions keep their signatures.

- [ ] **Step 1: Add the private machine types next to the runtime environment.**

Use these concrete responsibilities and field types:

```haskell
data EvaluationContext = EvaluationContext
  { evaluationModulePath :: Maybe [Text],
    evaluationBuiltinMode :: BuiltinResolutionMode,
    evaluationBindingTypeHints :: Map BindingRuntimeHintKey SignatureType,
    evaluationEnvironment :: RuntimeEnv
  }

data RuntimeResultObligation
  = ApplyFunctionResultHint SignatureType
  | ApplyExplicitResultHint SignatureType
  | AttachDefaultIntegerResult
  deriving (Eq, Show)

newtype RuntimeReturnPolicy =
  RuntimeReturnPolicy [RuntimeResultObligation]

data EvaluationControl
  = EvaluateExpression EvaluationContext Expr
  | ApplyCallable RuntimeValue RuntimeValue
  | ForceRuntimeValue RuntimeValue
  | ReturnRuntimeValue RuntimeValue

data EvaluationFrame
  = EvaluateApplicationArgument EvaluationContext Expr
  | ApplyEvaluatedFunction RuntimeValue
  | EvaluateListElement EvaluationContext [RuntimeValue] [Expr]
  | EvaluateTupleElement EvaluationContext [RuntimeValue] [Expr]
  | EvaluateIfBranch EvaluationContext Expr Expr
  | EvaluateCaseArms EvaluationContext [CaseArm]
  | EvaluateCaseGuard EvaluationContext RuntimeEnv Expr [CaseArm]
  | EvaluateBuiltinRightOperand EvaluationContext Text Expr
  | ApplyBuiltinBinary Text RuntimeValue
  | EvaluateDeclaredLeftOperand EvaluationContext RuntimeValue Expr
  | EvaluateDeclaredRightOperand EvaluationContext Expr
  | EvaluateLeftSection EvaluationContext Text
  | EvaluateRightSection EvaluationContext Text
  | ApplyTypeApplicationHint EvaluationContext SourceSpan SignatureType
  | ApplyRemainingArguments [RuntimeValue]

data EvaluationContinuation = EvaluationContinuation
  { continuationReturnPolicy :: RuntimeReturnPolicy,
    continuationFrame :: EvaluationFrame
  }

data EvaluationMachine = EvaluationMachine
  { evaluationControl :: EvaluationControl,
    evaluationContinuations :: [EvaluationContinuation],
    evaluationReturnPolicy :: RuntimeReturnPolicy
  }
```

Do not add a generic callback/function-valued frame; frames must remain
inspectable data. The listed frames cover every current `Expr` constructor and
qualified-method argument replay.

- [ ] **Step 2: Implement the loop and value-return protocol.**

Add:

```haskell
runEvaluationMachine ::
  Monad m =>
  RuntimeHost (RuntimeHostEvaluationT m) ->
  EvaluationContext ->
  Expr ->
  ExceptT Diagnostic (RuntimeHostEvaluationT m) RuntimeValue
runEvaluationMachine host context expression =
  advance
    EvaluationMachine
      { evaluationControl = EvaluateExpression context expression,
        evaluationContinuations = [],
        evaluationReturnPolicy = RuntimeReturnPolicy []
      }
  where
    advance machine =
      stepEvaluationMachine host machine >>= \next ->
        case next of
          EvaluationFinished value -> pure value
          EvaluationContinues nextMachine -> advance nextMachine

data EvaluationProgress
  = EvaluationFinished RuntimeValue
  | EvaluationContinues EvaluationMachine
```

`stepEvaluationMachine` must perform one semantic transition. When a nested
non-tail expression is suspended, push `EvaluationContinuation` with the
caller's current return policy and start the nested evaluation with an empty
policy. `ReturnRuntimeValue` first discharges the current policy in the same
inner-to-outer order as the old recursive calls, then consumes one frame and
restores that frame's saved parent policy. With no frame remaining, the
discharged value is `EvaluationFinished`.

This boundary rule is required for typed non-tail calls such as `f value + 1`:
`f`'s result hint must be applied before binary evaluation resumes, while the
outer function's return policy remains pending.

- [ ] **Step 3: Migrate every expression constructor to machine transitions.**

Implement the exhaustive mapping below. Each row is required; the compiler's
exhaustiveness check is not a substitute for preserving the stated order.

| Expression | Transition |
| --- | --- |
| `ELit` | Return `literalRuntimeValue`. |
| `EVar` | Resolve the cell/builtin, then force qualified/deferred values through the shared host-aware forcing path. |
| `ELambda` | Return `VClosure` with the current environment and module path. |
| `EOperatorValue` | Return a builtin operator or resolve the declared operator binding. |
| `EList` / `ETuple` | Evaluate elements left to right using accumulator frames; reverse the accumulator once. |
| `EApply` | Push argument expression, evaluate function, then push evaluated function while evaluating the argument. |
| `ETypeApplication` | Evaluate its function first, then apply the existing concrete/runtime shape hint logic. |
| `EIf` | Push branch choice, evaluate condition, then transfer directly to exactly one selected branch. |
| `EPatternCase` | Evaluate scrutinee once, match arms in source order, evaluate only matching guards, then transfer directly to the selected body. |
| `EBinary` | Preserve builtin left-to-right operands and declared operator/function/left/right order. |
| `ESectionLeft` / `ESectionRight` | Preserve current operand-first behavior and declared-section closure construction. |
| `EBlock` | Evaluate the prefix scope, then transfer directly to its final `SExpr`; preserve `E3006` for a block without a terminal result. |

For a terminal block, split only the final `SExpr`. Evaluate the prefix with
the current host-aware scope machinery, retain its resulting environment and
active module path, and set `EvaluateExpression` for the terminal expression
without pushing a continuation. A non-terminal or declaration-only block must
preserve current `E3006` behavior.

Use an explicit helper to retain the module declaration active at the terminal
expression:

```haskell
modulePathAfterStatements :: Maybe [Text] -> [Statement] -> Maybe [Text]
modulePathAfterStatements =
  foldl'
    ( \activeModulePath statement ->
        case statement of
          SModule _ modulePath -> Just modulePath
          _ -> activeModulePath
    )
```

For binary evaluation, `EvaluateBuiltinRightOperand` suspends the returned left
value while evaluating the right operand, and `ApplyBuiltinBinary` invokes the
primitive with both values. `EvaluateDeclaredLeftOperand` applies the resolved
operator to the returned left value while
`EvaluateDeclaredRightOperand context rightExpr` waits for that partial
callable; after the partial callable returns, evaluate `rightExpr` and use
`ApplyEvaluatedFunction` for the final tail application.

- [ ] **Step 4: Route both public expression paths through the machine.**

Keep the exported signatures unchanged. Make `evalValueWithHost` a thin call
to `runEvaluationMachine` with an `EvaluationContext`. Make the pure
`evalValueWithModulePath` specialization install `disabledRuntimeHost` and
`Identity` around the same machine:

```haskell
evalValueWithModulePath currentModulePath builtinMode bindingTypeHints env expression =
  runIdentity
    ( runRuntimeHostEvaluation disabledRuntimeHost $ \host ->
        runExceptT
          ( runEvaluationMachine
              host
              (EvaluationContext currentModulePath builtinMode bindingTypeHints env)
              expression
          )
    )
```

Do not leave the old pure expression `case` as a fallback. There must be one
expression transition table.

- [ ] **Step 5: Compile before migrating callable application.**

Run:

```bash
cabal build --project-dir=jazz-next all
```

Expected: PASS once all expression constructors and frames are exhaustive.
The deep tests may still fail because closure application still recursively
re-enters evaluation; do not claim GREEN yet.

### Task 3: Move callable application and result policy into the loop

**Files:**

- Modify: `jazz-next/src/JazzNext/Compiler/Runtime.hs`
- Test: `jazz-next/test/JazzNext/Compiler/Semantics/Runtime/RecursionTests.hs`

**Interfaces:**

- Consumes: `ApplyCallable`, `RuntimeResultObligation`, existing builtin,
  operator, constructor, qualified-method, argument-hint, and result-hint
  helpers.
- Produces: stack-safe closure-body transfer for ordinary, operator,
  qualified-method, prelude, and imported closures.

- [ ] **Step 1: Replace recursive callable application with transitions.**

Implement `ApplyCallable` cases with this exact policy:

| Runtime value | Action |
| --- | --- |
| `VDeferredHostBinding` | Force once through the existing scoped cache, then set `ApplyCallable` to the forced value and original argument. |
| `VExplicitTypeApplication` | Instantiate the function hint before application; otherwise append `ApplyExplicitResultHint` and reapply the inner callable. |
| `VExplicitResultHint` | Append `ApplyExplicitResultHint` and reapply the inner callable. |
| `VTyped` | Apply its argument hint immediately, append `ApplyFunctionResultHint`, and reapply the inner callable. |
| `VClosure` | Apply its argument hint, append its result/default obligation, replace control with `EvaluateExpression` in the captured environment/module path, and retain the caller's continuation frames unchanged. |
| sections, builtins, operators, constructors | Use the existing primitive helpers and return their immediate or partial value. |
| `VQualifiedMethod` | Select the candidate with existing evidence and ambiguity rules, then feed captured arguments through `ApplyCallable` in order; the final application uses the caller's continuation. |
| non-callable | Preserve `E3008` and the current rendered runtime type. |

Append obligations in semantic return order. Normalize adjacent duplicate
`AttachDefaultIntegerResult` obligations and adjacent identical
`ApplyFunctionResultHint` obligations; both are idempotent under the current
runtime hint helpers. Do not normalize explicit-result obligations, infer
compatibility between distinct hints, or reorder distinct obligations.

- [ ] **Step 2: Remove the old recursive application implementations.**

Delete the expression-evaluating branches from `applyRuntimeFunction` and
`applyRuntimeFunctionWithHost`. Retain pure primitive helpers that do not call
back into expression evaluation. Rename a retained helper only if its name
otherwise falsely claims to evaluate closures.

Use this search as a structural gate:

```bash
rg -n "evalValueWithModulePath|evalValueWithHost" jazz-next/src/JazzNext/Compiler/Runtime.hs
```

Expected: occurrences are entry wrappers, scope entry points, deferred binding
evaluation, or comments only. Neither callable application function may invoke
an evaluator recursively.

- [ ] **Step 3: Run the pure regression and runtime suite.**

Run:

```bash
cabal test --project-dir=jazz-next runtime-semantics-spec --test-show-details=failures
```

Expected: PASS, including the 50,000-call ordinary recursion, 10,000-call
case-arm regression, and 10,000-call typed recursion. Confirm the RED failure
mode recorded in Task 1 is absent.

- [ ] **Step 4: Commit the shared pure machine milestone.**

```bash
git add jazz-next/src/JazzNext/Compiler/Runtime.hs jazz-next/test/JazzNext/Compiler/Semantics/Runtime/RecursionTests.hs
git commit -m "feat: add stack-safe runtime evaluation machine"
```

### Task 4: Preserve host sequencing through tail transfer

**Files:**

- Modify: `jazz-next/src/JazzNext/Compiler/Runtime.hs`
- Test: `jazz-next/test/JazzNext/Compiler/Semantics/Runtime/HostIOTests.hs`

**Interfaces:**

- Consumes: the shared machine from Task 3, `RuntimeHostEvaluationT`, deferred
  host cells, and `DeferredHostScopeId` caching.
- Produces: the same evaluator transitions under a configured host with exact
  effect order and no duplicate effect execution.

- [ ] **Step 1: Audit every host operation boundary against the shared loop.**

Keep host calls only in `evalBuiltinWithHost` and deferred-cell forcing. Verify
the shared machine reaches them after evaluating arguments in source order.
Do not add a second host-specific expression `case`.

The following existing ordering test must remain unchanged and green:

```haskell
assertEqual
  "only selected nested effects run"
  [ WriteStdoutCall "closure",
    WriteStdoutCall "branch",
    WriteStderrCall "arm",
    WriteStdoutCall "block"
  ]
  calls
```

- [ ] **Step 2: Make terminal block transfer reuse the current host state.**

When an `EBlock` prefix creates `VDeferredHostBinding` cells, evaluate its
terminal expression in the same `RuntimeHostEvaluationT` run. Do not call
`runRuntimeHostEvaluation` from inside the host machine. This preserves both
the scope ID and memoization cache used by
`testHostBindingCacheSeparatesDynamicScopeInvocations`.

- [ ] **Step 3: Run the host-path regression and complete suite.**

Run:

```bash
cabal test --project-dir=jazz-next runtime-semantics-spec --test-show-details=failures
```

Expected: PASS, including 20,000 host-path calls, one `WriteStdoutCall
"before"`, selected-branch ordering, hostful mutual recursion, and dynamic
scope cache separation.

- [ ] **Step 4: Commit host-path parity.**

```bash
git add jazz-next/src/JazzNext/Compiler/Runtime.hs jazz-next/test/JazzNext/Compiler/Semantics/Runtime/HostIOTests.hs
git commit -m "fix: preserve host ordering in tail evaluation"
```

### Task 5: Prove imported closure transfer and diagnostic parity

**Files:**

- Modify: `jazz-next/src/JazzNext/Compiler/Runtime.hs` only for defects exposed
  by module-path or diagnostic parity tests
- Modify: `jazz-next/test/JazzNext/Compiler/Semantics/Runtime/RecursionTests.hs`
- Test: `jazz-next/test/JazzNext/Compiler/Modules/LoaderSpec.hs`

**Interfaces:**

- Consumes: `VClosure`'s captured environment and `Maybe [Text]` module path,
  the real `ModuleRuntime` import/export pipeline, and existing runtime
  diagnostics.
- Produces: 20,000-call imported closure proof and representative pure/host
  diagnostic equivalence.

- [ ] **Step 1: Run the imported-module depth test.**

Run:

```bash
cabal test --project-dir=jazz-next loader-spec --test-show-details=failures
```

Expected: PASS with output `0`. If it fails, fix closure transfer by using the
`VClosure`'s captured module path and environment in `EvaluationContext`; do
not modify `ModuleRuntime` or copy imports into the caller environment.

- [ ] **Step 2: Add pure/host diagnostic parity coverage.**

In `RecursionTests.hs`, evaluate representative failing expressions through
both `evaluateRuntimeExpr` and `evaluateRuntimeExprWithHost`, and compare
`renderDiagnostic` with this deterministic host and exact test shape:

```haskell
diagnosticParityExpressions :: [Expr]
diagnosticParityExpressions =
  [ EVar "missing",
    EIf (ELit (LInt 1)) (ELit (LInt 2)) (ELit (LInt 3)),
    EApply (ELit (LInt 1)) (ELit (LInt 2)),
    EPatternCase (ELit (LInt 1)) []
  ]

diagnosticParityHost :: RuntimeHost Identity
diagnosticParityHost =
  RuntimeHost
    { runtimeHostReadText = \_ -> pure (Right ""),
      runtimeHostWriteText = \_ _ -> pure (Right ()),
      runtimeHostReadStdin = pure (Right ""),
      runtimeHostWriteStdout = \_ -> pure (Right ()),
      runtimeHostWriteStderr = \_ -> pure (Right ()),
      runtimeHostArguments = pure [],
      runtimeHostExit = \_ -> pure (Right ())
    }

testPureAndHostDiagnosticsMatch :: IO ()
testPureAndHostDiagnosticsMatch =
  mapM_ assertParity diagnosticParityExpressions
  where
    assertParity expression =
      case
          ( evaluateRuntimeExpr expression,
            runIdentity (evaluateRuntimeExprWithHost diagnosticParityHost expression)
          )
        of
          (Left pureDiagnostic, Left hostDiagnostic) ->
            assertEqual
              "pure/host rendered diagnostic"
              (renderDiagnostic pureDiagnostic)
              (renderDiagnostic hostDiagnostic)
          (pureResult, hostResult) ->
            failTest
              ( "expected matching diagnostic failures, found "
                  <> Text.pack (show pureResult)
                  <> " and "
                  <> Text.pack (show hostResult)
              )
```

Add `Identity`, `runIdentity`, `RuntimeHost (..)`,
`evaluateRuntimeExprWithHost`, and the required AST constructors to the current
imports. The test asserts exact rendered diagnostics, not only error codes, and
performs no ambient I/O.

- [ ] **Step 3: Run focused runtime and loader verification.**

Run:

```bash
cabal test --project-dir=jazz-next runtime-semantics-spec loader-spec --test-show-details=failures
```

Expected: PASS with all three regression floors and exact diagnostic parity.

- [ ] **Step 4: Commit module and diagnostic evidence.**

```bash
git add jazz-next/src/JazzNext/Compiler/Runtime.hs jazz-next/test/JazzNext/Compiler/Semantics/Runtime/RecursionTests.hs jazz-next/test/JazzNext/Compiler/Modules/LoaderSpec.hs
git commit -m "test: prove stack-safe module tail calls"
```

### Task 6: Remove evaluator duplication and close the queue child

**Files:**

- Modify: `jazz-next/src/JazzNext/Compiler/Runtime.hs`
- Modify: `docs/superpowers/plans/2026-07-11-jazz-next-stack-safe-evaluation.md`
- Modify: `docs/superpowers/specs/2026-07-10-jazz-next-bootstrap-interpreter-profile-design.md`
- Modify: `docs/superpowers/specs/2026-07-11-jazz-next-stack-safe-evaluation-design.md`
- Modify: `docs/execution/blocker-contracts.md`
- Modify: `docs/execution/queue.md`
- Modify: `docs/execution/done-archive.md`

**Interfaces:**

- Consumes: all focused GREEN evidence and the queue child-lifecycle contract.
- Produces: one expression evaluator, completed plan metadata, archived closure
  evidence, and the bootstrap umbrella's next evidence-backed curation state.

- [ ] **Step 1: Run duplication and boundary searches.**

Run:

```bash
rg -n "evalValueWithModulePath|evalValueWithHost|applyRuntimeFunctionWithHost|applyRuntimeFunction" jazz-next/src/JazzNext/Compiler/Runtime.hs
rg -n "Evaluation(Control|Continuation|Machine)|RuntimeResultObligation" jazz-next/src/JazzNext/Compiler/ModuleRuntime.hs jazz-next/src/JazzNext/Compiler/Driver.hs
rg -n "bytecode|opcode|LLVM" jazz-next/src/JazzNext/Compiler/Runtime.hs
```

Expected:

- pure and host names remain only as stable wrappers or non-recursive primitive
  helpers;
- no evaluator machine type appears in `ModuleRuntime.hs` or `Driver.hs`; and
- no bytecode/opcode/LLVM execution concept appears in `Runtime.hs`.

Delete dead recursive evaluator branches and imports revealed by the searches.

- [ ] **Step 2: Run formatter and focused verification.**

Run:

```bash
cabal format jazz-next/jazz-next.cabal
cabal test --project-dir=jazz-next runtime-semantics-spec loader-spec --test-show-details=failures
```

Expected: PASS. Revert any no-op Cabal formatting churn if the Cabal file did
not require a semantic edit.

- [ ] **Step 3: Run the full repository verification gate.**

Run:

```bash
bash jazz-next/scripts/test-warning-config.sh
bash scripts/check-execution-queue.sh
bash scripts/check-docs.sh
git diff --check
```

Expected: all commands PASS. `check-docs.sh` may report the known warning that
Prettier was found outside the Nix shell; that warning is non-fatal.

- [ ] **Step 4: Record closeout metadata.**

After GREEN evidence:

- set this plan to `status: done` with `completed_on: 2026-07-11` (or the
  actual completion date) and check every completed step;
- mark the detailed design `Status: Implemented` and add exact verification
  evidence;
- update the parent bootstrap profile so stack-safe evaluation is complete;
- move `JN-BOOTSTRAP-STACK-SAFE-EVALUATION-001` from `Ready Now` to
  `done-archive.md` with the three depth results, host-ordering proof,
  diagnostic parity, and LLVM-boundary statement;
- update the bootstrap blocker to the next smallest source-backed child, or
  explicitly state that no candidate is promoted if evidence is insufficient;
  and
- keep `Ready Now` and `Next Curation Target` consistent with that decision.

- [ ] **Step 5: Commit closeout.**

```bash
git add docs jazz-next/src/JazzNext/Compiler/Runtime.hs
git commit -m "docs: close stack-safe evaluation batch"
```

## Execution Notes

- RED evidence: not run yet.
- GREEN evidence: not run yet.
- Full verification: not run yet.
