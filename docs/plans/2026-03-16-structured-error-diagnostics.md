# Structured Error Diagnostics Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Replace raw compile-time `Text` errors in `jazz-next` with structured error records, and render them to `Text` only at the outer result/CLI boundary.

**Architecture:** Introduce a small `ErrorRecord` type in `jazz-next/src/JazzNext/Compiler/Diagnostics.hs` rather than a fully unified warning/error sum type. Thread `[ErrorRecord]` through analyzer, type inference, prelude validation, module resolution, and driver plumbing, then centralize rendering in diagnostics/CLI helpers so semantic code stops constructing user-facing strings inline.

**Tech Stack:** Haskell (`runghc` test entrypoints), `jazz-next` compiler modules, existing test harness in `jazz-next/test/JazzNext/TestHarness.hs`.

Execution note:
- All implementation work for this plan must stay under `jazz-next/`.
- `jazz-hs/` and `jazz2/` remain read-only reference code.
- Runtime errors (`runRuntimeErrors`) are out of scope for this plan. This plan only covers compile-time diagnostics.

---

## Scope Guardrails

In scope:
- structured compile-time error payloads for analyzer, type inference, prelude validation, module resolution, parse/lower failure wrapping, and driver compile/run compile-error output,
- stable diagnostic rendering helpers,
- tests updated to assert on structured error data or rendered output through shared helpers,
- preserving existing user-visible error text unless a test-backed improvement is required.

Out of scope:
- unifying warnings and errors into a single `Diagnostic` sum type,
- changing diagnostic codes/messages unless necessary to preserve existing behavior,
- refactoring runtime error plumbing,
- changing language semantics.

## Design Lock

- Add `ErrorRecord` alongside `WarningRecord`, not instead of it.
- `ErrorRecord` must carry enough structure to avoid message construction in semantic code:
  - stable error code,
  - primary message text,
  - optional primary source span,
  - optional related source span,
  - optional subject/symbol text.
- Rendering happens through diagnostics helpers such as `renderErrorRecord`.
- Compiler-internal phases should return `[ErrorRecord]`; CLI-facing results may still expose rendered `Text` only if changing the public surface would create unnecessary churn.
- If a boundary cannot be converted in one step, wrap legacy `Text` immediately into an `ErrorRecord` and note the remaining producer in a follow-up comment or task note before proceeding.

## Exact File Targets

Core diagnostics:
- Modify: `jazz-next/src/JazzNext/Compiler/Diagnostics.hs`
- Modify: `jazz-next/test/JazzNext/TestHarness.hs`

Semantic producers:
- Modify: `jazz-next/src/JazzNext/Compiler/Analyzer.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/TypeInference.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/PreludeContract.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/ModuleResolver.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Driver.hs`

Possible CLI/result boundary:
- Modify: `jazz-next/src/JazzNext/CLI/Main.hs`

Tests:
- Modify: `jazz-next/test/PuritySemanticsSpec.hs`
- Modify: `jazz-next/test/BindingSignatureCoherenceSpec.hs`
- Modify: `jazz-next/test/IfExpressionTypeSpec.hs`
- Modify: `jazz-next/test/PreludeLoadingSpec.hs`
- Modify: `jazz-next/test/ModuleResolutionSpec.hs`
- Modify: `jazz-next/test/CLISpec.hs`
- Create: `jazz-next/test/StructuredErrorDiagnosticsSpec.hs`

## Task 1: Add Structured Error Foundations

**Files:**
- Modify: `jazz-next/src/JazzNext/Compiler/Diagnostics.hs`
- Modify: `jazz-next/test/JazzNext/TestHarness.hs`
- Create: `jazz-next/test/StructuredErrorDiagnosticsSpec.hs`

**Step 1: Write the failing diagnostics foundation test**

Add a new spec that locks the `ErrorRecord` render contract. Start with a narrow rendering test instead of changing semantic modules first.

```haskell
testRenderErrorRecordWithPrimaryAndRelatedSpans :: IO ()
testRenderErrorRecordWithPrimaryAndRelatedSpans = do
  let rendered =
        renderErrorRecord $
          ErrorRecord
            { errorCodeText = "E1010",
              errorMessage = "binding 'x' cannot call impure callee 'print!'",
              errorPrimarySpan = Just (SourceSpan 2 1),
              errorRelatedSpan = Just (SourceSpan 1 1),
              errorSubject = Just "x"
            }
  assertContains "rendered error code" "E1010" rendered
  assertContains "rendered primary span" "2:1" rendered
  assertContains "rendered related span" "1:1" rendered
```

**Step 2: Run the new test to verify it fails**

Run:

```bash
runghc -i./jazz-next/src -i./jazz-next/test jazz-next/test/StructuredErrorDiagnosticsSpec.hs
```

Expected: FAIL because `ErrorRecord` and `renderErrorRecord` do not exist yet.

**Step 3: Implement the minimal diagnostics structure**

Add an `ErrorRecord` to `Diagnostics.hs` and export a renderer plus small constructors/helpers.

```haskell
data ErrorRecord = ErrorRecord
  { errorCodeText :: Text,
    errorMessage :: Text,
    errorPrimarySpan :: Maybe SourceSpan,
    errorRelatedSpan :: Maybe SourceSpan,
    errorSubject :: Maybe Text
  }
  deriving (Eq, Show)

renderErrorRecord :: ErrorRecord -> Text
renderErrorRecord err =
  errorCodeText err
    <> ": "
    <> errorMessage err
    <> renderPrimary (errorPrimarySpan err)
    <> renderRelated (errorRelatedSpan err)
```

Also add harness helpers that avoid repeating render logic across specs:

```haskell
assertSingleErrorCode :: Text -> Text -> [ErrorRecord] -> IO ()
assertRenderedErrorContains :: Text -> Text -> ErrorRecord -> IO ()
```

**Step 4: Run the new test to verify it passes**

Run:

```bash
runghc -i./jazz-next/src -i./jazz-next/test jazz-next/test/StructuredErrorDiagnosticsSpec.hs
```

Expected: PASS.

**Step 5: Commit**

```bash
git add jazz-next/src/JazzNext/Compiler/Diagnostics.hs \
  jazz-next/test/JazzNext/TestHarness.hs \
  jazz-next/test/StructuredErrorDiagnosticsSpec.hs
git commit -m "feat(jazz-next): add structured compile error records"
```

## Task 2: Migrate Analyzer Semantic Errors Off Raw Text

**Files:**
- Modify: `jazz-next/src/JazzNext/Compiler/Analyzer.hs`
- Modify: `jazz-next/test/PuritySemanticsSpec.hs`
- Modify: `jazz-next/test/BindingSignatureCoherenceSpec.hs`

**Step 1: Write failing analyzer-facing tests**

Update existing analyzer-driven specs to assert on structured errors instead of raw rendered `Text`.

Example target changes:

```haskell
assertSingleErrorCode
  "pure binding calling impure builtin"
  "E1010"
  (compileErrors result)
```

For a tighter regression, add one explicit analyzer error-shape test:

```haskell
assertRight "primary span present" (firstErrorPrimarySpan result) $ \spanValue ->
  assertEqual "impure call span" (SourceSpan 2 1) spanValue
```

**Step 2: Run the affected tests to verify they fail**

Run:

```bash
runghc -i./jazz-next/src -i./jazz-next/test jazz-next/test/PuritySemanticsSpec.hs
runghc -i./jazz-next/src -i./jazz-next/test jazz-next/test/BindingSignatureCoherenceSpec.hs
```

Expected: FAIL because `Analyzer` still returns `[Text]`.

**Step 3: Implement minimal analyzer conversion**

Change analyzer result plumbing from `[Text]` to `[ErrorRecord]`.

Key edits:
- `analysisErrors :: [ErrorRecord]`
- `collectExprDiagnostics ... -> ([WarningRecord], [ErrorRecord])`
- convert `mkUnboundVariableError`, `mkMissingBindingForSignatureError`, `mkMismatchedSignatureError`, and `mkImpureCallInPureContextError` to build `ErrorRecord`

Use constructors like:

```haskell
mkImpureCallInPureContextError :: AnalysisContext -> Identifier -> Maybe SourceSpan -> ErrorRecord
mkImpureCallInPureContextError context calleeName maybeCalleeSpan =
  ErrorRecord
    { errorCodeText = "E1010",
      errorMessage =
        contextLabel context
          <> " cannot call impure callee '"
          <> identifierText calleeName
          <> "'",
      errorPrimarySpan = Nothing,
      errorRelatedSpan = maybeCalleeSpan,
      errorSubject = Just (identifierText calleeName)
    }
```

Do not over-design this step. Reuse existing message text.

**Step 4: Run the affected tests to verify they pass**

Run:

```bash
runghc -i./jazz-next/src -i./jazz-next/test jazz-next/test/PuritySemanticsSpec.hs
runghc -i./jazz-next/src -i./jazz-next/test jazz-next/test/BindingSignatureCoherenceSpec.hs
```

Expected: PASS.

**Step 5: Commit**

```bash
git add jazz-next/src/JazzNext/Compiler/Analyzer.hs \
  jazz-next/test/PuritySemanticsSpec.hs \
  jazz-next/test/BindingSignatureCoherenceSpec.hs
git commit -m "refactor(jazz-next): emit structured analyzer errors"
```

## Task 3: Migrate Type Inference and Driver Compile Plumbing

**Files:**
- Modify: `jazz-next/src/JazzNext/Compiler/TypeInference.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Driver.hs`
- Modify: `jazz-next/test/IfExpressionTypeSpec.hs`
- Modify: `jazz-next/test/CLISpec.hs`

**Step 1: Write failing tests for typed compile errors**

Update existing type-error assertions to use the new helpers, and add one driver-level rendering test if the driver still exposes rendered text outward.

Example:

```haskell
assertSingleErrorCode
  "if condition type error"
  "E2001"
  (compileErrors result)
```

If `CompileResult` still exposes `[Text]`, add a lower-level `InferenceResult` assertion first, then decide whether to switch `CompileResult` to `[ErrorRecord]` or add `renderCompileErrors`.

**Step 2: Run the affected tests to verify they fail**

Run:

```bash
runghc -i./jazz-next/src -i./jazz-next/test jazz-next/test/IfExpressionTypeSpec.hs
runghc -i./jazz-next/src -i./jazz-next/test jazz-next/test/CLISpec.hs
```

Expected: FAIL because `TypeInference` and `Driver` still expect `[Text]`.

**Step 3: Implement minimal compile-error threading**

Change:

```haskell
data InferenceResult = InferenceResult
  { inferredExpr :: Expr,
    inferredWarnings :: [WarningRecord],
    inferredErrors :: [ErrorRecord]
  }
```

Convert `mkBinaryTypeError`, `mkStrictEqualityTypeError`, `mkApplyTypeError`, and related constructors to return `ErrorRecord`.

In `Driver.hs`, choose one of these and keep it consistent:
- preferred: `CompileResult.compileErrors :: [ErrorRecord]` and `RunResult.runCompileErrors :: [ErrorRecord]`
- acceptable transitional boundary: keep result fields as `[Text]`, but only render by calling `map renderErrorRecord` in `Driver.hs`

Recommendation: use `[ErrorRecord]` in result types and render in CLI/tests when needed. This is the cleanest endpoint and avoids another intermediate conversion layer.

**Step 4: Run the affected tests to verify they pass**

Run:

```bash
runghc -i./jazz-next/src -i./jazz-next/test jazz-next/test/IfExpressionTypeSpec.hs
runghc -i./jazz-next/src -i./jazz-next/test jazz-next/test/CLISpec.hs
```

Expected: PASS.

**Step 5: Commit**

```bash
git add jazz-next/src/JazzNext/Compiler/TypeInference.hs \
  jazz-next/src/JazzNext/Compiler/Driver.hs \
  jazz-next/test/IfExpressionTypeSpec.hs \
  jazz-next/test/CLISpec.hs
git commit -m "refactor(jazz-next): thread structured compile errors through inference and driver"
```

## Task 4: Migrate Remaining Compile-Time Producers

**Files:**
- Modify: `jazz-next/src/JazzNext/Compiler/PreludeContract.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/ModuleResolver.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Driver.hs`
- Modify: `jazz-next/test/PreludeLoadingSpec.hs`
- Modify: `jazz-next/test/ModuleResolutionSpec.hs`
- Modify: `jazz-next/test/CLISpec.hs`

**Step 1: Write failing tests for prelude/resolution diagnostics**

Convert existing compile-time tests to assert structured error codes and, where useful, rendered text through shared helpers.

Example:

```haskell
assertSingleErrorCode
  "unknown prelude kernel bridge symbol"
  "E0004"
  (compileErrors result)
```

and:

```haskell
assertSingleErrorCode
  "missing import symbol"
  "E4007"
  (compileErrors result)
```

**Step 2: Run the affected tests to verify they fail**

Run:

```bash
runghc -i./jazz-next/src -i./jazz-next/test jazz-next/test/PreludeLoadingSpec.hs
runghc -i./jazz-next/src -i./jazz-next/test jazz-next/test/ModuleResolutionSpec.hs
runghc -i./jazz-next/src -i./jazz-next/test jazz-next/test/CLISpec.hs
```

Expected: FAIL because these modules still emit `Text`.

**Step 3: Implement minimal producer conversion**

Convert `PreludeContract` and `ModuleResolver` helper constructors from `Text` to `ErrorRecord`.

For parse/prelude-load boundaries in `Driver.hs`, wrap parse/load failures immediately:

```haskell
mkParseErrorRecord :: Text -> ErrorRecord
mkParseErrorRecord parseError =
  ErrorRecord
    { errorCodeText = "E0001",
      errorMessage = "parse error: " <> parseError,
      errorPrimarySpan = Nothing,
      errorRelatedSpan = Nothing,
      errorSubject = Nothing
    }
```

Do not leave any compile-time producer returning naked `Text` after this task.

**Step 4: Run the affected tests to verify they pass**

Run:

```bash
runghc -i./jazz-next/src -i./jazz-next/test jazz-next/test/PreludeLoadingSpec.hs
runghc -i./jazz-next/src -i./jazz-next/test jazz-next/test/ModuleResolutionSpec.hs
runghc -i./jazz-next/src -i./jazz-next/test jazz-next/test/CLISpec.hs
```

Expected: PASS.

**Step 5: Commit**

```bash
git add jazz-next/src/JazzNext/Compiler/PreludeContract.hs \
  jazz-next/src/JazzNext/Compiler/ModuleResolver.hs \
  jazz-next/src/JazzNext/Compiler/Driver.hs \
  jazz-next/test/PreludeLoadingSpec.hs \
  jazz-next/test/ModuleResolutionSpec.hs \
  jazz-next/test/CLISpec.hs
git commit -m "refactor(jazz-next): migrate remaining compile diagnostics to structured errors"
```

## Task 5: Full Verification and Cleanup

**Files:**
- Modify: `jazz-next/scripts/test-warning-config.sh` (only if new test file must be added)
- Modify: `docs/plans/2026-03-16-structured-error-diagnostics.md`

**Step 1: Add the new diagnostics spec to the test script if needed**

If `StructuredErrorDiagnosticsSpec.hs` is not already covered by the shell runner, add it in the correct order near other diagnostics/compiler tests.

**Step 2: Run the full suite**

Run:

```bash
bash jazz-next/scripts/test-warning-config.sh
```

Expected: PASS for the full `jazz-next` suite.

**Step 3: Inspect for leftover raw compile-time error constructors**

Run:

```bash
rg -n "mk.*Error :: .*Text|analysisErrors :: \\[Text\\]|inferredErrors :: \\[Text\\]|compileErrors :: \\[Text\\]|runCompileErrors :: \\[Text\\]" jazz-next/src/JazzNext/Compiler
```

Expected: no compile-time diagnostics still modeled as raw `[Text]`. Runtime errors may still appear and are acceptable.

**Step 4: Commit the last integration cleanup**

```bash
git add jazz-next/scripts/test-warning-config.sh \
  docs/plans/2026-03-16-structured-error-diagnostics.md
git commit -m "test(jazz-next): cover structured compile error diagnostics"
```

## Final Verification Checklist

- `ErrorRecord` exists and is exported from `Diagnostics.hs`.
- `Analyzer`, `TypeInference`, `PreludeContract`, `ModuleResolver`, and compile-time `Driver` paths no longer construct user-facing diagnostic strings inline.
- All compile-time diagnostic rendering is centralized.
- Existing error codes remain stable.
- `bash jazz-next/scripts/test-warning-config.sh` passes.
- `rg` confirms no compile-time `[Text]` error lists remain.

