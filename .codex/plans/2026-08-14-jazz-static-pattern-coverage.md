---
id: JN-PATTERN-COVERAGE-ANALYSIS-001
status: ready
priority: P1
size: L
kind: impl
autonomous_ready: yes
depends_on: []
plan_section: "Full closeout"
target_paths:
  - src/Jazz/Compiler/PatternCoverage.hs
  - src/Jazz/Compiler/TypeInference.hs
  - src/Jazz/Compiler/TypeInference/State.hs
  - src/Jazz/Compiler/TypeInference/Diagnostics.hs
  - src/Jazz/Compiler/DiagnosticCatalog.hs
  - src/Jazz/Compiler/BundledPrelude.hs
  - jazz/stdlib/Prelude.jz
  - jazz.cabal
  - test/Jazz/Compiler/Semantics/PatternCoverageSpec.hs
  - test/Jazz/Compiler/Semantics/AdtPatternTypeSpec.hs
  - test/Jazz/Compiler/Semantics/AdtPatternRuntimeSpec.hs
  - test/Jazz/Compiler/Semantics/LambdaSemanticsSpec.hs
  - test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures.hs
  - test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec.hs
  - test/Jazz/Compiler/Diagnostics/DiagnosticCatalogSpec.hs
  - docs/language/control-flow.md
  - docs/language/algebraic-data-types-and-patterns.md
  - docs/reference/diagnostics.md
  - docs/project/status.md
  - rfcs/accepted/0012-static-pattern-coverage.md
  - .codex/execution/queue.md
  - .codex/execution/blocker-contracts.md
verification:
  - nix --extra-experimental-features 'nix-command flakes' develop --command cabal test pattern-coverage-spec pattern-semantics-spec adt-pattern-type-spec adt-pattern-runtime-spec lambda-semantics-spec diagnostic-catalog-spec module-pipeline-contract-spec --test-show-details=direct --jobs=1
  - nix --extra-experimental-features 'nix-command flakes' develop --command cabal build all
  - nix --extra-experimental-features 'nix-command flakes' develop --command cabal test all --test-show-details=direct --jobs=1
  - bash scripts/check-execution-queue.sh
  - bash scripts/check-docs.sh
  - git diff --check
deliverable: "Reject non-exhaustive pattern matches with E2018 and wholly unreachable arms with E2019 across the complete active pattern surface using one resolved-type usefulness analysis."
last_verified: 2026-08-14
---

# Jazz Static Pattern Coverage Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use
> superpowers:subagent-driven-development (recommended) or
> superpowers:executing-plans to implement this plan task-by-task. Steps use
> checkbox (`- [ ]`) syntax for tracking.

**Goal:** Reject non-exhaustive cases and pattern lambdas plus wholly
unreachable arms across every active Jazz pattern form.

**Architecture:** The inference traversal records immutable pattern-match
observations while preserving its single-pass constraint work. After final
substitution, one pure usefulness-matrix engine consumes resolved scrutinee
types and the final constructor inventory. Type inference materializes strict
`E2018` and `E2019` diagnostics only when earlier compilation phases are clean;
runtime selection and `E3022` remain unchanged.

**Tech Stack:** Haskell, GHC 9.14.1, Cabal, `containers`, canonical Jazz core,
resolved type inference, Nix

## Global Constraints

- Implement only accepted RFC 0012 and
  `JN-PATTERN-COVERAGE-ANALYSIS-001`.
- Analyze every canonical `EPatternCase`; surface cases and pattern lambdas must
  share one coverage path.
- Support wildcard, variable, integral/`Bool`/`Char`/`Text` literal,
  constructor, exact-list, cons-list, tuple, as-, and top-level or-patterns.
- Treat `Bool`, unit, lists, tuples, and declared ADTs as closed constructor
  spaces. Treat integral, numeric-width, `Char`, `Text`, and other literal
  domains as open unless an irrefutable pattern covers them.
- Treat only unguarded arms as coverage evidence. Never constant-fold guards.
- Emit hard `E2018` for a non-exhaustive match with one deterministic witness.
- Emit hard `E2019` for a wholly unreachable one-based arm index.
- Consider an or-pattern arm reachable when any alternative is useful. Do not
  diagnose partial redundancy inside a useful or-pattern.
- Suppress coverage diagnostics whenever existing analyzer or type inference
  diagnostics contain an effective error.
- Record inference observations once; do not rerun inference, inspect runtime
  values, or make coverage part of unification.
- Preserve stable traversal ordering, arm ordering, qualified constructor
  ordering, and deterministic missing-pattern rendering.
- Keep the interpreter and runtime `E3022` unchanged as defensive boundaries.
- Do not change Typed Core, Lowered IR, managed-value lowering, pattern-lambda
  backend ABI, module export behavior, or runtime selection.
- Run Cabal only inside the checked-in Nix development shell with `--jobs=1`
  for tests.
- Commit each green milestone with the commit message named below.

---

### Task 1: Promote the RFC 0012 implementation child

**Files:**

- Create: `.codex/plans/2026-08-14-jazz-static-pattern-coverage.md`
- Modify: `.codex/execution/queue.md`
- Modify: `.codex/execution/blocker-contracts.md`

**Interfaces:**

- Consume: accepted RFC 0012 and
  `.codex/plans/2026-08-14-jazz-static-pattern-coverage-design.md`.
- Produce: one `P1`, size `L`, autonomous `Ready Now` row whose target and
  verification lists exactly match this plan's frontmatter.

- [x] **Step 1: Record the approved design and durable decision.** The design,
      RFC 0012, and curation candidate were committed as `e7e974b2`.

- [x] **Step 2: Create this active implementation plan.** Copy the accepted
      semantics into global constraints, name exact files and commands, and
      keep `.codex/plans/` as the sole active plan tree.

- [x] **Step 3: Promote the child.** Move
      `JN-PATTERN-COVERAGE-ANALYSIS-001` from `Next Curation Target` to
      `Ready Now`, link this plan at `Full closeout`, and mark the bootstrap
      umbrella as executing the accepted child.

- [x] **Step 4: Validate and commit the promotion.**

Run:

```bash
bash scripts/check-execution-queue.sh
git diff --check
```

Expected: both commands succeed.

Commit:

```bash
git add .codex/plans/2026-08-14-jazz-static-pattern-coverage.md .codex/execution/queue.md .codex/execution/blocker-contracts.md
git commit -m "docs: ready static pattern coverage"
```

### Task 2: Implement the pure coverage engine

**Files:**

- Create: `src/Jazz/Compiler/PatternCoverage.hs`
- Create: `test/Jazz/Compiler/Semantics/PatternCoverageSpec.hs`
- Modify: `jazz.cabal`

**Interfaces:**

- Consume: `Pattern`, `CaseArm`, `ExpressionType`, `DataTypeBinding`, and
  `ConstructorArgumentType` from the active compiler.
- Produce:

```haskell
data PatternCoverageFailure
  = NonExhaustivePattern Pattern
  | UnreachablePatternArm Int

constructorInventoryFromBindings ::
  Map Text DataTypeBinding -> TypeEnv -> ConstructorInventory

analyzePatternCoverage ::
  ConstructorInventory ->
  ExpressionType ->
  [CaseArm] ->
  [PatternCoverageFailure]

renderCoveragePattern :: Pattern -> Text
```

- [x] **Step 1: Add the failing scalar and closed-domain tests.** Register a
      `pattern-coverage-spec` stanza in `jazz.cabal` and add exact assertions:

```haskell
tests :: [NamedTest]
tests =
  [ ("empty Bool match misses False", assertCoverage TBoolType [] [NonExhaustivePattern (PLiteral (LBool False))]),
    ("Bool constructors are exhaustive", assertCoverage TBoolType [arm (PLiteral (LBool False)), arm (PLiteral (LBool True))] []),
    ("duplicate Bool arm is unreachable", assertCoverage TBoolType [arm (PLiteral (LBool False)), arm (PLiteral (LBool False)), arm PWildcard] [UnreachablePatternArm 2]),
    ("open integer literals need a fallback", assertCoverage TIntType [arm (PLiteral (LInt 0))] [NonExhaustivePattern PWildcard]),
    ("unguarded wildcard makes later arm unreachable", assertCoverage TIntType [arm PWildcard, arm (PLiteral (LInt 1))] [UnreachablePatternArm 2])
  ]
```

- [x] **Step 2: Run the new suite and prove the engine is absent.**

Run:

```bash
nix --extra-experimental-features 'nix-command flakes' develop --command cabal test pattern-coverage-spec --test-show-details=direct --jobs=1
```

Expected: FAIL because `Jazz.Compiler.PatternCoverage` or its exported
functions do not exist.

- [x] **Step 3: Add the public pure contract and internal normalized model.**
      Define the exported failure type and functions above. Keep these internal:

```haskell
data CoveragePattern
  = CoverageWildcard
  | CoverageConstructor CoverageConstructor [CoveragePattern]

data CoverageConstructor
  = BoolConstructor Bool
  | UnitConstructor
  | ListNilConstructor
  | ListConsConstructor
  | TupleConstructor Int
  | DataConstructorName Name
  | LiteralConstructor Literal

data ConstructorShape = ConstructorShape
  { constructorShapeTag :: CoverageConstructor,
    constructorShapeFieldTypes :: [ExpressionType],
    constructorShapePattern :: [CoveragePattern] -> Pattern
  }
```

Normalize variables/wildcards to `CoverageWildcard`, unwrap `PAs`, expand
`POr`, rewrite exact lists to cons/nil, preserve tuple arity, and preserve ADT
constructor identity.

- [x] **Step 4: Implement usefulness and witness search.** Use one recursive
      matrix algorithm:

```haskell
useful :: ConstructorInventory -> [ExpressionType] -> [[CoveragePattern]] -> [CoveragePattern] -> Maybe [CoveragePattern]
useful inventory columnTypes matrix vector =
  case (columnTypes, vector) of
    ([], []) -> if null matrix then Just [] else Nothing
    (columnType : remainingTypes, CoverageConstructor tag fields : remainingPatterns) ->
      useful inventory (fieldTypes inventory columnType tag <> remainingTypes)
        (specializeMatrix tag matrix) (fields <> remainingPatterns)
    (columnType : remainingTypes, CoverageWildcard : remainingPatterns) ->
      if constructorsAreComplete inventory columnType (headConstructorSet matrix)
        then firstUsefulConstructor inventory columnType matrix remainingTypes remainingPatterns
        else (CoverageWildcard :) <$> useful inventory remainingTypes (defaultMatrix matrix) remainingPatterns
    _ -> Nothing
```

For reachability, query each expanded arm alternative against preceding
unguarded rows and report `UnreachablePatternArm` only when every alternative
is useless. Add expanded rows only for an unguarded arm. For exhaustiveness,
query one wildcard after all arms and convert the returned witness vector back
to one Jazz `Pattern`.

- [x] **Step 5: Run scalar tests to green.** Run the Task 2 Step 2 command.

Expected: PASS.

- [x] **Step 6: Add failing structural tests.** Cover exact expected failures
      for unit, tuple products, empty/cons/exact lists, generic and recursive
      ADTs, as-patterns, whole and partial or-pattern redundancy, and guards:

```haskell
guarded patternValue = CaseArm patternValue (Just (ELit (LBool True))) (ELit (LInt 0))

assertCoverage TBoolType
  [guarded (PLiteral (LBool False)), arm (PLiteral (LBool True))]
  [NonExhaustivePattern (PLiteral (LBool False))]

assertCoverage maybeIntType
  [arm (PConstructor "Nothing" []), arm (PConstructor "Just" [PWildcard])]
  []

assertCoverage (TListType TIntType)
  [arm (PList []), arm (PConsList PWildcard PWildcard)]
  []
```

- [x] **Step 7: Implement constructor inventories and deterministic rendering.**
      Combine `DataTypeBinding` constructor counts with the lexical `TypeEnv`.
      Treat an ADT as closed only when all constructor bindings are visible;
      otherwise require an irrefutable fallback. Instantiate visible field types
      from the constructor bindings and resolved type arguments. Preserve stable
      qualified-name order. Render witnesses as
      valid Jazz pattern syntax: `_`, `False`, `True`, `()`, `[]`,
      `[head | tail]`, `(left, right)`, and `Constructor field` with parentheses
      only where nesting requires them.

- [x] **Step 8: Run the full pure suite and commit.**

Run:

```bash
nix --extra-experimental-features 'nix-command flakes' develop --command cabal test pattern-coverage-spec pattern-semantics-spec --test-show-details=direct --jobs=1
```

Expected: PASS.

Commit:

```bash
git add jazz.cabal src/Jazz/Compiler/PatternCoverage.hs test/Jazz/Compiler/Semantics/PatternCoverageSpec.hs
git commit -m "feat: analyze pattern coverage"
```

### Task 3: Retain and resolve match observations during inference

**Files:**

- Modify: `src/Jazz/Compiler/PatternCoverage.hs`
- Modify: `src/Jazz/Compiler/TypeInference/State.hs`
- Modify: `src/Jazz/Compiler/TypeInference.hs`
- Modify: `test/Jazz/Compiler/Semantics/PatternCoverageSpec.hs`

**Interfaces:**

- Consume: `analyzePatternCoverage` from Task 2 and final `InferState`
  substitution/data-type inventory.
- Produce:

```haskell
data PatternCoverageSite = PatternCoverageSite
  { patternCoverageSiteOrdinal :: Int,
    patternCoverageSiteConstructorInventory :: ConstructorInventory,
    patternCoverageSiteScrutineeType :: ExpressionType,
    patternCoverageSiteArms :: [CaseArm]
  }

reservePatternCoverageSite :: InferState -> (Int, InferState)
recordPatternCoverageSite :: PatternCoverageSite -> InferState -> InferState
inferPatternCoverageSites :: InferState -> [PatternCoverageSite]
```

- [x] **Step 1: Add failing integration tests.** Compile source rather than
      calling the pure engine:

```haskell
testCompleteBoolCaseCompiles = do
  result <- compileSource defaultWarningSettings "x = case True { | False -> 0 | True -> 1 }."
  assertEqual "complete Bool diagnostics" [] (compileErrors result)

testNestedSitesAreReportedOnce = do
  result <- compileSource defaultWarningSettings "x = case True { | True -> case False { | False -> 0 } }."
  assertEqual "nested coverage codes" ["E2018", "E2018"] (map diagnosticCodeTextFromDiagnostic (compileErrors result))
```

Also add an invalid-pattern case that asserts only existing `E2011`, proving
coverage suppression.

- [x] **Step 2: Run the suite and verify source compilation still accepts
      incomplete matches.** Run the Task 2 Step 2 command.

Expected: FAIL because no pipeline coverage diagnostics exist.

- [x] **Step 3: Extend `InferenceOutput`.** Add:

```haskell
outputPatternCoverageSites :: Seq PatternCoverageSite
outputNextPatternCoverageOrdinal :: Int
```

Initialize them to `Seq.empty` and `0`. `reservePatternCoverageSite` returns the
current ordinal and increments only the counter. `recordPatternCoverageSite`
appends exactly one site. `inferPatternCoverageSites` materializes the sequence
without exposing mutable state.

- [x] **Step 4: Record every canonical match once.** In both
      `EPatternCase` traversal branches, reserve the ordinal before visiting the
      scrutinee, thread the reserved state into child inference, and append the
      site after arm inference:

```haskell
let (coverageOrdinal, stateWithCoverageOrdinal) = reservePatternCoverageSite state
    (scrutineeResult, stateAfterScrutinee) =
      inferExprTypeDetailed builtinMode env stateWithCoverageOrdinal scrutineeExpr
    stateWithCoverageSite =
      recordPatternCoverageSite
        ( PatternCoverageSite
            coverageOrdinal
            (constructorInventoryFromBindings (inferDataTypes finalState) env)
            scrutineeType
            caseArms
        )
        finalState
```

Return `stateWithCoverageSite` while using it consistently for final
specialization and profile checks.

- [x] **Step 5: Resolve sites after final substitution.** Extend
      `FinalizedInference` with final coverage diagnostics. Sort observations by
      ordinal, resolve scrutinee types through `resolveType finalState`, and call
      `analyzePatternCoverage` once per site using the recorded lexical
      constructor inventory. Update `forceFinalizedInferenceContainers` so
      results cannot retain solver state.

- [x] **Step 6: Suppress cascades at `finishInference`.** Compute:

```haskell
baseDiagnostics = analyzerDiagnostics <> finalizedTypeErrors finalizedInference
coverageDiagnostics
  | any ((== SeverityError) . diagnosticSeverity) baseDiagnostics = []
  | otherwise = finalizedCoverageDiagnostics finalizedInference
diagnostics = baseDiagnostics <> coverageDiagnostics
```

Do not let warning-only analyzer output suppress coverage.

- [x] **Step 7: Lock rollback and production parity.** Add tests where an
      invalid duplicate-binder arm, unknown constructor, or failed nested body
      does not leak a coverage site, and verify the typed-core production entry
      returns the same source coverage errors before any profile status.

- [x] **Step 8: Run inference-focused tests and commit.**

Run:

```bash
nix --extra-experimental-features 'nix-command flakes' develop --command cabal test pattern-coverage-spec adt-pattern-type-spec module-pipeline-contract-spec jazz-typed-core-expression-direct-call-spec --test-show-details=direct --jobs=1
```

Expected: integration, suppression, module, and typed-core entry tests pass.

Commit:

```bash
git add src/Jazz/Compiler/PatternCoverage.hs src/Jazz/Compiler/TypeInference/State.hs src/Jazz/Compiler/TypeInference.hs test/Jazz/Compiler/Semantics/PatternCoverageSpec.hs
git commit -m "feat: retain resolved pattern matches"
```

### Task 4: Publish strict diagnostics and migrate affected behavior tests

**Files:**

- Modify: `src/Jazz/Compiler/TypeInference/Diagnostics.hs`
- Modify: `src/Jazz/Compiler/DiagnosticCatalog.hs`
- Modify: `src/Jazz/Compiler/TypeInference.hs`
- Modify: `test/Jazz/Compiler/Semantics/PatternCoverageSpec.hs`
- Modify: `test/Jazz/Compiler/Semantics/AdtPatternRuntimeSpec.hs`
- Modify: `test/Jazz/Compiler/Semantics/LambdaSemanticsSpec.hs`
- Modify: `test/Jazz/Compiler/Diagnostics/DiagnosticCatalogSpec.hs`

**Interfaces:**

- Consume: ordered `PatternCoverageFailure` values from Tasks 2 and 3.
- Produce:

```haskell
mkNonExhaustivePatternMatchError :: Pattern -> Diagnostic
mkUnreachablePatternArmError :: Int -> Diagnostic
```

with stable codes `E2018` and `E2019`.

- [x] **Step 1: Add failing catalog and diagnostic assertions.** Extend the
      exact error inventory through `2019`. Assert exact summaries:

```text
non-exhaustive pattern match; missing pattern: False
pattern arm 2 is unreachable because earlier unguarded arms cover it
```

- [x] **Step 2: Run focused diagnostics and prove the codes are absent.**

Run:

```bash
nix --extra-experimental-features 'nix-command flakes' develop --command cabal test pattern-coverage-spec diagnostic-catalog-spec --test-show-details=direct --jobs=1
```

Expected: FAIL on missing `E2018`/`E2019` catalog entries or summaries.

- [x] **Step 3: Add the stable catalog entries and constructors.** Extend
      `ErrorCode` with `E2018 | E2019`; keep `errorSubsystem` unchanged because
      both fall under `TypeDiagnostics`. Implement the two diagnostic builders
      with `CompilationOrigin`, the exact summaries above, and deterministic
      `help` for `E2018`: `add an unguarded arm that covers the missing pattern`.

- [x] **Step 4: Materialize ordered failures.** Map `NonExhaustivePattern` and
      `UnreachablePatternArm` to the new builders only after final type
      resolution. Emit unreachable arms in source order, then the
      non-exhaustive failure for that site.

- [x] **Step 5: Convert runtime no-match expectations to compile errors.** In
      `AdtPatternRuntimeSpec.hs` and `LambdaSemanticsSpec.hs`, replace tests whose
      purpose is no-match failure with `E2018` compile assertions and no runtime
      output. For successful tests that intentionally use refutable pattern
      lambdas, add explicit ordered wildcard clauses, for example:

```jazz
pick = \|((item, _) | (_, item)) -> item |(_) -> 0.
```

Do not weaken coverage or delete success coverage to preserve old fixtures.

- [x] **Step 6: Add strict reachability cases.** Prove `E2019` for duplicate
      literals, constructor arms after a covered constructor, exact-list arms
      after a cons wildcard, guarded arms covered by an earlier unguarded arm,
      and a wholly covered or-pattern. Prove repeated guarded patterns remain
      reachable.

- [x] **Step 7: Run the affected behavior matrix.**

Run:

```bash
nix --extra-experimental-features 'nix-command flakes' develop --command cabal test pattern-coverage-spec adt-pattern-type-spec adt-pattern-runtime-spec lambda-semantics-spec diagnostic-catalog-spec module-pipeline-contract-spec --test-show-details=direct --jobs=1
```

Expected: PASS with strict source errors and unchanged selection behavior for
exhaustive programs.

- [x] **Step 8: Commit the diagnostic contract.**

```bash
git add src/Jazz/Compiler/TypeInference/Diagnostics.hs src/Jazz/Compiler/DiagnosticCatalog.hs src/Jazz/Compiler/TypeInference.hs test/Jazz/Compiler/Semantics/PatternCoverageSpec.hs test/Jazz/Compiler/Semantics/AdtPatternRuntimeSpec.hs test/Jazz/Compiler/Semantics/LambdaSemanticsSpec.hs test/Jazz/Compiler/Diagnostics/DiagnosticCatalogSpec.hs
git commit -m "feat: reject incomplete pattern matches"
```

### Task 5: Close public documentation and execution state

**Files:**

- Modify: `docs/language/control-flow.md`
- Modify: `docs/language/algebraic-data-types-and-patterns.md`
- Modify: `docs/reference/diagnostics.md`
- Modify: `docs/project/status.md`
- Modify: `rfcs/accepted/0012-static-pattern-coverage.md`
- Modify: `.codex/plans/2026-08-14-jazz-static-pattern-coverage.md`
- Modify: `.codex/execution/queue.md`
- Modify: `.codex/execution/blocker-contracts.md`

**Interfaces:**

- Consume: verified implementation commits from Tasks 2 through 4.
- Produce: public truth for strict coverage, RFC implementation closure, a
  `complete` plan, and a terminal queue with no invented next child.

- [ ] **Step 1: Update public semantics.** State that cases and pattern lambdas
      must be statically exhaustive, guarded arms do not count, wholly
      unreachable arms are rejected, and runtime `E3022` remains defensive.
      Update the status row from `Planned` to `Implemented`.

- [ ] **Step 2: Update diagnostic documentation.** Extend the type range to
      `E2001`–`E2019` and document `E2018`/`E2019` concisely.

- [ ] **Step 3: Record RFC closure.** Add an `Implementation status` section to
      RFC 0012 naming the completed child and verified semantics.

- [ ] **Step 4: Run focused verification before closeout.**

Run:

```bash
nix --extra-experimental-features 'nix-command flakes' develop --command cabal test pattern-coverage-spec pattern-semantics-spec adt-pattern-type-spec adt-pattern-runtime-spec lambda-semantics-spec diagnostic-catalog-spec module-pipeline-contract-spec --test-show-details=direct --jobs=1
nix --extra-experimental-features 'nix-command flakes' develop --command cabal build all
```

Expected: all named suites and build targets pass.

- [ ] **Step 5: Run the full serialized suite and repository gates.**

Run:

```bash
nix --extra-experimental-features 'nix-command flakes' develop --command cabal test all --test-show-details=direct --jobs=1
bash scripts/check-execution-queue.sh
bash scripts/check-docs.sh
git diff --check
```

Expected: all Cabal suites, queue/docs checks, and diff checks pass. If the
outer shell lacks `lychee`, rerun `bash scripts/check-docs.sh` inside the Nix
development shell and record that exact successful command.

- [ ] **Step 6: Close the dispatcher.** Remove the completed `Ready Now` row,
      keep `Done` empty, update the bootstrap blocker with completion evidence,
      and explicitly record that no later source-backed candidate is named.
      Set this plan's `status: complete` and `plan_section: "Full closeout"`.

- [ ] **Step 7: Commit the verified closeout.**

```bash
git add docs/language/control-flow.md docs/language/algebraic-data-types-and-patterns.md docs/reference/diagnostics.md docs/project/status.md rfcs/accepted/0012-static-pattern-coverage.md .codex/plans/2026-08-14-jazz-static-pattern-coverage.md .codex/execution/queue.md .codex/execution/blocker-contracts.md
git commit -m "docs: close static pattern coverage"
```

## Full closeout

- [ ] Every active pattern form is normalized by one pure coverage engine.
- [ ] Every canonical `EPatternCase` is observed exactly once after inference.
- [ ] `E2018` rejects incomplete cases and pattern lambdas with a stable witness.
- [ ] `E2019` rejects wholly unreachable arms with a stable one-based index.
- [ ] Guarded arms never contribute coverage or shadow later arms.
- [ ] Existing analyzer/type errors suppress coverage cascades.
- [ ] Exhaustive programs preserve runtime selection and defensive `E3022`.
- [ ] Focused suites, build, full serialized tests, docs/queue checks, and
      `git diff --check` pass from committed state.
