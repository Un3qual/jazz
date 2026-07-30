# Jazz Pattern-Lambda Clauses Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Implement Jazz's historical `\|(patterns) -> body |(patterns) -> body` expression syntax for ordered, curried, multi-body pattern lambdas while continuing to reject Haskell-style named equations.

**Architecture:** Add an expression-level surface clause node to the hosted and Jazz-authored parsers. Lower every clause lambda to existing generated unary lambda arguments wrapped around one ordered `EPatternCase`/`CorePatternCaseExpression`; do not add a core callable or restore statement-level function nodes.

**Tech Stack:** Haskell/GHC 9.14.1, Jazz-authored `.jz` compiler modules, Cabal, the existing handwritten token parsers, repository/editor audits, and Nix development-shell verification.

## Global Constraints

- Modify compiler behavior only under `jazz-next/`.
- Treat `jazz-hs/` and `jazz2/` as read-only references.
- Keep `name pattern = body.` rejected with no compatibility warning or rewrite.
- Keep ordinary `\(patterns) -> body` lambdas and same-body lambda or-patterns unchanged.
- Preserve currying, partial application, recursive ordinary bindings, ordered first-match behavior, and runtime diagnostic `E3022`.
- All clauses in one `\|` expression must have the same non-zero source arity; `()` remains one Unit-pattern parameter.
- Reuse `LambdaPatternArgument`; do not restore equation-specific generated names.
- Preserve pipe operators in clause bodies unless the tokens form a complete `|(parameters) ->` clause boundary.
- Keep indentation non-semantic.
- Use red-green TDD and commit each independently verified task.

---

### Task 1: Hosted Parser, Surface AST, Lowering, and Runtime Contract

**Files:**

- Modify: `jazz-next/test/JazzNext/Compiler/Parser/LambdaParserSpec.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Semantics/LambdaSemanticsSpec.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Parser/AST.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Parser/Failure.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Parser/Expression.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Parser/Lower.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Force.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/ModuleResolver.hs`

**Interfaces:**

- Consumes: existing `SurfacePattern`, `SurfaceLambdaParameter`, `LambdaPatternArgument`, `EPatternCase`, `CaseArm`, and case-arm body boundary parsing.
- Produces:

```haskell
data SurfacePatternLambdaClause =
  SurfacePatternLambdaClause SourceSpan (NonEmpty SurfacePattern) SurfaceExpr

data SurfaceExpr
  = ...
  | SEPatternLambda (NonEmpty SurfacePatternLambdaClause)

data ParserPatternFailure
  = ConsLikeListPatternHeadCount
  | PatternLambdaClauseArityMismatch Int Int
```

- `SELambda` remains unchanged for ordinary single-body lambdas.

- [x] **Step 1: Add public parser failures before implementation**

Add focused tests that use only the existing public parser entrypoint, so they
compile before the new AST constructor exists:

```haskell
testAcceptsPatternLambdaClauses :: IO ()
testAcceptsPatternLambdaClauses =
  assertRight
    "multi-body pattern lambda"
    ( parseSurfaceProgram
        "choose = \\|(Nothing, fallback) -> fallback |(Just item, _) -> item."
    )
    (\_ -> pure ())

testRejectsPatternLambdaClauseArityMismatch :: IO ()
testRejectsPatternLambdaClauseArityMismatch =
  assertLeftDiagnosticContains
    "pattern lambda clause arity"
    "pattern-lambda clauses must all have 1 parameter(s), found 2"
    (parseSurfaceProgram "choose = \\|([]) -> 0 |([item | rest], fallback) -> item.")

testKeepsPipeOperatorInPatternLambdaBody :: IO ()
testKeepsPipeOperatorInPatternLambdaBody =
  assertRight
    "pipe operator before next lambda clause"
    ( parseSurfaceProgram
        "operator (|) tier 4 precedence 20 left. choose = \\|(0) -> 1 | 2 |(_) -> 3."
    )
    (\_ -> pure ())

testRejectsPatternLambdaWithoutHead :: IO ()
testRejectsPatternLambdaWithoutHead =
  assertLeftDiagnosticContains
    "pattern lambda without head"
    "expected '('"
    (parseSurfaceProgram "choose = \\|.")

testRejectsPatternLambdaHeadWithoutArrow :: IO ()
testRejectsPatternLambdaHeadWithoutArrow =
  assertLeftDiagnosticContains
    "pattern lambda head without arrow"
    "expected '->'"
    (parseSurfaceProgram "choose = \\|(item) item.")

testRejectsPatternLambdaWithoutBody :: IO ()
testRejectsPatternLambdaWithoutBody =
  assertLeftDiagnosticContains
    "pattern lambda without body"
    "expected expression"
    (parseSurfaceProgram "choose = \\|(item) ->.")
```

Register all six tests in `LambdaParserSpec.tests`.

- [x] **Step 2: Add end-to-end semantic failures before implementation**

Add these real-source tests to `LambdaSemanticsSpec` and register them:

```haskell
testPatternLambdaClauseOrderRuntime :: IO ()
testPatternLambdaClauseOrderRuntime = do
  result <-
    runSource
      defaultWarningSettings
      "pick = \\|(0) -> 10 |(_) -> 20. (pick 0, pick 1)."
  assertSuccessfulRuntime "ordered pattern lambda clauses" (Just "(10, 20)") result

testPatternLambdaClausePartialApplicationRuntime :: IO ()
testPatternLambdaClausePartialApplicationRuntime = do
  result <-
    runSource
      defaultWarningSettings
      "choose = \\|(Nothing, fallback) -> fallback |(Just item, _) -> item. keep = choose (Just 42). keep 0."
  assertSuccessfulRuntime "partial pattern lambda application" (Just "42") result

testRecursivePatternLambdaClausesRuntime :: IO ()
testRecursivePatternLambdaClausesRuntime = do
  result <-
    runSource
      defaultWarningSettings
      "length = \\|([]) -> 0 |([_ | rest]) -> 1 + length rest. length [1, 2, 3, 4]."
  assertSuccessfulRuntime "recursive pattern lambda clauses" (Just "4") result

testPatternLambdaClausesNoMatchRuntime :: IO ()
testPatternLambdaClausesNoMatchRuntime = do
  result <- runSource defaultWarningSettings "onlyZero = \\|(0) -> 1. onlyZero 1."
  assertEqual "non-exhaustive compile errors" [] (runCompileErrors result)
  assertSingleDiagnosticCode
    "pattern lambda clause no-match code"
    "E3022"
    (runRuntimeErrors result)
```

Reuse the suite's existing `assertSuccessfulRuntime` helper.

- [x] **Step 3: Run RED tests**

Run:

```bash
nix --extra-experimental-features 'nix-command flakes' develop -c \
  cabal test --project-dir=jazz-next \
  lambda-parser-spec lambda-semantics-spec \
  --test-show-details=failures
```

Expected: FAIL because `parseLambdaExpr` requires `(` immediately after `\`
and rejects the leading `|`.

- [x] **Step 4: Add the surface types and structured diagnostic**

Export and define `SurfacePatternLambdaClause` in `Parser/AST.hs`, then add
`SEPatternLambda` to `SurfaceExpr`.

In `Parser/Failure.hs`, add
`PatternLambdaClauseArityMismatch expected actual` and render it as:

```text
pattern-lambda clauses must all have <expected> parameter(s), found <actual>
```

Keep the diagnostic under `PatternFailure`; it is expression/pattern syntax,
not a declaration failure.

- [x] **Step 5: Parse the historical `\|` form**

Extend `parseLambdaExpr`:

```haskell
case tokens of
  Token {tokenKind = TOperator "|"} : _ ->
    parsePatternLambdaExpr parseBlock context stop lambdaToken
  Token {tokenKind = TLParen} : _ ->
    parseOrdinaryLambdaExpr ...
```

Implement focused helpers in `Parser/Expression.hs`:

```haskell
parsePatternLambdaExpr ::
  StatementBlockParser ->
  ParserContext ->
  Stop ->
  Token ->
  Parser SurfaceExpr

parsePatternLambdaClauseHead ::
  Parser (SourceSpan, NonEmpty SurfacePattern)

surfaceLambdaParameterPattern ::
  SurfaceLambdaParameter ->
  SurfacePattern
```

The first helper consumes the initial `|`, parses a parenthesized
`parseLambdaParameters` list, converts identifiers to `SPVariable`, consumes
`->`, and parses the body with the existing case-arm body expression machinery
plus the caller's outer `Stop`.

Collect later clauses while the remaining stream begins with a `|` whose
following parenthesized parameter list ends at `->`. Compare each head length
with the first clause arity and fail at the mismatching pipe/head span.

Reuse the existing case-arm boundary logic for body expressions. A body-level
pipe is a clause boundary only when lookahead recognizes a complete
`|(parameters) ->` head; otherwise normal operator precedence owns it.

- [x] **Step 6: Lower clauses to existing core**

In `Parser/Lower.hs`, add:

```haskell
lowerSurfacePatternLambda ::
  NonEmpty SurfacePatternLambdaClause ->
  Expr
```

For arity `n`, create generated names:

```haskell
map (generatedName . LambdaPatternArgument) [1 .. n]
```

Build the scrutinee as `EVar arg1` for arity one and
`ETuple [EVar arg1, ..., EVar argN]` otherwise. Convert each clause to one
`CaseArm`, using its direct pattern for arity one and `PTuple` for arity
greater than one. Wrap the `EPatternCase` in nested `ELambda` nodes in source
parameter order.

Do not route through `lowerSurfaceLambda`; ordinary identifier lambdas retain
their direct source-name core binders.

- [x] **Step 7: Complete hosted traversals**

Update every exhaustive `SurfaceExpr` traversal in `Force.hs` and
`ModuleResolver.hs`.

For free/reference collection, inspect each clause body under only that
clause's pattern binders:

```haskell
collectPatternLambdaClauseReferences boundNames
  (SurfacePatternLambdaClause _ patterns body) =
    collectExprReferences
      (Set.union boundNames (Set.unions (map collectPatternBinders (NonEmpty.toList patterns))))
      body
      <> Set.unions (map collectPatternReferences (NonEmpty.toList patterns))
```

Qualified value/type reference collection visits every clause body and every
pattern reference using the existing helpers. Forcing recursively forces
clause patterns and bodies while preserving order and spans.

- [x] **Step 8: Add exact AST and lowering assertions**

Now that the public types exist, extend `LambdaParserSpec` with exact expected
surface and core trees for:

```jazz
choose =
  \|(Nothing, fallback) -> fallback
   |(Just item, _) -> item.
```

Assert `SEPatternLambda` contains two ordered clauses, each with arity two.
Assert lowering produces two generated unary lambdas around one tuple
`EPatternCase` with two ordered arms.

- [x] **Step 9: Run GREEN hosted tests**

Run the same command from Step 3.

Expected: PASS for both suites.

- [x] **Step 10: Run adjacent hosted regression suites**

Run:

```bash
nix --extra-experimental-features 'nix-command flakes' develop -c \
  cabal test --project-dir=jazz-next \
  declaration-parser-spec adt-pattern-runtime-spec module-resolution-spec \
  recursive-bindings-spec \
  --test-show-details=failures
```

Expected: PASS, including continued rejection of named equations.

- [x] **Step 11: Commit the hosted implementation**

```bash
git add jazz-next/src/JazzNext/Compiler \
  jazz-next/test/JazzNext/Compiler/Parser/LambdaParserSpec.hs \
  jazz-next/test/JazzNext/Compiler/Semantics/LambdaSemanticsSpec.hs
git commit -m "feat: add pattern lambda clauses"
```

---

### Task 2: Jazz-Authored Parser and Canonical-Core Parity

**Files:**

- Modify: `jazz-next/jazz/compiler/ParserTypes.jz`
- Modify: `jazz-next/jazz/compiler/ParserExpression.jz`
- Modify: `jazz-next/jazz/compiler/CoreLower.jz`
- Modify: `jazz-next/test/JazzNext/Compiler/Bootstrap/CanonicalParserComparison.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Bootstrap/CanonicalParserComparisonSpec.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Bootstrap/JazzParserParity.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Bootstrap/JazzCoreParity.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Bootstrap/JazzParserControlFlowPatternsSpec.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Bootstrap/JazzCoreControlFlowPatternsSpec.hs`

**Interfaces:**

- Consumes: Task 1's `SurfacePatternLambdaClause` and `SEPatternLambda`.
- Produces Jazz-authored mirrors:

```jazz
data SurfacePatternLambdaClause =
  SurfacePatternLambdaClause CanonicalSpan NonEmpty(SurfacePattern) SurfaceExpr.

data SurfaceExpr
  = ...
  | PatternLambdaExpression NonEmpty(SurfacePatternLambdaClause).

data ParserPatternFailure
  = ConsLikeListPatternHeadCount
  | PatternLambdaClauseArityMismatch Int Int.
```

- [x] **Step 1: Add parity failures first**

In `JazzParserControlFlowPatternsSpec`, register:

```haskell
( "parses ordered pattern lambda clauses",
  assertStage0Parity
    "pattern lambda clauses"
    "choose = \\|(Nothing, fallback) -> fallback |(Just item, _) -> item."
)
```

Add a mismatched-arity source to the direct failure parity table.

In `JazzCoreControlFlowPatternsSpec`, add a direct `SEPatternLambda` fixture
whose expected output is the same generated-lambda/ordered-case core tree
locked by Task 1.

Add `PatternLambdaExpression`, `SurfacePatternLambdaClause`, and
`PatternLambdaClauseArityMismatch` to
`CanonicalParserComparisonSpec`'s complete schema/failure inventories.

- [x] **Step 2: Run RED parity tests**

Run:

```bash
nix --extra-experimental-features 'nix-command flakes' develop -c \
  cabal test --project-dir=jazz-next \
  canonical-parser-comparison-spec \
  jazz-parser-control-flow-patterns-spec \
  jazz-core-control-flow-patterns-spec \
  --test-show-details=failures
```

Expected: FAIL because the Jazz schema/parser/lowerer cannot construct or
lower the new surface expression.

- [x] **Step 3: Mirror the schema and canonical adapters**

Add the exact Jazz types above to `ParserTypes.jz`. Update the Haskell runtime
value adapters in `CanonicalParserComparison.hs`, `JazzParserParity.hs`, and
`JazzCoreParity.hs` so both implementations serialize:

```text
PatternLambdaExpression
  [SurfacePatternLambdaClause span patterns body, ...]
```

Preserve clause order, source spans, parameter order, and body structure.

Update all complete constructor and failure inventories rather than adding
fallback rendering.

- [x] **Step 4: Implement Jazz-authored parsing**

In `ParserExpression.jz`, mirror the hosted helpers:

- recognize `OperatorKind "|"` immediately after `LambdaPunctuation`;
- parse one parenthesized head through the existing lambda-parameter parser;
- convert `IdentifierParameter name` to `VariablePattern name`;
- preserve `PatternParameter pattern`;
- parse each body until a definite `|(parameters) ->` boundary or the caller's
  outer stop;
- enforce the first clause's arity for later clauses; and
- construct `PatternLambdaExpression`.

Reuse existing token-parser combinators and boundary scanners. Do not parse
named declarations or add an equation statement.

- [x] **Step 5: Implement Jazz-authored lowering**

In `CoreLower.jz`, add helpers that:

- lower each clause's patterns/body;
- create `CoreGeneratedName (CoreLambdaPatternArgument index)` arguments;
- use one variable scrutinee for arity one and `CoreTupleExpression` for
  multiple arguments;
- build ordered `CoreCaseArm` values; and
- nest `CoreLambdaExpression` values around one
  `CorePatternCaseExpression`.

Update profile accounting and qualification traversal for
`PatternLambdaExpression`. Keep the result in
`ControlFlowPatternsProfile`.

- [x] **Step 6: Run GREEN parity tests**

Run the same command from Step 2.

Expected: PASS with exact repeated hosted/Jazz-authored results.

- [x] **Step 7: Run adjacent bootstrap suites**

Run:

```bash
nix --extra-experimental-features 'nix-command flakes' develop -c \
  cabal test --project-dir=jazz-next \
  jazz-parser-parity-spec jazz-parser-scale-spec \
  jazz-parser-types-declarations-modules-spec \
  jazz-typed-core-contract-spec \
  --test-show-details=failures
```

Expected: PASS.

- [x] **Step 8: Commit the Jazz-authored mirror**

```bash
git add jazz-next/jazz/compiler \
  jazz-next/test/JazzNext/Compiler/Bootstrap
git commit -m "feat: mirror pattern lambda clauses in Jazz"
```

---

### Task 3: Authored Jazz, Feature Inventory, and Editor Surface

**Files:**

- Modify: `jazz-next/programs/fannkuch/Fannkuch.jz`
- Modify: `jazz-next/programs/merge-sort/MergeSort.jz`
- Modify: `jazz-next/programs/n-queens/Queens.jz`
- Modify: `jazz-next/programs/prime-sieve/Sieve.jz`
- Modify: `jazz-next/programs/symbolic-differentiation/Symbolic.jz`
- Modify: `jazz-next/programs/tak/Tak.jz` only if a genuine multi-head
  definition improves it; do not manufacture a wildcard clause around its
  existing conditional.
- Modify: `jazz-next/test/JazzNext/Repository/FeatureInventory.hs`
- Modify: `jazz-next/test/JazzNext/Repository/AuditSpec.hs`
- Modify: `jazz-next/editors/vscode-jazz/fixtures/representative.jz`
- Modify: `jazz-next/editors/vscode-jazz/syntaxes/jazz.tmLanguage.json`

**Interfaces:**

- Consumes: complete hosted parser and runtime plus `SEPatternLambda`.
- Produces: `PatternLambdaClausesFeature` repository inventory coverage and a
  representative editor fixture using `\|`.

- [x] **Step 1: Add repository/editor failures first**

Add `PatternLambdaClausesFeature` to `SurfaceFeature`.

Teach `inventoryExpr` to assign it for `SEPatternLambda`, inventory every
clause pattern/body, and preserve all nested features.

Add the feature to the required authored-source coverage set and add
`("pattern lambda clauses", "\\|(")` to `requiredEditorSyntax`.

Add an editor grammar assertion that the `\|` introducer is recognized as
lambda punctuation rather than only as an ordinary pipe operator.

- [x] **Step 2: Run RED repository tests**

Run:

```bash
nix --extra-experimental-features 'nix-command flakes' develop -c \
  cabal test --project-dir=jazz-next \
  repository-audit-spec program-corpus-spec \
  --test-show-details=failures
```

Expected: FAIL because no authored source or editor fixture yet demonstrates
the new feature and the grammar does not scope the combined introducer.

- [x] **Step 3: Migrate the mistaken equation-removal rewrites**

Convert functions in the five algorithm modules whose complete body is direct
dispatch over all lambda parameters:

```jazz
permutations =
  \|([]) -> [[]]
   |([first | rest]) ->
      listConcat $ listMap (insertEverywhere first) $ permutations rest.
```

```jazz
merge =
  \|([], rights) -> rights
   |(lefts, []) -> lefts
   |([left | lefts], [right | rights]) ->
      if left <= right then
        listPrepend left $ merge lefts (listPrepend right rights)
      else
        listPrepend right $ merge (listPrepend left lefts) rights.
```

Apply the same rule to eligible definitions in `Queens.jz`, `Sieve.jz`, and
`Symbolic.jz`. Keep explicit `case` when the scrutinee is computed, matching is
nested after setup, or the function is inspecting a value inside a larger
body.

Audit `Tak.jz`; retain its single ordinary lambda plus `if` unless a real
second pattern head exists. Do not add syntax merely to satisfy inventory.

Do not broadly rewrite pre-existing compiler/stdlib `case` expressions that
were not introduced by the mistaken equation migration.

- [x] **Step 4: Update the editor fixture and grammar**

Add a formatted representative form:

```jazz
chooseValue =
  \|(Nothing, fallback) -> fallback
   |(Just item, _) -> item.
```

Place the combined `\|` matcher before the general lambda-backslash and
operator matchers in `jazz.tmLanguage.json`, with the established lambda scope.
Keep standalone `|` highlighted as an operator/pattern delimiter according to
its surrounding repository rule.

- [x] **Step 5: Run GREEN repository tests**

Run the same command from Step 2.

Expected: PASS for feature coverage, editor validation, every program output,
and benchmark manifest expectations.

- [x] **Step 6: Audit all authored syntax**

Run:

```bash
rg -n --pcre2 \
  '^(?!\s*(?:data|if|else|case|module|import|operator|class|impl)\b)\s*[a-z][A-Za-z0-9_\x27]*\h+[^=\n]+\h=(?:\h|$)' \
  jazz-next --glob '*.jz' \
  --glob '!dist-newstyle/**' \
  --glob '!dist-newstyle-profile-hotspots/**'
```

Expected: no named Haskell-style equations.

Review all new `\|` forms for consistent line breaks, `$` use, and the authored
source-format policy.

- [x] **Step 7: Commit authored sources and editor support**

```bash
git add jazz-next/programs \
  jazz-next/test/JazzNext/Repository \
  jazz-next/editors/vscode-jazz
git commit -m "refactor: use pattern lambda clauses in Jazz"
```

---

### Task 4: Active Documentation and Complete Verification

**Files:**

- Modify: `docs/spec/pattern-matching-semantics.md`
- Modify: `docs/spec/adt-pattern-semantics.md`
- Modify: `docs/spec/authoritative-syntax.md`
- Modify: `docs/jazz-language-state.md`
- Modify: `docs/execution/queue.md`
- Modify: `jazz-next/README.md`
- Modify: `docs/superpowers/plans/2026-07-30-jazz-remove-function-equations.md`
- Modify: `docs/superpowers/specs/2026-07-30-jazz-pattern-lambda-clauses-design.md`

**Interfaces:**

- Consumes: verified behavior from Tasks 1–3.
- Produces: one consistent active language contract and a clean, fully passing
  branch.

- [ ] **Step 1: Reconcile active documentation**

Document:

- ordinary `\(patterns) -> body`;
- same-body `\(pattern | alternative) -> body`;
- multi-body `\|(patterns) -> body |(patterns) -> body`;
- common arity, ordered matching, per-clause binders, currying/partial
  application, and `E3022`;
- continued rejection of `name pattern = body.`; and
- the boundary between canonical clause lambdas and intentional explicit
  `case`.

Mark the earlier implementation plan's explicit-`case` migration instruction
as superseded by this plan. Change the new design status from approved to
implemented only after all verification below succeeds.

- [ ] **Step 2: Run focused documentation gates**

Run:

```bash
bash scripts/check-execution-queue.sh
bash scripts/check-docs.sh
git diff --check
```

Expected: all pass.

- [ ] **Step 3: Run the complete Cabal test matrix**

Run:

```bash
nix --extra-experimental-features 'nix-command flakes' develop -c \
  cabal test --project-dir=jazz-next all --test-show-details=failures
```

Expected: every suite passes with zero failures.

- [ ] **Step 4: Run repository residue checks**

Run:

```bash
rg -n \
  'SSFunction|SurfaceFunctionClause|FunctionEquationArgument|CoreFunctionEquationArgument|parseFunctionEquation' \
  jazz-next/src jazz-next/jazz jazz-next/test jazz-next/jazz-next.cabal \
  --glob '!dist-newstyle/**' \
  --glob '!dist-newstyle-profile-hotspots/**'
```

Expected: no equation-specific representation or parser path.

Run the named-equation `.jz` scan from Task 3 again.

- [ ] **Step 5: Commit documentation and final verification state**

```bash
git add docs jazz-next/README.md
git commit -m "docs: document pattern lambda clauses"
git status --short --branch
```

Expected: the branch is clean after the commit.
