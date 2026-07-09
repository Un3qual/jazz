# Jazz Unit as Empty Tuple Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add `()` as Jazz's zero-element tuple Unit value, pattern, and type, with `\()` as sugar for a unary Unit-pattern lambda.

**Architecture:** Reuse the existing list-backed tuple representations at every compiler layer: empty lists become Unit without adding new AST, type, or runtime constructors. Extend only the surface parsers and regression coverage; the existing lowering, tuple inference, pattern matching, runtime rendering, and structural equality paths remain the semantic implementation. Preserve `NonEmpty SurfaceLambdaParameter` by representing `\()` as one `SPTuple []` pattern parameter.

**Tech Stack:** Haskell 2010, Cabal, Megaparsec, Jazz's parser/surface AST/lowering/type-inference/runtime pipeline, repository test harness.

## Global Constraints

- Modify only the active compiler under `jazz-next/` and its authoritative documentation; `jazz-hs/` and `jazz2/` remain read-only.
- Do not add a true arity-zero lambda, function type, or application node.
- Do not add a named `Unit` primitive or constructor; `()` is the zero-element tuple at every layer.
- Keep `NonEmpty SurfaceLambdaParameter` and all existing unary/currying behavior.
- Keep malformed tuple/lambda diagnostics deterministic and retain existing diagnostic codes unless a focused test proves one misleading.
- Implement each behavior test-first and commit coherent checkpoints.

---

## File Map

- `jazz-next/src/JazzNext/Compiler/Parser.hs`: parse empty parenthesized expressions and the `\()` Unit-lambda shorthand.
- `jazz-next/src/JazzNext/Compiler/Parser/Pattern.hs`: parse `()` as the empty tuple pattern.
- `jazz-next/src/JazzNext/Compiler/Parser/Signature.hs`: parse `()` in monomorphic and constrained signature grammars.
- `jazz-next/test/JazzNext/Compiler/Parser/ExpressionParserSpec.hs`: direct Unit-expression parser regression.
- `jazz-next/test/JazzNext/Compiler/Parser/PatternParserSpec.hs`: direct Unit-pattern parser regression.
- `jazz-next/test/JazzNext/Compiler/Parser/ParserFoundationSpec.hs`: structured and lowered Unit signature/value regressions.
- `jazz-next/test/JazzNext/Compiler/Parser/LambdaParserSpec.hs`: shorthand/explicit Unit lambda parsing, lowering, and malformed syntax regressions.
- `jazz-next/test/JazzNext/Compiler/Semantics/LambdaSemanticsSpec.hs`: end-to-end Unit lambda typing and runtime regressions.
- `jazz-next/test/JazzNext/Compiler/Semantics/RuntimeSemanticsSpec.hs`: Unit rendering and structural-equality regression.
- `docs/spec/authoritative-syntax.md`: canonical Unit and Unit-lambda syntax.
- `docs/spec/pattern-matching-semantics.md`: zero-arity tuple pattern behavior.
- `docs/spec/adt-pattern-semantics.md`: active zero-element tuple value/type/pattern status.
- `docs/spec/semantics/bindings-and-signatures.md`: `()` in supported monomorphic signature grammar.

---

### Task 1: Parse and lower Unit values, patterns, and signature types

**Files:**
- Modify: `jazz-next/test/JazzNext/Compiler/Parser/ExpressionParserSpec.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Parser/PatternParserSpec.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Parser/ParserFoundationSpec.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Semantics/RuntimeSemanticsSpec.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Parser.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Parser/Pattern.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Parser/Signature.hs`

**Interfaces:**
- Consumes: existing `SETuple [SurfaceExpr]`, `SPTuple [SurfacePattern]`, `SurfaceTypeTuple [SurfaceSignatureType]`, and `SurfaceConstrainedTypeTuple [SurfaceConstrainedSignatureType]` constructors.
- Produces: `parseParenExpr ... [TRParen] == Right (SETuple [], ...)`; `parseCaseArmPatternTokens` returns `SPTuple []` for `()`; both signature parsers return empty tuple types; existing lowering yields `ETuple []`, `PTuple []`, `TypeTuple []`, and `ConstraintTypeTuple []` without new constructors.

- [ ] **Step 1: Add the failing Unit-expression parser test**

Add the test registration and function to `ExpressionParserSpec.hs`:

```haskell
    ("parses Unit as the empty tuple expression", testParsesUnitExpression),
```

```haskell
testParsesUnitExpression :: IO ()
testParsesUnitExpression = do
  tokens <- lexSource "()."
  assertExpression
    "Unit expression"
    (SETuple [])
    [TDot]
    (parseExpressionTokens Set.empty [] tokens)
```

- [ ] **Step 2: Add the failing Unit-pattern parser test**

Register and add this test in `PatternParserSpec.hs`:

```haskell
    ("parses Unit case-arm pattern tokens", testParsesUnitCaseArmPatternTokens),
```

```haskell
testParsesUnitCaseArmPatternTokens :: IO ()
testParsesUnitCaseArmPatternTokens = do
  tokens <- lexSource "() -> body"
  assertEqual
    "Unit case-arm pattern"
    (Right (SPTuple [], [TArrow, TIdentifier "body"]))
    (fmap (fmap tokenKinds) (parseCaseArmPatternTokens tokens))
```

- [ ] **Step 3: Add failing structured/lowered Unit signature tests**

Import no new constructors; the required constructors are already imported by `ParserFoundationSpec.hs`. Register these tests:

```haskell
    ("parses Unit value and signature into structured nodes", testParseUnitValueAndSignature),
    ("parses constrained Unit signature into structured nodes", testParseConstrainedUnitSignature),
    ("lowers Unit value and signature into analyzer AST", testLowerUnitValueAndSignature),
```

Add the test bodies:

```haskell
testParseUnitValueAndSignature :: IO ()
testParseUnitValueAndSignature =
  assertEqual
    "Unit value and signature"
    ( Right
        ( SEBlock
            [ SSSignature
                "unit"
                (SourceSpan 1 1)
                (SurfaceSignatureType (SurfaceTypeTuple [])),
              SSLet "unit" (SourceSpan 2 1) (SETuple [])
            ]
        )
    )
    (parseSurfaceProgram "unit :: ().\nunit = ().")

testParseConstrainedUnitSignature :: IO ()
testParseConstrainedUnitSignature =
  assertEqual
    "constrained Unit signature"
    ( Right
        ( SEBlock
            [ SSSignature
                "unit"
                (SourceSpan 1 1)
                (SurfaceConstrainedSignature [] (SurfaceConstrainedTypeTuple [])),
              SSLet "unit" (SourceSpan 2 1) (SETuple [])
            ]
        )
    )
    (parseSurfaceProgram "unit :: @{}: ().\nunit = ().")

testLowerUnitValueAndSignature :: IO ()
testLowerUnitValueAndSignature =
  assertRight
    "parse + lower Unit value/signature"
    (parseSurfaceProgram "unit :: ().\nunit = ().")
    ( \surfaceProgram ->
        assertEqual
          "lowered Unit AST"
          ( EBlock
              [ SSignature
                  "unit"
                  (SourceSpan 1 1)
                  (SignatureType (TypeTuple [])),
                SLet "unit" (SourceSpan 2 1) (ETuple [])
              ]
          )
          (lowerSurfaceExpr surfaceProgram)
    )
```

- [ ] **Step 4: Add a failing runtime rendering and equality test**

Register and add this test in `RuntimeSemanticsSpec.hs`:

```haskell
    ("Unit renders and participates in structural equality", testUnitRenderingAndEquality),
```

```haskell
testUnitRenderingAndEquality :: IO ()
testUnitRenderingAndEquality = do
  result <- runSource defaultWarningSettings "(() == (), ())."
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "(True, ())") (runOutput result)
```

- [ ] **Step 5: Run the focused tests and verify the red state**

Run from `jazz-next/`:

```bash
cabal test expression-parser-spec pattern-parser-spec parser-foundation-spec runtime-semantics-spec --test-show-details=direct
```

Expected: FAIL. `()` cannot yet start an empty parenthesized expression, `parseTuplePattern` expects a first pattern, and both signature parsers expect a first type.

- [ ] **Step 6: Parse the Unit expression**

Add the empty case before operator/grouping handling in `parseParenExpr` in `Parser.hs`:

```haskell
parseParenExpr knownAliases declaredOperators tokensAfterLeftParen =
  case tokensAfterLeftParen of
    Token {tokenKind = TRParen} : remaining ->
      Right (SETuple [], remaining)
    operatorToken@(Token {tokenKind = TOperator operatorSymbol}) : rest ->
      case rest of
        Token {tokenKind = TRParen} : remaining -> do
          requireOperatorVisible declaredOperators operatorToken
          Right (SEOperatorValue operatorSymbol, remaining)
        _ -> do
          requireOperatorVisible declaredOperators operatorToken
          (rightExpr, afterRightExpr) <- parseExpr knownAliases declaredOperators rest
          remaining <- consumeRightParen afterRightExpr
          pure (SESectionRight operatorSymbol rightExpr, remaining)
    _ -> do
      (innerExpr, afterInnerExpr) <- parseExpr knownAliases declaredOperators tokensAfterLeftParen
      case afterInnerExpr of
        Token {tokenKind = TComma} : rest -> do
          (tupleElements, afterTupleElements) <-
            parseTupleElements knownAliases declaredOperators [innerExpr] rest
          remaining <- consumeRightParen afterTupleElements
          Right (SETuple tupleElements, remaining)
        operatorToken@(Token {tokenKind = TOperator operatorSymbol}) : Token {tokenKind = TRParen} : rest -> do
          requireOperatorVisible declaredOperators operatorToken
          Right (SESectionLeft innerExpr operatorSymbol, rest)
        _ -> do
          remaining <- consumeRightParen afterInnerExpr
          Right (innerExpr, remaining)
```

- [ ] **Step 7: Parse the Unit pattern**

Make `parseTuplePattern` in `Parser/Pattern.hs` recognize the closing token before requesting a first pattern:

```haskell
parseTuplePattern :: Token -> PatternParser SurfacePattern
parseTuplePattern leftParenToken = do
  tokens <- getRemainingTokens
  case tokens of
    Token {tokenKind = TRParen} : rest -> do
      setRemainingTokens rest
      pure (SPTuple [])
    _ -> do
      firstPattern <- parseCasePattern
      afterFirstPattern <- getRemainingTokens
      case afterFirstPattern of
        Token {tokenKind = TComma} : rest -> do
          setRemainingTokens rest
          tuplePatterns <- parseTuplePatternElements [firstPattern]
          consumeRightParen
          pure (SPTuple tuplePatterns)
        _ ->
          throwDiagnostic (expectedCasePatternDiagnostic leftParenToken)
```

- [ ] **Step 8: Parse Unit in both signature grammars**

Change `constrainedParenthesizedTypeParser` in `Parser/Signature.hs` to accept an empty body while preserving grouping and tuples:

```haskell
constrainedParenthesizedTypeParser =
  betweenTokenKinds TLParen TRParen $
    ( MP.lookAhead (TokenParser.parseTokenKind TRParen)
        *> pure (SurfaceConstrainedTypeTuple [])
    )
      <|> do
        firstElement <- constrainedSignatureTypeParser
        remainingElements <- MP.many (commaParser *> constrainedSignatureTypeParser)
        case remainingElements of
          [] -> pure firstElement
          _ -> pure (SurfaceConstrainedTypeTuple (firstElement : remainingElements))
```

Apply the same shape to `parenthesizedSignatureTypeParser`:

```haskell
parenthesizedSignatureTypeParser =
  betweenTokenKinds TLParen TRParen $
    ( MP.lookAhead (TokenParser.parseTokenKind TRParen)
        *> pure (SurfaceTypeTuple [])
    )
      <|> do
        firstElement <- signatureTypeParser
        remainingElements <- MP.many (commaParser *> signatureTypeParser)
        case remainingElements of
          [] -> pure firstElement
          _ -> pure (SurfaceTypeTuple (firstElement : remainingElements))
```

- [ ] **Step 9: Run the focused tests and verify green**

```bash
cabal test expression-parser-spec pattern-parser-spec parser-foundation-spec runtime-semantics-spec --test-show-details=direct
```

Expected: all three suites PASS, including existing grouping and non-empty tuple regressions.

- [ ] **Step 10: Commit the Unit value/type/pattern slice**

```bash
git add \
  jazz-next/src/JazzNext/Compiler/Parser.hs \
  jazz-next/src/JazzNext/Compiler/Parser/Pattern.hs \
  jazz-next/src/JazzNext/Compiler/Parser/Signature.hs \
  jazz-next/test/JazzNext/Compiler/Parser/ExpressionParserSpec.hs \
  jazz-next/test/JazzNext/Compiler/Parser/PatternParserSpec.hs \
  jazz-next/test/JazzNext/Compiler/Parser/ParserFoundationSpec.hs \
  jazz-next/test/JazzNext/Compiler/Semantics/RuntimeSemanticsSpec.hs
git commit -m "feat: add Jazz Unit value type and pattern"
```

---

### Task 2: Add Unit-lambda sugar and prove unary semantics end to end

**Files:**
- Modify: `jazz-next/test/JazzNext/Compiler/Parser/LambdaParserSpec.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Semantics/LambdaSemanticsSpec.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Parser.hs`

**Interfaces:**
- Consumes: Task 1's `SPTuple []`, `SETuple []`, `SurfaceTypeTuple []`, and unchanged pattern-parameter lowering.
- Produces: `\()` and `\(())` both parse as `SELambda (SurfaceLambdaPattern (SPTuple []) :| []) body`; lowering remains a unary `ELambda`; `() -> a` signatures and `function ()` applications typecheck through existing tuple semantics.

- [ ] **Step 1: Replace the obsolete rejection with failing shorthand and explicit parser tests**

Remove the `"rejects empty lambda parameter list"` registration and `testRejectsEmptyLambdaParameters`. Register:

```haskell
    ("parses Unit lambda shorthand as one pattern parameter", testParsesUnitLambdaShorthand),
    ("parses explicit nested Unit lambda parameter", testParsesExplicitUnitLambdaParameter),
    ("lowers Unit lambda shorthand to one core lambda", testLowersUnitLambdaShorthand),
```

Add:

```haskell
testParsesUnitLambdaShorthand :: IO ()
testParsesUnitLambdaShorthand =
  assertEqual
    "Unit lambda shorthand AST"
    ( Right
        ( SEBlock
            [ SSLet
                "thunk"
                (SourceSpan 1 1)
                ( SELambda
                    (SurfaceLambdaPattern (SPTuple []) :| [])
                    (SELit (SLInt 42))
                )
            ]
        )
    )
    (parseSurfaceProgram "thunk = \\() -> 42.")

testParsesExplicitUnitLambdaParameter :: IO ()
testParsesExplicitUnitLambdaParameter =
  assertEqual
    "explicit Unit lambda AST"
    ( Right
        ( SEBlock
            [ SSLet
                "thunk"
                (SourceSpan 1 1)
                ( SELambda
                    (SurfaceLambdaPattern (SPTuple []) :| [])
                    (SELit (SLInt 42))
                )
            ]
        )
    )
    (parseSurfaceProgram "thunk = \\(()) -> 42.")

testLowersUnitLambdaShorthand :: IO ()
testLowersUnitLambdaShorthand =
  assertRight
    "parse + lower Unit lambda"
    (parseSurfaceProgram "thunk = \\() -> 42.")
    (\surfaceProgram -> assertEqual "lowered Unit lambda" expectedProgram (lowerSurfaceExpr surfaceProgram))
  where
    generatedName = "$lambda_pattern_arg_1"
    expectedProgram =
      EBlock
        [ SLet
            "thunk"
            (SourceSpan 1 1)
            ( ELambda
                generatedName
                ( EPatternCase
                    (EVar generatedName)
                    [CaseArm (PTuple []) Nothing (ELit (LInt 42))]
                )
            )
        ]
```

- [ ] **Step 2: Add a malformed trailing-comma regression**

Register and add:

```haskell
    ("rejects trailing comma after Unit lambda parameter", testRejectsTrailingCommaAfterUnitParameter),
```

```haskell
testRejectsTrailingCommaAfterUnitParameter :: IO ()
testRejectsTrailingCommaAfterUnitParameter =
  assertLeftDiagnosticContains
    "Unit lambda trailing comma"
    "expected"
    (parseSurfaceProgram "thunk = \\((),) -> 42.")
```

- [ ] **Step 3: Add failing semantic and runtime regressions**

Register these tests in `LambdaSemanticsSpec.hs`:

```haskell
    ("Unit lambda signature and repeated applications run", testUnitLambdaRuntime),
    ("Unit lambda rejects a non-Unit argument", testUnitLambdaTypeMismatch),
    ("Unit case pattern runs", testUnitCasePatternRuntime),
```

Add:

```haskell
testUnitLambdaRuntime :: IO ()
testUnitLambdaRuntime = do
  result <-
    runSource
      defaultWarningSettings
      "thunk :: () -> Int. thunk = \\() -> 42. (thunk (), thunk ())."
  assertEqual "warnings" [] (runWarnings result)
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "(42, 42)") (runOutput result)

testUnitLambdaTypeMismatch :: IO ()
testUnitLambdaTypeMismatch = do
  result <- compileSource defaultWarningSettings "thunk = \\() -> 42. thunk 1."
  assertSingleDiagnosticCode
    "Unit lambda type mismatch code"
    "E2006"
    (compileErrors result)
  case compileErrors result of
    compileError : _ ->
      assertContains
        "Unit lambda type rendering"
        "()"
        (renderDiagnostic compileError)
    [] ->
      failTest "expected Unit lambda type mismatch"

testUnitCasePatternRuntime :: IO ()
testUnitCasePatternRuntime = do
  result <- runSource defaultWarningSettings "case () { | () -> 42 }."
  assertEqual "warnings" [] (runWarnings result)
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "42") (runOutput result)
```

- [ ] **Step 4: Run the focused suites and verify the red state**

```bash
cabal test lambda-parser-spec lambda-semantics-spec --test-show-details=direct
```

Expected: FAIL because `parseLambdaParameters` still diagnoses the immediate `)` as a missing parameter. The explicit `\(())` and general Unit case tests may already pass after Task 1; the shorthand and end-to-end shorthand tests must fail.

- [ ] **Step 5: Parse `\()` as one Unit-pattern parameter**

Change the first case in `parseLambdaParameters` in `Parser.hs`:

```haskell
parseLambdaParameters tokensAfterLeftParen =
  case tokensAfterLeftParen of
    Token {tokenKind = TRParen} : rest ->
      Right (SurfaceLambdaPattern (SPTuple []) :| [], rest)
    _ -> do
      (firstParameter, afterFirstParameter) <- parseLambdaParameter tokensAfterLeftParen
      go firstParameter [] afterFirstParameter
```

Remove the old diagnostic branch for an immediate `TRParen`. Do not change `NonEmpty`, `lowerSurfaceLambda`, the core `ELambda`, or `EApply`.

- [ ] **Step 6: Run focused parser and semantic suites**

```bash
cabal test lambda-parser-spec lambda-semantics-spec --test-show-details=direct
```

Expected: both suites PASS. The signed Unit lambda produces `(42, 42)`, the non-Unit application reports E2006, and the Unit case pattern returns `42`.

- [ ] **Step 7: Run adjacent tuple and pattern suites**

```bash
cabal test expression-parser-spec pattern-parser-spec parser-foundation-spec adt-pattern-parser-spec adt-pattern-runtime-spec adt-pattern-type-spec --test-show-details=direct
```

Expected: all suites PASS; grouping, two-or-more-element tuples, pattern lambdas, and ADT behavior remain unchanged.

- [ ] **Step 8: Commit Unit lambda behavior**

```bash
git add \
  jazz-next/src/JazzNext/Compiler/Parser.hs \
  jazz-next/test/JazzNext/Compiler/Parser/LambdaParserSpec.hs \
  jazz-next/test/JazzNext/Compiler/Semantics/LambdaSemanticsSpec.hs
git commit -m "feat: add unary Unit lambda syntax"
```

---

### Task 3: Document Unit semantics and perform full verification

**Files:**
- Modify: `docs/spec/authoritative-syntax.md`
- Modify: `docs/spec/pattern-matching-semantics.md`
- Modify: `docs/spec/adt-pattern-semantics.md`
- Modify: `docs/spec/semantics/bindings-and-signatures.md`

**Interfaces:**
- Consumes: Tasks 1-2's tested syntax and semantics.
- Produces: authoritative documentation stating that `()` is the zero-tuple Unit and `\()` is unary Unit-pattern sugar, plus repository-wide verification evidence.

- [ ] **Step 1: Update authoritative syntax**

Add a concise paragraph near the existing canonical lambda/tuple implementation note in `authoritative-syntax.md`:

```markdown
`()` is the Unit value and zero-element tuple type. `\() -> expression` is
canonical shorthand for a lambda with one Unit-pattern parameter, equivalent
to `\(()) -> expression`; it is invoked through ordinary application as
`function ()`. Jazz does not have a distinct nullary function type or call
form.
```

- [ ] **Step 2: Update pattern and tuple semantics**

In `pattern-matching-semantics.md`, extend the tuple-pattern rules with:

```markdown
The zero-element tuple pattern `()` matches only the Unit value `()`, binds no
names, and participates in the same exact-arity tuple rule as non-empty tuple
patterns. A `\()` lambda lowers through this pattern rule as one unary
parameter; it is not a nullary lambda.
```

In `adt-pattern-semantics.md`, state that tuple values, signatures, and patterns include the zero-element Unit form while one-element parentheses remain grouping.

- [ ] **Step 3: Update supported signature documentation**

In `semantics/bindings-and-signatures.md`, add `()` to the supported concrete monomorphic signature grammar and state that it lowers to the zero-element tuple type rather than a new primitive.

- [ ] **Step 4: Run documentation and diff checks**

From the repository root:

```bash
bash scripts/check-docs.sh
bash scripts/check-execution-queue.sh
git diff --check
```

Expected: all checks PASS and `git diff --check` prints no output.

- [ ] **Step 5: Commit authoritative documentation**

```bash
git add \
  docs/spec/authoritative-syntax.md \
  docs/spec/pattern-matching-semantics.md \
  docs/spec/adt-pattern-semantics.md \
  docs/spec/semantics/bindings-and-signatures.md
git commit -m "docs: specify Jazz Unit semantics"
```

- [ ] **Step 6: Verify package metadata and build**

From `jazz-next/`:

```bash
cabal check
cabal build all
```

Expected: `cabal check` reports no errors or warnings; build succeeds.

- [ ] **Step 7: Run the complete Cabal suite**

```bash
cabal test all --test-show-details=never
```

Expected: all registered test suites PASS.

- [ ] **Step 8: Run the compatibility warning test runner**

From the repository root:

```bash
bash jazz-next/scripts/test-warning-config.sh
```

Expected: every existing spec program reports success and the command exits zero.

- [ ] **Step 9: Run a packaged CLI Unit-lambda smoke test**

From the repository root:

```bash
printf 'thunk :: () -> Int. thunk = \\() -> 42. thunk ().' | cabal run --project-dir=jazz-next jazz-next -- --run -
```

Expected output ends with:

```text
42
```

- [ ] **Step 10: Audit final scope and repository state**

```bash
git diff --check HEAD~3..HEAD
git status --short
git log --oneline -6
```

Expected: no whitespace errors, a clean worktree, three coherent Unit feature commits after the design/plan commits, and no changes under `jazz-hs/` or `jazz2/`.
