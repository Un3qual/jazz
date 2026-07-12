# Jazz Required `then` Keyword Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make `if <condition> then <trueBranch> else <falseBranch>` the only accepted Jazz conditional syntax.

**Architecture:** Add `then` to the shared lexical vocabulary first, including the Jazz-authored canonical lexer contract, then use that explicit token as the parser boundary. Keep `SEIf`, `EIf`, typing, runtime evaluation, and future backend lowering unchanged; finish by migrating all active Jazz sources and embedded Jazz programs.

**Tech Stack:** Haskell, Megaparsec, Cabal, Jazz `.jz` standard-library modules

## Global Constraints

- Implement new compiler behavior only under `jazz-next/`.
- Do not modify `jazz-hs/` or `jazz2/`.
- Reject the old conditional syntax immediately; do not add compatibility parsing or deprecation warnings.
- Preserve two-space indentation in every `.jz` file.
- Preserve `SEIf` and `EIf` representation and semantics.

---

### Task 1: Reserve `then` in both canonical lexers

**Files:**
- Modify: `jazz-next/src/JazzNext/Compiler/Parser/Lexer.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Parser/TokenParser.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Parser/TokenParserSpec.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Parser/FixtureCorpus.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Bootstrap/CanonicalLexerComparison.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Bootstrap/CanonicalLexerComparisonSpec.hs`
- Modify: `jazz-next/stdlib/LexerTypes.jz`
- Modify: `jazz-next/stdlib/Lexer.jz`
- Modify: `jazz-next/test/JazzNext/Compiler/Bootstrap/JazzLexerParitySpec.hs`

**Interfaces:**
- Consumes: `TokenKind`, `CanonicalKeyword`, `CanonicalTokenKind`, and `identifierKind` in both lexer implementations.
- Produces: Haskell `TThen` and bootstrap `ThenKeyword`, with identical lexeme and source-span behavior.

- [ ] **Step 1: Add failing Haskell and canonical parity expectations**

Add a `TokenParserSpec` case that tokenizes `"if condition then yes else no"` and expects:

```haskell
[TIf, TIdentifier "condition", TThen, TIdentifier "yes", TElse, TIdentifier "no"]
```

Add `then` and `KeywordKind(ThenKeyword)` to the all-token fixture and canonical constructor expectations. Add exact Jazz parity input containing `then` so the Jazz-authored lexer must agree.

- [ ] **Step 2: Run the focused tests and verify RED**

Run:

```bash
cabal test --project-dir=jazz-next token-parser-spec canonical-lexer-comparison-spec jazz-lexer-parity-spec --test-show-details=failures
```

Expected: compilation or assertion failure because `TThen` and `ThenKeyword` do not exist and `then` is currently an identifier.

- [ ] **Step 3: Implement the lexical vocabulary**

Add `TThen` between `TIf` and `TElse`, map `"then" -> TThen`, and render it as `"'then'"` in token diagnostics. Add and export `ThenKeyword` in `LexerTypes.jz`, then map the exact identifier in `Lexer.jz`:

```jz
else if name == "then"
  KeywordKind ThenKeyword
```

Add `ThenKeyword` to the Haskell canonical comparison ADT and map `TThen` to it.

- [ ] **Step 4: Run the focused tests and verify GREEN**

Run the Task 1 Cabal command again. Expected: all three test suites pass.

- [ ] **Step 5: Commit the lexical contract**

```bash
git add jazz-next/src/JazzNext/Compiler/Parser/Lexer.hs jazz-next/src/JazzNext/Compiler/Parser/TokenParser.hs jazz-next/test/JazzNext/Compiler/Parser/TokenParserSpec.hs jazz-next/test/JazzNext/Compiler/Parser/FixtureCorpus.hs jazz-next/test/JazzNext/Compiler/Bootstrap/CanonicalLexerComparison.hs jazz-next/test/JazzNext/Compiler/Bootstrap/CanonicalLexerComparisonSpec.hs jazz-next/stdlib/LexerTypes.jz jazz-next/stdlib/Lexer.jz jazz-next/test/JazzNext/Compiler/Bootstrap/JazzLexerParitySpec.hs
git commit -m "feat: reserve then in Jazz lexers"
```

### Task 2: Require the explicit parser boundary

**Files:**
- Modify: `jazz-next/src/JazzNext/Compiler/Parser/Expression.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Parser/IfExpressionParserSpec.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Parser/ExpressionParserSpec.hs`

**Interfaces:**
- Consumes: `TThen` from Task 1 and the existing `Stop = [Token] -> Bool` expression boundary.
- Produces: `parseIfExpr` accepting only the canonical form while still returning `SEIf`.

- [ ] **Step 1: Change parser tests to the canonical syntax**

Use sources such as:

```jz
x = if True then 1 else 2.
x = if cond then if inner then a else b else c.
x = if f value then yes else no.
```

Add a regression asserting `x = if cond yes else no.` fails with `expected 'then'`. Keep the missing-`else`, extra-`else`, lowering, and canonical-AST assertions.

- [ ] **Step 2: Run the parser suite and verify RED**

Run:

```bash
cabal test --project-dir=jazz-next if-expression-parser-spec expression-parser-spec --test-show-details=failures
```

Expected: canonical forms fail because `parseIfExpr` still treats `then` as the start of a branch, and the old form remains accepted.

- [ ] **Step 3: Implement the explicit `then` boundary**

Define a focused stop predicate:

```haskell
thenStarts :: Stop
thenStarts tokens =
  case tokens of
    Token {tokenKind = TThen} : _ -> True
    _ -> False
```

Parse the condition with the ordinary application-capable expression parser up to `then`, require `parseToken TThen`, then parse the true branch and require the existing `else`. Emit a focused expected-`then` diagnostic for end-of-input or another encountered token. Do not retain `parseExprWithoutApplicationUntil` if it has no remaining callers.

- [ ] **Step 4: Run the parser suites and verify GREEN**

Run the Task 2 Cabal command again. Expected: both suites pass.

- [ ] **Step 5: Commit the parser behavior**

```bash
git add jazz-next/src/JazzNext/Compiler/Parser/Expression.hs jazz-next/test/JazzNext/Compiler/Parser/IfExpressionParserSpec.hs jazz-next/test/JazzNext/Compiler/Parser/ExpressionParserSpec.hs
git commit -m "feat: require then in Jazz conditionals"
```

### Task 3: Migrate active Jazz source and specification evidence

**Files:**
- Modify: `docs/spec/control-flow/if-expressions.md`
- Modify: `jazz-next/stdlib/Char.jz`
- Modify: `jazz-next/stdlib/Lexer.jz`
- Modify: `jazz-next/test/JazzNext/CLI/CLISpec.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Modules/Loader/CapabilitiesTests.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Parser/AdtPattern/PatternsTests.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Parser/FixtureCorpus.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Semantics/AdtPatternTypeSpec.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Semantics/BindingSignature/ConstraintsTests.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Semantics/BindingSignature/DiagnosticsTests.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Semantics/BindingSignature/RecursionTests.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Semantics/CoreNormalizationSpec.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Semantics/IfExpressionTypeSpec.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Semantics/LambdaSemanticsSpec.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Semantics/Runtime/CapabilitiesTests.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Semantics/Runtime/ControlFlowTests.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Semantics/Runtime/RecursionTests.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Semantics/Runtime/RenderingTests.hs`

**Interfaces:**
- Consumes: canonical grammar from Task 2.
- Produces: an active compiler tree with no old-form conditionals and documentation matching executable behavior.

- [ ] **Step 1: Enumerate remaining old-form sources**

Run:

```bash
rg -n --glob '*.jz' '\bif\b' jazz-next
rg -n 'if .* else' jazz-next/test
```

Classify each match as Jazz source syntax or Haskell implementation prose/code. Change only Jazz sources, embedded Jazz strings, and syntax documentation.

- [ ] **Step 2: Migrate sources minimally**

Insert `then` after each complete condition without changing branch structure. Preserve the existing two-space indentation in `Lexer.jz`, `Char.jz`, and any other active Jazz file. Update embedded programs in parser, semantic, runtime, loader, and bootstrap tests.

- [ ] **Step 3: Update the canonical specification**

Change the grammar and examples in `docs/spec/control-flow/if-expressions.md` to require `then`. Remove the obsolete no-application boundary language and state that the condition is a full expression ending at `then`.

- [ ] **Step 4: Run focused and full verification**

Run:

```bash
cabal test --project-dir=jazz-next token-parser-spec canonical-lexer-comparison-spec jazz-lexer-parity-spec if-expression-parser-spec expression-parser-spec if-expression-type-spec --test-show-details=failures
bash jazz-next/scripts/test-warning-config.sh
bash scripts/check-docs.sh
git diff --check
```

Expected: every command exits zero; all registered Jazz tests pass; docs and queue checks pass; no whitespace errors remain.

- [ ] **Step 5: Verify no active old syntax remains**

Review every `if` match under `jazz-next/` and confirm every Jazz conditional contains `then`. Confirm `git diff --stat` contains no changes under `jazz-hs/` or `jazz2/`.

- [ ] **Step 6: Commit the migration**

```bash
git add docs/spec/control-flow/if-expressions.md jazz-next
git commit -m "refactor: migrate Jazz conditionals to then"
```

- [ ] **Step 7: Push the stacked branch**

```bash
git push -u origin codex/require-then-keyword
```

Expected: the remote branch is created on top of `codex/bootstrap-jazz-authored-lexer` without modifying PR 105.
