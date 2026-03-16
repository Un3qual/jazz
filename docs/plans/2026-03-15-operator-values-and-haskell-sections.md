# Operator Values and Haskell-Style Sections Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Make operators first-class function values in `jazz-next` while preserving Haskell-style left/right section semantics and the existing precedence contract.

**Architecture:** Extend the parser with a bare operator-value form, keep left/right section syntax, and move infix plus left-section semantics toward ordinary operator-value application. Runtime and type inference should treat bare operators as callable curried values, while right sections stay explicit to preserve flipped-argument behavior for non-commutative operators.

**Tech Stack:** Haskell (`jazz-next` parser, AST, lowering, runtime, type inference, tests), Markdown docs, shell verification via `runghc` and `bash jazz-next/scripts/test-warning-config.sh`.

---

## Task 1: Lock Parser Expectations With Failing Tests

**Files:**
- Modify: `jazz-next/test/OperatorSectionSpec.hs`
- Modify: `jazz-next/test/OperatorFixitySpec.hs`
- Modify: `jazz-next/test/OperatorInvalidSyntaxSpec.hs`

**Step 1: Write the failing tests**

Add parser coverage for:

- `(+)` parsing as a dedicated operator-value node
- `(/)` parsing as a dedicated operator-value node
- `(+) 1 2` parsing as ordinary application of the operator value
- `(1 +)` still parsing as a left section
- `(+ 1)` still parsing as a right section
- `(1 + 2)` still parsing as a grouped infix expression
- `f x + g y * z` parsing with application tighter than infix and `*` tighter than `+`
- `f $ g x + h y` parsing with `$` lower than arithmetic/application

**Step 2: Run the targeted parser tests to verify they fail**

Run:

```bash
runghc -i./jazz-next/src -i./jazz-next/test jazz-next/test/OperatorSectionSpec.hs
runghc -i./jazz-next/src -i./jazz-next/test jazz-next/test/OperatorFixitySpec.hs
runghc -i./jazz-next/src -i./jazz-next/test jazz-next/test/OperatorInvalidSyntaxSpec.hs
```

Expected:

- New tests fail because bare operator values are not yet represented or parsed.

**Step 3: Do not change production code yet**

Keep the failures as evidence that the new behavior is not implemented.

**Step 4: Commit checkpoint**

```bash
git add jazz-next/test/OperatorSectionSpec.hs jazz-next/test/OperatorFixitySpec.hs jazz-next/test/OperatorInvalidSyntaxSpec.hs
git commit -m "test(parser): add operator-value and precedence parity coverage"
```

## Task 2: Lock Semantic Expectations With Failing Tests

**Files:**
- Modify: `jazz-next/test/PrimitiveSemanticsSpec.hs`
- Modify: `jazz-next/test/RuntimeSemanticsSpec.hs`

**Step 1: Write the failing tests**

Add semantic coverage for:

- `x = (+).` compiles
- `x = (+) 1 2.` compiles
- `x = ((+) 1) 2.` compiles
- `x = (+) 1 2.` runs to `3`
- `x = ((+) 1) 2.` runs to `3`
- `x = (1 +) 2.` still runs to `3`
- `x = (/ 2) 10.` runs to `5`
- `x = ((/) 2) 10.` runs to `0` under integer division
- type inference for bare operator values works through ordinary application

**Step 2: Run the targeted semantic tests to verify they fail**

Run:

```bash
runghc -i./jazz-next/src -i./jazz-next/test jazz-next/test/PrimitiveSemanticsSpec.hs
runghc -i./jazz-next/src -i./jazz-next/test jazz-next/test/RuntimeSemanticsSpec.hs
```

Expected:

- New tests fail because bare operator values do not exist yet.

**Step 3: Keep failures focused**

If failures come from parse rejection rather than runtime/type semantics, that is acceptable at this stage; the point is to prove the behavior is missing.

**Step 4: Commit checkpoint**

```bash
git add jazz-next/test/PrimitiveSemanticsSpec.hs jazz-next/test/RuntimeSemanticsSpec.hs
git commit -m "test(runtime): lock operator-value semantic parity"
```

## Task 3: Add Surface and Core Representation for Bare Operator Values

**Files:**
- Modify: `jazz-next/src/JazzNext/Compiler/Parser/AST.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/AST.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Parser/Lower.hs`

**Step 1: Add the minimal AST constructors**

Add one surface constructor and one core constructor for bare operator values, using a precise name such as:

- `SEOperatorValue Text`
- `EOperatorValue Text`

Do not remove existing binary or section constructors yet.

**Step 2: Run parser/lowering tests**

Run:

```bash
runghc -i./jazz-next/src -i./jazz-next/test jazz-next/test/OperatorSectionSpec.hs
```

Expected:

- Tests still fail because parsing is not wired up yet, but the code compiles far enough for parser work to proceed.

**Step 3: Keep lowering simple**

Lower the new surface node directly to the core node. Do not add semantic transformation here yet.

**Step 4: Commit checkpoint**

```bash
git add jazz-next/src/JazzNext/Compiler/Parser/AST.hs jazz-next/src/JazzNext/Compiler/AST.hs jazz-next/src/JazzNext/Compiler/Parser/Lower.hs
git commit -m "refactor(ast): add bare operator value nodes"
```

## Task 4: Parse Bare Operator Values Without Regressing Sections or Grouping

**Files:**
- Modify: `jazz-next/src/JazzNext/Compiler/Parser.hs`
- Inspect if needed: `jazz-next/src/JazzNext/Compiler/Parser/Operator.hs`

**Step 1: Implement the smallest parser change that can distinguish**

Support the following cases in `parseParenExpr`:

- `(+)` -> operator value
- `(+ 2)` -> right section
- `(2 +)` -> left section
- `(1 + 2)` -> grouped infix expression

Do not alter the existing precedence climber beyond what is required for this distinction.

**Step 2: Run parser tests**

Run:

```bash
runghc -i./jazz-next/src -i./jazz-next/test jazz-next/test/OperatorSectionSpec.hs
runghc -i./jazz-next/src -i./jazz-next/test jazz-next/test/OperatorFixitySpec.hs
runghc -i./jazz-next/src -i./jazz-next/test jazz-next/test/OperatorInvalidSyntaxSpec.hs
```

Expected:

- Parser tests pass.

**Step 3: Refactor only if needed**

If `parseParenExpr` becomes hard to read, extract a tiny helper for operator-paren classification. Do not rewrite unrelated parser paths.

**Step 4: Commit checkpoint**

```bash
git add jazz-next/src/JazzNext/Compiler/Parser.hs jazz-next/src/JazzNext/Compiler/Parser/Operator.hs jazz-next/test/OperatorSectionSpec.hs jazz-next/test/OperatorFixitySpec.hs jazz-next/test/OperatorInvalidSyntaxSpec.hs
git commit -m "feat(parser): support bare operator values"
```

## Task 5: Canonicalize Infix and Left Sections Toward Ordinary Operator Application

**Files:**
- Modify: `jazz-next/src/JazzNext/Compiler/TypeInference.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Desugar.hs` only if it remains part of the active path

**Step 1: Decide the active canonicalization point**

Use the already-active canonicalization path in `TypeInference.hs` unless another current path is more central.

**Step 2: Implement the minimal canonicalization**

Transform:

- `EBinary op a b` -> `EApply (EApply (EOperatorValue op) a) b`
- `ESectionLeft a op` -> `EApply (EOperatorValue op) a`

Keep:

- `ESectionRight op b` as a dedicated node
- `EOperatorValue op` as the representation for bare operator values

**Step 3: Run semantic tests**

Run:

```bash
runghc -i./jazz-next/src -i./jazz-next/test jazz-next/test/PrimitiveSemanticsSpec.hs
runghc -i./jazz-next/src -i./jazz-next/test jazz-next/test/RuntimeSemanticsSpec.hs
```

Expected:

- Some tests may still fail because runtime/type support for `EOperatorValue` is not implemented yet.

**Step 4: Commit checkpoint**

```bash
git add jazz-next/src/JazzNext/Compiler/TypeInference.hs jazz-next/src/JazzNext/Compiler/Desugar.hs
git commit -m "refactor(core): canonicalize infix and left sections via operator values"
```

## Task 6: Add Runtime Support for Bare Operator Values

**Files:**
- Modify: `jazz-next/src/JazzNext/Compiler/Runtime.hs`

**Step 1: Add a runtime representation for operator values**

Add a runtime callable form that can capture 0, 1, or 2 arguments for builtin operators.

Suggested shape:

- `VOperator Text [RuntimeValue]`

Keep it minimal and dispatch through the existing binary primitive evaluator when saturated.

**Step 2: Implement evaluation and application**

- evaluating `EOperatorValue "+"` yields `VOperator "+" []`
- applying `VOperator op []` captures the first argument
- applying `VOperator op [arg1]` captures the second argument and evaluates
- applying a fully saturated operator result through normal runtime rules must preserve deterministic failures

**Step 3: Preserve right-section behavior**

`VSectionRight` should still call `evalBinary` with flipped argument order for the supplied operand.

**Step 4: Run runtime tests**

Run:

```bash
runghc -i./jazz-next/src -i./jazz-next/test jazz-next/test/RuntimeSemanticsSpec.hs
```

Expected:

- Runtime tests pass, including the non-commutative distinction between `(/ 2)` and `((/) 2)`.

**Step 5: Commit checkpoint**

```bash
git add jazz-next/src/JazzNext/Compiler/Runtime.hs jazz-next/test/RuntimeSemanticsSpec.hs
git commit -m "feat(runtime): treat operators as first-class callable values"
```

## Task 7: Add Type Support for Bare Operator Values

**Files:**
- Modify: `jazz-next/src/JazzNext/Compiler/TypeInference.hs`

**Step 1: Add operator-value typing**

Give `EOperatorValue` a curried function type based on the existing operator rule table.

Examples:

- `+` -> `Int -> Int -> Int`
- `<` -> `Int -> Int -> Bool`
- `==` -> `t -> t -> Bool` with current equality restrictions preserved

**Step 2: Reuse the existing operator rule table**

Do not duplicate operator metadata in a second type-only table if the current rule path can be extended cleanly.

**Step 3: Re-run compile/type tests**

Run:

```bash
runghc -i./jazz-next/src -i./jazz-next/test jazz-next/test/PrimitiveSemanticsSpec.hs
```

Expected:

- Primitive semantics tests pass.

**Step 4: Commit checkpoint**

```bash
git add jazz-next/src/JazzNext/Compiler/TypeInference.hs jazz-next/test/PrimitiveSemanticsSpec.hs
git commit -m "feat(types): infer curried types for bare operator values"
```

## Task 8: Update Documentation to Match the Implemented Semantics

**Files:**
- Modify: `docs/jazz-language-state.md`
- Modify: `docs/spec/syntax/operators.md`

**Step 1: Update the docs**

Clarify:

- bare operator values are supported,
- `1 + 2` is semantically aligned with operator-value application,
- Haskell-style sections remain distinct from ordinary partial application for non-commutative operators.

**Step 2: Run a quick doc sanity check**

Run:

```bash
rg -n "operator identifiers can be used as functions|section semantics|ESectionLeft|ESectionRight" docs/jazz-language-state.md docs/spec/syntax/operators.md
```

Expected:

- Doc wording is aligned and no stale claims remain.

**Step 3: Commit checkpoint**

```bash
git add docs/jazz-language-state.md docs/spec/syntax/operators.md
git commit -m "docs(spec): document operator values and Haskell-style sections"
```

## Task 9: Full Verification

**Files:**
- No new files

**Step 1: Run focused suites**

Run:

```bash
runghc -i./jazz-next/src -i./jazz-next/test jazz-next/test/OperatorSectionSpec.hs
runghc -i./jazz-next/src -i./jazz-next/test jazz-next/test/OperatorFixitySpec.hs
runghc -i./jazz-next/src -i./jazz-next/test jazz-next/test/OperatorInvalidSyntaxSpec.hs
runghc -i./jazz-next/src -i./jazz-next/test jazz-next/test/PrimitiveSemanticsSpec.hs
runghc -i./jazz-next/src -i./jazz-next/test jazz-next/test/RuntimeSemanticsSpec.hs
```

Expected:

- All focused suites pass.

**Step 2: Run the project verification script**

Run:

```bash
bash jazz-next/scripts/test-warning-config.sh
```

Expected:

- Full `jazz-next` verification passes.

**Step 3: Review for incidental churn**

Run:

```bash
git diff --stat
git diff -- jazz-next/src/JazzNext/Compiler/Parser.hs jazz-next/src/JazzNext/Compiler/Runtime.hs jazz-next/src/JazzNext/Compiler/TypeInference.hs
```

Expected:

- Changes are limited to the planned parser/core/runtime/type/doc scope.

**Step 4: Final commit checkpoint**

```bash
git add docs/jazz-language-state.md docs/spec/syntax/operators.md \
  jazz-next/src/JazzNext/Compiler/AST.hs \
  jazz-next/src/JazzNext/Compiler/Parser/AST.hs \
  jazz-next/src/JazzNext/Compiler/Parser/Lower.hs \
  jazz-next/src/JazzNext/Compiler/Parser.hs \
  jazz-next/src/JazzNext/Compiler/Runtime.hs \
  jazz-next/src/JazzNext/Compiler/TypeInference.hs \
  jazz-next/test/OperatorSectionSpec.hs \
  jazz-next/test/OperatorFixitySpec.hs \
  jazz-next/test/OperatorInvalidSyntaxSpec.hs \
  jazz-next/test/PrimitiveSemanticsSpec.hs \
  jazz-next/test/RuntimeSemanticsSpec.hs
git commit -m "feat(jazz-next): make operators first-class values"
```
