---
id: JN-OPERATORS-DECLARED-FUNCTION-BINDINGS-001
status: done
priority: P1
size: M
kind: impl
autonomous_ready: yes
depends_on:
  - JN-OPERATORS-STAGE2-FIXED-TIER-PARSER-001
last_verified: 2026-06-28
completed_on: 2026-06-28
plan_section: "Batch 1: Declared operator function bindings"
target_paths:
  - docs/spec/syntax/operators.md
  - jazz-next/src/JazzNext/Compiler/Driver.hs
  - jazz-next/src/JazzNext/Compiler/Identifier.hs
  - jazz-next/src/JazzNext/Compiler/Parser.hs
  - jazz-next/src/JazzNext/Compiler/RecursiveBindings.hs
  - jazz-next/src/JazzNext/Compiler/TypeInference.hs
  - jazz-next/src/JazzNext/Compiler/Runtime.hs
  - jazz-next/test/JazzNext/Compiler/Modules/LoaderSpec.hs
  - jazz-next/test/JazzNext/Compiler/Parser/OperatorFixitySpec.hs
  - jazz-next/test/JazzNext/Compiler/Parser/OperatorInvalidSyntaxSpec.hs
  - jazz-next/test/JazzNext/Compiler/Semantics/PrimitiveSemanticsSpec.hs
  - jazz-next/test/JazzNext/Compiler/Semantics/RecursiveBindingsSpec.hs
  - jazz-next/test/JazzNext/Compiler/Semantics/RuntimeSemanticsSpec.hs
verification:
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Modules/LoaderSpec.hs
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Parser/OperatorFixitySpec.hs
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Parser/OperatorInvalidSyntaxSpec.hs
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/PrimitiveSemanticsSpec.hs
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/RecursiveBindingsSpec.hs
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/RuntimeSemanticsSpec.hs
  - bash scripts/check-execution-queue.sh
  - bash scripts/check-docs.sh
  - git diff --check
deliverable: "Execute same-source declared user operators through explicit ordinary function bindings written as `(op) = <callable>.`, covering infix use, bare operator values, and left and right sections while preserving fixed-tier parsing, source-unit locality, no imports or exports for operator bindings, no custom precedence, no custom associativity, no new builtins, and no runtime overload dispatch."
---

# Jazz-Next Declared Operator Function Bindings Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use
> superpowers:subagent-driven-development (recommended) or
> superpowers:executing-plans to implement this plan task-by-task. Steps use
> checkbox (`- [ ]`) syntax for tracking.

**Goal:** make Stage 2 declared user operators executable in the same source
unit by binding each declared symbol to an ordinary callable value.

**Architecture:** keep the landed `operator <symbol> tier <1-5>.` declaration
as fixity metadata only, then add a narrow top-level or module-body binding
statement for an already-declared user operator: `(%%) = <expr>.` The parser
lowers that binding to a compiler-owned hidden ordinary binding, while type
inference and runtime resolve `EOperatorValue`, `EBinary`, and section nodes for
non-builtin operators through the same hidden binding. Builtin operators stay on
the existing primitive `VOperator` and `evalBinary` path.

**Tech Stack:** Haskell modules under `jazz-next/src/JazzNext/Compiler`, focused
`runghc` parser/type/runtime suites under `jazz-next/test/JazzNext/Compiler`,
the active operator syntax spec, and repo-root queue/docs validation.

---

## Source Evidence

- `docs/execution/queue.md` keeps `JN-USER-DEFINED-OPERATORS-PLAN-001` blocked
  because the completed Stage 2 child added declaration recognition and parser
  metadata only; runtime operator semantics remain separate.
- `docs/execution/blocker-contracts.md` asks the next operator unblocker to
  define how declared operators get executable bindings, while excluding custom
  precedence, custom associativity, new builtins, runtime overload dispatch, and
  re-promoting the parser-only child.
- `docs/spec/syntax/operators.md` says Stage 2 declarations are source-unit
  local parser and fixity metadata, and that executable semantics for declared
  user operators require a later child.
- `jazz-next/src/JazzNext/Compiler/Parser.hs` already tracks
  `DeclaredOperators` while parsing, permits declared symbols in infix,
  operator-value, and section positions after declaration, and consumes
  operator declarations without emitting AST statements.
- `jazz-next/src/JazzNext/Compiler/TypeInference.hs` and
  `jazz-next/src/JazzNext/Compiler/Runtime.hs` already support builtin
  `EOperatorValue`, `EBinary`, `ESectionLeft`, and `ESectionRight`; non-builtin
  operator symbols currently have no type or runtime binding path.

## Contract

Accepted source form:

```jazz
operator %% tier 2.
(%%) = \(left) -> \(right) -> left + right.
result = 1 %% 2.
```

Rules:

- `(op) = <expr>.` is allowed only at file scope or directly inside a module
  body, matching operator declarations. Nested block operator bindings are
  parser errors.
- `op` must name a user operator already declared earlier in the same source
  unit. Binding an undeclared operator or any builtin operator is a parser
  error.
- The binding expression is an ordinary Jazz expression. It must type as a
  callable value when used as an infix operator, bare operator value, or section.
- `left %% right` is equivalent to `((%%) left) right`.
- `(left %%)` is equivalent to `((%%) left)`.
- `(%% right)` is equivalent to `\left -> ((%%) left) right`, preserving the
  existing right-section argument order for non-commutative functions.
- `%%` has no import, export, re-export, or cross-module binding behavior in
  this child. A module can use its own declared and bound operator internally;
  callers must declare and bind their own source-unit-local operator symbols.
- This child does not add operator type signatures, implicit overload
  resolution, dictionaries, typeclass solver behavior, custom precedence,
  custom associativity, new builtins, or new operator declaration syntax.

## Target Paths

- `docs/spec/syntax/operators.md`: record the executable binding contract and
  the remaining Stage 2 non-goals.
- `jazz-next/src/JazzNext/Compiler/Identifier.hs`: add a compiler-owned hidden
  operator binding identifier helper so parser, type inference, and runtime use
  the same non-source binding key.
- `jazz-next/src/JazzNext/Compiler/Parser.hs`: parse `(op) = <expr>.` as a
  statement-level operator binding for already-declared user operators and lower
  it to an ordinary hidden `SSLet`.
- `jazz-next/src/JazzNext/Compiler/TypeInference.hs`: resolve non-builtin
  operator values, infix expressions, and sections through the hidden ordinary
  binding instead of the builtin operator rule table.
- `jazz-next/src/JazzNext/Compiler/Runtime.hs`: evaluate non-builtin operator
  values, infix expressions, and sections by applying the hidden ordinary
  binding.
- `jazz-next/test/JazzNext/Compiler/Parser/OperatorFixitySpec.hs`: prove the
  binding statement parses after declaration and does not alter fixed-tier
  infix parsing.
- `jazz-next/test/JazzNext/Compiler/Parser/OperatorInvalidSyntaxSpec.hs`: prove
  undeclared, builtin, and nested operator bindings are rejected.
- `jazz-next/test/JazzNext/Compiler/Semantics/PrimitiveSemanticsSpec.hs`: prove
  declared operator bindings typecheck through ordinary function types and
  reject missing or non-callable bindings.
- `jazz-next/test/JazzNext/Compiler/Semantics/RuntimeSemanticsSpec.hs`: prove
  declared operator infix use, bare operator values, and sections execute.

## Deliverable

Execute same-source declared user operators through explicit ordinary function
bindings written as `(op) = <callable>.`, covering infix use, bare operator
values, and left and right sections while preserving fixed-tier parsing,
source-unit locality, no imports or exports for operator bindings, no custom
precedence, no custom associativity, no new builtins, and no runtime overload
dispatch.

## Verification

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Parser/OperatorFixitySpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Parser/OperatorInvalidSyntaxSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Modules/LoaderSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/PrimitiveSemanticsSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/RecursiveBindingsSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/RuntimeSemanticsSpec.hs
bash scripts/check-execution-queue.sh
bash scripts/check-docs.sh
git diff --check
```

## Batch 1: Declared Operator Function Bindings

### Task 1: Lock Parser Behavior

**Files:**

- Modify: `jazz-next/test/JazzNext/Compiler/Parser/OperatorFixitySpec.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Parser/OperatorInvalidSyntaxSpec.hs`

- [x] **Step 1: Add accepted binding coverage**

Add parser coverage showing that a binding for an already-declared operator is
accepted and that the declared symbol still participates in the fixed-tier
precedence table:

```haskell
testParsesDeclaredOperatorFunctionBinding :: IO ()
testParsesDeclaredOperatorFunctionBinding =
  assertEqual
    "declared operator binding parses"
    (Right expectedProgram)
    (parseSurfaceProgram "operator %% tier 2.\n(%%) = \\(left) -> \\(right) -> left + right.\nresult = 1 %% 2 * 3.")
  where
    expectedProgram =
      SEBlock
        [ SSLet
            (mkOperatorBindingIdentifier "%%")
            (SourceSpan 2 2)
            ( SELambda
                [SurfaceLambdaIdentifier "left"]
                ( SELambda
                    [SurfaceLambdaIdentifier "right"]
                    (SEBinary "+" (SEVar "left") (SEVar "right"))
                )
            ),
          SSLet
            "result"
            (SourceSpan 3 1)
            ( SEBinary
                "%%"
                (SELit (SLInt 1))
                (SEBinary "*" (SELit (SLInt 2)) (SELit (SLInt 3)))
            )
        ]
```

Add `mkOperatorBindingIdentifier` and `SurfaceLambdaIdentifier` imports to the
suite when this test is added. The expected tree locks that parsing succeeds
only after the declaration and that `1 %% 2 * 3` keeps tier-2 behavior with `*`
binding tighter than `%%`.

- [x] **Step 2: Add invalid binding coverage**

Add parser failures for each invalid source:

```haskell
parseSurfaceProgram "(%%) = \\(left) -> \\(right) -> left + right."
-- expected: operator '%%' must be declared before binding

parseSurfaceProgram "(+) = \\(left) -> \\(right) -> left + right."
-- expected: cannot bind built-in operator '+'

parseSurfaceProgram "operator %% tier 2.\nx = { (%%) = \\(left) -> \\(right) -> left + right. 1 }."
-- expected: operator bindings are only allowed at file scope or directly in module bodies
```

- [x] **Step 3: Verify parser tests fail before implementation**

Run:

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Parser/OperatorFixitySpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Parser/OperatorInvalidSyntaxSpec.hs
```

Expected before implementation: at least the new accepted binding test fails
because statement-level `(op) =` is not parsed yet.

### Task 2: Add Hidden Operator Binding Identity

**Files:**

- Modify: `jazz-next/src/JazzNext/Compiler/Identifier.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Parser.hs`

- [x] **Step 1: Add one shared hidden identifier helper**

Add exports from `Identifier.hs`:

```haskell
mkOperatorBindingIdentifier,
operatorBindingIdentifierText
```

Implement them with a deterministic encoding that cannot collide with source
identifiers and never inherits impurity from a trailing operator `!`:

```haskell
operatorBindingIdentifierText :: Text -> Text
operatorBindingIdentifierText operatorSymbol =
  "$operator:" <> Text.concatMap encodeOperatorChar operatorSymbol
  where
    encodeOperatorChar char =
      let hexText = Text.pack (map toUpper (showHex (ord char) ""))
       in "%" <> Text.justifyRight 2 '0' hexText

mkOperatorBindingIdentifier :: Text -> Identifier
mkOperatorBindingIdentifier =
  mkIdentifier . operatorBindingIdentifierText
```

If `Text.concatMap`, `Text.justifyRight`, `ord`, `showHex`, or `toUpper`
imports are missing, add them locally in `Identifier.hs`. For example, `(%%)`
uses the hidden backing name `$operator:%25%25`; do not expose the hidden name
in user-facing parser syntax.

- [x] **Step 2: Parse statement-level `(op) = expr.`**

In `Parser.hs`, add a statement branch before generic expression statements:

```haskell
Token {tokenKind = TLParen} : operatorToken@(Token {tokenKind = TOperator operatorSymbol}) : Token {tokenKind = TRParen} : Token {tokenKind = TEquals} : rest ->
  fmap singleStatement (parseOperatorBinding context knownAliases declaredOperators operatorToken operatorSymbol rest)
```

`parseOperatorBinding` should:

- reject `NestedBlockContext`;
- reject builtin operators with `cannot bind built-in operator '<op>'`;
- require the symbol to exist in the current `declaredOperators`;
- parse the RHS with `parseExpr knownAliases declaredOperators`;
- consume the terminating dot;
- return `SSLet (mkOperatorBindingIdentifier operatorSymbol) (tokenSpan operatorToken) valueExpr`.

This keeps later phases on ordinary binding infrastructure and avoids a new
core statement form.

### Task 3: Type Non-Builtin Operators Through Ordinary Function Bindings

**Files:**

- Modify: `jazz-next/src/JazzNext/Compiler/TypeInference.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Semantics/PrimitiveSemanticsSpec.hs`

- [x] **Step 1: Add focused type tests**

Add tests that compile these sources:

```jazz
operator %% tier 2.
(%%) = \(left) -> \(right) -> left + right.
1 %% 2.
```

```jazz
operator %% tier 2.
(%%) = \(left) -> \(right) -> left == right.
(%%) 1 1.
```

Add failures for:

```jazz
operator %% tier 2.
1 %% 2.
```

Expected diagnostic text: `operator '%%' has no executable binding`.

```jazz
operator %% tier 2.
(%%) = 1.
1 %% 2.
```

Expected diagnostic: the existing generic non-function application diagnostic,
not a runtime-only failure.

- [x] **Step 2: Resolve non-builtin operator values from the type environment**

In `inferExprType`, keep builtin `EOperatorValue` behavior first. When
`instantiateOperatorType operatorSymbol` returns `Nothing`, look up
`operatorBindingIdentifierText operatorSymbol` in the current environment and
instantiate that ordinary binding. If no binding exists, add a deterministic
compile diagnostic:

```haskell
mkUnboundOperatorBindingError operatorSymbol =
  mkDiagnostic "E2010" ("operator '" <> operatorSymbol <> "' has no executable binding")
```

- [x] **Step 3: Type non-builtin infix and sections by applying the operator value**

For `EBinary operatorSymbol leftExpr rightExpr`, preserve the existing builtin
operator rule path. If `lookupOperatorRule operatorSymbol` returns `Nothing`,
infer the expression as:

```haskell
EApply (EApply (EOperatorValue operatorSymbol) leftExpr) rightExpr
```

For `ESectionLeft leftExpr operatorSymbol`, preserve the existing builtin
section rule path. If the operator is not builtin, infer:

```haskell
EApply (EOperatorValue operatorSymbol) leftExpr
```

For `ESectionRight operatorSymbol rightExpr`, preserve the existing builtin
section rule path. If the operator is not builtin, infer a fresh left argument
type and unify the operator binding type with:

```haskell
leftType -> rightType -> resultType
```

Return:

```haskell
leftType -> resultType
```

This gives right sections the same flipped argument order as builtin sections
without adding overload dispatch.

### Task 4: Execute Non-Builtin Operators Through Ordinary Runtime Bindings

**Files:**

- Modify: `jazz-next/src/JazzNext/Compiler/Runtime.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Semantics/RuntimeSemanticsSpec.hs`

- [x] **Step 1: Add runtime tests**

Add tests for these successful sources:

```jazz
operator %% tier 2.
(%%) = \(left) -> \(right) -> left + right.
1 %% 2.
```

Expected output: `3`.

```jazz
operator %% tier 2.
(%%) = \(left) -> \(right) -> left + right.
(%%) 1 2.
```

Expected output: `3`.

```jazz
operator %% tier 2.
(%%) = \(left) -> \(right) -> left - right.
((%%) 2) 10.
```

Expected output: `-8`.

```jazz
operator %% tier 2.
(%%) = \(left) -> \(right) -> left - right.
(%% 2) 10.
```

Expected output: `8`.

- [x] **Step 2: Evaluate non-builtin operator values from the runtime env**

In `evalValueWithModulePath`, preserve builtin `EOperatorValue` behavior. For
non-builtin symbols, look up `operatorBindingIdentifierText operatorSymbol` in
the current runtime environment, force qualified method values the same way
`EVar` does, and fail with the runtime counterpart only if typechecking was
bypassed.

- [x] **Step 3: Apply non-builtin infix and sections through the function path**

For non-builtin `EBinary`, evaluate the hidden binding, apply it to the left
value, then apply that result to the right value with `applyRuntimeFunction`.

For non-builtin `ESectionLeft`, evaluate the hidden binding and apply it to the
captured left value immediately, returning the resulting callable value.

For non-builtin `ESectionRight`, return a closure equivalent to:

```jazz
\left -> ((op) left) capturedRight
```

Use compiler-owned hidden identifiers in the closure environment so no source
identifier can collide with the captured operator function, captured right
value, or generated left parameter.

### Task 5: Update the Operator Spec and Run Verification

**Files:**

- Modify: `docs/spec/syntax/operators.md`

- [x] **Step 1: Record the binding contract**

Add a Stage 2 executable binding subsection that documents:

- `operator %% tier 2.` is still the declaration and fixity metadata;
- `(%%) = <callable>.` supplies the same-source executable binding;
- infix, bare operator value, left section, and right section equivalences;
- source-unit locality and no import/export behavior;
- non-goals for custom precedence, custom associativity, new builtins, overload
  dispatch, operator signatures, dictionaries, and solver expansion.

- [x] **Step 2: Run focused verification**

Run:

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Parser/OperatorFixitySpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Parser/OperatorInvalidSyntaxSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Modules/LoaderSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/PrimitiveSemanticsSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/RecursiveBindingsSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/RuntimeSemanticsSpec.hs
bash scripts/check-execution-queue.sh
bash scripts/check-docs.sh
git diff --check
```

Expected after implementation: all commands pass.
