# Final review fix report

## Status

DONE

Base: `bf3f87dc1774e80e7ad9fc2eed5f9c5f989c2f9d`

Implementation commits:

- `a362d5ac06d4d2a082316d2f7af9f37e44cda419` — fix typed-core direct lambda recipe validation
- `31c674bad38fe1363c3b2534e936a332142b5daa` — fix recursive pattern binder ownership

## Finding 1: flattened direct leading-lambda recipes

### Regression and RED

The fixture inventory now includes the accepted source fixture `three-argument-direct-call` with exact typed-core and lowered-program expectations for:

```jazz
sumThree :: Int -> Int -> Int -> Int.
sumThree = \(first, second, third) -> first + second + third.
sumThree 10 20 12.
```

The typed expectation deliberately gives the second leading lambda the remaining flattened recipe `[Int, Int] -> Int`, distinguishing a direct leading-lambda chain from recursively staged unary closure recipes.

Before production changes, the focused direct-call suite failed at the new assertion with `TypedCallableRecipeMismatch` at module `App.Main`, statement `[1]`, expression `[0,0]`: validation expected the staged recipe `[Int] -> ([Int] -> Int)` but received the valid remaining flattened direct recipe `[Int, Int] -> Int`.

The Haskell/hosted contract fixture was also expanded to a three-lambda direct declaration. Before the mirror fix, its Haskell validation failed at statement `[0]`, expression `[0,0]` for the same recipe mismatch; once the Haskell side advanced, hosted parity exposed the unchanged Jazz mirror until it was fixed.

### Implementation

Both validators now derive the leading-lambda recipe rule from the declaration or impl-method scheme's callable shape:

- `TypedDirectCallableShape` carries the flattened-recipe exemption through only consecutive leading lambda-body edges.
- Closure-shaped declarations, anonymous lambdas, and non-lambda child edges require staged unary recipes.
- Impl methods use the same scheme-owned decision.
- Haskell and Jazz use the same decision and traversal shape.

The existing malformed anonymous, nested, and closure-shaped flattened-recipe cases remain rejected. The closure-shaped named malformed fixture now also records the root callable-recipe mismatch that the corrected direct/closure distinction exposes.

### GREEN

The following focused targets passed after both mirrors were updated:

```text
nix --extra-experimental-features 'nix-command flakes' develop --command cabal test jazz-typed-core-expression-direct-call-spec --test-show-details=failures --jobs=1
nix --extra-experimental-features 'nix-command flakes' develop --command cabal test jazz-typed-core-contract-spec --test-show-details=failures --jobs=1
```

## Finding 2: pattern-binder-aware callable ownership

### Regression and RED

The canonical recursion suite and runtime semantics suite now encode the required witness:

```jazz
f = {
  apparent = \(x) -> x.
  captured = \(x) -> f.
  case True { | apparent -> apparent }.
}.
f.
```

Before production changes:

- `recursive-bindings-spec` failed the canonical assertion with `expected fromList [], got fromList [(0,[0])]`.
- `runtime-semantics-spec` failed with `pattern-binder witness is not a runtime recursive group: expected False, got True`.

The runtime regression proves all three relevant properties after the fix: statement 0 is not in a recursive group, it has no self-recursive-function visibility, and source execution completes without compile/runtime diagnostics and returns `True`.

### Implementation

`RecursiveBindings.exprContainsFunctionBranch` now threads a bound-name set through its traversal and extends that set with every case-arm pattern before examining the arm body. A bound arm variable therefore cannot resolve through an earlier same-named scope binding. The canonical predicate also explicitly traverses `ETypeApplication`, preserving the behavior that had existed only in the runtime duplicate.

`Runtime.ScopePlan` now imports and re-exports the canonical predicate instead of maintaining its own semantically divergent implementation.

### GREEN and mutation evidence

After implementation, both focused targets passed. A compile-valid mutation then changed the case-arm destructuring binder to `_` and passed the unextended `boundNames` set into the body traversal, removing only the pattern-binder exclusion. With that mutation:

- `recursive-bindings-spec` compiled and failed only the new canonical assertion, again reporting the false `(0,[0])` group.
- `runtime-semantics-spec` compiled and failed only the new runtime-plan assertion, again reporting `expected False, got True`.

The binder-aware traversal was restored, and both focused targets passed again.

## Final serialized verification

The mandated source verification was run one process at a time, in order, with `--jobs=1`:

1. `recursive-bindings-spec` — PASS
2. `runtime-semantics-spec` — PASS
3. `jazz-typed-core-expression-direct-call-spec` — PASS
4. `jazz-typed-core-contract-spec` — PASS
5. `git diff --check bf3f87dc1774e80e7ad9fc2eed5f9c5f989c2f9d..HEAD` — PASS

No full Cabal suite/build, `scripts/ci/main-functional.sh`, `nix flake check`, or other broad memory-heavy gate was run.

## Scope and ownership audit

Before adding this report, the base-to-head range contained only these nine intended files:

```text
jazz/compiler/TypedCoreValidate.jz
src/Jazz/Compiler/RecursiveBindings.hs
src/Jazz/Compiler/Runtime/ScopePlan.hs
src/Jazz/Compiler/TypedCore/Validate.hs
test/Jazz/Compiler/Bootstrap/JazzTypedCoreContractSpec.hs
test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures.hs
test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec.hs
test/Jazz/Compiler/Semantics/RecursiveBindingsSpec.hs
test/Jazz/Compiler/Semantics/Runtime/RecursionTests.hs
```

The pinned project-status snapshot was not edited. `expressionDependencyNames` remains exported, implemented, and covered by its existing direct-call-spec tests.

An optional whole-file Ormolu check performed during Finding 1 reported pre-existing formatting differences outside this fix's changed hunks. No unrelated bulk formatting was applied; the required range `git diff --check` is clean.

## Concerns

None within the required scope. The intentionally excluded broad gates remain unrun, and the optional out-of-scope whole-file formatting differences remain untouched.

## Round 1 re-review: definition-site bound-name ownership

### Finding

The first pattern-binder fix correctly stopped an arm-bound name from resolving through an earlier same-named callable, but `exprContainsFunctionBranch` then carried that arm's `boundNames` backward when following any scope alias. An alias initializer defined before the case arm therefore lost its own lexical environment.

The concrete witness is:

```jazz
f = {
  target = \(x) -> f.
  alias = target.
  case True { | target -> alias }.
}.
f.
```

Here the arm pattern binds only the arm body's `target`. The `target` reference in the earlier `alias = target` initializer must resolve to the earlier lambda.

### TDD RED

The canonical regression encodes the witness directly and expects the legitimate singleton recursive group `(0,[0])`. Before the production fix, the serialized focused run failed only the new assertion:

```text
pattern-bound use site does not hide an alias initializer's prior callable:
expected fromList [(0,[0])], got fromList []
```

The runtime regression builds a scope plan from the same witness, requires both recursive-group and self-recursive-function visibility, and runs a finite source analogue that forces the recursive closure. Before the production fix, the serialized runtime run failed only the new plan assertion:

```text
definition-site witness is a runtime recursive group: expected True, got False
```

Commands:

```text
nix --extra-experimental-features 'nix-command flakes' develop --command cabal test recursive-bindings-spec --test-show-details=failures --jobs=1
nix --extra-experimental-features 'nix-command flakes' develop --command cabal test runtime-semantics-spec --test-show-details=failures --jobs=1
```

### Implementation

Each `ScopeBindingExpr` now retains the bound-name set from its initializer's definition site. `scopeStatementContexts` captures that environment when it records a scope binding. Every binding-follow path restores the recorded environment instead of inheriting the alias use site's environment:

- canonical `exprContainsFunctionBranch` callable recognition;
- recursive-cell alias ownership analysis;
- non-alias reference ownership analysis.

The case-arm body still receives its extended pattern-bound environment, preserving the original false-positive fix. Only traversal into an earlier binding initializer switches back to the initializer's recorded environment.

### GREEN and mutation evidence

After the implementation, both focused suites passed. The executable runtime source completed without compile/runtime diagnostics and returned `0`, proving that the resulting closure received legitimate recursive visibility.

A compile-valid mutation then changed only canonical scope-alias traversal back to the alias use-site `boundNames` environment. Under that mutation:

- `recursive-bindings-spec` compiled and again failed with `expected fromList [(0,[0])], got fromList []`;
- `runtime-semantics-spec` compiled and again failed with `expected True, got False` for recursive runtime-group visibility.

The definition-site traversal was restored, and both focused suites passed again. No broad/full Cabal build or suite, main functional gate, or Nix flake gate was run.
