# Jazz-Next Hosted Core Control Flow and Patterns Design

## Status

Approved in discussion on `2026-07-21` as canonical-core child 2.

The hosted canonical-core schema, checked stage-0 adapter, shared parity
harness, and expression-foundation lowerer are complete. This child extends
that internal hosted boundary through patterns, cases, conditionals, and
lambdas. It does not promote a production parser-to-core facade or change the
stage-0 Haskell compiler path.

## Goal

Make the Jazz-authored lowerer match
`JazzNext.Compiler.Parser.Lower.lowerSurfaceExpr` for every control-flow,
pattern, and lambda rule assigned to canonical-core child 2 while preserving
the exact unsupported boundary established by child 1.

The result must:

- lower every canonical pattern constructor;
- lower guarded and unguarded case arms recursively;
- lower conditionals and nested control flow;
- lower multi-parameter lambdas into nested unary lambdas;
- desugar pattern parameters through exact generated names and one-arm pattern
  cases; and
- return `Nothing` for an entire tree whenever any nested form belongs to a
  later child.

## Chosen Approach

Use one private, profile-driven recursive lowering kernel in `CoreLower.jz`.

The existing `lowerFoundationExpression` entry point remains available and
continues to reject all control-flow and lambda constructors. A new
`lowerControlFlowPatternsExpression` entry point enables child-2 forms while
sharing all child-1 literal, collection, application, operator, section,
block, and statement logic.

Two alternatives were rejected:

1. Expanding `lowerFoundationExpression` in place would erase the accepted
   child-1 boundary and make its name and regression suite misleading.
2. Copying the foundation lowerer into a second implementation would create
   two recursive definitions whose shared semantics could drift.

The profile is private implementation state, not a new public compiler
contract. Both exported entry points remain pure and return `Maybe CoreExpr`.

## Owned Semantics

### Patterns

Pattern lowering is structural and total over the existing `SurfacePattern`
schema:

- wildcard;
- source-name variable;
- literal, including every canonical literal representation;
- source-name constructor with recursively lowered arguments;
- exact list;
- cons-like list;
- tuple, including unit;
- source-name as-pattern; and
- ordered or-pattern alternatives.

Pattern lowering performs no binder validation, exhaustiveness analysis, type
checking, or match execution. Those remain later compiler responsibilities.

### Conditionals and cases

An `IfExpression` lowers its condition and both branches recursively under the
same child-2 profile. If any subtree is deferred, the whole expression returns
`Nothing`.

A `CaseExpression` lowers its scrutinee and every arm in source order. Each arm
preserves its pattern, optional guard, and body. Guards and bodies use the same
recursive expression boundary; one deferred guard or body rejects the entire
case. Empty direct-AST arm lists remain representable because stage 0 lowers
surface values structurally even though the hosted parser rejects empty case
syntax.

### Lambdas and generated names

Lambda parameters are processed in source order and emitted as nested unary
`CoreLambdaExpression` values by folding from the final body toward the first
parameter.

Identifier parameters use `CoreSourceName` directly. A pattern parameter at
one-based source position `n` uses exactly
`CoreGeneratedName (CoreLambdaPatternArgument n)` for both:

- the unary lambda binder; and
- the variable scrutinized by its one-arm `CorePatternCaseExpression`.

The generated index is the parameter's original position, including preceding
identifier parameters. Each generated name is structured data; no rendered
prefix or text encoding participates in equality.

## Preserved Unsupported Boundary

The new entry point continues to return `Nothing`, without partial core output,
for every later-child form at any depth:

- explicit type application;
- binary `$` application desugaring;
- signatures;
- data, class, and impl declarations;
- operator-storage bindings and signatures;
- module and import statements; and
- any block, lambda, condition, case scrutinee, guard, arm body, collection, or
  application containing one of those forms.

These forms are scheduled rather than abandoned. Canonical-core child 3 owns
explicit type application, `$`, signatures, declarations, and operator-storage
names. Child 4 owns module/import extraction, exports, path validation, span
qualification, structured module failures, and the production composed facade.
Type inference, runtime integration, lowered IR, LLVM, object/link production,
and the native runtime remain later separately reviewed milestones.

## Fixed Parity Contract

The positive direct family contains exactly 18 stable fixtures:

1. `if-basic`
2. `if-nested`
3. `if-collection-branches`
4. `if-block-branches`
5. `case-empty-arms`
6. `case-pattern-inventory`
7. `case-guarded`
8. `case-nested-scrutinee`
9. `case-nested-body`
10. `lambda-identifier`
11. `lambda-identifiers-multiple`
12. `lambda-pattern-wildcard`
13. `lambda-pattern-composite`
14. `lambda-pattern-or`
15. `lambda-mixed-parameters`
16. `lambda-two-pattern-parameters`
17. `lambda-nested-control-flow`
18. `block-control-flow`

`case-pattern-inventory` includes wildcard, variable, integer, fractional,
boolean, character, text, constructor, exact-list, cons-list, tuple, as-, and
or-pattern values. The mixed and two-pattern lambda fixtures prove that
generated indices reflect original one-based positions rather than pattern-only
positions.

The composed family contains exactly these 14 successful hosted-parser sources:

```text
if True then 1 else 0.
if outer then if inner then 1 else 2 else 3.
if cond then { value = 1. value. } else { value = 2. value. }.
case value { | Just item -> item | Nothing -> 0 }.
case value { | Just item if keep -> item | Nothing -> 0 }.
case value { | _ -> 0 | name -> 1 | 2 -> 2 | 'x' -> 3 | "x" -> 4 | True -> 5 | Just item -> 6 | [head, tail] -> 7 | [head | tail] -> 8 | () -> 9 | (left, right) -> 10 | whole@Nothing -> 11 | Just item | Nothing -> 12 }.
case if cond then left else right { | _ -> 0 }.
case outer { | Just item -> case item { | _ -> 1 } | Nothing -> 0 }.
\(value) -> value.
\(left, right) -> left.
\([head | tail]) -> head.
\(Just item | Nothing) -> item.
\(first, Just second, third) -> second.
{ loop = \(value) -> case value { | Just next -> loop next | _ -> if False then value else value }. loop. }.
```

The direct unsupported family contains exactly 12 stable fixtures:

1. `type-application-root`
2. `type-application-condition`
3. `type-application-case-scrutinee`
4. `type-application-case-guard`
5. `type-application-lambda-body`
6. `dollar-case-body`
7. `signature-if-block`
8. `data-case-block`
9. `class-lambda-block`
10. `impl-lambda-block`
11. `operator-storage-nested-block`
12. `module-import-nested-block`

Together they place later forms at the root or beneath a condition, case
scrutinee, case guard, case body, lambda body, or block statement. The final
fixture includes both statement forms so the fixed family covers type
application, `$`, signature, data, class, impl, operator-storage binding,
module, and import ownership without expanding past 12 cases.

Every direct and composed positive family is compared with stage 0 twice. The
unsupported family is also run twice and must produce only `Nothing`, with no
compile or runtime errors. Existing child-1 fixtures remain unchanged and must
continue to pass.

## Harness and File Boundaries

Implementation remains limited to:

- `jazz-next/jazz/compiler/CoreLower.jz`;
- `jazz-next/test/JazzNext/Compiler/Bootstrap/JazzCoreParity.hs`;
- new
  `jazz-next/test/JazzNext/Compiler/Bootstrap/JazzCoreControlFlowPatternsSpec.hs`;
- `jazz-next/jazz-next.cabal`; and
- queue, blocker, plan, archive, and status documentation needed to promote and
  close this exact child.

`CoreTypes.jz` already represents every required child-2 value. It may receive
only contract clarification or regression assertions; this child does not
redesign its schema.

The shared Haskell harness may generalize its batch runner so both entry points
reuse module generation and checked-in source lookup. It must continue to
derive expected values by calling stage-0 `lowerSurfaceExpr` and then applying
the checked structural adapter. It must not reproduce lowering decisions.

## Verification

Required focused verification is:

- `jazz-core-control-flow-patterns-spec`;
- `jazz-core-expression-foundation-spec`;
- `canonical-core-comparison-spec`;
- `canonical-parser-comparison-spec`;
- `jazz-parser-control-flow-patterns-spec`; and
- `repository-audit-spec`.

Closeout also requires a warning-clean development build, routine non-exhaustive
Cabal `all`, `cabal check`, queue/docs validators, and `git diff --check`.

The opt-in exhaustive parser scale components are explicitly excluded. This
child neither changes parser behavior nor needs exhaustive scale evidence.

## Completion Boundary

This child is complete when both hosted entry points preserve their distinct
ownership profiles, all fixed families match stage 0 repeatedly, nested
unsupported forms fail all-or-nothing, and the queue archives only
`JN-BOOTSTRAP-JAZZ-CORE-CONTROL-FLOW-PATTERNS-001`.

Completion promotes no declaration, module, production-facade, or backend
work. Canonical-core child 3 becomes the sole next curation candidate.
