---
id: JN-OPERATORS-AS-FUNCTIONS-001
status: complete
priority: P1
size: L
kind: impl
autonomous_ready: yes
depends_on: []
plan_section: "Implementation"
target_paths:
  - jazz/stdlib/Prelude.jz
  - src/Jazz/Compiler/ModuleResolver/Names.hs
  - src/Jazz/Compiler/TypeInference.hs
  - src/Jazz/Compiler/Runtime/Primitives.hs
  - test/Jazz/Compiler/Stdlib/FoundationsTests.hs
  - test/Jazz/Compiler/Stdlib/OperatorFunctionsTests.hs
verification:
  - "cabal test all -f-full-parser-scale --jobs=1 --test-show-details=failures"
  - "bash scripts/ci/haskell-quality.sh"
  - "bash scripts/check-docs.sh"
deliverable: "Boolean not and one function path for operators"
last_verified: 2026-09-14
---

# Operators as functions

The maintainer requests ordinary Boolean `not` and one implementation for both
operator and named function spellings. On 2026-09-14 the maintainer explicitly
chose ordinary function rules, including explicit mixed numeric conversion and
Equatable evidence for ADTs, over the previous operator-only exceptions.

## Design

Keep parsing and precedence. Resolve `==` to `equals`, `!=` to `differs`,
`<`, `<=`, `>`, `>=` to `lessThan`, `lessThanOrEqual`, `greaterThan`,
`greaterThanOrEqual`; arithmetic maps to `add`, `subtract`, `multiply`, `divide`.
`$` is application, with `apply` as its callable spelling. Resolve these names in
the same lexical scope as written calls. Existing source-local operator bindings
retain their current declaration and transport rules. `|` remains pattern syntax.

`not` is a Prelude function, not a keyword. Arithmetic methods belong to the
existing Num class. Comparison helpers use Comparable.compare. Primitive scalar
implementations call kernel functions, never their own operator aliases. Lists
and tuples use their existing element-aware Equatable instances; no implicit
structural ADT equality or numeric coercion is introduced. Kernel arithmetic and
equality retain only their primitive validation and runtime implementation.

Reuse the existing user-operator desugaring for infix expressions, values and
sections, preserving captured operand evaluation, lexical scope and source spans.
Remove the built-in operator dispatch, alias schemes and checked operand metadata
once ordinary calls own all executable operator uses. Hosted parser/lowering
continues to represent surface notation; parity tests must verify the same
frontend contract. Migrate maintained programs and tests to explicit instances
or conversions where they relied on the deliberately removed behavior.

## Implementation

- [x] Add and verify not, including higher-order use; simplify Boolean negations.
- [x] Add ordinary operator functions and primitive implementations; desugar all
      executable operators through normal resolution and evidence.
- [x] Remove replaced operator-only inference, runtime, and metadata paths.
- [x] Migrate maintained consumers, tests and public contracts.
- [x] Verify focused behavior, full regular suites, clean quality gate and docs;
      commit coherent batches and close the queue row.

## Implementation and verification receipt

Boolean `not` is committed in `40f78a07`; ordinary dispatch for all eleven
executable operators and removal of the replaced compiler/runtime paths is
committed in `7a52c11c`. Arithmetic, equality, ordering, operator values, sections,
and `$` share lexical resolution, inference and evidence with their named calls.
The migration also fixed nested recursive method evidence and retained scalar
recursive-cycle diagnostics after desugaring.

All 62 regular suites passed: the complete matrix followed by successful reruns
of two fixtures updated for the intentional contract changes. All 17 corpus
programs retain their expected output and pass their measured work budgets.
All four full parser-scale workloads passed. Both production-only and whole-tree
Weeder checks passed from a fresh HIE build, with tests added only after the
production check. HLint, Ormolu, package checks, generated invariants, public
documentation, examples, snippets, and queue checks passed. The generated and
authored Prelude match exactly.

Only exceeded evaluator-work ceilings were rebased from measurements, with 10%
headroom rounded up to two significant digits. Ordinary function dispatch adds
method, closure and kernel calls. Existing list-allocation and continuation-depth
ceilings remain unchanged; these measurements are work counts, not timing ratios.

| Family       | Evaluator transitions | Applications | List cells | Maximum continuation depth | Maximum capture width |
| ------------ | --------------------- | ------------ | ---------- | -------------------------- | --------------------- |
| expression   | 23,952,562            | 2,988,713    | 111,351    | 1,061                      | 41                    |
| declarations | 10,646,980            | 1,309,276    | 66,661     | 1,076                      | 35                    |
| control-flow | 44,464,429            | 5,493,722    | 219,783    | 1,096                      | 41                    |
| operator     | 53,027,838            | 6,568,857    | 186,499    | 1,116                      | 41                    |
