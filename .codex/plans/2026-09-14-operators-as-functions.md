---
id: JN-OPERATORS-AS-FUNCTIONS-001
status: ready
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
- [ ] Verify focused behavior, full regular suites, clean quality gate and docs;
      commit coherent batches and close the queue row.
