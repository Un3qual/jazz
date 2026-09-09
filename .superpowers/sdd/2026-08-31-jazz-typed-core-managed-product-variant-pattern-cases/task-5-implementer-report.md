# Task 5 implementer report

## Status

Complete, including the Task 5 Important review fixes. Managed as-patterns,
top-level alternatives, guarded structured rows, mixed source-order
fallthrough, ambient transport, and both value/function tail positions now
lower to exact validated Lowered IR. Final collectively exhaustive alternatives
use a recursive catalog-derived decision tree, and alternative binder scanning
retains stable maximum occurrence multiplicity in both validators.

## RED evidence

The Task 5 exact fixtures and transport assertions were added before changing
the emitter. The focused suite compiled cleanly and first failed at the old
as-pattern boundary:

```text
managed-as-constructor-pattern exact managed product/variant lowering:
expected successful lowering, got LoweredIRUnsupported ...
TypedPatternPath ["App","Main"] [1] [0,0] LoweredIRUnsupportedPattern
```

After the initial alternative implementation, the exact `Choice` regression
was strengthened to contain refutable constructor children and a later source
arm. Compilation remained clean and produced the intended second RED:

```text
managed-or-constructor-pattern exact managed product/variant lowering:
expected successful lowering, got LoweredIRUnsupported ...
TypedPatternPath ["App","Main"] [1] [0,0] LoweredIRUnsupportedPattern
```

The first mandated three-suite run exposed an existing producer regression from
commit `b73c6271`: Haskell duplicate-binder scanning ignored distinct binder
identities in later Or alternatives, while the hosted validator visited every
alternative. The existing typed-core contract was the RED:

```text
duplicate binder scanning visits every or-pattern alternative:
expected [TypedDuplicateBinder ...], got []
```

The review-fix lowerer fixtures then clean-compiled and failed at the old narrow
final-Or boundary exactly as intended:

```text
managed-or-reverse-total ... LoweredIRUnsupportedPattern
TypedPatternPath ["App","Main"] [1] [0,0]

managed-or-recursive-total ... LoweredIRUnsupportedPattern
TypedPatternPath ["App","Main"] [2] [0,0]
```

The independent validator multiplicity regressions also failed with the two
specific symptoms from the review:

```text
or-pattern alternatives retain only the stable maximum binder multiplicity:
expected [], got TypedDuplicateBinder ... [0] [0,0,2]

or-pattern alternatives retain real excess binder occurrences:
expected TypedDuplicateBinder ... [0] [0,0,1,1], got only
TypedOrPatternBinderMismatch ... [0] [0,0]
```

## Implementation

- `ManagedAs` appends the current operand to the ordered pending binder payload
  before matching its child. Those operands become lexical bindings only in the
  complete success block.
- Refutable top-level alternatives enter authored-order blocks. Constructor and
  nested-literal failure advances to the next alternative; the last failure
  advances to the next source arm with only outer control operands and the
  original scrutinee. All successes use the first alternative's canonical
  binder identities and one common body parameter contract.
- Exhaustive irrefutable constructor alternatives retain a catalog-derived
  tag dispatch with selected-only field projection.
- Structured guards start only after complete matching. Their true edge carries
  canonical binder operands; their false edge carries only restored outer
  control state and the original scrutinee.
- Existing `ProduceValue` joins and `FinishFunction` direct/closure returns are
  reused. No pattern-specific result channel or transport representation was
  added.
- Runtime requirement collection already recursively traversed
  `typedPatternChildren`; no production change was needed. A new regression
  proves nested managed patterns discover only the existing Text layout and no
  service or schema.
- The Haskell and Jazz-authored validators now count canonical same-identity Or
  binders once while retaining distinct later identities for module-wide
  duplicate detection. The focused same-ID regression and hosted parity both
  remain green.
- Final exhaustive alternatives are compiled as an authored-row decision
  matrix. Closed variants enumerate every catalog constructor as an explicit
  switch case with no default, recursively refine selected fields, and route
  every leaf to its authored alternative block. Binder extraction happens only
  on the selected leaf before all leaves enter one canonical success block.
- Haskell and Jazz occurrence scans begin with the first alternative's stable
  occurrences, then append only each later alternative's per-binder excess over
  the represented maximum. Selection follows authored occurrence order, so
  true duplicates keep deterministic paths while A/B/B alternatives do not
  invent a second B occurrence.

## Exact fixtures

- Complete scrutinee plus nested projected field as distinct arm parameters.
- Guarded as-pattern transport and false-guard fallthrough.
- Refutable authored-order `Choice` alternatives sharing one common body.
- Repeated constructors, nested literal failure, guarded false fallthrough, and
  later catch-all in exact source order.
- Captured scalar in a closure managed arm.
- Nested managed case with independent result joins.
- Closure-valued managed result joined before application.
- Managed cases in direct and closure function-result position with exact
  return terminators.
- Authored-order `Right item | Left item` exhaustive dispatch opposite catalog
  order, with explicit catalog tag cases and no default.
- Recursively collective `Left (whole @ None) | Left (whole @ Some _) | Right
  whole` coverage with explicit nested Option dispatch and one canonical body.
- A/B/B stable alternative occurrence multiplicity, a retained later A/A excess
  at its second authored path, and an outer collision with distinct later B.

## Files

- `src/Jazz/Compiler/LoweredIR/Lower/Emit.hs`
- `src/Jazz/Compiler/TypedCore/Validate/Patterns.hs`
- `jazz/compiler/TypedCoreValidate.jz`
- `test/Jazz/Compiler/Bootstrap/TypedCoreContract/RegressionTests.hs`
- `test/Jazz/Compiler/Bootstrap/TypedCoreContract/ReviewFixtures.hs`
- `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures/ManagedProductsVariants.hs`
- `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures/LowererBoundary.hs`
- `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec/ManagedProductsVariantsTests.hs`
- `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec/ScalarTextTests.hs`

`Requirements.hs` was audited but intentionally left unchanged because its
existing recursive traversal is the required implementation.

## GREEN evidence

The focused suite passed after the final alternative-chain implementation. The
typed-core contract suite then passed independently, including Haskell/hosted
parity and both the distinct-later-ID and canonical-same-ID Or regressions.

After final Haskell formatting, the required three-suite command passed twice:

```text
All TypedCoreExpressionDirectCall tests passed.
All JazzTypedCoreContract tests passed.
All JazzLoweredIRContract tests passed.
```

The pinned Nix Ormolu check and `git diff --check` both exited zero.

For the review fix, the focused expression suite passed with both new exact
decision-tree fixtures. The typed-core contract suite passed independently,
including both new multiplicity contracts, the existing distinct-later-ID
collision, and full Haskell/hosted parity twice. After final formatting, the
exact three-suite gate again passed twice, and the pinned Ormolu check exited
zero.

## Concerns

No remaining Task 5 concern. Scalar Or-patterns remain fail-closed because the
approved managed-pattern admission boundary permits alternatives only for
managed structured scrutinees. Unsupported nested Or/list/text forms retain
their existing exact failures.
