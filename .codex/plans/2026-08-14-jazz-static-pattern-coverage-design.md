# Jazz static pattern coverage design

**Date:** 2026-08-14

**Status:** Approved for implementation

## Purpose

Add compile-time exhaustiveness and unreachable-arm analysis for the complete
active Jazz pattern surface. The batch replaces ordinary source programs'
dependence on runtime `E3022` with deterministic compile errors while preserving
`E3022` as a defensive interpreter boundary.

Accepted RFC 0012 owns the public semantic delta. The implementation remains in
the active Haskell compiler under `src/` and does not change Typed Core, Lowered
IR, runtime selection, or the Jazz-authored hosted compiler schemas.

## Approved semantics

Coverage applies to every canonical `EPatternCase`. Surface case expressions and
pattern lambdas both lower through that representation, so they share one
coverage contract.

The analysis understands the complete active pattern vocabulary:

- wildcard and variable patterns;
- integral, `Bool`, `Char`, and `Text` literals;
- data constructors and their fields;
- exact-length and cons-like list patterns;
- fixed-arity tuple patterns, including unit;
- as-patterns; and
- top-level or-patterns.

A match is exhaustive only when its unguarded arms cover every value admitted by
the resolved scrutinee type. Guarded arms never contribute to exhaustiveness,
including syntactically constant guards. This keeps the rule independent of
constant folding and preserves the possibility that every guard evaluates to
`False`.

An arm is unreachable when its complete pattern space is already covered by
earlier unguarded arms. Earlier guarded arms never make a later arm unreachable.
A guarded arm can still be unreachable when an earlier unguarded arm already
covers it. An or-pattern arm is reachable when at least one alternative remains
useful. Partial redundancy inside a still-useful or-pattern is not diagnosed in
this batch because canonical patterns do not retain alternative-level source
spans.

Non-exhaustive matches are hard compile errors. Wholly unreachable arms are hard
compile errors. Neither diagnostic is configurable through warning settings.

## Coverage model

`Jazz.Compiler.PatternCoverage` owns a pure usefulness-matrix implementation.
It receives one resolved scrutinee type, the visible data-type constructor
inventory, and source-ordered arms. It does not depend on the inference solver,
analyzer traversal, runtime values, or backend representations.

Patterns normalize only for analysis:

- variables and wildcards become the coverage wildcard;
- as-patterns contribute their inner pattern;
- or-patterns expand into alternative rows without changing source arm identity;
- exact lists expand into nested list-cons cells ending in list-nil;
- cons-like lists expand into one list-cons cell;
- tuples use one fixed-arity product constructor;
- ADT constructors use their resolved constructor identity and arity; and
- scalar literals use exact open-domain literal constructors.

Closed constructor spaces are `Bool`, unit, lists, tuples, and declared ADTs.
Integral, `Char`, `Text`, numeric-width, and other scalar domains remain open:
literal rows cannot exhaust them without a wildcard or variable. Floating
literal patterns remain rejected by existing pattern typing and do not need a
coverage rule.

The usefulness algorithm answers two questions with the same machinery:

1. Is the current arm's normalized pattern useful against preceding unguarded
   rows?
2. Is a wildcard useful against all unguarded rows after the final arm?

The second query also returns one deterministic witness. Constructor order comes
from the declared type, `Bool` orders `False` before `True`, list order is `[]`
before cons, tuple fields remain left-to-right, and open scalar domains use `_`
when no more specific missing witness is stable. Witness rendering uses Jazz
pattern syntax and replaces unconstrained fields with `_`.

## Inference integration

Coverage requires final resolved types but must not participate in unification.
The inference traversal therefore records immutable `PatternCoverageSite`
observations in `InferenceOutput`. Each observation contains:

- a stable traversal ordinal reserved before visiting the match's children;
- the scrutinee type term produced by the ordinary inference pass; and
- the original source-ordered arms.

The observation contains no solver callback and does not rerun inference. After
defaulting and final substitution, `finishInference` resolves every observation
against the final inference state, orders sites by their reserved ordinal, and
passes them to the pure coverage engine with the final data-type inventory.

Coverage diagnostics are suppressed when the ordinary analyzer or type
inference already produced an effective error. This avoids secondary reports
from invalid patterns, unresolved constructors, bad guards, or ambiguous types.
Warnings that remain warnings do not suppress coverage.

Coverage diagnostics append after ordinary analyzer and type diagnostics. Sites
are ordered by traversal ordinal. Within one site, unreachable arms are emitted
in arm order, followed by the non-exhaustive diagnostic. This ordering is part
of the deterministic compiler contract.

Inference rollback helpers must preserve the site ordinal counter and retain
only observations belonging to retained successful traversal. Focused rollback
tests will cover failed patterns and failed nested expressions so speculative
solver work cannot leak or duplicate coverage reports.

## Diagnostics

The type-diagnostic catalog gains:

- `E2018`: non-exhaustive pattern match; and
- `E2019`: unreachable pattern arm.

`E2018` includes one deterministic missing-pattern witness in its summary and a
help message directing the author to add an unguarded covering arm. `E2019`
identifies the one-based arm index and explains that earlier unguarded arms
already cover it.

Canonical core does not retain inner pattern spans. These diagnostics therefore
use the same spanless structured form as existing inner pattern type errors. The
arm index and witness provide stable local identity without inventing inaccurate
statement-level labels.

## Runtime and backend behavior

The reference interpreter's selection algorithm and runtime `E3022` remain
unchanged. Ordinary source programs that pass compilation will no longer reach
`E3022` through a non-exhaustive case or pattern lambda. The runtime code remains
necessary for independently constructed canonical core, defensive invariants,
and any future embedding boundary.

Typed Core and Lowered IR do not gain coverage metadata. RFC 0011's required
final unguarded catch-all remains an independent opt-in lowerer profile rule; a
source match may be statically exhaustive through closed constructor coverage
without satisfying that narrower backend profile.

## Approaches rejected

### Analyzer-only syntactic coverage

The analyzer runs over untyped canonical core and cannot reliably distinguish a
closed ADT constructor set, instantiate generic constructor fields, or select
the correct imported type inventory. Syntactic checks would either miss useful
closed-type coverage or guess across unresolved names.

### Coverage inside pattern unification

Embedding matrix state in `inferPatternType` would couple usefulness decisions to
solver rollback, duplicate coverage logic across case-arm inference paths, and
emit diagnostics before the scrutinee type is final. Coverage consumes inference
facts; it is not itself an inference constraint.

### Scalar-only coverage

Limiting the batch to RFC 0011's scalar backend profile would leave ordinary ADT,
list, tuple, as-pattern, or-pattern, and pattern-lambda behavior dependent on
`E3022`. The active language already types and executes those forms. One complete
canonical coverage engine is more coherent than a temporary scalar checker.

## Verification design

A dedicated pure coverage suite proves usefulness and witness behavior for:

- empty matches, wildcard and variable coverage;
- open literal domains and duplicate literals;
- complete and incomplete `Bool` matches;
- unit and tuple products;
- empty, exact, cons-like, nested, and recursive list shapes;
- nullary, product, generic, recursive, and imported ADTs;
- as-pattern normalization;
- complete, partially redundant, and wholly redundant or-pattern arms;
- guarded arms that never add coverage;
- guarded arms shadowed by earlier unguarded coverage; and
- deterministic missing witnesses and arm indices.

Source-pipeline tests prove:

- non-exhaustive cases and pattern lambdas fail with `E2018`;
- wholly unreachable arms fail with `E2019`;
- complete matches compile and retain runtime behavior;
- false guards require a later unguarded covering arm;
- nested and module-compiled matches are checked once;
- invalid typed patterns suppress coverage cascades; and
- existing warning policy does not alter coverage severity.

Catalog tests lock the new codes and type-diagnostic range. Public language,
status, and diagnostic pages replace the old "not implemented" claim with the
strict static contract while documenting defensive `E3022` behavior.

Focused verification runs the new coverage suite, pattern semantics, ADT pattern
typing and runtime, module compilation, and diagnostic catalog suites. Closeout
runs `cabal build all`, the complete serialized Cabal suite, documentation and
queue checks, repository audit, and `git diff --check`.

## Non-goals

- Constant-folding guards or using guarded arms as coverage evidence.
- Diagnosing a redundant alternative inside an otherwise useful or-pattern.
- New pattern syntax, nested or-patterns, pattern synonyms, or lambda guards.
- Managed-value Typed Core production or managed pattern lowering.
- Pattern-lambda backend lowering or callable mismatch ABI changes.
- Removing or renumbering runtime `E3022`.
- Optimizing runtime selection, producing decision trees, or changing arm order.
- Changing Typed Core, Lowered IR, native code generation, or module export
  semantics.
- Adding configurable coverage warnings or compatibility flags.

## Acceptance criteria

The batch is complete when every valid active pattern form participates in one
resolved-type coverage analysis, incomplete matches deterministically emit
`E2018`, wholly unreachable arms deterministically emit `E2019`, guarded arms
never contribute coverage, valid exhaustive programs retain existing runtime
selection, invalid programs do not receive coverage cascades, public docs match
the implemented contract, and focused plus full serialized verification pass.
