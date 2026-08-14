# RFC 0012: Static pattern coverage

Status: Accepted
Date: 2026-08-14
Supersedes: None.

## Decision

Jazz will reject non-exhaustive pattern matches and wholly unreachable pattern
arms during compilation. The rule applies to ordinary case expressions and
pattern lambdas across the complete active pattern surface: literals,
constructors, exact and cons-like lists, tuples, as-patterns, or-patterns,
wildcards, and variables.

Coverage is computed after type inference from the resolved scrutinee type and
visible constructor inventory. Non-exhaustive matches emit `E2018` with one
deterministic missing-pattern witness. Wholly unreachable arms emit `E2019` with
their one-based arm index. Both are hard compile errors.

Only unguarded arms contribute to coverage. A guarded arm never makes a match
exhaustive and never makes a later arm unreachable, even when its guard is
syntactically constant. A guarded arm is itself unreachable when earlier
unguarded arms already cover its complete pattern space.

An or-pattern arm is reachable when at least one alternative is useful. This
decision does not diagnose redundant alternatives inside an otherwise useful
or-pattern.

The reference interpreter retains runtime `E3022` as a defensive no-match
boundary. Coverage does not change runtime selection, Typed Core, Lowered IR, or
RFC 0011's narrower final-catch-all backend profile.

## Context

Jazz already parses, types, and executes ordered matches over closed ADTs,
lists, tuples, and `Bool` as well as open scalar literal domains. It also
supports as-patterns, top-level or-patterns, guards, and pattern lambdas. Before
this decision, a source-valid match with no selected arm failed only at runtime
with `E3022`, and arms covered by earlier patterns remained silently dead.

RFC 0011 deliberately kept static coverage separate from scalar-pattern
lowering because coverage owns constructor spaces, open literal domains,
or-pattern usefulness, guards, and public diagnostic policy. The active
language surface is now stable enough for one complete canonical analysis
instead of a backend-profile-specific approximation.

Strict errors are appropriate before 1.0: an incomplete match is a latent
runtime failure, and a wholly unreachable arm cannot affect program behavior.
Keeping guards out of coverage avoids unsound reasoning about arbitrary
expressions and keeps runtime ordering unchanged.

## Consequences

- Type inference must retain each match's scrutinee type and source-ordered arms
  until final substitution without rerunning inference.
- One pure usefulness-matrix engine owns exhaustiveness, reachability, and
  deterministic witness generation for every active pattern form.
- Coverage diagnostics are suppressed when existing analyzer or inference
  errors would make the match facts unreliable.
- Closed constructor spaces come from resolved `Bool`, unit, list, tuple, and
  declared ADT types. Integral, `Char`, `Text`, and other scalar literal domains
  remain open unless covered by a wildcard or variable.
- Surface pattern lambdas inherit the rule through their canonical pattern-case
  representation.
- Existing source programs may need an unguarded fallback or removal of dead
  arms. No compatibility warning mode is introduced.
- Runtime `E3022`, backend pattern lowering, managed-value representation,
  pattern synonyms, guard folding, partial or-pattern redundancy, and selection
  optimization require no change under this decision.
