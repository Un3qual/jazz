# Abstraction Vocabulary Decision Matrix

Status: accepted decision artifact
Primary plan: `docs/plans/spec-clarification/2026-03-02/abstractions/08-trait-vocabulary-and-capability-model.md`

## Decision

Jazz uses Haskell-like canonical capability names for the standard vocabulary.

Canonical family:

- `Eq`
- `Ord`
- `Num`
- `Integral`
- `Fractional`
- `Showable`
- `Default`

Domain-oriented words such as `Collection`, `Orderable`, and `Numeric` may appear in prose or examples only when they are clearly aliases, pedagogical wording, or future-library names. They are not the canonical core capability vocabulary.

## Matrix

| Candidate | Outcome | Rationale |
| --- | --- | --- |
| Haskell-like core names (`Eq`, `Ord`, `Num`) | Selected | Matches the active constrained-signature vocabulary and keeps future class/impl semantics aligned with the existing type-signature surface. |
| Domain-like names (`Collection`, `Orderable`, `Numeric`) | Rejected as canonical core | These names read well in introductory prose but do not match the active compiler vocabulary. They can exist later as ordinary stdlib classes if useful. |
| Two-tier compiler/user alias model | Deferred | It adds mapping complexity before class/impl semantics exist. If aliases are needed later, they should be ordinary stdlib definitions or explicit compatibility docs. |

## Non-Canonical Term Handling

- `trait` is not canonical syntax; canonical declarations use `class` and `impl`.
- `Collection`, `Orderable`, and `Numeric` are not builtin compiler vocabulary.
- Docs may mention non-canonical terms only with explicit wording such as "historical", "example alias", or "future library name".

