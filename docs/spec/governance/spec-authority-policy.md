# Jazz Spec Authority Policy

Status: active
Locked decisions: 2026-03-03
Primary plan: `docs/plans/spec-clarification/2026-03-03/governance/17-jazz2-alignment-and-spec-authority.md`

## Purpose

Prevent spec drift between repository docs, `jazz-hs`, and `jazz2` by defining one authority model and one change-control workflow.

## Authority Model

### 1) Canonical language spec (`docs/spec/*`)

- Normative language rules must be written under `docs/spec/*`.
- When a canonical spec section exists, it is the source of truth for that semantic area.

### 2) Active implementation authority (`jazz-hs`)

- Until a semantic area is fully specified under `docs/spec/*`, `jazz-hs` behavior and tests are the temporary behavioral authority.
- Conflicts between desired semantics and current `jazz-hs` behavior must be resolved by decision records, then implemented to converge.

### 3) Repository summaries (non-primary)

- `README.md` and `docs/jazz-language-state.md` summarize status and direction.
- These files must stay aligned with canonical spec + implementation, but they are not the primary normative source.

### 4) `jazz2` status

- `jazz2` is reference-only and non-normative in the current phase.
- `jazz2` may inform design direction (for example AST organization or future type-system ideas), but it does not define current language behavior.

## Hybrid Change-Acceptance Workflow

### Semantic language changes

- Semantic changes require a decision record or RFC before implementation.
- Required artifacts in the same change series:
  - decision doc with rationale and migration notes,
  - canonical spec update in `docs/spec/*`,
  - tests and implementation updates.

### Non-semantic/internal changes

- Implementation-first is allowed for refactors, tooling, and internal cleanup that does not change language semantics.
- If observable behavior, docs, or test expectations change, docs and tests must be updated in the same change.

## Allowed vs Disallowed Claims

Allowed:
- "`jazz2` is a reference-only design source."
- "Current behavior is defined by `docs/spec/*` and active `jazz-hs` tests."
- "This README is descriptive, not the canonical spec."

Disallowed:
- "`jazz2` is the authoritative implementation."
- "`jazz2` defines the canonical syntax/semantics today."
- "README examples are normative by default without spec alignment."

## Maintenance Checklist

For any semantic change:
1. Add or update decision record.
2. Update canonical spec section in `docs/spec/*`.
3. Update tests and implementation in the normative path.
4. Update summary docs (`README.md`, `docs/jazz-language-state.md`) to match.
5. Run `scripts/check-spec-authority.sh`.

