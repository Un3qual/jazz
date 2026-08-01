# Jazz Spec Authority Policy

Status: active transitional policy
Locked decisions: 2026-07-31

## Purpose

Keep public language contracts, current behavior, durable decisions, and roadmap
material in a clear authority order during the repository productization work.

## Authority Model

### 1) Public language contracts

- During the current repository-canonicalization workstream, `docs/spec/` is
  the transitional public contract for language and runtime behavior.
- Once Workstream 2 creates `docs/language/` and `docs/reference/`, those paths
  become the highest public authority and the transition allowance ends.

### 2) Current implementation evidence

- Current `src/`, `jazz/`, and `test/` behavior is the next source of evidence
  for implemented semantics not yet covered by a public contract.
- Conflicts between desired semantics and current behavior require an accepted
  decision before implementation converges on the new contract.

### 3) Accepted RFCs

- Workstream 2 establishes accepted RFCs as authoritative durable decisions.
- An accepted RFC may refine or supersede an earlier decision, but does not make
  unimplemented syntax or runtime behavior available by itself.

### 4) Roadmap material

- Roadmap material is informative and non-normative.
- A roadmap item does not override public contracts, verified behavior, or an
  accepted RFC.

### 5) Repository summaries

- `README.md`, `docs/feature-status.md`, and `docs/jazz-language-state.md`
  summarize status and direction.
- These files must stay aligned with the authority order above, but they are not
  independent normative sources.

## Hybrid Change-Acceptance Workflow

### Semantic language changes

- Semantic changes require an RFC or decision record before implementation.
- The same change series must update the governing contract, tests,
  implementation, rationale, and migration notes.

### Non-semantic or internal changes

- Implementation-first work is allowed for refactors, tooling, and internal
  cleanup that does not change language semantics.
- If observable behavior, documentation, or test expectations change, docs and
  tests must be updated in the same change.

## Maintenance Checklist

For any semantic change:

1. Add or update the RFC or decision record.
2. Update the public contract.
3. Update tests and implementation in the root paths.
4. Update summary documentation to match.
5. Run `scripts/check-spec-authority.sh`.
