# RFC 0001: Language authority and change control

Status: Accepted
Date: 2026-07-31
Supersedes: Authority and governance decisions dated 2026-03-03 and 2026-07-31.

## Decision

Jazz uses this descending authority order:

1. Canonical public language contracts under `docs/language/` and
   `docs/reference/`.
2. Behavior verified by the current implementation and tests under `src/`,
   `jazz/`, and `test/` when the public contract does not yet cover a detail.
3. Accepted durable decisions under `rfcs/accepted/`.
4. Roadmap material, which is informative and non-normative.

During the documentation-reset transition, `docs/spec/` remains the temporary
public contract. That allowance ends when its content has been rewritten into
the canonical public directories.

An accepted RFC explains intent and constrains future implementation, but it
does not make unimplemented behavior available. If a public contract and the
implementation disagree, the conflict must be resolved explicitly; a roadmap
entry or internal plan cannot decide the language by itself.

Every semantic language change requires an accepted RFC before implementation.
The change that implements it must also update the governing public contract,
behavior tests, rationale, and any required migration guidance. Internal
refactors and tooling changes may proceed implementation-first when they do not
change observable language behavior, but their tests and documentation must be
updated in the same change when observable tooling behavior changes.

`.codex/execution/` and `.codex/plans/` coordinate work. They never define
public language behavior by themselves.

## Context

Jazz previously mixed specifications, implementation plans, status summaries,
historical compiler comparisons, and execution queues under one documentation
tree. That made it possible for an old plan or an experimental implementation
to appear as authoritative as an implemented contract.

The project needs both an accurate user-facing contract and a durable place to
record architectural intent. Giving public contracts the highest authority
keeps user documentation truthful. Treating current implementation and tests
as the next evidence source acknowledges behavior that has shipped before its
documentation is complete. RFCs then preserve the reason for a decision
without claiming that acceptance alone implements it.

## Consequences

- Public documentation must describe implemented behavior and label partial or
  planned work honestly.
- Semantic work cannot be promoted from a roadmap or task plan directly into
  code; it first needs an accepted RFC.
- Accepted RFCs must be updated or superseded when their durable decision
  changes rather than silently drifting from code.
- Repository summaries may explain status, but they are not independent
  normative sources.
- Historical plans and execution logs can be deleted once their durable
  decisions and still-open work have proper owners.
