# RFC 0001: Language authority and change control

Status: Accepted
Date: 2026-07-31
Supersedes: Pre-RFC authority and governance decisions recorded on 2026-03-03 and 2026-07-31.

## Decision

Jazz uses this descending authority order:

1. Canonical public language contracts under `docs/language/` and
   `docs/reference/`.
2. Behavior verified by the current implementation and tests under `src/`,
   `jazz/`, and `test/` when the public contract does not yet cover a detail.
3. Accepted durable decisions under `rfcs/accepted/`.
4. Roadmap material, which is informative and non-normative.

For claims about behavior users can rely on now, the public contract and
verified implementation are operational truth. An accepted RFC may record an
existing durable decision or authorize an explicit future contract delta. A
future delta must be labeled as unimplemented; acceptance constrains the work
that may implement it but does not override current operational truth or make
the behavior available.

If a public contract and the implementation disagree about current behavior,
the conflict must be resolved explicitly. A deliberately future RFC is not
such a conflict, because it describes target behavior rather than claiming
current availability. A roadmap entry or internal plan cannot decide either
current or target language behavior by itself.

Every semantic language change requires an accepted RFC before implementation.
Acceptance may therefore precede the implementation and public-contract
change. When the authorized delta lands, its governing public contract,
implementation, behavior tests, rationale, and any required migration guidance
must land together. Until that coordinated change is complete, the existing
public contract and verified behavior remain in force. Internal refactors and
tooling changes may proceed implementation-first when they do not change
observable language behavior, but their tests and documentation must be
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
- A future-facing accepted RFC authorizes a target contract but cannot be cited
  as evidence that the target is implemented or available.
- Accepted RFCs must be updated or superseded when their durable decision
  changes rather than silently drifting from code.
- Repository summaries may explain status, but they are not independent
  normative sources.
- Historical plans and execution logs can be deleted once their durable
  decisions and still-open work have proper owners.
- This RFC does not authorize a semantic change on the strength of a roadmap
  entry, accepted plan, or implementation artifact alone.
