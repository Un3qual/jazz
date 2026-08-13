---
title: Governance
description: Understand Jazz language authority, semantic change control, and documentation maintenance.
sidebar_position: 3
---

Jazz resolves conflicting claims in this order:

1. curated public language and reference documentation;
2. current compiler, standard-library, and test behavior as implementation
   evidence;
3. accepted durable decision records; and
4. roadmap material, which is non-normative.

Public documentation describes implemented behavior. Working but incomplete
behavior is `Partial`; future work is `Planned` and belongs on the roadmap.

Semantic language changes require a reviewed decision record before
implementation. It records the decision, context, consequences, non-goals, and
any compatibility or diagnostic effects. Acceptance alone does not make the
behavior public; code, tests, and documentation must land together.

Non-semantic compiler refactors may proceed without a language decision when
they preserve observable behavior. Changes to stable diagnostic codes, module
visibility, runtime values, standard-library contracts, or accepted syntax are
semantic and follow the full process.

Contributors should report documentation drift like a code defect. See the
[contribution guide](contributing.md).

## Stewardship and review

Maintainers review and merge repository changes. At the project's current
scale, an accepted RFC and its implementation pull request record a decision;
there is no formal voting body or guaranteed release cadence. Substantial
proposals begin in the issue tracker.

Maintainers may request changes, defer a proposal, or reject it when semantics
are underspecified, compatibility costs are unjustified, verification is
missing, or the work conflicts with an accepted decision. Durable decisions
are superseded with a new RFC rather than silently rewritten.

Release publication is a separate explicit maintainer decision. Passing CI or
preparing artifacts does not by itself publish or support a release.
