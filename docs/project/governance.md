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

Public documentation describes implemented behavior. A working but incomplete
surface is labeled `Partial`; future work is labeled `Planned` and belongs on
the roadmap rather than in executable examples.

Semantic language changes require a reviewed decision record before
implementation. A proposal must state its decision, context, consequences, and
explicit non-goals. Record motivation and, where the change requires them,
compatibility, diagnostics, alternatives, and verification evidence. Acceptance
does not make unfinished behavior public; code, tests, and documentation must
land together before the language guide changes.

Non-semantic compiler refactors may proceed without a language decision when
they preserve observable behavior. Changes to stable diagnostic codes, module
visibility, runtime values, standard-library contracts, or accepted syntax are
semantic and follow the full process.

Contributors should report documentation drift like a code defect. See the
[contribution guide](contributing.md).

## Stewardship and review

Maintainers review and merge repository changes. At the project's current
scale, acceptance is recorded through an accepted RFC and the pull request that
lands its implementation; the project does not claim a formal voting body or a
guaranteed release cadence. Substantial proposals should begin in the issue
tracker so motivation, alternatives, compatibility, and verification can be
reviewed before implementation work grows.

Maintainers may request changes, defer a proposal, or reject it when semantics
are underspecified, compatibility costs are unjustified, verification is
missing, or the work conflicts with an accepted decision. Durable decisions
are superseded with a new RFC rather than silently rewritten.

Release publication is a separate explicit maintainer decision. Passing CI or
preparing artifacts does not by itself publish or support a release.
