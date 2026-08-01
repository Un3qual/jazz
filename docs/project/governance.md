---
title: Governance
description: Understand Jazz language authority, semantic change control, and documentation maintenance.
sidebar_position: 3
---

# Governance

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
implementation. A proposal must define motivation, accepted syntax and
semantics, compatibility, diagnostics, alternatives, consequences, and a
verification plan. Acceptance does not make unfinished behavior public; code,
tests, and documentation must land together before the language guide changes.

Non-semantic compiler refactors may proceed without a language decision when
they preserve observable behavior. Changes to stable diagnostic codes, module
visibility, runtime values, standard-library contracts, or accepted syntax are
semantic and follow the full process.

Contributors should report documentation drift like a code defect. See the
[contribution guide](contributing.md).
