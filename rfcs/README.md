# Jazz RFCs

RFCs record durable language and compiler decisions. They explain why a
boundary exists and what future changes must preserve; they are not task plans,
release notes, or user documentation.

## Status directories

- `accepted/` contains decisions that govern ongoing work. An accepted RFC may
  be amended or superseded only by another accepted RFC.
- `proposed/` contains reviewable proposals. A proposal has no authority over
  public documentation, implemented behavior, or an accepted RFC.
- Active implementation plans and execution state belong under `.codex/`, not
  in this tree.

RFCs are repository design records. The public documentation website reads
only `docs/` and does not publish this directory.

## Accepted decisions

| RFC                                                            | Decision                              |
| -------------------------------------------------------------- | ------------------------------------- |
| [0001](accepted/0001-language-authority-and-change-control.md) | Language authority and change control |
| [0002](accepted/0002-repository-productization.md)             | Repository productization             |
| [0003](accepted/0003-bootstrap-interpreter-profile.md)         | Bootstrap interpreter profile         |
| [0004](accepted/0004-hosted-canonical-compiler.md)             | Hosted canonical compiler             |
| [0005](accepted/0005-typed-core-elaboration.md)                | Typed-core elaboration                |
| [0006](accepted/0006-lowered-ir-contract.md)                   | Lowered IR contract                   |
| [0007](accepted/0007-runtime-host-boundary.md)                 | Runtime host boundary                 |
| [0008](accepted/0008-parser-scale-and-performance-tiers.md)    | Parser scale and performance tiers    |
| [0009](accepted/0009-typed-core-closure-and-recursion.md)      | Typed-core closures and recursion     |
| [0010](accepted/0010-typed-core-conditional-control-flow.md)   | Typed-core conditional control flow   |

## Process

Open a proposal only for a decision that should outlive its implementation
plan. Number it when it is ready for review, place it under `proposed/`, and
describe one coherent decision. Record alternatives only when they clarify the
selected boundary.

Acceptance requires maintainer approval, a coherent durable boundary, and an
explicit distinction between current behavior and any authorized future
contract delta. A future delta need not already match the current public
contract or implementation; acceptance constrains subsequent work without
claiming that the behavior is available. Move the file to `accepted/` and
change its status in the same change.

When an accepted semantic delta is implemented, update the public contract,
implementation, behavior tests, rationale, and any required migration notes in
one coordinated change. Until then, existing public documentation and verified
implementation remain the operational truth.

## Template

```markdown
# RFC NNNN: Title

Status: Proposed
Date: YYYY-MM-DD
Supersedes: None.

## Decision

State the durable choice and its boundaries.

## Context

Explain the problem, relevant constraints, and why a durable decision is
needed.

## Consequences

Describe the benefits, costs, follow-up requirements, and explicit non-goals.
```
