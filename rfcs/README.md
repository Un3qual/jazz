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

## Process

Open a proposal only for a decision that should outlive its implementation
plan. Number it when it is ready for review, place it under `proposed/`, and
describe one coherent decision. Record alternatives only when they clarify the
selected boundary.

Acceptance requires maintainer approval and evidence that the decision agrees
with the public language contract and current implementation constraints. Move
the file to `accepted/` and change its status in the same change. If an accepted
RFC changes language semantics, update the public contract, implementation,
tests, rationale, and migration notes together.

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
