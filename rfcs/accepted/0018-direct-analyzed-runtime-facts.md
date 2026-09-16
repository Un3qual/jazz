# RFC 0018: Execute analyzed semantic facts directly

Status: Accepted
Date: 2026-09-11
Supersedes: RFC 0016's retention of runtime plans only.

> **2026-09-15 amendment:** The hosted frontend retention and structural comparison obligations below are retired by [RFC 0022](0022-hosted-compiler-removal.md). Canonical `Lowered` core, direct analyzed execution, and ordinary Haskell compiler/runtime tests remain in force.

## Decision

The analyzed-core interpreter consumes checked instantiation, evidence, literal,
and result representation facts directly. Remove the stored `RuntimePlan` and
`RuntimeObligation` instruction sequence that restates those facts on each node.
Explicit instantiation retains its target identity and ordered type arguments,
including qualified methods without an ordinary lexical binder.

Preserve the execution order of instantiation, evidence selection, numeric
literal specialization, and result constraints. Result handling still owns
function annotations, higher-order hints, integer defaulting, and profile-frame
closure. These runtime controls are not a second analyzed instruction sequence.
Concrete method evidence names the selected implementation and method. Dynamic
method selection remains available where checking cannot select an implementation.

## Context

The approved compiler architecture remediation replaces independently collected
semantic records with checked subtrees. Storing an additional executable plan
for the same decisions duplicates their ownership and requires consumers to
recover semantic facts from instructions.

## Consequences

This amendment authorizes direct consumption without changing public language
semantics, evaluation order, laziness, host effects, diagnostics, or result
projections. RFC 0016's optional-backend removal, phase-indexed AST, analyzed
interpreter, nominal identities, and attached semantic facts remain in force.
Canonical `Lowered` core remains independent of runtime representation decisions.
Historically, this amendment also retained the hosted frontend and its canonical
`Lowered` structural comparison tests; RFC 0022 retired those obligations.

Maintainer approval of the architecture remediation plan covers this narrow
amendment. Acceptance authorizes the implementation; it does not claim that
all runtime-plan consumers have already been migrated.
