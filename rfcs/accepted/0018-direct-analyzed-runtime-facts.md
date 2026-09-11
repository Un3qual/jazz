# RFC 0018: Execute analyzed semantic facts directly

Status: Accepted
Date: 2026-09-11
Amends: RFC 0016's retention of runtime plans only.

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
interpreter, nominal identities, attached semantic facts, and hosted-frontend
boundaries remain in force. Canonical `Lowered` core and its structural
comparison tests remain independent of runtime representation decisions.

Maintainer approval of the architecture remediation plan covers this narrow
amendment. Acceptance authorizes the implementation; it does not claim that
all runtime-plan consumers have already been migrated.
