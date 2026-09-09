# RFC 0016: Remove the optional compiler backend

Status: Accepted
Date: 2026-09-07
Supersedes: RFCs 0005, 0006, 0009, 0010, 0011, 0013, 0014, and 0015; the backend requirements of RFCs 0003 and 0004.

## Decision

Remove the optional Haskell Typed Core producer, schema, validator, and Lowered
IR lowering pipeline, together with their Jazz schema and validator mirrors,
exclusive tests, and benchmarks. These representations are no longer required
steps toward self-hosting or a future interpreter migration.

Keep the Haskell compiler and analyzed-core interpreter, phase-indexed AST,
semantic types, nominal identities, attached analysis facts, and runtime plans.
Keep the Jazz-authored lexer, parser, canonical-core lowerer, and their
structural differential tests. Canonical core's `Lowered` phase remains the
output of surface lowering; it is independent of the removed Lowered IR.

The backend contracts in RFCs 0005, 0006, 0009, 0010, 0011, 0013, 0014, and
0015 are retired. RFCs 0003 and 0004 retain their stage-0, hosted-frontend, and
conformance decisions, but no longer prescribe these backend representations,
their mirrors, or a mandatory backend route to self-hosting. Public language
semantics, including the pattern coverage contract in RFC 0012, are unchanged.

## Context

The optional backend duplicated representations and validation for a bounded
subset of the language without an emitter or an execution consumer. Maintaining
that path alongside the working interpreter added substantial code and tests
without delivering another way to run Jazz. The hosted frontend has an
independent tested purpose and does not depend on those backend schemas.

## Consequences

The working compiler has fewer representations and integration paths to
maintain. Backend-specific structural coverage disappears with the backend;
ordinary semantic and runtime coverage remains. Earlier RFCs remain as historical
records, with explicit supersession notices.

Self-hosting and native compilation remain goals, not approval to restore the
removed architecture. Future backend work requires a concrete execution goal
and a fresh accepted design that justifies each representation and its consumer.
This decision does not select a native target, promise an ABI, or change the
interpreter's semantics.
