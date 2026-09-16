# RFC 0016: Remove the optional compiler backend

Status: Accepted
Date: 2026-09-07
Supersedes: RFCs 0005, 0006, 0009, 0010, 0011, 0013, 0014, and 0015; the backend requirements of RFCs 0003 and 0004.

Amended by: [RFC 0018](0018-direct-analyzed-runtime-facts.md) replaces runtime-plan retention with direct consumption of analyzed semantic facts.

> **2026-09-15 amendment:** The hosted frontend retention and conformance requirements below are superseded by [RFC 0022](0022-hosted-compiler-removal.md). The Jazz-authored compiler and its exclusive support are removed; the standard library and Haskell compiler remain. The optional-backend removal remains in force.

## Decision

Remove the optional Haskell Typed Core producer, schema, validator, and Lowered
IR lowering pipeline, together with their Jazz schema and validator mirrors,
exclusive tests, and benchmarks. These representations are no longer required
steps toward self-hosting or a future interpreter migration.

Keep the Haskell compiler and analyzed-core interpreter, phase-indexed AST,
semantic types, nominal identities, and attached analysis facts. RFC 0018
replaces runtime-plan retention with direct consumption of those facts.
Canonical core's `Lowered` phase remains the output of surface lowering;
it is independent of the removed Lowered IR.

Historically, this decision also retained the Jazz-authored lexer, parser,
canonical-core lowerer, and their structural differential tests. RFC 0022
retired that requirement and removed those components.

The backend contracts in RFCs 0005, 0006, 0009, 0010, 0011, 0013, 0014, and
0015 are retired. This decision originally retained RFCs 0003 and 0004's stage-0,
hosted-frontend, and conformance decisions without prescribing the backend
representations, their mirrors, or a mandatory backend route to self-hosting.
RFC 0022 subsequently retired the staged hosted-compiler implementation,
hosted-frontend retention, and conformance obligations. The Haskell compiler
and interpreter, historically called stage 0, remain the active implementation.
Public language semantics, including the pattern coverage contract in RFC 0012,
are unchanged.

## Context

The optional backend duplicated representations and validation for a bounded
subset of the language without an emitter or an execution consumer. Maintaining
that path alongside the working interpreter added substantial code and tests
without delivering another way to run Jazz. At acceptance, the hosted frontend
had an independent tested purpose and did not depend on those backend schemas.

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
