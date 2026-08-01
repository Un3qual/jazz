# RFC 0005: Typed-core elaboration

Status: Accepted
Date: 2026-07-31
Supersedes: Typed-core decisions dated 2026-07-22 and 2026-07-30.

## Decision

Typed core is a separate, structurally aligned tree between semantic inference
and backend-neutral lowering. Canonical core remains the unchanged input to
analysis, inference, and the reference interpreter; it is not annotated in
place with post-inference data.

The inference traversal is the single owner of semantic decisions. When an
explicit typed-core profile is selected, the same recursive visit retains a
provisional typed node alongside its ordinary inference result. After the
outer scope completes, one finalization pass applies the accepted solver state,
performs existing defaulting, canonicalizes generalized parameters, resolves
recorded evidence, derives representation recipes, constructs the final typed
tree, and validates it. Finalization must not infer again, create new solver
variables, or select different capability evidence.

Every executable typed node records:

- its final semantic type;
- a backend-neutral representation recipe;
- resolved or compiler-owned name identity;
- explicit and implicit type instantiations in solver-defined order; and
- capability-evidence uses in deterministic obligation order.

Typed patterns record the matched type and representation, and every binder
has stable identity and annotation. Typed bindings retain their resolved name,
monomorphic type or generalized scheme, ordered type and evidence parameters,
typed value, and existing span. Typed modules retain resolved imports, public
exports and interface facts, ordered declarations and executable statements,
and terminal result information without embedding host paths or mutable
inference types.

`TypedType` contains only final semantic types. Solver allocation identifiers,
literal ranges, unresolved names, and unbound variables cannot cross the
boundary. Representation recipes describe portable semantic shapes, including
scalars, managed text and collections, variants, products, closures, and
generalized representation parameters; they are not LLVM types. Every
representation parameter must be specialized before lowered IR is emitted.

Production Haskell and Jazz-authored code mirror the typed-core schema and
complete ordered validator. A contract change updates both schemas, both
validators when relevant, checked conversion, and parity fixtures together.
Sidecar maps keyed by spans, names, or structural paths and a second post-
inference type pass are rejected.

The currently implemented opt-in producer covers one resolved module with
closed scalar expressions and concrete, monomorphic, non-capturing local direct
calls. It validates the resulting `TypedProgram` and can lower that bounded
profile to validated lowered IR. It rejects imports, ambient prelude inputs,
managed values, polymorphism and evidence-bearing executable nodes, closures,
recursion, control flow, patterns, and multiple modules with structured profile
failures. Normal inference results, compile mode, and interpreter run mode are
unchanged.

Closure and recursion elaboration is the next design gate. Control-flow,
patterns, managed data, multi-module integration, hosted inference, and native
backend work require later accepted RFCs and independently executable slices.

## Context

Canonical core is intentionally pre-inference, while backend-neutral lowered
IR requires a concrete representation for every value and call. Existing
inference state contains temporary variables and runtime hints but does not
retain final types for every inner node. Inner expressions also lack unique
spans, so annotation maps cannot identify them safely.

Reconstructing typed core after inference would repeat instantiation,
defaulting, constraint solving, and method selection. A structurally aligned
tree produced during the original traversal makes that semantic handoff
explicit and independently validatable.

## Consequences

- Inference plumbing may retain provisional nodes only when an opt-in producer
  requests them; inference-only operation avoids that allocation.
- Typed-core production is blocked by existing error diagnostics before
  profile or invariant results are considered.
- Source diagnostics, producer-profile failures, typed-core invariant failures,
  lowerer-profile failures, and lowered-IR invariant failures remain distinct.
- Lowerers consume validated typed core and may not inspect inference internals
  or interpreter runtime evidence.
- Complete typed-core production and production compiler integration are not
  implied by the bounded direct-call profile.
