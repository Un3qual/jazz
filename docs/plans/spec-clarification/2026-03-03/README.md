# Spec Clarification Plan Batch (2026-03-03)

## Progress

- [x] Re-audited old implementation (`jazz-hs`) and current specs/docs
- [x] Mapped already-covered clarification items (`1-12a`) to avoid duplication
- [x] Identified additional unresolved language-design domains
- [x] Created a domain evidence matrix (`domain-gap-matrix.md`)
- [x] Draft detailed execution plans for each new domain
- [x] Validate non-overlap assumptions with maintainer
- [x] Lock maintainer decision gates captured on 2026-03-03
- [x] Execute Phase-0 spec publication batch for domains 14/15/16 and tooling 18
- [x] Verify candidate-step implementation status for domains 13/14/15/16 against current `jazz-next` state before selecting next work batch
- [x] Execute domain 13 batch 1 in `jazz-next` (signature adjacency + use-before-definition diagnostics + tests)
- [x] Execute domain 13 batch 2 in `jazz-next` (mutual-recursion groups + forward-reference contract tests)
- [ ] Bootstrap `jazz-next` lexer/parser foundation before additional domain execution (next-agent start point)
- [ ] Execute plans (future work)

## Scope Boundary

This batch only includes clarification domains that are not already directly planned in:

- `docs/plans/spec-cleanup/2026-03-02`
- `docs/plans/spec-clarification/2026-03-02`

## New Domain Plans (This Batch)

- `semantics/13-binding-and-signature-coherence.md`
- `control-flow/14-if-expression-surface-and-semantics.md`
- `syntax/15-operator-fixity-and-sections.md`
- `runtime/16-primitive-semantics-contract.md`
- `governance/17-jazz2-alignment-and-spec-authority.md`

## Domain Execution Status

- [ ] 13 binding/signature coherence
- [x] 13a binding/signature coherence batch 1 (`jazz-next` analyzer/tests)
- [x] 13b binding/signature coherence batch 2 (`jazz-next` recursion-group semantics + tests)
- [ ] 14 if-expression surface and semantics
- [ ] 15 operator fixity and sections
- [ ] 16 primitive semantics contract
- [x] 17 jazz2 alignment and spec authority

## Implementation Status Verification (2026-03-03 Batch)

- [x] Verified candidate-step status for 13 (bindings/signatures) against current repo state.
- [x] Verified candidate-step status for 14 (if-expression) against current repo state.
- [x] Verified candidate-step status for 15 (operator/fixity/sections) against current repo state.
- [x] Verified candidate-step status for 16 (primitive semantics) against current repo state.
- [x] Verified candidate-step status for 18 (warning flags) against current repo state.

## Follow-On Plans

- `tooling/18-compiler-warning-flags.md` (implemented in `jazz-next`; retained as execution record)

## Next Agent Kickoff (Parser/Lexer First)

Before continuing domain 14/15/16 implementation, start with parser/lexer foundation in `jazz-next`:

1. Create a minimal lexer + parser surface in `jazz-next` with tests-first coverage for current accepted syntax slices.
2. Establish a parse-AST to analyzer-AST boundary so existing analyzer tests can consume parser output incrementally.
3. Implement domain 14 (`if`) and domain 15 (operators/sections) on top of that parser surface, not by extending analyzer-only hand-built AST fixtures.
4. If parser behavior exposes syntax ambiguities, update the relevant spec clarification docs in the same change before proceeding.

## Locked Governance Clarifications (2026-03-03)

- `jazz2` is reference-only and non-normative for current language behavior.
- Normative behavior is anchored to canonical specs under `docs/spec/*`, with active implementation behavior/tests as temporary authority where canonical spec sections are not yet written.
- Semantic language changes require decision records before implementation; non-semantic/internal changes may be implementation-first only with same-change docs/tests updates.
- Policy document: `docs/spec/governance/spec-authority-policy.md`

## Batch Design Notes

- Each plan includes explicit progress checkboxes and commit checkpoints.
- Each plan is detailed enough for independent execution, while preserving executor discretion for implementation-level tradeoffs.
- Decision gates are called out where maintainer intent is required before irreversible implementation changes.
