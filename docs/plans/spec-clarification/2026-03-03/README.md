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
- [x] Bootstrap `jazz-next` lexer/parser foundation before additional domain execution (surface parser + parse/lower boundary + tests)
- [x] Execute domain 14 batch 1 in `jazz-next` (`if` parser/lowering/type-contract tests + diagnostics)
- [x] Execute domain 14 batch 2 in `jazz-next` (`if` desugaring + source compile/CLI pipeline alignment + tests)
- [x] Execute domain 15 batch 1 in `jazz-next` (operator fixity/sections parser + AST/lowering + tests)
- [x] Execute domain 16 batch 1 in `jazz-next` (primitive conformance tests + strict-equality mismatch diagnostics)
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
- [x] 14a if-expression parser + type-contract batch (`jazz-next` parser/AST/lowering/type diagnostics + tests)
- [x] 14b if-expression desugaring + source compile/CLI pipeline batch (`jazz-next` desugar pass + source-entry driver path + tests)
- [x] 15 operator fixity and sections
- [x] 15a operator fixity/sections parser-alignment batch (`jazz-next` operator table + precedence parser + section AST/tests)
- [ ] 16 primitive semantics contract
- [x] 16a primitive semantics phase-1 conformance batch (`jazz-next` primitive test suite + `E2004` strict-equality mismatch diagnostics)
- [x] 17 jazz2 alignment and spec authority

## Implementation Status Verification (2026-03-03 Batch)

- [x] Verified candidate-step status for 13 (bindings/signatures) against current repo state.
- [x] Verified candidate-step status for 14 (if-expression) against current repo state.
- [x] Verified candidate-step status for 15 (operator/fixity/sections) against current repo state.
- [x] Verified candidate-step status for 16 (primitive semantics) against current repo state.
- [x] Verified candidate-step status for 18 (warning flags) against current repo state.

## Follow-On Plans

- `tooling/18-compiler-warning-flags.md` (implemented in `jazz-next`; retained as execution record)

## Next Agent Kickoff (Post-Bootstrap)

Parser/lexer foundation is now in place in `jazz-next`. Continue with domain execution on that baseline:

1. Complete remaining domain 14 work (runtime branch semantics alignment in active `jazz-next` backend path).
2. Continue domain 16 runtime alignment beyond phase-1 tests (`map`/`hd`/`tl` semantics + runtime fatal diagnostic path in active `jazz-next` pipeline).
3. If parser behavior exposes syntax ambiguities, update the relevant spec clarification docs in the same change before proceeding.

## Locked Governance Clarifications (2026-03-03)

- `jazz2` is reference-only and non-normative for current language behavior.
- Normative behavior is anchored to canonical specs under `docs/spec/*`, with active implementation behavior/tests as temporary authority where canonical spec sections are not yet written.
- Semantic language changes require decision records before implementation; non-semantic/internal changes may be implementation-first only with same-change docs/tests updates.
- Policy document: `docs/spec/governance/spec-authority-policy.md`

## Batch Design Notes

- Each plan includes explicit progress checkboxes and commit checkpoints.
- Each plan is detailed enough for independent execution, while preserving executor discretion for implementation-level tradeoffs.
- Decision gates are called out where maintainer intent is required before irreversible implementation changes.
