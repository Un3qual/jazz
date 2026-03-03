# Spec Clarification Plan Batch (2026-03-03)

## Progress

- [x] Re-audited old implementation (`jazz-hs`) and current specs/docs
- [x] Mapped already-covered clarification items (`1-12a`) to avoid duplication
- [x] Identified additional unresolved language-design domains
- [x] Created a domain evidence matrix (`domain-gap-matrix.md`)
- [x] Draft detailed execution plans for each new domain
- [x] Validate non-overlap assumptions with maintainer
- [x] Lock maintainer decision gates captured on 2026-03-03
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
- [ ] 14 if-expression surface and semantics
- [ ] 15 operator fixity and sections
- [ ] 16 primitive semantics contract
- [x] 17 jazz2 alignment and spec authority

## Follow-On Plans

- `tooling/18-compiler-warning-flags.md` (optional warning-flag infrastructure requested by item 13 decision lock)

## Locked Governance Clarifications (2026-03-03)

- `jazz2` is reference-only and non-normative for current language behavior.
- Normative behavior is anchored to canonical specs under `docs/spec/*`, with active implementation behavior/tests as temporary authority where canonical spec sections are not yet written.
- Semantic language changes require decision records before implementation; non-semantic/internal changes may be implementation-first only with same-change docs/tests updates.
- Policy document: `docs/spec/governance/spec-authority-policy.md`

## Batch Design Notes

- Each plan includes explicit progress checkboxes and commit checkpoints.
- Each plan is detailed enough for independent execution, while preserving executor discretion for implementation-level tradeoffs.
- Decision gates are called out where maintainer intent is required before irreversible implementation changes.
