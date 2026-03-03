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

## Batch Design Notes

- Each plan includes explicit progress checkboxes and commit checkpoints.
- Each plan is detailed enough for independent execution, while preserving executor discretion for implementation-level tradeoffs.
- Decision gates are called out where maintainer intent is required before irreversible implementation changes.
