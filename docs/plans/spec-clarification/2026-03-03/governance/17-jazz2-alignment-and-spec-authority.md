# Jazz2 Alignment and Spec Authority Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Remove governance ambiguity between `jazz-hs` and `jazz2` by defining an explicit authority model for specs, implementation, and migration decisions.

**Architecture:** Establish a formal source-of-truth hierarchy and a change-control policy for how `jazz2` may influence active language semantics. Apply policy across docs so future contributors cannot unintentionally treat unfinished `jazz2` sketches as normative behavior.

**Tech Stack:** Repository docs (`README`, `docs/jazz-language-state.md`, planning docs), optional CI/docs checks.

---

## Progress

- [x] Drift evidence collected (`jazz2` intent vs active implementation)
- [ ] Governance policy decision locked
- [ ] Source-of-truth hierarchy documented
- [ ] Docs aligned across repo roots
- [ ] Anti-drift enforcement added

## Verification Evidence (Current Ambiguity)

- `jazz2/src/Jazz/Parser.hs`: parser module is effectively empty.
- `jazz2/src/Jazz/Parser/Lexer.hs`: lexer is `undefined`.
- `jazz2/src/Jazz/AST.hs`: contains a different structural direction than active `jazz-hs` AST.
- `docs/jazz-language-state.md`: describes `jazz2` as unfinished, but no formal policy defines whether it is archival, experimental, or migration-target.

## Scope Guardrails

In scope:
- authority policy for language behavior claims,
- explicit status of `jazz2`,
- migration/change-control process between implementations.

Out of scope:
- immediate rewrite of `jazz2`,
- forcing feature parity across both implementations in this phase.

## Decision Gates

- [ ] Gate A: `jazz2` status.
  - Option A1: archival reference only.
  - Option A2: experimental design sandbox with explicit non-normative status.
  - Option A3: active migration target with milestone commitments.
- [ ] Gate B: source-of-truth hierarchy.
  - Option B1: `jazz-hs` behavior + `docs/spec/*` are normative.
  - Option B2: docs-only normative with implementation treated as lagging target.
  - Option B3: dual-source model (discouraged due drift risk).
- [ ] Gate C: acceptance workflow for semantic changes inspired by `jazz2`.
  - Option C1: RFC-style decision record required before implementation.
  - Option C2: implementation-first allowed if tests/docs updated in same change.

## Phase 0: Governance Policy Draft

- [ ] Write an explicit policy doc covering gates A/B/C and approved process.
- [ ] Include examples of allowed vs disallowed cross-implementation claims.

Create:
- `docs/spec/governance/spec-authority-policy.md`

### Commit Checkpoint (Phase 0)

```bash
git add docs/spec/governance/spec-authority-policy.md \
  docs/plans/spec-clarification/2026-03-03/governance/17-jazz2-alignment-and-spec-authority.md
git commit -m "docs(governance): define Jazz spec authority policy"
```

## Phase 1: Documentation Convergence

- [ ] Update top-level README to reflect chosen `jazz2` status clearly.
- [ ] Update `docs/jazz-language-state.md` with source-of-truth hierarchy.
- [ ] Update planning index docs to link policy for future plans.

Modify:
- `README.md`
- `docs/jazz-language-state.md`
- `docs/plans/spec-clarification/2026-03-03/README.md`

### Commit Checkpoint (Phase 1)

```bash
git add README.md docs/jazz-language-state.md docs/plans/spec-clarification/2026-03-03/README.md
git commit -m "docs(governance): align repository docs to spec authority policy"
```

## Phase 2: Anti-Drift Guardrails

- [ ] Add lightweight docs check that flags unsupported normative claims in `jazz2` paths.
- [ ] Add maintenance checklist for semantic changes:
  - update policy references,
  - update authoritative spec docs,
  - update tests in normative implementation.

Create/Modify:
- `scripts/check-spec-authority.sh`
- `docs/spec/governance/spec-authority-policy.md`
- optional CI workflow or precommit hook file if repository policy allows.

### Commit Checkpoint (Phase 2)

```bash
git add scripts/check-spec-authority.sh docs/spec/governance/spec-authority-policy.md
git commit -m "chore(governance): add spec-authority anti-drift checks"
```

## Verification Commands

```bash
rg -n "jazz2|source of truth|normative|authoritative" README.md docs/jazz-language-state.md docs/spec/governance
bash scripts/check-spec-authority.sh
```

## Definition of Done

- [ ] `jazz2` status is explicit and unambiguous.
- [ ] Source-of-truth hierarchy is documented and linked from top-level docs.
- [ ] Change-control policy exists for adopting `jazz2` ideas.
- [ ] Anti-drift checks are in place.
