# Spec Clarification Plan Batch (2026-03-02)

## Progress

- [x] Identified language-design ambiguity domains not fully covered by existing spec-cleanup items #1-#6
- [x] Dispatched specialist subagents in parallel by expertise area
- [x] Produced one detailed plan file per domain
- [x] Organized plans by domain subfolders
- [x] Included checkbox progress tracking in each plan
- [x] Included commit checkpoints in each plan
- [x] Included Nix reproducibility commands in each plan
- [x] Locked clarifications from maintainer decisions (2026-03-02)
- [ ] Execute clarification plans (future work)

## Scope Boundary

These plans cover additional clarity work beyond current spec-cleanup plans in:
- `/Users/admin/.codex/worktrees/8c77/jazz-main/docs/plans/spec-cleanup/2026-03-02`

## Domain Plans

- [07-type-grammar-and-arrow-associativity.md](/Users/admin/.codex/worktrees/8c77/jazz-main/docs/plans/spec-clarification/2026-03-02/type-system/07-type-grammar-and-arrow-associativity.md)
- [08-trait-vocabulary-and-capability-model.md](/Users/admin/.codex/worktrees/8c77/jazz-main/docs/plans/spec-clarification/2026-03-02/abstractions/08-trait-vocabulary-and-capability-model.md)
- [09-module-loader-and-import-resolution.md](/Users/admin/.codex/worktrees/8c77/jazz-main/docs/plans/spec-clarification/2026-03-02/modules/09-module-loader-and-import-resolution.md)
- [10-stdlib-boundary-selfhosted-vs-hardcoded.md](/Users/admin/.codex/worktrees/8c77/jazz-main/docs/plans/spec-clarification/2026-03-02/stdlib/10-stdlib-boundary-selfhosted-vs-hardcoded.md)
- [11-adt-and-pattern-matching-positioning.md](/Users/admin/.codex/worktrees/8c77/jazz-main/docs/plans/spec-clarification/2026-03-02/semantics/11-adt-and-pattern-matching-positioning.md)
- [12-backend-target-strategy.md](/Users/admin/.codex/worktrees/8c77/jazz-main/docs/plans/spec-clarification/2026-03-02/runtime/12-backend-target-strategy.md)
- [12a-haskell-interpreter-implementation.md](/Users/admin/.codex/worktrees/8c77/jazz-main/docs/plans/spec-clarification/2026-03-02/runtime/12a-haskell-interpreter-implementation.md)

## Locked Clarifications (Approved 2026-03-02)

- [x] ADT and pattern matching positioning: `CORE`
- [x] Backend target strategy: `Haskell interpreter only` for now
- [x] JavaScript backend: out of active scope
- [x] LLVM backend: out of active scope

## Planning Notes

- All plans are written to allow executor research-based decisions while preserving concrete checkpoints.
- All plans include verification evidence anchored to current repository files.
- Where scope likely branches, plans include explicit suggested sub-plan file paths.
