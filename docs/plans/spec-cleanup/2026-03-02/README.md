# Spec Cleanup Plan Batch (2026-03-02)

## Progress

- [x] Verified unresolved cleanup items from `/Users/admin/.codex/worktrees/8c77/jazz-main/docs/jazz-language-state.md`
- [x] Dispatched one subagent per unresolved item
- [x] Generated one detailed plan file per unresolved item
- [x] Organized plans by subfolder (`decisions/`, `documentation/`, `compiler/`)
- [x] Included Nix environment considerations in every plan
- [x] Locked cross-item product decisions from maintainer approval (2026-03-02)
- [ ] Execute plans (future work)

## Item Status (Verified Unresolved)

1. [x] Authoritative syntax for functions/modules/traits/collections is not finalized
2. [x] `map`/`filter` argument order is not finalized
3. [x] `!` purity semantics are not finalized
4. [x] Canonical abstraction keyword (`trait` vs `class`) is not finalized
5. [x] Top-level docs do not cleanly separate implemented vs planned
6. [x] Parse-only feature handling (`remove` vs `fully implement`) is not finalized

## Locked Decisions (Approved 2026-03-02)

- [x] Canonical abstraction keyword: `class` (`trait` becomes non-canonical/deprecated alias during migration)
- [x] Canonical `map`/`filter` order: function-first (`map f xs`, `filter p xs`)
- [x] `!` purity semantics: compiler-enforced, delivered first as a stubbed V1 enforcement pass

## Plan Files

- [01-authoritative-syntax.md](/Users/admin/.codex/worktrees/8c77/jazz-main/docs/plans/spec-cleanup/2026-03-02/decisions/01-authoritative-syntax.md)
- [02-map-filter-order.md](/Users/admin/.codex/worktrees/8c77/jazz-main/docs/plans/spec-cleanup/2026-03-02/decisions/02-map-filter-order.md)
- [03-purity-bang-semantics.md](/Users/admin/.codex/worktrees/8c77/jazz-main/docs/plans/spec-cleanup/2026-03-02/decisions/03-purity-bang-semantics.md)
- [04-trait-vs-class-keyword.md](/Users/admin/.codex/worktrees/8c77/jazz-main/docs/plans/spec-cleanup/2026-03-02/decisions/04-trait-vs-class-keyword.md)
- [05-readme-implemented-vs-planned.md](/Users/admin/.codex/worktrees/8c77/jazz-main/docs/plans/spec-cleanup/2026-03-02/documentation/05-readme-implemented-vs-planned.md)
- [06-parse-only-features-resolution.md](/Users/admin/.codex/worktrees/8c77/jazz-main/docs/plans/spec-cleanup/2026-03-02/compiler/06-parse-only-features-resolution.md)

## Nix Coordination Note

Before executing any item plan, create or adopt one repo-level Nix entrypoint (`flake.nix` preferred) and ensure each plan runs verification through `nix develop -c ...` for reproducibility.
