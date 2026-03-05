# Spec Cleanup Plan Batch (2026-03-02)

## Progress

- [x] Verified unresolved cleanup items from `docs/jazz-language-state.md`
- [x] Dispatched one subagent per unresolved item
- [x] Generated one detailed plan file per unresolved item
- [x] Organized plans by subfolder (`decisions/`, `documentation/`, `compiler/`)
- [x] Included Nix environment considerations in every plan
- [x] Locked cross-item product decisions from maintainer approval (2026-03-02)
- [x] Started execution with item #1 task #1 (repo-level Nix flake bootstrap)
- [x] Started execution with item #1 task #2 (authoritative syntax decision record)
- [ ] Execute plans (future work)

## Item Status At Plan Creation (Verified Unresolved)

1. [x] Authoritative syntax for functions/modules/traits/collections is not finalized
2. [x] `map`/`filter` argument order is not finalized
3. [x] `!` purity semantics are not finalized
4. [x] Canonical abstraction keyword (`trait` vs `class`) is not finalized
5. [x] Top-level docs do not cleanly separate implemented vs planned
6. [x] Parse-only feature handling (`remove` vs `fully implement`) is not finalized

## Resolution Updates Since Plan Creation

- [x] Item #3 (`!` purity semantics) is now implemented in active `jazz-next` with stub-v1 enforcement and documented contract:
  - `docs/spec/semantics/purity-bang-stub-v1.md`
  - `docs/plans/spec-cleanup/2026-03-02/decisions/03-purity-bang-semantics.md`

## Locked Decisions (Approved 2026-03-02)

- [x] Canonical abstraction keyword: `class` (`trait` becomes non-canonical/deprecated alias during migration)
- [x] Canonical `map`/`filter` order: function-first (`map f xs`, `filter p xs`)
- [x] `!` purity semantics: compiler-enforced, delivered first as a stubbed V1 enforcement pass

## Plan Files

- [01-authoritative-syntax.md](decisions/01-authoritative-syntax.md)
- [02-map-filter-order.md](decisions/02-map-filter-order.md)
- [03-purity-bang-semantics.md](decisions/03-purity-bang-semantics.md)
- [04-trait-vs-class-keyword.md](decisions/04-trait-vs-class-keyword.md)
- [05-readme-implemented-vs-planned.md](documentation/05-readme-implemented-vs-planned.md)
- [06-parse-only-features-resolution.md](compiler/06-parse-only-features-resolution.md)

## Nix Coordination Note

Before executing any item plan, create or adopt one repo-level Nix entrypoint (`flake.nix` preferred) and ensure each plan runs verification through `nix develop -c ...` for reproducibility.
