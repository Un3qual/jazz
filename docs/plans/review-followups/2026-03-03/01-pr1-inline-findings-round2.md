# PR #1 Inline Findings Follow-up (Round 2)

## Context

This checklist tracks verification and fixes for the second batch of CodeRabbit inline findings on PR #1.

## Progress

- [x] Gathered and re-verified all reported findings against current branch contents.
- [x] Applied spec-clarification path/style fixes (`README`, `09`, `13`, `14`, `15`).
- [x] Applied spec-cleanup path portability fixes (`01`, `02`).
- [x] Applied spec-cleanup stale-Nix/path fixes (`04`, `05`).
- [x] Re-ran grep-based validation for all reported patterns.
- [x] Committed fixes in logical batches.

## Findings Checklist

- [x] `docs/plans/spec-clarification/2026-03-02/modules/09-module-loader-and-import-resolution.md`
  - remove `jazz-hs/flake.nix` and `jazz-hs/flake.lock` references
  - update `nix ... develop jazz-hs -c ...` commands to root-flake form
- [x] `docs/plans/spec-clarification/2026-03-02/README.md`
  - convert absolute local links and scope paths to repo-relative
- [x] `docs/plans/spec-clarification/2026-03-03/control-flow/14-if-expression-surface-and-semantics.md`
  - resolve markdown list/fence formatting issues (ordered list style + fenced block spacing)
- [x] `docs/plans/spec-clarification/2026-03-03/semantics/13-binding-and-signature-coherence.md`
  - neutralize workflow-coupled opener text
  - change `Self recursion` to `Self-recursion`
- [x] `docs/plans/spec-clarification/2026-03-03/syntax/15-operator-fixity-and-sections.md`
  - neutralize workflow-coupled opener text
  - replace `Builtin` with `Built-in`
- [x] `docs/plans/spec-cleanup/2026-03-02/decisions/01-authoritative-syntax.md`
  - replace absolute paths with repo-relative paths
  - convert absolute command snippets to portable repo-relative commands
- [x] `docs/plans/spec-cleanup/2026-03-02/decisions/02-map-filter-order.md`
  - replace absolute paths with repo-relative paths
  - convert command snippets/search commands to portable forms
- [x] `docs/plans/spec-cleanup/2026-03-02/decisions/04-trait-vs-class-keyword.md`
  - replace absolute paths with repo-relative paths
  - update stale Nix environment text to reflect root flake presence
- [x] `docs/plans/spec-cleanup/2026-03-02/documentation/05-readme-implemented-vs-planned.md`
  - replace absolute paths with repo-relative paths
  - update stale root-Nix-shell guidance to root-flake guidance

## Verification Commands

```bash
rg -n '/Users/admin/.codex/worktrees/8c77/jazz-main|jazz-hs/flake.nix|jazz-hs/flake.lock|develop jazz-hs -c|For Claude|Self recursion|\bBuiltin\b|If no root Nix shell exists|repo has no `flake.nix`/`shell.nix`' docs/plans/spec-clarification docs/plans/spec-cleanup
```

## Commit Log

- [x] `be4d0d5` `docs(spec-clarification): normalize flake targets links and terminology`
- [x] `e11a940` `docs(spec-cleanup): replace workstation paths and stale nix assumptions`
