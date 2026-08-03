# Jazz Repository Canonicalization Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Replace the three-generation repository layout with one root-level `jazz` compiler while preserving an explicit archive point and keeping compiler behavior unchanged.

**Architecture:** The active contents of `jazz-next/` move to conventional root paths, the Haskell namespace becomes `Jazz.*`, and Cabal/Nix/CLI identities become `jazz`. The legacy implementations are removed from the default branch only after an annotated archive tag is created and pushed. Existing behavioral tests remain the safety net; new repository audits make the final layout and name invariants executable.

**Tech Stack:** Git, Bash, Haskell 2010, Cabal 3, GHC 9.14.1, Nix flakes, ripgrep

## Global Constraints

- Execute this workstream on a fresh `codex/repository-canonicalization` branch based on the merged productization-design branch.
- Before Task 3, obtain explicit maintainer authorization for this one-time exception to the current `AGENTS.md` legacy read-only and `jazz-next/` workspace policy. Approval of the design alone is not permission to delete or relocate guarded compiler trees; record the execution authorization in the task or pull-request history.
- The checked-out starting commit must still contain `jazz-hs/`, `jazz2/`, and `jazz-next/` and must be the exact commit tagged for archival.
- Push `archive/pre-root-canonicalization-2026-07-31` before committing any deletion. Stop if the remote tag exists at a different object.
- Use `git mv` and `git rm` so Git can retain rename history. Do not copy legacy implementations into another tracked directory.
- Do not change Jazz syntax, type rules, runtime behavior, diagnostics, or public standard-library behavior.
- Do not create a public Haskell library API; `jazz-internal` remains private.
- Leave the broad public-documentation rewrite and historical-plan curation to Workstream 2. Update only paths and authority statements required to make this migration truthful and green.
- Do not run the expensive full-parser-scale or complete benchmark workload until the final extended verification task.
- Commit after each task whose end state builds or passes its focused checks.

---

## Task 1: Record and protect the pre-migration state

**Files:**

- Inspect: `AGENTS.md`
- Inspect: `README.md`
- Inspect: `flake.nix`
- Inspect: `jazz-next/jazz-next.cabal`
- Inspect: `scripts/check-docs.sh`
- Inspect: `scripts/check-spec-authority.sh`
- Create remotely: annotated Git tag `archive/pre-root-canonicalization-2026-07-31`

- [ ] Fetch the current default branch and existing tags without rebasing the working branch:

  ```bash
  git fetch origin main --tags
  git status --short --branch
  git log -1 --decorate --oneline
  ```

  Expected: the worktree is clean, and the current branch is based on the reviewed default-branch commit that still contains all three compiler directories.

- [ ] Record the exact pre-migration object in the shell and verify the three layouts exist:

  ```bash
  archive_commit="$(git rev-parse HEAD)"
  printf '%s\n' "$archive_commit"
  test -d jazz-hs
  test -d jazz2
  test -d jazz-next
  test -f jazz-next/jazz-next.cabal
  ```

  Expected: all four `test` commands succeed and `archive_commit` contains the reviewed starting object. Keep this shell open through the tag steps.

- [ ] Check whether the archive tag already exists locally or remotely and fail closed on any mismatched peeled object:

  ```bash
  set -euo pipefail
  tag=archive/pre-root-canonicalization-2026-07-31
  local_tag_object=
  local_archive_commit=
  if git show-ref --verify --quiet "refs/tags/$tag"; then
    local_tag_object="$(git rev-parse "refs/tags/$tag")"
    local_archive_commit="$(git rev-parse "$tag^{}")"
    test "$(git cat-file -t "refs/tags/$tag")" = tag
  fi
  remote_tag_listing="$(git ls-remote --tags origin "refs/tags/$tag*")"
  remote_tag_object="$(printf '%s\n' "$remote_tag_listing" | awk -v ref="refs/tags/$tag" '$2 == ref { print $1 }')"
  remote_archive_commit="$(printf '%s\n' "$remote_tag_listing" | awk -v ref="refs/tags/$tag^{}" '$2 == ref { print $1 }')"
  test -z "$local_archive_commit" || test "$local_archive_commit" = "$archive_commit"
  test -z "$remote_tag_object" || test -n "$remote_archive_commit"
  test -n "$remote_tag_object" || test -z "$remote_archive_commit"
  test -z "$remote_archive_commit" || test "$remote_archive_commit" = "$archive_commit"
  ```

  Expected: an absent tag is safe to create; an existing local or remote tag is safe only when it is annotated and its peeled object equals `$archive_commit`. The single `ls-remote` call must succeed and the exact-ref parsing must distinguish the tag object from its returned `^{}` line; lookup errors and mismatches abort the migration.

- [ ] If the local tag is absent, either fetch the matching remote annotated tag or create it when both sides are absent. Then inspect it:

  ```bash
  if test -z "$local_tag_object" && test -n "$remote_tag_object"; then
    git fetch origin "refs/tags/$tag:refs/tags/$tag"
  elif test -z "$local_tag_object"; then
    git tag -a "$tag" "$archive_commit" -m "Archive repository before root canonicalization; preserves jazz-hs, jazz2, and jazz-next"
  fi
  git show --no-patch --decorate "$tag"
  test "$(git rev-parse "$tag^{}")" = "$archive_commit"
  ```

  Expected: the peeled tag object is exactly `$archive_commit`.

- [ ] Push the verified local tag only when the remote tag was absent, then remotely verify it before deleting anything:

  ```bash
  if test -z "$remote_tag_object"; then
    git push origin "refs/tags/$tag"
  fi
  remote_tag_listing="$(git ls-remote --tags origin "refs/tags/$tag*")"
  remote_archive_commit="$(printf '%s\n' "$remote_tag_listing" | awk -v ref="refs/tags/$tag^{}" '$2 == ref { print $1 }')"
  test "$remote_archive_commit" = "$archive_commit"
  ```

  Expected: the remote contains the annotated tag and its peeled object equals `$archive_commit`.

- [ ] Save `$archive_commit` and the pre-migration baseline outputs in the task notes and pull-request description, not in the repository. Record the commit independently before closing the shell; later verification must compare the tags to this recorded value rather than deriving the expected value from a tag under test:

  ```bash
  nix develop --command cabal build --project-dir=jazz-next all
  nix develop --command cabal test --project-dir=jazz-next all --test-show-details=direct
  bash scripts/check-docs.sh
  bash scripts/check-spec-authority.sh
  bash scripts/test-check-execution-queue.sh
  ```

  Expected: all baseline checks pass. If an unrelated baseline failure exists, document it and stop rather than hiding it inside the migration.

## Task 2: Add an executable canonical-layout contract

**Files:**

- Modify: `jazz-next/test/JazzNext/Repository/AuditSpec.hs`
- Modify: `jazz-next/test/JazzNext/Repository/Root.hs`
- Test: `jazz-next/test/JazzNext/Repository/AuditSpec.hs`

- [ ] Add repository-audit cases, using the current names initially, that assert the final repository root:

  - contains `jazz-next.cabal` only until the rename step changes the expectation to `jazz.cabal`;
  - does not contain `jazz-hs/`, `jazz2/`, or a nested `jazz-next/` directory;
  - contains `app/`, `benchmark/`, `editors/`, `jazz/`, `program-support/`, `programs/`, `src/`, and `test/`; and
  - does not rely on a parent-directory fallback to find the package.

- [ ] Run the focused repository suite and confirm the new final-layout assertions fail against the old layout:

  ```bash
  nix develop --command cabal test --project-dir=jazz-next repository-audit-spec --test-show-details=direct
  ```

  Expected: failure names the legacy or nested directory that violates the final layout.

- [ ] Keep the existing `findJazzNextPackageRoot` name, `jazz-next.cabal` marker, and nested-package fallback while the focused red test still runs against the pre-move layout. Task 4, after Task 3 creates root `jazz.cabal`, renames the API to `findJazzPackageRoot`, changes the marker, and removes the `ancestor </> "jazz-next"` fallback.

- [ ] Keep the expected final-layout assertions failing until Tasks 3 and 4 complete; do not commit this intentionally red intermediate state by itself.

## Task 3: Remove the legacy implementations and promote active files

**Files:**

- Delete: `jazz-hs/**`
- Delete: `jazz2/**`
- Move: `jazz-next/app/**` -> `app/**`
- Move: `jazz-next/benchmark/**` -> `benchmark/**`
- Move: `jazz-next/editors/**` -> `editors/**`
- Move: `jazz-next/jazz/**` -> `jazz/**`
- Move: `jazz-next/program-support/**` -> `program-support/**`
- Move: `jazz-next/programs/**` -> `programs/**`
- Move: `jazz-next/src/**` -> `src/**`
- Move: `jazz-next/test/**` -> `test/**`
- Move: `jazz-next/cabal.project` -> `cabal.project`
- Move: `jazz-next/cabal.project.profile-hotspots` -> `cabal.project.profile-hotspots`
- Move: `jazz-next/cabal.project.profile-stages` -> `cabal.project.profile-stages`
- Move: `jazz-next/PERFORMANCE.md` -> `PERFORMANCE.md`
- Move: `jazz-next/jazz-next.cabal` -> `jazz.cabal`
- Move and retitle: `jazz-next/README.md` -> `docs/compiler/architecture.md`

- [ ] Verify none of the target root paths already contains unrelated content:

  ```bash
  for path in app benchmark editors jazz program-support programs src test cabal.project cabal.project.profile-hotspots cabal.project.profile-stages jazz.cabal PERFORMANCE.md docs/compiler/architecture.md; do test ! -e "$path"; done
  ```

  Expected: every target is absent. `README.md`, `LICENSE`, `docs/`, `scripts/`, and the root Nix files remain in place.

- [ ] Create `docs/compiler/` and use Git-aware moves for every active compiler path listed above. Move the active compiler README to `docs/compiler/architecture.md` so it cannot overwrite the root README.

- [ ] Remove the two legacy trees with Git:

  ```bash
  git rm -r jazz-hs jazz2
  ```

- [ ] Remove the now-empty `jazz-next/` directory and verify the structural end state:

  ```bash
  test ! -e jazz-hs
  test ! -e jazz2
  test ! -e jazz-next
  test -f jazz.cabal
  test -f src/JazzNext/Compiler/Driver.hs
  test -f jazz/stdlib/Prelude.jz
  test -f test/JazzNext/Repository/AuditSpec.hs
  ```

- [ ] Inspect Git's rename detection before changing file contents:

  ```bash
  git status --short
  git diff --summary --find-renames=50%
  ```

  Expected: active files appear as moves where possible; legacy files appear as deletions.

## Task 4: Canonicalize Cabal, Haskell, CLI, and filesystem identities

**Files:**

- Move: `src/JazzNext/**` -> `src/Jazz/**`
- Move: `test/JazzNext/**` -> `test/Jazz/**`
- Move: `program-support/JazzNext/**` -> `program-support/Jazz/**`
- Move: `benchmark/JazzNext/**` -> `benchmark/Jazz/**`
- Modify: `app/Main.hs`
- Modify: `jazz.cabal`
- Modify: `cabal.project`
- Modify: `cabal.project.profile-hotspots`
- Modify: `cabal.project.profile-stages`
- Modify: all moved `*.hs` sources under `src/`, `test/`, `program-support/`, and `benchmark/`
- Modify: user-visible product labels in fixtures and scripts under `test/`, `programs/`, and `benchmark/`

- [ ] Move each `JazzNext/` directory to `Jazz/` with `git mv`.

- [ ] Replace Haskell module declarations and imports from `JazzNext` to `Jazz` in all moved Haskell sources. Rename `findJazzNextPackageRoot` to `findJazzPackageRoot` and update its callers.

- [ ] Update `jazz.cabal` exactly as follows:

  - package `name: jazz`;
  - private library `library jazz-internal`;
  - executable `executable jazz`;
  - benchmark `benchmark jazz-bench`;
  - every internal dependency becomes `jazz:jazz-internal`;
  - every exposed and other module uses `Jazz.*`;
  - generated module and autogen module become `Paths_jazz`;
  - package description refers to the Jazz compiler, not an active rewrite.

- [ ] Update Cabal project files so `package jazz` receives the development and profiling flags.

- [ ] Replace product-owned labels such as temporary directory prefixes, profiling event names, benchmark metadata, CLI help headers, test-suite descriptions, and error text when `jazz-next` means the current product. Preserve no compatibility alias for the `jazz-next` executable.

- [ ] Verify no active compiler identity remains outside intentionally historical documentation:

  ```bash
  if rg -n "jazz-next|JazzNext|Paths_jazz_next" app benchmark editors jazz program-support programs src test jazz.cabal cabal.project cabal.project.profile-hotspots cabal.project.profile-stages PERFORMANCE.md; then
    exit 1
  else
    test "$?" -eq 1
  fi
  ```

  Expected: no matches.

- [ ] Build before running tests:

  ```bash
  nix develop --command cabal build all
  ```

  Expected: Cabal resolves package `jazz`, private library `jazz-internal`, and executable `jazz` with warnings treated as errors.

- [ ] Run the focused repository and CLI tests:

  ```bash
  nix develop --command cabal test repository-audit-spec cli-spec --test-show-details=direct
  ```

  Expected: canonical-layout, package-root, package-policy, and CLI identity assertions pass.

- [ ] Commit the structural and identity migration:

  ```bash
  git add -A
  git commit -m "refactor: canonicalize Jazz repository root"
  ```

## Task 5: Repair Nix, ignore rules, repository guidance, and audits

**Files:**

- Modify: `flake.nix`
- Modify: `.gitignore`
- Modify: `AGENTS.md`
- Modify: `scripts/check-docs.sh`
- Modify: `scripts/check-spec-authority.sh`
- Modify: `scripts/check-clarification-specs.sh`
- Modify: `scripts/check-execution-queue.py`
- Modify: `scripts/check-execution-queue.sh`
- Modify: `scripts/test-check-execution-queue.sh`
- Modify: active files under `docs/execution/**`, `docs/spec/**`, `docs/feature-status.md`, `docs/jazz-language-state.md`, and `docs/jazz-improvement-backlog.md` only where path truth is required
- Test: `test/Jazz/Repository/AuditSpec.hs`

- [ ] Change `flake.nix` to call `callCabal2nix "jazz" ./. { }`, wrap that derivation with `pkgs.haskell.lib.enableCabalFlag ... "development"`, bind the warning-clean result as `jazz`, and expose it through `checks.jazz-test-suite`. Leave `packages.default` and `apps.default` to the alpha-release preparation workstream.

- [ ] Replace nested build ignores with root paths:

  ```text
  dist-newstyle/
  dist-newstyle-profile-*/
  benchmark-results/
  profile-results/
  website/node_modules/
  website/build/
  ```

  Keep existing Python cache ignores.

- [ ] Rewrite `AGENTS.md` so root `src/`, `jazz/`, `app/`, and `test/` are the only active compiler paths. Remove the legacy read-only policy because the legacy directories no longer exist. Keep the instruction to commit along the way.

- [ ] Replace `scripts/check-spec-authority.sh` legacy-authority assertions with the accepted authority order:

  1. `docs/language/` and `docs/reference/` once Workstream 2 creates them;
  2. current `src/`, `jazz/`, and `test/` behavior;
  3. accepted RFCs; and
  4. roadmap material as non-normative.

  During this workstream, the check may accept the still-current `docs/spec/` path as the public contract, but it must reject claims that `jazz-hs`, `jazz2`, or `jazz-next` are active authority. Workstream 2 removes the transition allowance.

- [ ] Update `scripts/check-docs.sh` and queue scripts only enough to follow canonical root paths. Do not move execution documents yet.

- [ ] Treat Markdown path updates as part of this migration: before the root move, active implementation references must still point to `jazz-next/...`; after Tasks 3 and 4, update active status, specification, and repository-guidance references to root paths such as `src/Jazz/...`, `test/Jazz/...`, and `jazz/stdlib/...`. Historical implementation plans may retain old names until Workstream 2 deletes or curates them.

- [ ] Add repository-audit assertions that read the root `flake.nix`, `.gitignore`, `AGENTS.md`, Cabal files, and active scripts, rejecting `jazz-next`, `JazzNext`, `Paths_jazz_next`, `jazz-hs`, and `jazz2` as live product paths.

- [ ] Run focused infrastructure checks:

  ```bash
  nix flake check
  bash scripts/check-docs.sh
  bash scripts/check-spec-authority.sh
  bash scripts/check-clarification-specs.sh
  bash scripts/check-execution-queue.sh
  bash scripts/test-check-execution-queue.sh
  nix develop --command cabal test repository-audit-spec --test-show-details=direct
  ```

  Expected: all checks pass from repository root.

- [ ] Audit non-historical tracked files for obsolete identity:

  ```bash
  if git grep -n -E "jazz-next|JazzNext|Paths_jazz_next|jazz-hs|jazz2" -- \
    . \
    ':(exclude)docs/plans/**' \
    ':(exclude)docs/superpowers/**' \
    ':(exclude).codex/plans/**'; then
    exit 1
  else
    test "$?" -eq 1
  fi
  ```

  Expected: no active product-path claims. Any hits must be either corrected now or explicitly documented as historical material scheduled for Workstream 2 removal.

- [ ] Commit build and repository-policy repairs:

  ```bash
  git add -A
  git commit -m "build: target the canonical Jazz package"
  ```

## Task 6: Verify behavioral equivalence with the ordinary matrix

**Files:**

- Modify only if a root-move defect is found: files already in scope above

- [ ] Run formatting and package validation:

  ```bash
  git ls-files '*.hs' | xargs nix develop --command ormolu --mode check
  nix develop --command cabal check
  git diff --check
  ```

  Expected: no formatting drift, package warnings, or whitespace errors.

- [ ] Run the warning-clean build and complete ordinary test matrix:

  ```bash
  nix develop --command cabal clean
  nix develop --command cabal build all
  nix develop --command cabal test all --test-show-details=direct
  ```

  Expected: every default test component passes; the manual `full-parser-scale` flag remains disabled.

- [ ] Exercise the renamed CLI from the repository root:

  ```bash
  nix develop --command cabal run jazz -- --help
  ```

  Expected: the command succeeds and user-visible output says `jazz`, never `jazz-next`.

- [ ] Run all root repository validators again:

  ```bash
  bash scripts/check-docs.sh
  bash scripts/check-spec-authority.sh
  bash scripts/check-clarification-specs.sh
  bash scripts/check-execution-queue.sh
  bash scripts/test-check-execution-queue.sh
  nix flake check
  ```

  Expected: all pass.

- [ ] If fixes were required, commit them separately:

  ```bash
  git add -A
  git commit -m "test: verify canonical repository layout"
  ```

## Task 7: Run the one-time extended migration gate

**Files:**

- Create as untracked artifacts only: `benchmark-results/**`, `profile-results/**`
- Do not commit generated benchmark or profile output

- [ ] Run the exhaustive parser-scale components once with the manual flag enabled:

  ```bash
  nix develop --command cabal test all --flags=+full-parser-scale --test-show-details=direct
  ```

  Expected: the ordinary matrix and all full-scale parser components pass. This command is intentionally not part of routine PR CI.

- [ ] Build the profiling configuration and run its existing smoke commands documented in `PERFORMANCE.md`:

  ```bash
  nix develop --command cabal build all --project-file=cabal.project.profile-stages
  nix develop --command cabal build all --project-file=cabal.project.profile-hotspots
  ```

  Expected: both profiling configurations build under the canonical package and module names.

- [ ] Run one bounded benchmark smoke rather than treating timing as a regression threshold:

  ```bash
  nix develop --command cabal bench jazz-bench --benchmark-options='--jazz-smoke'
  ```

  Expected: the benchmark completes and emits canonical `jazz` metadata. Timing differences are recorded for review but do not fail this migration by percentage.

- [ ] Confirm ignored generated outputs and intended tracked state:

  ```bash
  git status --short
  git diff --check
  ```

  Expected: no build, benchmark, or profile output is tracked; the worktree is clean after any final verification commit.

## Task 8: Publish the independently reviewable canonicalization change

**Files:**

- No new files

- [ ] Review the complete branch diff with rename detection:

  ```bash
  git diff --stat origin/main...HEAD
  git diff --summary --find-renames=50% origin/main...HEAD
  git log --oneline origin/main..HEAD
  ```

  Expected: the diff contains repository moves, deletions, and identity/path repairs only—no language-semantic feature work.

- [ ] Verify the archive tag one last time:

  ```bash
  set -euo pipefail
  tag=archive/pre-root-canonicalization-2026-07-31
  : "${recorded_archive_commit:?copy the exact Task 1 commit from the task or PR record}"
  local_archive_commit="$(git rev-parse "$tag^{}")"
  remote_tag_listing="$(git ls-remote --tags origin "refs/tags/$tag*")"
  remote_archive_commit="$(printf '%s\n' "$remote_tag_listing" | awk -v ref="refs/tags/$tag^{}" '$2 == ref { print $1 }')"
  test -n "$remote_archive_commit"
  test "$local_archive_commit" = "$recorded_archive_commit"
  test "$remote_archive_commit" = "$recorded_archive_commit"
  ```

  Expected: `recorded_archive_commit == local peeled tag == remote peeled tag`; the expected commit comes from the independent Task 1 record, and any lookup failure aborts verification.

- [ ] Push `codex/repository-canonicalization` and open a dedicated pull request. The PR description must include the archive tag, ordinary and extended verification results, and a statement that compiler semantics are unchanged.

- [ ] Report the obsolete draft Copilot-instructions pull request as superseded and ready for maintainer closure. Do not close it automatically and do not merge its old-layout instructions.
