# Jazz Project Operations Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add professional project metadata, contributor workflows, tiered continuous integration, and repeatable alpha-release artifacts without putting long performance workloads on pull requests.

**Architecture:** Small checked scripts own each verification tier, and GitHub Actions only orchestrates those scripts. Pull requests run a warning-clean build plus a focused functional matrix when compiler-relevant paths change, while documentation-only pull requests run docs/site checks without compiling Haskell. The complete ordinary suite runs on `main`; exhaustive parser scale, corpus repetition, profiling, and benchmarks run weekly or manually; release candidates reuse every tier and package reproducible artifacts.

**Tech Stack:** GitHub Actions, Bash, Python 3, Nix flakes, Cabal, GHC 9.14.1, npm, Docusaurus, GitHub issue forms

## Global Constraints

- Execute on `codex/project-operations`, based on the merged Docusaurus workstream.
- The repository-canonicalization workstream is therefore already merged: root `jazz.cabal`, `cabal.project`, `src/`, `jazz/`, and the `.#jazz` Nix package are prerequisites, not artifacts introduced by this plan.
- The fast pull-request tier must not invoke `cabal bench`, `jazz-bench`, enable `full-parser-scale`, use either profiling Cabal project, or run the complete `program-corpus-spec` workload.
- The pull-request target is ten minutes or less. Use focused tests and safe caches; do not raise the expected duration to normalize a slow suite.
- The default `main` tier runs every ordinary Cabal suite but excludes the opt-in complete program corpus, manual exhaustive-scale flags, profiling builds, and benchmarks.
- Only weekly/manual extended and release workflows may run exhaustive parser scale, the complete performance corpus, profiling builds, or benchmarks.
- Timing results are advisory. CI may fail because a benchmark did not complete or emitted invalid artifacts, but never solely because elapsed time regressed by a percentage on a shared runner.
- Documentation-only pull requests must not install GHC or rebuild the compiler.
- Cache dependency/build data only; never cache credentials, generated release signatures, or mutable deployment output.
- Pin every third-party GitHub Action to a full immutable commit SHA with a trailing version comment. Configure Dependabot for `github-actions` so those pins remain updateable without mutable tags.
- Do not publish a package, tag, GitHub release, or registry artifact from this workstream. Prepare and verify alpha artifacts; publication remains an explicit maintainer action.
- Commit CI policy/scripts, workflows, community files, package metadata, and release preparation at reviewable boundaries.

---

## Task 1: Make the CI tier policy executable

**Files:**

- Create: `scripts/ci/fast-compiler.sh`
- Create: `scripts/ci/main-functional.sh`
- Create: `scripts/ci/extended.sh`
- Create: `scripts/ci/release-candidate.sh`
- Create: `scripts/ci/determinism.sh`
- Create: `scripts/ci/verify-benchmark-artifacts.py`
- Create: `scripts/ci/test-verify-benchmark-artifacts.py`
- Create: `scripts/check-ci-policy.py`
- Create: `scripts/test-check-ci-policy.py`
- Modify: `jazz.cabal`

- [ ] Write standard-library Python fixture tests that enforce:

  - the fast script contains a warning-clean build, repository audit, focused compiler/runtime/module/stdlib/CLI/contract tests, and example smoke checks;
  - the fast script rejects `cabal bench`, `jazz-bench`, positive `full-parser-scale` forms such as `-ffull-parser-scale` or `--flags=+full-parser-scale`, `profile-hotspots`, `profile-stages`, and `program-corpus-spec`, while requiring the negative `-f-full-parser-scale` form;
  - the main script contains `cabal test all` with both manual full-workload flags explicitly disabled and rejects benchmark, profiling, positive full-scale forms, and direct `program-corpus-spec` tokens;
  - the extended script contains all four full parser-scale components, `program-corpus-spec`, both profiling projects, `jazz-bench`, determinism checks, and artifact directories;
  - the release script invokes ordinary, extended, package, Nix, docs, site, and artifact validation; and
  - PR workflows cannot directly bypass the scripts with an inlined long command.

- [ ] Run tests before implementing the policy checker:

  ```bash
  python3 -m unittest scripts/test-check-ci-policy.py
  ```

  Expected: missing-policy behavior fails.

- [ ] Implement `scripts/check-ci-policy.py` with deterministic violations and an optional fixture-root argument.

- [ ] Add a manual Cabal flag `full-program-corpus`, defaulting to `False`, and make `program-corpus-spec` buildable only when it is enabled. Implement `scripts/ci/fast-compiler.sh` with `set -euo pipefail` and these exact operations; pass `-fdevelopment -f-full-program-corpus -f-full-parser-scale` to both build and test so warning enforcement and both long-workload exclusions are explicit even outside the checked-in Cabal project:

  ```bash
  test_components=(
    cli-spec runtime-observation-spec warning-config-spec
    structured-error-diagnostics-spec diagnostic-catalog-spec
    signature-rendering-spec loader-spec module-resolution-spec
    module-exports-spec module-pipeline-contract-spec
    prelude-loading-spec stdlib-spec canonical-lexer-comparison-spec
    canonical-parser-comparison-spec canonical-core-comparison-spec
    jazz-lowered-ir-contract-spec jazz-typed-core-contract-spec
    jazz-typed-core-expression-direct-call-spec parser-core-spec
    jazz-parser-parity-spec jazz-parser-scale-spec
    jazz-lexer-parity-spec parser-foundation-spec
    binding-signature-coherence-spec purity-semantics-spec
    runtime-semantics-spec repository-audit-spec
  )
  cabal build all -fdevelopment -f-full-program-corpus -f-full-parser-scale
  cabal test "${test_components[@]}" \
    -fdevelopment -f-full-program-corpus -f-full-parser-scale \
    --test-show-details=direct
  cabal check
  bash scripts/check-examples.sh
  git diff --check
  ```

  Use a Bash array for test component names and `--test-show-details=direct`. Do not add the complete program corpus or any benchmark/profiling component when tuning this list.

- [ ] Implement `scripts/ci/main-functional.sh` to run warning-clean `cabal build all -fdevelopment -f-full-program-corpus -f-full-parser-scale`, `cabal test all -fdevelopment -f-full-program-corpus -f-full-parser-scale --test-show-details=direct`, `cabal check`, repository/docs/queue/example validators, `nix flake check`, and `git diff --check`. Keep both manual workloads explicitly disabled even when a caller has a permissive Cabal project or local configuration.

- [ ] Implement `scripts/ci/determinism.sh` to run `examples/functions/factorial.jz` twice with `--runtime-stats=json` and twice with separate Speedscope output paths, then compare stdout, stderr, and profile bytes with `cmp`. Write artifacts only below a caller-provided `JAZZ_ARTIFACT_ROOT`, defaulting to `artifacts/determinism`.

- [ ] Write fixtures for `scripts/ci/verify-benchmark-artifacts.py`, then implement it with standard-library Python. Given the generated benchmark root and expected environment label, require exactly one labeled run directory, a nonempty `environment.json` object with the expected schema, label, and matching run id, plus a `results.csv` with the exact required header and at least one data row. The verifier must read the just-generated files, not only exercise fixed metadata values in `benchmark-metadata-spec`.

- [ ] Implement `scripts/ci/extended.sh` to:

  1. create and validate `JAZZ_ARTIFACT_ROOT` before any workload, then install an `EXIT` finalizer that preserves the original exit status and always writes the manifest from all regular evidence files produced so far, excluding the manifest itself; a finalizer failure changes a successful run to failure but never hides an earlier workload failure;
  2. run all tests with `-ffull-parser-scale -ffull-program-corpus`, including the four explicitly named full parser-scale suites and the first complete corpus execution, with a temporary Cabal test log;
  3. run `program-corpus-spec -ffull-program-corpus` a second time with a separate test log, normalize only the stable per-program results and terminal success marker into `corpus/pass-one.txt` and `corpus/pass-two.txt`, require both files to be nonempty and byte-identical, and leave both below `JAZZ_ARTIFACT_ROOT`;
  4. run `scripts/ci/determinism.sh`;
  5. build `cabal.project.profile-stages` and `cabal.project.profile-hotspots`;
  6. run `cabal bench jazz-bench` with an environment label supplied by `JAZZ_BENCHMARK_LABEL` and result root below `JAZZ_ARTIFACT_ROOT`;
  7. validate the generated run directory with `scripts/ci/verify-benchmark-artifacts.py`, then run `benchmark-metadata-spec` for its unit-level contract coverage; and
  8. let the unconditional finalizer emit `JAZZ_ARTIFACT_ROOT/manifest.json` with schema `{ "schema_version": 1, "artifacts": [{ "path": <normalized POSIX-relative path>, "sha256": <lowercase hex> }] }`. Hash every available regular evidence file below the artifact root, exclude the manifest itself, sort entries by normalized path, reject paths outside the root, and require release validation to enforce the same contract plus the complete required evidence set on successful runs. Release-level `SHA256SUMS` separately hashes the four final archives, excludes itself, and is the only release-directory checksum authority.

  Do not compare timing values to a threshold.

- [ ] Implement `scripts/ci/release-candidate.sh` to invoke the main and extended scripts, install and build the website, run `cabal sdist all`, run `nix build .#jazz` with a caller-owned `JAZZ_NIX_RESULT` out-link, and validate that all prerequisite outputs exist. Accept `JAZZ_RELEASE_VERSION` and fail if it is absent or not shaped like `0.<minor>.<patch>-alpha.<n>`. Accept one caller-owned `JAZZ_ARTIFACT_ROOT` (or establish a version-scoped default), export that exact path unchanged to `extended.sh`, and leave it intact for `build-alpha.sh`; the candidate must not delete or replace it before the alpha builder archives it. Final archive naming, cleanup, and checksums remain owned by `scripts/release/build-alpha.sh`, so the two scripts cannot recurse into one another.

- [ ] Run the policy tests and static checker:

  ```bash
  python3 -m unittest scripts/test-check-ci-policy.py
  python3 scripts/check-ci-policy.py
  ```

  Expected: all tier rules pass.

- [ ] Commit tier scripts and policy tests:

  ```bash
  git add -A
  git commit -m "ci: codify Jazz verification tiers"
  ```

## Task 2: Add the fast pull-request workflow

**Files:**

- Create: `.github/workflows/ci-pr.yml`
- Modify: `.github/workflows/docs-pages.yml`
- Modify: `scripts/check-ci-policy.py`
- Modify: `scripts/test-check-ci-policy.py`

- [ ] Configure `ci-pr.yml` for `pull_request` with least-privilege `contents: read` and `pull-requests: read` (required by `dorny/paths-filter` when it lists changed pull-request files), concurrency keyed by workflow and pull-request number, and `cancel-in-progress: true`.

- [ ] Add a `changes` job using `dorny/paths-filter` pinned to a full commit SHA to emit `compiler` when any compiler, standard-library, program, example, Cabal, Nix, editor, or CI-script path changes. Treat unclassified repository infrastructure changes as compiler-relevant. Documentation-only paths are:

  ```text
  README.md
  docs/**
  rfcs/**
  .codex/**
  website/**
  CONTRIBUTING.md
  SECURITY.md
  CHANGELOG.md
  RELEASING.md
  .github/ISSUE_TEMPLATE/**
  .github/PULL_REQUEST_TEMPLATE.md
  ```

- [ ] Add a `docs-and-site` job that always runs, uses Node 22 with npm cache, installs only `website/` dependencies with `npm ci`, and runs public-doc, docs, RFC, website, and CI-policy checks. It must not install Nix/GHC or run any Cabal command.

- [ ] Add `compiler-fast` only when `changes.compiler == 'true'`. Install Nix with SHA-pinned `cachix/install-nix-action`, cache the Cabal store and `dist-newstyle/` with SHA-pinned `actions/cache` keyed by OS, `flake.lock`, `jazz.cabal`, and `cabal.project`, then run:

  ```bash
  nix develop --command bash scripts/ci/fast-compiler.sh
  ```

  Set `timeout-minutes: 12`; the documented performance target remains under ten minutes, leaving two minutes only for runner variance and artifact cleanup.

- [ ] Add an always-running `pr-gate` aggregate job with a stable required-check name. It succeeds only when `docs-and-site` succeeds and `compiler-fast` either succeeds or was legitimately skipped because the change is documentation-only.

- [ ] Replace every third-party `uses:` reference already present in `docs-pages.yml` and introduced by this task with a full commit SHA plus a trailing version comment. Extend CI-policy tests to reject mutable action references in every workflow, in addition to rejecting Cabal commands in `docs-and-site`, performance tokens anywhere in the PR workflow, missing cancellation, an absent timeout, or a required gate that ignores a failed dependency.

- [ ] Validate locally:

  ```bash
  python3 -m unittest scripts/test-check-ci-policy.py
  python3 scripts/check-ci-policy.py
  if rg -n -e 'cabal bench|jazz-bench|-ffull-parser-scale|--flags=\+full-parser-scale|-ffull-program-corpus|--flags=\+full-program-corpus|profile-hotspots|profile-stages|program-corpus-spec' .github/workflows/ci-pr.yml scripts/ci/fast-compiler.sh; then
    exit 1
  else
    test "$?" -eq 1
  fi
  ```

  Expected: tests pass and the final search prints nothing.

- [ ] Commit the PR workflow:

  ```bash
  git add -A
  git commit -m "ci: add fast pull request checks"
  ```

## Task 3: Add the complete ordinary `main` workflow

**Files:**

- Create: `.github/workflows/ci-main.yml`
- Modify: `scripts/check-ci-policy.py`
- Modify: `scripts/test-check-ci-policy.py`

- [ ] Trigger `ci-main.yml` on pushes to `main` and `workflow_dispatch`. Use read-only contents permission, branch-scoped concurrency, and cancellation for superseded `main` runs.

- [ ] Install Nix and the same safe Cabal caches as PR CI, then execute:

  ```bash
  nix develop --command bash scripts/ci/main-functional.sh
  ```

  Set `timeout-minutes: 60` so a stuck suite terminates without redefining slow performance work as ordinary validation.

- [ ] Upload ordinary test logs only on failure, with a seven-day retention. Do not upload `dist-newstyle/` or the Cabal store as artifacts.

- [ ] Extend policy tests to require `main` and manual triggers, the complete ordinary script, and absence of exhaustive/performance tokens.

- [ ] Validate and commit:

  ```bash
  python3 -m unittest scripts/test-check-ci-policy.py
  python3 scripts/check-ci-policy.py
  git diff --check
  git add -A
  git commit -m "ci: validate the complete ordinary matrix on main"
  ```

## Task 4: Add weekly and manual extended verification

**Files:**

- Create: `.github/workflows/ci-extended.yml`
- Modify: `scripts/check-ci-policy.py`
- Modify: `scripts/test-check-ci-policy.py`

- [ ] Trigger `ci-extended.yml` at `17 7 * * 0` and with `workflow_dispatch`. Do not add `pull_request` or ordinary `push` triggers.

- [ ] Set read-only contents permission, a non-cancelling `extended` concurrency group, and `timeout-minutes: 360`.

- [ ] Install Nix and safe caches, set:

  ```text
  JAZZ_ARTIFACT_ROOT=artifacts/extended
  JAZZ_BENCHMARK_LABEL=github-actions-extended
  ```

  Then run `nix develop --command bash scripts/ci/extended.sh`.

- [ ] Upload `artifacts/extended/` with SHA-pinned `actions/upload-artifact`, `if: always()`, a 30-day retention, and `if-no-files-found: error`. The artifact must include every available failure diagnostic plus the unconditional SHA-256 manifest; successful runs must additionally include benchmark CSV/environment JSON, deterministic profiles, and normalized corpus outputs. Add a policy fixture that fails before the first workload and still proves the finalizer creates an uploadable manifest without masking the original failure.

- [ ] Add a step summary that reports completion state and artifact paths. It may list benchmark observations but must not classify percentage timing changes as pass/fail.

- [ ] Extend policy tests to require schedule/manual-only triggers, all extended categories, artifact upload on success or failure, and no timing-threshold expression.

- [ ] Validate and commit:

  ```bash
  python3 -m unittest scripts/test-check-ci-policy.py
  python3 scripts/check-ci-policy.py
  git diff --check
  git add -A
  git commit -m "ci: schedule extended Jazz verification"
  ```

## Task 5: Add contributor and repository hygiene

**Files:**

- Create: `CONTRIBUTING.md`
- Create: `SECURITY.md`
- Create: `CHANGELOG.md`
- Create: `RELEASING.md`
- Create: `.editorconfig`
- Create: `.gitattributes`
- Create: `.github/PULL_REQUEST_TEMPLATE.md`
- Create: `.github/ISSUE_TEMPLATE/bug-report.yml`
- Create: `.github/ISSUE_TEMPLATE/language-proposal.yml`
- Create: `.github/ISSUE_TEMPLATE/documentation.yml`
- Create: `.github/ISSUE_TEMPLATE/config.yml`
- Create: `.github/dependabot.yml`
- Modify: `docs/project/contributing.md`
- Modify: `docs/project/governance.md`
- Modify: `README.md`

- [ ] Write `CONTRIBUTING.md` as the canonical contributor guide with:

  - experimental/pre-1.0 expectations;
  - Nix-based setup and root build/test commands;
  - the distinction between semantic RFC-first changes and implementation-first maintenance;
  - source, test, Jazz-authored code, public docs, RFC, and `.codex` ownership;
  - formatting, warning, focused-test, ordinary-test, and docs/site commands;
  - the four CI tiers and why performance work is not on every PR;
  - small-commit and pull-request expectations; and
  - instructions for reporting bugs, proposing language changes, and improving docs.

- [ ] Keep `docs/project/contributing.md` as the shorter website orientation page and link to the root guide on GitHub. Do not duplicate the full command reference.

- [ ] Write `SECURITY.md` with supported-version policy (`main` and the latest alpha only), private reporting through GitHub Security Advisories, requested reproduction/impact details, a best-effort initial response target of seven days, coordinated-disclosure expectations, and a clear statement that ordinary bugs belong in issues. Do not claim a guaranteed response SLA.

- [ ] Write `CHANGELOG.md` in Keep a Changelog structure with `Unreleased` sections and an initial `0.1.0-alpha.1` preparation section. Link compare URLs to `https://github.com/un3qual/jazz` but do not claim the alpha has shipped.

- [ ] Write `RELEASING.md` with the exact local and CI artifact process, version update points, changelog promotion, tag convention `v0.<minor>.<patch>-alpha.<n>`, release verification, archive checksums, GitHub Pages verification, and rollback/non-publication steps.

- [ ] Add `.editorconfig` rules for UTF-8, LF, final newline, trimmed trailing whitespace, two-space Haskell/Jazz/YAML/JSON indentation, four-space Python indentation, tabs for Makefiles, and preserved Markdown trailing spaces.

- [ ] Add `.gitattributes` for LF text normalization and binary treatment of PNG, WOFF/WOFF2, and other generated brand assets.

- [ ] Add a concise PR template with summary, semantic-change/RFC declaration, verification evidence, documentation impact, performance-tier impact, and checklist. Do not require irrelevant sections to be filled with ceremony.

- [ ] Add three focused issue forms:

  - bug report: version/revision, platform, reproduction, expected/actual behavior, diagnostics;
  - language proposal: problem, current behavior, proposed semantics, alternatives, compatibility, implementation impact;
  - docs issue: affected URL/page, problem, suggested correction.

  Disable blank issues in `config.yml` and link security reports to GitHub Security Advisories and support questions to repository discussions only if Discussions is confirmed enabled; otherwise link questions to the documentation and issue search.

- [ ] Add Dependabot weekly grouped updates for `npm` in `/website` and `github-actions` in `/`. Limit open pull requests to five per ecosystem.

- [ ] Update root README links to the new contributor, security, changelog, and release files without exceeding the README line budget.

- [ ] Validate Markdown, forms, and links:

  ```bash
  nix develop --command prettier --check README.md CONTRIBUTING.md SECURITY.md CHANGELOG.md RELEASING.md docs .github
  bash scripts/check-docs.sh
  bash scripts/check-website.sh
  git diff --check
  ```

  Expected: all checks pass.

- [ ] Commit repository hygiene:

  ```bash
  git add -A
  git commit -m "meta: add Jazz contributor and project policies"
  ```

## Task 6: Complete package and editor metadata

**Files:**

- Modify: `jazz.cabal`
- Modify: `editors/vscode-jazz/package.json`
- Modify: `editors/vscode-jazz/README.md`
- Modify: `test/Jazz/Repository/PackagePolicy.hs`
- Modify: `test/Jazz/Repository/AuditSpec.hs`

- [ ] Add these Cabal fields and validate their exact values:

  ```text
  synopsis: A statically typed functional language with practical syntax
  homepage: https://un3qual.github.io/jazz/
  bug-reports: https://github.com/un3qual/jazz/issues
  author: un3qual
  maintainer: un3qual
  category: Language
  stability: Experimental
  tested-with: GHC == 9.14.1
  license: GPL-3.0-only
  license-file: LICENSE
  ```

  Add a `source-repository head` stanza with `type: git` and `location: https://github.com/un3qual/jazz.git`.

- [ ] Refresh `extra-doc-files` and `extra-source-files` so the source distribution includes README, changelog, contribution/security/release guides, performance guide, public docs, RFCs, examples, Jazz standard library/compiler sources, program corpus, editor metadata/fixtures, and runtime fixtures. Do not include `.codex/`, website build output, benchmark results, or profile results.

- [ ] Extend package-policy tests to require the metadata above, reject example-domain or empty URLs and legacy product names, confirm `jazz-internal` remains private, and inspect the `cabal sdist` file list for required and forbidden paths.

- [ ] Add `repository`, `homepage`, `bugs`, `icon`, and `keywords` metadata to the VS Code extension. Keep its scope honest: syntax highlighting and editor configuration only, with no language-server/formatter claims.

- [ ] Update the editor README to use canonical root paths and give manual install plus VSIX packaging commands without claiming marketplace publication.

- [ ] Validate metadata:

  ```bash
  nix develop --command cabal check
  nix develop --command cabal sdist all
  nix develop --command cabal test repository-audit-spec --test-show-details=direct
  npm --prefix website run build
  git diff --check
  ```

  Expected: Cabal reports no package warnings, the source archive contains required public sources and excludes private execution/output paths, and editor metadata audits pass.

- [ ] Commit metadata separately:

  ```bash
  git add -A
  git commit -m "meta: complete Jazz package metadata"
  ```

## Task 7: Prepare repeatable alpha artifacts and the release workflow

**Files:**

- Create: `scripts/release/build-alpha.sh`
- Create: `scripts/release/verify-artifacts.py`
- Create: `scripts/release/test-verify-artifacts.py`
- Create: `release-notes/0.1.0-alpha.1.md`
- Create: `.github/workflows/release.yml`
- Modify: `flake.nix`
- Modify: `scripts/ci/release-candidate.sh`
- Modify: `RELEASING.md`

- [ ] Add Nix `packages.jazz`, `packages.default`, and `apps.default` for the canonical `jazz` derivation/executable. Keep `checks.jazz-test-suite` warning-clean and do not add hidden network dependencies.

- [ ] Verify the app interface before packaging without leaving a root `result` symlink:

  ```bash
  nix_result_root="$(mktemp -d)"
  trap 'rm -r -- "$nix_result_root"' EXIT
  nix build .#jazz --out-link "$nix_result_root/jazz-result"
  nix run . -- --help
  ```

  Expected: the build succeeds, the canonical Jazz CLI help is printed, and the trap removes the temporary result root. Release scripts must use the same trapped-temporary out-link lifecycle.

- [ ] Write fixture tests for artifact verification covering missing files, duplicate manifest entries, incorrect SHA-256 hashes, unexpected filenames, unsafe or duplicate archive members, symlink members, a Nix archive without its executable/runtime closure, and a valid alpha artifact set.

- [ ] Implement `scripts/release/verify-artifacts.py` to require exactly:

  ```text
  jazz-<version>-source.tar.gz
  jazz-<version>-nix-<system>.tar.gz
  jazz-<version>-docs.tar.gz
  jazz-<version>-benchmark-evidence.tar.gz
  SHA256SUMS
  ```

  It must validate names and hashes, safely enumerate every gzip-tar member, reject absolute/traversal/duplicate/symlink/special members, and inspect the expected contents rather than treating a nonempty archive as valid. Require the source package root and `jazz.cabal`, the static docs `index.html`, the complete benchmark evidence set and manifest, and a self-contained Nix runtime closure whose recorded root can be imported and queried from a temporary local store.

- [ ] Implement `scripts/release/build-alpha.sh` to require `git status --porcelain=v1 --untracked-files=all` to be empty and require `JAZZ_RELEASE_VERSION`. Create a trapped temporary work root and define one evidence root within it; pass that exact `JAZZ_ARTIFACT_ROOT` and an explicit temporary Nix `--out-link` path through `scripts/ci/release-candidate.sh`. Do not put the out-link symlink into a tarball. Enumerate the Nix result's runtime requisites, export a self-contained closure plus root-store-path/system metadata into a regular-file staging tree, and archive that materialized tree. Archive the Cabal source, `website/build`, and the still-live shared evidence root, write sorted `SHA256SUMS`, and run the member-aware verifier. Atomically move the verified set under `artifacts/release/<version>/`; cleanup owns the temporary Nix result and evidence root on success or failure and runs only after the evidence archive is complete.

- [ ] Write honest release notes for `0.1.0-alpha.1` covering implemented language/compiler/library/CLI behavior, known limitations, experimental compatibility, installation via Nix/source, website/docs, and checksum verification. Do not claim a native backend, package manager, LSP, production stability, or completed self-hosting.

- [ ] Add `release.yml` for `workflow_dispatch` with required `version` input and pushes of tags matching `v*`. Give it read-only contents permission, set `timeout-minutes: 480`, install Nix/Node, and run the release script with `JAZZ_RELEASE_VERSION` derived from the input or tag.

- [ ] Upload verified release artifacts with SHA-pinned `actions/upload-artifact`, 30-day retention, and `if-no-files-found: error`. Do not grant `contents: write` and do not create a GitHub release automatically.

- [ ] Extend CI-policy checks so release workflow invokes the release script, contains all release gates, has no publication permission, and cannot skip benchmark completion.

- [ ] Run focused release tooling tests and static policy validation before the clean-tree gate:

  ```bash
  python3 -m unittest scripts/release/test-verify-artifacts.py
  python3 -m unittest scripts/ci/test-verify-benchmark-artifacts.py
  python3 scripts/check-ci-policy.py
  git diff --check
  ```

  Expected: focused verifier and policy fixtures pass while the implementation is still uncommitted.

- [ ] Commit only the release scripts, tests, notes, workflow, and Nix changes before invoking the clean-tree release builder:

  ```bash
  git add -A
  git commit -m "release: prepare Jazz alpha artifacts"
  ```

- [ ] Run one local candidate from that clean commit and verify the exact final set:

  ```bash
  test -z "$(git status --porcelain=v1 --untracked-files=all)"
  JAZZ_RELEASE_VERSION=0.1.0-alpha.1 bash scripts/release/build-alpha.sh
  python3 scripts/release/verify-artifacts.py artifacts/release/0.1.0-alpha.1
  test -z "$(git status --porcelain=v1 --untracked-files=all)"
  ```

  Expected: the complete release candidate passes, all exact artifacts and archive members verify, generated release files remain ignored, and the builder leaves the worktree clean. If the audit requires a tooling correction, commit that correction separately and rerun the candidate from the new clean commit.

## Task 8: Measure CI scope and finish the operations workstream

**Files:**

- Modify only if measurement or review finds a policy defect: files already in scope

- [ ] Run the fast compiler script three times from a warm dependency cache and record elapsed wall-clock values in the pull-request description, not in a tracked benchmark baseline:

  ```bash
  /usr/bin/time -p nix develop --command bash scripts/ci/fast-compiler.sh
  /usr/bin/time -p nix develop --command bash scripts/ci/fast-compiler.sh
  /usr/bin/time -p nix develop --command bash scripts/ci/fast-compiler.sh
  ```

  Expected: each warm run is under ten minutes. If not, move the slowest non-core test group to `main-functional.sh`; never move repository audit, CLI, modules, standard library, core parser, runtime semantics, or example smoke checks.

- [ ] Prove PR CI has no long performance path and main CI has no extended path:

  ```bash
  python3 scripts/check-ci-policy.py
  if rg -n -e 'cabal bench|jazz-bench|-ffull-parser-scale|--flags=\+full-parser-scale|-ffull-program-corpus|--flags=\+full-program-corpus|profile-hotspots|profile-stages|program-corpus-spec' .github/workflows/ci-pr.yml scripts/ci/fast-compiler.sh; then
    exit 1
  else
    test "$?" -eq 1
  fi
  if rg -n -e 'cabal bench|jazz-bench|-ffull-parser-scale|--flags=\+full-parser-scale|-ffull-program-corpus|--flags=\+full-program-corpus|profile-hotspots|profile-stages|program-corpus-spec' .github/workflows/ci-main.yml scripts/ci/main-functional.sh; then
    exit 1
  else
    test "$?" -eq 1
  fi
  ```

  Expected: both searches print nothing.

- [ ] Run the complete ordinary local gate:

  ```bash
  nix develop --command bash scripts/ci/main-functional.sh
  npm --prefix website ci
  bash scripts/check-website.sh
  python3 -m unittest scripts/test-check-ci-policy.py scripts/ci/test-verify-benchmark-artifacts.py scripts/release/test-verify-artifacts.py
  git diff --check
  ```

  Expected: all ordinary, documentation, website, policy, and release-tool tests pass. Do not rerun the complete extended tier if Task 7's release-candidate build already produced passing evidence for the current commit.

- [ ] Review repository presentation and scope:

  ```bash
  git diff --stat origin/main...HEAD
  git log --oneline origin/main..HEAD
  test -z "$(git status --porcelain=v1 --untracked-files=all)"
  ```

  Expected: the worktree is clean and the branch contains operations, metadata, CI, and release preparation only.

- [ ] Push `codex/project-operations` and open a dedicated pull request. Include warm fast-tier timings, ordinary and extended/release evidence, artifact names, explicit confirmation that PR CI excludes long performance work, and the manual steps remaining to publish `v0.1.0-alpha.1`.
