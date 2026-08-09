# Jazz Publication Validation Simplification Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Preserve Jazz's publication guarantees while deleting custom parser surface, mirrored fixtures, and unreachable defensive branches from PR 127.

**Architecture:** Docusaurus, the Jazz compiler-backed example runner, Node asset tests, TypeScript, and actionlint own the behavior they already implement. Three narrow Python checks retain only Jazz-specific inventory, built-output, and Pages safety policy under a trusted-contributor accidental-regression model.

**Tech Stack:** Python 3 standard library, Node.js 22, pnpm 11.18.0, Docusaurus 3.10.2, actionlint 1.7.12, GitHub Actions

## Global Constraints

- Modify only PR 127; its base is PR 126 at `5643dcb7205b30c0b6cfb083eea98ce5cbd3cc86`.
- Keep all GitHub Actions on immutable commit SHAs and every checkout on `persist-credentials: false`.
- Preserve per-job least-privilege Pages permissions and deployment order.
- Keep compiler-backed executable examples and fragment receipts synchronized with public docs.
- Do not add YAML, Markdown, HTML, CSS, JSX, or browser parsers.
- Delete deliberately evasive fixture cases and private source-shape checks.

---

### Task 1: Collapse the Pages workflow checker

**Files:**

- Modify: `.github/workflows/docs-pages.yml`
- Modify: `scripts/check-docs-pages-workflow.py`
- Modify: `scripts/test-docs-pages-workflow.py`
- Modify: `scripts/ci/fast-compiler.sh`
- Modify: `scripts/ci/main-functional.sh`

**Interfaces:**

- Consumes: actionlint-valid `.github/workflows/docs-pages.yml`.
- Produces: `validate(root: Path) -> list[str]` for Jazz-specific deployment safety.

- [x] **Step 1: Replace the mirrored workflow fixture with tests that copy the checked-in workflow and mutate unpinned action, unsafe checkout or ref overrides, broad permissions, disabled critical validation, missing build/boundary, and bad deployment order.**
- [x] **Step 2: Run `python3 scripts/test-docs-pages-workflow.py` and confirm the new tests fail against the parser-heavy checker or unpinned pnpm action for the intended reasons.**
- [x] **Step 3: Pin the pnpm action and reduce the checker to immutable-action, permission, checkout, required-command, and order checks without YAML parsing.**
- [x] **Step 4: Add `actionlint` to the Nix-backed fast and ordinary CI scripts.**
- [x] **Step 5: Run the focused test, live checker, and `nix develop --command actionlint` until green.**
- [x] **Step 6: Commit the Pages validation simplification.**

### Task 2: Collapse the website publication boundary

**Files:**

- Modify: `scripts/check-website-boundary.py`
- Modify: `scripts/test-check-website-boundary.py`
- Modify: `scripts/check-website.sh`

**Interfaces:**

- Consumes: trusted website source plus optional `website/build` output.
- Produces: config/inventory failures before build and forbidden-term/remote-URL failures after build.

- [x] **Step 1: Replace HTML/CSS/browser-evasion fixtures with focused mutations for broken-link policy, wrong docs root, an internal reference, and an ordinary remote font/image URL in built output.**
- [x] **Step 2: Run `python3 scripts/test-check-website-boundary.py` and confirm the desired narrow contract is red where the existing checker reports obsolete source-shape behavior.**
- [x] **Step 3: Delete authored-source scanning, `HTMLParser`, CSS escape/function parsing, `srcset`, duplicate-attribute, `srcdoc`, and malformed-authority branches.**
- [x] **Step 4: Implement exact Docusaurus policy markers plus a simple emitted-resource URL/internal-term scan that leaves ordinary navigation links to Docusaurus.**
- [x] **Step 5: Run the focused suite and a real production build followed by the boundary scan.**
- [x] **Step 6: Commit the website boundary simplification.**

### Task 3: Collapse public documentation validation

**Files:**

- Modify: `scripts/check-public-docs.py`
- Modify: `scripts/test-check-public-docs.py`
- Modify: `scripts/check-docs.sh`
- Modify: `.github/workflows/docs-pages.yml`

**Interfaces:**

- Consumes: `docs/`, `README.md`, `scripts/example-cases.tsv`, explicit example markers, optional `--jazz-bin`.
- Produces: required-page, internal-publication, and canonical-example synchronization failures.

- [x] **Step 1: Replace parser-bypass fixtures with repository-copy mutations for a missing required page, `draft`, non-Markdown file, escaping symlink, internal term, executable source drift, expected-output drift, and fragment receipt drift.**
- [x] **Step 2: Run the focused suite and verify the retained behaviors fail for the named mutation.**
- [x] **Step 3: Reduce the checker to inventory/frontmatter, lexical internal-term, explicit marker/fence, manifest, output, and optional compiler fragment checks. Let Docusaurus own links, fragments, navigation, and Markdown visibility.**
- [x] **Step 4: Remove workflow path entries and aggregate-test calls that referred only to deleted parser helpers or theoretical fixture coverage.**
- [x] **Step 5: Run focused tests, `bash scripts/check-docs.sh --jazz-bin <path>`, and the real Docusaurus build.**
- [x] **Step 6: Commit the public-doc validation simplification.**

### Task 4: Pin all PR 127 workflows and consolidate safety policy

**Files:**

- Modify: `.github/workflows/ci-pr.yml`
- Modify: `.github/workflows/ci-main.yml`
- Modify: `.github/workflows/ci-extended.yml`
- Modify: `.github/workflows/release.yml`
- Modify: `scripts/check-ci-policy.py`
- Modify: `scripts/test-check-ci-policy.py`

**Interfaces:**

- Consumes: every checked-in GitHub Actions workflow.
- Produces: one generic immutable-action and safe-checkout policy in addition to existing workload-specific checks.

- [x] **Step 1: Add focused policy tests that reject any non-SHA `uses:` value and any checkout that persists credentials or overrides the triggering repository/revision.**
- [x] **Step 2: Run `python3 scripts/test-check-ci-policy.py` and confirm red against the current PR 127 workflows.**
- [x] **Step 3: Resolve each upstream action tag to its immutable commit, update workflows and existing exact references, and add the minimal generic policy.**
- [x] **Step 4: Run CI policy tests, live policy, and actionlint.**
- [x] **Step 5: Commit workflow pinning and safety consolidation.**

### Task 5: Full verification and anti-slop review

**Files:**

- Review: all changes relative to `origin/codex/docusaurus-website`

**Interfaces:**

- Consumes: completed Tasks 1-4.
- Produces: pushed PR 127 head with evidence.

- [x] **Step 1: Run Python compilation and formatting checks, focused validator suites, compiler-backed docs/examples, website tests/typecheck/build, CI policy, actionlint, the full Nix flake check, and `git diff --check`.**
- [x] **Step 2: Compare line counts and `git diff --stat`; require material net deletion in the target checker-test pairs.**
- [x] **Step 3: Review every remaining parser/helper/branch/fallback for a concrete reachable input; delete anything that only answers a deliberate-evasion fixture.**
- [x] **Step 4: Commit any verification cleanup, force-push the rebased HEAD with lease to `codex/project-operations`, and confirm PR 127 targets `codex/docusaurus-website`.**
