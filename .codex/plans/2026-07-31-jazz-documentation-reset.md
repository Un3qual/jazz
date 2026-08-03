# Jazz Documentation Reset Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Replace the mixed public/internal documentation tree with a concise professional README, curated public language documentation, durable RFCs, and private project execution records.

**Architecture:** `docs/` becomes the only public documentation source, `rfcs/` stores durable decisions, and `.codex/` owns active execution state and plans. Existing specifications are rewritten by topic rather than bulk-moved, accepted architectural decisions are consolidated into numbered RFCs, and completed or superseded implementation history is deleted after its durable information is captured. Repository checks enforce the boundary and execute the README example.

**Tech Stack:** Markdown, YAML front matter, Bash, Python 3, Jazz CLI, Prettier, ripgrep

## Global Constraints

- Execute this workstream on `codex/documentation-reset`, based on the merged repository-canonicalization workstream. All commands and links use root-level `jazz`, `src/`, `test/`, and `jazz.cabal` paths.
- Public documentation must describe implemented behavior only. Partial behavior must be labeled `Partial`; future work belongs on the roadmap and must not be presented as executable syntax.
- `docs/` must contain no Superpowers artifacts, agent instructions, execution queues, implementation checklists, verification logs, or historical compiler comparisons.
- Do not publish `.codex/` or `rfcs/` through the eventual Docusaurus configuration. Public docs may explain the RFC process without copying internal task history.
- Do not preserve a document solely because it already exists. Rewrite durable content into its final owner, then delete the source.
- Keep the root README between 100 and 150 lines and optimize it for first-time language users.
- Every complete Jazz program shown as runnable must exist under `examples/` and be executed by a repository check. Inline fragments must be labeled as fragments.
- Do not change compiler semantics to make documentation claims pass. Correct the claim or file the gap in `docs/project/roadmap.md`.
- Commit after the boundary migration, RFC curation, public docs, and README/checks are each independently reviewable.

---

## Task 1: Create a failing public-documentation boundary check

**Files:**

- Create: `scripts/check-public-docs.py`
- Create: `scripts/check-public-docs.sh`
- Create: `scripts/test-check-public-docs.py`
- Modify: `scripts/check-docs.sh`
- Test: `scripts/test-check-public-docs.py`

- [ ] Write unit tests around a temporary fixture tree for these rules:

  - every Markdown file below `docs/` has nonempty `title`, `description`, and `sidebar_position` front matter;
  - only `getting-started`, `language`, `standard-library`, `reference`, `compiler`, `project`, and `index.md` may exist directly under `docs/`;
  - public Markdown rejects `docs/superpowers`, `docs/execution`, `.codex/`, `jazz-next`, `JazzNext`, `jazz-hs`, and `jazz2` path references;
  - Markdown links cannot escape from `docs/` into `.codex/` or `rfcs/`;
  - each marker `<!-- jazz-example: executable path=examples/... -->` names a tracked `.jz` file below root `examples/`, immediately classifies the adjacent Jazz fence, and that fence matches the tracked source byte-for-byte;
  - every other Jazz fence is explicitly classified with `<!-- jazz-example: fragment -->`, so runnable-looking blocks cannot bypass source validation;
  - every tracked `examples/**/*.jz` is referenced by at least one public page or the root README; and
  - required public pages listed in Task 5 exist.

- [ ] Run the new tests before implementing the checker:

  ```bash
  python3 scripts/test-check-public-docs.py
  ```

  Expected: failures report the missing checker behavior.

- [ ] Implement `scripts/check-public-docs.py` with standard-library Python only. It must accept an optional repository-root argument for fixtures, emit one actionable line per violation, sort output deterministically, and exit nonzero on any violation.

- [ ] Make `scripts/check-public-docs.sh` a strict Bash wrapper that resolves the Git root and invokes the Python checker.

- [ ] Re-run the checker tests:

  ```bash
  python3 scripts/test-check-public-docs.py
  ```

  Expected: all fixture cases pass.

- [ ] Invoke `scripts/check-public-docs.sh` from `scripts/check-docs.sh`, then run it against the current tree:

  ```bash
  bash scripts/check-public-docs.sh
  ```

  Expected: it fails because the old mixed documentation layout has not yet been curated.

## Task 2: Move active execution state out of public docs

**Files:**

- Move: `docs/execution/README.md` -> `.codex/execution/README.md`
- Move: `docs/execution/blocker-contracts.md` -> `.codex/execution/blocker-contracts.md`
- Move: `docs/execution/queue.md` -> `.codex/execution/queue.md`
- Move: `docs/execution/prompts/autonomous-next-batch.md` -> `.codex/execution/prompts/autonomous-next-batch.md`
- Move: `docs/execution/prompts/curated-next-batch.md` -> `.codex/execution/prompts/curated-next-batch.md`
- Delete: `docs/execution/done-archive.md`
- Modify: `scripts/check-execution-queue.py`
- Modify: `scripts/check-execution-queue.sh`
- Modify: `scripts/test-check-execution-queue.sh`
- Modify: `scripts/check-docs.sh`
- Modify: `AGENTS.md`

- [ ] Before moving files, extract any still-open work from `docs/execution/done-archive.md` into the live queue. Do not retain completed-row verification narratives.

- [ ] Move the five active execution files with `git mv`, delete `done-archive.md`, and remove the empty `docs/execution/` tree.

- [ ] Change queue validators and their fixtures to treat `.codex/execution/queue.md` as the dispatcher and `.codex/execution/blocker-contracts.md` as its contract source. Error messages must print the new paths.

- [ ] Update `AGENTS.md` to direct task dispatch to `.codex/execution/queue.md` and planning to `.codex/plans/` while making clear that neither location defines public language behavior.

- [ ] Run the queue validation suite:

  ```bash
  bash scripts/check-execution-queue.sh
  bash scripts/test-check-execution-queue.sh
  ```

  Expected: both pass against `.codex/execution/`.

- [ ] Commit the internal-state move:

  ```bash
  git add -A
  git commit -m "docs: separate execution state from public docs"
  ```

## Task 3: Curate durable decisions into RFCs

**Files:**

- Create: `rfcs/README.md`
- Create: `rfcs/accepted/0001-language-authority-and-change-control.md`
- Create: `rfcs/accepted/0002-repository-productization.md`
- Create: `rfcs/accepted/0003-bootstrap-interpreter-profile.md`
- Create: `rfcs/accepted/0004-hosted-canonical-compiler.md`
- Create: `rfcs/accepted/0005-typed-core-elaboration.md`
- Create: `rfcs/accepted/0006-lowered-ir-contract.md`
- Create: `rfcs/accepted/0007-runtime-host-boundary.md`
- Create: `rfcs/accepted/0008-parser-scale-and-performance-tiers.md`
- Create: `rfcs/proposed/README.md`
- Modify: `scripts/check-docs.sh`
- Delete after consolidation: `docs/superpowers/specs/**`
- Delete after consolidation: `docs/superpowers/plans/**`
- Delete after consolidation: `docs/plans/**`

- [ ] Add an RFC template to `rfcs/README.md` with scalar header fields `Status:`, `Date:`, and `Supersedes:`, followed by `## Decision`, `## Context`, and `## Consequences` sections. Define `accepted/` as durable decisions and `proposed/` as reviewable proposals; task plans remain under `.codex/plans/`. Reuse these exact forms in `scripts/check-docs.sh` and its fixtures.

- [ ] Write RFC 0001 from the durable authority and governance rules. It must establish public docs, implementation/tests, accepted RFCs, and roadmap as the descending authority order and require an RFC before semantic language changes.

- [ ] Rewrite the approved repository-productization design as RFC 0002. Preserve the final directory boundary, canonical `jazz` identity, archive-tag decision, website choice, and tiered CI decision. Remove task-specific approval history and tool instructions.

- [ ] Consolidate accepted compiler architecture decisions into RFCs 0003–0008:

  - 0003: the interpreter-based bootstrap profile and what self-hosting does and does not mean;
  - 0004: Jazz-authored lexer/parser/core parity and hosted canonical compiler boundaries;
  - 0005: typed-core ownership, validation, elaboration, and direct-call scope;
  - 0006: backend-neutral lowered IR, validation, and native-backend boundary;
  - 0007: typed `RuntimeHost`, deterministic test hosts, and production CLI host I/O;
  - 0008: bounded default parser scale, explicit exhaustive scale, semantic budgets, profiling, and advisory wall-clock measurements.

- [ ] For each RFC, inspect all source designs on that topic and write one coherent current decision. Do not concatenate design files. Record superseded decision dates in the `Supersedes` field without retaining deleted file paths as live links.

- [ ] Add `rfcs/proposed/README.md` explaining that proposals are numbered only when opened for review and do not override accepted contracts.

- [ ] Extend `scripts/check-docs.sh` to require RFC 0001–0008 and the RFC index, require the standard RFC fields, and reject `Status: Accepted` files below `rfcs/proposed/`.

- [ ] Confirm all still-active execution plans already live in `.codex/plans/`. Delete every file under `docs/superpowers/` and `docs/plans/` only after the eight RFCs and the public mapping in Task 5 contain their durable decisions.

- [ ] Audit RFC completeness:

  ```bash
  require_all_rfc_files_match() {
    local missing status
    if missing="$(rg --files-without-match "$1" rfcs/accepted/*.md)"; then
      printf '%s\n' "$missing" >&2
      return 1
    else
      status=$?
      test "$status" -eq 1
    fi
  }
  require_all_rfc_files_match '^Status: Accepted$'
  require_all_rfc_files_match '^Date: [0-9]{4}-[0-9]{2}-[0-9]{2}$'
  require_all_rfc_files_match '^Supersedes: .+$'
  require_all_rfc_files_match '^## Decision$'
  require_all_rfc_files_match '^## Context$'
  require_all_rfc_files_match '^## Consequences$'
  test ! -e docs/superpowers
  test ! -e docs/plans
  ```

  Expected: all six `rg --files-without-match` commands print nothing and both deleted-tree assertions succeed.

- [ ] Commit RFC curation separately:

  ```bash
  git add -A
  git commit -m "docs: curate durable decisions as RFCs"
  ```

## Task 4: Add checked user examples

**Files:**

- Create: `examples/hello.jz`
- Create: `examples/functions/factorial.jz`
- Create: `examples/patterns/result.jz`
- Create: `examples/modules/src/Example/Main.jz`
- Create: `examples/modules/src/Example/Greeting.jz`
- Create: `examples/README.md`
- Create: `scripts/check-examples.sh`
- Modify: `jazz.cabal`
- Modify: `test/Jazz/Repository/AuditSpec.hs`

- [ ] Add `examples/hello.jz` as the smallest runnable expression:

  ```jazz
  "Hello, Jazz".
  ```

  Expected CLI output: `"Hello, Jazz"` followed by a newline.

- [ ] Add `examples/functions/factorial.jz` with the implemented pattern-lambda-clause syntax:

  ```jazz
  factorial :: Int -> Int.
  factorial =
    \|(0) -> 1
     |(n) -> n * factorial (n - 1).
  factorial 6.
  ```

  Expected output is `720` followed by a newline.

- [ ] Add `examples/patterns/result.jz` as a self-contained generic ADT and exhaustive case:

  ```jazz
  data Result e a = Err e | Ok a.
  unwrapOr :: Int -> Result(Int, Int) -> Int.
  unwrapOr = \(fallback, result) -> case result {
    | Err _ -> fallback
    | Ok item -> item
  }.
  unwrapOr 0 (Ok 41).
  ```

  Expected output is `41` followed by a newline.

- [ ] Add `examples/modules/src/Example/Greeting.jz`:

  ```jazz
  module Example::Greeting (value greeting) {
    greeting = "Hello from a Jazz module".
  }
  ```

  Add `examples/modules/src/Example/Main.jz`:

  ```jazz
  module Example::Main {
    import Example::Greeting.

    greeting.
  }
  ```

  Run it with `--run --entry-module Example::Main --module-root examples/modules/src`; expected output is `"Hello from a Jazz module"` followed by a newline.

- [ ] Document exact compile, run, and module-graph commands in `examples/README.md`. State that these are teaching examples; `programs/` remains the production-shaped correctness and benchmark corpus.

- [ ] Add all example sources to `extra-source-files` in `jazz.cabal` and to the authored-source inventory in `test/Jazz/Repository/AuditSpec.hs`.

- [ ] Implement `scripts/check-examples.sh` to run `cabal build jazz` once, resolve the executable with `cabal list-bin jazz`, execute the three standalone examples and the module example through that real CLI binary, compare exact stdout to expected literals embedded in the script, and fail on CLI stderr or a nonzero exit. Keep Cabal's own build output separate from captured program output.

- [ ] Run the examples twice to prove deterministic output:

  ```bash
  bash scripts/check-examples.sh
  bash scripts/check-examples.sh
  nix develop --command cabal test repository-audit-spec --test-show-details=direct
  ```

  Expected: both example runs and the repository audit pass.

## Task 5: Author the curated public documentation tree

**Files:**

- Create: `docs/index.md`
- Create: `docs/getting-started/overview.md`
- Create: `docs/getting-started/installation.md`
- Create: `docs/getting-started/first-program.md`
- Create: `docs/getting-started/cli.md`
- Create: `docs/language/overview.md`
- Create: `docs/language/source-and-blocks.md`
- Create: `docs/language/bindings-and-functions.md`
- Create: `docs/language/types-and-signatures.md`
- Create: `docs/language/algebraic-data-types-and-patterns.md`
- Create: `docs/language/control-flow.md`
- Create: `docs/language/modules.md`
- Create: `docs/language/operators.md`
- Create: `docs/language/capabilities.md`
- Create: `docs/language/purity.md`
- Create: `docs/standard-library/overview.md`
- Create: `docs/standard-library/prelude.md`
- Create: `docs/standard-library/list.md`
- Create: `docs/standard-library/maybe-result-nonempty.md`
- Create: `docs/standard-library/dictionary.md`
- Create: `docs/standard-library/queue.md`
- Create: `docs/standard-library/map-and-set.md`
- Create: `docs/standard-library/char-and-text.md`
- Create: `docs/standard-library/io.md`
- Create: `docs/reference/lexical-grammar.md`
- Create: `docs/reference/expression-grammar.md`
- Create: `docs/reference/module-resolution.md`
- Create: `docs/reference/cli.md`
- Create: `docs/reference/diagnostics.md`
- Create: `docs/reference/runtime-values.md`
- Create: `docs/compiler/architecture.md`
- Create: `docs/compiler/pipeline.md`
- Create: `docs/compiler/bootstrapping.md`
- Create: `docs/project/status.md`
- Create: `docs/project/roadmap.md`
- Create: `docs/project/governance.md`
- Create: `docs/project/contributing.md`
- Rewrite then delete: `docs/spec/**`
- Rewrite then delete: `docs/feature-status.md`
- Rewrite then delete: `docs/jazz-language-state.md`
- Rewrite then delete: `docs/jazz-improvement-backlog.md`
- Modify: `scripts/check-spec-authority.sh`

- [ ] Give every new page Docusaurus-compatible `title`, `description`, and `sidebar_position` front matter. Use sentence-case titles and relative links that remain inside `docs/`.

- [ ] Rewrite the old specifications by this exact ownership map:

  | Existing topic                                      | Final public owner                                                                                 |
  | --------------------------------------------------- | -------------------------------------------------------------------------------------------------- |
  | authoritative syntax and source forms               | `language/source-and-blocks.md`, `reference/lexical-grammar.md`, `reference/expression-grammar.md` |
  | bindings, signatures, generic named types           | `language/bindings-and-functions.md`, `language/types-and-signatures.md`                           |
  | ADTs, tuples, cases, guards, and patterns           | `language/algebraic-data-types-and-patterns.md`, `language/control-flow.md`                        |
  | module layout, imports, exports, resolution, cycles | `language/modules.md`, `reference/module-resolution.md`                                            |
  | operators and precedence                            | `language/operators.md`, `reference/expression-grammar.md`                                         |
  | capabilities and rejected trait vocabulary          | `language/capabilities.md`                                                                         |
  | purity bang contract                                | `language/purity.md`                                                                               |
  | primitive, text, and runtime values                 | `reference/runtime-values.md`, relevant standard-library pages                                     |
  | CLI input and warning flags                         | `getting-started/cli.md`, `reference/cli.md`, `reference/diagnostics.md`                           |
  | standard-library boundary and API details           | `standard-library/**`                                                                              |
  | current feature matrix                              | `project/status.md`                                                                                |
  | improvement backlog                                 | `project/roadmap.md`                                                                               |
  | compiler state and bootstrap notes                  | `compiler/**`                                                                                      |

- [ ] Derive grammar and diagnostics claims from current parser code, diagnostic catalog, tests, and CLI help—not from superseded prose. Include exact implemented forms and label every intentionally incomplete area.

- [ ] Split the existing standard-library README into the nine pages listed above. Preserve public names, edge cases, complexity qualifiers, representation privacy, and host-I/O error categories. Do not expose private `__kernel_*` names as user APIs.

- [ ] Rewrite `docs/compiler/architecture.md` from the active compiler README and accepted RFCs. Describe major stages and ownership without embedding implementation task history.

- [ ] Build `docs/project/status.md` from live tests and source. Give it an `Updated: 2026-07-31` field and an `Implementation snapshot:` field populated with the reviewed canonicalization baseline commit on which this documentation workstream is based. Do not attempt to embed the hash of the commit that contains the status page; that hash would be self-referential. Use only `Implemented`, `Partial`, and `Planned` labels with concise evidence links to public pages.

- [ ] Condense the backlog into `docs/project/roadmap.md` with four horizons: language completion, self-hosting, native backend, and ecosystem. Do not promise dates.

- [ ] Delete the old source files only after every mapping row has a reviewed final owner. Remove empty `docs/spec/` and obsolete top-level document paths. Rewrite `scripts/check-spec-authority.sh` in the same change to validate the new governance page and authority RFC, reject the removed paths, and stop requiring legacy authority vocabulary.

- [ ] Run the public-boundary checker:

  ```bash
  bash scripts/check-public-docs.sh
  ```

  Expected: all required pages, front matter, links, and example references pass; no internal path or legacy identity leaks into `docs/`.

- [ ] Commit the public documentation set:

  ```bash
  git add -A
  git commit -m "docs: publish the Jazz language guide"
  ```

## Task 6: Replace the root README

**Files:**

- Rewrite: `README.md`
- Modify: `scripts/check-docs.sh`
- Modify: `scripts/check-public-docs.py`
- Test: `scripts/test-check-public-docs.py`

- [ ] Write a 100–150 line README with this exact content order:

  1. local logo or wordmark and the line `A statically typed functional language with practical syntax`;
  2. honest `Experimental / pre-1.0` maturity notice;
  3. the checked `examples/functions/factorial.jz` program and expected `720` output;
  4. Nix-based root quick start with `nix develop`, `cabal build all`, and `cabal run jazz -- --run examples/functions/factorial.jz`;
  5. compact `Available today` and `In development` lists;
  6. links to getting started, language guide, standard library, reference, compiler, status, roadmap, contribution guide, issue tracker, and `https://un3qual.github.io/jazz/` (labeled as publishing with Workstream 3 until deployment is live);
  7. contribution and GPL-3.0-only license lines.

- [ ] Remove the personal project story, category-theory footnote, JavaScript comparisons, aspirational code, legacy repository explanation, full feature matrix, and internal authority details.

- [ ] Use only repository-local image paths. Do not depend on `?raw=true` GitHub image URLs.

- [ ] Extend the public-doc checker tests and implementation to enforce the README line budget, required maturity language, local image, executable-example marker, and absence of banned legacy/internal terms.

- [ ] Run the README example through the real CLI and verify the exact displayed output:

  ```bash
  nix develop --command cabal run jazz -- --run examples/functions/factorial.jz
  ```

  Expected: `720` followed by one newline.

- [ ] Run README and docs checks:

  ```bash
  bash scripts/check-examples.sh
  bash scripts/check-public-docs.sh
  bash scripts/check-docs.sh
  nix develop --command prettier --check README.md docs rfcs .codex/execution .codex/plans
  git diff --check
  ```

  Expected: all checks pass.

- [ ] Commit the new front door:

  ```bash
  git add -A
  git commit -m "docs: replace the Jazz project README"
  ```

## Task 7: Complete the documentation migration audit

**Files:**

- Modify only if audit findings require it: files already in scope

- [ ] Verify the final ownership roots:

  ```bash
  find docs -maxdepth 2 -type f -print | sort
  find rfcs -maxdepth 2 -type f -print | sort
  find .codex -maxdepth 3 -type f -print | sort
  ```

  Expected: `docs/` contains only curated public pages, `rfcs/` contains durable decisions, and `.codex/` contains execution state and plans.

- [ ] Prove the removed trees and identities are absent from public material:

  ```bash
  test ! -e docs/superpowers
  test ! -e docs/plans
  test ! -e docs/execution
  test ! -e docs/spec
  if rg -n "superpowers|\.codex/|docs/execution|jazz-next|JazzNext|jazz-hs|jazz2" README.md docs; then
    exit 1
  else
    test "$?" -eq 1
  fi
  ```

  Expected: all `test` commands succeed and `rg` prints nothing.

- [ ] Run the complete ordinary functional and repository validation matrix:

  ```bash
  nix develop --command cabal build all
  nix develop --command cabal test all --test-show-details=direct
  nix develop --command cabal check
  bash scripts/check-examples.sh
  bash scripts/check-public-docs.sh
  bash scripts/check-docs.sh
  bash scripts/check-spec-authority.sh
  bash scripts/check-execution-queue.sh
  bash scripts/test-check-execution-queue.sh
  git diff --check
  ```

  Expected: every command passes. Extended parser scale, profiling, and benchmarks are intentionally excluded because this workstream changes prose, examples, and validation—not compiler behavior.

- [ ] Review the final branch diff and commit any audit-only corrections:

  ```bash
  git diff --stat origin/main...HEAD
  git log --oneline origin/main..HEAD
  git status --short
  ```

  Expected: the branch contains only documentation ownership, public content, examples, and their checks.

- [ ] Push `codex/documentation-reset` and open a dedicated pull request. The PR description must include the deleted documentation categories, the final ownership model, executable-example evidence, and the ordinary verification results.
