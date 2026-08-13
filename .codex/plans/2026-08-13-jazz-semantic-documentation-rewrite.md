# Jazz Semantic Documentation Rewrite Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Rewrite every public Jazz documentation page so Learn teaches language behavior, Reference states exact contracts, and Standard Library documents only useful API semantics.

**Architecture:** Preserve the existing Docusaurus site, navigation, routes, signatures, type links, search, examples, and visual design. Perform the rewrite in independently reviewable content groups, using `docs/language/` and `docs/reference/` as the public contract and checking any new semantic claim against active compiler, standard-library, and test evidence.

**Tech Stack:** Docusaurus 3.10.2, Markdown/MDX, React 19, TypeScript 6, Node 22, pnpm 11.18.0, Pagefind 1.5.2, Python and shell documentation checks.

## Global Constraints

- A useful sentence explains meaning, observable behavior, composition, a consequential edge case, failure mode, cost, or a real task.
- Remove prose that only restates a heading, type signature, function name, command, or punctuation rule.
- Show syntax in the smallest useful code fragment instead of narrating it token by token.
- Learn pages explain mental models and consequences; exact accepted forms, precedence, lexical restrictions, and grammar belong in Reference.
- Standard-library prose is limited to non-obvious contracts such as empty-input behavior, ordering, stability, laziness, callback invocation, persistence, failure, normalization, and complexity.
- Standard-library examples appear only when the declaration does not make a consequential behavior clear; they remain short expressions rather than complete programs.
- Reference remains compact and exact. Grammar pages may describe punctuation when required to define valid Jazz, but do not add tutorial commentary for obvious spellings.
- Compiler pages describe stages and responsibility transitions, not implementation files.
- Contributor pages may name repository paths only when the path is necessary to perform contributor work.
- Do not add marketing claims, publication-process commentary, repository-synchronization commentary, broad banned-word tests, or brittle assertions over ordinary prose.
- Do not change Jazz syntax, semantics, compiler behavior, standard-library APIs, routes, navigation, search, logo placement, highlighting, or type-link rendering.
- Preserve every `jazz-signature` fence metadata token and the exact public signature it marks.
- Preserve generated examples and content required by `scripts/check-public-docs.py`, `scripts/check-stdlib-api-docs.py`, and `website/scripts/test-experience.mjs`.
- Run website commands through the checked-in Node 22/Nix environment and pinned pnpm 11.18.0.
- Commit each independently reviewable content group.

---

### Task 1: Rewrite the homepage, documentation introduction, and getting-started path

**Files:**

- Modify: `website/src/components/HomepageHeader.tsx`
- Modify: `website/src/components/DocumentationDirectory.tsx`
- Modify: `website/src/components/CodeProof.tsx`
- Modify: `docs/index.md`
- Modify: `docs/getting-started/overview.md`
- Modify: `docs/getting-started/installation.md`
- Modify: `docs/getting-started/first-program.md`
- Modify: `docs/getting-started/cli.md`

**Interfaces:**

- Preserves: the existing homepage component hierarchy, hero title lockup, checked code proof, and direct documentation routes.
- Produces: a factual language introduction and task-oriented first-use path.
- Consumes: current CLI behavior and checked example output; no capability claim may be inferred from roadmap copy.

- [x] **Step 1: Establish the focused baseline.** Run:

  ```bash
  nix --extra-experimental-features 'nix-command flakes' develop --command pnpm --dir website run test:experience
  bash scripts/check-public-docs.sh
  ```

  Expected: both commands pass before prose changes.

- [x] **Step 2: Rewrite the homepage copy.** Keep the existing layout and links. Make the header answer what Jazz is and how programs are organized at the language level. Remove parser/analyzer/typechecker inventories, accepted-form trivia, slogans, and prose that explains the visible command or output. Keep `CodeProof` limited to the filename, source, invocation, and result.

- [x] **Step 3: Rewrite the documentation introduction.** In `docs/index.md`, replace syntax-led orientation with a compact description of Jazz's programming model: typed functional programs, algebraic data, pattern matching, module boundaries, purity, and explicit capabilities. Keep “Choose a path,” but make each route description state the reader outcome rather than describe the documentation machinery.

- [x] **Step 4: Rewrite the getting-started sequence.** Apply these page-specific outcomes:

  - `overview.md`: what running a Jazz program demonstrates and where to continue;
  - `installation.md`: prerequisites and installation actions only, without explaining ordinary shell notation;
  - `first-program.md`: values, function application, inference, and the observed result, using the program rather than prose to carry syntax;
  - `cli.md`: task-based compile/run/module-graph guidance, leaving exact flags and precedence to `docs/reference/cli.md`.

- [x] **Step 5: Review the content boundary.** Confirm the homepage and getting-started pages contain no repository file inventory, publication/verification discussion, marketing promise, or sentence whose only information is already visible in the adjacent code or command.

- [x] **Step 6: Verify the first-use path.** Run:

  ```bash
  bash scripts/check-public-docs.sh
  nix --extra-experimental-features 'nix-command flakes' develop --command pnpm --dir website run test:experience
  nix --extra-experimental-features 'nix-command flakes' develop --command pnpm --dir website run typecheck
  git diff --check
  ```

  Expected: all commands exit zero.

- [x] **Step 7: Commit the first-use rewrite.** Run:

  ```bash
  git add website/src/components/HomepageHeader.tsx website/src/components/DocumentationDirectory.tsx website/src/components/CodeProof.tsx docs/index.md docs/getting-started
  git commit -m "docs: make the introduction language-first"
  ```

### Task 2: Rewrite the language guide around semantics and programming consequences

**Files:**

- Modify: `docs/language/overview.md`
- Modify: `docs/language/source-and-blocks.md`
- Modify: `docs/language/bindings-and-functions.md`
- Modify: `docs/language/types-and-signatures.md`
- Modify: `docs/language/algebraic-data-types-and-patterns.md`
- Modify: `docs/language/control-flow.md`
- Modify: `docs/language/operators.md`
- Modify: `docs/language/modules.md`
- Modify: `docs/language/purity.md`
- Modify: `docs/language/capabilities.md`

**Interfaces:**

- Produces: the conceptual Learn layer for the public language contract.
- Links to: lexical grammar, expression grammar, module resolution, runtime values, and relevant standard-library modules for exact detail.
- Preserves: supported examples and every limitation that materially affects how a reader writes a valid program.

- [x] **Step 1: Record the language-guide baseline.** Run:

  ```bash
  bash scripts/check-public-docs.sh
  rg -n "accepted|form|syntax|parser|token|terminated|spelled|written as|declaration" docs/language
  ```

  Expected: the docs checker passes; the search produces an audit list to classify rather than a list of automatically forbidden words.

- [x] **Step 2: Rewrite the overview and source model.** Make `overview.md` introduce Jazz's evaluation and programming model, then route readers to concepts. Make `source-and-blocks.md` explain scope, expression sequencing, final values, and where declarations are visible. Move or link exact terminator/module-declaration spelling to Reference instead of narrating punctuation.

- [x] **Step 3: Rewrite bindings, functions, and types.** Make `bindings-and-functions.md` explain immutability, lexical scope, closure capture, curried application, and evaluation consequences. Make `types-and-signatures.md` explain inference, what signatures guarantee, polymorphism, constraints, and mismatch behavior. Retain syntax only through compact representative fragments and Reference links.

- [x] **Step 4: Rewrite data, patterns, and control flow.** Make `algebraic-data-types-and-patterns.md` explain modeling alternatives, construction, destructuring, branch selection, binding, and exhaustiveness. Make `control-flow.md` explain expression results, branch type agreement, guard selection, and static rejection; do not narrate keywords already visible in examples.

- [x] **Step 5: Rewrite operators and modules.** Make `operators.md` explain precedence as a reading/evaluation contract, operator values, sections, and when source-local declarations affect a program; remove parser-internal framing. Make `modules.md` explain namespace boundaries, import visibility, export behavior, dependency ordering, and cycles; link exact lookup rules to module resolution.

- [x] **Step 6: Rewrite purity and capabilities.** Make `purity.md` explain referential transparency, effect boundaries, and the consequences for reasoning and composition. Make `capabilities.md` explain constrained polymorphism, instance selection, coherence, and failure to satisfy a required capability. Keep concrete syntax subordinate to those behaviors.

- [x] **Step 7: Perform the semantic evidence review.** For every added guarantee, locate support in `docs/reference/`, active `src/`, `jazz/`, or `test/`. Remove or narrow claims that are supported only by roadmap material. Verify that negative syntax notes remain only for likely misunderstandings.

- [x] **Step 8: Verify the language guide.** Run:

  ```bash
  bash scripts/check-public-docs.sh
  bash scripts/check-docs.sh
  nix --extra-experimental-features 'nix-command flakes' develop --command pnpm --dir website run build
  git diff --check
  ```

  Expected: documentation checks pass and Docusaurus reports no broken links.

- [x] **Step 9: Commit the language-guide rewrite.** Run:

  ```bash
  git add docs/language
  git commit -m "docs: explain Jazz through language semantics"
  ```

### Task 3: Tighten reference, compiler, and project documentation

**Files:**

- Modify: `docs/reference/lexical-grammar.md`
- Modify: `docs/reference/expression-grammar.md`
- Modify: `docs/reference/module-resolution.md`
- Modify: `docs/reference/cli.md`
- Modify: `docs/reference/diagnostics.md`
- Modify: `docs/reference/runtime-values.md`
- Modify: `docs/compiler/architecture.md`
- Modify: `docs/compiler/pipeline.md`
- Modify: `docs/compiler/bootstrapping.md`
- Modify: `docs/project/status.md`
- Modify: `docs/project/roadmap.md`
- Modify: `docs/project/governance.md`
- Modify: `docs/project/contributing.md`

**Interfaces:**

- Produces: compact exact lookup pages, stage-oriented compiler explanations, and factual project procedures.
- Preserves: all anchors targeted by signature type links, especially the built-in value anchors in `runtime-values.md`.
- Preserves: contributor commands and repository paths only where they are the actual instruction.

- [x] **Step 1: Establish structural and anchor baselines.** Run:

  ```bash
  bash scripts/check-docs.sh
  nix --extra-experimental-features 'nix-command flakes' develop --command pnpm --dir website run test:signatures
  ```

  Expected: both commands pass before prose changes.

- [x] **Step 2: Tighten lexical and expression reference.** Retain exact grammar, delimiters, precedence, and validity boundaries. Remove sentences that merely pronounce a token or repeat a production. Add concise semantic clarification only for ambiguous parsing, evaluation order, or a restriction whose consequence is not clear from the grammar.

- [x] **Step 3: Tighten operational reference pages.** Keep `module-resolution.md`, `cli.md`, and `diagnostics.md` organized for lookup. State resolution order, mode selection, exit/output behavior, diagnostic identity, and severity exactly. Remove tutorial transitions and duplicate explanations already owned by Learn.

- [x] **Step 4: Tighten runtime values without changing anchors.** Keep the existing headings for `Bool`, numeric types, tuples, and unit byte-for-byte at the heading level so type-link destinations remain stable. Explain representation, promotion, equality, rendering, and failure only where observable behavior differs or composes non-obviously.

- [x] **Step 5: Audit compiler pages by stage.** Preserve Source and modules, Parse, Resolve, Analyze, Diagnose, Interpret, and Prepare a backend as the common vocabulary. Remove syntax exposition, implementation-file discussion, and repetition between architecture and pipeline: architecture owns responsibilities, pipeline owns ordering, and bootstrapping owns hosted-stage parity and promotion.

- [x] **Step 6: Audit project pages by reader task.** Keep current status and roadmap limitations explicit; remove release-style promotion and implementation-detail clutter. Keep governance focused on decision authority and review. Keep contributing focused on actions, with repository paths only where a contributor needs them.

- [x] **Step 7: Verify exact contracts and stable anchors.** Run:

  ```bash
  bash scripts/check-docs.sh
  nix --extra-experimental-features 'nix-command flakes' develop --command pnpm --dir website run test:signatures
  nix --extra-experimental-features 'nix-command flakes' develop --command pnpm --dir website run build
  git diff --check
  ```

  Expected: all commands exit zero; the production type-link checker reports no missing destination or fragment.

- [x] **Step 8: Commit the reference and project rewrite.** Run:

  ```bash
  git add docs/reference docs/compiler docs/project
  git commit -m "docs: tighten reference and project guidance"
  ```

### Task 4: Remove redundant standard-library prose and examples

**Files:**

- Modify: `docs/standard-library/overview.md`
- Modify: `docs/standard-library/prelude.md`
- Modify: `docs/standard-library/maybe.md`
- Modify: `docs/standard-library/result.md`
- Modify: `docs/standard-library/nonempty.md`
- Modify: `docs/standard-library/list.md`
- Modify: `docs/standard-library/dictionary.md`
- Modify: `docs/standard-library/queue.md`
- Modify: `docs/standard-library/map.md`
- Modify: `docs/standard-library/set.md`
- Modify: `docs/standard-library/char.md`
- Modify: `docs/standard-library/text.md`
- Modify: `docs/standard-library/io.md`
- Modify: `docs/standard-library/io-error.md`

**Interfaces:**

- Consumes: the public export and signature inventory from `jazz/stdlib/*.jz`.
- Preserves: one heading and one exact linked signature for every public type, constructor, and value required by `scripts/check-stdlib-api-docs.py`.
- Produces: short API contracts and only behavior-bearing examples.

- [x] **Step 1: Establish API coverage and generated-link baselines.** Run:

  ```bash
  python3 scripts/check-stdlib-api-docs.py .
  python3 scripts/test-check-stdlib-api-docs.py
  nix --extra-experimental-features 'nix-command flakes' develop --command pnpm --dir website run test:signatures
  nix --extra-experimental-features 'nix-command flakes' develop --command pnpm --dir website run build
  ```

  Expected: all checks pass and the production checker reports nonzero Jazz signatures and type links.

- [x] **Step 2: Rewrite module introductions and type entries.** Make each introduction explain the abstraction and when it matters. For algebraic types, explain the alternatives and invariant. For abstract collections, state persistence, ordering model, key requirements, and meaningful complexity once at module or type level rather than repeating it per function.

- [x] **Step 3: Audit every value contract.** Preserve each heading, marker, and signature. Delete paraphrases whose only content is the function name or type transformation. Retain or rewrite prose for empty collections, missing values, ordering and stability, bounds/clamping, eager versus deferred callbacks, error propagation, normalization, host effects, and asymptotic cost.

- [x] **Step 4: Audit every example.** Delete examples for direct predicates, constructors, conversions, and accessors when the signature and useful contract fully explain them. Retain short examples for argument order, nested branch behavior, fallback evaluation, stable sorting/grouping, boundary handling, text normalization, effects, and error cases. Keep examples as expressions; do not expand them into complete programs.

- [x] **Step 5: Resolve duplicate and ambiguous API presentation.** In particular, distinguish type and constructor entries that share a visible name, such as `NonEmpty` and `IOError`, through their surrounding section and contract without changing canonical headings or signatures. Ensure Prelude compatibility helpers are identified as compatibility surface rather than presented as preferred duplicate APIs.

- [x] **Step 6: Run API coverage after the editorial pass.** Run:

  ```bash
  python3 scripts/check-stdlib-api-docs.py .
  python3 scripts/test-check-stdlib-api-docs.py
  nix --extra-experimental-features 'nix-command flakes' develop --command pnpm --dir website run test:signatures
  nix --extra-experimental-features 'nix-command flakes' develop --command pnpm --dir website run build
  git diff --check
  ```

  Expected: every public export remains documented, all exact signatures match source, every type-link destination resolves, and ordinary examples remain unlinked.

- [x] **Step 7: Commit the API editorial pass.** Run:

  ```bash
  git add docs/standard-library
  git commit -m "docs: make standard library contracts concise"
  ```

### Task 5: Integrated editorial and production closeout

**Files:**

- Modify: `.codex/plans/2026-08-13-jazz-semantic-documentation-rewrite.md` only to check completed steps and append exact verification receipts.

**Interfaces:**

- Consumes: the four committed content groups above.
- Produces: one complete page-by-page editorial audit and a production-ready static website artifact.

- [x] **Step 1: Audit every rendered public source.** Enumerate `docs/**/*.md` and public homepage copy. For each file, confirm its category, reader question, absence of redundant syntax narration, absence of marketing/meta copy, supported semantic claims, and valid internal links. Record any corrections directly in the owning task's files.

- [x] **Step 2: Review the complete diff for accidental contract changes.** Run:

  ```bash
  git diff origin/main...HEAD -- docs website/src/components .codex/plans/2026-08-13-jazz-semantic-documentation-rewrite-design.md .codex/plans/2026-08-13-jazz-semantic-documentation-rewrite.md
  ```

  Confirm that routes, signature markers, signatures, built-in headings, generated source imports, navigation, and component structure did not change except where this plan explicitly permits copy changes.

- [x] **Step 3: Run the complete documentation gate.** Run:

  ```bash
  nix --extra-experimental-features 'nix-command flakes' develop --command bash scripts/check-docs.sh
  ```

  Expected: all public-doc, API coverage, Markdown visibility, example, RFC, authority, link, and formatting checks pass.

- [x] **Step 4: Run the complete website gate.** Run:

  ```bash
  nix --extra-experimental-features 'nix-command flakes' develop --command bash scripts/check-website.sh
  ```

  Expected: brand, search, signature, experience, typecheck, production build, Pagefind, highlighting, type-link, and publication-boundary checks pass.

- [x] **Step 5: Inspect the production content.** Serve `website/build` and inspect the homepage, one getting-started page, representative language and reference pages, compiler architecture, Project Status, and Prelude/Maybe/List/Text/IO standard-library pages at desktop and mobile widths. Confirm readable density, intact search, linked signature types, stable anchors, no overflow, and no visually orphaned headings caused by deleted prose.

- [x] **Step 6: Run repository hygiene checks.** Run:

  ```bash
  git diff --check
  git status --short
  ```

  Expected: no whitespace errors and only the intentionally preserved untracked `.playwright-cli/` directory outside committed work.

- [x] **Step 7: Record receipts and commit closeout.** Check completed steps, append command results and production counts to this plan, then run:

  ```bash
  git add .codex/plans/2026-08-13-jazz-semantic-documentation-rewrite.md
  git commit -m "docs: record semantic rewrite verification"
  ```

- [ ] **Step 8: Push the branch and refresh the pull request.** Run:

  ```bash
  git push origin codex/docs-search-signature-links
  gh pr checks 136 --watch
  ```

  Expected: the branch pushes successfully and required pull-request checks pass. Re-read unresolved review threads after the push and address only comments that remain applicable to this rewrite.

## Verification receipts

- `scripts/check-docs.sh`: passed on 2026-08-13, including public documentation, standard-library API coverage, RFC structure, authority, clarification, links, examples, and execution-queue checks.
- `scripts/check-website.sh`: passed on 2026-08-13, including 30 experience tests, 10 signature-link tests, TypeScript typecheck, Docusaurus production build, and publication-boundary checks.
- Production content: Pagefind indexed 42 pages and 42 fragments; highlighting verified 240 Jazz blocks and 6 token colors; type-link validation verified 225 signatures, 683 links, and 15 ordinary examples.
- Editorial audit: all 42 public Markdown pages plus homepage components reviewed; no repository-synchronization, compiler-backed-publication, marketing, or parser-internal framing remained outside contributor instructions and exact reference contracts.
- Browser QA: desktop 1440x1000 and mobile 390x844 passed across the homepage, getting started, language, reference, compiler, project, and representative standard-library pages. The pages had no horizontal document overflow; long mobile signatures remained locally scrollable.
- Search QA: `Meta+K` focused the search combobox; `maybeMap` returned the `Maybe` API entry; keyboard selection opened the exact API anchor; `Escape` closed the dialog and restored focus to the search button. A clean reproduction completed with zero console errors.
- Repository hygiene: `git diff --check` passed; only the pre-existing untracked `.playwright-cli/` directory remained outside committed work.
- Non-blocking known warning: the production server bundle still reports Docusaurus/Webpack's dynamic-dependency warning for `SearchBar/index.tsx`; the build and all generated search checks pass.
