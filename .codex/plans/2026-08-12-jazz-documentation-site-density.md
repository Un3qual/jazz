# Jazz Documentation Site Density Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Turn the Jazz website into a compact, technically organized programming-language documentation site with separate learning, reference, and module API navigation.

**Architecture:** Keep the existing Docusaurus docs plugin, theme, branding, TextMate highlighter, and checked-example pipeline. Split navigation into three sidebars, replace the editorial homepage with a concise introduction and documentation directory, split combined standard-library pages into one page per module, and rewrite public compiler/site copy around language behavior and compiler stages.

**Tech Stack:** Docusaurus 3.10.2, React 19, TypeScript 6, CSS Modules, Markdown, Node 22, pnpm 11.18.0, Python and shell publication checks.

## Global Constraints

- Preserve the existing Jazz colors, typography, wordmark, Bellhook mark, light/dark modes, focus treatment, and TextMate highlighting.
- The navbar contains Learn, Language, Standard Library, Reference, and GitHub; it does not contain Status.
- Learn, Standard Library, and Reference use independent Docusaurus sidebars within the existing single docs plugin.
- Every standard-library module has one page and one module-tree entry.
- Public teaching, language, compiler, and API copy describes Jazz behavior rather than repository synchronization, publication, or verification mechanics.
- Compiler documentation explains conceptual stages and data transitions instead of active source-file ownership.
- Contributor-only pages may name repository paths when those paths are necessary instructions.
- Keep mobile interactive targets at least 44px while making desktop navigation and documentation typography denser.
- Do not change Jazz syntax, compiler behavior, standard-library APIs, or generated-example/highlighting mechanisms.
- Run website and docs commands through the checked-in Node 22/Nix environment and pinned pnpm 11.18.0.
- Commit every completed milestone independently.

---

### Task 1: Separate navigation and tighten the documentation shell

**Files:**

- Modify: `website/scripts/test-experience.mjs`
- Modify: `website/sidebars.ts`
- Modify: `website/docusaurus.config.ts`
- Modify: `website/src/css/custom.css`

**Interfaces:**

- Produces: Docusaurus sidebars named `learnSidebar`, `standardLibrarySidebar`, and `referenceSidebar`.
- Produces: navbar destinations for those sidebars plus the direct Language and external GitHub links.
- Preserves: the single classic docs plugin at route base `/docs`.
- Preserves: a production-buildable sidebar inventory by using the existing
  combined standard-library pages until Task 3 replaces them atomically.

- [x] **Step 1: Add failing navigation and density contracts.** Replace the single-sidebar assumptions in `website/scripts/test-experience.mjs` with checks that load the active site configuration and read `website/sidebars.ts`. Require:

  ```js
  assert.deepEqual(
    siteConfig.themeConfig.navbar.items.map(({ label }) => label),
    ["Learn", "Language", "Standard Library", "Reference", "GitHub"],
  );
  assert.doesNotMatch(
    JSON.stringify(siteConfig.themeConfig.navbar.items),
    /Status/,
  );

  for (const sidebar of [
    "learnSidebar",
    "standardLibrarySidebar",
    "referenceSidebar",
  ]) {
    assert.match(sidebars, new RegExp(`\\b${sidebar}\\s*:`));
  }
  assert.doesNotMatch(sidebars, /\\bjazzSidebar\\s*:/);
  ```

  Add CSS contract checks for a desktop navbar height smaller than `4.25rem`, explicit vertical centering on `.navbar__inner`, `.navbar__items`, and `.navbar__link`, and a mobile rule that retains 44px targets.

- [x] **Step 2: Run the focused website test and verify RED.** Run:

  ```bash
  nix --extra-experimental-features 'nix-command flakes' develop --command pnpm --dir website run test:experience
  ```

  Expected: failure because only `jazzSidebar` exists, the navbar still contains Docs and Status, and the navbar height remains `4.25rem`.

- [x] **Step 3: Define the three sidebars.** In `website/sidebars.ts`, create:

  - `learnSidebar`: index, Getting Started, Language, Compiler, and Project categories;
  - `standardLibrarySidebar`: overview, Prelude, and the existing library pages,
    grouped under Data, Collections, Text, and System where their current
    combined ownership permits it; Task 3 replaces the combined ids with the
    final one-module tree; and
  - `referenceSidebar`: the six exact reference pages.

  Use normal category labels and `collapsed: false` only for the top-level module groups that should remain visible as the API tree. Do not duplicate any document id between sidebars.

- [x] **Step 4: Replace the navbar contract.** In `website/docusaurus.config.ts`:

  - rename Docs to Learn and target `learnSidebar`;
  - retain the direct Language overview link;
  - target Standard Library at `standardLibrarySidebar`;
  - add Reference targeting `referenceSidebar`;
  - remove Status; and
  - retain GitHub on the right.

- [x] **Step 5: Tighten and center the shell.** In `website/src/css/custom.css`:

  - set `--ifm-navbar-height` to `3.5rem`;
  - size the desktop wordmark wrapper to fit inside that height;
  - explicitly center `.navbar__inner`, `.navbar__items`, `.navbar__brand`, and `.navbar__link`;
  - remove unconditional 44px height from desktop navbar/sidebar links where it creates excess vertical space;
  - use compact desktop menu padding and line height; and
  - restore `min-height: 44px` and `min-width: 44px` for the navbar drawer and other mobile controls inside the existing `max-width: 996px` rule.

  Also reduce document `h1` size, section gaps, body leading, breadcrumbs, and desktop sidebar row padding without reducing the document content column below its current readable width.

- [x] **Step 6: Run focused tests and type checking.** Run:

  ```bash
  nix --extra-experimental-features 'nix-command flakes' develop --command pnpm --dir website run test:experience
  nix --extra-experimental-features 'nix-command flakes' develop --command pnpm --dir website run typecheck
  nix --extra-experimental-features 'nix-command flakes' develop --command pnpm --dir website run build
  git diff --check
  ```

  Expected: all commands exit zero.

- [x] **Step 7: Commit the navigation milestone.** Run:

  ```bash
  git add website/scripts/test-experience.mjs website/sidebars.ts website/docusaurus.config.ts website/src/css/custom.css
  git commit -m "website: separate documentation navigation"
  ```

### Task 2: Replace the editorial homepage with a compact introduction

**Files:**

- Modify: `website/scripts/test-experience.mjs`
- Modify: `website/src/pages/index.tsx`
- Modify: `website/src/pages/index.module.css`
- Modify: `website/src/components/HomepageHeader.tsx`
- Modify: `website/src/components/CodeProof.tsx`
- Create: `website/src/components/DocumentationDirectory.tsx`
- Delete: `website/src/components/EditorialBand.tsx`
- Delete: `website/src/components/HomepageFooterCta.tsx`

**Interfaces:**

- `HomepageHeader(): ReactNode` produces the compact language introduction and contains the checked code proof.
- `DocumentationDirectory(): ReactNode` produces grouped direct links to Learn, Standard Library, Reference, Compiler, and Project destinations.
- `CodeProof(): ReactNode` renders only the Jazz example, invocation, and result; it does not explain build synchronization.

- [x] **Step 1: Replace the old homepage source-shape test with a behavioral content contract.** Require one `h1`, a `header`, a `main`, one Jazz code proof, and direct routes for:

  ```text
  /docs/getting-started/overview
  /docs/language/overview
  /docs/standard-library/overview
  /docs/reference/expression-grammar
  /docs/compiler/architecture
  /docs/project/status
  ```

  Reject the old components and copy:

  ```js
  for (const forbidden of [
    "EditorialBand",
    "HomepageFooterCta",
    "Language, in three movements",
    "Strong ideas. Clear notation.",
    "The next phrase is yours",
    "synchronized directly from the repository",
    "compiler-backed example check",
  ]) {
    assert.doesNotMatch(source, new RegExp(forbidden.replaceAll(".", "\\.")));
  }
  ```

  Replace the full-bleed/hero-motion test with a density contract that rejects `min-height: calc(100svh`, `100vw`, `.editorialBand`, and `.closing`, while retaining focus-visible, reduced-motion, and mobile rules.

- [x] **Step 2: Run the focused website test and verify RED.** Run the Task 1 focused website test. Expected: failure on the old editorial components, old copy, and viewport-filling hero CSS.

- [x] **Step 3: Build the compact introduction.** Update `HomepageHeader.tsx` so the first viewport contains:

  - `h1` Jazz;
  - the factual sentence “A statically typed functional language with practical syntax.”;
  - links to Getting Started and the Language Guide;
  - the existing Bellhook mark as a restrained brand element; and
  - `CodeProof` in the adjacent column.

  Update `CodeProof.tsx` so its accessible label and caption present `factorial.jz`, the generated invocation, and the generated output without any synchronization or verification prose.

- [x] **Step 4: Add the documentation directory.** Create `DocumentationDirectory.tsx` with five plain sections: Learn, Standard Library, Reference, Compiler, and Project. Each section gets one factual sentence and 2-4 direct links; use lists and dividers, not cards, badges, metrics, or CTA copy.

- [x] **Step 5: Simplify the page composition.** Make `index.tsx` render `HomepageHeader` and `DocumentationDirectory` only. Delete `EditorialBand.tsx` and `HomepageFooterCta.tsx` after all imports are removed.

- [x] **Step 6: Replace homepage CSS.** Rebuild `index.module.css` around:

  - a centered content width no wider than `92rem`;
  - a compact two-column intro with `clamp()` gutters and a single-column mobile breakpoint;
  - a modest display title rather than the current 5-10rem hero title;
  - a two- or three-column documentation directory using border rules instead of cards;
  - short entrance/link transitions; and
  - the existing reduced-motion override.

  Remove unused hero, score-line, editorial-band, and closing-section selectors and keyframes.

- [x] **Step 7: Run focused tests and type checking.** Run the Task 1 Step 6 commands. Expected: all commands exit zero.

- [x] **Step 8: Commit the homepage milestone.** Run:

  ```bash
  git add website/scripts/test-experience.mjs website/src/pages/index.tsx website/src/pages/index.module.css website/src/components
  git commit -m "website: make the homepage documentation-first"
  ```

### Task 3: Split the standard library into a module tree

**Files:**

- Modify: `scripts/check-public-docs.py`
- Modify: `website/scripts/test-experience.mjs`
- Modify: `website/sidebars.ts`
- Modify: `docs/standard-library/overview.md`
- Create: `docs/standard-library/maybe.md`
- Create: `docs/standard-library/result.md`
- Create: `docs/standard-library/nonempty.md`
- Create: `docs/standard-library/map.md`
- Create: `docs/standard-library/set.md`
- Create: `docs/standard-library/char.md`
- Create: `docs/standard-library/text.md`
- Create: `docs/standard-library/io.md`
- Create: `docs/standard-library/io-error.md`
- Delete: `docs/standard-library/maybe-result-nonempty.md`
- Delete: `docs/standard-library/map-and-set.md`
- Delete: `docs/standard-library/char-and-text.md`
- Modify: `docs/standard-library/list.md`
- Modify: `docs/standard-library/dictionary.md`
- Modify: `docs/standard-library/queue.md`
- Modify: `docs/standard-library/prelude.md`
- Modify: `docs/language/purity.md`
- Modify: `docs/reference/cli.md`
- Modify: `docs/reference/runtime-values.md`

**Interfaces:**

- Produces: stable module routes `/docs/standard-library/<lowercase-module-name>` except `IOError`, which uses `/docs/standard-library/io-error`.
- Produces: required public page inventory entries for all 13 documented modules: Prelude, Maybe, Result, NonEmpty, List, Dictionary, Queue, Map, Set, Char, Text, IO, and IOError, plus the overview.
- Removes: the four combined-page routes.

- [x] **Step 1: Change the required-page and sidebar contracts before creating pages.** Replace the four combined entries in `REQUIRED_PAGES` with the nine split-page names. Extend the website experience test to require every final module id in `standardLibrarySidebar` and reject `maybe-result-nonempty`, `map-and-set`, and `char-and-text`.

- [x] **Step 2: Run public-doc and website tests and verify RED.** Run:

  ```bash
  python3 scripts/test-check-public-docs.py
  nix --extra-experimental-features 'nix-command flakes' develop --command pnpm --dir website run test:experience
  ```

  Expected: failure because the required split pages do not yet exist and the sidebar still references combined pages.

- [x] **Step 3: Split Data modules.** Move the existing `Maybe`, `Result`, and `NonEmpty` contracts into `maybe.md`, `result.md`, and `nonempty.md`. Each page must name its public constructors, helpers, branch behavior, and meaningful complexity. Cross-link conversions between Maybe and Result and the pattern-matching guide.

- [x] **Step 4: Split collection modules.** Move Map and Set into `map.md` and `set.md`; retain the separate List, Dictionary, and Queue pages. Organize every collection page using consistent operation headings such as Construction, Querying, Updating, Traversal, and Complexity, omitting empty headings where a module does not expose that family.

- [x] **Step 5: Split text and system modules.** Move Char and Text into `char.md` and `text.md`. Split the public error ADT into `io-error.md` and effectful host operations into `io.md`. Cross-link IO results to IOError and Result, and link text literal spelling to lexical grammar.

- [x] **Step 6: Replace the Standard Library overview.** Remove the abstraction/purpose table and repository-root `--module-root jazz/stdlib` command. Explain automatic Prelude loading and explicit imports in two short paragraphs, followed by the same Data, Collections, Text, and System module tree as linked Markdown lists.

- [x] **Step 7: Update all internal links and the sidebar tree.** Point language/reference/library pages at the new module routes. Ensure the Standard Library sidebar categories use the exact tree from the design and no document id appears in Learn or Reference.

- [x] **Step 8: Run focused publication and website checks.** Run:

  ```bash
  python3 scripts/test-check-public-docs.py
  bash scripts/check-public-docs.sh
  nix --extra-experimental-features 'nix-command flakes' develop --command pnpm --dir website run test:experience
  nix --extra-experimental-features 'nix-command flakes' develop --command pnpm --dir website run build
  git diff --check
  ```

  Expected: all commands exit zero and Docusaurus reports no broken links.

- [x] **Step 9: Commit the module-tree milestone.** Run:

  ```bash
  git add docs/standard-library docs/language/purity.md docs/reference scripts/check-public-docs.py website/sidebars.ts website/scripts/test-experience.mjs
  git commit -m "docs: organize the standard library by module"
  ```

### Task 4: Remove meta copy and document compiler stages

**Files:**

- Modify: `website/scripts/test-experience.mjs`
- Modify: `website/src/components/CodeProof.tsx` if any prohibited copy remains
- Modify: `docs/getting-started/overview.md`
- Modify: `docs/compiler/architecture.md`
- Modify: `docs/compiler/pipeline.md`
- Modify: `docs/compiler/bootstrapping.md`
- Modify: `docs/project/status.md`
- Modify: `scripts/check-docs.sh`
- Modify: `README.md`

**Interfaces:**

- Produces: compiler documentation whose shared phase vocabulary is Source and modules, Parse, Resolve, Analyze, Diagnose, Interpret, and Prepare a backend.
- Preserves: explicit separation between ordinary canonical-core interpretation and the bounded opt-in typed-core/lowered-IR path.
- Removes: public Pages-enablement, repository-sync, compiler-backed-documentation, active-source-path, and documentation-snapshot commentary.

- [x] **Step 1: Add failing public-copy and compiler-stage contracts.** Add a test that concatenates the homepage components, `docs/getting-started/overview.md`, the three compiler pages, and `docs/project/status.md`. Reject these public phrases case-insensitively:

  ```text
  available after merge
  Pages enablement
  post-merge follow-up
  synchronized directly from the repository
  compiler-backed example check
  documentation-only commits
  Implementation snapshot:
  ```

  For the three compiler pages, reject active-path forms matching `` `(?:src|jazz|app|test|programs)/ ``. Require architecture headings or lead terms for Source and modules, Parse, Resolve, Analyze, Diagnose, Interpret, and Prepare a backend.

- [x] **Step 2: Run the focused website test and verify RED.** Run the Task 1 focused website test. Expected: failure on the getting-started deployment note, architecture source paths, and status snapshot commentary.

- [x] **Step 3: Rewrite Getting Started as user documentation.** Remove the website publication/settings paragraph. Start with what the Jazz executable can compile and run, retain the checked Hello example and command, and end with the first-program and language-guide links.

- [x] **Step 4: Rewrite compiler architecture around stages.** In `architecture.md`, give each conceptual stage one short section explaining its input, responsibility, and output. Explain module-graph ordering where relevant, keep diagnostics presentation-neutral, and distinguish the canonical interpreter from optional backend preparation. Do not name implementation modules or repository paths.

- [x] **Step 5: Tighten pipeline and bootstrapping.** Keep pipeline ordering exact for standalone and module-graph compilation, but express it through the shared stage vocabulary. Keep bootstrapping focused on stage 0, hosted components, parity evidence, the current promotion boundary, and remaining gates; remove source-layout discussion and long implementation-inventory prose.

- [x] **Step 6: Remove snapshot and publication commentary.** Remove the implementation commit hash and documentation-only disclaimer from Project Status. Update `scripts/check-docs.sh` to require the maintained `Updated: 2026-08-12` line but stop requiring a source snapshot hash. Remove the obsolete Pages-enablement note from the README website link while preserving the link itself.

- [x] **Step 7: Run focused docs and website checks.** Run:

  ```bash
  nix --extra-experimental-features 'nix-command flakes' develop --command pnpm --dir website run test:experience
  bash scripts/check-docs.sh
  nix --extra-experimental-features 'nix-command flakes' develop --command pnpm --dir website run typecheck
  git diff --check
  ```

  Expected: all commands exit zero.

- [x] **Step 8: Commit the content milestone.** Run:

  ```bash
  git add README.md docs/getting-started/overview.md docs/compiler docs/project/status.md scripts/check-docs.sh website/scripts/test-experience.mjs website/src/components/CodeProof.tsx
  git commit -m "docs: make public documentation technical and direct"
  ```

### Task 5: Integrated website closeout

**Files:**

- Modify: `.codex/plans/2026-08-12-jazz-documentation-site-density.md` only to record completed steps and verification receipts.

**Interfaces:**

- Consumes: the four committed milestones above.
- Produces: a clean production build and one authoritative Node 22/Nix website receipt.

- [x] **Step 1: Review the complete implementation diff.** Run:

  ```bash
  git diff 9a8e66eb..HEAD -- website docs scripts/check-public-docs.py scripts/check-docs.sh README.md
  rg -n -i 'vibecode|marketing|synchronized directly|compiler-backed example check|available after merge|Pages enablement|documentation-only commits|Implementation snapshot' website/src docs README.md
  ```

  Expected: the diff contains only the approved documentation-site work; the text scan returns no prohibited public copy.

- [x] **Step 2: Run the authoritative website gate once.** Run:

  ```bash
  nix --extra-experimental-features 'nix-command flakes' develop --command bash scripts/check-website.sh
  ```

  Expected: brand tests, experience tests, TypeScript, production build, TextMate output check, Pages policy, and website boundary checks all pass.

- [x] **Step 3: Run repository documentation and diff gates.** Run:

  ```bash
  nix --extra-experimental-features 'nix-command flakes' develop --command bash scripts/check-docs.sh
  git diff --check
  git status --short
  ```

  Expected: docs and diff checks pass; status contains only the implementation-plan receipt update.

- [x] **Step 4: Record receipts and commit closeout.** Mark completed plan steps, record the exact successful commands and date under this task, then run:

  ```bash
  git add .codex/plans/2026-08-12-jazz-documentation-site-density.md
  git commit -m "docs: close documentation site redesign"
  ```

**Verification receipts (2026-08-12):**

- `nix --extra-experimental-features 'nix-command flakes' develop --command bash scripts/check-website.sh` — passed: 10 brand tests, 20 experience tests, TypeScript, production build, Jazz highlighting, Pages policy, and website boundary checks.
- `nix --extra-experimental-features 'nix-command flakes' develop --command bash scripts/check-docs.sh` — passed: public documentation, Markdown visibility, example, RFC structure, authority, and queue checks.
- Browser visual QA — passed at 1440×1000 and 390×844: centered navigation, no horizontal overflow, responsive homepage, module-tree sidebar, and stage-based compiler architecture.
- `git diff --check` and prohibited-copy scans — passed with no findings.
