# Jazz Documentation Search and Signature Type Links Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task. Keep the checkboxes and verification receipts current.

**Goal:** Add keyboard-first, entirely static documentation search and turn every known concrete type or capability in standard-library signature blocks into an unobtrusive link to its canonical documentation.

**Architecture:** Run Pagefind 1.5.2 after the Docusaurus production build and load its generated browser API only when the search dialog opens. Mark only rendered document content as indexable. Carry the existing `<!-- jazz-signature -->` marker through the Markdown AST into code-block metadata, then split TextMate-highlighted signature tokens against one explicit type-destination map. Preserve ordinary Jazz blocks and all existing syntax colors.

**Tech Stack:** Docusaurus 3.10.2, React 19, TypeScript, CSS modules, Pagefind 1.5.2, Shiki 4.4.2, Node's test runner, pinned pnpm 11.18.0/Node 22 Nix environment.

## Global Constraints

- The production site remains a self-contained GitHub Pages artifact: no hosted search, API key, telemetry, remote fallback, or runtime network dependency.
- Index only the main documentation content, not the navbar, sidebar, table of contents, paginator, or footer.
- Put Search immediately before GitHub in the navbar; do not add a status item or change the other navigation groups.
- Open search from its control, `/`, or `Ctrl/Cmd+K`; never intercept `/` from an input, textarea, select, or editable element.
- Use a native modal dialog or equivalent accessible focus trap, return focus on close, and support Escape, Arrow Up/Down, and Enter.
- Only adjacent `<!-- jazz-signature -->` Jazz fences gain links. Ordinary examples and non-Jazz blocks must render exactly as before.
- Link every mapped concrete type, built-in, and capability occurrence; leave generic variables and unknown identifiers plain.
- Preserve TextMate token colors. Type links have no underline in default, hover, active, visited, or focus states; hover/focus use background and outline instead.
- Resolve every route through the configured `/jazz/` base URL.
- Commit each independently reviewable milestone.

---

### Task 1: Add the static search index to the production artifact

**Files:**

- Modify: `website/package.json`
- Modify: `website/pnpm-lock.yaml`
- Modify: `website/src/theme/DocItem/Layout/index.tsx`
- Modify: `website/scripts/test-experience.mjs`
- Create: `website/scripts/check-built-search.mjs`
- Create: `website/scripts/test-check-built-search.mjs`

**Interfaces:**

- Consumes: rendered files under `website/build/` whose document-content wrapper has `data-pagefind-body`.
- Produces: `website/build/pagefind/pagefind.js`, the Pagefind WASM/runtime files, and index fragments shipped with the Pages artifact.

- [x] **Step 1: Add failing production-index contracts.** Extend `test-experience.mjs` to require a `data-pagefind-body` wrapper around `DocItemContent`, an exact `pagefind` dev dependency, and a build chain that runs Pagefind after Docusaurus and before built-output checks. Add `test-check-built-search.mjs` with temporary build fixtures for a missing runtime, missing WASM, empty index fragments, and a valid minimal generated index.

- [x] **Step 2: Run the focused test and verify RED.** Run:

  ```bash
  nix --extra-experimental-features 'nix-command flakes' develop --command pnpm --dir website run test:experience
  nix --extra-experimental-features 'nix-command flakes' develop --command node --test website/scripts/test-check-built-search.mjs
  ```

  Expected: failure because the document body is not marked, Pagefind is absent, the build contract has not changed, and the checker implementation does not exist.

- [x] **Step 3: Pin Pagefind and update the lockfile.** Run:

  ```bash
  nix --extra-experimental-features 'nix-command flakes' develop --command pnpm --dir website add --save-dev --save-exact pagefind@1.5.2
  ```

  Keep Pagefind in `devDependencies`; the browser runtime is generated into the static artifact and is not bundled from `node_modules`.

- [x] **Step 4: Mark only documentation content as searchable.** Wrap `DocItemContent` in a neutral element carrying `data-pagefind-body`. Keep breadcrumbs, version UI, mobile/desktop TOCs, footer, and paginator outside that element so repeated shell text cannot dominate results.

- [x] **Step 5: Extend the production build.** Change the website build script to run:

  ```text
  sync-factorial -> docusaurus build -> pagefind --site build --output-subdir pagefind -> highlighting check -> search-index check
  ```

  Make `check-built-search.mjs` assert the generated browser module, WASM, metadata, and at least one index fragment exist and that Pagefind reports indexed documentation pages. Do not generate the Pagefind playground.

- [x] **Step 6: Verify the generated artifact.** Run:

  ```bash
  nix --extra-experimental-features 'nix-command flakes' develop --command pnpm --dir website run test:experience
  nix --extra-experimental-features 'nix-command flakes' develop --command pnpm --dir website run build
  ```

  Expected: both commands pass and `website/build/pagefind/` contains a non-empty local index.

- [x] **Step 7: Commit the static-index milestone.** Run:

  ```bash
  git add website/package.json website/pnpm-lock.yaml website/src/theme/DocItem/Layout/index.tsx website/scripts/test-experience.mjs website/scripts/check-built-search.mjs website/scripts/test-check-built-search.mjs
  git commit -m "feat(website): build a static documentation index"
  ```

**Verification receipts (2026-08-12):**

- RED: `pnpm --dir website run test:experience` failed before implementation because the Pagefind body wrapper and exact dependency were absent; `node --test website/scripts/test-check-built-search.mjs` failed because the checker did not exist.
- GREEN: `nix --extra-experimental-features 'nix-command flakes' develop --command pnpm --dir website run test:experience` passed 21 tests, and `nix --extra-experimental-features 'nix-command flakes' develop --command node --test website/scripts/test-check-built-search.mjs` passed all four generated-artifact fixtures.
- Artifact: `nix --extra-experimental-features 'nix-command flakes' develop --command pnpm --dir website run build` passed after Pagefind 1.5.2 indexed 42 documentation pages and emitted 42 non-empty fragments.

### Task 2: Build the keyboard-first search dialog

**Files:**

- Create: `website/scripts/pagefind-search-model.mjs`
- Create: `website/scripts/pagefind-search-model.d.mts`
- Create: `website/scripts/test-pagefind-search-model.mjs`
- Create: `website/src/theme/SearchBar/index.tsx`
- Create: `website/src/theme/SearchBar/styles.module.css`
- Modify: `website/docusaurus.config.ts`
- Modify: `website/package.json`
- Modify: `website/scripts/test-experience.mjs`
- Modify: `scripts/check-website.sh`

**Interfaces:**

- Consumes: the lazy-loaded generated module at `<baseUrl>/pagefind/pagefind.js` and Pagefind `search(query)`/`result.data()` responses.
- Produces: normalized result rows `{url, pageTitle, sectionTitle, category, excerpt}` plus an accessible navbar search dialog.

- [x] **Step 1: Write search-model tests.** Cover `/`, `Ctrl+K`, and `Cmd+K`; editable-target exclusion; category derivation for Getting started, Language, Standard library, Compiler, Project, and Reference routes; base-URL-safe result URLs; page results with and without Pagefind `sub_results`; and empty result normalization. Require excerpts to come only from the local Pagefind response.

- [x] **Step 2: Add failing navbar/UI contracts.** Extend `test-experience.mjs` to require a right-positioned `{type: 'search'}` item immediately before GitHub, a real swizzled `SearchBar`, a modal dialog with an accessible name, and explicit loading, empty, and unavailable copy. Add `test:search` to `website/package.json` and invoke it from `scripts/check-website.sh`.

- [x] **Step 3: Run the search tests and verify RED.** Run:

  ```bash
  nix --extra-experimental-features 'nix-command flakes' develop --command pnpm --dir website run test:search
  nix --extra-experimental-features 'nix-command flakes' develop --command pnpm --dir website run test:experience
  ```

  Expected: the model/UI contracts fail because neither implementation exists.

- [x] **Step 4: Implement the pure search model.** Add exact shortcut detection, editable-target detection, route categorization, result flattening, and `/jazz/`-safe URL normalization in `pagefind-search-model.mjs`. Keep this module DOM-free so the Node tests exercise the same behavior used by React.

- [x] **Step 5: Implement lazy Pagefind loading and dialog state.** In `SearchBar`, use the configured base URL to dynamically import the generated Pagefind browser module only after the dialog opens. Model `idle`, `loading`, `ready`, and `unavailable` states; cancel stale async query updates; and show a concise unavailable state during `docusaurus start`, where no generated index is expected.

- [x] **Step 6: Implement accessible interaction.** Use a modal `<dialog>` with autofocus search input, close control, keyboard hint, and a ranked result list. Keep the active row visible, handle Arrow Up/Down and Enter from the input, close on successful internal navigation, and rely on modal focus containment plus explicit opener-focus restoration. Register `/` and `Ctrl/Cmd+K` once and remove listeners on unmount.

- [x] **Step 7: Style the utility UI.** Make the desktop control compact and text-forward, reduce it to a search icon with an accessible label at narrow widths, make the mobile dialog use the available viewport width, and keep all touch targets at least 44px. Add restrained open/close and active-row state changes with a reduced-motion override; do not add cards, gradients, promotional copy, or persistent search history.

- [x] **Step 8: Verify search behavior.** Run:

  ```bash
  nix --extra-experimental-features 'nix-command flakes' develop --command pnpm --dir website run test:search
  nix --extra-experimental-features 'nix-command flakes' develop --command pnpm --dir website run test:experience
  nix --extra-experimental-features 'nix-command flakes' develop --command pnpm --dir website run typecheck
  nix --extra-experimental-features 'nix-command flakes' develop --command pnpm --dir website run build
  ```

  Then serve the production build and query `maybeMap`; verify a Standard library result links to the `maybeMap` section. Also verify `/`, `Ctrl/Cmd+K`, Escape, Arrow keys, Enter, empty results, and that typing `/` in the search input does not reopen or reset the dialog.

- [x] **Step 9: Commit the search-interface milestone.** Run:

  ```bash
  git add website/scripts/pagefind-search-model.mjs website/scripts/pagefind-search-model.d.mts website/scripts/test-pagefind-search-model.mjs website/src/theme/SearchBar website/docusaurus.config.ts website/package.json website/scripts/test-experience.mjs scripts/check-website.sh
  git commit -m "feat(website): add local documentation search"
  ```

**Verification receipts (2026-08-12):**

- RED: `nix --extra-experimental-features 'nix-command flakes' develop --command pnpm --dir website run test:search` failed because `pagefind-search-model.mjs` did not exist; `... run test:experience` failed because the search navbar item and swizzled dialog did not exist.
- GREEN: `nix --extra-experimental-features 'nix-command flakes' develop --command pnpm --dir website run test:search` passed all 6 model tests; `... run test:experience` passed all 21 tests; and `... run typecheck` passed.
- Artifact: `nix --extra-experimental-features 'nix-command flakes' develop --command pnpm --dir website run build` passed after Pagefind indexed 42 documentation pages and emitted 42 fragments.
- Browser: served the production artifact under `/jazz/`; `/`, `Ctrl+K`, and `Cmd+K` opened the named modal; `maybeMap` returned the Standard library `Maybe` section and Arrow Up/Down plus Enter navigated to `docs/standard-library/maybe.html#maybemap`; an impossible query rendered the empty state; `/` in the input remained text; and Escape closed the dialog and restored focus to its opener.

### Task 3: Carry signature markers into code-block metadata and map type syntax

**Files:**

- Create: `website/scripts/remark-jazz-signatures.mjs`
- Create: `website/scripts/remark-jazz-signatures.d.mts`
- Create: `website/scripts/test-remark-jazz-signatures.mjs`
- Create: `website/scripts/jazz-type-links.mjs`
- Create: `website/scripts/jazz-type-links.d.mts`
- Create: `website/scripts/test-jazz-type-links.mjs`
- Create: `website/src/theme/CodeBlock/Content/String/index.tsx`
- Modify: `website/docusaurus.config.ts`
- Modify: `website/package.json`
- Modify: `scripts/check-website.sh`

**Interfaces:**

- Consumes: a Markdown AST where an HTML `<!-- jazz-signature -->` node is immediately followed, allowing only whitespace, by a Jazz code node.
- Produces: a `jazz-signature` metastring flag, extended code-block metadata, and deterministic link spans `{start, end, destination}` over raw signature source.

- [x] **Step 1: Write marker-transform tests.** Cover an adjacent Jazz fence, the permitted blank line represented by the AST, a non-Jazz fence, an ordinary Jazz example, a non-adjacent marker, and preservation of existing fence metadata. Only the adjacent Jazz fence may receive `jazz-signature`.

- [x] **Step 2: Write type-map tests.** Require these destinations:

  - module pages for `Maybe`, `Result`, `NonEmpty`, `Dictionary`, `Queue`, `Map`, `Set`, `Char`, `Text`, and `List`;
  - List for `[` and `]` type-syntax delimiters;
  - `IOError` and `IOErrorCategory` headings on the IOError page;
  - Prelude headings for `Ordering`, `Eq`, `Ord`, `Num`, `Integral`, `Fractional`, `Showable`, and `Default`;
  - exact Runtime values anchors for `Bool`, `Int`, `Float`, `Int8/16/32/64`, `UInt8/16/32/64`, `Float16/32/64`, tuple delimiters, and unit syntax.

  Also cover repeated occurrences, nested types such as `Result(IOError, Maybe(Text))`, generic variables, lowercase values, unknown capitalized identifiers, list nesting, function-argument parentheses, tuple parentheses, and unit `()`.

- [x] **Step 3: Run the marker and mapping tests and verify RED.** Run:

  ```bash
  nix --extra-experimental-features 'nix-command flakes' develop --command pnpm --dir website run test:signatures
  ```

  Expected: failure because the transform and mapping modules do not exist.

- [x] **Step 4: Implement the remark transform.** Export a pure tree transform plus the remark plugin wrapper. Append `jazz-signature` to the following Jazz code node's `meta` without removing other metadata or altering the visible HTML marker. Register it in the classic docs preset before the default remark plugins.

- [x] **Step 5: Preserve the signature flag in Docusaurus context.** Swizzle `CodeBlock/Content/String`, reuse Docusaurus's standard metadata creation and word-wrap behavior, and add a typed `jazzSignature` boolean derived from the exact metastring token. Do not infer signatures from `::`, capitalization, the current route, or code contents.

- [x] **Step 6: Implement lexical link spans.** Keep one explicit immutable destination map. Match known identifiers exactly; detect balanced list syntax; classify parentheses as unit or tuple only when structurally warranted; and link only the delimiters for composite list/tuple syntax so nested concrete identifiers can retain their own links. Resolve overlaps deterministically and leave unknowns untouched.

- [x] **Step 7: Verify marker and mapping behavior.** Run:

  ```bash
  nix --extra-experimental-features 'nix-command flakes' develop --command pnpm --dir website run test:signatures
  nix --extra-experimental-features 'nix-command flakes' develop --command pnpm --dir website run typecheck
  ```

  Expected: every transform/mapping regression and TypeScript check passes.

- [x] **Step 8: Commit the semantic-foundation milestone.** Run:

  ```bash
  git add website/scripts/remark-jazz-signatures.mjs website/scripts/remark-jazz-signatures.d.mts website/scripts/test-remark-jazz-signatures.mjs website/scripts/jazz-type-links.mjs website/scripts/jazz-type-links.d.mts website/scripts/test-jazz-type-links.mjs website/src/theme/CodeBlock/Content/String/index.tsx website/docusaurus.config.ts website/package.json scripts/check-website.sh
  git commit -m "feat(website): identify signature type destinations"
  ```

**Verification receipts (2026-08-12):**

- RED: `nix --extra-experimental-features 'nix-command flakes' develop --command pnpm --dir website run test:signatures` failed before implementation because both new semantic modules were absent.
- GREEN: the same focused command passed all 9 AST-transform and literal-span fixtures after implementation.
- Integration: `nix --extra-experimental-features 'nix-command flakes' develop --command pnpm --dir website run typecheck` and `... run build` passed; the build preserved the existing Pagefind artifact checks and indexed 42 documentation pages.
- Scope: Task 3 only carries an explicit marker into code-block metadata and returns deterministic raw-source spans. Rendering links and adding Runtime values anchors remain Task 4 work.
- Fix round 1/5: parser-backed fixtures now parse Markdown with direct pinned `unified@11.0.5` and `remark-parse@11.0.0`, then run the exported remark plugin wrapper for adjacent, blank-line, non-Jazz, ordinary, and non-adjacent cases. The focused suite passed all 10 tests; typecheck and build passed.

### Task 4: Render linked signature tokens and add canonical built-in anchors

**Files:**

- Modify: `website/src/theme/CodeBlock/Content/index.tsx`
- Modify: `website/src/theme/CodeBlock/Content/styles.module.css`
- Create: `website/src/theme/CodeBlock/Line/Token/index.tsx`
- Create: `website/src/theme/CodeBlock/Line/Token/styles.module.css`
- Create: `website/scripts/check-built-type-links.mjs`
- Modify: `website/scripts/test-experience.mjs`
- Modify: `website/package.json`
- Modify: `docs/reference/runtime-values.md`

**Interfaces:**

- Consumes: Shiki `ThemedToken[][]`, raw source offsets, `metadata.jazzSignature`, and the type-link spans from Task 3.
- Produces: ordinary token spans or internal `<a data-jazz-type-link>` elements inside `<pre data-jazz-signature>` while preserving copyable source text and code-line layout.

- [x] **Step 1: Add failing renderer/build contracts.** Require signature `<pre>` elements to expose `data-jazz-signature`, linked tokens to expose `data-jazz-type-link`, ordinary Jazz blocks to expose neither, and all links to pass through Docusaurus base-URL handling. Add a built-output checker that scans standard-library HTML for mapped links, confirms their route/fragment targets exist, and confirms at least one ordinary Jazz example has no type links.

- [x] **Step 2: Run focused checks and verify RED.** Run:

  ```bash
  nix --extra-experimental-features 'nix-command flakes' develop --command pnpm --dir website run test:experience
  nix --extra-experimental-features 'nix-command flakes' develop --command pnpm --dir website run build
  ```

  Expected: failure because no rendered type links or built-in anchors exist.

- [x] **Step 3: Split highlighted tokens at semantic boundaries.** For signature blocks only, walk TextMate tokens in source order, preserve every character and style bit, and split tokens where a type-link span begins or ends. Add the canonical destination to the resulting `JazzToken`; leave the existing code path unchanged for ordinary Jazz blocks.

- [x] **Step 4: Swizzle token output for internal links.** Keep the default `<span>` for every token without a destination. For linked tokens, render a Docusaurus internal link whose URL is produced by the base-URL utility. Preserve inherited token color and code semantics, and do not interfere with line numbering, wrapping, selection, or the copy button.

- [x] **Step 5: Style links as syntax first.** Set `color: inherit` and `text-decoration: none` for every link state, including visited, hover, active, and focus-visible. Use a small-radius background shift on hover and a visible offset outline/background on focus. Add no inline icon and no layout-changing font treatment.

- [x] **Step 6: Add stable Runtime values destinations.** Expand `runtime-values.md` with concise, individually headed entries for `Bool`, `Int`, `Int8`, `Int16`, `Int32`, `Int64`, `UInt8`, `UInt16`, `UInt32`, `UInt64`, `Float`, `Float16`, `Float32`, `Float64`, tuples, and unit. Keep the page dense, avoid tables, and preserve the existing numeric promotion, equality, rendering, and runtime-failure contracts.

- [x] **Step 7: Wire and run built-output verification.** Add `check-built-type-links.mjs` after the existing highlighting/search checks in the build script. It must verify representative links for module types, built-ins, capabilities, nested signatures, list syntax, tuple/unit syntax, and every destination fragment, while proving an ordinary marked example remains link-free.

- [x] **Step 8: Run documentation and website checks.** Run:

  ```bash
  nix --extra-experimental-features 'nix-command flakes' develop --command bash scripts/check-docs.sh
  nix --extra-experimental-features 'nix-command flakes' develop --command pnpm --dir website run test:signatures
  nix --extra-experimental-features 'nix-command flakes' develop --command pnpm --dir website run test:experience
  nix --extra-experimental-features 'nix-command flakes' develop --command pnpm --dir website run typecheck
  nix --extra-experimental-features 'nix-command flakes' develop --command pnpm --dir website run build
  ```

  Expected: all checks pass and the production HTML contains valid internal type links only in signature blocks.

- [x] **Step 9: Commit the linked-signature milestone.** Run:

  ```bash
  git add website/src/theme/CodeBlock/Content/index.tsx website/src/theme/CodeBlock/Content/styles.module.css website/src/theme/CodeBlock/Line/Token website/scripts/check-built-type-links.mjs website/scripts/test-experience.mjs website/package.json docs/reference/runtime-values.md
  git commit -m "feat(docs): link types in API signatures"
  ```

**Verification receipts (2026-08-12):**

- Architecture decision: after the three failed HTML-marker propagation hypotheses documented in the Task 4 report, the user approved native fence metadata. All 225 standard-library signatures now use ` ```jazz jazz-signature `; the obsolete remark marker plugin and adjacent HTML markers were removed, while ordinary `jazz-example` fences remain unchanged.
- RED: the public-doc checker rejected the new native fence and incorrectly accepted the legacy marker; the focused signature suite failed because the native metadata helper did not exist; the production build reached the linked-type checker and failed because no signature metadata reached rendered HTML.
- GREEN: the native fence parser regression proves the exact `jazz-signature` metastring reaches `metadata.jazzSignature`, while near matches and ordinary Jazz fences remain false. Public documentation tests pass 17/17 and standard-library API tests pass 9/9.
- Rendering: signature-only token splitting preserves TextMate content, color, and font-style bits; linked output uses Docusaurus base-URL resolution and retains copyable source text. Generated HTML contains 225 signature blocks and 683 links, with representative module, built-in, capability, nested, list, tuple, and unit targets present; 15 ordinary examples remain link-free.
- GREEN: `scripts/check-docs.sh`, `pnpm --dir website run test:signatures` (7/7), `pnpm --dir website run test:experience` (25/25), `pnpm --dir website run typecheck`, and `pnpm --dir website run build` all pass. The pre-existing SearchBar dynamic-dependency webpack warning remains unchanged.
- Commit: `ef458c1a` (`feat(docs): link types in API signatures`).

### Task 5: Integrated production and visual closeout

**Files:**

- Modify: `.codex/plans/2026-08-12-jazz-doc-search-signature-links.md` only for completed checkboxes and verification receipts.

**Interfaces:**

- Consumes: Tasks 1-4.
- Produces: a clean committed tree with authoritative docs/site gates and desktop/mobile browser evidence.

- [x] **Step 1: Review the complete change.** Run:

  ```bash
  git diff ee51f447..HEAD -- docs/reference/runtime-values.md scripts/check-website.sh website .codex/plans/2026-08-12-jazz-doc-search-signature-links.md
  git diff --check
  ```

  Confirm there is no hosted-search configuration, telemetry, marketing copy, guessed identifier linking, ordinary-example linking, or unrelated navbar/homepage redesign.

- [x] **Step 2: Run the authoritative documentation gate.** Run:

  ```bash
  nix --extra-experimental-features 'nix-command flakes' develop --command bash scripts/check-docs.sh
  ```

  Expected: all documentation, examples, link, authority, queue, and formatting checks pass.

- [x] **Step 3: Run the authoritative website gate.** Run:

  ```bash
  nix --extra-experimental-features 'nix-command flakes' develop --command bash scripts/check-website.sh
  ```

  Expected: search-model, signature-model, brand, experience, TypeScript, production build, Pagefind index, highlighting, linked-target, Pages policy, and boundary checks all pass.

- [x] **Step 4: Perform production browser QA.** Serve `website/build` and inspect 1440x1000 and 390x844 viewports. At both sizes verify no horizontal overflow; the Search control is reachable; `/` and `Ctrl/Cmd+K` open the dialog; focus is contained and restored; `maybeMap` returns the correct API section; an impossible query shows the empty state; Escape closes; and reduced motion removes transforms. On Maybe, Result, List, IO, and Prelude pages, verify type links retain syntax colors, have no underline in any state, show visible keyboard focus, and resolve under `/jazz/`. Verify an ordinary Jazz example contains no links.

- [x] **Step 5: Record receipts and commit closeout.** Mark completed plan steps, append the date and exact successful commands, then run:

  ```bash
  git add .codex/plans/2026-08-12-jazz-doc-search-signature-links.md
  git commit -m "docs: close documentation search work"
  ```

**Verification receipts (2026-08-12):**

- Review: `git diff ee51f447..HEAD -- docs/reference/runtime-values.md scripts/check-website.sh website .codex/plans/2026-08-12-jazz-doc-search-signature-links.md` and `git diff --check` passed. The reviewed change contains no hosted-search configuration, runtime telemetry, marketing copy, guessed identifier linking, ordinary-example linking, or unrelated navbar/homepage redesign.
- Documentation: `nix --extra-experimental-features 'nix-command flakes' develop --command bash scripts/check-docs.sh` passed all public-documentation, standard-library API, example, RFC, link, authority, clarification, execution-queue, regression, and formatting checks.
- Website: `nix --extra-experimental-features 'nix-command flakes' develop --command bash scripts/check-website.sh` passed the Pages policy, 10 brand tests, 8 search-model tests, 9 signature tests, 25 experience tests, TypeScript, production build, Pagefind index (42 pages and 42 fragments), highlighting (240 blocks and 6 token colors), linked targets (225 signatures and 683 links with 15 ordinary examples), and publication-boundary check.
- Closeout repair: the first website-gate run exposed CSS `url(...)` syntax being applied to generated JavaScript and misclassifying `new URL(...)` parsing bases from the site model and Pagefind. `python3 scripts/test-check-website-boundary.py` failed RED on those parsing bases, then passed 8/8 after resource contexts became suffix-aware while literal JavaScript `fetch()` and `import()` URLs remained rejected. Commit `df05951b` records the repair.
- Publication: `.github/workflows/docs-pages.yml` uploads `website/build`, so the generated `website/build/pagefind/` runtime, WASM, metadata, and fragments are included in the Pages artifact.
- Browser, desktop: the production artifact served at `/jazz/` under a 1440x1000 viewport with zero horizontal overflow. Search was reachable; `/`, `Cmd+K`, and `Ctrl+K` opened the modal with focus contained; `maybeMap` returned the Standard library Maybe section; Arrow Down/Up and Enter navigated to `/jazz/docs/standard-library/maybe.html#maybemap`; an impossible query showed the empty state; and navigation/close restored opener focus. The generated reduced-motion rule disables the search dialog animation/transition, so its transform keyframe does not run.
- Browser, mobile: at 390x844 the Search control was visible at 44x44, the page and dialog had zero horizontal overflow, `/` and `Cmd+K` opened the modal with focus contained, `maybeMap` returned and navigated to the same section, the impossible query showed the empty state, and close/navigation restored opener focus.
- Escape: the Task 2 production-browser receipt above records Escape closing the modal and restoring opener focus. During this closeout, the in-app Browser's synthetic Escape dispatch did not consistently trigger the native dialog cancel path; source inspection confirmed the production `<dialog onCancel>` still prevents the native default, calls `closeSearch()`, closes the dialog, clears state, and restores the opener. This is recorded as an automation limitation rather than contradictory product evidence.
- Signature links: Maybe (23), Result (22), List (194), IO (26), and Prelude (55) retained token colors, had no underline, resolved only under `/jazz/`, and showed a visible 2px keyboard-focus outline. `docs/getting-started/first-program` contained four ordinary Jazz blocks and zero type links.
- Browser evidence: `.superpowers/sdd/2026-08-12-jazz-doc-search-signature-links/evidence/desktop-search-empty-1440x1000.png`, `.superpowers/sdd/2026-08-12-jazz-doc-search-signature-links/evidence/desktop-signature-focus-1440x1000.png`, and `.superpowers/sdd/2026-08-12-jazz-doc-search-signature-links/evidence/mobile-search-results-390x844.png`.
- Minor ruling: the server compiler still warns that the base-URL-dependent Pagefind import request is an expression. It is the deliberate lazy local-runtime boundary, the production build and browser import both pass, and suppressing it narrowly would require hard-coding `/jazz/`, indirect evaluation, or bundling a generated module. No runtime behavior was changed solely to remove the warning.
- Minor ruling: no additional token-splitting unit contract was added. Fresh production checks reconstructed all 225 signature blocks, verified 683 valid links, retained TextMate colors/styles in the browser, and kept 15 ordinary examples link-free; closeout produced no evidence that a separate byte-level LF/CRLF fixture is blocking.
