# Jazz Docusaurus Website Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Ship a distinctive, accessible Docusaurus site that publishes only curated Jazz documentation and deploys repeatably to GitHub Pages.

**Architecture:** `website/` owns the Docusaurus TypeScript application, theme, local assets, and production build. The docs plugin reads the repository-root `docs/` tree through `path: "../docs"`; `rfcs/` and `.codex/` never enter the content pipeline. The repository is trusted source: a small regression guard checks canonical configuration, obvious authored remote references, and generated resource contexts without attempting to interpret arbitrary TypeScript, JSX, MDX, or YAML. A custom full-bleed home page and restrained Infima overrides provide the visual identity, while a dedicated GitHub Pages workflow builds and deploys immutable static output.

**Tech Stack:** Docusaurus 3 Classic, React, TypeScript, CSS Modules, Prism, npm lockfile, Node.js 22, GitHub Actions, GitHub Pages

## Global Constraints

- Execute on `codex/docusaurus-website`, based on the merged documentation-reset workstream.
- Scaffold the current Docusaurus 3 Classic TypeScript template, then commit the exact resolved versions in `website/package-lock.json`. Do not leave `latest` ranges in the committed manifest.
- Docusaurus may read only `docs/`. Do not copy, symlink, import, transform, or publish `rfcs/` or `.codex/`.
- Public documentation inputs under root `docs/` must be plain `.md` files. `.mdx` is forbidden there; the current public documentation has no required MDX content. Interactive React and TypeScript remain authored only inside `website/`.
- Do not add a blog, documentation versioning, analytics, search service, playground, user accounts, or remote runtime content.
- Use local or bundled assets and fonts. A production page must render without fetching Google Fonts, CDNs, remote images, or runtime APIs.
- Use no more than two typefaces and one dominant brass accent. Avoid card grids, decorative gradients, floating dashboards, pill clutter, and a stock Docusaurus homepage.
- Keep the published docs pipeline in plain-Markdown mode: configure `markdown.format` as `md`, publish only `.md` files, and do not permit front matter to enable or auto-detect MDX.
- Keep runtime resources local. The boundary guard scans relevant authored site files for obvious `scheme://` and protocol-relative references, allowing only the named GitHub navigation targets and the production site origin/base route. This is a maintainable regression check for reviewed repository code, not a proof against URLs assembled by arbitrary expressions; normal local JSX expressions, imports, and spreads remain permitted.
- Author plain CSS only. `.scss`, `.sass`, and `.less` sources are outside the website profile, and the post-build boundary check scans generated HTML and CSS resource contexts for remote loads without treating arbitrary dependency JavaScript strings as resources.
- Motion must respect `prefers-reduced-motion` and cannot delay navigation or content access.
- Build and structural checks replace browser automation in this workstream. Record a short human visual-review checklist in the PR for desktop, mobile, light, and dark modes.
- Commit the scaffold/boundary, brand system, homepage/docs theme, and deployment workflow separately.

## Visual Direction

**Visual thesis:** Jazz should feel like a precise editorial score printed in warm brass and deep ink, anchored by one oversized saxophone-shaped `J` rather than generic developer-tool chrome.

**Content plan:**

1. Full-bleed hero: Jazz wordmark, one-sentence promise, primary `Get started` action, secondary `Language guide` action, and the saxophone-`J` motif.
2. Proof section: one real checked Jazz program beside its result, with no surrounding card.
3. Depth section: three horizontal editorial bands for static typing, functional composition, and the self-hosting path.
4. Final call to action: build the compiler or read current project status.

**Interaction thesis:**

- the hero wordmark, copy, and brass motif enter as one short staggered sequence;
- thin score-line accents shift subtly with scroll using CSS-only transforms; and
- code-copy, navigation, and text-link states use a fast underline/reveal transition.

All three interactions become static when reduced motion is requested.

---

## Task 1: Add a failing website content-boundary contract

**Files:**

- Create: `scripts/check-website-boundary.py`
- Create: `scripts/test-check-website-boundary.py`
- Create: `scripts/check-website.sh`

- [x] Write focused fixture tests that require:

  - `website/docusaurus.config.ts` to set the docs path to exactly `../docs`;
  - the blog plugin to be disabled;
  - no config or authored site source to reference `.codex`, `rfcs`, legacy documentation paths, or legacy compiler identities;
  - broken links and broken Markdown links to fail the production build;
  - `website/build/` output, when present, to contain none of the internal-path or legacy-identity strings;
  - obvious remote references in relevant authored site files to be rejected unless they are exact approved GitHub or production navigation URLs;
  - generated HTML resource attributes and generated CSS `url()`/`@import` targets to stay local, apart from Docusaurus production metadata and approved navigation;
  - public documentation to remain `.md`, with symlinks unable to escape `docs/`; and
  - production configuration to use `https://un3qual.github.io` with base URL `/jazz/`.

- [x] Run the tests before implementing the checker:

  ```bash
  python3 -m unittest scripts/test-check-website-boundary.py
  ```

  Expected: failures identify missing configuration validation.

- [x] Implement the checker with standard-library Python only, deterministic sorted violations, and an optional fixture-root argument. Keep it concise and explicit: use exact configuration assertions, `HTMLParser` for generated HTML resource attributes, and simple CSS `url()`/`@import` matching. Do not build a JavaScript, MDX, YAML, or arbitrary-expression interpreter.

- [x] Implement `scripts/check-website.sh` as a strict Bash entrypoint that runs the Python boundary check, `npm run typecheck`, `npm run build`, and a second boundary check against generated output.

- [x] Re-run unit tests:

  ```bash
  python3 -m unittest scripts/test-check-website-boundary.py
  ```

  Expected: all fixture tests pass. The full wrapper remains red until the site exists.

## Task 2: Scaffold the Docusaurus application and pin the toolchain

**Files:**

- Create: `website/package.json`
- Create: `website/package-lock.json`
- Create: `website/.nvmrc`
- Create: `website/tsconfig.json`
- Create: `website/docusaurus.config.ts`
- Create: `website/sidebars.ts`
- Create: `website/src/css/custom.css`
- Create: `website/src/pages/index.tsx`
- Create: `website/src/pages/index.module.css`
- Create: `website/scripts/sync-factorial.mjs`
- Modify: `flake.nix`
- Modify: `.gitignore`

- [x] Create the TypeScript Classic scaffold under a temporary directory with npm, inspect it, then move only the needed application files into `website/`. Do not retain template blog pages, tutorial docs, or sample assets:

  ```bash
  npm create docusaurus@latest /tmp/jazz-docusaurus classic -- --typescript --package-manager npm --skip-install
  ```

- [x] Set `website/.nvmrc` to `22`. In `website/package.json`, set `private: true`, `engines.node` to `>=22 <23`, and scripts:

  ```json
  {
    "prestart": "node scripts/sync-factorial.mjs",
    "start": "docusaurus start",
    "prebuild": "node scripts/sync-factorial.mjs",
    "build": "docusaurus build",
    "serve": "docusaurus serve",
    "typecheck": "tsc --noEmit",
    "clear": "docusaurus clear",
    "render:brand": "node scripts/render-social-card.mjs",
    "check": "npm run typecheck && npm run build"
  }
  ```

- [x] Add `@fontsource-variable/manrope` and `@fontsource/ibm-plex-mono` as bundled dependencies, and `sharp` as a development dependency for deterministic social-card rendering. Use Manrope for interface/editorial text and IBM Plex Mono for code only.

- [x] Install from `website/` and commit the generated lockfile:

  ```bash
  cd website
  npm install
  npm run typecheck
  ```

  Expected: dependency resolution and TypeScript checks succeed. Replace any permissive template version range with the exact installed Docusaurus/React package versions while retaining `package-lock.json` integrity.

- [x] Configure the classic preset in `docusaurus.config.ts`:

  - `title: "Jazz"`;
  - `tagline: "A statically typed functional language with practical syntax"`;
  - `url: "https://un3qual.github.io"`;
  - `baseUrl: "/jazz/"`;
  - `organizationName: "un3qual"` and `projectName: "jazz"`;
  - `trailingSlash: false`;
  - `onBrokenLinks: "throw"` and broken Markdown links treated as errors;
  - docs `path: "../docs"`, `routeBasePath: "docs"`, and `sidebarPath` pointing to `sidebars.ts`;
  - blog disabled;
  - navbar links for Docs, Language, Standard Library, Status, and GitHub; and
  - footer links for Getting Started, Reference, Roadmap, Contributing, GitHub, issues, security, and license.

- [x] Define one explicit `sidebars.ts` tree matching the six public documentation sections. Do not use autogenerated discovery for top-level ordering.

- [x] Add Node.js 22 and npm to the Nix development shell while retaining the compiler toolchain. Add `website/node_modules/`, `website/build/`, and `website/.docusaurus/` to `.gitignore`.

- [x] Run the boundary and clean-build checks:

  ```bash
  python3 scripts/check-website-boundary.py
  npm --prefix website ci
  npm --prefix website run typecheck
  npm --prefix website run build
  ```

  Expected: Docusaurus builds every root-docs page and the boundary check finds no internal content sources.

- [x] Commit the scaffold and enforced content boundary:

  ```bash
  git add -A
  git commit -m "website: scaffold the Jazz documentation site"
  ```

## Task 3: Build the local brand asset system

**Files:**

- Create: `website/static/img/jazz-mark.svg`
- Create: `website/static/img/jazz-mark-dark.svg`
- Create: `website/static/img/jazz-wordmark.svg`
- Create: `website/static/img/jazz-wordmark-dark.svg`
- Create: `website/static/img/favicon.svg`
- Create: `website/static/img/social-card.svg`
- Create: `website/static/img/social-card.png`
- Create: `website/static/img/brand/README.md`
- Create: `website/scripts/render-social-card.mjs`
- Modify: `README.md`
- Delete: `jazz_logo.png`

- [x] Use the existing cartoon saxophone logo only as visual reference. Redraw it as a flat, original SVG system with:

  - an unmistakable saxophone silhouette that also reads as `J`;
  - deep ink outlines/surfaces;
  - one warm brass fill/accent;
  - no embedded raster data, gradients, text converted from an unlicensed font, or fine detail that disappears at favicon size; and
  - separate light/dark contrast variants rather than CSS filters.

- [x] Compose wordmark variants from the mark plus live or outlined `Jazz` lettering. Keep the mark independently usable in the navbar and favicon.

- [x] Create a 1200×630 social-preview composition in SVG with large Jazz branding, the one-line language promise, ample quiet space, and no screenshots or UI frames. Implement `website/scripts/render-social-card.mjs` with `sharp`, requiring exactly 1200×630 output, and record `npm --prefix website run render:brand` as the regeneration command in `website/static/img/brand/README.md`.

- [x] Add meaningful `<title>`/`<desc>` to informative SVGs and mark decorative instances with empty alternative text in React. Ensure SVG IDs are unique and no external references exist.

- [x] Point the root README at `website/static/img/jazz-wordmark.svg`, then delete the old root `jazz_logo.png` after confirming no references remain.

- [x] Validate assets:

  ```bash
  rg -n "https?://|data:image" website/static/img --glob '*.svg'
  rg -n "jazz_logo\.png" .
  file website/static/img/social-card.png
  ```

  Expected: SVGs have no remote or embedded-raster dependencies, no old logo references remain, and the social card is a 1200×630 PNG.

- [x] Commit brand assets independently:

  ```bash
  git add -A
  git commit -m "website: add the Jazz visual identity"
  ```

## Task 4: Implement the custom full-bleed homepage

**Files:**

- Create: `website/src/components/BrandMark.tsx`
- Create: `website/src/components/CodeProof.tsx`
- Create: `website/src/components/EditorialBand.tsx`
- Create: `website/src/components/HomepageHeader.tsx`
- Create: `website/src/components/HomepageFooterCta.tsx`
- Create: `website/src/generated/factorial.ts`
- Rewrite: `website/src/pages/index.tsx`
- Rewrite: `website/src/pages/index.module.css`
- Modify: `website/src/css/custom.css`

- [x] Implement `index.tsx` as four semantic sections matching the approved content plan. Use one `<h1>`, logical heading order, landmark elements, and Docusaurus `Link` components for internal navigation.

- [x] Make the hero edge-to-edge with no inherited container or maximum-width frame. Constrain only the text/action column; use the oversized saxophone-`J` as the dominant visual plane. The header plus hero content must fit within a common mobile viewport without hiding the actions.

- [x] Implement `website/scripts/sync-factorial.mjs` to read `examples/functions/factorial.jz` and emit `website/src/generated/factorial.ts` as one escaped exported string. Run it through `prestart` and `prebuild`. Render that export in `CodeProof.tsx`, and make `scripts/check-website-boundary.py` fail when the generated string is not byte-for-byte equal to the source example.

- [x] Implement the three editorial bands without card containers:

  - `Types that stay readable` links to types and signatures;
  - `Composition without ceremony` links to bindings/functions and patterns; and
  - `A compiler growing into Jazz` links to compiler and bootstrapping docs.

  Each band gets one concise paragraph and one text link.

- [x] Add primary `Get started` and secondary `Read the language guide` actions in the hero. The final CTA links to build instructions and current status.

- [x] Implement the three motion ideas with CSS animations/transforms only. Keep the hero entrance below 500 ms, hover transitions below 180 ms, and all animated distances below 24 px. Disable animation and scroll transforms inside `@media (prefers-reduced-motion: reduce)`.

- [x] Add responsive layouts at content-driven breakpoints, visible focus states, minimum 44×44 px action targets, and AA contrast for normal text in both themes.

- [x] Run structural checks:

  ```bash
  npm --prefix website run typecheck
  npm --prefix website run build
  python3 scripts/check-website-boundary.py
  rg -n "card|gradient|https?://" website/src/pages website/src/components website/src/css
  ```

  Expected: build and boundary checks pass; any `card`, `gradient`, or remote URL match is reviewed and removed unless it is an intentional GitHub navigation URL.

## Task 5: Customize documentation, syntax highlighting, and accessibility

**Files:**

- Create: `website/src/theme/prism-include-languages.ts`
- Create: `website/src/theme/DocItem/Layout/index.tsx`
- Create: `website/src/theme/DocItem/Layout/styles.module.css`
- Create: `website/src/theme/Navbar/Logo/index.tsx`
- Modify: `website/src/css/custom.css`
- Modify: `website/docusaurus.config.ts`
- Modify: `website/sidebars.ts`

- [x] Define a Prism `jazz` grammar covering implemented comments, strings/chars, numeric literals, `module`, `import`, `export`, `data`, `case`, `if`, `then`, `else`, operators, capability declarations, type signatures, constructors, and bang-suffixed identifiers. Extend the original Docusaurus language loader rather than replacing existing languages.

- [x] Configure Jazz Markdown fences to use the custom grammar and set light/dark code themes with strong contrast. Keep line highlighting and copy affordances legible without thick borders or shadow-heavy containers.

- [x] Customize doc layout spacing, table overflow, heading anchors, breadcrumbs, pagination, admonitions, code blocks, and mobile sidebar. Preserve Docusaurus semantics and keyboard behavior; do not fork components that only need CSS.

- [x] Replace the stock navbar logo component only as needed to select the correct local light/dark mark. Ensure the text `Jazz` remains available to assistive technology.

- [x] Add metadata defaults, favicon, Open Graph image, theme color, and descriptive page titles in `docusaurus.config.ts`.

- [x] Add a skip-to-content link if the selected Docusaurus template does not already provide one. Confirm all icon-only controls have accessible names and all decorative SVGs are hidden from assistive technology.

- [x] Run the full local website check:

  ```bash
  bash scripts/check-website.sh
  ```

  Expected: typecheck, production build, link handling, content boundary, and generated-output scans pass.

- [x] Inspect representative generated pages without a browser tool:

  ```bash
  test -f website/build/index.html
  test -f website/build/docs/getting-started/overview.html
  test -f website/build/docs/language/types-and-signatures.html
  test -f website/build/docs/project/status.html
  rg -n "Jazz|Get started|A statically typed functional language" website/build/index.html
  ```

  Expected: the homepage and representative docs routes exist and contain their primary content.

- [x] Commit homepage and documentation theme work:

  ```bash
  git add -A
  git commit -m "website: create the Jazz documentation experience"
  ```

## Task 6: Add GitHub Pages deployment

**Files:**

- Create: `.github/workflows/docs-pages.yml`
- Modify: `README.md`
- Modify: `docs/getting-started/overview.md`

- [x] Add a Pages workflow triggered by pushes to `main` affecting `docs/**`, `website/**`, `README.md`, or the workflow itself, plus `workflow_dispatch`.

- [x] Configure least-privilege permissions:

  ```yaml
  permissions:
    contents: read
    pages: write
    id-token: write
  ```

  Add `concurrency.group: pages` and `cancel-in-progress: true`.

- [x] The build job must use Ubuntu, `actions/checkout@v4`, `actions/setup-node@v4` with Node 22 and npm cache keyed by `website/package-lock.json`, `npm ci`, `npm run typecheck`, `npm run build`, the generated-output boundary check, `actions/configure-pages@v5`, and `actions/upload-pages-artifact@v3` with path `website/build`.

- [x] The deploy job must use the `github-pages` environment and `actions/deploy-pages@v4`, exposing the returned `page_url` as the environment URL.

- [x] Activate the already-authored README and Getting Started link to `https://un3qual.github.io/jazz/` once GitHub Pages is enabled for GitHub Actions in repository settings; remove the temporary `publishing with Workstream 3` label at that point.

- [x] Validate workflow syntax structurally and run the same build locally:

  ```bash
  rg -n "pull_request|cabal bench|full-parser-scale|profil" .github/workflows/docs-pages.yml
  npm --prefix website ci
  bash scripts/check-website.sh
  git diff --check
  ```

  Expected: the workflow contains no compiler/performance jobs, and the local production artifact passes all checks.

- [x] Commit deployment separately:

  ```bash
  git add -A
  git commit -m "ci: deploy Jazz documentation to GitHub Pages"
  ```

## Task 7: Final website review and publication

**Files:**

- Modify only if review findings require it: files already in scope

- [x] Run all documentation and site checks from a clean dependency install:

  ```bash
  npm --prefix website ci
  npm --prefix website run clear
  bash scripts/check-public-docs.sh
  bash scripts/check-website.sh
  bash scripts/check-docs.sh
  git diff --check
  ```

  Expected: a clean install reproduces the production site and no internal content leaks into output. The destructive cleanup targets only generated, ignored website directories.

- [x] Review the first-viewport source against the visual thesis:

  - Jazz is the loudest text and the saxophone-`J` is the dominant visual;
  - header plus hero fits common desktop and mobile viewport budgets;
  - there is one primary action and one secondary action;
  - no hero card, stat strip, logo cloud, card mosaic, or decorative gradient appears;
  - page headings alone communicate the full story; and
  - light/dark and reduced-motion behavior are intentional.

- [x] Add a human review checklist to the pull-request description for 1440 px desktop, 390 px mobile, light mode, dark mode, keyboard navigation, and reduced motion. Do not mark those visual items complete without maintainer observation.

- [x] Review branch scope:

  ```bash
  git diff --stat origin/main...HEAD
  git log --oneline origin/main..HEAD
  git status --short
  ```

  Expected: only the website, branding, site checks, Pages workflow, and direct website-link updates are present.

- [x] Push `codex/docusaurus-website` and open a dedicated pull request. Include the production build result, content-boundary result, final Pages URL, and the human visual-review checklist.
