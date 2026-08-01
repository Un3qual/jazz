# Jazz Docs pnpm and TextMate Highlighting Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Migrate the Jazz documentation site to pnpm and render every Jazz code block from the repository's existing TextMate grammar.

**Architecture:** `editors/vscode-jazz/syntaxes/jazz.tmLanguage.json` remains the only authored Jazz grammar. A small Shiki adapter loads it directly, while a focused Docusaurus `CodeBlock/Content` wrapper delegates non-Jazz blocks to the original Prism renderer and renders Jazz tokens with Shiki. Website installation, scripts, CI, Pages, and release verification use one pinned pnpm toolchain and `website/pnpm-lock.yaml`.

**Tech Stack:** pnpm 11.18.0, Node.js 22, Docusaurus 3.10.2, React 19, TypeScript 6, Shiki, TextMate JSON, Node test runner, Python `unittest`, GitHub Actions

## Global Constraints

- `editors/vscode-jazz/syntaxes/jazz.tmLanguage.json` is the sole authored Jazz highlighting grammar.
- Do not maintain or generate a second Prism grammar for Jazz.
- Use Shiki only for normalized `jazz` blocks; preserve Docusaurus Prism behavior for every other language.
- Preserve code titles, copy and wrap controls, line numbering, highlighted lines, light and dark modes, and server-rendered markup.
- Use pnpm 11.18.0 with a frozen `website/pnpm-lock.yaml` in automation.
- Do not add GHC, Cabal, compiler tests, benchmarks, or performance suites to documentation CI.
- Keep this plan and all other `.codex/` content outside the published docs pipeline.

---

## File structure

- `website/scripts/jazz-highlighter.mjs`: loads the repository TextMate grammar into Shiki and exposes deterministic Jazz tokenization.
- `website/scripts/jazz-highlighter.d.mts`: describes the JavaScript module to TypeScript consumers.
- `website/src/theme/CodeBlock/Content/index.tsx`: chooses Shiki for Jazz and delegates all other languages to Docusaurus's original content renderer.
- `website/src/theme/CodeBlock/Content/styles.module.css`: contains only the pre/code layout styles required by the Jazz branch.
- `website/scripts/check-built-highlighting.mjs`: verifies that the production build contains server-rendered TextMate-highlighted Jazz markup.
- `website/scripts/test-experience.mjs`: owns tokenizer and renderer regression tests.
- `scripts/check-docs-pages-workflow.py` and `scripts/check-ci-policy.py`: enforce pnpm in the corresponding workflows.
- `scripts/test-docs-pages-workflow.py` and `scripts/test-check-ci-policy.py`: fixture tests for those policy contracts.

### Task 1: Migrate the website toolchain and automation to pnpm

**Files:**
- Modify: `website/package.json`
- Delete: `website/package-lock.json`
- Create: `website/pnpm-lock.yaml`
- Modify: `scripts/check-website.sh`
- Modify: `scripts/ci/release-candidate.sh`
- Modify: `website/static/img/brand/README.md`
- Modify: `flake.nix`
- Modify: `.github/workflows/ci-pr.yml`
- Modify: `.github/workflows/docs-pages.yml`
- Modify: `.github/workflows/release.yml`
- Modify: `scripts/check-docs-pages-workflow.py`
- Modify: `scripts/check-ci-policy.py`
- Test: `scripts/test-docs-pages-workflow.py`
- Test: `scripts/test-check-ci-policy.py`

**Interfaces:**
- Consumes: the existing website scripts and Node.js 22 engine constraint.
- Produces: `packageManager: "pnpm@11.18.0"`, `website/pnpm-lock.yaml`, and automation that installs with `pnpm install --frozen-lockfile`.

- [ ] **Step 1: Change the workflow fixtures to require pnpm**

  In both Python test fixtures, require `pnpm/action-setup@v4` with `version: 11.18.0`, `actions/setup-node@v4` with `cache: pnpm` and `cache-dependency-path: website/pnpm-lock.yaml`, then use:

  ```yaml
  - name: Install website dependencies
    run: pnpm install --frozen-lockfile
    working-directory: website
  ```

  Replace all website `npm run <script>` fixture commands with `pnpm run <script>`. Rename assertions so their messages require a pnpm cache, pnpm lockfile, and frozen install.

- [ ] **Step 2: Run the policy tests and verify RED**

  Run:

  ```bash
  python3 scripts/test-docs-pages-workflow.py
  python3 scripts/test-check-ci-policy.py
  ```

  Expected: FAIL because the production validators and checked-in workflows still require npm and `website/package-lock.json`.

- [ ] **Step 3: Update the validators and workflows minimally**

  Update the two production validators to require the same pinned pnpm action, cache settings, frozen install, and ordered pnpm commands as the fixtures. Update all three workflows with this installation sequence:

  ```yaml
  - name: Set up pnpm
    uses: pnpm/action-setup@v4
    with:
      version: 11.18.0

  - name: Set up Node.js
    uses: actions/setup-node@v4
    with:
      node-version: 22
      cache: pnpm
      cache-dependency-path: website/pnpm-lock.yaml
  ```

  Preserve the current docs-only workload and all existing workflow permissions, triggers, gates, and deployment steps.

- [ ] **Step 4: Update first-party commands and package metadata**

  Add this top-level field to `website/package.json`:

  ```json
  "packageManager": "pnpm@11.18.0"
  ```

  Change the aggregate script to:

  ```json
  "check": "pnpm run typecheck && pnpm run build"
  ```

  Replace root-level website invocations with `pnpm --dir "$ROOT/website" run <script>` in shell scripts and `pnpm --dir website ...` in documentation. Add pnpm to the Nix development shell so release-candidate verification can resolve it without a network bootstrap.

- [ ] **Step 5: Generate the pnpm lockfile and remove the npm lockfile**

  Run from the repository root:

  ```bash
  pnpm --dir website install --lockfile-only
  git rm website/package-lock.json
  ```

  Expected: `website/pnpm-lock.yaml` is created and resolves the exact manifest; the npm lockfile is staged for removal.

- [ ] **Step 6: Run the focused pnpm policy checks and verify GREEN**

  Run:

  ```bash
  pnpm --dir website install --frozen-lockfile
  python3 scripts/test-docs-pages-workflow.py
  python3 scripts/check-docs-pages-workflow.py
  python3 scripts/test-check-ci-policy.py
  python3 scripts/check-ci-policy.py
  ```

  Expected: all fixture suites and live workflow validators pass.

- [ ] **Step 7: Commit the pnpm migration**

  ```bash
  git add website/package.json website/pnpm-lock.yaml scripts/check-website.sh scripts/ci/release-candidate.sh website/static/img/brand/README.md flake.nix .github/workflows/ci-pr.yml .github/workflows/docs-pages.yml .github/workflows/release.yml scripts/check-docs-pages-workflow.py scripts/check-ci-policy.py scripts/test-docs-pages-workflow.py scripts/test-check-ci-policy.py
  git commit -m "build: migrate documentation site to pnpm"
  ```

### Task 2: Load the repository TextMate grammar with Shiki

**Files:**
- Create: `website/scripts/jazz-highlighter.mjs`
- Create: `website/scripts/jazz-highlighter.d.mts`
- Modify: `website/scripts/test-experience.mjs`
- Modify: `website/package.json`
- Modify: `website/pnpm-lock.yaml`
- Test: `website/scripts/test-experience.mjs`
- Fixture: `editors/vscode-jazz/fixtures/representative.jz`
- Grammar: `editors/vscode-jazz/syntaxes/jazz.tmLanguage.json`

**Interfaces:**
- Consumes: the checked-in TextMate grammar and Shiki's synchronous core with the JavaScript regex engine.
- Produces: `tokenizeJazz(code, colorMode, options?)`, returning `{tokens, fg, bg}` with `colorMode` equal to `light` or `dark`; `options.includeExplanation` is forwarded for scope-level regression assertions.

- [ ] **Step 1: Add a failing TextMate scope regression**

  Extend `website/scripts/test-experience.mjs` to import `tokenizeJazz`, load the existing representative fixture, tokenize it with `{includeExplanation: true}`, and collect every `scopeName` from token explanations. Assert that the set includes these authored grammar scopes:

  ```js
  for (const scope of [
    'comment.line.number-sign.jazz',
    'keyword.declaration.jazz',
    'entity.name.type.jazz',
    'entity.name.function.constructor.jazz',
    'string.quoted.double.jazz',
    'string.quoted.single.jazz',
    'constant.numeric.jazz',
    'keyword.operator.jazz',
    'keyword.operator.signature.jazz',
    'entity.name.function.effectful.jazz',
  ]) {
    assert.ok(scopes.has(scope), `missing TextMate scope: ${scope}`);
  }
  ```

  Also assert that the highlighter module imports `../../editors/vscode-jazz/syntaxes/jazz.tmLanguage.json` and contains no copied `patterns` or `repository` grammar object.

- [ ] **Step 2: Run the experience test and verify RED**

  Run:

  ```bash
  pnpm --dir website run test:experience
  ```

  Expected: FAIL with `ERR_MODULE_NOT_FOUND` for `scripts/jazz-highlighter.mjs`.

- [ ] **Step 3: Add Shiki and implement the smallest grammar adapter**

  Install Shiki as an exact website dependency through pnpm. Implement a module-level synchronous highlighter using `createHighlighterCoreSync`, `createJavaScriptRegexEngine`, the `github-light` and `dracula` themes, and a shallow metadata adaptation of the imported grammar:

  ```js
  const jazzLanguage = {
    ...jazzTextMateGrammar,
    name: 'jazz',
    aliases: ['Jazz'],
  };

  const highlighter = createHighlighterCoreSync({
    engine: createJavaScriptRegexEngine(),
    langs: [jazzLanguage],
    themes: [githubLight, dracula],
  });

  export function tokenizeJazz(code, colorMode, options = {}) {
    return highlighter.codeToTokens(code, {
      lang: 'jazz',
      theme: colorMode === 'dark' ? 'dracula' : 'github-light',
      includeExplanation: options.includeExplanation ?? false,
    });
  }
  ```

  Type the declaration with Shiki's `CodeToTokensResult` and a literal `JazzColorMode = 'light' | 'dark'`. Do not expose grammar-writing APIs.

- [ ] **Step 4: Run the tokenizer regression and verify GREEN**

  Run:

  ```bash
  pnpm --dir website run test:experience
  pnpm --dir website run typecheck
  ```

  Expected: the representative fixture exposes every required TextMate scope and TypeScript accepts the JavaScript module interface.

- [ ] **Step 5: Commit the TextMate adapter**

  ```bash
  git add website/package.json website/pnpm-lock.yaml website/scripts/jazz-highlighter.mjs website/scripts/jazz-highlighter.d.mts website/scripts/test-experience.mjs
  git commit -m "feat: load Jazz TextMate grammar with Shiki"
  ```

### Task 3: Render Jazz blocks through Shiki and remove the Prism duplicate

**Files:**
- Create: `website/src/theme/CodeBlock/Content/index.tsx`
- Create: `website/src/theme/CodeBlock/Content/styles.module.css`
- Create: `website/scripts/check-built-highlighting.mjs`
- Modify: `website/scripts/test-experience.mjs`
- Modify: `website/package.json`
- Modify: `website/docusaurus.config.ts`
- Delete: `website/scripts/prism-jazz-grammar.mjs`
- Delete: `website/scripts/prism-jazz-grammar.d.mts`
- Delete: `website/src/theme/prism-include-languages.ts`
- Test: `website/scripts/test-experience.mjs`

**Interfaces:**
- Consumes: `tokenizeJazz(code, colorMode)` from Task 2 and Docusaurus `CodeBlockMetadata` from `useCodeBlockContext()`.
- Produces: a `CodeBlock/Content` theme wrapper that sets `data-jazz-highlighter="textmate"` only for Jazz blocks and delegates every other language to `@theme-original/CodeBlock/Content`.

- [ ] **Step 1: Add failing renderer and build-contract tests**

  Replace the old Prism-registration tests with assertions that:

  ```js
  assert.match(renderer, /language\s*!==\s*['"]jazz['"]/);
  assert.match(renderer, /@theme-original\/CodeBlock\/Content/);
  assert.match(renderer, /tokenizeJazz/);
  assert.match(renderer, /data-jazz-highlighter=['"]textmate['"]/);
  assert.doesNotMatch(config, /additionalLanguages:\s*\[['"]jazz['"]\]/);
  ```

  Assert the three standalone Prism files no longer exist, and assert `website/package.json` defines:

  ```json
  "postbuild": "node scripts/check-built-highlighting.mjs"
  ```

- [ ] **Step 2: Run the experience test and verify RED**

  Run:

  ```bash
  pnpm --dir website run test:experience
  ```

  Expected: FAIL because the renderer and postbuild checker do not exist and the Jazz Prism registration remains.

- [ ] **Step 3: Implement the Jazz renderer**

  Copy only Docusaurus's pre/code structural styles into the local CSS module. In the wrapper, return `OriginalCodeBlockContent` unless `metadata.language === 'jazz'`. For Jazz, use `useColorMode()` and `tokenizeJazz()` and render token lines while preserving:

  ```tsx
  <pre
    ref={wordWrap.codeBlockRef}
    tabIndex={0}
    data-jazz-highlighter="textmate"
    className={clsx(classNameProp, `language-${language}`, styles.codeBlock)}>
    <code
      className={clsx(
        styles.codeBlockLines,
        lineNumbersStart !== undefined && styles.codeBlockLinesWithNumbering,
      )}
      style={{
        counterReset:
          lineNumbersStart === undefined
            ? undefined
            : `line-count ${lineNumbersStart - 1}`,
      }}>
      {/* Shiki token lines rendered through @theme/CodeBlock/Line */}
    </code>
  </pre>
  ```

  Adapt Shiki's `content`, `color`, and `fontStyle` into `getTokenProps`; pass `lineClassNames[index]` and `showLineNumbers` to Docusaurus's existing line renderer. Map Shiki font-style flags to CSS italic, bold, and underline styles. Do not alter the shared layout, title, or button components.

- [ ] **Step 4: Remove the duplicate Prism integration**

  Remove the `additionalLanguages: ['jazz']` setting and delete the three Jazz Prism files. Do not remove `prism-react-renderer`, because Docusaurus and non-Jazz code blocks still depend on it.

- [ ] **Step 5: Add the production-build assertion**

  Implement `check-built-highlighting.mjs` to scan generated HTML files and require:

  ```js
  const jazzBlock = /<pre[^>]*data-jazz-highlighter="textmate"[\s\S]*?<\/pre>/g;
  ```

  Require at least one Jazz block and at least four distinct inline token colors inside the matched blocks. Exit nonzero with a direct message if the build contains no TextMate marker or visibly tokenized Jazz output.

- [ ] **Step 6: Run the renderer tests and production build and verify GREEN**

  Run:

  ```bash
  pnpm --dir website run test:experience
  pnpm --dir website run typecheck
  pnpm --dir website run clear
  pnpm --dir website run build
  ```

  Expected: all tests pass; the postbuild checker finds server-rendered Jazz blocks with at least four token colors.

- [ ] **Step 7: Commit the renderer replacement**

  ```bash
  git add website/src/theme/CodeBlock/Content website/scripts/check-built-highlighting.mjs website/scripts/test-experience.mjs website/package.json website/docusaurus.config.ts website/scripts/prism-jazz-grammar.mjs website/scripts/prism-jazz-grammar.d.mts website/src/theme/prism-include-languages.ts
  git commit -m "feat: render Jazz docs from TextMate grammar"
  ```

### Task 4: Run the complete ordinary documentation gate

**Files:**
- Verify: all files changed by Tasks 1-3

**Interfaces:**
- Consumes: the pinned pnpm lockfile, pnpm workflows, TextMate adapter, and Docusaurus renderer.
- Produces: evidence that the ordinary docs pipeline passes without the extended compiler/performance tier.

- [ ] **Step 1: Reinstall exactly from the lockfile**

  ```bash
  pnpm --dir website install --frozen-lockfile
  ```

  Expected: no manifest or lockfile changes.

- [ ] **Step 2: Run focused unit and workflow tests**

  ```bash
  pnpm --dir website run test:brand
  pnpm --dir website run test:experience
  python3 scripts/test-docs-pages-workflow.py
  python3 scripts/test-check-ci-policy.py
  python3 scripts/test-check-website-boundary.py
  ```

  Expected: every suite passes.

- [ ] **Step 3: Run the ordinary docs and site checks**

  ```bash
  bash scripts/check-public-docs.sh
  bash scripts/check-docs.sh
  bash scripts/check-spec-authority.sh
  bash scripts/check-website.sh
  python3 scripts/check-ci-policy.py
  ```

  Expected: public boundary, docs, RFC authority, TypeScript, production build, generated publication boundary, and CI workload policy all pass. Do not run `scripts/ci/extended.sh` or any benchmark.

- [ ] **Step 4: Inspect the final repository state**

  ```bash
  git status --short
  git diff --check HEAD~4..HEAD
  git log -5 --oneline
  ```

  Expected: no uncommitted files, no whitespace errors, and separate commits for the design, implementation plan, pnpm migration, grammar adapter, and renderer.
