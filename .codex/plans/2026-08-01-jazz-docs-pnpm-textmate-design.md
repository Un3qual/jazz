# Jazz documentation pnpm and TextMate highlighting design

**Date:** 2026-08-01

**Status:** Approved for implementation

## Purpose

Make the documentation site use pnpm consistently and render Jazz code with the same checked-in syntax grammar as the VS Code extension. The change must improve visible highlighting without creating a second language definition or adding compiler and performance work to the documentation pipeline.

## Source of truth

`editors/vscode-jazz/syntaxes/jazz.tmLanguage.json` is the sole authored Jazz syntax-highlighting grammar. The VS Code extension continues to load it directly. The website loads the same file through a TextMate-compatible highlighter and does not maintain an independent Prism grammar.

## Website rendering

The website will use Shiki for code blocks whose normalized language is `jazz`. Shiki will register the repository TextMate grammar under the lowercase `jazz` language identifier and tokenize it with a JavaScript regular-expression engine compatible with the grammar's current patterns.

A focused Docusaurus theme override at `website/src/theme/CodeBlock/Content/` will select the renderer:

- Jazz blocks use Shiki tokens derived from the repository grammar.
- All other languages continue through Docusaurus's existing Prism renderer.

The Jazz path will preserve the surrounding Docusaurus code-block contract: titles, copy and word-wrap controls, line numbering, highlighted-line metadata, focus behavior, light and dark color modes, and server-rendered output. It applies equally to fenced documentation examples and the homepage `CodeProof` component.

The existing website-only Prism registration files will be removed:

- `website/scripts/prism-jazz-grammar.mjs`
- `website/scripts/prism-jazz-grammar.d.mts`
- `website/src/theme/prism-include-languages.ts`

The Docusaurus Prism configuration will no longer register `jazz` as an additional Prism language.

## pnpm migration

`website/package.json` will declare the exact pnpm version used to create the lockfile. `website/package-lock.json` will be replaced by `website/pnpm-lock.yaml`, and internal package scripts will invoke other scripts through pnpm.

Every first-party website command will use pnpm consistently, including:

- local development and brand instructions;
- `scripts/check-website.sh`;
- release-candidate website verification;
- fast pull-request CI;
- documentation Pages build and deployment;
- release workflow caching;
- CI-policy and Pages-workflow validators and their tests.

GitHub Actions will install the pinned pnpm version and cache pnpm's store using `website/pnpm-lock.yaml`. Installs will use `pnpm install --frozen-lockfile`. The documentation workflow remains isolated from GHC, Cabal, compiler tests, benchmarks, and performance suites.

Dependabot remains configured with the `npm` package ecosystem because GitHub uses that ecosystem name for pnpm manifests and lockfiles.

## Verification

Tests will establish the behavior before the implementation changes:

1. The website experience test will require the repository TextMate grammar integration and reject the deleted standalone Prism path.
2. A tokenizer regression will exercise `editors/vscode-jazz/fixtures/representative.jz` and verify representative scopes for comments, declarations, types, constructors, strings, characters, numbers, operators, signatures, and effectful identifiers.
3. The production website build will be checked for server-rendered, tokenized Jazz markup from a public example.
4. Workflow-policy fixture tests will require the pinned pnpm setup, frozen-lockfile installs, pnpm cache, and pnpm commands.

Final verification will run the focused website experience and workflow-policy tests, TypeScript checking, a clean production build, publication-boundary checks, and repository diff checks. It will not run the extended performance suite.

## Failure behavior

An unreadable or invalid TextMate grammar, an unrecognized `jazz` registration, a pnpm lockfile mismatch, or missing generated highlighting will fail the relevant test or production build. The implementation will not silently fall back to the standalone Prism grammar.

## Non-goals

- Redesigning the website or documentation content.
- Changing Jazz language syntax as part of the website work.
- Replacing Prism for non-Jazz languages.
- Publishing internal `.codex/` plans on the documentation site.
- Adding compiler benchmarks or long performance suites to docs CI.
