# Jazz publication validation simplification design

**Date:** 2026-08-09

**Status:** Approved for implementation by the delegated PR 127 request

## Purpose and trust boundary

PR 127 currently carries bespoke parsers for GitHub Actions YAML, rendered Markdown, HTML, CSS, `srcset`, CSS escapes, and Docusaurus configuration source shape. Across `check-docs-pages-workflow.py`, `check-website-boundary.py`, `check-public-docs.py`, and their tests, this is nearly 6,000 lines of policy code before the existing Markdown helper suites and CI-policy checker are counted.

Jazz accepts changes from trusted contributors through review. The validation boundary protects against ordinary mistakes: a missing page, stale copied example, internal path in public content, remote runtime URL, broadened workflow permission, unsafe checkout, unpinned action, or reordered deployment step. It does not protect against an author deliberately encoding a bypass while also changing the checker and CI that enforce it. Tests for CSS escapes, malformed browser-normalized authorities, duplicate HTML attributes, hidden decoy markup, and hand-parsed YAML block-scalar impersonation are outside this boundary.

## Guarantee map

| Guarantee | Smallest authoritative mechanism |
| --- | --- |
| Production build and broken-link failures | `pnpm --dir website run build`; Docusaurus owns Markdown rendering, routes, navigation resolution, and broken links. A small configuration check preserves `onBrokenLinks: 'throw'` and `onBrokenMarkdownLinks: 'throw'`. |
| Required public docs and navigation integrity | A compact public-doc inventory checks required `.md` files, rejects `draft`, non-Markdown files, escaping symlinks, and explicit internal-only terms. Docusaurus validates rendered links, fragments, sidebars, navbar, and footer links. |
| Compiler-backed example synchronization | `scripts/check-examples.py` executes `scripts/example-cases.tsv`; the public-doc checker only matches explicit `jazz-example` and `jazz-example-output` markers to those canonical sources/results. Fragment hashes remain the lightweight docs-CI receipt and are compiler-validated when `--jazz-bin` is supplied. |
| Deterministic brand assets | `website/scripts/test-brand-assets.mjs` renders real assets and compares output bytes. No Python asset-shape policy is needed. |
| No internal-only publication | Source inventory rejects named internal references; a post-build text scan rejects them in emitted HTML, CSS, and JavaScript. `.codex/`, RFCs, and execution records remain outside Docusaurus's configured `../docs` root. |
| No remote runtime resources | A post-build scan extracts ordinary absolute/protocol-relative URLs and permits only the exact production origin and named GitHub navigation destinations. This catches accidental fonts, images, stylesheets, scripts, and fetch targets without emulating browsers. |
| Pinned Actions, least privilege, safe checkout, syntax/order | Pin every `uses:` entry to an immutable commit. A compact Pages contract checks exact actions, job permissions, `persist-credentials: false`, required commands, and deployment order. `actionlint` owns YAML/workflow syntax and runs in Nix-backed CI verification. |

## Component boundaries

- `scripts/check-public-docs.py` owns Jazz-specific public inventory and explicit example synchronization only. It does not parse general Markdown links, rendered visibility, or Docusaurus slugs.
- `scripts/check-website-boundary.py` owns the Docusaurus publication root/config policy and a simple emitted-output scan only. It does not parse authored TypeScript, HTML trees, CSS grammar, or browser URL normalization.
- `scripts/check-docs-pages-workflow.py` owns the small semantic Pages deployment contract only. It assumes `actionlint` has validated YAML and does not implement a YAML parser.
- `scripts/check-website.sh` composes source checks, Node behavior tests, TypeScript, the real production build, and the post-build boundary check.
- `scripts/ci/fast-compiler.sh` and `scripts/ci/main-functional.sh` run `actionlint` so invalid workflows cannot bypass the ordinary PR/main gates.

## Failure behavior and tests

Focused tests start from the checked-in valid workflow or documentation tree and mutate one observable contract. Each retained test names a realistic accidental regression. Removed tests are limited to deliberate parser evasions, exact private source shapes, or upstream Docusaurus/CommonMark/browser semantics.

The implementation should materially reduce total lines in the three checker-test pairs and must not replace deleted parsers with new wrappers. Final verification includes focused red/green runs, the complete docs and website gates, compiler-backed examples, CI policy, `actionlint`, lint/compile checks, and `git diff --check`.

## Non-goals

- Redesigning documentation or website content.
- Changing Jazz language behavior.
- Treating repository authors as hostile input producers.
- Preserving historical bot findings whose only value is defeating an intentionally evasive checker fixture.
