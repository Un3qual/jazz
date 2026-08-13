# Documentation Search and Signature Type Links Design

**Date:** 2026-08-12
**Status:** Approved design

## Objective

Add self-contained full-site documentation search to the static Docusaurus build and make every concrete type or capability in standard-library signature blocks link to its canonical documentation destination. Search must require no hosted indexing service, account, API key, or runtime network dependency.

## Static search architecture

Pagefind indexes the rendered Docusaurus production output after the normal build. Its generated index and search runtime ship inside the GitHub Pages artifact and load only when search is opened. Indexing operates on rendered pages so titles, headings, prose, API entry names, and signatures are searchable without maintaining a parallel content inventory.

The build marks the main documentation content as searchable and excludes repeated shell content such as the navbar, sidebar, table of contents, and footer. Search results retain the page title, closest matching section, documentation category, route, and a short matching excerpt.

## Search interface

The navbar gains a Search control immediately before GitHub. It is a compact utility control, not a marketing element. Activating it opens a restrained dialog containing:

- one autofocus search input;
- a short keyboard hint;
- ranked results with page title, matching section, category, and excerpt; and
- explicit empty, loading, and unavailable states.

The dialog opens from the navbar control, `/`, or `Ctrl/Cmd+K`. It closes with Escape, the close control, or successful navigation. Keyboard focus stays within the open dialog and returns to the invoking control when it closes. Arrow-key and Enter behavior follows the underlying result list semantics. Search must not intercept `/` while the user is already typing in an input, textarea, select, or editable element.

On mobile, the same dialog uses the available viewport width, a full-width input, and touch targets of at least 44px. The navbar control may reduce to an icon and accessible label when horizontal space is constrained.

## Search failure behavior

Local development may run without a generated Pagefind index. In that state, opening search shows a concise unavailable message rather than throwing or leaving a perpetual spinner. Production verification requires the generated index and exercises a real query against it.

No search telemetry, remote request, or hosted fallback is added.

## Signature link boundary

Only fenced Jazz blocks immediately marked with `<!-- jazz-signature -->` receive semantic type links. Ordinary examples, executable examples, homepage code, and non-Jazz blocks retain their existing rendering.

The existing Jazz syntax highlighter continues to produce token colors. A signature-aware renderer recognizes concrete type, capability, and type-syntax tokens and wraps them in internal Docusaurus links. Generic variables such as `a`, `b`, `e`, `f`, `k`, `v`, and `w` remain plain tokens.

Links preserve the syntax-highlight color and never display an underline, including hover and focus states. Hover uses a subtle background or color shift. Keyboard focus uses a visible outline or background treatment that does not rely on underlining.

## Canonical type destinations

The renderer owns one explicit mapping from public type syntax to canonical routes and anchors:

- `Maybe`, `Result`, `NonEmpty`, `Dictionary`, `Queue`, `Map`, and `Set` link to their standard-library module pages.
- `Char` and `Text` link to their standard-library module pages.
- `List` and bracket list syntax such as `[a]` link to the List page.
- `IOError` and `IOErrorCategory` link to their headings on the IOError page.
- `Ordering`, `Eq`, `Ord`, `Num`, `Integral`, `Fractional`, `Showable`, and `Default` link to their Prelude entries.
- `Bool`, `Int`, `Float`, every sized signed, unsigned, and floating numeric type, tuple syntax, and unit syntax link to stable anchors on Runtime values.

The Runtime values page receives explicit headings or anchors where necessary so every built-in destination is precise and stable. User-defined or unknown capitalized identifiers remain unlinked rather than guessing a destination.

## Rendering and routing

Links use the configured Docusaurus base URL, so they work under `/jazz/` in production and under local development routes. The renderer must preserve whitespace, punctuation, syntax scopes, copy-button behavior, and accessible code semantics.

Type-link generation is deterministic and independent of the current page. Linking a type to the current page is allowed because it still provides a stable entry anchor and consistent behavior across signatures.

## Visual thesis

Search should feel like part of a dense programming-language reference: immediate, keyboard-first, low-chrome, and optimized for scanning exact symbols. Signature links should read first as syntax and second as navigation.

## Content plan

- Navbar: one Search utility before GitHub.
- Search dialog: input, keyboard hint, ranked results, and explicit state copy.
- API signatures: existing highlighted blocks with unobtrusive semantic type links.
- Runtime values: only the additional anchors and concise type orientation needed to supply canonical destinations.

## Interaction thesis

- A fast dialog entrance and exit clarifies focus without decorative motion.
- Active result and signature-link hover/focus treatments sharpen affordance.
- Reduced-motion users receive the same state changes without transforms.

## Verification

- Test Pagefind post-build indexing against the production `build/` directory and run a real query that returns a known API entry.
- Test search keyboard activation, editable-element exclusion, close behavior, loading, empty, unavailable, and result rendering states.
- Test the type-destination mapping independently with built-ins, standard-library types, capabilities, list syntax, tuple/unit syntax, generic variables, and unknown identifiers.
- Test that only `jazz-signature` fences receive linked tokens and ordinary Jazz examples contain none.
- Verify every mapped internal route and anchor through the production Docusaurus build.
- Run the authoritative documentation and website gates and inspect search plus signature links at desktop and mobile sizes.

## Non-goals

- Algolia, Typesense Cloud, or another hosted search service.
- Search telemetry, personalization, recent searches, or query persistence.
- Linking arbitrary identifiers or user-defined types by heuristic.
- Adding links inside ordinary code examples.
- Changing Jazz syntax colors or standard-library behavior.
- Redesigning the navbar, documentation layout, or homepage beyond adding the Search utility.
