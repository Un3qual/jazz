# Jazz documentation site density and navigation design

**Date:** 2026-08-12

**Status:** Approved for implementation

## Purpose

Make the Jazz website read and navigate like programming-language
documentation. Retain the existing visual identity, but replace the spacious,
campaign-style homepage and mixed documentation navigation with compact
orientation, direct technical content, and module-oriented standard-library
reference pages.

The site should help a reader answer four questions quickly:

1. What kind of language is Jazz?
2. How do I run a small program?
3. Where is the guide or exact reference for a language feature?
4. What does a particular standard-library module export?

## Information architecture

The documentation plugin will expose three independent sidebars.

### Learn

The Learn sidebar owns explanatory and project-oriented material:

- documentation index;
- getting started and the first-program tutorial;
- language guide;
- compiler architecture, pipeline, and bootstrapping explanations; and
- project status, roadmap, governance, and contributing information.

### Standard Library

The Standard Library sidebar owns only module documentation. It presents a
module tree rather than a survey table:

- Overview
- Prelude
- Data
  - Maybe
  - Result
  - NonEmpty
- Collections
  - List
  - Dictionary
  - Queue
  - Map
  - Set
- Text
  - Char
  - Text
- System
  - IO
  - IOError

Every named module receives its own page and route. The current combined pages
for Maybe/Result/NonEmpty, Map/Set, Char/Text, and IO/IOError will be split.
Cross-links between related modules will replace the overview's abstraction and
purpose table.

Module pages will lead with import behavior or the primary type, then group
public names by operation. Signatures, edge behavior, ordering guarantees, and
complexity remain where they affect program design. Pages will not add a
generic summary table merely to restate those groups.

### Reference

The Reference sidebar owns compact, exact contracts:

- lexical grammar;
- expression grammar;
- module resolution;
- CLI behavior;
- diagnostics; and
- runtime values.

The navbar will link to Learn, Language, Standard Library, Reference, and
GitHub. Language links directly to the language overview inside Learn. Project
Status remains reachable through Learn and the footer but does not appear in
the navbar.

## Homepage

The homepage remains an introduction, not a bare documentation directory. Its
first viewport will use a compact two-column layout:

- a short factual description of Jazz and direct links to Getting Started and
  the Language Guide; and
- one representative Jazz program with its run command and result.

Below the introduction, a dense documentation directory will link to the main
Learn, Standard Library, Reference, Compiler, and Project destinations. Plain
headings, short descriptions, lists, and rules will provide hierarchy. The
homepage will remove the full-viewport hero, numbered editorial movements,
campaign-style closing call to action, oversized display headings, slogans, and
verification-process copy.

The current colors, typography, wordmark, Bellhook mark, Jazz syntax
highlighting, light/dark themes, and focus treatment remain. Motion is limited
to short entrance and link-state transitions and respects reduced-motion
preferences.

## Density and navigation behavior

The desktop navbar will be shorter than the current 4.25rem bar. Its inner
container, brand, links, and right-side controls will share one explicit
vertical center so labels no longer sit high relative to the wordmark. Mobile
controls retain accessible touch targets.

Documentation typography will use smaller page titles, shorter section gaps,
tighter paragraph leading, and denser desktop sidebar rows while preserving a
readable line length. Tables and code blocks remain horizontally usable on
small screens. Mobile navigation and interactive controls keep at least 44px
touch targets even where the desktop sidebar is denser.

## Public-content rules

Public teaching, language, compiler, and API pages describe Jazz behavior,
not the mechanics used to publish or verify the website. Remove copy about:

- Pages enablement or post-merge repository settings;
- examples being synchronized from repository fixtures;
- compiler-backed documentation checks; and
- documentation-only implementation snapshots.

Repository paths and source-file ownership do not belong in language or
compiler explanations. Contributor setup may still name repository directories
and commands when the path itself is necessary to perform contributor work.

Copy should be factual and specific. Avoid aspirational promises, slogans,
marketing claims, design commentary, and headings that do not help readers find
or use technical information.

## Compiler documentation model

The compiler pages will explain data and responsibility transitions rather
than enumerate implementation files. The architecture and pipeline will use
the following conceptual stages:

1. source selection and module discovery;
2. parsing and canonical-core construction;
3. name resolution and module-interface construction;
4. scope and pattern analysis, type inference, capability checks, and purity
   checks;
5. ordered diagnostics and warning policy;
6. canonical-core interpretation for ordinary run mode; and
7. optional typed-core production, backend-neutral lowering, and structural
   validation for the bounded backend-preparation path.

The standalone and module-graph variants will be explained where their stage
ordering differs. The bootstrapping page will describe stage 0, hosted
components, behavioral parity, and promotion requirements without listing
their source locations. Current partial boundaries remain explicit, but status
detail should link to Project Status instead of overwhelming the architecture
overview.

## Implementation boundaries

- Keep Docusaurus and the existing single docs plugin.
- Define the three sidebars in `website/sidebars.ts`; do not create a second
  documentation system or duplicate content across plugin instances.
- Keep the checked example generator and TextMate highlighter as internal build
  mechanisms. Remove only the public copy that discusses those mechanisms.
- Split standard-library Markdown pages without changing Jazz library source or
  public language behavior.
- Update internal links, navbar links, footer links, pagination ownership, and
  structural tests alongside moved or split pages.

## Verification

Focused tests will require:

- the exact navbar destinations and absence of a Status navbar item;
- independent Learn, Standard Library, and Reference sidebars;
- the complete module tree and one documentation page per module;
- a compact introductory homepage without the old editorial regions or public
  synchronization/verification copy;
- compiler architecture content organized by conceptual stage without active
  source-path inventories; and
- the Standard Library overview without the abstraction/purpose table.

Final verification will run the website experience and brand tests, TypeScript
type checking, the production Docusaurus build and highlighting check, public
documentation checks, website boundary checks, and `git diff --check` through
the repository's Node 22/Nix website gate.

## Non-goals

- Changing Jazz syntax, compiler behavior, or standard-library APIs.
- Replacing Docusaurus, the TextMate highlighter, or the existing brand.
- Adding search, versioned docs, a package registry, or interactive examples.
- Hiding experimental or partial language boundaries.
- Removing repository-specific instructions from contributor-only workflows
  where those instructions are the subject of the page.
