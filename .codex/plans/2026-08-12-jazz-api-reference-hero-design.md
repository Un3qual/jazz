# Standard Library API Reference and Hero Lockup Design

**Date:** 2026-08-12
**Status:** Approved design

## Objective

Turn each standard-library module page into a conventional programming-language API reference. Every public type, constructor, and value must be individually discoverable and documented from its canonical declaration. Recompose the homepage hero so the Jazz mark is a deliberate part of the title lockup instead of a small, absolutely positioned decoration.

## Standard library reference model

Each module page follows the same reading order:

1. Module purpose and import guidance.
2. Public types and constructors, when present.
3. Exported values grouped by responsibility.
4. Related modules and language-guide links.

Every exported value receives its own heading, exact type signature in a `jazz` code block, and a concise contract. The contract states the important observable behavior: empty-input handling, ordering, persistence, failure semantics, callback behavior, and complexity when those details matter.

Examples are short expressions rather than complete programs. They appear only when they clarify behavior that the signature and contract do not make obvious, such as argument order, clamping, stable ordering, fallback selection, normalization, or error handling. Routine predicates and direct accessors do not need repetitive examples.

The page must not use a function inventory table or a bullet list as a substitute for API entries. Introductory summaries remain short so the signatures and contracts carry most of the information density.

## Source of truth and coverage

The public export lists and type signatures in `jazz/stdlib/*.jz` define the reference inventory. Documentation tests compare those declarations with the corresponding Markdown module page so omissions and stale signatures fail verification.

Internal helpers and private constructors are excluded. Public algebraic data types document their constructors before function entries. Abstract types state that their representation is private and document only their public type name and exported values.

## Hero composition

**Visual thesis:** a compact technical introduction anchored by one unmistakable Jazz title lockup, balanced against the compiler-checked code sample.

The translucent, absolutely positioned mark is removed. The mark becomes part of normal document flow inside the copy column. On desktop it sits beside the `Jazz` title at a visually substantial size, with their baselines and optical centers aligned. On mobile it moves above the title and remains large enough to read without crowding the summary or actions.

The hero retains its two-column desktop composition: title lockup and language introduction on the left, code proof on the right. It retains a single-column mobile layout. The mark uses the existing canonical SVG asset and remains decorative to assistive technology because the adjacent title supplies the accessible name.

## Content plan

- Hero: Jazz title lockup, one-sentence description, concise language detail, two documentation routes, and the code proof.
- Documentation directory: unchanged in purpose and placement.
- Standard-library pages: module orientation followed immediately by grouped API entries.

## Interaction thesis

- Preserve the restrained copy and code entrance sequence already used by the hero.
- Preserve clear hover and focus treatment on documentation routes.
- Add no decorative logo animation; placement and scale provide the hierarchy.

Reduced-motion behavior continues to remove entrance transforms.

## Responsive and accessibility behavior

- The desktop lockup must not overlap the code column at the current 760px collapse boundary.
- The mobile lockup must fit within the existing page gutter with no horizontal overflow.
- Navigation targets remain at least 44px on touch layouts.
- Heading hierarchy remains semantic: one page `h1`, module groups as `h2`, and individual API entries at a consistent subordinate level where grouping requires it.
- Signature blocks use Jazz highlighting and remain horizontally scrollable on narrow screens.

## Verification

- Add a documentation regression check that compares every public standard-library export and signature with its module page.
- Extend the website experience test to require a normal-flow hero lockup and reject the previous absolute-positioned, undersized mark.
- Run the focused documentation and website tests red before implementation and green afterward.
- Run the full documentation gate, website gate, TypeScript check, production build, and desktop/mobile browser visual QA before completion.

## Non-goals

- Generating API prose from source code.
- Documenting internal helpers or kernel primitives.
- Adding executable full-program examples to every entry.
- Redesigning the documentation directory, navbar, or overall visual theme.
- Changing standard-library behavior or public signatures.
