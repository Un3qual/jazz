# Jazz semantic documentation rewrite design

**Date:** 2026-08-13

**Status:** Approved for implementation

## Purpose

Rewrite the public Jazz website so it documents the language rather than
translating its notation into prose. The existing visual design and the Learn,
Standard Library, and Reference information architecture remain. The change is
an editorial rewrite across every public page.

A useful sentence must do at least one of the following:

- explain what a language feature means;
- state observable behavior or a programmer-facing guarantee;
- show how features compose;
- identify a consequential edge case, failure mode, or cost; or
- help a reader complete a real task.

Sentences that only restate a heading, type signature, function name, command,
or punctuation rule should be removed. Syntax is normally shown in code rather
than narrated in English.

## Research basis

The rewrite follows the separation used by established language documentation:

- Go distinguishes user guidance, the exact language specification, the
  standard library, and idiomatic programming guidance.
- Python's tutorial teaches concepts and use, while its language reference is
  organized around the data model, execution model, expressions, statements,
  and grammar.
- Rust API reference pages lead with declarations and spend prose on ownership,
  evaluation, failure, alternatives, and other consequential behavior.
- Elixir keeps syntax reference separate from guides that explain semantics and
  behavior.

These sites are models for separation of concerns, not templates to copy. Jazz
will avoid repetitive API prose and examples that merely restate an obvious
name or signature.

## Editorial model

### Learn

Learn pages build a mental model of Jazz. Each page begins with the capability
or programming problem, then explains behavior, consequences, and composition.
Short code fragments demonstrate the model.

Learn pages may mention syntax when a reader needs it to use the feature, but
they do not describe source text token by token. Exact accepted forms,
precedence, lexical restrictions, and grammar belong in Reference.

Useful subjects include scope, evaluation order, inference, persistence,
pattern selection, exhaustiveness, effects, visibility, module boundaries, and
runtime consequences. Unsupported forms are called out only when a likely
misunderstanding or migration trap makes the restriction important.

### Standard Library

Module pages remain API references organized by public types, constructors, and
values. Each public value retains its exact linked type signature.

Prose states only non-obvious contracts: empty-input behavior, ordering,
stability, laziness, callback invocation, persistence, failure, normalization,
or complexity. A declaration whose name and signature already communicate its
complete behavior does not need a paragraph that repeats them.

Examples remain short expressions. They appear only when they clarify argument
order, a boundary case, a transformation, a failure path, or another behavior
that is difficult to infer from the declaration. Complete runnable programs are
not required.

Module introductions explain the abstraction and when it is useful. They do
not provide export inventories or abstraction/purpose tables.

### Reference

Reference pages are compact, exact contracts. They own lexical forms, grammar,
precedence, evaluation rules, module resolution, diagnostics, runtime values,
and CLI behavior.

Grammar can describe punctuation precisely because that is the page's job, but
obvious token spellings do not receive tutorial commentary. Reference prose
focuses on ambiguity resolution, validity boundaries, observable semantics,
and cross-links to the relevant Learn page.

### Getting started

Getting-started pages are task-oriented. Commands and programs are followed by
the result a reader should expect and the first useful conceptual consequence.
They do not explain ordinary shell syntax or narrate each visible character in
an example.

### Compiler and project pages

Compiler pages describe the stages that transform a Jazz program and the
responsibility of each stage. They do not inventory implementation files.

Project pages remain procedural. Contributor material may name repository paths
when a contributor must act on them; public language and compiler explanations
may not. Status, governance, and roadmap prose should be factual and free of
marketing or publication-process commentary.

### Homepage

The homepage introduces Jazz as a programming language: its programming model,
important capabilities, and the shortest route into the documentation. It does
not lead with parser or compiler-component inventories, accepted-form trivia,
slogans, or claims about how documentation is generated and verified.

## Site-wide rewrite scope

The audit covers every rendered public Markdown page under `docs/` and all
public homepage copy under `website/src/`. It includes:

- the documentation introduction;
- getting started and first-program material;
- every language guide page;
- every standard-library module page;
- every reference page;
- compiler architecture, pipeline, and bootstrapping pages; and
- status, roadmap, governance, and contributing pages.

Not every page requires the same amount of change. Exact reference material,
commands, signatures, public API names, and genuinely useful examples remain.
The audit changes or removes prose only when it violates the editorial model.

## Rewrite procedure

For each page:

1. Identify the reader's question and the page's documentation category.
2. Remove duplicated introductions, headings, signature paraphrases, syntax
   narration, marketing language, and publication metadata.
3. Reorder the surviving material around behavior and consequences.
4. Add missing semantic guarantees only when they are supported by the public
   language contract or implementation evidence.
5. Replace verbose syntax descriptions with the smallest useful code fragment
   and a Reference link.
6. Retain examples only when they teach a non-obvious behavior.
7. Check terminology and cross-links against adjacent pages.

The rewrite must not infer capabilities from aspirations or roadmap material.
`docs/language/` and `docs/reference/` remain the public contract; compiler,
standard-library, and tests provide implementation evidence where the contract
needs confirmation.

## Quality bar

The finished site should let a reader distinguish immediately between:

- learning how Jazz behaves;
- looking up an exact language rule; and
- looking up a standard-library declaration.

Pages should be shorter where existing prose is redundant, but not shallow.
Information density comes from retaining consequential facts and removing
explanations of the obvious, not from compressing text mechanically.

## Verification

Verification will include:

- a page-by-page editorial audit recorded in the implementation work;
- existing public-documentation and standard-library coverage checks;
- website experience, brand, search, signature-link, type-link, and boundary
  checks;
- TypeScript type checking and the production Docusaurus/Pagefind build;
- link and generated-site validation; and
- `git diff --check`.

Content tests will protect structural contracts and exact public facts. The
rewrite will not introduce a broad banned-word test or brittle assertions over
ordinary prose.

## Non-goals

- Changing Jazz syntax, semantics, compiler behavior, or standard-library APIs.
- Redesigning the site, navigation, search, logo, or code rendering.
- Moving all syntax out of Learn pages regardless of teaching value.
- Making every example a complete runnable program.
- Adding examples to obvious APIs merely for visual consistency.
- Hiding real limitations, partial behavior, or compatibility constraints.
