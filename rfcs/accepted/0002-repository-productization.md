# RFC 0002: Repository productization

Status: Accepted
Date: 2026-07-31
Supersedes: Repository-productization decision dated 2026-07-31.

## Decision

The Jazz repository represents one language, one active compiler, and one CLI.
The canonical product identity is `jazz`:

- Cabal package and executable: `jazz`;
- private Haskell library: `jazz-internal`;
- benchmark executable: `jazz-bench`;
- Haskell module namespace: `Jazz.*`; and
- generated Cabal module: `Paths_jazz`.

The active compiler is rooted at conventional repository paths. `src/` owns
the Haskell stage-0 compiler and runtime, `jazz/` owns the Jazz-authored
standard library and hosted compiler, `app/` owns the CLI entry point, and
`test/` owns verification. `benchmark/`, `program-support/`, `programs/`, and
`editors/` own their corresponding project surfaces. The package remains an
internal compiler package; this decision does not create a supported Haskell
embedding API.

The pre-migration multi-implementation layout is preserved by the annotated
tag:

```text
archive/pre-root-canonicalization-2026-07-31
```

Git history and that tag are the archive. Removed implementations are not kept
in a live `archive/` directory and do not remain sources of current authority.

Repository information has four owners:

```text
docs/       Curated public documentation and the only website docs source
rfcs/       Durable language and compiler decisions
.codex/     Active execution state and implementation plans
website/    Docusaurus application, theme, configuration, and assets
```

The documentation website uses Docusaurus, reads only `docs/`, and is initially
deployed to GitHub Pages. RFCs and internal execution material are not
published by default. The root README is a concise, user-first entry point with
an executable example, root-level quick start, honest maturity statement, and
links to the full documentation.

Continuous integration is tiered:

- pull requests receive a fast functional, repository, documentation, website,
  and example gate with a target duration below ten minutes;
- pushes to `main` run the complete ordinary functional matrix;
- weekly and manual extended runs own exhaustive scale, corpus, benchmark,
  profiling, and determinism evidence; and
- release candidates and version tags run functional and extended checks plus
  artifact verification.

Physical timing is advisory. Deterministic correctness and semantic-budget
failures remain actionable. RFC 0008 defines the detailed performance-tier
boundary.

## Context

The repository accumulated several compiler generations, a transitional
product name, duplicated package roots, and a documentation tree that mixed
public material with agent artifacts. The active package was still private and
unreleased, making this the least disruptive point to establish a single
canonical identity and conventional layout.

The same cleanup needed to preserve recoverability without making historical
code look supported. A stable annotated tag provides a discoverable recovery
point while allowing the main branch and public documentation to describe only
the current project.

## Consequences

- Build, test, documentation, editor, profiling, and path-discovery tooling use
  repository-root commands and canonical names.
- Public documentation, durable rationale, and internal execution history no
  longer share a publication boundary.
- Superpowers artifacts and completed task plans are removed after durable
  decisions are curated.
- The website can be redesigned independently of compiler moves because it has
  a single explicit source tree.
- Fast pull-request feedback does not pay for full performance workloads.
- A package manager, registry, playground, language server, public embedding
  API, native backend, and production-readiness claim remain separate future
  decisions.
