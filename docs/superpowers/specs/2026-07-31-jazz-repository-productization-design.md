# Jazz Repository Productization Design

## Status

Accepted in discussion on `2026-07-31`.

This design defines how the Jazz repository will move from a multi-generation
compiler workspace into a professional, single-language project with one active
compiler, current public documentation, a Docusaurus website, and proportionate
project operations. It intentionally separates the migration into independent
workstreams so repository moves cannot be obscured by website or release work.

## Context

The repository currently presents three compiler generations at its root:

- `jazz-hs/`, a legacy implementation with vendored dependencies;
- `jazz2/`, an unfinished experimental rewrite; and
- `jazz-next/`, the only active compiler, interpreter, standard library, test,
  benchmark, and editor implementation.

The root Nix flake builds only `jazz-next/`. The active package is private and
unreleased, and no supported Haskell embedding API exists, so this is the least
disruptive point at which to remove the transitional `next` identity.

The existing `docs/` tree also mixes several unrelated concerns:

- canonical language contracts;
- current project status;
- historical comparisons with legacy implementations;
- completed implementation plans;
- agent execution queues; and
- Superpowers design and execution artifacts.

That mixture is useful during rapid development but is not an appropriate
source tree for a public language website.

## Goals

1. Make the repository unambiguously represent one language and one active
   compiler.
2. Preserve recoverability of the pre-migration repository without retaining
   legacy implementations on `main`.
3. Promote the active compiler to conventional root-level package paths.
4. Remove the transitional `jazz-next` and `JazzNext` identity throughout the
   build, executable, modules, tests, diagnostics, and documentation.
5. Make `README.md` a concise, accurate entry point for language users.
6. Establish intentionally authored public documentation and a professional
   Docusaurus website.
7. Separate public documentation, durable decisions, and agent execution state.
8. Add fast, useful CI and basic contributor/release hygiene without running
   exhaustive performance workloads on every change.

## Non-Goals

This program does not add or change Jazz language semantics. It also does not
introduce:

- a package manager or registry;
- a web playground;
- a language server;
- documentation versioning;
- analytics or telemetry;
- a new compiler backend;
- a public compiler embedding API;
- production-readiness claims; or
- a large governance structure.

Those remain separate projects and require their own designs when they become
the smallest useful next step.

## Decision 1: Canonicalize the Repository Root

### Preserve one explicit archive point

Immediately before the first destructive migration commit, create the annotated
tag:

```text
archive/pre-root-canonicalization-2026-07-31
```

The tag must point to the reviewed pre-migration `main` commit and state that it
preserves the last repository layout containing `jazz-hs/`, `jazz2/`, and
`jazz-next/`. Ordinary Git history remains available, but the tag provides a
stable, discoverable recovery point.

### Remove legacy implementations from `main`

Delete `jazz-hs/` and `jazz2/` in full, including vendored dependencies, local
build metadata, empty READMEs, placeholder standard-library files, examples,
and duplicated licenses. Do not move either implementation into an `archive/`
directory. The annotated tag and Git history are the archive.

Before deleting legacy-oriented documentation, extract only decisions that are
still authoritative for the current language. Do not preserve implementation
comparisons, superseded migration instructions, or historical path references
merely to avoid deleting files.

### Promote the active compiler to root

Move the contents of `jazz-next/` to conventional root paths:

```text
app/
benchmark/
editors/
jazz/
program-support/
programs/
src/
test/
cabal.project
cabal.project.profile-hotspots
cabal.project.profile-stages
jazz.cabal
PERFORMANCE.md
```

The existing root retains:

```text
.codex/
docs/
rfcs/
scripts/
website/
flake.nix
flake.lock
README.md
LICENSE
```

### Remove the transitional name

Perform the following canonical renames as part of the root migration:

- Cabal package: `jazz-next` -> `jazz`;
- private library: `jazz-next-internal` -> `jazz-internal`;
- executable: `jazz-next` -> `jazz`;
- benchmark executable: `jazz-next-bench` -> `jazz-bench`;
- Haskell module namespace: `JazzNext.*` -> `Jazz.*`;
- generated Cabal module: `Paths_jazz_next` -> `Paths_jazz`;
- Nix derivation and check names: `jazz-next` -> `jazz`; and
- user-visible CLI, diagnostic, profiling, fixture, and temporary-file labels
  where the old name denotes the active product rather than historical text.

The private library boundary remains private. This migration does not create a
public Haskell API.

### Migration constraints

- Use Git-aware moves for active files so history remains traceable.
- Keep language and runtime behavior unchanged.
- Update package-root discovery to locate `jazz.cabal` at the repository root.
- Update all build scripts, test fixtures, editor metadata, documentation links,
  ignore rules, profiling configurations, and source-layout audits together.
- Remove authority checks that require legacy implementations to be mentioned.
- Replace them with checks that establish `docs/` plus current implementation
  behavior as the authority.
- Treat the obsolete draft Copilot-instructions pull request as superseded,
  report it for maintainer closure, and do not attempt to merge instructions
  written for the old layout.

## Decision 2: Establish a Documentation Boundary

The final repository uses four distinct information roots:

```text
docs/       Curated public documentation and the only Docusaurus docs source
rfcs/       Durable language and compiler design decisions
.codex/     Active agent execution state and implementation planning
website/    Docusaurus application, theme, configuration, and static assets
```

### Public documentation

`docs/` contains only material intentionally written for language users or
contributors. It is organized as:

```text
docs/
  getting-started/
  language/
  standard-library/
  reference/
  compiler/
  project/
```

The current documentation is curated as follows:

| Current material | Final treatment |
| --- | --- |
| `docs/spec/**` | Rewrite into `docs/language/` and `docs/reference/` |
| `docs/feature-status.md` | Refresh as `docs/project/status.md` |
| Current parts of `docs/jazz-language-state.md` | Rewrite into language and compiler documentation |
| Legacy analysis in `docs/jazz-language-state.md` | Delete |
| `docs/jazz-improvement-backlog.md` | Condense into `docs/project/roadmap.md` |
| `docs/execution/**` | Move active state to `.codex/execution/`; delete completed archives |
| Active implementation plans | Move to `.codex/plans/` |
| Completed or superseded plans | Delete after extracting durable decisions |
| Durable accepted designs | Curate into `rfcs/accepted/` |
| Proposed durable designs | Store in `rfcs/proposed/` |
| Task-specific or superseded designs | Delete after extracting durable decisions |
| `docs/superpowers/**` | Eliminate from the final tree |

Superpowers documents are never website content. Docusaurus must not scan,
transform, copy, or publish them. A durable decision may be rewritten as an RFC
or summarized in public compiler documentation, but task lists, agent language,
verification logs, and implementation history do not cross that boundary.

### Authority

The authority order after migration is:

1. canonical public language contracts under `docs/language/` and
   `docs/reference/`;
2. behavior verified by current `src/`, `jazz/`, and `test/` code;
3. accepted decisions under `rfcs/accepted/`; and
4. roadmap material, which is informative rather than normative.

`.codex/` never defines public language behavior by itself.

## Decision 3: Replace the README

The root README is a user-first front door, not a project diary or a complete
manual. It should remain approximately 100-150 focused lines and contain:

1. a local Jazz logo or wordmark, concise tagline, and truthful badges;
2. a short description of the language;
3. one checked-in, executable Jazz example with expected output;
4. a root-level quick start using `jazz` commands;
5. a compact implemented-versus-developing feature summary;
6. an explicit experimental/pre-1.0 maturity statement;
7. links to the website, language guide, reference, standard library, roadmap,
   contributing guide, and issue tracker; and
8. license and contribution information.

The README must not contain JavaScript comparisons, aspirational syntax shown
as executable, exhaustive feature matrices, internal queues, legacy history,
or long compiler-architecture explanations. CI must execute the primary README
example through the real CLI.

## Decision 4: Build a Docusaurus Website

### Technology and ownership

Use Docusaurus for the documentation site. `website/` owns the Node application,
custom theme, configuration, and static assets. Docusaurus reads only the root
`docs/` directory. It does not publish `rfcs/` or `.codex/` by default.

Use GitHub Pages as the initial host and GitHub Actions for deployment. A custom
domain may be added later without changing the source architecture.

### Information architecture

The public site provides:

- Home;
- Getting Started;
- Language Guide;
- Standard Library;
- Language and CLI Reference;
- Compiler and Bootstrapping;
- Project Status and Roadmap; and
- Contributing and Governance.

Every feature page distinguishes behavior that is available today from partial
or planned work. Planned syntax must not appear as an executable example.

### Initial website scope

The initial site includes responsive navigation, accessible light and dark
themes, Jazz syntax presentation, a custom visual identity, useful code blocks,
metadata/social cards, link checking, and deployment automation.

The initial site excludes a playground, blog, documentation versioning, user
accounts, analytics, automated API extraction, and elaborate interactive
tutorials.

The site should not look like unmodified Docusaurus Classic. It uses a restrained
custom type system, a distinctive but limited Jazz color palette, polished code
surfaces, and proper local SVG/light/dark brand assets.

## Decision 5: Add Proportionate Project Polish

### Required repository additions

Add or refresh:

- `CONTRIBUTING.md`;
- `SECURITY.md`;
- `CHANGELOG.md`;
- `.editorconfig`;
- a pull-request template;
- focused issue templates;
- complete Cabal project metadata, including homepage, issue tracker, source
  repository, maintainer, synopsis, and tested compiler versions;
- approachable, CI-executed examples distinct from benchmark workloads;
- an accurate project status page and roadmap; and
- local SVG logo, wordmark, favicon, light/dark variants, and social-preview
  assets.

Do not add large governance documents, performance badges, or production claims
that the project cannot yet substantiate.

### Release preparation

After the cleanup and CI are stable, prepare an explicitly labeled alpha
release with repeatable artifacts and Nix flake app support. Automated binaries,
documentation search, and release-note automation belong to this later polish
step rather than the root migration.

## Decision 6: Tier Continuous Integration

CI is divided into four tiers.

### Fast pull-request tier

Run on every relevant pull request and require success before merge:

- warning-clean compiler build;
- focused compiler, runtime, module, standard-library, CLI, and contract tests;
- repository and source-layout audits;
- public documentation validation;
- Docusaurus build and link checking; and
- README and user-example smoke execution.

This tier must exclude `cabal bench`, profiling builds, exhaustive parser-scale
flags, and long performance workloads. Its target wall-clock budget is ten
minutes or less. If it exceeds that budget, move appropriate suites to the
default functional tier rather than normalizing slow pull-request feedback.

Documentation-only changes run documentation and website checks without
rebuilding the compiler. Compiler, Cabal, Nix, standard-library, or Jazz-authored
compiler changes trigger the fast compiler suite. CI must cancel superseded
pull-request runs and cache dependencies safely.

### Default functional tier

Run on pushes to `main` and by manual dispatch:

- the complete ordinary Cabal test matrix;
- all default deterministic parity suites; and
- package and repository validation.

This tier still excludes explicit exhaustive-scale flags, profiling, and
benchmarks.

### Extended tier

Run weekly and by manual dispatch:

- full parser-scale suites;
- the complete production-shaped corpus;
- profiling builds;
- `cabal bench`;
- repeated determinism checks; and
- stored benchmark and profiling artifacts.

Correctness failures are actionable. Timing changes are advisory and must not
fail solely because a shared runner is slower than a previous runner.

### Release tier

Run for release candidates and version tags:

- all functional and extended checks;
- package validation;
- clean release builds;
- documentation and website builds; and
- distributable artifact verification.

Benchmark completion is required, but an isolated timing percentage does not
block a release without a separately validated performance regression.

## Delivery Workstreams

This program is delivered through four sequential workstreams with separate
specs, plans, verification, commits, and pull requests.

### Workstream 1: Repository canonicalization

Create the archive tag, remove legacy implementations, promote the active
package to root, rename the package/executable/module identity, repair build and
path ownership, and update internal authority checks. Do not mix semantic
compiler changes into this workstream.

### Workstream 2: Documentation reset

Establish `docs/`, `rfcs/`, and `.codex/`; curate or delete existing material;
rewrite the README; and author the initial public documentation set. This begins
only after canonical root paths are stable.

### Workstream 3: Website

Create and customize Docusaurus, connect it only to the curated public docs,
add brand assets, validate the production build, and deploy to GitHub Pages.

### Workstream 4: Project operations

Add tiered CI, contributor files, package metadata, examples, templates, and
alpha-release preparation. CI must use canonical root commands; release
preparation follows stable docs and website delivery.

## Verification and Failure Handling

### Migration verification

The root migration is complete only when:

- `jazz-hs/`, `jazz2/`, and `jazz-next/` are absent from `main`;
- active non-historical files contain no product references to `jazz-next`,
  `JazzNext`, `Paths_jazz_next`, or legacy compiler paths;
- Nix and Cabal build the root `jazz` package;
- the executable reports the canonical `jazz` identity;
- the complete ordinary test matrix passes in the checked-in Nix environment;
- explicit extended suites pass once before publication;
- package checks, repository audits, docs checks, and `git diff --check` pass;
  and
- the archive tag resolves to the reviewed pre-migration commit.

If the migration cannot reach this state without semantic compiler changes,
stop and restore from the last reviewable migration commit. The archive tag is a
recovery and reference point, not a branch on which cleanup work continues.

### Documentation and website verification

- Docusaurus consumes only `docs/`.
- No Superpowers or `.codex/` material appears in generated output.
- All public internal links resolve.
- Published Jazz examples are executable or explicitly marked non-executable.
- README commands run from repository root.
- Status claims match the current implementation and carry a current revision
  or update date.
- The production website build succeeds without network-only content
  dependencies.

### Review boundaries

Each workstream must be independently reviewable. A later workstream must not be
used to hide failing verification in an earlier one. Repository moves should be
reviewed as moves and identity changes; public writing should be reviewed for
accuracy; website code should be reviewed for accessibility and build behavior;
and CI should be reviewed for feedback time and signal quality.

## Success Criteria

The program is complete when a new visitor sees one language, one compiler, one
CLI, one current README, and one coherent documentation site; a contributor can
build and test from repository root; internal execution artifacts do not leak
into public documentation; pull requests receive fast required feedback; and
the project can produce an honest, repeatable pre-1.0 release without relying on
legacy code or undocumented local knowledge.
