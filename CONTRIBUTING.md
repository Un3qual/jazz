# Contributing to Jazz

Jazz is an experimental, pre-1.0 language. Contributions are welcome, but the
language, compiler, standard library, diagnostics, and CLI are still evolving.
Keep changes focused, preserve current behavior unless a semantic change has
been accepted, and avoid compatibility claims that the project cannot yet
support.

## Set up the repository

The supported development environment is the repository's Nix flake. From the
repository root:

```bash
nix --extra-experimental-features 'nix-command flakes' develop
cabal build all
cabal test all --test-show-details=direct
```

`cabal.project` enables the development flag, so compiler warnings fail the
build. See the [getting-started guide](docs/getting-started/overview.md) for the
current source-build prerequisites and first program.

## Choose the right change process

Observable language semantics are RFC-first. Changes to accepted syntax,
typing, diagnostics, module visibility, runtime values, or standard-library
contracts need an accepted RFC before implementation. Start with the
[RFC process](rfcs/README.md) and use the language-proposal issue form.

Maintenance that preserves observable language behavior can begin with an
implementation and pull request. Examples include internal refactoring,
test-strengthening, build fixes, and documentation corrections. If a proposed
maintenance change reveals a semantic choice, pause implementation and move
that choice into an RFC.

## Repository ownership

- `src/` owns the active Haskell compiler and runtime.
- `app/` owns the command-line entry point.
- `test/` owns compiler, runtime, CLI, contract, and repository tests.
- `jazz/` owns the Jazz-authored standard library and hosted compiler sources.
- `examples/` owns checked teaching programs; `programs/` owns the larger
  correctness and performance corpus.
- `docs/language/` and `docs/reference/` are the public language contract.
  Other `docs/` sections explain usage, implementation, and project status.
- `rfcs/` records durable semantic and architectural decisions.
- `.codex/execution/` and `.codex/plans/` are internal coordination state, not
  public language documentation.
- `website/` owns the Docusaurus site that publishes the curated `docs/` tree.

Update behavior, focused tests, and affected public documentation together.
Runnable examples shown in public docs must come from checked files under
`examples/`.

## Verify a change

While developing, run the smallest relevant test component:

```bash
cabal test parser-core-spec --test-show-details=direct
```

Before requesting review, run the checks appropriate to the change. The normal
compiler path is:

```bash
cabal build all
cabal test all --test-show-details=direct
cabal check
bash scripts/check-examples.sh
git diff --check
```

Check changed Haskell files with the repository's pinned formatter, passing
explicit file paths:

```bash
ormolu --mode check src/Jazz/Compiler/Parser.hs test/Jazz/Compiler/Parser/ParserFoundationSpec.hs
```

For public documentation or website work, run:

```bash
prettier --check README.md CONTRIBUTING.md SECURITY.md CHANGELOG.md RELEASING.md docs .github
bash scripts/check-docs.sh
bash scripts/check-website.sh
```

Run commands inside `nix develop` so tool versions match CI. The CI entry points
are also available locally:

- `scripts/ci/fast-compiler.sh` runs warning-clean, focused pull-request checks.
- `scripts/ci/main-functional.sh` runs the complete ordinary test matrix.
- `scripts/ci/extended.sh` runs exhaustive parser-scale, repeated corpus,
  profiling, determinism, and benchmark work.
- `scripts/ci/release-candidate.sh` combines all release-candidate gates.

Pull requests run documentation/site checks and, when compiler-relevant files
change, the fast compiler tier. Pushes to `main` run the ordinary tier. Extended
verification runs weekly or manually; release verification is explicit.
Performance and profiling workloads are intentionally absent from routine pull
requests because they are long-running and shared-runner timings are advisory,
not stable pass/fail thresholds.

## Submit a pull request

Prefer small commits that each explain one coherent change. A pull request
should:

- explain the problem and the chosen approach;
- state whether language semantics change and link the accepted RFC when they
  do;
- include the exact verification performed;
- update tests and public documentation where behavior changes; and
- identify whether fast, ordinary, extended, or release verification is
  relevant.

Do not mix unrelated cleanup into a behavioral change. Draft pull requests are
appropriate for early design feedback, but review-ready changes should be
warning-clean and free of generated build output.

## Report or propose work

- Use the [bug-report form](https://github.com/un3qual/jazz/issues/new?template=bug-report.yml)
  for reproducible compiler, runtime, CLI, or tooling defects.
- Use the [language-proposal form](https://github.com/un3qual/jazz/issues/new?template=language-proposal.yml)
  to define a semantic problem before drafting an RFC.
- Use the [documentation form](https://github.com/un3qual/jazz/issues/new?template=documentation.yml)
  for inaccurate, missing, or unclear public documentation.
- Report vulnerabilities privately as described in the
  [security policy](SECURITY.md), not in a public issue.

Search existing issues before opening a new one. For substantial work, open or
join an issue first so scope and semantic authority are clear.
