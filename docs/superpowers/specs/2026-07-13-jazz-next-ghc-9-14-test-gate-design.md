# Jazz-Next GHC 9.14 and Test-Gate Foundation Design

## Status

Approved for implementation on `2026-07-13`.

This is the design checkpoint for Batch 1 of
[`docs/jazz-improvement-backlog.md`](../../jazz-improvement-backlog.md). It
covers items 6 and 15 only: the GHC 9.14.1 migration, use of
`MultilineStrings` in tests, and replacement of the warning-named shell
mega-gate with Cabal-owned test execution and focused Haskell repository
audits.

## Decision Summary

The active `jazz-next` workspace will use one exact repository toolchain: GHC
9.14.1 with `base-4.22`. The root Nix flake will provide that toolchain and
build and test `jazz-next`, not either legacy compiler. Cabal will remain the
package and test orchestrator.

Hand-written multiline Jazz programs embedded in Haskell will use GHC's
`MultilineStrings` extension. Most instances are test fixtures, but the rule is
not test-directory-specific. Escaped newline literals and explicit string
assembly remain appropriate only when whitespace, line endings, indentation,
or source spans are the behavior under test. Generated inputs may use builders
and combinators.

The current `jazz-next/scripts/test-warning-config.sh` mega-gate will be
removed. Cabal will discover and execute every registered test suite. A new
Haskell repository-audit suite will own the small set of stable source-tree
policies that are not compiler behavior. The audit will not invoke shell tools
or parse the Cabal file to rediscover tests. Existing source-spelling guards
will be replaced by behavior tests where they express a real contract and
deleted where they merely freeze an implementation detail.

## Goals

- Make GHC 9.14.1 the reproducible compiler for active development, package
  checks, and CI-equivalent local verification.
- Update Cabal bounds to the package versions used by that compiler without
  widening them beyond the next incompatible major version.
- Preserve `Haskell2010` as the default language edition; the compiler upgrade
  does not imply a repository-wide language-edition change.
- Make multiline Jazz programs embedded anywhere in Haskell readable and
  establish an explicit exception policy for whitespace-sensitive tests.
- Give Cabal sole ownership of test-suite registration, selection, execution,
  and failure reporting.
- Move stable repository-format checks into ordinary Haskell tests with clear
  failures and fixture coverage.
- Remove the direct-`runghc` compatibility machinery from the active test path.
- Leave every later improvement batch with one documented, reliable baseline
  verification contract.

## Non-Goals

- No Jazz parser, analyzer, runtime, code-generation, or language-semantics
  change belongs to this batch.
- This batch does not switch to `GHC2024`, `GHC2021`, or another default
  language edition.
- It does not reorganize `.jz` sources under `jazz-next/jazz/`; that is Batch 2.
- It does not introduce realistic program fixtures, Criterion benchmarks,
  runtime counters, cost centres, eventlog markers, or heap-profile presets;
  those are Batch 3. The new toolchain must remain compatible with those later
  profiling additions.
- It does not perform the broad documentation consolidation reserved for Batch 7. Active instructions are updated now, while historical plans and closure
  evidence retain the commands that were true when they were written.
- It does not modify any file under `jazz-hs/` or `jazz2/`.

## Current State

The repository currently has two competing notions of the active Haskell
workspace:

- `jazz-next/jazz-next.cabal` describes the active compiler and 36 Cabal test
  suites, but its dependency bounds target GHC 9.4 and `base-4.17`.
- the root Nix flake also selects GHC 9.4, builds `jazz-hs`, and names that
  legacy package as the repository test check.

The Cabal test inventory is complete, but
`jazz-next/scripts/test-warning-config.sh` independently parses the Cabal file
with `awk`, invokes every suite with a custom `runghc.sh` wrapper, exercises the
wrapper through fake executables and environment manipulation, runs separate
stdlib-format scripts, and performs architecture checks with `rg`. Its name no
longer describes its role, and the duplicated test discovery can drift from
Cabal.

The architecture checks also mix three different kinds of policy:

1. observable compiler behavior that belongs in compiler tests;
2. stable repository layout or formatting rules that can justify a repository
   audit; and
3. implementation spellings whose absence does not prove the intended
   invariant.

That mixture is why the script became a mega-gate. The migration must classify
the checks instead of transliterating the shell into Haskell.

## Approaches Considered

### Keep and rename the shell mega-gate

This is the smallest edit, but it preserves duplicate test discovery, direct
`runghc` compilation semantics, shell portability work, and source-text
architecture assertions. A better name would not fix the ownership problem.

### Write a Haskell executable that launches every test suite

This removes shell syntax but recreates functionality Cabal already provides.
The executable would still need a second test inventory, subprocess handling,
and custom result aggregation. It would be a Haskell-shaped test runner rather
than a simpler test system.

### Use Cabal for tests and Haskell only for repository audits

This is the chosen approach. Cabal already owns component registration,
dependencies, compiler options, selection, and reporting. Haskell is useful
only for policies that inspect repository-owned files and benefit from typed,
portable validation and table-driven fixtures.

For the compiler installation, a local-only GHCup instruction was also
considered. GHCup remains a valid way for a contributor to obtain GHC 9.14.1,
but it cannot make the repository's package check reproducible. The pinned Nix
flake is therefore the repository contract; Cabal commands remain usable in a
matching non-Nix environment.

## Toolchain Ownership

### Exact compiler and package set

The repository pins GHC 9.14.1 rather than selecting an unversioned `latest`
attribute. The implementation will update `flake.nix` and `flake.lock` to a
Nixpkgs revision that contains the exact compiler and a compatible
`cabal-install`.

The default development shell must report GHC 9.14.1. Its active Haskell
package set must build `jazz-next/jazz-next.cabal`. Stack and a legacy-package
check are removed from the active shell/check contract because `jazz-next` is a
Cabal project and the legacy implementations are read-only references.

The flake's required package check will build and run the `jazz-next` tests.
It must not obtain its primary success by compiling `jazz-hs` or `jazz2`.

### Cabal bounds

`jazz-next.cabal` will target `base >= 4.22 && < 4.23`. Direct dependency
bounds will be raised to include the versions shipped with or selected for GHC
9.14.1, while retaining next-major upper bounds. The implementation plan must
record each bound change from the resolved package set rather than guessing at
versions.

All library, executable, and test components continue to inherit `Haskell2010`.
`MultilineStrings` is added to the shared test configuration. A production
module that genuinely contains an authored multiline Jazz program enables the
extension locally rather than expanding the extension surface of the whole
compiler library. Programmatically assembled sources such as generated builtin
declarations remain builders, not artificial multiline literals.

### One supported verification environment

Nix is the reproducible repository environment. A contributor may use GHCup or
another installer if `ghc --numeric-version` is `9.14.1` and Cabal resolves the
same supported dependency ranges. The repository does not maintain parallel
GHC 9.4 and 9.14 gates after this migration.

## Multiline Jazz Source Policy

Haskell source distinguishes authored Jazz examples from constructed inputs.

### Authored multiline programs

A Jazz program written by a person as a multiline Haskell literal uses
`MultilineStrings`. The literal should visually match the `.jz` program,
including meaningful indentation, and should not be assembled with `<>`, `++`,
`unlines`, or repeated `"\\n"` fragments merely to fit Haskell's old string
syntax.

This applies to complete programs, module sources, declaration groups, and
diagnostic fixtures whose content spans multiple source lines.

### Intentional explicit whitespace

Escaped newlines, string concatenation, and exact fragment lists remain
allowed when their explicit construction makes the tested contract clearer.
The allowed cases are tests of:

- whitespace or indentation significance;
- LF, CRLF, or other line-ending handling;
- empty, leading, or trailing lines;
- exact line and column spans; or
- lexer/parser behavior at a boundary created by an individual whitespace
  character.

These exceptions should be locally evident from the test name or a short
comment. They are not a reason to keep unrelated program fixtures in the old
form.

### Generated inputs and external fixtures

Stress tests may use `Text.replicate`, builders, folds, or other combinators
when the input is defined by size rather than authored content. Batch 1 does not
move substantial programs to external files. Batch 3 will prefer checked-in
`.jz` fixtures for realistic, reusable, and multi-module programs.

The migration is reviewed semantically rather than enforced by a global regex:
a text search cannot reliably distinguish whitespace tests, generated input,
and ordinary program literals. New review guidance and nearby test structure
are a better guard than another source-spelling audit.

## Cabal-Owned Test Gate

### Canonical commands

The canonical active test command is:

```sh
cabal test --project-dir=jazz-next all --test-show-details=failures
```

The complete release-style verification also builds all declared components:

```sh
cabal build --project-dir=jazz-next all
cabal test --project-dir=jazz-next all --test-show-details=failures
```

A focused suite is run through its Cabal component name, for example:

```sh
cabal test --project-dir=jazz-next parser-foundation-spec --test-show-details=failures
```

No script parses `jazz-next.cabal` to reconstruct the suite inventory. Adding a
test suite in Cabal automatically adds it to the `all` gate.

### Repository audit suite

A new `repository-audit-spec` test suite will be registered in Cabal and use
the existing `JazzNext.TestHarness`. It is an ordinary in-process Haskell test,
not a subprocess runner.

The suite may inspect checked-in files for stable repository policies. Its
initial responsibility is stdlib source formatting:

- `Prelude.jz` remains exempt from the module-brace rule while it is a special
  bundled prelude source;
- other current stdlib modules have a top-level module header ending in `{`;
- the closing `}` is the final non-blank line; and
- non-blank body lines use the required module-body indentation.

The validator will be a pure function over path/text inputs. Table-driven
in-memory fixtures will cover valid and invalid headers, closing braces, body
indentation, and Prelude exemption. A thin I/O layer will enumerate the real
source directory, sort paths for deterministic reporting, read files, and
render all violations through the normal test harness.

Repository-root discovery must be deterministic from either the repository
root or the `jazz-next` package root and must fail with an actionable message if
the expected `jazz-next.cabal` marker cannot be found. It must not call `find`,
`awk`, `rg`, `bash`, or another process.

### Classification of the old architecture guards

Each `rg` guard in `test-warning-config.sh` receives one of these dispositions
during implementation:

- **Behavior contract:** keep or add a focused compiler test that would fail if
  the prohibited architecture changed observable semantics.
- **Package/repository contract:** add a Haskell audit only when the property is
  stable and externally meaningful, such as the active library remaining
  private.
- **Implementation spelling:** remove the guard. Searches for type names,
  record fields, deleted module names, or a particular parser expression do
  not establish the intended architecture and make safe refactors harder.

The implementation plan must include a disposition table for every existing
guard. No guard may disappear accidentally, and none may be ported merely to
preserve line-for-line parity with the shell script.

## Script and Documentation Migration

Once Cabal and the repository-audit suite cover the accepted responsibilities,
the following active compatibility scripts are removed:

- `jazz-next/scripts/test-warning-config.sh`;
- `jazz-next/scripts/test-check-stdlib-format.sh`;
- `jazz-next/scripts/check-stdlib-format.sh`; and
- `jazz-next/scripts/runghc.sh`.

Cabal component commands replace direct `runghc` for focused tests. `cabal run
--project-dir=jazz-next jazz-next -- ...` replaces invoking the CLI's `Main.hs`
through the wrapper.

Active instructions in `jazz-next/README.md`, `docs/execution/README.md`,
`docs/execution/blocker-contracts.md`, and current tooling specifications will
be updated in the same implementation. Historical implementation plans,
accepted-design evidence, and `docs/execution/done-archive.md` retain their old
commands because those commands document how earlier work was actually
verified.

If an active automation outside those documents still invokes a removed
script, it must be migrated before deletion. A repository search is part of the
implementation verification.

## Failure and Reporting Behavior

- A compiler or dependency mismatch fails during Cabal/Nix resolution rather
  than falling through to another GHC on `PATH`.
- A failed Cabal suite reports the component name and the existing named test
  failure output.
- Repository audit failures report every violating relative path and rule in a
  deterministic order in one run.
- Failure to locate the repository/package root is a repository-audit failure,
  not an empty successful scan.
- An empty or missing stdlib directory is a failure.
- The gate never silently skips a Cabal test suite because a second inventory
  was not updated.

## Verification Contract

Implementation is complete only after fresh evidence establishes all of the
following:

1. the pinned development shell reports GHC 9.14.1 and a compatible Cabal;
2. Cabal resolves `base-4.22` and all direct bounds without jailbreaks;
3. `cabal build --project-dir=jazz-next all` succeeds;
4. `cabal test --project-dir=jazz-next all --test-show-details=failures`
   succeeds, including `repository-audit-spec`;
5. the Nix `jazz-next` check succeeds and no required flake check builds a
   legacy compiler as the active package;
6. the repository-audit validator's valid and invalid table tests succeed;
7. all hand-written multiline Jazz programs embedded in Haskell use
   `MultilineStrings`, with explicit constructions confined to locally evident
   whitespace/span tests and generated inputs;
8. no active script, automation, README, execution guide, blocker contract, or
   tooling specification invokes a removed wrapper;
9. `jazz-hs/` and `jazz2/` have no changes; and
10. repository documentation checks and `git diff --check` succeed.

If Nix cannot be executed in the implementation environment, the toolchain
migration is not declared fully verified merely because a non-Nix Cabal run
passes. The blocked Nix evidence must be reported explicitly and completed in
an environment that can evaluate and build the flake.

## Consequences

The immediate cost is a broad but mechanical rewrite of embedded test source
and a one-time package-set migration. Contributors with only GHC 9.4 will need
to enter the Nix shell or install GHC 9.14.1.

In return, the repository loses a bespoke test runner, fake-executable shell
tests, and duplicate component inventory. Individual tests use the same Cabal
configuration as the full gate, repository policies gain portable unit tests,
and later batches can rely on readable fixtures and one reproducible toolchain.

The stricter toolchain pin intentionally favors a clear active compiler over a
compatibility matrix. Supporting multiple GHC releases can be reconsidered
when Jazz has users who benefit from that matrix; it is unnecessary overhead
for the current compiler workspace.
