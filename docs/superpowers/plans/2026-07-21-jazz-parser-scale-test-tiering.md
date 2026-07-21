# Jazz Parser Scale Test Tiering Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use
> `superpowers:executing-plans` to implement this plan task-by-task. Steps use
> checkbox syntax for tracking. Per maintainer preference, this plan specifies
> outcomes, interfaces, tests, and commands without embedding implementation
> bodies.

**Goal:** Keep deterministic parser-scale coverage in routine testing while
moving the existing 513-statement ceilings into focused opt-in components.

**Architecture:** Parameterize the four existing scale generators, centralize
their assertions, run 65-statement profiles twice in the default suite, and
gate four single-run 513-statement suites behind `full-parser-scale`.

**Tech Stack:** GHC 9.14.1, Cabal manual flags and test components, the existing
Jazz runtime-observation API, and the Nix-pinned development environment.

**Design checkpoint:**
[`2026-07-21-jazz-parser-scale-test-tiering-design.md`](../specs/2026-07-21-jazz-parser-scale-test-tiering-design.md)

**Status:** Complete on `2026-07-21`.

## Global Constraints

- Preserve the four existing full-scale source shapes, outputs, and ceilings.
- Keep twice-run determinism in the default smoke tier.
- Do not add wall-clock thresholds or benchmark semantics to correctness tests.
- Do not change parser, runtime, observation, fixture-corpus, queue, or legacy
  compiler behavior.
- Run all compiler and test commands through the Nix-pinned environment.
- Commit each independently reviewable milestone.

## File and Responsibility Map

| File | Responsibility |
| --- | --- |
| `jazz-next/test/JazzNext/Compiler/Bootstrap/JazzParserScale.hs` | Parameterize the four generated workloads. |
| `jazz-next/test/JazzNext/Compiler/Bootstrap/JazzParserScaleAssertions.hs` | Own common scale limits and result assertions. |
| `jazz-next/test/JazzNext/Compiler/Bootstrap/JazzParserScaleSpec.hs` | Run twice-repeated 65-statement smoke profiles. |
| `jazz-next/test/JazzNext/Compiler/Bootstrap/JazzParserScaleFull*Spec.hs` | Run one preserved 513-statement profile per component. |
| `jazz-next/jazz-next.cabal` | Register the default suite, manual flag, and gated exhaustive components. |
| `jazz-next/README.md`, `jazz-next/PERFORMANCE.md` | Document routine and exhaustive commands. |
| Operators/full-parity design and plan | Keep the landed evidence and current verification contract accurate. |

## Task 1: Parameterize the scale runners and land deterministic smoke coverage

- [x] Change the default scale spec first to request 64 bindings or 16
  declaration groups and exact 65-statement results.
- [x] Run `jazz-parser-scale-spec` and confirm it fails because the three fixed
  profile runners do not yet accept a size.
- [x] Parameterize all four generators and runners without changing their
  grammar templates.
- [x] Add the shared assertion owner and keep complete-statistics equality in
  the twice-run smoke path.
- [x] Run the smoke suite, record stable observations, and set measured ceilings
  above those values.
- [x] Repeat the smoke suite to prove the observations are stable.
- [x] Commit as `test: add fast hosted parser scale gate`.

## Task 2: Add independently runnable exhaustive components

- [x] Add `full-parser-scale` as a default-disabled manual Cabal flag.
- [x] Add one gated test entrypoint for each expression, declarations,
  control-flow, and operator profile.
- [x] Make every full entrypoint run once at the existing size and reuse the
  landed full ceilings through the shared assertion owner.
- [x] Compile all four components explicitly with the flag without executing
  the exhaustive workloads.
- [x] Confirm Cabal can address each component independently and that the
  default-disabled flag keeps them out of routine `all`.
- [x] Commit as `test: gate exhaustive hosted parser scale profiles`.

## Task 3: Update the verification contract and close the follow-up

- [x] Update README and performance guidance with the default smoke and
  explicit exhaustive commands.
- [x] Update the operators/full-parity design and plan so historical twice-run
  evidence remains clear while ongoing verification uses smoke twice and full
  only when explicitly requested for niche diagnosis.
- [x] Run the default `all` matrix and confirm no gated full component appears.
- [x] Compile-check all four exhaustive components with
  `-ffull-parser-scale`; do not execute them in routine verification.
- [x] Run the warning-clean build, `cabal check`, queue/docs validators, and
  `git diff --check`.
- [x] Confirm no parser, runtime, observation, fixture-corpus, queue, or legacy
  compiler path changed.
- [x] Commit as `docs: document parser scale test tiers`; leave the PR push to
  the controller.

## Completion Evidence

Completed on `2026-07-21` across `bf04918`, `354f51f`, and this documentation
closeout. The default smoke suite ran all four 65-statement profiles twice with
identical complete statistics and zero host operations:

| Profile | Evaluator transitions | Applications | List cells | Maximum continuation depth |
| --- | ---: | ---: | ---: | ---: |
| Expression | 2,701,565 | 326,516 | 13,526 | 165 |
| Declarations | 1,147,204 | 137,013 | 7,474 | 173 |
| Control flow | 5,187,384 | 627,007 | 26,321 | 200 |
| Operator | 6,140,452 | 740,815 | 22,899 | 220 |

The Nix-pinned routine
`cabal test --project-dir=jazz-next all --test-show-details=failures` command
exited `0`. Its complete build/run log contains `jazz-parser-scale-spec` and no
`jazz-parser-scale-full-*` component. The observed wall-clock time was 77.10
seconds; this is reporting evidence only and is not a correctness threshold.

The Nix-pinned compile-only command selected
`-ffull-parser-scale -fdevelopment` plus all four
`test:jazz-parser-scale-full-*` targets and exited `0` after preprocessing and
building every target. No exhaustive component was executed. The warning-clean
development build, `cabal check`, queue validator, docs validator, and
`git diff --check` also passed. The closeout diff contains only this plan, the
operators/full-parity design and plan, `jazz-next/README.md`, and
`jazz-next/PERFORMANCE.md`; parser, runtime, observation, fixture-corpus, queue,
and legacy compiler paths are unchanged.

## Completion Gate

- Routine `all` retains twice-run determinism for every grammar profile at 65
  statements.
- The exhaustive gate retains every landed 513-statement output and ceiling in
  four focused components.
- Full workloads run once and are not silently included in routine `all`.
- Full workloads are not executed by this follow-up; they are reserved for an
  explicit maintainer request or scale-regression investigation.
- Documentation and actual Cabal commands agree.
- Default correctness, exhaustive-target compile-only, warning-clean build,
  package, and repository checks pass; exhaustive runtime remains manual-only.
