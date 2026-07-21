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

- [ ] Change the default scale spec first to request 64 bindings or 16
  declaration groups and exact 65-statement results.
- [ ] Run `jazz-parser-scale-spec` and confirm it fails because the three fixed
  profile runners do not yet accept a size.
- [ ] Parameterize all four generators and runners without changing their
  grammar templates.
- [ ] Add the shared assertion owner and keep complete-statistics equality in
  the twice-run smoke path.
- [ ] Run the smoke suite, record stable observations, and set measured ceilings
  above those values.
- [ ] Repeat the smoke suite to prove the observations are stable.
- [ ] Commit as `test: add fast hosted parser scale gate`.

## Task 2: Add independently runnable exhaustive components

- [ ] Add `full-parser-scale` as a default-disabled manual Cabal flag.
- [ ] Add one gated test entrypoint for each expression, declarations,
  control-flow, and operator profile.
- [ ] Make every full entrypoint run once at the existing size and reuse the
  landed full ceilings through the shared assertion owner.
- [ ] Run each component explicitly with the flag and confirm exact
  513-statement output, zero host operations, and bounded statistics.
- [ ] Run the four components together and confirm Cabal can address and report
  them independently.
- [ ] Commit as `test: gate exhaustive hosted parser scale profiles`.

## Task 3: Update the verification contract and close the follow-up

- [ ] Update README and performance guidance with the default smoke and
  explicit exhaustive commands.
- [ ] Update the operators/full-parity design and plan so historical twice-run
  evidence remains clear while ongoing verification uses smoke twice and full
  once.
- [ ] Run the default `all` matrix and confirm no gated full component appears.
- [ ] Run the exhaustive four-component matrix with `-ffull-parser-scale`.
- [ ] Run the warning-clean build, `cabal check`, queue/docs validators, and
  `git diff --check`.
- [ ] Confirm no parser, runtime, observation, fixture-corpus, queue, or legacy
  compiler path changed.
- [ ] Commit as `docs: document parser scale test tiers` and push PR `#117`.

## Completion Gate

- Routine `all` retains twice-run determinism for every grammar profile at 65
  statements.
- The exhaustive gate retains every landed 513-statement output and ceiling in
  four focused components.
- Full workloads run once and are not silently included in routine `all`.
- Documentation and actual Cabal commands agree.
- All focused, default, exhaustive, build, package, and repository checks pass.

