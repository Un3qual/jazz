# Jazz Parser Scale Test Tiering Design

## Status

Discussion-approved on `2026-07-21` as a review follow-up for PR `#117`.
This design changes how the landed hosted-parser scale evidence is scheduled;
it does not weaken the parser behavior, parity, or full-scale ceilings proved by
the operators/full-parity batch.

## Problem

`jazz-parser-scale-spec` currently runs four production-shaped hosted-parser
profiles sequentially. Every profile contains 513 parsed statements and runs
twice so output and complete runtime statistics can be compared. The suite
therefore executes approximately 243.6 million Jazz evaluator transitions,
while each invocation also rebuilds and evaluates the bundled prelude and
imported compiler module graph. Because the component is unconditionally
registered, every `cabal test all` run pays the exhaustive cost.

The operator and control-flow profiles account for approximately 74.5 percent
of the recorded evaluator work. The suite is valuable evidence, but an
hour-scale default feedback loop is the wrong scheduling policy for it.

## Goal

Keep deterministic, production-shaped hosted-parser scale protection in the
default test matrix while moving the existing 513-statement ceilings into an
explicit exhaustive gate that is independently runnable by grammar family.

The routine gate should be small enough for ordinary development and PR
verification. The exhaustive gate should continue to catch stack, termination,
host-operation, and semantic-work regressions at the landed scale.

## Design

### Parameterized profile generation

The four profile runners accept their workload size explicitly. Expression,
control-flow, and operator profiles receive a binding count. The declarations
profile receives the number of signature/binding/data/import groups.

Two named configurations own the only supported sizes:

- smoke: 64 bindings or 16 declaration groups, producing 65 parsed statements;
- full: 512 bindings or 128 declaration groups, producing 513 parsed
  statements.

The smoke size retains every generated grammar shape multiple times. Callers
do not duplicate generator bodies or edit source templates to select a tier.

### Default deterministic smoke suite

`jazz-parser-scale-spec` remains an ordinary, always-buildable Cabal test. It
runs all four 65-statement profiles twice and requires:

- successful compilation and runtime completion;
- the exact 65-statement output;
- identical output and complete runtime statistics across both runs;
- zero host operations; and
- measured smoke-scale ceilings for evaluator transitions, applications, list
  cells, and maximum continuation depth.

This suite owns determinism. Its limits are measured after the parameterized
runners exist and then tightened above the stable observations.

### Opt-in exhaustive suites

A manual Cabal flag named `full-parser-scale` defaults to disabled. When
enabled, it makes four test components buildable:

- `jazz-parser-scale-full-expression-spec`;
- `jazz-parser-scale-full-declarations-spec`;
- `jazz-parser-scale-full-control-flow-spec`; and
- `jazz-parser-scale-full-operator-spec`.

Each component runs its 513-statement profile once. It requires the exact
output, successful termination, zero host operations, and the already-landed
full-scale ceilings. Repeating the full workload is unnecessary because the
same generators and result assertions receive twice-run determinism coverage
at smoke size.

Separate components provide focused retries and allow Cabal to schedule them
independently when parallel test execution is available. Correctness must not
depend on parallel scheduling.

### Shared assertions

A bootstrap-test helper owns scale limits plus the common success,
observation, output, host-operation, and ceiling assertions. It exposes one
single-run assertion and one deterministic-pair assertion. Profile entrypoints
remain small and declarative.

### Command contract

Routine verification remains:

```bash
cabal test --project-dir=jazz-next all --test-show-details=failures
```

Exhaustive verification is explicit:

```bash
cabal test --project-dir=jazz-next -ffull-parser-scale \
  jazz-parser-scale-full-expression-spec \
  jazz-parser-scale-full-declarations-spec \
  jazz-parser-scale-full-control-flow-spec \
  jazz-parser-scale-full-operator-spec \
  --test-show-details=failures
```

Parser or runtime changes should run the exhaustive command before merge.
Release and scheduled validation may run it independently of the routine
matrix. The repository does not introduce a wall-clock pass/fail threshold.

## Alternatives Considered

### Keep the exhaustive suite in default `all` and only parallelize it

This preserves one command but still consumes the same compute, depends on
machine concurrency for acceptable feedback, and remains slow on constrained
CI workers. Parallel components are useful for the exhaustive tier, not a
substitute for tiering.

### Permanently shrink the existing scale profiles

This makes routine testing fast but discards the 513-statement stack and work
regression boundary. It is not acceptable because the hosted parser executes
inside the stage-0 Jazz interpreter and has previously needed large traversal
evidence.

### Move the exhaustive checks into benchmarks

Benchmarks own physical timing and environment metadata. These checks assert
deterministic semantic counts and correctness, so they remain tests behind an
explicit flag rather than becoming timing benchmarks.

## Scope

This follow-up changes only bootstrap scale-test generation, assertions, Cabal
test registration, and directly affected test documentation. It does not
change parser grammar, runtime semantics, observation counters, scale ceilings,
the canonical fixture corpus, queue state, or any legacy compiler path.

## Acceptance Criteria

- Default `cabal test all` runs all four 65-statement profiles twice and does
  not build or run any full-scale component.
- Smoke observations are identical across repeated runs, have zero host
  operations, and remain below measured limits.
- Enabling `full-parser-scale` exposes four independently addressable
  components.
- Each full component preserves the existing 513-statement output and landed
  ceilings with zero host operations.
- The exhaustive components do not repeat the full workload.
- README, performance guidance, and the operators/full-parity verification
  record distinguish routine smoke from exhaustive scale verification.
- Warning-clean build, `cabal check`, queue/docs validators, and
  `git diff --check` pass.

