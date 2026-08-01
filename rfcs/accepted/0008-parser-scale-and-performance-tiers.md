# RFC 0008: Parser scale and performance tiers

Status: Accepted
Date: 2026-07-31
Supersedes: Performance, test-gate, parser-scale, and CI decisions dated 2026-07-13, 2026-07-14, 2026-07-21, and 2026-07-31.

## Decision

Jazz separates deterministic correctness and semantic-work budgets from
machine-dependent timing and profiling. Correctness and unexplained semantic-
budget regressions are gating. Wall-clock time, physical allocation, heap
residency, and sampled profiles are advisory evidence unless a separate
reproducible regression has been validated.

Hosted-parser scale has two deterministic tiers:

- The default `jazz-parser-scale-spec` runs expression, declaration,
  control-flow, and operator profiles twice at 65 parsed statements. It checks
  exact output, termination, byte-identical complete runtime statistics, zero
  host operations, and measured upper bounds for evaluator transitions,
  applications, list construction, and continuation depth.
- Four full components retain the 513-statement profiles behind the
  default-disabled `full-parser-scale` Cabal flag. Each runs once, checks exact
  output and the landed semantic ceilings, and can be selected independently.

The full profiles are correctness tests, not benchmarks. They are excluded
from ordinary local and pull-request test matrices. Their generators and
ceilings remain versioned so a scale regression can be reproduced without
making every feedback loop pay the exhaustive cost.

The production-shaped program corpus is shared by correctness and measurement.
Its manifest owns exact results and deterministic upper budgets. Running less
semantic work is not a failure. Budget changes are reviewed source changes and
are never rewritten automatically from a benchmark run.

Physical measurement has two independent tools:

- `jazz-bench` measures parse/lower, analysis, module preparation, runtime, and
  whole-program boundaries. Durable results include compatible environment,
  toolchain, corpus, build, RTS, and machine metadata.
- Separate stage and hotspot Cabal project files enable GHC time, allocation,
  eventlog, and heap profiling. Profiling builds remain isolated from normal
  build artifacts.

Runtime statistics and deterministic Jazz semantic profiles are evaluator-
owned observation modes. Semantic profiles use evaluator transitions as a
logical clock and can be byte-identical for identical executions; they do not
claim to measure time or bytes. GHC profiles measure the current Haskell
implementation and are intentionally machine- and build-dependent.

Repository CI uses four scheduling tiers:

1. **Pull request:** warning-clean focused compiler/runtime/CLI/contract tests,
   repository and source audits, documentation and website checks, and user
   example smoke tests. The target is ten minutes or less. This tier excludes
   `cabal bench`, full parser scale, profiling builds, and the complete
   production corpus.
2. **Main:** the complete ordinary Cabal test matrix, default deterministic
   parity and parser smoke suites, package checks, and repository validation.
   It still excludes full parser scale, benchmarks, and profiling.
3. **Extended:** weekly and manual runs execute full parser scale, the complete
   corpus, profiling builds, benchmarks, and repeated determinism checks, and
   retain useful artifacts.
4. **Release:** release candidates and version tags run functional and
   extended checks, clean package and documentation builds, and distributable
   artifact verification. Benchmark completion is required, but an isolated
   timing percentage does not block release by itself.

Documentation-only pull requests run documentation and website validation
without rebuilding the compiler. Superseded pull-request runs are cancelled,
and dependency caches must not weaken correctness or artifact isolation.

## Context

The complete hosted-parser scale suite performs hundreds of millions of
interpreter transitions and once made every `cabal test all` run take roughly
an hour. That evidence is useful, but scheduling it on every edit turns a
targeted scale guard into poor routine feedback.

Similarly, timing on shared runners is sensitive to machine load, build mode,
and sampling. Jazz already has stable semantic counters and deterministic
corpus expectations that can gate structural work without pretending physical
measurements are portable.

## Consequences

- Ordinary development retains representative scale and determinism coverage
  without running exhaustive workloads.
- Weekly/manual and release workflows must provide explicit artifact roots and
  environment labels for benchmark and profile output.
- A timing change prompts investigation and same-environment reproduction; it
  does not fail solely on a shared-runner percentage.
- Semantic budgets must name their metric, observed value, and ceiling when
  they fail, making structural regressions actionable.
- Performance-oriented changes can use one corpus across correctness,
  deterministic observation, benchmark, and profiling layers without
  confusing those layers' meanings.
