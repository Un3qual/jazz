# Jazz-Next Program Corpus, Benchmarks, and Observability Design

## Status

Approved on `2026-07-14`; implementation pending.

This is the design checkpoint for Batch 3 of
[`docs/jazz-improvement-backlog.md`](../../jazz-improvement-backlog.md). It
covers items 2, 3, and 4 only: realistic Jazz programs, a benchmark suite,
runtime-owned semantic statistics, deterministic Jazz execution profiles, and
separate GHC profiling for the Haskell implementation.

## Decision Summary

Batch 3 uses a layered measurement architecture:

1. a shared, manifest-backed program corpus under `jazz-next/programs/`;
2. runtime-owned semantic observation that is independent of benchmarking;
3. a `tasty-bench` executable for machine-dependent stage and end-to-end
   measurements; and
4. separate GHC profiling builds for Haskell time, allocation, heap, and
   eventlog analysis.

The same substantial `.jz` programs serve correctness tests and benchmarks.
The ordinary test gate checks their results and deterministic runtime-work
budgets. Timing, physical allocation, and heap results include environment
metadata and remain diagnostic rather than portable CI gates.

The public runtime flags are:

```text
--runtime-stats[=human|json]
--runtime-profile=PATH
```

Both flags require `--run` and may be combined. Statistics go to stderr. The
profile flag writes a deterministic, Speedscope-compatible evented JSON file
whose logical clock counts Jazz evaluator transitions. It is distinct from the
GHC JSON profile, which measures sampled Haskell time and physical allocation.

## Goals

- Exercise modules, generic ADTs, patterns, recursion, inference,
  capabilities, text, lists, and deterministic runtime behavior in
  production-shaped Jazz programs.
- Keep substantial programs as external `.jz` sources instead of embedding
  them in Haskell strings.
- Use one corpus for correctness, examples, deterministic work checks, stage
  benchmarks, and end-to-end benchmarks.
- Measure parsing/lowering, analysis and inference, module preparation,
  evaluation, and the whole load-to-result pipeline independently.
- Make timing and physical-allocation comparisons reproducible enough to be
  useful without pretending they are portable across machines or builds.
- Make Jazz runtime work observable through stable semantic counters.
- Produce a deterministic Jazz-level flame graph attributed to Jazz call
  stacks rather than Haskell evaluator functions.
- Preserve GHC profiling for stage/sub-stage time, physical allocation, heap
  residency, cost-centre stacks, and eventlog analysis.
- Keep measurement disabled by default and keep program stdout unchanged.

## Non-Goals

- No Jazz syntax, type-system rule, module rule, or runtime semantic behavior
  changes merely to create benchmark programs.
- No LLVM/native backend benchmark is introduced. Stage-0 interpreter results
  remain clearly labeled and separate from future native compilation and
  execution results.
- Jazz semantic construction counts are not described as physical byte
  allocations or garbage-collector measurements.
- No wall-clock sampler is added to the Jazz semantic profiler.
- No hosted performance dashboard, cross-machine timing threshold, or noisy
  wall-clock CI failure gate is added.
- No bundled classic FlameGraph renderer or SVG generator is added. Speedscope
  is the interactive flame-graph target.
- No native-runtime allocator instrumentation is designed before that runtime
  exists.
- Nothing under `jazz-hs/` or `jazz2/` is modified.

## Approaches Considered

### Entirely custom benchmark harness

A custom executable could own timing loops, statistics, result formats,
baseline comparison, filtering, and reporting. It would provide maximum
control, but most of that work is generic benchmark infrastructure rather than
Jazz functionality. Reimplementing it would increase maintenance and make the
statistical behavior harder to trust. This approach is rejected.

### Criterion-centred harness

Criterion provides mature statistical analysis and reports, but it has a
substantially larger dependency surface and still cannot replace Jazz-specific
corpus metadata, deterministic budgets, semantic counters, or profiles. Its
extra analysis is not needed for the first suite. This approach is rejected.

### Project-owned corpus and observation with `tasty-bench` timing

This is the chosen approach. Jazz owns the programs, stage boundaries,
semantic metrics, profile format, metadata compatibility checks, and
deterministic regression policy. `tasty-bench` owns repeated physical timing,
CSV results, baseline comparison, filtering, and optional RTS allocation
reporting. It supports GHC 9.14 and can be replaced later without changing the
runtime-observation API or corpus format.

## Architecture and Ownership

The batch has four independently understandable units.

### Program corpus

`jazz-next/programs/` owns substantial programs used by both tests and
benchmarks. It is deliberately outside `jazz-next/jazz/stdlib` and
`jazz-next/jazz/compiler`: corpus sources are development inputs, not shipped
stdlib or hosted-compiler modules.

This shared root supersedes the earlier backlog placement sentence that left
test fixtures and benchmark inputs under separate ownership trees. Focused
parser and semantic fixtures remain in `jazz-next/test/`; only substantial
multi-module programs move into the shared corpus.

### Corpus support

A project-internal Haskell support module loads and validates the manifest,
resolves case paths, runs cases, and exposes stage-ready inputs. It is compiled
only into the corpus test and benchmark components. The production compiler
library does not depend on corpus metadata or `tasty-bench`.

### Runtime observation

The evaluator accepts an observation request and owns all changes to semantic
counters, logical time, call stacks, and profile events. The driver selects the
mode and transports the completed report; it does not infer metrics from
compiler data or render counters while evaluation is in progress.

### Physical measurement and profiling

The benchmark component measures ordinary optimized builds. Dedicated Cabal
profiling presets build compatible profiling versions of the library and
executable for GHC cost-centre, heap, and eventlog analysis. Neither facility
changes the meaning of runtime semantic metrics.

## Shared Program Corpus

### Layout and manifest

The corpus uses this structure:

```text
jazz-next/programs/
  README.md
  corpus.json
  identifier-classifier/
    Main.jz
    expected.stdout
  expression-evaluator/
    Main.jz
    ...
  ...
```

`corpus.json` has a versioned top-level schema and one entry per case. Each
entry contains:

- a stable case identifier and directory;
- the entry source and module root;
- an expected termination category and expected stdout file;
- a `fast` or `full` workload class;
- feature tags from a validated vocabulary;
- the compiler and runtime benchmark groups in which it participates; and
- deterministic upper budgets for evaluator steps, applications, and maximum
  continuation depth, with optional budgets for other counters.

Expected output remains in a separate file so multiline output is readable and
does not become escaped JSON. Source paths are relative to the case directory.
The loader rejects absolute paths and traversal outside the corpus root.

The validator reports all manifest violations in stable case/path order. It
rejects an unknown schema version, duplicate identifiers, duplicate
directories, unknown workload or feature tags, missing entry/output files,
paths outside the corpus root, and benchmark groups that have no corresponding
harness registration.

### Initial program set

The initial corpus contains at least these production-shaped workloads:

- an identifier/token classifier derived from the Jazz-authored lexer work,
  emphasizing text and list processing;
- a recursive expression evaluator using ADTs, nested patterns, and explicit
  error results;
- generic tree transformations across modules using higher-order mapping and
  folding;
- a dependency planner represented by an association-list graph;
- a deterministic capability-driven workflow; and
- one larger mini-front-end that combines multiple modules and several of the
  preceding structures.

The implementation may split a workload into small and large input variants,
but it must preserve the feature coverage above. Inputs are deterministic and
do not depend on wall-clock time, randomness, network access, or mutable
external state.

### Test use

The Cabal-registered corpus test runs each case through the real module loader,
compiler pipeline, and runtime, then checks termination and stdout exactly.
`fast` cases support quick smoke use; the full registered suite includes all
cases. Focused compiler test suites remain independent, so running one parser
or inference suite does not execute the corpus.

The same run obtains runtime statistics and checks every declared deterministic
budget. A budget is an upper limit, not an exact snapshot: reduced work does
not fail. Exceeding a budget reports the case, metric, limit, actual value, and
percentage increase. Budget updates are ordinary reviewed edits to
`corpus.json`; no test silently rewrites them.

## Benchmark Harness

### Cabal component

A dedicated `jazz-next-bench` Cabal benchmark depends on
`jazz-next-internal`, the corpus support module, and `tasty-bench`. It does not
become part of the `jazz-next` executable or runtime API.

The benchmark tree mirrors compiler ownership and includes these groups:

- `parse-lower`: source text through the lowered AST;
- `analysis`: lowered program through static analysis and type inference;
- `module-preparation`: module discovery, resolution, and runtime-ready program
  construction;
- `runtime`: evaluation of an already prepared program; and
- `whole-program`: entry path through final runtime result.

Stage-specific setup is performed outside the timed action. For example, the
runtime group receives an already prepared program, while the whole-program
group starts from filesystem paths. Each group uses an explicit stage-specific
forcing function so lazy Haskell evaluation cannot shift meaningful work into
setup, output rendering, or a later benchmark. Broad `NFData` instances are
not added to compiler types solely for benchmarking when a narrower forcing
boundary expresses the completed stage more accurately.

The harness has a smoke mode that executes every registered case/group once
without attempting statistically meaningful timing. This catches stale
registrations and stage failures quickly. Normal benchmark runs remain
single-threaded unless a future benchmark explicitly measures parallel work.

### Results and metadata

Recorded physical results live under an ignored
`jazz-next/benchmark-results/` root. A result set contains:

```text
results.csv
environment.json
```

Creating a recorded result set requires an explicit
`--environment-label=NAME`. An unrecorded interactive run and smoke mode do not
require one. This prevents unrelated developer machines from silently sharing
an ambiguous default identity.

The versioned environment document records at least:

- Git commit and dirty-worktree status;
- corpus schema version and selected cases;
- GHC, Cabal, and package versions;
- operating system and architecture;
- an explicit environment/machine label;
- optimized or profiling build mode;
- RTS capabilities and relevant RTS arguments;
- benchmark arguments and time mode; and
- run timestamp.

Comparing result sets requires compatible corpus, GHC, build-mode, time-mode,
and environment labels. The tool reports incompatibilities instead of
presenting a misleading percentage. A caller may override an incompatibility
only explicitly, and the comparison output records that override.

`tasty-bench` CSV baselines and slowdown reports are developer-facing evidence.
They may be uploaded as CI artifacts or attached to a review, but repository
tests never fail solely because a shared runner was slower. Deterministic Jazz
work budgets are the performance-related CI gate.

## Runtime Observation Model

### Public modes

The runtime represents observation as data rather than scattered booleans. The
four effective modes are:

- disabled;
- aggregate statistics only;
- semantic profile only; and
- statistics plus semantic profile.

Disabled evaluation does not increment counters or allocate profile events. An
instrumented transition performs one mode check and updates only the requested
state. Measurement overhead is expected when observation is enabled and must
not be included in uninstrumented benchmark groups unless that overhead is the
subject of the benchmark.

### Runtime statistics

The version-1 statistics object contains termination state and semantic
counters for:

- evaluator transitions and forced values;
- total applications and applications of closures, builtins, operators,
  constructors, and methods;
- current and maximum continuation depth;
- closures created, bindings captured, and maximum capture width;
- list cells, tuples, and saturated ADT values constructed;
- pattern attempts, successful matches, and bindings introduced;
- builtin calls and host operations; and
- deferred-binding cache hits, misses, and recursive evaluations.

All totals use non-negative integer types large enough for long-running
programs. Maximum values update at the point the corresponding runtime state
changes. Definitions document precisely what increments each counter; tests
assert semantic behavior rather than searching runtime source for increment
expressions.

These counters describe logical Jazz work. For example, one logical list-cell
construction may allocate several Haskell objects, be optimized differently in
a later runtime, or eventually map to a native allocation of a different size.
Only GHC/native-runtime tools may claim physical byte counts.

### Jazz semantic profile

The runtime maintains a logical clock that advances once for every evaluator
machine transition. It also maintains a semantic call stack separate from the
evaluator continuation stack. A root frame spans the complete runtime attempt
so work outside a callable remains visible. Applying a callable opens a child
frame and returning from that activation closes it at the current
logical-clock value.

Named frames use fully qualified Jazz binding names. Curried lambda stages use
stable parameter/depth suffixes so immediately nested unary closures remain
distinguishable without pretending they are independent top-level functions.
Anonymous closures inherit a stable label from their enclosing binding and
lambda position. Builtins, operators, constructors, methods, and host
operations use visibly categorized frame names.

The writer emits the Speedscope evented profile schema with value unit `none`:
the values are deterministic work units, not seconds. The profile contains no
timestamp, Git commit, absolute path, random identifier, or unstable map order,
so two identical successful runs produce byte-identical output.

If runtime evaluation fails, the observer synthetically closes remaining open
frames at the final logical time and marks the profile name as incomplete with
the termination category. The result remains structurally valid and useful for
finding work performed before the error.

## CLI Contract and Failure Handling

`--runtime-stats`, `--runtime-stats=human`, and
`--runtime-stats=json` are equivalent observation requests with the first two
selecting the human renderer. Any other value is a CLI usage error.

Statistics are emitted after runtime completion in a stable field order. Human
output is multiline. JSON output is one compact, versioned object on its own
final stderr line. If runtime diagnostics precede partial JSON statistics, a
collector can still parse the final line without treating the entire stderr
stream as one JSON document.

`--runtime-profile=PATH` writes the deterministic Jazz profile. The path must
be non-empty and have a writable parent. The flag selects the Speedscope format
without a second format option; another format would require a demonstrated
consumer and a separate design amendment.

Both flags require `--run`. They may be used together in one evaluation so the
profile and counters describe exactly the same execution. Ordinary Jazz stdout
is unchanged.

When compilation fails, runtime observation never starts and neither runtime
artifact is emitted. When runtime starts and fails, requested partial
statistics and profiles are emitted after the diagnostic. A requested profile
is written to a sibling temporary file and atomically renamed after encoding
and flush succeed. A create, encode, flush, or rename failure removes the
temporary file where possible, preserves any existing destination, produces a
structured CLI diagnostic, and makes the command fail.

## GHC Profiling

### Stable stage catalogue

A small compiler profiling module owns a `CompilerStage` catalogue and its
stable rendered names. The catalogue covers:

- source loading and module discovery;
- lexing, parsing, and lowering;
- module resolution;
- static analysis;
- type inference, constraint solving, and capability solving;
- runtime preparation;
- evaluation and host operations; and
- diagnostic rendering where it is a measurable part of the requested work.

Benchmark group names and eventlog marker names derive from this catalogue.
Eventlog instrumentation emits paired begin/end user markers. Manual `SCC`
annotations use the same literal namespace around the phase boundary
functions. The set is intentionally small: GHC warns that excessive
source-level cost centres can inhibit optimization and distort the program
being measured.

Phase boundary wrappers fully evaluate the documented result for that phase
before recording the end marker. This keeps eventlog durations and benchmark
boundaries aligned instead of marking only construction of a lazy result.

### Profiling presets

Two Cabal project presets live beside `jazz-next/cabal.project`:

- the stage preset enables profiling libraries/executables and the explicit
  stable cost centres; and
- the hotspot preset additionally enables `-fprof-late` for automatic
  top-level cost centres added after optimization.

GHC 9.14 recommends `-fprof-late` because it is added after the optimizer and
therefore resembles an optimized non-profiled executable more closely than
early blanket cost-centre insertion. Internal worker names or inlined-cost
attribution may still appear in hotspot profiles; the explicit stage frames
remain the stable navigation layer.

The presets and documented Cabal/RTS commands replace a task-specific shell
wrapper. Profiling builds are isolated from ordinary builds because GHC
profiling code is binary-incompatible with non-profiling code.

### Supported artifacts

The performance guide documents commands for:

- `-p`, `-P`, and `-pa` text time/allocation reports;
- `-pj` JSON cost-centre profiles, which GHC documents as directly loadable by
  Speedscope;
- eventlogs containing cost-centre samples, stage markers, GC, and
  runtime-system activity; and
- heap profiles by producer/cost centre, closure type, module, and type.

GHC's allocation total is cumulative physical allocation by the Haskell
implementation. Heap profiling answers the distinct question of live heap over
time. The documentation keeps both separate from Jazz semantic construction
counts.

## Testing and Verification

Implementation proceeds in test-first slices and adds behavior coverage for:

1. manifest decoding, validation, deterministic ordering, and root confinement;
2. expected output and termination for every corpus case;
3. deterministic budget success and useful over-budget diagnostics;
4. known runtime-counter behavior for small programs;
5. disabled-observation behavior and combined statistics/profile collection;
6. byte-identical profiles for two identical runs;
7. balanced, ordered, Speedscope-compatible profile events;
8. CLI parsing, `--run` requirements, stdout/stderr separation, partial runtime
   failure output, and profile-write failure handling;
9. benchmark registration and one-pass smoke execution; and
10. profiled-build smoke execution that produces parseable GHC JSON.

Runtime counter tests invoke programs and inspect reports through the runtime
API. They do not assert that particular Haskell source lines contain increment
calls. The profile tests decode the produced JSON, validate frame indices and
event nesting, and confirm logical end time equals evaluator-step count.

The ordinary full gate builds all components, runs all Cabal tests including
the corpus and deterministic budgets, runs the benchmark smoke mode, checks
documentation and repository invariants, and checks the diff. A separate
profile verification command builds through the stage preset, runs one fast
corpus case with `-pj`, and decodes the resulting JSON. It validates profiling
support without treating sampled numbers as expected values.

## Documentation and Results Workflow

`jazz-next/programs/README.md` documents corpus schema, feature tags, workload
classes, expected-output files, and the reviewed budget-update process.

`jazz-next/PERFORMANCE.md` is the performance runbook. It documents:

- benchmark smoke and full commands;
- result recording and compatible baseline comparison;
- runtime statistics in human and JSON modes;
- deterministic Jazz profile generation and Speedscope use;
- stage and hotspot GHC profiling builds;
- time/allocation, heap, and eventlog artifact interpretation; and
- the distinction between semantic work, cumulative Haskell allocation, and
  live heap residency.

`jazz-next/README.md` links to both documents and provides only the shortest
common commands. Generated benchmark and profile artifacts are ignored; source
manifests, expected output, deterministic budgets, Cabal presets, and docs are
versioned.

On completion, Batch 3 is marked implemented in
`docs/jazz-improvement-backlog.md`. That closeout also updates its earlier
fixture-placement sentence to point to the accepted shared corpus. The general
execution queue remains a separate dispatcher and is not rewritten as a
benchmark task list.

## Delivery Shape

Batch 3 is one implementation plan and one pull request with reviewable commits
in this order:

1. corpus format, loader, realistic programs, and correctness tests;
2. benchmark component, forcing boundaries, results, and metadata;
3. runtime statistics and deterministic budget checks;
4. Jazz semantic profile and CLI integration;
5. GHC stage catalogue, profiling presets, and smoke verification; and
6. performance/corpus documentation and backlog closeout.

Later commits may build on earlier internal APIs, but each commit must keep all
pre-existing tests passing and include the focused verification for its slice.

## References

- [GHC 9.14.1 profiling guide](https://downloads.haskell.org/ghc/9.14.1/docs/users_guide/profiling.html)
- [tasty-bench package documentation](https://hackage.haskell.org/package/tasty-bench)
- [Speedscope file format and viewer](https://github.com/jlfwong/speedscope)
