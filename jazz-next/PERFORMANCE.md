# Jazz performance and profiling

Jazz has three complementary measurement layers. Use the layer that answers
the question instead of treating all of them as interchangeable:

| Layer          | Measures                                              | Stability                                      | Primary artifact                                |
| -------------- | ----------------------------------------------------- | ---------------------------------------------- | ----------------------------------------------- |
| Corpus budgets | Semantic Jazz work performed by the runtime           | Deterministic for a fixed program and compiler | Correctness-test result                         |
| `tasty-bench`  | Physical time for compiler and interpreter boundaries | Machine and environment dependent              | `results.csv` plus `environment.json`           |
| GHC profiling  | Physical behavior of the Haskell implementation       | Build, machine, RTS, and sampling dependent    | `.prof`, `.eventlog`, `.hp`, and RTS statistics |

A semantic construction count is not a byte-allocation count. GHC allocation
is cumulative allocation by the current Haskell compiler/interpreter, while a
heap profile shows live residency over time. A future native Jazz runtime will
need allocator-owned physical statistics of its own.

The commands below assume the repository's Nix development shell and the
`jazz-next/` directory:

```bash
nix --extra-experimental-features 'nix-command flakes' develop
cd jazz-next
```

## Correctness and semantic budgets

The corpus suite is the deterministic performance gate. It runs all `fast` and
`full` cases, verifies behavior, and enforces semantic upper bounds:

```bash
cabal test program-corpus-spec --test-show-details=failures
```

There is intentionally no wall-clock CI gate. The `fast` classification selects
cases eligible for benchmark smoke coverage; it does not weaken correctness
coverage. See [`programs/README.md`](programs/README.md) for the manifest and
budget authoring contract.

## Benchmarks

The benchmark tree has five boundaries:

| Group                | Timed work                                                     |
| -------------------- | -------------------------------------------------------------- |
| `parse-lower`        | Tokenize, parse the surface program, and lower to the core AST |
| `analysis`           | Re-analyze the lowered entry module with imported interfaces   |
| `module-preparation` | Discover, resolve, analyze, and prepare a module program       |
| `runtime`            | Evaluate an already prepared program                           |
| `whole-program`      | Load the entry program through final runtime result            |

Setup required by a narrower group is performed before its timed action, and
the result is forced before the sample ends. Smoke mode executes one fast case
for every group once without collecting meaningful timing:

```bash
cabal bench jazz-next-bench --benchmark-options='--jazz-smoke'
```

Analysis setup compiles the program once to materialize validated dependency
interfaces; the timed action then re-analyzes the entry module against those
interfaces.

List the registered tree or select cases and a Tasty pattern:

```bash
cabal bench jazz-next-bench --benchmark-options='--list-tests'
cabal bench jazz-next-bench \
  --benchmark-options='--jazz-case=identifier-classifier --pattern=runtime'
```

An ordinary unlabelled run is useful for exploration but does not own durable
results. To record results, provide an environment label:

```bash
cabal bench jazz-next-bench \
  --benchmark-options='--environment-label=m1-local --time-mode=cpu'
```

This writes an ignored directory under
`benchmark-results/<label>/<run-id>/` containing `results.csv` and
`environment.json`. `--result-root=PATH` changes the root, and repeated
`--jazz-case=ID` options restrict the recorded case set. Smoke mode cannot
write durable results.

The environment document records the Git revision and dirty state, corpus and
selected cases, tool/package versions, operating system, architecture, CPU
identity, build mode, RTS capabilities and arguments, benchmark arguments, time
mode, and timestamp. Git, Cabal, and CPU probes use explicit
available/unavailable facts, so packaged trees and minimal CI images still
produce results without inventing metadata. Before comparing CSV files, require
the metadata to agree on schema, environment label, corpus, selected cases,
tool/package versions, platform, CPU, build mode, RTS configuration, and time
mode. The benchmark metadata API reports every mismatch and only permits an
incompatible comparison through an explicit override. A timestamp or Git
revision difference identifies runs but does not by itself change measurement
compatibility.

Use several samples, inspect variance, and reproduce a suspected regression on
the same quiet machine. Benchmark timing is evidence, not a deterministic test
expectation.

### Focused standard-library cases

The corpus includes five fast cases that isolate common library workloads
without reducing them to single-call microbenchmarks:

| Case                    | Intended performance shape                                                                  |
| ----------------------- | ------------------------------------------------------------------------------------------- |
| `word-frequency`        | Repeated insertion-ordered `Dictionary` lookup and update over tokenized Unicode text       |
| `sorted-index`          | AVL `Map` and `Set` construction followed by ascending traversal and boundary queries       |
| `queue-traversal`       | Persistent FIFO enqueue/dequeue during breadth-first traversal                              |
| `text-processing`       | Multiple scalar-aware text passes, including splitting, searching, replacement, and padding |
| `collection-boundaries` | Collection construction and consumption across module abstraction boundaries                |

Record the complete focused set on one machine with:

```bash
cabal bench jazz-next-bench \
  --benchmark-options='--environment-label=stdlib-local --time-mode=cpu --jazz-case=word-frequency --jazz-case=sorted-index --jazz-case=queue-traversal --jazz-case=text-processing --jazz-case=collection-boundaries'
```

The collection contracts predict different growth curves. `Dictionary` is an
insertion-ordered association list, so key lookup and updates are linear in the
number of distinct keys. `Map` and `Set` use balanced trees, so key operations
are logarithmic and ordered materialization is linear. `Queue` uses front and
rear lists, giving amortized constant-time enqueue/dequeue and linear
materialization. Whole-text transforms are linear in the scalar input and
output they traverse, except that repeated transformations necessarily make
repeated passes.

Use semantic statistics to explain a timing change before attributing it to
host noise. For example, more `operatorApplications` in `sorted-index` can
indicate extra comparisons, while more `listCellsConstructed` in
`text-processing` can indicate an avoidable intermediate representation.
`bindingsCaptured` includes imported runtime environments, so adding public
library bindings can raise it without changing a case's algorithm. The
case-specific ceilings in `programs/corpus.json` are deterministic regression
guards; recorded machine timings remain evidence and are not universal
pass/fail thresholds.

## Jazz runtime statistics

Runtime observation is opt-in and implemented inside the Jazz evaluator. The
ordinary disabled path does not collect counters or profile events. Statistics
are printed to stderr so successful program stdout is unchanged:

```bash
cabal run jazz-next -- --run --runtime-stats first.jz
cabal run jazz-next -- --run --runtime-stats=json first.jz
```

For a corpus module graph, include both its case root and the standard-library
root:

```bash
cabal run jazz-next -- \
  --run \
  --entry-module Main \
  --module-root programs/identifier-classifier \
  --module-root jazz/stdlib \
  --runtime-stats=json
```

The report includes evaluator transitions; forced values; applications by
callable kind; current and maximum continuation depth; closures and captured
bindings; list, tuple, and saturated ADT construction; pattern attempts,
matches, and bindings; builtin and host calls; and deferred-cache outcomes.
These are logical operations. They do not report Haskell bytes, GC time, or
live heap.

Successful and runtime-failed evaluations produce reports; a runtime failure is
marked failed and contains work collected before failure. Compilation failure
does not start evaluation and therefore has no runtime report.

## Deterministic Jazz semantic flame graphs

Write a Jazz-level semantic profile with:

```bash
mkdir -p profile-results
cabal run jazz-next -- \
  --run \
  --runtime-profile=profile-results/jazz-program.speedscope.json \
  first.jz
```

The file follows Speedscope's evented JSON schema. Open it in Speedscope to see
Jazz callables and their deterministic nesting. Its unit is `none`, and event
positions are evaluator work units rather than time samples. Repeating the same
program produces byte-identical profile bytes. A failed runtime produces a
balanced, explicitly incomplete profile for the work completed before failure.

`--runtime-stats` and `--runtime-profile` may be enabled together. Profile write
errors are CLI failures rather than silently discarded data.

## GHC stage and hotspot profiling

GHC profiling answers questions about the Haskell implementation, including
time, byte allocation, garbage collection, live heap, and compiler-stage
hotspots. Profiling builds use isolated build directories because their objects
are not compatible with the ordinary build.

The stage preset keeps automatic cost centres off and exposes the stable Jazz
compiler-stage annotations. Create a JSON cost-centre profile, RTS allocation
summary, and eventlog for one fast case:

```bash
mkdir -p profile-results/ghc-stages
cabal bench \
  --project-file=cabal.project.profile-stages \
  --builddir=dist-newstyle-profile-stages \
  jazz-next-bench \
  --benchmark-options='--jazz-smoke --jazz-case=identifier-classifier +RTS -sprofile-results/ghc-stages/identifier-classifier.stats -pj -poprofile-results/ghc-stages/identifier-classifier -l -olprofile-results/ghc-stages/identifier-classifier.eventlog -RTS'
```

The JSON `.prof` file is directly loadable by Speedscope. Unlike the Jazz
semantic profile, it describes sampled Haskell cost-centre stacks, has physical
time/allocation data, and varies between runs. The eventlog includes RTS/GC
events and paired user markers named
`jazz-stage:<stage>:begin`/`jazz-stage:<stage>:end` around forced IO phase
boundaries. Pure sub-stages retain the same stable names as manual SCC frames in
the `.prof` file; they do not manufacture eventlog intervals by changing pure
compiler APIs or evaluation order.

Stable stages are:

| Stage                           | Boundary                                                 |
| ------------------------------- | -------------------------------------------------------- |
| `source-loading`                | Read requested source text                               |
| `module-discovery`              | Find module files and dependencies                       |
| `lexing`, `parsing`, `lowering` | Convert source through surface syntax to core AST        |
| `module-resolution`             | Build and validate the dependency-ordered module program |
| `static-analysis`               | Run module/expression semantic analysis                  |
| `type-inference`                | Infer types at the public inference boundary             |
| `constraint-solving`            | Solve accumulated type constraints                       |
| `capability-solving`            | Resolve capability requirements                          |
| `runtime-preparation`           | Build runtime-ready module state                         |
| `evaluation`                    | Execute Jazz evaluator work                              |
| `host-operation`                | Invoke validated host effects                            |
| `diagnostic-rendering`          | Render a user-facing diagnostic where measured           |

Use the stage `.prof` file, rather than assuming every catalogue entry is an
eventlog interval, to compare time and allocation for pure stages such as
module resolution, static analysis, constraint solving, and capability solving.
For discovery below those stable frames, use the late-cost-centre hotspot
preset:

```bash
mkdir -p profile-results/ghc-hotspots
cabal bench \
  --project-file=cabal.project.profile-hotspots \
  --builddir=dist-newstyle-profile-hotspots \
  jazz-next-bench \
  --benchmark-options='--jazz-smoke --jazz-case=identifier-classifier +RTS -pj -poprofile-results/ghc-hotspots/identifier-classifier-hotspots -RTS'
```

Internal worker names may change in this profile; use the manual stage profile
for stable comparisons and the hotspot profile to locate implementation detail.

Generate a live-heap profile separately so profiling modes do not distort each
other:

```bash
cabal bench \
  --project-file=cabal.project.profile-hotspots \
  --builddir=dist-newstyle-profile-hotspots \
  jazz-next-bench \
  --benchmark-options='--jazz-smoke --jazz-case=identifier-classifier +RTS -hc -i0.001 -poprofile-results/ghc-hotspots/identifier-classifier-heap -RTS'
```

This produces an `.hp` file grouped by cost-centre stack. Other GHC heap
breakdowns can group by closure description, type, retainer, biography, or
module when a particular investigation needs them. Heap samples show what
remains live; they do not replace the cumulative allocation total in `-s` or
`-pj` output.

All generated benchmark and profile artifacts are ignored. Keep a useful result
with the review or external benchmark record that motivated it, not in the
source distribution.
