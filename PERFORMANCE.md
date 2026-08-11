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

The commands below assume the repository root and its Nix development shell:

```bash
nix --extra-experimental-features 'nix-command flakes' develop
```

## Bounded local verification

Run only one Cabal, Jazz, profiling, or Nix command at a time. The verification
scripts default Cabal jobs, Nix build jobs, and Nix cores to `1`; override the
defaults only when the machine has measured capacity:

```bash
JAZZ_CABAL_JOBS=2 JAZZ_NIX_JOBS=2 JAZZ_NIX_CORES=2 \
  bash scripts/ci/main-functional.sh
```

With no phase selection, `main-functional.sh` remains the authoritative main
gate: repository preflight, the ordinary Cabal build and complete test suite,
repository checks, and `nix flake check`. During a focused batch, select the
narrowest completed phase instead of restarting the whole gate after every
edit:

```bash
JAZZ_MAIN_PHASE=compiler bash scripts/ci/main-functional.sh
JAZZ_MAIN_PHASE=repository bash scripts/ci/main-functional.sh
JAZZ_MAIN_PHASE=nix bash scripts/ci/main-functional.sh
```

On a memory-constrained development machine, the low-memory phase runs the
compiler and repository phases serially and deliberately omits Nix:

```bash
JAZZ_MAIN_PHASE=low-memory bash scripts/ci/main-functional.sh
```

The script prints that omission so its receipt cannot be mistaken for complete
main or release evidence. Before publication, run exactly one fresh full
closeout after the final source change. Release candidates still require the
complete main, extended/profile, Nix build, packaging, website, determinism,
and artifact-validation gates through `scripts/ci/release-candidate.sh`.

If a long command goes quiet, inspect and keep waiting for that process. Do not
start a duplicate Cabal, Jazz, profiling, or Nix invocation merely because a
terminal session stopped displaying output.

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

The full corpus includes six algorithmic profile anchors with deliberately
different runtime shapes:

- `n-queens` is branch- and pattern-heavy backtracking;
- `merge-sort` emphasizes recursive split/merge traversal and list allocation;
- `prime-sieve` is a long numeric/filtering workload;
- `fannkuch` emphasizes permutation allocation and prefix-reversal stacks;
- `tak` isolates recursive application and continuation depth; and
- `symbolic-differentiation` exercises recursive ADT construction,
  transformation, simplification, and evaluation.

All six participate in every benchmark boundary. Their manifest ceilings come
from deterministic runtime observations with bounded headroom; they are full
workloads and therefore do not lengthen the default fast smoke selection.

### Hosted parser scale tiers

Hosted-parser scale correctness has two deterministic semantic-budget tiers:

- The default `jazz-parser-scale-spec` smoke tier runs expression,
  declarations, control-flow, and operator profiles twice at 65 parsed
  statements. It requires exact output, successful termination, zero host
  operations, complete-statistics equality between runs, and measured semantic
  ceilings. It is included in the routine matrix:

  ```bash
  cabal test all --test-show-details=failures
  ```

- The full tier preserves one 513-statement semantic-budget check per grammar
  family behind the default-disabled `full-parser-scale` flag. It is a niche
  manual diagnostic, not routine verification. Run it only when a maintainer
  explicitly requests it, a full-scale generator or ceiling changes, or smoke
  evidence needs full-scale reproduction:

  ```bash
  cabal test -ffull-parser-scale \
    jazz-parser-scale-full-expression-spec \
    jazz-parser-scale-full-declarations-spec \
    jazz-parser-scale-full-control-flow-spec \
    jazz-parser-scale-full-operator-spec \
    --test-show-details=failures
  ```

Both tiers assert logical Jazz work rather than physical duration. They have no
wall-clock threshold and are not benchmarks. Use the `tasty-bench` workflows
below for repeated physical timing, environment metadata, and comparison
artifacts.

## Benchmarks

The benchmark tree has six boundaries:

| Group                | Timed work                                                     |
| -------------------- | -------------------------------------------------------------- |
| `parse-lower`        | Tokenize, parse the surface program, and lower to the core AST |
| `analysis`           | Re-analyze the lowered entry module with imported interfaces   |
| `module-preparation` | Discover, resolve, analyze, and prepare a module program       |
| `typed-lowering`     | Validate trusted Typed Core and lower it into Lowered IR       |
| `runtime`            | Evaluate an already prepared program                           |
| `whole-program`      | Load the entry program through final runtime result            |

Setup required by a narrower group is performed before its timed action, and
the result is forced before the sample ends. Smoke mode executes one fast case
for every group once without collecting meaningful timing:

```bash
cabal bench jazz-bench --benchmark-options='--jazz-smoke'
```

Analysis setup compiles the program once to materialize validated dependency
interfaces; the timed action then re-analyzes the entry module against those
interfaces.

### Generated compiler scale cases

Compiler-scale cases are generated in memory and are opt-in, so the ordinary
repeated corpus tree and extended benchmark workload remain unchanged. Smoke
mode still executes one case per boundary; because no corpus case owns the
`typed-lowering` boundary, it uses the smallest generated typed-validation
fixture for that one boundary. The registered case families isolate these
growth curves:

| Scenario                          | Stable case sizes                   | Timed groups                                      | Exact result or artifact            |
| --------------------------------- | ----------------------------------- | ------------------------------------------------- | ----------------------------------- |
| Sequential polymorphic bindings   | 64, 128, 256, 512                   | `analysis`, `module-preparation`                  | `(42, True)`                        |
| Wide module fanout, width 16      | 8, 16, 32, 64 modules               | `module-preparation`, `whole-program`             | `0`                                 |
| Wide module fanout, width 1       | 64, 128, 256, 512                   | `module-preparation`, `whole-program`             | `0`                                 |
| Shared-interface fanout, width 16 | 16, 32, 64, 128 modules             | `module-preparation`, `whole-program`             | `0`                                 |
| Resolver fact-rich declarations   | 16, 32, 64, 128 groups              | `module-preparation`                              | `Token`                             |
| Typed validation handoff          | 64, 128, 256, 512 nodes             | `typed-lowering`                                  | valid Lowered IR                    |
| Lowered temporary validation      | 64, 256, 1024, 4096 instructions    | `typed-lowering`                                  | valid Lowered IR                    |
| Typed recursive statement graph   | 128, 512, 1024, 2048 statements     | `typed-lowering`                                  | valid Typed Core graph              |
| Typed forward-signed functions    | 128, 512, 1024, 2048 functions      | `typed-lowering`                                  | valid Lowered IR                    |
| Typed wide export providers       | 128, 512, 1024, 2048 providers      | `typed-lowering`                                  | valid Typed Core export inventory   |
| Wide constructor applications     | 32, 64, 128, 256 fields             | `analysis`, `runtime`, `whole-program`            | `(<function>, (0, midpoint, last))` |
| Capability candidate width        | 16, 32, 64, 128 candidates          | `analysis`, `runtime`, `whole-program`            | last candidate index                |
| Host-free opaque environments     | 64, 256, 1024, 4096 bindings        | `runtime`, `whole-program`                        | `1`                                 |
| Analyzer diagnostic chains        | 64, 128, 256, 512 nodes             | `analysis`                                        | exact error count                   |
| Interleaved recursive groups      | 16, 32, 64, 128 groups              | `analysis`, `module-preparation`                  | `(1, True)`                         |
| Recursive preview bursts          | 16, 32, 64, 128 groups              | `analysis`                                        | `(1, True)`                         |
| Recursive rebinding bursts        | 128, 256, 512, 1024                 | `analysis`                                        | final rebound value                 |
| Constrained signatures            | 32, 64, 128, 256                    | `parse-lower`, `analysis`                         | `(1, True)`                         |
| Deferred constraint bursts        | 128, 256, 512, 1024                 | `analysis`                                        | exact result list                   |
| Deep nested lambdas               | 16, 32, 64, 128 levels              | `analysis`, `module-preparation`, `whole-program` | `(1, depth)`                        |
| Large declared operator tables    | 16, 32, 64, 128 symbols             | `parse-lower`                                     | parses and lowers                   |
| Nested expression blocks          | 16, 32, 64, 128 levels              | `parse-lower`                                     | parses and lowers                   |
| Ambiguous case-arm pipe bodies    | 64, 128, 256, 512 terms             | `parse-lower`                                     | one left-associated body            |
| Exact long token streams          | 1,024, 4,096, 16,384, 65,536 tokens | `parse-lower`                                     | exact token count                   |
| Identifier token streams          | 1,024, 4,096, 16,384, 65,536 tokens | `parse-lower`                                     | exact token count                   |
| Literal token streams             | 1,024, 4,096, 16,384, 65,536 tokens | `parse-lower`                                     | exact token count                   |
| Nested runtime applications       | 64, 128, 256, 512 levels            | `runtime`                                         | `7`                                 |
| Runtime import width              | 64, 128, 256, 512 exports           | `runtime`, `whole-program`                        | `7`                                 |

Case identifiers encode the controlling size, for example
`sequential-polymorphic-bindings-0064` and
`wide-module-fanout-0008x0016`; the final field is the interface width, so
`wide-module-fanout-0064x0001` isolates dependency lookup with minimal rebasing.
`shared-interface-fanout-0016x0016` makes every dependent module import the
same interface, isolating repeated dependency rebasing and cache reuse.
List one generated case with:

```bash
cabal bench jazz-bench --jobs=1 \
  --benchmark-options='--jazz-scale-case=sequential-polymorphic-bindings-0064 --list-tests'
```

The leaves are stable, such as
`All.compiler-scale.analysis.sequential-polymorphic-bindings-0064`. Repeat
`--jazz-scale-case=ID` to select a comparable curve, and add an environment
label to record the ordinary `results.csv` and `environment.json` pair:

```bash
cabal bench jazz-bench --jobs=1 \
  --benchmark-options='--environment-label=compiler-scale-local --time-mode=cpu --jazz-scale-case=sequential-polymorphic-bindings-0064 --jazz-scale-case=sequential-polymorphic-bindings-0128 --jazz-scale-case=sequential-polymorphic-bindings-0256 --jazz-scale-case=sequential-polymorphic-bindings-0512 +RTS -T -RTS'
```

Generated selectors cannot be combined with `--jazz-case` or `--jazz-smoke`.
The semantic tests compile and evaluate the smallest runtime-capable case and
assert exact output. Parser-only families traverse the real lexer, parser, and
lowerer, and the smallest long-stream case asserts its exact token count.
Physical time, cumulative allocation, and maximum residency remain recorded
evidence rather than deterministic thresholds. Use
`+RTS -T -RTS` when the optimized CSV must include `Allocated`, `Copied`, and
`Peak Memory` columns; the captured environment records the `-T` configuration
so comparisons cannot silently mix it with a timing-only run.

List the registered tree or select cases and a Tasty pattern:

```bash
cabal bench jazz-bench --benchmark-options='--list-tests'
cabal bench jazz-bench \
  --benchmark-options='--jazz-case=identifier-classifier --pattern=runtime'
```

An ordinary unlabelled run is useful for exploration but does not own durable
results. To record results, provide an environment label:

```bash
cabal bench jazz-bench \
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
cabal bench jazz-bench \
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
cabal run jazz -- --run --runtime-stats first.jz
cabal run jazz -- --run --runtime-stats=json first.jz
```

For a corpus module graph, include both its case root and the standard-library
root:

```bash
cabal run jazz -- \
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
cabal run jazz -- \
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
  jazz-bench \
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
  jazz-bench \
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
  jazz-bench \
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
