# Jazz program corpus

`programs/corpus.json` is the source of truth for production-shaped Jazz
programs shared by correctness tests and benchmarks. Each case lives in its own
directory so multi-module programs can be loaded exactly as user programs are.
Focused parser, diagnostic, and failure fixtures remain under
`test/fixtures/`.

## Manifest contract

The manifest has a `schemaVersion` and a stable, lexicographically ordered
`cases` array. A case contains:

- `id`: a permanent, human-readable identifier used by tests, benchmark names,
  result metadata, and review discussions;
- `directory`: the case directory relative to `programs/`;
- `entrySource`: the `.jz` entry file relative to the case directory;
- `moduleRoot`: the module search root relative to the case directory;
- `expectedTermination`: `success`, `compile-failure`, or `runtime-failure`;
- `expectedStdout`: the exact expected-output file relative to the case
  directory;
- `workload`: `fast` or `full`;
- `features`: the language areas intentionally exercised;
- `benchmarks`: the benchmark groups in which the case participates; and
- `budgets`: deterministic upper limits on semantic Jazz runtime work.

All paths must be relative, remain inside the corpus root after
canonicalization, and exist. The entry source must be beneath its module root.
The loader rejects duplicate IDs and directories, unknown vocabulary, missing
paths, absolute paths, `..` escapes, and symlink escapes. Expected output is
read verbatim, including its final newline.

Case IDs are durable data. Rename one only when intentionally invalidating
existing benchmark history and references. Adding fields or vocabulary that an
older loader cannot interpret requires a schema-version decision.

## Current cases

`corpus.json` remains the machine-readable source of truth; this inventory gives
reviewers and benchmark users a quick map from its durable IDs to their intent.

| Case ID | Workload | Purpose and principal features |
| --- | --- | --- |
| `capability-workflow` | `fast` | Processes workflow values through capability-constrained functions and multi-module dispatch. |
| `collection-boundaries` | `fast` | Passes abstract `Dictionary`, `Map`, `Set`, and `Queue` values between modules without exposing constructors. |
| `dependency-planner` | `fast` | Computes a dependency plan over an association-list graph using modules, recursion, patterns, text, and lists. |
| `expression-evaluator` | `fast` | Evaluates a small expression ADT with environments, recursive interpretation, and multi-module inference. |
| `fannkuch` | `full` | Generates every size-seven permutation, performs prefix reversals, and reduces maximum flip count; it emphasizes list allocation and recursive callable profiles. |
| `identifier-classifier` | `fast` | Classifies realistic identifier text using recursion, patterns, lists, and bundled text/character utilities. |
| `merge-sort` | `full` | Stably split/merges 128 deterministic values and reports length, extrema, and a weighted checksum. |
| `mini-frontend` | `full` | Runs token classification, expression analysis, and evaluation across a production-shaped module graph. |
| `n-queens` | `full` | Reports all 92 eight-queen solutions and 736 placed queens using backtracking, pattern-lambda clauses, closures, and persistent lists. |
| `prime-sieve` | `full` | Finds the 303 primes through 2000 with recursive filtering and reports count, last prime, and sum. |
| `queue-traversal` | `fast` | Performs a breadth-first traversal with the persistent FIFO `Queue`. |
| `sorted-index` | `fast` | Builds a persistent AVL `Map` and `Set`, then traverses their ascending views across a module boundary. |
| `symbolic-differentiation` | `full` | Differentiates and simplifies a recursive expression ADT, then reports structural and evaluated checksums. |
| `tak` | `full` | Runs the recursive Takeuchi benchmark with little collection noise, emphasizing application and continuation stacks. |
| `text-processing` | `fast` | Normalizes Unicode text through lines, words, replacement, splitting, search, joining, and padding. |
| `tree-transformations` | `full` | Builds, maps, and folds a generic recursive tree to exercise generic ADTs and higher-order traversal. |
| `word-frequency` | `fast` | Counts words with insertion-ordered `Dictionary` updates and Unicode-aware text tokenization. |

## Workloads, features, and benchmarks

`fast` cases are eligible for the one-pass benchmark smoke run. `full` cases
may take more work and are excluded from smoke selection. Both classes run in
the correctness suite and participate in ordinary benchmarks unless a run is
filtered.

The version-1 feature vocabulary is:

- `modules`, `generic-adts`, `patterns`, `recursion`, and `inference`;
- `capabilities`, `text`, `lists`, `dictionaries`, `ordered-collections`, and
  `queues`; and
- `deterministic-runtime`.

The benchmark groups are `parse-lower`, `analysis`, `module-preparation`,
`runtime`, and `whole-program`. Only declare a group when the case is useful at
that boundary. The corpus as a whole must cover every feature, workload class,
and benchmark group.

## Correctness and budgets

Every checked-in case must have deterministic inputs, termination, stdout,
diagnostics, warnings, and runtime statistics. Do not read the network, clock,
randomness, or undeclared machine state. The correctness suite executes every
case, compares exact behavior, runs it with runtime statistics enabled, and
checks its budgets:

```bash
cabal test program-corpus-spec --test-show-details=failures
```

`steps`, `applications`, and `maxContinuationDepth` are required. Optional
limits use the JSON names emitted by runtime statistics, such as
`closuresCreated`, `listCellsConstructed`, `patternAttempts`, `builtinCalls`,
and `deferredCacheMisses`. Every budget is an upper bound, not an expected exact
count. Lower work continues to pass.

Set budgets above the observed deterministic result with enough headroom for
small, intentional implementation changes, but not so much that a meaningful
regression disappears. When a budget changes, investigate the semantic cause
and describe it in review. Never replace a semantic budget with a wall-clock
test. See [`../PERFORMANCE.md`](../PERFORMANCE.md) for interpreting these
counters and collecting physical measurements.

## Adding or changing a case

1. Create a uniquely named directory containing ordinary formatted `.jz`
   modules and `expected.stdout`.
2. Add a lexicographically placed manifest entry with a stable ID, precise
   feature tags, deliberate benchmark participation, and initial budgets.
3. Run the correctness suite twice and confirm its observation report is
   deterministic.
4. Run the benchmark smoke mode and any affected ordinary benchmark groups.
5. If the case is intended for smoke coverage, keep it small, mark it `fast`,
   and ensure it contributes a useful group boundary.
6. Review the program as representative Jazz code, not merely as a way to make
   coverage counters increase.

Changes must preserve exact expected behavior unless the program is
intentionally being revised. Reviewers should treat unexplained output,
termination, budget, feature, and benchmark-membership changes as contract
changes rather than fixture maintenance.
