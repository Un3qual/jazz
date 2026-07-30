# Jazz Algorithmic Program Corpus Implementation Plan

> **Superseded in part on 2026-07-30:** The workloads remain active, but their
> function equations were migrated to pattern lambdas and explicit `case`.
> See `2026-07-30-jazz-remove-function-equations.md`.

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add six deterministic, computationally substantial Jazz programs that exercise functional algorithms and produce useful benchmark, statistics, and semantic-profile evidence.

**Architecture:** Each workload is a self-contained module graph with a concise deterministic stdout checksum. Cases are registered as lexical-order `full` workloads in the existing versioned manifest and use the existing five benchmark boundaries. Correctness, semantic budgets, repeated statistics, and byte-identical Speedscope output are tested without wall-clock CI thresholds.

**Tech Stack:** Jazz modules, JSON program manifest, Haskell corpus runner/tests, `tasty-bench`, Jazz runtime observations and Speedscope profiles, Cabal/Nix.

## Global Constraints

- Add exactly these full workloads: `fannkuch`, `merge-sort`, `n-queens`, `prime-sieve`, `symbolic-differentiation`, and `tak`.
- Manifest entries remain in stable lexical order.
- Every case compiles and runs through the production module graph.
- Every case has exact stdout, exact termination, deterministic repeated statistics, semantic ceilings with bounded headroom, and a balanced nontrivial semantic profile.
- Every case participates in `parse-lower`, `analysis`, `module-preparation`, `runtime`, and `whole-program`.
- New source dogfoods function equations, structured constructor fields, `$`, and other applicable modern syntax without decorative use.
- No universal wall-clock threshold is added.
- Generated benchmark, statistics, and profile artifacts remain ignored.
- `jazz-hs/` and `jazz2/` remain untouched.
- Each workload lands in its own focused commit before manifest-wide documentation and final verification.

---

## File Structure

- Each case has `jazz-next/programs/CASE/Main.jz` as its entry module and one
  algorithm module named below in the owning task.
- Each case has `jazz-next/programs/CASE/expected.stdout` containing the exact
  rendered program value and final newline.
- `jazz-next/programs/corpus.json`: stable lexical registry, feature tags, benchmark groups, and budgets.
- `jazz-next/programs/README.md`: case inventory and authoring intent.
- `jazz-next/PERFORMANCE.md`: performance shape and profiling commands.
- `jazz-next/test/JazzNext/ProgramCorpus/ProgramCorpusSpec.hs`: repeated observation and profile validation.
- `jazz-next/program-support/JazzNext/ProgramCorpus/{Types,Manifest,Runner}.hs`: changed only if the manifest feature vocabulary or reusable profile assertion needs extension.

### Task 1: Extend Corpus Validation for Full Algorithmic Profiles

**Files:**
- Modify: `jazz-next/test/JazzNext/ProgramCorpus/ProgramCorpusSpec.hs`
- Modify: `jazz-next/program-support/JazzNext/ProgramCorpus/Types.hs` only if exact algorithm tags are added
- Modify: `jazz-next/program-support/JazzNext/ProgramCorpus/Manifest.hs` only if the schema vocabulary changes

**Interfaces:**
- Produces: a required case-id set for the six algorithms.
- Produces: reusable `assertDeterministicCaseObservation`.
- Produces: reusable `assertBalancedNontrivialProfile`.
- Preserves: schema version 1 unless a new manifest field, rather than a new enum value, is introduced.

- [ ] **Step 1: Add failing registration and profile assertions**

Assert the manifest contains:

```haskell
Set.fromList
  [ "fannkuch",
    "merge-sort",
    "n-queens",
    "prime-sieve",
    "symbolic-differentiation",
    "tak"
  ]
```

For each, assert `FullWorkload`, successful termination, and all five benchmark groups. Add a profile assertion that parses the Speedscope JSON and checks:

- at least three distinct callable frames;
- at least eight open/close events;
- equal open and close counts;
- maximum stack depth at least three;
- byte-identical output on a second run.

- [ ] **Step 2: Run the corpus suite and confirm all six cases are missing**

Run:

```bash
nix --extra-experimental-features 'nix-command flakes' develop -c \
  cabal test --project-dir=jazz-next \
    program-corpus-spec \
    --test-show-details=failures
```

Expected: FAIL listing the six absent case identifiers.

- [ ] **Step 3: Refactor repeated observation/profile helpers**

Use one runner path for all new cases. Compare the complete runtime statistics value from run one and run two, not selected counters. Validate profile balance from parsed events and compare the complete emitted bytes.

- [ ] **Step 4: Run existing corpus tests**

Temporarily gate the required-id assertion behind the test's local fixture list while helper refactoring is verified, then restore the final required set before Task 7. Existing cases must pass unchanged.

- [ ] **Step 5: Commit**

```bash
git add jazz-next/test/JazzNext/ProgramCorpus jazz-next/program-support/JazzNext/ProgramCorpus
git commit -m "test: validate algorithmic corpus profiles"
```

### Task 2: Add `n-queens`

**Files:**
- Create: `jazz-next/programs/n-queens/Queens.jz`
- Create: `jazz-next/programs/n-queens/Main.jz`
- Create: `jazz-next/programs/n-queens/expected.stdout`
- Modify: `jazz-next/programs/corpus.json`

**Interfaces:**
- Produces: `solutionCount :: Int -> Int`.
- Expected stdout: `92\n` for the eight-queen problem.

- [ ] **Step 1: Add the manifest entry and expected output before the source**

Register `n-queens` as `full` with all five benchmark groups and features:

```json
["modules", "patterns", "recursion", "inference", "lists", "deterministic-runtime"]
```

Use temporary conservative semantic ceilings copied above no existing measured value; the case must fail as a missing source until implementation exists.

- [ ] **Step 2: Run the single-case corpus test and confirm the source is missing**

Run:

```bash
nix --extra-experimental-features 'nix-command flakes' develop -c \
  cabal test --project-dir=jazz-next program-corpus-spec --test-show-details=failures
```

Expected: FAIL for `n-queens` path/loading.

- [ ] **Step 3: Implement deterministic backtracking with function equations**

Use this public shape:

```jazz
module Queens (value solutionCount) {
  safe _ _ [] = True.
  safe column distance [placed | rest] =
    column != placed
      && abs (column - placed) != distance
      && safe column (distance + 1) rest.

  place 0 placed = [placed].
  integerRangeFrom current upper =
    if current > upper
      then []
      else listPrepend current $ integerRangeFrom (current + 1) upper.

  integerRange lower upper = integerRangeFrom lower upper.

  absolute number = if number < 0 then 0 - number else number.

  place remaining placed =
    listConcat $ listMap
      (\column ->
        if safe column 1 placed
          then listMap (listPrepend column) $ place (remaining - 1) placed
          else [])
      (integerRange 1 8).

  solutionCount size = length $ place size [].
}
```

Import `List` and use its checked-in `listPrepend`, `listMap`, and `listConcat`
exports. Keep `integerRange` and `absolute` local; do not add new builtins.

- [ ] **Step 4: Run, measure twice, and set ceilings**

Run the production CLI twice with `--runtime-stats=json`, compare complete JSON, then set each manifest budget to the measured value plus approximately 15% headroom, rounded upward to a readable boundary.

- [ ] **Step 5: Generate and validate the semantic profile**

Write two profiles in separate temporary directories, compare bytes, and run the corpus profile assertion. Expected: balanced recursive `place`/`safe` stacks.

- [ ] **Step 6: Commit**

```bash
git add jazz-next/programs/n-queens jazz-next/programs/corpus.json
git commit -m "feat: add n-queens corpus workload"
```

### Task 3: Add `merge-sort`

**Files:**
- Create: `jazz-next/programs/merge-sort/MergeSort.jz`
- Create: `jazz-next/programs/merge-sort/Main.jz`
- Create: `jazz-next/programs/merge-sort/expected.stdout`
- Modify: `jazz-next/programs/corpus.json`

**Interfaces:**
- Produces: `mergeSort :: [Int] -> [Int]`.
- Produces: deterministic checksum over a fixed list with duplicates.

- [ ] **Step 1: Register the missing case and a fixed expected checksum**

Generate exactly 128 inputs for indices `1..128` with:

```jazz
nextItem index = (index * 73 + 19) % 211.
```

The expected line is:

```text
128:0:210:1156540
```

Independently verify those four fields in the corpus test using the equivalent
Haskell list expression before accepting `expected.stdout`.

- [ ] **Step 2: Confirm the registered case fails before source creation**

Run the corpus suite. Expected: missing-path failure for `merge-sort`.

- [ ] **Step 3: Implement stable split and merge equations**

Use canonical definitions shaped like:

```jazz
splitAlternating [] = ([], []).
splitAlternating [item] = ([item], []).
splitAlternating [left, right | rest] =
  case splitAlternating rest {
    | (lefts, rights) -> ([left | lefts], [right | rights])
  }.

merge [] rights = rights.
merge lefts [] = lefts.
merge [left | lefts] [right | rights] =
  if left <= right
    then [left | merge lefts [right | rights]]
    else [right | merge [left | lefts] rights].
```

Use Jazz's checked-in cons spelling `[first | rest]`; express the two-item split
as nested cons patterns rather than inventing `[left, right | rest]` if the
function-head parser correctly rejects that noncanonical form. Preserve stable
ordering for equal keys.

- [ ] **Step 4: Verify exact output, repeated stats, budgets, and profile**

Run twice, compare complete statistics, set bounded ceilings, and require recursive split/merge frames in the balanced profile.

- [ ] **Step 5: Commit**

```bash
git add jazz-next/programs/merge-sort jazz-next/programs/corpus.json
git commit -m "feat: add merge-sort corpus workload"
```

### Task 4: Add `prime-sieve`

**Files:**
- Create: `jazz-next/programs/prime-sieve/Sieve.jz`
- Create: `jazz-next/programs/prime-sieve/Main.jz`
- Create: `jazz-next/programs/prime-sieve/expected.stdout`
- Modify: `jazz-next/programs/corpus.json`

**Interfaces:**
- Produces: `primesThrough :: Int -> [Int]`.
- Expected stdout: count, largest prime, and sum for a fixed bound.

- [ ] **Step 1: Register a full case over a fixed bound**

Use the fixed inclusive bound `2,000`. The exact expected output is:

```text
303:1999:277050
```

- [ ] **Step 2: Confirm the case fails before implementation**

Run the corpus suite. Expected: missing-path failure.

- [ ] **Step 3: Implement a list sieve with higher-order filtering**

Use:

```jazz
sieve [] = [].
sieve [prime | candidates] =
  [prime | sieve $ filter (\candidate -> candidate % prime != 0) candidates].
```

Build candidates with a local `integerRangeFrom` using `listPrepend`; do not add
a range primitive. Return only the text/value rendering `303:1999:277050`.

- [ ] **Step 4: Verify output independently and measure**

Verify the checksum against a Haskell-side calculation in the test or a one-off checked command, then run twice for exact statistics and profiles. Set ceilings from observed counts.

- [ ] **Step 5: Commit**

```bash
git add jazz-next/programs/prime-sieve jazz-next/programs/corpus.json
git commit -m "feat: add prime-sieve corpus workload"
```

### Task 5: Add `fannkuch` and `tak`

**Files:**
- Create: `jazz-next/programs/fannkuch/Fannkuch.jz`
- Create: `jazz-next/programs/fannkuch/Main.jz`
- Create: `jazz-next/programs/fannkuch/expected.stdout`
- Create: `jazz-next/programs/tak/Takeuchi.jz`
- Create: `jazz-next/programs/tak/Main.jz`
- Create: `jazz-next/programs/tak/expected.stdout`
- Modify: `jazz-next/programs/corpus.json`

**Interfaces:**
- Produces: `fannkuch :: Int -> Int` returning maximum flips.
- Produces: `tak :: Int -> Int -> Int -> Int`.

- [ ] **Step 1: Register both cases and confirm missing sources fail**

Use lexical manifest positions: `fannkuch` before `identifier-classifier`, and `tak` among the `t` cases. Both are `full` and include all five benchmark groups.

- [ ] **Step 2: Implement permutation generation and prefix reversal**

Use function equations for empty/singleton list boundaries and ordinary higher-order list construction for permutations. Select a fixed size, beginning with `7`, that yields intensive but practical evaluator work. Print only the maximum flip count.

- [ ] **Step 3: Verify `fannkuch` twice and commit it separately**

Compare exact stdout, complete statistics, and profile bytes. The profile must include permutation generation, prefix reversal, and maximum reduction.

```bash
git add jazz-next/programs/fannkuch jazz-next/programs/corpus.json
git commit -m "feat: add fannkuch corpus workload"
```

- [ ] **Step 4: Implement Takeuchi recursion**

Use:

```jazz
tak x y z =
  if y < x
    then tak
      (tak (x - 1) y z)
      (tak (y - 1) z x)
      (tak (z - 1) x y)
    else z.
```

Use the fixed terminating input `(12, 6, 0)` and expected output `1`.

- [ ] **Step 5: Verify `tak` twice and commit**

Require deep but balanced callable stacks and low collection construction relative to applications.

```bash
git add jazz-next/programs/tak jazz-next/programs/corpus.json
git commit -m "feat: add Takeuchi corpus workload"
```

### Task 6: Add `symbolic-differentiation`

**Files:**
- Create: `jazz-next/programs/symbolic-differentiation/Expression.jz`
- Create: `jazz-next/programs/symbolic-differentiation/Main.jz`
- Create: `jazz-next/programs/symbolic-differentiation/expected.stdout`
- Modify: `jazz-next/programs/corpus.json`

**Interfaces:**
- Produces: structured recursive expression ADT.
- Produces: `differentiate :: Text -> Expression -> Expression`.
- Produces: `simplify :: Expression -> Expression`.
- Produces: canonical deterministic rendered output or checksum.

- [ ] **Step 1: Register the missing case**

Use features:

```json
["modules", "generic-adts", "patterns", "recursion", "inference", "text", "deterministic-runtime"]
```

If the existing feature vocabulary cannot distinguish the algorithm, keep schema version 1 and do not add a decorative tag.

- [ ] **Step 2: Confirm missing source failure**

Run the corpus suite. Expected: missing-path failure.

- [ ] **Step 3: Implement the expression tree and ordered transforms**

Use:

```jazz
data Expression
  = Constant Int
  | Variable Text
  | Add Expression Expression
  | Multiply Expression Expression
  | Power Expression Int.
```

Implement differentiation with function-head constructor patterns and simplification with ordered equations for zero, one, constant folding, and recursive normalization. Apply `differentiate` and `simplify` repeatedly to a fixed expression large enough to produce nontrivial tree-transform stacks.

- [ ] **Step 4: Verify canonical output and type/runtime behavior**

Build the sum of `Power (Variable "x") n` for `n = 1..12`, differentiate it
four times, simplify after every pass, and return the structural checksum
`9:8:30888`: nine remaining terms, highest exponent eight, and summed
coefficients 30,888. Run twice for statistics and profile bytes, then set
ceilings.

- [ ] **Step 5: Commit**

```bash
git add jazz-next/programs/symbolic-differentiation jazz-next/programs/corpus.json
git commit -m "feat: add symbolic differentiation workload"
```

### Task 7: Close Manifest, Documentation, and Benchmark Registration

**Files:**
- Modify: `jazz-next/programs/corpus.json`
- Modify: `jazz-next/programs/README.md`
- Modify: `jazz-next/PERFORMANCE.md`
- Modify: `jazz-next/test/JazzNext/ProgramCorpus/ProgramCorpusSpec.hs`
- Modify: `jazz-next/test/JazzNext/Benchmark/MetadataSpec.hs` or `StageSpec.hs` only if existing closed case inventories require it

**Interfaces:**
- Produces: complete lexical manifest with measured budgets.
- Produces: documented algorithm intent and expected performance shape.
- Restores: unconditional required six-case assertion from Task 1.

- [ ] **Step 1: Sort and validate the complete manifest**

Ensure case identifiers and directories are lexical, unique, relative, and present. Every new case uses:

```json
"workload": "full",
"expectedTermination": "success",
"benchmarks": [
  "parse-lower",
  "analysis",
  "module-preparation",
  "runtime",
  "whole-program"
]
```

- [ ] **Step 2: Restore the unconditional corpus assertions**

Remove the temporary local gating from Task 1. Assert exact registration, workload, benchmark groups, repeated stats, and profile requirements for all six cases.

- [ ] **Step 3: Document each algorithm and performance shape**

Add a table to `programs/README.md` with algorithm, checksum, language features, and semantic stress. Add a performance section distinguishing:

- branching/pattern attempts for n-queens;
- recursive allocation for merge-sort and fannkuch;
- repeated filtering for the sieve;
- deep callable application for tak;
- constructor/tree normalization for symbolic differentiation.

Include a command selecting all six `--jazz-case` arguments without adding a wall-clock pass/fail rule.

- [ ] **Step 4: Run corpus and benchmark smoke**

Run:

```bash
nix --extra-experimental-features 'nix-command flakes' develop -c \
  cabal test --project-dir=jazz-next \
    program-corpus-spec \
    benchmark-metadata-spec \
    benchmark-stage-spec \
    --test-show-details=failures \
    --jobs=1
nix --extra-experimental-features 'nix-command flakes' develop -c \
  cabal bench --project-dir=jazz-next jazz-next-bench \
    --benchmark-options='--jazz-smoke'
```

Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add jazz-next/programs jazz-next/PERFORMANCE.md jazz-next/test/JazzNext/ProgramCorpus jazz-next/test/JazzNext/Benchmark
git commit -m "docs: register algorithmic performance workloads"
```

### Task 8: Run the Combined Pre-Bootstrap Quality Gate

**Files:**
- Modify only files required by failures found in this verification task.

**Interfaces:**
- Produces: the complete green gate required before typed-core/bootstrap feature progression resumes.

- [ ] **Step 1: Run every new case twice and compare complete observations**

For each of:

```text
fannkuch
merge-sort
n-queens
prime-sieve
symbolic-differentiation
tak
```

run the production module graph twice with JSON runtime statistics and two semantic profile paths. Compare stdout, termination, complete statistics values, and profile bytes.

- [ ] **Step 2: Run the full serialized build/test/bench matrix**

Run:

```bash
nix --extra-experimental-features 'nix-command flakes' develop -c \
  cabal build --project-dir=jazz-next all
nix --extra-experimental-features 'nix-command flakes' develop -c \
  cabal test --project-dir=jazz-next all \
    --test-show-details=failures \
    --jobs=1
nix --extra-experimental-features 'nix-command flakes' develop -c \
  cabal bench --project-dir=jazz-next jazz-next-bench \
    --benchmark-options='--jazz-smoke'
```

Expected: PASS.

- [ ] **Step 3: Run repository/documentation hygiene**

Run:

```bash
bash scripts/check-docs.sh
bash scripts/check-execution-queue.sh
git diff --check
git diff main...HEAD -- jazz-hs jazz2
```

Expected: PASS with no legacy implementation changes.

- [ ] **Step 4: Perform the anti-slop diff review**

Inspect:

```bash
git diff --stat main...HEAD
git diff main...HEAD -- jazz-next/src jazz-next/jazz jazz-next/programs jazz-next/test
```

Reject any new duplicate grammar/type parser, opaque fallback, decorative `$`/type application, redundant wrapper module, or source-string assertion for AST-visible behavior.

- [ ] **Step 5: Commit any final corrections**

If corrections were required:

```bash
git add jazz-next docs
git commit -m "fix: close pre-bootstrap quality gate"
```

If no corrections were required, preserve the prior focused commit boundaries and record the final verification evidence.
