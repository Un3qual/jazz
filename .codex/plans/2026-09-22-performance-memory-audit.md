# Performance and memory audit

Date: 2026-09-22. Source: `3bb74993`, after the typed-list sharing fix.
This is a completed audit, not an implementation plan or an executable queue
entry. Compiler and runtime code are unchanged.

## Recommended small batch

Start with findings 1–3. They remove demonstrated work at its existing owner:
one empty-set guard, reuse of an existing free-variable summary, and one strict
counter field. Measure each independently. No new cache, framework, dependency,
or runtime representation is needed.

### 1. Skip primitive-constraint collection for closed bindings

[Capabilities.hs:498](../../src/Jazz/Compiler/TypeInference/Capabilities.hs)
scans every accumulated numeric/equality variable even when `schemeVariables`
is empty. Its own filter requires a **nonempty** variable set contained in that
empty set, so the result must be `[]`.

Ordinary `v0 = 0.` bindings reach this path after their type has defaulted to
closed `Int64`; `generalizedTypeBinding` forces the constraint list to decide
whether the binding is plain. At 4,096 such declarations, the collector and its
descendants account for **83.1% of allocation and 84.6% of sampled CPU time** in
the cost-centre profile.

Small fix: `Set.null schemeVariables = []` before the existing comprehensions.
This collector has no state updates or diagnostics to preserve by traversing.
Keep the existing behavior for nonempty sets.

### 2. Reuse the existing inference environment summary

[Scope.hs:976 and 991](../../src/Jazz/Compiler/TypeInference/Scope.hs) rescan the
visible environment for each constrained binding. The same function already
uses `envFreeVariables` at lines 1005–1008. On 4,096 independent constrained
functions, `freeTypeVariablesInEnv` and its descendants account for **83.4% of
allocation and 81.6% of sampled CPU time**.

Small fix: use the existing summary-selection logic at both earlier call sites.
Resolve the summary against each required solver state; do not reuse a resolved
set across constraint finalization. Retain the current full-scan fallback for
recursive and intervening groups.

### 3. Make the scope-number counter strict

[HostEvaluation.hs:66](../../src/Jazz/Compiler/Runtime/HostEvaluation.hs) writes
`scopeId + 1` into the lazy `Int` field at
[Types.hs:132](../../src/Jazz/Compiler/Runtime/Types.hs). Each block creates a
scope. A block containing only its terminal expression creates no binding key
that would force the number, allowing increment thunks to accumulate.

An expression-only block countdown grows from 2.50 MB maximum live heap at
50,000 iterations to 4.96 MB at 200,000; the corresponding bare countdown stays
at 1.89 MB. The separate heap profile attributes 4.76 MB of its 5.59 MB peak to
the allocation stack under `freshDeferredHostScopeId`.

Small fix: make only `runtimeHostEvaluationNextScopeId` strict (`!Int`). Confirm
with an isolated before/after run; the reported measurements are current costs,
not a measured speedup from a patch.

## Other worthwhile targets

### 4. Stop carrying and rescanning unrelated module inventories

[ModuleResolver.hs:336](../../src/Jazz/Compiler/ModuleResolver.hs) derives each
module's dependency maps from all previously resolved modules.
[Imports.hs:126](../../src/Jazz/Compiler/ModuleResolver/Imports.hs) retains the
inventory map in the module's validated scope, including unrelated siblings.
Independent leaves therefore accumulate quadratic map-spine metadata.

[ModuleImportScope.hs:58](../../src/Jazz/Compiler/ModuleImportScope.hs) also
reconstructs each dependency view by scanning all imported names and aliases.
The 2,048-leaf profile attributes **24.6% of allocation** to this operation and
its descendants. Whole-run live heap reaches 131.63 MB; that total includes all
compiler/runtime work and is not entirely attributable to these maps.

First restrict dependency facts to actual import paths and reuse the resulting
maps. Keep the public-reference identity registry global for transitive
re-exports. Measure again before adding a grouped dependency-view representation;
the repeated view scans are a separate remaining opportunity.

### 5. Use keyed lookup for explicit exports

[ModuleExports.hs:203 and 243](../../src/Jazz/Compiler/ModuleExports.hs) scans or
filters an entire `Set` per export selector. With 4,096 literal declarations,
adding explicit `value vN` exports raises elapsed time from **0.651 s to
1.172 s**. Inventory membership itself takes 10.2% of sampled CPU time in that
profile; set filtering is another prominent cost.

Small fix: exact `(namespace, name)` membership/selection; bare names need only
the finite namespace alternatives. Preserve constructor/class-method ownership
metadata in `restrictExportInventory`.

### 6. Bound local-binding cache retention, after checking escape behavior

[Engine.hs:461 and 1694](../../src/Jazz/Compiler/Runtime/Engine.hs) forces each
block-local binding and stores its result in the run-wide cache. Entries survive
after tail transfer. Adding unused `scratch = [n, n + 1, n + 2].` to the countdown
raises maximum live heap from 1.92 MB at 1,000 iterations to 21.00 MB at 50,000.
The heap profile shows live allocations at cache insertion and binding-value
creation; the source establishes the cache's retention lifetime.

This is distinct from the counter: cache keys already force their scope numbers.
Safe reclamation must preserve escaping closures and exactly-once effects.
Do not implement unconditional block-exit eviction. Investigate a bounded case
of provably disposable bindings before designing a general cache-lifetime change.

### 7. Index class-method collision candidates

[Analyzer.hs:519](../../src/Jazz/Compiler/Analyzer.hs) scans every statement for
every class method, including unrelated declarations. A 4,096-class input with
one unique method per class allocates 2.67 GB and takes 1.188 s. The analyzer's
scope-diagnostics cost centre alone accounts for 34.9% of allocation in its
profile; inference has additional costs, so this is not a prediction of the
total improvement.

Use a local declaration index by method spelling. Preserve diagnostic order and
the distinction between earlier classes and all let/data declarations.

### Smaller source-backed opportunities, not separately measured

- [Primitives.hs:369](../../src/Jazz/Compiler/Runtime/Primitives.hs): `filterElements`
  allocates a full `(value, Bool)` list before retaining selected values. Use
  `filterM` with the existing Boolean validator; preserve callback order and
  `E3019`. Measure reject-all and mixed inputs before assigning priority.
- [Capabilities.hs:210](../../src/Jazz/Compiler/TypeInference/Capabilities.hs):
  coherence validation enumerates implementation pairs across unrelated
  capabilities. Group by capability before pairing; preserve exact overlap
  checking and diagnostic order. This still needs a dedicated scaling probe.

## Measurements and limits

Ordinary CLI build: pinned GHC 9.14.1, Cabal 3.16.1.0, `-O1`, aarch64 macOS.
Builds and runs were serialized. Each ordinary result is the median of three
fresh processes using `+RTS -s`; runtime observations were disabled. Elapsed
time includes loading, compilation, evaluation, and rendering. MB/GB below are
decimal. Allocation is cumulative; live heap is RTS sampled maximum residency,
not process RSS. Outputs and successful exit statuses were checked on every run.

| Input                        |    Size | Elapsed (s) | Allocated (GB) | Max live heap (MB) |
| ---------------------------- | ------: | ----------: | -------------: | -----------------: |
| Literal declarations         |   1,024 |       0.085 |          0.184 |               5.38 |
| Literal declarations         |   2,048 |       0.208 |          0.526 |               9.50 |
| Literal declarations         |   4,096 |       0.651 |          1.765 |              20.87 |
| Constrained functions        |   1,024 |       0.278 |          0.743 |              12.89 |
| Constrained functions        |   2,048 |       0.800 |          2.475 |              28.86 |
| Constrained functions        |   4,096 |       2.795 |          9.005 |              53.76 |
| Independent imported leaves  |     512 |       0.514 |          0.917 |              27.56 |
| Independent imported leaves  |   1,024 |       1.132 |          2.140 |              59.09 |
| Independent imported leaves  |   2,048 |       2.750 |          5.580 |             131.63 |
| Bare countdown               | 200,000 |       0.816 |          3.818 |               1.89 |
| Block countdown              | 200,000 |       0.858 |          4.214 |               4.96 |
| Countdown with local scratch |  50,000 |       0.566 |          2.213 |              21.00 |

Attribution uses separate builds from `cabal.project.profile-hotspots`, with
`-p` for time/allocation and `-hc -i0.02` for live heap. Profile percentages
include descendants where stated; overlapping costs must not be added.
Instrumentation changes timing and heap size, so profiled totals are not mixed
with the ordinary results above. No proposed optimization was implemented or
measured after a patch, and the full correctness suite was not rerun for this
documentation-only audit.

The pass covered runtime scopes/primitives, inference/analyzer/capability
selection, module imports/exports, lexer/parser, and standard-library collection
and text paths. Existing lexer dispatch and indexed token streams already avoid
the older obvious costs. Text probes also showed substantial interpreter
overhead (`replaceAll` with no matches on 16,000 characters allocates 2.84 GB),
but the profile did not isolate a similarly small library change; it is not in
the recommended batch.

## Reproduction

Temporary inputs, the three-sample runner, raw RTS logs, and profiles are in
`/private/tmp/jazz-perf-audit-20260922/`. They are local artifacts, not committed
benchmark infrastructure. Input shapes are:

- Literals: `module Main { v0 = 0. ... vN = N. 0. }`; explicit-export control
  adds `(value v0, ..., value vN)` to the same header.
- Constrained functions: declare `class Probe(a) { probe :: a -> Bool. }.` and
  its `Int` implementation returning `True`; repeat independent
  `fN = \(x) -> Probe::probe x.` bindings and call the last with `1`.
- Fanout: N leaves each export one distinct integer value; Main imports each
  under a distinct alias and sums them. This follows the existing
  `wide-module-fanout-*x0001` benchmark family.
- Countdown: `count :: Int -> Int. count = \(n) -> if n == 0 then 0 else count
(n - 1).`; wrap only the body in `{ ... . }`, then optionally add the scratch
  binding above. Each version returns `0`.
- Classes: N distinct `class CN(a) { mN :: a -> Bool. }.` declarations and final
  `0`, with no implementations or calls.

For each generated directory, run the ordinary built executable with:

```sh
jazz --run --entry-module Main --module-root INPUT --module-root jazz/stdlib \
  +RTS -s -RTS
```
