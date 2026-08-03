# Jazz Improvement Backlog

Status: durable discussion backlog; not an execution queue or implementation plan

Last updated: 2026-07-15

This document preserves the sixteen improvements agreed during the July 2026
Jazz design discussion. It records intent, important constraints, and proposed
batch boundaries so the ideas are not lost before individual design specs and
implementation plans are written.

This file does not make an item ready for implementation. The live dispatcher
remains [`docs/execution/queue.md`](execution/queue.md), and any implementation
batch must still receive its own accepted design, plan, target paths, and
verification contract.

## Proposed Batch Order

### Batch 1: Haskell toolchain and test-gate foundation

Status: completed on 2026-07-13.

Items: 6 and 15.

- Upgrade the active `Jazz` project to GHC 9.14.1 and its matching
  `base-4.22` range before rewriting embedded test programs.
- Replace the warning-named shell mega-gate with Cabal-owned test execution and
  focused Haskell repository-audit coverage.
- Establish the stable toolchain and test gate that every later batch will use.

### Batch 2: Jazz source organization and immediate editor ergonomics

Status: completed on 2026-07-13.

Items: 10, 1, and 9.

- Move shipped Jazz-authored sources under the locked
  `jazz/{stdlib,compiler}` root.
- Rewrite existing Jazz code to use the already-supported compact
  multi-parameter lambda form.
- Add TextMate-compatible syntax highlighting after the canonical source tree
  exists, so fixtures and examples can come from their permanent locations.

### Batch 3: Program corpus, benchmarks, and observability

Status: completed on 2026-07-14. See the
[`Jazz` performance guide](../PERFORMANCE.md) and
[`programs` corpus guide](../programs/README.md).

Items: 2, 3, and 4.

- Establish realistic Jazz programs and correctness fixtures.
- Add compiler, interpreter, and whole-program benchmarks with recorded
  baselines before the larger compiler and stdlib refactors.
- Add runtime-owned Jazz execution statistics and GHC profiling support for
  the Haskell compiler's stages and sub-stages.

### Batch 4: Unified diagnostics and focused Haskell structure improvements

Status: completed on 2026-07-14. See the
[approved design](superpowers/specs/2026-07-14-jazz-next-unified-diagnostics-design.md)
and [implementation plan](superpowers/plans/2026-07-14-jazz-next-unified-diagnostics.md).

Items: 14, 13, 11, and 12.

- Unified warnings and errors as one catalog-backed diagnostic report with
  explicit severity, ordered result streams, labeled spans, notes, and help.
- Centralized human diagnostic rendering and canonical `SignatureType`
  rendering without introducing a universal rendering typeclass.
- Removed the two constructor aliases whose call sites expressed no semantic
  transition; stronger types remained limited to catalog and diagnostic
  invariants exposed by the work.
- Used the Batch 3 deterministic corpus, stage smoke checks, GHC profiling
  build, and same-machine benchmark runs as correctness and performance
  evidence rather than a fixed percentage gate.

### Batch 5: Module export ergonomics and stdlib growth

Status: completed on 2026-07-15. See the
[approved design](superpowers/specs/2026-07-15-jazz-next-constructor-exports-broad-stdlib-design.md),
[implementation plan](superpowers/plans/2026-07-15-jazz-next-constructor-exports-broad-stdlib.md),
[module export contract](spec/modules/06-explicit-export-lists.md),
[standard-library reference](../jazz/stdlib/README.md), and
[performance guide](../PERFORMANCE.md).

Items: 5 and 8.

- Added `type Box(..)` and `type Box(Pack, Empty)` export groups while
  preserving abstract types, individual constructors, and selective exposure.
- Expanded the Jazz-authored stdlib with approachable foundation, optional,
  text, linear collection, ordered collection, and host-I/O APIs.
- Locked public names, abstraction boundaries, edge behavior, ordering,
  complexity, deterministic corpus budgets, same-machine benchmark recording,
  runtime statistics, and GHC profiling evidence.

### Batch 6: Source-level compiler directives

Item: 7.

- Add narrowly scoped per-file and per-line diagnostic directives only after
  the unified diagnostic model exists.
- Update syntax highlighting for the accepted directive syntax in the same
  batch.

### Batch 7: Documentation consolidation

Item: 16.

- Audit, compact, and clarify the documentation after the structural and
  terminology changes have landed.
- Keep active documentation accurate during every earlier batch; Batch 7 is the
  broader consolidation pass, not permission to leave broken links or stale
  active contracts in the meantime.
- Preserve historical plans and closure evidence as history rather than
  rewriting them as current specifications.

## The Sixteen Improvements

### 1. Use compact multi-parameter lambdas in Jazz source

Rewrite hand-written Jazz code from nested lambdas such as
`\(left) -> \(right) -> expression` to the already-supported
`\(left, right) -> expression` form. Preserve the existing lowering to nested
unary core lambdas and partial-application semantics.

Do not add a second function-declaration syntax such as
`function(left, right) = expression`. The accepted surface remains ordinary
bindings whose values may be compact multi-parameter lambdas.

### 2. Add realistic complex Jazz programs as tests

Add checked-in programs that combine modules, generic ADTs, patterns,
recursion, inference, capabilities, text and list processing, and deterministic
runtime behavior. Keep focused unit tests; realistic programs supplement them
rather than replacing them.

Prefer external `.jz` fixtures for substantial and multi-module programs so the
same corpus can support correctness tests, examples, and benchmarks.

### 3. Create a Jazz benchmark suite

Add reproducible benchmarks for compiler phases, interpreter behavior, and
whole Jazz programs. Include microbenchmarks that locate regressions and larger
programs that expose end-to-end changes. Record toolchain, machine, input, and
statistical context with baselines, and avoid fragile wall-clock gates on noisy
shared CI runners.

Keep stage-0 interpreter measurements distinct from the future LLVM/native
backend's compilation and execution measurements.

### 4. Add runtime statistics and Haskell compiler profiling

Instrument the Jazz runtime implementation with opt-in semantic statistics such
as evaluation steps, function applications, continuation depth, closures,
captured environments, list/tuple/ADT constructions, pattern attempts, builtin
calls, host operations, and caches. Emit human-readable or JSON reports to
stderr so program output remains unchanged.

The Haskell interpreter reports logical Jazz allocations. GHC profiling reports
physical byte allocation, heap, and garbage-collector behavior of the current
Haskell implementation. A future native runtime must add allocator-owned
physical statistics for native Jazz execution rather than reusing either
measurement as a substitute.

Also retain GHC profiling as a separate but complementary facility for the
Haskell-written compiler and interpreter. Support time and allocation profiles,
heap profiles, cost-centre stacks, eventlog analysis, and stable markers around
compiler stages and meaningful sub-stages so parsing, lowering, resolution,
analysis, inference, runtime preparation, and evaluation can be measured
independently.

### 5. Add concise data-constructor export groups

Preserve constructor-level export control because abstract data types and
selective constructor exposure are useful API boundaries. Add concise syntax
for exporting a type with all constructors or a selected constructor subset.
Keep the typed per-namespace export inventory as the internal representation.

Implemented syntax is `type Box` for an abstract type, `type Box(..)` for the
type plus every owned constructor, and `type Box(Pack, Empty)` for the type plus
selected owned constructors. Individual `constructor Pack` exports remain
available. The flat typed export inventory remains the internal boundary.

### 6. Upgrade to GHC 9.14.1 and use `MultilineStrings`

Upgrade the active `Jazz` toolchain, package bounds, development shell, and
verification environment to GHC 9.14.1 and `base-4.22`. Treat any broader
Haskell language-edition change as a separate decision rather than silently
coupling it to the compiler upgrade.

Use GHC's `MultilineStrings` extension for hand-written multiline Jazz programs
embedded in Haskell tests. Escaped newline literals and string concatenation are
reserved for tests that directly exercise whitespace, indentation, line ending,
or source-span behavior. Programmatically generated stress inputs may continue
to use builders or combinators instead of pretending to be hand-written source
literals.

### 7. Add source-level diagnostic directives

Support narrowly scoped comment directives for file-level warning policy and
line- or declaration-level diagnostic suppression or promotion. Do not expose
arbitrary optimization, backend, or language behavior as per-line flags.

Parse directives as structured lexer trivia, define CLI and CI precedence, and
base suppression on diagnostic codes/categories and source spans. This work
depends on item 14.

### 8. Expand the Jazz-authored stdlib

Add useful Jazz-written APIs for lists, `Maybe`, `Result`, text, lookup, folds,
searching, transformation, and composition over a minimal kernel substrate.
Initial datatype candidates include `Ordering`, `NonEmpty`, an association-list
structure with honest linear-time behavior, and a two-list queue. A persistent
ordered map and set should follow when `Ord` has a real comparison operation;
hash-based collections should wait for an accepted hashing and native-runtime
contract.

Give existing marker capabilities real operations where useful, including an
ordering operation, user-facing value rendering, and defaults. Do not add
abstractions merely to mirror Haskell.

Batch 5 implemented these foundations as `List`, `Maybe`, `Result`, `NonEmpty`,
`Dictionary`, `Queue`, `Map`, `Set`, `Char`, and `Text`, plus the existing
`IO`/`IOError` boundary. Hash collections remain deferred pending an accepted
hashing and native-runtime contract.

Jazz aims to remain approachable to developers without type-theory or category
theory background. Public functionality analogous to `Semigroup`, `Monoid`, or
other category-theory-heavy vocabulary should use plain, task-oriented Jazz
names. `Foldable` is not automatically excluded, but every new public builtin,
class, datatype, and operation name must receive an explicit naming review
before it is accepted.

### 9. Add TextMate-compatible Jazz syntax highlighting

Add a minimal editor package containing a `.tmLanguage` grammar, language
configuration, `.jz` registration, and representative fixtures. Cover comments,
literals, escapes, numbers, declarations, control flow, types, constructors,
capabilities, purity-marked names, signatures, and operators.

Keep this syntax-only. A language server and semantic highlighting remain
future work.

### 10. Separate Jazz stdlib and compiler sources under one root

Use the locked layout:

```text
jazz/
  stdlib/
  compiler/
```

The stdlib contains general user-facing Jazz modules. The compiler directory
contains the Jazz-authored lexer, parser, compiler data structures, and future
bootstrap implementation. Compiler modules may depend on the stdlib; the stdlib
must not depend on compiler implementation modules.

Production-shaped correctness and benchmark inputs share the
`programs/` corpus rather than being moved under the shipped-source
root. Small, focused fixtures remain under `test/`.

### 11. Remove or justify trivial Haskell aliases

Remove unused aliases that only repeat an exported data constructor and express
no semantic transition. Retain small constructors/helpers when they communicate
phase ownership, centralize validation, or provide a future abstraction
boundary. Apply this through focused API review rather than a mechanical global
replacement.

### 12. Use advanced Haskell features only for concrete invariants

Advanced Haskell features and compiler extensions are allowed; they were never
banned for beginner-friendliness. Use newtypes, `NonEmpty`, phantom phases,
GADTs, type families, or related features when they demonstrably prevent an
invalid compiler state or make a phase boundary safer.

Do not use them as decoration or as a substitute for a smaller ordinary ADT.
Readability and approachability remain real costs to weigh, but not a blanket
reason to reject a feature that materially improves compiler correctness.
Reassess phase-indexed compiler structures when canonical core and the
backend-neutral lowered IR make the relevant invariants clear.

### 13. Consolidate rendering without forcing one universal typeclass

Audit the repeated `renderSomething` functions. Extract duplicated rendering of
the same domain into shared functions or a focused pretty-printing module. Use a
narrow typeclass when several types genuinely share one canonical rendering
operation and composable call sites benefit from it.

Keep explicit functions when output depends on context, such as source syntax
versus diagnostics, qualified versus unqualified names, or runtime values
versus runtime types. A typeclass moves exhaustive constructor handling into an
instance; it does not eliminate the need for total case analysis.

### 14. Unify warnings, errors, and diagnostics

Treat a diagnostic as the common structured user-facing report. Warnings and
errors are diagnostics distinguished by severity and behavior, not separate
duplicated record hierarchies. A warning may be promoted by changing its
severity; an error is fatal independently of warning configuration.

Use one diagnostic representation for severity, stable code, optional warning
category, message payload, primary and secondary labeled spans, notes, help, and
future fix-its. Phase-specific error or warning detail types may convert into
the common diagnostic form through focused constructors or a conversion
typeclass, but they must not duplicate the common presentation fields.

Unify warning and error catalogs and render text, JSON, or future editor output
only at the reporting boundary.

### 15. Replace the warning-named shell mega-gate

Let Cabal discover and run registered Haskell test suites. Add focused
Cabal-registered Haskell repository-audit tests for structural invariants,
source consistency, and project-owned formatting rules. Prefer behavioral,
type-level, or compile-time tests over raw source-string absence checks.

Remove the custom Cabal inventory parser and stop treating direct `runghc`
execution as the canonical test path. Deprecate and remove the compatibility
wrapper if no independent use remains. Retain small shell entrypoints only when
they genuinely orchestrate external tools or Git state instead of duplicating
Cabal or Haskell test logic.

### 16. Audit and compact documentation

Create a clear documentation authority map. Keep canonical language/runtime
contracts, live execution state, implementation status, completed plans, and
historical evidence visibly distinct. Compact oversized status documents by
moving unique normative material into focused specs and replacing repetition
with links.

Remove genuinely stale or redundant active documentation, repair broken links
and current path references, and add checks for stale active-path claims. Do not
rewrite historical plans as though they were current contracts; preserve their
evidence while making their historical status unambiguous.
