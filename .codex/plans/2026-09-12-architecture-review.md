---
id: JN-COMPILER-ARCHITECTURE-REVIEW-20260912
status: complete
priority: P2
size: M
kind: design
autonomous_ready: no
depends_on: []
last_verified: 2026-09-12
plan_section: "Review"
target_paths:
  - src/Jazz/Compiler/
  - test/Jazz/Compiler/
verification:
  - Source and history review with focused correctness probes under GHC 9.14.1
  - bash scripts/check-execution-queue.sh
deliverable: Evidence-backed architecture and code quality review with bounded recommendations and explicit retention decisions.
supersedes: []
---

# Compiler architecture review

## Review

Reviewed `b0b7dfca564b35613319fe1855479a74c6d509d0..171b22abbdc838f97e4b6317eccab0314d97ed60`: PR 158's architecture rewrite and the two local cleanup commits. Line references below name the reviewed head. This report records review completion, not implementation of its recommendations. No implementation work is dispatched by this report.

Three subagents performed six bounded review passes: runtime; inference/analyzer; parser/resolver; module/driver/interfaces; test quality/quality gates; and cross-cutting identity/evidence invariants. The agent-thread limit prevented creation of additional fresh reviewers, so completed reviewers took the additional subjects. The coordinator independently checked findings, traced consumers/history, and ran the correctness probes. Following the maintainer's correction, the review used ordinary architecture and code quality criteria without Ponytail.

Result: two reproduced correctness issues, two small architecture cleanups, two optional quality improvements, and one compatibility retirement candidate requiring a deliberate decision. Neither correctness mechanism is newly introduced by this branch; both remain in the reviewed code after the rewrite. No general compiler redesign is indicated by these findings.

## Correctness findings

### A1 — P2: recognize raw prepend by resolved callable identity

**Location:** `src/Jazz/Compiler/TypeInference.hs:486–491`; consumers at lines 483 and 502.

`builtinListPrependRawExpr` recognizes any variable with the spelling `__kernel_listPrependRaw`. Resolution allows a local lambda parameter to shadow that builtin, and ordinary application inference correctly checks the supplied function. The specializer then overwrites its result type with the second argument's list type and refines the callable draft as though it were the primitive.

Reproduced through the public source execution API:

```jazz
invoke = \(__kernel_listPrependRaw) -> __kernel_listPrependRaw 1 [True].
invoke (\(n, xs) -> n) + 1.
```

This fails with `E2003`, reporting operands `[Bool]` and `Int`. Renaming only the parameter to `f` returns `2`. Without the arithmetic use, both forms return `1`, which demonstrates why a result-only smoke case misses the incorrect inferred type.

**Recommended change:** make the recognition decision from the resolved reference and type environment, and reuse it for result specialization and draft refinement. Recognize genuine builtin references and `BuiltinAliasTypeBinding BuiltinListPrependRaw`: the lexical kernel bridge in `jazz/stdlib/Prelude.jz:263` must retain its representation specialization. Merely checking a different spelling or accepting only direct builtin references is insufficient.

**Scope:** small, localized correctness fix. Add a behavioral shadowing case that consumes the inferred result, while retaining existing concrete list representation coverage. The spelling predicate already exists at baseline `b0b7dfca`; the rewrite carries it into the new checked-draft path. Confidence: high, source-level reproduction.

### A2 — P2: use nominal identity for explicit ADT result hints

**Location:** `src/Jazz/Compiler/Runtime/Semantics.hs:521–527`.

`runtimeValueCanAcceptTypeHint` still compares `identifierText` in both ADT cases, while the downstream `applyRuntimeTypeHint` uses nominal equality. Equivalent defining/imported views can therefore fail the guard before reaching the corrected matcher. Different owners with the same local spelling can pass it and lose an existing annotation when the downstream matcher unwraps the value but rejects the new nominal type.

The three-case direct runtime probe reproduced:

- Defining `Lib::One`'s local `Box` value rejects the equivalent imported `Lib::One::Box` result hint.
- The reverse view likewise fails to acquire the valid hint.
- An unrelated owner's same-spelled `Box` hint strips a correctly annotated phantom `Box(Bool)`. Exact matching against `Box(Int)` changes from `False` before the operation to `True` afterwards.

These are active runtime helpers: callable application records `ApplyExplicitResultHint` in `Runtime/Engine.hs:1445–1456`, and return-policy discharge calls the helper at lines 1864–1865. Existing nominal tests cover `applyRuntimeTypeHint`, not this guard. The defect is reproduced at the runtime API boundary; an end-to-end source example for these precise ADT view cases was not established.

**Recommended change:** replace both rendered-name comparisons with `typeName == runtimeConstructorTypeName shape`. Preserve arity, saturation, and the distinct explicit-result policy. Extend the existing nominal behavior coverage to this entrypoint, including annotation preservation for unrelated owners.

**Scope:** two local comparisons plus focused behavioral coverage. The comparisons predate the branch; the adjacent nominal-identity cleanup missed them. Confidence: high at the runtime boundary; source-level impact is not separately proven.

## Small architecture cleanups

### A3 — P3: remove the abandoned forward-binding diagnostic result

**Location:** `src/Jazz/Compiler/TypeInference/Scope.hs:315–321`, produced at lines 616–621 and returned at line 421.

`forwardAnalysisBindings` used to feed analyzer inputs. Every current caller discards it: `TypeInference.hs:173`, `Scope.hs:306`, and `Scope.hs:336`. No test reads the returned map. The scope API still advertises a diagnostic handoff that no longer exists.

**Recommended change:** return `(CheckedScope, InferState)` directly, remove the map reconstruction, and give the root-scope entrypoint a name that describes its current role. This is low risk and removes an obsolete protocol; laziness means it should not be advertised as a measured performance improvement.

Do not fold removal of the whole forward-signature policy into this change. Current production mode combinations cannot populate it, but the direct test API still admits other combinations. Its retirement needs a separate behavior review. `InferenceMode`, recursive previews, and expected-type checking remain active.

### A4 — P3: derive module analysis ownership from the resolved root

**Location:** `src/Jazz/Compiler/ModuleAnalysis.hs:111`, sole owner-callback use at line 158.

`analyzeModule` accepts a source-owner callback alongside a resolved module that already carries its owner. The ordinary caller copies the root owner into `const` (`ModuleCompiler.hs:87–89`); the prelude caller supplies the same owner already stored by prelude resolution. Both direct test calls also agree with the resolved artifact.

**Recommended change:** remove the callback from `analyzeModule` and `moduleInferenceInputs`, and inspect `resolvedNodeOwner` on `coreModuleBodyNode`. This leaves one source of truth and prevents contradictory standalone/named classifications. Two production and two test calls need adjustment.

**Scope:** low. Keep `hideRootBindings`, which controls the independent bundled-prelude warning policy. Do not substitute `isStandaloneSourceModule`: an in-memory artifact with a named source header has a different ownership distinction. No current caller mismatch was found.

## Optional quality improvements

### A5 — consolidate module capability fields using the existing neutral record

**Location:** `src/Jazz/Compiler/ModuleInterface.hs:77–81`.

The five interface capability fields duplicate `ScopeCapabilityFacts`. `TypeInference.moduleInterfaceFromState` splits that record into five fields, and `ModuleAnalysis.importSelectedInterface` reconstructs it. Publication at `ModuleInterface.hs:97–101` repeats transformations already implemented by `publishCapabilityNames` at lines 214–221.

**Recommended change:** consider `interfaceCapabilities :: ScopeCapabilityFacts` and reuse the existing publication helper. This reduces synchronization obligations without introducing another abstraction. No current fact inconsistency was found.

**Scope:** moderate representation migration across four production modules and two test modules. Preserve export filtering and `ImportedInterface`'s explicit implementation-method deduplication; ordinary `ScopeCapabilityFacts` combination appends lists and is not interchangeable. This is useful maintenance work, not a merge blocker. Carrier duplication predates the branch; the new publication code makes its maintenance cost more visible.

### A6 — keep synthetic runtime fixture identities internally consistent

**Location:** `test/Jazz/Compiler/Semantics/Runtime/ResolvedFixture.hs:85–87`.

The adapter allocates fresh node/binder IDs and restores authored facts while replacing only their resolution fields. `Fixtures.hs:217` leaves explicit instantiations targeting the sentinel `StandaloneSourceUnit/-1`; constructor binder lists and scheme-map keys at lines 334–336 retain their old IDs. The named dependency fixture in `HostIOTests.hs:705–743` is an active example.

Current runtime tests succeed because execution reads instantiation arguments and constructor scheme values without these keys. This is fixture consistency debt, not a production defect. Such fixtures cannot be evidence that lexical and semantic identities agree.

**Recommended change:** locally rebase constructor binder/scheme keys from the resolved constructor node and derive explicit-instantiation targets from the resolved callable reference. Preserve deliberately authored semantic types and malformed facts in tests that explicitly exercise rejection. Do not add a generic remapping system: the original sentinel IDs collide.

**Scope:** modest test-only work introduced by the branch's fixture reindexing. Real compiler identity contract tests remain valuable and should be retained.

## Compatibility retirement candidate

`Runtime/Types.hs:281–297` retains the list-facing `VQualifiedMethod` pattern, but repository search found no callers: active engine, semantics, fixtures, and observation code use `VQualifiedMethodApplication`. Reconstructing through the old pattern also loses selected-method state. `weeder-production.toml:4` incorrectly claims the CLI matches it.

However, the August 30 implementation plan explicitly retained this compatibility API at lines 1468–1471. Treat removal as a deliberate retirement of that commitment, rather than silently classifying it as ordinary dead code. If retired, remove the pattern/export and its `COMPLETE` declaration; retain the actively used `VConstructor` pattern and candidate helpers with other consumers. No present runtime bug is attributed to an adapter with no caller.

## Retentions and rejected candidates

- Shared program and lexical-scope traversal have actual single owners. No surviving duplicate legacy evaluator was found.
- Lazy and deferred cells have different cycle-detection, effect, and cache lifetimes. Their storage redesign and performance follow-ups remain outside this review's recommendations.
- Private checked drafts, nominal identities, meaningful analyzed facts, preview/rejected-pattern state policies, dynamic dispatch, result controls, and distinct runtime outcomes have concrete consumers or accepted contracts.
- Reachable private-type metadata does not make private names importable. Validated import selection and implementation-method deduplication remain necessary.
- An apparent imported-class overwrite required unsupported re-export behavior and was discarded after tracing public interface construction and existing tests.
- The suspected named-versus-standalone prelude difference did not reproduce: both tested forms produced the same `E1001` diagnostics.
- The hosted frontend and Bootstrap tests remain explicitly retained. Existing deferred hosted qualification parity work is not reopened.
- Earlier ledger ideas requiring allocation/retention measurements or broader representation redesign are not relabeled as newly discovered defects.

## Verification and limits

The current library build was confirmed under pinned GHC 9.14.1 with `cabal build lib:jazz-internal --builddir=/private/tmp/jazz-pr158-build --offline --jobs=4`. Focused probes were rerun against that build. The kernel-shadowing probe checks four source executions; the nominal-result probe deliberately exits nonzero after reproducing its three violated expectations. The prelude probe checked four source executions and withdrew that candidate.

Local evidence: `/private/tmp/jazz-pr158-kernel-shadow-review.hs`, `/private/tmp/jazz-pr158-kernel-shadow-review.log`, `/private/tmp/jazz-runtime-nominal-result-hint-probe.hs`, `/private/tmp/jazz-pr158-nominal-hint-review.log`, and `/private/tmp/jazz-pr158-prelude-review.hs`.

This review did not modify compiler or test code, rerun the full test matrix, execute benchmark/performance/profiling/scale/corpus-budget workloads, refresh PR comments, or publish anything. The review report is the only repository change. The subsequent approved implementation is recorded below.

## Implemented follow-up

The maintainer approved all six findings and explicit retirement of `VQualifiedMethod`. All seven are implemented locally in `3e20b7a9`; the [fixes plan](2026-09-12-architecture-review-fixes.md) records the exact scope, red-to-green evidence, cross-reviews, and completed verification. Twelve distinct correctness suites, complete clean builds, both Weeder checks, HLint, formatting and documentation checks passed. Nothing was pushed, and the retained architecture and execution exclusions remain in force.
