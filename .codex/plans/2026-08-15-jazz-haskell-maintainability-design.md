# Jazz Haskell maintainability refactor design

**Date:** 2026-08-15

**Status:** Approved for implementation planning

## Purpose

Make the active Haskell compiler and runtime easier to read, navigate, and
extend without changing Jazz language behavior or weakening the compiler's
existing phase boundaries.

This design follows a live repository audit of two supplied maintainability
reports. It accepts the reports where the source supports them, corrects claims
that overstate the evidence, and adds issues found independently during the
audit. The implementation is a sequence of behavior-preserving refactors, not
a compiler rewrite and not a redesign of the public language.

## Current evidence

At the audited revision, active Haskell production code is approximately
50,000 lines and active Haskell test code approximately 87,000 lines. The main
navigation hotspots are:

| Module | Approximate size | Responsibilities currently co-located |
| --- | ---: | --- |
| `TypedCore/Validate.hs` | 5,669 lines | program ordering, visibility, declarations, types, recipes, expressions, patterns, evidence, interfaces |
| `LoweredIR/Lower.hs` | 3,682 lines | runtime requirements, callable shapes, capture/profile analysis, CFG and instruction emission |
| `Runtime.hs` | 3,428 lines | public wrappers, evaluator machine, scope evaluation, application, observation adaptation |
| `TypeInference/Elaboration.hs` | 2,750 lines | provisional data use, callable/capture analysis, recursive finalization |
| `TypeInference/Scope.hs` | 2,566 lines | scope preparation, recursive groups, declaration inference, ordered traversal |

The largest test hotspots include a Typed Core contract specification above
16,000 lines and direct-call fixture/spec modules above 7,000 and 5,000 lines.
These measurements identify review and navigation pressure; they are not
architectural limits and will not become line-count gates.

The audited Nix development shell passed:

- `cabal build all -fdevelopment`;
- `cabal check`; and
- `git diff --check`.

The repository therefore starts this work from a compiling baseline. The
ambient host Cabal is too old for the project's `MultilineStrings` syntax, so
all authoritative build and test verification must use the checked-in Nix
development environment.

## Audit conclusions

### Findings accepted

The reports are correct that:

- the large validation, elaboration, lowering, and runtime modules combine
  responsibilities that can be separated along existing semantic boundaries;
- long positional APIs obscure call-site meaning, especially
  `inferScopeTypeInternal` and the recursive `finalizeExpression` helper;
- `ModuleResolver` defines a second `ResolvedModule` summary beside the richer
  `ModuleGraph.ResolvedModule` and accumulates both during traversal;
- type inference duplicates diagnostic forcing because the current ownership
  graph makes `Force` unusable from `TypeInference` without a cycle;
- `TypeInference.Pattern` contains an explicit partial `error` for a state its
  producer never intends to create;
- several private runtime, driver, and inference forwarding wrappers have no
  repository consumers;
- oversized test modules mix domain fixtures, expected artifacts,
  orchestration, and regression inventories; and
- most phase-specific representations are justified because they carry
  different invariants and serve different consumers.

### Findings corrected or narrowed

The implementation must not treat every report recommendation as established
fact:

- `TypedCore.Validate.ModuleContext` having many fields does not by itself make
  it a mistaken abstraction. Expression validation genuinely needs several of
  those domains. The validator must be split in dependency order, with shared
  context kept coherent, rather than mechanically partitioned by field count.
- provisional elaboration types are Cabal-private compiler contracts used by
  inference producers and focused tests. They should move to an internal types
  module, but making all constructors opaque now would add ceremony without a
  useful invariant boundary.
- `RuntimeOutcome` is already the canonical result in observed and host-aware
  execution paths. The concrete defect is duplicated outcome adaptation in
  `Runtime` and `ModuleRuntime`, plus outcome ownership in the semantically
  narrower `Runtime.Observation` module.
- moving only `InferenceResult` cannot resolve the forcing cycle. Diagnostic
  strictness must also move to a neutral dependency that both inference and
  `Force` can use.
- splitting the runtime machine from scope evaluation is not initially a
  mechanical file move because those paths call each other. Shared outcomes
  and request/context types must be extracted first so later splits follow an
  acyclic dependency graph.
- HLint output is not currently a clean architectural signal. The pinned HLint
  parser rejects valid syntax used by the supported compiler, while the pinned
  formatter reports repository-wide historical drift. This work will format
  touched Haskell and make tool compatibility explicit; it will not hide a
  large unrelated mass-format in the refactor.

### Additional findings

The audit found these issues beyond the supplied reports:

- `ModuleResolver.ResolvedState` stores both the compatibility summary list and
  the production graph-module list, so the duplicate `ResolvedModule` is also
  duplicated traversal state rather than only a naming problem.
- pattern binders are stored as the general `TypeEnv`/`TypeBinding` even though
  this path constructs only plain expression-type bindings. That overly broad
  representation is the direct cause of the partial failure.
- `Runtime` and `ModuleRuntime` independently define conversions from
  `RuntimeControl` and diagnostics to `RuntimeOutcome`, the reverse diagnostic
  adapter, and legacy-exit diagnostic construction.
- callback types used by inference traversal are owned by
  `TypeInference.Diagnostics`, even though they are not diagnostic concepts.
- lowering already contains a natural, self-contained runtime-requirement
  traversal and a distinct shape/profile analysis before emission. These are
  stronger extraction seams than file size alone.
- some apparently public wrapper matrices are only private-library APIs. Their
  definitions and exports should be removed when no active source or test
  consumes them rather than preserved indefinitely as speculative
  compatibility.

## Accepted constraints

The refactor must preserve:

- public Jazz syntax and semantics;
- the documented phase model from surface and canonical forms through Typed
  Core, Lowered IR, and runtime values;
- the `ValidatedTypedProgram` and equivalent validation proof boundaries;
- diagnostic content, ordering, and failure precedence unless a focused test
  proves that an existing ordering was accidental and a separate behavior
  decision is approved;
- the ordinary canonical-core execution path and the opt-in Typed Core/lowered
  path;
- hosted-Haskell parity contracts; and
- deterministic artifact construction and test fixtures.

Cabal-private Haskell APIs and test-only helpers may change. No public language
documentation should change unless verification finds an existing document
that incorrectly describes current behavior.

The refactor must not introduce:

- a universal compiler AST;
- a phase-wide typeclass hierarchy;
- a generic visitor framework spanning unrelated representations;
- one global `CompilerContext`;
- a generic fixture DSL without demonstrated duplication savings;
- mirrored stage-0 Jazz module churn when no hosted schema or behavior changes;
  or
- source-file size budgets as CI policy.

## Architectural approach

The work uses invariant-first modularization. It first narrows invalid states
and gives shared contracts one neutral owner. It then replaces ambiguous
argument lists with named records. Only after those dependencies are explicit
does it split large orchestration modules along proven data-flow seams.

The order is important:

```text
make invalid states unrepresentable
             |
             v
centralize shared contracts and adapters
             |
             v
name request, environment, and location data
             |
             v
extract analysis from orchestration/emission
             |
             v
split validator/runtime where dependencies are now acyclic
             |
             v
rehome domain tests and remove obsolete private APIs
```

Each milestone must compile and pass its focused tests before the next
milestone begins. Moves that are behavior-neutral should remain separate from
semantic or invariant changes so review can distinguish them.

## Component design

### 1. Pattern binding invariant

`TypeInference.Pattern` will represent bindings produced by a pattern with a
dedicated map from `Name` to `ExpressionType`, rather than the general
`TypeEnv` containing any `TypeBinding` constructor.

Pattern combination and duplicate-binder consistency checks will operate on
that dedicated type. Conversion to `PlainTypeBinding` occurs only at the
boundary where an arm's environment is extended.

This removes `patternBindingExpressionType` and its internal `error`. The
impossible non-plain case becomes unrepresentable, and future type-binding
constructors cannot silently widen the pattern path.

Focused tests will cover successful binder inference, duplicate binders with
matching types, incompatible duplicate binders, nested patterns, and arm
environment extension.

### 2. Canonical resolved-module artifact

`ModuleGraph.ResolvedModule` will be the sole production representation of a
resolved module. `ModuleResolver.ResolvedState` will accumulate only graph
modules. Resolver entry points that currently return summary modules will
return the rich graph result or a projection derived at the boundary.

Summary-only tests will define local projections or assertions over the rich
record. The resolver-local `ResolvedModule` type, its same-named constructors,
and the duplicate reverse accumulator will be removed.

Any remaining compatibility resolution mode must be evaluated by active call
sites, not its name. A mode used only by a focused recursion test may be kept as
an explicit test fixture when it still verifies a live boundary; otherwise it
will be removed with its dead branch.

### 3. Neutral inference result and diagnostic strictness

`InferenceResult` and structural result types needed by both inference and
forcing will move to `Jazz.Compiler.TypeInference.Result`.

Diagnostic-specific deep evaluation will move to a neutral focused module,
`Jazz.Compiler.Diagnostics.Strictness`. `TypeInference` will call that neutral
function, while `Jazz.Compiler.Force` will reuse and, where compatibility
requires, re-export it. Generic forcing combinators should remain with the
existing forcing implementation unless moving one is necessary to keep the
neutral module acyclic.

There will be one implementation of diagnostic and diagnostic-label forcing.
The split must not make diagnostics, runtime, module compilation, and inference
mutually dependent.

### 4. Elaboration contracts and finalization context

Provisional elaboration ADTs, callable shape data, capture data, expression
roles, and finalization failure types will move to
`TypeInference.Elaboration.Types` when they are shared by more than one
producer module. These remain explicit Cabal-private records and constructors;
no opaque smart-constructor layer is added without a new invariant to enforce.

The recursive finalizer will replace its long positional argument list with:

- `FinalizationEnv`, containing immutable program/function data, callable
  shapes, scalar capture information, and eager-closure information; and
- `FinalizationLocation`, containing statement identity, child path,
  parameters, evaluation timing, and expression role.

Recursive calls update the location deliberately rather than rebuilding an
unlabelled sequence of values. The refactor must preserve child paths and
failure ordering exactly.

After the types and records are established, callable/capture/profile analysis
may move to `TypeInference.Elaboration.Profiles` and final construction to
`TypeInference.Elaboration.Finalize`. The existing `Elaboration` module stays
as a small private façade for current consumers.

Traversal callback aliases currently owned by `TypeInference.Diagnostics`
will move to the narrow neutral type module that owns the corresponding
inference contract. Diagnostics will consume those aliases rather than own
them.

### 5. Scope inference request

`inferScopeTypeInternal` will take a named `ScopeInferenceRequest` instead of a
Boolean followed by seven positional inputs. The current Boolean becomes an
explicit sum type such as `ForwardSignedFunctionsPolicy`, with constructors
that communicate whether forward signed functions are permitted.

The request separates:

- immutable modes, callback, prelude indices, and initial environment;
- the initial inference state; and
- the prepared scope being traversed.

This design does not create a compiler-wide context and does not hide state
transitions behind a new monad. Existing public convenience functions remain
thin, while definition-only private wrappers are removed.

Because the current internal function is itself very large, the first pass
only names its inputs and extracts cohesive local operations with clear inputs
and outputs. A follow-on file split is allowed only when those helpers no
longer require the entire request and traversal state.

### 6. Runtime outcome ownership

A neutral `Jazz.Compiler.Runtime.Outcome` module will own both
`RuntimeControl` and `RuntimeOutcome`, plus the canonical conversions:

- control result to runtime outcome;
- diagnostic result to runtime outcome;
- runtime outcome to the legacy `Either Diagnostic value` boundary; and
- legacy exit/control diagnostic construction.

Moving `RuntimeControl` together with `RuntimeOutcome` avoids an import cycle
between `Runtime.Types` and the neutral outcome module. `Runtime.Types` and
`Runtime.Observation` may re-export moved names temporarily when that makes a
reviewable transition, but only `Runtime.Outcome` implements the adapters.

`Runtime` and `ModuleRuntime` will delete their duplicated local copies and
use the shared conversions. `RuntimeOutcome` remains the internal canonical
result. `Either Diagnostic value` remains only at compatibility entry points
that still have active consumers.

After this extraction, runtime entry points will use a small named execution
request/options record where several host, observation, and source-unit
parameters currently travel together. Definition-only wrappers in `Runtime`
and `Driver` will be removed after exact repository call-site checks.

### 7. Lowering analysis and emission

Lowering will be separated along the source's existing execution order rather
than through a generic lowering framework:

- `LoweredIR.Lower.Types` owns internal lowering request/state and shared
  analysis results;
- `LoweredIR.Lower.Requirements` performs the structural runtime-support
  requirement collection;
- `LoweredIR.Lower.Shapes` owns callable shape, capture, and supported-profile
  analysis; and
- `LoweredIR.Lower` remains the façade and initially owns CFG/instruction
  emission, with an internal `Emit` module added only when the shared types make
  that dependency clean.

Analysis modules must consume validated Typed Core and return named immutable
results. They do not allocate blocks or temporaries. Emission consumes those
results and owns mutable lowering state. This prevents future feature work from
mixing semantic discovery into instruction emission.

`LoweredIR.Validate` remains independent and unchanged except for import moves
required by the new ownership.

### 8. Typed Core validator split

The validator split will proceed topologically, not by copying arbitrary
regions into seven files at once.

The first extraction establishes internal modules for shared context/path
operations and total lookup helpers. The three guarded `Map.!` lookups used by
recursive-group ordering will become total operations whose failure produces
the existing structured validation failure. The proof that preceding
validation made a key present must no longer live only in control flow.

Subsequent extractions follow dependency direction:

1. type, recipe, and scheme validation;
2. names, visibility, imports, exports, and declaration contracts;
3. patterns and binder agreement;
4. evidence, capability, and implementation checks;
5. expression and ordered-statement traversal; and
6. program/module orchestration.

`TypedCore.Validate` remains the public façade exposing the existing entry
points. Internal modules may share one coherent read-only validation context.
The split must not introduce competing partial contexts that are repeatedly
reassembled at every expression.

### 9. Runtime implementation split

The runtime will be split after outcome and request ownership is stable.
Dependency-safe target responsibilities are:

- `Runtime.Machine`: controls, frames, continuations, stepping, and machine
  completion;
- `Runtime.Apply`: callable dispatch, currying, and application;
- `Runtime.Scope`: scope-plan execution, binding publication, and module-scope
  orchestration; and
- existing `Runtime.Semantics`, `Runtime.Primitives`, `Runtime.Observation`, and
  `Runtime.ScopePlan` retain their current focused roles.

If `Machine`, `Apply`, and `Scope` still form a call cycle after shared request
and outcome extraction, their mutually recursive orchestration stays together
until a narrower callback or command/result seam is demonstrated by the code.
Avoiding a new module cycle takes priority over reaching a target file list.

`Runtime.hs` remains the compatibility façade for actively used entry points.
No host effect moves into the pure evaluator, and runtime observation remains a
separate concern from execution outcome.

### 10. Test and fixture ownership

Large Typed Core contract and direct-call suites will be split by the contract
they verify, with one small aggregator per suite. Expected domains include:

- modules, imports, exports, and ordering;
- declarations, schemes, types, and recipes;
- expressions, application, and diagnostics;
- captures, recursion, and callable shapes;
- lowering requirements and runtime services; and
- validator and hosted-parity contracts.

Fixtures move with the domain that owns their semantics. Shared constructors
belong in small domain-specific fixture modules only after at least two suites
need them. Large exact expected artifacts remain acceptable where they protect
the compiler contract; the refactor targets ownership and navigation, not test
brevity at the expense of precision.

Tests must assert behavior and invariants, not source line counts, helper names,
or the number of internal modules.

### 11. Private API pruning

Before retaining a forwarding wrapper, the implementation will search active
`src`, `app`, and `test` consumers. A wrapper with no consumer beyond its own
definition and no required façade role will be removed with its export.

Where several wrappers are used, one fully configured internal request
function and a small number of stable convenience entry points will remain.
This applies separately to type inference, runtime, module runtime, and driver
APIs; it does not justify one universal execution request across phases.

### 12. Tooling and regression prevention

Prevention is primarily structural:

- dedicated types prevent pattern bindings from containing unsupported binding
  variants;
- neutral owner modules prevent duplicated result adapters and forcing logic;
- one resolved-module artifact prevents parallel resolver state;
- request/environment records make mode additions named rather than
  positional;
- analysis results prevent lowering emission from rediscovering semantic
  facts; and
- façades define narrow dependency directions.

A narrow repository source-policy test will cover active compiler Haskell and
reject explicit partial escape hatches that this refactor removes, including
`error` and the qualified partial map lookup operator. The check will be
syntax-aware if the supported parser can handle all active source; otherwise a
small, documented lexical check is acceptable only for those exact constructs.
It must exclude comments, strings, generated files, and tests or prove that its
matching cannot produce false positives. It will not grow into a general style
linter.

Touched Haskell modules must pass the repository formatter. The implementation
will record and use a formatter version compatible with the supported GHC
syntax before considering repository-wide enforcement. Existing unrelated
format drift is not part of this batch. HLint becomes a gate only after its
parser version accepts the active language extensions; hints are then reviewed
for semantic appropriateness rather than applied mechanically.

## Error and compatibility behavior

The refactor introduces no new user-facing error category. Totalized internal
lookups return the structured failure already owned by their phase. If an
internal condition previously believed impossible is reached and no matching
structured failure exists, implementation must add a focused internal
invariant failure at that phase boundary rather than crash or reuse an
unrelated public diagnostic.

Diagnostic ordering remains stable. Conversions between runtime controls,
runtime outcomes, and compatibility diagnostics become shared but retain their
current values. Removing dead private wrappers does not authorize changes to
active CLI behavior or library results.

## Verification strategy

Every invariant or behavior-adjacent change starts with a focused failing test.
Pure file moves and export-list reductions may use characterization tests that
are already green, but each commit must compile before proceeding.

Focused verification includes:

- pattern inference and coverage tests;
- module graph and resolution tests;
- inference strictness/forcing tests;
- Typed Core producer and contract tests;
- Lowered IR producer, validator, and runtime-requirement tests;
- runtime, module runtime, observation, and driver tests; and
- repository policy tests for the exact partial constructs removed.

Milestone closeout runs, inside the checked-in Nix shell:

```text
cabal build all -fdevelopment
cabal test all -fdevelopment --test-show-details=direct --jobs=1
cabal check
```

It also runs the compatible formatter check for touched Haskell, relevant
repository/documentation checks, and `git diff --check`. Tests should be run
serially where the existing suite or runtime environment requires deterministic
resource use.

## Delivery sequence

The implementation will be planned and reviewed as four independently
verifiable milestones under this umbrella design:

1. **Invariant and ownership cleanup**: pattern bindings, total validator
   lookups, runtime outcome adapters, inference results/strictness, and the
   canonical resolver artifact.
2. **Inference navigation**: elaboration types/finalization records, scope
   request/mode, traversal callback ownership, and unused inference wrappers.
3. **Lowering and runtime navigation**: requirement/shape analysis extraction,
   runtime request ownership, dependency-safe runtime splits, and runtime/driver
   wrapper pruning.
4. **Validator and test navigation**: staged validator modules, large suite and
   fixture rehoming, formatter/tool compatibility, and final policy coverage.

Each milestone should use small commits that separate moves, API adaptation,
and invariant changes. The repository guidance to commit along the way applies
throughout. If a milestone reveals that a proposed split would create a cycle
or increase context plumbing, the implementation pauses at the smaller clean
boundary and records that evidence rather than forcing the target file layout.

## Completion criteria

This maintainability pass is complete when:

- the explicit pattern-inference crash and guarded partial validator lookups
  are gone;
- resolver traversal maintains one resolved-module representation;
- diagnostic forcing and runtime outcome adaptation each have one
  implementation owner;
- the named request/environment records replace the audited long positional
  interfaces;
- lowering requirements and shape/profile analysis are independent from
  emission state;
- validator and runtime responsibilities are split only where imports remain
  acyclic and context flow becomes clearer;
- definition-only private wrapper matrices are pruned;
- the largest contract and direct-call tests have domain-based navigation;
- focused and full Nix verification passes; and
- the working tree contains no unrelated formatting or behavior changes.

## Approaches rejected

### Literal file-first splitting

Immediately moving every suggested responsibility to the filenames listed in
the reports would create broad import churn before shared contracts have clear
owners. In the validator and runtime it could replace one large module with a
cycle or repeated context plumbing. The accepted design establishes dependency
seams first.

### Minimal local cleanup only

Replacing the single `error`, renaming one resolver type, and deleting a few
wrappers would remove obvious symptoms but leave duplicate state, ambiguous
interfaces, and analysis/emission coupling in place. That would not make the
next compiler stages materially easier to implement.

### Representation collapse or universal frameworks

Merging phase ADTs or introducing generic visitors and typeclasses would erase
useful invariant boundaries and trade visible local duplication for abstract
cross-phase coupling. The current phase model is a strength; ownership and
module boundaries around it are what need repair.
