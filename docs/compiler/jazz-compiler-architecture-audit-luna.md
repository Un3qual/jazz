# Jazz compiler architecture audit

Date: 2026-09-10

## Scope and method

This is a code-only architecture audit of `src/Jazz/Compiler/**/*.hs` at the
current checkout. It does not use repository documentation, plans, tests,
commit history, or earlier audits. The focus is program and data movement:
representation ownership, phase boundaries, state flow, identity, interfaces,
and runtime handoffs. Local function shortening is out of scope.

The directory contains 88 Haskell files and approximately 37,171 physical
lines, including comments and blank lines. The largest concentration is in
`Runtime/Engine.hs`, `TypeInference/Scope.hs`,
`TypeInference/Capabilities.hs`, `TypeInference.hs`, and
`ModuleResolver.hs`.

## Executive conclusion

The compiler is over-engineered mainly because it has accumulated several
parallel models of the same program.

It has a phase-indexed core AST, which is a reasonable design, but it also has:

- a separate surface AST that remains relevant after lowering;
- raw token rescans after parsing;
- positional statement ownership and repeated node reindexing;
- inference output stored in many `CoreNodeId`-keyed maps;
- a later pass that reconstructs analyzed nodes from those maps;
- several overlapping module-interface and export representations;
- a runtime plan interpreted between analysis and evaluation;
- separate pure and host runtime walkers;
- multiple result, diagnostic, observation, and driver wrappers.

The result is not one compiler pipeline. It is a compiler, an analyzer, an
interpreter, a host-effect layer, and a profiling system sharing a collection
of partially overlapping data contracts.

## Data flow

```text
source text
  -> tokens
  -> SurfaceExpr
       ├─ lowering -> Expr/CoreModule 'Lowered
       ├─ export/reference discovery
       └─ raw-token rescans for selected source locations
  -> CoreProgram 'Resolved
  -> module analysis
  -> InferState
       ├─ solver state
       ├─ declaration state
       ├─ capability state
       └─ node-indexed output maps
  -> Analyzer traversal
  -> analyzed-tree attachment traversal
  -> CoreProgram 'Analyzed
  -> ModuleInterface / ImportedInterface
  -> RuntimeScopePlan / RuntimePlan
  -> pure or host evaluator
  -> observations/outcomes
  -> Driver result
```

The navigation cost is relational. To understand one expression's type or
runtime behavior, a reader often has to follow an AST node into a state map,
then into an attachment function, then into a runtime consumer.

## Ranked findings

### 1. P1 — shrink: the phase-indexed AST is paired with a detached semantic database

`CorePhase`, `FactsAt`, and `CoreNode` are defined at
[AST.hs:60](../../src/Jazz/Compiler/AST.hs#L60) and
[AST.hs:78](../../src/Jazz/Compiler/AST.hs#L78). This suggests that the AST is the
authoritative carrier for phase-specific meaning.

Inference does not actually produce that final carrier. `InferenceOutput`
stores expression types, binary operations, evidence, explicit
instantiations, pattern facts, and statement facts in separate maps at
[State.hs:128](../../src/Jazz/Compiler/TypeInference/State.hs#L128). A later
attachment pass joins those maps back onto the resolved AST at
[Analyzed.hs:223](../../src/Jazz/Compiler/TypeInference/Analyzed.hs#L223).

Runtime then consumes the reconstructed facts and runtime plans at
[Engine.hs:1498](../../src/Jazz/Compiler/Runtime/Engine.hs#L1498).

This creates a database-style synchronization protocol:

```text
resolved AST
  + expression-type map
  + operator map
  + evidence map
  + instantiation map
  + pattern map
  + statement map
  -> analyzed AST
```

Missing facts, duplicate facts, stale node IDs, and special-case insertions
are now architectural failure modes. The compiler has the cost of an analyzed
tree without letting analysis produce one directly.

### 2. P1 — shrink: `InferState` is a cross-cutting journal rather than narrowly owned inference state

`InferState` combines solver state, declarations, module capabilities, visible
types, evidence candidates, semantic output, diagnostics, coverage state, and
invariant failures. The four major state domains are visible at
[State.hs:99](../../src/Jazz/Compiler/TypeInference/State.hs#L99) and
[State.hs:173](../../src/Jazz/Compiler/TypeInference/State.hs#L173).

Different subsystems manipulate the same conceptual journal through different
effect protocols:

- explicit `(value, state)` threading;
- `Maybe InferState` for failed unification;
- `State InferState` in implementation checking;
- nested scope state containing another complete inference state;
- field-specific rollback and watermark snapshots.

The scope walker contains a full inference state inside its own state product,
while pattern and capability code selectively preserves or rolls back subsets
of the journal. Each caller must know which fields are speculative and which
must survive.

This is mutable bookkeeping encoded in immutable records. The need for
speculation is real; the broad shared journal and partial rollback contracts
are avoidable navigation cost.

### 3. P1 — shrink: module dependencies cross a copied, parallel interface boundary

`ModuleInterface` already contains independent maps and sets for exported
values, data types, classes, methods, capabilities, and implementations at
[ModuleInterface.hs:51](../../src/Jazz/Compiler/ModuleInterface.hs#L51).

`ModuleCompiler` then carries dependency information as a tuple containing an
export inventory, interface, binder inventory, and evidence candidates at
[ModuleCompiler.hs:94](../../src/Jazz/Compiler/ModuleCompiler.hs#L94).

That tuple becomes `ImportedInterface`, which has more parallel fields at
[ModuleAnalysis.hs:271](../../src/Jazz/Compiler/ModuleAnalysis.hs#L271). Import
selection and rebasing independently transform those maps. The rebasing layer
starts at [ModuleAnalysis.hs:436](../../src/Jazz/Compiler/ModuleAnalysis.hs#L436)
and handles types, schemes, capabilities, method keys, and evidence
separately.

Import filtering and name rebasing are necessary semantics. The avoidable part
is materializing multiple overlapping views of one dependency boundary and
requiring their merges and rebases to stay synchronized.

### 4. P1 — shrink: prelude handling is an out-of-band positional protocol

The prelude has two downstream identities. It can remain a separate artifact,
or it can be spliced into a standalone expression. The artifact model begins
at [Prelude.hs:58](../../src/Jazz/Compiler/Prelude.hs#L58), while the inline
composition path begins at [Driver.hs:643](../../src/Jazz/Compiler/Driver.hs#L643).

The inline path prepends statements, reindexes the resulting tree, and records
hidden/prelude statement indexes. Type inference carries those indexes at
[TypeInference.hs:196](../../src/Jazz/Compiler/TypeInference.hs#L196), and
analyzed attachment reconstructs statement ownership from them.

Runtime repeats the same provenance protocol in its request and scope-plan
types.

Prelude-specific behavior is necessary. Two representations plus positional
ownership and repeated reindexing are not.

### 5. P1 — shrink: the frontend has two parser engines and multiple source owners

The public parser uses Megaparsec over a token stream, while declarations also
manually parse `TokenStream -> Either ParserFailure`. The manual boundary is
visible at [Declaration.hs:156](../../src/Jazz/Compiler/Parser/Declaration.hs#L156).

The manual parser measures how many tokens it consumed and re-enters the
Megaparsec parser. Consumption, failure, backtracking, and parser context
therefore have overlapping owners.

After lowering, module resolution still traverses the original surface tree
for exports and references at [ModuleResolver.hs:393](../../src/Jazz/Compiler/ModuleResolver.hs#L393)
and [ModuleResolver.hs:543](../../src/Jazz/Compiler/ModuleResolver.hs#L543). Some
qualified class source locations are recovered by rescanning raw tokens.

The surface/core boundary is defensible. Keeping the surface AST and raw token
stream semantically alive after that boundary makes the frontend hard to
navigate.

### 6. P1 — shrink: import visibility is computed independently by validation and name resolution

`ModuleResolver` validates imports and then resolves names as separate steps at
[ModuleResolver.hs:257](../../src/Jazz/Compiler/ModuleResolver.hs#L257).

`ModuleResolver/Imports.hs` builds visible inventories, while
`ModuleResolver/Names.hs` constructs a separate resolution context, alias paths,
and origin maps. The two subsystems reconstruct visibility from the same
imports and module inventories.

Early validation is useful for diagnostics. The normalized import scope should
not be rebuilt twice with different ownership and accessor layers.

### 7. P1 — shrink: recursive scope facts have no single owner

`RecursiveBindings.hs` defines a substantial recursive-scope model beginning at
[RecursiveBindings.hs:84](../../src/Jazz/Compiler/RecursiveBindings.hs#L84).

The same statements are then interpreted independently by:

- unused-binding analysis;
- type-inference scope preparation and previews;
- analyzed-tree binder reconstruction;
- runtime scope planning;
- runtime closure capture.

Examples include [Scope.hs:673](../../src/Jazz/Compiler/TypeInference/Scope.hs#L673),
[Analyzed.hs:597](../../src/Jazz/Compiler/TypeInference/Analyzed.hs#L597),
[ScopePlan.hs:97](../../src/Jazz/Compiler/Runtime/ScopePlan.hs#L97), and
[Engine.hs:1381](../../src/Jazz/Compiler/Runtime/Engine.hs#L1381).

SCCs, recursive cells, and closure capture are genuine language requirements.
The duplicated ownership of lexical scope is the problem.

### 8. P2 — yagni: the same type and signature language has multiple dialects

`TypeRepresentation.hs` provides generic semantic and signature structures,
but they are re-labeled as expression types, analyzed types, diagnostic types,
constructor argument types, evidence types, and runtime hints.

`ExpressionType` and `AnalyzedType` are structurally identical while still
participating in a full projection pass. Unsupported signatures are rejected by
one converter, optionally ignored by others, and independently reinterpreted by
capability code.

Some separation is required for inference variables and type schemes. The
parallel type dialects and repeated conversions are not all required by the
language.

### 9. P2 — shrink: pure and host runtime evaluation duplicate the scope engine

Runtime dispatches between pure and host scope execution at
[Engine.hs:475](../../src/Jazz/Compiler/Runtime/Engine.hs#L475). The host path
partitions statements and rebuilds pure requests; the pure path constructs its
own runtime plans, lazy environments, and binding cells.

`ModuleRuntime` repeats the split at the program level. Pure module evaluation
starts at [ModuleRuntime.hs:155](../../src/Jazz/Compiler/ModuleRuntime.hs#L155),
while the host path has a separate traversal.

Host effects are a legitimate boundary. Two outer walkers, two module folds,
and request ping-pong are avoidable.

### 10. P2 — shrink: runtime plans form a second instruction language between analysis and evaluation

`RuntimePlan` and runtime obligations are semantic facts attached during
analysis. Runtime later interprets them as return obligations, evidence supply,
numeric specialization, and result constraints.

This divides evaluation semantics between type inference, analyzed-tree
attachment, and the evaluator. The runtime is not simply evaluating the
analyzed tree; it is interpreting a second mini-program attached to it.

### 11. P2 — shrink: observation, host state, outcomes, and driver results form overlapping runtime protocols

Observation modes, counters, profile state, host caches, machine depth, and
continuation depth are all threaded through runtime evaluation. Runtime results
then pass through runtime control, runtime outcome, observation result, and
driver execution/result types.

Distinguishing diagnostics from explicit program exits is necessary. The number
of overlapping carriers and the fact that observation participates in execution
routing are avoidable coupling.

### 12. P2 — yagni: the driver manually expands a public API matrix

`Driver.hs` exposes combinations of standalone versus module execution,
bundled versus explicit versus resolved preludes, host versus pure execution,
and observed versus unobserved execution.

The actual semantic pipelines are few, but the wrappers multiply around them.
This hides phase transitions and duplicates result/error assembly.

## Lower-severity symptoms

- `CoreProgram` stores both an ordered module collection and a module map.
- `CoreModule` stores a split body and repeatedly reconstructs an expression
  block.
- Diagnostics are stored inside analyzed module facts and separately
  accumulated by the module compiler.
- Typed `ModulePath` values are flattened into `[Text]` in inference and
  runtime.
- Runtime reaches back into the compiler coordinator just to inspect analyzed
  errors.
- Evidence candidates are scanned, stored in inference state, and rescanned
  for publication.

## What is not over-engineering by itself

The following complexity is justified by the current language/runtime contract:

- lexing and parsing;
- lowered, resolved, and analyzed phase distinctions;
- unification, occurs checks, generalization, numeric constraints, and
  defaulting;
- recursive bindings and lazy closure cells;
- ADTs, patterns, and coverage analysis;
- module dependency ordering and import filtering;
- a lazy evaluator and host-effect boundary;
- explicit exit versus runtime failure;
- structured diagnostics with source spans.

The issue is the duplicated ownership surrounding these features, not the
features themselves.

## Why the architecture likely accumulated this way

The code shape is consistent with a ratchet, although this audit intentionally
does not claim historical proof:

1. A phase-indexed AST established strong phase contracts.
2. New semantic information was added beside the AST instead of changing the
   semantic carrier.
3. Node IDs became foreign keys between independent traversals.
4. An attachment pass was added to reconstruct analyzed nodes.
5. Modules added inventories, interfaces, binder maps, evidence maps, and
   rebasing.
6. Prelude injection added positional ownership and reindexing.
7. Host execution and observation added alternate runtime control paths.
8. Existing public entrypoints preserved a growing wrapper matrix.

Each addition is locally understandable. Together they form several partial
models that all claim ownership of the same program.

## Simplest viable target shape

The code should converge on one owner for each major concept:

```text
source
  -> one parser-neutral representation
  -> one resolved semantic representation
  -> one analyzed/executable representation
  -> one module interface boundary
  -> one evaluator parameterized by host capability
```

The existing phase types can remain. The main change is to stop using them as a
backbone for detached fact databases, positional ownership protocols, and
runtime-specific plans.

If a future backend is added, introduce a deliberate executable/backend IR at
that point. Do not keep extending the semantic AST until it also becomes a
compiler IR, an interpreter input, a profiling carrier, and a module-runtime
contract.

## Estimate

The earlier `-250 lines` estimate was too low. Based on the amount of duplicated
carrier and adapter code, a realistic implementation-level target is roughly
3,000–7,000 net removable lines, with the central estimate near 5,000. The
larger gain would be fewer semantic representations and ownership boundaries,
not merely fewer lines.

No source files were changed by this audit.
