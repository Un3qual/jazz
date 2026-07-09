# jazz-next Compiler Architecture Remediation Design

**Date:** 2026-07-09
**Status:** Approved in conversation; awaiting written-spec review
**Scope:** Active compiler under `jazz-next/` only

## Purpose

Replace the source-replay module architecture and address the related code-quality
findings without changing Jazz language syntax or semantics.

The remediation must:

- replace module source replay with an in-memory, dependency-ordered module graph;
- parse and lower each module source exactly once;
- keep module boundaries through analysis, inference, and runtime evaluation;
- replace text-encoded compiler names with structured names;
- make the lowered core AST canonical;
- use one token-parser and diagnostic model;
- centralize core-pattern binder semantics;
- split type inference along state-ownership boundaries;
- keep compiler implementation modules private to the Cabal package; and
- split oversized test modules without introducing an unnecessary test framework.

Breaking Haskell APIs are allowed. Jazz behavior is not allowed to change unless
an existing outcome is demonstrably inconsistent or impossible to execute, in
which case the change must be treated as a separately documented bug fix with a
regression test.

## Current Problems

### Module replay

`ModuleResolver` parses every module to discover declarations, imports, exports,
and references, but retains only path metadata. `ModuleReplay` then reloads the
same source, parses and lowers it again, derives several export-closure maps, and
builds separate flattened validation and runtime programs.

The flattened programs use compiler-generated textual names such as
`__module::...`. `TypeInference` and `Runtime` recognize those spellings, which
makes module behavior an implicit convention shared across unrelated phases.

### Non-canonical core

The core AST retains both `EIf` and `ECase`, even though both are three-way
conditional nodes. Actual pattern matching is represented separately by
`EPatternCase`. A nominal `Desugar` pass converts `EIf` to `ECase`, but production
code does not consistently use that pass; downstream phases support both nodes.

Module/import syntax and tokenized unsupported signatures also survive farther
into semantic processing than their ownership warrants.

### Parser abstraction stack

Some parser components use Megaparsec combinators directly, while others wrap
hand-written `[Token] -> Either Diagnostic ...` functions inside Megaparsec.
`PatternParser` adds another `Either Diagnostic` layer and manually gets, sets,
and clears the Megaparsec input stream.

### Repeated semantic traversals

Core-pattern binder and common-or-pattern binder behavior is independently
implemented by analyzer, recursive binding analysis, module replay, type
inference, and runtime-adjacent code. Adding a pattern constructor requires
coordinated edits to several copies of the same semantic rule.

### Type inference ownership

`TypeInference.hs` owns the internal type language, substitution and unification,
numeric and equality obligations, declaration facts, capability facts, module
state, runtime hints, deferred constraints, pattern typing, signature handling,
recursive groups, and diagnostic construction. Its state record mixes all of
those lifetimes, and several important values use long positional constructors.

### Package and tests

The Cabal library exposes every implementation module. Tests need internal
access, but that does not justify publishing the parser, replay, AST, solver, and
runtime internals as an external library API.

Several test executables are well scoped, but their main modules have accumulated
between roughly 1,000 and 3,000 lines in one flat test list.

## Chosen Architecture

Use an in-memory module graph with per-module compilation and evaluation.

This is intentionally between two rejected extremes:

- It does not preserve structured flattening as the permanent architecture;
  flattening would still erase module ownership and require special handling.
- It does not introduce serialized module artifacts, interface versioning,
  incremental compilation, or a linker; Jazz does not need those facilities yet.

The pipeline is:

```text
source lookup
  -> SurfaceModule
  -> CoreModule
  -> ResolvedProgram [ResolvedModule]
  -> CompiledProgram [CompiledModule + ModuleInterface]
  -> RuntimeProgram [RuntimeModule]
```

Dependencies are processed in deterministic topological order. Each module is
kept as a separate value throughout the pipeline.

## Module Data Model

The exact field spelling may change during implementation, but the ownership and
invariants are fixed by this design.

### `SurfaceModule`

Parser-owned representation of one source file:

- optional declared module path;
- module/import syntax and source spans;
- surface statements;
- source identity used by diagnostics.

The parser remains syntax-faithful. It does not resolve imports or decide whether
a referenced name is local, imported, builtin, or missing.

### `CoreModule`

Lowered representation of one source file:

- validated module path metadata;
- structured imports;
- canonical executable/declaration statements;
- unresolved structured name references;
- structured unsupported-signature reasons where necessary to preserve existing
  semantic diagnostics.

`SModule` is not an executable core statement. Imports are consumed by module
resolution and do not reach analyzer, inference, or runtime as ordinary
statements.

The accepted and rejected results for currently tested import placements remain
unchanged. If an untested nested import is accepted by the parser but cannot be
resolved or executed, that inconsistency is treated as a true bug: it receives a
deterministic diagnostic and an explicit regression test rather than acquiring
new implicit semantics during this refactor.

### `ResolvedProgram`

Contains:

- the entry module path;
- dependency-ordered resolved modules;
- resolver-owned inventories needed to validate imports and namespaces.

### `ResolvedModule`

Contains:

- module path;
- source path;
- resolved imports;
- its already-parsed and lowered core module;
- resolved references to imported values, constructors, types, classes, and
  implementations.

The resolver never discards the AST and no later phase rereads source text.

### `ModuleInterface`

The compile-time information an importer may observe:

- exported value and constructor type schemes;
- exported data/type facts;
- exported class signatures;
- exported implementation facts and method information required by the current
  constraint semantics;
- runtime-hint metadata required by imported bindings; and
- the declared export inventory used for import validation.

It contains no runtime closures or source text.

### `CompiledModule`

Contains the resolved module, inference result, warnings, errors, runtime hints,
and its `ModuleInterface` when compilation succeeds.

### `RuntimeModule`

Contains runtime cells for the module's exported values, constructors, and method
implementations. Importers consume these cells through structured import maps.

Dependency module expression statements remain semantically validated but are
not executed. Only the entry module's terminal expression determines CLI output,
preserving the current replay contract.

## Structured Names

Compiler-generated meaning must not be encoded in identifier text.

The core name representation distinguishes at least:

- unresolved unqualified source references;
- unresolved alias-qualified source references;
- resolved local references;
- resolved imported references carrying module path and namespace;
- builtin references; and
- generated references carrying a structured generated-name kind and identity.

Namespaces distinguish values, constructors, types, and capabilities where the
existing language semantics require separate lookup.

One non-parameterized core AST is retained to avoid introducing a generic AST
framework. Phase wrappers establish the invariant:

- `CoreModule` may contain unresolved source references;
- `ResolvedModule` may contain only resolved, builtin, or generated references.

Variable binders continue to use source identifiers. Generated lambda-pattern
arguments and operator helpers use generated-name constructors rather than `$...`
text. User-name purity is derived only from user spelling, never from a generated
or module-qualified encoding.

Alias-qualified value and constructor references stay structured until the
resolver maps the alias to a module path. Unqualified imports enter an explicit
imported scope. Local declarations retain the current shadowing and rebinding
rules.

## Canonical Core AST

The surface AST remains syntax-faithful. Lowering owns all syntax sugar.

The canonical expression representation keeps:

- `EIf` as the sole boolean conditional node;
- `EPatternCase` as the sole general pattern-matching node;
- unary `ELambda` nodes;
- structured name references; and
- the existing literal, collection, application, operator, and block forms.

`ECase` is deleted. It is currently shaped exactly like `EIf` and is not a
general case node. The production-unused `Desugar` module is deleted after all
callers and tests use canonical lowering.

Multi-parameter lambdas and pattern parameters are lowered once into nested unary
lambdas and pattern cases. `()` remains the empty tuple/Unit representation, and
`\()` remains one Unit-pattern parameter.

Unsupported signature syntax does not remain as a raw token list in the core.
Lowering produces a structured rejection reason sufficient to reproduce the
existing `E2009` code and message when semantic checking reaches the signature.

## Compiler Phase Ownership

### `Compiler.Name`

Owns structured name references, generated-name kinds, module-qualified resolved
names, rendering for diagnostics, and source identifier purity boundaries.

### `Compiler.Pattern`

Owns core-pattern operations shared by semantic phases:

- bound names;
- common bound names for or-patterns;
- referenced constructor names; and
- narrow helpers needed by scope extension.

The implementation stays concrete and domain-specific. This design does not add
recursion schemes or a generic traversal framework.

Surface-pattern helpers remain parser-owned because surface and core patterns
have different phase responsibilities.

### Parser modules

`Compiler.Parser` becomes a small public façade inside the package.

`Parser.Expression`, `Parser.Pattern`, `Parser.Declaration`, and
`Parser.Signature` become real token parsers over the common Megaparsec parser
type. They use one custom error component and one `Diagnostic` conversion at the
runner boundary.

The token parser provides two explicit runners:

- a complete runner that requires end of input; and
- a prefix runner that returns the unconsumed suffix.

Parser components no longer embed `Either Diagnostic` inside Megaparsec or
manually clear input to satisfy `eof`. Parser context such as declared operators,
known aliases, and statement placement remains explicit data passed to the
relevant grammar functions.

### `Compiler.ModuleResolver`

Owns:

- source discovery under normalized roots;
- parsing and lowering each module once;
- declared-path validation;
- dependency and cycle resolution;
- import inventory validation;
- alias and explicit-import validation;
- namespace-aware name resolution; and
- deterministic dependency ordering.

### `Compiler.ModuleCompiler`

Owns dependency-ordered semantic compilation. It constructs imported analysis
and inference inputs from already-compiled dependency interfaces, then publishes
the next module interface.

### `Compiler.ModuleRuntime`

Owns dependency-ordered module evaluation. It constructs unqualified and
alias-qualified runtime import maps from dependency runtime modules, evaluates
declarations once, and publishes exported runtime cells.

### `Compiler.ModuleReplay`

Is deleted after the new pipeline becomes the only driver path. No compatibility
flag or permanent comparison mode remains.

## Type Inference Ownership

`Compiler.TypeInference` remains the façade for existing callers while delegating
to internal modules.

### `TypeInference.Types`

Owns:

- internal expression types;
- literal ranges and numeric constraints;
- type bindings;
- type schemes;
- scheme constraints; and
- named records replacing long positional scheme and deferred-constraint
  constructors.

### `TypeInference.Solver`

Owns:

- fresh variables;
- substitutions and resolution;
- occurs checks and variable binding;
- unification;
- numeric and strict-equality solver obligations; and
- the solver-specific portion of inference state.

### `TypeInference.Capabilities`

Owns class declarations, implementation facts, method signatures, inferred and
explicit constraints, deferred constraint finalization, and imported capability
facts from module interfaces.

### `TypeInference.Pattern`

Owns core-pattern type checking, constructor/list/tuple pattern typing,
or-pattern agreement, and pattern binding types. It reuses `Compiler.Pattern`
for binder semantics.

### `TypeInference.Scope`

Owns statement-order inference, signatures, ordinary and recursive binding
groups, module-interface construction, and imported compile-time environments.

### `TypeInference.Diagnostics`

Owns type-specific diagnostic construction and type rendering used by those
diagnostics.

### State shape

Explicit state threading remains. The top-level inference state contains named
subrecords for:

- solver state;
- declaration and capability facts;
- current module context; and
- accumulated outputs such as runtime hints and diagnostics.

This design intentionally does not add `StateT`, lenses, a plugin-like pass
framework, or another abstraction merely to shorten field updates.

## Analysis and Runtime Inputs

Analyzer and inference entrypoints receive explicit inputs rather than learning
module behavior from statements or encoded names.

Those inputs include:

- imported unqualified bindings;
- alias-qualified bindings;
- imported constructors and type facts;
- imported classes and implementation facts;
- builtin resolution mode;
- warning settings; and
- module/source context.

Runtime evaluation receives structured runtime imports and binding hints. It does
not inspect module declaration statements, import statements, or name prefixes.

## Diagnostics and Error Compatibility

Existing user-visible diagnostic behavior is part of the compatibility contract:

- codes;
- summaries;
- primary and related spans;
- subjects;
- notes;
- deterministic ordering;
- source paths;
- warning ordering; and
- warning promotion behavior.

Module source paths are attached at parse/lower time through parser source
context. The new pipeline does not need a whole-AST traversal solely to qualify
statement spans after reparsing.

Changing an existing diagnostic requires evidence that the old result is
internally inconsistent, plus a focused regression test and documentation note.

Internal invariants should use types or structured internal diagnostics instead
of text parsing. Rendered diagnostics are outputs, not an interchange format.

## Cabal Boundary

The package uses a private named library, `jazz-next-internal`, containing the
compiler implementation. The executable and test suites depend on that private
library.

There is no public Haskell library API during this unreleased phase. A curated
public façade can be designed later when external embedding requirements exist.

This removes accidental publication of parser adapters, raw AST constructors,
module machinery, solver internals, catalogs, and runtime implementation types.

## Test Organization

The existing lightweight `NamedTest` harness remains. No Hspec or Tasty
dependency is added solely for organization.

Existing Cabal suite names remain available for focused execution. Main test
modules that exceed roughly 1,000 lines are divided by semantic concern into
supporting modules that export named test groups. Likely divisions include:

- runtime control flow, recursion, numeric behavior, capabilities, and rendering;
- module loading success paths, visibility, diagnostics, and runtime integration;
- binding signatures, generalization, constraints, recursion, and diagnostics;
- parser declarations, patterns, operators, signatures, and foundational forms.

The suite main files aggregate those groups. Shared fixtures and assertions move
to narrowly named support modules instead of being copied.

## Migration Strategy

The replacement lands as a sequence of behavior-preserving commits.

### Phase 1: Characterize current behavior

Add or strengthen tests for:

- dependency ordering and cycles;
- unqualified, explicit, and alias imports;
- import collisions and ambiguity;
- hidden exports;
- constructors and generic data types across modules;
- classes, implementations, and qualified methods across modules;
- declared operators and operator helpers across modules;
- dependency expression validation without dependency expression execution;
- source-qualified diagnostics and their ordering;
- warning behavior; and
- entry-module runtime output.

These tests become the replacement pipeline's parity contract.

### Phase 2: Canonical shared foundations

Introduce structured names and shared core-pattern semantics. Canonicalize the
core AST around `EIf` and `EPatternCase`, update all passes, and delete `ECase`
and `Desugar`.

### Phase 3: Parser consolidation

Move all token grammar onto the common Megaparsec parser and error model. Keep
all accepted/rejected syntax and diagnostic text stable.

### Phase 4: Inference extraction

Extract type definitions, solver state, capability handling, pattern typing,
scope inference, and diagnostics. Introduce the compile-time module-interface
types and imported inference inputs without changing standalone behavior.

### Phase 5: New module pipeline

Implement resolved module units, dependency interfaces, per-module analysis and
inference, runtime module exports, and structured imports. During this phase,
tests may compare the new pipeline against replay as a temporary oracle.

### Phase 6: Cutover and deletion

Switch `Driver` and CLI module entrypoints to the new pipeline. Delete
`ModuleReplay`, synthetic module names, replay-only dependency-closure logic,
replay-specific runtime/type checks, source replay, and temporary comparison
code.

### Phase 7: Package and test boundaries

Convert the compiler library to a private internal library and split oversized
test modules while preserving suite names and commands.

### Phase 8: Documentation and final audit

Update active architecture and module documentation. Run structural searches and
the complete verification matrix.

## Verification

Every implementation phase follows test-driven development:

1. add or identify a focused failing characterization/structure test;
2. make the smallest coherent change;
3. run the focused suite;
4. run adjacent suites;
5. commit the verified slice.

Final verification includes:

- `cabal check`;
- `cabal build all`;
- `cabal test all`;
- `bash jazz-next/scripts/test-warning-config.sh`;
- packaged CLI standalone and module-graph smoke tests;
- `bash scripts/check-docs.sh`;
- `bash scripts/check-execution-queue.sh`;
- `git diff --check`;
- a clean worktree audit; and
- confirmation that `jazz-hs/` and `jazz2/` were untouched.

Structural completion checks require:

- no `ModuleReplay` source or Cabal module entry;
- no compiler recognition of `__module::` or replay-only prefixes;
- no second parse/lower pass for resolved module sources;
- no `ECase` constructor or production `Desugar` module;
- one core-pattern binder implementation;
- no nested `Either Diagnostic` token parser adapters;
- inference code divided by the ownership boundaries above;
- no public compiler implementation library; and
- oversized flat test modules divided by concern.

## Non-Goals

This remediation does not add:

- new Jazz syntax or semantic features;
- serialized module artifacts;
- incremental compilation or caching;
- a linker or package manager;
- a stable external Haskell API;
- a generic compiler-pass framework;
- monad-transformer or lens infrastructure;
- a new test framework solely for organization;
- changes under `jazz-hs/` or `jazz2/`; or
- changes to the documented Unit and unary Unit-lambda semantics.

## Success Criteria

The work is complete when all seven review findings are substantively removed,
not merely renamed:

1. module compilation is per-module and replay-free;
2. the core AST is canonical;
3. the parser has one grammar/error model;
4. pattern binder semantics have one core implementation;
5. type inference has explicit subsystem ownership and state boundaries;
6. compiler internals are private to the Cabal package; and
7. oversized tests are split by concern.

All existing behavior tests and compatibility checks must pass after the old
architecture is deleted.
