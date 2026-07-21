# Jazz-Next Hosted Canonical Core Design

## Status

Approved in discussion on `2026-07-21`. This design selects canonical-core
lowering as the next hosted compiler milestone after the Jazz-authored parser
reached complete stage-0 parity.

The active Haskell implementation under `jazz-next/` remains stage 0 and the
semantic oracle. The hosted implementation will match the current
`JazzNext.Compiler.Parser.Lower` boundary before any redesign of the core,
type-inference port, or backend work begins.

Implementation status (`2026-07-21`): child 1 is complete. `CoreTypes.jz`
defines the full comparison schema, the checked Haskell adapter rejects values
outside the lowering boundary, and `CoreLower.lowerFoundationExpression`
matches stage 0 directly and through hosted parser composition for literals,
source/qualified names, operator values, collections, tuples, ordinary
application, non-`$` binary nodes, both sections, and ordinary blocks. Every
deferred or recursively unsupported tree returns `Nothing`. Children 2-4
remain: control flow/patterns/lambdas; signatures/declarations/operators; and
modules/corpus closure.

## Goal

Add a Jazz-authored lowering stage that consumes the existing Jazz-authored
surface AST and produces a canonical representation of the active stage-0 core
AST. The hosted result must match stage 0 exactly for every supported lowering
rule, including desugaring, generated names, source spans, module metadata, and
the two lowering diagnostics.

This milestone establishes an independently testable compiler boundary:

```text
Jazz source
  -> Jazz-authored lexer
  -> Jazz-authored parser
  -> Jazz-authored canonical-core lowering
  -> canonical comparison result
```

It does not claim that type inference, name resolution, analysis, or runtime
execution are hosted.

## Chosen Approach

Use a staged exact-parity port.

The first alternative was to implement the complete lowerer in one child. That
would combine the core schema, comparison adapter, expression transformations,
pattern and lambda desugaring, declarations, module validation, source
qualification, and corpus closure in one review. The resulting batch would be
too broad to isolate regressions reliably.

The second alternative was to redesign the core around the long-term typed-core
or backend-neutral IR model before hosting the current lowerer. That would
change the stage-0 boundary and the hosted boundary simultaneously, making
parity ambiguous and skipping the next pipeline stage recorded by the bootstrap
profile.

The selected approach preserves stage 0 as the oracle, establishes one stable
comparison contract, and divides lowering into four independently reviewable
children.

## Active Boundary

The hosted lowerer mirrors the output of:

- `lowerSurfaceExpr :: SurfaceExpr -> Expr`; and
- `lowerSurfaceModule :: FilePath -> [Text] -> SurfaceExpr -> Either Diagnostic CoreModule`.

The current `Expr` is the canonical core consumed by analysis, type inference,
and the interpreter. It is not itself a fully annotated typed-core tree. Hosting
this boundary therefore does not imply a hosted typechecker or freeze a future
typed-core representation.

The Haskell files remain authoritative during this milestone. Production
compiler execution continues through the Haskell lowerer; the Jazz-authored
lowerer is exercised through differential tests until a separate integration
contract promotes it.

## Ownership

New compiler implementation belongs under `jazz-next/jazz/compiler/`:

- `CoreTypes.jz` owns the hosted canonical name, span, literal, pattern,
  signature, statement, expression, module, and lowering-result data.
- `CoreLower.jz` owns pure expression lowering and fallible module lowering.
- A small `Core.jz` facade composes the hosted parser and lowerer in the fourth
  child, after the direct lowering contract is stable. It does not duplicate
  parsing logic.

Test-only Haskell support belongs under
`jazz-next/test/JazzNext/Compiler/Bootstrap/`:

- `CanonicalCoreComparison.hs` converts stage-0 `Expr`, `CoreModule`, and the
  two permitted module-lowering failures into the hosted comparison schema.
- Shared parity support executes Jazz lowering through the existing
  interpreter and compares canonical values.
- Focused suite entry points own each implementation child's fixed fixture
  family.

The comparison adapter is not a second lowerer. It only structurally translates
already-lowered stage-0 values and never reproduces lowering decisions from a
surface tree.

## Canonical Core Contract

The hosted core contract represents every form that the active stage-0 lowerer
can emit:

- literals, including normalized fractional values, original fractional-source
  metadata, and optional target widths;
- source, qualified, and lowering-generated names;
- wildcard, variable, literal, constructor, list, cons-list, tuple, as-, and
  or-patterns;
- unary lambdas, values, lists, tuples, application, explicit type application,
  conditionals, pattern cases, binary operators, sections, and blocks;
- ordinary bindings, signatures, data declarations, classes, impls, and
  expression statements;
- signature constraints, supported signature types, and tokenized unsupported
  signatures; and
- declared module path, explicit export metadata, resolved import metadata,
  and the executable core body.

Resolved and builtin names are not part of the hosted lowering output because
they are introduced by later compiler phases. The hosted name schema includes
only source names, qualified names, and the generated-name forms emitted by
lowering:

- lambda-pattern arguments with their one-based parameter index; and
- encoded operator-binding storage names.

Canonical spans carry an optional normalized source path plus one-based line and
column values. Direct expression lowering preserves unqualified spans. Module
lowering qualifies every retained core, import, export, declaration, method,
and explicit-type-application span with the supplied canonical source path.

The comparison representation must use deterministic Jazz values. It must not
depend on Haskell `Show` output, host-specific absolute paths, source-string
inspection, or implementation file contents.

## Lowering Semantics

Most surface constructors lower structurally. The hosted lowerer must preserve
the non-trivial stage-0 rules exactly:

- a qualified surface variable becomes one qualified core name;
- a multi-parameter lambda becomes nested unary lambdas in source order;
- a pattern parameter becomes a generated unary parameter whose body is a
  one-arm pattern case;
- binary `$` becomes ordinary application while other binary operators remain
  binary core nodes;
- an operator binding or adjacent signature becomes the same generated hidden
  storage name as stage 0;
- qualified names inside signature types use the active two-segment qualified
  name rule;
- module and import statements are removed from the executable core body and
  retained as `CoreModule` metadata; and
- export-selector spans and all other retained spans are source-qualified only
  by module lowering.

The hosted implementation must not call Haskell compiler internals, add a host
lowering intrinsic, or route decisions through test callbacks. It uses ordinary
Jazz ADTs, list operations, text operations, and recursion through the existing
stack-safe evaluator.

## Module Validation and Failures

Expression lowering is total for every `SurfaceExpr` value accepted by the
hosted parser.

Module lowering can fail only for the two conditions currently owned by stage
0:

1. multiple module declarations (`E4005`); or
2. one module declaration whose path differs from the expected module path
   (`E4006`).

The hosted result uses structured failure reasons containing the diagnostic
code and the semantic inputs needed for comparison. Tests compare those
structures rather than duplicating presentation strings. The test-only Haskell
adapter extracts the same structure from the stage-0 result.

No analyzer, resolver, type, warning, import-availability, or runtime error is
reclassified as a lowering failure. Parser and lexer failures remain owned by
their existing stages.

## Data Flow

Direct differential coverage uses this path:

```text
fixed surface fixture
  -> stage-0 lowerSurfaceExpr/lowerSurfaceModule
  -> test-only canonical adapter
  -> expected canonical core value

fixed surface fixture
  -> Jazz CoreLower
  -> actual canonical core value

expected == actual
```

Integration coverage additionally composes the hosted lexer and parser before
`CoreLower`. The stage-0 side independently tokenizes, parses, and lowers the
same source. Parser-rejected and lexer-rejected fixtures stop at their existing
owners and are not counted as lowering fixtures.

Every successfully parsed fixture selected for lowering parity belongs to
exactly one fixed lowering family. A manifest audit rejects omissions,
duplicates, family drift, and accidentally included parse failures.

## Implementation Children

### 1. Contract, Harness, and Expression Foundation

Define the hosted core schema, the total stage-0 comparison adapter, and the
shared parity runner. Implement literals, source and qualified variables,
lists, tuples, ordinary application, operator values, structural binary and
section nodes, and foundational blocks/statements needed by the fixed
expression family.

This is the first executable child. It proves that the comparison boundary and
pure hosted lowering work; it does not contain placeholder constructors for
later transformations.

Completed on `2026-07-21` with exact repeated direct and parser-composed parity.
The internal `Maybe` boundary remains in place for the later children.

### 2. Patterns, Control Flow, and Lambda Desugaring

Add every pattern form, guarded case arms, conditionals, and nested control
flow. Add multi-parameter unary-lambda lowering and generated pattern-argument
desugaring with exact one-based indices.

### 3. Signatures, Declarations, and Operators

Add signature types and constraints, tokenized unsupported signatures, data,
class, and impl payloads, explicit type application, `$` application
desugaring, and exact hidden operator-binding names.

### 4. Modules and Corpus Closure

Add module/import extraction, explicit export metadata, expected-path
validation, structured `E4005`/`E4006` failures, complete span qualification,
and the composed source-to-core facade. Close the manifest over every
successfully parsed fixture in the accepted parser corpus.

Only one child is promoted into `Ready Now` at a time. Each child plan must
name concrete paths, a fixed fixture family, focused verification, and the
constructors or transformations that remain outside that child.

## Verification Strategy

Each child requires:

- exact differential comparison against stage 0 for its fixed fixture family;
- direct unit cases for every newly owned transformation and failure;
- regression execution for all previously landed lowering families;
- warning-clean development compilation;
- the routine Cabal test matrix;
- queue and documentation validators; and
- `git diff --check`.

Milestone closure requires the audited lowerable-corpus manifest and exact
repeated parity through the direct lowering path and the composed source path.
The repetition proves determinism; it is not a scale profile.

The default test matrix must not run the exhaustive parser scale components.
This milestone adds no new exhaustive scale gate. Milestone closure includes
one bounded deterministic smoke case for representative nested lowering; large
synthetic compiler profiles remain manual, opt-in evidence for rare performance
investigations.

Tests must assert behavior and canonical values. Source-string assertions about
function names, file layout, or implementation text are not acceptable parity
evidence.

## Queue Transition

After this design is reviewed, a separate implementation plan will define only
the first child: contract, harness, and expression foundation. The plan and
queue row must agree exactly on target paths, verification, deliverable, and
dependencies.

The implementation plan will describe responsibilities, observable behavior,
test cases, and integration points. It will not reproduce the code to be
written.

When the first child closes, its evidence moves to `done-archive.md` and only
the second child may be curated. Later children remain in this design rather
than appearing as simultaneously executable queue rows.

## Non-Goals

This milestone does not:

- replace the production Haskell lowerer;
- change the active core AST or its semantics;
- introduce typed-core annotations or host type inference;
- port name resolution, module resolution, analysis, warnings, type inference,
  evidence elaboration, compilation, or evaluation;
- define backend-neutral lowered IR, LLVM IR, object generation, linking, or a
  native runtime;
- add parser recovery, a public parser/lowering API, or new source syntax;
- add Haskell callbacks, parser/lowering intrinsics, bytecode, or a VM;
- require exhaustive parser scale execution; or
- modify `jazz-hs/` or `jazz2/`.

Those boundaries require separate reviewed contracts after hosted canonical
core parity is complete.
