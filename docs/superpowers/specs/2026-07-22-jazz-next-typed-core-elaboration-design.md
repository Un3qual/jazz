# Jazz-Next Typed-Core Elaboration Design

## Status

Approved in discussion and reviewed in written form on `2026-07-22`.
No implementation child is promoted by this document alone; the contract-
foundation plan remains a separate review gate.

The hosted lexer, parser, canonical-core lowerer, and backend-neutral lowered-IR
contract foundation are complete. The next compiler boundary must retain the
resolved type, capability evidence, name identity, and representation recipe
for every executable core node before any honest core-to-IR lowerer can be
promoted.

## Goal

Define the permanent typed-core elaboration boundary for `jazz-next` between
semantic inference and backend-neutral lowering.

The boundary must:

- preserve the current canonical core as the unchanged input to analysis,
  inference, and the reference interpreter;
- retain a final semantic type and backend-neutral representation recipe for
  every expression, pattern binder, and executable binding;
- make polymorphic instantiations and capability evidence explicit rather than
  asking the lowerer to repeat solver decisions;
- require resolved compiler names and deterministic symbol identities;
- support both the Haskell stage-0 compiler and a later Jazz-authored
  typechecker through matching data contracts; and
- provide enough structured validation for a later lowerer to reject an
  incomplete or contradictory typed tree before producing lowered IR.

This design establishes the boundary and its first contract child. It does not
lower canonical core, emit lowered IR, add LLVM, or change runtime execution.

## Current Boundary

`JazzNext.Compiler.AST.Expr` is canonical core. Module resolution rewrites
source and qualified names to `ResolvedName` values before module compilation,
but standalone expression entry points may still receive source names.

`JazzNext.Compiler.TypeInference.ExpressionType` is the active solver model.
It includes temporary solver variables and integer-literal ranges that are
useful during inference but are not stable compiler output. The inference
traversal returns an expression type while updating substitutions, capability
obligations, visible bindings, module interfaces, diagnostics, and runtime
hints.

`InferenceResult` currently retains the original canonical expression,
diagnostics, runtime type hints, and module-interface facts. It does not retain
the root expression type or any inner-node types. Inner canonical expressions
also do not have distinct source spans, so a span-keyed annotation map cannot
identify them safely.

Runtime qualified-method candidates carry compiler-owned `RuntimeEvidence`,
but those records are interpreter values. They do not identify every
compile-time evidence argument or attach the selected evidence to typed call
sites. A native lowerer must not inspect interpreter closures or repeat runtime
candidate selection.

`JazzNext.Compiler.LoweredIR` accepts only concrete value representations.
Type variables, unresolved names, overloaded literals, unsolved capability
constraints, and source-level function types cannot enter that contract.

## Considered Approaches

### Selected: A Separate Structurally Aligned Typed Tree

Define a typed-core tree whose constructors align with canonical core while
carrying final annotations directly on each node. Build a provisional version
during the existing inference traversal, then finalize it once with the final
solver state.

This keeps the interpreter boundary stable, gives the lowerer direct structural
access to every annotation, and avoids fragile node identifiers. It requires a
new schema and a deliberate inference refactor, but it makes the semantic
handoff explicit and independently testable.

### Rejected: Add Annotations to Canonical Core

Changing `Expr`, `Pattern`, and `Statement` to carry inferred types would force
the parser lowerer, analyzer, resolver, interpreter, hosted canonical-core
schema, and all existing equality/parity fixtures to understand a post-
inference concern. The same tree could no longer be both pre-inference
canonical input and post-inference output.

### Rejected: Sidecar Maps Keyed by Spans, Names, or Structural Paths

Inner expressions do not have unique spans. Names repeat across uses and
scopes. Structural paths are invalidated by canonical rewrites and make every
consumer coordinate two independently traversed trees. Any of these keys could
silently attach a valid annotation to the wrong node.

### Rejected: Run a Second Post-Inference Type Pass

A second traversal that reconstructs types from the final environment would
duplicate application instantiation, literal defaulting, constraint solving,
and qualified-method selection. Drift between the diagnostic pass and the
elaboration pass would make a successful compile depend on which solver ran
last.

## Pipeline Position

The permanent stage-0 flow becomes:

```text
resolved canonical core
  -> analyzer and inference traversal
  -> provisional typed core plus final solver state
  -> typed-core finalization and validation
  -> typed core
  -> later specialization and core-to-IR lowering
  -> validated backend-neutral lowered IR
```

The analyzer and inference traversal remains the single owner of semantic
decisions. Typed-core finalization may resolve and canonicalize decisions
already present in the solver state; it may not infer an expression again or
select different evidence.

The current interpreter continues to consume canonical core plus its existing
runtime hints. Production compilation and run mode do not switch to typed core
as part of the first contract child.

## Ownership

Production Haskell ownership belongs under
`jazz-next/src/JazzNext/Compiler/`:

- `TypedCore.hs` owns the final typed-core data contract and stable identifiers;
- `TypedCore/Validate.hs` owns complete structural and annotation validation;
- `TypeInference/Elaboration.hs` later owns final substitution, literal
  defaulting, type-parameter canonicalization, representation recipes, and
  construction of the final contract from provisional inference output; and
- `LoweredIR/Lower.hs` may later consume only validated typed core. It must not
  depend on inference internals or interpreter runtime values.

Jazz-authored ownership belongs under `jazz-next/jazz/compiler/`:

- `TypedCoreTypes.jz` mirrors the permanent typed-core contract; and
- `TypedCoreValidate.jz` mirrors typed-core validation for hosted contract
  evidence.

Test support belongs under
`jazz-next/test/JazzNext/Compiler/Bootstrap/`:

- `CanonicalTypedCoreComparison.hs` performs checked conversion between
  Haskell typed-core values and ordinary Jazz runtime values; and
- `JazzTypedCoreContractSpec.hs` owns the fixed valid and invalid contract
  fixtures and exact repeated comparison.

The comparison adapter is not an elaborator. It only translates already-built
typed-core values and validation failures. It must reject unknown constructors,
wrong field shapes, malformed identifiers, and values outside the contract.

## Typed Program and Module Shape

A typed program contains:

- an ordered typed prelude module when the compilation uses one;
- ordered typed modules in the resolved dependency order;
- the resolved entry-module path; and
- no absolute filesystem paths or host-specific compiler objects.

A typed module contains:

- its resolved module path;
- the resolved imports already accepted by the module resolver;
- its public export inventory and a stable typed projection of the public
  module interface;
- ordered typed declarations and executable statements; and
- the final type and representation recipe of its terminal expression.

Typed modules preserve enough declaration metadata to derive data layouts,
function symbols, evidence parameters, and module-visible definitions. They do
not preserve parser tokens, unsupported surface syntax, or module declarations
that the resolver has already converted to module metadata.

The typed module interface contains exported value schemes, data declarations,
class declarations, concrete impl identities, and evidence identities using
only typed-core contract types. The current `ModuleInterface` may supply those
facts during finalization, but its `TypeBinding`, `ExpressionType`, and other
inference-internal values are not embedded in typed core.

Prelude ownership stays explicit. A prelude binding uses the existing ambient
origin rather than pretending to belong to the entry module. Imported names
retain their defining module path even when the source used an alias.

## Node Structure

Typed expressions mirror every active canonical-core expression form:

- literals and resolved variables;
- lambdas, operator values, lists, tuples, and applications;
- explicit type application;
- conditionals and pattern cases;
- binary operators and both section forms; and
- blocks containing typed statements.

Each typed expression contains one `TypedNodeInfo` record with:

- its final `TypedType`;
- its `TypedRepresentationRecipe`;
- zero or more explicit type instantiations in solver-defined order; and
- zero or more capability-evidence uses in deterministic obligation order.

Typed patterns mirror every active core pattern. Each pattern node records the
type and representation recipe of the value it matches. Every variable or as-
pattern binder additionally records its resolved binder name, type, and
representation recipe. All alternatives of an or-pattern must expose the same
ordered binder contract.

Typed binding statements record:

- the resolved binding name;
- the final monomorphic type or generalized scheme;
- ordered type parameters;
- ordered evidence parameters;
- the typed value expression; and
- the statement span already retained by canonical core.

Signatures, data declarations, classes, and impls retain typed metadata needed
for module interfaces, layouts, and evidence lookup. They are not executable
expression nodes and do not receive invented unit-valued instructions.

The schema does not use a universal annotation map. Parent/child alignment is
represented by ordinary typed-tree nesting.

## Final Semantic Types

`TypedType` is a stable compiler contract distinct from the mutable solver
model. It contains:

- `Int` and `Float` aliases as semantic types;
- each concrete signed, unsigned, and floating numeric width;
- `Bool`, `Char`, and `Text`;
- homogeneous lists;
- fixed-arity tuples, including the empty tuple used as unit;
- resolved data types with ordered type arguments;
- right-associated function types; and
- canonical type parameters owned by a binding scheme.

`TypedType` does not contain solver allocation identifiers, integer-literal
ranges, unresolved named types, or unbound type variables.

Finalization applies the complete final substitution and the existing literal-
defaulting rules before constructing `TypedType`. A remaining free solver
variable is valid only when it maps to a declared generalized type parameter.
All other remaining variables are elaboration failures.

Generalized parameters use stable zero-based ordinals in the existing
`schemeQuantifiedOrder`, scoped to the owning binding. Internal `TVarType Int`
values never cross the boundary. Explicit and implicit instantiations record
the same parameter ordinals with the concrete typed arguments chosen at that
use site.

## Representation Recipes

Typed core owns backend-neutral representation recipes. A recipe states how a
semantic value can become a concrete `LoweredRepresentation` after all required
type parameters are instantiated. It is not an LLVM type or target layout.

The recipe vocabulary contains:

- unit;
- boolean;
- signed and unsigned integers with `8`, `16`, `32`, and `64` widths;
- `Float16`, `Float32`, and `Float64`;
- Unicode scalar `Char`;
- managed text;
- a managed homogeneous list recipe;
- a managed product recipe for tuples;
- a managed variant recipe identified by resolved data type and type
  arguments;
- a closure recipe with ordered parameter and result recipes; and
- a representation parameter corresponding to a generalized type parameter.

Semantic `Int` maps to signed `64`; semantic `Float` maps to `Float64`. The
empty tuple maps to unit. Non-empty tuples, lists, text, and data values map to
managed recipes whose stable lowered-layout identifiers are assigned later by
the lowerer from their complete structural recipe.

A representation parameter is valid in generalized typed core but is not a
`LoweredRepresentation`. Before emitting lowered IR, the specialization
worklist must substitute every representation parameter with a concrete
recipe. Any remaining representation parameter blocks lowering with a
structured failure; the lowerer may not guess a boxed representation.

This contract selects specialization as the first route from rank-1
polymorphic typed core to concrete lowered IR. It does not introduce a universal
boxed value, hidden pointer assumptions, or an unversioned runtime type tag.
Dictionary materialization may be designed later without changing semantic
type annotations.

## Names and Symbols

Executable typed core accepts only:

- `ResolvedName` with current-module, imported-module, or ambient-prelude
  origin;
- `BuiltinName` selected from the active builtin catalog; and
- compiler-owned `GeneratedName` forms already present in canonical core.

`SourceName` and `QualifiedName` are validation failures in executable typed
core. Display text is not symbol identity: two values with the same rendered
name but different module origins remain different.

The later lowerer derives stable function and layout identifiers from
structured module paths, namespaces, resolved names, and specialization
arguments. It must not use `Show`, absolute paths, process hashes, map
iteration order, or host pointer identity.

## Type Instantiation and Evidence

Every use of a generalized binding records a `TypedInstantiation` containing
the owning binding identity and the ordered mapping from scheme parameter
ordinals to final typed arguments. Explicit `@Type` and implicit instantiation
share this representation; an explicit application additionally retains its
source span for diagnostics.

An evidence parameter represents one source or inferred scheme obligation.
Its stable identity contains the owning binding, the obligation index, the
capability name, optional method key, and target typed type. Explicit
constraints retain source order; inferred constraints follow the stable order
already published by the solver after explicit constraints.

An evidence use records:

- the evidence-parameter identity it satisfies, when the call passes evidence
  to a generalized binding;
- the resolved capability/class name;
- the concrete target typed type;
- the selected impl origin and identity; and
- the method key and selected method-body identity when method dispatch is
  involved.

Fully applied qualified-method calls must identify exactly one selected method
body. A bare or partially applied qualified method may retain an ordered
candidate set only when current language behavior permits further argument
application to resolve it. Such a candidate set is valid typed core but is not
eligible for the first direct-call lowerer.

Missing, duplicate, contradictory, or ambiguous evidence remains diagnostic-
first. Typed core is produced only after the existing inference diagnostics
accept the expression. The finalizer may report an invariant failure if the
accepted solver state still cannot materialize the evidence recorded during
the same traversal; it may not select a new candidate.

Interpreter `RuntimeEvidence` remains an execution detail of the current
runtime. Typed-core evidence is compile-time structured data and never embeds a
`RuntimeValue`, runtime closure, Haskell function, or evaluated method body.

## Single-Pass Inference and Finalization

Inference functions that currently return `(Maybe ExpressionType, InferState)`
will eventually return the same semantic result plus a provisional typed node.
The provisional node may refer to solver variables and recorded evidence
obligations, but it already has the exact canonical child structure selected by
that traversal.

After the outer inference scope finishes, finalization performs one complete
walk that:

1. applies the final substitution to every provisional type;
2. applies existing binding and literal defaults;
3. canonicalizes generalized variables to scheme-owned parameter ordinals;
4. converts resolved semantic types to representation recipes;
5. resolves recorded evidence identities from the accepted solver output;
6. constructs final typed patterns, expressions, statements, and modules; and
7. runs typed-core validation.

Finalization does not call `inferExprType`, unify types, create fresh solver
variables, search visible impls, or emit ordinary analyzer/type diagnostics.

The first implementation keeps typed inference opt-in so the interpreter path
does not change while the contract is incomplete. The permanent public result
uses an explicit outcome:

```text
TypedCoreBlockedByDiagnostics
TypedCoreInvariantFailures [TypedCoreValidationFailure]
TypedCoreSucceeded TypedProgram
```

`BlockedByDiagnostics` is used when the existing compilation result contains
errors. Warnings do not block typed core. `InvariantFailures` is structured
compiler evidence, not a replacement for source diagnostics.

## Validation

Validation traverses the complete typed program and returns failures in stable
module, statement, pattern, and expression order.

The contract validator detects at least:

- source or qualified names that survived module resolution;
- solver identifiers, literal ranges, or unbound variables in final types;
- type parameters or representation parameters outside their owning scheme;
- mismatched semantic type and representation recipe;
- an application whose function, argument, and result types disagree;
- conditionals with non-boolean conditions or unequal branch types;
- pattern cases whose scrutinee, patterns, guards, and arm results disagree;
- inconsistent binder sets or binder annotations across or-pattern arms;
- duplicate or unstable scheme parameter and evidence identities;
- instantiation arity, ordering, or type mismatches;
- missing, duplicate, ambiguous, or contradictory capability evidence;
- qualified-method evidence that does not identify a visible selected impl;
- managed data recipes whose resolved type or arguments disagree with the
  declaration metadata;
- callable recipes whose parameter/result recipes disagree with their typed
  function type; and
- absolute paths, host-specific identifiers, or runtime values embedded in the
  contract.

Validation does not prove control-flow or lowered-IR invariants. Those remain
owned by the later lowerer and `LoweredIR.Validate`.

## Canonical Comparison

The Haskell/Jazz comparison contract renders complete typed programs and
complete ordered validation failures as ordinary constructor-shaped values.
It preserves declaration, parameter, evidence, module, statement, and child
order exactly.

The fixed valid fixture family covers at least:

- defaulted and explicit-width scalar literals;
- resolved local, imported, ambient, builtin, and generated names;
- lists, tuples, data values, and function recipes;
- monomorphic and generalized bindings;
- implicit and explicit type instantiation;
- explicit and inferred capability evidence;
- unique qualified-method selection and permitted partial candidate sets;
- patterns and or-pattern binder alignment; and
- multi-module identity without absolute source paths.

The fixed invalid fixture family covers every validation category named above.
Both validators run every fixture twice and compare complete ordered output.

Canonical comparison must not use Haskell `Show`, source-string inspection,
implementation-file contents, unordered map rendering, absolute paths, or
interpreter result strings.

## First Implementation Child

The first child defined by this design is
`JN-BOOTSTRAP-TYPED-CORE-CONTRACT-FOUNDATION-001`.

It implements:

- the complete Haskell and Jazz typed-core schemas described here;
- complete structural validators in both implementations;
- the checked Haskell/Jazz comparison adapter;
- fixed valid fixtures spanning types, recipes, names, instantiations,
  evidence, expressions, patterns, bindings, and modules; and
- fixed invalid fixtures covering every required validation category.

The child constructs fixtures directly. It does not yet refactor inference,
produce typed core from source, lower typed core to IR, or switch production
compilation. That makes the contract reviewable before inference internals and
backend lowering begin changing together.

## Later Ordered Work

After the contract foundation closes, later design and implementation gates
proceed in this order:

1. produce typed-core expression foundations from stage-0 inference and lower
   closed scalar expressions plus non-capturing direct calls;
2. elaborate generalized bindings, specialization arguments, closure capture,
   closure environments, recursive bindings, and closure calls;
3. elaborate and lower conditionals, blocks, patterns, switches, and tail
   calls;
4. integrate typed modules, imported definitions, prelude ownership, data
   layouts, runtime services, and qualified-method bodies;
5. close deterministic typed-core and lowered-IR parity across the accepted
   source corpus;
6. design and implement the Jazz-authored analyzer, resolver, inference, and
   elaboration producer against the same typed-core contract;
7. define LLVM lowering and the versioned native-runtime ABI;
8. add object generation and platform linking; and
9. prove native stage-1 and stage-2 equivalence.

Only one independently verifiable child is promoted at a time. This ordering
does not pre-approve universal boxing, runtime-generic dispatch, dictionary
optimization, LLVM types, object formats, garbage collection, or platform ABI
details.

## Verification Strategy

The first contract child requires:

- exact repeated Haskell/Jazz comparison for every valid and invalid fixture;
- complete ordered validation results from both implementations;
- regression execution of the hosted canonical-core and lowered-IR contract
  suites;
- warning-clean `jazz-next` development compilation;
- the routine Cabal test matrix without exhaustive parser-scale components;
- `cabal check`;
- execution-queue and documentation validators; and
- `git diff --check`.

Tests assert structured values and behavior. They must not assert source text,
constructor declarations by string search, or implementation file layout.

## Queue Transition

This proposed design does not change `Ready Now`, `Next Curation Target`, or
the bootstrap blocker by itself.

After written review accepts this design, its implementation plan must name the
exact schemas, constructors, validators, fixed fixture inventory, commands,
and queue/frontmatter values for
`JN-BOOTSTRAP-TYPED-CORE-CONTRACT-FOUNDATION-001`. Only after that plan is
reviewed may the child move into `Ready Now`.

Closing the child must archive its exact evidence, return the queue to the next
design gate, and avoid claims that inference produces typed core or that
core-to-IR lowering exists.

## Non-Goals

This milestone does not:

- change canonical core, parser lowering, name-resolution behavior, or the
  interpreter input;
- run inference twice or infer types in a comparison adapter;
- integrate typed core into `CompiledModule` or production run mode;
- lower any expression to backend-neutral IR;
- define monomorphization caches, optimization passes, or code deduplication;
- choose universal boxing, runtime type tags, dictionary layouts, or a garbage
  collector;
- change module import/export, class visibility, overlap, orphan, default-
  method, or superclass semantics;
- introduce LLVM values, instructions, modules, data layouts, or tool calls;
- generate objects, link binaries, or define a platform ABI;
- add exhaustive default performance suites; or
- modify `jazz-hs/` or `jazz2/`.
