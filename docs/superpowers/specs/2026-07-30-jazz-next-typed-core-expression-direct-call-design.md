# Jazz-Next Typed-Core Expression and Direct-Call Design

## Status

Approved in discussion and written form on `2026-07-30`. The implementation
child `JN-BOOTSTRAP-TYPED-CORE-EXPRESSION-DIRECT-CALL-001` completed on
`2026-07-30` and is archived.

The typed-core and backend-neutral lowered-IR contract foundations are complete,
and the pre-bootstrap language-quality gate has closed. The child now produces
and validates the bounded typed-core scalar/direct-call profile during its
opt-in inference traversal and lowers it to validated backend-neutral IR.
Normal compile/run remains canonical-core/interpreter based. Closure/recursion
is the next design gate, not an implementation promotion.

## Goal

Define the first production-shaped, opt-in stage-0 path that:

- produces typed-core expressions during the existing inference traversal;
- finalizes those expressions once from the accepted solver state;
- constructs and validates one honest single-module `TypedProgram`;
- lowers closed scalar expressions and non-capturing direct calls into the
  existing backend-neutral `LoweredProgram`; and
- validates the produced IR without changing normal compilation or interpreter
  execution.

This child proves the producer/lowerer boundary. It does not claim complete
typed-core production, module integration, native execution, or self-hosted
inference.

## Current Boundary

Ordinary `JazzNext.Compiler.TypeInference` entrypoints traverse canonical core
and return an `InferenceResult` containing the unchanged expression,
diagnostics, runtime hints, and a module interface. Their recursive expression
functions return only a `Maybe ExpressionType` plus `InferState`; the completed
opt-in profile instead retains its provisional nodes during that same traversal.

`JazzNext.Compiler.TypedCore` already defines the permanent typed program,
module, statement, expression, type, representation-recipe, name, binder,
instantiation, evidence, and validation contracts. Its Haskell and Jazz
validators have exact parity over the fixed contract corpus.

`JazzNext.Compiler.LoweredIR` defines the permanent backend-neutral
program, function, block, instruction, call, representation, layout, runtime
service, terminator, and validation contracts. The completed child supplies a
separate opt-in lowerer for only its bounded profile.

The normal compiler and interpreter still consume canonical core plus runtime
hints. They must remain unchanged until later integration gates are accepted.

## Considered Approaches

### Selected: Profile-Aware Paired Inference

Generalize the internal inference result so one recursive visit can retain both
the current type result and an optional provisional typed node. Existing
entrypoints select inference-only mode and project the same public
`InferenceResult`; the new opt-in entrypoint selects the first typed-core
production profile.

Supported nodes retain their canonical child structure, raw solver types,
resolved names, and recorded semantic decisions. Unsupported nodes still run
ordinary inference but return no provisional node and report structured profile
failures. Finalization runs only after the traversal has completed.

This changes shared inference plumbing, but it keeps one semantic owner and
does not widen the first executable profile.

### Rejected: Produce Every Typed-Core Form Now

Producing every typed expression, pattern, declaration, module, instantiation,
and evidence form immediately would make the internal result uniform sooner.
It would also merge closures, recursion, control flow, pattern matching,
polymorphism, capability evidence, data layouts, imports, runtime services, and
module integration into one child. Those forms have separate ordering and
lowering decisions and remain separate gates.

### Rejected: Reconstruct Typed Core After Inference

A post-inference traversal would have to repeat instantiation, defaulting,
constraint, qualified-method, and application decisions or recover them from a
sidecar map. Inner canonical nodes do not have unique spans, and structural
paths are brittle when inference follows canonical rewrites. A second semantic
pass or sidecar reconstruction would violate the accepted typed-core boundary.

## Pipeline and Opt-In Boundary

The first profile is:

```text
resolved single-module canonical core
  -> analyzer and one inference traversal
  -> provisional typed statements and expressions plus final solver state
  -> typed-core finalization
  -> TypedProgram validation
  -> closed-scalar/direct-call lowering
  -> LoweredProgram validation
```

The producer accepts:

- one resolved entry module;
- the existing `InferenceInputs` for that module;
- one caller-supplied portable `TypedSourcePath`; and
- an explicit expression/direct-call profile selection.

The module path in `InferenceInputs` must equal the resolved module path. The
portable source path must be relative and must pass the existing typed-core
source-path contract. The profile rejects resolved imports, imported type or
capability inputs, and ambient-prelude inputs. A multi-module program is not an
input to this entrypoint. The producer never copies or normalizes an absolute
host path into typed core.

The result pairs the unchanged `InferenceResult` with one stage-0 production
status:

- blocked by existing error diagnostics;
- unsupported by the selected production profile, with complete ordered
  structured failures;
- rejected by typed-core invariant validation; or
- succeeded with a validated `TypedProgram`.

The unsupported status belongs to the Haskell stage-0 producer, not to the
mirrored typed-core contract. The existing `TypedCoreOutcome` constructors and
the Jazz-authored typed-core schema remain unchanged.

Production status precedence is fixed. Existing error diagnostics produce the
blocked status even if the same source is outside the profile. Otherwise,
ordered profile failures precede typed-core validation; typed-core validation
runs only for a completely produced program.

The lowerer accepts a `TypedProgram` and returns one of:

- typed-core validation failures;
- complete ordered structured profile failures;
- lowered-IR invariant failures; or
- a validated `LoweredProgram`.

The lowerer never accepts an unvalidated tree by convention: it runs typed-core
validation itself before checking the lowering profile.

Lowering status precedence is also fixed: typed-core validation failures,
profile failures, lowered-IR invariant failures, then success.

## Single-Pass Production

The internal inference result carries:

- the existing optional `ExpressionType`;
- an optional provisional expression selected by the active production
  profile; and
- ordered production-profile failures.

The scope traversal similarly retains provisional supported statements. This is
a return value of the traversal, not an annotation map in `InferState`.
Inference-only mode does not allocate provisional trees or production failures.

For a supported expression, the provisional node records:

- the exact canonical constructor and supported children;
- its unresolved solver `ExpressionType`;
- resolved canonical names and binders;
- the binding or application identity needed for direct calls; and
- the existing instantiation or evidence decision when one is present.

This first profile accepts no polymorphic or evidence-bearing executable node,
but the provisional shape must not make a later producer repeat inference to
obtain those decisions.

Unsupported parents and children are reported in deterministic structural
order. A node outside the profile does not receive a fabricated typed
constructor. Ordinary type inference continues so source diagnostics remain
complete and unchanged.

## Finalization

Finalization runs once after inference and only when error diagnostics and
profile failures are both empty. It:

1. applies the final solver substitution to every provisional type;
2. applies the existing binding and literal defaults;
3. converts `ExpressionType` into the permanent `TypedType`;
4. derives the exact `TypedRepresentationRecipe`;
5. converts canonical `Name` values into `TypedCoreName` values;
6. assigns deterministic binder ids from module and structural paths;
7. constructs monomorphic typed schemes and the single-module interface;
8. builds `TypedProgram Nothing [entryModule] entryModulePath`; and
9. runs `validateTypedProgram`.

`TIntegerLiteralType` defaults to `TypedIntType` with a signed 64-bit recipe.
`TIntType` remains the semantic `Int` alias with that recipe. `TFloatType`
remains the semantic `Float` alias with a 64-bit float recipe. Explicit numeric
types retain their exact signed, unsigned, or floating width.

Source and qualified names must not survive the resolver boundary. Unresolved
solver variables, generalized schemes, evidence parameters or selections,
managed recipes, and function recipes outside a supported top-level function
produce profile failures rather than guessed values.

The module interface contains exactly the supported functions selected by the
resolved public export inventory. Non-value exports and exported definitions
outside the profile are rejected. Source statement order, canonical export
inventory order, and function order remain deterministic.

## First Production Profile

### Supported Scalar Values

The scalar profile contains:

- unit, represented by the empty tuple and `TypedUnitRecipe`;
- `Bool`;
- `Char`;
- `Int` and `Float`;
- `Int8`, `Int16`, `Int32`, `Int64`;
- `UInt8`, `UInt16`, `UInt32`, `UInt64`; and
- `Float16`, `Float32`, `Float64`.

`Text` is managed and is not a scalar in this profile.

Supported expressions are scalar literals, scalar parameter references,
supported builtin binary expressions, fully saturated local direct calls, and
the nested applications needed to express those calls. The supported builtin
operators are:

- arithmetic: `+`, `-`, `*`, `/`;
- ordering: `<`, `<=`, `>`, `>=`; and
- equality: `==`, `!=`.

Application written with `$` is already canonical application by this boundary.
It is supported whenever the resulting application spine is an otherwise valid
fully saturated direct call.

### Supported Functions and Statements

One supported module may contain:

- optional concrete monomorphic signatures for supported functions;
- top-level bindings whose values are one or more leading lambdas followed by a
  supported scalar body; and
- one or more ordered scalar expression statements.

The final expression statement is the module result. Earlier expression
statements are evaluated, typed, and lowered in source order and their scalar
results are discarded. A module with no expression statement is outside this
profile and must produce a structured profile failure before permanent
typed-core construction or validation. Empty modules must never reach a
partial host operation.

Same-scope function rebinding and duplicate/shadowed leading-lambda parameter
names are outside this first profile. The producer and lowerer reject them
structurally before success or lowered-IR invariant validation; this slice does
not guess which name-based declaration or binder identity should win.

The producer treats the canonical root `EBlock` as the module scope. Nested
`EBlock` expressions remain outside the first profile.

Leading curried lambdas become one ordered lowered parameter list. Every
parameter and result must have a concrete scalar type and recipe.

Permanent typed-core validation predeclares a later same-module function only
when a prior matching concrete monomorphic signature is present and the
binding value has one or more leading typed lambdas. This narrow mirrored
Haskell/Jazz amendment permits the required source-ordered forward acyclic
call graph without exposing later scalar values, unsigned functions,
generalized/evidence-bearing functions, or unrelated declarations. The fixed
16-valid / 28-invalid typed-core contract manifest remains unchanged; one
valid and two invalid supplemental visibility cases prove exact parity twice.
Nested typed blocks retain ordinary source-order visibility and do not receive
this module-only forward predeclaration.

A supported function:

- is named by a resolved current-module value name;
- captures no lexical value;
- is not self-recursive or part of a mutually recursive component;
- is never used as a bare value;
- is called only with exactly its full flattened arity; and
- may call other supported functions in the same module.

The local direct-call graph may be forward-referencing but must be acyclic.
Calls to imported definitions, ambient-prelude definitions, constructors,
builtins other than the listed primitive operators, or user-defined operator
values remain outside this profile.

### Explicit Deferrals

The first producer/lowerer rejects:

- `Text`, lists, non-unit tuples, data values, and managed recipes;
- local block bindings and scalar top-level constants;
- partial application, over-application, higher-order parameters or results,
  bare function values, closures, and captures;
- self recursion, mutual recursion, and tail-call lowering;
- explicit type application, generalized schemes, representation parameters,
  capability parameters, and evidence uses;
- conditionals, cases, patterns, guards, joins, switches, and nontrivial blocks;
- data, class, and impl declarations;
- imports, ambient prelude ownership, multiple modules, runtime services, and
  data-layout requests; and
- Jazz-authored analysis, inference, elaboration, or lowering.

## Direct-Call Lowering

The first lowered program contains:

- version `1`;
- no layouts;
- no runtime services;
- supported local functions in source order;
- one synthetic entry function after the local functions; and
- the synthetic entry function id as the program entry.

Stable function identifiers are `::`-joined resolved module-path segments plus
the source value name. The entry identifier is the same module path plus the
compiler-reserved `$entry` member, which cannot collide with an ordinary source
value. Each function has one block named `entry`, parameters named `arg1`,
`arg2`, and so on, and temporaries named `t1`, `t2`, and so on in emission
order within that block. Binder ids use the same zero-based module, statement,
and expression child paths consumed by `TypedCore.Validate`. No identifier uses
a host path, pointer, hash, or map iteration order.

The lowerer maps:

- unit, boolean, character, integral, and floating literals to matching
  `LoweredImmediate` operands;
- scalar parameters to `LoweredFunctionParameterOperand`;
- supported operators to the matching `LoweredPrimitiveOperation`; and
- fully saturated local calls to `LoweredDirectCall`.

Nested operands are lowered left to right. A primitive operation or direct call
emits one result temporary. An immediate or parameter may be returned directly.
Every function and the synthetic entry have one `LoweredReturn` terminator.
A tail-position direct call remains an ordinary call followed by return in this
profile; tail-call terminators belong to the later control-flow child.

The lowerer derives every representation from validated typed-core node
information. It does not inspect inference state, redo defaulting, select
evidence, or infer call signatures.

## Structured Failure Model

Production-profile failures carry:

- a module, statement, and expression structural path;
- a stable failure kind; and
- only the typed name, type, arity, or feature detail needed by that kind.

The fixed failure kinds cover invalid profile inputs, unsupported statements or
expressions, unresolved names, unsupported or non-concrete types,
non-monomorphic schemes, managed representations, captures, recursive
components, bare function values, partial or over-applied calls, non-local call
targets, and unsupported exports.

Lowering-profile failures use typed-core structural paths and cover unsupported
typed statements or expressions, managed or callable value positions, invalid
function shape, captures, recursion, call arity, and non-local targets.

Profile failures are not source diagnostics and are not typed-core or
lowered-IR invariant failures. Rendered prose is not the testing contract.

## Fixed Fixture Inventory

One new focused suite owns an audited manifest of exactly 16 accepted fixtures:

1. `unit-entry`;
2. `bool-entry`;
3. `char-entry`;
4. `default-int-entry`;
5. `default-float-entry`;
6. `explicit-numeric-widths`;
7. `arithmetic-operators`;
8. `ordering-operators`;
9. `equality-operators`;
10. `scalar-parameter-return`;
11. `single-argument-direct-call`;
12. `curried-multi-argument-direct-call`;
13. `forward-direct-call-dag`;
14. `nested-direct-calls`;
15. `dollar-direct-call`; and
16. `exported-direct-function`.

`explicit-numeric-widths` covers all eleven explicit numeric types. The
operator fixtures cover every operator admitted by this profile.

The suite also owns an audited manifest of exactly 20 rejected fixtures:

1. `source-diagnostic`;
2. `invalid-portable-source-path`;
3. `resolved-import`;
4. `ambient-prelude-input`;
5. `text-value`;
6. `list-value`;
7. `non-unit-tuple`;
8. `data-value`;
9. `conditional`;
10. `pattern-case`;
11. `local-block-binding`;
12. `bare-function-value`;
13. `partial-direct-call`;
14. `oversaturated-direct-call`;
15. `capturing-function`;
16. `self-recursive-function`;
17. `mutually-recursive-functions`;
18. `polymorphic-or-evidence-function`;
19. `imported-direct-call`; and
20. `user-defined-operator-call`.

Each accepted source is parsed, resolved, inferred, finalized, typed-core
validated, lowered, and lowered-IR validated twice. Tests compare complete
structured production status, complete `TypedProgram`, and complete
`LoweredProgram` values against explicit expected values. Each rejected fixture
compares the complete ordered structured failure result twice.

The suite audits fixture names, counts, uniqueness, scalar-width coverage,
operator coverage, and failure-kind coverage. It also proves that
inference-only entrypoints return unchanged results for representative accepted
and rejected sources.

Existing typed-core and lowered-IR Haskell/Jazz contract suites remain the
cross-language schema and validator oracle. This child does not invent a
Jazz-authored producer or comparison adapter.

## Implementation Ownership and Exact Target Paths

Production changes are limited to:

- `jazz-next/src/JazzNext/Compiler/Analyzer.hs`;
- `jazz-next/src/JazzNext/Compiler/TypeInference.hs`;
- `jazz-next/src/JazzNext/Compiler/TypeInference/Diagnostics.hs`;
- `jazz-next/src/JazzNext/Compiler/TypeInference/Scope.hs`;
- `jazz-next/src/JazzNext/Compiler/TypeInference/Elaboration.hs` (new);
- `jazz-next/src/JazzNext/Compiler/LoweredIR/Validate.hs`;
- `jazz-next/src/JazzNext/Compiler/TypedCore/Validate.hs`;
- `jazz-next/jazz/compiler/LoweredIRTypes.jz`;
- `jazz-next/jazz/compiler/LoweredIRValidate.jz`;
- `jazz-next/jazz/compiler/TypedCoreValidate.jz`;
- `jazz-next/src/JazzNext/Compiler/LoweredIR/Lower.hs` (new); and
- `jazz-next/jazz-next.cabal`.

Test ownership is limited to:

- `jazz-next/test/JazzNext/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures.hs`
  (new); and
- `jazz-next/test/JazzNext/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec.hs`
  (new);
- `jazz-next/test/JazzNext/Compiler/Bootstrap/CanonicalLoweredIRComparison.hs`;
- `jazz-next/test/JazzNext/Compiler/Bootstrap/JazzLoweredIRContractSpec.hs`;
- `jazz-next/test/JazzNext/Compiler/Bootstrap/JazzTypedCoreContractSpec.hs`.

Coordination and status paths are:

- `docs/execution/blocker-contracts.md`;
- `docs/execution/done-archive.md`;
- `docs/execution/queue.md`;
- `docs/superpowers/specs/2026-07-10-jazz-next-bootstrap-interpreter-profile-design.md`;
- this design;
- `docs/superpowers/plans/2026-07-30-jazz-next-typed-core-expression-direct-call.md`
  (created only after written-design approval); and
- `jazz-next/README.md`.

The implementation must not modify the permanent constructors in
`TypedCore.hs` or `LoweredIR.hs`. The reviewed forward signed-function
visibility defect authorizes only the mirrored validator and supplemental
parity paths named above. Any other contract defect stops the child for a
reviewed design amendment rather than silently widening these target paths.

The reviewed Task 3 analyzer/signature-ownership amendment authorizes
`Analyzer.hs` only to carry production-only eligible forward-function facts
through the existing analyzer/inference boundary. Eligibility and ordinary
pending-signature state must be derived together by one source-scope
preparation owner from one signature elaboration result; a discarded
eligibility prepass plus normal signature re-elaboration is not permitted.

The reviewed Task 4 UInt64 amendment authorizes both permanent lowered-IR
validators and their existing cross-language contract suite to correct the
unsigned-64 immediate range to `0..18446744073709551615`. The first
upper-half value and maximum must validate in both implementations; the first
overflow must fail identically. This does not widen variant/tag carrier rules
or authorize Haskell constructor changes. Because Jazz `Int` constructor
fields coerce through signed 64-bit range before validation, the Jazz
`LoweredUnsignedIntegerImmediate` payload is instead canonical signed-decimal
`Text`; the checked adapter owns the Haskell `Integer` to Jazz `Text` bridge,
and the Jazz validator retains negative, malformed, and overflow ownership.

No `.jz` compiler implementation is added in this child. Any Jazz source used
as a fixture must use the canonical language surface already implemented; no
legacy or test-only syntax is permitted.

## Verification

The implementation child must run:

1. the new focused typed-core/direct-call suite together with the existing
   typed-core and lowered-IR contract suites;
2. a warning-clean development build;
3. the serialized routine `jazz-next` Cabal test matrix;
4. `cabal check`;
5. execution-queue and documentation validation; and
6. whitespace validation.

The exact commands are:

```text
nix --extra-experimental-features 'nix-command flakes' develop -c cabal test --project-dir=jazz-next jazz-typed-core-expression-direct-call-spec jazz-typed-core-contract-spec jazz-lowered-ir-contract-spec --jobs=1 --test-show-details=failures
nix --extra-experimental-features 'nix-command flakes' develop -c cabal build --project-dir=jazz-next -fdevelopment all --jobs=1
nix --extra-experimental-features 'nix-command flakes' develop -c cabal test --project-dir=jazz-next all --jobs=1 --test-show-details=failures
nix --extra-experimental-features 'nix-command flakes' develop -c bash -lc 'cd jazz-next && cabal check'
bash scripts/check-execution-queue.sh
bash scripts/check-docs.sh
git diff --check
```

Tests assert structured values and behavior. They must not assert
implementation source text, constructor declarations by string search, Haskell
`Show` output, absolute paths, or unordered rendering.

## Queue Transition

Written approval and implementation closeout were recorded on `2026-07-30`.
The archived plan
`JN-BOOTSTRAP-TYPED-CORE-EXPRESSION-DIRECT-CALL-001` fixed the exact ownership,
fixture manifests, non-goals, and verification commands from this design.
The completed profile is intentionally opt-in: normal compile/run continues to
use canonical core and the interpreter. Closure/recursion is now the next
ordered design gate; control flow, multi-module integration, LLVM, object/link,
and native-runtime work remain unpromoted until separate approved designs and
executor-ready children exist.

## Non-Goals

This design does not:

- change canonical core, parser lowering, name resolution, ordinary
  diagnostics, runtime hints, `InferenceResult`, module compilation, or
  interpreter input;
- add typed core or lowered IR to normal compile/run results;
- run inference twice or retain annotations in a sidecar map;
- produce complete typed core;
- change the permanent mirrored typed-core or lowered-IR schemas;
- add a Jazz-authored analyzer, resolver, inference engine, elaborator, or
  lowerer;
- lower closures, recursion, control flow, patterns, multi-module or import
  integration, managed values, layouts, runtime services, or tail calls;
- introduce optimizations, bytecode, or a VM;
- define LLVM lowering, a native-runtime ABI, object generation, or linking; or
- modify `jazz-hs/` or `jazz2/`.
