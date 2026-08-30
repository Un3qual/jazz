# Jazz Haskell Quality Remediation Design

**Date:** 2026-08-30

**Status:** Approved

## Objective

Improve the active Haskell compiler, runtime, CLI, benchmark support, and
tests without changing the public Jazz language contract except for one
confirmed module-import correctness bug and one benchmark-only diagnostic.
The work should make invalid states harder to represent, remove unlawful or
obsolete interfaces, use standard Haskell collections where they match the
domain, and reduce maintenance risk without introducing generic frameworks.

## Audited Scope

The audit covered all active Haskell paths:

- `src/` — 95 production modules, approximately 50,700 lines.
- `app/` — the CLI entry point.
- `benchmark/` and `program-support/` — benchmark and corpus support.
- `test/` — 151 Haskell test modules across 67 Cabal test suites.

The clean baseline is GHC 9.14.1 through the repository Nix shell. The
development build completed with repository warnings promoted to errors, and
all 67 Cabal test suites passed before implementation began.

## Design Principles

1. Preserve public Jazz syntax, typing, diagnostic ordering, module ordering,
   import precedence, runtime evaluation order, and bootstrap parity.
2. Prefer structured values to serialized semantic keys.
3. Use lawful standard typeclasses only. A value with no meaningful general
   equality relation must not expose `Eq` for test convenience.
4. Use `Set` for membership and uniqueness, `Map` for keyed lookup, `Seq` only
   for genuinely incremental ordered append, and lists for small/source-ordered
   traversals where their asymptotics remain linear.
5. Retain independent fail-closed validators and exact diagnostic order.
6. Remove private compatibility APIs only after migrating every live caller.
7. Extract repeated pure decisions, but do not introduce a monad-polymorphic
   runtime framework or other abstraction larger than the duplication removed.
8. Add behavioral regressions before correctness changes and characterization
   coverage before high-risk refactors.
9. Format touched files only. A repository-wide formatter rewrite would hide
   semantic changes inside unrelated churn.

## 1. Structured Capability Facts and Identifier Policy

Concrete implementation facts currently cross compiler boundaries as rendered
`Text`. `ModuleCompiler.rebaseFact` then tokenizes that text with an identifier
predicate that disagrees with the lexer: valid identifier continuations `!`
and `'` are not recognized. Imported capabilities or types using those
characters can therefore fail to rebase and produce a false missing-impl
diagnostic.

Introduce a structured `ConcreteImplFact` containing the capability `Name` and
the concrete `SignatureType`. Store `Set ConcreteImplFact` in
`ScopeCapabilityFacts` and `ModuleInterface`. Render a fact only when producing
a diagnostic. Rebase the capability name and every name inside its signature
tree structurally; delete textual token scanning.

Derive `Ord` for `SignatureType`, whose constructors and fields already have
lawful structural orderings. `ConcreteImplFact` can then use `Set` directly.

Centralize source identifier start and continuation predicates in
`Jazz.Compiler.Name`. Both the lexer and module-path parser will consume the
same policy. Operator-binding encoding remains separate.

Required regression: compile a module graph in which one module exports a
capability, concrete implementation, and data type whose identifiers use `!`
and `'`, then import and satisfy that fact from another module.

## 2. Lawful Runtime Equality Boundary

`Eq RuntimeValue` currently unwraps metadata wrappers and compares a subset of
constructors, but returns `False` for every other pair. Closures, builtins,
deferred host bindings, and partial applications are therefore unequal to
themselves, violating `Eq` reflexivity.

Literal pattern matching is the only production consumer of general
`RuntimeValue` equality. Replace it with a narrowly named
`runtimeValueMatchesLiteral` function that:

- unwraps `VTyped`, explicit type applications, and explicit result hints;
- compares integer and float literal payloads without treating metadata as
  source-level equality;
- compares booleans, characters, and text;
- rejects non-literal runtime constructors.

Remove `Eq RuntimeValue`, equality instances that exist only because of it,
and `Eq` from aggregate results that contain arbitrary runtime values. Tests
must assert rendered or constructor-specific observable results rather than
depend on a partial general equality relation. Jazz's source-level structural
equality continues to use the explicit runtime primitive implementation.

## 3. Linear Validation and Source-Ordered Collections

Typed Core and Lowered IR are explicit trust boundaries. They intentionally
return every validation failure in deterministic order, but several duplicate
scans use `failures <> [failure]`, `nub`, and repeated list membership. Large
invalid constructed inputs can therefore become quadratic.

Use strict folds with a `Set` of seen values and a reversed result list,
reversing once at the boundary. Replace stable `nub` operations only where
their element type has a suitable `Ord` instance and preserve first-occurrence
order. Use `Set TypedEvidenceCandidate` for candidate membership where the
existing ordering is structural.

Apply the same source-order discipline to the identified elaboration profile,
finalization, and capability-variable deduplication paths. Do not introduce
`Seq` for small collections without evidence that repeated append is the
dominant operation.

Add many-duplicate validator regressions that assert both the number and exact
order of failures. Existing canonical Haskell/Jazz parity fixtures remain
unchanged.

## 4. Explicit Inference Walk State

`inferScopeTypeInternal` has an immutable `ScopeInferenceRequest`, but its
local recursive walk passes ten positional state values through every
statement branch. Similar types and repeated resets make accidental argument
swaps or stale cache propagation difficult to review.

Introduce a strict `ScopeWalkState` record for:

- type environment and its cached free variables;
- last expression type and pending signature;
- pending signatures by statement;
- recursive-group start states and preview cache;
- module capability baseline facts;
- current inference state.

The statement list remains the recursion argument. Each branch uses an
explicit record update, making resets of the recursive preview cache visible.
Preserve transactional preview behavior, forward-signature policy, source
ordering, and Typed Core production failure order.

After that refactor is stable, migrate the remaining `InferExprFn` callers and
synthetic tests to `InferExprWithModeFn` pinned to `InferenceOnly`, then remove
the older callback and its adapters. Production-aware elaboration remains on
the richer callback.

## 5. Remove Private Compatibility APIs

The private library still exports resolver entry points that adapt the
canonical `resolveProgramWithAmbientExports` API into older test-oriented
forms. Parser declarations likewise retain list-based adapters around the
active `TokenStream` API.

Migrate tests to small test-local fixtures around the canonical APIs, then
remove:

- `resolveModuleGraph`;
- `resolveModuleGraphWithLookup`;
- `resolveModuleGraphWithLookupAndVisibleSymbols`;
- the compatibility `resolveProgram` adapter and its `Identity` machinery;
- list-returning import/data/capability parser adapters and
  `adaptListExpressionParser`.

The rich `Jazz.Compiler.ModuleGraph.ResolvedModule` remains the sole production
module artifact.

## 6. Bounded Runtime and CLI Deduplication

The pure and host-backed module evaluators must keep distinct control monads,
host allocation behavior, and public entry points. Extract only shared pure
steps: determine the module evaluation mode/import environment, and publish a
completed module/output into the accumulator. Keep the two traversal loops
explicit. Add host-free parity coverage comparing the pure and host-capable
paths.

Extract the duplicate `CompileResult` to `CliOutput` rendering in the CLI.
This is a pure presentation helper; source and module compilation orchestration
remain separate.

## 7. Standard Data Models and Mechanical Cleanup

Make the following bounded, compiler-checked changes:

- represent enabled/promoted warning categories as `Set WarningCategory`;
- replace `sort (nub patterns)` with `Set.toAscList (Set.fromList patterns)`;
- use `newtype` for `CompileResult`, `RuntimeIntMetadata`, and
  `ImplMethodType`;
- use `Data.Functor.unzip` for the `NonEmpty` structured-constructor split;
- remove the nine validated unused `OverloadedStrings` pragmas;
- fix benchmark command capture so `ExitSuccess` with disallowed empty output
  reports that the command produced no output rather than that it failed.

The benchmark diagnostic change is tooling-only and receives a direct unit
test around a pure classification helper.

## Explicit Non-Goals

- No public language syntax or specification changes.
- No wholesale split of large compiler modules based only on line count.
- No generic parser-combinator rewrite.
- No monad-polymorphic unification of the runtime engines.
- No replacement of dependency-ordered module lists with maps.
- No removal of `RuntimeOutcome`/`Either` compatibility boundaries while they
  remain active public/internal entry points.
- No removal of centralized managed-layout ownership or selective forcing.
- No blanket application of HLint suggestions; ASCII-specific predicates,
  diagnostic construction, and deliberate explicit code remain explicit.
- No repository-wide Ormolu rewrite.

## Delivery and Verification

Each numbered implementation task will be independently reviewed and
committed. Correctness fixes use witnessed red-green tests. Behavior-preserving
refactors use focused characterization suites before and after the change.

The final gate is:

```sh
nix --extra-experimental-features 'nix-command flakes' develop --command \
  cabal build all -fdevelopment --jobs=1

nix --extra-experimental-features 'nix-command flakes' develop --command \
  cabal test all --test-show-details=failures --jobs=1
```

Additionally, run pinned Ormolu on every touched Haskell file,
`git diff --check`, and an aggregate whole-branch review against this design.
