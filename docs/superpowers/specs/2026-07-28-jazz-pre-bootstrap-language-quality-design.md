# Jazz Pre-Bootstrap Language Quality Design

## Status

Implemented on `2026-07-28`.

Superseded in part on `2026-07-30`: the function-equation decision and its
related parser/lowering design are no longer active. Jazz retains
pattern-shaped lambda parameters and explicit `case` dispatch instead. See
`2026-07-30-jazz-remove-function-equations-design.md`.

This design defines the language, source-quality, benchmark, and bootstrap
hardening pass that must complete before the next typed-core/bootstrap batch.
It supersedes the earlier source-ergonomics decision that ordinary bindings
plus compact lambdas would remain the only function-definition surface.

## Goal

Make the complete authored Jazz source tree use the language Jazz has already
implemented, close the remaining function-head and constructor-type grammar
gaps, reserve `value` consistently, and add computationally substantial
programs that produce useful benchmark and profiling evidence.

## Implementation Boundary

- All compiler implementation changes land under `jazz-next/`.
- `jazz-hs/` and `jazz2/` remain read-only legacy references.
- Haskell remains the authoritative stage-0 compiler and interpreter.
- The Jazz-authored lexer, parser, canonical-core lowerer, and validators remain
  differential mirrors and move with every accepted stage-0 syntax change.
- No typed-core expression production, core-to-IR lowering, LLVM work,
  object/link work, or native-runtime implementation advances during this
  quality pass.

## Decision Summary

1. Keep Jazz's kernel small and expose ordinary functionality through
   Jazz-authored Prelude and standard-library modules.
2. Keep `if ... then ... else ...` as non-strict syntax. Do not pretend it is
   an ordinary strict function.
3. Add Haskell-style function equations with patterns in function heads,
   contiguous ordered clauses, and ordinary currying.
4. Lower function equations into the existing lambda and pattern-case core
   rather than adding a second analyzer/runtime representation.
5. Replace opaque data-constructor arguments with structured signature types.
6. Use `Type(arguments)` as the sole named type-application syntax in
   declarations and signatures.
7. Make `value` a globally reserved lexer keyword in both stage 0 and the
   Jazz-authored lexer.
8. Reformat long data declarations and enforce the format through the
   repository audit.
9. Audit every authored `.jz` file for the best applicable modern Jazz syntax,
   and require the authored source corpus collectively to exercise every
   implemented feature.
10. Add six deterministic, computationally substantial program-corpus cases.

## Delivery Decomposition

This document is the shared quality-gate design. Delivery uses three separate
implementation plans because a reviewer could accept or reject each workstream
independently:

1. **Language surface and parity**
   - reserve `value`;
   - add structured constructor field types;
   - add function equations and grouped head patterns; and
   - update the stage-0/Jazz lexer, parser, and canonical-core mirrors.
2. **Authored source and editor quality**
   - migrate reserved identifiers;
   - correct TextMate export/keyword scopes;
   - reformat long data declarations;
   - audit every authored `.jz` file for applicable modern syntax; and
   - enforce aggregate implemented-feature coverage.
3. **Algorithmic program corpus**
   - add the six full workloads;
   - establish deterministic outputs, budgets, and profiles; and
   - update benchmark and performance documentation.

The plans execute in that order. The source audit depends on the language
surface, and the new programs deliberately dogfood both earlier workstreams.
The next bootstrap milestone remains blocked until all three plans pass their
individual gates and the combined final matrix.

## Bootstrap Audit Outcome

Starting the hosted bootstrap was not an architectural mistake: the Haskell
stage-0 compiler remains the executable semantic authority, and differential
Jazz-authored lexer/parser/core modules continue to expose real portability
gaps. Advancing the next bootstrap milestone before stabilizing the source
surface would have been premature. This pass therefore paused forward
typed-core/backend work, synchronized both frontend implementations, and made
the authored-source audit a gate. Once the combined verification matrix is
green, the existing bootstrap architecture may continue; no rollback or
parallel replacement compiler is warranted.

## Approaches Considered

### Cosmetic compatibility patch

This approach would reserve `value`, reformat sources, add a few programs, and
parse a single function-head form as shallow sugar. It is smaller, but it would
leave constructor payloads opaque, would not support real ordered function
clauses, and would preserve the grammar workaround that produced forms such as
`Branch (Tree(a)) (Tree(a))`.

This approach is rejected.

### Surface normalization over the existing core

This is the chosen approach. The surface AST represents structured constructor
types and grouped function clauses. Lowering translates clauses into existing
curried lambdas and pattern cases, while downstream analysis and runtime
semantics continue to consume the established core forms.

It closes the language-quality gaps without inventing duplicate callable,
matching, or conditional machinery.

### Core and runtime redesign

This approach would add dedicated function-clause nodes, turn conditionals into
special callable builtins, or redesign every downstream compiler phase around
new representations. It would destabilize the completed canonical-core and
typed-core contracts without improving observable language semantics.

This approach is rejected.

## Kernel, Prelude, Operators, and Conditionals

Haskell does not make ordinary `if/then/else` a strict function. Jazz should
copy the useful boundary, not the misconception:

- compiler/runtime intrinsics provide behavior that ordinary Jazz cannot
  implement efficiently or at all;
- public collection, text, data, and composition APIs are Jazz-authored
  Prelude or standard-library functions wherever practical;
- builtin and declared operators remain callable values with sections and
  partial application;
- `$` remains low-precedence right-associative application and lowers to
  ordinary `EApply`; and
- `if` retains non-strict branch selection and its existing core semantics.

This batch does not add an `ifThenElse` builtin, reopen public no-prelude
fallback, add new builtin operators, or expand the kernel merely to make the
builtin catalog appear more Haskell-like.

## Function Equations and Pattern Heads

### Canonical syntax

Ordinary value bindings remain valid:

```jazz
answer = 42.
```

One or more head patterns define a function:

```jazz
identity item = item.
constant left right = left.
```

Multiple contiguous clauses with the same name define one ordered function:

```jazz
mapMaybe transform Nothing = Nothing.
mapMaybe transform (Just item) = Just (transform item).
```

Complex head patterns use parentheses when required to keep adjacent function
parameters unambiguous:

```jazz
zipWith combine [leftHead | leftTail] [rightHead | rightTail] =
  listPrepend
    (combine leftHead rightHead)
    (zipWith combine leftTail rightTail).
zipWith combine _ _ = [].
```

`(left, right)` remains one tuple pattern parameter. `left right` remains two
function parameters.

### Clause rules

1. Clauses form a group only when they are contiguous, have the same function
   name, and occur in the same statement scope.
2. Every clause in a group must have the same positive arity.
3. Clauses match from top to bottom.
4. Existing pattern binder, duplicate-binder, or-pattern, constructor-arity,
   type-unification, and runtime matching rules apply unchanged.
5. A non-exhaustive call uses the existing deterministic pattern-match failure
   path. This batch does not add exhaustiveness checking.
6. A signature immediately preceding the first clause constrains the complete
   clause group.
7. A later non-contiguous group follows the existing same-scope rebinding
   policy rather than silently joining the earlier group.
8. Function-head guards, `where` clauses, pattern synonyms, and named/default
   parameters are not introduced by this batch.

### Surface representation

The parser surface owns the new syntax explicitly:

```haskell
data SurfaceFunctionClause =
  SurfaceFunctionClause SourceSpan [SurfacePattern] SurfaceExpr

data SurfaceStatement
  = ...
  | SSFunction Identifier SourceSpan (NonEmpty SurfaceFunctionClause)
```

The exact constructor ordering may follow the existing module's style, but the
surface representation must preserve clause order, per-clause span, patterns,
and body separately. It must not store raw source text.

### Lowering

A clause group lowers to one ordinary `SLet`:

1. generate one stable internal parameter per source parameter position;
2. wrap the lowered body in the same nested unary `ELambda` structure used by
   compact lambdas;
3. match a one-argument function directly against its generated argument;
4. match a multi-argument function against an `ETuple` of generated arguments;
5. lower each source clause into one ordered `CaseArm`; and
6. preserve source spans and generated-name identity for diagnostics,
   bootstrap comparison, and later typed-core production.

No `SFunction`, clause dispatch, or new callable value reaches analyzer,
inference, module runtime, or the interpreter.

### Pattern grouping

Parenthesized grouped patterns are accepted where needed for function heads and
remain semantically transparent. A comma distinguishes a tuple pattern from a
grouped pattern. Grouping does not add a core pattern constructor.

Grouped or-patterns remain subject to the same binder compatibility rules as
top-level or-patterns. This design does not add pattern synonyms or a new
pattern precedence system.

## Structured Data-Constructor Types

### Problem

The current parser stores constructor arguments as either a bare name or an
opaque balanced parenthesized/bracketed group. This causes:

- `Branch Tree(a) Tree(a)` to be miscounted as four constructor fields;
- `Branch (Tree(a)) (Tree(a))` to preserve arity while discarding both field
  types; and
- generic constructor inference to rely on incomplete metadata.

### Canonical constructor syntax

Constructor applications remain ordinary whitespace applications:

```jazz
tree = Branch (Leaf 1) (Leaf 2).
```

Constructor payload declarations use the existing signature-type grammar:

```jazz
data Tree a
  = Leaf a
  | Branch Tree(a) Tree(a).
```

Additional examples:

```jazz
data Result error result
  = Error error
  | Ok result.

data Callback a b
  = Callback (a -> b).

data Forest a
  = Forest [Tree(a)].
```

`Leaf(a)` is not constructor-declaration call syntax. `Tree a` is not named
type application; in a constructor declaration it denotes two positional
fields. `Type(arguments)` remains the only named type-application spelling.

### Representation and typing

- Replace `SurfaceDataConstructorArgumentName` and
  `SurfaceDataConstructorArgumentOpaque` with structured
  `SurfaceSignatureType` constructor fields.
- Replace `DataConstructorArgumentName` and `DataConstructorArgumentOpaque`
  with structured `SignatureType` constructor fields.
- Reuse the signature parser's supported type grammar and diagnostics rather
  than maintaining a second partial type parser.
- Resolve declared type parameters and named type identities through the
  existing module/type visibility rules.
- Build generic constructor schemes from the complete structured field types.
- Preserve structured field types through module interfaces, canonical-core
  comparison values, the Jazz-authored parser/core mirrors, and typed-core
  contract inputs.
- Remove the opaque fallback instead of retaining it as compatibility syntax.

Redundant grouping such as `(Tree(a))` remains parseable as ordinary type
grouping, but authored code and canonical examples use `Tree(a)`.

## Globally Reserved `value`

### Lexer contract

`value` becomes a real reserved keyword:

- add a dedicated stage-0 token kind;
- add the corresponding `CanonicalKeyword` case to Jazz-authored lexer values;
- classify it identically in both lexers;
- update canonical lexer/parser/core comparison adapters and fixtures; and
- reject it anywhere an ordinary binding, parameter, pattern binder, type
  variable, module alias, or unqualified identifier is required.

The module export parser consumes the keyword as the `ValueNamespace` selector
prefix:

```jazz
module Example (value answer) {
  answer = 42.
}
```

The other export prefixes retain their current contextual behavior. Reserving
`value` does not silently reserve `type`, `constructor`, or `class`.

### Source migration

Existing authored uses of `value` are renamed by meaning, not by a global text
replacement:

- collection elements use `item` or a domain name;
- successful computations use `result`;
- constructor contents use `payload`;
- literal/token contents use `literal`, `scalar`, or `tokenText`;
- traversal state uses `current`;
- generic type variables use a domain name or a conventional short variable;
  and
- module export prefixes remain `value`.

Comments, text literals, diagnostic examples, and historical documentation are
changed only when they describe active syntax or executable source.

### Editor behavior

The TextMate grammar must stop applying the export-modifier scope globally:

- module export-list regions recognize `value`, `type`, `constructor`, and
  `class` as export modifiers;
- `value` receives a reserved-keyword scope outside those regions;
- ordinary identifiers containing `value` as a substring are unaffected; and
- the representative fixture includes valid export use and an invalid
  standalone-keyword highlighting example in a separately non-parsed fixture
  if needed.

Repository tests validate the relevant grammar scopes and the executable editor
fixture through the active parser.

## Jazz Source Formatting

### Data declaration layout

A data declaration that would exceed 100 columns uses a multiline layout:

```jazz
data TypedLiteral
  = TypedIntegerLiteral Text
  | TypedFractionalLiteral Text Text Maybe(TypedNumericType)
  | TypedBooleanLiteral Bool
  | TypedCharacterLiteral Char
  | TypedTextLiteral Text.
```

When one constructor still exceeds 100 columns, its payload types continue on
indented lines:

```jazz
data TypedFunction
  = TypedFunction
      TypedFunctionId
      Maybe(TypedParameter)
      [TypedParameter]
      TypedRepresentation
      [TypedBlock]
      TypedBlockId.
```

Short declarations may remain on one line. The final dot terminates the final
constructor, and continuation indentation follows the owning module's existing
two-space body indentation.

### Enforcement

The repository's Jazz source-format audit checks data-declaration line length
and continuation structure over shipped stdlib/compiler sources, program
corpus sources, and editor fixtures. Tests that intentionally contain malformed
or one-line syntax fixtures are excluded by role rather than weakened globally.

## Repository-Wide Idiomatic Jazz Audit

### Scope

The audit covers every authored `.jz` file under:

- `jazz-next/jazz/stdlib/`;
- `jazz-next/jazz/compiler/`;
- `jazz-next/programs/`; and
- `jazz-next/editors/vscode-jazz/fixtures/`.

Embedded Jazz fixtures in Haskell tests are reviewed when touched by lexer,
parser, lowering, or semantic changes, but they are not rewritten when their
purpose is to preserve a specific historical or invalid syntax boundary.

### File-level rule

Every authored file is reviewed for the best applicable current language:

- function equations and pattern heads instead of a lambda immediately
  followed only by a dispatching `case`;
- compact lambdas for anonymous higher-order functions;
- `$` where it removes grouping without obscuring data construction;
- operator sections and bare operator values where they express the operation
  directly;
- explicit type application where it resolves or intentionally documents a
  generalized choice;
- constructor, list, tuple, as-, or-, and guarded patterns where they replace
  manual tests or projections;
- ordinary Prelude/stdlib functions instead of direct kernel names outside
  bridge-owning modules; and
- multiline data declarations under the format contract.

The audit does not force every feature into every file. A file without a
polymorphic ambiguity does not receive decorative type application, and a
clear direct application is not rewritten to `$` merely to increase a count.

### Aggregate feature coverage

Add an AST-based repository feature inventory over the complete authored
source set. The inventory must prove that the authored sources collectively
exercise every implemented surface family, including:

- literals and scalar/numeric-width forms;
- ordinary and compact-lambda bindings;
- function equations and multi-clause pattern heads;
- currying and partial application;
- lists, tuples, Unit, generic ADTs, and structured constructor payloads;
- every active pattern family and case guards;
- conditionals;
- ordinary application, `$`, builtin operator values, sections, declared
  operators, precedence, and associativity;
- signatures, constrained signatures, and explicit type application;
- modules, aliases, imports, explicit imports, and every export selector form;
- classes, impls, qualified methods, and compiler-owned evidence-consuming
  source paths;
- pure and `!`-marked host-facing functions; and
- all currently public Prelude and standard-library module families.

The feature inventory uses parsed AST behavior rather than brittle whole-file
substring assertions. A small source-form check is acceptable only for syntax
whose AST intentionally erases the distinction, such as `$` lowering.

The implementation plan includes a file-by-file review ledger so no authored
source silently escapes the audit.

## Program Corpus Expansion

### New full workloads

Add six deterministic `full` cases:

1. `n-queens`
   - backtracking search with pattern-head clauses, lists, higher-order
     predicates, and deterministic solution counting;
   - stresses branching, pattern attempts, closures, and list construction.
2. `merge-sort`
   - stable divide-and-conquer sorting over a fixed nontrivial integer list;
   - stresses recursive splitting, merging, comparisons, and allocation.
3. `prime-sieve`
   - list-based prime generation over a fixed bound using ordinary integer
     operations and filtering;
   - stresses repeated traversal, predicates, and numeric work.
4. `fannkuch`
   - permutation generation plus prefix reversal over a fixed size;
   - stresses intensive list construction, traversal, and maximum reduction.
5. `tak`
   - the classic recursive Takeuchi workload with a fixed terminating input;
   - stresses callable application, continuation depth, and branch selection
     with little collection noise.
6. `symbolic-differentiation`
   - differentiates and repeatedly simplifies a fixed algebraic expression ADT;
   - stresses functional tree transforms, constructor patterns, equality, and
     recursive normalization.

Each program prints a concise deterministic checksum, count, or canonical
result rather than its complete intermediate data.

### Manifest and documentation

- Add each case to `programs/corpus.json` in stable lexical order.
- Make an explicit schema-version decision if the feature vocabulary expands.
- Give each case exact feature tags, benchmark groups, semantic budgets, and
  expected output.
- Document the case's algorithm and intended performance shape in
  `programs/README.md` and `PERFORMANCE.md`.
- Keep generated benchmark, statistics, and profile outputs ignored.

### Correctness and budgets

For every new case:

1. compile and run it through the production module graph;
2. compare exact stdout and termination;
3. collect runtime statistics twice and require complete equality;
4. set semantic ceilings with useful but not excessive headroom;
5. include the case in the applicable parse/lower, analysis,
   module-preparation, runtime, and whole-program groups; and
6. run a semantic profile and confirm it contains balanced, nontrivial stacks
   for the intended algorithm.

No universal wall-clock threshold is added to CI.

## Bootstrap Coordination

The current bootstrap architecture remains valid: stage-0 Haskell runs and
checks Jazz-authored compiler components through exact differential tests.
What pauses is forward feature progression, not the existing hosted path.

During this pass:

- update the Jazz-authored lexer for reserved `value`;
- update the Jazz-authored parser for function equations, grouped head
  patterns, and structured constructor field types;
- update Jazz-authored canonical-core schemas/lowering for the new surface
  values and removal of opaque constructor fields;
- extend exact repeated parity fixtures and scale coverage;
- migrate the compiler's own `.jz` sources only after the hosted parser can
  represent the new syntax; and
- keep typed-core and lowered-IR validators behaviorally unchanged except for
  schema construction needed to consume structured constructor metadata.

The next bootstrap design gate may resume only after the complete quality pass
and source audit are green.

## Diagnostics and Failure Behavior

Add deterministic diagnostics for:

- `value` used where an identifier is required;
- inconsistent arity within a contiguous clause group;
- malformed or unterminated grouped head patterns;
- invalid constructor payload types;
- undeclared constructor type parameters within nested field types; and
- unsupported constructor field type forms.

Existing diagnostics remain authoritative for duplicate binders, unknown
constructors, constructor arity, pattern mismatch, type mismatch, module
visibility, and explicit type application.

Diagnostics retain exact source paths and primary spans. Hosted parser
comparisons require the same structured reason and span as stage 0.

## Test Strategy

Implementation uses one red-green cycle per behavior:

1. reserve `value` and prove both lexers reject identifier use;
2. parse one function equation and lower it to the existing curried core;
3. group ordered clauses, diagnose arity mismatch, and execute pattern
   fallthrough;
4. parse structured constructor field types and prove generic recursive
   constructor inference;
5. remove opaque constructor metadata through module/runtime/bootstrap
   boundaries;
6. migrate editor scopes and authored source identifiers;
7. enforce multiline data formatting;
8. complete the file-by-file idiom and aggregate feature audit; and
9. add each corpus workload with deterministic statistics and profile evidence.

Tests assert observable parser, type, runtime, module, and profile behavior.
Source-string assertions are limited to editor/formatting rules and surface
forms erased intentionally during lowering.

## Verification

Focused component suites run throughout implementation. The final gate is:

```sh
nix --extra-experimental-features 'nix-command flakes' develop -c \
  cabal build --project-dir=jazz-next all
nix --extra-experimental-features 'nix-command flakes' develop -c \
  cabal test --project-dir=jazz-next all \
    --test-show-details=failures \
    --jobs=1
nix --extra-experimental-features 'nix-command flakes' develop -c \
  cabal bench --project-dir=jazz-next jazz-next-bench \
    --benchmark-options='--jazz-smoke'
bash scripts/check-docs.sh
bash scripts/check-execution-queue.sh
git diff --check
```

In addition:

- run every new program twice with runtime statistics and compare the complete
  observations;
- generate semantic profiles for all six new cases and validate deterministic
  bytes on repeated runs;
- verify all stage-0/Jazz lexer, parser, and canonical-core parity suites;
- verify the aggregate authored-feature inventory is complete; and
- inspect the final diff for gratuitous feature use, unnecessary abstractions,
  opaque fallbacks, duplicated grammar/type parsing, and accidental changes to
  `jazz-hs/` or `jazz2/`.

## Milestones and Commit Boundaries

1. Commit this approved design.
2. Commit the detailed implementation plan.
3. Reserve `value`, migrate identifiers, and correct editor scopes.
4. Add structured constructor field types and remove opaque metadata.
5. Add function equations and pattern-head lowering.
6. Bring Jazz-authored lexer/parser/core parity forward.
7. Complete the repository-wide idiomatic source and data-format audit.
8. Add and verify the six program-corpus workloads.
9. Run the complete verification matrix and close the quality gate.

Each implementation milestone must be independently testable and committed
before the next milestone begins.

## Non-Goals

- Treating `if` as a strict ordinary function.
- Adding a new conditional builtin or widening no-prelude public fallback.
- Reserving every contextual export/declaration prefix.
- Function guards, `where` clauses, default arguments, records, GADTs,
  existential constructors, infix constructors, or pattern synonyms.
- Exhaustiveness or redundancy analysis.
- New builtin operators, runtime operator overload dispatch, or cross-module
  user-defined operators.
- New typed-core production, core-to-IR lowering, LLVM emission, native
  runtime, object generation, or linking.
- Wall-clock CI performance thresholds.
- Modifying legacy compiler implementations.
