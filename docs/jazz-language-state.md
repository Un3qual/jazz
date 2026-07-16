# Jazz Language State

This document consolidates what the repository currently says about Jazz across:

- the top-level [README](../README.md)
- the older Haskell implementation in [jazz-hs](../jazz-hs)
- the later rewrite attempt in [jazz2](../jazz2)

The codebase is now governed by an explicit authority policy:

1. Canonical language rules belong in `docs/spec/*`.
2. Active compiler behavior belongs in `jazz-next/` and its linked tests/plans.
3. `jazz-hs` and `jazz2` are read-only reference implementations unless a user
   explicitly asks for legacy maintenance.

Policy reference:

- `docs/spec/governance/spec-authority-policy.md`

## Short Summary

Jazz appears intended to be a statically typed, mostly functional language inspired by Haskell and Elixir, with:

- simple syntax
- strong type inference
- curried functions by default
- immutable bindings
- algebraic data types and pattern matching
- a trait/typeclass-like abstraction system

Today, the active end-to-end implementation is the smaller `jazz-next` subset
tracked by `docs/feature-status.md`, the runtime/semantic specs, and the
execution queue. The older `jazz-hs` compiler remains useful historical
evidence, but its parse-only behavior is not an active implementation target.

The active compiler uses one surface-parser ownership model and a canonical core
with `EIf`, `EPatternCase`, and ordinary application nodes. Module-graph mode
parses and lowers every source once, resolves structured names into a retained
dependency-ordered graph, compiles each module against explicit interfaces, and
evaluates each module against explicit runtime exports. The implementation is a
private Haskell library behind the supported `jazz-next` CLI; these internal
boundaries do not change Jazz syntax.

Active `jazz-next` module headers accept optional explicit export allowlists.
The contextual prefixes `value`, `constructor`, `type`, and `class` select one
exact typed namespace; bare selectors retain the compatibility behavior of
publishing every owned same-text entry. `type Box(..)` publishes the type and
all of its owned constructors; `type Box(Pack, Empty)` publishes the type and
only the selected owned constructors. The grouped forms expand into the same
flat typed inventory used by the rest of the compiler. Omitting the list
preserves export-all behavior, while `()` publishes nothing. Resolver dependencies, compiler
imports, and runtime publication share the validated public typed inventory;
unlisted owned declarations remain available inside the defining module for
resolution, inference, and evaluation. Unknown, wrong-namespace, or
imported-only header entries report `E4015`, and re-exports remain unsupported.

Active `jazz-next` also implements single-line `Char` and `Text` literals.
Character literals use single quotes, text literals use double quotes, and the
accepted escapes are `\\`, `\'`, `\"`, `\n`, `\r`, `\t`, `\0`, and
`\u{HEX}` for a Unicode scalar. Both types work in adjacent rank-1 generic
signatures, literal patterns, lists/tuples, strict equality/inequality, runtime
rendering, module transport, `Eq`, and `Ord`. Ordinary explicit-import `Char`
and `Text` modules add Unicode classification and simple case mapping plus
scalar-aware indexing, slicing, construction, concatenation, search, splitting,
replacement, trimming, and padding. Public library code composes private
backend-neutral kernel adapters where primitive scalar/text behavior is
required. Bytes, normalization, locale-sensitive conversion, and implicit
`Char`/`Text` conversion remain unimplemented. The complete current API and
complexity contract is in `jazz-next/jazz/stdlib/README.md`.

The broader Jazz-authored library also provides total list helpers, `Maybe`,
`Result`, `NonEmpty`, insertion-ordered `Dictionary`, FIFO `Queue`, persistent
AVL `Map`, ordered `Set`, and the explicit-import host `IO`/`IOError` boundary.
The linear and ordered collection representations are abstract; their public
operations return persistent values and define insertion, FIFO, or ascending
order explicitly.

Host text I/O is available through ordinary explicit imports of `IO` and
`IOError`; neither module is part of the bundled prelude. `IO` exports exactly
`readText!`, `writeText!`, `readStdin!`, `writeStdout!`, `writeStderr!`,
`arguments!`, and `exit!`. Recoverable file and stream operations return
`Result(IOError, value)`. `IOError` records one of `NotFound`,
`PermissionDenied`, `AlreadyExists`, `InvalidData`, `ResourceExhausted`,
`Interrupted`, `Unsupported`, or `Other`, an optional path used only by file
operations, and a normalized message. Seven private kernel bridges cross a
typed monadic `RuntimeHost` seam: deterministic hosts drive tests, pure legacy
entry points use a disabled host, module evaluation propagates an explicitly
injected host, and CLI run mode alone installs the production strict-UTF-8
host. The Jazz API owns the durable semantic boundary; Haskell exceptions and
platform error numbers remain stage-0 details, and the future native runtime
must implement the same contract without exposing LLVM types through the
frontend.

The Haskell interpreter is the stage-0/reference execution engine. The selected
long-term artifact pipeline is Jazz source to canonical typed core to a
permanent backend-neutral lowered IR, then LLVM IR, object files, native
linking, and a native runtime ABI. No bytecode or bytecode VM detour is planned.

## What The Top-Level README Claims

The top-level [README](../README.md) describes Jazz as:

- a functional language inspired by Haskell and Elixir
- easier for beginners than Haskell's more category-theory-heavy style
- strongly and statically typed
- highly inference-driven
- eventually LLVM-backed for performance
- built around more approachable typeclasses such as `Collection` and `Orderable`

It explicitly claims or strongly implies the following language features:

- ADTs
- pattern matching
- tuples
- first-class functions
- curried functions
- immutable variables
- pure-by-default functions
- impure functions marked with `!`
- module namespaces like `module Person::Organs::Heart { ... }`
- statement/declaration terminators written as `.`
- type annotations written with `::`
- a right-associative `$` application operator

The README examples also imply:

- lambdas use `\(args) -> expr`
- multiline function bodies use `{ ... }`
- lists use `[ ... ]`
- function application can be space-separated

## What Legacy `jazz-hs` Actually Implements

`jazz-hs` is a legacy reference implementation. Its local [README](../jazz-hs/README.md) is empty, so its behavior is visible through parser, AST, type inference, tests, and example programs, but new compiler work should not target it.

### Legacy Pipeline

The implemented pipeline in [jazz-hs/src/Lib.hs](../jazz-hs/src/Lib.hs) is:

1. parse source text
2. run analysis (currently type inference only)
3. run optimizer
4. generate JavaScript

The Haskell interpreter in [jazz-hs/src/Interpreter.hs](../jazz-hs/src/Interpreter.hs) is mostly commented out and should be treated as non-functional.

### Concrete Syntax In `jazz-hs`

The parser in [jazz-hs/src/Parser/Lang.hs](../jazz-hs/src/Parser/Lang.hs) and tests in [jazz-hs/test/ParserSpec.hs](../jazz-hs/test/ParserSpec.hs) define this surface syntax:

- A program is a sequence of root expressions separated by `.`.
- Blocks use `{ ... }` and contain the same dot-separated program form.
- Line comments use `//`.
- Block comments use `{* ... *}`.
- Lowercase identifiers are normal variable/function names.
- Uppercase identifiers are used for types, constructors, and module path segments.
- Internal names may start with `$`, such as `$intAdd` or `$Int`.
- Function application is left-associative by juxtaposition:
  - `f x y` means `(f x) y`
- Parenthesized call syntax also works:
  - `f(5)`
- Infix operators are parsed as curried function application:
  - `1 + 2` becomes `((+) 1) 2`
- Operator identifiers can be used as functions:
  - `(+)`
  - `(+) 1 2`
- Partial operator sections are supported:
  - `(+10)`
  - `(10+)`
  - `(*2)`
- Sections keep Haskell-style meaning and are distinct from ordinary partial application:
  - `(+ 2)` means `\x -> x + 2`
  - `((+) 2)` means `\x -> 2 + x`
- `$` is parsed as right-associative low-precedence application.
- Source-local user operators can be declared with fixed tiers or custom
  numeric precedence:
  - `operator %% tier 2.`
  - `operator %% precedence 25.`
- User-operator declarations can include explicit associativity:
  - `operator %% tier 2 left.`
  - `operator <| precedence 10 right.`
  - `operator ?> tier 4 nonassoc.`
- Declared user operators become executable through ordinary parenthesized
  bindings such as `(%%) = \(left) -> \(right) -> left + right.`

### Parsed Expression Forms

The parser and AST support:

- integer literals
- float literals
- boolean literals: `True`, `False`
- string literals
- list literals: `[1, 2, 3]`
- tuple literals: `(1, 2)`
- variable references
- lambdas: `\(x) -> expr`
- multi-argument lambdas, which desugar into nested unary lambdas
- blocks
- variable declarations: `x = expr`
- type signatures: `x :: Type`
- imports
- modules
- data declarations
- class declarations
- class implementations
- `case` expressions

### Parsed Pattern Forms

Pattern syntax is active in the `case` parser and in lambda parameter lists.
Pattern-shaped lambda parameters lower through internal `case` expressions so
they reuse the same binder, type, and runtime matching rules:

- literal patterns
- variable patterns
- wildcard `_`
- tuple patterns
- list patterns, including cons-like forms such as `[hd | tl]`
- constructor patterns such as `Cons(hd, _)`
- as-patterns such as `whole @ Just item`

### Parsed Declarations

The parser accepts:

- value binding:
  - `x = 5`
- type signature:
  - `x :: Integer`
- import:
  - `import Foo::Bar`
  - `import Foo::Bar as B`
  - `import Std::List (map, filter)`
- module:
  - `module Foo::Bar { ... }`
- data declaration:
  - `data Maybe(a) { Just(a), Nothing }`
- class declaration:
  - `class @{Ord(a)}: Eq(a) { ... }`
- class implementation:
  - `impl @{Ord(a)}: Eq(Integer) { ... }`

### Type Syntax In `jazz-hs`

The parser supports:

- named types:
  - `Integer`
  - `Bool`
  - `Maybe(Integer)`
- type variables:
  - `a`
- list types:
  - `[a]`
- tuple types:
  - `(a, b)`
- function types:
  - `a -> b`
- constrained type signatures:
  - `x :: @{Eq(a), Ord(b)}: a -> b -> c`

Active-path note: `jazz-next` now parses function arrows right-associatively. In other words:

- `a -> b -> c` means `a -> (b -> c)`
- `(a -> b) -> c` requires explicit parentheses

The older left-associative behavior should be treated as legacy-reference drift rather than the active language contract.

Active-path note: `jazz-next` now carries one recursive signature tree through ordinary and constrained signatures, class methods, impl targets, explicit type application, module interfaces, and runtime evidence. The accepted rank-1 grammar includes lower-case variables; primitives; exact-arity named applications such as `Maybe(Char)` and `Result(IOError, Text)`; applications nested in lists, tuples, and functions; and right-associative arrows. Adjacent generic signatures implicitly quantify variables in deterministic first-occurrence order, rigidly check those variables at the definition, reject implementation constraints not entailed by the declared contract, and instantiate them freshly at each use; explicit signatures also make direct constructor aliases polymorphic. Named types resolve through the visible type namespace, preserve nominal module identity, and reject unknown, partial, or wrong-arity applications with `E2009` at the owning signature or explicit-argument span. Existing constraint policy remains: visible class arity and impl facts are required, duplicate or unused constraints reject, constrained variables must occur in the body, and class method signatures cannot introduce method-local variables. Explicit type application binds the first quantified variable and records runtime evidence only when the instantiated type is concrete; polymorphic templates preserve real type-variable nodes rather than nominal sentinels. Higher-rank types, higher-kinded variables, method-local quantification, type lambdas, aliases, and explicit `forall` remain outside the active contract.

### Builtins And Type Environment In `jazz-hs`

The hardcoded builtin type environment in [jazz-hs/src/Types.hs](../jazz-hs/src/Types.hs) only includes:

- `+`
- `-`
- `*`
- `/`
- `==`
- `print!`
- `map`
- `hd`
- `tl`

The hardcoded trait set includes:

- `Num`
- `Integral`
- `Fractional`
- `Eq`
- `Ord`
- `Showable`
- `Default`

The builtins imply:

- arithmetic is curried
- `map` is function-first in the implementation type environment:
  - `map :: (a -> b) -> [a] -> [b]`
- `hd` and `tl` operate on lists

### What Actually Works End To End

The only features that clearly work through parse -> type inference -> optimization -> JS generation are the smaller core:

- literal expressions
- top-level sequential bindings
- simple variable references
- simple lambdas with plain parameters
- function application
- builtin arithmetic
- list literals
- simple tuple typing (but not tuple codegen)
- `print!`
- `map`
- `hd`
- `tl`
- `$` application

Example programs in [jazz-hs/ExamplePrograms](../jazz-hs/ExamplePrograms) mostly stay within this subset.

### JavaScript Runtime Semantics In `jazz-hs`

The JS backend in [jazz-hs/src/CodeGen/Javascript.hs](../jazz-hs/src/CodeGen/Javascript.hs) lowers builtins to a tiny JS prelude:

- `+`, `-`, `*`, `/` become curried JS helpers
- `map` becomes `xs.map(f)`
- `hd` becomes array destructuring of the first element
- `tl` becomes array destructuring of the tail
- `==` becomes JavaScript loose equality (`==`)
- `print!` becomes `console.log(...)`

Constant folding in the optimizer only handles integer `+`, `-`, and `*`.

## Legacy `jazz-hs` Features That Exist Mostly As Scaffolding

A large part of the richer language exists in legacy `jazz-hs` AST and parser
form, but is not fully supported by analysis and code generation. This is
historical evidence only; active feature work must use `jazz-next` contracts.

These features appear partially implemented or parse-only:

- `data` declarations
- `class` declarations
- `impl` declarations
- `module` declarations
- `import` declarations
- `case` expressions
- tuple code generation
- pattern-matching function parameters
- constructor-aware type inference/runtime behavior
- true module loading
- a real prelude/standard library hookup

Key examples:

- `case` has parser support, but type inference does not implement it.
- tuple literals parse and infer, but JS generation explicitly errors on tuples.
- lambda pattern parameters parse, but JS generation errors on non-simple parameters.
- type signatures parse and are analyzed, but JS generation has no branch for `ETypeSignature`.
- `if` exists in the AST and code generator, but there is no parser for `if ... else ...`, so it is not currently reachable from source code.

## Top-Level README vs `jazz-hs` Mismatches

Several important inconsistencies exist between the aspirational README and the concrete Haskell implementation.

### `map`/`filter` Argument Order

This mismatch has been resolved in active docs and `jazz-next` behavior. The
top-level README now documents and demonstrates function-first collection
combinators:

- `map :: (a -> b) -> [a] -> [b]`
- `map f xs`
- `filter p xs`

Historical collection-first examples such as `map xs f` or `filter xs p` are
non-canonical archival evidence only. Active `jazz-next` work must not add a
parser alias, runtime adapter, or deprecated-syntax warning path for those
forms.

### Function Definition Style

The top-level README says functions are "declared with assignment to a lambda", but also shows:

- `add10 = (+10).`
- `add10List = map add10.`

So the docs implicitly treat any expression assignment as a function definition, not just lambda assignment.

### Purity / Effects

The top-level README says:

- functions are pure by default
- impure functions must end with `!`
- pure functions cannot call impure functions

`jazz-next` now enforces a stub-v1 purity contract in compile/analyze paths:

- names ending with `!` are treated as impure callees,
- pure bindings reject direct calls to known impure callees,
- impure bindings and top-level expression statements remain permissive.

Current limitations (still planned):

- no effect polymorphism,
- no higher-order purity proofs,
- no cross-module purity graph.

Normative stub-v1 contract:

- `docs/spec/semantics/purity-bang-stub-v1.md`

### Typeclass Naming

The top-level README describes approachable typeclasses like `Collection` and `Orderable`.

`jazz-hs` instead contains:

- parser syntax centered on `class` / `impl`
- traits in the type system named `Num`, `Eq`, `Ord`, etc.

The names and abstraction model are related, but not stable or consistent.

### Claimed Features vs Working Features

The top-level README strongly presents ADTs, pattern matching, tuples, and modules as language features.

`jazz-hs` only partially supports them:

- many of them parse
- several infer partially
- several still fail in code generation or runtime behavior

## `static/Prelude.jz` Looks Like A Different Dialect

The file [jazz-hs/static/Prelude.jz](../jazz-hs/static/Prelude.jz) is valuable because it shows intended direction, but it does not cleanly match the currently working parser/compiler.

It includes:

- nested modules
- richer data definitions
- `trait` declarations
- `impl` blocks
- internal runtime primitives like `$intAdd`
- wrapper types like `Int($Int)` and `Float($Float)`

But there are multiple mismatches with the active parser:

- the parser recognizes `class`, not `trait`
- the parser's `impl` syntax expects constraint syntax like `@{...}:`, while the prelude often uses simpler `impl Num(Int) { ... }`
- the legacy compiler did not auto-load this prelude
- much of the functionality implied by the prelude is not wired into code generation or analysis

Best interpretation: `Prelude.jz` captures intended future language/library design more than current executable behavior.

## What `jazz2` Adds

`jazz2` is a reference-only design source and is non-normative for current Jazz behavior. It is also a mostly unfinished rewrite: its local [README](../jazz2/README.md) is empty, the parser entrypoint is effectively empty, the lexer is `undefined`, and the standard library `.jz` files are empty placeholders.

The meaningful information in `jazz2` is mostly in [jazz2/src/Jazz/AST.hs](../jazz2/src/Jazz/AST.hs):

- qualified names are clearly intended
- there is an expression core with:
  - variables
  - constructors
  - application
  - lambdas
  - literals
  - `let`
  - `case`
- there are simple patterns:
  - variable
  - wildcard
  - constructor patterns
- there is a type core with:
  - type constructors
  - type variables
  - type application
  - function arrows

However:

- there is no working concrete syntax
- signatures are structurally incomplete
- module/import support is mostly commented out
- richer declarations are mostly commented out
- there is no operator system yet
- stdlib files are empty

Best interpretation: `jazz2` shows the shape of a potential cleaner redesign, but not a usable language definition.

## Things That Are Still Unsettled Or Implementation-Pending

Based on the full repo, these areas still require implementation convergence even when a decision lock now exists:

- Extending the implemented rank-1 generic named-type contract beyond exact-arity nominal applications, deterministic implicit quantification, module transport, explicit first-variable application, and concrete runtime evidence remains blocked behind separate verifier-backed children. Future work includes higher-rank and higher-kinded types, type lambdas, aliases, explicit `forall`, associated types, and user-visible dictionaries:
  - `docs/spec/semantics/bindings-and-signatures.md`
  - `jazz-next/src/JazzNext/Compiler/TypeInference.hs`
- Extending staged operator roadmap work in `jazz-next` beyond implemented v1
  parser/fixity/sections behavior, source-local fixed-tier declarations,
  same-source executable operator bindings, adjacent operator signatures, and
  custom numeric precedence/associativity. Runtime overload dispatch,
  cross-module operator APIs, new precedence ranges, and new built-in operators
  remain blocked until separate executable contracts exist:
  - `docs/spec/syntax/operators.md`
  - `jazz-next/test/JazzNext/Compiler/Parser/OperatorFixitySpec.hs`
  - `jazz-next/test/JazzNext/Compiler/Parser/OperatorInvalidSyntaxSpec.hs`
  - `jazz-next/test/JazzNext/Compiler/Parser/OperatorSectionSpec.hs`
  - `jazz-next/test/JazzNext/Compiler/Semantics/RuntimeSemanticsSpec.hs`
- Extending primitive semantics coverage beyond the implemented v1 runtime/type subset (`+`, `-`, `*`, `/`, `==`, `!=`, `<`, `<=`, `>`, `>=`, `map`, `filter`, `hd`, `tl`, `print!`, target-named numeric conversions `toInt8`..`toFloat64`, Float64 fractional literal defaults, direct annotated `Float16`/`Float32` fractional literal bindings, same concrete `Float`/`Float16`/`Float32`/`Float64` arithmetic with width-preserving runtime float results, same concrete `Float`/`Float16`/`Float32`/`Float64` comparison/equality, and structural list/tuple/ADT equality over equality-supported element and constructor payload types) as the runtime surface expands:
  - `docs/spec/runtime/primitive-semantics.md`
  - `jazz-next/test/JazzNext/Compiler/Semantics/PrimitiveSemanticsSpec.hs`
  - `jazz-next/test/JazzNext/Compiler/Semantics/RuntimeSemanticsSpec.hs`
- Extending class/impl abstraction semantics beyond the active declaration,
  constrained-signature, concrete-impl, and explicit `Class::method` dispatch
  slice. The bundled prelude supplies executable `Eq.equals`, `Ord.compare`,
  `Showable.show`, and `Default.defaultValue` methods over appropriate built-in
  scalar types. Unqualified overloads, under-applied overloaded function
  values, default methods, superclasses, and broader cross-module coherence
  remain future work. `Self` is not reserved.
- Extending the locked warning-flag tooling contract in `jazz-next` beyond the implemented `same-scope-rebinding`, `shadowing-outer-scope`, and ordinary block `unused-binding` emitters (reserved metadata for `deprecated-syntax` / `W0004` is covered, but this category is closed as reserved-only for the current active language surface until a future accepted syntax surface is intentionally deprecated):
  - `docs/spec/tooling/compiler-warning-flags.md`
- CLI source selection is active in `jazz-next`: standalone compile and `--run`
  read stdin by default or one positional `.jz` source file when provided; source
  files are rejected with module-graph `--entry-module` mode. The tooling
  contract is tracked in `docs/spec/tooling/cli-source-input.md`.
- CLI discoverability is active in `jazz-next`: explicit `--help` / `-h` usage
  output exits `0`, writes usage to stdout, preempts
  source/config/prelude/module reads, avoids a bare `help` subcommand so
  positional source paths remain intact, and keeps compile/run semantics
  unchanged.
- Tuple literals, concrete tuple signature types, fixed-arity tuple case
  patterns, cons-like list case patterns, and pattern-shaped lambda parameters
  are now active core runtime/type features in `jazz-next`.
- Module/import loading semantics are partially implemented in `jazz-next`:
  canonical brace-bodied declarations, alias/symbol-list imports, grouped
  constructor exports, explicit visibility diagnostics, and qualified lookup
  flow through a parse-once graph, per-module interfaces, and runtime exports.
  The clarification matrix and active contracts are
  `docs/spec/modules/00-module-clarification-matrix.md` through
  `docs/spec/modules/06-explicit-export-lists.md`.
- Whether ADTs and pattern matching are central in the current design or just inherited scaffolding.
- Implementing the selected LLVM native backend: backend-neutral lowered IR,
  LLVM lowering, object generation/linking, and the native runtime ABI remain
  pending after the target decision.

## Authority Hierarchy And Working Baseline

If you need a practical baseline for continuing Jazz, use this order:

1. Treat `docs/spec/*` as the canonical source of truth when a section exists.
2. For uncovered semantic areas, use `jazz-hs` behavior/tests as legacy evidence and implement convergence work in `jazz-next`.
3. Treat the top-level README as aspirational/non-normative summary text.
4. Treat `static/Prelude.jz` as a future-design sketch, not an exact spec.
5. Treat `jazz2` as a reference-only redesign source, not the active implementation target.
6. Assume the currently working active implementation (`jazz-next`) is a small interpreter-oriented expression language with:
   - dot-separated statements and scope blocks
   - handwritten-parser parity coverage in `jazz-next/test/JazzNext/Compiler/Parser/ParserFoundationSpec.hs`, `ModuleImportParserSpec.hs`, `OperatorFixitySpec.hs`, and `OperatorSectionSpec.hs` that locks current core expression, module/import, operator/fixity, and section AST shape, span, and deterministic diagnostic behavior before any future Megaparsec/CST migration
   - canonical lambdas with lexical closure runtime support (`\(x) -> expr`, multi-argument lambdas lowered into nested unary functions); pattern-shaped parameters lower through internal pattern-case bodies while preserving ordinary unary core lambdas
   - application, list literals, and tuple literals
   - adjacent rank-1 signatures over primitives, lower-case variables, exact-arity named applications, nested list/tuple/function compositions, empty `@{}:` wrappers, concrete constrained signatures, and solver-backed variable constrained signatures, with fresh per-use instantiation and explicit first-variable type application
   - `if ... else ...` surface expressions retained as canonical core `EIf`
   - canonical `data` declarations, including lowercase generic declaration parameters preserved in active metadata, with constructor values/applications plus direct `case <expr> { | pattern -> expr ... }` parsing/lowering for literal, wildcard, variable, constructor, bracketed-list, cons-like list, tuple, and as-patterns; analyzer/type/runtime execution covers integer, boolean, character, text, and fractional literal patterns, wildcard, variable, declared constructor patterns, exact-length bracketed-list patterns, cons-like list head/tail patterns, fixed-arity tuple patterns, and as-patterns
   - active top-level/module-body `class` and concrete `impl` declarations,
     constrained signatures, explicit `Class::method` dispatch, substituted
     impl-body checking, and deterministic missing/ambiguous evidence
     diagnostics. The bundled prelude provides executable `Eq.equals`,
     `Ord.compare`, `Showable.show`, and `Default.defaultValue` methods plus the
     `Num`, `Integral`, and `Fractional` marker capabilities across appropriate
     built-in scalar types. Unqualified overloads, under-applied overloaded
     function values, default methods, and superclasses remain future work.
   - opt-in compiler warnings for same-scope rebinding (`W0001`),
     outer-scope shadowing (`W0002`), and ordinary block unused bindings
     (`W0003`), with warning-as-error promotion while preserving default
     warning-silent compilation
   - built-in operator fixity plus executable left/right section semantics
   - strict primitive typing/runtime semantics for `+`, `-`, `*`, `/`, `==`, `!=`, `<`, `<=`, `>`, `>=`, plus prelude-provided public helpers `map`, `filter`, `hd`, `tl`, `print!`, target-named numeric conversions `toInt8`..`toFloat64`, backend-independent `Char`/`Text` literals and equality, default Float64 fractional literal values, direct annotated `Float16`/`Float32` fractional literal bindings, same concrete `Float`/`Float16`/`Float32`/`Float64` arithmetic with width-preserving runtime float results, same concrete `Float`/`Float16`/`Float32`/`Float64` comparison/equality, and structural list/tuple/ADT equality over equality-supported element and constructor payload types, while numeric-width planning now uses cross-platform `Int64`/`Float64` defaults, source-exact fractional literal conversion checks, context-directed literals, and explicit conversion for mixed concrete widths
   - stage-0 runtime execution via `--run` CLI mode, with standalone CLI source input selected from stdin by default or one positional `.jz` file, while successful CLI and driver compile paths are diagnostic-only: compile returns warnings/errors and no generated artifact; LLVM-generated native binaries are the selected future artifact target
   - bundled-prelude loading by default in `compileSource`, `runSource`, `compileModuleGraph`, `runModuleGraph`, and CLI paths, while explicit no-prelude entry points (`compileSourceWithPrelude Nothing`, `runSourceWithPrelude Nothing`, `compileModuleGraphWithPrelude Nothing`, `runModuleGraphWithPrelude Nothing`, `--no-prelude`, and low-level AST/runtime helpers) expose only `__kernel_*` bridge names; source and module graph harnesses now cover public alias rejection, kernel bridge availability, bundled helper visibility, default bundled capability class and impl-fact visibility, no-prelude capability-fact absence, and explicit-prelude helper visibility, and the checked-in `jazz-next/jazz/stdlib/Prelude.jz` mirror is covered against the catalog-generated bundled prelude source
   - explicit-import Jazz-authored foundation modules `List`, `Maybe`,
     `Result`, and `NonEmpty`; their public constructors and operation families
     are documented in `jazz-next/jazz/stdlib/README.md` and execute through
     ordinary generic signatures, constructors, patterns, and Jazz functions.
   - explicit-import persistent collections: insertion-ordered `Dictionary`,
     FIFO `Queue`, AVL `Map`, and ordered `Set`. Their constructors and
     invariants are private, while checked-in model traces verify observable
     ordering, update, and persistence behavior.
   - explicit-import Unicode `Char` and `Text` utilities plus strict UTF-8
     `IO`/`IOError`; only the irreducible scalar, bulk-text, and host operations
     cross private backend-neutral kernel seams.
   - shipped Jazz-authored sources are separated under `jazz-next/jazz/`:
     thirteen user-facing/special prelude sources live in `stdlib/`, while the
     hosted `Lexer` and `LexerTypes` modules live in `compiler/`. Compiler
     sources may import stdlib modules; a parsed repository audit rejects the
     inverse dependency.

## Hybrid Semantic-Change Workflow

- Semantic language changes must be documented by a decision record or RFC before implementation.
- Non-semantic/internal changes may be implementation-first only when docs/tests are updated in the same change.
- Policy details and examples are in `docs/spec/governance/spec-authority-policy.md`.

## Top-level Docs Contract

- `README.md` is a high-level summary and must keep language claims split into:
  - "Implemented Today (verified)"
  - "Planned / Aspirational"
- `docs/feature-status.md` is the canonical feature-status matrix and evidence source.
- If README and matrix wording diverge, treat `docs/feature-status.md` as authoritative and bring README back in sync.
- Feature status changes must include evidence paths and a verification commit reference in `docs/feature-status.md`.

## Recommended Next Spec Cleanup

If this repo is going to become a coherent language project, the highest-value cleanup would be:

Status update for item `#1`:

- Active-path ADT/pattern contract is now recorded in `docs/spec/adt-pattern-semantics.md` and `docs/spec/pattern-matching-semantics.md`.
- The active ADT/pattern rebase is closed around the currently landed `jazz-next` subset: direct `case` parsing/lowering plus analyzer/type/runtime execution for literal, wildcard, variable, constructor, exact-length bracketed-list, cons-like list, fixed-arity tuple, and as-patterns; pattern-shaped lambda parameters lowered through internal pattern cases; canonical `data` declaration parsing/lowering; generic declaration-parameter metadata for shapes such as `data Maybe a = Nothing | Just a.`; generic constructor value/application type schemes for direct constructor uses; analyzer/type/runtime support for constructor values and constructor application arity; structural ADT equality for declared constructors with equality-supported payloads; tuple literal values and concrete tuple signature types; and deterministic `E3023` runtime diagnostics for constructor over-application paths.
- Direct generic constructor values/applications and eligible ordinary or signed generic bindings instantiate their quantified variables freshly per use; ordinary constructor aliases remain monomorphic unless given an explicit generic signature.
- Pattern-shaped lambda parameters are active on the `jazz-next` path and reuse the committed `case` pattern engine through lowering.

Status update for item `#3`:

- Stub-v1 purity enforcement is now implemented in active `jazz-next` compiler/analyzer flow.
- Normative behavior is documented in `docs/spec/semantics/purity-bang-stub-v1.md`.

Status update for item `#5`:

- Implemented-vs-planned split is now published in `README.md`.
- Canonical evidence-backed feature status is now tracked in `docs/feature-status.md`.

Runtime/product status:

- Active `jazz-next` product docs now describe one interpreter-backed path:
  successful compile is diagnostic-only, successful `--run` prints evaluated
  runtime output, and future product/runtime behavior deltas remain blocked
  until they have concrete target paths and verification.

1. Keep future pattern forms such as guards, or-patterns, and pattern synonyms blocked until concrete binder/type/runtime contracts are planned on the active path; tuple literals, concrete tuple signature types, fixed-arity tuple case patterns, cons-like list case patterns, as-patterns, and lambda parameter patterns now execute as core runtime/type features.
2. Keep future module/import work (`domain 09`) scoped to concrete product or semantic deltas beyond the closed active Phase 6 harness. The file-layout parser/resolver, resolution/import-binding, and loader/migration harnesses are complete, and the file layout/package-root, deterministic resolution/cycle, loader pipeline, qualified import, and migration policy specs are published.
3. Keep future stdlib work scoped to concrete needs with explicit public names,
   edge behavior, complexity, and runtime/backend contracts. Hash collections,
   bytes/encoding, broader I/O, and Unicode normalization remain separate
   candidates; the current library is documented in
   `jazz-next/jazz/stdlib/README.md`.
4. Extend staged operator roadmap work in `jazz-next` (user-defined operator phases) according to `docs/spec/syntax/operators.md`.
5. Keep `deprecated-syntax` / `W0004` reserved-only until a future accepted active-path syntax surface is intentionally deprecated; implement a W0004 emitter only after that surface, payload, target paths, and focused verification are specified in `jazz-next`.
6. Keep legacy `jazz-hs` parse-only behavior documented as historical evidence only; do not add new compiler behavior there.
