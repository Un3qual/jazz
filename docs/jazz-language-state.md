# Jazz Language State

This document consolidates what the repository currently says about Jazz across:

- the top-level [README](../README.md)
- the older Haskell implementation in [jazz-hs](../jazz-hs)
- the later rewrite attempt in [jazz2](../jazz2)

The codebase is now governed by an explicit authority policy:

1. Canonical language rules belong in `docs/spec/*`.
2. Until a semantic area is fully specified there, `jazz-hs` behavior/tests are the temporary behavioral authority.
3. `jazz2` is reference-only and non-normative in the current phase.

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

Today, the only end-to-end implemented subset is much smaller. In practice, `jazz-hs` supports a small expression language that can be parsed, type-checked, optimized a little, and compiled to JavaScript. Many richer features parse but do not work end to end.

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

## What `jazz-hs` Actually Implements

`jazz-hs` is the main concrete source of truth. Its local [README](../jazz-hs/README.md) is empty, so the real spec lives in parser, AST, type inference, tests, and example programs.

### Active Pipeline

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

Active-path note: `jazz-next` now lexes and parses constrained signatures such as `x :: @{Eq(a), Ord(b)}: a -> b -> c` into structured parser/core payloads. Empty constraint blocks (`@{}:`) normalize to the existing monomorphic signature subset. Non-empty concrete unary constraints over `Int`, `Bool`, width-specific numeric types, nested concrete lists, or concrete tuple compositions normalize as annotation-only monomorphic signatures only when the source has a visible matching `class` declaration and concrete `impl` fact, for example `class Eq { }.` plus `impl Eq(Int) { }.` before `x :: @{Eq(Int)}: Int`. The default bundled prelude now supplies canonical class declarations for `Eq`, `Ord`, `Num`, `Integral`, `Fractional`, `Showable`, and `Default`, but it does not yet supply default concrete impl facts. Known unary constraints over lower-case type variables still normalize under a monomorphic annotation-only contract when every signature variable appears in a supported unary constraint and every constrained variable appears in the signature body; for example, `id :: @{Eq(a)}: a -> a` is accepted but later use sites refine the same binding type rather than receiving fresh polymorphic instantiations. Missing concrete class/impl facts, unknown constraints, wrong-arity constraints, unconstrained body variables, unused constrained variables, type applications, and function-type constraint arguments still reject deterministically with `E2009` and retain the attached signature statement as their primary diagnostic span. Duplicate non-empty constraint names also reject with `E2009`, name the duplicate constraint, and retain the signature statement span.

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

## `jazz-hs` Features That Exist Mostly As Scaffolding

A large part of the richer language exists in AST and parser form, but is not fully supported by analysis and code generation.

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

### `map` Argument Order

The top-level README documents:

- `map :: (a -> b) -> [a] -> [b]`

But its example uses:

- `map myArr \(i) -> ...`

That example is collection-first, while `jazz-hs` implements `map` as function-first. The example and the implementation do not agree.

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
- the active compiler does not auto-load this prelude
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

- Extending parsed signature type grammar beyond the closed structured-signature rebase in `jazz-next` (adjacent signatures over `Int`, `Bool`, nested concrete list types, concrete tuple types, right-associative function types, parenthesized function-type overrides, empty `@{}:` constrained wrappers, class/impl-validated concrete unary constrained signatures, duplicate non-empty constraint diagnostics, monomorphic variable constrained signatures, unsupported-variable constrained-signature diagnostics, and unsupported constrained-signature primary spans are implemented and test-covered; polymorphic/generalized type-variable signatures, type-scheme instantiation/generalization, defaulting, and solver-backed constraints remain blocked behind a future semantics contract):
  - `docs/spec/semantics/bindings-and-signatures.md`
  - `jazz-next/src/JazzNext/Compiler/TypeInference.hs`
- Extending staged operator roadmap work in `jazz-next` beyond implemented v1 parser/fixity/sections behavior:
  - `docs/spec/syntax/operators.md`
  - `jazz-next/test/JazzNext/Compiler/Parser/OperatorFixitySpec.hs`
  - `jazz-next/test/JazzNext/Compiler/Parser/OperatorSectionSpec.hs`
- Extending primitive semantics coverage beyond the implemented v1 runtime/type subset (`+`, `-`, `*`, `/`, `==`, `!=`, `map`, `filter`, `hd`, `tl`, `print!`, and target-named numeric conversions `toInt8`..`toFloat64`) as the runtime surface expands:
  - `docs/spec/runtime/primitive-semantics.md`
  - `jazz-next/test/JazzNext/Compiler/Semantics/PrimitiveSemanticsSpec.hs`
  - `jazz-next/test/JazzNext/Compiler/Semantics/RuntimeSemanticsSpec.hs`
- Extending class/impl abstraction semantics in `jazz-next` beyond the active
  parser/core declaration ownership, duplicate class/impl fact validation,
  concrete constrained-signature fact checks, and bundled-prelude canonical
  class declarations; method bodies, dispatch, dictionaries, default methods,
  superclass semantics, inferred constraints, defaulting, cross-module
  coherence, and runtime evidence remain future work.
- Extending the locked warning-flag tooling contract in `jazz-next` beyond the implemented `same-scope-rebinding`, `shadowing-outer-scope`, and ordinary block `unused-binding` emitters (reserved metadata for `deprecated-syntax` / `W0004` is covered, but this category does not emit diagnostics yet and its concrete warning policy is deferred):
  - `docs/spec/tooling/compiler-warning-flags.md`
- CLI source selection is active in `jazz-next`: standalone compile and `--run`
  read stdin by default or one positional `.jz` source file when provided; source
  files are rejected with module-graph `--entry-module` mode. The tooling
  contract is tracked in `docs/spec/tooling/cli-source-input.md`.
- Tuple literals, concrete tuple signature types, fixed-arity tuple case
  patterns, cons-like list case patterns, and pattern-shaped lambda parameters
  are now active core runtime/type features in `jazz-next`.
- Module/import loading semantics are partially implemented in `jazz-next`: canonical brace-bodied module declarations, alias/symbol-list imports, explicit symbol-list visibility diagnostics, alias-import unqualified visibility diagnostics, `Alias::symbol` qualified alias lookup, default bundled-prelude module graph driver helpers, explicit no-prelude module graph ownership checks, and deterministic resolver/binding diagnostics now work in the active parser/CLI path. The baseline clarification matrix is tracked in `docs/spec/modules/00-module-clarification-matrix.md`; module file layout/package-root behavior, deterministic resolution/cycle behavior, loader pipeline behavior, qualified import/name-binding semantics, and migration/compatibility policy are specified in `docs/spec/modules/01-file-layout-and-package-roots.md`, `docs/spec/modules/02-resolution-algorithm-and-cycles.md`, `docs/spec/modules/03-loader-behavior-and-diagnostics.md`, `docs/spec/modules/04-qualified-imports-and-binding.md`, and `docs/spec/modules/05-migration-and-compatibility.md`. The active file-layout parser/resolver, resolution/import-binding, and loader/migration harnesses are now locked in `jazz-next/test/JazzNext/Compiler/Parser/ModuleImportParserSpec.hs`, `jazz-next/test/JazzNext/Compiler/Modules/ModuleResolutionSpec.hs`, `jazz-next/test/JazzNext/Compiler/Modules/LoaderSpec.hs`, and `jazz-next/test/JazzNext/CLI/CLISpec.hs`; no active Phase 6 module harness row remains in `Ready Now`.
- Whether ADTs and pattern matching are central in the current design or just inherited scaffolding.
- Which non-JavaScript product backend, if any, should exist beyond interpreter-backed execution.

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
   - adjacent type signatures over the supported monomorphic subset (`Int`, `Bool`, nested concrete list types, concrete tuple types, right-associative function types, explicit parenthesized function-type overrides, empty `@{}:` constrained wrappers, concrete unary constrained signatures, and known unary variable constrained signatures under the monomorphic annotation-only contract)
   - `if ... else ...` surface expressions (canonicalized to `case` internally)
   - canonical `data` declarations, including lowercase generic declaration parameters preserved in active metadata, with constructor values/applications plus direct `case <expr> { | pattern -> expr ... }` parsing/lowering for literal, wildcard, variable, constructor, bracketed-list, cons-like list, tuple, and as-patterns; analyzer/type/runtime execution covers literal, wildcard, variable, declared constructor patterns, exact-length bracketed-list patterns, cons-like list head/tail patterns, fixed-arity tuple patterns, and as-patterns
   - active top-level/module-body `class` and `impl` abstraction declarations that lower into core declaration nodes, reject duplicate class declarations and duplicate concrete impl facts, and let concrete constrained signatures validate against visible class/impl facts; non-canonical `trait` declarations reject with diagnostics pointing future abstraction syntax back to `class`/`impl`, while `class`/`impl`/`trait` remain available as ordinary binding, signature, and qualified-alias identifiers; the bundled default prelude now declares the canonical `Eq`, `Ord`, `Num`, `Integral`, `Fractional`, `Showable`, and `Default` classes, but method bodies, dispatch, dictionaries, default concrete impl facts, defaulting, and runtime evidence remain future work
   - opt-in compiler warnings for same-scope rebinding (`W0001`),
     outer-scope shadowing (`W0002`), and ordinary block unused bindings
     (`W0003`), with warning-as-error promotion while preserving default
     warning-silent compilation
   - built-in operator fixity plus executable left/right section semantics
   - strict primitive typing/runtime semantics for `+`, `-`, `*`, `/`, `==`, `!=`, plus prelude-provided public helpers `map`, `filter`, `hd`, `tl`, `print!`, and target-named numeric conversions `toInt8`..`toFloat64`; numeric-width planning now uses cross-platform `Int64`/`Float64` defaults, context-directed literals, and explicit conversion for mixed concrete widths
   - runtime execution via `--run` CLI mode, with standalone CLI source input selected from stdin by default or one positional `.jz` file, while successful CLI and driver compile paths are diagnostic-only: compile returns warnings/errors and no generated artifact
   - bundled-prelude loading by default in `compileSource`, `runSource`, `compileModuleGraph`, `runModuleGraph`, and CLI paths, while explicit no-prelude entry points (`compileSourceWithPrelude Nothing`, `runSourceWithPrelude Nothing`, `compileModuleGraphWithPrelude Nothing`, `runModuleGraphWithPrelude Nothing`, `--no-prelude`, and low-level AST/runtime helpers) expose only `__kernel_*` bridge names; source and module graph harnesses now cover public alias rejection, kernel bridge availability, bundled helper visibility, default bundled capability class visibility without default impl facts, and explicit-prelude helper visibility, and the checked-in `jazz-next/stdlib/Prelude.jz` mirror is covered against the catalog-generated bundled prelude source

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
- The active ADT/pattern rebase is closed around the currently landed `jazz-next` subset: direct `case` parsing/lowering plus analyzer/type/runtime execution for literal, wildcard, variable, constructor, exact-length bracketed-list, cons-like list, fixed-arity tuple, and as-patterns; pattern-shaped lambda parameters lowered through internal pattern cases; canonical `data` declaration parsing/lowering; generic declaration-parameter metadata for shapes such as `data Maybe a = Nothing | Just a.`; generic constructor value/application type schemes for direct constructor uses; analyzer/type/runtime support for constructor values and constructor application arity; tuple literal values and concrete tuple signature types; and deterministic `E3023` runtime diagnostics for constructor over-application paths.
- Direct generic constructor values and applications instantiate declaration-owned type parameters freshly per use, while ordinary user bindings remain monomorphic and do not generalize constructor aliases.
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
3. Keep remaining stdlib-boundary follow-up work (`domain 10`) scoped to concrete future prelude/catalog growth; the current bundled source/module graph paths, explicit no-prelude module graph boundary, and checked-in prelude reproducibility evidence are covered in `jazz-next`.
4. Extend staged operator roadmap work in `jazz-next` (user-defined operator phases) according to `docs/spec/syntax/operators.md`.
5. Implement future warning emitters for the remaining reserved `deprecated-syntax` metadata in `jazz-next` according to `docs/spec/tooling/compiler-warning-flags.md`.
6. Keep legacy `jazz-hs` parse-only behavior documented as historical evidence only; do not add new compiler behavior there.
