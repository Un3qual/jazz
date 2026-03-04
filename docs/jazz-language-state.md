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
  - `(+) 1 2`
- Partial operator sections are supported:
  - `(+10)`
  - `(10+)`
  - `(*2)`
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

Pattern syntax exists in lambda parameters and in the `case` parser:

- literal patterns
- variable patterns
- wildcard `_`
- tuple patterns
- list patterns, including cons-like forms such as `[hd | tl]`
- constructor patterns such as `Cons(hd, _)`

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

Important caveat: function arrows are currently parsed left-associatively, not right-associatively. In other words:

- `a -> b -> c` is parsed as `(a -> b) -> c`
- not the more conventional `a -> (b -> c)`

That is almost certainly accidental and should be treated as an implementation bug rather than intended language design.

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

`jazz-hs` does not implement an effect or purity checker. The `!` suffix is just allowed syntax for identifiers; there is no semantic enforcement of purity.

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

- Extending parsed signature type grammar beyond the current `Int`/`Bool` subset in `jazz-next` (binding/signature coherence contract itself is implemented and test-covered):
  - `docs/spec/semantics/bindings-and-signatures.md`
  - `jazz-next/src/JazzNext/Compiler/TypeInference.hs`
- Extending staged operator roadmap work in `jazz-next` beyond implemented v1 parser/fixity/sections behavior:
  - `docs/spec/syntax/operators.md`
  - `jazz-next/test/OperatorFixitySpec.hs`
  - `jazz-next/test/OperatorSectionSpec.hs`
- Extending primitive semantics coverage beyond the implemented v1 runtime/type subset (`+`, `-`, `*`, `/`, `==`, `!=`, `map`, `hd`, `tl`) as the runtime surface expands:
  - `docs/spec/runtime/primitive-semantics.md`
  - `jazz-next/test/PrimitiveSemanticsSpec.hs`
  - `jazz-next/test/RuntimeSemanticsSpec.hs`
- Extending the locked warning-flag tooling contract in `jazz-next` beyond the implemented `same-scope-rebinding` category:
  - `docs/spec/tooling/compiler-warning-flags.md`
- Whether tuples are a core runtime feature or just parsed syntax in active implementation behavior.
- Whether modules/imports are purely syntactic organization or part of a real file/module loader.
- Whether the standard library is meant to be self-hosted in `.jz` or hardcoded in the compiler/runtime.
- Whether ADTs and pattern matching are central in the current design or just inherited scaffolding.
- Whether the eventual target is JavaScript, LLVM, or both.

## Authority Hierarchy And Working Baseline

If you need a practical baseline for continuing Jazz, use this order:

1. Treat `docs/spec/*` as the canonical source of truth when a section exists.
2. For uncovered semantic areas, use `jazz-hs` behavior/tests as legacy evidence and implement convergence work in `jazz-next`.
3. Treat the top-level README as aspirational/non-normative summary text.
4. Treat `static/Prelude.jz` as a future-design sketch, not an exact spec.
5. Treat `jazz2` as a reference-only redesign source, not the active implementation target.
6. Assume the currently working active implementation (`jazz-next`) is a small interpreter-oriented expression language with:
   - dot-separated statements and scope blocks
   - application and list literals
   - `if ... else ...` surface expressions (canonicalized to `case` internally)
   - built-in operator fixity plus executable left/right section semantics
   - strict primitive typing/runtime semantics for `+`, `-`, `*`, `/`, `==`, `!=`, `map`, `hd`, `tl`
   - runtime execution via `--run` CLI mode plus compile-mode placeholder codegen output

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

Status update for item #1:

- Decision lock is recorded in `docs/spec/authoritative-syntax.md`.
- Implementation/test alignment is still pending and must be executed in `jazz-next`.

Status update for item #5:

- Implemented-vs-planned split is now published in `README.md`.
- Canonical evidence-backed feature status is now tracked in `docs/feature-status.md`.

1. Rebase ADT/pattern execution planning (`domain 11`) onto current `jazz-next` parser/type/runtime architecture and tests.
2. Rebase module/import loader planning (`domain 09`) onto `jazz-next` with deterministic file-resolution diagnostics.
3. Rebase stdlib boundary planning (`domain 10`) onto `jazz-next` to lock intrinsic-vs-prelude ownership.
4. Extend staged operator roadmap work in `jazz-next` (user-defined operator phases) according to `docs/spec/syntax/operators.md`.
5. Extend warning-flag plumbing beyond `same-scope-rebinding` in `jazz-next` according to `docs/spec/tooling/compiler-warning-flags.md`.
6. Keep legacy `jazz-hs` parse-only behavior documented as historical evidence only; do not add new compiler behavior there.
