# jazz-next

This directory is the only active target for new compiler implementation work.

Legacy references:

- `jazz-hs/` is read-only historical reference code.
- `jazz2/` is read-only experimental/reference code.

Do not implement new compiler functionality in legacy directories.

`jazz-next` is currently a CLI/compiler package. Its Haskell implementation is
provided through the private `jazz-next-internal` package library solely for the
executable and test components; there is no supported Haskell embedding API yet.

The shared production-shaped program corpus is documented in
[`programs/README.md`](programs/README.md). Benchmarking, deterministic runtime
statistics, Jazz semantic flame graphs, and GHC profiling are documented in
[`PERFORMANCE.md`](PERFORMANCE.md).

## Current architecture

- `Compiler.Parser.*` owns surface grammar and one Megaparsec error model;
  `Parser.Lower` produces the canonical core AST.
- `Compiler.AST` has one conditional node (`EIf`), one pattern-case node
  (`EPatternCase`), and ordinary `EApply` for application including `$`.
- `Compiler.Name` owns both source identifiers and structured resolved names;
  compiler phases render names only for diagnostics and user output.
- `Compiler.ModuleResolver` parses and lowers each source once into a
  dependency-ordered `ResolvedProgram` of retained `CoreModule` values.
- `Compiler.ModuleCompiler` analyzes each module against explicit dependency
  `ModuleInterface` values; `Compiler.ModuleRuntime` evaluates modules against
  explicit runtime exports and executes only the entry module's expressions.
- `Compiler.TypeInference` is the public façade over focused internal type,
  state, solver, capability, pattern, scope, and diagnostic modules.
- `Compiler.DiagnosticCatalog` owns every published diagnostic code and its
  metadata; `Compiler.Diagnostics` owns the presentation-neutral report, and
  `Compiler.Diagnostics.Render` owns stable human-readable output.
- `Compiler.SignatureRendering` owns canonical source-signature text. Inferred
  expression types retain their separate type-inference renderer because they
  represent a different compiler layer and surface.
- `Compiler.Driver` coordinates prelude preparation, resolution, per-module
  compilation, warning promotion, and optional runtime evaluation. Compile and
  run results each store one ordered diagnostic stream; severity/origin accessors
  provide warning, compile-error, and runtime-error views without duplicate
  storage.
- `jazz-next.cabal` defines the private `jazz-next-internal` implementation
  library, the `jazz-next` executable, and the registered test suites.
- Cabal discovers and runs every registered test suite; `repository-audit-spec`
  owns stdlib formatting and private-package policy.

These are implementation boundaries only. The module and import syntax exposed
to Jazz programs is unchanged by the internal module pipeline.

## Jazz-authored sources

Shipped Jazz source lives under one package-owned root:

- [`jazz/stdlib/`](jazz/stdlib/README.md) contains the bundled prelude and
  general user-facing standard-library modules, including total list and text
  utilities plus persistent `Dictionary`, `Queue`, `Map`, and `Set` values.
- `jazz/compiler/` contains the Jazz-authored compiler implementation. Alongside
  the hosted lexer and canonical token types, it now includes the generic
  parser kernel plus compiler-local token, signature, immutable context,
  operator-metadata, declaration, pattern, expression, program, and
  token/source façade modules. The hosted parser covers the complete accepted
  surface grammar, including signatures and explicit type application,
  data/class/impl and module/import/export declarations, lambdas,
  conditionals, cases, guards, patterns, fixed and source-local declared
  operators, precedence, associativity, values, sections, bindings, and
  operator signatures. Six fixed families assign all 365 parser fixtures
  exactly once and match the active Haskell stage-0 parser through both façades
  twice. `CoreTypes.jz` defines the complete hosted canonical-core comparison
  schema, and `CoreLower.jz` now hosts expression lowering through patterns,
  cases, conditionals, and lambdas.

Compiler modules may import standard-library modules. Standard-library modules
must not import compiler implementation modules; `repository-audit-spec`
enforces that dependency direction from parsed module imports. Production-shaped
correctness and benchmark programs live in the shared `programs/` corpus,
outside this shipped-source root. Small, focused fixtures remain under `test/`.

Ordinary multi-argument Jazz functions use compact lambdas such as
`\(left, right) -> left == right`. The compiler preserves currying and partial
application by lowering that surface form to nested unary core lambdas.

### Hosted canonical core

`CoreTypes` represents every core constructor emitted by the active Haskell
stage-0 lowerer, including exact fractional source parts, generated lowering
names, optional-path spans, signatures, declarations, imports, exports, and
module results. The Haskell comparison adapter is test-only and structurally
translates already-lowered values into this schema.

`CoreLower.lowerFoundationExpression :: SurfaceExpr -> Maybe CoreExpr` is an
internal milestone entry point. It currently lowers literals, source and
qualified variables, operator values, lists, tuples, ordinary application,
non-`$` binary expressions, both section forms, and blocks containing ordinary
non-operator bindings or expression statements. It retains that exact child-1
boundary even as later profiles land.

`CoreLower.lowerControlFlowPatternsExpression :: SurfaceExpr -> Maybe CoreExpr`
reuses the same private recursive kernel and additionally lowers every pattern,
guarded case, conditional, nested control-flow, and multi-parameter or
pattern-lambda rule. Pattern parameters use structured generated names with
their original one-based source positions. The whole tree still returns
`Nothing` when any nested form belongs to a later child, including type
application, `$`, signatures, declarations, operator bindings, imports, and
modules. Neither entry point is a supported public compiler API, and neither
replaces the production Haskell lowerer.

`canonical-core-comparison-spec` inventories the complete comparison contract.
`jazz-core-expression-foundation-spec` compares the hosted lowerer with stage 0
through direct surface values and through the hosted parser, runs both paths
twice for deterministic complete-value equality, and keeps parser failures
distinct from valid-but-deferred lowering.
`jazz-core-control-flow-patterns-spec` adds 18 direct and 14 parser-composed
positive fixtures plus 12 nested later-child rejection fixtures. Every family
runs twice and compares complete values or exact `Nothing` results.

## Editor support

The dependency-free [`editors/vscode-jazz`](editors/vscode-jazz/README.md)
package registers `.jz` files and supplies TextMate syntax highlighting plus
basic VS Code language configuration. It is syntax-only; language-server and
semantic editor features remain future work.

## Test layout

- `test/JazzNext/TestHarness.hs`: shared assertion helpers and test runner plumbing.
- `test/JazzNext/CLI/`: CLI entrypoint coverage.
- `test/JazzNext/Compiler/Config/`: warning/config parsing coverage.
- `test/JazzNext/Compiler/Diagnostics/`: diagnostic rendering and metadata coverage.
- `test/JazzNext/Compiler/Modules/`: prelude loading, module graph, and resolver coverage.
- `test/JazzNext/Compiler/Parser/`: parser, lowering, and operator-surface coverage.
- `test/JazzNext/Compiler/Bootstrap/`: canonical Haskell/Jazz comparison
  adapters plus hosted lexer/parser/core component coverage; the complete core
  schema and expression-foundation direct/composed parity; exact 52-case
  expression, 101-case declarations, 75-case control-flow/pattern, 55-case
  operator, 26-case mixed operator/control-flow, and 56-case corpus-closure
  families; complete repeated 365-case parity; and deterministic
  expression/declarations/control-flow/operator scale profiles.
- `test/JazzNext/Compiler/Semantics/`: analyzer, type, runtime, and builtin semantics coverage.
- `programs/`: shared multi-module correctness and benchmark corpus.

## Run a first program

Create `first.jz`:

```jazz
answer = 40 + 2.
answer.
```

Compile it:

```bash
cabal run --project-dir=jazz-next jazz-next -- first.jz
```

Successful compile output is quiet. Run it:

```bash
cabal run --project-dir=jazz-next jazz-next -- --run first.jz
```

Expected output:

```text
42
```

Run source from stdin explicitly:

```bash
printf '40 + 2.' | cabal run --project-dir=jazz-next jazz-next -- --run -
```

Show CLI help:

```bash
cabal run --project-dir=jazz-next jazz-next -- --help
```

The help path prints usage to stdout and does not read stdin, source files,
warning config files, or Prelude files.

## Build and test

```bash
# from repository root:
nix --extra-experimental-features 'nix-command flakes' develop
cabal build --project-dir=jazz-next all
cabal test --project-dir=jazz-next all --test-show-details=failures
cabal test --project-dir=jazz-next repository-audit-spec --test-show-details=failures
```

`cabal test --project-dir=jazz-next all` is the routine matrix. It includes
`jazz-parser-scale-spec`, which runs all four 65-statement hosted-parser smoke
profiles twice and requires deterministic semantic observations. The four
preserved 513-statement profiles are a niche manual diagnostic and are excluded
from the default matrix. When a maintainer explicitly requests full-scale
diagnosis, enable and select all four gated targets:

```bash
cabal test --project-dir=jazz-next -ffull-parser-scale \
  jazz-parser-scale-full-expression-spec \
  jazz-parser-scale-full-declarations-spec \
  jazz-parser-scale-full-control-flow-spec \
  jazz-parser-scale-full-operator-spec \
  --test-show-details=failures
```

Do not add this exhaustive command to routine PR, release, or scheduled
verification. Use it only to reproduce a suspected scale regression, validate
a changed full-scale generator or ceiling, or satisfy an explicit maintainer
request.

Cabal discovers every registered default suite. Use a test component name,
such as `repository-audit-spec`, for a focused run. The repository audit owns
the Jazz source-format and dependency-layering contracts, editor-package
metadata, documentation entry points, and the private-package policy. Git's
actual ignore behavior for generated performance artifacts is checked at the
repository gate.
