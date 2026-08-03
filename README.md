<p align="center">
  <img src="./jazz_logo.png" alt="Jazz logo" width="132" />
</p>

# Jazz

A statically typed functional language with practical syntax

> **Experimental / pre-1.0:** Jazz is under active development. The language,
> standard library, diagnostics, and command-line interface may change before a
> stable release.

Jazz combines type inference, algebraic data types, pattern matching, immutable
bindings, first-class functions, and modules in a compact, expression-oriented
language. The current compiler checks and runs Jazz programs through an
interpreter.

## A first Jazz program

This checked example defines factorial with typed pattern-lambda clauses and
evaluates it for `6`.

<!-- jazz-example: executable path=examples/functions/factorial.jz -->

```jazz
factorial :: Int -> Int.
factorial =
  \|(0) -> 1
   |(n) -> n * factorial (n - 1).
factorial 6.
```

Expected output:

```text
720
```

## Quick start

Jazz currently builds from source. From the repository root, enter the
reproducible Nix development environment, build the toolchain, and run the
factorial example:

```bash
nix develop
cabal build all
cabal run jazz -- --run examples/functions/factorial.jz
```

The final command compiles, checks, and evaluates the program. The CLI also
supports module entry points, diagnostic rendering, warning controls, and
compile-only checks.

## Available today

- Static types with local inference, explicit signatures, generic named types,
  tuples, lists, and width-specific numeric types.
- Immutable bindings, curried functions, pattern-lambda clauses, blocks, and
  precedence-aware operators.
- Algebraic data types with constructor, literal, variable, list, tuple,
  wildcard, as-pattern, or-pattern, case, and guard support.
- Modules with imports, explicit exports, cycle diagnostics, visibility checks,
  and deterministic graph loading.
- An interpreter-backed CLI with stable runtime rendering and structured
  diagnostics.
- A bundled Prelude plus collection, optional/result, text, character, and host
  I/O library modules.
- Checked teaching examples and a production-shaped correctness and performance
  corpus.

## In development

- Complete capability method dispatch and extend the current direct-call purity
  checks to higher-order and cross-module analysis.
- A fully Jazz-authored canonical compiler pipeline.
- Complete typed-core production and backend-neutral lowering.
- Native code generation, linking, and a production runtime.
- Stable releases, package distribution, editor tooling, and a broader
  ecosystem.

Planned work is tracked separately from implemented behavior and is not
presented as runnable language syntax.

## Documentation

- [Getting started](docs/getting-started/overview.md) — install Jazz and run your
  first program.
- [Language guide](docs/language/overview.md) — learn source forms, functions,
  types, patterns, modules, and effects.
- [Standard library](docs/standard-library/overview.md) — browse the Prelude and
  explicit-import modules.
- [Language reference](docs/reference/expression-grammar.md) — check exact
  expression grammar and accepted forms.
- [Compiler](docs/compiler/architecture.md) — understand the implementation and
  compilation pipeline.
- [Status](docs/project/status.md) — see what is implemented, partial, and
  planned.
- [Roadmap](docs/project/roadmap.md) — follow the major development horizons.
- [Contribution guide](docs/project/contributing.md) — build, test, document, and
  propose changes.
- [Issue tracker](https://github.com/un3qual/jazz/issues) — report defects and
  discuss focused improvements.
- [Website (publishing with Workstream 3)](https://un3qual.github.io/jazz/) — the
  documentation site will publish with the website workstream.

## Contributing

Contributions are welcome. Start with the
[contribution guide](docs/project/contributing.md), keep behavior and tests in
the same change, and use the [issue tracker](https://github.com/un3qual/jazz/issues)
to coordinate substantial work.

## License

Jazz is free software licensed under [GPL-3.0-only](LICENSE).
