# Checked Jazz examples

These small programs teach one language feature at a time. The `programs/`
tree remains the production-shaped correctness and benchmark corpus.

Build the real command-line executable once:

```bash
cabal build jazz
```

Compile a standalone source file (success is quiet):

```bash
cabal run jazz -- examples/hello.jz
```

Run the standalone examples:

```bash
cabal run jazz -- --run examples/hello.jz
cabal run jazz -- --run examples/functions/factorial.jz
cabal run jazz -- --run examples/patterns/result.jz
```

Compile and run the module graph:

```bash
cabal run jazz -- --entry-module Example::Main --module-root examples/modules/src
cabal run jazz -- --run --entry-module Example::Main --module-root examples/modules/src
```

Run the alias-qualified class example, including a constrained function and an
implementation for a local type:

```bash
cabal run jazz -- --run --entry-module Example::Compare --module-root examples/modules/src
```

Its result is `(True, False, True)`.

The preceding checked outputs are, respectively, `"Hello, Jazz"`, `720`, `41`, and
`"Hello from a Jazz module"`, each followed by a newline. Run the complete
example check with:

```bash
bash scripts/check-examples.sh
```
