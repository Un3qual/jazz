# Jazz Next Compiler Review Remediation Design

**Date:** 2026-07-09  
**Status:** Approved  
**Scope:** `jazz-next/`

## Context

A detailed review of the active Jazz compiler found five correctness and
packaging gaps:

1. Multiple bare, unqualified imports can expose the same symbol and silently
   select a definition according to module replay order.
2. Semantic diagnostics produced from a module graph report line and column but
   not the source file that owns the span.
3. Lexically equivalent module roots, such as `src` and `src/.`, are treated as
   distinct and can produce a false ambiguous-module diagnostic.
4. The Cabal package exposes only a library, so `cabal test` runs no tests and
   `cabal run jazz-next` has no executable target.
5. The exposed surface AST permits a lambda with no parameters even though the
   parser rejects that syntax, and lowering that impossible state calls
   `error`.

Jazz is not released, so breaking changes to its Haskell API are acceptable.
The remediation must preserve Jazz syntax and intended language semantics,
except where the existing behavior is demonstrably broken.

## Goals

- Make unqualified import resolution reject ambiguity independent of module
  discovery or replay order.
- Preserve source-file provenance through semantic analysis and diagnostic
  rendering.
- Treat lexically equivalent module roots as one root.
- Make the supported Cabal commands build the CLI and run the complete existing
  test suite.
- Make the surface AST encode the parser's non-empty lambda-parameter
  invariant, leaving lowering total for that case.
- Add focused regression coverage and update the public compiler documentation.

## Non-goals

- Redesigning the module graph or the full located AST.
- Changing import syntax, name lookup precedence, or the set of declarations
  exported by a module.
- Canonicalizing symlinks inside the pure module resolver.
- Changing standalone parser diagnostics or Jazz runtime behavior.
- Modifying the legacy `jazz-hs/` or `jazz2/` implementations.

## Design

### 1. Deterministic unqualified import collision checking

The resolver will validate the visible symbol set of every non-aliased import:

- A bare import contributes every unqualified name admitted by the existing
  export rules for its target module, including class names where the current
  resolver treats them as importable symbols.
- A symbol-list import contributes the requested importable names after the
  existing unknown-symbol validation.
- An aliased import contributes no unqualified names.

Validation will maintain a map from each visible unqualified name to the import
that first introduced it. If another non-aliased import contributes the same
name, resolution will emit E4008 with both import spans and module contexts.
This generalizes the existing symbol-list collision rule instead of creating a
second ambiguity mechanism.

Local declarations retain their current lookup behavior. This change addresses
only the broken case where two imports make the same unqualified name visible;
it does not introduce a new precedence rule.

### 2. Source provenance in semantic spans

`SourceSpan` will gain a source-bearing form while retaining the existing
line-and-column form for standalone parsing. Conceptually:

```haskell
data SourceSpan
  = SourceSpan Int Int
  | SourceSpanIn FilePath Int Int
```

Module loading will attach the resolved source path to every span in the
lowered module before modules are combined for replay and analysis. Because
semantic diagnostics already carry primary and related `SourceSpan` values,
the provenance will flow through existing error construction without adding a
parallel diagnostic ownership mechanism.

Span accessors and rendering will support both constructors. A source-bearing
span renders as `path:line:column`; an unowned span keeps the current
`line:column` output. Related spans preserve their own source path, including
cross-module diagnostics.

### 3. Lexical module-root normalization

Module roots will be normalized before deduplication and before candidate paths
are built. The resolver's existing abstract file lookup remains intact, so the
normalization is lexical rather than filesystem-dependent. This makes `src`,
`src/.`, repeated separators, and equivalent parent-directory components share
one lookup root without adding IO or changing testability.

Symlink equivalence is deliberately excluded because determining physical file
identity would require filesystem effects that do not belong in the pure
resolver interface.

### 4. Cabal executable and test registration

The package will add:

- An executable named `jazz-next` whose small `app/Main.hs` delegates to the
  existing CLI module.
- Cabal test-suite stanzas for every current `*Spec.hs` entry point, sharing a
  common test configuration where possible.

The existing spec programs remain the behavioral source of truth. Registering
them directly avoids inventing a second test runner or changing individual test
semantics. The existing warning-configuration shell test remains usable, while
`cabal test all` becomes the standard complete-suite command and
`cabal run jazz-next -- ...` becomes the standard packaged CLI command.

### 5. Total surface-lambda lowering

The surface AST will store lambda parameters as
`NonEmpty SurfaceLambdaParameter`. The parser will construct `NonEmpty` only
after recognizing the grammar's required first parameter. Lowering can then use
a total non-empty fold and remove its `error` branch.

This is an intentional breaking change to the exposed Haskell AST API. It does
not change Jazz syntax: zero-parameter surface lambdas remain rejected exactly
as they are today.

## Regression Strategy

Implementation will follow focused red-green-refactor cycles:

1. Add a module-resolver regression proving two bare imports with a shared
   export produce E4008 regardless of declaration order.
2. Add a module-graph semantic-error regression proving rendered primary and
   related spans include their source files.
3. Add a resolver regression proving `src` and `src/.` do not create E4002.
4. Preserve the observed failing Cabal command behavior as command-level red
   evidence, then verify the executable and complete registered suite after the
   package change.
5. Update surface-AST tests and add lowering coverage around the non-empty
   invariant.

After each focused change, run the narrow affected specs. Before completion,
run the full Cabal suite, the existing warning-configuration script, a CLI
smoke test, and package/build checks appropriate to the repository.

## Documentation Impact

- Specify that collisions between any two non-aliased imports are E4008,
  including bare imports.
- Document source-qualified semantic diagnostic locations.
- Document lexical module-root normalization and its symlink boundary.
- Update README commands to use the packaged executable and Cabal test suite.

## Delivery

The work will be committed in coherent checkpoints: approved design,
implementation plan, focused compiler fixes with regression tests, and final
documentation/verification cleanup. No files under `jazz-hs/` or `jazz2/` will
be modified.
