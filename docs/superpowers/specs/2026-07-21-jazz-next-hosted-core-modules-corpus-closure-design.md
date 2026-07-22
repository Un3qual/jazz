# Jazz-Next Hosted Core Modules and Corpus Closure Design

## Status

Approved and completed on `2026-07-21` as the fourth and final child of the
hosted canonical-core milestone. The implementation preserves all three earlier
profile boundaries while completing the private source-to-core path.

Closure evidence: all 17 direct module fixtures and 13 composed sources match
complete stage-0 results twice. The explicit ordered manifest contains all and
only the 196 accepted fixtures from the fixed 365-case parser corpus, and every
facade result matches stage 0 twice without lexical, parser, or module-lowering
failure. The warning-clean build, focused regressions, routine non-exhaustive
Cabal matrix, bounded parser scale, and package check pass.

## Goal

Complete the Jazz-authored canonical-core boundary by adding total expression
lowering, module/import extraction, exact module metadata, expected-path
validation, structured module-lowering failures, complete source-path span
qualification, and one composed source-to-core facade.

The child must match the active stage-0
`JazzNext.Compiler.Parser.Lower` boundary for every successfully parsed fixture
in the fixed 365-case parser corpus. It closes hosted canonical-core parity; it
does not replace the production Haskell lowerer or begin backend work.

## Chosen Approach

Extend the existing ordered profile-driven lowerer with one complete profile,
one module collector, and one thin facade.

The recursive lowering transformation remains shared. The complete profile
admits module and import statements, while the three existing wrappers retain
their earlier observable boundaries. Module lowering separately inspects only
the top-level block to collect metadata and remove top-level module/import
statements from the executable body. It then uses the same complete expression
transformation for the retained body, including any nested imports accepted by
the parser.

Two alternatives were rejected:

1. A standalone module lowerer would duplicate expression and statement
   traversal, making later parity fixes likely to drift.
2. A test-only source composition would prove differential behavior without
   delivering the real Jazz-authored source-to-core boundary required by the
   bootstrap profile.

The shared kernel may be refactored so it produces both a canonical value and
the minimum lowering profile required by that surface tree. Existing wrappers
then return `Nothing` when that requirement exceeds their boundary, while the
complete wrapper returns the already-produced value directly. This preserves
all earlier deferral behavior without an impossible `Nothing` branch in module
lowering and without traversing or transforming the tree twice.

## Ownership and API

The implementation remains under the active compiler path:

- `jazz-next/jazz/compiler/CoreTypes.jz` owns the composed source-result type in
  addition to its existing canonical module and module-failure schema.
- `jazz-next/jazz/compiler/CoreLower.jz` owns the complete lowering profile,
  module metadata extraction, validation, selector conversion, and recursive
  source-path qualification.
- `jazz-next/jazz/compiler/Core.jz` owns the thin hosted
  lexer-to-parser-to-module-lowerer facade and no parsing or lowering rules.
- `jazz-next/test/JazzNext/Compiler/Bootstrap/CanonicalCoreComparison.hs`
  remains the structural adapter for already-lowered stage-0 values.
- `jazz-next/test/JazzNext/Compiler/Bootstrap/JazzCoreParity.hs` owns shared
  runners for direct module lowering and composed source lowering.
- `jazz-next/test/JazzNext/Compiler/Bootstrap/JazzCoreModulesCorpusClosureSpec.hs`
  owns the fixed module fixtures, facade fixtures, successful-corpus manifest,
  and bounded deterministic smoke case.
- `jazz-next/jazz-next.cabal` registers the focused suite and the new checked-in
  Jazz compiler module.

The hosted entry points are private bootstrap boundaries:

- complete expression lowering accepts any `SurfaceExpr` and returns one
  `CoreExpr`;
- module lowering accepts a canonical source path, an expected module path,
  and one parsed `SurfaceExpr`, then returns `CoreModuleLoweringResult`; and
- source lowering accepts a canonical source path, an expected module path,
  and source text, then returns a composed result that keeps lexical, parser,
  and module-lowering outcomes distinct.

The composed result retains the existing path and structured error values. It
does not flatten failures into text or introduce a new diagnostic renderer.

## Complete Expression Lowering

The fourth profile includes every prior expression, pattern, signature,
declaration, and operator transformation. It additionally lowers:

- `ModuleStatement` to `CoreModuleStatement`, discarding export metadata only
  in the expression-level representation exactly as stage 0 does; and
- `ImportStatement` to `CoreImportStatement`, preserving path, alias, symbols,
  and source order.

This makes expression lowering total over the fixed `SurfaceExpr` schema.
Nested module/import forms in direct fixtures lower structurally. The hosted
parser may reject some such source forms, but direct structural parity remains
defined by the stage-0 expression lowerer rather than by parser reachability.

The foundation, control-flow/patterns, and
signatures/declarations/operators wrappers keep their existing `Maybe`
contracts and exact deferral behavior. Earlier tests must continue to observe
`Nothing` for every form outside their child boundary.

## Module Collection and Validation

Module lowering follows the stage-0 order exactly:

1. If the root is a block, inspect only its top-level statements. A non-block
   root has no declarations or imports.
2. Collect every top-level module declaration in source order, retaining its
   span, path, and optional export list.
3. Collect every top-level import in source order, retaining its span, path,
   alias, and optional symbol list.
4. Remove only top-level module and import statements from the executable body.
   Nested imports remain ordinary core statements.
5. Validate declarations before constructing the successful module result.
6. Lower the retained body with the complete expression transformation and
   qualify all retained spans with the supplied canonical source path.

Declaration validation has only three outcomes:

- no declaration produces no declared path and no declared exports;
- one declaration whose path equals the expected path produces its declared
  path and preserves `Nothing` versus `Just []` for omitted versus explicitly
  empty export lists; or
- a mismatch produces `CoreModulePathMismatchFailure`, while two or more
  declarations produce `CoreMultipleModuleDeclarationsFailure` in source
  order.

The two failure constructors are the structured `E4006` and `E4005`
counterparts respectively. They contain the canonical source path and the
semantic path/declaration inputs used by stage 0. Presentation strings remain
owned by the production Haskell diagnostic wrapper.

## Module Metadata

Module export conversion preserves every parser-owned distinction:

- bare, `value`, `constructor`, `type`, and `class` named selectors;
- abstract type exports;
- all-constructor exports and the group span;
- selected non-empty constructor lists and every located-name span; and
- selector order.

Import conversion preserves path segments, optional alias, optional symbol
list, symbol order, and import order. Module lowering performs no resolution,
availability checking, re-export expansion, alias interpretation, or
visibility filtering.

## Source-Path Span Qualification

Direct expression lowering continues to produce `CoreSpan Nothing line column`.
Module lowering rewrites every retained core span to
`CoreSpan (Just sourcePath) line column` without changing line or column.

Qualification recursively covers:

- let, signature, data, class, impl, module, import, and expression statements;
- class method signatures and impl methods;
- explicit type applications;
- case guards and bodies, lambda bodies, applications, collections,
  conditionals, operator nodes, and nested blocks;
- module declarations and imports retained as metadata; and
- module export selector, constructor-group, and located-constructor spans.

Patterns, names, literals, and signature payloads do not acquire synthetic
spans because their canonical schema does not contain them. Qualification is a
pure structural pass and does not normalize, reinterpret, or inspect source
text.

## Composed Source Facade

`Core.jz` calls the existing hosted `Parser.parseSource` exactly once. It then:

- forwards lexical failure with its canonical source path and lexical value;
- forwards parser failure with its canonical source path and parser value; or
- passes the successful surface expression and parsed path to module lowering,
  returning either the structured module failure or canonical module.

The facade does not duplicate parser branches, recover from failures, invoke
Haskell callbacks, or render diagnostics. The expected module path remains an
explicit caller input so the same source can exercise both successful and
`E4006` paths deterministically.

## Fixed Fixture Families

Every family executes twice and compares deterministic canonical values.

### Direct module/result family: 17 fixtures

1. `non-block-no-metadata`
2. `block-no-declaration`
3. `module-exports-omitted`
4. `module-exports-empty`
5. `named-export-namespaces`
6. `type-export-abstract`
7. `type-export-all-constructors`
8. `type-export-selected-constructors`
9. `import-plain`
10. `import-alias`
11. `import-symbols`
12. `imports-source-order`
13. `nested-import-preserved`
14. `complete-span-qualification`
15. `path-mismatch`
16. `multiple-declarations-two`
17. `multiple-declarations-three`

Together these fixtures cover a non-block expression, a block with no module
metadata, omitted and explicitly empty exports, every export selector, every
import shape, ordered metadata extraction, top-level removal with nested import
preservation, every span-bearing core owner, and both structured failures.

Assertions distinguish omitted metadata from explicit empty lists and compare
the complete structured result, not selected rendered fragments.

### Composed facade family: 13 sources

1. `module-free`
2. `module-no-exports`
3. `module-empty-exports`
4. `module-named-exports`
5. `module-type-exports`
6. `import-plain`
7. `import-alias`
8. `import-symbols`
9. `nested-import`
10. `mixed-full-surface`
11. `path-mismatch`
12. `lexical-failure`
13. `parser-failure`

This family proves matching declarations, every export/import shape,
composition with declarations, control flow, and operators, and preservation
of all three failure owners. `mixed-full-surface` is also the bounded
representative nested-module smoke case; no large synthetic fixture is added.

### Successful-parser corpus manifest

The fixed parser corpus currently contains 365 fixtures: 196 accepted and 169
rejected. The lowering manifest explicitly names all 196 accepted fixtures in
parser-corpus order. Its audit rejects:

- duplicate manifest names;
- names absent from the parser corpus;
- rejected fixtures included as lowering inputs;
- accepted fixtures omitted from the manifest;
- order drift; and
- a count other than 196 unless the manifest and reviewed contract are updated
  together.

Each manifested fixture is processed through the complete hosted source
facade. Its expected module path is fixed as fixture metadata: it matches the
single declared path when one exists and uses the family default when none
exists. Direct fixtures, rather than adaptive corpus logic, own deliberate
`E4005` and `E4006` failures.

The stage-0 side independently tokenizes, parses, lowers with
`lowerSurfaceModuleDetailed`, and structurally adapts the result. The hosted
side lexes, parses, and lowers through `Core.jz`. Both complete result lists
must match exactly on two executions.

## Verification

The focused suite must prove:

- complete expression parity for module/import statements;
- exact module and import extraction order;
- exact export metadata, including all namespaces and constructor selectors;
- exact omitted-versus-empty metadata distinctions;
- exact structured `E4005` and `E4006` results;
- recursive source-path qualification for every span-bearing core owner;
- distinct lexical, parser, and lowering outcomes through the facade;
- complete, duplicate-free, ordered coverage of all 196 parser-success
  fixtures; and
- repeated deterministic parity for every direct, composed, and corpus case.

Regression verification includes all three earlier hosted-core suites,
canonical-core comparison, canonical-parser comparison, every hosted-parser
parity family, and repository audit. Closeout also requires a warning-clean
development build, routine Cabal `all`, `cabal check`, queue/docs validators,
and `git diff --check`.

The default matrix must not run the opt-in exhaustive parser scale components.
Only the bounded routine parser-scale suite may run through Cabal `all`. This
child uses the fixed `mixed-full-surface` source as its bounded representative
smoke case and adds no large synthetic performance gate.

Tests assert canonical values and behavior. They do not inspect implementation
source strings or duplicate lowering decisions in the comparison adapter.

## Non-Goals

This child does not:

- replace or call through the production Haskell lowerer;
- change the active parser, surface AST, core AST semantics, or diagnostics;
- add module resolution, import availability checks, re-exports, packages,
  wildcard or hiding imports, or alias-qualified capabilities;
- port name resolution, analysis, warnings, type inference, evidence,
  evaluation, host operations, or intrinsics;
- introduce typed core, backend-neutral lowered IR, bytecode, a VM, LLVM,
  object generation, linking, or a native runtime;
- publish a public parser or lowerer API;
- run exhaustive parser scale suites; or
- modify `jazz-hs/` or `jazz2/`.
