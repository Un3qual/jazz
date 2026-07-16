# Jazz-Next Bootstrap Parser Expression Foundation Design

## Status

Implemented and verified on `2026-07-16` after discussion and written review.

This is the design checkpoint for
`JN-BOOTSTRAP-JAZZ-PARSER-EXPRESSION-FOUNDATION-001`, the second ordered child
of the accepted
[bootstrap Jazz parser design](2026-07-12-jazz-next-bootstrap-jazz-parser-design.md).
The parser contract and generic kernel were complete before this child. The
expression foundation is now complete and archived; types/declarations/modules
is the sole next curation target and remains unpromoted.

## Goal

Implement the first useful Jazz-authored grammar slice as ordinary compiler
modules. The hosted parser must parse complete programs containing foundational
expressions, ordinary bindings, expression statements, and nested blocks from
the canonical Jazz token stream. It must also expose a source façade that
composes the Jazz lexer without erasing the distinction between lexical and
parser failures.

For one explicit stable fixture family, the hosted parser must match stage 0
exactly for complete surface ASTs, retained spans, structured failures, and
deterministic rendering.

## Decision Summary

- The grammar is split across token, expression, program, and façade modules.
- The already-landed `ParserCore` remains the only generic parser-combinator
  kernel. This child does not introduce a second cursor or choice model.
- The parser exposes both token and source entry points.
- The expression foundation covers literals, names, application, grouping,
  unit, tuples, lists, blocks, ordinary bindings, expression statements, and
  complete program sequencing.
- A named `ExpressionFoundation` fixture family explicitly selects stable
  fixture names from the existing corpus. Existing opaque fixture names are not
  renamed, and new semantic fixtures are added only for real coverage gaps.
- Exact parity is fail-fast and structured. No recovery, partial AST, or
  string-only comparison is added.
- Large-input evidence uses deterministic runtime observations and stack-safety
  assertions. Physical time remains review evidence rather than a CI threshold.
- The modules remain compiler-private and ordinary Jazz. No parser-specific
  builtin, host callback, mutable token buffer, or public stdlib parser API is
  authorized.

## Approaches Considered

### One grammar module plus a façade

All token matching, expression parsing, statement parsing, and sequencing could
live in one grammar module. This minimizes module plumbing, but later grammar
children would extend the same owner with declarations, patterns, signatures,
and operators. The file would accumulate unrelated decisions and recreate the
large case-block-heavy structure that the parser-combinator foundation was
intended to avoid.

### Expression and program modules with local token helpers

Expression and program parsing could be separated while each keeps its own
token predicates and failure constructors. This is compact for the first
slice, but it duplicates the boundary that every later grammar owner needs and
makes exact failure behavior easier to drift.

### Shared token layer, focused grammar modules, and a small façade

This is the chosen approach. One compiler-local token layer specializes
`ParserCore` for canonical Jazz tokens and structured parser failures. Separate
expression and program modules own their grammar areas. A small façade owns
complete token/source results. The expression module accepts block parsing as a
dependency, avoiding a compiler-module import cycle while keeping the recursive
grammar explicit.

## Architecture and Ownership

### `ParserToken`

`ParserToken` is the only new layer that understands both `CanonicalToken` and
`ParserCore`. It owns token predicates, punctuation and keyword recognition,
span access, adjacency checks, and construction of structured grammar
rejections.

It does not own expression or statement grammar. It does not expose cursor
constructors, reinterpret consumption, or make zero-progress failure
recoverable. Later parser children reuse this layer rather than creating local
token-runner conventions.

### `ParserExpression`

`ParserExpression` owns the foundational expression grammar. It consumes a
block parser dependency rather than importing `ParserProgram`, so blocks may be
primary expressions without creating an import cycle.

This module has no source-text entry point, module/declaration context,
operator table, type grammar, or pattern grammar. Its result is the fixed
`SurfaceExpr` schema from `ParserTypes`.

### `ParserProgram`

`ParserProgram` owns ordinary statements, block termination, top-level
sequencing, and the recursive connection between expression and block parsing.
It is responsible for distinguishing a binding statement from an expression
statement and for producing a complete `BlockExpression` program.

The program owner does not lower the surface tree or retain parser state after
one complete result.

### `Parser`

`Parser` is the compiler façade. It exposes complete token parsing and complete
source parsing, converts the generic kernel reply into the fixed parser result
schema, and preserves lexical and parser failures as distinct source outcomes.
It contains no grammar alternatives.

### Existing foundation modules

`ParserCore` remains generic over token, problem, and value types. Implementation
exposed one module-boundary defect: explicit Jazz imports could not name the
abstract `Parser` type or inspect generic failures without importing internal
constructors indirectly. The narrow foundation correction exports the existing
`Parser` constructor and typed failure offset/problem accessors; it does not
change cursor, consumption, choice, progress, or failure-selection semantics.
`ParserTypes` remains the accepted complete surface/result/failure schema and is
unchanged.

All four new modules live under `jazz-next/jazz/compiler/`. They are not public
stdlib modules, and no file under `jazz-hs/` or `jazz2/` changes.

## Entry Points and Data Flow

The token entry point accepts a normalized canonical source path and canonical
token sequence. It parses exactly one complete program. Success returns the
path and complete surface block; failure returns the path and one structured
parser failure. Remaining tokens are a failure rather than a prefix success.

The source entry point accepts the same logical path plus source text. It calls
the Jazz lexer and then follows one of three paths:

1. lexical success followed by parser success becomes
   `CanonicalSourceSuccess`;
2. lexical failure becomes `CanonicalSourceLexicalFailure` without invoking
   the grammar; or
3. lexical success followed by parser failure becomes
   `CanonicalSourceParserFailure`.

Neither entry point renders diagnostics or ASTs into ad hoc text. The parity
harness renders the ordinary structured values through the existing generic
runtime-value renderer.

## Expression and Statement Contract

The accepted grammar surface is:

- normalized integer literals;
- source-exact fractional literals with supported width suffixes;
- boolean, character, and text literals;
- ordinary names and immediately adjacent qualified names;
- parenthesized expressions;
- unit represented by the fixed empty-tuple surface form;
- fixed-order tuples and lists, including empty values;
- left-associated application of primary expressions;
- empty and populated block expressions;
- ordinary `name = expression.` bindings;
- `expression.` statements; and
- empty or populated top-level programs represented as block expressions.

Literal construction preserves the source-exact numeric contract fixed by
`ParserTypes`. Qualification obeys the current adjacency rule. Lists and tuples
preserve source order. Application does not anticipate operator precedence;
operator tokens remain outside this child.

Binding/expression disambiguation remains deterministic. An identifier does not
commit to a binding until the binding shape is recognized. Once `=` has been
consumed, failure in the right-hand side remains a binding failure and cannot
fall back to an expression statement.

Statements require their current terminators. Blocks require their closing
delimiter. The hosted grammar preserves stage-0 handling for missing
expressions, missing terminators, malformed qualification, malformed
list/tuple delimiters, incomplete bindings, trailing tokens, and end of input
inside a block.

## Failure Semantics

Grammar sites construct the same `ParserFailureReason` variants, encountered
token forms, selected spans, error code, and payload text as stage 0 for every
fixture in the family.

Ordinary unconsumed rejection may participate in a grammar choice. Consumed
failure remains committed. `ZeroProgressProblem` remains a nonrecoverable
kernel invariant across every grammar layer and converts deterministically to
`InternalParserFailure TokenStreamParseFailure`; an outer alternative must not
hide it.

The kernel offset is used for deterministic failure selection. User-visible
location remains the selected canonical source span carried by the structured
problem. Equal-position ordinary failures preserve declaration order as fixed
by `ParserCore`.

This child adds no recovery, synchronization token, partial tree, multiple
failure accumulation, or presentation-oriented diagnostic concatenation.

## Stable Fixture Family

The shared parser fixture manifest gains a named `ExpressionFoundation` family.
The family maps to an explicit ordered list of fixture names. It does not use a
positional prefix, source-text inspection, or a generated guess based on the
expected result.

Existing `parser-corpus-NNNN` entries keep their stable identities. The family
selects suitable existing cases for broad evidence and adds semantically named
fixtures only where no current case isolates a required success or failure.
Every new parser fixture updates the shared corpus and the relevant family in
the same change.

Manifest validation requires:

- globally unique fixture names;
- unique membership within a family;
- every family member to reference an existing fixture;
- stable declared order; and
- an expression-foundation mix containing successful parses, parser failures,
  and a representative lexical failure for the source façade.

Later children add their own named families. Full classification and
full-corpus hosted-parser parity remain the final parser child's responsibility.

## Parity and Regression Testing

A reusable Haskell parity harness runs the ordinary checked-in Jazz compiler
modules through the real module graph. A focused test suite uses the family
manifest to compare stage 0 and the hosted parser.

The suite proves:

- complete surface AST and retained-span equality through the token entry
  point;
- complete source-result equality through the source entry point;
- exact code, optional span, reason constructor, and payload equality for
  parser failures;
- lexical/parser phase separation;
- success coverage for every accepted expression and statement form;
- rejection coverage for each in-scope malformed boundary;
- byte-identical repeated batch evaluation; and
- typed module-boundary access to the intended façade rather than private
  grammar representation.

Static Jazz source embedded in Haskell uses `MultilineStrings`. Dynamic fixture
values are inserted through a shared renderer and multiline template rather
than hand-concatenated complete programs. Explicit escaped strings remain
limited to tests that directly assert whitespace, adjacency, or source spans.

The reusable harness is designed for later named grammar families. Those
children extend its inputs and expectations rather than creating parallel
comparison systems.

## Scale and Performance Evidence

The isolated `jazz-parser-scale-spec` suite includes a generated 512-binding
expression-foundation program plus one terminal expression statement. It parses
successfully as a 513-statement block across two runs with identical output and
runtime statistics, successful termination, and zero host operations.

The observed values are 21,751,223 evaluator transitions, 2,630,524
applications, 110,804 list cells, and maximum continuation depth 1,060. The
reviewed deterministic ceilings are 22,000,000, 2,700,000, 115,000, and 1,100,
respectively. These budgets detect structural work regressions without using
wall-clock duration as a test threshold.

The test does not gate on wall-clock duration, physical allocation, or a fixed
percentage comparison. Same-machine benchmark or profile output may accompany
review as evidence. The existing benchmark and GHC profiling facilities remain
available for stage-0 compiler measurements.

A dedicated hosted-parser benchmark case is deferred until additional grammar
families make the workload representative. Any later representation change
must be justified by benchmark, profiling, or runtime-observation evidence
rather than intuition.

## File Ownership

The implementation plan will fix work in these owners:

- new `jazz-next/jazz/compiler/ParserToken.jz`;
- new `jazz-next/jazz/compiler/ParserExpression.jz`;
- new `jazz-next/jazz/compiler/ParserProgram.jz`;
- new `jazz-next/jazz/compiler/Parser.jz`;
- new reusable bootstrap parser parity support and focused spec under
  `jazz-next/test/JazzNext/Compiler/Bootstrap/`;
- the shared parser fixture manifest;
- `jazz-next/jazz-next.cabal`; and
- the queue, blocker, parser design, feature status, and directly affected
  documentation needed for promotion and closeout.

The implementation plan may name additional focused test-support files when
their ownership is explicit. It must not absorb unrelated parser, compiler,
stdlib, benchmark-corpus, or legacy-reference work.

## Verification and Closeout

The implementation plan must name:

- the focused hosted-parser parity test target;
- the canonical parser, parser-kernel, token-parser, lexer-parity, and
  repository-audit targets that protect its boundaries;
- a development-warning build;
- the full Cabal test suite;
- queue and documentation validators; and
- `git diff --check`.

Physical performance evidence is recorded for review but is not a portable
pass/fail threshold.

Implementation and verification are complete, and the expression-foundation
child is in the done archive. The types/declarations/modules child is the sole
next curation target, but it is not automatically promoted. Control flow and
patterns, then operators and full parity, retain their ordered planning gates.

## Non-Goals

This child does not add signatures, explicit type application, declarations,
modules or imports, lambdas, control flow, patterns, operators, precedence,
associativity, sections, recovery, partial ASTs, multiple errors, lowering,
canonical typed core, bytecode, a VM, backend-neutral lowered IR, LLVM lowering,
object generation, linking, or native-runtime behavior.

It does not replace the active Haskell parser, expose a public parser library,
add a parser-specific primitive or host bridge, expand the substantial program
benchmark corpus, or modify `jazz-hs/` or `jazz2/`.
