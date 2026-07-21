# Jazz-Next Bootstrap Parser Operators and Full Parity Design

## Status

Discussion-approved and written-reviewed on `2026-07-20`. This document is the
design checkpoint for `JN-BOOTSTRAP-JAZZ-PARSER-OPERATORS-FULL-PARITY-001`,
the fifth and final ordered grammar child of the accepted
[bootstrap Jazz parser design](2026-07-12-jazz-next-bootstrap-jazz-parser-design.md).

The contract/kernel, expression-foundation, types/declarations/modules, and
control-flow/patterns children are complete and archived. This child is
eligible for queue promotion. Canonical core, backend-neutral lowered IR,
LLVM lowering, object/link production, and the native runtime remain separate
later milestones.

## Goal

Complete the Jazz-authored hosted parser by adding the frozen built-in operator
table, source-unit-local user-declared operators, precedence, associativity,
operator values, sections, operator bindings and signatures, and exact
source-order visibility. Match the active Haskell stage-0 parser over every
fixture in the fixed 365-case corpus while preserving the landed surface AST,
structured failure schema, parser kernel, lexer/parser phase distinction, and
token/source façades.

The child is one parser milestone. It does not add operator runtime semantics,
cross-module operator APIs, canonical core, lowering, or backend work.

## Scope

The child adds:

- one compiler-local `ParserOperator` owner for operator metadata and symbol
  policy;
- the frozen built-in precedence and associativity table;
- fixed-tier and numeric-precedence user declarations with optional `left`,
  `right`, or `nonassoc` associativity;
- immutable source-order operator context at top level and within module
  bodies;
- infix precedence climbing integrated with the landed stop-aware expression
  grammar;
- bare operator values, left sections, and right sections;
- operator-specific bindings and signatures under their existing surface
  statement representation;
- exact declaration, duplicate, scope, reserved-symbol, range, undeclared-use,
  adjacency, and non-associative-chain failures;
- fixed `Operators`, `MixedOperatorControlFlow`, and `CorpusClosure` fixture
  families;
- exclusive assignment of every fixed fixture to exactly one family;
- repeated complete 365-case token/source parity; and
- an additive deterministic 513-statement operator/control-flow scale profile.

The child does not add:

- a new surface, failure, result, or parser-kernel constructor;
- operator imports, exports, re-exports, or cross-module visibility;
- new built-in operators or runtime overload dispatch;
- operator execution, typing, lowering, or analyzer changes;
- non-adjacent operator signatures or any change to signature coherence;
- recovery, partial ASTs, or diagnostic normalization;
- parser-specific host intrinsics or Haskell callbacks;
- a public parser-combinator or operator library;
- canonical core, bytecode, VM, lowered IR, LLVM, object, link, or native
  runtime work; or
- changes under `jazz-hs/` or `jazz2/`.

## Approaches Considered

### Pre-scan declarations before parsing the source unit

A pre-scan would make expression parsing simpler because every declared
operator would be known up front. It would also make a later declaration
visible to earlier expressions, incorrectly permit forward use, complicate
duplicate failure positions, and blur module/source-unit isolation. This
approach conflicts with the accepted immutable source-order context contract.

### Keep operator rules inline in expression and declaration modules

This minimizes the file count, but it duplicates built-in lookup, tier
mapping, symbol validation, and associativity rules. Expression and declaration
grammar could then drift while still compiling. It also leaves no focused
owner for the central table required by the canonical operator specification.

### Isolate metadata and keep recursive grammar with its current owners

This is the chosen approach. `ParserOperator` owns values and policy that do
not recurse into syntax. `ParserDeclaration` owns declarations, bindings,
signatures, and context transitions. `ParserExpression` owns precedence
climbing, operator values, sections, and integration with recursive control
flow. `ParserProgram` threads the explicit immutable context between
statements. This mirrors stage 0 without copying every Haskell helper or
creating a module cycle.

## Module Ownership

### `ParserOperator.jz`

`ParserOperator` owns:

- `OperatorAssociativity` with left, right, and non-associative cases;
- an abstract `OperatorInfo` containing symbol, numeric precedence, and
  associativity;
- the frozen built-in table in this exact order: `*`, `/`, `+`, `-`, `|`,
  `==`, `!=`, `<`, `<=`, `>=`, `>`, `$`;
- declared tier mapping `1..5` onto precedence `5..1`, with tier 5 inheriting
  right associativity and the others inheriting left associativity;
- numeric precedence validation for the inclusive range `1..99`, defaulting
  to left associativity;
- optional associativity replacement;
- declared-before-built-in lookup;
- built-in and reserved symbol checks; and
- the Stage 2 symbol alphabet `!%&*+-/<>?^|~`.

The exact reserved symbol set is `->`, `=>`, `//`, `/*`, `*/`, and `--`, in
addition to the frozen built-ins. `ParserOperator` does not parse tokens or
expressions and does not know about statements or context.

### `ParserContext.jz`

`ParserContext` gains an ordered list of declared `OperatorInfo` values.
Aliases and statement scope retain their landed representation and behavior.

Context transitions are exact:

- the initial top-level context has no declared user operators;
- a successful declaration prepends one new operator for later statements in
  the same source unit;
- a module body starts with empty aliases and empty declared operators;
- a nested expression block inherits aliases and declared operators but changes
  only the statement context;
- imports never add operators; and
- no declaration mutates an earlier context value.

This keeps lookup deterministic, duplicate detection local, and forward uses
invalid without a pre-scan or mutable table.

### `ParserDeclaration.jz`

`ParserDeclaration` owns operator declaration recognition and validation. A
successful declaration emits no `SurfaceStatement`; it returns the updated
context that is visible to the following statement. A failed declaration
retains the exact structured reason and span from stage 0.

Declarations are accepted only at file scope or directly in a module body.
They require a valid non-built-in, non-reserved symbol, `tier 1..5` or
`precedence 1..99`, at most one valid associativity word, and the terminating
dot. Duplicate declarations in the same source unit are rejected.

Parenthesized operator signatures and bindings are ordinary surface signature
and let statements using the existing hidden operator binding name convention
already represented by the canonical schema. Both require an earlier
declaration in the same source unit, reject built-ins, and are forbidden in
nested expression blocks, classes, and impls. The parser does not implement
signature-to-binding coherence; that remains later semantic ownership.

### `ParserProgram.jz`

Program sequencing consumes statement results as `([SurfaceStatement],
ParserContext)`. Ordinary statements preserve the context, imports update only
aliases, operator declarations update only operators and return an empty
statement list, and module declarations retain their existing placement and
terminal-source behavior.

The program owner remains responsible for source-order threading. It does not
inspect precedence or operator symbols.

### `ParserExpression.jz`

`ParserExpression` retains complete recursive expression ownership. Its public
entry points remain `parseFoundationalExpression` and
`parseFoundationalExpressionWithContext`; they expand to the complete hosted
expression grammar without exposing a precedence or stop parameter.

The private recursive seam becomes a stop-aware precedence climber:

1. parse one primary and its application/type-application tail;
2. inspect the next operator without consuming a caller-owned stop token;
3. resolve metadata from the current immutable context and frozen built-ins;
4. compare its precedence with the current floor;
5. recurse with `precedence + 1` for left/non-associative operators or the same
   precedence for right-associative operators; and
6. build the existing `BinaryExpression` constructor.

A same-precedence continuation involving a non-associative operator fails with
the existing `NonAssociativeOperatorChain` reason. An unresolved symbol fails
with the existing `UndeclaredOperator ... ExpressionOperatorUse` reason.

The climber is used in ordinary expressions, lambda bodies, conditional
conditions and branches, case scrutinees, case guards, case bodies, lists,
tuples, and nested blocks. Existing delimiter predicates continue to own
`then`, `else`, arm arrows, right braces, next-arm pipes, commas, closing
brackets, closing parentheses, and statement dots. Operator parsing must not
consume those boundaries.

Parenthesized forms are exact:

- `(<op>)` produces `OperatorValueExpression`;
- `(expr <op>)` produces `LeftSectionExpression`;
- `(<op> expr)` produces `RightSectionExpression`; and
- `(expr)` remains grouping.

All three operator forms require the symbol to be visible. Application retains
higher binding power than every infix operator. Section recognition is based
on token shape and existing spans; it does not introduce synthetic lambdas or
new AST nodes.

The pipe token remains context-sensitive. At a case-arm boundary it belongs to
the case grammar; within a guard or body it remains the frozen tier-3 operator
when stage 0 can complete an expression before the following arm boundary.
This decision stays inside the existing stop-aware control-flow seam.

### Fixed owners

`ParserTypes.jz`, `ParserCore.jz`, `ParserToken.jz`, `ParserPattern.jz`, and
`Parser.jz` remain unchanged. Every required AST and failure constructor is
already present. A mismatch that cannot be represented by the fixed schema is
a foundational defect and is not authorization to revise those files here.

## Source-Order and Isolation Invariants

- Built-ins are visible in every expression without context entries.
- A user declaration is visible only after its terminating dot.
- Forward infix use, operator values, sections, signatures, and bindings fail.
- Duplicate declarations fail even when their fixity is identical.
- Module bodies receive an empty operator context and may declare their own
  operators for later statements in that module.
- A module-local declaration does not escape the module.
- Nested expression blocks inherit visible operators but cannot declare, bind,
  or sign them.
- Imports and exports do not transport operator metadata.
- Parsing a second source unit starts from the empty initial context.

## Structured Failures

The child uses only the existing failure schema:

- `BuiltinOperatorCannotBeRedeclared`, `BuiltinOperatorCannotBeBound`, and
  `BuiltinOperatorCannotBeSigned`;
- `ReservedOperatorSymbol`, `DuplicateOperatorDeclaration`, and
  `InvalidOperatorSymbol`;
- `OperatorTierOutOfRange` and `OperatorPrecedenceOutOfRange`;
- `DeclarationOutsideAllowedScope` for declaration, binding, and signature;
- `UndeclaredOperator` with expression, binding, or signature use;
- `NonAssociativeOperatorChain`;
- exact `ExpectedSyntax` payloads for missing symbols, fixity words, numeric
  values, associativity words, delimiters, operands, and terminators; and
- existing internal invariant reasons only for impossible token shapes.

Failures remain fail-fast, farthest-offset, declaration-order deterministic,
and phase-correct. Lexically invalid operator or literal fixtures remain source
lexical failures rather than parser failures.

## Fixed Fixture Partition

The existing families remain fixed and unchanged:

- `ExpressionFoundation`: 52 fixtures;
- `TypesDeclarationsModules`: 101 fixtures; and
- `ControlFlowPatterns`: 75 fixtures.

The remaining 137 fixtures are assigned as follows.

### `Operators` (55)

```text
lexer-operator-runs
parser-corpus-0025 parser-corpus-0026
parser-corpus-0043 parser-corpus-0044
parser-corpus-0073 parser-corpus-0075
parser-corpus-0079 parser-corpus-0080 parser-corpus-0081
parser-corpus-0082 parser-corpus-0083 parser-corpus-0084 parser-corpus-0085
parser-corpus-0099
parser-corpus-0160 parser-corpus-0161 parser-corpus-0162
parser-corpus-0163 parser-corpus-0164 parser-corpus-0165 parser-corpus-0166
parser-corpus-0169 parser-corpus-0170 parser-corpus-0171 parser-corpus-0172
parser-corpus-0173 parser-corpus-0175 parser-corpus-0176 parser-corpus-0177
parser-corpus-0178 parser-corpus-0179 parser-corpus-0180 parser-corpus-0181
parser-corpus-0183 parser-corpus-0186 parser-corpus-0187 parser-corpus-0188
parser-corpus-0189 parser-corpus-0196
parser-corpus-0223 parser-corpus-0224 parser-corpus-0225 parser-corpus-0226
parser-corpus-0227 parser-corpus-0228 parser-corpus-0229 parser-corpus-0230
parser-corpus-0231 parser-corpus-0232 parser-corpus-0239 parser-corpus-0243
parser-corpus-0299 parser-corpus-0300 parser-corpus-0307
```

### `MixedOperatorControlFlow` (26)

```text
parser-corpus-0021 parser-corpus-0022 parser-corpus-0027
parser-corpus-0089 parser-corpus-0103 parser-corpus-0149
parser-corpus-0167 parser-corpus-0168 parser-corpus-0174
parser-corpus-0184 parser-corpus-0185 parser-corpus-0198
parser-corpus-0244
parser-corpus-0250 parser-corpus-0251 parser-corpus-0252
parser-corpus-0253 parser-corpus-0254 parser-corpus-0255
parser-corpus-0256 parser-corpus-0257
parser-corpus-0271 parser-corpus-0278 parser-corpus-0281
parser-corpus-0291 parser-corpus-0305
```

### `CorpusClosure` (56)

```text
lexer-arbitrary-precision-integer lexer-all-token-constructors
lexer-comments-spaces-and-tabs lexer-lf-spans
lexer-empty-character lexer-multi-scalar-character
lexer-unterminated-character lexer-unterminated-text lexer-raw-newline
lexer-invalid-escape lexer-unterminated-unicode-escape
lexer-empty-unicode-escape lexer-nonhex-unicode-escape
lexer-overlong-unicode-escape lexer-nonscalar-unicode-escape
parser-corpus-0002 parser-corpus-0003 parser-corpus-0004
parser-corpus-0005 parser-corpus-0006 parser-corpus-0007
parser-corpus-0008 parser-corpus-0009 parser-corpus-0010
parser-corpus-0011 parser-corpus-0012 parser-corpus-0013
parser-corpus-0014 parser-corpus-0015 parser-corpus-0016
parser-corpus-0017 parser-corpus-0018 parser-corpus-0019
parser-corpus-0020 parser-corpus-0023 parser-corpus-0029
parser-corpus-0030 parser-corpus-0031 parser-corpus-0033
parser-corpus-0035 parser-corpus-0037 parser-corpus-0040
parser-corpus-0130 parser-corpus-0132 parser-corpus-0190
parser-corpus-0202 parser-corpus-0203 parser-corpus-0209
parser-corpus-0213 parser-corpus-0217 parser-corpus-0218
parser-corpus-0219 parser-corpus-0238 parser-corpus-0242
parser-corpus-0311 parser-corpus-0312
```

`CorpusClosure` is not a new grammar slice. It is the explicit accountability
family for remaining lexical, literal, statement-boundary, reserved-syntax,
and already-landed integration fixtures that were intentionally outside the
three earlier child families.

## Manifest Invariants

The manifest validator must reject, in deterministic corpus/family order:

- duplicate fixture names in the corpus;
- duplicate names within one family;
- family members missing from the corpus;
- a fixture assigned to more than one family; and
- a corpus fixture assigned to no family.

The six family sizes sum to 365 and their union equals the complete corpus.
Membership remains by explicit stable name, never by positional prefixes or a
runtime scan of Haskell source files.

## Differential Parity

For each focused family and for the complete corpus, the Haskell harness:

1. validates the manifest;
2. loads fixtures in stable order;
3. compares the complete stage-0 token result with the hosted token result
   twice for tokenizable fixtures;
4. compares the complete stage-0 source result with the hosted source result
   twice for every fixture;
5. requires byte-identical repeated hosted output; and
6. retains lexical failure, parser failure, and success as distinct source
   outcomes.

Acceptance-only assertions, AST-constructor subsets, normalized diagnostics,
or fixture exclusions are insufficient.

## Scale Evidence

Add one generated operator/control-flow profile. It contains one leading user
operator declaration, 512 ordinary bindings, and one terminal expression. The
declaration produces no surface statement, so the parsed block contains
exactly 513 statements.

The 512 bindings rotate through:

- mixed built-in precedence and left associativity;
- right-associative `$` chains and operator values/sections;
- user-operator precedence and sections after declaration; and
- operator expressions inside lambdas, conditionals, case guards, case bodies,
  lists, tuples, and nested blocks.

Run the profile twice through the ordinary Jazz module graph and require:

- output `513` both times;
- identical runtime observation statistics;
- zero host operations;
- successful termination; and
- measured evaluator-transition, application, list-cell, and continuation
  depth values under recorded ceilings.

Initial ceilings may be conservative for the first measured red/green run but
must be tightened above the stable observation before closeout. The three
landed scale profiles retain their existing sources, outputs, and ceilings.

## Verification and Closeout

Completion requires:

- focused operator metadata/context, declaration, expression, and mixed
  control-flow behavior tests;
- exact parity for all six families;
- repeated complete 365-case token/source parity;
- all four scale profiles with deterministic statistics and zero host
  operations;
- a warning-clean development build;
- every registered Cabal suite;
- `cabal check`;
- queue and docs validators;
- `git diff --check`; and
- direct confirmation that `jazz-hs/`, `jazz2/`, `ParserTypes.jz`,
  `ParserCore.jz`, `ParserToken.jz`, `ParserPattern.jz`, and `Parser.jz` did not
  change.

On closeout, archive the child and remove it from `Ready Now` in the same pass.
Update the parent parser and interpreter-profile designs to record full hosted
parser parity. Do not promote canonical-core or backend implementation without
a separately reviewed child contract and plan.

## Acceptance Criteria

- The approved module ownership and immutable source-order context are present.
- Built-in and declared precedence, associativity, sections, declarations,
  bindings, signatures, adjacency, and scope failures match stage 0 exactly.
- Operator parsing composes with every landed control-flow delimiter.
- The existing surface/failure/kernel/façade contracts remain unchanged.
- All 365 fixtures are assigned exactly once across the six fixed families.
- Focused and complete token/source parity is exact and deterministic.
- The operator scale profile passes twice with recorded bounded observations
  and zero host operations; all prior profiles remain green.
- No runtime, core, backend, public-parser, host-callback, or legacy-reference
  work enters the change.
