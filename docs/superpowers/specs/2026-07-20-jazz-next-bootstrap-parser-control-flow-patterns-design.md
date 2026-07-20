# Jazz-Next Bootstrap Parser Control Flow and Patterns Design

## Status

Discussion-approved on `2026-07-20` and pending written review before queue
promotion. This document is the design checkpoint for
`JN-BOOTSTRAP-JAZZ-PARSER-CONTROL-FLOW-PATTERNS-001`, the fourth ordered child
of the accepted
[bootstrap Jazz parser design](2026-07-12-jazz-next-bootstrap-jazz-parser-design.md).

The contract/kernel, expression-foundation, and types/declarations/modules
children are complete and archived. This child remains outside `Ready Now`
until this design and its implementation plan pass review. Operators and full
parser-corpus parity remain the fifth ordered child.

## Goal

Extend the landed Jazz-authored hosted parser with lambdas,
`if`/`then`/`else`, `case`, single case-arm guards, and the complete accepted
pattern surface. Match the active Haskell stage-0 parser exactly over one
fixed 75-case family while preserving the fixed surface AST, structured
failure schema, token/source façades, grammar-context rules, and all evidence
from the first three children.

The child is one coherent parser milestone. It does not add operator grammar,
full-corpus closure, recovery, lowering, or runtime semantics.

## Scope

The child adds:

- lambda expressions with one or more parenthesized parameters;
- identifier and pattern lambda parameters;
- nested `if`/`then`/`else` expressions with exact delimiter ownership;
- `case` expressions, one or more pipe-led arms, and recursive arm bodies;
- the single optional `if` guard currently accepted on a case arm;
- literal, wildcard, variable, constructor, exact-list, cons-list, tuple,
  as-, and top-level or-patterns;
- the distinct stage-0 restrictions for lambda parameters, constructor
  arguments, and case-arm patterns;
- stop-aware recursive expression parsing at control-flow delimiters;
- one fixed `ControlFlowPatterns` fixture family;
- focused component coverage for pattern and control-flow boundaries; and
- an additive deterministic 513-statement control-flow scale profile.

The child does not add:

- fixed or user-defined operators, precedence, associativity, sections, or
  operator declaration visibility;
- mixed operator/control-flow parity cases;
- full parser-corpus assignment or parity closure;
- multiple case guards, lambda guards, pattern synonyms, nested/grouped
  or-patterns, or new pattern forms;
- changes to `ParserTypes`, `ParserCore`, or the public parser façade;
- new grammar-context state;
- parser recovery, partial ASTs, or diagnostic normalization;
- a public parser-combinator or pattern-parser API;
- parser-specific host intrinsics or Haskell callbacks;
- lowering, canonical core, type inference, bytecode, VM, LLVM, object, link,
  or native-runtime work; or
- changes under `jazz-hs/` or `jazz2/`.

## Approaches Considered

### Keep all new grammar in `ParserExpression.jz`

This minimizes new modules, but it would make one module own literals,
application, explicit type application, blocks, recursive control flow, and
the complete pattern grammar. Pattern-specific recursion and failure rules
would become difficult to review independently and would enlarge the module
before the operator child adds another expression layer.

### Add separate `ParserPattern.jz` and `ParserControlFlow.jz` modules

This maximizes surface-level separation. Control flow recursively consumes
expressions, however, so `ParserControlFlow` would either import
`ParserExpression` and create a cycle or accept a large family of higher-order
callbacks for conditions, branches, scrutinees, guards, and bodies. That
abstraction cost is not justified for a compiler-local parser with one
consumer.

### Isolate patterns and retain recursive control flow in expressions

This is the chosen approach. Add `ParserPattern.jz` for the independent
pattern domain. Keep lambda, conditional, case, guard, and recursive
expression ownership in `ParserExpression.jz`, behind a private stop-aware
seam. This mirrors the active stage-0 module boundary without copying every
Haskell helper or creating a dependency cycle.

## Module Ownership

### `ParserPattern.jz`

`ParserPattern` owns the complete pattern grammar required by this child. It
depends on canonical tokens, `ParserCore`, and the existing pattern and lambda
parameter constructors from `ParserTypes`; it does not depend on
`ParserExpression`, `ParserProgram`, declaration grammar, or grammar context.

The module exposes compiler-local token parsers for:

- one case pattern;
- one complete case-arm pattern, including top-level alternatives; and
- one lambda parameter.

The case-pattern parser accepts:

- integer, character, text, and boolean literal patterns;
- `_` wildcard patterns;
- lowercase variable patterns;
- uppercase constructor patterns with zero or more arguments;
- exact list patterns such as `[head, _]`;
- cons-list patterns such as `[head | tail]`;
- unit and tuple patterns;
- lowercase as-patterns such as `whole @ Just item`; and
- top-level or-patterns in case arms.

The parser preserves the existing distinctions between top-level patterns and
constructor arguments. An or-pattern is collected only at the complete
case-arm or lambda-parameter level; it is not accepted inside another pair of
parentheses. Constructor arguments use the stage-0 subset and stop at the same
arm, guard, tuple, list, and expression delimiters.

Lambda parsing preserves the existing `SurfaceLambdaParameter` distinction.
An ordinary lowercase identifier becomes `IdentifierParameter`; wildcard,
literal, constructor, list, tuple, as-, or or-pattern syntax becomes
`PatternParameter`. Parenthesized unit remains the stage-0 pattern parameter,
while a bare unparenthesized lambda parameter remains rejected.

### `ParserExpression.jz`

`ParserExpression` continues to own all expression recursion. Its existing
public entry points remain:

```text
parseFoundationalExpression
parseFoundationalExpressionWithContext
```

Their implementation expands from the expression foundation to the complete
non-operator expression slice. A private stop-aware recursive seam carries a
delimiter predicate through primary parsing, application tails, blocks, and
new control-flow forms. No stop predicate becomes public API.

The primary-expression dispatcher recognizes `if`, `case`, and the canonical
lambda token before falling back to ordinary identifiers. This preserves the
stage-0 reserved-word boundary without changing lexer token kinds. Ordinary
names such as `then` and `else` remain controlled by the exact recursive stop
site rather than being globally removed from identifier syntax.

`ParserExpression` owns:

- lambda parameter lists and the required arrow;
- recursive lambda bodies;
- conditional conditions, branches, and delimiter checks;
- case scrutinees and brace ownership;
- non-empty pipe-led arm lists;
- the single optional guard per arm; and
- recursive arm guards and bodies.

It delegates only pattern syntax to `ParserPattern`.

### Existing parser modules

`ParserProgram` retains statement sequencing and nested-block callbacks.
`ParserDeclaration` retains binding, expression-statement, declaration, and
module ownership. Both continue to consume the same context-aware expression
entry point. `ParserContext` gains no field or transition. `Parser.jz` keeps
the distinct canonical-token and source entry points.

`ParserTypes.jz` and `ParserCore.jz` are fixed by Child 1 and must remain
unchanged. Every surface, pattern, case-arm, lambda-parameter, failure, and
parser-result constructor required by this child already exists.

## Stop-Aware Expression Data Flow

The private recursive expression seam takes a stop predicate that is checked
before primary dispatch and before every application-tail step. A caller may
therefore delimit an expression without consuming the delimiter or teaching
the parser that a delimiter is globally reserved.

Control-flow parsing uses it as follows:

1. At `if`, parse the condition until the adjacent `then` delimiter.
2. Consume `then`, parse the true branch until the matching `else`, and consume
   `else` exactly once.
3. Parse the false branch with the caller's inherited stop predicate. Nested
   conditionals therefore associate each `else` with the nearest incomplete
   `if` exactly as stage 0 does.
4. At `case`, parse the scrutinee until the opening arm brace.
5. Require a pipe-led first arm. Parse one complete arm pattern through
   `ParserPattern`.
6. If `if` follows the pattern, parse one guard until `->`. A second guard is
   not accepted.
7. Consume `->`, then parse the arm body until the next top-level arm pipe or
   closing brace.
8. Repeat without capturing pipes or braces inside nested lists, tuples,
   blocks, lambdas, conditionals, or cases.
9. Return the delimiter to the enclosing parser layer and preserve the
   caller's stop boundary after the control-flow expression completes.

Blocks inside conditions, scrutinees, guards, or bodies continue through the
landed `ParserProgram` callback with `NestedBlockContext`. No control-flow form
mutates aliases or statement placement.

## Control-Flow Contract

### Lambdas

A lambda starts with the canonical backslash token, requires one
parenthesized parameter list, requires at least one parameter, and consumes a
comma-separated list without a trailing comma. The closing parenthesis must
be followed by `->`. The body is a complete recursive expression under the
caller's stop boundary.

Patterns are accepted only through `ParserPattern`'s lambda-parameter entry.
Case-arm guards are not lambda-parameter syntax. Duplicate parameter names are
preserved as parser output because binding validation belongs to later
analysis.

### Conditionals

An `if` expression requires a condition, `then` branch, and `else` branch.
Missing or extra delimiters preserve the exact stage-0 structured failure,
encountered token, payload, and span. Nested conditionals remain
right-associated through recursive stop ownership.

### Cases and guards

A `case` expression requires a scrutinee, opening brace, non-empty pipe-led arm
list, and closing brace. Each arm contains one complete pattern, an optional
single `if` guard, `->`, and one recursive body expression.

Top-level or-pattern alternatives belong to the pattern, not the arm list.
The parser uses stage-0 delimiter and lookahead behavior to distinguish the
next pattern alternative, guard, arrow, next arm, and closing brace. It does
not simplify unusual but accepted stage-0 arm boundaries merely because a
cleaner grammar would be possible.

## Pattern and Failure Contract

Every accepted result uses the existing surface constructors without
normalization. Every rejected result compares the full existing failure:
code, optional span, reason constructor, encountered token or end-of-input,
and all text or structured payloads.

The child preserves these stage-0 boundaries:

- fractional literal patterns use the existing
  `UnsupportedSyntax FractionalLiteralPattern` reason;
- cons-like lists reject more than one head through
  `PatternFailure ConsLikeListPatternHeadCount`;
- malformed list, tuple, constructor, as-, and or-patterns fail at the same
  token and with the same expectation context as stage 0;
- nested or grouped or-patterns remain rejected;
- a guard is accepted only after a complete case-arm pattern;
- a guard without an expression remains rejected;
- lambda-parameter guards remain rejected;
- missing lambda arrows, case-arm pipes, arm arrows, braces, `then`, and
  `else` retain exact structured failures; and
- lexically rejected sources remain source-façade lexical failures rather than
  parser failures.

No fixture may be excluded, reclassified, or compared only for acceptance to
avoid an exact failure mismatch. If the fixed schema cannot represent a
stage-0 result, that is a foundational defect requiring separate review, not
authorization to change `ParserTypes` in this child.

## Fixed `ControlFlowPatterns` Family

The family contains exactly 75 fixtures in explicit manifest order: 72
existing checked-in corpus cases plus three focused additions. Family
membership is by stable name, never by a positional corpus prefix.

### Foundational lambdas and control flow (6)

```text
parser-corpus-0042
parser-corpus-0045
parser-corpus-0046
parser-corpus-0048
parser-corpus-0049
parser-corpus-0063
```

### Lambda and reserved-word boundaries (15)

```text
parser-corpus-0086
parser-corpus-0087
parser-corpus-0088
parser-corpus-0090
parser-corpus-0091
parser-corpus-0092
parser-corpus-0093
parser-corpus-0094
parser-corpus-0095
parser-corpus-0096
parser-corpus-0097
parser-corpus-0098
parser-corpus-0100
parser-corpus-0101
parser-corpus-0102
```

### Later lambda contexts (5)

```text
parser-corpus-0195
parser-corpus-0197
parser-corpus-0199
parser-corpus-0200
parser-corpus-0201
```

### Case foundations and block scrutinees (5)

```text
parser-corpus-0245
parser-corpus-0246
parser-corpus-0247
parser-corpus-0248
parser-corpus-0249
```

### Case-arm and pattern boundaries (13)

```text
parser-corpus-0258
parser-corpus-0259
parser-corpus-0260
parser-corpus-0261
parser-corpus-0262
parser-corpus-0263
parser-corpus-0264
parser-corpus-0265
parser-corpus-0266
parser-corpus-0267
parser-corpus-0268
parser-corpus-0269
parser-corpus-0270
```

### Constructor, list, guard, and body boundaries (24)

```text
parser-corpus-0272
parser-corpus-0273
parser-corpus-0274
parser-corpus-0275
parser-corpus-0276
parser-corpus-0277
parser-corpus-0279
parser-corpus-0280
parser-corpus-0282
parser-corpus-0283
parser-corpus-0284
parser-corpus-0285
parser-corpus-0286
parser-corpus-0287
parser-corpus-0288
parser-corpus-0289
parser-corpus-0290
parser-corpus-0292
parser-corpus-0293
parser-corpus-0294
parser-corpus-0295
parser-corpus-0296
parser-corpus-0297
parser-corpus-0298
```

### Conditional boundaries (4)

```text
parser-corpus-0301
parser-corpus-0302
parser-corpus-0303
parser-corpus-0304
```

### Focused additions (3)

`control-flow-patterns-guarded-or-pattern` is accepted:

```jazz
x = case value { | Just item | Also item if ok -> item | Nothing -> 0 }.
```

`control-flow-patterns-lambda-guard-rejected` is rejected:

```jazz
f = \(Just item | Also item if ok) -> item.
```

`control-flow-patterns-recursive-block` is accepted:

```jazz
x = { loop = \(value) -> case value { | Just next -> loop next | _ -> if False then value else value }. loop. }.
```

These additions fix the operator-independent equivalents of the guarded
or-pattern and lambda-guard boundaries and add one combined recursive
nested-block integration case. They do not duplicate an existing stable name.

The shared manifest validator must continue to reject duplicate fixture
names, duplicate members within a family, and missing family members.

## Deferred Mixed Operator Cases

The fixed family intentionally excludes fixtures whose control-flow result
cannot be reached without operator grammar. Those cases remain assigned to
Child 5, including operator bindings with lambda bodies, operator expressions
inside lambda bodies, operator scrutinees, comparison guards, operator-valued
case bodies, and conditionals with comparison conditions.

In particular, the current corpus cases `parser-corpus-0021`, `0022`, `0027`,
`0089`, `0103`, `0149`, `0167`, `0168`, `0174`, `0184`, `0185`, `0198`,
`0244`, `0250` through `0257`, `0271`, `0278`, `0281`, `0291`, and `0305`
must not be added to `ControlFlowPatterns`. Child 5 must exercise them after
operator ownership exists and then prove full manifest assignment.

## Differential Parity

For all 75 family members, the Haskell harness:

1. loads the family by stable manifest name;
2. tokenizes the source with the active stage-0 lexer;
3. canonicalizes the normalized logical path and token sequence;
4. computes the complete stage-0 token-entry result;
5. computes the complete stage-0 source-entry result;
6. runs the Jazz-authored token batch twice;
7. runs the Jazz-authored source batch twice; and
8. requires byte-identical rendered values for expected versus actual and
   first versus second execution.

The comparison includes the complete surface AST and retained spans for
successes and the complete structured failure for rejections. The existing
52-case `ExpressionFoundation` and 101-case `TypesDeclarationsModules`
families remain unchanged and run as regressions.

## Control-Flow Scale Evidence

Add a separate generated program containing exactly 512 uniquely named
bindings followed by one terminal expression statement, for exactly 513
surface statements. The binding templates rotate deterministically through:

- a lambda whose body is a nested conditional;
- a lambda with an exact or cons-list parameter whose body is a case;
- a lambda with a constructor or as-pattern whose case arm has a simple
  boolean guard; and
- a recursive reference inside a nested block-owned control-flow body.

The generated source uses only grammar owned by Children 1 through 4. It uses
no infix operators, sections, operator values, operator declarations, or
parser-specific host support.

Run the generated case twice through the ordinary Jazz module graph and
require:

- successful compilation and execution of the hosted-parser harness;
- output `513` on both runs;
- identical runtime observation statistics;
- zero host operations;
- at most `100,000,000` evaluator transitions;
- at most `12,000,000` applications;
- at most `600,000` list cells constructed; and
- at most `1,200` maximum continuation depth.

These ceilings are initial deterministic safety budgets, not performance
targets. The implementation must record the actual observations and may
tighten the ceilings before closeout. It may not weaken either landed scale
profile. The 512-binding expression profile and 513-statement declarations
profile remain additive regression gates.

## Target Paths

The implementation plan may create or modify only the active `jazz-next` and
coordination surfaces required by this child:

- create `jazz-next/jazz/compiler/ParserPattern.jz`;
- modify `jazz-next/jazz/compiler/ParserExpression.jz`;
- retain and, only if integration requires it, narrowly modify
  `jazz-next/jazz/compiler/ParserProgram.jz` and
  `jazz-next/jazz/compiler/ParserDeclaration.jz`;
- modify `jazz-next/test/JazzNext/Compiler/Parser/FixtureCorpus.hs`;
- modify
  `jazz-next/test/JazzNext/Compiler/Bootstrap/CanonicalLexerComparisonSpec.hs`
  only for shared-corpus invariants affected by the three focused additions;
- modify
  `jazz-next/test/JazzNext/Compiler/Bootstrap/CanonicalParserComparisonSpec.hs`
  for the fixed corpus total and new family contract;
- modify
  `jazz-next/test/JazzNext/Compiler/Bootstrap/JazzParserParity.hs`;
- create
  `jazz-next/test/JazzNext/Compiler/Bootstrap/JazzParserControlFlowPatternsSpec.hs`;
- modify `jazz-next/test/JazzNext/Compiler/Bootstrap/JazzParserScale.hs` and
  `JazzParserScaleSpec.hs`;
- modify `jazz-next/jazz-next.cabal` for the new compiler module and focused
  test suite; and
- update this design, the accepted parent parser design, the implementation
  plan, `docs/execution/queue.md`, `docs/execution/blocker-contracts.md`, and
  `docs/execution/done-archive.md` during promotion and closeout.

Parser-core, schema, token-parser, lexer behavior, and stage-0 parser tests are
verification surfaces, not anticipated implementation targets. The two
canonical comparison specs may change only for the new corpus total, family
manifest, and fixed expectation audit. Any required schema or stage-0 behavior
change stops this child for a separate reviewed correction.

## Verification Contract

Focused verification must cover:

- direct `ParserPattern` component behavior for every pattern form and
  malformed boundary;
- direct stop-aware expression behavior for lambda, conditional, case, guard,
  nesting, and delimiter ownership;
- exact token/source parity for all 75 `ControlFlowPatterns` fixtures twice;
- unchanged parity for the 52-case and 101-case landed families;
- the new 513-statement control-flow scale profile twice;
- both landed scale profiles unchanged;
- schema and total Haskell-adapter coverage; and
- the active Haskell parser's focused lambda, conditional, case, pattern,
  declaration, module, and expression suites.

The implementation must finish with:

1. a warning-clean all-target `jazz-next` build;
2. the focused hosted-parser component, parity, and scale suites;
3. canonical lexer/parser comparison, parser-core, token-parser, parser schema,
   and relevant active-parser suites;
4. `cabal test --project-dir=jazz-next all`;
5. `cabal check` for `jazz-next`;
6. the warning-configuration and stdlib-format gates;
7. `bash scripts/check-execution-queue.sh`;
8. `bash scripts/check-docs.sh`;
9. `git diff --check`; and
10. confirmation that no file under `jazz-hs/` or `jazz2/` changed.

The implementation plan must spell out the exact current commands and Cabal
suite names. It may not replace this contract with a generic test instruction.

## Delivery and Queue Lifecycle

After written design and plan review:

1. add exactly one `Ready Now` row for
   `JN-BOOTSTRAP-JAZZ-PARSER-CONTROL-FLOW-PATTERNS-001`;
2. align the queue row and plan frontmatter exactly on priority, readiness,
   dependencies, plan section, target paths, deliverable, and verification;
3. implement in test-first milestone commits;
4. run focused parity and scale evidence before full closeout;
5. record the exact family size and runtime observations;
6. archive the child and remove it from `Ready Now` in the same closeout pass;
7. update the parent parser design and blocker contract to make
   operators/full parity the sole next curation target; and
8. rerun queue, docs, and diff validators after removing every stale reference
   that still names control flow and patterns as future work.

Completing this child makes Child 5 eligible for separate design and planning.
It does not promote Child 5 automatically.

## Acceptance Checklist

- `ParserPattern` owns the complete accepted pattern surface without importing
  expression or program parsing.
- `ParserExpression` owns stop-aware recursive lambda, conditional, case, and
  guard parsing without exposing a new public stop API.
- `ParserTypes`, `ParserCore`, `ParserContext`, and parser façades retain their
  landed contracts.
- All 75 `ControlFlowPatterns` fixtures match complete stage-0 token and source
  results twice.
- Both landed parity families remain byte-identical and deterministic.
- The new 513-statement profile succeeds twice with deterministic statistics,
  zero host operations, and recorded observations within its ceilings.
- Both landed scale profiles retain their current outputs and ceilings.
- Mixed operator/control-flow fixtures remain deferred to Child 5.
- No recovery, full-corpus, lowering, backend, runtime, or legacy-reference
  work enters the change.
- The queue, blocker contract, parent design, child plan, archive, and README
  status agree after closeout.
