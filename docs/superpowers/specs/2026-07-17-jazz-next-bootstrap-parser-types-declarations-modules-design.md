# Jazz-Next Bootstrap Parser Types, Declarations, and Modules Design

## Status

Approved section by section in discussion on `2026-07-17`. This document is
the written design checkpoint for
`JN-BOOTSTRAP-JAZZ-PARSER-TYPES-DECLARATIONS-MODULES-001`, the third ordered
child of the accepted
[bootstrap Jazz parser design](2026-07-12-jazz-next-bootstrap-jazz-parser-design.md).

This child remains a curation target until its written design and subsequent
implementation plan have both been reviewed. Review of this document does not
itself promote the child into `Ready Now`.

## Goal

Extend the landed Jazz-authored hosted parser with the next coherent grammar
slice: signatures, explicit type applications, data/class/impl declarations,
module headers, imports, exports, and the grammar context required by those
forms. The Jazz parser must match the active Haskell stage-0 parser exactly over
one explicit stable fixture family while preserving the fixed surface AST and
structured failure contract.

The result is one executor-ready child delivered as one pull request with
milestone commits. It does not pre-seed or absorb later parser children.

## Scope

The child adds:

- ordinary and constrained signature payloads;
- named, variable, applied, list, tuple, unit, numeric-width, and
  right-associative function signature types;
- lossless unsupported-signature token fallback;
- explicit type application expressions;
- data declarations and constructor argument surfaces;
- class declarations with method signatures;
- impl declarations with expression-owned method bodies;
- module headers, module paths, and module export selectors;
- imports, aliases, and explicit import symbol lists;
- immutable top-level, module-body, and nested-block grammar context;
- forward import-alias collection within the current statement-list scope;
- declaration placement, duplicate, and boundary diagnostics; and
- focused token/source parity plus mixed-grammar scale evidence.

The child does not add:

- lambdas, `if`, `case`, guards, or pattern grammar;
- fixed or user-defined operator parsing, precedence, associativity, or
  sections;
- full parser-corpus parity;
- parser recovery or partial ASTs;
- a public parser-combinator API;
- parser-specific host intrinsics or Haskell callbacks;
- lowering, canonical core, type inference, bytecode, VM, LLVM, object, link,
  or native-runtime work; or
- changes under `jazz-hs/` or `jazz2/`.

## Approaches Considered

### Extend `ParserProgram` directly

This minimizes the number of new modules, but it would make `ParserProgram`
own sequencing, signature types, declarations, modules, exports, aliases, and
scope rules. The resulting unit would be difficult to review and would give
the later pattern and operator children a poor integration boundary.

### Split every declaration form into its own module

Separate type, signature, data, class, impl, module, import, export, and
context modules would maximize local isolation. It would also introduce many
small cross-module interfaces before the hosted parser has a second consumer
for those distinctions. That is unnecessary API surface for this child.

### Mirror the active stage-0 grammar domains

This is the chosen approach. Add focused `ParserSignature`, `ParserContext`,
and `ParserDeclaration` modules; keep module/import/export syntax under
declaration ownership; keep statement sequencing in `ParserProgram`; and
reuse the signature type parser from `ParserExpression`. This follows the
accepted parent design and the active Haskell ownership model without copying
every Haskell helper one for one.

## Module Ownership

### `ParserSignature.jz`

`ParserSignature` owns signature payload and type grammar. It parses supported
payloads into the existing `SurfaceSignaturePayload` and
`SurfaceSignatureType` constructors, including:

- empty and non-empty constraint blocks;
- qualified constraint names;
- primitive and numeric-width names;
- lowercase type variables and uppercase named types;
- adjacent named type applications such as `Maybe(Int)`;
- `List(a)` normalization to the same node as `[a]`;
- tuple and unit types; and
- right-associative function types.

The module also owns conversion from canonical lexer tokens to
`SurfaceSignatureToken`. When a statement has already been classified as a
signature but the payload is not in the supported grammar, the module returns
`UnsupportedSignature` with every payload token preserved in order. It does
not turn unsupported type notation into a parser failure.

`ParserSignature` exposes one token-parser entry point for a supported type
prefix so `ParserExpression` can parse explicit type applications without
creating a second type grammar. It does not own signature statement
classification, matching-binding checks, or statement terminators; those are
declaration grammar concerns.

### `ParserContext.jz`

`ParserContext` owns ordinary immutable grammar state:

- `StatementContext` with `TopLevelContext`, `ModuleBodyContext`, and
  `NestedBlockContext`;
- the set of aliases visible in the current statement-list scope; and
- constructors and transformations for initial, module-body, and nested-block
  context.

The top-level "seen prior form" bit remains loop state in `ParserProgram`
because it enforces ordering within one program rather than name visibility.
Declared-operator metadata remains deferred to Child 5. Adding that field
later does not authorize operator parsing in this child.

A module body receives a fresh alias set and `ModuleBodyContext`. A nested
expression block inherits the enclosing visible aliases but changes to
`NestedBlockContext`. Context never lives in `ParserCore`, global mutable
state, or an evaluator effect.

### `ParserDeclaration.jz`

`ParserDeclaration` owns statement classification and all declaration-level
grammar in this child. Its responsibilities are:

- preserve the landed binding/expression/signature boundary decisions;
- collect one signature payload through its terminating dot, delegate payload
  interpretation to `ParserSignature`, and build `SignatureStatement`;
- parse data constructors and validate declared parameters, constructor names,
  argument delimiters, and duplicates;
- parse class parameters and class method signatures;
- parse impl targets and impl method bindings while delegating each method
  expression to the injected expression parser;
- parse module paths, optional export lists, and module bodies;
- parse imports, optional aliases, and optional explicit symbol lists;
- pre-scan aliases at the current statement-list depth;
- register parsed import aliases idempotently; and
- enforce declaration scope and duplicate rules through the existing
  `ParserFailureReason` schema.

Module headers, imports, and exports stay in this module because they are
statement/declaration syntax and share scope and duplicate validation. They do
not get folded into the source façade or module resolver.

`ParserDeclaration` accepts expression and block callbacks at the recursive
boundaries used by impl methods and module bodies. It does not duplicate
expression grammar or depend on later control-flow, pattern, or operator
implementations.

### `ParserProgram.jz`

`ParserProgram` becomes the sequencing and recursive-orchestration layer. It:

- constructs the initial context;
- asks `ParserDeclaration` to pre-collect aliases for the current statement
  list;
- parses statements in source order while threading explicit context;
- tracks whether a prior top-level form has appeared;
- enforces that a module header is the first top-level form;
- creates isolated module-body context;
- creates inherited nested-block context; and
- returns one `BlockExpression` containing the complete flattened stage-0
  surface statement sequence.

The existing expression-foundation statement logic moves to its natural owner
only where required to avoid two statement dispatchers. `ParserProgram` does
not retain parallel signature/declaration classifiers after the move.

### `ParserExpression.jz`

`ParserExpression` gains grammar context as an explicit input and adds
explicit type application to its application tail. An `@` must satisfy the
same adjacency and type-prefix rules as stage 0. The type is parsed by
`ParserSignature`; successful parsing creates the existing
`TypeApplicationExpression` with the retained source span.

The expression module does not gain declaration parsing. Its context-aware
interface is the integration point for alias visibility now and declared
operators in Child 5.

### `Parser.jz` and `ParserTypes.jz`

`Parser.jz` remains the façade with distinct canonical-token and source entry
points. Source parsing continues to distinguish lexical failures from parser
failures.

`ParserTypes.jz` remains the fixed surface/result/failure schema established by
Child 1. This child may correct an objectively missing constructor only through
a separate reviewed schema correction; implementation convenience is not a
reason to change the contract.

## Grammar Context and Data Flow

The token entry path is:

1. `Parser.parseTokens` receives a normalized path and canonical lexer tokens.
2. `ParserProgram` asks `ParserDeclaration` to scan the current top-level token
   list once for import aliases at depth zero.
3. `ParserProgram` parses forms sequentially with `TopLevelContext`, the
   collected alias set, and an explicit prior-form bit.
4. `ParserDeclaration` returns the surface statement or statements produced by
   one form together with the next immutable context.
5. A module header creates a `ModuleStatement`, pre-scans only its own body,
   parses that body with a fresh alias set and `ModuleBodyContext`, and returns
   the header followed by the body statements in the same flattened order as
   stage 0.
6. A nested expression block inherits aliases from its enclosing scope, changes
   to `NestedBlockContext`, and rejects declarations that are not legal there.
7. `ParserProgram` produces one complete `BlockExpression` or the first
   structured parser failure.

Alias pre-collection intentionally preserves stage-0 forward visibility, so a
qualified use may precede its import within the same top-level or module-body
statement list. The scan tracks delimiter depth and stops at the current
right-brace boundary. It does not collect aliases from nested modules or
expression blocks into the enclosing scope.

The source entry path first invokes the landed Jazz lexer. A lexical failure
remains `CanonicalSourceLexicalFailure`; successful lexing continues through
the token entry path; a parser failure remains
`CanonicalSourceParserFailure`. The façade never flattens either failure into
display text.

## Signature Compatibility

The landed expression child already contains careful statement-boundary
classification for signature-looking forms. This child preserves those
decisions while moving their final ownership into `ParserDeclaration`:

- reserved `True` and `False` signature names remain rejected with the existing
  declaration failure;
- adjacent and spaced `::` retain their stage-0 classifications;
- payload scanning may not cross a token shape that begins another statement;
- matching-binding checks retain their existing stage-0 boundary behavior;
- abstraction and operator words remain ordinary names where stage 0 treats
  them as ordinary names; and
- unsupported payload syntax is recorded losslessly after signature
  classification rather than rejected early.

Explicit type application reuses the supported type grammar but does not use
the unsupported-signature fallback. A malformed explicit type argument remains
a parser failure with the existing explicit-type-application reason or
expected-syntax payload.

## Declaration and Module Compatibility

The implementation preserves the current stage-0 rules represented by the
fixed failure schema, including:

- data type parameter and constructor-name uniqueness;
- constructor arguments referring only to declared type parameters where the
  current grammar requires that relation;
- constructor delimiter consistency;
- explicit lowercase class parameters, uniqueness, and the current exactly-one
  parameter rule;
- class bodies containing signatures rather than expressions;
- concrete impl targets and ordinary impl method bindings;
- unique impl method names;
- imports disallowing an alias together with a symbol list;
- non-empty and duplicate-free import/export lists;
- constructor export-group rules and namespace-preserving export selectors;
- imports and data/class/impl declarations being legal at top level and inside
  a module body but not in nested expression blocks;
- nested module rejection; and
- the module header being the first top-level form.

`trait` remains rejected as unsupported abstraction syntax. It is not accepted
as a declaration alias or compatibility form. Occurrences of the text `trait`
that stage 0 treats as ordinary binding names or import aliases remain ordinary
names.

## Failure Model

The parser remains fail-fast and returns no partial AST. This child preserves:

- the existing `ParserFailureReason` constructors and semantic payloads;
- retained source spans and normalized logical paths;
- consumed versus unconsumed failure behavior;
- explicit rollback only at shared prefixes;
- farthest-failure selection with declaration-order tie breaking; and
- progress checks in repeated parsing and alias scans.

Malformed declaration structure uses the existing specific declaration
failures for scope violations, duplicate names or list items, invalid class
parameters, non-concrete impl targets, constructor parameter errors, import
alias conflicts, export-list errors, and module placement. Module-body and
nested-block errors retain their original scope and token span.

The parity adapter compares structured values. It does not inspect diagnostic
summaries, exception names, or rendered messages. Compatibility entry points
continue to render structured stage-0 parser failures through the existing
diagnostic renderer.

## Fixed Parity Family

Add `TypesDeclarationsModules` to `ParserFixtureFamily`. Its manifest contains
exactly 101 explicitly ordered fixtures: 98 existing checked-in corpus cases
and three focused additions. Selection is by name, never by position or a
numeric slice.

### Existing signature and type cases (21)

- `parser-corpus-0034`
- `parser-corpus-0038`
- `parser-corpus-0039`
- `parser-corpus-0047`
- `parser-corpus-0050`
- `parser-corpus-0074`
- `parser-corpus-0076`
- `parser-corpus-0077`
- `parser-corpus-0078`
- `parser-corpus-0131`
- `parser-corpus-0191`
- `parser-corpus-0192`
- `parser-corpus-0204`
- `parser-corpus-0205`
- `parser-corpus-0207`
- `parser-corpus-0208`
- `parser-corpus-0215`
- `parser-corpus-0216`
- `parser-corpus-0220`
- `parser-corpus-0221`
- `parser-corpus-0222`

### Existing explicit type application cases (3)

- `parser-corpus-0210`
- `parser-corpus-0211`
- `parser-corpus-0212`

### Existing class cases (11)

- `parser-corpus-0052`
- `parser-corpus-0053`
- `parser-corpus-0054`
- `parser-corpus-0055`
- `parser-corpus-0056`
- `parser-corpus-0057`
- `parser-corpus-0058`
- `parser-corpus-0059`
- `parser-corpus-0060`
- `parser-corpus-0061`
- `parser-corpus-0062`

### Existing data cases (9)

- `parser-corpus-0064`
- `parser-corpus-0065`
- `parser-corpus-0066`
- `parser-corpus-0067`
- `parser-corpus-0068`
- `parser-corpus-0069`
- `parser-corpus-0070`
- `parser-corpus-0071`
- `parser-corpus-0072`

### Existing impl cases (5)

- `parser-corpus-0104`
- `parser-corpus-0105`
- `parser-corpus-0106`
- `parser-corpus-0107`
- `parser-corpus-0108`

### Existing import and alias cases (22)

- `parser-corpus-0109`
- `parser-corpus-0110`
- `parser-corpus-0111`
- `parser-corpus-0112`
- `parser-corpus-0113`
- `parser-corpus-0114`
- `parser-corpus-0115`
- `parser-corpus-0116`
- `parser-corpus-0117`
- `parser-corpus-0118`
- `parser-corpus-0119`
- `parser-corpus-0120`
- `parser-corpus-0121`
- `parser-corpus-0122`
- `parser-corpus-0123`
- `parser-corpus-0124`
- `parser-corpus-0125`
- `parser-corpus-0126`
- `parser-corpus-0127`
- `parser-corpus-0128`
- `parser-corpus-0129`
- `parser-corpus-0133`

### Existing module and export cases (27)

- `parser-corpus-0134`
- `parser-corpus-0135`
- `parser-corpus-0136`
- `parser-corpus-0137`
- `parser-corpus-0138`
- `parser-corpus-0139`
- `parser-corpus-0140`
- `parser-corpus-0141`
- `parser-corpus-0142`
- `parser-corpus-0143`
- `parser-corpus-0144`
- `parser-corpus-0145`
- `parser-corpus-0146`
- `parser-corpus-0147`
- `parser-corpus-0148`
- `parser-corpus-0150`
- `parser-corpus-0151`
- `parser-corpus-0152`
- `parser-corpus-0153`
- `parser-corpus-0154`
- `parser-corpus-0155`
- `parser-corpus-0156`
- `parser-corpus-0157`
- `parser-corpus-0158`
- `parser-corpus-0159`
- `parser-corpus-0235`
- `parser-corpus-0306`

### Focused additions (3)

- `types-declarations-modules-unsupported-forall-signature` is parser-accepted
  source `x :: forall a.\nx = 1.` and must produce an
  `UnsupportedSignature` payload with lossless tokens.
- `types-declarations-modules-foundational-impl-method` is parser-accepted
  source `impl Eq(Int) { equals = 1. }.` and proves impl expression callback
  ownership without requiring later expression grammar.
- `types-declarations-modules-applied-explicit-type-application` is
  parser-accepted source `value = id @Maybe(Int) value.\nvalue.` and proves
  reuse of applied signature types in expression position.

The manifest validator continues to reject duplicate fixture names, duplicate
family members, and missing family members. A focused test asserts the exact
family size, order, and presence of successes and parser failures. Lexical
failure coverage remains owned by the expression family and complete corpus;
this family does not add an unrelated lexical case.

## Differential Parity

For all 101 fixtures, the Haskell harness:

1. resolves the family from the manifest;
2. runs the stage-0 parser and canonicalizes its complete surface AST or
   structured failure;
3. invokes the Jazz parser through the canonical-token entry point twice;
4. invokes the Jazz parser through the source entry point twice;
5. requires deterministic output within each entry point; and
6. requires exact equality with the corresponding stage-0 rendering.

The comparison includes retained spans, signature tokens, namespace-preserving
export selectors, declaration payloads, alias-qualified references, and every
structured failure payload. Acceptance/rejection alone is insufficient.

The existing 52-case `ExpressionFoundation` family remains unchanged and runs
as a regression gate.

## Mixed-Grammar Scale Evidence

Add a deterministic generated module containing exactly 512 module-body forms:

- 128 signature/binding pairs whose bindings use aliases declared later in the
  same module body, producing 256 forms;
- 128 generic data declarations with unique type, parameter, and constructor
  names; and
- 128 trailing imports with unique aliases.

Including the module header, the expected surface statement count is exactly
513. Uses preceding imports prove forward alias pre-collection at scale. The
generator uses only grammar owned by this child plus the landed expression
foundation.

Run the generated case twice through the ordinary Jazz module graph and require:

- successful compilation and execution of the parity harness;
- output `513` on both runs;
- identical runtime observation statistics;
- zero host operations;
- at most `80,000,000` evaluator transitions;
- at most `10,000,000` applications;
- at most `500,000` list cells constructed; and
- at most `1,100` maximum continuation depth.

The existing deterministic 512-binding expression scale case and its tighter
ceilings remain unchanged. The new profile is additive and may not weaken the
landed gate.

## Verification Contract

Focused verification includes:

- component tests for signature types, fallback token conversion, context
  transitions, alias scans, declaration parsing, module/export parsing, and
  explicit type application;
- schema and total Haskell-adapter tests;
- exact token/source parity over both named fixture families;
- the existing expression scale profile; and
- the new 513-statement mixed-grammar scale profile.

The implementation must finish with:

1. a warning-clean all-target `jazz-next` build;
2. all focused hosted-parser, canonical comparison, parser-core, token-parser,
   lexer-parity, and repository-audit suites;
3. all Cabal test suites;
4. `cabal check`;
5. the warning configuration and stdlib-format gates;
6. `bash scripts/check-execution-queue.sh`;
7. `bash scripts/check-docs.sh`;
8. shell syntax checks for affected scripts; and
9. `git diff --check` plus confirmation that no legacy-reference path changed.

The subsequent implementation plan must spell out the exact current commands
and Cabal suite names rather than replacing this contract with a generic
"tests pass" step.

## One-PR Delivery and Milestone Commits

The implementation is delivered as one pull request with milestone commits:

1. define the failing named-family, component, and scale contracts;
2. add signature grammar and explicit type application;
3. add immutable context plus data/class/impl declaration grammar;
4. add module, import, export, alias, and placement grammar;
5. complete exact parity and deterministic scale evidence; and
6. run full verification and close out queue/design documentation.

Commits may combine adjacent steps when the intermediate tree would otherwise
be uncompilable, but the history must retain reviewable milestones rather than
one monolithic implementation commit.

## Queue and Documentation Closeout

Before implementation, the reviewed design and implementation plan must agree
with the queue row and blocker contract on:

- child ID;
- target paths;
- deliverable;
- the 101-case fixture family;
- the mixed scale profile;
- focused and full verification; and
- explicit exclusions.

Only then may curation move
`JN-BOOTSTRAP-JAZZ-PARSER-TYPES-DECLARATIONS-MODULES-001` from `Next Curation
Target` into `Ready Now` with matching plan frontmatter.

On verified implementation completion, closeout must:

- archive this child in `docs/execution/done-archive.md`;
- remove its `Ready Now` row;
- update the bootstrap blocker and parent parser designs with exact landed
  evidence;
- make control-flow/patterns the sole next unpromoted parser curation target;
- keep operators/full parity ordered behind it; and
- update `jazz-next/README.md` only for directly affected hosted-parser module
  and test ownership.

Closeout does not claim complete parser parity or stage-1 bootstrap readiness.

## Acceptance Checklist

- The selected module ownership matches the accepted stage-0 domain model.
- Signature fallback is lossless and does not become a parser rejection.
- Explicit type application reuses the signature type grammar.
- Grammar context is immutable, explicit, and scoped correctly.
- Alias pre-collection preserves forward visibility without leaking across
  scope boundaries.
- Data, class, impl, module, import, and export forms preserve complete surface
  values and existing structured failures.
- `ParserTypes` does not change for implementation convenience.
- The exact 101-case family matches stage 0 through token and source entry
  points deterministically.
- The mixed 513-statement scale case is deterministic, host-operation-free,
  and within every fixed ceiling.
- The existing expression parity and scale gates remain unchanged and green.
- No control-flow, pattern, operator, full-corpus, backend, or legacy work
  enters the child.
- Focused, full-suite, queue/docs, shell, and cleanliness gates pass.
- Closeout archives only this child and curates, but does not promote, the
  control-flow/patterns child.
