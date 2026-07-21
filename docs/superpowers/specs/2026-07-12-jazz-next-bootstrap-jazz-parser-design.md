# Jazz-Next Bootstrap Jazz Parser Design

## Status

Approved in discussion and accepted after written review on `2026-07-16`.
The parser contract/kernel and expression-foundation children completed on
`2026-07-16`; types/declarations/modules completed on `2026-07-17` with exact
101-case parity and deterministic mixed scale evidence; control-flow/patterns
completed on `2026-07-20` with exact 75-case parity and deterministic
control-flow scale evidence; operators/full parity completed on `2026-07-20`
with six-family 365-case parity and deterministic operator scale evidence. The
five ordered grammar children are complete.

## Goal

Port the active Jazz surface parser into ordinary Jazz modules while preserving
exact stage-0 behavior over the fixed parser corpus. The hosted Jazz parser must
consume the existing canonical lexer tokens, produce the ordinary parser-owned
surface AST, return structured parser failures, and remain independent of
Haskell parser callbacks and backend implementation details.

This is the next hosted-bootstrap milestone after the Jazz-authored lexer. It
does not collapse parsing into lowering or canonical typed core. It also does
not change the permanent pipeline from canonical typed core through
backend-neutral lowered IR to LLVM, object production, linking, and the native
runtime.

## Decision Summary

- The parser remains fail-fast and returns no partial AST.
- Successful parity compares the complete parser-owned surface AST before
  lowering.
- Parser failures use a parser-specific structured ADT and convert separately
  into the unified user-facing `Diagnostic`.
- The primary parser entry point consumes canonical lexer tokens. A convenience
  source entry point composes the Jazz lexer and parser without obscuring which
  component failed.
- Jazz parser naming and module ownership broadly mirror the active Haskell
  parser. Canonical-comparison terminology stays at the parity boundary rather
  than being repeated throughout the AST.
- A small generic parser-combinator kernel is implemented first. It remains
  compiler-local until another real consumer justifies a stable public stdlib
  API.
- Grammar implementation proceeds in ordered reviewable slices behind one
  fixed AST, failure, and comparison contract.
- Existing Jazz `List`, `Maybe`, `Result`, `Map`, `Set`, `Text`, `Char`, and
  `NonEmpty` APIs are sufficient for the accepted design. No parser-specific
  primitive or host bridge is authorized.

## Approaches Considered

### Monolithic parser port

A single child could port the entire parser after fixing the comparison schema.
This would avoid temporary partial grammar support, but the active Haskell
grammar is split across several owner modules and includes expressions,
declarations, modules, signatures, patterns, control flow, and configurable
operators. One implementation review would be too large to verify comfortably,
and a late architectural defect would invalidate most of the branch.

### General public parser library first

A public stdlib parser-combinator library could be designed before the Jazz
grammar. That would prevent ad hoc cursor plumbing, but it would freeze a broad
user-facing API before any Jazz parser had exercised its consumption,
backtracking, context, error-selection, and performance behavior. It would also
make public-library design, documentation, and compatibility part of the
critical bootstrap path.

### Compiler-local parser kernel plus grammar slices

This is the chosen approach. The first implementation child establishes a
generic, independently tested parser kernel under compiler ownership together
with the fixed parser result and parity contract. Later children add coherent
grammar areas through that kernel. The kernel has no AST dependency and can be
promoted to a general stdlib module later without moving grammar code or
changing parser semantics.

## Architectural Boundaries

The Jazz implementation follows the active stage-0 ownership model:

- `ParserCore` owns generic parser state, consumption, success, failure, and
  combinators. It knows nothing about Jazz tokens, syntax, or ASTs.
- `ParserTypes` owns the ordinary parser-facing AST, result, and structured
  failure types.
- Focused expression, declaration, pattern, signature, operator, and context
  modules own their respective grammar areas.
- `Parser` is the façade that ties the recursive grammar together and exposes
  token and source entry points.
- The Haskell parity adapter is test-only. It converts stage-0 parser results
  into the same ordinary value schema used by Jazz and renders both sides with
  the existing generic runtime-value renderer.

These are ownership boundaries rather than a requirement to reproduce every
stage-0 file or internal helper one for one. The Jazz modules may split an owner
when necessary to keep each unit understandable, but they must not merge
unrelated grammar domains or create a second AST.

The hosted parser ends at the surface AST. Existing Haskell lowering remains
the active stage-0 path while parser parity is developed. Later bootstrap work
may port lowering and canonical typed core behind their own accepted contracts;
the parser does not call them or encode their representation.

## Public Entry Points and Results

The token entry point consumes a normalized source path plus the canonical
token sequence emitted by the Jazz lexer. It returns either one complete
surface program or one structured parser failure. Lexing and parsing therefore
remain independently measurable stages.

The source convenience entry point composes the existing Jazz lexer and token
parser. Its result distinguishes three outcomes:

1. successful parsing with the normalized path and complete surface program;
2. the existing canonical lexical failure; or
3. a structured parser failure.

A lexical failure is never converted into a parser failure. A parser failure is
never flattened into presentation text merely to pass through the façade.

The token and source entry points correspond to stage 0's
`parseSurfaceProgramTokens` and `parseSurfaceProgram`. Their names should remain
recognizable unless Jazz syntax forces a small spelling adjustment. The Jazz
API does not introduce a parallel family of `CanonicalSurface...` names: its
ordinary `SurfaceExpr`, `SurfaceStatement`, and related values are the canonical
parser result.

## Surface AST Contract

The Jazz surface AST mirrors the semantic constructors owned by the active
Haskell `Parser.AST` module:

- literals, variables, qualified variables, lambdas, applications, explicit
  type applications, lists, tuples, operators, sections, control flow, cases,
  and blocks;
- ordinary, signature, data, class, impl, module, import, and expression
  statements;
- literal, variable, wildcard, constructor, list, cons-like list, tuple, as-,
  and or-patterns;
- signature constraints, named and applied types, list and tuple types,
  right-associative function types, and structured unsupported-signature token
  fallbacks; and
- constructor arguments, class method signatures, impl methods, module export
  selectors, and the source spans currently retained by the surface tree.

Identifiers, module path components, operator symbols, signature tokens, and
export selectors remain semantically distinct even when each ultimately
contains text. The parity schema does not replace them with a single untyped
string field.

The parser comparison contract includes the selected spans already retained by
stage 0. It does not invent full-tree source ranges as part of the bootstrap
port. Adding comprehensive ranges remains a separate language-tooling change
because it would intentionally change both parser outputs.

## Source-Exact Numeric Literals

The active Haskell surface tree uses arbitrary-precision `Integer` and stores a
rounded `Double` alongside source-exact fractional metadata. Jazz currently has
neither an arbitrary-precision integer type nor a representation-neutral public
decimal-to-floating conversion suitable for compiler semantics.

The Jazz surface tree therefore stores integer literals as normalized decimal
text. Fractional literals store normalized whole-number text, the exact
fractional digit text (including leading or trailing zeroes and therefore its
scale), and the optional width suffix. This representation is lossless for
every canonical lexer token, including values outside fixed-width runtime
integer ranges.

The Haskell parity adapter normalizes its `Integer`, `Double`, and
source-metadata values into the same source-exact representation. Binary
floating rendering is not part of parser parity.

Stage-0 parser rejection of non-finite or out-of-range Float64 source literals
remains authoritative. The Jazz parser implements the same magnitude check with
compiler-local decimal text comparison. It does not require a parser-specific
host conversion or silently move the rejection into a later compiler stage.

## Structured Parser Failures

Parsing is fail-fast. A failure result contains:

- stable `E0001` identity;
- the normalized logical source path;
- an optional source span, because an end-of-input failure may not have a
  current token position; and
- one structured reason with the semantic payload required to reproduce the
  user-facing diagnostic.

Shared expected/found reasons cover ordinary token and end-of-input failures.
Their payloads distinguish expected syntax from an encountered token kind and
lexeme. Grammar-specific rule violations use explicit reason constructors with
payloads such as an operator, declaration, method, parameter, module member, or
violated grammar boundary.

Human diagnostic summaries, label wording, punctuation, Haskell exception
names, and Megaparsec expectation sets are not part of parser parity. The
stage-0 parser must expose structured failure data before constructing its
existing `Diagnostic`; the Haskell adapter may not inspect
`diagnosticSummary` to reconstruct a reason.

Compatibility entry points retain the current `Either Diagnostic` behavior by
converting a structured stage-0 parser failure through one explicit diagnostic
renderer. Existing wording and primary labels remain stable unless a separately
reviewed diagnostic change says otherwise.

## Parser-Core Semantics

The compiler-local parser kernel is generic over token, error, and result value
types. Its immutable cursor retains the remaining input plus a monotonic offset
used to compare progress. Consuming the head of the remaining list is the
normal operation; the kernel does not repeatedly index from the beginning of a
linked list.

Each parser step records consumption:

- success returns a value and the next cursor;
- failure returns one error and whether the branch consumed input;
- choice tries another branch only after an unconsumed failure;
- explicit rollback is available for a genuinely shared prefix;
- lookahead inspects without consuming; and
- when attempted alternatives fail at different offsets, the farthest failure
  wins, with declaration order breaking ties.

The kernel provides a small practical vocabulary for token inspection,
requirements, result transformation, dependent sequencing, alternatives,
lookahead, optional forms, repetition, and separated sequences. Public names
should describe these operations directly rather than requiring familiarity
with category-theory terminology.

Repeated parsing must make progress. A repeated parser that succeeds without
consuming input returns an internal parser failure instead of looping forever.
Repetition and token traversal use tail-recursive loops so the shared stack-safe
evaluator does not regain host-stack growth through library code.

The grammar stays predictive. It prefers token lookahead and direct dispatch
over broad speculative alternatives. Explicit rollback is narrow and visible;
the kernel is not an unrestricted backtracking search engine.

## Grammar Context

Import aliases, declared operators, and top-level/module/block statement
context remain ordinary immutable grammar context. They are not hidden inside
the generic parser state, a global mutable table, or a generalized effect
stack.

Grammar modules receive the context they need and return explicit updates at
the same boundaries where stage 0 changes visibility. This retains source-order
operator behavior, forward import-alias collection within the appropriate
scope, module-header placement rules, and isolation between nested blocks and
source units.

Ordered `Map` and `Set` values own lookup and duplicate detection where stage 0
uses Haskell maps or sets. Grammar code should not fall back to repeated linear
scans merely because the first lexer was list-oriented.

## Runtime and Stdlib Boundary

The accepted parser design uses existing ordinary Jazz modules:

- `List`, `Maybe`, and `Result` for parser flow and AST collections;
- `Map` and `Set` for grammar context and duplicate detection;
- `Text` and `Char` for names, symbols, numeric components, and error payloads;
  and
- `NonEmpty` where emptiness is semantically invalid.

The parser design does not authorize parser-specific builtins, Haskell
callbacks, mutable host token buffers, lexer/parser intrinsics, or a Haskell
collection bridge. The parser core and grammar execute as ordinary Jazz code
through the shared evaluator.

If implementation reveals a missing general-purpose collection or text
operation, it must be proposed and tested as a reusable API. The parser cannot
smuggle a private intrinsic into the kernel. Performance evidence should come
from the existing benchmark, compiler-stage profiling, and runtime-observation
tools before a representation boundary changes.

## Ordered Implementation Children

### Child 1: contract and parser kernel

Fix the complete Jazz surface/result/failure schema, add the total Haskell
normalization adapter, implement the generic compiler-local parser kernel, and
prove its consumption, error-selection, progress, determinism, and large-input
behavior. This child completed on `2026-07-16` without substantive Jazz
grammar.

### Child 2: expression foundation

Add program and block sequencing, literals, names, applications, lists, tuples,
unit, and ordinary binding and expression statements. Compare an explicit
stable fixture family for both successful ASTs and structured failures. This
child completed on `2026-07-16` with exact 43-case token/source parity and
deterministic 512-binding scale evidence.

### Child 3: types, declarations, and modules

Add signatures, explicit type applications, data/class/impl declarations,
module headers, imports, exports, and the corresponding grammar-context
updates. Preserve current unsupported-signature fallbacks and declaration
diagnostics. This child completed on `2026-07-17` with the fixed surface schema,
exact 101-case token/source parity across repeated evaluation, and a
deterministic 513-statement mixed profile. `ParserCore` and `ParserTypes`
remained unchanged.

### Child 4: control flow and patterns

Add lambdas, `if`/`then`/`else`, `case`, guards, and the complete accepted
pattern surface. Include rejection parity for malformed arm, guard, lambda,
constructor, list, tuple, as-, and or-pattern boundaries. This child completed
on `2026-07-20` with independent `ParserPattern` ownership, private stop-aware
control-flow recursion in `ParserExpression`, exact 75-case token/source parity
across repeated evaluation, and a deterministic 513-statement control-flow
profile. The 52-case and 101-case families remain exact; `ParserCore`,
`ParserTypes`, `ParserContext`, and the parser façades remain unchanged.

### Child 5: operators and full parity closure

Add fixed and user-declared operators, precedence, associativity, sections,
adjacency rules, source-order visibility, and full corpus closure. Prove that
every fixed parser fixture is assigned to a covered grammar family and that the
complete hosted parser matches stage 0 exactly.

This child completed on `2026-07-20`. `ParserOperator` owns fixed and declared
metadata, immutable context threads source-order visibility and scope reset,
and expression/declaration/program parsing matches stage 0 for precedence,
associativity, values, sections, bindings, signatures, failures, and mixed
control-flow composition. The 52 / 101 / 75 / 55 / 26 / 56 families assign all
365 fixtures exactly once and match complete token/source results twice. All
four scale profiles are deterministic with zero host operations; the operator
profile records 49,040,140 transitions / 5,914,883 applications / 186,465 list
cells / depth 1,116.

The children are ordered and may be reviewed as stacked changes. Child 1 fixes
the contract. Later children must not revise it merely to simplify an
implementation. If a genuine schema defect is discovered, correct it in the
foundational branch and review that change before rebasing dependent work.

## Fixture and Parity Rules

The existing stable parser fixture manifest remains the source corpus. Grammar
slice membership uses explicit stable fixture names and named feature families,
not positional prefixes such as taking the first N cases.

For every in-scope fixture:

- both implementations receive the same normalized logical path and canonical
  token sequence;
- accepted results compare the complete surface AST and retained spans;
- rejected results compare code, optional span, reason constructor, and every
  payload exactly;
- each side renders through the generic runtime-value renderer; and
- repeated evaluation must produce byte-identical output.

Lexically rejected source fixtures exercise the source façade and compare the
existing canonical lexical failure. They do not count as parser failures.

The final child must prove that every fixed fixture belongs to a covered family
and run full parity in manifest order. New parser fixtures must update the
shared manifest and family assignment in the same change.

## Verification Strategy

The parser-kernel suite covers:

- consumed and unconsumed success and failure;
- committed choice, explicit rollback, lookahead, and deterministic farthest
  failure selection;
- optional, repeated, and separated parsing;
- zero-progress repetition rejection;
- context independence; and
- long stack-safe token traversal.

Schema and adapter tests cover every AST and error constructor, normalized
paths, selected spans, source-exact integer and fractional values, lossless
unsupported-signature tokens, total Haskell mapping, and deterministic
rendering.

Each grammar child runs its focused differential fixture family. The final
child adds full-corpus coverage, repeated deterministic comparison, large
program traversal, and exact source-façade discrimination between lexical and
parser failures.

Every implementation child also runs:

```text
cabal test --project-dir=jazz-next all
bash scripts/check-execution-queue.sh
bash scripts/check-docs.sh
git diff --check
```

Focused commands and the complete set of affected test targets are fixed in
each child's implementation plan rather than embedded as guessed file-level
instructions in this design.

## Performance Evidence

The hosted Jazz parser must not allocate or traverse the entire remaining token
stream for each token consumed. Cursor operations are constant-time at the API
level, and repetition remains tail-recursive.

The first child records kernel behavior for large successful and failing token
streams. Later slices add representative parser benchmarks. Final closure
records same-machine hosted Jazz parser evidence and retains GHC profiling for
the stage-0 lexing, parsing, and lowering stages and their named sub-stages.

Physical timing and allocation measurements are review evidence rather than a
flaky fixed pass/fail threshold. Crashes, semantic mismatches, nondeterminism,
host-stack growth, incompatible comparison metadata, or unexplained
deterministic-budget overruns are failures. A reproducible physical regression
must be investigated and explained before closeout.

## Queue and Documentation Closeout

The contract/kernel and all five grammar children are complete and archived.
Together they establish the fixed surface/failure schema, compiler-local
token/expression/pattern/signature/context/operator/declaration/program/façade
owners, exact complete stage-0 comparison across six families and all 365
fixtures, and four deterministic scale profiles without parser-specific host
support.

`Ready Now` is empty. This closeout does not promote canonical core,
backend-neutral lowered IR, LLVM lowering, object/link production, or native
runtime implementation; each requires a separately reviewed child contract and
plan.

## Non-Goals

This design does not add parser recovery, partial ASTs, multiple parser errors,
full-tree source ranges, a public stdlib parser API, arbitrary-precision numeric
semantics outside the surface representation, new language syntax, lowering,
canonical typed core, type inference, a bytecode format, a VM, backend-neutral
lowered IR, LLVM lowering, object generation, linking, or a native runtime.

It does not modify `jazz-hs/` or `jazz2/`, and it does not make Jazz compiler
modules depend on Haskell compiler values.
